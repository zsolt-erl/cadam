-module(cadam_gen_et_viewer).

-behaviour(gen_server).

-export([start_link/2, stop/1]).

-export([behaviour_info/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-include("et.hrl").

-record(state,
        {collector_pid,    % Pid of collector process
	 callback_module,
	 callback_state,
	 pt_key          = first
	}).


-spec behaviour_info(atom()) -> [{atom(), non_neg_integer()}] | 'undefined'.
behaviour_info(callbacks) ->
    [{event_handler,2}, 
     {handle_call, 3},
     {handle_info, 2},
     {init,0}
    ];

behaviour_info(_Other) ->
    undefined.




%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

start_link(CollectorPid, Callback) ->
    gen_server:start_link({local, Callback}, ?MODULE, [CollectorPid, Callback], []).

stop(CallbackModule)->
    gen_server:call(CallbackModule, stop).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------

init([CollectorPid, CallbackModule]) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    et_collector:dict_insert(CollectorPid, {subscriber, self()}, ?MODULE),
    CallbackState = CallbackModule:init(),
    State = #state{collector_pid = CollectorPid, 
		   callback_module = CallbackModule,
		   callback_state = CallbackState},
    {ok, State, 1000}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call(get_collector_pid, _From, S) ->
    Reply = S#state.collector_pid,
    {reply, Reply, S, 0};

handle_call(stop, _From, S) ->
    {stop, shutdown, S};

handle_call(Request, From, State = #state{callback_module = CallbackModule, callback_state = CallbackState}) ->
    {reply, Reply, NewCallbackState} = CallbackModule:handle_call(Request, From, CallbackState),
    {reply, Reply, State#state{callback_state = NewCallbackState}, 0}.
	


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_cast(Msg, S) ->
    ok = error_logger:format("~p(~p): handle_cast(~p, ~p)~n",
                             [?MODULE, self(), Msg, S]),
    {noreply, S, 0}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info({et, {more_events, _Size}}, S) ->
%%    io:format("got more events~n"),
    {noreply, S, 0};

handle_info({et, close}, S) ->
    {stop, shutdown, S};

handle_info({'EXIT', Pid, Reason}, S) ->
    if
	Pid =:= S#state.collector_pid ->
	    unlink(Pid),
	    {stop, Reason, S};
	true ->
	    {noreply, S, 0}
    end;

handle_info(timeout, State = #state{callback_module = CallbackModule, callback_state = CallbackState})->
    %% pull in more events and log them
    Fun=fun(Event, {Count, _LastEvent, StateAcc})->
		NewStateAcc=CallbackModule:event_handler(Event, StateAcc),
		{Count+1, Event, NewStateAcc}
	end,
    {EventCount, LastEvent, NewCallbackState}=
	et_collector:iterate(State#state.collector_pid, State#state.pt_key, 10, Fun, {0, none, CallbackState}),

    Key  = case LastEvent of
	       none -> State#state.pt_key;
	       _    -> et_collector:make_key(trace_ts, LastEvent)
	   end,
    Timeout=
	if
	    EventCount < 10 -> 1000;
	    true            -> 10
	end,
    {noreply, State#state{pt_key = Key, callback_state = NewCallbackState}, Timeout};


handle_info(Info, State = #state{callback_module = CallbackModule, callback_state = CallbackState}) ->
    {noreply, NewCallbackState} = CallbackModule:handle_info(Info, CallbackState),
    {noreply, State#state{callback_state = NewCallbackState}, 0}.



%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------

terminate(Reason, S) ->
    io:format("~p stopped with reason:~p~nstate:~p~n", [S#state.callback_module, Reason, S]),
    ignore.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.





