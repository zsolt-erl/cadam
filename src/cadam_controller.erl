%%%-------------------------------------------------------------------
%%% File    : cadam_controller.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Description : 
%%%
%%% Created : 26 Jun 2011 by Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%%-------------------------------------------------------------------
-module(cadam_controller).

-behaviour(gen_server).

-include("cadam.hrl").


%% API
-export([start_link/0, start_module/1, stop_module/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {running_modules = []}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_module(CadamModule)->
    gen_server:call(?MODULE, {start_module, CadamModule}).

stop_module(CadamModule)->
    gen_server:call(?MODULE, {stop_module, CadamModule}).



%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    net_kernel:monitor_nodes(true),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call( {start_module, CadamModule}, _From, State) ->
    if
	CadamModule == cadam_metrics ->
	    MetricsQueueSup = ?PERMCHILD(cadam_metrics_queue_sup, supervisor, []),
	    {ok, _Child} = supervisor:start_child(cadam_sup, MetricsQueueSup);
	true -> skip
    end,

    Child = ?TRANSCHILD(CadamModule, worker, []),
    Reply = supervisor:start_child(cadam_sup, Child),
    {reply, Reply, State};


handle_call( {stop_module, CadamModule}, _From, State) ->
    if
	CadamModule == cadam_metrics ->
	    case supervisor:terminate_child(cadam_sup, cadam_metrics_queue_sup) of
		ok    -> supervisor:delete_child(cadam_sup, cadam_metrics_queue_sup);
		Error -> Error
	    end;
	true -> skip
    end,
    Reply = 
	case supervisor:terminate_child(cadam_sup, CadamModule) of
	    ok    -> supervisor:delete_child(cadam_sup, CadamModule);
	    Error2 -> Error2
	end,
    {reply, Reply, State};


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({nodeup, Node}, State) ->
    case application:get_env(cadam, netload_et) of
	{ok, true} ->
	    io:format("New node:~p,    Loading et modules.~n",[Node]),
	    %% load needed et modules on Node
	    %%  ??? when we receive the nodeup can we load et? or et_collector has already tried loading it on the new node
	    %%  from the local beam,   or we can just simply unload whatever is there and load our version
	    Modules = [et, et_collector, et_selector],
	    [ misc:network_load(Node, Module) || Module <- Modules ];
	_Other ->
	    skip
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
