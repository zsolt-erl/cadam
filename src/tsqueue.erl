%%%-------------------------------------------------------------------
%%% File    : tsqueue.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Description : time segment queue
%%%           collects values pushed into it or sent to it from an other tsqueue
%%%           collection restarts at the end of the time segment at which point it
%%%           sends the average of the queue to the target processes
%%%
%%%           these queues can be chained this way it's easy to create an average of a metric 
%%%           by certain time intervals
%%%           by specifing a receiver process as a target the calculated averages are streamed to the 
%%%           process, otherwise the last average can be queried with tsqueue:get_last_average/1
%%%
%%% Created : 27 Jun 2011 by Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%%-------------------------------------------------------------------
-module(tsqueue).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, clear/1, push/2, get_most_recent/1, get_queue/1, get_last_average/1, 
	 add_target/2, remove_target/2, stop/1]).

%% internal exports
-export([timer_loop/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  queue        = [],
	  last_average = 0   :: integer(), 
	  time_segment       :: non_neg_integer(),
	  timer_pid          :: pid(),
	  targets            :: [pid()]
	 }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

start_link(TimeSegment)->
    start_link(TimeSegment, []).

start_link(TimeSegment, Targets) ->
    {ok, Pid}=gen_server:start_link(?MODULE, [TimeSegment, Targets], []),
    Pid.

clear(ServerPid)->
    gen_server:call(ServerPid, clear).

push(Num, ServerPid)->
    ServerPid ! {self(), tsqueue, Num}.

%% get the most recent value that was pushed into the queue 
%% ServerPid is the pid of the queue process returned by start_link
get_most_recent(ServerPid)->
    gen_server:call(ServerPid, get_most_recent).

%% get the whole queue  (head is the most recent)
get_queue(ServerPid)->
    gen_server:call(ServerPid, get_queue).

%% get the last calculated average (this is what it sends to the Targets)
get_last_average(ServerPid)->
    gen_server:call(ServerPid, get_last_average).

add_target(ServerPid, TargetPid)->
    gen_server:call(ServerPid, {add_target, TargetPid}).

remove_target(ServerPid, TargetPid)->
    gen_server:call(ServerPid, {remove_target, TargetPid}).


stop(ServerPid)->
    gen_server:call(ServerPid, stop).

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
init([TimeSegment, Targets]) ->
    TimerPid=timer_start(TimeSegment, self()),
    {ok, #state{time_segment = TimeSegment, timer_pid = TimerPid, targets = Targets}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% handle_call({push, Num}, _From, State = #state{queue = Queue}) ->
%%     NewQueue = [Num|Queue],
%%     {reply, NewQueue, State#state{queue = NewQueue}};

handle_call(clear, _From, State) ->
    {reply, ok, State#state{queue = []}};

handle_call(get_most_recent, _From, State = #state{queue = Queue}) ->
    Reply = 
	case Queue of
	    []     -> empty;
	    [H|_T] -> {most_recent, H}
	end,
    {reply, Reply, State};

handle_call(get_queue, _From, State = #state{queue = Queue}) ->
    Reply = Queue,
    {reply, Reply, State};

handle_call(get_last_average, _From, State = #state{last_average = LastAverage}) ->
    Reply = LastAverage,
    {reply, Reply, State};

handle_call({add_target, Pid}, _From, State = #state{targets = Targets}) ->
    {reply, ok, State#state{targets = [Pid|Targets]}};

handle_call({remove_target, Pid}, _From, State = #state{targets = Targets}) ->
    {reply, ok, State#state{targets = Targets -- [Pid]}};

handle_call(stop, _From, State) ->
    {stop, stopped, State};


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
handle_info(timeout, State = #state{queue = Queue, targets = Targets}) ->
    LastAverage = 
	case length(Queue) of
	    0 ->
		0;
	    Len ->
		Avg = round(lists:sum(Queue) / Len),
		SendToTarget = fun(Target)->
				       case is_pid(Target) andalso is_process_alive(Target) of
					   true  -> Target ! {self(), tsqueue, Avg};
					   false -> dont_send
				       end
			       end,
		[ SendToTarget(Target) || Target <- Targets ],
		Avg
	end,
    {noreply, State#state{queue = [], last_average = LastAverage}};

%% incoming value, just store it in queue
handle_info({_From, tsqueue, Num}, State = #state{queue = Queue}) ->
%%    io:format("QUEUE: ~p storing: ~w   ~w~n", [self(), Num, [Num|Queue]]),
    {noreply, State#state{queue = [Num|Queue]}};

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


timer_start(Interval, Pid)->
    spawn_link(?MODULE, timer_loop, [Interval, Pid]).

%% timer_stop(TimerPid)->
%%     unlink(TimerPid),
%%     exit(TimerPid, kill).
 
timer_loop(Interval, Pid)->
    receive
    after
	Interval ->
	    Pid ! timeout
    end,
    timer_loop(Interval, Pid).
