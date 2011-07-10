%% KEEPS CREATING NEW QUEUES FOR NO REASON


%% collects the metrics events and channels them into a series of queues
%% processes can subscribe to metrics / queue stage
%% eg. raw cpu_load --> 1min queue --> 5 min queue --> 15 min queue

-module(cadam_metrics).

-behaviour(cadam_gen_et_viewer).

%% API exports
-export([start_link/0, start_link/1, stop/0, get_all_metrics/0, get_average/2, get_queues/1,
	 subscribe_to_queue/3, unsubscribe_from_queue/3]).

%% cadam_gen_et_viewer callback exports
-export([init/0, event_handler/2, handle_call/3, handle_info/2]).

-include("et.hrl").
-include("cadam.hrl").

-record(metric, {id, q1, q2, q3}).
-record(state, { metrics = [] :: [#metric{}] }).

-define(Q1TIME, 2000).    %% 2 sec
-define(Q2TIME, 60000).   %% 1 min
-define(Q3TIME, 300000).  %% 5 min

start_link()->
    cadam_gen_et_viewer:start_link( global:whereis_name(et_collector), ?MODULE ).

start_link(CollectorPid)->
    cadam_gen_et_viewer:start_link(CollectorPid, ?MODULE).

stop()->
    cadam_gen_et_viewer:stop(?MODULE).

%% get IDs of all metrics we know about
get_all_metrics()->
    gen_server:call(cadam_metrics, get_all_metrics).

%% get last calculated average of a metric for a specific queue
get_average(MetricID, QueueNum)->
    gen_server:call(cadam_metrics, {get_average, MetricID, QueueNum}).

%% get content of all queues for a certain metric
get_queues(MetricID)->
    gen_server:call(cadam_metrics, {get_queues, MetricID}).

subscribe_to_queue(MetricID, QueueNum, SubscriberPid)->
    gen_server:call(cadam_metrics, {subscribe_to_queue, MetricID, QueueNum, SubscriberPid}).

unsubscribe_from_queue(MetricID, QueueNum, SubscriberPid)->
    gen_server:call(cadam_metrics, {unsubscribe_from_queue, MetricID, QueueNum, SubscriberPid}).

%%%----------------------------------------------------------------------
%%% cadam_gen_et_viewer callback functions
%%%----------------------------------------------------------------------

%% needs to return the initial state
init()->
    %% THIS DOESN'T WORK, JUST HANGS
    %% CANNOT START A CHILD OF MY OWN SUP IN INIT??
    %% start a supervisor for the metrics queues and put it under cadam_sup
    %% MetricsQueueSup = ?PERMCHILD(cadam_metrics_queue_sup, supervisor, []),
    %% {ok, _Child} = supervisor:start_child(cadam_sup, MetricsQueueSup),
    start_metrics_generators(),
    io:format("Started metrics generators on all connected nodes~n"),
    #state{}.


%% events that are coming to the metrics module should have a proplist as their contents
%% this proplist is a {MetricsName, MetricsValue} pair
%% each metric is stored in #state{} as {metric, {From, MetricsName}, Queue1, Queue2, Queue3} so a 
%% metric is uniquely identified by the {From, MetricsName} tuple

-spec( event_handler(#event{}, #state{}) -> #state{} ).

%% only handle metrics events
event_handler(Event = #event{to = metrics}, State)->
    #event{detail_level = _DetailLevel, from = From, label = _Label, contents = Contents} = Event,

    %% io:format("Got event:~p~n", [Event]),
    
    HandleMetricFromEvent = 
	fun({MName, MVal}, StateAcc = #state{metrics = Metrics})->
		case lists:keyfind({From, MName}, 2, Metrics) of
		    false -> 
			%% io:format("Registering new metric:~p~n", [{From, MName}]),
			%% this is a new metric we didn't see before
			%% set up queue chain for this and push in first value
			Q3 = tsqueue:start_link(?Q3TIME),
			Q2 = tsqueue:start_link(?Q2TIME, [Q3]),
			Q1 = tsqueue:start_link(?Q1TIME, [Q2]),
			tsqueue:push(MVal, Q1),
			NewMetric=#metric{id = {From, MName}, q1 = Q1, q2 = Q2, q3 = Q3},
			StateAcc#state{metrics = [NewMetric|Metrics]};    %% adding new metric to state
		    #metric{q1 = Q1} ->
			%% io:format("Updating metric:~p~n", [{From, MName}]),
			%% this is a known metric
			%% push value into first queue
			tsqueue:push(MVal, Q1),
			StateAcc     %% State unchanged
		end
	end,

    _NewState = lists:foldl( HandleMetricFromEvent, State, Contents );

%% ignore all non metrics events
event_handler(_Event, State)->
%%    io:format("METRICS: not my event: ~p~n", [_Event]),
    State.


%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%----------------------------------------------------------------------

handle_call(get_all_metrics, _From, State = #state{metrics = Metrics}) ->
    Reply = [ID || #metric{id = ID} <- Metrics],
    {reply, Reply, State};

handle_call({get_average, MetricID, QueueNumber}, _From, State = #state{metrics = Metrics}) ->
    Reply = 
	case lists:keyfind(MetricID, 2, Metrics) of
	    false ->
		{error, nonex_metric};
	    #metric{id = MetricID, q1 = Q1, q2 = Q2, q3 = Q3} ->
		case QueueNumber of
		    1 -> tsqueue:get_last_average(Q1);
		    2 -> tsqueue:get_last_average(Q2);		
		    3 -> tsqueue:get_last_average(Q3);
		    _ -> {error, nonex_queue}
		end
	end,
    {reply, Reply, State};

handle_call({get_queues, MetricID}, _From, State = #state{metrics = Metrics}) ->
    Reply = 
	case lists:keyfind(MetricID, 2, Metrics) of
	    false ->
		{error, nonex_metric};
	    #metric{id = MetricID, q1 = Q1, q2 = Q2, q3 = Q3} ->
		{tsqueue:get_queue(Q1), tsqueue:get_queue(Q2), tsqueue:get_queue(Q3)} 
	end,
    {reply, Reply, State};

handle_call({subscribe_to_queue, MetricID, QueueNumber, SubscriberPid}, _From, State = #state{metrics = Metrics}) ->
    Reply = 
	case lists:keyfind(MetricID, 2, Metrics) of
	    false ->
		{error, nonex_metric};
	    #metric{id = MetricID, q1 = Q1, q2 = Q2, q3 = Q3} ->
		case QueueNumber of
		    1 -> tsqueue:add_target(Q1, SubscriberPid), {subscribed_to, Q1};
		    2 -> tsqueue:add_target(Q2, SubscriberPid), {subscribed_to, Q2};
		    3 -> tsqueue:add_target(Q3, SubscriberPid), {subscribed_to, Q3};
		    _ -> {error, nonex_queue}
		end
	end,
    {reply, Reply, State};

handle_call({unsubscribe_from_queue, MetricID, QueueNumber, SubscriberPid}, _From, State = #state{metrics = Metrics}) ->
    Reply = 
	case lists:keyfind(MetricID, 2, Metrics) of
	    false ->
		{error, nonex_metric};
	    #metric{id = MetricID, q1 = Q1, q2 = Q2, q3 = Q3} ->
		case QueueNumber of
		    1 -> tsqueue:remove_target(Q1, SubscriberPid), {unsubscribed_from, Q1};
		    2 -> tsqueue:remove_target(Q2, SubscriberPid), {unsubscribed_from, Q2};
		    3 -> tsqueue:remove_target(Q3, SubscriberPid), {unsubscribed_from, Q3};
		    _ -> {error, nonex_queue}
		end
	end,
    {reply, Reply, State};

handle_call(Request, From, S) ->
    ok = error_logger:format("~p(~p): handle_call(~p, ~p, ~p)~n",
                             [?MODULE, self(), Request, From, S]),
    Reply = {error, {bad_request, Request}},
    {reply, Reply, S}.




%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}        
%%----------------------------------------------------------------------
handle_info({nodeup, Node}, State) ->
    %% start metrics generators
    io:format("New node:~p,    Starting metrics generators.~n",[Node]),
    start_metrics_generators(Node),
    {noreply, State};


handle_info(_Info, S) ->
%%    ok = io:format("~p(~p): handle_info(~p, ~p)~n", [?MODULE, self(), _Info, S]),
    {noreply, S}.



%%%----------------------------------------------------------------------
%%% internal functions
%%%----------------------------------------------------------------------

%% starts metrics generators on all connected nodes
start_metrics_generators()->
    [start_metrics_generators(Node) || Node <- nodes()].

start_metrics_generators(Node)->
    %% load cadam_metrics_generators module on Node
    misc:network_load(Node, cadam_metrics_generators),

    %% start generators on Node
    cadam_metrics_generators:start_generator(memory_report, Node, 1000),
    cadam_metrics_generators:start_generator(process_count_report, Node, 1000),
    cadam_metrics_generators:start_generator(cpu_load, Node, 10000),
    ok.
