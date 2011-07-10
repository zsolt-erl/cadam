%%%-------------------------------------------------------------------
%%% File    : websocket_server.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Description : handles communication between gui and cadam modules 
%%%               (eg. between gui and cadam_metrics)
%%%
%%% Created : 29 Jun 2011 by Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%%-------------------------------------------------------------------
-module(websocket_server).

-behaviour(gen_server).

%% API
-export([start_link/0, subscribe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, all_atoms_to_lists/1]).

-record(state, {connected     = false, 
		websocket, 
		subscriptions = []           :: [{MetricID :: tuple(), QueueNum :: non_neg_integer(), QueuePid :: pid()}]
	       }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


subscribe(MetricsID)->
    gen_server:call(?MODULE, {subscribe, MetricsID}).

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
    webserver:start(9000),
    io:format("Started webserver on port 9000~nAccess web gui at: http://localhost:9000/gui~n"),
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
handle_call({subscribe, MetricID}, _From, State) ->
    Reply = cadam_metrics:subscribe_to_queue(MetricID, 1, self()),
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
handle_info({ok, WebSocket}, State = #state{connected = false}) ->
    io:format("~nwebsocket open, waiting for connection~n"),
    case yaws_api:websocket_receive(WebSocket) of
	{error,closed} ->
	    io:format("The websocket got disconnected right from the start. "
		      "This wasn't supposed to happen!!~n"),
	    {noreply, State};
	{ok, Messages} ->
	    case Messages of
		[<<"client-connected">>| Rest] ->
		    io:format("Client connected, switching to active mode~n"),
		    yaws_api:websocket_setopts(WebSocket, [{active, true}]),
		    ConnectedState = State#state{connected = true, websocket = WebSocket},
		    NewState = handle_raw_messages(Rest, ConnectedState),
		    {noreply, NewState};
		Other ->
		    io:format("websocket_owner got: ~p. Waiting for proper connection~n", [Other]),
		    {noreply, State}
	    end
    end;

%% handle anything coming from the gui
%% gui sends json objects:
%% 1. {cmd : "get_all_metrics"}
%%    Response: {response_for: "get_all_metrics", response: [[From, MetricName], ...]}
%%
%% 2. {cmd: "subscribe_to_queue", metricid: [From, MetricName], queuenum: Number}
%%    Response: {response_for: "subscribe_to_queue", response: "ok" | "error"} 
%%
%% 3. {cmd: "unsubscribe_from_queue", metricid: [From, MetricName], queuenum: Number}
%%    Response: {response_for: "unsubscribe_from_queue", response: "ok" | "error"} 


handle_info({tcp, _WebSocket, DataFrames}, State = #state{connected = true}) ->
    io:format("~p got ~p~n",[?MODULE, DataFrames]),
    BinChunks = yaws_websockets:unframe_all(DataFrames, []),
    io:format("bin data:~p~n", [BinChunks]),
    NewState = handle_raw_messages(BinChunks, State),
    {noreply, NewState};

handle_info({tcp_closed, _WebSocket}, State = #state{connected = true, subscriptions = Subscriptions}) ->
    [tsqueue:remove_target(QueuePid, self()) || {_,_,QueuePid} <- Subscriptions],
    io:format("Websocket closed. Waiting for new connection...~n"),
    {noreply, State#state{connected = false}};

handle_info({QueuePid, tsqueue, Value}, State = #state{connected = true, websocket = WebSocket, 
						       subscriptions = Subscriptions}) ->
    %% find the metric id that is subscribed to this queue
    case lists:keyfind(QueuePid, 3, Subscriptions) of
	false ->
	    io:format("Got msg from a queue that I'm not subscribed to. Ignoring it.~n");
	{MetricID, _QueueNum, _QueuePid} ->
	    Response = {struct, [{metricid, metricid_to_jsonarray(MetricID)}, {value, Value}]},
	    Message  = {struct, [{response_for, "subscription"}, {response, Response}]},
	    yaws_api:websocket_send(WebSocket, json2:encode(Message))
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
    yaws:stop(),
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


%% takes a list of unframed messages and handles them
-spec( handle_raw_messages([binary()], #state{}) -> #state{}).
handle_raw_messages(BinChunks, State)->
    lists:foldl(
      fun(OneChunk, StateAcc)->
	      case json2:decode_string(binary_to_list(OneChunk)) of
		  {error, _} ->
		      io:format("Not a JSON formatted message:~p~n", [OneChunk]),
		      StateAcc;
		  {ok, JSON} ->
		      handle_json_message(JSON, StateAcc)
	      end
      end, 
      State, 
      BinChunks).

%% handles a message that has been decoded into json
-spec( handle_json_message(tuple(), #state{}) -> NewState :: #state{} ).
handle_json_message(JSON, State = #state{websocket = WebSocket})->
    Command = jsonrpc:s(JSON, cmd),
    io:format("Got command via websocket: ~p~n", [Command]),
    case Command of
	"get_all_metrics" ->
	    Metrics = cadam_metrics:get_all_metrics(),
	    JSONMetrics = {array, [{array, [From, MetricName]} || {From, MetricName} <- Metrics]},
	    Response = {struct, [{response_for, Command}, {response, all_atoms_to_lists(JSONMetrics)}]},
	    yaws_api:websocket_send(WebSocket, json2:encode(Response)),
	    State;
	"subscribe_to_queue" ->
	    io:format("subscribe request:~p~n", [JSON]),
	    MetricID = metricid_from_jsonarray( jsonrpc:s(JSON, metricid) ),
	    QueueNum = jsonrpc:s(JSON, queuenum),
	    io:format("metricid:~p,    queueunum:~p~n",[MetricID, QueueNum]),
	    {Result, NewState} = 
		case cadam_metrics:subscribe_to_queue(MetricID, QueueNum, self()) of
		    {subscribed_to, QueuePid} -> 
			%% store metricid and queuepid
			CurrentSubscriptions = State#state.subscriptions,
			{ok, State#state{subscriptions = [{MetricID, QueueNum, QueuePid} | CurrentSubscriptions]}};
		    {error, _} -> 
			{error, State}
		end,
	    Response = {struct, [{response_for, Command}, {response, all_atoms_to_lists(Result)}]},
	    yaws_api:websocket_send(WebSocket, json2:encode(Response)),
	    NewState;
	"unsubscribe_from_queue" ->
	    io:format("unsubscribe request:~p~n", [JSON]),
	    MetricID = metricid_from_jsonarray( jsonrpc:s(JSON, metricid) ),
	    QueueNum = jsonrpc:s(JSON, queuenum),
	    {Result, NewState} = 
		case cadam_metrics:unsubscribe_from_queue(MetricID, QueueNum, self()) of
		    {unsubscribed_from, QueuePid} -> 
			%% delete metricid and queuepid
			CurrentSubscriptions = State#state.subscriptions,
			{ok, State#state{subscriptions = CurrentSubscriptions -- [{MetricID, QueueNum, QueuePid}]}};
		    {error, _} -> 
			{error, State}
		end,
	    Response = {struct, [{response_for, Command}, {response, all_atoms_to_lists(Result)}]},
	    yaws_api:websocket_send(WebSocket, json2:encode(Response)),
	    NewState;
	_Other ->
	    io:format("Unexpected msg from websocket:~p~n", [JSON]),
	    State
    end.



%% convert atoms to lists in a term (except for 'array' and 'struct', these are needed by the json encoder, this means 
%% that if array or struct needs to be a json key or value it already has to be a string)
all_atoms_to_lists(array) -> array;
all_atoms_to_lists(struct) -> struct;
all_atoms_to_lists(A) when is_atom(A) ->
    atom_to_list(A);
all_atoms_to_lists(L) when is_list(L) ->
    [all_atoms_to_lists(Elem) || Elem <- L];
all_atoms_to_lists(T) when is_tuple(T) ->
    List = tuple_to_list(T),
    Converted = [all_atoms_to_lists(Elem) || Elem <- List],
    list_to_tuple(Converted);
all_atoms_to_lists(Other) ->
    Other.


%% metricid is {atom(), atom()} here but [string(), string()] in gui (json doesn't have atoms)
metricid_to_jsonarray({From, MetricName})->
    {array, [atom_to_list(From), atom_to_list(MetricName)]}.

metricid_from_jsonarray({array, [FromStr, MNameStr]})->
    {list_to_atom(FromStr), list_to_atom(MNameStr)}.
    
