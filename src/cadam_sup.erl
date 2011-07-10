
-module(cadam_sup).

-behaviour(supervisor).

-include("cadam.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    case application:get_env(cadam, netload_et) of
	{ok, true} ->
	    io:format("Loading et on connected nodes~n"),
	    %% load needed et modules on all connected nodes
	    %% this is needed if the et app does not exist on the monitored nodes
	    %% (eg. when monitoring a riak cluster)
	    Modules = [et, et_collector, et_selector],
	    [ misc:network_load(Module) || Module <- Modules ];
	_Other ->
	    skip
    end,

    CollectorArgs = [ [{trace_global, true},{trace_pattern, {et, max}}] ],
    Collector  = ?PERMCHILD(et_collector, worker, CollectorArgs),
    Controller = ?PERMCHILD(cadam_controller, worker, []),
    WebSocketServer = ?PERMCHILD(websocket_server, worker, []),
    {ok, { {one_for_one, 5, 10}, [Controller, Collector, WebSocketServer]} }.

