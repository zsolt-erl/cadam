-module(cadam_metrics_queue_sup).

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
    ChildSpec = ?TEMPCHILD(tsqueue, worker, []),
    {ok, { {simple_one_for_one, 5, 10}, [ChildSpec]}}.

