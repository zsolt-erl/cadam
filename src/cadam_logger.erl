-module(cadam_logger).

-behaviour(cadam_gen_et_viewer).

-export([start_link/0, start_link/1, stop/0]).

%% cadam_gen_et_viewer callback exports
-export([init/0, event_handler/2, handle_call/3, handle_info/2]).

-include("et.hrl").

start_link()->
    cadam_gen_et_viewer:start_link( global:whereis_name(et_collector), ?MODULE ).

start_link(CollectorPid)->
    cadam_gen_et_viewer:start_link(CollectorPid, ?MODULE).

stop()->
    cadam_gen_et_viewer:stop(?MODULE).


%%%----------------------------------------------------------------------
%%% cadam_gen_et_viewer callback functions
%%%----------------------------------------------------------------------

%% needs to return the initial state
init()->
    error_logger:add_report_handler(file_logger, "priv/logs/"++atom_to_list(node())).

%% needs to return the new state
event_handler(Event, State)->
    error_logger:info_report(cadam, Event),
    State.


handle_call(Request, From, S) ->
    ok = error_logger:format("~p(~p): handle_call(~p, ~p, ~p)~n",
                             [?MODULE, self(), Request, From, S]),
    Reply = {error, {bad_request, Request}},
    {reply, Reply, S}.


%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%----------------------------------------------------------------------
handle_info(_Info, S) ->
%%    ok = io:format("~p(~p): handle_info(~p, ~p)~n", [?MODULE, self(), _Info, S]),
    {noreply, S}.
