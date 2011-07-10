-module(cadam_tty).

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

%%----------------------------------------------------------------------
%% Func: init/0
%% Returns: State
%%----------------------------------------------------------------------
init()->
    ok.

%%----------------------------------------------------------------------
%% Func: event_handler/2
%% Returns: State
%%----------------------------------------------------------------------
event_handler(Event, State)->
    io:format("-----------------------~n~p~n--------------------------~n", [Event]),
    State.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%----------------------------------------------------------------------
handle_call(_Request, _From, S) ->
    %% ok = error_logger:format("~p(~p): handle_call(~p, ~p, ~p)~n",
    %%                          [?MODULE, self(), Request, From, S]),
    %% Reply = {error, {bad_request, Request}},
    Reply = ok,
    {reply, Reply, S}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%----------------------------------------------------------------------
handle_info(_Info, S) ->
%%    ok = io:format("~p(~p): handle_info(~p, ~p)~n", [?MODULE, self(), _Info, S]),
    {noreply, S}.
