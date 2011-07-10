%%% File    : cadam.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Description : this is the public API
%%% Created : 26 Jun 2011 by Zsolt Keszthelyi <zsolt.erl@gmail.com>

-module(cadam).

-export([start/0, start/1, connect/1, stop/0, start_module/1, stop_module/1, et_viewer/0]).

start()->
    start(node()).

start(Node)->
    net_adm:ping(Node),

    %% use alternate et beams
    application:load(cadam),
    case application:get_env(cadam, et_beam_path) of
	{ok, Val} ->
	    code:add_patha(Val),
	    io:format("Adding ~s to code path!!~n", [Val]);
	_Other ->
	    skip
    end,

    application:start(cadam).

connect(Node)->
    net_adm:ping(Node),
    nodes().

stop()->
    application:stop(cadam).

start_module(CadamModule)->
    cadam_controller:start_module(CadamModule).

stop_module(CadamModule)->
    cadam_controller:stop_module(CadamModule).

et_viewer()->
    Collector = global:whereis_name(et_collector),
    et_viewer:start([{collector_pid, Collector}]).
