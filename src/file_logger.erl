-module(file_logger).

-behaviour(gen_event).

-include("et.hrl").

-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, code_change/3]).

init(File) ->
    process_flag(trap_exit, true),
    case file:open(File, [write]) of
	{ok,Fd} ->
	    {ok, {Fd, File}};
	Error ->
	    Error
    end.
    
handle_event({_Type, GL, _Msg}, State) when node(GL) =/= node() ->
    {ok, State};

%% handle info_report when it is type 'cadam'
%% Report :: #event{}  from et.hrl
handle_event({info_report, _GL, {_Pid, cadam, Report}}, State={Fd, _File})->
    #event{detail_level=Detail, trace_ts=TimeStamp, from=From, to=To, label=Label, contents=Contents} = Report,

    Time=misc:datetime_to_string( calendar:now_to_local_time(TimeStamp) ),
    case From=:=To of
	true ->
	    io:format(Fd, "[~p] [~s]: ~p -- ~p : ~p~n", [Detail, Time, From, Label, Contents]);
	false ->
	    io:format(Fd, "[~p] [~s]: ~p --~p--> ~p : ~p~n", [Detail, Time, From, Label, To, Contents])
    end,
    {ok, State};

handle_event(_, State) ->
    {ok, State}.

handle_info({'EXIT', Fd, _Reason}, {Fd, _File}) ->
    remove_handler;

handle_info(_, State) ->
    {ok, State}.

handle_call(_Query, State) ->
    {ok, {error, bad_query}, State}.

terminate(_Reason, State) ->
    case State of
        {Fd, _File} ->
            ok = file:close(Fd);
        _ ->
            ok
    end,
    [].

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

