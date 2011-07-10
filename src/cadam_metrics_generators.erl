%%% File    : cadam_metrics_generators.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Description : contains funs that genrate metrics data
%%                also has a helper function to load this module on other nodes
%%% Created : 28 Jun 2011 by Zsolt Keszthelyi <zsolt.erl@gmail.com>

-module(cadam_metrics_generators).

-include("cadam_macros.hrl").

-compile(export_all).


%%%----------------------------------------------------------------------
%%% event generators
%%% an event generator has to generate an event and do nothing else
%%% these generators are run on the nodes in the cluster
%%%
%%% a generator can have an init fun (generator fun name + _init)
%%% if it exists it will be used to initialize the generator on the 
%%% target node
%%% when init is successfull the init function needs to return 'ok'
%%% otherwise the generator will not be started
%%%----------------------------------------------------------------------

memory_report() ->
    M = erlang:memory(),
    Total  = proplists:get_value(total, M),
    Atom   = proplists:get_value(atom,  M),
    ?SYS_METRICS(memory_total, Total),
    ?SYS_METRICS(memory_atom, Atom).


process_count_report() ->
    ?SYS_METRICS(process_count, erlang:system_info(process_count)).


cpu_load_init()->
    application:start(sasl),
    application:start(os_mon).

cpu_load()->
    ?SYS_METRICS(cpu_load, cpu_sup:avg1()).


%%%------------------------------------------------------------------------------------------------
%%% fun to start a generator on a target node (?MODULE has to be already loaded on the target node)
%%% the generator process will monitor the 'cadam' node and exit when 'cadam' node goes down
%%% this is to make sure we don't leave generators running when cadam goes down
%%%------------------------------------------------------------------------------------------------
-spec( start_generator(atom(), node(), non_neg_integer()) -> ok ).
start_generator(GeneratorFun, TargetNode, Delay)->
    spawn(TargetNode, ?MODULE, start_generator_on_target, [GeneratorFun, Delay, node()]).

start_generator_on_target(GeneratorFun, Delay, CadamNode)->
    register(GeneratorFun, self()),
    net_kernel:monitor_nodes(true),
    
    %% check if there's an init fun for this generator
    InitName = list_to_atom(atom_to_list(GeneratorFun)++"_init"),
    Exports  = proplists:get_value(exports, ?MODULE:module_info()),
    case lists:member({InitName, 0}, Exports) of
	false ->
	    %% no init function, just start running the loop
	    generator_loop(GeneratorFun, Delay, CadamNode);
	true ->
	    %% inititalize the generator and start the loop if successfull
	    case ?MODULE:InitName() of
		ok ->
		    generator_loop(GeneratorFun, Delay, CadamNode);
		_Else ->
		    exit(init_error)
	    end
    end.

generator_loop(GeneratorFun, Delay, CadamNode)->
    receive
	{nodedown, CadamNode} ->
	    exit(cadam_is_down);
	_Other ->
	    skip
    after
	Delay ->
	    ?MODULE:GeneratorFun()
    end,
    generator_loop(GeneratorFun, Delay, CadamNode).
		    


    
