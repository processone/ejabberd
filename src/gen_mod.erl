%%%----------------------------------------------------------------------
%%% File    : gen_mod.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 24 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(gen_mod).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0,
	 start_module/3,
	 stop_module/2,
	 get_opt/2,
	 get_opt/3,
	 get_module_opt/4,
	 loaded_modules/1,
	 loaded_modules_with_opts/1,
	 get_hosts/2,
	 get_module_proc/2,
	 is_loaded/2]).

-export([behaviour_info/1]).

-include("ejabberd.hrl").

-record(ejabberd_module, {module_host, opts}).

behaviour_info(callbacks) ->
    [{start, 2},
     {stop, 1}];
behaviour_info(_Other) ->
    undefined.

start() ->
    ets:new(ejabberd_modules, [named_table,
			       public,
			       {keypos, #ejabberd_module.module_host}]),
    ok.


start_module(Host, Module, Opts) ->
    case catch Module:start(Host, Opts) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	_ ->
	    ets:insert(ejabberd_modules,
		       #ejabberd_module{module_host = {Module, Host},
					opts = Opts}),
	    ok
    end.

stop_module(Host, Module) ->
    case catch Module:stop(Host) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	{wait, ProcList} when is_list(ProcList) ->
	    lists:foreach(fun wait_for_process/1, ProcList),
	    ets:delete(ejabberd_modules, {Module, Host}),
	    ok;
	{wait, Process} ->
	    wait_for_process(Process),
	    ets:delete(ejabberd_modules, {Module, Host}),
	    ok;
	_ ->
	    ets:delete(ejabberd_modules, {Module, Host}),
	    ok
    end.

wait_for_process(Process) ->
    MonitorReference = erlang:monitor(process, Process),
    wait_for_stop(Process, MonitorReference).

wait_for_stop(Process, MonitorReference) ->
    receive
	{'DOWN', MonitorReference, _Type, _Object, _Info} ->
	    ok
    after 5000 ->
	    catch exit(whereis(Process), kill),
	    wait_for_stop1(MonitorReference)
    end.

wait_for_stop1(MonitorReference) ->
    receive
	{'DOWN', MonitorReference, _Type, _Object, _Info} ->
	    ok
    after 5000 ->
	    ok
    end.

get_opt(Opt, Opts) ->
    case lists:keysearch(Opt, 1, Opts) of
	false ->
 	    % TODO: replace with more appropriate function
 	    [] = {undefined_option, Opt};
	{value, {_, Val}} ->
	    Val
    end.

get_opt(Opt, Opts, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
	false ->
	    Default;
	{value, {_, Val}} ->
	    Val
    end.

get_module_opt(Host, Module, Opt, Default) ->
    OptsList = ets:lookup(ejabberd_modules, {Module, Host}),
    case OptsList of
	[] ->
	    Default;
	[#ejabberd_module{opts = Opts} | _] ->
	    get_opt(Opt, Opts, Default)
    end.

loaded_modules(Host) ->
    ets:select(ejabberd_modules,
	       [{#ejabberd_module{_ = '_', module_host = {'$1', Host}},
		 [],
		 ['$1']}]).

loaded_modules_with_opts(Host) ->
    ets:select(ejabberd_modules,
	       [{#ejabberd_module{_ = '_', module_host = {'$1', Host},
				  opts = '$2'},
		 [],
		 [{{'$1', '$2'}}]}]).

get_hosts(Opts, Prefix) ->
    case catch gen_mod:get_opt(hosts, Opts) of
	{'EXIT', _Error1} ->
	    case catch gen_mod:get_opt(host, Opts) of
		{'EXIT', _Error2} ->
		    [Prefix ++ Host || Host <- ?MYHOSTS];
		Host ->
		    [Host]
	    end;
	Hosts ->
	    Hosts
    end.

get_module_proc(Host, Base) ->
    list_to_atom(atom_to_list(Base) ++ "_" ++ Host).

is_loaded(Host, Module) ->
    ets:member(ejabberd_modules, {Module, Host}).

