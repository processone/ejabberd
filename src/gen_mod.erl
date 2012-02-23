%%%----------------------------------------------------------------------
%%% File    : gen_mod.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : 
%%% Created : 24 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(gen_mod).
-author('alexey@process-one.net').

-export([start/0,
	 start_module/3,
	 stop_module/2,
	 stop_module_keep_config/2,
	 get_opt/2,
	 get_opt/3,
	 get_opt_host/3,
	 get_module_opt/4,
	 get_module_opt_host/3,
	 loaded_modules/1,
	 loaded_modules_with_opts/1,
	 get_hosts/2,
	 get_module_proc/2,
	 get_module_proc_existing/2,
	 is_loaded/2]).

-export([behaviour_info/1]).

-include("ejabberd.hrl").

-record(ejabberd_module, {module_host, opts}).
%% module_host = {Module::atom(), Host::string()}

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
    MTokens = string:tokens(atom_to_list(Module), "_"),
    case lists:split(length(MTokens) - 1, MTokens) of
	{ModulePlainList, ["odbc"]} ->
	    Module2 = list_to_atom(string:join(ModulePlainList, "_")),
	    ?WARNING_MSG("The module ~p is obsolete. Replace it with ~p and "
			 "add the option {backend, odbc}",
			 [Module, Module2]),
	    start_module2(Host, Module2, [{backend, odbc} | Opts]);
	_ ->
	    start_module2(Host, Module, Opts)
    end.

start_module2(Host, Module, Opts) ->
    set_module_opts_mnesia(Host, Module, Opts),
    ets:insert(ejabberd_modules,
	       #ejabberd_module{module_host = {Module, Host},
				opts = Opts}),
    try Module:start(Host, Opts)
    catch Class:Reason ->
	    del_module_mnesia(Host, Module),
	    ets:delete(ejabberd_modules, {Module, Host}),
	    ErrorText = io_lib:format("Problem starting the module ~p for host ~p ~n options: ~p~n ~p: ~p~n stacktarce: ~p",
		    [Module, Host, Opts, Class, Reason, erlang:get_stacktrace()]),
	    ?CRITICAL_MSG(ErrorText, []),
	    case is_app_running(ejabberd) of
		true ->
		    erlang:raise(Class, Reason, erlang:get_stacktrace());
		false ->
		    ?CRITICAL_MSG("ejabberd initialization was aborted because a module start failed.", []),
		    timer:sleep(3000),
		    erlang:halt(string:substr(lists:flatten(ErrorText), 1, 199))
	    end
    end.

is_app_running(AppName) ->
    %% Use a high timeout to prevent a false positive in a high load system
    Timeout = 15000,
    lists:keymember(AppName, 1, application:which_applications(Timeout)).

%% @doc Stop the module in a host, and forget its configuration.
stop_module(Host, Module) ->
    case stop_module_keep_config(Host, Module) of
	error ->
	    error;
	ok ->
	    del_module_mnesia(Host, Module)
    end.

%% @doc Stop the module in a host, but keep its configuration.
%% As the module configuration is kept in the Mnesia local_config table,
%% when ejabberd is restarted the module will be started again.
%% This function is useful when ejabberd is being stopped
%% and it stops all modules.
stop_module_keep_config(Host, Module) ->
    case catch Module:stop(Host) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]),
	    error;
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
	    throw({undefined_option, Opt});
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

get_module_opt(global, Module, Opt, Default) ->
	Hosts = ?MYHOSTS,
	[Value | Values] = lists:map(
		fun(Host) ->
			get_module_opt(Host, Module, Opt, Default)
		end,
		Hosts),
	Same_all = lists:all(
		fun(Other_value) ->
			Other_value == Value
		end,
		Values),
	case Same_all of
		true -> Value;
		false -> Default
	end;

get_module_opt(Host, Module, Opt, Default) ->
    OptsList = ets:lookup(ejabberd_modules, {Module, Host}),
    case OptsList of
	[] ->
	    OptsList2 = ets:lookup(ejabberd_modules, {Module, global}),
	    case OptsList2 of
		[] ->
		    Default;
		[#ejabberd_module{opts = Opts} | _] ->
		    get_opt(Opt, Opts, Default)
	    end;
	[#ejabberd_module{opts = Opts} | _] ->
	    get_opt(Opt, Opts, Default)
    end.

get_module_opt_host(Host, Module, Default) ->
    Val = get_module_opt(Host, Module, host, Default),
    re:replace(Val, "@HOST@", Host, [global,{return,list}]).

get_opt_host(Host, Opts, Default) ->
    case Host of
	global ->
	    Val = get_opt(host, Opts, Default),
	    {global, re:replace(Val, ".@HOST@", "", [global,{return,list}])};
	Host ->
	    Val = get_opt(host, Opts, Default),
	    re:replace(Val, "@HOST@", Host, [global,{return,list}])
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

set_module_opts_mnesia(global, _Module, _Opts) ->
    %% Modules on the global host are usually static, so we shouldn't manipulate them.
    ok;
set_module_opts_mnesia(Host, Module, Opts) ->
    Modules = case ejabberd_config:get_local_option({modules, Host}) of
		  undefined ->
		      [];
		  Ls ->
		      Ls
	      end,
    Modules1 = lists:keydelete(Module, 1, Modules),
    Modules2 = [{Module, Opts} | Modules1],
    ejabberd_config:add_local_option({modules, Host}, Modules2).

del_module_mnesia(global, _Module) ->
    %% Modules on the global host are usually static, so we shouldn't manipulate them.
    ok;
del_module_mnesia(Host, Module) ->
    Modules = case ejabberd_config:get_local_option({modules, Host}) of
		  undefined ->
		      [];
		  Ls ->
		      Ls
	      end,
    Modules1 = lists:keydelete(Module, 1, Modules),
    ejabberd_config:add_local_option({modules, Host}, Modules1).

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

get_module_proc_existing(Host, Base) ->
    Proc = get_module_proc(Host, Base),
    %% If the process doesn't exist for Host, it may exist for global
    case {whereis(Proc), Host == global} of
	{undefined, false} -> get_module_proc(global, Base);
	{undefined, true} -> not_existing;
	{_, _} -> Proc
    end.

get_module_proc(Host, Base) when is_binary(Host) ->
    get_module_proc(binary_to_list(Host), Base);
get_module_proc(global, Base) ->
    list_to_atom(atom_to_list(Base) ++ "__global");
get_module_proc(Host, {frontend, Base}) ->
    get_module_proc("frontend_" ++ Host, Base);
get_module_proc(Host, Base) ->
    list_to_atom(atom_to_list(Base) ++ "_" ++ Host).

%% @spec(Host::string() | global, Module::atom()) -> true | false
%% @doc Check if the module is loaded in this host (or global), or not.
is_loaded(Host, Module) ->
    ets:member(ejabberd_modules, {Module, Host})
    orelse ets:member(ejabberd_modules, {Module, global}).
