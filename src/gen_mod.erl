%%%----------------------------------------------------------------------
%%% File    : gen_mod.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose :
%%% Created : 24 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(gen_mod).

-behaviour(ejabberd_config).
-behaviour(supervisor).

-author('alexey@process-one.net').

-export([init/1, start_link/0, start_child/3, start_child/4,
	 stop_child/1, stop_child/2, config_reloaded/0]).
-export([start_module/2, start_module/3,
	 stop_module/2, stop_module_keep_config/2, get_opt/3,
	 get_opt/4, get_opt_host/3, opt_type/1, is_equal_opt/5,
	 get_module_opt/4, get_module_opt/5, get_module_opt_host/3,
	 loaded_modules/1, loaded_modules_with_opts/1,
	 get_hosts/2, get_module_proc/2, is_loaded/2, is_loaded_elsewhere/2,
	 start_modules/0, start_modules/1, stop_modules/0, stop_modules/1,
	 db_mod/2, db_mod/3, ram_db_mod/2, ram_db_mod/3,
	 db_type/2, db_type/3, ram_db_type/2, ram_db_type/3]).

%%-export([behaviour_info/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(ejabberd_module,
        {module_host = {undefined, <<"">>} :: {atom(), binary()},
         opts = [] :: opts() | '_' | '$2'}).

-type opts() :: [{atom(), any()}].
-type db_type() :: atom().

-callback start(binary(), opts()) -> ok | {ok, pid()}.
-callback stop(binary()) -> any().
-callback reload(binary(), opts(), opts()) -> ok | {ok, pid()}.
-callback mod_opt_type(atom()) -> fun((term()) -> term()) | [atom()].
-callback depends(binary(), opts()) -> [{module(), hard | soft}].

-optional_callbacks([reload/3]).

-export_type([opts/0]).
-export_type([db_type/0]).

-ifndef(GEN_SERVER).
-define(GEN_SERVER, gen_server).
-endif.

start_link() ->
    case supervisor:start_link({local, ejabberd_gen_mod_sup}, ?MODULE, []) of
	{ok, Pid} ->
	    gen_mod:start_modules(),
	    {ok, Pid};
	Err ->
	    Err
    end.

init([]) ->
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 50),
    ejabberd_hooks:add(host_up, ?MODULE, start_modules, 40),
    ejabberd_hooks:add(host_down, ?MODULE, stop_modules, 70),
    ets:new(ejabberd_modules,
	    [named_table, public,
	     {keypos, #ejabberd_module.module_host},
	     {read_concurrency, true}]),
    {ok, {{one_for_one, 10, 1}, []}}.

-spec start_child(module(), binary() | global, opts()) -> ok | {error, any()}.
start_child(Mod, Host, Opts) ->
    start_child(Mod, Host, Opts, get_module_proc(Host, Mod)).

-spec start_child(module(), binary() | global, opts(), atom()) -> ok | {error, any()}.
start_child(Mod, Host, Opts, Proc) ->
    Spec = {Proc, {?GEN_SERVER, start_link,
		   [{local, Proc}, Mod, [Host, Opts], []]},
            transient, 2000, worker, [Mod]},
    supervisor:start_child(ejabberd_gen_mod_sup, Spec).

-spec stop_child(module(), binary() | global) -> ok | {error, any()}.
stop_child(Mod, Host) ->
    stop_child(get_module_proc(Host, Mod)).

-spec stop_child(atom()) -> ok | {error, any()}.
stop_child(Proc) ->
    supervisor:terminate_child(ejabberd_gen_mod_sup, Proc),
    supervisor:delete_child(ejabberd_gen_mod_sup, Proc).

-spec start_modules() -> any().

%% Start all the modules in all the hosts
start_modules() ->
    lists:foreach(
	fun(Host) ->
	    start_modules(Host)
	end, ?MYHOSTS).

get_modules_options(Host) ->
    ejabberd_config:get_option(
      {modules, Host},
      fun(Mods) ->
	      lists:map(
		fun({M, A}) when is_atom(M), is_list(A) ->
			{M, A}
		end, Mods)
      end, []).

sort_modules(Host, ModOpts) ->
    G = digraph:new([acyclic]),
    lists:foreach(
      fun({Mod, Opts}) ->
	      digraph:add_vertex(G, Mod, Opts),
	      Deps = try Mod:depends(Host, Opts) catch _:undef -> [] end,
	      lists:foreach(
		fun({DepMod, Type}) ->
			case lists:keyfind(DepMod, 1, ModOpts) of
			    false when Type == hard ->
				ErrTxt = io_lib:format(
					   "failed to load module '~s' "
					   "because it depends on module '~s' "
					   "which is not found in the config",
					   [Mod, DepMod]),
				?ERROR_MSG(ErrTxt, []),
				digraph:del_vertex(G, Mod),
				maybe_halt_ejabberd(ErrTxt);
			    false when Type == soft ->
				?WARNING_MSG("module '~s' is recommended for "
					     "module '~s' but is not found in "
					     "the config",
					     [DepMod, Mod]);
			    {DepMod, DepOpts} ->
				digraph:add_vertex(G, DepMod, DepOpts),
				case digraph:add_edge(G, DepMod, Mod) of
				    {error, {bad_edge, Path}} ->
					?WARNING_MSG("cyclic dependency detected "
						     "between modules: ~p",
						     [Path]);
				    _ ->
					ok
				end
			end
		end, Deps)
      end, ModOpts),
    [digraph:vertex(G, V) || V <- digraph_utils:topsort(G)].

-spec start_modules(binary()) -> ok.

start_modules(Host) ->
    Modules = sort_modules(Host, get_modules_options(Host)),
    lists:foreach(
	fun({Module, Opts}) ->
	    start_module(Host, Module, Opts)
	end, Modules).

-spec start_module(binary(), atom()) -> ok | {ok, pid()} | {error, not_found_in_config}.

start_module(Host, Module) ->
    Modules = get_modules_options(Host),
    case lists:keyfind(Module, 1, Modules) of
	{_, Opts} ->
	    start_module(Host, Module, Opts);
	false ->
	    {error, not_found_in_config}
    end.

-spec start_module(binary(), atom(), opts()) -> ok | {ok, pid()}.

start_module(Host, Module, Opts0) ->
    ?DEBUG("loading ~s at ~s", [Module, Host]),
    Opts = validate_opts(Module, Opts0),
    store_options(Host, Module, Opts),
    try case Module:start(Host, Opts) of
	    ok -> ok;
	    {ok, Pid} when is_pid(Pid) -> {ok, Pid};
	    Err -> erlang:error(Err)
	end
    catch Class:Reason ->
	  ets:delete(ejabberd_modules, {Module, Host}),
	  ErrorText =
	      io_lib:format("Problem starting the module ~s for host "
			    "~s ~n options: ~p~n ~p: ~p~n~p",
			    [Module, Host, Opts, Class, Reason,
			     erlang:get_stacktrace()]),
	  ?CRITICAL_MSG(ErrorText, []),
          maybe_halt_ejabberd(ErrorText),
	  erlang:raise(Class, Reason, erlang:get_stacktrace())
    end.

-spec reload_modules(binary()) -> ok.
reload_modules(Host) ->
    NewMods = ejabberd_config:get_option(
		{modules, Host}, opt_type(modules), []),
    OldMods = ets:select(
		ejabberd_modules,
		ets:fun2ms(
		  fun(#ejabberd_module{module_host = {M, H}, opts = O})
			when H == Host -> {M, O}
		  end)),
    lists:foreach(
      fun({Mod, _Opts}) ->
	      case lists:keymember(Mod, 1, NewMods) of
		  false ->
		      stop_module(Host, Mod);
		  true ->
		      ok
	      end
      end, OldMods),
    lists:foreach(
      fun({Mod, Opts}) ->
	      case lists:keymember(Mod, 1, OldMods) of
		  false ->
		      start_module(Host, Mod, Opts);
		  true ->
		      ok
	      end
      end, NewMods),
    lists:foreach(
      fun({Mod, OldOpts}) ->
	      case lists:keyfind(Mod, 1, NewMods) of
		  {_, NewOpts} when NewOpts /= OldOpts ->
		      reload_module(Host, Mod, NewOpts, OldOpts);
		  _ ->
		      ok
	      end
      end, OldMods).

-spec reload_module(binary(), module(), opts(), opts()) -> ok | {ok, pid()}.
reload_module(Host, Module, NewOpts0, OldOpts) ->
    case erlang:function_exported(Module, reload, 3) of
	true ->
	    ?DEBUG("reloading ~s at ~s", [Module, Host]),
	    NewOpts = validate_opts(Module, NewOpts0),
	    store_options(Host, Module, NewOpts),
	    try case Module:reload(Host, NewOpts, OldOpts) of
		    ok -> ok;
		    {ok, Pid} when is_pid(Pid) -> {ok, Pid};
		    Err -> erlang:error(Err)
		end
	    catch Class:Reason ->
		    StackTrace = erlang:get_stacktrace(),
		    ?CRITICAL_MSG("failed to reload module ~s at ~s:~n"
				  "** Reason = ~p",
				  [Module, Host,
				   {Class, {Reason, StackTrace}}]),
		    erlang:raise(Class, Reason, StackTrace)
	    end;
	false ->
	    ?WARNING_MSG("module ~s doesn't support reloading "
			 "and will be restarted", [Module]),
	    stop_module(Host, Module),
	    start_module(Host, Module, NewOpts0)
    end.

-spec store_options(binary(), module(), opts()) -> true.
store_options(Host, Module, Opts) ->
    ets:insert(ejabberd_modules,
	       #ejabberd_module{module_host = {Module, Host},
				opts = Opts}).

maybe_halt_ejabberd(ErrorText) ->
    case is_app_running(ejabberd) of
	false ->
	    ?CRITICAL_MSG("ejabberd initialization was aborted "
			  "because a module start failed.",
			  []),
	    timer:sleep(3000),
	    erlang:halt(string:substr(lists:flatten(ErrorText), 1, 199));
	true ->
	    ok
    end.

is_app_running(AppName) ->
    Timeout = 15000,
    lists:keymember(AppName, 1,
		    application:which_applications(Timeout)).

-spec stop_modules() -> ok.

stop_modules() ->
    lists:foreach(
	fun(Host) ->
	    stop_modules(Host)
	end, ?MYHOSTS).

-spec stop_modules(binary()) -> ok.

stop_modules(Host) ->
    Modules = get_modules_options(Host),
    lists:foreach(
	fun({Module, _Args}) ->
	    gen_mod:stop_module_keep_config(Host, Module)
	end, Modules).

-spec stop_module(binary(), atom()) -> error | {aborted, any()} | {atomic, any()}.

stop_module(Host, Module) ->
    ?DEBUG("stopping ~s at ~s", [Module, Host]),
    case stop_module_keep_config(Host, Module) of
      error -> error;
      ok -> ok
    end.

-spec stop_module_keep_config(binary(), atom()) -> error | ok.

stop_module_keep_config(Host, Module) ->
    case catch Module:stop(Host) of
      {'EXIT', Reason} -> ?ERROR_MSG("~p", [Reason]), error;
      {wait, ProcList} when is_list(ProcList) ->
	  lists:foreach(fun wait_for_process/1, ProcList),
	  ets:delete(ejabberd_modules, {Module, Host}),
	  ok;
      {wait, Process} ->
	  wait_for_process(Process),
	  ets:delete(ejabberd_modules, {Module, Host}),
	  ok;
      _ -> ets:delete(ejabberd_modules, {Module, Host}), ok
    end.

wait_for_process(Process) ->
    MonitorReference = erlang:monitor(process, Process),
    wait_for_stop(Process, MonitorReference).

wait_for_stop(Process, MonitorReference) ->
    receive
      {'DOWN', MonitorReference, _Type, _Object, _Info} -> ok
      after 5000 ->
		catch exit(whereis(Process), kill),
		wait_for_stop1(MonitorReference)
    end.

wait_for_stop1(MonitorReference) ->
    receive
      {'DOWN', MonitorReference, _Type, _Object, _Info} -> ok
      after 5000 -> ok
    end.

-type check_fun() :: fun((any()) -> any()) | {module(), atom()}.

-spec get_opt(atom() | {atom(), binary()|global}, opts(), check_fun()) -> any().

get_opt(Opt, Opts, F) ->
    get_opt(Opt, Opts, F, undefined).

-spec get_opt(atom() | {atom(), binary()|global}, opts(), check_fun(), any()) -> any().

get_opt({Opt, Host}, Opts, F, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
        false ->
            ejabberd_config:get_option({Opt, Host}, F, Default);
        {value, {_, Val}} ->
            ejabberd_config:prepare_opt_val(Opt, Val, F, Default)
    end;
get_opt(Opt, Opts, F, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
        false ->
            Default;
        {value, {_, Val}} ->
            ejabberd_config:prepare_opt_val(Opt, Val, F, Default)
    end.

-spec get_module_opt(global | binary(), atom(), atom(), check_fun()) -> any().

get_module_opt(Host, Module, Opt, F) ->
    get_module_opt(Host, Module, Opt, F, undefined).

-spec get_module_opt(global | binary(), atom(), atom(), check_fun(), any()) -> any().

get_module_opt(global, Module, Opt, F, Default) ->
    Hosts = (?MYHOSTS),
    [Value | Values] = lists:map(fun (Host) ->
					 get_module_opt(Host, Module, Opt,
							F, Default)
				 end,
				 Hosts),
    Same_all = lists:all(fun (Other_value) ->
				 Other_value == Value
			 end,
			 Values),
    case Same_all of
      true -> Value;
      false -> Default
    end;
get_module_opt(Host, Module, Opt, F, Default) ->
    OptsList = ets:lookup(ejabberd_modules, {Module, Host}),
    case OptsList of
      [] -> Default;
      [#ejabberd_module{opts = Opts} | _] ->
	  get_opt(Opt, Opts, F, Default)
    end.

-spec get_module_opt_host(global | binary(), atom(), binary()) -> binary().

get_module_opt_host(Host, Module, Default) ->
    Val = get_module_opt(Host, Module, host,
                         fun iolist_to_binary/1,
                         Default),
    ejabberd_regexp:greplace(Val, <<"@HOST@">>, Host).

-spec get_opt_host(binary(), opts(), binary()) -> binary().

get_opt_host(Host, Opts, Default) ->
    Val = get_opt(host, Opts, fun iolist_to_binary/1, Default),
    ejabberd_regexp:greplace(Val, <<"@HOST@">>, Host).


get_module_mod_opt_type_fun(Module) ->
    DBSubMods = ejabberd_config:v_dbs_mods(Module),
    fun(Opt) ->
	    Res = lists:foldl(fun(Mod, {Funs, ArgsList, _} = Acc) ->
				      case catch Mod:mod_opt_type(Opt) of
					  Fun when is_function(Fun) ->
					      {[Fun | Funs], ArgsList, true};
					  L when is_list(L) ->
					      {Funs, L ++ ArgsList, true};
					  _ ->
					      Acc
				      end
			      end, {[], [], false}, [Module | DBSubMods]),
	    case Res of
		{[], [], false} ->
		    throw({'EXIT', {undef, mod_opt_type}});
		{[], Args, _} -> Args;
		{Funs, _, _} ->
		    fun(Val) ->
			    lists:any(fun(F) ->
					      try F(Val) of
						  _ ->
						      true
					      catch {replace_with, _NewVal} = E ->
						      throw(E);
						    {invalid_syntax, _Error} = E2 ->
						      throw(E2);
						    _:_ ->
						      false
					      end
				      end, Funs)
		    end
	    end
    end.

validate_opts(Module, Opts) ->
    ModOptFun = get_module_mod_opt_type_fun(Module),
    lists:filtermap(
      fun({Opt, Val}) ->
	      case catch ModOptFun(Opt) of
		  VFun when is_function(VFun) ->
		      try VFun(Val) of
			  _ ->
			      true
		      catch {replace_with, NewVal} ->
			      {true, {Opt, NewVal}};
			    {invalid_syntax, Error} ->
			      ?ERROR_MSG("ignoring invalid value '~p' for "
					 "option '~s' of module '~s': ~s",
					 [Val, Opt, Module, Error]),
			      false;
			    _:_ ->
			      ?ERROR_MSG("ignoring invalid value '~p' for "
					 "option '~s' of module '~s'",
					 [Val, Opt, Module]),
			      false
		      end;
		  L when is_list(L) ->
		      SOpts = str:join([[$', atom_to_list(A), $'] || A <- L], <<", ">>),
		      ?ERROR_MSG("unknown option '~s' for module '~s' will be"
				 " likely ignored, available options are: ~s",
				 [Opt, Module, SOpts]),
		      true;
		  {'EXIT', {undef, _}} ->
		      ?WARNING_MSG("module '~s' doesn't export mod_opt_type/1",
				   [Module]),
		      true
	      end;
	 (Junk) ->
	      ?ERROR_MSG("failed to understand option ~p for module '~s'",
			 [Junk, Module]),
	      false
      end, Opts).

-spec db_type(binary() | global, module()) -> db_type();
	     (opts(), module()) -> db_type().

db_type(Opts, Module) when is_list(Opts) ->
    db_type(global, Opts, Module);
db_type(Host, Module) when is_atom(Module) ->
    case catch Module:mod_opt_type(db_type) of
	F when is_function(F) ->
	    case get_module_opt(Host, Module, db_type, F) of
		undefined -> ejabberd_config:default_db(Host, Module);
		Type -> Type
	    end;
	_ ->
	    undefined
    end.

-spec db_type(binary() | global, opts(), module()) -> db_type().

db_type(Host, Opts, Module) ->
    case catch Module:mod_opt_type(db_type) of
	F when is_function(F) ->
	    case get_opt(db_type, Opts, F) of
		undefined -> ejabberd_config:default_db(Host, Module);
		Type -> Type
	    end;
	_ ->
	    undefined
    end.

-spec db_mod(binary() | global | db_type(), module()) -> module().

db_mod(Type, Module) when is_atom(Type) ->
    list_to_atom(atom_to_list(Module) ++ "_" ++ atom_to_list(Type));
db_mod(Host, Module) when is_binary(Host) orelse Host == global ->
    db_mod(db_type(Host, Module), Module).

-spec db_mod(binary() | global, opts(), module()) -> module().

db_mod(Host, Opts, Module) when is_list(Opts) ->
    db_mod(db_type(Host, Opts, Module), Module).

-spec ram_db_type(binary() | global, module()) -> db_type();
		 (opts(), module()) -> db_type().
ram_db_type(Opts, Module) when is_list(Opts) ->
    ram_db_type(global, Opts, Module);
ram_db_type(Host, Module) when is_atom(Module) ->
    case catch Module:mod_opt_type(ram_db_type) of
	F when is_function(F) ->
	    case get_module_opt(Host, Module, ram_db_type, F) of
		undefined -> ejabberd_config:default_ram_db(Host, Module);
		Type -> Type
	    end;
	_ ->
	    undefined
    end.

-spec ram_db_type(binary() | global, opts(), module()) -> db_type().
ram_db_type(Host, Opts, Module) ->
    case catch Module:mod_opt_type(ram_db_type) of
	F when is_function(F) ->
	    case get_opt(ram_db_type, Opts, F) of
		undefined -> ejabberd_config:default_ram_db(Host, Module);
		Type -> Type
	    end;
	_ ->
	    undefined
    end.

-spec ram_db_mod(binary() | global | db_type(), module()) -> module().
ram_db_mod(Type, Module) when is_atom(Type), Type /= global ->
    list_to_atom(atom_to_list(Module) ++ "_" ++ atom_to_list(Type));
ram_db_mod(Host, Module) when is_binary(Host) orelse Host == global ->
    ram_db_mod(ram_db_type(Host, Module), Module).

-spec ram_db_mod(binary() | global, opts(), module()) -> module().
ram_db_mod(Host, Opts, Module) when is_list(Opts) ->
    ram_db_mod(ram_db_type(Host, Opts, Module), Module).

-spec loaded_modules(binary()) -> [atom()].

loaded_modules(Host) ->
    ets:select(ejabberd_modules,
	       [{#ejabberd_module{_ = '_', module_host = {'$1', Host}},
		 [], ['$1']}]).

-spec loaded_modules_with_opts(binary()) -> [{atom(), opts()}].

loaded_modules_with_opts(Host) ->
    ets:select(ejabberd_modules,
	       [{#ejabberd_module{_ = '_', module_host = {'$1', Host},
				  opts = '$2'},
		 [], [{{'$1', '$2'}}]}]).

-spec get_hosts(opts(), binary()) -> [binary()].

get_hosts(Opts, Prefix) ->
    case get_opt(hosts, Opts,
                 fun(Hs) -> [iolist_to_binary(H) || H <- Hs] end) of
        undefined ->
            case get_opt(host, Opts,
                         fun iolist_to_binary/1) of
                undefined ->
                    [<<Prefix/binary, Host/binary>> || Host <- ?MYHOSTS];
                Host ->
                    [Host]
            end;
        Hosts ->
            Hosts
    end.

-spec get_module_proc(binary(), {frontend, atom()} | atom()) -> atom().
get_module_proc(Host, {frontend, Base}) ->
    get_module_proc(<<"frontend_", Host/binary>>, Base);
get_module_proc(Host, Base) ->
    binary_to_atom(
      <<(erlang:atom_to_binary(Base, latin1))/binary, "_", Host/binary>>,
      latin1).

-spec is_loaded(binary(), atom()) -> boolean().

is_loaded(Host, Module) ->
    ets:member(ejabberd_modules, {Module, Host}).

-spec is_loaded_elsewhere(binary(), atom()) -> boolean().
is_loaded_elsewhere(Host, Module) ->
    ets:select_count(
      ejabberd_modules,
      ets:fun2ms(
	fun(#ejabberd_module{module_host = {Mod, H}}) ->
		(Mod == Module) and (H /= Host)
	end)) /= 0.

-spec config_reloaded() -> ok.
config_reloaded() ->
    lists:foreach(
      fun(Host) ->
	      reload_modules(Host)
      end, ?MYHOSTS).

-spec is_equal_opt(atom(), opts(), opts(), check_fun(), any()) ->
			  true | {false, any(), any()}.
is_equal_opt(Opt, NewOpts, OldOpts, VFun, Default) ->
    NewVal = get_opt(Opt, NewOpts, VFun, Default),
    OldVal = get_opt(Opt, OldOpts, VFun, Default),
    if NewVal /= OldVal ->
	    {false, NewVal, OldVal};
       true ->
	    true
    end.

opt_type(modules) ->
    fun(Mods) ->
	    lists:map(
	      fun({M, A}) when is_atom(M), is_list(A) ->
		      {M, A}
	      end, Mods)
    end;
opt_type(_) -> [modules].
