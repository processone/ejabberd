%%%----------------------------------------------------------------------
%%% File    : gen_mod.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose :
%%% Created : 24 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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
-export([start_module/2, stop_module/2, stop_module_keep_config/2,
	 get_opt/2, get_opt_hosts/2, opt_type/1, is_equal_opt/3,
	 get_module_opt/3, get_module_opt_host/3,
	 loaded_modules/1, loaded_modules_with_opts/1,
	 get_hosts/2, get_module_proc/2, is_loaded/2, is_loaded_elsewhere/2,
	 start_modules/0, start_modules/1, stop_modules/0, stop_modules/1,
	 db_mod/2, db_mod/3, ram_db_mod/2, ram_db_mod/3,
	 is_db_configured/2]).

%% Deprecated functions
-export([get_opt/3, get_opt/4, get_module_opt/4, get_module_opt/5,
	 get_opt_host/3, get_opt_hosts/3, db_type/2, db_type/3,
	 ram_db_type/2, ram_db_type/3]).
-deprecated([{get_opt, 3},
	     {get_opt, 4},
	     {get_opt_host, 3},
	     {get_opt_hosts, 3},
	     {get_module_opt, 4},
	     {get_module_opt, 5},
	     {db_type, 2},
	     {db_type, 3},
	     {ram_db_type, 2},
	     {ram_db_type, 3}]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(ejabberd_module,
        {module_host = {undefined, <<"">>} :: {atom(), binary()},
         opts = [] :: opts() | '_' | '$2',
	 order = 0 :: integer()}).

-type opts() :: [{atom(), any()}].
-type db_type() :: atom().

-callback start(binary(), opts()) -> ok | {ok, pid()}.
-callback stop(binary()) -> any().
-callback reload(binary(), opts(), opts()) -> ok | {ok, pid()}.
-callback mod_opt_type(atom()) -> fun((term()) -> term()) | [atom()].
-callback mod_options(binary()) -> opts().
-callback depends(binary(), opts()) -> [{module(), hard | soft}].

-optional_callbacks([mod_opt_type/1, reload/3]).

-export_type([opts/0]).
-export_type([db_type/0]).

-ifndef(GEN_SERVER).
-define(GEN_SERVER, gen_server).
-endif.

start_link() ->
    case supervisor:start_link({local, ejabberd_gen_mod_sup}, ?MODULE, []) of
	{ok, Pid} ->
	    start_modules(),
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
            transient, timer:minutes(1), worker, [Mod]},
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
    sort_modules(Host, ejabberd_config:get_option({modules, Host}, [])).

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
					   "Failed to load module '~s' "
					   "because it depends on module '~s' "
					   "which is not found in the config",
					   [Mod, DepMod]),
				?ERROR_MSG(ErrTxt, []),
				digraph:del_vertex(G, Mod),
				maybe_halt_ejabberd();
			    false when Type == soft ->
				?WARNING_MSG("Module '~s' is recommended for "
					     "module '~s' but is not found in "
					     "the config",
					     [DepMod, Mod]);
			    {DepMod, DepOpts} ->
				digraph:add_vertex(G, DepMod, DepOpts),
				case digraph:add_edge(G, DepMod, Mod) of
				    {error, {bad_edge, Path}} ->
					?WARNING_MSG("Cyclic dependency detected "
						     "between modules: ~p",
						     [Path]);
				    _ ->
					ok
				end
			end
		end, Deps)
      end, ModOpts),
    {Result, _} = lists:mapfoldl(
		    fun(V, Order) ->
			    {M, O} = digraph:vertex(G, V),
			    {{M, O, Order}, Order+1}
		    end, 1, digraph_utils:topsort(G)),
    digraph:delete(G),
    Result.

-spec start_modules(binary()) -> ok.

start_modules(Host) ->
    Modules = get_modules_options(Host),
    lists:foreach(
	fun({Module, Opts, Order}) ->
	    start_module(Host, Module, Opts, Order)
	end, Modules).

-spec start_module(binary(), atom()) -> ok | {ok, pid()} | {error, not_found_in_config}.

start_module(Host, Module) ->
    Modules = get_modules_options(Host),
    case lists:keyfind(Module, 1, Modules) of
	{_, Opts, Order} ->
	    start_module(Host, Module, Opts, Order);
	false ->
	    {error, not_found_in_config}
    end.

-spec start_module(binary(), atom(), opts(), integer()) -> ok | {ok, pid()}.
start_module(Host, Module, Opts, Order) ->
    start_module(Host, Module, Opts, Order, true).

-spec start_module(binary(), atom(), opts(), integer(), boolean()) -> ok | {ok, pid()}.
start_module(Host, Module, Opts0, Order, NeedValidation) ->
    ?DEBUG("Loading ~s at ~s", [Module, Host]),
    Res = if NeedValidation ->
		  validate_opts(Host, Module, Opts0);
	     true ->
		  {ok, Opts0}
	  end,
    case Res of
	{ok, Opts} ->
	    store_options(Host, Module, Opts, Order),
	    try case Module:start(Host, Opts) of
		    ok -> ok;
		    {ok, Pid} when is_pid(Pid) -> {ok, Pid};
		    Err -> erlang:error(Err)
		end
	    catch Class:Reason ->
		    ets:delete(ejabberd_modules, {Module, Host}),
		    ErrorText =
			case Reason == undef andalso
			     code:ensure_loaded(Module) /= {module, Module} of
			    true ->
				io_lib:format("Failed to load unknown module "
					      "~s for host ~s: make sure "
					      "there is no typo and ~s.beam "
					      "exists inside either ~s or ~s "
					      "directory",
					      [Module, Host, Module,
					       filename:dirname(code:which(?MODULE)),
					       ext_mod:modules_dir()]);
			    false ->
				io_lib:format("Problem starting the module ~s for host "
					      "~s ~n options: ~p~n ~p: ~p~n~p",
					      [Module, Host, Opts, Class, Reason,
					       erlang:get_stacktrace()])
			end,
		    ?CRITICAL_MSG(ErrorText, []),
		    maybe_halt_ejabberd(),
		    erlang:raise(Class, Reason, erlang:get_stacktrace())
	    end;
	{error, _ErrorText} ->
	    maybe_halt_ejabberd()
    end.

-spec reload_modules(binary()) -> ok.
reload_modules(Host) ->
    NewMods = get_modules_options(Host),
    OldMods = lists:reverse(loaded_modules_with_opts(Host)),
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
      fun({Mod, Opts, Order}) ->
	      case lists:keymember(Mod, 1, OldMods) of
		  false ->
		      start_module(Host, Mod, Opts, Order);
		  true ->
		      ok
	      end
      end, NewMods),
    lists:foreach(
      fun({Mod, OldOpts}) ->
	      case lists:keyfind(Mod, 1, NewMods) of
		  {_, NewOpts0, Order} ->
		      case validate_opts(Host, Mod, NewOpts0) of
			  {ok, OldOpts} ->
			      ok;
			  {ok, NewOpts} ->
			      reload_module(Host, Mod, NewOpts, OldOpts, Order);
			  {error, _} ->
			      ok
		      end;
		  _ ->
		      ok
	      end
      end, OldMods).

-spec reload_module(binary(), module(), opts(), opts(), integer()) -> ok | {ok, pid()}.
reload_module(Host, Module, NewOpts, OldOpts, Order) ->
    case erlang:function_exported(Module, reload, 3) of
	true ->
	    ?DEBUG("Reloading ~s at ~s", [Module, Host]),
	    store_options(Host, Module, NewOpts, Order),
	    try case Module:reload(Host, NewOpts, OldOpts) of
		    ok -> ok;
		    {ok, Pid} when is_pid(Pid) -> {ok, Pid};
		    Err -> erlang:error(Err)
		end
	    catch Class:Reason ->
		    StackTrace = erlang:get_stacktrace(),
		    ?CRITICAL_MSG("Failed to reload module ~s at ~s:~n"
				  "** Reason = ~p",
				  [Module, Host,
				   {Class, {Reason, StackTrace}}]),
		    erlang:raise(Class, Reason, StackTrace)
	    end;
	false ->
	    ?WARNING_MSG("Module ~s doesn't support reloading "
			 "and will be restarted", [Module]),
	    stop_module(Host, Module),
	    start_module(Host, Module, NewOpts, Order, false)
    end.

-spec store_options(binary(), module(), opts(), integer()) -> true.
store_options(Host, Module, Opts, Order) ->
    ets:insert(ejabberd_modules,
	       #ejabberd_module{module_host = {Module, Host},
				opts = Opts, order = Order}).

maybe_halt_ejabberd() ->
    case is_app_running(ejabberd) of
	false ->
	    ?CRITICAL_MSG("ejabberd initialization was aborted "
			  "because a module start failed.",
			  []),
	    ejabberd:halt();
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
    Modules = lists:reverse(loaded_modules_with_opts(Host)),
    lists:foreach(
	fun({Module, _Args}) ->
		stop_module_keep_config(Host, Module)
	end, Modules).

-spec stop_module(binary(), atom()) -> error | {aborted, any()} | {atomic, any()}.

stop_module(Host, Module) ->
    case stop_module_keep_config(Host, Module) of
      error -> error;
      ok -> ok
    end.

-spec stop_module_keep_config(binary(), atom()) -> error | ok.

stop_module_keep_config(Host, Module) ->
    ?DEBUG("Stopping ~s at ~s", [Module, Host]),
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

-spec get_opt(atom(), opts()) -> any().
get_opt(Opt, Opts) ->
    case lists:keyfind(Opt, 1, Opts) of
	{_, Val} -> Val;
	false ->
	    ?DEBUG("Attempt to read unspecified option ~s", [Opt]),
	    undefined
    end.

-spec get_opt(atom(), opts(), check_fun() | any()) -> any().

get_opt(Opt, Opts, F) when is_function(F) ->
    get_opt(Opt, Opts, undefined);
get_opt(Opt, Opts, Default) ->
    case lists:keyfind(Opt, 1, Opts) of
        false ->
            Default;
        {_, Val} ->
	    Val
    end.

-spec get_opt(atom() | {atom(), binary()}, opts(), check_fun(), any()) -> any().
get_opt(Opt, Opts, _, Default) ->
    get_opt(Opt, Opts, Default).

-spec get_module_opt(global | binary(), atom(), atom()) -> any().

get_module_opt(Host, Module, Opt) ->
    get_module_opt(Host, Module, Opt, undefined).

-spec get_module_opt(global | binary(), atom(), atom(), any()) -> any().

get_module_opt(Host, Module, Opt, F) when is_function(F) ->
    get_module_opt(Host, Module, Opt, undefined);
get_module_opt(global, Module, Opt, Default) ->
    Hosts = (?MYHOSTS),
    [Value | Values] = lists:map(fun (Host) ->
					 get_module_opt(Host, Module, Opt,
							Default)
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
get_module_opt(Host, Module, Opt, Default) ->
    OptsList = ets:lookup(ejabberd_modules, {Module, Host}),
    case OptsList of
      [] -> Default;
      [#ejabberd_module{opts = Opts} | _] ->
	  get_opt(Opt, Opts, Default)
    end.

-spec get_module_opt(global | binary(), atom(), atom(), check_fun(), any()) -> any().
get_module_opt(Host, Module, Opt, _, Default) ->
    get_module_opt(Host, Module, Opt, Default).

-spec get_module_opt_host(global | binary(), atom(), binary()) -> binary().

get_module_opt_host(Host, Module, Default) ->
    Val = get_module_opt(Host, Module, host, Default),
    ejabberd_regexp:greplace(Val, <<"@HOST@">>, Host).

-spec get_opt_host(binary(), opts(), binary()) -> binary().

get_opt_host(Host, Opts, Default) ->
    Val = get_opt(host, Opts, Default),
    ejabberd_regexp:greplace(Val, <<"@HOST@">>, Host).

-spec get_opt_hosts(binary(), opts()) -> [binary()].
get_opt_hosts(Host, Opts) ->
    get_opt_hosts(Host, Opts, undefined).

-spec get_opt_hosts(binary(), opts(), binary()) -> [binary()].
get_opt_hosts(Host, Opts, Default) ->
    Vals = case get_opt(hosts, Opts) of
	       L when L == [] orelse L == undefined ->
		   case get_opt(host, Opts) of
		       undefined -> [Default];
		       H -> [H]
		   end;
	       L ->
		   L
	   end,
    [ejabberd_regexp:greplace(V, <<"@HOST@">>, Host) || V <- Vals].

-spec get_validators(binary(), {module(), [module()]}) -> list() | undef.
get_validators(Host, {Module, SubMods}) ->
    Validators =
	dict:to_list(
	  lists:foldl(
	    fun(Mod, D) ->
		    try list_known_opts(Host, Mod) of
			Os ->
			    lists:foldl(
			      fun({Opt, SubOpt} = O, Acc) ->
				      SubF = Mod:mod_opt_type(O),
				      F = try Mod:mod_opt_type(Opt)
					  catch _:_ -> fun(X) -> X end
					  end,
				      dict:append_list(
					Opt, [F, {SubOpt, [SubF]}], Acc);
				 (O, Acc) ->
				      F = Mod:mod_opt_type(O),
				      dict:store(O, [F], Acc)
			      end, D, Os)
		    catch _:undef ->
			    D
		    end
	    end, dict:new(), [Module|SubMods])),
    case Validators of
	[] ->
	    case have_validators(Module) of
		false ->
		    case code:ensure_loaded(Module) of
			{module, _} ->
			    ?WARNING_MSG("Third-party module '~s' doesn't export "
					 "options validator; consider to upgrade "
					 "the module", [Module]);
			_ ->
			    %% Silently ignore this, the error will be
			    %% generated later
			    ok
		    end,
		    undef;
		true ->
		    []
	    end;
	_ ->
	    Validators
    end.

-spec have_validators(module()) -> boolean().
have_validators(Module) ->
    erlang:function_exported(Module, mod_options, 1)
	orelse erlang:function_exported(Module, mod_opt_type, 1).

-spec validate_opts(binary(), module(), opts()) -> {ok, opts()} | {error, string()}.
validate_opts(Host, Module, Opts0) ->
    SubMods = get_submodules(Host, Module, Opts0),
    DefaultOpts = lists:flatmap(
		    fun(M) ->
			    try M:mod_options(Host)
			    catch _:undef -> []
			    end
		    end, [Module|SubMods]),
    Required = lists:filter(fun is_atom/1, DefaultOpts),
    try
	Opts = merge_opts(Opts0, DefaultOpts, Module),
	{ok, case get_validators(Host, {Module, SubMods}) of
		 undef ->
		     Opts;
		 Validators ->
		     Opts1 = validate_opts(Host, Module, Opts, Required, Validators),
		     remove_duplicated_opts(Opts1)
	     end}
    catch _:{missing_required_option, Opt} ->
	    ErrTxt = io_lib:format("Module '~s' is missing required option '~s'",
				   [Module, Opt]),
	    ?ERROR_MSG(ErrTxt, []),
	    {error, ErrTxt}
    end.

validate_opts(Host, Module, Opts, Required, Validators) when is_list(Opts) ->
    lists:flatmap(
      fun({Opt, Val}) when is_atom(Opt) ->
	      case lists:keyfind(Opt, 1, Validators) of
		  {_, L} ->
		      case lists:partition(fun is_function/1, L) of
			  {[VFun|_], []} ->
			      validate_opt(Module, Opt, Val, Required, VFun);
			  {[VFun|_], SubValidators} ->
			      try validate_opts(Host, Module, Val, Required, SubValidators) of
				  SubOpts ->
				      validate_opt(Module, Opt, SubOpts, Required, VFun)
			      catch _:bad_option ->
				      ?ERROR_MSG("Ignoring invalid value '~p' for "
						 "option '~s' of module '~s'",
						 [Val, Opt, Module]),
				      fail_if_option_is_required(Opt, Required),
				      []
			      end
		      end;
		  false ->
		      case Validators of
			  [] ->
			      ?ERROR_MSG("Ignoring unknown option '~s' of '~s':"
					 " the module doesn't have any options",
					 [Opt, Module]);
			  _ ->
			      ?ERROR_MSG("Ignoring unknown option '~s' of '~s',"
					 " available options are: ~s",
					 [Opt, Module,
					  misc:join_atoms(
					    [K || {K, _} <- Validators],
					    <<", ">>)])
		      end,
		      []
	      end;
	 (_) ->
	      erlang:error(bad_option)
      end, Opts);
validate_opts(_, _, _, _, _) ->
    erlang:error(bad_option).

-spec validate_opt(module(), atom(), any(), [atom()],
		   [{atom(), check_fun(), any()}]) -> [{atom(), any()}].
validate_opt(Module, Opt, Val, Required, VFun) ->
    try VFun(Val) of
	NewVal -> [{Opt, NewVal}]
    catch {invalid_syntax, Error} ->
	    ?ERROR_MSG("Ignoring invalid value '~p' for "
		       "option '~s' of module '~s': ~s",
		       [Val, Opt, Module, Error]),
	    fail_if_option_is_required(Opt, Required),
	    [];
	  _:_ ->
	    ?ERROR_MSG("Ignoring invalid value '~p' for "
		       "option '~s' of module '~s'",
		       [Val, Opt, Module]),
	    fail_if_option_is_required(Opt, Required),
	    []
    end.

-spec fail_if_option_is_required(atom(), [atom()]) -> ok | no_return().
fail_if_option_is_required(Opt, Required) ->
    case lists:member(Opt, Required) of
	true -> erlang:error({missing_required_option, Opt});
	false -> ok
    end.

-spec list_known_opts(binary(), module()) -> [atom() | {atom(), atom()}].
list_known_opts(Host, Module) ->
    try Module:mod_options(Host) of
	DefaultOpts ->
	    lists:flatmap(
	      fun({Opt, [{A, _}|_] = Vals}) when is_atom(A) ->
		      [{Opt, Val} || {Val, _} <- Vals];
		 ({Opt, _}) -> [Opt];
		 (Opt) -> [Opt]
	      end, DefaultOpts)
    catch _:undef ->
	    Module:mod_opt_type('')
    end.

-spec merge_opts(opts(), opts(), module()) -> opts().
merge_opts(Opts, DefaultOpts, Module) ->
    Result =
	lists:foldr(
	  fun({Opt, Default}, Acc) ->
		  case lists:keyfind(Opt, 1, Opts) of
		      {_, Val} ->
			  case Default of
			      [{A, _}|_] when is_atom(A) andalso is_list(Val) ->
				  case is_opt_list(Val) of
				      true ->
					  [{Opt, merge_opts(Val, Default, Module)}|Acc];
				      false ->
					  ?ERROR_MSG(
					     "Ignoring invalid value '~p' for "
					     "option '~s' of module '~s'",
					     [Val, Opt, Module]),
					  [{Opt, Default}|Acc]
				  end;
			      Val ->
				  [{Opt, Default}|Acc];
			      _ ->
				  [{Opt, Val}, {Opt, Default}|Acc]
			  end;
		      _ ->
			  [{Opt, Default}|Acc]
		  end;
	     (Opt, Acc) ->
		  case lists:keyfind(Opt, 1, Opts) of
		      {_, Val} ->
			  [{Opt, Val}|Acc];
		      false ->
			  erlang:error({missing_required_option, Opt})
		  end
	  end, [], DefaultOpts),
    lists:foldl(
      fun({Opt, Val}, Acc) ->
	      case lists:keymember(Opt, 1, Result) of
		  true -> Acc;
		  false -> [{Opt, Val}|Acc]
	      end
      end, Result, Opts).

remove_duplicated_opts([{Opt, Val}, {Opt, _Default}|Opts]) ->
    [{Opt, Val}|remove_duplicated_opts(Opts)];
remove_duplicated_opts([{Opt, [{SubOpt, _}|_] = SubOpts}|Opts])
  when is_atom(SubOpt) ->
    [{Opt, remove_duplicated_opts(SubOpts)}|remove_duplicated_opts(Opts)];
remove_duplicated_opts([OptVal|Opts]) ->
    [OptVal|remove_duplicated_opts(Opts)];
remove_duplicated_opts([]) ->
    [].

-spec get_submodules(binary(), module(), opts()) -> [module()].
get_submodules(Host, Module, Opts) ->
    try Module:mod_options(Host) of
	DefaultOpts ->
	    Mod1 = case lists:keyfind(db_type, 1, DefaultOpts) of
		       {_, T1} ->
			   DBType = proplists:get_value(db_type, Opts, T1),
			   [db_mod(DBType, Module)];
		       false ->
			   []
		   end,
	    Mod2 = case lists:keyfind(ram_db_type, 1, DefaultOpts) of
		       {_, T2} ->
			   RamDBType = proplists:get_value(ram_db_type, Opts, T2),
			   [ram_db_mod(RamDBType, Module)];
		       false ->
			   []
		   end,
	    Mod1 ++ Mod2
    catch _:undef ->
	    []
    end.

-spec db_type(binary() | global, module()) -> db_type();
	     (opts(), module()) -> db_type().

db_type(Opts, Module) when is_list(Opts) ->
    db_type(global, Opts, Module);
db_type(Host, Module) when is_atom(Module) ->
    case get_module_opt(Host, Module, db_type) of
	undefined ->
	    ejabberd_config:default_db(Host, Module);
	Type ->
	    Type
    end.

-spec db_type(binary() | global, opts(), module()) -> db_type().

db_type(Host, Opts, Module) ->
    case get_opt(db_type, Opts) of
	undefined ->
	    ejabberd_config:default_db(Host, Module);
	Type ->
	    Type
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
    case get_module_opt(Host, Module, ram_db_type) of
	undefined ->
	    ejabberd_config:default_ram_db(Host, Module);
	Type ->
	    Type
    end.

-spec ram_db_type(binary() | global, opts(), module()) -> db_type().
ram_db_type(Host, Opts, Module) ->
    case get_opt(ram_db_type, Opts) of
	undefined ->
	    ejabberd_config:default_ram_db(Host, Module);
	Type ->
	    Type
    end.

-spec ram_db_mod(binary() | global | db_type(), module()) -> module().
ram_db_mod(Type, Module) when is_atom(Type), Type /= global ->
    list_to_atom(atom_to_list(Module) ++ "_" ++ atom_to_list(Type));
ram_db_mod(Host, Module) when is_binary(Host) orelse Host == global ->
    ram_db_mod(ram_db_type(Host, Module), Module).

-spec ram_db_mod(binary() | global, opts(), module()) -> module().
ram_db_mod(Host, Opts, Module) when is_list(Opts) ->
    ram_db_mod(ram_db_type(Host, Opts, Module), Module).

is_db_configured(Type, Host) ->
    lists:any(
      fun(#ejabberd_module{module_host = {_, H}, opts = Opts})
	    when H == Host orelse Host == global ->
	      case lists:keyfind(db_type, 1, Opts) of
		  {_, Type} -> true;
		  _ ->
		      case lists:keyfind(ram_db_type, 1, Opts) of
			  {_, Type} -> true;
			  _ -> false
		      end
	      end;
	 (_) ->
	      false
      end, ets:tab2list(ejabberd_modules)).

-spec loaded_modules(binary()) -> [atom()].

loaded_modules(Host) ->
    Mods = ets:select(
	     ejabberd_modules,
	     ets:fun2ms(
	       fun(#ejabberd_module{module_host = {Mod, H},
				    order = Order}) when H == Host ->
		       {Mod, Order}
	       end)),
    [Mod || {Mod, _} <- lists:keysort(2, Mods)].

-spec loaded_modules_with_opts(binary()) -> [{atom(), opts()}].

loaded_modules_with_opts(Host) ->
    Mods = ets:select(
	     ejabberd_modules,
	     ets:fun2ms(
	       fun(#ejabberd_module{module_host = {Mod, H}, opts = Opts,
				    order = Order}) when H == Host ->
		       {Mod, Opts, Order}
	       end)),
    [{Mod, Opts} || {Mod, Opts, _} <- lists:keysort(3, Mods)].

-spec get_hosts(opts(), binary()) -> [binary()].

get_hosts(Opts, Prefix) ->
    case get_opt(hosts, Opts) of
        undefined ->
            case get_opt(host, Opts) of
                undefined ->
                    [<<Prefix/binary, Host/binary>> || Host <- ?MYHOSTS];
                Host ->
                    [Host]
            end;
        Hosts ->
            Hosts
    end.

-spec get_module_proc(binary() | global, atom()) -> atom().
get_module_proc(global, Base) ->
    get_module_proc(<<"global">>, Base);
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

-spec is_equal_opt(atom(), opts(), opts()) ->
			  true | {false, any(), any()}.
is_equal_opt(Opt, NewOpts, OldOpts) ->
    NewVal = get_opt(Opt, NewOpts),
    OldVal = get_opt(Opt, OldOpts),
    if NewVal /= OldVal ->
	    {false, NewVal, OldVal};
       true ->
	    true
    end.

-spec is_opt_list(term()) -> boolean().
is_opt_list([]) ->
    true;
is_opt_list(L) when is_list(L) ->
    lists:all(
      fun({Opt, _Val}) -> is_atom(Opt);
	 (_) -> false
      end, L);
is_opt_list(_) ->
    false.

-spec opt_type(modules) -> fun(([{atom(), list()}]) -> [{atom(), list()}]);
	      (atom()) -> [atom()].
opt_type(modules) ->
    fun(Mods) ->
	    lists:map(
	      fun({M, A}) when is_atom(M) ->
		      true = is_opt_list(A),
		      {M, A}
	      end, Mods)
    end;
opt_type(_) -> [modules].
