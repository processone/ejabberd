%%%----------------------------------------------------------------------
%%% File    : gen_mod.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose :
%%% Created : 24 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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
-behaviour(supervisor).
-author('alexey@process-one.net').

-export([init/1, start_link/0, start_child/3, start_child/4,
	 stop_child/1, stop_child/2, stop/0, config_reloaded/0]).
-export([start_module/2, stop_module/2, stop_module_keep_config/2,
	 get_opt/2, set_opt/3, get_opt_hosts/1, is_equal_opt/3,
	 get_module_opt/3, get_module_opts/2, get_module_opt_hosts/2,
	 loaded_modules/1, loaded_modules_with_opts/1,
	 get_hosts/2, get_module_proc/2, is_loaded/2, is_loaded_elsewhere/2,
	 start_modules/0, start_modules/1, stop_modules/0, stop_modules/1,
	 db_mod/2, ram_db_mod/2]).
-export([validate/2]).

%% Deprecated functions
%% update_module/3 is used by test suite ONLY
-export([update_module/3]).
-deprecated([{update_module, 3}]).

-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("ejabberd_stacktrace.hrl").

-record(ejabberd_module,
        {module_host = {undefined, <<"">>} :: {atom(), binary()},
         opts = [] :: opts() | '_' | '$2',
         registrations = [] :: [registration()],
	 order = 0 :: integer()}).

-type opts() :: #{atom() => term()}.
-type db_type() :: atom().
-type opt_desc() :: #{desc => binary() | [binary()],
                      value => string() | binary()}.
-type opt_doc() :: {atom(), opt_desc()} | {atom(), opt_desc(), [opt_doc()]}.

-type component() :: ejabberd_sm | ejabberd_local.
-type registration() ::
        {hook, atom(), atom(), integer()} |
        {hook, atom(), module(), atom(), integer()} |
        {iq_handler, component(), binary(), atom()} |
        {iq_handler, component(), binary(), module(), atom()}.
-export_type([registration/0]).

-callback start(binary(), opts()) ->
    ok | {ok, pid()} |
    {ok, [registration()]} | {error, term()}.
-callback stop(binary()) -> any().
-callback reload(binary(), opts(), opts()) -> ok | {ok, pid()} | {error, term()}.
-callback mod_opt_type(atom()) -> econf:validator().
-callback mod_options(binary()) -> [{atom(), term()} | atom()].
-callback mod_doc() -> #{desc => binary() | [binary()],
                         opts => [opt_doc()],
                         example => [string()] | [{binary(), [string()]}]}.
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
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 60),
    ejabberd_hooks:add(host_up, ?MODULE, start_modules, 40),
    ejabberd_hooks:add(host_down, ?MODULE, stop_modules, 70),
    ets:new(ejabberd_modules,
	    [named_table, public,
	     {keypos, #ejabberd_module.module_host},
	     {read_concurrency, true}]),
    {ok, {{one_for_one, 10, 1}, []}}.

-spec stop() -> ok.
stop() ->
    ejabberd_hooks:delete(config_reloaded, ?MODULE, config_reloaded, 60),
    ejabberd_hooks:delete(host_up, ?MODULE, start_modules, 40),
    ejabberd_hooks:delete(host_down, ?MODULE, stop_modules, 70),
    stop_modules(),
    ejabberd_sup:stop_child(ejabberd_gen_mod_sup).

-spec start_child(module(), binary(), opts()) -> {ok, pid()} | {error, any()}.
start_child(Mod, Host, Opts) ->
    start_child(Mod, Host, Opts, get_module_proc(Host, Mod)).

-spec start_child(module(), binary(), opts(), atom()) -> {ok, pid()} | {error, any()}.
start_child(Mod, Host, Opts, Proc) ->
    Spec = {Proc, {?GEN_SERVER, start_link,
		   [{local, Proc}, Mod, [Host, Opts],
		    ejabberd_config:fsm_limit_opts([])]},
            transient, timer:minutes(1), worker, [Mod]},
    supervisor:start_child(ejabberd_gen_mod_sup, Spec).

-spec stop_child(module(), binary()) -> ok | {error, any()}.
stop_child(Mod, Host) ->
    stop_child(get_module_proc(Host, Mod)).

-spec stop_child(atom()) -> ok | {error, any()}.
stop_child(Proc) ->
    supervisor:terminate_child(ejabberd_gen_mod_sup, Proc),
    supervisor:delete_child(ejabberd_gen_mod_sup, Proc).

-spec start_modules() -> any().
start_modules() ->
    Hosts = ejabberd_option:hosts(),
    ?INFO_MSG("Loading modules for ~ts", [misc:format_hosts_list(Hosts)]),
    lists:foreach(fun start_modules/1, Hosts).

-spec start_modules(binary()) -> ok.
start_modules(Host) ->
    Modules = ejabberd_option:modules(Host),
    lists:foreach(
	fun({Module, Opts, Order}) ->
	    start_module(Host, Module, Opts, Order)
	end, Modules).

-spec start_module(binary(), atom()) -> ok | {ok, pid()} | {error, not_found_in_config}.
start_module(Host, Module) ->
    Modules = ejabberd_option:modules(Host),
    case lists:keyfind(Module, 1, Modules) of
	{_, Opts, Order} ->
	    start_module(Host, Module, Opts, Order);
	false ->
	    {error, not_found_in_config}
    end.

-spec start_module(binary(), atom(), opts(), integer()) -> ok | {ok, pid()}.
start_module(Host, Module, Opts, Order) ->
    ?DEBUG("Loading ~ts at ~ts", [Module, Host]),
    store_options(Host, Module, Opts, Order),
    try case Module:start(Host, Opts) of
	    ok -> ok;
	    {ok, Pid} when is_pid(Pid) -> {ok, Pid};
	    {ok, Registrations} when is_list(Registrations) ->
                store_options(Host, Module, Opts, Registrations, Order),
                add_registrations(Host, Module, Registrations),
                ok;
	    Err ->
		ets:delete(ejabberd_modules, {Module, Host}),
		erlang:error({bad_return, Module, Err})
	end
    catch ?EX_RULE(Class, Reason, Stack) ->
	    StackTrace = ?EX_STACK(Stack),
	    ets:delete(ejabberd_modules, {Module, Host}),
	    ErrorText = format_module_error(
			  Module, start, 2,
			  Opts, Class, Reason,
			  StackTrace),
	    ?CRITICAL_MSG(ErrorText, []),
	    maybe_halt_ejabberd(),
	    erlang:raise(Class, Reason, StackTrace)
    end.

-spec reload_modules(binary()) -> ok.
reload_modules(Host) ->
    NewMods = ejabberd_option:modules(Host),
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
		  {_, NewOpts, Order} ->
		      if OldOpts /= NewOpts ->
			      reload_module(Host, Mod, NewOpts, OldOpts, Order);
			 true ->
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
	    ?DEBUG("Reloading ~ts at ~ts", [Module, Host]),
	    store_options(Host, Module, NewOpts, Order),
	    try case Module:reload(Host, NewOpts, OldOpts) of
		    ok -> ok;
		    {ok, Pid} when is_pid(Pid) -> {ok, Pid};
		    Err -> erlang:error({bad_return, Module, Err})
		end
	    catch ?EX_RULE(Class, Reason, Stack) ->
		    StackTrace = ?EX_STACK(Stack),
		    ErrorText = format_module_error(
                                  Module, reload, 3,
                                  NewOpts, Class, Reason,
				  StackTrace),
                    ?CRITICAL_MSG(ErrorText, []),
		    erlang:raise(Class, Reason, StackTrace)
	    end;
	false ->
	    ?WARNING_MSG("Module ~ts doesn't support reloading "
			 "and will be restarted", [Module]),
	    stop_module(Host, Module),
	    start_module(Host, Module, NewOpts, Order)
    end.

-spec update_module(binary(), module(), opts()) -> ok | {ok, pid()}.
update_module(Host, Module, Opts) ->
    case ets:lookup(ejabberd_modules, {Module, Host}) of
	[#ejabberd_module{opts = OldOpts, order = Order}] ->
	    NewOpts = maps:merge(OldOpts, Opts),
	    reload_module(Host, Module, NewOpts, OldOpts, Order);
	[] ->
	    erlang:error({module_not_loaded, Module, Host})
    end.

-spec store_options(binary(), module(), opts(), integer()) -> true.
store_options(Host, Module, Opts, Order) ->
    case ets:lookup(ejabberd_modules, {Module, Host}) of
	[M] ->
            store_options(
              Host, Module, Opts, M#ejabberd_module.registrations, Order);
	[] ->
            store_options(Host, Module, Opts, [], Order)
    end.

-spec store_options(binary(), module(), opts(), [registration()], integer()) -> true.
store_options(Host, Module, Opts, Registrations, Order) ->
    ets:insert(ejabberd_modules,
	       #ejabberd_module{module_host = {Module, Host},
				opts = Opts,
                                registrations = Registrations,
                                order = Order}).

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
      end, ejabberd_option:hosts()).

-spec stop_modules(binary()) -> ok.
stop_modules(Host) ->
    Modules = lists:reverse(loaded_modules_with_opts(Host)),
    lists:foreach(
	fun({Module, _Args}) ->
		stop_module_keep_config(Host, Module)
	end, Modules).

-spec stop_module(binary(), atom()) -> error | ok.
stop_module(Host, Module) ->
    stop_module_keep_config(Host, Module).

-spec stop_module_keep_config(binary(), atom()) -> error | ok.
stop_module_keep_config(Host, Module) ->
    ?DEBUG("Stopping ~ts at ~ts", [Module, Host]),
    Registrations =
        case ets:lookup(ejabberd_modules, {Module, Host}) of
            [M] ->
                M#ejabberd_module.registrations;
            [] ->
                []
        end,
    del_registrations(Host, Module, Registrations),
    try Module:stop(Host) of
	_ ->
	    ets:delete(ejabberd_modules, {Module, Host}),
	    ok
    catch ?EX_RULE(Class, Reason, St) ->
            StackTrace = ?EX_STACK(St),
            ?ERROR_MSG("Failed to stop module ~ts at ~ts:~n** ~ts",
                       [Module, Host,
                        misc:format_exception(2, Class, Reason, StackTrace)]),
	    error
    end.

-spec add_registrations(binary(), module(), [registration()]) -> ok.
add_registrations(Host, Module, Registrations) ->
    lists:foreach(
      fun({hook, Hook, Function, Seq}) ->
              ejabberd_hooks:add(Hook, Host, Module, Function, Seq);
         ({hook, Hook, Module1, Function, Seq}) ->
              ejabberd_hooks:add(Hook, Host, Module1, Function, Seq);
         ({iq_handler, Component, NS, Function}) ->
              gen_iq_handler:add_iq_handler(
                Component, Host, NS, Module, Function);
         ({iq_handler, Component, NS, Module1, Function}) ->
              gen_iq_handler:add_iq_handler(
                Component, Host, NS, Module1, Function)
      end, Registrations).

-spec del_registrations(binary(), module(), [registration()]) -> ok.
del_registrations(Host, Module, Registrations) ->
    lists:foreach(
      fun({hook, Hook, Function, Seq}) ->
              ejabberd_hooks:delete(Hook, Host, Module, Function, Seq);
         ({hook, Hook, Module1, Function, Seq}) ->
              ejabberd_hooks:delete(Hook, Host, Module1, Function, Seq);
         ({iq_handler, Component, NS, _Function}) ->
              gen_iq_handler:remove_iq_handler(Component, Host, NS);
         ({iq_handler, Component, NS, _Module, _Function}) ->
              gen_iq_handler:remove_iq_handler(Component, Host, NS)
      end, Registrations).

-spec get_opt(atom(), opts()) -> any().
get_opt(Opt, Opts) ->
    maps:get(Opt, Opts).

-spec set_opt(atom(), term(), opts()) -> opts().
set_opt(Opt, Val, Opts) ->
    maps:put(Opt, Val, Opts).

-spec get_module_opt(global | binary(), atom(), atom()) -> any().
get_module_opt(global, Module, Opt) ->
    get_module_opt(ejabberd_config:get_myname(), Module, Opt);
get_module_opt(Host, Module, Opt) ->
    Opts = get_module_opts(Host, Module),
    get_opt(Opt, Opts).

-spec get_module_opt_hosts(binary(), module()) -> [binary()].
get_module_opt_hosts(Host, Module) ->
    Opts = get_module_opts(Host, Module),
    get_opt_hosts(Opts).

-spec get_opt_hosts(opts()) -> [binary()].
get_opt_hosts(Opts) ->
    case get_opt(hosts, Opts) of
	L when L == [] orelse L == undefined ->
	    [get_opt(host, Opts)];
	L ->
	    L
    end.

-spec get_module_opts(binary(), module()) -> opts().
get_module_opts(Host, Module) ->
    try ets:lookup_element(ejabberd_modules, {Module, Host}, 3)
    catch _:badarg -> erlang:error({module_not_loaded, Module, Host})
    end.

-spec db_mod(binary() | global | db_type() | opts(), module()) -> module().
db_mod(T, M) ->
    db_mod(db_type, T, M).

-spec ram_db_mod(binary() | global | db_type() | opts(), module()) -> module().
ram_db_mod(T, M) ->
    db_mod(ram_db_type, T, M).

-spec db_mod(db_type | ram_db_type,
	     binary() | global | db_type() | opts(), module()) -> module().
db_mod(Opt, Host, Module) when is_binary(Host) orelse Host == global ->
    db_mod(Opt, get_module_opt(Host, Module, Opt), Module);
db_mod(Opt, Opts, Module) when is_map(Opts) ->
    db_mod(Opt, get_opt(Opt, Opts), Module);
db_mod(_Opt, Type, Module) when is_atom(Type) ->
    list_to_existing_atom(atom_to_list(Module) ++ "_" ++ atom_to_list(Type)).

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
                    [<<Prefix/binary, Host/binary>> || Host <- ejabberd_option:hosts()];
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
    lists:foreach(fun reload_modules/1, ejabberd_option:hosts()).

-spec is_equal_opt(atom(), opts(), opts()) -> true | {false, any(), any()}.
is_equal_opt(Opt, NewOpts, OldOpts) ->
    NewVal = get_opt(Opt, NewOpts),
    OldVal = get_opt(Opt, OldOpts),
    if NewVal /= OldVal ->
	    {false, NewVal, OldVal};
       true ->
	    true
    end.

%%%===================================================================
%%% Formatters
%%%===================================================================
-spec format_module_error(atom(), start | reload, non_neg_integer(), opts(),
			  error | exit | throw, any(),
			  [tuple()]) -> iolist().
format_module_error(Module, Fun, Arity, Opts, Class, Reason, St) ->
    case {Class, Reason} of
	{error, {bad_return, Module, {error, _} = Err}} ->
	    io_lib:format("Failed to ~ts module ~ts: ~ts",
			  [Fun, Module, misc:format_val(Err)]);
	{error, {bad_return, Module, Ret}} ->
	    io_lib:format("Module ~ts returned unexpected value from ~ts/~B:~n"
                          "** Error: ~p~n"
                          "** Hint: this is either not an ejabberd module "
			  "or it implements ejabberd API incorrectly",
			  [Module, Fun, Arity, Ret]);
	_ ->
	    io_lib:format("Internal error of module ~ts has "
			  "occurred during ~ts:~n"
			  "** Options: ~p~n"
			  "** ~ts",
			  [Module, Fun, Opts,
			   misc:format_exception(2, Class, Reason, St)])
    end.

%%%===================================================================
%%% Validation
%%%===================================================================
-spec validator(binary()) -> econf:validator().
validator(Host) ->
    econf:options(
      #{modules =>
	    econf:and_then(
	      econf:map(
		econf:beam([{start, 2}, {stop, 1},
			    {mod_options, 1}, {depends, 2}]),
		econf:options(
		  #{db_type => econf:atom(),
		    ram_db_type => econf:atom(),
		    '_' => econf:any()})),
	      fun(L) ->
		      Validators = maps:from_list(
				     lists:map(
				       fun({Mod, Opts}) ->
					       {Mod, validator(Host, Mod, Opts)}
				       end, L)),
		      Validator = econf:options(Validators, [unique]),
		      Validator(L)
	      end)}).

-spec validator(binary(), module(), [{atom(), term()}]) -> econf:validator().
validator(Host, Module, Opts) ->
    {Required, {DefaultOpts1, Validators}} =
	lists:mapfoldl(
	  fun({M, DefOpts}, {DAcc, VAcc}) ->
		  lists:mapfoldl(
		    fun({Opt, Def}, {DAcc1, VAcc1}) ->
			    {[], {DAcc1#{Opt => Def},
				  VAcc1#{Opt => get_opt_type(Module, M, Opt)}}};
		       (Opt, {DAcc1, VAcc1}) ->
			    {[Opt], {DAcc1,
				     VAcc1#{Opt => get_opt_type(Module, M, Opt)}}}
		    end, {DAcc, VAcc}, DefOpts)
	  end, {#{}, #{}}, get_defaults(Host, Module, Opts)),
    econf:and_then(
      econf:options(
	Validators,
	[{required, lists:usort(lists:flatten(Required))},
	 {return, map}, unique]),
      fun(Opts1) ->
	      maps:merge(DefaultOpts1, Opts1)
      end).

-spec validate(binary(), [{module(), opts()}]) ->
		      {ok, [{module(), opts(), integer()}]} |
		      econf:error_return().
validate(Host, ModOpts) ->
    case econf:validate(validator(Host), [{modules, ModOpts}]) of
	{ok, [{modules, ModOpts1}]} ->
	    try sort_modules(Host, ModOpts1)
	    catch throw:{?MODULE, Reason} ->
		    {error, Reason, [modules]}
	    end;
	{error, _, _} = Err ->
	    Err
    end.

-spec get_defaults(binary(), module(), [{atom(), term()}]) ->
			  [{module(), [{atom(), term()} | atom()]}].
get_defaults(Host, Module, Opts) ->
    DefaultOpts = Module:mod_options(Host),
    [{Module, DefaultOpts}|
     lists:filtermap(
       fun({Opt, T1}) when Opt == db_type; Opt == ram_db_type ->
	       T2 = proplists:get_value(Opt, Opts, T1),
	       DBMod = list_to_atom(atom_to_list(Module) ++ "_" ++ atom_to_list(T2)),
	       case code:ensure_loaded(DBMod) of
		   {module, _} ->
		       case erlang:function_exported(DBMod, mod_options, 1) of
			   true ->
			       {true, {DBMod, DBMod:mod_options(Host)}};
			   false ->
			       false
		       end;
		   _ ->
		       false
	       end;
	  (_) ->
	       false
       end, DefaultOpts)].

-spec get_opt_type(module(), module(), atom()) -> econf:validator().
get_opt_type(Mod, SubMod, Opt) ->
    try SubMod:mod_opt_type(Opt)
    catch _:_ -> Mod:mod_opt_type(Opt)
    end.

-spec sort_modules(binary(), [{module(), opts()}]) -> {ok, [{module(), opts(), integer()}]}.
sort_modules(Host, ModOpts) ->
    G = digraph:new([acyclic]),
    lists:foreach(
      fun({Mod, Opts}) ->
	      digraph:add_vertex(G, Mod, Opts),
	      Deps = Mod:depends(Host, Opts),
	      lists:foreach(
		fun({DepMod, Type}) ->
			case lists:keyfind(DepMod, 1, ModOpts) of
			    false when Type == hard ->
				throw({?MODULE, {missing_module_dep, Mod, DepMod}});
			    false when Type == soft ->
				warn_soft_dep_fail(DepMod, Mod);
			    {DepMod, DepOpts} ->
				digraph:add_vertex(G, DepMod, DepOpts),
				case digraph:add_edge(G, DepMod, Mod) of
				    {error, {bad_edge, Path}} ->
					warn_cyclic_dep(Path);
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
    {ok, Result}.

-spec warn_soft_dep_fail(module(), module()) -> ok.
warn_soft_dep_fail(DepMod, Mod) ->
    ?WARNING_MSG("Module ~ts is recommended for module "
		 "~ts but is not found in the config",
		 [DepMod, Mod]).

-spec warn_cyclic_dep([module()]) -> ok.
warn_cyclic_dep(Path) ->
    ?WARNING_MSG("Cyclic dependency detected between modules ~ts. "
		 "This is either a bug, or the modules are not "
		 "supposed to work together in this configuration. "
		 "The modules will still be loaded though",
		 [misc:format_cycle(Path)]).
