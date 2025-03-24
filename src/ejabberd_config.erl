%%%----------------------------------------------------------------------
%%% File    : ejabberd_config.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Load config file
%%% Created : 14 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
-module(ejabberd_config).

%% API
-export([get_option/1]).
-export([load/0, reload/0, format_error/1, path/0]).
-export([env_binary_to_list/2]).
-export([get_myname/0, get_uri/0, get_copyright/0]).
-export([get_shared_key/0, get_node_start/0]).
-export([fsm_limit_opts/1]).
-export([codec_options/0]).
-export([version/0]).
-export([default_db/2, default_db/3, default_ram_db/2, default_ram_db/3]).
-export([beams/1, validators/1, globals/0, may_hide_data/1]).
-export([dump/0, dump/1, convert_to_yaml/1, convert_to_yaml/2]).
-export([callback_modules/1]).
-export([set_option/2]).
-export([get_defined_keywords/1, get_predefined_keywords/1, replace_keywords/2, replace_keywords/3]).

%% Deprecated functions
-export([get_option/2]).
-export([get_version/0, get_myhosts/0]).
-export([get_mylang/0, get_lang/1]).
-deprecated([{get_option, 2},
	     {get_version, 0},
	     {get_myhosts, 0},
	     {get_mylang, 0},
	     {get_lang, 1}]).

-include("logger.hrl").
-include("ejabberd_stacktrace.hrl").

-type option() :: atom() | {atom(), global | binary()}.
-type error_reason() :: {merge_conflict, atom(), binary()} |
			{old_config, file:filename_all(), term()} |
			{write_file, file:filename_all(), term()} |
			{exception, term(), term(), term()}.
-type error_return() :: {error, econf:error_reason(), term()} |
			{error, error_reason()}.
-type host_config() :: #{{atom(), binary() | global} => term()}.

-callback opt_type(atom()) -> econf:validator().
-callback options() -> [atom() | {atom(), term()}].
-callback globals() -> [atom()].
-callback doc() -> any().

-optional_callbacks([globals/0]).

-ifndef(OTP_BELOW_28).
-dialyzer([no_opaque_union]).
-endif.

%%%===================================================================
%%% API
%%%===================================================================
-spec load() -> ok | error_return().
load() ->
    load(path()).

-spec load(file:filename_all()) -> ok | error_return().
load(Path) ->
    ConfigFile = unicode:characters_to_binary(Path),
    UnixTime = erlang:monotonic_time(second),
    ?INFO_MSG("Loading configuration from ~ts", [ConfigFile]),
    _ = ets:new(ejabberd_options,
		[named_table, public, {read_concurrency, true}]),
    case load_file(ConfigFile) of
	ok ->
	    set_shared_key(),
	    set_node_start(UnixTime),
	    ?INFO_MSG("Configuration loaded successfully", []);
	Err ->
	    Err
    end.

-spec reload() -> ok | error_return().
reload() ->
    ejabberd_systemd:reloading(),
    ConfigFile = path(),
    ?INFO_MSG("Reloading configuration from ~ts", [ConfigFile]),
    OldHosts = get_myhosts(),
    Res = case load_file(ConfigFile) of
	      ok ->
		  NewHosts = get_myhosts(),
		  AddHosts = NewHosts -- OldHosts,
		  DelHosts = OldHosts -- NewHosts,
		  lists:foreach(
		    fun(Host) ->
			    ejabberd_hooks:run(host_up, [Host])
		    end, AddHosts),
		  lists:foreach(
		    fun(Host) ->
			    ejabberd_hooks:run(host_down, [Host])
		    end, DelHosts),
		  ejabberd_hooks:run(config_reloaded, []),
		  % logger is started too early to be able to use hooks, so
		  % we need to call it separately
		  ejabberd_logger:config_reloaded(),
		  delete_host_options(DelHosts),
		  ?INFO_MSG("Configuration reloaded successfully", []);
	      Err ->
		  ?ERROR_MSG("Configuration reload aborted: ~ts",
			     [format_error(Err)]),
		  Err
	  end,
    ejabberd_systemd:ready(),
    Res.

-spec dump() -> ok | error_return().
dump() ->
    dump(stdout).

-spec dump(stdout | file:filename_all()) -> ok | error_return().
dump(Output) ->
    Y = get_option(yaml_config),
    dump(Y, Output).

-spec dump(term(), stdout | file:filename_all()) -> ok | error_return().
dump(Y, Output) ->
    Data = fast_yaml:encode(Y),
    case Output of
	stdout ->
	    io:format("~ts~n", [Data]);
	FileName ->
	    try
		ok = filelib:ensure_dir(FileName),
		ok = file:write_file(FileName, Data)
	    catch _:{badmatch, {error, Reason}} ->
		    {error, {write_file, FileName, Reason}}
	    end
    end.

-spec get_option(option(), term()) -> term().
get_option(Opt, Default) ->
    try get_option(Opt)
    catch _:badarg -> Default
    end.

-spec get_option(option()) -> term().
get_option(Opt) when is_atom(Opt) ->
    get_option({Opt, global});
get_option({O, Host} = Opt) ->
    Tab = case get_tmp_config() of
	      undefined -> ejabberd_options;
	      T -> T
	  end,
    try ets:lookup_element(Tab, Opt, 2)
    catch ?EX_RULE(error, badarg, St) when Host /= global ->
	    StackTrace = ?EX_STACK(St),
	    Val = get_option({O, global}),
	    ?DEBUG("Option '~ts' is not defined for virtual host '~ts'. "
		   "This is a bug, please report it with the following "
		   "stacktrace included:~n** ~ts",
		   [O, Host, misc:format_exception(2, error, badarg, StackTrace)]),
	    Val
    end.

-spec set_option(option(), term()) -> ok.
set_option(Opt, Val) when is_atom(Opt) ->
    set_option({Opt, global}, Val);
set_option(Opt, Val) ->
    Tab = case get_tmp_config() of
	      undefined -> ejabberd_options;
	      T -> T
	  end,
    ets:insert(Tab, {Opt, Val}),
    ok.

-spec get_version() -> binary().
get_version() ->
    get_option(version).

-spec get_myhosts() -> [binary(), ...].
get_myhosts() ->
    get_option(hosts).

-spec get_myname() -> binary().
get_myname() ->
    get_option(host).

-spec get_mylang() -> binary().
get_mylang() ->
    get_lang(global).

-spec get_lang(global | binary()) -> binary().
get_lang(Host) ->
    get_option({language, Host}).

-spec get_uri() -> binary().
get_uri() ->
    <<"https://www.process-one.net/ejabberd/">>.

-spec get_copyright() -> binary().
get_copyright() ->
    <<"Copyright (c) ProcessOne">>.

-spec get_shared_key() -> binary().
get_shared_key() ->
    get_option(shared_key).

-spec get_node_start() -> integer().
get_node_start() ->
    get_option(node_start).

-spec fsm_limit_opts([proplists:property()]) -> [{max_queue, pos_integer()}].
fsm_limit_opts(Opts) ->
    case lists:keyfind(max_fsm_queue, 1, Opts) of
	{_, I} when is_integer(I), I>0 ->
	    [{max_queue, I}];
	false ->
	    case get_option(max_fsm_queue) of
		undefined -> [];
		N -> [{max_queue, N}]
	    end
    end.

-spec codec_options() -> [xmpp:decode_option()].
codec_options() ->
    case get_option(validate_stream) of
	true -> [];
	false -> [ignore_els]
    end.

%% Do not use this function in runtime:
%% It's slow and doesn't read 'version' option from the config.
%% Use ejabberd_option:version() instead.
-spec version() -> binary().
version() ->
    case application:get_env(ejabberd, custom_vsn) of
	{ok, Vsn0} when is_list(Vsn0) ->
	    list_to_binary(Vsn0);
	{ok, Vsn1} when is_binary(Vsn1) ->
	    Vsn1;
	_ ->
	    case application:get_key(ejabberd, vsn) of
		undefined -> <<"">>;
		{ok, Vsn} -> list_to_binary(Vsn)
	    end
    end.

-spec default_db(binary() | global, module()) -> atom().
default_db(Host, Module) ->
    default_db(default_db, Host, Module, mnesia).

-spec default_db(binary() | global, module(), atom()) -> atom().
default_db(Host, Module, Default) ->
    default_db(default_db, Host, Module, Default).

-spec default_ram_db(binary() | global, module()) -> atom().
default_ram_db(Host, Module) ->
    default_db(default_ram_db, Host, Module, mnesia).

-spec default_ram_db(binary() | global, module(), atom()) -> atom().
default_ram_db(Host, Module, Default) ->
    default_db(default_ram_db, Host, Module, Default).

-spec default_db(default_db | default_ram_db, binary() | global, module(), atom()) -> atom().
default_db(Opt, Host, Mod, Default) ->
    Type = get_option({Opt, Host}),
    DBMod = list_to_atom(atom_to_list(Mod) ++ "_" ++ atom_to_list(Type)),
    case code:ensure_loaded(DBMod) of
	{module, _} -> Type;
	{error, _} ->
	    ?WARNING_MSG("Module ~ts doesn't support database '~ts' "
			 "defined in option '~ts', using "
			 "'~ts' as fallback", [Mod, Type, Opt, Default]),
	    Default
    end.

-spec beams(local | external | all) -> [module()].
beams(local) ->
    {ok, Mods} = application:get_key(ejabberd, modules),
    Mods;
beams(external) ->
    ExtMods = [Name || {Name, _Details} <- ext_mod:installed()],
    lists:foreach(
      fun(ExtMod) ->
              ExtModPath = ext_mod:module_ebin_dir(ExtMod),
              case lists:member(ExtModPath, code:get_path()) of
                  true -> ok;
                  false -> code:add_patha(ExtModPath)
              end
      end, ExtMods),
    case application:get_env(ejabberd, external_beams) of
        {ok, Path} ->
            case lists:member(Path, code:get_path()) of
                true -> ok;
                false -> code:add_patha(Path)
            end,
            Beams = filelib:wildcard(filename:join(Path, "*\.beam")),
            CustMods = [list_to_atom(filename:rootname(filename:basename(Beam)))
                        || Beam <- Beams],
            CustMods ++ ExtMods;
        _ ->
            ExtMods
    end;
beams(all) ->
    beams(local) ++ beams(external).

-spec may_hide_data(term()) -> term().
may_hide_data(Data) ->
    case get_option(hide_sensitive_log_data) of
        false -> Data;
        true -> "hidden_by_ejabberd"
    end.

%% Some Erlang apps expects env parameters to be list and not binary.
%% For example, Mnesia is not able to start if mnesia dir is passed as a binary.
%% However, binary is most common on Elixir, so it is easy to make a setup mistake.
-spec env_binary_to_list(atom(), atom()) -> {ok, any()} | undefined.
env_binary_to_list(Application, Parameter) ->
    %% Application need to be loaded to allow setting parameters
    application:load(Application),
    case application:get_env(Application, Parameter) of
        {ok, Val} when is_binary(Val) ->
            BVal = binary_to_list(Val),
            application:set_env(Application, Parameter, BVal),
            {ok, BVal};
        Other ->
            Other
    end.

%% ejabberd_options calls this function when parsing options inside host_config
-spec validators([atom()]) -> {econf:validators(), [atom()]}.
validators(Disallowed) ->
    Host = global,
    DefinedKeywords = get_defined_keywords(Host),
    validators(Disallowed, DefinedKeywords).

%% validate/1 calls this function when parsing toplevel options
-spec validators([atom()], [any()]) -> {econf:validators(), [atom()]}.
validators(Disallowed, DK) ->
    Modules = callback_modules(all),
    Validators = lists:foldl(
		   fun(M, Vs) ->
			   maps:merge(Vs, validators(M, Disallowed, DK))
		   end, #{}, Modules),
    Required = lists:flatmap(
		 fun(M) ->
			 [O || O <- M:options(), is_atom(O)]
		 end, Modules),
    {Validators, Required}.

-spec convert_to_yaml(file:filename()) -> ok | error_return().
convert_to_yaml(File) ->
    convert_to_yaml(File, stdout).

-spec convert_to_yaml(file:filename(),
                      stdout | file:filename()) -> ok | error_return().
convert_to_yaml(File, Output) ->
    case read_erlang_file(File, []) of
	{ok, Y} ->
	    dump(Y, Output);
	Err ->
	    Err
    end.

-spec format_error(error_return()) -> string().
format_error({error, Reason, Ctx}) ->
    econf:format_error(Reason, Ctx);
format_error({error, {merge_conflict, Opt, Host}}) ->
    lists:flatten(
      io_lib:format(
	"Cannot merge value of option '~ts' defined in append_host_config "
	"for virtual host ~ts: only options of type list or map are allowed "
	"in append_host_config. Hint: specify the option in host_config",
	[Opt, Host]));
format_error({error, {old_config, Path, Reason}}) ->
    lists:flatten(
      io_lib:format(
	"Failed to read configuration from '~ts': ~ts~ts",
	[Path,
	 case Reason of
	     {_, _, _} -> "at line ";
	     _ -> ""
	 end, file:format_error(Reason)]));
format_error({error, {write_file, Path, Reason}}) ->
    lists:flatten(
      io_lib:format(
	"Failed to write to '~ts': ~ts",
	[Path,
	 file:format_error(Reason)]));
format_error({error, {exception, Class, Reason, St}}) ->
    lists:flatten(
      io_lib:format(
	"Exception occurred during configuration processing. "
	"This is most likely due to faulty/incompatible validator in "
	"third-party code. If you are not running any third-party "
	"code, please report the bug with ejabberd configuration "
	"file attached and the following stacktrace included:~n** ~ts",
	[misc:format_exception(2, Class, Reason, St)])).

%% @format-begin

replace_keywords(Host, Value) ->
    Keywords = get_defined_keywords(Host) ++ get_predefined_keywords(Host),
    replace_keywords(Host, Value, Keywords).

replace_keywords(Host, List, Keywords) when is_list(List) ->
    [replace_keywords(Host, Element, Keywords) || Element <- List];
replace_keywords(Host, Atom, Keywords) when is_atom(Atom) ->
    Str = atom_to_list(Atom),
    Bin = iolist_to_binary(Str),
    case Str == string:uppercase(Str) of
        false ->
            BinaryReplaced = replace_keywords(Host, Bin, Keywords),
            binary_to_atom(BinaryReplaced, utf8);
        true ->
            case proplists:get_value(Bin, Keywords) of
                undefined ->
                    Atom;
                Replacement ->
                    Replacement
            end
    end;
replace_keywords(_Host, Binary, Keywords) when is_binary(Binary) ->
    lists:foldl(fun ({Key, Replacement}, V) when is_binary(Replacement) ->
                        misc:expand_keyword(<<"@", Key/binary, "@">>, V, Replacement);
                    ({_, _}, V) ->
                        V
                end,
                Binary,
                Keywords);
replace_keywords(Host, {Element1, Element2}, Keywords) ->
    {Element1, replace_keywords(Host, Element2, Keywords)};
replace_keywords(_Host, Value, _DK) ->
    Value.

get_defined_keywords(Host) ->
    Tab = case get_tmp_config() of
              undefined ->
                  ejabberd_options;
              T ->
                  T
          end,
    get_defined_keywords(Tab, Host).

get_defined_keywords(Tab, Host) ->
    KeysHost =
        case ets:lookup(Tab, {define_keyword, Host}) of
            [{_, List}] ->
                List;
            _ ->
                []
        end,
    KeysGlobal =
        case Host /= global andalso ets:lookup(Tab, {define_keyword, global}) of
            [{_, ListG}] ->
                ListG;
            _ ->
                []
        end,
    %% Trying to get defined keywords in host_config when starting ejabberd,
    %% the options are not yet stored in ets
    KeysTemp =
        case not is_atom(Tab) andalso KeysHost == [] andalso KeysGlobal == [] of
            true ->
                get_defined_keywords_yaml_config(ets:lookup_element(Tab, {yaml_config, global}, 2));
            false ->
                []
        end,
    lists:reverse(KeysTemp ++ KeysGlobal ++ KeysHost).

get_defined_keywords_yaml_config(Y) ->
    [{erlang:atom_to_binary(KwAtom, latin1), KwValue}
     || {KwAtom, KwValue} <- proplists:get_value(define_keyword, Y, [])].

get_predefined_keywords(Host) ->
    HostList =
        case Host of
            global ->
                [];
            _ ->
                [{<<"HOST">>, Host}]
        end,
    {ok, [[Home]]} = init:get_argument(home),
    HostList
    ++ [{<<"HOME">>, list_to_binary(Home)},
        {<<"SEMVER">>, ejabberd_option:version()},
        {<<"VERSION">>,
         misc:semver_to_xxyy(
             ejabberd_option:version())}].
%% @format-end

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec path() -> binary().
path() ->
    unicode:characters_to_binary(
      case get_env_config() of
	  {ok, Path} ->
	      Path;
	  undefined ->
	      case os:getenv("EJABBERD_CONFIG_PATH") of
		  false ->
		      "ejabberd.yml";
		  Path ->
		      Path
	      end
      end).

-spec get_env_config() -> {ok, string()} | undefined.
get_env_config() ->
    %% First case: the filename can be specified with: erl -config "/path/to/ejabberd.yml".
    case application:get_env(ejabberd, config) of
	R = {ok, _Path} -> R;
	undefined ->
            %% Second case for embbeding ejabberd in another app, for example for Elixir:
            %% config :ejabberd,
            %%   file: "config/ejabberd.yml"
            application:get_env(ejabberd, file)
    end.

-spec create_tmp_config() -> ok.
create_tmp_config() ->
    T = ets:new(options, [private]),
    put(ejabberd_options, T),
    ok.

-spec get_tmp_config() -> ets:tid() | undefined.
get_tmp_config() ->
    get(ejabberd_options).

-spec delete_tmp_config() -> ok.
delete_tmp_config() ->
    case get_tmp_config() of
	undefined ->
	    ok;
	T ->
	    erase(ejabberd_options),
	    ets:delete(T),
	    ok
    end.

-spec callback_modules(local | external | all) -> [module()].
callback_modules(local) ->
    [ejabberd_options];
callback_modules(external) ->
    lists:filter(
      fun(M) ->
	      case code:ensure_loaded(M) of
		  {module, _} ->
		      erlang:function_exported(M, options, 0)
			  andalso erlang:function_exported(M, opt_type, 1);
		  {error, _} ->
		      false
	      end
      end, beams(external));
callback_modules(all) ->
    lists_uniq(callback_modules(local) ++ callback_modules(external)).

-ifdef(OTP_BELOW_25).
lists_uniq(List) ->
    lists:usort(List).
-else.
lists_uniq(List) ->
    lists:uniq(List).
-endif.

-spec validators(module(), [atom()], [any()]) -> econf:validators().
validators(Mod, Disallowed, DK) ->
    Keywords = DK ++ get_predefined_keywords(global),
    maps:from_list(
      lists:filtermap(
	fun(O) ->
		case lists:member(O, Disallowed) of
		    true -> false;
		    false ->
			Type =
			 try Mod:opt_type(O)
			 catch _:_ ->
				 ejabberd_options:opt_type(O)
			 end,
                         TypeProcessed =
                             econf:and_then(
                                  fun(B) ->
                                      replace_keywords(global, B, Keywords)
                                  end,
                              Type),
			{true, {O, TypeProcessed}}
		end
	end, proplists:get_keys(Mod:options()))).

read_file(File) ->
    read_file(File, [replace_macros, include_files, include_modules_configs]).

read_file(File, Opts) ->
    {Opts1, Opts2} = proplists:split(Opts, [replace_macros, include_files]),
    Ret = case filename:extension(File) of
	      Ex when Ex == <<".yml">> orelse Ex == <<".yaml">> ->
		  Files = case proplists:get_bool(include_modules_configs, Opts2) of
			      true -> ext_mod:modules_configs();
			      false -> []
			  end,
		  lists:foreach(
		    fun(F) ->
			    ?INFO_MSG("Loading third-party configuration from ~ts", [F])
		    end, Files),
		  read_yaml_files([File|Files], lists:flatten(Opts1));
	      _ ->
		  read_erlang_file(File, lists:flatten(Opts1))
	  end,
    case Ret of
	{ok, Y} ->
            InstalledModules = maybe_install_contrib_modules(Y),
            ValResult = validate(Y),
            case InstalledModules of
                [] -> ok;
                _ -> spawn(fun() -> timer:sleep(5000), ?MODULE:reload() end)
            end,
            ValResult;
	Err ->
	    Err
    end.

get_additional_macros() ->
    MacroStrings = lists:foldl(fun([$E, $J, $A, $B, $B, $E, $R, $D, $_, $M, $A, $C, $R, $O, $_ | MacroString], Acc) ->
                                        [parse_macro_string(MacroString) | Acc];
                                   (_, Acc) ->
                                        Acc
                                end,
                                [],
                                os:getenv()),
    {additional_macros, MacroStrings}.

parse_macro_string(MacroString) ->
    [NameString, ValueString] = string:split(MacroString, "="),
    {ok, [ValueDecoded]} = fast_yaml:decode(ValueString, [plain_as_atom]),
    {list_to_atom(NameString), ValueDecoded}.

read_yaml_files(Files, Opts) ->
    ParseOpts = [plain_as_atom, get_additional_macros() | lists:flatten(Opts)],
    lists:foldl(
      fun(File, {ok, Y1}) ->
	      case econf:parse(File, #{'_' => econf:any()}, ParseOpts) of
		  {ok, Y2} -> {ok, Y1 ++ Y2};
		  Err -> Err
	      end;
	 (_, Err) ->
	      Err
      end, {ok, []}, Files).

read_erlang_file(File, _) ->
    case ejabberd_old_config:read_file(File) of
	{ok, Y} ->
	    econf:replace_macros(Y);
	Err ->
	    Err
    end.

-spec maybe_install_contrib_modules(term()) -> [atom()].
maybe_install_contrib_modules(Options) ->
    case {lists:keysearch(allow_contrib_modules, 1, Options),
          lists:keysearch(install_contrib_modules, 1, Options)} of
        {Allow, {value, {_, InstallContribModules}}}
          when (Allow == false) or
               (Allow == {value, {allow_contrib_modules, true}}) ->
            ext_mod:install_contrib_modules(InstallContribModules, Options);
        _ ->
            []
    end.

-spec validate(term()) -> {ok, [{atom(), term()}]} | error_return().
validate(Y1) ->
    case pre_validate(Y1) of
	{ok, Y2} ->
	    set_loglevel(proplists:get_value(loglevel, Y2, info)),
	    ejabberd_logger:set_modules_fully_logged(proplists:get_value(log_modules_fully, Y2, [])),
	    case ejabberd_config_transformer:map_reduce(Y2) of
		{ok, Y3} ->
		    Hosts = proplists:get_value(hosts, Y3),
		    Version = proplists:get_value(version, Y3, version()),
		    DK = get_defined_keywords_yaml_config(Y3),
		    create_tmp_config(),
		    set_option(hosts, Hosts),
		    set_option(host, hd(Hosts)),
		    set_option(version, Version),
		    set_option(yaml_config, Y3),
		    {Validators, Required} = validators([], DK),
		    Validator = econf:options(Validators,
					      [{required, Required},
					       unique]),
		    econf:validate(Validator, Y3);
		Err ->
		    Err
	    end;
	Err ->
	    Err
    end.

-spec pre_validate(term()) -> {ok, [{atom(), term()}]} | error_return().
pre_validate(Y1) ->
    econf:validate(
      econf:and_then(
        econf:options(
          #{hosts => ejabberd_options:opt_type(hosts),
            loglevel => ejabberd_options:opt_type(loglevel),
            version => ejabberd_options:opt_type(version),
            '_' => econf:any()},
          [{required, [hosts]}]),
        fun econf:group_dups/1), Y1).

-spec load_file(binary()) -> ok | error_return().
load_file(File) ->
    try
	case read_file(File) of
	    {ok, Terms} ->
		case set_host_config(Terms) of
		    {ok, Map} ->
			T = get_tmp_config(),
			Hosts = get_myhosts(),
			apply_defaults(T, Hosts, Map),
			case validate_modules(Hosts) of
			    {ok, ModOpts} ->
				ets:insert(T, ModOpts),
				set_option(host, hd(Hosts)),
				commit(),
				set_fqdn();
			    Err ->
				abort(Err)
			end;
		    Err ->
			abort(Err)
		end;
	    Err ->
		abort(Err)
	end
    catch ?EX_RULE(Class, Reason, St) ->
	    {error, {exception, Class, Reason, ?EX_STACK(St)}}
    end.

-spec commit() -> ok.
commit() ->
    T = get_tmp_config(),
    NewOpts = ets:tab2list(T),
    ets:insert(ejabberd_options, NewOpts),
    delete_tmp_config().

-spec abort(error_return()) -> error_return().
abort(Err) ->
    delete_tmp_config(),
    try ets:lookup_element(ejabberd_options, {loglevel, global}, 2) of
	Level -> set_loglevel(Level)
    catch _:badarg ->
	    ok
    end,
    Err.

-spec set_host_config([{atom(), term()}]) -> {ok, host_config()} | error_return().
set_host_config(Opts) ->
    Map1 = lists:foldl(
	     fun({Opt, Val}, M) when Opt /= host_config,
				     Opt /= append_host_config ->
		     maps:put({Opt, global}, Val, M);
		(_, M) ->
		     M
	     end, #{}, Opts),
    HostOpts = proplists:get_value(host_config, Opts, []),
    AppendHostOpts = proplists:get_value(append_host_config, Opts, []),
    Map2 = lists:foldl(
	     fun({Host, Opts1}, M1) ->
		     lists:foldl(
		       fun({Opt, Val}, M2) ->
			       maps:put({Opt, Host}, Val, M2)
		       end, M1, Opts1)
	     end, Map1, HostOpts),
    Map3 = lists:foldl(
	     fun(_, {error, _} = Err) ->
		     Err;
		({Host, Opts1}, M1) ->
		     lists:foldl(
		       fun(_, {error, _} = Err) ->
			       Err;
			  ({Opt, L1}, M2) when is_list(L1) ->
			       L2 = try maps:get({Opt, Host}, M2)
				    catch _:{badkey, _} ->
					    maps:get({Opt, global}, M2, [])
				    end,
			       L3 = L2 ++ L1,
			       maps:put({Opt, Host}, L3, M2);
			  ({Opt, _}, _) ->
			       {error, {merge_conflict, Opt, Host}}
		       end, M1, Opts1)
	     end, Map2, AppendHostOpts),
    case Map3 of
	{error, _} -> Map3;
	_ -> {ok, Map3}
    end.

-spec apply_defaults(ets:tid(), [binary()], host_config()) -> ok.
apply_defaults(Tab, Hosts, Map) ->
    Defaults1 = defaults(),
    apply_defaults(Tab, global, Map, Defaults1),
    {_, Defaults2} = proplists:split(Defaults1, globals()),
    lists:foreach(
      fun(Host) ->
	      set_option(host, Host),
	      apply_defaults(Tab, Host, Map, Defaults2)
      end, Hosts).

-spec apply_defaults(ets:tid(), global | binary(),
		     host_config(),
		     [atom() | {atom(), term()}]) -> ok.
apply_defaults(Tab, Host, Map, Defaults) ->
    lists:foreach(
      fun({Opt, Default}) ->
	      try maps:get({Opt, Host}, Map) of
		  Val ->
		      ets:insert(Tab, {{Opt, Host}, Val})
	      catch _:{badkey, _} when Host == global ->
		      Default1 = compute_default(Default, Host),
		      ets:insert(Tab, {{Opt, Host}, Default1});
		    _:{badkey, _} ->
		      try maps:get({Opt, global}, Map) of
			  V -> ets:insert(Tab, {{Opt, Host}, V})
		      catch _:{badkey, _} ->
			      Default1 = compute_default(Default, Host),
			      ets:insert(Tab, {{Opt, Host}, Default1})
		      end
	      end;
	 (Opt) when Host == global ->
	      Val = maps:get({Opt, Host}, Map),
	      ets:insert(Tab, {{Opt, Host}, Val});
	 (_) ->
	      ok
      end, Defaults).

-spec defaults() -> [atom() | {atom(), term()}].
defaults() ->
    lists:foldl(
      fun(Mod, Acc) ->
	      lists:foldl(
		fun({Opt, Val}, Acc1) ->
			lists:keystore(Opt, 1, Acc1, {Opt, Val});
		   (Opt, Acc1) ->
			case lists:member(Opt, Acc1) of
			    true -> Acc1;
			    false -> [Opt|Acc1]
			end
		end, Acc, Mod:options())
      end, ejabberd_options:options(), callback_modules(external)).

-spec globals() -> [atom()].
globals() ->
    lists:usort(
      lists:flatmap(
	fun(Mod) ->
		case erlang:function_exported(Mod, globals, 0) of
		    true -> Mod:globals();
		    false -> []
		end
	end, callback_modules(all))).

%% The module validator depends on virtual host, so we have to
%% validate modules in this separate function.
-spec validate_modules([binary()]) -> {ok, list()} | error_return().
validate_modules(Hosts) ->
    lists:foldl(
      fun(Host, {ok, Acc}) ->
	      set_option(host, Host),
	      ModOpts = get_option({modules, Host}),
	      case gen_mod:validate(Host, ModOpts) of
		  {ok, ModOpts1} ->
		      {ok, [{{modules, Host}, ModOpts1}|Acc]};
		  Err ->
		      Err
	      end;
	 (_, Err) ->
	      Err
      end, {ok, []}, Hosts).

-spec delete_host_options([binary()]) -> ok.
delete_host_options(Hosts) ->
    lists:foreach(
      fun(Host) ->
	      ets:match_delete(ejabberd_options, {{'_', Host}, '_'})
      end, Hosts).

-spec compute_default(fun((global | binary()) -> T) | T, global | binary()) -> T.
compute_default(F, Host) when is_function(F, 1) ->
    F(Host);
compute_default(Val, _) ->
    Val.

-spec set_fqdn() -> ok.
set_fqdn() ->
    FQDNs = get_option(fqdn),
    xmpp:set_config([{fqdn, FQDNs}]).

-spec set_shared_key() -> ok.
set_shared_key() ->
    Key = case erlang:get_cookie() of
	      nocookie ->
		  str:sha(p1_rand:get_string());
	      Cookie ->
		  str:sha(erlang:atom_to_binary(Cookie, latin1))
	  end,
    set_option(shared_key, Key).

-spec set_node_start(integer()) -> ok.
set_node_start(UnixTime) ->
    set_option(node_start, UnixTime).

-spec set_loglevel(logger:level()) -> ok.
set_loglevel(Level) ->
    ejabberd_logger:set(Level).
