%%%----------------------------------------------------------------------
%%% File    : ejabberd_config.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Load config file
%%% Created : 14 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
-author('alexey@process-one.net').

-export([start/0, load_file/1, reload_file/0, read_file/1,
	 add_global_option/2, add_local_option/2,
	 get_global_option/2, get_local_option/2,
         get_global_option/3, get_local_option/3,
         get_option/2, get_option/3, add_option/2,
         get_vh_by_auth_method/1, is_file_readable/1,
         get_version/0, get_myhosts/0, get_mylang/0,
         prepare_opt_val/4, convert_table_to_binary/5,
         transform_options/1, collect_options/1,
         convert_to_yaml/1, convert_to_yaml/2,
         env_binary_to_list/2, opt_type/1, may_hide_data/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_config.hrl").
-include_lib("kernel/include/file.hrl").

-callback opt_type(atom()) -> function() | [atom()].

%% @type macro() = {macro_key(), macro_value()}

%% @type macro_key() = atom().
%% The atom must have all characters in uppercase.

%% @type macro_value() = term().


start() ->
    case catch mnesia:table_info(local_config, storage_type) of
        disc_copies ->
            mnesia:delete_table(local_config);
        _ ->
            ok
    end,
    mnesia:create_table(local_config,
			[{ram_copies, [node()]},
			 {local_content, true},
			 {attributes, record_info(fields, local_config)}]),
    mnesia:add_table_copy(local_config, node(), ram_copies),
    Config = get_ejabberd_config_path(),
    State0 = read_file(Config),
    State = validate_opts(State0),
    %% This start time is used by mod_last:
    UnixTime = p1_time_compat:system_time(seconds),
    SharedKey = case erlang:get_cookie() of
                    nocookie ->
                        p1_sha:sha(randoms:get_string());
                    Cookie ->
                        p1_sha:sha(jlib:atom_to_binary(Cookie))
                end,
    State1 = set_option({node_start, global}, UnixTime, State),
    State2 = set_option({shared_key, global}, SharedKey, State1),
    set_opts(State2).

%% @doc Get the filename of the ejabberd configuration file.
%% The filename can be specified with: erl -config "/path/to/ejabberd.yml".
%% It can also be specified with the environtment variable EJABBERD_CONFIG_PATH.
%% If not specified, the default value 'ejabberd.yml' is assumed.
%% @spec () -> string()
get_ejabberd_config_path() ->
    case get_env_config() of
	{ok, Path} -> Path;
	undefined ->
	    case os:getenv("EJABBERD_CONFIG_PATH") of
		false ->
		    ?CONFIG_PATH;
		Path ->
		    Path
	    end
    end.

-spec get_env_config() -> {ok, string()} | undefined.
get_env_config() ->
    %% First case: the filename can be specified with: erl -config "/path/to/ejabberd.yml".
    case application:get_env(config) of
	R = {ok, _Path} -> R;
	undefined ->
            %% Second case for embbeding ejabberd in another app, for example for Elixir:
            %% config :ejabberd,
            %%   file: "config/ejabberd.yml"
            application:get_env(ejabberd, file)
    end.

%% @doc Read the ejabberd configuration file.
%% It also includes additional configuration files and replaces macros.
%% This function will crash if finds some error in the configuration file.
%% @spec (File::string()) -> #state{}.
read_file(File) ->
    read_file(File, [{replace_macros, true},
                     {include_files, true},
                     {include_modules_configs, true}]).

read_file(File, Opts) ->
    Terms1 = get_plain_terms_file(File, Opts),
    Terms_macros = case proplists:get_bool(replace_macros, Opts) of
                       true -> replace_macros(Terms1);
                       false -> Terms1
                   end,
    Terms = transform_terms(Terms_macros),
    State = lists:foldl(fun search_hosts/2, #state{}, Terms),
    {Head, Tail} = lists:partition(
                     fun({host_config, _}) -> false;
                        ({append_host_config, _}) -> false;
                        (_) -> true
                     end, Terms),
    State1 = lists:foldl(fun process_term/2, State, Head ++ Tail),
    State1#state{opts = compact(State1#state.opts)}.

-spec load_file(string()) -> ok.

load_file(File) ->
    State = read_file(File),
    set_opts(State).

-spec reload_file() -> ok.

reload_file() ->
    Config = get_ejabberd_config_path(),
    load_file(Config).

-spec convert_to_yaml(file:filename()) -> ok | {error, any()}.

convert_to_yaml(File) ->
    convert_to_yaml(File, stdout).

-spec convert_to_yaml(file:filename(),
                      stdout | file:filename()) -> ok | {error, any()}.

convert_to_yaml(File, Output) ->
    State = read_file(File, [{include_files, false}]),
    Opts = [{K, V} || #local_config{key = K, value = V} <- State#state.opts],
    {GOpts, HOpts} = split_by_hosts(Opts),
    NewOpts = GOpts ++ lists:map(
                         fun({Host, Opts1}) ->
                                 {host_config, [{Host, Opts1}]}
                         end, HOpts),
    Data = p1_yaml:encode(lists:reverse(NewOpts)),
    case Output of
        stdout ->
            io:format("~s~n", [Data]);
        FileName ->
            file:write_file(FileName, Data)
    end.

%% Some Erlang apps expects env parameters to be list and not binary.
%% For example, Mnesia is not able to start if mnesia dir is passed as a binary.
%% However, binary is most common on Elixir, so it is easy to make a setup mistake.
-spec env_binary_to_list(atom(), atom()) -> {ok, any()}|undefined.
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

%% @doc Read an ejabberd configuration file and return the terms.
%% Input is an absolute or relative path to an ejabberd config file.
%% Returns a list of plain terms,
%% in which the options 'include_config_file' were parsed
%% and the terms in those files were included.
%% @spec(iolist()) -> [term()]
get_plain_terms_file(File) ->
    get_plain_terms_file(File, [{include_files, true}]).

get_plain_terms_file(File, Opts) when is_binary(File) ->
    get_plain_terms_file(binary_to_list(File), Opts);
get_plain_terms_file(File1, Opts) ->
    File = get_absolute_path(File1),
    case consult(File) of
	{ok, Terms} ->
            BinTerms1 = strings_to_binary(Terms),
            ModInc = case proplists:get_bool(include_modules_configs, Opts) of
                         true ->
                            Files = [{filename:rootname(filename:basename(F)), F}
                                     || F <- filelib:wildcard(ext_mod:config_dir() ++ "/*.{yml,yaml}")
                                          ++ filelib:wildcard(ext_mod:modules_dir() ++ "/*/conf/*.{yml,yaml}")],
                            [proplists:get_value(F,Files) || F <- proplists:get_keys(Files)];
                         _ ->
                            []
                     end,
            BinTerms = BinTerms1 ++ [{include_config_file, list_to_binary(V)} || V <- ModInc],
            case proplists:get_bool(include_files, Opts) of
                true ->
                    include_config_files(BinTerms);
                false ->
                    BinTerms
            end;
	{error, Reason} ->
	    ?ERROR_MSG(Reason, []),
	    exit_or_halt(Reason)
    end.

consult(File) ->
    case filename:extension(File) of
        Ex when (Ex == ".yml") or (Ex == ".yaml") ->
            case p1_yaml:decode_from_file(File, [plain_as_atom]) of
                {ok, []} ->
                    {ok, []};
                {ok, [Document|_]} ->
                    {ok, parserl(Document)};
                {error, Err} ->
                    Msg1 = "Cannot load " ++ File ++ ": ",
                    Msg2 = p1_yaml:format_error(Err),
                    {error, Msg1 ++ Msg2}
            end;
        _ ->
            case file:consult(File) of
                {ok, Terms} ->
                    {ok, Terms};
                {error, {LineNumber, erl_parse, _ParseMessage} = Reason} ->
                    {error, describe_config_problem(File, Reason, LineNumber)};
                {error, Reason} ->
                    {error, describe_config_problem(File, Reason)}
            end
    end.

parserl(<<"> ", Term/binary>>) ->
    {ok, A2, _} = erl_scan:string(binary_to_list(Term)),
    {ok, A3} = erl_parse:parse_term(A2),
    A3;
parserl({A, B}) ->
    {parserl(A), parserl(B)};
parserl([El|Tail]) ->
    [parserl(El) | parserl(Tail)];
parserl(Other) ->
    Other.

%% @doc Convert configuration filename to absolute path.
%% Input is an absolute or relative path to an ejabberd configuration file.
%% And returns an absolute path to the configuration file.
%% @spec (string()) -> string()
get_absolute_path(File) ->
    case filename:pathtype(File) of
	absolute ->
	    File;
	relative ->
	    {ok, Dir} = file:get_cwd(),
	    filename:absname_join(Dir, File)
    end.


search_hosts(Term, State) ->
    case Term of
	{host, Host} ->
	    if
		State#state.hosts == [] ->
		    add_hosts_to_option([Host], State);
		true ->
		    ?ERROR_MSG("Can't load config file: "
			       "too many hosts definitions", []),
		    exit("too many hosts definitions")
	    end;
	{hosts, Hosts} ->
	    if
		State#state.hosts == [] ->
		    add_hosts_to_option(Hosts, State);
		true ->
		    ?ERROR_MSG("Can't load config file: "
			       "too many hosts definitions", []),
		    exit("too many hosts definitions")
	    end;
	_ ->
	    State
    end.

add_hosts_to_option(Hosts, State) ->
    PrepHosts = normalize_hosts(Hosts),
    set_option({hosts, global}, PrepHosts, State#state{hosts = PrepHosts}).

normalize_hosts(Hosts) ->
    normalize_hosts(Hosts,[]).
normalize_hosts([], PrepHosts) ->
    lists:reverse(PrepHosts);
normalize_hosts([Host|Hosts], PrepHosts) ->
    case jid:nodeprep(iolist_to_binary(Host)) of
	error ->
	    ?ERROR_MSG("Can't load config file: "
		       "invalid host name [~p]", [Host]),
	    exit("invalid hostname");
	PrepHost ->
	    normalize_hosts(Hosts, [PrepHost|PrepHosts])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Errors reading the config file

describe_config_problem(Filename, Reason) ->
    Text1 = lists:flatten("Problem loading ejabberd config file " ++ Filename),
    Text2 = lists:flatten(" : " ++ file:format_error(Reason)),
    ExitText = Text1 ++ Text2,
    ExitText.

describe_config_problem(Filename, Reason, LineNumber) ->
    Text1 = lists:flatten("Problem loading ejabberd config file " ++ Filename),
    Text2 = lists:flatten(" approximately in the line "
			  ++ file:format_error(Reason)),
    ExitText = Text1 ++ Text2,
    Lines = get_config_lines(Filename, LineNumber, 10, 3),
    ?ERROR_MSG("The following lines from your configuration file might be"
	       " relevant to the error: ~n~s", [Lines]),
    ExitText.

get_config_lines(Filename, TargetNumber, PreContext, PostContext) ->
    {ok, Fd} = file:open(Filename, [read]),
    LNumbers = lists:seq(TargetNumber-PreContext, TargetNumber+PostContext),
    NextL = io:get_line(Fd, no_prompt),
    R = get_config_lines2(Fd, NextL, 1, LNumbers, []),
    file:close(Fd),
    R.

get_config_lines2(_Fd, eof, _CurrLine, _LNumbers, R) ->
    lists:reverse(R);
get_config_lines2(_Fd, _NewLine, _CurrLine, [], R) ->
    lists:reverse(R);
get_config_lines2(Fd, Data, CurrLine, [NextWanted | LNumbers], R) when is_list(Data) ->
    NextL = io:get_line(Fd, no_prompt),
    if
	CurrLine >= NextWanted ->
	    Line2 = [integer_to_list(CurrLine), ": " | Data],
	    get_config_lines2(Fd, NextL, CurrLine+1, LNumbers, [Line2 | R]);
	true ->
	    get_config_lines2(Fd, NextL, CurrLine+1, [NextWanted | LNumbers], R)
    end.

%% If ejabberd isn't yet running in this node, then halt the node
exit_or_halt(ExitText) ->
    case [Vsn || {ejabberd, _Desc, Vsn} <- application:which_applications()] of
	[] ->
	    timer:sleep(1000),
	    halt(string:substr(ExitText, 1, 199));
	[_] ->
	    exit(ExitText)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Support for 'include_config_file'

get_config_option_key(Name, Val) ->
    if Name == listen ->
            case Val of
                {{Port, IP, Trans}, _Mod, _Opts} ->
                    {Port, IP, Trans};
                {{Port, Trans}, _Mod, _Opts} when Trans == tcp; Trans == udp ->
                    {Port, {0,0,0,0}, Trans};
                {{Port, IP}, _Mod, _Opts} ->
                    {Port, IP, tcp};
                {Port, _Mod, _Opts} ->
                    {Port, {0,0,0,0}, tcp};
                V when is_list(V) ->
                    lists:foldl(
                      fun({port, Port}, {_, IP, T}) ->
                              {Port, IP, T};
                         ({ip, IP}, {Port, _, T}) ->
                              {Port, IP, T};
                         ({transport, T}, {Port, IP, _}) ->
                              {Port, IP, T};
                         (_, Res) ->
                              Res
                      end, {5222, {0,0,0,0}, tcp}, Val)
            end;
       is_tuple(Val) ->
            element(1, Val);
       true ->
            Val
    end.

maps_to_lists(IMap) ->
    maps:fold(fun(Name, Map, Res) when Name == host_config orelse Name == append_host_config ->
                      [{Name, [{Host, maps_to_lists(SMap)} || {Host,SMap} <- maps:values(Map)]} | Res];
                 (Name, Map, Res) when is_map(Map) ->
                      [{Name, maps:values(Map)} | Res];
                 (Name, Val, Res) ->
                      [{Name, Val} | Res]
              end, [], IMap).

merge_configs(Terms, ResMap) ->
    lists:foldl(fun({Name, Val}, Map) when is_list(Val) ->
                        Old = maps:get(Name, Map, #{}),
                        New = lists:foldl(fun(SVal, OMap) ->
                                                  NVal = if Name == host_config orelse Name == append_host_config ->
                                                                 {Host, Opts} = SVal,
                                                                 {_, SubMap} = maps:get(Host, OMap, {Host, #{}}),
                                                                 {Host, merge_configs(Opts, SubMap)};
                                                            true ->
                                                                 SVal
                                                         end,
                                                  maps:put(get_config_option_key(Name, SVal), NVal, OMap)
                                          end, Old, Val),
                        maps:put(Name, New, Map);
                   ({Name, Val}, Map) ->
                        maps:put(Name, Val, Map)
                end, ResMap, Terms).


%% @doc Include additional configuration files in the list of terms.
%% @spec ([term()]) -> [term()]
include_config_files(Terms) ->
    {FileOpts, Terms1} =
        lists:mapfoldl(
          fun({include_config_file, _} = T, Ts) ->
                  {[transform_include_option(T)], Ts};
             ({include_config_file, _, _} = T, Ts) ->
                  {[transform_include_option(T)], Ts};
             (T, Ts) ->
                  {[], [T|Ts]}
          end, [], Terms),
    Terms2 = lists:flatmap(
               fun({File, Opts}) ->
                       include_config_file(File, Opts)
               end, lists:flatten(FileOpts)),

    M1 = merge_configs(Terms1, #{}),
    M2 = merge_configs(Terms2, M1),
    maps_to_lists(M2).

transform_include_option({include_config_file, File}) when is_list(File) ->
    case is_string(File) of
        true -> {File, []};
        false -> File
    end;
transform_include_option({include_config_file, Filename}) ->
    {Filename, []};
transform_include_option({include_config_file, Filename, Options}) ->
    {Filename, Options}.

include_config_file(Filename, Options) ->
    Included_terms = get_plain_terms_file(Filename),
    Disallow = proplists:get_value(disallow, Options, []),
    Included_terms2 = delete_disallowed(Disallow, Included_terms),
    Allow_only = proplists:get_value(allow_only, Options, all),
    keep_only_allowed(Allow_only, Included_terms2).

%% @doc Filter from the list of terms the disallowed.
%% Returns a sublist of Terms without the ones which first element is
%% included in Disallowed.
%% @spec (Disallowed::[atom()], Terms::[term()]) -> [term()]
delete_disallowed(Disallowed, Terms) ->
    lists:foldl(
      fun(Dis, Ldis) ->
	      delete_disallowed2(Dis, Ldis)
      end,
      Terms,
      Disallowed).

delete_disallowed2(Disallowed, [H|T]) ->
    case element(1, H) of
	Disallowed ->
	    ?WARNING_MSG("The option '~p' is disallowed, "
			 "and will not be accepted", [Disallowed]),
	    delete_disallowed2(Disallowed, T);
	_ ->
	    [H|delete_disallowed2(Disallowed, T)]
    end;
delete_disallowed2(_, []) ->
    [].

%% @doc Keep from the list only the allowed terms.
%% Returns a sublist of Terms with only the ones which first element is
%% included in Allowed.
%% @spec (Allowed::[atom()], Terms::[term()]) -> [term()]
keep_only_allowed(all, Terms) ->
    Terms;
keep_only_allowed(Allowed, Terms) ->
    {As, NAs} = lists:partition(
		  fun(Term) ->
			  lists:member(element(1, Term), Allowed)
		  end,
		  Terms),
    [?WARNING_MSG("This option is not allowed, "
		  "and will not be accepted:~n~p", [NA])
     || NA <- NAs],
    As.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Support for Macro

%% @doc Replace the macros with their defined values.
%% @spec (Terms::[term()]) -> [term()]
replace_macros(Terms) ->
    {TermsOthers, Macros} = split_terms_macros(Terms),
    replace(TermsOthers, Macros).

%% @doc Split Terms into normal terms and macro definitions.
%% @spec (Terms) -> {Terms, Macros}
%%       Terms = [term()]
%%       Macros = [macro()]
split_terms_macros(Terms) ->
    lists:foldl(
      fun(Term, {TOs, Ms}) ->
	      case Term of
		  {define_macro, Key, Value} ->
		      case is_correct_macro({Key, Value}) of
			  true ->
			      {TOs, Ms++[{Key, Value}]};
			  false ->
			      exit({macro_not_properly_defined, Term})
		      end;
                  {define_macro, KeyVals} ->
                      case lists:all(fun is_correct_macro/1, KeyVals) of
                          true ->
                              {TOs, Ms ++ KeyVals};
                          false ->
                              exit({macros_not_properly_defined, Term})
                      end;
		  Term ->
		      {TOs ++ [Term], Ms}
	      end
      end,
      {[], []},
      Terms).

is_correct_macro({Key, _Val}) ->
    is_atom(Key) and is_all_uppercase(Key);
is_correct_macro(_) ->
    false.

%% @doc Recursively replace in Terms macro usages with the defined value.
%% @spec (Terms, Macros) -> Terms
%%       Terms = [term()]
%%       Macros = [macro()]
replace([], _) ->
    [];
replace([Term|Terms], Macros) ->
    [replace_term(Term, Macros) | replace(Terms, Macros)];
replace(Term, Macros) ->
    replace_term(Term, Macros).

replace_term(Key, Macros) when is_atom(Key) ->
    case is_all_uppercase(Key) of
	true ->
	    case proplists:get_value(Key, Macros) of
		undefined -> exit({undefined_macro, Key});
		Value -> Value
	    end;
	false ->
	    Key
    end;
replace_term({use_macro, Key, Value}, Macros) ->
    proplists:get_value(Key, Macros, Value);
replace_term(Term, Macros) when is_list(Term) ->
    replace(Term, Macros);
replace_term(Term, Macros) when is_tuple(Term) ->
    List = tuple_to_list(Term),
    List2 = replace(List, Macros),
    list_to_tuple(List2);
replace_term(Term, _) ->
    Term.

is_all_uppercase(Atom) ->
    String = erlang:atom_to_list(Atom),
    lists:all(fun(C) when C >= $a, C =< $z -> false;
		 (_) -> true
	      end, String).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Process terms

process_term(Term, State) ->
    case Term of
	{host_config, HostTerms} ->
            lists:foldl(
              fun({Host, Terms}, AccState) ->
                      lists:foldl(fun(T, S) ->
                                          process_host_term(T, Host, S, set)
                                  end, AccState, Terms)
              end, State, HostTerms);
        {append_host_config, HostTerms} ->
            lists:foldl(
              fun({Host, Terms}, AccState) ->
                      lists:foldl(fun(T, S) ->
                                          process_host_term(T, Host, S, append)
                                  end, AccState, Terms)
              end, State, HostTerms);
	_ ->
            process_host_term(Term, global, State, set)
    end.

process_host_term(Term, Host, State, Action) ->
    case Term of
        {modules, Modules} when Action == set ->
            set_option({modules, Host}, replace_modules(Modules), State);
        {modules, Modules} when Action == append ->
            append_option({modules, Host}, replace_modules(Modules), State);
        {host, _} ->
            State;
        {hosts, _} ->
            State;
	{Opt, Val} when Action == set ->
	    set_option({Opt, Host}, Val, State);
        {Opt, Val} when Action == append ->
            append_option({Opt, Host}, Val, State);
        Opt ->
            ?WARNING_MSG("Ignore invalid (outdated?) option ~p", [Opt]),
            State
    end.

set_option(Opt, Val, State) ->
    State#state{opts = [#local_config{key = Opt, value = Val} |
                        State#state.opts]}.

append_option({Opt, Host}, Val, State) ->
    GlobalVals = lists:flatmap(
                   fun(#local_config{key = {O, global}, value = V})
                         when O == Opt ->
                           if is_list(V) -> V;
                              true -> [V]
                           end;
                      (_) ->
                           []
                   end, State#state.opts),
    NewVal = if is_list(Val) -> Val ++ GlobalVals;
                true -> [Val|GlobalVals]
             end,
    set_option({Opt, Host}, NewVal, State).

set_opts(State) ->
    Opts = State#state.opts,
    F = fun() ->
		lists:foreach(fun(R) ->
				      mnesia:write(R)
			      end, Opts)
	end,
    case mnesia:transaction(F) of
	{atomic, _} -> ok;
	{aborted,{no_exists,Table}} ->
	    MnesiaDirectory = mnesia:system_info(directory),
	    ?ERROR_MSG("Error reading Mnesia database spool files:~n"
		       "The Mnesia database couldn't read the spool file for the table '~p'.~n"
		       "ejabberd needs read and write access in the directory:~n   ~s~n"
		       "Maybe the problem is a change in the computer hostname,~n"
		       "or a change in the Erlang node name, which is currently:~n   ~p~n"
		       "Check the ejabberd guide for details about changing the~n"
		       "computer hostname or Erlang node name.~n",
		       [Table, MnesiaDirectory, node()]),
	    exit("Error reading Mnesia database")
    end.

add_global_option(Opt, Val) ->
    add_option(Opt, Val).

add_local_option(Opt, Val) ->
    add_option(Opt, Val).

add_option(Opt, Val) when is_atom(Opt) ->
    add_option({Opt, global}, Val);
add_option(Opt, Val) ->
    mnesia:transaction(fun() ->
			       mnesia:write(#local_config{key = Opt,
							  value = Val})
		       end).

-spec prepare_opt_val(any(), any(), check_fun(), any()) -> any().

prepare_opt_val(Opt, Val, F, Default) ->
    Res = case F of
              {Mod, Fun} ->
                  catch Mod:Fun(Val);
              _ ->
                  catch F(Val)
          end,
    case Res of
        {'EXIT', _} ->
            ?INFO_MSG("Configuration problem:~n"
                      "** Option: ~s~n"
                      "** Invalid value: ~s~n"
                      "** Using as fallback: ~s",
                      [format_term(Opt),
                       format_term(Val),
                       format_term(Default)]),
            Default;
        _ ->
            Res
    end.

-type check_fun() :: fun((any()) -> any()) | {module(), atom()}.

-spec get_global_option(any(), check_fun()) -> any().

get_global_option(Opt, F) ->
    get_option(Opt, F, undefined).

-spec get_global_option(any(), check_fun(), any()) -> any().

get_global_option(Opt, F, Default) ->
    get_option(Opt, F, Default).

-spec get_local_option(any(), check_fun()) -> any().

get_local_option(Opt, F) ->
    get_option(Opt, F, undefined).

-spec get_local_option(any(), check_fun(), any()) -> any().

get_local_option(Opt, F, Default) ->
    get_option(Opt, F, Default).

-spec get_option(any(), check_fun()) -> any().

get_option(Opt, F) ->
    get_option(Opt, F, undefined).

-spec get_option(any(), check_fun(), any()) -> any().

get_option(Opt, F, Default) when is_atom(Opt) ->
    get_option({Opt, global}, F, Default);
get_option(Opt, F, Default) ->
    case Opt of
        {O, global} when is_atom(O) -> ok;
        {O, H} when is_atom(O), is_binary(H) -> ok;
        _ -> ?WARNING_MSG("Option ~p has invalid (outdated?) format. "
                          "This is likely a bug", [Opt])
    end,
    case ets:lookup(local_config, Opt) of
	[#local_config{value = Val}] ->
	    prepare_opt_val(Opt, Val, F, Default);
        _ ->
            case Opt of
                {Key, Host} when Host /= global ->
                    get_option({Key, global}, F, Default);
                _ ->
                    Default
            end
    end.

get_modules_with_options() ->
    {ok, Mods} = application:get_key(ejabberd, modules),
    ExtMods = [Name || {Name, _Details} <- ext_mod:installed()],
    lists:foldl(
      fun(Mod, D) ->
	      case catch Mod:opt_type('') of
		  Opts when is_list(Opts) ->
		      lists:foldl(
			fun(Opt, Acc) ->
				dict:append(Opt, Mod, Acc)
			end, D, Opts);
		  {'EXIT', {undef, _}} ->
		      D
	      end
      end, dict:new(), [?MODULE|ExtMods++Mods]).

validate_opts(#state{opts = Opts} = State) ->
    ModOpts = get_modules_with_options(),
    NewOpts = lists:filter(
		fun(#local_config{key = {Opt, _Host}, value = Val}) ->
			case dict:find(Opt, ModOpts) of
			    {ok, [Mod|_]} ->
				VFun = Mod:opt_type(Opt),
				case catch VFun(Val) of
				    {'EXIT', _} ->
					?ERROR_MSG("ignoring option '~s' with "
						   "invalid value: ~p",
						   [Opt, Val]),
					false;
				    _ ->
					true
				end;
			    _ ->
				?ERROR_MSG("unknown option '~s' will be likely"
					   " ignored", [Opt]),
				true
			end
		end, Opts),
    State#state{opts = NewOpts}.

-spec get_vh_by_auth_method(atom()) -> [binary()].

%% Return the list of hosts handled by a given module
get_vh_by_auth_method(AuthMethod) ->
    mnesia:dirty_select(local_config,
			[{#local_config{key = {auth_method, '$1'},
					value=AuthMethod},[],['$1']}]).

%% @spec (Path::string()) -> true | false
is_file_readable(Path) ->
    case file:read_file_info(Path) of
	{ok, FileInfo} ->
	    case {FileInfo#file_info.type, FileInfo#file_info.access} of
		{regular, read} -> true;
		{regular, read_write} -> true;
		_ -> false
	    end;
	{error, _Reason} ->
	    false
    end.

get_version() ->
    case application:get_key(ejabberd, vsn) of
        undefined -> "";
        {ok, Vsn} -> list_to_binary(Vsn)
    end.

-spec get_myhosts() -> [binary()].

get_myhosts() ->
    get_option(hosts, fun(V) -> V end).

-spec get_mylang() -> binary().

get_mylang() ->
    get_option(
      language,
      fun iolist_to_binary/1,
      <<"en">>).

replace_module(mod_announce_odbc) -> {mod_announce, odbc};
replace_module(mod_blocking_odbc) -> {mod_blocking, odbc};
replace_module(mod_caps_odbc) -> {mod_caps, odbc};
replace_module(mod_irc_odbc) -> {mod_irc, odbc};
replace_module(mod_last_odbc) -> {mod_last, odbc};
replace_module(mod_muc_odbc) -> {mod_muc, odbc};
replace_module(mod_offline_odbc) -> {mod_offline, odbc};
replace_module(mod_privacy_odbc) -> {mod_privacy, odbc};
replace_module(mod_private_odbc) -> {mod_private, odbc};
replace_module(mod_roster_odbc) -> {mod_roster, odbc};
replace_module(mod_shared_roster_odbc) -> {mod_shared_roster, odbc};
replace_module(mod_vcard_odbc) -> {mod_vcard, odbc};
replace_module(mod_vcard_xupdate_odbc) -> {mod_vcard_xupdate, odbc};
replace_module(mod_pubsub_odbc) -> {mod_pubsub, odbc};
replace_module(Module) ->
    case is_elixir_module(Module) of
        true  -> expand_elixir_module(Module);
        false -> Module
    end.

replace_modules(Modules) ->
    lists:map(
        fun({Module, Opts}) ->
                case replace_module(Module) of
                    {NewModule, DBType} ->
                        emit_deprecation_warning(Module, NewModule, DBType),
                        NewOpts = [{db_type, DBType} |
                                   lists:keydelete(db_type, 1, Opts)],
                        {NewModule, transform_module_options(Module, NewOpts)};
                    NewModule ->
                        if Module /= NewModule ->
                                emit_deprecation_warning(Module, NewModule);
                           true ->
                                ok
                        end,
                        {NewModule, transform_module_options(Module, Opts)}
                end
        end, Modules).

%% Elixir module naming
%% ====================

%% If module name start with uppercase letter, this is an Elixir module:
is_elixir_module(Module) ->
    case atom_to_list(Module) of
        [H|_] when H >= 65, H =< 90 -> true;
        _ ->false
    end.

%% We assume we know this is an elixir module
expand_elixir_module(Module) ->
    case atom_to_list(Module) of
        %% Module name already specified as an Elixir from Erlang module name
        "Elixir." ++ _ -> Module;
        %% if start with uppercase letter, this is an Elixir module: Append 'Elixir.' to module name.
        ModuleString ->
            list_to_atom("Elixir." ++ ModuleString)
    end.

strings_to_binary([]) ->
    [];
strings_to_binary(L) when is_list(L) ->
    case is_string(L) of
        true ->
            list_to_binary(L);
        false ->
            strings_to_binary1(L)
    end;
strings_to_binary({A, B, C, D}) when
	is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
    {A, B, C ,D};
strings_to_binary(T) when is_tuple(T) ->
    list_to_tuple(strings_to_binary1(tuple_to_list(T)));
strings_to_binary(X) ->
    X.

strings_to_binary1([El|L]) ->
    [strings_to_binary(El)|strings_to_binary1(L)];
strings_to_binary1([]) ->
    [];
strings_to_binary1(T) ->
    T.

is_string([C|T]) when (C >= 0) and (C =< 255) ->
    is_string(T);
is_string([]) ->
    true;
is_string(_) ->
    false.

binary_to_strings(B) when is_binary(B) ->
    binary_to_list(B);
binary_to_strings([H|T]) ->
    [binary_to_strings(H)|binary_to_strings(T)];
binary_to_strings(T) when is_tuple(T) ->
    list_to_tuple(binary_to_strings(tuple_to_list(T)));
binary_to_strings(T) ->
    T.

format_term(Bin) when is_binary(Bin) ->
    io_lib:format("\"~s\"", [Bin]);
format_term(S) when is_list(S), S /= [] ->
    case lists:all(fun(C) -> (C>=0) and (C=<255) end, S) of
        true ->
            io_lib:format("\"~s\"", [S]);
        false ->
            io_lib:format("~p", [binary_to_strings(S)])
    end;
format_term(T) ->
    io_lib:format("~p", [binary_to_strings(T)]).

transform_terms(Terms) ->
    %% We could check all ejabberd beams, but this
    %% slows down start-up procedure :(
    Mods = [mod_register,
            mod_last,
            ejabberd_s2s,
            ejabberd_listener,
            ejabberd_odbc_sup,
            shaper,
            ejabberd_s2s_out,
            acl,
            ejabberd_config],
    collect_options(transform_terms(Mods, Terms)).

transform_terms([Mod|Mods], Terms) ->
    case catch Mod:transform_options(Terms) of
        {'EXIT', _} = Err ->
            ?ERROR_MSG("Failed to transform terms by ~p: ~p", [Mod, Err]),
            transform_terms(Mods, Terms);
        NewTerms ->
            transform_terms(Mods, NewTerms)
    end;
transform_terms([], NewTerms) ->
    NewTerms.

transform_module_options(Module, Opts) ->
    Opts1 = gen_iq_handler:transform_module_options(Opts),
    try
        Module:transform_module_options(Opts1)
    catch error:undef ->
            Opts1
    end.

compact(Cfg) ->
    Opts = [{K, V} || #local_config{key = K, value = V} <- Cfg],
    {GOpts, HOpts} = split_by_hosts(Opts),
    [#local_config{key = {O, global}, value = V} || {O, V} <- GOpts] ++
        lists:flatmap(
          fun({Host, OptVal}) ->
                  case lists:member(OptVal, GOpts) of
                      true ->
                          [];
                      false ->
                          [#local_config{key = {Opt, Host}, value = Val}
                           || {Opt, Val} <- OptVal]
                  end
          end, lists:flatten(HOpts)).

split_by_hosts(Opts) ->
    Opts1 = orddict:to_list(
              lists:foldl(
                fun({{Opt, Host}, Val}, D) ->
                        orddict:append(Host, {Opt, Val}, D)
                end, orddict:new(), Opts)),
    case lists:keytake(global, 1, Opts1) of
        {value, {global, GlobalOpts}, HostOpts} ->
            {GlobalOpts, HostOpts};
        _ ->
            {[], Opts1}
    end.

collect_options(Opts) ->
    {D, InvalidOpts} =
        lists:foldl(
          fun({K, V}, {D, Os}) when is_list(V) ->
                  {orddict:append_list(K, V, D), Os};
             ({K, V}, {D, Os}) ->
                  {orddict:store(K, V, D), Os};
             (Opt, {D, Os}) ->
                  {D, [Opt|Os]}
          end, {orddict:new(), []}, Opts),
    InvalidOpts ++ orddict:to_list(D).

transform_options(Opts) ->
    Opts1 = lists:foldl(fun transform_options/2, [], Opts),
    {HOpts, Opts2} = lists:mapfoldl(
                       fun({host_config, O}, Os) ->
                               {[O], Os};
                          (O, Os) ->
                               {[], [O|Os]}
                       end, [], Opts1),
    {AHOpts, Opts3} = lists:mapfoldl(
                        fun({append_host_config, O}, Os) ->
                                {[O], Os};
                           (O, Os) ->
                                {[], [O|Os]}
                        end, [], Opts2),
    HOpts1 = case collect_options(lists:flatten(HOpts)) of
                 [] ->
                     [];
                 HOs ->
                     [{host_config,
                       [{H, transform_terms(O)} || {H, O} <- HOs]}]
             end,
    AHOpts1 = case collect_options(lists:flatten(AHOpts)) of
                  [] ->
                      [];
                  AHOs ->
                      [{append_host_config,
                        [{H, transform_terms(O)} || {H, O} <- AHOs]}]
              end,
    HOpts1 ++ AHOpts1 ++ Opts3.

transform_options({domain_certfile, Domain, CertFile}, Opts) ->
    ?WARNING_MSG("Option 'domain_certfile' now should be defined "
                 "per virtual host or globally. The old format is "
                 "still supported but it is better to fix your config", []),
    [{host_config, [{Domain, [{domain_certfile, CertFile}]}]}|Opts];
transform_options(Opt, Opts) when Opt == override_global;
                                  Opt == override_local;
                                  Opt == override_acls ->
    ?WARNING_MSG("Ignoring '~s' option which has no effect anymore", [Opt]),
    Opts;
transform_options({host_config, Host, HOpts}, Opts) ->
    {AddOpts, HOpts1} =
        lists:mapfoldl(
          fun({{add, Opt}, Val}, Os) ->
                  ?WARNING_MSG("Option 'add' is deprecated. "
                               "The option is still supported "
                               "but it is better to fix your config: "
                               "use 'append_host_config' instead.", []),
                  {[{Opt, Val}], Os};
             (O, Os) ->
                  {[], [O|Os]}
          end, [], HOpts),
    [{append_host_config, [{Host, lists:flatten(AddOpts)}]},
     {host_config, [{Host, HOpts1}]}|Opts];
transform_options({define_macro, Macro, Val}, Opts) ->
    [{define_macro, [{Macro, Val}]}|Opts];
transform_options({include_config_file, _} = Opt, Opts) ->
    [{include_config_file, [transform_include_option(Opt)]} | Opts];
transform_options({include_config_file, _, _} = Opt, Opts) ->
    [{include_config_file, [transform_include_option(Opt)]} | Opts];
transform_options(Opt, Opts) ->
    [Opt|Opts].

-spec convert_table_to_binary(atom(), [atom()], atom(),
                              fun(), fun()) -> ok.

convert_table_to_binary(Tab, Fields, Type, DetectFun, ConvertFun) ->
    case is_table_still_list(Tab, DetectFun) of
        true ->
            ?INFO_MSG("Converting '~s' table from strings to binaries.", [Tab]),
            TmpTab = list_to_atom(atom_to_list(Tab) ++ "_tmp_table"),
            catch mnesia:delete_table(TmpTab),
            case mnesia:create_table(TmpTab,
                                     [{disc_only_copies, [node()]},
                                      {type, Type},
                                      {local_content, true},
                                      {record_name, Tab},
                                      {attributes, Fields}]) of
                {atomic, ok} ->
                    mnesia:transform_table(Tab, ignore, Fields),
                    case mnesia:transaction(
                           fun() ->
                                   mnesia:write_lock_table(TmpTab),
                                   mnesia:foldl(
                                     fun(R, _) ->
                                             NewR = ConvertFun(R),
                                             mnesia:dirty_write(TmpTab, NewR)
                                     end, ok, Tab)
                           end) of
                        {atomic, ok} ->
                            mnesia:clear_table(Tab),
                            case mnesia:transaction(
                                   fun() ->
                                           mnesia:write_lock_table(Tab),
                                           mnesia:foldl(
                                             fun(R, _) ->
                                                     mnesia:dirty_write(R)
                                             end, ok, TmpTab)
                                   end) of
                                {atomic, ok} ->
                                    mnesia:delete_table(TmpTab);
                                Err ->
                                    report_and_stop(Tab, Err)
                            end;
                        Err ->
                            report_and_stop(Tab, Err)
                    end;
                Err ->
                    report_and_stop(Tab, Err)
            end;
        false ->
            ok
    end.

is_table_still_list(Tab, DetectFun) ->
    is_table_still_list(Tab, DetectFun, mnesia:dirty_first(Tab)).

is_table_still_list(_Tab, _DetectFun, '$end_of_table') ->
    false;
is_table_still_list(Tab, DetectFun, Key) ->
    Rs = mnesia:dirty_read(Tab, Key),
    Res = lists:foldl(fun(_, true) ->
                              true;
                         (_, false) ->
                              false;
                         (R, _) ->
                              case DetectFun(R) of
                                  '$next' ->
                                      '$next';
                                  El ->
                                      is_list(El)
                              end
                      end, '$next', Rs),
    case Res of
        true ->
            true;
        false ->
            false;
        '$next' ->
            is_table_still_list(Tab, DetectFun, mnesia:dirty_next(Tab, Key))
    end.

report_and_stop(Tab, Err) ->
    ErrTxt = lists:flatten(
               io_lib:format(
                 "Failed to convert '~s' table to binary: ~p",
                 [Tab, Err])),
    ?CRITICAL_MSG(ErrTxt, []),
    timer:sleep(1000),
    halt(string:substr(ErrTxt, 1, 199)).

emit_deprecation_warning(Module, NewModule, DBType) ->
    ?WARNING_MSG("Module ~s is deprecated, use ~s with 'db_type: ~s'"
                 " instead", [Module, NewModule, DBType]).

emit_deprecation_warning(Module, NewModule) ->
    case is_elixir_module(NewModule) of
        %% Do not emit deprecation warning for Elixir
        true -> ok;
        false ->
            ?WARNING_MSG("Module ~s is deprecated, use ~s instead",
                         [Module, NewModule])
    end.

opt_type(hide_sensitive_log_data) ->
    fun (H) when is_boolean(H) -> H end;
opt_type(hosts) ->
    fun(L) when is_list(L) ->
	    lists:map(
	      fun(H) ->
		      iolist_to_binary(H)
	      end, L)
    end;
opt_type(language) ->
    fun iolist_to_binary/1;
opt_type(_) ->
    [hide_sensitive_log_data, hosts, language].

-spec may_hide_data(string()) -> string();
                   (binary()) -> binary().

may_hide_data(Data) ->
    case ejabberd_config:get_option(
	hide_sensitive_log_data,
	    fun(false) -> false;
	       (true) -> true
	    end,
        false) of
	false ->
	    Data;
	true ->
	    "hidden_by_ejabberd"
    end.
