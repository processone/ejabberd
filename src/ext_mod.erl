%%%----------------------------------------------------------------------
%%% File    : ext_mod.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : external modules management
%%% Created : 19 Feb 2015 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2006-2017   ProcessOne
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

-module(ext_mod).

-behaviour(ejabberd_config).
-behaviour(gen_server).
-author("Christophe Romain <christophe.romain@process-one.net>").

-export([start_link/0, update/0, check/1,
         available_command/0, available/0, available/1,
         installed_command/0, installed/0, installed/1,
         install/1, uninstall/1, upgrade/0, upgrade/1,
         add_sources/2, del_sources/1, modules_dir/0,
         config_dir/0, opt_type/1, get_commands_spec/0]).

-export([compile_erlang_file/2, compile_elixir_file/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd_commands.hrl").
-include("logger.hrl").

-define(REPOS, "https://github.com/processone/ejabberd-contrib").

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    [code:add_patha(module_ebin_dir(Module))
     || {Module, _} <- installed()],
    p1_http:start(),
    ejabberd_commands:register_commands(get_commands_spec()),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_commands:unregister_commands(get_commands_spec()).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- ejabberd commands
get_commands_spec() ->
    [#ejabberd_commands{name = modules_update_specs,
                        tags = [admin,modules],
                        desc = "",
                        longdesc = "",
                        module = ?MODULE, function = update,
                        args = [],
                        result = {res, integer}},
     #ejabberd_commands{name = modules_available,
                        tags = [admin,modules],
                        desc = "",
                        longdesc = "",
                        module = ?MODULE, function = available_command,
                        args = [],
                        result = {modules, {list,
                                  {module, {tuple,
                                   [{name, atom},
                                    {summary, string}]}}}}},
     #ejabberd_commands{name = modules_installed,
                        tags = [admin,modules],
                        desc = "",
                        longdesc = "",
                        module = ?MODULE, function = installed_command,
                        args = [],
                        result = {modules, {list,
                                  {module, {tuple,
                                   [{name, atom},
                                    {summary, string}]}}}}},
     #ejabberd_commands{name = module_install,
                        tags = [admin,modules],
                        desc = "",
                        longdesc = "",
                        module = ?MODULE, function = install,
                        args = [{module, binary}],
                        result = {res, integer}},
     #ejabberd_commands{name = module_uninstall,
                        tags = [admin,modules],
                        desc = "",
                        longdesc = "",
                        module = ?MODULE, function = uninstall,
                        args = [{module, binary}],
                        result = {res, integer}},
     #ejabberd_commands{name = module_upgrade,
                        tags = [admin,modules],
                        desc = "",
                        longdesc = "",
                        module = ?MODULE, function = upgrade,
                        args = [{module, binary}],
                        result = {res, integer}},
     #ejabberd_commands{name = module_check,
                        tags = [admin,modules],
                        desc = "",
                        longdesc = "",
                        module = ?MODULE, function = check,
                        args = [{module, binary}],
                        result = {res, integer}}
        ].
%% -- public modules functions

update() ->
    add_sources(?REPOS),
    Res = lists:foldl(fun({Package, Spec}, Acc) ->
                Path = proplists:get_value(url, Spec, ""),
                Update = add_sources(Package, Path),
                ?INFO_MSG("Update package ~s: ~p", [Package, Update]),
                case Update of
                    ok -> Acc;
                    Error -> [Error|Acc]
                end
        end, [], modules_spec(sources_dir(), "*")),
    case Res of
        [] -> ok;
        [Error|_] -> Error
    end.

available() ->
    Jungle = modules_spec(sources_dir(), "*/*"),
    Standalone = modules_spec(sources_dir(), "*"),
    lists:keysort(1,
        lists:foldl(fun({Key, Val}, Acc) ->
                lists:keystore(Key, 1, Acc, {Key, Val})
            end, Jungle, Standalone)).
available(Module) when is_atom(Module) ->
    available(misc:atom_to_binary(Module));
available(Package) when is_binary(Package) ->
    Available = [misc:atom_to_binary(K) || K<-proplists:get_keys(available())],
    lists:member(Package, Available).

available_command() ->
    [short_spec(Item) || Item <- available()].

installed() ->
    modules_spec(modules_dir(), "*").
installed(Module) when is_atom(Module) ->
    installed(misc:atom_to_binary(Module));
installed(Package) when is_binary(Package) ->
    Installed = [misc:atom_to_binary(K) || K<-proplists:get_keys(installed())],
    lists:member(Package, Installed).

installed_command() ->
    [short_spec(Item) || Item <- installed()].

install(Module) when is_atom(Module) ->
    install(misc:atom_to_binary(Module));
install(Package) when is_binary(Package) ->
    Spec = [S || {Mod, S} <- available(), misc:atom_to_binary(Mod)==Package],
    case {Spec, installed(Package), is_contrib_allowed()} of
        {_, _, false} ->
            {error, not_allowed};
        {[], _, _} ->
            {error, not_available};
        {_, true, _} ->
            {error, conflict};
        {[Attrs], _, _} ->
            Module = misc:binary_to_atom(Package),
            case compile_and_install(Module, Attrs) of
                ok ->
                    code:add_patha(module_ebin_dir(Module)),
                    ejabberd_config:reload_file(),
                    case erlang:function_exported(Module, post_install, 0) of
                        true -> Module:post_install();
                        _ -> ok
                    end;
                Error ->
                    delete_path(module_lib_dir(Module)),
                    Error
            end
    end.

uninstall(Module) when is_atom(Module) ->
    uninstall(misc:atom_to_binary(Module));
uninstall(Package) when is_binary(Package) ->
    case installed(Package) of
        true ->
            Module = misc:binary_to_atom(Package),
            case erlang:function_exported(Module, pre_uninstall, 0) of
                true -> Module:pre_uninstall();
                _ -> ok
            end,
            [catch gen_mod:stop_module(Host, Module)
             || Host <- ejabberd_config:get_myhosts()],
            code:purge(Module),
            code:delete(Module),
            code:del_path(module_ebin_dir(Module)),
            delete_path(module_lib_dir(Module)),
            ejabberd_config:reload_file();
        false ->
            {error, not_installed}
    end.

upgrade() ->
    [{Package, upgrade(Package)} || {Package, _Spec} <- installed()].
upgrade(Module) when is_atom(Module) ->
    upgrade(misc:atom_to_binary(Module));
upgrade(Package) when is_binary(Package) ->
    uninstall(Package),
    install(Package).

add_sources(Path) when is_list(Path) ->
    add_sources(iolist_to_binary(module_name(Path)), Path).
add_sources(_, "") ->
    {error, no_url};
add_sources(Module, Path) when is_atom(Module), is_list(Path) ->
    add_sources(misc:atom_to_binary(Module), Path);
add_sources(Package, Path) when is_binary(Package), is_list(Path) ->
    DestDir = sources_dir(),
    RepDir = filename:join(DestDir, module_name(Path)),
    delete_path(RepDir),
    case filelib:ensure_dir(RepDir) of
        ok ->
            case {string:left(Path, 4), string:right(Path, 2)} of
                {"http", "ip"} -> extract(zip, geturl(Path), DestDir);
                {"http", "gz"} -> extract(tar, geturl(Path), DestDir);
                {"http", _} -> extract_url(Path, DestDir);
                {"git@", _} -> extract_github_master(Path, DestDir);
                {_, "ip"} -> extract(zip, Path, DestDir);
                {_, "gz"} -> extract(tar, Path, DestDir);
                _ -> {error, unsupported_source}
            end;
        Error ->
            Error
    end.

del_sources(Module) when is_atom(Module) ->
    del_sources(misc:atom_to_binary(Module));
del_sources(Package) when is_binary(Package) ->
    case uninstall(Package) of
        ok ->
            SrcDir = module_src_dir(misc:binary_to_atom(Package)),
            delete_path(SrcDir);
        Error ->
            Error
    end.

check(Module) when is_atom(Module) ->
    check(misc:atom_to_binary(Module));
check(Package) when is_binary(Package) ->
    case {available(Package), installed(Package)} of
        {false, _} ->
            {error, not_available};
        {_, false} ->
            Status = install(Package),
            uninstall(Package),
            case Status of
                ok -> check_sources(misc:binary_to_atom(Package));
                Error -> Error
            end;
        _ ->
            check_sources(misc:binary_to_atom(Package))
    end.

%% -- archives and variables functions

geturl(Url) ->
    geturl(Url, []).
geturl(Url, UsrOpts) ->
    geturl(Url, [], UsrOpts).
geturl(Url, Hdrs, UsrOpts) ->
    Host = case getenv("PROXY_SERVER", "", ":") of
        [H, Port] -> [{proxy_host, H}, {proxy_port, list_to_integer(Port)}];
        [H] -> [{proxy_host, H}, {proxy_port, 8080}];
        _ -> []
    end,
    User = case getenv("PROXY_USER", "", [4]) of
        [U, Pass] -> [{proxy_user, U}, {proxy_password, Pass}];
        _ -> []
    end,
    case p1_http:request(get, Url, Hdrs, [], Host++User++UsrOpts++[{version, "HTTP/1.0"}]) of
        {ok, 200, Headers, Response} ->
            {ok, Headers, Response};
        {ok, Code, _Headers, Response} ->
            {error, {Code, Response}};
        {error, Reason} ->
            {error, Reason}
    end.

getenv(Env) ->
    getenv(Env, "").
getenv(Env, Default) ->
    case os:getenv(Env) of
        false -> Default;
        "" -> Default;
        Value -> Value
    end.
getenv(Env, Default, Separator) ->
    string:tokens(getenv(Env, Default), Separator).

extract(zip, {ok, _, Body}, DestDir) ->
    extract(zip, iolist_to_binary(Body), DestDir);
extract(tar, {ok, _, Body}, DestDir) ->
    extract(tar, {binary, iolist_to_binary(Body)}, DestDir);
extract(_, {error, Reason}, _) ->
    {error, Reason};
extract(zip, Zip, DestDir) ->
    case zip:extract(Zip, [{cwd, DestDir}]) of
        {ok, _} -> ok;
        Error -> Error
    end;
extract(tar, Tar, DestDir) ->
    erl_tar:extract(Tar, [compressed, {cwd, DestDir}]).

extract_url(Path, DestDir) ->
    hd([extract_github_master(Path, DestDir) || string:str(Path, "github") > 0]
     ++[{error, unsupported_source}]).

extract_github_master(Repos, DestDir) ->
    Base = case string:tokens(Repos, ":") of
        ["git@github.com", T1] -> "https://github.com/"++T1;
        _ -> Repos
    end,
    Url = case lists:reverse(Base) of
        [$t,$i,$g,$.|T2] -> lists:reverse(T2);
        _ -> Base
    end,
    case extract(zip, geturl(Url++"/archive/master.zip"), DestDir) of
        ok ->
            RepDir = filename:join(DestDir, module_name(Repos)),
            file:rename(RepDir++"-master", RepDir);
        Error ->
            Error
    end.

copy(From, To) ->
    case filelib:is_dir(From) of
        true ->
            Copy = fun(F) ->
                    SubFrom = filename:join(From, F),
                    SubTo = filename:join(To, F),
                    copy(SubFrom, SubTo)
            end,
            lists:foldl(fun(ok, ok) -> ok;
                           (ok, Error) -> Error;
                           (Error, _) -> Error
                end, ok,
                [Copy(filename:basename(X)) || X<-filelib:wildcard(From++"/*")]);
        false ->
            filelib:ensure_dir(To),
            case file:copy(From, To) of
                {ok, _} -> ok;
                Error -> Error
            end
    end.

delete_path(Path) ->
    case filelib:is_dir(Path) of
        true ->
            [delete_path(SubPath) || SubPath <- filelib:wildcard(Path++"/*")],
            file:del_dir(Path);
        false ->
            file:delete(Path)
    end.

modules_dir() ->
    DefaultDir = filename:join(getenv("HOME"), ".ejabberd-modules"),
    getenv("CONTRIB_MODULES_PATH", DefaultDir).

sources_dir() ->
    filename:join(modules_dir(), "sources").

config_dir() ->
    DefaultDir = filename:join(modules_dir(), "conf"),
    getenv("CONTRIB_MODULES_CONF_DIR", DefaultDir).

module_lib_dir(Package) ->
    filename:join(modules_dir(), Package).

module_ebin_dir(Package) ->
    filename:join(module_lib_dir(Package), "ebin").

module_src_dir(Package) ->
    Rep = module_name(Package),
    SrcDir = sources_dir(),
    Standalone = filelib:wildcard(Rep, SrcDir),
    Jungle = filelib:wildcard("*/"++Rep, SrcDir),
    case Standalone++Jungle of
        [RepDir|_] -> filename:join(SrcDir, RepDir);
        _ -> filename:join(SrcDir, Rep)
    end.

module_name(Id) ->
    filename:basename(filename:rootname(Id)).

module(Id) ->
    misc:binary_to_atom(iolist_to_binary(module_name(Id))).

module_spec(Spec) ->
    [{path, filename:dirname(Spec)}
      | case consult(Spec) of
            {ok, Meta} -> Meta;
            _ -> []
        end].

modules_spec(Dir, Path) ->
    Wildcard = filename:join(Path, "*.spec"),
    lists:sort(
        [{module(Match), module_spec(filename:join(Dir, Match))}
         || Match <- filelib:wildcard(Wildcard, Dir)]).

short_spec({Module, Attrs}) when is_atom(Module), is_list(Attrs) ->
    {Module, proplists:get_value(summary, Attrs, "")}.

is_contrib_allowed() ->
    ejabberd_config:get_option(allow_contrib_modules,
               fun(false) -> false;
                  (no) -> false;
                  (_) -> true
            end, true).

%% -- build functions

check_sources(Module) ->
    SrcDir = module_src_dir(Module),
    SpecFile = filename:flatten([Module, ".spec"]),
    {ok, Dir} = file:get_cwd(),
    file:set_cwd(SrcDir),
    HaveSrc = case filelib:is_dir("src") or filelib:is_dir("lib") of
        true -> [];
        false -> [{missing, "src (Erlang) or lib (Elixir) sources directory"}]
    end,
    DirCheck = lists:foldl(
            fun({Type, Name}, Acc) ->
                case filelib:Type(Name) of
                    true -> Acc;
                    false -> [{missing, Name}|Acc]
                end
            end, HaveSrc, [{is_file, "README.txt"},
                           {is_file, "COPYING"},
                           {is_file, SpecFile}]),
    SpecCheck = case consult(SpecFile) of
        {ok, Spec} ->
            lists:foldl(
                fun(Key, Acc) ->
                    case lists:keysearch(Key, 1, Spec) of
                        false -> [{missing_meta, Key}|Acc];
                        {value, {Key, [_NoEmpty|_]}} -> Acc;
                        {value, {Key, Val}} -> [{invalid_meta, {Key, Val}}|Acc]
                    end
                end, [], [author, summary, home, url]);
        {error, Error} ->
            [{invalid_spec, Error}]
    end,
    file:set_cwd(Dir),
    Result = DirCheck ++ SpecCheck,
    case Result of
        [] -> ok;
        _ -> {error, Result}
    end.

compile_and_install(Module, Spec) ->
    SrcDir = module_src_dir(Module),
    LibDir = module_lib_dir(Module),
    case filelib:is_dir(SrcDir) of
        true ->
            case compile_deps(SrcDir) of
                ok ->
                    case compile(SrcDir) of
                        ok -> install(Module, Spec, SrcDir, LibDir);
                        Error -> Error
                    end;
                Error ->
                    Error
            end;
        false ->
            Path = proplists:get_value(url, Spec, ""),
            case add_sources(Module, Path) of
                ok -> compile_and_install(Module, Spec);
                Error -> Error
            end
    end.

compile_deps(LibDir) ->
    Deps = filename:join(LibDir, "deps"),
    case filelib:is_dir(Deps) of
        true -> ok;  % assume deps are included
        false -> fetch_rebar_deps(LibDir)
    end,
    Rs = [compile(Dep) || Dep <- filelib:wildcard(filename:join(Deps, "*"))],
    compile_result(Rs).

compile(LibDir) ->
    Bin = filename:join(LibDir, "ebin"),
    Inc = filename:join(LibDir, "include"),
    Lib = filename:join(LibDir, "lib"),
    Src = filename:join(LibDir, "src"),
    Options = [{outdir, Bin}, {i, Inc} | compile_options()],
    filelib:ensure_dir(filename:join(Bin, ".")),
    [copy(App, Bin) || App <- filelib:wildcard(Src++"/*.app")],
    Er = [compile_erlang_file(Bin, File, Options)
          || File <- filelib:wildcard(Src++"/*.erl")],
    Ex = [compile_elixir_file(Bin, File)
          || File <- filelib:wildcard(Lib ++ "/*.ex")],
    compile_result(Er++Ex).

compile_result(Results) ->
    case lists:dropwhile(
            fun({ok, _}) -> true;
               (_) -> false
            end, Results) of
        [] -> ok;
        [Error|_] -> Error
    end.

compile_options() ->
    [verbose, report_errors, report_warnings]
    ++ [{i, filename:join(app_dir(App), "include")}
        || App <- [fast_xml, xmpp, p1_utils, ejabberd]].

app_dir(App) ->
    case code:lib_dir(App) of
        {error, bad_name} ->
            case code:which(App) of
                Beam when is_list(Beam) ->
                    filename:dirname(filename:dirname(Beam));
                _ ->
                    "."
            end;
        Dir ->
            Dir
    end.

compile_erlang_file(Dest, File) ->
    compile_erlang_file(Dest, File, compile_options()).

compile_erlang_file(Dest, File, ErlOptions) ->
    Options = [{outdir, Dest} | ErlOptions],
    case compile:file(File, Options) of
        {ok, Module} -> {ok, Module};
        {ok, Module, _} -> {ok, Module};
        {ok, Module, _, _} -> {ok, Module};
        error -> {error, {compilation_failed, File}};
        {error, E, W} -> {error, {compilation_failed, File, E, W}}
    end.

compile_elixir_file(Dest, File) when is_list(Dest) and is_list(File) ->
  compile_elixir_file(list_to_binary(Dest), list_to_binary(File));

compile_elixir_file(Dest, File) ->
  try 'Elixir.Kernel.ParallelCompiler':files_to_path([File], Dest, []) of
    [Module] -> {ok, Module}
  catch
    _ -> {error, {compilation_failed, File}}
  end.

install(Module, Spec, SrcDir, LibDir) ->
    {ok, CurDir} = file:get_cwd(),
    file:set_cwd(SrcDir),
    Files1 = [{File, copy(File, filename:join(LibDir, File))}
                  || File <- filelib:wildcard("{ebin,priv,conf,include}/**")],
    Files2 = [{File, copy(File, filename:join(LibDir, filename:join(lists:nthtail(2,filename:split(File)))))}
                  || File <- filelib:wildcard("deps/*/{ebin,priv}/**")],
    Errors = lists:dropwhile(fun({_, ok}) -> true;
                                (_) -> false
            end, Files1++Files2),
    Result = case Errors of
        [{F, {error, E}}|_] ->
            {error, {F, E}};
        [] ->
            SpecPath = proplists:get_value(path, Spec),
            SpecFile = filename:flatten([Module, ".spec"]),
            copy(filename:join(SpecPath, SpecFile), filename:join(LibDir, SpecFile))
    end,
    file:set_cwd(CurDir),
    Result.

%% -- minimalist rebar spec parser, only support git

fetch_rebar_deps(SrcDir) ->
    case rebar_deps(filename:join(SrcDir, "rebar.config"))
      ++ rebar_deps(filename:join(SrcDir, "rebar.config.script")) of
        [] ->
            ok;
        Deps ->
            {ok, CurDir} = file:get_cwd(),
            file:set_cwd(SrcDir),
            filelib:ensure_dir(filename:join("deps", ".")),
            lists:foreach(fun({_App, Cmd}) ->
                        os:cmd("cd deps; "++Cmd++"; cd ..")
                end, Deps),
            file:set_cwd(CurDir)
    end.

rebar_deps(Script) ->
    case file:script(Script) of
        {ok, Config} when is_list(Config) ->
            [rebar_dep(Dep) || Dep <- proplists:get_value(deps, Config, [])];
        {ok, {deps, Deps}} ->
            [rebar_dep(Dep) || Dep <- Deps];
        _ ->
            []
    end.
rebar_dep({App, _, {git, Url}}) ->
    {App, "git clone "++Url++" "++filename:basename(App)};
rebar_dep({App, _, {git, Url, {branch, Ref}}}) ->
    {App, "git clone -n "++Url++" "++filename:basename(App)++
     "; (cd "++filename:basename(App)++
     "; git checkout -q origin/"++Ref++")"};
rebar_dep({App, _, {git, Url, {tag, Ref}}}) ->
    {App, "git clone -n "++Url++" "++filename:basename(App)++
     "; (cd "++filename:basename(App)++
     "; git checkout -q "++Ref++")"};
rebar_dep({App, _, {git, Url, Ref}}) ->
    {App, "git clone -n "++Url++" "++filename:basename(App)++
     "; (cd "++filename:basename(App)++
     "; git checkout -q "++Ref++")"}.

%% -- YAML spec parser

consult(File) ->
    case fast_yaml:decode_from_file(File, [plain_as_atom]) of
        {ok, []} -> {ok, []};
        {ok, [Doc|_]} -> {ok, [format(Spec) || Spec <- Doc]};
        {error, Err} -> {error, fast_yaml:format_error(Err)}
    end.

format({Key, Val}) when is_binary(Val) ->
    {Key, binary_to_list(Val)};
format({Key, Val}) -> % TODO: improve Yaml parsing
    {Key, Val}.

opt_type(allow_contrib_modules) ->
    fun (false) -> false;
        (no) -> false;
        (_) -> true
    end;
opt_type(_) -> [allow_contrib_modules].
