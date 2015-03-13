%%%----------------------------------------------------------------------
%%% File    : ext_mod.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : external modules management
%%% Created : 19 Feb 2015 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2006-2015   ProcessOne
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
-author("Christophe Romain <christophe.romain@process-one.net>").

%% Packaging service
-export([start/0, stop/0, update/0, check/1,
         available_command/0, available/0, available/1,
         installed_command/0, installed/0, installed/1,
         install/1, uninstall/1,
         upgrade/0, upgrade/1,
         add_sources/2, del_sources/1]).

-include("ejabberd_commands.hrl").

-define(REPOS, "https://github.com/processone/ejabberd-contrib").

%% -- ejabberd init and commands

start() ->
    case is_contrib_allowed() of
        true ->
            [code:add_patha(module_ebin_dir(Module))
             || {Module, _} <- installed()],
            application:start(inets),
            ejabberd_commands:register_commands(commands());
        false ->
            ok
    end.

stop() ->
    ejabberd_commands:unregister_commands(commands()).

commands() ->
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
    lists:foreach(fun({Package, Spec}) ->
                Path = proplists:get_value(url, Spec, ""),
                add_sources(Package, Path)
        end, modules_spec(sources_dir(), "*")).

available() ->
    Jungle = modules_spec(sources_dir(), "*/*"),
    Standalone = modules_spec(sources_dir(), "*"),
    lists:keysort(1,
        lists:foldl(fun({Key, Val}, Acc) ->
                lists:keystore(Key, 1, Acc, {Key, Val})
            end, Jungle, Standalone)).
available(Module) when is_atom(Module) ->
    available(jlib:atom_to_binary(Module));
available(Package) when is_binary(Package) ->
    Available = [jlib:atom_to_binary(K) || K<-proplists:get_keys(available())],
    lists:member(Package, Available).

available_command() ->
    [short_spec(Item) || Item <- available()].

installed() ->
    modules_spec(modules_dir(), "*").
installed(Module) when is_atom(Module) ->
    installed(jlib:atom_to_binary(Module));
installed(Package) when is_binary(Package) ->
    Installed = [jlib:atom_to_binary(K) || K<-proplists:get_keys(installed())],
    lists:member(Package, Installed).

installed_command() ->
    [short_spec(Item) || Item <- installed()].

install(Module) when is_atom(Module) ->
    install(jlib:atom_to_binary(Module));
install(Package) when is_binary(Package) ->
    Spec = [S || {Mod, S} <- available(), jlib:atom_to_binary(Mod)==Package],
    case {Spec, installed(Package), is_contrib_allowed()} of
        {_, _, false} ->
            {error, not_allowed};
        {[], _, _} ->
            {error, not_available};
        {_, true, _} ->
            {error, conflict};
        {[Attrs], _, _} ->
            Module = jlib:binary_to_atom(Package),
            case compile_and_install(Module, Attrs) of
                ok ->
                    code:add_patha(module_ebin_dir(Module)),
                    ok;
                Error ->
                    delete_path(module_lib_dir(Module)),
                    Error
            end
    end.

uninstall(Module) when is_atom(Module) ->
    uninstall(jlib:atom_to_binary(Module));
uninstall(Package) when is_binary(Package) ->
    case installed(Package) of
        true ->
            Module = jlib:binary_to_atom(Package),
            [catch gen_mod:stop_module(Host, Module)
             || Host <- ejabberd_config:get_myhosts()],
            code:purge(Module),
            code:delete(Module),
            code:del_path(module_ebin_dir(Module)),
            delete_path(module_lib_dir(Module));
        false ->
            {error, not_installed}
    end.

upgrade() ->
    [{Package, upgrade(Package)} || {Package, _Spec} <- installed()].
upgrade(Module) when is_atom(Module) ->
    upgrade(jlib:atom_to_binary(Module));
upgrade(Package) when is_binary(Package) ->
    uninstall(Package),
    install(Package).

add_sources(Path) when is_list(Path) ->
    add_sources(module_name(Path), Path).
add_sources(_, "") ->
    {error, no_url};
add_sources(Module, Path) when is_atom(Module), is_list(Path) ->
    add_sources(jlib:atom_to_binary(Module), Path);
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
    del_sources(jlib:atom_to_binary(Module));
del_sources(Package) when is_binary(Package) ->
    case uninstall(Package) of
        ok ->
            SrcDir = module_src_dir(jlib:binary_to_atom(Package)),
            delete_path(SrcDir);
        Error ->
            Error
    end.

check(Module) when is_atom(Module) ->
    check(jlib:atom_to_binary(Module));
check(Package) when is_binary(Package) ->
    case {available(Package), installed(Package)} of
        {false, _} ->
            {error, not_available};
        {_, false} ->
            Status = install(Package),
            uninstall(Package),
            case Status of
                ok -> check_sources(jlib:binary_to_atom(Package));
                Error -> Error
            end;
        _ ->
            check_sources(jlib:binary_to_atom(Package))
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
    case httpc:request(get, {Url, Hdrs}, Host++User++UsrOpts, []) of
        {ok, {{_, 200, _}, Headers, Response}} ->
            {ok, Headers, Response};
        {ok, {{_, Code, _}, _Headers, Response}} ->
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
    case extract(zip, geturl(Repos ++ "/archive/master.zip"), DestDir) of
        ok ->
            RepDir = filename:join(DestDir, module_name(Repos)),
            file:rename(RepDir++"-master", RepDir);
        Error ->
            Error
    end.

copy_file(From, To) ->
    filelib:ensure_dir(To),
    file:copy(From, To).

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
    jlib:binary_to_atom(iolist_to_binary(module_name(Id))).

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
            {ok, Dir} = file:get_cwd(),
            file:set_cwd(SrcDir),
            Result = case compile(Module, Spec, LibDir) of
                ok -> install(Module, Spec, LibDir);
                Error -> Error
            end,
            file:set_cwd(Dir),
            Result;
        false ->
            Path = proplists:get_value(url, Spec, ""),
            case add_sources(Module, Path) of
                ok -> compile_and_install(Module, Spec);
                Error -> Error
            end
    end.

compile(_Module, _Spec, DestDir) ->
    Ebin = filename:join(DestDir, "ebin"),
    filelib:ensure_dir(filename:join(Ebin, ".")),
    EjabBin = filename:dirname(code:which(ejabberd)),
    EjabInc = filename:join(filename:dirname(EjabBin), "include"),
    Logger = case code:is_loaded(lager) of
        {file, _} -> [{d, 'LAGER'}];
        _ -> []
    end,
    Options = [{outdir, Ebin}, {i, "include"}, {i, EjabInc},
               {d, 'NO_EXT_LIB'},  %% use include instead of include_lib
               verbose, report_errors, report_warnings]
              ++ Logger,
    Result = [case compile:file(File, Options) of
            {ok, _} -> ok;
            {ok, _, _} -> ok;
            {ok, _, _, _} -> ok;
            error -> {error, {compilation_failed, File}};
            Error -> Error
        end
        || File <- filelib:wildcard("src/*.erl")],
    case lists:dropwhile(
            fun(ok) -> true;
                (_) -> false
            end, Result) of
        [] -> ok;
        [Error|_] -> Error
    end.

install(Module, _Spec, DestDir) ->
    SpecFile = filename:flatten([Module, ".spec"]),
    [copy_file(File, filename:join(DestDir, File))
     || File <- [SpecFile | filelib:wildcard("{ebin,priv,conf,include}/**")]],
    ok.

%% -- YAML spec parser

consult(File) ->
    case p1_yaml:decode_from_file(File, [plain_as_atom]) of
        {ok, []} -> {ok, []};
        {ok, [Doc|_]} -> {ok, [format(Spec) || Spec <- Doc]};
        {error, Err} -> {error, p1_yaml:format_error(Err)}
    end.

format({Key, Val}) when is_binary(Val) ->
    {Key, binary_to_list(Val)};
format({Key, Val}) -> % TODO: improve Yaml parsing
    {Key, Val}.
