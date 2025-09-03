%%%----------------------------------------------------------------------
%%% File    : ext_mod.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : external modules management
%%% Created : 19 Feb 2015 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2006-2025   ProcessOne
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

-behaviour(gen_server).
-author("Christophe Romain <christophe.romain@process-one.net>").

-export([start_link/0,
         update/0,
         check/1,
         available_command/0,
         available/0, available/1,
         installed_command/0,
         installed/0, installed/1,
         install/1,
         uninstall/1,
         upgrade/0, upgrade/1,
         add_paths/0,
         add_sources/1, add_sources/2,
         del_sources/1,
         modules_dir/0,
         install_contrib_modules/2,
         config_dir/0,
         get_commands_spec/0]).
-export([modules_configs/0, module_ebin_dir/1]).
-export([compile_erlang_file/2, compile_elixir_files/2]).
-export([web_menu_node/3, web_page_node/3, webadmin_node_contrib/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ejabberd_commands.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include("logger.hrl").
-include("translate.hrl").

-include_lib("xmpp/include/xmpp.hrl").
-include_lib("stdlib/include/zip.hrl").

-define(REPOS, "git@github.com:processone/ejabberd-contrib.git").

-record(state, {}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    process_flag(trap_exit, true),
    add_paths(),
    application:start(inets),
    inets:start(httpc, [{profile, ext_mod}]),
    ejabberd_commands:register_commands(get_commands_spec()),
    ejabberd_hooks:add(webadmin_menu_node, ?MODULE, web_menu_node, 50),
    ejabberd_hooks:add(webadmin_page_node, ?MODULE, web_page_node, 50),
    {ok, #state{}}.


add_paths() ->
    [ code:add_pathsz([module_ebin_dir(Module) | module_deps_dirs(Module)])
      || {Module, _} <- installed() ].


handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.


handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.


handle_info({'ETS-TRANSFER', Table, Process, Module}, State) ->
    ?DEBUG("ejabberd now controls ETS table ~p from process ~p for module ~p",
           [Table, Process, Module]),
    {noreply, State};

handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ejabberd_hooks:delete(webadmin_menu_node, ?MODULE, web_menu_node, 50),
    ejabberd_hooks:delete(webadmin_page_node, ?MODULE, web_page_node, 50),
    ejabberd_commands:unregister_commands(get_commands_spec()).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% -- ejabberd commands
get_commands_spec() ->
    [#ejabberd_commands{
       name = modules_update_specs,
       tags = [modules],
       desc = "Update the module source code from Git",
       longdesc = "A connection to Internet is required",
       module = ?MODULE,
       function = update,
       args = [],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = modules_available,
       tags = [modules],
       desc = "List the contributed modules available to install",
       module = ?MODULE,
       function = available_command,
       result_desc = "List of tuples with module name and description",
       result_example = [{mod_cron, "Execute scheduled commands"},
                         {mod_rest, "ReST frontend"}],
       args = [],
       result = {modules, {list,
                           {module, {tuple,
                                     [{name, atom},
                                      {summary, string}]}}}}
      },
     #ejabberd_commands{
       name = modules_installed,
       tags = [modules],
       desc = "List the contributed modules already installed",
       module = ?MODULE,
       function = installed_command,
       result_desc = "List of tuples with module name and description",
       result_example = [{mod_cron, "Execute scheduled commands"},
                         {mod_rest, "ReST frontend"}],
       args = [],
       result = {modules, {list,
                           {module, {tuple,
                                     [{name, atom},
                                      {summary, string}]}}}}
      },
     #ejabberd_commands{
       name = module_install,
       tags = [modules],
       desc = "Compile, install and start an available contributed module",
       module = ?MODULE,
       function = install,
       args_desc = ["Module name"],
       args_example = [<<"mod_rest">>],
       args = [{module, binary}],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = module_uninstall,
       tags = [modules],
       desc = "Uninstall a contributed module",
       module = ?MODULE,
       function = uninstall,
       args_desc = ["Module name"],
       args_example = [<<"mod_rest">>],
       args = [{module, binary}],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = module_upgrade,
       tags = [modules],
       desc = "Upgrade the running code of an installed module",
       longdesc = "In practice, this uninstalls, cleans the compiled files, and installs the module",
       note = "improved in 25.07",
       module = ?MODULE,
       function = upgrade,
       args_desc = ["Module name"],
       args_example = [<<"mod_rest">>],
       args = [{module, binary}],
       result = {res, rescode}
      },
     #ejabberd_commands{
       name = module_check,
       tags = [modules],
       desc = "Check the contributed module repository compliance",
       module = ?MODULE,
       function = check,
       args_desc = ["Module name"],
       args_example = [<<"mod_rest">>],
       args = [{module, binary}],
       result = {res, rescode}
      }].
%% -- public modules functions


update() ->
    Contrib = maps:put(?REPOS, [], maps:new()),
    Jungles = lists:foldl(fun({Package, Spec}, Acc) ->
                                  Repo = proplists:get_value(url, Spec, ""),
                                  Mods = maps:get(Repo, Acc, []),
                                  maps:put(Repo, [Package | Mods], Acc)
                          end,
                          Contrib,
                          modules_spec(sources_dir(), "*/*")),
    Repos = maps:fold(fun(Repo, _Mods, Acc) ->
                              Update = add_sources(Repo),
                              ?INFO_MSG("Update packages from repo ~ts: ~p", [Repo, Update]),
                              case Update of
                                  ok -> Acc;
                                  Error -> [{repository, Repo, Error} | Acc]
                              end
                      end,
                      [],
                      Jungles),
    Res = lists:foldl(fun({Package, Spec}, Acc) ->
                              Repo = proplists:get_value(url, Spec, ""),
                              Update = add_sources(Package, Repo),
                              ?INFO_MSG("Update package ~ts: ~p", [Package, Update]),
                              case Update of
                                  ok -> Acc;
                                  Error -> [{Package, Repo, Error} | Acc]
                              end
                      end,
                      Repos,
                      modules_spec(sources_dir(), "*")),
    case Res of
        [] -> ok;
        [Error | _] -> Error
    end.


available() ->
    Jungle = modules_spec(sources_dir(), "*/*"),
    Standalone = modules_spec(sources_dir(), "*"),
    lists:keysort(1,
                  lists:foldl(fun({Key, Val}, Acc) ->
                                      lists:keystore(Key, 1, Acc, {Key, Val})
                              end,
                              Jungle,
                              Standalone)).


available(Module) when is_atom(Module) ->
    available(misc:atom_to_binary(Module));
available(Package) when is_binary(Package) ->
    Available = [ misc:atom_to_binary(K) || K <- proplists:get_keys(available()) ],
    lists:member(Package, Available).


available_command() ->
    [ short_spec(Item) || Item <- available() ].


installed() ->
    modules_spec(modules_dir(), "*").


installed(Module) when is_atom(Module) ->
    installed(misc:atom_to_binary(Module));
installed(Package) when is_binary(Package) ->
    Installed = [ misc:atom_to_binary(K) || K <- proplists:get_keys(installed()) ],
    lists:member(Package, Installed).


installed_command() ->
    [ short_spec(Item) || Item <- installed() ].


install(Module) when is_atom(Module) ->
    install(misc:atom_to_binary(Module), undefined);
install(Package) when is_binary(Package) ->
    install(Package, undefined).


install(Package, Config) when is_binary(Package) ->
    Spec = [ S || {Mod, S} <- available(), misc:atom_to_binary(Mod) == Package ],
    case {Spec, installed(Package), is_contrib_allowed(Config)} of
        {_, _, false} ->
            {error, not_allowed};
        {[], _, _} ->
            {error, not_available};
        {_, true, _} ->
            {error, conflict};
        {[Attrs], _, _} ->
            Module = misc:binary_to_atom(Package),
            case compile_and_install(Module, Attrs, Config) of
                ok ->
                    code:add_pathsz([module_ebin_dir(Module) | module_deps_dirs(Module)]),
                    ejabberd_config_reload(Config),
                    maybe_print_module_status(Module),
                    copy_commit_json(Package, Attrs),
                    ModuleRuntime = get_runtime_module_name(Module),
                    case erlang:function_exported(ModuleRuntime, post_install, 0) of
                        true -> ModuleRuntime:post_install();
                        _ -> ok
                    end;
                Error ->
                    delete_path(module_lib_dir(Module)),
                    Error
            end
    end.


ejabberd_config_reload(Config) when is_list(Config) ->
    %% Don't reload config when ejabberd is starting
    %% because it will be reloaded after installing
    %% all the external modules from install_contrib_modules
    ok;
ejabberd_config_reload(undefined) ->
    ejabberd_config:reload().


maybe_print_module_status(Module) ->
    case get_module_status_el(Module) of
        [_, {xmlcdata, String}] ->
            io:format("~ts~n", [String]);
        _ ->
            ok
    end.


uninstall(Module) when is_atom(Module) ->
    uninstall(misc:atom_to_binary(Module));
uninstall(Package) when is_binary(Package) ->
    case installed(Package) of
        true ->
            Module = misc:binary_to_atom(Package),
            ModuleRuntime = get_runtime_module_name(Module),
            case erlang:function_exported(ModuleRuntime, pre_uninstall, 0) of
                true -> ModuleRuntime:pre_uninstall();
                _ -> ok
            end,
            [ catch gen_mod:stop_module(Host, ModuleRuntime)
              || Host <- ejabberd_option:hosts() ],
            code:purge(ModuleRuntime),
            code:delete(ModuleRuntime),
            [ code:del_path(PathDelete) || PathDelete <- [module_ebin_dir(Module) | module_deps_dirs(Module)] ],
            delete_path(module_lib_dir(Module)),
            ejabberd_config:reload();
        false ->
            {error, not_installed}
    end.


upgrade() ->
    [ {Package, upgrade(Package)} || {Package, _Spec} <- installed() ].


upgrade(Module) when is_atom(Module) ->
    upgrade(misc:atom_to_binary(Module));
upgrade(Package) when is_binary(Package) ->
    uninstall(Package),
    clean(Package),
    install(Package).


clean(Package) ->
    Spec = [ S || {Mod, S} <- available(), misc:atom_to_binary(Mod) == Package ],
    case Spec of
        [] ->
            {error, not_available};
        [Attrs] ->
            Path = proplists:get_value(path, Attrs),
            [ delete_path(SubPath) || SubPath <- filelib:wildcard(Path ++ "/{deps,ebin}") ]
    end.


add_sources(Path) when is_list(Path) ->
    add_sources(iolist_to_binary(module_name(Path)), Path).


add_sources(_, "") ->
    {error, no_url};
add_sources(Module, Path) when is_atom(Module), is_list(Path) ->
    add_sources(misc:atom_to_binary(Module), Path);
add_sources(Package, Path) when is_binary(Package), is_list(Path) ->
    DestDir = sources_dir(),
    RepDir = filename:join(DestDir, module_name(Path)),
    delete_path(RepDir, binary_to_list(Package)),
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
    case getenv("PROXY_SERVER", "", ":") of
        [H, Port] ->
            httpc:set_options([{proxy, {{H, list_to_integer(Port)}, []}}], ext_mod);
        [H] ->
            httpc:set_options([{proxy, {{H, 8080}, []}}], ext_mod);
        _ ->
            ok
    end,
    User = case getenv("PROXY_USER", "", ":") of
               [U, Pass] -> [{proxy_auth, {U, Pass}}];
               _ -> []
           end,
    UA = {"User-Agent", "ejabberd/ext_mod"},
    case httpc:request(get, {Url, [UA]}, User, [{body_format, binary}], ext_mod) of
        {ok, {{_, 200, _}, Headers, Response}} ->
            {ok, Headers, Response};
        {ok, {{_, 403, Reason}, _Headers, _Response}} ->
            {error, Reason};
        {ok, {{_, Code, _}, _Headers, Response}} ->
            {error, {Code, Response}};
        {error, Reason} ->
            {error, Reason}
    end.


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
    {ok, DirList} = zip:list_dir(Zip),
    Offending =
        lists:filter(fun(#zip_comment{}) ->
                             false;
                        (#zip_file{name = Filename}) ->
                             absolute == filename:pathtype(Filename)
                     end,
                     DirList),
    case Offending of
        [] ->
            extract(zip_verified, Zip, DestDir);
        _ ->
            Filenames = [ F#zip_file.name || F <- Offending ],
            ?ERROR_MSG("The zip file includes absolute file paths:~n  ~p", [Filenames]),
            {error, {zip_absolute_path, Filenames}}
    end;
extract(zip_verified, Zip, DestDir) ->
    case zip:extract(Zip, [{cwd, DestDir}]) of
        {ok, _} -> ok;
        Error -> Error
    end;
extract(tar, Tar, DestDir) ->
    erl_tar:extract(Tar, [compressed, {cwd, DestDir}]).


extract_url(Path, DestDir) ->
    hd([ extract_github_master(Path, DestDir) || string:str(Path, "github") > 0 ] ++
       [{error, unsupported_source}]).


extract_github_master(Repos, DestDir) ->
    Base = case string:tokens(Repos, ":") of
               ["git@github.com", T1] -> "https://github.com/" ++ T1;
               _ -> Repos
           end,
    Url = case lists:reverse(Base) of
              [$t, $i, $g, $. | T2] -> lists:reverse(T2);
              _ -> Base
          end,
    case extract(zip, geturl(Url ++ "/archive/master.zip"), DestDir) of
        ok ->
            RepDir = filename:join(DestDir, module_name(Repos)),
            RepDirSpec = filename:join(DestDir, module_spec_name(RepDir)),
            file:rename(RepDir ++ "-master", RepDirSpec),
            maybe_write_commit_json(Url, RepDirSpec);
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
                        end,
                        ok,
                        [ Copy(filename:basename(X)) || X <- filelib:wildcard(From ++ "/*") ]);
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
            [ delete_path(SubPath) || SubPath <- filelib:wildcard(Path ++ "/*") ],
            file:del_dir(Path);
        false ->
            file:delete(Path)
    end.


delete_path(Path, Package) ->
    delete_path(filename:join(filename:dirname(Path), Package)).


modules_dir() ->
    DefaultDir = filename:join(misc:get_home(), ".ejabberd-modules"),
    getenv("CONTRIB_MODULES_PATH", DefaultDir).


sources_dir() ->
    filename:join(modules_dir(), "sources").


config_dir() ->
    DefaultDir = filename:join(modules_dir(), "conf"),
    getenv("CONTRIB_MODULES_CONF_DIR", DefaultDir).


-spec modules_configs() -> [binary()].
modules_configs() ->
    Fs = [ {filename:rootname(filename:basename(F)), F}
           || F <- filelib:wildcard(config_dir() ++ "/*.{yml,yaml}") ++
                   filelib:wildcard(modules_dir() ++ "/*/conf/*.{yml,yaml}") ],
    [ unicode:characters_to_binary(proplists:get_value(F, Fs))
      || F <- proplists:get_keys(Fs) ].


module_lib_dir(Package) ->
    filename:join(modules_dir(), Package).


module_ebin_dir(Package) ->
    filename:join(module_lib_dir(Package), "ebin").


module_src_dir(Package) ->
    Rep = module_name(Package),
    SrcDir = sources_dir(),
    Standalone = filelib:wildcard(Rep, SrcDir),
    Jungle = filelib:wildcard("*/" ++ Rep, SrcDir),
    case Standalone ++ Jungle of
        [RepDir | _] -> filename:join(SrcDir, RepDir);
        _ -> filename:join(SrcDir, Rep)
    end.


module_name(Id) ->
    filename:basename(filename:rootname(Id)).


module_spec_name(Path) ->
    case filelib:wildcard(filename:join(Path ++ "-master", "*.spec")) of
        "" ->
            module_name(Path);
        ModuleName ->
            filename:basename(ModuleName, ".spec")
    end.


module(Id) ->
    misc:binary_to_atom(iolist_to_binary(module_name(Id))).


module_spec(Spec) ->
    [{path, filename:dirname(Spec)} | case consult(Spec) of
                                          {ok, Meta} -> Meta;
                                          _ -> []
                                      end].


modules_spec(Dir, Path) ->
    Wildcard = filename:join(Path, "*.spec"),
    lists:sort(
      [ {module(Match), module_spec(filename:join(Dir, Match))}
        || Match <- filelib:wildcard(Wildcard, Dir) ]).


short_spec({Module, Attrs}) when is_atom(Module), is_list(Attrs) ->
    {Module, proplists:get_value(summary, Attrs, "")}.


is_contrib_allowed(Config) when is_list(Config) ->
    case lists:keyfind(allow_contrib_modules, 1, Config) of
        false -> true;
        {_, false} -> false;
        {_, true} -> true
    end;
is_contrib_allowed(undefined) ->
    ejabberd_option:allow_contrib_modules().


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
                             false -> [{missing, Name} | Acc]
                         end
                 end,
                 HaveSrc,
                 [{is_file, "README.md"},
                  {is_file, "COPYING"},
                  {is_file, SpecFile}]),
    SpecCheck = case consult(SpecFile) of
                    {ok, Spec} ->
                        lists:foldl(
                          fun(Key, Acc) ->
                                  case lists:keysearch(Key, 1, Spec) of
                                      false -> [{missing_meta, Key} | Acc];
                                      {value, {Key, [_NoEmpty | _]}} -> Acc;
                                      {value, {Key, Val}} -> [{invalid_meta, {Key, Val}} | Acc]
                                  end
                          end,
                          [],
                          [author, summary, home, url]);
                    {error, Error} ->
                        [{invalid_spec, Error}]
                end,
    file:set_cwd(Dir),
    Result = DirCheck ++ SpecCheck,
    case Result of
        [] -> ok;
        _ -> {error, Result}
    end.


compile_and_install(Module, Spec, Config) ->
    SrcDir = module_src_dir(Module),
    LibDir = module_lib_dir(Module),
    case filelib:is_dir(SrcDir) of
        true ->
            case compile_deps(SrcDir) of
                ok ->
                    case compile(SrcDir, filename:join(SrcDir, "deps")) of
                        ok -> install(Module, Spec, SrcDir, LibDir, Config);
                        Error -> Error
                    end;
                Error ->
                    Error
            end;
        false ->
            Path = proplists:get_value(url, Spec, ""),
            case add_sources(Module, Path) of
                ok -> compile_and_install(Module, Spec, Config);
                Error -> Error
            end
    end.


compile_deps(LibDir) ->
    DepsDir = filename:join(LibDir, "deps"),
    case filelib:is_dir(DepsDir) of
        true -> ok;  % assume deps are included
        false -> fetch_rebar_deps(LibDir)
    end,
    Rs = [ compile(Dep, DepsDir) || Dep <- filelib:wildcard(filename:join(DepsDir, "*")) ],
    compile_result(Rs).


compile(LibDir, DepsDir) ->
    Bin = filename:join(LibDir, "ebin"),
    Lib = filename:join(LibDir, "lib"),
    Src = filename:join(LibDir, "src"),
    Includes = [ {i, Inc} || Inc <- filelib:wildcard(DepsDir ++ "/**/include") ],
    Options = [{outdir, Bin},
               {i, LibDir ++ "/.."},
               {i, filename:join(LibDir, "include")} | Includes ++ compile_options()],
    ?DEBUG("compile options: ~p", [Options]),
    filelib:ensure_dir(filename:join(Bin, ".")),
    [ copy(App, filename:join(Bin, filename:basename(App, ".src"))) || App <- filelib:wildcard(Src ++ "/*.app*") ],
    compile_c_files(LibDir),
    ErlFiles = filelib:wildcard(Src ++ "/**/*.erl"),
    ?DEBUG("erl files to compile: ~p", [ErlFiles]),
    Er = [ compile_erlang_file(Bin, File, Options)
           || File <- ErlFiles ],
    Ex = compile_elixir_files(Bin, filelib:wildcard(Lib ++ "/**/*.ex")),
    compile_result(lists:flatten([Er, Ex])).


compile_c_files(LibDir) ->
    case file:read_file_info(filename:join(LibDir, "c_src/Makefile")) of
        {ok, _} ->
            os:cmd("cd " ++ LibDir ++ "; make -C c_src");
        {error, _} ->
            ok
    end.


compile_result(Results) ->
    case lists:dropwhile(
           fun({ok, _}) -> true;
              (_) -> false
           end,
           Results) of
        [] -> ok;
        [Error | _] -> Error
    end.


maybe_define_lager_macro() ->
    case list_to_integer(erlang:system_info(otp_release)) < 22 of
        true -> [{d, 'LAGER'}];
        false -> []
    end.


compile_options() ->
    [verbose, report_errors, report_warnings, debug_info, ?ALL_DEFS] ++
    maybe_define_lager_macro() ++
    [ {i, filename:join(app_dir(App), "include")}
      || App <- [fast_xml, xmpp, p1_utils, ejabberd] ] ++
    [ {i, filename:join(app_dir(App), "include")}
      || App <- [p1_xml, p1_xmpp] ]  % paths used in Debian packages
    ++
    [ {i, filename:join(mod_dir(Mod), "include")}
      || Mod <- installed() ].


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


mod_dir({Package, Spec}) ->
    Default = filename:join(modules_dir(), Package),
    proplists:get_value(path, Spec, Default).


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


-ifdef(ELIXIR_ENABLED).


compile_elixir_files(_, []) ->
    ok;
compile_elixir_files(Dest, [File | _] = Files) when is_list(Dest) and is_list(File) ->
    BinFiles = [ list_to_binary(F) || F <- Files ],
    compile_elixir_files(list_to_binary(Dest), BinFiles);

compile_elixir_files(Dest, Files) ->
    try 'Elixir.Kernel.ParallelCompiler':compile_to_path(Files, Dest, [{return_diagnostics, true}]) of
        {ok, Modules, []} when is_list(Modules) ->
            {ok, Modules};
        {ok, Modules, Warnings} when is_list(Modules) ->
            ?WARNING_MSG("Warnings compiling module: ~n~p", [Warnings]),
            {ok, Modules}
    catch
        A:B ->
            ?ERROR_MSG("Problem ~p compiling Elixir files: ~p~nFiles: ~p", [A, B, Files]),
            {error, {compilation_failed, Files}}
    end.


-else.


compile_elixir_files(_, []) ->
    ok;
compile_elixir_files(_, Files) ->
    ErrorString = "Attempted to compile Elixir files, but Elixir support is "
                  "not available in ejabberd. Try compiling ejabberd using "
                  "'./configure --enable-elixir' or './configure --with-rebar=mix'",
    ?ERROR_MSG(ErrorString, []),
    io:format("Error: " ++ ErrorString ++ "~n", []),
    {error, {elixir_not_available, Files}}.


-endif.


install(Module, Spec, SrcDir, LibDir, Config) ->
    {ok, CurDir} = file:get_cwd(),
    file:set_cwd(SrcDir),
    Files1 = [ {File, copy(File, filename:join(LibDir, File))}
               || File <- filelib:wildcard("{ebin,priv,conf,include}/**") ],
    Files2 = [ {File, copy(File, filename:join(LibDir, filename:join(lists:nthtail(2, filename:split(File)))))}
               || File <- filelib:wildcard("deps/*/ebin/**") ],
    Files3 = [ {File, copy(File, filename:join(LibDir, File))}
               || File <- filelib:wildcard("deps/*/priv/**") ],
    Errors = lists:dropwhile(fun({_, ok}) -> true;
                                (_) -> false
                             end,
                             Files1 ++ Files2 ++ Files3),
    inform_module_configuration(Module, LibDir, Files1, Config),
    Result = case Errors of
                 [{F, {error, E}} | _] ->
                     {error, {F, E}};
                 [] ->
                     SpecPath = proplists:get_value(path, Spec),
                     SpecFile = filename:flatten([Module, ".spec"]),
                     copy(filename:join(SpecPath, SpecFile), filename:join(LibDir, SpecFile))
             end,
    file:set_cwd(CurDir),
    Result.


inform_module_configuration(Module, LibDir, Files1, Config) ->
    Res = lists:filter(fun({[$c, $o, $n, $f | _], ok}) -> true;
                          (_) -> false
                       end,
                       Files1),
    AlreadyConfigured = lists:keymember(Module, 1, get_modules(Config)),
    case {Res, AlreadyConfigured} of
        {[{ConfigPath, ok}], false} ->
            FullConfigPath = filename:join(LibDir, ConfigPath),
            io:format("Module ~p has been installed and started.~n"
                      "It's configured in the file:~n  ~s~n"
                      "Configure the module in that file, or remove it~n"
                      "and configure in your main ejabberd.yml~n",
                      [Module, FullConfigPath]);
        {[{ConfigPath, ok}], true} ->
            FullConfigPath = filename:join(LibDir, ConfigPath),
            file:rename(FullConfigPath, FullConfigPath ++ ".example"),
            io:format("Module ~p has been installed and started.~n"
                      "The ~p configuration in your ejabberd.yml is used.~n",
                      [Module, Module]);
        {[], _} ->
            io:format("Module ~p has been installed.~n"
                      "Now you can configure it in your ejabberd.yml~n",
                      [Module])
    end.


get_modules(Config) when is_list(Config) ->
    {modules, Modules} = lists:keyfind(modules, 1, Config),
    Modules;
get_modules(undefined) ->
    ejabberd_config:get_option(modules).


%% -- minimalist rebar spec parser, only support git


fetch_rebar_deps(SrcDir) ->
    case rebar_deps(filename:join(SrcDir, "rebar.config")) ++
         rebar_deps(filename:join(SrcDir, "rebar.config.script")) of
        [] ->
            ok;
        Deps ->
            {ok, CurDir} = file:get_cwd(),
            file:set_cwd(SrcDir),
            filelib:ensure_dir(filename:join("deps", ".")),
            lists:foreach(fun({App, Cmd}) ->
                                  io:format("Fetching dependency ~s: ", [App]),
                                  Result = os:cmd("cd deps; " ++ Cmd ++ "; cd .."),
                                  io:format("~s", [Result])
                          end,
                          Deps),
            file:set_cwd(CurDir)
    end.


rebar_deps(Script) ->
    case file:script(Script) of
        {ok, Config} when is_list(Config) ->
            [ rebar_dep(Dep) || Dep <- proplists:get_value(deps, Config, []) ];
        {ok, {deps, Deps}} ->
            [ rebar_dep(Dep) || Dep <- Deps ];
        _ ->
            []
    end.


rebar_dep({App, Version, Git}) when Version /= ".*" ->
    AppS = atom_to_list(App),
    Help = os:cmd("mix hex.package"),
    case string:find(Help, "mix hex.package fetch") /= nomatch of
        true ->
            {App, "mix hex.package fetch " ++ AppS ++ " " ++ Version ++ " --unpack --output " ++ AppS};
        false ->
            io:format("I'll download ~p using git because I can't use Mix "
                      "to fetch from hex.pm:~n~s",
                      [AppS, Help]),
            rebar_dep({App, ".*", Git})
    end;

rebar_dep({App, _, {git, Url}}) ->
    {App, "git clone " ++ Url ++ " " ++ filename:basename(App)};
rebar_dep({App, _, {git, Url, {branch, Ref}}}) ->
    {App,
     "git clone -n " ++ Url ++ " " ++ filename:basename(App) ++
     "; (cd " ++ filename:basename(App) ++
     "; git checkout -q origin/" ++ Ref ++ ")"};
rebar_dep({App, _, {git, Url, {tag, Ref}}}) ->
    {App,
     "git clone -n " ++ Url ++ " " ++ filename:basename(App) ++
     "; (cd " ++ filename:basename(App) ++
     "; git checkout -q " ++ Ref ++ ")"};
rebar_dep({App, _, {git, Url, Ref}}) ->
    {App,
     "git clone -n " ++ Url ++ " " ++ filename:basename(App) ++
     "; (cd " ++ filename:basename(App) ++
     "; git checkout -q " ++ Ref ++ ")"}.


module_deps_dirs(Module) ->
    SrcDir = module_src_dir(Module),
    LibDir = module_lib_dir(Module),
    DepsDir = filename:join(LibDir, "deps"),
    Deps = rebar_deps(filename:join(SrcDir, "rebar.config")) ++
        rebar_deps(filename:join(SrcDir, "rebar.config.script")),
    [ filename:join(DepsDir, App) || {App, _Cmd} <- Deps ].


%% -- YAML spec parser


consult(File) ->
    case fast_yaml:decode_from_file(File, [plain_as_atom]) of
        {ok, []} -> {ok, []};
        {ok, [Doc | _]} -> {ok, [ format(Spec) || Spec <- Doc ]};
        {error, Err} -> {error, fast_yaml:format_error(Err)}
    end.


format({Key, Val}) when is_binary(Val) ->
    {Key, binary_to_list(Val)};
format({Key, Val}) ->  % TODO: improve Yaml parsing
    {Key, Val}.


%% -- COMMIT.json


maybe_write_commit_json(Url, RepDir) ->
    case (os:getenv("GITHUB_ACTIONS") == "true") of
        true ->
            ok;
        false ->
            write_commit_json(Url, RepDir)
    end.


write_commit_json(Url, RepDir) ->
    Url2 = string_replace(Url, "https://github.com", "https://api.github.com/repos"),
    BranchUrl = lists:flatten(Url2 ++ "/branches/master"),
    case geturl(BranchUrl) of
        {ok, _Headers, Body} ->
            {ok, F} = file:open(filename:join(RepDir, "COMMIT.json"), [raw, write]),
            file:write(F, Body),
            file:close(F);
        {error, Reason} ->
            Reason
    end.


find_commit_json(Attrs) ->
    FromPath = get_module_path(Attrs),
    case {find_commit_json_path(FromPath),
          find_commit_json_path(filename:join(FromPath, ".."))} of
        {{ok, FromFile}, _} ->
            FromFile;
        {_, {ok, FromFile}} ->
            FromFile;
        _ ->
            not_found
    end.


-ifdef(HAVE_URI_STRING).  %% Erlang/OTP 20 or higher can use this:


string_replace(Subject, Pattern, Replacement) ->
    string:replace(Subject, Pattern, Replacement).


find_commit_json_path(Path) ->
    filelib:find_file("COMMIT.json", Path).


-else.  % Workaround for Erlang/OTP older than 20:


string_replace(Subject, Pattern, Replacement) ->
    B = binary:replace(list_to_binary(Subject),
                       list_to_binary(Pattern),
                       list_to_binary(Replacement)),
    binary_to_list(B).


find_commit_json_path(Path) ->
    case filelib:wildcard("COMMIT.json", Path) of
        [] ->
            {error, commit_json_not_found};
        ["COMMIT.json"] = File ->
            {ok, filename:join(Path, File)}
    end.


-endif.


copy_commit_json(Package, Attrs) ->
    DestPath = module_lib_dir(Package),
    case find_commit_json(Attrs) of
        not_found ->
            ok;
        FromFile ->
            file:copy(FromFile, filename:join(DestPath, "COMMIT.json"))
    end.


get_commit_details(Dirname) ->
    RepDir = filename:join(sources_dir(), Dirname),
    get_commit_details2(filename:join(RepDir, "COMMIT.json")).


get_commit_details2(Path) ->
    case file:read_file(Path) of
        {ok, Body} ->
            parse_details(Body);
        _ ->
            #{
              sha => unknown_sha,
              date => <<>>,
              message => <<>>,
              html => <<>>,
              author_name => <<>>,
              commit_html_url => <<>>
             }
    end.


parse_details(Body) ->
    Contents = misc:json_decode(Body),

    {ok, Commit} = maps:find(<<"commit">>, Contents),
    {ok, Sha} = maps:find(<<"sha">>, Commit),
    {ok, CommitHtmlUrl} = maps:find(<<"html_url">>, Commit),

    {ok, Commit2} = maps:find(<<"commit">>, Commit),
    {ok, Message} = maps:find(<<"message">>, Commit2),
    {ok, Author} = maps:find(<<"author">>, Commit2),
    {ok, AuthorName} = maps:find(<<"name">>, Author),
    {ok, Committer} = maps:find(<<"committer">>, Commit2),
    {ok, Date} = maps:find(<<"date">>, Committer),

    {ok, Links} = maps:find(<<"_links">>, Contents),
    {ok, Html} = maps:find(<<"html">>, Links),

    #{
      sha => Sha,
      date => Date,
      message => Message,
      html => Html,
      author_name => AuthorName,
      commit_html_url => CommitHtmlUrl
     }.


%% -- Web Admin

-define(AXC(URL, Attributes, Text),
        ?XAE(<<"a">>, [{<<"href">>, URL} | Attributes], [?C(Text)])).

-define(INPUTCHECKED(Type, Name, Value),
        ?XA(<<"input">>,
            [{<<"type">>, Type},
             {<<"name">>, Name},
             {<<"disabled">>, <<"true">>},
             {<<"checked">>, <<"true">>},
             {<<"value">>, Value}])).

%% @format-begin


web_menu_node(Acc, _Node, _Lang) ->
    Acc ++
    [{<<"contrib">>, <<"Contrib Modules (Detailed)">>},
     {<<"contrib-api">>, <<"Contrib Modules (API)">>}].


web_page_node(_,
              Node,
              #request{
                path = [<<"contrib">>],
                q = Query,
                lang = Lang
               } =
                  R) ->
    Title =
        ?H1GL(<<"Contrib Modules (Detailed)">>,
              <<"../../developer/extending-ejabberd/modules/#ejabberd-contrib">>,
              <<"ejabberd-contrib">>),
    Res = [ejabberd_cluster:call(Node,
                                 ejabberd_web_admin,
                                 make_command,
                                 [webadmin_node_contrib,
                                  R,
                                  [{<<"node">>, Node}, {<<"query">>, Query}, {<<"lang">>, Lang}],
                                  []])],
    {stop, Title ++ Res};
web_page_node(_, Node, #request{path = [<<"contrib-api">> | RPath]} = R) ->
    Title =
        ?H1GL(<<"Contrib Modules (API)">>,
              <<"../../developer/extending-ejabberd/modules/#ejabberd-contrib">>,
              <<"ejabberd-contrib">>),
    _TableInstalled = make_table_installed(Node, R, RPath),
    _TableAvailable = make_table_available(Node, R, RPath),
    TableInstalled = make_table_installed(Node, R, RPath),
    TableAvailable = make_table_available(Node, R, RPath),
    Res = [?X(<<"hr">>),
           ?XAC(<<"h2">>, [{<<"id">>, <<"specs">>}], <<"Specs">>),
           ?XE(<<"blockquote">>,
               [ejabberd_cluster:call(Node,
                                      ejabberd_web_admin,
                                      make_command,
                                      [modules_update_specs, R])]),
           ?X(<<"hr">>),
           ?XAC(<<"h2">>, [{<<"id">>, <<"installed">>}], <<"Installed">>),
           ?XE(<<"blockquote">>,
               [ejabberd_cluster:call(Node,
                                      ejabberd_web_admin,
                                      make_command,
                                      [modules_installed, R, [], [{only, presentation}]]),
                ejabberd_cluster:call(Node,
                                      ejabberd_web_admin,
                                      make_command,
                                      [module_uninstall, R, [], [{only, presentation}]]),
                ejabberd_cluster:call(Node,
                                      ejabberd_web_admin,
                                      make_command,
                                      [module_upgrade, R, [], [{only, presentation}]]),
                TableInstalled]),
           ?X(<<"hr">>),
           ?XAC(<<"h2">>, [{<<"id">>, <<"available">>}], <<"Available">>),
           ?XE(<<"blockquote">>,
               [ejabberd_cluster:call(Node,
                                      ejabberd_web_admin,
                                      make_command,
                                      [modules_available, R, [], [{only, presentation}]]),
                ejabberd_cluster:call(Node,
                                      ejabberd_web_admin,
                                      make_command,
                                      [module_install, R, [], [{only, presentation}]]),
                TableAvailable,
                ejabberd_cluster:call(Node, ejabberd_web_admin, make_command, [module_check, R])])],
    {stop, Title ++ Res};
web_page_node(Acc, _, _) ->
    Acc.


make_table_installed(Node, R, RPath) ->
    Columns = [<<"Name">>, <<"Summary">>, <<"">>, <<"">>],
    ModulesInstalled =
        ejabberd_cluster:call(Node,
                              ejabberd_web_admin,
                              make_command_raw_value,
                              [modules_installed, R, []]),
    Rows =
        lists:map(fun({Name, Summary}) ->
                          NameBin = misc:atom_to_binary(Name),
                          Upgrade =
                              ejabberd_cluster:call(Node,
                                                    ejabberd_web_admin,
                                                    make_command,
                                                    [module_upgrade,
                                                     R,
                                                     [{<<"module">>, NameBin}],
                                                     [{only, button}, {input_name_append, [NameBin]}]]),
                          Uninstall =
                              ejabberd_cluster:call(Node,
                                                    ejabberd_web_admin,
                                                    make_command,
                                                    [module_uninstall,
                                                     R,
                                                     [{<<"module">>, NameBin}],
                                                     [{only, button},
                                                      {style, danger},
                                                      {input_name_append, [NameBin]}]]),
                          {?C(NameBin), ?C(list_to_binary(Summary)), Upgrade, Uninstall}
                  end,
                  ModulesInstalled),
    ejabberd_web_admin:make_table(200, RPath, Columns, Rows).


make_table_available(Node, R, RPath) ->
    Columns = [<<"Name">>, <<"Summary">>, <<"">>],
    ModulesAll =
        ejabberd_cluster:call(Node,
                              ejabberd_web_admin,
                              make_command_raw_value,
                              [modules_available, R, []]),
    ModulesInstalled =
        ejabberd_cluster:call(Node,
                              ejabberd_web_admin,
                              make_command_raw_value,
                              [modules_installed, R, []]),
    ModulesNotInstalled =
        lists:filter(fun({Mod, _}) -> not lists:keymember(Mod, 1, ModulesInstalled) end,
                     ModulesAll),
    Rows =
        lists:map(fun({Name, Summary}) ->
                          NameBin = misc:atom_to_binary(Name),
                          Install =
                              ejabberd_cluster:call(Node,
                                                    ejabberd_web_admin,
                                                    make_command,
                                                    [module_install,
                                                     R,
                                                     [{<<"module">>, NameBin}],
                                                     [{only, button}, {input_name_append, [NameBin]}]]),
                          {?C(NameBin), ?C(list_to_binary(Summary)), Install}
                  end,
                  ModulesNotInstalled),
    ejabberd_web_admin:make_table(200, RPath, Columns, Rows).


webadmin_node_contrib(Node, Query, Lang) ->
    QueryRes = list_modules_parse_query(Query),
    Contents = get_content(Node, Query, Lang),
    Result =
        case QueryRes of
            ok ->
                [?XREST(?T("Submitted"))];
            nothing ->
                []
        end,
    Result ++ Contents.
%% @format-end


get_module_home(Module, Attrs) ->
    case get_module_information(home, Attrs) of
        "https://github.com/processone/ejabberd-contrib/tree/master/" = P1 ->
            P1 ++ atom_to_list(Module);
        Other ->
            Other
    end.


get_module_summary(Attrs) ->
    get_module_information(summary, Attrs).


get_module_author(Attrs) ->
    get_module_information(author, Attrs).


get_module_path(Attrs) ->
    get_module_information(path, Attrs).


get_module_information(Attribute, Attrs) ->
    case lists:keyfind(Attribute, 1, Attrs) of
        false -> "";
        {_, Value} -> Value
    end.


get_installed_module_el({ModAtom, Attrs}, Lang) ->
    Mod = misc:atom_to_binary(ModAtom),
    Home = list_to_binary(get_module_home(ModAtom, Attrs)),
    Summary = list_to_binary(get_module_summary(Attrs)),
    Author = list_to_binary(get_module_author(Attrs)),
    FromPath = get_module_path(Attrs),
    FromFile = case find_commit_json_path(FromPath) of
                   {ok, FF} -> FF;
                   {error, _} -> "dummypath"
               end,
    #{
      sha := CommitSha,
      date := CommitDate,
      message := CommitMessage,
      author_name := CommitAuthorName,
      commit_html_url := CommitHtmlUrl
     } = get_commit_details2(FromFile),

    [SourceSpec] = [ S || {M, S} <- available(), M == ModAtom ],
    SourceFile = find_commit_json(SourceSpec),
    #{
      sha := SourceSha,
      date := SourceDate,
      message := SourceMessage,
      author_name := SourceAuthorName,
      commit_html_url := SourceHtmlUrl
     } = get_commit_details2(SourceFile),

    UpgradeEls =
        case CommitSha == SourceSha of
            true ->
                [];
            false ->
                SourceTitleEl = make_title_el(SourceDate, SourceMessage, SourceAuthorName),
                [?XE(<<"td">>,
                     [?INPUT(<<"checkbox">>, <<"selected_upgrade">>, Mod),
                      ?C(<<" ">>),
                      ?AXC(SourceHtmlUrl, [SourceTitleEl], binary:part(SourceSha, {0, 8}))])]
        end,

    Started =
        case gen_mod:is_loaded(hd(ejabberd_option:hosts()), ModAtom) of
            false ->
                [?C(<<" ">>)];
            true ->
                []
        end,
    TitleEl = make_title_el(CommitDate, CommitMessage, CommitAuthorName),
    Status = get_module_status_el(ModAtom),
    HomeTitleEl = make_home_title_el(Summary, Author),
    ?XE(<<"tr">>,
        [?XE(<<"td">>, [?AXC(Home, [HomeTitleEl], Mod)]),
         ?XE(<<"td">>,
             [?INPUTTD(<<"checkbox">>, <<"selected_uninstall">>, Mod),
              ?C(<<" ">>),
              get_commit_link(CommitHtmlUrl, TitleEl, CommitSha),
              ?C(<<" - ">>)] ++
             Started ++
             Status) | UpgradeEls]).


get_module_status_el(ModAtom) ->
    case {get_module_status(ModAtom),
          get_module_status(elixir_module_name(ModAtom))} of
        {Str, unknown} when is_list(Str) ->
            [?C(<<" ">>), ?C(Str)];
        {unknown, Str} when is_list(Str) ->
            [?C(<<" ">>), ?C(Str)];
        {unknown, unknown} ->
            []
    end.


get_module_status(Module) ->
    try Module:mod_status() of
        Str when is_list(Str) ->
            Str
    catch
        _:_ ->
            unknown
    end.


%% When a module named mod_whatever in ejabberd-modules
%% is written in Elixir, its runtime name is 'Elixir.ModWhatever'
get_runtime_module_name(Module) ->
    case is_elixir_module(Module) of
        true -> elixir_module_name(Module);
        false -> Module
    end.


is_elixir_module(Module) ->
    LibDir = module_src_dir(Module),
    Lib = filename:join(LibDir, "lib"),
    Src = filename:join(LibDir, "src"),
    case {filelib:wildcard(Lib ++ "/*.{ex}"),
          filelib:wildcard(Src ++ "/*.{erl}")} of
        {[_ | _], []} ->
            true;
        {[], _} ->
            false
    end.


%% Converts mod_some_thing to Elixir.ModSomeThing
elixir_module_name(ModAtom) ->
    list_to_atom("Elixir." ++ elixir_module_name("_" ++ atom_to_list(ModAtom), [])).


elixir_module_name([], Res) ->
    lists:reverse(Res);
elixir_module_name([$_, Char | Remaining], Res) ->
    [Upper] = uppercase([Char]),
    elixir_module_name(Remaining, [Upper | Res]);
elixir_module_name([Char | Remaining], Res) ->
    elixir_module_name(Remaining, [Char | Res]).


-ifdef(HAVE_URI_STRING).


uppercase(String) ->
    string:uppercase(String).  % OTP 20 or higher


-else.


uppercase(String) ->
    string:to_upper(String).  % OTP older than 20


-endif.


get_available_module_el({ModAtom, Attrs}) ->
    Installed = installed(),
    Mod = misc:atom_to_binary(ModAtom),
    Home = list_to_binary(get_module_home(ModAtom, Attrs)),
    Summary = list_to_binary(get_module_summary(Attrs)),
    Author = list_to_binary(get_module_author(Attrs)),
    HomeTitleEl = make_home_title_el(Summary, Author),
    InstallCheckbox =
        case lists:keymember(ModAtom, 1, Installed) of
            false -> [?INPUT(<<"checkbox">>, <<"selected_install">>, Mod)];
            true -> [?INPUTCHECKED(<<"checkbox">>, <<"selected_install">>, Mod)]
        end,
    ?XE(<<"tr">>,
        [?XE(<<"td">>, InstallCheckbox ++ [?C(<<" ">>), ?AXC(Home, [HomeTitleEl], Mod)]),
         ?XE(<<"td">>, [?C(Summary)])]).


get_installed_modules_table(Lang) ->
    Modules = installed(),
    Tail = [?XE(<<"tr">>,
                [?XE(<<"td">>, []),
                 ?XE(<<"td">>,
                     [?INPUTTD(<<"submit">>, <<"uninstall">>, ?T("Uninstall"))]),
                 ?XE(<<"td">>,
                     [?INPUTT(<<"submit">>, <<"upgrade">>, ?T("Upgrade"))])])],
    TBody = [ get_installed_module_el(Module, Lang) || Module <- lists:sort(Modules) ],
    ?XAE(<<"table">>,
         [],
         [?XE(<<"tbody">>, TBody ++ Tail)]).


get_available_modules_table(Lang) ->
    Modules = get_available_notinstalled(),
    Tail = [?XE(<<"tr">>,
                [?XE(<<"td">>,
                     [?INPUTT(<<"submit">>, <<"install">>, ?T("Install"))])])],
    TBody = [ get_available_module_el(Module) || Module <- lists:sort(Modules) ],
    ?XAE(<<"table">>,
         [],
         [?XE(<<"tbody">>, TBody ++ Tail)]).


make_title_el(Date, Message, AuthorName) ->
    LinkTitle = <<Message/binary, "\n", AuthorName/binary, "\n", Date/binary>>,
    {<<"title">>, LinkTitle}.


make_home_title_el(Summary, Author) ->
    LinkTitle = <<Summary/binary, "\n", Author/binary>>,
    {<<"title">>, LinkTitle}.


get_commit_link(_CommitHtmlUrl, _TitleErl, unknown_sha) ->
    ?C(<<"Please Update Specs">>);
get_commit_link(CommitHtmlUrl, TitleEl, CommitSha) ->
    ?AXC(CommitHtmlUrl, [TitleEl], binary:part(CommitSha, {0, 8})).


get_content(Node, Query, Lang) ->
    {{_CommandCtl}, _Res} =
        case catch parse_and_execute(Query, Node) of
            {'EXIT', _} -> {{""}, <<"">>};
            Result_tuple -> Result_tuple
        end,

    AvailableModulesEls = get_available_modules_table(Lang),
    InstalledModulesEls = get_installed_modules_table(Lang),

    Sources = get_sources_list(),
    SourceEls = (?XAE(<<"table">>,
                      [],
                      [?XE(<<"tbody">>,
                           (lists:map(
                              fun(Dirname) ->
                                      #{
                                        sha := CommitSha,
                                        date := CommitDate,
                                        message := CommitMessage,
                                        html := Html,
                                        author_name := AuthorName,
                                        commit_html_url := CommitHtmlUrl
                                       } = get_commit_details(Dirname),
                                      TitleEl = make_title_el(CommitDate, CommitMessage, AuthorName),
                                      ?XE(<<"tr">>,
                                          [?XE(<<"td">>, [?AC(Html, Dirname)]),
                                           ?XE(<<"td">>,
                                               [get_commit_link(CommitHtmlUrl, TitleEl, CommitSha)]),
                                           ?XE(<<"td">>, [?C(CommitMessage)])])
                              end,
                              lists:sort(Sources))))])),

    [?XC(<<"p">>,
         translate:translate(
           Lang, ?T("Update specs to get modules source, then install desired ones."))),
     ?XAE(<<"form">>,
          [{<<"method">>, <<"post">>}],
          [?XCT(<<"h3">>, ?T("Sources Specs:")),
           SourceEls,
           ?BR,
           ?INPUTT(<<"submit">>,
                   <<"updatespecs">>,
                   translate:translate(Lang, ?T("Update Specs"))),

           ?XCT(<<"h3">>, ?T("Installed Modules:")),
           InstalledModulesEls,
           ?BR,

           ?XCT(<<"h3">>, ?T("Other Modules Available:")),
           AvailableModulesEls])].


get_sources_list() ->
    case file:list_dir(sources_dir()) of
        {ok, Filenames} -> Filenames;
        {error, enoent} -> []
    end.


get_available_notinstalled() ->
    Installed = installed(),
    lists:filter(
      fun({Mod, _}) ->
              not lists:keymember(Mod, 1, Installed)
      end,
      available()).


parse_and_execute(Query, Node) ->
    {[Exec], _} = lists:partition(
                    fun(ExType) ->
                            lists:keymember(ExType, 1, Query)
                    end,
                    [<<"updatespecs">>]),
    Commands = {get_val(<<"updatespecs">>, Query)},
    {_, R} = parse1_command(Exec, Commands, Node),
    {Commands, R}.


get_val(Val, Query) ->
    {value, {_, R}} = lists:keysearch(Val, 1, Query),
    binary_to_list(R).


parse1_command(<<"updatespecs">>, {_}, _Node) ->
    Res = update(),
    {oook, io_lib:format("~p", [Res])}.


list_modules_parse_query(Query) ->
    case {lists:keysearch(<<"install">>, 1, Query),
          lists:keysearch(<<"upgrade">>, 1, Query),
          lists:keysearch(<<"uninstall">>, 1, Query)} of
        {{value, _}, _, _} -> list_modules_parse_install(Query);
        {_, {value, _}, _} -> list_modules_parse_upgrade(Query);
        {_, _, {value, _}} -> list_modules_parse_uninstall(Query);
        _ -> nothing
    end.


list_modules_parse_install(Query) ->
    lists:foreach(
      fun({Mod, _}) ->
              ModBin = misc:atom_to_binary(Mod),
              case lists:member({<<"selected_install">>, ModBin}, Query) of
                  true -> install(Mod);
                  _ -> ok
              end
      end,
      get_available_notinstalled()),
    ok.


list_modules_parse_upgrade(Query) ->
    lists:foreach(
      fun({Mod, _}) ->
              ModBin = misc:atom_to_binary(Mod),
              case lists:member({<<"selected_upgrade">>, ModBin}, Query) of
                  true -> upgrade(Mod);
                  _ -> ok
              end
      end,
      installed()),
    ok.


list_modules_parse_uninstall(Query) ->
    lists:foreach(
      fun({Mod, _}) ->
              ModBin = misc:atom_to_binary(Mod),
              case lists:member({<<"selected_uninstall">>, ModBin}, Query) of
                  true -> uninstall(Mod);
                  _ -> ok
              end
      end,
      installed()),
    ok.


install_contrib_modules(Modules, Config) ->
    lists:filter(fun(Module) ->
                         case install(misc:atom_to_binary(Module), Config) of
                             {error, conflict} ->
                                 false;
                             ok ->
                                 true
                         end
                 end,
                 Modules).
