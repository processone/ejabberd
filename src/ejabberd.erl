%%%----------------------------------------------------------------------
%%% File    : ejabberd.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd wrapper: start / stop
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd).
-author('alexey@process-one.net').

-protocol({xep, 4, '2.9'}).
-protocol({xep, 86, '1.0'}).
-protocol({xep, 106, '1.1'}).
-protocol({xep, 170, '1.0'}).
-protocol({xep, 205, '1.0'}).
-protocol({xep, 212, '1.0'}).
-protocol({xep, 216, '1.0'}).
-protocol({xep, 243, '1.0'}).
-protocol({xep, 270, '1.0'}).

-export([start/0, stop/0, start_app/1, start_app/2,
	 get_pid_file/0, check_app/1, module_name/1]).

-include("logger.hrl").

start() ->
    %%ejabberd_cover:start(),
    application:start(ejabberd).

stop() ->
    application:stop(ejabberd).
    %%ejabberd_cover:stop().

%% @spec () -> false | string()
get_pid_file() ->
    case os:getenv("EJABBERD_PID_PATH") of
	false ->
	    false;
	"" ->
	    false;
	Path ->
	    Path
    end.

start_app(App) ->
    start_app(App, temporary).

start_app(App, Type) ->
    StartFlag = not is_loaded(),
    start_app(App, Type, StartFlag).

check_app(App) ->
    StartFlag = not is_loaded(),
    spawn(fun() -> check_app_modules(App, StartFlag) end),
    ok.

is_loaded() ->
    Apps = application:which_applications(),
    lists:keymember(ejabberd, 1, Apps).

start_app(App, Type, StartFlag) when not is_list(App) ->
    start_app([App], Type, StartFlag);
start_app([App|Apps], Type, StartFlag) ->
    case application:start(App,Type) of
        ok ->
            spawn(fun() -> check_app_modules(App, StartFlag) end),
            start_app(Apps, Type, StartFlag);
        {error, {already_started, _}} ->
            start_app(Apps, Type, StartFlag);
        {error, {not_started, DepApp}} ->
            case lists:member(DepApp, [App|Apps]) of
                true ->
                    Reason = io_lib:format(
                               "failed to start application '~p': "
                               "circular dependency on '~p' detected",
                               [App, DepApp]),
                    exit_or_halt(Reason, StartFlag);
                false ->
                    start_app([DepApp,App|Apps], Type, StartFlag)
            end;
        Err ->
            Reason = io_lib:format("failed to start application '~p': ~p",
                                   [App, Err]),
            exit_or_halt(Reason, StartFlag)
    end;
start_app([], _Type, _StartFlag) ->
    ok.

check_app_modules(App, StartFlag) ->
    sleep(5000),
    case application:get_key(App, modules) of
        {ok, Mods} ->
            lists:foreach(
              fun(Mod) ->
                      case code:which(Mod) of
                          non_existing ->
                              File = get_module_file(App, Mod),
                              Reason = io_lib:format(
                                         "couldn't find module ~s "
                                         "needed for application '~p'",
                                         [File, App]),
                              exit_or_halt(Reason, StartFlag);
                          _ ->
                              sleep(10)
                      end
              end, Mods);
        _ ->
            %% No modules? This is strange
            ok
    end.

exit_or_halt(Reason, StartFlag) ->
    ?CRITICAL_MSG(Reason, []),
    if StartFlag ->
            %% Wait for the critical message is written in the console/log
            timer:sleep(1000),
            halt(string:substr(lists:flatten(Reason), 1, 199));
       true ->
            erlang:error(application_start_failed)
    end.

sleep(N) ->
    timer:sleep(randoms:uniform(N)).

get_module_file(App, Mod) ->
    BaseName = atom_to_list(Mod),
    case code:lib_dir(App, ebin) of
        {error, _} ->
            BaseName;
        Dir ->
            filename:join([Dir, BaseName ++ ".beam"])
    end.

module_name([Dir, _, <<H,_/binary>> | _] = Mod) when H >= 65, H =< 90 ->
    Module = str:join([elixir_name(M) || M<-tl(Mod)], <<>>),
    Prefix = case elixir_name(Dir) of
	<<"Ejabberd">> -> <<"Elixir.Ejabberd.">>;
	Lib -> <<"Elixir.Ejabberd.", Lib/binary, ".">>
    end,
    misc:binary_to_atom(<<Prefix/binary, Module/binary>>);
module_name([<<"ejabberd">> | _] = Mod) ->
    Module = str:join([erlang_name(M) || M<-Mod], $_),
    misc:binary_to_atom(Module);
module_name(Mod) when is_list(Mod) ->
    Module = str:join([erlang_name(M) || M<-tl(Mod)], $_),
    misc:binary_to_atom(Module).

elixir_name(Atom) when is_atom(Atom) ->
    elixir_name(misc:atom_to_binary(Atom));
elixir_name(<<H,T/binary>>) when H >= 65, H =< 90 ->
    <<H, T/binary>>;
elixir_name(<<H,T/binary>>) ->
    <<(H-32), T/binary>>.

erlang_name(Atom) when is_atom(Atom) ->
    misc:atom_to_binary(Atom);
erlang_name(Bin) when is_binary(Bin) ->
    Bin.
