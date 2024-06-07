%%%----------------------------------------------------------------------
%%% File    : ejabberd.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd wrapper: start / stop
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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
-compile({no_auto_import, [{halt, 0}]}).

-protocol({rfc, 6122}).
-protocol({rfc, 7590}).
-protocol({xep, 4, '2.9'}).
-protocol({xep, 59, '1.0'}).
-protocol({xep, 86, '1.0'}).
-protocol({xep, 106, '1.1'}).
-protocol({xep, 170, '1.0'}).
-protocol({xep, 205, '1.0'}).
-protocol({xep, 212, '1.0'}).
-protocol({xep, 216, '1.0'}).
-protocol({xep, 243, '1.0'}).
-protocol({xep, 270, '1.0'}).
-protocol({xep, 368, '1.1.0'}).
-protocol({xep, 386, '0.3.0', '24.02', "", ""}).
-protocol({xep, 388, '0.4.0', '24.02', "", ""}).
-protocol({xep, 424, '0.4.0', '24.02', "", ""}).
-protocol({xep, 440, '0.4.0', '24.02', "", ""}).
-protocol({xep, 474, '0.3.0', '24.02', "", ""}).
-protocol({xep, 485, '0.2.0', '24.02', "", "mod_pubsub_serverinfo in ejabberd-contrib.git"}).

-export([start/0, stop/0, halt/0, start_app/1, start_app/2,
	 get_pid_file/0, check_apps/0, module_name/1, is_loaded/0]).

-include("logger.hrl").

start() ->
    application:ensure_all_started(ejabberd).

stop() ->
    application:stop(ejabberd).

halt() ->
    ejabberd_logger:flush(),
    erlang:halt(1, [{flush, true}]).

-spec get_pid_file() -> false | string().
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

is_loaded() ->
    Apps = application:which_applications(),
    lists:keymember(ejabberd, 1, Apps).

start_app(App, Type, StartFlag) when is_atom(App) ->
    start_app([App], Type, StartFlag);
start_app([App|Apps], Type, StartFlag) ->
    case application:start(App,Type) of
        ok ->
            start_app(Apps, Type, StartFlag);
        {error, {already_started, _}} ->
            start_app(Apps, Type, StartFlag);
        {error, {not_started, DepApp}} ->
            case lists:member(DepApp, [App|Apps]) of
                true ->
                    Reason = io_lib:format(
                               "Failed to start Erlang application '~ts': "
                               "circular dependency with '~ts' detected",
                               [App, DepApp]),
                    exit_or_halt(Reason, StartFlag);
                false ->
                    start_app([DepApp,App|Apps], Type, StartFlag)
            end;
        {error, Why} ->
            Reason = io_lib:format(
		       "Failed to start Erlang application '~ts': ~ts. ~ts",
		       [App, format_error(Why), hint()]),
            exit_or_halt(Reason, StartFlag)
    end;
start_app([], _Type, _StartFlag) ->
    ok.

check_app_modules(App, StartFlag) ->
    case application:get_key(App, modules) of
        {ok, Mods} ->
            lists:foreach(
              fun(Mod) ->
                      case code:which(Mod) of
                          non_existing ->
                              File = get_module_file(App, Mod),
                              Reason = io_lib:format(
                                         "Couldn't find file ~ts needed "
					 "for Erlang application '~ts'. ~ts",
                                         [File, App, hint()]),
                              exit_or_halt(Reason, StartFlag);
                          _ ->
			      ok
                      end
              end, Mods);
        _ ->
            %% No modules? This is strange
            ok
    end.

check_apps() ->
    spawn(
      fun() ->
	      Apps = [ejabberd |
		      [App || {App, _, _} <- application:which_applications(),
			      App /= ejabberd, App /= hex]],
	      ?DEBUG("Checking consistency of applications: ~ts",
		     [misc:join_atoms(Apps, <<", ">>)]),
	      misc:peach(
		fun(App) ->
			check_app_modules(App, true)
		end, Apps),
	      ?DEBUG("All applications are intact", []),
	      lists:foreach(fun erlang:garbage_collect/1, processes())
      end).

-spec exit_or_halt(iodata(), boolean()) -> no_return().
exit_or_halt(Reason, StartFlag) ->
    ?CRITICAL_MSG(Reason, []),
    if StartFlag ->
            %% Wait for the critical message is written in the console/log
            halt();
       true ->
            erlang:error(application_start_failed)
    end.

get_module_file(App, Mod) ->
    BaseName = atom_to_list(Mod),
    case code:lib_dir(App) of
        {error, _} ->
            BaseName;
        Dir ->
            filename:join([Dir, "ebin", BaseName ++ ".beam"])
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

format_error({Reason, File}) when is_list(Reason), is_list(File) ->
    Reason ++ ": " ++ File;
format_error(Term) ->
    io_lib:format("~p", [Term]).

hint() ->
    "This usually means that ejabberd or Erlang "
    "was compiled/installed incorrectly.".
