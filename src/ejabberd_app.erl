%%%----------------------------------------------------------------------
%%% File    : ejabberd_app.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd's application callback module
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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

-module(ejabberd_app).

-author('alexey@process-one.net').

-behaviour(application).

-export([start/2, prep_stop/1, stop/1]).

-include("logger.hrl").


%%%
%%% Application API
%%%

start(normal, _Args) ->
    try
	{T1, _} = statistics(wall_clock),
	ejabberd_logger:start(),
	write_pid_file(),
	start_included_apps(),
	misc:warn_unset_home(),
	start_elixir_application(),
	setup_if_elixir_conf_used(),
	case ejabberd_config:load() of
	    ok ->
		ejabberd_mnesia:start(),
		delete_unused_tables(),
		file_queue_init(),
		maybe_add_nameservers(),
		case ejabberd_sup:start_link() of
		    {ok, SupPid} ->
			ejabberd_system_monitor:start(),
			register_elixir_config_hooks(),
			ejabberd_cluster:wait_for_sync(infinity),
			ejabberd_hooks:run(ejabberd_started, []),
			ejabberd:check_apps(),
			ejabberd_systemd:ready(),
			maybe_start_exsync(),
			{T2, _} = statistics(wall_clock),
			?INFO_MSG("ejabberd ~ts is started in the node ~p in ~.2fs",
				  [ejabberd_option:version(),
				   node(), (T2-T1)/1000]),
			maybe_print_elixir_version(),
			?INFO_MSG("~ts",
				  [erlang:system_info(system_version)]),
			print_distribution_listening(),
			{ok, SupPid};
		    Err ->
			?CRITICAL_MSG("Failed to start ejabberd application: ~p", [Err]),
			ejabberd:halt()
		end;
	    Err ->
		?CRITICAL_MSG("Failed to start ejabberd application: ~ts",
			      [ejabberd_config:format_error(Err)]),
		ejabberd:halt()
	end
    catch throw:{?MODULE, Error} ->
	    ?DEBUG("Failed to start ejabberd application: ~p", [Error]),
	    ejabberd:halt()
    end;
start(_, _) ->
    {error, badarg}.

start_included_apps() ->
    {ok, Apps} = application:get_key(ejabberd, included_applications),
    lists:foreach(
	fun(mnesia) ->
	       ok;
	   (lager) ->
		ok;
	   (os_mon)->
	       ok;
	   (App) ->
	       application:ensure_all_started(App)
	end, Apps).

%% Prepare the application for termination.
%% This function is called when an application is about to be stopped,
%% before shutting down the processes of the application.
prep_stop(State) ->
    ejabberd_systemd:stopping(),
    ejabberd_hooks:run(ejabberd_stopping, []),
    ejabberd_listener:stop(),
    ejabberd_sm:stop(),
    ejabberd_service:stop(),
    ejabberd_s2s:stop(),
    ejabberd_system_monitor:stop(),
    gen_mod:prep_stop(),
    gen_mod:stop(),
    State.

%% All the processes were killed when this function is called
stop(_State) ->
    ?INFO_MSG("ejabberd ~ts is stopped in the node ~p",
	      [ejabberd_option:version(), node()]),
    delete_pid_file().

%%%
%%% Internal functions
%%%

%% If ejabberd is running on some Windows machine, get nameservers and add to Erlang
maybe_add_nameservers() ->
    case os:type() of
	{win32, _} -> add_windows_nameservers();
	_ -> ok
    end.

add_windows_nameservers() ->
    IPTs = win32_dns:get_nameservers(),
    ?INFO_MSG("Adding machine's DNS IPs to Erlang system:~n~p", [IPTs]),
    lists:foreach(fun(IPT) -> inet_db:add_ns(IPT) end, IPTs).

%%%
%%% PID file
%%%

write_pid_file() ->
    case ejabberd:get_pid_file() of
	false ->
	    ok;
	PidFilename ->
	    write_pid_file(os:getpid(), PidFilename)
    end.

write_pid_file(Pid, PidFilename) ->
    case file:write_file(PidFilename, io_lib:format("~ts~n", [Pid])) of
	ok ->
	    ok;
	{error, Reason} = Err ->
	    ?CRITICAL_MSG("Cannot write PID file ~ts: ~ts",
			  [PidFilename, file:format_error(Reason)]),
	    throw({?MODULE, Err})
    end.

delete_pid_file() ->
    case ejabberd:get_pid_file() of
	false ->
	    ok;
	PidFilename ->
	    file:delete(PidFilename)
    end.

delete_unused_tables() ->
    mnesia:delete_table(muc_occupant_id).

file_queue_init() ->
    QueueDir = case ejabberd_option:queue_dir() of
		   undefined ->
		       MnesiaDir = mnesia:system_info(directory),
		       filename:join(MnesiaDir, "queue");
		   Path ->
		       Path
	       end,
    case p1_queue:start(QueueDir) of
	ok -> ok;
	Err -> throw({?MODULE, Err})
    end.

%%%
%%% Elixir
%%%

-ifdef(ELIXIR_ENABLED).
is_using_elixir_config() ->
    Config = ejabberd_config:path(),
    try 'Elixir.Ejabberd.ConfigUtil':is_elixir_config(Config) of
        B when is_boolean(B) -> B
    catch
        _:_ -> false
    end.

setup_if_elixir_conf_used() ->
  case is_using_elixir_config() of
    true -> 'Elixir.Ejabberd.Config.Store':start_link();
    false -> ok
  end.

register_elixir_config_hooks() ->
  case is_using_elixir_config() of
    true -> 'Elixir.Ejabberd.Config':start_hooks();
    false -> ok
  end.

start_elixir_application() ->
    case application:ensure_started(elixir) of
	ok -> ok;
	{error, _Msg} -> ?ERROR_MSG("Elixir application not started.", [])
    end.

maybe_start_exsync() ->
    case os:getenv("RELIVE") of
        "true" -> rpc:call(node(), 'Elixir.ExSync.Application', start, []);
        _ -> ok
    end.

maybe_print_elixir_version() ->
    ?INFO_MSG("Elixir ~ts", [maps:get(build, 'Elixir.System':build_info())]).
-else.
setup_if_elixir_conf_used() -> ok.
register_elixir_config_hooks() -> ok.
start_elixir_application() -> ok.
maybe_start_exsync() -> ok.
maybe_print_elixir_version() -> ok.
-endif.

print_distribution_listening() ->
    Links = case erlang:whereis(net_kernel) of
        undefined ->
            [];
        P ->
            {links, L} = erlang:process_info(P, links),
            L
    end,
    {Addr, Port} = lists:foldl(
          fun(Link, Acc) ->
                  case catch inet:sockname(Link) of
                      {ok, {A1, P1}} -> {misc:ip_to_list(A1), P1};
                      _ -> Acc
                  end
          end, {"UnknownAddress", "UnknownPort"}, Links),
    ?INFO_MSG("Start accepting TCP connections at ~ts:~p for erlang distribution", [Addr, Port]).
