%%%----------------------------------------------------------------------
%%% File    : ejabberd_app.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd's application callback module
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-include("ejabberd.hrl").
-include("logger.hrl").

%%%
%%% Application API
%%%

start(normal, _Args) ->
    {T1, _} = statistics(wall_clock),
    ejabberd_logger:start(),
    write_pid_file(),
    start_apps(),
    start_elixir_application(),
    ejabberd:check_app(ejabberd),
    setup_if_elixir_conf_used(),
    ejabberd_config:start(),
    ejabberd_mnesia:start(),
    file_queue_init(),
    maybe_add_nameservers(),
    case ejabberd_sup:start_link() of
	{ok, SupPid} ->
	    register_elixir_config_hooks(),
	    ejabberd_cluster:wait_for_sync(infinity),
	    {T2, _} = statistics(wall_clock),
	    ?INFO_MSG("ejabberd ~s is started in the node ~p in ~.2fs",
		      [?VERSION, node(), (T2-T1)/1000]),
	    lists:foreach(fun erlang:garbage_collect/1, processes()),
	    {ok, SupPid};
	Err ->
	    Err
    end;
start(_, _) ->
    {error, badarg}.

%% Prepare the application for termination.
%% This function is called when an application is about to be stopped,
%% before shutting down the processes of the application.
prep_stop(State) ->
    ejabberd_listener:stop_listeners(),
    ejabberd_sm:stop(),
    gen_mod:stop_modules(),
    State.

%% All the processes were killed when this function is called
stop(_State) ->
    ?INFO_MSG("ejabberd ~s is stopped in the node ~p", [?VERSION, node()]),
    delete_pid_file(),
    %%ejabberd_debug:stop(),
    ok.


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
    case file:open(PidFilename, [write]) of
	{ok, Fd} ->
	    io:format(Fd, "~s~n", [Pid]),
	    file:close(Fd);
	{error, Reason} ->
	    ?ERROR_MSG("Cannot write PID file ~s~nReason: ~p", [PidFilename, Reason]),
	    throw({cannot_write_pid_file, PidFilename, Reason})
    end.

delete_pid_file() ->
    case ejabberd:get_pid_file() of
	false ->
	    ok;
	PidFilename ->
	    file:delete(PidFilename)
    end.

file_queue_init() ->
    QueueDir = case ejabberd_config:queue_dir() of
		   undefined ->
		       MnesiaDir = mnesia:system_info(directory),
		       filename:join(MnesiaDir, "queue");
		   Path ->
		       Path
	       end,
    p1_queue:start(QueueDir).

start_apps() ->
    crypto:start(),
    ejabberd:start_app(sasl),
    ejabberd:start_app(ssl),
    ejabberd:start_app(p1_utils),
    ejabberd:start_app(fast_yaml),
    ejabberd:start_app(fast_tls),
    ejabberd:start_app(xmpp),
    ejabberd:start_app(cache_tab),
    start_eimp().

setup_if_elixir_conf_used() ->
  case ejabberd_config:is_using_elixir_config() of
    true -> 'Elixir.Ejabberd.Config.Store':start_link();
    false -> ok
  end.

register_elixir_config_hooks() ->
  case ejabberd_config:is_using_elixir_config() of
    true -> 'Elixir.Ejabberd.Config':start_hooks();
    false -> ok
  end.

start_elixir_application() ->
    case ejabberd_config:is_elixir_enabled() of
	true ->
	    case application:ensure_started(elixir) of
		ok -> ok;
		{error, _Msg} -> ?ERROR_MSG("Elixir application not started.", [])
	    end;
	_ ->
	    ok
    end.

-ifdef(GRAPHICS).
start_eimp() ->
    ejabberd:start_app(eimp).
-else.
start_eimp() ->
    ok.
-endif.
