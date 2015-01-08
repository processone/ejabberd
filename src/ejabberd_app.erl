%%%----------------------------------------------------------------------
%%% File    : ejabberd_app.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd's application callback module
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_app).
-author('alexey@process-one.net').

-behaviour(application).

-export([start_modules/0,start/2, prep_stop/1, stop/1, init/0]).

-include("ejabberd.hrl").
-include("logger.hrl").

%%%
%%% Application API
%%%

start(normal, _Args) ->
    ejabberd_logger:start(),
    write_pid_file(),
    start_apps(),
    ejabberd:check_app(ejabberd),
    randoms:start(),
    db_init(),
    start(),
    translate:start(),
    ejabberd_ctl:init(),
    ejabberd_commands:init(),
    ejabberd_admin:start(),
    gen_mod:start(),
    ejabberd_config:start(),
    set_loglevel_from_config(),
    acl:start(),
    shaper:start(),
    connect_nodes(),
    Sup = ejabberd_sup:start_link(),
    ejabberd_rdbms:start(),
    ejabberd_riak_sup:start(),
    ejabberd_auth:start(),
    cyrsasl:start(),
    % Profiling
    %ejabberd_debug:eprof_start(),
    %ejabberd_debug:fprof_start(),
    maybe_add_nameservers(),
    start_modules(),
    ejabberd_listener:start_listeners(),
    ?INFO_MSG("ejabberd ~s is started in the node ~p", [?VERSION, node()]),
    Sup;
start(_, _) ->
    {error, badarg}.

%% Prepare the application for termination.
%% This function is called when an application is about to be stopped,
%% before shutting down the processes of the application.
prep_stop(State) ->
    ejabberd_listener:stop_listeners(),
    stop_modules(),
    ejabberd_admin:stop(),
    broadcast_c2s_shutdown(),
    timer:sleep(5000),
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

start() ->
    spawn_link(?MODULE, init, []).

init() ->
    register(ejabberd, self()),
    loop().

loop() ->
    receive
	_ ->
	    loop()
    end.

db_init() ->
    MyNode = node(),
    DbNodes = mnesia:system_info(db_nodes),
    case lists:member(MyNode, DbNodes) of
	true ->
	    ok;
	false ->
	    ?CRITICAL_MSG("Node name mismatch: I'm [~s], "
			  "the database is owned by ~p", [MyNode, DbNodes]),
	    ?CRITICAL_MSG("Either set ERLANG_NODE in ejabberdctl.cfg "
			  "or change node name in Mnesia", []),
	    erlang:error(node_name_mismatch)
    end,
    case mnesia:system_info(extra_db_nodes) of
	[] ->
	    mnesia:create_schema([node()]);
	_ ->
	    ok
    end,
    ejabberd:start_app(mnesia, permanent),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).

%% Start all the modules in all the hosts
start_modules() ->
    lists:foreach(
      fun(Host) ->
              Modules = ejabberd_config:get_option(
                          {modules, Host},
                          fun(Mods) ->
                                  lists:map(
                                    fun({M, A}) when is_atom(M), is_list(A) ->
                                            {M, A}
                                    end, Mods)
                          end, []),
              lists:foreach(
                fun({Module, Args}) ->
                        gen_mod:start_module(Host, Module, Args)
                end, Modules)
      end, ?MYHOSTS).

%% Stop all the modules in all the hosts
stop_modules() ->
    lists:foreach(
      fun(Host) ->
              Modules = ejabberd_config:get_option(
                          {modules, Host},
                          fun(Mods) ->
                                  lists:map(
                                    fun({M, A}) when is_atom(M), is_list(A) ->
                                            {M, A}
                                    end, Mods)
                          end, []),
              lists:foreach(
                fun({Module, _Args}) ->
                        gen_mod:stop_module_keep_config(Host, Module)
                end, Modules)
      end, ?MYHOSTS).

connect_nodes() ->
    Nodes = ejabberd_config:get_option(
              cluster_nodes,
              fun(Ns) ->
                      true = lists:all(fun is_atom/1, Ns),
                      Ns
              end, []),
    lists:foreach(fun(Node) ->
                          net_kernel:connect_node(Node)
                  end, Nodes).

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


broadcast_c2s_shutdown() ->
    Children = ejabberd_sm:get_all_pids(),
    lists:foreach(
      fun(C2SPid) when node(C2SPid) == node() ->
	      C2SPid ! system_shutdown;
	 (_) ->
	      ok
      end, Children).

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

set_loglevel_from_config() ->
    Level = ejabberd_config:get_option(
              loglevel,
              fun(P) when P>=0, P=<5 -> P end,
              4),
    ejabberd_logger:set(Level).

start_apps() ->
    ejabberd:start_app(sasl),
    ejabberd:start_app(ssl),
    ejabberd:start_app(p1_yaml),
    ejabberd:start_app(p1_tls),
    ejabberd:start_app(p1_xml),
    ejabberd:start_app(p1_stringprep),
    ejabberd:start_app(p1_zlib),
    ejabberd:start_app(p1_cache_tab).
