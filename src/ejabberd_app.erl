%%%----------------------------------------------------------------------
%%% File    : ejabberd_app.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd's application callback module
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_app).
-author('alexey@process-one.net').

-behaviour(application).

-export([start_modules/0,start/2, get_log_path/0, prep_stop/1, stop/1, init/0]).

-include("ejabberd.hrl").


%%%
%%% Application API
%%%

start(normal, _Args) ->
    ejabberd_loglevel:set(4),
    write_pid_file(),
    application:start(sasl),
    application:start(exmpp),
    randoms:start(),
    db_init(),
    sha:start(),
    start(),
    translate:start(),
    acl:start(),
    ejabberd_ctl:init(),
    ejabberd_commands:init(),
    ejabberd_admin:start(),
    gen_mod:start(),
    ejabberd_config:start(),
    ejabberd_check:config(),
    connect_nodes(),
    %% Loading ASN.1 driver explicitly to avoid races in LDAP
    catch asn1rt:load_driver(),
    Sup = ejabberd_sup:start_link(),
    ejabberd_rdbms:start(),
    ejabberd_auth:start(),
    cyrsasl:start(),
    % Profiling
    %ejabberd_debug:eprof_start(),
    %ejabberd_debug:fprof_start(),
    maybe_add_nameservers(),
    print_start_debug_info(),
    start_modules(),
    ejabberd_cluster:announce(),
    ejabberd_node_groups:start(),
    ejabberd_listener:start_listeners(),
    ?INFO_MSG("ejabberd ~s is started in the node ~p", [?VERSION, node()]),
    Sup;
start(_, _) ->
    {error, badarg}.

%% Prepare the application for termination.
%% This function is called when an application is about to be stopped,
%% before shutting down the processes of the application.
prep_stop(State) ->
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
    %erlang:system_flag(fullsweep_after, 0),
    %error_logger:logfile({open, ?LOG_PATH}),
    LogPath = get_log_path(),
    error_logger:add_report_handler(ejabberd_logger_h, LogPath),
    erl_ddll:load_driver(ejabberd:get_so_path(), tls_drv),
    ok.

db_init() ->
    case mnesia:system_info(extra_db_nodes) of
	[] ->
	    mnesia:create_schema([node()]);
	_ ->
	    ok
    end,
    application:start(mnesia, permanent),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).

%% Start all the modules in all the hosts
start_modules() ->
    case ejabberd_config:get_local_option({static_modules, global}) of
	undefined ->
	    ok;
	StaticModules ->
	    lists:foreach(
	      fun({Module, Args}) ->
		      gen_mod:start_module(global, Module, Args)
	      end, StaticModules)
    end,
    lists:foreach(
      fun(Host) ->
	      case ejabberd_config:get_local_option({modules, Host}) of
		  undefined ->
		      ok;
		  Modules ->
		      lists:foreach(
			fun({Module, Args}) ->
				gen_mod:start_module(Host, Module, Args)
			end, Modules)
	      end
      end, ?MYHOSTS).

%% Stop all the modules in all the hosts
stop_modules() ->
    lists:foreach(
      fun(Host) ->
	      case ejabberd_config:get_local_option({modules, Host}) of
		  undefined ->
		      ok;
		  Modules ->
		      lists:foreach(
			fun({Module, _Args}) ->
				gen_mod:stop_module_keep_config(Host, Module)
			end, Modules)
	      end
      end, ?MYHOSTS).

connect_nodes() ->
    case ejabberd_config:get_local_option(cluster_nodes) of
	undefined ->
	    ok;
	Nodes when is_list(Nodes) ->
	    lists:foreach(fun(Node) ->
				  net_kernel:connect_node(Node)
			  end, Nodes)
    end.

%% @spec () -> string()
%% @doc Returns the full path to the ejabberd log file.
%% It first checks for application configuration parameter 'log_path'.
%% If not defined it checks the environment variable EJABBERD_LOG_PATH.
%% And if that one is neither defined, returns the default value:
%% "ejabberd.log" in current directory.
get_log_path() ->
    case application:get_env(log_path) of
	{ok, Path} ->
	    Path;
	undefined ->
	    case os:getenv("EJABBERD_LOG_PATH") of
		false ->
		    ?LOG_PATH;
		Path ->
		    Path
	    end
    end.


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
    Children = supervisor:which_children(ejabberd_c2s_sup),
    lists:foreach(
      fun({_, C2SPid, _, _}) ->
	      C2SPid ! system_shutdown
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

print_start_debug_info() ->
    ?DEBUG("Inet DB RC:~n~p", [inet_db:get_rc()]),
    ?DEBUG("Mnesia database:~n~p", [mnesia:system_info(all)]),
    ?DEBUG("Mnesia tables:~n~p",
	   [ [{Table, mnesia:table_info(Table, all)} ||
		 Table <- lists:sort(mnesia:system_info(tables))] ]),
    ok.
