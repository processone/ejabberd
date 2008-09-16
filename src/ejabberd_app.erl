%%%----------------------------------------------------------------------
%%% File    : ejabberd_app.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd's application callback module
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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

-export([start/2, prep_stop/1, stop/1, init/0]).

-include("ejabberd.hrl").


%%%
%%% Application API
%%%

start(normal, _Args) ->
    ejabberd_loglevel:set(4),
    application:start(sasl),
    randoms:start(),
    db_init(),
    sha:start(),
    stringprep_sup:start_link(),
    translate:start(),
    acl:start(),
    ejabberd_ctl:init(),
    gen_mod:start(),
    ejabberd_config:start(),
    ejabberd_check:config(),
    start(),
    connect_nodes(),
    Sup = ejabberd_sup:start_link(),
    ejabberd_rdbms:start(),
    ejabberd_auth:start(),
    cyrsasl:start(),
    % Profiling
    %eprof:start(),
    %eprof:profile([self()]),
    %fprof:trace(start, "/tmp/fprof"),
    start_modules(),
    Sup;
start(_, _) ->
    {error, badarg}.

%% Prepare the application for termination.
%% This function is called when an application is about to be stopped, 
%% before shutting down the processes of the application.
prep_stop(State) ->
    stop_modules(),
    State.

%% All the processes were killed when this function is called
stop(_State) ->
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
    LogPath =
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
	end,
    error_logger:add_report_handler(ejabberd_logger_h, LogPath),
    erl_ddll:load_driver(ejabberd:get_so_path(), tls_drv),
    case erl_ddll:load_driver(ejabberd:get_so_path(), expat_erl) of
	ok -> ok;
	{error, already_loaded} -> ok
    end,
    Port = open_port({spawn, expat_erl}, [binary]),
    loop(Port).


loop(Port) ->
    receive
	_ ->
	    loop(Port)
    end.

db_init() ->
    case mnesia:system_info(extra_db_nodes) of
	[] ->
	    mnesia:create_schema([node()]);
	_ ->
	    ok
    end,
    mnesia:start(),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).

%% Start all the modules in all the hosts
start_modules() ->
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

