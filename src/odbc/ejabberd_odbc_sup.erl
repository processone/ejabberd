%%%----------------------------------------------------------------------
%%% File    : ejabberd_odbc_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ODBC connections supervisor
%%% Created : 22 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_odbc_sup).
-author('alexey@process-one.net').

%% API
-export([start_link/1,
	 init/1,
	 add_pid/3,
	 remove_pid/3,
	 get_dbtype/1,
	 get_pids/1,
	 get_random_pid/1
	]).

-include("ejabberd.hrl").

-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_ODBC_START_INTERVAL, 30). % 30 seconds

% time to wait for the supervisor to start its child before returning
% a timeout error to the request
-define(CONNECT_TIMEOUT, 500). % milliseconds


-record(sql_pool, {host, pid, dbtype}).

start_link(Host) ->
    mnesia:create_table(sql_pool,
			[{ram_copies, [node()]},
			 {type, bag},
			 {local_content, true},
			 {attributes, record_info(fields, sql_pool)}]),
    mnesia:add_table_copy(sql_pool, node(), ram_copies),
    F = fun() ->
		mnesia:delete({sql_pool, Host})
	end,
    mnesia:ets(F),
    supervisor:start_link({local, gen_mod:get_module_proc(Host, ?MODULE)},
			  ?MODULE, [Host]).

init([Host]) ->
    PoolSize = case ejabberd_config:get_local_option({odbc_pool_size, Host}) of
		   I when is_integer(I) ->
		       I;
	    undefined ->
		       ?DEFAULT_POOL_SIZE;
		   Other ->
		       ?ERROR_MSG("Wrong odbc_pool_size definition '~p' "
				  "for host ~p, default to ~p~n",
				  [Other, Host, ?DEFAULT_POOL_SIZE]),
		       ?DEFAULT_POOL_SIZE
	       end,
    StartInterval = case ejabberd_config:get_local_option({odbc_start_interval,
							   Host}) of
			Interval when is_integer(Interval) ->
			    Interval;
			undefined ->
			    ?DEFAULT_ODBC_START_INTERVAL;
			_Other2 ->
			    ?ERROR_MSG("Wrong odbc_start_interval "
				       "definition '~p' for host ~p, "
				       "defaulting to ~p~n",
				       [_Other2, Host,
					?DEFAULT_ODBC_START_INTERVAL]),
			    ?DEFAULT_ODBC_START_INTERVAL
		    end,
    {ok, {{one_for_one, PoolSize*10, 1},
	  lists:map(
	    fun(I) ->
		    {I,
		     {ejabberd_odbc, start_link, [Host, StartInterval*1000]},
		     transient,
                     2000,
		     worker,
		     [?MODULE]}
	    end, lists:seq(1, PoolSize))}}.

%% @spec (Host::string()) -> [pid()]
get_pids(Host) ->
    case ejabberd_config:get_local_option({odbc_server, Host}) of
	{host, Host1} ->
	    get_pids(Host1);
	_ ->
	    Rs = mnesia:dirty_read(sql_pool, Host),
	    [R#sql_pool.pid || R <- Rs]
    end.

get_random_pid(HostB) when is_binary(HostB) ->
    get_random_pid(binary_to_list(HostB));
get_random_pid(global) ->
    get_random_pid("localhost");
get_random_pid(Host) ->
    Pids = get_pids(ejabberd:normalize_host(Host)),
    lists:nth(erlang:phash(now(), length(Pids)), Pids).

get_dbtype(HostB) when is_binary(HostB) ->
    get_dbtype(binary_to_list(HostB));
get_dbtype(global) ->
    get_dbtype("localhost");
get_dbtype(Host) ->
    case ejabberd_config:get_local_option({odbc_server, Host}) of
       {host, Host1} ->
           get_dbtype(Host1);
       _ ->
           [#sql_pool{dbtype = Dbtype}|_] = mnesia:dirty_read(sql_pool, Host),
           Dbtype
    end.

add_pid(Host, Pid, Dbtype) ->
    F = fun() ->
		mnesia:write(
		  #sql_pool{host = Host,
			    pid = Pid,
			    dbtype = Dbtype})
	end,
    mnesia:ets(F).

remove_pid(Host, Pid, Dbtype) ->
    F = fun() ->
		mnesia:delete_object(
		  #sql_pool{host = Host,
			    pid = Pid,
			    dbtype = Dbtype})
	end,
    mnesia:ets(F).
