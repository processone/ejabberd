%%%----------------------------------------------------------------------
%%% File    : ejabberd_odbc_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ODBC connections supervisor
%%% Created : 22 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_odbc_sup).

-author('alexey@process-one.net').

%% API
-export([start_link/1, init/1, add_pid/2, remove_pid/2,
	 get_pids/1, get_random_pid/1, transform_options/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(PGSQL_PORT, 5432).

-define(MYSQL_PORT, 3306).

-define(DEFAULT_POOL_SIZE, 10).

-define(DEFAULT_ODBC_START_INTERVAL, 30).

-define(CONNECT_TIMEOUT, 500).

-record(sql_pool, {host, pid}).

start_link(Host) ->
    mnesia:create_table(sql_pool,
			[{ram_copies, [node()]}, {type, bag},
			 {local_content, true},
			 {attributes, record_info(fields, sql_pool)}]),
    mnesia:add_table_copy(sql_pool, node(), ram_copies),
    F = fun () -> mnesia:delete({sql_pool, Host}) end,
    mnesia:ets(F),
    supervisor:start_link({local,
			   gen_mod:get_module_proc(Host, ?MODULE)},
			  ?MODULE, [Host]).

init([Host]) ->
    PoolSize = ejabberd_config:get_option(
                 {odbc_pool_size, Host},
                 fun(I) when is_integer(I), I>0 -> I end,
                 ?DEFAULT_POOL_SIZE),
    StartInterval = ejabberd_config:get_option(
                      {odbc_start_interval, Host},
                      fun(I) when is_integer(I), I>0 -> I end,
                      ?DEFAULT_ODBC_START_INTERVAL),
    {ok,
     {{one_for_one, PoolSize * 10, 1},
      lists:map(fun (I) ->
			{I,
			 {ejabberd_odbc, start_link,
			  [Host, StartInterval * 1000]},
			 transient, 2000, worker, [?MODULE]}
		end,
		lists:seq(1, PoolSize))}}.

get_pids(Host) ->
    Rs = mnesia:dirty_read(sql_pool, Host),
    [R#sql_pool.pid || R <- Rs].

get_random_pid(Host) ->
    case get_pids(Host) of
      [] -> none;
      Pids -> lists:nth(erlang:phash(now(), length(Pids)), Pids)
    end.

add_pid(Host, Pid) ->
    F = fun () ->
		mnesia:write(#sql_pool{host = Host, pid = Pid})
	end,
    mnesia:ets(F).

remove_pid(Host, Pid) ->
    F = fun () ->
		mnesia:delete_object(#sql_pool{host = Host, pid = Pid})
	end,
    mnesia:ets(F).

transform_options(Opts) ->
    lists:foldl(fun transform_options/2, [], Opts).

transform_options({odbc_server, {Type, Server, Port, DB, User, Pass}}, Opts) ->
    [{odbc_type, Type},
     {odbc_server, Server},
     {odbc_port, Port},
     {odbc_database, DB},
     {odbc_username, User},
     {odbc_password, Pass}|Opts];
transform_options({odbc_server, {mysql, Server, DB, User, Pass}}, Opts) ->
    transform_options({odbc_server, {mysql, Server, ?MYSQL_PORT, DB, User, Pass}}, Opts);
transform_options({odbc_server, {pgsql, Server, DB, User, Pass}}, Opts) ->
    transform_options({odbc_server, {pgsql, Server, ?PGSQL_PORT, DB, User, Pass}}, Opts);
transform_options(Opt, Opts) ->
    [Opt|Opts].
