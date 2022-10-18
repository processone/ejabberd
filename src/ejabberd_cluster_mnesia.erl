%%%----------------------------------------------------------------------
%%% File    : ejabberd_cluster_mnesia.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : ejabberd clustering management via Mnesia
%%% Created : 7 Oct 2015 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
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

-module(ejabberd_cluster_mnesia).
-behaviour(ejabberd_cluster).

%% API
-export([init/0, get_nodes/0, join/1, leave/1,
	 get_known_nodes/0, node_id/0, get_node_by_id/1,
	 send/2, wait_for_sync/1, subscribe/1]).

-include("logger.hrl").

-spec init() -> ok.
init() ->
    ok.

-spec get_nodes() -> [node()].

get_nodes() ->
    mnesia:system_info(running_db_nodes).

-spec get_known_nodes() -> [node()].

get_known_nodes() ->
    lists:usort(mnesia:system_info(db_nodes)
		++ mnesia:system_info(extra_db_nodes)).

-spec join(node()) -> ok | {error, any()}.

join(Node) ->
    case {node(), net_adm:ping(Node)} of
        {Node, _} ->
            {error, {not_master, Node}};
        {_, pong} ->
            application:stop(ejabberd),
            application:stop(mnesia),
            mnesia:delete_schema([node()]),
            application:start(mnesia),
            case mnesia:change_config(extra_db_nodes, [Node]) of
                {ok, _} ->
                    replicate_database(Node),
                    wait_for_sync(infinity),
                    application:stop(mnesia),
                    application:start(ejabberd);
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, {no_ping, Node}}
    end.

-spec leave(node()) -> ok | {error, any()}.

leave(Node) ->
    case {node(), net_adm:ping(Node)} of
        {Node, _} ->
            Cluster = get_nodes()--[Node],
            leave(Cluster, Node);
        {_, pong} ->
            rpc:call(Node, ?MODULE, leave, [Node], 10000);
        {_, pang} ->
            case mnesia:del_table_copy(schema, Node) of
                {atomic, ok} -> ok;
                {aborted, Reason} -> {error, Reason}
            end
    end.
leave([], Node) ->
    {error, {no_cluster, Node}};
leave([Master|_], Node) ->
    application:stop(ejabberd),
    application:stop(mnesia),
    spawn(fun() ->
              rpc:call(Master, mnesia, del_table_copy, [schema, Node]),
              mnesia:delete_schema([node()]),
              erlang:halt(0)
          end),
    ok.

-spec node_id() -> binary().
node_id() ->
    integer_to_binary(erlang:phash2(node())).

-spec get_node_by_id(binary()) -> node().
get_node_by_id(Hash) ->
    try binary_to_integer(Hash) of
	I -> match_node_id(I)
    catch _:_ ->
	    node()
    end.

-spec send({atom(), node()}, term()) -> boolean().
send(Dst, Msg) ->
    case erlang:send(Dst, Msg, [nosuspend, noconnect]) of
	ok -> true;
	_ -> false
    end.

-spec wait_for_sync(timeout()) -> ok.
wait_for_sync(Timeout) ->
    ?INFO_MSG("Waiting for Mnesia synchronization to complete", []),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), Timeout),
    ok.

-spec subscribe(_) -> ok.
subscribe(_) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

replicate_database(Node) ->
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    lists:foreach(
        fun(Table) ->
            Type = rpc:call(Node, mnesia, table_info, [Table, storage_type]),
            mnesia:add_table_copy(Table, node(), Type)
        end, mnesia:system_info(tables)--[schema]).

-spec match_node_id(integer()) -> node().
match_node_id(I) ->
    match_node_id(I, get_nodes()).

-spec match_node_id(integer(), [node()]) -> node().
match_node_id(I, [Node|Nodes]) ->
    case erlang:phash2(Node) of
	I -> Node;
	_ -> match_node_id(I, Nodes)
    end;
match_node_id(_I, []) ->
    node().
