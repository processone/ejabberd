%%%----------------------------------------------------------------------
%%% File    : nodetree_tree.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Standard node tree plugin
%%% Created :  1 Dec 2007 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2021   ProcessOne
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

%%% @doc The module <strong>{@module}</strong> is the default PubSub node tree plugin.
%%% <p>It is used as a default for all unknown PubSub node type.  It can serve
%%% as a developer basis and reference to build its own custom pubsub node tree
%%% types.</p>
%%% <p>PubSub node tree plugins are using the {@link gen_nodetree} behaviour.</p>
%%% <p><strong>The API isn't stabilized yet</strong>. The pubsub plugin
%%% development is still a work in progress. However, the system is already
%%% usable and useful as is. Please, send us comments, feedback and
%%% improvements.</p>

-module(nodetree_tree).
-behaviour(gen_pubsub_nodetree).
-author('christophe.romain@process-one.net').

-include_lib("stdlib/include/qlc.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-include("pubsub.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

-export([init/3, terminate/2, options/0, set_node/1,
    get_node/3, get_node/2, get_node/1, get_nodes/2,
    get_nodes/1, get_parentnodes/3, get_parentnodes_tree/3,
    get_subnodes/3, get_subnodes_tree/3, create_node/6,
    delete_node/2]).

init(_Host, _ServerHost, _Options) ->
    ejabberd_mnesia:create(?MODULE, pubsub_node,
	[{disc_copies, [node()]},
	    {attributes, record_info(fields, pubsub_node)},
	    {index, [id]}]),
    %% mnesia:transform_table(pubsub_state, ignore, StatesFields)
    ok.

terminate(_Host, _ServerHost) ->
    ok.

options() ->
    [{virtual_tree, false}].

set_node(Node) when is_record(Node, pubsub_node) ->
    mnesia:write(Node).

get_node(Host, Node, _From) ->
    get_node(Host, Node).

get_node(Host, Node) ->
    case mnesia:read({pubsub_node, {Host, Node}}) of
	[Record] when is_record(Record, pubsub_node) -> Record;
	_ -> {error, xmpp:err_item_not_found(?T("Node not found"), ejabberd_option:language())}
    end.

get_node(Nidx) ->
    case mnesia:index_read(pubsub_node, Nidx, #pubsub_node.id) of
	[Record] when is_record(Record, pubsub_node) -> Record;
	_ -> {error, xmpp:err_item_not_found(?T("Node not found"), ejabberd_option:language())}
    end.

get_nodes(Host) ->
    get_nodes(Host, infinity).

get_nodes(Host, infinity) ->
    mnesia:match_object(#pubsub_node{nodeid = {Host, '_'}, _ = '_'});
get_nodes(Host, Limit) ->
    case mnesia:select(
	   pubsub_node,
	   ets:fun2ms(
	     fun(#pubsub_node{nodeid = {H, _}} = Node) when H == Host ->
		     Node
	     end), Limit, read) of
	'$end_of_table' -> [];
	{Nodes, _} -> Nodes
    end.

get_parentnodes(Host, Node, _From) ->
    case catch mnesia:read({pubsub_node, {Host, Node}}) of
	[Record] when is_record(Record, pubsub_node) ->
	    Record#pubsub_node.parents;
	_ ->
	    []
    end.

get_parentnodes_tree(Host, Node, _From) ->
    get_parentnodes_tree(Host, Node, 0, []).
get_parentnodes_tree(Host, Node, Level, Acc) ->
    case catch mnesia:read({pubsub_node, {Host, Node}}) of
	[Record] when is_record(Record, pubsub_node) ->
	    Tree = [{Level, [Record]}|Acc],
	    case Record#pubsub_node.parents of
		[Parent] -> get_parentnodes_tree(Host, Parent, Level+1, Tree);
		_ -> Tree
	    end;
	_ ->
	    Acc
    end.

get_subnodes(Host, <<>>, infinity) ->
    mnesia:match_object(#pubsub_node{nodeid = {Host, '_'}, parents = [], _ = '_'});
get_subnodes(Host, <<>>, Limit) ->
    case mnesia:select(
	   pubsub_node,
	   ets:fun2ms(
	     fun(#pubsub_node{nodeid = {H, _}, parents = []} = Node) when H == Host ->
		     Node
	     end), Limit, read) of
	'$end_of_table' -> [];
	{Nodes, _} -> Nodes
    end;
get_subnodes(Host, Node, infinity) ->
    Q = qlc:q([N
		|| #pubsub_node{nodeid = {NHost, _},
			parents = Parents} =
		    N
		    <- mnesia:table(pubsub_node),
		    Host == NHost, lists:member(Node, Parents)]),
    qlc:e(Q);
get_subnodes(Host, Node, Limit) ->
    case mnesia:select(
	   pubsub_node,
	   ets:fun2ms(
	     fun(#pubsub_node{nodeid = {H, _}, parents = Ps} = N)
		   when H == Host andalso Ps /= [] -> N
	     end), Limit, read) of
	'$end_of_table' -> [];
	{Nodes, _} ->
	    lists:filter(
	      fun(#pubsub_node{parents = Parents}) ->
		      lists:member(Node, Parents)
	      end, Nodes)
    end.

get_subnodes_tree(Host, Node, _From) ->
    get_subnodes_tree(Host, Node).

get_subnodes_tree(Host, Node) ->
    case get_node(Host, Node) of
	{error, _} ->
	    [];
	Rec ->
	    BasePlugin = misc:binary_to_atom(<<"node_",
			(Rec#pubsub_node.type)/binary>>),
	    {result, BasePath} = BasePlugin:node_to_path(Node),
	    mnesia:foldl(fun (#pubsub_node{nodeid = {H, N}} = R, Acc) ->
			Plugin = misc:binary_to_atom(<<"node_",
				    (R#pubsub_node.type)/binary>>),
			{result, Path} = Plugin:node_to_path(N),
			case lists:prefix(BasePath, Path) and (H == Host) of
			    true -> [R | Acc];
			    false -> Acc
			end
		end,
		[], pubsub_node)
    end.

create_node(Host, Node, Type, Owner, Options, Parents) ->
    BJID = jid:tolower(jid:remove_resource(Owner)),
    case mnesia:read({pubsub_node, {Host, Node}}) of
	[] ->
	    ParentExists = case Host of
		{_U, _S, _R} ->
		    %% This is special case for PEP handling
		    %% PEP does not uses hierarchy
		    true;
		_ ->
		    case Parents of
			[] ->
			    true;
			[Parent | _] ->
			    case catch mnesia:read({pubsub_node, {Host, Parent}}) of
				[#pubsub_node{owners = [{<<>>, Host, <<>>}]}] ->
				    true;
				[#pubsub_node{owners = Owners}] ->
				    lists:member(BJID, Owners);
				_ ->
				    false
			    end;
			_ ->
			    false
		    end
	    end,
	    case ParentExists of
		true ->
		    Nidx = pubsub_index:new(node),
		    mnesia:write(#pubsub_node{nodeid = {Host, Node},
			    id = Nidx, parents = Parents,
			    type = Type, owners = [BJID],
			    options = Options}),
		    {ok, Nidx};
		false ->
		    {error, xmpp:err_forbidden()}
	    end;
	_ ->
	    {error, xmpp:err_conflict(?T("Node already exists"), ejabberd_option:language())}
    end.

delete_node(Host, Node) ->
    Removed = get_subnodes_tree(Host, Node),
    lists:foreach(fun (#pubsub_node{nodeid = {_, SubNode}, id = SubNidx}) ->
		pubsub_index:free(node, SubNidx),
		mnesia:delete({pubsub_node, {Host, SubNode}})
	end,
	Removed),
    Removed.
