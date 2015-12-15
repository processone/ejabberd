%%%----------------------------------------------------------------------
%%% File    : nodetree_tree.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Standard node tree plugin
%%% Created :  1 Dec 2007 by Christophe Romain <christophe.romain@process-one.net>
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

%%% @doc The module <strong>{@module}</strong> is the default PubSub node tree plugin.
%%% <p>It is used as a default for all unknown PubSub node type.  It can serve
%%% as a developer basis and reference to build its own custom pubsub node tree
%%% types.</p>
%%% <p>PubSub node tree plugins are using the {@link gen_nodetree} behaviour.</p>
%%% <p><strong>The API isn't stabilized yet</strong>. The pubsub plugin
%%% development is still a work in progress. However, the system is already
%%% useable and useful as is. Please, send us comments, feedback and
%%% improvements.</p>

-module(nodetree_tree).
-behaviour(gen_pubsub_nodetree).
-author('christophe.romain@process-one.net').

-include_lib("stdlib/include/qlc.hrl").

-include("pubsub.hrl").
-include("jlib.hrl").

-export([init/3, terminate/2, options/0, set_node/1,
    get_node/3, get_node/2, get_node/1, get_nodes/2,
    get_nodes/1, get_parentnodes/3, get_parentnodes_tree/3,
    get_subnodes/3, get_subnodes_tree/3, create_node/6,
    delete_node/2]).

init(_Host, _ServerHost, _Options) ->
    mnesia:create_table(pubsub_node,
	[{disc_copies, [node()]},
	    {attributes, record_info(fields, pubsub_node)}]),
    mnesia:add_table_index(pubsub_node, id),
    NodesFields = record_info(fields, pubsub_node),
    case mnesia:table_info(pubsub_node, attributes) of
	NodesFields -> ok;
	_ -> ok
    end,
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
    case catch mnesia:read({pubsub_node, {Host, Node}}) of
	[Record] when is_record(Record, pubsub_node) -> Record;
	_ -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

get_node(Nidx) ->
    case catch mnesia:index_read(pubsub_node, Nidx, #pubsub_node.id) of
	[Record] when is_record(Record, pubsub_node) -> Record;
	_ -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

get_nodes(Host, _From) ->
    get_nodes(Host).

get_nodes(Host) ->
    mnesia:match_object(#pubsub_node{nodeid = {Host, '_'}, _ = '_'}).

get_parentnodes(_Host, _Node, _From) ->
    [].

%% @doc <p>Default node tree does not handle parents, return a list
%% containing just this node.</p>
get_parentnodes_tree(Host, Node, _From) ->
    case catch mnesia:read({pubsub_node, {Host, Node}}) of
	[Record] when is_record(Record, pubsub_node) -> [{0, [Record]}];
	_ -> []
    end.

get_subnodes(Host, Node, _From) ->
    get_subnodes(Host, Node).

get_subnodes(Host, <<>>) ->
    Q = qlc:q([N
		|| #pubsub_node{nodeid = {NHost, _},
			parents = Parents} =
		    N
		    <- mnesia:table(pubsub_node),
		    Host == NHost, Parents == []]),
    qlc:e(Q);
get_subnodes(Host, Node) ->
    Q = qlc:q([N
		|| #pubsub_node{nodeid = {NHost, _},
			parents = Parents} =
		    N
		    <- mnesia:table(pubsub_node),
		    Host == NHost, lists:member(Node, Parents)]),
    qlc:e(Q).

get_subnodes_tree(Host, Node, _From) ->
    get_subnodes_tree(Host, Node).

get_subnodes_tree(Host, Node) ->
    case get_node(Host, Node) of
	{error, _} ->
	    [];
	Rec ->
	    BasePlugin = jlib:binary_to_atom(<<"node_",
			(Rec#pubsub_node.type)/binary>>),
	    BasePath = BasePlugin:node_to_path(Node),
	    mnesia:foldl(fun (#pubsub_node{nodeid = {H, N}} = R, Acc) ->
			Plugin = jlib:binary_to_atom(<<"node_",
				    (R#pubsub_node.type)/binary>>),
			Path = Plugin:node_to_path(N),
			case lists:prefix(BasePath, Path) and (H == Host) of
			    true -> [R | Acc];
			    false -> Acc
			end
		end,
		[], pubsub_node)
    end.

create_node(Host, Node, Type, Owner, Options, Parents) ->
    BJID = jid:tolower(jid:remove_resource(Owner)),
    case catch mnesia:read({pubsub_node, {Host, Node}}) of
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
				[#pubsub_node{owners = [{[], Host, []}]}] ->
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
		    {error, ?ERR_FORBIDDEN}
	    end;
	_ ->
	    {error, ?ERR_CONFLICT}
    end.

delete_node(Host, Node) ->
    Removed = get_subnodes_tree(Host, Node),
    lists:foreach(fun (#pubsub_node{nodeid = {_, SubNode}, id = SubNidx}) ->
		pubsub_index:free(node, SubNidx),
		mnesia:delete({pubsub_node, {Host, SubNode}})
	end,
	Removed),
    Removed.
