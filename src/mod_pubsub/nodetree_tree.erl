%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%% 
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% 
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2009, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2009, ProcessOne.
%%%
%%%
%%% @copyright 2006-2009 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

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
-author('christophe.romain@process-one.net').

-include_lib("stdlib/include/qlc.hrl").
-include_lib("exmpp/include/exmpp.hrl").

-include("pubsub.hrl").

-behaviour(gen_pubsub_nodetree).

-export([init/3,
	 terminate/2,
	 options/0,
	 set_node/1,
	 get_node/3,
	 get_node/2,
	 get_node/1,
	 get_nodes/2,
	 get_nodes/1,
	 get_parentnodes/3,
	 get_parentnodes_tree/3,
	 get_subnodes/3,
	 get_subnodes_tree/3,
	 create_node/6,
	 delete_node/2
	]).


%% ================
%% API definition
%% ================

%% @spec (Host, ServerHost, Opts) -> any()
%%     Host = mod_pubsub:host()
%%     ServerHost = host()
%%     Opts = list()
%% @doc <p>Called during pubsub modules initialisation. Any pubsub plugin must
%% implement this function. It can return anything.</p>
%% <p>This function is mainly used to trigger the setup task necessary for the
%% plugin. It can be used for example by the developer to create the specific
%% module database schema if it does not exists yet.</p>
init(_Host, _ServerHost, _Opts) ->
    mnesia:create_table(pubsub_node,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, pubsub_node)}]),
    mnesia:add_table_index(pubsub_node, id),
    NodesFields = record_info(fields, pubsub_node),
    case mnesia:table_info(pubsub_node, attributes) of
	NodesFields -> ok;
	_ ->
	    ok
	    %% mnesia:transform_table(pubsub_state, ignore, StatesFields)
    end,
    ok.
terminate(_Host, _ServerHost) ->
    ok.

%% @spec () -> [Option]
%%     Option = mod_pubsub:nodetreeOption()
%% @doc Returns the default pubsub node tree options.
options() ->
    [{virtual_tree, false}].

%% @spec (NodeRecord) -> ok | {error, Reason}
%%     Record = mod_pubsub:pubsub_node()
set_node(Record) when is_record(Record, pubsub_node) ->
    mnesia:write(Record);
set_node(_) ->
    {error, 'internal-server-error'}.

%% @spec (Host, Node, From) -> pubsubNode() | {error, Reason}
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
get_node(Host, Node, _From) ->
    get_node(Host, Node).
get_node(Host, Node) ->
    case catch mnesia:read({pubsub_node, {Host, Node}}) of
	[Record] when is_record(Record, pubsub_node) -> Record;
	[] -> {error, 'item-not-found'};
	Error -> Error
    end.
get_node(NodeId) ->
    case catch mnesia:index_read(pubsub_node, NodeId, #pubsub_node.id) of
	[Record] when is_record(Record, pubsub_node) -> Record;
	[] -> {error, 'item-not-found'};
	Error -> Error
    end.

%% @spec (Host, From) -> [pubsubNode()] | {error, Reason}
%%     Host = mod_pubsub:host() | mod_pubsub:jid()
get_nodes(Host, _From) ->
    get_nodes(Host).
get_nodes(Host) ->
    mnesia:match_object(#pubsub_node{nodeid = {Host, '_'}, _ = '_'}).

%% @spec (Host, Node, From) -> [{Depth, Record}] | {error, Reason}
%%     Host   = mod_pubsub:host() | mod_pubsub:jid()
%%     Node   = mod_pubsub:pubsubNode()
%%     From   = mod_pubsub:jid()
%%     Depth  = integer()
%%     Record = pubsubNode()
%% @doc <p>Default node tree does not handle parents, return empty list.</p>
get_parentnodes(_Host, _Node, _From) ->
    [].

%% @spec (Host, Node, From) -> [{Depth, Record}] | {error, Reason}
%%     Host   = mod_pubsub:host() | mod_pubsub:jid()
%%     Node   = mod_pubsub:pubsubNode()
%%     From   = mod_pubsub:jid()
%%     Depth  = integer()
%%     Record = pubsubNode()
%% @doc <p>Default node tree does not handle parents, return a list
%% containing just this node.</p>
get_parentnodes_tree(Host, Node, From) ->
    case get_node(Host, Node, From) of
	N when is_record(N, pubsub_node) -> [{0, [N]}];
	_Error -> []
    end.

%% @spec (Host, Node, From) -> [pubsubNode()] | {error, Reason}
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%%     From = mod_pubsub:jid()
get_subnodes(Host, Node, _From) ->
    get_subnodes(Host, Node).
get_subnodes(Host, <<>>) ->
    Q = qlc:q([N || #pubsub_node{nodeid = {NHost, _},
                          parents = Parents} = N <- mnesia:table(pubsub_node),
               Host == NHost,
               Parents == []]),
    qlc:e(Q);
get_subnodes(Host, Node) ->
    Q = qlc:q([N || #pubsub_node{nodeid = {NHost, _},
				 parents = Parents} = N <- mnesia:table(pubsub_node),
		       Host == NHost,
		       lists:member(Node, Parents)]),
    qlc:e(Q).

%% @spec (Host, Index, From) -> [pubsubNodeIdx()] | {error, Reason}
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%%     From = mod_pubsub:jid()
get_subnodes_tree(Host, Node, _From) ->
    get_subnodes_tree(Host, Node).
get_subnodes_tree(Host, Node) ->
    case get_node(Host, Node) of
    {error, _} ->
	[];
    Rec ->
	BasePlugin = list_to_atom("node_"++Rec#pubsub_node.type),
	BasePath = BasePlugin:node_to_path(Node),
	mnesia:foldl(fun(#pubsub_node{nodeid = {H, N}} = R, Acc) ->
		Plugin = list_to_atom("node_"++R#pubsub_node.type),
		Path = Plugin:node_to_path(N),
		case lists:prefix(BasePath, Path) and (H == Host) of
		    true -> [R | Acc];
		    false -> Acc
		end
	    end, [], pubsub_node)
    end.

%% @spec (Host, Node, Type, Owner, Options) -> ok | {error, Reason}
%%     Host = mod_pubsub:host() | mod_pubsub:jid()
%%     Node = mod_pubsub:pubsubNode()
%%     NodeType = mod_pubsub:nodeType()
%%     Owner = mod_pubsub:jid()
%%     Options = list()
create_node(Host, Node, Type, Owner, Options, Parents) ->
    BJID = jlib:short_prepd_bare_jid(Owner),
    case mnesia:read({pubsub_node, {Host, Node}}) of
	[] ->
	    ParentExists =
		case Host of
		    {_U, _S, _R} ->
			%% This is special case for PEP handling
			%% PEP does not uses hierarchy
			true;
		    _ ->
			case Parents of
                [] -> true;
                [Parent | _] ->
			    case mnesia:read({pubsub_node, {Host, Parent}}) of
				    [#pubsub_node{owners = [{[], Host, []}]}] -> true;
                    [#pubsub_node{owners = Owners}] -> lists:member(BJID, Owners);
                    _ -> false
			    end;
                _ -> 
                    false
			end
		end,
	    case ParentExists of
		true ->
		    NodeId = pubsub_index:new(node),
		    mnesia:write(#pubsub_node{nodeid = {Host, Node},
					      id = NodeId,
					      parents = Parents,
					      type = Type,
					      owners = [BJID],
					      options = Options}),
		    {ok, NodeId};
		false ->
		    %% Requesting entity is prohibited from creating nodes
		    {error, 'forbidden'}
	    end;
	_ ->
	    %% NodeID already exists
	    {error, 'conflict'}
    end.

%% @spec (Host, Node) -> [mod_pubsub:node()]
%%     Host = mod_pubsub:host() | mod_pubsub:jid()
%%     Node = mod_pubsub:pubsubNode()
delete_node(Host, Node) ->
    Removed = get_subnodes_tree(Host, Node),
    lists:foreach(fun(#pubsub_node{nodeid = {_, N}, id = I}) ->
	    pubsub_index:free(node, I),
	    mnesia:delete({pubsub_node, {Host, N}})
	end, Removed),
    Removed.
