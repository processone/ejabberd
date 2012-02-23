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
%%% Portions created by ProcessOne are Copyright 2006-2012, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2012, ProcessOne.
%%%
%%%
%%% @copyright 2006-2012 ProcessOne
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

-include("pubsub.hrl").

-behaviour(gen_pubsub_nodetree).

-export([
	 init/3,
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
-spec(init/3 ::
      (
	     Host       :: string(),
	     ServerHost :: string(),
	     Opts       :: [{Key::atom(), Value::term()}])
      -> 'ok'
	    ).

init(_Host, _ServerHost, _Opts) ->
    mnesia:create_table(pubsub_node,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, pubsub_node)}]),
    mnesia:add_table_index(pubsub_node, idx),
    NodesFields = record_info(fields, pubsub_node),
    case mnesia:table_info(pubsub_node, attributes) of
	NodesFields -> ok;
	_           -> ok  %% mnesia:transform_table(pubsub_state, ignore, StatesFields)
    end,
    ok.


-spec(terminate/2 ::
      (
		  Host       :: string(),
		  ServerHost :: string())
      -> 'ok'
	    ).

terminate(_Host, _ServerHost) ->
    ok.

%% @spec () -> [Option]
%%     Option = mod_pubsub:nodetreeOption()
%% @doc Returns the default pubsub node tree options.
-spec(options/0 :: () -> Options::[{'virtual_tree', 'false'}]).

options() -> [{'virtual_tree', 'false'}].

%% @spec (NodeRecord) -> ok | {error, Reason}
%%     Record = mod_pubsub:pubsub_node()
						%(
						%  Node :: pubsubNode() -> 'ok' | {'error', Reason::_};
						%  Node :: any()        -> {'error', 'internal-server-error'}
						%).
%% -spec breaks compilation

set_node(#pubsub_node{} = Node) -> mnesia:write(Node);
set_node(_)                     -> {error, 'internal-server-error'}.

%% @spec (Host, Node, From) -> pubsubNode() | {error, Reason}
%%     Host = mod_pubsub:host()
%%     Node = node()
-spec(get_node/3 ::
      (
		 Host   :: host(), % hostPubsub | hostPEP()
		 NodeId :: nodeId(),
		 JID    :: jidEntity())
      -> pubsubNode() | {error, 'item-not-found'} | any()
	    ).

get_node(Host, NodeId, _JID) ->
    get_node(Host, NodeId).


-spec(get_node/2 ::
      (
		 Host   :: host(), % hostPubsub | hostPEP()
		 NodeId :: nodeId())
      -> pubsubNode() | {error, 'item-not-found'} | any()
	    ).

get_node(Host, NodeId) ->
    case catch mnesia:read({pubsub_node, {Host, NodeId}}) of
	[#pubsub_node{} = Node] -> Node;
	[]                      -> {error, 'item-not-found'};
	Error                   -> Error
    end.


-spec(get_node/1 ::
      (
		 NodeIdx :: nodeIdx())
      -> pubsubNode() | {'error', 'item-not-found'}
	    ).

get_node(NodeIdx) ->
    case catch mnesia:index_read(pubsub_node, NodeIdx, #pubsub_node.idx) of
	[#pubsub_node{} = Node] -> Node;
	[]                      -> {error, 'item-not-found'};
	Error                   -> Error
    end.

%% @spec (Host, From) -> [pubsubNode()] | {error, Reason}
%%     Host = mod_pubsub:host() | ljid()
-spec(get_nodes/2 ::
      (
		  Host :: host(), % hostPubsub | hostPEP()
		  JID  :: jidEntity())
      -> Nodes :: [] | [Node::pubsubNode()]
	    ).

get_nodes(Host, _JID) ->
    get_nodes(Host).


-spec(get_nodes/1 ::
      (
		  Host :: host()) % hostPubsub | hostPEP()
      -> Nodes :: [] | [Node::pubsubNode()]
	    ).

get_nodes(Host) ->
    mnesia:match_object(#pubsub_node{id = {Host, '_'}, _ = '_'}).

%% @spec (Host, Node, From) -> [{Depth, Record}] | {error, Reason}
%%     Host   = mod_pubsub:host() | ljid()
%%     Node   = node()
%%     From   = ljid()
%%     Depth  = integer()
%%     Record = pubsubNode()
%% @doc <p>Default node tree does not handle parents, return empty list.</p>
-spec(get_parentnodes/3 ::
      (
			Host   :: host(), % hostPubsub | hostPEP()
			NodeId :: nodeId(),
			JID    :: jidEntity())
      -> ParentNodes :: []
	    ).

get_parentnodes(_Host, _NodeId, _JID) ->
    [].

%% @spec (Host, Node, From) -> [{Depth, Record}] | {error, Reason}
%%     Host   = mod_pubsub:host() | ljid()
%%     Node   = node()
%%     From   = ljid()
%%     Depth  = integer()
%%     Record = pubsubNode()
%% @doc <p>Default node tree does not handle parents, return a list
%% containing just this node.</p>
-spec(get_parentnodes_tree/3 ::
      (
			     Host   :: host(), % hostPubsub | hostPEP()
			     NodeId :: nodeId(),
			     JID    :: jidEntity())
      -> ParentNodesTree :: [] | [{0, [ParentNode::pubsubNode()]}]
	    ).

get_parentnodes_tree(Host, NodeId, JID) ->
    case get_node(Host, NodeId, JID) of
	#pubsub_node{} = ParentNode -> [{0, [ParentNode]}];
	_Error                      -> []
    end.

%% @spec (Host, Node, From) -> [pubsubNode()] | {error, Reason}
%%     Host = mod_pubsub:host()
%%     Node = node()
%%     From = ljid()
-spec(get_subnodes/3 ::
      (
		     Host   :: host(), % hostPubsub | hostPEP()
		     NodeId :: nodeId(),
		     JID    :: jidEntity())
      -> SubNodes :: [] | [Node::pubsubNode()]
	    ).

get_subnodes(Host, NodeId, _JID) ->
    get_subnodes(Host, NodeId).


-spec(get_subnodes/2 ::
      (
		     ParentNodeHost :: host(), % hostPubsub | hostPEP()
		     ParentNodeId   :: nodeId())
      -> SubNodes :: [] | [Node::pubsubNode()]
	    ).

get_subnodes(ParentNodeHost, <<>>) ->
    Q = qlc:q(
	  [Node
	   || #pubsub_node{id = {Host, _}, parents = ParentNodeIds} = Node
		  <- mnesia:table(pubsub_node),
	      ParentNodeHost == Host,
	      ParentNodeIds == []]),
    qlc:e(Q);
get_subnodes(ParentNodeHost, ParentNodeId) ->
    Q = qlc:q(
	  [Node
	   || #pubsub_node{id = {Host, _}, parents = ParentNodeIds} = Node
		  <- mnesia:table(pubsub_node),
	      ParentNodeHost == Host,
	      lists:member(ParentNodeId, ParentNodeIds)]),
    qlc:e(Q).

%% @spec (Host, Index, From) -> [nodeidx()] | {error, Reason}
%%     Host = mod_pubsub:host()
%%     Node = node()
%%     From = ljid()
-spec(get_subnodes_tree/3 ::
      (
			  Host         :: host(), % hostPubsub | hostPEP()
			  ParentNodeId :: nodeId(),
			  JID          :: jidEntity())
      -> SubNodes :: [] | [Node::pubsubNode()]
	    ).

get_subnodes_tree(Host, ParentNodeId, _JID) ->
    get_subnodes_tree(Host, ParentNodeId).


-spec(get_subnodes_tree/2 ::
      (
			  Host         :: host(), % hostPubsub | hostPEP()
			  ParentNodeId :: nodeId())
      -> SubNodes :: [] | [Node::pubsubNode()]
	    ).

get_subnodes_tree(ParentNodeHost, ParentNodeId) ->
    case get_node(ParentNodeHost, ParentNodeId) of
	{error, _} -> [];
	#pubsub_node{type = ParentNodeType} = _ParentNode ->
	    BasePlugin = list_to_atom("node_"++ParentNodeType),
	    BasePath = BasePlugin:node_to_path(ParentNodeId),
	    mnesia:foldl(fun
			 (#pubsub_node{id = {Host, NodeId}, type = Type} = Node, Acc) ->
				Plugin = list_to_atom("node_"++Type),
				Path = Plugin:node_to_path(NodeId),
				case lists:prefix(BasePath, Path) and (Host == ParentNodeHost) of
				    true  -> [Node | Acc];
				    false -> Acc
				end
			end, [], pubsub_node)
    end.

%% @spec (Host, Node, Type, Owner, Options, Parents) -> ok | {error, Reason}
%%     Host = mod_pubsub:host() | ljid()
%%     Node = node()
%%     NodeType = nodeType()
%%     Owner = ljid()
%%     Options = list()
-spec(create_node/6 ::
      (
		    Host          :: host(), % hostPubsub | hostPEP()
		    NodeId        :: nodeId(),
		    Type          :: nodeType(),
		    JID           :: jidEntity(),
		    Options       :: [nodeOption()],
		    ParentNodeIds :: [] | [nodeId()])
      -> {'ok', NodeIdx::nodeIdx()}
	     | {'error', 'conflict' | 'forbidden'}
	    ).

create_node(Host, NodeId, Type, #jid{node = U, domain = S} = _JID, Options, ParentNodeIds) ->
    Owner = {U,S,undefined},
    case mnesia:read({pubsub_node, {Host, NodeId}}) of
	[] ->
	    ParentExists = case Host of
			       %% This is special case for PEP handling, PEP does not uses hierarchy
			       {_, _, _} -> true;
			       _ ->
				   case ParentNodeIds of
				       []           -> true;
				       [ParentNodeId | _] ->
					   case mnesia:read({pubsub_node, {Host, ParentNodeId}}) of
					       [#pubsub_node{owners = [{undefined, Host, undefined}]}] -> true;
					       [#pubsub_node{owners = Owners}] -> lists:member(Owner, Owners);
					       _ -> false
					   end;
				       _ -> false
				   end
			   end,
	    case ParentExists of
		true ->
		    NodeIdx = pubsub_index:new(node),
		    mnesia:write(
		      #pubsub_node{id = {Host, NodeId},
                                   idx = NodeIdx,
                                   parents = ParentNodeIds,
                                   type = Type,
                                   owners = [Owner],
                                   options = Options}),
                    {ok, NodeIdx};
    false -> %% Requesting entity is prohibited from creating nodes
        {error, 'forbidden'}
      end;
  _ -> %% Node already exists
      {error, 'conflict'}
    end.

%% @spec (Host, Node) -> [mod_pubsub:node()]
%%     Host = mod_pubsub:host() | ljid()
%%     Node = node()
-spec(delete_node/2 ::
(
  Host         :: host(), % hostPubsub | hostPEP()
  ParentNodeId :: nodeId())
    -> DeletedNodes :: [] | [Node::pubsubNode()]
).

delete_node(Host, ParentNodeId) ->
    DeletedNodes = get_subnodes_tree(Host, ParentNodeId),
    lists:foreach(fun(#pubsub_node{id = {_, NodeId}, idx = NodeIdx}) ->
      pubsub_index:free(node, NodeIdx),
      mnesia:delete({pubsub_node, {Host, NodeId}})
    end, DeletedNodes),
    DeletedNodes.
