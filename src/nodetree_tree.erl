%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%% 
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% 
%%%
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2015, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2015, ProcessOne.
%%%
%%%
%%% @copyright 2006-2015 ProcessOne
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

-include("jlib.hrl").

-behaviour(gen_pubsub_nodetree).

-export([init/3, terminate/2, options/0, set_node/1,
	 get_node/3, get_node/2, get_node/1, get_nodes/2,
	 get_nodes/1, get_parentnodes/3, get_parentnodes_tree/3,
	 get_subnodes/3, get_subnodes_tree/3, create_node/6,
	 delete_node/2]).

%% ================
%% API definition
%% ================

%% @spec (Host, ServerHost, Options) -> ok
%%	   Host       = string()
%%	   ServerHost = string()
%%	   Options    = [{atom(), term()}]
%% @doc <p>Called during pubsub modules initialisation. Any pubsub plugin must
%% implement this function. It can return anything.</p>
%% <p>This function is mainly used to trigger the setup task necessary for the
%% plugin. It can be used for example by the developer to create the specific
%% module database schema if it does not exists yet.</p>
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
%% @spec (Host, ServerHost) -> ok
%%	   Host       = string()
%%	   ServerHost = string()

%% @spec () -> Options
%%	   Options = [mod_pubsub:nodeOption()]
%% @doc Returns the default pubsub node tree options.
terminate(_Host, _ServerHost) -> ok.

options() -> [{virtual_tree, false}].

%% @spec (Node) -> ok | {error, Reason}
%%     Node   = mod_pubsub:pubsubNode()
%%  	 Reason = mod_pubsub:stanzaError()
-spec(set_node/1 ::
(
  Node::mod_pubsub:pubsubNode())
    -> ok
).
set_node(Node) when is_record(Node, pubsub_node) ->
    mnesia:write(Node).
%set_node(_) -> {error, ?ERR_INTERNAL_SERVER_ERROR}.

get_node(Host, Node, _From) -> get_node(Host, Node).

%% @spec (Host, NodeId) -> Node | {error, Reason}
%%     Host   = mod_pubsub:host()
%%     NodeId = mod_pubsub:nodeId()
%%     Node   = mod_pubsub:pubsubNode()
%%  	 Reason = mod_pubsub:stanzaError()
-spec(get_node/2 ::
(
  Host   :: mod_pubsub:host(),
  NodeId :: mod_pubsub:nodeId())
    -> mod_pubsub:pubsubNode()
     | {error, xmlel()}
).
get_node(Host, NodeId) ->
    case catch mnesia:read({pubsub_node, {Host, NodeId}}) of
      [Record] when is_record(Record, pubsub_node) -> Record;
      [] -> {error, ?ERR_ITEM_NOT_FOUND}
%      Error -> Error
    end.

-spec(get_node/1 ::
(
  NodeIdx::mod_pubsub:nodeIdx())
    -> mod_pubsub:pubsubNode()
     | {error, xmlel()}
).
get_node(NodeIdx) ->
    case catch mnesia:index_read(pubsub_node, NodeIdx, #pubsub_node.id) of
      [Record] when is_record(Record, pubsub_node) -> Record;
      [] -> {error, ?ERR_ITEM_NOT_FOUND}
%      Error -> Error
    end.

get_nodes(Host, _From) -> get_nodes(Host).

%% @spec (Host) -> Nodes | {error, Reason}
%%     Host   = mod_pubsub:host()
%%     Nodes  = [mod_pubsub:pubsubNode()]
%%  	 Reason = {aborted, atom()}
-spec(get_nodes/1 ::
(
  Host::mod_pubsub:host())
    -> [mod_pubsub:pubsubNode()]
).
get_nodes(Host) ->
    mnesia:match_object(#pubsub_node{nodeid = {Host, '_'}, _ = '_'}).

%% @spec (Host, Node, From) -> []
%%     Host   = mod_pubsub:host()
%%     NodeId = mod_pubsub:nodeId()
%%     From   = mod_pubsub:jid()
%% @doc <p>Default node tree does not handle parents, return empty list.</p>
get_parentnodes(_Host, _NodeId, _From) -> [].

%% @spec (Host, NodeId, From) -> [{Depth, Node}] | []
%%     Host   = mod_pubsub:host()
%%     NodeId = mod_pubsub:nodeId()
%%     From   = mod_pubsub:jid()
%%     Depth  = integer()
%%     Node   = mod_pubsub:pubsubNode()
%% @doc <p>Default node tree does not handle parents, return a list
%% containing just this node.</p>
-spec(get_parentnodes_tree/3 ::
(
  Host   :: mod_pubsub:host(),
  NodeId :: mod_pubsub:nodeId(),
  From   :: jid())
    -> [{0, [mod_pubsub:pubsubNode(),...]}]
).
get_parentnodes_tree(Host, NodeId, From) ->
    case get_node(Host, NodeId, From) of
      Node when is_record(Node, pubsub_node) -> [{0, [Node]}];
      _Error -> []
    end.

%% @spec (Host, NodeId, From) -> Nodes
%%     Host   = mod_pubsub:host()
%%     NodeId = mod_pubsub:nodeId()
%%     From   = mod_pubsub:jid()
%%     Nodes  = [mod_pubsub:pubsubNode()]
get_subnodes(Host, NodeId, _From) ->
    get_subnodes(Host, NodeId).

-spec(get_subnodes/2 ::
(
  Host   :: mod_pubsub:host(),
  NodeId :: mod_pubsub:nodeId())
    -> [mod_pubsub:pubsubNode()]
).
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

%% @spec (Host, NodeId) -> Nodes
%%     Host   = mod_pubsub:host()
%%     NodeId = mod_pubsub:nodeId()
%%     Nodes  = [] | [mod_pubsub:pubsubNode()]
-spec(get_subnodes_tree/2 ::
(
  Host :: mod_pubsub:host(),
  NodeId :: mod_pubsub:nodeId())
    -> [mod_pubsub:pubsubNode()]
).
get_subnodes_tree(Host, NodeId) ->
    case get_node(Host, NodeId) of
      {error, _} -> [];
      Rec ->
	  BasePlugin = jlib:binary_to_atom(<<"node_",
					       (Rec#pubsub_node.type)/binary>>),
	  BasePath = BasePlugin:node_to_path(NodeId),
	  mnesia:foldl(fun (#pubsub_node{nodeid = {H, N}} = R,
			    Acc) ->
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

%% @spec (Host, NodeId, Type, Owner, Options, Parents) ->
%%   {ok, NodeIdx} | {error, Reason}
%%     Host     = mod_pubsub:host()
%%     NodeId   = mod_pubsub:nodeId()
%%     Type     = mod_pubsub:nodeType()
%%     Owner    = mod_pubsub:jid()
%%     Options  = [mod_pubsub:nodeOption()]
%%     Parents  = [] | [mod_pubsub:nodeId()]
%%     NodeIdx  = mod_pubsub:nodeIdx()
%%     Reason   = mod_pubsub:stanzaError()
-spec(create_node/6 ::
(
  Host    :: mod_pubsub:host(),
  NodeId  :: mod_pubsub:nodeId(),
  Type    :: binary(), 
  Owner   :: jid(),
  Options :: mod_pubsub:nodeOptions(),
  Parents :: [mod_pubsub:nodeId()])
    -> {ok, NodeIdx::mod_pubsub:nodeIdx()}
    %%%
     | {error, xmlel()}
).
create_node(Host, NodeId, Type, Owner, Options, Parents) ->
    BJID = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    case catch mnesia:read({pubsub_node, {Host, NodeId}}) of
      [] ->
	  ParentExists = case Host of
			   {_U, _S, _R} ->
			       %% This is special case for PEP handling
			       %% PEP does not uses hierarchy
			       true;
			   _ ->
			       case Parents of
				 [] -> true;
				 [Parent | _] ->
				     case catch mnesia:read({pubsub_node,
							     {Host, Parent}})
					 of
				       [#pubsub_node{owners =
							 [{[], Host, []}]}] ->
					   true;
				       [#pubsub_node{owners = Owners}] ->
					   lists:member(BJID, Owners);
				       _ -> false
				     end;
				 _ -> false
			       end
			 end,
	  case ParentExists of
	    true ->
		NodeIdx = pubsub_index:new(node),
		mnesia:write(#pubsub_node{nodeid = {Host, NodeId},
					  id = NodeIdx, parents = Parents,
					  type = Type, owners = [BJID],
					  options = Options}),
		{ok, NodeIdx};
	    false -> {error, ?ERR_FORBIDDEN}
	  end;
      _ -> {error, ?ERR_CONFLICT}
    end.

%% @spec (Host, NodeId) -> Removed
%%     Host    = mod_pubsub:host()
%%     NodeId  = mod_pubsub:nodeId()
%%     Removed = [mod_pubsub:pubsubNode()]
-spec(delete_node/2 ::
(
  Host :: mod_pubsub:host(),
  NodeId :: mod_pubsub:nodeId())
    -> [mod_pubsub:pubsubNode(),...]
).
delete_node(Host, NodeId) ->
    Removed = get_subnodes_tree(Host, NodeId),
    lists:foreach(fun (#pubsub_node{nodeid = {_, SubNodeId}, id = SubNodeIdx}) ->
			  pubsub_index:free(node, SubNodeIdx),
			  mnesia:delete({pubsub_node, {Host, SubNodeId}})
		  end,
		  Removed),
    Removed.
