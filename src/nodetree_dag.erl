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
%%% @author Brian Cully <bjc@kublai.com>
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

-module(nodetree_dag).

-author('bjc@kublai.com').

%% API
-export([init/3, terminate/2, options/0, set_node/1,
	 get_node/3, get_node/2, get_node/1, get_nodes/2,
	 get_nodes/1, get_parentnodes/3, get_parentnodes_tree/3,
	 get_subnodes/3, get_subnodes_tree/3, create_node/6,
	 delete_node/2]).

-include_lib("stdlib/include/qlc.hrl").

-include("ejabberd.hrl").
-include("logger.hrl").

-include("pubsub.hrl").

-include("jlib.hrl").

-behaviour(gen_pubsub_nodetree).

-define(DEFAULT_NODETYPE, leaf).

-define(DEFAULT_PARENTS, []).

-define(DEFAULT_CHILDREN, []).

-compile(export_all).

%%====================================================================
%% API
%%====================================================================
init(Host, ServerHost, Opts) ->
    nodetree_tree:init(Host, ServerHost, Opts).

terminate(Host, ServerHost) ->
    nodetree_tree:terminate(Host, ServerHost).

-spec(create_node/6 ::
(
  Key     :: mod_pubsub:hostPubsub(),
  NodeID  :: mod_pubsub:nodeId(),
  Type    :: binary(),
  Owner   :: jid(),
  Options :: mod_pubsub:nodeOptions(),
  Parents :: [mod_pubsub:nodeId()])
    -> {ok, NodeIdx::mod_pubsub:nodeIdx()}
     | {error, xmlel()}
).
create_node(Key, NodeID, Type, Owner, Options, Parents) ->
    OwnerJID = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    case find_node(Key, NodeID) of
      false ->
	  NodeIdx = pubsub_index:new(node),
	  N = #pubsub_node{nodeid = oid(Key, NodeID), id = NodeIdx,
			   type = Type, parents = Parents, owners = [OwnerJID],
			   options = Options},
	  case set_node(N) of
	    ok -> {ok, NodeIdx};
	    Other -> Other
	  end;
      _ -> {error, ?ERR_CONFLICT}
    end.

-spec(set_node/1 ::
(
  PubsubNode::mod_pubsub:pubsubNode())
    -> ok
    %%%
     | {error, xmlel()}
).
set_node(#pubsub_node{nodeid = {Key, _}, owners = Owners, options = Options} =
	     Node) ->
    Parents = find_opt(collection, ?DEFAULT_PARENTS, Options),
    case validate_parentage(Key, Owners, Parents) of
      true ->
	  mnesia:write(Node#pubsub_node{parents = Parents});
      Other -> Other
    end.

-spec(delete_node/2 ::
(
  Key    :: mod_pubsub:hostPubsub(),
  NodeID :: mod_pubsub:nodeId())
    -> [mod_pubsub:pubsubNode(),...]
    %%%
     | {error, xmlel()}
).
delete_node(Key, NodeID) ->
    case find_node(Key, NodeID) of
      false -> {error, ?ERR_ITEM_NOT_FOUND};
      Node ->
	  lists:foreach(fun (#pubsub_node{options = Opts} =
				 Child) ->
				NewOpts = remove_config_parent(NodeID, Opts),
				Parents = find_opt(collection, ?DEFAULT_PARENTS,
						   NewOpts),
				ok = mnesia:write(pubsub_node,
						  Child#pubsub_node{parents =
									Parents,
								    options =
									NewOpts},
						  write)
			end,
			get_subnodes(Key, NodeID)),
	  pubsub_index:free(node, Node#pubsub_node.id),
	  mnesia:delete_object(pubsub_node, Node, write),
	  [Node]
    end.

options() -> nodetree_tree:options().

get_node(Host, NodeID, _From) -> get_node(Host, NodeID).

-spec(get_node/2 ::
(
  Host :: mod_pubsub:hostPubsub(),
  NodeID :: mod_pubsub:nodeId())
    -> mod_pubsub:pubsubNode()
    %%%
     | {error, xmlel}
).
get_node(Host, NodeID) ->
    case find_node(Host, NodeID) of
      false -> {error, ?ERR_ITEM_NOT_FOUND};
      Node -> Node
    end.

-spec(get_node/1 ::
(
  NodeIdx::mod_pubsub:nodeIdx())
    -> mod_pubsub:pubsubNode()
     | {error, xmlel()}
).
get_node(NodeId) -> nodetree_tree:get_node(NodeId).

get_nodes(Key, From) ->
    nodetree_tree:get_nodes(Key, From).

-spec(get_nodes/1 ::
(
  Host::mod_pubsub:host())
    -> [mod_pubsub:pubsubNode()]
).
get_nodes(Key) -> nodetree_tree:get_nodes(Key).

-spec(get_parentnodes/3 ::
(
  Host   :: mod_pubsub:hostPubsub(),
  NodeID :: mod_pubsub:nodeId(),
  _From  :: _)
    -> [mod_pubsub:pubsubNode()]
    %%%
     | {error, xmlel()}
).
get_parentnodes(Host, NodeID, _From) ->
    case find_node(Host, NodeID) of
      false -> {error, ?ERR_ITEM_NOT_FOUND};
      #pubsub_node{parents = Parents} ->
	  Q = qlc:q([N
		     || #pubsub_node{nodeid = {NHost, NNode}} = N
			    <- mnesia:table(pubsub_node),
			Parent <- Parents, Host == NHost, Parent == NNode]),
	  qlc:e(Q)
    end.

get_parentnodes_tree(Host, NodeID, _From) ->
    Pred = fun (NID, #pubsub_node{nodeid = {_, NNodeID}}) ->
		   NID == NNodeID
	   end,
    Tr = fun (#pubsub_node{parents = Parents}) -> Parents
	 end,
    traversal_helper(Pred, Tr, Host, [NodeID]).

get_subnodes(Host, NodeID, _From) ->
    get_subnodes(Host, NodeID).

-spec(get_subnodes/2 ::
(
  Host   :: mod_pubsub:hostPubsub(),
  NodeId :: mod_pubsub:nodeId())
    -> [mod_pubsub:pubsubNode()]
).
get_subnodes(Host, <<>>) ->
    get_subnodes_helper(Host, <<>>);
get_subnodes(Host, NodeID) ->
    case find_node(Host, NodeID) of
      false -> {error, ?ERR_ITEM_NOT_FOUND};
      _ -> get_subnodes_helper(Host, NodeID)
    end.

-spec(get_subnodes_helper/2 ::
(
  Host :: mod_pubsub:hostPubsub(),
  NodeID :: mod_pubsub:nodeId())
    -> [mod_pubsub:pubsubNode()]
).
get_subnodes_helper(Host, NodeID) ->
    Q = qlc:q([Node
	       || #pubsub_node{nodeid = {NHost, _},
			       parents = Parents} =
		      Node
		      <- mnesia:table(pubsub_node),
		  Host == NHost, lists:member(NodeID, Parents)]),
    qlc:e(Q).

get_subnodes_tree(Host, NodeID, From) ->
    Pred = fun (NID, #pubsub_node{parents = Parents}) ->
		   lists:member(NID, Parents)
	   end,
    Tr = fun (#pubsub_node{nodeid = {_, N}}) -> [N] end,
    traversal_helper(Pred, Tr, 1, Host, [NodeID],
		     [{0, [get_node(Host, NodeID, From)]}]).

%%====================================================================
%% Internal functions
%%====================================================================
oid(Key, Name) -> {Key, Name}.

%% Key    = jlib:jid() | host()
%% NodeID = string()
-spec(find_node/2 ::
(
  Key :: mod_pubsub:hostPubsub(),
  NodeID :: mod_pubsub:nodeId())
    -> mod_pubsub:pubsubNode() | false
).
find_node(Key, NodeID) ->
    case mnesia:read(pubsub_node, oid(Key, NodeID), read) of
      [] -> false;
      [Node] -> Node
    end.

%% Key     = jlib:jid() | host()
%% Default = term()
%% Options = [{Key = atom(), Value = term()}]
find_opt(Key, Default, Options) ->
    case lists:keysearch(Key, 1, Options) of
      {value, {Key, Val}} -> Val;
      _ -> Default
    end.

-spec(traversal_helper/4 ::
(
  Pred   :: fun(),
  Tr     :: fun(),
  Host   :: mod_pubsub:hostPubsub(),
  NodeId :: [mod_pubsub:pubsubNode(),...])
    -> [{Depth::non_neg_integer(), Nodes::[mod_pubsub:pubsubNode(),...]}]
).

traversal_helper(Pred, Tr, Host, NodeIDs) ->
    traversal_helper(Pred, Tr, 0, Host, NodeIDs, []).

traversal_helper(_Pred, _Tr, _Depth, _Host, [], Acc) ->
    Acc;
traversal_helper(Pred, Tr, Depth, Host, NodeIDs, Acc) ->
    Q = qlc:q([Node
	       || #pubsub_node{nodeid = {NHost, _}} = Node
		      <- mnesia:table(pubsub_node),
		  NodeID <- NodeIDs, Host == NHost, Pred(NodeID, Node)]),
    Nodes = qlc:e(Q),
    IDs = lists:flatmap(Tr, Nodes),
    traversal_helper(Pred, Tr, Depth + 1, Host, IDs,
		     [{Depth, Nodes} | Acc]).

remove_config_parent(NodeID, Options) ->
    remove_config_parent(NodeID, Options, []).

remove_config_parent(_NodeID, [], Acc) ->
    lists:reverse(Acc);
remove_config_parent(NodeID, [{collection, Parents} | T], Acc) ->
    remove_config_parent(NodeID, T,
        [{collection, lists:delete(NodeID, Parents)} | Acc]);
remove_config_parent(NodeID, [H | T], Acc) ->
    remove_config_parent(NodeID, T, [H | Acc]).

-spec(validate_parentage/3 ::
(
  Key            :: mod_pubsub:hostPubsub(),
  Owners         :: [ljid(),...],
  Parent_NodeIds :: [mod_pubsub:nodeId()])
    -> true
    %%%
     | {error, xmlel()}
).
validate_parentage(_Key, _Owners, []) -> true;
validate_parentage(Key, Owners, [[] | T]) ->
    validate_parentage(Key, Owners, T);
validate_parentage(Key, Owners, [<<>> | T]) ->
    validate_parentage(Key, Owners, T);
validate_parentage(Key, Owners, [ParentID | T]) ->
    case find_node(Key, ParentID) of
      false -> {error, ?ERR_ITEM_NOT_FOUND};
      #pubsub_node{owners = POwners, options = POptions} ->
	  NodeType = find_opt(node_type, ?DEFAULT_NODETYPE, POptions),
	  MutualOwners = [O || O <- Owners, PO <- POwners, O == PO],
	  case {MutualOwners, NodeType} of
	    {[], _} -> {error, ?ERR_FORBIDDEN};
	    {_, collection} -> validate_parentage(Key, Owners, T);
	    {_, _} -> {error, ?ERR_NOT_ALLOWED}
	  end
    end.

%% @spec (Host) -> jid()
%%	Host = host()
%% @doc <p>Generate pubsub service JID.</p>
service_jid(Host) ->
    case Host of
      {U, S, _} -> jlib:make_jid(U, S, <<>>);
      _         -> jlib:make_jid(<<>>, Host, <<>>)
    end.
