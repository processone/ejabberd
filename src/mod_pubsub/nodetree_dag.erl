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
	 delete_node/2]).

-include_lib("stdlib/include/qlc.hrl").

-include("ejabberd.hrl").
-include("pubsub.hrl").

-behaviour(gen_pubsub_nodetree).

-define(DEFAULT_NODETYPE, leaf).
-define(DEFAULT_PARENTS, []).
-define(DEFAULT_CHILDREN, []).

-compile(export_all).

%%====================================================================
%% API
%%====================================================================
-spec(init/3 ::
      (
	     Host       :: string(),
	     ServerHost :: string(),
	     Opts       :: [{Key::atom(), Value::term()}])
      -> 'ok'
	    ).

init(Host, ServerHost, Opts) ->
    nodetree_tree:init(Host, ServerHost, Opts).


-spec(terminate/2 ::
      (
		  Host       :: string(),
		  ServerHost :: string())
      -> 'ok'
	    ).

terminate(Host, ServerHost) ->
    nodetree_tree:terminate(Host, ServerHost).


-spec(create_node/6 ::
      (
		    Host          :: hostPubsub(),
		    NodeId        :: nodeId(),
		    Type          :: nodeType(),
		    JID           :: jidEntity(),
		    Options       :: [nodeOption()],
		    ParentNodeIds :: [] | [nodeId()])
      -> {'ok', NodeIdx::nodeIdx()} | {'error', _}
	    ).

create_node(Host, NodeId, Type, #jid{node = U, domain = S} = _JID, Options, ParentNodeIds) ->
    case find_node(Host, NodeId) of
	false ->
	    NodeIdx = pubsub_index:new(node),
	    Node = #pubsub_node{id = {Host, NodeId},
			     idx = NodeIdx,
			     type = Type,
			     parents = ParentNodeIds,
			     owners = [_Owner = {U,S,undefined}],
			     options = Options},
	    case set_node(Node) of
		ok    -> {ok, NodeIdx};
		Other -> Other
	    end;
	_ ->
	    {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'conflict')}
    end.


-spec(set_node/1 ::
      (
		    Node :: pubsubNode())
      -> 'ok' | {'error', _}
	    ).

set_node(#pubsub_node{id = {Host, _},
		      owners  = Owners,
		      options = Options} = Node) ->
    ParentNodeIds = find_opt('collection', ?DEFAULT_PARENTS,  Options),
    case validate_parentage(Host, Owners, ParentNodeIds) of
	true ->
	    %% Update parents whenever the config changes.
	    mnesia:write(Node#pubsub_node{parents = ParentNodeIds});
	Other ->
	    Other
    end.


-spec(delete_node/2 ::
      (
		    Host   :: hostPubsub(),
		    NodeId :: nodeId())
      -> [pubsubNode()] | {'error', _}
	    ).

delete_node(Host, NodeId) ->
    case find_node(Host, NodeId) of
	false ->
	    {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'item-not-found')};
	ParentNode ->
	    %% Find all of N's children, update their configs to
	    %% remove N from the collection setting.
	    lists:foreach(fun (#pubsub_node{options = Options} = Node) ->
				  NewOptions = remove_config_parent(NodeId, Options),
				  Parents = find_opt('collection', ?DEFAULT_PARENTS, NewOptions),
				  ok = mnesia:write(pubsub_node,
						    Node#pubsub_node{
						      parents = Parents,
						      options = NewOptions},
						    write)
			  end, get_subnodes(Host, NodeId)),

	    %% Remove and return the requested node.
	    pubsub_index:free(node, ParentNode#pubsub_node.idx),
	    mnesia:delete_object(pubsub_node, ParentNode, write),
	    [ParentNode]
    end.


-spec(options/0 :: () -> Options::[Option::nodeOption()]).
                        %Options::[{'virtual_tree', 'false'}])

options() -> nodetree_tree:options().


-spec(get_node/3 ::
      (
		 Host   :: hostPubsub(),
		 NodeId :: nodeId(),
		 JID    :: jidEntity())
      -> pubsubNode() | {'error', _}
	    ).

get_node(Host, NodeId, _JID) ->
    get_node(Host, NodeId).


-spec(get_node/2 ::
      (
		 Host   :: hostPubsub(),
		 NodeId :: nodeId())
      -> pubsubNode() | {'error', _}
	    ).

get_node(Host, NodeId) ->
    case find_node(Host, NodeId) of
	false -> {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'item-not-found')};
	Node -> Node
    end.


-spec(get_node/1 ::
      (
		 NodeIdx :: nodeIdx())
      -> pubsubNode() | {'error', 'item-not-found'}
	    ).

get_node(NodeIdx) ->
    nodetree_tree:get_node(NodeIdx).


-spec(get_nodes/2 ::
      (
		  Host :: hostPubsub(),
		  JID  :: jidEntity())
      -> Nodes :: [] | [Node::pubsubNode()]
	    ).

get_nodes(Host, JID) ->
    nodetree_tree:get_nodes(Host, JID).


-spec(get_nodes/1 ::
      (
		  Host :: hostPubsub())
      -> Nodes :: [] | [Node::pubsubNode()]
	    ).

get_nodes(Host) ->
    nodetree_tree:get_nodes(Host).


-spec(get_parentnodes/3 ::
      (
			Host   :: hostPubsub(),
			NodeId :: nodeId(),
			JID    :: jidEntity())
      -> ParentNodes :: [] | [ParentNode::pubsubNode()]
	    ).

get_parentnodes(Host, NodeId, _JID) ->
    case find_node(Host, NodeId) of
	false -> {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'item-not-found')};
	#pubsub_node{parents = ParentNodeIds} ->
	    Q = qlc:q([Node || #pubsub_node{id = {NHost, NNodeId}} = Node
	        <- mnesia:table(pubsub_node),
			    ParentNodeId <- ParentNodeIds,
			    Host == NHost,
			    ParentNodeId == NNodeId]),
	    qlc:e(Q)
    end.


-spec(get_parentnodes_tree/3 ::
      (
			     Host   :: hostPubsub(),
			     NodeId :: nodeId(),
			     JID    :: jidEntity())
      -> [] | [{Depth::integer(), Nodes :: [] | [Node::pubsubNode()]}]
	    ).

get_parentnodes_tree(Host, NodeId, _JID) ->
    Pred = fun (Name, #pubsub_node{id = {_, NodeName}}) -> Name == NodeName end,
    Tr = fun (#pubsub_node{parents = Parents}) -> Parents end,
    traversal_helper(Pred, Tr, Host, [NodeId]).


-spec(get_subnodes/3 ::
      (
			ParentNodeHost :: hostPubsub(),
			ParentNodeId   :: nodeId(),
			JID            :: jidEntity())
      -> [] | [Node::pubsubNode()] | {'error', _}
	    ).

get_subnodes(Host, Node, _JID) ->
    get_subnodes(Host, Node).


-spec(get_subnodes/2 ::
      (
			ParentNodeHost :: hostPubsub(),
			ParentNodeId   :: nodeId())
      -> [] | [Node::pubsubNode()] | {'error', _}
	    ).

get_subnodes(ParentNodeHost, <<>>) ->
    get_subnodes_helper(ParentNodeHost, <<>>);
get_subnodes(ParentNodeHost, ParentNodeId) ->
    case find_node(ParentNodeHost, ParentNodeId) of
	false -> {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'item-not-found')};
	_ -> get_subnodes_helper(ParentNodeHost, ParentNodeId)
    end.


-spec(get_subnodes_helper/2 ::
      (
			ParentNodeHost :: hostPubsub(),
			ParentNodeId   :: nodeId())
      -> SubNodes :: [] | [Node::pubsubNode()]
	    ).

get_subnodes_helper(ParentNodeHost, ParentNodeId) ->
    Q = qlc:q([Node || #pubsub_node{id = {Host, _},
		parents = ParentNodeIds} = Node <- mnesia:table(pubsub_node),
		ParentNodeHost == Host,
		lists:member(ParentNodeId, ParentNodeIds)]),
    qlc:e(Q).


-spec(get_subnodes_tree/3 ::
      (
			ParentNodeHost :: hostPubsub(),
			ParentNodeId   :: nodeId(),
			JID            :: jidEntity())
      -> [] | [{Depth::integer(), Nodes :: [] | [Node::pubsubNode()]}]
	    ).

get_subnodes_tree(Host, ParentNodeId, JID) ->
    Pred = fun(NodeId, #pubsub_node{parents = ParentNodeIds}) ->
		   lists:member(NodeId, ParentNodeIds)
	   end,
    Tr = fun (#pubsub_node{id = {_, NodeId}}) -> [NodeId] end,
    traversal_helper(Pred, Tr, 1, Host, [ParentNodeId],
                     [{0, [get_node(Host, ParentNodeId, JID)]}]).

%%====================================================================
%% Internal functions
%%====================================================================
oid(Key, Name) -> {Key, Name}.

%% Key    = jlib:jid() | host()
%% Node = string()
-spec(find_node/2 ::
      (
		 Host   :: hostPubsub(),
		 NodeId :: nodeId())
      -> pubsubNode() | 'false'
	    ).

find_node(Host, NodeId) ->
    case mnesia:read({pubsub_node, {Host, NodeId}}) of
	[]     -> false;
	[Node] -> Node
    end.

%% Key     = jlib:jid() | host()
%% Default = term()
%% Options = [{Key = atom(), Value = term()}]
-spec(find_opt/3 ::
      (
		 Key     :: atom(),
		 Default :: term(),
		 Options :: [Option::nodeOption()])
      -> Value::term()
	    ).

find_opt(Key, Default, Options) ->
    case lists:keysearch(Key, 1, Options) of
	{value, {Key, Value}} -> Value;
	_		   -> Default
    end.


-spec(traversal_helper/4 ::
      (
			Pred           :: fun(),
			Tr             :: fun(),
			ParentNodeHost :: hostPubsub(),
			ParentNodeIds  :: [] | [ParentNodeId::nodeId()])
      -> [] | [{Depth::integer(), Nodes :: [] | [Node::pubsubNode()]}]
	    ).

traversal_helper(Pred, Tr, ParentNodeHost, ParentNodeIds) ->
    traversal_helper(Pred, Tr, 0, ParentNodeHost, ParentNodeIds, []).


-spec(traversal_helper/6 ::
      (
			Pred           :: fun(),
			Tr             :: fun(),
			Depth          :: integer(),
			ParentNodeHost :: hostPubsub(),
			ParentNodeIds  :: [] | [ParentNodeId::nodeId()],
			Acc            :: [] | [{Depth::integer(), Nodes :: [] | [Node::pubsubNode()]}])
      -> [] | [{Depth::integer(), Nodes :: [] | [Node::pubsubNode()]}]
	    ).

traversal_helper(_Pred, _Tr, _Depth, _Host, [], Acc) ->
    Acc;
traversal_helper(Pred, Tr, Depth, ParentNodeHost, ParentNodeIds, Acc) ->
    Q = qlc:q([Node || #pubsub_node{id = {Host, _}} = Node <- mnesia:table(pubsub_node),
		       ParentNodeId <- ParentNodeIds,
		       ParentNodeHost == Host,
		       Pred(ParentNodeId, Node)]),
    Nodes = qlc:e(Q),
    Ids = lists:flatmap(Tr, Nodes),
    traversal_helper(Pred, Tr, Depth + 1, ParentNodeHost, Ids, [{Depth, Nodes} | Acc]).


-spec(remove_config_parent/2 ::
      (
			NodeId  :: nodeId(),
			Options :: [Option::nodeOption()])
      -> [Option::nodeOption()]
	    ).

remove_config_parent(NodeId, Options) ->
    remove_config_parent(NodeId, Options, []).


-spec(remove_config_parent/3 ::
      (
			NodeId  :: nodeId(),
			Options :: [] | [Option::nodeOption()],
			Acc     :: [Option::nodeOption()])
      -> [Option::nodeOption()]
	    ).

remove_config_parent(_NodeId, [], Acc) ->
    lists:reverse(Acc);
remove_config_parent(NodeId, [{'collection', ParentNodeIds} | Options], Acc) ->
    remove_config_parent(NodeId, Options,
			 [{'collection', lists:delete(NodeId, ParentNodeIds)} | Acc]);
remove_config_parent(NodeId, [Option | Options], Acc) ->
    remove_config_parent(NodeId, Options, [Option | Acc]).


-spec(validate_parentage/3 ::
      (
			Host          :: hostPubsub(),
			Owners        :: [Owner::bareUsr()],
			ParentNodeIds :: [] | [ParentNodeId::nodeId()] | [ParentNodeId :: nodeId() | []])
      -> 'true' | {'error', _}
	    ).

validate_parentage(_Host, _Owners, []) ->
    true;
validate_parentage(Host, Owners, [[] | ParentNodeIds]) ->
    validate_parentage(Host, Owners, ParentNodeIds);
validate_parentage(Host, Owners, [<<>> | ParentNodeIds]) ->
    validate_parentage(Host, Owners, ParentNodeIds);
validate_parentage(Host, Owners, [ParentNodeId | ParentNodeIds]) ->
    case find_node(Host, ParentNodeId) of
	false -> {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'item_not_found')};
	#pubsub_node{owners = ParentNodeOwners, options = Options} ->
	    NodeType = find_opt('node_type', ?DEFAULT_NODETYPE, Options),
	    MutualOwners = [Owner || Owner <- Owners, ParentNodeOwner <- ParentNodeOwners,
				 Owner == ParentNodeOwner],
	    case {MutualOwners, NodeType} of
		{[], _}	 -> {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'forbidden')};
		{_, 'collection'} -> validate_parentage(Host, Owners, ParentNodeIds);
		{_, _} -> {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'not-allowed')}
	    end
    end.

%% @spec (Host) -> jid()
%%	Host = host()
%% @doc <p>Generate pubsub service JID.</p>
-spec(service_jid/1 ::
      (
		    Host :: hostPubsub())
      -> ServiceJID :: jidContact() | jidComponent() %% should only return jidContact()
	    ).

service_jid(Host) ->
    case Host of
	{U,S,_} -> exmpp_jid:make(U, S);
	_       -> exmpp_jid:make(Host)
    end.
