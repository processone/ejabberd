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
%%% @author Brian Cully <bjc@kublai.com>
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

-module(nodetree_dag).
-behaviour(gen_pubsub_nodetree).
-author('bjc@kublai.com').

-include_lib("stdlib/include/qlc.hrl").

-include("pubsub.hrl").
-include("jlib.hrl").

-export([init/3, terminate/2, options/0, set_node/1,
    get_node/3, get_node/2, get_node/1, get_nodes/2,
    get_nodes/1, get_parentnodes/3, get_parentnodes_tree/3,
    get_subnodes/3, get_subnodes_tree/3, create_node/6,
    delete_node/2]).


-define(DEFAULT_NODETYPE, leaf).
-define(DEFAULT_PARENTS, []).
-define(DEFAULT_CHILDREN, []).

init(Host, ServerHost, Opts) ->
    nodetree_tree:init(Host, ServerHost, Opts).

terminate(Host, ServerHost) ->
    nodetree_tree:terminate(Host, ServerHost).

set_node(#pubsub_node{nodeid = {Key, _}, owners = Owners, options = Options} = Node) ->
    Parents = find_opt(collection, ?DEFAULT_PARENTS, Options),
    case validate_parentage(Key, Owners, Parents) of
	true -> mnesia:write(Node#pubsub_node{parents = Parents});
	Other -> Other
    end.

create_node(Key, Node, Type, Owner, Options, Parents) ->
    OwnerJID = jid:tolower(jid:remove_resource(Owner)),
    case find_node(Key, Node) of
	false ->
	    Nidx = pubsub_index:new(node),
	    N = #pubsub_node{nodeid = oid(Key, Node), id = Nidx,
		    type = Type, parents = Parents, owners = [OwnerJID],
		    options = Options},
	    case set_node(N) of
		ok -> {ok, Nidx};
		Other -> Other
	    end;
	_ ->
	    {error, ?ERR_CONFLICT}
    end.

delete_node(Key, Node) ->
    case find_node(Key, Node) of
	false ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	Record ->
	    lists:foreach(fun (#pubsub_node{options = Opts} = Child) ->
			NewOpts = remove_config_parent(Node, Opts),
			Parents = find_opt(collection, ?DEFAULT_PARENTS, NewOpts),
			ok = mnesia:write(pubsub_node,
				Child#pubsub_node{parents = Parents,
				    options = NewOpts},
				write)
		end,
		get_subnodes(Key, Node)),
	    pubsub_index:free(node, Record#pubsub_node.id),
	    mnesia:delete_object(pubsub_node, Record, write),
	    [Record]
    end.

options() ->
    nodetree_tree:options().

get_node(Host, Node, _From) ->
    get_node(Host, Node).

get_node(Host, Node) ->
    case find_node(Host, Node) of
	false -> {error, ?ERR_ITEM_NOT_FOUND};
	Record -> Record
    end.

get_node(Node) ->
    nodetree_tree:get_node(Node).

get_nodes(Key, From) ->
    nodetree_tree:get_nodes(Key, From).

get_nodes(Key) ->
    nodetree_tree:get_nodes(Key).

get_parentnodes(Host, Node, _From) ->
    case find_node(Host, Node) of
	false ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	#pubsub_node{parents = Parents} ->
	    Q = qlc:q([N
			|| #pubsub_node{nodeid = {NHost, NNode}} = N
			    <- mnesia:table(pubsub_node),
			    Parent <- Parents, Host == NHost, Parent == NNode]),
	    qlc:e(Q)
    end.

get_parentnodes_tree(Host, Node, _From) ->
    Pred = fun (NID, #pubsub_node{nodeid = {_, NNode}}) ->
	    NID == NNode
    end,
    Tr = fun (#pubsub_node{parents = Parents}) -> Parents
    end,
    traversal_helper(Pred, Tr, Host, [Node]).

get_subnodes(Host, Node, _From) ->
    get_subnodes(Host, Node).

get_subnodes(Host, <<>>) ->
    get_subnodes_helper(Host, <<>>);
get_subnodes(Host, Node) ->
    case find_node(Host, Node) of
	false -> {error, ?ERR_ITEM_NOT_FOUND};
	_ -> get_subnodes_helper(Host, Node)
    end.

get_subnodes_helper(Host, Node) ->
    Q = qlc:q([N
		|| #pubsub_node{nodeid = {NHost, _},
			parents = Parents} =
		    N
		    <- mnesia:table(pubsub_node),
		    Host == NHost, lists:member(Node, Parents)]),
    qlc:e(Q).

get_subnodes_tree(Host, Node, From) ->
    Pred = fun (NID, #pubsub_node{parents = Parents}) ->
	    lists:member(NID, Parents)
    end,
    Tr = fun (#pubsub_node{nodeid = {_, N}}) -> [N] end,
    traversal_helper(Pred, Tr, 1, Host, [Node],
	[{0, [get_node(Host, Node, From)]}]).

%%====================================================================
%% Internal functions
%%====================================================================
oid(Key, Name) -> {Key, Name}.

%% Key    = jlib:jid() | host()
%% Node = string()
-spec(find_node/2 ::
    (
	Key :: mod_pubsub:hostPubsub(),
	Node :: mod_pubsub:nodeId())
    -> mod_pubsub:pubsubNode() | false
    ).
find_node(Key, Node) ->
    case mnesia:read(pubsub_node, oid(Key, Node), read) of
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
	Pred    :: fun(),
		    Tr      :: fun(),
				Host    :: mod_pubsub:hostPubsub(),
				Nodes :: [mod_pubsub:nodeId(),...])
				-> [{Depth::non_neg_integer(), Nodes::[mod_pubsub:pubsubNode(),...]}]
				).
traversal_helper(Pred, Tr, Host, Nodes) ->
    traversal_helper(Pred, Tr, 0, Host, Nodes, []).

traversal_helper(_Pred, _Tr, _Depth, _Host, [], Acc) ->
    Acc;
traversal_helper(Pred, Tr, Depth, Host, Nodes, Acc) ->
    Q = qlc:q([N
		|| #pubsub_node{nodeid = {NHost, _}} = N
		    <- mnesia:table(pubsub_node),
		    Node <- Nodes, Host == NHost, Pred(Node, N)]),
    Nodes = qlc:e(Q),
    IDs = lists:flatmap(Tr, Nodes),
    traversal_helper(Pred, Tr, Depth + 1, Host, IDs, [{Depth, Nodes} | Acc]).

remove_config_parent(Node, Options) ->
    remove_config_parent(Node, Options, []).

remove_config_parent(_Node, [], Acc) ->
    lists:reverse(Acc);
remove_config_parent(Node, [{collection, Parents} | T], Acc) ->
    remove_config_parent(Node, T, [{collection, lists:delete(Node, Parents)} | Acc]);
remove_config_parent(Node, [H | T], Acc) ->
    remove_config_parent(Node, T, [H | Acc]).

-spec(validate_parentage/3 ::
    (
	Key            :: mod_pubsub:hostPubsub(),
	Owners         :: [ljid(),...],
	Parent_Nodes :: [mod_pubsub:nodeId()])
    -> true
    %%%
    | {error, xmlel()}
    ).
validate_parentage(_Key, _Owners, []) ->
    true;
validate_parentage(Key, Owners, [[] | T]) ->
    validate_parentage(Key, Owners, T);
validate_parentage(Key, Owners, [<<>> | T]) ->
    validate_parentage(Key, Owners, T);
validate_parentage(Key, Owners, [ParentID | T]) ->
    case find_node(Key, ParentID) of
	false ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	#pubsub_node{owners = POwners, options = POptions} ->
	    NodeType = find_opt(node_type, ?DEFAULT_NODETYPE, POptions),
	    MutualOwners = [O || O <- Owners, PO <- POwners, O == PO],
	    case {MutualOwners, NodeType} of
		{[], _} -> {error, ?ERR_FORBIDDEN};
		{_, collection} -> validate_parentage(Key, Owners, T);
		{_, _} -> {error, ?ERR_NOT_ALLOWED}
	    end
    end.
