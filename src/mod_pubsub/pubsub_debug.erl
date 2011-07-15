-module(pubsub_debug).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").

-compile(export_all).

nodeid(Host, Node) ->
    case mnesia:dirty_read({pubsub_node, {Host, Node}}) of
    [N] -> nodeid(N);
    _ -> 0
    end.
nodeid(N) -> N#pubsub_node.id.
nodeids() -> [nodeid(Host, Node) || {Host, Node} <- mnesia:dirty_all_keys(pubsub_node)].
nodeids_by_type(Type) -> [nodeid(N) || N <- mnesia:dirty_match_object(#pubsub_node{type=Type, _='_'})].
nodeids_by_option(Key, Value) -> [nodeid(N) || N <- mnesia:dirty_match_object(#pubsub_node{_='_'}), lists:member({Key, Value}, N#pubsub_node.options)].
nodeids_by_owner(JID) -> [nodeid(N) || N <- mnesia:dirty_match_object(#pubsub_node{_='_'}), lists:member(JID, N#pubsub_node.owners)].
nodes_by_id(I) -> mnesia:dirty_match_object(#pubsub_node{id=I, _='_'}).
nodes() -> [element(2, element(2, N)) || N <- mnesia:dirty_match_object(#pubsub_node{_='_'})].

state(JID, NodeId) ->
    case mnesia:dirty_read({pubsub_state, {JID, NodeId}}) of
    [S] -> S;
    _ -> undefined
    end.
states(NodeId) -> mnesia:dirty_index_read(pubsub_state, NodeId, #pubsub_state.nodeidx).
stateid(S) -> element(1, S#pubsub_state.stateid).
stateids(NodeId) -> [stateid(S) || S <- states(NodeId)].
states_by_jid(JID) -> mnesia:dirty_match_object(#pubsub_state{stateid={JID, '_'}, _='_'}).

item(ItemId, NodeId) ->
    case mnesia:dirty_read({pubsub_item, {ItemId, NodeId}}) of
    [I] -> I;
    _ -> undefined
    end.
items(NodeId) -> mnesia:dirty_index_read(pubsub_item, NodeId, #pubsub_item.nodeidx).
itemid(I) -> element(1, I#pubsub_item.itemid).
itemids(NodeId) -> [itemid(I) || I <- items(NodeId)].
items_by_id(ItemId) -> mnesia:dirty_match_object(#pubsub_item{itemid={ItemId, '_'}, _='_'}).

affiliated(NodeId) -> [stateid(S) || S <- states(NodeId), S#pubsub_state.affiliation=/=none].
subscribed(NodeId) -> [stateid(S) || S <- states(NodeId), S#pubsub_state.subscriptions=/=[]].
%subscribed(NodeId) -> [stateid(S) || S <- states(NodeId), S#pubsub_state.subscription=/=none]. %% old record
owners(NodeId) -> [stateid(S) || S <- states(NodeId), S#pubsub_state.affiliation==owner].

orphan_items(NodeId) ->
    itemids(NodeId) -- lists:foldl(fun(S, A) -> A++S#pubsub_state.items end, [], states(NodeId)).
newer_items(NodeId, Seconds) ->
    Now = calendar:universal_time(),
    Oldest = calendar:seconds_to_daystime(Seconds),
    [itemid(I) || I <- items(NodeId), calendar:time_difference(calendar:now_to_universal_time(element(1, I#pubsub_item.modification)), Now) < Oldest].
older_items(NodeId, Seconds) ->
    Now = calendar:universal_time(),
    Oldest = calendar:seconds_to_daystime(Seconds),
    [itemid(I) || I <- items(NodeId), calendar:time_difference(calendar:now_to_universal_time(element(1, I#pubsub_item.modification)), Now) > Oldest].

orphan_nodes() -> [I || I <- nodeids(), owners(I)==[]].
duplicated_nodes() -> L = nodeids(), lists:usort(L -- lists:seq(1, lists:max(L))).
node_options(NodeId) ->
    [N] = mnesia:dirty_match_object(#pubsub_node{id=NodeId, _='_'}),
    N#pubsub_node.options.
update_node_options(Key, Value, NodeId) ->
    [N] = mnesia:dirty_match_object(#pubsub_node{id=NodeId, _='_'}),
    NewOptions = lists:keyreplace(Key, 1, N#pubsub_node.options, {Key, Value}),
    mnesia:dirty_write(N#pubsub_node{options = NewOptions}).

check() ->
    mnesia:transaction(fun() ->
	case mnesia:read({pubsub_index, node}) of
	[Idx] ->
	    Free = Idx#pubsub_index.free,
	    Last = Idx#pubsub_index.last,
	    Allocated = lists:seq(1, Last) -- Free,
	    NodeIds = mnesia:foldl(fun(N,A) -> [nodeid(N)|A] end, [], pubsub_node),
	    StateIds = lists:usort(mnesia:foldl(fun(S,A) -> [element(2, S#pubsub_state.stateid)|A] end, [], pubsub_state)),
	    ItemIds = lists:usort(mnesia:foldl(fun(I,A) -> [element(2, I#pubsub_item.itemid)|A] end, [], pubsub_item)),
	    BadNodeIds = NodeIds -- Allocated,
	    BadStateIds = StateIds -- NodeIds,
	    BadItemIds = ItemIds -- NodeIds,
	    Lost = Allocated -- NodeIds,
	    [{bad_nodes, [N#pubsub_node.nodeid || N <- lists:flatten([mnesia:match_object(#pubsub_node{id=I, _='_'}) || I <- BadNodeIds])]},
	     {bad_states, lists:foldl(fun(N,A) -> A++[{I,N} || I <- stateids(N)] end, [], BadStateIds)},
	     {bad_items, lists:foldl(fun(N,A) -> A++[{I,N} || I <- itemids(N)] end, [], BadItemIds)},
	     {lost_idx, Lost},
	     {orphaned, [I || I <- NodeIds, owners(I)==[]]},
	     {duplicated, lists:usort(NodeIds -- lists:seq(1, lists:max(NodeIds)))}];
	_ ->
	    no_index
	end
    end).

rebuild_index() ->
    mnesia:transaction(fun() ->
	NodeIds = mnesia:foldl(fun(N,A) -> [nodeid(N)|A] end, [], pubsub_node),
	Last = lists:max(NodeIds),
	Free = lists:seq(1, Last) -- NodeIds,
	mnesia:write(#pubsub_index{index = node, last = Last, free = Free})
    end).

pep_subscriptions(LUser, LServer, LResource) ->
    case ejabberd_sm:get_session_pid({LUser, LServer, LResource}) of
    C2SPid when is_pid(C2SPid) ->
        case catch ejabberd_c2s:get_subscribed(C2SPid) of
        Contacts when is_list(Contacts) ->
            lists:map(fun({U, S, _}) ->
                io_lib:format("~s@~s", [U, S])
            end, Contacts);
        _ ->
            []
        end;
    _ ->
        []
    end.
