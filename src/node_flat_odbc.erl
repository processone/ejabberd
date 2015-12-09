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
%%% @copyright 2006-2015 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @doc The module <strong>{@module}</strong> is the default PubSub plugin.
%%% <p>It is used as a default for all unknown PubSub node type.  It can serve
%%% as a developer basis and reference to build its own custom pubsub node
%%% types.</p>
%%% <p>PubSub plugin nodes are using the {@link gen_node} behaviour.</p>

-module(node_flat_odbc).
-behaviour(gen_pubsub_node).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").
-include("jlib.hrl").

-export([init/3, terminate/2, options/0, features/0,
    create_node_permission/6, create_node/2, delete_node/1,
    purge_node/2, subscribe_node/8, unsubscribe_node/4,
    publish_item/6, delete_item/4, remove_extra_items/3,
    get_entity_affiliations/2, get_node_affiliations/1,
    get_affiliation/2, set_affiliation/3,
    get_entity_subscriptions/2, get_node_subscriptions/1,
    get_subscriptions/2, set_subscriptions/4,
    get_pending_nodes/2, get_states/1, get_state/2,
    set_state/1, get_items/7, get_items/3, get_item/7,
    get_item/2, set_item/1, get_item_name/3, node_to_path/1,
    path_to_node/1,
    get_entity_subscriptions_for_send_last/2, get_last_items/3]).

-export([decode_jid/1, encode_jid/1,
    decode_affiliation/1, decode_subscriptions/1,
    encode_affiliation/1, encode_subscriptions/1,
    encode_host/1]).

init(_Host, _ServerHost, _Opts) ->
    %%pubsub_subscription_odbc:init(),
    ok.

terminate(_Host, _ServerHost) ->
    ok.

options() ->
    [{odbc, true}, {rsm, true} | node_flat:options()].

features() ->
    [<<"rsm">> | node_flat:features()].

create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
    node_flat:create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access).

create_node(Nidx, Owner) ->
    {_U, _S, _R} = OwnerKey = jid:tolower(jid:remove_resource(Owner)),
    State = #pubsub_state{stateid = {OwnerKey, Nidx}, affiliation = owner},
    catch ejabberd_odbc:sql_query_t([<<"insert into pubsub_state(nodeid, jid, affiliation, subscriptions) "
		"values(">>, state_to_raw(Nidx, State), <<");">>]),
    {result, {default, broadcast}}.

delete_node(Nodes) ->
    Reply = lists:map(fun (#pubsub_node{id = Nidx} = PubsubNode) ->
		    Subscriptions = case catch
			ejabberd_odbc:sql_query_t([<<"select jid, subscriptions "
				    "from pubsub_state where nodeid='">>, Nidx, <<"';">>])
		    of
			{selected, [<<"jid">>, <<"subscriptions">>], RItems} ->
			    [{decode_jid(SJID), decode_subscriptions(Subs)} || [SJID, Subs] <- RItems];
			_ ->
			    []
		    end,
		    {PubsubNode, Subscriptions}
	    end, Nodes),
    {result, {default, broadcast, Reply}}.

subscribe_node(Nidx, Sender, Subscriber, AccessModel,
	    SendLast, PresenceSubscription, RosterGroup, _Options) ->
    SubKey = jid:tolower(Subscriber),
    GenKey = jid:remove_resource(SubKey),
    Authorized = jid:tolower(jid:remove_resource(Sender)) == GenKey,
    {Affiliation, Subscriptions} = select_affiliation_subscriptions(Nidx, GenKey, SubKey),
    Whitelisted = lists:member(Affiliation, [member, publisher, owner]),
    PendingSubscription = lists:any(fun
		({pending, _}) -> true;
		(_) -> false
	    end,
	    Subscriptions),
    Owner = Affiliation == owner,
    if not Authorized ->
	    {error,
		?ERR_EXTENDED((?ERR_BAD_REQUEST), <<"invalid-jid">>)};
	(Affiliation == outcast) or (Affiliation == publish_only) ->
	    {error, ?ERR_FORBIDDEN};
	PendingSubscription ->
	    {error,
		?ERR_EXTENDED((?ERR_NOT_AUTHORIZED), <<"pending-subscription">>)};
	(AccessModel == presence) and (not PresenceSubscription) and (not Owner) ->
	    {error,
		?ERR_EXTENDED((?ERR_NOT_AUTHORIZED), <<"presence-subscription-required">>)};
	(AccessModel == roster) and (not RosterGroup) and (not Owner) ->
	    {error,
		?ERR_EXTENDED((?ERR_NOT_AUTHORIZED), <<"not-in-roster-group">>)};
	(AccessModel == whitelist) and (not Whitelisted) and (not Owner) ->
	    {error,
		?ERR_EXTENDED((?ERR_NOT_ALLOWED), <<"closed-node">>)};
	%%MustPay ->
	%%        % Payment is required for a subscription
	%%        {error, ?ERR_PAYMENT_REQUIRED};
	%%ForbiddenAnonymous ->
	%%        % Requesting entity is anonymous
	%%        {error, ?ERR_FORBIDDEN};
	true ->
	    %%{result, SubId} = pubsub_subscription_odbc:subscribe_node(Subscriber, Nidx, Options),
	    {NewSub, SubId} = case Subscriptions of
		[{subscribed, Id}|_] ->
		    {subscribed, Id};
		[] ->
		    Id = pubsub_subscription_odbc:make_subid(),
		    Sub = case AccessModel of
			authorize -> pending;
			_ -> subscribed
		    end,
		    update_subscription(Nidx, SubKey, [{Sub, Id} | Subscriptions]),
		    {Sub, Id}
	    end,
	    case {NewSub, SendLast} of
		{subscribed, never} ->
		    {result, {default, subscribed, SubId}};
		{subscribed, _} ->
		    {result, {default, subscribed, SubId, send_last}};
		{_, _} ->
		    {result, {default, pending, SubId}}
	    end
    end.

unsubscribe_node(Nidx, Sender, Subscriber, SubId) ->
    SubKey = jid:tolower(Subscriber),
    GenKey = jid:remove_resource(SubKey),
    Authorized = jid:tolower(jid:remove_resource(Sender)) == GenKey,
    {Affiliation, Subscriptions} = select_affiliation_subscriptions(Nidx, SubKey),
    SubIdExists = case SubId of
	<<>> -> false;
	Binary when is_binary(Binary) -> true;
	_ -> false
    end,
    if
	%% Requesting entity is prohibited from unsubscribing entity
	not Authorized ->
	    {error, ?ERR_FORBIDDEN};
	%% Entity did not specify SubId
	%%SubId == "", ?? ->
	%%        {error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
	%% Invalid subscription identifier
	%%InvalidSubId ->
	%%        {error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
	%% Requesting entity is not a subscriber
	Subscriptions == [] ->
	    {error,
		?ERR_EXTENDED((?ERR_UNEXPECTED_REQUEST_CANCEL), <<"not-subscribed">>)};
	%% Subid supplied, so use that.
	SubIdExists ->
	    Sub = first_in_list(fun
			({_, S}) when S == SubId -> true;
			(_) -> false
		    end,
		    Subscriptions),
	    case Sub of
		{value, S} ->
		    delete_subscription(SubKey, Nidx, S, Affiliation, Subscriptions),
		    {result, default};
		false ->
		    {error,
			?ERR_EXTENDED((?ERR_UNEXPECTED_REQUEST_CANCEL), <<"not-subscribed">>)}
	    end;
	%% Asking to remove all subscriptions to the given node
	SubId == all ->
	    [delete_subscription(SubKey, Nidx, S, Affiliation, Subscriptions)
		|| S <- Subscriptions],
	    {result, default};
	%% No subid supplied, but there's only one matching subscription
	length(Subscriptions) == 1 ->
	    delete_subscription(SubKey, Nidx, hd(Subscriptions), Affiliation, Subscriptions),
	    {result, default};
	%% No subid and more than one possible subscription match.
	true ->
	    {error,
		?ERR_EXTENDED((?ERR_BAD_REQUEST), <<"subid-required">>)}
    end.

delete_subscription(SubKey, Nidx, {Subscription, SubId}, Affiliation, Subscriptions) ->
    NewSubs = Subscriptions -- [{Subscription, SubId}],
    %%pubsub_subscription_odbc:unsubscribe_node(SubKey, Nidx, SubId),
    case {Affiliation, NewSubs} of
	{none, []} -> del_state(Nidx, SubKey);
	_ -> update_subscription(Nidx, SubKey, NewSubs)
    end.

publish_item(Nidx, Publisher, PublishModel, MaxItems, ItemId, Payload) ->
    SubKey = jid:tolower(Publisher),
    GenKey = jid:remove_resource(SubKey),
    {Affiliation, Subscriptions} = select_affiliation_subscriptions(Nidx, GenKey, SubKey),
    Subscribed = case PublishModel of
	subscribers -> node_flat:is_subscribed(Subscriptions);
	_ -> undefined
    end,
    if not ((PublishModel == open) or
		    (PublishModel == publishers) and
		    ((Affiliation == owner)
			 or (Affiliation == publisher)
			 or (Affiliation == publish_only))
		    or (Subscribed == true)) ->
	    {error, ?ERR_FORBIDDEN};
	true ->
	    if MaxItems > 0 ->
		    PubId = {p1_time_compat:timestamp(), SubKey},
		    set_item(#pubsub_item{itemid = {ItemId, Nidx},
			    creation = {p1_time_compat:timestamp(), GenKey},
			    modification = PubId,
			    payload = Payload}),
		    Items = [ItemId | itemids(Nidx, GenKey) -- [ItemId]],
		    {result, {_, OI}} = remove_extra_items(Nidx, MaxItems, Items),
		    {result, {default, broadcast, OI}};
		true ->
		    {result, {default, broadcast, []}}
	    end
    end.

remove_extra_items(_Nidx, unlimited, ItemIds) ->
    {result, {ItemIds, []}};
remove_extra_items(Nidx, MaxItems, ItemIds) ->
    NewItems = lists:sublist(ItemIds, MaxItems),
    OldItems = lists:nthtail(length(NewItems), ItemIds),
    del_items(Nidx, OldItems),
    {result, {NewItems, OldItems}}.

delete_item(Nidx, Publisher, PublishModel, ItemId) ->
    SubKey = jid:tolower(Publisher),
    GenKey = jid:remove_resource(SubKey),
    {result, Affiliation} = get_affiliation(Nidx, GenKey),
    Allowed = Affiliation == publisher orelse
	Affiliation == owner orelse
	PublishModel == open orelse
	case get_item(Nidx, ItemId) of
	{result, #pubsub_item{creation = {_, GenKey}}} -> true;
	_ -> false
    end,
    if not Allowed ->
	    {error, ?ERR_FORBIDDEN};
	true ->
	    case del_item(Nidx, ItemId) of
		{updated, 1} -> {result, {default, broadcast}};
		_ -> {error, ?ERR_ITEM_NOT_FOUND}
	    end
    end.

purge_node(Nidx, Owner) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    GenState = get_state(Nidx, GenKey),
    case GenState of
	#pubsub_state{affiliation = owner} ->
	    {result, States} = get_states(Nidx),
	    lists:foreach(fun
		    (#pubsub_state{items = []}) -> ok;
		    (#pubsub_state{items = Items}) -> del_items(Nidx, Items)
		end,
		States),
	    {result, {default, broadcast}};
	_ ->
	    {error, ?ERR_FORBIDDEN}
    end.

get_entity_affiliations(Host, Owner) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    H = encode_host(Host),
    J = encode_jid(GenKey),
    Reply = case catch
	ejabberd_odbc:sql_query_t([<<"select node, type, i.nodeid, affiliation "
		    "from pubsub_state i, pubsub_node n where "
		    "i.nodeid = n.nodeid and jid='">>, J, <<"' and host='">>, H, <<"';">>])
    of
	{selected, [<<"node">>, <<"type">>, <<"nodeid">>, <<"affiliation">>], RItems} ->
	    [{nodetree_tree_odbc:raw_to_node(Host, [N, <<"">>, T, I]), decode_affiliation(A)}
		|| [N, T, I, A] <- RItems];
	_ ->
	    []
    end,
    {result, Reply}.

get_node_affiliations(Nidx) ->
    Reply = case catch
	ejabberd_odbc:sql_query_t([<<"select jid, affiliation from pubsub_state "
		    "where nodeid='">>, Nidx, <<"';">>])
    of
	{selected, [<<"jid">>, <<"affiliation">>], RItems} ->
	    [{decode_jid(J), decode_affiliation(A)} || [J, A] <- RItems];
	_ ->
	    []
    end,
    {result, Reply}.

get_affiliation(Nidx, Owner) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    J = encode_jid(GenKey),
    Reply = case catch
	ejabberd_odbc:sql_query_t([<<"select affiliation from pubsub_state "
		    "where nodeid='">>, Nidx, <<"' and jid='">>, J, <<"';">>])
    of
	{selected, [<<"affiliation">>], [[A]]} ->
	    decode_affiliation(A);
	_ ->
	    none
    end,
    {result, Reply}.

set_affiliation(Nidx, Owner, Affiliation) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    {_, Subscriptions} = select_affiliation_subscriptions(Nidx, GenKey),
    case {Affiliation, Subscriptions} of
	{none, []} -> del_state(Nidx, GenKey);
	_ -> update_affiliation(Nidx, GenKey, Affiliation)
    end.

get_entity_subscriptions(Host, Owner) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    H = encode_host(Host),
    SJ = encode_jid(SubKey),
    GJ = encode_jid(GenKey),
    Query = case SubKey of
	GenKey ->
	    [<<"select node, type, i.nodeid, jid, subscriptions "
		    "from pubsub_state i, pubsub_node n "
		    "where i.nodeid = n.nodeid and jid like '">>, GJ, <<"%' and host='">>, H, <<"';">>];
	_ ->
	    [<<"select node, type, i.nodeid, jid, subscriptions "
		    "from pubsub_state i, pubsub_node n "
		    "where i.nodeid = n.nodeid and jid in ('">>, SJ, <<"', '">>, GJ, <<"') and host='">>, H, <<"';">>]
    end,
    Reply = case catch ejabberd_odbc:sql_query_t(Query) of
	{selected,
		    [<<"node">>, <<"type">>, <<"nodeid">>, <<"jid">>, <<"subscriptions">>], RItems} ->
	    lists:foldl(fun ([N, T, I, J, S], Acc) ->
			Node = nodetree_tree_odbc:raw_to_node(Host, [N, <<"">>, T, I]),
			Jid = decode_jid(J),
			case decode_subscriptions(S) of
			    [] ->
				[{Node, none, Jid} | Acc];
			    Subs ->
				lists:foldl(fun ({Sub, SubId}, Acc2) ->
					    [{Node, Sub, SubId, Jid} | Acc2]
				    end,
				    Acc, Subs)
			end
		end,
		[], RItems);
	_ ->
	    []
    end,
    {result, Reply}.

-spec(get_entity_subscriptions_for_send_last/2 ::
    (
	Host :: mod_pubsub:hostPubsub(),
	Owner :: jid())
    -> {result,
    [{mod_pubsub:pubsubNode(),
    mod_pubsub:subscription(),
    mod_pubsub:subId(),
    ljid()}]
    }
    ).
get_entity_subscriptions_for_send_last(Host, Owner) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    H = encode_host(Host),
    SJ = encode_jid(SubKey),
    GJ = encode_jid(GenKey),
    Query = case SubKey of
	GenKey ->
	    [<<"select node, type, i.nodeid, jid, subscriptions "
		    "from pubsub_state i, pubsub_node n, pubsub_node_option o "
		    "where i.nodeid = n.nodeid and n.nodeid = o.nodeid and name='send_last_published_item' "
		    "and val='on_sub_and_presence' and jid like '">>, GJ, <<"%' and host='">>, H, <<"';">>];
	_ ->
	    [<<"select node, type, i.nodeid, jid, subscriptions "
		    "from pubsub_state i, pubsub_node n, pubsub_node_option o "
		    "where i.nodeid = n.nodeid and n.nodeid = o.nodeid and name='send_last_published_item' "
		    "and val='on_sub_and_presence' and jid in ('">>, SJ, <<"', '">>, GJ, <<"') and host='">>, H, <<"';">>]
    end,
    Reply = case catch ejabberd_odbc:sql_query_t(Query) of
	{selected,
		    [<<"node">>, <<"type">>, <<"nodeid">>, <<"jid">>, <<"subscriptions">>], RItems} ->
	    lists:foldl(fun ([N, T, I, J, S], Acc) ->
			Node = nodetree_tree_odbc:raw_to_node(Host, [N, <<"">>, T, I]),
			Jid = decode_jid(J),
			case decode_subscriptions(S) of
			    [] ->
				[{Node, none, Jid} | Acc];
			    Subs ->
				lists:foldl(fun ({Sub, SubId}, Acc2) ->
					    [{Node, Sub, SubId, Jid}| Acc2]
				    end,
				    Acc, Subs)
			end
		end,
		[], RItems);
	_ ->
	    []
    end,
    {result, Reply}.

get_node_subscriptions(Nidx) ->
    Reply = case catch
	ejabberd_odbc:sql_query_t([<<"select jid, subscriptions from pubsub_state "
		    "where nodeid='">>, Nidx, <<"';">>])
    of
	{selected, [<<"jid">>, <<"subscriptions">>], RItems} ->
	    lists:foldl(fun ([J, S], Acc) ->
			Jid = decode_jid(J),
			case decode_subscriptions(S) of
			    [] ->
				[{Jid, none} | Acc];
			    Subs ->
				lists:foldl(fun ({Sub, SubId}, Acc2) ->
					    [{Jid, Sub, SubId} | Acc2]
				    end,
				    Acc, Subs)
			end
		end,
		[], RItems);
	_ ->
	    []
    end,
    {result, Reply}.

get_subscriptions(Nidx, Owner) ->
    SubKey = jid:tolower(Owner),
    J = encode_jid(SubKey),
    Reply = case catch
	ejabberd_odbc:sql_query_t([<<"select subscriptions from pubsub_state where "
		    "nodeid='">>, Nidx, <<"' and jid='">>, J, <<"';">>])
    of
	{selected, [<<"subscriptions">>], [[S]]} ->
	    decode_subscriptions(S);
	_ ->
	    []
    end,
    {result, Reply}.

set_subscriptions(Nidx, Owner, Subscription, SubId) ->
    SubKey = jid:tolower(Owner),
    SubState = get_state_without_itemids(Nidx, SubKey),
    case {SubId, SubState#pubsub_state.subscriptions} of
	{_, []} ->
	    case Subscription of
		none ->
		    {error,
			?ERR_EXTENDED((?ERR_BAD_REQUEST), <<"not-subscribed">>)};
		_ ->
		    new_subscription(Nidx, Owner, Subscription, SubState)
	    end;
	{<<>>, [{_, SID}]} ->
	    case Subscription of
		none -> unsub_with_subid(Nidx, SID, SubState);
		_ -> replace_subscription({Subscription, SID}, SubState)
	    end;
	{<<>>, [_ | _]} ->
	    {error,
		?ERR_EXTENDED((?ERR_BAD_REQUEST), <<"subid-required">>)};
	_ ->
	    case Subscription of
		none -> unsub_with_subid(Nidx, SubId, SubState);
		_ -> replace_subscription({Subscription, SubId}, SubState)
	    end
    end.

replace_subscription(NewSub, SubState) ->
    NewSubs = replace_subscription(NewSub, SubState#pubsub_state.subscriptions, []),
    set_state(SubState#pubsub_state{subscriptions = NewSubs}).

replace_subscription(_, [], Acc) -> Acc;
replace_subscription({Sub, SubId}, [{_, SubId} | T], Acc) ->
    replace_subscription({Sub, SubId}, T, [{Sub, SubId} | Acc]).

new_subscription(_Nidx, _Owner, Subscription, SubState) ->
    %%{result, SubId} = pubsub_subscription_odbc:subscribe_node(Owner, Nidx, []),
    SubId = pubsub_subscription_odbc:make_subid(),
    Subscriptions = [{Subscription, SubId} | SubState#pubsub_state.subscriptions],
    set_state(SubState#pubsub_state{subscriptions = Subscriptions}),
    {Subscription, SubId}.

unsub_with_subid(Nidx, SubId, SubState) ->
    %%pubsub_subscription_odbc:unsubscribe_node(SubState#pubsub_state.stateid, Nidx, SubId),
    NewSubs = [{S, Sid}
	    || {S, Sid} <- SubState#pubsub_state.subscriptions,
		SubId =/= Sid],
    case {NewSubs, SubState#pubsub_state.affiliation} of
	{[], none} -> del_state(Nidx, element(1, SubState#pubsub_state.stateid));
	_ -> set_state(SubState#pubsub_state{subscriptions = NewSubs})
    end.

get_pending_nodes(Host, Owner) ->
    GenKey = jid:remove_resource(jid:tolower(Owner)),
    States = mnesia:match_object(#pubsub_state{stateid = {GenKey, '_'},
		affiliation = owner, _ = '_'}),
    Nidxxs = [Nidx || #pubsub_state{stateid = {_, Nidx}} <- States],
    NodeTree = mod_pubsub:tree(Host),
    Reply = mnesia:foldl(fun (#pubsub_state{stateid = {_, Nidx}} = S, Acc) ->
		    case lists:member(Nidx, Nidxxs) of
			true ->
			    case get_nodes_helper(NodeTree, S) of
				{value, Node} -> [Node | Acc];
				false -> Acc
			    end;
			false ->
			    Acc
		    end
	    end,
	    [], pubsub_state),
    {result, Reply}.

get_nodes_helper(NodeTree, #pubsub_state{stateid = {_, N}, subscriptions = Subs}) ->
    HasPending = fun
	({pending, _}) -> true;
	(pending) -> true;
	(_) -> false
    end,
    case lists:any(HasPending, Subs) of
	true ->
	    case NodeTree:get_node(N) of
		#pubsub_node{nodeid = {_, Node}} -> {value, Node};
		_ -> false
	    end;
	false ->
	    false
    end.

get_states(Nidx) ->
    case catch
	ejabberd_odbc:sql_query_t([<<"select jid, affiliation, subscriptions "
		    "from pubsub_state where nodeid='">>, Nidx, <<"';">>])
    of
	{selected,
		    [<<"jid">>, <<"affiliation">>, <<"subscriptions">>], RItems} ->
	    {result,
		lists:map(fun ([SJID, Aff, Subs]) ->
			    #pubsub_state{stateid = {decode_jid(SJID), Nidx},
				items = itemids(Nidx, SJID),
				affiliation = decode_affiliation(Aff),
				subscriptions = decode_subscriptions(Subs)}
		    end,
		    RItems)};
	_ ->
	    {result, []}
    end.

get_state(Nidx, JID) ->
    State = get_state_without_itemids(Nidx, JID),
    {SJID, _} = State#pubsub_state.stateid,
    State#pubsub_state{items = itemids(Nidx, SJID)}.

-spec(get_state_without_itemids/2 ::
    (Nidx :: mod_pubsub:nodeIdx(),
	Key :: ljid()) ->
    mod_pubsub:pubsubState()
    ).
get_state_without_itemids(Nidx, JID) ->
    J = encode_jid(JID),
    case catch
	ejabberd_odbc:sql_query_t([<<"select jid, affiliation, subscriptions "
		    "from pubsub_state where jid='">>, J, <<"' and nodeid='">>, Nidx, <<"';">>])
    of
	{selected,
		    [<<"jid">>, <<"affiliation">>, <<"subscriptions">>], [[SJID, Aff, Subs]]} ->
	    #pubsub_state{stateid = {decode_jid(SJID), Nidx},
		affiliation = decode_affiliation(Aff),
		subscriptions = decode_subscriptions(Subs)};
	_ ->
	    #pubsub_state{stateid = {JID, Nidx}}
    end.

set_state(State) ->
    {_, Nidx} = State#pubsub_state.stateid,
    set_state(Nidx, State).

set_state(Nidx, State) ->
    {JID, _} = State#pubsub_state.stateid,
    J = encode_jid(JID),
    S = encode_subscriptions(State#pubsub_state.subscriptions),
    A = encode_affiliation(State#pubsub_state.affiliation),
    case catch
	ejabberd_odbc:sql_query_t([<<"update pubsub_state set subscriptions='">>, S, <<"', affiliation='">>, A,
		<<"' where nodeid='">>, Nidx, <<"' and jid='">>, J, <<"';">>])
    of
	{updated, 1} ->
	    ok;
	_ ->
	    catch
	    ejabberd_odbc:sql_query_t([<<"insert into pubsub_state(nodeid, jid, affiliation, subscriptions) "
			"values('">>,
		    Nidx, <<"', '">>, J, <<"', '">>, A, <<"', '">>, S, <<"');">>])
    end,
    ok.

del_state(Nidx, JID) ->
    J = encode_jid(JID),
    catch ejabberd_odbc:sql_query_t([<<"delete from pubsub_state where jid='">>,
	    J, <<"' and nodeid='">>, Nidx, <<"';">>]),
    ok.

%get_items(Nidx, _From) ->
%    case catch
%	ejabberd_odbc:sql_query_t([<<"select itemid, publisher, creation, modification, payload "
%		    "from pubsub_item where nodeid='">>, Nidx,
%		<<"' order by modification desc;">>])
%    of
%	{selected,
%		    [<<"itemid">>, <<"publisher">>, <<"creation">>, <<"modification">>, <<"payload">>], RItems} ->
%	    {result, [raw_to_item(Nidx, RItem) || RItem <- RItems]};
%	_ ->
%	    {result, []}
%    end.

get_items(Nidx, From, none) ->
    MaxItems = case catch
	ejabberd_odbc:sql_query_t([<<"select val from pubsub_node_option "
		    "where nodeid='">>, Nidx, <<"' and name='max_items';">>])
    of
	{selected, [<<"val">>], [[Value]]} ->
	    Tokens = element(2, erl_scan:string(binary_to_list(<<Value/binary, ".">>))),
	    element(2, erl_parse:parse_term(Tokens));
	_ ->
	    ?MAXITEMS
    end,
    get_items(Nidx, From, #rsm_in{max = MaxItems});
get_items(Nidx, _From,
	    #rsm_in{max = M, direction = Direction, id = I, index = IncIndex}) ->
    Max = ejabberd_odbc:escape(jlib:i2l(M)),
    {Way, Order} = case Direction of
	%     aft -> {<<"<">>, <<"desc">>};
	%     before when I == <<>> -> {<<"is not">>, <<"asc">>};
	%     before -> {<<">">>, <<"asc">>};
	%     _ when IncIndex =/= undefined ->
	%         {<<"<">>, <<"desc">>}; % using index
	_ ->
	    {<<"is not">>, <<"desc">>}% Can be better
    end,
    [AttrName, Id] = case I of
	undefined when IncIndex =/= undefined ->
	    case catch
		ejabberd_odbc:sql_query_t([<<"select modification from pubsub_item pi "
			    "where exists ( select count(*) as count1 "
			    "from pubsub_item where nodeid='">>, Nidx,
			<<"' and modification > pi.modification having count1 = ">>,
			ejabberd_odbc:escape(jlib:i2l(IncIndex)), <<" );">>])
	    of
		{selected, [_], [[O]]} ->
		    [<<"modification">>, <<"'", O/binary, "'">>];
		_ ->
		    [<<"modification">>, <<"null">>]
	    end;
	undefined ->
	    [<<"modification">>, <<"null">>];
	<<>> ->
	    [<<"modification">>, <<"null">>];
	I ->
	    [A, B] = str:tokens(ejabberd_odbc:escape(jlib:i2l(I)), <<"@">>),
	    [A, <<"'", B/binary, "'">>]
    end,
    Count = case catch
	ejabberd_odbc:sql_query_t([<<"select count(*) from pubsub_item where nodeid='">>, Nidx, <<"';">>])
    of
	{selected, [_], [[C]]} -> C;
	_ -> <<"0">>
    end,
    case catch
	ejabberd_odbc:sql_query_t([<<"select itemid, publisher, creation, modification, payload "
		    "from pubsub_item where nodeid='">>, Nidx,
		<<"' and ">>, AttrName, <<" ">>, Way, <<" ">>, Id, <<" order by ">>,
		AttrName, <<" ">>, Order, <<" limit ">>, jlib:i2l(Max), <<" ;">>])
    of
	{selected,
		    [<<"itemid">>, <<"publisher">>, <<"creation">>, <<"modification">>, <<"payload">>], RItems} ->
	    case RItems of
		[[_, _, _, F, _]|_] ->
		    Index = case catch
			ejabberd_odbc:sql_query_t([<<"select count(*) from pubsub_item "
				    "where nodeid='">>, Nidx, <<"' and ">>,
				AttrName, <<" > '">>, F, <<"';">>])
		    of
			%{selected, [_], [{C}, {In}]} -> [string:strip(C, both, $"), string:strip(In, both, $")];
			{selected, [_], [[In]]} -> In;
			_ -> <<"0">>
		    end,
		    [_, _, _, L, _] = lists:last(RItems),
		    RsmOut = #rsm_out{count = Count, index = Index,
			    first = <<"modification@", F/binary>>,
			    last = <<"modification@", (jlib:i2l(L))/binary>>},
		    {result, {[raw_to_item(Nidx, RItem) || RItem <- RItems], RsmOut}};
		[] ->
		    {result, {[], #rsm_out{count = Count}}}
	    end;
	_ ->
	    {result, {[], none}}
    end.

get_items(Nidx, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId, RSM) ->
    SubKey = jid:tolower(JID),
    GenKey = jid:remove_resource(SubKey),
    {Affiliation, Subscriptions} = select_affiliation_subscriptions(Nidx, GenKey, SubKey),
    Whitelisted = node_flat:can_fetch_item(Affiliation, Subscriptions),
    if %%SubId == "", ?? ->
	%% Entity has multiple subscriptions to the node but does not specify a subscription ID
	%{error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
	%%InvalidSubId ->
	%% Entity is subscribed but specifies an invalid subscription ID
	%{error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
	(Affiliation == outcast) or (Affiliation == publish_only) ->
	    {error, ?ERR_FORBIDDEN};
	(AccessModel == presence) and not PresenceSubscription ->
	    {error,
		?ERR_EXTENDED((?ERR_NOT_AUTHORIZED), <<"presence-subscription-required">>)};
	(AccessModel == roster) and not RosterGroup ->
	    {error,
		?ERR_EXTENDED((?ERR_NOT_AUTHORIZED), <<"not-in-roster-group">>)};
	(AccessModel == whitelist) and not Whitelisted ->
	    {error,
		?ERR_EXTENDED((?ERR_NOT_ALLOWED), <<"closed-node">>)};
	(AccessModel == authorize) and not Whitelisted ->
	    {error, ?ERR_FORBIDDEN};
	%%MustPay ->
	%%        % Payment is required for a subscription
	%%        {error, ?ERR_PAYMENT_REQUIRED};
	true ->
	    get_items(Nidx, JID, RSM)
    end.

get_last_items(Nidx, _From, Count) ->
    case catch
	ejabberd_odbc:sql_query_t([<<"select itemid, publisher, creation, modification, payload "
		    "from pubsub_item where nodeid='">>, Nidx,
		<<"' order by modification desc limit ">>, jlib:i2l(Count), <<";">>])
    of
	{selected,
		    [<<"itemid">>, <<"publisher">>, <<"creation">>, <<"modification">>, <<"payload">>], RItems} ->
	    {result, [raw_to_item(Nidx, RItem) || RItem <- RItems]};
	_ ->
	    {result, []}
    end.

get_item(Nidx, ItemId) ->
    I = ejabberd_odbc:escape(ItemId),
    case catch
	ejabberd_odbc:sql_query_t([<<"select itemid, publisher, creation, "
		    "modification, payload from pubsub_item "
		    "where nodeid='">>, Nidx, <<"' and itemid='">>, I, <<"';">>])
    of
	{selected,
		    [<<"itemid">>, <<"publisher">>, <<"creation">>, <<"modification">>, <<"payload">>], [RItem]} ->
	    {result, raw_to_item(Nidx, RItem)};
	_ ->
	    {error, ?ERR_ITEM_NOT_FOUND}
    end.

get_item(Nidx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId) ->
    SubKey = jid:tolower(JID),
    GenKey = jid:remove_resource(SubKey),
    {Affiliation, Subscriptions} = select_affiliation_subscriptions(Nidx, GenKey, SubKey),
    Whitelisted = node_flat:can_fetch_item(Affiliation, Subscriptions),
    if %%SubId == "", ?? ->
	%% Entity has multiple subscriptions to the node but does not specify a subscription ID
	%{error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
	%%InvalidSubId ->
	%% Entity is subscribed but specifies an invalid subscription ID
	%{error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
	(Affiliation == outcast) or (Affiliation == publish_only) ->
	    {error, ?ERR_FORBIDDEN};
	(AccessModel == presence) and not PresenceSubscription ->
	    {error,
		?ERR_EXTENDED((?ERR_NOT_AUTHORIZED), <<"presence-subscription-required">>)};
	(AccessModel == roster) and not RosterGroup ->
	    {error,
		?ERR_EXTENDED((?ERR_NOT_AUTHORIZED), <<"not-in-roster-group">>)};
	(AccessModel == whitelist) and not Whitelisted ->
	    {error,
		?ERR_EXTENDED((?ERR_NOT_ALLOWED), <<"closed-node">>)};
	(AccessModel == authorize) and not Whitelisted ->
	    {error, ?ERR_FORBIDDEN};
	%%MustPay ->
	%%        % Payment is required for a subscription
	%%        {error, ?ERR_PAYMENT_REQUIRED};
	true ->
	    get_item(Nidx, ItemId)
    end.

set_item(Item) ->
    {ItemId, Nidx} = Item#pubsub_item.itemid,
    I = ejabberd_odbc:escape(ItemId),
    {C, _} = Item#pubsub_item.creation,
    {M, JID} = Item#pubsub_item.modification,
    P = encode_jid(JID),
    Payload = Item#pubsub_item.payload,
    XML = ejabberd_odbc:escape(str:join([xml:element_to_binary(X) || X<-Payload], <<>>)),
    S = fun ({T1, T2, T3}) ->
	    str:join([jlib:i2l(T1, 6), jlib:i2l(T2, 6), jlib:i2l(T3, 6)], <<":">>)
    end,
    case catch
	ejabberd_odbc:sql_query_t([<<"update pubsub_item set publisher='">>, P,
		<<"', modification='">>, S(M),
		<<"', payload='">>, XML,
		<<"' where nodeid='">>, Nidx, <<"' and itemid='">>, I, <<"';">>])
    of
	{updated, 1} ->
	    ok;
	_ ->
	    catch
	    ejabberd_odbc:sql_query_t([<<"insert into pubsub_item (nodeid, itemid, "
			"publisher, creation, modification, payload) "
			"values('">>, Nidx, <<"', '">>, I, <<"', '">>, P,
		    <<"', '">>, S(C), <<"', '">>, S(M),
		    <<"', '">>, XML, <<"');">>])
    end,
    ok.

del_item(Nidx, ItemId) ->
    I = ejabberd_odbc:escape(ItemId),
    catch ejabberd_odbc:sql_query_t([<<"delete from pubsub_item where itemid='">>,
	    I, <<"' and nodeid='">>, Nidx, <<"';">>]).

del_items(_, []) ->
    ok;
del_items(Nidx, [ItemId]) ->
    del_item(Nidx, ItemId);
del_items(Nidx, ItemIds) ->
    I = str:join([[<<"'">>, ejabberd_odbc:escape(X), <<"'">>] || X <- ItemIds], <<",">>),
    catch
    ejabberd_odbc:sql_query_t([<<"delete from pubsub_item where itemid in (">>,
	    I, <<") and nodeid='">>, Nidx, <<"';">>]).

get_item_name(_Host, _Node, Id) ->
    Id.

node_to_path(Node) ->
    node_flat:node_to_path(Node).

path_to_node(Path) ->
    node_flat:path_to_node(Path).


first_in_list(_Pred, []) ->
    false;
first_in_list(Pred, [H | T]) ->
    case Pred(H) of
	true -> {value, H};
	_ -> first_in_list(Pred, T)
    end.

itemids(Nidx, {U, S, R}) ->
    itemids(Nidx, encode_jid({U, S, R}));
itemids(Nidx, SJID) ->
    case catch
	ejabberd_odbc:sql_query_t([<<"select itemid from pubsub_item where "
		    "nodeid='">>, Nidx, <<"' and publisher like '">>, SJID,
		<<"%' order by modification desc;">>])
    of
	{selected, [<<"itemid">>], RItems} ->
	    [ItemId || [ItemId] <- RItems];
	_ ->
	    []
    end.

select_affiliation_subscriptions(Nidx, JID) ->
    J = encode_jid(JID),
    case catch
	ejabberd_odbc:sql_query_t([<<"select affiliation,subscriptions from "
		    "pubsub_state where nodeid='">>,
		Nidx, <<"' and jid='">>, J, <<"';">>])
    of
	{selected, [<<"affiliation">>, <<"subscriptions">>], [[A, S]]} ->
	    {decode_affiliation(A), decode_subscriptions(S)};
	_ ->
	    {none, []}
    end.

select_affiliation_subscriptions(Nidx, JID, JID) ->
    select_affiliation_subscriptions(Nidx, JID);
select_affiliation_subscriptions(Nidx, GenKey, SubKey) ->
    {result, Affiliation} = get_affiliation(Nidx, GenKey),
    {result, Subscriptions} = get_subscriptions(Nidx, SubKey),
    {Affiliation, Subscriptions}.

update_affiliation(Nidx, JID, Affiliation) ->
    J = encode_jid(JID),
    A = encode_affiliation(Affiliation),
    case catch
	ejabberd_odbc:sql_query_t([<<"update pubsub_state set affiliation='">>,
		A, <<"' where nodeid='">>, Nidx,
		<<"' and jid='">>, J, <<"';">>])
    of
	{updated, 1} ->
	    ok;
	_ ->
	    catch
	    ejabberd_odbc:sql_query_t([<<"insert into pubsub_state(nodeid, jid, affiliation, subscriptions) "
			"values('">>, Nidx, <<"', '">>, J, <<"', '">>, A, <<"', '');">>])
    end.

update_subscription(Nidx, JID, Subscription) ->
    J = encode_jid(JID),
    S = encode_subscriptions(Subscription),
    case catch
	ejabberd_odbc:sql_query_t([<<"update pubsub_state set subscriptions='">>, S,
		<<"' where nodeid='">>, Nidx, <<"' and jid='">>, J, <<"';">>])
    of
	{updated, 1} ->
	    ok;
	_ ->
	    catch
	    ejabberd_odbc:sql_query_t([<<"insert into pubsub_state(nodeid, jid, affiliation, subscriptions) "
			"values('">>, Nidx, <<"', '">>, J, <<"', 'n', '">>, S, <<"');">>])
    end.

-spec(decode_jid/1 ::
    (   SJID :: binary())
    -> ljid()
    ).
decode_jid(SJID) ->
    jid:tolower(jid:from_string(SJID)).

-spec(decode_affiliation/1 ::
    (   Arg :: binary())
    -> atom()
    ).
decode_affiliation(<<"o">>) -> owner;
decode_affiliation(<<"p">>) -> publisher;
decode_affiliation(<<"u">>) -> publish_only;
decode_affiliation(<<"m">>) -> member;
decode_affiliation(<<"c">>) -> outcast;
decode_affiliation(_) -> none.

-spec(decode_subscription/1 ::
    (   Arg :: binary())
    -> atom()
    ).
decode_subscription(<<"s">>) -> subscribed;
decode_subscription(<<"p">>) -> pending;
decode_subscription(<<"u">>) -> unconfigured;
decode_subscription(_) -> none.

-spec(decode_subscriptions/1 ::
    (   Subscriptions :: binary())
    -> [] | [{atom(), binary()},...]
    ).
decode_subscriptions(Subscriptions) ->
    lists:foldl(fun (Subscription, Acc) ->
		case str:tokens(Subscription, <<":">>) of
		    [S, SubId] -> [{decode_subscription(S), SubId} | Acc];
		    _ -> Acc
		end
	end,
	[], str:tokens(Subscriptions, <<",">>)).

-spec(encode_jid/1 ::
    (   JID :: ljid())
    -> binary()
    ).
encode_jid(JID) ->
    ejabberd_odbc:escape(jid:to_string(JID)).

-spec(encode_host/1 ::
    (   Host :: host())
    -> binary()
    ).
encode_host({_U, _S, _R} = LJID) -> encode_jid(LJID);
encode_host(Host) -> ejabberd_odbc:escape(Host).

-spec(encode_affiliation/1 ::
    (   Arg :: atom())
    -> binary()
    ).
encode_affiliation(owner) -> <<"o">>;
encode_affiliation(publisher) -> <<"p">>;
encode_affiliation(publish_only) -> <<"u">>;
encode_affiliation(member) -> <<"m">>;
encode_affiliation(outcast) -> <<"c">>;
encode_affiliation(_) -> <<"n">>.

-spec(encode_subscription/1 ::
    (   Arg :: atom())
    -> binary()
    ).
encode_subscription(subscribed) -> <<"s">>;
encode_subscription(pending) -> <<"p">>;
encode_subscription(unconfigured) -> <<"u">>;
encode_subscription(_) -> <<"n">>.

-spec(encode_subscriptions/1 ::
    (   Subscriptions :: [] | [{atom(), binary()},...])
    -> binary()
    ).
encode_subscriptions(Subscriptions) ->
    str:join([<<(encode_subscription(S))/binary, ":", SubId/binary>>
	    || {S, SubId} <- Subscriptions], <<",">>).

%%% record getter/setter

state_to_raw(Nidx, State) ->
    {JID, _} = State#pubsub_state.stateid,
    J = encode_jid(JID),
    A = encode_affiliation(State#pubsub_state.affiliation),
    S = encode_subscriptions(State#pubsub_state.subscriptions),
    [<<"'">>, Nidx, <<"', '">>, J, <<"', '">>, A, <<"', '">>, S, <<"'">>].

raw_to_item(Nidx, [ItemId, SJID, Creation, Modification, XML]) ->
    JID = decode_jid(SJID),
    ToTime = fun (Str) ->
	    [T1, T2, T3] = str:tokens(Str, <<":">>),
	    {jlib:l2i(T1), jlib:l2i(T2), jlib:l2i(T3)}
    end,
    Payload = case xml_stream:parse_element(XML) of
	{error, _Reason} -> [];
	El -> [El]
    end,
    #pubsub_item{itemid = {ItemId, Nidx},
	creation = {ToTime(Creation), JID},
	modification = {ToTime(Modification), JID},
	payload = Payload}.
