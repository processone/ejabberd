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

%%% @todo The item table should be handled by the plugin, but plugin that do
%%% not want to manage it should be able to use the default behaviour.
%%% @todo Plugin modules should be able to register to receive presence update
%%% send to pubsub.

%%% @doc The module <strong>{@module}</strong> is the default PubSub plugin.
%%% <p>It is used as a default for all unknown PubSub node type.  It can serve
%%% as a developer basis and reference to build its own custom pubsub node
%%% types.</p>
%%% <p>PubSub plugin nodes are using the {@link gen_node} behaviour.</p>
%%% <p><strong>The API isn't stabilized yet</strong>. The pubsub plugin
%%% development is still a work in progress. However, the system is already
%%% useable and useful as is. Please, send us comments, feedback and
%%% improvements.</p>

-module(node_hometree_odbc).
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
    set_state/1, get_items/7, get_items/3,
    get_items/6, get_items/2, get_item/7,
    get_item/2, set_item/1, get_item_name/3, node_to_path/1,
    path_to_node/1,
    get_entity_subscriptions_for_send_last/2, get_last_items/3]).

-export([decode_jid/1, encode_jid/1,
    decode_affiliation/1, decode_subscriptions/1,
    encode_affiliation/1, encode_subscriptions/1,
    encode_host/1]).

init(Host, ServerHost, _Opts) ->
    pubsub_subscription_odbc:init(),
    Owner = mod_pubsub:service_jid(Host),
    mod_pubsub:create_node(Host, ServerHost, <<"/home">>, Owner, <<"hometree">>),
    mod_pubsub:create_node(Host, ServerHost, <<"/home/", ServerHost/binary>>, Owner, <<"hometree">>),
    ok.

terminate(_Host, _ServerHost) ->
    ok.

options() ->
    [{odbc, true}, {rsm, true} | node_hometree:options()].

features() ->
    [<<"rsm">> | node_hometree:features()].

%% @doc Checks if the current user has the permission to create the requested node
%% <p>In {@link node_default}, the permission is decided by the place in the
%% hierarchy where the user is creating the node. The access parameter is also
%% checked in the default module. This parameter depends on the value of the
%% <tt>access_createnode</tt> ACL value in ejabberd config file.</p>
%% <p>This function also check that node can be created a a children of its
%% parent node</p>
create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
    node_hometree:create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access).

create_node(Nidx, Owner) ->
    {_U, _S, _R} = OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    State = #pubsub_state{stateid = {OwnerKey, Nidx}, affiliation = owner},
    catch ejabberd_odbc:sql_query_t([<<"insert into pubsub_state(nodeid, jid, affiliation, subscriptions) "
		"values(">>, state_to_raw(Nidx, State), <<");">>]),
    {result, {default, broadcast}}.

delete_node(Nodes) ->
    Reply = lists:map(fun (#pubsub_node{id = Nidx} = Node) ->
		    Subscriptions = case catch
			ejabberd_odbc:sql_query_t([<<"select jid, subscriptions "
				    "from pubsub_state where nodeid='">>, Nidx, <<"';">>])
		    of
			{selected, [<<"jid">>, <<"subscriptions">>], RItems} ->
			    [{decode_jid(SJID), decode_subscriptions(Subs)} || [SJID, Subs] <- RItems];
			_ ->
			    []
		    end,
		    {Node, Subscriptions}
	    end,
	    Nodes),
    {result, {default, broadcast, Reply}}.

%% @doc <p>Accepts or rejects subcription requests on a PubSub node.</p>
%% <p>The mechanism works as follow:
%% <ul>
%% <li>The main PubSub module prepares the subscription and passes the
%% result of the preparation as a record.</li>
%% <li>This function gets the prepared record and several other parameters and
%% can decide to:<ul>
%%  <li>reject the subscription;</li>
%%  <li>allow it as is, letting the main module perform the database
%%  persistance;</li>
%%  <li>allow it, modifying the record. The main module will store the
%%  modified record;</li>
%%  <li>allow it, but perform the needed persistance operations.</li></ul>
%% </li></ul></p>
%% <p>The selected behaviour depends on the return parameter:
%%  <ul>
%%   <li><tt>{error, Reason}</tt>: an IQ error result will be returned. No
%%   subscription will actually be performed.</li>
%%   <li><tt>true</tt>: Subscribe operation is allowed, based on the
%%   unmodified record passed in parameter <tt>SubscribeResult</tt>. If this
%%   parameter contains an error, no subscription will be performed.</li>
%%   <li><tt>{true, PubsubState}</tt>: Subscribe operation is allowed, but
%%   the {@link mod_pubsub:pubsubState()} record returned replaces the value
%%   passed in parameter <tt>SubscribeResult</tt>.</li>
%%   <li><tt>{true, done}</tt>: Subscribe operation is allowed, but the
%%   {@link mod_pubsub:pubsubState()} will be considered as already stored and
%%   no further persistance operation will be performed. This case is used,
%%   when the plugin module is doing the persistance by itself or when it want
%%   to completly disable persistance.</li></ul>
%% </p>
%% <p>In the default plugin module, the record is unchanged.</p>
subscribe_node(Nidx, Sender, Subscriber, AccessModel,
	    SendLast, PresenceSubscription, RosterGroup, Options) ->
    SubKey = jlib:jid_tolower(Subscriber),
    GenKey = jlib:jid_remove_resource(SubKey),
    Authorized = jlib:jid_tolower(jlib:jid_remove_resource(Sender)) == GenKey,
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
	    {result, SubId} = pubsub_subscription_odbc:subscribe_node(Subscriber, Nidx, Options),
	    NewSub = case AccessModel of
		authorize -> pending;
		_ -> subscribed
	    end,
	    update_subscription(Nidx, SubKey, [{NewSub, SubId} | Subscriptions]),
	    case {NewSub, SendLast} of
		{subscribed, never} -> {result, {default, subscribed, SubId}};
		{subscribed, _} -> {result, {default, subscribed, SubId, send_last}};
		{_, _} -> {result, {default, pending, SubId}}
	    end
    end.

%% @doc <p>Unsubscribe the <tt>Subscriber</tt> from the <tt>Node</tt>.</p>
unsubscribe_node(Nidx, Sender, Subscriber, SubId) ->
    SubKey = jlib:jid_tolower(Subscriber),
    GenKey = jlib:jid_remove_resource(SubKey),
    Authorized = jlib:jid_tolower(jlib:jid_remove_resource(Sender)) == GenKey,
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
    pubsub_subscription_odbc:unsubscribe_node(SubKey, Nidx, SubId),
    case {Affiliation, NewSubs} of
	{none, []} -> del_state(Nidx, SubKey);
	_ -> update_subscription(Nidx, SubKey, NewSubs)
    end.

%% @doc <p>Publishes the item passed as parameter.</p>
%% <p>The mechanism works as follow:
%% <ul>
%% <li>The main PubSub module prepares the item to publish and passes the
%% result of the preparation as a {@link mod_pubsub:pubsubItem()} record.</li>
%% <li>This function gets the prepared record and several other parameters and can decide to:<ul>
%%  <li>reject the publication;</li>
%%  <li>allow the publication as is, letting the main module perform the database persistance;</li>
%%  <li>allow the publication, modifying the record. The main module will store the modified record;</li>
%%  <li>allow it, but perform the needed persistance operations.</li></ul>
%% </li></ul></p>
%% <p>The selected behaviour depends on the return parameter:
%%  <ul>
%%   <li><tt>{error, Reason}</tt>: an iq error result will be return. No
%%   publication is actually performed.</li>
%%   <li><tt>true</tt>: Publication operation is allowed, based on the
%%   unmodified record passed in parameter <tt>Item</tt>. If the <tt>Item</tt>
%%   parameter contains an error, no subscription will actually be
%%   performed.</li>
%%   <li><tt>{true, Item}</tt>: Publication operation is allowed, but the
%%   {@link mod_pubsub:pubsubItem()} record returned replaces the value passed
%%   in parameter <tt>Item</tt>. The persistance will be performed by the main
%%   module.</li>
%%   <li><tt>{true, done}</tt>: Publication operation is allowed, but the
%%   {@link mod_pubsub:pubsubItem()} will be considered as already stored and
%%   no further persistance operation will be performed. This case is used,
%%   when the plugin module is doing the persistance by itself or when it want
%%   to completly disable persistance.</li></ul>
%% </p>
%% <p>In the default plugin module, the record is unchanged.</p>
publish_item(Nidx, Publisher, PublishModel, MaxItems, ItemId, Payload) ->
    SubKey = jlib:jid_tolower(Publisher),
    GenKey = jlib:jid_remove_resource(SubKey),
    {Affiliation, Subscriptions} = select_affiliation_subscriptions(Nidx, GenKey, SubKey),
    Subscribed = case PublishModel of
	subscribers -> is_subscribed(Subscriptions);
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
		    PubId = {now(), SubKey},
		    set_item(#pubsub_item{itemid = {ItemId, Nidx},
			    creation = {now(), GenKey},
			    modification = PubId,
			    payload = Payload}),
		    Items = [ItemId | itemids(Nidx, GenKey) -- [ItemId]],
		    {result, {_, OI}} = remove_extra_items(Nidx, MaxItems, Items),
		    {result, {default, broadcast, OI}};
		true ->
		    {result, {default, broadcast, []}}
	    end
    end.

%% @doc <p>This function is used to remove extra items, most notably when the
%% maximum number of items has been reached.</p>
%% <p>This function is used internally by the core PubSub module, as no
%% permission check is performed.</p>
%% <p>In the default plugin module, the oldest items are removed, but other
%% rules can be used.</p>
%% <p>If another PubSub plugin wants to delegate the item removal (and if the
%% plugin is using the default pubsub storage), it can implements this function like this:
%% ```remove_extra_items(Nidx, MaxItems, ItemIds) ->
%%           node_default:remove_extra_items(Nidx, MaxItems, ItemIds).'''</p>
remove_extra_items(_Nidx, unlimited, ItemIds) ->
    {result, {ItemIds, []}};
remove_extra_items(Nidx, MaxItems, ItemIds) ->
    NewItems = lists:sublist(ItemIds, MaxItems),
    OldItems = lists:nthtail(length(NewItems), ItemIds),
    del_items(Nidx, OldItems),
    {result, {NewItems, OldItems}}.

%% @doc <p>Triggers item deletion.</p>
%% <p>Default plugin: The user performing the deletion must be the node owner
%% or a publisher, or PublishModel being open.</p>
delete_item(Nidx, Publisher, PublishModel, ItemId) ->
    SubKey = jlib:jid_tolower(Publisher),
    GenKey = jlib:jid_remove_resource(SubKey),
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
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
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

%% @doc <p>Return the current affiliations for the given user</p>
%% <p>The default module reads affiliations in the main Mnesia
%% <tt>pubsub_state</tt> table. If a plugin stores its data in the same
%% table, it should return an empty list, as the affiliation will be read by
%% the default PubSub module. Otherwise, it should return its own affiliation,
%% that will be added to the affiliation stored in the main
%% <tt>pubsub_state</tt> table.</p>
get_entity_affiliations(Host, Owner) ->
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
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
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
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
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
    {_, Subscriptions} = select_affiliation_subscriptions(Nidx, GenKey),
    case {Affiliation, Subscriptions} of
	{none, []} -> del_state(Nidx, GenKey);
	_ -> update_affiliation(Nidx, GenKey, Affiliation)
    end.

%% @doc <p>Return the current subscriptions for the given user</p>
%% <p>The default module reads subscriptions in the main Mnesia
%% <tt>pubsub_state</tt> table. If a plugin stores its data in the same
%% table, it should return an empty list, as the affiliation will be read by
%% the default PubSub module. Otherwise, it should return its own affiliation,
%% that will be added to the affiliation stored in the main
%% <tt>pubsub_state</tt> table.</p>
get_entity_subscriptions(Host, Owner) ->
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
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

%% do the same as get_entity_subscriptions but filter result only to
%% nodes having send_last_published_item=on_sub_and_presence
%% as this call avoid seeking node, it must return node and type as well
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
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
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
    SubKey = jlib:jid_tolower(Owner),
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
    SubKey = jlib:jid_tolower(Owner),
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

new_subscription(Nidx, Owner, Subscription, SubState) ->
    {result, SubId} = pubsub_subscription_odbc:subscribe_node(Owner, Nidx, []),
    Subscriptions = [{Subscription, SubId} | SubState#pubsub_state.subscriptions],
    set_state(SubState#pubsub_state{subscriptions = Subscriptions}),
    {Subscription, SubId}.

unsub_with_subid(Nidx, SubId, SubState) ->
    pubsub_subscription_odbc:unsubscribe_node(SubState#pubsub_state.stateid, Nidx, SubId),
    NewSubs = [{S, Sid}
	    || {S, Sid} <- SubState#pubsub_state.subscriptions,
		SubId =/= Sid],
    case {NewSubs, SubState#pubsub_state.affiliation} of
	{[], none} -> del_state(Nidx, element(1, SubState#pubsub_state.stateid));
	_ -> set_state(SubState#pubsub_state{subscriptions = NewSubs})
    end.

%% @doc <p>Returns a list of Owner's nodes on Host with pending
%% subscriptions.</p>
get_pending_nodes(Host, Owner) ->
    GenKey = jlib:jid_remove_resource(jlib:jid_tolower(Owner)),
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

%% @doc Returns the list of stored states for a given node.
%% <p>For the default PubSub module, states are stored in Mnesia database.</p>
%% <p>We can consider that the pubsub_state table have been created by the main
%% mod_pubsub module.</p>
%% <p>PubSub plugins can store the states where they wants (for example in a
%% relational database).</p>
%% <p>If a PubSub plugin wants to delegate the states storage to the default node,
%% they can implement this function like this:
%% ```get_states(Nidx) ->
%%           node_default:get_states(Nidx).'''</p>
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

%% @doc <p>Returns a state (one state list), given its reference.</p>
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

%% @doc <p>Write a state into database.</p>
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

%% @doc <p>Delete a state from database.</p>
del_state(Nidx, JID) ->
    J = encode_jid(JID),
    catch ejabberd_odbc:sql_query_t([<<"delete from pubsub_state where jid='">>,
	    J, <<"' and nodeid='">>, Nidx, <<"';">>]),
    ok.

%% @doc Returns the list of stored items for a given node.
%% <p>For the default PubSub module, items are stored in Mnesia database.</p>
%% <p>We can consider that the pubsub_item table have been created by the main
%% mod_pubsub module.</p>
%% <p>PubSub plugins can store the items where they wants (for example in a
%% relational database), or they can even decide not to persist any items.</p>
%% <p>If a PubSub plugin wants to delegate the item storage to the default node,
%% they can implement this function like this:
%% ```get_items(Nidx, From) ->
%%           node_default:get_items(Nidx, From).'''</p>
get_items(Nidx, _From) ->
    case catch
	ejabberd_odbc:sql_query_t([<<"select itemid, publisher, creation, modification, payload "
		    "from pubsub_item where nodeid='">>, Nidx,
		<<"' order by modification desc;">>])
    of
	{selected,
		    [<<"itemid">>, <<"publisher">>, <<"creation">>, <<"modification">>, <<"payload">>], RItems} ->
	    {result, [raw_to_item(Nidx, RItem) || RItem <- RItems]};
	_ ->
	    {result, []}
    end.

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

get_items(Nidx, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    get_items(Nidx, JID, AccessModel, PresenceSubscription, RosterGroup, SubId, none).

get_items(Nidx, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId, RSM) ->
    SubKey = jlib:jid_tolower(JID),
    GenKey = jlib:jid_remove_resource(SubKey),
    {Affiliation, Subscriptions} = select_affiliation_subscriptions(Nidx, GenKey, SubKey),
    Whitelisted = can_fetch_item(Affiliation, Subscriptions),
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

%% @doc <p>Returns an item (one item list), given its reference.</p>
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
    SubKey = jlib:jid_tolower(JID),
    GenKey = jlib:jid_remove_resource(SubKey),
    {Affiliation, Subscriptions} = select_affiliation_subscriptions(Nidx, GenKey, SubKey),
    Whitelisted = can_fetch_item(Affiliation, Subscriptions),
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

%% @doc <p>Write an item into database.</p>
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

%% @doc <p>Delete an item from database.</p>
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
    node_hometree:node_to_path(Node).

path_to_node(Path) ->
    node_hometree:path_to_node(Path).

can_fetch_item(owner, _) -> true;
can_fetch_item(member, _) -> true;
can_fetch_item(publisher, _) -> true;
can_fetch_item(publish_only, _) -> false;
can_fetch_item(outcast, _) -> false;
can_fetch_item(none, Subscriptions) -> is_subscribed(Subscriptions).
%can_fetch_item(_Affiliation, _Subscription) -> false.

is_subscribed(Subscriptions) ->
    lists:any(fun
	    ({subscribed, _SubId}) -> true;
	    (_) -> false
	end,
	Subscriptions).

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
    jlib:jid_tolower(jlib:string_to_jid(SJID)).

-spec(decode_affiliation/1 ::
    (   Arg :: binary())
    -> atom()
    ).
decode_affiliation(<<"o">>) -> owner;
decode_affiliation(<<"p">>) -> publisher;
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
    ejabberd_odbc:escape(jlib:jid_to_string(JID)).

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
