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

-module(node_flat).
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
    path_to_node/1, can_fetch_item/2, is_subscribed/1]).

init(_Host, _ServerHost, _Opts) ->
    %%pubsub_subscription:init(),
    mnesia:create_table(pubsub_state,
	[{disc_copies, [node()]},
	    {type, ordered_set},
	    {attributes, record_info(fields, pubsub_state)}]),
    mnesia:create_table(pubsub_item,
	[{disc_only_copies, [node()]},
	    {attributes, record_info(fields, pubsub_item)}]),
    ItemsFields = record_info(fields, pubsub_item),
    case mnesia:table_info(pubsub_item, attributes) of
	ItemsFields -> ok;
	_ -> mnesia:transform_table(pubsub_item, ignore, ItemsFields)
    end,
    ok.

terminate(_Host, _ServerHost) ->
    ok.

options() ->
    [{deliver_payloads, true},
	{notify_config, false},
	{notify_delete, false},
	{notify_retract, true},
	{purge_offline, false},
	{persist_items, true},
	{max_items, ?MAXITEMS},
	{subscribe, true},
	{access_model, open},
	{roster_groups_allowed, []},
	{publish_model, publishers},
	{notification_type, headline},
	{max_payload_size, ?MAX_PAYLOAD_SIZE},
	{send_last_published_item, on_sub_and_presence},
	{deliver_notifications, true},
	{presence_based_delivery, false}].

features() ->
    [<<"create-nodes">>,
	<<"auto-create">>,
	<<"access-authorize">>,
	<<"delete-nodes">>,
	<<"delete-items">>,
	<<"get-pending">>,
	<<"instant-nodes">>,
	<<"manage-subscriptions">>,
	<<"modify-affiliations">>,
	<<"outcast-affiliation">>,
	<<"persistent-items">>,
	<<"publish">>,
	<<"publish-only-affiliation">>,
	<<"purge-nodes">>,
	<<"retract-items">>,
	<<"retrieve-affiliations">>,
	<<"retrieve-items">>,
	<<"retrieve-subscriptions">>,
	<<"subscribe">>,
	<<"subscription-notifications">>].
%%<<"subscription-options">>

%% @doc Checks if the current user has the permission to create the requested node
%% <p>In flat node, any unused node name is allowed. The access parameter is also
%% checked. This parameter depends on the value of the
%% <tt>access_createnode</tt> ACL value in ejabberd config file.</p>
create_node_permission(Host, ServerHost, _Node, _ParentNode, Owner, Access) ->
    LOwner = jid:tolower(Owner),
    Allowed = case LOwner of
	{<<"">>, Host, <<"">>} ->
	    true; % pubsub service always allowed
	_ ->
	    acl:match_rule(ServerHost, Access, LOwner) =:= allow
    end,
    {result, Allowed}.

create_node(Nidx, Owner) ->
    OwnerKey = jid:tolower(jid:remove_resource(Owner)),
    set_state(#pubsub_state{stateid = {OwnerKey, Nidx},
	    affiliation = owner}),
    {result, {default, broadcast}}.

delete_node(Nodes) ->
    Tr = fun (#pubsub_state{stateid = {J, _}, subscriptions = Ss}) ->
	    lists:map(fun (S) -> {J, S} end, Ss)
    end,
    Reply = lists:map(fun (#pubsub_node{id = Nidx} = PubsubNode) ->
		    {result, States} = get_states(Nidx),
		    lists:foreach(fun (#pubsub_state{stateid = {LJID, _}, items = Items}) ->
				del_items(Nidx, Items),
				del_state(Nidx, LJID)
			end, States),
		    {PubsubNode, lists:flatmap(Tr, States)}
	    end, Nodes),
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
	    SendLast, PresenceSubscription, RosterGroup, _Options) ->
    SubKey = jid:tolower(Subscriber),
    GenKey = jid:remove_resource(SubKey),
    Authorized = jid:tolower(jid:remove_resource(Sender)) == GenKey,
    GenState = get_state(Nidx, GenKey),
    SubState = case SubKey of
	GenKey -> GenState;
	_ -> get_state(Nidx, SubKey)
    end,
    Affiliation = GenState#pubsub_state.affiliation,
    Subscriptions = SubState#pubsub_state.subscriptions,
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
	    %%SubId = pubsub_subscription:add_subscription(Subscriber, Nidx, Options),
	    {NewSub, SubId} = case Subscriptions of
		[{subscribed, Id}|_] ->
		    {subscribed, Id};
		[] ->
		    Id = pubsub_subscription:make_subid(),
		    Sub = case AccessModel of
			authorize -> pending;
			_ -> subscribed
		    end,
		    set_state(SubState#pubsub_state{subscriptions =
			    [{Sub, Id} | Subscriptions]}),
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

%% @doc <p>Unsubscribe the <tt>Subscriber</tt> from the <tt>Node</tt>.</p>
unsubscribe_node(Nidx, Sender, Subscriber, SubId) ->
    SubKey = jid:tolower(Subscriber),
    GenKey = jid:remove_resource(SubKey),
    Authorized = jid:tolower(jid:remove_resource(Sender)) == GenKey,
    GenState = get_state(Nidx, GenKey),
    SubState = case SubKey of
	GenKey -> GenState;
	_ -> get_state(Nidx, SubKey)
    end,
    Subscriptions = lists:filter(fun
		({_Sub, _SubId}) -> true;
		(_SubId) -> false
	    end,
	    SubState#pubsub_state.subscriptions),
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
		    SubState#pubsub_state.subscriptions),
	    case Sub of
		{value, S} ->
		    delete_subscriptions(SubKey, Nidx, [S], SubState),
		    {result, default};
		false ->
		    {error,
			?ERR_EXTENDED((?ERR_UNEXPECTED_REQUEST_CANCEL), <<"not-subscribed">>)}
	    end;
	%% Asking to remove all subscriptions to the given node
	SubId == all ->
	    delete_subscriptions(SubKey, Nidx, Subscriptions, SubState),
	    {result, default};
	%% No subid supplied, but there's only one matching subscription
	length(Subscriptions) == 1 ->
	    delete_subscriptions(SubKey, Nidx, Subscriptions, SubState),
	    {result, default};
	%% No subid and more than one possible subscription match.
	true ->
	    {error,
		?ERR_EXTENDED((?ERR_BAD_REQUEST), <<"subid-required">>)}
    end.

delete_subscriptions(SubKey, Nidx, Subscriptions, SubState) ->
    NewSubs = lists:foldl(fun ({Subscription, SubId}, Acc) ->
		    %%pubsub_subscription:delete_subscription(SubKey, Nidx, SubId),
		    Acc -- [{Subscription, SubId}]
	    end, SubState#pubsub_state.subscriptions, Subscriptions),
    case {SubState#pubsub_state.affiliation, NewSubs} of
	{none, []} -> del_state(Nidx, SubKey);
	_          -> set_state(SubState#pubsub_state{subscriptions = NewSubs})
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
    SubKey = jid:tolower(Publisher),
    GenKey = jid:remove_resource(SubKey),
    GenState = get_state(Nidx, GenKey),
    SubState = case SubKey of
	GenKey -> GenState;
	_ -> get_state(Nidx, SubKey)
    end,
    Affiliation = GenState#pubsub_state.affiliation,
    Subscribed = case PublishModel of
	subscribers -> is_subscribed(GenState#pubsub_state.subscriptions) orelse
		       is_subscribed(SubState#pubsub_state.subscriptions);
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
		    Now = p1_time_compat:timestamp(),
		    PubId = {Now, SubKey},
		    Item = case get_item(Nidx, ItemId) of
			{result, OldItem} ->
			    OldItem#pubsub_item{modification = PubId,
				payload = Payload};
			_ ->
			    #pubsub_item{itemid = {ItemId, Nidx},
				creation = {Now, GenKey},
				modification = PubId,
				payload = Payload}
		    end,
		    Items = [ItemId | GenState#pubsub_state.items -- [ItemId]],
		    {result, {NI, OI}} = remove_extra_items(Nidx, MaxItems, Items),
		    set_item(Item),
		    set_state(GenState#pubsub_state{items = NI}),
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
    SubKey = jid:tolower(Publisher),
    GenKey = jid:remove_resource(SubKey),
    GenState = get_state(Nidx, GenKey),
    #pubsub_state{affiliation = Affiliation, items = Items} = GenState,
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
	    case lists:member(ItemId, Items) of
		true ->
		    del_item(Nidx, ItemId),
		    set_state(GenState#pubsub_state{items = lists:delete(ItemId, Items)}),
		    {result, {default, broadcast}};
		false ->
		    case Affiliation of
			owner ->
			    {result, States} = get_states(Nidx),
			    lists:foldl(fun
				    (#pubsub_state{items = PI} = S, Res) ->
					case lists:member(ItemId, PI) of
					    true ->
						Nitems = lists:delete(ItemId, PI),
						del_item(Nidx, ItemId),
						set_state(S#pubsub_state{items = Nitems}),
						{result, {default, broadcast}};
					    false ->
						Res
					end;
				    (_, Res) ->
					Res
				end,
				{error, ?ERR_ITEM_NOT_FOUND}, States);
			_ ->
			    {error, ?ERR_ITEM_NOT_FOUND}
		    end
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
		    (#pubsub_state{items = []}) ->
			ok;
		    (#pubsub_state{items = Items} = S) ->
			del_items(Nidx, Items),
			set_state(S#pubsub_state{items = []})
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
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    States = mnesia:match_object(#pubsub_state{stateid = {GenKey, '_'}, _ = '_'}),
    NodeTree = mod_pubsub:tree(Host),
    Reply = lists:foldl(fun (#pubsub_state{stateid = {_, N}, affiliation = A}, Acc) ->
		    case NodeTree:get_node(N) of
			#pubsub_node{nodeid = {Host, _}} = Node -> [{Node, A} | Acc];
			_ -> Acc
		    end
	    end,
	    [], States),
    {result, Reply}.

get_node_affiliations(Nidx) ->
    {result, States} = get_states(Nidx),
    Tr = fun (#pubsub_state{stateid = {J, _}, affiliation = A}) -> {J, A} end,
    {result, lists:map(Tr, States)}.

get_affiliation(Nidx, Owner) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    #pubsub_state{affiliation = Affiliation} = get_state(Nidx, GenKey),
    {result, Affiliation}.

set_affiliation(Nidx, Owner, Affiliation) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    GenState = get_state(Nidx, GenKey),
    case {Affiliation, GenState#pubsub_state.subscriptions} of
	{none, []} -> del_state(Nidx, GenKey);
	_ -> set_state(GenState#pubsub_state{affiliation = Affiliation})
    end.

%% @doc <p>Return the current subscriptions for the given user</p>
%% <p>The default module reads subscriptions in the main Mnesia
%% <tt>pubsub_state</tt> table. If a plugin stores its data in the same
%% table, it should return an empty list, as the affiliation will be read by
%% the default PubSub module. Otherwise, it should return its own affiliation,
%% that will be added to the affiliation stored in the main
%% <tt>pubsub_state</tt> table.</p>
get_entity_subscriptions(Host, Owner) ->
    {U, D, _} = SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    States = case SubKey of
	GenKey ->
	    mnesia:match_object(#pubsub_state{stateid = {{U, D, '_'}, '_'}, _ = '_'});
	_ ->
	    mnesia:match_object(#pubsub_state{stateid = {GenKey, '_'}, _ = '_'})
	    ++
	    mnesia:match_object(#pubsub_state{stateid = {SubKey, '_'}, _ = '_'})
    end,
    NodeTree = mod_pubsub:tree(Host),
    Reply = lists:foldl(fun (#pubsub_state{stateid = {J, N}, subscriptions = Ss}, Acc) ->
		    case NodeTree:get_node(N) of
			#pubsub_node{nodeid = {Host, _}} = Node ->
			    lists:foldl(fun ({Sub, SubId}, Acc2) ->
					[{Node, Sub, SubId, J} | Acc2]
				end,
				Acc, Ss);
			_ ->
			    Acc
		    end
	    end,
	    [], States),
    {result, Reply}.

get_node_subscriptions(Nidx) ->
    {result, States} = get_states(Nidx),
    Tr = fun (#pubsub_state{stateid = {J, _}, subscriptions = Subscriptions}) ->
	    case Subscriptions of
		[_ | _] ->
		    lists:foldl(fun ({S, SubId}, Acc) ->
				[{J, S, SubId} | Acc]
			end,
			[], Subscriptions);
		[] ->
		    [];
		_ ->
		    [{J, none}]
	    end
    end,
    {result, lists:flatmap(Tr, States)}.

get_subscriptions(Nidx, Owner) ->
    SubKey = jid:tolower(Owner),
    SubState = get_state(Nidx, SubKey),
    {result, SubState#pubsub_state.subscriptions}.

set_subscriptions(Nidx, Owner, Subscription, SubId) ->
    SubKey = jid:tolower(Owner),
    SubState = get_state(Nidx, SubKey),
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

new_subscription(_Nidx, _Owner, Sub, SubState) ->
    %%SubId = pubsub_subscription:add_subscription(Owner, Nidx, []),
    SubId = pubsub_subscription:make_subid(),
    Subs = SubState#pubsub_state.subscriptions,
    set_state(SubState#pubsub_state{subscriptions = [{Sub, SubId} | Subs]}),
    {Sub, SubId}.

unsub_with_subid(Nidx, SubId, #pubsub_state{stateid = {Entity, _}} = SubState) ->
    %%pubsub_subscription:delete_subscription(SubState#pubsub_state.stateid, Nidx, SubId),
    NewSubs = [{S, Sid}
	    || {S, Sid} <- SubState#pubsub_state.subscriptions,
		SubId =/= Sid],
    case {NewSubs, SubState#pubsub_state.affiliation} of
	{[], none} -> del_state(Nidx, Entity);
	_ -> set_state(SubState#pubsub_state{subscriptions = NewSubs})
    end.

%% @doc <p>Returns a list of Owner's nodes on Host with pending
%% subscriptions.</p>
get_pending_nodes(Host, Owner) ->
    GenKey = jid:remove_resource(jid:tolower(Owner)),
    States = mnesia:match_object(#pubsub_state{stateid = {GenKey, '_'},
		affiliation = owner,
		_ = '_'}),
    NodeIdxs = [Nidx || #pubsub_state{stateid = {_, Nidx}} <- States],
    NodeTree = mod_pubsub:tree(Host),
    Reply = mnesia:foldl(fun (#pubsub_state{stateid = {_, Nidx}} = S, Acc) ->
		    case lists:member(Nidx, NodeIdxs) of
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
    States = case catch mnesia:match_object(
	    #pubsub_state{stateid = {'_', Nidx}, _ = '_'}) of
	List when is_list(List) -> List;
	_ -> []
    end,
    {result, States}.

%% @doc <p>Returns a state (one state list), given its reference.</p>
get_state(Nidx, Key) ->
    StateId = {Key, Nidx},
    case catch mnesia:read({pubsub_state, StateId}) of
	[State] when is_record(State, pubsub_state) -> State;
	_ -> #pubsub_state{stateid = StateId}
    end.

%% @doc <p>Write a state into database.</p>
set_state(State) when is_record(State, pubsub_state) ->
    mnesia:write(State).
%set_state(_) -> {error, ?ERR_INTERNAL_SERVER_ERROR}.

%% @doc <p>Delete a state from database.</p>
del_state(Nidx, Key) ->
    mnesia:delete({pubsub_state, {Key, Nidx}}).

%% @doc Returns the list of stored items for a given node.
%% <p>For the default PubSub module, items are stored in Mnesia database.</p>
%% <p>We can consider that the pubsub_item table have been created by the main
%% mod_pubsub module.</p>
%% <p>PubSub plugins can store the items where they wants (for example in a
%% relational database), or they can even decide not to persist any items.</p>
get_items(Nidx, _From, _RSM) ->
    Items = mnesia:match_object(#pubsub_item{itemid = {'_', Nidx}, _ = '_'}),
    {result, {lists:reverse(lists:keysort(#pubsub_item.modification, Items)), none}}.

get_items(Nidx, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId, RSM) ->
    SubKey = jid:tolower(JID),
    GenKey = jid:remove_resource(SubKey),
    GenState = get_state(Nidx, GenKey),
    SubState = get_state(Nidx, SubKey),
    Affiliation = GenState#pubsub_state.affiliation,
    BareSubscriptions = GenState#pubsub_state.subscriptions,
    FullSubscriptions = SubState#pubsub_state.subscriptions,
    Whitelisted = can_fetch_item(Affiliation, BareSubscriptions) orelse
		  can_fetch_item(Affiliation, FullSubscriptions),
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

%% @doc <p>Returns an item (one item list), given its reference.</p>

get_item(Nidx, ItemId) ->
    case mnesia:read({pubsub_item, {ItemId, Nidx}}) of
	[Item] when is_record(Item, pubsub_item) -> {result, Item};
	_ -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

get_item(Nidx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId) ->
    SubKey = jid:tolower(JID),
    GenKey = jid:remove_resource(SubKey),
    GenState = get_state(Nidx, GenKey),
    Affiliation = GenState#pubsub_state.affiliation,
    Subscriptions = GenState#pubsub_state.subscriptions,
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
set_item(Item) when is_record(Item, pubsub_item) ->
    mnesia:write(Item).
%set_item(_) -> {error, ?ERR_INTERNAL_SERVER_ERROR}.

%% @doc <p>Delete an item from database.</p>
del_item(Nidx, ItemId) ->
    mnesia:delete({pubsub_item, {ItemId, Nidx}}).

del_items(Nidx, ItemIds) ->
    lists:foreach(fun (ItemId) -> del_item(Nidx, ItemId)
	end,
	ItemIds).

get_item_name(_Host, _Node, Id) ->
    Id.

%% @doc <p>Return the path of the node. In flat it's just node id.</p>
node_to_path(Node) ->
    [(Node)].

path_to_node(Path) ->
    case Path of
	% default slot
	[Node] -> iolist_to_binary(Node);
	% handle old possible entries, used when migrating database content to new format
	[Node | _] when is_binary(Node) ->
	    iolist_to_binary(str:join([<<"">> | Path], <<"/">>));
	% default case (used by PEP for example)
	_ -> iolist_to_binary(Path)
    end.

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
