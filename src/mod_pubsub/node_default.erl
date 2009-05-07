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

-module(node_default).
-author('christophe.romain@process-one.net').

-include_lib("exmpp/include/exmpp.hrl").

-include("pubsub.hrl").

-behaviour(gen_pubsub_node).

%% API definition
-export([init/3, terminate/2,
	 options/0, features/0,
	 create_node_permission/6,
	 create_node/2,
	 delete_node/1,
	 purge_node/2,
	 subscribe_node/7,
	 unsubscribe_node/4,
	 publish_item/6,
	 delete_item/4,
	 remove_extra_items/3,
	 get_entity_affiliations/2,
	 get_node_affiliations/1,
	 get_affiliation/2,
	 set_affiliation/3,
	 get_entity_subscriptions/2,
	 get_node_subscriptions/1,
	 get_subscription/2,
	 set_subscription/3,
	 get_states/1,
	 get_state/2,
	 set_state/1,
	 get_items/6,
	 get_items/2,
	 get_item/7,
	 get_item/2,
	 set_item/1,
	 get_item_name/3
	]).

%% ================
%% API definition
%% ================

%% @spec (Host, ServerHost, Opts) -> any()
%%	 Host = mod_pubsub:host()
%%	 ServerHost = mod_pubsub:host()
%%	 Opts = list()
%% @doc <p>Called during pubsub modules initialisation. Any pubsub plugin must
%% implement this function. It can return anything.</p>
%% <p>This function is mainly used to trigger the setup task necessary for the
%% plugin. It can be used for example by the developer to create the specific
%% module database schema if it does not exists yet.</p>
init(_Host, _ServerHost, _Opts) ->
    mnesia:create_table(pubsub_state,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, pubsub_state)}]),
    StatesFields = record_info(fields, pubsub_state),
    case mnesia:table_info(pubsub_state, attributes) of
	StatesFields -> ok;
	_ ->
	    mnesia:transform_table(pubsub_state, ignore, StatesFields)
    end,
    mnesia:create_table(pubsub_item,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, pubsub_item)}]),
    ItemsFields = record_info(fields, pubsub_item),
    case mnesia:table_info(pubsub_item, attributes) of
	ItemsFields -> ok;
	_ ->
	    mnesia:transform_table(pubsub_item, ignore, ItemsFields)
    end,
    ok.

%% @spec (Host, ServerHost) -> any()
%%	 Host = mod_pubsub:host()
%%	 ServerHost = host()
%% @doc <p>Called during pubsub modules termination. Any pubsub plugin must
%% implement this function. It can return anything.</p>
terminate(_Host, _ServerHost) ->
    ok.

%% @spec () -> [Option]
%%	 Option = mod_pubsub:nodeOption()
%% @doc Returns the default pubsub node options.
%% <p>Example of function return value:</p>
%%	```
%%	 [{deliver_payloads, true},
%%	  {notify_config, false},
%%	  {notify_delete, false},
%%	  {notify_retract, true},
%%	  {persist_items, true},
%%	  {max_items, 10},
%%	  {subscribe, true},
%%	  {access_model, open},
%%	  {publish_model, publishers},
%%	  {max_payload_size, 100000},
%%	  {send_last_published_item, never},
%%	  {presence_based_delivery, false}]'''
options() ->
    [{node_type, default},
     {deliver_payloads, true},
     {notify_config, false},
     {notify_delete, false},
     {notify_retract, true},
     {persist_items, true},
     {max_items, ?MAXITEMS div 2},
     {subscribe, true},
     {access_model, open},
     {roster_groups_allowed, []},
     {publish_model, publishers},
     {max_payload_size, ?MAX_PAYLOAD_SIZE},
     {send_last_published_item, on_sub_and_presence},
     {deliver_notifications, true},
     {presence_based_delivery, false}].

%% @spec () -> []
%% @doc Returns the node features
features() ->
    ["create-nodes",
     "auto-create",
     "delete-nodes",
     "delete-items",
     "instant-nodes",
     "manage-subscriptions",
     "modify-affiliations",
     "outcast-affiliation",
     "persistent-items",
     "publish",
     "purge-nodes",
     "retract-items",
     "retrieve-affiliations",
     "retrieve-items",
     "retrieve-subscriptions",
     "subscribe",
     "subscription-notifications"
    ].

%% @spec (Host, ServerHost, Node, ParentNode, Owner, Access) -> bool()
%%	 Host = mod_pubsub:host()
%%	 ServerHost = mod_pubsub:host()
%%	 Node = mod_pubsub:pubsubNode()
%%	 ParentNode = mod_pubsub:pubsubNode()
%%	 Owner = mod_pubsub:jid()
%%	 Access = all | atom()
%% @doc Checks if the current user has the permission to create the requested node
%% <p>In {@link node_default}, the permission is decided by the place in the
%% hierarchy where the user is creating the node. The access parameter is also
%% checked in the default module. This parameter depends on the value of the
%% <tt>access_createnode</tt> ACL value in ejabberd config file.</p>
%% <p>This function also check that node can be created a a children of its
%% parent node</p>
%% <p>PubSub plugins can redefine the PubSub node creation rights as they
%% which. They can simply delegate this check to the {@link node_default}
%% module by implementing this function like this:
%% ```check_create_user_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
%%	   node_default:check_create_user_permission(Host, ServerHost, Node, ParentNode, Owner, Access).'''</p>
create_node_permission(Host, ServerHost, Node, _ParentNode, Owner, Access) ->
    LOwner = jlib:short_prepd_jid(Owner),
    {User, Server, _Resource} = LOwner,
    Allowed = case LOwner of
	{undefined, Host, undefined} ->
	    true; % pubsub service always allowed
	_ ->
	    {LU, LS, LR} = LOwner,
	    case acl:match_rule(ServerHost, Access, exmpp_jid:make_jid(LU, LS, LR)) of
		allow ->
		    case Node of
			["home", Server, User | _] -> true;
			_ -> false
		    end;
		_ ->
		    false
	    end
    end,
    {result, Allowed}.

%% @spec (NodeId, Owner) ->
%%		  {result, Result} | exit
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 Owner = mod_pubsub:jid()
%% @doc <p></p>
create_node(NodeId, Owner) ->
    OwnerKey = jlib:short_prepd_bare_jid(Owner),
    set_state(#pubsub_state{stateid = {OwnerKey, NodeId}, affiliation = owner}),
    {result, {default, broadcast}}.

%% @spec (Removed) -> ok
%%	 Removed = [mod_pubsub:pubsubNode()]
%% @doc <p>purge items of deleted nodes after effective deletion.</p>
delete_node(Removed) ->
    Tr = fun(#pubsub_state{stateid = {J, _}, subscription = S}) ->
		 {J, S}
	 end,
    Reply = lists:map(
	fun(#pubsub_node{id = NodeId} = PubsubNode) ->
	    {result, States} = get_states(NodeId),
	    lists:foreach(
		fun(#pubsub_state{stateid = {LJID, _}, items = Items}) ->
		    del_items(NodeId, Items),
		    del_state(NodeId, LJID)
	    end, States),
	    {PubsubNode, lists:map(Tr, States)}
	end, Removed),
    {result, {default, broadcast, Reply}}.

%% @spec (NodeId, Sender, Subscriber, AccessModel, SendLast, PresenceSubscription, RosterGroup) ->
%%		 {error, Reason} | {result, Result}
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
subscribe_node(NodeId, Sender, Subscriber, AccessModel,
	       SendLast, PresenceSubscription, RosterGroup) ->
    SubKey = jlib:short_prepd_jid(Subscriber),
    GenKey = jlib:short_prepd_bare_jid(SubKey),
    Authorized = (jlib:short_prepd_bare_jid(Sender) == GenKey),
    GenState = get_state(NodeId, GenKey),
    SubState = case SubKey of
	GenKey -> GenState;
	_ -> get_state(NodeId, SubKey)
	end,
    Affiliation = GenState#pubsub_state.affiliation,
    Subscription = SubState#pubsub_state.subscription,
    Whitelisted = lists:member(Affiliation, [member, publisher, owner]),
    if
	not Authorized ->
	    %% JIDs do not match
	    {error, ?ERR_EXTENDED('bad-request', "invalid-jid")};
	Affiliation == outcast ->
	    %% Requesting entity is blocked
	    {error, 'forbidden'};
	Subscription == pending ->
	    %% Requesting entity has pending subscription
	    {error, ?ERR_EXTENDED('not-authorized', "pending-subscription")};
	(AccessModel == presence) and (not PresenceSubscription) ->
	    %% Entity is not authorized to create a subscription (presence subscription required)
	    {error, ?ERR_EXTENDED('not-authorized', "presence-subscription-required")};
	(AccessModel == roster) and (not RosterGroup) ->
	    %% Entity is not authorized to create a subscription (not in roster group)
	    {error, ?ERR_EXTENDED('not-authorized', "not-in-roster-group")};
	(AccessModel == whitelist) and (not Whitelisted) ->
	    %% Node has whitelist access model and entity lacks required affiliation
	    {error, ?ERR_EXTENDED('not-allowed', "closed-node")};
	(AccessModel == authorize) -> % TODO: to be done
	    %% Node has authorize access model
	    {error, 'forbidden'};
	%%MustPay ->
	%%	% Payment is required for a subscription
	%%	{error, ?ERR_PAYMENT_REQUIRED};
	%%ForbiddenAnonymous ->
	%%	% Requesting entity is anonymous
	%%	{error, 'forbidden'};
	true ->
	    NewSubscription =
		if
		    AccessModel == authorize ->
			pending;
		    %%NeedConfiguration ->
		    %%	unconfigured
		    true ->
			subscribed
		end,
	    set_state(SubState#pubsub_state{subscription = NewSubscription}),
	    case NewSubscription of
		subscribed ->
		    case SendLast of
			never -> {result, {default, NewSubscription}};
			_ -> {result, {default, NewSubscription, send_last}}
		    end;
		_ ->
		    {result, {default, NewSubscription}}
	    end
    end.

%% @spec (NodeId, Sender, Subscriber, SubID) ->
%%			{error, Reason} | {result, []}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 Sender = mod_pubsub:jid()
%%	 Subscriber = mod_pubsub:jid()
%%	 SubID = string()
%%	 Reason = mod_pubsub:stanzaError()
%% @doc <p>Unsubscribe the <tt>Subscriber</tt> from the <tt>Node</tt>.</p>
unsubscribe_node(NodeId, Sender, Subscriber, _SubId) ->
    SubKey = jlib:short_prepd_jid(Subscriber),
    GenKey = jlib:short_prepd_bare_jid(SubKey),
    Authorized = (jlib:short_prepd_bare_jid(Sender) == GenKey),
    GenState = get_state(NodeId, GenKey),
    SubState = case SubKey of
	GenKey -> GenState;
	_ -> get_state(NodeId, SubKey)
	end,
    if
	%% Requesting entity is prohibited from unsubscribing entity
	not Authorized ->
	    {error, 'forbidden'};
	%% Entity did not specify SubID
	%%SubID == "", ?? ->
	%%	{error, ?ERR_EXTENDED('bad-request', "subid-required")};
	%% Invalid subscription identifier
	%%InvalidSubID ->
	%%	{error, ?ERR_EXTENDED('not-acceptable', "invalid-subid")};
	%% Requesting entity is not a subscriber
	SubState#pubsub_state.subscription == none ->
	    {error, ?ERR_EXTENDED('unexpected-request', "not-subscribed")};
	%% Was just subscriber, remove the record
	SubState#pubsub_state.affiliation == none ->
	    del_state(NodeId, SubKey),
	    {result, default};
	true ->
	    set_state(SubState#pubsub_state{subscription = none}),
	    {result, default}
    end.

%% @spec (NodeId, Publisher, PublishModel, MaxItems, ItemId, Payload) ->
%%		 {true, PubsubItem} | {result, Reply}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 Publisher = mod_pubsub:jid()
%%	 PublishModel = atom()
%%	 MaxItems = integer()
%%	 ItemId = string()
%%	 Payload = term()
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
publish_item(NodeId, Publisher, PublishModel, MaxItems, ItemId, Payload) ->
    SubKey = jlib:short_prepd_jid(Publisher),
    GenKey = jlib:short_prepd_bare_jid(SubKey),
    GenState = get_state(NodeId, GenKey),
    SubState = case SubKey of
	GenKey -> GenState;
	_ -> get_state(NodeId, SubKey)
	end,
    Affiliation = GenState#pubsub_state.affiliation,
    Subscription = SubState#pubsub_state.subscription,
    if
	not ((PublishModel == open)
	     or ((PublishModel == publishers)
		 and ((Affiliation == owner) or (Affiliation == publisher)))
	     or ((PublishModel == subscribers)
		 and (Subscription == subscribed))) ->
	    %% Entity does not have sufficient privileges to publish to node
	    {error, 'forbidden'};
	true ->
	    %% TODO: check creation, presence, roster
	    if MaxItems > 0 ->
		PubId = {now(), SubKey},
		Item = case get_item(NodeId, ItemId) of
		       {result, OldItem} ->
			   OldItem#pubsub_item{modification = PubId,
					       payload = Payload};
		       _ ->
			   #pubsub_item{itemid = {ItemId, NodeId},
					creation = {now(), GenKey},
					modification = PubId,
					payload = Payload}
		   end,
		Items = [ItemId | GenState#pubsub_state.items--[ItemId]],
		{result, {NI, OI}} = remove_extra_items(NodeId, MaxItems, Items),
		set_item(Item),
		set_state(GenState#pubsub_state{items = NI}),
		{result, {default, broadcast, OI}};
	       true ->
		{result, {default, broadcast, []}}
	    end
    end.

%% @spec (NodeId, MaxItems, ItemIds) -> {NewItemIds,OldItemIds}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 MaxItems = integer() | unlimited
%%	 ItemIds = [ItemId::string()]
%%	 NewItemIds = [ItemId::string()]
%% @doc <p>This function is used to remove extra items, most notably when the
%% maximum number of items has been reached.</p>
%% <p>This function is used internally by the core PubSub module, as no
%% permission check is performed.</p>
%% <p>In the default plugin module, the oldest items are removed, but other
%% rules can be used.</p>
%% <p>If another PubSub plugin wants to delegate the item removal (and if the
%% plugin is using the default pubsub storage), it can implements this function like this:
%% ```remove_extra_items(NodeId, MaxItems, ItemIds) ->
%%	   node_default:remove_extra_items(NodeId, MaxItems, ItemIds).'''</p>
remove_extra_items(_NodeId, unlimited, ItemIds) ->
    {result, {ItemIds, []}};
remove_extra_items(NodeId, MaxItems, ItemIds) ->
    NewItems = lists:sublist(ItemIds, MaxItems),
    OldItems = lists:nthtail(length(NewItems), ItemIds),
    %% Remove extra items:
    del_items(NodeId, OldItems),
    %% Return the new items list:
    {result, {NewItems, OldItems}}.

%% @spec (NodeId, Publisher, PublishModel, ItemId) ->
%%		  {error, Reason::stanzaError()} |
%%		  {result, []}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 Publisher = mod_pubsub:jid()
%%	 PublishModel = atom()
%%	 ItemId = string()
%% @doc <p>Triggers item deletion.</p>
%% <p>Default plugin: The user performing the deletion must be the node owner
%% or a publisher.</p>
delete_item(NodeId, Publisher, PublishModel, ItemId) ->
    GenKey = jlib:short_prepd_bare_jid(Publisher),
    GenState = get_state(NodeId, GenKey),
    #pubsub_state{affiliation = Affiliation, items = Items} = GenState,
    Allowed = (Affiliation == publisher) orelse (Affiliation == owner)
	orelse (PublishModel == open)
	orelse case get_item(NodeId, ItemId) of
		   {result, #pubsub_item{creation = {_, GenKey}}} -> true;
		   _ -> false
	       end,
    if
	not Allowed ->
	    %% Requesting entity does not have sufficient privileges
	    {error, 'forbidden'};
	true ->
	    case lists:member(ItemId, Items) of
		true ->
		    del_item(NodeId, ItemId),
		    NewItems = lists:delete(ItemId, Items),
		    set_state(GenState#pubsub_state{items = NewItems}),
		    {result, {default, broadcast}};
		false ->
		    %% Non-existent node or item
		    {error, 'item-not-found'}
	    end
    end.

%% @spec (NodeId, Owner) ->
%%		  {error, Reason::stanzaError()} |
%%		  {result, {default, broadcast}}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 Owner = mod_pubsub:jid()
purge_node(NodeId, Owner) ->
    GenKey = jlib:short_prepd_bare_jid(Owner),
    GenState = get_state(NodeId, GenKey),
    case GenState of
	#pubsub_state{items = Items, affiliation = owner} ->
	    del_items(NodeId, Items),
	    set_state(GenState#pubsub_state{items = []}),
	    {result, {default, broadcast}};
	_ ->
	    %% Entity is not owner
	    {error, 'forbidden'}
    end.

%% @spec (Host, JID) -> [{Node,Affiliation}]
%%	 Host = host()
%%	 JID = mod_pubsub:jid()
%% @doc <p>Return the current affiliations for the given user</p>
%% <p>The default module reads affiliations in the main Mnesia
%% <tt>pubsub_state</tt> table. If a plugin stores its data in the same
%% table, it should return an empty list, as the affiliation will be read by
%% the default PubSub module. Otherwise, it should return its own affiliation,
%% that will be added to the affiliation stored in the main
%% <tt>pubsub_state</tt> table.</p>
get_entity_affiliations(_Host, Owner) ->
    GenKey = jlib:short_prepd_bare_jid(Owner),
    States = mnesia:match_object(
	       #pubsub_state{stateid = {GenKey, '_'}, _ = '_'}),
    Tr = fun(#pubsub_state{stateid = {_, N}, affiliation = A}) ->
	    {get_nodename(N), A}
	 end,
    {result, lists:map(Tr, States)}.

get_node_affiliations(NodeId) ->
    {result, States} = get_states(NodeId),
    Tr = fun(#pubsub_state{stateid = {J, {_, _}}, affiliation = A}) ->
	    {J, A}
	 end,
    {result, lists:map(Tr, States)}.

get_affiliation(NodeId, Owner) ->
    GenKey = jlib:short_prepd_bare_jid(Owner),
    GenState = get_state(NodeId, GenKey),
    {result, GenState#pubsub_state.affiliation}.

set_affiliation(NodeId, Owner, Affiliation) ->
    GenKey = jlib:short_prepd_bare_jid(Owner),
    GenState = get_state(NodeId, GenKey),
    case {Affiliation, GenState#pubsub_state.subscription} of
	{none, none} ->
	    del_state(NodeId, GenKey);
	_ ->
	    set_state(GenState#pubsub_state{affiliation = Affiliation})
    end.

%% @spec (Host, Owner) -> [{Node,Subscription}]
%%	 Host = host()
%%	 Owner = mod_pubsub:jid()
%% @doc <p>Return the current subscriptions for the given user</p>
%% <p>The default module reads subscriptions in the main Mnesia
%% <tt>pubsub_state</tt> table. If a plugin stores its data in the same
%% table, it should return an empty list, as the affiliation will be read by
%% the default PubSub module. Otherwise, it should return its own affiliation,
%% that will be added to the affiliation stored in the main
%% <tt>pubsub_state</tt> table.</p>
get_entity_subscriptions(_Host, Owner) ->
    {U, D, _} = SubKey = jlib:short_prepd_jid(Owner),
    GenKey = jlib:short_prepd_bare_jid(SubKey),
    States = case SubKey of
	GenKey -> mnesia:match_object(
	       #pubsub_state{stateid = {{U, D, '_'}, '_'}, _ = '_'});
	_ -> mnesia:match_object(
	       #pubsub_state{stateid = {GenKey, '_'}, _ = '_'})
	    ++ mnesia:match_object(
	       #pubsub_state{stateid = {SubKey, '_'}, _ = '_'})
    end,
    Tr = fun(#pubsub_state{stateid = {J, N}, subscription = S}) ->
	    {get_nodename(N), S, J}
	 end,
    {result, lists:map(Tr, States)}.

get_node_subscriptions(NodeId) ->
    {result, States} = get_states(NodeId),
    Tr = fun(#pubsub_state{stateid = {J, _}, subscription = S}) ->
	    {J, S}
	 end,
    {result, lists:map(Tr, States)}.

get_subscription(NodeId, Owner) ->
    SubKey = jlib:short_prepd_jid(Owner),
    SubState = get_state(NodeId, SubKey),
    {result, SubState#pubsub_state.subscription}.

set_subscription(NodeId, Owner, Subscription) ->
    SubKey = jlib:short_prepd_jid(Owner),
    SubState = get_state(NodeId, SubKey),
    case {Subscription, SubState#pubsub_state.affiliation} of
	{none, none} ->
	    del_state(NodeId, SubKey);
	_ ->
	    set_state(SubState#pubsub_state{subscription = Subscription})
    end,
    ok.

%% @spec (NodeId) -> [States] | []
%%	 NodeId = mod_pubsub:pubsubNodeId()
%% @doc Returns the list of stored states for a given node.
%% <p>For the default PubSub module, states are stored in Mnesia database.</p>
%% <p>We can consider that the pubsub_state table have been created by the main
%% mod_pubsub module.</p>
%% <p>PubSub plugins can store the states where they wants (for example in a
%% relational database).</p>
%% <p>If a PubSub plugin wants to delegate the states storage to the default node,
%% they can implement this function like this:
%% ```get_states(NodeId) ->
%%	   node_default:get_states(NodeId).'''</p>
get_states(NodeId) ->
    States = mnesia:match_object(
	       #pubsub_state{stateid = {'_', NodeId}, _ = '_'}),
    {result, States}.

%% @spec (NodeId, JID) -> [State] | []
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 JID = mod_pubsub:jid()
%%	 State = mod_pubsub:pubsubItems()
%% @doc <p>Returns a state (one state list), given its reference.</p>
get_state(NodeId, JID) ->
    StateId = {JID, NodeId},
    case mnesia:read({pubsub_state, StateId}) of
	[State] when is_record(State, pubsub_state) -> State;
	_ -> #pubsub_state{stateid=StateId}
    end.

%% @spec (State) -> ok | {error, Reason::stanzaError()}
%%	 State = mod_pubsub:pubsubStates()
%% @doc <p>Write a state into database.</p>
set_state(State) when is_record(State, pubsub_state) ->
    mnesia:write(State);
set_state(_) ->
    {error, 'internal-server-error'}.

%% @spec (StateId) -> ok | {error, Reason::stanzaError()}
%%	 StateId = mod_pubsub:pubsubStateId()
%% @doc <p>Delete a state from database.</p>
del_state(NodeId, JID) ->
    mnesia:delete({pubsub_state, {JID, NodeId}}).

%% @spec (NodeId, From) -> [Items] | []
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 Items = mod_pubsub:pubsubItems()
%% @doc Returns the list of stored items for a given node.
%% <p>For the default PubSub module, items are stored in Mnesia database.</p>
%% <p>We can consider that the pubsub_item table have been created by the main
%% mod_pubsub module.</p>
%% <p>PubSub plugins can store the items where they wants (for example in a
%% relational database), or they can even decide not to persist any items.</p>
%% <p>If a PubSub plugin wants to delegate the item storage to the default node,
%% they can implement this function like this:
%% ```get_items(NodeId, From) ->
%%	   node_default:get_items(NodeId, From).'''</p>
get_items(NodeId, _From) ->
    Items = mnesia:match_object(
	      #pubsub_item{itemid = {'_', NodeId}, _ = '_'}),
    {result, Items}.
get_items(NodeId, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId) ->
    GenKey = jlib:short_prepd_bare_jid(JID),
    GenState = get_state(NodeId, GenKey),
    Affiliation = GenState#pubsub_state.affiliation,
    Subscription = GenState#pubsub_state.subscription,
    Whitelisted = can_fetch_item(Affiliation, Subscription),
    if
	%%SubID == "", ?? ->
	    %% Entity has multiple subscriptions to the node but does not specify a subscription ID
	    %{error, ?ERR_EXTENDED('bad-request', "subid-required")};
	%%InvalidSubID ->
	    %% Entity is subscribed but specifies an invalid subscription ID
	    %{error, ?ERR_EXTENDED('not-acceptable', "invalid-subid")};
	Affiliation == outcast ->
	    %% Requesting entity is blocked
	    {error, 'forbidden'};
	(AccessModel == presence) and (not PresenceSubscription) ->
	    %% Entity is not authorized to create a subscription (presence subscription required)
	    {error, ?ERR_EXTENDED('not-authorized', "presence-subscription-required")};
	(AccessModel == roster) and (not RosterGroup) ->
	    %% Entity is not authorized to create a subscription (not in roster group)
	    {error, ?ERR_EXTENDED('not-authorized', "not-in-roster-group")};
	(AccessModel == whitelist) and (not Whitelisted) ->
	    %% Node has whitelist access model and entity lacks required affiliation
	    {error, ?ERR_EXTENDED('not-allowed', "closed-node")};
	(AccessModel == authorize) -> % TODO: to be done
	    %% Node has authorize access model
	    {error, 'forbidden'};
	%%MustPay ->
	%%	% Payment is required for a subscription
	%%	{error, ?ERR_PAYMENT_REQUIRED};
	true ->
	    get_items(NodeId, JID)
    end.

%% @spec (NodeId, ItemId) -> [Item] | []
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 ItemId = string()
%%	 Item = mod_pubsub:pubsubItems()
%% @doc <p>Returns an item (one item list), given its reference.</p>
get_item(NodeId, ItemId) ->
    case mnesia:read({pubsub_item, {ItemId, NodeId}}) of
	[Item] when is_record(Item, pubsub_item) ->
	    {result, Item};
	_ ->
	    {error, 'item-not-found'}
    end.
get_item(NodeId, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId) ->
    GenKey = jlib:short_prepd_bare_jid(JID),
    GenState = get_state(NodeId, GenKey),
    Affiliation = GenState#pubsub_state.affiliation,
    Subscription = GenState#pubsub_state.subscription,
    Whitelisted = can_fetch_item(Affiliation, Subscription),
    if
	%%SubID == "", ?? ->
	    %% Entity has multiple subscriptions to the node but does not specify a subscription ID
	    %{error, ?ERR_EXTENDED('bad-request', "subid-required")};
	%%InvalidSubID ->
	    %% Entity is subscribed but specifies an invalid subscription ID
	    %{error, ?ERR_EXTENDED('not-acceptable', "invalid-subid")};
	Affiliation == outcast ->
	    %% Requesting entity is blocked
	    {error, 'forbidden'};
	(AccessModel == presence) and (not PresenceSubscription) ->
	    %% Entity is not authorized to create a subscription (presence subscription required)
	    {error, ?ERR_EXTENDED('not-authorized', "presence-subscription-required")};
	(AccessModel == roster) and (not RosterGroup) ->
	    %% Entity is not authorized to create a subscription (not in roster group)
	    {error, ?ERR_EXTENDED('not-authorized', "not-in-roster-group")};
	(AccessModel == whitelist) and (not Whitelisted) ->
	    %% Node has whitelist access model and entity lacks required affiliation
	    {error, ?ERR_EXTENDED('not-allowed', "closed-node")};
	(AccessModel == authorize) -> % TODO: to be done
	    %% Node has authorize access model
	    {error, 'forbidden'};
	%%MustPay ->
	%%	% Payment is required for a subscription
	%%	{error, ?ERR_PAYMENT_REQUIRED};
	true ->
	    get_item(NodeId, ItemId)
    end.

%% @spec (Item) -> ok | {error, Reason::stanzaError()}
%%	 Item = mod_pubsub:pubsubItems()
%% @doc <p>Write an item into database.</p>
set_item(Item) when is_record(Item, pubsub_item) ->
    mnesia:write(Item);
set_item(_) ->
    {error, 'internal-server-error'}.

%% @spec (NodeId, ItemId) -> ok | {error, Reason::stanzaError()}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 ItemId = string()
%% @doc <p>Delete an item from database.</p>
del_item(NodeId, ItemId) ->
    mnesia:delete({pubsub_item, {ItemId, NodeId}}).
del_items(NodeId, ItemIds) ->
    lists:foreach(fun(ItemId) ->
	del_item(NodeId, ItemId)
    end, ItemIds).

%% @doc <p>Return the name of the node if known: Default is to return
%% node id.</p>
get_item_name(_host, _Node, Id) ->
    Id.

%% @spec (Affiliation, Subscription) -> true | false
%%       Affiliation = owner | member | publisher | outcast | none
%%       Subscription = subscribed | none
%% @doc Determines if the combination of Affiliation and Subscribed
%% are allowed to get items from a node.
can_fetch_item(owner,        _)             -> true;
can_fetch_item(member,       _)             -> true;
can_fetch_item(publisher,    _)             -> true;
can_fetch_item(outcast,      _)             -> false;
can_fetch_item(none,         subscribed)    -> true;
can_fetch_item(none,         none)          -> false;
can_fetch_item(_Affiliation, _Subscription) -> false.

%% @spec (NodeId) -> Node
%% @doc retreive pubsubNode() representation giving a NodeId
get_nodename(NodeId) ->
    case mnesia:index_read(pubsub_node, NodeId, #pubsub_node.id) of
	[#pubsub_node{nodeid = {_, Node}}] -> Node;
	_ -> []
    end.
