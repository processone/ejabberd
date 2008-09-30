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
%%% Portions created by ProcessOne are Copyright 2006-2008, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2008, ProcessOne.
%%%
%%%
%%% @copyright 2006-2008 ProcessOne
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

-include("pubsub.hrl").
-include("jlib.hrl").

-behaviour(gen_pubsub_node).

%% API definition
-export([init/3, terminate/2,
	 options/0, features/0,
	 create_node_permission/6,
	 create_node/3,
	 delete_node/2,
	 purge_node/3,
	 subscribe_node/8,
	 unsubscribe_node/5,
	 publish_item/7,
	 delete_item/4,
	 remove_extra_items/4,
	 get_entity_affiliations/2,
	 get_node_affiliations/2,
	 get_affiliation/3,
	 set_affiliation/4,
	 get_entity_subscriptions/2,
	 get_node_subscriptions/2,
	 get_subscription/3,
	 set_subscription/4,
	 get_states/2,
	 get_state/3,
	 set_state/1,
	 get_items/7,
	 get_items/3,
	 get_item/8,
	 get_item/3,
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
%% ```check_create_user_permission(Host, Node, Owner, Access) ->
%%	   node_default:check_create_user_permission(Host, Node, Owner, Access).'''</p>
create_node_permission(Host, ServerHost, Node, _ParentNode, Owner, Access) ->
    LOwner = jlib:jid_tolower(Owner),
    {User, Server, _Resource} = LOwner,
    Allowed = case LOwner of
	{"", Host, ""} ->
	    true; % pubsub service always allowed
	_ ->
	    case acl:match_rule(ServerHost, Access, LOwner) of
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

%% @spec (Host, Node, Owner) ->
%%		  {result, Result} | exit
%%	 Host = mod_pubsub:host()
%%	 Node = mod_pubsub:pubsubNode()
%%	 Owner = mod_pubsub:jid()
%% @doc <p></p>
create_node(Host, Node, Owner) ->
    OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    mnesia:write(#pubsub_state{stateid = {OwnerKey, {Host, Node}},
			       affiliation = owner, subscription = none}),
    {result, {default, broadcast}}.


%% @spec (Host, Removed) -> ok
%%	 Host = mod_pubsub:host()
%%	 Removed = [mod_pubsub:pubsubNode()]
%% @doc <p>purge items of deleted nodes after effective deletion.</p>
delete_node(Host, Removed) ->
    lists:foreach(
      fun(Node) ->
	      lists:foreach(
		fun(#pubsub_state{stateid = StateId, items = Items}) ->
			lists:foreach(
			  fun(ItemId) ->
				  mnesia:delete(
				    {pubsub_item, {ItemId, {Host, Node}}})
			  end, Items),
			mnesia:delete({pubsub_state, StateId})
		end,
		mnesia:match_object(
		  #pubsub_state{stateid = {'_', {Host, Node}}, _ = '_'}))
      end, Removed),
    {result, {default, broadcast, Removed}}.

%% @spec (Host, Node, Sender, Subscriber, AccessModel, SendLast, PresenceSubscription, RosterGroup) ->
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
subscribe_node(Host, Node, Sender, Subscriber, AccessModel,
	       SendLast, PresenceSubscription, RosterGroup) ->
    SenderKey = jlib:jid_tolower(Sender),
    Authorized = (jlib:jid_remove_resource(SenderKey) == jlib:jid_remove_resource(Subscriber)),
						% TODO add some acl check for Authorized ?
    State = case get_state(Host, Node, Subscriber) of
		{error, ?ERR_ITEM_NOT_FOUND} ->
		    #pubsub_state{stateid = {Subscriber, {Host, Node}}}; % TODO: bug on Key ?
		{result, S} -> S
	    end,
    #pubsub_state{affiliation = Affiliation,
		  subscription = Subscription} = State,
    if
	not Authorized ->
	    %% JIDs do not match
	    {error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "invalid-jid")};
	Affiliation == outcast ->
	    %% Requesting entity is blocked
	    {error, ?ERR_FORBIDDEN};
	Subscription == pending ->
	    %% Requesting entity has pending subscription
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "pending-subscription")};
	(AccessModel == presence) and (not PresenceSubscription) ->
	    %% Entity is not authorized to create a subscription (presence subscription required)
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "presence-subscription-required")};
	(AccessModel == roster) and (not RosterGroup) ->
	    %% Entity is not authorized to create a subscription (not in roster group)
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "not-in-roster-group")};
	(AccessModel == whitelist) ->  % TODO: to be done
	    %% Node has whitelist access model
	    {error, ?ERR_EXTENDED(?ERR_NOT_ALLOWED, "closed-node")};
	(AccessModel == authorize) -> % TODO: to be done
	    %% Node has authorize access model
	    {error, ?ERR_FORBIDDEN};
	%%MustPay ->
	%%	% Payment is required for a subscription
	%%	{error, ?ERR_PAYMENT_REQUIRED};
	%%ForbiddenAnonymous ->
	%%	% Requesting entity is anonymous
	%%	{error, ?ERR_FORBIDDEN};
	true ->
	    NewSubscription =
		if
		    AccessModel == authorize ->
			pending;
		    %%TODO Affiliation == none -> ?
		    %%NeedConfiguration ->
		    %%	unconfigured
		    true ->
			subscribed
		end,
	    set_state(State#pubsub_state{subscription = NewSubscription}),
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

%% @spec (Host, Node, Sender, Subscriber, SubID) ->
%%			{error, Reason} | {result, []}
%%	 Host = mod_pubsub:host()
%%	 Node = mod_pubsub:pubsubNode()
%%	 Sender = mod_pubsub:jid()
%%	 Subscriber = mod_pubsub:jid()
%%	 SubID = string()
%%	 Reason = mod_pubsub:stanzaError()
%% @doc <p>Unsubscribe the <tt>Subscriber</tt> from the <tt>Node</tt>.</p>
unsubscribe_node(Host, Node, Sender, Subscriber, _SubId) ->
    SenderKey = jlib:jid_tolower(Sender),
    Match = jlib:jid_remove_resource(SenderKey) == jlib:jid_remove_resource(Subscriber),
    Authorized = case Match of
		     true ->
			 true;
		     false ->
			 case get_state(Host, Node, SenderKey) of % TODO: bug on Key ?
			     {result, #pubsub_state{affiliation=owner}} -> true;
			     _ -> false
			 end
		 end,
    case get_state(Host, Node, Subscriber) of
	{error, ?ERR_ITEM_NOT_FOUND} ->
	    %% Requesting entity is not a subscriber
	    {error, ?ERR_EXTENDED(?ERR_UNEXPECTED_REQUEST, "not-subscribed")};
	{result, State} ->
	    if
		%% Entity did not specify SubID
		%%SubID == "", ?? ->
		%%	{error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
		%% Invalid subscription identifier
		%%InvalidSubID ->
		%%	{error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
		%% Requesting entity is not a subscriber
		State#pubsub_state.subscription == none ->
		    {error, ?ERR_EXTENDED(?ERR_UNEXPECTED_REQUEST, "not-subscribed")};
		%% Requesting entity is prohibited from unsubscribing entity
		not Authorized ->
		    {error, ?ERR_FORBIDDEN};
		true ->
		    set_state(State#pubsub_state{subscription = none}),
		    {result, default}
	    end
    end.

%% @spec (Host, Node, Publisher, PublishModel, MaxItems, ItemId, Payload) ->
%%		 {true, PubsubItem} | {result, Reply}
%%	 Host = mod_pubsub:host()
%%	 Node = mod_pubsub:pubsubNode()
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
publish_item(Host, Node, Publisher, PublishModel, MaxItems, ItemId, Payload) ->
    PublisherKey = jlib:jid_tolower(jlib:jid_remove_resource(Publisher)),
    State = case get_state(Host, Node, PublisherKey) of
		{error, ?ERR_ITEM_NOT_FOUND} -> #pubsub_state{stateid={PublisherKey, {Host, Node}}};
		{result, S} -> S
	    end,
    #pubsub_state{affiliation = Affiliation,
		  subscription = Subscription} = State,
    if
	not ((PublishModel == open)
	     or ((PublishModel == publishers)
		 and ((Affiliation == owner) or (Affiliation == publisher)))
	     or ((PublishModel == subscribers)
		 and (Subscription == subscribed))) ->
	    %% Entity does not have sufficient privileges to publish to node
	    {error, ?ERR_FORBIDDEN};
	true ->
	    PubId = {PublisherKey, now()},
	    %% TODO: check creation, presence, roster (EJAB-663)
	    Item = case get_item(Host, Node, ItemId) of
		       {error, ?ERR_ITEM_NOT_FOUND} ->
			   #pubsub_item{itemid = {ItemId, {Host, Node}},
					creation = PubId,
					modification = PubId,
					payload = Payload};
		       {result, OldItem} ->
			   OldItem#pubsub_item{modification = PubId,
					       payload = Payload}
		   end,
	    Items = [ItemId | State#pubsub_state.items],
	    {result, {NI, OI}} = remove_extra_items(
				   Host, Node, MaxItems, Items),
	    set_item(Item),
	    set_state(State#pubsub_state{items = NI}),
	    {result, {default, broadcast, OI}}
    end.

%% @spec (Host, Node, MaxItems, ItemIds) -> {NewItemIds,OldItemIds}
%%	 Host = mod_pubsub:host()
%%	 Node = mod_pubsub:pubsubNode()
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
%% ```remove_extra_items(Host, Node, MaxItems, ItemIds) ->
%%	   node_default:remove_extra_items(Host, Node, MaxItems, ItemIds).'''</p>
remove_extra_items(_Host, _Node, unlimited, ItemIds) ->
    {result, {ItemIds, []}};
remove_extra_items(Host, Node, MaxItems, ItemIds) ->
    NewItems = lists:sublist(ItemIds, MaxItems),
    OldItems = lists:nthtail(length(NewItems), ItemIds),
    %% Remove extra items:
    lists:foreach(fun(ItemId) ->
			  mnesia:delete({pubsub_item, {ItemId, {Host, Node}}})
		  end, OldItems),
    %% Return the new items list:
    {result, {NewItems, OldItems}}.

%% @spec (Host, Node, JID, ItemId) ->
%%		  {error, Reason::stanzaError()} |
%%		  {result, []}
%%	 Host = mod_pubsub:host()
%%	 Node = mod_pubsub:pubsubNode()
%%	 JID = mod_pubsub:jid()
%%	 ItemId = string()
%% @doc <p>Triggers item deletion.</p>
%% <p>Default plugin: The user performing the deletion must be the node owner
%% or a publisher.</p>
delete_item(Host, Node, Publisher, ItemId) ->
    PublisherKey = jlib:jid_tolower(jlib:jid_remove_resource(Publisher)),
    State = case get_state(Host, Node, PublisherKey) of
		{error, ?ERR_ITEM_NOT_FOUND} ->
		    #pubsub_state{stateid = {PublisherKey, {Host, Node}}};
		{result, S} ->
		    S
	    end,
    #pubsub_state{affiliation = Affiliation, items = Items} = State,
    Allowed = (Affiliation == publisher) orelse (Affiliation == owner)
	orelse case get_item(Host, Node, ItemId) of
		   {result, #pubsub_item{creation = {PublisherKey, _}}} -> true;
		   _ -> false
	       end,
    if
	not Allowed ->
	    %% Requesting entity does not have sufficient privileges
	    {error, ?ERR_FORBIDDEN};
	true ->
	    case get_item(Host, Node, ItemId) of
		{result, _} ->
		    mnesia:delete({pubsub_item, {ItemId, {Host, Node}}}),
		    NewItems = lists:delete(ItemId, Items),
		    set_state(State#pubsub_state{items = NewItems}),
		    {result, {default, broadcast}};
		_ ->
		    %% Non-existent node or item
		    {error, ?ERR_ITEM_NOT_FOUND}
	    end
    end.

%% @spec (Host, Node, Owner) ->
%%		  {error, Reason::stanzaError()} |
%%		  {result, {default, broadcast}}
%%	 Host = mod_pubsub:host()
%%	 Node = mod_pubsub:pubsubNode()
%%	 Owner = mod_pubsub:jid()
purge_node(Host, Node, Owner) ->
    OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    case get_state(Host, Node, OwnerKey) of
	{result, #pubsub_state{items = Items, affiliation = owner}} ->
	    lists:foreach(fun(ItemId) ->
				  mnesia:delete({pubsub_item, {ItemId, {Host, Node}}})
			  end, Items),
	    {result, {default, broadcast}};
	{result, _} ->
	    %% Entity is not owner
	    {error, ?ERR_FORBIDDEN};
	_ ->
	    {error, ?ERR_ITEM_NOT_FOUND}
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
get_entity_affiliations(Host, Owner) ->
    OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    States = mnesia:match_object(
	       #pubsub_state{stateid = {OwnerKey, {Host, '_'}},
			     _ = '_'}),
    Tr = fun(#pubsub_state{stateid = {_, {_, N}}, affiliation = A}) ->
		 {N, A}
	 end,
    {result, lists:map(Tr, States)}.

get_node_affiliations(Host, Node) ->
    States = mnesia:match_object(
	       #pubsub_state{stateid = {'_', {Host, Node}},
			     _ = '_'}),
    Tr = fun(#pubsub_state{stateid = {J, {_, _}}, affiliation = A}) ->
		 {J, A}
	 end,
    {result, lists:map(Tr, States)}.

get_affiliation(Host, Node, Owner) ->
    OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    Affiliation = case get_state(Host, Node, OwnerKey) of
		      {result, #pubsub_state{affiliation = A}} -> A;
		      _ -> none
		  end,
    {result, Affiliation}.

set_affiliation(Host, Node, Owner, Affiliation) ->
    OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    Record = case get_state(Host, Node, OwnerKey) of
		 {error, ?ERR_ITEM_NOT_FOUND} ->
		     #pubsub_state{stateid = {OwnerKey, {Host, Node}},
				   affiliation = Affiliation};
		 {result, State} ->
		     State#pubsub_state{affiliation = Affiliation}
	     end,
    set_state(Record),
    ok.

%% @spec (Host, Owner) -> [{Node,Subscription}]
%%	 Host = host()
%%	 Owner = mod_pubsub:jid()
%%	 Node = mod_pubsub:pubsubNode()
%% @doc <p>Return the current subscriptions for the given user</p>
%% <p>The default module reads subscriptions in the main Mnesia
%% <tt>pubsub_state</tt> table. If a plugin stores its data in the same
%% table, it should return an empty list, as the affiliation will be read by
%% the default PubSub module. Otherwise, it should return its own affiliation,
%% that will be added to the affiliation stored in the main
%% <tt>pubsub_state</tt> table.</p>
get_entity_subscriptions(Host, Owner) ->
    OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    States = mnesia:match_object(
	       #pubsub_state{stateid = {OwnerKey, {Host, '_'}},
			     _ = '_'}),
    Tr = fun(#pubsub_state{stateid = {_, {_, N}}, subscription = S}) ->
		 {N, S}
	 end,
    {result, lists:map(Tr, States)}.

get_node_subscriptions(Host, Node) ->
    States = mnesia:match_object(
	       #pubsub_state{stateid = {'_', {Host, Node}},
			     _ = '_'}),
    Tr = fun(#pubsub_state{stateid = {J, {_, _}}, subscription = S}) ->
		 {J, S}
	 end,
    {result, lists:map(Tr, States)}.

get_subscription(Host, Node, Owner) ->
    OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    Subscription = case get_state(Host, Node, OwnerKey) of
		       {result, #pubsub_state{subscription = S}} -> S;
		       _ -> none
		   end,
    {result, Subscription}.

set_subscription(Host, Node, Owner, Subscription) ->
    OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    Record = case get_state(Host, Node, OwnerKey) of
		 {error, ?ERR_ITEM_NOT_FOUND} ->
		     #pubsub_state{stateid = {OwnerKey, {Host, Node}},
				   subscription = Subscription};
		 {result, State} ->
		     State#pubsub_state{subscription = Subscription}
	     end,
    set_state(Record),
    ok.

%% @spec (Host, Node) -> [States] | []
%%	 Host = mod_pubsub:host()
%%	 Node = mod_pubsub:pubsubNode()
%%	 Item = mod_pubsub:pubsubItems()
%% @doc Returns the list of stored states for a given node.
%% <p>For the default PubSub module, states are stored in Mnesia database.</p>
%% <p>We can consider that the pubsub_state table have been created by the main
%% mod_pubsub module.</p>
%% <p>PubSub plugins can store the states where they wants (for example in a
%% relational database).</p>
%% <p>If a PubSub plugin wants to delegate the states storage to the default node,
%% they can implement this function like this:
%% ```get_states(Host, Node) ->
%%	   node_default:get_states(Host, Node).'''</p>
get_states(Host, Node) ->
    States = mnesia:match_object(
	       #pubsub_state{stateid = {'_', {Host, Node}}, _ = '_'}),
    {result, States}.

%% @spec (JID, Host, Node) -> [State] | []
%%	 Host = mod_pubsub:host()
%%	 Node = mod_pubsub:pubsubNode()
%%	 JID = mod_pubsub:jid()
%%	 State = mod_pubsub:pubsubItems()
%% @doc <p>Returns a state (one state list), given its reference.</p>
get_state(Host, Node, JID) ->
    case mnesia:read({pubsub_state, {JID, {Host, Node}}}) of
	[State] when is_record(State, pubsub_state) ->
	    {result, State};
	_ ->
	    {error, ?ERR_ITEM_NOT_FOUND}
    end.

%% @spec (State) -> ok | {error, Reason::stanzaError()}
%%	 State = mod_pubsub:pubsubStates()
%% @doc <p>Write a state into database.</p>
set_state(State) when is_record(State, pubsub_state) ->
    mnesia:write(State);
set_state(_) ->
    {error, ?ERR_INTERNAL_SERVER_ERROR}.

%% @spec (Host, Node) -> [Items] | []
%%	 Host = mod_pubsub:host()
%%	 Node = mod_pubsub:pubsubNode()
%%	 Items = mod_pubsub:pubsubItems()
%% @doc Returns the list of stored items for a given node.
%% <p>For the default PubSub module, items are stored in Mnesia database.</p>
%% <p>We can consider that the pubsub_item table have been created by the main
%% mod_pubsub module.</p>
%% <p>PubSub plugins can store the items where they wants (for example in a
%% relational database), or they can even decide not to persist any items.</p>
%% <p>If a PubSub plugin wants to delegate the item storage to the default node,
%% they can implement this function like this:
%% ```get_items(Host, Node, From) ->
%%	   node_default:get_items(Host, Node, From).'''</p>
get_items(Host, Node, _From) ->
    Items = mnesia:match_object(
	      #pubsub_item{itemid = {'_', {Host, Node}}, _ = '_'}),
    {result, Items}.
get_items(Host, Node, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId) ->
    {Affiliation, Subscription} = 
	case get_state(Host, Node, jlib:jid_tolower(jlib:jid_remove_resource(JID))) of
	{result, #pubsub_state{affiliation = A, subscription = S}} -> {A, S}; 
	_ -> {none, none}
	end, 
    Subscribed = not ((Subscription == none) or (Subscription == pending)),
    if
	%%SubID == "", ?? ->
	    %% Entity has multiple subscriptions to the node but does not specify a subscription ID
	    %{error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
	%%InvalidSubID ->
	    %% Entity is subscribed but specifies an invalid subscription ID
	    %{error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
	Affiliation == outcast ->
	    %% Requesting entity is blocked
	    {error, ?ERR_FORBIDDEN};
	(AccessModel == open) and (not Subscribed) ->
	    %% Entity is not subscribed
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "not-subscribed")};
	(AccessModel == presence) and (not PresenceSubscription) ->
	    %% Entity is not authorized to create a subscription (presence subscription required)
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "presence-subscription-required")};
	(AccessModel == roster) and (not RosterGroup) ->
	    %% Entity is not authorized to create a subscription (not in roster group)
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "not-in-roster-group")};
	(AccessModel == whitelist) ->  % TODO: to be done
	    %% Node has whitelist access model
	    {error, ?ERR_EXTENDED(?ERR_NOT_ALLOWED, "closed-node")};
	(AccessModel == authorize) -> % TODO: to be done
	    %% Node has authorize access model
	    {error, ?ERR_FORBIDDEN};
	%%MustPay ->
	%%	% Payment is required for a subscription
	%%	{error, ?ERR_PAYMENT_REQUIRED};
	true ->
	    get_items(Host, Node, JID)
    end.

%% @spec (Host, Node, ItemId) -> [Item] | []
%%	 Host = mod_pubsub:host()
%%	 Node = mod_pubsub:pubsubNode()
%%	 ItemId = string()
%%	 Item = mod_pubsub:pubsubItems()
%% @doc <p>Returns an item (one item list), given its reference.</p>
get_item(Host, Node, ItemId) ->
    case mnesia:read({pubsub_item, {ItemId, {Host, Node}}}) of
	[Item] when is_record(Item, pubsub_item) ->
	    {result, Item};
	_ ->
	    {error, ?ERR_ITEM_NOT_FOUND}
    end.
get_item(Host, Node, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId) ->
    {Affiliation, Subscription} = 
	case get_state(Host, Node, jlib:jid_tolower(jlib:jid_remove_resource(JID))) of
	{result, #pubsub_state{affiliation = A, subscription = S}} -> {A, S}; 
	_ -> {none, none}
	end, 
    Subscribed = not ((Subscription == none) or (Subscription == pending)),
    if
	%%SubID == "", ?? ->
	    %% Entity has multiple subscriptions to the node but does not specify a subscription ID
	    %{error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
	%%InvalidSubID ->
	    %% Entity is subscribed but specifies an invalid subscription ID
	    %{error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
	Affiliation == outcast ->
	    %% Requesting entity is blocked
	    {error, ?ERR_FORBIDDEN};
	(AccessModel == open) and (not Subscribed) ->
	    %% Entity is not subscribed
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "not-subscribed")};
	(AccessModel == presence) and (not PresenceSubscription) ->
	    %% Entity is not authorized to create a subscription (presence subscription required)
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "presence-subscription-required")};
	(AccessModel == roster) and (not RosterGroup) ->
	    %% Entity is not authorized to create a subscription (not in roster group)
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "not-in-roster-group")};
	(AccessModel == whitelist) ->  % TODO: to be done
	    %% Node has whitelist access model
	    {error, ?ERR_EXTENDED(?ERR_NOT_ALLOWED, "closed-node")};
	(AccessModel == authorize) -> % TODO: to be done
	    %% Node has authorize access model
	    {error, ?ERR_FORBIDDEN};
	%%MustPay ->
	%%	% Payment is required for a subscription
	%%	{error, ?ERR_PAYMENT_REQUIRED};
	true ->
	    get_item(Host, Node, ItemId)
    end.

%% @spec (Item) -> ok | {error, Reason::stanzaError()}
%%	 Item = mod_pubsub:pubsubItems()
%% @doc <p>Write a state into database.</p>
set_item(Item) when is_record(Item, pubsub_item) ->
    mnesia:write(Item);
set_item(_) ->
    {error, ?ERR_INTERNAL_SERVER_ERROR}.

%% @doc <p>Return the name of the node if known: Default is to return
%% node id.</p>
get_item_name(_Host, _Node, Id) ->
    Id.
