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
%%% Portions created by ProcessOne are Copyright 2006-2012, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2012, ProcessOne.
%%%
%%%
%%% @copyright 2006-2012 ProcessOne
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

-module(node_flat).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").

-behaviour(gen_pubsub_node).

%% API definition
-export([
	 init/3,
	 terminate/2,
	 options/0,
	 features/0,
	 create_node_permission/6,
	 create_node/2,
	 delete_node/1,
	 purge_node/2,
	 subscribe_node/8,
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
	 get_subscriptions/2,
	 set_subscriptions/4,
	 get_pending_nodes/2,
	 get_states/1,
	 get_state/2,
	 set_state/1,
	 get_items/6,
	 get_items/2,
	 get_item/7,
	 get_item/2,
	 set_item/1,
	 get_item_name/3,
	 node_to_path/1,
	 path_to_node/1
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
-spec(init/3 ::
      (
	     Host       :: string(),
	     ServerHost :: string(),
	     Opts       :: [{Key::atom(), Value::term()}])
      -> 'ok'
	    ).

init(_Host, _ServerHost, _Opts) ->
    pubsub_subscription:init(),
    mnesia:create_table(pubsub_state,
			[{disc_copies, [node()]},
			 {index, [nodeidx]},
			 {attributes, record_info(fields, pubsub_state)}]),
    mnesia:create_table(pubsub_item,
			[{disc_only_copies, [node()]},
			 {index, [nodeidx]},
			 {attributes, record_info(fields, pubsub_item)}]),
    ItemsFields = record_info(fields, pubsub_item),
    case mnesia:table_info(pubsub_item, attributes) of
	ItemsFields -> ok;
	_           -> mnesia:transform_table(pubsub_item, ignore, ItemsFields)
    end,
    ok.

%% @spec (Host, ServerHost) -> any()
%%	 Host = mod_pubsub:host()
%%	 ServerHost = host()
%% @doc <p>Called during pubsub modules termination. Any pubsub plugin must
%% implement this function. It can return anything.</p>
-spec(terminate/2 ::
      (
		  Host       :: string(),
		  ServerHost :: string())
      -> 'ok'
	    ).

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
-spec(options/0 :: () -> [nodeOption()]).

options() ->
    [{'deliver_payloads', true},
     {'notify_config', false},
     {'notify_delete', false},
     {'notify_retract', true},
     {'notify_sub', false},
     {'purge_offline', false},
     {'persist_items', true},
     {'max_items', ?MAXITEMS},
     {'subscribe', true},
     {'access_model', open},
     {'roster_groups_allowed', []},
     {'publish_model', publishers},
     {'notification_type', headline},
     {'max_payload_size', ?MAX_PAYLOAD_SIZE},
     {'send_last_published_item', on_sub_and_presence},
     {'deliver_notifications', true},
     {'presence_based_delivery', false}].

%% @spec () -> []
%% @doc Returns the node features
-spec(features/0 :: () -> [Feature::string()]).

features() ->
    ["create-nodes",
     "auto-create",
     "access-authorize",
     "delete-nodes",
     "delete-items",
     "get-pending",
     "instant-nodes",
     "manage-subscriptions",
     "modify-affiliations",
     "multi-subscribe",
     "outcast-affiliation",
     "persistent-items",
     "publish",
     "purge-nodes",
     "retract-items",
     "retrieve-affiliations",
     "retrieve-items",
     "retrieve-subscriptions",
     "subscribe",
     "subscription-notifications",
     "subscription-options"
    ].

%% use same code as node_flat, but do not limite node to
%% the home/localhost/user/... hierarchy
%% any node is allowed
-spec(create_node_permission/6 ::
      (
			       Host         :: hostPubsub(),
			       ServerHost   :: string(),
			       NodeId       :: nodeId(),
			       ParentNodeId :: nodeId(),
			       JID          :: jidEntity(),
			       Access       :: atom())
      -> {'result', IsAllowed::boolean()}
	    ).

create_node_permission(Host, ServerHost, _NodeId, _ParentNodeId, #jid{node = U, domain = S, resource = R} = JID, Access) ->
    Owner = {U,S,R},
    IsAllowed = case Owner of
		    {undefined, Host, undefined} -> true; % pubsub service always allowed
		    _ -> acl:match_rule(ServerHost, Access, JID) =:= 'allow'
		end,
    {result, IsAllowed}.

%% @spec (NodeId, Owner) ->
%%		  {result, Result} | exit
%%	 NodeId = nodeidx()
%%	 Owner = ljid()
%% @doc <p></p>
-spec(create_node/2 ::
      (
		    NodeIdx :: nodeIdx(),
		    JID     :: jidEntity())
      -> {'result', {'default', 'broadcast'}}
	    ).

create_node(NodeIdx, #jid{node = U, domain = S} = _JID) ->
    set_state(#pubsub_state{id = {{U,S,undefined}, NodeIdx}, nodeidx = NodeIdx, affiliation = 'owner'}),
    {'result', {'default', 'broadcast'}}.

%% @spec (Removed) -> ok
%%	 Removed = [mod_pubsub:pubsub_node()]
%% @doc <p>purge items of deleted nodes after effective deletion.</p>
-spec(delete_node/1 ::
      (
		    Nodes :: [Node::pubsubNode()])
      -> {result, {'default', 'broadcast',
		   Reply :: [{Node :: pubsubNode(),
			      [{Owner         :: bareUsr(),
				Subscriptions :: [{Subscription :: subscription(),
						   SubId        :: subId()}]}]}]}}
	    ).

delete_node(Nodes) ->
    Fun = fun(#pubsub_state{id = {Entity, _}, subscriptions = Subscriptions}) ->
		  [{Entity, Subscription} || Subscription <- Subscriptions]
	  end,
    Reply = lists:map(fun(#pubsub_node{idx = NodeIdx} = Node) ->
			      {result, States} = get_states(NodeIdx),
			      lists:foreach(fun(#pubsub_state{id = {Owner, _}, items = ItemIds}) ->
						    del_items(NodeIdx, ItemIds),
						    del_state(NodeIdx, Owner),
						    {Node, lists:flatmap(Fun, States)}
					    end, States)
		      end, Nodes),
    {'result', {'default', 'broadcast', Reply}}.

%% @spec (NodeId, Sender, Subscriber, AccessModel, SendLast, PresenceSubscription, RosterGroup, Options) ->
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
-spec(subscribe_node/8 ::
      (
		       NodeIdx              :: nodeIdx(),
		       JID                  :: jidEntity(),
		       Subscriber           :: jidEntity(),
		       AccessModel          :: accessModel(),
		       SendLast             :: atom(),
		       PresenceSubscription :: boolean(),
		       RosterGroup          :: boolean(),
		       Options              :: [nodeOption()])
      -> {'result', {'default',
		     Subscription :: 'subscribed',
		     SubId        :: subId()}}
	     | {'result', {'default',
			   Subscription :: 'subscribed',
			   SubId        :: subId(),
			   SendLast     ::' send_last'}}
	     | {'result', {'default',
			   Subscription :: 'pending',
			   SubId        :: subId()}}
	     | {'error', _} %% TODO add all error cases
	    ).

subscribe_node(NodeIdx, Sender, {U, S, R}, AccessModel, SendLast, PresenceSubscription, RosterGroup, Options) ->
    subscribe_node(NodeIdx, Sender, exmpp_jid:make({U, S, R}), AccessModel, SendLast, PresenceSubscription, RosterGroup, Options);
subscribe_node(NodeIdx, #jid{node = Usender, domain = Ssender} = _Sender, #jid{node = U, domain = S, resource = R} = Subscriber, AccessModel, SendLast, PresenceSubscription, RosterGroup, Options) ->
    SubKey = {U, S, R},
    GenKey = {U, S, undefined},
    Authorized = ({Usender, Ssender, undefined} == GenKey),
    GenState = #pubsub_state{affiliation = Affiliation} = get_state(NodeIdx, GenKey),
    SubState = #pubsub_state{subscriptions = Subscriptions} = case SubKey of
								  GenKey -> GenState;
								  _      -> get_state(NodeIdx, SubKey)
							      end,
    Whitelisted = lists:member(Affiliation, ['member', 'publisher', 'owner']),
    PendingSubscription = lists:any(fun
				    ({'pending', _}) -> true;
				    (_)              -> false
				   end, Subscriptions),
    if
	not Authorized ->
	    %% JIDs do not match
	    {'error', ?ERR_EXTENDED('bad-request', "invalid-jid")};
	Affiliation == 'outcast' ->
	    %% Requesting entity is blocked
	    {'error', 'forbidden'};
	PendingSubscription ->
	    %% Requesting entity has pending subscription
	    {'error', ?ERR_EXTENDED('not-authorized', "pending-subscription")};
	(AccessModel == 'presence') and (not PresenceSubscription) ->
	    %% Entity is not authorized to create a subscription (presence subscription required)
	    {'error', ?ERR_EXTENDED('not-authorized', "presence-subscription-required")};
	(AccessModel == 'roster') and (not RosterGroup) ->
	    %% Entity is not authorized to create a subscription (not in roster group)
	    {'error', ?ERR_EXTENDED('not-authorized', "not-in-roster-group")};
	(AccessModel == 'whitelist') and (not Whitelisted) ->
	    %% Node has whitelist access model and entity lacks required affiliation
	    {'error', ?ERR_EXTENDED('not-allowed', "closed-node")};
	%%MustPay ->
	%% Payment is required for a subscription
	%% {'error', ?ERR_PAYMENT_REQUIRED};
	%%ForbiddenAnonymous ->
	%% Requesting entity is anonymous
	%% {'error', 'forbidden'};
	true ->
	    case pubsub_subscription:subscribe_node(Subscriber, NodeIdx, Options) of
		{'result', SubId} ->
		    NewSubscription = case AccessModel of
					  'authorize' -> 'pending';
					  _           -> 'subscribed'
				      end,
		    set_state(SubState#pubsub_state{subscriptions = [{NewSubscription, SubId} | Subscriptions]}),
		    case {NewSubscription, SendLast} of
			{'subscribed', 'never'} -> {'result', {'default', 'subscribed', SubId}};
			{'subscribed', _}       -> {'result', {'default', 'subscribed', SubId, 'send_last'}};
			{_, _}                  -> {'result', {'default', 'pending', SubId}}
		    end;
		_ ->
		    {'error', 'internal-server-error'}
	    end
    end.

%% @spec (NodeId, Sender, Subscriber, SubId) ->
%%			{error, Reason} | {result, []}
%%	 NodeId = nodeidx()
%%	 Sender = ljid()
%%	 Subscriber = ljid()
%%	 SubId = mod_pubsub:subid()
%%	 Reason = mod_pubsub:stanzaError()
%% @doc <p>Unsubscribe the <tt>Subscriber</tt> from the <tt>Node</tt>.</p>
-spec(unsubscribe_node/4 ::
      (
			 NodeIdx    :: nodeIdx(),
			 JID        :: jidEntity(),
			 Subscriber :: jidEntity(),
			 SubId      :: subId())
      -> {'result', 'default'} | {'error', _} %% TODO : add all error cases
	    ).

unsubscribe_node(NodeIdx, Sender, {U, S, R}, SubId) ->
    unsubscribe_node(NodeIdx, Sender, exmpp_jid:make({U, S, R}), SubId);
unsubscribe_node(NodeIdx, #jid{node = Usender, domain = Ssender} = _Sender, #jid{node = U, domain = S, resource = R} = _Subscriber, SubId) ->
    SubKey = {U, S, R},
    GenKey = {U, S, undefined},
    Authorized = ({Usender, Ssender, undefined} == GenKey),
    GenState = get_state(NodeIdx, GenKey),
    SubState = case SubKey of
		   GenKey -> GenState;
		   _      -> get_state(NodeIdx, SubKey)
	       end,
    Subscriptions = SubState#pubsub_state.subscriptions,
    SubIdExists = case SubId of
		      <<>>                        -> false;
		      SubId when is_binary(SubId) -> true;
		      _                           -> false
		  end,
    if
	%% Requesting entity is prohibited from unsubscribing entity
	not Authorized ->
	    {'error', 'forbidden'};
	%% Entity did not specify SubId
	%%SubId == "", ?? ->
	%%	{'error', ?ERR_EXTENDED('bad-request', "subid-required")};
	%% Invalid subscription identifier
	%%InvalidSubId ->
	%%	{'error', ?ERR_EXTENDED('not-acceptable', "invalid-subid")};
	%% Requesting entity is not a subscriber
	Subscriptions == [] ->
	    {'error', ?ERR_EXTENDED('unexpected-request', "not-subscribed")};
	%% Subid supplied, so use that.
	SubIdExists ->
	    OldSubscription = first_in_list(fun
					    ({_, OldSubId}) when OldSubId == SubId -> true;
					    (_) -> false
					   end, Subscriptions),
	    case OldSubscription of
		{'value', Subscribed} ->
		    delete_subscriptions(SubKey, NodeIdx, [Subscribed], SubState),
		    {'result', 'default'};
		false ->
		    {'error', ?ERR_EXTENDED('unexpected-request', "not-subscribed")}
	    end;
	%% Asking to remove all subscriptions to the given node
	SubId == 'all' ->
	    delete_subscriptions(SubKey, NodeIdx, Subscriptions, SubState),
	    {'result', 'default'};
	%% No subid supplied, but there's only one matching
	%% subscription, so use that.
	length(Subscriptions) == 1 ->
	    delete_subscriptions(SubKey, NodeIdx, Subscriptions, SubState),
	    {'result', 'default'};
	true ->
	    {'error', ?ERR_EXTENDED('bad-request', "subid-required")}
    end.


-spec(delete_subscriptions/4 ::
      (
			     Entity        :: fullUsr(),
			     NodeIdx       :: nodeIdx(),
			     Subscriptions :: [{Subscription::subscription(), SubId::subId()}],
			     State         :: pubsubState())
      -> 'ok' | {error, 'internal-server-error'}
	    ).

delete_subscriptions(Entity, NodeIdx, Subscriptions, State) ->
    NewSubscriptions = lists:foldl(fun({Subscription, SubId}, Acc) ->
					   pubsub_subscription:delete_subscription(Entity, NodeIdx, SubId),
					   Acc -- [{Subscription, SubId}]
				   end, State#pubsub_state.subscriptions, Subscriptions),
    case {State#pubsub_state.affiliation, NewSubscriptions} of
	{'none', []} ->
						% Just a regular subscriber, and this is final item, so
						% delete the state.
	    del_state(NodeIdx, Entity);
	_ ->
	    set_state(State#pubsub_state{subscriptions = NewSubscriptions})
    end.


%% @spec (NodeId, Publisher, PublishModel, MaxItems, ItemId, Payload) ->
%%		 {true, PubsubItem} | {result, Reply}
%%	 NodeId = nodeidx()
%%	 Publisher = ljid()
%%	 PublishModel = atom()
%%	 MaxItems = integer()
%%	 ItemId = item()
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
-spec(publish_item/6 ::
      (
		     NodeIdx      :: nodeIdx(),
		     JID          :: jidEntity(),
		     PublishModel :: atom(), %% TODO : make a generic publishMod() type
		     MaxItems     :: 'unlimited' | integer(),
		     ItemId       :: itemId(),
		     Payload      :: payload())
      -> {'result', {'default', 'broadcast', ItemIds :: [] | [itemId()]}}
	     | {'error', _}
	    ).

publish_item(NodeIdx, #jid{node = U, domain = S, resource = R} = _JID, PublishModel, MaxItems, ItemId, Payload) ->
    SubKey = {U,S,R},
    GenKey = {U,S,undefined},
    GenState = get_state(NodeIdx, GenKey),
    SubState = case SubKey of
		   GenKey -> GenState;
		   _      -> get_state(NodeIdx, SubKey)
	       end,
    Affiliation = GenState#pubsub_state.affiliation,
    Subscribed = case PublishModel of
		     'subscribers' -> is_subscribed(SubState#pubsub_state.subscriptions);
		     _             -> undefined
		 end,
    if
	not ((PublishModel == 'open') or ((PublishModel == 'publishers')
					  and ((Affiliation == 'owner') or (Affiliation == 'publisher')))
	     or (Subscribed == 'true')) ->
	    %% Entity does not have sufficient privileges to publish to node
	    {'error', 'forbidden'};
	true ->
	    %% TODO: check creation, presence, roster
	    if MaxItems > 0 ->
		    Now = now(),
		    Modification = {Now, SubKey},
		    Item = case get_item(NodeIdx, ItemId) of
			       {'result', OldItem} ->
				   OldItem#pubsub_item{modification = Modification, payload = Payload};
			       _ ->
				   #pubsub_item{
				 id = {ItemId, NodeIdx},
				 nodeidx = NodeIdx,
				 creation = {Now, GenKey},  % TODO, better use {Now, SubKey} ?
				 modification = Modification,
				 payload = Payload}
			   end,
		    Items = [ItemId | GenState#pubsub_state.items--[ItemId]],
		    {result, {NewItems, OldItems}} = remove_extra_items(NodeIdx, MaxItems, Items),
		    set_item(Item),
		    set_state(GenState#pubsub_state{items = NewItems}),
		    {'result', {'default', 'broadcast', OldItems}};
	       true ->
		    {'result', {'default', 'broadcast', []}}
	    end
    end.

%% @spec (NodeId, MaxItems, ItemIds) -> {NewItemIds,OldItemIds}
%%	 NodeId = nodeidx()
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
-spec(remove_extra_items/3 ::
      (
			   NodeIdx  :: nodeIdx(),
			   MaxItems :: 'unlimited' | integer(),
			   ItemsIds :: [ItemId::itemId()])
      -> {'result',
	  {OldItems :: [] | [ItemId::itemId()],
	   NewItems :: [] | [ItemId::itemId()]}}
	    ).

remove_extra_items(_NodeIdx, 'unlimited', ItemIds) ->
    {'result', {ItemIds, []}};
remove_extra_items(NodeIdx, MaxItems, ItemIds) ->
    NewItems = lists:sublist(ItemIds, MaxItems),
    OldItems = lists:nthtail(length(NewItems), ItemIds),
    %% Remove extra items:
    del_items(NodeIdx, OldItems),
    %% Return the new items list:
    {'result', {NewItems, OldItems}}.

%% @spec (NodeId, Publisher, PublishModel, ItemId) ->
%%		  {error, Reason::stanzaError()} |
%%		  {result, []}
%%	 NodeId = nodeidx()
%%	 Publisher = ljid()
%%	 PublishModel = atom()
%%	 ItemId = item()
%% @doc <p>Triggers item deletion.</p>
%% <p>Default plugin: The user performing the deletion must be the node owner
%% or a publisher, or PublishModel being open.</p>
-spec(delete_item/4 ::
      (
		    NodeIdx      :: nodeIdx(),
		    JID          :: jidEntity(),
		    PublishModel :: atom(),
		    ItemId       :: itemId())
      -> {'result', {'default', 'broadcast'}} | {'error', _}
	    ).

delete_item(NodeIdx, #jid{node = U, domain = S} = _JID, PublishModel, ItemId) ->
    GenKey = {U,S,undefined},
    GenState = #pubsub_state{affiliation = Affiliation, items = Items} = get_state(NodeIdx, GenKey),
    Allowed = (Affiliation == 'publisher')
	orelse (Affiliation == 'owner')
	orelse (PublishModel == 'open')
	orelse case get_item(NodeIdx, ItemId) of
		   {result, #pubsub_item{creation = {_, GenKey}}} -> true;
		   _                                              -> false
	       end,
    if
	not Allowed ->
	    %% Requesting entity does not have sufficient privileges
	    {'error', 'forbidden'};
	true ->
	    case lists:member(ItemId, Items) of
		true ->
		    del_item(NodeIdx, ItemId),
		    set_state(GenState#pubsub_state{items = lists:delete(ItemId, Items)}),
		    {'result', {'default', 'broadcast'}};
		false ->
		    case Affiliation of
			'owner' ->
			    %% Owner can delete any items from its own node
			    {'result', States} = get_states(NodeIdx),
			    lists:foldl(fun
					(#pubsub_state{items = ItemIds} = State, Result) ->
					       case lists:member(ItemId, ItemIds) of
						   true ->
						       del_item(NodeIdx, ItemId),
						       set_state(State#pubsub_state{items = lists:delete(ItemId, ItemIds)}),
						       {'result', {'default', 'broadcast'}};
						   false -> Result
					       end;
					(_, Result) -> Result
				       end, {'error', 'item-not-found'}, States);
			_ ->
			    %% Non-existent node or item
			    {'error', 'item-not-found'}
		    end
	    end
    end.

%% @spec (NodeId, Owner) ->
%%		  {error, Reason::stanzaError()} |
%%		  {result, {default, broadcast}}
%%	 NodeId = nodeidx()
%%	 Owner = ljid()
-spec(purge_node/2 ::
      (
		   NodeIdx :: nodeIdx(),
		   JID     :: jidEntity())
      -> {'result', {'default', 'broadcast'}} | {'error', 'forbidden'}
	    ).

purge_node(NodeIdx, #jid{node = U, domain = S} = _JID) ->
						%GenKey = {U,S,undefined}
						%GenState = get_state(NodeIdx, _GenKey = {U,S,undefined}),
    case _GenState = get_state(NodeIdx, _GenKey = {U,S,undefined}) of % experimental
	#pubsub_state{affiliation = 'owner'} ->
	    {'result', States} = get_states(NodeIdx),
	    lists:foreach(fun
			  (#pubsub_state{items = []}) ->
				 ok;
			  (#pubsub_state{items = Items} = State) ->
				 del_items(NodeIdx, Items),
				 set_state(State#pubsub_state{items = []})
			 end, States),
	    {'result', {'default', 'broadcast'}};
	_ -> %% Entity is not owner
	    {'error', 'forbidden'}
    end.

%% @spec (Host, JID) -> [{Node,Affiliation}]
%%	 Host = host()
%%	 JID = ljid()
%% @doc <p>Return the current affiliations for the given user</p>
%% <p>The default module reads affiliations in the main Mnesia
%% <tt>pubsub_state</tt> table. If a plugin stores its data in the same
%% table, it should return an empty list, as the affiliation will be read by
%% the default PubSub module. Otherwise, it should return its own affiliation,
%% that will be added to the affiliation stored in the main
%% <tt>pubsub_state</tt> table.</p>
-spec(get_entity_affiliations/2 ::
      (
				Host :: binary(),
				JID  :: jidEntity())
      -> {'result', Reply :: [] | [{Node::pubsubNode(), Affiliation::affiliation()}]}
	    ).

get_entity_affiliations(Host, #jid{node = U, domain = S} = _JID) ->
    States = mnesia:match_object(#pubsub_state{id = {{U,S,undefined}, '_'}, _ = '_'}),
    NodeTree = case catch ets:lookup(gen_mod:get_module_proc(Host, 'config'), 'nodetree') of
		   [{'nodetree', Tree}] -> Tree;
		   _                    -> 'nodetree_tree'
	       end,
    Reply = lists:foldl(fun
			(#pubsub_state{id = {_, NodeIdx}, affiliation = Affiliation}, Acc) ->
			       case NodeTree:get_node(NodeIdx) of
				   #pubsub_node{id = {Host, _}} = Node -> [{Node, Affiliation}|Acc];
				   _ -> Acc
			       end
		       end, [], States),
    {result, Reply}.


-spec(get_node_affiliations/1 ::
      (
			      NodeIdx :: nodeIdx())
      -> {'result', [] | [{Entity::fullUsr(), Affiliation::affiliation()}]}
	    ).

get_node_affiliations(NodeIdx) ->
    {result, States} = get_states(NodeIdx),
    Fun = fun(#pubsub_state{id = {Entity, _}, affiliation = Affiliation}) ->
		  {Entity, Affiliation}
	  end,
    {result, lists:map(Fun, States)}.


-spec(get_affiliation/2 ::
      (
			NodeIdx :: nodeIdx(),
			JID     :: jidEntity())
      -> {'result', Affiliation::affiliation()}
	    ).

get_affiliation(NodeIdx, #jid{node = U, domain = S} = _JID) ->
    #pubsub_state{affiliation = Affiliation} = get_state(NodeIdx, _GenKey = {U,S,undefined}),
    {'result', Affiliation}.


-spec(set_affiliation/3 ::
      (
			NodeIdx     :: nodeIdx(),
			JID         :: jidEntity(),
			Affiliation :: affiliation())
      -> 'ok' | {error, 'internal-server-error'}
	    ).

set_affiliation(NodeIdx, #jid{node = U, domain = S} = _JID, Affiliation) ->
    GenKey = {U,S,undefined},
    GenState = #pubsub_state{subscriptions = Subscriptions} = get_state(NodeIdx, GenKey),
    case {Affiliation, Subscriptions} of
	{'none', []} -> del_state(NodeIdx, GenKey);
	_ -> set_state(GenState#pubsub_state{affiliation = Affiliation})
    end.

%% @spec (Host, Owner) -> [{Node,Subscription}]
%%	 Host = host()
%%	 Owner = ljid()
%% @doc <p>Return the current subscriptions for the given user</p>
%% <p>The default module reads subscriptions in the main Mnesia
%% <tt>pubsub_state</tt> table. If a plugin stores its data in the same
%% table, it should return an empty list, as the affiliation will be read by
%% the default PubSub module. Otherwise, it should return its own affiliation,
%% that will be added to the affiliation stored in the main
%% <tt>pubsub_state</tt> table.</p>
-spec(get_entity_subscriptions/2 ::
      (
				 Host :: hostPubsub(),
				 JID  :: jidEntity())
      -> {'result', []
	  | [{Node         :: pubsubNode(),
	      Subscription :: subscription(),
	      SubId        :: subId(),
	      Entity       :: fullUsr()}]}
	    ).

get_entity_subscriptions(Host, #jid{node = U, domain = S, resource = R} = _JID) ->
    SubKey = {U,S,R},
    GenKey = {U,S,undefined},
    States = case SubKey of
		 GenKey ->
		     mnesia:match_object(#pubsub_state{id = {{U, S, '_'}, '_'}, _ = '_'});
		 _ ->
		     mnesia:match_object(#pubsub_state{id = {GenKey, '_'}, _ = '_'})
			 ++
			 mnesia:match_object(#pubsub_state{id = {SubKey, '_'}, _ = '_'})
	     end,
    NodeTree = case catch ets:lookup(gen_mod:get_module_proc(Host, 'config'), 'nodetree') of
		   [{'nodetree', Tree}] -> Tree;
		   _                    -> 'nodetree_tree'
	       end,
    Reply = lists:foldl(fun
			(#pubsub_state{id = {Entity, NodeIdx}, subscriptions = Subscriptions}, Acc) ->
			       case NodeTree:get_node(NodeIdx) of
				   #pubsub_node{id = {Host, _}} = Node ->
				       lists:foldl(fun
						   ({Subscription, SubId}, Acc2) -> [{Node, Subscription, SubId, Entity} | Acc2]
						%(Subscriptions, Acc2)         -> [{Node, Subscription, Entity} | Acc2]
						  end, Acc, Subscriptions);
				   _ -> Acc
			       end
		       end, [], States),
    {result, Reply}.


-spec(get_node_subscriptions/1 ::
      (
			       NodeIdx :: nodeIdx())
      -> {'result', []
	  %| [{Entity::fullUsr(), 'none'}]
						%| [{Entity::fullUsr(), Subscription::subscription()}] %% still useful case ?
	  | [{Entity::fullUsr(), Subscription::subscription(), SubId::subId()}]}
	    ).

get_node_subscriptions(NodeIdx) ->
    {result, States} = get_states(NodeIdx),
    Fun = fun(#pubsub_state{id = {Entity, _}, subscriptions = Subscriptions}) ->
	    [{Entity, Subscription, SubId} || {Subscription, SubId} <- Subscriptions]
	  end,
    {result, lists:flatmap(Fun, States)}.


-spec(get_subscriptions/2 ::
      (
			  NodeIdx :: nodeIdx(),
			  JID     :: jidEntity())
      -> {'result', Subscriptions :: [] | [{Subscription::subscription(), SubId::subId()}]}
	    ).

get_subscriptions(NodeIdx, #jid{node = U, domain = S, resource = R} = _JID) ->
    #pubsub_state{subscriptions = Subscriptions} = get_state(NodeIdx, _SubKey = {U,S,R}),
    {result, Subscriptions}.


-spec(set_subscriptions/4 ::
      (
			  NodeIdx      :: nodeIdx(),
			  JID          :: jidEntity(),
			  Subscription :: subscription(),
			  SubId        :: subId())
      -> 'ok'
	     | {Subscription::subscription(), SubId::subId()}
	     | {error, _}
	    ).

set_subscriptions(NodeIdx,  #jid{node = U, domain = S, resource = R} = JID, Subscription, SubId) ->
    SubKey = {U,S,R},
    SubState = #pubsub_state{subscriptions = Subscriptions} = get_state(NodeIdx, SubKey),
    case {SubId, Subscriptions} of
	{_, []} ->
	    case Subscription of
		'none' -> {'error', ?ERR_EXTENDED('bad_request', "not-subscribed")};
		_      -> new_subscription(NodeIdx, JID, Subscription, SubState)
	    end;
	{<<>>, [{_, OtherSubId}]} ->
	    case Subscription of
		'none' -> unsub_with_subid(NodeIdx, OtherSubId, SubState);
		_      -> replace_subscription({Subscription, OtherSubId}, SubState)
	    end;
	{<<>>, [_|_]} ->
	    {error, ?ERR_EXTENDED('bad_request', "subid-required")};
	_ ->
	    case Subscription of
		'none' -> unsub_with_subid(NodeIdx, SubId, SubState);
		_      -> replace_subscription({Subscription, SubId}, SubState)
	    end
    end.


-spec(replace_subscription/2 ::
      (
			     NewSubscription :: {Subscription::subscription(), SubId::subId()},
			     SubState        :: pubsubState())
      -> 'ok' | {error, 'internal-server-error'}
	    ).

replace_subscription(NewSubcription, #pubsub_state{subscriptions = Subscriptions} = SubState) ->
    NewSubcriptions = replace_subscription(NewSubcription, Subscriptions, []),
    set_state(SubState#pubsub_state{subscriptions = NewSubcriptions}).


-spec(replace_subscription/3 ::
      (
			     NewSubscription :: {Subscription::subscription(), SubId::subId()},
			     Subscriptions   :: [{Subscription::subscription(), SubId::subId()}],
			     Acc             :: [] | [{Subscription::subscription(), SubId::subId()}])
      -> NewSubscriptions :: [] | [{Subscription::subscription(), SubId::subId()}]
	    ).
%% TODO : add clause case in -spec
replace_subscription(_, [], Acc) ->
    Acc;
replace_subscription({Subcription, SubId}, [{_, SubId} | OtherSubcriptions], Acc) ->
    replace_subscription({Subcription, SubId}, OtherSubcriptions, [{Subcription, SubId} | Acc]).


-spec(new_subscription/4 ::
      (
			 NodeIdx       :: nodeIdx(),
			 JID           :: jidEntity(),
			 Subscriptions :: subscription(),
			 SubState      :: pubsubState())
      -> {Subscription::subscription(), SubId::subId()}
	    ).

new_subscription(NodeIdx, #jid{node = U, domain = S, resource = R} = _JID, Subscription, #pubsub_state{subscriptions = Subscriptions} = SubState) ->
    SubId = pubsub_subscription:add_subscription({U,S,R}, NodeIdx, []),
    set_state(SubState#pubsub_state{subscriptions = [{Subscription, SubId} | Subscriptions]}),
    {Subscription, SubId}.


-spec(unsub_with_subid/3 ::
      (
			 NodeIdx  :: nodeIdx(),
			 SubId    :: subId(),
			 SubState :: pubsubState())
      -> 'ok' | {'error', 'internal-server-error'}
	    ).

unsub_with_subid(NodeIdx, SubId, #pubsub_state{id = {Entity, _}} = SubState) ->
    pubsub_subscription:delete_subscription(Entity, NodeIdx, SubId),
    NewSubcriptions = lists:filter(fun
				   ({_, SID}) -> SubId =/= SID
				  end, SubState#pubsub_state.subscriptions),
    case {NewSubcriptions, SubState#pubsub_state.affiliation} of
	{[], 'none'} -> del_state(NodeIdx, Entity);
	_ -> set_state(SubState#pubsub_state{subscriptions = NewSubcriptions})
    end.

%% @spec (Host, Owner) -> {result, [Node]} | {error, Reason}
%%       Host = host()
%%       Owner = jid()
%%       Node = node()
%% @doc <p>Returns a list of Owner's nodes on Host with pending
%% subscriptions.</p>
-spec(get_pending_nodes/2 ::
      (
			  Host :: hostPubsub(),
			  JID  :: jidEntity())
      -> 'false' | {'value', NodeId::nodeId()}
	    ).

get_pending_nodes(Host, #jid{node = U, domain = S} = _JID) ->
    States = mnesia:match_object(
	       #pubsub_state{id = {{U,S,undefined}, '_'}, affiliation = 'owner', _ = '_'}),
    NodeIdxs = [NodeIdx || #pubsub_state{id = {_, NodeIdx}} <- States],
    NodeTree = case catch ets:lookup(gen_mod:get_module_proc(Host, 'config'), 'nodetree') of
		   [{'nodetree', Tree}] -> Tree;
		   _                    -> 'nodetree_tree'
	       end,
    Reply = mnesia:foldl(fun
			 (#pubsub_state{id = {_, NodeIdx}} = State, Acc) ->
				case lists:member(NodeIdx, NodeIdxs) of
				    true ->
					case get_nodes_helper(NodeTree, State) of
					    {value, NodeId} -> [NodeId | Acc];
					    false           -> Acc
					end;
				    false -> Acc
				end
			end, [], pubsub_state),
    {result, Reply}.


-spec(get_nodes_helper/2 ::
      (
			 NodeTree :: atom(),
			 State    :: pubsubState())
      -> 'false' | {'value', NodeId::nodeId()}
	    ).

get_nodes_helper(NodeTree, #pubsub_state{id = {_, NodeIdx}, subscriptions = Subscriptions}) ->
    HasPending = fun
		     ({'pending', _}) -> 'true';
						%('pending')      -> 'true'; %% Not useful anymore
	(_)              -> 'false'
											    end,
case lists:any(HasPending, Subscriptions) of
    true ->
	case NodeTree:get_node(NodeIdx) of
	    #pubsub_node{id = {_, NodeId}} -> {value, NodeId};
	    _                              -> false
	end;
    false -> false
end.

%% @spec (NodeId) -> {result, [State] | []}
%%	 NodeId = nodeidx()
%%	 State = mod_pubsub:pubsubState()
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
-spec(get_states/1 ::
      (
		   NodeIdx :: nodeIdx())
      -> {'result', States :: [] | [State::pubsubState()]}
	    ).

get_states(NodeIdx) ->
    States = case
	     catch mnesia:index_read(pubsub_state, NodeIdx, #pubsub_state.nodeidx)
		 of
		 PubsubStates when is_list(PubsubStates) -> PubsubStates; %% is_list(PubsubStates) useful ?
		 _                                       -> []
	     end,
    {result, States}.

%% @spec (NodeId, JID) -> [State] | []
%%	 NodeId = nodeidx()
%%	 JID = ljid()
%%	 State = mod_pubsub:pubsub_item()
%% @doc <p>Returns a state (one state list), given its reference.</p>
-spec(get_state/2 ::
      (
		  NodeIdx :: nodeIdx(),
		  Entity  :: fullUsr())
      -> State::pubsubState()
	    ).

get_state(NodeIdx, Entity) ->
    case catch mnesia:read({pubsub_state, {Entity, NodeIdx}}) of
	[#pubsub_state{} = State] -> State;
	_                         -> #pubsub_state{id = {Entity, NodeIdx}, nodeidx=NodeIdx}
    end.

%% @spec (State) -> ok | {error, Reason::stanzaError()}
%%	 State = mod_pubsub:pubsub_state()
%% @doc <p>Write a state into database.</p>
-spec(set_state/1 ::
      (
		  State :: pubsubState())
      -> 'ok' | {error, 'internal-server-error'}
	    ).

set_state(#pubsub_state{} = State) -> mnesia:write(State);
set_state(_)                       -> {error, 'internal-server-error'}.
%% @spec (NodeId, JID) -> ok | {error, Reason::stanzaError()}
%%	 NodeId = nodeidx()
%% @doc <p>Delete a state from database.</p>
-spec(del_state/2 ::
      (
		  NodeIdx :: nodeIdx(),
		  Entity  :: fullUsr())
      -> 'ok'
	    ).

del_state(NodeIdx, Entity) ->
    mnesia:delete({pubsub_state, {Entity, NodeIdx}}).

%% @spec (NodeId, From) -> [Items] | []
%%	 NodeId = nodeidx()
%%	 Items = mod_pubsub:pubsub_item()
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
-spec(get_items/2 ::
      (
		  NodeIdx :: nodeIdx(),
		  Entity  :: fullUsr())
      -> {'result', Items :: [] | [Item::pubsubItem()]}
	    ).

get_items(NodeIdx, _Entity) ->
    Items = mnesia:index_read(pubsub_item, NodeIdx, #pubsub_item.nodeidx),
    {result, lists:reverse(lists:keysort(#pubsub_item.modification, Items))}.


-spec(get_items/6 ::
      (
		  NodeIdx              :: nodeIdx(),
		  JID                  :: jidEntity(),
		  AccessModel          :: accessModel(),
		  PresenceSubscription :: boolean(),
		  RosterGroup          :: boolean(),
		  SubId                :: subId())
      -> {'result', Items :: [] | [Item::pubsubItem()]}
	     | {'error', _}
	    ).

get_items(NodeIdx, #jid{node = U, domain = S, resource = R} = _JID, AccessModel, PresenceSubscription, RosterGroup, _SubId) ->
    SubKey = {U,S,R},
    GenKey = {U,S,undefined},
    GenState = get_state(NodeIdx, GenKey),
    SubState = get_state(NodeIdx, SubKey),
    Affiliation = GenState#pubsub_state.affiliation,
    Subscriptions = SubState#pubsub_state.subscriptions,
    Whitelisted = can_fetch_item(Affiliation, Subscriptions),
    if
	%%SubId == "", ?? ->
	%% Entity has multiple subscriptions to the node but does not specify a subscription ID
						%{error, ?ERR_EXTENDED('bad-request', "subid-required")};
	%%InvalidSubId ->
	%% Entity is subscribed but specifies an invalid subscription ID
						%{error, ?ERR_EXTENDED('not-acceptable', "invalid-subid")};
	Affiliation == 'outcast' ->
	    %% Requesting entity is blocked
	    {'error', 'forbidden'};
	(AccessModel == 'presence') and (not PresenceSubscription) ->
	    %% Entity is not authorized to create a subscription (presence subscription required)
	    {'error', ?ERR_EXTENDED('not-authorized', "presence-subscription-required")};
	(AccessModel == 'roster') and (not RosterGroup) ->
	    %% Entity is not authorized to create a subscription (not in roster group)
	    {'error', ?ERR_EXTENDED('not-authorized', "not-in-roster-group")};
	(AccessModel == 'whitelist') and (not Whitelisted) ->
	    %% Node has whitelist access model and entity lacks required affiliation
	    {'error', ?ERR_EXTENDED('not-allowed', "closed-node")};
	(AccessModel == 'authorize') and (not Whitelisted) ->
	    %% Node has authorize access model
	    {'error', 'forbidden'};
	%%MustPay ->
	%% % Payment is required for a subscription
	%% {error, ?ERR_PAYMENT_REQUIRED};
	true ->
	    get_items(NodeIdx, {U,S,R})
    end.

%% @spec (NodeId, ItemId) -> [Item] | []
%%	 NodeId = nodeidx()
%%	 ItemId = item()
%%	 Item = mod_pubsub:pubsub_item()
%% @doc <p>Returns an item (one item list), given its reference.</p>
-spec(get_item/2 ::
      (
		 NodeIdx :: nodeIdx(),
		 ItemId  :: itemId())
      -> {'result', Item::pubsubItem()} | {'error', 'item-not-found'}
	    ).

get_item(NodeIdx, ItemId) ->
    case mnesia:read({pubsub_item, {ItemId, NodeIdx}}) of
	[#pubsub_item{} = Item] -> {result, Item};
	_                       -> {error, 'item-not-found'}
    end.


-spec(get_item/7 ::
      (
		 NodeIdx              :: nodeIdx(),
		 ItemId               :: itemId(),
		 JID                  :: jidEntity(),
		 AccessModel          :: accessModel(),
		 PresenceSubscription :: boolean(),
		 RosterGroup          :: boolean(),
		 SubId                :: subId())
      -> {'result', Item::pubsubItem()} | {'error', 'item-not-found'}
	    ).

get_item(NodeIdx, ItemId, #jid{node = U, domain = S} = _JID, AccessModel, PresenceSubscription, RosterGroup, _SubId) ->
    GenKey = {U,S,undefined},
    GenState = get_state(NodeIdx, GenKey),
    Affiliation = GenState#pubsub_state.affiliation,
    Subscriptions = GenState#pubsub_state.subscriptions,
    Whitelisted = can_fetch_item(Affiliation, Subscriptions),
    if
	%%SubId == "", ?? ->
	%% Entity has multiple subscriptions to the node but does not specify a subscription ID
						%{error, ?ERR_EXTENDED('bad-request', "subid-required")};
	%%InvalidSubId ->
	%% Entity is subscribed but specifies an invalid subscription ID
						%{error, ?ERR_EXTENDED('not-acceptable', "invalid-subid")};
	Affiliation == 'outcast' ->
	    %% Requesting entity is blocked
	    {'error', 'forbidden'};
	(AccessModel == 'presence') and (not PresenceSubscription) ->
	    %% Entity is not authorized to create a subscription (presence subscription required)
	    {'error', ?ERR_EXTENDED('not-authorized', "presence-subscription-required")};
	(AccessModel == 'roster') and (not RosterGroup) ->
	    %% Entity is not authorized to create a subscription (not in roster group)
	    {'error', ?ERR_EXTENDED('not-authorized', "not-in-roster-group")};
	(AccessModel == 'whitelist') and (not Whitelisted) ->
	    %% Node has whitelist access model and entity lacks required affiliation
	    {'error', ?ERR_EXTENDED('not-allowed', "closed-node")};
	(AccessModel == 'authorize') and (not Whitelisted) ->
	    %% Node has authorize access model
	    {'error', 'forbidden'};
	%%MustPay ->
	%% % Payment is required for a subscription
	%% {error, ?ERR_PAYMENT_REQUIRED};
	true ->
	    get_item(NodeIdx, ItemId)
    end.

%% @spec (Item) -> ok | {error, Reason::stanzaError()}
%%	 Item = mod_pubsub:pubsub_item()
%% @doc <p>Write an item into database.</p>
-spec(set_item/1 ::
      (
		 Item :: pubsubItem())
      -> 'ok' | {error, 'internal-server-error'}
	    ).

set_item(#pubsub_item{} = Item) -> mnesia:write(Item);
set_item(_)                     -> {error, 'internal-server-error'}.

%% @spec (NodeId, ItemId) -> ok | {error, Reason::stanzaError()}
%%	 NodeId = nodeidx()
%%	 ItemId = item()
%% @doc <p>Delete an item from database.</p>
-spec(del_item/2 ::
      (
		 NodeIdx :: nodeIdx(),
		 ItemId  :: itemId())
      -> 'ok'
	    ).

del_item(NodeIdx, ItemId) ->
    mnesia:delete({pubsub_item, {ItemId, NodeIdx}}).


-spec(del_items/2 ::
      (
		  NodeIdx  :: nodeIdx(),
		  ItemIds  :: [itemId()])
      -> 'ok'
	    ).

del_items(NodeIdx, ItemIds) ->
    lists:foreach(fun(ItemId) ->
			  del_item(NodeIdx, ItemId)
		  end, ItemIds).

%% @doc <p>Return the name of the node if known: Default is to return
%% node id.</p>
get_item_name(_Host, _Node, Id) ->
    Id.

node_to_path(Node) ->
    [Node].

path_to_node([]) ->
    <<>>;
path_to_node(Path) ->
    case Path of
						% default slot
	[Node] -> Node;
						% handle old possible entries, used when migrating database content to new format
	[Node|_] when is_list(Node) -> list_to_binary(string:join([""|Path], "/"));
						% default case (used by PEP for example)
	_ -> list_to_binary(Path)
    end.

%% @spec (Affiliation, Subscription) -> true | false
%%       Affiliation = owner | member | publisher | outcast | none
%%       Subscription = subscribed | none
%% @doc Determines if the combination of Affiliation and Subscribed
%% are allowed to get items from a node.
-spec(can_fetch_item/2 ::
      (
		       Affiliation   :: affiliation(),
		       Subscriptions :: [{subscription(), subId()}])
      -> CanFetchItem::boolean()
	    ).
%% TODO : add 'publish-only' case
can_fetch_item('owner',    _) -> true;
can_fetch_item('member',   _) -> true;
can_fetch_item('publisher',_) -> true;
can_fetch_item('outcast',  _) -> false;
can_fetch_item('none', Subscriptions) -> is_subscribed(Subscriptions);
can_fetch_item(_Affiliation, _Subscription) -> false.


-spec(is_subscribed/1 ::
      (
		      Subscriptions :: [{Subscription::subscription(), SubId::subId()}])
      -> IsSubscribed::boolean()
	    ).

is_subscribed(Subscriptions) ->
    lists:any(fun
	      ({'subscribed', _SubId}) -> true;
	      (_)                      -> false
	     end, Subscriptions).


%% Returns the first item where Pred() is true in List
-spec(first_in_list/2 ::
      (
		      Predicate     :: fun(),
					  Subscriptions :: [{Subscription::subscription(), SubId::subId()}])
      -> 'false' | {'value', {Subscription::subscription(), SubId::subId()}}
	    ).

first_in_list(_Predicate, []) -> false;
first_in_list(Predicate, [Subscription | OtherSubscriptions]) ->
    case Predicate(Subscription) of
	true -> {'value', Subscription};
	_    -> first_in_list(Predicate, OtherSubscriptions)
    end.
