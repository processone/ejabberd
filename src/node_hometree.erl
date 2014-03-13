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
%%% Portions created by ProcessOne are Copyright 2006-2014, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2014, ProcessOne.
%%%
%%%
%%% @copyright 2006-2014 ProcessOne
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

-module(node_hometree).

-author('christophe.romain@process-one.net').

-include("pubsub.hrl").

-include("jlib.hrl").

-behaviour(gen_pubsub_node).

%% API definition
-export([init/3, terminate/2, options/0, features/0,
	 create_node_permission/6, create_node/2, delete_node/1,
	 purge_node/2, subscribe_node/8, unsubscribe_node/4,
	 publish_item/6, delete_item/4, remove_extra_items/3,
	 get_entity_affiliations/2, get_node_affiliations/1,
	 get_affiliation/2, set_affiliation/3,
	 get_entity_subscriptions/2, get_node_subscriptions/1,
	 get_subscriptions/2, set_subscriptions/4,
	 get_pending_nodes/2, get_states/1, get_state/2,
	 set_state/1, get_items/6, get_items/2, get_item/7,
	 get_item/2, set_item/1, get_item_name/3, node_to_path/1,
	 path_to_node/1]).

%% ================
%% API definition
%% ================

%% @spec (Host, ServerHost, Options) -> ok
%%	 Host       = string()
%%	 ServerHost = string()
%%	 Options    = [{atom(), term()}]
%% @doc <p>Called during pubsub modules initialisation. Any pubsub plugin must
%% implement this function. It can return anything.</p>
%% <p>This function is mainly used to trigger the setup task necessary for the
%% plugin. It can be used for example by the developer to create the specific
%% module database schema if it does not exists yet.</p>
init(_Host, _ServerHost, _Options) ->
    pubsub_subscription:init(),
    mnesia:create_table(pubsub_state,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, pubsub_state)}]),
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

%% @spec (Host, ServerHost) -> ok
%%	 Host       = string()
%%	 ServerHost = string()
%% @doc <p>Called during pubsub modules termination. Any pubsub plugin must
%% implement this function. It can return anything.</p>
terminate(_Host, _ServerHost) -> ok.

-spec(options/0 :: () -> NodeOptions::mod_pubsub:nodeOptions()).

%% @spec () -> Options
%%	 Options = [mod_pubsub:nodeOption()]
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
    [{deliver_payloads, true}, {notify_config, false},
     {notify_delete, false}, {notify_retract, true},
     {purge_offline, false}, {persist_items, true},
     {max_items, ?MAXITEMS}, {subscribe, true},
     {access_model, open}, {roster_groups_allowed, []},
     {publish_model, publishers},
     {notification_type, headline},
     {max_payload_size, ?MAX_PAYLOAD_SIZE},
     {send_last_published_item, on_sub_and_presence},
     {deliver_notifications, true},
     {presence_based_delivery, false}].

%% @spec () -> Features
%%	 Features = [string()]
%% @doc Returns the node features
-spec(features/0 :: () -> Features::[binary(),...]).
features() ->
    [<<"create-nodes">>, <<"auto-create">>,
     <<"access-authorize">>, <<"delete-nodes">>,
     <<"delete-items">>, <<"get-pending">>,
     <<"instant-nodes">>, <<"manage-subscriptions">>,
     <<"modify-affiliations">>, <<"multi-subscribe">>,
     <<"outcast-affiliation">>, <<"persistent-items">>,
     <<"publish">>, <<"purge-nodes">>, <<"retract-items">>,
     <<"retrieve-affiliations">>, <<"retrieve-items">>,
     <<"retrieve-subscriptions">>, <<"subscribe">>,
     <<"subscription-notifications">>,
     <<"subscription-options">>].

%% @spec (Host, ServerHost, NodeId, ParentNodeId, Owner, Access) -> {result, Allowed}
%%	 Host         = mod_pubsub:hostPubsub()
%%	 ServerHost   = string()
%%	 NodeId       = mod_pubsub:nodeId()
%%	 ParentNodeId = mod_pubsub:nodeId()
%%	 Owner        = mod_pubsub:jid()
%%	 Access       = all | atom()
%%   Allowed      = boolean()
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
%% ```check_create_user_permission(Host, ServerHost, NodeId, ParentNodeId, Owner, Access) ->
%%	   node_default:check_create_user_permission(Host, ServerHost, NodeId, ParentNodeId, Owner, Access).'''</p>
-spec(create_node_permission/6 ::
(
  Host          :: mod_pubsub:host(),
  ServerHost    :: binary(),
  NodeId        :: mod_pubsub:nodeId(),
  _ParentNodeId :: mod_pubsub:nodeId(),
  Owner         :: jid(),
  Access        :: atom())
    -> {result, boolean()}
).

create_node_permission(Host, ServerHost, NodeId, _ParentNodeId, Owner, Access) ->
    LOwner = jlib:jid_tolower(Owner),
    {User, Server, _Resource} = LOwner,
    Allowed = case LOwner of
		{<<"">>, Host, <<"">>} ->
		    true; % pubsub service always allowed
		_ ->
		    case acl:match_rule(ServerHost, Access, LOwner) of
		      allow ->
			  case node_to_path(NodeId) of
			    [<<"home">>, Server, User | _] -> true;
			    _ -> false
			  end;
		      _ -> false
		    end
	      end,
    {result, Allowed}.

%% @spec (NodeIdx, Owner) -> {result, {default, broadcast}} 
%%	 NodeIdx = mod_pubsub:nodeIdx()
%%	 Owner   = mod_pubsub:jid()
%% @doc <p></p>
-spec(create_node/2 :: 
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  Owner   :: jid())
    -> {result, {default, broadcast}}
).

create_node(NodeIdx, Owner) ->
    OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    set_state(#pubsub_state{stateid = {OwnerKey, NodeIdx}, affiliation = owner}),
    {result, {default, broadcast}}.

%% @spec (Nodes) -> {result, {default, broadcast, Reply}}
%%	 Nodes = [mod_pubsub:pubsubNode()]
%%	 Reply = [{mod_pubsub:pubsubNode(),
%%            [{mod_pubsub:ljid(), [{mod_pubsub:subscription(), mod_pubsub:subId()}]}]}]
%% @doc <p>purge items of deleted nodes after effective deletion.</p>
-spec(delete_node/1 ::
(
  Nodes :: [mod_pubsub:pubsubNode(),...])
    -> {result,
        {default, broadcast,
         [{mod_pubsub:pubsubNode(),
           [{ljid(), [{mod_pubsub:subscription(), mod_pubsub:subId()}]},...]},...]
         }
        }
).
delete_node(Nodes) ->
    Tr = fun (#pubsub_state{stateid = {J, _}, subscriptions = Ss}) ->
		 lists:map(fun (S) -> {J, S} end, Ss)
	end,
    Reply = lists:map(fun (#pubsub_node{id = NodeIdx} = PubsubNode) ->
		{result, States} = get_states(NodeIdx),
		lists:foreach(fun (#pubsub_state{stateid = {LJID, _}, items = Items}) ->
			del_items(NodeIdx, Items),
			del_state(NodeIdx, LJID)
		end, States),
		{PubsubNode, lists:flatmap(Tr, States)}
	end, Nodes),
    {result, {default, broadcast, Reply}}.

%% @spec (NodeIdx, Sender, Subscriber, AccessModel, SendLast, PresenceSubscription, RosterGroup, Options) -> {error, Reason} | {result, Result}
%%	 NodeIdx              = mod_pubsub:nodeIdx()
%%	 Sender               = mod_pubsub:jid()
%%	 Subscriber           = mod_pubsub:jid()
%%	 AccessModel          = mod_pubsub:accessModel()
%%	 SendLast             = atom()
%%	 PresenceSubscription = boolean()
%%	 RosterGroup          = boolean()
%%	 Options              = [mod_pubsub:nodeOption()]
%%	 Reason               = mod_pubsub:stanzaError()
%%	 Result               = {result, {default, subscribed, mod_pubsub:subId()}}
%%                        | {result, {default, subscribed, mod_pubsub:subId(), send_last}}
%%                        | {result, {default, pending,    mod_pubsub:subId()}}
%%
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
  NodeIdx              :: mod_pubsub:nodeIdx(),
  Sender               :: jid(),
  Subscriber           :: ljid(),
  AccessModel          :: mod_pubsub:accessModel(),
  SendLast             :: 'never' | 'on_sub' | 'on_sub_and_presence',
  PresenceSubscription :: boolean(),
  RosterGroup          :: boolean(),
  Options              :: mod_pubsub:subOptions())
    -> {result, {default, subscribed, mod_pubsub:subId()}}
     | {result, {default, subscribed, mod_pubsub:subId(), send_last}}
     | {result, {default, pending, mod_pubsub:subId()}}
    %%%
     | {error, xmlel()}
).
subscribe_node(NodeIdx, Sender, Subscriber, AccessModel,
	       SendLast, PresenceSubscription, RosterGroup, Options) ->
    SubKey = jlib:jid_tolower(Subscriber),
    GenKey = jlib:jid_remove_resource(SubKey),
    Authorized =
	jlib:jid_tolower(jlib:jid_remove_resource(Sender)) ==
	  GenKey,
    GenState = get_state(NodeIdx, GenKey),
    SubState = case SubKey of
		 GenKey -> GenState;
		 _ -> get_state(NodeIdx, SubKey)
	       end,
    Affiliation = GenState#pubsub_state.affiliation,
    Subscriptions = SubState#pubsub_state.subscriptions,
    Whitelisted = lists:member(Affiliation,
			       [member, publisher, owner]),
    PendingSubscription = lists:any(fun ({pending, _}) ->
					    true;
					(_) -> false
				    end,
				    Subscriptions),
    if not Authorized ->
	   {error,
	    ?ERR_EXTENDED((?ERR_BAD_REQUEST), <<"invalid-jid">>)};
       Affiliation == outcast -> {error, ?ERR_FORBIDDEN};
       PendingSubscription ->
	   {error,
	    ?ERR_EXTENDED((?ERR_NOT_AUTHORIZED),
			  <<"pending-subscription">>)};
       (AccessModel == presence) and
	 not PresenceSubscription ->
	   {error,
	    ?ERR_EXTENDED((?ERR_NOT_AUTHORIZED),
			  <<"presence-subscription-required">>)};
       (AccessModel == roster) and not RosterGroup ->
	   {error,
	    ?ERR_EXTENDED((?ERR_NOT_AUTHORIZED),
			  <<"not-in-roster-group">>)};
       (AccessModel == whitelist) and not Whitelisted ->
	   {error,
	    ?ERR_EXTENDED((?ERR_NOT_ALLOWED), <<"closed-node">>)};
       %%MustPay ->
       %%	% Payment is required for a subscription
       %%	{error, ?ERR_PAYMENT_REQUIRED};
       %%ForbiddenAnonymous ->
       %%	% Requesting entity is anonymous
       %%	{error, ?ERR_FORBIDDEN};
       true ->
	        SubId = pubsub_subscription:add_subscription(Subscriber, NodeIdx, Options),
	        NewSub = case AccessModel of
			    authorize -> pending;
			    _ -> subscribed
			  end,
		    set_state(SubState#pubsub_state{subscriptions =
						     [{NewSub, SubId} | Subscriptions]}),
		    case {NewSub, SendLast} of
		        {subscribed, never} ->
		            {result, {default, subscribed, SubId}};
		        {subscribed, _} ->
		            {result, {default, subscribed, SubId, send_last}};
		        {_, _} -> {result, {default, pending, SubId}}
		    end
    end.

%% @spec (NodeIdx, Sender, Subscriber, SubId) ->	{error, Reason} | {result, default}
%%	 NodeIdx    = mod_pubsub:nodeIdx()
%%	 Sender     = mod_pubsub:jid()
%%	 Subscriber = mod_pubsub:jid()
%%	 SubId      = mod_pubsub:subId()
%%	 Reason     = mod_pubsub:stanzaError()
%% @doc <p>Unsubscribe the <tt>Subscriber</tt> from the <tt>Node</tt>.</p>
-spec(unsubscribe_node/4 ::
(
  NodeIdx    :: mod_pubsub:nodeIdx(),
  Sender     :: jid(),
  Subscriber :: ljid(),
  SubId      :: subId())
    -> {result, default}
     %
     | {error, xmlel()}
).

unsubscribe_node(NodeIdx, Sender, Subscriber, SubId) ->
    SubKey = jlib:jid_tolower(Subscriber),
    GenKey = jlib:jid_remove_resource(SubKey),
    Authorized =
	jlib:jid_tolower(jlib:jid_remove_resource(Sender)) ==
	  GenKey,
    GenState = get_state(NodeIdx, GenKey),
    SubState = case SubKey of
		 GenKey -> GenState;
		 _ -> get_state(NodeIdx, SubKey)
	       end,
    Subscriptions = lists:filter(fun ({_Sub, _SubId}) ->
					 true;
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
      not Authorized -> {error, ?ERR_FORBIDDEN};
      %% Entity did not specify SubId
      %%SubId == "", ?? ->
      %%	{error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
      %% Invalid subscription identifier
      %%InvalidSubId ->
      %%	{error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
      %% Requesting entity is not a subscriber
      Subscriptions == [] ->
	  {error,
	   ?ERR_EXTENDED((?ERR_UNEXPECTED_REQUEST_CANCEL),
			 <<"not-subscribed">>)};
      %% Subid supplied, so use that.
      SubIdExists ->
	  Sub = first_in_list(fun (S) ->
				      case S of
					{_Sub, SubId} -> true;
					_ -> false
				      end
			      end,
			      SubState#pubsub_state.subscriptions),
	  case Sub of
	    {value, S} ->
		delete_subscriptions(SubKey, NodeIdx, [S], SubState),
		{result, default};
	    false ->
		{error,
		 ?ERR_EXTENDED((?ERR_UNEXPECTED_REQUEST_CANCEL),
			       <<"not-subscribed">>)}
	  end;
      %% Asking to remove all subscriptions to the given node
      SubId == all ->
	  delete_subscriptions(SubKey, NodeIdx, Subscriptions, SubState),
	  {result, default};
      %% No subid supplied, but there's only one matching subscription
      length(Subscriptions) == 1 ->
	  delete_subscriptions(SubKey, NodeIdx, Subscriptions, SubState),
	  {result, default};
      %% No subid and more than one possible subscription match.
      true ->
	  {error,
	   ?ERR_EXTENDED((?ERR_BAD_REQUEST), <<"subid-required">>)}
    end.

-spec(delete_subscriptions/4 ::
(
  SubKey        :: ljid(),
  NodeIdx       :: mod_pubsub:nodeIdx(),
  Subscriptions :: [{mod_pubsub:subscription(), mod_pubsub:subId()}],
  SubState      :: mod_pubsub:pubsubState())
    -> ok
).
delete_subscriptions(SubKey, NodeIdx, Subscriptions, SubState) ->
    NewSubs = lists:foldl(fun ({Subscription, SubId}, Acc) ->
			pubsub_subscription:delete_subscription(SubKey, NodeIdx, SubId),
			Acc -- [{Subscription, SubId}]
	end, SubState#pubsub_state.subscriptions, Subscriptions),
    case {SubState#pubsub_state.affiliation, NewSubs} of
      {none, []} -> del_state(NodeIdx, SubKey);
      _          -> set_state(SubState#pubsub_state{subscriptions = NewSubs})
    end.

%% @spec (NodeIdx, Publisher, PublishModel, MaxItems, ItemId, Payload) ->
%%		 {result, {default, broadcast, ItemIds}} | {error, Reason}
%%	 NodeIdx      = mod_pubsub:nodeIdx()
%%	 Publisher    = mod_pubsub:jid()
%%	 PublishModel = atom()
%%	 MaxItems     = integer()
%%	 ItemId       = mod_pubsub:itemId()
%%	 Payload      = mod_pubsub:payload()
%%	 ItemIds      = [mod_pubsub:itemId()] | []
%%	 Reason       = mod_pubsub:stanzaError()
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
  NodeIdx      :: mod_pubsub:nodeIdx(),
  Publisher    :: jid(),
  PublishModel :: mod_pubsub:publishModel(),
  Max_Items    :: non_neg_integer(),
  ItemId       :: <<>> | mod_pubsub:itemId(),
  Payload      :: mod_pubsub:payload())
    -> {result, {default, broadcast, [mod_pubsub:itemId()]}}
    %%%
     | {error, xmlel()}
).

publish_item(NodeIdx, Publisher, PublishModel, MaxItems, ItemId, Payload) ->
    SubKey = jlib:jid_tolower(Publisher),
    GenKey = jlib:jid_remove_resource(SubKey),
    GenState = get_state(NodeIdx, GenKey),
    SubState = case SubKey of
		 GenKey -> GenState;
		 _ -> get_state(NodeIdx, SubKey)
	       end,
    Affiliation = GenState#pubsub_state.affiliation,
    Subscribed = case PublishModel of
		   subscribers ->
		       is_subscribed(SubState#pubsub_state.subscriptions);
		   _ -> undefined
		 end,
    if not
	 ((PublishModel == open) or
	    (PublishModel == publishers) and
	      ((Affiliation == owner) or (Affiliation == publisher))
	    or (Subscribed == true)) ->
	   {error, ?ERR_FORBIDDEN};
       true ->
	   if MaxItems > 0 ->
		  Now = now(),
		  PubId = {Now, SubKey},
		  Item = case get_item(NodeIdx, ItemId) of
			   {result, OldItem} ->
			       OldItem#pubsub_item{modification = PubId,
						   payload = Payload};
			   _ ->
			       #pubsub_item{itemid = {ItemId, NodeIdx},
					    creation = {Now, GenKey},
					    modification = PubId,
					    payload = Payload}
			 end,
		  Items = [ItemId | GenState#pubsub_state.items --
				      [ItemId]],
		  {result, {NI, OI}} = remove_extra_items(NodeIdx,
							  MaxItems, Items),
		  set_item(Item),
		  set_state(GenState#pubsub_state{items = NI}),
		  {result, {default, broadcast, OI}};
	      true -> {result, {default, broadcast, []}}
	   end
    end.

%% @spec (NodeIdx, MaxItems, ItemIds) -> {result, {NewItemIds,OldItemIds}}
%%	 NodeIdx    = mod_pubsub:nodeIdx()
%%	 MaxItems   = integer() | unlimited
%%	 ItemIds    = [mod_pubsub:itemId()]
%%	 NewItemIds = [mod_pubsub:itemId()]
%%	 OldItemIds = [mod_pubsub:itemId()] | []
%% @doc <p>This function is used to remove extra items, most notably when the
%% maximum number of items has been reached.</p>
%% <p>This function is used internally by the core PubSub module, as no
%% permission check is performed.</p>
%% <p>In the default plugin module, the oldest items are removed, but other
%% rules can be used.</p>
%% <p>If another PubSub plugin wants to delegate the item removal (and if the
%% plugin is using the default pubsub storage), it can implements this function like this:
%% ```remove_extra_items(NodeIdx, MaxItems, ItemIds) ->
%%	   node_default:remove_extra_items(NodeIdx, MaxItems, ItemIds).'''</p>
-spec(remove_extra_items/3 ::
(
  NodeIdx   :: mod_pubsub:nodeIdx(),
  Max_Items :: unlimited | non_neg_integer(),
  ItemIds   :: [mod_pubsub:itemId()])
    -> {result,
        {NewItems::[mod_pubsub:itemId()],
         OldItems::[mod_pubsub:itemId()]}
       }
).
remove_extra_items(_NodeIdx, unlimited, ItemIds) ->
    {result, {ItemIds, []}};
remove_extra_items(NodeIdx, MaxItems, ItemIds) ->
    NewItems = lists:sublist(ItemIds, MaxItems),
    OldItems = lists:nthtail(length(NewItems), ItemIds),
    del_items(NodeIdx, OldItems),
    {result, {NewItems, OldItems}}.

%% @spec (NodeIdx, Publisher, PublishModel, ItemId) ->
%%     {result, {default, broadcast}} | {error, Reason}
%%	 NodeIdx      = mod_pubsub:nodeIdx()
%%	 Publisher    = mod_pubsub:jid()
%%	 PublishModel = atom()
%%	 ItemId       = mod_pubsub:itemId()
%%   Reason       = mod_pubsub:stanzaError()
%% @doc <p>Triggers item deletion.</p>
%% <p>Default plugin: The user performing the deletion must be the node owner
%% or a publisher, or PublishModel being open.</p>
-spec(delete_item/4 ::
(
  NodeIdx      :: mod_pubsub:nodeIdx(),
  Publisher    :: jid(),
  PublishModel :: mod_pubsub:publishModel(),
  ItemId       :: <<>> | mod_pubsub:itemId())
    -> {result, {default, broadcast}}
    %%%
     | {error, xmlel()}
).
delete_item(NodeIdx, Publisher, PublishModel, ItemId) ->
    SubKey = jlib:jid_tolower(Publisher),
    GenKey = jlib:jid_remove_resource(SubKey),
    GenState = get_state(NodeIdx, GenKey),
    #pubsub_state{affiliation = Affiliation, items = Items} = GenState,
    Allowed = Affiliation == publisher orelse
		Affiliation == owner orelse
		  PublishModel == open orelse
		    case get_item(NodeIdx, ItemId) of
		      {result, #pubsub_item{creation = {_, GenKey}}} -> true;
		      _ -> false
		    end,
    if not Allowed -> {error, ?ERR_FORBIDDEN};
       true ->
	   case lists:member(ItemId, Items) of
	     true ->
		 del_item(NodeIdx, ItemId),
		 set_state(GenState#pubsub_state{items = lists:delete(ItemId, Items)}),
		 {result, {default, broadcast}};
	     false ->
		 case Affiliation of
		   owner ->
		       {result, States} = get_states(NodeIdx),
		       lists:foldl(fun (#pubsub_state{items = PI} = S, Res) ->
					   case lists:member(ItemId, PI) of
					     true ->
						 del_item(NodeIdx, ItemId),
						 set_state(S#pubsub_state{items
									      = lists:delete(ItemId, PI)}),
						 {result, {default, broadcast}};
					     false -> Res
					   end;
				       (_, Res) -> Res
				   end,
				   {error, ?ERR_ITEM_NOT_FOUND}, States);
		   _ -> {error, ?ERR_ITEM_NOT_FOUND}
		 end
	   end
    end.

%% @spec (NodeIdx, Owner) -> {error, Reason} | {result, {default, broadcast}}
%%	 NodeIdx = mod_pubsub:nodeIdx()
%%	 Owner   = mod_pubsub:jid()
%%   Reason  = mod_pubsub:stanzaError()
-spec(purge_node/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  Owner   :: jid())
    -> {result, {default, broadcast}}
     | {error, xmlel()}
).

purge_node(NodeIdx, Owner) ->
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
    GenState = get_state(NodeIdx, GenKey),
    case GenState of
      #pubsub_state{affiliation = owner} ->
	  {result, States} = get_states(NodeIdx),
	  lists:foreach(fun (#pubsub_state{items = []}) -> ok;
			    (#pubsub_state{items = Items} = S) ->
				del_items(NodeIdx, Items),
				set_state(S#pubsub_state{items = []})
			end,
			States),
	  {result, {default, broadcast}};
      _ -> {error, ?ERR_FORBIDDEN}
    end.

%% @spec (Host, Owner) -> {result, Reply}
%%	 Host  = mod_pubsub:hostPubsub()
%%	 Owner = mod_pubsub:jid()
%%	 Reply = [] | [{mod_pubsub:pubsubNode(), mod_pubsub:affiliation()}]
%% @doc <p>Return the current affiliations for the given user</p>
%% <p>The default module reads affiliations in the main Mnesia
%% <tt>pubsub_state</tt> table. If a plugin stores its data in the same
%% table, it should return an empty list, as the affiliation will be read by
%% the default PubSub module. Otherwise, it should return its own affiliation,
%% that will be added to the affiliation stored in the main
%% <tt>pubsub_state</tt> table.</p>
-spec(get_entity_affiliations/2 ::
(
  Host  :: mod_pubsub:host(),
  Owner :: jid())
    -> {result, [{mod_pubsub:pubsubNode(), mod_pubsub:affiliation()}]}
).

get_entity_affiliations(Host, Owner) ->
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
    States = mnesia:match_object(#pubsub_state{stateid = {GenKey, '_'}, _ = '_'}),
    NodeTree = case catch
		      ets:lookup(gen_mod:get_module_proc(Host, config),
				 nodetree)
		   of
		 [{nodetree, N}] -> N;
		 _ -> nodetree_tree
	       end,
    Reply = lists:foldl(fun (#pubsub_state{stateid = {_, N}, affiliation = A},
			     Acc) ->
				case NodeTree:get_node(N) of
				  #pubsub_node{nodeid = {Host, _}} = Node ->
				      [{Node, A} | Acc];
				  _ -> Acc
				end
			end,
			[], States),
    {result, Reply}.

-spec(get_node_affiliations/1 ::
(
  NodeIdx::mod_pubsub:nodeIdx())
    -> {result, [{ljid(), mod_pubsub:affiliation()}]}
).

get_node_affiliations(NodeIdx) ->
    {result, States} = get_states(NodeIdx),
    Tr = fun (#pubsub_state{stateid = {J, _},
			    affiliation = A}) ->
		 {J, A}
	 end,
    {result, lists:map(Tr, States)}.

-spec(get_affiliation/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  Owner   :: jid())
    -> {result, mod_pubsub:affiliation()}
).

get_affiliation(NodeIdx, Owner) ->
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
    #pubsub_state{affiliation = Affiliation} = get_state(NodeIdx, GenKey),
    {result, Affiliation}.

-spec(set_affiliation/3 ::
(
  NodeIdx     :: mod_pubsub:nodeIdx(),
  Owner       :: ljid(),
  Affiliation :: mod_pubsub:affiliation())
    -> ok
).
set_affiliation(NodeIdx, Owner, Affiliation) ->
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
    GenState = get_state(NodeIdx, GenKey),
    case {Affiliation, GenState#pubsub_state.subscriptions} of
      {none, []} -> del_state(NodeIdx, GenKey);
      _ -> set_state(GenState#pubsub_state{affiliation = Affiliation})
    end.

%% @spec (Host, Owner) ->
%%      {'result', []
%%               | [{Node, Subscription, SubId, Entity}]
%%               | [{Node, Subscription, Entity}]}
%%	 Host         = mod_pubsub:hostPubsub()
%%	 Owner        = mod_pubsub:jid()
%%	 Node         = mod_pubsub:pubsubNode()
%%	 Subscription = mod_pubsub:subscription()
%%	 SubId        = mod_pubsub:subId()
%%	 Entity       = mod_pubsub:ljid()
%% @doc <p>Return the current subscriptions for the given user</p>
%% <p>The default module reads subscriptions in the main Mnesia
%% <tt>pubsub_state</tt> table. If a plugin stores its data in the same
%% table, it should return an empty list, as the affiliation will be read by
%% the default PubSub module. Otherwise, it should return its own affiliation,
%% that will be added to the affiliation stored in the main
%% <tt>pubsub_state</tt> table.</p>
-spec(get_entity_subscriptions/2 ::
(
  Host :: mod_pubsub:host(),
  Owner :: jid())
    -> {result,
          [{mod_pubsub:pubsubNode(),
            mod_pubsub:subscription(),
            mod_pubsub:subId(),
            ljid()}]
       }
).

get_entity_subscriptions(Host, Owner) ->
    {U, D, _} = SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
    States = case SubKey of
	       GenKey ->
		   mnesia:match_object(#pubsub_state{stateid =
							 {{U, D, '_'}, '_'},
						     _ = '_'});
	       _ ->
		   mnesia:match_object(#pubsub_state{stateid =
							 {GenKey, '_'},
						     _ = '_'})
		     ++
		     mnesia:match_object(#pubsub_state{stateid =
							   {SubKey, '_'},
						       _ = '_'})
	     end,
    NodeTree = case catch
		      ets:lookup(gen_mod:get_module_proc(Host, config),
				 nodetree)
		   of
		 [{nodetree, N}] -> N;
		 _ -> nodetree_tree
	       end,
    Reply = lists:foldl(fun (#pubsub_state{stateid = {J, N}, subscriptions = Ss},
			     Acc) ->
				case NodeTree:get_node(N) of
				  #pubsub_node{nodeid = {Host, _}} = Node ->
				      lists:foldl(fun ({Sub, SubId}, Acc2) ->
							  [{Node, Sub, SubId, J} | Acc2]
						  end,
						  Acc, Ss);
				  _ -> Acc
				end
			end,
			[], States),
    {result, Reply}.

-spec(get_node_subscriptions/1 ::
(
  NodeIdx::mod_pubsub:nodeIdx())
    -> {result,
        [{ljid(), mod_pubsub:subscription(), mod_pubsub:subId()}] |
        [{ljid(), none},...]
       }
).
get_node_subscriptions(NodeIdx) ->
    {result, States} = get_states(NodeIdx),
    Tr = fun (#pubsub_state{stateid = {J, _},
			    subscriptions = Subscriptions}) ->
		 case Subscriptions of
		   [_ | _] ->
		       lists:foldl(fun ({S, SubId}, Acc) ->
					   [{J, S, SubId} | Acc]
				   end,
				   [], Subscriptions);
		   [] -> [];
		   _ -> [{J, none}]
		 end
	 end,
    {result, lists:flatmap(Tr, States)}.

-spec(get_subscriptions/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  Owner   :: ljid())
    -> {result, [{mod_pubsub:subscription(), mod_pubsub:subId()}]}
).
get_subscriptions(NodeIdx, Owner) ->
    SubKey = jlib:jid_tolower(Owner),
    SubState = get_state(NodeIdx, SubKey),
    {result, SubState#pubsub_state.subscriptions}.

-spec(set_subscriptions/4 ::
(
  NodeIdx      :: mod_pubsub:nodeIdx(),
  Owner        :: jid(),
  Subscription :: mod_pubsub:subscription(),
  SubId        :: mod_pubsub:subId())
    -> ok
    %%%
    | {error, xmlel()}
).

set_subscriptions(NodeIdx, Owner, Subscription, SubId) ->
    SubKey = jlib:jid_tolower(Owner),
    SubState = get_state(NodeIdx, SubKey),
    case {SubId, SubState#pubsub_state.subscriptions} of
      {_, []} ->
	  case Subscription of
	    none ->
		{error,
		 ?ERR_EXTENDED((?ERR_BAD_REQUEST),
			       <<"not-subscribed">>)};
	    _ ->
		new_subscription(NodeIdx, Owner, Subscription, SubState)
	  end;
      {<<>>, [{_, SID}]} ->
	  case Subscription of
	    none -> unsub_with_subid(NodeIdx, SID, SubState);
	    _ -> replace_subscription({Subscription, SID}, SubState)
	  end;
      {<<>>, [_ | _]} ->
	  {error,
	   ?ERR_EXTENDED((?ERR_BAD_REQUEST),
			 <<"subid-required">>)};
      _ ->
	  case Subscription of
	    none -> unsub_with_subid(NodeIdx, SubId, SubState);
	    _ ->
		replace_subscription({Subscription, SubId}, SubState)
	  end
    end.

replace_subscription(NewSub, SubState) ->
    NewSubs = replace_subscription(NewSub, SubState#pubsub_state.subscriptions, []),
    set_state(SubState#pubsub_state{subscriptions = NewSubs}).

replace_subscription(_, [], Acc) -> Acc;
replace_subscription({Sub, SubId}, [{_, SubID} | T], Acc) ->
    replace_subscription({Sub, SubId}, T, [{Sub, SubID} | Acc]).

new_subscription(NodeId, Owner, Subscription, SubState) ->
    SubId = pubsub_subscription:add_subscription(Owner, NodeId, []),
    Subscriptions = SubState#pubsub_state.subscriptions,
    set_state(SubState#pubsub_state{subscriptions =
					[{Subscription, SubId} | Subscriptions]}),
    {Subscription, SubId}.

-spec(unsub_with_subid/3 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  SubId   :: mod_pubsub:subId(),
  SubState :: mod_pubsub:pubsubState())
    -> ok
).
unsub_with_subid(NodeIdx, SubId, #pubsub_state{stateid = {Entity, _}} = SubState) ->
    pubsub_subscription:delete_subscription(SubState#pubsub_state.stateid,
					    NodeIdx, SubId),
    NewSubs = lists:filter(fun ({_, SID}) -> SubId =/= SID
			   end,
			   SubState#pubsub_state.subscriptions),
    case {NewSubs, SubState#pubsub_state.affiliation} of
      {[], none} ->
	    del_state(NodeIdx, Entity);
      _ ->
	  set_state(SubState#pubsub_state{subscriptions = NewSubs})
    end.

%% TODO : doc
%% @spec (Host, Owner) -> {result, Reply} | {error, Reason} 
%%	 Host  = mod_pubsub:hostPubsub()
%%   Owner = mod_pubsub:jid()
%%   Reply = [] | [mod_pubsub:nodeId()]
%% @doc <p>Returns a list of Owner's nodes on Host with pending
%% subscriptions.</p>
-spec(get_pending_nodes/2 ::
(
  Host  :: mod_pubsub:host(),
  Owner :: jid())
    -> {result, [mod_pubsub:nodeId()]}
).

get_pending_nodes(Host, Owner) ->
    GenKey = jlib:jid_remove_resource(jlib:jid_tolower(Owner)),
    States = mnesia:match_object(#pubsub_state{stateid =
						   {GenKey, '_'},
					       affiliation = owner, _ = '_'}),
    NodeIDs = [ID
	       || #pubsub_state{stateid = {_, ID}} <- States],
    NodeTree = case catch
		      ets:lookup(gen_mod:get_module_proc(Host, config),
				 nodetree)
		   of
		 [{nodetree, N}] -> N;
		 _ -> nodetree_tree
	       end,
    Reply = mnesia:foldl(fun (#pubsub_state{stateid = {_, NID}} = S,
			      Acc) ->
				 case lists:member(NID, NodeIDs) of
				   true ->
				       case get_nodes_helper(NodeTree, S) of
					 {value, Node} -> [Node | Acc];
					 false -> Acc
				       end;
				   false -> Acc
				 end
			 end,
			 [], pubsub_state),
    {result, Reply}.

-spec(get_nodes_helper/2 ::
(
  NodeTree     :: module(),
  Pubsub_State :: mod_pubsub:pubsubState())
    -> {value, NodeId::mod_pubsub:nodeId()}
     | false
    
).
get_nodes_helper(NodeTree, #pubsub_state{stateid = {_, N}, subscriptions = Subs}) ->
    HasPending = fun ({pending, _}) -> true;
		     (pending) -> true;
		     (_) -> false
		 end,
    case lists:any(HasPending, Subs) of
      true ->
	  case NodeTree:get_node(N) of
	    #pubsub_node{nodeid = {_, Node}} -> {value, Node};
	    _ -> false
	  end;
      false -> false
    end.

%% @spec (NodeIdx) -> {result, States}
%%	 NodeIdx = mod_pubsub:nodeIdx()
%%	 States  = [] | [mod_pubsub:pubsubState()]
%% @doc Returns the list of stored states for a given node.
%% <p>For the default PubSub module, states are stored in Mnesia database.</p>
%% <p>We can consider that the pubsub_state table have been created by the main
%% mod_pubsub module.</p>
%% <p>PubSub plugins can store the states where they wants (for example in a
%% relational database).</p>
%% <p>If a PubSub plugin wants to delegate the states storage to the default node,
%% they can implement this function like this:
%% ```get_states(NodeIdx) ->
%%	   node_default:get_states(NodeIdx).'''</p>
-spec(get_states/1 ::
(
  NodeIdx::mod_pubsub:nodeIdx())
    -> {result, [mod_pubsub:pubsubState()]}
).

get_states(NodeIdx) ->
    States = case catch mnesia:match_object(
	       #pubsub_state{stateid = {'_', NodeIdx}, _ = '_'}) of
	List when is_list(List) -> List;
	_ -> []
    end,
    {result, States}.

%% @spec (NodeIdx, JID) -> State
%%	 NodeIdx = mod_pubsub:nodeIdx()
%%	 JID     = mod_pubsub:jid()
%%	 State   = mod_pubsub:pubsubState()
%% @doc <p>Returns a state (one state list), given its reference.</p>
-spec(get_state/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  JID     :: ljid())
    -> mod_pubsub:pubsubState()
).

get_state(NodeIdx, JID) ->
    StateId = {JID, NodeIdx},
    case catch mnesia:read({pubsub_state, StateId}) of
	[State] when is_record(State, pubsub_state) -> State;
	_ -> #pubsub_state{stateid=StateId}
    end.

%% @spec (State) -> ok | {error, Reason}
%%	 State  = mod_pubsub:pubsubState()
%%	 Reason = mod_pubsub:stanzaError()
%% @doc <p>Write a state into database.</p>
-spec(set_state/1 ::
(
  State::mod_pubsub:pubsubState())
    -> ok
).
set_state(State) when is_record(State, pubsub_state) ->
    mnesia:write(State).
%set_state(_) -> {error, ?ERR_INTERNAL_SERVER_ERROR}.

%% @spec (NodeIdx, JID) -> ok | {error, Reason}
%%	 NodeIdx = mod_pubsub:nodeIdx()
%%	 JID     = mod_pubsub:jid()
%%	 Reason  = mod_pubsub:stanzaError()
%% @doc <p>Delete a state from database.</p>
-spec(del_state/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  JID     :: ljid())
    -> ok
).
del_state(NodeIdx, JID) ->
    mnesia:delete({pubsub_state, {JID, NodeIdx}}).

%% @spec (NodeIdx, From) -> {result, Items}
%%	 NodeIdx = mod_pubsub:nodeIdx()
%%	 From    = mod_pubsub:jid()
%%	 Items   = [] | [mod_pubsub:pubsubItem()]
%% @doc Returns the list of stored items for a given node.
%% <p>For the default PubSub module, items are stored in Mnesia database.</p>
%% <p>We can consider that the pubsub_item table have been created by the main
%% mod_pubsub module.</p>
%% <p>PubSub plugins can store the items where they wants (for example in a
%% relational database), or they can even decide not to persist any items.</p>
%% <p>If a PubSub plugin wants to delegate the item storage to the default node,
%% they can implement this function like this:
%% ```get_items(NodeIdx, From) ->
%%	   node_default:get_items(NodeIdx, From).'''</p>
-spec(get_items/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  _From   :: jid())
    -> {result, [mod_pubsub:pubsubItem()]}
).

get_items(NodeIdx, _From) ->
    Items = mnesia:match_object(#pubsub_item{itemid = {'_', NodeIdx}, _ = '_'}),
    {result, lists:reverse(lists:keysort(#pubsub_item.modification, Items))}.

-spec(get_items/6 ::
(
  NodeIdx               :: mod_pubsub:nodeIdx(),
  JID                   :: jid(),
  AccessModel           :: mod_pubsub:accessModel(),
  Presence_Subscription :: boolean(),
  RosterGroup           :: boolean(),
  _SubId                :: mod_pubsub:subId())
    -> {result, [mod_pubsub:pubsubItem()]}
    %%%
     | {error, xmlel()}
).

get_items(NodeIdx, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId) ->
    SubKey = jlib:jid_tolower(JID),
    GenKey = jlib:jid_remove_resource(SubKey),
    GenState = get_state(NodeIdx, GenKey),
    SubState = get_state(NodeIdx, SubKey),
    Affiliation = GenState#pubsub_state.affiliation,
    Subscriptions = SubState#pubsub_state.subscriptions,
    Whitelisted = can_fetch_item(Affiliation, Subscriptions),
    if %%SubId == "", ?? ->
       %% Entity has multiple subscriptions to the node but does not specify a subscription ID
       %{error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
       %%InvalidSubId ->
       %% Entity is subscribed but specifies an invalid subscription ID
       %{error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
       GenState#pubsub_state.affiliation == outcast ->
	   {error, ?ERR_FORBIDDEN};
       (AccessModel == presence) and
	 not PresenceSubscription ->
	   {error,
	    ?ERR_EXTENDED((?ERR_NOT_AUTHORIZED),
			  <<"presence-subscription-required">>)};
       (AccessModel == roster) and not RosterGroup ->
	   {error,
	    ?ERR_EXTENDED((?ERR_NOT_AUTHORIZED),
			  <<"not-in-roster-group">>)};
       (AccessModel == whitelist) and not Whitelisted ->
	   {error,
	    ?ERR_EXTENDED((?ERR_NOT_ALLOWED), <<"closed-node">>)};
       (AccessModel == authorize) and not Whitelisted ->
	   {error, ?ERR_FORBIDDEN};
       %%MustPay ->
       %%	% Payment is required for a subscription
       %%	{error, ?ERR_PAYMENT_REQUIRED};
       true -> get_items(NodeIdx, JID)
    end.

%% @spec (NodeIdx, ItemId) -> {result, Item} | {error, 'item-not-found'}
%%	 NodeIdx = mod_pubsub:nodeIdx()
%%	 ItemId  = mod_pubsub:itemId()
%%	 Item    = mod_pubsub:pubsubItem()
%% @doc <p>Returns an item (one item list), given its reference.</p>
-spec(get_item/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  ItemId  :: mod_pubsub:itemId())
    -> {result, mod_pubsub:pubsubItem()}
     | {error, xmlel()}
).

get_item(NodeIdx, ItemId) ->
    case mnesia:read({pubsub_item, {ItemId, NodeIdx}}) of
        [Item] when is_record(Item, pubsub_item) -> {result, Item};
        _ -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

%% @spec (NodeIdx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) -> {result, Item} | {error, Reason}
%%	 NodeIdx              = mod_pubsub:nodeIdx()
%%	 ItemId               = mod_pubsub:itemId()
%%	 JID                  = mod_pubsub:jid()
%%	 AccessModel          = mod_pubsub:accessModel()
%%	 PresenceSubscription = boolean()
%%	 RosterGroup          = boolean()
%%	 SubId                = mod_pubsub:subId()
%%	 Item                 = mod_pubsub:pubsubItem()
%%	 Reason               = mod_pubsub:stanzaError() | 'item-not-found'
-spec(get_item/7 ::
(
  NodeIdx              :: mod_pubsub:nodeIdx(),
  ItemId               :: mod_pubsub:itemId(),
  JID                  :: jid(),
  AccessModel          :: mod_pubsub:accessModel(),
  PresenceSubscription :: boolean(),
  RosterGroup          :: boolean(),
  SubId                :: mod_pubsub:subId())
    -> {result, mod_pubsub:pubsubItem()}
     | {error, xmlel()}
).

get_item(NodeIdx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup,
  _SubId) ->
    SubKey = jlib:jid_tolower(JID),
    GenKey = jlib:jid_remove_resource(SubKey),
    GenState = get_state(NodeIdx, GenKey),
    Affiliation = GenState#pubsub_state.affiliation,
    Subscriptions = GenState#pubsub_state.subscriptions,
    Whitelisted = can_fetch_item(Affiliation, Subscriptions),
    if %%SubId == "", ?? ->
       %% Entity has multiple subscriptions to the node but does not specify a subscription ID
       %{error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
       %%InvalidSubId ->
       %% Entity is subscribed but specifies an invalid subscription ID
       %{error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
       GenState#pubsub_state.affiliation == outcast ->
	   {error, ?ERR_FORBIDDEN};
       (AccessModel == presence) and
	 not PresenceSubscription ->
	   {error,
	    ?ERR_EXTENDED((?ERR_NOT_AUTHORIZED),
			  <<"presence-subscription-required">>)};
       (AccessModel == roster) and not RosterGroup ->
	   {error,
	    ?ERR_EXTENDED((?ERR_NOT_AUTHORIZED), <<"not-in-roster-group">>)};
       (AccessModel == whitelist) and not Whitelisted ->
	   {error,
	    ?ERR_EXTENDED((?ERR_NOT_ALLOWED), <<"closed-node">>)};
       (AccessModel == authorize) and not Whitelisted ->
	   {error, ?ERR_FORBIDDEN};
       %%MustPay ->
       %%	% Payment is required for a subscription
       %%	{error, ?ERR_PAYMENT_REQUIRED};
       true -> get_item(NodeIdx, ItemId)
    end.

%% @spec (Item) -> ok | {error, Reason}
%%	 Item   = mod_pubsub:pubsubItem()
%%	 Reason = mod_pubsub:stanzaError()
%% @doc <p>Write an item into database.</p>
-spec(set_item/1 ::
(
  Item::mod_pubsub:pubsubItem())
    -> ok
).
set_item(Item) when is_record(Item, pubsub_item) ->
    mnesia:write(Item).
%set_item(_) -> {error, ?ERR_INTERNAL_SERVER_ERROR}.

%% @spec (NodeIdx, ItemId) -> ok | {error, Reason}
%%	 NodeIdx = mod_pubsub:nodeIdx()
%%	 ItemId  = mod_pubsub:itemId()
%%	 Reason  = mod_pubsub:stanzaError()
%% @doc <p>Delete an item from database.</p>
-spec(del_item/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  ItemId  :: mod_pubsub:itemId())
    -> ok
).
del_item(NodeIdx, ItemId) ->
    mnesia:delete({pubsub_item, {ItemId, NodeIdx}}).

-spec(del_items/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  ItemIds :: [mod_pubsub:pubsubItem(),...])
    -> ok
).

del_items(NodeIdx, ItemIds) ->
    lists:foreach(fun (ItemId) -> del_item(NodeIdx, ItemId)
		  end,
		  ItemIds).

get_item_name(_Host, _Node, Id) -> Id.

%% @doc <p>Return the name of the node if known: Default is to return
%% node id.</p>
-spec(node_to_path/1 ::
(
  Node::binary())
    -> [binary()]
).
node_to_path(Node) -> str:tokens((Node), <<"/">>).

-spec(path_to_node/1 ::
(
  Path :: [binary()])
    -> binary()
).

path_to_node([]) -> <<>>;
path_to_node(Path) ->
    iolist_to_binary(str:join([<<"">> | Path], <<"/">>)).

%% @spec (Affiliation, Subscription) -> true | false
%%       Affiliation = owner | member | publisher | outcast | none
%%       Subscription = subscribed | none
%% @doc Determines if the combination of Affiliation and Subscribed
%% are allowed to get items from a node.
can_fetch_item(owner, _) -> true;
can_fetch_item(member, _) -> true;
can_fetch_item(publisher, _) -> true;
can_fetch_item(outcast, _) -> false;
can_fetch_item(none, Subscriptions) ->
    is_subscribed(Subscriptions).
%can_fetch_item(_Affiliation, _Subscription) -> false.

is_subscribed(Subscriptions) ->
    lists:any(fun ({subscribed, _SubId}) -> true;
		  (_) -> false
	      end,
	      Subscriptions).

%% Returns the first item where Pred() is true in List
first_in_list(_Pred, []) -> false;
first_in_list(Pred, [H | T]) ->
    case Pred(H) of
      true -> {value, H};
      _ -> first_in_list(Pred, T)
    end.
