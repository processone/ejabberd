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

-author('christophe.romain@process-one.net').

-include("pubsub.hrl").

-include("jlib.hrl").

-define(PUBSUB, mod_pubsub_odbc).

-behaviour(gen_pubsub_node).

%% API definition
-export([init/3, terminate/2, options/0, features/0,
	 create_node_permission/6, create_node/2, delete_node/1,
	 purge_node/2, subscribe_node/8, unsubscribe_node/4,
	 publish_item/6, delete_item/4, remove_extra_items/3,
	 get_entity_affiliations/2, get_node_affiliations/1,
	 get_affiliation/2, set_affiliation/3,
	 get_entity_subscriptions/2,
	 get_entity_subscriptions_for_send_last/2,
	 get_node_subscriptions/1, get_subscriptions/2,
	 set_subscriptions/4, get_pending_nodes/2, get_states/1,
	 get_state/2, set_state/1, get_items/7, get_items/6,
	 get_items/3, get_items/2, get_item/7, get_item/2,
	 set_item/1, get_item_name/3, get_last_items/3,
	 path_to_node/1, node_to_path/1]).

-export([decode_jid/1,
	 decode_affiliation/1, decode_subscriptions/1,
	 encode_jid/1, encode_affiliation/1,
	 encode_subscriptions/1]).

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
    pubsub_subscription_odbc:init(), ok.

%% @spec (Host, ServerHost) -> any()
%%	 Host = mod_pubsub:host()
%%	 ServerHost = host()
%% @doc <p>Called during pubsub modules termination. Any pubsub plugin must
%% implement this function. It can return anything.</p>
terminate(_Host, _ServerHost) -> ok.

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
-spec(options/0 :: () -> NodeOptions::mod_pubsub:nodeOptions()).
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
     {presence_based_delivery, false}, {odbc, true},
     {rsm, true}].

%% @spec () -> []
%% @doc Returns the node features
-spec(features/0 :: () -> Features::[Feature::binary(),...]).
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
     <<"subscription-options">>, <<"rsm">>].

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
-spec(create_node_permission/6 ::
(
  Host        :: mod_pubsub:host(),
  ServerHost  :: binary(),
  Node        :: mod_pubsub:nodeId(),
  _ParentNode :: _,
  Owner       :: jid(),
  Access      :: atom())
    -> {result, boolean()}
).
create_node_permission(Host, ServerHost, Node, _ParentNode, Owner, Access) ->
    LOwner = jlib:jid_tolower(Owner),
    {User, Server, _Resource} = LOwner,
    Allowed = case LOwner of
		{<<"">>, Host, <<"">>} ->
		    true; % pubsub service always allowed
		_ ->
		    case acl:match_rule(ServerHost, Access, LOwner) of
		      allow ->
			  case node_to_path(Node) of
			    [<<"home">>, Server, User | _] -> true;
			    _ -> false
			  end;
		      _ -> false
		    end
	      end,
    {result, Allowed}.

%% @spec (NodeId, Owner) ->
%%		  {result, Result} | exit
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 Owner = mod_pubsub:jid()
%% @doc <p></p>
-spec(create_node/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  Owner   :: jid())
    -> {result, {default, broadcast}}
).
create_node(NodeIdx, Owner) ->
    OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    State = #pubsub_state{stateid = {OwnerKey, NodeIdx}, affiliation = owner},
    catch
      ejabberd_odbc:sql_query_t([<<"insert into pubsub_state(nodeid, jid, "
				   "affiliation, subscriptions) values(">>,
				 state_to_raw(NodeIdx, State), <<");">>]),
    {result, {default, broadcast}}.

%% @spec (Removed) -> ok
%%	 Removed = [mod_pubsub:pubsubNode()]
%% @doc <p>purge items of deleted nodes after effective deletion.</p>
-spec(delete_node/1 ::
(
  Removed :: [mod_pubsub:pubsubNode(),...])
    -> {result, {default, broadcast, _}}
).
delete_node(Removed) ->
    Reply = lists:map(fun (#pubsub_node{id = NodeId} =
			       PubsubNode) ->
			      Subscriptions = case catch
						     ejabberd_odbc:sql_query_t([<<"select jid, subscriptions from pubsub_state "
										  "where nodeid='">>,
										NodeId,
										<<"';">>])
						  of
						{selected,
						 [<<"jid">>,
						  <<"subscriptions">>],
						 RItems} ->
						    lists:map(fun ([SJID,
								    Subscriptions]) ->
								      {decode_jid(SJID),
								       decode_subscriptions(Subscriptions)}
							      end,
							      RItems);
						_ -> []
					      end,
			      {PubsubNode, Subscriptions}
		      end,
		      Removed),
    {result, {default, broadcast, Reply}}.

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
     | {error, _}
     | {error, _, binary()}
).
subscribe_node(NodeId, Sender, Subscriber, AccessModel,
	       SendLast, PresenceSubscription, RosterGroup, Options) ->
    SubKey = jlib:jid_tolower(Subscriber),
    GenKey = jlib:jid_remove_resource(SubKey),
    Authorized =
	jlib:jid_tolower(jlib:jid_remove_resource(Sender)) ==
	  GenKey,
    {Affiliation, Subscriptions} =
	select_affiliation_subscriptions(NodeId, GenKey,
					 SubKey),
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
	   {result, SubId} = pubsub_subscription_odbc:subscribe_node(Subscriber, NodeId, Options),
		 NewSub = case AccessModel of
			    authorize -> pending;
			    _ -> subscribed
			  end,
		 update_subscription(NodeId, SubKey,
				     [{NewSub, SubId} | Subscriptions]),
		 case {NewSub, SendLast} of
		   {subscribed, never} ->
		       {result, {default, subscribed, SubId}};
		   {subscribed, _} ->
		       {result, {default, subscribed, SubId, send_last}};
		   {_, _} -> {result, {default, pending, SubId}}
		 end
    end.

%% @spec (NodeId, Sender, Subscriber, SubId) ->
%%			{error, Reason} | {result, []}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 Sender = mod_pubsub:jid()
%%	 Subscriber = mod_pubsub:jid()
%%	 SubId = mod_pubsub:subid()
%%	 Reason = mod_pubsub:stanzaError()
%% @doc <p>Unsubscribe the <tt>Subscriber</tt> from the <tt>Node</tt>.</p>
-spec(unsubscribe_node/4 ::
(
  NodeIdx    :: mod_pubsub:nodeIdx(),
  Sender     :: jid(),
  Subscriber :: jid(),
  SubId      :: subId())
    -> {result, default}
     %
     | {error, _}
     | {error, _, binary()}
).
unsubscribe_node(NodeId, Sender, Subscriber, SubId) ->
    SubKey = jlib:jid_tolower(Subscriber),
    GenKey = jlib:jid_remove_resource(SubKey),
    Authorized =
	jlib:jid_tolower(jlib:jid_remove_resource(Sender)) ==
	  GenKey,
    {Affiliation, Subscriptions} =
	select_affiliation_subscriptions(NodeId, SubKey),
    SubIdExists = case SubId of
		    <<>> -> false;
		    List when is_binary(List) -> true;
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
			      Subscriptions),
	  case Sub of
	    {value, S} ->
		delete_subscription(SubKey, NodeId, S, Affiliation,
				    Subscriptions),
		{result, default};
	    false ->
		{error,
		 ?ERR_EXTENDED((?ERR_UNEXPECTED_REQUEST_CANCEL),
			       <<"not-subscribed">>)}
	  end;
      %% Asking to remove all subscriptions to the given node
      SubId == all ->
	  [delete_subscription(SubKey, NodeId, S, Affiliation,
			       Subscriptions)
	   || S <- Subscriptions],
	  {result, default};
      %% No subid supplied, but there's only one matching
      %% subscription, so use that.
      length(Subscriptions) == 1 ->
	  delete_subscription(SubKey, NodeId, hd(Subscriptions),
			      Affiliation, Subscriptions),
	  {result, default};
      %% No subid and more than one possible subscription match.
      true ->
	  {error,
	   ?ERR_EXTENDED((?ERR_BAD_REQUEST), <<"subid-required">>)}
    end.

%-spec(delete_subscriptions/5 ::
%(
%  SubKey        :: ljid(),
%  NodeIdx       :: mod_pubsub:nodeIdx(),
%  _             :: {mod_pubsub:subscription(), mod_pubsub:subId()},
%  SubState      :: mod_pubsub:pubsubState(),
%  Subscriptions :: [{mod_pubsub:subscription(), mod_pubsub:subId()}])
%    -> ok
%).
delete_subscription(SubKey, NodeIdx,
		    {Subscription, SubId}, Affiliation, Subscriptions) ->
    NewSubs = Subscriptions -- [{Subscription, SubId}],
    pubsub_subscription_odbc:unsubscribe_node(SubKey, NodeIdx, SubId),
    case {Affiliation, NewSubs} of
      {none, []} -> del_state(NodeIdx, SubKey);
      _ -> update_subscription(NodeIdx, SubKey, NewSubs)
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
     | {error, _}
).
publish_item(NodeIdx, Publisher, PublishModel, MaxItems, ItemId, Payload) ->
    SubKey = jlib:jid_tolower(Publisher),
    GenKey = jlib:jid_remove_resource(SubKey),
    {Affiliation, Subscriptions} =
	select_affiliation_subscriptions(NodeIdx, GenKey, SubKey),
    Subscribed = case PublishModel of
		   subscribers -> is_subscribed(Subscriptions);
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
		  PubId = {now(), SubKey},
		  set_item(#pubsub_item{itemid = {ItemId, NodeIdx},
					creation = {now(), GenKey},
					modification = PubId,
					payload = Payload}),
		  Items = [ItemId | itemids(NodeIdx, GenKey) -- [ItemId]],
		  {result, {_, OI}} = remove_extra_items(NodeIdx, MaxItems, Items),
		  {result, {default, broadcast, OI}};
	      true -> {result, {default, broadcast, []}}
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
    del_items(NodeId, OldItems),
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
-spec(delete_item/4 ::
(
  NodeIdx      :: mod_pubsub:nodeIdx(),
  Publisher    :: jid(),
  PublishModel :: mod_pubsub:publishModel(),
  ItemId       :: <<>> | mod_pubsub:itemId())
    -> {result, {default, broadcast}}
    %%%
     | {error, _}
).
delete_item(NodeIdx, Publisher, PublishModel, ItemId) ->
    SubKey = jlib:jid_tolower(Publisher),
    GenKey = jlib:jid_remove_resource(SubKey),
    {result, Affiliation} = get_affiliation(NodeIdx, GenKey),
    Allowed = Affiliation == publisher orelse
		Affiliation == owner orelse
		  PublishModel == open orelse
		    case get_item(NodeIdx, ItemId) of
		      {result, #pubsub_item{creation = {_, GenKey}}} -> true;
		      _ -> false
		    end,
    if not Allowed -> {error, ?ERR_FORBIDDEN};
       true ->
	   case del_item(NodeIdx, ItemId) of
	     {updated, 1} -> {result, {default, broadcast}};
	     _ -> {error, ?ERR_ITEM_NOT_FOUND}
	   end
    end.

%% @spec (NodeId, Owner) ->
%%		  {error, Reason::stanzaError()} |
%%		  {result, {default, broadcast}}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 Owner = mod_pubsub:jid()
-spec(purge_node/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  Owner   :: jid())
    -> {result, {default, broadcast}}
     | {error, _}
).
purge_node(NodeIdx, Owner) ->
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
    GenState = get_state(NodeIdx, GenKey),
    case GenState of
      #pubsub_state{affiliation = owner} ->
	  {result, States} = get_states(NodeIdx),
	  lists:foreach(fun (#pubsub_state{items = []}) -> ok;
			    (#pubsub_state{items = Items}) ->
				del_items(NodeIdx, Items)
			end,
			States),
	  {result, {default, broadcast}};
      _ -> {error, ?ERR_FORBIDDEN}
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
-spec(get_entity_affiliations/2 ::
(
  Host  :: mod_pubsub:hostPubsub(),
  Owner :: jid())
    -> {result, [{mod_pubsub:pubsubNode(), mod_pubsub:affiliation()}]}
).
get_entity_affiliations(Host, Owner) ->
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
    H = (?PUBSUB):escape(Host),
    J = encode_jid(GenKey),
    Reply = case catch
		   ejabberd_odbc:sql_query_t([<<"select node, type, i.nodeid, affiliation "
						"from pubsub_state i, pubsub_node n where "
						"i.nodeid = n.nodeid and jid='">>,
					      J, <<"' and host='">>, H,
					      <<"';">>])
		of
	      {selected,
	       [<<"node">>, <<"type">>, <<"nodeid">>,
		<<"affiliation">>],
	       RItems} ->
		  lists:map(fun ([N, T, I, A]) ->
				    Node = nodetree_tree_odbc:raw_to_node(Host,
									  [N,
									   <<"">>,
									   T,
									   I]),
				    {Node, decode_affiliation(A)}
			    end,
			    RItems);
	      _ -> []
	    end,
    {result, Reply}.

-spec(get_node_affiliations/1 ::
(
  NodeIdx::mod_pubsub:nodeIdx())
    -> {result, [{ljid(), mod_pubsub:affiliation()}]}
).
get_node_affiliations(NodeIdx) ->
    Reply = case catch
		   ejabberd_odbc:sql_query_t([<<"select jid, affiliation from pubsub_state "
						"where nodeid='">>,
					      NodeIdx, <<"';">>])
		of
	      {selected, [<<"jid">>, <<"affiliation">>], RItems} ->
		  lists:map(fun ([J, A]) ->
				    {decode_jid(J), decode_affiliation(A)}
			    end,
			    RItems);
	      _ -> []
	    end,
    {result, Reply}.

-spec(get_affiliation/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  Owner   :: ljid())
    -> {result, mod_pubsub:affiliation()}
).

get_affiliation(NodeIdx, Owner) ->
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
    J = encode_jid(GenKey),
    Reply = case catch
		   ejabberd_odbc:sql_query_t([<<"select affiliation from pubsub_state "
						"where nodeid='">>,
					      NodeIdx, <<"' and jid='">>, J,
					      <<"';">>])
		of
	      {selected, [<<"affiliation">>], [[A]]} ->
		  decode_affiliation(A);
	      _ -> none
	    end,
    {result, Reply}.

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
    {_, Subscriptions} = select_affiliation_subscriptions(NodeIdx, GenKey),
    case {Affiliation, Subscriptions} of
      {none, none} -> del_state(NodeIdx, GenKey);
      _ -> update_affiliation(NodeIdx, GenKey, Affiliation)
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
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
    H = (?PUBSUB):escape(Host),
    SJ = encode_jid(SubKey),
    GJ = encode_jid(GenKey),
    Query = case SubKey of
	      GenKey ->
		  [<<"select node, type, i.nodeid, jid, subscriptio"
		     "ns from pubsub_state i, pubsub_node "
		     "n where i.nodeid = n.nodeid and jid "
		     "like '">>,
		   GJ, <<"%' and host='">>, H, <<"';">>];
	      _ ->
		  [<<"select node, type, i.nodeid, jid, subscriptio"
		     "ns from pubsub_state i, pubsub_node "
		     "n where i.nodeid = n.nodeid and jid "
		     "in ('">>,
		   SJ, <<"', '">>, GJ, <<"') and host='">>, H, <<"';">>]
	    end,
    Reply = case catch ejabberd_odbc:sql_query_t(Query) of
	      {selected,
	       [<<"node">>, <<"type">>, <<"nodeid">>, <<"jid">>,
		<<"subscriptions">>],
	       RItems} ->
		  lists:foldl(fun ([N, T, I, J, S], Acc) ->
				      Node =
					  nodetree_tree_odbc:raw_to_node(Host,
									 [N,
									  <<"">>,
									  T,
									  I]),
				      Jid = decode_jid(J),
				      case decode_subscriptions(S) of
					[] -> [{Node, none, Jid} | Acc];
					Subs ->
					    lists:foldl(fun ({Sub, SubId}, Acc2) ->
								[{Node, Sub, SubId, Jid} | Acc2]
							end,
							Acc, Subs)
				      end
			      end,
			      [], RItems);
	      _ -> []
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
    H = (?PUBSUB):escape(Host),
    SJ = encode_jid(SubKey),
    GJ = encode_jid(GenKey),
    Query = case SubKey of
	      GenKey ->
		  [<<"select node, type, i.nodeid, jid, subscriptio"
		     "ns from pubsub_state i, pubsub_node "
		     "n, pubsub_node_option o where i.nodeid "
		     "= n.nodeid and n.nodeid = o.nodeid and "
		     "name='send_last_published_item' and "
		     "val='on_sub_and_presence' and jid like "
		     "'">>,
		   GJ, <<"%' and host='">>, H, <<"';">>];
	      _ ->
		  [<<"select node, type, i.nodeid, jid, subscriptio"
		     "ns from pubsub_state i, pubsub_node "
		     "n, pubsub_node_option o where i.nodeid "
		     "= n.nodeid and n.nodeid = o.nodeid and "
		     "name='send_last_published_item' and "
		     "val='on_sub_and_presence' and jid in "
		     "('">>,
		   SJ, <<"', '">>, GJ, <<"') and host='">>, H, <<"';">>]
	    end,
    Reply = case catch ejabberd_odbc:sql_query_t(Query) of
	      {selected,
	       [<<"node">>, <<"type">>, <<"nodeid">>, <<"jid">>,
		<<"subscriptions">>],
	       RItems} ->
		  lists:foldl(fun ([N, T, I, J, S], Acc) ->
				      Node =
					  nodetree_tree_odbc:raw_to_node(Host,
									 [N,
									  <<"">>,
									  T,
									  I]),
				      Jid = decode_jid(J),
				      case decode_subscriptions(S) of
					[] -> [{Node, none, Jid} | Acc];
					Subs ->
					    lists:foldl(fun ({Sub, SubId}, Acc2) ->
								[{Node, Sub, SubId, Jid}| Acc2]
							end,
							Acc, Subs)
				      end
			      end,
			      [], RItems);
	      _ -> []
	    end,
    {result, Reply}.

-spec(get_node_subscriptions/1 ::
(
  NodeIdx::mod_pubsub:nodeIdx())
    -> {result, [{ljid(), mod_pubsub:subscription(), mod_pubsub:subId()}]}
).
get_node_subscriptions(NodeIdx) ->
    Reply = case catch
		   ejabberd_odbc:sql_query_t([<<"select jid, subscriptions from pubsub_state "
						"where nodeid='">>,
					      NodeIdx, <<"';">>])
		of
	      {selected, [<<"jid">>, <<"subscriptions">>], RItems} ->
		  lists:foldl(fun ([J, S], Acc) ->
				      Jid = decode_jid(J),
				      case decode_subscriptions(S) of
					[] -> [{Jid, none} | Acc];
					Subs ->
					    lists:foldl(fun ({Sub, SubId}, Acc2) ->
								[{Jid, Sub, SubId} | Acc2]
							end,
							Acc, Subs)
				      end
			      end,
			      [], RItems);
	      _ -> []
	    end,
    {result, Reply}.

-spec(get_subscriptions/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  Owner   :: ljid())
    -> {result, [{mod_pubsub:subscription(), mod_pubsub:subId()}]}
).
get_subscriptions(NodeIdx, Owner) ->
    SubKey = jlib:jid_tolower(Owner),
    J = encode_jid(SubKey),
    Reply = case catch
		   ejabberd_odbc:sql_query_t([<<"select subscriptions from pubsub_state "
						"where nodeid='">>,
					      NodeIdx, <<"' and jid='">>, J,
					      <<"';">>])
		of
	      {selected, [<<"subscriptions">>], [[S]]} ->
		  decode_subscriptions(S);
	      _ -> []
	    end,
    {result, Reply}.

-spec(set_subscriptions/4 ::
(
  NodeIdx      :: mod_pubsub:nodeIdx(),
  Owner        :: jid(),
  Subscription :: mod_pubsub:subscription(),
  SubId        :: mod_pubsub:subId())
    -> _
    %%%
     | {error, xmlel()}
).
set_subscriptions(NodeIdx, Owner, Subscription, SubId) ->
    SubKey = jlib:jid_tolower(Owner),
    SubState = get_state_without_itemids(NodeIdx, SubKey),
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
      {<<"">>, [{_, SID}]} ->
	  case Subscription of
	    none -> unsub_with_subid(NodeIdx, SID, SubState);
	    _ -> replace_subscription({Subscription, SID}, SubState)
	  end;
      {<<"">>, [_ | _]} ->
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

-spec(replace_subscription/2 ::
(
  NewSub :: {mod_pubsub:subscription(), mod_pubsub:subId()},
  SubState :: mod_pubsub:pubsubState())
    -> {result, []}
).
replace_subscription(NewSub, SubState) ->
    NewSubs = replace_subscription(NewSub, SubState#pubsub_state.subscriptions, []),
    set_state(SubState#pubsub_state{subscriptions = NewSubs}).

replace_subscription(_, [], Acc) -> Acc;
replace_subscription({Sub, SubId}, [{_, SubID} | T], Acc) ->
    replace_subscription({Sub, SubId}, T, [{Sub, SubID} | Acc]).

-spec(new_subscription/4 ::
(
  NodeIdx      :: mod_pubsub:nodeIdx(),
  Owner        :: jid(),
  Subscription :: mod_pubsub:subscription(),
  SubState     :: mod_pubsub:pubsubState())
    -> {mod_pubsub:subscription(), mod_pubsub:subId()}
    %%%
     | {error, xmlel()}
).

new_subscription(NodeIdx, Owner, Subscription, SubState) ->
    case pubsub_subscription_odbc:subscribe_node(Owner, NodeIdx, []) of
      {result, SubId} ->
	  Subscriptions = SubState#pubsub_state.subscriptions,
	  set_state(SubState#pubsub_state{subscriptions =
					      [{Subscription, SubId} | Subscriptions]}),
	  {Subscription, SubId};
      _ -> {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

-spec(unsub_with_subid/3 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  SubId   :: mod_pubsub:subId(),
  SubState :: mod_pubsub:pubsubState())
    -> ok
).
unsub_with_subid(NodeIdx, SubId, SubState) ->
    pubsub_subscription_odbc:unsubscribe_node(SubState#pubsub_state.stateid,
					      NodeIdx, SubId),
    NewSubs = lists:filter(fun ({_, SID}) -> SubId =/= SID
			   end,
			   SubState#pubsub_state.subscriptions),
    case {NewSubs, SubState#pubsub_state.affiliation} of
      {[], none} ->
	  del_state(NodeIdx,
		    element(1, SubState#pubsub_state.stateid));
      _ ->
	  set_state(SubState#pubsub_state{subscriptions = NewSubs})
    end.

%% @spec (Host, Owner) -> {result, [Node]} | {error, Reason}
%%       Host = host()
%%       Owner = jid()
%%       Node = pubsubNode()
%% @doc <p>Returns a list of Owner's nodes on Host with pending
%% subscriptions.</p>
-spec(get_pending_nodes/2 ::
(
  Host  :: mod_pubsub:hostPubsub(),
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
		 _ -> nodetree_tree_odbc
	       end,
    Reply = mnesia:foldl(fun (#pubsub_state{stateid =
						{_, NID}} =
				  S,
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

get_nodes_helper(NodeTree,
		 #pubsub_state{stateid = {_, N},
			       subscriptions = Subs}) ->
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
-spec(get_states/1 ::
(
  NodeIdx::mod_pubsub:nodeIdx())
    -> {result, [mod_pubsub:pubsubState()]}
).
get_states(NodeIdx) ->
    case catch
	   ejabberd_odbc:sql_query_t([<<"select jid, affiliation, subscriptions "
					"from pubsub_state where nodeid='">>,
				      NodeIdx, <<"';">>])
	of
      {selected,
       [<<"jid">>, <<"affiliation">>, <<"subscriptions">>],
       RItems} ->
	  {result,
	   lists:map(fun ([SJID, Affiliation, Subscriptions]) ->
			     #pubsub_state{stateid = {decode_jid(SJID), NodeIdx},
					   items = itemids(NodeIdx, SJID),
					   affiliation = decode_affiliation(Affiliation),
					   subscriptions = decode_subscriptions(Subscriptions)}
		     end,
		     RItems)};
      _ -> {result, []}
    end.

%% @spec (NodeId, JID) -> [State] | []
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 JID = mod_pubsub:jid()
%%	 State = mod_pubsub:pubsubItems()
%% @doc <p>Returns a state (one state list), given its reference.</p>

-spec(get_state/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  JID     :: ljid())
    -> mod_pubsub:pubsubState()
).
get_state(NodeIdx, JID) ->
    State = get_state_without_itemids(NodeIdx, JID),
    {SJID, _} = State#pubsub_state.stateid,
    State#pubsub_state{items = itemids(NodeIdx, SJID)}.

-spec(get_state_without_itemids/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  JID     :: jid())
    -> mod_pubsub:pubsubState()
).
get_state_without_itemids(NodeIdx, JID) ->
    J = encode_jid(JID),
    case catch
	   ejabberd_odbc:sql_query_t([<<"select jid, affiliation, subscriptions "
					"from pubsub_state where jid='">>,
				      J, <<"' and nodeid='">>, NodeIdx,
				      <<"';">>])
	of
      {selected,
       [<<"jid">>, <<"affiliation">>, <<"subscriptions">>],
       [[SJID, Affiliation, Subscriptions]]} ->
	  #pubsub_state{stateid = {decode_jid(SJID), NodeIdx},
			affiliation = decode_affiliation(Affiliation),
			subscriptions = decode_subscriptions(Subscriptions)};
      _ -> #pubsub_state{stateid = {JID, NodeIdx}}
    end.

%% @spec (State) -> ok | {error, Reason::stanzaError()}
%%	 State = mod_pubsub:pubsubStates()
%% @doc <p>Write a state into database.</p>

-spec(set_state/1 ::
(
  State :: mod_pubsub:pubsubState())
    -> {result, []}
).
set_state(State) ->
    {_, NodeIdx} = State#pubsub_state.stateid,
    set_state(NodeIdx, State).

-spec(set_state/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  State   :: mod_pubsub:pubsubState())
    -> {result, []}
).
set_state(NodeIdx, State) ->
    {JID, _} = State#pubsub_state.stateid,
    J = encode_jid(JID),
    S = encode_subscriptions(State#pubsub_state.subscriptions),
    A = encode_affiliation(State#pubsub_state.affiliation),
    case catch
	   ejabberd_odbc:sql_query_t([<<"update pubsub_state set subscriptions='">>,
				      S, <<"', affiliation='">>, A,
				      <<"' where nodeid='">>, NodeIdx,
				      <<"' and jid='">>, J, <<"';">>])
	of
      {updated, 1} -> ok;
      _ ->
	  catch
	    ejabberd_odbc:sql_query_t([<<"insert into pubsub_state(nodeid, jid, "
					 "affiliation, subscriptions) values('">>,
				       NodeIdx, <<"', '">>, J, <<"', '">>, A,
				       <<"', '">>, S, <<"');">>])
    end,
    {result, []}.

%% @spec (NodeId, JID) -> ok | {error, Reason::stanzaError()}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 JID = mod_pubsub:jid()
%% @doc <p>Delete a state from database.</p>
del_state(NodeId, JID) ->
    J = encode_jid(JID),
    catch
      ejabberd_odbc:sql_query_t([<<"delete from pubsub_state where jid='">>,
				 J, <<"' and nodeid='">>, NodeId, <<"';">>]),
    ok.

%% @spec (NodeId, From) -> {[Items],RsmOut} | []
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
    case catch
	   ejabberd_odbc:sql_query_t([<<"select itemid, publisher, creation, "
					"modification, payload from pubsub_item "
					"where nodeid='">>,
				      NodeId,
				      <<"' order by modification desc;">>])
	of
      {selected,
       [<<"itemid">>, <<"publisher">>, <<"creation">>,
	<<"modification">>, <<"payload">>],
       RItems} ->
	  {result,
	   lists:map(fun (RItem) -> raw_to_item(NodeId, RItem) end,
		     RItems)};
      _ -> {result, []}
    end.

get_items(NodeId, From, none) ->
    MaxItems = case catch
		      ejabberd_odbc:sql_query_t([<<"select val from pubsub_node_option where "
						   "nodeid='">>,
						 NodeId,
						 <<"' and name='max_items';">>])
		   of
		 {selected, [<<"val">>], [[Value]]} ->
		     Tokens = element(2,
				erl_scan:string(binary_to_list(<<Value/binary, ".">>))),
		     element(2, erl_parse:parse_term(Tokens));
		 _ -> ?MAXITEMS
	       end,
    get_items(NodeId, From, #rsm_in{max = MaxItems});
get_items(NodeId, _From,
	  #rsm_in{max = M, direction = Direction, id = I,
		  index = IncIndex}) ->
    Max = (?PUBSUB):escape(jlib:i2l(M)),
    {Way, Order} = case Direction of
		     aft -> {<<"<">>, <<"desc">>};
		     before when I == <<>> -> {<<"is not">>, <<"asc">>};
		     before -> {<<">">>, <<"asc">>};
		     _ when IncIndex =/= undefined ->
			 {<<"<">>, <<"desc">>}; % using index
		     _ -> {<<"is not">>, <<"desc">>}% Can be better
		   end,
    [AttrName, Id] = case I of
		       undefined when IncIndex =/= undefined ->
			   case catch
				  ejabberd_odbc:sql_query_t([<<"select modification from pubsub_item "
							       "pi where exists ( select count(*) as "
							       "count1 from pubsub_item where nodeid='">>,
							     NodeId,
							     <<"' and modification > pi.modification "
							       "having count1 = ">>,
							     (?PUBSUB):escape(jlib:i2l(IncIndex)),
							     <<" );">>])
			       of
			     {selected, [_], [[O]]} ->
				 [<<"modification">>, <<"'", O/binary, "'">>];
			     _ -> [<<"modification">>, <<"null">>]
			   end;
		       undefined -> [<<"modification">>, <<"null">>];
		       <<>> -> [<<"modification">>, <<"null">>];
		       I ->
			   [A, B] = str:tokens((?PUBSUB):escape(jlib:i2l(I)),
					       <<"@">>),
			   [A, <<"'", B/binary, "'">>]
		     end,
    Count = case catch
		   ejabberd_odbc:sql_query_t([<<"select count(*) from pubsub_item where "
						"nodeid='">>,
					      NodeId, <<"';">>])
		of
	      {selected, [_], [[C]]} -> C;
	      _ -> <<"0">>
	    end,
    case catch
	   ejabberd_odbc:sql_query_t([<<"select itemid, publisher, creation, "
					"modification, payload from pubsub_item "
					"where nodeid='">>,
				      NodeId, <<"' and ">>, AttrName, <<" ">>,
				      Way, <<" ">>, Id, <<" order by ">>,
				      AttrName, <<" ">>, Order, <<" limit ">>,
				      jlib:i2l(Max), <<" ;">>])
	of
      {selected,
       [<<"itemid">>, <<"publisher">>, <<"creation">>,
	<<"modification">>, <<"payload">>],
       RItems} ->
	  case RItems of
	    [[_, _, _, F, _]|_] ->
		Index = case catch
			       ejabberd_odbc:sql_query_t([<<"select count(*) from pubsub_item where "
							    "nodeid='">>,
							  NodeId, <<"' and ">>,
							  AttrName, <<" > '">>,
							  F, <<"';">>])
			    of
			  %{selected, [_], [{C}, {In}]} -> [string:strip(C, both, $"), string:strip(In, both, $")];
			  {selected, [_], [[In]]} -> In;
			  _ -> <<"0">>
			end,
		[_, _, _, L, _] = lists:last(RItems),
		RsmOut = #rsm_out{count = Count, index = Index,
				  first = <<"modification@", F/binary>>,
				  last = <<"modification@", (jlib:i2l(L))/binary>>},
		{result, {[raw_to_item(NodeId, RItem) || RItem <- RItems], RsmOut}};
	    [] -> {result, {[], #rsm_out{count = Count}}};
	    0 -> {result, {[], #rsm_out{count = Count}}}
	  end;
      _ -> {result, {[], none}}
    end.

get_items(NodeId, JID, AccessModel,
	  PresenceSubscription, RosterGroup, SubId) ->
    get_items(NodeId, JID, AccessModel,
	      PresenceSubscription, RosterGroup, SubId, none).

get_items(NodeId, JID, AccessModel,
	  PresenceSubscription, RosterGroup, _SubId, RSM) ->
    SubKey = jlib:jid_tolower(JID),
    GenKey = jlib:jid_remove_resource(SubKey),
    {Affiliation, Subscriptions} =
	select_affiliation_subscriptions(NodeId, GenKey,
					 SubKey),
    Whitelisted = can_fetch_item(Affiliation,
				 Subscriptions),
    if %%SubId == "", ?? ->
       %% Entity has multiple subscriptions to the node but does not specify a subscription ID
       %{error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
       %%InvalidSubId ->
       %% Entity is subscribed but specifies an invalid subscription ID
       %{error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
       Affiliation == outcast -> {error, ?ERR_FORBIDDEN};
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
       true -> get_items(NodeId, JID, RSM)
    end.

get_last_items(NodeId, _From, Count) ->
    case catch
	   ejabberd_odbc:sql_query_t([<<"select itemid, publisher, creation, "
					"modification, payload from pubsub_item "
					"where nodeid='">>,
				      NodeId,
				      <<"' order by modification desc limit ">>,
				      jlib:i2l(Count), <<";">>])
	of
      {selected,
       [<<"itemid">>, <<"publisher">>, <<"creation">>,
	<<"modification">>, <<"payload">>],
       RItems} ->
	  {result,
	   lists:map(fun (RItem) -> raw_to_item(NodeId, RItem) end,
		     RItems)};
      _ -> {result, []}
    end.

%% @spec (NodeId, ItemId) -> [Item] | []
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 ItemId = string()
%%	 Item = mod_pubsub:pubsubItems()
%% @doc <p>Returns an item (one item list), given its reference.</p>
get_item(NodeId, ItemId) ->
    I = (?PUBSUB):escape(ItemId),
    case catch
	   ejabberd_odbc:sql_query_t([<<"select itemid, publisher, creation, "
					"modification, payload from pubsub_item "
					"where nodeid='">>,
				      NodeId, <<"' and itemid='">>, I,
				      <<"';">>])
	of
      {selected,
       [<<"itemid">>, <<"publisher">>, <<"creation">>,
	<<"modification">>, <<"payload">>],
       [RItem]} ->
	  {result, raw_to_item(NodeId, RItem)};
      _ -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

get_item(NodeId, ItemId, JID, AccessModel,
	 PresenceSubscription, RosterGroup, _SubId) ->
    SubKey = jlib:jid_tolower(JID),
    GenKey = jlib:jid_remove_resource(SubKey),
    {Affiliation, Subscriptions} =
	select_affiliation_subscriptions(NodeId, GenKey,
					 SubKey),
    Whitelisted = can_fetch_item(Affiliation,
				 Subscriptions),
    if %%SubId == "", ?? ->
       %% Entity has multiple subscriptions to the node but does not specify a subscription ID
       %{error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
       %%InvalidSubId ->
       %% Entity is subscribed but specifies an invalid subscription ID
       %{error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
       Affiliation == outcast -> {error, ?ERR_FORBIDDEN};
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
       true -> get_item(NodeId, ItemId)
    end.

%% @spec (Item) -> ok | {error, Reason::stanzaError()}
%%	 Item = mod_pubsub:pubsubItems()
%% @doc <p>Write an item into database.</p>
set_item(Item) ->
    {ItemId, NodeId} = Item#pubsub_item.itemid,
    I = (?PUBSUB):escape(ItemId),
    {C, _} = Item#pubsub_item.creation,
    {M, JID} = Item#pubsub_item.modification,
    P = encode_jid(JID),
    Payload = Item#pubsub_item.payload,
    XML = (?PUBSUB):escape(str:join([xml:element_to_binary(X) || X<-Payload], <<>>)),
    S = fun ({T1, T2, T3}) ->
		str:join([jlib:i2l(T1, 6), jlib:i2l(T2, 6), jlib:i2l(T3, 6)], <<":">>)
	end,
    case catch
	   ejabberd_odbc:sql_query_t([<<"update pubsub_item set publisher='">>,
				      P, <<"', modification='">>, S(M),
				      <<"', payload='">>, XML,
				      <<"' where nodeid='">>, NodeId,
				      <<"' and itemid='">>, I, <<"';">>])
	of
      {updated, 1} -> ok;
      _ ->
	  catch
	    ejabberd_odbc:sql_query_t([<<"insert into pubsub_item (nodeid, itemid, "
					 "publisher, creation, modification, payload) "
					 "values('">>,
				       NodeId, <<"', '">>, I, <<"', '">>, P,
				       <<"', '">>, S(C), <<"', '">>, S(M),
				       <<"', '">>, XML, <<"');">>])
    end,
    {result, []}.

%% @spec (NodeId, ItemId) -> ok | {error, Reason::stanzaError()}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 ItemId = string()
%% @doc <p>Delete an item from database.</p>
del_item(NodeId, ItemId) ->
    I = (?PUBSUB):escape(ItemId),
    catch
      ejabberd_odbc:sql_query_t([<<"delete from pubsub_item where itemid='">>,
				 I, <<"' and nodeid='">>, NodeId, <<"';">>]).

del_items(_, []) -> ok;
del_items(NodeId, [ItemId]) -> del_item(NodeId, ItemId);
del_items(NodeId, ItemIds) ->
    I = str:join([[<<"'">>, (?PUBSUB):escape(X), <<"'">>]
		  || X <- ItemIds],
		 <<",">>),
    catch
      ejabberd_odbc:sql_query_t([<<"delete from pubsub_item where itemid "
				   "in (">>,
				 I, <<") and nodeid='">>, NodeId, <<"';">>]).

get_item_name(_Host, _Node, Id) -> Id.

node_to_path(Node) -> str:tokens((Node), <<"/">>).

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
can_fetch_item(none, Subscriptions) -> is_subscribed(Subscriptions).

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

itemids(NodeId, {U, S, R}) ->
    itemids(NodeId, encode_jid({U, S, R}));
itemids(NodeId, SJID) ->
    case catch
	   ejabberd_odbc:sql_query_t([<<"select itemid from pubsub_item where "
					"nodeid='">>,
				      NodeId, <<"' and publisher like '">>,
				      SJID,
				      <<"%' order by modification desc;">>])
	of
      {selected, [<<"itemid">>], RItems} ->
	  lists:map(fun ([ItemId]) -> ItemId end, RItems);
      _ -> []
    end.

select_affiliation_subscriptions(NodeId, JID) ->
    J = encode_jid(JID),
    case catch
	   ejabberd_odbc:sql_query_t([<<"select affiliation,subscriptions from "
					"pubsub_state where nodeid='">>,
				      NodeId, <<"' and jid='">>, J, <<"';">>])
	of
      {selected, [<<"affiliation">>, <<"subscriptions">>],
       [[A, S]]} ->
	  {decode_affiliation(A), decode_subscriptions(S)};
      _ -> {none, []}
    end.

select_affiliation_subscriptions(NodeId, JID, JID) ->
    select_affiliation_subscriptions(NodeId, JID);
select_affiliation_subscriptions(NodeId, GenKey,
				 SubKey) ->
    {result, Affiliation} = get_affiliation(NodeId, GenKey),
    {result, Subscriptions} = get_subscriptions(NodeId,
						SubKey),
    {Affiliation, Subscriptions}.

update_affiliation(NodeId, JID, Affiliation) ->
    J = encode_jid(JID),
    A = encode_affiliation(Affiliation),
    case catch
	   ejabberd_odbc:sql_query_t([<<"update pubsub_state set affiliation='">>,
				      A, <<"' where nodeid='">>, NodeId,
				      <<"' and jid='">>, J, <<"';">>])
	of
      {updated, 1} -> ok;
      _ ->
	  catch
	    ejabberd_odbc:sql_query_t([<<"insert into pubsub_state(nodeid, jid, "
					 "affiliation, subscriptions) values('">>,
				       NodeId, <<"', '">>, J, <<"', '">>, A,
				       <<"', '');">>])
    end.

update_subscription(NodeId, JID, Subscription) ->
    J = encode_jid(JID),
    S = encode_subscriptions(Subscription),
    case catch
	   ejabberd_odbc:sql_query_t([<<"update pubsub_state set subscriptions='">>,
				      S, <<"' where nodeid='">>, NodeId,
				      <<"' and jid='">>, J, <<"';">>])
	of
      {updated, 1} -> ok;
      _ ->
	  catch
	    ejabberd_odbc:sql_query_t([<<"insert into pubsub_state(nodeid, jid, "
					 "affiliation, subscriptions) values('">>,
				       NodeId, <<"', '">>, J, <<"', 'n', '">>,
				       S, <<"');">>])
    end.

decode_jid(JID) ->
    jlib:jid_tolower(jlib:string_to_jid(JID)).

decode_affiliation(<<"o">>) -> owner;
decode_affiliation(<<"p">>) -> publisher;
decode_affiliation(<<"m">>) -> member;
decode_affiliation(<<"c">>) -> outcast;
decode_affiliation(_) -> none.

decode_subscription(<<"s">>) -> subscribed;
decode_subscription(<<"p">>) -> pending;
decode_subscription(<<"u">>) -> unconfigured;
decode_subscription(_) -> none.

decode_subscriptions(Subscriptions) ->
    lists:foldl(fun (Subscription, Acc) ->
			case str:tokens(Subscription, <<":">>) of
			  [S, SubId] -> [{decode_subscription(S), SubId} | Acc];
			  _ -> Acc
			end
		end,
		[], str:tokens(Subscriptions, <<",">>)).

encode_jid(JID) ->
    (?PUBSUB):escape(JID).

encode_affiliation(owner) -> <<"o">>;
encode_affiliation(publisher) -> <<"p">>;
encode_affiliation(member) -> <<"m">>;
encode_affiliation(outcast) -> <<"c">>;
encode_affiliation(_) -> <<"n">>.

encode_subscription(subscribed) -> <<"s">>;
encode_subscription(pending) -> <<"p">>;
encode_subscription(unconfigured) -> <<"u">>;
encode_subscription(_) -> <<"n">>.

encode_subscriptions(Subscriptions) ->
    str:join(lists:map(fun ({S, SubId}) ->
			       <<(encode_subscription(S))/binary, ":",
				 SubId/binary>>
		       end,
		       Subscriptions),
	     <<",">>).

%%% record getter/setter

state_to_raw(NodeId, State) ->
    {JID, _} = State#pubsub_state.stateid,
    J = encode_jid(JID),
    A = encode_affiliation(State#pubsub_state.affiliation),
    S =
	encode_subscriptions(State#pubsub_state.subscriptions),
    [<<"'">>, NodeId, <<"', '">>, J, <<"', '">>, A,
     <<"', '">>, S, <<"'">>].

raw_to_item(NodeId,
	    [ItemId, SJID, Creation, Modification, XML]) ->
    JID = decode_jid(SJID),
    ToTime = fun (Str) ->
		     [T1, T2, T3] = str:tokens(Str, <<":">>),
		     {jlib:l2i(T1), jlib:l2i(T2), jlib:l2i(T3)}
	     end,
    Payload = case xml_stream:parse_element(XML) of
		{error, _Reason} -> [];
		El -> [El]
	      end,
    #pubsub_item{itemid = {ItemId, NodeId},
		 creation = {ToTime(Creation), JID},
		 modification = {ToTime(Modification), JID},
		 payload = Payload}.
