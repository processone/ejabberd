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
%%% Portions created by ProcessOne are Copyright 2006-2013, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2013, ProcessOne.
%%%
%%%
%%% @copyright 2006-2013 ProcessOne
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

-export([decode_jid/1, decode_node/1,
	 decode_affiliation/1, decode_subscriptions/1,
	 encode_jid/1, encode_affiliation/1,
	 encode_subscriptions/1]).

%% ================
%% API definition
%% ================

init(_Host, _ServerHost, _Opts) ->
    pubsub_subscription_odbc:init(), ok.

terminate(_Host, _ServerHost) -> ok.

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
						    lists:map(fun ({SJID,
								    Subscriptions}) ->
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
		    [] -> false;
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

remove_extra_items(_NodeId, unlimited, ItemIds) ->
    {result, {ItemIds, []}};
remove_extra_items(NodeId, MaxItems, ItemIds) ->
    NewItems = lists:sublist(ItemIds, MaxItems),
    OldItems = lists:nthtail(length(NewItems), ItemIds),
    del_items(NodeId, OldItems),
    {result, {NewItems, OldItems}}.

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
		  lists:map(fun ({N, T, I, A}) ->
				    Node = nodetree_tree_odbc:raw_to_node(Host,
									  {N,
									   <<"">>,
									   T,
									   I}),
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
		  lists:map(fun ({J, A}) ->
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
	      {selected, [<<"affiliation">>], [{A}]} ->
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
		  lists:foldl(fun ({N, T, I, J, S}, Acc) ->
				      Node =
					  nodetree_tree_odbc:raw_to_node(Host,
									 {N,
									  <<"">>,
									  T,
									  I}),
				      Jid = decode_jid(J),
				      case decode_subscriptions(S) of
					[] -> [{Node, none, Jid} | Acc];
					Subs ->
					    lists:foldl(fun ({Sub, SubId},
							     Acc2) ->
								[{Node, Sub,
								  SubId, Jid}
								 | Acc2];
							    (Sub, Acc2) ->
								[{Node, Sub,
								  Jid}
								 | Acc2]
							end,
							Acc, Subs)
				      end
			      end,
			      [], RItems);
	      _ -> []
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
		  lists:foldl(fun ({N, T, I, J, S}, Acc) ->
				      Node =
					  nodetree_tree_odbc:raw_to_node(Host,
									 {N,
									  <<"">>,
									  T,
									  I}),
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
		  lists:foldl(fun ({J, S}, Acc) ->
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
	      {selected, [<<"subscriptions">>], [{S}]} ->
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
	   lists:map(fun ({SJID, Affiliation, Subscriptions}) ->
			     #pubsub_state{stateid = {decode_jid(SJID), NodeIdx},
					   items = itemids(NodeIdx, SJID),
					   affiliation = decode_affiliation(Affiliation),
					   subscriptions = decode_subscriptions(Subscriptions)}
		     end,
		     RItems)};
      _ -> {result, []}
    end.


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
       [{SJID, Affiliation, Subscriptions}]} ->
	  #pubsub_state{stateid = {decode_jid(SJID), NodeIdx},
			affiliation = decode_affiliation(Affiliation),
			subscriptions = decode_subscriptions(Subscriptions)};
      _ -> #pubsub_state{stateid = {JID, NodeIdx}}
    end.


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

del_state(NodeId, JID) ->
    J = encode_jid(JID),
    catch
      ejabberd_odbc:sql_query_t([<<"delete from pubsub_state where jid='">>,
				 J, <<"' and nodeid='">>, NodeId, <<"';">>]),
    ok.

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
		 {selected, [<<"val">>], [{Value}]} ->
		     Tokens = element(2,
				      erl_scan:string(<<Value/binary, ".">>)),
		     element(2, erl_parse:parse_term(Tokens));
		 _ -> ?MAXITEMS
	       end,
    get_items(NodeId, From, #rsm_in{max = MaxItems});
get_items(NodeId, _From,
	  #rsm_in{max = M, direction = Direction, id = I,
		  index = IncIndex}) ->
    Max = (?PUBSUB):escape(i2l(M)),
    {Way, Order} = case Direction of
		     aft -> {<<"<">>, <<"desc">>};
		     before when I == [] -> {<<"is not">>, <<"asc">>};
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
							     (?PUBSUB):escape(i2l(IncIndex)),
							     <<" );">>])
			       of
			     {selected, [_], [{O}]} ->
				 [<<"modification">>, <<"'", O/binary, "'">>];
			     _ -> [<<"modification">>, <<"null">>]
			   end;
		       undefined -> [<<"modification">>, <<"null">>];
		       [] -> [<<"modification">>, <<"null">>];
		       I ->
			   [A, B] = str:tokens((?PUBSUB):escape(i2l(I)),
					       <<"@">>),
			   [A, <<"'", B/binary, "'">>]
		     end,
    Count = case catch
		   ejabberd_odbc:sql_query_t([<<"select count(*) from pubsub_item where "
						"nodeid='">>,
					      NodeId, <<"';">>])
		of
	      {selected, [_], [{C}]} -> C;
	      _ -> <<"0">>
	    end,
    case catch
	   ejabberd_odbc:sql_query_t([<<"select itemid, publisher, creation, "
					"modification, payload from pubsub_item "
					"where nodeid='">>,
				      NodeId, <<"' and ">>, AttrName, <<" ">>,
				      Way, <<" ">>, Id, <<" order by ">>,
				      AttrName, <<" ">>, Order, <<" limit ">>,
				      i2l(Max), <<" ;">>])
	of
      {selected,
       [<<"itemid">>, <<"publisher">>, <<"creation">>,
	<<"modification">>, <<"payload">>],
       RItems} ->
	  case str:len(RItems) of
	    0 -> {result, {[], #rsm_out{count = Count}}};
	    _ ->
		{_, _, _, F, _} = hd(RItems),
		Index = case catch
			       ejabberd_odbc:sql_query_t([<<"select count(*) from pubsub_item where "
							    "nodeid='">>,
							  NodeId, <<"' and ">>,
							  AttrName, <<" > '">>,
							  F, <<"';">>])
			    of
			  %{selected, [_], [{C}, {In}]} -> [string:strip(C, both, $"), string:strip(In, both, $")];
			  {selected, [_], [{In}]} -> In;
			  _ -> <<"0">>
			end,
		{_, _, _, L, _} = lists:last(RItems),
		RsmOut = #rsm_out{count = Count, index = Index,
				  first = <<"modification@", F/binary>>,
				  last = <<"modification@", (i2l(L))/binary>>},
		{result,
		 {lists:map(fun (RItem) -> raw_to_item(NodeId, RItem)
			    end,
			    RItems),
		  RsmOut}}
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
				      i2l(Count), <<";">>])
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

set_item(Item) ->
    {ItemId, NodeId} = Item#pubsub_item.itemid,
    I = (?PUBSUB):escape(ItemId),
    {C, _} = Item#pubsub_item.creation,
    {M, JID} = Item#pubsub_item.modification,
    P = encode_jid(JID),
    Payload = Item#pubsub_item.payload,
    XML = (?PUBSUB):escape(lists:flatten(lists:map(fun
						     (X) ->
							 xml:element_to_binary(X)
						   end,
						   Payload))),
    S = fun ({T1, T2, T3}) ->
		lists:flatten([i2l(T1, 6), <<":">>, i2l(T2, 6), <<":">>,
			       i2l(T3, 6)])
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

can_fetch_item(owner, _) -> true;
can_fetch_item(member, _) -> true;
can_fetch_item(publisher, _) -> true;
can_fetch_item(outcast, _) -> false;
can_fetch_item(none, Subscriptions) ->
    is_subscribed(Subscriptions);
can_fetch_item(_Affiliation, _Subscription) -> false.

is_subscribed(Subscriptions) ->
    lists:any(fun ({subscribed, _SubId}) -> true;
		  (_) -> false
	      end,
	      Subscriptions).

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
	  lists:map(fun ({ItemId}) -> ItemId end, RItems);
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
       [{A, S}]} ->
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

decode_jid(SJID) ->
    jlib:jid_tolower(jlib:string_to_jid(SJID)).

decode_node(N) -> (?PUBSUB):string_to_node(N).

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

%-spec(encode_jid/1 ::
%(
%  JID :: jid() | jid())
%    -> binary()
%).
encode_jid(JID) ->
    (?PUBSUB):escape(jlib:jid_to_string(JID)).

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
	    {ItemId, SJID, Creation, Modification, XML}) ->
    JID = decode_jid(SJID),
    ToTime = fun (Str) ->
		     [T1, T2, T3] = str:tokens(Str, <<":">>),
		     {l2i(T1), l2i(T2), l2i(T3)}
	     end,
    Payload = case xml_stream:parse_element(XML) of
		{error, _Reason} -> [];
		El -> [El]
	      end,
    #pubsub_item{itemid = {ItemId, NodeId},
		 creation = {ToTime(Creation), JID},
		 modification = {ToTime(Modification), JID},
		 payload = Payload}.

l2i(L) when is_binary(L) -> jlib:binary_to_integer(L);
l2i(I) when is_integer(I) -> I.

i2l(I) when is_integer(I) ->
    iolist_to_binary(integer_to_list(I));
i2l(L) when is_binary(L) -> L.

i2l(I, N) when is_integer(I) -> i2l(i2l(I), N);
i2l(L, N) when is_binary(L) ->
    case str:len(L) of
      N -> L;
      C when C > N -> L;
      _ -> i2l(<<$0, L/binary>>, N)
    end.
