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

%%% @doc The module <strong>{@module}</strong> is the pep PubSub plugin.
%%% <p>PubSub plugin nodes are using the {@link gen_pubsub_node} behaviour.</p>

-module(node_pep).

-author('christophe.romain@process-one.net').

-include("ejabberd.hrl").
-include("logger.hrl").

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

init(Host, ServerHost, Opts) ->
    node_hometree:init(Host, ServerHost, Opts),
    complain_if_modcaps_disabled(ServerHost),
    ok.

terminate(Host, ServerHost) ->
    node_hometree:terminate(Host, ServerHost), ok.

-spec(options/0 :: () -> NodeOptions::mod_pubsub:nodeOptions()).
options() ->
    [{deliver_payloads, true}, {notify_config, false},
     {notify_delete, false}, {notify_retract, false},
     {purge_offline, false}, {persist_items, true},
     {max_items, 1}, {subscribe, true},
     {access_model, presence}, {roster_groups_allowed, []},
     {publish_model, publishers},
     {notification_type, headline},
     {max_payload_size, ?MAX_PAYLOAD_SIZE},
     {send_last_published_item, on_sub_and_presence},
     {deliver_notifications, true},
     {presence_based_delivery, true}].

-spec(features/0 :: () -> Features::[binary(),...]).
features() ->
    [<<"create-nodes">>, %*
     <<"auto-create">>, %*
     <<"auto-subscribe">>, %*
     <<"delete-nodes">>, %*
     <<"delete-items">>, %*
     <<"filtered-notifications">>, %*
     <<"modify-affiliations">>, <<"outcast-affiliation">>,
     <<"persistent-items">>,
     <<"publish">>, %*
     <<"purge-nodes">>, <<"retract-items">>,
     <<"retrieve-affiliations">>,
     <<"retrieve-items">>, %*
     <<"retrieve-subscriptions">>, <<"subscribe">>].


-spec(create_node_permission/6 ::
(
  Host          :: mod_pubsub:hostPEP(),
  ServerHost    :: binary(),
  NodeId        :: mod_pubsub:nodeId(),
  _ParentNodeId :: mod_pubsub:nodeId(),
  Owner         :: jid(),
  Access        :: atom())
    -> {result, boolean()}
).

create_node_permission(Host, ServerHost, _Node, _ParentNode, Owner, Access) ->
    LOwner = jlib:jid_tolower(Owner),
    {User, Server, _Resource} = LOwner,
    Allowed = case LOwner of
		{<<"">>, Host, <<"">>} ->
		    true; % pubsub service always allowed
		_ ->
		    case acl:match_rule(ServerHost, Access, LOwner) of
		      allow ->
			  case Host of
			    {User, Server, _} -> true;
			    _ -> false
			  end;
		      E -> ?DEBUG("Create not allowed : ~p~n", [E]), false
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
    node_hometree:create_node(NodeIdx, Owner).

-spec(delete_node/1 ::
(
  Nodes :: [mod_pubsub:pubsubNode(),...])
    -> {result,
        {[],
         [{mod_pubsub:pubsubNode(),
           [{ljid(), [{mod_pubsub:subscription(), mod_pubsub:subId()}]},...]},...]
         }
        }
).

delete_node(Removed) ->
    case node_hometree:delete_node(Removed) of
      {result, {_, _, Result}} -> {result, {[], Result}};
      Error -> Error
    end.

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
subscribe_node(NodeIdx, Sender, Subscriber, AccessModel, SendLast,
  PresenceSubscription, RosterGroup, Options) ->
    node_hometree:subscribe_node(NodeIdx, Sender, Subscriber, AccessModel,
        SendLast, PresenceSubscription, RosterGroup, Options).

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
unsubscribe_node(NodeIdx, Sender, Subscriber, SubID) ->
    case node_hometree:unsubscribe_node(NodeIdx, Sender, Subscriber, SubID) of
      {error, Error} -> {error, Error};
      {result, _} -> {result, []}
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
     | {error, xmlel()}
).
publish_item(NodeIdx, Publisher, Model, MaxItems, ItemId, Payload) ->
    node_hometree:publish_item(NodeIdx, Publisher, Model, MaxItems, ItemId, Payload).

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
remove_extra_items(NodeId, MaxItems, ItemIds) ->
    node_hometree:remove_extra_items(NodeId, MaxItems, ItemIds).


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
    node_hometree:delete_item(NodeIdx, Publisher, PublishModel, ItemId).

-spec(purge_node/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  Owner   :: jid())
    -> {result, {default, broadcast}}
     | {error, xmlel()}
).
purge_node(NodeIdx, Owner) ->
    node_hometree:purge_node(NodeIdx, Owner).

-spec(get_entity_affiliations/2 ::
(
  Host  :: mod_pubsub:hostPEP(),
  Owner :: jid())
    -> {result, [{mod_pubsub:pubsubNode(), mod_pubsub:affiliation()}]}
).
get_entity_affiliations(_Host, Owner) ->
    {_, D, _} = SubKey = jlib:jid_tolower(Owner),
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
    States = mnesia:match_object(#pubsub_state{stateid =
						   {GenKey, '_'},
					       _ = '_'}),
    NodeTree = case catch
		      ets:lookup(gen_mod:get_module_proc(D, config), nodetree)
		   of
		 [{nodetree, N}] -> N;
		 _ -> nodetree_tree
	       end,
    Reply = lists:foldl(fun (#pubsub_state{stateid = {_, N},
					   affiliation = A},
			     Acc) ->
				case NodeTree:get_node(N) of
				  #pubsub_node{nodeid = {{_, D, _}, _}} =
				      Node ->
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
    node_hometree:get_node_affiliations(NodeIdx).

-spec(get_affiliation/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  Owner   :: jid())
    -> {result, mod_pubsub:affiliation()}
).
get_affiliation(NodeIdx, Owner) ->
    node_hometree:get_affiliation(NodeIdx, Owner).

-spec(set_affiliation/3 ::
(
  NodeIdx     :: mod_pubsub:nodeIdx(),
  Owner       :: ljid(),
  Affiliation :: mod_pubsub:affiliation())
    -> ok
).
set_affiliation(NodeIdx, Owner, Affiliation) ->
    node_hometree:set_affiliation(NodeIdx, Owner, Affiliation).

-spec(get_entity_subscriptions/2 ::
(
  Host :: mod_pubsub:hostPEP(),
  Owner :: jid())
    -> {result,
          [{mod_pubsub:pubsubNode(),
            mod_pubsub:subscription(),
            mod_pubsub:subId(),
            ljid()}]
       }
).
get_entity_subscriptions(_Host, Owner) ->
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
		      ets:lookup(gen_mod:get_module_proc(D, config), nodetree)
		   of
		 [{nodetree, N}] -> N;
		 _ -> nodetree_tree
	       end,
    Reply = lists:foldl(fun (#pubsub_state{stateid = {J, N},
					   subscriptions = Ss},
			     Acc) ->
				case NodeTree:get_node(N) of
				  #pubsub_node{nodeid = {{_, D, _}, _}} = Node ->
				      lists:foldl(fun
				        ({subscribed, SubID}, Acc2) ->
					        [{Node, subscribed, SubID, J} | Acc2];
						({pending, _SubID}, Acc2) ->
							[{Node, pending, J} | Acc2];
						(S, Acc2) ->
							  [{Node, S, J} | Acc2]
					    end, Acc, Ss);
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
    node_hometree:get_node_subscriptions(NodeIdx).

-spec(get_subscriptions/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  Owner   :: ljid())
    -> {result, [{mod_pubsub:subscription(), mod_pubsub:subId()}]}
).
get_subscriptions(NodeIdx, Owner) ->
    node_hometree:get_subscriptions(NodeIdx, Owner).

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
    node_hometree:set_subscriptions(NodeIdx, Owner, Subscription, SubId).

-spec(get_pending_nodes/2 ::
(
  Host  :: mod_pubsub:hostPubsub(),
  Owner :: jid())
    -> {result, [mod_pubsub:nodeId()]}
).
get_pending_nodes(Host, Owner) ->
    node_hometree:get_pending_nodes(Host, Owner).

-spec(get_states/1 ::
(
  NodeIdx::mod_pubsub:nodeIdx())
    -> {result, [mod_pubsub:pubsubState()]}
).
get_states(NodeIdx) -> node_hometree:get_states(NodeIdx).

-spec(get_state/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  JID     :: ljid())
    -> mod_pubsub:pubsubState()
).
get_state(NodeIdx, JID) ->
    node_hometree:get_state(NodeIdx, JID).

-spec(set_state/1 ::
(
  State::mod_pubsub:pubsubState())
    -> ok
).
set_state(State) -> node_hometree:set_state(State).

-spec(get_items/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  _From   :: jid())
    -> {result, [mod_pubsub:pubsubItem()]}
).
get_items(NodeIdx, From) ->
    node_hometree:get_items(NodeIdx, From).

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
get_items(NodeIdx, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_hometree:get_items(NodeIdx, JID, AccessModel, PresenceSubscription, RosterGroup, SubId).

-spec(get_item/2 ::
(
  NodeIdx :: mod_pubsub:nodeIdx(),
  ItemId  :: mod_pubsub:itemId())
    -> {result, mod_pubsub:pubsubItem()}
     | {error, xmlel()}
).
get_item(NodeIdx, ItemId) ->
    node_hometree:get_item(NodeIdx, ItemId).

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
  SubId) ->
    node_hometree:get_item(NodeIdx, ItemId, JID, AccessModel, PresenceSubscription,
        RosterGroup, SubId).

-spec(set_item/1 ::
(
  Item::mod_pubsub:pubsubItem())
    -> ok
).
set_item(Item) -> node_hometree:set_item(Item).

get_item_name(Host, Node, Id) ->
    node_hometree:get_item_name(Host, Node, Id).

node_to_path(Node) -> node_flat:node_to_path(Node).

path_to_node(Path) -> node_flat:path_to_node(Path).

%%%
%%% Internal
%%%

%% @doc Check mod_caps is enabled, otherwise show warning.
%% The PEP plugin for mod_pubsub requires mod_caps to be enabled in the host.
%% Check that the mod_caps module is enabled in that Jabber Host
%% If not, show a warning message in the ejabberd log file.
complain_if_modcaps_disabled(ServerHost) ->
    case gen_mod:is_loaded(ServerHost, mod_caps) of
      false ->
	  ?WARNING_MSG("The PEP plugin is enabled in mod_pubsub "
		       "of host ~p. This plugin requires mod_caps "
		       "to be enabled, but it isn't.",
		       [ServerHost]);
      true -> ok
    end.
