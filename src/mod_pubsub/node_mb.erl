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
%%% @author Eric Cestari <eric@ohmforce.com>
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================


%%% @doc The module <strong>{@module}</strong> is the pep microblog PubSub plugin.
%%% <p> To be used, mod_pubsub must be configured :
%%% {mod_pubsub,   [ % requires mod_caps
%%%   	  {access_createnode, pubsub_createnode},
%%%   	  {plugins, ["default", "pep","mb"]},
%%%	  {pep_mapping, [{"urn:xmpp:microblog", "mb"}]}
%%%   	 ]},
%%% </p>
%%% <p>PubSub plugin nodes are using the {@link gen_pubsub_node} behaviour.</p>

-module(node_mb).
-author('eric@ohmforce.com').

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("pubsub.hrl").

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

init(Host, ServerHost, Opts) ->
    node_pep:init(Host, ServerHost, Opts).

terminate(Host, ServerHost) ->
    node_pep:terminate(Host, ServerHost),
    ok.

options() ->
    [{node_type, pep},
     {deliver_payloads, true},
     {notify_config, false},
     {notify_delete, false},
     {notify_retract, false},
     {persist_items, true},
     {max_items, ?MAXITEMS},
     {subscribe, true},
     {access_model, presence},
     {roster_groups_allowed, []},
     {publish_model, publishers},
     {max_payload_size, ?MAX_PAYLOAD_SIZE},
     {send_last_published_item, on_sub_and_presence},
     {deliver_notifications, true},
     {presence_based_delivery, true}].

features() ->
    ["create-nodes", %*
     "auto-create", %*
     "auto-subscribe", %*
     "delete-nodes", %*
     "delete-any", %*
     "filtered-notifications", %*
     "modify-affiliations",
     "outcast-affiliation",
     "persistent-items",
     "publish", %*
     "purge-nodes",
     "retract-items",
     "retrieve-affiliations",
     "retrieve-items", %*
     "retrieve-subscriptions",
     "subscribe" %*
    ].

create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
    node_pep:create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access).
    
create_node(Host, Node, Owner) ->
	 node_pep:create_node(Host, Node, Owner).
	
delete_node(Host, Removed) ->
    node_pep:delete_node(Host, Removed).

subscribe_node(Host, Node, Sender, Subscriber, AccessModel,
	       SendLast, PresenceSubscription, RosterGroup) ->
    node_pep:subscribe_node(
      Host, Node, Sender, Subscriber, AccessModel, SendLast,
      PresenceSubscription, RosterGroup).

unsubscribe_node(Host, Node, Sender, Subscriber, SubID) ->
    node_pep:unsubscribe_node(Host, Node, Sender, Subscriber, SubID).

publish_item(Host, Node, Publisher, Model, MaxItems, ItemId, Payload) ->
    node_pep:publish_item(Host, Node, Publisher, Model, MaxItems, ItemId, Payload).

remove_extra_items(Host, Node, MaxItems, ItemIds) ->
    node_pep:remove_extra_items(Host, Node, MaxItems, ItemIds).

delete_item(Host, Node, JID, ItemId) ->
    node_pep:delete_item(Host, Node, JID, ItemId).

purge_node(Host, Node, Owner) ->
    node_pep:purge_node(Host, Node, Owner).

get_entity_affiliations(Host, Owner) ->
    node_pep:get_entity_affiliations(Host, Owner).

get_node_affiliations(Host, Node) ->
	node_pep:get_node_affiliations(Host, Node).

get_affiliation(Host, Node, Owner) ->
    node_pep:get_affiliation(Host, Node, Owner).

set_affiliation(Host, Node, Owner, Affiliation) ->
	node_pep:set_affiliation(Host, Node, Owner, Affiliation).

get_entity_subscriptions(Host,Owner) ->
    node_pep:get_entity_subscriptions(Host, Owner).

get_node_subscriptions(Host, Node) ->
    node_pep:get_node_subscriptions(Host, Node).

get_subscription(Host,Node,Owner) ->
    node_pep:get_subscription(Host,Node,Owner).

set_subscription(Host, Node, Owner, Subscription) ->
    node_pep:set_subscription(Host, Node, Owner, Subscription).

get_states(Host, Node) ->
    node_pep:get_states(Host, Node).

get_state(Host, Node, JID) ->
    node_pep:get_state(Host, Node, JID).

set_state(State) ->
    node_pep:set_state(State).

get_items(Host, Node, From) ->
    node_pep:get_items(Host, Node, From).

get_items(Host, Node, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_pep:get_items(Host, Node, JID, AccessModel, PresenceSubscription, RosterGroup, SubId).

get_item(Host, Node, ItemId) ->
    node_pep:get_item(Host, Node, ItemId).

get_item(Host, Node, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_pep:get_item(Host, Node, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId).

set_item(Item) ->
    node_pep:set_item(Item).

get_item_name(Host, Node, Id) ->
    node_pep:get_item_name(Host, Node, Id).

