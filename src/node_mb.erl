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
    node_pep:init(Host, ServerHost, Opts).

terminate(Host, ServerHost) ->
    node_pep:terminate(Host, ServerHost), ok.

options() ->
    [{deliver_payloads, true}, {notify_config, false},
     {notify_delete, false}, {notify_retract, false},
     {purge_offline, false}, {persist_items, true},
     {max_items, ?MAXITEMS}, {subscribe, true},
     {access_model, presence}, {roster_groups_allowed, []},
     {publish_model, publishers},
     {notification_type, headline},
     {max_payload_size, ?MAX_PAYLOAD_SIZE},
     {send_last_published_item, on_sub_and_presence},
     {deliver_notifications, true},
     {presence_based_delivery, true}].

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

create_node_permission(Host, ServerHost, Node,
		       ParentNode, Owner, Access) ->
    node_pep:create_node_permission(Host, ServerHost, Node,
				    ParentNode, Owner, Access).

create_node(NodeId, Owner) ->
    node_pep:create_node(NodeId, Owner).

delete_node(Removed) -> node_pep:delete_node(Removed).

subscribe_node(NodeId, Sender, Subscriber, AccessModel,
	       SendLast, PresenceSubscription, RosterGroup, Options) ->
    node_pep:subscribe_node(NodeId, Sender, Subscriber,
			    AccessModel, SendLast, PresenceSubscription,
			    RosterGroup, Options).

unsubscribe_node(NodeId, Sender, Subscriber, SubID) ->
    node_pep:unsubscribe_node(NodeId, Sender, Subscriber,
			      SubID).

publish_item(NodeId, Publisher, Model, MaxItems, ItemId,
	     Payload) ->
    node_pep:publish_item(NodeId, Publisher, Model,
			  MaxItems, ItemId, Payload).

remove_extra_items(NodeId, MaxItems, ItemIds) ->
    node_pep:remove_extra_items(NodeId, MaxItems, ItemIds).

delete_item(NodeId, Publisher, PublishModel, ItemId) ->
    node_pep:delete_item(NodeId, Publisher, PublishModel,
			 ItemId).

purge_node(NodeId, Owner) ->
    node_pep:purge_node(NodeId, Owner).

get_entity_affiliations(Host, Owner) ->
    node_pep:get_entity_affiliations(Host, Owner).

get_node_affiliations(NodeId) ->
    node_pep:get_node_affiliations(NodeId).

get_affiliation(NodeId, Owner) ->
    node_pep:get_affiliation(NodeId, Owner).

set_affiliation(NodeId, Owner, Affiliation) ->
    node_pep:set_affiliation(NodeId, Owner, Affiliation).

get_entity_subscriptions(Host, Owner) ->
    node_pep:get_entity_subscriptions(Host, Owner).

get_node_subscriptions(NodeId) ->
    node_pep:get_node_subscriptions(NodeId).

get_subscriptions(NodeId, Owner) ->
    node_pep:get_subscriptions(NodeId, Owner).

set_subscriptions(NodeId, Owner, Subscription, SubId) ->
    node_pep:set_subscriptions(NodeId, Owner, Subscription,
			       SubId).

get_pending_nodes(Host, Owner) ->
    node_hometree:get_pending_nodes(Host, Owner).

get_states(NodeId) -> node_pep:get_states(NodeId).

get_state(NodeId, JID) ->
    node_pep:get_state(NodeId, JID).

set_state(State) -> node_pep:set_state(State).

get_items(NodeId, From) ->
    node_pep:get_items(NodeId, From).

get_items(NodeId, JID, AccessModel,
	  PresenceSubscription, RosterGroup, SubId) ->
    node_pep:get_items(NodeId, JID, AccessModel,
		       PresenceSubscription, RosterGroup, SubId).

get_item(NodeId, ItemId) ->
    node_pep:get_item(NodeId, ItemId).

get_item(NodeId, ItemId, JID, AccessModel,
	 PresenceSubscription, RosterGroup, SubId) ->
    node_pep:get_item(NodeId, ItemId, JID, AccessModel,
		      PresenceSubscription, RosterGroup, SubId).

set_item(Item) -> node_pep:set_item(Item).

get_item_name(Host, Node, Id) ->
    node_pep:get_item_name(Host, Node, Id).

node_to_path(Node) -> node_pep:node_to_path(Node).

path_to_node(Path) -> node_pep:path_to_node(Path).
