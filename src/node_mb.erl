%%%----------------------------------------------------------------------
%%% File    : node_mb.erl
%%% Author  : Eric Cestari <ecestari@process-one.net>
%%% Purpose : PEP microglobing experimentation
%%% Created : 25 Sep 2008 by Eric Cestari <ecestari@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(node_mb).
-behaviour(gen_pubsub_node).
-author('ecestari@process-one.net').

-include("pubsub.hrl").

%%% @doc The module <strong>{@module}</strong> is the pep microblog PubSub plugin.
%%% <p>To be used, mod_pubsub must be configured:<pre>
%%% mod_pubsub:
%%%   access_createnode: pubsub_createnode
%%%   ignore_pep_from_offline: false
%%%   plugins:
%%%     - "flat"
%%%     - "pep" # Requires mod_caps.
%%%     - "mb"
%%%   pep_mapping:
%%%     "urn:xmpp:microblog:0": "mb"
%%% </pre></p>
%%% <p>PubSub plugin nodes are using the {@link gen_pubsub_node} behaviour.</p>

-export([init/3, terminate/2, options/0, features/0,
    create_node_permission/6, create_node/2, delete_node/1,
    purge_node/2, subscribe_node/8, unsubscribe_node/4,
    publish_item/7, delete_item/4, remove_extra_items/3,
    get_entity_affiliations/2, get_node_affiliations/1,
    get_affiliation/2, set_affiliation/3,
    get_entity_subscriptions/2, get_node_subscriptions/1,
    get_subscriptions/2, set_subscriptions/4,
    get_pending_nodes/2, get_states/1, get_state/2,
    set_state/1, get_items/7, get_items/3, get_item/7,
    get_last_items/3,
    get_item/2, set_item/1, get_item_name/3, node_to_path/1,
    path_to_node/1]).

init(Host, ServerHost, Opts) ->
    node_pep:init(Host, ServerHost, Opts).

terminate(Host, ServerHost) ->
    node_pep:terminate(Host, ServerHost), ok.

options() ->
    [{deliver_payloads, true},
	{notify_config, false},
	{notify_delete, false},
	{notify_retract, false},
	{purge_offline, false},
	{persist_items, true},
	{max_items, ?MAXITEMS},
	{subscribe, true},
	{access_model, presence},
	{roster_groups_allowed, []},
	{publish_model, publishers},
	{notification_type, headline},
	{max_payload_size, ?MAX_PAYLOAD_SIZE},
	{send_last_published_item, on_sub_and_presence},
	{deliver_notifications, true},
	{presence_based_delivery, true},
	{itemreply, none}].

features() ->
    [<<"create-nodes">>,
	<<"auto-create">>,
	<<"auto-subscribe">>,
	<<"delete-nodes">>,
	<<"delete-items">>,
	<<"filtered-notifications">>,
	<<"modify-affiliations">>,
	<<"outcast-affiliation">>,
	<<"persistent-items">>,
	<<"publish">>,
	<<"purge-nodes">>,
	<<"retract-items">>,
	<<"retrieve-affiliations">>,
	<<"retrieve-items">>,
	<<"retrieve-subscriptions">>,
	<<"subscribe">>].

create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
    node_pep:create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access).

create_node(Nidx, Owner) ->
    node_pep:create_node(Nidx, Owner).

delete_node(Removed) ->
    node_pep:delete_node(Removed).

subscribe_node(Nidx, Sender, Subscriber, AccessModel,
	    SendLast, PresenceSubscription, RosterGroup, Options) ->
    node_pep:subscribe_node(Nidx, Sender, Subscriber, AccessModel, SendLast,
	PresenceSubscription, RosterGroup, Options).

unsubscribe_node(Nidx, Sender, Subscriber, SubId) ->
    node_pep:unsubscribe_node(Nidx, Sender, Subscriber, SubId).

publish_item(Nidx, Publisher, Model, MaxItems, ItemId, Payload, PubOpts) ->
    node_pep:publish_item(Nidx, Publisher, Model, MaxItems, ItemId,
	Payload, PubOpts).

remove_extra_items(Nidx, MaxItems, ItemIds) ->
    node_pep:remove_extra_items(Nidx, MaxItems, ItemIds).

delete_item(Nidx, Publisher, PublishModel, ItemId) ->
    node_pep:delete_item(Nidx, Publisher, PublishModel, ItemId).

purge_node(Nidx, Owner) ->
    node_pep:purge_node(Nidx, Owner).

get_entity_affiliations(Host, Owner) ->
    node_pep:get_entity_affiliations(Host, Owner).

get_node_affiliations(Nidx) ->
    node_pep:get_node_affiliations(Nidx).

get_affiliation(Nidx, Owner) ->
    node_pep:get_affiliation(Nidx, Owner).

set_affiliation(Nidx, Owner, Affiliation) ->
    node_pep:set_affiliation(Nidx, Owner, Affiliation).

get_entity_subscriptions(Host, Owner) ->
    node_pep:get_entity_subscriptions(Host, Owner).

get_node_subscriptions(Nidx) ->
    node_pep:get_node_subscriptions(Nidx).

get_subscriptions(Nidx, Owner) ->
    node_pep:get_subscriptions(Nidx, Owner).

set_subscriptions(Nidx, Owner, Subscription, SubId) ->
    node_pep:set_subscriptions(Nidx, Owner, Subscription, SubId).

get_pending_nodes(Host, Owner) ->
    node_pep:get_pending_nodes(Host, Owner).

get_states(Nidx) ->
    node_pep:get_states(Nidx).

get_state(Nidx, JID) ->
    node_pep:get_state(Nidx, JID).

set_state(State) ->
    node_pep:set_state(State).

get_items(Nidx, From, RSM) ->
    node_pep:get_items(Nidx, From, RSM).

get_items(Nidx, JID, AccessModel, PresenceSubscription, RosterGroup, SubId, RSM) ->
    node_pep:get_items(Nidx, JID, AccessModel, PresenceSubscription, RosterGroup, SubId, RSM).

get_last_items(Nidx, From, Count) ->
    node_pep:get_last_items(Nidx, From, Count).

get_item(Nidx, ItemId) ->
    node_pep:get_item(Nidx, ItemId).

get_item(Nidx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_pep:get_item(Nidx, ItemId, JID, AccessModel,
	PresenceSubscription, RosterGroup, SubId).

set_item(Item) ->
    node_pep:set_item(Item).

get_item_name(Host, Node, Id) ->
    node_pep:get_item_name(Host, Node, Id).

node_to_path(Node) ->
    node_pep:node_to_path(Node).

path_to_node(Path) ->
    node_pep:path_to_node(Path).
