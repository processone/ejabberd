%%%----------------------------------------------------------------------
%%% File    : node_online.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Handle only online users, remove offline subscriptions and nodes
%%% Created : 15 Dec 2015 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-module(node_online).
-behaviour(gen_pubsub_node).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").
-include("jlib.hrl").

-export([init/3, terminate/2, options/0, features/0,
    create_node_permission/6, create_node/2, delete_node/1,
    purge_node/2, subscribe_node/8, unsubscribe_node/4,
    publish_item/6, delete_item/4, remove_extra_items/3,
    get_entity_affiliations/2, get_node_affiliations/1,
    get_affiliation/2, set_affiliation/3,
    get_entity_subscriptions/2, get_node_subscriptions/1,
    get_subscriptions/2, set_subscriptions/4,
    get_pending_nodes/2, get_states/1, get_state/2,
    set_state/1, get_items/7, get_items/3, get_item/7,
    get_item/2, set_item/1, get_item_name/3, node_to_path/1,
    path_to_node/1]).

-export([user_offline/3]).

init(Host, ServerHost, Opts) ->
    node_flat:init(Host, ServerHost, Opts),
    ejabberd_hooks:add(sm_remove_connection_hook, ServerHost,
		       ?MODULE, user_offline, 75),
    ok.

terminate(Host, ServerHost) ->
    node_flat:terminate(Host, ServerHost),
    ejabberd_hooks:delete(sm_remove_connection_hook, ServerHost,
			  ?MODULE, user_offline, 75),
    ok.

user_offline(_SID, #jid{luser=LUser,lserver=LServer}, _Info) ->
    mod_pubsub:remove_user(LUser, LServer).

options() ->
    [{deliver_payloads, true},
	{notify_config, false},
	{notify_delete, false},
	{notify_retract, false},
	{purge_offline, true},
	{persist_items, true},
	{max_items, ?MAXITEMS},
	{subscribe, true},
	{access_model, open},
	{roster_groups_allowed, []},
	{publish_model, publishers},
	{notification_type, headline},
	{max_payload_size, ?MAX_PAYLOAD_SIZE},
	{send_last_published_item, on_sub_and_presence},
	{deliver_notifications, true},
	{presence_based_delivery, true}].

features() ->
    node_flat:features().

create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
    node_flat:create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access).

create_node(Nidx, Owner) ->
    node_flat:create_node(Nidx, Owner).

delete_node(Removed) ->
    node_flat:delete_node(Removed).

subscribe_node(Nidx, Sender, Subscriber, AccessModel,
	    SendLast, PresenceSubscription, RosterGroup, Options) ->
    node_flat:subscribe_node(Nidx, Sender, Subscriber, AccessModel, SendLast,
	PresenceSubscription, RosterGroup, Options).

unsubscribe_node(Nidx, Sender, Subscriber, SubId) ->
    node_flat:unsubscribe_node(Nidx, Sender, Subscriber, SubId).

publish_item(Nidx, Publisher, Model, MaxItems, ItemId, Payload) ->
    node_flat:publish_item(Nidx, Publisher, Model, MaxItems, ItemId, Payload).

remove_extra_items(Nidx, MaxItems, ItemIds) ->
    node_flat:remove_extra_items(Nidx, MaxItems, ItemIds).

delete_item(Nidx, Publisher, PublishModel, ItemId) ->
    node_flat:delete_item(Nidx, Publisher, PublishModel, ItemId).

purge_node(Nidx, Owner) ->
    node_flat:purge_node(Nidx, Owner).

get_entity_affiliations(Host, Owner) ->
    node_flat:get_entity_affiliations(Host, Owner).

get_node_affiliations(Nidx) ->
    node_flat:get_node_affiliations(Nidx).

get_affiliation(Nidx, Owner) ->
    node_flat:get_affiliation(Nidx, Owner).

set_affiliation(Nidx, Owner, Affiliation) ->
    node_flat:set_affiliation(Nidx, Owner, Affiliation).

get_entity_subscriptions(Host, Owner) ->
    node_flat:get_entity_subscriptions(Host, Owner).

get_node_subscriptions(Nidx) ->
    node_flat:get_node_subscriptions(Nidx).

get_subscriptions(Nidx, Owner) ->
    node_flat:get_subscriptions(Nidx, Owner).

set_subscriptions(Nidx, Owner, Subscription, SubId) ->
    node_flat:set_subscriptions(Nidx, Owner, Subscription, SubId).

get_pending_nodes(Host, Owner) ->
    node_flat:get_pending_nodes(Host, Owner).

get_states(Nidx) ->
    node_flat:get_states(Nidx).

get_state(Nidx, JID) ->
    node_flat:get_state(Nidx, JID).

set_state(State) ->
    node_flat:set_state(State).

get_items(Nidx, From, RSM) ->
    node_flat:get_items(Nidx, From, RSM).

get_items(Nidx, JID, AccessModel, PresenceSubscription, RosterGroup, SubId, RSM) ->
    node_flat:get_items(Nidx, JID, AccessModel,
	PresenceSubscription, RosterGroup, SubId, RSM).

get_item(Nidx, ItemId) ->
    node_flat:get_item(Nidx, ItemId).

get_item(Nidx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_flat:get_item(Nidx, ItemId, JID, AccessModel,
	PresenceSubscription, RosterGroup, SubId).

set_item(Item) ->
    node_flat:set_item(Item).

get_item_name(Host, Node, Id) ->
    node_flat:get_item_name(Host, Node, Id).

node_to_path(Node) ->
    node_flat:node_to_path(Node).

path_to_node(Path) ->
    node_flat:path_to_node(Path).
