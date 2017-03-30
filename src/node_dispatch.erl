%%%----------------------------------------------------------------------
%%% File    : node_dispatch.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Publish item to node and all child subnodes
%%% Created :  1 Dec 2007 by Christophe Romain <christophe.romain@process-one.net>
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

%%% @doc <p>The <strong>{@module}</strong> module is a PubSub plugin whose
%%% goal is to republished each published item to all its children.</p>
%%% <p>Users cannot subscribe to this node, but are supposed to subscribe to
%%% its children.</p>
%%% This module can not work with virtual nodetree

-module(node_dispatch).
-behaviour(gen_pubsub_node).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").
-include("xmpp.hrl").

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
    node_hometree:init(Host, ServerHost, Opts).

terminate(Host, ServerHost) ->
    node_hometree:terminate(Host, ServerHost).

options() ->
    [{deliver_payloads, true},
	{notify_config, false},
	{notify_delete, false},
	{notify_retract, true},
	{purge_offline, false},
	{persist_items, true},
	{max_items, ?MAXITEMS},
	{subscribe, true},
	{access_model, presence},
	{roster_groups_allowed, []},
	{publish_model, publishers},
	{notification_type, headline},
	{max_payload_size, ?MAX_PAYLOAD_SIZE},
	{send_last_published_item, never},
	{deliver_notifications, true},
	{presence_based_delivery, false},
	{itemreply, none}].

features() ->
    [<<"create-nodes">>,
	<<"delete-nodes">>,
	<<"instant-nodes">>,
	<<"outcast-affiliation">>,
	<<"persistent-items">>,
	<<"publish">>,
	<<"retrieve-items">>].

create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
    node_hometree:create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access).

create_node(Nidx, Owner) ->
    node_hometree:create_node(Nidx, Owner).

delete_node(Nodes) ->
    node_hometree:delete_node(Nodes).

subscribe_node(_Nidx, _Sender, _Subscriber, _AccessModel, _SendLast, _PresenceSubscription,
	    _RosterGroup, _Options) ->
    {error, mod_pubsub:extended_error(xmpp:err_feature_not_implemented(),
				      mod_pubsub:err_unsupported('subscribe'))}.

unsubscribe_node(_Nidx, _Sender, _Subscriber, _SubId) ->
    {error, mod_pubsub:extended_error(xmpp:err_feature_not_implemented(),
				      mod_pubsub:err_unsupported('subscribe'))}.

publish_item(Nidx, Publisher, PublishModel, MaxItems, ItemId, Payload,
	     PubOpts) ->
    case nodetree_tree:get_node(Nidx) of
	#pubsub_node{nodeid = {Host, Node}} ->
	    lists:foreach(fun (SubNode) ->
			node_hometree:publish_item(SubNode#pubsub_node.id,
			    Publisher, PublishModel, MaxItems,
			    ItemId, Payload, PubOpts)
		end,
		nodetree_tree:get_subnodes(Host, Node, Publisher)),
	    {result, {default, broadcast, []}};
	Error ->
	    Error
    end.

remove_extra_items(_Nidx, _MaxItems, ItemIds) ->
    {result, {ItemIds, []}}.

delete_item(_Nidx, _Publisher, _PublishModel, _ItemId) ->
    {error, mod_pubsub:extended_error(xmpp:err_feature_not_implemented(),
				      mod_pubsub:err_unsupported('delete-items'))}.

purge_node(_Nidx, _Owner) ->
    {error, mod_pubsub:extended_error(xmpp:err_feature_not_implemented(),
				      mod_pubsub:err_unsupported('purge-nodes'))}.

get_entity_affiliations(_Host, _Owner) ->
    {result, []}.

get_node_affiliations(_Nidx) ->
    {result, []}.

get_affiliation(_Nidx, _Owner) ->
    {result, none}.

set_affiliation(Nidx, Owner, Affiliation) ->
    node_hometree:set_affiliation(Nidx, Owner, Affiliation).

get_entity_subscriptions(_Host, _Owner) ->
    {result, []}.

get_node_subscriptions(Nidx) ->
    node_hometree:get_node_subscriptions(Nidx).

get_subscriptions(_Nidx, _Owner) ->
    {result, []}.

set_subscriptions(Nidx, Owner, Subscription, SubId) ->
    node_hometree:set_subscriptions(Nidx, Owner, Subscription, SubId).

get_pending_nodes(Host, Owner) ->
    node_hometree:get_pending_nodes(Host, Owner).

get_states(Nidx) ->
    node_hometree:get_states(Nidx).

get_state(Nidx, JID) ->
    node_hometree:get_state(Nidx, JID).

set_state(State) ->
    node_hometree:set_state(State).

get_items(Nidx, From, RSM) ->
    node_hometree:get_items(Nidx, From, RSM).

get_items(Nidx, JID, AccessModel, PresenceSubscription, RosterGroup, SubId, RSM) ->
    node_hometree:get_items(Nidx, JID, AccessModel,
	PresenceSubscription, RosterGroup, SubId, RSM).

get_last_items(Nidx, From, Count) ->
    node_hometree:get_last_items(Nidx, From, Count).

get_item(Nidx, ItemId) ->
    node_hometree:get_item(Nidx, ItemId).

get_item(Nidx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_hometree:get_item(Nidx, ItemId, JID, AccessModel,
	PresenceSubscription, RosterGroup, SubId).

set_item(Item) ->
    node_hometree:set_item(Item).

get_item_name(Host, Node, Id) ->
    node_hometree:get_item_name(Host, Node, Id).

node_to_path(Node) ->
    node_hometree:node_to_path(Node).

path_to_node(Path) ->
    node_hometree:path_to_node(Path).
