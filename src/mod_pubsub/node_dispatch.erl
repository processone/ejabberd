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
%%% Portions created by ProcessOne are Copyright 2006-2009, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2009, ProcessOne.
%%%
%%%
%%% @copyright 2006-2009 ProcessOne
%%% @author Christophe romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

-module(node_dispatch).
-author('christophe.romain@process-one.net').

-include_lib("exmpp/include/exmpp.hrl").

-include("pubsub.hrl").

-behaviour(gen_pubsub_node).

%%% @doc <p>The <strong>{@module}</strong> module is a PubSub plugin whose
%%% goal is to republished each published item to all its children.</p>
%%% <p>Users cannot subscribe to this node, but are supposed to subscribe to
%%% its children.</p>
%%% This module can not work with virtual nodetree

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
    node_default:init(Host, ServerHost, Opts).

terminate(Host, ServerHost) ->
    node_default:terminate(Host, ServerHost).

options() ->
    [{node_type, dispatch},
     {deliver_payloads, true},
     {notify_config, false},
     {notify_delete, false},
     {notify_retract, true},
     {persist_items, true},
     {max_items, ?MAXITEMS div 2},
     {subscribe, true},
     {access_model, open},
     {roster_groups_allowed, []},
     {publish_model, publishers},
     {max_payload_size, ?MAX_PAYLOAD_SIZE},
     {send_last_published_item, never},
     {deliver_notifications, true},
     {presence_based_delivery, false}].

features() ->
    ["create-nodes",
     "delete-nodes",
     "instant-nodes",
     "outcast-affiliation",
     "persistent-items",
     "publish",
     %%"purge-nodes",
     %%"retract-items",
     %%"retrieve-affiliations",
     "retrieve-items"
     %%"retrieve-subscriptions",
     %%"subscribe",
     %%"subscription-notifications",
    ].

create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
    node_default:create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access).

create_node(Host, Node, Owner) ->
    node_default:create_node(Host, Node, Owner).

delete_node(Host, Removed) ->
    node_default:delete_node(Host, Removed).

subscribe_node(_Host, _Node, _Sender, _Subscriber, _AccessModel,
	       _SendLast, _PresenceSubscription, _RosterGroup) ->
    {error, 'forbidden'}.

unsubscribe_node(_Host, _Node, _Sender, _Subscriber, _SubID) ->
    {error, 'forbidden'}.

publish_item(Host, Node, Publisher, Model, MaxItems, ItemId, Payload) ->
    lists:foreach(fun(SubNode) ->
			  node_default:publish_item(
			    Host, SubNode, Publisher, Model,
			    MaxItems, ItemId, Payload)
		  end, nodetree_default:get_subnodes(Host, Node, Publisher)).

remove_extra_items(_Host, _Node, _MaxItems, ItemIds) ->
    {result, {ItemIds, []}}.

delete_item(_Host, _Node, _JID, _ItemId) ->
    {error, 'item-not-found'}.

purge_node(_Host, _Node, _Owner) ->
    {error, 'forbidden'}.

get_entity_affiliations(_Host, _Owner) ->
    {result, []}.

get_node_affiliations(_Host, _Node) ->
    {result, []}.

get_affiliation(_Host, _Node, _Owner) ->
    {result, []}.

set_affiliation(Host, Node, Owner, Affiliation) ->
    node_default:set_affiliation(Host, Node, Owner, Affiliation).

get_entity_subscriptions(_Host, _Owner) ->
    {result, []}.

get_node_subscriptions(_Host, _Node) ->
    {result, []}.

get_subscription(_Host, _Node, _Owner) ->
    {result, []}.

set_subscription(Host, Node, Owner, Subscription) ->
    node_default:set_subscription(Host, Node, Owner, Subscription).

get_states(Host, Node) ->
    node_default:get_states(Host, Node).

get_state(Host, Node, JID) ->
    node_default:get_state(Host, Node, JID).

set_state(State) ->
    node_default:set_state(State).

get_items(Host, Node, From) ->
    node_default:get_items(Host, Node, From).

get_items(Host, Node, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_default:get_items(Host, Node, JID, AccessModel, PresenceSubscription, RosterGroup, SubId).

get_item(Host, Node, ItemId) ->
    node_default:get_item(Host, Node, ItemId).

get_item(Host, Node, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_default:get_item(Host, Node, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId).

set_item(Item) ->
    node_default:set_item(Item).

get_item_name(Host, Node, Id) ->
    node_default:get_item_name(Host, Node, Id).
