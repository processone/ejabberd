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
%%% @copyright 2006-2015 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================


-module(node_hometree_odbc).
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
    path_to_node/1,
    get_entity_subscriptions_for_send_last/2, get_last_items/3]).

init(Host, ServerHost, Opts) ->
    node_flat_odbc:init(Host, ServerHost, Opts),
    Owner = mod_pubsub:service_jid(Host),
    mod_pubsub:create_node(Host, ServerHost, <<"/home">>, Owner, <<"hometree">>),
    mod_pubsub:create_node(Host, ServerHost, <<"/home/", ServerHost/binary>>, Owner, <<"hometree">>),
    ok.

terminate(Host, ServerHost) ->
    node_flat_odbc:terminate(Host, ServerHost).

options() ->
    [{odbc, true}, {rsm, true} | node_hometree:options()].

features() ->
    [<<"rsm">> | node_hometree:features()].

create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
    node_hometree:create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access).

create_node(Nidx, Owner) ->
    node_flat_odbc:create_node(Nidx, Owner).

delete_node(Nodes) ->
    node_flat_odbc:delete_node(Nodes).

subscribe_node(Nidx, Sender, Subscriber, AccessModel,
	    SendLast, PresenceSubscription, RosterGroup, Options) ->
    node_flat_odbc:subscribe_node(Nidx, Sender, Subscriber, AccessModel, SendLast,
	PresenceSubscription, RosterGroup, Options).

unsubscribe_node(Nidx, Sender, Subscriber, SubId) ->
    node_flat_odbc:unsubscribe_node(Nidx, Sender, Subscriber, SubId).

publish_item(Nidx, Publisher, Model, MaxItems, ItemId, Payload) ->
    node_flat_odbc:publish_item(Nidx, Publisher, Model, MaxItems, ItemId, Payload).

remove_extra_items(Nidx, MaxItems, ItemIds) ->
    node_flat_odbc:remove_extra_items(Nidx, MaxItems, ItemIds).

delete_item(Nidx, Publisher, PublishModel, ItemId) ->
    node_flat_odbc:delete_item(Nidx, Publisher, PublishModel, ItemId).

purge_node(Nidx, Owner) ->
    node_flat_odbc:purge_node(Nidx, Owner).

get_entity_affiliations(Host, Owner) ->
    node_flat_odbc:get_entity_affiliations(Host, Owner).

get_node_affiliations(Nidx) ->
    node_flat_odbc:get_node_affiliations(Nidx).

get_affiliation(Nidx, Owner) ->
    node_flat_odbc:get_affiliation(Nidx, Owner).

set_affiliation(Nidx, Owner, Affiliation) ->
    node_flat_odbc:set_affiliation(Nidx, Owner, Affiliation).

get_entity_subscriptions(Host, Owner) ->
    node_flat_odbc:get_entity_subscriptions(Host, Owner).

get_entity_subscriptions_for_send_last(Host, Owner) ->
    node_flat_odbc:get_entity_subscriptions_for_send_last(Host, Owner).

get_node_subscriptions(Nidx) ->
    node_flat_odbc:get_node_subscriptions(Nidx).

get_subscriptions(Nidx, Owner) ->
    node_flat_odbc:get_subscriptions(Nidx, Owner).

set_subscriptions(Nidx, Owner, Subscription, SubId) ->
    node_flat_odbc:set_subscriptions(Nidx, Owner, Subscription, SubId).

get_pending_nodes(Host, Owner) ->
    node_flat_odbc:get_pending_nodes(Host, Owner).

get_states(Nidx) ->
    node_flat_odbc:get_states(Nidx).

get_state(Nidx, JID) ->
    node_flat_odbc:get_state(Nidx, JID).

set_state(State) ->
    node_flat_odbc:set_state(State).

get_items(Nidx, From, RSM) ->
    node_flat_odbc:get_items(Nidx, From, RSM).

get_items(Nidx, JID, AccessModel, PresenceSubscription, RosterGroup, SubId, RSM) ->
    node_flat_odbc:get_items(Nidx, JID, AccessModel,
	PresenceSubscription, RosterGroup, SubId, RSM).

get_item(Nidx, ItemId) ->
    node_flat_odbc:get_item(Nidx, ItemId).

get_item(Nidx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_flat_odbc:get_item(Nidx, ItemId, JID,
	AccessModel, PresenceSubscription, RosterGroup, SubId).

set_item(Item) ->
    node_flat_odbc:set_item(Item).

get_item_name(Host, Node, Id) ->
    node_flat_odbc:get_item_name(Host, Node, Id).

get_last_items(Nidx, From, Count) ->
    node_flat_odbc:get_last_items(Nidx, From, Count).

node_to_path(Node) ->
    node_hometree:node_to_path(Node).

path_to_node(Path) ->
    node_hometree:path_to_node(Path).

