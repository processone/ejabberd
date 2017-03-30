%%%----------------------------------------------------------------------
%%% File    : node_dag.erl
%%% Author  : Brian Cully <bjc@kublai.com>
%%% Purpose : experimental support of XEP-248
%%% Created : 15 Jun 2009 by Brian Cully <bjc@kublai.com>
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

-module(node_dag).
-behaviour(gen_pubsub_node).
-author('bjc@kublai.com').

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
    [{node_type, leaf} | node_hometree:options()].

features() ->
    [<<"multi-collection">> | node_hometree:features()].

create_node_permission(_Host, _ServerHost, _Node, _ParentNode, _Owner, _Access) ->
    {result, true}.

create_node(Nidx, Owner) ->
    node_hometree:create_node(Nidx, Owner).

delete_node(Removed) ->
    node_hometree:delete_node(Removed).

subscribe_node(Nidx, Sender, Subscriber, AccessModel,
	    SendLast, PresenceSubscription, RosterGroup, Options) ->
    node_hometree:subscribe_node(Nidx, Sender, Subscriber, AccessModel, SendLast,
	PresenceSubscription, RosterGroup, Options).

unsubscribe_node(Nidx, Sender, Subscriber, SubId) ->
    node_hometree:unsubscribe_node(Nidx, Sender, Subscriber, SubId).

publish_item(Nidx, Publisher, Model, MaxItems, ItemId, Payload, PubOpts) ->
    case nodetree_dag:get_node(Nidx) of
	#pubsub_node{options = Options} ->
	    case find_opt(node_type, Options) of
		collection ->
		    Txt = <<"Publishing items to collection node is not allowed">>,
		    {error, mod_pubsub:extended_error(
			      xmpp:err_not_allowed(Txt, ?MYLANG),
			      mod_pubsub:err_unsupported('publish'))};
		_ ->
		    node_hometree:publish_item(Nidx, Publisher, Model,
			MaxItems, ItemId, Payload, PubOpts)
	    end;
	Err -> Err
    end.

find_opt(_, []) -> false;
find_opt(Option, [{Option, Value} | _]) -> Value;
find_opt(Option, [_ | T]) -> find_opt(Option, T).

remove_extra_items(Nidx, MaxItems, ItemIds) ->
    node_hometree:remove_extra_items(Nidx, MaxItems, ItemIds).

delete_item(Nidx, Publisher, PublishModel, ItemId) ->
    node_hometree:delete_item(Nidx, Publisher, PublishModel, ItemId).

purge_node(Nidx, Owner) ->
    node_hometree:purge_node(Nidx, Owner).

get_entity_affiliations(Host, Owner) ->
    node_hometree:get_entity_affiliations(Host, Owner).

get_node_affiliations(Nidx) ->
    node_hometree:get_node_affiliations(Nidx).

get_affiliation(Nidx, Owner) ->
    node_hometree:get_affiliation(Nidx, Owner).

set_affiliation(Nidx, Owner, Affiliation) ->
    node_hometree:set_affiliation(Nidx, Owner, Affiliation).

get_entity_subscriptions(Host, Owner) ->
    node_hometree:get_entity_subscriptions(Host, Owner).

get_node_subscriptions(Nidx) ->
    node_hometree:get_node_subscriptions(Nidx).

get_subscriptions(Nidx, Owner) ->
    node_hometree:get_subscriptions(Nidx, Owner).

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
