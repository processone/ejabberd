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
%%% The Initial Developer of the Original Code is Process-one.
%%% Portions created by Process-one are Copyright 2006-2008, Process-one
%%% All Rights Reserved.''
%%% This software is copyright 2006-2008, Process-one.
%%%
%%%
%%% @copyright 2006-2008 Process-one
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @doc The module <strong>{@module}</strong> is the pep PubSub plugin.
%%% <p>PubSub plugin nodes are using the {@link gen_pubsub_node} behaviour.</p>

-module(node_pep).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").
-include("jlib.hrl").

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
	 get_items/2,
	 get_item/3,
	 set_item/1
	]).

init(Host, ServerHost, Opts) ->
    node_default:init(Host, ServerHost, Opts),
    ok.

terminate(Host, ServerHost) ->
    node_default:terminate(Host, ServerHost),
    ok.

options() ->
    [{node_type, pep},
     {deliver_payloads, true},
     {notify_config, false},
     {notify_delete, false},
     {notify_retract, false},
     {persist_items, false},
     {max_items, ?MAXITEMS div 2},
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
     "filtered-notifications", %*
     "item-ids",
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

create_node_permission(_Host, _ServerHost, _Node, _ParentNode, _Owner, _Access) ->
    %% TODO may we check bare JID match ?
    {result, true}.

create_node(Host, Node, Owner) ->
    case node_default:create_node(Host, Node, Owner) of
	{result, _} -> {result, []};
	Error -> Error
    end.

delete_node(Host, Removed) ->
    case node_default:delete_node(Host, Removed) of
	{result, {_, _, Removed}} -> {result, {[], Removed}};
	Error -> Error
    end.

subscribe_node(Host, Node, Sender, Subscriber, AccessModel,
	       SendLast, PresenceSubscription, RosterGroup) ->
    node_default:subscribe_node(
      Host, Node, Sender, Subscriber, AccessModel, SendLast,
      PresenceSubscription, RosterGroup).

unsubscribe_node(Host, Node, Sender, Subscriber, SubID) ->
    case node_default:unsubscribe_node(Host, Node, Sender, Subscriber, SubID) of
	{error, Error} -> {error, Error};
	{result, _} -> {result, []}
    end.

publish_item(Host, Node, Publisher, Model, MaxItems, ItemId, Payload) ->
    node_default:publish_item(Host, Node, Publisher, Model, MaxItems, ItemId, Payload).

remove_extra_items(Host, Node, MaxItems, ItemIds) ->
    node_default:remove_extra_items(Host, Node, MaxItems, ItemIds).

delete_item(Host, Node, JID, ItemId) ->
    node_default:delete_item(Host, Node, JID, ItemId).

purge_node(Host, Node, Owner) ->
    node_default:purge_node(Host, Node, Owner).

get_entity_affiliations(_Host, Owner) ->
    OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    node_default:get_entity_affiliations(OwnerKey, Owner).

get_node_affiliations(_Host, Node) ->
    States = mnesia:match_object(
	#pubsub_state{stateid = {'_', {'_', Node}},
	_ = '_'}),
    Tr = fun(#pubsub_state{stateid = {J, {_, _}}, affiliation = A}) ->
	{J, A}
	end,
    {result, lists:map(Tr, States)}.

get_affiliation(_Host, Node, Owner) ->
    OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    node_default:get_affiliation(OwnerKey, Node, Owner).

set_affiliation(_Host, Node, Owner, Affiliation) ->
    OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    Record = case get_state(OwnerKey, Node, OwnerKey) of
	    {error, ?ERR_ITEM_NOT_FOUND} ->
		#pubsub_state{stateid = {OwnerKey, {OwnerKey, Node}},
			      affiliation = Affiliation};
	    {result, State} ->
		State#pubsub_state{affiliation = Affiliation}
    end,    
    set_state(Record),
    ok.

get_entity_subscriptions(_Host, _Owner) ->
    {result, []}.

get_node_subscriptions(_Host, _Node) ->
    {result, []}.

get_subscription(_Host, _Node, _Owner) ->
    {result, unknown}.

set_subscription(_Host, _Node, _Owner, _Subscription) ->
    ok.

get_states(Host, Node) ->
    node_default:get_states(Host, Node).

get_state(Host, Node, JID) ->
    node_default:get_state(Host, Node, JID).

set_state(State) ->
    node_default:set_state(State).

get_items(Host, Node) ->
    node_default:get_items(Host, Node).

get_item(Host, Node, ItemId) ->
    node_default:get_item(Host, Node, ItemId).

set_item(Item) ->
    node_default:set_item(Item).
