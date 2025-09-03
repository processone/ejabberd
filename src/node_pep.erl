%%%----------------------------------------------------------------------
%%% File    : node_pep.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Standard PubSub PEP plugin
%%% Created :  1 Dec 2007 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

%%% @doc The module <strong>{@module}</strong> is the pep PubSub plugin.
%%% <p>PubSub plugin nodes are using the {@link gen_pubsub_node} behaviour.</p>

-module(node_pep).
-behaviour(gen_pubsub_node).
-author('christophe.romain@process-one.net').

-protocol({xep, 384, '0.8.3', '21.12', "complete", ""}).

-include("pubsub.hrl").

-export([init/3,
         terminate/2,
         options/0,
         features/0,
         create_node_permission/6,
         create_node/2,
         delete_node/1,
         purge_node/2,
         subscribe_node/8,
         unsubscribe_node/4,
         publish_item/7,
         delete_item/4,
         remove_extra_items/2, remove_extra_items/3,
         remove_expired_items/2,
         get_entity_affiliations/2,
         get_node_affiliations/1,
         get_affiliation/2,
         set_affiliation/3,
         get_entity_subscriptions/2,
         get_node_subscriptions/1,
         get_subscriptions/2,
         set_subscriptions/4,
         get_pending_nodes/2,
         get_states/1,
         get_state/2,
         set_state/1,
         get_items/7, get_items/3,
         get_item/7,
         get_last_items/3,
         get_only_item/2,
         get_item/2,
         set_item/1,
         get_item_name/3,
         node_to_path/1,
         path_to_node/1,
         depends/3]).


depends(_Host, _ServerHost, _Opts) ->
    [{mod_caps, hard}].


init(Host, ServerHost, Opts) ->
    node_flat:init(Host, ServerHost, Opts),
    ok.


terminate(Host, ServerHost) ->
    node_flat:terminate(Host, ServerHost),
    ok.


options() ->
    [{deliver_payloads, true},
     {notify_config, false},
     {notify_delete, false},
     {notify_retract, false},
     {purge_offline, false},
     {persist_items, true},
     {max_items, 1},
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
     <<"config-node">>,
     <<"config-node-max">>,
     <<"delete-nodes">>,
     <<"delete-items">>,
     <<"filtered-notifications">>,
     <<"item-ids">>,
     <<"modify-affiliations">>,
     <<"multi-items">>,
     <<"outcast-affiliation">>,
     <<"persistent-items">>,
     <<"publish">>,
     <<"publish-options">>,
     <<"purge-nodes">>,
     <<"retract-items">>,
     <<"retrieve-affiliations">>,
     <<"retrieve-items">>,
     <<"retrieve-subscriptions">>,
     <<"subscribe">>].


create_node_permission(Host, ServerHost, _Node, _ParentNode, Owner, Access) ->
    LOwner = jid:tolower(Owner),
    {User, Server, _Resource} = LOwner,
    Allowed = case LOwner of
                  {<<"">>, Host, <<"">>} ->
                      true;  % pubsub service always allowed
                  _ ->
                      case acl:match_rule(ServerHost, Access, LOwner) of
                          allow ->
                              case Host of
                                  {User, Server, _} -> true;
                                  _ -> false
                              end;
                          _ ->
                              false
                      end
              end,
    {result, Allowed}.


create_node(Nidx, Owner) ->
    node_flat:create_node(Nidx, Owner).


delete_node(Nodes) ->
    node_flat:delete_node(Nodes).


subscribe_node(Nidx,
               Sender,
               Subscriber,
               AccessModel,
               SendLast,
               PresenceSubscription,
               RosterGroup,
               Options) ->
    node_flat:subscribe_node(Nidx,
                             Sender,
                             Subscriber,
                             AccessModel,
                             SendLast,
                             PresenceSubscription,
                             RosterGroup,
                             Options).


unsubscribe_node(Nidx, Sender, Subscriber, SubId) ->
    case node_flat:unsubscribe_node(Nidx, Sender, Subscriber, SubId) of
        {error, Error} -> {error, Error};
        {result, _} -> {result, default}
    end.


publish_item(Nidx, Publisher, Model, MaxItems, ItemId, Payload, PubOpts) ->
    node_flat:publish_item(Nidx,
                           Publisher,
                           Model,
                           MaxItems,
                           ItemId,
                           Payload,
                           PubOpts).


remove_extra_items(Nidx, MaxItems) ->
    node_flat:remove_extra_items(Nidx, MaxItems).


remove_extra_items(Nidx, MaxItems, ItemIds) ->
    node_flat:remove_extra_items(Nidx, MaxItems, ItemIds).


remove_expired_items(Nidx, Seconds) ->
    node_flat:remove_expired_items(Nidx, Seconds).


delete_item(Nidx, Publisher, PublishModel, ItemId) ->
    node_flat:delete_item(Nidx, Publisher, PublishModel, ItemId).


purge_node(Nidx, Owner) ->
    node_flat:purge_node(Nidx, Owner).


get_entity_affiliations(Host, Owner) ->
    {_, D, _} = SubKey = jid:tolower(Owner),
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    States = mnesia:match_object(#pubsub_state{stateid = {GenKey, '_'}, _ = '_'}),
    NodeTree = mod_pubsub:tree(Host),
    Reply = lists:foldl(fun(#pubsub_state{stateid = {_, N}, affiliation = A}, Acc) ->
                                case NodeTree:get_node(N) of
                                    #pubsub_node{nodeid = {{_, D, _}, _}} = Node -> [{Node, A} | Acc];
                                    _ -> Acc
                                end
                        end,
                        [],
                        States),
    {result, Reply}.


get_node_affiliations(Nidx) ->
    node_flat:get_node_affiliations(Nidx).


get_affiliation(Nidx, Owner) ->
    node_flat:get_affiliation(Nidx, Owner).


set_affiliation(Nidx, Owner, Affiliation) ->
    node_flat:set_affiliation(Nidx, Owner, Affiliation).


get_entity_subscriptions(Host, Owner) ->
    {U, D, _} = SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    States = case SubKey of
                 GenKey ->
                     mnesia:match_object(#pubsub_state{stateid = {{U, D, '_'}, '_'}, _ = '_'});
                 _ ->
                     mnesia:match_object(#pubsub_state{stateid = {GenKey, '_'}, _ = '_'}) ++
                     mnesia:match_object(#pubsub_state{stateid = {SubKey, '_'}, _ = '_'})
             end,
    NodeTree = mod_pubsub:tree(Host),
    Reply = lists:foldl(fun(#pubsub_state{stateid = {J, N}, subscriptions = Ss}, Acc) ->
                                case NodeTree:get_node(N) of
                                    #pubsub_node{nodeid = {{_, D, _}, _}} = Node ->
                                        lists:foldl(fun({subscribed, SubId}, Acc2) ->
                                                            [{Node, subscribed, SubId, J} | Acc2];
                                                       ({pending, _SubId}, Acc2) ->
                                                            [{Node, pending, J} | Acc2];
                                                       (S, Acc2) ->
                                                            [{Node, S, J} | Acc2]
                                                    end,
                                                    Acc,
                                                    Ss);
                                    _ ->
                                        Acc
                                end
                        end,
                        [],
                        States),
    {result, Reply}.


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
    node_flat:get_items(Nidx,
                        JID,
                        AccessModel,
                        PresenceSubscription,
                        RosterGroup,
                        SubId,
                        RSM).


get_last_items(Nidx, From, Count) ->
    node_flat:get_last_items(Nidx, From, Count).


get_only_item(Nidx, From) ->
    node_flat:get_only_item(Nidx, From).


get_item(Nidx, ItemId) ->
    node_flat:get_item(Nidx, ItemId).


get_item(Nidx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_flat:get_item(Nidx,
                       ItemId,
                       JID,
                       AccessModel,
                       PresenceSubscription,
                       RosterGroup,
                       SubId).


set_item(Item) ->
    node_flat:set_item(Item).


get_item_name(Host, Node, Id) ->
    node_flat:get_item_name(Host, Node, Id).


node_to_path(Node) ->
    node_flat:node_to_path(Node).


path_to_node(Path) ->
    node_flat:path_to_node(Path).
