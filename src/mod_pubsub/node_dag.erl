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
%%% @author Brian Cully <bjc@kublai.com>
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

-module(node_dag).
-author('bjc@kublai.com').

-include("pubsub.hrl").
-include("jlib.hrl").

-behaviour(gen_pubsub_node).

%% API definition
-export([init/3, terminate/2,
	 options/0, features/0,
	 create_node_permission/6,
	 create_node/2,
	 delete_node/1,
	 purge_node/2,
	 subscribe_node/8,
	 unsubscribe_node/4,
	 publish_item/6,
	 delete_item/4,
	 remove_extra_items/3,
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
	 get_items/6,
	 get_items/2,
	 get_item/7,
	 get_item/2,
	 set_item/1,
	 get_item_name/3,
     node_to_path/1,
     path_to_node/1]).


init(Host, ServerHost, Opts) ->
    node_flat:init(Host, ServerHost, Opts).

terminate(Host, ServerHost) ->
    node_flat:terminate(Host, ServerHost).

options() ->
    [{node_type, leaf} | node_flat:options()].

features() ->
    ["multi-collection" | node_flat:features()].

create_node_permission(_Host, _ServerHost, _Node, _ParentNode,
		       _Owner, _Access) ->
    {result, true}.

create_node(NodeID, Owner) ->
    node_flat:create_node(NodeID, Owner).

delete_node(Removed) ->
    node_flat:delete_node(Removed).

subscribe_node(NodeID, Sender, Subscriber, AccessModel,
	       SendLast, PresenceSubscription, RosterGroup, Options) ->
    node_flat:subscribe_node(NodeID, Sender, Subscriber, AccessModel,
				SendLast, PresenceSubscription, RosterGroup,
				Options).

unsubscribe_node(NodeID, Sender, Subscriber, SubID) ->
    node_flat:unsubscribe_node(NodeID, Sender, Subscriber, SubID).

publish_item(NodeID, Publisher, Model, MaxItems, ItemID, Payload) ->
    %% TODO: should look up the NodeTree plugin here. There's no
    %% access to the Host of the request at this level, so for now we
    %% just use nodetree_dag.
    case nodetree_dag:get_node(NodeID) of
        #pubsub_node{options = Options} ->
            case find_opt(node_type, Options) of
                collection ->
                    {error, mod_pubsub:extended_error('not-allowed', "publish")};
                _ ->
                    node_flat:publish_item(NodeID, Publisher, Model,
                                               MaxItems, ItemID, Payload)
            end;
        Err ->
            Err
    end.

find_opt(_,      [])                    -> false;
find_opt(Option, [{Option, Value} | _]) -> Value;
find_opt(Option, [_ | T])               -> find_opt(Option, T).

remove_extra_items(NodeID, MaxItems, ItemIDs) ->
    node_flat:remove_extra_items(NodeID, MaxItems, ItemIDs).

delete_item(NodeID, Publisher, PublishModel, ItemID) ->
    node_flat:delete_item(NodeID, Publisher, PublishModel, ItemID).

purge_node(NodeID, Owner) ->
    node_flat:purge_node(NodeID, Owner).

get_entity_affiliations(Host, Owner) ->
    node_flat:get_entity_affiliations(Host, Owner).

get_node_affiliations(NodeID) ->
    node_flat:get_node_affiliations(NodeID).

get_affiliation(NodeID, Owner) ->
    node_flat:get_affiliation(NodeID, Owner).

set_affiliation(NodeID, Owner, Affiliation) ->
    node_flat:set_affiliation(NodeID, Owner, Affiliation).

get_entity_subscriptions(Host, Owner) ->
    node_flat:get_entity_subscriptions(Host, Owner).

get_node_subscriptions(NodeID) ->
    node_flat:get_node_subscriptions(NodeID).

get_subscriptions(NodeID, Owner) ->
    node_flat:get_subscriptions(NodeID, Owner).

set_subscriptions(NodeID, Owner, Subscription, SubID) ->
    node_flat:set_subscriptions(NodeID, Owner, Subscription, SubID).

get_pending_nodes(Host, Owner) ->
    node_flat:get_pending_nodes(Host, Owner).

get_states(NodeID) ->
    node_flat:get_states(NodeID).

get_state(NodeID, JID) ->
    node_flat:get_state(NodeID, JID).

set_state(State) ->
    node_flat:set_state(State).

get_items(NodeID, From) ->
    node_flat:get_items(NodeID, From).

get_items(NodeID, JID, AccessModel, PresenceSubscription,
	  RosterGroup, SubID) ->
    node_flat:get_items(NodeID, JID, AccessModel, PresenceSubscription,
			   RosterGroup, SubID).

get_item(NodeID, ItemID) ->
    node_flat:get_item(NodeID, ItemID).

get_item(NodeID, ItemID, JID, AccessModel, PresenceSubscription,
	 RosterGroup, SubID) ->
    node_flat:get_item(NodeID, ItemID, JID, AccessModel,
			  PresenceSubscription, RosterGroup, SubID).

set_item(Item) ->
    node_flat:set_item(Item).

get_item_name(Host, Node, ID) ->
    node_flat:get_item_name(Host, Node, ID).

node_to_path(Node) ->
    node_flat:node_to_path(Node).

path_to_node(Path) ->
    node_flat:path_to_node(Path).
