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
    node_hometree:init(Host, ServerHost, Opts).

terminate(Host, ServerHost) ->
    node_hometree:terminate(Host, ServerHost).

options() ->
    [{node_type, leaf} | node_hometree:options()].

features() ->
    [<<"multi-collection">> | node_hometree:features()].

create_node_permission(_Host, _ServerHost, _Node,
		       _ParentNode, _Owner, _Access) ->
    {result, true}.

create_node(NodeID, Owner) ->
    node_hometree:create_node(NodeID, Owner).

delete_node(Removed) ->
    node_hometree:delete_node(Removed).

subscribe_node(NodeID, Sender, Subscriber, AccessModel,
	       SendLast, PresenceSubscription, RosterGroup, Options) ->
    node_hometree:subscribe_node(NodeID, Sender, Subscriber,
				 AccessModel, SendLast, PresenceSubscription,
				 RosterGroup, Options).

unsubscribe_node(NodeID, Sender, Subscriber, SubID) ->
    node_hometree:unsubscribe_node(NodeID, Sender,
				   Subscriber, SubID).

publish_item(NodeID, Publisher, Model, MaxItems, ItemID,
	     Payload) ->
    case nodetree_dag:get_node(NodeID) of
      #pubsub_node{options = Options} ->
	  case find_opt(node_type, Options) of
	    collection ->
		{error,
		 ?ERR_EXTENDED((?ERR_NOT_ALLOWED), <<"publish">>)};
	    _ ->
		node_hometree:publish_item(NodeID, Publisher, Model,
					   MaxItems, ItemID, Payload)
	  end;
      Err -> Err
    end.

find_opt(_, []) -> false;
find_opt(Option, [{Option, Value} | _]) -> Value;
find_opt(Option, [_ | T]) -> find_opt(Option, T).

remove_extra_items(NodeID, MaxItems, ItemIDs) ->
    node_hometree:remove_extra_items(NodeID, MaxItems,
				     ItemIDs).

delete_item(NodeID, Publisher, PublishModel, ItemID) ->
    node_hometree:delete_item(NodeID, Publisher,
			      PublishModel, ItemID).

purge_node(NodeID, Owner) ->
    node_hometree:purge_node(NodeID, Owner).

get_entity_affiliations(Host, Owner) ->
    node_hometree:get_entity_affiliations(Host, Owner).

get_node_affiliations(NodeID) ->
    node_hometree:get_node_affiliations(NodeID).

get_affiliation(NodeID, Owner) ->
    node_hometree:get_affiliation(NodeID, Owner).

set_affiliation(NodeID, Owner, Affiliation) ->
    node_hometree:set_affiliation(NodeID, Owner,
				  Affiliation).

get_entity_subscriptions(Host, Owner) ->
    node_hometree:get_entity_subscriptions(Host, Owner).

get_node_subscriptions(NodeID) ->
    node_hometree:get_node_subscriptions(NodeID).

get_subscriptions(NodeID, Owner) ->
    node_hometree:get_subscriptions(NodeID, Owner).

set_subscriptions(NodeID, Owner, Subscription, SubID) ->
    node_hometree:set_subscriptions(NodeID, Owner,
				    Subscription, SubID).

get_pending_nodes(Host, Owner) ->
    node_hometree:get_pending_nodes(Host, Owner).

get_states(NodeID) -> node_hometree:get_states(NodeID).

get_state(NodeID, JID) ->
    node_hometree:get_state(NodeID, JID).

set_state(State) -> node_hometree:set_state(State).

get_items(NodeID, From) ->
    node_hometree:get_items(NodeID, From).

get_items(NodeID, JID, AccessModel,
	  PresenceSubscription, RosterGroup, SubID) ->
    node_hometree:get_items(NodeID, JID, AccessModel,
			    PresenceSubscription, RosterGroup, SubID).

get_item(NodeID, ItemID) ->
    node_hometree:get_item(NodeID, ItemID).

get_item(NodeID, ItemID, JID, AccessModel,
	 PresenceSubscription, RosterGroup, SubID) ->
    node_hometree:get_item(NodeID, ItemID, JID, AccessModel,
			   PresenceSubscription, RosterGroup, SubID).

set_item(Item) -> node_hometree:set_item(Item).

get_item_name(Host, Node, ID) ->
    node_hometree:get_item_name(Host, Node, ID).

node_to_path(Node) -> node_hometree:node_to_path(Node).

path_to_node(Path) -> node_hometree:path_to_node(Path).
