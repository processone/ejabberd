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


-spec(init/3 ::
      (
	     Host       :: string(),
	     ServerHost :: string(),
	     Opts       :: [{Key::atom(), Value::term()}])
      -> 'ok'
	    ).

init(Host, ServerHost, Opts) ->
    node_flat:init(Host, ServerHost, Opts).


-spec(terminate/2 ::
      (
		  Host       :: string(),
		  ServerHost :: string())
      -> 'ok'
	    ).

terminate(Host, ServerHost) ->
    node_flat:terminate(Host, ServerHost).


-spec(options/0 :: () -> [nodeOption()]).

options() ->
    [{'node_type', 'leaf'} | node_flat:options()].


-spec(features/0 :: () -> [Feature::string()]).

features() ->
    ["multi-collection" | node_flat:features()].


-spec(create_node_permission/6 ::
      (
			       Host         :: hostPubsub(),
			       ServerHost   :: string(),
			       NodeId       :: nodeId(),
			       ParentNodeId :: nodeId(),
			       JID          :: jidEntity(),
			       Access       :: atom())
      -> {'result', 'true'}
	    ).

create_node_permission(_Host, _ServerHost, _NodeId, _ParentNodeId,
		       _JID, _Access) ->
    {result, true}.


-spec(create_node/2 ::
      (
		    NodeIdx :: nodeIdx(),
		    JID     :: jidEntity())
      -> {'result', {'default', 'broadcast'}}
	    ).

create_node(NodeIdx, JID) ->
    node_flat:create_node(NodeIdx, JID).


-spec(delete_node/1 ::
      (
		    Nodes :: [Node::pubsubNode()])
      -> {result, {'default', 'broadcast',
		   Reply :: [{Node :: pubsubNode(),
			      [{Owner         :: bareUsr(),
				Subscriptions :: [{Subscription :: subscription(),
						   SubId        :: subId()}]}]}]}}
	    ).

delete_node(Nodes) ->
    node_flat:delete_node(Nodes).


-spec(subscribe_node/8 ::
      (
		       NodeIdx              :: nodeIdx(),
		       JID                  :: jidEntity(),
		       Subscriber           :: jidEntity(),
		       AccessModel          :: accessModel(),
		       SendLast             :: atom(),
		       PresenceSubscription :: boolean(),
		       RosterGroup          :: boolean(),
		       Options              :: [nodeOption()])
      -> {'result', {'default',
		     Subscription :: 'subscribed',
		     SubId        :: subId()}}
	     | {'result', {'default',
			   Subscription :: 'subscribed',
			   SubId        :: subId(),
			   SendLast     ::' send_last'}}
	     | {'result', {'default',
			   Subscription :: 'pending',
			   SubId        :: subId()}}
	     | {'error', _} %% TODO add all error cases
	    ).

subscribe_node(NodeIdx, JID, Subscriber, AccessModel,
	       SendLast, PresenceSubscription, RosterGroup, Options) ->
    node_flat:subscribe_node(NodeIdx, JID, Subscriber, AccessModel,
				SendLast, PresenceSubscription, RosterGroup,
				Options).


-spec(unsubscribe_node/4 ::
      (
			 NodeIdx    :: nodeIdx(),
			 JID        :: jidEntity(),
			 Subscriber :: jidEntity(),
			 SubId      :: subId())
      -> {'result', 'default'} | {'error', _} %% TODO : add all error cases
	    ).

unsubscribe_node(NodeIdx, JID, Subscriber, SubId) ->
    node_flat:unsubscribe_node(NodeIdx, JID, Subscriber, SubId).


-spec(publish_item/6 ::
      (
		     NodeIdx      :: nodeIdx(),
		     JID          :: jidEntity(),
		     PublishModel :: atom(), %% TODO : make a generic publishMod() type
		     MaxItems     :: 'unlimited' | integer(),
		     ItemId       :: itemId(),
		     Payload      :: payload())
      -> {'result', {'default', 'broadcast', ItemIds :: [] | [itemId()]}}
	     | {'error', _}
	    ).

publish_item(NodeIdx, JID, PublishModel, MaxItems, ItemId, Payload) ->
    %% TODO: should look up the NodeTree plugin here. There's no
    %% access to the Host of the request at this level, so for now we
    %% just use nodetree_dag.
    case nodetree_dag:get_node(NodeIdx) of
        #pubsub_node{options = Options} ->
            case find_opt('node_type', Options) of
                'collection' ->
                    {error, mod_pubsub:extended_error('not-allowed', "publish")};
                _ ->
                    node_flat:publish_item(NodeIdx, JID, PublishModel,
                                               MaxItems, ItemId, Payload)
            end;
        Error ->
            Error
    end.


-spec(find_opt/2 ::
      (
		     Key     :: atom(),
		     Options :: [] | [Option::nodeOption()])
      -> Value :: 'false' | term()
	    ).

find_opt(_,      [])              -> false;
find_opt(Key, [{Key, Value} | _]) -> Value;
find_opt(Key, [_ | Options])      -> find_opt(Key, Options).


-spec(remove_extra_items/3 ::
      (
			   NodeIdx  :: nodeIdx(),
			   MaxItems :: 'unlimited' | integer(),
			   ItemsIds :: [ItemId::itemId()])
      -> {'result',
	  {OldItems :: [] | [ItemId::itemId()],
	   NewItems :: [] | [ItemId::itemId()]}}
	    ).

remove_extra_items(NodeIdx, MaxItems, ItemIds) ->
    node_flat:remove_extra_items(NodeIdx, MaxItems, ItemIds).


-spec(delete_item/4 ::
      (
		    NodeIdx      :: nodeIdx(),
		    JID          :: jidEntity(),
		    PublishModel :: atom(),
		    ItemId       :: itemId())
      -> {'result', {'default', 'broadcast'}} | {'error', _}
	    ).

delete_item(NodeIdx, JID, PublishModel, ItemId) ->
    node_flat:delete_item(NodeIdx, JID, PublishModel, ItemId).


-spec(purge_node/2 ::
      (
		   NodeIdx :: nodeIdx(),
		   JID     :: jidEntity())
      -> {'result', {'default', 'broadcast'}} | {'error', 'forbidden'}
	    ).

purge_node(NodeIdx, JID) ->
    node_flat:purge_node(NodeIdx, JID).


-spec(get_entity_affiliations/2 ::
      (
				Host :: binary(),
				JID  :: jidEntity())
      -> {'result', Reply :: [] | [{Node::pubsubNode(), Affiliation::affiliation()}]}
	    ).

get_entity_affiliations(Host, JID) ->
    node_flat:get_entity_affiliations(Host, JID).


-spec(get_node_affiliations/1 ::
      (
			      NodeIdx :: nodeIdx())
      -> {'result', [] | [{Entity::fullUsr(), Affiliation::affiliation()}]}
	    ).

get_node_affiliations(NodeIdx) ->
    node_flat:get_node_affiliations(NodeIdx).


-spec(get_affiliation/2 ::
      (
			NodeIdx :: nodeIdx(),
			JID     :: jidEntity())
      -> {'result', Affiliation::affiliation()}
	    ).

get_affiliation(NodeIdx, JID) ->
    node_flat:get_affiliation(NodeIdx, JID).


-spec(set_affiliation/3 ::
      (
			NodeIdx     :: nodeIdx(),
			JID         :: jidEntity(),
			Affiliation :: affiliation())
      -> 'ok' | {error, 'internal-server-error'}
	    ).

set_affiliation(NodeIdx, JID, Affiliation) ->
    node_flat:set_affiliation(NodeIdx, JID, Affiliation).


-spec(get_entity_subscriptions/2 ::
      (
				 Host :: hostPubsub(),
				 JID  :: jidEntity())
      -> {'result', []
	  | [{Node         :: pubsubNode(),
	      Subscription :: subscription(),
	      SubId        :: subId(),
	      Entity       :: fullUsr()}]}
	    ).

get_entity_subscriptions(Host, JID) ->
    node_flat:get_entity_subscriptions(Host, JID).


-spec(get_node_subscriptions/1 ::
      (
			       NodeIdx :: nodeIdx())
      -> {'result', []
	  | [{Entity::fullUsr(), 'none'}]
	  | [{Entity::fullUsr(), Subscription::subscription(), SubId::subId()}]}
	    ).

get_node_subscriptions(NodeIdx) ->
    node_flat:get_node_subscriptions(NodeIdx).


-spec(get_subscriptions/2 ::
      (
			  NodeIdx :: nodeIdx(),
			  JID     :: jidEntity())
      -> {'result', Subscriptions :: [] | [{Subscription::subscription(), SubId::subId()}]}
	    ).

get_subscriptions(NodeIdx, JID) ->
    node_flat:get_subscriptions(NodeIdx, JID).


-spec(set_subscriptions/4 ::
      (
			  NodeIdx      :: nodeIdx(),
			  JID          :: jidEntity(),
			  Subscription :: subscription(),
			  SubId        :: subId())
      -> 'ok'
	     | {Subscription::subscription(), SubId::subId()}
	     | {error, _}
	    ).

set_subscriptions(NodeIdx, JID, Subscription, SubId) ->
    node_flat:set_subscriptions(NodeIdx, JID, Subscription, SubId).


-spec(get_pending_nodes/2 ::
      (
			  Host :: hostPubsub(),
			  JID  :: jidEntity())
      -> 'false' | {'value', NodeId::nodeId()}
	    ).

get_pending_nodes(Host, JID) ->
    node_flat:get_pending_nodes(Host, JID).


-spec(get_states/1 ::
      (
		   NodeIdx :: nodeIdx())
      -> {'result', States :: [] | [State::pubsubState()]}
	    ).

get_states(NodeIdx) ->
    node_flat:get_states(NodeIdx).


-spec(get_state/2 ::
      (
		  NodeIdx :: nodeIdx(),
		  Entity  :: fullUsr())
      -> State::pubsubState()
	    ).

get_state(NodeIdx, Entity) ->
    node_flat:get_state(NodeIdx, Entity).


-spec(set_state/1 ::
      (
		  State :: pubsubState())
      -> 'ok' | {error, 'internal-server-error'}
	    ).

set_state(State) ->
    node_flat:set_state(State).


-spec(get_items/2 ::
      (
		  NodeIdx :: nodeIdx(),
		  Entity  :: fullUsr())
      -> {'result', Items :: [] | [Item::pubsubItem()]}
	    ).

get_items(NodeIdx, Entity) ->
    node_flat:get_items(NodeIdx, Entity).


-spec(get_items/6 ::
      (
		  NodeIdx              :: nodeIdx(),
		  JID                  :: jidEntity(),
		  AccessModel          :: accessModel(),
		  PresenceSubscription :: boolean(),
		  RosterGroup          :: boolean(),
		  SubId                :: subId())
      -> {'result', Items :: [] | [Item::pubsubItem()]}
	     | {'error', _}
	    ).

get_items(NodeIdx, JID, AccessModel, PresenceSubscription,
	  RosterGroup, SubId) ->
    node_flat:get_items(NodeIdx, JID, AccessModel, PresenceSubscription,
			   RosterGroup, SubId).


-spec(get_item/2 ::
      (
		 NodeIdx :: nodeIdx(),
		 ItemId  :: itemId())
      -> {'result', Item::pubsubItem()} | {'error', 'item-not-found'}
	    ).

get_item(NodeIdx, ItemId) ->
    node_flat:get_item(NodeIdx, ItemId).


-spec(get_item/7 ::
      (
		 NodeIdx              :: nodeIdx(),
		 ItemId               :: itemId(),
		 JID                  :: jidEntity(),
		 AccessModel          :: accessModel(),
		 PresenceSubscription :: boolean(),
		 RosterGroup          :: boolean(),
		 SubId                :: subId())
      -> {'result', Item::pubsubItem()} | {'error', 'item-not-found'}
	    ).

get_item(NodeIdx, ItemId, JID, AccessModel, PresenceSubscription,
	 RosterGroup, SubId) ->
    node_flat:get_item(NodeIdx, ItemId, JID, AccessModel,
			  PresenceSubscription, RosterGroup, SubId).


-spec(set_item/1 ::
      (
		 Item :: pubsubItem())
      -> 'ok' | {error, 'internal-server-error'}
	    ).

set_item(Item) ->
    node_flat:set_item(Item).

get_item_name(Host, Node, ItemId) ->
    node_flat:get_item_name(Host, Node, ItemId).

node_to_path(Node) ->
    node_flat:node_to_path(Node).

path_to_node(Path) ->
    node_flat:path_to_node(Path).
