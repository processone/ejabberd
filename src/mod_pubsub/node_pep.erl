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
%%% Portions created by ProcessOne are Copyright 2006-2012, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2012, ProcessOne.
%%%
%%%
%%% @copyright 2006-2012 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @doc The module <strong>{@module}</strong> is the pep PubSub plugin.
%%% <p>PubSub plugin nodes are using the {@link gen_pubsub_node} behaviour.</p>

-module(node_pep).
-author('christophe.romain@process-one.net').

-include("ejabberd.hrl").
-include("pubsub.hrl").

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
	 path_to_node/1
	]).


-spec(init/3 ::
      (
	     Host       :: string(),
	     ServerHost :: string(),
	     Opts       :: [{Key::atom(), Value::term()}])
      -> 'ok'
	    ).

init(Host, ServerHost, Opts) ->
    node_flat:init(Host, ServerHost, Opts),
    complain_if_modcaps_disabled(ServerHost),
    ok.


-spec(terminate/2 ::
      (
		  Host       :: string(),
		  ServerHost :: string())
      -> 'ok'
	    ).

terminate(Host, ServerHost) ->
    node_flat:terminate(Host, ServerHost),
    ok.


-spec(options/0 :: () -> [nodeOption()]).

options() ->
    [{deliver_payloads, true},
     {notify_config, false},
     {notify_delete, false},
     {notify_retract, false},
     {notify_sub, false},
     {purge_offline, false},
     {persist_items, false},
     {max_items, ?MAXITEMS},
     {subscribe, true},
     {access_model, presence},
     {roster_groups_allowed, []},
     {publish_model, publishers},
     {notification_type, headline},
     {max_payload_size, ?MAX_PAYLOAD_SIZE},
     {send_last_published_item, on_sub_and_presence},
     {deliver_notifications, true},
     {presence_based_delivery, true}].


-spec(features/0 :: () -> [Feature::string()]).

features() ->
    ["create-nodes", %*
     "auto-create", %*
     "auto-subscribe", %*
     "delete-nodes", %*
     "delete-items", %*
     "filtered-notifications", %*
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


-spec(create_node_permission/6 ::
      (
			       Host         :: hostPEP(),
			       ServerHost   :: string(),
			       NodeId       :: nodeId(),
			       ParentNodeId :: nodeId(),
			       JID          :: jidEntity(),
			       Access       :: atom())
      -> {'result', IsAllowed::boolean()}
	    ).

create_node_permission(Host, ServerHost, _NodeId, _ParentNodeId, #jid{node = U, domain = S, resource = R} = JID, Access) ->
    Owner = {U,S,R},
    Allowed = case Owner of
	{undefined, Host, undefined} ->
	    true; % pubsub service always allowed
	_ ->
	    case acl:match_rule(ServerHost, Access, JID) of
		'allow' ->
		    case Host of 
			{_, _, _} -> true;
			_ -> false
		    end;
		Error ->
		    ?DEBUG("Create not allowed : ~p~n", [Error]),
		    false
	    end
    end,
    {result, Allowed}.


-spec(create_node/2 ::
      (
		    NodeIdx :: nodeIdx(),
		    JID     :: jidEntity())
      -> {'result', []}% | {'error', 'internal-server-error'}
	    ).

create_node(NodeIdx, JID) ->
    node_flat:create_node(NodeIdx, JID).
%    case node_flat:create_node(NodeIdx, JID) of
%	{result, _} -> {result, []};
%	Error -> Error
%    end.


-spec(delete_node/1 ::
      (
		    Nodes :: [Node::pubsubNode()])
      -> {result, {[],
		   Removed :: [] | [{Node :: pubsubNode(),
			      [{Owner         :: bareUsr(),
				Subscriptions :: [{Subscription :: subscription(),
						   SubId        :: subId()}]}]}]}}
			 %| {'error', 'internal-server-error'}
	    ).

delete_node(Nodes) ->
    {result, {_, _, Nodes}} = node_flat:delete_node(Nodes),
    {result, {[], Nodes}}.
%    case node_flat:delete_node(Nodes) of
%	{result, {_, _, Nodes}} -> {result, {[], Nodes}};
%	Error -> Error
%    end.


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
    node_flat:subscribe_node(
      NodeIdx, JID, Subscriber, AccessModel, SendLast,
      PresenceSubscription, RosterGroup, Options).


-spec(unsubscribe_node/4 ::
      (
			 NodeIdx    :: nodeIdx(),
			 JID        :: jidEntity(),
			 Subscriber :: jidEntity(),
			 SubId      :: subId())
      -> {'result', []} | {'error', _} %% TODO : add all error cases
	    ).

unsubscribe_node(NodeIdx, JID, Subscriber, SubId) ->
    case node_flat:unsubscribe_node(NodeIdx, JID, Subscriber, SubId) of
	{error, Error} -> {error, Error};
	{result, _} -> {result, []}
    end.


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
    node_flat:publish_item(NodeIdx, JID, PublishModel, MaxItems, ItemId, Payload).


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

get_entity_affiliations(_Host, #jid{node = U, domain = S} = _JID) ->
    States = mnesia:match_object(#pubsub_state{id = {_GenKey = {U,S,undefined}, '_'}, _ = '_'}),
    NodeTree = case catch ets:lookup(gen_mod:get_module_proc(S, 'config'), 'nodetree') of
	    [{'nodetree', Tree}] -> Tree;
	    _ -> 'nodetree_tree'
	end,
    Reply = lists:foldl(fun(#pubsub_state{id = {_, NodeIdx}, affiliation = Affiliation}, Acc) ->
	case NodeTree:get_node(NodeIdx) of
	    #pubsub_node{id = {{_, S, _}, _}} = Node -> [{Node, Affiliation}|Acc];
	    _ -> Acc
	end
    end, [], States),
    {result, Reply}.


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
				 Host :: hostPEP(),
				 JID  :: jidEntity())
      -> {'result', []
	  | [{Node         :: pubsubNode(),
	      Subscription :: subscription(),
	      SubId        :: subId(),
	      Entity       :: fullUsr()}]}
	    ).

get_entity_subscriptions(_Host, #jid{node = U, domain = S, resource = R} = _JID) ->
    SubKey = {U,S,R},
    GenKey = {U,S,undefined},
    States = case SubKey of
	GenKey -> mnesia:match_object(
	       #pubsub_state{id = {{U, S, '_'}, '_'}, _ = '_'});
	_ -> mnesia:match_object(
	       #pubsub_state{id = {GenKey, '_'}, _ = '_'})
	    ++ mnesia:match_object(
	       #pubsub_state{id = {SubKey, '_'}, _ = '_'})
    end,
    NodeTree = case catch ets:lookup(gen_mod:get_module_proc(S, 'config'), 'nodetree') of
	    [{nodetree, Tree}] -> Tree;
	    _ -> 'nodetree_tree'
	end,
    Reply = lists:foldl(fun(#pubsub_state{id = {Entity, NodeIdx}, subscriptions = Subscriptions}, Acc) ->
	case NodeTree:get_node(NodeIdx) of
	    #pubsub_node{id = {{_, S, _}, _}} = Node ->
			lists:foldl(fun({subscribed, SubId}, Acc2) ->
					   [{Node, subscribed, SubId, Entity} | Acc2];
					({pending, _SubId}, Acc2) ->
					    [{Node, pending, Entity} | Acc2];
					(Subscription, Acc2) ->
					    [{Node, Subscription, Entity} | Acc2]
				    end, Acc, Subscriptions);
	    _ -> Acc
	end
    end, [], States),
    {result, Reply}.


-spec(get_node_subscriptions/1 ::
      (
			       NodeIdx :: nodeIdx())
      -> {'result', []
	  | [{Entity::fullUsr(), 'none'}]
						%| [{Entity::fullUsr(), Subscription::subscription()}] %% still useful case ?
	  | [{Entity::fullUsr(), Subscription::subscription(), SubId::subId()}]}
	    ).

get_node_subscriptions(NodeIdx) ->
    %% note: get_node_subscriptions is used for broadcasting
    %% there should not have any subscriptions
    %% but that call returns also all subscription to none
    %% and this is required for broadcast to occurs
    %% DO NOT REMOVE
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
			  Host :: hostPEP(),
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
      -> 'ok' | {'error', 'internal-server-error'}
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

get_items(NodeIdx, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_flat:get_items(NodeIdx, JID, AccessModel, PresenceSubscription, RosterGroup, SubId).


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

get_item(NodeIdx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_flat:get_item(NodeIdx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId).


-spec(set_item/1 ::
      (
		 Item :: pubsubItem())
      -> 'ok' | {error, 'internal-server-error'}
	    ).

set_item(Item) ->
    node_flat:set_item(Item).


get_item_name(Host, Node, Id) ->
    node_flat:get_item_name(Host, Node, Id).


node_to_path(Node) ->
    node_flat:node_to_path(Node).


path_to_node(Path) ->
    node_flat:path_to_node(Path).


%%%
%%% Internal
%%%

%% @doc Check mod_caps is enabled, otherwise show warning.
%% The PEP plugin for mod_pubsub requires mod_caps to be enabled in the host.
%% Check that the mod_caps module is enabled in that Jabber Host
%% If not, show a warning message in the ejabberd log file.
-spec(complain_if_modcaps_disabled/1 ::
      (
		 ServerHost :: string())
      -> 'ok' | true
	    ).

complain_if_modcaps_disabled(ServerHost) ->
    Modules = ejabberd_config:get_local_option({modules, ServerHost}),
    ModCaps = [mod_caps_enabled || {mod_caps, _Opts} <- Modules],
    case ModCaps of
	[] ->
	    ?WARNING_MSG("The PEP plugin is enabled in mod_pubsub of host ~p. "
			 "This plugin requires mod_caps to be enabled, "
			 "but it isn't.", [ServerHost]);
	_ ->
	    ok
    end.
