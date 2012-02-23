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
%%TODO : fix SQL requests with nodeid/id and id/idx changes
-module(node_pep_odbc).
-author('christophe.romain@process-one.net').

-include("ejabberd.hrl").
-include("pubsub.hrl").

-define(PUBSUB, mod_pubsub_odbc).
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
	 get_entity_subscriptions_for_send_last/2,
	 get_node_subscriptions/1,
	 get_subscriptions/2,
	 set_subscriptions/4,
	 get_pending_nodes/2,
	 get_states/1,
	 get_state/2,
	 set_state/1,
	 get_items/7,
	 get_items/6,
	 get_items/3,
	 get_items/2,
	 get_item/7,
	 get_item/2,
	 set_item/1,
	 get_item_name/3,
	 get_last_items/3,
	 node_to_path/1,
	 path_to_node/1
	]).

init(Host, ServerHost, Opts) ->
    node_flat_odbc:init(Host, ServerHost, Opts),
    complain_if_modcaps_disabled(ServerHost),
    ok.

terminate(Host, ServerHost) ->
    node_flat_odbc:terminate(Host, ServerHost),
    ok.

options() ->
    [{odbc, true},
     {deliver_payloads, true},
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

create_node_permission(Host, ServerHost, _Node, _ParentNode, Owner, Access) ->
    LOwner = jlib:short_prepd_jid(Owner),
    {User, Server, Resource} = LOwner,
    Allowed = case LOwner of
	{undefined, Host, undefined} ->
	    true; % pubsub service always allowed
	_ ->
	    JID = exmpp_jid:make(User, Server, Resource),
	    case acl:match_rule(ServerHost, Access, JID) of
		allow ->
		    case Host of 
			{User, Server, _} -> true;
			_ -> false
		    end;
		E ->
		    ?DEBUG("Create not allowed : ~p~n", [E]),
		    false
	    end
    end,
    {result, Allowed}.

create_node(NodeId, Owner) ->
    case node_flat_odbc:create_node(NodeId, Owner) of
	{result, _} -> {result, []};
	Error -> Error
    end.

delete_node(Removed) ->
    case node_flat_odbc:delete_node(Removed) of
	{result, {_, _, Removed}} -> {result, {[], Removed}};
	Error -> Error
    end.

subscribe_node(NodeId, Sender, Subscriber, AccessModel,
	       SendLast, PresenceSubscription, RosterGroup, Options) ->
    node_flat_odbc:subscribe_node(
      NodeId, Sender, Subscriber, AccessModel, SendLast,
      PresenceSubscription, RosterGroup, Options).

unsubscribe_node(NodeId, Sender, Subscriber, SubId) ->
    case node_flat_odbc:unsubscribe_node(NodeId, Sender, Subscriber, SubId) of
	{error, Error} -> {error, Error};
	{result, _} -> {result, []}
    end.

publish_item(NodeId, Publisher, Model, MaxItems, ItemId, Payload) ->
    node_flat_odbc:publish_item(NodeId, Publisher, Model, MaxItems, ItemId, Payload).

remove_extra_items(NodeId, MaxItems, ItemIds) ->
    node_flat_odbc:remove_extra_items(NodeId, MaxItems, ItemIds).

delete_item(NodeId, Publisher, PublishModel, ItemId) ->
    node_flat_odbc:delete_item(NodeId, Publisher, PublishModel, ItemId).

purge_node(NodeId, Owner) ->
    node_flat_odbc:purge_node(NodeId, Owner).

get_entity_affiliations(_Host, Owner) ->
    OwnerKey = jlib:short_prepd_bare_jid(Owner),
    node_flat_odbc:get_entity_affiliations(OwnerKey, Owner).

get_node_affiliations(NodeId) ->
    node_flat_odbc:get_node_affiliations(NodeId).

get_affiliation(NodeId, Owner) ->
    node_flat_odbc:get_affiliation(NodeId, Owner).

set_affiliation(NodeId, Owner, Affiliation) ->
    node_flat_odbc:set_affiliation(NodeId, Owner, Affiliation).

get_entity_subscriptions(_Host, Owner) ->
    SubKey = jlib:short_prepd_jid(Owner),
    GenKey = jlib:short_prepd_bare_jid(SubKey),
    Host = ?PUBSUB:escape(element(2, SubKey)),
    SJ = node_flat_odbc:encode_jid(SubKey),
    GJ = node_flat_odbc:encode_jid(GenKey),
    Query = case SubKey of
	GenKey ->
	    ["select host, node, type, i.nodeid, jid, subscription "
	     "from pubsub_state i, pubsub_node n "
	     "where i.nodeid = n.nodeid "
	     "and jid like '", GJ, "%' "
	     "and host like '%@", Host, "';"];
	_ ->
	    ["select host, node, type, i.nodeid, jid, subscription "
	     "from pubsub_state i, pubsub_node n "
	     "where i.nodeid = n.nodeid "
	     "and jid in ('", SJ, "', '", GJ, "') "
	     "and host like '%@", Host, "';"]
    end,
    Reply = case catch ejabberd_odbc:sql_query_t(Query) of
	{selected, ["host", "node", "type", "nodeid", "jid", "subscription"], RItems} ->
	    lists:map(fun({H, N, T, I, J, S}) ->
		O = node_flat_odbc:decode_jid(H),
		Node = nodetree_tree_odbc:raw_to_node(O, {N, "", T, I}),
		{Node, node_flat_odbc:decode_subscription(S), node_flat_odbc:decode_jid(J)}
	    end, RItems);
	_ ->
	    []
	end,
    {result, Reply}.
get_entity_subscriptions_for_send_last(_Host, Owner) ->
    SubKey = jlib:short_prepd_jid(Owner),
    GenKey = jlib:short_prepd_bare_jid(SubKey),
    Host = ?PUBSUB:escape(element(2, SubKey)),
    SJ = node_flat_odbc:encode_jid(SubKey),
    GJ = node_flat_odbc:encode_jid(GenKey),
    Query = case SubKey of
	GenKey ->
	    ["select host, node, type, i.nodeid, jid, subscription "
	     "from pubsub_state i, pubsub_node n, pubsub_node_option o "
	     "where i.nodeid = n.nodeid and n.nodeid = o.nodeid "
	     "and name='send_last_published_item' and val='on_sub_and_presence' "
	     "and jid like '", GJ, "%' "
	     "and host like '%@", Host, "';"];
	_ ->
	    ["select host, node, type, i.nodeid, jid, subscription "
	     "from pubsub_state i, pubsub_node n, pubsub_node_option o "
	     "where i.nodeid = n.nodeid and n.nodeid = o.nodeid "
	     "and name='send_last_published_item' and val='on_sub_and_presence' "
	     "and jid in ('", SJ, "', '", GJ, "') "
	     "and host like '%@", Host, "';"]
    end,
    Reply = case catch ejabberd_odbc:sql_query_t(Query) of
	{selected, ["host", "node", "type", "nodeid", "jid", "subscription"], RItems} ->
	    lists:map(fun({H, N, T, I, J, S}) ->
		O = node_flat_odbc:decode_jid(H),
		Node = nodetree_tree_odbc:raw_to_node(O, {N, "", T, I}),
		{Node, node_flat_odbc:decode_subscription(S), node_flat_odbc:decode_jid(J)}
	    end, RItems);
	_ ->
	    []
	end,
    {result, Reply}.


get_node_subscriptions(NodeId) ->
    %% note: get_node_subscriptions is used for broadcasting
    %% there should not have any subscriptions
    %% but that call returns also all subscription to none
    %% and this is required for broadcast to occurs
    %% DO NOT REMOVE
    node_flat_odbc:get_node_subscriptions(NodeId).

get_subscriptions(NodeId, Owner) ->
    node_flat_odbc:get_subscriptions(NodeId, Owner).

set_subscriptions(NodeId, Owner, Subscription, SubId) ->
    node_flat_odbc:set_subscriptions(NodeId, Owner, Subscription, SubId).

get_pending_nodes(Host, Owner) ->
    node_flat_odbc:get_pending_nodes(Host, Owner).

get_states(NodeId) ->
    node_flat_odbc:get_states(NodeId).

get_state(NodeId, JID) ->
    node_flat_odbc:get_state(NodeId, JID).

set_state(State) ->
    node_flat_odbc:set_state(State).

get_items(NodeId, From) ->
    node_flat_odbc:get_items(NodeId, From).
get_items(NodeId, From, RSM) ->
    node_flat_odbc:get_items(NodeId, From, RSM).
get_items(NodeId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    get_items(NodeId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId, none).
get_items(NodeId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId, RSM) ->
    node_flat_odbc:get_items(NodeId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId, RSM).

get_last_items(NodeId, JID, Count) ->
    node_flat_odbc:get_last_items(NodeId, JID, Count).

get_item(NodeId, ItemId) ->
    node_flat_odbc:get_item(NodeId, ItemId).

get_item(NodeId, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_flat_odbc:get_item(NodeId, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId).

set_item(Item) ->
    node_flat_odbc:set_item(Item).

get_item_name(Host, Node, Id) ->
    node_flat_odbc:get_item_name(Host, Node, Id).

node_to_path(Node) ->
    node_flat_odbc:node_to_path(Node).

path_to_node(Path) ->
    node_flat_odbc:path_to_node(Path).

%%%
%%% Internal
%%%

%% @doc Check mod_caps is enabled, otherwise show warning.
%% The PEP plugin for mod_pubsub requires mod_caps to be enabled in the host.
%% Check that the mod_caps module is enabled in that Jabber Host
%% If not, show a warning message in the ejabberd log file.
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
