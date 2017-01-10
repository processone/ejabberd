%%%----------------------------------------------------------------------
%%% File    : node_pep_sql.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Standard PubSub PEP plugin with ODBC backend
%%% Created :  1 Dec 2007 by Christophe Romain <christophe.romain@process-one.net>
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

%%% @doc The module <strong>{@module}</strong> is the pep PubSub plugin.
%%% <p>PubSub plugin nodes are using the {@link gen_pubsub_node} behaviour.</p>

-module(node_pep_sql).
-behaviour(gen_pubsub_node).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").
-include("logger.hrl").

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
    get_item/2, set_item/1, get_item_name/3, node_to_path/1,
    path_to_node/1, depends/3,
    get_entity_subscriptions_for_send_last/2, get_last_items/3]).

depends(_Host, _ServerHost, _Opts) ->
    [{mod_caps, hard}].

init(Host, ServerHost, Opts) ->
    node_flat_sql:init(Host, ServerHost, Opts),
    ok.

terminate(Host, ServerHost) ->
    node_flat_sql:terminate(Host, ServerHost),
    ok.

options() ->
    [{sql, true}, {rsm, true} | node_pep:options()].

features() ->
    [<<"rsm">> | node_pep:features()].

create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
    node_pep:create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access).

create_node(Nidx, Owner) ->
    node_flat_sql:create_node(Nidx, Owner),
    {result, {default, broadcast}}.

delete_node(Nodes) ->
    {result, {_, _, Result}} = node_flat_sql:delete_node(Nodes),
    {result, {[], Result}}.

subscribe_node(Nidx, Sender, Subscriber, AccessModel,
	    SendLast, PresenceSubscription, RosterGroup, Options) ->
    node_flat_sql:subscribe_node(Nidx, Sender, Subscriber, AccessModel, SendLast,
	PresenceSubscription, RosterGroup, Options).


unsubscribe_node(Nidx, Sender, Subscriber, SubId) ->
    case node_flat_sql:unsubscribe_node(Nidx, Sender, Subscriber, SubId) of
	{error, Error} -> {error, Error};
	{result, _} -> {result, []}
    end.

publish_item(Nidx, Publisher, Model, MaxItems, ItemId, Payload, PubOpts) ->
    node_flat_sql:publish_item(Nidx, Publisher, Model, MaxItems, ItemId,
	Payload, PubOpts).

remove_extra_items(Nidx, MaxItems, ItemIds) ->
    node_flat_sql:remove_extra_items(Nidx, MaxItems, ItemIds).

delete_item(Nidx, Publisher, PublishModel, ItemId) ->
    node_flat_sql:delete_item(Nidx, Publisher, PublishModel, ItemId).

purge_node(Nidx, Owner) ->
    node_flat_sql:purge_node(Nidx, Owner).

get_entity_affiliations(_Host, Owner) ->
    OwnerKey = jid:tolower(jid:remove_resource(Owner)),
    node_flat_sql:get_entity_affiliations(OwnerKey, Owner).

get_node_affiliations(Nidx) ->
    node_flat_sql:get_node_affiliations(Nidx).

get_affiliation(Nidx, Owner) ->
    node_flat_sql:get_affiliation(Nidx, Owner).

set_affiliation(Nidx, Owner, Affiliation) ->
    node_flat_sql:set_affiliation(Nidx, Owner, Affiliation).

get_entity_subscriptions(_Host, Owner) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    HostLike = node_flat_sql:encode_host_like(element(2, SubKey)),
    SJ = ejabberd_sql:escape(node_flat_sql:encode_jid(SubKey)),
    GJ = ejabberd_sql:escape(node_flat_sql:encode_jid(GenKey)),
    GJLike = ejabberd_sql:escape(node_flat_sql:encode_jid_like(GenKey)),
    Query = case SubKey of
	GenKey ->
	    [<<"select host, node, type, i.nodeid, jid, "
		    "subscriptions from pubsub_state i, pubsub_node n "
		    "where i.nodeid = n.nodeid and jid "
		    "like '">>, GJLike, <<"%' escape '^' and host like '%@">>, HostLike, <<"' escape '^';">>];
	_ ->
	    [<<"select host, node, type, i.nodeid, jid, "
		    "subscriptions from pubsub_state i, pubsub_node n "
		    "where i.nodeid = n.nodeid and jid "
		    "in ('">>, SJ, <<"', '">>, GJ, <<"') and host like '%@">>, HostLike, <<"' escape '^';">>]
    end,
    Reply = case catch ejabberd_sql:sql_query_t(Query) of
	{selected,
		    [<<"host">>, <<"node">>, <<"type">>, <<"nodeid">>, <<"jid">>, <<"subscriptions">>],
		    RItems} ->
	    lists:map(fun ([H, N, T, I, J, S]) ->
			O = node_flat_sql:decode_jid(H),
			Node = nodetree_tree_sql:raw_to_node(O, [N, <<"">>, T, I]),
			{Node,
			    node_flat_sql:decode_subscriptions(S),
			    node_flat_sql:decode_jid(J)}
		end,
		RItems);
	_ ->
	    []
    end,
    {result, Reply}.

get_entity_subscriptions_for_send_last(_Host, Owner) ->
    SubKey = jid:tolower(Owner),
    GenKey = jid:remove_resource(SubKey),
    HostLike = node_flat_sql:encode_host_like(element(2, SubKey)),
    SJ = ejabberd_sql:escape(node_flat_sql:encode_jid(SubKey)),
    GJ = ejabberd_sql:escape(node_flat_sql:encode_jid(GenKey)),
    GJLike = ejabberd_sql:escape(node_flat_sql:encode_jid_like(GenKey)),
    Query = case SubKey of
	GenKey ->
	    [<<"select host, node, type, i.nodeid, jid, "
		    "subscriptions from pubsub_state i, pubsub_node n, "
		    "pubsub_node_option o where i.nodeid = n.nodeid "
		    "and n.nodeid = o.nodeid and name='send_last_published_item' and "
		    "val='on_sub_and_presence' and jid like '">>,
		GJLike, <<"%' escape '^' and host like '%@">>, HostLike, <<"' escape '^';">>];
	_ ->
	    [<<"select host, node, type, i.nodeid, jid, "
		    "subscriptions from pubsub_state i, pubsub_node n, "
		    "pubsub_node_option o where i.nodeid = n.nodeid "
		    "and n.nodeid = o.nodeid and name='send_last_published_item' and "
		    "val='on_sub_and_presence' and jid in ",
		    "('">>, SJ, <<"', '">>, GJ, <<"') and host like '%@">>, HostLike, <<"' escape '^';">>]
    end,
    Reply = case catch ejabberd_sql:sql_query_t(Query) of
	{selected,
		    [<<"host">>, <<"node">>, <<"type">>, <<"nodeid">>, <<"jid">>, <<"subscriptions">>],
		    RItems} ->
	    lists:map(fun ([H, N, T, I, J, S]) ->
			O = node_flat_sql:decode_jid(H),
			Node = nodetree_tree_sql:raw_to_node(O, [N, <<"">>, T, I]),
			{Node,
			    node_flat_sql:decode_subscriptions(S),
			    node_flat_sql:decode_jid(J)}
		end,
		RItems);
	_ ->
	    []
    end,
    {result, Reply}.

get_node_subscriptions(Nidx) ->
    node_flat_sql:get_node_subscriptions(Nidx).

get_subscriptions(Nidx, Owner) ->
    node_flat_sql:get_subscriptions(Nidx, Owner).

set_subscriptions(Nidx, Owner, Subscription, SubId) ->
    node_flat_sql:set_subscriptions(Nidx, Owner, Subscription, SubId).

get_pending_nodes(Host, Owner) ->
    node_flat_sql:get_pending_nodes(Host, Owner).

get_states(Nidx) ->
    node_flat_sql:get_states(Nidx).

get_state(Nidx, JID) ->
    node_flat_sql:get_state(Nidx, JID).

set_state(State) ->
    node_flat_sql:set_state(State).

get_items(Nidx, From, RSM) ->
    node_flat_sql:get_items(Nidx, From, RSM).

get_items(Nidx, JID, AccessModel, PresenceSubscription, RosterGroup, SubId, RSM) ->
    node_flat_sql:get_items(Nidx, JID, AccessModel,
	PresenceSubscription, RosterGroup, SubId, RSM).

get_last_items(Nidx, JID, Count) ->
    node_flat_sql:get_last_items(Nidx, JID, Count).

get_item(Nidx, ItemId) ->
    node_flat_sql:get_item(Nidx, ItemId).

get_item(Nidx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_flat_sql:get_item(Nidx, ItemId, JID, AccessModel,
	PresenceSubscription, RosterGroup, SubId).

set_item(Item) ->
    node_flat_sql:set_item(Item).

get_item_name(Host, Node, Id) ->
    node_flat_sql:get_item_name(Host, Node, Id).

node_to_path(Node) ->
    node_flat_sql:node_to_path(Node).

path_to_node(Path) ->
    node_flat_sql:path_to_node(Path).
