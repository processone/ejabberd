%%%----------------------------------------------------------------------
%%% File    : gen_pubsub_node.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Define pubsub plugin behaviour
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

-module(gen_pubsub_node).

-include("xmpp.hrl").

-type(host() :: mod_pubsub:host()).
-type(nodeId() :: mod_pubsub:nodeId()).
-type(nodeIdx() :: mod_pubsub:nodeIdx()).
-type(itemId() :: mod_pubsub:itemId()).
-type(pubsubNode() :: mod_pubsub:pubsubNode()).
-type(pubsubState() :: mod_pubsub:pubsubState()).
-type(pubsubItem() :: mod_pubsub:pubsubItem()).
-type(subOptions() :: mod_pubsub:subOptions()).
-type(pubOptions() :: mod_pubsub:pubOptions()).
-type(affiliation() :: mod_pubsub:affiliation()).
-type(subscription() :: mod_pubsub:subscription()).
-type(subId() :: mod_pubsub:subId()).
-type(accessModel() :: mod_pubsub:accessModel()).
-type(publishModel() :: mod_pubsub:publishModel()).
-type(payload() :: mod_pubsub:payload()).

-callback init(Host :: binary(),
	ServerHost :: binary(),
	Opts :: [any()]) -> atom().

-callback terminate(Host :: host(),
	ServerHost :: binary()) -> atom().

-callback options() -> [{atom(), any()}].

-callback features() -> [binary()].

-callback create_node_permission(Host :: host(),
	ServerHost :: binary(),
	Node :: nodeId(),
	ParentNode :: nodeId(),
	Owner :: jid(), Access :: atom()) ->
    {result, boolean()}.

-callback create_node(NodeIdx :: nodeIdx(),
	Owner   :: jid()) ->
    {result, {default, broadcast}}.

-callback delete_node(Nodes :: [pubsubNode(),...]) ->
    {result,
	{default, broadcast,
	    [{pubsubNode(),
		    [{ljid(), [{subscription(), subId()}]},...]},...]
	    }
	}
    |
    {result,
	{[],
	    [{pubsubNode(),
		    [{ljid(), [{subscription(), subId()}]},...]},...]
	    }
	}.

-callback purge_node(NodeIdx :: nodeIdx(),
	Owner :: jid()) ->
    {result, {default, broadcast}} |
    {error, stanza_error()}.

-callback subscribe_node(NodeIdx :: nodeIdx(),
	Sender :: jid(),
	Subscriber :: jid(),
	AccessModel :: accessModel(),
	SendLast :: 'never' | 'on_sub' | 'on_sub_and_presence',
	PresenceSubscription :: boolean(),
	RosterGroup :: boolean(),
	Options :: subOptions()) ->
    {result, {default, subscribed, subId()}} |
    {result, {default, subscribed, subId(), send_last}} |
    {result, {default, pending, subId()}} |
    {error, stanza_error()}.

-callback unsubscribe_node(NodeIdx :: nodeIdx(),
	Sender :: jid(),
	Subscriber :: jid(),
	SubId :: subId()) ->
    {result, []} |
    {error, stanza_error()}.

-callback publish_item(NodeId :: nodeIdx(),
	Publisher :: jid(),
	PublishModel :: publishModel(),
	Max_Items :: non_neg_integer(),
	ItemId :: <<>> | itemId(),
	Payload :: payload(),
	Options :: pubOptions()) ->
    {result, {default, broadcast, [itemId()]}} |
    {error, stanza_error()}.

-callback delete_item(NodeIdx :: nodeIdx(),
	Publisher :: jid(),
	PublishModel :: publishModel(),
	ItemId :: <<>> | itemId()) ->
    {result, {default, broadcast}} |
    {error, stanza_error()}.

-callback remove_extra_items(NodeIdx :: nodeIdx(),
	Max_Items :: unlimited | non_neg_integer(),
	ItemIds :: [itemId()]) ->
    {result, {[itemId()], [itemId()]}
	}.

-callback get_node_affiliations(NodeIdx :: nodeIdx()) ->
    {result, [{ljid(), affiliation()}]}.

-callback get_entity_affiliations(Host :: host(),
	Owner :: jid()) ->
    {result, [{pubsubNode(), affiliation()}]}.

-callback get_affiliation(NodeIdx :: nodeIdx(),
	Owner :: jid()) ->
    {result, affiliation()}.

-callback set_affiliation(NodeIdx :: nodeIdx(),
	Owner :: jid(),
	Affiliation :: affiliation()) ->
    ok |
    {error, stanza_error()}.

-callback get_node_subscriptions(NodeIdx :: nodeIdx()) ->
    {result,
	[{ljid(), subscription(), subId()}] |
	[{ljid(), none},...]
	}.

-callback get_entity_subscriptions(Host :: host(),
	Key :: jid()) ->
    {result, [{pubsubNode(), subscription(), subId(), ljid()}]
	}.

-callback get_subscriptions(NodeIdx :: nodeIdx(),
	Owner :: jid()) ->
    {result, [{subscription(), subId()}]}.

-callback get_pending_nodes(Host :: host(),
	Owner :: jid()) ->
    {result, [nodeId()]}.

-callback get_states(NodeIdx::nodeIdx()) ->
    {result, [pubsubState()]}.

-callback get_state(NodeIdx :: nodeIdx(),
	Key :: ljid()) ->
    pubsubState().

-callback set_state(State::pubsubState()) ->
    ok |
    {error, stanza_error()}.

-callback get_items(nodeIdx(), jid(), accessModel(),
		    boolean(), boolean(), binary(),
		    undefined | rsm_set()) ->
    {result, {[pubsubItem()], undefined | rsm_set()}} | {error, stanza_error()}.

-callback get_items(nodeIdx(), jid(), undefined | rsm_set()) ->
    {result, {[pubsubItem()], undefined | rsm_set()}}.

-callback get_last_items(nodeIdx(), jid(), undefined | rsm_set()) ->
    {result, {[pubsubItem()], undefined | rsm_set()}}.

-callback get_item(NodeIdx :: nodeIdx(),
	ItemId :: itemId(),
	JID :: jid(),
	AccessModel :: accessModel(),
	PresenceSubscription :: boolean(),
	RosterGroup :: boolean(),
	SubId :: subId()) ->
    {result, pubsubItem()} |
    {error, stanza_error()}.

-callback get_item(NodeIdx :: nodeIdx(),
	ItemId :: itemId()) ->
    {result, pubsubItem()} |
    {error, stanza_error()}.

-callback set_item(Item :: pubsubItem()) ->
    ok.
%   | {error, _}.

-callback get_item_name(Host :: host(),
	ServerHost :: binary(),
	Node :: nodeId()) ->
    itemId().

-callback node_to_path(Node :: nodeId()) ->
    [nodeId()].

-callback path_to_node(Node :: [nodeId()]) ->
    nodeId().
