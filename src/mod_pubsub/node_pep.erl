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
%%% Portions created by ProcessOne are Copyright 2006-2008, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2008, ProcessOne.
%%%
%%%
%%% @copyright 2006-2008 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @doc The module <strong>{@module}</strong> is the pep PubSub plugin.
%%% <p>PubSub plugin nodes are using the {@link gen_pubsub_node} behaviour.</p>

-module(node_pep).
-author('christophe.romain@process-one.net').

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("pubsub.hrl").

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
	 get_items/7,
	 get_items/3,
	 get_item/8,
	 get_item/3,
	 set_item/1,
	 get_item_name/3
	]).

init(Host, ServerHost, Opts) ->
    node_default:init(Host, ServerHost, Opts),
    complain_if_modcaps_disabled(ServerHost),
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
    {User, Server, _Resource} = LOwner, 
    Allowed = case LOwner of
	{undefined, Host, undefined} ->
	    true; % pubsub service always allowed
	_ ->
	    case acl:match_rule(ServerHost, Access, LOwner) of
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
    OwnerKey = jlib:short_prepd_bare_jid(Owner),
    node_default:get_entity_affiliations(OwnerKey, Owner).

get_node_affiliations(Host, Node) ->
    OwnerKey = jlib:short_bare_jid(Host),
    node_default:get_node_affiliations(OwnerKey, Node).

get_affiliation(_Host, Node, Owner) ->
    OwnerKey = jlib:short_prepd_bare_jid(Owner),
    node_default:get_affiliation(OwnerKey, Node, Owner).

set_affiliation(_Host, Node, Owner, Affiliation) ->
    OwnerKey = jlib:short_prepd_bare_jid(Owner),
    Record = case get_state(OwnerKey, Node, OwnerKey) of
	    {error, 'item-not-found'} ->
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
    {result, none}.

set_subscription(_Host, _Node, _Owner, _Subscription) ->
    ok.

get_states(Host, Node) ->
    node_default:get_states(Host, Node).

get_state(Host, Node, JID) ->
    node_default:get_state(Host, Node, JID).

set_state(State) ->
    node_default:set_state(State).

get_items(Host, Node, From) ->
    node_default:get_items(Host, Node, From).

get_items(Host, Node, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_default:get_items(Host, Node, JID, AccessModel, PresenceSubscription, RosterGroup, SubId).

get_item(Host, Node, ItemId) ->
    node_default:get_item(Host, Node, ItemId).

get_item(Host, Node, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_default:get_item(Host, Node, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId).

set_item(Item) ->
    node_default:set_item(Item).

get_item_name(Host, Node, Id) ->
    node_default:get_item_name(Host, Node, Id).


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

