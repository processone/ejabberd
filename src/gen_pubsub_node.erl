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
%%% Portions created by ProcessOne are Copyright 2006-2015, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2015, ProcessOne.
%%%
%%%
%%% @copyright 2006-2015 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @private
%%% @doc <p>The module <strong>{@module}</strong> defines the PubSub node
%%% plugin behaviour. This behaviour is used to check that a PubSub plugin
%%% respects the current ejabberd PubSub plugin API.</p>

-module(gen_pubsub_node).

-include("jlib.hrl").

-type(host() :: mod_pubsub:host()
              | mod_pubsub_odbc:host()
).

-type(nodeId() :: mod_pubsub:nodeId()
                | mod_pubsub_odbc:nodeId()
).

-type(nodeIdx() :: mod_pubsub:nodeIdx()
                | mod_pubsub_odbc:nodeIdx()
).

-type(itemId() :: mod_pubsub:itemId()
                | mod_pubsub_odbc:itemId()
).

-type(pubsubNode() :: mod_pubsub:pubsubNode()
                    | mod_pubsub_odbc:pubsubNode()
).

-type(pubsubState() :: mod_pubsub:pubsubState()
                     | mod_pubsub_odbc:pubsubState()
).

-type(pubsubItem() :: mod_pubsub:pubsubItem()
                    | mod_pubsub_odbc:pubsubItem()
).

-type(nodeOptions() :: mod_pubsub:nodeOptions()
                     | mod_pubsub_odbc:nodeOptions()
).

-type(subOptions() :: mod_pubsub:subOptions()
                    | mod_pubsub_odbc:subOptions()
).

-type(affiliation() :: mod_pubsub:affiliation()
                     | mod_pubsub_odbc:affiliation()
).

-type(subscription() :: mod_pubsub:subscription()
                      | mod_pubsub_odbc:subscription()
).

-type(subId() :: mod_pubsub:subId()
               | mod_pubsub_odbc:subId()
).

-type(accessModel() :: mod_pubsub:accessModel()
                     | mod_pubsub_odbc:accessModel()
).

-type(publishModel() :: mod_pubsub:publishModel()
                     | mod_pubsub_odbc:publishModel()
).

-type(payload() :: mod_pubsub:payload()
                 | mod_pubsub_odbc:payload()
).

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
    {error, xmlel()}.

-callback subscribe_node(NodeIdx :: nodeIdx(),
                         Sender :: jid(),
                         Subscriber :: ljid(),
                         AccessModel :: accessModel(),
                         SendLast :: 'never' | 'on_sub' | 'on_sub_and_presence',
                         PresenceSubscription :: boolean(),
                         RosterGroup :: boolean(),
                         Options :: subOptions()) ->
    {result, {default, subscribed, subId()}} |
    {result, {default, subscribed, subId(), send_last}} |
    {result, {default, pending, subId()}} |
    {error, xmlel()}.

-callback unsubscribe_node(NodeIdx :: nodeIdx(),
                           Sender :: jid(),
                           Subscriber :: ljid(),
                           SubId :: subId()) ->
    {result, default} |
    {error, xmlel()}.

-callback publish_item(NodeId :: nodeIdx(),
                       Publisher :: jid(),
                       PublishModel :: publishModel(),
                       Max_Items :: non_neg_integer(),
                       ItemId :: <<>> | itemId(),
                       Payload :: payload()) ->
    {result, {default, broadcast, [itemId()]}} |
    {error, xmlel()}.

-callback delete_item(NodeIdx :: nodeIdx(),
                      Publisher :: jid(),
                      PublishModel :: publishModel(),
                      ItemId :: <<>> | itemId()) ->
    {result, {default, broadcast}} |
    {error, xmlel()}.

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
                          Owner :: ljid(),
                          Affiliation :: affiliation()) ->
    ok |
    {error, xmlel()}.

-callback get_node_subscriptions(NodeIdx :: nodeIdx()) ->
    {result,
     [{ljid(), subscription(), subId()}] |
     [{ljid(), none},...]
    }.

-callback get_entity_subscriptions(Host :: host(),
                                   Owner :: jid()) ->
    {result, [{pubsubNode(), subscription(), subId(), ljid()}]
    }.

-callback get_subscriptions(NodeIdx :: nodeIdx(),
                            Owner :: ljid()) ->
    {result, [{subscription(), subId()}]}.

-callback get_pending_nodes(Host :: host(),
                            Owner :: jid()) ->
    {result, [nodeId()]}.

-callback get_states(NodeIdx::nodeIdx()) ->
    {result, [pubsubState()]}.

-callback get_state(NodeIdx :: nodeIdx(),
                    JID :: ljid()) ->
    pubsubState().

-callback set_state(State::pubsubState()) ->
    ok |
    {error, xmlel()}.

-callback get_items(NodeIdx :: nodeIdx(),
                    JID :: jid(),
                    AccessModel :: accessModel(),
                    Presence_Subscription :: boolean(),
                    RosterGroup :: boolean(),
                    SubId :: subId()) ->
    {result, [pubsubItem()]} |
    {error, xmlel()}.

-callback get_items(NodeIdx :: nodeIdx(),
                    From :: jid()) ->
    {result, [pubsubItem()]}.

-callback get_item(NodeIdx :: nodeIdx(),
                   ItemId :: itemId(),
                   JID :: jid(),
                   AccessModel :: accessModel(),
                   PresenceSubscription :: boolean(),
                   RosterGroup :: boolean(),
                   SubId :: subId()) ->
    {result, pubsubItem()} |
    {error, xmlel()}.

-callback get_item(NodeIdx :: nodeIdx(),
                   ItemId :: itemId()) ->
    {result, pubsubItem()} |
    {error, xmlel()}.

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
