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
%%% Portions created by ProcessOne are Copyright 2006-2013, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2013, ProcessOne.
%%%
%%% @copyright 2006-2013 ProcessOne
%%% @author Karim Gemayel <karim.gemayel@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @headerfile "pubsub_dev.hrl"

-module(node_flat_dev).
-author('karim.gemayel@process-one.net').

-include("pubsub_dev.hrl").
-include("pubsub_api.hrl").
-include("logger.hrl").

-compile(export_all).



%%
-spec(capabilities/0 :: () -> #capabilities{}).

capabilities() ->
    #capabilities{plugin = ?MODULE}.


%%
-spec(pubsub_features/0 :: () -> Pubsub_Features::exmpp_pubsub:pubsub_features()).

pubsub_features() ->
    [<<"access-authorize">>,
     <<"access-open">>,
     <<"access-presence">>,
     <<"access-roster">>,
     <<"access-whitelist">>,
     <<"auto-create">>,
     <<"auto-subscribe">>,
    %<<"collections">>,
     <<"config-node">>,
     <<"create-and-configure">>,
     <<"create-nodes">>,
     <<"delete-items">>,
     <<"delete-nodes">>,
    %<<"filtered-notifications">>,
     <<"get-pending">>,
     <<"instant-nodes">>,
     <<"item-ids">>,
     <<"last-published">>,
     <<"leased-subscription">>,
     <<"manage-subscriptions">>,
     <<"member-affiliation">>,
     <<"meta-data">>,
     <<"modify-affiliations">>,
    %<<"multi-collections">>,
     <<"multi-subscribe">>,
     <<"outcast-affiliation">>,
     <<"persistent-items">>,
     <<"presence-notifications">>,
    %"presence-subscribe">>,
     <<"publish">>,
     <<"publish-only-affiliation">>,
     <<"publish-options">>,
     <<"publisher-affiliation">>,
     <<"purge-nodes">>,
     <<"retract-items">>,
     <<"retrieve-affiliations">>,
     <<"retrieve-default">>,
     <<"retrieve-default-sub">>,
     <<"retrieve-items">>,
     <<"retrieve-subscriptions">>,
     <<"subscribe">>,
     <<"subscription-notifications">>,
     <<"subscription-options">>].

%%
-spec(pubsub_features/1 ::
(
  Entity_Type :: 'local' | 'remote')
    -> Pubsub_Features::exmpp_pubsub:pubsub_features()
).

pubsub_features('local') ->
    [<<"access-authorize">>,
     <<"access-open">>,
     <<"access-presence">>,
     <<"access-roster">>,
     <<"access-whitelist">>,
     <<"auto-create">>,
     <<"auto-subscribe">>,
    %<<"collections">>,
     <<"config-node">>,
     <<"create-and-configure">>,
     <<"create-nodes">>,
     <<"delete-items">>,
     <<"delete-nodes">>,
    %<<"filtered-notifications">>,
     <<"get-pending">>,
     <<"instant-nodes">>,
     <<"item-ids">>,
     <<"last-published">>,
     <<"leased-subscription">>,
     <<"manage-subscriptions">>,
     <<"member-affiliation">>,
     <<"meta-data">>,
     <<"modify-affiliations">>,
    %<<"multi-collections">>,
     <<"multi-subscribe">>,
     <<"outcast-affiliation">>,
     <<"persistent-items">>,
     <<"presence-notifications">>,
    %<<"presence-subscribe">>,
     <<"publish">>,
     <<"publish-only-affiliation">>,
     <<"publish-options">>,
     <<"publisher-affiliation">>,
     <<"purge-nodes">>,
     <<"retract-items">>,
     <<"retrieve-affiliations">>,
     <<"retrieve-default">>,
     <<"retrieve-default-sub">>,
     <<"retrieve-items">>,
     <<"retrieve-subscriptions">>,
     <<"subscribe">>,
     <<"subscription-notifications">>,
     <<"subscription-options">>];
%%
pubsub_features('remote') ->
    [<<"access-authorize">>,
     <<"access-open">>,
     <<"access-presence">>,
     <<"access-roster">>,
     <<"access-whitelist">>,
     <<"auto-create">>,
     <<"auto-subscribe">>,
    %<<"collections">>,
     <<"config-node">>,
     <<"create-and-configure">>,
     <<"create-nodes">>,
     <<"delete-items">>,
     <<"delete-nodes">>,
    %<<"filtered-notifications">>,
     <<"get-pending">>,
     <<"instant-nodes">>,
     <<"item-ids">>,
     <<"last-published">>,
    %<<"leased-subscription">>,
     <<"manage-subscriptions">>,
     <<"member-affiliation">>,
     <<"meta-data">>,
     <<"modify-affiliations">>,
    %<<"multi-collections">>,
     <<"multi-subscribe">>,
     <<"outcast-affiliation">>,
     <<"persistent-items">>,
     <<"publish">>,
     <<"publish-only-affiliation">>,
     <<"publish-options">>,
     <<"publisher-affiliation">>,
     <<"purge-nodes">>,
     <<"retract-items">>,
     <<"retrieve-affiliations">>,
     <<"retrieve-default">>,
     <<"retrieve-default-sub">>,
     <<"retrieve-items">>,
     <<"retrieve-subscriptions">>,
     <<"subscribe">>,
     <<"subscription-notifications">>,
     <<"subscription-options">>].

%%
%%
-spec(node_options/1 ::
(
  Node_Type :: 'leaf' | 'collection')
    -> Node_Options :: pubsub_options:options_node_leaf()
                     | pubsub_options:options_node_collection()
).

node_options('leaf') ->
    [{'access_model', open},
     {'contact', []},
    %{'collection', []},
     {'deliver_notifications', true},
     {'deliver_payloads', true},
     {'description', undefined},
     {'item_expire', undefined},
     {'itemreply', publisher},
     {'language', <<"en">>},
     {'max_items', 2},
     {'max_payload_size', undefined},
     {'node_type', leaf},
     {'notification_type', normal},
     {'notify_config', true},
     {'notify_delete', true},
     {'notify_retract', true},
     {'notify_sub', true},
     {'persist_items', true},
     {'presence_based_delivery', true},
     {'publish_model', open},
     {'purge_offline', false},
     {'roster_groups_allowed', []},
     {'send_last_published_item', on_sub_and_presence},
     {'subscribe', true},
     {'tempsub', false},
     {'title', undefined},
     {'type', undefined}];
%%
node_options('collection') ->
    [].

item_options() ->
    [{'access_model', open},
     {'deliver_notifications', true},
     {'deliver_payloads', true},
     {'item_expire', undefined},
     {'itemreply', publisher},
%    {'max_payload_size', undefined},
     {'notification_type', normal},
     {'notify_config', true},
     {'notify_retract', true},
     {'persist_items', true},
     {'presence_based_delivery', true},
%    {'publish_model', open},
     {'purge_offline', false},
     {'roster_groups_allowed', []},
     {'send_last_published_item', on_sub_and_presence},
     {'type', undefined}].

%%
-spec(subscription_options/2 ::
(
  Entity_Type :: 'local' | 'remote',
  Node_Type   :: 'leaf' | 'collection')
    -> Subscription_Options :: [] | pubsub_options:options_subscription()
).

subscription_options('local', 'leaf') ->
    [];
subscription_options('remote', 'leaf') ->
    [];
subscription_options('local', 'collection') ->
    [];
subscription_options('remote', 'collection') ->
    [].

%%
-spec(default_subscription_options/2 ::
(
  Entity_Type :: 'local' | 'remote',
  Node_Type   :: 'leaf')%| 'collection')
    -> Subscription_Options :: pubsub_options:options_subscription_leaf()
).

default_subscription_options('local', 'leaf') ->
    [{'deliver', true},
     {'expire', undefined},
    %{'include_body', undefined},
     {'show-values', ['away', 'chat', 'dnd', 'online', 'xa']}];
%%
default_subscription_options('remote', 'leaf') ->
    [{'deliver', true}
    %{'include_body', undefined}
    ].
%%
%%default_subscription_options('local', 'collection') ->
%%    [{'deliver', true},
%%    %{'expire', undefined},
%%    %{'include_body', undefined},
%%     {'show-values', ['away', 'chat', 'dnd', 'online', 'xa']},
%%     {'subscription_type', 'all'},
%%     {'subscription_depth', 1}];
%%
%%default_subscription_options('remote', 'collection') ->
%%    [{'deliver', true},
%%    %{'include_body', undefined},
%%     {'show-values', ['away', 'chat', 'dnd', 'online', 'xa']},
%%     {'subscription_type', 'all'},
%%     {'subscription_depth', 1}].

%%
-spec(features/0 :: () -> Features::exmpp_pubsub:features()).

features() ->
    [%?NS_ADDRESS,
     ?NS_DISCO_INFO,
     ?NS_DISCO_ITEMS,
     %?NS_RSM,
     ?NS_VCARD].

%%
identity() ->
    [{<<"pubsub">>, <<"Publish-Subscribe">>, <<"service">>},
     {<<"pubsub">>, <<"ejabberd/mod_pubsub/flat">>, <<"service">>}].
