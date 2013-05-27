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

-module(pubsub_options).
-author('karim.gemayel@process-one.net').

-compile(export_all).

-include("pubsub_dev.hrl").

-import(xml,
[
  get_tag_cdata/1,
  get_tag_attr_s/2,
  remove_cdata/1
]).


-import(pubsub_tools,
[
  get_entity_roster/1,
  is_jid/1
]).


%% --------------------------------------------------------------------
%% Documentation / type definitions.
%% --------------------------------------------------------------------

%% Node Options
%  pubsub#node_config FORM_TYPE

% Defined options values

%-- Access Model --%
-export_type([
  access_model/0,
  access_model_open/0,
  access_model_presence/0,
  access_model_roster/0,
  access_model_authorize/0,
  access_model_whitelist/0
]).

%% @type access_model_open() = 'open'.
-type(access_model_open() :: 'open').

%% @type access_model_presence() = 'presence'.
-type(access_model_presence() :: 'presence').

%% @type access_model_roster() = 'roster'.
-type(access_model_roster() :: 'roster').

%% @type access_model_authorize() = 'authorize'.
-type(access_model_authorize() :: 'authorize').

%% @type access_model_whitelist() = 'whitelist'.
-type(access_model_whitelist() :: 'whitelist').

%% @type access_model() = Open | Presence | Roster | Authorize | Whitelist
%%    Open = pubsub_options:access_model_open()
%%    Presence = pubsub_options:access_model_presence()
%%    Roster = pubsub_options:access_model_roster()
%%    Authorize = pubsub_options:access_model_authorize()
%%    Whitelist = pubsub_options:access_model_whitelist().
-type(access_model() :: pubsub_options:access_model_open()
                      | pubsub_options:access_model_presence()
                      | pubsub_options:access_model_roster()
                      | pubsub_options:access_model_authorize()
                      | pubsub_options:access_model_whitelist()
).

%%

%-- Children_Association_Policy --%
-export_type([
  children_association_policy/0,
  %%
  children_association_policy_all/0,
  children_association_policy_owners/0,
  children_association_policy_whitelist/0
]).

%% @type children_association_policy_all() = 'all'.
-type(children_association_policy_all() :: 'all').

%% @type children_association_policy_owners() = 'owners'.
-type(children_association_policy_owners() :: 'owners').

%% @type children_association_policy_whitelist() = 'whitelist'.
-type(children_association_policy_whitelist() :: 'whitelist').

%% @type children_association_policy() = All | Owners | Whitelist
%%    All = pubsub_options:children_association_policy_all()
%%    Owners = pubsub_options:children_association_policy_owners()
%%    Whitelist = pubsub_options:children_association_policy_whitelist().
-type(children_association_policy()
  :: pubsub_options:children_association_policy_all()
   | pubsub_options:children_association_policy_owners()
   | pubsub_options:children_association_policy_whitelist()
).

%%

%-- ItemReply -- %%
-export_type([
  item_reply/0,
  item_reply_owner/0,
  item_reply_publisher/0
]).

%% @type(item_reply_owner() = 'owner').
-type(item_reply_owner() :: 'owner').

%% @type(item_reply_publisher() = 'publisher').
-type(item_reply_publisher() :: 'publisher').

%% @type(item_reply() = Owner | Publisher).
-type(item_reply() :: pubsub_options:item_reply_owner()
                    | pubsub_options:item_reply_publisher()
).


-export_type([
  language/0,
  description/0,
  title/0
]).

-type(language() :: binary()).

-type(description() :: binary()).

-type(title() :: binary()).

%%

%-- Node types --%%

-export_type([
  node_type/0,
  node_type_leaf/0,
  node_type_collection/0
]).

%% @type node_type_collection() = 'collection'.
-type(node_type_collection() :: 'collection').

%% @type node_type_leaf() = 'leaf'.
-type(node_type_leaf() :: 'leaf').

%% @type node_type() = Leaf | Collection
%%    Leaf = pubsub_options:node_type_leaf()
%%    Collection = pubsub_options:node_type_collection()
-type(node_type() :: pubsub_options:node_type_leaf()
                   | pubsub_options:node_type_collection()
).

%%

%-- Notification Types --%%
-export_type([
  notification_type/0,
  notification_type_chat/0,
  notification_type_headline/0,
  notification_type_normal/0
]).

-type(notification_type_chat() :: 'chat').

-type(notification_type_headline() :: 'headline').

-type(notification_type_normal() :: 'normal').

-type(notification_type()
  :: pubsub_options:notification_type_headline()
   | pubsub_options:notification_type_normal()
  %| pubsub_options:notification_type_chat()
).

%%

%-- Publish Model --%
-export_type([
  publish_model/0,
  publish_model_publishers/0,
  publish_model_subscribers/0,
  publish_model_open/0
]).

%% @type publish_model_publishers() = 'publishers'.
-type(publish_model_publishers() :: 'publishers').

%% @type publish_model_subscribers() = 'subscribers'.
-type(publish_model_subscribers() :: 'subscribers').

%% @type publish_model_open() = 'open'.
-type(publish_model_open() :: 'open').

%% @type publish_model() = Publishers | Subscribers | Open
%%    Publishers = pubsub_options:publish_model_publishers()
%%    Subscribers = pubsub_options:publish_model_subscribers()
%%    Open = pubsub_options:publish_model_open()
-type(publish_model() :: pubsub_options:publish_model_publishers()
                       | pubsub_options:publish_model_subscribers()
                       | pubsub_options:publish_model_open()
).


-export_type([
  roster_group/0,
  roster_groups/0,
  roster_groups_allowed/0,
  rosters_groups_allowed/0
]).

-type(roster_group() :: binary()).
-type(roster_groups() :: [pubsub_options:roster_group()]).
-type(roster_groups_allowed()
  :: {Node_Owner::xmpp_jid:usr_bare(),
      Roster_Groups::pubsub_options:roster_groups()}
).
-type(rosters_groups_allowed()
  :: [Roster_Group_Allowed::pubsub_options:roster_groups_allowed()]
).

%%

%-- Send Last Published Item --%
-export_type([
  send_last_published_item/0,
  %%
  send_last_published_item_never/0,
  send_last_published_item_on_sub/0,
  send_last_published_item_on_sub_and_presence/0
]).

%% @type send_last_published_item_never() = 'never'.
-type(send_last_published_item_never() :: 'never').

%% @type send_last_published_item_on_sub() = 'on_sub'.
-type(send_last_published_item_on_sub() :: 'on_sub').

%% @type send_last_published_item_on_sub_and_presence() = 'on_sub_and_presence'.
-type(send_last_published_item_on_sub_and_presence() :: 'on_sub_and_presence').

%% @type send_last_published_item() = Never | On_Sub | On_Sub_And_Presence
%%    Never = pubsub_options:send_last_published_item_never()
%%    On_Sub = pubsub_options:send_last_published_item_on_sub()
%%    On_Sub_And_Presence = pubsub_options:send_last_published_item_on_sub_and_presence().
-type(send_last_published_item()
  :: pubsub_options:send_last_published_item_never()
   | pubsub_options:send_last_published_item_on_sub()
   | pubsub_options:send_last_published_item_on_sub_and_presence()
).

%%


-export_type([
  option_node_access_model/0,
  option_node_children_association_policy/0,
  option_node_children_association_whitelist/0,
  option_node_children/0,
  option_node_children_max/0,
  option_node_collections/0,
  option_node_children_max_collections/0,
  option_node_children_max_multi_collections/0,
  option_node_contact/0,
  option_node_deliver_notifications/0,
  option_node_deliver_payloads/0,
  option_node_description/0,
  option_node_item_expire/0,
  option_node_itemreply/0,
  option_node_language/0,
  option_node_max_items/0,
  option_node_max_payload_size/0,
  option_node_node_type_leaf/0,
  option_node_node_type_collection/0,
  option_node_node_type/0,
  option_node_notification_type/0,
  option_node_notify_config/0,
  option_node_notify_delete/0,
  option_node_notify_retract/0,
  option_node_notify_sub/0,
  option_node_persist_items/0,
  option_node_presence_based_delivery/0,
  option_node_publish_model/0,
  option_node_purge_offline/0,
  option_node_roster_groups_allowed/0,
  option_node_send_last_published_item/0,
  option_node_tempsub/0,
  option_node_subscribe/0,
  option_node_title/0,
  option_node_type/0
]).

-type(option_node_access_model() ::
  {Key   :: 'access_model',
   Value :: pubsub_options:access_model()}
).

%-type(option_node_body_xslt() ::
%  {Key   :: 'body_xslt(',
%   Value :: binary()}
%).

-type(option_node_children_association_policy() ::
  {Key   :: 'children_association_policy',
   Value :: pubsub_options:children_association_policy()}
).

-type(option_node_children_association_whitelist() ::
  {Key   :: 'children_association_whitelist',
   Value :: [Jid_Entity::xmpp_jid:raw_jid_entity_bare()]}
).

-type(option_node_children() ::
  {Key   :: 'children',
   Value :: [NodeId::exmpp_pubsub:nodeId()]}
).

-type(option_node_children_max() ::
  {Key   :: 'children_max',
   Value :: undefined | non_neg_integer()}
).

-type(option_node_children_max_collections() ::
  {Key   :: 'children_max',
   Value :: undefined | 0..1}
).

-type(option_node_children_max_multi_collections() ::
  {Key   :: 'children_max',
   Value :: undefined | non_neg_integer()}
).


-type(option_node_collections() ::
  {Key   :: 'collection',
   Value :: [NodeId::exmpp_pubsub:nodeId()]}
).

-type(option_node_contact() ::
  {Key   :: 'contact',
   Value :: [Jid_Entity::xmpp_jid:raw_jid_entity()]}
).

%-type(option_node_dataform_xslt() ::
%  {Key   :: 'dataform_xslt',
%   Value :: binary()}
%).

-type(option_node_deliver_notifications() ::
  {Key   :: 'deliver_notifications',
   Value :: boolean()}
).

-type(option_node_deliver_payloads() ::
  {Key   :: 'deliver_payloads',
   Value :: boolean()}
).

-type(option_node_description() ::
  {Key   :: 'description',
   Value :: undefined | pubsub_options:description()}
).

-type(option_node_item_expire() ::
  {Key   :: 'item_expire',
   Value :: undefined | non_neg_integer()}
).

-type(option_node_itemreply() ::
  {Key   :: 'itemreply',
   Value :: pubsub_options:item_reply()}
).

-type(option_node_language() ::
  {Key   :: 'language',
   Value :: undefined | pubsub_options:language()}
).

-type(option_node_max_items() ::
  {Key   :: 'max_items',
   Value :: undefined | non_neg_integer()}
).

-type(option_node_max_payload_size() ::
  {Key   :: 'max_payload_size',
   Value :: undefined | non_neg_integer()}
).

-type(option_node_node_type_leaf() ::
  {Key   :: 'node_type',
   Value :: pubsub_options:node_type_leaf()}
).

-type(option_node_node_type_collection() ::
  {Key   :: 'node_type',
   Value :: pubsub_options:node_type_collection()}
).

-type(option_node_node_type()
  :: pubsub_options:option_node_node_type_leaf()
   | pubsub_options:option_node_node_type_collection()
).

-type(option_node_notification_type() ::
  {Key   :: 'notification_type',
   Value :: pubsub_options:notification_type()}
).

-type(option_node_notify_config() ::
  {Key   :: 'notify_config',
   Value :: boolean()}
).

-type(option_node_notify_delete() ::
  {Key   :: 'notify_delete',
   Value :: boolean()}
).

-type(option_node_notify_retract() ::
  {Key   :: 'notify_retract',
   Value :: boolean()}
).

-type(option_node_notify_sub() ::
  {Key   :: 'notify_sub',
   Value :: boolean()}
).

-type(option_node_persist_items() ::
  {Key   :: 'persist_items',
   Value :: boolean()}
).

-type(option_node_presence_based_delivery() ::
  {Key   :: 'presence_based_delivery',
   Value :: boolean()}
).

-type(option_node_publish_model() ::
  {Key   :: 'publish_model',
   Value :: pubsub_options:publish_model()}
).

-type(option_node_purge_offline() ::
  {Key   :: 'purge_offline',
   Value :: boolean()}
).

-type(option_node_roster_groups_allowed() ::
  {Key   :: 'roster_groups_allowed',
   Value :: pubsub_options:rosters_groups_allowed()}
).

-type(option_node_send_last_published_item() ::
  {Key   :: 'send_last_published_item',
   Value :: pubsub_options:send_last_published_item()}
).

-type(option_node_tempsub() ::
  {Key   :: 'tempsub',
   Value :: boolean()}
).

-type(option_node_subscribe() ::
  {Key   :: 'subscribe',
   Value :: boolean()}
).

-type(option_node_title() ::
  {Key   :: 'title',
   Value :: undefined | pubsub_options:title()}
).

-type(option_node_type() ::
  {Key   :: 'type',
   Value :: undefined | binary()}
).



%-- Node Option Type (local or remote user ; leaf or collection node) --%
-export_type([
  option_node/0,
  options_node/0,
  %%
  option_node_leaf/0,
  option_node_collection/0,
  %%
  options_node_leaf/0,
  options_node_collection/0
]).
 
-type(option_node_leaf()
  :: pubsub_options:option_node_access_model()
   | pubsub_options:option_node_collections()
   | pubsub_options:option_node_contact()
   | pubsub_options:option_node_deliver_notifications()
   | pubsub_options:option_node_deliver_payloads()
   | pubsub_options:option_node_description()
   | pubsub_options:option_node_item_expire()
   | pubsub_options:option_node_itemreply()
   | pubsub_options:option_node_language()
   | pubsub_options:option_node_max_items()
   | pubsub_options:option_node_max_payload_size()
   | pubsub_options:option_node_node_type_leaf()
   | pubsub_options:option_node_notification_type()
   | pubsub_options:option_node_notify_config()
   | pubsub_options:option_node_notify_delete()
   | pubsub_options:option_node_notify_retract()
   | pubsub_options:option_node_notify_sub()
   | pubsub_options:option_node_persist_items()
   | pubsub_options:option_node_presence_based_delivery()
   | pubsub_options:option_node_publish_model()
   | pubsub_options:option_node_purge_offline()
   | pubsub_options:option_node_roster_groups_allowed()   
   | pubsub_options:option_node_send_last_published_item()
   | pubsub_options:option_node_tempsub()
   | pubsub_options:option_node_subscribe()
   | pubsub_options:option_node_title()
   | pubsub_options:option_node_type()
).

%%
-type(option_node_collection()
  :: pubsub_options:option_node_access_model()
   | pubsub_options:option_node_children_association_policy()
   | pubsub_options:option_node_children_association_whitelist()
   | pubsub_options:option_node_children()
   | pubsub_options:option_node_children_max()
   | pubsub_options:option_node_collections()
   | pubsub_options:option_node_contact()
   | pubsub_options:option_node_deliver_notifications()
   | pubsub_options:option_node_deliver_payloads()
   | pubsub_options:option_node_description()
   | pubsub_options:option_node_item_expire()
   | pubsub_options:option_node_itemreply()
   | pubsub_options:option_node_language()
   | pubsub_options:option_node_max_items()
   | pubsub_options:option_node_max_payload_size()
   | pubsub_options:option_node_node_type_collection()
   | pubsub_options:option_node_notification_type()
   | pubsub_options:option_node_notify_config()
   | pubsub_options:option_node_notify_delete()
   | pubsub_options:option_node_notify_retract()
   | pubsub_options:option_node_notify_sub()
   | pubsub_options:option_node_persist_items()
   | pubsub_options:option_node_presence_based_delivery()
   | pubsub_options:option_node_publish_model()
   | pubsub_options:option_node_purge_offline()
   | pubsub_options:option_node_roster_groups_allowed()
   | pubsub_options:option_node_send_last_published_item()
   | pubsub_options:option_node_tempsub()
   | pubsub_options:option_node_subscribe()
   | pubsub_options:option_node_title()
   | pubsub_options:option_node_type()
).

%%
-type(option_node()
  :: pubsub_options:option_node_leaf()
   | pubsub_options:option_node_collection()
).

%%
-type(options_node_leaf()
  :: [pubsub_options:option_node_leaf(),...]
).

%%
-type(options_node_collection()
  :: [pubsub_options:option_node_collection(),...]
).

%%
-type(options_node()
  :: pubsub_options:options_node_leaf()
   | pubsub_options:options_node_collection()
).

%-- Publish-Options --%
%% pubsub#publish-options FORM_TYPE
-export_type([
  option_item_access_model/0,
  option_item_deliver_notifications/0,
  option_item_deliver_payloads/0,
  option_item_item_expire/0,
  option_item_itemreply/0,
%%option_item_max_payload_size/0,
  option_item_notification_type/0,
  option_item_notify_config/0,
  option_item_notify_retract/0,
  option_item_persist_items/0,
  option_item_presence_based_delivery/0,
  option_item_publish_model/0,
  option_item_purge_offline/0,
  option_item_roster_groups_allowed/0,
  option_item_send_last_published_item/0,
  option_item_type/0
]).

-type(option_item_access_model() ::
  {Key   :: 'access_model',
   Value :: pubsub_options:access_model()}
).

-type(option_item_deliver_notifications() ::
  {Key   :: 'deliver_notifications',
   Value :: boolean()}
).

-type(option_item_deliver_payloads() ::
  {Key   :: 'deliver_payloads',
   Value :: boolean()}
).

-type(option_item_item_expire() ::
  {Key   :: 'item_expire',
   Value :: undefined | non_neg_integer()}
).

-type(option_item_itemreply() ::
  {Key   :: 'itemreply',
   Value :: pubsub_options:item_reply()}
).

%-type(option_item_max_payload_size() ::
%  {Key   :: 'max_payload_size',
%   Value :: undefined | non_neg_integer()}
%).

-type(option_item_notification_type() ::
  {Key   :: 'notification_type',
   Value :: pubsub_options:notification_type()}
).

-type(option_item_notify_config() ::
  {Key   :: 'notify_config',
   Value :: boolean()}
).

-type(option_item_notify_retract() ::
  {Key   :: 'notify_retract',
   Value :: boolean()}
).

-type(option_item_persist_items() ::
  {Key   :: 'persist_items',
   Value :: boolean()}
).

-type(option_item_presence_based_delivery() ::
  {Key   :: 'presence_based_delivery',
   Value :: boolean()}
).

-type(option_item_publish_model() ::
  {Key   :: 'publish_model',
   Value :: pubsub_options:publish_model()}
).

-type(option_item_purge_offline() ::
  {Key   :: 'purge_offline',
   Value :: boolean()}
).

-type(option_item_roster_groups_allowed() ::
  {Key   :: 'roster_groups_allowed',
   Value :: pubsub_options:rosters_groups_allowed()}
).

-type(option_item_send_last_published_item() ::
  {Key   :: 'send_last_published_item',
   Value :: pubsub_options:send_last_published_item()}
).

-type(option_item_type() ::
  {Key   :: 'type',
   Value :: undefined | binary()}
).

-export_type([
  option_item/0,
  options_item/0
]).

-type(option_item()
  :: pubsub_options:option_item_access_model()
   | pubsub_options:option_item_deliver_notifications()
   | pubsub_options:option_item_deliver_payloads()
   | pubsub_options:option_item_item_expire()
   | pubsub_options:option_item_itemreply()
%% | pubsub_options:option_item_max_payload_size()
   | pubsub_options:option_item_notification_type()
   | pubsub_options:option_item_notify_config()
   | pubsub_options:option_item_notify_retract()
   | pubsub_options:option_item_persist_items()
   | pubsub_options:option_item_presence_based_delivery()
   | pubsub_options:option_item_publish_model()
   | pubsub_options:option_item_purge_offline()
   | pubsub_options:option_item_roster_groups_allowed()
   | pubsub_options:option_item_send_last_published_item()
   | pubsub_options:option_item_type()
).

%%
-type(options_item()
  :: [pubsub_options:option_item(),...]
).


%-- Subscription Options --%%
-export_type([
  option_subscription_deliver/0,
  option_subscription_expire/0,
  option_subscription_expire_presence/0,
  option_subscription_expire_datetime/0,
  option_subscription_include_body/0,
  option_subscription_show_values/0,
  option_subscription_subscription_type/0,
  option_subscription_subscription_depth/0
]).

%%
-type(option_subscription_deliver() ::
  {Key   :: 'deliver',
   Value :: boolean()}
).

%-type(option_subscription_digest() ::
%  {Key   :: 'digest',
%   Value :: boolean()}
%).

%-type(option_subscription_digest_frequency() ::
%  {Key   :: 'digest_frequency',
%   Value :: undefined | non_neg_integer()}
%).

-type(option_subscription_expire_presence() ::
  {Key   :: 'expire',
   Value :: undefined | 'presence'}
).

-type(option_subscription_expire_datetime() ::
  {Key   :: 'expire',
   Value :: undefined | erlang:timestamp()}
).

-type(option_subscription_expire()
  :: pubsub_options:option_subscription_expire_presence()
   | pubsub_options:option_subscription_expire_datetime()
).

-type(option_subscription_include_body() ::
  {Key   :: 'include_body',
   Value :: boolean()}
).

-type(option_subscription_show_values() ::
  {Key   :: 'show-values',
   Value :: ['away' | 'chat' | 'dnd' | 'online' | 'xa']}
).

-type(option_subscription_subscription_type() ::
  {Key   :: 'subscription_type',
   Value :: 'all' | 'items' | 'nodes'}
).

-type(option_subscription_subscription_depth() ::
  {Key   :: 'subscription_depth',
   Value :: 'all' | exmpp_pubsub:level()}
).


-export_type([
  option_subscription/0,
  %%
  option_subscription_leaf/0,
  option_subscription_collection/0,
  %%
  option_subscription_leaf_local/0,
  option_subscription_leaf_remote/0,
  %%
  option_subscription_collection_local/0,
  option_subscription_collection_remote/0
]).

%%
-type(option_subscription_leaf_local()
  :: pubsub_options:option_subscription_deliver()
   | pubsub_options:option_subscription_expire()
 % | pubsub_options:option_subscription_include_body()
   | pubsub_options:option_subscription_show_values()
).

%%
-type(option_subscription_leaf_remote()
  :: pubsub_options:option_subscription_deliver()
 % | pubsub_options:option_subscription_expire_datetime()
 % | pubsub_options:option_subscription_include_body()
).

%%
-type(option_subscription_collection_local()
  :: pubsub_options:option_subscription_deliver()
   | pubsub_options:option_subscription_expire()
   | pubsub_options:option_subscription_expire_presence()
   | pubsub_options:option_subscription_expire_datetime()
  %| pubsub_options:option_subscription_include_body()
   | pubsub_options:option_subscription_show_values()
   | pubsub_options:option_subscription_subscription_type()
   | pubsub_options:option_subscription_subscription_depth()
).

%%
-type(option_subscription_collection_remote()
  :: pubsub_options:option_subscription_deliver()
   | pubsub_options:option_subscription_expire_datetime()
   | pubsub_options:option_subscription_include_body()
   | pubsub_options:option_subscription_subscription_type()
   | pubsub_options:option_subscription_subscription_depth()
).

%%
-type(option_subscription_leaf()
  :: pubsub_options:option_subscription_leaf_local()
   | pubsub_options:option_subscription_leaf_remote()
).

%%
-type(option_subscription_collection()
  :: pubsub_options:option_subscription_collection_local()
   | pubsub_options:option_subscription_collection_remote()
).


%%
-type(option_subscription()
  :: pubsub_options:option_subscription_leaf()
   | pubsub_options:option_subscription_collection()
).


-export_type([
  options_subscription/0,
  %%
  options_subscription_leaf/0,
  options_subscription_collection/0,
  %%
  options_subscription_local/0,
  options_subscription_remote/0,
  %%
  options_subscription_leaf_local/0,
  options_subscription_leaf_remote/0,
  %%
  options_subscription_collection_local/0,
  options_subscription_collection_remote/0
]).

%%
-type(options_subscription_leaf_local()
  :: [pubsub_options:options_subscription_leaf_local(),...]
).

%%
-type(options_subscription_leaf_remote()
  :: [pubsub_options:options_subscription_leaf_remote(),...]
).

%%
-type(options_subscription_collection_local()
  :: [pubsub_options:options_subscription_collection_local(),...]
).

%%
-type(options_subscription_collection_remote()
  :: [pubsub_options:options_subscription_collection_remote(),...]
).

%%
-type(options_subscription_leaf()
  :: pubsub_options:options_subscription_leaf_local()
   | pubsub_options:options_subscription_leaf_remote()
).

%%
-type(options_subscription_collection()
  :: pubsub_options:options_subscription_collection_local()
   | pubsub_options:options_subscription_collection_remote()
).

%%
-type(options_subscription_local()
  :: pubsub_options:options_subscription_leaf_local()
   | pubsub_options:options_subscription_collection_local()
).

%%
-type(options_subscription_remote()
  :: pubsub_options:options_subscription_leaf_remote()
   | pubsub_options:options_subscription_collection_remote()
).

-type(options_subscription()
  :: pubsub_options:options_subscription_leaf()
   | pubsub_options:options_subscription_collection()
).

%% --------------------------------------------------------------------
%% Functions.
%% --------------------------------------------------------------------


xmlns(Xmlel) ->
    get_tag_attr_s(<<"xmlns">>, Xmlel).


-define(Is_Xmlel_Field(Xmlel),
(
  Xmlel#xmlel.name == <<"field">>
)).

-define(Is_Xmlel_Value(Xmlel),
(
  Xmlel#xmlel.name  == <<"value">>         andalso
  Xmlel#xmlel.attrs == []
)).

-define(Is_Xmlel_X(Xmlel),
(
  Xmlel#xmlel.name  == <<"x">>
)).

-define(Is_Xmlel_X_Submit(Xmlel),
(
  %Xmlel#xmlel.ns    == 'jabber:x:data' andalso
  Xmlel#xmlel.name  == <<"x">>           % andalso
 % Xmlel#xmlel.attrs == [#xmlattr{name = <<"type">>, value = <<"submit">>}]
)).

-define(Is_Xmlel_X_Form(Xmlel),
(
  %Xmlel#xmlel.ns    == 'jabber:x:data' andalso
  Xmlel#xmlel.name  == <<"x">>             andalso
  Xmlel#xmlel.attrs == [{<<"type">>, <<"form">>}]
)).



%%
%%
-define(Is_Default_Subscribe_Option(Option),
(
  Option == {'deliver', true}
      orelse
  Option == {'expire', undefined}
      orelse
  Option == {'include_body', false}
      orelse
  Option == {'show-values', ['away' , 'chat' , 'dnd' , 'online' , 'xa']}
)).


%%
-spec(trim_subscription_options/1 ::
(
  Subscription_Options::pubsub_options:options_subscription())
    -> Subscription_Options::pubsub_options:options_subscription()
).

trim_subscription_options(Subscription_Options) ->
    trim_subscription_options(Subscription_Options, []).

%%
-spec(trim_subscription_options/2 ::
(
  Options              :: [] | pubsub_options:options_subscription(),
  Subscription_Options :: [] | pubsub_options:options_subscription())
    -> Subscription_Options :: [] | pubsub_options:options_subscription()
).

trim_subscription_options([] = _Options, Subscription_Options) ->
    Subscription_Options;
%%
trim_subscription_options([Option | Options], Subscription_Options)
  when ?Is_Default_Subscribe_Option(Option) ->
    trim_subscription_options(Options, Subscription_Options);
%%
trim_subscription_options([Option | Options], Subscription_Options) ->
    trim_subscription_options(Options, [Option | Subscription_Options]).

%%
-spec(parse_xmlel_x/5 ::
(
  Plugin          :: exmpp_pubsub:plugin(),
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Entity          :: xmpp_jid:usr_entity(),
  Form_Type       :: 'node_config' | 'publish-options',
  Xmlels          :: [Xmlel_X::xmlel(),...]
                   | [Xmlel::xmlel()]
                   | [])
    -> {ok,
        Options :: pubsub_options:options_node()
                 | pubsub_options:options_item()}
    %%%
     | {error, 'not-acceptable'}
     | {error, 'feature-not-implemented', 'collections'}
    %%%
     | {error, 'invalid-options'}
     | {error, 'jid-malformed'}
     %
     | {error, 'not-acceptable'}
     | {error, 'not-acceptable', 'unsupported-access-model'}
     %
     | {error, 'feature-not-implemented', 'collections'}
     | {error, 'feature-not-implemented', 'multi-collections'}
).

parse_xmlel_x(Plugin, _Pubsub_Features, _Entity, 'node_config', []) ->
    {ok, Plugin:node_options('leaf')};
parse_xmlel_x(_Plugin, _Pubsub_Features, _Entity, 'publish-options', []) ->
    {ok, []};
%%
parse_xmlel_x(Plugin, Pubsub_Features, {U,S,_} = _Entity, Form_Type, [Xmlel_X])
  when ?Is_Xmlel_X(Xmlel_X) ->
    case
        {xmlns(Xmlel_X),
         options_type(remove_cdata(Xmlel_X#xmlel.children), Pubsub_Features)}
    of
        {?NS_XDATA, {ok, 'node_config' = Form_Type, Node_Type, Xmlels}} ->
            parse_xmlels_field(Form_Type, Node_Type, Pubsub_Features,
                {U,S,undefined}, Xmlels, Plugin:node_options(Node_Type));
        %%
        {?NS_XDATA, {ok, 'publish-options' = Form_Type, Node_Type, Xmlels}} ->
            parse_xmlels_field(Form_Type, Node_Type, Pubsub_Features,
                {U,S,undefined}, Xmlels, Plugin:item_options());
        %%
        {?NS_XDATA, Error} ->
            Error;
        _ ->
            {error, 'invalid-options'}
    end;
parse_xmlel_x(_Plugin, _Pubsub_Features, _Entity, _Form_Type, _Xmlels) ->
    {error, 'invalid-options'}.

%%
-spec(parse_xmlel_x/7 ::
(
  Plugin          :: exmpp_pubsub:plugin(),
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Entity          :: xmpp_jid:usr_entity(),
  Entity_Type     :: 'local' | 'remote',
  Form_Type       :: 'subscribe_options',
  Node_Type       :: 'leaf' | 'collection',
  Xmlels          :: [Xmlel_X::xmlel(),...]
                   | [Xmlel::xmlel()]
                   | [])
    -> {ok,
        Subscription_Options :: [] | pubsub_options:options_subscription_leaf()}
    %%%
     | {error, 'invalid-options'}
     | {error, 'not-acceptable'}
).

parse_xmlel_x(Plugin, _Pubsub_Features, _Entity, Entity_Type, 'subscribe_options',
  Node_Type, []) ->
    {ok, Plugin:subscription_options(Entity_Type, Node_Type)};
%%
parse_xmlel_x(Plugin, Pubsub_Features, {U,S,_} = _Entity, Entity_Type,
  'subscribe_options' = Form_Type, Node_Type, [Xmlel_X])
  when ?Is_Xmlel_X(Xmlel_X) ->
    case
        {xmlns(Xmlel_X),
         options_type(remove_cdata(Xmlel_X#xmlel.children), 'subscribe_options', [])}
    of
        {?NS_XDATA, {ok, 'subscribe_options', Xmlels}} ->
            parse_xmlels_field(Form_Type, Node_Type, Pubsub_Features,
                {U,S,undefined}, Entity_Type, Xmlels,
                Plugin:default_subscription_options(Entity_Type, Node_Type));
        {?NS_XDATA, Error} ->
            Error;
        _ ->
            {error, 'invalid-options'}
    end;
%%
parse_xmlel_x(_Plugin, _Pubsub_Features, _Entity, _Entity_Type, 'subscribe_options',
  _Node_Type, _Xmlels) ->
    {error, 'invalid-options'}.

%%
-spec(options_type/3 ::
(
  Xmlels        :: [Xmlel_Field::xmlel()]
                 | [Xmlel::xmlel()],
  Form_Type     :: 'subscribe_options',
  Xmlels_Fields :: [Xmlel_Field::xmlel()]
                 | [Xmlel::xmlel()])
    -> {ok,
        Form_Type    :: 'subscribe_options',
        Xmlels_Field :: [Xmlel_Field::xmlel()]
                      | [Xmlel::xmlel()]}
    %%%
     | {error, 'not-acceptable'}
).

options_type([] = _Xmlels_Field, 'subscribe_options', _Xmlels) ->
    {error, 'not-acceptable'};
%%
options_type([Xmlel_Field | Xmlels_Field], 'subscribe_options', Xmlels)
  when ?Is_Xmlel_Field(Xmlel_Field) ->
    case {xmlns(Xmlel_Field), get_tag_attr_s(<<"var">>, Xmlel_Field)} of
        {_, <<>>} ->
            {error, 'not-acceptable'};
        {?NS_XDATA, <<"FORM_TYPE">>} ->
            case remove_cdata(Xmlel_Field#xmlel.children) of
                [Xmlel_Value] when ?Is_Xmlel_Value(Xmlel_Value) ->
                    case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
                        {?NS_XDATA, ?NS_PUBSUB_SUBSCRIBE_OPTIONS} ->
                            {ok, 'subscribe_options', Xmlels ++ Xmlels_Field};
                        _ ->
                            {error, 'not-acceptable'}
                    end;
                _ ->
                    {error, 'not-acceptable'}
            end;
        {?NS_XDATA, _} ->
            options_type(Xmlels_Field, 'subscribe_options',
                [Xmlel_Field | Xmlels]);
        _ ->
            {error, 'not-acceptable'}
    end.

%%
-spec(options_type/2 ::
(
  Xmlels          :: [Xmlel_Field::xmlel()]
                   | [Xmlel::xmlel()],
  Pubsub_Features :: exmpp_pubsub:pubsub_features())
    -> {ok,
        Form_Type    :: 'node_config' | 'publish-options',
        Node_Type    :: 'leaf' | 'collection',
        Xmlels_Field :: [Xmlel_Field::xmlel()]
                      | [Xmlel::xmlel()]}
    %%%
     | {error, 'not-acceptable'}
     | {error, 'feature-not-implemented', 'collections'}
).

options_type(Xmlels, Pubsub_Features) ->
    options_type(Xmlels,
        lists:member(<<"collections">>, Pubsub_Features), undefined, undefined, []).

%%
-spec(options_type/5 ::
(
  Xmlels        :: [Xmlel_Field::xmlel()]
                 | [Xmlel::xmlel()],
  Collections   :: boolean(),
  Form_Type     :: undefined | 'node_config' | 'publish-options',
  Node_Type     :: undefined | 'leaf' | 'collection',
  Xmlels_Fields :: [Xmlel_Field::xmlel()]
                 | [Xmlel::xmlel()])
    -> {ok,
        Form_Type    :: 'node_config' | 'publish-options',
        Node_Type    :: 'leaf' | 'collection',
        Xmlels_Field :: [Xmlel_Field::xmlel()]
                      | [Xmlel::xmlel()]}
    %%%
     | {error, 'not-acceptable'}
     | {error, 'feature-not-implemented', 'collections'}
).

options_type([] = _Xmlels, _Collections, Form_Type, undefined = _Node_Type,
  Xmlels_Field)
  when Form_Type =/= undefined ->
    {ok, Form_Type, 'leaf', Xmlels_Field};
%%
options_type([] = _Xmlels, _Collections, Form_Type, Node_Type, Xmlels_Field)
  when Form_Type =/= undefined ->
    {ok, Form_Type, Node_Type, Xmlels_Field};
%%
options_type([] = _Xmlels, _Collections, undefined = _Form_Type, _Node_Type,
  _Xmlels_Field) ->
    {error, 'bad-options'};
%%
options_type(Xmlels, _Collections, Form_Type, Node_Type, Xmlels_Field)
  when Form_Type =/= undefined andalso Node_Type =/= undefined ->
    {ok, Form_Type, Node_Type, Xmlels ++ Xmlels_Field};
%%
options_type([Xmlel_Field | Xmlels_Field], Collections, Form_Type, Node_Type,
  Xmlels)
  when ?Is_Xmlel_Field(Xmlel_Field) ->
    case {xmlns(Xmlel_Field), get_tag_attr_s(<<"var">>, Xmlel_Field)} of
        {_, <<>>} ->
            {error, 'not-acceptable'};
        {?NS_XDATA, <<"FORM_TYPE">>} ->
            case remove_cdata(Xmlel_Field#xmlel.children) of
                [Xmlel_Value] when ?Is_Xmlel_Value(Xmlel_Value) ->
                    case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
                        {?NS_XDATA, ?NS_PUBSUB_NODE_CONFIG} ->
                            options_type(Xmlels_Field, Collections, 'node_config',
                                Node_Type, Xmlels);
                        {?NS_XDATA, ?NS_PUBSUB_PUBLISH_OPTIONS} ->
                            options_type(Xmlels_Field, Collections,
                                'publish-options', Node_Type, Xmlels);
                        _ ->
                            {error, 'not-acceptable'}
                    end;
                _ ->
                    {error, 'not-acceptable'}
            end;
        {?NS_XDATA, <<"pubsub#node_type">>} ->
            case remove_cdata(Xmlel_Field#xmlel.children) of
                [Xmlel_Value] when ?Is_Xmlel_Value(Xmlel_Value) ->
                    case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
                        {?NS_XDATA, <<"leaf">>} ->
                            options_type(Xmlels_Field, Collections, Form_Type,
                                'leaf', Xmlels);
                        {?NS_XDATA, <<"collection">>} ->
                            case Collections of
                                true ->
                                    options_type(Xmlels_Field, Collections,
                                        Form_Type, 'collection', Xmlels);
                                false ->
                                    {error,
                                     'feature-not-implemented', 'collections'}
                            end;
                        _ ->
                            {error, 'not-acceptable'}
                    end
            end;
        {?NS_XDATA, _Xmlattr} ->
            options_type(Xmlels_Field, Collections, Form_Type, Node_Type,
                [Xmlel_Field | Xmlels]);
        _ ->
            {error, 'not-acceptable'}
    end.

%%
-spec(parse_xmlels_field/6 ::
(
  Options_Type    :: 'node_config' | 'publish-options',
  Node_Type       :: 'leaf' | 'collection',
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Entity          :: xmpp_jid:usr_entity(),
  Xmlels          :: [Xmlel_Field::xmlel()]
                   | [Xmlel::xmlel()],
  Options         :: pubsub_options:options_node()
                   | pubsub_options:options_item()
                   %%
                   | {error, 'invalid-options'}
                   | {error, 'jid-malformed'}
                   %
                   | {error, 'not-acceptable'}
                   | {error, 'not-acceptable', 'unsupported-access-model'}
                   %
                   | {error, 'feature-not-implemented', 'collections'}
                   | {error, 'feature-not-implemented', 'multi-collections'})
    -> {ok,
        Options :: pubsub_options:options_node()
                 | pubsub_options:options_item()}
    %%%
     | {error, 'invalid-options'}
     | {error, 'jid-malformed'}
     %
     | {error, 'not-acceptable'}
     | {error, 'not-acceptable', 'unsupported-access-model'}
     %
     | {error, 'feature-not-implemented', 'collections'}
     | {error, 'feature-not-implemented', 'multi-collections'}
).

%%
parse_xmlels_field(_Options_Type, _Node_Type, _Pubsub_Features, _Entity,
  _Xmlels, Error)
  when is_tuple(Error) ->
    Error;
parse_xmlels_field(_Options_Type, _Node_Type, _Pubsub_Features, _Entity,
  [] = _Xmlels, Options) ->
    {ok, Options};
%%
parse_xmlels_field(Options_Type, Node_Type, Pubsub_Features, Entity,
  [Xmlel_Field | Xmlels], Options)
  when ?Is_Xmlel_Field(Xmlel_Field) ->
    case xmlns(Xmlel_Field) of
        ?NS_XDATA ->
            parse_xmlels_field(Options_Type, Node_Type, Pubsub_Features, Entity,
                Xmlels,
                parse_xmlel_field_attrs(
                    get_tag_attr_s(<<"var">>, Xmlel_Field),
                    Options_Type, Node_Type, Pubsub_Features, Entity,
                    remove_cdata(Xmlel_Field#xmlel.children),
                    Options));
        _ ->
            {error, 'invalid-options'}
    end;
%%
parse_xmlels_field(_Options_Type, _Node_Type, _Pubsub_Feature, _Entity,
  _Xmlels, _Options) ->
    {error, 'invalid-options'}.

%%
-spec(parse_xmlels_field/7 ::
(
  Options_Type    :: 'subscribe_options',
  Node_Type       :: 'leaf' | 'collection',
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Entity          :: xmpp_jid:usr_entity(),
  Entity_Type     :: 'local' | 'remote',
  Xmlels          :: [Xmlel_Field::xmlel()]
                   | [Xmlel::xmlel()],
  Options         :: pubsub_options:options_subscription()
                   %
                   | {error, 'invalid-options'}
                   | {error, 'not-acceptable'})
    -> {ok,
        Options :: pubsub_options:options_node()
                 | pubsub_options:options_item()}
    %%%
     | {error, 'invalid-options'}
     | {error, 'not-acceptable'}
).

%%
parse_xmlels_field(_Options_Type, _Node_Type, _Pubsub_Features, _Entity,
  _Entity_Type, _Xmlels, Error)
  when is_tuple(Error) ->
    Error;
parse_xmlels_field(_Options_Type, _Node_Type, _Pubsub_Features, _Entity,
  _Entity_Type, [] = _Xmlels, Options) ->
    {ok, trim_subscription_options(Options)};
%%
parse_xmlels_field(Options_Type, Node_Type, Pubsub_Features, Entity,
  Entity_Type, [Xmlel_Field | Xmlels], Options)
  when ?Is_Xmlel_Field(Xmlel_Field) ->
    case xmlns(Xmlel_Field) of
        ?NS_XDATA ->
            parse_xmlels_field(Options_Type, Node_Type, Pubsub_Features, Entity,
                Entity_Type,
                Xmlels,
                parse_xmlel_field_attrs(
                    get_tag_attr_s(<<"var">>, Xmlel_Field),
                    Options_Type, Node_Type, Pubsub_Features, Entity, Entity_Type,
                    remove_cdata(Xmlel_Field#xmlel.children),
                    Options));
        _ ->
            {error, 'invalid-options'}
    end;
%%
parse_xmlels_field(_Options_Type, _Node_Type, _Pubsub_Feature, _Entity,
  _Entity_Type, _Xmlels, _Options) ->
    {error, 'invalid-options'}.

%%
-spec(parse_xmlel_field_attrs/7 ::
(
  Pubsub_Option   :: binary(),
  Options_Type    :: 'node_config' | 'publish-options',
  Node_Type       :: 'leaf' | 'collection',
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Entity          :: xmpp_jid:usr_entity(),
  Xmlels          :: [Xmlel_Value::xmlel(),...]
                   | [Xmlel::xmlel(),...],
  Options         :: pubsub_options:options_node()
                   | pubsub_options:options_item())
    -> Options :: pubsub_options:options_node()
                | pubsub_options:options_item()
    %%%
     | {error, 'invalid-options'}
     | {error, 'jid-malformed'}
     %
     | {error, 'not-acceptable'}
     | {error, 'not-acceptable', 'unsupported-access-model'}
     %
     | {error, 'feature-not-implemented', 'collections'}
     | {error, 'feature-not-implemented', 'multi-collections'}
).

parse_xmlel_field_attrs(<<"pubsub#roster_groups_allowed">>, Options_Type,
  Node_Type, Pubsub_Features, Entity, Xmlels, Options)
  when (Options_Type == 'node_config' orelse Options_Type == 'publish-options')
  andalso (Node_Type == 'leaf' orelse Node_Type == 'collection') ->
    case lists:member(<<"access-roster">>, Pubsub_Features) of
        true ->
            case
                case Options_Type of
                    'node_config' ->
                        parse_pubsub_option_node(<<"roster_groups_allowed">>,
                            Node_Type, Pubsub_Features, Xmlels);
                    'publish-options' ->
                        parse_pubsub_option_item(<<"roster_groups_allowed">>,
                            Pubsub_Features, Xmlels)
                end
                
            of
                {ok, Roster_Groups_Allowed} ->
                    case lists:keyfind('roster_groups_allowed', 1, Options) of
                        false ->
                            [{Entity, Roster_Groups_Allowed} | Options];
                        {'roster_groups_allowed', Rosters_Groups_Allowed} ->
                            lists:keyreplace('roster_groups_allowed', 1, Options,
                                {'roster_groups_allowed',
                                 _New_Rosters_Groups_Allowed =
                                 case
                                     lists:keyreplace(Entity, 1,
                                         Rosters_Groups_Allowed,
                                         {Entity, Roster_Groups_Allowed})
                                 of
                                     Rosters_Groups_Allowed ->
                                         [{Entity, Roster_Groups_Allowed}
                                         | Rosters_Groups_Allowed];
                                     New_Rosters_Groups_Allowed ->
                                         New_Rosters_Groups_Allowed
                                 end})
                    end;
                Error ->
                    Error
            end;
        false ->
            {error, 'invalid-options'}
    end;
%%
parse_xmlel_field_attrs(<<"pubsub#", Pubsub_Option/binary>>,
  'node_config' = _Options_Type, Node_Type, Pubsub_Features, _Entity, Xmlels,
  Node_Options) ->
    case
        lists:keymember(
            Key = list_to_atom(binary_to_list(Pubsub_Option)), 1, Node_Options)
    of
        true ->
            case
                parse_pubsub_option_node(Pubsub_Option, Node_Type,
                    Pubsub_Features, Xmlels)
            of
                {ok, Value} ->
                    lists:keyreplace(Key, 1, Node_Options, {Key, Value});
                Error ->
                    Error
            end;
        false ->
            Node_Options%{error, 'invalid-options'}
    end;
%%
parse_xmlel_field_attrs(<<"pubsub#", Pubsub_Option/binary>>,
  'publish-options' = _Options_Type, _Node_Type, Pubsub_Features, _Entity, Xmlels,
  Item_Options) ->
    case parse_pubsub_option_item(Pubsub_Option, Pubsub_Features, Xmlels) of
        {ok, Value}
          when   Value == undefined
          orelse Value == [] ->
            lists:keydelete(_Key = list_to_atom(binary_to_list(Pubsub_Option)),
                1, Item_Options);
        {ok, Value} ->
            Key = list_to_atom(binary_to_list(Pubsub_Option)),
            case lists:keyreplace(Key, 1, Item_Options, {Key, Value}) of
                Item_Options     -> [{Key, Value} | Item_Options];
                New_Item_Options -> New_Item_Options
            end;
        Error ->
            Error
    end;
%%
parse_xmlel_field_attrs(_Xmlattr_value, _Options_Type, _Node_Type,
  _Pubsub_Features, _Entity, _Xmlels, _Options) ->
    {error, 'invalid-options'}.

%%
-spec(parse_xmlel_field_attrs/8 ::
(
  Pubsub_Option   :: binary(),
  Options_Type    :: 'subscribe_options',
  Node_Type       :: 'leaf' | 'collection',
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Entity          :: xmpp_jid:usr_entity(),
  Entity_Type     :: 'local' | 'remote',
  Xmlels          :: [Xmlel_Value::xmlel(),...]
                   | [Xmlel::xmlel(),...],
  Subscription_Options :: pubsub_options:options_subscription())
    -> Subscription_Options :: pubsub_options:options_subscription()
    %%%
     | {error, 'invalid-options'}
).

parse_xmlel_field_attrs(<<"pubsub#", Pubsub_Option/binary>>,
  'subscribe_options' = _Options_Type, Node_Type, Pubsub_Features, _Entity,
  Entity_Type, Xmlels, Subscription_Options) ->
    case
        lists:keymember(Key = list_to_atom(binary_to_list(Pubsub_Option)), 1,
            Subscription_Options)
    of
        true ->
            case
                parse_pubsub_option_subscription(Pubsub_Option, Node_Type,
                    Entity_Type, Pubsub_Features, Xmlels)
            of
                {ok, Value}
                  when ?Is_Default_Subscribe_Option({Key, Value}) ->
                    lists:keydelete(Key, 1, Subscription_Options);
                {ok, Value} ->
                    lists:keyreplace(Key, 1, Subscription_Options, {Key, Value});
                Error ->
                    Error
            end;
        false ->
            {error, 'invalid-options'}
    end;
%%
parse_xmlel_field_attrs(_Xmlattr_value, _Options_Type, _Node_Type,
  _Pubsub_Features, _Entity, _Entity_Type, _Xmlels, _Options) ->
    {error, 'invalid-options'}.


%%
%%
-spec(parse_pubsub_option_node/4 ::
(
  Pubsub_Option   :: binary(),
  Node_Type       :: 'collection' | 'leaf',
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Xmlels          :: [Xmlel_Value::xmlel()]
                   | [Xmlel::xmlel()])
    -> {ok, Access_Model                   :: pubsub_options:access_model()}
   % | {ok, Body_XSLT                      :: undefined | binary()}
     | {ok, Children_Association_Policy    :: 'all' | 'owners' | 'whitelist'}
     | {ok, Children_Association_Whitelist :: [Jid::xmpp_jid:raw_jid_entity_bare()]}
     | {ok, Children                       :: [Child_NodeId::exmpp_pubsub:nodeId()]}
     | {ok, Children_Max                   :: undefined | non_neg_integer()}
     | {ok, Collection                     :: [Parent_NodeId::exmpp_pubsub:nodeId()]}
     | {ok, Contact                        :: [Jid::xmpp_jid:raw_jid_entity()]}
   % | {ok, Dataform_XSLT                  :: undefined | binary()}
     | {ok, Deliver_Notifications          :: boolean()}
     | {ok, Deliver_Payloads               :: boolean()}
     | {ok, Description                    :: undefined | binary()}
     | {ok, Item_Expire                    :: undefined | non_neg_integer()}
     | {ok, ItemReply                      :: pubsub_options:item_reply()}
     | {ok, Language                       :: undefined | binary()}
     | {ok, Max_Items                      :: undefined | non_neg_integer()}
     | {ok, Max_Payload_Size               :: undefined | non_neg_integer()}
     | {ok, Node_Type                      :: pubsub_options:node_type()}
     | {ok, Notification_Type              :: pubsub_options:notification_type()}
     | {ok, Notify_Config                  :: boolean()}
     | {ok, Notify_Delete                  :: boolean()}
     | {ok, Notify_Retract                 :: boolean()}
     | {ok, Notify_Sub                     :: boolean()}
     | {ok, Persist_Items                  :: boolean()}
     | {ok, Presence_Based_Delivery        :: boolean()}
     | {ok, Publish_Model                  :: pubsub_options:publish_model()}
     | {ok, Purge_Offline                  :: boolean()}
     | {ok, Rosters_Groups_Allowed         :: pubsub_options:rosters_groups_allowed()}
     | {ok, Send_Last_Published_Item       :: pubsub_options:send_last_published_item()}
     | {ok, Tempsub                        :: boolean()}
     | {ok, Subscribe                      :: boolean()}
     | {ok, Title                          :: undefined | binary()}
     | {ok, Type                           :: undefined | atom()}
    %%%
     | {error, 'invalid-options'}
     | {error, 'jid-malformed'}
     %
     | {error, 'not-acceptable'}
     | {error, 'not-acceptable', 'unsupported-access-model'}
     %
     | {error, 'feature-not-implemented', 'collections'}
     | {error, 'feature-not-implemented', 'multi-collections'}
).

parse_pubsub_option_node(<<"access_model">> = _Pubsub_Option, Node_Type,
  Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    parse_xmlel_value({'access_model', Pubsub_Features}, Xmlels);
%%
%%parse_pubsub_option_node(<<"pubsub#body_xslt">> = _Pubsub_Option,
%% 'leaf' = _Node_Type, Pusbub_Features, Xmlels) ->
%%    case lists:member(<<"publish">>, Pubsub_Features) of
%%        true ->
%%            parse_xmlel_value('text-single', Xmlels);
%%        false ->
%%            {error, 'invalid-options'}
%%    end;
%%
parse_pubsub_option_node(<<"children_association_policy">> = _Pubsub_Option,
  'collection' = _Node_Type, _Pubsub_Features, Xmlels) ->
    parse_xmlel_value({'list-single', 'atom',
        _Default_Values = ['all', 'owners', 'whitelist']}, Xmlels);
%%
parse_pubsub_option_node(<<"children_association_whitelist">> = _Pubsub_Option,
  'collection' = _Node_Type, _Pubsub_Features, [] = _Xmlels) ->
    {ok, []};
parse_pubsub_option_node(<<"children_association_whitelist">> = _Pubsub_Option,
  'collection' = _Node_Type, _Pubsub_Features, Xmlels) ->
    parse_xmlel_value({'jid-multi', 'bare'}, Xmlels);
%%
%%
parse_pubsub_option_node(<<"children">> = _Pubsub_Option,
  'collection' = _Node_Type, _Pubsub_Features, [] = Xmlels) ->
     parse_xmlel_value('text-multi', Xmlels);
parse_pubsub_option_node(<<"children">> = _Pubsub_Option,
  'collection' = _Node_Type, _Pubsub_Features, [_] = Xmlels) ->
     parse_xmlel_value('text-multi', Xmlels);
parse_pubsub_option_node(<<"children">> = _Pubsub_Option,
  'collection' = _Node_Type, Pubsub_Features, Xmlels) ->
    case lists:member(<<"multi-collections">>, Pubsub_Features) of
        true  -> parse_xmlel_value('text-multi', Xmlels);
        false -> {error, 'feature-not-implemented', 'multi-collections'}
    end;
%%
%%
parse_pubsub_option_node(<<"children_max">> = _Pubsub_Option,
  'collection' = _Node_Type, _Pubsub_Features, [] = _Xmlels) ->
    {ok, undefined};
parse_pubsub_option_node(<<"children_max">> = _Pubsub_Option,
  'collection' = _Node_Type, Pubsub_Features, [Xmlel_Value])
  when ?Is_Xmlel_Value(Xmlel_Value) ->
    case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
        {?NS_XDATA, <<>>} ->
            {ok, undefined};
        {?NS_XDATA, CData} ->
            case catch list_to_integer(binary_to_list(CData)) of
                Integer when Integer == 0 orelse Integer == 1 ->
                    {ok, Integer};
                Integer when is_integer(Integer) andalso Integer > 1 ->
                    case
                        lists:member(<<"multi-collections">>, Pubsub_Features)
                    of
                        true  -> {ok, Integer};
                        false -> {error, 'not-acceptable'}
                    end;
                _ ->
                    {error, 'not-acceptable'}
            end;
        _ ->
            {error, 'not-acceptable'}
    end;
parse_pubsub_option_node(<<"children_max">> = _Pubsub_Option,
  'collection' = _Node_Type, _Pubsub_Features, _Xmlels) ->
    {error, 'not-acceptable'};
%%
parse_pubsub_option_node(<<"collection">> = _Pubsub_Option, Node_Type,
  Pubsub_Features, [] = _Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"collections">>, Pubsub_Features) of
        true  -> {ok, []};
        false -> {error, 'feature-not-implemented', 'collections'}
    end;
parse_pubsub_option_node(<<"collection">> = _Pubsub_Option, Node_Type,
  Pubsub_Features, [_] = Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"collections">>, Pubsub_Features) of
        true  -> parse_xmlel_value('text-multi', Xmlels);
        false -> {error, 'feature-not-implemented', 'collections'}
    end;
parse_pubsub_option_node(<<"collection">> = _Pubsub_Option, Node_Type,
  Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"collections">>, Pubsub_Features) of
        true ->
            case lists:member(<<"multi-collections">>, Pubsub_Features) of
                true  -> parse_xmlel_value('text-multi', Xmlels);
                false -> {error, 'feature-not-implemented', 'multi-collections'}
            end;
        false ->
            {error, 'feature-not-implemented', 'collections'}
    end;
%%
parse_pubsub_option_node(<<"contact">> = _Pubsub_Option, Node_Type,
  _Pubsub_Features, [] = _Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    {ok, []};
parse_pubsub_option_node(<<"contact">> = _Pubsub_Option, Node_Type,
  _Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    parse_xmlel_value({'jid-multi', 'full'}, Xmlels);
%%
%%parse_pubsub_option_node(<<"pubsub#dataform_xslt">> = _Pubsub_Option,
%% 'leaf' = _Node_Type, Pusbub_Features, Xmlels) ->
%%    case lists:member(<<"publish">>, Pubsub_Features) of
%%        true ->
%%            parse_xmlel_value('text-single', Xmlels);
%%        false ->
%%            {error, 'invalid-options'}
%%    end;
%%
parse_pubsub_option_node(<<"deliver_notifications">> = _Pubsub_Option,
  'leaf' = _Node_Type, Pubsub_Features, Xmlels) ->
    case lists:member(<<"subscribe">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"deliver_payloads">> = _Pubsub_Option, Node_Type,
  Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"subscribe">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"description">> = _Pubsub_Option, Node_Type,
  _Pubsub_Features, [] = _Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    {ok, undefined};
parse_pubsub_option_node(<<"description">> = _Pubsub_Option, Node_Type,
  _Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    parse_xmlel_value('text-single', Xmlels);
%%
parse_pubsub_option_node(<<"item_expire">> = _Pubsub_Option, 'leaf' = _Node_Type,
  Pubsub_Features, [] = _Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true  -> {ok, undefined};
        false -> {error, 'invalid-options'}
    end;
parse_pubsub_option_node(<<"item_expire">> = _Pubsub_Option, 'leaf' = _Node_Type,
  Pubsub_Features, Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true  -> parse_xmlel_value({'text-single', 'integer'}, Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"itemreply">> = _Pubsub_Option, 'leaf' = _Node_Type,
  Pubsub_Features, Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true ->
            parse_xmlel_value({'list-single', 'atom',
                _Default_Values = ['owner', 'publisher']}, Xmlels);
        false ->
            {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"language">> = _Pubsub_Option, Node_Type,
  _Pubsub_Features, [] = _Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    {ok, undefined};
parse_pubsub_option_node(<<"language">> = _Pubsub_Option, Node_Type,
  _Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    parse_xmlel_value('text-single', Xmlels);
%%
parse_pubsub_option_node(<<"max_items">> = _Pubsub_Option, 'leaf' = _Node_Type,
  Pubsub_Features, [] = _Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true  -> {ok, undefined};
        false -> {error, 'invalid-options'}
    end;
parse_pubsub_option_node(<<"max_items">> = _Pubsub_Option, 'leaf' = _Node_Type,
  Pubsub_Features, Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true  -> parse_xmlel_value({'text-single', 'integer'}, Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"max_payload_size">> = _Pubsub_Option,
  'leaf' = _Node_Type, Pubsub_Features, [] = _Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true  -> {ok, undefined};
        false -> {error, 'invalid-options'}
    end;
parse_pubsub_option_node(<<"max_payload_size">> = _Pubsub_Option,
  'leaf' = _Node_Type, Pubsub_Features, Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true  -> parse_xmlel_value({'text-single', 'integer'}, Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"notification_type">> = _Pubsub_Option, Node_Type,
  Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true ->
            parse_xmlel_value({'list-single', 'atom',
                _Default_Values = ['headline', 'normal']}, Xmlels);
        false ->
            {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"notify_config">> = _Pubsub_Option, Node_Type,
  Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"config-node">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"notify_delete">> = _Pubsub_Option, Node_Type,
  Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"delete-nodes">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"notify_retract">> = _Pubsub_Option,
  'leaf' = _Node_Type, Pubsub_Features, Xmlels) ->
    case lists:member(<<"delete-items">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"notify_sub">> = _Pubsub_Option, Node_Type,
  Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"subscribe">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"persist_items">> = _Pubsub_Option,
  'leaf' = _Node_Type, Pubsub_Features,  Xmlels) ->
    case lists:member(<<"persistent-items">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"presence_based_delivery">> = _Pubsub_Option,
  Node_Type, Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"presence-notifications">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"publish_model">> = _Pubsub_Option,
  'leaf' = _Node_Type, Pubsub_Features, Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true ->
            parse_xmlel_value({'list-single', 'atom',
                _Default_Values = ['open', 'publishers', 'subscribers']},
                Xmlels);
        false ->
            {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"purge_offline">> = _Pubsub_Option,
  'leaf' = _Node_Type, Pubsub_Features, Xmlels) ->
    case lists:member(<<"purge-nodes">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"roster_groups_allowed">> = _Pubsub_Option,
  Node_Type, Pubsub_Features, [] = _Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"access-roster">>, Pubsub_Features) of
        true  -> {ok, []};
        false -> {error, 'invalid-options'}
    end;
parse_pubsub_option_node(<<"roster_groups_allowed">> = _Pubsub_Option,
  Node_Type, Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"access-roster">>, Pubsub_Features) of
        true  -> parse_xmlel_value('list-multi', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"send_last_published_item">> = _Pubsub_Option,
  'leaf' = _Node_Type, Pubsub_Features, Xmlels) ->
    case lists:member(<<"last-published">>, Pubsub_Features) of
        true ->
            parse_xmlel_value({'list-single', 'atom',
                _Default_Values = case
                    lists:member(<<"presence-notifications">>, Pubsub_Features)
                of
                    true  -> ['never', 'on_sub', 'on_sub_and_presence'];
                    false -> ['never', 'on_sub']
                end},
                Xmlels);
        false ->
            {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"tempsub">> = _Pubsub_Option, Node_Type,
  Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"subscribe">>, Pubsub_Features) of
        true ->
            case lists:member(<<"presence-notifications">>, Pubsub_Features) of
                true  -> parse_xmlel_value('boolean', Xmlels);
                false -> {error, 'invalid-options'}
            end;
        false ->
            {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"subscribe">> = _Pubsub_Option, Node_Type,
  Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"subscribe">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(<<"title">> = _Pubsub_Option, Node_Type,
  _Pubsub_Features, [] = _Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    {ok, undefined};
parse_pubsub_option_node(<<"title">> = _Pubsub_Option, Node_Type,
  _Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    parse_xmlel_value('text-single', Xmlels);
%%
parse_pubsub_option_node(<<"type">> = _Pubsub_Option,
  'leaf' = _Node_Type, Pubsub_Features, [] = _Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true  -> {ok, undefined};
        false -> {error, 'invalid-options'}
    end;
parse_pubsub_option_node(<<"type">> = _Pubsub_Option,
  'leaf' = _Node_Type, Pubsub_Features, Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true  -> parse_xmlel_value({'text-single', 'atom'}, Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_node(_Pubsub_Option, _Node_Type, _Pubsub_Features, _Xmlels) ->
    {error, 'invalid-options'}.

%%
%%
-spec(parse_pubsub_option_item/3 ::
(
  Pubsub_Option   :: binary(),
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Xmlels          :: [Xmlel_Value::xmlel()]
                   | [Xmlel::xmlel()])
    -> {ok, Access_Model             :: pubsub_options:access_model()}
   % | {ok, Body_XSLT                :: undefined | binary()}
   % | {ok, Dataform_XSLT            :: undefined | binary()}
     | {ok, Deliver_Notifications    :: boolean()}
     | {ok, Deliver_Payloads         :: boolean()}
     | {ok, Item_Expire              :: undefined | non_neg_integer()}
     | {ok, ItemReply                :: pubsub_options:item_reply()}
%%   | {ok, Max_Payload_Size         :: undefined | non_neg_integer()}
     | {ok, Notification_Type        :: pubsub_options:notification_type()}
     | {ok, Notify_Config            :: boolean()}
     | {ok, Notify_Retract           :: boolean()}
     | {ok, Persist_Items            :: boolean()}
     | {ok, Presence_Based_Delivery  :: boolean()}
%    | {ok, Publish_Model            :: pubsub_options:publish_model()}
     | {ok, Purge_Offline            :: boolean()}
     | {ok, Rosters_Groups_Allowed   :: pubsub_options:rosters_groups_allowed()}
     | {ok, Send_Last_Published_Item :: pubsub_options:send_last_published_item()}
     | {ok, Type                     :: undefined | atom()}
    %%%
     | {error, 'invalid-options'}
     %
     | {error, 'not-acceptable'}
     | {error, 'not-acceptable', 'unsupported-access-model'}
).

parse_pubsub_option_item(<<"access_model">> = _Pubsub_Option, Pubsub_Features,
  Xmlels) ->
    parse_xmlel_value({'access_model', Pubsub_Features}, Xmlels);
%%
%%parse_pubsub_option_item(<<"pubsub#body_xslt">> = _Pubsub_Option,
%%  Pusbub_Features, Xmlels) ->
%%    case lists:member(<<"publish">>, Pubsub_Features) of
%%        true ->
%%            parse_xmlel_value('text-single', Xmlels);
%%        false ->
%%            {error, 'invalid-options'}
%%    end;
%%
%%parse_pubsub_option_item(<<"pubsub#dataform_xslt">> = _Pubsub_Option,
%%  Pusbub_Features, Xmlels) ->
%%    case lists:member(<<"publish">>, Pubsub_Features) of
%%        true ->
%%            parse_xmlel_value('text-single', Xmlels);
%%        false ->
%%            {error, 'invalid-options'}
%%    end;
%%
parse_pubsub_option_item(<<"deliver_notifications">> = _Pubsub_Option,
  Pubsub_Features, Xmlels) ->
    case lists:member(<<"subscribe">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_item(<<"deliver_payloads">> = _Pubsub_Option,
  Pubsub_Features, Xmlels) ->
    case lists:member(<<"subscribe">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_item(<<"item_expire">> = _Pubsub_Option, Pubsub_Features,
  [] = _Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true  -> {ok, undefined};
        false -> {error, 'invalid-options'}
    end;
parse_pubsub_option_item(<<"item_expire">> = _Pubsub_Option, Pubsub_Features,
  Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true  -> parse_xmlel_value({'text-single', 'integer'}, Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_item(<<"itemreply">> = _Pubsub_Option, Pubsub_Features,
  Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true ->
            parse_xmlel_value({'list-single', 'atom',
                _Default_Values = ['owner', 'publisher']}, Xmlels);
        false ->
            {error, 'invalid-options'}
    end;
%%
%parse_pubsub_option_item(<<"max_payload_size">> = _Pubsub_Option,
%  Pubsub_Features, Xmlels) ->
%    case lists:member(<<"publish">>, Pubsub_Features) of
%        true ->
%            parse_xmlel_value({'text-single', 'integer'}, Xmlels);
%        false ->
%            {error, 'invalid-options'}
%    end;
%%
parse_pubsub_option_item(<<"notification_type">> = _Pubsub_Option,
  Pubsub_Features, Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true ->
            parse_xmlel_value({'list-single', 'atom',
                _Default_Values = ['headline', 'normal']}, Xmlels);
        false ->
            {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_item(<<"notify_config">> = _Pubsub_Option, Pubsub_Features,
  Xmlels) ->
    case lists:member(<<"config-node">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_item(<<"notify_retract">> = _Pubsub_Option, Pubsub_Features,
  Xmlels) ->
    case lists:member(<<"delete-items">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_item(<<"persist_items">> = _Pubsub_Option, Pubsub_Features,
  Xmlels) ->
    case lists:member(<<"persistent-items">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_item(<<"presence_based_delivery">> = _Pubsub_Option,
  Pubsub_Features, Xmlels) ->
    case lists:member(<<"presence-notifications">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
%parse_pubsub_option_item(<<"publish_model">> = _Pubsub_Option,
%  Pubsub_Features, Xmlels) ->
%    case lists:member(<<"publish">>, Pubsub_Features) of
%        true ->
%            parse_xmlel_value({'list-single', 'atom',
%                _Default_Values = ['open', 'publishers', 'subscribers']},
%                Xmlels);
%        false ->
%            {error, 'invalid-options'}
%    end;
%%
parse_pubsub_option_item(<<"purge_offline">> = _Pubsub_Option, Pubsub_Features,
  Xmlels) ->
    case lists:member(<<"purge-nodes">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_item(<<"roster_groups_allowed">> = _Pubsub_Option,
  Pubsub_Features, [] = _Xmlels) ->
    case lists:member(<<"access-roster">>, Pubsub_Features) of
        true  -> {ok, []};
        false -> {error, 'invalid-options'}
    end;
parse_pubsub_option_item(<<"roster_groups_allowed">> = _Pubsub_Option,
  Pubsub_Features, Xmlels) ->
    case lists:member(<<"access-roster">>, Pubsub_Features) of
        true  -> parse_xmlel_value('list-multi', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_item(<<"send_last_published_item">> = _Pubsub_Option,
  Pubsub_Features, Xmlels) ->
    case lists:member(<<"last-published">>, Pubsub_Features) of
        true ->
            parse_xmlel_value({'list-single', 'atom',
                _Default_Values = case
                    lists:member(<<"presence-notifications">>, Pubsub_Features)
                of
                    true  -> ['never', 'on_sub', 'on_sub_and_presence'];
                    false -> ['never', 'on_sub']
                end},
                Xmlels);
        false ->
            {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_item(<<"type">> = _Pubsub_Option, Pubsub_Features,
  [] = _Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true  -> {ok, undefined};
        false -> {error, 'invalid-options'}
    end;
parse_pubsub_option_item(<<"type">> = _Pubsub_Option, Pubsub_Features, Xmlels) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true  -> parse_xmlel_value({'text-single', 'atom'}, Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_item(_Pubsub_Option, _Pubsub_Features, _Xmlels) ->
    {error, 'invalid-options'}.

%%
%%
-spec(parse_pubsub_option_subscription/5 ::
(
  Pubsub_Option   :: binary(),
  Node_Type       :: 'collection' | 'leaf',
  Entity_Type     :: 'local' | 'remote',
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Xmlels          :: [Xmlel_Value::xmlel()]
                   | [Xmlel::xmlel()])
    ->
   %   {ok, Digest             :: boolean()}
   % | {ok, Digest_Frequency   :: undefined | non_neg_integer()}
       {ok, Deliver            :: boolean()}
     | {ok, Expire             :: undefined | 'presence' | erlang:timestamp()}
%    | {ok, Include_Body       :: boolean()}
     | {ok, Show_Values        :: ['away' | 'chat' | 'dnd' | 'online' | 'xa']}
%    | {ok, Subscription_Type  :: 'all' | 'items' | 'nodes'}
%    | {ok, Subscription_Depth :: 'all' | non_neg_integer()}
    %%%
     | {error, 'invalid-options'}
     %
     | {error, 'not-acceptable'}
).

%% parse_pubsub_option_subscription(<<"digest">> = _Pubsub_Option,
%%  'leaf' = _Node_Type, _Entity_Type, Pubsub_Features, Xmlels) ->
%%    case lists:member(<<"subscribe">>, Pubsub_Features) of
%%       true ->
%%            parse_xmlel_value('boolean', Xmlels);
%%        false ->
%%            {error, 'invalid-options'}
%%    end;
%%
%% parse_pubsub_option_subscription(<<"digest_frequency">> = _Pubsub_Option,
%%  'leaf' = _Node_Type, _Entity_Type, Pubsub_Features, Xmlels) ->
%%    case lists:member(<<"subscribe">>, Pubsub_Features) of
%%       true ->
%%            parse_xmlel_value({'text-single', 'integer'}, Xmlels);
%%        false ->
%%            {error, 'invalid-options'}
%%    end;
%%
parse_pubsub_option_subscription(<<"deliver">> = _Pubsub_Option, Node_Type,
  _Entity_Type, Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"subscribe">>, Pubsub_Features) of
        true  -> parse_xmlel_value('boolean', Xmlels);
        false -> {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_subscription(<<"expire">> = _Pubsub_Option, Node_Type,
  'local' = _Entity_Type, Pubsub_Features, [] = _Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case
        lists:member(<<"presence-notifications">>, Pubsub_Features)
            orelse
        lists:member(<<"leased-subscriptions">>, Pubsub_Features)
    of
        true  -> {ok, undefined};
        false -> {error, 'invalid-options'}
    end;
parse_pubsub_option_subscription(<<"expire">> = _Pubsub_Option, Node_Type,
  'local' = _Entity_Type, Pubsub_Features, [Xmlel_Value])
  when    Node_Type == 'collection' orelse Node_Type == 'leaf'
  andalso ?Is_Xmlel_Value(Xmlel_Value) ->
    case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
        {?NS_XDATA, <<>>} ->
            {ok, undefined};
        {?NS_XDATA, <<"presence">>} ->
            case lists:member(<<"presence-notifications">>, Pubsub_Features) of
                true  -> {ok, 'presence'};
                false -> {error, 'invalid-options'}
            end;
        {?NS_XDATA, CData} ->
            case lists:member(<<"leased-subscriptions">>, Pubsub_Features) of
                true ->
                    case jlib:datetime_string_to_timestamp(CData) of
                        undefined -> {error, 'not-acceptable'};
                        DateTime  -> {ok, DateTime}
                    end;
                false ->
                    {error, 'invalid-options'}
            end;
        _ ->
            {error, 'invalid-options'}
    end;
%
parse_pubsub_option_subscription(<<"expire">> = _Pubsub_Option, Node_Type,
  'remote' = _Entity_Type, Pubsub_Features, [] = _Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"leased-subscriptions">>, Pubsub_Features) of
        true  -> {ok, undefined};
        false -> {error, 'invalid-options'}
    end;
parse_pubsub_option_subscription(<<"expire">> = _Pubsub_Option, Node_Type,
  'remote' = _Entity_Type, Pubsub_Features, [Xmlel_Value])
  when    Node_Type == 'collection' orelse Node_Type == 'leaf'
  andalso ?Is_Xmlel_Value(Xmlel_Value) ->
    case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
        {?NS_XDATA, <<>>} ->
            {ok, undefined};
        {?NS_XDATA, <<"presence">>} ->
            {error, 'invalid-options'};
        {?NS_XDATA, CData} ->
            case lists:member(<<"leased-subscriptions">>, Pubsub_Features) of
                true ->
                    case jlib:datetime_string_to_timestamp(CData) of
                        undefined -> {error, 'not-acceptable'};
                        DateTime  -> {ok, DateTime}
                    end;
                false ->
                    {error, 'invalid-options'}
            end;
        _ ->
            {error, 'invalid-options'}
    end;
%%
%parse_pubsub_option_subscription(<<"include_body">> = _Pubsub_Option,
%  'leaf' = _Node_Type, _Entity_Type, Pubsub_Features, Xmlels) ->
%    case lists:member(<<"subscribe">>, Pubsub_Features) of
%        true ->
%            parse_xmlel_value('boolean', Xmlels);
%        false ->
%            {error, 'invalid-options'}
%    end;
%%
parse_pubsub_option_subscription(<<"show-values">> = _Pubsub_Option, Node_Type,
  'local' = _Entity_Type, Pubsub_Features, [] = _Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"presence-notifications">>, Pubsub_Features) of
        true  -> {ok, []};
        false -> {error, 'invalid-options'}
    end;
parse_pubsub_option_subscription(<<"show-values">> = _Pubsub_Option, Node_Type,
  'local' = _Entity_Type, Pubsub_Features, Xmlels)
  when Node_Type == 'collection' orelse Node_Type == 'leaf' ->
    case lists:member(<<"presence-notifications">>, Pubsub_Features) of
        true ->
            parse_xmlel_value({'list-multi', 'atom',
                _Default_Values = ['away' ,'chat', 'dnd', 'online', 'xa']},
                Xmlels);
        false ->
            {error, 'invalid-options'}
    end;
%%
parse_pubsub_option_subscription(<<"show-values">> = _Pubsub_Option, _Node_Type,
  'remote' = _Entity_Type, _Pubsub_Features, _Xmlels) ->
    {error, 'invalid-options'};
%%
%parse_pubsub_option_subscription(<<"subscription_type">> = _Pubsub_Option,
%  'collection' = _Node_Type, _Entity_Type, _Pubsub_Features, Xmlels) ->
%    parse_xmlel_value({'list-single', 'atom',
%        _Default_Values = ['all', 'items', 'nodes']}, Xmlels);
%%
%parse_pubsub_option_subscription(<<"subscription_depth">> = _Pubsub_Option,
%  'collection' = _Node_Type, _Entity_Type, _Pubsub_Features, Xmlels) ->
%    case Xmlels of
%        [] ->
%            {ok, 0};
%        [Xmlel_Value] when ?Is_Xmlel_Value(Xmlel_Value) ->
%            case {Xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
%                {?NS_XDATA, <<>>} ->
%                    {ok, 0};
%                {?NS_XDATA, <<"all">>} ->
%                    {ok, 'all'};
%                {?NS_XDATA, CData} ->
%                    case catch list_to_integer(binary_to_list(CData)) of
%                        Integer
%                          when is_integer(Integer) andalso Integer > -1 ->
%                            {ok, Integer};
%                        _Error ->
%                            {error, 'not-acceptable'}
%                    end;
%                _ ->
%                    {error, 'not-acceptable'}
%            end;
%        _Xmlels ->
%            {error, 'not-acceptable'}
%    end;
%%
parse_pubsub_option_subscription(_Pubsub_Option, _Node_Type, _Entity_Type,
  _Pubsub_Features, _Xmlels) ->
    {error, 'invalid-options'}.


-spec(parse_xmlel_value/2 ::
(
  DataType     :: {'jid-multi',   'bare'}
                | {'jid-multi',   'full'}
                | {'list-multi', 'atom', Default_Values::[Default_Value::atom(),...]}
                | 'list-multi'
                | 'text-multi'
                | {'access_model', Pubsub_Features::exmpp_pubsub:pubsub_features()}
                | 'boolean'
                | 'text-single'
                | {'text-single', 'atom'}
                | {'text-single', 'integer'}
                | {'list-single', 'atom', Default_Values::[Default_Value::atom(),...]},
  Xmlels       :: [Xmlel_Value::xmlel()]
                | [Xmlel::xmlel()])
    -> %%
       {ok, Values :: []
                    | [Value::binary()]
                    | [Value::atom()]
                    | [Value::xmpp_jid:raw_jid_entity_bare()]
                    | [Value::xmpp_jid:raw_jid_entity()]}
     | {ok, Value :: atom()
                   | binary()
                   | boolean()
                   | non_neg_integer()
                   | undefined}
    %%%
     | {error, 'invalid-options'}
     | {error, 'jid-malformed'}
     | {error, 'not-acceptable'}
     | {error, 'not-acceptable', 'unsupported-access-model'}
).

%%
parse_xmlel_value({'jid-multi', 'bare'}, [] = _Xmlels) ->
    {ok, []};
%%
parse_xmlel_value({'jid-multi', 'bare'}, Xmlels) ->
    parse_xmlel_value({'jid-multi', 'bare'}, Xmlels, []);
%%
parse_xmlel_value({'jid-multi', 'full'}, [] = _Xmlels) ->
    {ok, []};
%%
parse_xmlel_value({'jid-multi', 'full'}, Xmlels) ->
    parse_xmlel_value({'jid-multi', 'full'}, Xmlels, []);
%%
%%
parse_xmlel_value({'list-multi', 'atom', _Default_Values}, [] = _Xmlels) ->
    {error, 'not-acceptable'};
%%
parse_xmlel_value({'list-multi', 'atom', Default_Values}, Xmlels) ->
    parse_xmlel_value({'list-multi', 'atom', Default_Values}, Xmlels, []);
%%
parse_xmlel_value('list-multi', Xmlels) ->
    parse_xmlel_value('list-multi', Xmlels, []);
%%
%%
parse_xmlel_value('text-multi', [] = _Xmlels) ->
    {ok, []};
%%
parse_xmlel_value('text-multi', Xmlels) ->
    parse_xmlel_value('text-multi', Xmlels, []);
%%

parse_xmlel_value({'access_model', Pubsub_Features}, [Xmlel_Value])
  when ?Is_Xmlel_Value(Xmlel_Value) ->
    case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
        {?NS_XDATA, <<"authorize">>} ->
            case lists:member(<<"access-authorize">>, Pubsub_Features) of
                true  ->
                    {ok, 'authorize'};
                false ->
                    {error, 'not-acceptable', 'unsupported-access-model'}
            end;
        %%
        {?NS_XDATA, <<"open">>} ->
            case lists:member(<<"access-open">>, Pubsub_Features) of
                true  ->
                    {ok, 'open'};
                false ->
                    {error, 'not-acceptable', 'unsupported-access-model'}
            end;
        %%
        {?NS_XDATA, <<"presence">>} ->
            case lists:member(<<"access-presence">>, Pubsub_Features) of
                true  ->
                    {ok, 'presence'};
                false ->
                    {error, 'not-acceptable', 'unsupported-access-model'}
            end;
        %%
        {?NS_XDATA, <<"roster">>} ->
            case lists:member(<<"access-roster">>, Pubsub_Features) of
                true  ->
                    {ok, 'roster'};
                false ->
                    {error, 'not-acceptable', 'unsupported-access-model'}
            end;
        %%
        {?NS_XDATA, <<"whitelist">>} ->
            case lists:member(<<"access-whitelist">>, Pubsub_Features) of
                true  ->
                    {ok, 'whitelist'};
                false ->
                    {error, 'not-acceptable', 'unsupported-access-model'}
            end;
        %%
        _ ->
            {error, 'not-acceptable'}
    end;
parse_xmlel_value({'access_model', _Pubsub_Features}, _Xmlels) ->
    {error, 'not-acceptable'};
%%
%%
parse_xmlel_value('boolean', [Xmlel_Value])
  when ?Is_Xmlel_Value(Xmlel_Value) ->
    case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
        {?NS_XDATA, False}
          when   False == <<"0">>
          orelse False == <<"false">> -> 
            {ok, false};
        {?NS_XDATA, True}
          when   True == <<"1">>
          orelse True == <<"true">> -> 
            {ok, true};
        _ ->
            {error, 'not-acceptable'}
    end;
parse_xmlel_value('boolean', _Xmlels) ->
    {error, 'not-acceptable'};
%%
%%
parse_xmlel_value('text-single', [] = _Xmlels) ->
    {ok, undefined};
parse_xmlel_value('text-single', [Xmlel_Value])
  when ?Is_Xmlel_Value(Xmlel_Value) ->
    case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
        {?NS_XDATA, <<>>}  -> {ok, undefined};
        {?NS_XDATA, CData} -> {ok, CData};
        _                  -> {error, 'not-acceptable'}
    end;
parse_xmlel_value('text-single', _Xmlels) ->
    {error, 'not-acceptable'};
%%
%%
parse_xmlel_value({'text-single', 'atom'}, [] = _Xmlels) ->
    {ok, undefined};
parse_xmlel_value({'text-single', 'atom'}, [Xmlel_Value])
  when ?Is_Xmlel_Value(Xmlel_Value) ->
    case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
        {?NS_XDATA, <<>>}  -> {ok, undefined};
        {?NS_XDATA, CData} -> {ok, list_to_atom(binary_to_list(CData))};
        _                  -> {error, 'not-acceptable'}
    end;
parse_xmlel_value({'text-single', 'atom'}, _Xmlels) ->
    {error, 'not-acceptable'};
%%
%%
parse_xmlel_value({'text-single', 'integer'}, [] = _Xmlels) ->
    {ok, undefined};
parse_xmlel_value({'text-single', 'integer'}, [Xmlel_Value])
  when ?Is_Xmlel_Value(Xmlel_Value) ->
    case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
        {?NS_XDATA, <<>>}  ->
            {ok, undefined};
        {?NS_XDATA, CData} ->
            case catch list_to_integer(binary_to_list(CData)) of
                Integer when is_integer(Integer) andalso Integer > -1 ->
                    {ok, Integer};
                _ ->
                    {error, 'not-acceptable'}
            end;
        _ ->
            {error, 'not-acceptable'}
    end;
parse_xmlel_value({'text-single', 'integer'}, _Xmlels) ->
    {error, 'not-acceptable'};
%%
%%
parse_xmlel_value({'list-single', 'atom', Default_Values}, [Xmlel_Value])
  when ?Is_Xmlel_Value(Xmlel_Value) ->
    case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
        {_, <<>>} ->
            {error, 'not-acceptable'};
        {?NS_XDATA, CData} ->
            case
                lists:member(Value = list_to_atom(binary_to_list(CData)),
                    Default_Values)
            of
                true  ->
                    {ok, Value};
                false ->
                    {error, 'not-acceptable'}
            end;
        _ ->
            {error, 'not-acceptable'}
    end;
parse_xmlel_value({'list-single', 'atom', _Default_Values}, _Xmlels) ->
    {error, 'not-acceptable'};
%%
parse_xmlel_value({_DataType, _Default_Values}, _Xmlels) ->
    {error, 'not-acceptable'}.
%%

%%
-spec(parse_xmlel_value/3 ::
(
  DataType :: {'list-multi', 'atom', Default_Values::[Default_Value::atom(),...]}
            | 'list-multi'
            | 'text-multi'
            | {'jid-multi', 'full'}
            | {'jid-multi', 'bare'},
  Xmlels   :: [Xmlel_Value::xmlel()]
            | [Xmlel::xmlel()],
  Values   :: [Value::binary()]
            | [Value::atom()]
            | [Value::xmpp_jid:raw_jid_entity_bare()]
            | [Value::xmpp_jid:raw_jid_entity_full()])
    -> {ok,
        Values :: [Value::binary()]
                | [Value::atom()]
                | [Value::xmpp_jid:raw_jid_entity_bare()]
                | [Value::xmpp_jid:raw_jid_entity_full()]}
    %%%
     | {error, 'not-acceptable'}
     | {error, 'jid-malformed'}
).

parse_xmlel_value(_DataType, _Xmlels = [], Values) ->
    {ok, lists:usort(Values)};
%%
%%
parse_xmlel_value({'list-multi', 'atom', Default_Values},
  [Xmlel_Value | Xmlels], Values)
  when ?Is_Xmlel_Value(Xmlel_Value) ->
    case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
        {?NS_XDATA, <<>>} ->
            parse_xmlel_value({'list-multi', 'atom', Default_Values},
                Xmlels, Values);
        {?NS_XDATA, CData} ->
            case
                lists:member(Value = list_to_atom(binary_to_list(CData)),
                    Default_Values)
            of
                true ->
                    parse_xmlel_value({'list-multi', 'atom', Default_Values},
                        Xmlels, [Value | Values]);
                false ->
                    {error, 'not-acceptable'}
            end;
        _ ->
            {error, 'not-acceptable'}
    end;
%%
%%
parse_xmlel_value('list-multi', [Xmlel_Value | Xmlels], Values)
  when ?Is_Xmlel_Value(Xmlel_Value) ->
    parse_xmlel_value('list-multi', Xmlels,
       case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
           {?NS_XDATA, <<>>}  -> Values;
           {?NS_XDATA, CData} -> [CData | Values];
           _                  -> {error, 'not-acceptable'}
       end);
%%
%%
parse_xmlel_value('text-multi', [Xmlel_Value | Xmlels] , Values)
  when ?Is_Xmlel_Value(Xmlel_Value) ->
    parse_xmlel_value('text-multi', Xmlels,
      case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
          {?NS_XDATA, <<>>}  -> Values;
          {?NS_XDATA, CData} -> [_Value = CData | Values];
          _                  -> {error, 'not-acceptable'}
      end);
%%
%%
parse_xmlel_value({'jid-multi', 'full'}, [Xmlel_Value | Xmlels], Values)
  when ?Is_Xmlel_Value(Xmlel_Value) ->
    case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
        {?NS_XDATA, <<>>} ->
            parse_xmlel_value({'jid-multi', 'full'}, Xmlels, Values);
        {?NS_XDATA, CData} ->
            case is_jid(CData) of
                #jid{} = Jid ->
                    parse_xmlel_value({'jid-multi', 'full'}, Xmlels,
                        [jlib:jid_to_string(Jid) | Values]);
                Error ->
                    Error
            end;
        _ ->
            {error, 'not-acceptable'}
    end;
%%
parse_xmlel_value({'jid-multi', 'bare'}, [Xmlel_Value | Xmlels], Values)
  when ?Is_Xmlel_Value(Xmlel_Value) ->
    case {xmlns(Xmlel_Value), get_tag_cdata(Xmlel_Value)} of
        {?NS_XDATA, <<>>} ->
            parse_xmlel_value({'jid-multi', 'bare'}, Xmlels, Values);
        {?NS_XDATA, CData} ->
            case is_jid(CData) of
                #jid{} = Jid ->
                    parse_xmlel_value({'jid-multi', 'bare'}, Xmlels,
                        [jlib:jid_to_string(Jid#jid{resource = <<>>, lresource = <<>>})
                        | Values]);
                Error ->
                    Error
          end;
        _ ->
            {error, 'not-acceptable'}
    end;
%%
parse_xmlel_value(_Data_Type, _Xmlels, _Values) ->
    {error, 'not-acceptable'}.

%%
%%
-spec(xdata_x/6 ::
(
  Options_Type    :: 'node_config' | 'subscribe_options',
  Form_Type       :: 'form' | 'result',
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Host            :: xmpp_jid:raw_jid_component_bare(),
  Entity          :: xmpp_jid:usr_bare(),
  Options         :: pubsub_options:options_node()
                   | pubsub_options:options_subscription())
    -> Xmlel_X::xmlel()
).

xdata_x('node_config', 'form', Pubsub_Features, Host, Entity, Node_Options) ->
    xmpp_xdata:xmlel_x(<<"form">>,
        _Xmlels_Field = [
            xmpp_xdata:xmlel_field(<<"FORM_TYPE">>, <<"hidden">>,
                [?NS_PUBSUB_NODE_CONFIG])
           |xdata_field(Pubsub_Features, Host, Entity, Node_Options, [])
        ]);

xdata_x('subscribe_options', 'form', Pubsub_Features, Host, Entity,
  Subscription_Options) ->
    xmpp_xdata:xmlel_x(<<"form">>,
        _Xmlels_Field = [
            xmpp_xdata:xmlel_field(<<"FORM_TYPE">>, <<"hidden">>,
                [?NS_PUBSUB_SUBSCRIBE_OPTIONS])
           |xdata_field(Pubsub_Features, Host, Entity, Subscription_Options, [])
        ]);

xdata_x('subscribe_options', 'result', Pubsub_Features, Host, Entity,
  Subscription_Options) ->
    xmpp_xdata:xmlel_x(<<"result">>,
        _Xmlels_Field = [
            xmpp_xdata:xmlel_field(<<"FORM_TYPE">>, <<"hidden">>,
                [?NS_PUBSUB_SUBSCRIBE_OPTIONS])
           |xdata_field(Pubsub_Features, Host, Entity, Subscription_Options, [])
        ]).

%%
%%
-spec(xdata_x/7 ::
(
  Options_Type                 :: 'subscribe_options',
  Form_Type                    :: 'form',
  Pubsub_Features              :: exmpp_pubsub:pubsub_features(),
  Host                         :: xmpp_jid:raw_jid_component_bare(),
  Entity                       :: xmpp_jid:usr_bare(),
  Subscription_Options         :: [] | pubsub_options:options_subscription(),
  Default_Subscription_Options :: pubsub_options:options_subscription())
    -> Xmlel_X::xmlel()
).


%%
xdata_x('subscribe_options', 'form', Pubsub_Features, Host, Entity,
  Subscription_Options, Default_Subscription_Options) ->
    xmpp_xdata:xmlel_x(<<"form">>,
        _Xmlels_Field = [
            xmpp_xdata:xmlel_field(<<"FORM_TYPE">>, <<"hidden">>,
                [?NS_PUBSUB_SUBSCRIBE_OPTIONS])
           |xdata_field(Pubsub_Features, Host, Entity,
                filter_subscription_options(Subscription_Options,
                    Default_Subscription_Options, []),
                [])
        ]).

%%
-spec(filter_subscription_options/3 ::
(
  Options                      :: [] | pubsub_options:options_subscription(),
  Default_Subscription_Options :: pubsub_options:options_subscription(),
  Subscription_Options         :: [] | pubsub_options:options_subscription())
    -> Subscription_Options::pubsub_options:options_subscription()
).

filter_subscription_options([] = _Options, Default_Subscription_Options,
  Subscription_Options) ->
    Default_Subscription_Options ++ Subscription_Options;
%%
filter_subscription_options([{Key, Value} | Options], Default_Subscription_Options,
  Subscription_Options) ->
    filter_subscription_options(Options,
        lists:keydelete(Key, 1, Default_Subscription_Options),
        [{Key, Value} | Subscription_Options]).


-define(Label_Deliver, <<"Enable delivery ?">>).
-define(Label_Digest, <<"Receive digest notifications ?">>).
-define(Label_Digest_Frequency, <<"Receive digest notifications (approx. one per day)?">>).
-define(Label_Expire, <<"Requested lease period">>).
-define(Label_Include_Body, <<"Receive message body in addition to payload ?">>).
-define(Label_Show_Values, <<"Select the presence types which are allowed to receive event notifications">>).

%%
%% pubsub#subscribe_options
%%
-spec(xdata_field/5 ::
(
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Host            :: xmpp_jid:raw_jid_component_bare(),
  Entity          :: xmpp_jid:usr_bare(),
  Options         :: [] | pubsub_options:options_subscription(),
  Xmlels_Field    :: [] | [Xmlel_Field::xmlel(),...])
    -> Xmlels_Field::[Xmlel_Field::xmlel(),...]
).
%%
xdata_field(_Pubsub_Features, _Host, _Entity, [] = _Options, Xmlels_Field) ->
    lists:sort(Xmlels_Field);
%% 'pubsub#deliver'
xdata_field(Pubsub_Features, Host, Entity, [{'deliver', Deliver} | Options],
  Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_boolean(
             <<"pubsub#deliver">>,
             <<"Enable delivery?">>,
             xdata_value('boolean', Deliver))
        | Xmlels_Field]);
%% 'pubsub#digest'
xdata_field(Pubsub_Features, Host, Entity, [{'digest', Digest} | Options],
  Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_boolean(
             <<"pubsub#digest">>,
             <<"Receive digest notifications (approx. one per day)?">>,
             xdata_value('boolean', Digest))
        | Xmlels_Field]);
%% 'pubsub#digest_frequency'
xdata_field(Pubsub_Features, Host, Entity,
  [{'digest_frequency', Digest_Frequency} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_text_single(
            <<"pubsub#digest_frequency">>,
            <<"Receive digest notifications (approx. one per day)?">>,
            xdata_value('integer', Digest_Frequency))
        | Xmlels_Field]);
%% 'pubsub#expire'
xdata_field(Pubsub_Features, Host, Entity,
  [{'expire', Expire} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_text_single(
            <<"pubsub#expire">>,
            <<"Requested lease period">>,
            case is_atom(Expire) of
                true  -> xdata_value('atom', Expire);
                false -> xdata_value('date', Expire)
            end)
        | Xmlels_Field]);
%% 'pubsub#include_body'
xdata_field(Pubsub_Features, Host, Entity,
  [{'include_body', Include_Body} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_boolean(
             <<"pubsub#include_body">>,
             <<"Receive message body in addition to payload?">>,
             xdata_value('boolean', Include_Body))
        | Xmlels_Field]);
%% 'pubsub#show-values'
xdata_field(Pubsub_Features, Host, Entity,
  [{'show-values', Show_Values} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_list_multi(
            <<"pubsub#show-values">>,
            <<"Select the presence types which are allowed to receive event notifications">>,
            lists:map(fun
                ('away')   -> <<"away">>;
                ('chat')   -> <<"chat">>;
                ('dnd')    -> <<"dnd">>;
                ('online') -> <<"online">>;
                ('xa')     -> <<"xa">>
            end, Show_Values),
            [{<<"away">>,   <<"Away">>},
             {<<"chat">>,   <<"Want to Chat">>},
             {<<"dnd">>,    <<"Do Not Disturb">>},
             {<<"online">>, <<"Available">>},
             {<<"xa">>,     <<"Extended Away">>}])
        | Xmlels_Field]);
%% 'pubsub#subscription_type'
xdata_field(Pubsub_Features, Host, Entity,
  [{'subscription_type', Subscription_Type} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_list_single(
            <<"pubsub#subscription_type">>,
            <<"Subscription Type">>,
            case Subscription_Type of
                undefined -> <<"items">>;
                'items'   -> <<"items">>;
                'nodes'   -> <<"nodes">>;
                'all'     -> <<"all">>
            end,
            [{<<"items">>, <<"Receive notification of items only">>},
             {<<"nodes">>, <<"Receive notification of new nodes only">>},
             {<<"all">>,   <<"Receive notification of items and nodes">>}])
        | Xmlels_Field]);
%% 'pubsub#subscription_depth'
xdata_field(Pubsub_Features, Host, Entity,
  [{'subscription_depth', Subscription_Depth} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_list_single(
            <<"pubsub#subscription_depth">>,
            <<"How far to traverse the node graph for notifications">>,
            case Subscription_Depth of
                undefined -> <<"1">>;
                'all'     -> <<"all">>;
                _         -> xdata_value('integer', Subscription_Depth)

            end,
            [{<<"1">>,   <<"Receive notification from direct child nodes only">>},
             {<<"all">>, <<"Receive notification from all descendent nodes">>}])
        | Xmlels_Field]);
%%
%% pubsub#node_config
%%
%% 'pubsub#access_model'
xdata_field(Pubsub_Features, Host, Entity,
  [{'access_model', Access_Model} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_list_single(
            <<"pubsub#access_model">>,
            <<"Specify the subscriber model">>,
            case Access_Model of
               % undefined  -> <<"open">>;
                'authorize' -> <<"authorize">>;
                'open'      -> <<"open">>;
                'presence'  -> <<"presence">>;
                'roster'    -> <<"roster">>;
                'whitelist' -> <<"whitelist">>
            end,
            xdata_access_model(Pubsub_Features,
                ['authorize', 'open', 'presence', 'roster', 'whitelist']))
        | Xmlels_Field]);
%% 'pubsub#body_xslt'
xdata_field(Pubsub_Features, Host, Entity,
  [{'body_xslt', Xslt} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_text_single(
            <<"pubsub#body_xslt">>,
            <<"The URL of an XSL transformation which can be applied to "
              "payloads in order to generate an appropriate message body element">>,
            Xslt)
        | Xmlels_Field]);
%% 'pubsub#children_association_policy'
xdata_field(Pubsub_Features, Host, Entity,
  [{'children_association_policy', Children_Association_Policy} | Options],
  Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_list_single(
            <<"pubsub#children_association_policy">>,
            <<"Who may associate leaf nodes with a collection">>,
            case Children_Association_Policy of
                undefined   -> <<"owners">>;
                'owners'    -> <<"owners">>;
                'all'       -> <<"all">>;
                'whitelist' -> <<"whitelist">>
            end,
            [{<<"all">>,       <<"Anyone may associate leaf nodes with the collection">>},
             {<<"owners">>,    <<"Only collection node owners may associate leaf"
                                 " nodes with the collection">>},
             {<<"whitelist">>, <<"Only those on a whitelist may associate leaf "
                                 "nodes with the collection">>}])
        | Xmlels_Field]);
%% 'pubsub#children_association_whitelist'
xdata_field(Pubsub_Features, Host, Entity,
  [{'children_association_whitelist', Children_Association_Whitelist}
  | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_jid_multi(
             <<"pubsub#children_association_whitelist">>,
             <<"The list of JIDs that may associate leaf nodes with a collection">>,
             Children_Association_Whitelist)
        | Xmlels_Field]);
%% 'pubsub#children'
xdata_field(Pubsub_Features, Host, Entity,
  [{'children', Children} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_text_multi(
             <<"pubsub#children">>,
             <<"The child nodes (leaf or collection) associated with a collection">>,
             Children)
        | Xmlels_Field]);
%% 'pubsub#children_max'
xdata_field(Pubsub_Features, Host, Entity,
  [{'children_max', Children_Max} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_text_single(
            <<"pubsub#children_max">>,
            <<"The maximum number of child nodes that can be associated with a collection">>,
            xdata_value('integer', Children_Max))
        | Xmlels_Field]);
%% 'pubsub#collection'
xdata_field(Pubsub_Features, Host, Entity,
  [{'collection', Collection} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_text_multi(
             <<"pubsub#collection">>,
             <<"The collection(s) with which a node is affiliated">>,
             Collection)
        | Xmlels_Field]);
%% 'pubsub#contact'
xdata_field(Pubsub_Features, Host, Entity,
  [{'contact', Contacts} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_jid_multi(
             <<"pubsub#contact">>,
             <<"The JIDs of those to contact with questions">>,
             Contacts)
        | Xmlels_Field]);
%% 'pubsub#dataform_xslt'
xdata_field(Pubsub_Features, Host, Entity,
  [{'dataform_xslt', Dataform_Xslt} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_text_single(
            <<"pubsub#dataform_xslt">>,
            <<"Payload XSLT">>,
            xdata_value({'text-multi', 'atom'}, Dataform_Xslt))
        | Xmlels_Field]);
%% 'pubsub#deliver_notifications'
xdata_field(Pubsub_Features, Host, Entity,
  [{'deliver_notifications', Deliver_Notifications} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_boolean(
             <<"pubsub#deliver_notifications">>,
             <<"Whether to deliver event notifications">>,
             xdata_value('boolean', Deliver_Notifications))
        | Xmlels_Field]);
%% 'pubsub#deliver_payloads'
xdata_field(Pubsub_Features, Host, Entity,
  [{'deliver_payloads', Deliver_Payloads} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_boolean(
             <<"pubsub#deliver_payloads">>,
             <<"Whether to deliver payloads with event notifications">>,
             xdata_value('boolean', Deliver_Payloads))
        | Xmlels_Field]);
%% 'pubsub#description'
xdata_field(Pubsub_Features, Host, Entity,
  [{'description', Description} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_text_single(
            <<"pubsub#description">>,
            <<"A description of the node">>,
            Description)
        | Xmlels_Field]);
%% 'pubsub#item_expire'
xdata_field(Pubsub_Features, Host, Entity,
  [{'item_expire', Item_Expire} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_text_single(
            <<"pubsub#item_expire">>,
            <<"Number of seconds after which to automatically purge items">>,
            xdata_value('integer', Item_Expire))
        | Xmlels_Field]);
%% 'pubsub#itemreply'
xdata_field(Pubsub_Features, Host, Entity,
  [{'itemreply', ItemReply} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_list_single(
            <<"pubsub#itemreply">>,
            <<"Whether owners or publisher should receive replies to items">>,
            case ItemReply of
                undefined   -> <<"publisher">>;
                'owner'     -> <<"owner">>;
                'publisher' -> <<"publisher">>
            end,
            [{<<"owner">>, <<"Statically specify a replyto of the node owner(s)">>},
             {<<"publisher">>, <<"Dynamically specify a replyto of the item publisher">>}])
        | Xmlels_Field]);
%% 'pubsub#language'
xdata_field(Pubsub_Features, Host, Entity,
  [{'language', Language} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_list_single(
            <<"pubsub#language">>,
            <<"The default language of the node">>,
            case Language of
                undefined -> <<"en">>;
                _Language  -> Language
            end,
            [{<<"en">>, <<"English">>}
             %{<<"es">>, <<"Español">>},
             %{<<"fr">>, <<"Français">>},
             %{<<"pl">>, <<"Polski">>},
             %{<<"ru">>, <<"Русский">>}
             ])
        | Xmlels_Field]);
%% 'pubsub#max_items'
xdata_field(Pubsub_Features, Host, Entity,
  [{'max_items', Max_Items} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_text_single(
            <<"pubsub#max_items">>,
            <<"The maximum number of items to persist">>,
            xdata_value('integer', Max_Items))
        | Xmlels_Field]);
%% 'pubsub#max_payload_size'
xdata_field(Pubsub_Features, Host, Entity,
  [{'max_payload_size', Max_Payload_Size} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_text_single(
            <<"pubsub#max_payload_size">>,
            <<"The maximum payload size in bytes">>,
            xdata_value('integer', Max_Payload_Size))
        | Xmlels_Field]);
%% 'pubsub#node_type'
xdata_field(Pubsub_Features, Host, Entity,
  [{'node_type', Node_Type} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host,
        Entity, Options,
        [case lists:member(<<"collections">>, Pubsub_Features) of
             true ->
                 xmpp_xdata:xmlel_field_list_single(
                     <<"pubsub#node_type">>,
                     <<"Whether the node is a leaf (default) or a collection">>,
                     case Node_Type of
                         undefined    -> <<"leaf">>;
                         'leaf'       -> <<"leaf">>;
                         'collection' -> <<"collection">>
                     end,
                     [{<<"leaf">>, <<"The node is a leaf node (default)">>},
                      {<<"collection">>, <<"The node is a collection node">>}]);
             false ->
                 %% Collections not supported -> non-modifiable field
%                 xmpp_xdata:xmlel_field_fixed(
%                     <<"pubsub#node_type">>,
%                     <<"The node is a leaf node (default)">>,
%                     <<"leaf">>)
                 xmpp_xdata:xmlel_field_list_single(
                     <<"pubsub#node_type">>,
                     <<"Whether the node is a leaf (default) or a collection">>,
                     <<"leaf">>,
                     [{<<"leaf">>, <<"The node is a leaf node (default)">>}])
         end
        |Xmlels_Field]);
%% 'pubsub#notification_type'
xdata_field(Pubsub_Features, Host, Entity,
  [{'notification_type', Notification_Type} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_list_single(
            <<"pubsub#notification_type">>,
            <<"Specify the delivery style for event notifications">>,
            case Notification_Type of
                undefined  -> <<"headline">>;
                'headline' -> <<"headline">>;
                'normal'   -> <<"normal">>;
                'chat'     -> <<"chat">>
            end,
            [{<<"headline">>, <<"Messages of type headline">>},
             {<<"normal">>,   <<"Messages of type normal">>}
            %{<<"chat">>,     <<"Messages of type chat">>}
            %{<<"presence">>, <<"Presence">>}
            %{<<"iq">>,       <<"Iq">>}
             ])
        | Xmlels_Field]);
%% 'pubsub#notify_config'
xdata_field(Pubsub_Features, Host, Entity,
  [{'notify_config', Notify_Config} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_boolean(
             <<"pubsub#notify_config">>,
             <<"Notify owners and subscribers when the node configuration changes">>,
             xdata_value('boolean', Notify_Config))
        | Xmlels_Field]);
%% 'pubsub#notify_delete'
xdata_field(Pubsub_Features, Host, Entity,
  [{'notify_delete', Notify_Delete} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_boolean(
             <<"pubsub#notify_delete">>,
             <<"Notify owners and subscribers when the node is deleted">>,
             xdata_value('boolean', Notify_Delete))
        | Xmlels_Field]);
%% 'pubsub#notify_retract'
xdata_field(Pubsub_Features, Host, Entity,
  [{'notify_retract', Notify_Retract} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_boolean(
             <<"pubsub#notify_retract">>,
             <<"Notify subscribers when items are removed from the node">>,
             xdata_value('boolean', Notify_Retract))
        | Xmlels_Field]);
%% 'pubsub#notify_sub'
xdata_field(Pubsub_Features, Host, Entity,
  [{'notify_sub', Notify_Sub} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_boolean(
             <<"pubsub#notify_sub">>,
             <<"Notify owners about new subscribers and unsubscribes">>,
             xdata_value('boolean', Notify_Sub))
        | Xmlels_Field]);
%% 'pubsub#persist_items'
xdata_field(Pubsub_Features, Host, Entity,
  [{'persist_items', Persist_Items} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_boolean(
             <<"pubsub#persist_items">>,
             <<"Persist items to storage">>,
             xdata_value('boolean', Persist_Items))
        | Xmlels_Field]);
%% 'pubsub#presence_based_delivery'
xdata_field(Pubsub_Features, Host, Entity,
  [{'presence_based_delivery', Presence_Based_Delivery} | Options]
 , Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_boolean(
             <<"pubsub#presence_based_delivery">>,
             <<"Deliver event notifications only to available users">>,
             xdata_value('boolean', Presence_Based_Delivery))
        | Xmlels_Field]);
%% 'pubsub#publish_model'
xdata_field(Pubsub_Features, Host, Entity,
  [{'publish_model', Publish_Model} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_list_single(
            <<"pubsub#publish_model">>,
            <<"Specify the publisher model">>,
            case Publish_Model of
               % undefined    -> <<"open">>;
                'open'        -> <<"open">>;
                'publishers'  -> <<"publishers">>;
                'subscribers' -> <<"subscribers">>
            end,
            [{<<"publishers">>,  <<"Only publishers may publish">>},
             {<<"subscribers">>, <<"Subscribers may publish">>},
             {<<"open">>,        <<"Anyone may publish">>}])
        | Xmlels_Field]);
%% 'pubsub#purge_offline'
xdata_field(Pubsub_Features, Host, Entity,
  [{'purge_offline', Purge_Offline} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_boolean(
             <<"pubsub#purge_offline">>,
             <<"Purge all items when the relevant publisher goes offline ?">>,
             xdata_value('boolean', Purge_Offline))
        | Xmlels_Field]);
%% 'pubsub#roster_groups_allowed'
xdata_field(Pubsub_Features, Host, Entity,
  [{'roster_groups_allowed', Rosters_Groups_Allowed} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_list_multi(
             <<"pubsub#roster_groups_allowed">>,
             <<"Roster group(s) allowed to subscribe and retrieve items">>,
             case lists:keyfind(Entity, 1, Rosters_Groups_Allowed) of
                 false                    -> [];
                 {_Entity, Roster_Groups} -> Roster_Groups
             end,
             xdata_roster(Host, Entity))
        | Xmlels_Field]);
%% 'pubsub#send_last_published_item'
xdata_field(Pubsub_Features, Host, Entity,
  [{'send_last_published_item', Send_Last_Published_Item} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_list_single(
            <<"pubsub#send_last_published_item">>,
            <<"When to send the last published item">>,
            case Send_Last_Published_Item of
                undefined             -> <<"on_sub_and_presence">>;
                'never'               -> <<"never">>;
                'on_sub_and_presence' -> <<"on_sub_and_presence">>;
                'on_sub'              -> <<"on_sub">>
            end,
            [{<<"never">>,               <<"Never">>},
             {<<"on_sub">>,              <<"When a new subscription is processed">>},
             {<<"on_sub_and_presence">>, <<"When a new subscription is processed "
                                           "and whenever a subscriber comes online">>}])
        | Xmlels_Field]);
%% 'pubsub#subscribe'
xdata_field(Pubsub_Features, Host, Entity,
  [{'subscribe', Subscribe} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_boolean(
             <<"pubsub#subscribe">>,
             <<"Whether to allow subscriptions">>,
             xdata_value('boolean', Subscribe))
        | Xmlels_Field]);
%% 'pubsub#tempsub'
xdata_field(Pubsub_Features, Host, Entity,
  [{'tempsub', Tempsub} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_boolean(
             <<"pubsub#tempsub">>,
             <<"Make all subscriptions temporary, based on subscriber presence">>,
             xdata_value('boolean', Tempsub))
        | Xmlels_Field]);
%% 'pubsub#title'
xdata_field(Pubsub_Features, Host, Entity,
  [{'title', Title} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_text_single(
            <<"pubsub#title">>,
            <<"A friendly name for the node">>,
            Title)
        | Xmlels_Field]);
%% 'pubsub#type'
xdata_field(Pubsub_Features, Host, Entity,
  [{'type', Type} | Options], Xmlels_Field) ->
    xdata_field(Pubsub_Features, Host, Entity, Options,
        [xmpp_xdata:xmlel_field_text_single(
             <<"pubsub#type">>,
             <<"Specify the type of payload data to be provided at this node">>,
             Type)
        | Xmlels_Field]).



%%
-spec(xdata_value/2 ::
(
  DataType :: 'atom'
            | 'boolean'
            | 'date'
            | 'integer'
            | {'text-multi', 'atom'},
  Value    :: undefined
            | atom()
            | boolean()
            | erlang:timestamp()
            | non_neg_integer()
            | [atom()])
    -> Xdata_Value :: binary() | [binary(),...]
).

xdata_value(_, undefined)     -> undefined;
%%
xdata_value('atom', Value)    -> list_to_binary(atom_to_list(Value));
%%
xdata_value('boolean', false) -> <<"false">>;
xdata_value('boolean', true)  -> <<"true">>;
%%
xdata_value('date', TimeStamp) ->
    {DateTime, _} = jlib:timestamp_to_iso(calendar:now_to_datetime(TimeStamp), utc),
    list_to_binary(DateTime ++ "Z");
%%
xdata_value('integer', Value) -> list_to_binary(integer_to_list(Value));
%%
xdata_value({'text-multi', 'atom'}, Values) ->
    [list_to_binary(atom_to_list(Value)) || Value <- Values].

%%
-spec(xdata_access_model/3 ::
(
  Access_Model   :: pubsub_options:access_model(),
  Pubsub_Feature :: boolean(),
  Access_Models  :: [{binary(), binary()}])
    -> Access_Models :: [{binary(), binary()}]
).

xdata_access_model(_Access_Model, false, Access_Models) ->
    Access_Models;
%%
xdata_access_model('authorize' = _Access_Model, true, Access_Models) ->
    [{<<"authorize">>, <<"Subscription requests must be approved and only "
                         "subscribers may retrieve items">>}
    | Access_Models];
%%
xdata_access_model('open' = _Access_Model, true, Access_Models) ->
    [{<<"open">>, <<"Anyone may subscribe and retrieve items">>}
    | Access_Models];
%%
xdata_access_model('presence' = _Access_Model, true, Access_Models) ->
    [{<<"presence">>, <<"Anyone with a presence subscription of both or from may "
                        "subscribe and retrieve items">>}
    | Access_Models];
%%
xdata_access_model('roster' = _Access_Model, true, Access_Models) ->
    [{<<"roster">>, <<"Anyone in the specified roster group(s) may subscribe and "
                      "retrieve items">>}
    | Access_Models];
%%
xdata_access_model('whitelist' = _Access_Model, true, Access_Models) ->
    [{<<"whitelist">>, <<"Only those on a whitelist may subscribe and retrieve items">>}
    | Access_Models].

%%
-spec(xdata_access_model/2 ::
(
  Pubsub_Features      :: exmpp_pubsub:pubsub_features(),
  Default_Access_Model :: ['authorize'|'open'|'presence'|'roster'|'whitelist',...])
    -> Access_Models :: [{binary(), binary()},...]
).

xdata_access_model(Pubsub_Features, Default_Access_Models) ->
    lists:foldr(fun
        ('authorize', Access_Models) ->
            xdata_access_model('authorize',
                lists:member(<<"access-authorize">>, Pubsub_Features), Access_Models);
        %%
        ('open', Access_Models) ->
            xdata_access_model('open',
                lists:member(<<"access-open">>, Pubsub_Features), Access_Models);
        %%
        ('presence', Access_Models) ->
            xdata_access_model('presence',
                lists:member(<<"access-presence">>, Pubsub_Features), Access_Models);
        %%
        ('roster', Access_Models) ->
            xdata_access_model('roster',
                lists:member(<<"access-roster">>, Pubsub_Features), Access_Models);
        %%
        ('whitelist', Access_Models) ->
            xdata_access_model('whitelist',
                lists:member(<<"access-whitelist">>, Pubsub_Features), Access_Models)
        %
    end, [], Default_Access_Models).

%%

%% Local Entity
-spec(xdata_roster/2 ::
(
  Host   :: binary(),
  Entity :: xmpp_jid:usr_bare())
    -> Roster_Groups :: [Roster_Group::binary()]
).

xdata_roster(Host, {_, Host, _} = Entity) ->
    _Roster_Groups = lists:usort(lists:foldl(fun
        (#roster{groups = Groups}, Roster_Groups) ->
            lists:foldl(fun
                (Group, Roster_Groups_Bis) ->
                    [Group | Roster_Groups_Bis]
                end, Roster_Groups, Groups)
        end, [], _Roster = get_entity_roster(Entity)));
%% Remote Entity
xdata_roster(_Host, _Entity) ->
    [].
