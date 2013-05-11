

%-include_lib("exmpp/include/exmpp.hrl").
%-include_lib("exmpp/include/exmpp_jid.hrl").
-include("logger.hrl").
-include("ejabberd.hrl").
-include("mod_roster.hrl").
-include("jlib.hrl").
%-include("pubsub_api.hrl").

-define(NS_JABBER_CLIENT, <<"jabber:client">>).

%%
%% Pubsub Node
-record(pubsub_node_dev,
{
  id            ,%:: {Pubsub_Host :: exmpp_pubsub:host(),
                 %   NodeId      :: undefined | exmpp_pubsub:nodeId()},
  idx           ,%:: exmpp_pubsub:nodeIdx(),
  creation      ,%:: {DateTime::erlang:timestamp(), Entity::xmpp_jid:usr_entity()},
  level    = 0  ,%:: exmpp_pubsub:level(),
  owners   = [] ,%:: [Owner::xmpp_jid:usr_bare(),...],
  itemids  = [] ,%:: [ItemId::exmpp_pubsub:itemId()],
  options  = []  %:: pubsub_options:options_node()
}).

%%
%% Pubsub State
-record(pubsub_state_dev,
{
  id                     ,%:: {Entity  :: xmpp_jid:usr_bare(),
                          %   NodeIdx :: exmpp_pubsub:nodeIdx()},
  nodeidx                ,%:: exmpp_pubsub:nodeIdx(),
  affiliation   = 'none' ,%:: exmpp_pubsub:affiliation(),
  access                 ,%:: pubsub_options:access_model(),
  subscriptions = []     ,%:: [] | exmpp_pubsub:subscriptions(),
 %groups        = []     ,%:: [Roster_Group::binary()],
  itemids       = []      %:: [ItemId::exmpp_pubsub:itemId()]
}).

%%
%% Pubsub Item
-record(pubsub_item_dev,
{
  id           ,%:: {ItemId::exmpp_pubsub:itemId(), NodeIdx::exmpp_pubsub:nodeIdx()},
  nodeidx      ,%:: exmpp_pubsub:nodeIdx(),
  owners = []  ,%:: [Owner::xmpp_jid:usr_bare(),...],
  creation     ,%:: {DateTime::erlang:timestamp(), Entity::xmpp_jid:usr_entity()},
  modification ,%:: {DateTime::erlang:timestamp(), Entity::xmpp_jid:usr_entity()},
  payload      ,%:: exmpp_pubsub:payload(),
  options = []  %:: [] | pubsub_options:options_item()
}).

%%
%% Pubsub Last Item
-record(pubsub_last_item_dev,
{
  nodeidx      ,%:: exmpp_pubsub:nodeIdx(),
  id           ,%:: exmpp_pubsub:itemId(),
  owners  = [] ,%:: [Owner::xmpp_jid:usr_bare(),...],
  creation     ,%:: {DateTime::erlang:timestamp(), Entity::xmpp_jid:usr_entity()},
  payload      ,%:: exmpp_pubsub:payload(),
  options = []  %:: [] | pubsub_options:options_item()
}).

%%
%% Pubsub Index
-record(pubsub_index_dev,
{
  index :: exmpp_pubsub:index(),
  last  :: exmpp_pubsub:nodeIdx(),
  free  :: [exmpp_pubsub:nodeIdx()]
}).

%%
%% Pubsub_Subscription_Pending
-record(pubsub_subscription_pending,
{
  id      ,%:: {Entity::xmpp_jid:usr_bare(), NodeIdx::exmpp_pubsub:nodeIdx()}
  nodeidx ,%:: exmpp_pubsub:nodeIdx()
  subids   %:: [SubId::exmpp_pubsub:subId(),...]
}).



%%
%% Internal data structures
%%
-record(entity,
{
  id            :: xmpp_jid:usr_bare(),
  local         :: boolean(),
  affiliation   :: 'member' | 'owner' | 'publisher',
  subscriptions :: []%[exmpp_pubsub:subscription(),...]
}).

%%
-record(node,
{
  id                           :: exmpp_pubsub:nodeId(),
  owners                       :: [Node_Owner::xmpp_jid:usr_bare(),...],
  access_model                 :: pubsub_options:access_model(),
% itemreply                    :: 'owner' | 'publisher',
% notification_type            :: 'headline' | 'normal',
% presence_based_delivery      :: boolean(),
  rosters_groups_allowed  = [] :: [] | pubsub_options:rosters_groups_allowed()
}).

%%
-record(item,
{
  access_model                ,%:: undefined | pubsub_options:access_model(),
  presence_based_delivery     ,%:: undefined | boolean(),
  rosters_groups_allowed  = [],%:: [] | pubsub_options:rosters_groups_allowed()
  stanza                       %::#xmlel{}
}).

%%
-record(cache,
{
  %% Entity is online
  presence               :: undefined | mod_pubsub_dev:presence_cache(),
  %% Entity has presence subscriptions with at least one node owner
  presence_subscriptions :: undefined | boolean(),
  %% Entity has presence subscriptions with at least one node owner
  %% and is contained in at least one roster group from the node owner
  rosters_groups         :: undefined | boolean() %% TODO : use a list
}).

%%
-record(subids,
{
  presence    = undefined :: undefined | [] | mod_pubsub_dev:resources_subids(),
  no_presence = undefined :: undefined | [] | mod_pubsub_dev:resources_subids()
}).

%%
-record(event,
{
  host      :: xmpp_jid:raw_jid_component_bare(),
  component :: xmpp_jid:component_bare(),
  entity    :: mod_pubsub_dev:entity(),
  node      :: mod_pubsub_dev:n0de(),
  cache     :: mod_pubsub_dev:cache(),
  subids    :: mod_pubsub_dev:subids()
}).

