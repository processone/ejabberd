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
%%% @copyright 2006-2012 ProcessOne
%%% @author Karim Gemayel <karim.gemayel@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @headerfile "pubsub_dev.hrl"

-module(pubsub_core).
-author('karim.gemayel@process-one.net').

-compile(export_all).

-include("pubsub_dev.hrl").
-include("pubsub_api.hrl").

-import(pubsub_tools,
[
  is_jid/1,
  get_option/2,
  get_option/3,
  get_value/2,
  get_value/3,
  set_value/3,
  %%
  check_access_model/3,
  check_access_model/7,
  check_publish_model/3,
  is_contact_subscribed_to_node_owners/3,
  is_contact_in_allowed_roster_groups/2,
  has_subscriptions/1
]).



-spec(features/3 ::
(
  Function        :: 'create_node'
                   | 'purge_node'
                   | 'publish_item'
                   | 'retract_item'
                   | 'get_items'
                   | 'subscribe_node'
                   | 'get_configure_subscription_default'
                   | 'get_configure_node_default',
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Criteria        :: %-- Create_Node --%
                     {NodeId      :: undefined | exmpp_pubsub:nodeId(),
                      Node_Config :: [] | [Xmlel::xmlel(),...]}
                  %% %-- Purge_Node --%
                   | 'none'
                  %% %-- Publish_Item --%
                   | {NodeId          :: undefined | exmpp_pubsub:nodeId(),
                      Node_Config     :: [] | [Xmlel::xmlel(),...],
                      Publish_Options :: [] | [Xmlel::xmlel(),...]}
                  %% %-- Retract_Item --%
                   | 'none'
                  %% %-- Get_Items --%
                   | 'none'
                  %% %-- Subscribe_Node --%
                   | {Subscribe_Options :: [Xmlel::xmlel()]}
                  %% %-- Get_Configure_Subscription_Default --%
                   | 'none'
                  %% %-- Get_Configure_Node_Default --%
                   | 'none'
                   )
    -> ok
    %%%
     %-- Create_Node --%
     | {error, 'feature-not-implemented', 'create-nodes'}
     | {error, 'feature-not-implemented', 'create-and-configure'}
     %
     | {error, 'not-acceptable', 'nodeid-required'}
     %-- Purge_Node --%
     | {error, 'feature-not-implemented', 'purge-nodes'}
     | {error, 'feature-not-implemented', 'persistent-items'}
     %-- Publish_Item --%
     | {error, 'feature-not-implemented', 'publish'}
     | {error, 'feature-not-implemented', 'auto-create'}
     | {error, 'feature-not-implemented', 'config-node'}
     | {error, 'feature-not-implemented', 'publish-options'}
     %-- Retract_Item --%
     | {error, 'feature-not-implemented', 'delete-items'}
     | {error, 'feature-not-implemented', 'persistent-items'}
     %-- Get_Items --%
     | {error, 'feature-not-implemented', 'retrieve-items'}
     | {error, 'feature-not-implemented', 'persistent-items'}
     %-- Subscribe_Node --%
     | {error, 'feature-not-implemented', 'subscribe'}
     | {error, 'feature-not-implemented', 'subscription-options'}
     %-- Get_Configure_Subscription_Default --%
     | {error, 'feature-not-implemented', 'subscription-options'}
     | {error, 'feature-not-implemented', 'retrieve-default-sub'}
     %-- Get_Configure_Node_Default --%
     | {error, 'feature-not-implemented', 'config-node'}
     | {error, 'feature-not-implemented', 'retrieve-default'}
).

%-- Create_Node --%
features('create_node', Pubsub_Features, {NodeId, [] = _Node_Config}) ->
    case lists:member(<<"create-nodes">>, Pubsub_Features) of
        true when NodeId =/= undefined ->
            ok;
        true ->
            case lists:member(<<"instant-nodes">>, Pubsub_Features) of
                true ->
                    ok;
                false ->
                    {error, 'not-acceptable', 'nodeid-required'}
            end;
        false ->
            {error, 'feature-not-implemented', 'create-nodes'}
    end;
%
features('create_node', Pubsub_Features, {NodeId, _Node_Config}) ->
    case features('create_node', Pubsub_Features, {NodeId, []}) of
        ok ->
            case lists:member(<<"create-and-configure">>, Pubsub_Features) of
                true ->
                    ok;
                false ->
                    {error, 'feature-not-implemented', 'create-and-configure'}
            end;
        Error ->
            Error
    end;
%-- Purge_Node --%
features('purge_node', Pubsub_Features, none) ->
    case lists:member(<<"purge-nodes">>, Pubsub_Features) of
        true ->
            case lists:member(<<"persistent-items">>, Pubsub_Features) of
                true ->
                    ok;
                false ->
                    {error, 'feature-not-implemented', 'persistent-items'}
            end;
        false ->
            {error, 'feature-not-implemented', 'purge-nodes'}
    end;
%-- Publish_Item --%
features('publish_item', Pubsub_Features,
  {NodeId, [] = _Node_Config, [] = _Publish_Options}) ->
    case lists:member(<<"publish">>, Pubsub_Features) of
        true when NodeId =/= undefined ->
            ok;
        true ->
            case lists:member(<<"auto-create">>, Pubsub_Features) of
                true ->
                    ok;
                false ->
                    {error, 'feature-not-implemented', 'auto-create'}
            end;
        false ->
            {error, 'feature-not-implemented', 'publish'}
    end;
%%
features('publish_item', Pubsub_Features,
  {NodeId, _Node_Config, [] = _Publish_Options}) ->
    case features('publish_item', Pubsub_Features, {NodeId, [], []}) of
        ok ->
            case lists:member(<<"config-node">>, Pubsub_Features) of
                true ->
                    ok;
                false ->
                    {error, 'feature-not-implemented', 'config-node'}
            end;
        Error ->
            Error
    end;
%
features('publish_item', Pubsub_Features,
  {NodeId, Node_Config, _Publish_Options}) ->
    case features('publish_item', Pubsub_Features, {NodeId, Node_Config, []}) of
        ok ->
            case lists:member(<<"publish-options">>, Pubsub_Features) of
                true ->
                    ok;
                false ->
                    {error, 'feature-not-implemented', 'publish-options'}
            end;
        Error ->
            Error
    end;
%-- Purge_Node --%
features('retract_item', Pubsub_Features, none) ->
    case lists:member(<<"delete-items">>, Pubsub_Features) of
        true ->
            case lists:member(<<"persistent-items">>, Pubsub_Features) of
                true ->
                    ok;
                false ->
                    {error, 'feature-not-implemented', 'persistent-items'}
            end;
        false ->
            {error, 'feature-not-implemented', 'delete-items'}
    end;
%-- Get_Items --%
features('get_items', Pubsub_Features, none) ->
    case lists:member(<<"retrieve-items">>, Pubsub_Features) of
        true ->
            case lists:member(<<"persistent-items">>, Pubsub_Features) of
                true ->
                    ok;
                false ->
                    {error, 'feature-not-implemented', 'persistent-items'}
            end;
        false ->
            {error, 'feature-not-implemented', 'retrieve-items'}
    end;
%-- Subscribe_Node --%
features('subscribe_node', Pubsub_Features, {[] = _Subscribe_Options}) ->
    case lists:member(<<"subscribe">>, Pubsub_Features) of
        true ->
            ok;
        false ->
            {error, 'feature-not-implemented', 'retrieve-items'}
    end;
%%
features('subscribe_node', Pubsub_Features, {_Subscribe_Options}) ->
    case lists:member(<<"subscribe">>, Pubsub_Features) of
        true ->
            case lists:member(<<"subscription-options">>, Pubsub_Features) of
                true ->
                    ok;
                false ->
                    {error, 'feature-not-implemented', 'subscription-options'}
            end;
        false ->
            {error, 'feature-not-implemented', 'retrieve-items'}
    end;
%-- Get_Configure_Subscription_Default --%
features('get_configure_subscription_default', Pubsub_Features, 'none') ->
    case lists:member(<<"subscription-options">>, Pubsub_Features) of
        true ->
            case lists:member(<<"retrieve-default-sub">>, Pubsub_Features) of
                true ->
                    ok;
                false ->
                    {error, 'feature-not-implemented', 'retrieve-default-sub'}
            end;
        false ->
            {error, 'feature-not-implemented', 'subscription-options'}
    end;
%-- Get_Configure_Node_Default --%
features('get_configure_node_default', Pubsub_Features, 'none') ->
    case lists:member(<<"config-node">>, Pubsub_Features) of
        true ->
            case lists:member(<<"retrieve-default">>, Pubsub_Features) of
                true ->
                    ok;
                false ->
                    {error, 'feature-not-implemented', 'retrieve-default'}
            end;
        false ->
            {error, 'feature-not-implemented', 'config-node'}
    end.


%-- Create_Node --%
-spec(create_node/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId :: undefined | exmpp_pubsub:nodeId(),
                   Node_Config :: [] | [Xmlel::xmlel(),...]},
  Capabilities :: #capabilities{})
    -> %%
       {result, [Xmlel_Pubsub::xmlel(),...]}
    %%%
     | {error, 'feature-not-implemented', 'create-nodes'}
     | {error, 'feature-not-implemented', 'create-and-configure'}
     %
     | {error, 'not-acceptable', 'nodeid-required'}
     %
     | {error, 'forbidden'}
     | {error, 'conflict'}
     %
     | {error, 'not-acceptable'}
     | {error, 'feature-not-implemented', 'collections'}
     %
     | {error, 'invalid-options'}
     | {error, 'jid-malformed'}
     %
     | {error, 'not-acceptable'}
     | {error, 'not-acceptable', 'unsupported-access-model'}
     %
     | {error, 'feature-not-implemented', 'collections'}
     | {error, 'feature-not-implemented', 'multi-collections'}
).


create_node(Host, Pubsub_Host, {U,S,R} = Entity,
  {NodeId, Node_Config} = _Parameters,
  #capabilities{
    plugin = Plugin,
    api = #api{db = API_DB, broadcast = API_Broadcast, options = Options_Mod}}
  = _Capabilities) ->
    case
        checks_on_create_node(Host, {U,S,R}, Plugin, {NodeId, Node_Config},
            Options_Mod)
    of
        {ok, Pubsub_Features, Node_Options} ->
            case
                pubsub_db:transaction('mnesia',
                    API_DB#api_db.create_node,
                    'create_node',
                    [Host, Pubsub_Host, {U,S,undefined}, Pubsub_Features,
                        {NodeId, Node_Options}]
                )
            of
                {result, NodeId_Bis, _Actions} ->
                    case
                        (_Actions =/= [])
                             andalso
                        lists:member(<<"subscription-notifications">>,
                            Pubsub_Features)
                    of
                        true ->
                            [{Notification_Type, Subscription}] = _Actions,
                            spawn(API_Broadcast
                                #api_broadcast.notify_subscriptions,
                                'notify_subscriptions',
                                [Host, Pubsub_Host, NodeId_Bis, Notification_Type,
                                    [{Entity, [Subscription]}]]);
                        false ->
                            ok
                    end,
                    {result,
                        [exmpp_pubsub:xmlel_pubsub('pubsub',
                            [exmpp_pubsub:xmlel_create(NodeId_Bis)])]};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.


checks_on_create_node(Host, {U,S,R} = _Entity, Plugin, {NodeId, Node_Config},
  Options_Mod) ->
    case
        features('create_node', Pubsub_Features = Plugin:pubsub_features(),
            {NodeId, Node_Config})
    of
        ok ->
            case
                acl:match_rule(Host, pubsub_createnode,
                    jlib:jid_to_string({U, S, <<>>}))
            of
                allow ->
                    case
                        Options_Mod:parse_xmlel_x(Plugin, Pubsub_Features,
                            {U,S,R}, 'node_config', Node_Config)
                    of
                        {ok, Node_Options} ->
                            {ok, Pubsub_Features, Node_Options};
                        Error ->
                            Error
                    end;
                _Deny ->
                    {error, 'forbidden'}
            end;
        Error ->
            Error
    end.

%-- Delete_Node --%
-spec(delete_node/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId :: undefined | exmpp_pubsub:nodeId(),
                   RedirectURI :: undefined | binary()},
  Capabilities :: #capabilities{})
    -> {result, []}
    %%%
     | {error, 'feature-not-implemented', 'delete-nodes'}
     %
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
).

delete_node(Host, Pubsub_Host, {U,S,_R} = _Entity,
   _Parameters = {NodeId, RedirectURI},
   #capabilities{plugin = Plugin, api = #api{db = API_DB, broadcast = API_Broadcast}}
   = _Capabilities) ->
     Pubsub_Features = Plugin:pubsub_features(),
    case lists:member(<<"delete-nodes">>, Pubsub_Features) of
        true ->
            case
                pubsub_db:transaction('mnesia',
                    API_DB#api_db.delete_node,
                    'delete_node',
                    [Pubsub_Host, {U,S,undefined}, {NodeId}])
            of
                {result, _Actions = []} ->
                    {result, []};
                {result,
                 _Actions = [{Notification_Type, Recipients}]} ->
                     spawn(API_Broadcast#api_broadcast.notify_delete,
                         'notify_delete',
                         [Host, Pubsub_Host, NodeId, Notification_Type,
                          RedirectURI, Recipients]),
                     {result, []};
                Error ->
                    Error
             end;
        false ->
            {error, 'feature-not-implemented', 'delete-nodes'}
     end.

%-- Purge_Node --%
-spec(purge_node/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId :: undefined | exmpp_pubsub:nodeId()},
  Capabilities :: #capabilities{})
    -> {result, []}
    %%%
     | {error, 'feature-not-implemented', 'purge-nodes'}
     | {error, 'feature-not-implemented', 'persistent-items'}
     %
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
).

purge_node(Host, Pubsub_Host, {U,S,_R} = _Entity,
  {NodeId} = _Parameters,
   #capabilities{plugin = Plugin, api = #api{db = API_DB, broadcast = API_Broadcast}}
   = _Capabilities) ->
    Pubsub_Features = Plugin:pubsub_features(),
    case features('purge_node', Pubsub_Features, none) of
        ok ->
            case
                pubsub_db:transaction('mnesia',
                    API_DB#api_db.purge_node,
                    'purge_node',
                    [Pubsub_Host, {U,S,undefined}, {NodeId}])
            of
                {result, []} ->
                    {result, []};
                {result,
                 _Actions = [{Notification_Type, Recipients}]} ->
                     spawn(API_Broadcast#api_broadcast.notify_purge,
                         'notify_purge',
                         [Host, Pubsub_Host, NodeId, Notification_Type, Recipients]),
                     {result, []};
                 Error ->
                     Error
            end;
        Error ->
            Error
    end.

%%
-spec(publish_item/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {
    NodeId          :: undefined | exmpp_pubsub:nodeId(),
    Item            :: 0 | 1,
    ItemId          :: undefined | exmpp_pubsub:itemId(),
    Payload         :: exmpp_pubsub:payload() | [Xmlel::xmlel()],
    Node_Config     :: [], %| [Xmlel::xmlel(),...],
    Publish_Options :: [Xmlel::xmlel()]
  },
  Capabilities :: #capabilities{})
    -> %%
       {result, Xmlel_Pubsub::[Xmlel::xmlel()]}
    %%%
     | {error, 'forbidden'}
     | {error, 'item-not-found'}
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
     %
     | {error, 'bad-request',    'invalid-payload'}
     | {error, 'bad-request',    'item-required'}
     | {error, 'bad-request',    'item-forbidden'}
     | {error, 'not-acceptable', 'payload-too-big'}
).
publish_item(Host, Pubsub_Host, {U,S,R} = _Entity,
  {_NodeId, Item, _ItemId, Payload, [] = Node_Config, Publish_Options},
   #capabilities{plugin = Plugin,
  api = #api{options = Options_Module, db = API_DB,
  broadcast = API_Broadcast}} = _Capabilities) ->
    case
        checks_on_publish_item(Host, {U,S,R}, Plugin, Options_Module,
            {_NodeId, Node_Config, Publish_Options})
    of
        {ok, Pubsub_Features, _Node_Options, Item_Options} ->
            case
                pubsub_db:transaction('mnesia',
                    API_DB#api_db.publish_item,
                    'publish_item',
                    [Host, Pubsub_Host, {U,S,R}, Plugin, Pubsub_Features,
                     {_NodeId, Item, _ItemId, Payload, {Node_Config, Item_Options}}])
            of
                {result, NodeId, ItemId, Node_Options, Node_Owners, SubId,
                 Subscription, Broadcasted_Items, Pubsub_States} ->
                    case Pubsub_States of
                        [] ->
                            ok;
                        _Pubsub_States ->
                            spawn(API_Broadcast#api_broadcast.broadcast_publish,
                                broadcast_publish,
                                [Host, Pubsub_Host, NodeId, Node_Options,
                                 Node_Owners, Broadcasted_Items,
                                 case SubId of
                                    undefined ->
                                        undefined;
                                    _SubId ->
                                        {_Subscriber = jlib:jid_to_string({U, S, <<>>}),
                                         _Subscription_State = 'subscribed',
                                         SubId}
                                 end,
                                 Pubsub_States])
                    end,
                    {result,
                        [exmpp_pubsub:xmlel_pubsub('pubsub',
                            [exmpp_pubsub:xmlel_publish('pubsub', NodeId,
                                [exmpp_pubsub:xmlel_item('pubsub', ItemId)])]
                        )]
                    };
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%%
-spec(checks_on_publish_item/5 ::
(
  Host           :: xmpp_jid:raw_jid_component_bare(),
  Entity         :: xmpp_jid:usr_entity(),
  Plugin         :: exmpp_pubsub:plugin(),
  Options_Module :: module(),
  Criteria       :: {
    NodeId          :: undefined | exmpp_pubsub:nodeId(),
    Node_Config     :: [Xmlel_X::xmlel(),...]
                     | [Xmlel::xmlel()],
    Publish_Options :: [Xmlel_X::xmlel(),...]
                     | [Xmlel::xmlel()]
  })
    -> {ok,
        Pubsub_Features ::exmpp_pubsub:pubsub_features(),
        Node_Options    :: [] | pubsub_options:options_node(),
        Item_Options    :: [] | pubsub_options:options_item()}
    %%%
     | {error, 'forbidden'}
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

checks_on_publish_item(Host, {U,S,R} = _Entity, Plugin, Options_Module,
  {undefined = NodeId, Node_Config, Publish_Options}) ->
    case
        features('publish_item', Pubsub_Features = Plugin:pubsub_features(),
            {NodeId, Node_Config, Publish_Options})
    of
        ok ->
            case
                acl:match_rule(Host, pubsub_createnode,
                    jlib:jid_to_string({U, S, <<>>}))
            of
                allow ->
                    case
                        {Options_Module:parse_xmlel_x(Plugin, Pubsub_Features,
                             {U,S,R}, 'node_config', Node_Config),
                         Options_Module:parse_xmlel_x(Plugin, Pubsub_Features,
                             {U,S,R}, 'publish-options', Publish_Options)}
                    of
                        {{ok, Node_Options}, {ok, Item_Options}} ->
                            {ok, Pubsub_Features, Node_Options, Item_Options};
                        {Error, {ok, _Item_Options}} ->
                            Error;
                        {{ok, _Node_Options}, Error} ->
                            Error;
                        {Error, _Error} ->
                            Error
                    end;
                _Deny ->
                    {error, 'forbidden'}
            end;
        Error ->
            Error
    end;
%%
checks_on_publish_item(_Host, Entity, Plugin, Options_Module,
  {NodeId, Node_Config, Publish_Options}) ->
    case
        features('publish_item', Pubsub_Features = Plugin:pubsub_features(),
            {NodeId, Node_Config, Publish_Options})
    of
        ok ->
            case
                {Options_Module:parse_xmlel_x(Plugin, Pubsub_Features, Entity,
                     'node_config', Node_Config),
                 Options_Module:parse_xmlel_x(Plugin, Pubsub_Features, Entity,
                     'publish-options', Publish_Options)}
            of
                {{ok, Node_Options}, {ok, Item_Options}} ->
                    {ok, Pubsub_Features, Node_Options, Item_Options};
                {Error, {ok, _Item_Options}} ->
                    Error;
                {{ok, _Node_Options}, Error} ->
                    Error;
                {Error, _Error} ->
                    Error
            end;
        Error ->
            Error
    end.

%-- Retract_Items --%
-spec(retract_item/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId               :: undefined | exmpp_pubsub:nodeId(),
                   ItemId               :: undefined | exmpp_pubsub:itemId(),
                   Adhoc_Notify_Retract :: undefined | boolean()},
  Capabilities :: #capabilities{})
    -> {result, []}
    %%%
     | {error, 'feature-not-implemented', 'delete-items'}
     | {error, 'feature-not-implemented', 'persistent-items'}
     %
     | {error, 'bad-request', 'nodeid-required'}
     | {error, 'bad-request', 'item-required'}
     %
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
).

retract_item(Host, Pubsub_Host, {U,S,_R} = _Entity,
  {NodeId, ItemId, Adhoc_Notify_Retract} = _Parameters,
   #capabilities{plugin = Plugin, api = #api{db = API_DB, broadcast = API_Broadcast}}
   = _Capabilities) ->
    Pubsub_Features = Plugin:pubsub_features(),
    case features('retract_item', Pubsub_Features, none) of
        ok ->
            case
                pubsub_db:transaction('mnesia',
                    API_DB#api_db.retract_item,
                    'retract_item',
                    [Pubsub_Host, {U,S,undefined},
                     {NodeId, ItemId, Adhoc_Notify_Retract}])
            of
                {result, []} ->
                    {result, []};
                {result,
                 [_Notify_Retract = {
                      Node_Owners, Node_Options, Item_Options, Pubsub_States
                  }]} ->
                    spawn(API_Broadcast#api_broadcast.broadcast_publish,
                        broadcast_publish,
                        [Host, Pubsub_Host, NodeId, Node_Options, Node_Owners,
                         {[{ItemId, Item_Options}], []}, undefined, Pubsub_States]),
                    {result, []};
                 Error ->
                     Error
            end;
        Error ->
            Error
    end.

%-- Subscribe_Node --%
-spec(subscribe_node/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId            :: undefined | exmpp_pubsub:nodeId(),
                   Jid               :: binary() | undefined,
                   Subscribe_Options :: [Xmlel::xmlel(),...]
                                      | [Xmlel::xmlel()]},
  Capabilities :: #capabilities{})
    -> {result, [Xmlel_Pubsub::xmlel(),...]}
    %%%
     | {error, 'feature-not-implemented', 'subscribe'}
     | {error, 'feature-not-implemented', 'subscription-options'}
     %
     | {error, 'bad-request', 'invalid-jid'}
     %
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
     %
     | {error, 'invalid-options'}
     | {error, 'not-acceptable'}
     %
     | {error, 'not-authorized', 'presence-subscription-required'}
     | {error, 'not-authorized', 'not-in-roster-group'}
     | {error, 'not-authorized', 'pending-subscription'}
     | {error, 'not-allowed',    'closed-node'}
).

subscribe_node(Host, Pubsub_Host, {U,S,_} = Entity,
  {NodeId, Jid, Subscribe_Options} = _Parameters,
   #capabilities{plugin = Plugin,
    api = #api{options = Options_Module, db = API_DB, broadcast = API_Broadcast}}
   = _Capabilities) ->
    Pubsub_Features = Plugin:pubsub_features(),
    case features('subscribe_node', Pubsub_Features, {Subscribe_Options}) of
        ok ->
            case is_jid(Jid) of
                #jid{luser = U, lserver = S, lresource = Resource} ->
                    case
                        pubsub_db:transaction('mnesia',
                            API_DB#api_db.subscribe_node,
                            'subscribe_node',
                            [Host, Pubsub_Host, {U,S,undefined}, Plugin, Options_Module,
                             Pubsub_Features,
                             {NodeId, Resource, Subscribe_Options}])
                    of
                        {result,
                         {Subscription_State, SubId, _Resource, _Subscription_Options}
                         = Subscription, Affiliation,
                         Node_Owners, Node_Options, Pubsub_Last_Item} ->
                            case Pubsub_Last_Item of
                                #pubsub_last_item_dev{
                                    id       = ItemId,
                                    creation = {DateTime, Creator},
                                    payload  = Payload,
                                    options  = Item_Options
                                } ->
                                    spawn(API_Broadcast#api_broadcast.broadcast_publish_last,
                                        broadcast_publish_last,
                                        [Host, Pubsub_Host, NodeId, Node_Options,
                                         Node_Owners,
                                        [{ItemId, Item_Options, Payload, Creator, DateTime}],
                                        Entity, Affiliation, Subscription]);
                                _ ->
                                    ok
                            end,
                            {result,
                             [exmpp_pubsub:xmlel_pubsub('pubsub',
                                  [exmpp_pubsub:xmlel_subscription('pubsub',
                                       NodeId, Jid, SubId, Subscription_State)])]};
                        Error ->
                            Error
                    end;
                _ ->
                    {error, 'bad-request', 'invalid-jid'}
            end;
        Error ->
            Error
    end.

%-- Unubscribe_Node --%
-spec(unsubscribe_node/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId :: undefined | exmpp_pubsub:nodeId(),
                   Jid    :: binary() | undefined,
                   _SubId :: undefined | exmpp_pubsub:subId()},
  Capabilities :: #capabilities{})
    -> {result, []}
    %%%
     | {error, 'feature-not-implemented', 'subscribe'}
     | {error, 'forbidden'}
     %
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
     | {error, 'unexpected',     'not-subscribed'}
     %
     | {error, 'unexpected',     'not-subscribed'}
     | {error, 'not-acceptable', 'invalid-subid'}
     | {error, 'bad-request',    'subid-required'}
     | {error, 'not-acceptable', 'invalid-subid'}
).

unsubscribe_node(Host, Pubsub_Host, {U,S,_} = Entity,
  {NodeId, Jid, _SubId} = _Parameters,
   #capabilities{plugin = Plugin, api = #api{db = API_DB, broadcast = API_Broadcast}}
   = _Capabilities) ->
    case lists:member(<<"subscribe">>, Pubsub_Features = Plugin:pubsub_features()) of
        true ->
            case is_jid(Jid) of
                #jid{luser = U, lserver = S, lresource = Resource} ->
                    case
                        pubsub_db:transaction('mnesia',
                            API_DB#api_db.unsubscribe_node,
                            'unsubscribe_node',
                            [Host, Pubsub_Host, {U,S,undefined},
                             {NodeId, Resource, _SubId}])
                    of
                        {result, SubId} ->
                            {result, []};
                        {result, SubId, Node_Owners, Notification_Type} ->
                            spawn(API_Broadcast#api_broadcast.notify_subscription,
                                notify_subscription,
                                [Host, NodeId, jlib:jid_to_string({<<>>, Host, <<>>}),
                                 Node_Owners, Notification_Type,
                                 {Jid, 'none', SubId}]),
                            {result, []};
                        Error ->
                            Error
                    end;
                _ ->
                    {error, 'forbidden'}
            end;
        false ->
            {error, 'feature-not-implemented', 'subscribe'}
    end.

%-- Get_Items --%
-spec(get_items/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId    :: undefined | exmpp_pubsub:nodeId(),
                   _SubId    :: undefined | exmpp_pubsub:subId(),
                   Max_Items :: non_neg_integer(),
                   ItemIds   :: undefined}
                | {NodeId    :: undefined | exmpp_pubsub:nodeId(),
                   _SubId    :: undefined | exmpp_pubsub:subId(),
                   Max_Items :: undefined,
                   ItemIds   :: [] | exmpp_pubsub:itemIds()},
  Capabilities :: #capabilities{})
    -> {result, [Xmlel_Pubsub::xmlel(),...]}
    %%%
     | {error, 'feature-not-implemented', 'retrieve-items'}
     | {error, 'feature-not-implemented', 'persistent-items'}
     %
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
     | {error, 'not-authorized', 'presence-subscription-required'}
     | {error, 'not-authorized', 'not-in-roster-group'}
     | {error, 'not-allowed',    'closed-node'}
).

get_items(Host, Pubsub_Host, {U,S,_} = _Entity,
  {NodeId, SubId, Max_Items, ItemIds},
   #capabilities{plugin = Plugin, api = #api{db = API_DB}} = _Capabilities) ->
    case features('get_items', Plugin:pubsub_features(), none) of
        ok ->
            case
                pubsub_db:transaction('mnesia',
                    API_DB#api_db.get_items,
                    'get_items',
                    [Host, Pubsub_Host, {U,S,undefined},
                     {NodeId, SubId, Max_Items, ItemIds}])
            of
                {result, Pubsub_Items, Cache, Node_Owners, Node_Access_Model,
                 Node_Groups, Affiliation, Access} ->
                    {result,
                     [exmpp_pubsub:xmlel_pubsub('pubsub',
                          [exmpp_pubsub:xmlel_items('pubsub', NodeId,
                               result_get_items(Host, Pubsub_Items,
                                   {{U,S,undefined}, Affiliation, Access},
                                   {Node_Owners, Node_Access_Model, Node_Groups},
                                   Cache, []))])]};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%%
-spec(result_get_items/6 ::
(
  Host :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Items :: [] | mod_pubsub_dev:pubsub_items(),
  Entity_Parameters :: {Entity      :: xmpp_jid:usr_bare(),
                        Affiliation :: 'member' | 'none' | 'owner' | 'publisher',
                        Access      :: undefined | 'presence' | 'roster' | 'authorize'},
  Node_Parameters   :: {Node_Owners       :: mod_pubsub_dev:node_owners(),
                        Node_Access_Model :: pubsub_options:access_model(),
                        Node_Groups       :: [Group::binary()]},
  Cache             :: {Presence_Cache :: undefined | boolean(),
                        Groups_Cache   :: undefined
                                        | [{Entity :: xmpp_jid:usr_bare(),
                                            Groups :: [Group::binary()]}]},
  Xmlels_Item       :: [Xmlel_Item::#xmlel{name::binary(), children::[]}])
    -> Xmlels_Item :: [Xmlel_Item::#xmlel{name::binary(), children::[]}]
).

result_get_items(_Host, [] = _Pubsub_Items, _Entity_Parameters, _Node_Parameters,
  _Cache, Xmlels_Item) ->
    Xmlels_Item;
%%
result_get_items(Host,
  [#pubsub_item_dev{id = {ItemId, _NodeIdx}, payload = Payload, options = Item_Options}
  | Pubsub_Items],
  {Entity, Affiliation, Access},
  {Node_Owners, Node_Access_Model, Node_Groups},
  Cache, Xmlels_Item)
  when   Item_Options == []
  orelse Affiliation  == 'owner' ->
    result_get_items(Host, Pubsub_Items,
        {Entity, Affiliation, Access},
        {Node_Owners, Node_Access_Model, Node_Groups},
        Cache,
        [exmpp_pubsub:xmlel_item('pubsub', ItemId, Payload)
        | Xmlels_Item]);
%%
result_get_items(Host,
  [#pubsub_item_dev{id = {ItemId, _NodeIdx}, payload = Payload, options = Item_Options} | Pubsub_Items],
  {Entity, Affiliation, Access},
  {Node_Owners, Node_Access_Model, Node_Groups},
  {Presence_Cache, Groups_Cache} = _Cache, Xmlels_Item) ->
    Item_Groups = get_value(Item_Options, 'roster_groups_allowed', []),
    case get_value(Item_Options, 'access_model', undefined) of
        %%
        Item_Access_Model
          when   Item_Access_Model == undefined
          orelse Item_Access_Model == 'open'
          orelse Item_Access_Model == 'authorize'
          orelse Item_Access_Model == 'whitelist'
          %
          orelse (Item_Access_Model == Node_Access_Model
              andalso
                  Item_Access_Model =/= 'roster')
          %
          orelse (Item_Access_Model == 'presence'
              andalso
                  (Access == 'presence'
                       orelse
                   Access == 'roster'
                       orelse
                   Presence_Cache == true))
          %
          orelse (Item_Access_Model == 'roster'
              andalso
                  Node_Access_Model == 'roster'
              andalso
                  Item_Groups == [])
          %
          orelse ((Item_Access_Model == 'authorize'
                       orelse
                   Item_Access_Model == 'whitelist')
              andalso
                  (Affiliation == 'member'
                       orelse
                   Affiliation == 'publisher')) ->
            result_get_items(Host, Pubsub_Items,
                {Entity, Affiliation, Access},
                {Node_Owners, Node_Access_Model, Node_Groups},
                {Presence_Cache, Groups_Cache},
                [exmpp_pubsub:xmlel_item('pubsub', ItemId, Payload)
                | Xmlels_Item]);
        %%
        Item_Access_Model
          when    Item_Access_Model == 'presence'
          andalso Presence_Cache    == undefined ->
            case is_contact_subscribed_to_node_owners(Host, Entity, Node_Owners) of
                false ->
                    result_get_items(Host, Pubsub_Items,
                        {Entity, Affiliation, Access},
                        {Node_Owners, Node_Access_Model, Node_Groups},
                        {_Presence_Cache = false, Groups_Cache},
                        Xmlels_Item);
                _Node_Owner ->
                    result_get_items(Host, Pubsub_Items,
                        {Entity, Affiliation, Access},
                        {Node_Owners, Node_Access_Model, Node_Groups},
                        {_Presence_Cache = true, Groups_Cache},
                        [exmpp_pubsub:xmlel_item('pubsub', ItemId, Payload)
                        | Xmlels_Item])
            end;
        Item_Access_Model
          when    Item_Access_Model == 'roster'
          andalso Groups_Cache      == undefined ->
            case is_contact_in_allowed_roster_groups(Entity, Item_Groups) of
                {Node_Owner, Roster_Group} ->
                    result_get_items(Host, Pubsub_Items,
                        {Entity, Affiliation, Access},
                        {Node_Owners, Node_Access_Model, Node_Groups},
                        {Presence_Cache, _Groups_Cache = [{Node_Owner, [Roster_Group]}]},
                        [exmpp_pubsub:xmlel_item('pubsub', ItemId, Payload)
                        | Xmlels_Item]);
                false ->
                    result_get_items(Host, Pubsub_Items,
                        {Entity, Affiliation, Access},
                        {Node_Owners, Node_Access_Model, Node_Groups},
                        {Presence_Cache, Groups_Cache},
                        Xmlels_Item)
            end;
        Item_Access_Model
          when Item_Access_Model == 'roster' ->
            case
                lists:any(fun
                    ({Owner, Groups}) ->
                        case lists:keyfind(Owner, 1, Item_Groups) of
                            {_Owner, Owner_Groups} ->
                                lists:any(fun
                                    (Group) ->
                                        lists:member(Group, Owner_Groups)
                                end, Groups);
                            false ->
                                false
                        end
                end, Groups_Cache)
            of
                true ->
                    result_get_items(Host, Pubsub_Items,
                        {Entity, Affiliation, Access},
                        {Node_Owners, Node_Access_Model, Node_Groups},
                        {Presence_Cache, Groups_Cache},
                        [exmpp_pubsub:xmlel_item('pubsub', ItemId, Payload)
                        | Xmlels_Item]);
                false ->
                    case is_contact_in_allowed_roster_groups(Entity, Node_Groups) of
                        {Node_Owner, Roster_Group} ->
                            result_get_items(Host, Pubsub_Items,
                                {Entity, Affiliation, Access},
                                {Node_Owners, Node_Access_Model, Node_Groups},
                                {Presence_Cache,
                                 _Groups_Cache = case
                                     lists:keyfind(Node_Owner, 1, Groups_Cache)
                                of
                                    {_Node_Owner, Node_Owner_Groups} ->
                                        lists:keyreplace(Node_Owner, 1, Groups_Cache,
                                            {Node_Owner,
                                             [Roster_Group
                                             | lists:delete(Roster_Group,  Node_Owner_Groups)]});
                                    false ->
                                        [{Node_Owner, [Roster_Group]} | Groups_Cache]
                                end},
                                [exmpp_pubsub:xmlel_item('pubsub', ItemId, Payload)
                                | Xmlels_Item]);
                        false ->
                            result_get_items(Host, Pubsub_Items,
                                {Entity, Affiliation, Access},
                                {Node_Owners, Node_Access_Model, Node_Groups},
                                {Presence_Cache, Groups_Cache},
                                Xmlels_Item)
                    end
            end;
        _ ->
            result_get_items(Host, Pubsub_Items,
                {Entity, Affiliation, Access},
                {Node_Owners, Node_Access_Model, Node_Groups},
                {Presence_Cache, Groups_Cache},
                Xmlels_Item)
    end.

%-- Set_Configure_Subscription --%
-spec(set_configure_subscription/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId            :: undefined | exmpp_pubsub:nodeId(),
                   Jid               :: binary() | undefined,
                   SubId             :: undefined | exmpp_pubsub:subId(),
                   Subscribe_Options :: [Xmlel::xmlel(),...]
                                      | [Xmlel::xmlel()]},
  Capabilities :: #capabilities{})
    -> {result, []}
    %%%
     | {error, 'feature-not-implemented', 'subscription-options'}
     %
     | {error, 'bad-request', 'invalid-options'}
     | {error, 'bad-request', 'jid-required'}
     | {error, 'bad-request', 'invalid-jid'}
     %
     | {error, 'item-not-found'}
     | {error, 'unexpected-request', 'not-subscribed'}
     | {error, 'forbidden'}
     | {error, 'bad-request', 'invalid-options'}
     %
     | {error, 'unexpected-request', 'not-subscribed'}
     | {error, 'bad-request',        'subid-required'}
     | {error, 'forbidden'}
     | {error, 'not-acceptable',     'invalid-subid'}
).

set_configure_subscription(Host, Pubsub_Host, {U,S,_} = Entity,
  {NodeId, Jid, SubId, Subscribe_Options},
   #capabilities{plugin = Plugin, api = #api{options = Options_Module, db = API_DB}}
   = _Capabilities) ->
    case
        checks_on_set_configure_subscription(Entity, Jid, Plugin,
            Subscribe_Options)
    of
        {ok, Pubsub_Features, Resource} ->
            case
                pubsub_db:transaction('mnesia',
                    API_DB#api_db.set_configure_subscription,
                    'set_configure_subscription',
                    [Host, Pubsub_Host, Entity, Plugin, Options_Module,
                    Pubsub_Features, {NodeId, SubId, Resource, Subscribe_Options}])
            of
                {result, []} ->
                    {result, []};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%%
-spec(checks_on_set_configure_subscription/4 ::
(
  Entity            :: xmpp_jid:usr_entity(),
  Subscriber_Jid    :: binary() | undefined,
  Plugin            :: exmpp_pubsub:plugin(),
  Subscribe_Options :: [Xmlel::xmlel(),...]
                     | [Xmlel::xmlel()])
    -> {ok,
        Pubsub_Features :: exmpp_pubsub:pubsub_features(),
        Resource        :: undefined | xmpp_jid:resource_jid()}
    %%%
     | {error, 'feature-not-implemented', 'subscription-options'}
     %
     | {error, 'bad-request', 'invalid-options'}
     | {error, 'bad-request', 'jid-required'}
     | {error, 'bad-request', 'invalid-jid'}
).

checks_on_set_configure_subscription({U,S,_} = _Entity, Subscriber_Jid, Plugin,
  Subscribe_Options) ->
    Pubsub_Features = Plugin:pubsub_features(),
    case lists:member(<<"subscription-options">>, Pubsub_Features) of
        true ->
            case Subscribe_Options of
                [] ->
                    {error, 'bad-request', 'invalid-options'};
                _ ->
                    case Subscriber_Jid of
                        undefined ->
                            {error, 'bad-request', 'jid-required'};
                        _ ->
                            case is_jid(Subscriber_Jid) of
                                #jid{luser = U, lserver = S, lresource = Resource} ->
                                    {ok, Pubsub_Features, Resource};
                                _ ->
                                    {error, 'bad-request', 'invalid-jid'}
                            end
                    end
            end;
        false ->
            {error, 'feature-not-implemented', 'subscription-options'}
    end.

%-- Get_Entity_Affiliations --%
-spec(get_entity_affiliations/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId :: undefined | exmpp_pubsub:nodeId()},
  Capabilities :: #capabilities{})
    -> {result, [Xmlel_Pubsub::xmlel(),...]}
    %%%
     | {error, 'feature-not-implemented', 'retrieve-affiliations'}
).

get_entity_affiliations(_Host, Pubsub_Host, {U,S,_R} = _Entity,
  {NodeId} = _Parameters,
   #capabilities{plugin = Plugin, api = #api{db = API_DB}} = _Capabilities) ->
    case
        lists:member(<<"retrieve-affiliations">>,
            _Pubsub_Features = Plugin:pubsub_features())
    of
        true ->
            {result,
             result_get_entity_affiliations(
                 _Entity_Affiliations = pubsub_db:transaction('mnesia',
                     API_DB#api_db.get_entity_affiliations,
                     'get_entity_affiliations',
                     [Pubsub_Host, {U,S,undefined}, {NodeId}]))};
        false ->
            {error, 'feature-not-implemented', 'retrieve-affiliations'}
    end.

%%
-spec(result_get_entity_affiliations/1 ::
(
  Entity_Affiliations :: [Entity_Affiliation :: {
                              NodeId :: exmpp_pubsub:nodeId(),
                              Affiliation :: exmpp_pubsub:affiliation()
                          }])
    -> [Xmlel_Pubsub::xmlel(),...]
).

result_get_entity_affiliations(Entity_Affiliations) ->
    [exmpp_pubsub:xmlel_pubsub('pubsub',
         [exmpp_pubsub:xmlel_affiliations('pubsub',
              lists:map(fun({NodeId, Affiliation}) ->
                  exmpp_pubsub:xmlel_affiliation('pubsub', NodeId, Affiliation)
              end, Entity_Affiliations)
         )]
    )].


%-- Get_Entity_Subscriptions --%
-spec(get_entity_subscriptions/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId :: undefined | exmpp_pubsub:nodeId()},
  Capabilities :: #capabilities{})
    -> {result, [Xmlel_Pubsub::xmlel(),...]}
    %%%
     | {error, 'feature-not-implemented', 'retrieve-subscriptions'}
).

get_entity_subscriptions(_Host, Pubsub_Host, {U,S,_R} = _Entity,
  {NodeId} = _Parameters,
   #capabilities{plugin = Plugin, api = #api{db = API_DB}} = _Capabilities) ->
    case
        lists:member(<<"retrieve-subscriptions">>,
            _Pubsub_Features = Plugin:pubsub_features())
    of
        true ->
            {result,
             result_get_entity_subscriptions({U,S,undefined},
                 _Entity_Subscriptions = pubsub_db:transaction('mnesia',
                     API_DB#api_db.get_entity_subscriptions,
                     'get_entity_subscriptions',
                     [Pubsub_Host, {U,S,undefined}, {NodeId}]))
            };
        false ->
            {error, 'feature-not-implemented', 'retrieve-subscriptions'}
    end.

%%
-spec(result_get_entity_subscriptions/2 ::
(
  Entity               :: xmpp_jid:usr_bare(),
  Entity_Subscriptions :: [{NodeId::exmpp_pubsub:nodeId(),
                            Subscriptions::exmpp_pubsub:subscriptions()}])
    -> [Xmlel_Pubsub::xmlel(),...]
).

result_get_entity_subscriptions({U,S,_R} = _Entity, Entity_Subscriptions) ->
    [exmpp_pubsub:xmlel_pubsub('pubsub',
         [exmpp_pubsub:xmlel_subscriptions('pubsub',
              lists:map(fun
                  ({NodeId, Subscriptions}) ->
                      lists:map(fun
                          %%
                          ({Subscription_State, SubId, {caps, Resource},
                            _Subscription_Options}) ->
                              exmpp_pubsub:xmlel_subscription('pubsub',
                                  NodeId,
                                  _Jid = jlib:jid_to_string({U, S, <<>>}),
                                  SubId,
                                  Subscription_State);
                          %%
                          ({Subscription_State, SubId, Resource,
                            _Subscription_Options}) ->
                              exmpp_pubsub:xmlel_subscription('pubsub',
                                  NodeId,
                                  _Jid = jlib:jid_to_string({U, S, Resource}),
                                  SubId,
                                  Subscription_State)
                          %
                      end, Subscriptions)
              end, Entity_Subscriptions)
         )]
    )].


%-- Get_Node_Affiliations --%
-spec(get_node_affiliations/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId :: undefined | exmpp_pubsub:nodeId()},
  Capabilities :: #capabilities{})
    -> {result, [Xmlel_Pubsub::xmlel(),...]}
    %%%
     | {error, 'feature-not-implemented', 'modify-affiliations'}
     %
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
).

get_node_affiliations(Host, Pubsub_Host, {U,S,_R} = _Entity,
  {NodeId} = _Parameters,
   #capabilities{plugin = Plugin, api = #api{db = API_DB}} = _Capabilities) ->
    case
        lists:member(<<"modify-affiliations">>,
            _Pubsub_Features = Plugin:pubsub_features())
    of
        true ->
            case
                pubsub_db:transaction('mnesia',
                    API_DB#api_db.get_node_affiliations,
                    'get_node_affiliations',
                    [Host, Pubsub_Host, {U,S,undefined}, {NodeId}])
            of
                {result, Pubsub_States} ->
                    {result, result_get_node_affiliations(NodeId, Pubsub_States)};
                Error ->
                    Error
            end;
        false ->
            {error, 'feature-not-implemented', 'modify-affiliations'}
    end.

%%
-spec(result_get_node_affiliations/2 ::
(
  NodeId        :: exmpp_pubsub:nodeId(),
  Pubsub_States :: [Pusbub_State::#pubsub_state_dev{},...])
    -> [Xmlel_Pubsub::xmlel(),...]
).

result_get_node_affiliations(NodeId, Pubsub_States) ->
    [exmpp_pubsub:xmlel_pubsub('pubsub#owner',
         [exmpp_pubsub:xmlel_affiliations('pubsub#owner', NodeId,
              lists:map(fun
                  (#pubsub_state_dev{id = {Entity, _NodeIdx},
                   affiliation = Affiliation}) ->
                      exmpp_pubsub:xmlel_affiliation('pubsub#owner',
                          _Jid = pubsub_tools:jid_to_string(Entity),
                          Affiliation)
              end, Pubsub_States)
         )]
    )].


%-- Get_Node_Subscriptions --%
-spec(get_node_subscriptions/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId :: undefined | exmpp_pubsub:nodeId()},
  Capabilities :: #capabilities{})
    -> {result, [Xmlel_Pubsub::xmlel(),...]}
    %%%
     | {error, 'feature-not-implemented', 'manage-subscriptions'}
     %
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
).

get_node_subscriptions(Host, Pubsub_Host, {U,S,_R} = _Entity,
  {NodeId} = _Parameters,
   #capabilities{plugin = Plugin, api = #api{db = API_DB}} = _Capabilities) ->
    case
        lists:member(<<"manage-subscriptions">>,
            _Pubsub_Features = Plugin:pubsub_features())
    of
        true ->
            case
                 pubsub_db:transaction('mnesia',
                    API_DB#api_db.get_node_subscriptions,
                    'get_node_subscriptions',
                    [Host, Pubsub_Host, {U,S,undefined}, {NodeId}])
            of
                {result, Pubsub_States} ->
                    {result, result_get_node_subscriptions(NodeId, Pubsub_States)};
                Error ->
                    Error
            end;
        false ->
            {error, 'feature-not-implemented', 'manage-subscriptions'}
    end.

%%
-spec(result_get_node_subscriptions/2 ::
(
  NodeId        :: exmpp_pubsub:nodeId(),
  Pubsub_States :: [Pubsub_State::#pubsub_state_dev{}])
    -> [Xmlel_Pubsub::xmlel(),...]
).

result_get_node_subscriptions(NodeId, Pubsub_States) ->
    [exmpp_pubsub:xmlel_pubsub('pubsub#owner',
         [exmpp_pubsub:xmlel_subscriptions('pubsub#owner', NodeId,
              lists:foldl(fun
                  %%
                  (#pubsub_state_dev{affiliation = 'none'}, Xmlels_Subscription) ->
                      Xmlels_Subscription;
                  %%
                      (#pubsub_state_dev{id = {{U, S, _}, _NodeIdx},
                       subscriptions = Subscriptions}, Xmlels_Subscription) ->
                          Xmlels_Subscription
                          ++
                          lists:map(fun
                              %%
                              ({Subscription_State, SubId, {caps, Resource},
                                _Subscription_Options}) ->
                                  exmpp_pubsub:xmlel_subscription('pubsub#owner',
                                      _Jid = pubsub_tools:jid_to_string({U, S, Resource}),
                                      SubId,
                                      Subscription_State);
                              %%
                              ({Subscription_State, SubId, Resource,
                                _Subscription_Options}) ->
                                  exmpp_pubsub:xmlel_subscription('pubsub#owner',
                                      _Jid = pubsub_tools:jid_to_string({U, S, Resource}),
                                      SubId,
                                      Subscription_State)
                          end, Subscriptions)
                  %
              end, [], Pubsub_States)
         )]
    )].

%% Get_Configure_Subscription_Default
-spec(get_configure_subscription_default/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId :: undefined | exmpp_pubsub:nodeId()},
  Capabilities :: #capabilities{})
    -> {result, [Xmlel_Pubsub::xmlel(),...]}
    %%%
     | {error, 'feature-not-implemented', 'subscription-options'}
     | {error, 'feature-not-implemented', 'retrieve-default-sub'}
     %
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
).

get_configure_subscription_default(Host, Pubsub_Host, {U,S,_R} = _Entity,
  {undefined = _NodeId} = _Parameters,
   #capabilities{plugin = Plugin, api = #api{options = Options_Module,
   db = API_DB}} = _Capabilities) ->
    Pubsub_Features = Plugin:pubsub_features(),
    case features('get_configure_subscription_default', Pubsub_Features, none) of
        ok ->
            {result,
             [exmpp_pubsub:xmlel_pubsub('pubsub',
                [exmpp_pubsub:xmlel_default('pubsub',
                     [Options_Module:xdata_x('subscribe_options', 'result',
                      Pubsub_Features, Host, {U,S,undefined},
                      _Subscription_Options = Plugin:default_subscription_options(
                          case Host of
                              S -> 'local';
                              _ -> 'remote'
                          end,
                         'leaf')
                      )]
                 )]
              )]
            };
        Error ->
            Error
    end;
%%
get_configure_subscription_default(Host, Pubsub_Host, {U,S,_R} = _Entity,
  {NodeId} = _Parameters,
   #capabilities{plugin = Plugin, api = #api{options = Options_Module,
   db = API_DB}} = _Capabilities) ->
    Pubsub_Features = Plugin:pubsub_features(),
    case features('get_configure_subscription_default', Pubsub_Features, none) of
        ok ->
            case
                 pubsub_db:transaction('mnesia',
                    API_DB#api_db.get_configure_subscription_default,
                    'get_configure_subscription_default',
                    [Host, Pubsub_Host, {U,S,undefined}, NodeId])
            of
                ok ->
                    {result,
                     [exmpp_pubsub:xmlel_pubsub('pubsub',
                        [exmpp_pubsub:xmlel_default('pubsub', NodeId,
                             [Options_Module:xdata_x('subscribe_options', 'result',
                              Pubsub_Features, Host, {U,S,undefined},
                              _Subscription_Options = Plugin:default_subscription_options(
                                  case Host of
                                      S -> 'local';
                                      _ -> 'remote'
                                  end,
                                 'leaf')
                              )]
                         )]
                      )]
                    };
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% Get_Configure_Subscription
-spec(get_configure_subscription/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId :: undefined | exmpp_pubsub:nodeId(),
                   Jid    :: binary() | undefined,
                   SubId  :: undefined | exmpp_pubsub:subId()},
  Capabilities :: #capabilities{})
    -> {result, [Xmlel_Pubsub::xmlel(),...]}
    %%%
     | {error, 'feature-not-implemented', 'subscription-options'}
     %
     | {error, 'item-not-found'}
     | {error, 'unexpected-request', 'not-subscribed'}
     | {error, 'forbidden'}
     %
     | {error, 'unexpected-request', 'not-subscribed'}
     | {error, 'bad-request',        'subid-required'}
     | {error, 'forbidden'}
     | {error, 'not-acceptable',     'invalid-subid'}
).

get_configure_subscription(Host, Pubsub_Host, {U,S,_R} = _Entity,
  {NodeId, Jid, SubId} = _Parameters,
   #capabilities{plugin = Plugin, api = #api{options = Options_Module,
   db = API_DB}} = _Capabilities) ->
    Pubsub_Features = Plugin:pubsub_features(),
    case checks_on_get_configure_subscription({U,S,undefined}, Jid, Plugin) of
        {ok, Pubsub_Features, Resource} ->
            case
                pubsub_db:transaction('mnesia',
                    API_DB#api_db.get_configure_subscription,
                    'get_configure_subscription',
                    [Host, Pubsub_Host, {U,S,undefined}, Plugin,
                     Pubsub_Features, {NodeId, SubId, Resource}])
            of
                {result, {_, _, _, Subscription_Options} = Subscription} ->
                    {result,
                     [exmpp_pubsub:xmlel_pubsub('pubsub',
                        [exmpp_pubsub:xmlel_options('pubsub', NodeId, Jid, SubId,
                             [Options_Module:xdata_x('subscribe_options', 'form',
                              Pubsub_Features, Host, {U,S,undefined},Subscription_Options,
                              _Default_Subscription_Options = Plugin:default_subscription_options(
                                  case Host of
                                      S -> 'local';
                                      _ -> 'remote'
                                  end,
                                 'leaf')
                              )]
                         )]
                      )]
                    };
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%%
-spec(checks_on_get_configure_subscription/3 ::
(
  Entity            :: xmpp_jid:usr_entity(),
  Subscriber_Jid    :: binary() | undefined,
  Plugin            :: exmpp_pubsub:plugin())
    -> {ok,
        Pubsub_Features :: exmpp_pubsub:pubsub_features(),
        Resource        :: undefined | xmpp_jid:resource_jid()}
    %%%
     | {error, 'feature-not-implemented', 'subscription-options'}
     %
     | {error, 'bad-request', 'jid-required'}
     | {error, 'bad-request', 'invalid-jid'}
).

checks_on_get_configure_subscription({U,S,_} = _Entity, Subscriber_Jid, Plugin) ->
    Pubsub_Features = Plugin:pubsub_features(),
    case lists:member(<<"subscription-options">>, Pubsub_Features) of
        true ->
            case Subscriber_Jid of
                undefined ->
                    {error, 'bad-request', 'jid-required'};
                _ ->
                    case is_jid(Subscriber_Jid) of
                        #jid{luser = U, lserver = S, lresource = Resource} ->
                            {ok, Pubsub_Features, Resource};
                        _ ->
                            {error, 'bad-request', 'invalid-jid'}
                    end
            end;
        false ->
            {error, 'feature-not-implemented', 'subscription-options'}
    end.

%% Get_Configure_Node_Default
-spec(get_configure_node_default/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId :: undefined | exmpp_pubsub:nodeId()},
  Capabilities :: #capabilities{})
    -> {result, [Xmlel_Pubsub::xmlel(),...]}
    %%%
     | {error, 'feature-not-implemented', 'config-node'}
     | {error, 'feature-not-implemented', 'retrieve-default'}
).

get_configure_node_default(Host, Pubsub_Host, {U,S,_R} = _Entity,
  {undefined = _NodeId} = _Parameters,
   #capabilities{plugin = Plugin, api = #api{options = Options_Module,
   db = API_DB}} = _Capabilities) ->
    Pubsub_Features = Plugin:pubsub_features(),
    case features('get_configure_node_default', Pubsub_Features, none) of
        ok ->
            {result,
             [exmpp_pubsub:xmlel_pubsub('pubsub#owner',
                [exmpp_pubsub:xmlel_default('pubsub#owner',
                     [Options_Module:xdata_x('node_config', 'form',
                      Pubsub_Features, Host, {U,S,undefined},
                      _Node_Options = Plugin:node_options('leaf')
                      )]
                 )]
              )]
            };
        Error ->
            Error
    end.

%% Get_Configure_Node
-spec(get_configure_node/5 ::
(
  Host         :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host  :: exmpp_pubsub:host(),
  Entity       :: xmpp_jid:usr_entity(),
  Parameters   :: {NodeId :: undefined | exmpp_pubsub:nodeId()},
  Capabilities :: #capabilities{})
    -> {result, [Xmlel_Pubsub::xmlel(),...]}
    %%%
     | {error, 'feature-not-implemented', 'config-node'}
     %
     | {error, 'bad-request', 'nodeid-required'}
     | {error, 'item-not-found'}
     | {error, 'forbidden'}

).

get_configure_node(Host, Pubsub_Host, {U,S,_R} = _Entity,
  {NodeId} = _Parameters,
   #capabilities{plugin = Plugin, api = #api{options = Options_Module,
   db = API_DB}} = _Capabilities) ->
    Pubsub_Features = Plugin:pubsub_features(),
    case lists:member(<<"config-node">>, Pubsub_Features) of
        true ->
            case
                pubsub_db:transaction('mnesia',
                    API_DB#api_db.get_configure_node,
                    'get_configure_node',
                    [Host, Pubsub_Host, {U,S,undefined}, {NodeId}])
            of
                {result, Node_Options} ->
                    {result,
                     [exmpp_pubsub:xmlel_pubsub('pubsub#owner',
                        [exmpp_pubsub:xmlel_configure('pubsub#owner', NodeId,
                             [Options_Module:xdata_x('node_config', 'form',
                              Pubsub_Features, Host, {U,S,undefined}, Node_Options
                              )]
                         )]
                      )]
                    };
                Error ->
                    Error
            end;
        false ->
            {error, 'feature-not-implemented', 'config-node'}
    end.

