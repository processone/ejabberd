%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http:%%www.erlang.org/.
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
%%%   [http:%%www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @headerfile "pubsub_dev.hrl"

-module(pubsub_db).
-author('karim.gemayel@process-one.net').

-compile(export_all).

-include("pubsub_dev.hrl").
-include("pubsub_api.hrl").


-import(pubsub_tools,
[
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



%% @doc Intialize mnesia

-spec(init/2 ::
(
  Backend :: 'mnesia' | 'odbc',
  Suffix  :: atom())
    -> 'ok'
).

init('mnesia', Suffix) ->
    pubsub_db_mnesia:init(Suffix);
init('odbc', _Suffix) ->
    ok.


transaction('mnesia', Module, Function, Arguments) ->
    pubsub_db_mnesia:transaction(Module, Function, Arguments).


%%
db_transaction(Module, Function, Arguments) ->
    case mnesia:transaction(
        fun() ->
            apply(Module, Function, Arguments)
        end)
    of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            ?INFO_MSG("DB TRANSACTION ERROR ~p", [Reason]),
            {error, 'internal-server-error'}
    end.

%-- Create_Node --%
-spec(create_node/5 ::
(
  Host            :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host     :: exmpp_pubsub:host_pubsub(),
  Entity          :: xmpp_jid:usr_bare(),
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Parameters      :: {NodeId       :: undefined | exmpp_pubsub:nodeId(),
                      Node_Options :: pubsub_options:options_node()})
    -> %%
       {result,
        NodeId :: exmpp_pubsub:nodeId(),
        Notify :: []
                | [Notify_Subscription :: {
                       Notification_Type :: pubsub_options:notification_type(),
                       Subscription      :: exmpp_pubsub:subscription_subscribed()
                   },...]
       }
     %
    %%%
     | {error, 'conflict'}
).

create_node(Host, Pubsub_Host, {_U,S,_R} = Entity, Pubsub_Features,
  _Parameters = {undefined = _NodeId, Node_Options}) ->
    case
        pubsub_db_mnesia:read_node('dev',
            Pubsub_Host, NodeId = exmpp_pubsub:nodeId())
    of
        undefined ->
            {result,
             NodeId,
             _Notify = case
                 pubsub_db_mnesia:create_node('dev', Pubsub_Host, Entity, NodeId,
                     _Level = 1, Node_Options, Pubsub_Features)
             of
                 undefined ->
                     [];
                 Subscription ->
                     [_Notify_Subscription = {
                          _Notification_Type = get_value(Node_Options,
                              'notification_type', 'headline'),
                          Subscription
                     }]
             end};
        _Pubsub_Node ->
            create_node(Host, Pubsub_Host, Entity, Pubsub_Features,
                {_NodeId = undefined, Node_Options})
    end;
%%
create_node(Host, Pubsub_Host, {_U,S,_R} = Entity, Pubsub_Features,
  _Parameters = {NodeId, Node_Options}) ->
    case pubsub_db_mnesia:read_node('dev', Pubsub_Host, NodeId) of
        undefined ->
            {result,
             NodeId,
             _Notify = case
                 pubsub_db_mnesia:create_node('dev', Pubsub_Host, Entity, NodeId,
                     _Level = 1, Node_Options, Pubsub_Features)
             of
                 undefined ->
                     [];
                 Subscription ->
                     [_Notify_Subscription = {
                          _Notification_Type = get_value(Node_Options,
                              'notification_type', 'headline'),
                          Subscription
                     }]
             end};
        _Pubsub_Node ->
            {error, 'conflict'}
    end.

%-- Delete_Node --%
-spec(delete_node/3 ::
(
  Pubsub_Host :: exmpp_pubsub:host(),
  Entity      :: xmpp_jid:usr_bare(),
  Parameters  :: {NodeId :: undefined | exmpp_pubsub:nodeId()})
    -> %%
       {result,
        Notify :: []
                | [Notify_Delete :: {
                       Notification_Type::pubsub_options:notification_type(),
                       Recipients::[Entity::xmpp_jid:usr_bare()]
                   }]
       }
    %%%
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
).

delete_node(_Pubsub_Host, _Entity, {undefined = _NodeId}) ->
    {error, 'item-not-found'};
%%
delete_node(Pubsub_Host, Entity, {NodeId}) ->
    case pubsub_db_mnesia:read_node('dev', Pubsub_Host, NodeId) of
        undefined ->
            {error, 'item-not-found'};
        Pubsub_Node ->
            case
                lists:member(Entity, Owners = Pubsub_Node#pubsub_node_dev.owners)
            of
                true ->
                    {result,
                     _Notify = case
                         pubsub_db_mnesia:delete_node('dev', Pubsub_Node)
                     of
                         undefined ->
                             [];
                         Subscribers ->
                             [_Notify_Delete = {
                                  _Notification_Type = get_value(
                                      Pubsub_Node#pubsub_node_dev.options,
                                          'notification_type', 'headline'),
                                  _Recipients = lists:append(Owners, Subscribers)
                              }]
                     end};
                false ->
                    {error, 'forbidden'}
            end
    end.

%-- Purge_Node --%
-spec(purge_node/3 ::
(
  Pubsub_Host :: exmpp_pubsub:host(),
  Entity      :: xmpp_jid:usr_bare(),
  Parameters  :: {NodeId :: undefined | exmpp_pubsub:nodeId()})
    -> %%
       {result,
        Notify :: []
                | [Notify_Purge :: {
                       Notification_Type::pubsub_options:notification_type(),
                       Recipients::[Entity::xmpp_jid:usr_bare()]
                   }]
       }
    %%%
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
     %
     | {error, 'feature-not-implemented', 'persistent-items'}
).

purge_node(_Pubsub_Host, _Entity, {undefined = _NodeId}) ->
    {error, 'item-not-found'};
%%
purge_node(Pubsub_Host, Entity, {NodeId}) ->
    case pubsub_db_mnesia:read_node('dev', Pubsub_Host, NodeId) of
        undefined ->
            {error, 'item-not-found'};
        Pubsub_Node ->
            case lists:member(Entity, Pubsub_Node#pubsub_node_dev.owners) of
                true ->
                    case
                        _Persist_Items = get_value(
                            Node_Options = Pubsub_Node#pubsub_node_dev.options,
                            'persist_items', true)
                    of
                        true ->
                            {result,
                             _Notify = case
                                 pubsub_db_mnesia:purge_node('dev', Pubsub_Node)
                             of
                                 undefined ->
                                     [];
                                 Subscribers ->
                                     [_Notify_Purge = {
                                          _Notification_Type = get_value(
                                              Node_Options,
                                              'notification_type', 'headline'),
                                          _Recipients = Subscribers
                                     }]
                             end};
                        false ->
                            {error, 'feature-not-implemented', 'persistent-items'}
                    end;
                false ->
                    {error, 'forbidden'}
            end
    end.

%-- Publish_Item --%
-spec(publish_item/6 ::
(
  Host            :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host     :: exmpp_pubsub:host_pubsub(),
  Entity          :: xmpp_jid:usr_entity(),
  Plugin          :: exmpp_pubsub:plugin(),
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Parameters      :: {
    NodeId  :: undefined | exmpp_pubsub:nodeId(),
    Item    :: 0 | 1,
    ItemId  :: undefined | exmpp_pubsub:itemId(),
    Payload :: exmpp_pubsub:payload() | [Xmlel::xmlel(),...],
    Options :: {Node_Options :: [] | pubsub_options:options_node(),
                Item_Options :: [] | pubsub_options:options_item()}
  })
    -> {result,
        NodeId            :: exmpp_pubsub:nodeId(),
        ItemId            :: exmpp_pubsub:itemId(),
        Node_Options      :: pubsub_options:options_node(),
        Node_Owners       :: [Node_Owner::xmpp_jid:usr_bare(),...],
        SubId             :: undefined | exmpp_pubsub:subId(),
        Subscription      :: undefined | exmpp_pubsub:subscription_subscribed(),
        Broadcasted_Items :: {
          Published_Items :: [Published_Item::{
            ItemId       :: exmpp_pubsub:itemId(),
            Item_Options :: [] | pubsub_options:options_item(),
            Payload      :: exmpp_pubsub:payload(),
            Publisher    :: xmpp_jid:usr_entity()
          }],
          %
          Retracted_Items :: [Retracted_Item::{
            ItemId       :: exmpp_pubsub:itemId(),
            Item_Options :: [] | pubsub_options:options_item()
          }]
        },
        Pubsub_States::[Pubsub_State::mod_pubsub_dev:pubsub_state(),...]}
    %%%
     | {error, 'forbidden'}
     | {error, 'item-not-found'}
     %
     | {error, 'bad-request',    'invalid-payload'}
     | {error, 'bad-request',    'item-required'}
     | {error, 'bad-request',    'item-forbidden'}
     | {error, 'not-acceptable', 'payload-too-big'}
).

%%
publish_item(Host, Pubsub_Host, {U,S,R} = Entity, Plugin, Pubsub_Features,
  {undefined = _NodeId, Item, _ItemId, Payload,
   {[] = _Node_Options, Item_Options}}) ->
    case
        pubsub_db_mnesia:read_node('dev',
            Pubsub_Host, NodeId = exmpp_pubsub:nodeId())
    of
        undefined ->
            case
                options_on_publish_item(none, {Item, Payload,
                    Options = {
                        Node_Options = Plugin:node_options('leaf'),
                        Item_Options
                    }})
            of
                ok ->
                    {ItemId, Pubsub_State} = pubsub_db_mnesia:publish_item1('dev',
                        Pubsub_Host, Entity, NodeId, _Level = 1, _ItemId, Payload,
                        Options, Pubsub_Features),
                    Subscription = case
                        Pubsub_State#pubsub_state_dev.subscriptions
                    of
                        []              -> undefined;
                        [_Subscription] -> _Subscription
                    end,
                    {result, NodeId, ItemId, Node_Options,
                     _Node_Owners = [{U,S,undefined}],
                     _SubId = case Subscription of
                         undefined ->
                             undefined;
                         {_Subscription_State, SubId, _Resource,
                          _Subscription_Options} ->
                             SubId
                     end,
                     Subscription,
                     {_Published_Items = case
                          get_value({Node_Options, Item_Options},
                              'deliver_notifications', false)
                      of
                          true ->
                              [{ItemId,
                                Item_Options,
                                _Payload = case
                                    get_value({Node_Options, Item_Options},
                                        'deliver_payloads', false)
                                of
                                    true  ->
                                        case Payload of
                                            [Xmlel] -> Xmlel;
                                            []      -> []
                                        end;
                                    false -> []
                                end,
                                _Publisher = {U,S,R}
                               }];
                          false ->
                              []
                      end,
                      _Retracted_Items = []},
                     _Pubsub_States = [Pubsub_State]};
                Error ->
                    Error
            end;
        _Pusbub_Node ->
            publish_item(Host, Pubsub_Host, {U,S,R}, Plugin, Pubsub_Features,
                {undefined, Item, _ItemId, Payload, {[], Item_Options}})
    end;

%%
publish_item(Host, Pubsub_Host, {U,S,R} = Entity, Plugin, Pubsub_Features,
  {NodeId, Item, _ItemId, Payload, {[] = _Node_Options, Item_Options}}) ->
    case pubsub_db_mnesia:read_node('dev', Pubsub_Host, NodeId) of
        undefined ->
            case
                checks_on_publish_item(Host, {U,S,undefined}, Plugin,
                    {Item, Payload, _Node_Options = [], Item_Options})
            of
                {ok, Pubsub_Features, Node_Options} ->
                    Options = {Node_Options, Item_Options},
                    {ItemId, Pubsub_State} = pubsub_db_mnesia:publish_item1('dev',
                        Pubsub_Host, Entity, NodeId, _Level = 1, _ItemId, Payload,
                        Options, Pubsub_Features),
                    Subscription = case
                        Pubsub_State#pubsub_state_dev.subscriptions
                    of
                        []              -> undefined;
                        [_Subscription] -> _Subscription
                    end,
                    {result, NodeId, ItemId, Node_Options,
                     _Node_Owners = [{U,S,undefined}],
                     _SubId = case Subscription of
                         undefined ->
                             undefined;
                         {_Subscription_State, SubId, _Resource,
                          _Subscription_Options} ->
                             SubId
                     end,
                     Subscription,
                     {_Published_Items = case
                          get_value({Node_Options, Item_Options},
                              'deliver_notifications', false)
                      of
                          true ->
                              [{ItemId,
                                Item_Options,
                                _Payload = case
                                    get_value({Node_Options, Item_Options},
                                        'deliver_payloads', false)
                                of
                                    true  ->
                                        case Payload of
                                            [Xmlel] -> Xmlel;
                                            []      -> []
                                        end;
                                    false -> []
                                end,
                                _Publisher = {U,S,R}
                               }];
                          false ->
                              []
                      end,
                      _Retracted_Items = []},
                     _Pubsub_States = [Pubsub_State]};
                Error ->
                    Error
            end;
        Pubsub_Node ->
            NodeIdx = Pubsub_Node#pubsub_node_dev.idx,
            Pubsub_State = pubsub_db_mnesia:read_state('dev',
                {U,S,undefined}, NodeIdx),
            case
                options_on_publish_item(Host, Entity,
                    _Affiliation = case Pubsub_State of
                        undefined -> 'none';
                        _         -> Pubsub_State#pubsub_state_dev.affiliation
                    end,
                    _Subscriptions = case Pubsub_State of
                        undefined -> [];
                        _         -> Pubsub_State#pubsub_state_dev.subscriptions
                    end,
                    _Options = {
                        Node_Options = Pubsub_Node#pubsub_node_dev.options,
                        Item_Options
                    },
                    Node_Owners = Pubsub_Node#pubsub_node_dev.owners,
                    Item,
                    Payload) 
            of
                ok ->
                    {ItemId, Retracted_Items, Pubsub_States} =
                        pubsub_db_mnesia:publish_item2('dev', Pubsub_Host,
                            {U,S,R}, Pubsub_State, Pubsub_Node,
                            case _ItemId of
                                undefined ->
                                    undefined;
                                _ ->
                                    case
                                        pubsub_db_mnesia:read_item('dev',
                                            NodeIdx, _ItemId)
                                    of
                                        undefined   -> _ItemId;
                                        Pubsub_Item -> Pubsub_Item
                                    end
                            end,
                            Payload, Item_Options, Pubsub_Features),
                    {result, NodeId, ItemId, Node_Options, Node_Owners,
                     _SubId = undefined, _Subscription = undefined,
                     {_Published_Items = case
                          get_value({Node_Options, Item_Options},
                              'deliver_notifications', false)
                      of
                          true ->
                              [{ItemId,
                                Item_Options,
                                _Payload = case
                                    get_value({Node_Options, Item_Options},
                                        'deliver_payloads', false)
                                of
                                    true  ->
                                        case Payload of
                                            [Xmlel] -> Xmlel;
                                            []      -> []
                                        end;
                                    false -> []
                                end,
                                _Publisher = {U,S,R}
                               }];
                          false ->
                              []
                      end,
                      Retracted_Items},
                     Pubsub_States};
                Error ->
                    Error
            end
    end.


%%
-spec(options_on_publish_item/8 ::
(
  Host          :: xmpp_jid:raw_jid_component_bare(),
  Entity        :: xmpp_jid:usr_bare(),
  Affiliation   :: exmpp_pubsub:affiliation(),
  Subscriptions :: exmpp_pubsub:subscriptions(),
  Options       :: {Node_Options :: pubsub_options:options_node(),
                    Item_Options :: [] | pubsub_options:options_item()},
  Node_Owners   :: [Node_Owner::xmpp_jid:usr_bare(),...],
  Item          :: 0 | 1,
  Payload       :: exmpp_pubsub:payload() | [Xmlel::xmlel(),...])
    -> ok
    %%%
     | {error, 'forbidden'}
     | {error, 'item-not-found'}
     %
     | {error, 'bad-request',    'invalid-payload'}
     | {error, 'bad-request',    'item-required'}
     | {error, 'bad-request',    'item-forbidden'}
     | {error, 'not-acceptable', 'payload-too-big'}
).

options_on_publish_item(_Host, _Entity, 'owner' = _Affiliation, _Subscriptions,
  Options, _Node_Owners, Item, Payload) ->
    options_on_publish_item(none, {Item, Payload, Options});
%%
options_on_publish_item(Host, Entity, 'outcast' = _Affiliation, _Subscriptions,
  {Node_Options, _Item_Options}, Node_Owners, _Item, _Payload) ->
    case get_value(Node_Options, 'access_model') of
        %%
        'open'->
              {error, 'forbidden'};
        %%
        'presence' ->
            case
                is_contact_subscribed_to_node_owners(Host, Entity, Node_Owners)
            of
                false ->
                    {error, 'item-not-found'};
                _Node_Owner ->
                    {error, 'forbidden'}
            end;
        %%
        'roster' ->
            case
                is_contact_in_allowed_roster_groups(Entity,
                    _Rosters_Groups_Allowed = get_value(Node_Options,
                        'roster_groups_allowed'))
            of
                false ->
                    {error, 'item-not-found'};
                {_Node_Owner, _Roster_Group_Allowed} ->
                    {error, 'forbidden'}
            end;
        %%
        'authorize' ->
            {error, 'forbidden'};
        %%
        'whitelist' ->
            {error, 'item-not-found'}
    end;
%%
options_on_publish_item(Host, Entity, Affiliation, Subscriptions,
  {Node_Options, Item_Options} = _Options, Node_Owners, Item, Payload) ->
    %% 'pubsub#access_model'
    case
        check_access_model(Host, Entity,
            _Access_Model = get_value(Node_Options, 'access_model'),
            Affiliation, Subscriptions, Node_Owners,
            _Rosters_Groups_Allowed = get_value(Node_Options,
                'roster_groups_allowed'))
    of
        ok ->
            %% 'pubsub#publish_model'
            case
                check_publish_model(
                    _Publish_Model = get_value(Node_Options, 'publish_model'),
                    Affiliation, Subscriptions)
            of
                ok ->
                    options_on_publish_item(none,
                        {Item, Payload, {Node_Options, Item_Options}});
                Error ->
                    Error
            end;
        Error ->
            Error
    
    end.

%%
-spec(options_on_publish_item/2 ::
(

  Option   :: none
            | {pubsub_options:option_node_deliver_payloads(),
               pubsub_options:option_node_persist_items()}
            | pubsub_options:option_node_max_payload_size()
            | pubsub_options:option_node_type(),
           %%
  Criteria :: {Item    :: 0 | 1, 
               Payload :: exmpp_pubsub:payload() | [Xmlel::xmlel(),...],
               Options :: {Node_Options :: pubsub_options:options_node(),
                           Item_Options :: [] | pubsub_options:options_item()}}
            | {Item    :: 0 | 1, 
               Payload :: exmpp_pubsub:payload(),
               Options :: {Node_Options :: pubsub_options:options_node(),
                           Item_Options :: [] | pubsub_options:options_item()}}
            %
            | {Payload :: exmpp_pubsub:payload(),
               Options :: {Node_Options :: pubsub_options:options_node(),
                           Item_Options :: [] | pubsub_options:options_item()}}
            %
            | {Payload :: exmpp_pubsub:payload()})
    -> %%
       ok
    %%%
     | {error, 'bad-request',    'invalid-payload'}
     | {error, 'bad-request',    'item-required'}
     | {error, 'bad-request',    'payload-required'}
     | {error, 'bad-request',    'item-forbidden'}
     | {error, 'not-acceptable', 'payload-too-big'}
).

%% Payloads count
options_on_publish_item(none, {Item, [] = Payload, Options}) ->
    options_on_publish_item(
        {get_option(Options, 'deliver_payloads', false),
         get_option(Options, 'persist_items', false)},
        {Item, Payload, Options});
%%
options_on_publish_item(none, {Item, [Payload], Options}) ->
    options_on_publish_item(
        {get_option(Options, 'deliver_payloads', false),
         get_option(Options, 'persist_items', false)},
        {Item, Payload, Options});
%%
options_on_publish_item(none, _Parameters) ->
    {error, 'bad-request', 'invalid-payload'};
%%
%% 'pubsub#deliver_payloads'
%% 'pubsub#persist_items'
options_on_publish_item(
  {{'deliver_payloads', _Deliver_Payloads}, {'persist_items', true}},
  {0 = _Item, _Payload, _Options}) ->
    {error, 'bad-request', 'item-required'};
%%
options_on_publish_item(
  {{'deliver_payloads', true}, {'persist_items', _Persist_Items}},
  {_Item, [] = _Payload, _Options}) ->
    {error, 'bad-request', 'payload-required'};
%%
options_on_publish_item(
  {{'deliver_payloads', false}, {'persist_items', false}},
  {1 = _Item, _Payload, _Options}) ->
    {error, 'bad-request', 'item-forbidden'};
%%
options_on_publish_item(
  {{'deliver_payloads', Deliver_Payloads}, {'persist_items', Persist_Items}},
  {_Item, [] = _Payload, Options}) ->
    ok;
%%
options_on_publish_item(
  {{'deliver_payloads', Deliver_Payloads}, {'persist_items', Persist_Items}},
  {_Item, Payload, {Node_Options, Item_Options} = _Options}) ->
    options_on_publish_item(
        _Max_Payload_Size = get_option(Node_Options, 'max_payload_size', 0),
        {Payload, {Node_Options, Item_Options}});
%% 'pubsub#max_payload_size'
options_on_publish_item({'max_payload_size', Max_Payload_Size},
  {Payload, Options}) ->
    case byte_size(term_to_binary(Payload)) =< Max_Payload_Size of
        true ->
            options_on_publish_item(get_option(Options, 'type', undefined),
                {Payload});
        false ->
            {error, 'not-acceptable', 'payload-too-big'}
    end;
%% 'pubsub#type'
options_on_publish_item({'type', Type}, {Payload})
  when   Type == undefined ->
  %%orelse Type == Payload#xmlel.ns ->
      ok;
%%
options_on_publish_item(_, _) ->
    {error, 'bad-request', 'invalid-payload'}.



%%
-spec(checks_on_publish_item/4 ::
(
  Host       :: xmpp_jid:raw_jid_component_bare(),
  Entity     :: xmpp_jid:usr_entity(),
  Plugin     :: exmpp_pubsub:plugin(),
  Parameters :: {Item         :: 0 | 1,
                 Payload      :: [Xmlel::xmlel()],
                 Node_Options :: [],
                 Item_Options :: [] | pubsub_options:options_item()})
    -> {ok,
        Pubsub_Features :: exmpp_pubsub:pubsub_features(),
        Node_Options    :: pubsub_options:options_node()}
    %%%
     | {error, 'forbidden'}
     | {error, 'item-not-found'}
     %
     | {error, 'bad-request',    'invalid-payload'}
     | {error, 'bad-request',    'item-required'}
     | {error, 'bad-request',    'item-forbidden'}
     | {error, 'not-acceptable', 'payload-too-big'}
).


checks_on_publish_item({U,S,_} = _Host, {U,S,_} = _Entity, Plugin,
  {Item, Payload, [] = _Node_Options, Item_Options}) ->
    case
        lists:member(<<"auto-create">>, Pubsub_Features = Plugin:pubsub_features())
    of
        true ->
            case
                options_on_publish_item(none,
                    {Item,
                     Payload,
                     _Options = {
                         Node_Options = Plugin:node_options('leaf'),
                         Item_Options
                     }})
            of
                ok    -> {ok, Pubsub_Features, Node_Options};
                Error -> Error
            end;
        false ->
            {error, 'item-not-found'}
    end;
%%
checks_on_publish_item(_Host, _Entity, _Plugin, _Parameters)
  when is_tuple(_Host) ->
    {error, 'foribdden'};
%%
checks_on_publish_item(Host, {U,S,_R} = _Entity, Plugin,
  {Item, Payload, [] = _Node_Options, Item_Options}) ->
    case acl:match_rule(Host, pubsub_createnode, jlib:jid_to_string({U,S, <<>>})) of
        allow ->
            case
                lists:member(<<"auto-create">>,
                    Pubsub_Features = Plugin:pubsub_features())
            of
                true ->
                    case
                        options_on_publish_item(none,
                            {Item, Payload,
                             _Options = {
                                 Node_Options = Plugin:node_options('leaf'),
                                 Item_Options
                             }})
                    of
                        ok    -> {ok, Pubsub_Features, Node_Options};
                        Error -> Error
                    end;
                false ->
                    {error, 'item-not-found'}
            end;
        _Deny ->
            {error, 'forbidden'}
    end.

%-- Retract_Item --%
-spec(retract_item/3 ::
(
  Pubsub_Host :: exmpp_pubsub:host(),
  Entity      :: xmpp_jid:usr_bare(),
  Parameters  :: {NodeId               :: undefined | exmpp_pubsub:nodeId(),
                  ItemId               :: undefined | exmpp_pubsub:itemId(),
                  Adhoc_Notify_Retract :: undefined | boolean()})
    -> {result,
        Notify :: []
                | [Notify_Retract :: {
                       Node_Owners   :: [Entity::xmpp_jid:usr_bare(),...],
                       Node_Options  :: pubsub_options:options_node(),
                       Item_Options  :: [] | pubsub_options:options_item(),
                       Pubsub_States :: mod_pubsub_dev:pubsub_states()
                   }]}
    %%%
     | {error, 'bad-request', 'nodeid-required'}
     | {error, 'bad-request', 'item-required'}
     %
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
).

retract_item(_Pubsub_Host, _Entity,
  {undefined = _NodeId, _ItemId, _Adhoc_Notify_Retract}) ->
    {error, 'bad-request', 'nodeid-required'};
%%
retract_item(_Pubsub_Host, _Entity,
  {_NodeId, undefined = _ItemId, _Adhoc_Notify_Retract}) ->
    {error, 'bad-request', 'item-required'};
%%
retract_item(Pubsub_Host, Entity, {NodeId, ItemId, Adhoc_Notify_Retract}) ->
    case pubsub_db_mnesia:read_node('dev', Pubsub_Host, NodeId) of
        undefined ->
            {error, 'item-not-found'};
        Pubsub_Node ->
            case
                pubsub_db_mnesia:read_state('dev', Entity,
                    Pubsub_Node#pubsub_node_dev.idx)
            of
                Pubsub_State
                  when   Pubsub_State == undefined
                  orelse Pubsub_State#pubsub_state_dev.affiliation == 'outcast' ->
                    {error, 'forbidden'};
                Pubsub_State ->
                    case
                        pubsub_db_mnesia:retract_item('dev', Pubsub_Node,
                            Pubsub_State, ItemId, Adhoc_Notify_Retract)
                    of
                        {ok, undefined} ->
                            {result, _Notify = []};
                        {ok, Item_Options, Pubsub_States} ->
                            {result,
                             _Notify = [_Notify_Retract = {
                                 _Node_Owners = Pubsub_Node#pubsub_node_dev.owners,
                                 _Node_Options = Pubsub_Node#pubsub_node_dev.options,
                                 Item_Options,
                                 Pubsub_States
                             }]
                            };
                        Error ->
                            Error
                    end
            end
    end.


%-- Subscribe_Node --%
-spec(subscribe_node/7 ::
(
  Host            :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host     :: exmpp_pubsub:host(),
  Entity          :: xmpp_jid:usr_bare(),
  Plugin          :: exmpp_pubsub:plugin(),
  Options_Module  :: module(),
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Parameters      :: {NodeId            :: exmpp_pubsub:nodeId(),
                      Resource          :: undefined | xmpp_jid:resource_jid(),
                      Subscribe_Options :: [Xmlel::xmlel(),...]
                                         | [Xmlel::xmlel()]})
    -> {result,
        Subscription_Subscribed :: exmpp_pubsub:subscription_subscribed(),
        Affiliation             :: 'member' | 'owner' | 'publisher',
        Node_Owners             :: [Entity::xmpp_jid:usr_bare(),...],
        Node_Options            :: pubsub_options:options_node(),
        Pubsub_Last_Item        :: undefined | mod_pubsub_dev:pubsub_last_item()}
     %
     | {result,
        Subscription_Pending :: exmpp_pubsub:subscription_pending(),
        Affiliation          :: 'member',
        Node_Owners          :: [Entity::xmpp_jid:usr_bare(),...],
        Node_Options         :: pubsub_options:options_node(),
        Pubsub_Last_Item     :: undefined}
    %%%
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

subscribe_node(_Host, _Pubsub_Host, _Entity, _Plugin, _Options_Module,
  _Pubsub_Features, {undefined = _NodeId, _Resource, _Subscribe_Options}) ->
    {error, 'item-not-found'};
%%
subscribe_node(Host, Pubsub_Host, {_, S,_} = Entity, Plugin, Options_Module,
  Pubsub_Features, {NodeId, Resource, Subscribe_Options}) ->
    case pubsub_db_mnesia:read_node('dev', Pubsub_Host, NodeId) of
        undefined ->
            {error, 'item-not-found'};
        #pubsub_node_dev{idx = NodeIdx, owners = Node_Owners, options = Node_Options} ->
            case
                Options_Module:parse_xmlel_x(Plugin, Pubsub_Features, Entity,
                    _Entity_Type = case S == Host of
                        true  -> 'local';
                        false -> 'remote'
                    end,
                    'subscribe_options',
                    _Node_Type = get_value(Node_Options, 'node_type', 'leaf'),
                    Subscribe_Options)
            of
                {ok, Subscription_Options} ->
                    Pubsub_State = case
                        pubsub_db_mnesia:read_state('dev', Entity, NodeIdx)
                    of
                        undefined ->
                            #pubsub_state_dev{
                                id      = {Entity, NodeIdx},
                                nodeidx = NodeIdx
                            };
                        _Pubsub_State ->
                            _Pubsub_State
                    end,
                    case
                        pubsub_db_mnesia:subscribe_node('dev', Host, Node_Options,
                            Node_Owners, Pubsub_State, Resource,
                            Subscription_Options, Pubsub_Features)
                    of
                        {result, Subscription, Pubsub_Last_Item} ->
                            {result,
                             Subscription,
                             case Pubsub_State#pubsub_state_dev.affiliation of
                                'none'      -> 'member';
                                Affiliation -> Affiliation
                             end,
                             Node_Owners, Node_Options, Pubsub_Last_Item};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end
    end.

%-- Unsubscribe_Node --%
-spec(unsubscribe_node/4 ::
(
  Host            :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host     :: exmpp_pubsub:host(),
  Entity          :: xmpp_jid:usr_bare(),
  Parameters      :: {NodeId    :: exmpp_pubsub:nodeId(),
                      _Resource :: undefined
                                 | xmpp_jid:resource_jid()
                                 | {'caps', xmpp_jid:resource_jid()},
                      _SubId    :: exmpp_pubsub:subId()})
    -> {result,
        SubId :: exmpp_pubsub:subId()}
     |
       {result,
        SubId             :: exmpp_pubsub:subId(),
        Node_Owners       :: [Entity::xmpp_jid:usr_bare(),...],
        Notification_Type :: pubsub_options:notification_type()}
    %%%
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
     | {error, 'unexpected',     'not-subscribed'}
     %
     | {error, 'unexpected',     'not-subscribed'}
     | {error, 'not-acceptable', 'invalid-subid'}
     | {error, 'bad-request',    'subid-required'}
     | {error, 'not-acceptable', 'invalid-subid'}
).

unsubscribe_node(_Host, _Pubsub_Host, _Entity,
  {undefined = _NodeId, _Resource, _SubId}) ->
    {error, 'item-not-found'};
%%
unsubscribe_node(Host, Pubsub_Host, Entity, {NodeId, _Resource, _SubId}) ->
    case pubsub_db_mnesia:read_node('dev', Pubsub_Host, NodeId) of
        undefined ->
            {error, 'item-not-found'};
        #pubsub_node_dev{idx = NodeIdx, owners = Node_Owners, options = Node_Options} ->
            case pubsub_db_mnesia:read_state('dev', Entity, NodeIdx) of
                Pubsub_State
                  when   Pubsub_State == undefined
                  orelse Pubsub_State#pubsub_state_dev.affiliation   == 'none'
                  orelse Pubsub_State#pubsub_state_dev.subscriptions == [] ->
                    {error, 'unexpected', 'not-subscribed'};
                Pubsub_State
                  when Pubsub_State#pubsub_state_dev.affiliation == 'publish-only'->
                    {error, 'forbidden'};
                Pubsub_State ->
                    case
                        pubsub_db_mnesia:unsubscribe_node('dev', Host,
                            _Node_Access_Model = get_value(Node_Options, 'access_model'),
                            Pubsub_State, _Resource, _SubId)
                    of
                        {ok, Resource, SubId} ->
                            case get_value(Node_Options, 'notify_sub', false) of
                                true ->
                                    {result, SubId, Node_Owners,
                                     get_value(Node_Options, 'notification_type',
                                         'headline')};
                                false ->
                                    {result, SubId}
                            end;
                        Error ->
                            Error
                    end
            end
    end.

%-- Set_Configure_Subscription --%
-spec(set_configure_subscription/7 ::
(
  Host            :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host     :: exmpp_pubsub:host(),
  Entity          :: xmpp_jid:usr_bare(),
  Plugin          :: exmpp_pubsub:plugin(),
  Options_Module  :: module(),
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Parameters      :: {NodeId            :: undefined | exmpp_pubsub:nodeId(),
                      SubId             :: undefined | exmpp_pubsub:subId(),
                      Resource          :: undefined
                                         | xmpp_jid:resource_jid()
                                         | {'caps', xmpp_jid:resource_jid()},
                      Subscribe_Options :: [Xmlel::xmlel(),...]
                                         | [Xmlel::xmlel()]})
    -> {result, []}
    %%%
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

set_configure_subscription(_Host, _Pubsub_Host, _Entity, _Plugin, _Options_Module,
  _Pubsub_Features, {undefined = _NodeId, _SubId, _Resource, _Subscribe_Options}) ->
    {error, 'item-not-found'};
%%
set_configure_subscription(Host, Pubsub_Host, {_,S,_} = Entity, Plugin, Options_Module,
  Pubsub_Features, {NodeId, SubId, Resource, Subscribe_Options}) ->
    case pubsub_db_mnesia:read_node('dev', Pubsub_Host, NodeId) of
        undefined ->
            {error, 'item-not-found'};
        #pubsub_node_dev{idx = NodeIdx, owners = Node_Owners, options = Node_Options} ->
            case pubsub_db_mnesia:read_state('dev', Entity, NodeIdx) of
                undefined ->
                    {error, 'unexpected-request', 'not-subscribed'};
                #pubsub_state_dev{affiliation = Affiliation}
                  when   Affiliation == 'publish-only'
                  orelse Affiliation == 'outcast' ->
                    {error, 'forbidden'};
                #pubsub_state_dev{subscriptions = []} ->
                    {error, 'unexpected-request', 'not-subscribed'};
                Pubsub_State ->
                    case
                        Options_Module:parse_xmlel_x(Plugin, Pubsub_Features, Entity,
                            _Entity_Type = case S == Host of
                                true  -> 'local';
                                false -> 'remote'
                            end,
                            'subscribe_options',
                            _Node_Type = get_value(Node_Options, 'node_type', 'leaf'),
                            Subscribe_Options)
                    of
                        {ok, Subscription_Options} ->
                            case
                                pubsub_db_mnesia:set_configure_subscription('dev',
                                    Pubsub_State, SubId, Resource,
                                    Subscription_Options) 
                            of
                                ok ->
                                    {result, []};
                                Error ->
                                    Error
                            end;
                        _Error ->
                            {error, 'bad-request', 'invalid-options'}
                    end
            end
    end.

%-- Get_Items --%
-spec(get_items/4 ::
(
  Host        :: xmpp_jid:raw_jid_component_bare(),
  Pusbub_Host :: exmpp_pubsub:host(),
  Entity      :: xmpp_jid:usr_bare(),
  Parameters  :: {NodeId    :: undefined | exmpp_pubsub:nodeId(),
                  _SubId    :: undefined | exmpp_pubsub:subId(),
                  Max_Items :: non_neg_integer(),
                  ItemIds   :: undefined}
               | {NodeId    :: undefined | exmpp_pubsub:nodeId(),
                  _SubId    :: undefined | exmpp_pubsub:subId(),
                  Max_Items :: undefined,
                  ItemIds   :: [] | exmpp_pubsub:itemIds()})
    -> {result,
        Pubsub_Items      :: [] | mod_pubsub_dev:pubsub_items(),
        Cache             :: {Presence_Cache :: undefined,
                              Groups_Cache   :: undefined}
                           | {Presence_Cache :: true,
                              Groups_Cache   :: undefined}
                           | {Presence_Cache :: undefined,
                              Groups_Cache   :: [{Node_Owner :: xmpp_jid:usr_bare(),
                                                  Groups     :: [Group::binary()]}]},
        Node_Owners       :: mod_pubsub_dev:node_owners(),
        Node_Access_Model :: pubsub_options:access_model(),
        Node_Groups       :: pubsub_options:rosters_groups_allowed(),
        Affiliation       :: exmpp_pubsub:affiliation(),
        Access            :: undefined | 'presence' | 'roster' | 'pending' | 'authorize'}
    %%%
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
     | {error, 'not-authorized', 'presence-subscription-required'}
     | {error, 'not-authorized', 'not-in-roster-group'}
     | {error, 'not-allowed',    'closed-node'}
).

get_items(_Host, _Pubsub_Host, _Entity,
  {undefined = _NodeId, _SubId, _Max_Items, _ItemIds}) ->
    {error, 'item-not-found'};
%%
get_items(Host, Pubsub_Host, Entity, {NodeId, _SubId, Max_Items, ItemIds}) ->
    case pubsub_db_mnesia:read_node('dev', Pubsub_Host, NodeId) of
        undefined ->
            {error, 'item-not-found'};
        #pubsub_node_dev{idx = NodeIdx, owners = Node_Owners,
         options = Node_Options, itemids = Node_ItemIds} ->
            {Affiliation, Access} = case lists:member(Entity, Node_Owners) of
                true ->
                    {'owner', undefined};
                false ->
                    case pubsub_db_mnesia:read_state('dev', Entity, NodeIdx) of
                        undefined ->
                            {'none', undefined};
                        #pubsub_state_dev{affiliation = _Affiliation,
                         access = _Access} ->
                            {_Affiliation, _Access}
                    end
            end,
            Node_Access_Model = get_value(Node_Options, 'access_model'),
            Node_Groups = get_value(Node_Options, 'roster_groups_allowed', []),
            case
                pubsub_db_mnesia:get_items('dev', Host,
                    {NodeIdx, Node_Owners, Node_ItemIds, Node_Access_Model, Node_Groups},
                    {Entity, Affiliation, Access},
                    {Max_Items, ItemIds, _SubId})
            of
                {ok, Pubsub_Items, Cache} ->
                    {result, Pubsub_Items, Cache, Node_Owners, Node_Access_Model,
                     Node_Groups, Affiliation, Access};
                Error ->
                    Error
            end
    end.

%-- Get_Entity_Affiliations --%
-spec(get_entity_affiliations/3 ::
(
  Pubsub_Host :: exmpp_pubsub:host(),
  Entity      :: xmpp_jid:usr_bare(),
  Parameters  :: {NodeId :: undefined | exmpp_pubsub:nodeId()})
    -> Entity_Affiliations :: [Entity_Affiliation :: {
           NodeId      :: exmpp_pubsub:nodeId(),
           Affiliation :: exmpp_pubsub:affiliation()
       }]
).

get_entity_affiliations(Pubsub_Host, Entity, {NodeId}) ->
    pubsub_db_mnesia:get_entity_affiliations('dev', Pubsub_Host, Entity, NodeId).

%-- Get_Entity_Subscriptions --%
-spec(get_entity_subscriptions/3 ::
(
  Pubsub_Host :: exmpp_pubsub:host(),
  Entity      :: xmpp_jid:usr_bare(),
  Parameters  :: {NodeId :: undefined | exmpp_pubsub:nodeId()})
    -> Entity_Subscriptions :: [{
           NodeId        :: exmpp_pubsub:nodeId(),
           Subscriptions :: _%exmpp_pubsub:subscriptions()
       }]
).

get_entity_subscriptions(Pubsub_Host, Entity, {NodeId}) ->
    pubsub_db_mnesia:get_entity_subscriptions('dev', Pubsub_Host, Entity, NodeId).


%-- Get_Node_Affiliations --%
-spec(get_node_affiliations/4 ::
(
  Host        :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host :: exmpp_pubsub:host(),
  Entity      :: xmpp_jid:usr_bare(),
  Parameters  :: {NodeId :: undefined | exmpp_pubsub:nodeId()})
    -> {result, Pubsub_States::[Pubsub_State::mod_pubsub_dev:pubsub_state(),...]}
    %%%
     | {error, 'item-not-found'}
     | {error, 'forbidden'}

).

get_node_affiliations(_Host, _Pubsub_Host, _Entity, {undefined = _NodeId}) ->
    {error, 'item-not-found'};
%%
get_node_affiliations(Host, Pubsub_Host, Entity, {NodeId}) ->
    case pubsub_db_mnesia:read_node('dev', Pubsub_Host, NodeId) of
        undefined ->
            {error, 'item-not-found'};
        #pubsub_node_dev{idx = NodeIdx, owners = Owners, options = Options} ->
            case lists:member(Entity, Owners) of
                true ->
                    {result,
                     _Pubsub_States = pubsub_db_mnesia:index_read_state('dev',
                         NodeIdx)};
                false ->
                    %% Depending on the node access_model, the returned error
                    %% can be 'forbidden' or 'item-not-found' to not disclose
                    %% private nodes
                    case get_value(Options, 'access_model') of
                        %%
                        'open' ->
                            {error, 'forbidden'};
                        %%
                        'presence' ->
                            case
                                is_contact_subscribed_to_node_owners(Host,
                                    Entity, Owners)
                            of
                                false ->
                                    {error, 'item-not-found'};
                                _Node_Owner ->
                                    {error, 'forbidden'}
                            end;
                        %%
                        'roster' ->
                            case
                                is_contact_in_allowed_roster_groups(Entity,
                                    _Rosters_Groups_Allowed = get_value(
                                         Options, 'roster_groups_allowed'))
                            of
                                false ->
                                    {error, 'item-not-found'};
                                {_Node_Owner, _Roster_Group_Allowed} ->
                                    {error, 'forbidden'}
                            end;
                        %%
                        'authorize' ->
                            {error, 'forbidden'};
                        %%
                        'whitelist' ->
                            case
                                pubsub_db_mnesia:read_state('dev', Entity, NodeIdx)
                            of
                                #pubsub_state_dev{affiliation = Affiliation}
                                  when   Affiliation == 'member'
                                  orelse Affiliation == 'publish-only'
                                  orelse Affiliation == 'publisher' ->
                                    {error, 'forbidden'};
                                _Pubsub_State ->
                                    {error, 'item-not-found'}
                            end
                    end
            end
    end.

%-- Get_Node_Subscriptions --%
-spec(get_node_subscriptions/4 ::
(
  Host        :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host :: exmpp_pubsub:host(),
  Entity      :: xmpp_jid:usr_bare(),
  Parameters  :: {NodeId :: undefined | exmpp_pubsub:nodeId()})
    -> {result, Pubsub_States::[Pubsub_State::mod_pubsub_dev:pubsub_state(),...]}
    %%%
     | {error, 'item-not-found'}
     | {error, 'forbidden'}

).

get_node_subscriptions(_Host, _Pubsub_Host, _Entity, {undefined = _NodeId}) ->
    {error, 'item-not-found'};
%%
get_node_subscriptions(Host, Pubsub_Host, Entity, {NodeId}) ->
    case pubsub_db_mnesia:read_node('dev', Pubsub_Host, NodeId) of
        undefined ->
            {error, 'item-not-found'};
        #pubsub_node_dev{idx = NodeIdx, owners = Owners, options = Options} ->
            case lists:member(Entity, Owners) of
                true ->
                    {result,
                     _Pubsub_States = pubsub_db_mnesia:index_read_state('dev',
                         NodeIdx)};
                false ->
                    %% Depending on the node access_model, the returned error
                    %% can be 'forbidden' or 'item-not-found' to not disclose
                    %% private nodes
                    case get_value(Options, 'access_model') of
                        %%
                        'open' ->
                            {error, 'forbidden'};
                        %%
                        'presence' ->
                            case
                                is_contact_subscribed_to_node_owners(Host,
                                    Entity, Owners)
                            of
                                false ->
                                    {error, 'item-not-found'};
                                _Node_Owner ->
                                    {error, 'forbidden'}
                            end;
                        %%
                        'roster' ->
                            case
                                is_contact_in_allowed_roster_groups(Entity,
                                    _Rosters_Groups_Allowed = get_value(Options,
                                        'roster_groups_allowed'))
                            of
                                false ->
                                    {error, 'item-not-found'};
                                {_Node_Owner, _Roster_Group_Allowed} ->
                                    {error, 'forbidden'}
                            end;
                        %%
                        'authorize' ->
                            {error, 'forbidden'};
                        %%
                        'whitelist' ->
                            case
                                pubsub_db_mnesia:read_state('dev', Entity, NodeIdx)
                            of
                                #pubsub_state_dev{affiliation = Affiliation}
                                  when   Affiliation == 'member'
                                  orelse Affiliation == 'publish-only'
                                  orelse Affiliation == 'publisher' ->
                                    {error, 'forbidden'};
                                _Pubsub_State ->
                                    {error, 'item-not-found'}
                            end
                    end
            end
    end.

%% Get_Configure_Subscription_Default
-spec(get_configure_subscription_default/4 ::
(
  Host        :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host :: exmpp_pubsub:host(),
  Entity      :: xmpp_jid:usr_bare(),
  NodeId      :: exmpp_pubsub:nodeId())
    -> ok
    %%%
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
).

get_configure_subscription_default(Host, Pubsub_Host, Entity, NodeId) ->
    case pubsub_db_mnesia:read_node('dev', Pubsub_Host, NodeId) of
        undefined ->
            {error, 'item-not-found'};
        #pubsub_node_dev{idx = NodeIdx, owners = Node_Owners, options = Node_Options} ->
            case lists:member(Entity, Node_Owners) of
                true ->
                    ok;
                false ->
                    Node_Access_Model = get_value(Node_Options, 'access_model'),
                    case pubsub_db_mnesia:read_state('dev', Entity, NodeIdx) of
                        undefined
                          when Node_Access_Model == 'open' ->
                            ok;
                        undefined
                          when Node_Access_Model == 'presence' ->
                            case
                                is_contact_subscribed_to_node_owners(Host,
                                    Entity, Node_Owners)
                            of
                                false       -> {error, 'forbidden'};
                                _Node_Owner ->  ok
                            end;
                        undefined
                          when Node_Access_Model == 'roster' ->
                            case
                                is_contact_in_allowed_roster_groups(Entity,
                                    _Rosters_Groups_Allowed =
                                        get_value(Node_Options,
                                            'roster_groups_allowed', []))
                            of
                                false -> {error, 'forbidden'};
                                _     -> ok
                            end;
                        undefined ->
                            {error, 'forbidden'};
                        #pubsub_state_dev{affiliation = Affiliation,
                         access = Access}
                          when    Affiliation == 'publisher'
                          orelse  Affiliation == 'member'
                          orelse (Affiliation == 'none'
                                      andalso
                                  Node_Access_Model == 'open')
                          orelse Access == 'presence'
                          orelse Access == 'roster' ->
                            ok;
                        #pubsub_state_dev{affiliation = 'none'}
                          when Node_Access_Model == 'presence' ->
                            case
                                is_contact_subscribed_to_node_owners(Host,
                                    Entity, Node_Owners)
                            of
                                false ->
                                    {error, 'forbidden'};
                                _Node_Owner ->
                                    ok
                            end;
                        #pubsub_state_dev{affiliation = 'none'}
                          when Node_Access_Model == 'roster' ->
                            case
                                is_contact_in_allowed_roster_groups(Entity,
                                    _Rosters_Groups_Allowed =
                                        get_value(Node_Options,
                                            'roster_groups_allowed', []))
                            of
                                false ->
                                    {error, 'forbidden'};
                                _ ->
                                    ok
                            end;
                        _ ->
                            {error, 'forbidden'}
                        
                    end
            end
    end.

%-- Get_Configure_Subscription --%
-spec(get_configure_subscription/6 ::
(
  Host            :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host     :: exmpp_pubsub:host(),
  Entity          :: xmpp_jid:usr_bare(),
  Plugin          :: exmpp_pubsub:plugin(),
  Pubsub_Features :: exmpp_pubsub:pubsub_features(),
  Parameters      :: {NodeId   :: undefined | exmpp_pubsub:nodeId(),
                      SubId    :: undefined | exmpp_pubsub:subId(),
                      Resource :: undefined
                                | xmpp_jid:resource_jid()
                                | {'caps', xmpp_jid:resource_jid()}
                     })
    -> {result,
        Subscription :: exmpp_pubsub:subscription_subscribed()
                      | exmpp_pubsub:subscription_unconfigured()}
    %%%
     | {error, 'item-not-found'}
     | {error, 'unexpected-request', 'not-subscribed'}
     | {error, 'forbidden'}
     %
     | {error, 'unexpected-request', 'not-subscribed'}
     | {error, 'bad-request',        'subid-required'}
     | {error, 'forbidden'}
     | {error, 'not-acceptable',     'invalid-subid'}
).

get_configure_subscription(_Host, _Pubsub_Host, _Entity, _Plugin,
  _Pubsub_Features, {undefined = _NodeId, _SubId, _Resource}) ->
    {error, 'item-not-found'};
%%
get_configure_subscription(_Host, Pubsub_Host, Entity, Plugin,
  Pubsub_Features, {NodeId, SubId, Resource}) ->
    case pubsub_db_mnesia:read_node('dev', Pubsub_Host, NodeId) of
        undefined ->
            {error, 'item-not-found'};
        #pubsub_node_dev{idx = NodeIdx, owners = Node_Owners, options = Node_Options} ->
            case pubsub_db_mnesia:read_state('dev', Entity, NodeIdx) of
                undefined ->
                    {error, 'unexpected-request', 'not-subscribed'};
                #pubsub_state_dev{affiliation = Affiliation}
                  when   Affiliation == 'publish-only'
                  orelse Affiliation == 'outcast' ->
                    {error, 'forbidden'};
                #pubsub_state_dev{subscriptions = []} ->
                    {error, 'unexpected-request', 'not-subscribed'};
                #pubsub_state_dev{subscriptions = Subscriptions} ->
                    case filter_subscription(Subscriptions, SubId, Resource) of
                        {ok, Subscription} -> {result, Subscription};
                        Error              -> Error
                    end
            end
    end.

-spec(filter_subscription/3 ::
(
  Subscriptions :: exmpp_pubsub:subscriptions(),
  SubId         :: undefined | exmpp_pubsub:subId(),
  Resource      :: undefined
                 | xmpp_jid:resource_jid()
                 | {'caps', xmpp_jid:resource_jid()})
    -> {ok,
        Subscription :: exmpp_pubsub:subscription_subscribed()
                      | exmpp_pubsub:subscription_unconfigured()}
    %%%
     | {error, 'unexpected-request', 'not-subscribed'}
     | {error, 'bad-request',        'subid-required'}
     | {error, 'forbidden'}
     | {error, 'not-acceptable',     'invalid-subid'}
).

filter_subscription(Subscriptions, undefined = _SubId, Resource) ->
    case lists:keytake(Resource, 3, Subscriptions) of
        {value, {Subscription_State, SubId, Resource, Subscription_Options},
         _Subscriptions} ->
            case lists:keymember(Resource, 3, _Subscriptions) of
                false
                  when Subscription_State == 'pending' ->
                    {error, 'forbidden'};
                false ->
                    {ok,
                     {Subscription_State, SubId, Resource, Subscription_Options}};
                true ->
                    {error, 'bad-request', 'subid-required'}
            end;
         false ->
            {error, 'unexpected-request', 'not-subscribed'}
    end;
%%
filter_subscription(Subscriptions, SubId, Resource) ->
    case lists:keyfind(SubId, 2, Subscriptions) of
        %%
        {'pending', _, Resource, _} ->
            {error, 'forbidden'};
        %%
        {Subscription_State, SubId, Resource, Subscription_Options} ->
            {ok, {Subscription_State, SubId, Resource, Subscription_Options}};
        %%
        {_, SubId, _, _} ->
            {error, 'unexpected-request', 'not-subscribed'};
        false ->
            case lists:keyfind(Resource, 3, Subscriptions) of
                {'pending', _, Resource, _} ->
                    {error, 'forbidden'};
                {_, _, Resource, _} ->
                    {error, 'not-acceptable', 'invalid-subid'};
                _ ->
                    {error, 'unexpected-request', 'not-subscribed'}
            end
    end.

%-- Get_Configure_Node --%
-spec(get_configure_node/4 ::
(
  Host        :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host :: exmpp_pubsub:host(),
  Entity      :: xmpp_jid:usr_bare(),
  Parameters  :: {NodeId :: undefined | exmpp_pubsub:nodeId()})
    -> {result, Node_Options :: pubsub_options:options_node()}
    %%%
     | {error, 'bad-request', 'nodeid-required'}
     | {error, 'item-not-found'}
     | {error, 'forbidden'}
).

get_configure_node(_Host, _Pubsub_Host, _Entity, {undefined = _NodeId}) ->
    {error, 'bad-request', 'nodeid-required'};
%%
get_configure_node(_Host, Pubsub_Host, Entity, {NodeId}) ->
    case pubsub_db_mnesia:read_node('dev', Pubsub_Host, NodeId) of
        undefined ->
            {error, 'item-not-found'};
        #pubsub_node_dev{owners = Node_Owners, options = Node_Options} ->
            case lists:member(Entity, Node_Owners) of
                true  -> {result, Node_Options};
                false -> {error, 'forbidden'}
            end
    end.
