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
%%% Portions created by ProcessOne are Copyright 2006-2011, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2011, ProcessOne.
%%%
%%% @copyright 2006-2011 ProcessOne
%%% @author Karim Gemayel <karim.gemayel@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @headerfile "pubsub_dev.hrl"

-module(pubsub_broadcast).
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
  has_subscriptions/1,
  get_resources/2,
  get_resources_show/2,
  rosters_groups_allowed_cache/2
]).


-spec(notify_subscriptions/5 ::
(
  Host                   :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host            :: exmpp_pubsub:host(),
  NodeId                 :: exmpp_pubsub:nodeId(),
  Notification_Type      :: pubsub_options:notification_type(),
  Entities_Subscriptions :: [Entity_Subscriptions :: {
                                 Entity :: xmpp_jid:usr_entity(),
                                 Subscriptions :: _%sub:subscriptions()
                             }])
    -> 'ok'
).

notify_subscriptions(_Host, Pubsub_Host, NodeId, Notification_Type,
  Entities_Subscriptions) ->
    Pubsub_Component_Jid = jlib:make_jid(<<>>, Pubsub_Host, <<>>),
    lists:foreach(fun
        (_Entity = {{U,S,_R}, Subscriptions}) ->
            lists:foreach(fun
                ({Subscription_State, SubId, Resource, _Subscription_Options}) ->
                    Recipient_Jid = jlib:make_jid(U,S,Resource),
                    ejabberd_router:route(_From = Pubsub_Component_Jid,
                        _To = Recipient_Jid,
                        _Stanza_Message = exmpp_pubsub:xmlel_message(
                            Notification_Type,
                            [exmpp_pubsub:xmlel_event([
                                 exmpp_pubsub:xmlel_subscription('pubsub#event',
                                     NodeId,
                                     _Jid = pubsub_tools:jid_to_string(Recipient_Jid),
                                     SubId,
                                     Subscription_State
                                 )
                             ])]
                        )
                    )
            
            end, Subscriptions)
    end, Entities_Subscriptions).

%%
-spec(notify_subscription/6 ::
(
  Host                 :: xmpp_jid:raw_jid_component_bare(),
  NodeId               :: exmpp_pubsub:nodeId(),
  Pubsub_Component_Jid :: xmpp_jid:component_bare(),
  Recipient            :: xmpp_jid:entity_bare() | [xmpp_jid:entity_bare(),...],
  Notification_Type    :: pubsub_options:notification_type(),
  Subscription         :: {
    Subscriber         :: xmpp_jid:raw_jid_entity(),
    Subscription_State :: exmpp_pubsub:subscription_state(),
    SubId              :: exmpp_pubsub:subId()
  })
    -> 'ok'
).

notify_subscription(_Host, NodeId, Pubsub_Component_Jid, Recipients,
  Notification_Type, {Subscriber, Subscription_State, SubId})
  when is_list(Recipients) ->
    Stanza_Message = exmpp_pubsub:xmlel_message(Notification_Type,
      [exmpp_pubsub:xmlel_event([
           exmpp_pubsub:xmlel_subscription('pubsub#event',
               NodeId, Subscriber, SubId, Subscription_State)
            ])
        ]),
    lists:foreach(fun
        (Recipient) ->
            ejabberd_router:route(_From = Pubsub_Component_Jid,
                _To = pubsub_tools:make_jid(Recipient), Stanza_Message)
    end, Recipients);
%%
notify_subscription(_Host, NodeId, Pubsub_Component_Jid, Recipient,
  Notification_Type, {Subscriber, Subscription_State, SubId}) ->
  Stanza_Message = exmpp_pubsub:xmlel_message(Notification_Type,
      [exmpp_pubsub:xmlel_event([
           exmpp_pubsub:xmlel_subscription('pubsub#event',
               NodeId, Subscriber, SubId, Subscription_State)
            ])
        ]),
  ejabberd_router:route(_From = Pubsub_Component_Jid,
    _To = Recipient, Stanza_Message).

%%
-spec(notify_delete/6 ::
(
  Host              :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host       :: exmpp_pubsub:host(),
  NodeId            :: exmpp_pubsub:nodeId(),
  Notification_Type :: pubsub_options:notification_type(),
  RedirectURI       :: undefined | binary(),
  Recipients        :: [Recipient::xmpp_jid:usr_bare(),...])
    -> 'ok'
).

notify_delete(_Host, Pubsub_Host, NodeId, Notification_Type, RedirectURI,
  Recipients) ->
    Pubsub_Component_Jid = jlib:make_jid(<<>>, Pubsub_Host, <<>>),
    Stanza_Message = exmpp_pubsub:xmlel_message(Notification_Type,
        [exmpp_pubsub:xmlel_event([
             exmpp_pubsub:xmlel_delete(NodeId, RedirectURI)])]
    ),
    lists:foreach(fun
        (Recipient) ->
            ejabberd_router:route(_To = Pubsub_Component_Jid,
                _From = pubsub_tools:make_jid(Recipient),
                Stanza_Message
            )
    end, Recipients).

%%
-spec(notify_purge/5 ::
(
  Host              :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host       :: exmpp_pubsub:host(),
  NodeId            :: exmpp_pubsub:nodeId(),
  Notification_Type :: pubsub_options:notification_type(),
  Recipients        :: [Recipient::xmpp_jid:usr_bare(),...])
    -> 'ok'
).

notify_purge(_Host, Pubsub_Host, NodeId, Notification_Type, Recipients) ->
    Pubsub_Component_Jid = jlib:make_jid(<<>>, Pubsub_Host, <<>>),
    Stanza_Message = exmpp_pubsub:xmlel_message(Notification_Type,
        [exmpp_pubsub:xmlel_event([
             exmpp_pubsub:xmlel_purge(NodeId)])]),
    lists:foreach(fun
        (Recipient) ->
            ejabberd_router:route(_To = Pubsub_Component_Jid,
                _From = pubsub_tools:make_jid(Recipient),
                Stanza_Message
            )
    end, Recipients).

%%
-spec(notify_publish/8 ::
(
  From              :: xmpp_jid:component_bare(),
  To                :: xmpp_jid:entity(),
  Notification_Type :: 'headline' | 'normal',
  NodeId            :: exmpp_pubsub:nodeId(),
  ItemId            :: exmpp_pubsub:itemId(),
  Publisher         :: undefined | xmpp_jid:raw_jid_entity(),
  Payload           :: exmpp_pubsub:payload(),
  SubIds            :: [SubId::exmpp_pubsub:subId(),...])
    -> 'ok'
).

notify_publish(From, To, Notification_Type, NodeId, ItemId, Publisher, Payload,
  SubIds) ->
    ejabberd_router:route(From, To,
        _Stanza_Message = exmpp_pubsub:xmlel_message(Notification_Type,
            case SubIds of
                [_SubId] ->
                    [exmpp_pubsub:xmlel_event([
                         exmpp_pubsub:xmlel_items('pubsub#event', NodeId,
                             [exmpp_pubsub:xmlel_item('pubsub#event', ItemId,
                                  Publisher, Payload)
                             ])
                         ])
                    ];
                _SubIds ->
                    [exmpp_pubsub:xmlel_event([
                         exmpp_pubsub:xmlel_items('pubsub#event', NodeId,
                             [exmpp_pubsub:xmlel_item('pubsub#event', ItemId,
                                  Publisher, Payload)
                             ])
                         ]),
                     exmpp_pubsub:xmlel_headers(
                        [exmpp_pubsub:xmlel_header(SubId) || SubId <- SubIds])
                    ]
            end)
        ).


-define(Xmlel(NS, Name, Attrs, Children),
(
    #xmlel{
        name     = Name,
        attrs    = [{<<"xmlns">>, NS} | Attrs],
        children = Children
    }
)).

-define(CData(Data),
(
    {xmlcdata, Data}
)).

-define(Stanza_Message(Type, Id, Children),
(
    ?Xmlel(?NS_JABBER_CLIENT, <<"message">>, [{<<"type">>, Type}, {<<"id">>, Id}],
        Children)
)).

%%
-spec(stanza_notify_publish/5 ::
(
  Notification_Type :: pubsub_options:notification_type(),
  NodeId            :: exmpp_pubsub:nodeId(),
  ItemId            :: exmpp_pubsub:itemId(),
  Publisher         :: undefined | xmpp_jid:usr_entity() | xmpp_jid:raw_jid_entity(),
  Payload           :: exmpp_pubsub:payload())
    -> Stanza_Notify_Publish::xmlel()
).

stanza_notify_publish(Notification_Type, NodeId, ItemId, Publisher, Payload)
  when is_tuple(Publisher) ->
    stanza_notify_publish(Notification_Type, NodeId, ItemId,
        pubsub_tools:jid_to_string(Publisher), Payload);

stanza_notify_publish(Notification_Type, NodeId, ItemId, Publisher, Payload) ->
    ?Stanza_Message(list_to_binary(atom_to_list(Notification_Type)), exmpp_pubsub:id(),[
        ?Xmlel(?NS_PUBSUB_EVENT, <<"event">>, [], [
            ?Xmlel(?NS_PUBSUB_EVENT, <<"items">>, [{<<"node">>, NodeId}], [
                ?Xmlel(?NS_PUBSUB_EVENT, <<"item">>,
                    case Publisher of
                        undefined ->
                            [{<<"id">>, ItemId}];
                        _ ->
                            [{<<"id">>, ItemId}, {<<"publisher">>, Publisher}]
                    end,
                    case Payload of
                        [] -> [];
                        _  -> [Payload]
                    end)
                ])
            ])
        ]).


%%
-spec(stanza_notify_retract/3 ::
(
  Notification_Type :: pubsub_options:notification_type(),
  NodeId            :: exmpp_pubsub:nodeId(),
  ItemId            :: exmpp_pubsub:itemId())
    -> Stanza_Notify_Retract::xmlel()
).

stanza_notify_retract(Notification_Type, NodeId, ItemId) ->
    ?Stanza_Message(list_to_binary(atom_to_list(Notification_Type)), exmpp_pubsub:id(),[
        ?Xmlel(?NS_PUBSUB_EVENT, <<"event">>, [], [
            ?Xmlel(?NS_PUBSUB_EVENT, <<"items">>, [{<<"node">>, NodeId}], [
                ?Xmlel(?NS_PUBSUB_EVENT, <<"retract">>, [{<<"id">>, ItemId}], [])
            ])
        ])
    ]).


%%
-spec(notify_retract/6 ::
(
  From              :: xmpp_jid:component_bare(),
  To                :: xmpp_jid:entity(),
  Notification_Type :: 'headline' | 'normal',
  NodeId            :: exmpp_pubsub:nodeId(),
  ItemId            :: exmpp_pubsub:itemId(),
  SubIds            :: [SubId::exmpp_pubsub:subId(),...])
    -> none
).

notify_retract(From, To, Notification_Type, NodeId, ItemId, SubIds) ->
    ejabberd_router:route(From, To,
        _Stanza_Message = exmpp_pubsub:xmlel_message(Notification_Type,
            case SubIds of
                [_SubId] ->
                    [exmpp_pubsub:xmlel_event([
                         exmpp_pubsub:xmlel_items('pubsub#event', NodeId,
                             [exmpp_pubsub:xmlel_retract(ItemId)])
                         ])
                    ];
                _SubIds ->
                    [exmpp_pubsub:xmlel_event([
                         exmpp_pubsub:xmlel_items('pubsub#event', NodeId,
                             [exmpp_pubsub:xmlel_retract(ItemId)])
                         ]),
                     exmpp_pubsub:xmlel_headers(
                         [exmpp_pubsub:xmlel_header(SubId) || SubId <- SubIds])
                    ]
            end)
        ).


broadcast_item(From, To, Stanza, SubIds) ->
    ejabberd_router:route(From, To,
        case SubIds of
            [_] -> Stanza;
            _   -> exmpp_xml:append_child(Stanza, shim_headers(SubIds))
        end).


shim_headers(SubIds) ->
    ?Xmlel(?NS_SHIM, <<"headers">>, [],
        [?Xmlel(?NS_SHIM, <<"header">>, [{<<"name">>, <<"SubID">>}], [?CData(SubId)])
        || SubId <- SubIds]
        ).

%%
xmlel_delay(From, DateTime) ->
    {T_string, Tz_string} = jlib:timestamp_to_iso(
        calendar:now_to_datetime(DateTime), 'utc'),
    ?Xmlel(?NS_DELAY, <<"delay">>,
        [{<<"from">>, pubsub_tools:jid_to_string(From)},
         {<<"stamp">>, list_to_binary(T_string ++ Tz_string)}],
        []).

%%
-spec(broadcast_publish/8 ::
(
  Host              :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host       :: exmpp_pubsub:host(),
  NodeId            :: exmpp_pubsub:nodeId(),
  Node_Options      :: pubsub_options:options_node(),
  Node_Owners       :: [Node_Owner::xmpp_jid:usr_bare(),...],
  Broadcasted_Items :: {
    Published_Items :: [{
      ItemId       :: exmpp_pubsub:itemId(),
      Item_Options :: [] | pubsub_options:options_item(),
      Payload      :: exmpp_pubsub:payload(),
      Publisher    :: xmpp_jid:usr_entity()
    }],
    %
    Retracted_Items :: [{
      ItemId       :: exmpp_pubsub:itemId(),
      Item_Options :: [] | pubsub_options:options_item()
    }]
  },
  Subscription      :: undefined
                     | {Subscriber         :: xmpp_jid:raw_jid_entity(),
                        Subscription_State :: _,%exmpp_pubsub:subscription_state(),
                        SubId              :: exmpp_pubsub:subId()},
  Pubsub_States :: [Pubsub_State::mod_pubsub_dev:pubsub_state(),...])
    -> 'ok'
).

broadcast_publish(Host, Pubsub_Host, NodeId, Node_Options, Node_Owners,
  {Published_Items, Retracted_Items}, Subscription, Pubsub_States) ->
    Event = #event{
        host      = Host,
        component = jlib:make_jid(<<>>, Pubsub_Host, <<>>),
        node      = #node{
            id                     = NodeId,
            owners                 = Node_Owners,
            access_model           = get_value(Node_Options, 'access_model'),
            rosters_groups_allowed = get_value(Node_Options, 'roster_groups_allowed')
        }
    },
    Broadcasted_Items = lists:map(fun
        %%
        ({ItemId, _Item_Options = []}) ->
            #item{
                presence_based_delivery = get_value(Node_Options,
                    'presence_based_delivery'),
                stanza = stanza_notify_retract(
                    get_value(Node_Options, 'notification_type', 'headline'),
                    NodeId, ItemId)
            };
        %%
        ({ItemId, Item_Options}) ->
            #item{
                access_model = get_value(Item_Options, 'access_model', undefined),
                presence_based_delivery = get_value({Node_Options, Item_Options},
                    'presence_based_delivery'),
                rosters_groups_allowed = get_value(Item_Options,
                    'roster_groups_allowed', []),
                stanza = stanza_notify_retract(
                    get_value({Node_Options, Item_Options}, 'notification_type',
                        'headline'),
                    NodeId, ItemId)
            };
        %%
        ({ItemId, _Item_Options = [], Payload, Publisher}) ->
            #item{
                presence_based_delivery = get_value(Node_Options,
                    'presence_based_delivery'),
                stanza = stanza_notify_publish(
                    get_value(Node_Options, 'notification_type', 'headline'),
                    NodeId,
                    ItemId,
                    _Publisher = case
                        get_value(Node_Options, 'itemreply', 'owner')
                    of
                        'publisher' -> pubsub_tools:jid_to_string(Publisher);
                       _Owner       -> undefined
                    end,
                    Payload)
            };
        %%
        ({ItemId, Item_Options, Payload, Publisher}) ->
            #item{
                access_model = get_value(Item_Options, 'access_model'),
                presence_based_delivery = get_value({Node_Options, Item_Options},
                    'presence_based_delivery'),
                rosters_groups_allowed = get_value(Item_Options,
                    'roster_groups_allowed', []),
                stanza = stanza_notify_publish(
                    get_value({Node_Options, Item_Options}, 'notification_type',
                        'headline'),
                    NodeId,
                    ItemId,
                    _Publisher = case
                        get_value({Node_Options, Item_Options}, 'itemreply',
                            'owner')
                    of
                        'publisher' -> pubsub_tools:jid_to_string(Publisher);
                       _Owner       -> undefined
                    end,
                    Payload)
            }
        %  
    end, lists:append(Retracted_Items, Published_Items)),
    %
    lists:foreach(fun
        %%
        (#pubsub_state_dev{id = {{U,S,R} = _Entity, _NodeIdx},
         affiliation = Affiliation, subscriptions = Subscriptions})
          when   (Affiliation == 'member'
          orelse  Affiliation == 'owner'
          orelse  Affiliation == 'publisher')
          andalso Subscriptions =/= [] ->
            case Subscription =/= undefined andalso Affiliation == 'owner' of
                true ->
                    notify_subscription(Host, NodeId,
                        _Pubsub_Component_Jid = Event#event.component,
                        _Recipient = jlib:make_jid(U,S,<<>>),
                        _Notification_Type = get_value(Node_Options,
                            'notification_type'),
                         Subscription);
                false ->
                    ok
            end,
            lists:foldl(fun broadcast_item/2,
                Event#event{
                    entity = #entity{
                        id            = {U,S,R},
                        local         = S == Event#event.host,
                        affiliation   = Affiliation,
                        subscriptions = Subscriptions
                    },
                    cache  = #cache{},
                    subids = #subids{}
                },
                Broadcasted_Items);
        %%
        (_Pubsub_State) ->
            ok
        %
    end, Pubsub_States).

%%

-spec(broadcast_publish_last/9 ::
(
  Host            :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host     :: exmpp_pubsub:host(),
  NodeId          :: exmpp_pubsub:nodeId(),
  Node_Options    :: pubsub_options:options_node(),
  Node_Owners     :: [Node_Owner::xmpp_jid:usr_bare(),...],
  Published_Items :: [Published_Item :: {
      ItemId       :: exmpp_pubsub:itemId(),
      Item_Options :: [] | pubsub_options:options_item(),
      Payload      :: exmpp_pubsub:payload(),
      Publisher    :: xmpp_jid:usr_entity(),
      DateTime     :: erlang:timestamp()
  },...],
  Entity          :: xmpp_jid:usr_entity(),
  Affiliation     :: 'member' | 'owner' | 'publisher',
  Subscription    :: exmpp_pubsub:subscription())
    -> none()
).

broadcast_publish_last(Host, Pubsub_Host, NodeId, Node_Options, Node_Owners,
  Published_Items, {U,S,R} = Entity, Affiliation, Subscription) ->
    Event = #event{
        host      = Host,
        component = jlib:make_jid(<<>>, Pubsub_Host, <<>>),
        node      = #node{
            id                     = NodeId,
            owners                 = Node_Owners,
            access_model           = get_value(Node_Options, 'access_model'),
            rosters_groups_allowed = get_value(Node_Options, 'roster_groups_allowed')
        }
    },
    Broadcasted_Items = lists:map(fun
        %%
        ({ItemId, _Item_Options = [], Payload, Publisher, DateTime}) ->
            #item{
                presence_based_delivery = get_value(Node_Options,
                    'presence_based_delivery'),
                stanza = exmpp_xml:append_child(stanza_notify_publish(
                    get_value(Node_Options, 'notification_type', 'headline'),
                    NodeId,
                    ItemId,
                    _Publisher = case
                        get_value(Node_Options, 'itemreply', 'owner')
                    of
                        'publisher' -> pubsub_tools:jid_to_string(Publisher);
                       _Owner       -> undefined
                    end,
                    Payload),
                    xmlel_delay(Publisher, DateTime))
            };
        %%
        ({ItemId, Item_Options, Payload, Publisher, DateTime}) ->
            #item{
                access_model = get_value(Item_Options, 'access_model'),
                presence_based_delivery = get_value({Node_Options, Item_Options},
                    'presence_based_delivery'),
                rosters_groups_allowed = get_value(Item_Options,
                    'roster_groups_allowed', []),
                stanza = exmpp_xml:append_child(stanza_notify_publish(
                    get_value({Node_Options, Item_Options}, 'notification_type',
                        'headline'),
                    NodeId,
                    ItemId,
                    _Publisher = case
                        get_value({Node_Options, Item_Options}, 'itemreply',
                            'owner')
                    of
                        'publisher' -> pubsub_tools:jid_to_string(Publisher);
                       _Owner       -> undefined
                    end,
                    Payload),
                    xmlel_delay(Publisher, DateTime))
            }
        %  
    end, Published_Items),
    case get_value(Node_Options, 'notify_sub', false) of
        true ->
            {Subscription_State, SubId, Resource, _} = Subscription,
            Subscriber = pubsub_tools:jid_to_string({U,S,Resource}),
            Notification_Type = get_value(Node_Options, 'notification_type',
                'headline'),
            lists:foreach(fun
                (Node_Owner) ->
                    notify_subscription(Host, NodeId,
                        _Pubsub_Component_Jid = Event#event.component,
                        _Recipient = pubsub_tools:make_jid(Node_Owner),
                        Notification_Type,
                         {Subscriber, Subscription_State, SubId})
            
            end, Node_Owners);
        false ->
            ok
    end,
    %%
    lists:foldl(fun broadcast_item/2,
        Event#event{
            entity = #entity{
                id            = {U,S,R},
                local         = S == Event#event.host,
                affiliation   = Affiliation,
                subscriptions = [Subscription]
            },
            cache  = #cache{},
            subids = #subids{}
        },
        Broadcasted_Items).


-spec(broadcast_publish_last2/9 ::
(
  Host            :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host     :: exmpp_pubsub:host(),
  NodeId          :: exmpp_pubsub:nodeId(),
  Node_Options    :: pubsub_options:options_node(),
  Node_Owners     :: [Node_Owner::xmpp_jid:usr_bare(),...],
  Published_Items :: [Published_Item :: {
      ItemId       :: exmpp_pubsub:itemId(),
      Item_Options :: [] | pubsub_options:options_item(),
      Payload      :: exmpp_pubsub:payload(),
      Publisher    :: xmpp_jid:usr_entity(),
      DateTime     :: erlang:timestamp()
  },...],
  Entity          :: xmpp_jid:usr_entity(),
  Affiliation     :: 'member' | 'owner' | 'publisher',
  Subscriptions    :: exmpp_pubsub:subscription())
    -> none()
).

broadcast_publish_last2(Host, Pubsub_Host, NodeId, Node_Options, Node_Owners,
  Published_Items, {U,S,R} = Entity, Affiliation, Subscriptions) ->
    Event = #event{
        host      = Host,
        component = jlib:make_jid(<<>>, Pubsub_Host, <<>>),
        node      = #node{
            id                     = NodeId,
            owners                 = Node_Owners,
            access_model           = get_value(Node_Options, 'access_model'),
            rosters_groups_allowed = get_value(Node_Options, 'roster_groups_allowed')
        }
    },
    Broadcasted_Items = lists:map(fun
        %%
        ({ItemId, _Item_Options = [], Payload, Publisher, DateTime}) ->
            #item{
                presence_based_delivery = get_value(Node_Options,
                    'presence_based_delivery'),
                stanza = exmpp_xml:append_child(stanza_notify_publish(
                    get_value(Node_Options, 'notification_type', 'headline'),
                    NodeId,
                    ItemId,
                    _Publisher = case
                        get_value(Node_Options, 'itemreply', 'owner')
                    of
                        'publisher' -> pubsub_tools:jid_to_string(Publisher);
                       _Owner       -> undefined
                    end,
                    Payload),
                    xmlel_delay(Publisher, DateTime))
            };
        %%
        ({ItemId, Item_Options, Payload, Publisher, DateTime}) ->
            #item{
                access_model = get_value(Item_Options, 'access_model'),
                presence_based_delivery = get_value({Node_Options, Item_Options},
                    'presence_based_delivery'),
                rosters_groups_allowed = get_value(Item_Options,
                    'roster_groups_allowed', []),
                stanza = exmpp_xml:append_child(stanza_notify_publish(
                    get_value({Node_Options, Item_Options}, 'notification_type',
                        'headline'),
                    NodeId,
                    ItemId,
                    _Publisher = case
                        get_value({Node_Options, Item_Options}, 'itemreply',
                            'owner')
                    of
                        'publisher' -> pubsub_tools:jid_to_string(Publisher);
                       _Owner       -> undefined
                    end,
                    Payload),
                    xmlel_delay(Publisher, DateTime))
            }
        %  
    end, Published_Items),
    %%
    lists:foldl(fun broadcast_item/2,
        Event#event{
            entity = #entity{
                id            = {U,S,R},
                local         = S == Event#event.host,
                affiliation   = Affiliation,
                subscriptions = Subscriptions
            },
            cache  = #cache{},
            subids = #subids{}
        },
        Broadcasted_Items).

%%
-define(Resources_Subids(Item, Event),
    (Item#item.presence_based_delivery == false
         andalso
     Event#event.subids#subids.no_presence =/= undefined
         andalso
     Event#event.subids#subids.no_presence =/= [])
             orelse
    (Item#item.presence_based_delivery == true
         andalso
     Event#event.subids#subids.presence =/= undefined
         andalso
    Event#event.subids#subids.presence =/= [])
).

-define(Item_Access_Model(Item, Event),
    Item#item.access_model == undefined
        orelse
    Item#item.access_model == 'open'
        orelse
    Item#item.access_model == 'authorize'
        orelse
    Item#item.access_model == 'whitelist'
        orelse
    (Item#item.access_model == Event#event.node#node.access_model
                 andalso
     Item#item.access_model =/= 'roster')
        orelse
    (Item#item.access_model == 'presence'
            andalso
           (Event#event.entity#entity.affiliation == 'owner'
                orelse
%            Event#event.entity#entity.affiliation == 'publisher'
%                orelse
            Event#event.node#node.access_model == 'presence'
                orelse
            Event#event.node#node.access_model == 'roster'
                orelse
            Event#event.cache#cache.presence_subscriptions == true)
          )
        orelse
    (Item#item.access_model == 'roster'
            andalso
           (Event#event.entity#entity.affiliation == 'owner'
                orelse
            Event#event.entity#entity.affiliation == 'publisher'
                orelse
            Event#event.cache#cache.rosters_groups == true)
          )
).


-define(Broadcast_Event(Item, Event),
  (?Resources_Subids(Item, Event)) and (?Item_Access_Model(Item, Event))
).

-define(Presence_Based_Delivery(Item, Event),
    Item#item.presence_based_delivery == true
        andalso
    Event#event.subids#subids.presence == undefined
).

-define(No_Presence_Based_Delivery(Item, Event),
    Item#item.presence_based_delivery == false
        andalso
    Event#event.subids#subids.no_presence == undefined
).

-define(Presence_Based_Delivery_Cache(Item, Event),
    Item#item.presence_based_delivery == true
        andalso
    Event#event.cache#cache.presence == undefined
).

-define(Item_Access_Model_Presence(Item, Event),
  Item#item.access_model == 'presence'
      andalso
  Event#event.cache#cache.presence_subscriptions == undefined
).

-define(Item_Access_Model_Roster(Item, Event),
  Item#item.access_model == 'roster'
      andalso
  Event#event.cache#cache.rosters_groups == undefined
).


-spec(broadcast_item/2 ::
(
  Item  :: mod_pubsub_dev:item(),
  Event :: mod_pubsub_dev:event())
    -> Event::mod_pubsub_dev:event()
%      Event::#event{cache::#cache{rosters_groups :: undefined}}
).



%%
broadcast_item(#item{stanza = Stanza} = Item, 
  #event{entity = #entity{id = {U,S,_R}}} = Event)
  when ?Broadcast_Event(Item, Event) ->
    lists:foreach(fun
        ({Resource, SubIds}) ->
            broadcast_item(_From = Event#event.component,
                _To = jlib:make_jid(U,S,Resource),
                Stanza,
                SubIds)
    end,
        _Resources_SubIds = case Item#item.presence_based_delivery of
            true  -> Event#event.subids#subids.presence;
            false -> Event#event.subids#subids.no_presence
        end),
    Event#event{cache = Event#event.cache#cache{rosters_groups = undefined}};
%%
broadcast_item(Item, Event)
  when ?No_Presence_Based_Delivery(Item, Event) ->
    {Presence_Cache, Resources_SubIds} = resources_subids(
        _Entity = Event#event.entity#entity.id,
        _Local_Entity = Event#event.entity#entity.local,
        _Presence_Based_Delivery = false,
        _Subscriptions = Event#event.entity#entity.subscriptions,
        {Event#event.cache#cache.presence, []}),
    case Resources_SubIds of
        [] ->
            Event#event{
                cache = Event#event.cache#cache{
                    presence = Presence_Cache,
                    rosters_groups = undefined
                },
                subids = Event#event.subids#subids{
                    no_presence = []
                }
            };
        _Resources_SubIds ->
            broadcast_item(Item,
                Event#event{
                    cache = Event#event.cache#cache{
                        presence = Presence_Cache
                    },
                    subids = Event#event.subids#subids{
                        no_presence = Resources_SubIds
                    }
                }
            )
    end;
%%
broadcast_item(Item, #event{entity = #entity{id = {U,S,_R}}} = Event)
  when ?Presence_Based_Delivery_Cache(Item, Event) ->
    case get_resources_show(U,S) of
        [] ->
            Event#event{
                cache = Event#event.cache#cache{
                    presence = [],
                    rosters_groups = undefined
                }
            };
        Resources_Show ->
            broadcast_item(Item,
                Event#event{
                    cache = Event#event.cache#cache{
                        presence = Resources_Show
                    }
                })
    end;
%%
broadcast_item(Item, Event)
  when ?Presence_Based_Delivery(Item, Event) ->
    {Presence_Cache, Resources_SubIds} = resources_subids(
        _Entity = Event#event.entity#entity.id,
        _Local_Entity = Event#event.entity#entity.local,
        _Presence_Based_Delivery = true,
        _Subscriptions = Event#event.entity#entity.subscriptions,
        {Event#event.cache#cache.presence, []}),
    case Resources_SubIds of
        [] ->
            Event#event{
                cache = Event#event.cache#cache{
                    presence = Presence_Cache,
                    rosters_groups = undefined
                },
                subids = Event#event.subids#subids{
                    presence = []}
            };
        _Resources_SubIds ->
            broadcast_item(Item,
                Event#event{
                    cache = Event#event.cache#cache{
                        presence = Presence_Cache
                    },
                    subids = Event#event.subids#subids{
                        presence = Resources_SubIds
                    }
                })
    end;
%%
broadcast_item(Item, Event)
  when ?Item_Access_Model_Presence(Item, Event) ->
    case
        is_contact_subscribed_to_node_owners(
            _Host = Event#event.host,
            _Entity = Event#event.entity#entity.id,
            _Node_Owners = Event#event.node#node.owners
        )
    of
        false ->
            Event#event{
                cache = Event#event.cache#cache{
                    presence_subscriptions = false,
                    rosters_groups = undefined
                }
            };
        _Node_Owner ->
            broadcast_item(Item,
                Event#event{
                    cache = Event#event.cache#cache{
                        presence_subscriptions = true
                    }
                }
            )
    end;
%%
broadcast_item(Item, Event)
  when ?Item_Access_Model_Roster(Item, Event) ->
    case Event#event.node#node.access_model of
        'roster' ->
            case Item#item.rosters_groups_allowed of
                %%
                [] ->
                    broadcast_item(Item,
                        Event#event{
                            cache = Event#event.cache#cache{
                                rosters_groups = true
                            }
                        }
                    );
                %%
                Item_Rosters_Groups_Allowed ->
                    case
                        is_contact_in_allowed_roster_groups(
                            _Contact = Event#event.entity#entity.id,
                            _Rosters_Groups_Allowed = Item_Rosters_Groups_Allowed
                        )
                    of
                        false ->
                            Event;
                        {_Node_Owner, _Roster_Group_Allowed} ->
                            broadcast_item(Item,
                                Event#event{
                                    cache = Event#event.cache#cache{
                                        rosters_groups = true
                                    }
                                })
                    end
            end;
        _Node_Access_Model ->
            case
                is_contact_in_allowed_roster_groups(
                    _Contact = Event#event.entity#entity.id,
                    _Rosters_Groups_Allowed = case
                        Item#item.rosters_groups_allowed
                    of
                        [] ->
                            Event#event.node#node.rosters_groups_allowed;
                        Item_Rosters_Groups_Allowed ->
                            Item_Rosters_Groups_Allowed
                    end
                )
            of
                false ->
                    Event;
                {_Node_Owner, _Roster_Group_Allowed} ->
                    broadcast_item(Item,
                        Event#event{
                            cache = Event#event.cache#cache{rosters_groups = true}
                        })
            end
    end;
%%
broadcast_item(_Item, Event) ->
    Event#event{cache = Event#event.cache#cache{rosters_groups = undefined}}.


%%
-spec(resources_subids/5 ::
(
  Entity                  :: xmpp_jid:usr_bare(),
  Local_Entity            :: boolean(),
  Presence_Based_Delivery :: boolean(),
  Subscriptions           :: _,%exmpp_pubsub:subscriptions() | [],
  Cache                   :: {
    Presence_Cache   :: undefined | [] | mod_pubsub_dev:presence_cache(),
    Resources_SubIds :: undefined | [] | mod_pubsub_dev:resources_subids()
  })
    -> {Presence_Cache   :: undefined | [] | mod_pubsub_dev:presence_cache(),
        Resources_SubIds :: [] | mod_pubsub_dev:resources_subids()}
).

resources_subids(_Entity, _Local_Entity, _Presence_Based_Delivery,
  [] = _Subscriptions, {Presence_Cache, Resources_SubIds}) ->
    {Presence_Cache, Resources_SubIds};
%%
resources_subids(Entity, Local_Entity, Presence_Based_Delivery,
  [{'pending', _SubId, _Resource, _Subscription_Options} | Subscriptions],
  {Presence_Cache, Resources_SubIds}) ->
    resources_subids(Entity, Local_Entity, Presence_Based_Delivery, Subscriptions,
        {Presence_Cache, Resources_SubIds});
%%
resources_subids(Entity, Local_Entity, Presence_Based_Delivery,
  [{Subscription_State, SubId, {caps, Resource}, Subscription_Options}
  | Subscriptions], {Presence_Cache, Resources_SubIds}) ->
    resources_subids(Entity, Local_Entity, Presence_Based_Delivery,
        [{Subscription_State, SubId, Resource, Subscription_Options}
        | Subscriptions],
        {Presence_Cache, Resources_SubIds});
%% Presence_Based_Delivery = false
%% Local_Entity = boolean()
%% Subscription_Options = []
resources_subids(Entity, Local_Entity, false = Presence_Based_Delivery,
  [{_Subscription_State, SubId, Resource, [] = _Subscription_Options}
  | Subscriptions], {Presence_Cache, Resources_SubIds}) ->
    resources_subids(Entity, Local_Entity, Presence_Based_Delivery, Subscriptions,
        {Presence_Cache,
         _Resources_SubIds = resources_subids(Resource, SubId,
             Resources_SubIds)});
%% Presence_Based_Delivery = false
%% Local_Entity = true
%% Resource = undefined | xmpp_jid:resource_jid()
%% Subscription_Options =/= []
resources_subids({U,S,R} = _Entity, true = Local_Entity, false = Presence_Based_Delivery,
  [{_Subscription_State, SubId, Resource, Subscription_Options} | Subscriptions],
  {Presence_Cache, Resources_SubIds}) ->
    resources_subids({U,S,R}, Local_Entity, Presence_Based_Delivery,
       Subscriptions,
        case get_value(Subscription_Options, 'deliver', true) of
            true ->
                case get_value(Subscription_Options, 'show-values') of
                    'none' -> %% 'show-values' NA 
                        {Presence_Cache,
                         _Resources_SubIds = resources_subids(Resource, SubId,
                             Resources_SubIds)};
                    %%
                    Show_Values
                      when   Show_Values == []
                      orelse Presence_Cache == [] ->
                        {Presence_Cache, Resources_SubIds};
                    %%
                    Show_Values
                      when Presence_Cache == undefined ->
                        case get_resources_show(U, S) of
                            %%%%
                            [] ->
                                {_Presence_Cache = [], Resources_SubIds};
                            %%%%
                            Resources_Show ->
                                {_Presence_Cache = Resources_Show,
                                 _Resources_SubIds = lists:foldl(fun
                                    (Resource_To_Deliver, Resources_SubIds_Bis) ->
                                        resources_subids(Resource_To_Deliver,
                                            SubId, Resources_SubIds_Bis)
                                 end,
                                    Resources_SubIds,
                                    _Resources_To_Deliver = resources_to_deliver(
                                        Resource, Show_Values, Resources_Show,
                                            []))}
                        end;
                    %%
                    Show_Values ->
                        {Presence_Cache,
                         _Resources_SubIds = lists:foldl(fun
                            (Resource_To_Deliver, Resources_SubIds_Bis) ->
                                resources_subids(Resource_To_Deliver,
                                    SubId, Resources_SubIds_Bis)
                         end,
                            Resources_SubIds,
                            _Resources_To_Deliver = resources_to_deliver(
                                Resource, Show_Values, Presence_Cache, []))}
                end;
            false ->
                {Presence_Cache, Resources_SubIds}
        end);
%% Presence_Based_Delivery = false
%% Local_Entity = false
%% Subscription_Options =/= []
resources_subids(Entity, false = Local_Entity, false = Presence_Based_Delivery,
  [{_Subscription_State, SubId, Resource, Subscription_Options} | Subscriptions],
  {Presence_Cache, Resources_SubIds}) ->
    resources_subids(Entity, Local_Entity, Presence_Based_Delivery, Subscriptions,
        case get_value(Subscription_Options, 'deliver', true) of
            true ->
                {Presence_Cache,
                 _Resources_SubIds = resources_subids(Resource, SubId,
                     Resources_SubIds)};
            false ->
                {Presence_Cache, Resources_SubIds}
        end);
%% Presence_Based_Delivery = true
%% Local_Entity = true
%% Subscription_Options : NA
%% Presence_Cache = 'none'
resources_subids({U,S,R} = _Entity, true = _Local_Entity,
  true = _Presence_Based_Delivery, Subscriptions,
  {undefined = _Presence_Cache, Resources_SubIds}) ->
    case get_resources_show(U, S) of
        [] ->
            {[], Resources_SubIds};
        _Resources_Show = Presence_Cache ->
            resources_subids({U,S,R}, true, true, Subscriptions,
               {Presence_Cache, Resources_SubIds})
    end;
%% Presence_Based_Delivery = true
%% Local_Entity = true
%% Subscription_Options : NA
%% Presence_Cache = []
resources_subids(_Entity, true = _Local_Entity, true = _Presence_Based_Delivery,
  _Subscriptions, {[] = Presence_Cache, Resources_SubIds}) ->
    {Presence_Cache, Resources_SubIds};
%% Presence_Based_Delivery = true
%% Local_Entity = true
%% Resource = undefined | xmpp_jid:resource_jid()
%% Subscription_Options =/= []
%% Presence_Cache =/= []
resources_subids(Entity, true = _Local_Entity, true = Presence_Based_Delivery,
  [{_Subscription_State, SubId, Resource, Subscription_Options} | Subscriptions],
  {Presence_Cache, Resources_SubIds}) ->
    resources_subids(Entity, true, Presence_Based_Delivery, Subscriptions,
        case
            get_value(Subscription_Options, 'deliver', true) == true
                andalso
            (Resource == undefined
                     orelse lists:keymember(Resource, 1, Presence_Cache))
        of
            true ->
                {Presence_Cache,
                 _Resources_SubIds = case
                     get_value(Subscription_Options, 'show-values')
                 of
                    'none' when Resource == undefined -> %% 'show-values' NA
                        lists:foldl(fun
                            ({Resource_To_Deliver, _Show}, Resources_SubIds_Bis) ->
                                resources_subids(Resource_To_Deliver,
                                    SubId, Resources_SubIds_Bis)
                        end, Resources_SubIds, Presence_Cache);
                    'none' -> %% 'show-values' NA
                        resources_subids(Resource, SubId, Resources_SubIds);
                    [] ->
                        Resources_SubIds;
                    Show_Values ->
                        lists:foldl(fun
                             (Resource_To_Deliver, Resources_SubIds_Bis) ->
                                 resources_subids(Resource_To_Deliver,
                                     SubId, Resources_SubIds_Bis)
                         end,
                             Resources_SubIds,
                                 _Resources_To_Deliver = resources_to_deliver(
                                     Resource, Show_Values, Presence_Cache, []))
                 end};
            false ->
                {Presence_Cache, Resources_SubIds}
        end);
%% Presence_Based_Delivery = true
%% Local_Entity = false
%% Subscription_Options : NA
resources_subids(_Entity, false = _Local_Entity, true = _Presence_Based_Delivery,
  _Subscriptions, {Presence_Cache, Resources_SubIds}) ->
      {Presence_Cache, Resources_SubIds}.

%%
-spec(resources_subids/3 ::
(
  Resource         :: xmpp_jid:resource_jid(),
  SubId            :: exmpp_pubsub:subId(),
  Resources_SubIds :: [] | mod_pubsub_dev:resources_subids())
    -> %%
       Resources_SubIds::mod_pubsub_dev:resources_subids()
).

resources_subids(Resource, SubId, Resources_SubIds) ->
    _Resources_SubIds = case lists:keyfind(Resource, 1, Resources_SubIds) of
        {_Resource, SubIds} ->
            lists:keyreplace(Resource, 1, Resources_SubIds,
                {Resource, [SubId | SubIds]});
        false ->
            [{Resource, [SubId]} | Resources_SubIds]
    end.

%%
-spec(resources_to_deliver/4 ::
(
  Resource       :: undefined | xmpp_jid:resource_jid(),
  Show_Values    :: ['away' | 'chat' | 'dnd' | 'online' | 'xa',...],
  Presence_Cache :: [] | mod_pubsub_dev:presence_cache(),
  Resources_To_Deliver :: [Resource::xmpp_jid:resource_jid()])
    -> %%
       Resources_To_Deliver::[Resource::xmpp_jid:resource_jid()]
).

resources_to_deliver(_Resource, _Show_Values, [] = _Presence_Cache,
  Resources_To_Deliver) ->
    Resources_To_Deliver;
%%
resources_to_deliver(Resource, Show_Values, [Resource_Show | Presence_Cache],
  Resources_To_Deliver) ->
    resources_to_deliver(Resource, Show_Values, Presence_Cache,
        _Resources_To_Deliver = resource_to_deliver(Resource, Show_Values,
            Resource_Show, Resources_To_Deliver)).

%%
-spec(resource_to_deliver/4 ::
(
  Resource      :: undefined | xmpp_jid:resource_jid(),
  Show_Values   :: ['away' | 'chat' | 'dnd' | 'online' | 'xa',...],
  Resource_Show :: mod_pubsub_dev:resource_show(),
  Resources_To_Deliver :: [Resource::xmpp_jid:resource_jid()])
    -> %%
       Resources_To_Deliver::[Resource::xmpp_jid:resource_jid()]
).

resource_to_deliver(_Resource, [] = _Show_Values, _Resource_Show,
  Resources_To_Deliver) ->
    Resources_To_Deliver;
%%
resource_to_deliver(undefined = _Resource, [_Show_Value = Show | Show_Values],
  {Resource, Show}, Resources_To_Deliver) ->
    resource_to_deliver(_Resource = undefined, Show_Values, {Resource, Show},
        [_Resource_To_Deliver = Resource | Resources_To_Deliver]);
%%
resource_to_deliver(Resource, [_Show_Value = Show | Show_Values],
  {Resource, Show}, Resources_To_Deliver) ->
    resource_to_deliver(Resource, Show_Values, {Resource, Show},
        [_Resource_To_Deliver = Resource | Resources_To_Deliver]);
%%
resource_to_deliver(Resource, [_Show_Value | Show_Values], Resource_Show,
  Resources_To_Deliver) ->
    resource_to_deliver(Resource, Show_Values, Resource_Show,
        Resources_To_Deliver).

