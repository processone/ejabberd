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

-module(pubsub_hooks).
-author('karim.gemayel@process-one.net').

-compile(export_all).

-include("pubsub_dev.hrl").

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

-import(pubsub_db_mnesia,
[
  table/2
]).

%% 'node_config#send_last_published_item'
-spec(presence_online/3 ::
(
  From   :: xmpp_jid:entity_full(),
  To     :: xmpp_jid:entity_full(),
  C2SPid :: pid())
    -> 'ok'
).

presence_online(#jid{luser = U, lserver = S, lresource = R} = Jid, Jid, C2SPid) ->
    ?INFO_MSG("PRESENCE ONLINE From ~p ~n, To ~p ~n, C2SPid ~p ~n",
        [Jid, Jid, C2SPid]),
    Pubsub_Features = node_flat_dev:pubsub_features(),
    case lists:member(<<"last-published">>, Pubsub_Features) of
        true ->
            spawn(?MODULE, last_published_items,
                [_Host = <<"localhost">>, {U,S,R}]);
        false ->
            ok
    end;
%%
presence_online(_, _, _) ->
    ok.


%%
-spec(last_published_items/2 ::
(
  Host   :: xmpp_jid:raw_jid_component_bare(),
  Entity :: xmpp_jid:usr_full())
    -> 'ok'
).

last_published_items(Host, {U,S,_R} = Entity) ->
    Table_Pubsub_State = table('pubsub_state', 'dev'),
    Table_Pubsub_Node = table('pubsub_node', 'dev'),
    Table_Pubsub_Last_Item = table('pubsub_last_item', 'dev'),
    lists:foreach(fun
        (State) ->
            last_published_items(Host, Entity, Table_Pubsub_Node,
                Table_Pubsub_Last_Item, State)
    end,
        mnesia:dirty_select(Table_Pubsub_State,
            [{#pubsub_state_dev{
                  id            = {{U,S,undefined}, '$1'},
                  affiliation   = '$2',
                  access        = '$3',
                  subscriptions = '$4',
                  _             = '_'
             },
             [{'=/=', '$2', 'outcast'},
              {'=/=', '$3', 'pending'},
              {'=/=', '$4', []}],
             ['$$']}])).

%%
-spec(last_published_items/5 ::
(
  Host              :: xmpp_jid:raw_jid_component_bare(),
  Entity            :: xmpp_jid:usr_full(),
  Table_Pubsub_Node :: atom(),
  Table_Pubsub_Last_Item :: atom(),
  State                  :: [exmpp_pubsub:nodeIdx()           |
                             'member' | 'owner' | 'publisher' |
                             _                                |
                             exmpp_pubsub:subscriptions(),... ])
    -> 'ok'
).

last_published_items(Host, {_, _, Online_Resource} = Entity,
  Table_Pubsub_Node, Table_Pubsub_Last_Item,
  [NodeIdx, Affiliation, _Access, Subscriptions]) ->
    case mnesia:dirty_index_read(Table_Pubsub_Node, NodeIdx, idx) of
        [#pubsub_node_dev{
             id      = {Pubsub_Host, NodeId},
             owners  = Node_Owners,
             options = Node_Options
         }] ->
            case get_value(Node_Options, 'send_last_published_item') of
                'on_sub_and_presence' ->
                    case mnesia:dirty_read(Table_Pubsub_Last_Item, NodeIdx) of
                        [#pubsub_last_item_dev{
                             id       = ItemId,
                             creation = {DateTime, Publisher},
                             payload  = Payload,
                             options  = Item_Options
                         }] ->
                            spawn(pubsub_broadcast, broadcast_publish_last2,
                                [Host, Pubsub_Host, NodeId, Node_Options,
                                 Node_Owners,
                                 [{ItemId, Item_Options, Payload, Publisher, DateTime}],
                                   Entity, Affiliation,
                                 lists:foldl(fun
                                    ({Subscription_State, SubId, Resource, Subscription_Options},
                                     Acc)
                                      when    Subscription_State =/= 'pending'
                                      andalso (Resource == undefined
                                                 orelse
                                               Resource == Online_Resource) ->
                                        [{Subscription_State, SubId,
                                          Online_Resource, Subscription_Options}
                                        | Acc];
                                    (_Subscription, Acc) ->
                                        Acc
                                    end, [], Subscriptions)]);
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

%% 'node_config#purge_offline'
%% 'node_config#tempsub'
%% 'subscribe_options#expire'

-spec(presence_offline/3 ::
(
  _   :: _,
  Jid :: xmpp_jid:entity_full(),
  _   :: _)
    -> 'ok'
).

presence_offline({_DateTime, _Pid},
  #jid{luser = U, lserver = S, lresource = Offline_Resource} = _Jid, _) ->
    Host = <<"localhost">>,
    Pubsub_Features = node_flat_dev:pubsub_features(),
    Subscribe = lists:member(<<"subscribe">>, Pubsub_Features),
    Leased_Subscription = lists:member(<<"leased-subscription">>, Pubsub_Features),
    Purge_Nodes = lists:member(<<"purge-nodes">>, Pubsub_Features),
    Persistent_Items = lists:member(<<"persistent-items">>, Pubsub_Features),
    Subscription_Notifications = lists:member(<<"subscription-notifications">>,
        Pubsub_Features),
    Table_Pubsub_State = table('pubsub_state', 'dev'),
    Table_Pubsub_Node = table('pubsub_node', 'dev'),
    Online_Resources = case ejabberd_sm:get_user_resources(U, S) of
        [] -> false;
        _  -> true
    end,
    lists:foreach(fun
        (Pubsub_State) ->
            presence_offline(Host,
                _Entity = {U,S,undefined},
                Table_Pubsub_State,
                Pubsub_State,
                Table_Pubsub_Node,
                Offline_Resource,
                Online_Resources,
                {Subscribe,
                 Leased_Subscription,
                 Purge_Nodes,
                 Persistent_Items,
                 Subscription_Notifications})
    end,
        mnesia:dirty_match_object(Table_Pubsub_State,
            #pubsub_state_dev{
                id = {{U,S,undefined}, '_'},
                _  = '_'
            })).

%%
-spec(presence_offline/8 ::
(
  Host               :: xmpp_jid:raw_jid_component_bare(),
  Entity             :: xmpp_jid:usr_bare(),
  Table_Pubsub_State :: atom(),
  Pubsub_State       :: mod_pubsub_dev:pubsub_state(),
  Table_Pubsub_Node  :: atom(),
  Offline_Resource   :: xmpp_jid:resource_jid(),
  Online_Resources   :: boolean(),
  Pubsub_Features    :: {Subscribe                  :: boolean(),
                         Leased_Subscription        :: boolean(),
                         Purge_Nodes                :: boolean(),
                         Persistent_Items           :: boolean(),
                         Subscription_Notifications :: boolean()})
    -> 'ok'
).

presence_offline(Host, Entity, Table_Pubsub_State, Pubsub_State,
  Table_Pubsub_Node, Offline_Resource, Online_Resources,
  {Subscribe, Leased_Subscription, Purge_Nodes, Persistent_Items,
   Subscription_Notifications}) ->
    case
        mnesia:dirty_index_read(Table_Pubsub_Node,
            Pubsub_State#pubsub_state_dev.nodeidx, idx)
    of
        [#pubsub_node_dev{id = {Pubsub_Host, NodeId}} = Pubsub_Node] ->
            Node_Options = Pubsub_Node#pubsub_node_dev.options,

            case
                Subscribe == true
                    andalso
                get_value(Node_Options, 'tempsub') == true
            of
                true ->
                    case
                        filter_tempsub_subscriptions(
                            get_value(Node_Options, 'notify_sub'),
                            Pubsub_State#pubsub_state_dev.subscriptions,
                            Offline_Resource, Online_Resources, {[], []})
                    of
                        %%
                        {_Unexpired_Subscriptions, [] = _Expired_Subscriptions} ->
                            ok;
                        %%
                        {[] = _Unexpired_Subscriptions, Expired_Subscriptions}
                          when    Pubsub_State#pubsub_state_dev.affiliation == 'member'
                          andalso Pubsub_State#pubsub_state_dev.itemids     == [] ->
                            mnesia:dirty_delete_object(Table_Pubsub_State,
                                Pubsub_State),
                            Notification_Type = get_value(Node_Options,
                                'notification_type', 'headline'),
                            notify_subscriptions(Host, Pubsub_Host,
                                Entity, NodeId, Notification_Type,
                                _Recipients = case Subscription_Notifications of
                                    true ->
                                        case
                                            lists:member(Entity,
                                                Pubsub_Node#pubsub_node_dev.owners)
                                        of
                                            true ->
                                                Pubsub_Node#pubsub_node_dev.owners;
                                            false ->
                                                [Entity
                                                | Pubsub_Node#pubsub_node_dev.owners]
                                        end;
                                    false ->
                                        Pubsub_Node#pubsub_node_dev.owners
                                end,
                                Expired_Subscriptions),
                            ok;
                        %%
                        {Unexpired_Subscriptions, Expired_Subscriptions} ->
                            mnesia:dirty_write(Table_Pubsub_State,
                                Pubsub_State#pubsub_state_dev{
                                    subscriptions = Unexpired_Subscriptions
                                }),
                            Notification_Type = get_value(Node_Options,
                                'notification_type', 'headline'),
                                notify_subscriptions(Host, Pubsub_Host,
                                Entity, NodeId, Notification_Type,
                                _Recipients = case Subscription_Notifications of
                                    true ->
                                        case
                                            lists:member(Entity,
                                                Pubsub_Node#pubsub_node_dev.owners)
                                        of
                                            true ->
                                                Pubsub_Node#pubsub_node_dev.owners;
                                            false ->
                                                [Entity
                                                | Pubsub_Node#pubsub_node_dev.owners]
                                        end;
                                    false ->
                                        Pubsub_Node#pubsub_node_dev.owners
                                end,
                                Expired_Subscriptions),
                            ok
                    end;
                _ ->
                    case Subscribe == true andalso Leased_Subscription == true of
                        true ->
                            case
                                filter_expired_subscriptions(
                                    get_value(Node_Options, 'notify_sub'),
                                    Pubsub_State#pubsub_state_dev.subscriptions,
                                    Offline_Resource, Online_Resources, {[], []})
                            of
                        %%
                                {_Unexpired_Subscriptions, [] = _Expired_Subscriptions} ->
                                    ok;
                                %%
                                {[] = _Unexpired_Subscriptions, Expired_Subscriptions}
                                  when Pubsub_State#pubsub_state_dev.affiliation == 'member' ->
                                    mnesia:dirty_delete_object(Table_Pubsub_State,
                                        Pubsub_State),
                                    Notification_Type = get_value(Node_Options,
                                        'notification_type', 'headline'),
                                    notify_subscriptions(Host, Pubsub_Host,
                                        Entity, NodeId, Notification_Type,
                                        _Recipients = case
                                            Subscription_Notifications
                                        of
                                            true ->
                                                case
                                                    lists:member(Entity,
                                                    Pubsub_Node#pubsub_node_dev.owners)
                                                of
                                                    true ->
                                                        Pubsub_Node#pubsub_node_dev.owners;
                                                    false ->
                                                        [Entity
                                                        |Pubsub_Node#pubsub_node_dev.owners]
                                                end;
                                            false ->
                                                Pubsub_Node#pubsub_node_dev.owners
                                        end,
                                        Expired_Subscriptions),
                                    ok;
                                %%
                                {Unexpired_Subscriptions, Expired_Subscriptions} ->
                                    mnesia:dirty_write(Table_Pubsub_State,
                                        Pubsub_State#pubsub_state_dev{
                                            subscriptions = Unexpired_Subscriptions
                                        }),
                                    Notification_Type = get_value(Node_Options,
                                        'notification_type', 'headline'),
                                    notify_subscriptions(Host, Pubsub_Host,
                                        Entity, NodeId, Notification_Type,
                                        _Recipients = case
                                            Subscription_Notifications
                                        of
                                            true ->
                                                case
                                                    lists:member(Entity,
                                                    Pubsub_Node#pubsub_node_dev.owners)
                                                of
                                                    true ->
                                                        Pubsub_Node#pubsub_node_dev.owners;
                                                    false ->
                                                        [Entity
                                                        | Pubsub_Node#pubsub_node_dev.owners]
                                                end;
                                            false ->
                                                Pubsub_Node#pubsub_node_dev.owners
                                        end,
                                        Expired_Subscriptions),
                                    ok
                            end;
                        false ->
                            ok
                    end
            end;
        [] ->
            ok
    end.

%%
-spec(notify_subscriptions/7 ::
(
  Host              :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host       :: exmpp_pubsub:host(),
  Entity            :: xmpp_jid:usr_bare(),
  NodeId            :: exmpp_pubsub:nodeId(),
  Notification_Type :: 'message' | 'headline',
  Recipients        :: [Entity::xmpp_jid:usr_bare(),...],
  Subscriptions     :: [Subscription::exmpp_pubsub:subscription(),...])
    -> 'ok'
).

notify_subscriptions(Host, Pubsub_Host, {U,S,_} = _Entity, NodeId,
  Notification_Type, Recipients, Subscriptions) ->
    Pubsub_Component_Jid = jlib:make_jid(<<>>, Pubsub_Host, <<>>),
    Jids = [pubsub_tools:make_jid(Recipient) || Recipient <- Recipients],
    lists:foreach(fun
        ({_, SubId, Resource, _}) ->
            Subscriber = jlib:jid_to_string({U,S,Resource}),
            lists:foreach(fun
                (Recipient) ->
                    spawn(pubsub_broadcast, notify_subscription,
                        [Host, NodeId, Pubsub_Component_Jid, Recipient,
                         Notification_Type, {Subscriber, 'none', SubId}])
            end, Jids)
    end, Subscriptions).

%%
filter_purged_offline_itemids([Publisher_ItemId | Publisher_ItemIds],
  Table_Pubsub_Item, NodeIdx, {U,S,R} = _Entity,
  {Node_ItemIds, Unpurged_ItemIds, Purged_ItemIds}) ->
    filter_purged_offline_itemids(Publisher_ItemIds, Table_Pubsub_Item, NodeIdx,
        {U,S,R},
        case
            mnesia:dirty_index_match_object(Table_Pubsub_Item,
                #pubsub_item_dev{
                    id      = {Publisher_ItemId, NodeIdx},
                    nodeidx = NodeIdx,
                    _       = '_'
                },
                nodeidx)
        of
            [#pubsub_item_dev{creation = {_DateTime, {U,S,_}}} = Pubsub_Item] ->
                mnesia:dirty_delete_object(Table_Pubsub_Item, Pubsub_Item);
            _ ->
                {Node_ItemIds,
                 [Publisher_ItemId | Unpurged_ItemIds],
                 Purged_ItemIds}
        end).


%%
-spec(filter_tempsub_subscriptions/5 ::
(
  Notify_Sub                     :: boolean() | 'none',
  Subscriptions                  :: [] | exmpp_pubsub:subscriptions(),
  Offline_Resource               :: xmpp_jid:resource_jid(),
  Online_Resources               :: boolean(),
  Filtered_TempSub_Subscriptions :: {
      Unexpired_Subscriptions :: [] | exmpp_pubsub:subscriptions(),
      Expired_Subscriptions   :: [] | exmpp_pubsub:subscriptions()
  })
    -> Filtered_TempSub_Subscriptions :: {
           Unexpired_Subscriptions :: [] | exmpp_pubsub:subscriptions(),
           Expired_Subscriptions   :: [] | exmpp_pubsub:subscriptions()
       }
).

filter_tempsub_subscriptions(_Notify_Sub, [] = _Subscriptions,
  _Offline_Resource, _Online_Resources, Filtered_TempSub_Subscriptions) ->
    Filtered_TempSub_Subscriptions;
%%
filter_tempsub_subscriptions(Notify_Sub,
  [{Subscription_State, SubId, Resource, Subscription_Options} | Subscriptions],
  Offline_Resource, Online_Resources,
  {Unexpired_Subscriptions, Expired_Subscriptions})
  when ((Online_Resources == false
             andalso
        (Resource == undefined orelse Resource == Offline_Resource))
  orelse
        (Online_Resources == true
             andalso
         Resource == Offline_Resource)) ->
    filter_tempsub_subscriptions(Notify_Sub, Subscriptions, Offline_Resource,
        Online_Resources,
        {Unexpired_Subscriptions,
         _Expired_Subscriptions = case Notify_Sub of
             true ->
                [{Subscription_State, SubId, Resource, Subscription_Options}
                | Expired_Subscriptions];
             _ ->
                Expired_Subscriptions
         end});
%%
filter_tempsub_subscriptions(Notify_Sub, [Subscription | Subscriptions],
  Offline_Resource, Online_Resources,
  {Unexpired_Subscriptions, Expired_Subscriptions}) ->
    filter_tempsub_subscriptions(Notify_Sub, Subscriptions, Offline_Resource,
        Online_Resources,
        {[Subscription | Unexpired_Subscriptions], Expired_Subscriptions}).

%%
-spec(filter_expired_subscriptions/5 ::
(
  Notify_Sub                     :: boolean() | 'none',
  Subscriptions                  :: [] | exmpp_pubsub:subscriptions(),
  Offline_Resource               :: xmpp_jid:resource_jid(),
  Online_Resources               :: boolean(),
  Filtered_Expired_Subscriptions :: {
      Unexpired_Subscriptions :: [] | exmpp_pubsub:subscriptions(),
      Expired_Subscriptions   :: [] | exmpp_pubsub:subscriptions()
  })
    -> Filtered_Expired_Subscriptions :: {
           Unexpired_Subscriptions :: [] | exmpp_pubsub:subscriptions(),
           Expired_Subscriptions   :: [] | exmpp_pubsub:subscriptions()
       }
).

filter_expired_subscriptions(_Notify_Sub, [] = _Subscriptions, _Offline_Resource,
  _Online_Resources, Filtered_Expired_Subscriptions) ->
    Filtered_Expired_Subscriptions;
%%
filter_expired_subscriptions(Notify_Sub,
  [{Subscription_State, SubId, Resource, Subscription_Options} | Subscriptions],
  Offline_Resource, Online_Resources,
  {Unexpired_Subscriptions, Expired_Subscriptions})
  when    Subscription_Options =/= []
  andalso ((Online_Resources == false
                andalso
           (Resource == undefined orelse Resource == Offline_Resource))
                        orelse
           (Online_Resources == true
                 andalso
            Resource == Offline_Resource)) ->
    filter_expired_subscriptions(Notify_Sub, Subscriptions, Offline_Resource,
        Online_Resources,
        case get_value(Subscription_Options, 'expire') of
            'presence' ->
                {Unexpired_Subscriptions,
                 _Expired_Subscriptions = case Notify_Sub of
                    true ->
                        [{Subscription_State, SubId, Resource, Subscription_Options}
                        | Expired_Subscriptions];
                    false ->
                        Expired_Subscriptions
                 end};
            _ ->
                {[{Subscription_State, SubId, Resource, Subscription_Options}
                 | Unexpired_Subscriptions],
                 Expired_Subscriptions}
        end);
%%
filter_expired_subscriptions(Notify_Sub, [Subscription | Subscriptions],
  Offline_Resource, Online_Resources,
  {Unexpired_Subscriptions, Expired_Subscriptions}) ->
    filter_expired_subscriptions(Notify_Sub, Subscriptions, Offline_Resource,
        Online_Resources,
        {[Subscription | Unexpired_Subscriptions], Expired_Subscriptions}).


%%
roster_process_item(Roster_Item, Server) ->
    spawn(pubsub_groups, monitor_roster_groups, [Server, Roster_Item]),
    Roster_Item.



roster_in_subscription(Boolean, User, Server, Jid_Contact,
  'subscribed' = _Subscription_Type) ->
    spawn(pubsub_groups, monitor_contacts2,
        ['subscribed', {User, Server, undefined}, Jid_Contact]),
    Boolean;
roster_in_subscription(Boolean, _User, _Server, _JID, _SubscriptionType) ->
    Boolean.
