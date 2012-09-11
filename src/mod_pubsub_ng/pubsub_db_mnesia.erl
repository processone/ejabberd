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
%%% Portions created by ProcessOne are Copyright 2006-2011, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2011, ProcessOne.
%%%
%%% @copyright 2006-2011 ProcessOne
%%% @author Karim Gemayel <karim.gemayel@process-one.net>
%%%   [http:%%www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @headerfile "pubsub_dev.hrl"

-module(pubsub_db_mnesia).
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


-export_type([
  suffix/0,
  table/0
]).

-type(suffix() :: undefined | atom()).
-type(table() :: atom()).


%%
-spec(init/1 ::
(
  Suffix :: undefined | atom())
    -> 'ok'
).

init(Suffix) ->
    create_table_pubsub_node(Suffix),
    create_table_pubsub_state(Suffix),
    create_table_pubsub_item(Suffix),
    create_table_pubsub_last_item(Suffix),
    create_table_pubsub_subscription_pending(Suffix),
    pubsub_index_dev:init(Suffix),
    pubsub_groups:create_table_pubsub_groups(Suffix).

%%
-spec(table/2 ::
(
  Base_Table :: atom(),
  Suffix     :: undefined | atom())
    -> Table :: atom()
).

table(Base_Table, undefined = _Suffix) ->
    _Table = Base_Table;
table(Base_Table, Suffix) ->
    _Table = list_to_atom(atom_to_list(Base_Table) ++ "_" ++ atom_to_list(Suffix)).


%%
create_table_pubsub_node(Suffix) ->
    mnesia:create_table(table('pubsub_node', Suffix),
        [{type, set},
         {disc_copies, [node()]},
         {record_name, pubsub_node_dev},
         {attributes, record_info(fields, pubsub_node_dev)}]),
    mnesia:add_table_index(pubsub_node_dev, idx).

create_table_pubsub_state(Suffix) ->
    mnesia:create_table(table('pubsub_state', Suffix),
        [{type, set},
         {disc_copies, [node()]},
         {record_name, pubsub_state_dev},
         {attributes, record_info(fields, pubsub_state_dev)}]),
    mnesia:add_table_index(pubsub_state_dev, nodeidx).

create_table_pubsub_item(Suffix) ->
    mnesia:create_table(table('pubsub_item', Suffix),
        [{type, set},
         {disc_copies, [node()]},
         {record_name, pubsub_item_dev},
         {attributes, record_info(fields, pubsub_item_dev)}]),
    mnesia:add_table_index(pubsub_item_dev, nodeidx).

create_table_pubsub_last_item(Suffix) ->
    mnesia:create_table(table('pubsub_last_item', Suffix),
        [{type, set},
         {disc_copies, [node()]},
         {record_name, pubsub_last_item_dev},
         {attributes, record_info(fields, pubsub_last_item_dev)}]).

create_table_pubsub_subscription_pending(Suffix) ->
    mnesia:create_table(table('pubsub_subscription_pending', Suffix),
        [{type, set},
         {disc_copies, [node()]},
         {record_name, pubsub_subscription_pending},
         {attributes, record_info(fields, pubsub_subscription_pending)}]),
    mnesia:add_table_index(pubsub_subscription_pending, nodeidx).

%%
transaction(Module, Function, Arguments) ->
    case mnesia:transaction(fun() -> apply(Module, Function, Arguments) end) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            ?INFO_MSG("DB TRANSACTION ERROR ~p", [Reason]),
            {error, 'internal-server-error'}
    end.

%%
-spec(read_item/3 ::
(
  Suffix  :: atom(),
  NodeIdx :: exmpp_pubsub:nodeIdx(),
  ItemId  :: exmpp_pubsub:itemId())
    -> Pubsub_Item :: undefined | mod_pubsub_dev:pubsub_item()
).

read_item(Suffix, NodeIdx, ItemId) ->
    _Pubsub_Item = case
        mnesia:read({_Table = table('pubsub_item', Suffix), {ItemId, NodeIdx}})
    of
        [Pubsub_Item] -> Pubsub_Item;
        []            -> undefined
    end.

%%
-spec(read_node/3 ::
(
  Suffix      :: atom(),
  Pubsub_Host :: exmpp_pubsub:host(),
  NodeId      :: exmpp_pubsub:nodeId())
    -> Pubsub_Node :: undefined | mod_pubsub_dev:pubsub_node()
).

read_node(Suffix, Pubsub_Host, NodeId) ->
    _Pubsub_Node = case
        mnesia:read({_Table = table('pubsub_node', Suffix),
            {Pubsub_Host, NodeId}})
    of
        [Pubsub_Node] -> Pubsub_Node;
        []            -> undefined
    end.

%%
-spec(read_state/3 ::
(
  Suffix  :: atom(),
  Entity  :: xmpp_jid:usr_entity(),
  NodeIdx :: exmpp_pubsub:nodeIdx())
    -> Pubsub_State :: undefined | mod_pubsub_dev:pubsub_state()
).

read_state(Suffix, {U,S,_R} = _Entity, NodeIdx) ->
    _Pubsub_State = case
        mnesia:read({_Table = table('pubsub_state', Suffix),
            {{U,S,undefined}, NodeIdx}})
    of
        [Pubsub_State] -> Pubsub_State;
        []             -> undefined
    end.

%%
-spec(index_read_node/2 ::
(
  Suffix  :: atom(),
  NodeIdx :: exmpp_pubsub:nodeIdx())
    -> Pubsub_Nodes::[Pubsub_Node::mod_pubsub_dev:pubsub_node()]
).

index_read_node(Suffix, NodeIdx) ->
    _Pubsub_Nodes = mnesia:index_read(_Table = table('pubsub_node', Suffix),
        NodeIdx, idx).

%%
-spec(index_read_state/2 ::
(
  Suffix  :: atom(),
  NodeIdx :: exmpp_pubsub:nodeIdx())
    -> Pubsub_States::[Pubsub_State::mod_pubsub_dev:pubsub_state()]
).

index_read_state(Suffix, NodeIdx) ->
    _Pubsub_States = mnesia:index_read(_Table = table('pubsub_state', Suffix),
        NodeIdx, nodeidx).


%-- Create_Node --%
-spec(create_node/7 ::
(
  Suffix          :: atom(),
  Pubsub_Host     :: exmpp_pubsub:host(),
  Entity          :: xmpp_jid:usr_entity(),
  NodeId          :: exmpp_pubsub:nodeId(),
  Level           :: exmpp_pubsub:level(),
  Node_Options    :: pubsub_options:options_node(),
  Pubsub_Features :: exmpp_pubsub:pubsub_features())
    -> Subscription :: undefined | exmpp_pubsub:subscription_subscribed()
).

create_node(Suffix, Pubsub_Host, {U,S,R} = _Entity, NodeId, Level, Node_Options,
  Pubsub_Features) ->
    %% Create Pubsub_Node
    mnesia:write(table('pubsub_node', Suffix),
        #pubsub_node_dev{
            id       = {Pubsub_Host, NodeId},
            idx      = NodeIdx = pubsub_index_dev:new(Suffix, 'node'),
            level    = Level,
            creation = {_DateTime = now(), _Creator = {U,S,R}},
            owners   = [{U,S,undefined}],
            options  = Node_Options
        },
        write),
    _Subscription = case
        lists:member(<<"auto-subscribe">>, Pubsub_Features)
            andalso
        get_value(Node_Options, 'subscribe', false)
    of
        true ->
            mnesia:write(table('pubsub_state', Suffix),
                #pubsub_state_dev{
                    id            = {{U,S,undefined}, NodeIdx},
                    nodeidx       = NodeIdx,
                    affiliation   = 'owner',
                    subscriptions = [Subscription = {
                        _Subscription_State = 'subscribed',
                        _SubId = exmpp_pubsub:subId(),
                        _Resource = undefined,
                        _Subscription_Options = []
                    }]
                },
                write),
            Subscription;
        false ->
            mnesia:write(table('pubsub_state', Suffix),
                #pubsub_state_dev{
                    id          = {{U,S,undefined}, NodeIdx},
                    nodeidx     = NodeIdx,
                    affiliation = 'owner'
                },
                write),
            undefined
    end.

%-- Purge_Node --%
%% TODO : purge Pubsub_Purge_Offline
%% TODO : purge Pubsub_Expiry_Item
-spec(purge_node/2 ::
(
  Suffix      :: atom(),
  Pubsub_Node :: mod_pubsub_dev:pubsub_node())
    -> Recipients :: undefined | [Entity::xmpp_jid:usr_bare()]
).

purge_node(Suffix, Pubsub_Node) ->
    %% Update Pubsub_Node
    mnesia:write(table('pubsub_node', Suffix),
        Pubsub_Node#pubsub_node_dev{itemids = []}, write),
    %% Delete Pubsub_Last_Item
    mnesia:delete(table('pubsub_last_item', Suffix),
        NodeIdx = Pubsub_Node#pubsub_node_dev.idx, write),
    %%
    Table_Pubsub_State = table('pubsub_state', Suffix),
    Table_Pubsub_Item = table('pubsub_item', Suffix),
    _Recipients = case
        get_value(Pubsub_Node#pubsub_node_dev.options, 'notify_retract', false)
    of
        true ->
            %% Delete Pubsub_Items
            lists:foreach(fun
                (Pubsub_Item) ->
                    mnesia:delete_object(Table_Pubsub_Item, Pubsub_Item, write)
                %
            end, _Pubsub_Items = mnesia:index_read(Table_Pubsub_Item, NodeIdx,
                     nodeidx)),
            %% Update Pubsub_States
            _Subscribers = lists:foldl(fun
                %%
                (Pubsub_State, Subscribers)
                  when (Pubsub_State#pubsub_state_dev.affiliation == 'owner'
                            orelse
                        Pubsub_State#pubsub_state_dev.affiliation == 'outcast'
                            orelse
                        Pubsub_State#pubsub_state_dev.affiliation == 'publish-only')
                  andalso Pubsub_State#pubsub_state_dev.itemids =/= [] ->
                    mnesia:write(Table_Pubsub_State,
                        Pubsub_State#pubsub_state_dev{itemids = []},
                        write),
                    Subscribers;
                %%
                (#pubsub_state_dev{id = {Entity, _NodeIdx}} = Pubsub_State,
                 Subscribers)
                  when    Pubsub_State#pubsub_state_dev.affiliation == 'publisher'
                  andalso Pubsub_State#pubsub_state_dev.itemids     =/= [] ->
                    mnesia:write(Table_Pubsub_State,
                        Pubsub_State#pubsub_state_dev{itemids = []}, write),
                    case Pubsub_State#pubsub_state_dev.subscriptions of
                        [] = _Subscriptions ->
                            Subscribers;
                        _Subscriptions ->
                            [_Subscriber = Entity | Subscribers]
                    end;
                %%
                (#pubsub_state_dev{id = {Entity, _NodeIdx}} = Pubsub_State,
                 Subscribers)
                  when    Pubsub_State#pubsub_state_dev.affiliation   == 'publisher'
                  andalso Pubsub_State#pubsub_state_dev.subscriptions =/= [] ->
                    [_Subscriber = Entity | Subscribers];
                %%
                (#pubsub_state_dev{id = {Entity, _NodeIdx}} = Pubsub_State,
                 Subscribers)
                  when    Pubsub_State#pubsub_state_dev.affiliation == 'member'
                  andalso Pubsub_State#pubsub_state_dev.itemids     =/= [] ->
                    mnesia:write(Table_Pubsub_State,
                        Pubsub_State#pubsub_state_dev{itemids = []}, write),
                    case
                        (Pubsub_State#pubsub_state_dev.subscriptions == []
                             orelse
                         has_subscriptions(
                             Pubsub_State#pubsub_state_dev.subscriptions))
                    of
                        true  -> [_Subscriber = Entity | Subscribers];
                        false -> Subscribers
                    end;
                %%
                (#pubsub_state_dev{id = {Entity, _NodeIdx}} = Pubsub_State,
                 Subscribers)
                  when Pubsub_State#pubsub_state_dev.affiliation == 'member'->
                    case
                        (Pubsub_State#pubsub_state_dev.subscriptions == []
                             orelse
                         has_subscriptions(
                             Pubsub_State#pubsub_state_dev.subscriptions))
                    of
                        true  -> [_Subscriber = Entity | Subscribers];
                        false -> Subscribers
                    end;
                %%
                (_Pubsub_State, Subscribers) ->
                    Subscribers
                %
            end,
                [],
                    _Pubsub_States = mnesia:index_read(Table_Pubsub_State,
                        NodeIdx, nodeidx));
        false ->
            %% Update Pubsub_States
            lists:foreach(fun
                (_Items_Owner = Entity) ->
                    case
                        mnesia:read(Table_Pubsub_State, {Entity, NodeIdx},
                            write)
                    of
                        [Pubsub_State]
                          when Pubsub_State#pubsub_state_dev.itemids =/= [] ->
                            mnesia:write(Table_Pubsub_State,
                                Pubsub_State#pubsub_state_dev{itemids = []},
                                    write);
                        _Pubsub_State ->
                            ok
                    end
            end,
                _Items_Owners = lists:foldl(fun
                    %% Delete Pubsub_Items
                    (Pubsub_Item, Items_Owners) ->
                        mnesia:delete_object(Table_Pubsub_Item, Pubsub_Item,
                            write),
                        lists:usort(lists:append(Items_Owners,
                            Pubsub_Item#pubsub_item_dev.owners))
                end, [],
                    _Pubsub_Items = mnesia:index_read(Table_Pubsub_Item, NodeIdx,
                        nodeidx))
            ),
            _Subscribers = undefined
    end.

%-- Delete_Node --%
-spec(delete_node/2 ::
(
  Suffix      :: atom(),
  Pubsub_Node :: mod_pubsub_dev:pubsub_node())
    -> Recipients :: undefined | [Entity::xmpp_jid:usr_bare()]
).

delete_node(Suffix, Pubsub_Node) ->
    %% Delete Pubsub_Node
    mnesia:delete_object(table('pubsub_node', Suffix), Pubsub_Node, write),
    pubsub_index_dev:free(Suffix, 'node',
        NodeIdx = Pubsub_Node#pubsub_node_dev.idx),
    %% Delete Pubsub_Last_Item
    mnesia:delete(table('pubsub_last_item', Suffix), NodeIdx, write),
    %% Delete Pubsub_Items
    Table_Pubsub_Item = table('pubsub_item', Suffix),
    lists:foreach(fun
        (Pubsub_Item) ->
            mnesia:delete_object(Table_Pubsub_Item, Pubsub_Item, write)
        %
    end,
        _Pubsub_Items = mnesia:index_read(Table_Pubsub_Item, NodeIdx,
            nodeidx)),
    %% Delete Pubsub_States
    Table_Pubsub_State = table('pubsub_state', Suffix),
    _Recipients = case
        get_value(Pubsub_Node#pubsub_node_dev.options, 'notify_delete', false)
    of
        true ->
            _Subscribers = lists:foldl(fun
                (#pubsub_state_dev{id = {Entity, _NodeIdx}} = Pubsub_State,
                 Subscribers) ->
                    mnesia:delete_object(Table_Pubsub_State, Pubsub_State,
                        write),
                    case
                        {Pubsub_State#pubsub_state_dev.affiliation,
                         Pubsub_State#pubsub_state_dev.subscriptions}
                    of
                        %%
                        {'publisher' = _Affiliation, Subscriptions}
                          when Subscriptions =/= [] ->
                            [_Subscriber = Entity | Subscribers];
                        %%
                        {'member' = _Affiliation, _Subscriptions} ->
                            case
                                (_Subscriptions == []
                                     orelse
                                 has_subscriptions(_Subscriptions))
                            of
                                true ->
                                    [_Subscriber = Entity | Subscribers];
                                false ->
                                    Subscribers
                            end;
                        %%
                        {_Affiliation, _Subscriptions} ->
                            Subscribers
                    end
                %
            end, [],
                _Pubsub_States = mnesia:index_read(Table_Pubsub_State, NodeIdx,
                    nodeidx));
        false ->
            lists:foreach(fun
                (Pubsub_State) ->
                    mnesia:delete_object(Table_Pubsub_State, Pubsub_State,
                        write)
            end,
                _Pubsub_States = mnesia:index_read(Table_Pubsub_State, NodeIdx,
                    nodeidx)),
            _Subscribers = undefined
    end.

%-- Get_Entity_Subscriptions --%
-spec(get_entity_subscriptions/4 ::
(
  Suffix      :: atom(),
  Pubsub_Host :: exmpp_pubsub:host(),
  Entity      :: xmpp_jid:usr_entity(),
  NodeId      :: undefined | exmpp_pubsub:nodeId())
    -> Entity_Subscriptions :: [{
           NodeId        :: exmpp_pubsub:nodeId(),
           Subscriptions :: exmpp_pubsub:subscriptions()
       }]
).

get_entity_subscriptions(Suffix, Pubsub_Host, {U,S,_R} = _Entity,
  undefined = _NodeId) ->
    Table_Pubsub_Node = table('pubsub_node', Suffix),
    _Entity_Subscriptions = lists:foldl(fun
        ([NodeIdx, Subscriptions], Entity_Subscriptions) ->
            case mnesia:index_read(Table_Pubsub_Node, NodeIdx, idx) of
                %% IMPORTANT : Pubsub_Host must be used in the matching,
                %% else it'd return all NodeIds subscriptions (in case of VHosts)
                %% and not only the ones from to the current Pubsub_Host
                [#pubsub_node_dev{id = {Pubsub_Host, NodeId}}] ->
                    [{NodeId, Subscriptions} | Entity_Subscriptions];
                _Pubsub_Node ->
                    Entity_Subscriptions
            end
    end, [],
        mnesia:select(table('pubsub_state', Suffix),
            [{#pubsub_state_dev{
                  id            = {{U,S,undefined}, '$1'},
                  subscriptions = '$2',
                  _             = '_'},
            [{'=/=', '$2', []}],
            ['$$']}]));
%%
get_entity_subscriptions(Suffix, Pubsub_Host, {U,S,_R} = _Entity, NodeId) ->
    _Entity_Subscriptions = case
        mnesia:read(table('pubsub_node', Suffix), {Pubsub_Host, NodeId}, read)
    of
        [] ->
            [];
        [#pubsub_node_dev{idx = NodeIdx}] ->
            case
                mnesia:read(table('pubsub_state', Suffix),
                    {{U,S,undefined}, NodeIdx}, read)
            of
                [#pubsub_state_dev{subscriptions = Subscriptions}]
                  when Subscriptions =/= [] ->
                    [{NodeId, Subscriptions}];
                _Pubsub_State ->
                    []
            end
    end.

%-- Get_Entity_Affiliations --%
-spec(get_entity_affiliations/4 ::
(
  Suffix      :: atom(),
  Pubsub_Host :: exmpp_pubsub:host(),
  Entity      :: xmpp_jid:usr_bare(),
  NodeId      :: undefined | exmpp_pubsub:nodeId())
    -> Entity_Affiliations :: [Entity_Affiliation :: {
           NodeId      :: exmpp_pubsub:nodeId(),
           Affiliation :: exmpp_pubsub:affiliation()
       }]
).

get_entity_affiliations(Suffix, Pubsub_Host, {U,S,_R} = _Entity,
  undefined = _NodeId) ->
    Table_Pubsub_Node = table('pubsub_node', Suffix),
    _Entity_Affiliations = lists:foldl(fun
        ([NodeIdx, Affiliation], Entity_Affiliations) ->
            case mnesia:index_read(Table_Pubsub_Node, NodeIdx, idx) of
                %% IMPORTANT : Pubsub_Host must be used in the matching,
                %% else it'd return all NodeIds affiliations (in case of VHosts)
                %% and not only the ones from to the current Pubsub_Host
                [#pubsub_node_dev{id = {Pubsub_Host, NodeId}}] ->
                    [{NodeId, Affiliation} | Entity_Affiliations];
                _Pubsub_Node ->
                    Entity_Affiliations
            end
    end, [],
        mnesia:select(table('pubsub_state', Suffix),
            [{#pubsub_state_dev{
                  id          = {{U,S,undefined}, '$1'},
                  affiliation = '$2',
                  _           = '_'},
             [],
             ['$$']}],
             read)
        );
%%
get_entity_affiliations(Suffix, Pubsub_Host, {U,S,_R} = _Entity, NodeId) ->
    _Entity_Affiliations = case
        mnesia:read(table('pubsub_node', Suffix), {Pubsub_Host, NodeId}, read)
    of
        [] ->
            [];
        [#pubsub_node_dev{idx = NodeIdx}] ->
            case
                mnesia:read(table('pubsub_state', Suffix),
                    {{U,S,undefined}, NodeIdx})
            of
                [#pubsub_state_dev{affiliation = Affiliation}] ->
                    [{NodeId, Affiliation}];
                _Pubsub_State ->
                    []
            end
    end.

%-- Publish_Item --%
-spec(publish_item1/9 ::
(
  Suffix          :: atom(),
  Pubsub_Host     :: exmpp_pubsub:host(),
  Entity          :: xmpp_jid:usr_entity(),
  NodeId          :: undefined | exmpp_pubsub:nodeId(),
  Level           :: exmpp_pubsub:level(),
  ItemId          :: undefined | exmpp_pubsub:itemId(),
  Payload         :: [Xmlel::#xmlel{}] | exmpp_pubsub:payload(),
  Options         :: {Node_Options :: pubsub_options:options_node(),
                      Item_Options :: [] | pubsub_options:options_item()},
  Pubsub_Features :: exmpp_pubsub:pubsub_features())
    -> {ItemId             :: exmpp_pubsub:itemId(),
        Pubsub_State@Owner :: mod_pubsub_dev:pubsub_state_owner()}
).

publish_item1(Suffix, Pubsub_Host, Entity, NodeId, Level, ItemId,
  [Payload], Options, Pubsub_Features) ->
    publish_item1(Suffix, Pubsub_Host, Entity, NodeId, Level, ItemId, Payload,
        Options, Pubsub_Features);

publish_item1(Suffix, Pubsub_Host, Entity, NodeId, Level, undefined = _ItemId,
  Payload, Options, Pubsub_Features) ->
    publish_item1(Suffix, Pubsub_Host, Entity, NodeId, Level,
        exmpp_pubsub:itemId(), Payload, Options, Pubsub_Features);
%%
publish_item1(Suffix, Pubsub_Host, {U,S,R} = _Entity, NodeId, Level, ItemId,
  Payload, {Node_Options, Item_Options} = Options, Pubsub_Features) ->
    NodeIdx = pubsub_index_dev:new(Suffix, 'node'),
    DateTime = now(),
    Write_Item = get_value(Options, 'persist_items', false) andalso
        (get_value(Node_Options, 'max_items', 0) > 0),
    %% Create Pubsub_Node
    mnesia:write(table('pubsub_node', Suffix),
        #pubsub_node_dev{
            id       = {Pubsub_Host, NodeId},
            idx      = NodeIdx,
            creation = {DateTime, _Creator = {U,S,R}},
            level    = Level,
            owners   = [{U,S,undefined}],
            itemids  = case Write_Item of
                true  -> [ItemId];
                false -> []
            end,
            options  = Node_Options
        },
        write),
    %% Create Pubsub_Item
    case Write_Item of
        false ->
            ok;
        true ->
            mnesia:write(table('pubsub_item', Suffix),
                #pubsub_item_dev{
                    id       = {ItemId, NodeIdx},
                    nodeidx  = NodeIdx,
                    owners   = [{U,S,undefined}],
                    creation = {DateTime, _Creator = {U,S,R}},
                    payload  = Payload,
                    options  = Item_Options
                },
                write),
            %%TODO : Register pubsub#item_expire
            case get_value(Options, 'item_expire', undefined) of
                undefined ->
                    ko;
                Delay ->
                    ok
            end
    end,
    %% Create Pubsub_Last_Item
    case get_value(Options, 'send_last_published_item', 'never') of
        'never' ->
            ok;
        _Send_Last_Published_Item ->
            mnesia:write(table('pubsub_last_item', Suffix),
                #pubsub_last_item_dev{
                    nodeidx  = NodeIdx,
                    id       = ItemId,
                    owners   = [{U,S,undefined}],
                    creation = {DateTime, _Creator = {U,S,R}},
                    payload  = Payload,
                    options  = Item_Options
                },
                write)
    end,
    %% Create Pubsub_State
    mnesia:write(table('pubsub_state', Suffix),
        Pubsub_State = #pubsub_state_dev{
            id            = {{U,S,undefined}, NodeIdx},
            nodeidx       = NodeIdx,
            itemids       = case Write_Item of
                true  -> [ItemId];
                false -> []
            end,
            affiliation   = 'owner',
            subscriptions = case
                lists:member(<<"auto-subscribe">>, Pubsub_Features)
                    andalso
                get_value(Node_Options, 'subscribe', false)
            of
                true  ->
                    [{_Subscription_State = 'subscribed',
                      _SubId = exmpp_pubsub:subId(),
                      _Resource = undefined,
                      _Subscription_Options = []}];
                false ->
                    []
            end
        },
        write),
    {ItemId, Pubsub_State}.

%%
-spec(publish_item2/9 ::
(
  Suffix          :: atom(),
  Pubsub_Host     :: exmpp_pubsub:host(),
  Entity          :: xmpp_jid:usr_entity(),
  Pubsub_State    :: undefined | mod_pubsub_dev:pubsub_state(),
  Pubsub_Node     :: mod_pubsub_dev:pubsub_node(),
  _               :: undefined | exmpp_pubsub:itemId() | mod_pubsub_dev:pubsub_item(),
  Payload         :: [Xmlel::#xmlel{}] | exmpp_pubsub:payload(),
  Options         :: {Node_Options :: pubsub_options:options_node(),
                      Item_Options :: [] | pubsub_options:options_item()},
  Pubsub_Features :: exmpp_pubsub:pubsub_features())
    -> {ItemId          :: exmpp_pubsub:itemId(),
        Retracted_Items :: [Retracted_Item::{
            ItemId       :: exmpp_pubsub:itemId(),
            Item_Options :: [] | pubsub_options:options_node()
        }],
        Pubsub_States   :: [Pubsub_State::mod_pubsub_dev:pubsub_state(),...]}
).


publish_item2(Suffix, Pubsub_Host, Entity, Pubsub_State, Pubsub_Node, Pubsub_Item,
  [Payload], Item_Options, Pubsub_Features) ->
    publish_item2(Suffix, Pubsub_Host, Entity, Pubsub_State, Pubsub_Node,
        Pubsub_Item, Payload, Item_Options, Pubsub_Features);
%%
publish_item2(Suffix, Pubsub_Host, {U,S,R} = _Entity, Pubsub_State, Pubsub_Node,
  #pubsub_item_dev{id = {ItemId, NodeIdx}} = Pubsub_Item, Payload, Item_Options,
  Pubsub_Features) ->
    Node_Options = Pubsub_Node#pubsub_node_dev.options,
    DateTime = now(),
    Table_Pubsub_State = table('pubsub_state', Suffix),
    _Retracted_Items = case
        {get_value(Node_Options, 'persist_items', false),
         get_value(Node_Options, 'max_items', 0)}
    of
        %%
        {Persist_Items, Max_Items}
          when   Persist_Items == false
          orelse Max_Items     == 0 ->
            case Pubsub_State of
                undefined ->
                    mnesia:write(table('pubsub_state', Suffix),
                        #pubsub_state_dev{
                            id      = {{U,S,undefined}, NodeIdx},
                            nodeidx = NodeIdx
                        },
                        write),
                    [];
                _Pubsub_State ->
                    []
            end;
        %%
        {true, undefined} ->
            mnesia:write(table('pubsub_node', Suffix),
                Pubsub_Node#pubsub_node_dev{
                    itemids = lists:reverse([ItemId
                        | lists:reverse(lists:delete(ItemId,
                                Pubsub_Node#pubsub_node_dev.itemids))])
                },
                write),
            %%
            mnesia:write(Table_Pubsub_State,
                case Pubsub_State of
                    undefined ->
                        #pubsub_state_dev{
                            id      = {{U,S,undefined}, NodeIdx},
                            nodeidx = NodeIdx,
                            itemids = [ItemId]
                        };
                    _ ->
                        Pubsub_State#pubsub_state_dev{
                            itemids = [ItemId | lists:delete(ItemId,
                                Pubsub_State#pubsub_state_dev.itemids)]
                        }
                end,
                write),
            mnesia:write(table('pubsub_item', Suffix),
                Pubsub_Item#pubsub_item_dev{
                    modification = {DateTime, {U,S,R}},
                    payload      = Payload,
                    options      = Item_Options,
                    owners       = [{U,S,undefined} |
                        lists:delete({U,S,undefined},
                            Pubsub_Item#pubsub_item_dev.owners)]
                },
                write),
            [];
        {true, Max_Items} ->
            {New_ItemIds, Old_ItemIds} = max_items(Max_Items,
                lists:reverse([ItemId
                    | lists:reverse(lists:delete(ItemId,
                          Pubsub_Node#pubsub_node_dev.itemids))])),
            {Retracted_Items, Publisher_Retracted_ItemIds} =
                delete_old_items(Suffix,
                    _Publisher = {U,S,undefined},
                    NodeIdx,
                    _Notify_Retract = get_value(Node_Options,
                        'notify_retract', false),
                    Old_ItemIds, {[], {[], []}}),
            %%
            mnesia:write(table('pubsub_node', Suffix),
                Pubsub_Node#pubsub_node_dev{
                    itemids = New_ItemIds
                },
                write),
            mnesia:write(table('pubsub_item', Suffix),
                Pubsub_Item#pubsub_item_dev{
                    modification = {DateTime, {U,S,R}},
                    payload      = Payload,
                    options      = Item_Options,
                    owners       = [{U,S,undefined} |
                        lists:delete({U,S,undefined},
                            Pubsub_Item#pubsub_item_dev.owners)]
                },
                write),
            mnesia:write(Table_Pubsub_State,
                case Pubsub_State of
                    undefined ->
                        #pubsub_state_dev{
                            id      = {{U,S,undefined}, NodeIdx},
                            nodeidx = NodeIdx,
                            itemids = [ItemId]
                        };
                    _ ->
                        Pubsub_State#pubsub_state_dev{
                            itemids = [ItemId
                                | lists:foldl(fun
                                        (Publisher_Retracted_ItemId,
                                            Publisher_ItemIds) ->
                                                lists:delete(
                                                    Publisher_Retracted_ItemId,
                                                    Publisher_ItemIds)
                                    end,
                                        lists:delete(ItemId,
                                            Pubsub_State#pubsub_state_dev.itemids),
                                                Publisher_Retracted_ItemIds)]
                        }
                end,
                write),
                Retracted_Items

    end,
    case get_value(Node_Options, 'send_last_published_item', 'never') of
        'never' ->
            ok;
        _Send_Last_Published_Item ->
            mnesia:write(table('pubsub_last_item', Suffix),
                #pubsub_last_item_dev{
                    nodeidx  = NodeIdx,
                    id       = ItemId,
                    owners   = [{U,S,undefined}],
                    creation = {DateTime, _Creator = {U,S,R}},
                    payload  = Payload,
                    options  = Item_Options
                },
                write)
    end,
    {ItemId, _Retracted_Items,
     _Pubsub_States = mnesia:index_read(Table_Pubsub_State, NodeIdx, nodeidx)};
%%
publish_item2(Suffix, Pubsub_Host, {U,S,R} = Entity, Pubsub_State, Pubsub_Node,
  _ItemId, Payload, Item_Options, Pubsub_Features) ->
    NodeIdx = Pubsub_Node#pubsub_node_dev.idx,
    Node_Options = Pubsub_Node#pubsub_node_dev.options,
    ItemId = case _ItemId of
        undefined -> exmpp_pubsub:itemId();
        _         -> _ItemId
    end,
    DateTime = now(),
    Table_Pubsub_State = table('pubsub_state', Suffix),
    _Retracted_Items = case
        {get_value(Node_Options, 'persist_items', false),
         get_value(Node_Options, 'max_items', 0)}
    of
        %%
        {Persist_Items, Max_Items}
          when   Persist_Items == false
          orelse Max_Items     == 0 ->
            case Pubsub_State of
                undefined ->
                    mnesia:write(Table_Pubsub_State,
                        #pubsub_state_dev{
                            id      = {{U,S,undefined}, NodeIdx},
                            nodeidx = NodeIdx
                        },
                        write),
                    [];
                _Pubsub_State ->
                    []
            end;
        %%
        {true, undefined} ->
            mnesia:write(table('pubsub_node', Suffix),
                Pubsub_Node#pubsub_node_dev{
                    itemids = lists:reverse([ItemId
                        | lists:reverse(ItemId,
                              Pubsub_Node#pubsub_node_dev.itemids)])
                },
                write),
            %%
            mnesia:write(Table_Pubsub_State,
                case Pubsub_State of
                    undefined ->
                        #pubsub_state_dev{
                            id      = {{U,S,undefined}, NodeIdx},
                            nodeidx = NodeIdx,
                            itemids = [ItemId]
                        };
                    _ ->
                        Pubsub_State#pubsub_state_dev{
                            itemids = [ItemId |
                                Pubsub_State#pubsub_state_dev.itemids]
                        }
                end,
                write),
                mnesia:write(table('pubsub_item', Suffix),
                    #pubsub_item_dev{
                        id       = {ItemId, NodeIdx},
                        nodeidx  = NodeIdx,
                        owners   = [{U,S,undefined}],
                        creation = {DateTime, {U,S,R}},
                        payload  = Payload,
                        options  = Item_Options
                    },
                    write),
                [];
            {true, Max_Items} ->
                {New_ItemIds, Old_ItemIds} = max_items(Max_Items,
                    lists:reverse([ItemId
                        | lists:reverse(Pubsub_Node#pubsub_node_dev.itemids)])),
                {Retracted_Items, Publisher_Retracted_ItemIds} =
                    delete_old_items(Suffix,
                        _Publisher = {U,S,undefined},
                        NodeIdx,
                        _Notify_Retract = get_value(Node_Options,
                            'notify_retract', false),
                        Old_ItemIds, {[], {[], []}}),
                %%
                mnesia:write(table('pubsub_node', Suffix),
                    Pubsub_Node#pubsub_node_dev{
                        itemids = New_ItemIds
                    },
                    write),
                mnesia:write(table('pubsub_item', Suffix),
                    #pubsub_item_dev{
                        id       = {ItemId, NodeIdx},
                        nodeidx  = NodeIdx,
                        owners   = [{U,S,undefined}],
                        creation = {DateTime, {U,S,R}},
                        payload  = Payload,
                        options  = Item_Options
                    },
                    write),
            %%
            mnesia:write(Table_Pubsub_State,
                case Pubsub_State of
                    undefined ->
                        #pubsub_state_dev{
                            id      = {{U,S,undefined}, NodeIdx},
                            nodeidx = NodeIdx,
                            itemids = [ItemId]
                        };
                    _ ->
                        Pubsub_State#pubsub_state_dev{
                            itemids = [ItemId
                                | lists:foldl(fun
                                        (Publisher_Retracted_ItemId,
                                         Publisher_ItemIds) ->
                                             lists:delete(
                                                 Publisher_Retracted_ItemId,
                                                 Publisher_ItemIds)
                                  end,
                                      Pubsub_State#pubsub_state_dev.itemids,
                                          Publisher_Retracted_ItemIds)]
                        }
                end,
                write),
                Retracted_Items

    end,
    case get_value(Node_Options, 'send_last_published_item', 'never') of
        'never' ->
            ok;
        _Send_Last_Published_Item ->
            mnesia:write(table('pubsub_last_item', Suffix),
                #pubsub_last_item_dev{
                    nodeidx  = NodeIdx,
                    id       = ItemId,
                    owners   = [{U,S,undefined}],
                    creation = {DateTime, _Creator = {U,S,R}},
                    payload  = Payload,
                    options  = Item_Options
                },
                write)
    end,
    {ItemId, _Retracted_Items,
     _Pubsub_States = mnesia:index_read(Table_Pubsub_State, NodeIdx, nodeidx)}.

%%
-spec(max_items/2 ::
(
  Max_Items   :: undefined | non_neg_integer(),
  New_ItemIds :: [ItemId::exmpp_pubsub:itemId()])
    -> ItemIds :: {New_ItemIds::[ItemId::exmpp_pubsub:itemId()],
                   Old_ItemIds::[ItemId::exmpp_pubsub:itemId()]}
).

max_items(undefined = _Max_Items, New_ItemIds) ->
    _ItemIds = {New_ItemIds, _Old_ItemIds = []};
max_items(0 = _Max_Items, New_ItemIds) ->
    _ItemIds = {_New_ItemIds = [], _Old_ItemIds = New_ItemIds};
max_items(Max_Items, New_ItemIds) ->
    _ItemIds = case Max_Items >= (Length_Items = length(New_ItemIds)) of
        true ->
            {New_ItemIds, _Old_ItemIds = []};
        false ->
            diff_items((Length_Items - Max_Items), _Count = 0, {New_ItemIds, []})
    end.

%%
-spec(diff_items/3 ::
(
  Diff_Items :: pos_integer(),
  Count      :: non_neg_integer(),
  ItemIds    :: {New_ItemIds::[ItemId::exmpp_pubsub:itemId()],
                 Old_ItemIds::[ItemId::exmpp_pubsub:itemId()]})
    -> ItemIds :: {New_ItemIds::[ItemId::exmpp_pubsub:itemId()],
                   Old_ItemIds::[ItemId::exmpp_pubsub:itemId()]}
).

diff_items(Diff_Items, _Count = Diff_Items, ItemIds) ->
    ItemIds;
diff_items(Diff_Items, Count, {[ItemId | New_ItemIds], Old_ItemIds}) ->
    diff_items(Diff_Items, Count + 1, {New_ItemIds, [ItemId | Old_ItemIds]}).


%%
-spec(delete_old_items/6 ::
(
  Suffix              :: atom(),
  Publisher           :: xmpp_jid:usr_bare(),
  NodeIdx             :: exmpp_pubsub:nodeIdx(),
  Node_Notify_Retract :: boolean(),
  ItemIds             :: [ItemId::exmpp_pubsub:itemId()],
  %%
  {Retracted_Items :: [Retracted_Item::{
     ItemId       :: exmpp_pubsub:itemId(),
     Item_Options :: [] | pubsub_options:options_item()
   }],
   %
   Publishers_Retracted_ItemIds :: {
       %%
       Entities_Retracted_ItemIds  :: [Entity_Retracted_ItemIds::{
           Entity  :: xmpp_jid:usr_bare(),
           ItemIds :: [ItemId::exmpp_pubsub:itemId()]
       }],
       %%
       Publisher_Retracted_ItemIds :: [ItemId::exmpp_pubsub:itemId()]
   }
  })
    -> {Retracted_Items :: [Retracted_Item::{
          ItemId       :: exmpp_pubsub:itemId(),
          Item_Options :: [] | pubsub_options:options_item()
        }],
        Publisher_Retracted_ItemIds :: [ItemId::exmpp_pubsub:itemId()]}
).

delete_old_items(Suffix, _Publisher, NodeIdx, _Notify_Retract, [] = _ItemIds,
  {Retracted_Items, {Entities_ItemIds, Publisher_ItemIds}}) ->
    Table_Pubsub_State = table('pubsub_state', Suffix),
    lists:foreach(fun
        ({Entity, Entity_ItemIds}) ->
            case mnesia:read(Table_Pubsub_State, {Entity, NodeIdx}, write) of
                [Pubsub_State] ->
                    mnesia:write(Table_Pubsub_State,
                        Pubsub_State#pubsub_state_dev{
                            itemids = lists:foldl(fun
                                (Retracted_ItemId, ItemIds) ->
                                    lists:delete(Retracted_ItemId, ItemIds)
                          end,
                              Pubsub_State#pubsub_state_dev.itemids,
                                    Entity_ItemIds)
                        },
                        write);
                     [] -> %% shouldn't happen
                        ok
            end
            %
    end, Entities_ItemIds),
    {Retracted_Items, Publisher_ItemIds};
%%
delete_old_items(Suffix, Publisher, NodeIdx, Notify_Retract, [ItemId | ItemIds],
  {Retracted_Items, Publishers_ItemIds}) ->
    Table_Pubsub_Item = table('pubsub_item', Suffix),
    delete_old_items(Suffix, Publisher, NodeIdx, Notify_Retract, ItemIds,
        case mnesia:read(Table_Pubsub_Item, {ItemId, NodeIdx}, write) of
            [Pubsub_Item] ->
                mnesia:delete_object(Table_Pubsub_Item, Pubsub_Item, write),
                %% @TODO: Unregister 'pubsub#item_expire'
                {_Retracted_Items = case
                     get_value(Pubsub_Item#pubsub_item_dev.options,
                         'notify_retract', Notify_Retract)
                 of
                    true ->
                        [{ItemId, Pubsub_Item#pubsub_item_dev.options}
                        | Retracted_Items];
                    false ->
                        Retracted_Items
                 end,
                 %%
                 _Publishers_ItemIds = lists:foldl(fun
                     %%
                     (Item_Owner, {Entities_ItemIds, Publisher_ItemIds})
                       when Item_Owner == Publisher ->
                         {Entities_ItemIds,
                          [ItemId | Publisher_ItemIds]};
                     %%
                     (_Item_Owner = Entity,
                      {Entities_ItemIds, Publisher_ItemIds}) ->
                         {_Entities_ItemIds = case
                              lists:keyfind(Entity, 1, Entities_ItemIds)
                          of
                              {_Entity, Entity_ItemIds} ->
                                  lists:keyreplace(Entity, 1, Entities_ItemIds,
                                      {Entity, [ItemId | Entity_ItemIds]});
                            false ->
                                [{Entity, [ItemId]} | Entities_ItemIds]
                          end,
                          Publisher_ItemIds}
                 end, Publishers_ItemIds, Pubsub_Item#pubsub_item_dev.owners)};
            [] ->
                {Retracted_Items, Publishers_ItemIds}
        end).


%-- Retract_Item --%
-spec(retract_item/5 ::
(
  Suffix               :: atom(),
  Pubsub_Node          :: mod_pubsub_dev:pubsub_node(),
  Pubsub_State         :: mod_pubsub_dev:pubsub_state_owner()
                        | mod_pubsub_dev:pubsub_state_member()
                        | mod_pubsub_dev:pubsub_state_publish_only()
                        | mod_pubsub_dev:pubsub_state_publisher()
                        | mod_pubsub_dev:pubsub_state_none(),
  ItemId               :: exmpp_pubsub:itemId(),
  Adhoc_Notify_Retract :: undefined | boolean())
    -> {ok, undefined}
     %
     | {ok,
        Item_Options  :: [] | pubsub_options:options_item(),
        Pubsub_States :: mod_pubsub_dev:pubsub_states()}
    %%%
     | {error, 'forbidden'}
     | {error, 'item-not-found'}
).

retract_item(Suffix, Pubsub_Node,
  #pubsub_state_dev{id = {Entity, NodeIdx}} = Pubsub_State, ItemId,
  Adhoc_Notify_Retract) ->
    Table_Pubsub_Item = table('pubsub_item', Suffix),
    case mnesia:read(Table_Pubsub_Item, {ItemId, NodeIdx}, write) of
        [Pubsub_Item]
          when   Pubsub_State#pubsub_state_dev.affiliation == 'owner'
          orelse Pubsub_State#pubsub_state_dev.affiliation == 'publisher' ->
            mnesia:delete_object(Table_Pubsub_Item, Pubsub_Item, write),
            Table_Pubsub_Last_Item = table('pubsub_last_item', Suffix),
            case mnesia:read(Table_Pubsub_Last_Item, NodeIdx, read) of
                [Pubsub_Last_Item]
                  when Pubsub_Last_Item#pubsub_last_item_dev.id == ItemId ->
                    mnesia:delete_object(Table_Pubsub_Last_Item,
                        Pubsub_Last_Item, write);
                _ ->
                    ok
            end,
            Table_Pubsub_State = table('pubsub_state', Suffix),
            case
                retract_item(NodeIdx, ItemId, Table_Pubsub_State, Entity,
                    _Item_Owners = Pubsub_Item#pubsub_item_dev.owners, false)
            of
                true ->
                    mnesia:write(Table_Pubsub_State,
                        Pubsub_State#pubsub_state_dev{
                            itemids = lists:delete(ItemId,
                                Pubsub_State#pubsub_state_dev.itemids)
                        },
                        write);
                false ->
                    ok
            end,
            mnesia:write(table('pubsub_node', Suffix),
                Pubsub_Node#pubsub_node_dev{
                    itemids = lists:delete(ItemId,
                        Pubsub_Node#pubsub_node_dev.itemids)
                },
                write),
            case
                Adhoc_Notify_Retract == true
                    orelse
                get_value(
                    {Pubsub_Node#pubsub_node_dev.options,
                     Pubsub_Item#pubsub_item_dev.options},
                     'notify_retract', false)
            of
                true ->
                    {ok,
                     _Item_Options = Pubsub_Item#pubsub_item_dev.options,
                     _Pubsub_States = mnesia:index_read(Table_Pubsub_State, NodeIdx, nodeidx)};
                false ->
                    {ok, undefined}
            end;
        %%
        [#pubsub_item_dev{creation = {_DateTime, _Creator = Entity}} = Pubsub_Item] ->
            mnesia:delete_object(Table_Pubsub_Item, Pubsub_Item, write),
            Table_Pubsub_Last_Item = table('pubsub_last_item', Suffix),
            case mnesia:read(Table_Pubsub_Last_Item, NodeIdx, read) of
                [Pubsub_Last_Item]
                  when Pubsub_Last_Item#pubsub_last_item_dev.id == ItemId ->
                    mnesia:delete_object(Table_Pubsub_Last_Item,
                        Pubsub_Last_Item, write);
                _ ->
                    ok
            end,
            Table_Pubsub_State = table('pubsub_state', Suffix),
            case
                retract_item(NodeIdx, ItemId, Table_Pubsub_State, Entity,
                    _Item_Owners = Pubsub_Item#pubsub_item_dev.owners, false)
            of
                true ->
                    mnesia:write(Table_Pubsub_State,
                        Pubsub_State#pubsub_state_dev{
                            itemids = lists:delete(ItemId,
                                Pubsub_State#pubsub_state_dev.itemids)
                        },
                        write);
                false ->
                    ok
            end,
            mnesia:write(table('pubsub_node', Suffix),
                Pubsub_Node#pubsub_node_dev{
                    itemids = lists:delete(ItemId,
                        Pubsub_Node#pubsub_node_dev.itemids)
                },
                write),
            case
                Adhoc_Notify_Retract == true
                    orelse
                get_value(
                    {Pubsub_Node#pubsub_node_dev.options,
                     Pubsub_Item#pubsub_item_dev.options},
                     'notify_retract', false)
            of
                true ->
                    {ok,
                     _Item_Options = Pubsub_Item#pubsub_item_dev.options,
                     _Pubsub_States = mnesia:index_read(Table_Pubsub_State, NodeIdx, nodeidx)};
                false ->
                    {ok, undefined}
            end;
        [_Pubsub_Item] ->
            {error, 'forbidden'};
        %%
        [] ->
            {error, 'item-not-found'}
    end.


%%
-spec(retract_item/6 ::
(
  NodeIdx            :: exmpp_pubsub:nodeIdx(),
  ItemId             :: exmpp_pubsub:itemId(),
  Table_Pubsub_State :: atom(),
  Entity             :: xmpp_jid:usr_bare() | undefined,
  Item_Owners        :: [Entity::xmpp_jid:usr_bare(),...],
  Retract_ItemId     :: boolean())
    -> Retract_ItemId :: boolean()
).

retract_item(_NodeIdx, _ItemId, _Table_Pubsub_State, _Entity, [] = _Item_Owners,
  Retract_ItemId) ->
    Retract_ItemId;
%%
retract_item(NodeIdx, ItemId, Table_Pubsub_State, Entity,
  [_Item_Owner = Entity | Item_Owners], _Retract_ItemId) ->
    retract_item(NodeIdx, ItemId, Table_Pubsub_State, undefined, Item_Owners,
        true);
%%
retract_item(NodeIdx, ItemId, Table_Pubsub_State, _Entity,
  [_Item_Owner = Entity | Item_Owners], Retract_ItemId) ->
    case mnesia:read(Table_Pubsub_State, {Entity, NodeIdx}, write) of
        [Pubsub_State] ->
            mnesia:write(Table_Pubsub_State,
                Pubsub_State#pubsub_state_dev{
                    itemids = lists:delete(ItemId,
                        Pubsub_State#pubsub_state_dev.itemids)
                },
                write);
        [] ->
            ok
    end,
    retract_item(NodeIdx, ItemId, Table_Pubsub_State, _Entity, Item_Owners,
        Retract_ItemId).

%-- Subscribe_Node --%
-spec(subscribe_node/8 ::
(
  Suffix               :: atom(),
  Host                 :: xmpp_jid:raw_jid_component_bare(),
  Node_Options         :: pubsub_options:options_node(),
  Node_Owners          :: [Entity::xmpp_jid:usr_bare(),...],
  Pubsub_State         :: mod_pubsub_dev:pubsub_state(),
  Resource             :: undefined
                        | xmpp_jid:resource_jid()
                        | {'caps', xmpp_jid:resource_jid()},
  Subscription_Options :: [] | pubsub_options:options_subscription(),
  Pubsub_Features      :: exmpp_pubsub:pubsub_features())
    -> {result,
        Subscription_Subscribed :: exmpp_pubsub:subscription_subscribed(),
        Pubsub_Last_Item        :: undefined | mod_pubsub_dev:pubsub_last_item()}
     %
     | {result,
        Subscription_Pending :: exmpp_pubsub:subscription_pending(),
        Pubsub_Last_Item     :: undefined}
    %%%
     | {error, 'forbidden'}
     | {error, 'not-authorized', 'presence-subscription-required'}
     | {error, 'not-authorized', 'not-in-roster-group'}
     | {error, 'not-authorized', 'pending-subscription'}
     | {error, 'not-allowed',    'closed-node'}
).

subscribe_node(Suffix, Host, Node_Options, Node_Owners,
  #pubsub_state_dev{id = {{_U,S,_R} = Entity, _NodeIdx}} = Pubsub_State, Resource,
  Subscription_Options, Pubsub_Features) ->
    Affiliation = Pubsub_State#pubsub_state_dev.affiliation,
    Subscriptions = Pubsub_State#pubsub_state_dev.subscriptions,
    case
        options_on_subscribe_node(Host, Node_Options, Node_Owners, Entity,
            Resource, Affiliation, Subscriptions, Subscription_Options,
            Pubsub_Features)
    of
        %%
        {ok, 'new',
         {'pending', SubId, _Resource, _Subscription_Options} = Subscription} ->
            mnesia:write(table('pubsub_state', Suffix),
                Pubsub_State#pubsub_state_dev{
                    affiliation   = case Affiliation of
                        'none'      -> 'member';
                        Affiliation -> Affiliation
                    end,
                    subscriptions = [Subscription | Subscriptions]
                },
                write),
            Table_Pubsub_Subscription_Pending = table('pubsub_subscription_pending',
                Suffix),
            case
                mnesia:read(Table_Pubsub_Subscription_Pending,
                    Pubsub_State#pubsub_state_dev.id, write)
            of
                [] ->
                    mnesia:write(Table_Pubsub_Subscription_Pending,
                        #pubsub_subscription_pending{
                            id      = Pubsub_State#pubsub_state_dev.id,
                            nodeidx = Pubsub_State#pubsub_state_dev.nodeidx,
                            subids  = [SubId]
                        },
                        write);
                [Pubsub_Subscription_Pending] ->
                    mnesia:write(Table_Pubsub_Subscription_Pending,
                        Pubsub_Subscription_Pending#pubsub_subscription_pending{
                            subids = [SubId
                                | Pubsub_Subscription_Pending
                                      #pubsub_subscription_pending.subids]
                        },
                        write)
            end,
            {result, Subscription, _Pubsub_Last_Item = undefined};
        %%
        {ok, Type, Subscription} ->
            case Type of
                'new' ->
                    mnesia:write(table('pubsub_state', Suffix),
                        Pubsub_State#pubsub_state_dev{
                            affiliation   = case Affiliation of
                                'none'      -> 'member';
                                Affiliation -> Affiliation
                            end,
                            subscriptions = [Subscription | Subscriptions]
                        },
                        write);
                'old' ->
                    ok
            end,
            {result,
             Subscription,
             _Pubsub_Last_Item = case
                 (get_value(Subscription_Options, 'deliver', true) == true)
                          andalso
                 (get_value(Subscription_Options, 'show-values') =/= [])
                          andalso
                 (get_value(Node_Options, 'send_last_published_item', 'never')
                      =/= 'never')
             of
                 true ->
                     case
                         mnesia:read(table('pubsub_last_item', Suffix),
                             Pubsub_State#pubsub_state_dev.nodeidx,
                             read)
                     of
                         []                 -> undefined;
                         [Pubsub_Last_Item] -> Pubsub_Last_Item
                     end;
             false ->
                 undefined
             end};
        Error ->
            Error
    end.

%%
-spec(options_on_subscribe_node/9 ::
(
  Host                 :: xmpp_jid:raw_jid_component_bare(),
  Node_Options         :: pubsub_options:options_node(),
  Node_Owners          :: [Entity::xmpp_jid:usr_bare(),...],
  Entity               :: xmpp_jid:usr_bare(),
  Resource             :: undefined
                        | xmpp_jid:resource_jid()
                        | {'caps', xmpp_jid:resource_jid()},
  Affiliation          :: exmpp_pubsub:affiliation(),
  Subscriptions        :: [] | exmpp_pubsub:subscriptions(),
  Subscription_Options :: [] | pubsub_options:options_subscription(),
  Pubsub_Features      :: exmpp_pubsub:pubsub_features())
    -> {ok,
        Type         :: 'new' | 'old',
        Subscription :: exmpp_pubsub:subscription_subscribed()}
     %
     | {ok,
        Type         :: 'new',
        Subscription :: exmpp_pubsub:subscription_pending()}
    %%%
     | {error, 'forbidden'}
     | {error, 'not-authorized', 'presence-subscription-required'}
     | {error, 'not-authorized', 'not-in-roster-group'}
     | {error, 'not-authorized', 'pending-subscription'}
     | {error, 'not-allowed',    'closed-node'}
).

options_on_subscribe_node(Host, Node_Options, Node_Owners, Entity,
  Resource, Affiliation, Subscriptions, Subscription_Options, Pubsub_Features) ->
    options_on_subscribe_node(get_option(Node_Options, 'subscribe'), Host,
        Node_Options, Node_Owners, Entity, Resource, Affiliation, Subscriptions,
        Subscription_Options,
        _Multi_Subscribe = lists:member(<<"multi-subscribe">>, Pubsub_Features)).

%%
-spec(options_on_subscribe_node/10 ::
(
  Option               :: pubsub_options:option_node_access_model()
                        | pubsub_options:option_node_subscribe(),
  Host                 :: xmpp_jid:raw_jid_component_bare(),
  Node_Options         :: pubsub_options:options_node(),
  Node_Owners          :: [Entity::xmpp_jid:usr_bare(),...],
  Entity               :: xmpp_jid:usr_bare(),
  Resource             :: undefined
                        | xmpp_jid:resource_jid()
                        | {'caps', xmpp_jid:resource_jid()},
  Affiliation          :: exmpp_pubsub:affiliation(),
  Subscriptions        :: [] | exmpp_pubsub:subscriptions(),
  Subscription_Options :: [] | pubsub_options:options_subscription(),
  Multi_Subscribe      :: boolean())
    -> {ok,
        Type         :: 'new' | 'old',
        Subscription :: exmpp_pubsub:subscription_subscribed()}
     %
     | {ok,
        Type         :: 'new',
        Subscription :: exmpp_pubsub:subscription_pending()}
    %%%
     | {error, 'forbidden'}
     | {error, 'not-authorized', 'presence-subscription-required'}
     | {error, 'not-authorized', 'not-in-roster-group'}
     | {error, 'not-authorized', 'pending-subscription'}
     | {error, 'not-allowed',    'closed-node'}
).

options_on_subscribe_node({'subscribe', Subscribe}, _Host, _Node_Options,
  _Node_Owners, _Entity, _Resource, Affiliation, _Subscriptions,
  _Subscription_Options, _Multi_Subscribe)
  when   Subscribe   == false
  orelse Affiliation == 'outcast'
  orelse Affiliation == 'publish-only' ->
    {error, 'forbidden'};
%%
options_on_subscribe_node({'subscribe', true}, Host, Node_Options,
  Node_Owners, Entity, Resource, Affiliation, Subscriptions,
  Subscription_Options, Multi_Subscribe) ->
    options_on_subscribe_node(get_option(Node_Options, 'access_model'), Host,
        Node_Options, Node_Owners, Entity, Resource, Affiliation, Subscriptions,
        Subscription_Options, Multi_Subscribe);

options_on_subscribe_node({'access_model', open}, _Host, _Node_Options,
  _Node_Owners, _Entity, Resource, _Affiliation, Subscriptions,
  Subscription_Options, false = _Multi_Subscribe) ->
    case lists:keyfind(Resource, 3, Subscriptions) of
        {_Resource, Subscription} ->
            {ok, 'old', Subscription};
        false ->
            {ok,
             'new',
             exmpp_pubsub:subscription_subscribed(Resource, Subscription_Options)}
    end;
%%
options_on_subscribe_node({'access_model', open}, _Host, _Node_Options,
  _Node_Owners, _Entity, Resource, _Affiliation, _Subscriptions,
  Subscription_Options, true = _Multi_Subscribe) ->
    {ok, 'new', exmpp_pubsub:subscription_subscribed(Resource, Subscription_Options)};
%%
options_on_subscribe_node({'access_model', presence}, _Host, _Node_Options,
  _Node_Owners, _Entity, Resource, Affiliation, Subscriptions,
  Subscription_Options, false = _Multi_Subscribe)
  when   Affiliation == 'owner'
  orelse Affiliation == 'publisher'
  orelse Affiliation == 'member' ->
    case lists:keyfind(Resource, 3, Subscriptions) of
        {_Resource, Subscription} ->
            {ok, 'old', Subscription};
        false ->
            {ok,
             'new',
             exmpp_pubsub:subscription_subscribed(Resource, Subscription_Options)}
    end;
%%
options_on_subscribe_node({'access_model', presence}, _Host, _Node_Options,
  _Node_Owners, _Entity, Resource, Affiliation, _Subscriptions,
  Subscription_Options, true = _Multi_Subscribe)
  when   Affiliation == 'owner'
  orelse Affiliation == 'publisher'
  orelse Affiliation == 'member' ->
    {ok,
     'new',
     exmpp_pubsub:subscription_subscribed(Resource, Subscription_Options)};
%%
options_on_subscribe_node({'access_model', presence}, _Host, _Node_Options,
  _Node_Owners, _Entity, Resource, Affiliation, Subscriptions,
  Subscription_Options, false = _Multi_Subscribe)
  when   Affiliation == 'owner'
  orelse Affiliation == 'publisher'
  orelse Affiliation == 'member' ->
    case lists:keyfind(Resource, 3, Subscriptions) of
        {_Resource, Subscription} ->
            {ok, 'old', Subscription};
        false ->
            {ok,
             'new',
             exmpp_pubsub:subscription_subscribed(Resource, Subscription_Options)}
    end;
%%
options_on_subscribe_node({'access_model', presence}, Host, _Node_Options,
  Node_Owners, Entity, Resource, _Affiliation, _Subscriptions,
  Subscription_Options, _Multi_Subscribe) ->
    case is_contact_subscribed_to_node_owners(Host, Entity, Node_Owners) of
        false ->
            {error, 'not-authorized', 'presence-subscription-required'};
        _Node_Owner ->
            {ok,
             'new',
             exmpp_pubsub:subscription_subscribed(Resource, Subscription_Options)}
    end;
%%
options_on_subscribe_node({'access_model', roster}, _Host, _Node_Options,
  _Node_Owners, _Entity, Resource, Affiliation, _Subscriptions,
  Subscription_Options, true = _Multi_Subscribe)
  when   Affiliation == 'owner'
  orelse Affiliation == 'publisher'
  orelse Affiliation == 'member' ->
    {ok,
     'new',
     exmpp_pubsub:subscription_subscribed(Resource, Subscription_Options)};
%%
options_on_subscribe_node({'access_model', roster}, _Host, _Node_Options,
  _Node_Owners, _Entity, Resource, Affiliation, Subscriptions,
  Subscription_Options, false = _Multi_Subscribe)
  when   Affiliation == 'owner'
  orelse Affiliation == 'publisher'
  orelse Affiliation == 'member' ->
    case lists:keyfind(Resource, 3, Subscriptions) of
        {_Resource, Subscription} ->
            {ok, 'old', Subscription};
        false ->
            {ok,
             'new',
             exmpp_pubsub:subscription_subscribed(Resource, Subscription_Options)}
    end;
%%
options_on_subscribe_node({'access_model', roster}, _Host, Node_Options,
  _Node_Owners, Entity, Resource, _Affiliation, _Subscriptions,
  Subscription_Options, _Multi_Subscribe) ->
    case
        is_contact_in_allowed_roster_groups(Entity,
            get_value(Node_Options, 'roster_groups_allowed', []))
    of
        false ->
            {error, 'not-authorized', 'not-in-roster-group'};
        {_Node_Owner, _Roster_Group} ->
            {ok,
             'new',
             exmpp_pubsub:subscription_subscribed(Resource, Subscription_Options)}
    end;
%%
options_on_subscribe_node({'access_model', authorize}, _Host, _Node_Options,
  _Node_Owners, _Entity, Resource, Affiliation, _Subscriptions,
  Subscription_Options, true = _Multi_Subscribe)
  when   Affiliation == 'owner'
  orelse Affiliation == 'publisher'
  orelse Affiliation == 'member' ->
    {ok,
     'new',
     exmpp_pubsub:subscription_subscribed(Resource, Subscription_Options)};
%%
options_on_subscribe_node({'access_model', authorize}, _Host, _Node_Options,
  _Node_Owners, _Entity, Resource, Affiliation, Subscriptions,
  Subscription_Options, false = _Multi_Subscribe)
  when   Affiliation == 'owner'
  orelse Affiliation == 'publisher'
  orelse Affiliation == 'member' ->
    case lists:keyfind(Resource, 3, Subscriptions) of
        {_Resource, Subscription} ->
            {ok, 'old', Subscription};
        false ->
            {ok,
             'new',
             exmpp_pubsub:subscription_subscribed(Resource, Subscription_Options)}
    end;
%%
options_on_subscribe_node({'access_model', authorize}, _Host, _Node_Options,
  _Node_Owners, _Entity, Resource, _Affiliation, Subscriptions,
  Subscription_Options, false = _Multi_Subscribe) ->
    case lists:keyfind(Resource, 3, Subscriptions) of
        {_Resource, _Subscription} ->
            {error, 'not-authorized', 'pending-subscription'};
        false ->
            {ok,
             'new',
             exmpp_pubsub:subscription_pending(Resource, Subscription_Options)}
    end;
%%
options_on_subscribe_node({'access_model', whitelist}, _Host, _Node_Options,
  _Node_Owners, _Entity, Resource, Affiliation, _Subscriptions,
  Subscription_Options,  true = _Multi_Subscribe)
  when   Affiliation == 'owner'
  orelse Affiliation == 'publisher'
  orelse Affiliation == 'member' ->
    {ok,
     'new',
     exmpp_pubsub:subscription_subscribed(Resource, Subscription_Options)};
%%
options_on_subscribe_node({'access_model', whitelist}, _Host, _Node_Options,
  _Node_Owners, _Entity, Resource, Affiliation, Subscriptions,
  Subscription_Options, false = _Multi_Subscribe)
  when   Affiliation == 'owner'
  orelse Affiliation == 'publisher'
  orelse Affiliation == 'member' ->
    case lists:keyfind(Resource, 3, Subscriptions) of
        {_Resource, Subscription} ->
            {ok, 'old', Subscription};
        false ->
            {ok,
             'new',
             exmpp_pubsub:subscription_subscribed(Resource, Subscription_Options)}
    end;
%%
options_on_subscribe_node({'access_model', whitelist}, _Host, _Node_Options,
  _Node_Owners, _Entity, _Resource, _Affiliation, _Subscriptions,
  _Subscription_Options, _Multi_Subscribe) ->
    {error, 'not-allowed', 'closed-node'}.


%-- Unsubscribe_Node --%
-spec(unsubscribe_node/6 ::
(
  Suffix            :: atom(),
  Host              :: xmpp_jid:raw_jid_component_bare(),
  Node_Access_Model :: pubsub_options:access_model(),
  Pubsub_State      :: mod_pubsub_dev:pubsub_state_member()
                     | mod_pubsub_dev:pubsub_state_outcast()
                     | mod_pubsub_dev:pubsub_state_owner()
                     | mod_pubsub_dev:pubsub_state_publisher(),
  Resource          :: undefined
                     | xmpp_jid:resource_jid()
                     | {'caps', xmpp_jid:resource_jid()},
  SubId             :: undefined | exmpp_pubsub:subId())
    -> {ok,
        Resource :: undefined
                  | xmpp_jid:resource_jid()
                  | {'caps', xmpp_jid:resource_jid()},
        SubId    :: exmpp_pubsub:subId()}
    %%%
     | {error, 'unexpected',     'not-subscribed'}
     | {error, 'not-acceptable', 'invalid-subid'}
     | {error, 'bad-request',    'subid-required'}
     | {error, 'not-acceptable', 'invalid-subid'}
).

unsubscribe_node(Suffix, Host, Node_Access_Model,
  #pubsub_state_dev{subscriptions = [{Subscription_State, SubId, Resource, _}]}
  = Pubsub_State, Resource, _SubId)
  when _SubId == undefined orelse _SubId == SubId ->
    mnesia:write(table('pubsub_state', Suffix),
        Pubsub_State#pubsub_state_dev{
            subscriptions = []
        },
        write),
    %% unregister pending subscription
    case Subscription_State of
        'pending' ->
            unregister_pending_subscription(Suffix,
                Pubsub_State#pubsub_state_dev.id, SubId);
        _ ->
            ok
    end,
    case Node_Access_Model of
        'roster' ->
            ok;
        _ ->
            ok
    end,
    {ok, Resource, SubId};
%%
unsubscribe_node(Suffix, Host, Node_Access_Model,
  #pubsub_state_dev{subscriptions = [{_Subscription_State, SubId, _Resource, _}]}
  = Pubsub_State, Resource, SubId) ->
    {error, 'unexpected', 'not-subscribed'};
%%
unsubscribe_node(Suffix, Host, Node_Access_Model,
  #pubsub_state_dev{subscriptions = [{_Subscription_State, _SubId, Resource, _}]}
  = Pubsub_State, Resource, SubId) ->
    {error, 'not-acceptable', 'invalid-subid'};
%%
unsubscribe_node(Suffix, Host, Node_Access_Model, Pubsub_State, Resource,
  undefined = _SubId) ->
    case lists:keytake(Resource, 3, Pubsub_State#pubsub_state_dev.subscriptions) of
        {value,
         {Subscription_State, SubId, Resource, _} = Subscription, Subscriptions} ->
            case lists:keymember(Resource, 3, Subscriptions) of
                false ->
                    mnesia:write(table('pubsub_state', Suffix),
                        Pubsub_State#pubsub_state_dev{
                            subscriptions = Subscriptions
                        },
                        write),
                    %% unregister pending subscription
                    case Subscription_State of
                        'pending' ->
                            unregister_pending_subscription(Suffix,
                                Pubsub_State#pubsub_state_dev.id, SubId);
                        _ ->
                            ok
                    end,
                    case Node_Access_Model of
                        'roster' ->
                            ok;
                        _ ->
                            ok
                    end,
                    %% TODO : unregister 'pubsub#tempsub'
                    {ok, Resource, SubId};
                true ->
                    {error, 'bad-request', 'subid-required'}
            end;
        false ->
            {error, 'unexpected', 'not-subscribed'}
    end;
%%
unsubscribe_node(Suffix, Host, Node_Access_Model, Pubsub_State, Resource, SubId) ->
    case lists:keytake(SubId, 2, Pubsub_State#pubsub_state_dev.subscriptions) of
        {value,
         {Subscription_State, SubId, Resource, _} = Subscription, Subscriptions} ->
            mnesia:write(table('pubsub_state', Suffix),
                Pubsub_State#pubsub_state_dev{
                    subscriptions = Subscriptions
                },
                write),
                %% unregister pending subscription
                case Subscription_State of
                    'pending' ->
                        unregister_pending_subscription(Suffix,
                            Pubsub_State#pubsub_state_dev.id, SubId);
                    _ ->
                        ok
                end,
                case Node_Access_Model of
                    'roster' ->
                        ok;
                    _ ->
                        ok
                end,
                {ok, Resource, SubId};
        {value, {_Subscription_State, SubId, _Resource, _} = Subscription, _} ->
            {error, 'unexpected', 'not-subscribed'};
        false ->
            {error, 'not-acceptable', 'invalid-subid'}
    end.

%%
-spec(register_pending_subscription/3 ::
(
  Suffix  :: atom(),
  StateId :: {Entity::xmpp_jid:usr_bare(), NodeIdx::exmpp_pubsub:nodeIdx()},
  SubId   :: exmpp_pubsub:subId())
    -> 'ok'
).

register_pending_subscription(Suffix, {Entity, NodeIdx} = _StateId, SubId) ->
    Table_Pubsub_Subscription_Pending = table('pubsub_subscription_pending', Suffix),
    case mnesia:read(Table_Pubsub_Subscription_Pending, {Entity, NodeIdx}, write) of
        [] ->
            mnesia:write(Table_Pubsub_Subscription_Pending,
                #pubsub_subscription_pending{
                    id      = {Entity, NodeIdx},
                    nodeidx = NodeIdx,
                    subids  = [SubId]
                },
                write);
        [Pubsub_Subscription_Pending] ->
            mnesia:write(Table_Pubsub_Subscription_Pending,
                Pubsub_Subscription_Pending#pubsub_subscription_pending{
                    subids = [SubId
                        | Pubsub_Subscription_Pending
                              #pubsub_subscription_pending.subids]
                },
                write)
    end.

%%
-spec(unregister_pending_subscription/3 ::
(
  Suffix  :: atom(),
  StateId :: {Entity::xmpp_jid:usr_bare(), NodeIdx::exmpp_pubsub:nodeIdx()},
  SubId   :: exmpp_pubsub:subId())
    -> 'ok'
).

unregister_pending_subscription(Suffix, StateId, SubId) ->
    Table_Pubsub_Subscription_Pending = table('pubsub_subscription_pending', Suffix),
    case mnesia:read(Table_Pubsub_Subscription_Pending, StateId, write) of
        %%
        [#pubsub_subscription_pending{subids = [SubId]} = Pubsub_Subscription_Pending] ->
            mnesia:delete_object(Table_Pubsub_Subscription_Pending,
                Pubsub_Subscription_Pending, write);
        %%
        [Pubsub_Subscription_Pending] ->
            mnesia:write(Table_Pubsub_Subscription_Pending,
                Pubsub_Subscription_Pending#pubsub_subscription_pending{
                    subids = lists:delete(SubId,
                        Pubsub_Subscription_Pending#pubsub_subscription_pending.subids)
                    },
                write);
        [] ->
            ok
    end.

%-- Set_Configure_Subscription --%
-spec(set_configure_subscription/5 ::
(
  Suffix               :: atom(),
  Pubsub_State         :: mod_pubsub_dev:pubsub_state_member()
                        | mod_pubsub_dev:pubsub_state_owner()
                        | mod_pubsub_dev:pubsub_state_publisher(),
  SubId                :: undefined | exmpp_pubsub:subId(),
  Resource             :: undefined
                        | xmpp_jid:resource_jid()
                        | {'caps', xmpp_jid:resource_jid()},
  Subscription_Options :: pubsub_options:options_subscription())
    -> 'ok'
    %%%
     | {error, 'unexpected-request', 'not-subscribed'}
     | {error, 'bad-request',        'subid-required'}
     | {error, 'forbidden'}
     | {error, 'not-acceptable',     'invalid-subid'}
).

set_configure_subscription(Suffix, Pubsub_State, undefined = _SubId, Resource,
  Subscription_Options) ->
    case
        lists:keytake(Resource, 3, Pubsub_State#pubsub_state_dev.subscriptions)
    of
        {value, {Subscription_State, SubId, Resource, _Subscription_Options},
         Subscriptions} ->
            case lists:keymember(Resource, 3, Subscriptions) of
                false
                  when Subscription_State == 'pending' ->
                    {error, 'forbidden'};
                false ->
                    mnesia:write(table('pubsub_state', Suffix),
                        Pubsub_State#pubsub_state_dev{
                            subscriptions = [
                                {Subscription_State, SubId, Resource,
                                 Subscription_Options}
                                | Subscriptions]
                        },
                        write),
                    'ok';
                true ->
                    {error, 'bad-request', 'subid-required'}
            end;
         false ->
            {error, 'unexpected-request', 'not-subscribed'}
    end;
%%
set_configure_subscription(Suffix, Pubsub_State, SubId, Resource,
  Subscription_Options) ->
    case lists:keytake(SubId, 2, Pubsub_State#pubsub_state_dev.subscriptions) of
        {value, {'pending', SubId, Resource, _Subscription_Options},
         _Subscriptions} ->
            {error, 'forbidden'};
        {value, {Subscription_State, SubId, Resource, _Subscription_Options},
         Subscriptions} ->
            mnesia:write(table('pubsub_state', Suffix),
                Pubsub_State#pubsub_state_dev{
                    subscriptions = [
                        {Subscription_State, SubId, Resource,
                            Subscription_Options}
                        | Subscriptions]
                },
                write),
                'ok';
        {value, {_Subscription_State, SubId, _Resource, _Subscription_Options},
         _Subscriptions} ->
            {error, 'unexpected-request', 'not-subscribed'};
        false ->
            case
                lists:keytake(Resource, 3,
                    Pubsub_State#pubsub_state_dev.subscriptions)
            of
                {value, {'pending', _SubId, Resource, _Subscription_Options},
                 _Subscriptions} ->
                    {error, 'forbidden'};
                {value, {_Subscription_State, _SubId, _Resource, _Subscription_Options},
                 _Subscriptions} ->
                    {error, 'not-acceptable', 'invalid-subid'};
                false ->
                    {error, 'unexpected-request', 'not-subscribed'}
            end
    end.

%-- Get_Items --%
-spec(get_items/5 ::
(
  Suffix            :: pubsub_db_mnesia:table(),
  Host              :: xmpp_jid:raw_jid_component_bare(),
  Node_Parameters   :: {NodeIdx           :: exmpp_pubsub:nodeIdx(),
                        Node_Owners       :: mod_pubsub_dev:node_owners(),
                        Node_ItemIds      :: [] | exmpp_pubsub:itemIds(),
                        Node_Access_Model :: pubsub_options:access_model(),
                        Node_Groups       :: pubsub_options:rosters_groups_allowed()},
  %%
  Entity_Parameters :: {Entity      :: xmpp_jid:usr_bare(),
                        Affiliation :: exmpp_pubsub:affiliation(),
                        Access      :: undefined | 'pending' | 'roster' | 'presence' | 'authorize'},
  %%
  Criteria          :: {Max_Items :: non_neg_integer(),
                        ItemIds   :: undefined,
                        _SubId    :: exmpp_pubsub:subId()}
                     | {Max_Items :: undefined,
                        ItemIds   :: exmpp_pubsub:itemIds(),
                        _SubId    :: exmpp_pubsub:subId()})
    -> {ok,
        Pubsub_Items :: [] | mod_pubsub_dev:pubsub_items(),
        Cache        :: {Presence_Cache :: undefined,
                         Groups_Cache   :: undefined}}
     %
     | {ok,
        Pubsub_Items :: [] | mod_pubsub_dev:pubsub_items(),
        Cache        :: {Presence_Cache :: true,
                         Groups_Cache   :: undefined}}
     %
     | {ok,
        Pubsub_Items :: [] | mod_pubsub_dev:pubsub_items(),
        Cache        :: {Presence_Cache :: undefined,
                         Groups_Cache   :: [{Node_Owner :: xmpp_jid:usr_bare(),
                                             Groups     :: [Group::binary()]}]}}
    %%%
     | {error, 'forbidden'}
     | {error, 'not-authorized', 'presence-subscription-required'}
     | {error, 'not-authorized', 'not-in-roster-group'}
     | {error, 'not-allowed',    'closed-node'}
).

get_items(_Suffix, _Host, _Node_Parameters, {_Entity, Affiliation, Access}, _Criteria)
  when   Affiliation == 'outcast'
  orelse Affiliation == 'publish-only'
  orelse Access      == 'pending' ->
    {error, 'forbidden'};
%%
get_items(Suffix, _Host,
  {NodeIdx, _Node_Owners, Node_ItemIds, Node_Access_Model, _Node_Groups},
  {_Entity, Affiliation, Access}, {Max_Items, ItemIds, _SubId})
  when   Affiliation        == 'owner'
  %
  orelse Affiliation        == 'publisher'
  %
  orelse Affiliation        == 'member'
  %
  orelse Node_Access_Model  == 'open'
  %
  orelse (Node_Access_Model == 'presence'
      andalso
          Access == 'presence'
              orelse Access == 'roster')
  %
  orelse (Node_Access_Model == 'roster'
      andalso
          Access == 'roster')
  %
  orelse (Node_Access_Model == 'authorize'
      andalso
          Access == 'authorize') ->
    {ok,
     _Pusbub_Items = get_items(Suffix, NodeIdx,
         get_max_itemids(Max_Items, ItemIds, Node_ItemIds)),
     _Cache = {undefined, undefined}};
%
get_items(Suffix, Host,
  {NodeIdx, Node_Owners, Node_ItemIds, 'presence' = _Node_Access_Model, _Node_Groups},
  {Entity, _Affiliation, _Access}, {Max_Items, ItemIds, _SubId}) ->
    case is_contact_subscribed_to_node_owners(Host, Entity, Node_Owners) of
        false ->
            {error, 'not-authorized', 'presence-subscription-required'};
        Node_Owner ->
            {ok,
             _Pusbub_Items = get_items(Suffix, NodeIdx,
                 get_max_itemids(Max_Items, ItemIds, Node_ItemIds)),
             _Cache = {_Presence_Cache = true, _Groups_Cache = undefined}}
    end;
%%
get_items(Suffix, _Host,
  {NodeIdx, _Node_Owners, Node_ItemIds, 'roster' = _Node_Access_Model, Node_Groups},
  {Entity, _Affiliation, _Access}, {Max_Items, ItemIds, _SubId}) ->
    case is_contact_in_allowed_roster_groups(Entity, Node_Groups) of
        {Node_Owner, Roster_Group} ->
            {ok,
             _Pusbub_Items = get_items(Suffix, NodeIdx,
                 get_max_itemids(Max_Items, ItemIds, Node_ItemIds)),
             _Cache = {
                 _Presence_Cache = undefined,
                 _Groups_Cache = [{Node_Owner, [Roster_Group]}]
             }
            };
        false ->
            {error, 'not-authorized', 'not-in-roster-group'}
    end;
%%
get_items(_Suffix, _Host,
  {_NodeIdx, _Node_Owners, _Node_ItemIds, 'authorize' = _Node_Access_Model, _Node_Groups},
  _Entity_Parameters, _Criteria) ->
    {error, 'not-allowed', 'closed-node'};
%%
get_items(_Suffix, _Host, _Node_Parameters, _Entity_Parameters, _Criteria) ->
    {error, 'forbidden'}.

%%
-spec(get_max_itemids/3 ::
(
  Max_Items    :: undefined | non_neg_integer(),
  ItemIds      :: [] | exmpp_pubsub:itemIds(),
  Node_ItemIds :: [] | exmpp_pubsub:itemIds())
    -> Max_ItemIds :: undefined | [] | exmpp_pubsub:itemIds()
).

get_max_itemids(undefined = _Max_Items, [] = _ItemIds, _Node_ItemIds) ->
    _Max_ItemIds = undefined;
%%
get_max_itemids(undefined = _Max_Items, ItemIds, Node_ItemIds) ->
    _Max_ItemIds = lists:filter(fun
        (ItemId) ->
            lists:member(ItemId, Node_ItemIds)
    end, ItemIds);
%%
get_max_itemids(Max_Items, undefined = _ItemIds, Node_ItemIds) ->
    _Max_ItemIds = get_max_itemids(Max_Items, [], Node_ItemIds);
%%
get_max_itemids(_Max_Items, ItemIds, _Node_ItemIds)
  when _Max_Items == 0 orelse _Node_ItemIds == [] ->
    _Max_ItemIds = lists:reverse(ItemIds);
%%
get_max_itemids(Max_Items, ItemIds, [ItemId | Node_ItemIds]) ->
    get_max_itemids(Max_Items - 1, [ItemId | ItemIds], Node_ItemIds).

%%
-spec(get_items/3 ::
(
  Suffix  :: pubsub_db_mnesia:suffix(),
  NodeIdx :: exmpp_pubsub:nodeIdx(),
  ItemIds :: undefined | [] | exmpp_pubsub:itemIds())
    -> Pubsub_Items :: [] | mod_pubsub_dev:pubsub_items()
).

get_items(Suffix, NodeIdx, undefined = _ItemIds) ->
    _Pubsub_Items = mnesia:index_read(table('pubsub_item', Suffix), NodeIdx, nodeidx);
%%
get_items(Suffix, NodeIdx, ItemIds) ->
    Table_Pubsub_Item = table('pubsub_item', Suffix),
    _Pubsub_Items = lists:foldl(fun
        (ItemId, Pubsub_Items) ->
            case
                mnesia:index_match_object(Table_Pubsub_Item,
                    #pubsub_item_dev{
                        id      = {ItemId, NodeIdx},
                        nodeidx = NodeIdx,
                        _       = '_'
                    },
                    nodeidx,
                    read)
            of
                [Pubsub_Item] -> [Pubsub_Item | Pubsub_Items];
                _             -> Pubsub_Items
            end
    end, [], ItemIds).
