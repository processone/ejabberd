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

-module(pubsub_groups).
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
  check_acces_model/3,
  check_acces_model/7,
  check_publish_model/3,
  get_entity_roster/1,
  is_contact_subscribed_to_node_owners/3,
  is_contact_in_allowed_roster_groups/2,
  has_subscriptions/1,
  get_resources/2,
  get_resources_show/2,
  rosters_groups_allowed_cache/2
]).

-import(pubsub_db_mnesia,
[
  table/2
]).


-type(group() :: binary()).

-type(node_group() :: group()).

-type(roster_group() :: group()).

-type(node_groups() :: [Node_Group::node_group(),...]).

-type(roster_groups() :: [Roster_Group::roster_group(),...]).

-type(nodeIdxs() :: [NodeIdx::exmpp_pubsub:nodeIdx(),...]).

-type(n0de()
  :: {NodeIdx :: exmpp_pubsub:nodeIdx(),
      Groups  :: node_groups()}
).

-type(n0des() :: [Node::n0de(),...]).

-type(subscription()
  :: {NodeIdx :: exmpp_pubsub:nodeIdx(),
      Groups  :: roster_groups()}
).

-type(subscriptions() :: [Subscription::subscription(),...]).

-type(contact()
  :: {Entity        :: xmpp_jid:usr_bare(),
      Subscriptions :: subscriptions()}
).

-type(contacts() :: [Contact::contact(),...]).


-record(pubsub_groups,
{
  owner         :: xmpp_jid:usr_bare(),
  nodes    = [] :: n0des(),
  contacts = [] :: [] | contacts()
}).


create_table_pubsub_groups(Suffix) ->
    mnesia:create_table(table('pubsub_groups', Suffix),
        [{type, set},
         {disc_copies, [node()]},
         {record_name, pubsub_groups},
         {attributes, record_info(fields, pubsub_groups)}]).



%%
-spec(register_groups/4 ::
(
  Suffix  :: atom(),
  NodeIdx :: exmpp_pubsub:nodeIdx(),
  Owner   :: xmpp_jid:usr_bare(),
  Groups  :: [] | node_groups())
    -> undefined
     %
     | {Subscribers   :: [Entity::xmpp_jid:usr_bare()],
        Unsubscribers :: [Entity::xmpp_jid:usr_bare()]}
).

register_groups(Suffix, NodeIdx, Owner, [] = _Groups) ->
    case
        mnesia:read(Table_Pubsub_Groups = table('pubsub_groups', 'dev'),
            Owner, write)
    of
        %%
        [] ->
            undefined;
        %%
        [#pubsub_groups{nodes = [{NodeIdx, _Node_Groups}]} = Pubsub_Groups] ->
            mnesia:delete_object(Table_Pubsub_Groups, Pubsub_Groups, write),
            %
            {_Subscribers   = [],
             _Unsubscribers = lists:foldl(fun
                ({Entity, Subscriptions}, Unsubscribers) ->
                    case lists:keymember(NodeIdx, 1, Subscriptions) of
                        true  -> [Entity | Unsubscribers];
                        false -> Unsubscribers
                    end
                %
             end, [], Pubsub_Groups#pubsub_groups.contacts)};
        %%
        [Pubsub_Groups] ->
            case lists:keyfind(NodeIdx, 1, Pubsub_Groups#pubsub_groups.nodes) of
                {_NodeIdx, Node_Groups} ->
                    {Contacts, Unsubscribers} = subscriptions1(NodeIdx,
                        Pubsub_Groups#pubsub_groups.contacts),
                    %
                    mnesia:write(Table_Pubsub_Groups,
                        Pubsub_Groups#pubsub_groups{
                            contacts = Contacts
                        },
                        write),
                    %
                    {_Subscribers   = [],
                     _Unsubscribers = Unsubscribers};
                false ->
                    undefined
            end
    end;
%%
register_groups(Suffix, NodeIdx, Owner, Groups) ->
    case
        mnesia:read(Table_Pubsub_Groups = table('pubsub_groups', 'dev'), Owner,
            write)
    of
        [] ->
            {Contacts, Subscribers} = subscriptions2(NodeIdx, Owner, Groups),
            %
            mnesia:write(Table_Pubsub_Groups,
                #pubsub_groups{
                    owner    = Owner,
                    nodes    = [{NodeIdx, Groups}],
                    contacts = Contacts
                },
                write),
            %
            {_Subscribers   = Subscribers,
             _Unsubscribers = []};
        [Pubsub_Groups] ->
            case lists:keyfind(NodeIdx, 1, Pubsub_Groups#pubsub_groups.nodes) of
                {_NodeIdx, Node_Groups} ->
                    case diff_groups(Groups, Node_Groups) of
                        %%
                        {[] = _New_Groups, [] = _Old_Groups} ->
                            {_Subscribers   = [],
                             _Unsubscribers = []};
                        %%
                        {[] = _New_Groups, Old_Groups} ->
                            {Contacts, Unsubscribers}
                                = subscriptions3(NodeIdx, Old_Groups,
                                    Pubsub_Groups#pubsub_groups.contacts),
                            %
                            mnesia:write(Table_Pubsub_Groups,
                                Pubsub_Groups#pubsub_groups{
                                    nodes    = lists:keyreplace(NodeIdx, 1,
                                        Pubsub_Groups#pubsub_groups.nodes,
                                        {NodeIdx, Groups}),
                                    contacts = Contacts
                                },
                                write),
                            %
                            {_Subscribers   = [],
                             _Unsubscribers = Unsubscribers};
                        %%
                        {_New_Groups, _Old_Groups}  ->
                            {Contacts, Subscribers, Unsubscribers}
                                = subscriptions4(NodeIdx, Owner, Node_Groups,
                                    Pubsub_Groups#pubsub_groups.contacts),
                            %
                            mnesia:write(Table_Pubsub_Groups,
                                Pubsub_Groups#pubsub_groups{
                                    nodes    = lists:keyreplace(NodeIdx, 1,
                                        Pubsub_Groups#pubsub_groups.nodes,
                                            {NodeIdx, Node_Groups}),
                                    contacts = Contacts
                                },
                                write),
                            %
                            {Subscribers    = Subscribers,
                             _Unsubscribers = Unsubscribers}
                    end;
                false ->
                    {Contacts, Subscribers}
                        = subscriptions5(NodeIdx, Owner, Groups,
                              _Contacts = Pubsub_Groups#pubsub_groups.contacts),
                    %
                    mnesia:write(Table_Pubsub_Groups,
                        Pubsub_Groups#pubsub_groups{
                            nodes    = [{NodeIdx, Groups}
                                | Pubsub_Groups#pubsub_groups.nodes],
                            contacts = Contacts
                        },
                        write),
                    %
                    {_Subscribers   = Subscribers,
                     _Unsubscribers = []}
            end
    end.


%% TODO : fix this hook managment
-spec(monitor_contacts/3 ::
(
  _            :: 'subscribed',
  Entity       :: xmpp_jid:usr_bare(),
  Jid_Contacts :: xmpp_jid:entity_bare())
    -> undefined
    %
    | {Server  :: xmpp_jid:raw_jid_component_bare(),
       Entity  :: xmpp_jid:usr_bare(),
       Contact :: xmpp_jid:usr_bare(),
       _       :: {'subscribed', NodeIdxs :: nodeIdxs()}}
).

monitor_contacts('subscribed', {_, Server, _} = Entity, Jid_Contact) ->
    case roster_groups(Entity, Contact = jlid:jid_to_lower(Jid_Contact)) of
        [] ->
            undefined;
        Roster_Groups ->
            case
                mnesia:read(Table_Pubsub_Groups = table('pubsub_groups', dev),
                    _Owner = Entity, write)
            of
                [] ->
                    undefined;
                [Pubsub_Groups] ->
                    case
                        filter_subscriptions(Pubsub_Groups#pubsub_groups.nodes,
                            Roster_Groups)
                    of
                        [] ->
                            undefined;
                        Subscriptions ->
                            mnesia:write(Table_Pubsub_Groups,
                                Pubsub_Groups#pubsub_groups{
                                    contacts = case
                                        lists:keyreplace(Contact, 1,
                                            Pubsub_Groups#pubsub_groups.contacts,
                                            {Contact, Subscriptions})
                                    of
                                        _Contacts
                                          when _Contacts ==
                                               Pubsub_Groups#pubsub_groups.contacts ->
                                            [{Contact, Subscriptions}
                                            | Pubsub_Groups#pubsub_groups.contacts];
                                        Contacts ->
                                            Contacts
                                    end
                                },
                                write),
                            {Server, Entity, Contact,
                             {'subscribed',
                              _NodeIdx = lists:map(fun
                                  ({NodeIdx, _Groups}) ->
                                      NodeIdx
                              end, Subscriptions)}}
                    end
            end
    end.


%%
-spec(monitor_groups/2 ::
(
  Server :: xmpp_jid:raw_jid_component_bare(),
  Roster :: #roster{groups :: [] | roster_groups()})
    -> undefined
    %
    | {Server        :: xmpp_jid:raw_jid_component_bare(),
       Entity        :: xmpp_jid:usr_bare(),
       Diff_NodeIdxs :: {NodeIdxs_Subscriptions   :: [] | nodeIdxs(),
                         NodeIdxs_Unsubscriptions :: [] | nodeIdxs()}}
).

monitor_groups(Server, #roster{us = {U,S}} = _Roster) ->
    case
        mnesia:read(Table_Pubsub_Groups = table('pubsub_groups', dev),
            _Owner = {U,S,undefined}, write)
    of
        [] ->
            undefined;
        [Pubsub_Groups] ->
            case
                lists:keyfind(_Roster#roster.jid, 1,
                    Pubsub_Groups#pubsub_groups.contacts)
            of  %%
                {_Entity, Old_Subscriptions}
                  when   _Roster#roster.subscription == 'delete'
                  orelse _Roster#roster.subscription == 'none' ->
                    mnesia:write(Table_Pubsub_Groups,
                        Pubsub_Groups#pubsub_groups{
                            contacts = lists:keydelete(_Roster#roster.jid, 1,
                                Pubsub_Groups#pubsub_groups.contacts)
                        },
                        write),
                    {Server,
                     _Entity = _Roster#roster.jid,
                     _Diff_NodeIdxs = diff_nodeidxs(_New_Subscriptions = [],
                         Old_Subscriptions)};
               %%
                {_Entity, Old_Subscriptions}
                  when (_Roster#roster.subscription == 'both'
                            orelse
                               _Roster#roster.subscription == 'from')
                  andalso _Roster#roster.groups == [] ->
                    mnesia:write(Table_Pubsub_Groups,
                        Pubsub_Groups#pubsub_groups{
                            contacts = lists:keydelete(_Roster#roster.jid, 1,
                                Pubsub_Groups#pubsub_groups.contacts)
                        },
                        write),
                    {Server,
                     _Roster#roster.jid,
                     _Diff_NodeIdxs = diff_nodeidxs(_New_Subscriptions = [],
                         Old_Subscriptions)};
                %%
                {_Entity, Old_Subscriptions}
                  when   _Roster#roster.subscription == 'both'
                  orelse _Roster#roster.subscription == 'from' ->
                    case
                        filter_subscriptions(Pubsub_Groups#pubsub_groups.nodes,
                            _Roster#roster.groups)
                    of
                        [] ->
                            mnesia:write(Table_Pubsub_Groups,
                                Pubsub_Groups#pubsub_groups{
                                    contacts = lists:keydelete(_Roster#roster.jid,
                                        1, Pubsub_Groups#pubsub_groups.contacts)
                                },
                                write),
                            {Server, _Roster#roster.jid,
                             diff_nodeidxs(_New_Subscriptions = [],
                                 Old_Subscriptions)};
                        New_Subscriptions ->
                            mnesia:write(Table_Pubsub_Groups,
                                Pubsub_Groups#pubsub_groups{
                                    contacts = lists:keyreplace(_Roster#roster.jid,
                                        1, Pubsub_Groups#pubsub_groups.contacts,
                                        {_Roster#roster.jid, New_Subscriptions})
                                },
                                write),
                            {Server,
                             _Entity = _Roster#roster.jid,
                             _Diff_NodeIdxs = diff_nodeidxs(New_Subscriptions,
                                 Old_Subscriptions)}
                    end;
                %%
                false
                  when (_Roster#roster.subscription == 'both'
                            orelse
                                _Roster#roster.subscription == 'from')
                  andalso _Roster#roster.groups =/= [] ->
                    case
                        filter_subscriptions(Pubsub_Groups#pubsub_groups.nodes,
                            _Roster#roster.groups)
                    of
                        [] ->
                            undefined;
                        New_Subscriptions ->
                            mnesia:write(Table_Pubsub_Groups,
                                Pubsub_Groups#pubsub_groups{
                                    contacts = [{_Roster#roster.jid, New_Subscriptions}
                                        | Pubsub_Groups#pubsub_groups.contacts]
                                },
                                write),
                            {Server,
                             _Entity = _Roster#roster.jid,
                             _Diff_NodeIdxs = diff_nodeidxs(New_Subscriptions,
                                 _Old_Subscriptions = [])}
                    end;
                %%
                _ ->
                    undefined
            end
    end.



%%
%%
-spec(subscriptions1/2 ::
(
  NodeIdx      :: exmpp_pubsub:nodeIdx(),
  Old_Contacts :: [] | contacts())
    -> Diff_Contacts :: {New_Contacts  :: [] | contacts(),
                         Unsubscribers :: [Entity::xmpp_jid:usr_bare()]}
).

subscriptions1(NodeIdx, Contacts) ->
    _Diff_Contacts = subscriptions1(NodeIdx, _Old_Contacts = Contacts,
        {_New_Contacts = [], _Unsubscribers = []}).


%%
-spec(subscriptions1/3 ::
(
  NodeIdx       :: exmpp_pubsub:nodeIdx(),
  Old_Contacts  :: [] | contacts(),
  Diff_Contacts :: {Contacts      :: [] | contacts(),
                    Unsubscribers :: [Entity::xmpp_jid:usr_bare()]})
    -> Diff_Contacts :: {New_Contacts  :: [] | contacts(),
                         Unsubscribers :: [Entity::xmpp_jid:usr_bare()]}
).

subscriptions1(_NodeIdx, [] = _Old_Contacts, Diff_Contacts) ->
    Diff_Contacts;
%%
subscriptions1(NodeIdx, [{Entity, Subscriptions} | Old_Contacts],
  {Contacts, Unsubscribers}) ->
    subscriptions1(NodeIdx, Old_Contacts,
        _Diff_Contacts = case lists:keydelete(NodeIdx, 1, Subscriptions) of
            [] ->
                {_Contacts      = Contacts,
                 _Unsubscribers = [Entity | Unsubscribers]};
            Subscriptions ->
                {_Contacts      = [{Entity, Subscriptions} | Contacts],
                 _Unsubscribers = Unsubscribers};
            New_Subscriptions ->
                {_Contacts      = [{Entity, New_Subscriptions} | Contacts],
                 _Unsubscribers = [Entity | Unsubscribers]}
        end).


%%
-spec(subscriptions2/3 ::
(
  NodeIdx     :: exmpp_pubsub:nodeIdx(),
  Owner       :: xmpp_jid:usr_bare(),
  Node_Groups :: node_groups())
    -> Diff_Contacts :: {Contacts    :: [] | contacts(),
                         Subscribers :: [Entity::xmpp_jid:usr_bare()]}
).

subscriptions2(NodeIdx, Owner, Node_Groups) ->
    subscriptions2(NodeIdx, _Rosters = get_entity_roster(Owner), Node_Groups,
        {_New_Contacts = [], _Subscribers = []}).

%%
-spec(subscriptions2/4 ::
(
  NodeIdx       :: exmpp_pubsub:nodeIdx(),
  Rosters       :: [Roster::#roster{groups :: [] | roster_groups()}],
  Node_Groups   :: node_groups(),
  Diff_Contacts :: {Contacts    :: [] | contacts(),
                    Subscribers :: [Entity::xmpp_jid:usr_bare()]})
    -> Diff_Contacts :: {Contacts    :: [] | contacts(),
                         Subscribers :: [Entity::xmpp_jid:usr_bare()]}
).

subscriptions2(_NodeIdx, [] = _Rosters, _Node_Groups, Diff_Contacts) ->
    Diff_Contacts;
%%
subscriptions2(NodeIdx, [_Roster | Rosters], Node_Groups, {Contacts, Subscribers})
  when _Roster#roster.groups =/= [] ->
    subscriptions2(NodeIdx, Rosters, Node_Groups,
        _Diff_Contacts = case
            filter_groups(_Roster#roster.groups, Node_Groups, [])
        of
            [] ->
                {_Contacts    = Contacts,
                 _Subscribers = Subscribers};
            Roster_Groups ->
                {_Contacts    = [{_Roster#roster.jid, [{NodeIdx, Roster_Groups}]}
                                | Contacts],
                 _Subscribers = [_Roster#roster.jid | Subscribers]}
        end);
%%
subscriptions2(NodeIdx, [_Roster | Rosters], Node_Groups, Diff_Contacts) ->
    subscriptions2(NodeIdx, Rosters, Node_Groups, Diff_Contacts).


%%
-spec(subscriptions3/3 ::
(
  NodeIdx       :: exmpp_pubsub:nodeIdx(),
  Old_Groups    :: roster_groups(),
  Contacts      :: contacts() | [])
    -> Diff_Contacts :: {Contacts      :: [] | contacts(),
                         Unsubscribers :: [Entity::xmpp_jid:usr_bare()]}
).
subscriptions3(NodeIdx, Old_Groups, Contacts) ->
    subscriptions3(NodeIdx, Contacts, Old_Groups, {[],[]}).

%%
-spec(subscriptions3/4 ::
(
  NodeIdx       :: exmpp_pubsub:nodeIdx(),
  Old_Contacts  :: contacts() | [],
  Old_Groups    :: roster_groups(),
  Diff_Contacts :: {Contacts      :: [] | contacts(),
                    Unsubscribers :: [Entity::xmpp_jid:usr_bare()]})
    -> Diff_Contacts :: {Contacts      :: [] | contacts(),
                         Unsubscribers :: [Entity::xmpp_jid:usr_bare()]}
).

subscriptions3(_NodeIdx, [] = _Contacts, _Old_Groups, Diff_Contacts) ->
    Diff_Contacts;
%%
subscriptions3(NodeIdx, [{Entity, Subscriptions} | Old_Contacts], Old_Groups,
  {Contacts, Unsubscribers}) ->
    subscriptions3(NodeIdx, Old_Contacts, Old_Groups,
        _Diff_Contacts = case lists:keyfind(NodeIdx, 1, Subscriptions) of
            {_NodeIdx, Roster_Groups} ->
                case delete_groups(Old_Groups, Roster_Groups) of
                    [] ->
                        {_Contacts      = Contacts,
                         _Unsubscribers = [Entity | Unsubscribers]};
                    Groups ->
                        {_Contacts      = [{Entity,
                                            lists:keyreplace(NodeIdx, 1,
                                                Subscriptions, {NodeIdx, Groups})}
                                          | Contacts],
                         _Unsubscribers = Unsubscribers}
                end;
            false ->
                {_Contacts      = [{Entity, Subscriptions} | Contacts],
                 _Unsubscribers = Unsubscribers}
        end).


%%
-spec(subscriptions4/4 ::
(
  NodeIdx     :: exmpp_pubsub:nodeIdx(),
  _           :: xmpp_jid:usr_bare()
               | [Roster::#roster{groups :: [] | roster_groups()}],
  Node_Groups :: node_groups(),
  _           :: [] | contacts()
               | {Contacts      :: [] | contacts(),
                  Subscribers   :: [Entity::xmpp_jid:usr_bare()],
                  Unsubscribers :: [Entity::xmpp_jid:usr_bare()]})
    -> Diff_Contacts :: {Contacts      :: [] | contacts(),
                         Subscribers   :: [Entity::xmpp_jid:usr_bare()],
                         Unsubscribers :: [Entity::xmpp_jid:usr_bare()]}
).

subscriptions4(_NodeIdx, [] = _Rosters, _Node_Groups, Diff_Contacts) ->
    Diff_Contacts;
%%
subscriptions4(NodeIdx, [_Roster | Rosters], Node_Groups,
  {Contacts, Subscribers, Unsubscribers})
  when _Roster#roster.groups =/= [] ->
    subscriptions4(NodeIdx, Rosters, Node_Groups,
        _Diff_Contacts = case
            {lists:keyfind(Entity = _Roster#roster.jid, 1, Contacts),
             filter_groups(_Roster#roster.groups, Node_Groups, [])}
        of
            %%
            {false, [] = _Groups} ->
                {_Contacts      = Contacts,
                 _Subscribers   = Subscribers,
                 _Unsubscribers = Unsubscribers};
            %%
            {false, Groups} ->
                {_Contacts      = [{Entity, [{NodeIdx, Groups}]} | Contacts],
                 _Subscribers   = [Entity | Subscribers],
                 _Unsubscribers = Unsubscribers};
            %%
            {{_Entity, [{NodeIdx, _Roster_Groups}] = _Subscriptions}, [] = _Groups} ->
                {_Contacts      = lists:keydelete(Entity, 1, Contacts),
                 _Subscribers   = Subscribers,
                 _Unsubscribers = [Entity | Unsubscribers]};
            %%
            {{_Entity, Subscriptions}, [] = _Groups} ->
                case lists:keydelete(NodeIdx, 1, Subscriptions) of
                    Subscriptions ->
                        {_Contacts      = Contacts,
                         _Subscribers   = Subscribers,
                         _Unsubscribers = Unsubscribers};
                    New_Subscriptions ->
                        {_Contacts      = lists:keyreplace(Entity, 1, Contacts,
                                              {Entity, New_Subscriptions}),
                         _Subscribers   = Subscribers,
                         _Unsubscribers = [Entity | Unsubscribers]}
                end;
            {{_Entity, Subscriptions}, Groups} ->
                case lists:keyreplace(NodeIdx, 1, Subscriptions, {NodeIdx, Groups}) of
                    Subscriptions ->
                        {_Contacts      = lists:keyreplace(Entity, 1, Contacts,
                                              {Entity,
                                               [{NodeIdx, Groups} | Subscriptions]}),
                         _Subscribers   = [Entity | Subscribers],
                         _Unsubscribers = Unsubscribers};
                    New_Subscriptions ->
                        {_Contacts      = lists:keyreplace(Entity, 1, Contacts,
                                              {Entity, New_Subscriptions}),
                         _Subscribers   = Subscribers,
                         _Unsubscribers = Unsubscribers}
                end
        end);
%%
subscriptions4(NodeIdx, [_Roster | Rosters], Node_Groups, Diff_Contacts) ->
    subscriptions4(NodeIdx, Rosters, Node_Groups, Diff_Contacts);
%%
subscriptions4(NodeIdx, Owner, Node_Groups, Contacts) ->
    subscriptions4(NodeIdx, _Rosters = get_entity_roster(Owner), Node_Groups,
        _Diff_Contacts = {Contacts, _Subscribers = [], _Unsubscribers = []}).



-spec(subscriptions5/4 ::
(
  NodeIdx     :: exmpp_pubsub:nodeIdx(),
  _           :: xmpp_jid:usr_bare()
               | [Roster::#roster{groups :: [] | roster_groups()}],
  Node_Groups :: node_groups(),
  _           :: [] | contacts()
               | {Contacts    :: [] | contacts(),
                  Subscribers :: [Entity::xmpp_jid:usr_bare()]})
    -> Diff_Contacts :: {Contacts    :: [] | contacts(),
                         Subscribers :: [Entity::xmpp_jid:usr_bare()]}
).

subscriptions5(NodeIdx, [_Roster | Rosters], Node_Groups, {Contacts, Subscribers})
  when _Roster#roster.groups =/= [] ->
    subscriptions5(NodeIdx, Rosters, Node_Groups,
        _Diff_Contacts = case
            filter_groups(_Roster#roster.groups, Node_Groups, [])
        of
            [] ->
                {_Contacts    = Contacts,
                 _Subscribers = Subscribers};
            Groups ->
                {_Contacts    = case
                     lists:keyfind(_Roster#roster.jid, 1, Contacts)
                 of
                     false ->
                         [{_Roster#roster.jid, [{NodeIdx, Groups}]} | Contacts];
                     {Entity, Subscriptions} ->
                         lists:keyreplace(Entity, 1, Contacts,
                             {Entity, [{NodeIdx, Groups} | Subscriptions]})
                 end,
                 _Subscribers = [_Roster#roster.jid | Subscribers]}
        end);
%%
subscriptions5(NodeIdx, [_Roster | Rosters], Node_Groups, Diff_Contacts) ->
    subscriptions5(NodeIdx, Rosters, Node_Groups, Diff_Contacts);
%%
subscriptions5(NodeIdx, Owner, Node_Groups, Contacts) ->
    subscriptions5(NodeIdx, _Rosters = get_entity_roster(Owner), Node_Groups,
        _Diff_Contacts = {Contacts, _Subscribers = []}).


%%
-spec(delete_groups/2 ::
(
  Old_Groups :: roster_groups(),
  Groups     :: [] | roster_groups())
    -> Groups :: [] | roster_groups()
).

delete_groups(_Old_Groups, [] = _Groups) ->
    _Groups = [];
%%
delete_groups([] = _Old_Groups, Groups) ->
    Groups;
%%
delete_groups([Old_Group | Old_Groups], Groups) ->
    _Groups = delete_groups(Old_Groups, lists:delete(Old_Group, Groups)).


%%
-spec(diff_groups/2 ::
(
  Groups :: node_groups() | [],
  _      :: node_groups()
          | {New_Groups :: []  | node_groups(),
             Old_Groups :: []  | node_groups()})
    -> Diff_Groups :: {New_Groups :: []  | node_groups(),
                       Old_Groups :: []  | node_groups()}
).

diff_groups([] = _Groups, Diff_Groups) -> %% when is_tuple(Diff_Groups)
    Diff_Groups;
%%
diff_groups([Group | Groups], {New_Groups, Old_Groups}) ->
    diff_groups(Groups,
        _Diff_Groups = case lists:delete(Group, Old_Groups) of
            Old_Groups  -> {[Group | New_Groups], Old_Groups};
            Old_Groups2 -> {New_Groups, Old_Groups2}
        end);
%%
diff_groups(New_Groups, Old_Groups) ->
    diff_groups(New_Groups, {[], Old_Groups}).


%%
-spec(filter_groups/3 ::
(
  Roster_Groups :: roster_groups() | [],
  Node_Groups   :: node_groups(),
  Groups        :: [] | roster_groups())
    -> Groups :: [] | roster_groups()
).

filter_groups([] = _Roster_Groups, _Node_Groups, Groups) ->
    Groups;
%%
filter_groups([Roster_Group | Roster_Groups], Node_Groups, Groups) ->
    filter_groups(Roster_Groups, Node_Groups,
        _Groups = case lists:member(Roster_Group, Node_Groups) of
            true  -> [Roster_Group | Groups];
            false -> Groups
        end). 


%%
-spec(filter_subscriptions/2 ::
(
  Nodes         :: n0des(),
  Roster_Groups :: roster_groups())
    -> Subscriptions :: [] | subscriptions()
).

filter_subscriptions(Nodes, Roster_Groups) ->
    _Subscriptions = filter_subscriptions(Nodes, Roster_Groups, []).

%%
-spec(filter_subscriptions/3 ::
(
  Nodes         :: n0des(),
  Roster_Groups :: roster_groups(),
  Subscriptions :: [] | subscriptions())
    -> Subscriptions :: [] | subscriptions()
).

filter_subscriptions([] = _Nodes, _Roster_Groups, Subscriptions) ->
    Subscriptions;
%%
filter_subscriptions([{NodeIdx, Node_Groups} | Nodes], Roster_Groups,
  Subscriptions) ->
    filter_subscriptions(Nodes, Roster_Groups,
        _Subscriptions = case filter_groups(Roster_Groups, Node_Groups, []) of
            []     -> Subscriptions;
            Groups -> [{NodeIdx, Groups} | Subscriptions]
        end).


%%
-spec(diff_nodeidxs/2 ::
(
  New_Subscriptions :: [] | subscriptions(),
  Old_Subscriptions :: [] | subscriptions())
    -> Diff_NodeIdxs :: {NodeIdxs_Subscriptions   :: [] | nodeIdxs(),
                         NodeIdxs_Unsubscriptions :: [] | nodeIdxs()}
).

diff_nodeidxs(New_Subscriptions, Old_Subscriptions) ->
    _Diff_NodeIdxs = diff_nodeidxs(New_Subscriptions, Old_Subscriptions, []).

%%
-spec(diff_nodeidxs/3 ::
(
  New_Subscriptions      :: [] | subscriptions(),
  Old_Subscriptions      :: [] | subscriptions(),
  NodeIdxs_Subscriptions :: [] | nodeIdxs())
    -> Diff_NodeIdxs :: {NodeIdxs_Subscriptions   :: [] | nodeIdxs(),
                         NodeIdxs_Unsubscriptions :: [] | nodeIdxs()}
).

diff_nodeidxs([] = _New_Subscriptions, Old_Subscriptions,
  NodeIdxs_Subscriptions) ->
    _Diff_NodeIdxs = {
        _NodeIdxs_Subscriptions   = NodeIdxs_Subscriptions,
        _NodeIdxs_Unsubscriptions = lists:map(fun
            ({NodeIdx, _Roster_Groups}) ->
               NodeIdx
         end, Old_Subscriptions)};
%%
diff_nodeidxs([{NodeIdx, _Groups} | New_Subscriptions], [] = Old_Subscriptions,
  NodeIdxs_Subscriptions) ->
    diff_nodeidxs(New_Subscriptions, Old_Subscriptions,
        [NodeIdx | NodeIdxs_Subscriptions]);
%%
diff_nodeidxs([{NodeIdx, _Groups} | New_Subscriptions], Old_Subscriptions,
  NodeIdxs_Subscriptions) ->
    case lists:keydelete(NodeIdx, 1, Old_Subscriptions) of
        Old_Subscriptions ->
            diff_nodeidxs(New_Subscriptions, Old_Subscriptions,
                [NodeIdx | NodeIdxs_Subscriptions]);
        Subscriptions ->
            diff_nodeidxs(New_Subscriptions, _Old_Subscriptions = Subscriptions,
                NodeIdxs_Subscriptions)
    end.


%% TODO : use a new mod_roster_api for this call
-spec(roster_groups/2 ::
(
  Entity  :: xmpp_jid:usr_bare(),
  Contact :: xmpp_jid:usr_bare())
    -> Roster_Groups :: [] | roster_groups()
).

roster_groups({User, Server, _} = _Entity, Contact) ->   
    _Roster_Groups = case
        gen_storage:transaction(Server, rosteritem,
            fun() ->
                gen_storage:select(Server, rostergroup,
                    [{'=', user_host_jid, {User, Server, Contact}}])
            end)
    of
        {atomic, _RosterGroups} ->
            lists:map(fun
                (_RosterGroup) ->
                    _RosterGroup#roster.groups
            end, _RosterGroups);
        _Error
            -> {error, 'internal-server-error'}
    end.


%%
-spec(select_roster_groups/1 ::
(
  Entity :: xmpp_jid:usr_entity())
    -> Roster_Groups :: [] | roster_groups()
).

select_roster_groups({User, Server, _} = Entity) ->
    _Roster_Groups = case
        gen_storage:transaction(Server, rosteritem,
            fun() ->
                gen_storage:select(Server, rostergroup,
                    [{'=', user_host_jid, {User, Server, '_'}}])
            end)
    of
        {atomic, _RosterGroups} ->
            lists:map(fun
                (_RosterGroup) ->
                    _RosterGroup#roster.groups
            end, _RosterGroups);
        _Error
            -> {error, 'internal-server-error'}
    end.


%% TESTS

%%
monitor_roster_groups(Server, Roster) ->
    ?INFO_MSG("SERVER ~p ROSTER ~p", [Server, Roster]),
    Result = pubsub_db:transaction('mnesia', ?MODULE, monitor_groups,
        [Server, Roster]),
    ?INFO_MSG("MONITOR ROSTER GROUPS ~p", [Result]).


monitor_contacts2('subscribed', Entity, Jid_Contact) ->
    Result = pubsub_db:transaction('mnesia', ?MODULE, monitor_contacts,
        ['subscribed', Entity, Jid_Contact]),
    ?INFO_MSG("MONITOR CONTACTS2 ~p", [Result]).

register1(NodeIdx, User, Groups) ->
    Server = <<"localhost">>,
    Owner = {list_to_binary(atom_to_list(User)), Server, undefined},
    Groups_B = lists:map(fun(Group) -> list_to_binary(atom_to_list(Group)) end, Groups),
    register_groups(dev, NodeIdx, Owner, Groups_B).

register(NodeIdx, User, Groups) ->
    pubsub_db:transaction('mnesia', ?MODULE, register1, [NodeIdx, User, Groups]).
