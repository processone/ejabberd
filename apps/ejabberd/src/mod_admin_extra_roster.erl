%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra_roster.erl
%%% Author  : Badlop <badlop@process-one.net>, Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module(mod_admin_extra_roster).
-author('badlop@process-one.net').

-export([
    commands/0,

    add_rosteritem/7,
    delete_rosteritem/4,
    process_rosteritems/5,
    get_roster/2,
    push_roster/3,
    push_roster_all/1,
    push_alltoall/2
    ]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("mod_roster.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

%%%
%%% Register commands
%%%

commands() ->
    [
        #ejabberd_commands{name = add_rosteritem, tags = [roster],
                           desc = "Add an item to a user's roster (supports ODBC)",
                           module = ?MODULE, function = add_rosteritem,
                           args = [{localuser, binary}, {localserver, binary},
                                   {user, binary}, {server, binary},
                                   {nick, binary}, {group, binary},
                                   {subs, binary}],
                           result = {res, rescode}},
        %%{"", "subs= none, from, to or both"},
        %%{"", "example: add-roster peter localhost mike server.com MiKe Employees both"},
        %%{"", "will add mike@server.com to peter@localhost roster"},
        #ejabberd_commands{name = delete_rosteritem, tags = [roster],
                           desc = "Delete an item from a user's roster (supports ODBC)",
                           module = ?MODULE, function = delete_rosteritem,
                           args = [{localuser, binary}, {localserver, binary},
                                   {user, binary}, {server, binary}],
                           result = {res, rescode}},
        #ejabberd_commands{name = process_rosteritems, tags = [roster],
                           desc = "List or delete rosteritems that match filtering options (Mnesia only!)",
                           longdesc = "Explanation of each argument:\n"
                           " - action: what to do with each rosteritem that "
                           "matches all the filtering options\n"
                           " - subs: subscription type\n"
                           " - asks: pending subscription\n"
                           " - users: the JIDs of the local user\n"
                           " - contacts: the JIDs of the contact in the roster\n"
                           "\n"
                           "Allowed values in the arguments:\n"
                           "  ACTION = list | delete\n"
                           "  SUBS = SUB[:SUB]* | any\n"
                           "  SUB = none | from | to | both\n"
                           "  ASKS = ASK[:ASK]* | any\n"
                           "  ASK = none | out | in\n"
                           "  USERS = JID[:JID]* | any\n"
                           "  CONTACTS = JID[:JID]* | any\n"
                           "  JID = characters valid in a JID, and can use the "
                           "Regular expression syntax: http://www.erlang.org/doc/man/re.html#id212737\n"
                           "\n"
                           "This example will list roster items with subscription "
                           "'none', 'from' or 'to' that have any ask property, of "
                           "local users which JID is in the virtual host "
                           "'example.org' and that the contact JID is either a "
                           "bare server name (without user part) or that has a "
                           "user part and the server part contains the word 'icq'"
                           ":\n  list none:from:to any *@example.org *:*@*icq*",
                           module = ?MODULE, function = process_rosteritems,
                           args = [{action, string}, {subs, string},
                                   {asks, string}, {users, string},
                                   {contacts, string}],
                           result = {res, rescode}},
        #ejabberd_commands{name = get_roster, tags = [roster],
                           desc = "Get roster of a local user",
                           module = ?MODULE, function = get_roster,
                           args = [{user, binary}, {host, binary}],
                           result = {contacts, {list, {contact, {tuple, [
                                {jid, binary},
                                {nick, binary},
                                {subscription, binary},
                                {ask, binary},
                                {group, binary}
                                ]}}}}},
        #ejabberd_commands{name = push_roster, tags = [roster],
                           desc = "Push template roster from file to a user",
                           module = ?MODULE, function = push_roster,
                           args = [{file, string}, {user, binary}, {host, binary}],
                           result = {res, rescode}},
        #ejabberd_commands{name = push_roster_all, tags = [roster],
                           desc = "Push template roster from file to all those users",
                           module = ?MODULE, function = push_roster_all,
                           args = [{file, string}],
                           result = {res, rescode}},
        #ejabberd_commands{name = push_roster_alltoall, tags = [roster],
                           desc = "Add all the users to all the users of Host in Group",
                           module = ?MODULE, function = push_alltoall,
                           args = [{host, string}, {group, binary}],
                           result = {res, rescode}}
        ].

%%%
%%% Roster
%%%

add_rosteritem(LocalUser, LocalServer, User, Server, Nick, Group, Subs) ->
    case subscribe(LocalUser, LocalServer, User, Server, Nick, Group, Subs, []) of
        {atomic, ok} ->
            push_roster_item(LocalUser, LocalServer, User, Server, {add, Nick, Subs, Group}),
            ok;
        _ ->
            error
    end.

subscribe(LU, LS, User, Server, Nick, Group, SubscriptionS, _Xattrs) ->
    ItemEl = build_roster_item(User, Server, {add, Nick, SubscriptionS, Group}),
    {ok, M} = loaded_module(LS,[mod_roster_odbc,mod_roster]),
    M:set_items(
        LU, LS,
        #xmlel{ name = <<"query">>,
               attrs = [{<<"xmlns">>,<<"jabber:iq:roster">>}],
               children = [ItemEl]}).

delete_rosteritem(LocalUser, LocalServer, User, Server) ->
    case unsubscribe(LocalUser, LocalServer, User, Server) of
        {atomic, ok} ->
            push_roster_item(LocalUser, LocalServer, User, Server, remove),
            ok;
        _  ->
            error
    end.

unsubscribe(LU, LS, User, Server) ->
    ItemEl = build_roster_item(User, Server, remove),
    {ok, M} = loaded_module(LS,[mod_roster_odbc,mod_roster]),
    M:set_items(
        LU, LS,
        #xmlel{ name = <<"query">>,
               attrs = [{<<"xmlns">>,<<"jabber:iq:roster">>}],
               children = [ItemEl]}).

loaded_module(Domain,Options) ->
    LoadedModules = gen_mod:loaded_modules(Domain),
    case lists:filter(fun(Module) ->
                    lists:member(Module, LoadedModules)
            end, Options) of
        [M|_] -> {ok, M};
        [] -> {error,not_found}
    end.

%% -----------------------------
%% Get Roster
%% -----------------------------

get_roster(User, Server) ->
    Items = ejabberd_hooks:run_fold(roster_get, Server, [], [{User, Server}]),
    make_roster_xmlrpc(Items).

%% Note: if a contact is in several groups, the contact is returned
%% several times, each one in a different group.
make_roster_xmlrpc(Roster) ->
    lists:foldl(
        fun(Item, Res) ->
                JIDS = jlib:jid_to_binary(Item#roster.jid),
                Nick = Item#roster.name,
                Subs = atom_to_list(Item#roster.subscription),
                Ask = atom_to_list(Item#roster.ask),
                Groups = case Item#roster.groups of
                    [] -> [""];
                    Gs -> Gs
                end,
                ItemsX = [{JIDS, Nick, Subs, Ask, Group}
                          || Group <- Groups],
                ItemsX ++ Res
        end,
        [],
        Roster).


%%-----------------------------
%% Push Roster from file
%%-----------------------------

push_roster(File, User, Server) ->
    {ok, [Roster]} = file:consult(File),
    subscribe_roster({User, Server, "", User}, roster_list_to_binary(Roster)).

push_roster_all(File) ->
    {ok, [Roster]} = file:consult(File),
    subscribe_all(roster_list_to_binary(Roster)).

roster_list_to_binary(Roster) ->
    [ {
            list_to_binary(Usr),
            list_to_binary(Srv),
            list_to_binary(Grp),
            list_to_binary(Nick) } || {Usr, Srv, Grp, Nick} <- Roster ].

subscribe_all(Roster) ->
    subscribe_all(Roster, Roster).
subscribe_all([], _) ->
    ok;
subscribe_all([User1 | Users], Roster) ->
    subscribe_roster(User1, Roster),
    subscribe_all(Users, Roster).

subscribe_roster(_, []) ->
    ok;
%% Do not subscribe a user to itself
subscribe_roster({Name, Server, Group, Nick}, [{Name, Server, _, _} | Roster]) ->
    subscribe_roster({Name, Server, Group, Nick}, Roster);
%% Subscribe Name2 to Name1
subscribe_roster({Name1, Server1, Group1, Nick1}, [{Name2, Server2, Group2, Nick2} | Roster]) ->
    subscribe(Name1, Server1, Name2, Server2, Nick2, Group2, <<"both">>, []),
    subscribe_roster({Name1, Server1, Group1, Nick1}, Roster).

push_alltoall(S, G) ->
    Users = ejabberd_auth:get_vh_registered_users(S),
    Users2 = build_list_users(G, Users, []),
    subscribe_all(Users2),
    ok.

build_list_users(_Group, [], Res) ->
    Res;
build_list_users(Group, [{User, Server}|Users], Res) ->
    build_list_users(Group, Users, [{User, Server, Group, User}|Res]).

%% @spec(LU, LS, U, S, Action) -> ok
%%       Action = {add, Nick, Subs, Group} | remove
%% @doc Push to the roster of account LU@LS the contact U@S.
%% The specific action to perform is defined in Action.
push_roster_item(LU, LS, U, S, Action) ->
    lists:foreach(fun(R) ->
                push_roster_item(LU, LS, R, U, S, Action)
        end, ejabberd_sm:get_user_resources(LU, LS)).

push_roster_item(LU, LS, R, U, S, Action) ->
    LJID = jlib:make_jid(LU, LS, R),
    BroadcastEl = build_broadcast(U, S, Action),
    ejabberd_router:route(LJID, LJID, BroadcastEl),
    Item = build_roster_item(U, S, Action),
    ResIQ = build_iq_roster_push(Item),
    ejabberd_router:route(LJID, LJID, ResIQ).

build_roster_item(U, S, {add, Nick, Subs, Group}) ->
    #xmlel{ name = <<"item">>,
           attrs = [{<<"jid">>, jlib:jid_to_binary(jlib:make_jid(U, S, <<"">>))},
                    {<<"name">>, Nick},
                    {<<"subscription">>, Subs}],
           children = [#xmlel{ name = <<"group">>, children = [#xmlcdata{content = Group}]}]
          };
build_roster_item(U, S, remove) ->
    #xmlel{ name = <<"item">>,
           attrs = [{<<"jid">>, jlib:jid_to_binary(jlib:make_jid(U, S, <<"">>))},
                    {<<"subscription">>, <<"remove">>}]}.

build_iq_roster_push(Item) ->
    #xmlel{ name = <<"iq">>,
           attrs = [{<<"type">>, <<"set">>}, {<<"id">>, <<"push">>}],
           children = [#xmlel{ name = <<"query">>, attrs = [{<<"xmlns">>, ?NS_ROSTER}], children = [Item]}] }.

build_broadcast(U, S, {add, _Nick, Subs, _Group}) ->
    build_broadcast(U, S, list_to_existing_atom(binary_to_list(Subs)));
build_broadcast(U, S, remove) ->
    build_broadcast(U, S, none);
%% @spec (U::binary(), S::binary(), Subs::atom()) -> any()
%% Subs = both | from | to | none
build_broadcast(U, S, SubsAtom) when is_atom(SubsAtom) ->
    #xmlel{ name = <<"broadcast">>, children = [{item, {U, S, ""}, SubsAtom}] }.

%%-----------------------------
%% Purge roster items
%%-----------------------------

process_rosteritems(ActionS, SubsS, AsksS, UsersS, ContactsS) ->
    Action = case ActionS of
        "list" -> list;
        "delete" -> delete
    end,

    Subs = lists:foldl(
            fun(any, _) -> [none, from, to, both];
                (Sub, Subs) -> [Sub | Subs]
            end,
            [],
            [list_to_atom(S) || S <- string:tokens(SubsS, ":")]
            ),

    Asks = lists:foldl(
            fun(any, _) -> [none, out, in];
                (Ask, Asks) -> [Ask | Asks]
            end,
            [],
            [list_to_atom(S) || S <- string:tokens(AsksS, ":")]
            ),

    Users = lists:foldl(
            fun(<<"any">>, _) -> [<<".*">>, <<".*@.*">>];
                (U, Us) -> [U | Us]
            end,
            [],
            [list_to_binary(S) || S <- string:tokens(UsersS, ":")]
            ),

    Contacts = lists:foldl(
            fun(<<"any">>, _) -> [<<".*">>, <<".*@.*">>];
                (U, Us) -> [U | Us]
            end,
            [],
            [list_to_binary(S) || S <- string:tokens(ContactsS, ":")]
            ),

    case rosteritem_purge({Action, Subs, Asks, Users, Contacts}) of
        {atomic, ok} ->
            ok;
        {error, Reason} ->
            io:format("Error purging rosteritems: ~p~n", [Reason]),
            error;
        {badrpc, Reason} ->
            io:format("BadRPC purging rosteritems: ~p~n", [Reason]),
            error
    end.

%% @spec ({Action::atom(), Subs::[atom()], Asks::[atom()], User::binary(), Contact::binary()}) -> {atomic, ok}
rosteritem_purge(Options) ->
    Num_rosteritems = mnesia:table_info(roster, size),
    io:format("There are ~p roster items in total.~n", [Num_rosteritems]),
    Key = mnesia:dirty_first(roster),
    ok = rip(Key, Options, {0, Num_rosteritems, 0, 0}),
    {atomic, ok}.

rip('$end_of_table', _Options, Counters) ->
    print_progress_line(Counters),
    ok;
rip(Key, Options, {Pr, NT, NV, ND}) ->
    Key_next = mnesia:dirty_next(roster, Key),
    {Action, _, _, _, _} = Options,
    ND2 = case decide_rip(Key, Options) of
        true ->
            apply_action(Action, Key),
            ND+1;
        false ->
            ND
    end,
    NV2 = NV+1,
    Pr2 = print_progress_line({Pr, NT, NV2, ND2}),
    rip(Key_next, Options, {Pr2, NT, NV2, ND2}).

apply_action(list, Key) ->
    {User, Server, JID} = Key,
    {RUser, RServer, _} = JID,
    io:format("Matches: ~s@~s ~s@~s~n", [User, Server, RUser, RServer]);
apply_action(delete, Key) ->
    apply_action(list, Key),
    mnesia:dirty_delete(roster, Key).

print_progress_line({Pr, NT, NV, ND}) ->
    Pr2 = trunc((NV/NT)*100),
    case Pr == Pr2 of
        true ->
            ok;
        false ->
            io:format("Progress ~p% - visited ~p - deleted ~p~n", [Pr2, NV, ND])
    end,
    Pr2.

decide_rip(Key, {_Action, Subs, Asks, User, Contact}) ->
    case catch mnesia:dirty_read(roster, Key) of
        [RI] ->
            lists:member(RI#roster.subscription, Subs)
            andalso lists:member(RI#roster.ask, Asks)
            andalso decide_rip_jid(RI#roster.us, User)
            andalso decide_rip_jid(RI#roster.jid, Contact);
        _ ->
            false
    end.

%% Returns true if the server of the JID is included in the servers
decide_rip_jid({UName, UServer, _UResource}, MatchList) ->
    decide_rip_jid({UName, UServer}, MatchList);
decide_rip_jid({UName, UServer}, MatchList) ->
    lists:any(
        fun(MatchString) ->
                MJID = jlib:binary_to_jid(MatchString),
                MName = MJID#jid.luser,
                MServer = MJID#jid.lserver,
                IsServer = is_regexp_match(UServer, MServer),
                case MName of
                    [] when UName == [] ->
                        IsServer;
                    [] ->
                        false;
                    _ ->
                        IsServer
                        andalso is_regexp_match(UName, MName)
                end
        end,
        MatchList).

is_regexp_match(String, RegExp) ->
    case re:run(String, RegExp) of
        nomatch ->
            false;
        {match, _} ->
            true;
        {error, ErrDesc} ->
            io:format("Wrong regexp ~p: ~p", [RegExp, ErrDesc]),
            false
    end.
