%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra_srg.erl
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

-module(mod_admin_extra_srg).
-author('badlop@process-one.net').

-export([
    commands/0,

    srg_create/5,
    srg_delete/2,
    srg_list/1,
    srg_get_info/2,
    srg_get_members/2,
    srg_user_add/4,
    srg_user_del/4
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
        #ejabberd_commands{name = srg_create, tags = [shared_roster_group],
                           desc = "Create a Shared Roster Group",
                           longdesc = "If you want to specify several group "
                           "identifiers in the Display argument,\n"
                           "put  \\ \" around the argument and\nseparate the "
                           "identifiers with \\ \\ n\n"
                           "For example:\n"
                           "  ejabberdctl srg_create group3 localhost "
                           "name desc \\\"group1\\\\ngroup2\\\"",
                           module = ?MODULE, function = srg_create,
                           args = [{group, binary}, {host, binary},
                                   {name, binary}, {description, binary}, {display, binary}],
                           result = {res, rescode}},
        #ejabberd_commands{name = srg_delete, tags = [shared_roster_group],
                           desc = "Delete a Shared Roster Group",
                           module = ?MODULE, function = srg_delete,
                           args = [{group, binary}, {host, binary}],
                           result = {res, rescode}},
        #ejabberd_commands{name = srg_list, tags = [shared_roster_group],
                           desc = "List the Shared Roster Groups in Host",
                           module = ?MODULE, function = srg_list,
                           args = [{host, binary}],
                           result = {groups, {list, {id, string}}}},
        #ejabberd_commands{name = srg_get_info, tags = [shared_roster_group],
                           desc = "Get info of a Shared Roster Group",
                           module = ?MODULE, function = srg_get_info,
                           args = [{group, binary}, {host, binary}],
                           result = {informations, {list, {information, {tuple, [{key, string}, {value, string}]}}}}},
        #ejabberd_commands{name = srg_get_members, tags = [shared_roster_group],
                           desc = "Get members of a Shared Roster Group",
                           module = ?MODULE, function = srg_get_members,
                           args = [{group, binary}, {host, binary}],
                           result = {members, {list, {member, string}}}},
        #ejabberd_commands{name = srg_user_add, tags = [shared_roster_group],
                           desc = "Add the JID user@host to the Shared Roster Group",
                           module = ?MODULE, function = srg_user_add,
                           args = [{user, binary}, {host, binary}, {group, binary}, {grouphost, binary}],
                           result = {res, rescode}},
        #ejabberd_commands{name = srg_user_del, tags = [shared_roster_group],
                           desc = "Delete this JID user@host from the Shared Roster Group",
                           module = ?MODULE, function = srg_user_del,
                           args = [{user, binary}, {host, binary}, {group, binary}, {grouphost, binary}],
                           result = {res, rescode}}
        ].

%%%
%%% Shared Roster Groups
%%%

srg_create(Group, Host, Name, Description, Display) ->
    DisplayList = case Display of
        [] -> [];
        _ -> binary:split(Display, <<"\\\\n">>)
    end,
    Opts = [{name, Name},
            {displayed_groups, DisplayList},
            {description, Description}],
    {atomic, ok} = mod_shared_roster:create_group(Host, Group, Opts),
    ok.

srg_delete(Group, Host) ->
    {atomic, ok} = mod_shared_roster:delete_group(Host, Group),
    ok.

srg_list(Host) ->
    lists:sort(mod_shared_roster:list_groups(Host)).

srg_get_info(Group, Host) ->
    Opts = mod_shared_roster:get_group_opts(Host,Group),
    [{io_lib:format("~p", [Title]),
      io_lib:format("~p", [Value])} || {Title, Value} <- Opts].

srg_get_members(Group, Host) ->
    Members = mod_shared_roster:get_group_explicit_users(Host,Group),
    [jlib:jid_to_binary(jlib:make_jid(MUser, MServer, <<"">>))
     || {MUser, MServer} <- Members].

srg_user_add(User, Host, Group, GroupHost) ->
    {atomic, ok} = mod_shared_roster:add_user_to_group(GroupHost, {User, Host}, Group),
    ok.

srg_user_del(User, Host, Group, GroupHost) ->
    {atomic, ok} = mod_shared_roster:remove_user_from_group(GroupHost, {User, Host}, Group),
    ok.

