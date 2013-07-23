%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra_sessions.erl
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

-module(mod_admin_extra_sessions).
-author('badlop@process-one.net').


-export([
    commands/0,

    num_resources/2,
    resource_num/3,
    kick_session/4,
    status_num/2, status_num/1,
    status_list/2, status_list/1,
    connected_users_info/0,
    connected_users_vhost/1,
    set_presence/7,
    user_sessions_info/2
    ]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

%%%
%%% Register commands
%%%

commands() ->
    [
        #ejabberd_commands{name = num_resources, tags = [session],
                           desc = "Get the number of resources of a user",
                           module = ?MODULE, function = num_resources,
                           args = [{user, string}, {host, string}],
                           result = {resources, integer}},
        #ejabberd_commands{name = resource_num, tags = [session],
                           desc = "Resource string of a session number",
                           module = ?MODULE, function = resource_num,
                           args = [{user, string}, {host, string}, {num, integer}],
                           result = {resource, string}},
        #ejabberd_commands{name = kick_session, tags = [session],
                           desc = "Kick a user session",
                           module = ?MODULE, function = kick_session,
                           args = [{user, string}, {host, string}, {resource, string}, {reason, string}],
                           result = {res, rescode}},
        #ejabberd_commands{name = status_num_host, tags = [session, stats],
                           desc = "Number of logged users with this status in host",
                           module = ?MODULE, function = status_num,
                           args = [{host, string}, {status, string}],
                           result = {users, integer}},
        #ejabberd_commands{name = status_num, tags = [session, stats],
                           desc = "Number of logged users with this status",
                           module = ?MODULE, function = status_num,
                           args = [{status, string}],
                           result = {users, integer}},
        #ejabberd_commands{name = status_list_host, tags = [session],
                           desc = "List of users logged in host with their statuses",
                           module = ?MODULE, function = status_list,
                           args = [{host, string}, {status, string}],
                           result = {users, {list,
                                             {userstatus, {tuple, [
                                {user, string},
                                {host, string},
                                {resource, string},
                                {priority, integer},
                                {status, string}
                                ]}}
                                            }}},
        #ejabberd_commands{name = status_list, tags = [session],
                           desc = "List of logged users with this status",
                           module = ?MODULE, function = status_list,
                           args = [{status, string}],
                           result = {users, {list,
                                             {userstatus, {tuple, [
                                {user, string},
                                {host, string},
                                {resource, string},
                                {priority, integer},
                                {status, string}
                                ]}}
                                            }}},
        #ejabberd_commands{name = connected_users_info,
                           tags = [session],
                           desc = "List all established sessions and their information",
                           module = ?MODULE, function = connected_users_info,
                           args = [],
                           result = {connected_users_info,
                                     {list,
                                      {sessions, {tuple,
                                                  [{jid, string},
                                                   {connection, string},
                                                   {ip, string},
                                                   {port, integer},
                                                   {priority, integer},
                                                   {node, string},
                                                   {uptime, integer}
                                                  ]}}
                                     }}},
        #ejabberd_commands{name = connected_users_vhost,
                           tags = [session],
                           desc = "Get the list of established sessions in a vhost",
                           module = ?MODULE, function = connected_users_vhost,
                           args = [{host, string}],
                           result = {connected_users_vhost, {list, {sessions, string}}}},
        #ejabberd_commands{name = user_sessions_info,
                           tags = [session],
                           desc = "Get information about all sessions of a user",
                           module = ?MODULE, function = user_sessions_info,
                           args = [{user, string}, {host, string}],
                           result = {sessions_info,
                                     {list,
                                      {session, {tuple,
                                                 [{connection, string},
                                                  {ip, string},
                                                  {port, integer},
                                                  {priority, integer},
                                                  {node, string},
                                                  {uptime, integer},
                                                  {status, string},
                                                  {resource, string},
                                                  {statustext, string}
                                                 ]}}
                                     }}},

        #ejabberd_commands{name = set_presence,
                           tags = [session],
                           desc = "Set presence of a session",
                           module = ?MODULE, function = set_presence,
                           args = [{user, string}, {host, string},
                                   {resource, string}, {type, string},
                                   {show, string}, {status, string},
                                   {priority, string}],
                           result = {res, rescode}}
        ].

%%%
%%% Sessions
%%%

num_resources(User, Host) ->
    length(ejabberd_sm:get_user_resources(User, Host)).

resource_num(User, Host, Num) ->
    Resources = ejabberd_sm:get_user_resources(User, Host),
    case (0<Num) and (Num=<length(Resources)) of
        true ->
            lists:nth(Num, Resources);
        false ->
            lists:flatten(io_lib:format("Error: Wrong resource number: ~p", [Num]))
    end.

kick_session(User, Server, Resource, ReasonText) ->
    mod_admin_extra_common:kick_session(User, Server, Resource, ReasonText).

status_num(Host, Status) ->
    length(get_status_list(Host, Status)).
status_num(Status) ->
    status_num(all, Status).
status_list(Host, Status) ->
    Res = get_status_list(Host, Status),
    [{U, S, R, P, St} || {U, S, R, P, St} <- Res].
status_list(Status) ->
    status_list(all, Status).


get_status_list(Host, StatusRequired) ->
    %% Get list of all logged users
    Sessions = ejabberd_sm:dirty_get_my_sessions_list(),
    %% Reformat the list
    Sessions2 = [ {Session#session.usr, Session#session.sid, Session#session.priority} || Session <- Sessions],
    Fhost = case Host of
        all ->
            %% All hosts are requested, so dont filter at all
            fun(_, _) -> true end;
        _ ->
            %% Filter the list, only Host is interesting
            fun(A, B) -> A == B end
    end,
    Sessions3 = [ {Pid, Server, Priority} || {{_User, Server, _Resource}, {_, Pid}, Priority} <- Sessions2, apply(Fhost, [Server, Host])],
    %% For each Pid, get its presence
    Sessions4 = [ {ejabberd_c2s:get_presence(Pid), Server, Priority} || {Pid, Server, Priority} <- Sessions3],
    %% Filter by status
    Fstatus = case StatusRequired of
        all ->
            fun(_, _) -> true end;
        _ ->
            fun(A, B) -> A == B end
    end,
    [{User, Server, Resource, Priority, StatusText}
     || {{User, Resource, Status, StatusText}, Server, Priority} <- Sessions4,
        apply(Fstatus, [Status, StatusRequired])].

connected_users_info() ->
    USRIs = ejabberd_sm:get_full_session_list(),
    CurrentSec = calendar:datetime_to_gregorian_seconds({date(), time()}),
    lists:map(
        fun({{U, S, R}, {Now, Pid}, Priority, Info}) ->
                Conn = proplists:get_value(conn, Info),
                {Ip, Port} = proplists:get_value(ip, Info),
                IPS = inet_parse:ntoa(Ip),
                NodeS = atom_to_list(node(Pid)),
                Uptime = CurrentSec - calendar:datetime_to_gregorian_seconds(
                        calendar:now_to_local_time(Now)),
                {[U, $@, S, $/, R], atom_to_list(Conn), IPS, Port, Priority, NodeS, Uptime}
        end,
        USRIs).

connected_users_vhost(Host) ->
    USRs = ejabberd_sm:get_vh_session_list(list_to_binary(Host)),
    [ [U, $@, S, $/, R] || {U, S, R} <- USRs].

set_presence(User, Host, Resource, Type, Show, Status, Priority) ->
    Pid = ejabberd_sm:get_session_pid(User, Host, Resource),
    USR = User ++ "@" ++ Host ++ "/" ++ Resource,
    US = User ++ "@" ++ Host,
    Message = {route_xmlstreamelement,
               #xmlel{ name = "presence",
                      attrs = [{"from", USR}, {"to", US}, {"type", Type}],
                      children = [#xmlel{ name = "show", children = [#xmlcdata{content = Show}]},
                                  #xmlel{ name = "status", children = [#xmlcdata{content = Status}]},
                                  #xmlel{ name = "priority", children = [#xmlcdata{content = Priority}]}]}},
    Pid ! Message.

user_sessions_info(User, Host) ->
    CurrentSec = calendar:datetime_to_gregorian_seconds({date(), time()}),
    US = {User, Host},
    Sessions = case catch mnesia:dirty_index_read(session, US, #session.us) of
        {'EXIT', _Reason} ->
            [];
        Ss ->
            Ss
    end,
    lists:map(
        fun(Session) ->
                {_U, _S, Resource} = Session#session.usr,
                {Now, Pid} = Session#session.sid,
                {_U, _Resource, Status, StatusText} = ejabberd_c2s:get_presence(Pid),
                Info = Session#session.info,
                Priority = Session#session.priority,
                Conn = proplists:get_value(conn, Info),
                {Ip, Port} = proplists:get_value(ip, Info),
                IPS = inet_parse:ntoa(Ip),
                NodeS = atom_to_list(node(Pid)),
                Uptime = CurrentSec - calendar:datetime_to_gregorian_seconds(
                        calendar:now_to_local_time(Now)),
                {atom_to_list(Conn), IPS, Port, Priority, NodeS, Uptime, Status, Resource, StatusText}
        end,
        Sessions).

