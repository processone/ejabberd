%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra_stats.erl
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

-module(mod_admin_extra_stats).
-author('badlop@process-one.net').


-export([
    commands/0,

    stats/1, stats/2
    ]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").

%%%
%%% Register commands
%%%

commands() ->
    [
        #ejabberd_commands{name = stats, tags = [stats],
                           desc = "Get statistical value: registeredusers onlineusers onlineusersnode uptimeseconds",
                           module = ?MODULE, function = stats,
                           args = [{name, binary}],
                           result = {stat, integer}},
        #ejabberd_commands{name = stats_host, tags = [stats],
                           desc = "Get statistical value for this host: registeredusers onlineusers",
                           module = ?MODULE, function = stats,
                           args = [{name, binary}, {host, binary}],
                           result = {stat, integer}}
        ].

%%%
%%% Stats
%%%

stats(Name) ->
    case Name of
        <<"uptimeseconds">> -> trunc(element(1, erlang:statistics(wall_clock))/1000);
        <<"registeredusers">> -> lists:sum([
                    ejabberd_auth:get_vh_registered_users_number(Server)
                    || Server <- ejabberd_config:get_global_option(hosts) ]);
        <<"onlineusersnode">> -> ejabberd_sm:get_node_sessions_number();
        <<"onlineusers">> -> ejabberd_sm:get_total_sessions_number()
    end.

stats(Name, Host) ->
    case Name of
        <<"registeredusers">> -> ejabberd_auth:get_vh_registered_users_number(Host);
        <<"onlineusers">> -> ejabberd_sm:get_vh_session_number(Host)
    end.


