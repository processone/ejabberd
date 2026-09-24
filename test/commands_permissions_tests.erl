%%%-------------------------------------------------------------------
%%% Author  : Badlop <badlop@process-one.net>
%%% Created :
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%-------------------------------------------------------------------

%%%% definitions
%% @format-begin

-module(commands_permissions_tests).

-include_lib("eunit/include/eunit.hrl").

prepare_arguments_zip_grouphost_test() ->
    ArgsFormat =
        [{user, binary},
         {host, binary},
         {service, binary},
         {group, binary},
         {grouphost, binary},
         {localhost, binary}],
    Arguments =
        [<<"user.name">>,
         <<"user.host">>,
         <<"global">>,
         <<"group.id">>,
         <<"group.host">>,
         <<"local.host">>],
    ?assertMatch([{service, <<"global">>} | _],
                 ejabberd_access_permissions:prepare_arguments(ArgsFormat, Arguments)).

get_vhost_service_test() ->
    ?assertEqual(global_scope,
                 get_vhost_argument([{user, <<"user.name">>},
                                     {host, <<"user.host">>},
                                     {service, <<"global">>},
                                     {localhost, <<"local.host">>},
                                     {grouphost, <<"group.host">>},
                                     {group, <<"group.id">>}])).

get_vhost_grouphost_test() ->
    ?assertEqual(<<"group.host">>,
                 get_vhost_argument([{user, <<"user.name">>},
                                     {host, <<"user.host">>},
                                     {localhost, <<"local.host">>},
                                     {grouphost, <<"group.host">>},
                                     {group, <<"group.id">>}])).

get_vhost_localhost_test() ->
    ?assertEqual(<<"local.host">>,
                 get_vhost_argument([{user, <<"user.name">>},
                                     {host, <<"user.host">>},
                                     {localhost, <<"local.host">>},
                                     {group, <<"group.id">>}])).

get_vhost_host_test() ->
    ?assertEqual(<<"user.host">>,
                 get_vhost_argument([{user, <<"user.name">>},
                                     {host, <<"user.host">>},
                                     {group, <<"group.id">>}])).

get_vhost_none_test() ->
    ?assertEqual(no_host_argument,
                 get_vhost_argument([{user, <<"user.name">>}, {group, <<"group.id">>}])).

get_vhost_argument(Arguments) ->
    ejabberd_access_permissions:get_vhost_argument(
        ejabberd_access_permissions:sort_arguments_relevance(Arguments)).
