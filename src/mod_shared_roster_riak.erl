%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_shared_roster_riak).

-behaviour(mod_shared_roster).

%% API
-export([init/2, list_groups/1, groups_with_opts/1, create_group/3,
	 delete_group/2, get_group_opts/2, set_group_opts/3,
	 get_user_groups/2, get_group_explicit_users/2,
	 get_user_displayed_groups/3, is_user_in_group/3,
	 add_user_to_group/3, remove_user_from_group/3, import/2]).

-include("mod_roster.hrl").
-include("mod_shared_roster.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

list_groups(Host) ->
    case ejabberd_riak:get_keys_by_index(sr_group, <<"host">>, Host) of
        {ok, Gs} ->
            [G || {G, _} <- Gs];
        _ ->
            []
    end.

groups_with_opts(Host) ->
    case ejabberd_riak:get_by_index(sr_group, sr_group_schema(),
				    <<"host">>, Host) of
        {ok, Rs} ->
            [{G, O} || #sr_group{group_host = {G, _}, opts = O} <- Rs];
        _ ->
            []
    end.

create_group(Host, Group, Opts) ->
    {atomic, ejabberd_riak:put(#sr_group{group_host = {Group, Host},
                                         opts = Opts},
			       sr_group_schema(),
                               [{'2i', [{<<"host">>, Host}]}])}.

delete_group(Host, Group) ->
    try
        ok = ejabberd_riak:delete(sr_group, {Group, Host}),
        ok = ejabberd_riak:delete_by_index(sr_user, <<"group_host">>,
                                           {Group, Host}),
        {atomic, ok}
    catch _:{badmatch, Err} ->
            {atomic, Err}
    end.

get_group_opts(Host, Group) ->
    case ejabberd_riak:get(sr_group, sr_group_schema(), {Group, Host}) of
        {ok, #sr_group{opts = Opts}} -> Opts;
        _ -> error
    end.

set_group_opts(Host, Group, Opts) ->
    {atomic, ejabberd_riak:put(#sr_group{group_host = {Group, Host},
                                         opts = Opts},
			       sr_group_schema(),
                               [{'2i', [{<<"host">>, Host}]}])}.

get_user_groups(US, Host) ->
    case ejabberd_riak:get_by_index(sr_user, sr_user_schema(), <<"us">>, US) of
        {ok, Rs} ->
            [Group || #sr_user{group_host = {Group, H}} <- Rs, H == Host];
        _ ->
            []
    end.

get_group_explicit_users(Host, Group) ->
    case ejabberd_riak:get_by_index(sr_user, sr_user_schema(),
				    <<"group_host">>, {Group, Host}) of
        {ok, Rs} ->
            [R#sr_user.us || R <- Rs];
        _ ->
            []
    end.

get_user_displayed_groups(LUser, LServer, GroupsOpts) ->
    case ejabberd_riak:get_by_index(sr_user, sr_user_schema(),
                                    <<"us">>, {LUser, LServer}) of
        {ok, Rs} ->
            [{Group, proplists:get_value(Group, GroupsOpts, [])}
             || #sr_user{group_host = {Group, _}} <- Rs];
        _ ->
            []
    end.

is_user_in_group(US, Group, Host) ->
    case ejabberd_riak:get_by_index(sr_user, sr_user_schema(), <<"us">>, US) of
        {ok, Rs} ->
            lists:any(
	      fun(#sr_user{group_host = {G, H}}) ->
		      (Group == G) and (Host == H)
	      end, Rs);
        _Err ->
            false
    end.

add_user_to_group(Host, US, Group) ->
    {atomic, ejabberd_riak:put(
               #sr_user{us = US, group_host = {Group, Host}},
	       sr_user_schema(),
               [{i, {US, {Group, Host}}},
                {'2i', [{<<"us">>, US},
                        {<<"group_host">>, {Group, Host}}]}])}.

remove_user_from_group(Host, US, Group) ->
    {atomic, ejabberd_riak:delete(sr_group, {US, {Group, Host}})}.

import(_LServer, #sr_group{group_host = {_, Host}} = G) ->
    ejabberd_riak:put(G, sr_group_schema(), [{'2i', [{<<"host">>, Host}]}]);
import(_LServer, #sr_user{us = US, group_host = {Group, Host}} = User) ->
    ejabberd_riak:put(User, sr_user_schema(),
                      [{i, {US, {Group, Host}}},
                       {'2i', [{<<"us">>, US},
                               {<<"group_host">>, {Group, Host}}]}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
sr_group_schema() ->
    {record_info(fields, sr_group), #sr_group{}}.

sr_user_schema() ->
    {record_info(fields, sr_user), #sr_user{}}.
