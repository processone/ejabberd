%%%-------------------------------------------------------------------
%%% File    : mod_shared_roster_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2021   ProcessOne
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
%%%----------------------------------------------------------------------

-module(mod_shared_roster_mnesia).

-behaviour(mod_shared_roster).

%% API
-export([init/2, list_groups/1, groups_with_opts/1, create_group/3,
	 delete_group/2, get_group_opts/2, set_group_opts/3,
	 get_user_groups/2, get_group_explicit_users/2,
	 get_user_displayed_groups/3, is_user_in_group/3,
	 add_user_to_group/3, remove_user_from_group/3, import/3]).
-export([need_transform/1, transform/1, use_cache/1]).

-include("mod_roster.hrl").
-include("mod_shared_roster.hrl").
-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, sr_group,
			   [{disc_copies, [node()]},
			    {attributes, record_info(fields, sr_group)}]),
    ejabberd_mnesia:create(?MODULE, sr_user,
			   [{disc_copies, [node()]}, {type, bag},
			    {attributes, record_info(fields, sr_user)},
			    {index, [group_host]}]).

list_groups(Host) ->
    mnesia:dirty_select(sr_group,
			[{#sr_group{group_host = {'$1', '$2'}, _ = '_'},
			  [{'==', '$2', Host}], ['$1']}]).

-spec use_cache(binary()) -> boolean().
use_cache(_Host) ->
    false.

groups_with_opts(Host) ->
    Gs = mnesia:dirty_select(sr_group,
			     [{#sr_group{group_host = {'$1', Host}, opts = '$2',
					 _ = '_'},
			       [], [['$1', '$2']]}]),
    lists:map(fun ([G, O]) -> {G, O} end, Gs).

create_group(Host, Group, Opts) ->
    R = #sr_group{group_host = {Group, Host}, opts = Opts},
    F = fun () -> mnesia:write(R) end,
    mnesia:transaction(F).

delete_group(Host, Group) ->
    GroupHost = {Group, Host},
    F = fun () ->
		mnesia:delete({sr_group, GroupHost}),
		Users = mnesia:index_read(sr_user, GroupHost,
					  #sr_user.group_host),
		lists:foreach(fun (UserEntry) ->
				      mnesia:delete_object(UserEntry)
			      end,
			      Users)
	end,
    mnesia:transaction(F).

get_group_opts(Host, Group) ->
    case catch mnesia:dirty_read(sr_group, {Group, Host}) of
	[#sr_group{opts = Opts}] -> {ok, Opts};
	_ -> error
    end.

set_group_opts(Host, Group, Opts) ->
    R = #sr_group{group_host = {Group, Host}, opts = Opts},
    F = fun () -> mnesia:write(R) end,
    mnesia:transaction(F).

get_user_groups(US, Host) ->
    case catch mnesia:dirty_read(sr_user, US) of
	Rs when is_list(Rs) ->
	    [Group || #sr_user{group_host = {Group, H}} <- Rs, H == Host];
	_ ->
	    []
    end.

get_group_explicit_users(Host, Group) ->
    Read = (catch mnesia:dirty_index_read(sr_user,
					  {Group, Host}, #sr_user.group_host)),
    case Read of
	Rs when is_list(Rs) -> [R#sr_user.us || R <- Rs];
	_ -> []
    end.

get_user_displayed_groups(LUser, LServer, GroupsOpts) ->
    case catch mnesia:dirty_read(sr_user, {LUser, LServer}) of
	Rs when is_list(Rs) ->
	    [{Group, proplists:get_value(Group, GroupsOpts, [])}
	     || #sr_user{group_host = {Group, H}} <- Rs,
		H == LServer];
	_ ->
	    []
    end.

is_user_in_group(US, Group, Host) ->
    case mnesia:dirty_match_object(
	   #sr_user{us = US, group_host = {Group, Host}}) of
	[] -> false;
	_ -> true
    end.

add_user_to_group(Host, US, Group) ->
    R = #sr_user{us = US, group_host = {Group, Host}},
    F = fun () -> mnesia:write(R) end,
    mnesia:transaction(F).

remove_user_from_group(Host, US, Group) ->
    R = #sr_user{us = US, group_host = {Group, Host}},
    F = fun () -> mnesia:delete_object(R) end,
    mnesia:transaction(F).

import(LServer, <<"sr_group">>, [Group, SOpts, _TimeStamp]) ->
    G = #sr_group{group_host = {Group, LServer},
                  opts = ejabberd_sql:decode_term(SOpts)},
    mnesia:dirty_write(G);
import(LServer, <<"sr_user">>, [SJID, Group, _TimeStamp]) ->
    #jid{luser = U, lserver = S} = jid:decode(SJID),
    User = #sr_user{us = {U, S}, group_host = {Group, LServer}},
    mnesia:dirty_write(User).

need_transform({sr_group, {G, H}, _})
  when is_list(G) orelse is_list(H) ->
    ?INFO_MSG("Mnesia table 'sr_group' will be converted to binary", []),
    true;
need_transform({sr_user, {U, S}, {G, H}})
  when is_list(U) orelse is_list(S) orelse is_list(G) orelse is_list(H) ->
    ?INFO_MSG("Mnesia table 'sr_user' will be converted to binary", []),
    true;
need_transform({sr_group, {_, _}, [{name, _} | _]}) ->
    ?INFO_MSG("Mnesia table 'sr_group' will be converted from option Name to Label", []),
    true;
need_transform(_) ->
    false.

transform(#sr_group{group_host = {G, _H}, opts = Opts} = R)
  when is_binary(G)  ->
    Opts2 = case proplists:get_value(name, Opts, false) of
	false -> Opts;
	Name -> [{label, Name} | proplists:delete(name, Opts)]
    end,
    R#sr_group{opts = Opts2};
transform(#sr_group{group_host = {G, H}, opts = Opts} = R) ->
    R#sr_group{group_host = {iolist_to_binary(G), iolist_to_binary(H)},
	       opts = mod_shared_roster:opts_to_binary(Opts)};
transform(#sr_user{us = {U, S}, group_host = {G, H}} = R) ->
    R#sr_user{us = {iolist_to_binary(U), iolist_to_binary(S)},
	      group_host = {iolist_to_binary(G), iolist_to_binary(H)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
