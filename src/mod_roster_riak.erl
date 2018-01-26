%%%-------------------------------------------------------------------
%%% File    : mod_roster_riak.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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

-module(mod_roster_riak).

-behaviour(mod_roster).

%% API
-export([init/2, read_roster_version/2, write_roster_version/4,
	 get_roster/2, get_roster_item/3, create_roster/1,
	 roster_subscribe/4, remove_user/2, update_roster/4,
	 del_roster/3, read_subscription_and_groups/3, transaction/2,
	 import/3]).

-include("mod_roster.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

read_roster_version(LUser, LServer) ->
    case ejabberd_riak:get(roster_version, roster_version_schema(),
			   {LUser, LServer}) of
        {ok, #roster_version{version = V}} -> {ok, V};
        _Err -> error
    end.

write_roster_version(LUser, LServer, _InTransaction, Ver) ->
    US = {LUser, LServer},
    ejabberd_riak:put(#roster_version{us = US, version = Ver},
		      roster_version_schema()).

get_roster(LUser, LServer) ->
    case ejabberd_riak:get_by_index(roster, roster_schema(),
				    <<"us">>, {LUser, LServer}) of
        {ok, Items} -> {ok, Items};
        _Err -> error
    end.

roster_subscribe(LUser, LServer, _LJID, Item) ->
    ejabberd_riak:put(Item, roster_schema(),
                      [{'2i', [{<<"us">>, {LUser, LServer}}]}]).

transaction(_LServer, F) ->
    {atomic, F()}.

get_roster_item(LUser, LServer, LJID) ->
    case ejabberd_riak:get(roster, roster_schema(), {LUser, LServer, LJID}) of
        {ok, I} ->
            {ok, I};
        {error, _} ->
	    error
    end.

remove_user(LUser, LServer) ->
    ejabberd_riak:delete_by_index(roster, <<"us">>, {LUser, LServer}).

update_roster(LUser, LServer, _LJID, Item) ->
    ejabberd_riak:put(Item, roster_schema(),
                      [{'2i', [{<<"us">>, {LUser, LServer}}]}]).

del_roster(LUser, LServer, LJID) ->
    ejabberd_riak:delete(roster, {LUser, LServer, LJID}).

read_subscription_and_groups(LUser, LServer, LJID) ->
    case ejabberd_riak:get(roster, roster_schema(), {LUser, LServer, LJID}) of
	{ok, #roster{subscription = Subscription,
		     ask = Ask,
		     groups = Groups}} ->
	    {ok, {Subscription, Ask, Groups}};
	_ ->
	    error
    end.

create_roster(#roster{us = {LUser, LServer}} = RItem) ->
    ejabberd_riak:put(
      RItem, roster_schema(),
      [{'2i', [{<<"us">>, {LUser, LServer}}]}]).

import(_LServer, <<"rosterusers">>, RosterItem) ->
    {LUser, LServer} = RosterItem#roster.us,
    ejabberd_riak:put(RosterItem, roster_schema(),
		      [{'2i', [{<<"us">>, {LUser, LServer}}]}]);
import(LServer, <<"roster_version">>, [LUser, Ver]) ->
    RV = #roster_version{us = {LUser, LServer}, version = Ver},
    ejabberd_riak:put(RV, roster_version_schema()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
roster_schema() ->
    {record_info(fields, roster), #roster{}}.

roster_version_schema() ->
    {record_info(fields, roster_version), #roster_version{}}.
