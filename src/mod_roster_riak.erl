%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_roster_riak).


-behaviour(mod_roster).

%% API
-export([init/2, read_roster_version/2, write_roster_version/4,
	 get_roster/2, get_roster_by_jid/3,
	 roster_subscribe/4, get_roster_by_jid_with_groups/3,
	 remove_user/2, update_roster/4, del_roster/3, transaction/2,
	 read_subscription_and_groups/3, get_only_items/2, import/2]).

-include("jlib.hrl").
-include("mod_roster.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

read_roster_version(LUser, LServer) ->
    case ejabberd_riak:get(roster_version, roster_version_schema(),
			   {LUser, LServer}) of
        {ok, #roster_version{version = V}} -> V;
        _Err -> error
    end.

write_roster_version(LUser, LServer, _InTransaction, Ver) ->
    US = {LUser, LServer},
    ejabberd_riak:put(#roster_version{us = US, version = Ver},
		      roster_version_schema()).

get_roster(LUser, LServer) ->
    case ejabberd_riak:get_by_index(roster, roster_schema(),
				    <<"us">>, {LUser, LServer}) of
        {ok, Items} -> Items;
        _Err -> []
    end.

get_roster_by_jid(LUser, LServer, LJID) ->
    case ejabberd_riak:get(roster, roster_schema(), {LUser, LServer, LJID}) of
        {ok, I} ->
            I#roster{jid = LJID, name = <<"">>, groups = [], xs = []};
        {error, notfound} ->
            #roster{usj = {LUser, LServer, LJID},
                    us = {LUser, LServer}, jid = LJID};
        Err ->
            exit(Err)
    end.

get_only_items(LUser, LServer) ->
    get_roster(LUser, LServer).

roster_subscribe(LUser, LServer, _LJID, Item) ->
    ejabberd_riak:put(Item, roster_schema(),
                      [{'2i', [{<<"us">>, {LUser, LServer}}]}]).

transaction(_LServer, F) ->
    {atomic, F()}.

get_roster_by_jid_with_groups(LUser, LServer, LJID) ->
    case ejabberd_riak:get(roster, roster_schema(), {LUser, LServer, LJID}) of
        {ok, I} ->
            I;
        {error, notfound} ->
            #roster{usj = {LUser, LServer, LJID},
                    us = {LUser, LServer}, jid = LJID};
        Err ->
            exit(Err)
    end.

remove_user(LUser, LServer) ->
    {atomic, ejabberd_riak:delete_by_index(roster, <<"us">>, {LUser, LServer})}.

update_roster(LUser, LServer, _LJID, Item) ->
    ejabberd_riak:put(Item, roster_schema(),
                      [{'2i', [{<<"us">>, {LUser, LServer}}]}]).

del_roster(LUser, LServer, LJID) ->
    ejabberd_riak:delete(roster, {LUser, LServer, LJID}).

read_subscription_and_groups(LUser, LServer, LJID) ->
    case ejabberd_riak:get(roster, roster_schema(), {LUser, LServer, LJID}) of
        {ok, #roster{subscription = Subscription,
                     groups = Groups}} ->
            {Subscription, Groups};
        _ ->
            error
    end.

import(_LServer, #roster{us = {LUser, LServer}} = R) ->
    ejabberd_riak:put(R, roster_schema(),
		      [{'2i', [{<<"us">>, {LUser, LServer}}]}]);
import(_LServer, #roster_version{} = RV) ->
    ejabberd_riak:put(RV, roster_version_schema()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
roster_schema() ->
    {record_info(fields, roster), #roster{}}.

roster_version_schema() ->
    {record_info(fields, roster_version), #roster_version{}}.
