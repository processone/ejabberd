%%%-------------------------------------------------------------------
%%% File    : mod_muc_riak.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-module(mod_muc_riak).

-behaviour(mod_muc).
-behaviour(mod_muc_room).

%% API
-export([init/2, import/3, store_room/4, restore_room/3, forget_room/3,
	 can_use_nick/4, get_rooms/2, get_nick/3, set_nick/4]).
-export([register_online_room/4, unregister_online_room/4, find_online_room/3,
	 get_online_rooms/3, count_online_rooms/2, rsm_supported/0,
	 register_online_user/4, unregister_online_user/4,
	 count_online_rooms_by_user/3, get_online_rooms_by_user/3]).
-export([set_affiliation/6, set_affiliations/4, get_affiliation/5,
	 get_affiliations/3, search_affiliation/4]).

-include("jid.hrl").
-include("mod_muc.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

store_room(_LServer, Host, Name, Opts) ->
    {atomic, ejabberd_riak:put(#muc_room{name_host = {Name, Host},
                                         opts = Opts},
			       muc_room_schema())}.

restore_room(_LServer, Host, Name) ->
    case ejabberd_riak:get(muc_room, muc_room_schema(), {Name, Host}) of
        {ok, #muc_room{opts = Opts}} -> Opts;
        _ -> error
    end.

forget_room(_LServer, Host, Name) ->
    {atomic, ejabberd_riak:delete(muc_room, {Name, Host})}.

can_use_nick(LServer, Host, JID, Nick) ->
    {LUser, LServer, _} = jid:tolower(JID),
    LUS = {LUser, LServer},
    case ejabberd_riak:get_by_index(muc_registered,
				    muc_registered_schema(),
                                    <<"nick_host">>, {Nick, Host}) of
        {ok, []} ->
            true;
        {ok, [#muc_registered{us_host = {U, _Host}}]} ->
            U == LUS;
        {error, _} ->
            true
    end.

get_rooms(_LServer, Host) ->
    case ejabberd_riak:get(muc_room, muc_room_schema()) of
        {ok, Rs} ->
            lists:filter(
              fun(#muc_room{name_host = {_, H}}) ->
                      Host == H
              end, Rs);
        _Err ->
            []
    end.

get_nick(LServer, Host, From) ->
    {LUser, LServer, _} = jid:tolower(From),
    US = {LUser, LServer},
    case ejabberd_riak:get(muc_registered,
			   muc_registered_schema(),
			   {US, Host}) of
        {ok, #muc_registered{nick = Nick}} -> Nick;
        {error, _} -> error
    end.

set_nick(LServer, Host, From, Nick) ->
    {LUser, LServer, _} = jid:tolower(From),
    LUS = {LUser, LServer},
    {atomic,
     case Nick of
         <<"">> ->
             ejabberd_riak:delete(muc_registered, {LUS, Host});
         _ ->
             Allow = case ejabberd_riak:get_by_index(
                            muc_registered,
			    muc_registered_schema(),
                            <<"nick_host">>, {Nick, Host}) of
                         {ok, []} ->
                             true;
                         {ok, [#muc_registered{us_host = {U, _Host}}]} ->
                             U == LUS;
                         {error, _} ->
                             false
                     end,
             if Allow ->
                     ejabberd_riak:put(#muc_registered{us_host = {LUS, Host},
                                                       nick = Nick},
				       muc_registered_schema(),
                                       [{'2i', [{<<"nick_host">>,
                                                 {Nick, Host}}]}]);
                true ->
                     false
             end
     end}.

set_affiliation(_ServerHost, _Room, _Host, _JID, _Affiliation, _Reason) ->
    {error, not_implemented}.

set_affiliations(_ServerHost, _Room, _Host, _Affiliations) ->
    {error, not_implemented}.

get_affiliation(_ServerHost, _Room, _Host, _LUser, _LServer) ->
    {error, not_implemented}.

get_affiliations(_ServerHost, _Room, _Host) ->
    {error, not_implemented}.

search_affiliation(_ServerHost, _Room, _Host, _Affiliation) ->
    {error, not_implemented}.

register_online_room(_, _, _, _) ->
    erlang:error(not_implemented).

unregister_online_room(_, _, _, _) ->
    erlang:error(not_implemented).

find_online_room(_, _, _) ->
    erlang:error(not_implemented).

count_online_rooms(_, _) ->
    erlang:error(not_implemented).

get_online_rooms(_, _, _) ->
    erlang:error(not_implemented).

rsm_supported() ->
    false.

register_online_user(_, _, _, _) ->
    erlang:error(not_implemented).

unregister_online_user(_, _, _, _) ->
    erlang:error(not_implemented).

count_online_rooms_by_user(_, _, _) ->
    erlang:error(not_implemented).

get_online_rooms_by_user(_, _, _) ->
    erlang:error(not_implemented).

import(_LServer, <<"muc_room">>,
       [Name, RoomHost, SOpts, _TimeStamp]) ->
    Opts = mod_muc:opts_to_binary(ejabberd_sql:decode_term(SOpts)),
    ejabberd_riak:put(
      #muc_room{name_host = {Name, RoomHost}, opts = Opts},
      muc_room_schema());
import(_LServer, <<"muc_registered">>,
       [J, RoomHost, Nick, _TimeStamp]) ->
    #jid{user = U, server = S} = jid:decode(J),
    R = #muc_registered{us_host = {{U, S}, RoomHost}, nick = Nick},
    ejabberd_riak:put(R, muc_registered_schema(),
		      [{'2i', [{<<"nick_host">>, {Nick, RoomHost}}]}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
muc_room_schema() ->
    {record_info(fields, muc_room), #muc_room{}}.

muc_registered_schema() ->
    {record_info(fields, muc_registered), #muc_registered{}}.
