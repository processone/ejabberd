%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_muc_riak).

-behaviour(mod_muc).

%% API
-export([init/2, import/2, store_room/4, restore_room/3, forget_room/3,
	 can_use_nick/4, get_rooms/2, get_nick/3, set_nick/4]).

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

import(_LServer, #muc_room{} = R) ->
    ejabberd_riak:put(R, muc_room_schema());
import(_LServer, #muc_registered{us_host = {_, Host}, nick = Nick} = R) ->
    ejabberd_riak:put(R, muc_registered_schema(),
		      [{'2i', [{<<"nick_host">>, {Nick, Host}}]}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
muc_room_schema() ->
    {record_info(fields, muc_room), #muc_room{}}.

muc_registered_schema() ->
    {record_info(fields, muc_registered), #muc_registered{}}.
