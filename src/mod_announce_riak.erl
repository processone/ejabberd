%%%-------------------------------------------------------------------
%%% File    : mod_announce_riak.erl
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

-module(mod_announce_riak).

-behaviour(mod_announce).

%% API
-export([init/2, set_motd_users/2, set_motd/2, delete_motd/1,
	 get_motd/1, is_motd_user/2, set_motd_user/2, import/3]).

-include("xmpp.hrl").
-include("mod_announce.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

set_motd_users(_LServer, USRs) ->
    try
	lists:foreach(
	  fun({U, S, _R}) ->
		  ok = ejabberd_riak:put(#motd_users{us = {U, S}},
					 motd_users_schema(),
					 [{'2i', [{<<"server">>, S}]}])
	  end, USRs),
	{atomic, ok}
    catch _:{badmatch, Err} ->
	    {atomic, Err}
    end.

set_motd(LServer, Packet) ->
    {atomic, ejabberd_riak:put(#motd{server = LServer,
				     packet = Packet},
			       motd_schema())}.

delete_motd(LServer) ->
    try
	ok = ejabberd_riak:delete(motd, LServer),
	ok = ejabberd_riak:delete_by_index(motd_users,
					   <<"server">>,
					   LServer),
	{atomic, ok}
    catch _:{badmatch, Err} ->
	    {atomic, Err}
    end.

get_motd(LServer) ->
    case ejabberd_riak:get(motd, motd_schema(), LServer) of
        {ok, #motd{packet = Packet}} ->
	    {ok, Packet};
	_ ->
	    error
    end.

is_motd_user(LUser, LServer) ->
    case ejabberd_riak:get(motd_users, motd_users_schema(),
			   {LUser, LServer}) of
	{ok, #motd_users{}} -> true;
	_ -> false
    end.

set_motd_user(LUser, LServer) ->
    {atomic, ejabberd_riak:put(
	       #motd_users{us = {LUser, LServer}}, motd_users_schema(),
	       [{'2i', [{<<"server">>, LServer}]}])}.

import(LServer, <<"motd">>, [<<>>, XML, _TimeStamp]) ->
    El = fxml_stream:parse_element(XML),
    ejabberd_riak:put(#motd{server = LServer, packet = El}, motd_schema());
import(LServer, <<"motd">>, [LUser, <<>>, _TimeStamp]) ->
    Users = #motd_users{us = {LUser, LServer}},
    ejabberd_riak:put(Users, motd_users_schema(),
		      [{'2i', [{<<"server">>, LServer}]}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
motd_schema() ->
    {record_info(fields, motd), #motd{}}.

motd_users_schema() ->
    {record_info(fields, motd_users), #motd_users{}}.
