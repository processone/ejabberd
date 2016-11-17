%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_announce_riak).
-behaviour(mod_announce).

%% API
-export([init/2, set_motd_users/2, set_motd/2, delete_motd/1,
	 get_motd/1, is_motd_user/2, set_motd_user/2, import/2]).

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

import(_LServer, #motd{} = Motd) ->
    ejabberd_riak:put(Motd, motd_schema());
import(_LServer, #motd_users{us = {_, S}} = Users) ->
    ejabberd_riak:put(Users, motd_users_schema(),
		      [{'2i', [{<<"server">>, S}]}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
motd_schema() ->
    {record_info(fields, motd), #motd{}}.

motd_users_schema() ->
    {record_info(fields, motd_users), #motd_users{}}.
