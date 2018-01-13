%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 15 Apr 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
%%%-------------------------------------------------------------------
-module(ejabberd_sm_riak).
-behaviour(ejabberd_sm).

%% API
-export([init/0, set_session/1, delete_session/1, get_sessions/0,
	 get_sessions/1, get_sessions/2]).

-include("ejabberd_sm.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    clean_table().

set_session(Session) ->
    ejabberd_riak:put(Session, session_schema(),
		      [{'2i', [{<<"us">>, Session#session.us}]}]).

delete_session(Session) ->
    ejabberd_riak:delete(session, Session#session.sid).

get_sessions() ->
    case ejabberd_riak:get(session, session_schema()) of
	{ok, Ss} -> Ss;
	{error, _} -> []
    end.

get_sessions(LServer) ->
    [S || S <- get_sessions(), element(2, S#session.us) == LServer].

get_sessions(U, S) ->
    ejabberd_riak:get_by_index(session, session_schema(), <<"us">>, {U, S}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
session_schema() ->
    {record_info(fields, session), #session{}}.

clean_table() ->
    %% TODO: not very efficient, rewrite using map-reduce or something
    ?DEBUG("Cleaning Riak 'sm' table...", []),
    lists:foreach(
      fun(#session{sid = {_, Pid} = SID}) when node(Pid) == node() ->
	      ejabberd_riak:delete(session, SID);
	 (_) ->
	      ok
      end, get_sessions()).
