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
-module(mod_proxy65_riak).
-behaviour(mod_proxy65).

%% API
-export([init/0, register_stream/2, unregister_stream/1, activate_stream/4]).

-include("logger.hrl").

-record(proxy65, {sid   :: binary(),
		  pid_t :: pid(),
		  pid_i :: pid() | undefined,
		  jid_i :: binary() | undefined}).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    clean_table().

register_stream(SID, Pid) ->
    case ejabberd_riak:get(proxy65, proxy65_schema(), SID) of
	{error, notfound} ->
	    ejabberd_riak:put(#proxy65{sid = SID, pid_t = Pid},
			      proxy65_schema());
	{ok, #proxy65{pid_i = undefined} = R} ->
	    ejabberd_riak:put(R#proxy65{pid_i = Pid},
			      proxy65_schema());
	{ok, _} ->
	    {error, conflict};
	{error, _} ->
	    {error, db_failure}
    end.

unregister_stream(SID) ->
    case ejabberd_riak:delete(proxy65, SID) of
	ok -> ok;
	{error, _} -> {error, db_failure}
    end.

activate_stream(SID, IJID, MaxConnections, _Node) ->
    try
	case ejabberd_riak:get(proxy65, proxy65_schema(), SID) of
	    {ok, #proxy65{pid_t = TPid, pid_i = IPid,
			  jid_i = undefined} = R} when is_pid(IPid) ->
		{ok, Num} = ejabberd_riak:count_by_index(
			      proxy65, <<"jid_i">>, IJID),
		if Num >= MaxConnections ->
			{error, {limit, IPid, TPid}};
		   true ->
			ok = ejabberd_riak:put(
			       R#proxy65{jid_i = IJID},
			       proxy65_schema(),
			       [{'2i', [{<<"jid_i">>, IJID}]}]),
			{ok, IPid, TPid}
		end;
	    {ok, #proxy65{jid_i = JID}} ->
		if is_binary(JID) -> {error, conflict};
		   true -> {error, notfound}
		end;
	    {error, _} = Err ->
		Err
	end
    catch _:{badmatch, {error, _}} ->
	    {error, db_failure}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
proxy65_schema() ->
    {record_info(fields, proxy65), #proxy65{}}.

clean_table() ->
    ?DEBUG("Cleaning Riak 'proxy65' table...", []),
    case ejabberd_riak:get(proxy65, proxy65_schema()) of
	{ok, Rs} ->
	    lists:foreach(
	      fun(#proxy65{sid = SID, pid_t = TPid, pid_i = IPid}) ->
		      if node(TPid) == node() orelse
			 (is_pid(IPid) andalso node(IPid) == node()) ->
			      ejabberd_riak:delete(proxy65, SID);
			 true ->
			      ok
		      end
	      end, Rs);
	{error, Reason} = Err ->
	    ?ERROR_MSG("Failed to clean Riak 'proxy65' table: ~p", [Reason]),
	    Err
    end.
