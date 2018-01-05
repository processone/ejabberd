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
-module(mod_bosh_riak).
-behaviour(mod_bosh).

%% API
-export([init/0, open_session/2, close_session/1, find_session/1]).

-record(bosh, {sid :: binary(),
	       pid :: pid()}).

-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    clean_table().

open_session(SID, Pid) ->
    ejabberd_riak:put(#bosh{sid = SID, pid = Pid}, bosh_schema()).

close_session(SID) ->
    ejabberd_riak:delete(bosh, SID).

find_session(SID) ->
    case ejabberd_riak:get(bosh, bosh_schema(), SID) of
	{ok, #bosh{pid = Pid}} -> {ok, Pid};
	{error, _} = Err -> Err
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
bosh_schema() ->
    {record_info(fields, bosh), #bosh{}}.

clean_table() ->
    ?DEBUG("Cleaning Riak 'bosh' table...", []),
    case ejabberd_riak:get(bosh, bosh_schema()) of
	{ok, Rs} ->
	    lists:foreach(
	      fun(#bosh{sid = SID, pid = Pid}) when node(Pid) == node() ->
		      ejabberd_riak:delete(bosh, SID);
		 (_) ->
		      ok
	      end, Rs);
	{error, Reason} = Err ->
	    ?ERROR_MSG("failed to clean Riak 'bosh' table: ~p", [Reason]),
	    Err
    end.
