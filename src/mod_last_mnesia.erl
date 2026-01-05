%%%-------------------------------------------------------------------
%%% File    : mod_last_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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

-module(mod_last_mnesia).

-behaviour(mod_last).

%% API
-export([init/2, import/2, get_last/2, store_last_info/4,
	 remove_user/2, use_cache/1]).
-export([need_transform/1, transform/1]).

-include("mod_last.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, last_activity,
			   [{disc_only_copies, [node()]},
			    {attributes, record_info(fields, last_activity)}]).

use_cache(Host) ->
    case mnesia:table_info(last_activity, storage_type) of
	disc_only_copies ->
	    mod_last_opt:use_cache(Host);
	_ ->
	    false
    end.

get_last(LUser, LServer) ->
    case mnesia:dirty_read(last_activity, {LUser, LServer}) of
	[] ->
	    error;
	[#last_activity{timestamp = TimeStamp,
			status = Status}] ->
	    {ok, {TimeStamp, Status}}
    end.

store_last_info(LUser, LServer, TimeStamp, Status) ->
    mnesia:dirty_write(#last_activity{us = {LUser, LServer},
				      timestamp = TimeStamp,
				      status = Status}).

remove_user(LUser, LServer) ->
    US = {LUser, LServer},
    mnesia:dirty_delete({last_activity, US}).

import(_LServer, #last_activity{} = LA) ->
    mnesia:dirty_write(LA).

need_transform({last_activity, {U, S}, _, Status})
  when is_list(U) orelse is_list(S) orelse is_list(Status) ->
    ?INFO_MSG("Mnesia table 'last_activity' will be converted to binary", []),
    true;
need_transform(_) ->
    false.

transform(#last_activity{us = {U, S}, status = Status} = R) ->
    R#last_activity{us = {iolist_to_binary(U), iolist_to_binary(S)},
		    status = iolist_to_binary(Status)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
