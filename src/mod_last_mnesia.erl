%%%-------------------------------------------------------------------
%%% File    : mod_last_mnesia.erl
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

-module(mod_last_mnesia).

-behaviour(mod_last).

%% API
-export([init/2, import/2, get_last/2, store_last_info/4, remove_user/2]).

-include("mod_last.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, last_activity,
			[{disc_copies, [node()]},
			 {attributes,
			  record_info(fields, last_activity)}]),
    update_table().

get_last(LUser, LServer) ->
    case mnesia:dirty_read(last_activity, {LUser, LServer}) of
	[] ->
	    not_found;
	[#last_activity{timestamp = TimeStamp,
			status = Status}] ->
	    {ok, TimeStamp, Status}
    end.

store_last_info(LUser, LServer, TimeStamp, Status) ->
    US = {LUser, LServer},
    F = fun () ->
		mnesia:write(#last_activity{us = US,
					    timestamp = TimeStamp,
					    status = Status})
	end,
    mnesia:transaction(F).

remove_user(LUser, LServer) ->
    US = {LUser, LServer},
    F = fun () -> mnesia:delete({last_activity, US}) end,
    mnesia:transaction(F).

import(_LServer, #last_activity{} = LA) ->
    mnesia:dirty_write(LA).

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_table() ->
    Fields = record_info(fields, last_activity),
    case mnesia:table_info(last_activity, attributes) of
      Fields ->
          ejabberd_config:convert_table_to_binary(
            last_activity, Fields, set,
            fun(#last_activity{us = {U, _}}) -> U end,
            fun(#last_activity{us = {U, S}, status = Status} = R) ->
                    R#last_activity{us = {iolist_to_binary(U),
                                          iolist_to_binary(S)},
                                    status = iolist_to_binary(Status)}
            end);
      _ ->
	  ?INFO_MSG("Recreating last_activity table", []),
	  mnesia:transform_table(last_activity, ignore, Fields)
    end.
