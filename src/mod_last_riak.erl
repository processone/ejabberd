%%%-------------------------------------------------------------------
%%% File    : mod_last_riak.erl
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

-module(mod_last_riak).

-behaviour(mod_last).

%% API
-export([init/2, import/2, get_last/2, store_last_info/4, remove_user/2]).

-include("mod_last.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

get_last(LUser, LServer) ->
    case ejabberd_riak:get(last_activity, last_activity_schema(),
			   {LUser, LServer}) of
        {ok, #last_activity{timestamp = TimeStamp,
                            status = Status}} ->
            {ok, TimeStamp, Status};
        {error, notfound} ->
            not_found;
        Err ->
            Err
    end.

store_last_info(LUser, LServer, TimeStamp, Status) ->
    US = {LUser, LServer},
    {atomic, ejabberd_riak:put(#last_activity{us = US,
                                              timestamp = TimeStamp,
                                              status = Status},
			       last_activity_schema())}.

remove_user(LUser, LServer) ->
    {atomic, ejabberd_riak:delete(last_activity, {LUser, LServer})}.

import(_LServer, #last_activity{} = LA) ->
    ejabberd_riak:put(LA, last_activity_schema()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
last_activity_schema() ->
    {record_info(fields, last_activity), #last_activity{}}.
