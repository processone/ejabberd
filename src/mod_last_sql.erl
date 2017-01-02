%%%-------------------------------------------------------------------
%%% File    : mod_last_sql.erl
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

-module(mod_last_sql).

-behaviour(mod_last).

-compile([{parse_transform, ejabberd_sql_pt}]).

%% API
-export([init/2, get_last/2, store_last_info/4, remove_user/2,
	 import/2, export/1]).

-include("mod_last.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

get_last(LUser, LServer) ->
    case catch sql_queries:get_last(LServer, LUser) of
        {selected, []} ->
            not_found;
        {selected, [{TimeStamp, Status}]} ->
            {ok, TimeStamp, Status};
        Reason ->
	    ?ERROR_MSG("failed to get last for user ~s@~s: ~p",
		       [LUser, LServer, Reason]),
	    {error, {invalid_result, Reason}}
    end.

store_last_info(LUser, LServer, TimeStamp, Status) ->
    sql_queries:set_last_t(LServer, LUser, TimeStamp, Status).

remove_user(LUser, LServer) ->
    sql_queries:del_last(LServer, LUser).

export(_Server) ->
    [{last_activity,
      fun(Host, #last_activity{us = {LUser, LServer},
                               timestamp = TimeStamp, status = Status})
            when LServer == Host ->
              [?SQL("delete from last where username=%(LUser)s;"),
               ?SQL("insert into last(username, seconds, state)"
                    " values (%(LUser)s, %(TimeStamp)d, %(Status)s);")];
         (_Host, _R) ->
              []
      end}].

import(_LServer, _LA) ->
    pass.
