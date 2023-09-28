%%%-------------------------------------------------------------------
%%% File    : mod_last_sql.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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


%% API
-export([init/2, get_last/2, store_last_info/4, remove_user/2,
	 import/2, export/1]).

-include("mod_last.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(Host, _Opts) ->
    ejabberd_sql_schema:update_schema(Host, ?MODULE, schemas()),
    ok.

schemas() ->
    [#sql_schema{
        version = 1,
        tables =
            [#sql_table{
                name = <<"last">>,
                columns =
                    [#sql_column{name = <<"username">>, type = text},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"seconds">>, type = text},
                     #sql_column{name = <<"state">>, type = text}],
                indices = [#sql_index{
                              columns = [<<"server_host">>, <<"username">>],
                              unique = true}]}]}].

get_last(LUser, LServer) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("select @(seconds)d, @(state)s from last"
		" where username=%(LUser)s and %(LServer)H")) of
        {selected, []} ->
	    error;
        {selected, [{TimeStamp, Status}]} ->
            {ok, {TimeStamp, Status}};
        _Reason ->
	    {error, db_failure}
    end.

store_last_info(LUser, LServer, TimeStamp, Status) ->
    TS = integer_to_binary(TimeStamp),
    case ?SQL_UPSERT(LServer, "last",
		     ["!username=%(LUser)s",
                      "!server_host=%(LServer)s",
		      "seconds=%(TS)s",
		      "state=%(Status)s"]) of
	ok ->
	    ok;
	_Err ->
	    {error, db_failure}
    end.

remove_user(LUser, LServer) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from last where username=%(LUser)s and %(LServer)H")).

export(_Server) ->
    [{last_activity,
      fun(Host, #last_activity{us = {LUser, LServer},
                               timestamp = TimeStamp, status = Status})
            when LServer == Host ->
              TS = integer_to_binary(TimeStamp),
              [?SQL("delete from last where username=%(LUser)s and %(LServer)H;"),
               ?SQL_INSERT("last",
                           ["username=%(LUser)s",
                            "server_host=%(LServer)s",
                            "seconds=%(TS)s",
                            "state=%(Status)s"])];
         (_Host, _R) ->
              []
      end}].

import(_LServer, _LA) ->
    pass.
