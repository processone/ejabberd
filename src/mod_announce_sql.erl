%%%-------------------------------------------------------------------
%%% File    : mod_announce_sql.erl
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

-module(mod_announce_sql).

-behaviour(mod_announce).


%% API
-export([init/2, set_motd_users/2, set_motd/2, delete_motd/1,
	 get_motd/1, is_motd_user/2, set_motd_user/2, import/3,
	 export/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("mod_announce.hrl").
-include("ejabberd_sql_pt.hrl").
-include("logger.hrl").

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
                name = <<"motd">>,
                columns =
                    [#sql_column{name = <<"username">>, type = text},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"xml">>, type = text},
                     #sql_column{name = <<"created_at">>, type = timestamp,
                                 default = true}],
                indices = [#sql_index{
                              columns = [<<"server_host">>, <<"username">>],
                              unique = true}]}]}].

set_motd_users(LServer, USRs) ->
    F = fun() ->
		lists:foreach(
		  fun({U, _S, _R}) ->
                          ?SQL_UPSERT_T(
                             "motd",
                             ["!username=%(U)s",
                              "!server_host=%(LServer)s",
                              "xml=''"])
		  end, USRs)
	end,
    transaction(LServer, F).

set_motd(LServer, Packet) ->
    XML = fxml:element_to_binary(Packet),
    F = fun() ->
                ?SQL_UPSERT_T(
                   "motd",
                   ["!username=''",
                    "!server_host=%(LServer)s",
                    "xml=%(XML)s"])
	end,
    transaction(LServer, F).

delete_motd(LServer) ->
    F = fun() ->
                ejabberd_sql:sql_query_t(
                  ?SQL("delete from motd where %(LServer)H"))
	end,
    transaction(LServer, F).

get_motd(LServer) ->
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(xml)s from motd"
                      " where username='' and %(LServer)H")) of
        {selected, [{XML}]} ->
	    parse_element(XML);
	{selected, []} ->
	    error;
	_ ->
	    {error, db_failure}
    end.

is_motd_user(LUser, LServer) ->
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(username)s from motd"
                      " where username=%(LUser)s and %(LServer)H")) of
        {selected, [_|_]} ->
	    {ok, true};
	{selected, []} ->
	    {ok, false};
	_ ->
	    {error, db_failure}
    end.

set_motd_user(LUser, LServer) ->
    F = fun() ->
                ?SQL_UPSERT_T(
                   "motd",
                   ["!username=%(LUser)s",
                    "!server_host=%(LServer)s",
                    "xml=''"])
        end,
    transaction(LServer, F).

export(_Server) ->
    [{motd,
      fun(Host, #motd{server = LServer, packet = El})
            when LServer == Host ->
              XML = fxml:element_to_binary(El),
              [?SQL("delete from motd where username='' and %(LServer)H;"),
               ?SQL_INSERT(
                  "motd",
                  ["username=''",
                   "server_host=%(LServer)s",
                   "xml=%(XML)s"])];
         (_Host, _R) ->
              []
      end},
     {motd_users,
      fun(Host, #motd_users{us = {LUser, LServer}})
            when LServer == Host, LUser /= <<"">> ->
              [?SQL("delete from motd where username=%(LUser)s and %(LServer)H;"),
               ?SQL_INSERT(
                  "motd",
                  ["username=%(LUser)s",
                   "server_host=%(LServer)s",
                   "xml=''"])];
         (_Host, _R) ->
              []
      end}].

import(_, _, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
transaction(LServer, F) ->
    case ejabberd_sql:sql_transaction(LServer, F) of
	{atomic, _} -> ok;
	_ -> {error, db_failure}
    end.

parse_element(XML) ->
    case fxml_stream:parse_element(XML) of
        El when is_record(El, xmlel) ->
            {ok, El};
        _ ->
            ?ERROR_MSG("Malformed XML element in SQL table "
                       "'motd' for username='': ~ts", [XML]),
            {error, db_failure}
    end.
