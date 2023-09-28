%%%-------------------------------------------------------------------
%%% File    : mod_private_sql.erl
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

-module(mod_private_sql).
-behaviour(mod_private).

%% API
-export([init/2, set_data/3, get_data/3, get_all_data/2, del_data/2,
	 import/3, export/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("mod_private.hrl").
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
                name = <<"private_storage">>,
                columns =
                    [#sql_column{name = <<"username">>, type = text},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"namespace">>, type = text},
                     #sql_column{name = <<"data">>, type = text},
                     #sql_column{name = <<"created_at">>, type = timestamp,
                                 default = true}],
                indices = [#sql_index{
                              columns = [<<"server_host">>, <<"username">>,
                                         <<"namespace">>],
                              unique = true}]}]}].

set_data(LUser, LServer, Data) ->
    F = fun() ->
		lists:foreach(
		  fun({XMLNS, El}) ->
			  SData = fxml:element_to_binary(El),
			  ?SQL_UPSERT_T(
			     "private_storage",
			     ["!username=%(LUser)s",
                              "!server_host=%(LServer)s",
			      "!namespace=%(XMLNS)s",
			      "data=%(SData)s"])
		  end, Data)
	end,
    case ejabberd_sql:sql_transaction(LServer, F) of
	{atomic, ok} ->
	    ok;
	_ ->
	    {error, db_failure}
    end.

get_data(LUser, LServer, XMLNS) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("select @(data)s from private_storage"
		" where username=%(LUser)s and %(LServer)H"
                " and namespace=%(XMLNS)s")) of
	{selected, [{SData}]} ->
	    parse_element(LUser, LServer, SData);
	{selected, []} ->
	    error;
	_ ->
	    {error, db_failure}
    end.

get_all_data(LUser, LServer) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("select @(namespace)s, @(data)s from private_storage"
		" where username=%(LUser)s and %(LServer)H")) of
	{selected, []} ->
	    error;
        {selected, Res} ->
            {ok, lists:flatmap(
		   fun({_, SData}) ->
			   case parse_element(LUser, LServer, SData) of
			       {ok, El} -> [El];
			       error -> []
			   end
		   end, Res)};
        _ ->
	    {error, db_failure}
    end.

del_data(LUser, LServer) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("delete from private_storage"
                " where username=%(LUser)s and %(LServer)H")) of
	{updated, _} ->
	    ok;
	_ ->
	    {error, db_failure}
    end.

export(_Server) ->
    [{private_storage,
      fun(Host, #private_storage{usns = {LUser, LServer, XMLNS},
                                 xml = Data})
            when LServer == Host ->
              SData = fxml:element_to_binary(Data),
	      [?SQL("delete from private_storage where"
		    " username=%(LUser)s and %(LServer)H and namespace=%(XMLNS)s;"),
               ?SQL_INSERT(
                  "private_storage",
                  ["username=%(LUser)s",
                   "server_host=%(LServer)s",
                   "namespace=%(XMLNS)s",
                   "data=%(SData)s"])];
         (_Host, _R) ->
              []
      end}].

import(_, _, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_element(LUser, LServer, XML) ->
    case fxml_stream:parse_element(XML) of
	El when is_record(El, xmlel) ->
	    {ok, El};
	_ ->
	    ?ERROR_MSG("Malformed XML element in SQL table "
		       "'private_storage' for user ~ts@~ts: ~ts",
		       [LUser, LServer, XML]),
	    error
    end.
