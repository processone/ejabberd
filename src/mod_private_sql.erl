%%%-------------------------------------------------------------------
%%% File    : mod_private_sql.erl
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

-module(mod_private_sql).

-behaviour(mod_private).

%% API
-export([init/2, set_data/3, get_data/3, get_all_data/2, remove_user/2,
	 import/3, export/1]).

-include("xmpp.hrl").
-include("mod_private.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

set_data(LUser, LServer, Data) ->
    F = fun() ->
		lists:foreach(
		  fun({XMLNS, El}) ->
			  SData = fxml:element_to_binary(El),
			  sql_queries:set_private_data(
			    LServer, LUser, XMLNS, SData)
		  end, Data)
	end,
    ejabberd_sql:sql_transaction(LServer, F).

get_data(LUser, LServer, XMLNS) ->
    case catch sql_queries:get_private_data(LServer, LUser, XMLNS) of
	{selected, [{SData}]} ->
	    case fxml_stream:parse_element(SData) of
		Data when is_record(Data, xmlel) ->
		    {ok, Data};
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

get_all_data(LUser, LServer) ->
    case catch sql_queries:get_private_data(LServer, LUser) of
        {selected, Res} ->
            lists:flatmap(
              fun({_, SData}) ->
                      case fxml_stream:parse_element(SData) of
                          #xmlel{} = El ->
                              [El];
                          _ ->
                              []
                      end
              end, Res);
        _ ->
            []
    end.

remove_user(LUser, LServer) ->
    sql_queries:del_user_private_storage(LServer, LUser).

export(_Server) ->
    [{private_storage,
      fun(Host, #private_storage{usns = {LUser, LServer, XMLNS},
                                 xml = Data})
            when LServer == Host ->
              SData = fxml:element_to_binary(Data),
              sql_queries:set_private_data_sql(LUser, XMLNS, SData);
         (_Host, _R) ->
              []
      end}].

import(_, _, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
