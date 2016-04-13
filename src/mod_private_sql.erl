%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_private_sql).

-behaviour(mod_private).

%% API
-export([init/2, set_data/3, get_data/3, get_all_data/2, remove_user/2,
	 import/1, import/2, export/1]).

-include("jlib.hrl").
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
			  odbc_queries:set_private_data(
			    LServer, LUser, XMLNS, SData)
		  end, Data)
	end,
    ejabberd_odbc:sql_transaction(LServer, F).

get_data(LUser, LServer, XMLNS) ->
    case catch odbc_queries:get_private_data(LServer, LUser, XMLNS) of
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
    case catch odbc_queries:get_private_data(LServer, LUser) of
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
    odbc_queries:del_user_private_storage(LServer, LUser).

export(_Server) ->
    [{private_storage,
      fun(Host, #private_storage{usns = {LUser, LServer, XMLNS},
                                 xml = Data})
            when LServer == Host ->
              Username = ejabberd_odbc:escape(LUser),
              LXMLNS = ejabberd_odbc:escape(XMLNS),
              SData =
                  ejabberd_odbc:escape(fxml:element_to_binary(Data)),
              odbc_queries:set_private_data_sql(Username, LXMLNS,
                                                SData);
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select username, namespace, data from private_storage;">>,
      fun([LUser, XMLNS, XML]) ->
              El = #xmlel{} = fxml_stream:parse_element(XML),
              #private_storage{usns = {LUser, LServer, XMLNS},
                               xml = El}
      end}].

import(_, _) ->
    pass.

%%%===================================================================
%%% Internal functions
%%%===================================================================
