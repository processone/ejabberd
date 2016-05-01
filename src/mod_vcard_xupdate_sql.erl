%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_vcard_xupdate_sql).

-behaviour(mod_vcard_xupdate).

%% API
-export([init/2, import/2, add_xupdate/3, get_xupdate/2, remove_xupdate/2,
	 import/1, export/1]).

-include("mod_vcard_xupdate.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

add_xupdate(LUser, LServer, Hash) ->
    Username = ejabberd_sql:escape(LUser),
    SHash = ejabberd_sql:escape(Hash),
    F = fun () ->
		sql_queries:update_t(<<"vcard_xupdate">>,
				      [<<"username">>, <<"hash">>],
				      [Username, SHash],
				      [<<"username='">>, Username, <<"'">>])
	end,
    ejabberd_sql:sql_transaction(LServer, F).

get_xupdate(LUser, LServer) ->
    Username = ejabberd_sql:escape(LUser),
    case ejabberd_sql:sql_query(LServer,
				 [<<"select hash from vcard_xupdate where "
				    "username='">>,
				  Username, <<"';">>])
	of
      {selected, [<<"hash">>], [[Hash]]} -> Hash;
      _ -> undefined
    end.

remove_xupdate(LUser, LServer) ->
    Username = ejabberd_sql:escape(LUser),
    F = fun () ->
		ejabberd_sql:sql_query_t([<<"delete from vcard_xupdate where username='">>,
					   Username, <<"';">>])
	end,
    ejabberd_sql:sql_transaction(LServer, F).

export(_Server) ->
    [{vcard_xupdate,
      fun(Host, #vcard_xupdate{us = {LUser, LServer}, hash = Hash})
            when LServer == Host ->
              Username = ejabberd_sql:escape(LUser),
              SHash = ejabberd_sql:escape(Hash),
              [[<<"delete from vcard_xupdate where username='">>,
                Username, <<"';">>],
               [<<"insert into vcard_xupdate(username, "
                  "hash) values ('">>,
                Username, <<"', '">>, SHash, <<"');">>]];
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select username, hash from vcard_xupdate;">>,
      fun([LUser, Hash]) ->
              #vcard_xupdate{us = {LUser, LServer}, hash = Hash}
      end}].

import(_LServer, _) ->
    pass.

%%%===================================================================
%%% Internal functions
%%%===================================================================
