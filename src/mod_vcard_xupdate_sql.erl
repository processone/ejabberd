%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_vcard_xupdate_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(mod_vcard_xupdate).

%% API
-export([init/2, import/2, add_xupdate/3, get_xupdate/2, remove_xupdate/2,
	 import/1, export/1]).

-include("mod_vcard_xupdate.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

add_xupdate(LUser, LServer, Hash) ->
    F = fun () ->
		?SQL_UPSERT_T(
                   "vcard_xupdate",
                   ["!username=%(LUser)s",
                    "hash=%(Hash)s"])
	end,
    ejabberd_sql:sql_transaction(LServer, F).

get_xupdate(LUser, LServer) ->
    case ejabberd_sql:sql_query(
           LServer,
           ?SQL("select @(hash)s from vcard_xupdate where"
                " username=%(LUser)s"))
	of
        {selected, [{Hash}]} -> Hash;
        _ -> undefined
    end.

remove_xupdate(LUser, LServer) ->
    F = fun () ->
		ejabberd_sql:sql_query_t(
                  ?SQL("delete from vcard_xupdate where username=%(LUser)s"))
	end,
    ejabberd_sql:sql_transaction(LServer, F).

export(_Server) ->
    [{vcard_xupdate,
      fun(Host, #vcard_xupdate{us = {LUser, LServer}, hash = Hash})
         when LServer == Host ->
              [?SQL("delete from vcard_xupdate where username=%(LUser)s;"),
               ?SQL("insert into vcard_xupdate(username, hash) values ("
                    "%(LUser)s, %(Hash)s);")];
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
