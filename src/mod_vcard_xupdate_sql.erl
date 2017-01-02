%%%-------------------------------------------------------------------
%%% File    : mod_vcard_xupdate_sql.erl
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

-module(mod_vcard_xupdate_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(mod_vcard_xupdate).

%% API
-export([init/2, import/3, add_xupdate/3, get_xupdate/2, remove_xupdate/2,
	 export/1]).

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

import(_, _, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
