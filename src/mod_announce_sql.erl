%%%-------------------------------------------------------------------
%%% File    : mod_announce_sql.erl
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

-module(mod_announce_sql).

-behaviour(mod_announce).

-compile([{parse_transform, ejabberd_sql_pt}]).

%% API
-export([init/2, set_motd_users/2, set_motd/2, delete_motd/1,
	 get_motd/1, is_motd_user/2, set_motd_user/2, import/3,
	 export/1]).

-include("xmpp.hrl").
-include("mod_announce.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

set_motd_users(LServer, USRs) ->
    F = fun() ->
		lists:foreach(
		  fun({U, _S, _R}) ->
                          ?SQL_UPSERT_T(
                             "motd",
                             ["!username=%(U)s",
                              "xml=''"])
		  end, USRs)
	end,
    ejabberd_sql:sql_transaction(LServer, F).

set_motd(LServer, Packet) ->
    XML = fxml:element_to_binary(Packet),
    F = fun() ->
                ?SQL_UPSERT_T(
                   "motd",
                   ["!username=''",
                    "xml=%(XML)s"])
	end,
    ejabberd_sql:sql_transaction(LServer, F).

delete_motd(LServer) ->
    F = fun() ->
                ejabberd_sql:sql_query_t(?SQL("delete from motd"))
	end,
    ejabberd_sql:sql_transaction(LServer, F).

get_motd(LServer) ->
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(xml)s from motd where username=''")) of
        {selected, [{XML}]} ->
            case fxml_stream:parse_element(XML) of
                {error, _} ->
                    error;
                Packet ->
		    {ok, Packet}
	    end;
	_ ->
	    error
    end.

is_motd_user(LUser, LServer) ->
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(username)s from motd"
                      " where username=%(LUser)s")) of
        {selected, [_|_]} ->
	    true;
	_ ->
	    false
    end.

set_motd_user(LUser, LServer) ->
    F = fun() ->
                ?SQL_UPSERT_T(
                   "motd",
                   ["!username=%(LUser)s",
                    "xml=''"])
        end,
    ejabberd_sql:sql_transaction(LServer, F).

export(_Server) ->
    [{motd,
      fun(Host, #motd{server = LServer, packet = El})
            when LServer == Host ->
              XML = fxml:element_to_binary(El),
              [?SQL("delete from motd where username='';"),
               ?SQL("insert into motd(username, xml) values ('', %(XML)s);")];
         (_Host, _R) ->
              []
      end},
     {motd_users,
      fun(Host, #motd_users{us = {LUser, LServer}})
            when LServer == Host, LUser /= <<"">> ->
              [?SQL("delete from motd where username=%(LUser)s;"),
               ?SQL("insert into motd(username, xml) values (%(LUser)s, '');")];
         (_Host, _R) ->
              []
      end}].

import(_, _, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
