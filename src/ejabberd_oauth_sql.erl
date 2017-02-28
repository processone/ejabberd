%%%-------------------------------------------------------------------
%%% File    : ejabberd_oauth_sql.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : OAUTH2 SQL backend
%%% Created : 27 Jul 2016 by Alexey Shchepin <alexey@process-one.net>
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module(ejabberd_oauth_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-export([init/0,
         store/1,
         lookup/1,
         clean/1]).

-include("ejabberd_oauth.hrl").
-include("ejabberd.hrl").
-include("ejabberd_sql_pt.hrl").
-include("jid.hrl").

init() ->
    ok.

store(R) ->
    Token = R#oauth_token.token,
    {User, Server} = R#oauth_token.us,
    SJID = jid:encode({User, Server, <<"">>}),
    Scope = str:join(R#oauth_token.scope, <<" ">>),
    Expire = R#oauth_token.expire,
    ?SQL_UPSERT(
       ?MYNAME,
       "oauth_token",
       ["!token=%(Token)s",
        "jid=%(SJID)s",
        "scope=%(Scope)s",
        "expire=%(Expire)d"]).

lookup(Token) ->
    case ejabberd_sql:sql_query(
           ?MYNAME,
           ?SQL("select @(jid)s, @(scope)s, @(expire)d"
                " from oauth_token where token=%(Token)s")) of
        {selected, [{SJID, Scope, Expire}]} ->
            JID = jid:decode(SJID),
            US = {JID#jid.luser, JID#jid.lserver},
            #oauth_token{token = Token,
                         us = US,
                         scope = str:tokens(Scope, <<" ">>),
                         expire = Expire};
        _ ->
            false
    end.

clean(TS) ->
    ejabberd_sql:sql_query(
      ?MYNAME,
      ?SQL("delete from oauth_token where expire < %(TS)d")).

