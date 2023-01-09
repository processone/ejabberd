%%%-------------------------------------------------------------------
%%% File    : ejabberd_oauth_sql.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : OAUTH2 SQL backend
%%% Created : 27 Jul 2016 by Alexey Shchepin <alexey@process-one.net>
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module(ejabberd_oauth_sql).
-behaviour(ejabberd_oauth).

-export([init/0,
	 store/1,
	 lookup/1,
	 clean/1,
	 lookup_client/1,
	 store_client/1,
	 remove_client/1, revoke/1]).

-include("ejabberd_oauth.hrl").
-include("ejabberd_sql_pt.hrl").
-include_lib("xmpp/include/jid.hrl").
-include("logger.hrl").

init() ->
    ok.

store(R) ->
    Token = R#oauth_token.token,
    {User, Server} = R#oauth_token.us,
    SJID = jid:encode({User, Server, <<"">>}),
    Scope = str:join(R#oauth_token.scope, <<" ">>),
    Expire = R#oauth_token.expire,
    case ?SQL_UPSERT(
	    ejabberd_config:get_myname(),
	    "oauth_token",
	    ["!token=%(Token)s",
	     "jid=%(SJID)s",
	     "scope=%(Scope)s",
	     "expire=%(Expire)d"]) of
	ok ->
	    ok;
	_ ->
	    {error, db_failure}
    end.

lookup(Token) ->
    case ejabberd_sql:sql_query(
           ejabberd_config:get_myname(),
           ?SQL("select @(jid)s, @(scope)s, @(expire)d"
                " from oauth_token where token=%(Token)s")) of
        {selected, [{SJID, Scope, Expire}]} ->
            JID = jid:decode(SJID),
            US = {JID#jid.luser, JID#jid.lserver},
            {ok, #oauth_token{token = Token,
			      us = US,
			      scope = str:tokens(Scope, <<" ">>),
			      expire = Expire}};
        _ ->
            error
    end.

revoke(Token) ->
    case ejabberd_sql:sql_query(
	ejabberd_config:get_myname(),
	?SQL("delete from oauth_token where token=%(Token)s")) of
	{error, _} ->
	    {error, <<"db error">>};
	_ ->
	    ok
    end.

clean(TS) ->
    ejabberd_sql:sql_query(
      ejabberd_config:get_myname(),
      ?SQL("delete from oauth_token where expire < %(TS)d")).

lookup_client(ClientID) ->
    case ejabberd_sql:sql_query(
           ejabberd_config:get_myname(),
           ?SQL("select @(client_name)s, @(grant_type)s, @(options)s"
                " from oauth_client where client_id=%(ClientID)s")) of
        {selected, [{ClientName, SGrantType, SOptions}]} ->
            GrantType =
                case SGrantType of
                    <<"password">> -> password;
                    <<"implicit">> -> implicit
                end,
            case misc:base64_to_term(SOptions) of
                {term, Options} ->
                    {ok, #oauth_client{client_id = ClientID,
                                       client_name = ClientName,
                                       grant_type = GrantType,
                                       options = Options}};
                _ ->
                    error
            end;
        _ ->
            error
    end.

store_client(#oauth_client{client_id = ClientID,
                           client_name = ClientName,
                           grant_type = GrantType,
                           options = Options}) ->
    SGrantType =
        case GrantType of
            password -> <<"password">>;
            implicit -> <<"implicit">>
        end,
    SOptions = misc:term_to_base64(Options),
    case ?SQL_UPSERT(
	    ejabberd_config:get_myname(),
	    "oauth_client",
	    ["!client_id=%(ClientID)s",
	     "client_name=%(ClientName)s",
	     "grant_type=%(SGrantType)s",
	     "options=%(SOptions)s"]) of
	ok ->
	    ok;
	_ ->
	    {error, db_failure}
    end.

remove_client(Client) ->
    ejabberd_sql:sql_query(
      ejabberd_config:get_myname(),
      ?SQL("delete from oauth_client where client=%(Client)s")).
