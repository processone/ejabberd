%%%-------------------------------------------------------------------
%%% File    : ejabberd_oauth_rest.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : OAUTH2 REST backend
%%% Created : 26 Jul 2016 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2021   ProcessOne
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

-module(ejabberd_oauth_rest).
-behaviour(ejabberd_oauth).

-export([init/0,
         store/1,
         lookup/1,
         clean/1,
         lookup_client/1,
         store_client/1]).

-include("ejabberd_oauth.hrl").
-include("logger.hrl").
-include_lib("xmpp/include/jid.hrl").

init() ->
    rest:start(ejabberd_config:get_myname()),
    ok.

store(R) ->
    Path = path(<<"store">>),
    %% Retry 2 times, with a backoff of 500millisec
    {User, Server} = R#oauth_token.us,
    SJID = jid:encode({User, Server, <<"">>}),
    case rest:with_retry(
           post,
           [ejabberd_config:get_myname(), Path, [],
            {[{<<"token">>, R#oauth_token.token},
              {<<"user">>, SJID},
              {<<"scope">>, R#oauth_token.scope},
              {<<"expire">>, R#oauth_token.expire}
             ]}], 2, 500) of
        {ok, Code, _} when Code == 200 orelse Code == 201 ->
            ok;
        Err ->
            ?ERROR_MSG("Failed to store oauth record ~p: ~p", [R, Err]),
            {error, db_failure}
    end.

lookup(Token) ->
    Path = path(<<"lookup">>),
    case rest:with_retry(post, [ejabberd_config:get_myname(), Path, [],
                                {[{<<"token">>, Token}]}],
                         2, 500) of
        {ok, 200, {Data}} ->
            SJID = proplists:get_value(<<"user">>, Data, <<>>),
            JID = jid:decode(SJID),
            US = {JID#jid.luser, JID#jid.lserver},
            Scope = proplists:get_value(<<"scope">>, Data, []),
            Expire = proplists:get_value(<<"expire">>, Data, 0),
            {ok, #oauth_token{token = Token,
			      us = US,
			      scope = Scope,
			      expire = Expire}};
        {ok, 404, _Resp} ->
            error;
        Other ->
            ?ERROR_MSG("Unexpected response for oauth lookup: ~p", [Other]),
            case ejabberd_option:oauth_cache_rest_failure_life_time() of
                infinity -> error;
                Time -> {cache_with_timeout, error, Time}
	    end
    end.

clean(_TS) ->
    ok.

path(Path) ->
    Base = ejabberd_option:ext_api_path_oauth(),
    <<Base/binary, "/", Path/binary>>.

store_client(#oauth_client{client_id = ClientID,
                           client_name = ClientName,
                           grant_type = GrantType,
                           options = Options} = R) ->
    Path = path(<<"store_client">>),
    SGrantType =
        case GrantType of
            password -> <<"password">>;
            implicit -> <<"implicit">>
        end,
    SOptions = misc:term_to_base64(Options),
    %% Retry 2 times, with a backoff of 500millisec
    case rest:with_retry(
           post,
           [ejabberd_config:get_myname(), Path, [],
            {[{<<"client_id">>, ClientID},
              {<<"client_name">>, ClientName},
              {<<"grant_type">>, SGrantType},
              {<<"options">>, SOptions}
             ]}], 2, 500) of
        {ok, Code, _} when Code == 200 orelse Code == 201 ->
            ok;
        Err ->
            ?ERROR_MSG("Failed to store oauth record ~p: ~p", [R, Err]),
            {error, db_failure}
    end.

lookup_client(ClientID) ->
    Path = path(<<"lookup_client">>),
    case rest:with_retry(post, [ejabberd_config:get_myname(), Path, [],
                                {[{<<"client_id">>, ClientID}]}],
                         2, 500) of
        {ok, 200, {Data}} ->
            ClientName = proplists:get_value(<<"client_name">>, Data, <<>>),
            SGrantType = proplists:get_value(<<"grant_type">>, Data, <<>>),
            GrantType =
                case SGrantType of
                    <<"password">> -> password;
                    <<"implicit">> -> implicit
                end,
            SOptions = proplists:get_value(<<"options">>, Data, <<>>),
            case misc:base64_to_term(SOptions) of
                {term, Options} ->
                    {ok, #oauth_client{client_id = ClientID,
                                       client_name = ClientName,
                                       grant_type = GrantType,
                                       options = Options}};
                _ ->
                    error
            end;
        {ok, 404, _Resp} ->
            error;
        Other ->
            ?ERROR_MSG("Unexpected response for oauth lookup: ~p", [Other]),
	    error
    end.
