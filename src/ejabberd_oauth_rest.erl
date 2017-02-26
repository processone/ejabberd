%%%-------------------------------------------------------------------
%%% File    : ejabberd_oauth_rest.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : OAUTH2 REST backend
%%% Created : 26 Jul 2016 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_oauth_rest).

-export([init/0,
         store/1,
         lookup/1,
         clean/1,
         opt_type/1]).

-include("ejabberd.hrl").
-include("ejabberd_oauth.hrl").
-include("logger.hrl").
-include("jid.hrl").

init() ->
    rest:start(?MYNAME),
    ok.

store(R) ->
    Path = path(<<"store">>),
    %% Retry 2 times, with a backoff of 500millisec
    {User, Server} = R#oauth_token.us,
    SJID = jid:encode({User, Server, <<"">>}),
    case rest:with_retry(
           post,
           [?MYNAME, Path, [],
            {[{<<"token">>, R#oauth_token.token},
              {<<"user">>, SJID},
              {<<"scope">>, R#oauth_token.scope},
              {<<"expire">>, R#oauth_token.expire}
             ]}], 2, 500) of
        {ok, Code, _} when Code == 200 orelse Code == 201 ->
            ok;
        Err ->
            ?ERROR_MSG("failed to store oauth record ~p: ~p", [R, Err]),
            {error, Err}
    end.

lookup(Token) ->
    Path = path(<<"lookup">>),
    case rest:with_retry(post, [?MYNAME, Path, [],
                                {[{<<"token">>, Token}]}],
                         2, 500) of
        {ok, 200, {Data}} ->
            SJID = proplists:get_value(<<"user">>, Data, <<>>),
            JID = jid:decode(SJID),
            US = {JID#jid.luser, JID#jid.lserver},
            Scope = proplists:get_value(<<"scope">>, Data, []),
            Expire = proplists:get_value(<<"expire">>, Data, 0),
            #oauth_token{token = Token,
                         us = US,
                         scope = Scope,
                         expire = Expire};
        {ok, 404, _Resp} ->
            false;
        Other ->
            ?ERROR_MSG("Unexpected response for oauth lookup: ~p", [Other]),
            {error, rest_failed}
    end.

clean(_TS) ->
    ok.

path(Path) ->
    Base = ejabberd_config:get_option(ext_api_path_oauth,
                                      fun(X) -> iolist_to_binary(X) end,
                                      <<"/oauth">>),
    <<Base/binary, "/", Path/binary>>.


opt_type(ext_api_path_oauth) ->
    fun (X) -> iolist_to_binary(X) end;
opt_type(_) -> [ext_api_path_oauth].
