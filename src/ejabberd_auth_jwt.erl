%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_jwt.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Authentication using JWT tokens
%%% Created : 16 Mar 2019 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2019   ProcessOne
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

-module(ejabberd_auth_jwt).

-author('mremond@process-one.net').

-behaviour(ejabberd_auth).

-export([start/1, stop/1, check_password/4,
	 store_type/1, plain_password_required/1
        ]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    case ejabberd_option:jwt_key(Host) of
	undefined ->
	    ?ERROR_MSG("Option jwt_key is not configured for ~s: "
		       "JWT authentication won't work", [Host]);
	_ ->
	    ok
    end.

stop(_Host) -> ok.

plain_password_required(_Host) -> true.

store_type(_Host) -> external.

-spec check_password(binary(), binary(), binary(), binary()) -> {ets_cache:tag(), boolean()}.
check_password(User, AuthzId, Server, Token) ->
    %% MREMOND: Should we move the AuthzId check at a higher level in
    %%          the call stack?
    if AuthzId /= <<>> andalso AuthzId /= User ->
            {nocache, false};
       true ->
            if Token == <<"">> -> {nocache, false};
               true ->
                    {nocache, check_jwt_token(User, Server, Token)}
            end
    end.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
check_jwt_token(User, Server, Token) ->
    JWK = ejabberd_option:jwt_key(Server),
    try jose_jwt:verify(JWK, Token) of
        {true, {jose_jwt, Fields}, Signature} ->
            ?DEBUG("jwt verify: ~p - ~p~n", [Fields, Signature]),
	    case maps:find(<<"exp">>, Fields) of
                error ->
		    %% No expiry in token => We consider token invalid:
		    false;
                {ok, Exp} ->
                    Now = erlang:system_time(second),
                    if
                        Exp > Now ->
                            case maps:find(<<"jid">>, Fields) of
                                error ->
                                    false;
                                {ok, SJID} ->
                                    try
                                        JID = jid:decode(SJID),
                                        (JID#jid.luser == User) andalso
                                        (JID#jid.lserver == Server)
                                    catch error:{bad_jid, _} ->
                                            false
                                    end
                            end;
                        true ->
                            %% return false, if token has expired
                            false
                    end
            end;
        {false, _, _} ->
            false
    catch
        error:{badarg, _} ->
            false
    end.

%% TODO: auth0 username is defined in 'jid' field, but we should
%% allow customizing the name of the field containing the username
%% to adapt to custom claims.
