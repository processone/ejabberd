%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_jwt.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Authentication using JWT tokens
%%% Created : 16 Mar 2019 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
	 store_type/1, plain_password_required/1,
         user_exists/2, use_cache/1
        ]).
%% 'ejabberd_hooks' callback:
-export([check_decoded_jwt/5]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    %% We add our default JWT verifier with hook priority 100.
    %% So if you need to check or verify your custom JWT before the
    %% default verifier, It's better to use this hook with priority
    %% little than 100 and return bool() or {stop, bool()} in your own
    %% callback function.
    ejabberd_hooks:add(check_decoded_jwt, Host, ?MODULE, check_decoded_jwt, 100),
    case ejabberd_option:jwt_key(Host) of
	undefined ->
	    ?ERROR_MSG("Option jwt_key is not configured for ~ts: "
		       "JWT authentication won't work", [Host]);
	_ ->
	    ok
    end.

stop(Host) ->
    ejabberd_hooks:delete(check_decoded_jwt, Host, ?MODULE, check_decoded_jwt, 100).

plain_password_required(_Host) -> true.

store_type(_Host) -> external.

-spec check_password(binary(), binary(), binary(), binary()) -> {ets_cache:tag(), boolean() | {stop, boolean()}}.
check_password(User, AuthzId, Server, Token) ->
    %% MREMOND: Should we move the AuthzId check at a higher level in
    %%          the call stack?
    if AuthzId /= <<>> andalso AuthzId /= User ->
            {nocache, false};
       true ->
            if Token == <<"">> -> {nocache, false};
               true ->
                    Res = check_jwt_token(User, Server, Token),
                    Rule = ejabberd_option:jwt_auth_only_rule(Server),
                    case acl:match_rule(Server, Rule,
                                        jid:make(User, Server, <<"">>)) of
                        deny ->
                            {nocache, Res};
                        allow ->
                            {nocache, {stop, Res}}
                    end
            end
    end.

user_exists(User, Host) ->
  %% Checking that the user has an active session
  %% If the session was negociated by the JWT auth method then we define that the user exists
  %% Any other cases will return that the user doesn't exist
  {nocache, case ejabberd_sm:get_user_info(User, Host) of
              [{_, Info}] -> proplists:get_value(auth_module, Info) == ejabberd_auth_jwt;
              _ -> false
            end}.

use_cache(_) ->
    false.

%%%----------------------------------------------------------------------
%%% 'ejabberd_hooks' callback
%%%----------------------------------------------------------------------
check_decoded_jwt(true, Fields, _Signature, Server, User) ->
    JidField = ejabberd_option:jwt_jid_field(Server),
    case maps:find(JidField, Fields) of
        {ok, SJid} when is_binary(SJid) ->
            try
                JID = jid:decode(SJid),
                JID#jid.luser == User andalso JID#jid.lserver == Server
            catch error:{bad_jid, _} ->
                false
            end;
        _ -> % error | {ok, _UnknownType}
            false
    end;
check_decoded_jwt(Acc, _, _, _, _) ->
    Acc.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
check_jwt_token(User, Server, Token) ->
    JWK = ejabberd_option:jwt_key(Server),
    try jose_jwt:verify(JWK, Token) of
        {true, {jose_jwt, Fields}, Signature} ->
            Now = erlang:system_time(second),
            ?DEBUG("jwt verify at system timestamp ~p: ~p - ~p~n", [Now, Fields, Signature]),
	    case maps:find(<<"exp">>, Fields) of
                error ->
		    %% No expiry in token => We consider token invalid:
		    false;
                {ok, Exp} ->
                    if
                        Exp > Now ->
                            ejabberd_hooks:run_fold(
                                check_decoded_jwt,
                                Server,
                                true,
                                [Fields, Signature, Server, User]
                            );
                        true ->
                            %% return false, if token has expired
                            false
                    end
            end;
        {false, _, _} ->
            false
    catch
        A:B ->
            ?DEBUG("jose_jwt:verify failed ~n for account ~p@~p~n "
                   " JWK and token: ~p~n with error: ~p",
                   [User, Server, {JWK, Token}, {A, B}]),
            false
    end.
