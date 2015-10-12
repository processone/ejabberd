%%%----------------------------------------------------------------------
%%% File    : cyrsasl_oauth.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : X-OAUTH2 SASL mechanism
%%% Created : 17 Sep 2015 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-module(cyrsasl_oauth).

-author('alexey@process-one.net').

-export([start/1, stop/0, mech_new/4, mech_step/2, parse/1]).

-behaviour(cyrsasl).

-record(state, {host}).

start(_Opts) ->
    cyrsasl:register_mechanism(<<"X-OAUTH2">>, ?MODULE, plain),
    ok.

stop() -> ok.

mech_new(Host, _GetPassword, _CheckPassword, _CheckPasswordDigest) ->
    {ok, #state{host = Host}}.

mech_step(State, ClientIn) ->
    case prepare(ClientIn) of
        [AuthzId, User, Token] ->
            case ejabberd_oauth:check_token(
                   User, State#state.host, <<"sasl_auth">>, Token) of
                true ->
                    {ok,
                     [{username, User}, {authzid, AuthzId},
                      {auth_module, ejabberd_oauth}]};
                false ->
                    {error, <<"not-authorized">>, User}
            end;
        _ -> {error, <<"bad-protocol">>}
    end.

prepare(ClientIn) ->
    case parse(ClientIn) of
        [<<"">>, UserMaybeDomain, Token] ->
            case parse_domain(UserMaybeDomain) of
                %% <NUL>login@domain<NUL>pwd
                [User, _Domain] -> [UserMaybeDomain, User, Token];
                %% <NUL>login<NUL>pwd
                [User] -> [<<"">>, User, Token]
            end;
        %% login@domain<NUL>login<NUL>pwd
        [AuthzId, User, Token] -> [AuthzId, User, Token];
        _ -> error
    end.

parse(S) -> parse1(binary_to_list(S), "", []).

parse1([0 | Cs], S, T) ->
    parse1(Cs, "", [list_to_binary(lists:reverse(S)) | T]);
parse1([C | Cs], S, T) -> parse1(Cs, [C | S], T);
%parse1([], [], T) ->
%    lists:reverse(T);
parse1([], S, T) ->
    lists:reverse([list_to_binary(lists:reverse(S)) | T]).

parse_domain(S) -> parse_domain1(binary_to_list(S), "", []).

parse_domain1([$@ | Cs], S, T) ->
    parse_domain1(Cs, "", [list_to_binary(lists:reverse(S)) | T]);
parse_domain1([C | Cs], S, T) ->
    parse_domain1(Cs, [C | S], T);
parse_domain1([], S, T) ->
    lists:reverse([list_to_binary(lists:reverse(S)) | T]).
