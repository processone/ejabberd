%%%----------------------------------------------------------------------
%%% File    : cyrsasl_plain.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : PLAIN SASL mechanism
%%% Created :  8 Mar 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(cyrsasl_plain).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/1, stop/0, mech_new/2, mech_step/2, parse/1]).

-behaviour(cyrsasl).

-record(state, {check_password}).

start(_Opts) ->
    cyrsasl:register_mechanism("PLAIN", ?MODULE),
    ok.

stop() ->
    ok.

mech_new(_GetPassword, CheckPassword) ->
    {ok, #state{check_password = CheckPassword}}.

mech_step(State, ClientIn) ->
    case parse(ClientIn) of
	[AuthzId, User, Password] ->
	    case (State#state.check_password)(User, Password) of
		true ->
		    {ok, [{username, User}, {authzid, AuthzId}]};
		_ ->
		    {error, "bad-auth"}
	    end;
	_ ->
	    {error, "bad-protocol"}
    end.


parse(S) ->
    parse1(S, "", []).

parse1([0 | Cs], S, T) ->
    parse1(Cs, "", [lists:reverse(S) | T]);
parse1([C | Cs], S, T) ->
    parse1(Cs, [C | S], T);
%parse1([], [], T) ->
%    lists:reverse(T);
parse1([], S, T) ->
    lists:reverse([lists:reverse(S) | T]).




