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

-export([start/1, stop/0, mech_new/0, mech_step/2, parse/1]).

-behaviour(cyrsasl).
%-behaviour(gen_mod).

start(Opts) ->
    cyrsasl:register_mechanism("PLAIN", ?MODULE),
    ok.

stop() ->
    ok.

mech_new() ->
    {ok, []}.

mech_step(State, ClientIn) ->
    case parse(ClientIn) of
	[_, User, Password] ->
	    case ejabberd_auth:check_password(User, Password) of
		true ->
		    {ok, [{username, User}]};
		_ ->
		    {error, "454"}
	    end;
	_ ->
	    {error, "454"}
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




