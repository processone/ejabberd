%%%----------------------------------------------------------------------
%%% File    : cyrsasl_digest.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : DIGEST-MD5 SASL mechanism
%%% Created : 11 Mar 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(cyrsasl_digest).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/1,
	 stop/0,
	 mech_new/0,
	 mech_step/2]).

-behaviour(cyrsasl).
%-behaviour(gen_mod).

-record(state, {step, nonce, username}).

start(Opts) ->
    cyrsasl:register_mechanism("DIGEST-MD5", ?MODULE),
    ok.

stop() ->
    ok.

mech_new() ->
    {ok, #state{step = 1,
		nonce = randoms:get_string()}}.

mech_step(#state{step = 1, nonce = Nonce} = State, "") ->
    {continue,
     "nonce=\"" ++ jlib:encode_base64(Nonce) ++
     "\",qop=\"auth,auth-int\",charset=utf-8,algorithm=md5-sess",
     State#state{step = 3}};
mech_step(#state{step = 3, nonce = Nonce} = State, ClientIn) ->
    case parse(ClientIn) of
	bad ->
	    {error, "454"};
	KeyVals ->
	    UserName = xml:get_attr_s("username", KeyVals),
	    case ejabberd_auth:get_password(UserName) of
		false ->
		    {error, "454"};
		Passwd ->
		    Response = response(KeyVals, UserName, Passwd,
					"AUTHENTICATE"),
		    case xml:get_attr_s("response", KeyVals) of
			Response ->
			    RspAuth = response(KeyVals, UserName, Passwd, ""),
			    {continue,
			     "rspauth=" ++ RspAuth,
			     State#state{step = 5, username = UserName}};
			_ ->
			    {error, "454"}
		    end
	    end
    end;
mech_step(#state{step = 5, username = UserName} = State, "") ->
    {ok, [{username, UserName}]};
mech_step(A, B) ->
    io:format("SASL DIGEST: A ~p B ~p", [A,B]),
    {error, "454"}.


parse(S) ->
    parse1(S, "", []).

parse1([$= | Cs], S, Ts) ->
    parse2(Cs, lists:reverse(S), "", Ts);
parse1([C | Cs], S, Ts) ->
    parse1(Cs, [C | S], Ts);
parse1([], [], T) ->
    lists:reverse(T);
parse1([], S, T) ->
    bad.

parse2([$" | Cs], Key, Val, Ts) ->
    parse3(Cs, Key, Val, Ts);
parse2([C | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, [C | Val], Ts);
parse2([], _, _, _) ->
    bad.

parse3([$" | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, Val, Ts);
parse3([C | Cs], Key, Val, Ts) ->
    parse3(Cs, Key, [C | Val], Ts);
parse3([], _, _, _) ->
    bad.

parse4([$, | Cs], Key, Val, Ts) ->
    parse1(Cs, "", [{Key, lists:reverse(Val)} | Ts]);
parse4([C | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, [C | Val], Ts);
parse4([], Key, Val, Ts) ->
    parse1([], "", [{Key, lists:reverse(Val)} | Ts]).






digit_to_xchar(D) when (D >= 0) and (D < 10) ->
    D + 48;
digit_to_xchar(D) ->
    D + 87.

hex(S) ->
    hex(S, []).

hex([], Res) ->
    lists:reverse(Res);
hex([N | Ns], Res) ->
    hex(Ns, [digit_to_xchar(N rem 16),
	     digit_to_xchar(N div 16) | Res]).


response(KeyVals, User, Passwd, A2Prefix) ->
    Realm = xml:get_attr_s("realm", KeyVals),
    Nonce = xml:get_attr_s("nonce", KeyVals),
    CNonce = xml:get_attr_s("cnonce", KeyVals),
    DigestURI = xml:get_attr_s("digest-uri", KeyVals),
    NC = xml:get_attr_s("nc", KeyVals),
    QOP = xml:get_attr_s("qop", KeyVals),
    A1 = binary_to_list(crypto:md5(User ++ ":" ++ Realm ++ ":" ++ Passwd)) ++
	":" ++ Nonce ++ ":" ++ CNonce,
    case QOP of
	"auth" ->
	    A2 = A2Prefix ++ ":" ++ DigestURI;
	_ ->
	    A2 = A2Prefix ++ ":" ++ DigestURI ++
		":00000000000000000000000000000000"
    end,
    T = hex(binary_to_list(crypto:md5(A1))) ++ ":" ++ Nonce ++ ":" ++
	NC ++ ":" ++ CNonce ++ ":" ++ QOP ++ ":" ++
	hex(binary_to_list(crypto:md5(A2))),
    hex(binary_to_list(crypto:md5(T))).



