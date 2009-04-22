%%%----------------------------------------------------------------------
%%% File    : cyrsasl_digest.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : DIGEST-MD5 SASL mechanism
%%% Created : 11 Mar 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(cyrsasl_digest).
-author('alexey@sevcom.net').

-export([start/1,
	 stop/0,
	 mech_new/4,
	 mech_step/2]).

-include("ejabberd.hrl").

-behaviour(cyrsasl).

%% @type mechstate() = {state, Step, Nonce, Username, AuthzId, GetPassword, CheckPassword, AuthModule, Host}
%%     Step = 1 | 3 | 5
%%     Nonce = string()
%%     Username = string()
%%     AuthzId = string()
%%     GetPassword = function()
%%     AuthModule = atom()
%%     Host = string().

-record(state, {step, nonce, username, authzid, get_password, check_password, auth_module,
		host}).

%% @spec (Opts) -> true
%%     Opts = term()

start(_Opts) ->
    cyrsasl:register_mechanism("DIGEST-MD5", ?MODULE, true).

%% @spec () -> ok

stop() ->
    ok.

%% @spec (Host, GetPassword, CheckPassword, CheckPasswordDigest) -> {ok, State}
%%     Host = string()
%%     GetPassword = function()
%%     CheckPassword = function()
%%     State = mechstate()

mech_new(Host, GetPassword, _CheckPassword, CheckPasswordDigest) ->
    {ok, #state{step = 1,
		nonce = randoms:get_string(),
		host = Host,
		get_password = GetPassword,
		check_password = CheckPasswordDigest}}.

%% @spec (State, ClientIn) -> Ok | Continue | Error
%%     State = mechstate()
%%     ClientIn = string()
%%     Ok = {ok, Props}
%%         Props = [Prop]
%%         Prop = {username, Username} | {authzid, AuthzId} | {auth_module, AuthModule}
%%         Username = string()
%%         AuthzId = string()
%%         AuthModule = atom()
%%     Continue = {continue, ServerOut, New_State}
%%         ServerOut = string()
%%         New_State = mechstate()
%%     Error = {error, Reason} | {error, Reason, Username}
%%         Reason = term()

mech_step(#state{step = 1, nonce = Nonce} = State, _) ->
    {continue,
     "nonce=\"" ++ Nonce ++
     "\",qop=\"auth\",charset=utf-8,algorithm=md5-sess",
     State#state{step = 3}};
mech_step(#state{step = 3, nonce = Nonce} = State, ClientIn) ->
    case parse(ClientIn) of
	bad ->
	    {error, 'bad-protocol'};
	KeyVals ->
	    DigestURI = proplists:get_value("digest-uri", KeyVals, ""),
	    UserName = proplists:get_value("username", KeyVals, ""),
	    case is_digesturi_valid(DigestURI, State#state.host) of
		false ->
		    ?DEBUG("User login not authorized because digest-uri "
			   "seems invalid: ~p", [DigestURI]),
		    {error, 'not-authorized', UserName};
		true ->
		    AuthzId = proplists:get_value("authzid", KeyVals, ""),
		    case (State#state.get_password)(UserName) of
			{false, _} ->
			    {error, 'not-authorized', UserName};
			{Passwd, AuthModule} ->
				case (State#state.check_password)(UserName, Passwd,
					proplists:get_value("response", KeyVals, ""),
					fun(PW) -> response(KeyVals, UserName, PW, Nonce, AuthzId,
						"AUTHENTICATE") end) of
				{true, _} ->
				    RspAuth = response(KeyVals,
						       UserName, Passwd,
						       Nonce, AuthzId, ""),
				    {continue,
				     "rspauth=" ++ RspAuth,
				     State#state{step = 5,
						 auth_module = AuthModule,
						 username = UserName,
						 authzid = AuthzId}};
				{false, _} ->
				    {error, 'not-authorized', UserName}
			    end
		    end
	    end
    end;
mech_step(#state{step = 5,
		 auth_module = AuthModule,
		 username = UserName,
		 authzid = AuthzId}, "") ->
    {ok, [{username, UserName}, {authzid, AuthzId},
	  {auth_module, AuthModule}]};
mech_step(A, B) ->
    ?DEBUG("SASL DIGEST: A ~p B ~p", [A,B]),
    {error, 'bad-protocol'}.

%% @spec (S) -> [{Key, Value}] | bad
%%     S = string()
%%     Key = string()
%%     Value = string()

parse(S) ->
    parse1(S, "", []).

%% @hidden

parse1([$= | Cs], S, Ts) ->
    parse2(Cs, lists:reverse(S), "", Ts);
parse1([$, | Cs], [], Ts) ->
    parse1(Cs, [], Ts);
parse1([$\s | Cs], [], Ts) ->
    parse1(Cs, [], Ts);
parse1([C | Cs], S, Ts) ->
    parse1(Cs, [C | S], Ts);
parse1([], [], T) ->
    lists:reverse(T);
parse1([], _S, _T) ->
    bad.

%% @hidden

parse2([$\" | Cs], Key, Val, Ts) ->
    parse3(Cs, Key, Val, Ts);
parse2([C | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, [C | Val], Ts);
parse2([], _, _, _) ->
    bad.

%% @hidden

parse3([$\" | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, Val, Ts);
parse3([$\\, C | Cs], Key, Val, Ts) ->
    parse3(Cs, Key, [C | Val], Ts);
parse3([C | Cs], Key, Val, Ts) ->
    parse3(Cs, Key, [C | Val], Ts);
parse3([], _, _, _) ->
    bad.

%% @hidden

parse4([$, | Cs], Key, Val, Ts) ->
    parse1(Cs, "", [{Key, lists:reverse(Val)} | Ts]);
parse4([$\s | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, Val, Ts);
parse4([C | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, [C | Val], Ts);
parse4([], Key, Val, Ts) ->
    parse1([], "", [{Key, lists:reverse(Val)} | Ts]).


%% @spec (DigestURICase, JabberHost) -> bool()
%%     DigestURICase = string()
%%     JabberHost = string()
%%
%% @doc Check if the digest-uri is valid.
%% RFC-2831 allows to provide the IP address in Host,
%% however ejabberd doesn't allow that.
%% If the service (for example jabber.example.org)
%% is provided by several hosts (being one of them server3.example.org),
%% then digest-uri can be like xmpp/server3.example.org/jabber.example.org
%% In that case, ejabberd only checks the service name, not the host.

is_digesturi_valid(DigestURICase, JabberHost) ->
    DigestURI = exmpp_stringprep:to_lower(DigestURICase),
    case catch string:tokens(DigestURI, "/") of
	["xmpp", Host] when Host == JabberHost ->
	    true;
	["xmpp", _Host, ServName] when ServName == JabberHost ->
	    true;
	_ ->
	    false
    end.




%% @hidden

digit_to_xchar(D) when (D >= 0) and (D < 10) ->
    D + 48;
digit_to_xchar(D) ->
    D + 87.

%% @hidden

hex(S) ->
    hex(S, []).

%% @hidden

hex([], Res) ->
    lists:reverse(Res);
hex([N | Ns], Res) ->
    hex(Ns, [digit_to_xchar(N rem 16),
	     digit_to_xchar(N div 16) | Res]).


%% @spec (KeyVals, User, Passwd, Nonce, AuthzId, A2Prefix) -> string()
%%     KeyVals = [{Key, Value}]
%%         Key = string()
%%         Value = string()
%%     User = string()
%%     Passwd = string()
%%     Nonce = string()
%%     AuthzId = nil() | string()
%%     A2Prefix = string()

response(KeyVals, User, Passwd, Nonce, AuthzId, A2Prefix) ->
    Realm = proplists:get_value("realm", KeyVals, ""),
    CNonce = proplists:get_value("cnonce", KeyVals, ""),
    DigestURI = proplists:get_value("digest-uri", KeyVals, ""),
    NC = proplists:get_value("nc", KeyVals, ""),
    QOP = proplists:get_value("qop", KeyVals, ""),
    A1 = case AuthzId of
	     "" ->
		 binary_to_list(
		   crypto:md5(User ++ ":" ++ Realm ++ ":" ++ Passwd)) ++
		     ":" ++ Nonce ++ ":" ++ CNonce;
	     _ ->
		 binary_to_list(
		   crypto:md5(User ++ ":" ++ Realm ++ ":" ++ Passwd)) ++
		     ":" ++ Nonce ++ ":" ++ CNonce ++ ":" ++ AuthzId
	 end,
    A2 = case QOP of
	     "auth" ->
		 A2Prefix ++ ":" ++ DigestURI;
	     _ ->
		 A2Prefix ++ ":" ++ DigestURI ++
		     ":00000000000000000000000000000000"
	 end,
    T = hex(binary_to_list(crypto:md5(A1))) ++ ":" ++ Nonce ++ ":" ++
	NC ++ ":" ++ CNonce ++ ":" ++ QOP ++ ":" ++
	hex(binary_to_list(crypto:md5(A2))),
    hex(binary_to_list(crypto:md5(T))).



