%%%----------------------------------------------------------------------
%%% File    : cyrsasl_digest.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : DIGEST-MD5 SASL mechanism
%%% Created : 11 Mar 2003 by Alexey Shchepin <alexey@sevcom.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%%----------------------------------------------------------------------

-module(cyrsasl_digest).
-author('alexey@sevcom.net').

-export([start/1,
	 stop/0,
	 mech_new/4,
	 mech_step/2]).

-include("ejabberd.hrl").

-behaviour(cyrsasl).

-record(state, {step, nonce, username, authzid, get_password, check_password, auth_module,
		host, hostfqdn}).

start(_Opts) ->
    Fqdn = get_local_fqdn(),
    ?INFO_MSG("FQDN used to check DIGEST-MD5 SASL authentication: ~p", [Fqdn]),
    cyrsasl:register_mechanism("DIGEST-MD5", ?MODULE, digest).

stop() ->
    ok.

mech_new(Host, GetPassword, _CheckPassword, CheckPasswordDigest) ->
    {ok, #state{step = 1,
		nonce = randoms:get_string(),
		host = Host,
		hostfqdn = get_local_fqdn(),
		get_password = GetPassword,
		check_password = CheckPasswordDigest}}.

mech_step(#state{step = 1, nonce = Nonce} = State, _) ->
    {continue,
     "nonce=\"" ++ Nonce ++
     "\",qop=\"auth\",charset=utf-8,algorithm=md5-sess",
     State#state{step = 3}};
mech_step(#state{step = 3, nonce = Nonce} = State, ClientIn) ->
    case parse(ClientIn) of
	bad ->
	    {error, "bad-protocol"};
	KeyVals ->
	    DigestURI = xml:get_attr_s("digest-uri", KeyVals),
	    UserName = xml:get_attr_s("username", KeyVals),
	    case is_digesturi_valid(DigestURI, State#state.host, State#state.hostfqdn) of
		false ->
		    ?DEBUG("User login not authorized because digest-uri "
			   "seems invalid: ~p (checking for Host ~p, FQDN ~p)", [DigestURI,
			   State#state.host, State#state.hostfqdn]),
		    {error, "not-authorized", UserName};
		true ->
		    AuthzId = xml:get_attr_s("authzid", KeyVals),
		    case (State#state.get_password)(UserName) of
			{false, _} ->
			    {error, "not-authorized", UserName};
			{Passwd, AuthModule} ->
				case (State#state.check_password)(UserName, "",
					xml:get_attr_s("response", KeyVals),
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
				false ->
				    {error, "not-authorized", UserName};
				{false, _} ->
				    {error, "not-authorized", UserName}
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
    {error, "bad-protocol"}.

parse(S) ->
    parse1(S, "", []).

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

parse2([$\" | Cs], Key, Val, Ts) ->
    parse3(Cs, Key, Val, Ts);
parse2([C | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, [C | Val], Ts);
parse2([], _, _, _) ->
    bad.

parse3([$\" | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, Val, Ts);
parse3([$\\, C | Cs], Key, Val, Ts) ->
    parse3(Cs, Key, [C | Val], Ts);
parse3([C | Cs], Key, Val, Ts) ->
    parse3(Cs, Key, [C | Val], Ts);
parse3([], _, _, _) ->
    bad.

parse4([$, | Cs], Key, Val, Ts) ->
    parse1(Cs, "", [{Key, lists:reverse(Val)} | Ts]);
parse4([$\s | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, Val, Ts);
parse4([C | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, [C | Val], Ts);
parse4([], Key, Val, Ts) ->
    parse1([], "", [{Key, lists:reverse(Val)} | Ts]).


%% @doc Check if the digest-uri is valid.
%% RFC-2831 allows to provide the IP address in Host,
%% however ejabberd doesn't allow that.
%% If the service (for example jabber.example.org)
%% is provided by several hosts (being one of them server3.example.org),
%% then acceptable digest-uris would be:
%% xmpp/server3.example.org/jabber.example.org, xmpp/server3.example.org and
%% xmpp/jabber.example.org
%% The last version is not actually allowed by the RFC, but implemented by popular clients
is_digesturi_valid(DigestURICase, JabberDomain, JabberFQDN) ->
    DigestURI = stringprep:tolower(DigestURICase),
    case catch string:tokens(DigestURI, "/") of
	["xmpp", Host] ->
	    IsHostFqdn = is_host_fqdn(Host, JabberFQDN),
	    (Host == JabberDomain) or IsHostFqdn;
	["xmpp", Host, ServName] ->
	    IsHostFqdn = is_host_fqdn(Host, JabberFQDN),
	    (ServName == JabberDomain) and IsHostFqdn;
	_ ->
	    false
    end.

is_host_fqdn(Host, [Letter | _Tail] = Fqdn) when not is_list(Letter) ->
    Host == Fqdn;
is_host_fqdn(_Host, []) ->
    false;
is_host_fqdn(Host, [Fqdn | _FqdnTail]) when Host == Fqdn ->
    true;
is_host_fqdn(Host, [Fqdn | FqdnTail]) when Host /= Fqdn ->
    is_host_fqdn(Host, FqdnTail).

get_local_fqdn() ->
    case (catch get_local_fqdn2()) of
	Str when is_list(Str) -> Str;
	_ -> "unknown-fqdn, please configure fqdn option in ejabberd.cfg!"
    end.
get_local_fqdn2() ->
    case ejabberd_config:get_local_option(fqdn) of
	ConfiguredFqdn when is_list(ConfiguredFqdn) ->
	    ConfiguredFqdn;
	_undefined ->
	    {ok, Hostname} = inet:gethostname(),
	    {ok, {hostent, Fqdn, _, _, _, _}} = inet:gethostbyname(Hostname),
	    Fqdn
    end.

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


response(KeyVals, User, Passwd, Nonce, AuthzId, A2Prefix) ->
    Realm = xml:get_attr_s("realm", KeyVals),
    CNonce = xml:get_attr_s("cnonce", KeyVals),
    DigestURI = xml:get_attr_s("digest-uri", KeyVals),
    NC = xml:get_attr_s("nc", KeyVals),
    QOP = xml:get_attr_s("qop", KeyVals),
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



