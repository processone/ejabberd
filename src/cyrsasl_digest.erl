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
	 mech_new/1,
	 mech_step/2]).

-include("ejabberd.hrl").
-include("cyrsasl.hrl").

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
    cyrsasl:register_mechanism("DIGEST-MD5", ?MODULE, digest).

%% @spec () -> ok

stop() ->
    ok.

mech_new(#sasl_params{host=Host, get_password=GetPassword,
		      check_password_digest=CheckPasswordDigest}) ->
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
%%     Error = {error, Reason} | {error, Reason, Text, Username}
%%         Reason = term()

mech_step(#state{step = 1, nonce = Nonce} = State, _) ->
    {continue,
     "nonce=\"" ++ Nonce ++
     "\",qop=\"auth\",charset=utf-8,algorithm=md5-sess",
     State#state{step = 3}};
mech_step(#state{step = 3, nonce = Nonce} = State, ClientIn) ->
    case parse(ClientIn) of
	bad ->
	    {error, 'malformed-request'};
	KeyVals ->
	    DigestURI = proplists:get_value("digest-uri", KeyVals, ""),
	    UserName = proplists:get_value("username", KeyVals, ""),
	    case is_digesturi_valid(DigestURI, State#state.host) of
		false ->
		    ?DEBUG("User login not authorized because digest-uri "
			   "seems invalid: ~p", [DigestURI]),
		    {error, 'not-authorized', "", UserName};
		true ->
		    AuthzId = proplists:get_value("authzid", KeyVals, ""),
		    case (State#state.get_password)(UserName) of
			{false, _} ->
			    {error, 'not-authorized', "", UserName};
			{Passwd, AuthModule} ->
				case (State#state.check_password)(UserName, "",
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
				false ->
				    {error, 'not-authorized', "", UserName};
				{false, _} ->
				    {error, 'not-authorized', "", UserName}
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
    {error, 'malformed-request'}.

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
    %% handle non-fully latin-1 strings as specified                             
    %% on RFC 2831 Section 2.1.2.1 (EJAB-476)                                    
    SUser = sanitize(User),
    SPasswd = sanitize(Passwd),
    SRealm = sanitize(Realm),
    A1 = case AuthzId of
	     "" ->
		 binary_to_list(
		   crypto:md5(SUser ++ ":" ++ SRealm ++ ":" ++ SPasswd)) ++
		     ":" ++ Nonce ++ ":" ++ CNonce;
	     _ ->
		 binary_to_list(
		   crypto:md5(SUser ++ ":" ++ SRealm ++ ":" ++ SPasswd)) ++
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


sanitize(V) ->
    L = from_utf8(V),
    case lists:all(fun is_latin1/1, L) of
       true -> L;
       false -> V
    end.

%%%% copied from xmerl_ucs:from_utf8/1 and xmerl_ucs:is_latin1/1 , to not
%%%% require xmerl as a dependency only for this.

from_utf8(Bin) when is_binary(Bin) -> from_utf8(binary_to_list(Bin));
from_utf8(List) ->
    case expand_utf8(List) of
    {Result,0} -> Result;
    {_Res,_NumBadChar} ->
        exit({ucs,{bad_utf8_character_code}})
    end.



%% expand_utf8([Byte]) -> {[UnicodeChar],NumberOfBadBytes}
%%  Expand UTF8 byte sequences to ISO 10646/Unicode
%%  charactes. Any illegal bytes are removed and the number of
%%  bad bytes are returned.
%%
%%  Reference:
%%     RFC 3629: "UTF-8, a transformation format of ISO 10646".

expand_utf8(Str) ->
    expand_utf8_1(Str, [], 0).

expand_utf8_1([C|Cs], Acc, Bad) when C < 16#80 ->
    %% Plain Ascii character.
    expand_utf8_1(Cs, [C|Acc], Bad);
expand_utf8_1([C1,C2|Cs], Acc, Bad) when C1 band 16#E0 =:= 16#C0,
                     C2 band 16#C0 =:= 16#80 ->
    case ((C1 band 16#1F) bsl 6) bor (C2 band 16#3F) of
    C when 16#80 =< C ->
        expand_utf8_1(Cs, [C|Acc], Bad);
    _ ->
        %% Bad range.
        expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1,C2,C3|Cs], Acc, Bad) when C1 band 16#F0 =:= 16#E0,
                        C2 band 16#C0 =:= 16#80,
                        C3 band 16#C0 =:= 16#80 ->
    case ((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
    (C3 band 16#3F) of
    C when 16#800 =< C ->
        expand_utf8_1(Cs, [C|Acc], Bad);
    _ ->
        %% Bad range.
        expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1,C2,C3,C4|Cs], Acc, Bad) when C1 band 16#F8 =:= 16#F0,
                           C2 band 16#C0 =:= 16#80,
                           C3 band 16#C0 =:= 16#80,
                           C4 band 16#C0 =:= 16#80 ->
    case ((((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
    (C3 band 16#3F)) bsl 6) bor (C4 band 16#3F) of
    C when 16#10000 =< C ->
        expand_utf8_1(Cs, [C|Acc], Bad);
    _ ->
        %% Bad range.
        expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([_|Cs], Acc, Bad) ->
    %% Ignore bad character.
    expand_utf8_1(Cs, Acc, Bad+1);
expand_utf8_1([], Acc, Bad) -> {lists:reverse(Acc),Bad}.


%%% Test for legitimate Latin-1 code
is_latin1(Ch) when is_integer(Ch), Ch >= 0, Ch =< 255 -> true;
is_latin1(_) -> false.

