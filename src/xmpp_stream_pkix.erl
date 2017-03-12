%%%-------------------------------------------------------------------
%%% Created : 13 Dec 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%-------------------------------------------------------------------
-module(xmpp_stream_pkix).

%% API
-export([authenticate/1, authenticate/2, get_cert_domains/1, format_error/1]).

-include("xmpp.hrl").
-include_lib("public_key/include/public_key.hrl").
-include("XmppAddr.hrl").

-type cert() :: #'OTPCertificate'{}.

%%%===================================================================
%%% API
%%%===================================================================
-spec authenticate(xmpp_stream_in:state() | xmpp_stream_out:state())
      -> {ok, binary()} | {error, atom(), binary()}.
authenticate(State) ->
    authenticate(State, <<"">>).

-spec authenticate(xmpp_stream_in:state() | xmpp_stream_out:state(), binary())
      -> {ok, binary()} | {error, atom(), binary()}.
authenticate(#{xmlns := ?NS_SERVER, sockmod := SockMod,
	       socket := Socket} = State, Authzid) ->
    Peer = maps:get(remote_server, State, Authzid),
    case verify_cert(SockMod, Socket) of
	{ok, Cert} ->
	    case ejabberd_idna:domain_utf8_to_ascii(Peer) of
		false ->
		    {error, idna_failed, Peer};
		AsciiPeer ->
		    case lists:any(
			   fun(D) -> match_domain(AsciiPeer, D) end,
			   get_cert_domains(Cert)) of
			true ->
			    {ok, Peer};
			false ->
			    {error, hostname_mismatch, Peer}
		    end
	    end;
	{error, Reason} ->
	    {error, Reason, Peer}
    end;
authenticate(#{xmlns := ?NS_CLIENT, sockmod := SockMod,
	       socket := Socket, lserver := LServer}, Authzid) ->
    JID = try jid:decode(Authzid)
	  catch _:{bad_jid, <<>>} -> jid:make(LServer);
		_:{bad_jid, _} -> {error, invalid_authzid, Authzid}
	  end,
    case JID of
	#jid{user = User} ->
	    case verify_cert(SockMod, Socket) of
		{ok, Cert} ->
		    JIDs = get_xmpp_addrs(Cert),
		    get_username(JID, JIDs, LServer);
		{error, Reason} ->
		    {error, Reason, User}
	    end;
	Err ->
	    Err
    end.

format_error(idna_failed) ->
    {'bad-protocol', <<"Remote domain is not an IDN hostname">>};
format_error(hostname_mismatch) ->
    {'not-authorized', <<"Certificate host name mismatch">>};
format_error(jid_mismatch) ->
    {'not-authorized', <<"Certificate JID mismatch">>};
format_error(get_cert_failed) ->
    {'bad-protocol', <<"Failed to get peer certificate">>};
format_error(invalid_authzid) ->
    {'invalid-authzid', <<"Malformed JID">>};
format_error(Other) ->
    {'not-authorized', erlang:atom_to_binary(Other, utf8)}.

-spec get_cert_domains(cert()) -> [binary()].
get_cert_domains(Cert) ->
    TBSCert = Cert#'OTPCertificate'.tbsCertificate,
    {rdnSequence, Subject} = TBSCert#'OTPTBSCertificate'.subject,
    Extensions = TBSCert#'OTPTBSCertificate'.extensions,
    get_domain_from_subject(lists:flatten(Subject)) ++
	get_domains_from_san(Extensions).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec verify_cert(module(), ejabberd_socket:socket()) -> {ok, cert()} | {error, atom()}.
verify_cert(SockMod, Socket) ->
    case SockMod:get_peer_certificate(Socket, otp) of
	{ok, Cert} ->
	    case SockMod:get_verify_result(Socket) of
		0 ->
		    {ok, Cert};
		VerifyRes ->
		    %% TODO: return atomic errors
		    %% This should be improved in fast_tls
		    Reason = fast_tls:get_cert_verify_string(VerifyRes, Cert),
		    {error, erlang:binary_to_atom(Reason, utf8)}
	    end;
	{error, _Reason} ->
	    {error, get_cert_failed};
	error ->
	    {error, get_cert_failed}
    end.

-spec get_domain_from_subject([#'AttributeTypeAndValue'{}]) -> [binary()].
get_domain_from_subject(AttrVals) ->
    case lists:keyfind(?'id-at-commonName',
		       #'AttributeTypeAndValue'.type,
		       AttrVals) of
	#'AttributeTypeAndValue'{value = {_, S}} ->
	    try jid:decode(iolist_to_binary(S)) of
		#jid{luser = <<"">>, lresource = <<"">>, lserver = Domain} ->
		    [Domain];
		_ ->
		    []
	    catch _:{bad_jid, _} ->
		    []
	    end;
	_ ->
	    []
    end.

-spec get_domains_from_san([#'Extension'{}] | asn1_NOVALUE) -> [binary()].
get_domains_from_san(Extensions) when is_list(Extensions) ->
    case lists:keyfind(?'id-ce-subjectAltName',
		       #'Extension'.extnID,
		       Extensions) of
	#'Extension'{extnValue = Vals} ->
	    lists:flatmap(
	      fun({dNSName, S}) ->
		      [iolist_to_binary(S)];
		 ({otherName, AnotherName}) ->
		      case decode_xmpp_addr(AnotherName) of
			  {ok, #jid{luser = <<"">>,
				    lresource = <<"">>,
				    lserver = Domain}} ->
			      case ejabberd_idna:domain_utf8_to_ascii(Domain) of
				  false ->
				      [];
				  ASCIIDomain ->
				      [ASCIIDomain]
			      end;
			  _ ->
			      []
		      end;
		 (_) ->
		      []
	      end, Vals);
	_ ->
	    []
    end;
get_domains_from_san(_) ->
    [].

-spec decode_xmpp_addr(#'AnotherName'{}) -> {ok, jid()} | error.
decode_xmpp_addr(#'AnotherName'{'type-id' = ?'id-on-xmppAddr',
				value = XmppAddr}) ->
    try 'XmppAddr':decode('XmppAddr', XmppAddr) of
	{ok, JIDStr} ->
	    try {ok, jid:decode(iolist_to_binary(JIDStr))}
	    catch _:{bad_jid, _} -> error
	    end;
	_ ->
	    error
    catch _:_ ->
	    error
    end;
decode_xmpp_addr(_) ->
    error.

-spec get_xmpp_addrs(cert()) -> [jid()].
get_xmpp_addrs(Cert) ->
    TBSCert = Cert#'OTPCertificate'.tbsCertificate,
    case TBSCert#'OTPTBSCertificate'.extensions of
	Extensions when is_list(Extensions) ->
	    case lists:keyfind(?'id-ce-subjectAltName',
			       #'Extension'.extnID,
			       Extensions) of
		#'Extension'{extnValue = Vals} ->
		    lists:flatmap(
		      fun({otherName, AnotherName}) ->
			      case decode_xmpp_addr(AnotherName) of
				  {ok, JID} -> [JID];
				  _ -> []
			      end;
			 (_) ->
			      []
		      end, Vals);
		_ ->
		    []
	    end;
	_ ->
	    []
    end.

match_domain(Domain, Domain) -> true;
match_domain(Domain, Pattern) ->
    DLabels = str:tokens(Domain, <<".">>),
    PLabels = str:tokens(Pattern, <<".">>),
    match_labels(DLabels, PLabels).

match_labels([], []) -> true;
match_labels([], [_ | _]) -> false;
match_labels([_ | _], []) -> false;
match_labels([DL | DLabels], [PL | PLabels]) ->
    case lists:all(fun (C) ->
			   $a =< C andalso C =< $z orelse
			     $0 =< C andalso C =< $9 orelse
			       C == $- orelse C == $*
		   end,
		   binary_to_list(PL))
	of
      true ->
	  Regexp = ejabberd_regexp:sh_to_awk(PL),
	  case ejabberd_regexp:run(DL, Regexp) of
	    match -> match_labels(DLabels, PLabels);
	    nomatch -> false
	  end;
      false -> false
    end.

-spec get_username(jid(), [jid()], binary()) ->
			  {ok, binary()} | {error, jid_mismatch, binary()}.
get_username(#jid{user = User, lserver = LS}, _, LServer) when LS /= LServer ->
    %% The user provided JID from different domain
    {error, jid_mismatch, User};
get_username(#jid{user = <<>>}, [#jid{user = U, lserver = LS}], LServer)
  when U /= <<>> andalso LS == LServer ->
    %% The user didn't provide JID or username, and there is only
    %% one 'non-global' JID matching current domain
    {ok, U};
get_username(#jid{user = User, luser = LUser}, JIDs, LServer) when User /= <<>> ->
    %% The user provided username
    lists:foldl(
      fun(_, {ok, _} = OK) ->
	      OK;
	 (#jid{user = <<>>, lserver = LS}, _) when LS == LServer ->
	      %% Found "global" JID in the certficate
	      %% (i.e. in the form of 'domain.com')
	      %% within current domain, so we force matching
	      {ok, User};
	 (#jid{luser = LU, lserver = LS}, _) when LU == LUser, LS == LServer ->
	      %% Found exact JID matching
	      {ok, User};
	 (_, Err) ->
	      Err
      end, {error, jid_mismatch, User}, JIDs);
get_username(#jid{user = User}, _, _) ->
    %% Nothing from above is true
    {error, jid_mismatch, User}.
