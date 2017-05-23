%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  4 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
-module(ejabberd_pkix).

-behaviour(gen_server).
-behaviour(ejabberd_config).

%% API
-export([start_link/0, add_certfile/1, format_error/1, opt_type/1,
	 get_certfile/1, try_certfile/1, route_registered/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("public_key/include/public_key.hrl").
-include("logger.hrl").
-include("jid.hrl").

-record(state, {validate = true :: boolean(),
		certs = #{}}).
-record(cert_state, {domains = [] :: [binary()]}).

-type cert() :: #'OTPCertificate'{}.
-type priv_key() :: public_key:private_key().
-type pub_key() :: #'RSAPublicKey'{} | {integer(), #'Dss-Parms'{}} | #'ECPoint'{}.
-type bad_cert_reason() :: cert_expired | invalid_issuer | invalid_signature |
			   name_not_permitted | missing_basic_constraint |
			   invalid_key_usage | selfsigned_peer | unknown_sig_algo |
			   unknown_ca | missing_priv_key.
-type bad_cert() :: {bad_cert, bad_cert_reason()}.
-type cert_error() :: not_cert | not_der | not_pem | encrypted.
-export_type([cert_error/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec add_certfile(filename:filename())
      -> ok | {error, cert_error() | file:posix()}.
add_certfile(Path) ->
    gen_server:call(?MODULE, {add_certfile, prep_path(Path)}).

-spec try_certfile(filename:filename()) -> binary().
try_certfile(Path0) ->
    Path = prep_path(Path0),
    case mk_cert_state(Path, false) of
	{ok, _} -> Path;
	{error, _} -> erlang:error(badarg)
    end.

route_registered(Route) ->
    gen_server:call(?MODULE, {route_registered, Route}).

-spec format_error(cert_error() | file:posix()) -> string().
format_error(not_cert) ->
    "no PEM encoded certificates found";
format_error(not_pem) ->
    "failed to decode from PEM format";
format_error(not_der) ->
    "failed to decode from DER format";
format_error(encrypted) ->
    "encrypted certificate found in the chain";
format_error({bad_cert, cert_expired}) ->
    "certificate is no longer valid as its expiration date has passed";
format_error({bad_cert, invalid_issuer}) ->
    "certificate issuer name does not match the name of the "
	"issuer certificate in the chain";
format_error({bad_cert, invalid_signature}) ->
    "certificate was not signed by its issuer certificate in the chain";
format_error({bad_cert, name_not_permitted}) ->
    "invalid Subject Alternative Name extension";
format_error({bad_cert, missing_basic_constraint}) ->
    "certificate, required to have the basic constraints extension, "
	"does not have a basic constraints extension";
format_error({bad_cert, invalid_key_usage}) ->
    "certificate key is used in an invalid way according "
	"to the key-usage extension";
format_error({bad_cert, selfsigned_peer}) ->
    "self-signed certificate in the chain";
format_error({bad_cert, unknown_sig_algo}) ->
    "certificate is signed using unknown algorithm";
format_error({bad_cert, unknown_ca}) ->
    "certificate is signed by unknown CA";
format_error({bad_cert, missing_priv_key}) ->
    "no matching private key found for certificate in the chain";
format_error({bad_cert, Unknown}) ->
    lists:flatten(io_lib:format("~w", [Unknown]));
format_error(Why) ->
    case file:format_error(Why) of
	"unknown POSIX error" ->
	    atom_to_list(Why);
	Reason ->
	    Reason
    end.

-spec get_certfile(binary()) -> {ok, binary()} | error.
get_certfile(Domain) ->
    case ejabberd_idna:domain_utf8_to_ascii(Domain) of
	false ->
	    error;
	ASCIIDomain ->
	    case ets:lookup(?MODULE, ASCIIDomain) of
		[] ->
		    case binary:split(ASCIIDomain, <<".">>, [trim]) of
			[_, Host] ->
			    case ets:lookup(?MODULE, <<"*.", Host/binary>>) of
				[{_, Path}|_] ->
				    {ok, Path};
				[] ->
				    error
			    end;
			_ ->
			    error
		    end;
		[{_, Path}|_] ->
		    {ok, Path}
	    end
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

opt_type(ca_path) ->
    fun(Path) -> iolist_to_binary(Path) end;
opt_type(_) ->
    [ca_path].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, [named_table, public, bag]),
    ejabberd_hooks:add(route_registered, ?MODULE, route_registered, 50),
    Validate = case os:type() of
		   {win32, _} -> false;
		   _ ->
		       code:ensure_loaded(public_key),
		       erlang:function_exported(
			 public_key, short_name_hash, 1)
	       end,
    if Validate -> check_ca_dir();
       true -> ok
    end,
    State = #state{validate = Validate},
    {ok, add_certfiles(State)}.

handle_call({add_certfile, Path}, _, State) ->
    {Result, NewState} = add_certfile(Path, State),
    {reply, Result, NewState};
handle_call({route_registered, Host}, _, State) ->
    NewState = add_certfiles(Host, State),
    case get_certfile(Host) of
	{ok, _} -> ok;
	error ->
	    ?WARNING_MSG("No certificate found matching '~s': strictly "
			 "configured clients or servers will reject "
			 "connections with this host", [Host])
    end,
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    ?WARNING_MSG("unexpected info: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_hooks:delete(route_registered, ?MODULE, route_registered, 50).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_certfiles(State) ->
    lists:foldl(
      fun(Host, AccState) ->
	      add_certfiles(Host, AccState)
      end, State, ejabberd_config:get_myhosts()).

add_certfiles(Host, State) ->
    lists:foldl(
      fun(Opt, AccState) ->
	      case ejabberd_config:get_option({Opt, Host}) of
		  undefined -> AccState;
		  Path ->
		      {_, NewAccState} = add_certfile(Path, AccState),
		      NewAccState
	      end
      end, State, [c2s_certfile, s2s_certfile, domain_certfile]).

add_certfile(Path, State) ->
    case maps:get(Path, State#state.certs, undefined) of
	#cert_state{} ->
	    {ok, State};
	undefined ->
	    case mk_cert_state(Path, State#state.validate) of
		{error, Reason} ->
		    {{error, Reason}, State};
		{ok, CertState} ->
		    NewCerts = maps:put(Path, CertState, State#state.certs),
		    lists:foreach(
		      fun(Domain) ->
			      ets:insert(?MODULE, {Domain, Path})
		      end, CertState#cert_state.domains),
		    {ok, State#state{certs = NewCerts}}
	    end
    end.

mk_cert_state(Path, Validate) ->
    case check_certfile(Path, Validate) of
	{ok, Ds} ->
	    {ok, #cert_state{domains = Ds}};
	{invalid, Ds, {bad_cert, _} = Why} ->
	    ?WARNING_MSG("certificate from ~s is invalid: ~s",
			 [Path, format_error(Why)]),
	    {ok, #cert_state{domains = Ds}};
	{error, Why} = Err ->
	    ?ERROR_MSG("failed to read certificate from ~s: ~s",
		       [Path, format_error(Why)]),
	    Err
    end.

-spec check_certfile(filename:filename(), boolean())
      -> {ok, [binary()]} | {invalid, [binary()], bad_cert()} |
	 {error, cert_error() | file:posix()}.
check_certfile(Path, Validate) ->
    try
	{ok, Data} = file:read_file(Path),
	{ok, Certs, PrivKeys} = pem_decode(Data),
	CertPaths = get_cert_paths(Certs),
	Domains = get_domains(CertPaths),
	case match_cert_keys(CertPaths, PrivKeys) of
	    {ok, _} ->
		case validate(CertPaths, Validate) of
		    ok -> {ok, Domains};
		    {error, Why} -> {invalid, Domains, Why}
		end;
	    {error, Why} ->
		{invalid, Domains, Why}
	end
    catch _:{badmatch, {error, _} = Err} ->
	    Err
    end.

-spec pem_decode(binary()) -> {ok, [cert()], [priv_key()]} |
			      {error, cert_error()}.
pem_decode(Data) ->
    try public_key:pem_decode(Data) of
	PemEntries ->
	    case decode_certs(PemEntries) of
		{error, _} = Err ->
		    Err;
		Objects ->
		    case lists:partition(
			   fun(#'OTPCertificate'{}) -> true;
			      (_) -> false
			   end, Objects) of
			{[], _} ->
			    {error, not_cert};
			{Certs, PrivKeys} ->
			    {ok, Certs, PrivKeys}
		    end
	    end
    catch _:_ ->
	    {error, not_pem}
    end.

-spec decode_certs([public_key:pem_entry()]) -> {[cert()], [priv_key()]} |
						{error, not_der | encrypted}.
decode_certs(PemEntries) ->
    try lists:foldr(
	  fun(_, {error, _} = Err) ->
		  Err;
	     ({_, _, Flag}, _) when Flag /= not_encrypted ->
		  {error, encrypted};
	     ({'Certificate', Der, _}, Acc) ->
		  [public_key:pkix_decode_cert(Der, otp)|Acc];
	     ({'PrivateKeyInfo', Der, not_encrypted}, Acc) ->
		  #'PrivateKeyInfo'{privateKeyAlgorithm =
					#'PrivateKeyInfo_privateKeyAlgorithm'{
					   algorithm = Algo},
				    privateKey = Key} =
		      public_key:der_decode('PrivateKeyInfo', Der),
		  case Algo of
		      ?'rsaEncryption' ->
			  [public_key:der_decode(
			     'RSAPrivateKey', iolist_to_binary(Key))|Acc];
		      ?'id-dsa' ->
			  [public_key:der_decode(
			     'DSAPrivateKey', iolist_to_binary(Key))|Acc];
		      ?'id-ecPublicKey' ->
			  [public_key:der_decode(
			     'ECPrivateKey', iolist_to_binary(Key))|Acc];
		      _ ->
			  Acc
		  end;
	     ({Tag, Der, _}, Acc) when Tag == 'RSAPrivateKey';
				       Tag == 'DSAPrivateKey';
				       Tag == 'ECPrivateKey' ->
		  [public_key:der_decode(Tag, Der)|Acc];
	     (_, Acc) ->
		  Acc
	  end, [], PemEntries)
    catch _:_ ->
	    {error, not_der}
    end.

-spec validate([{path, [cert()]}], boolean()) -> ok | {error, bad_cert()}.
validate([{path, Path}|Paths], true) ->
    case validate_path(Path) of
	ok ->
	    validate(Paths, true);
	Err ->
	    Err
    end;
validate(_, _) ->
    ok.

-spec validate_path([cert()]) -> ok | {error, bad_cert()}.
validate_path([Cert|_] = Certs) ->
    case find_local_issuer(Cert) of
	{ok, IssuerCert} ->
	    try public_key:pkix_path_validation(IssuerCert, Certs, []) of
		{ok, _} ->
		    ok;
		Err ->
		    Err
	    catch error:function_clause ->
		    case erlang:get_stacktrace() of
			[{public_key, pkix_sign_types, _, _}|_] ->
			    {error, {bad_cert, unknown_sig_algo}};
			ST ->
			    %% Bug in public_key application
			    erlang:raise(error, function_clause, ST)
		    end
	    end;
	{error, _} = Err ->
	    case public_key:pkix_is_self_signed(Cert) of
		true ->
		    {error, {bad_cert, selfsigned_peer}};
		false ->
		    Err
	    end
    end.

-spec ca_dir() -> string().
ca_dir() ->
    ejabberd_config:get_option(ca_path, "/etc/ssl/certs").

-spec check_ca_dir() -> ok.
check_ca_dir() ->
    case filelib:wildcard(filename:join(ca_dir(), "*.0")) of
	[] ->
	    Hint = "configuring 'ca_path' option might help",
	    case file:list_dir(ca_dir()) of
		{error, Why} ->
		    ?WARNING_MSG("failed to read CA directory ~s: ~s; ~s",
				 [ca_dir(), file:format_error(Why), Hint]);
		{ok, _} ->
		    ?WARNING_MSG("CA directory ~s doesn't contain "
				 "hashed certificate files; ~s",
				 [ca_dir(), Hint])
	    end;
	_ ->
	    ok
    end.

-spec find_local_issuer(cert()) -> {ok, cert()} | {error, {bad_cert, unknown_ca}}.
find_local_issuer(Cert) ->
    {ok, {_, IssuerID}} = public_key:pkix_issuer_id(Cert, self),
    Hash = short_name_hash(IssuerID),
    filelib:fold_files(
      ca_dir(), Hash ++ "\\.[0-9]+", false,
      fun(_, {ok, IssuerCert}) ->
	      {ok, IssuerCert};
	 (CertFile, Acc) ->
	      try
		  {ok, Data} = file:read_file(CertFile),
		  {ok, [IssuerCert|_], _} = pem_decode(Data),
		  case public_key:pkix_is_issuer(Cert, IssuerCert) of
		      true ->
			  {ok, IssuerCert};
		      false ->
			  Acc
		  end
	      catch _:{badmatch, {error, Why}} ->
		      ?ERROR_MSG("failed to read CA certificate from \"~s\": ~s",
				 [CertFile, format_error(Why)]),
		      Acc
	      end
      end, {error, {bad_cert, unknown_ca}}).

-spec match_cert_keys([{path, [cert()]}], [priv_key()])
      -> {ok, [{cert(), priv_key()}]} | {error, {bad_cert, missing_priv_key}}.
match_cert_keys(CertPaths, PrivKeys) ->
    KeyPairs = [{pubkey_from_privkey(PrivKey), PrivKey} || PrivKey <- PrivKeys],
    match_cert_keys(CertPaths, KeyPairs, []).

-spec match_cert_keys([{path, [cert()]}], [{pub_key(), priv_key()}],
		      [{cert(), priv_key()}])
      -> {ok, [{cert(), priv_key()}]} | {error, {bad_cert, missing_priv_key}}.
match_cert_keys([{path, Certs}|CertPaths], KeyPairs, Result) ->
    [Cert|_] = RevCerts = lists:reverse(Certs),
    PubKey = pubkey_from_cert(Cert),
    case lists:keyfind(PubKey, 1, KeyPairs) of
	false ->
	    {error, {bad_cert, missing_priv_key}};
	{_, PrivKey} ->
	    match_cert_keys(CertPaths, KeyPairs, [{RevCerts, PrivKey}|Result])
    end;
match_cert_keys([], _, Result) ->
    {ok, Result}.

-spec pubkey_from_cert(cert()) -> pub_key().
pubkey_from_cert(Cert) ->
    TBSCert = Cert#'OTPCertificate'.tbsCertificate,
    PubKeyInfo = TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
    SubjPubKey = PubKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    case PubKeyInfo#'OTPSubjectPublicKeyInfo'.algorithm of
	#'PublicKeyAlgorithm'{
	   algorithm = ?rsaEncryption} ->
	    SubjPubKey;
	#'PublicKeyAlgorithm'{
	   algorithm = ?'id-dsa',
	   parameters = {params, DSSParams}} ->
	    {SubjPubKey, DSSParams};
	#'PublicKeyAlgorithm'{
	   algorithm = ?'id-ecPublicKey'} ->
	    SubjPubKey
    end.

-spec pubkey_from_privkey(priv_key()) -> pub_key().
pubkey_from_privkey(#'RSAPrivateKey'{modulus = Modulus,
				     publicExponent = Exp}) ->
    #'RSAPublicKey'{modulus = Modulus,
		    publicExponent = Exp};
pubkey_from_privkey(#'DSAPrivateKey'{p = P, q = Q, g = G, y = Y}) ->
    {Y, #'Dss-Parms'{p = P, q = Q, g = G}};
pubkey_from_privkey(#'ECPrivateKey'{publicKey = Key}) ->
    #'ECPoint'{point = Key}.

-spec get_domains([{path, [cert()]}]) -> [binary()].
get_domains(CertPaths) ->
    lists:usort(
      lists:flatmap(
	fun({path, Certs}) ->
		Cert = lists:last(Certs),
		xmpp_stream_pkix:get_cert_domains(Cert)
	end, CertPaths)).

-spec get_cert_paths([cert()]) -> [{path, [cert()]}].
get_cert_paths(Certs) ->
    G = digraph:new([acyclic]),
    lists:foreach(
      fun(Cert) ->
    	      digraph:add_vertex(G, Cert)
      end, Certs),
    lists:foreach(
      fun({Cert1, Cert2}) when Cert1 /= Cert2 ->
	      case public_key:pkix_is_issuer(Cert1, Cert2) of
		  true ->
		      digraph:add_edge(G, Cert1, Cert2);
		  false ->
		      ok
	      end;
	 (_) ->
	      ok
      end, [{Cert1, Cert2} || Cert1 <- Certs, Cert2 <- Certs]),
    Paths = lists:flatmap(
	      fun(Cert) ->
		      case digraph:in_degree(G, Cert) of
			  0 ->
			      get_cert_path(G, [Cert]);
			  _ ->
			      []
		      end
	      end, Certs),
    digraph:delete(G),
    Paths.

get_cert_path(G, [Root|_] = Acc) ->
    case digraph:out_edges(G, Root) of
	[] ->
	    [{path, Acc}];
	Es ->
	    lists:flatmap(
	      fun(E) ->
		      {_, _, V, _} = digraph:edge(G, E),
		      get_cert_path(G, [V|Acc])
	      end, Es)
    end.

-spec prep_path(filename:filename()) -> binary().
prep_path(Path0) ->
    case filename:pathtype(Path0) of
	relative ->
	    {ok, CWD} = file:get_cwd(),
	    iolist_to_binary(filename:join(CWD, Path0));
	_ ->
	    iolist_to_binary(Path0)
    end.

-ifdef(SHORT_NAME_HASH).
short_name_hash(IssuerID) ->
    public_key:short_name_hash(IssuerID).
-else.
short_name_hash(_) ->
    "".
-endif.
