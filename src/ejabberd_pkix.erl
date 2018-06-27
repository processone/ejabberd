%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  4 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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
	 get_certfile/1, try_certfile/1, route_registered/1,
	 config_reloaded/0, certs_dir/0, ca_file/0, get_default_certfile/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("public_key/include/public_key.hrl").
-include("logger.hrl").

-record(state, {validate = true :: boolean(),
		paths = [] :: [file:filename()],
		certs = #{} :: map(),
		graph :: digraph:graph(),
		keys = [] :: [public_key:private_key()]}).

-type state() :: #state{}.
-type cert() :: #'OTPCertificate'{}.
-type priv_key() :: public_key:private_key().
-type pub_key() :: #'RSAPublicKey'{} | {integer(), #'Dss-Parms'{}} | #'ECPoint'{}.
-type bad_cert_reason() :: cert_expired | invalid_issuer | invalid_signature |
			   name_not_permitted | missing_basic_constraint |
			   invalid_key_usage | selfsigned_peer | unknown_sig_algo |
			   unknown_ca | missing_priv_key | unknown_key_algo |
			   unknown_key_type | encrypted | not_der | not_cert |
			   not_pem.
-type cert_error() :: {bad_cert, bad_cert_reason()}.
-export_type([cert_error/0]).

-define(CA_CACHE, ca_cache).

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
    case load_certfile(Path) of
	{ok, _, _} -> Path;
	{error, _} -> erlang:error(badarg)
    end.

route_registered(Route) ->
    gen_server:call(?MODULE, {route_registered, Route}).

-spec format_error(cert_error() | file:posix()) -> string().
format_error({bad_cert, not_cert}) ->
    "no PEM encoded certificates found";
format_error({bad_cert, not_pem}) ->
    "failed to decode from PEM format";
format_error({bad_cert, not_der}) ->
    "failed to decode from DER format";
format_error({bad_cert, encrypted}) ->
    "encrypted certificate";
format_error({bad_cert, cert_expired}) ->
    "certificate is no longer valid as its expiration date has passed";
format_error({bad_cert, invalid_issuer}) ->
    "certificate issuer name does not match the name of the "
	"issuer certificate";
format_error({bad_cert, invalid_signature}) ->
    "certificate was not signed by its issuer certificate";
format_error({bad_cert, name_not_permitted}) ->
    "invalid Subject Alternative Name extension";
format_error({bad_cert, missing_basic_constraint}) ->
    "certificate, required to have the basic constraints extension, "
	"does not have a basic constraints extension";
format_error({bad_cert, invalid_key_usage}) ->
    "certificate key is used in an invalid way according "
	"to the key-usage extension";
format_error({bad_cert, selfsigned_peer}) ->
    "self-signed certificate";
format_error({bad_cert, unknown_sig_algo}) ->
    "certificate is signed using unknown algorithm";
format_error({bad_cert, unknown_key_algo}) ->
    "unknown private key algorithm";
format_error({bad_cert, unknown_key_type}) ->
    "private key is of unknown type";
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
    case get_certfile_no_default(Domain) of
	{ok, Path} ->
	    {ok, Path};
	error ->
	    get_default_certfile()
    end.

-spec get_certfile_no_default(binary()) -> {ok, binary()} | error.
get_certfile_no_default(Domain) ->
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

-spec get_default_certfile() -> {ok, binary()} | error.
get_default_certfile() ->
    case ets:first(?MODULE) of
	'$end_of_table' ->
	    error;
	Domain ->
	    case ets:lookup(?MODULE, Domain) of
		[{_, Path}|_] ->
		    {ok, Path};
		[] ->
		    error
	    end
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

config_reloaded() ->
    case use_cache() of
	true -> init_cache();
	false -> delete_cache()
    end,
    fast_tls:clear_cache(),
    gen_server:call(?MODULE, config_reloaded, 60000).

opt_type(ca_path) ->
    fun(Path) -> binary_to_list(Path) end;
opt_type(ca_file) ->
    fun(Path) ->
	    binary_to_list(misc:try_read_file(Path))
    end;
opt_type(certfiles) ->
    fun(CertList) ->
	    [binary_to_list(Path) || Path <- CertList]
    end;
opt_type(O) when O == c2s_certfile; O == s2s_certfile; O == domain_certfile ->
    fun(File) ->
	    ?WARNING_MSG("option '~s' is deprecated, use 'certfiles' instead", [O]),
	    misc:try_read_file(File)
    end;
opt_type(_) ->
    [ca_path, ca_file, certfiles, c2s_certfile, s2s_certfile, domain_certfile].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, [named_table, public]),
    ejabberd_hooks:add(route_registered, ?MODULE, route_registered, 50),
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 30),
    Validate = case os:type() of
		   {win32, _} -> false;
		   _ ->
		       code:ensure_loaded(public_key),
		       erlang:function_exported(
			 public_key, short_name_hash, 1)
	       end,
    if Validate -> check_ca();
       true -> ok
    end,
    G = digraph:new([acyclic]),
    init_cache(),
    State = #state{validate = Validate, graph = G},
    case filelib:ensure_dir(filename:join(certs_dir(), "foo")) of
	ok ->
	    clean_dir(certs_dir()),
	    case add_certfiles(State) of
		{ok, State1} ->
		    {ok, State1};
		{error, Why} ->
		    {stop, Why}
	    end;
	{error, Why} ->
	    ?CRITICAL_MSG("Failed to create directory ~s: ~s",
			  [certs_dir(), file:format_error(Why)]),
	    {stop, Why}
    end.

handle_call({add_certfile, Path}, _, State) ->
    case add_certfile(Path, State) of
	{ok, State1} ->
	    if State /= State1 ->
		    case build_chain_and_check(State1) of
			{ok, State2} ->
			    {reply, ok, State2};
			Err ->
			    {reply, Err, State1}
		    end;
	       true ->
		    {reply, ok, State1}
	    end;
	{Err, State1} ->
	    {reply, Err, State1}
    end;
handle_call({route_registered, Host}, _, State) ->
    case add_certfiles(Host, State) of
	{ok, NewState} ->
	    case get_certfile_no_default(Host) of
		{ok, _} -> ok;
		error ->
		    ?WARNING_MSG("No certificate found matching '~s': strictly "
				 "configured clients or servers will reject "
				 "connections with this host; obtain "
				 "a certificate for this (sub)domain from any "
				 "trusted CA such as Let's Encrypt "
				 "(www.letsencrypt.org)",
				 [Host])
	    end,
	    {reply, ok, NewState};
	{error, _} ->
	    {reply, ok, State}
    end;
handle_call(config_reloaded, _From, State) ->
    State1 = State#state{paths = [], certs = #{}, keys = []},
    case add_certfiles(State1) of
	{ok, State2} ->
	    {reply, ok, State2};
	{error, _} = Err ->
	    {reply, Err, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    ?WARNING_MSG("unexpected info: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_hooks:delete(route_registered, ?MODULE, route_registered, 50),
    ejabberd_hooks:delete(config_reloaded, ?MODULE, config_reloaded, 30),
    clean_dir(certs_dir()).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec certfiles_from_config_options() -> [atom()].
certfiles_from_config_options() ->
    [c2s_certfile, s2s_certfile, domain_certfile].

-spec get_certfiles_from_config_options(state()) -> [binary()].
get_certfiles_from_config_options(_State) ->
    Global = case ejabberd_config:get_option(certfiles) of
		 undefined ->
		     [];
		 Paths ->
		     lists:flatmap(
		       fun(Path) ->
			       case wildcard(Path) of
				   [] ->
				       ?WARNING_MSG(
					  "Path ~s is empty, please "
					  "make sure ejabberd has "
					  "sufficient rights to read it",
					  [Path]),
				       [];
				   Fs ->
				       Fs
			       end
		       end, Paths)
	     end,
    Local = lists:flatmap(
	      fun(OptHost) ->
		      case ejabberd_config:get_option(OptHost) of
			  undefined -> [];
			  Path -> [Path]
		      end
	      end, [{Opt, Host}
		    || Opt <- certfiles_from_config_options(),
		       Host <- ejabberd_config:get_myhosts()]),
    [iolist_to_binary(P) || P <- lists:usort(Local ++ Global)].

-spec add_certfiles(state()) -> {ok, state()} |
				{error, cert_error() | file:posix()}.
add_certfiles(State) ->
    ?DEBUG("Reading certificates", []),
    Paths = get_certfiles_from_config_options(State),
    State1 = lists:foldl(
	       fun(Path, Acc) ->
		       {_, NewAcc} = add_certfile(Path, Acc),
		       NewAcc
	       end, State, Paths),
    case build_chain_and_check(State1) of
	ok -> {ok, State1};
	{error, _} = Err -> Err
    end.

-spec add_certfiles(binary(), state()) -> {ok, state()} |
					  {error, cert_error() | file:posix()}.
add_certfiles(Host, State) ->
    State1 = lists:foldl(
	       fun(Opt, AccState) ->
		       case ejabberd_config:get_option({Opt, Host}) of
			   undefined -> AccState;
			   Path ->
			       {_, NewAccState} = add_certfile(Path, AccState),
			       NewAccState
		       end
	       end, State, certfiles_from_config_options()),
    if State /= State1 ->
	    case build_chain_and_check(State1) of
		ok -> {ok, State1};
		{error, _} = Err -> Err
	    end;
       true ->
	    {ok, State}
    end.

-spec add_certfile(file:filename_all(), state()) ->
              {ok, state()} | {{error, cert_error() | file:posix()}, state()}.
add_certfile(Path, State) ->
    case lists:member(Path, State#state.paths) of
	true ->
	    {ok, State};
	false ->
	    case load_certfile(Path) of
		{ok, Certs, Keys} ->
		    NewCerts = lists:foldl(
				 fun(Cert, Acc) ->
					 maps:put(Cert, Path, Acc)
				 end, State#state.certs, Certs),
		    {ok, State#state{paths = [Path|State#state.paths],
				     certs = NewCerts,
				     keys = Keys ++ State#state.keys}};
		{error, Why} = Err ->
		    ?ERROR_MSG("failed to read certificate from ~s: ~s",
			       [Path, format_error(Why)]),
		    {Err, State}
	    end
    end.

-spec build_chain_and_check(state()) -> ok | {error, cert_error() | file:posix()}.
build_chain_and_check(State) ->
    CertPaths = get_cert_paths(maps:keys(State#state.certs), State#state.graph),
    case match_cert_keys(CertPaths, State#state.keys) of
	{ok, Chains} ->
	    InvalidCerts = validate(CertPaths, State),
	    SortedChains = sort_chains(Chains, InvalidCerts),
	    store_certs(SortedChains, State);
	{error, Cert, Why} ->
	    Path = maps:get(Cert, State#state.certs),
	    ?ERROR_MSG("Failed to build certificate chain for ~s: ~s",
		       [Path, format_error(Why)]),
	    {error, Why}
    end.

-spec store_certs([{[cert()], priv_key()}], state()) -> ok | {error, file:posix()}.
store_certs(Chains, State) ->
    ?DEBUG("Storing certificate chains", []),
    Res = lists:foldl(
	    fun(_, {error, _} = Err) ->
		    Err;
	       ({Certs, Key}, Acc) ->
		    case store_cert(Certs, Key, State) of
			{ok, FileDoms} ->
			    Acc ++ FileDoms;
			{error, _} = Err ->
			    Err
		    end
	    end, [], Chains),
    case Res of
	{error, Why} ->
	    {error, Why};
	FileDomains ->
	    ets:delete_all_objects(?MODULE),
	    lists:foreach(
	      fun({Path, Domain}) ->
		      fast_tls:add_certfile(Domain, Path),
		      ets:insert(?MODULE, {Domain, Path})
	      end, FileDomains)
    end.

-spec store_cert([cert()], priv_key(), state()) -> {ok, [{binary(), binary()}]} |
						   {error, file:posix()}.
store_cert(Certs, Key, State) ->
    CertPEMs = public_key:pem_encode(
		 lists:map(
		   fun(Cert) ->
			   Type = element(1, Cert),
			   DER = public_key:pkix_encode(Type, Cert, otp),
			   {'Certificate', DER, not_encrypted}
		   end, Certs)),
    KeyPEM = public_key:pem_encode(
	       [{element(1, Key),
		 public_key:der_encode(element(1, Key), Key),
		 not_encrypted}]),
    PEMs = <<CertPEMs/binary, KeyPEM/binary>>,
    Cert = hd(Certs),
    FileName = filename:join(certs_dir(), str:sha(PEMs)),
    case file:write_file(FileName, PEMs) of
	ok ->
	    file:change_mode(FileName, 8#600),
	    case xmpp_stream_pkix:get_cert_domains(Cert) of
		[] ->
		    Path = maps:get(Cert, State#state.certs),
		    ?WARNING_MSG("Certificate from ~s doesn't define "
				 "any domain names", [Path]),
		    {ok, [{FileName, <<"">>}]};
		Domains ->
		    {ok, [{FileName, Domain} || Domain <- Domains]}
	    end;
	{error, Why} = Err ->
	    ?ERROR_MSG("Failed to write to ~s: ~s",
		       [FileName, file:format_error(Why)]),
	    Err
    end.

-spec sort_chains([{[cert()], priv_key()}], [cert()]) -> [{[cert()], priv_key()}].
sort_chains(Chains, InvalidCerts) ->
    lists:sort(
      fun({[Cert1|_], _}, {[Cert2|_], _}) ->
	      IsValid1 = not lists:member(Cert1, InvalidCerts),
	      IsValid2 = not lists:member(Cert2, InvalidCerts),
	      if IsValid1 and not IsValid2 ->
		      false;
		 IsValid2 and not IsValid1 ->
		      true;
		 true ->
		      compare_expiration_date(Cert1, Cert2)
	      end
      end, Chains).

%% Returns true if the first certificate has sooner expiration date
-spec compare_expiration_date(cert(), cert()) -> boolean().
compare_expiration_date(#'OTPCertificate'{
			   tbsCertificate =
			       #'OTPTBSCertificate'{
				  validity = #'Validity'{notAfter = After1}}},
			#'OTPCertificate'{
			   tbsCertificate =
			       #'OTPTBSCertificate'{
				  validity = #'Validity'{notAfter = After2}}}) ->
    get_timestamp(After1) =< get_timestamp(After2).

-spec load_certfile(file:filename_all()) -> {ok, [cert()], [priv_key()]} |
					    {error, cert_error() | file:posix()}.
load_certfile(Path) ->
    try
	{ok, Data} = file:read_file(Path),
	pem_decode(Data)
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
			{[], []} ->
			    {error, {bad_cert, not_cert}};
			{Certs, PrivKeys} ->
			    {ok, Certs, PrivKeys}
		    end
	    end
    catch E:R ->
	    St = erlang:get_stacktrace(),
	    ?DEBUG("PEM decoding stacktrace: ~p", [{E, {R, St}}]),
	    {error, {bad_cert, not_pem}}
    end.

-spec decode_certs([public_key:pem_entry()]) -> [cert() | priv_key()] |
						{error, cert_error()}.
decode_certs(PemEntries) ->
    try lists:flatmap(
	  fun({Tag, Der, Flag}) ->
		  decode_cert(Tag, Der, Flag)
	  end, PemEntries)
    catch _:{bad_cert, _} = Err ->
	    {error, Err};
	  E:R ->
	    St = erlang:get_stacktrace(),
	    ?DEBUG("DER decoding stacktrace: ~p", [{E, {R, St}}]),
	    {error, {bad_cert, not_der}}
    end.

-spec decode_cert(atom(), binary(), atom()) -> [cert() | priv_key()].
decode_cert(_, _, Flag) when Flag /= not_encrypted ->
    erlang:error({bad_cert, encrypted});
decode_cert('Certificate', Der, _) ->
    [public_key:pkix_decode_cert(Der, otp)];
decode_cert('PrivateKeyInfo', Der, not_encrypted) ->
    case public_key:der_decode('PrivateKeyInfo', Der) of
	#'PrivateKeyInfo'{privateKeyAlgorithm =
			      #'PrivateKeyInfo_privateKeyAlgorithm'{
				 algorithm = Algo},
			  privateKey = Key} ->
	    KeyBin = iolist_to_binary(Key),
	    case Algo of
		?'rsaEncryption' ->
		    [public_key:der_decode('RSAPrivateKey', KeyBin)];
		?'id-dsa' ->
		    [public_key:der_decode('DSAPrivateKey', KeyBin)];
		?'id-ecPublicKey' ->
		    [public_key:der_decode('ECPrivateKey', KeyBin)];
		_ ->
		    erlang:error({bad_cert, unknown_key_algo})
	    end;
	#'RSAPrivateKey'{} = Key -> [Key];
	#'DSAPrivateKey'{} = Key -> [Key];
	#'ECPrivateKey'{} = Key -> [Key];
	_ -> erlang:error({bad_cert, unknown_key_type})
    end;
decode_cert(Tag, Der, _) when Tag == 'RSAPrivateKey';
			      Tag == 'DSAPrivateKey';
			      Tag == 'ECPrivateKey' ->
    [public_key:der_decode(Tag, Der)];
decode_cert(_, _, _) ->
    [].

-spec validate([{path, [cert()]}], state()) -> [cert()].
validate(Paths, #state{validate = true} = State) ->
    ?DEBUG("Validating certificates", []),
    {ok, Re} = re:compile("^[a-f0-9]+\\.[0-9]+$", [unicode]),
    Hashes = case file:list_dir(ca_dir()) of
		 {ok, Files} ->
		     lists:foldl(
		       fun(File, Acc) ->
			       try re:run(File, Re) of
				   {match, _} ->
				       [Hash|_] = string:tokens(File, "."),
				       Path = filename:join(ca_dir(), File),
				       dict:append(Hash, Path, Acc);
				   nomatch ->
				       Acc
			       catch _:badarg ->
				       ?ERROR_MSG("Regexp failure on ~w", [File]),
				       Acc
			       end
		       end, dict:new(), Files);
		 {error, Why} ->
		     ?ERROR_MSG("Failed to list directory ~s: ~s",
			       [ca_dir(), file:format_error(Why)]),
		     dict:new()
	     end,
    lists:filtermap(
      fun({path, Path}) ->
	      case validate_path(Path, Hashes) of
		  ok ->
		      false;
		  {error, Cert, Reason} ->
		      File = maps:get(Cert, State#state.certs),
		      ?WARNING_MSG("Failed to validate certificate from ~s: ~s",
				   [File, format_error(Reason)]),
		      {true, Cert}
	      end
      end, Paths);
validate(_, _) ->
    [].

-spec validate_path([cert()], dict:dict()) -> ok | {error, cert(), cert_error()}.
validate_path([Cert|_] = Certs, Cache) ->
    case find_local_issuer(Cert, Cache) of
	{ok, IssuerCert} ->
	    try public_key:pkix_path_validation(IssuerCert, Certs, []) of
		{ok, _} ->
		    ok;
		{error, Reason} ->
		    {error, Cert, Reason}
	    catch error:function_clause ->
		    case erlang:get_stacktrace() of
			[{public_key, pkix_sign_types, _, _}|_] ->
			    {error, Cert, {bad_cert, unknown_sig_algo}};
			ST ->
			    %% Bug in public_key application
			    erlang:raise(error, function_clause, ST)
		    end
	    end;
	{error, Reason} ->
	    case public_key:pkix_is_self_signed(Cert) of
		true ->
		    {error, Cert, {bad_cert, selfsigned_peer}};
		false ->
		    {error, Cert, Reason}
	    end
    end.

-spec ca_dir() -> string().
ca_dir() ->
    ejabberd_config:get_option(ca_path, "/etc/ssl/certs").

-spec ca_file() -> string() | undefined.
ca_file() ->
    ejabberd_config:get_option(ca_file).

-spec certs_dir() -> string().
certs_dir() ->
    MnesiaDir = mnesia:system_info(directory),
    filename:join(MnesiaDir, "certs").

-spec clean_dir(file:filename_all()) -> ok.
clean_dir(Dir) ->
    ?DEBUG("Cleaning directory ~s", [Dir]),
    Files = wildcard(filename:join(Dir, "*")),
    lists:foreach(
      fun(Path) ->
	      case filelib:is_file(Path) of
		  true ->
		      file:delete(Path);
		  false ->
		      ok
	      end
      end, Files).

-spec check_ca() -> ok.
check_ca() ->
    CAFile = ca_file(),
    case wildcard(filename:join(ca_dir(), "*.0")) of
	[] when CAFile == undefined ->
	    Hint = "configuring 'ca_path' or 'ca_file' options might help",
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

-spec find_local_issuer(cert(), dict:dict()) -> {ok, cert()} |
						{error, {bad_cert, unknown_ca}}.
find_local_issuer(Cert, Hashes) ->
    case find_issuer_in_dir(Cert, Hashes) of
	{ok, IssuerCert} ->
	    {ok, IssuerCert};
	{error, Reason} ->
	    case ca_file() of
		undefined -> {error, Reason};
		CAFile -> find_issuer_in_file(Cert, CAFile)
	    end
    end.

-spec find_issuer_in_dir(cert(), dict:dict())
      -> {{ok, cert()} | {error, {bad_cert, unknown_ca}}, dict:dict()}.
find_issuer_in_dir(Cert, Cache) ->
    {ok, {_, IssuerID}} = public_key:pkix_issuer_id(Cert, self),
    Hash = short_name_hash(IssuerID),
    Files = case dict:find(Hash, Cache) of
		{ok, L} -> L;
		error -> []
	    end,
    lists:foldl(
      fun(_, {ok, _IssuerCert} = Acc) ->
	      Acc;
	 (Path, Err) ->
	      case read_ca_file(Path) of
		  {ok, [IssuerCert|_]} ->
		      case public_key:pkix_is_issuer(Cert, IssuerCert) of
			  true ->
			      {ok, IssuerCert};
			  false ->
			      Err
		      end;
		  error ->
		      Err
	      end
      end, {error, {bad_cert, unknown_ca}}, Files).

-spec find_issuer_in_file(cert(), file:filename_all() | undefined)
      -> {ok, cert()} | {error, {bad_cert, unknown_ca}}.
find_issuer_in_file(_Cert, undefined) ->
    {error, {bad_cert, unknown_ca}};
find_issuer_in_file(Cert, CAFile) ->
    case read_ca_file(CAFile) of
	{ok, IssuerCerts} ->
	    lists:foldl(
	      fun(_, {ok, _} = Res) ->
		      Res;
		 (IssuerCert, Err) ->
		      case public_key:pkix_is_issuer(Cert, IssuerCert) of
			  true -> {ok, IssuerCert};
			  false -> Err
		      end
	      end, {error, {bad_cert, unknown_ca}}, IssuerCerts);
	error ->
	    {error, {bad_cert, unknown_ca}}
    end.

-spec read_ca_file(file:filename_all()) -> {ok, [cert()]} | error.
read_ca_file(Path) ->
    case use_cache() of
	true ->
	    ets_cache:lookup(?CA_CACHE, Path,
			     fun() -> do_read_ca_file(Path) end);
	false ->
	    do_read_ca_file(Path)
    end.

-spec do_read_ca_file(file:filename_all()) -> {ok, [cert()]} | error.
do_read_ca_file(Path) ->
    try
	{ok, Data} = file:read_file(Path),
	{ok, IssuerCerts, _} = pem_decode(Data),
	{ok, IssuerCerts}
    catch _:{badmatch, {error, Why}} ->
	    ?ERROR_MSG("Failed to read CA certificate "
		       "from \"~s\": ~s",
		       [Path, format_error(Why)]),
	    error
    end.

-spec match_cert_keys([{path, [cert()]}], [priv_key()])
      -> {ok, [{cert(), priv_key()}]} | {error, {bad_cert, missing_priv_key}}.
match_cert_keys(CertPaths, PrivKeys) ->
    ?DEBUG("Finding matched certificate keys", []),
    KeyPairs = [{pubkey_from_privkey(PrivKey), PrivKey} || PrivKey <- PrivKeys],
    match_cert_keys(CertPaths, KeyPairs, []).

-spec match_cert_keys([{path, [cert()]}], [{pub_key(), priv_key()}],
		      [{cert(), priv_key()}])
      -> {ok, [{[cert()], priv_key()}]} | {error, cert(), {bad_cert, missing_priv_key}}.
match_cert_keys([{path, Certs}|CertPaths], KeyPairs, Result) ->
    [Cert|_] = RevCerts = lists:reverse(Certs),
    PubKey = pubkey_from_cert(Cert),
    case lists:keyfind(PubKey, 1, KeyPairs) of
	false ->
	    {error, Cert, {bad_cert, missing_priv_key}};
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

-spec get_cert_paths([cert()], digraph:graph()) -> [{path, [cert()]}].
get_cert_paths(Certs, G) ->
    ?DEBUG("Building certificates graph", []),
    {NewCerts, OldCerts} =
	lists:partition(
	  fun(Cert) ->
		  case digraph:vertex(G, Cert) of
		      false ->
			  digraph:add_vertex(G, Cert),
			  true;
		      {_, _} ->
			  false
		  end
	  end, Certs),
    add_edges(G, NewCerts, OldCerts),
    add_edges(G, OldCerts, NewCerts),
    add_edges(G, NewCerts, NewCerts),
    lists:flatmap(
      fun(Cert) ->
	      case digraph:in_degree(G, Cert) of
		  0 ->
		      get_cert_path(G, [Cert]);
		  _ ->
		      []
	      end
      end, Certs).

add_edges(G, [Cert1|T], L) ->
    case public_key:pkix_is_self_signed(Cert1) of
	true ->
	    ok;
	false ->
	    lists:foreach(
	      fun(Cert2) when Cert1 /= Cert2 ->
		      case public_key:pkix_is_issuer(Cert1, Cert2) of
			  true ->
			      digraph:add_edge(G, Cert1, Cert2);
			  false ->
			      ok
		      end;
		 (_) ->
		      ok
	      end, L)
    end,
    add_edges(G, T, L);
add_edges(_, [], _) ->
    ok.

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

-spec get_timestamp({utcTime | generalTime, string()}) -> string().
get_timestamp({utcTime, [Y1,Y2|T]}) ->
    case list_to_integer([Y1,Y2]) of
        N when N >= 50 -> [$1,$9,Y1,Y2|T];
	_ -> [$2,$0,Y1,Y2|T]
    end;
get_timestamp({generalTime, TS}) ->
    TS.

wildcard(Path) when is_binary(Path) ->
    wildcard(binary_to_list(Path));
wildcard(Path) ->
    filelib:wildcard(Path).

-spec use_cache() -> boolean().
use_cache() ->
    ejabberd_config:use_cache(global).

-spec init_cache() -> ok.
init_cache() ->
    ets_cache:new(?CA_CACHE, cache_opts()).

-spec delete_cache() -> ok.
delete_cache() ->
    ets_cache:delete(?CA_CACHE).

-spec cache_opts() -> [proplists:property()].
cache_opts() ->
    MaxSize = ejabberd_config:cache_size(global),
    CacheMissed = ejabberd_config:cache_missed(global),
    LifeTime = case ejabberd_config:cache_life_time(global) of
                   infinity -> infinity;
                   I -> timer:seconds(I)
               end,
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].
