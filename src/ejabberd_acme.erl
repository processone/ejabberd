-module (ejabberd_acme).

-export([%% Ejabberdctl Commands
	 get_certificates/2,
	 renew_certificates/0,
	 list_certificates/1,
	 revoke_certificate/1,
	 %% Command Options Validity
	 is_valid_account_opt/1,
	 is_valid_verbose_opt/1,
	 is_valid_domain_opt/1,
	 is_valid_revoke_cert/1,
	 %% Called by ejabberd_pkix
	 certificate_exists/1,
	 %% Key Related
	 generate_key/0,
	 to_public/1
	]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-include("ejabberd_acme.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([opt_type/1]).

-behavior(ejabberd_config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Command Functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Check Validity of command options
%%

-spec is_valid_account_opt(string()) -> boolean().
is_valid_account_opt("old-account") -> true;
is_valid_account_opt("new-account") -> true;
is_valid_account_opt(_) -> false.

-spec is_valid_verbose_opt(string()) -> boolean().
is_valid_verbose_opt("plain") -> true;
is_valid_verbose_opt("verbose") -> true;
is_valid_verbose_opt(_) -> false.

%% TODO: Make this check more complicated
-spec is_valid_domain_opt(string()) -> boolean().
is_valid_domain_opt("all") -> true;
is_valid_domain_opt(DomainString) ->
    case parse_domain_string(DomainString) of
	[] ->
	    false;
	SeparatedDomains ->
	    true
    end.

-spec is_valid_revoke_cert(string()) -> boolean().
is_valid_revoke_cert(DomainOrFile) ->	    
    lists:prefix("file:", DomainOrFile) orelse
	lists:prefix("domain:", DomainOrFile).
	    


%%
%% Get Certificate
%%

-spec get_certificates(domains_opt(), account_opt()) -> string() | {'error', _}.
get_certificates(Domains, NewAccountOpt) ->
    try
	CAUrl = get_config_ca_url(),
	get_certificates0(CAUrl, Domains, NewAccountOpt)
    catch
	throw:Throw ->
	    Throw;
	E:R ->
	    ?ERROR_MSG("Unknown ~p:~p, ~p", [E, R, erlang:get_stacktrace()]), 
	    {error, get_certificates}
    end.

-spec get_certificates0(url(), domains_opt(), account_opt()) -> string().
get_certificates0(CAUrl, Domains, "old-account") ->
    %% Get the current account
    {ok, _AccId, PrivateKey} = ensure_account_exists(),

    get_certificates1(CAUrl, Domains, PrivateKey);

get_certificates0(CAUrl, Domains, "new-account") ->
    %% Create a new account and save it to disk
    {ok, _Id, PrivateKey} = create_save_new_account(CAUrl),

    get_certificates1(CAUrl, Domains, PrivateKey).

-spec get_certificates1(url(), domains_opt(), jose_jwk:key()) -> string().
get_certificates1(CAUrl, "all", PrivateKey) ->
    Hosts = get_config_hosts(),
    get_certificates2(CAUrl, PrivateKey, Hosts);
get_certificates1(CAUrl, DomainString, PrivateKey) ->
    Domains = parse_domain_string(DomainString),
    Hosts = [list_to_bitstring(D) || D <- Domains],
    get_certificates2(CAUrl, PrivateKey, Hosts).

-spec get_certificates2(url(), jose_jwk:key(), [bitstring()]) -> string().
get_certificates2(CAUrl, PrivateKey, Hosts) ->
    %% Get a certificate for each host
    PemCertKeys = [get_certificate(CAUrl, Host, PrivateKey) || Host <- Hosts],

    %% Save Certificates
    SavedCerts = [save_certificate(Cert) || Cert <- PemCertKeys],

    %% Format the result to send back to ejabberdctl
    format_get_certificates_result(SavedCerts).

-spec format_get_certificates_result([{'ok', bitstring(), _} | 
				      {'error', bitstring(), _}]) ->
					    string().
format_get_certificates_result(Certs) ->
    Cond = lists:all(fun(Cert) ->
			     not is_error(Cert)
		     end, Certs),
    FormattedCerts = lists:join($\n,
				[format_get_certificate(C) || C <- Certs]),
    case Cond of
	true ->
	    Result = io_lib:format("Success:~n~s", [FormattedCerts]),
	    lists:flatten(Result); 
	_ ->
	    Result = io_lib:format("Error with one or more certificates~n~s", [lists:flatten(FormattedCerts)]),
	    lists:flatten(Result)
    end.

-spec format_get_certificate({'ok', bitstring(), _} | 
			     {'error', bitstring(), _}) ->
				    string().
format_get_certificate({ok, Domain, saved}) ->    
    io_lib:format("  Certificate for domain: \"~s\" acquired and saved", [Domain]);
format_get_certificate({ok, Domain, not_found}) ->    
    io_lib:format("  Certificate for domain: \"~s\" not found, so it was not renewed", [Domain]);
format_get_certificate({ok, Domain, exists}) ->    
    io_lib:format("  Certificate for domain: \"~s\" is not close to expiring", [Domain]);
format_get_certificate({error, Domain, Reason}) ->
    io_lib:format("  Error for domain: \"~s\",  with reason: \'~s\'", [Domain, Reason]).

-spec get_certificate(url(), bitstring(), jose_jwk:key()) -> 
			     {'ok', bitstring(), pem()} | 
			     {'error', bitstring(), _}.
get_certificate(CAUrl, DomainName, PrivateKey) ->
    ?INFO_MSG("Getting a Certificate for domain: ~p~n", [DomainName]),
    try
	{ok, _Authz} = create_new_authorization(CAUrl, DomainName, PrivateKey),
	create_new_certificate(CAUrl, DomainName, PrivateKey)
    catch
	throw:Throw ->
	    Throw;
	E:R ->
	    ?ERROR_MSG("Unknown ~p:~p, ~p", [E, R, erlang:get_stacktrace()]), 
	    {error, DomainName, get_certificate}
    end.

-spec create_save_new_account(url()) -> {'ok', string(), jose_jwk:key()} | no_return().
create_save_new_account(CAUrl) ->
    %% Get contact from configuration file
    Contact = get_config_contact(),

    %% Generate a Key
    PrivateKey = generate_key(),

    %% Create a new account
    {ok, Id} = create_new_account(CAUrl, Contact, PrivateKey),

    %% Write Persistent Data
    ok = write_account_persistent({Id, PrivateKey}),

    {ok, Id, PrivateKey}.

%% TODO:
%% Find a way to ask the user if he accepts the TOS
-spec create_new_account(url(), bitstring(), jose_jwk:key()) -> {'ok', string()} | 
								no_return().
create_new_account(CAUrl, Contact, PrivateKey) ->
    try
	{ok, Dirs, Nonce0} = ejabberd_acme_comm:directory(CAUrl),
	Req0 = [{ <<"contact">>, [Contact]}],
	{ok, {TOS, Account}, Nonce1} = 
	    ejabberd_acme_comm:new_account(Dirs, PrivateKey, Req0, Nonce0),
	{<<"id">>, AccIdInt} = lists:keyfind(<<"id">>, 1, Account),
	AccId = integer_to_list(AccIdInt),
	Req1 = [{ <<"agreement">>, list_to_bitstring(TOS)}],
	{ok, _Account2, _Nonce2} = 
	    ejabberd_acme_comm:update_account({CAUrl, AccId}, PrivateKey, Req1, Nonce1),
	{ok, AccId}
    catch
	E:R ->
	    ?ERROR_MSG("Error: ~p creating an account for contact: ~p", 
		       [{E,R}, Contact]),
	    throw({error,create_new_account})
    end.

-spec create_new_authorization(url(), bitstring(), jose_jwk:key()) ->
				      {'ok', proplist()} | no_return().
create_new_authorization(CAUrl, DomainName, PrivateKey) ->
    try
	{ok, Dirs, Nonce0} = ejabberd_acme_comm:directory(CAUrl),
	Req0 = [{<<"identifier">>,
		 {[{<<"type">>, <<"dns">>},
		   {<<"value">>, DomainName}]}},
		{<<"existing">>, <<"accept">>}],
	{ok, {AuthzUrl, Authz}, Nonce1} = 
	    ejabberd_acme_comm:new_authz(Dirs, PrivateKey, Req0, Nonce0),
	{ok, AuthzId} = location_to_id(AuthzUrl),

	Challenges = get_challenges(Authz),
	{ok, ChallengeUrl, KeyAuthz} =
	    acme_challenge:solve_challenge(<<"http-01">>, Challenges, PrivateKey),
	{ok, ChallengeId} = location_to_id(ChallengeUrl),
	Req3 = [{<<"type">>, <<"http-01">>},{<<"keyAuthorization">>, KeyAuthz}],
	{ok, _SolvedChallenge, _Nonce2} = ejabberd_acme_comm:complete_challenge(
					    {CAUrl, AuthzId, ChallengeId}, PrivateKey, Req3, Nonce1),

	{ok, AuthzValid, _Nonce} = ejabberd_acme_comm:get_authz_until_valid({CAUrl, AuthzId}),
	{ok, AuthzValid}
    catch
	E:R ->
	    ?ERROR_MSG("Error: ~p getting an authorization for domain: ~p~n",
		       [{E,R}, DomainName]),
	    throw({error, DomainName, authorization})
    end.

-spec create_new_certificate(url(), bitstring(), jose_jwk:key()) -> 
				    {ok, bitstring(), pem()}.
create_new_certificate(CAUrl, DomainName, PrivateKey) ->
    try
	{ok, Dirs, Nonce0} = ejabberd_acme_comm:directory(CAUrl),
	CSRSubject = [{commonName, bitstring_to_list(DomainName)}],
	{CSR, CSRKey} = make_csr(CSRSubject),
	{NotBefore, NotAfter} = not_before_not_after(),
	Req =
	    [{<<"csr">>, CSR},
	     {<<"notBefore">>, NotBefore},
	     {<<"NotAfter">>, NotAfter}
	    ],
	{ok, {_CertUrl, Certificate}, _Nonce1} = 
	    ejabberd_acme_comm:new_cert(Dirs, PrivateKey, Req, Nonce0),

	DecodedCert = public_key:pkix_decode_cert(list_to_binary(Certificate), plain),	
	PemEntryCert = public_key:pem_entry_encode('Certificate', DecodedCert),

	{_, CSRKeyKey} = jose_jwk:to_key(CSRKey),
	PemEntryKey = public_key:pem_entry_encode('ECPrivateKey', CSRKeyKey),

	PemCertKey = public_key:pem_encode([PemEntryKey, PemEntryCert]),

	{ok, DomainName, PemCertKey}
    catch		     
	E:R ->
	    ?ERROR_MSG("Error: ~p getting an authorization for domain: ~p~n",
		       [{E,R}, DomainName]),
	    throw({error, DomainName, certificate})
    end.

-spec ensure_account_exists() -> {ok, string(), jose_jwk:key()}.
ensure_account_exists() ->
    case read_account_persistent() of
	none ->
	    ?ERROR_MSG("No existing account", []),
	    throw({error, no_old_account});
	{ok, AccId, PrivateKey} ->
	    {ok, AccId, PrivateKey}
    end.


%%
%% Renew Certificates
%%
-spec renew_certificates() -> string() | {'error', _}.
renew_certificates() ->
    try
	CAUrl = get_config_ca_url(),
        renew_certificates0(CAUrl)
    catch
	throw:Throw ->
	    Throw;
	E:R ->
	    ?ERROR_MSG("Unknown ~p:~p, ~p", [E, R, erlang:get_stacktrace()]), 
	    {error, get_certificates}
    end.

-spec renew_certificates0(url()) -> string().
renew_certificates0(CAUrl) ->
    %% Get the current account
    {ok, _AccId, PrivateKey} = ensure_account_exists(),

    %% Find all hosts that we have certificates for
    Certs = read_certificates_persistent(),

    %% Get a certificate for each host
    PemCertKeys = [renew_certificate(CAUrl, Cert, PrivateKey) || Cert <- Certs],

    %% Save Certificates
    SavedCerts = [save_renewed_certificate(Cert) || Cert <- PemCertKeys],

    %% Format the result to send back to ejabberdctl
    format_get_certificates_result(SavedCerts).

-spec renew_certificate(url(), {bitstring(), data_cert()}, jose_jwk:key()) -> 
			       {'ok', bitstring(), _} | 
			       {'error', bitstring(), _}.
renew_certificate(CAUrl, {DomainName, _} = Cert, PrivateKey) ->
    case cert_to_expire(Cert) of
	true ->
	    get_certificate(CAUrl, DomainName, PrivateKey);
	false ->
	    {ok, DomainName, no_expire}
    end.


-spec cert_to_expire({bitstring(), data_cert()}) -> boolean().
cert_to_expire({DomainName, #data_cert{pem = Pem}}) ->
    Certificate = pem_to_certificate(Pem),
    Validity = get_utc_validity(Certificate),

    %% 30 days before expiration
    close_to_expire(Validity, 30).

-spec close_to_expire(string(), integer()) -> boolean().
close_to_expire(Validity, Days) ->
    {ValidDate, _ValidTime} = utc_string_to_datetime(Validity),
    ValidDays = calendar:date_to_gregorian_days(ValidDate),

    {CurrentDate, _CurrentTime} = calendar:universal_time(),
    CurrentDays = calendar:date_to_gregorian_days(CurrentDate),
    CurrentDays > ValidDays - Days.



%%
%% List Certificates
%%
-spec list_certificates(verbose_opt()) -> [string()] | [any()] | {error, _}.
list_certificates(Verbose) ->
    try
	list_certificates0(Verbose)
    catch
	throw:Throw ->
	    Throw;
	E:R ->
	    ?ERROR_MSG("Unknown ~p:~p, ~p", [E, R, erlang:get_stacktrace()]), 
	    {error, list_certificates}
    end.

-spec list_certificates0(verbose_opt()) -> [string()] | [any()].
list_certificates0(Verbose) ->
    Certs = read_certificates_persistent(),
    [format_certificate(DataCert, Verbose) || {_Key, DataCert} <- Certs].

%% TODO: Make this cleaner and more robust
-spec format_certificate(data_cert(), verbose_opt()) -> string().
format_certificate(DataCert, Verbose) ->
    #data_cert{
       domain = DomainName,
       pem = PemCert,
       path = Path
      } = DataCert,

    try
	Certificate = pem_to_certificate(PemCert),

	%% Find the commonName
	_CommonName = get_commonName(Certificate),

	%% Find the notAfter date
	NotAfter = get_notAfter(Certificate),

	case Verbose of
	    "plain" ->
		format_certificate_plain(DomainName, NotAfter, Path);
	    "verbose" ->
		format_certificate_verbose(DomainName, NotAfter, PemCert)
	end
    catch
	E:R ->
	    ?ERROR_MSG("Unknown ~p:~p, ~p", [E, R, erlang:get_stacktrace()]), 
	    fail_format_certificate(DomainName)
    end.

-spec format_certificate_plain(bitstring(), {expired | ok, string()}, string()) -> string().
format_certificate_plain(DomainName, NotAfter, Path) ->
    Result = lists:flatten(io_lib:format(
			     "  Domain: ~s~n" 
			     "    ~s~n" 
			     "    Path: ~s", 
			     [DomainName, format_validity(NotAfter), Path])),
    Result.

-spec format_certificate_verbose(bitstring(), {expired | ok, string()}, bitstring()) -> string().
format_certificate_verbose(DomainName, NotAfter, PemCert) ->
    Result = lists:flatten(io_lib:format(
			     "  Domain: ~s~n" 
			     "    ~s~n" 
			     "    Certificate In PEM format: ~n~s", 
			     [DomainName, format_validity(NotAfter), PemCert])),
    Result.

-spec format_validity({'expired' | 'ok', string()}) -> string().
format_validity({expired, NotAfter}) ->
    io_lib:format("Expired at: ~s UTC", [NotAfter]);
format_validity({ok, NotAfter}) ->
    io_lib:format("Valid until: ~s UTC", [NotAfter]).

-spec fail_format_certificate(bitstring()) -> string().
fail_format_certificate(DomainName) ->
    Result = lists:flatten(io_lib:format(
			     "  Domain: ~s~n" 
			     "    Failed to format Certificate", 
			     [DomainName])),
    Result.

-spec get_commonName(#'Certificate'{}) -> string().
get_commonName(#'Certificate'{tbsCertificate = TbsCertificate}) ->
    #'TBSCertificate'{
       subject = {rdnSequence, SubjectList}
      } = TbsCertificate, 

    %% TODO: Not the best way to find the commonName
    ShallowSubjectList = [Attribute || [Attribute] <- SubjectList],
    {_, _, CommonName} = lists:keyfind(attribute_oid(commonName), 2, ShallowSubjectList),

    %% TODO: Remove the length-encoding from the commonName before returning it
    CommonName.

-spec get_notAfter(#'Certificate'{}) -> {expired | ok, string()}.
get_notAfter(Certificate) ->
    UtcTime = get_utc_validity(Certificate),
    %% TODO: Find a library function to decode utc time
    [Y1,Y2,MO1,MO2,D1,D2,H1,H2,MI1,MI2,S1,S2,$Z] = UtcTime,
    YEAR = case list_to_integer([Y1,Y2]) >= 50 of
	       true -> "19" ++ [Y1,Y2];
	       _ -> "20" ++ [Y1,Y2]
	   end,
    NotAfter = lists:flatten(io_lib:format("~s-~s-~s ~s:~s:~s", 
					   [YEAR, [MO1,MO2], [D1,D2],
					    [H1,H2], [MI1,MI2], [S1,S2]])), 

    case close_to_expire(UtcTime, 0) of
	true ->
	    {expired, NotAfter};
	false ->
	    {ok, NotAfter}
    end.

-spec get_utc_validity(#'Certificate'{}) -> string().
get_utc_validity(#'Certificate'{tbsCertificate = TbsCertificate}) ->
    #'TBSCertificate'{
       validity = Validity
      } = TbsCertificate,

    #'Validity'{notAfter = {utcTime, UtcTime}} = Validity,
    UtcTime.

%%
%% Revoke Certificate
%%

-spec revoke_certificate(string()) -> {ok, deleted} | {error, _}.
revoke_certificate(DomainOrFile) ->
    try
	CAUrl = get_config_ca_url(),
	revoke_certificate0(CAUrl, DomainOrFile)
    catch
	throw:Throw ->
	    Throw;
	E:R ->
	    ?ERROR_MSG("Unknown ~p:~p, ~p", [E, R, erlang:get_stacktrace()]), 
	    {error, revoke_certificate}
    end.	

-spec revoke_certificate0(url(), string()) -> {ok, deleted}.
revoke_certificate0(CAUrl, DomainOrFile) ->
    ParsedCert = parse_revoke_cert_argument(DomainOrFile),
    revoke_certificate1(CAUrl, ParsedCert).

-spec revoke_certificate1(url(), {domain, bitstring()} | {file, file:filename()}) -> 
				 {ok, deleted}.
revoke_certificate1(CAUrl, {domain, Domain}) ->
    case domain_certificate_exists(Domain) of
	{Domain, Cert = #data_cert{pem=PemCert}} ->
	    ok = revoke_certificate2(CAUrl, PemCert),
	    ok = remove_certificate_persistent(Cert),
	    {ok, deleted};
	false ->
	    ?ERROR_MSG("Certificate for domain: ~p not found", [Domain]),
	    throw({error, not_found})
    end;
revoke_certificate1(CAUrl, {file, File}) ->
    case file:read_file(File) of
	{ok, Pem} ->
	    ok = revoke_certificate2(CAUrl, Pem),
	    {ok, deleted};
	{error, Reason} ->
	    ?ERROR_MSG("Error: ~p reading pem certificate-key file: ~p", [Reason, File]),
	    throw({error, Reason})
    end.
	

-spec revoke_certificate2(url(), pem()) -> ok.
revoke_certificate2(CAUrl, PemEncodedCert) ->
    {Certificate, CertPrivateKey} = prepare_certificate_revoke(PemEncodedCert),

    {ok, Dirs, Nonce} = ejabberd_acme_comm:directory(CAUrl),

    Req = [{<<"certificate">>, Certificate}],
    {ok, [], Nonce1} = ejabberd_acme_comm:revoke_cert(Dirs, CertPrivateKey, Req, Nonce),
    ok.

-spec parse_revoke_cert_argument(string()) -> {domain, bitstring()} | {file, file:filename()}.
parse_revoke_cert_argument([$f, $i, $l, $e, $:|File]) ->
    {file, File};
parse_revoke_cert_argument([$d, $o, $m, $a, $i, $n, $: | Domain]) ->
    {domain, list_to_bitstring(Domain)}.

-spec prepare_certificate_revoke(pem()) -> {bitstring(), jose_jwk:key()}.
prepare_certificate_revoke(PemEncodedCert) ->
    PemList = public_key:pem_decode(PemEncodedCert),
    PemCertEnc = lists:keyfind('Certificate', 1, PemList),
    PemCert = public_key:pem_entry_decode(PemCertEnc),
    DerCert = public_key:der_encode('Certificate', PemCert),
    Base64Cert = base64url:encode(DerCert),

    {ok, Key} = find_private_key_in_pem(PemEncodedCert),    
    {Base64Cert, Key}.

-spec domain_certificate_exists(bitstring()) -> {bitstring(), data_cert()} | false.    
domain_certificate_exists(Domain) ->
    Certs = read_certificates_persistent(),
    lists:keyfind(Domain, 1, Certs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Called by ejabberd_pkix to check
%% if a certificate exists for a 
%% specific host
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec certificate_exists(bitstring()) -> {true, file:filename()} | false.
certificate_exists(Host) ->
    Certificates = read_certificates_persistent(),
    case lists:keyfind(Host, 1 , Certificates) of
	false ->
	    false;
	{Host, #data_cert{path=Path}} ->
	    {true, Path}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Certificate Request Functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% For now we accept only generating a key of
%% specific type for signing the csr
%% TODO: Make this function handle more signing keys
%%  1. Derive oid from Key
%%  2. Derive the whole algo objects from Key
%% TODO: Encode Strings using length using a library function

-spec make_csr(proplist()) -> {binary(), jose_jwk:key()}.
make_csr(Attributes) ->
    Key = generate_key(),
    {_, KeyKey} = jose_jwk:to_key(Key),
    KeyPub = to_public(Key),
    try
	SubPKInfoAlgo = subject_pk_info_algo(KeyPub),
	{ok, RawBinPubKey} = raw_binary_public_key(KeyPub),
	SubPKInfo = subject_pk_info(SubPKInfoAlgo, RawBinPubKey),
	{ok, Subject} = attributes_from_list(Attributes),
	CRI = certificate_request_info(SubPKInfo, Subject),
	{ok, EncodedCRI} = der_encode(
			     'CertificationRequestInfo',
			     CRI),
	SignedCRI = public_key:sign(EncodedCRI, 'sha256', KeyKey),
	SignatureAlgo = signature_algo(Key, 'sha256'),
	CSR = certification_request(CRI, SignatureAlgo, SignedCRI),
	{ok, DerCSR} = der_encode(
			 'CertificationRequest',
			 CSR),
	Result = base64url:encode(DerCSR),
	{Result, Key}
    catch
	_:{badmatch, {error, bad_public_key}} ->
	    {error, bad_public_key};
	_:{badmatch, {error, bad_attributes}} ->
	    {error, bad_public_key};
	_:{badmatch, {error, der_encode}} ->
	    {error, der_encode}
    end.



subject_pk_info_algo(_KeyPub) ->
    #'SubjectPublicKeyInfoAlgorithm'{
       algorithm = ?'id-ecPublicKey',
       parameters = {asn1_OPENTYPE,<<6,8,42,134,72,206,61,3,1,7>>}
      }.

subject_pk_info(Algo, RawBinPubKey) ->
    #'SubjectPublicKeyInfo-PKCS-10'{
       algorithm = Algo,
       subjectPublicKey = RawBinPubKey
      }.

certificate_request_info(SubPKInfo, Subject) ->
    #'CertificationRequestInfo'{
       version = 0,
       subject = Subject,
       subjectPKInfo = SubPKInfo,
       attributes = []
      }.

signature_algo(_Key, _Hash) ->
    #'CertificationRequest_signatureAlgorithm'{
       algorithm = ?'ecdsa-with-SHA256',
       parameters = asn1_NOVALUE
      }.

certification_request(CRI, SignatureAlgo, SignedCRI) ->
    #'CertificationRequest'{
       certificationRequestInfo = CRI,
       signatureAlgorithm = SignatureAlgo,
       signature = SignedCRI
      }.

raw_binary_public_key(KeyPub) ->
    try
	{_, RawPubKey} = jose_jwk:to_key(KeyPub),
	{{_, RawBinPubKey}, _} = RawPubKey,
	{ok, RawBinPubKey}
    catch
	_:_ ->
	    ?ERROR_MSG("Bad public key: ~p~n", [KeyPub]),
	    {error, bad_public_key}
    end.

der_encode(Type, Term) ->
    try
	{ok, public_key:der_encode(Type, Term)}
    catch
	_:_ ->
	    ?ERROR_MSG("Cannot DER encode: ~p, with asn1type: ~p", [Term, Type]),
	    {error, der_encode}
    end.

%%
%% Attributes Parser
%%

attributes_from_list(Attrs) ->
    ParsedAttrs = [attribute_parser_fun(Attr) || Attr <- Attrs],
    case lists:any(fun is_error/1, ParsedAttrs) of
	true ->
	    {error, bad_attributes};
	false ->
	    {ok, {rdnSequence, [[PAttr] || PAttr <- ParsedAttrs]}}
    end.

attribute_parser_fun({AttrName, AttrVal}) ->
    try
	#'AttributeTypeAndValue'{
	   type = attribute_oid(AttrName),
	   %% TODO: Check if every attribute should be encoded as
	   %% common name. Actually it doesn't matter in
	   %% practice. Only in theory in order to have cleaner code.
	   value = public_key:der_encode('X520CommonName', {printableString, AttrVal})
	   %% value = length_bitstring(list_to_bitstring(AttrVal))
	  }
    catch
	_:_ ->
	    ?ERROR_MSG("Bad attribute: ~p~n", [{AttrName, AttrVal}]),
	    {error, bad_attributes}
    end.

-spec attribute_oid(atom()) -> tuple() | no_return().
attribute_oid(commonName) -> ?'id-at-commonName';
attribute_oid(countryName) -> ?'id-at-countryName';
attribute_oid(stateOrProvinceName) -> ?'id-at-stateOrProvinceName';
attribute_oid(localityName) -> ?'id-at-localityName';
attribute_oid(organizationName) -> ?'id-at-organizationName';
attribute_oid(_) -> error(bad_attributes).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Useful funs
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec location_to_id(url()) -> {ok, string()} | {error, not_found}.
location_to_id(Url0) ->
    Url = string:strip(Url0, right, $/),
    case string:rchr(Url, $/) of
	0 ->
	    ?ERROR_MSG("Couldn't find id in url: ~p~n", [Url]),
	    {error, not_found};
	Ind ->
	    {ok, string:sub_string(Url, Ind+1)}
    end.

-spec get_challenges(proplist()) -> [{proplist()}].
get_challenges(Body) ->
    {<<"challenges">>, Challenges} = proplists:lookup(<<"challenges">>, Body),
    Challenges.

-spec not_before_not_after() -> {binary(), binary()}.
not_before_not_after() ->
    %% TODO: Make notBefore and notAfter configurable somewhere
    {MegS, Sec, MicS} = erlang:timestamp(),
    NotBefore = xmpp_util:encode_timestamp({MegS, Sec, MicS}),
    %% The certificate will be valid for 90 Days after today
    NotAfter = xmpp_util:encode_timestamp({MegS+7, Sec+776000, MicS}),
    {NotBefore, NotAfter}.

-spec to_public(jose_jwk:key()) -> jose_jwk:key().
to_public(PrivateKey) ->
    jose_jwk:to_public(PrivateKey).
%% case jose_jwk:to_key(PrivateKey) of
%% 	#'RSAPrivateKey'{modulus = Mod, publicExponent = Exp} ->
%% 	    Public = #'RSAPublicKey'{modulus = Mod, publicExponent = Exp},
%% 	    jose_jwk:from_key(Public);
%% 	_ ->
%% 	    jose_jwk:to_public(PrivateKey)
%% end.

%% to_public(#'RSAPrivateKey'{modulus = Mod, publicExponent = Exp}) ->
%%     #'RSAPublicKey'{modulus = Mod, publicExponent = Exp};
%% to_public(PrivateKey) ->
%%     jose_jwk:to_public(PrivateKey).

-spec pem_to_certificate(pem()) -> #'Certificate'{}.
pem_to_certificate(Pem) ->
    PemList = public_key:pem_decode(Pem),
    PemEntryCert = lists:keyfind('Certificate', 1, PemList),
    Certificate = public_key:pem_entry_decode(PemEntryCert),
    Certificate.

%% TODO: Find a better and more robust way to parse the utc string
-spec utc_string_to_datetime(string()) -> calendar:datetime().
utc_string_to_datetime(UtcString) ->
    try
	[Y1,Y2,MO1,MO2,D1,D2,H1,H2,MI1,MI2,S1,S2,$Z] = UtcString,
	Year = list_to_integer("20" ++ [Y1,Y2]),
	Month = list_to_integer([MO1, MO2]),
	Day = list_to_integer([D1,D2]),
	Hour = list_to_integer([H1,H2]),
	Minute = list_to_integer([MI1,MI2]),
	Second = list_to_integer([S1,S2]),
	{{Year, Month, Day}, {Hour, Minute, Second}}
    catch
	E:R ->
	    ?ERROR_MSG("Unable to parse UTC string", []),
	    throw({error, utc_string_to_datetime})
    end.

-spec find_private_key_in_pem(pem()) -> {ok, jose_jwk:key()} | false.
find_private_key_in_pem(Pem) ->
    PemList = public_key:pem_decode(Pem),
    case find_private_key_in_pem1(private_key_types(), PemList) of
	false ->
	    false;
	PemKey ->
	    Key = public_key:pem_entry_decode(PemKey),
	    JoseKey = jose_jwk:from_key(Key),
	    {ok, JoseKey}
    end.
	    

-spec find_private_key_in_pem1([public_key:pki_asn1_type()],
			       [public_key:pem_entry()]) ->
				      public_key:pem_entry() | false.
find_private_key_in_pem1([], _PemList) ->
    false;
find_private_key_in_pem1([Type|Types], PemList) ->
    case lists:keyfind(Type, 1, PemList) of
	false ->
	    find_private_key_in_pem1(Types, PemList);
	Key ->
	    Key
    end.


-spec parse_domain_string(string()) -> [string()].
parse_domain_string(DomainString) ->
    string:tokens(DomainString, ";").

-spec private_key_types() -> [public_key:pki_asn1_type()].
private_key_types() ->
    ['RSAPrivateKey',
     'DSAPrivateKey',
     'ECPrivateKey'].

-spec is_error(_) -> boolean().
is_error({error, _}) -> true;
is_error({error, _, _}) -> true;
is_error(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Handle the persistent data structure
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec data_empty() -> [].
data_empty() ->
    [].

%%
%% Account
%%

-spec data_get_account(acme_data()) -> {ok, list(), jose_jwk:key()} | none.
data_get_account(Data) ->
    case lists:keyfind(account, 1, Data) of
	{account, #data_acc{id = AccId, key = PrivateKey}} ->
	    {ok, AccId, PrivateKey};
        false ->
	    none
    end.

-spec data_set_account(acme_data(), {list(), jose_jwk:key()}) -> acme_data().
data_set_account(Data, {AccId, PrivateKey}) -> 
    NewAcc = {account, #data_acc{id = AccId, key = PrivateKey}},
    lists:keystore(account, 1, Data, NewAcc).

%%
%% Certificates
%% 

-spec data_get_certificates(acme_data()) -> data_certs().
data_get_certificates(Data) ->
    case lists:keyfind(certs, 1, Data) of
	{certs, Certs} ->
	    Certs;
        false ->
	    []
    end.

-spec data_set_certificates(acme_data(), data_certs()) -> acme_data().
data_set_certificates(Data, NewCerts) -> 
    lists:keystore(certs, 1, Data, {certs, NewCerts}).

%% ATM we preserve one certificate for each domain
-spec data_add_certificate(acme_data(), data_cert()) -> acme_data().
data_add_certificate(Data, DataCert = #data_cert{domain=Domain}) ->
    Certs = data_get_certificates(Data),
    NewCerts = lists:keystore(Domain, 1, Certs, {Domain, DataCert}),
    data_set_certificates(Data, NewCerts).

-spec data_remove_certificate(acme_data(), data_cert()) -> acme_data().
data_remove_certificate(Data, DataCert = #data_cert{domain=Domain}) ->
    Certs = data_get_certificates(Data),
    NewCerts = lists:keydelete(Domain, 1, Certs),
    data_set_certificates(Data, NewCerts).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Handle Config and Persistence Files
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec persistent_file() -> file:filename().
persistent_file() ->
    MnesiaDir = mnesia:system_info(directory),
    filename:join(MnesiaDir, "acme.DAT").

%% The persistent file should be read and written only by its owner
-spec persistent_file_mode() -> 384.
persistent_file_mode() ->
    8#400 + 8#200. 

-spec read_persistent() -> {ok, acme_data()} | no_return().
read_persistent() ->
    case file:read_file(persistent_file()) of
	{ok, Binary} ->
	    {ok, binary_to_term(Binary)};
	{error, enoent} ->
	    create_persistent(),
	    {ok, data_empty()};
	{error, Reason} ->
	    ?ERROR_MSG("Error: ~p reading acme data file", [Reason]),
	    throw({error, Reason})
    end.

-spec write_persistent(acme_data()) -> ok | no_return().
write_persistent(Data) ->
    Binary = term_to_binary(Data),
    case file:write_file(persistent_file(), Binary) of
	ok -> ok;
	{error, Reason} ->
	    ?ERROR_MSG("Error: ~p writing acme data file", [Reason]),
	    throw({error, Reason})
    end.    

-spec create_persistent() -> ok | no_return().
create_persistent() ->
    Binary = term_to_binary(data_empty()),
    case file:write_file(persistent_file(), Binary) of
	ok ->
	    case file:change_mode(persistent_file(), persistent_file_mode()) of
		ok -> ok;
		{error, Reason} ->
		    ?ERROR_MSG("Error: ~p changing acme data file mode", [Reason]),
		    throw({error, Reason})
	    end;
	{error, Reason} ->
	    ?ERROR_MSG("Error: ~p creating acme data file", [Reason]),
	    throw({error, Reason})
    end.        

-spec write_account_persistent({list(), jose_jwk:key()}) -> ok | no_return().
write_account_persistent({AccId, PrivateKey}) ->
    {ok, Data} = read_persistent(),
    NewData = data_set_account(Data, {AccId, PrivateKey}),
    ok = write_persistent(NewData).

-spec read_account_persistent() -> {ok, list(), jose_jwk:key()} | none.
read_account_persistent() ->
    {ok, Data} = read_persistent(),
    data_get_account(Data).

-spec read_certificates_persistent() -> data_certs().
read_certificates_persistent() ->
    {ok, Data} = read_persistent(),
    data_get_certificates(Data).

-spec add_certificate_persistent(data_cert()) -> ok.
add_certificate_persistent(DataCert) ->
    {ok, Data} = read_persistent(),
    NewData = data_add_certificate(Data, DataCert),
    ok = write_persistent(NewData).

-spec remove_certificate_persistent(data_cert()) -> ok.
remove_certificate_persistent(DataCert) ->
    {ok, Data} = read_persistent(),
    NewData = data_remove_certificate(Data, DataCert),
    ok = write_persistent(NewData).

-spec save_certificate({ok, bitstring(), binary()} | {error, _, _}) -> 
			      {ok, bitstring(), saved} | {error, bitstring(), _}.
save_certificate({error, _, _} = Error) ->
    Error;
save_certificate({ok, DomainName, Cert}) ->
    try
        CertDir = get_config_cert_dir(),
	DomainString = bitstring_to_list(DomainName),
	CertificateFile = filename:join([CertDir, DomainString ++ "_cert.pem"]),
	%% TODO: At some point do the following using a Transaction so
	%% that there is no certificate saved if it cannot be added in
	%% certificate persistent storage
	write_cert(CertificateFile, Cert, DomainName),
	DataCert = #data_cert{
		      domain = DomainName,
		      pem = Cert,
		      path = CertificateFile
		     },
	add_certificate_persistent(DataCert),
	{ok, DomainName, saved}
    catch
	throw:Throw ->
	    Throw;
	E:R ->
	    ?ERROR_MSG("Unknown ~p:~p, ~p", [E, R, erlang:get_stacktrace()]), 
	    {error, DomainName, saving}
    end.

-spec save_renewed_certificate({ok, bitstring(), _} | {error, _, _}) -> 
				      {ok, bitstring(), _} | {error, bitstring(), _}.
save_renewed_certificate({error, _, _} = Error) ->
    Error;
save_renewed_certificate({ok, _, no_expire} = Cert) ->
    Cert;
save_renewed_certificate({ok, DomainName, Cert}) ->
    save_certificate({ok, DomainName, Cert}).

-spec write_cert(file:filename(), binary(), bitstring()) -> {ok, bitstring(), saved}.
write_cert(CertificateFile, Cert, DomainName) ->
    case file:write_file(CertificateFile, Cert) of
	ok ->
	    {ok, DomainName, saved};
	{error, Reason} ->
	    ?ERROR_MSG("Error: ~p saving certificate at file: ~p",
		       [Reason, CertificateFile]),
	    throw({error, DomainName, saving})
    end.

-spec get_config_acme() -> [{atom(), bitstring()}].
get_config_acme() ->
    case ejabberd_config:get_option(acme, undefined) of
	undefined ->
	    ?ERROR_MSG("No acme configuration has been specified", []),
	    throw({error, configuration});
        Acme ->
	    Acme
    end.

-spec get_config_contact() -> bitstring().
get_config_contact() ->
    Acme = get_config_acme(),
    case lists:keyfind(contact, 1, Acme) of
	{contact, Contact} ->
	    Contact;
	false ->
	    ?ERROR_MSG("No contact has been specified", []),
	    throw({error, configuration_contact})
    end.

-spec get_config_ca_url() -> string().
get_config_ca_url() ->
    Acme = get_config_acme(),
    case lists:keyfind(ca_url, 1, Acme) of
	{ca_url, CAUrl} ->
	    CAUrl;
	false ->
	    ?ERROR_MSG("No CA url has been specified", []),
	    throw({error, configuration_ca_url})
    end.


-spec get_config_hosts() -> [bitstring()].
get_config_hosts() ->
    case ejabberd_config:get_option(hosts, undefined) of
	undefined ->
	    ?ERROR_MSG("No hosts have been specified", []),
	    throw({error, configuration_hosts});
        Hosts ->
	    Hosts
    end.

-spec get_config_cert_dir() -> file:filename().
get_config_cert_dir() ->
    case ejabberd_config:get_option(cert_dir, undefined) of
	undefined ->
	    ?ERROR_MSG("No cert_dir configuration has been specified", []),
	    throw({error, configuration});
        CertDir ->
	    CertDir
    end.


-ifdef(GENERATE_RSA_KEY).
generate_key() ->
    ?INFO_MSG("Generate RSA key pair~n", []),
    Key = public_key:generate_key({rsa, 2048, 65537}),
    Key1 = Key#'RSAPrivateKey'{version = 'two-prime'},
    jose_jwk:from_key(Key1).
%% jose_jwk:generate_key({rsa, 2048}).
-else.
generate_key() ->
    ?INFO_MSG("Generate EC key pair~n", []),
    jose_jwk:generate_key({ec, secp256r1}).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Option Parsing Code
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_acme_opts(AcmeOpt) ->
    [parse_acme_opt(Opt) || Opt <- AcmeOpt].


parse_acme_opt({ca_url, CaUrl}) when is_bitstring(CaUrl) ->
    {ca_url, binary_to_list(CaUrl)};
parse_acme_opt({contact, Contact}) when is_bitstring(Contact) ->
    {contact, Contact}.

parse_cert_dir_opt(Opt) when is_bitstring(Opt) ->
    true = filelib:is_dir(Opt),
    Opt.

-spec opt_type(acme) -> fun(([{ca_url, string()} | {contact, bitstring()}]) -> 
				   ([{ca_url, string()} | {contact, bitstring()}]));
	      (cert_dir) -> fun((bitstring()) -> (bitstring()));
	      (atom()) -> [atom()].
opt_type(acme) ->
    fun parse_acme_opts/1;
opt_type(cert_dir) ->
    fun parse_cert_dir_opt/1;
opt_type(_) ->
    [acme, cert_dir].
