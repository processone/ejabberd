-module (ejabberd_acme).

-export([%% Ejabberdctl Commands
	 get_certificates/2,
	 list_certificates/1,
	 %% Command Options Validity
	 is_valid_account_opt/1,
	 is_valid_verbose_opt/1,
	 %% Misc
	 generate_key/0,
	 %% Debugging Scenarios
	 scenario/3,
	 scenario0/2,
	 new_user_scenario/2
         %% Not yet implemented
	 %% key_roll_over/5
         %% delete_authz/3
	]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-include("ejabberd_acme.hrl").
-include_lib("public_key/include/public_key.hrl").



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

%%
%% List Certificates
%%

list_certificates(Verbose) ->
    {ok, Certs} = read_certificates_persistent(),
    case Verbose of
	"plain" -> 
	    [{Domain, certificate_metadata(PemCert)} || {Domain, PemCert} <- Certs];
	"verbose" ->
	    Certs
    end.

%% TODO: Make this cleaner and more secure
certificate_metadata(PemCert) ->
    PemList = public_key:pem_decode(PemCert),
    PemEntryCert = lists:keyfind('Certificate', 1, PemList),
    #'Certificate'{tbsCertificate = #'TBSCertificate'{
				       subject = {rdnSequence, SubjectList},
				       validity = Validity}} 
	= public_key:pem_entry_decode(PemEntryCert),
    
    %% Find the commonName
    %% TODO: Not the best way to find the commonName
    ?INFO_MSG("Subject List: ~p", [SubjectList]),
    ShallowSubjectList = [Attribute || [Attribute] <- SubjectList],
    {_, _, CommonName} = lists:keyfind(attribute_oid(commonName), 2, ShallowSubjectList),

    %% Find the notAfter date
    %% TODO: Find a library function to decode utc time
    #'Validity'{notAfter = {utcTime, UtcTime}} = Validity,
    [Y1,Y2,MO1,MO2,D1,D2,H1,H2,MI1,MI2,S1,S2,$Z] = UtcTime,
    YEAR = case list_to_integer([Y1,Y2]) >= 50 of
	       true -> "19" ++ [Y1,Y2];
	       _ -> "20" ++ [Y1,Y2]
	   end,
    NotAfter = lists:flatten(io_lib:format("Valid until: ~s-~s-~s ~s:~s:~s", 
					   [YEAR, [MO1,MO2], [D1,D2],
					    [H1,H2], [MI1,MI2], [S1,S2]])), 

    NotAfter.

%%
%% Get Certificate
%%

%% Needs a hell lot of cleaning
-spec get_certificates(url(), account_opt()) -> 
			      [{'ok', bitstring(), 'saved'} | {'error', bitstring(), _}] | 
			      {'error', _}.
get_certificates(CAUrl, NewAccountOpt) ->
    try
	?INFO_MSG("Persistent: ~p~n", [file:read_file_info(persistent_file())]),
	get_certificates0(CAUrl, NewAccountOpt)
    catch
	throw:Throw ->
	    Throw;
	E:R ->
	    ?ERROR_MSG("Unknown ~p:~p, ~p", [E, R, erlang:get_stacktrace()]), 
	    {error, get_certificates}
    end.

-spec get_certificates0(url(), account_opt()) -> 
			       [{'ok', bitstring(), 'saved'} | {'error', bitstring(), _}] |
			       no_return().
get_certificates0(CAUrl, "old-account") ->
    %% Get the current account
    {ok, _AccId, PrivateKey} = ensure_account_exists(),

    get_certificates1(CAUrl, PrivateKey);

get_certificates0(CAUrl, "new-account") ->
    %% Create a new account and save it to disk
    {ok, _Id, PrivateKey} = create_save_new_account(CAUrl),
    
    get_certificates1(CAUrl, PrivateKey).

-spec get_certificates1(url(), jose_jwk:key()) -> 
			       [{'ok', bitstring(), 'saved'} | {'error', bitstring(), _}] |
			       no_return().
get_certificates1(CAUrl, PrivateKey) ->
    %% Read Config
    {ok, Hosts} = get_config_hosts(),

    %% Get a certificate for each host
    PemCertKeys = [get_certificate(CAUrl, Host, PrivateKey) || Host <- Hosts],

    %% Save Certificates
    SavedCerts = [save_certificate(Cert) || Cert <- PemCertKeys],

    %% Format the result to send back to ejabberdctl
    %% Result
    SavedCerts.

-spec get_certificate(url(), bitstring(), jose_jwk:key()) -> 
			     {'ok', bitstring(), pem_certificate()} | 
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
    {ok, Contact} = get_config_contact(),

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
	    ?ERROR_MSG("Error: ~p creating an account for contact", 
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

ensure_account_exists() ->
    case read_account_persistent() of
	none ->
	    ?ERROR_MSG("No existing account", []),
	    throw({error, no_old_account});
	{ok, AccId, PrivateKey} ->
	    {ok, AccId, PrivateKey}
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
    KeyPub = jose_jwk:to_public(Key),
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

%% TODO: I haven't found a function that does that, but there must exist one
length_bitstring(Bitstring) ->
    Size = byte_size(Bitstring),
    case Size =< 127 of
	true ->
	    <<12:8, Size:8, Bitstring/binary>>;
	false ->
	    LenOctets = binary:encode_unsigned(Size),
	    FirstOctet = byte_size(LenOctets),
	    <<12:8, 1:1, FirstOctet:7, Size:(FirstOctet * 8), Bitstring/binary>>
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
	   value = length_bitstring(list_to_bitstring(AttrVal))
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

not_before_not_after() ->
    %% TODO: Make notBefore and notAfter like they do it in other clients
    {MegS, Sec, MicS} = erlang:timestamp(),
    NotBefore = xmpp_util:encode_timestamp({MegS-1, Sec, MicS}),
    NotAfter = xmpp_util:encode_timestamp({MegS+1, Sec, MicS}),
    {NotBefore, NotAfter}.

is_error({error, _}) -> true;
is_error(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Handle the persistent data structure
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_empty() ->
    [].

%%
%% Account
%%

data_get_account(Data) ->
    case lists:keyfind(account, 1, Data) of
	{account, #data_acc{id = AccId, key = PrivateKey}} ->
	    {ok, AccId, PrivateKey};
        false ->
	    none
    end.

data_set_account(Data, {AccId, PrivateKey}) -> 
    NewAcc = {account, #data_acc{id = AccId, key = PrivateKey}},
    lists:keystore(account, 1, Data, NewAcc).

%%
%% Certificates
%% 

data_get_certificates(Data) ->
    case lists:keyfind(certs, 1, Data) of
	{certs, Certs} ->
	    {ok, Certs};
        false ->
	    {ok, []}
    end.

data_set_certificates(Data, NewCerts) -> 
    lists:keystore(certs, 1, Data, {certs, NewCerts}).

%% ATM we preserve one certificate for each domain
data_add_certificate(Data, {Domain, PemCert}) ->
    {ok, Certs} = data_get_certificates(Data),
    NewCerts = lists:keystore(Domain, 1, Certs, {Domain, PemCert}),
    data_set_certificates(Data, NewCerts).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Handle Config and Persistence Files
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

persistent_file() ->
    MnesiaDir = mnesia:system_info(directory),
    filename:join(MnesiaDir, "acme.DAT").

%% The persistent file should be rread and written only by its owner
persistent_file_mode() ->
    8#400 + 8#200. 

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

write_persistent(Data) ->
    Binary = term_to_binary(Data),
    case file:write_file(persistent_file(), Binary) of
	ok -> ok;
	{error, Reason} ->
	    ?ERROR_MSG("Error: ~p writing acme data file", [Reason]),
	    throw({error, Reason})
    end.    

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

write_account_persistent({AccId, PrivateKey}) ->
    {ok, Data} = read_persistent(),
    NewData = data_set_account(Data, {AccId, PrivateKey}),
    ok = write_persistent(NewData).

read_account_persistent() ->
    {ok, Data} = read_persistent(),
    data_get_account(Data).

read_certificates_persistent() ->
    {ok, Data} = read_persistent(),
    data_get_certificates(Data).

add_certificate_persistent({Domain, PemCert}) ->
    {ok, Data} = read_persistent(),
    NewData = data_add_certificate(Data, {Domain, PemCert}),
    ok = write_persistent(NewData).

save_certificate({error, _, _} = Error) ->
    Error;
save_certificate({ok, DomainName, Cert}) ->
    try
	{ok, CertDir} = get_config_cert_dir(),
	DomainString = bitstring_to_list(DomainName),
	CertificateFile = filename:join([CertDir, DomainString ++ "_cert.pem"]),
	%% TODO: At some point do the following using a Transaction so
	%% that there is no certificate saved if it cannot be added in
	%% certificate persistent storage
	write_cert(CertificateFile, Cert, DomainName),
	add_certificate_persistent({DomainName, Cert}),
	{ok, DomainName, saved}
    catch
	throw:Throw ->
	    Throw;
	E:R ->
	    ?ERROR_MSG("Unknown ~p:~p, ~p", [E, R, erlang:get_stacktrace()]), 
	    {error, DomainName, saving}
    end.

write_cert(CertificateFile, Cert, DomainName) ->
    case file:write_file(CertificateFile, Cert) of
	ok ->
	    {ok, DomainName, saved};
	{error, Reason} ->
	    ?ERROR_MSG("Error: ~p saving certificate at file: ~p",
		       [Reason, CertificateFile]),
	    throw({error, DomainName, saving})
    end.

get_config_acme() ->
    case ejabberd_config:get_option(acme, undefined) of
	undefined ->
	    ?ERROR_MSG("No acme configuration has been specified", []),
	    throw({error, configuration});
        Acme ->
	    {ok, Acme}
    end.

get_config_contact() ->
    {ok, Acme} = get_config_acme(),
    case lists:keyfind(contact, 1, Acme) of
	{contact, Contact} ->
	    {ok, Contact};
	false ->
	    ?ERROR_MSG("No contact has been specified", []),
	    throw({error, configuration_contact})
    end.

get_config_hosts() ->
    case ejabberd_config:get_option(hosts, undefined) of
	undefined ->
	    ?ERROR_MSG("No hosts have been specified", []),
	    throw({error, configuration_hosts});
        Hosts ->
	    {ok, Hosts}
    end.

get_config_cert_dir() ->
    case ejabberd_config:get_option(cert_dir, undefined) of
	undefined ->
	    ?ERROR_MSG("No cert_dir configuration has been specified", []),
	    throw({error, configuration});
        CertDir ->
	    {ok, CertDir}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Transaction Fun
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transaction([{Fun, Rollback} | Rest]) ->
    try
        {ok, Result} = Fun(),
        [Result | transaction(Rest)]
    catch Type:Reason ->
        Rollback(),
        erlang:raise(Type, Reason, erlang:get_stacktrace())
    end;
transaction([Fun | Rest]) ->
    % not every action require cleanup on error
    transaction([{Fun, fun () -> ok end} | Rest]);
transaction([]) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Debugging Funcs -- They are only used for the development phase
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% A typical acme workflow
scenario(CAUrl, AccId, PrivateKey) ->
    {ok, Dirs, Nonce0} = ejabberd_acme_comm:directory(CAUrl),

    {ok, {_TOS, Account}, Nonce1} = 
	ejabberd_acme_comm:get_account({CAUrl, AccId}, PrivateKey, Nonce0),
    ?INFO_MSG("Account: ~p~n", [Account]),

    Req =
	[{<<"identifier">>,
	  {[{<<"type">>, <<"dns">>},
	    {<<"value">>, <<"my-acme-test-ejabberd.com">>}]}},
	 {<<"existing">>, <<"accept">>}
	],
    {ok, Authz, Nonce2} = ejabberd_acme_comm:new_authz(Dirs, PrivateKey, Req, Nonce1),

    {Account, Authz, PrivateKey}.


new_user_scenario(CAUrl, HttpDir) ->
    PrivateKey = generate_key(),

    {ok, Dirs, Nonce0} = ejabberd_acme_comm:directory(CAUrl),
    %% ?INFO_MSG("Directories: ~p", [Dirs]),

    Req0 = [{ <<"contact">>, [<<"mailto:cert-example-admin@example2.com">>]}],
    {ok, {TOS, Account}, Nonce1} = ejabberd_acme_comm:new_account(Dirs, PrivateKey, Req0, Nonce0),

    {_, AccIdInt} = proplists:lookup(<<"id">>, Account),
    AccId = integer_to_list(AccIdInt),
    {ok, {_TOS, Account1}, Nonce2} = 
	ejabberd_acme_comm:get_account({CAUrl, AccId}, PrivateKey, Nonce1),
    %% ?INFO_MSG("Old account: ~p~n", [Account1]),

    Req1 = [{ <<"agreement">>, list_to_bitstring(TOS)}],
    {ok, Account2, Nonce3} = 
	ejabberd_acme_comm:update_account({CAUrl, AccId}, PrivateKey, Req1, Nonce2),

    %% NewKey = generate_key(),
    %% KeyChangeUrl = CAUrl ++ "/acme/key-change/",
    %% {ok, Account3, Nonce4} = key_roll_over(KeyChangeUrl, AccURL, PrivateKey, NewKey, Nonce3),
    %% ?INFO_MSG("Changed key: ~p~n", [Account3]),

    %% {ok, {_TOS, Account4}, Nonce5} = get_account(AccURL, NewKey, Nonce4),
    %% ?INFO_MSG("New account:~p~n", [Account4]),
    %% {Account4, PrivateKey}.

    AccIdBin = list_to_bitstring(integer_to_list(AccIdInt)),
    DomainName = << <<"my-acme-test-ejabberd">>/binary, AccIdBin/binary, <<".com">>/binary >>,
    Req2 =
	[{<<"identifier">>,
	  {[{<<"type">>, <<"dns">>},
	    {<<"value">>, DomainName}]}},
	 {<<"existing">>, <<"accept">>}
	],
    {ok, {AuthzUrl, Authz}, Nonce4} = 
	ejabberd_acme_comm:new_authz(Dirs, PrivateKey, Req2, Nonce3),

    {ok, AuthzId} = location_to_id(AuthzUrl),
    {ok, Authz2, Nonce5} = ejabberd_acme_comm:get_authz({CAUrl, AuthzId}),
    ?INFO_MSG("AuthzUrl: ~p~n", [AuthzUrl]),

    Challenges = get_challenges(Authz2),
    ?INFO_MSG("Challenges: ~p~n", [Challenges]),

    {ok, ChallengeUrl, KeyAuthz} =
	acme_challenge:solve_challenge(<<"http-01">>, Challenges, {PrivateKey, HttpDir}),
    ?INFO_MSG("File for http-01 challenge written correctly", []),

    {ok, ChallengeId} = location_to_id(ChallengeUrl),
    Req3 =
	[ {<<"type">>, <<"http-01">>}
	, {<<"keyAuthorization">>, KeyAuthz}
	],
    {ok, SolvedChallenge, Nonce6} = ejabberd_acme_comm:complete_challenge(
				      {CAUrl, AuthzId, ChallengeId}, PrivateKey, Req3, Nonce5),
    %% ?INFO_MSG("SolvedChallenge: ~p~n", [SolvedChallenge]),

    %% timer:sleep(2000),
    {ok, Authz3, Nonce7} = ejabberd_acme_comm:get_authz_until_valid({CAUrl, AuthzId}),

    #{"new-cert" := NewCert} = Dirs,
    CSRSubject = [{commonName, bitstring_to_list(DomainName)}],
    {CSR, CSRKey} = make_csr(CSRSubject),
    {MegS, Sec, MicS} = erlang:timestamp(),
    NotBefore = xmpp_util:encode_timestamp({MegS-1, Sec, MicS}),
    NotAfter = xmpp_util:encode_timestamp({MegS+1, Sec, MicS}),
    Req4 =
	[{<<"csr">>, CSR},
	 {<<"notBefore">>, NotBefore},
	 {<<"NotAfter">>, NotAfter}
	],
    {ok, {CertUrl, Certificate}, Nonce8} = 
	ejabberd_acme_comm:new_cert(Dirs, PrivateKey, Req4, Nonce7),
    ?INFO_MSG("CertUrl: ~p~n", [CertUrl]),

    {ok, CertId} = location_to_id(CertUrl),
    {ok, Certificate2, Nonce9} = ejabberd_acme_comm:get_cert({CAUrl, CertId}),

    DecodedCert = public_key:pkix_decode_cert(list_to_binary(Certificate2), plain),
    %% ?INFO_MSG("DecodedCert: ~p~n", [DecodedCert]),
    PemEntryCert = public_key:pem_entry_encode('Certificate', DecodedCert),
    %% ?INFO_MSG("PemEntryCert: ~p~n", [PemEntryCert]),

    {_, CSRKeyKey} = jose_jwk:to_key(CSRKey),
    PemEntryKey = public_key:pem_entry_encode('ECPrivateKey', CSRKeyKey),
    %% ?INFO_MSG("PemKey: ~p~n", [jose_jwk:to_pem(CSRKey)]),
    %% ?INFO_MSG("PemEntryKey: ~p~n", [PemEntryKey]),

    PemCert = public_key:pem_encode([PemEntryKey, PemEntryCert]),
    %% ?INFO_MSG("PemCert: ~p~n", [PemCert]),

    ok = file:write_file(HttpDir ++ "/my_server.pem", PemCert),

    Base64Cert = base64url:encode(Certificate2),
    Req5 = [{<<"certificate">>, Base64Cert}],
    {ok, [], Nonce10} = ejabberd_acme_comm:revoke_cert(Dirs, PrivateKey, Req5, Nonce9),

    {ok, Certificate3, Nonce11} = ejabberd_acme_comm:get_cert({CAUrl, CertId}),

    {Account2, Authz3, CSR, Certificate, PrivateKey}.


generate_key() ->
    jose_jwk:generate_key({ec, secp256r1}).

scenario3() ->
    CSRSubject = [{commonName, "my-acme-test-ejabberd.com"},
		  {organizationName, "Example Corp"}],
    {CSR, CSRKey} = make_csr(CSRSubject).


%% It doesn't seem to work, The user can get a new authorization even though the account has been deleted
delete_account_scenario(CAUrl) ->
    PrivateKey = generate_key(),

    DirURL = CAUrl ++ "/directory",
    {ok, Dirs, Nonce0} = ejabberd_acme_comm:directory(DirURL),
    %% ?INFO_MSG("Directories: ~p", [Dirs]),

    Req0 = [{ <<"contact">>, [<<"mailto:cert-example-admin@example2.com">>]}],
    {ok, {TOS, Account}, Nonce1} = ejabberd_acme_comm:new_account(Dirs, PrivateKey, Req0, Nonce0),

    {_, AccIdInt} = proplists:lookup(<<"id">>, Account),
    AccId = integer_to_list(AccIdInt),
    {ok, {_TOS, Account1}, Nonce2} = 
	ejabberd_acme_comm:get_account({CAUrl, AccId}, PrivateKey, Nonce1),
    %% ?INFO_MSG("Old account: ~p~n", [Account1]),

    Req1 = [{ <<"agreement">>, list_to_bitstring(TOS)}],
    {ok, Account2, Nonce3} = 
	ejabberd_acme_comm:update_account({CAUrl, AccId}, PrivateKey, Req1, Nonce2),

    %% Delete account
    {ok, Account3, Nonce4} = 
	ejabberd_acme_comm:delete_account({CAUrl, AccId}, PrivateKey, Nonce3),

    timer:sleep(3000),

    {ok, {_TOS, Account4}, Nonce5} = 
	ejabberd_acme_comm:get_account({CAUrl, AccId}, PrivateKey, Nonce4),
    ?INFO_MSG("New account: ~p~n", [Account4]),

    AccIdBin = list_to_bitstring(integer_to_list(AccIdInt)),
    DomainName = << <<"my-acme-test-ejabberd">>/binary, AccIdBin/binary, <<".com">>/binary >>,
    Req2 =
        [{<<"identifier">>,
          {[{<<"type">>, <<"dns">>},
            {<<"value">>, DomainName}]}},
         {<<"existing">>, <<"accept">>}
        ],
    {ok, {AuthzUrl, Authz}, Nonce6} = 
	ejabberd_acme_comm:new_authz(Dirs, PrivateKey, Req2, Nonce5),

    {ok, Account1, Account3, Authz}.

%% Just a test
scenario0(KeyFile, HttpDir) ->
    PrivateKey = jose_jwk:from_file(KeyFile),
    %% scenario("http://localhost:4000", "2", PrivateKey).
    %% delete_account_scenario("http://localhost:4000").
    new_user_scenario("http://localhost:4000", HttpDir).

%% scenario3().

