-module (ejabberd_acme).

-export([%% Directory
	 directory/1,
         %% Account
	 new_account/4,
	 update_account/4,
	 get_account/3,
	 delete_account/3,
         %% Authorization
	 new_authz/4,
	 get_authz/1,
	 complete_challenge/4,
         %% Certificate
	 new_cert/4,
         get_cert/1,
         revoke_cert/4,
         %% Ejabberdctl Commands
	 get_certificates/3,
	 %% Command Options Validity
	 is_valid_account_opt/1,
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

-define(REQUEST_TIMEOUT, 5000). % 5 seconds.
-define(MAX_POLL_REQUESTS, 20).
-define(POLL_WAIT_TIME, 500). % 500 ms.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Directory
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec directory(url()) -> {ok, dirs(), nonce()} | {error, _}.
directory(CAUrl) ->
    Url = CAUrl ++ "/directory",
    prepare_get_request(Url, fun get_dirs/1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Account Handling
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new_account(dirs(), jose_jwk:key(), proplist(), nonce()) ->
			 {ok, {url(), proplist()}, nonce()} | {error, _}.
new_account(Dirs, PrivateKey, Req, Nonce) ->
    #{"new-reg" := Url} = Dirs,
    EJson = {[{ <<"resource">>, <<"new-reg">>}] ++ Req},
    prepare_post_request(Url, PrivateKey, EJson, Nonce, fun get_response_tos/1).

-spec update_account({url(), string()}, jose_jwk:key(), proplist(), nonce()) ->
			    {ok, proplist(), nonce()} | {error, _}.
update_account({CAUrl, AccId}, PrivateKey, Req, Nonce) ->
    Url = CAUrl ++ "/acme/reg/" ++ AccId,
    EJson = {[{ <<"resource">>, <<"reg">>}] ++ Req},
    prepare_post_request(Url, PrivateKey, EJson, Nonce, fun get_response/1).

-spec get_account({url(), string()}, jose_jwk:key(), nonce()) ->
			 {ok, {url(), proplist()}, nonce()} | {error, _}.
get_account({CAUrl, AccId}, PrivateKey, Nonce) ->
    Url = CAUrl ++ "/acme/reg/" ++ AccId,
    EJson = {[{<<"resource">>, <<"reg">>}]},
    prepare_post_request(Url, PrivateKey, EJson, Nonce, fun get_response_tos/1).

-spec delete_account({url(), string()}, jose_jwk:key(), nonce()) ->
			    {ok, proplist(), nonce()} | {error, _}.
delete_account({CAUrl, AccId}, PrivateKey, Nonce) ->
    Url = CAUrl ++ "/acme/reg/" ++ AccId,
    EJson =
	{[{<<"resource">>, <<"reg">>},
	  {<<"status">>, <<"deactivated">>}]},
    prepare_post_request(Url, PrivateKey, EJson, Nonce, fun get_response/1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Authorization Handling
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new_authz(dirs(), jose_jwk:key(), proplist(), nonce()) ->
		       {ok, {url(), proplist()}, nonce()} | {error, _}.
new_authz(Dirs, PrivateKey, Req, Nonce) ->
    #{"new-authz" := Url} = Dirs,
    EJson = {[{<<"resource">>, <<"new-authz">>}] ++ Req},
    prepare_post_request(Url, PrivateKey, EJson, Nonce, fun get_response_location/1).

-spec get_authz({url(), string()}) -> {ok, proplist(), nonce()} | {error, _}.
get_authz({CAUrl, AuthzId}) ->
    Url = CAUrl ++ "/acme/authz/" ++ AuthzId,
    prepare_get_request(Url, fun get_response/1).

-spec complete_challenge({url(), string(), string()}, jose_jwk:key(), proplist(), nonce()) ->
				{ok, proplist(), nonce()} | {error, _}.
complete_challenge({CAUrl, AuthzId, ChallId}, PrivateKey, Req, Nonce) ->
    Url = CAUrl ++ "/acme/challenge/" ++ AuthzId ++ "/" ++ ChallId,
    EJson = {[{<<"resource">>, <<"challenge">>}] ++ Req},
    prepare_post_request(Url, PrivateKey, EJson, Nonce, fun get_response/1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Certificate Handling
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new_cert(dirs(), jose_jwk:key(), proplist(), nonce()) ->
		      {ok, {url(), list()}, nonce()} | {error, _}.
new_cert(Dirs, PrivateKey, Req, Nonce) ->
    #{"new-cert" := Url} = Dirs,
    EJson = {[{<<"resource">>, <<"new-cert">>}] ++ Req},
    prepare_post_request(Url, PrivateKey, EJson, Nonce, fun get_response_location/1,
			 "application/pkix-cert").

-spec get_cert({url(), string()}) -> {ok, list(), nonce()} | {error, _}.
get_cert({CAUrl, CertId}) ->
    Url = CAUrl ++ "/acme/cert/" ++ CertId,
    prepare_get_request(Url, fun get_response/1, "application/pkix-cert").

-spec revoke_cert(dirs(), jose_jwk:key(), proplist(), nonce()) ->
			 {ok, _, nonce()} | {error, _}.
revoke_cert(Dirs, PrivateKey, Req, Nonce) ->
    #{"revoke-cert" := Url} = Dirs,
    EJson = {[{<<"resource">>, <<"revoke-cert">>}] ++ Req},
    prepare_post_request(Url, PrivateKey, EJson, Nonce, fun get_response/1,
                         "application/pkix-cert").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Handle Response Functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_dirs({ok, proplist(), proplist()}) -> {ok, map(), nonce()}.
get_dirs({ok, Head, Return}) ->
    NewNonce = get_nonce(Head),
    StrDirectories = [{bitstring_to_list(X), bitstring_to_list(Y)} ||
			 {X, Y} <- Return],
    NewDirs = maps:from_list(StrDirectories),
    {ok, NewDirs, NewNonce}.

-spec get_response({ok, proplist(), proplist()}) -> {ok, proplist(), nonce()}.
get_response({ok, Head, Return}) ->
    NewNonce = get_nonce(Head),
    {ok, Return, NewNonce}.

-spec get_response_tos({ok, proplist(), proplist()}) -> {ok, {url(), proplist()}, nonce()}.
get_response_tos({ok, Head, Return}) ->
    TOSUrl = get_tos(Head),
    NewNonce = get_nonce(Head),
    {ok, {TOSUrl, Return}, NewNonce}.

-spec get_response_location({ok, proplist(), proplist()}) -> {ok, {url(), proplist()}, nonce()}.
get_response_location({ok, Head, Return}) ->
    Location = get_location(Head),
    NewNonce = get_nonce(Head),
    {ok, {Location, Return}, NewNonce}.


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
%% TODO: Encode Strings using length.

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
%% Authorization Polling
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_authz_until_valid({url(), string()}) -> {ok, proplist(), nonce()} | {error, _}.
get_authz_until_valid({CAUrl, AuthzId}) ->
    get_authz_until_valid({CAUrl, AuthzId}, ?MAX_POLL_REQUESTS).

-spec get_authz_until_valid({url(), string()}, non_neg_integer()) ->
				   {ok, proplist(), nonce()} | {error, _}.
get_authz_until_valid({_CAUrl, _AuthzId}, 0) ->
    ?ERROR_MSG("Maximum request limit waiting for validation reached", []),
    {error, max_request_limit};
get_authz_until_valid({CAUrl, AuthzId}, N) ->
    case get_authz({CAUrl, AuthzId}) of
	{ok, Resp, Nonce} ->
	    case is_authz_valid(Resp) of
		true ->
		    {ok, Resp, Nonce};
		false ->
		    timer:sleep(?POLL_WAIT_TIME),
		    get_authz_until_valid({CAUrl, AuthzId}, N-1)
	    end;
	{error, _} = Err ->
	    Err
    end.

-spec is_authz_valid(proplist()) -> boolean().
is_authz_valid(Authz) ->
    case proplists:lookup(<<"status">>, Authz) of
	{<<"status">>, <<"valid">>} ->
	    true;
	_ ->
	    false
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Request Functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TODO: Fix the duplicated code at the below 4 functions
-spec make_post_request(url(), bitstring(), string()) ->
			       {ok, proplist(), proplist()} | {error, _}.
make_post_request(Url, ReqBody, ResponseType) ->
    Options = [],
    HttpOptions = [{timeout, ?REQUEST_TIMEOUT}],
    case httpc:request(post,
		       {Url, [], "application/jose+json", ReqBody}, HttpOptions, Options) of
	{ok, {{_, Code, _}, Head, Body}} when Code >= 200, Code =< 299 ->
	    decode_response(Head, Body, ResponseType);
	Error ->
	    failed_http_request(Error, Url)
    end.

-spec make_get_request(url(), string()) ->
			      {ok, proplist(), proplist()} | {error, _}.
make_get_request(Url, ResponseType) ->
    Options = [],
    HttpOptions = [{timeout, ?REQUEST_TIMEOUT}],
    case httpc:request(get, {Url, []}, HttpOptions, Options) of
	{ok, {{_, Code, _}, Head, Body}} when Code >= 200, Code =< 299 ->
	    decode_response(Head, Body, ResponseType);
	Error ->
	    failed_http_request(Error, Url)
    end.

-spec prepare_post_request(url(), jose_jwk:key(), jiffy:json_value(),
			   nonce(), handle_resp_fun()) -> {ok, _, nonce()} | {error, _}.
prepare_post_request(Url, PrivateKey, EJson, Nonce, HandleRespFun) ->
    prepare_post_request(Url, PrivateKey, EJson, Nonce, HandleRespFun, "application/jose+json").

-spec prepare_post_request(url(), jose_jwk:key(), jiffy:json_value(),
			   nonce(), handle_resp_fun(), string()) -> {ok, _, nonce()} | {error, _}.
prepare_post_request(Url, PrivateKey, EJson, Nonce, HandleRespFun, ResponseType) ->
    case encode(EJson) of
	{ok, ReqBody} ->
	    FinalBody = sign_encode_json_jose(PrivateKey, ReqBody, Nonce),
	    case make_post_request(Url, FinalBody, ResponseType) of
		{ok, Head, Return} ->
		    HandleRespFun({ok, Head, Return});
		Error ->
		    Error
	    end;
	{error, Reason} ->
	    ?ERROR_MSG("Error: ~p when encoding: ~p", [Reason, EJson]),
	    {error, Reason}
    end.

-spec prepare_get_request(url(), handle_resp_fun()) ->
				 {ok, _, nonce()} | {error, _}.
prepare_get_request(Url, HandleRespFun) ->
    prepare_get_request(Url, HandleRespFun, "application/jose+json").

-spec prepare_get_request(url(), handle_resp_fun(), string()) ->
				 {ok, _, nonce()} | {error, _}.
prepare_get_request(Url, HandleRespFun, ResponseType) ->
    case make_get_request(Url, ResponseType) of
	{ok, Head, Return} ->
	    HandleRespFun({ok, Head, Return});
	Error ->
	    Error
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Jose Json Functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sign_json_jose(jose_jwk:key(), bitstring(), nonce()) -> {_, jws()}.
sign_json_jose(Key, Json, Nonce) ->
    PubKey = jose_jwk:to_public(Key),
    {_, BinaryPubKey} = jose_jwk:to_binary(PubKey),
    PubKeyJson = jiffy:decode(BinaryPubKey),
    %% TODO: Ensure this works for all cases
    AlgMap = jose_jwk:signer(Key),
    JwsMap =
	#{ <<"jwk">> => PubKeyJson,
	   %% <<"b64">> => true,
	   <<"nonce">> => list_to_bitstring(Nonce)
	 },
    JwsObj0 = maps:merge(JwsMap, AlgMap),
    JwsObj = jose_jws:from(JwsObj0),
    jose_jws:sign(Key, Json, JwsObj).

-spec sign_encode_json_jose(jose_jwk:key(), bitstring(), nonce()) -> bitstring().
sign_encode_json_jose(Key, Json, Nonce) ->
    {_, Signed} = sign_json_jose(Key, Json, Nonce),
    %% This depends on jose library, so we can consider it safe
    jiffy:encode(Signed).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Useful funs
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_nonce(proplist()) -> nonce() | 'none'.
get_nonce(Head) ->
    case proplists:lookup("replay-nonce", Head) of
	{"replay-nonce", Nonce} -> Nonce;
	none -> none
    end.

-spec get_location(proplist()) -> url() | 'none'.
get_location(Head) ->
    case proplists:lookup("location", Head) of
	{"location", Location} -> Location;
	none -> none
    end.

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

%% Very bad way to extract this
%% TODO: Find a better way
-spec get_tos(proplist()) -> url() | 'none'.
get_tos(Head) ->
    try
	[{_, Link}] = [{K, V} || {K, V} <- Head,
				 K =:= "link" andalso
				     lists:suffix("\"terms-of-service\"", V)],
	[Link1, _] = string:tokens(Link, ";"),
	Link2 = string:strip(Link1, left, $<),
	string:strip(Link2, right, $>)
    catch
	_:_ ->
	    none
    end.

-spec get_challenges(proplist()) -> [{proplist()}].
get_challenges(Body) ->
    {<<"challenges">>, Challenges} = proplists:lookup(<<"challenges">>, Body),
    Challenges.

decode_response(Head, Body, "application/pkix-cert") ->
    {ok, Head, Body};
decode_response(Head, Body, "application/jose+json") ->
    case decode(Body) of
	{ok, Return} ->
	    {ok, Head, Return};
	{error, Reason} ->
	    ?ERROR_MSG("Problem decoding: ~s", [Body]),
	    {error, Reason}
    end.

encode(EJson) ->
    try
	{ok, jiffy:encode(EJson)}
    catch
	_:Reason ->
	    {error, Reason}
    end.

decode(Json) ->
    try
	{Result} = jiffy:decode(Json),
	{ok, Result}
    catch
	_:Reason ->
	    {error, Reason}
    end.

is_error({error, _}) -> true;
is_error(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Handle Failed HTTP Requests
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec failed_http_request({ok, _} | {error, _}, url()) -> {error, _}.
failed_http_request({ok, {{_, Code, _}, _Head, Body}}, Url) ->
    ?ERROR_MSG("Got unexpected status code from <~s>: ~B, Body: ~s",
	       [Url, Code, Body]),
    {error, unexpected_code};
failed_http_request({error, Reason}, Url) ->
    ?ERROR_MSG("Error making a request to <~s>: ~p",
	       [Url, Reason]),
    {error, Reason}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Handle Config and Persistence Files
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

persistent_file() ->
    MnesiaDir = mnesia:system_info(directory),
    filename:join(MnesiaDir, "acme.DAT").

read_persistent() ->
    case file:read_file(persistent_file()) of
	{ok, Binary} ->
	    {ok, binary_to_term(Binary)};
	{error, enoent} ->
	    {ok, #data{}};
	{error, Reason} ->
	    ?ERROR_MSG("Error: ~p reading acme data file", [Reason]),
	    {error, Reason}
    end.

write_persistent(Data) ->
    Binary = term_to_binary(Data),
    case file:write_file(persistent_file(), Binary) of
	ok -> ok;
	{error, Reason} ->
	    ?ERROR_MSG("Error: ~p writing acme data file", [Reason]),
	    {error, Reason}
    end.    

get_account_persistent(#data{account = Account}) ->
    case Account of
	#data_acc{id = AccId, key = PrivateKey} ->
	    {ok, AccId, PrivateKey};
	none ->
	    none
    end.

set_account_persistent(Data = #data{}, {AccId, PrivateKey}) -> 
    NewAcc = #data_acc{id = AccId, key = PrivateKey},
    Data#data{account = NewAcc}.

get_config_contact() ->
    case ejabberd_config:get_option(acme, undefined) of
	undefined ->
	    ?ERROR_MSG("No acme configuration has been specified", []),
	    {error, configuration};
        Acme ->
	    case lists:keyfind(contact, 1, Acme) of
		{contact, Contact} ->
		    {ok, Contact};
		false ->
		    ?ERROR_MSG("No contact has been specified", []),
		    {error, configuration_contact}
	    end
    end.

get_config_hosts() ->
    case ejabberd_config:get_option(hosts, undefined) of
	undefined ->
	    ?ERROR_MSG("No hosts have been specified", []),
	    {error, configuration_hosts};
        Hosts ->
	    {ok, Hosts}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Command Functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Check Validity of command options
%%

is_valid_account_opt("old-account") -> true;
is_valid_account_opt("new-account") -> true;
is_valid_account_opt(_) -> false.

%%
%% Get Certificate
%%

%% Needs a hell lot of cleaning
get_certificates(CAUrl, HttpDir, NewAccountOpt) ->
    try
	get_certificates0(CAUrl, HttpDir, NewAccountOpt)
    catch
	E:R ->
	    {E,R}
    end.

get_certificates0(CAUrl, HttpDir, "old-account") ->
    %% Read Persistent Data
    {ok, Data} = read_persistent(),

    %% Get the current account
    case get_account_persistent(Data) of
	none ->
	    ?ERROR_MSG("No existing account", []),
	    {error, no_old_account};
	{ok, _AccId, PrivateKey} ->
	    get_certificates1(CAUrl, HttpDir, PrivateKey)
    end;
get_certificates0(CAUrl, HttpDir, "new-account") ->
    %% Get contact from configuration file
    {ok, Contact} = get_config_contact(),

    %% Generate a Key
    PrivateKey = generate_key(),

    %% Create a new account
    {ok, _Id} = create_new_account(CAUrl, Contact, Key),

    %% Write Persistent Data
    {ok, Data} = read_persistent(),
    NewData = set_account_persistent(Data, {Id, Key}),
    ok = write_persistent(NewData),

    get_certificates1(CAUrl, HttpDir, PrivateKey).


get_certificates1(CAUrl, HttpDir, PrivateKey) ->
    %% Read Config
    {ok, Hosts} = get_config_hosts(),

    %% Get a certificate for each host
    PemCertKeys = [get_certificate(CAUrl, Host, PrivateKey, HttpDir) || Host <- Hosts],
    {AccId, PrivateKey, PemCertKeys}.


get_certificate(CAUrl, DomainName, PrivateKey, HttpDir) ->
    ?INFO_MSG("Getting a Certificate for domain: ~p~n", [DomainName]),
    case create_new_authorization(CAUrl, DomainName, PrivateKey, HttpDir) of
	{ok, Authz} ->
	    create_new_certificate(CAUrl, DomainName, PrivateKey);
	{error, authorization} ->
	    {error, {authorization, {host, DomainName}}}
    end.

%% TODO:
%% Find a way to ask the user if he accepts the TOS
create_new_account(CAUrl, Contact, PrivateKey) ->
    try
	{ok, Dirs, Nonce0} = directory(CAUrl),
	Req0 = [{ <<"contact">>, [Contact]}],
	{ok, {TOS, Account}, Nonce1} = new_account(Dirs, PrivateKey, Req0, Nonce0),
	{<<"id">>, AccIdInt} = lists:keyfind(<<"id">>, 1, Account),
	AccId = integer_to_list(AccIdInt),
	Req1 = [{ <<"agreement">>, list_to_bitstring(TOS)}],
	{ok, Account2, _Nonce2} = update_account({CAUrl, AccId}, PrivateKey, Req1, Nonce1),
	{ok, AccId}
    catch
	E:R ->
	    {error,create_new_account}
    end.


create_new_authorization(CAUrl, DomainName, PrivateKey, HttpDir) ->
    try
	{ok, Dirs, Nonce0} = directory(CAUrl),
	Req0 = [{<<"identifier">>,
		 {[{<<"type">>, <<"dns">>},
		   {<<"value">>, DomainName}]}},
		{<<"existing">>, <<"accept">>}],
	{ok, {AuthzUrl, Authz}, Nonce1} = new_authz(Dirs, PrivateKey, Req0, Nonce0),
	{ok, AuthzId} = location_to_id(AuthzUrl),

	Challenges = get_challenges(Authz),
	{ok, ChallengeUrl, KeyAuthz} =
	    acme_challenge:solve_challenge(<<"http-01">>, Challenges, {PrivateKey, HttpDir}),
	{ok, ChallengeId} = location_to_id(ChallengeUrl),
	Req3 = [{<<"type">>, <<"http-01">>},{<<"keyAuthorization">>, KeyAuthz}],
	{ok, SolvedChallenge, Nonce2} = 
	    complete_challenge({CAUrl, AuthzId, ChallengeId}, PrivateKey, Req3, Nonce1),

	{ok, AuthzValid, _Nonce} = get_authz_until_valid({CAUrl, AuthzId}),
	{ok, AuthzValid}
    catch
	E:R ->
	    ?ERROR_MSG("Error: ~p getting an authorization for domain: ~p~n",
		       [{E,R}, DomainName]),
	    {error, authorization}
    end.

create_new_certificate(CAUrl, DomainName, PrivateKey) ->
    try
	{ok, Dirs, Nonce0} = directory(CAUrl),
	CSRSubject = [{commonName, bitstring_to_list(DomainName)}],
	{CSR, CSRKey} = make_csr(CSRSubject),
	{NotBefore, NotAfter} = not_before_not_after(),
	Req =
	    [{<<"csr">>, CSR},
	     {<<"notBefore">>, NotBefore},
	     {<<"NotAfter">>, NotAfter}
	    ],
	{ok, {CertUrl, Certificate}, Nonce1} = new_cert(Dirs, PrivateKey, Req, Nonce0),

	{ok, CertId} = location_to_id(CertUrl),

	DecodedCert = public_key:pkix_decode_cert(list_to_binary(Certificate), plain),	
	PemEntryCert = public_key:pem_entry_encode('Certificate', DecodedCert),

	{_, CSRKeyKey} = jose_jwk:to_key(CSRKey),
	PemEntryKey = public_key:pem_entry_encode('ECPrivateKey', CSRKeyKey),

	PemCertKey = public_key:pem_encode([PemEntryKey, PemEntryCert]),

	{ok, PemCertKey}
    catch		     
	E:R ->
	    ?ERROR_MSG("Error: ~p getting an authorization for domain: ~p~n",
		       [{E,R}, DomainName]),
	    {error, certificate}
    end.
not_before_not_after() ->
    %% TODO: Make notBefore and notAfter like they do it in other clients
    {MegS, Sec, MicS} = erlang:timestamp(),
    NotBefore = xmpp_util:encode_timestamp({MegS-1, Sec, MicS}),
    NotAfter = xmpp_util:encode_timestamp({MegS+1, Sec, MicS}),
    {NotBefore, NotAfter}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Debugging Funcs -- They are only used for the development phase
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% A typical acme workflow
scenario(CAUrl, AccId, PrivateKey) ->
    {ok, Dirs, Nonce0} = directory(CAUrl),

    {ok, {_TOS, Account}, Nonce1} = get_account({CAUrl, AccId}, PrivateKey, Nonce0),
    ?INFO_MSG("Account: ~p~n", [Account]),

    Req =
	[{<<"identifier">>,
	  {[{<<"type">>, <<"dns">>},
	    {<<"value">>, <<"my-acme-test-ejabberd.com">>}]}},
	 {<<"existing">>, <<"accept">>}
	],
    {ok, Authz, Nonce2} = new_authz(Dirs, PrivateKey, Req, Nonce1),

    {Account, Authz, PrivateKey}.


new_user_scenario(CAUrl, HttpDir) ->
    PrivateKey = generate_key(),

    {ok, Dirs, Nonce0} = directory(CAUrl),
    %% ?INFO_MSG("Directories: ~p", [Dirs]),

    Req0 = [{ <<"contact">>, [<<"mailto:cert-example-admin@example2.com">>]}],
    {ok, {TOS, Account}, Nonce1} = new_account(Dirs, PrivateKey, Req0, Nonce0),

    {_, AccIdInt} = proplists:lookup(<<"id">>, Account),
    AccId = integer_to_list(AccIdInt),
    {ok, {_TOS, Account1}, Nonce2} = get_account({CAUrl, AccId}, PrivateKey, Nonce1),
    %% ?INFO_MSG("Old account: ~p~n", [Account1]),

    Req1 = [{ <<"agreement">>, list_to_bitstring(TOS)}],
    {ok, Account2, Nonce3} = update_account({CAUrl, AccId}, PrivateKey, Req1, Nonce2),

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
    {ok, {AuthzUrl, Authz}, Nonce4} = new_authz(Dirs, PrivateKey, Req2, Nonce3),

    {ok, AuthzId} = location_to_id(AuthzUrl),
    {ok, Authz2, Nonce5} = get_authz({CAUrl, AuthzId}),
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
    {ok, SolvedChallenge, Nonce6} = complete_challenge({CAUrl, AuthzId, ChallengeId}, PrivateKey, Req3, Nonce5),
    %% ?INFO_MSG("SolvedChallenge: ~p~n", [SolvedChallenge]),

    %% timer:sleep(2000),
    {ok, Authz3, Nonce7} = get_authz_until_valid({CAUrl, AuthzId}),

    #{"new-cert" := NewCert} = Dirs,
    CSRSubject = [{commonName, bitstring_to_list(DomainName)},
		  {organizationName, "Example Corp"}],
    {CSR, CSRKey} = make_csr(CSRSubject),
    {MegS, Sec, MicS} = erlang:timestamp(),
    NotBefore = xmpp_util:encode_timestamp({MegS-1, Sec, MicS}),
    NotAfter = xmpp_util:encode_timestamp({MegS+1, Sec, MicS}),
    Req4 =
	[{<<"csr">>, CSR},
	 {<<"notBefore">>, NotBefore},
	 {<<"NotAfter">>, NotAfter}
	],
    {ok, {CertUrl, Certificate}, Nonce8} = new_cert(Dirs, PrivateKey, Req4, Nonce7),
    ?INFO_MSG("CertUrl: ~p~n", [CertUrl]),

    {ok, CertId} = location_to_id(CertUrl),
    {ok, Certificate2, Nonce9} = get_cert({CAUrl, CertId}),

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
    {ok, [], Nonce10} = revoke_cert(Dirs, PrivateKey, Req5, Nonce9),

    {ok, Certificate3, Nonce11} = get_cert(CertUrl),

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
    {ok, Dirs, Nonce0} = directory(DirURL),
    %% ?INFO_MSG("Directories: ~p", [Dirs]),

    Req0 = [{ <<"contact">>, [<<"mailto:cert-example-admin@example2.com">>]}],
    {ok, {TOS, Account}, Nonce1} = new_account(Dirs, PrivateKey, Req0, Nonce0),

    {_, AccIdInt} = proplists:lookup(<<"id">>, Account),
    AccId = integer_to_list(AccIdInt),
    {ok, {_TOS, Account1}, Nonce2} = get_account({CAUrl, AccId}, PrivateKey, Nonce1),
    %% ?INFO_MSG("Old account: ~p~n", [Account1]),

    Req1 = [{ <<"agreement">>, list_to_bitstring(TOS)}],
    {ok, Account2, Nonce3} = update_account({CAUrl, AccId}, PrivateKey, Req1, Nonce2),

    %% Delete account
    {ok, Account3, Nonce4} = delete_account({CAUrl, AccId}, PrivateKey, Nonce3),

    timer:sleep(3000),

    {ok, {_TOS, Account4}, Nonce5} = get_account({CAUrl, AccId}, PrivateKey, Nonce4),
    ?INFO_MSG("New account: ~p~n", [Account4]),

    AccIdBin = list_to_bitstring(integer_to_list(AccIdInt)),
    DomainName = << <<"my-acme-test-ejabberd">>/binary, AccIdBin/binary, <<".com">>/binary >>,
    Req2 =
        [{<<"identifier">>,
          {[{<<"type">>, <<"dns">>},
            {<<"value">>, DomainName}]}},
         {<<"existing">>, <<"accept">>}
        ],
    {ok, {AuthzUrl, Authz}, Nonce6} = new_authz(Dirs, PrivateKey, Req2, Nonce5),

    {ok, Account1, Account3, Authz}.

%% Just a test
scenario0(KeyFile, HttpDir) ->
    PrivateKey = jose_jwk:from_file(KeyFile),
    %% scenario("http://localhost:4000", "2", PrivateKey).
    %% delete_account_scenario("http://localhost:4000").
    new_user_scenario("http://localhost:4000", HttpDir).

%% scenario3().

