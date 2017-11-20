-module(ejabberd_acme_comm).
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
         %% Authorization polling
	 get_authz_until_valid/1,
	 %% Certificate
	 new_cert/4,
         get_cert/1,
         revoke_cert/4,
	 get_issuer_cert/1
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

%%%
%%% This module contains functions that implement all necessary http
%%% requests to the ACME Certificate Authority. Its purpose is to
%%% facilitate the acme client implementation by separating the
%%% handling/validating/parsing of all the needed http requests.
%%%

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
    prepare_post_request(Url, PrivateKey, EJson, Nonce, fun get_response_link_up/1,
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

-spec get_issuer_cert(url()) -> {ok, list(), nonce()} | {error, _}.
get_issuer_cert(IssuerCertUrl) ->
    prepare_get_request(IssuerCertUrl, fun get_response/1, "application/pkix-cert").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Handle Response Functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_dirs({ok, proplist(), proplist()}) -> {ok, map(), nonce()}.
get_dirs({ok, Head, Return}) ->
    NewNonce = get_nonce(Head),
    StrDirectories = [{bitstring_to_list(X), bitstring_to_list(Y)} ||
			 {X, Y} <- Return, is_bitstring(X) andalso is_bitstring(Y)],
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

-spec get_response_link_up({ok, proplist(), proplist()}) -> {ok, {url(), proplist()}, nonce()}.
get_response_link_up({ok, Head, Return}) ->
    LinkUp = get_link_up(Head),
    NewNonce = get_nonce(Head),
    {ok, {LinkUp, Return}, NewNonce}.

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
    PubKey = ejabberd_acme:to_public(Key),
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

-spec get_tos(proplist()) -> url() | 'none'.
get_tos(Head) ->
    get_header_link(Head, "\"terms-of-service\"").

-spec get_link_up(proplist()) -> url() | 'none'.
get_link_up(Head) ->
    get_header_link(Head, "rel=\"up\"").

%% TODO: Find a more reliable way to extract this
-spec get_header_link(proplist(), string()) -> url() | 'none'.
get_header_link(Head, Suffix) ->
    try
	[{_, Link}] = [{K, V} || {K, V} <- Head,
				 K =:= "link" andalso
				     lists:suffix(Suffix, V)],
	[Link1, _] = string:tokens(Link, ";"),
	Link2 = string:strip(Link1, left, $<),
	string:strip(Link2, right, $>)
    catch
	_:_ ->
	    none
    end.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Handle Failed HTTP Requests
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec failed_http_request({ok, _} | {error, _}, url()) -> {error, _}.
failed_http_request({ok, {{_, Code, Reason}, _Head, Body}}, Url) ->
    ?ERROR_MSG("Got unexpected status code from <~s>: ~B, Body: ~s",
	       [Url, Code, Body]),
    throw({error, {unexpected_code, Code, Reason}});
failed_http_request({error, Reason}, Url) ->
    ?ERROR_MSG("Error making a request to <~s>: ~p",
	       [Url, Reason]),
    throw({error, Reason}).
