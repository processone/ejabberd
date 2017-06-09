-module (ejabberd_acme).

-export([ scenario/3
        , scenario0/1
        , directory/1
        , get_account/3
        , new_account/4
        , update_account/4
        ]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(REQUEST_TIMEOUT, 5000). % 5 seconds.


-type nonce() :: string().
-type url() :: string().
-type proplist() :: [{_, _}].
-type jws() :: map().

-spec directory(url()) ->
  {ok, map(), nonce()} | {error, _}.
directory(Url) ->
  Options = [],
  HttpOptions = [{timeout, ?REQUEST_TIMEOUT}],
  case httpc:request(get, {Url, []}, HttpOptions, Options) of
    {ok, {{_, Code, _}, Head, Body}} when Code >= 200, Code =< 299 ->
      %% Decode the json string
      case decode(Body) of
        {error, Reason} ->
          ?ERROR_MSG("Problem decoding: ~s", [Body]),
          {error, Reason};
        Directories ->
          StrDirectories = [{bitstring_to_list(X), bitstring_to_list(Y)} ||
                              {X,Y} <- Directories],
          Nonce = get_nonce(Head),
          %% Return Map of Directories
          NewDirs = maps:from_list(StrDirectories),
          {ok, NewDirs, Nonce}
      end;
    Error ->
      failed_http_request(Error, Url)
  end.

-spec new_account(url(), jose_jwk:key(), proplist(), nonce()) ->
  {ok, {url(), proplist()}, nonce()} | {error, _}.
new_account(Url, PrivateKey, Req, Nonce) ->
  %% Make the request body
  ReqBody = jiffy:encode({[{ <<"resource">>, <<"new-reg">>}] ++ Req}),
  {_, SignedBody} = sign_json_jose(PrivateKey, ReqBody, Nonce),
  %% Encode the Signed body with jiffy
  FinalBody = jiffy:encode(SignedBody),
  Options = [],
  HttpOptions = [{timeout, ?REQUEST_TIMEOUT}],
  case httpc:request(post,
      {Url, [], "application/jose+json", FinalBody}, HttpOptions, Options) of
    {ok, {{_, Code, _}, Head, Body}} when Code >= 200, Code =< 299 ->
      case decode(Body) of
        {error, Reason} ->
          ?ERROR_MSG("Problem decoding: ~s", [Body]),
          {error, Reason};
        Return ->
          TOSUrl = get_tos(Head),
          NewNonce = get_nonce(Head),
          {ok, {TOSUrl, Return}, NewNonce}
      end;
    Error ->
      failed_http_request(Error, Url)
  end.

-spec update_account(url(), jose_jwk:key(), proplist(), nonce()) ->
  {ok, proplist(), nonce()} | {error, _}.
update_account(Url, PrivateKey, Req, Nonce) ->
  %% Make the request body
  ReqBody = jiffy:encode({[{ <<"resource">>, <<"reg">>}] ++ Req}),
  {_, SignedBody} = sign_json_jose(PrivateKey, ReqBody, Nonce),
  %% Encode the Signed body with jiffy
  FinalBody = jiffy:encode(SignedBody),
  Options = [],
  HttpOptions = [{timeout, ?REQUEST_TIMEOUT}],
  case httpc:request(post,
      {Url, [], "application/jose+json", FinalBody}, HttpOptions, Options) of
    {ok, {{_, Code, _}, Head, Body}} when Code >= 200, Code =< 299 ->
      case decode(Body) of
        {error, Reason} ->
          ?ERROR_MSG("Problem decoding: ~s", [Body]),
          {error, Reason};
        Return ->
          NewNonce = get_nonce(Head),
          {ok, Return, NewNonce}
      end;
    Error ->
      failed_http_request(Error, Url)
  end.

-spec get_account(url(), jose_jwk:key(), nonce()) ->
  {ok, {url(), proplist()}, nonce()} | {error, _}.
get_account(Url, PrivateKey, Nonce) ->
  %% Make the request body
  ReqBody = jiffy:encode({[{<<"resource">>, <<"reg">>}]}),
  %% Jose Sign
  {_, SignedBody} = sign_json_jose(PrivateKey, ReqBody, Nonce),
  %% Encode the Signed body with jiffy
  FinalBody = jiffy:encode(SignedBody),
  Options = [],
  HttpOptions = [{timeout, ?REQUEST_TIMEOUT}],
  case httpc:request(post,
      {Url, [], "application/jose+json", FinalBody}, HttpOptions, Options) of
    {ok, {{_, Code, _}, Head, Body}} when Code >= 200, Code =< 299 ->
      case decode(Body) of
        {error, Reason} ->
          ?ERROR_MSG("Problem decoding: ~s", [Body]),
          {error, Reason};
        Return ->
          TOSUrl = get_tos(Head),
          NewNonce = get_nonce(Head),
          {ok, {TOSUrl, Return}, NewNonce}
      end;
    Error ->
      failed_http_request(Error, Url)
  end.


%%
%% Useful funs
%%
-spec get_nonce(proplist()) -> nonce() | 'none'.
get_nonce(Head) ->
  case proplists:lookup("replay-nonce", Head) of
    {"replay-nonce", Nonce} -> Nonce;
    none -> none
  end.

%% Very bad way to extract this
%% TODO: Find a better way
-spec get_tos(proplist()) -> url() | 'none'.
get_tos(Head) ->
  try
    [{_, Link}] = [{K, V} || {K, V} <- Head,
        K =:= "link" andalso lists:suffix("\"terms-of-service\"", V)],
    [Link1, _] = string:tokens(Link, ";"),
    Link2 = string:strip(Link1, left, $<),
    string:strip(Link2, right, $>)
  catch
    _:_ ->
      none
  end.

-spec sign_json_jose(jose_jwk:key(), string(), nonce()) -> jws().
sign_json_jose(Key, Json, Nonce) ->
    % Generate a public key
    PubKey = jose_jwk:to_public(Key),
    {_, BinaryPubKey} = jose_jwk:to_binary(PubKey),
    PubKeyJson = jiffy:decode(BinaryPubKey),
    % Jws object containing the algorithm
    %% TODO: Dont hardcode the alg
    JwsObj = jose_jws:from(
      #{ <<"alg">> => <<"ES256">>
       % , <<"b64">> => true
       , <<"jwk">> => PubKeyJson
       , <<"nonce">> => list_to_bitstring(Nonce)
       }),

    %% Signed Message
    jose_jws:sign(Key, Json, JwsObj).

decode(Json) ->
  try
    {Result} = jiffy:decode(Json),
    Result
  catch
    _:Reason ->
      {error, Reason}
  end.

-spec failed_http_request({ok, _} | {error, _}, url()) -> {error, _}.
failed_http_request({ok, {{_, Code, _}, _Head, Body}}, Url) ->
  ?ERROR_MSG("Got unexpected status code from <~s>: ~B, Body: ~s",
    [Url, Code, Body]),
  {error, unexpected_code};
failed_http_request({error, Reason}, Url) ->
  ?ERROR_MSG("Error making a request to <~s>: ~p",
    [Url, Reason]),
  {error, Reason}.


%%
%% Debugging Funcs -- They are only used for the development phase
%%

%% A typical acme workflow
scenario(CAUrl, AccId, PrivateKey) ->
  DirURL = CAUrl ++ "/directory",
  {ok, Dirs, Nonce0} = directory(DirURL),

  AccURL = CAUrl ++ "/acme/reg/" ++ AccId,
  {ok, {_TOS, Account}, Nonce1} = get_account(AccURL, PrivateKey, Nonce0).

new_user_scenario(CAUrl) ->
  PrivateKey = generate_key(),

  DirURL = CAUrl ++ "/directory",
  {ok, Dirs, Nonce0} = directory(DirURL),

  #{"new-reg" := NewAccURL} = Dirs,
  Req0 = [{ <<"contact">>, [<<"mailto:cert-example-admin@example2.com">>]}],
  {ok, {TOS, Account}, Nonce1} = new_account(NewAccURL, PrivateKey, Req0, Nonce0),

  {_, AccId} = proplists:lookup(<<"id">>, Account),
  AccURL = CAUrl ++ "/acme/reg/" ++ integer_to_list(AccId),
  Req1 = [{ <<"agreement">>, list_to_bitstring(TOS)}],
  {ok, Account1, Nonce2} = update_account(AccURL, PrivateKey, Req1, Nonce1),

  {Account1, PrivateKey}.

generate_key() ->
  jose_jwk:generate_key({ec, secp256r1}).

%% Just a test
scenario0(KeyFile) ->
  PrivateKey = jose_jwk:from_file(KeyFile),
  scenario("http://localhost:4000", "2", PrivateKey).
  % new_user_scenario("http://localhost:4000").
