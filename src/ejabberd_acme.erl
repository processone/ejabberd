-module (ejabberd_acme).

-export([ scenario/3
        , scenario0/0
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
-define(DIRURL, "directory").
-define(REGURL, "/acme/reg/").

-define(DEFAULT_KEY_FILE, "private_key_temporary").


directory(DirURL) ->
  Options = [],
  HttpOptions = [{timeout, ?REQUEST_TIMEOUT}],
  case httpc:request(get, {DirURL, []}, HttpOptions, Options) of
    {ok, {{_, Code, _}, Head, Body}} when Code >= 200, Code =< 299 ->
      %% Decode the json string
      {Directories} = jiffy:decode(Body),
      StrDirectories = [{bitstring_to_list(X), bitstring_to_list(Y)} || 
                          {X,Y} <- Directories],
      % Find and save the replay nonce
      Nonce = get_nonce(Head),
      %% Return Map of Directories
      NewDirs = maps:from_list(StrDirectories),
      {ok, NewDirs, Nonce};
    {ok, {{_, Code, _}, Head, _Body}} ->
      ?ERROR_MSG("Got unexpected status code from <~s>: ~B",
        [DirURL, Code]),
      Nonce = get_nonce(Head),
      {error, unexpected_code, Nonce};
    {error, Reason} ->
      ?ERROR_MSG("Error requesting directory from <~s>: ~p",
        [DirURL, Reason]),
      {error, Reason}
  end.

new_account(NewAccURl, PrivateKey, Req, Nonce) ->
  %% Make the request body
  ReqBody = jiffy:encode({[{ <<"resource">>, <<"new-reg">>}] ++ Req}),
  {_, SignedBody} = sign_json_jose(PrivateKey, ReqBody, NewAccURl, Nonce),
  %% Encode the Signed body with jiffy
  FinalBody = jiffy:encode(SignedBody),
  Options = [],
  HttpOptions = [{timeout, ?REQUEST_TIMEOUT}],
  case httpc:request(post, 
      {NewAccURl, [], "application/jose+json", FinalBody}, HttpOptions, Options) of
    {ok, {{_, Code, _}, Head, Body}} when Code >= 200, Code =< 299 ->
      %% Decode the json string
      {Return} = jiffy:decode(Body),
      TOSUrl = get_TOS(Head),
      % Find and save the replay nonce
      NewNonce = get_nonce(Head),
      {ok, {TOSUrl, Return}, NewNonce};
    {ok, {{_, 409 = Code, _}, Head, Body}} ->
      ?ERROR_MSG("Got status code: ~B from <~s>, Body: ~s",
        [NewAccURl, Code, Body]),
      NewNonce = get_nonce(Head),
      {error, key_in_use, NewNonce};
    {ok, {{_, Code, _}, Head, Body}} ->
      ?ERROR_MSG("Got unexpected status code from <~s>: ~B, Body: ~s",
        [NewAccURl, Code, Body]),
      NewNonce = get_nonce(Head),
      {error, unexpected_code, NewNonce};
    {error, Reason} ->
      ?ERROR_MSG("Error requesting directory from <~s>: ~p",
        [NewAccURl, Reason]),
      {error, Reason, Nonce}
  end.

update_account(AccURl, PrivateKey, Req, Nonce) ->
  %% Make the request body
  ReqBody = jiffy:encode({[{ <<"resource">>, <<"reg">>}] ++ Req}),
  {_, SignedBody} = sign_json_jose(PrivateKey, ReqBody, AccURl, Nonce),
  %% Encode the Signed body with jiffy
  FinalBody = jiffy:encode(SignedBody),
  Options = [],
  HttpOptions = [{timeout, ?REQUEST_TIMEOUT}],
  case httpc:request(post, 
      {AccURl, [], "application/jose+json", FinalBody}, HttpOptions, Options) of
    {ok, {{_, Code, _}, Head, Body}} when Code >= 200, Code =< 299 ->
      %% Decode the json string
      {Return} = jiffy:decode(Body),
      % Find and save the replay nonce
      NewNonce = get_nonce(Head),
      {ok, Return, NewNonce};
    {ok, {{_, Code, _}, Head, Body}} ->
      ?ERROR_MSG("Got unexpected status code from <~s>: ~B, Body: ~s",
        [AccURl, Code, Body]),
      NewNonce = get_nonce(Head),
      {error, unexpected_code, NewNonce};
    {error, Reason} ->
      ?ERROR_MSG("Error requesting directory from <~s>: ~p",
        [AccURl, Reason]),
      {error, Reason, Nonce}
  end.


get_account(AccURl, PrivateKey, Nonce) ->
  %% Make the request body
  ReqBody = jiffy:encode({[
      { <<"resource">>, <<"reg">>}
  ]}),
  %% Jose Sign
  {_, SignedBody} = sign_json_jose(PrivateKey, ReqBody, AccURl, Nonce),
  %% Encode the Signed body with jiffy
  FinalBody = jiffy:encode(SignedBody),
  Options = [],
  HttpOptions = [{timeout, ?REQUEST_TIMEOUT}],
  case httpc:request(post, 
      {AccURl, [], "application/jose+json", FinalBody}, HttpOptions, Options) of
    {ok, {{_, Code, _}, Head, Body}} when Code >= 200, Code =< 299 ->
      %% Decode the json string
      {Return} = jiffy:decode(Body),
      TOSUrl = get_TOS(Head),
      % Find and save the replay nonce
      NewNonce = get_nonce(Head),
      {ok, {TOSUrl, Return}, NewNonce};
    {ok, {{_, Code, _}, Head, Body}} ->
      ?ERROR_MSG("Got unexpected status code from <~s>: ~B, Head: ~s",
        [AccURl, Code, Body]),
        NewNonce = get_nonce(Head),
      {error, unexpected_code, NewNonce};
    {error, Reason} ->
      ?ERROR_MSG("Error requesting directory from <~s>: ~p",
        [AccURl, Reason]),
      {error, Reason, Nonce}
  end.


%%
%% Useful funs
%%

get_nonce(Head) ->
  {"replay-nonce", Nonce} = proplists:lookup("replay-nonce", Head),
  Nonce.

%% Very bad way to extract this
%% TODO: Find a better way
get_TOS(Head) ->
  try
    [{_, Link}] = [{K, V} || {K, V} <- Head, 
        K =:= "link" andalso lists:suffix("\"terms-of-service\"", V)],
    [Link1, _] = string:tokens(Link, ";"),
    Link2 = string:strip(Link1, left, $<),
    string:strip(Link2, right, $>)
  catch
    _:_ ->
      no_tos
  end.


sign_json_jose(Key, Json, Url, Nonce) ->
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

%%
%% Debugging Funcs -- They are only used for the development phase
%%

%% A typical acme workflow
scenario(CAUrl, AccId, PrivateKey) ->
  
  DirURL = CAUrl ++ "/" ++ ?DIRURL,
  {ok, Dirs, Nonce0} = directory(DirURL),

  AccURL = CAUrl ++ ?REGURL ++ AccId,
  {ok, {_TOS, Account}, Nonce1} = get_account(AccURL, PrivateKey, Nonce0).

new_user_scenario(CAUrl) ->
  PrivateKey = generate_key(),

  DirURL = CAUrl ++ "/" ++ ?DIRURL,
  {ok, Dirs, Nonce0} = directory(DirURL),

  #{"new-reg" := NewAccURL} = Dirs,
  Req0 = [{ <<"contact">>, [<<"mailto:cert-example-admin@example2.com">>]}],
  {ok, {TOS, Account}, Nonce1} = new_account(NewAccURL, PrivateKey, Req0, Nonce0),

  {_, AccId} = proplists:lookup(<<"id">>, Account),
  AccURL = CAUrl ++ ?REGURL ++ integer_to_list(AccId),
  Req1 = [{ <<"agreement">>, list_to_bitstring(TOS)}],
  {ok, Account1, Nonce2} = update_account(AccURL, PrivateKey, Req1, Nonce1),

  {Account1, PrivateKey}.

generate_key() ->
  jose_jwk:generate_key({ec, secp256r1}).

%% Just a test
scenario0() ->
  PrivateKey = jose_jwk:from_file(?DEFAULT_KEY_FILE),
  % scenario("http://localhost:4000", "2", PrivateKey).
  new_user_scenario("http://localhost:4000").
