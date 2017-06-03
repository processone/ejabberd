-module(acme_challenge).

-export ([ key_authorization/2 
         , challenges_to_objects/1
         , solve_challenges/2 
         ]).
%% Challenge Types
%% ================
%% 1. http-01: https://tools.ietf.org/html/draft-ietf-acme-acme-05#section-7.2
%% 2. dns-01: https://tools.ietf.org/html/draft-ietf-acme-acme-05#section-7.3
%% 3. tls-sni-01: https://tools.ietf.org/html/draft-ietf-acme-acme-05#section-7.4
%% 4. (?) oob-01:  https://tools.ietf.org/html/draft-ietf-acme-acme-05#section-7.5
-define(DEFAULT_HTTP_DIR, "../test-server-for-acme").

-record(challenge, {
        type  = <<"http-01">> :: bitstring(),
        status = pending :: pending | valid | invalid,
        uri = <<"">> :: bitstring(),
        token = <<"">> :: bitstring()
        }).

key_authorization(Token, Key) ->
  Thumbprint = jose_jwk:thumbprint(Key),
  io:format("Thumbprint: ~p~n", [Thumbprint]),

  KeyAuthorization = erlang:iolist_to_binary([Token, <<".">>, Thumbprint]),
  % io:format("KeyAuthorization: ~p~n", [KeyAuthorization]),

  KeyAuthorization.  

challenges_to_objects(Challenges) ->
  [clean_challenge(X) || {X} <- Challenges].

clean_challenge(Challenge) ->
  {<<"type">>,Type} = proplists:lookup(<<"type">>, Challenge),
  {<<"status">>,Status} = proplists:lookup(<<"status">>, Challenge),
  {<<"uri">>,Uri} = proplists:lookup(<<"uri">>, Challenge),
  {<<"token">>,Token} = proplists:lookup(<<"token">>, Challenge),
  #challenge{
    type = Type,
    status = list_to_atom(bitstring_to_list(Status)),
    uri = Uri,
    token = Token
  }.

solve_challenges(Challenges, Key) ->
  [solve_challenge(X, Key) || X <- Challenges].


solve_challenge(Chal = #challenge{type = <<"http-01">>, token=Tkn}, Key) ->
  io:format("Http Challenge: ~p~n", [Chal]),
  KeyAuthz = key_authorization(Tkn, Key),
  io:format("KeyAuthorization: ~p~n", [KeyAuthz]),

  %% Create file for authorization
  ok = file:write_file(?DEFAULT_HTTP_DIR ++ 
    "/.well-known/acme-challenge/" ++ 
    bitstring_to_list(Tkn), KeyAuthz),
  {<<"http-01">>, Chal#challenge.uri, KeyAuthz};
solve_challenge(Challenge, Key) ->
  io:format("Challenge: ~p~n", [Challenge]).