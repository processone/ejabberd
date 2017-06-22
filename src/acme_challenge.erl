-module(acme_challenge).

-export ([ key_authorization/2,
	   solve_challenge/3
         ]).
%% Challenge Types
%% ================
%% 1. http-01: https://tools.ietf.org/html/draft-ietf-acme-acme-05#section-7.2
%% 2. dns-01: https://tools.ietf.org/html/draft-ietf-acme-acme-05#section-7.3
%% 3. tls-sni-01: https://tools.ietf.org/html/draft-ietf-acme-acme-05#section-7.4
%% 4. (?) oob-01:  https://tools.ietf.org/html/draft-ietf-acme-acme-05#section-7.5

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-include("ejabberd_acme.hrl").

-spec key_authorization(string(), jose_jwk:key()) -> bitstring().
key_authorization(Token, Key) ->
    Thumbprint = jose_jwk:thumbprint(Key),
    %% ?INFO_MSG("Thumbprint: ~p~n", [Thumbprint]),
    KeyAuthorization = erlang:iolist_to_binary([Token, <<".">>, Thumbprint]),
    KeyAuthorization.

-spec parse_challenge({proplist()}) -> {ok, acme_challenge()} | {error, _}.
parse_challenge(Challenge0) ->
    try
	{Challenge} = Challenge0,
	{<<"type">>,Type} = proplists:lookup(<<"type">>, Challenge),
	{<<"status">>,Status} = proplists:lookup(<<"status">>, Challenge),
	{<<"uri">>,Uri} = proplists:lookup(<<"uri">>, Challenge),
	{<<"token">>,Token} = proplists:lookup(<<"token">>, Challenge),
	Res = 
	    #challenge{
	       type = Type,
	       status = list_to_atom(bitstring_to_list(Status)),
	       uri = bitstring_to_list(Uri),
	       token = Token
	      },
	{ok, Res}
    catch
	_:Error ->
	    {error, Error}
    end.



-spec solve_challenge(bitstring(), [{proplist()}], _) -> {ok, url(), bitstring()} | {error, _}.
solve_challenge(ChallengeType, Challenges, Options) ->
    ParsedChallenges = [parse_challenge(Chall) || Chall <- Challenges],
    case lists:any(fun is_error/1, ParsedChallenges) of
	true ->
	    ?ERROR_MSG("Error parsing challenges: ~p~n", [Challenges]),
	    {error, parse_challenge};
	false ->
	    case [C || {ok, C} <- ParsedChallenges, is_challenge_type(ChallengeType, C)] of
		[Challenge] ->
		    solve_challenge1(Challenge, Options);
		_ ->
		    ?ERROR_MSG("Challenge ~p not found in challenges: ~p~n", [ChallengeType, Challenges]),
		    {error, not_found}
	    end
    end.

-spec solve_challenge1(acme_challenge(), _) -> {ok, url(), bitstring()} | {error, _}.
solve_challenge1(Chal = #challenge{type = <<"http-01">>, token=Tkn}, {Key, HttpDir}) ->
    KeyAuthz = key_authorization(Tkn, Key),
    FileLocation = HttpDir ++ "/.well-known/acme-challenge/" ++ bitstring_to_list(Tkn),
    case file:write_file(FileLocation, KeyAuthz) of
	ok ->
	    {ok, Chal#challenge.uri, KeyAuthz};
	{error, _} = Err ->
	    ?ERROR_MSG("Error writing to file: ~s with reason: ~p~n", [FileLocation, Err]),
	    Err
    end;
%% TODO: Fill stub
solve_challenge1(Challenge, _Key) ->
    ?INFO_MSG("Challenge: ~p~n", [Challenge]).

%% Useful functions

is_challenge_type(DesiredType, #challenge{type = Type}) when DesiredType =:= Type ->
    true;
is_challenge_type(_DesiredType, #challenge{type = _Type}) ->
    false.

is_error({error, _}) -> true;
is_error(_) -> false.
