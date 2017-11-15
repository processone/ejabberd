-module(acme_challenge).

-export ([key_authorization/2,
	  solve_challenge/3,
	  process/2,
	  register_hooks/1,
	  unregister_hooks/1,
	  acme_handler/3
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
-include("ejabberd_http.hrl").
-include("ejabberd_acme.hrl").

%% This is the default endpoint for the http challenge
%% This hooks is called from ejabberd_http
acme_handler(Handlers, _Host, Request) ->
    case Request#request.path of
	[<<".well-known">>|_] ->
	    [{[<<".well-known">>],acme_challenge}|Handlers];
	_ ->
	    Handlers
    end.

%% TODO: Maybe validate request here??
process(LocalPath, _Request) ->
    Result = ets_get_key_authorization(LocalPath),
    {200, 
     [{<<"Content-Type">>, <<"text/plain">>}], 
     Result}. 

register_hooks(_Domain) ->
    ?INFO_MSG("Registering hook for ACME HTTP headers", []),
    ejabberd_hooks:add(http_request_handlers, ?MODULE, acme_handler, 50).

unregister_hooks(_Domain) ->
    ?INFO_MSG("Unregistering hook for ACME HTTP headers", []),
    ejabberd_hooks:delete(http_request_handlers, ?MODULE, acme_handler, 50).

-spec key_authorization(bitstring(), jose_jwk:key()) -> bitstring().
key_authorization(Token, Key) ->
    Thumbprint = jose_jwk:thumbprint(Key),
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



-spec solve_challenge(bitstring(), [{proplist()}], _) ->
			     {ok, url(), bitstring()} | {error, _}.
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

-spec solve_challenge1(acme_challenge(), {jose_jwk:key(), string()}) ->
			      {ok, url(), bitstring()} | {error, _}.
solve_challenge1(Chal = #challenge{type = <<"http-01">>, token=Tkn}, Key) ->
    KeyAuthz = key_authorization(Tkn, Key),
    %% save_key_authorization(Chal, Tkn, KeyAuthz, HttpDir);
    ets_put_key_authorization(Tkn, KeyAuthz),
    {ok, Chal#challenge.uri, KeyAuthz};
solve_challenge1(Challenge, _Key) ->
    ?ERROR_MSG("Unkown Challenge Type: ~p", [Challenge]),
    {error, unknown_challenge}.


-spec ets_put_key_authorization(bitstring(), bitstring()) -> ok.
ets_put_key_authorization(Tkn, KeyAuthz) ->
    Tab = ets_get_acme_table(),
    Key = [<<"acme-challenge">>, Tkn],
    ets:insert(Tab, {Key, KeyAuthz}),
    ok.
 
-spec ets_get_key_authorization([bitstring()]) -> bitstring().
ets_get_key_authorization(Key) ->
    Tab = ets_get_acme_table(),
    case ets:lookup(Tab, Key) of
	[{Key, KeyAuthz}] ->
	    ets:delete(Tab, Key),
	    KeyAuthz;
	_ ->
	    ?ERROR_MSG("Unable to serve key authorization in: ~p", [Key]),
	    <<"">>
    end.

-spec ets_get_acme_table() -> atom().
ets_get_acme_table() ->
    case ets:info(acme) of
	undefined ->
	    ets:new(acme, [named_table, public]);
	_ ->
	    acme
    end.

%% Useful functions

is_challenge_type(DesiredType, #challenge{type = Type}) when DesiredType =:= Type ->
    true;
is_challenge_type(_DesiredType, #challenge{type = _Type}) ->
    false.

-spec is_error({'error', _}) -> 'true';
	      ({'ok', _}) -> 'false'.
is_error({error, _}) -> true;
is_error(_) -> false.
