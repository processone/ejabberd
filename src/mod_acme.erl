-module(mod_acme).

-behaviour(gen_server).

%% API
-export([ start/0
        , stop/1
        %% I tried to follow the naming convention found in the acme spec
        , directory/2
        , new_nonce/2
        %% Account
        , new_reg/2
        , update_account/2
        , account_info/2   %% TODO: Maybe change to get_account
        , account_key_change/2
        , deactivate_account/2
        %% Orders/Certificates
        , new_cert/2
        , new_authz/2
        , get_certificate/2
        , get_authz/2
        , complete_challenge/2
        , deactivate_authz/2
        , revoke_cert/2]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([scenario/0]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").
-include_lib("public_key/include/public_key.hrl").

% -define(CA_URL, "https://acme-v01.api.letsencrypt.org").



-define(DEFAULT_DIRECTORY, ?CA_URL ++ "/directory").
-define(DEFAULT_NEW_NONCE, ?CA_URL ++ "/acme/new_nonce").

-define(DEFAULT_KEY_FILE, "private_key_temporary").




-define(LOCAL_TESTING, true).

-ifdef(LOCAL_TESTING).
-define(CA_URL, "http://localhost:4000").
-define(DEFAULT_ACCOUNT, "2").
-define(DEFAULT_TOS, <<"http://boulder:4000/terms/v1">>).
-define(DEFAULT_AUTHZ, 
    <<"http://localhost:4000/acme/authz/XDAfMW6xBdRogD2-VIfTxlzo4RTlaE2U6x0yrwxnXlw">>).
-else.
-define(CA_URL, "https://acme-staging.api.letsencrypt.org").
-define(DEFAULT_ACCOUNT, "2273801").
-define(DEFAULT_TOS, <<"https://letsencrypt.org/documents/LE-SA-v1.1.1-August-1-2016.pdf">>).
-define(DEFAULT_AUTHZ, <<"">>).
-endif.

-record(state, {
        ca_url  = ?CA_URL :: list(),
        dir_url = ?DEFAULT_DIRECTORY :: list(),
        dirs = maps:new(),
        nonce = "",
        account = none
        }).

%% This will be initially just be filled with stub functions

start() ->
    gen_server:start(?MODULE, [], []).

stop(Pid) ->
    gen_server:stop(Pid).

%% Stub functions
directory(Pid, Options) ->
    gen_server:call(Pid, ?FUNCTION_NAME).

new_nonce(Pid, Options) ->
    gen_server:call(Pid, ?FUNCTION_NAME).

new_reg(Pid, Options) ->
    gen_server:call(Pid, ?FUNCTION_NAME).

update_account(Pid, AccountId) ->
    %% TODO: This has to have more info ofcourse
    gen_server:call(Pid, {?FUNCTION_NAME, AccountId}).

account_info(Pid, AccountId) ->
    gen_server:call(Pid, {?FUNCTION_NAME, AccountId}).

account_key_change(Pid, Options) ->
    ok.

deactivate_account(Pid, Options) ->
    ok.

new_cert(Pid, Options) ->
    gen_server:call(Pid, ?FUNCTION_NAME).

new_authz(Pid, Options) ->
    gen_server:call(Pid, ?FUNCTION_NAME).

get_certificate(Pid, Options) ->
    ok.

get_authz(Pid, Options) ->
    gen_server:call(Pid, ?FUNCTION_NAME).

complete_challenge(Pid, Options) ->
    gen_server:call(Pid, {?FUNCTION_NAME, Options}).

deactivate_authz(Pid, Options) ->
    ok.

revoke_cert(Pid, Options) ->
    ok.



%% GEN SERVER

init([]) ->
    %% TODO: Not the correct way of doing it
    ok = application:start(inets),
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),

    ok = application:start(base64url),
    ok = application:start(jose),

    {ok, #state{}}.

handle_call(directory, _From, S = #state{dir_url=Url, dirs=Dirs}) ->
    %% Make the get request
    {ok, {_Status, Head, Body}} = httpc:request(get, {Url, []}, [], []),
    
    %% Decode the json string
    {Directories} = jiffy:decode(Body),
    StrDirectories = [{bitstring_to_list(X), bitstring_to_list(Y)} || 
                        {X,Y} <- Directories],

    % Find and save the replay nonce
    % io:format("Directory Head Response: ~p~n", [Head]),
    Nonce = get_nonce(Head),

    %% Update the directories in state
    NewDirs = maps:from_list(StrDirectories),
    % io:format("New directories: ~p~n", [NewDirs]),
    
    {reply, {ok, {Directories}}, S#state{dirs = NewDirs, nonce = Nonce}};
handle_call(new_nonce, _From, S = #state{dirs=Dirs}) ->
    %% Get url from all directories
    #{"new_nonce" := Url} = Dirs,
    {ok, {Status, Head, []}} = 
        httpc:request(head, {Url, []}, [], []),
    {reply, {ok, {Status, Head}}, S};
handle_call(new_reg, _From, S = #state{ca_url = Ca, dirs=Dirs, nonce = Nonce}) ->
    %% Get url from all directories
    #{"new-reg" := Url} = Dirs,

    %% Make the request body
    ReqBody = jiffy:encode({
    [ { <<"contact">>, [<<"mailto:cert-admin@example.com">>]}
    , { <<"resource">>, <<"new-reg">>}
    ]}),

    %% Generate a key for the first time use
    Key = generate_key(),

    %% Write the key to a file
    jose_jwk:to_file(?DEFAULT_KEY_FILE, Key),

    %% Jose
    {_, SignedBody} = sign_a_json_object_using_jose(Key, ReqBody, Url, Nonce),
    % io:format("Signed Body: ~p~n", [SignedBody]),

    %% Encode the Signed body with jiffy
    FinalBody = jiffy:encode(SignedBody),

    %% Post request
    {ok, {Status, Head, Body}} = 
        httpc:request(post, {Url, [], "application/jose+json", FinalBody}, [], []),

    %% Get and save the new nonce
    NewNonce = get_nonce(Head),
        
    {reply, {ok, {Status, Head, Body}}, S#state{nonce=NewNonce}};    
handle_call({account_info, AccountId}, _From, S = #state{ca_url = Ca, dirs=Dirs, nonce = Nonce}) ->
    %% Get url from accountId
    Url = Ca ++ "/acme/reg/" ++ AccountId,

    %% Make the request body
    ReqBody = jiffy:encode({[
        { <<"resource">>, <<"reg">>}
    ]}),

    %% Get the key from a file
    Key = jose_jwk:from_file(?DEFAULT_KEY_FILE),

    %% Jose
    {_, SignedBody} = sign_a_json_object_using_jose(Key, ReqBody, Url, Nonce),
    % io:format("Signed Body: ~p~n", [SignedBody]),

    %% Encode the Signed body with jiffy
    FinalBody = jiffy:encode(SignedBody),

    %% Post request
    {ok, {Status, Head, Body}} = 
        httpc:request(post, {Url, [], "application/jose+json", FinalBody}, [], []),

    % Get and save the new nonce
    NewNonce = get_nonce(Head),
        
    {reply, {ok, {Status, Head, Body}}, S#state{nonce=NewNonce}};
handle_call({update_account, AccountId}, _From, S = #state{ca_url = Ca, dirs=Dirs, nonce = Nonce}) ->
    %% Get url from accountId
    Url = Ca ++ "/acme/reg/" ++ AccountId,

    %% Make the request body
    ReqBody = jiffy:encode({[
        { <<"resource">>, <<"reg">>}, 
        { <<"agreement">>, ?DEFAULT_TOS}
    ]}),

    %% Get the key from a file
    Key = jose_jwk:from_file(?DEFAULT_KEY_FILE),

    %% Jose
    {_, SignedBody} = sign_a_json_object_using_jose(Key, ReqBody, Url, Nonce),
    % io:format("Signed Body: ~p~n", [SignedBody]),

    %% Encode the Signed body with jiffy
    FinalBody = jiffy:encode(SignedBody),

    %% Post request
    {ok, {Status, Head, Body}} = 
        httpc:request(post, {Url, [], "application/jose+json", FinalBody}, [], []),

    % Get and save the new nonce
    NewNonce = get_nonce(Head),
        
    {reply, {ok, {Status, Head, Body}}, S#state{nonce=NewNonce}};
handle_call(new_cert, _From, S = #state{ca_url = Ca, dirs=Dirs, nonce = Nonce}) ->
    %% Get url from all directories
    #{"new-cert" := Url} = Dirs,

    %% Make the request body
    ReqBody = jiffy:encode({[
        { <<"resource">>, <<"new-cert">>}
    ]}),

    %% Get the key from a file
    Key = jose_jwk:from_file(?DEFAULT_KEY_FILE),

    %% Jose
    {_, SignedBody} = sign_a_json_object_using_jose(Key, ReqBody, Url, Nonce),
    % io:format("Signed Body: ~p~n", [SignedBody]),

    %% Encode the Signed body with jiffy
    FinalBody = jiffy:encode(SignedBody),

    %% Post request
    {ok, {Status, Head, Body}} = 
        httpc:request(post, {Url, [], "application/jose+json", FinalBody}, [], []),

    % Get and save the new nonce
    NewNonce = get_nonce(Head),
        
    {reply, {ok, {Status, Head, Body}}, S#state{nonce=NewNonce}};
handle_call(new_authz, _From, S = #state{ca_url = Ca, dirs=Dirs, nonce = Nonce}) ->
    %% Get url from all directories
    #{"new-authz" := Url} = Dirs,

    %% Make the request body
    ReqBody = jiffy:encode({
      [ { <<"identifier">>, {
          [ {<<"type">>, <<"dns">>} 
          , {<<"value">>, <<"my-acme-test-ejabberd.com">>}
          ] }}
      , {<<"existing">>, <<"accept">>}
      , { <<"resource">>, <<"new-authz">>}
      ] }),

    %% Get the key from a file
    Key = jose_jwk:from_file(?DEFAULT_KEY_FILE),

    %% Jose
    {_, SignedBody} = sign_a_json_object_using_jose(Key, ReqBody, Url, Nonce),
    % io:format("Signed Body: ~p~n", [SignedBody]),

    %% Encode the Signed body with jiffy
    FinalBody = jiffy:encode(SignedBody),

    %% Post request
    {ok, {Status, Head, Body}} = 
        httpc:request(post, {Url, [], "application/jose+json", FinalBody}, [], []),

    % Get and save the new nonce
    NewNonce = get_nonce(Head),
        
    {reply, {ok, {Status, Head, Body}}, S#state{nonce=NewNonce}};
handle_call(get_authz, _From, S = #state{ca_url = Ca, dirs=Dirs, nonce = Nonce}) ->
    %% Get url from all directories
    Url = bitstring_to_list(?DEFAULT_AUTHZ),

    %% Post request
    {ok, {Status, Head, Body}} = 
        % httpc:request(post, {Url, [], "application/jose+json", FinalBody}, [], []),
        httpc:request(Url),

    % Get and save the new nonce
    NewNonce = get_nonce(Head),
        
    {reply, {ok, {Status, Head, Body}}, S#state{nonce=NewNonce}};
handle_call({complete_challenge, [Solution]}, _From, S = #state{ca_url = Ca, dirs=Dirs, nonce = Nonce}) ->
    %% Get url from all directories
    {ChallengeType, BitUrl, KeyAuthz} = Solution,
    Url = bitstring_to_list(BitUrl),

    %% Make the request body
    ReqBody = jiffy:encode({
      [ { <<"keyAuthorization">>, KeyAuthz}
      , {<<"type">>, ChallengeType}
      , { <<"resource">>, <<"challenge">>}
      ] }),

    %% Get the key from a file
    Key = jose_jwk:from_file(?DEFAULT_KEY_FILE),

    %% Jose
    {_, SignedBody} = sign_a_json_object_using_jose(Key, ReqBody, Url, Nonce),
    % io:format("Signed Body: ~p~n", [SignedBody]),

    %% Encode the Signed body with jiffy
    FinalBody = jiffy:encode(SignedBody),

    %% Post request
    {ok, {Status, Head, Body}} = 
        httpc:request(post, {Url, [], "application/jose+json", FinalBody}, [], []),

    % Get and save the new nonce
    NewNonce = get_nonce(Head),
        
    {reply, {ok, {Status, Head, Body}}, S#state{nonce=NewNonce}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(_Info, State) -> 
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%% Util functions

final_url(Urls) ->
    Joined = lists:join("/", Urls),
    lists:flatten(Joined).

get_nonce(Head) ->
    {"replay-nonce", Nonce} = proplists:lookup("replay-nonce", Head),
    Nonce.

get_challenges({Body}) ->
    {<<"challenges">>, Challenges} = proplists:lookup(<<"challenges">>, Body),
    Challenges.


%% Test

generate_key() ->
    % Generate a key for now
    Key = jose_jwk:generate_key({ec, secp256r1}),
    io:format("Key: ~p~n", [Key]),
    Key.

sign_a_json_object_using_jose(Key, Json, Url, Nonce) ->
    % Generate a public key
    PubKey = jose_jwk:to_public(Key),
    % io:format("Public Key: ~p~n", [PubKey]),    
    {_, BinaryPubKey} = jose_jwk:to_binary(PubKey),
    % io:format("Public Key: ~p~n", [BinaryPubKey]),    
    PubKeyJson = jiffy:decode(BinaryPubKey),
    % io:format("Public Key: ~p~n", [PubKeyJson]), 
    
    % Jws object containing the algorithm
    JwsObj = jose_jws:from(
      #{ <<"alg">> => <<"ES256">>
       %% Im not sure if it is needed
       % , <<"b64">> => true
       , <<"jwk">> => PubKeyJson
       , <<"nonce">> => list_to_bitstring(Nonce)
       }),
    % io:format("Jws: ~p~n", [JwsObj]),

    %% Signed Message
    Signed = jose_jws:sign(Key, Json, JwsObj),
    % io:format("Signed: ~p~n", [Signed]),    

    %% Peek protected
    Protected = jose_jws:peek_protected(Signed),
    % io:format("Protected: ~p~n", [jiffy:decode(Protected)]),    

    %% Peek Payload
    Payload = jose_jws:peek_payload(Signed),
    io:format("Payload: ~p~n", [jiffy:decode(Payload)]),    

    %% Verify
    % {true, _} = jose_jws:verify(Key, Signed),
    % io:format("Verify: ~p~n", [jose_jws:verify(Key, Signed)]),
    
    Signed.

scenario() ->
     % scenario_new_account().
     scenario_old_account().

scenario_old_account() ->
    {ok, Pid} = start(),
    io:format("Server started: ~p~n", [Pid]),

    {ok, Result} = directory(Pid, []),
    io:format("Directory result: ~p~n", [Result]),

    %% Get the info of an existing account
    % {ok, {Status1, Head1, Body1}} = account_info(Pid, ?DEFAULT_ACCOUNT),
    % io:format("Account: ~p~nHead: ~p~nBody: ~p~n", 
    %     [?DEFAULT_ACCOUNT, {Status1, Head1}, jiffy:decode(Body1)]),

    %% Update the account to agree to terms and services
    {ok, {Status1, Head1, Body1}} = update_account(Pid, ?DEFAULT_ACCOUNT),
    io:format("Account: ~p~nHead: ~p~nBody: ~p~n", 
        [?DEFAULT_ACCOUNT, {Status1, Head1}, jiffy:decode(Body1)]),

    %% New authorization
    % {ok, {Status2, Head2, Body2}} = new_authz(Pid, []),
    % io:format("New Authz~nHead: ~p~nBody: ~p~n", 
    %     [{Status2, Head2}, jiffy:decode(Body2)]), 

    %% Get authorization
    {ok, {Status2, Head2, Body2}} = get_authz(Pid, []),
    io:format("Get Authz~nHead: ~p~nBody: ~p~n", 
        [{Status2, Head2}, jiffy:decode(Body2)]), 

    % Challenges = get_challenges(jiffy:decode(Body2)),
    % % io:format("Challenges: ~p~n", [Challenges]),

    % ChallengeObjects = acme_challenge:challenges_to_objects(Challenges),
    % % io:format("Challenges: ~p~n", [ChallengeObjects]),

    % %% Create a key-authorization
    % Key = jose_jwk:from_file(?DEFAULT_KEY_FILE),
    % % acme_challenge:key_authorization(<<"pipi">>, Key),

    % Solutions = acme_challenge:solve_challenges(ChallengeObjects, Key),
    % io:format("Solutions: ~p~n", [Solutions]),

    % {ok, {Status3, Head3, Body3}} = 
    %     complete_challenge(Pid, [X || X <- Solutions, X =/= ok]),
    % io:format("Complete_challenge~nHead: ~p~nBody: ~p~n", 
    %     [{Status3, Head3}, jiffy:decode(Body3)]), 

    %% New certification
    % {ok, {Status2, Head2, Body2}} = new_cert(Pid, []),
    % io:format("New Cert~nHead: ~p~nBody: ~p~n", 
    %     [{Status2, Head2}, jiffy:decode(Body2)]),    
    ok. 

scenario_new_account() ->
    {ok, Pid} = start(),
    io:format("Server started: ~p~n", [Pid]),

    {ok, Result} = directory(Pid, []),
    io:format("Directory result: ~p~n", [Result]),

    %% Request the creation of a new account
    {ok, {Status, Head, Body}} = new_reg(Pid, []),
    io:format("New account~nHead: ~p~nBody: ~p~n", [{Status, Head}, jiffy:decode(Body)]).