-module(mod_acme).

-behaviour(gen_server).

%% API
-export([ start/0
        , stop/1
        %% I tried to follow the naming convention found in the acme spec
        , directory/2
        , new_nonce/2
        %% Account
        , new_account/2
        , update_account/2
        , account_info/2   %% TODO: Maybe change to get_account
        , account_key_change/2
        , deactivate_account/2
        %% Orders/Certificates
        , new_order/2
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
-define(CA_URL, "https://acme-staging.api.letsencrypt.org").
% -define(CA_URL, "http://localhost:4000").

-define(DEFAULT_DIRECTORY, ?CA_URL ++ "/directory").

-define(DEFAULT_NEW_NONCE, ?CA_URL ++ "/acme/new_nonce").

-record(state, {
        ca_url  = ?CA_URL :: list(),
        dir_url = ?DEFAULT_DIRECTORY :: list(),
        dirs = maps:new(),
        nonce = ""
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

new_account(Pid, Options) ->
    gen_server:call(Pid, ?FUNCTION_NAME).

update_account(Pid, Options) ->
    ok.

account_info(Pid, Options) ->
    ok.

account_key_change(Pid, Options) ->
    ok.

deactivate_account(Pid, Options) ->
    ok.

new_order(Pid, Options) ->
    ok.

new_authz(Pid, Options) ->
    ok.

get_certificate(Pid, Options) ->
    ok.

get_authz(Pid, Options) ->
    ok.

complete_challenge(Pid, Options) ->
    ok.

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
    Result = jiffy:decode(Body),
    {Directories} = Result,
    StrDirectories = [{bitstring_to_list(X), bitstring_to_list(Y)} || 
                        {X,Y} <- Directories],

    % Find and save the replay nonce
    % io:format("Directory Head Response: ~p~n", [Head]),
    {"replay-nonce", Nonce} = proplists:lookup("replay-nonce", Head),

    %% Update the directories in state
    %% TODO: Get the merge of the old and the new dictionary
    NewDirs = maps:from_list(StrDirectories),
    % io:format("New directories: ~p~n", [NewDirs]),
    
    {reply, {ok, Result}, S#state{dirs = NewDirs, nonce = Nonce}};
handle_call(new_nonce, _From, S = #state{dirs=Dirs}) ->
    %% Get url from all directories
    #{"new_nonce" := Url} = Dirs,
    {ok, {Status, Head, []}} = 
        httpc:request(head, {Url, []}, [], []),
    {reply, {ok, {Status, Head}}, S};
handle_call(new_account, _From, S = #state{ca_url = Ca, dirs=Dirs, nonce = Nonce}) ->
    %% Get url from all directories
    #{"new-reg" := Url} = Dirs,

    %% Make the request body
    ReqBody = jiffy:encode({
    [ { <<"contact">>, 
          [ 
          <<"mailto:cert-admin@example.com">>
          ]
      }
    , { <<"resource">>, <<"new-reg">>}
    ]}),

    %% Jose
    {_, SignedBody} = sign_a_json_object_using_jose(ReqBody, Url, Nonce),
    io:format("Signed Body: ~p~n", [SignedBody]),

    %% Encode the Signed body with jiffy
    FinalBody = jiffy:encode(SignedBody),

    {ok, {Status, Head, Body}} = 
        httpc:request(post, {Url, [], "application/jose+json", FinalBody}, [], []),
    {reply, {ok, {Status, Head, Body}}, S};    
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



%% Test

sign_a_json_object_using_jose(Json, Url, Nonce) ->
    % Generate a key for now
    Key = jose_jwk:generate_key({ec, secp256r1}),
    io:format("Key: ~p~n", [Key]),

    % Generate a public key
    PubKey = jose_jwk:to_public(Key),
    io:format("Public Key: ~p~n", [PubKey]),    
    {_, BinaryPubKey} = jose_jwk:to_binary(PubKey),
    io:format("Public Key: ~p~n", [BinaryPubKey]),    
    PubKeyJson = jiffy:decode(BinaryPubKey),
    io:format("Public Key: ~p~n", [PubKeyJson]), 

    % KeyOkp = jose_jwk:to_okp(Key),
    % io:format("Key Okp: ~p~n", [KeyOkp]),

    
    % Jws object containing the algorithm
    JwsObj = jose_jws:from(
      #{ 
         % <<"alg">> => <<"HS256">>
         <<"alg">> => <<"ES256">>
       %% Im not sure if it is needed
       % , <<"b64">> => true
       , <<"jwk">> => PubKeyJson
       % , <<"url">> => list_to_bitstring(Url)
       , <<"nonce">> => list_to_bitstring(Nonce)
       }),
    io:format("Jws: ~p~n", [JwsObj]),

    % ProtectedObj = jose_jws:signing_input(Json, 
    %     #{ <<"alg">> => <<"HS256">>
    %     %% Im not sure if it is needed
    %      , <<"jwk">> => PubKeyJson
    %      , <<"url">> => Url
    %      , <<"nonce">> => Nonce
    %     }, JwsObj),
    % io:format("ProtectedObj: ~p~n", [ProtectedObj]),    

    % {Modules, ProtectedBinary} = to_binary(JwsObj),
    % io:format("ProtectedObj: ~p~n", [ProtectedObj]),    
    % Protected = base64url:encode(ProtectedBinary),
    % Payload = base64url:encode(PlainText),
    % SigningInput = signing_input(PlainText, Protected, NewJWS),
    % Signature = base64url:encode(ALGModule:sign(Key, SigningInput, NewALG)),
    % {Modules, maps:put(<<"payload">>, Payload, signature_to_map(Protected, Header, Key, Signature))};

    %% Signed Message
    Signed = jose_jws:sign(Key, Json, JwsObj),
    io:format("Signed: ~p~n", [Signed]),    

    %% Peek protected
    Protected = jose_jws:peek_protected(Signed),
    io:format("Protected: ~p~n", [jiffy:decode(Protected)]),    

    %% Peek Payload
    Payload = jose_jws:peek_payload(Signed),
    io:format("Payload: ~p~n", [jiffy:decode(Payload)]),    

    %% Verify
    io:format("Verify: ~p~n", [jose_jws:verify(Key, Signed)]),
    
    % %% To binary
    % Binary = jose_jws:to_binary(Signed),
    % io:format("Binary: ~p~n", [jose_jws:to_binary(Signed)]),

    % %% To map
    % Map = jose_jws:to_map(Signed),
    % io:format("Map: ~p~n", [jose_jws:to_map(Signed)]),
    
    Signed.

scenario() ->
    {ok, Pid} = start(),
    io:format("Server started: ~p~n", [Pid]),

    {ok, Result} = directory(Pid, []),
    io:format("Directory result: ~p~n", [Result]),

    {ok, Result1} = new_account(Pid, []),
    io:format("New account result: ~p~n", [Result1]),
    ok.  

