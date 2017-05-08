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

% -define(CA_URL, "https://acme-v01.api.letsencrypt.org").
-define(CA_URL, "https://acme-staging.api.letsencrypt.org").

-define(DEFAULT_DIRECTORY, "directory").

-define(DEFAULT_NEW_NONCE, "acme/new_nonce").


-record(dirs, {
        new_nonce = ?DEFAULT_NEW_NONCE
        }).

-record(state, {
        ca_url  = ?CA_URL :: list(),
        dir_url = ?DEFAULT_DIRECTORY :: list(),
        dirs = #dirs{} 
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
    ok.

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
    {ok, #state{}}.

handle_call(directory, _From, S = #state{ca_url = Ca, dir_url=Dir}) ->
    Url = final_url([Ca, Dir]),
    {ok, {_Status, _Head, Body}} = httpc:request(get, {Url, []}, [], []),
    Result = jiffy:decode(Body),
    {reply, {ok, Result}, S};
handle_call(new_nonce, _From, S = #state{ca_url = Ca, dirs=Dirs}) ->
    #dirs{new_nonce=New_nonce_url} = Dirs,
    Url = final_url([Ca, New_nonce_url]),
    {ok, {Status, Head, []}} = httpc:request(head, {Url, []}, [], []),
    {reply, {ok, {Status, Head}}, S};    
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


final_url(Urls) ->
    Joined = lists:join("/", Urls),
    lists:flatten(Joined).

scenario() ->
    {ok, Pid} = start(),
    io:format("Server started: ~p~n", [Pid]),

    {ok, Result} = directory(Pid, []),
    io:format("Directory result: ~p~n", [Result]),

    {ok, Result1} = new_nonce(Pid, []),
    io:format("New nonce result: ~p~n", [Result1]),
    ok.  
