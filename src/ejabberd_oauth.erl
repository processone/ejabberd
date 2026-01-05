%%%-------------------------------------------------------------------
%%% File    : ejabberd_oauth.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : OAUTH2 support
%%% Created : 20 Mar 2015 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module(ejabberd_oauth).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-export([start_link/0,
         get_client_identity/2,
         verify_redirection_uri/3,
         authenticate_user/2,
         authenticate_client/2,
         associate_access_code/3,
         associate_access_token/3,
         associate_refresh_token/3,
         check_token/1,
         check_token/4,
         check_token/2,
         scope_in_scope_list/2,
         process/2,
         config_reloaded/0,
         verify_resowner_scope/3]).

-export([get_commands_spec/0,
         oauth_issue_token/3, oauth_list_tokens/0, oauth_revoke_token/1,
         oauth_add_client_password/3,
         oauth_add_client_implicit/3,
         oauth_remove_client/1]).

-export([web_menu_main/2, web_page_main/2]).
-export([web_menu_system/3]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include("ejabberd_oauth.hrl").
-include("ejabberd_commands.hrl").
-include("translate.hrl").

-callback init() -> any().
-callback store(#oauth_token{}) -> ok | {error, any()}.
-callback lookup(binary()) -> {ok, #oauth_token{}} | error.
-callback revoke(binary()) -> ok | {error, binary()}.
-callback clean(non_neg_integer()) -> any().

-record(oauth_ctx, {
          password    :: binary() | admin_generated,
          client      :: #oauth_client{} | undefined
         }).

%% There are two ways to obtain an oauth token:
%%   * Using the web form/api results in the token being generated in behalf of the user providing the user/pass
%%   * Using the command line and oauth_issue_token command, the token is generated in behalf of ejabberd' sysadmin
%%    (as it has access to ejabberd command line).

get_commands_spec() ->
    [
     #ejabberd_commands{name = oauth_issue_token, tags = [oauth],
                        desc = "Issue an OAuth token for the given jid",
                        module = ?MODULE, function = oauth_issue_token,
                        args = [{jid, string},{ttl, integer}, {scopes, string}],
                        policy = restricted,
                        args_example = ["user@server.com", 3600, "connected_users_number;muc_online_rooms"],
                        args_desc = ["Jid for which issue token",
				     "Time to live of generated token in seconds",
				     "List of scopes to allow, separated by ';'"],
                        result = {result, {tuple, [{token, string}, {scopes, string}, {expires_in, string}]}}
                       },
     #ejabberd_commands{name = oauth_issue_token, tags = [oauth],
                        desc = "Issue an OAuth token for the given jid",
                        module = ?MODULE, function = oauth_issue_token,
                        version = 1,
                        note = "updated in 24.02",
                        args = [{jid, string}, {ttl, integer}, {scopes, {list, {scope, binary}}}],
                        policy = restricted,
                        args_example = ["user@server.com", 3600, ["connected_users_number", "muc_online_rooms"]],
                        args_desc = ["Jid for which issue token",
				     "Time to live of generated token in seconds",
				     "List of scopes to allow"],
                        result = {result, {tuple, [{token, string}, {scopes, {list, {scope, string}}}, {expires_in, string}]}}
                       },
     #ejabberd_commands{name = oauth_list_tokens, tags = [oauth],
                        desc = "List OAuth tokens, user, scope, and seconds to expire (only Mnesia)",
                        longdesc = "List  _`oauth.md|OAuth`_ tokens, their user and scope, and how many seconds remain until expiry",
                        module = ?MODULE, function = oauth_list_tokens,
                        args = [],
                        policy = restricted,
                        result = {tokens, {list, {token, {tuple, [{token, string}, {user, string}, {scope, string}, {expires_in, string}]}}}}
                       },
     #ejabberd_commands{name = oauth_revoke_token, tags = [oauth],
                        desc = "Revoke authorization for an OAuth token",
			note = "changed in 22.05",
                        module = ?MODULE, function = oauth_revoke_token,
                        args = [{token, binary}],
                        policy = restricted,
                        result = {res, restuple},
                        result_desc = "Result code"
                       },
     #ejabberd_commands{name = oauth_add_client_password, tags = [oauth],
                        desc = "Add OAuth client_id with password grant type",
                        module = ?MODULE, function = oauth_add_client_password,
                        args = [{client_id, binary},
                                {client_name, binary},
                                {secret, binary}],
                        policy = restricted,
                        result = {res, restuple}
                       },
     #ejabberd_commands{name = oauth_add_client_implicit, tags = [oauth],
                        desc = "Add OAuth client_id with implicit grant type",
                        module = ?MODULE, function = oauth_add_client_implicit,
                        args = [{client_id, binary},
                                {client_name, binary},
                                {redirect_uri, binary}],
                        policy = restricted,
                        result = {res, restuple}
                       },
     #ejabberd_commands{name = oauth_remove_client, tags = [oauth],
                        desc = "Remove OAuth client_id",
                        module = ?MODULE, function = oauth_remove_client,
                        args = [{client_id, binary}],
                        policy = restricted,
                        result = {res, restuple}
                       }
    ].

oauth_issue_token(Jid, TTLSeconds, [Head|_] = ScopesString) when is_integer(Head) ->
    Scopes = [list_to_binary(Scope) || Scope <- string:tokens(ScopesString, ";")],
    oauth_issue_token(Jid, TTLSeconds, Scopes);
oauth_issue_token(Jid, TTLSeconds, Scopes) ->
    try jid:decode(list_to_binary(Jid)) of
        #jid{luser =Username, lserver = Server} ->
            Ctx1 = #oauth_ctx{password = admin_generated},
            case oauth2:authorize_password({Username, Server}, Scopes, Ctx1) of
                {ok, {_Ctx,Authorization}} ->
                    {ok, {_AppCtx2, Response}} = oauth2:issue_token(Authorization, [{expiry_time, TTLSeconds}]),
		    {ok, AccessToken} = oauth2_response:access_token(Response),
		    {ok, VerifiedScope} = oauth2_response:scope(Response),
                    {AccessToken, VerifiedScope, integer_to_list(TTLSeconds) ++ " seconds"};
		{error, Error} ->
		    {error, Error}
            end
    catch _:{bad_jid, _} ->
            {error, "Invalid JID: " ++ Jid}
    end.

oauth_list_tokens() ->
    oauth_list_tokens(get_db_backend()).

oauth_list_tokens(ejabberd_oauth_mnesia) ->
    Tokens = mnesia:dirty_match_object(#oauth_token{_ = '_'}),
    {MegaSecs, Secs, _MiniSecs} = os:timestamp(),
    TS = 1000000 * MegaSecs + Secs,
    [{Token, jid:encode(jid:make(U,S)), Scope, integer_to_list(Expires - TS) ++ " seconds"} ||
        #oauth_token{token=Token, scope=Scope, us= {U,S},expire=Expires} <- Tokens];
oauth_list_tokens(DBMod) ->
    ?ERROR_MSG("Command oauth_list_tokens not implemented for database backend ~p", [DBMod]),
    [].

oauth_revoke_token(Token) ->
    DBMod = get_db_backend(),
    case DBMod:revoke(Token) of
        ok ->
            ets_cache:delete(oauth_cache, Token,
                             ejabberd_cluster:get_nodes()),
            {ok, ""};
        Other ->
            Other
    end.

oauth_add_client_password(ClientID, ClientName, Secret) ->
    DBMod = get_db_backend(),
    DBMod:store_client(#oauth_client{client_id = ClientID,
                                     client_name = ClientName,
                                     grant_type = password,
                                     options = [{secret, Secret}]}),
    {ok, []}.

oauth_add_client_implicit(ClientID, ClientName, RedirectURI) ->
    DBMod = get_db_backend(),
    DBMod:store_client(#oauth_client{client_id = ClientID,
                                     client_name = ClientName,
                                     grant_type = implicit,
                                     options = [{redirect_uri, RedirectURI}]}),
    {ok, []}.

oauth_remove_client(Client) ->
    DBMod = get_db_backend(),
    DBMod:remove_client(Client),
    {ok, []}.

config_reloaded() ->
    DBMod = get_db_backend(),
    case init_cache(DBMod) of
	true ->
	    ets_cache:setopts(oauth_cache, cache_opts());
	false ->
	    ok
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    DBMod = get_db_backend(),
    DBMod:init(),
    init_cache(DBMod),
    Expire = expire(),
    application:set_env(oauth2, backend, ejabberd_oauth),
    application:set_env(oauth2, expiry_time, Expire div 1000),
    application:start(oauth2),
    ejabberd_commands:register_commands(get_commands_spec()),
    ejabberd_hooks:add(webadmin_menu_main, ?MODULE, web_menu_main, 50),
    ejabberd_hooks:add(webadmin_page_main, ?MODULE, web_page_main, 50),
    ejabberd_hooks:add(webadmin_menu_system_post, ?MODULE, web_menu_system, 889),
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 50),
    erlang:send_after(expire(), self(), clean),
    {ok, ok}.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(clean, State) ->
    {MegaSecs, Secs, MiniSecs} = os:timestamp(),
    TS = 1000000 * MegaSecs + Secs,
    DBMod = get_db_backend(),
    DBMod:clean(TS),
    erlang:send_after(trunc(expire() * (1 + MiniSecs / 1000000)),
                      self(), clean),
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_hooks:delete(webadmin_menu_main, ?MODULE, web_menu_main, 50),
    ejabberd_hooks:delete(webadmin_page_main, ?MODULE, web_page_main, 50),
    ejabberd_hooks:delete(webadmin_menu_system_post, ?MODULE, web_menu_system, 889),
    ejabberd_hooks:delete(config_reloaded, ?MODULE, config_reloaded, 50).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

get_client_identity(<<"">>, Ctx) ->
    {ok, {Ctx, {client, unknown_client}}};
get_client_identity(ClientID, Ctx) when is_binary(ClientID) ->
    {ok, {Ctx, {client, ClientID}}}.

verify_redirection_uri(_ClientID, RedirectURI, Ctx) ->
    case Ctx of
        #oauth_ctx{client = #oauth_client{grant_type = implicit} = Client} ->
            case get_redirect_uri(Client) of
                RedirectURI ->
                    {ok, Ctx};
                _ ->
                    {error, invalid_uri}
            end;
        #oauth_ctx{client = #oauth_client{}} ->
            {error, invalid_client};
        _ ->
            {ok, Ctx}
    end.

authenticate_user({User, Server}, Ctx) ->
    case jid:make(User, Server) of
        #jid{} = JID ->
            Access =
                ejabberd_option:oauth_access(JID#jid.lserver),
            case acl:match_rule(JID#jid.lserver, Access, JID) of
                allow ->
                    case Ctx of
                        #oauth_ctx{password = admin_generated} ->
                            {ok, {Ctx, {user, User, Server}}};
                        #oauth_ctx{password = Password}
                        when is_binary(Password) ->
                            case ejabberd_auth:check_password(User, <<"">>, Server, Password) of
                                true ->
                                    {ok, {Ctx, {user, User, Server}}};
                                false ->
                                    {error, badpass}
                            end
                    end;
                deny ->
                    {error, badpass}
            end;
        error ->
            {error, badpass}
    end.

authenticate_client(ClientID, Ctx) ->
    case ejabberd_option:oauth_client_id_check() of
        allow ->
            {ok, {Ctx, {client, ClientID}}};
        deny -> {error, not_allowed};
        db ->
            DBMod = get_db_backend(),
            case DBMod:lookup_client(ClientID) of
                {ok, #oauth_client{} = Client} ->
                    {ok, {Ctx#oauth_ctx{client = Client}, {client, ClientID}}};
                _ ->
                    {error, not_allowed}
            end
    end.

-spec verify_resowner_scope({user, binary(), binary()}, [binary()], any()) ->
    {ok, any(), [binary()]} | {error, any()}.
verify_resowner_scope({user, _User, _Server}, Scope, Ctx) ->
    Cmds = [atom_to_binary(Name, utf8) || {Name, _, _} <- ejabberd_commands:list_commands()],
    AllowedScopes = [<<"ejabberd:user">>, <<"ejabberd:admin">>, <<"sasl_auth">>] ++ Cmds,
    case oauth2_priv_set:is_subset(oauth2_priv_set:new(Scope),
                                   oauth2_priv_set:new(AllowedScopes)) of
        true ->
            {ok, {Ctx, Scope}};
        false ->
            {error, badscope}
    end;
verify_resowner_scope(_, _, _) ->
    {error, badscope}.

%% This is callback for oauth tokens generated through the command line.  Only open and admin commands are
%% made available.
%verify_client_scope({client, ejabberd_ctl}, Scope, Ctx) ->
%    RegisteredScope = dict:fetch_keys(get_cmd_scopes()),
%    case oauth2_priv_set:is_subset(oauth2_priv_set:new(Scope),
%                                   oauth2_priv_set:new(RegisteredScope)) of
%        true ->
%            {ok, {Ctx, Scope}};
%        false ->
%            {error, badscope}
%    end.




-spec seconds_since_epoch(integer()) -> non_neg_integer().
seconds_since_epoch(Diff) ->
    {Mega, Secs, _} = os:timestamp(),
    Mega * 1000000 + Secs + Diff.


associate_access_code(_AccessCode, _Context, AppContext) ->
    %put(?ACCESS_CODE_TABLE, AccessCode, Context),
    {ok, AppContext}.

associate_access_token(AccessToken, Context, AppContext) ->
    {user, User, Server} = proplists:get_value(<<"resource_owner">>, Context, <<"">>),
    Expire = case proplists:get_value(expiry_time, AppContext, undefined) of
        undefined ->
            proplists:get_value(<<"expiry_time">>, Context, 0);
        ExpiresIn ->
            %% There is no clean way in oauth2 lib to actually override the TTL of the generated token.
            %% It always pass the global configured value.  Here we use the app context to pass the per-case
            %% ttl if we want to override it.
            seconds_since_epoch(ExpiresIn)
                           end,
    {user, User, Server} = proplists:get_value(<<"resource_owner">>, Context, <<"">>),
    Scope = proplists:get_value(<<"scope">>, Context, []),
    R = #oauth_token{
      token = AccessToken,
      us = {jid:nodeprep(User), jid:nodeprep(Server)},
      scope = Scope,
      expire = Expire
     },
    store(R),
    {ok, AppContext}.

associate_refresh_token(_RefreshToken, _Context, AppContext) ->
    %put(?REFRESH_TOKEN_TABLE, RefreshToken, Context),
    {ok, AppContext}.

scope_in_scope_list(Scope, ScopeList) ->
    TokenScopeSet = oauth2_priv_set:new(Scope),
    lists:any(fun(Scope2) ->
        oauth2_priv_set:is_member(Scope2, TokenScopeSet) end,
              ScopeList).

-spec check_token(binary()) -> {ok, {binary(), binary()}, [binary()]} |
			       {false, expired | not_found}.
check_token(Token) ->
    case lookup(Token) of
        {ok, #oauth_token{us = US,
                          scope = TokenScope,
                          expire = Expire}} ->
            {MegaSecs, Secs, _} = os:timestamp(),
            TS = 1000000 * MegaSecs + Secs,
            if
                Expire > TS ->
                    {ok, US, TokenScope};
                true ->
                    {false, expired}
            end;
        _ ->
            {false, not_found}
    end.

check_token(User, Server, ScopeList, Token) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    case lookup(Token) of
        {ok, #oauth_token{us = {LUser, LServer},
                      scope = TokenScope,
                          expire = Expire}} ->
            {MegaSecs, Secs, _} = os:timestamp(),
            TS = 1000000 * MegaSecs + Secs,
            if
                Expire > TS ->
                    TokenScopeSet = oauth2_priv_set:new(TokenScope),
                    lists:any(fun(Scope) ->
                                      oauth2_priv_set:is_member(Scope, TokenScopeSet) end,
                              ScopeList);
                true ->
                    {false, expired}
            end;
        _ ->
            {false, not_found}
    end.

check_token(ScopeList, Token) ->
    case lookup(Token) of
        {ok, #oauth_token{us = US,
                      scope = TokenScope,
                          expire = Expire}} ->
            {MegaSecs, Secs, _} = os:timestamp(),
            TS = 1000000 * MegaSecs + Secs,
            if
                Expire > TS ->
                    TokenScopeSet = oauth2_priv_set:new(TokenScope),
                    case lists:any(fun(Scope) ->
                                           oauth2_priv_set:is_member(Scope, TokenScopeSet) end,
                                   ScopeList) of
                        true -> {ok, user, US};
                        false -> {false, no_matching_scope}
                        end;
                true ->
                    {false, expired}
            end;
        _ ->
            {false, not_found}
    end.


store(R) ->
    DBMod = get_db_backend(),
    case DBMod:store(R) of
	ok ->
	    ets_cache:delete(oauth_cache, R#oauth_token.token,
			     ejabberd_cluster:get_nodes());
	{error, _} = Err ->
	    Err
    end.

lookup(Token) ->
    ets_cache:lookup(oauth_cache, Token,
		     fun() ->
			     DBMod = get_db_backend(),
			     DBMod:lookup(Token)
		     end).

-spec init_cache(module()) -> boolean().
init_cache(DBMod) ->
    UseCache = use_cache(DBMod),
    case UseCache of
	true ->
	    ets_cache:new(oauth_cache, cache_opts());
	false ->
	    ets_cache:delete(oauth_cache)
    end,
    UseCache.

use_cache(DBMod) ->
    case erlang:function_exported(DBMod, use_cache, 0) of
	true -> DBMod:use_cache();
	false -> ejabberd_option:oauth_use_cache()
    end.

cache_opts() ->
    MaxSize = ejabberd_option:oauth_cache_size(),
    CacheMissed = ejabberd_option:oauth_cache_missed(),
    LifeTime = ejabberd_option:oauth_cache_life_time(),
    [{max_size, MaxSize}, {life_time, LifeTime}, {cache_missed, CacheMissed}].

expire() ->
    ejabberd_option:oauth_expire().

-define(DIV(Class, Els),
	?XAE(<<"div">>, [{<<"class">>, Class}], Els)).
-define(INPUTID(Type, Name, Value),
	?XA(<<"input">>,
	    [{<<"type">>, Type}, {<<"name">>, Name},
	     {<<"value">>, Value}, {<<"id">>, Name}])).
-define(LABEL(ID, Els),
	?XAE(<<"label">>, [{<<"for">>, ID}], Els)).

process(_Handlers,
	#request{method = 'GET', q = Q, lang = Lang,
		 path = [_, <<"authorization_token">>]}) ->
    ResponseType = proplists:get_value(<<"response_type">>, Q, <<"">>),
    ClientId = proplists:get_value(<<"client_id">>, Q, <<"">>),
    JidEls = case proplists:get_value(<<"jid">>, Q, <<"">>) of
                <<"">> -> [?INPUTID(<<"email">>, <<"username">>, <<"">>)];
                Jid -> [?C(Jid), ?INPUT(<<"hidden">>, <<"username">>, Jid)]
            end,
    RedirectURI = proplists:get_value(<<"redirect_uri">>, Q, <<"">>),
    Scope = proplists:get_value(<<"scope">>, Q, <<"">>),
    State = proplists:get_value(<<"state">>, Q, <<"">>),
    Form =
        ?XAE(<<"form">>,
             [{<<"action">>, <<"authorization_token">>},
              {<<"method">>, <<"post">>}],
             [?LABEL(<<"username">>, [?CT(?T("User (jid)")), ?C(<<": ">>)])
             ] ++ JidEls ++ [
              ?BR,
              ?LABEL(<<"password">>, [?CT(?T("Password")), ?C(<<": ">>)]),
              ?INPUTID(<<"password">>, <<"password">>, <<"">>),
              ?INPUT(<<"hidden">>, <<"response_type">>, ResponseType),
              ?INPUT(<<"hidden">>, <<"client_id">>, ClientId),
              ?INPUT(<<"hidden">>, <<"redirect_uri">>, RedirectURI),
              ?INPUT(<<"hidden">>, <<"scope">>, Scope),
              ?INPUT(<<"hidden">>, <<"state">>, State),
              ?BR,
              ?LABEL(<<"ttl">>, [?CT(?T("Token TTL")), ?C(<<": ">>)]),
              ?XAE(<<"select">>, [{<<"name">>, <<"ttl">>}],
                   [
                   ?XAC(<<"option">>, [{<<"value">>, <<"3600">>}],<<"1 Hour">>),
                   ?XAC(<<"option">>, [{<<"value">>, <<"86400">>}],<<"1 Day">>),
                   ?XAC(<<"option">>, [{<<"value">>, <<"2592000">>}],<<"1 Month">>),
                   ?XAC(<<"option">>, [{<<"selected">>, <<"selected">>},{<<"value">>, <<"31536000">>}],<<"1 Year">>),
                   ?XAC(<<"option">>, [{<<"value">>, <<"315360000">>}],<<"10 Years">>)]),
              ?BR,
              ?INPUTT(<<"submit">>, <<"">>, ?T("Accept"))
             ]),
    Top =
        ?DIV(<<"section">>,
             [?DIV(<<"block">>,
                   [?A(<<"https://www.ejabberd.im">>,
                       [?XA(<<"img">>,
                            [{<<"height">>, <<"32">>},
                             {<<"src">>, logo()}])]
                      )])]),
    Middle =
        ?DIV(<<"white section">>,
             [?DIV(<<"block">>,
                   [?XC(<<"h1">>, <<"Authorization request">>),
                    ?XE(<<"p">>,
                        [?C(<<"Application ">>),
                         ?XC(<<"em">>, ClientId),
                         ?C(<<" wants to access scope ">>),
                         ?XC(<<"em">>, Scope)]),
                    Form
                   ])]),
    Bottom =
        ?DIV(<<"section">>,
             [?DIV(<<"block">>,
                   [?XAC(<<"a">>,
                         [{<<"href">>, <<"https://www.ejabberd.im">>},
                          {<<"title">>, <<"ejabberd XMPP server">>}],
                         <<"ejabberd">>),
                    ?C(<<" is maintained by ">>),
                    ?XAC(<<"a">>,
                         [{<<"href">>, <<"https://www.process-one.net">>},
                          {<<"title">>, <<"ProcessOne - Leader in Instant Messaging and Push Solutions">>}],
                         <<"ProcessOne">>)
                   ])]),
    Body = ?DIV(<<"container">>, [Top, Middle, Bottom]),
    ejabberd_web:make_xhtml(web_head(), [Body]);
process(_Handlers,
	#request{method = 'POST', q = Q, lang = _Lang,
		 path = [_, <<"authorization_token">>]}) ->
    _ResponseType = proplists:get_value(<<"response_type">>, Q, <<"">>),
    ClientId = proplists:get_value(<<"client_id">>, Q, <<"">>),
    RedirectURI = proplists:get_value(<<"redirect_uri">>, Q, <<"">>),
    SScope = proplists:get_value(<<"scope">>, Q, <<"">>),
    StringJID = proplists:get_value(<<"username">>, Q, <<"">>),
    try jid:decode(StringJID) of
        #jid{user = Username, server = Server} ->
            Password = proplists:get_value(<<"password">>, Q, <<"">>),
            State = proplists:get_value(<<"state">>, Q, <<"">>),
            Scope = str:tokens(SScope, <<" ">>),
            TTL = proplists:get_value(<<"ttl">>, Q, <<"">>),
            ExpiresIn = case TTL of
                            <<>> -> undefined;
                            _ -> binary_to_integer(TTL)
                        end,
            case oauth2:authorize_password({Username, Server},
                                           ClientId,
                                           RedirectURI,
                                           Scope,
                                           #oauth_ctx{password = Password}) of
                {ok, {_AppContext, Authorization}} ->
                    {ok, {_AppContext2, Response}} =
                        oauth2:issue_token(Authorization, [{expiry_time, ExpiresIn} || ExpiresIn /= undefined]),
                    {ok, AccessToken} = oauth2_response:access_token(Response),
                    {ok, Type} = oauth2_response:token_type(Response),
                    %%Ugly: workardound to return the correct expirity time, given than oauth2 lib doesn't really have
                    %%per-case expirity time.
                    Expires = case ExpiresIn of
                                  undefined ->
                                      {ok, Ex} = oauth2_response:expires_in(Response),
                                      Ex;
                                  _ ->
                                      ExpiresIn
                              end,
                    {ok, VerifiedScope} = oauth2_response:scope(Response),
                    %oauth2_wrq:redirected_access_token_response(ReqData,
                    %                                            RedirectURI,
                    %                                            AccessToken,
                    %                                            Type,
                    %                                            Expires,
                    %                                            VerifiedScope,
                    %                                            State,
                    %                                            Context);
                    {302, [{<<"Location">>,
                            <<RedirectURI/binary,
                              "?access_token=", AccessToken/binary,
                              "&token_type=", Type/binary,
                              "&expires_in=", (integer_to_binary(Expires))/binary,
                              "&scope=", (str:join(VerifiedScope, <<" ">>))/binary,
                              "&state=", State/binary>>
                           }],
                     ejabberd_web:make_xhtml([?XC(<<"h1">>, <<"302 Found">>)])};
                {error, Error} when is_atom(Error) ->
                    %oauth2_wrq:redirected_error_response(
                    %    ReqData, RedirectURI, Error, State, Context)
                    {302, [{<<"Location">>,
                            <<RedirectURI/binary,
                              "?error=", (atom_to_binary(Error, utf8))/binary,
                              "&state=", State/binary>>
                           }],
                     ejabberd_web:make_xhtml([?XC(<<"h1">>, <<"302 Found">>)])}
            end
    catch _:{bad_jid, _} ->
        State = proplists:get_value(<<"state">>, Q, <<"">>),
        {400, [{<<"Location">>,
                <<RedirectURI/binary,
                  "?error=invalid_request",
                  "&state=", State/binary>>
               }],
         ejabberd_web:make_xhtml([?XC(<<"h1">>, <<"400 Invalid request">>)])}
    end;
process(_Handlers,
	#request{method = 'POST', q = Q, lang = _Lang,
                 auth = HTTPAuth,
		 path = [_, <<"token">>]}) ->
    Access =
        case ejabberd_option:oauth_client_id_check() of
            allow ->
                case proplists:get_value(<<"grant_type">>, Q, <<"">>) of
                    <<"password">> ->
                        password;
                    _ ->
                        unsupported_grant_type
                end;
            deny ->
                deny;
            db ->
                {ClientID, Secret} =
                    case HTTPAuth of
                        {ClientID1, Secret1} ->
                            {ClientID1, Secret1};
                        _ ->
                            ClientID1 = proplists:get_value(
                                          <<"client_id">>, Q, <<"">>),
                            Secret1 = proplists:get_value(
                                        <<"client_secret">>, Q, <<"">>),
                            {ClientID1, Secret1}
                    end,
                DBMod = get_db_backend(),
                case DBMod:lookup_client(ClientID) of
                    {ok, #oauth_client{grant_type = password} = Client} ->
                        case get_client_secret(Client) of
                            Secret ->
                                case proplists:get_value(<<"grant_type">>, Q, <<"">>) of
                                    <<"password">> when
                                    Client#oauth_client.grant_type == password ->
                                        password;
                                    _ ->
                                        unsupported_grant_type
                                end;
                            _ ->
                                deny
                        end;
                    _ ->
                        deny
                end
        end,
    case Access of
        password ->
            SScope = proplists:get_value(<<"scope">>, Q, <<"">>),
            StringJID = proplists:get_value(<<"username">>, Q, <<"">>),
            try jid:decode(StringJID) of
                #jid{user = Username, server = Server} ->
                    Password = proplists:get_value(<<"password">>, Q, <<"">>),
                    Scope = str:tokens(SScope, <<" ">>),
                    TTL = proplists:get_value(<<"ttl">>, Q, <<"">>),
                    ExpiresIn = case TTL of
                                    <<>> -> undefined;
                                    _ -> binary_to_integer(TTL)
                                end,
                    case oauth2:authorize_password({Username, Server},
                                                   Scope,
                                                   #oauth_ctx{password = Password}) of
                        {ok, {_AppContext, Authorization}} ->
                            {ok, {_AppContext2, Response}} =
                                oauth2:issue_token(Authorization, [{expiry_time, ExpiresIn} || ExpiresIn /= undefined]),
                            {ok, AccessToken} = oauth2_response:access_token(Response),
                            {ok, Type} = oauth2_response:token_type(Response),
                            %%Ugly: workardound to return the correct expirity time, given than oauth2 lib doesn't really have
                            %%per-case expirity time.
                            Expires = case ExpiresIn of
                                          undefined ->
                                              {ok, Ex} = oauth2_response:expires_in(Response),
                                              Ex;
                                          _ ->
                                              ExpiresIn
                                      end,
                            {ok, VerifiedScope} = oauth2_response:scope(Response),
                            json_response(200, #{<<"access_token">> => AccessToken,
                                                 <<"token_type">> => Type,
                                                 <<"scope">> => str:join(VerifiedScope, <<" ">>),
                                                 <<"expires_in">> => Expires});
                        {error, Error} when is_atom(Error) ->
                            json_error(400, <<"invalid_grant">>, Error)
                    end
            catch _:{bad_jid, _} ->
                json_error(400, <<"invalid_request">>, invalid_jid)
            end;
        unsupported_grant_type ->
            json_error(400, <<"unsupported_grant_type">>,
                       unsupported_grant_type);
        deny ->
            ejabberd_web:error(not_allowed)
    end;

process(_Handlers, _Request) ->
    ejabberd_web:error(not_found).

-spec get_db_backend() -> module().

get_db_backend() ->
    DBType = ejabberd_option:oauth_db_type(),
    list_to_existing_atom("ejabberd_oauth_" ++ atom_to_list(DBType)).

get_client_secret(#oauth_client{grant_type = password, options = Options}) ->
    proplists:get_value(secret, Options, false).

get_redirect_uri(#oauth_client{grant_type = implicit, options = Options}) ->
    proplists:get_value(redirect_uri, Options, false).

%% Headers as per RFC 6749
json_response(Code, Body) ->
    {Code, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>},
           {<<"Cache-Control">>, <<"no-store">>},
           {<<"Pragma">>, <<"no-cache">>}],
     misc:json_encode(Body)}.

%% OAauth error are defined in:
%% https://tools.ietf.org/html/draft-ietf-oauth-v2-25#section-5.2
json_error(Code, Error, Reason) ->
    Desc = json_error_desc(Reason),
    Body = #{<<"error">> => Error,
             <<"error_description">> => Desc},
    json_response(Code, Body).

json_error_desc(access_denied)          -> <<"Access denied">>;
json_error_desc(badpass)                -> <<"Bad password">>;
json_error_desc(unsupported_grant_type) -> <<"Unsupported grant type">>;
json_error_desc(invalid_scope)          -> <<"Invalid scope">>;
json_error_desc(invalid_jid)            -> <<"Invalid JID">>.

web_head() ->
    [?XA(<<"meta">>, [{<<"http-equiv">>, <<"X-UA-Compatible">>},
                      {<<"content">>, <<"IE=edge">>}]),
     ?XA(<<"meta">>, [{<<"name">>, <<"viewport">>},
                      {<<"content">>,
                       <<"width=device-width, initial-scale=1">>}]),
     ?XC(<<"title">>, <<"Authorization request">>),
     ?XC(<<"style">>, css())
    ].

css() ->
    case misc:read_css("oauth.css") of
	{ok, Data} -> Data;
	{error, _} -> <<>>
    end.

logo() ->
    case misc:read_img("oauth-logo.png") of
	{ok, Img} ->
	    B64Img = base64:encode(Img),
	    <<"data:image/png;base64,", B64Img/binary>>;
	{error, _} ->
	    <<>>
    end.

%%%
%%% WebAdmin
%%%

%% @format-begin

web_menu_main(Acc, _Lang) ->
    Acc ++ [{<<"oauth">>, <<"OAuth">>}].

web_page_main(_, #request{path = [<<"oauth">>]} = R) ->
    Head = ?H1GLraw(<<"OAuth">>, <<"developer/ejabberd-api/oauth/">>, <<"OAuth">>),
    Set = [?X(<<"hr">>),
           ?XAC(<<"h2">>, [{<<"id">>, <<"token">>}], <<"Token">>),
           ?XE(<<"blockquote">>,
               [ejabberd_web_admin:make_command(oauth_list_tokens, R),
                ejabberd_web_admin:make_command(oauth_issue_token, R),
                ejabberd_web_admin:make_command(oauth_revoke_token, R)]),
           ?X(<<"hr">>),
           ?XAC(<<"h2">>, [{<<"id">>, <<"client">>}], <<"Client">>),
           ?XE(<<"blockquote">>,
               [ejabberd_web_admin:make_command(oauth_add_client_implicit, R),
                ejabberd_web_admin:make_command(oauth_add_client_password, R),
                ejabberd_web_admin:make_command(oauth_remove_client, R)])],
    {stop, Head ++ Set};
web_page_main(Acc, _) ->
    Acc.

web_menu_system(Result, _Request, _Level) ->
    Els = ejabberd_web_admin:make_menu_system(?MODULE, "⚫", "OAuth", "authorization_token"),
    Els ++ Result.
