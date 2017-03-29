%%%-------------------------------------------------------------------
%%% File    : ejabberd_oauth.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : OAUTH2 support
%%% Created : 20 Mar 2015 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
         verify_resowner_scope/3,
         associate_access_code/3,
         associate_access_token/3,
         associate_refresh_token/3,
         check_token/1,
         check_token/4,
         check_token/2,
         scope_in_scope_list/2,
         process/2,
         opt_type/1]).

-export([oauth_issue_token/3, oauth_list_tokens/0, oauth_revoke_token/1, oauth_list_scopes/0]).

-include("xmpp.hrl").

-include("ejabberd.hrl").
-include("logger.hrl").

-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include("ejabberd_oauth.hrl").

-include("ejabberd_commands.hrl").


%% There are two ways to obtain an oauth token:
%%   * Using the web form/api results in the token being generated in behalf of the user providing the user/pass
%%   * Using the command line and oauth_issue_token command, the token is generated in behalf of ejabberd' sysadmin
%%    (as it has access to ejabberd command line).

-define(EXPIRE, 4294967).

get_commands_spec() ->
    [
     #ejabberd_commands{name = oauth_issue_token, tags = [oauth],
                        desc = "Issue an oauth token for the given jid",
                        module = ?MODULE, function = oauth_issue_token,
                        args = [{jid, string},{ttl, integer}, {scopes, string}],
                        policy = restricted,
                        args_example = ["user@server.com", "connected_users_number;muc_online_rooms"],
                        args_desc = ["Jid for which issue token",
				     "Time to live of generated token in seconds",
				     "List of scopes to allow, separated by ';'"],
                        result = {result, {tuple, [{token, string}, {scopes, string}, {expires_in, string}]}}
                       },
     #ejabberd_commands{name = oauth_list_tokens, tags = [oauth],
                        desc = "List oauth tokens, user, scope, and seconds to expire (only Mnesia)",
                        longdesc = "List oauth tokens, their user and scope, and how many seconds remain until expirity",
                        module = ?MODULE, function = oauth_list_tokens,
                        args = [],
                        policy = restricted,
                        result = {tokens, {list, {token, {tuple, [{token, string}, {user, string}, {scope, string}, {expires_in, string}]}}}}
                       },
     #ejabberd_commands{name = oauth_list_scopes, tags = [oauth],
                        desc = "List scopes that can be granted, and commands",
                        longdesc = "List scopes that can be granted to tokens generated through the command line, together with the commands they allow",
                        module = ?MODULE, function = oauth_list_scopes,
                        args = [],
                        policy = restricted,
                        result = {scopes, {list, {scope, {tuple, [{scope, string}, {commands, string}]}}}}
                       },
     #ejabberd_commands{name = oauth_revoke_token, tags = [oauth],
                        desc = "Revoke authorization for a token (only Mnesia)",
                        module = ?MODULE, function = oauth_revoke_token,
                        args = [{token, string}],
                        policy = restricted,
                        result = {tokens, {list, {token, {tuple, [{token, string}, {user, string}, {scope, string}, {expires_in, string}]}}}},
                        result_desc = "List of remaining tokens"
                       }
    ].

oauth_issue_token(Jid, TTLSeconds, ScopesString) ->
    Scopes = [list_to_binary(Scope) || Scope <- string:tokens(ScopesString, ";")],
    try jid:decode(list_to_binary(Jid)) of
        #jid{luser =Username, lserver = Server} ->
            case oauth2:authorize_password({Username, Server},  Scopes, admin_generated) of
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
    Tokens = mnesia:dirty_match_object(#oauth_token{_ = '_'}),
    {MegaSecs, Secs, _MiniSecs} = os:timestamp(),
    TS = 1000000 * MegaSecs + Secs,
    [{Token, jid:encode(jid:make(U,S)), Scope, integer_to_list(Expires - TS) ++ " seconds"} ||
        #oauth_token{token=Token, scope=Scope, us= {U,S},expire=Expires} <- Tokens].


oauth_revoke_token(Token) ->
    ok = mnesia:dirty_delete(oauth_token, list_to_binary(Token)),
    oauth_list_tokens().

oauth_list_scopes() ->
    [ {Scope, string:join([atom_to_list(Cmd) || Cmd <- Cmds], ",")}   || {Scope, Cmds} <- dict:to_list(get_cmd_scopes())].




start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    DBMod = get_db_backend(),
    DBMod:init(),
    MaxSize =
        ejabberd_config:get_option(
          oauth_cache_size,
          fun(I) when is_integer(I), I>0 -> I end,
          1000),
    LifeTime =
        ejabberd_config:get_option(
          oauth_cache_life_time,
          fun(I) when is_integer(I), I>0 -> I end,
          timer:hours(1) div 1000),
    cache_tab:new(oauth_token,
		  [{max_size, MaxSize}, {life_time, LifeTime}]),
    Expire = expire(),
    application:set_env(oauth2, backend, ejabberd_oauth),
    application:set_env(oauth2, expiry_time, Expire),
    application:start(oauth2),
    ejabberd_commands:register_commands(get_commands_spec()),
    erlang:send_after(expire() * 1000, self(), clean),
    {ok, ok}.

handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(clean, State) ->
    {MegaSecs, Secs, MiniSecs} = os:timestamp(),
    TS = 1000000 * MegaSecs + Secs,
    DBMod = get_db_backend(),
    DBMod:clean(TS),
    erlang:send_after(trunc(expire() * 1000 * (1 + MiniSecs / 1000000)),
                      self(), clean),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


get_client_identity(Client, Ctx) -> {ok, {Ctx, {client, Client}}}.

verify_redirection_uri(_, _, Ctx) -> {ok, Ctx}.

authenticate_user({User, Server}, Ctx) ->
    case jid:make(User, Server) of
        #jid{} = JID ->
            Access =
                ejabberd_config:get_option(
                  {oauth_access, JID#jid.lserver},
		  fun(A) -> A end,
                  none),
            case acl:match_rule(JID#jid.lserver, Access, JID) of
                allow ->
                    case Ctx of
                        {password, Password} ->
                    case ejabberd_auth:check_password(User, <<"">>, Server, Password) of
                        true ->
                            {ok, {Ctx, {user, User, Server}}};
                        false ->
                            {error, badpass}
                    end;
                        admin_generated ->
                            {ok, {Ctx, {user, User, Server}}}
                    end;
                deny ->
                    {error, badpass}
            end;
        error ->
            {error, badpass}
    end.

authenticate_client(Client, Ctx) -> {ok, {Ctx, {client, Client}}}.

verify_resowner_scope({user, _User, _Server}, Scope, Ctx) ->
    Cmds = ejabberd_commands:get_exposed_commands(),
    Cmds1 = ['ejabberd:user', 'ejabberd:admin', sasl_auth | Cmds],
    RegisteredScope = [atom_to_binary(C, utf8) || C <- Cmds1],
    case oauth2_priv_set:is_subset(oauth2_priv_set:new(Scope),
                                   oauth2_priv_set:new(RegisteredScope)) of
        true ->
            {ok, {Ctx, Scope}};
        false ->
            {error, badscope}
    end;
verify_resowner_scope(_, _, _) ->
    {error, badscope}.


get_cmd_scopes() ->
    ScopeMap = lists:foldl(fun(Cmd, Accum) ->
                        case ejabberd_commands:get_command_policy_and_scope(Cmd) of
                            {ok, Policy, Scopes} when Policy =/= restricted ->
                                lists:foldl(fun(Scope, Accum2) ->
                                                    dict:append(Scope, Cmd, Accum2)
                                            end, Accum, Scopes);
                            _ -> Accum
                        end end, dict:new(), ejabberd_commands:get_exposed_commands()),
    ScopeMap.

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
    cache_tab:insert(
      oauth_token, R#oauth_token.token, R,
      fun() ->
              DBMod = get_db_backend(),
              DBMod:store(R)
      end).

lookup(Token) ->
    cache_tab:lookup(
      oauth_token, Token,
      fun() ->
              DBMod = get_db_backend(),
              case DBMod:lookup(Token) of
                  #oauth_token{} = R -> {ok, R};
                  _ -> error
              end
      end).


expire() ->
    ejabberd_config:get_option(
      oauth_expire,
      fun(I) when is_integer(I) -> I end,
      ?EXPIRE).

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
    RedirectURI = proplists:get_value(<<"redirect_uri">>, Q, <<"">>),
    Scope = proplists:get_value(<<"scope">>, Q, <<"">>),
    State = proplists:get_value(<<"state">>, Q, <<"">>),
    Form =
        ?XAE(<<"form">>,
             [{<<"action">>, <<"authorization_token">>},
              {<<"method">>, <<"post">>}],
             [?LABEL(<<"username">>, [?CT(<<"User (jid)">>), ?C(<<": ">>)]),
              ?INPUTID(<<"text">>, <<"username">>, <<"">>),
              ?BR,
              ?LABEL(<<"password">>, [?CT(<<"Password">>), ?C(<<": ">>)]),
              ?INPUTID(<<"password">>, <<"password">>, <<"">>),
              ?INPUT(<<"hidden">>, <<"response_type">>, ResponseType),
              ?INPUT(<<"hidden">>, <<"client_id">>, ClientId),
              ?INPUT(<<"hidden">>, <<"redirect_uri">>, RedirectURI),
              ?INPUT(<<"hidden">>, <<"scope">>, Scope),
              ?INPUT(<<"hidden">>, <<"state">>, State),
              ?BR,
              ?LABEL(<<"ttl">>, [?CT(<<"Token TTL">>), ?CT(<<": ">>)]),
              ?XAE(<<"select">>, [{<<"name">>, <<"ttl">>}],
                   [
                   ?XAC(<<"option">>, [{<<"value">>, <<"3600">>}],<<"1 Hour">>),
                   ?XAC(<<"option">>, [{<<"value">>, <<"86400">>}],<<"1 Day">>),
                   ?XAC(<<"option">>, [{<<"value">>, <<"2592000">>}],<<"1 Month">>),
                   ?XAC(<<"option">>, [{<<"selected">>, <<"selected">>},{<<"value">>, <<"31536000">>}],<<"1 Year">>),
                   ?XAC(<<"option">>, [{<<"value">>, <<"315360000">>}],<<"10 Years">>)]),
              ?BR,
              ?INPUTT(<<"submit">>, <<"">>, <<"Accept">>)
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
    #jid{user = Username, server = Server} = jid:decode(StringJID),
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
                                   {password, Password}) of
        {ok, {_AppContext, Authorization}} ->
            {ok, {_AppContext2, Response}} =
                oauth2:issue_token(Authorization, [{expiry_time, ExpiresIn} || ExpiresIn /= undefined ]),
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
    end;
process(_Handlers,
	#request{method = 'POST', q = Q, lang = _Lang,
		 path = [_, <<"token">>]}) ->
    case proplists:get_value(<<"grant_type">>, Q, <<"">>) of
      <<"password">> ->
        SScope = proplists:get_value(<<"scope">>, Q, <<"">>),
        StringJID = proplists:get_value(<<"username">>, Q, <<"">>),
        #jid{user = Username, server = Server} = jid:decode(StringJID),
        Password = proplists:get_value(<<"password">>, Q, <<"">>),
        Scope = str:tokens(SScope, <<" ">>),
        TTL = proplists:get_value(<<"ttl">>, Q, <<"">>),
        ExpiresIn = case TTL of
                        <<>> -> undefined;
                        _ -> binary_to_integer(TTL)
                    end,
        case oauth2:authorize_password({Username, Server},
                                       Scope,
                                       {password, Password}) of
            {ok, {_AppContext, Authorization}} ->
                {ok, {_AppContext2, Response}} =
                    oauth2:issue_token(Authorization, [{expiry_time, ExpiresIn} || ExpiresIn /= undefined ]),
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
                json_response(200, {[
                   {<<"access_token">>, AccessToken},
                   {<<"token_type">>, Type},
                   {<<"scope">>, str:join(VerifiedScope, <<" ">>)},
                   {<<"expires_in">>, Expires}]});
            {error, Error} when is_atom(Error) ->
                json_error(400, <<"invalid_grant">>, Error)
        end;
        _OtherGrantType ->
            json_error(400, <<"unsupported_grant_type">>, unsupported_grant_type)
  end;

process(_Handlers, _Request) ->
    ejabberd_web:error(not_found).

-spec get_db_backend() -> module().

get_db_backend() ->
    DBType = ejabberd_config:get_option(
               oauth_db_type,
               fun(T) -> ejabberd_config:v_db(?MODULE, T) end,
               mnesia),
    list_to_atom("ejabberd_oauth_" ++ atom_to_list(DBType)).


%% Headers as per RFC 6749
json_response(Code, Body) ->
    {Code, [{<<"Content-Type">>, <<"application/json;charset=UTF-8">>},
           {<<"Cache-Control">>, <<"no-store">>},
           {<<"Pragma">>, <<"no-cache">>}],
     jiffy:encode(Body)}.

%% OAauth error are defined in:
%% https://tools.ietf.org/html/draft-ietf-oauth-v2-25#section-5.2
json_error(Code, Error, Reason) ->
    Desc = json_error_desc(Reason),
    Body = {[{<<"error">>, Error},
             {<<"error_description">>, Desc}]},
    json_response(Code, Body).

json_error_desc(access_denied)          -> <<"Access denied">>;
json_error_desc(unsupported_grant_type) -> <<"Unsupported grant type">>;
json_error_desc(invalid_scope)          -> <<"Invalid scope">>.

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
    <<"
      body {
        margin: 0;
        padding: 0;

        font-family: sans-serif;
        color: #fff;
      }

      h1 {
        font-size: 3em;
        color: #444;
      }

      p {
        line-height: 1.5em;
        color: #888;
      }

      a {
        color: #fff;
      }
        a:hover,
        a:active {
          text-decoration: underline;
        }

      em {
        display: inline-block;
        padding: 0 5px;

        background: #f4f4f4;
        border-radius: 5px;

        font-style: normal;
        font-weight: bold;
        color: #444;
      }

      form {
        color: #444;
      }
        label {
          display: block;
          font-weight: bold;
        }

        input[type=text],
        input[type=password] {
          margin-bottom: 1em;
          padding: 0.4em;

          max-width: 330px;
          width: 100%;

          border: 1px solid #c4c4c4;
          border-radius: 5px;
          outline: 0;

          font-size: 1.2em;
        }
          input[type=text]:focus,
          input[type=password]:focus,
          input[type=text]:active,
          input[type=password]:active {
            border-color: #41AFCA;
          }

        input[type=submit] {
          font-size: 1em;
        }

      .container {
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;

        background: #424A55;
        background-image: -webkit-linear-gradient(270deg, rgba(48,52,62,0) 24%, #30353e 100%);
        background-image: linear-gradient(-180deg, rgba(48,52,62,0) 24%, #30353e 100%);
      }

      .section {
        padding: 3em;
      }
        .white.section {
          background: #fff;
          border-bottom: 4px solid #41AFCA;
        }

        .white.section a {
          text-decoration: none;
          color: #41AFCA;
        }
          .white.section a:hover,
          .white.section a:active {
            text-decoration: underline;
          }

      .container > .section { 
          background: #424A55;
      }

      .block {
        margin: 0 auto;
        max-width: 900px;
        width: 100%;
      }
">>.

logo() ->
    <<"data:image/png;base64,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">>.

opt_type(oauth_expire) ->
    fun(I) when is_integer(I), I >= 0 -> I end;
opt_type(oauth_access) ->
    fun acl:access_rules_validator/1;
opt_type(oauth_db_type) ->
    fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
opt_type(oauth_cache_life_time) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(oauth_cache_size) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(_) -> [oauth_expire, oauth_access, oauth_db_type].
