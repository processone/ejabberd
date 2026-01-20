%%%----------------------------------------------------------------------
%%% File    : mod_invites_sql.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Created : Mon Sep 15 2025 by Stefan Strigler <stefan@strigler.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2026 ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------
-module(mod_invites_sql).

-author('stefan@strigler.de').

-behaviour(mod_invites).

-export([cleanup_expired/1, create_invite/1, expire_tokens/2, get_invite/2, get_invites/2, init/2,
         is_reserved/3, is_token_valid/3, list_invites/1, remove_user/2,
         set_invitee/5]).

-export([sql_schemas/0]).

-include("mod_invites.hrl").
-include("ejabberd_sql_pt.hrl").

%-define(I(B), binary_to_integer(B)).

%% @format-begin

%%--------------------------------------------------------------------
%%| mod_invite callbacks
init(Host, _Opts) ->
    ejabberd_sql_schema:update_schema(Host, ?MODULE, sql_schemas()).

sql_schemas() ->
    [#sql_schema{version = 1,
                 tables =
                     [#sql_table{name = <<"invite_token">>,
                                 columns =
                                     [#sql_column{name = <<"token">>, type = text},
                                      #sql_column{name = <<"username">>, type = text},
                                      #sql_column{name = <<"server_host">>, type = text},
                                      #sql_column{name = <<"invitee">>,
                                                  type = {text, 191},
                                                  default = true},
                                      #sql_column{name = <<"created_at">>,
                                                  type = timestamp,
                                                  default = true},
                                      #sql_column{name = <<"expires">>,
                                                  type = timestamp,
                                                  default = true},
                                      #sql_column{name = <<"type">>, type = {char, 1}},
                                      #sql_column{name = <<"account_name">>, type = text}],
                                 indices =
                                     [#sql_index{columns = [<<"token">>], unique = true},
                                      #sql_index{columns =
                                                     [<<"username">>, <<"server_host">>]}]}]}].

cleanup_expired(Host) ->
    NOW = sql_now(),
    {updated, Count} =
        ejabberd_sql:sql_query(Host, ?SQL("DELETE FROM invite_token WHERE expires < %(NOW)t")),
    Count.

create_invite(Invite) ->
    #invite_token{inviter = {User, Host},
                  token = Token,
                  account_name = AccountName,
                  created_at = CreatedAt0,
                  expires = Expires0,
                  type = Type0} =
        Invite,
    Type = enc_type(Type0),
    CreatedAt = CreatedAt0,
    Expires = Expires0,

    Query =
        ?SQL_INSERT("invite_token",
                    ["token=%(Token)s",
                     "username=%(User)s",
                     "server_host=%(Host)s",
                     "type=%(Type)s",
                     "created_at=%(CreatedAt)t",
                     "expires=%(Expires)t",
                     "account_name=%(AccountName)s"]),
    {updated, 1} = ejabberd_sql:sql_query(Host, Query),
    Invite.

expire_tokens(User, Server) ->
    NOW = sql_now(),
    {updated, Count} =
        ejabberd_sql:sql_query(Server,
                               ?SQL("UPDATE invite_token SET expires = '1970-01-01 00:00:01' WHERE "
                                    "username = %(User)s AND %(Server)H AND expires > %(NOW)t AND "
                                    "type != 'R'")),
    Count.

get_invite(Host, Token) ->
    case ejabberd_sql:sql_query(Host,
                                ?SQL("SELECT @(username)s, @(invitee)s, @(type)s, @(account_name)s, "
                                     "@(expires)t, @(created_at)t FROM invite_token WHERE token = "
                                     "%(Token)s AND %(Host)H"))
    of
        {selected, [{User, Invitee, Type, AccountName, Expires, CreatedAt}]} ->
            #invite_token{token = Token,
                          inviter = {User, Host},
                          invitee = Invitee,
                          type = dec_type(Type),
                          account_name = AccountName,
                          expires = Expires,
                          created_at = CreatedAt};
        {selected, []} ->
            {error, not_found}
    end.

get_invites(Host, {User, _Host}) ->
    {selected, Invites} =
        ejabberd_sql:sql_query(Host,
                               ?SQL("SELECT @(token)s, @(invitee)s, @(type)s, @(account_name)s, "
                                    "@(expires)t, @(created_at)t FROM invite_token WHERE %(Host)H "
                                    "AND username = %(User)s")),
    lists:map(fun({Token, Invitee, Type, AccountName, Expires, CreatedAt}) ->
                 #invite_token{token = Token,
                               inviter = {User, Host},
                               invitee = Invitee,
                               type = dec_type(Type),
                               account_name = AccountName,
                               expires = Expires,
                               created_at = CreatedAt}
              end,
              Invites).

is_reserved(Host, Token, User) ->
    NOW = sql_now(),
    {selected, [{Count}]} =
        ejabberd_sql:sql_query(Host,
                               ?SQL("SELECT @(COUNT(*))d FROM invite_token WHERE %(Host)H AND token != %(Token)s AND "
                                    "account_name = %(User)s AND invitee = '' AND expires > %(NOW)t")),
    Count > 0.

is_token_valid(Host, Token, {User, Host}) ->
    NOW = sql_now(),
    {selected, Rows} =
        ejabberd_sql:sql_query(Host,
                               ?SQL("SELECT @(token)s FROM invite_token WHERE %(Host)H AND token = %(Token)s AND "
                                    "invitee = '' AND expires > %(NOW)t AND (%(User)s = '' OR username = %(User)s)")),
    case Rows /= [] of
        true ->
            true;
        false ->
            case get_invite(Host, Token) of
                {error, not_found} ->
                    throw(not_found);
                _ ->
                    false
            end
    end.

list_invites(Host) ->
    {selected, Rows} =
        ejabberd_sql:sql_query(Host,
                               ?SQL("SELECT @(token)s, @(username)s, @(type)s, @(account_name)s, "
                                    "@(expires)t, @(created_at)t FROM invite_token WHERE %(Host)H")),
    lists:map(fun({Token, User, Type, AccountName, Expires, CreatedAt}) ->
                 #invite_token{token = Token,
                               inviter = {User, Host},
                               type = dec_type(Type),
                               account_name = AccountName,
                               expires = Expires,
                               created_at = CreatedAt}
              end,
              Rows).

remove_user(User, Server) ->
    ejabberd_sql:sql_query(Server,
                           ?SQL("DELETE FROM invite_token WHERE username=%(User)s AND %(Server)H")).

set_invitee(Fun, Host, Token, Invitee, AccountName) ->
    Trans =
        fun() ->
           {updated, 1} =
               ejabberd_sql:sql_query_t(?SQL("UPDATE invite_token SET invitee = %(Invitee)s, account_name = %(AccountName)s WHERE "
                                             "%(Host)H AND token = %(Token)s AND invitee = '' AND (type != 'R' OR account_name = '' OR %(AccountName)s = '')")),
           ok = Fun()
        end,
    case ejabberd_sql:sql_transaction(Host, Trans) of
        {atomic, Res} ->
            Res;
        {aborted, {badmatch, {updated, 0}}} ->
            {error, conflict};
        {aborted, {badmatch, {error, _Res} = Error}} ->
            Error
    end.

%%--------------------------------------------------------------------
%%| helpers
sql_now() ->
    calendar:local_time().

enc_type(roster_only) ->
    <<"R">>;
enc_type(account_subscription) ->
    <<"S">>;
enc_type(account_only) ->
    <<"A">>.

dec_type(<<"R">>) ->
    roster_only;
dec_type(<<"S">>) ->
    account_subscription;
dec_type(<<"A">>) ->
    account_only.
