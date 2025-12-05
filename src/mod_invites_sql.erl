%%%----------------------------------------------------------------------
%%% File    : mod_invites_sql.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Created : Mon Sep 15 2025 by Stefan Strigler <stefan@strigler.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2025 ProcessOne
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

-export([cleanup_expired/1, create_invite/1, expire_tokens/2, get_invite/2, init/2, is_reserved/3,
         is_token_valid/3, list_invites/1, num_account_invites/2, remove_user/2, set_invitee/3]).

-export([sql_schemas/0]).

-include("mod_invites.hrl").
-include("ejabberd_sql_pt.hrl").

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
                                                  type = text,
                                                  default = true},
                                      #sql_column{name = <<"created_at">>,
                                                  type = timestamp,
                                                  default = true},
                                      #sql_column{name = <<"expires">>, type = timestamp},
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
    CreatedAt = datetime_to_sql_timestamp(CreatedAt0),
    Expires = datetime_to_sql_timestamp(Expires0),

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
                                ?SQL("SELECT @(token)s, @(username)s, @(invitee)s, @(type)s, @(account_name)s, "
                                     "@(expires)s, @(created_at)s FROM invite_token WHERE token = "
                                     "%(Token)s AND %(Host)H"))
    of
        {selected, [{Token, User, Invitee, Type, AccountName, Expires, CreatedAt}]} ->
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
                                    "@(expires)s, @(created_at)s FROM invite_token WHERE %(Host)H")),
    lists:map(
      fun({Token, User, Type, AccountName, Expires, CreatedAt}) ->
              #invite_token{token = Token,
                            inviter = {User, Host},
                            type = dec_type(Type),
                            account_name = AccountName,
                            expires = to_datetime(Expires),
                            created_at = to_datetime(CreatedAt)}
      end, Rows).

num_account_invites(User, Server) ->
    {selected, [{Count}]} =
        ejabberd_sql:sql_query(Server,
                               ?SQL("SELECT @(COUNT(*))d FROM invite_token WHERE username=%(User)s "
                                    "AND %(Server)H AND type != 'R'")),
    Count.

remove_user(User, Server) ->
    ejabberd_sql:sql_query(Server,
                           ?SQL("DELETE FROM invite_token WHERE username=%(User)s AND %(Server)H")).

set_invitee(Host, Token, Invitee) ->
    {updated, 1} =
        ejabberd_sql:sql_query(Host,
                               ?SQL("UPDATE invite_token SET invitee=%(Invitee)s WHERE token=%(Token)s")),
    ok.

%%--------------------------------------------------------------------
%%| helpers

datetime_to_sql_timestamp({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                 [Year, Month, Day, Hour, Minute, Second])).

sql_now() ->
    datetime_to_sql_timestamp(calendar:local_time()).

to_datetime({Date, {H, M, S, _}}) ->
    {Date, {H, M, S}};
to_datetime(DateTime) ->
    DateTime.

enc_type(roster_only) -> <<"R">>;
enc_type(account_subscription) -> <<"S">>;
enc_type(account_only) -> <<"A">>.

dec_type(<<"R">>) -> roster_only;
dec_type(<<"S">>) -> account_subscription;
dec_type(<<"A">>) -> account_only.
