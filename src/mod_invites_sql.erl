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

-export([cleanup_expired/1, create_invite/1, expire_tokens/2, get_invite/2, init/2,
         is_token_valid/3, num_account_invites/2, remove_user/2, set_invitee/3]).

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
                                      #sql_column{name = <<"type">>, type = text},
                                      #sql_column{name = <<"account_name">>, type = text}],
                                 indices =
                                     [#sql_index{columns = [<<"token">>], unique = true},
                                      #sql_index{columns =
                                                     [<<"username">>, <<"server_host">>]}]}]}].

cleanup_expired(Host) ->
    {updated, Count} =
        ejabberd_sql:sql_query(Host, "delete from invite_token where expires < now()"),
    Count.

create_invite(Invite) ->
    #invite_token{inviter = {User, Host},
                  token = Token,
                  account_name = AccountName0,
                  created_at = CreatedAt0,
                  expires = Expires0,
                  type = Type} =
        Invite,
    TypeBin = atom_to_binary(Type),
    AccountName =
        case AccountName0 of
            undefined ->
                <<>>;
            _ ->
                AccountName0
        end,
    CreatedAt = datetime_to_sql_timestamp(CreatedAt0),
    Expires = datetime_to_sql_timestamp(Expires0),

    Query =
        ?SQL_INSERT("invite_token",
                    ["token=%(Token)s",
                     "username=%(User)s",
                     "server_host=%(Host)s",
                     "type=%(TypeBin)s",
                     "created_at=%(CreatedAt)s",
                     "expires=%(Expires)s",
                     "account_name=%(AccountName)s"]),
    %    ejabberd_sql:sql_transaction(Host, Queries)
    {updated, 1} = ejabberd_sql:sql_query(Host, Query),
    Invite.

expire_tokens(User, Server) ->
    {updated, Count} =
        ejabberd_sql:sql_query(Server,
                               ?SQL("update invite_token set expires = '1970-01-01 00:00:01' where "
                                    "username = %(User)s and %(Server)H and expires > now() and "
                                    "type != 'roster_only'")),
    Count.

get_invite(Host, Token) ->
    case ejabberd_sql:sql_query(Host,
                                ?SQL("select @(token)s, @(username)s, @(type)s, @(account_name)s, "
                                     "@(expires)s, @(created_at)s from invite_token where token = "
                                     "%(Token)s and %(Host)H"))
    of
        {selected, [{Token, User, Type, AccountName0, Expires, CreatedAt}]} ->
            AccountName =
                case AccountName0 of
                    <<>> ->
                        undefined;
                    _ ->
                        AccountName0
                end,
            #invite_token{token = Token,
                          inviter = {User, Host},
                          type = binary_to_existing_atom(Type),
                          account_name = AccountName,
                          expires = Expires,
                          created_at = CreatedAt};
        {selected, []} ->
            {error, not_found}
    end.

is_token_valid(Host, Token, {User, Host}) ->
    {selected, Rows} =
        ejabberd_sql:sql_query(Host,
                               ?SQL("select @(token)s from invite_token where token = %(Token)s and username = %(User)s "
                                    "and %(Host)H and invitee = '' and expires > now()")),
    length(Rows) /= 0.

num_account_invites(User, Server) ->
    {selected, [{Count}]} =
        ejabberd_sql:sql_query(Server,
                               ?SQL("select @(count(*))d from invite_token where username=%(User)s "
                                    "and %(Server)H and type != 'roster_only'")),
    Count.

remove_user(User, Server) ->
    ejabberd_sql:sql_query(Server,
                           ?SQL("delete from invite_token where username=%(User)s and %(Server)H")).

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
