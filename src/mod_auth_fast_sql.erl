%%%-------------------------------------------------------------------
%%% File    : mod_auth_fast_sql.erl
%%% Author  : Pawel Chmielowski <pawel@process-one.net>
%%% Created : 1 Dec 2024 by Pawel Chmielowski <pawel@process-one.net>
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_auth_fast_sql).

-behaviour(mod_auth_fast).

%% API
-export([init/2]).
-export([get_tokens/3, del_token/4, del_tokens/2, set_token/6, rotate_token/3]).
-export([sql_schemas/0]).

-include_lib("xmpp/include/xmpp.hrl").

-include("ejabberd_sql_pt.hrl").
-include("logger.hrl").


%%%===================================================================
%%% API
%%%===================================================================
init(Host, _Opts) ->
    ejabberd_sql_schema:update_schema(Host, ?MODULE, sql_schemas()),
    ok.


sql_schemas() ->
    [#sql_schema{
       version = 1,
       tables =
           [#sql_table{
              name = <<"auth_fast_tokens">>,
              columns =
                  [#sql_column{name = <<"username">>, type = text},
                   #sql_column{name = <<"server_host">>, type = text},
                   #sql_column{name = <<"ua">>, type = text},
                   #sql_column{name = <<"type">>, type = {char, 1}},
                   #sql_column{name = <<"token">>, type = text},
                   #sql_column{name = <<"created_at">>, type = timestamp, default = true},
                   #sql_column{name = <<"expires">>, type = timestamp}],
              indices = [#sql_index{
                           columns = [<<"server_host">>, <<"username">>, <<"ua">>, <<"type">>],
                           unique = true
                          }]
             }]
      }].


-spec get_tokens(binary(), binary(), binary()) ->
          [{current | next, binary(), non_neg_integer()}].
get_tokens(LServer, LUser, UA) ->
    Now = calendar:now_to_datetime(erlang:timestamp()),
    UAs = base64:encode(UA),
    F = fun() ->
                case ejabberd_sql:sql_query_t(?SQL("select @(type)s, @(token)s, @(created_at)t, @(expires)t from auth_fast_tokens where "
                                                   "username=%(LUser)s and ua=%(UAs)s and %(LServer)H")) of
                    {selected, Entries} ->
                        lists:filtermap(
                          fun({<<"c">>, _Token, _Created, Expires}) when Expires < Now ->
                                  ejabberd_sql:sql_query_t(?SQL("delete from auth_fast_tokens where "
                                                                "username=%(LUser)s and ua=%(UAs)s and type='c' and %(LServer)H")),
                                  false;
                             ({<<"c">>, Token, Created, _Expires}) ->
                                  CreatedTS = system_time_seconds_from_datetime(Created),
                                  {true, {current, Token, CreatedTS}};
                             ({<<"n">>, _Token, _Created, Expires}) when Expires < Now ->
                                  ejabberd_sql:sql_query_t(?SQL("delete from auth_fast_tokens where "
                                                                "username=%(LUser)s and ua=%(UAs)s and type='n' and %(LServer)H")),
                                  false;
                             ({<<"n">>, Token, Created, _Expires}) ->
                                  CreatedTS = system_time_seconds_from_datetime(Created),
                                  {true, {next, Token, CreatedTS}};
                             (_) ->
                                  false
                          end,
                          Entries);
                    _ -> []
                end
        end,

    case ejabberd_sql:sql_transaction(LServer, F) of
        {atomic, Ret} ->
            Ret;
        _ ->
            []
    end.


-spec rotate_token(binary(), binary(), binary()) ->
          ok | {error, atom()}.
rotate_token(LServer, LUser, UA) ->
    UAs = base64:encode(UA),
    F = fun() ->
                case ejabberd_sql:sql_query_t(?SQL("select @(1)d from auth_fast_tokens where "
                                                   "username=%(LUser)s and ua=%(UAs)s and type='n' and %(LServer)H")) of
                    {selected, [_]} ->
                        ejabberd_sql:sql_query_t(?SQL("delete from auth_fast_tokens where "
                                                      "username=%(LUser)s and ua=%(UAs)s and type='c' and %(LServer)H")),
                        ejabberd_sql:sql_query_t(?SQL("update auth_fast_tokens set type='c' where "
                                                      "username=%(LUser)s and ua=%(UAs)s and type='n' and %(LServer)H"));
                    _ ->
                        ok
                end
        end,
    case ejabberd_sql:sql_transaction(LServer, F) of
        {atomic, _} ->
            ok;
        _ ->
            {error, db_error}
    end.


-spec del_token(binary(), binary(), binary(), current | next) ->
          ok | {error, atom()}.
del_token(LServer, LUser, UA, Type) ->
    UAs = base64:encode(UA),
    TypeS = case Type of
                current -> <<"c">>;
                _ -> <<"n">>
            end,
    case ejabberd_sql:sql_query(LServer,
                                ?SQL("delete from auth_fast_tokens where "
                                     "username=%(LUser)s and ua=%(UAs)s and type=%(TypeS)s and %(LServer)H")) of
        {updated, _} ->
            ok;
        _ ->
            {error, db_error}
    end.


-spec del_tokens(binary(), binary()) -> ok | {error, atom()}.
del_tokens(LServer, LUser) ->
    case ejabberd_sql:sql_query(LServer,
                                ?SQL("delete from auth_fast_tokens where "
                                     "username=%(LUser)s and %(LServer)H")) of
        {updated, _} ->
            ok;
        _ ->
            {error, db_error}
    end.


-spec set_token(binary(), binary(), binary(), current | next, binary(), non_neg_integer()) ->
          ok | {error, atom()}.
set_token(LServer, LUser, UA, Type, Token, Expires) ->
    UAs = base64:encode(UA),
    TypeS = case Type of
                current -> <<"c">>;
                _ -> <<"n">>
            end,
    ExpiresT = calendar:now_to_datetime(misc:usec_to_now(Expires * 1000000)),
    case ?SQL_UPSERT(
           LServer,
           "auth_fast_tokens",
           ["!username=%(LUser)s",
            "!server_host=%(LServer)s",
            "!ua=%(UAs)s",
            "!type=%(TypeS)s",
            "token=%(Token)s",
            "expires=%(ExpiresT)t"]) of
        ok ->
            ok;
        _ ->
            {error, db_error}
    end.


system_time_seconds_from_datetime(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - 719528 * 86400.
