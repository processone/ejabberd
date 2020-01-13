%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_sql.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentication via ODBC
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2020   ProcessOne
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

-module(ejabberd_auth_sql).


-author('alexey@process-one.net').

-behaviour(ejabberd_auth).

-export([start/1, stop/1, set_password/3, try_register/3,
	 get_users/2, count_users/2, get_password/2,
	 remove_user/2, store_type/1, plain_password_required/1,
	 export/1, which_users_exists/2]).

-include("scram.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").
-include("ejabberd_auth.hrl").

-define(SALT_LENGTH, 16).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(_Host) -> ok.

stop(_Host) -> ok.

plain_password_required(Server) ->
    store_type(Server) == scram.

store_type(Server) ->
    ejabberd_auth:password_format(Server).

set_password(User, Server, Password) ->
    F = fun() ->
		if is_record(Password, scram) ->
			set_password_scram_t(
			  User, Server,
			  Password#scram.storedkey, Password#scram.serverkey,
			  Password#scram.salt, Password#scram.iterationcount);
		   true ->
			set_password_t(User, Server, Password)
		end
	end,
    case ejabberd_sql:sql_transaction(Server, F) of
	{atomic, _} ->
	    {cache, {ok, Password}};
	{aborted, _} ->
	    {nocache, {error, db_failure}}
    end.

try_register(User, Server, Password) ->
    Res = if is_record(Password, scram) ->
		  add_user_scram(
		    Server, User,
		    Password#scram.storedkey, Password#scram.serverkey,
		    Password#scram.salt, Password#scram.iterationcount);
	     true ->
		  add_user(Server, User, Password)
	  end,
    case Res of
	{updated, 1} -> {cache, {ok, Password}};
	_ -> {nocache, {error, exists}}
    end.

get_users(Server, Opts) ->
    case list_users(Server, Opts) of
	{selected, Res} ->
	    [{U, Server} || {U} <- Res];
	_ -> []
    end.

count_users(Server, Opts) ->
    case users_number(Server, Opts) of
	{selected, [{Res}]} ->
	    Res;
	_Other -> 0
    end.

get_password(User, Server) ->
    case get_password_scram(Server, User) of
	{selected, [{Password, <<>>, <<>>, 0}]} ->
	    {cache, {ok, Password}};
	{selected, [{StoredKey, ServerKey, Salt, IterationCount}]} ->
	    {cache, {ok, #scram{storedkey = StoredKey,
				serverkey = ServerKey,
				salt = Salt,
				iterationcount = IterationCount}}};
	{selected, []} ->
	    {cache, error};
	_ ->
	    {nocache, error}
    end.

remove_user(User, Server) ->
    case del_user(Server, User) of
	{updated, _} ->
	    ok;
	_ ->
	    {error, db_failure}
    end.

-define(BATCH_SIZE, 1000).

set_password_scram_t(LUser, LServer,
                     StoredKey, ServerKey, Salt, IterationCount) ->
    ?SQL_UPSERT_T(
       "users",
       ["!username=%(LUser)s",
        "!server_host=%(LServer)s",
        "password=%(StoredKey)s",
        "serverkey=%(ServerKey)s",
        "salt=%(Salt)s",
        "iterationcount=%(IterationCount)d"]).

set_password_t(LUser, LServer, Password) ->
    ?SQL_UPSERT_T(
       "users",
       ["!username=%(LUser)s",
        "!server_host=%(LServer)s",
	"password=%(Password)s"]).

get_password_scram(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(password)s, @(serverkey)s, @(salt)s, @(iterationcount)d"
           " from users"
           " where username=%(LUser)s and %(LServer)H")).

add_user_scram(LServer, LUser,
               StoredKey, ServerKey, Salt, IterationCount) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL_INSERT(
         "users",
         ["username=%(LUser)s",
          "server_host=%(LServer)s",
          "password=%(StoredKey)s",
          "serverkey=%(ServerKey)s",
          "salt=%(Salt)s",
          "iterationcount=%(IterationCount)d"])).

add_user(LServer, LUser, Password) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL_INSERT(
         "users",
         ["username=%(LUser)s",
          "server_host=%(LServer)s",
          "password=%(Password)s"])).

del_user(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from users where username=%(LUser)s and %(LServer)H")).

list_users(LServer, []) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(username)s from users where %(LServer)H"));
list_users(LServer, [{from, Start}, {to, End}])
    when is_integer(Start) and is_integer(End) ->
    list_users(LServer,
	       [{limit, End - Start + 1}, {offset, Start - 1}]);
list_users(LServer,
	   [{prefix, Prefix}, {from, Start}, {to, End}])
    when is_binary(Prefix) and is_integer(Start) and
	   is_integer(End) ->
    list_users(LServer,
	       [{prefix, Prefix}, {limit, End - Start + 1},
		{offset, Start - 1}]);
list_users(LServer, [{limit, Limit}, {offset, Offset}])
    when is_integer(Limit) and is_integer(Offset) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(username)s from users "
           "where %(LServer)H "
           "order by username "
           "limit %(Limit)d offset %(Offset)d"));
list_users(LServer,
	   [{prefix, Prefix}, {limit, Limit}, {offset, Offset}])
    when is_binary(Prefix) and is_integer(Limit) and
	   is_integer(Offset) ->
    SPrefix = ejabberd_sql:escape_like_arg(Prefix),
    SPrefix2 = <<SPrefix/binary, $%>>,
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(username)s from users "
           "where username like %(SPrefix2)s %ESCAPE and %(LServer)H "
           "order by username "
           "limit %(Limit)d offset %(Offset)d")).

users_number(LServer) ->
    ejabberd_sql:sql_query(
      LServer,
      fun(pgsql, _) ->
              case
                  ejabberd_option:pgsql_users_number_estimate(LServer) of
                  true ->
                      ejabberd_sql:sql_query_t(
                        ?SQL("select @(reltuples :: bigint)d from pg_class"
                             " where oid = 'users'::regclass::oid"));
                  _ ->
                      ejabberd_sql:sql_query_t(
                        ?SQL("select @(count(*))d from users where %(LServer)H"))
	  end;
         (_Type, _) ->
              ejabberd_sql:sql_query_t(
                ?SQL("select @(count(*))d from users where %(LServer)H"))
      end).

users_number(LServer, [{prefix, Prefix}])
    when is_binary(Prefix) ->
    SPrefix = ejabberd_sql:escape_like_arg(Prefix),
    SPrefix2 = <<SPrefix/binary, $%>>,
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(count(*))d from users "
           "where username like %(SPrefix2)s %ESCAPE and %(LServer)H"));
users_number(LServer, []) ->
    users_number(LServer).

which_users_exists(LServer, LUsers) when length(LUsers) =< 100 ->
    try ejabberd_sql:sql_query(
        LServer,
        ?SQL("select @(username)s from users where username in %(LUsers)ls")) of
        {selected, Matching} ->
            [U || {U} <- Matching];
        {error, _} = E ->
            E
    catch _:B ->
        {error, B}
    end;
which_users_exists(LServer, LUsers) ->
    {First, Rest} = lists:split(100, LUsers),
    case which_users_exists(LServer, First) of
        {error, _} = E ->
            E;
        V ->
            case which_users_exists(LServer, Rest) of
                {error, _} = E2 ->
                    E2;
                V2 ->
                    V ++ V2
            end
    end.

export(_Server) ->
    [{passwd,
      fun(Host, #passwd{us = {LUser, LServer}, password = Password})
            when LServer == Host,
                 is_binary(Password) ->
              [?SQL("delete from users where username=%(LUser)s and %(LServer)H;"),
               ?SQL_INSERT(
                  "users",
                  ["username=%(LUser)s",
                   "server_host=%(LServer)s",
                   "password=%(Password)s"])];
         (Host, #passwd{us = {LUser, LServer}, password = #scram{} = Scram})
            when LServer == Host ->
              StoredKey = Scram#scram.storedkey,
              ServerKey = Scram#scram.serverkey,
              Salt = Scram#scram.salt,
              IterationCount = Scram#scram.iterationcount,
              [?SQL("delete from users where username=%(LUser)s and %(LServer)H;"),
               ?SQL_INSERT(
                  "users",
                  ["username=%(LUser)s",
                   "server_host=%(LServer)s",
                   "password=%(StoredKey)s",
                   "serverkey=%(ServerKey)s",
                   "salt=%(Salt)s",
                   "iterationcount=%(IterationCount)d"])];
         (_Host, _R) ->
              []
      end}].
