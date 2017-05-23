%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_sql.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via ODBC
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-author('alexey@process-one.net').

-behaviour(ejabberd_auth).
-behaviour(ejabberd_config).

-export([start/1, stop/1, set_password/3, try_register/3,
	 get_users/2, count_users/2, get_password/2,
	 remove_user/2, store_type/1, plain_password_required/1,
	 convert_to_scram/1, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

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
			  User,
			  Password#scram.storedkey, Password#scram.serverkey,
			  Password#scram.salt, Password#scram.iterationcount);
		   true ->
			set_password_t(User, Password)
		end
	end,
    case ejabberd_sql:sql_transaction(Server, F) of
	{atomic, _} ->
	    ok;
	{aborted, Reason} ->
	    ?ERROR_MSG("failed to write to SQL table: ~p", [Reason]),
	    {error, db_failure}
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
	{updated, 1} -> ok;
	_ -> {error, exists}
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
	    {ok, Password};
	{selected, [{StoredKey, ServerKey, Salt, IterationCount}]} ->
	    {ok, #scram{storedkey = StoredKey,
			serverkey = ServerKey,
			salt = Salt,
			iterationcount = IterationCount}};
	{selected, []} ->
	    error;
	Err ->
	    ?ERROR_MSG("Failed to read password for user ~s@~s: ~p",
		       [User, Server, Err]),
	    error
    end.

remove_user(User, Server) ->
    case del_user(Server, User) of
	{updated, _} ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to delete user ~s@~s: ~p",
		       [User, Server, Err]),
	    {error, db_failure}
    end.

-define(BATCH_SIZE, 1000).

set_password_scram_t(LUser,
                     StoredKey, ServerKey, Salt, IterationCount) ->
    ?SQL_UPSERT_T(
       "users",
       ["!username=%(LUser)s",
        "password=%(StoredKey)s",
        "serverkey=%(ServerKey)s",
        "salt=%(Salt)s",
        "iterationcount=%(IterationCount)d"]).

set_password_t(LUser, Password) ->
    ?SQL_UPSERT_T(
       "users",
       ["!username=%(LUser)s",
	"password=%(Password)s"]).

get_password_scram(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(password)s, @(serverkey)s, @(salt)s, @(iterationcount)d"
           " from users"
           " where username=%(LUser)s")).

add_user_scram(LServer, LUser,
               StoredKey, ServerKey, Salt, IterationCount) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("insert into users(username, password, serverkey, salt, "
           "iterationcount) "
           "values (%(LUser)s, %(StoredKey)s, %(ServerKey)s,"
           " %(Salt)s, %(IterationCount)d)")).

add_user(LServer, LUser, Password) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("insert into users(username, password) "
           "values (%(LUser)s, %(Password)s)")).

del_user(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from users where username=%(LUser)s")).

list_users(LServer, []) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(username)s from users"));
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
           "order by username "
           "limit %(Limit)d offset %(Offset)d"));
list_users(LServer,
	   [{prefix, Prefix}, {limit, Limit}, {offset, Offset}])
    when is_binary(Prefix) and is_integer(Limit) and
	   is_integer(Offset) ->
    SPrefix = ejabberd_sql:escape_like_arg_circumflex(Prefix),
    SPrefix2 = <<SPrefix/binary, $%>>,
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(username)s from users "
           "where username like %(SPrefix2)s escape '^' "
           "order by username "
           "limit %(Limit)d offset %(Offset)d")).

users_number(LServer) ->
    ejabberd_sql:sql_query(
      LServer,
      fun(pgsql, _) ->
              case
                  ejabberd_config:get_option(
                    {pgsql_users_number_estimate, LServer}, false) of
                  true ->
                      ejabberd_sql:sql_query_t(
                        ?SQL("select @(reltuples :: bigint)d from pg_class"
                             " where oid = 'users'::regclass::oid"));
                  _ ->
                      ejabberd_sql:sql_query_t(
                        ?SQL("select @(count(*))d from users"))
	  end;
         (_Type, _) ->
              ejabberd_sql:sql_query_t(
                ?SQL("select @(count(*))d from users"))
      end).

users_number(LServer, [{prefix, Prefix}])
    when is_binary(Prefix) ->
    SPrefix = ejabberd_sql:escape_like_arg_circumflex(Prefix),
    SPrefix2 = <<SPrefix/binary, $%>>,
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(count(*))d from users "
           "where username like %(SPrefix2)s escape '^'"));
users_number(LServer, []) ->
    users_number(LServer).

convert_to_scram(Server) ->
    LServer = jid:nameprep(Server),
    if
        LServer == error;
        LServer == <<>> ->
            {error, {incorrect_server_name, Server}};
        true ->
            F = fun () ->
                        BatchSize = ?BATCH_SIZE,
                        case ejabberd_sql:sql_query_t(
                               ?SQL("select @(username)s, @(password)s"
                                    " from users"
                                    " where iterationcount=0"
                                    " limit %(BatchSize)d")) of
                            {selected, []} ->
                                ok;
                            {selected, Rs} ->
                                lists:foreach(
                                  fun({LUser, Password}) ->
					  case jid:resourceprep(Password) of
					      error ->
						  ?ERROR_MSG(
						     "SASLprep failed for "
						     "password of user ~s@~s",
						     [LUser, LServer]);
					      _ ->
						  Scram = ejabberd_auth:password_to_scram(Password),
						  set_password_scram_t(
						    LUser,
						    Scram#scram.storedkey,
						    Scram#scram.serverkey,
						    Scram#scram.salt,
						    Scram#scram.iterationcount)
					  end
                                  end, Rs),
                                continue;
                            Err -> {bad_reply, Err}
                        end
                end,
            case ejabberd_sql:sql_transaction(LServer, F) of
                {atomic, ok} -> ok;
                {atomic, continue} -> convert_to_scram(Server);
                {atomic, Error} -> {error, Error};
                Error -> Error
            end
    end.

-spec opt_type(pgsql_users_number_estimate) -> fun((boolean()) -> boolean());
	      (atom()) -> [atom()].
opt_type(pgsql_users_number_estimate) ->
    fun (V) when is_boolean(V) -> V end;
opt_type(_) -> [pgsql_users_number_estimate].
