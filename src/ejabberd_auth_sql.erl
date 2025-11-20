%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_sql.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentication via ODBC
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
-behaviour(ejabberd_db_serialize).

-export([start/1, stop/1, set_password_multiple/3, try_register_multiple/3,
	 get_users/2, count_users/2, get_password/2,
	 remove_user/2, store_type/1, plain_password_required/1,
	 export/1, which_users_exists/2, drop_password_type/2, set_password_instance/3]).
-export([sql_schemas/0]).
-export([serialize/3, deserialize_start/1, deserialize/2]).

-include_lib("xmpp/include/scram.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").
-include("ejabberd_auth.hrl").
-include("ejabberd_db_serialize.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    ejabberd_sql_schema:update_schema(Host, ?MODULE, sql_schemas()),
    ok.

sql_schemas() ->
    [
	#sql_schema{
	version = 2,
	tables =
	[#sql_table{
	    name = <<"users">>,
	    columns =
	    [#sql_column{name = <<"username">>, type = text},
		#sql_column{name = <<"server_host">>, type = text},
		#sql_column{name = <<"type">>, type = smallint},
		#sql_column{name = <<"password">>, type = text},
		#sql_column{name = <<"serverkey">>, type = {text, 128},
		    default = true},
		#sql_column{name = <<"salt">>, type = {text, 128},
		    default = true},
		#sql_column{name = <<"iterationcount">>, type = integer,
		    default = true},
		#sql_column{name = <<"created_at">>, type = timestamp,
		    default = true}],
	    indices = [#sql_index{
		columns = [<<"server_host">>, <<"username">>, <<"type">>],
		unique = true}]}],
	update = [
	    {add_column, <<"users">>, <<"type">>},
	    {update_primary_key,<<"users">>,
		[<<"server_host">>, <<"username">>, <<"type">>]}
	]},
    #sql_schema{
        version = 1,
        tables =
            [#sql_table{
                name = <<"users">>,
                columns =
                    [#sql_column{name = <<"username">>, type = text},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"password">>, type = text},
                     #sql_column{name = <<"serverkey">>, type = {text, 128},
                                 default = true},
                     #sql_column{name = <<"salt">>, type = {text, 128},
                                 default = true},
                     #sql_column{name = <<"iterationcount">>, type = integer,
                                 default = true},
                     #sql_column{name = <<"created_at">>, type = timestamp,
                                 default = true}],
                indices = [#sql_index{
                              columns = [<<"server_host">>, <<"username">>],
                              unique = true}]}]}].

stop(_Host) -> ok.

plain_password_required(Server) ->
    store_type(Server) == scram.

store_type(Server) ->
    ejabberd_auth:password_format(Server).

hash_to_num(plain) -> 1;
hash_to_num(sha) -> 2;
hash_to_num(sha256) -> 3;
hash_to_num(sha512) -> 4.

num_to_hash(2) -> sha;
num_to_hash(3) -> sha256;
num_to_hash(4) -> sha512.

set_password_instance(User, Server, #scram{hash = Hash, storedkey = SK, serverkey = SEK,
					   salt = Salt, iterationcount = IC}) ->
    F = fun() ->
	set_password_scram_t(User, Server, Hash,
			     SK, SEK, Salt, IC)
	end,
    case ejabberd_sql:sql_transaction(Server, F) of
	{atomic, _} ->
	    ok;
	{aborted, _} ->
	    {error, db_failure}
    end;
set_password_instance(User, Server, Plain) ->
    F = fun() ->
	set_password_t(User, Server, Plain)
	end,
    case ejabberd_sql:sql_transaction(Server, F) of
	{atomic, _} ->
	    ok;
	{aborted, _} ->
	    {error, db_failure}
    end.

set_password_multiple(User, Server, Passwords) ->
    F =
    fun() ->
	ejabberd_sql:sql_query_t(
	    ?SQL("delete from users where username=%(User)s and %(Server)H")),
	lists:foreach(
	    fun(#scram{hash = Hash, storedkey = SK, serverkey = SEK,
		       salt = Salt, iterationcount = IC}) ->
		set_password_scram_t(
		    User, Server, Hash,
		    SK, SEK, Salt, IC);
	       (Plain) ->
		   set_password_t(User, Server, Plain)
	    end, Passwords)
    end,
    case ejabberd_sql:sql_transaction(Server, F) of
	{atomic, _} ->
	    {cache, {ok, Passwords}};
	{aborted, _} ->
	    {nocache, {error, db_failure}}
    end.

try_register_multiple(User, Server, Passwords) ->
    F =
	fun() ->
	    case ejabberd_sql:sql_query_t(
		?SQL("select @(count(*))d from users where username=%(User)s and %(Server)H")) of
		{selected, [{0}]} ->
		    lists:foreach(
			fun(#scram{hash = Hash, storedkey = SK, serverkey = SEK,
				   salt = Salt, iterationcount = IC}) ->
			    set_password_scram_t(
				User, Server, Hash,
				SK, SEK, Salt, IC);
			   (Plain) ->
			       set_password_t(User, Server, Plain)
			end, Passwords),
		    {cache, {ok, Passwords}};
		{selected, _} ->
		    {nocache, {error, exists}};
		_ ->
		    {nocache, {error, db_failure}}
	    end
	end,
    case ejabberd_sql:sql_transaction(Server, F) of
	{atomic, Res} ->
	    Res;
	{aborted, _} ->
	    {nocache, {error, db_failure}}
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
	{selected, []} ->
	    {cache, error};
	{selected, Passwords} ->
	    Converted = lists:map(
		fun({0, Password, <<>>, <<>>, 0}) ->
		    update_password_type(User, Server, 1),
		    Password;
		   ({_, Password, <<>>, <<>>, 0}) ->
		       Password;
		   ({0, StoredKey, ServerKey, Salt, IterationCount}) ->
		       {Hash, SK} = case StoredKey of
					<<"sha256:", Rest/binary>> ->
					    update_password_type(User, Server, 3, Rest),
					    {sha256, Rest};
					<<"sha512:", Rest/binary>> ->
					    update_password_type(User, Server, 4, Rest),
					    {sha512, Rest};
					Other ->
					    update_password_type(User, Server, 2),
					    {sha, Other}
				    end,
		       #scram{storedkey = SK,
			      serverkey = ServerKey,
			      salt = Salt,
			      hash = Hash,
			      iterationcount = IterationCount};
		   ({Type, StoredKey, ServerKey, Salt, IterationCount}) ->
		       Hash = num_to_hash(Type),
		       #scram{storedkey = StoredKey,
			      serverkey = ServerKey,
			      salt = Salt,
			      hash = Hash,
			      iterationcount = IterationCount}
		end, Passwords),
	    {cache, {ok, Converted}};
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

drop_password_type(LServer, Hash) ->
    Type = hash_to_num(Hash),
    ejabberd_sql:sql_query(
	LServer,
	?SQL("delete from users"
	     " where type=%(Type)d and %(LServer)H")).

set_password_scram_t(LUser, LServer, Hash,
                     StoredKey, ServerKey, Salt, IterationCount) ->
    Type = hash_to_num(Hash),
    ?SQL_UPSERT_T(
       "users",
       ["!username=%(LUser)s",
        "!server_host=%(LServer)s",
	"!type=%(Type)d",
        "password=%(StoredKey)s",
        "serverkey=%(ServerKey)s",
        "salt=%(Salt)s",
        "iterationcount=%(IterationCount)d"]).

set_password_t(LUser, LServer, Password) ->
    ?SQL_UPSERT_T(
       "users",
       ["!username=%(LUser)s",
        "!server_host=%(LServer)s",
	"!type=1",
	"password=%(Password)s",
	"serverkey=''",
	"salt=''",
	"iterationcount=0"]).

update_password_type(LUser, LServer, Type, Password) ->
    ejabberd_sql:sql_query(
	LServer,
	?SQL("update users set type=%(Type)d, password=%(Password)s"
	     " where username=%(LUser)s and type=0 and %(LServer)H")).

update_password_type(LUser, LServer, Type) ->
    ejabberd_sql:sql_query(
	LServer,
	?SQL("update users set type=%(Type)d"
	     " where username=%(LUser)s and type=0 and %(LServer)H")).

get_password_scram(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(type)d, @(password)s, @(serverkey)s, @(salt)s, @(iterationcount)d"
           " from users"
           " where username=%(LUser)s and %(LServer)H")).

del_user(LServer, LUser) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from users where username=%(LUser)s and %(LServer)H")).

list_users(LServer, []) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(distinct username)s from users where %(LServer)H"));
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
      ?SQL("select @(distinct username)s from users "
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
      ?SQL("select @(distinct username)s from users "
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
                        ?SQL("select @(count(distinct username))d from users where %(LServer)H"))
	  end;
         (_Type, _) ->
              ejabberd_sql:sql_query_t(
                ?SQL("select @(count(distinct username))d from users where %(LServer)H"))
      end).

users_number(LServer, [{prefix, Prefix}])
    when is_binary(Prefix) ->
    SPrefix = ejabberd_sql:escape_like_arg(Prefix),
    SPrefix2 = <<SPrefix/binary, $%>>,
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("select @(count(distinct username))d from users "
           "where username like %(SPrefix2)s %ESCAPE and %(LServer)H"));
users_number(LServer, []) ->
    users_number(LServer).

which_users_exists(LServer, LUsers) when length(LUsers) =< 100 ->
    try ejabberd_sql:sql_query(
        LServer,
        ?SQL("select @(distinct username)s from users where username in %(LUsers)ls")) of
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
      fun(Host, #passwd{us = {LUser, LServer, plain}, password = Password})
            when LServer == Host,
                 is_binary(Password) ->
              [?SQL("delete from users where username=%(LUser)s and type=1 and %(LServer)H;"),
               ?SQL_INSERT(
                  "users",
                  ["username=%(LUser)s",
                   "server_host=%(LServer)s",
                   "type=1",
                   "password=%(Password)s"])];
         (Host, {passwd, {LUser, LServer, _},
                         {scram, StoredKey, ServerKey, Salt, IterationCount}})
            when LServer == Host ->
              Hash = sha,
              Type = hash_to_num(Hash),
              [?SQL("delete from users where username=%(LUser)s and type=%(Type)d and %(LServer)H;"),
               ?SQL_INSERT(
                  "users",
                  ["username=%(LUser)s",
                   "server_host=%(LServer)s",
                   "type=%(Type)d",
                   "password=%(StoredKey)s",
                   "serverkey=%(ServerKey)s",
                   "salt=%(Salt)s",
                   "iterationcount=%(IterationCount)d"])];
         (Host, #passwd{us = {LUser, LServer, _}, password = #scram{} = Scram})
            when LServer == Host ->
	      StoredKey = Scram#scram.storedkey,
              ServerKey = Scram#scram.serverkey,
              Salt = Scram#scram.salt,
              IterationCount = Scram#scram.iterationcount,
              Type = hash_to_num(Scram#scram.hash),
              [?SQL("delete from users where username=%(LUser)s and type=%(Type)d and %(LServer)H;"),
               ?SQL_INSERT(
                  "users",
                  ["username=%(LUser)s",
                   "server_host=%(LServer)s",
                   "type=%(Type)d",
                   "password=%(StoredKey)s",
                   "serverkey=%(ServerKey)s",
                   "salt=%(Salt)s",
                   "iterationcount=%(IterationCount)d"])];
         (_Host, _R) ->
              []
      end}].


serialize(LServer, BatchSize, Last) ->
    Offset = case Last of
                 undefined -> 0;
                 _ -> Last
             end,
    case ejabberd_sql:sql_query(
           LServer,
           ?SQL("select @(username)s, @(type)d, @(password)s, @(serverkey)s, @(salt)s, @(iterationcount)d  from users "
                "where %(LServer)H "
                "order by username, type "
                "limit %(BatchSize)d offset %(Offset)d")) of
        {selected, Rows} ->
            ?DEBUG("GOT rows ~p", [Rows]),
            Data = lists:map(
                     fun({Username, _, Password, <<>>, <<>>, 0}) ->
                             #serialize_auth_v1{serverhost = LServer, username = Username, passwords = [Password]};
                        ({Username, 1, Password, _, _, _}) ->
                             #serialize_auth_v1{serverhost = LServer, username = Username, passwords = [Password]};
                        ({Username, 0, Password, ServerKey, Salt, IterationCount}) ->
                             {Hash, SK} = case Password of
                                              <<"sha256:", Rest/binary>> ->
                                                  {sha256, Rest};
                                              <<"sha512:", Rest/binary>> ->
                                                  {sha512, Rest};
                                              Other ->
                                                  {sha, Other}
                                          end,
                             #serialize_auth_v1{
                               serverhost = LServer,
                               username = Username,
                               passwords = [{Hash, SK, ServerKey, Salt, IterationCount}]
                              };
                        ({Username, Type, StoredKey, ServerKey, Salt, IterationCount}) ->
                             #serialize_auth_v1{
                               serverhost = LServer,
                               username = Username,
                               passwords = [{num_to_hash(Type),
                                             StoredKey,
                                             ServerKey,
                                             Salt,
                                             IterationCount}]
                              }
                     end,
                     Rows),
            Data2 = case length(Rows) < BatchSize of
                        true -> Data ++ [#serialize_auth_v1{serverhost = <<>>, username = <<>>, passwords = []}];
                        _ -> Data
                    end,
            {_, Data3, _, RC2} = lists:foldl(
                                   fun(Next, {undefined, Res, _, _}) ->
                                           {Next, Res, 1, 0};
                                      (#serialize_auth_v1{username = U1, passwords = [P1]},
                                       {#serialize_auth_v1{username = U2, passwords = P2} = Next, Res, NC, RC})
                                         when U1 == U2 ->
                                           {Next#serialize_auth_v1{passwords = [P1 | P2]}, Res, NC + 1, RC};
                                      (Current, {Next, Acc, NC, RC}) ->
                                           {Current, [Next | Acc], 1, RC + NC}
                                   end,
                                   {undefined, [], 0, 0},
                                   Data2),
            {ok, Data3, Offset + RC2};
        _ ->
            {error, io_lib:format("Error when retrieving passwords data from database", [])}
    end.


deserialize_start(LServer) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from users where %(LServer)H")).


deserialize(LServer, Batch) ->
    case ejabberd_sql:sql_transaction(LServer,
				      fun() ->
					  lists:foreach(
					      fun(#serialize_auth_v1{username = Username, passwords = Passwords}) ->
						  lists:foreach(
						      fun({Hash, StoredKey, ServerKey, Salt, IterationCount}) ->
							  set_password_scram_t(Username,
									       LServer,
									       Hash,
									       StoredKey,
									       ServerKey,
									       Salt,
									       IterationCount);
							 (Password) ->
							     set_password_t(Username, LServer, Password)
						      end,
						      Passwords)
					      end,
					      Batch)
				      end) of
	{atomic, _} -> ok;
	_ ->
	    {error, io_lib:format("Error when storing passwords in database", [])}
    end.
