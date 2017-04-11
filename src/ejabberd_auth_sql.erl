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

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-behaviour(ejabberd_auth).

-export([start/1, stop/1, set_password/3, check_password/4,
	 check_password/6, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2, get_password/2,
	 get_password_s/2, is_user_exists/2, remove_user/2,
	 remove_user/3, store_type/0, plain_password_required/0,
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

plain_password_required() ->
    case is_scrammed() of
      false -> false;
      true -> true
    end.

store_type() ->
    case is_scrammed() of
      false -> plain; %% allows: PLAIN DIGEST-MD5 SCRAM
      true -> scram %% allows: PLAIN SCRAM
    end.

%% @spec (User, AuthzId, Server, Password) -> true | false | {error, Error}
check_password(User, AuthzId, Server, Password) ->
    if AuthzId /= <<>> andalso AuthzId /= User ->
        false;
    true ->
        LServer = jid:nameprep(Server),
        LUser = jid:nodeprep(User),
    if (LUser == error) or (LServer == error) ->
            false;
       (LUser == <<>>) or (LServer == <<>>) ->
            false;
       true ->
            case is_scrammed() of
                true ->
                    try sql_queries:get_password_scram(LServer, LUser) of
                        {selected,
                         [{StoredKey, ServerKey, Salt, IterationCount}]} ->
                            Scram =
                                #scram{storedkey = StoredKey,
                                       serverkey = ServerKey,
                                       salt = Salt,
                                       iterationcount = IterationCount},
                            is_password_scram_valid_stored(Password, Scram, LUser, LServer);
                        {selected, []} ->
                            false; %% Account does not exist
                        {error, _Error} ->
                            false %% Typical error is that table doesn't exist
                    catch
                        _:_ ->
                            false %% Typical error is database not accessible
                    end;
                false ->
                    try sql_queries:get_password(LServer, LUser) of
                        {selected, [{Password}]} ->
                            Password /= <<"">>;
                        {selected, [{_Password2}]} ->
                            false; %% Password is not correct
                        {selected, []} ->
                            false; %% Account does not exist
                        {error, _Error} ->
                            false %% Typical error is that table doesn't exist
                    catch
                        _:_ ->
                            false %% Typical error is database not accessible
                    end
            end
        end
    end.

%% @spec (User, AuthzId, Server, Password, Digest, DigestGen) -> true | false | {error, Error}
check_password(User, AuthzId, Server, Password, Digest,
	       DigestGen) ->
    if AuthzId /= <<>> andalso AuthzId /= User ->
        false;
    true ->
        LServer = jid:nameprep(Server),
        LUser = jid:nodeprep(User),
    if (LUser == error) or (LServer == error) ->
            false;
       (LUser == <<>>) or (LServer == <<>>) ->
            false;
       true ->
            case is_scrammed() of
                false ->
                    try sql_queries:get_password(LServer, LUser) of
                        %% Account exists, check if password is valid
                        {selected, [{Passwd}]} ->
                            DigRes = if Digest /= <<"">> ->
                                             Digest == DigestGen(Passwd);
                                        true -> false
                                     end,
                            if DigRes -> true;
                               true -> (Passwd == Password) and (Password /= <<"">>)
                            end;
                        {selected, []} ->
                            false; %% Account does not exist
                        {error, _Error} ->
                            false %% Typical error is that table doesn't exist
                    catch
                        _:_ ->
                            false %% Typical error is database not accessible
                    end;
                true ->
                    false
            end
        end
    end.

%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, invalid_jid}
set_password(User, Server, Password) ->
    LServer = jid:nameprep(Server),
    LUser = jid:nodeprep(User),
    LPassword = jid:resourceprep(Password),
    if (LUser == error) or (LServer == error) ->
            {error, invalid_jid};
       (LUser == <<>>) or (LServer == <<>>) ->
            {error, invalid_jid};
       LPassword == error ->
	   {error, invalid_password};
       true ->
            case is_scrammed() of
                true ->
                    Scram = password_to_scram(Password),
                    case catch sql_queries:set_password_scram_t(
                                 LServer,
                                 LUser,
                                 Scram#scram.storedkey,
                                 Scram#scram.serverkey,
                                 Scram#scram.salt,
                                 Scram#scram.iterationcount
                                )
                        of
                        {atomic, ok} -> ok;
                        Other -> {error, Other}
                    end;
                false ->
                    case catch sql_queries:set_password_t(LServer,
                                                           LUser, Password)
                        of
                        {atomic, ok} -> ok;
                        Other -> {error, Other}
                    end
            end
    end.

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid}
try_register(User, Server, Password) ->
    LServer = jid:nameprep(Server),
    LUser = jid:nodeprep(User),
    LPassword = jid:resourceprep(Password),
    if (LUser == error) or (LServer == error) ->
            {error, invalid_jid};
       (LUser == <<>>) or (LServer == <<>>) ->
            {error, invalid_jid};
       LPassword == error and not is_record(Password, scram) ->
	   {error, invalid_password};
       true ->
            case is_scrammed() of
                true ->
                    Scram = case is_record(Password, scram) of
                        true -> Password;
                        false -> password_to_scram(Password)
                    end,
                    case catch sql_queries:add_user_scram(
                                 LServer,
                                 LUser,
                                 Scram#scram.storedkey,
                                 Scram#scram.serverkey,
                                 Scram#scram.salt,
                                 Scram#scram.iterationcount
                                ) of
                        {updated, 1} -> {atomic, ok};
                        _ -> {atomic, exists}
                    end;
                false ->
                    case catch sql_queries:add_user(LServer, LUser,
                                                     Password) of
                        {updated, 1} -> {atomic, ok};
                        _ -> {atomic, exists}
                    end
            end
    end.

dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(sql),
    lists:flatmap(fun (Server) ->
			  get_vh_registered_users(Server)
		  end,
		  Servers).

get_vh_registered_users(Server) ->
    case jid:nameprep(Server) of
        error -> [];
        <<>> -> [];
        LServer ->
            case catch sql_queries:list_users(LServer) of
                {selected, Res} ->
                    [{U, LServer} || {U} <- Res];
                _ -> []
            end
    end.

get_vh_registered_users(Server, Opts) ->
    case jid:nameprep(Server) of
        error -> [];
        <<>> -> [];
        LServer ->
            case catch sql_queries:list_users(LServer, Opts) of
                {selected, Res} ->
                    [{U, LServer} || {U} <- Res];
                _ -> []
            end
    end.

get_vh_registered_users_number(Server) ->
    case jid:nameprep(Server) of
        error -> 0;
        <<>> -> 0;
        LServer ->
            case catch sql_queries:users_number(LServer) of
                {selected, [{Res}]} ->
                    Res;
                _ -> 0
            end
    end.

get_vh_registered_users_number(Server, Opts) ->
    case jid:nameprep(Server) of
        error -> 0;
        <<>> -> 0;
        LServer ->
            case catch sql_queries:users_number(LServer, Opts) of
                {selected, [{Res}]} ->
                    Res;
                _Other -> 0
            end
    end.

get_password(User, Server) ->
    LServer = jid:nameprep(Server),
    LUser = jid:nodeprep(User),
    if (LUser == error) or (LServer == error) ->
            false;
       (LUser == <<>>) or (LServer == <<>>) ->
            false;
       true ->
            case is_scrammed() of
                true ->
                    case catch sql_queries:get_password_scram(
                                 LServer, LUser) of
                        {selected,
                         [{StoredKey, ServerKey, Salt, IterationCount}]} ->
                            {misc:decode_base64(StoredKey),
                             misc:decode_base64(ServerKey),
                             misc:decode_base64(Salt),
                             IterationCount};
                        _ -> false
                    end;
                false ->
                    case catch sql_queries:get_password(LServer, LUser)
                        of
                        {selected, [{Password}]} -> Password;
                        _ -> false
                    end
            end
    end.

get_password_s(User, Server) ->
    LServer = jid:nameprep(Server),
    LUser = jid:nodeprep(User),
    if (LUser == error) or (LServer == error) ->
            <<"">>;
       (LUser == <<>>) or (LServer == <<>>) ->
            <<"">>;
       true ->
            case is_scrammed() of
                false ->
                    case catch sql_queries:get_password(LServer, LUser) of
                        {selected, [{Password}]} -> Password;
                        _ -> <<"">>
                    end;
                true -> <<"">>
            end
    end.

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    LServer = jid:nameprep(Server),
    LUser = jid:nodeprep(User),
    if (LUser == error) or (LServer == error) ->
            false;
       (LUser == <<>>) or (LServer == <<>>) ->
            false;
       true ->
	  try sql_queries:get_password(LServer, LUser) of
	    {selected, [{_Password}]} ->
		true; %% Account exists
	    {selected, []} ->
		false; %% Account does not exist
	    {error, Error} -> {error, Error}
	  catch
	    _:B -> {error, B}
	  end
    end.

%% @spec (User, Server) -> ok | error
%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
remove_user(User, Server) ->
    LServer = jid:nameprep(Server),
    LUser = jid:nodeprep(User),
    if (LUser == error) or (LServer == error) ->
            error;
       (LUser == <<>>) or (LServer == <<>>) ->
            error;
       true ->
	  catch sql_queries:del_user(LServer, LUser),
	  ok
    end.

%% @spec (User, Server, Password) -> ok | error | not_exists | not_allowed
%% @doc Remove user if the provided password is correct.
remove_user(User, Server, Password) ->
    LServer = jid:nameprep(Server),
    LUser = jid:nodeprep(User),
    if (LUser == error) or (LServer == error) ->
            error;
       (LUser == <<>>) or (LServer == <<>>) ->
            error;
       true ->
            case is_scrammed() of
                true ->
                    case check_password(User, <<"">>, Server, Password) of
                        true ->
                            remove_user(User, Server),
                            ok;
                        false -> not_allowed
                    end;
                false ->
                    F = fun () ->
                                Result = sql_queries:del_user_return_password(
                                           LServer, LUser, Password),
                                case Result of
                                    {selected, [{Password}]} -> ok;
                                    {selected, []} -> not_exists;
                                    _ -> not_allowed
                                end
                        end,
                    {atomic, Result} = sql_queries:sql_transaction(
                                         LServer, F),
                    Result
            end
    end.

%%%
%%% SCRAM
%%%

is_scrammed() ->
    scram ==
      ejabberd_config:get_option({auth_password_format, ?MYNAME},
                                 fun(V) -> V end).

password_to_scram(Password) ->
    password_to_scram(Password,
		      ?SCRAM_DEFAULT_ITERATION_COUNT).

password_to_scram(Password, IterationCount) ->
    Salt = randoms:bytes(?SALT_LENGTH),
    SaltedPassword = scram:salted_password(Password, Salt,
					   IterationCount),
    StoredKey =
	scram:stored_key(scram:client_key(SaltedPassword)),
    ServerKey = scram:server_key(SaltedPassword),
    #scram{storedkey = misc:encode_base64(StoredKey),
	   serverkey = misc:encode_base64(ServerKey),
	   salt = misc:encode_base64(Salt),
	   iterationcount = IterationCount}.

is_password_scram_valid_stored(Pass, {scram,Pass,<<>>,<<>>,0}, LUser, LServer) ->
    ?INFO_MSG("Apparently, SQL auth method and scram password formatting are "
	"enabled, but the password of user '~s' in the 'users' table is not "
	"scrammed. You may want to execute this command: "
	"ejabberdctl convert_to_scram ~s", [LUser, LServer]),
    false;
is_password_scram_valid_stored(Password, Scram, _, _) ->
    is_password_scram_valid(Password, Scram).

is_password_scram_valid(Password, Scram) ->
    case jid:resourceprep(Password) of
	error ->
	    false;
	_ ->
	    IterationCount = Scram#scram.iterationcount,
	    Salt = misc:decode_base64(Scram#scram.salt),
	    SaltedPassword = scram:salted_password(Password, Salt,
						   IterationCount),
	    StoredKey =
		scram:stored_key(scram:client_key(SaltedPassword)),
	    misc:decode_base64(Scram#scram.storedkey) == StoredKey
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
						  Scram = password_to_scram(Password),
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
            case sql_queries:sql_transaction(LServer, F) of
                {atomic, ok} -> ok;
                {atomic, continue} -> convert_to_scram(Server);
                {atomic, Error} -> {error, Error};
                Error -> Error
            end
    end.

opt_type(auth_password_format) -> fun (V) -> V end;
opt_type(_) -> [auth_password_format].
