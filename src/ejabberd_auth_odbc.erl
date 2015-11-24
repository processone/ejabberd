%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via ODBC
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-module(ejabberd_auth_odbc).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-behaviour(ejabberd_auth).

-export([start/1, set_password/3, check_password/3,
	 check_password/5, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2, get_password/2,
	 get_password_s/2, is_user_exists/2, remove_user/2,
	 remove_user/3, store_type/0, plain_password_required/0,
	 convert_to_scram/1, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(SALT_LENGTH, 16).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(_Host) -> ok.

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

%% @spec (User, Server, Password) -> true | false | {error, Error}
check_password(User, Server, Password) ->
    LServer = jid:nameprep(Server),
    LUser = jid:nodeprep(User),
    if (LUser == error) or (LServer == error) ->
            false;
       (LUser == <<>>) or (LServer == <<>>) ->
            false;
       true ->
            Username = ejabberd_odbc:escape(LUser),
            case is_scrammed() of
                true ->
                    try odbc_queries:get_password_scram(LServer, Username) of
                        {selected, [<<"password">>, <<"serverkey">>,
                                    <<"salt">>, <<"iterationcount">>],
                         [[StoredKey, ServerKey, Salt, IterationCount]]} ->
                            Scram =
                                #scram{storedkey = StoredKey,
                                       serverkey = ServerKey,
                                       salt = Salt,
                                       iterationcount = binary_to_integer(
                                                          IterationCount)},
                            is_password_scram_valid(Password, Scram);
                        {selected, [<<"password">>, <<"serverkey">>,
                                    <<"salt">>, <<"iterationcount">>], []} ->
                            false; %% Account does not exist
                        {error, _Error} ->
                            false %% Typical error is that table doesn't exist
                    catch
                        _:_ ->
                            false %% Typical error is database not accessible
                    end;
                false ->
                    try odbc_queries:get_password(LServer, Username) of
                        {selected, [<<"password">>], [[Password]]} ->
                            Password /= <<"">>;
                        {selected, [<<"password">>], [[_Password2]]} ->
                            false; %% Password is not correct
                        {selected, [<<"password">>], []} ->
                            false; %% Account does not exist
                        {error, _Error} ->
                            false %% Typical error is that table doesn't exist
                    catch
                        _:_ ->
                            false %% Typical error is database not accessible
                    end
            end
    end.

%% @spec (User, Server, Password, Digest, DigestGen) -> true | false | {error, Error}
check_password(User, Server, Password, Digest,
	       DigestGen) ->
    LServer = jid:nameprep(Server),
    LUser = jid:nodeprep(User),
    if (LUser == error) or (LServer == error) ->
            false;
       (LUser == <<>>) or (LServer == <<>>) ->
            false;
       true ->
            case is_scrammed() of
                false ->
                    Username = ejabberd_odbc:escape(LUser),
                    try odbc_queries:get_password(LServer, Username) of
                        %% Account exists, check if password is valid
                        {selected, [<<"password">>], [[Passwd]]} ->
                            DigRes = if Digest /= <<"">> ->
                                             Digest == DigestGen(Passwd);
                                        true -> false
                                     end,
                            if DigRes -> true;
                               true -> (Passwd == Password) and (Password /= <<"">>)
                            end;
                        {selected, [<<"password">>], []} ->
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
    end.

%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, invalid_jid}
set_password(User, Server, Password) ->
    LServer = jid:nameprep(Server),
    LUser = jid:nodeprep(User),
    if (LUser == error) or (LServer == error) ->
            {error, invalid_jid};
       (LUser == <<>>) or (LServer == <<>>) ->
            {error, invalid_jid};
       true ->
            Username = ejabberd_odbc:escape(LUser),
            case is_scrammed() of
                true ->
                    Scram = password_to_scram(Password),
                    case catch odbc_queries:set_password_scram_t(
                                 LServer,
                                 Username,
                                 ejabberd_odbc:escape(Scram#scram.storedkey),
                                 ejabberd_odbc:escape(Scram#scram.serverkey),
                                 ejabberd_odbc:escape(Scram#scram.salt),
                                 integer_to_binary(Scram#scram.iterationcount)
                                )
                        of
                        {atomic, ok} -> ok;
                        Other -> {error, Other}
                    end;
                false ->
                    Pass = ejabberd_odbc:escape(Password),
                    case catch odbc_queries:set_password_t(LServer,
                                                           Username, Pass)
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
    if (LUser == error) or (LServer == error) ->
            {error, invalid_jid};
       (LUser == <<>>) or (LServer == <<>>) ->
            {error, invalid_jid};
       true ->
	  Username = ejabberd_odbc:escape(LUser),
            case is_scrammed() of
                true ->
                    Scram = password_to_scram(Password),
                    case catch odbc_queries:add_user_scram(
                                 LServer,
                                 Username,
                                 ejabberd_odbc:escape(Scram#scram.storedkey),
                                 ejabberd_odbc:escape(Scram#scram.serverkey),
                                 ejabberd_odbc:escape(Scram#scram.salt),
                                 integer_to_binary(Scram#scram.iterationcount)
                                ) of
                        {updated, 1} -> {atomic, ok};
                        _ -> {atomic, exists}
                    end;
                false ->
                    Pass = ejabberd_odbc:escape(Password),
                    case catch odbc_queries:add_user(LServer, Username,
                                                     Pass)
                        of
                        {updated, 1} -> {atomic, ok};
                        _ -> {atomic, exists}
                    end
            end
    end.

dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(odbc),
    lists:flatmap(fun (Server) ->
			  get_vh_registered_users(Server)
		  end,
		  Servers).

get_vh_registered_users(Server) ->
    LServer = jid:nameprep(Server),
    case catch odbc_queries:list_users(LServer) of
      {selected, [<<"username">>], Res} ->
	  [{U, LServer} || [U] <- Res];
      _ -> []
    end.

get_vh_registered_users(Server, Opts) ->
    LServer = jid:nameprep(Server),
    case catch odbc_queries:list_users(LServer, Opts) of
      {selected, [<<"username">>], Res} ->
	  [{U, LServer} || [U] <- Res];
      _ -> []
    end.

get_vh_registered_users_number(Server) ->
    LServer = jid:nameprep(Server),
    case catch odbc_queries:users_number(LServer) of
      {selected, [_], [[Res]]} ->
	  jlib:binary_to_integer(Res);
      _ -> 0
    end.

get_vh_registered_users_number(Server, Opts) ->
    LServer = jid:nameprep(Server),
    case catch odbc_queries:users_number(LServer, Opts) of
      {selected, [_], [[Res]]} ->
	  jlib:binary_to_integer(Res);
      _Other -> 0
    end.

get_password(User, Server) ->
    LServer = jid:nameprep(Server),
    LUser = jid:nodeprep(User),
    if (LUser == error) or (LServer == error) ->
            false;
       (LUser == <<>>) or (LServer == <<>>) ->
            false;
       true ->
            Username = ejabberd_odbc:escape(LUser),
            case is_scrammed() of
                true ->
                    case catch odbc_queries:get_password_scram(
                                 LServer, Username) of
                        {selected, [<<"password">>, <<"serverkey">>,
                                    <<"salt">>, <<"iterationcount">>],
                         [[StoredKey, ServerKey, Salt, IterationCount]]} ->
                            {jlib:decode_base64(StoredKey),
                             jlib:decode_base64(ServerKey),
                             jlib:decode_base64(Salt),
                             binary_to_integer(IterationCount)};
                        _ -> false
                    end;
                false ->
                    case catch odbc_queries:get_password(LServer, Username)
                        of
                        {selected, [<<"password">>], [[Password]]} -> Password;
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
                    Username = ejabberd_odbc:escape(LUser),
                    case catch odbc_queries:get_password(LServer, Username) of
                        {selected, [<<"password">>], [[Password]]} -> Password;
                        _ -> <<"">>
                    end;
                true -> <<"">>
            end
    end.

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    case jid:nodeprep(User) of
      error -> false;
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  LServer = jid:nameprep(Server),
	  try odbc_queries:get_password(LServer, Username) of
	    {selected, [<<"password">>], [[_Password]]} ->
		true; %% Account exists
	    {selected, [<<"password">>], []} ->
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
    case jid:nodeprep(User) of
      error -> error;
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  LServer = jid:nameprep(Server),
	  catch odbc_queries:del_user(LServer, Username),
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
                    case check_password(User, Server, Password) of
                        true ->
                            remove_user(User, Server),
                            ok;
                        false -> not_allowed
                    end;
                false ->
                    Username = ejabberd_odbc:escape(LUser),
                    Pass = ejabberd_odbc:escape(Password),
                    F = fun () ->
                                Result = odbc_queries:del_user_return_password(
                                           LServer, Username, Pass),
                                case Result of
                                    {selected, [<<"password">>],
                                     [[Password]]} -> ok;
                                    {selected, [<<"password">>],
                                     []} -> not_exists;
                                    _ -> not_allowed
                                end
                        end,
                    {atomic, Result} = odbc_queries:sql_transaction(
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
    Salt = crypto:rand_bytes(?SALT_LENGTH),
    SaltedPassword = scram:salted_password(Password, Salt,
					   IterationCount),
    StoredKey =
	scram:stored_key(scram:client_key(SaltedPassword)),
    ServerKey = scram:server_key(SaltedPassword),
    #scram{storedkey = jlib:encode_base64(StoredKey),
	   serverkey = jlib:encode_base64(ServerKey),
	   salt = jlib:encode_base64(Salt),
	   iterationcount = IterationCount}.

is_password_scram_valid(Password, Scram) ->
    IterationCount = Scram#scram.iterationcount,
    Salt = jlib:decode_base64(Scram#scram.salt),
    SaltedPassword = scram:salted_password(Password, Salt,
					   IterationCount),
    StoredKey =
	scram:stored_key(scram:client_key(SaltedPassword)),
    jlib:decode_base64(Scram#scram.storedkey) == StoredKey.

-define(BATCH_SIZE, 1000).

set_password_scram_t(Username,
                     StoredKey, ServerKey, Salt, IterationCount) ->
    odbc_queries:update_t(<<"users">>,
                          [<<"username">>,
                           <<"password">>,
                           <<"serverkey">>,
                           <<"salt">>,
                           <<"iterationcount">>],
                          [Username, StoredKey,
                           ServerKey, Salt,
                           IterationCount],
                          [<<"username='">>, Username,
                           <<"'">>]).

convert_to_scram(Server) ->
    LServer = jid:nameprep(Server),
    if
        LServer == error;
        LServer == <<>> ->
            {error, {incorrect_server_name, Server}};
        true ->
            F = fun () ->
                        case ejabberd_odbc:sql_query_t(
                               [<<"select username, password from users where "
                                 "iterationcount=0 limit ">>,
                                integer_to_binary(?BATCH_SIZE),
                                <<";">>]) of
                            {selected, [<<"username">>, <<"password">>], []} ->
                                ok;
                            {selected, [<<"username">>, <<"password">>], Rs} ->
                                lists:foreach(
                                  fun([LUser, Password]) ->
                                          Username = ejabberd_odbc:escape(LUser),
                                          Scram = password_to_scram(Password),
                                          set_password_scram_t(
                                            Username,
                                            ejabberd_odbc:escape(Scram#scram.storedkey),
                                            ejabberd_odbc:escape(Scram#scram.serverkey),
                                            ejabberd_odbc:escape(Scram#scram.salt),
                                            integer_to_binary(Scram#scram.iterationcount)
                                           )
                                  end, Rs),
                                continue;
                            Err -> {bad_reply, Err}
                        end
                end,
            case odbc_queries:sql_transaction(LServer, F) of
                {atomic, ok} -> ok;
                {atomic, continue} -> convert_to_scram(Server);
                {atomic, Error} -> {error, Error};
                Error -> Error
            end
    end.

opt_type(auth_password_format) -> fun (V) -> V end;
opt_type(_) -> [auth_password_format].
