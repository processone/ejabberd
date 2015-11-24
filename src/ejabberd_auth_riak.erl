%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_riak.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : Authentification via Riak
%%% Created : 12 Nov 2012 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
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

-module(ejabberd_auth_riak).

-author('alexey@process-one.net').

-behaviour(ejabberd_auth).

%% External exports
-export([start/1, set_password/3, check_password/3,
	 check_password/5, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2, get_password/2,
	 get_password_s/2, is_user_exists/2, remove_user/2,
	 remove_user/3, store_type/0, export/1, import/3,
	 plain_password_required/0]).
-export([passwd_schema/0]).

-include("ejabberd.hrl").

-record(passwd, {us = {<<"">>, <<"">>} :: {binary(), binary()} | '$1',
                 password = <<"">> :: binary() | scram() | '_'}).

-define(SALT_LENGTH, 16).

start(_Host) ->
    ok.

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

passwd_schema() ->
    {record_info(fields, passwd), #passwd{}}.

check_password(User, Server, Password) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    case ejabberd_riak:get(passwd, passwd_schema(), {LUser, LServer}) of
        {ok, #passwd{password = Password}} when is_binary(Password) ->
            Password /= <<"">>;
        {ok, #passwd{password = Scram}} when is_record(Scram, scram) ->
            is_password_scram_valid(Password, Scram);
        _ ->
            false
    end.

check_password(User, Server, Password, Digest,
	       DigestGen) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    case ejabberd_riak:get(passwd, passwd_schema(), {LUser, LServer}) of
      {ok, #passwd{password = Passwd}} when is_binary(Passwd) ->
	  DigRes = if Digest /= <<"">> ->
			  Digest == DigestGen(Passwd);
		      true -> false
		   end,
	  if DigRes -> true;
	     true -> (Passwd == Password) and (Password /= <<"">>)
	  end;
      {ok, #passwd{password = Scram}}
	  when is_record(Scram, scram) ->
	  Passwd = jlib:decode_base64(Scram#scram.storedkey),
	  DigRes = if Digest /= <<"">> ->
			  Digest == DigestGen(Passwd);
		      true -> false
		   end,
	  if DigRes -> true;
	     true -> (Passwd == Password) and (Password /= <<"">>)
	  end;
      _ -> false
    end.

set_password(User, Server, Password) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    if (LUser == error) or (LServer == error) ->
	   {error, invalid_jid};
       true ->
            Password2 = case is_scrammed() and is_binary(Password)
                        of
                            true -> password_to_scram(Password);
                            false -> Password
                        end,
            ok = ejabberd_riak:put(#passwd{us = US, password = Password2},
				   passwd_schema(),
                                   [{'2i', [{<<"host">>, LServer}]}])
    end.

try_register(User, Server, PasswordList) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Password = if is_list(PasswordList); is_binary(PasswordList) ->
      iolist_to_binary(PasswordList);
      true -> PasswordList
    end,
    US = {LUser, LServer},
    if (LUser == error) or (LServer == error) ->
	   {error, invalid_jid};
       true ->
            case ejabberd_riak:get(passwd, passwd_schema(), US) of
                {error, notfound} ->
                    Password2 = case is_scrammed() and
                                    is_binary(Password)
                                of
                                    true -> password_to_scram(Password);
                                    false -> Password
                                end,
                    {atomic, ejabberd_riak:put(
                               #passwd{us = US,
                                       password = Password2},
			       passwd_schema(),
                               [{'2i', [{<<"host">>, LServer}]}])};
                {ok, _} ->
                    exists;
                Err ->
                    {atomic, Err}
            end
    end.

dirty_get_registered_users() ->
    lists:flatmap(
      fun(Server) ->
              get_vh_registered_users(Server)
      end, ejabberd_config:get_vh_by_auth_method(riak)).

get_vh_registered_users(Server) ->
    LServer = jid:nameprep(Server),
    case ejabberd_riak:get_keys_by_index(passwd, <<"host">>, LServer) of
        {ok, Users} ->
            Users;
        _ ->
            []
    end.

get_vh_registered_users(Server, _) ->
    get_vh_registered_users(Server).

get_vh_registered_users_number(Server) ->
    LServer = jid:nameprep(Server),
    case ejabberd_riak:count_by_index(passwd, <<"host">>, LServer) of
        {ok, N} ->
            N;
        _ ->
            0
    end.

get_vh_registered_users_number(Server, _) ->
    get_vh_registered_users_number(Server).

get_password(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    case ejabberd_riak:get(passwd, passwd_schema(), {LUser, LServer}) of
      {ok, #passwd{password = Password}}
	  when is_binary(Password) ->
	  Password;
      {ok, #passwd{password = Scram}}
	  when is_record(Scram, scram) ->
	  {jlib:decode_base64(Scram#scram.storedkey),
	   jlib:decode_base64(Scram#scram.serverkey),
	   jlib:decode_base64(Scram#scram.salt),
	   Scram#scram.iterationcount};
      _ -> false
    end.

get_password_s(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    case ejabberd_riak:get(passwd, passwd_schema(), {LUser, LServer}) of
      {ok, #passwd{password = Password}}
	  when is_binary(Password) ->
	  Password;
      {ok, #passwd{password = Scram}}
	  when is_record(Scram, scram) ->
	  <<"">>;
      _ -> <<"">>
    end.

is_user_exists(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    case ejabberd_riak:get(passwd, passwd_schema(), {LUser, LServer}) of
      {error, notfound} -> false;
      {ok, _} -> true;
      Err -> Err
    end.

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    ejabberd_riak:delete(passwd, {LUser, LServer}),
    ok.

remove_user(User, Server, Password) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    case ejabberd_riak:get(passwd, passwd_schema(), {LUser, LServer}) of
        {ok, #passwd{password = Password}}
          when is_binary(Password) ->
            ejabberd_riak:delete(passwd, {LUser, LServer}),
            ok;
        {ok, #passwd{password = Scram}}
          when is_record(Scram, scram) ->
            case is_password_scram_valid(Password, Scram) of
                true ->
                    ejabberd_riak:delete(passwd, {LUser, LServer}),
                    ok;
                false -> not_allowed
            end;
        _ -> not_exists
    end.

%%%
%%% SCRAM
%%%

is_scrammed() ->
    scram ==
      ejabberd_config:get_local_option({auth_password_format, ?MYNAME},
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

export(_Server) ->
    [{passwd,
      fun(Host, #passwd{us = {LUser, LServer}, password = Password})
            when LServer == Host ->
              Username = ejabberd_odbc:escape(LUser),
              Pass = ejabberd_odbc:escape(Password),
              [[<<"delete from users where username='">>, Username, <<"';">>],
               [<<"insert into users(username, password) "
                  "values ('">>, Username, <<"', '">>, Pass, <<"');">>]];
         (_Host, _R) ->
              []
      end}].

import(LServer, riak, #passwd{} = Passwd) ->
    ejabberd_riak:put(Passwd, passwd_schema(), [{'2i', [{<<"host">>, LServer}]}]);
import(_, _, _) ->
    pass.
