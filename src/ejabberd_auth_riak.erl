%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_riak.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : Authentification via Riak
%%% Created : 12 Nov 2012 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
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

-module(ejabberd_auth_riak).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-behaviour(ejabberd_auth).

%% External exports
-export([start/1, stop/1, set_password/3, check_password/4,
	 check_password/6, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2, get_password/2,
	 get_password_s/2, is_user_exists/2, remove_user/2,
	 remove_user/3, store_type/0, export/1, import/2,
	 plain_password_required/0, opt_type/1]).
-export([passwd_schema/0]).

-include("ejabberd.hrl").
-include("ejabberd_sql_pt.hrl").

-record(passwd, {us = {<<"">>, <<"">>} :: {binary(), binary()} | '$1',
                 password = <<"">> :: binary() | scram() | '_'}).

-define(SALT_LENGTH, 16).

start(_Host) ->
    ok.

stop(_Host) ->
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

check_password(User, AuthzId, Server, Password) ->
    if AuthzId /= <<>> andalso AuthzId /= User ->
        false;
    true ->
        LUser = jid:nodeprep(User),
        LServer = jid:nameprep(Server),
    case ejabberd_riak:get(passwd, passwd_schema(), {LUser, LServer}) of
        {ok, #passwd{password = Password}} when is_binary(Password) ->
            Password /= <<"">>;
        {ok, #passwd{password = Scram}} when is_record(Scram, scram) ->
            is_password_scram_valid(Password, Scram);
        _ ->
            false
        end
    end.

check_password(User, AuthzId, Server, Password, Digest,
	       DigestGen) ->
    if AuthzId /= <<>> andalso AuthzId /= User ->
        false;
    true ->
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
	  Passwd = misc:decode_base64(Scram#scram.storedkey),
	  DigRes = if Digest /= <<"">> ->
			  Digest == DigestGen(Passwd);
		      true -> false
		   end,
	  if DigRes -> true;
	     true -> (Passwd == Password) and (Password /= <<"">>)
	  end;
      _ -> false
        end
    end.

set_password(User, Server, Password) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LPassword = jid:resourceprep(Password),
    US = {LUser, LServer},
    if (LUser == error) or (LServer == error) ->
	   {error, invalid_jid};
       LPassword == error ->
	   {error, invalid_password};
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
    LPassword = jid:resourceprep(Password),
    US = {LUser, LServer},
    if (LUser == error) or (LServer == error) ->
	   {error, invalid_jid};
       LPassword == error and not is_record(Password, scram) ->
	   {error, invalid_password};
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
	  {misc:decode_base64(Scram#scram.storedkey),
	   misc:decode_base64(Scram#scram.serverkey),
	   misc:decode_base64(Scram#scram.salt),
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

export(_Server) ->
    [{passwd,
      fun(Host, #passwd{us = {LUser, LServer}, password = Password})
         when LServer == Host ->
              [?SQL("delete from users where username=%(LUser)s;"),
               ?SQL("insert into users(username, password) "
                    "values (%(LUser)s, %(Password)s);")];
         (_Host, _R) ->
              []
      end}].

import(LServer, [LUser, Password, _TimeStamp]) ->
    Passwd = #passwd{us = {LUser, LServer}, password = Password},
    ejabberd_riak:put(Passwd, passwd_schema(), [{'2i', [{<<"host">>, LServer}]}]).

opt_type(auth_password_format) -> fun (V) -> V end;
opt_type(_) -> [auth_password_format].
