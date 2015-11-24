%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_internal.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via mnesia
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

-module(ejabberd_auth_internal).

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
	 remove_user/3, store_type/0, export/1, import/1,
	 import/3, plain_password_required/0, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-record(passwd, {us = {<<"">>, <<"">>} :: {binary(), binary()} | '$1',
                 password = <<"">> :: binary() | scram() | '_'}).

-record(reg_users_counter, {vhost = <<"">> :: binary(),
                            count = 0 :: integer() | '$1'}).

-define(SALT_LENGTH, 16).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    mnesia:create_table(passwd,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, passwd)}]),
    mnesia:create_table(reg_users_counter,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, reg_users_counter)}]),
    update_table(),
    update_reg_users_counter_table(Host),
    maybe_alert_password_scrammed_without_option(),
    ok.

update_reg_users_counter_table(Server) ->
    Set = get_vh_registered_users(Server),
    Size = length(Set),
    LServer = jid:nameprep(Server),
    F = fun () ->
		mnesia:write(#reg_users_counter{vhost = LServer,
						count = Size})
	end,
    mnesia:sync_dirty(F).

plain_password_required() ->
    is_scrammed().

store_type() ->
    case is_scrammed() of
      false -> plain; %% allows: PLAIN DIGEST-MD5 SCRAM
      true -> scram %% allows: PLAIN SCRAM
    end.

check_password(User, Server, Password) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read({passwd, US}) of
      [#passwd{password = Password}]
	  when is_binary(Password) ->
	  Password /= <<"">>;
      [#passwd{password = Scram}]
	  when is_record(Scram, scram) ->
	  is_password_scram_valid(Password, Scram);
      _ -> false
    end.

check_password(User, Server, Password, Digest,
	       DigestGen) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read({passwd, US}) of
      [#passwd{password = Passwd}] when is_binary(Passwd) ->
	  DigRes = if Digest /= <<"">> ->
			  Digest == DigestGen(Passwd);
		      true -> false
		   end,
	  if DigRes -> true;
	     true -> (Passwd == Password) and (Password /= <<"">>)
	  end;
      [#passwd{password = Scram}]
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

%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, invalid_jid}
set_password(User, Server, Password) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    if (LUser == error) or (LServer == error) ->
	   {error, invalid_jid};
       true ->
	   F = fun () ->
		       Password2 = case is_scrammed() and is_binary(Password)
				       of
				     true -> password_to_scram(Password);
				     false -> Password
				   end,
		       mnesia:write(#passwd{us = US, password = Password2})
	       end,
	   {atomic, ok} = mnesia:transaction(F),
	   ok
    end.

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid} | {error, not_allowed} | {error, Reason}
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
	   F = fun () ->
		       case mnesia:read({passwd, US}) of
			 [] ->
			     Password2 = case is_scrammed() and
						is_binary(Password)
					     of
					   true -> password_to_scram(Password);
					   false -> Password
					 end,
			     mnesia:write(#passwd{us = US,
						  password = Password2}),
			     mnesia:dirty_update_counter(reg_users_counter,
							 LServer, 1),
			     ok;
			 [_E] -> exists
		       end
	       end,
	   mnesia:transaction(F)
    end.

%% Get all registered users in Mnesia
dirty_get_registered_users() ->
    mnesia:dirty_all_keys(passwd).

get_vh_registered_users(Server) ->
    LServer = jid:nameprep(Server),
    mnesia:dirty_select(passwd,
			[{#passwd{us = '$1', _ = '_'},
			  [{'==', {element, 2, '$1'}, LServer}], ['$1']}]).

get_vh_registered_users(Server,
			[{from, Start}, {to, End}])
    when is_integer(Start) and is_integer(End) ->
    get_vh_registered_users(Server,
			    [{limit, End - Start + 1}, {offset, Start}]);
get_vh_registered_users(Server,
			[{limit, Limit}, {offset, Offset}])
    when is_integer(Limit) and is_integer(Offset) ->
    case get_vh_registered_users(Server) of
      [] -> [];
      Users ->
	  Set = lists:keysort(1, Users),
	  L = length(Set),
	  Start = if Offset < 1 -> 1;
		     Offset > L -> L;
		     true -> Offset
		  end,
	  lists:sublist(Set, Start, Limit)
    end;
get_vh_registered_users(Server, [{prefix, Prefix}])
    when is_binary(Prefix) ->
    Set = [{U, S}
	   || {U, S} <- get_vh_registered_users(Server),
	      str:prefix(Prefix, U)],
    lists:keysort(1, Set);
get_vh_registered_users(Server,
			[{prefix, Prefix}, {from, Start}, {to, End}])
    when is_binary(Prefix) and is_integer(Start) and
	   is_integer(End) ->
    get_vh_registered_users(Server,
			    [{prefix, Prefix}, {limit, End - Start + 1},
			     {offset, Start}]);
get_vh_registered_users(Server,
			[{prefix, Prefix}, {limit, Limit}, {offset, Offset}])
    when is_binary(Prefix) and is_integer(Limit) and
	   is_integer(Offset) ->
    case [{U, S}
	  || {U, S} <- get_vh_registered_users(Server),
	     str:prefix(Prefix, U)]
	of
      [] -> [];
      Users ->
	  Set = lists:keysort(1, Users),
	  L = length(Set),
	  Start = if Offset < 1 -> 1;
		     Offset > L -> L;
		     true -> Offset
		  end,
	  lists:sublist(Set, Start, Limit)
    end;
get_vh_registered_users(Server, _) ->
    get_vh_registered_users(Server).

get_vh_registered_users_number(Server) ->
    LServer = jid:nameprep(Server),
    Query = mnesia:dirty_select(reg_users_counter,
				[{#reg_users_counter{vhost = LServer,
						     count = '$1'},
				  [], ['$1']}]),
    case Query of
      [Count] -> Count;
      _ -> 0
    end.

get_vh_registered_users_number(Server,
			       [{prefix, Prefix}])
    when is_binary(Prefix) ->
    Set = [{U, S}
	   || {U, S} <- get_vh_registered_users(Server),
	      str:prefix(Prefix, U)],
    length(Set);
get_vh_registered_users_number(Server, _) ->
    get_vh_registered_users_number(Server).

get_password(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read(passwd, US) of
      [#passwd{password = Password}]
	  when is_binary(Password) ->
	  Password;
      [#passwd{password = Scram}]
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
    US = {LUser, LServer},
    case catch mnesia:dirty_read(passwd, US) of
      [#passwd{password = Password}]
	  when is_binary(Password) ->
	  Password;
      [#passwd{password = Scram}]
	  when is_record(Scram, scram) ->
	  <<"">>;
      _ -> <<"">>
    end.

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read({passwd, US}) of
      [] -> false;
      [_] -> true;
      Other -> {error, Other}
    end.

%% @spec (User, Server) -> ok
%% @doc Remove user.
%% Note: it returns ok even if there was some problem removing the user.
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    F = fun () ->
		mnesia:delete({passwd, US}),
		mnesia:dirty_update_counter(reg_users_counter, LServer,
					    -1)
	end,
    mnesia:transaction(F),
    ok.

%% @spec (User, Server, Password) -> ok | not_exists | not_allowed | bad_request
%% @doc Remove user if the provided password is correct.
remove_user(User, Server, Password) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    F = fun () ->
		case mnesia:read({passwd, US}) of
		  [#passwd{password = Password}]
		      when is_binary(Password) ->
		      mnesia:delete({passwd, US}),
		      mnesia:dirty_update_counter(reg_users_counter, LServer,
						  -1),
		      ok;
		  [#passwd{password = Scram}]
		      when is_record(Scram, scram) ->
		      case is_password_scram_valid(Password, Scram) of
			true ->
			    mnesia:delete({passwd, US}),
			    mnesia:dirty_update_counter(reg_users_counter,
							LServer, -1),
			    ok;
			false -> not_allowed
		      end;
		  _ -> not_exists
		end
	end,
    case mnesia:transaction(F) of
      {atomic, ok} -> ok;
      {atomic, Res} -> Res;
      _ -> bad_request
    end.

update_table() ->
    Fields = record_info(fields, passwd),
    case mnesia:table_info(passwd, attributes) of
        Fields ->
            convert_to_binary(Fields),
            maybe_scram_passwords(),
            ok;
        _ ->
            ?INFO_MSG("Recreating passwd table", []),
            mnesia:transform_table(passwd, ignore, Fields)
    end.

convert_to_binary(Fields) ->
    ejabberd_config:convert_table_to_binary(
      passwd, Fields, set,
      fun(#passwd{us = {U, _}}) -> U end,
      fun(#passwd{us = {U, S}, password = Pass} = R) ->
              NewUS = {iolist_to_binary(U), iolist_to_binary(S)},
              NewPass = case Pass of
                            #scram{storedkey = StoredKey,
                                   serverkey = ServerKey,
                                   salt = Salt} ->
                                Pass#scram{
                                  storedkey = iolist_to_binary(StoredKey),
                                  serverkey = iolist_to_binary(ServerKey),
                                  salt = iolist_to_binary(Salt)};
                            _ ->
                                iolist_to_binary(Pass)
                        end,
              R#passwd{us = NewUS, password = NewPass}
      end).

%%%
%%% SCRAM
%%%

%% The passwords are stored scrammed in the table either if the option says so,
%% or if at least the first password is scrammed.
is_scrammed() ->
    OptionScram = is_option_scram(),
    FirstElement = mnesia:dirty_read(passwd,
				     mnesia:dirty_first(passwd)),
    case {OptionScram, FirstElement} of
      {true, _} -> true;
      {false, [#passwd{password = Scram}]}
	  when is_record(Scram, scram) ->
	  true;
      _ -> false
    end.

is_option_scram() ->
    scram ==
      ejabberd_config:get_option({auth_password_format, ?MYNAME},
                                       fun(V) -> V end).

maybe_alert_password_scrammed_without_option() ->
    case is_scrammed() andalso not is_option_scram() of
      true ->
	  ?ERROR_MSG("Some passwords were stored in the database "
		     "as SCRAM, but 'auth_password_format' "
		     "is not configured 'scram'. The option "
		     "will now be considered to be 'scram'.",
		     []);
      false -> ok
    end.

maybe_scram_passwords() ->
    case is_scrammed() of
      true -> scram_passwords();
      false -> ok
    end.

scram_passwords() ->
    ?INFO_MSG("Converting the stored passwords into "
	      "SCRAM bits",
	      []),
    Fun = fun (#passwd{password = Password} = P) ->
		  Scram = password_to_scram(Password),
		  P#passwd{password = Scram}
	  end,
    Fields = record_info(fields, passwd),
    mnesia:transform_table(passwd, Fun, Fields).

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

import(LServer) ->
    [{<<"select username, password from users;">>,
      fun([LUser, Password]) ->
              #passwd{us = {LUser, LServer}, password = Password}
      end}].

import(_LServer, mnesia, #passwd{} = P) ->
    mnesia:dirty_write(P);
import(_, _, _) ->
    pass.

opt_type(auth_password_format) -> fun (V) -> V end;
opt_type(_) -> [auth_password_format].
