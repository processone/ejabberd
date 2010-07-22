%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_storage.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>, Stephan Maka
%%% Purpose : Authentification via gen_storage
%%% Created : 16 Sep 2008 Stephan Maka
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth_storage).
-author('alexey@process-one.net').

%% External exports
-export([start/1,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2,
	 get_password/2,
	 get_password_s/2,
	 is_user_exists/2,
	 remove_user/2,
	 remove_user/3,
	 plain_password_required/0
	]).

-include("ejabberd.hrl").

-record(passwd, {us, password}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    Backend =
	case ejabberd_config:get_local_option({auth_storage, Host}) of
	    undefined -> mnesia;
	    B -> B
	end,
    gen_storage:create_table(Backend, Host, passwd,
			     [{odbc_host, Host},
			      {disc_copies, [node()]},
			      {attributes, record_info(fields, passwd)},
			      {types, [{us, {text, text}}]}
			     ]),
    update_table(Host),
    ok.

plain_password_required() ->
    false.

check_password(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch gen_storage:dirty_read(LServer, {passwd, US}) of
	[#passwd{password = Password}] ->
	    Password /= "";
	_ ->
	    false
    end.

check_password(User, Server, Password, StreamID, Digest) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch gen_storage:dirty_read(LServer, {passwd, US}) of
	[#passwd{password = Passwd}] ->
	    DigRes = if
			 Digest /= "" ->
			     Digest == sha:sha(StreamID ++ Passwd);
			 true ->
			     false
		     end,
	    if DigRes ->
		    true;
	       true ->
		    (Passwd == Password) and (Password /= "")
	    end;
	_ ->
	    false
    end.

%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, invalid_jid}
set_password(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    if
	(LUser == error) or (LServer == error) ->
	    {error, invalid_jid};
	true ->
	    %% TODO: why is this a transaction?
	    F = fun() ->
			gen_storage:write(LServer,
					  #passwd{us = US,
						  password = Password})
		end,
	    {atomic, ok} = gen_storage:transaction(LServer, passwd, F),
	    ok
    end.

try_register(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    if
	(LUser == error) or (LServer == error) ->
	    {error, invalid_jid};
	true ->
	    F = fun() ->
			case gen_storage:read(LServer, {passwd, US}) of
			    [] ->
				gen_storage:write(LServer,
						  #passwd{us = US,
							  password = Password}),
				ok;
			    [_E] ->
				exists
			end
		end,
	    %% TODO: transaction retval?
	    gen_storage:transaction(LServer, passwd, F)
    end.

%% Get all registered users in Mnesia
dirty_get_registered_users() ->
    %% TODO:
    exit(not_implemented).

get_vh_registered_users(Server) ->
    LServer = jlib:nameprep(Server),
    lists:map(fun(#passwd{us = US}) ->
		      US
	      end,
	      gen_storage:dirty_select(LServer, passwd,
				       [{'=', us, {'_', LServer}}])).

get_vh_registered_users(Server, [{from, Start}, {to, End}])
  when is_integer(Start) and is_integer(End) ->
    get_vh_registered_users(Server, [{limit, End-Start+1}, {offset, Start}]);

get_vh_registered_users(Server, [{limit, Limit}, {offset, Offset}])
  when is_integer(Limit) and is_integer(Offset) ->
    case get_vh_registered_users(Server) of
	[] ->
	    [];
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
  when is_list(Prefix) ->
    Set = [{U,S} || {U, S} <- get_vh_registered_users(Server), lists:prefix(Prefix, U)],
    lists:keysort(1, Set);

get_vh_registered_users(Server, [{prefix, Prefix}, {from, Start}, {to, End}])
  when is_list(Prefix) and is_integer(Start) and is_integer(End) ->
    get_vh_registered_users(Server, [{prefix, Prefix}, {limit, End-Start+1}, {offset, Start}]);

get_vh_registered_users(Server, [{prefix, Prefix}, {limit, Limit}, {offset, Offset}])
  when is_list(Prefix) and is_integer(Limit) and is_integer(Offset) ->
    case [{U,S} || {U, S} <- get_vh_registered_users(Server), lists:prefix(Prefix, U)] of
	[] ->
	    [];
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
    Set = get_vh_registered_users(Server),
    length(Set).

get_vh_registered_users_number(Server, [{prefix, Prefix}]) when is_list(Prefix) ->
    Set = [{U, S} || {U, S} <- get_vh_registered_users(Server), lists:prefix(Prefix, U)],
    length(Set);

get_vh_registered_users_number(Server, _) ->
    get_vh_registered_users_number(Server).

get_password(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch gen_storage:dirty_read(LServer, passwd, US) of
	[#passwd{password = Password}] ->
	    Password;
	_ ->
	    false
    end.

get_password_s(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch gen_storage:dirty_read(LServer, passwd, US) of
	[#passwd{password = Password}] ->
	    Password;
	_ ->
	    []
    end.

is_user_exists(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch gen_storage:dirty_read(LServer, {passwd, US}) of
	[] ->
	    false;
	[_] ->
	    true;
	_ ->
	    false
    end.

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		gen_storage:delete(LServer, {passwd, US})
        end,
    gen_storage:transaction(LServer, passwd, F),
    ejabberd_hooks:run(remove_user, LServer, [User, Server]).

remove_user(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		case gen_storage:read(LServer, {passwd, US}) of
		    [#passwd{password = Password}] ->
			gen_storage:delete(LServer, {passwd, US}),
			ok;
		    [_] ->
			not_allowed;
		    _ ->
			not_exists
		end
        end,
    case gen_storage:transaction(LServer, passwd, F) of
	{atomic, ok} ->
	    ejabberd_hooks:run(remove_user, LServer, [User, Server]),
	    ok;
	{atomic, Res} ->
	    Res;
	_ ->
	    bad_request
    end.

update_table(Host) ->
    gen_storage_migration:migrate_mnesia(
      Host, passwd,
      [{passwd, [user, password],
	fun({passwd, User, Password}) ->
		#passwd{us = {User, Host},
			password = Password}
	end}]),
    gen_storage_migration:migrate_odbc(
      Host, [passwd],
      [{"users", ["username", "password"],
	fun(_, User, Password) ->
		#passwd{us = {User, Host},
			password = Password}
	end}]).
