%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via ODBC
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
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

-module(ejabberd_auth_odbc).
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

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @spec (Host) -> ok
%%     Host = string()

start(_Host) ->
    ok.

%% @spec () -> bool()

plain_password_required() ->
    false.

%% @spec (User, Server, Password) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()

check_password(User, Server, Password) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	Username = ejabberd_odbc:escape(LUser),
	LServer = exmpp_stringprep:nameprep(Server),
	try odbc_queries:get_password(LServer, Username) of
	    {selected, ["password"], [{Password}]} ->
		    Password /= ""; %% Password is correct, and not empty
		{selected, ["password"], [{_Password2}]} ->
		    false; %% Password is not correct
		{selected, ["password"], []} ->
		    false; %% Account does not exist
		{error, _Error} ->
		    false %% Typical error is that table doesn't exist
	    catch
		_:_ ->
		    false %% Typical error is database not accessible
	end
    catch
	_ ->
	    false
    end.

%% @spec (User, Server, Password, Digest, DigestGen) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()
%%     Digest = string()
%%     DigestGen = function()

check_password(User, Server, Password, Digest, DigestGen) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	Username = ejabberd_odbc:escape(LUser),
	LServer = exmpp_stringprep:nameprep(Server),
	try odbc_queries:get_password(LServer, Username) of
		%% Account exists, check if password is valid
	    {selected, ["password"], [{Passwd}]} ->
		DigRes = if
		    Digest /= "" ->
				     Digest == DigestGen(Passwd);
		    true ->
			false
		end,
		if DigRes ->
			true;
		    true ->
			(Passwd == Password) and (Password /= "")
		end;
		{selected, ["password"], []} ->
		    false; %% Account does not exist
		{error, _Error} ->
		    false %% Typical error is that table doesn't exist
	    catch
		_:_ ->
		    false %% Typical error is database not accessible
	end
    catch
	_ ->
	    false
    end.

%% @spec (User, Server, Password) -> ok | {error, invalid_jid}
%%     User = string()
%%     Server = string()
%%     Password = string()

set_password(User, Server, Password) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	Username = ejabberd_odbc:escape(LUser),
	Pass = ejabberd_odbc:escape(Password),
	LServer = exmpp_stringprep:nameprep(Server),
	case catch odbc_queries:set_password_t(LServer, Username, Pass) of
	    {atomic, ok} -> ok;
	    Other -> {error, Other}
	end
    catch
	_ ->
	    {error, invalid_jid}
    end.


%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid}
%%     User = string()
%%     Server = string()
%%     Password = string()

try_register(User, Server, Password) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	Username = ejabberd_odbc:escape(LUser),
	Pass = ejabberd_odbc:escape(Password),
	LServer = exmpp_stringprep:nameprep(Server),
	case catch odbc_queries:add_user(LServer, Username, Pass) of
	    {updated, 1} ->
		{atomic, ok};
	    _ ->
		{atomic, exists}
	end
    catch
	_ ->
	    {error, invalid_jid}
    end.

%% @spec () -> [{LUser, LServer}]
%%     LUser = string()
%%     LServer = string()

dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(odbc),
    lists:flatmap(
      fun(Server) ->
	      get_vh_registered_users(Server)
      end, Servers).

%% @spec (Server) -> [{LUser, LServer}]
%%     Server = string()
%%     LUser = string()
%%     LServer = string()

get_vh_registered_users(Server) ->
    LServer = exmpp_stringprep:nameprep(Server),
    case catch odbc_queries:list_users(LServer) of
	{selected, ["username"], Res} ->
	    [{U, LServer} || {U} <- Res];
	_ ->
	    []
    end.

%% @spec (Server, Opts) -> [{LUser, LServer}]
%%     Server = string()
%%     Opts = [{Opt, Val}]
%%         Opt = atom()
%%         Val = term()
%%     LUser = string()
%%     LServer = string()

get_vh_registered_users(Server, Opts) ->
    LServer = exmpp_stringprep:nameprep(Server),
    case catch odbc_queries:list_users(LServer, Opts) of
	{selected, ["username"], Res} ->
	    [{U, LServer} || {U} <- Res];
	_ ->
	    []
    end.

%% @spec (Server) -> Users_Number
%%     Server = string()
%%     Users_Number = integer()

get_vh_registered_users_number(Server) ->
    LServer = exmpp_stringprep:nameprep(Server),
    case catch odbc_queries:users_number(LServer) of
	{selected, [_], [{Res}]} ->
	    list_to_integer(Res);
	_ ->
	    0
    end.

%% @spec (Server, Opts) -> Users_Number
%%     Server = string()
%%     Opts = [{Opt, Val}]
%%         Opt = atom()
%%         Val = term()
%%     Users_Number = integer()

get_vh_registered_users_number(Server, Opts) ->
    LServer = exmpp_stringprep:nameprep(Server),
    case catch odbc_queries:users_number(LServer, Opts) of
	{selected, [_], [{Res}]} ->
	    list_to_integer(Res);
	_Other ->
	    0
    end.

%% @spec (User, Server) -> Password | false
%%     User = string()
%%     Server = string()
%%     Password = string()

get_password(User, Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	Username = ejabberd_odbc:escape(LUser),
	LServer = exmpp_stringprep:nameprep(Server),
	case catch odbc_queries:get_password(LServer, Username) of
	    {selected, ["password"], [{Password}]} ->
		Password;
	    _ ->
		false
	end
    catch
	_ ->
	    false
    end.

%% @spec (User, Server) -> Password | nil()
%%     User = string()
%%     Server = string()
%%     Password = string()

get_password_s(User, Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	Username = ejabberd_odbc:escape(LUser),
	LServer = exmpp_stringprep:nameprep(Server),
	case catch odbc_queries:get_password(LServer, Username) of
	    {selected, ["password"], [{Password}]} ->
		Password;
	    _ ->
		""
	end
    catch
	_ ->
	    ""
    end.

%% @spec (User, Server) -> bool()
%%     User = string()
%%     Server = string()

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	Username = ejabberd_odbc:escape(LUser),
	LServer = exmpp_stringprep:nameprep(Server),
	try odbc_queries:get_password(LServer, Username) of
	    {selected, ["password"], [{_Password}]} ->
		    true; %% Account exists
		{selected, ["password"], []} ->
		    false; %% Account does not exist
		{error, Error} ->
		    {error, Error} %% Typical error is that table doesn't exist
	    catch
		_:B ->
		    {error, B} %% Typical error is database not accessible
	end
    catch
	_ ->
	    false
    end.

%% @spec (User, Server) -> ok | error
%%     User = string()
%%     Server = string()
%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.

remove_user(User, Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	Username = ejabberd_odbc:escape(LUser),
	LServer = exmpp_stringprep:nameprep(Server),
	catch odbc_queries:del_user(LServer, Username),
        ok
    catch
	_ ->
	    error
    end.

%% @spec (User, Server, Password) -> ok | error | not_exists | not_allowed
%%     User = string()
%%     Server = string()
%%     Password = string()
%% @doc Remove user if the provided password is correct.

remove_user(User, Server, Password) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	Username = ejabberd_odbc:escape(LUser),
	Pass = ejabberd_odbc:escape(Password),
	LServer = exmpp_stringprep:nameprep(Server),
	F = fun() ->
		    Result = odbc_queries:del_user_return_password(
			       LServer, Username, Pass),
		    case Result of
			{selected, ["password"], [{Password}]} ->
			    ok;
			{selected, ["password"], []} ->
			    not_exists;
			_ ->
			    not_allowed
		    end
	    end,
	{atomic, Result} = odbc_queries:sql_transaction(LServer, F),
	Result
    catch
	_ ->
	    error
    end.
