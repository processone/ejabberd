%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification
%%% Created : 23 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

%% TODO: Use the functions in ejabberd auth to add and remove users.

-module(ejabberd_auth).
-author('alexey@process-one.net').

%% External exports
-export([start/0,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 check_password_with_authmodule/3,
	 check_password_with_authmodule/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2,
	 get_password/2,
	 get_password_s/2,
	 get_password_with_authmodule/2,
	 is_user_exists/2,
	 is_user_exists_in_other_modules/3,
	 remove_user/2,
	 remove_user/3,
	 plain_password_required/1
	]).

-export([auth_modules/1]).

-include("ejabberd.hrl").

%% @type authmodule() = ejabberd_auth_anonymous | ejabberd_auth_external |
%%                      ejabberd_auth_internal | ejabberd_auth_ldap |
%%                      ejabberd_auth_odbc | ejabberd_auth_pam | atom().

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @spec () -> term()

start() ->
    lists:foreach(
      fun(Host) ->
	      lists:foreach(
		fun(M) ->
			M:start(Host)
		end, auth_modules(Host))
      end, ?MYHOSTS).

%% @spec (Server) -> bool()
%%     Server = string()

plain_password_required(Server) when is_list(Server) ->
    lists:any(
      fun(M) ->
	      M:plain_password_required()
      end, auth_modules(Server)).

%% @spec (User, Server, Password) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()
%% @doc Check if the user and password can login in server.

check_password(User, Server, Password)
  when is_list(User), is_list(Server), is_list(Password) ->
    lists:any(
      fun(M) ->
	      M:check_password(User, Server, Password)
      end, auth_modules(Server)).

%% @spec (User, Server, Password, StreamID, Digest) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()
%%     StreamID = string()
%%     Digest = string()
%% @doc Check if the user and password can login in server.

check_password(User, Server, Password, StreamID, Digest)
  when is_list(User), is_list(Server), is_list(Password),
  is_list(StreamID), is_list(Digest) ->
    lists:any(
      fun(M) ->
	      M:check_password(User, Server, Password, StreamID, Digest)
      end, auth_modules(Server)).

%% @spec (User, Server, Password) -> {true, AuthModule} | false
%%     User = string()
%%     Server = string()
%%     Password = string()
%%     AuthModule = authmodule()
%% @doc Check if the user and password can login in server.
%% The user can login if at least an authentication method accepts the user
%% and the password.
%% The first authentication method that accepts the credentials is returned.

check_password_with_authmodule(User, Server, Password)
  when is_list(User), is_list(Server), is_list(Password) ->
    Res = lists:dropwhile(
	    fun(M) ->
		    not apply(M, check_password,
			      [User, Server, Password])
	    end, auth_modules(Server)),
    case Res of
	[] -> false;
	[AuthMod | _] -> {true, AuthMod}
    end.

%% @spec (User, Server, Password, StreamID, Digest) -> {true, AuthModule} | false
%%     User = string()
%%     Server = string()
%%     Password = string() | undefined
%%     StreamID = string()
%%     Digest = string() | undefined
%%     AuthModule = authmodule()
%% The password is 'undefined' if the client
%% authenticates using the digest method as defined in 
%% XEP-0078: Non-SASL Authentication

check_password_with_authmodule(User, Server, Password, StreamID, Digest)
  when is_list(User), is_list(Server), (is_list(Password) orelse Password == 'undefined'),
  is_list(StreamID), (is_list(Digest) orelse Digest == 'undefined')->
    Res = lists:dropwhile(
	    fun(M) ->
		    not apply(M, check_password,
			      [User, Server, Password, StreamID, Digest])
	    end, auth_modules(Server)),
    case Res of
	[] -> false;
	[AuthMod | _] -> {true, AuthMod}
    end.

%% @spec (User, Server, Password) -> ok | {error, ErrorType}
%%     User = string()
%%     Server = string()
%%     Password = string()
%%     ErrorType = empty_password | not_allowed | invalid_jid

set_password(_User, _Server, "") ->
    %% We do not allow empty password
    {error, empty_password};
set_password(User, Server, Password)
  when is_list(User), is_list(Server), is_list(Password) ->
    lists:foldl(
      fun(M, {error, _}) ->
	      M:set_password(User, Server, Password);
	 (_M, Res) ->
	      Res
      end, {error, not_allowed}, auth_modules(Server)).

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, not_allowed}
%%     User = string()
%%     Server = string()
%%     Password = string() | nil()

try_register(_User, _Server, "") ->
    %% We do not allow empty password
    {error, not_allowed};    
try_register(User, Server, Password)
  when is_list(User), is_list(Server), is_list(Password) ->
    case is_user_exists(User,Server) of
	true ->
	    {atomic, exists};
	false ->
	    case lists:member(exmpp_stringprep:nameprep(Server), ?MYHOSTS) of
		true ->
		    Res = lists:foldl(
		      fun(_M, {atomic, ok} = Res) ->
			      Res;
			 (M, _) ->
			      M:try_register(User, Server, Password)
		      end, {error, not_allowed}, auth_modules(Server)),
		    case Res of
			{atomic, ok} ->
			    ejabberd_hooks:run(register_user, list_to_binary(Server),
					       [User, Server]),
			    {atomic, ok};
			_ -> Res
		    end;
		false ->
		    {error, not_allowed}
	    end
    end.

%% @spec () -> [{LUser, LServer}]
%%     LUser = string()
%%     LServer = string()
%% @doc Registered users list do not include anonymous users logged.

dirty_get_registered_users() ->
    lists:flatmap(
      fun(M) ->
	      M:dirty_get_registered_users()
      end, auth_modules()).

%% @spec (Server) -> [{LUser, LServer}]
%%     Server = string()
%%     LUser = string()
%%     LServer = string()
%% @doc Registered users list do not include anonymous users logged.

get_vh_registered_users(Server) when is_list(Server) ->
    lists:flatmap(
      fun(M) ->
	      M:get_vh_registered_users(Server)
      end, auth_modules(Server)).

%% @spec (Server, Opts) -> [{LUser, LServer}]
%%     Server = string()
%%     Opts = [{Opt, Val}]
%%         Opt = atom()
%%         Val = term()
%%     LUser = string()
%%     LServer = string()

get_vh_registered_users(Server, Opts) when is_list(Server) ->
    lists:flatmap(
      fun(M) ->
		case erlang:function_exported(
		       M, get_vh_registered_users_number, 2) of
		    true ->
			M:get_vh_registered_users_number(Server, Opts);
		    false ->
			M:get_vh_registered_users_number(Server)
		end
      end, auth_modules(Server)).

%% @spec (Server) -> Users_Number
%%     Server = string()
%%     Users_Number = integer()

get_vh_registered_users_number(Server) when is_list(Server) ->
    lists:sum(
      lists:map(
	fun(M) ->
		case erlang:function_exported(
		       M, get_vh_registered_users_number, 1) of
		    true ->
			M:get_vh_registered_users_number(Server);
		    false ->
			length(M:get_vh_registered_users(Server))
		end
	end, auth_modules(Server))).

%% @spec (Server, Opts) -> Users_Number
%%     Server = string()
%%     Opts = [{Opt, Val}]
%%         Opt = atom()
%%         Val = term()
%%     Users_Number = integer()

get_vh_registered_users_number(Server, Opts) when is_list(Server) ->
    lists:sum(
      lists:map(
	fun(M) ->
		case erlang:function_exported(
		       M, get_vh_registered_users_number, 2) of
		    true ->
			M:get_vh_registered_users_number(Server, Opts);
		    false ->
			length(M:get_vh_registered_users(Server))
		end
	end, auth_modules(Server))).

%% @spec (User, Server) -> Password | false
%%     User = string()
%%     Server = string()
%%     Password = string()
%% @doc Get the password of the user.

get_password(User, Server) when is_list(User), is_list(Server) ->
    lists:foldl(
      fun(M, false) ->
	      M:get_password(User, Server);
	 (_M, Password) ->
	      Password
      end, false, auth_modules(Server)).

%% @spec (User, Server) -> Password | nil()
%%     User = string()
%%     Server = string()
%%     Password = string()
%% @doc Get the password of the user.

get_password_s(User, Server) when is_list(User), is_list(Server) ->
    case get_password(User, Server) of
	false ->
	    "";
	Password ->
	    Password
    end.

%% @spec (User, Server) -> {Password, AuthModule} | {false, none}
%%     User = string()
%%     Server = string()
%%     Password = string()
%%     AuthModule = authmodule()
%% @doc Get the password of the user and the auth module.

get_password_with_authmodule(User, Server)
  when is_list(User), is_list(Server) ->
    lists:foldl(
      fun(M, {false, _}) ->
	      {M:get_password(User, Server), M};
	 (_M, {Password, AuthModule}) ->
	      {Password, AuthModule}
      end, {false, none}, auth_modules(Server)).

%% @spec (User, Server) -> bool()
%%     User = string()
%%     Server = string()
%% @doc Returns true if the user exists in the DB or if an anonymous
%% user is logged under the given name.

is_user_exists(User, Server) when is_list(User), is_list(Server) ->
    lists:any(
      fun(M) ->
	      M:is_user_exists(User, Server)
      end, auth_modules(Server)).

%% @spec (Module, User, Server) -> bool
%%     Module = authmodule()
%%     User = string()
%%     Server = string()
%% @doc Check if the user exists in all authentications module except
%% the module passed as parameter.

is_user_exists_in_other_modules(Module, User, Server)
  when is_list(User), is_list(Server) ->
    lists:any(
      fun(M) ->
	      M:is_user_exists(User, Server)
      end, auth_modules(Server)--[Module]).

%% @spec (User, Server) -> ok | error | {error, not_allowed}
%%     User = string()
%%     Server = string()
%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.

remove_user(User, Server) when is_list(User), is_list(Server) ->
    R = lists:foreach(
      fun(M) ->
	      M:remove_user(User, Server)
      end, auth_modules(Server)),
    case R of
		ok -> ejabberd_hooks:run(remove_user, list_to_binary(exmpp_stringprep:nameprep(Server)), 
                                    [list_to_binary(User), list_to_binary(Server)]);
		_ -> none
    end,
    R.

%% @spec (User, Server, Password) -> ok | not_exists | not_allowed | bad_request | error
%%     User = string()
%%     Server = string()
%%     Password = string()
%% @doc Try to remove user if the provided password is correct.
%% The removal is attempted in each auth method provided:
%% when one returns 'ok' the loop stops;
%% if no method returns 'ok' then it returns the error message indicated by the last method attempted.

remove_user(User, Server, Password)
  when is_list(User), is_list(Server), is_list(Password) ->
    R = lists:foldl(
      fun(_M, ok = Res) ->
	      Res;
	 (M, _) ->
	      M:remove_user(User, Server, Password)
      end, error, auth_modules(Server)),
    case R of
		ok -> ejabberd_hooks:run(remove_user, list_to_binary(exmpp_stringprep:nameprep(Server)), 
                                    [list_to_binary(User), list_to_binary(Server)]);
		_ -> none
    end,
    R.


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% @spec () -> [authmodule()]
%% @doc Return the lists of all the auth modules actually used in the
%% configuration.

auth_modules() ->
    lists:usort(
      lists:flatmap(
	fun(Server) ->
		auth_modules(Server)
	end, ?MYHOSTS)).

%% @spec (Server) -> [authmodule()]
%%     Server = string()
%% @doc Return the list of authenticated modules for a given host.

auth_modules(Server) when is_list(Server) ->
    LServer = exmpp_stringprep:nameprep(Server),
    Method = ejabberd_config:get_local_option({auth_method, LServer}),
    Methods = if
		  Method == undefined -> [];
		  is_list(Method) -> Method;
		  is_atom(Method) -> [Method]
	      end,
    [list_to_atom("ejabberd_auth_" ++ atom_to_list(M)) || M <- Methods].
