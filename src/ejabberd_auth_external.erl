%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_external.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via LDAP external script
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

-module(ejabberd_auth_external).
-author('alexey@process-one.net').

%% External exports
-export([start/1,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_password/2,
	 get_password_s/2,
	 is_user_exists/2,
	 remove_user/2,
	 remove_user/3,
	 plain_password_required/0
	]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @spec (Host) -> ok
%%     Host = string()

start(Host) ->
    extauth:start(
      Host, ejabberd_config:get_local_option({extauth_program, Host})),
    ok.

%% @spec () -> bool()

plain_password_required() ->
    true.

%% @spec (User, Server, Password) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()

check_password(User, Server, Password) ->
    extauth:check_password(User, Server, Password) andalso Password /= "".

%% @spec (User, Server, Password, Digest, DigestGen) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()
%%     Digest = string()
%%     DigestGen = function()

check_password(User, Server, Password, _Digest, _DigestGen) ->
    check_password(User, Server, Password).

%% @spec (User, Server, Password) -> ok | {error, unknown_problem}
%%     User = string()
%%     Server = string()
%%     Password = string()

set_password(User, Server, Password) ->
    case extauth:set_password(User, Server, Password) of
	true -> ok;
	_ -> {error, unknown_problem}
    end.

%% @spec (User, Server, Password) -> {error, not_allowed}
%%     User = string()
%%     Server = string()
%%     Password = string()

try_register(_User, _Server, _Password) ->
    {error, not_allowed}.

%% @spec () -> nil()
%% @todo Write it.
%% @doc Return the list of all users handled by external.

dirty_get_registered_users() ->
    [].

%% @spec (Server) -> nil()
%%     Server = string()

get_vh_registered_users(_Server) ->
    [].

%% @spec (User, Server) -> bool()
%%     User = string()
%%     Server = string()

get_password(_User, _Server) ->
    false.

%% @spec (User, Server) -> nil()
%%     User = string()
%%     Server = string()

get_password_s(_User, _Server) ->
    "".

%% @spec (User, Server) -> bool()
%%     User = string()
%%     Server = string()

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    try extauth:is_user_exists(User, Server) of
	Res -> Res
    catch
	_:Error -> {error, Error}
    end.

%% @spec (User, Server) -> {error, not_allowed}
%%     User = string()
%%     Server = string()

remove_user(_User, _Server) ->
    {error, not_allowed}.

%% @spec (User, Server, Password) -> not_allowed
%%     User = string()
%%     Server = string()
%%     Password = string()

remove_user(_User, _Server, _Password) ->
    not_allowed.

