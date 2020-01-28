%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_external.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentication via LDAP external script
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2020   ProcessOne
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

-module(ejabberd_auth_external).

-author('alexey@process-one.net').

-behaviour(ejabberd_auth).

-export([start/1, stop/1, reload/1, set_password/3, check_password/4,
	 try_register/3, user_exists/2, remove_user/2,
	 store_type/1, plain_password_required/1]).

-include("logger.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    extauth:start(Host).

stop(Host) ->
    extauth:stop(Host).

reload(Host) ->
    extauth:reload(Host).

plain_password_required(_) -> true.

store_type(_) -> external.

check_password(User, AuthzId, Server, Password) ->
    if AuthzId /= <<>> andalso AuthzId /= User ->
	    {nocache, false};
       true ->
	    check_password_extauth(User, AuthzId, Server, Password)
    end.

set_password(User, Server, Password) ->
    case extauth:set_password(User, Server, Password) of
	Res when is_boolean(Res) -> {cache, {ok, Password}};
	{error, Reason} -> failure(User, Server, set_password, Reason)
    end.

try_register(User, Server, Password) ->
    case extauth:try_register(User, Server, Password) of
	true -> {cache, {ok, Password}};
	false -> {cache, {error, not_allowed}};
	{error, Reason} -> failure(User, Server, try_register, Reason)
    end.

user_exists(User, Server) ->
    case extauth:user_exists(User, Server) of
	Res when is_boolean(Res) -> {cache, Res};
	{error, Reason} -> failure(User, Server, user_exists, Reason)
    end.

remove_user(User, Server) ->
    case extauth:remove_user(User, Server) of
	false -> {error, not_allowed};
	true -> ok;
	{error, Reason} ->
	    {_, Err} = failure(User, Server, remove_user, Reason),
	    Err
    end.

check_password_extauth(User, _AuthzId, Server, Password) ->
    if Password /= <<"">> ->
	    case extauth:check_password(User, Server, Password) of
		Res when is_boolean(Res) -> {cache, Res};
		{error, Reason} ->
		    {Tag, _} = failure(User, Server, check_password, Reason),
		    {Tag, false}
	    end;
       true ->
	    {nocache, false}
    end.

-spec failure(binary(), binary(), atom(), any()) -> {nocache, {error, db_failure}}.
failure(User, Server, Fun, Reason) ->
    ?ERROR_MSG("External authentication program failed when calling "
	       "'~ts' for ~ts@~ts: ~p", [Fun, User, Server, Reason]),
    {nocache, {error, db_failure}}.
