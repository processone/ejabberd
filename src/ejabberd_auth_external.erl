%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_external.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via LDAP external script
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2019   ProcessOne
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

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-behaviour(ejabberd_auth).

-export([start/1, stop/1, reload/1, set_password/3, check_password/4,
	 try_register/3, user_exists/2, remove_user/2,
	 store_type/1, plain_password_required/1, opt_type/1]).

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
	    false;
       true ->
	    check_password_extauth(User, AuthzId, Server, Password)
    end.

set_password(User, Server, Password) ->
    case extauth:set_password(User, Server, Password) of
	Res when is_boolean(Res) -> ok;
	{error, Reason} -> failure(User, Server, set_password, Reason)
    end.

try_register(User, Server, Password) ->
    case extauth:try_register(User, Server, Password) of
	true -> ok;
	false -> {error, not_allowed};
	{error, Reason} -> failure(User, Server, try_register, Reason)
    end.

user_exists(User, Server) ->
    case extauth:user_exists(User, Server) of
	Res when is_boolean(Res) -> Res;
	{error, Reason} -> failure(User, Server, user_exists, Reason)
    end.

remove_user(User, Server) ->
    case extauth:remove_user(User, Server) of
	false -> {error, not_allowed};
	true -> ok;
	{error, Reason} -> failure(User, Server, remove_user, Reason)
    end.

check_password_extauth(User, _AuthzId, Server, Password) ->
    if Password /= <<"">> ->
	    case extauth:check_password(User, Server, Password) of
		Res when is_boolean(Res) -> Res;
		{error, Reason} ->
		    failure(User, Server, check_password, Reason),
		    false
	    end;
       true ->
	    false
    end.

-spec failure(binary(), binary(), atom(), any()) -> {error, db_failure}.
failure(User, Server, Fun, Reason) ->
    ?ERROR_MSG("External authentication program failed when calling "
	       "'~s' for ~s@~s: ~p", [Fun, User, Server, Reason]),
    {error, db_failure}.

opt_type(extauth_cache) ->
    ?WARNING_MSG("option 'extauth_cache' is deprecated and has no effect, "
		 "use authentication or global cache configuration "
		 "options: auth_use_cache, auth_cache_life_time, "
		 "use_cache, cache_life_time, and so on", []),
    fun (false) -> false;
	(I) when is_integer(I), I >= 0 -> I
    end;
opt_type(extauth_instances) ->
    ?WARNING_MSG("option 'extauth_instances' is deprecated and has no effect, "
		 "use 'extauth_pool_size'", []),
    fun (V) when is_integer(V), V > 0 -> V end;
opt_type(extauth_program) ->
    fun (V) -> binary_to_list(iolist_to_binary(V)) end;
opt_type(extauth_pool_name) ->
    fun (V) -> iolist_to_binary(V) end;
opt_type(extauth_pool_size) ->
    fun(I) when is_integer(I), I>0 -> I end;
opt_type(_) ->
    [extauth_program, extauth_pool_size, extauth_pool_name,
     %% Deprecated:
     extauth_cache, extauth_instances].
