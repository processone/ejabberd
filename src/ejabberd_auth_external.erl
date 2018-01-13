%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_external.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via LDAP external script
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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

-export([start/1, stop/1, set_password/3, check_password/4,
	 try_register/3, user_exists/2, remove_user/2,
	 store_type/1, plain_password_required/1, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    Cmd = ejabberd_config:get_option({extauth_program, Host}, "extauth"),
    extauth:start(Host, Cmd).

stop(Host) ->
    extauth:stop(Host).

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
	true -> ok;
	_ -> {error, db_failure}
    end.

try_register(User, Server, Password) ->
    extauth:try_register(User, Server, Password).

user_exists(User, Server) ->
    try extauth:user_exists(User, Server) of
	Res -> Res
    catch
	_:Error ->
	    ?ERROR_MSG("external authentication program failure: ~p",
		       [Error]),
	    {error, db_failure}
    end.

remove_user(User, Server) ->
    case extauth:remove_user(User, Server) of
	false -> {error, not_allowed};
	true -> ok
    end.

check_password_extauth(User, _AuthzId, Server, Password) ->
    extauth:check_password(User, Server, Password) andalso
      Password /= <<"">>.

-spec opt_type(extauth_cache) -> fun((false | non_neg_integer()) ->
				      false | non_neg_integer());
	      (extauth_program) -> fun((binary()) -> string());
	      (atom()) -> [atom()].
opt_type(extauth_cache) ->
    ?WARNING_MSG("option 'extauth_cache' is deprecated and has no effect, "
		 "use authentication or global cache configuration "
		 "options: auth_use_cache, auth_cache_life_time, "
		 "use_cache, cache_life_time, and so on", []),
    fun (false) -> false;
	(I) when is_integer(I), I >= 0 -> I
    end;
opt_type(extauth_program) ->
    fun (V) -> binary_to_list(iolist_to_binary(V)) end;
opt_type(_) -> [extauth_cache, extauth_program].
