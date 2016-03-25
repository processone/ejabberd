%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_mojoauth.erl
%%% Author  : Ben Langfeld <ben@langfeld.me>
%%% Purpose : Authentication via MojoAuth (http://mojoauth.mojolingo.com/)
%%% Created : 18 February 2015 by Ben Langfeld <ben@langfeld.me>
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

-module(ejabberd_auth_mojoauth).

-author('ben@langfeld.me').

-behaviour(ejabberd_auth).

%% External exports
-export([start/1, set_password/3, check_password/4,
		 check_password/6, try_register/3,
		 dirty_get_registered_users/0, get_vh_registered_users/1,
		 get_vh_registered_users/2,
		 get_vh_registered_users_number/1,
		 get_vh_registered_users_number/2, get_password/2,
		 get_password_s/2, is_user_exists/2, remove_user/2,
		 remove_user/3, store_type/0,
		 plain_password_required/0]).

-include("ejabberd.hrl").
-include("logger.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
	ejabberd_auth_internal:start(Host).

plain_password_required() -> true.

store_type() -> external.

secret(Server) ->
  LServer = jlib:nameprep(Server),
	ejabberd_config:get_option(
		{mojoauth_secret, LServer},
		fun(V) -> iolist_to_binary(V) end,
		"mojoauth").

check_password(User, AuthzId, Server, Password) ->
	case mojoauth:test_credentials([{username, User}, {password, Password}], secret(Server)) of
		{ok, AuthzId} -> true;
		_ -> false
	end.

check_password(User, AuthzId, Server, Password, _Digest, _DigestGen) ->
	check_password(User, AuthzId, Server, Password).

set_password(_User, _Server, _Password) -> {error, not_allowed}.

try_register(_User, _Server, _Password) -> {error, not_allowed}.

dirty_get_registered_users() ->
	ejabberd_auth_internal:dirty_get_registered_users().

get_vh_registered_users(Server) ->
	ejabberd_auth_internal:get_vh_registered_users(Server).

get_vh_registered_users(Server, Data) ->
	ejabberd_auth_internal:get_vh_registered_users(Server, Data).

get_vh_registered_users_number(Server) ->
	ejabberd_auth_internal:get_vh_registered_users_number(Server).

get_vh_registered_users_number(Server, Data) ->
	ejabberd_auth_internal:get_vh_registered_users_number(Server, Data).

get_password(_User, _Server) -> false.

get_password_s(_User, _Server) -> <<"">>.

is_user_exists(_User, _Server) -> true.

remove_user(_User, _Server) -> false.

remove_user(_User, _Server, _Password) -> false.
