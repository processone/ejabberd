%%%-------------------------------------------------------------------
%%% File    : ejabberd_auth_pam.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : PAM authentication
%%% Created : 5 Jul 2007 by Evgeniy Khramtsov <xram@jabber.ru>
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
%%%-------------------------------------------------------------------
-module(ejabberd_auth_pam).

-author('xram@jabber.ru').

-behaviour(ejabberd_auth).

%% External exports
%%====================================================================
%% API
%%====================================================================
-export([start/1, set_password/3, check_password/3,
	 check_password/5, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
         get_vh_registered_users/2, get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
	 get_password/2, get_password_s/2, is_user_exists/2,
	 remove_user/2, remove_user/3, store_type/0,
	 plain_password_required/0]).

start(_Host) ->
    ejabberd:start_app(p1_pam).

set_password(_User, _Server, _Password) ->
    {error, not_allowed}.

check_password(User, Server, Password, _Digest,
	       _DigestGen) ->
    check_password(User, Server, Password).

check_password(User, Host, Password) ->
    Service = get_pam_service(Host),
    UserInfo = case get_pam_userinfotype(Host) of
		 username -> User;
		 jid -> <<User/binary, "@", Host/binary>>
	       end,
    case catch epam:authenticate(Service, UserInfo,
				 Password)
	of
      true -> true;
      _ -> false
    end.

try_register(_User, _Server, _Password) ->
    {error, not_allowed}.

dirty_get_registered_users() -> [].

get_vh_registered_users(_Host) -> [].

get_vh_registered_users(_Host, _) -> [].

get_vh_registered_users_number(_Host) -> 0.

get_vh_registered_users_number(_Host, _) -> 0.

get_password(_User, _Server) -> false.

get_password_s(_User, _Server) -> <<"">>.

%% @spec (User, Server) -> true | false | {error, Error}
%% TODO: Improve this function to return an error instead of 'false' when connection to PAM failed
is_user_exists(User, Host) ->
    Service = get_pam_service(Host),
    UserInfo = case get_pam_userinfotype(Host) of
		 username -> User;
		 jid -> <<User/binary, "@", Host/binary>>
	       end,
    case catch epam:acct_mgmt(Service, UserInfo) of
      true -> true;
      _ -> false
    end.

remove_user(_User, _Server) -> {error, not_allowed}.

remove_user(_User, _Server, _Password) -> not_allowed.

plain_password_required() -> true.

store_type() -> external.

%%====================================================================
%% Internal functions
%%====================================================================
get_pam_service(Host) ->
    ejabberd_config:get_option(
      {pam_service, Host},
      fun iolist_to_binary/1,
      <<"ejabberd">>).

get_pam_userinfotype(Host) ->
    ejabberd_config:get_option(
      {pam_userinfotype, Host},
      fun(username) -> username;
         (jid) -> jid
      end,
      username).
