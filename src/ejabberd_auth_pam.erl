%%%-------------------------------------------------------------------
%%% File    : ejabberd_auth_pam.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : PAM authentication
%%% Created : 5 Jul 2007 by Evgeniy Khramtsov <xram@jabber.ru>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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

-export([start/1, stop/1, check_password/4,
	 user_exists/2, store_type/1, plain_password_required/1]).

start(_Host) ->
    ejabberd:start_app(epam).

stop(_Host) ->
    ok.

check_password(User, AuthzId, Host, Password) ->
    if AuthzId /= <<>> andalso AuthzId /= User ->
	    false;
       true ->
	    Service = get_pam_service(Host),
	    UserInfo = case get_pam_userinfotype(Host) of
			   username -> User;
			   jid -> <<User/binary, "@", Host/binary>>
		       end,
	    case catch epam:authenticate(Service, UserInfo, Password) of
		true -> {cache, true};
		false -> {cache, false};
		_ -> {nocache, false}
	    end
    end.

user_exists(User, Host) ->
    Service = get_pam_service(Host),
    UserInfo = case get_pam_userinfotype(Host) of
		 username -> User;
		 jid -> <<User/binary, "@", Host/binary>>
	       end,
    case catch epam:acct_mgmt(Service, UserInfo) of
	true -> {cache, true};
	false -> {cache, false};
	_Err -> {nocache, {error, db_failure}}
    end.

plain_password_required(_) -> true.

store_type(_) -> external.

%%====================================================================
%% Internal functions
%%====================================================================
get_pam_service(Host) ->
    ejabberd_option:pam_service(Host).

get_pam_userinfotype(Host) ->
    ejabberd_option:pam_userinfotype(Host).
