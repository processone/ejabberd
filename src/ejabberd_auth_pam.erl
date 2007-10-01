%%%-------------------------------------------------------------------
%%% File    : ejabberd_auth_pam.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : PAM authentication
%%% Created : 5 Jul 2007 by Evgeniy Khramtsov <xram@jabber.ru>
%%% Id      : $Id$
%%%-------------------------------------------------------------------
-module(ejabberd_auth_pam).
-author('xram@jabber.ru').

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

%%====================================================================
%% API
%%====================================================================
start(_Host) ->
    case epam:start() of
	{ok, _} -> ok;
	{error,{already_started, _}} -> ok;
	Err -> Err
    end.

set_password(_User, _Server, _Password) ->
    {error, not_allowed}.

check_password(User, Server, Password, _StreamID, _Digest) ->
    check_password(User, Server, Password).

check_password(User, Host, Password) ->
    Service = get_pam_service(Host),
    case catch epam:authenticate(Service, User, Password) of
	true -> true;
	_    -> false
    end.

try_register(_User, _Server, _Password) ->
    {error, not_allowed}.

dirty_get_registered_users() ->
    [].

get_vh_registered_users(_Host) ->
    [].

get_password(_User, _Server) ->
    false.

get_password_s(_User, _Server) ->
    "".

is_user_exists(User, Host) ->
    Service = get_pam_service(Host),
    case catch epam:acct_mgmt(Service, User) of
	true -> true;
	_    -> false
    end.

remove_user(_User, _Server) ->
    {error, not_allowed}.

remove_user(_User, _Server, _Password) ->
    {error, not_allowed}.

plain_password_required() ->
    true.

%%====================================================================
%% Internal functions
%%====================================================================
get_pam_service(Host) ->
    case ejabberd_config:get_local_option({pam_service, Host}) of
	undefined -> "ejabberd";
	Service   -> Service
    end.
