%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Authentification
%%% Created : 23 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_auth).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

%% External exports
-export([start/0,
	 set_password/2,
	 check_password/2,
	 check_password/4,
	 try_register/2,
	 dirty_get_registered_users/0,
	 get_password/1,
	 get_password_s/1,
	 is_user_exists/1,
	 remove_user/1,
	 remove_user/2,
	 plain_password_required/0
	]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    (auth_module()):start().

plain_password_required() ->
    (auth_module()):plain_password_required().

check_password(User, Password) ->
    (auth_module()):check_password(User, Password).

check_password(User, Password, StreamID, Digest) ->
    (auth_module()):check_password(User, Password, StreamID, Digest).

set_password(User, Password) ->
    (auth_module()):set_password(User, Password).

try_register(User, Password) ->
    (auth_module()):try_register(User, Password).

dirty_get_registered_users() ->
    (auth_module()):dirty_get_registered_users().

get_password(User) ->
    (auth_module()):get_password(User).

get_password_s(User) ->
    (auth_module()):get_password_s(User).

is_user_exists(User) ->
    (auth_module()):is_user_exists(User).

remove_user(User) ->
    (auth_module()):remove_user(User).

remove_user(User, Password) ->
    (auth_module()):remove_user(User, Password).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

auth_module() ->
    case ejabberd_config:get_local_option(auth_method) of
	external ->
	    ejabberd_auth_external;
	ldap ->
	    ejabberd_auth_ldap;
	odbc ->
	    ejabberd_auth_odbc;
	_ ->
	    ejabberd_auth_internal
    end.

