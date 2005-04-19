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

-include("ejabberd.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    (auth_module()):start().

plain_password_required() ->
    (auth_module()):plain_password_required().

check_password(User, Server, Password) ->
    (auth_module()):check_password(User, Server, Password).

check_password(User, Server, Password, StreamID, Digest) ->
    (auth_module()):check_password(User, Server, Password, StreamID, Digest).

set_password(User, Server, Password) ->
    (auth_module()):set_password(User, Server, Password).

try_register(User, Server, Password) ->
    case lists:member(jlib:nameprep(Server), ?MYHOSTS) of
	true ->
	    (auth_module()):try_register(User, Server, Password);
	false ->
	    {error, not_allowed}
    end.

dirty_get_registered_users() ->
    (auth_module()):dirty_get_registered_users().

get_vh_registered_users(Server) ->
    (auth_module()):get_vh_registered_users(Server).

get_password(User, Server) ->
    (auth_module()):get_password(User, Server).

get_password_s(User, Server) ->
    (auth_module()):get_password_s(User, Server).

is_user_exists(User, Server) ->
    (auth_module()):is_user_exists(User, Server).

remove_user(User, Server) ->
    (auth_module()):remove_user(User, Server).

remove_user(User, Server, Password) ->
    (auth_module()):remove_user(User, Server, Password).

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

