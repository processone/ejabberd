%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_external.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Authentification via LDAP external script
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_auth_external).
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

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    extauth:start(ejabberd_config:get_local_option(extauth_program)),
    ok.

plain_password_required() ->
    true.

check_password(User, _Server, Password) ->
    extauth:check_password(User, Password).

check_password(User, Server, Password, _StreamID, _Digest) ->
    check_password(User, Server, Password).

set_password(User, _Server, Password) ->
    extauth:set_password(User, Password).

try_register(_User, _Server, _Password) ->
    {error, not_allowed}.

dirty_get_registered_users() ->
    [].

get_vh_registered_users(_Server) ->
    [].

get_password(_User, _Server) ->
    false.

get_password_s(_User, _Server) ->
    "".

is_user_exists(User, _Server) ->
    extauth:is_user_exists(User).

remove_user(_User, _Server) ->
    {error, not_allowed}.

remove_user(_User, _Server, _Password) ->
    not_allowed.

