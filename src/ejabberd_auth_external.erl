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
    extauth:start(ejabberd_config:get_local_option(extauth_program)),
    ok.

plain_password_required() ->
    true.

check_password(User, Password) ->
    extauth:check_password(User, Password).

check_password(User, Password, _StreamID, _Digest) ->
    check_password(User, Password).

set_password(User, Password) ->
    extauth:set_password(User, Password).

try_register(_User, _Password) ->
    {error, not_allowed}.

dirty_get_registered_users() ->
    [].

get_password(_User) ->
    false.

get_password_s(_User) ->
    "".

is_user_exists(User) ->
    extauth:is_user_exists(User).

remove_user(_User) ->
    {error, not_allowed}.

remove_user(_User, _Password) ->
    not_allowed.

