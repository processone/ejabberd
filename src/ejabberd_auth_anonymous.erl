%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_anonymous.erl
%%% Author  : Mickael Remond <mickael.remond@process-one.net>
%%% Purpose : Anonymous feature support in ejabberd
%%% Created : 17 Feb 2006 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-module(ejabberd_auth_anonymous).

-behaviour(ejabberd_config).
-author('mickael.remond@process-one.net').

-export([start/1,
	 allow_anonymous/1,
	 is_sasl_anonymous_enabled/1,
	 is_login_anonymous_enabled/1,
	 anonymous_user_exist/2,
	 allow_multiple_connections/1,
	 register_connection/3,
	 unregister_connection/3
	]).

-export([login/2, set_password/3, check_password/4,
	 check_password/6, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2, get_password_s/2,
	 get_password/2, get_password/3, is_user_exists/2,
	 remove_user/2, remove_user/3, store_type/0,
	 plain_password_required/0, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jid.hrl").

start(Host) ->
    ejabberd_hooks:add(sm_register_connection_hook, Host,
		       ?MODULE, register_connection, 100),
    ejabberd_hooks:add(sm_remove_connection_hook, Host,
		       ?MODULE, unregister_connection, 100),
    ok.

%% Return true if anonymous is allowed for host or false otherwise
allow_anonymous(Host) ->
    lists:member(?MODULE, ejabberd_auth:auth_modules(Host)).

%% Return true if anonymous mode is enabled and if anonymous protocol is SASL
%% anonymous protocol can be: sasl_anon|login_anon|both
is_sasl_anonymous_enabled(Host) ->
    case allow_anonymous(Host) of
      false -> false;
      true ->
	  case anonymous_protocol(Host) of
	    sasl_anon -> true;
	    both -> true;
	    _Other -> false
	  end
    end.

%% Return true if anonymous login is enabled on the server
%% anonymous login can be use using standard authentication method (i.e. with
%% clients that do not support anonymous login)
is_login_anonymous_enabled(Host) ->
    case allow_anonymous(Host) of
      false -> false;
      true ->
	  case anonymous_protocol(Host) of
	    login_anon -> true;
	    both -> true;
	    _Other -> false
	  end
    end.

%% Return the anonymous protocol to use: sasl_anon|login_anon|both
%% defaults to login_anon
anonymous_protocol(Host) ->
    ejabberd_config:get_option(
      {anonymous_protocol, Host},
      fun(sasl_anon) -> sasl_anon;
         (login_anon) -> login_anon;
         (both) -> both
      end,
      sasl_anon).

%% Return true if multiple connections have been allowed in the config file
%% defaults to false
allow_multiple_connections(Host) ->
    ejabberd_config:get_option(
      {allow_multiple_connections, Host},
      fun(V) when is_boolean(V) -> V end,
      false).

anonymous_user_exist(User, Server) ->
    lists:any(
      fun({_LResource, Info}) ->
	      proplists:get_value(auth_module, Info) == ?MODULE
      end, ejabberd_sm:get_user_info(User, Server)).

%% Register connection
-spec register_connection(ejabberd_sm:sid(), jid(), ejabberd_sm:info()) -> ok.
register_connection(_SID,
		    #jid{luser = LUser, lserver = LServer}, Info) ->
    case proplists:get_value(auth_module, Info) of
	?MODULE ->
	    ejabberd_hooks:run(register_user, LServer, [LUser, LServer]);
	_ ->
	    ok
    end.

%% Remove an anonymous user from the anonymous users table
-spec unregister_connection(ejabberd_sm:sid(), jid(), ejabberd_sm:info()) -> any().
unregister_connection(_SID,
		      #jid{luser = LUser, lserver = LServer}, Info) ->
    case proplists:get_value(auth_module, Info) of
	?MODULE ->
	    ejabberd_hooks:run(remove_user, LServer, [LUser, LServer]);
	_ ->
	    ok
    end.

%% ---------------------------------
%% Specific anonymous auth functions
%% ---------------------------------

%% When anonymous login is enabled, check the password for permenant users
%% before allowing access
check_password(User, AuthzId, Server, Password) ->
    check_password(User, AuthzId, Server, Password, undefined,
		   undefined).

check_password(User, _AuthzId, Server, _Password, _Digest,
	       _DigestGen) ->
    case
      ejabberd_auth:is_user_exists_in_other_modules(?MODULE,
						    User, Server)
	of
      %% If user exists in other module, reject anonnymous authentication
      true -> false;
      %% If we are not sure whether the user exists in other module, reject anon auth
      maybe -> false;
      false -> login(User, Server)
    end.

login(User, Server) ->
    case is_login_anonymous_enabled(Server) of
      false -> false;
      true ->
	  case anonymous_user_exist(User, Server) of
	    %% Reject the login if an anonymous user with the same login
	    %% is already logged and if multiple login has not been enable
	    %% in the config file.
	    true -> allow_multiple_connections(Server);
	    %% Accept login and add user to the anonymous table
	    false -> true
	  end
    end.

%% When anonymous login is enabled, check that the user is permanent before
%% changing its password
set_password(User, Server, _Password) ->
    case anonymous_user_exist(User, Server) of
      true -> ok;
      false -> {error, not_allowed}
    end.

%% When anonymous login is enabled, check if permanent users are allowed on
%% the server:
try_register(_User, _Server, _Password) ->
    {error, not_allowed}.

dirty_get_registered_users() -> [].

get_vh_registered_users(Server) ->
    [{U, S}
     || {U, S, _R}
	    <- ejabberd_sm:get_vh_session_list(Server)].

get_vh_registered_users(Server, _) ->
    get_vh_registered_users(Server).

get_vh_registered_users_number(Server) ->
    length(get_vh_registered_users(Server)).

get_vh_registered_users_number(Server, _) ->
    get_vh_registered_users_number(Server).

%% Return password of permanent user or false for anonymous users
get_password(User, Server) ->
    get_password(User, Server, <<"">>).

get_password(User, Server, DefaultValue) ->
    case anonymous_user_exist(User, Server) or
	   login(User, Server)
	of
      %% We return the default value if the user is anonymous
      true -> DefaultValue;
      %% We return the permanent user password otherwise
      false -> false
    end.

get_password_s(User, Server) ->
    case get_password(User, Server) of
        false ->
            <<"">>;
        Password ->
            Password
    end.

is_user_exists(User, Server) ->
    anonymous_user_exist(User, Server).

remove_user(_User, _Server) -> {error, not_allowed}.

remove_user(_User, _Server, _Password) -> not_allowed.

plain_password_required() -> false.

store_type() ->
	plain.

opt_type(allow_multiple_connections) ->
    fun (V) when is_boolean(V) -> V end;
opt_type(anonymous_protocol) ->
    fun (sasl_anon) -> sasl_anon;
	(login_anon) -> login_anon;
	(both) -> both
    end;
opt_type(_) ->
    [allow_multiple_connections, anonymous_protocol].
