%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_anonymous.erl
%%% Author  : Mickael Remond <mickael.remond@process-one.net>
%%% Purpose : Anonymous feature support in ejabberd
%%% Created : 17 Feb 2006 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth_anonymous).
-author('mickael.remond@process-one.net').

-export([start/1,
	 stop/1,
	 allow_anonymous/1,
	 is_sasl_anonymous_enabled/1,
	 is_login_anonymous_enabled/1,
	 anonymous_user_exist/2,
	 allow_multiple_connections/1,
	 register_connection/3,
	 unregister_connection/3
	]).


%% Function used by ejabberd_auth:
-export([login/2,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_password/2,
	 get_password/3,
	 is_user_exists/2,
	 remove_user/2,
	 remove_user/3,
	 store_type/0,
	 plain_password_required/0]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-record(anonymous, {us, sid}).

%% @spec (Host) -> ok
%%     Host = string()
%% @doc Create the anonymous table if at least one virtual host has
%% anonymous features enabled.
%% Register to login / logout events.

start(Host) when is_list(Host) ->
    HostB = list_to_binary(Host),
    %% TODO: Check cluster mode
    update_tables(),
    mnesia:create_table(anonymous, [{ram_copies, [node()]},
				    {type, bag}, {local_content, true},
                                    {attributes, record_info(fields, anonymous)}]),
    mnesia:add_table_copy(anonymous, node(), ram_copies),
    %% The hooks are needed to add / remove users from the anonymous tables
    ejabberd_hooks:add(sm_register_connection_hook, HostB,
		       ?MODULE, register_connection, 100),
    ejabberd_hooks:add(sm_remove_connection_hook, HostB,
		       ?MODULE, unregister_connection, 100),
    ok.

stop(Host) when is_list(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(sm_register_connection_hook, HostB,
		       ?MODULE, register_connection, 100),
    ejabberd_hooks:delete(sm_remove_connection_hook, HostB,
		       ?MODULE, unregister_connection, 100),
    ok.

%% @spec (Host) -> bool()
%%     Host = string()
%% @doc Return true if anonymous is allowed for host or false otherwise.

allow_anonymous(Host) when is_list(Host) ->
    lists:member(?MODULE, ejabberd_auth:auth_modules(Host)).

%% @spec (Host) -> bool()
%%     Host = string()
%% @doc Return true if anonymous mode is enabled and if anonymous
%% protocol is SASL anonymous.
%% protocol can be: sasl_anon|login_anon|both

is_sasl_anonymous_enabled(Host) when is_list(Host) ->
    case allow_anonymous(Host) of
	false -> false;
	true ->
	    case anonymous_protocol(Host) of
		sasl_anon -> true;
		both      -> true;
		_Other    -> false
	    end
    end.

%% @spec (Host) -> bool()
%%     Host = string()
%% @doc Return true if anonymous login is enabled on the server.
%% anonymous login can be use using standard authentication method (i.e. with
%% clients that do not support anonymous login)

is_login_anonymous_enabled(Host) when is_list(Host) ->
    case allow_anonymous(Host) of
	false -> false;
	true  ->
	    case anonymous_protocol(Host) of
		login_anon -> true;
		both       -> true;
		_Other     -> false
	    end
    end.

%% @spec (Host) -> sasl_anon | login_anon | both
%%     Host = string()
%% @doc Return the anonymous protocol to use: sasl_anon|login_anon|both.
%% defaults to login_anon

anonymous_protocol(Host) when is_list(Host) ->
    case ejabberd_config:get_local_option({anonymous_protocol, Host}) of
	sasl_anon  -> sasl_anon;
	login_anon -> login_anon;
	both       -> both;
	_Other     -> sasl_anon
    end.

%% @spec (Host) -> bool()
%%     Host = string()
%% @doc Return true if multiple connections have been allowed in the
%% config file.
%% defaults to false

allow_multiple_connections(Host) when is_list(Host) ->
    ejabberd_config:get_local_option({allow_multiple_connections, Host})
        =:= true.

%% @spec (User, Server) -> bool()
%%     User = string()
%%     Server = string()
%% @doc Check if user exist in the anonymous database.

anonymous_user_exist(User, Server) when is_list(User), is_list(Server) ->
    anonymous_user_exist(list_to_binary(User), list_to_binary(Server));
anonymous_user_exist(User, Server) when is_binary(User), is_binary(Server) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read({anonymous, US}) of
	[] ->
	    false;
	[_H|_T] ->
	    true
    end.

%% @spec (SID, LUser, LServer) -> term()
%%     SID = term()
%%     LUser = string()
%%     LServer = string()
%% @doc Remove connection from Mnesia tables.

remove_connection(SID, LUser, LServer) when is_binary(LUser), is_binary(LServer) ->
    US = {LUser, LServer},
    F = fun() ->
		mnesia:delete_object({anonymous, US, SID})
        end,
    mnesia:async_dirty(F).

%% @spec (SID, JID, Info) -> term()
%%     SID = term()
%%     JID = exmpp_jid:jid()
%%     Info = [term()]
%% @doc Register connection.

register_connection(SID, JID, Info) when ?IS_JID(JID) ->
    LUser = exmpp_jid:prep_node(JID),
    LServer = exmpp_jid:prep_domain(JID),
    case proplists:get_value(auth_module, Info) of
        undefined ->
            ok;
        ?MODULE ->
	    ejabberd_hooks:run(register_user, LServer, [LUser, LServer]),
	    US = {LUser, LServer},
	    mnesia:async_dirty(
	      fun() -> mnesia:write(#anonymous{us = US, sid=SID})
	      end);
        _ ->
            ok
    end.

%% @spec (SID, JID, Ignored) -> term()
%%     SID = term()
%%     JID = exmpp_jid:jid()
%%     Ignored = term()
%% @doc Remove an anonymous user from the anonymous users table.

unregister_connection(SID, JID, _) when ?IS_JID(JID) ->
    LUser = exmpp_jid:prep_node(JID),
    LServer = exmpp_jid:prep_domain(JID),
    purge_hook(anonymous_user_exist(LUser, LServer),
	       LUser, LServer),
    remove_connection(SID, LUser, LServer).

%% @spec (bool(), LUser, LServer) -> term()
%%     LUser = string()
%%     LServer = string()
%% @doc Launch the hook to purge user data only for anonymous users.

purge_hook(false, _LUser, _LServer) ->
    ok;
purge_hook(true, LUser, LServer) when is_binary(LUser), is_binary(LServer) ->
    ejabberd_hooks:run(anonymous_purge_hook, LServer, [LUser, LServer]).

%% ---------------------------------
%% Specific anonymous auth functions
%% ---------------------------------

%% @spec (User, Server, Password) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()
%% @doc When anonymous login is enabled, check the password for
%% permenant users before allowing access.

check_password(User, Server, Password) ->
    check_password(User, Server, Password, undefined, undefined).
check_password(User, Server, _Password, _Digest, _DigestGen) ->
    %% We refuse login for registered accounts (They cannot logged but
    %% they however are "reserved")
    case ejabberd_auth:is_user_exists_in_other_modules(?MODULE,
						       User, Server) of
	%% If user exists in other module, reject anonnymous authentication
	true  -> false;
	%% If we are not sure whether the user exists in other module, reject anon auth
	maybe  -> false;
	false -> login(User, Server)
    end.

%% @spec (User, Server) -> bool()
%%     User = string()
%%     Server = string()

login(User, Server) ->
    case is_login_anonymous_enabled(Server) of
	false -> false;
	true  ->
	    case anonymous_user_exist(User, Server) of
		%% Reject the login if an anonymous user with the same login
		%% is already logged and if multiple login has not been enable
		%% in the config file.
		true  -> allow_multiple_connections(Server);
		%% Accept login and add user to the anonymous table
		false -> true
	    end
    end.

%% @spec (User, Server, Password) -> ok | {error, not_allowed}
%%     User = string()
%%     Server = string()
%%     Password = string()
%% @doc When anonymous login is enabled, check that the user is
%% permanent before changing its password.

set_password(User, Server, _Password) ->
    case anonymous_user_exist(User, Server) of
	true ->
	    ok;
	false ->
	    {error, not_allowed}
    end.

%% @spec (User, Server, Password) -> {error, not_allowed}
%%     User = string()
%%     Server = string()
%%     Password = string()
%% @doc When anonymous login is enabled, check if permanent users are
%% allowed on the server:

try_register(_User, _Server, _Password) ->
    {error, not_allowed}.

%% @spec () -> nil()

dirty_get_registered_users() ->
    [].

%% @spec (Server) -> nil()
%%     Server = string()

get_vh_registered_users(Server) ->
    [{U, S} || {U, S, _R} <- ejabberd_sm:get_vh_session_list(list_to_binary(Server))].

%% @spec (User, Server) -> Password | false
%%     User = string()
%%     Server = string()
%%     Password = nil()
%% @doc Return password of permanent user or false for anonymous users.

get_password(User, Server) ->
    get_password(User, Server, "").

%% @spec (User, Server, DefaultValue) -> DefaultValue | false
%%     User = string()
%%     Server = string()
%%     DefaultValue = string()

get_password(User, Server, DefaultValue) ->
    case anonymous_user_exist(User, Server) or login(User, Server) of
	%% We return the default value if the user is anonymous
	true ->
	    DefaultValue;
	%% We return the permanent user password otherwise
	false ->
	    false
    end.

%% @spec (User, Server) -> bool()
%%     User = string()
%%     Server = string()
%% @doc Returns true if the user exists in the DB or if an anonymous
%% user is logged under the given name.

is_user_exists(User, Server) ->
    anonymous_user_exist(User, Server).

%% @spec (User, Server) -> {error, not_allowed}
%%     User = string()
%%     Server = string()

remove_user(_User, _Server) ->
    {error, not_allowed}.

%% @spec (User, Server, Password) -> {error, not_allowed}
%%     User = string()
%%     Server = string()
%%     Password = string()

remove_user(_User, _Server, _Password) ->
    not_allowed.

%% @spec () -> bool()

plain_password_required() ->
    false.

store_type() ->
	plain.

update_tables() ->
    case catch mnesia:table_info(anonymous, local_content) of
	false ->
	    mnesia:delete_table(anonymous);
	_ ->
	    ok
    end.
