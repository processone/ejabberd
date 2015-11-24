%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification
%%% Created : 23 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

%% TODO: Use the functions in ejabberd auth to add and remove users.

-module(ejabberd_auth).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

%% External exports
-export([start/0, set_password/3, check_password/3,
	 check_password/5, check_password_with_authmodule/3,
	 check_password_with_authmodule/5, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
	 get_vh_registered_users/2, export/1, import/1,
	 get_vh_registered_users_number/1, import/3,
	 get_vh_registered_users_number/2, get_password/2,
	 get_password_s/2, get_password_with_authmodule/2,
	 is_user_exists/2, is_user_exists_in_other_modules/3,
	 remove_user/2, remove_user/3, plain_password_required/1,
	 store_type/1, entropy/1]).

-export([auth_modules/1, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-type opts() :: [{prefix, binary()} | {from, integer()} |
                 {to, integer()} | {limit, integer()} |
                 {offset, integer()}].

-callback start(binary()) -> any().
-callback plain_password_required() -> boolean().
-callback store_type() -> plain | external | scram.
-callback set_password(binary(), binary(), binary()) -> ok | {error, atom()}.
-callback remove_user(binary(), binary()) -> any().
-callback remove_user(binary(), binary(), binary()) -> any().
-callback is_user_exists(binary(), binary()) -> boolean() | {error, atom()}.
-callback check_password(binary(), binary(), binary()) -> boolean().
-callback check_password(binary(), binary(), binary(), binary(),
                         fun((binary()) -> binary())) -> boolean().
-callback try_register(binary(), binary(), binary()) -> {atomic, atom()} |
                                                        {error, atom()}.
-callback dirty_get_registered_users() -> [{binary(), binary()}].
-callback get_vh_registered_users(binary()) -> [{binary(), binary()}].
-callback get_vh_registered_users(binary(), opts()) -> [{binary(), binary()}].
-callback get_vh_registered_users_number(binary()) -> number().
-callback get_vh_registered_users_number(binary(), opts()) -> number().
-callback get_password(binary(), binary()) -> false | binary() | {binary(), binary(), binary(), integer()}.
-callback get_password_s(binary(), binary()) -> binary() | {binary(), binary(), binary(), integer()}.

start() ->
    %% This is only executed by ejabberd_c2s for non-SASL auth client
    lists:foreach(fun (Host) ->
			  lists:foreach(fun (M) -> M:start(Host) end,
					auth_modules(Host))
		  end,
		  ?MYHOSTS).

plain_password_required(Server) ->
    lists:any(fun (M) -> M:plain_password_required() end,
	      auth_modules(Server)).

store_type(Server) ->
%% @doc Check if the user and password can login in server.
%% @spec (User::string(), Server::string(), Password::string()) ->
%%     true | false
    lists:foldl(fun (_, external) -> external;
		    (M, scram) ->
			case M:store_type() of
			  external -> external;
			  _Else -> scram
			end;
		    (M, plain) -> M:store_type()
		end,
		plain, auth_modules(Server)).

-spec check_password(binary(), binary(), binary()) -> boolean().

check_password(User, Server, Password) ->
    case check_password_with_authmodule(User, Server,
					Password)
	of
      {true, _AuthModule} -> true;
      false -> false
    end.

%% @doc Check if the user and password can login in server.
%% @spec (User::string(), Server::string(), Password::string(),
%%        Digest::string(), DigestGen::function()) ->
%%     true | false
-spec check_password(binary(), binary(), binary(), binary(),
                     fun((binary()) -> binary())) -> boolean().
                                 
check_password(User, Server, Password, Digest,
	       DigestGen) ->
    case check_password_with_authmodule(User, Server,
					Password, Digest, DigestGen)
	of
      {true, _AuthModule} -> true;
      false -> false
    end.

%% @doc Check if the user and password can login in server.
%% The user can login if at least an authentication method accepts the user
%% and the password.
%% The first authentication method that accepts the credentials is returned.
%% @spec (User::string(), Server::string(), Password::string()) ->
%%     {true, AuthModule} | false
%% where
%%   AuthModule = ejabberd_auth_anonymous | ejabberd_auth_external
%%                 | ejabberd_auth_internal | ejabberd_auth_ldap
%%                 | ejabberd_auth_odbc | ejabberd_auth_pam
-spec check_password_with_authmodule(binary(), binary(), binary()) -> false |
                                                                      {true, atom()}.

check_password_with_authmodule(User, Server,
			       Password) ->
    check_password_loop(auth_modules(Server),
			[User, Server, Password]).

-spec check_password_with_authmodule(binary(), binary(), binary(), binary(),
                                     fun((binary()) -> binary())) -> false |
                                                                     {true, atom()}.

check_password_with_authmodule(User, Server, Password,
			       Digest, DigestGen) ->
    check_password_loop(auth_modules(Server),
			[User, Server, Password, Digest, DigestGen]).

check_password_loop([], _Args) -> false;
check_password_loop([AuthModule | AuthModules], Args) ->
    case apply(AuthModule, check_password, Args) of
      true -> {true, AuthModule};
      false -> check_password_loop(AuthModules, Args)
    end.

-spec set_password(binary(), binary(), binary()) -> ok |
                                                    {error, atom()}.

%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, ErrorType}
%% where ErrorType = empty_password | not_allowed | invalid_jid
set_password(_User, _Server, <<"">>) ->
    {error, empty_password};
set_password(User, Server, Password) ->
%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, not_allowed}
    lists:foldl(fun (M, {error, _}) ->
			M:set_password(User, Server, Password);
		    (_M, Res) -> Res
		end,
		{error, not_allowed}, auth_modules(Server)).

-spec try_register(binary(), binary(), binary()) -> {atomic, atom()} |
                                                    {error, atom()}.

try_register(_User, _Server, <<"">>) ->
    {error, not_allowed};
try_register(User, Server, Password) ->
    case is_user_exists(User, Server) of
      true -> {atomic, exists};
      false ->
	  LServer = jid:nameprep(Server),
	  case lists:member(LServer, ?MYHOSTS) of
	    true ->
		Res = lists:foldl(fun (_M, {atomic, ok} = Res) -> Res;
				      (M, _) ->
					  M:try_register(User, Server, Password)
				  end,
				  {error, not_allowed}, auth_modules(Server)),
		case Res of
		  {atomic, ok} ->
		      ejabberd_hooks:run(register_user, Server,
					 [User, Server]),
		      {atomic, ok};
		  _ -> Res
		end;
	    false -> {error, not_allowed}
	  end
    end.

%% Registered users list do not include anonymous users logged
-spec dirty_get_registered_users() -> [{binary(), binary()}].

dirty_get_registered_users() ->
    lists:flatmap(fun (M) -> M:dirty_get_registered_users()
		  end,
		  auth_modules()).

-spec get_vh_registered_users(binary()) -> [{binary(), binary()}].

%% Registered users list do not include anonymous users logged
get_vh_registered_users(Server) ->
    lists:flatmap(fun (M) ->
			  M:get_vh_registered_users(Server)
		  end,
		  auth_modules(Server)).

-spec get_vh_registered_users(binary(), opts()) -> [{binary(), binary()}].

get_vh_registered_users(Server, Opts) ->
    lists:flatmap(fun (M) ->
			  case erlang:function_exported(M,
							get_vh_registered_users,
							2)
			      of
			    true -> M:get_vh_registered_users(Server, Opts);
			    false -> M:get_vh_registered_users(Server)
			  end
		  end,
		  auth_modules(Server)).

get_vh_registered_users_number(Server) ->
    lists:sum(lists:map(fun (M) ->
				case erlang:function_exported(M,
							      get_vh_registered_users_number,
							      1)
				    of
				  true ->
				      M:get_vh_registered_users_number(Server);
				  false ->
				      length(M:get_vh_registered_users(Server))
				end
			end,
			auth_modules(Server))).

-spec get_vh_registered_users_number(binary(), opts()) -> number().

get_vh_registered_users_number(Server, Opts) ->
%% @doc Get the password of the user.
%% @spec (User::string(), Server::string()) -> Password::string()
    lists:sum(lists:map(fun (M) ->
				case erlang:function_exported(M,
							      get_vh_registered_users_number,
							      2)
				    of
				  true ->
				      M:get_vh_registered_users_number(Server,
								       Opts);
				  false ->
				      length(M:get_vh_registered_users(Server))
				end
			end,
			auth_modules(Server))).

-spec get_password(binary(), binary()) -> false | binary() | {binary(), binary(), binary(), integer()}.

get_password(User, Server) ->
    lists:foldl(fun (M, false) ->
			M:get_password(User, Server);
		    (_M, Password) -> Password
		end,
		false, auth_modules(Server)).

-spec get_password_s(binary(), binary()) -> binary() | {binary(), binary(), binary(), integer()}.

get_password_s(User, Server) ->
    case get_password(User, Server) of
      false -> <<"">>;
      Password -> Password
    end.

%% @doc Get the password of the user and the auth module.
%% @spec (User::string(), Server::string()) ->
%%     {Password::string(), AuthModule::atom()} | {false, none}
-spec get_password_with_authmodule(binary(), binary()) -> {false | binary(), atom()}.

get_password_with_authmodule(User, Server) ->
%% Returns true if the user exists in the DB or if an anonymous user is logged
%% under the given name
    lists:foldl(fun (M, {false, _}) ->
			{M:get_password(User, Server), M};
		    (_M, {Password, AuthModule}) -> {Password, AuthModule}
		end,
		{false, none}, auth_modules(Server)).

-spec is_user_exists(binary(), binary()) -> boolean().

is_user_exists(_User, <<"">>) ->
    false;

is_user_exists(User, Server) ->
%% Check if the user exists in all authentications module except the module
%% passed as parameter
%% @spec (Module::atom(), User, Server) -> true | false | maybe
    lists:any(fun (M) ->
		      case M:is_user_exists(User, Server) of
			{error, Error} ->
			    ?ERROR_MSG("The authentication module ~p returned "
				       "an error~nwhen checking user ~p in server "
				       "~p~nError message: ~p",
				       [M, User, Server, Error]),
			    false;
			Else -> Else
		      end
	      end,
	      auth_modules(Server)).

-spec is_user_exists_in_other_modules(atom(), binary(), binary()) -> boolean() | maybe.

is_user_exists_in_other_modules(Module, User, Server) ->
    is_user_exists_in_other_modules_loop(auth_modules(Server)
					   -- [Module],
					 User, Server).

is_user_exists_in_other_modules_loop([], _User,
				     _Server) ->
    false;
is_user_exists_in_other_modules_loop([AuthModule
				      | AuthModules],
				     User, Server) ->
    case AuthModule:is_user_exists(User, Server) of
      true -> true;
      false ->
	  is_user_exists_in_other_modules_loop(AuthModules, User,
					       Server);
      {error, Error} ->
	  ?DEBUG("The authentication module ~p returned "
		 "an error~nwhen checking user ~p in server "
		 "~p~nError message: ~p",
		 [AuthModule, User, Server, Error]),
	  maybe
    end.

-spec remove_user(binary(), binary()) -> ok.

%% @spec (User, Server) -> ok
%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
remove_user(User, Server) ->
    lists:foreach(fun (M) -> M:remove_user(User, Server)
		  end,
		  auth_modules(Server)),
    ejabberd_hooks:run(remove_user, jid:nameprep(Server),
		       [User, Server]),
    ok.

%% @spec (User, Server, Password) -> ok | not_exists | not_allowed | bad_request | error
%% @doc Try to remove user if the provided password is correct.
%% The removal is attempted in each auth method provided:
%% when one returns 'ok' the loop stops;
%% if no method returns 'ok' then it returns the error message indicated by the last method attempted.
-spec remove_user(binary(), binary(), binary()) -> any().

remove_user(User, Server, Password) ->
    R = lists:foldl(fun (_M, ok = Res) -> Res;
			(M, _) -> M:remove_user(User, Server, Password)
		    end,
		    error, auth_modules(Server)),
    case R of
      ok ->
	  ejabberd_hooks:run(remove_user, jid:nameprep(Server),
			     [User, Server]);
      _ -> none
    end,
    R.

%% @spec (IOList) -> non_negative_float()
%% @doc Calculate informational entropy.
entropy(B) ->
    case binary_to_list(B) of
      "" -> 0.0;
      S ->
	  Set = lists:foldl(fun (C,
				 [Digit, Printable, LowLetter, HiLetter,
				  Other]) ->
				    if C >= $a, C =< $z ->
					   [Digit, Printable, 26, HiLetter,
					    Other];
				       C >= $0, C =< $9 ->
					   [9, Printable, LowLetter, HiLetter,
					    Other];
				       C >= $A, C =< $Z ->
					   [Digit, Printable, LowLetter, 26,
					    Other];
				       C >= 33, C =< 126 ->
					   [Digit, 33, LowLetter, HiLetter,
					    Other];
				       true ->
					   [Digit, Printable, LowLetter,
					    HiLetter, 128]
				    end
			    end,
			    [0, 0, 0, 0, 0], S),
	  length(S) * math:log(lists:sum(Set)) / math:log(2)
    end.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
%% Return the lists of all the auth modules actually used in the
%% configuration
auth_modules() ->
    lists:usort(lists:flatmap(fun (Server) ->
				      auth_modules(Server)
			      end,
			      ?MYHOSTS)).

-spec auth_modules(binary()) -> [atom()].

%% Return the list of authenticated modules for a given host
auth_modules(Server) ->
    LServer = jid:nameprep(Server),
    Default = case gen_mod:default_db(LServer) of
		  mnesia -> internal;
		  DBType -> DBType
	      end,
    Methods = ejabberd_config:get_option(
                {auth_method, LServer},
                fun(V) when is_list(V) ->
                        true = lists:all(fun is_atom/1, V),
                        V;
                   (V) when is_atom(V) ->
                        [V]
                end, [Default]),
    [jlib:binary_to_atom(<<"ejabberd_auth_",
                           (jlib:atom_to_binary(M))/binary>>)
     || M <- Methods].

export(Server) ->
    ejabberd_auth_internal:export(Server).

import(Server) ->
    ejabberd_auth_internal:import(Server).

import(Server, mnesia, Passwd) ->
    ejabberd_auth_internal:import(Server, mnesia, Passwd);
import(Server, riak, Passwd) ->
    ejabberd_auth_riak:import(Server, riak, Passwd);
import(_, _, _) ->
    pass.

opt_type(auth_method) ->
    fun (V) when is_list(V) ->
	    true = lists:all(fun is_atom/1, V), V;
	(V) when is_atom(V) -> [V]
    end;
opt_type(_) -> [auth_method].
