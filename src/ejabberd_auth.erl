%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification
%%% Created : 23 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
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

%% TODO: Use the functions in ejabberd auth to add and remove users.

-module(ejabberd_auth).

-author('alexey@process-one.net').

%% External exports
-export([start/0, set_password/3, check_password/3,
	 check_password/5, check_password_with_authmodule/3,
	 check_password_with_authmodule/5, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
	 get_vh_registered_users/2, export/1,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2, get_password/2,
	 get_password_s/2, get_password_with_authmodule/2,
	 is_user_exists/2, is_user_exists_in_other_modules/3,
	 remove_user/2, remove_user/3, plain_password_required/1,
	 store_type/1, entropy/1]).

-export([auth_modules/1]).

%% For benchmarking
-record(passwd, {us = {<<"">>, <<"">>} :: {binary(), binary()} | '$1',
                 password = <<"">> :: binary() | scram() | '_'}).

-export([create_users/5]).

-include("ejabberd.hrl").

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
-callback get_password(binary(), binary()) -> false | binary().
-callback get_password_s(binary(), binary()) -> binary().

start() ->
    lists:foreach(fun (Host) ->
			  lists:foreach(fun (M) -> M:start(Host) end,
					auth_modules(Host))
		  end,
		  ?MYHOSTS).

plain_password_required(Server) ->
    lists:any(fun (M) -> M:plain_password_required() end,
	      auth_modules(Server)).

store_type(Server) ->
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

set_password(_User, _Server, <<"">>) ->
    {error, empty_password};
set_password(User, Server, Password) ->
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
	  LServer = jlib:nameprep(Server),
	  case lists:member(LServer, ?MYHOSTS) of
	    true ->
                MaxUsers = ejabberd_config:get_local_option({max_users, LServer},
                                                            fun(X) when is_integer(X) -> X end, no_limit),
                RegAllowed = case MaxUsers of
                                 no_limit ->
                                     true;
                                 Num ->
                                     get_vh_registered_users_number(LServer) < Num
                             end,
                case RegAllowed of
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
                    _ ->
                        {error, too_many_users}
                end;
	    false -> {error, not_allowed}
	  end
    end.

-spec dirty_get_registered_users() -> [{binary(), binary()}].

dirty_get_registered_users() ->
    lists:flatmap(fun (M) -> M:dirty_get_registered_users()
		  end,
		  auth_modules()).

-spec get_vh_registered_users(binary()) -> [{binary(), binary()}].

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

-spec get_password(binary(), binary()) -> false | binary().

get_password(User, Server) ->
    lists:foldl(fun (M, false) ->
			M:get_password(User, Server);
		    (_M, Password) -> Password
		end,
		false, auth_modules(Server)).

-spec get_password_s(binary(), binary()) -> binary().

get_password_s(User, Server) ->
    case get_password(User, Server) of
      false -> <<"">>;
      Password -> Password
    end.

-spec get_password_with_authmodule(binary(), binary()) -> {false | binary(), atom()}.

get_password_with_authmodule(User, Server) ->
    lists:foldl(fun (M, {false, _}) ->
			{M:get_password(User, Server), M};
		    (_M, {Password, AuthModule}) -> {Password, AuthModule}
		end,
		{false, none}, auth_modules(Server)).

-spec is_user_exists(binary(), binary()) -> boolean().

is_user_exists(User, Server) ->
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

remove_user(User, Server) ->
    lists:foreach(fun (M) -> M:remove_user(User, Server)
		  end,
		  auth_modules(Server)),
    ejabberd_hooks:run(remove_user, jlib:nameprep(Server),
		       [User, Server]),
    ok.

-spec remove_user(binary(), binary(), binary()) -> any().

remove_user(User, Server, Password) ->
    R = lists:foldl(fun (_M, ok = Res) -> Res;
			(M, _) -> M:remove_user(User, Server, Password)
		    end,
		    error, auth_modules(Server)),
    case R of
      ok ->
	  ejabberd_hooks:run(remove_user, jlib:nameprep(Server),
			     [User, Server]);
      _ -> none
    end,
    R.

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

auth_modules() ->
    lists:usort(lists:flatmap(fun (Server) ->
				      auth_modules(Server)
			      end,
			      ?MYHOSTS)).

-spec auth_modules(binary()) -> [atom()].

auth_modules(Server) ->
    LServer = jlib:nameprep(Server),
    Methods = ejabberd_config:get_local_option(
                {auth_method, LServer},
                fun(V) when is_list(V) ->
                        true = lists:all(fun is_atom/1, V),
                        V;
                   (V) when is_atom(V) ->
                        [V]
                end, []),
    [jlib:binary_to_atom(<<"ejabberd_auth_",
                           (jlib:atom_to_binary(M))/binary>>)
     || M <- Methods].

export(Server) ->
    ejabberd_auth_internal:export(Server).

-spec create_users(binary(), binary(), binary(),
                   pos_integer(), gen_mod:db_type()) -> any().

create_users(UserPattern, PassPattern, Server, Total, DBType) ->
    lists:foreach(
      fun(I) ->
              LUser = jlib:nodeprep(
                        iolist_to_binary([UserPattern, integer_to_list(I)])),
              Pass = iolist_to_binary([PassPattern, integer_to_list(I)]),
              LServer = jlib:nameprep(Server),
              US = {LUser, LServer},
              case DBType of
                  mnesia ->
                      mnesia:dirty_write(#passwd{us = US,
                                                 password = Pass});
                  riak ->
                      ejabberd_riak:put(
                        #passwd{us = US,
                                password = Pass},
                        [{'2i', [{<<"host">>, LServer}]}]);
                  odbc ->
                      erlang:error(odbc_not_supported)
              end
      end, lists:seq(1, Total)).
