%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_mnesia.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentication via mnesia
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

-module(ejabberd_auth_mnesia).

-author('alexey@process-one.net').

-behaviour(ejabberd_auth).

-export([start/1, stop/1, set_password_multiple/3, try_register_multiple/3,
	 get_users/2, init_db/0,
	 count_users/2, get_password/2,
	 remove_user/2, store_type/1, import/2,
	 plain_password_required/1, use_cache/1, drop_password_type/2, set_password_instance/3]).
-export([need_transform/1, transform/1]).

-include("logger.hrl").
-include_lib("xmpp/include/scram.hrl").
-include("ejabberd_auth.hrl").

-record(reg_users_counter, {vhost = <<"">> :: binary(),
                            count = 0 :: integer() | '$1'}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    init_db(),
    update_reg_users_counter_table(Host),
    ok.

stop(_Host) ->
    ok.

init_db() ->
    ejabberd_mnesia:create(?MODULE, passwd,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, passwd)}]),
    ejabberd_mnesia:create(?MODULE, reg_users_counter,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, reg_users_counter)}]).

update_reg_users_counter_table(Server) ->
    Set = get_users(Server, []),
    Size = length(Set),
    LServer = jid:nameprep(Server),
    F = fun () ->
		mnesia:write(#reg_users_counter{vhost = LServer,
						count = Size})
	end,
    mnesia:sync_dirty(F).

use_cache(Host) ->
    case mnesia:table_info(passwd, storage_type) of
	disc_only_copies ->
	    ejabberd_option:auth_use_cache(Host);
	_ ->
	    false
    end.

plain_password_required(Server) ->
    store_type(Server) == scram.

store_type(Server) ->
    ejabberd_auth:password_format(Server).

set_password_multiple(User, Server, Passwords) ->
    F = fun() ->
	lists:foreach(
	    fun(#scram{hash = Hash} = Password) ->
		mnesia:write(#passwd{us = {User, Server, Hash}, password = Password});
	       (Plain) ->
		   mnesia:write(#passwd{us = {User, Server, plain}, password = Plain})
	    end, Passwords)
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    {cache, {ok, Passwords}};
	{aborted, Reason} ->
	    ?ERROR_MSG("Mnesia transaction failed: ~p", [Reason]),
	    {nocache, {error, db_failure}}
    end.

set_password_instance(User, Server, Password) ->
    F = fun() ->
	case Password of
	    #scram{hash = Hash} = Password ->
		mnesia:write(#passwd{us = {User, Server, Hash}, password = Password});
	    Plain ->
		mnesia:write(#passwd{us = {User, Server, plain}, password = Plain})
	end
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    ?ERROR_MSG("Mnesia transaction failed: ~p", [Reason]),
	    {error, db_failure}
    end.

try_register_multiple(User, Server, Passwords) ->
    F = fun() ->
	case mnesia:select(passwd, [{{'_', {'$1', '$2', '_'}, '$3'},
				     [{'==', '$1', User},
				      {'==', '$2', Server}],
				     ['$3']}]) of
	    [] ->
		lists:foreach(
		    fun(#scram{hash = Hash} = Password) ->
			mnesia:write(#passwd{us = {User, Server, Hash}, password = Password});
		       (Plain) ->
			   mnesia:write(#passwd{us = {User, Server, plain}, password = Plain})
		    end, Passwords),
		mnesia:dirty_update_counter(reg_users_counter, Server, 1),
		{ok, Passwords};
	    [_] ->
		{error, exists}
	end
	end,
    case mnesia:transaction(F) of
	{atomic, Res} ->
	    {cache, Res};
	{aborted, Reason} ->
	    ?ERROR_MSG("Mnesia transaction failed: ~p", [Reason]),
	    {nocache, {error, db_failure}}
    end.

get_users(Server, []) ->
    Users = mnesia:dirty_select(passwd,
			[{#passwd{us = '$1', _ = '_'},
			  [{'==', {element, 2, '$1'}, Server}], ['$1']}]),
    misc:lists_uniq([{U, S} || {U, S, _} <- Users]);
get_users(Server, [{from, Start}, {to, End}])
  when is_integer(Start) and is_integer(End) ->
    get_users(Server, [{limit, End - Start + 1}, {offset, Start}]);
get_users(Server, [{limit, Limit}, {offset, Offset}])
  when is_integer(Limit) and is_integer(Offset) ->
    case get_users(Server, []) of
	[] ->
	    [];
	Users ->
	    Set = lists:keysort(1, Users),
	    L = length(Set),
	    Start = if Offset < 1 -> 1;
		       Offset > L -> L;
		       true -> Offset
		    end,
	    lists:sublist(Set, Start, Limit)
    end;
get_users(Server, [{prefix, Prefix}]) when is_binary(Prefix) ->
    Set = [{U, S} || {U, S} <- get_users(Server, []), str:prefix(Prefix, U)],
    lists:keysort(1, Set);
get_users(Server, [{prefix, Prefix}, {from, Start}, {to, End}])
  when is_binary(Prefix) and is_integer(Start) and is_integer(End) ->
    get_users(Server, [{prefix, Prefix}, {limit, End - Start + 1},
		       {offset, Start}]);
get_users(Server, [{prefix, Prefix}, {limit, Limit}, {offset, Offset}])
  when is_binary(Prefix) and is_integer(Limit) and is_integer(Offset) ->
    case [{U, S} || {U, S} <- get_users(Server, []), str:prefix(Prefix, U)] of
	[] ->
	    [];
	Users ->
	    Set = lists:keysort(1, Users),
	    L = length(Set),
	    Start = if Offset < 1 -> 1;
		       Offset > L -> L;
		       true -> Offset
		    end,
	    lists:sublist(Set, Start, Limit)
    end;
get_users(Server, _) ->
    get_users(Server, []).

count_users(Server, []) ->
    case mnesia:dirty_select(
	   reg_users_counter,
	   [{#reg_users_counter{vhost = Server, count = '$1'},
	     [], ['$1']}]) of
	[Count] -> Count;
	_ -> 0
    end;
count_users(Server, [{prefix, Prefix}]) when is_binary(Prefix) ->
    Set = [{U, S} || {U, S} <- get_users(Server, []), str:prefix(Prefix, U)],
    length(Set);
count_users(Server, _) ->
    count_users(Server, []).

get_password(User, Server) ->
    case mnesia:dirty_select(passwd, [{{'_', {'$1', '$2', '_'}, '$3'},
				       [{'==', '$1', User},
					{'==', '$2', Server}],
				       ['$3']}]) of
	[_|_] = List ->
	    List2 = lists:map(
		fun({scram, SK, SEK, Salt, IC}) ->
		    #scram{storedkey = SK, serverkey = SEK,
			   salt = Salt, hash = sha, iterationcount = IC};
		   (Other) -> Other
		end, List),
	    {cache, {ok, List2}};
	_ ->
	    {cache, error}
    end.

drop_password_type(Server, Hash) ->
    F = fun() ->
	Keys = mnesia:select(passwd, [{{'_', '$1', '_'},
				       [{'==', {element, 3, '$1'}, Hash},
					{'==', {element, 2, '$1'}, Server}],
				       ['$1']}]),
	lists:foreach(fun(Key) -> mnesia:delete({passwd, Key}) end, Keys),
	ok
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    ?ERROR_MSG("Mnesia transaction failed: ~p", [Reason]),
	    {error, db_failure}
    end.

remove_user(User, Server) ->
    F = fun () ->
	Keys = mnesia:select(passwd, [{{'_', '$1', '_'},
				       [{'==', {element, 1, '$1'}, User},
					{'==', {element, 2, '$1'}, Server}],
				       ['$1']}]),
	lists:foreach(fun(Key) -> mnesia:delete({passwd, Key}) end, Keys),
	mnesia:dirty_update_counter(reg_users_counter, Server, -1),
	ok
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    ?ERROR_MSG("Mnesia transaction failed: ~p", [Reason]),
	    {error, db_failure}
    end.

need_transform(#reg_users_counter{}) ->
    false;
need_transform({passwd, {_U, _S, _T}, _Pass}) ->
    false;
need_transform({passwd, {_U, _S}, _Pass}) ->
    true.

transform({passwd, {U, S}, Pass})
  when is_list(U) orelse is_list(S) orelse is_list(Pass) ->
    NewUS = {iolist_to_binary(U), iolist_to_binary(S)},
    NewPass = case Pass of
		  #scram{storedkey = StoredKey,
			 serverkey = ServerKey,
			 salt = Salt} ->
		      Pass#scram{
			storedkey = iolist_to_binary(StoredKey),
			serverkey = iolist_to_binary(ServerKey),
			salt = iolist_to_binary(Salt)};
		  _ ->
		      iolist_to_binary(Pass)
	      end,
    transform(#passwd{us = NewUS, password = NewPass});
transform(#passwd{us = {U, S}, password = Password} = P)
  when is_binary(Password) ->
    P#passwd{us = {U, S, plain}, password = Password};
transform({passwd, {U, S}, {scram, SK, SEK, Salt, IC}}) ->
    #passwd{us = {U, S, sha},
	    password = #scram{storedkey = SK, serverkey = SEK,
			      salt = Salt, hash = sha, iterationcount = IC}};
transform(#passwd{us = {U, S}, password = #scram{hash = Hash}} = P) ->
    P#passwd{us = {U, S, Hash}};
transform(Other) -> Other.

import(LServer, [LUser, Password, _TimeStamp]) ->
    mnesia:dirty_write(
      #passwd{us = {LUser, LServer}, password = Password}).
