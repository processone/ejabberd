%%%-------------------------------------------------------------------
%%% File    : ejabberd_oauth_mnesia.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : OAUTH2 mnesia backend
%%% Created : 20 Jul 2016 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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
%%%-------------------------------------------------------------------

-module(ejabberd_oauth_mnesia).
-behaviour(ejabberd_oauth).

-export([init/0,
	 store/1,
	 lookup/1,
	 clean/1,
	 lookup_client/1,
	 store_client/1,
	 remove_client/1,
	 use_cache/0, revoke/1]).

-include("ejabberd_oauth.hrl").

init() ->
    ejabberd_mnesia:create(?MODULE, oauth_token,
                        [{disc_only_copies, [node()]},
                         {attributes,
                          record_info(fields, oauth_token)}]),
    ejabberd_mnesia:create(?MODULE, oauth_client,
                        [{disc_copies, [node()]},
                         {attributes,
                          record_info(fields, oauth_client)}]),
    ok.

use_cache() ->
    case mnesia:table_info(oauth_token, storage_type) of
	disc_only_copies ->
	    ejabberd_option:oauth_use_cache();
	_ ->
	    false
    end.

store(R) ->
    mnesia:dirty_write(R).

lookup(Token) ->
    case catch mnesia:dirty_read(oauth_token, Token) of
        [R] ->
            {ok, R};
        _ ->
            error
    end.


-spec revoke(binary()) -> ok | {error, binary()}.
revoke(Token) ->
    mnesia:dirty_delete(oauth_token, Token).

clean(TS) ->
    F = fun() ->
		Ts = mnesia:select(
		       oauth_token,
		       [{#oauth_token{expire = '$1', _ = '_'},
			 [{'<', '$1', TS}],
			 ['$_']}]),
		lists:foreach(fun mnesia:delete_object/1, Ts)
        end,
    mnesia:async_dirty(F).

lookup_client(ClientID) ->
    case catch mnesia:dirty_read(oauth_client, ClientID) of
        [R] ->
            {ok, R};
        _ ->
            error
    end.

remove_client(ClientID) ->
    mnesia:dirty_delete(oauth_client, ClientID).

store_client(R) ->
    mnesia:dirty_write(R).
