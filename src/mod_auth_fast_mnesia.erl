%%%-------------------------------------------------------------------
%%% File    : mod_auth_fast_mnesia.erl
%%% Author  : Pawel Chmielowski <pawel@process-one.net>
%%% Created : 1 Dec 2024 by Pawel Chmielowski <pawel@process-one.net>
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

-module(mod_auth_fast_mnesia).

-behaviour(mod_auth_fast).

%% API
-export([init/2]).
-export([get_tokens/3, del_token/4, set_token/6, rotate_token/3]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").

-record(mod_auth_fast, {key = {<<"">>, <<"">>, <<"">>} :: {binary(), binary(), binary()} | '$1',
			token = <<>> :: binary() | '_',
			created_at = 0 :: non_neg_integer() | '_',
			expires_at = 0 :: non_neg_integer() | '_'}).

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, mod_auth_fast,
			   [{disc_only_copies, [node()]},
			    {attributes,
			     record_info(fields, mod_auth_fast)}]).

-spec get_tokens(binary(), binary(), binary()) ->
    [{current | next, binary(), non_neg_integer()}].
get_tokens(LServer, LUser, UA) ->
    Now = erlang:system_time(second),
    case mnesia:dirty_read(mod_auth_fast, {LServer, LUser, token_id(UA, next)}) of
	[#mod_auth_fast{token = Token, created_at = Created, expires_at = Expires}] when Expires > Now ->
	    [{next, Token, Created}];
	[#mod_auth_fast{}] ->
	    del_token(LServer, LUser, UA, next),
	    [];
	_ ->
	    []
    end ++
	case mnesia:dirty_read(mod_auth_fast, {LServer, LUser, token_id(UA, current)}) of
	    [#mod_auth_fast{token = Token, created_at = Created, expires_at = Expires}] when Expires > Now ->
		[{current, Token, Created}];
	    [#mod_auth_fast{}] ->
		del_token(LServer, LUser, UA, current),
		[];
	    _ ->
		[]
	end.

-spec rotate_token(binary(), binary(), binary()) ->
    ok | {error, atom()}.
rotate_token(LServer, LUser, UA) ->
    F = fun() ->
	case mnesia:dirty_read(mod_auth_fast, {LServer, LUser, token_id(UA, next)}) of
	    [#mod_auth_fast{token = Token, created_at = Created, expires_at = Expires}] ->
		mnesia:write(#mod_auth_fast{key = {LServer, LUser, token_id(UA, current)},
					    token = Token, created_at = Created,
					    expires_at = Expires}),
		mnesia:delete({mod_auth_fast, {LServer, LUser, token_id(UA, next)}});
	    _ ->
		ok
	end
	end,
    transaction(F).

-spec del_token(binary(), binary(), binary(), current | next) ->
    ok | {error, atom()}.
del_token(LServer, LUser, UA, Type) ->
    F = fun() ->
	mnesia:delete({mod_auth_fast, {LServer, LUser, token_id(UA, Type)}})
	end,
    transaction(F).

-spec set_token(binary(), binary(), binary(), current | next, binary(), non_neg_integer()) ->
    ok | {error, atom()}.
set_token(LServer, LUser, UA, Type, Token, Expires) ->
    F = fun() ->
	mnesia:write(#mod_auth_fast{key = {LServer, LUser, token_id(UA, Type)},
				    token = Token, created_at = erlang:system_time(second),
				    expires_at = Expires})
	end,
    transaction(F).

%%%===================================================================
%%% Internal functions
%%%===================================================================

token_id(UA, current) ->
    <<"c:", UA/binary>>;
token_id(UA, _) ->
    <<"n:", UA/binary>>.

transaction(F) ->
    case mnesia:transaction(F) of
	{atomic, Res} ->
	    Res;
	{aborted, Reason} ->
	    ?ERROR_MSG("Mnesia transaction failed: ~p", [Reason]),
	    {error, db_failure}
    end.
