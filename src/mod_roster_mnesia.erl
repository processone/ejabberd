%%%-------------------------------------------------------------------
%%% File    : mod_roster_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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

-module(mod_roster_mnesia).

-behaviour(mod_roster).

%% API
-export([init/2, read_roster_version/2, write_roster_version/4,
	 get_roster/2, get_roster_item/3, roster_subscribe/4,
	 remove_user/2, update_roster/4, del_roster/3, transaction/2,
	 read_subscription_and_groups/3, import/3, create_roster/1,
	 use_cache/2]).
-export([need_transform/1, transform/1]).

-include("mod_roster.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, roster,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, roster)},
			 {index, [us]}]),
    ejabberd_mnesia:create(?MODULE, roster_version,
			[{disc_only_copies, [node()]},
			 {attributes,
			  record_info(fields, roster_version)}]).

use_cache(Host, Table) ->
    case mnesia:table_info(Table, storage_type) of
	disc_only_copies ->
	    gen_mod:get_module_opt(Host, mod_roster, use_cache);
	_ ->
	    false
    end.

read_roster_version(LUser, LServer) ->
    US = {LUser, LServer},
    case mnesia:dirty_read(roster_version, US) of
	[#roster_version{version = V}] -> {ok, V};
	[] -> error
    end.

write_roster_version(LUser, LServer, InTransaction, Ver) ->
    US = {LUser, LServer},
    if InTransaction ->
	    mnesia:write(#roster_version{us = US, version = Ver});
       true ->
	    mnesia:dirty_write(#roster_version{us = US, version = Ver})
    end.

get_roster(LUser, LServer) ->
    {ok, mnesia:dirty_index_read(roster, {LUser, LServer}, #roster.us)}.

get_roster_item(LUser, LServer, LJID) ->
    case mnesia:read({roster, {LUser, LServer, LJID}}) of
	[I] -> {ok, I};
	[] -> error
    end.

roster_subscribe(_LUser, _LServer, _LJID, Item) ->
    mnesia:write(Item).

remove_user(LUser, LServer) ->
    US = {LUser, LServer},
    F = fun () ->
		lists:foreach(
		  fun (R) -> mnesia:delete_object(R) end,
		  mnesia:index_read(roster, US, #roster.us))
	end,
    mnesia:transaction(F).

update_roster(_LUser, _LServer, _LJID, Item) ->
    mnesia:write(Item).

del_roster(LUser, LServer, LJID) ->
    mnesia:delete({roster, {LUser, LServer, LJID}}).

read_subscription_and_groups(LUser, LServer, LJID) ->
    case mnesia:dirty_read(roster, {LUser, LServer, LJID}) of
	[#roster{subscription = Subscription, ask = Ask, groups = Groups}] ->
	    {ok, {Subscription, Ask, Groups}};
	_ ->
	    error
    end.

transaction(_LServer, F) ->
    mnesia:transaction(F).

create_roster(RItem) ->
    mnesia:dirty_write(RItem).

import(_LServer, <<"rosterusers">>, #roster{} = R) ->
    mnesia:dirty_write(R);
import(LServer, <<"roster_version">>, [LUser, Ver]) ->
    RV = #roster_version{us = {LUser, LServer}, version = Ver},
    mnesia:dirty_write(RV).

need_transform(#roster{usj = {U, S, _}}) when is_list(U) orelse is_list(S) ->
    ?INFO_MSG("Mnesia table 'roster' will be converted to binary", []),
    true;
need_transform(#roster_version{us = {U, S}, version = Ver})
  when is_list(U) orelse is_list(S) orelse is_list(Ver) ->
    ?INFO_MSG("Mnesia table 'roster_version' will be converted to binary", []),
    true;
need_transform(_) ->
    false.

transform(#roster{usj = {U, S, {LU, LS, LR}},
		  us = {U1, S1},
		  jid = {U2, S2, R2},
		  name = Name,
		  groups = Gs,
		  askmessage = Ask,
		  xs = Xs} = R) ->
    R#roster{usj = {iolist_to_binary(U), iolist_to_binary(S),
		    {iolist_to_binary(LU),
		     iolist_to_binary(LS),
		     iolist_to_binary(LR)}},
	     us = {iolist_to_binary(U1), iolist_to_binary(S1)},
	     jid = {iolist_to_binary(U2),
		    iolist_to_binary(S2),
		    iolist_to_binary(R2)},
	     name = iolist_to_binary(Name),
	     groups = [iolist_to_binary(G) || G <- Gs],
	     askmessage = try iolist_to_binary(Ask)
			  catch _:_ -> <<"">> end,
	     xs = [fxml:to_xmlel(X) || X <- Xs]};
transform(#roster_version{us = {U, S}, version = Ver} = R) ->
    R#roster_version{us = {iolist_to_binary(U), iolist_to_binary(S)},
		     version = iolist_to_binary(Ver)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
