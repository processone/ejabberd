%%%-------------------------------------------------------------------
%%% File    : mod_blocking_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(mod_blocking_mnesia).

-behaviour(mod_blocking).

%% API
-export([process_blocklist_block/3, unblock_by_filter/3,
	 process_blocklist_get/2]).

-include("mod_privacy.hrl").

%%%===================================================================
%%% API
%%%===================================================================
process_blocklist_block(LUser, LServer, Filter) ->
    F = fun () ->
		case mnesia:wread({privacy, {LUser, LServer}}) of
		    [] ->
			P = #privacy{us = {LUser, LServer}},
			NewDefault = <<"Blocked contacts">>,
			NewLists1 = [],
			List = [];
		    [#privacy{default = Default, lists = Lists} = P] ->
			case lists:keysearch(Default, 1, Lists) of
			    {value, {_, List}} ->
				NewDefault = Default,
				NewLists1 = lists:keydelete(Default, 1, Lists);
			    false ->
				NewDefault = <<"Blocked contacts">>,
				NewLists1 = Lists,
				List = []
			end
		end,
		NewList = Filter(List),
		NewLists = [{NewDefault, NewList} | NewLists1],
		mnesia:write(P#privacy{default = NewDefault,
				       lists = NewLists}),
		{ok, NewDefault, NewList}
	end,
    mnesia:transaction(F).

unblock_by_filter(LUser, LServer, Filter) ->
    F = fun () ->
		case mnesia:read({privacy, {LUser, LServer}}) of
		    [] ->
			%% No lists, nothing to unblock
			ok;
		    [#privacy{default = Default, lists = Lists} = P] ->
			case lists:keysearch(Default, 1, Lists) of
			    {value, {_, List}} ->
				NewList = Filter(List),
				NewLists1 = lists:keydelete(Default, 1, Lists),
				NewLists = [{Default, NewList} | NewLists1],
				mnesia:write(P#privacy{lists = NewLists}),
				{ok, Default, NewList};
			    false ->
				%% No default list, nothing to unblock
				ok
			end
		end
	end,
    mnesia:transaction(F).

process_blocklist_get(LUser, LServer) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
	{'EXIT', _Reason} -> error;
	[] -> [];
	[#privacy{default = Default, lists = Lists}] ->
	    case lists:keysearch(Default, 1, Lists) of
		{value, {_, List}} -> List;
		_ -> []
	    end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
