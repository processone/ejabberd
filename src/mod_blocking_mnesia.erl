%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_blocking_mnesia).

-behaviour(mod_blocking).

%% API
-export([process_blocklist_block/3, unblock_by_filter/3,
	 process_blocklist_get/2]).

-include("jlib.hrl").
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
