%%%-------------------------------------------------------------------
%%% File    : mod_blocking_sql.erl
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

-module(mod_blocking_sql).

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
		Default = case mod_privacy_sql:sql_get_default_privacy_list_t(LUser) of
			      {selected, []} ->
				  Name = <<"Blocked contacts">>,
				  case mod_privacy_sql:sql_get_privacy_list_id_t(LUser, Name) of
				      {selected, []} ->
					  mod_privacy_sql:sql_add_privacy_list(LUser, Name);
				      {selected, [{_ID}]} ->
					  ok
				  end,
				  mod_privacy_sql:sql_set_default_privacy_list(LUser, Name),
				  Name;
			      {selected, [{Name}]} -> Name
			  end,
		{selected, [{ID}]} =
		    mod_privacy_sql:sql_get_privacy_list_id_t(LUser, Default),
		case mod_privacy_sql:sql_get_privacy_list_data_by_id_t(ID) of
		    {selected, RItems = [_ | _]} ->
			List = lists:flatmap(fun mod_privacy_sql:raw_to_item/1, RItems);
		    _ ->
			List = []
		end,
		NewList = Filter(List),
		NewRItems = lists:map(fun mod_privacy_sql:item_to_raw/1,
				      NewList),
		mod_privacy_sql:sql_set_privacy_list(ID, NewRItems),
		{ok, Default, NewList}
	end,
    ejabberd_sql:sql_transaction(LServer, F).

unblock_by_filter(LUser, LServer, Filter) ->
    F = fun () ->
		case mod_privacy_sql:sql_get_default_privacy_list_t(LUser) of
		    {selected, []} -> ok;
		    {selected, [{Default}]} ->
			{selected, [{ID}]} =
			    mod_privacy_sql:sql_get_privacy_list_id_t(LUser, Default),
			case mod_privacy_sql:sql_get_privacy_list_data_by_id_t(ID) of
			    {selected, RItems = [_ | _]} ->
				List = lists:flatmap(fun mod_privacy_sql:raw_to_item/1,
						     RItems),
				NewList = Filter(List),
				NewRItems = lists:map(fun mod_privacy_sql:item_to_raw/1,
						      NewList),
				mod_privacy_sql:sql_set_privacy_list(ID, NewRItems),
				{ok, Default, NewList};
			    _ -> ok
			end;
		    _ -> ok
		end
	end,
    ejabberd_sql:sql_transaction(LServer, F).

process_blocklist_get(LUser, LServer) ->
    case catch mod_privacy_sql:sql_get_default_privacy_list(LUser, LServer) of
	{selected, []} -> [];
	{selected, [{Default}]} ->
	    case catch mod_privacy_sql:sql_get_privacy_list_data(
			 LUser, LServer, Default) of
		{selected, RItems} ->
		    lists:flatmap(fun mod_privacy_sql:raw_to_item/1, RItems);
		{'EXIT', _} -> error
	    end;
	{'EXIT', _} -> error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
