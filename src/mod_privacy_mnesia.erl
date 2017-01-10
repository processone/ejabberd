%%%-------------------------------------------------------------------
%%% File    : mod_privacy_mnesia.erl
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

-module(mod_privacy_mnesia).

-behaviour(mod_privacy).

%% API
-export([init/2, process_lists_get/2, process_list_get/3,
	 process_default_set/3, process_active_set/3,
	 remove_privacy_list/3, set_privacy_list/1,
	 set_privacy_list/4, get_user_list/2, get_user_lists/2,
	 remove_user/2, import/1]).

-include("xmpp.hrl").
-include("mod_privacy.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, privacy,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, privacy)}]),
    update_table().

process_lists_get(LUser, LServer) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
	{'EXIT', _Reason} -> error;
	[] -> {none, []};
	[#privacy{default = Default, lists = Lists}] ->
	    LItems = lists:map(fun ({N, _}) -> N end, Lists),
	    {Default, LItems}
    end.

process_list_get(LUser, LServer, Name) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
	{'EXIT', _Reason} -> error;
	[] -> not_found;
	[#privacy{lists = Lists}] ->
	    case lists:keysearch(Name, 1, Lists) of
		{value, {_, List}} -> List;
		_ -> not_found
	    end
    end.

process_default_set(LUser, LServer, none) ->
    F = fun () ->
		case mnesia:read({privacy, {LUser, LServer}}) of
		    [] -> ok;
		    [R] -> mnesia:write(R#privacy{default = none})
		end
	end,
    mnesia:transaction(F);
process_default_set(LUser, LServer, Name) ->
    F = fun () ->
		case mnesia:read({privacy, {LUser, LServer}}) of
		    [] -> not_found;
		    [#privacy{lists = Lists} = P] ->
			case lists:keymember(Name, 1, Lists) of
			    true ->
				mnesia:write(P#privacy{default = Name,
						       lists = Lists}),
				ok;
			    false -> not_found
			end
		end
	end,
    mnesia:transaction(F).

process_active_set(LUser, LServer, Name) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
	[] -> error;
	[#privacy{lists = Lists}] ->
	    case lists:keysearch(Name, 1, Lists) of
		{value, {_, List}} -> List;
		false -> error
	    end
    end.

remove_privacy_list(LUser, LServer, Name) ->
    F = fun () ->
		case mnesia:read({privacy, {LUser, LServer}}) of
		  [] -> ok;
		  [#privacy{default = Default, lists = Lists} = P] ->
		      if Name == Default -> conflict;
			 true ->
			     NewLists = lists:keydelete(Name, 1, Lists),
			     mnesia:write(P#privacy{lists = NewLists})
		      end
		end
	end,
    mnesia:transaction(F).

set_privacy_list(Privacy) ->
    mnesia:dirty_write(Privacy).

set_privacy_list(LUser, LServer, Name, List) ->
    F = fun () ->
		case mnesia:wread({privacy, {LUser, LServer}}) of
		  [] ->
		      NewLists = [{Name, List}],
		      mnesia:write(#privacy{us = {LUser, LServer},
					    lists = NewLists});
		  [#privacy{lists = Lists} = P] ->
		      NewLists1 = lists:keydelete(Name, 1, Lists),
		      NewLists = [{Name, List} | NewLists1],
		      mnesia:write(P#privacy{lists = NewLists})
		end
	end,
    mnesia:transaction(F).

get_user_list(LUser, LServer) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer})
	of
      [] -> {none, []};
      [#privacy{default = Default, lists = Lists}] ->
	  case Default of
	    none -> {none, []};
	    _ ->
		case lists:keysearch(Default, 1, Lists) of
		  {value, {_, List}} -> {Default, List};
		  _ -> {none, []}
		end
	  end;
      _ -> {none, []}
    end.

get_user_lists(LUser, LServer) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer}) of
        [#privacy{} = P] ->
            {ok, P};
        _ ->
            error
    end.

remove_user(LUser, LServer) ->
    F = fun () -> mnesia:delete({privacy, {LUser, LServer}}) end,
    mnesia:transaction(F).

import(#privacy{} = P) ->
    mnesia:dirty_write(P).

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_table() ->
    Fields = record_info(fields, privacy),
    case mnesia:table_info(privacy, attributes) of
      Fields ->
          ejabberd_config:convert_table_to_binary(
            privacy, Fields, set,
            fun(#privacy{us = {U, _}}) -> U end,
            fun(#privacy{us = {U, S}, default = Def, lists = Lists} = R) ->
                    NewLists =
                        lists:map(
                          fun({Name, Ls}) ->
                                  NewLs =
                                      lists:map(
                                        fun(#listitem{value = Val} = L) ->
                                                NewVal =
                                                    case Val of
                                                        {LU, LS, LR} ->
                                                            {iolist_to_binary(LU),
                                                             iolist_to_binary(LS),
                                                             iolist_to_binary(LR)};
                                                        none -> none;
                                                        both -> both;
                                                        from -> from;
                                                        to -> to;
                                                        _ -> iolist_to_binary(Val)
                                                    end,
                                                L#listitem{value = NewVal}
                                        end, Ls),
                                  {iolist_to_binary(Name), NewLs}
                          end, Lists),
                    NewDef = case Def of
                                 none -> none;
                                 _ -> iolist_to_binary(Def)
                             end,
                    NewUS = {iolist_to_binary(U), iolist_to_binary(S)},
                    R#privacy{us = NewUS, default = NewDef,
                              lists = NewLists}
            end);
      _ ->
	  ?INFO_MSG("Recreating privacy table", []),
	  mnesia:transform_table(privacy, ignore, Fields)
    end.
