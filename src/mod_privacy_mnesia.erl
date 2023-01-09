%%%-------------------------------------------------------------------
%%% File    : mod_privacy_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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
-export([init/2, set_default/3, unset_default/2, set_lists/1,
	 set_list/4, get_lists/2, get_list/3, remove_lists/2,
	 remove_list/3, use_cache/1, import/1]).
-export([need_transform/1, transform/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("mod_privacy.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, privacy,
			   [{disc_only_copies, [node()]},
			    {attributes, record_info(fields, privacy)}]).

use_cache(Host) ->
    case mnesia:table_info(privacy, storage_type) of
        disc_only_copies ->
            mod_privacy_opt:use_cache(Host);
        _ ->
            false
    end.

unset_default(LUser, LServer) ->
    F = fun () ->
		case mnesia:read({privacy, {LUser, LServer}}) of
		    [] -> ok;
		    [R] -> mnesia:write(R#privacy{default = none})
		end
	end,
    transaction(F).

set_default(LUser, LServer, Name) ->
    F = fun () ->
		case mnesia:read({privacy, {LUser, LServer}}) of
		    [] ->
			{error, notfound};
		    [#privacy{lists = Lists} = P] ->
			case lists:keymember(Name, 1, Lists) of
			    true ->
				mnesia:write(P#privacy{default = Name,
						       lists = Lists});
			    false ->
				{error, notfound}
			end
		end
	end,
    transaction(F).

remove_list(LUser, LServer, Name) ->
    F = fun () ->
		case mnesia:read({privacy, {LUser, LServer}}) of
		    [] ->
			{error, notfound};
		    [#privacy{default = Default, lists = Lists} = P] ->
			if Name == Default ->
				{error, conflict};
			   true ->
				NewLists = lists:keydelete(Name, 1, Lists),
				mnesia:write(P#privacy{lists = NewLists})
			end
		end
	end,
    transaction(F).

set_lists(Privacy) ->
    mnesia:dirty_write(Privacy).

set_list(LUser, LServer, Name, List) ->
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
    transaction(F).

get_list(LUser, LServer, Name) ->
    case mnesia:dirty_read(privacy, {LUser, LServer}) of
	[#privacy{default = Default, lists = Lists}] when Name == default ->
	    case lists:keyfind(Default, 1, Lists) of
		{_, List} -> {ok, {Default, List}};
		false -> error
	    end;
	[#privacy{lists = Lists}] ->
	    case lists:keyfind(Name, 1, Lists) of
		{_, List} -> {ok, {Name, List}};
		false -> error
	    end;
	[] ->
	    error
    end.

get_lists(LUser, LServer) ->
    case mnesia:dirty_read(privacy, {LUser, LServer}) of
        [#privacy{} = P] ->
            {ok, P};
        _ ->
            error
    end.

remove_lists(LUser, LServer) ->
    F = fun () -> mnesia:delete({privacy, {LUser, LServer}}) end,
    transaction(F).

import(#privacy{} = P) ->
    mnesia:dirty_write(P).

need_transform({privacy, {U, S}, _, _}) when is_list(U) orelse is_list(S) ->
    ?INFO_MSG("Mnesia table 'privacy' will be converted to binary", []),
    true;
need_transform(_) ->
    false.

transform(#privacy{us = {U, S}, default = Def, lists = Lists} = R) ->
    NewLists = lists:map(
		 fun({Name, Ls}) ->
			 NewLs = lists:map(
				   fun(#listitem{value = Val} = L) ->
					   NewVal = case Val of
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
    R#privacy{us = NewUS, default = NewDef, lists = NewLists}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
transaction(F) ->
    case mnesia:transaction(F) of
	{atomic, Result} ->
	    Result;
	{aborted, Reason} ->
	    ?ERROR_MSG("Mnesia transaction failed: ~p", [Reason]),
	    {error, db_failure}
    end.
