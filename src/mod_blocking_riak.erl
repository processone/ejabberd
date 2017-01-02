%%%-------------------------------------------------------------------
%%% File    : mod_blocking_riak.erl
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

-module(mod_blocking_riak).

-behaviour(mod_blocking).

%% API
-export([process_blocklist_block/3, unblock_by_filter/3,
	 process_blocklist_get/2]).

-include("mod_privacy.hrl").

%%%===================================================================
%%% API
%%%===================================================================
process_blocklist_block(LUser, LServer, Filter) ->
    {atomic,
     begin
         case ejabberd_riak:get(privacy, mod_privacy_riak:privacy_schema(),
				{LUser, LServer}) of
             {ok, #privacy{default = Default, lists = Lists} = P} ->
                 case lists:keysearch(Default, 1, Lists) of
                     {value, {_, List}} ->
                         NewDefault = Default,
                         NewLists1 = lists:keydelete(Default, 1, Lists);
                     false ->
                         NewDefault = <<"Blocked contacts">>,
                         NewLists1 = Lists,
                         List = []
                 end;
             {error, _} ->
                 P = #privacy{us = {LUser, LServer}},
                 NewDefault = <<"Blocked contacts">>,
                 NewLists1 = [],
                 List = []
         end,
         NewList = Filter(List),
         NewLists = [{NewDefault, NewList} | NewLists1],
         case ejabberd_riak:put(P#privacy{default = NewDefault,
                                          lists = NewLists},
				mod_privacy_riak:privacy_schema()) of
             ok ->
                 {ok, NewDefault, NewList};
             Err ->
                 Err
         end
     end}.

unblock_by_filter(LUser, LServer, Filter) ->
    {atomic,
     case ejabberd_riak:get(privacy, mod_privacy_riak:privacy_schema(),
			    {LUser, LServer}) of
         {error, _} ->
             %% No lists, nothing to unblock
             ok;
         {ok, #privacy{default = Default, lists = Lists} = P} ->
             case lists:keysearch(Default, 1, Lists) of
                 {value, {_, List}} ->
                     NewList = Filter(List),
                     NewLists1 = lists:keydelete(Default, 1, Lists),
                     NewLists = [{Default, NewList} | NewLists1],
                     case ejabberd_riak:put(P#privacy{lists = NewLists},
					    mod_privacy_riak:privacy_schema()) of
                         ok ->
                             {ok, Default, NewList};
                         Err ->
                             Err
                     end;
                 false ->
                     %% No default list, nothing to unblock
                     ok
             end
     end}.

process_blocklist_get(LUser, LServer) ->
    case ejabberd_riak:get(privacy, mod_privacy_riak:privacy_schema(),
			   {LUser, LServer}) of
        {ok, #privacy{default = Default, lists = Lists}} ->
            case lists:keysearch(Default, 1, Lists) of
                {value, {_, List}} -> List;
                _ -> []
            end;
        {error, notfound} ->
            [];
        {error, _} ->
            error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
