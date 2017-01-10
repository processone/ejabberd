%%%-------------------------------------------------------------------
%%% File    : mod_privacy_riak.erl
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

-module(mod_privacy_riak).

-behaviour(mod_privacy).

%% API
-export([init/2, process_lists_get/2, process_list_get/3,
	 process_default_set/3, process_active_set/3,
	 remove_privacy_list/3, set_privacy_list/1,
	 set_privacy_list/4, get_user_list/2, get_user_lists/2,
	 remove_user/2, import/1]).

-export([privacy_schema/0]).

-include("xmpp.hrl").
-include("mod_privacy.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

process_lists_get(LUser, LServer) ->
    case ejabberd_riak:get(privacy, privacy_schema(), {LUser, LServer}) of
        {ok, #privacy{default = Default, lists = Lists}} ->
            LItems = lists:map(fun ({N, _}) -> N end, Lists),
            {Default, LItems};
        {error, notfound} ->
            {none, []};
        {error, _} ->
            error
    end.

process_list_get(LUser, LServer, Name) ->
    case ejabberd_riak:get(privacy, privacy_schema(), {LUser, LServer}) of
        {ok, #privacy{lists = Lists}} ->
            case lists:keysearch(Name, 1, Lists) of
                {value, {_, List}} -> List;
                _ -> not_found
            end;
        {error, notfound} ->
            not_found;
        {error, _} ->
            error
    end.

process_default_set(LUser, LServer, none) ->
    {atomic,
     case ejabberd_riak:get(privacy, privacy_schema(), {LUser, LServer}) of
         {ok, R} ->
             ejabberd_riak:put(R#privacy{default = none}, privacy_schema());
         {error, _} ->
             ok
     end};
process_default_set(LUser, LServer, Name) ->
    {atomic,
     case ejabberd_riak:get(privacy, privacy_schema(), {LUser, LServer}) of
         {ok, #privacy{lists = Lists} = P} ->
             case lists:keymember(Name, 1, Lists) of
                 true ->
                     ejabberd_riak:put(P#privacy{default = Name,
                                                 lists = Lists},
				       privacy_schema());
                 false ->
                     not_found
             end;
         {error, _} ->
             not_found
     end}.

process_active_set(LUser, LServer, Name) ->
    case ejabberd_riak:get(privacy, privacy_schema(), {LUser, LServer}) of
        {ok, #privacy{lists = Lists}} ->
            case lists:keysearch(Name, 1, Lists) of
                {value, {_, List}} -> List;
                false -> error
            end;
        {error, _} ->
            error
    end.

remove_privacy_list(LUser, LServer, Name) ->
    {atomic,
     case ejabberd_riak:get(privacy, privacy_schema(), {LUser, LServer}) of
         {ok, #privacy{default = Default, lists = Lists} = P} ->
             if Name == Default ->
                     conflict;
                true ->
                     NewLists = lists:keydelete(Name, 1, Lists),
                     ejabberd_riak:put(P#privacy{lists = NewLists},
				       privacy_schema())
             end;
         {error, _} ->
             ok
     end}.

set_privacy_list(Privacy) ->
    ejabberd_riak:put(Privacy, privacy_schema()).

set_privacy_list(LUser, LServer, Name, List) ->
    {atomic,
     case ejabberd_riak:get(privacy, privacy_schema(), {LUser, LServer}) of
         {ok, #privacy{lists = Lists} = P} ->
             NewLists1 = lists:keydelete(Name, 1, Lists),
             NewLists = [{Name, List} | NewLists1],
             ejabberd_riak:put(P#privacy{lists = NewLists}, privacy_schema());
         {error, _} ->
             NewLists = [{Name, List}],
             ejabberd_riak:put(#privacy{us = {LUser, LServer},
                                        lists = NewLists},
			       privacy_schema())
     end}.

get_user_list(LUser, LServer) ->
    case ejabberd_riak:get(privacy, privacy_schema(), {LUser, LServer}) of
        {ok, #privacy{default = Default, lists = Lists}} ->
            case Default of
                none -> {none, []};
                _ ->
                    case lists:keysearch(Default, 1, Lists) of
                        {value, {_, List}} -> {Default, List};
                        _ -> {none, []}
                    end
            end;
        {error, _} ->
            {none, []}
    end.

get_user_lists(LUser, LServer) ->
    case ejabberd_riak:get(privacy, privacy_schema(), {LUser, LServer}) of
        {ok, #privacy{} = P} ->
            {ok, P};
        {error, _} ->
            error
    end.

remove_user(LUser, LServer) ->
    {atomic, ejabberd_riak:delete(privacy, {LUser, LServer})}.

import(#privacy{} = P) ->
    ejabberd_riak:put(P, privacy_schema()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
privacy_schema() ->
    {record_info(fields, privacy), #privacy{}}.
