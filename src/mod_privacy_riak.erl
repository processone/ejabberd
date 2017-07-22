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
-export([init/2, set_default/3, unset_default/2, set_lists/1,
	 set_list/4, get_lists/2, get_list/3, remove_lists/2,
	 remove_list/3, import/1]).

-export([privacy_schema/0]).

-include("xmpp.hrl").
-include("mod_privacy.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

unset_default(LUser, LServer) ->
    case ejabberd_riak:get(privacy, privacy_schema(), {LUser, LServer}) of
	{ok, R} ->
	    ejabberd_riak:put(R#privacy{default = none}, privacy_schema());
	{error, notfound} ->
	    ok;
	Err ->
	    Err
    end.

set_default(LUser, LServer, Name) ->
    case ejabberd_riak:get(privacy, privacy_schema(), {LUser, LServer}) of
	{ok, #privacy{lists = Lists} = P} ->
	    case lists:keymember(Name, 1, Lists) of
		true ->
		    ejabberd_riak:put(P#privacy{default = Name,
						lists = Lists},
				      privacy_schema());
		false ->
		    {error, notfound}
	    end;
	Err ->
	    Err
    end.

remove_list(LUser, LServer, Name) ->
    case ejabberd_riak:get(privacy, privacy_schema(), {LUser, LServer}) of
	{ok, #privacy{default = Default, lists = Lists} = P} ->
	    if Name == Default ->
		    {error, conflict};
	       true ->
		    NewLists = lists:keydelete(Name, 1, Lists),
		    ejabberd_riak:put(P#privacy{lists = NewLists},
				      privacy_schema())
	    end;
	Err ->
	    Err
    end.

set_lists(Privacy) ->
    ejabberd_riak:put(Privacy, privacy_schema()).

set_list(LUser, LServer, Name, List) ->
    case ejabberd_riak:get(privacy, privacy_schema(), {LUser, LServer}) of
	{ok, #privacy{lists = Lists} = P} ->
	    NewLists1 = lists:keydelete(Name, 1, Lists),
	    NewLists = [{Name, List} | NewLists1],
	    ejabberd_riak:put(P#privacy{lists = NewLists}, privacy_schema());
	{error, notfound} ->
	    NewLists = [{Name, List}],
	    ejabberd_riak:put(#privacy{us = {LUser, LServer},
				       lists = NewLists},
			      privacy_schema());
	Err ->
	    Err
    end.

get_list(LUser, LServer, Name) ->
    case ejabberd_riak:get(privacy, privacy_schema(), {LUser, LServer}) of
        {ok, #privacy{default = Default, lists = Lists}} when Name == default ->
            case lists:keyfind(Default, 1, Lists) of
		{_, List} -> {ok, {Default, List}};
		false -> error
	    end;
	{ok, #privacy{lists = Lists}} ->
	    case lists:keyfind(Name, 1, Lists) of
		{_, List} -> {ok, {Name, List}};
		false -> error
	    end;
	{error, notfound} ->
	    error;
	Err ->
	    Err
    end.

get_lists(LUser, LServer) ->
    case ejabberd_riak:get(privacy, privacy_schema(), {LUser, LServer}) of
        {ok, #privacy{} = P} ->
            {ok, P};
        {error, notfound} ->
            error;
	Err ->
	    Err
    end.

remove_lists(LUser, LServer) ->
    ejabberd_riak:delete(privacy, {LUser, LServer}).

import(#privacy{} = P) ->
    ejabberd_riak:put(P, privacy_schema()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
privacy_schema() ->
    {record_info(fields, privacy), #privacy{}}.
