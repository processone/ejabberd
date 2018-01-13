%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 15 Apr 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
%%%-------------------------------------------------------------------
-module(ejabberd_router_riak).
-behaviour(ejabberd_router).

%% API
-export([init/0, register_route/5, unregister_route/3, find_routes/1,
	 get_all_routes/0]).

-include("logger.hrl").
-include("ejabberd_router.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    clean_table().

register_route(Domain, ServerHost, LocalHint, _, Pid) ->
    ejabberd_riak:put(#route{domain = Domain,
			     server_host = ServerHost,
			     local_hint = LocalHint,
			     pid = Pid},
		      route_schema(),
		      [{i, {Domain, Pid}}, {'2i', [{<<"route">>, Domain}]}]).

unregister_route(Domain, _, Pid) ->
    ejabberd_riak:delete(route, {Domain, Pid}).

find_routes(Domain) ->
    ejabberd_riak:get_by_index(route, route_schema(), <<"route">>, Domain).

get_all_routes() ->
    case ejabberd_riak:get(route, route_schema()) of
	{ok, Routes} ->
	    {ok, lists:flatmap(
		   fun(#route{domain = D, server_host = S}) when D /= S ->
			   [D];
		      (_) ->
			   []
		   end, Routes)};
	Err ->
	    Err
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
route_schema() ->
    {record_info(fields, route), #route{}}.

clean_table() ->
    ?DEBUG("Cleaning Riak 'route' table...", []),
    case ejabberd_riak:get(route, route_schema()) of
	{ok, Routes} ->
	    lists:foreach(
	      fun(#route{domain = Domain, pid = Pid}) ->
		      ejabberd_riak:delete(route, {Domain, Pid})
	      end, Routes);
	{error, Err} ->
	    ?ERROR_MSG("failed to clean Riak 'route' table: ~p", [Err]),
	    Err
    end.
