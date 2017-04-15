%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 15 Apr 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
%%%-------------------------------------------------------------------
-module(mod_carboncopy_riak).
-behaviour(mod_carboncopy).

%% API
-export([init/2, enable/4, disable/3, list/2]).

-include("logger.hrl").
-include("mod_carboncopy.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    clean_table().

enable(LUser, LServer, LResource, NS) ->
    ejabberd_riak:put(#carboncopy{us = {LUser, LServer},
				  resource = LResource,
				  version = NS},
		      carboncopy_schema(),
		      [{i, {LUser, LServer, LResource}},
		       {'2i', [{<<"us">>, {LUser, LServer}}]}]).

disable(LUser, LServer, LResource) ->
    ejabberd_riak:delete(carboncopy, {LUser, LServer, LResource}).

list(LUser, LServer) ->
    case ejabberd_riak:get_by_index(
	   carboncopy, carboncopy_schema(),
	   <<"us">>, {LUser, LServer}) of
	{ok, Rs} ->
	    {ok, [{Resource, NS, Node}
		  || #carboncopy{resource = Resource,
				 version = NS,
				 node = Node} <- Rs]};
	{error, _} = Err ->
	    Err
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
carboncopy_schema() ->
    {record_info(fields, carboncopy), #carboncopy{}}.

clean_table() ->
    ?DEBUG("Cleaning Riak 'carboncopy' table...", []),
    case ejabberd_riak:get(carboncopy, carboncopy_schema()) of
	{ok, Rs} ->
	    lists:foreach(
	      fun(#carboncopy{us = {U, S}, resource = R, node = Node})
		    when Node == node() ->
		      ejabberd_riak:delete(carboncopy, {U, S, R});
		 (_) ->
		      ok
	      end, Rs);
	{error, Reason} = Err ->
	    ?ERROR_MSG("Failed to clean Riak 'carboncopy' table: ~p", [Reason]),
	    Err
    end.
