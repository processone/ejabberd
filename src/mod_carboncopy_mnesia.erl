%%%-------------------------------------------------------------------
%%% File    : mod_carboncopy_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 15 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(mod_carboncopy_mnesia).

-behaviour(mod_carboncopy).

%% API
-export([init/2, enable/4, disable/3, list/2]).

-include("mod_carboncopy.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    Fields = record_info(fields, carboncopy),
    try mnesia:table_info(carboncopy, attributes) of
	Fields ->
	    ok;
	_ ->
	    %% recreate..
	    mnesia:delete_table(carboncopy)
    catch _:_Error ->
	    %% probably table don't exist
	    ok
    end,
    ejabberd_mnesia:create(?MODULE, carboncopy,
			[{ram_copies, [node()]}, 
			 {attributes, record_info(fields, carboncopy)}, 
			 {type, bag}]),
    mnesia:add_table_copy(carboncopy, node(), ram_copies).

enable(LUser, LServer, LResource, NS) ->
    try mnesia:dirty_write(
	  #carboncopy{us = {LUser, LServer},
		      resource = LResource,
		      version = NS}) of
	ok -> ok
    catch _:Error ->
	    {error, Error}
    end.

disable(LUser, LServer, LResource) ->
    ToDelete = mnesia:dirty_match_object(
		 #carboncopy{us = {LUser, LServer},
			     resource = LResource,
			     version = '_'}),
    try lists:foreach(fun mnesia:dirty_delete_object/1, ToDelete) of
	ok -> ok
    catch _:Error ->
	    {error, Error}
    end.

list(LUser, LServer) ->
    mnesia:dirty_select(
      carboncopy,
      [{#carboncopy{us = {LUser, LServer}, resource = '$2', version = '$3'},
	[], [{{'$2','$3'}}]}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
