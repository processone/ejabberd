%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 15 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
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
    mnesia:create_table(carboncopy,
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
