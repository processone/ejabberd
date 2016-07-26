%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_private_riak).

-behaviour(mod_private).

%% API
-export([init/2, set_data/3, get_data/3, get_all_data/2, remove_user/2,
	 import/2]).

-include("xmpp.hrl").
-include("mod_private.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

set_data(LUser, LServer, Data) ->
    lists:foreach(
      fun({XMLNS, El}) ->
	      ejabberd_riak:put(#private_storage{usns = {LUser, LServer, XMLNS},
						 xml = El},
				private_storage_schema(),
				[{'2i', [{<<"us">>, {LUser, LServer}}]}])
      end, Data),
    {atomic, ok}.

get_data(LUser, LServer, XMLNS) ->
    case ejabberd_riak:get(private_storage, private_storage_schema(),
			   {LUser, LServer, XMLNS}) of
        {ok, #private_storage{xml = El}} ->
            {ok, El};
        _ ->
	    error
    end.

get_all_data(LUser, LServer) ->
    case ejabberd_riak:get_by_index(
           private_storage, private_storage_schema(),
	   <<"us">>, {LUser, LServer}) of
        {ok, Res} ->
            [El || #private_storage{xml = El} <- Res];
        _ ->
            []
    end.

remove_user(LUser, LServer) ->
    {atomic, ejabberd_riak:delete_by_index(private_storage,
                                           <<"us">>, {LUser, LServer})}.

import(_LServer, #private_storage{usns = {LUser, LServer, _}} = PS) ->
    ejabberd_riak:put(PS, private_storage_schema(),
		      [{'2i', [{<<"us">>, {LUser, LServer}}]}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
private_storage_schema() ->
    {record_info(fields, private_storage), #private_storage{}}.
