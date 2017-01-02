%%%-------------------------------------------------------------------
%%% File    : mod_private_riak.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(mod_private_riak).

-behaviour(mod_private).

%% API
-export([init/2, set_data/3, get_data/3, get_all_data/2, remove_user/2,
	 import/3]).

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

import(LServer, <<"private_storage">>,
       [LUser, XMLNS, XML, _TimeStamp]) ->
    El = #xmlel{} = fxml_stream:parse_element(XML),
    PS = #private_storage{usns = {LUser, LServer, XMLNS}, xml = El},
    ejabberd_riak:put(PS, private_storage_schema(),
		      [{'2i', [{<<"us">>, {LUser, LServer}}]}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
private_storage_schema() ->
    {record_info(fields, private_storage), #private_storage{}}.
