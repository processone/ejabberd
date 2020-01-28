%%%-------------------------------------------------------------------
%%% File    : mod_private_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2020   ProcessOne
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

-module(mod_private_mnesia).

-behaviour(mod_private).

%% API
-export([init/2, set_data/3, get_data/3, get_all_data/2, del_data/2,
	 use_cache/1, import/3]).
-export([need_transform/1, transform/1]).

-include("xmpp.hrl").
-include("mod_private.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, private_storage,
			   [{disc_only_copies, [node()]},
			    {attributes, record_info(fields, private_storage)}]).

use_cache(Host) ->
    case mnesia:table_info(private_storage, storage_type) of
	disc_only_copies ->
	    mod_private_opt:use_cache(Host);
	_ ->
	    false
    end.

set_data(LUser, LServer, Data) ->
    F = fun () ->
		lists:foreach(
		  fun({XmlNS, Xmlel}) ->
			  mnesia:write(
			    #private_storage{
			       usns = {LUser, LServer, XmlNS},
			       xml = Xmlel})
		  end, Data)
	end,
    transaction(F).

get_data(LUser, LServer, XmlNS) ->
    case mnesia:dirty_read(private_storage, {LUser, LServer, XmlNS}) of
	[#private_storage{xml = Storage_Xmlel}] ->
	    {ok, Storage_Xmlel};
	_ ->
	    error
    end.

get_all_data(LUser, LServer) ->
    case lists:flatten(
	   mnesia:dirty_select(private_storage,
			       [{#private_storage{usns = {LUser, LServer, '_'},
						  xml = '$1'},
				 [], ['$1']}])) of
	[] ->
	    error;
	Res ->
	    {ok, Res}
    end.

del_data(LUser, LServer) ->
    F = fun () ->
		Namespaces = mnesia:select(private_storage,
					   [{#private_storage{usns =
								  {LUser,
								   LServer,
								   '$1'},
							      _ = '_'},
					     [], ['$$']}]),
		lists:foreach(fun ([Namespace]) ->
				      mnesia:delete({private_storage,
						     {LUser, LServer,
						      Namespace}})
			      end,
			      Namespaces)
	end,
    transaction(F).

import(LServer, <<"private_storage">>,
       [LUser, XMLNS, XML, _TimeStamp]) ->
    El = #xmlel{} = fxml_stream:parse_element(XML),
    PS = #private_storage{usns = {LUser, LServer, XMLNS}, xml = El},
    mnesia:dirty_write(PS).

need_transform({private_storage, {U, S, NS}, _})
  when is_list(U) orelse is_list(S) orelse is_list(NS) ->
    ?INFO_MSG("Mnesia table 'private_storage' will be converted to binary", []),
    true;
need_transform(_) ->
    false.

transform(#private_storage{usns = {U, S, NS}, xml = El} = R) ->
    R#private_storage{usns = {iolist_to_binary(U),
			      iolist_to_binary(S),
			      iolist_to_binary(NS)},
		      xml = fxml:to_xmlel(El)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
transaction(F) ->
    case mnesia:transaction(F) of
	{atomic, Res} ->
	    Res;
	{aborted, Reason} ->
	    ?ERROR_MSG("Mnesia transaction failed: ~p", [Reason]),
	    {error, db_failure}
    end.
