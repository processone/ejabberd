%%%-------------------------------------------------------------------
%%% File    : mod_caps_mnesia.erl
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

-module(mod_caps_mnesia).

-behaviour(mod_caps).

%% API
-export([init/2, caps_read/2, caps_write/3, import/3]).

-include("mod_caps.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    case catch mnesia:table_info(caps_features, storage_type) of
        {'EXIT', _} ->
            ok;
        disc_only_copies ->
            ok;
        _ ->
            mnesia:delete_table(caps_features)
    end,
    ejabberd_mnesia:create(?MODULE, caps_features,
                        [{disc_only_copies, [node()]},
                         {local_content, true},
                         {attributes,
                          record_info(fields, caps_features)}]),
    update_table(),
    mnesia:add_table_copy(caps_features, node(),
                          disc_only_copies).

caps_read(_LServer, Node) ->
    case mnesia:dirty_read({caps_features, Node}) of
	[#caps_features{features = Features}] -> {ok, Features};
	_ -> error
    end.

caps_write(_LServer, Node, Features) ->
    mnesia:dirty_write(#caps_features{node_pair = Node,
				      features = Features}).

import(_LServer, NodePair, [I]) when is_integer(I) ->
    mnesia:dirty_write(
      #caps_features{node_pair = NodePair, features = I});
import(_LServer, NodePair, Features) ->
    mnesia:dirty_write(
      #caps_features{node_pair = NodePair, features = Features}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_table() ->
    Fields = record_info(fields, caps_features),
    case mnesia:table_info(caps_features, attributes) of
        Fields ->
            ejabberd_config:convert_table_to_binary(
              caps_features, Fields, set,
              fun(#caps_features{node_pair = {N, _}}) -> N end,
              fun(#caps_features{node_pair = {N, P},
                                 features = Fs} = R) ->
                      NewFs = if is_integer(Fs) ->
                                      Fs;
                                 true ->
                                      [iolist_to_binary(F) || F <- Fs]
                              end,
                      R#caps_features{node_pair = {iolist_to_binary(N),
                                                   iolist_to_binary(P)},
                                      features = NewFs}
              end);
        _ ->
            ?INFO_MSG("Recreating caps_features table", []),
            mnesia:transform_table(caps_features, ignore, Fields)
    end.
