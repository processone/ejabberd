%%%-------------------------------------------------------------------
%%% File    : mod_caps_riak.erl
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

-module(mod_caps_riak).

-behaviour(mod_caps).

%% API
-export([init/2, caps_read/2, caps_write/3, import/3]).

-include("mod_caps.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

caps_read(_LServer, Node) ->
    case ejabberd_riak:get(caps_features, caps_features_schema(), Node) of
	{ok, #caps_features{features = Features}} -> {ok, Features};
	_ -> error
    end.

caps_write(_LServer, Node, Features) ->
    ejabberd_riak:put(#caps_features{node_pair = Node,
				     features = Features},
		      caps_features_schema()).

import(_LServer, NodePair, [I]) when is_integer(I) ->
    ejabberd_riak:put(
      #caps_features{node_pair = NodePair, features = I},
      caps_features_schema());
import(_LServer, NodePair, Features) ->
    ejabberd_riak:put(
      #caps_features{node_pair = NodePair, features = Features},
      caps_features_schema()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
caps_features_schema() ->
    {record_info(fields, caps_features), #caps_features{}}.
