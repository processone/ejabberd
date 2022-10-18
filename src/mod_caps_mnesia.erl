%%%-------------------------------------------------------------------
%%% File    : mod_caps_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
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
-export([need_transform/1, transform/1]).

-include("mod_caps.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, caps_features,
			   [{disc_only_copies, [node()]},
			    {local_content, true},
			    {attributes, record_info(fields, caps_features)}]).

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

need_transform(#caps_features{node_pair = {N, P}, features = Fs}) ->
    case is_list(N) orelse is_list(P) orelse
	(is_list(Fs) andalso lists:any(fun is_list/1, Fs)) of
	true ->
	    ?INFO_MSG("Mnesia table 'caps_features' will be "
		      "converted to binary", []),
	    true;
	false ->
	    false
    end.

transform(#caps_features{node_pair = {N, P}, features = Fs} = R) ->
    NewFs = if is_integer(Fs) ->
		    Fs;
	       true ->
		    [iolist_to_binary(F) || F <- Fs]
	    end,
    R#caps_features{node_pair = {iolist_to_binary(N), iolist_to_binary(P)},
		    features = NewFs}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
