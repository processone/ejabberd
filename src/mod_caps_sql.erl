%%%-------------------------------------------------------------------
%%% File    : mod_caps_sql.erl
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

-module(mod_caps_sql).

-behaviour(mod_caps).

-compile([{parse_transform, ejabberd_sql_pt}]).

%% API
-export([init/2, caps_read/2, caps_write/3, export/1, import/3]).

-include("mod_caps.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

caps_read(LServer, {Node, SubNode}) ->
    case ejabberd_sql:sql_query(
           LServer,
           ?SQL("select @(feature)s from caps_features where"
                " node=%(Node)s and subnode=%(SubNode)s")) of
        {selected, [{H}|_] = Fs} ->
            case catch binary_to_integer(H) of
                Int when is_integer(Int), Int>=0 ->
                    {ok, Int};
                _ ->
                    {ok, [F || {F} <- Fs]}
            end;
        _ ->
            error
    end.

caps_write(LServer, NodePair, Features) ->
    ejabberd_sql:sql_transaction(
      LServer,
      sql_write_features_t(NodePair, Features)).

export(_Server) ->
    [{caps_features,
      fun(_Host, #caps_features{node_pair = NodePair,
                                features = Features}) ->
              sql_write_features_t(NodePair, Features);
         (_Host, _R) ->
              []
      end}].

import(_, _, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
sql_write_features_t({Node, SubNode}, Features) ->
    NewFeatures = if is_integer(Features) ->
                          [integer_to_binary(Features)];
                     true ->
                          Features
                  end,
    [?SQL("delete from caps_features where node=%(Node)s"
          " and subnode=%(SubNode)s;") |
     [?SQL("insert into caps_features(node, subnode, feature)"
           " values (%(Node)s, %(SubNode)s, %(F)s);") || F <- NewFeatures]].

