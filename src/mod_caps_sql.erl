%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_caps_sql).
-behaviour(mod_caps).

-compile([{parse_transform, ejabberd_sql_pt}]).

%% API
-export([init/2, caps_read/2, caps_write/3, export/1]).

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

