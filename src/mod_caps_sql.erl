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

%% API
-export([init/2, caps_read/2, caps_write/3, export/1]).

-include("mod_caps.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

caps_read(LServer, {Node, SubNode}) ->
    SNode = ejabberd_odbc:escape(Node),
    SSubNode = ejabberd_odbc:escape(SubNode),
    case ejabberd_odbc:sql_query(
	   LServer, [<<"select feature from caps_features where ">>,
		     <<"node='">>, SNode, <<"' and subnode='">>,
		     SSubNode, <<"';">>]) of
	{selected, [<<"feature">>], [[H]|_] = Fs} ->
	    case catch jlib:binary_to_integer(H) of
		Int when is_integer(Int), Int>=0 ->
		    {ok, Int};
		_ ->
		    {ok, lists:flatten(Fs)}
	    end;
	_ ->
	    error
    end.

caps_write(LServer, NodePair, Features) ->
    ejabberd_odbc:sql_transaction(
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
    SNode = ejabberd_odbc:escape(Node),
    SSubNode = ejabberd_odbc:escape(SubNode),
    NewFeatures = if is_integer(Features) ->
                          [jlib:integer_to_binary(Features)];
                     true ->
                          Features
                  end,
    [[<<"delete from caps_features where node='">>,
      SNode, <<"' and subnode='">>, SSubNode, <<"';">>]|
     [[<<"insert into caps_features(node, subnode, feature) ">>,
       <<"values ('">>, SNode, <<"', '">>, SSubNode, <<"', '">>,
       ejabberd_odbc:escape(F), <<"');">>] || F <- NewFeatures]].

