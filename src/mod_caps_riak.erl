%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_caps_riak).
-behaviour(mod_caps).

%% API
-export([init/2, caps_read/2, caps_write/3]).

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

%%%===================================================================
%%% Internal functions
%%%===================================================================
caps_features_schema() ->
    {record_info(fields, caps_features), #caps_features{}}.
