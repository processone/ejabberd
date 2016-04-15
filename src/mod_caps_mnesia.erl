%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_caps_mnesia).
-behaviour(mod_caps).

%% API
-export([init/2, caps_read/2, caps_write/3]).

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
    mnesia:create_table(caps_features,
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
