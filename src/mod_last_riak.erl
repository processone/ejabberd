%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_last_riak).
-behaviour(mod_last).

%% API
-export([init/2, import/2, get_last/2, store_last_info/4, remove_user/2]).

-include("mod_last.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

get_last(LUser, LServer) ->
    case ejabberd_riak:get(last_activity, last_activity_schema(),
			   {LUser, LServer}) of
        {ok, #last_activity{timestamp = TimeStamp,
                            status = Status}} ->
            {ok, TimeStamp, Status};
        {error, notfound} ->
            not_found;
        Err ->
            Err
    end.

store_last_info(LUser, LServer, TimeStamp, Status) ->
    US = {LUser, LServer},
    {atomic, ejabberd_riak:put(#last_activity{us = US,
                                              timestamp = TimeStamp,
                                              status = Status},
			       last_activity_schema())}.

remove_user(LUser, LServer) ->
    {atomic, ejabberd_riak:delete(last_activity, {LUser, LServer})}.

import(_LServer, #last_activity{} = LA) ->
    ejabberd_riak:put(LA, last_activity_schema()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
last_activity_schema() ->
    {record_info(fields, last_activity), #last_activity{}}.
