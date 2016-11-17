%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_last_sql).
-behaviour(mod_last).

-compile([{parse_transform, ejabberd_sql_pt}]).

%% API
-export([init/2, get_last/2, store_last_info/4, remove_user/2,
	 import/1, import/2, export/1]).

-include("mod_last.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

get_last(LUser, LServer) ->
    case catch sql_queries:get_last(LServer, LUser) of
        {selected, []} ->
            not_found;
        {selected, [{TimeStamp, Status}]} ->
            {ok, TimeStamp, Status};
        Reason ->
	    ?ERROR_MSG("failed to get last for user ~s@~s: ~p",
		       [LUser, LServer, Reason]),
	    {error, {invalid_result, Reason}}
    end.

store_last_info(LUser, LServer, TimeStamp, Status) ->
    sql_queries:set_last_t(LServer, LUser, TimeStamp, Status).

remove_user(LUser, LServer) ->
    sql_queries:del_last(LServer, LUser).

import(_LServer, _LA) ->
    pass.

export(_Server) ->
    [{last_activity,
      fun(Host, #last_activity{us = {LUser, LServer},
                               timestamp = TimeStamp, status = Status})
            when LServer == Host ->
              [?SQL("delete from last where username=%(LUser)s;"),
               ?SQL("insert into last(username, seconds, state)"
                    " values (%(LUser)s, %(TimeStamp)d, %(Status)s);")];
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select username, seconds, state from last">>,
      fun([LUser, TimeStamp, State]) ->
              #last_activity{us = {LUser, LServer},
                             timestamp = binary_to_integer(
                                           TimeStamp),
                             status = State}
      end}].

%%%===================================================================
%%% Internal functions
%%%===================================================================
