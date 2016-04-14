%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_irc_riak).

-behaviour(mod_irc).

%% API
-export([init/2, get_data/3, set_data/4, import/2]).

-include("jlib.hrl").
-include("mod_irc.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

get_data(_LServer, Host, From) ->
    {U, S, _} = jid:tolower(From),
    case ejabberd_riak:get(irc_custom, irc_custom_schema(), {{U, S}, Host}) of
        {ok, #irc_custom{data = Data}} ->
            Data;
        {error, notfound} ->
            empty;
        _Err ->
            error
    end.

set_data(_LServer, Host, From, Data) ->
    {U, S, _} = jid:tolower(From),
    {atomic, ejabberd_riak:put(#irc_custom{us_host = {{U, S}, Host},
                                           data = Data},
			       irc_custom_schema())}.

import(_LServer, #irc_custom{} = R) ->
    ejabberd_riak:put(R, irc_custom_schema()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
irc_custom_schema() ->
    {record_info(fields, irc_custom), #irc_custom{}}.
