%%%-------------------------------------------------------------------
%%% File    : mod_irc_riak.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(mod_irc_riak).

-behaviour(mod_irc).

%% API
-export([init/2, get_data/3, set_data/4, import/2]).

-include("jid.hrl").
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
