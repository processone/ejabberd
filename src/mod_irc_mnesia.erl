%%%-------------------------------------------------------------------
%%% File    : mod_irc_mnesia.erl
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

-module(mod_irc_mnesia).

-behaviour(mod_irc).

%% API
-export([init/2, get_data/3, set_data/4, import/2]).
-export([need_transform/1, transform/1]).

-include("jid.hrl").
-include("mod_irc.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, irc_custom,
			   [{disc_copies, [node()]},
			    {attributes, record_info(fields, irc_custom)}]).

get_data(_LServer, Host, From) ->
    {U, S, _} = jid:tolower(From),
    case catch mnesia:dirty_read({irc_custom, {{U, S}, Host}}) of
	{'EXIT', _Reason} -> error;
	[] -> empty;
	[#irc_custom{data = Data}] -> Data
    end.

set_data(_LServer, Host, From, Data) ->
    {U, S, _} = jid:tolower(From),
    F = fun () ->
		mnesia:write(#irc_custom{us_host = {{U, S}, Host},
					 data = Data})
	end,
    mnesia:transaction(F).

import(_LServer, #irc_custom{} = R) ->
    mnesia:dirty_write(R).

need_transform(#irc_custom{us_host = {{U, S}, H}})
  when is_list(U) orelse is_list(S) orelse is_list(H) ->
    ?INFO_MSG("Mnesia table 'irc_custom' will be converted to binary", []),
    true;
need_transform(_) ->
    false.

transform(#irc_custom{us_host = {{U, S}, H},
		      data = Data} = R) ->
    JID = jid:make(U, S),
    R#irc_custom{us_host = {{iolist_to_binary(U),
			     iolist_to_binary(S)},
			    iolist_to_binary(H)},
		 data = mod_irc:data_to_binary(JID, Data)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
