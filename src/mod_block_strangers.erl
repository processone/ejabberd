%%%-------------------------------------------------------------------
%%% File    : mod_block_strangers.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Block packets from non-subscribers
%%% Created : 25 Dec 2016 by Alexey Shchepin <alexey@process-one.net>
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
%%%-------------------------------------------------------------------
-module(mod_block_strangers).

-author('alexey@process-one.net').

-behaviour(gen_mod).

%% API
-export([start/2, stop/1,
         depends/2, mod_opt_type/1]).

-export([filter_packet/3]).

-include("xmpp.hrl").
-include("ejabberd.hrl").
-include("logger.hrl").

-define(SETS, gb_sets).

start(Host, _Opts) ->
    ejabberd_hooks:add(c2s_filter_incoming_packet, Host,
                       ?MODULE, filter_packet, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(c2s_filter_incoming_packet, Host,
                          ?MODULE, filter_packet, 50),
    ok.

filter_packet(deny = Acc, _State, _Msg) ->
    Acc;
filter_packet(Acc, State, Msg) ->
    From = xmpp:get_from(Msg),
    LFrom = jid:tolower(From),
    LBFrom = jid:remove_resource(LFrom),
    #{pres_a := PresA} = State,
    case ejabberd_router:is_my_route(From#jid.lserver)
        orelse (?SETS):is_element(LFrom, PresA)
	orelse (?SETS):is_element(LBFrom, PresA)
        orelse sets_bare_member(LBFrom, PresA) of
	true ->
	    Acc;
	false ->
            #{lserver := LServer} = State,
            Drop =
                gen_mod:get_module_opt(LServer, ?MODULE, drop,
                                       fun(B) when is_boolean(B) -> B end,
                                       true),
            Log =
                gen_mod:get_module_opt(LServer, ?MODULE, log,
                                       fun(B) when is_boolean(B) -> B end,
                                       false),
            if
                Log ->
                    ?INFO_MSG("Drop packet: ~s",
                              [fxml:element_to_binary(
                                 xmpp:encode(Msg, ?NS_CLIENT))]);
                true ->
                    ok
            end,
            if
                Drop ->
                    deny;
                true ->
                    Acc
            end
    end.


sets_bare_member({U, S, <<"">>} = LBJID, Set) ->
    case ?SETS:next(?SETS:iterator_from(LBJID, Set)) of
        {{U, S, _}, _} -> true;
        _ -> false
    end.


depends(_Host, _Opts) ->
    [].

mod_opt_type(drop) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(log) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(_) -> [drop, log].
