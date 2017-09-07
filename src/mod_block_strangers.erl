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
-export([start/2, stop/1, reload/3,
         depends/2, mod_opt_type/1]).

-export([filter_packet/1, filter_offline_msg/1]).

-include("xmpp.hrl").
-include("ejabberd.hrl").
-include("logger.hrl").

-define(SETS, gb_sets).

start(Host, _Opts) ->
    ejabberd_hooks:add(user_receive_packet, Host,
                       ?MODULE, filter_packet, 25),
    ejabberd_hooks:add(offline_message_hook, Host,
		       ?MODULE, filter_offline_msg, 25).

stop(Host) ->
    ejabberd_hooks:delete(user_receive_packet, Host,
                          ?MODULE, filter_packet, 25),
    ejabberd_hooks:delete(offline_message_hook, Host,
			  ?MODULE, filter_offline_msg, 25).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

filter_packet({#message{from = From} = Msg, State} = Acc) ->
    LFrom = jid:tolower(From),
    LBFrom = jid:remove_resource(LFrom),
    #{pres_a := PresA} = State,
    case (?SETS):is_element(LFrom, PresA)
	orelse (?SETS):is_element(LBFrom, PresA)
        orelse sets_bare_member(LBFrom, PresA) of
	false ->
	    case check_message(Msg) of
		allow -> Acc;
		deny -> {stop, {drop, State}}
	    end;
	true ->
	    Acc
    end;
filter_packet(Acc) ->
    Acc.

filter_offline_msg({_Action, #message{} = Msg} = Acc) ->
    case check_message(Msg) of
	allow -> Acc;
	deny -> {stop, {drop, Msg}}
    end.

check_message(#message{from = From, to = To} = Msg) ->
    LServer = To#jid.lserver,
    AllowLocalUsers =
        gen_mod:get_module_opt(LServer, ?MODULE, allow_local_users, true),
    case (Msg#message.body == [] andalso
          Msg#message.subject == [])
        orelse ((AllowLocalUsers orelse From#jid.luser == <<"">>) andalso
                ejabberd_router:is_my_route(From#jid.lserver)) of
	false ->
	    {Sub, _} = ejabberd_hooks:run_fold(
			 roster_get_jid_info, LServer,
			 {none, []}, [To#jid.luser, LServer, From]),
	    case Sub of
		none ->
		    Drop = gen_mod:get_module_opt(LServer, ?MODULE, drop, true),
		    Log = gen_mod:get_module_opt(LServer, ?MODULE, log, false),
		    if
			Log ->
			    ?INFO_MSG("Dropping message from ~s to ~s: "
				      "the sender is not in the roster",
				      [jid:encode(From), jid:encode(To)]);
			true ->
			    ok
		    end,
		    if
			Drop ->
			    deny;
			true ->
			    allow
		    end;
		_ ->
		    allow
	    end;
	true ->
	    allow
    end.

sets_bare_member({U, S, <<"">>} = LBJID, Set) ->
    case ?SETS:next(sets_iterator_from(LBJID, Set)) of
        {{U, S, _}, _} -> true;
        _ -> false
    end.

-ifdef(GB_SETS_ITERATOR_FROM).
sets_iterator_from(Element, Set) ->
    ?SETS:iterator_from(Element, Set).
-else.
%% Copied from gb_sets.erl
%% TODO: Remove after dropping R17 support
sets_iterator_from(S, {_, T}) ->
    iterator_from(S, T, []).

iterator_from(S, {K, _, T}, As) when K < S ->
    iterator_from(S, T, As);
iterator_from(_, {_, nil, _} = T, As) ->
    [T | As];
iterator_from(S, {_, L, _} = T, As) ->
    iterator_from(S, L, [T | As]);
iterator_from(_, nil, As) ->
    As.
-endif.


depends(_Host, _Opts) ->
    [].

mod_opt_type(drop) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(log) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(allow_local_users) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(_) -> [drop, log, allow_local_users].
