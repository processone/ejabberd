%%%-------------------------------------------------------------------
%%% File    : mod_announce_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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

-module(mod_announce_mnesia).

-behaviour(mod_announce).

%% API
-export([init/2, set_motd_users/2, set_motd/2, delete_motd/1,
	 get_motd/1, is_motd_user/2, set_motd_user/2, import/3]).
-export([need_transform/1, transform/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("mod_announce.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, motd,
			[{disc_only_copies, [node()]},
			 {attributes,
			  record_info(fields, motd)}]),
    ejabberd_mnesia:create(?MODULE, motd_users,
			[{disc_only_copies, [node()]},
			 {attributes,
			  record_info(fields, motd_users)}]).

set_motd_users(_LServer, USRs) ->
    F = fun() ->
		lists:foreach(
		  fun({U, S, _R}) ->
			  mnesia:write(#motd_users{us = {U, S}})
		  end, USRs)
	end,
    transaction(F).

set_motd(LServer, Packet) ->
    F = fun() ->
		mnesia:write(#motd{server = LServer, packet = Packet})
	end,
    transaction(F).

delete_motd(LServer) ->
    F = fun() ->
		mnesia:delete({motd, LServer}),
		mnesia:write_lock_table(motd_users),
		Users = mnesia:select(
			  motd_users,
			  [{#motd_users{us = '$1', _ = '_'},
			    [{'==', {element, 2, '$1'}, LServer}],
			    ['$1']}]),
		lists:foreach(fun(US) ->
				      mnesia:delete({motd_users, US})
			      end, Users)
	end,
    transaction(F).

get_motd(LServer) ->
    case mnesia:dirty_read({motd, LServer}) of
	[#motd{packet = Packet}] ->
	    {ok, Packet};
	[] ->
	    error
    end.

is_motd_user(LUser, LServer) ->
    case mnesia:dirty_read({motd_users, {LUser, LServer}}) of
	[#motd_users{}] -> {ok, true};
	_ -> {ok, false}
    end.

set_motd_user(LUser, LServer) ->
    F = fun() ->
		mnesia:write(#motd_users{us = {LUser, LServer}})
	end,
    transaction(F).

need_transform({motd, S, _}) when is_list(S) ->
    ?INFO_MSG("Mnesia table 'motd' will be converted to binary", []),
    true;
need_transform({motd_users, {U, S}, _}) when is_list(U) orelse is_list(S) ->
    ?INFO_MSG("Mnesia table 'motd_users' will be converted to binary", []),
    true;
need_transform(_) ->
    false.

transform(#motd{server = S, packet = P} = R) ->
    NewS = iolist_to_binary(S),
    NewP = fxml:to_xmlel(P),
    R#motd{server = NewS, packet = NewP};
transform(#motd_users{us = {U, S}} = R) ->
    NewUS = {iolist_to_binary(U), iolist_to_binary(S)},
    R#motd_users{us = NewUS}.

import(LServer, <<"motd">>, [<<>>, XML, _TimeStamp]) ->
    El = fxml_stream:parse_element(XML),
    mnesia:dirty_write(#motd{server = LServer, packet = El});
import(LServer, <<"motd">>, [LUser, <<>>, _TimeStamp]) ->
    mnesia:dirty_write(#motd_users{us = {LUser, LServer}}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
transaction(F) ->
    case mnesia:transaction(F) of
	{atomic, Res} ->
	    Res;
	{aborted, Reason} ->
	    ?ERROR_MSG("Mnesia transaction failed: ~p", [Reason]),
	    {error, db_failure}
    end.
