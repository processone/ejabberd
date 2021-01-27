%%%----------------------------------------------------------------------
%%% File    : mod_push_mnesia.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Mnesia backend for Push Notifications (XEP-0357)
%%% Created : 15 Jul 2017 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2017-2021 ProcessOne
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

-module(mod_push_mnesia).
-author('holger@zedat.fu-berlin.de').

-behaviour(mod_push).

%% API
-export([init/2, store_session/6, lookup_session/4, lookup_session/3,
	 lookup_sessions/3, lookup_sessions/2, lookup_sessions/1,
	 delete_session/3, delete_old_sessions/2, transform/1]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("mod_push.hrl").

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, push_session,
			   [{disc_only_copies, [node()]},
			    {type, bag},
			    {attributes, record_info(fields, push_session)}]).

store_session(LUser, LServer, TS, PushJID, Node, XData) ->
    US = {LUser, LServer},
    PushLJID = jid:tolower(PushJID),
    MaxSessions = ejabberd_sm:get_max_user_sessions(LUser, LServer),
    F = fun() ->
		enforce_max_sessions(US, MaxSessions),
		mnesia:write(#push_session{us = US,
					   timestamp = TS,
					   service = PushLJID,
					   node = Node,
					   xml = encode_xdata(XData)})
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    {ok, {TS, PushLJID, Node, XData}};
	{aborted, E} ->
	    ?ERROR_MSG("Cannot store push session for ~ts@~ts: ~p",
		       [LUser, LServer, E]),
	    {error, db_failure}
    end.

lookup_session(LUser, LServer, PushJID, Node) ->
    PushLJID = jid:tolower(PushJID),
    MatchSpec = ets:fun2ms(
		  fun(#push_session{us = {U, S}, service = P, node = N} = Rec)
			when U == LUser,
			     S == LServer,
			     P == PushLJID,
			     N == Node ->
			  Rec
		  end),
    case mnesia:dirty_select(push_session, MatchSpec) of
	[#push_session{timestamp = TS, xml = El}] ->
	    {ok, {TS, PushLJID, Node, decode_xdata(El)}};
	[] ->
	    ?DEBUG("No push session found for ~ts@~ts (~p, ~ts)",
		   [LUser, LServer, PushJID, Node]),
	    {error, notfound}
    end.

lookup_session(LUser, LServer, TS) ->
    MatchSpec = ets:fun2ms(
		  fun(#push_session{us = {U, S}, timestamp = T} = Rec)
			when U == LUser,
			     S == LServer,
			     T == TS ->
			  Rec
		  end),
    case mnesia:dirty_select(push_session, MatchSpec) of
	[#push_session{service = PushLJID, node = Node, xml = El}] ->
	    {ok, {TS, PushLJID, Node, decode_xdata(El)}};
	[] ->
	    ?DEBUG("No push session found for ~ts@~ts (~p)",
		   [LUser, LServer, TS]),
	    {error, notfound}
    end.

lookup_sessions(LUser, LServer, PushJID) ->
    PushLJID = jid:tolower(PushJID),
    MatchSpec = ets:fun2ms(
		  fun(#push_session{us = {U, S}, service = P} = Rec)
			when U == LUser,
			     S == LServer,
			     P == PushLJID ->
			  Rec
		  end),
    Records = mnesia:dirty_select(push_session, MatchSpec),
    {ok, records_to_sessions(Records)}.

lookup_sessions(LUser, LServer) ->
    Records = mnesia:dirty_read(push_session, {LUser, LServer}),
    {ok, records_to_sessions(Records)}.

lookup_sessions(LServer) ->
    MatchSpec = ets:fun2ms(
		  fun(#push_session{us = {_U, S}} = Rec)
			when S == LServer ->
			  Rec
		  end),
    Records = mnesia:dirty_select(push_session, MatchSpec),
    {ok, records_to_sessions(Records)}.

delete_session(LUser, LServer, TS) ->
    MatchSpec = ets:fun2ms(
		  fun(#push_session{us = {U, S}, timestamp = T} = Rec)
			when U == LUser,
			     S == LServer,
			     T == TS ->
			  Rec
		  end),
    F = fun() ->
		Recs = mnesia:select(push_session, MatchSpec),
		lists:foreach(fun mnesia:delete_object/1, Recs)
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	{aborted, E} ->
	    ?ERROR_MSG("Cannot delete push session of ~ts@~ts: ~p",
		       [LUser, LServer, E]),
	    {error, db_failure}
    end.

delete_old_sessions(_LServer, Time) ->
    DelIfOld = fun(#push_session{timestamp = T} = Rec, ok) when T < Time ->
		       mnesia:delete_object(Rec);
		  (_Rec, ok) ->
		       ok
	       end,
    F = fun() ->
		mnesia:foldl(DelIfOld, ok, push_session)
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	{aborted, E} ->
	    ?ERROR_MSG("Cannot delete old push sessions: ~p", [E]),
	    {error, db_failure}
    end.

transform({push_session, US, TS, Service, Node, XData}) ->
    ?INFO_MSG("Transforming push_session Mnesia table", []),
    #push_session{us = US, timestamp = TS, service = Service,
		  node = Node, xml = encode_xdata(XData)}.

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec enforce_max_sessions({binary(), binary()}, non_neg_integer() | infinity)
      -> ok.
enforce_max_sessions(_US, infinity) ->
    ok;
enforce_max_sessions({U, S} = US, MaxSessions) ->
    case mnesia:wread({push_session, US}) of
	Recs when length(Recs) >= MaxSessions ->
	    Recs1 = lists:sort(fun(#push_session{timestamp = TS1},
				   #push_session{timestamp = TS2}) ->
				       TS1 >= TS2
			       end, Recs),
	    OldRecs = lists:nthtail(MaxSessions - 1, Recs1),
	    ?INFO_MSG("Disabling old push session(s) of ~ts@~ts", [U, S]),
	    lists:foreach(fun(Rec) -> mnesia:delete_object(Rec) end, OldRecs);
	_ ->
	    ok
    end.

decode_xdata(undefined) ->
    undefined;
decode_xdata(El) ->
    xmpp:decode(El).

encode_xdata(undefined) ->
    undefined;
encode_xdata(XData) ->
    xmpp:encode(XData).

records_to_sessions(Records) ->
    [{TS, PushLJID, Node, decode_xdata(El)}
     || #push_session{timestamp = TS,
		      service = PushLJID,
		      node = Node,
		      xml = El} <- Records].
