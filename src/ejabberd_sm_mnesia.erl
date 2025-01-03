%%%-------------------------------------------------------------------
%%% File    : ejabberd_sm_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  9 Mar 2015 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

-module(ejabberd_sm_mnesia).

-behaviour(gen_server).
-behaviour(ejabberd_sm).

%% API
-export([init/0,
	 use_cache/1,
	 set_session/1,
	 delete_session/1,
	 get_sessions/0,
	 get_sessions/1,
	 get_sessions/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, start_link/0]).

-include("ejabberd_sm.hrl").
-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> ok | {error, any()}.
init() ->
    Spec = {?MODULE, {?MODULE, start_link, []},
	    transient, 5000, worker, [?MODULE]},
    case supervisor:start_child(ejabberd_backend_sup, Spec) of
	{ok, _Pid} -> ok;
	Err -> Err
    end.

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec use_cache(binary()) -> boolean().
use_cache(_LServer) ->
    false.

-spec set_session(#session{}) -> ok.
set_session(Session) ->
    mnesia:dirty_write(Session).

-spec delete_session(#session{}) -> ok.
delete_session(#session{sid = SID}) ->
    mnesia:dirty_delete(session, SID).

-spec get_sessions() -> [#session{}].
get_sessions() ->
    ets:tab2list(session).

-spec get_sessions(binary()) -> [#session{}].
get_sessions(LServer) ->
    mnesia:dirty_select(session,
			[{#session{usr = '$1', _ = '_'},
			  [{'==', {element, 2, '$1'}, LServer}], ['$_']}]).

-spec get_sessions(binary(), binary()) -> {ok, [#session{}]}.
get_sessions(LUser, LServer) ->
    {ok, mnesia:dirty_index_read(session, {LUser, LServer}, #session.us)}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    update_tables(),
    ejabberd_mnesia:create(?MODULE, session,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, session)},
			 {index, [usr,us]}]),
    ejabberd_mnesia:create(?MODULE, session_counter,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, session_counter)}]),
    mnesia:subscribe(system),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    ?INFO_MSG("Node ~p has left our Mnesia SM tables", [Node]),
    Sessions =
        ets:select(
          session,
          ets:fun2ms(
            fun(#session{sid = {_, Pid}} = S)
               when node(Pid) == Node ->
                    S
            end)),
    lists:foreach(
      fun(S) ->
              mnesia:dirty_delete_object(S)
      end, Sessions),
    {noreply, State};
handle_info({mnesia_system_event, {mnesia_up, Node}}, State) ->
    ?INFO_MSG("Node ~p joined our Mnesia SM tables", [Node]),
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_tables() ->
    case catch mnesia:table_info(session, attributes) of
      [ur, user, node] -> mnesia:delete_table(session);
      [ur, user, pid] -> mnesia:delete_table(session);
      [usr, us, pid] -> mnesia:delete_table(session);
      [usr, us, sid, priority, info] -> mnesia:delete_table(session);
      [sid, usr, us, priority] ->
	  mnesia:delete_table(session);
      [sid, usr, us, priority, info] -> ok;
      {'EXIT', _} -> ok
    end,
    case lists:member(presence, mnesia:system_info(tables))
	of
      true -> mnesia:delete_table(presence);
      false -> ok
    end,
    case lists:member(local_session, mnesia:system_info(tables)) of
	true ->
	    mnesia:delete_table(local_session);
	false ->
	    ok
    end.
