%%%-------------------------------------------------------------------
%%% File    : ejabberd_sm_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  9 Mar 2015 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(ejabberd_sm_mnesia).

-behaviour(gen_server).
-behaviour(ejabberd_sm).

%% API
-export([init/0,
	 set_session/1,
	 delete_session/4,
	 get_sessions/0,
	 get_sessions/1,
	 get_sessions/2,
	 get_sessions/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, start_link/0]).

-include("ejabberd.hrl").
-include("ejabberd_sm.hrl").
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

-spec set_session(#session{}) -> ok.
set_session(Session) ->
    mnesia:dirty_write(Session).

-spec delete_session(binary(), binary(), binary(), sid()) ->
			    {ok, #session{}} | {error, notfound}.
delete_session(_LUser, _LServer, _LResource, SID) ->
    case mnesia:dirty_read(session, SID) of
	[Session] ->
	    mnesia:dirty_delete(session, SID),
	    {ok, Session};
	[] ->
	    {error, notfound}
    end.

-spec get_sessions() -> [#session{}].
get_sessions() ->
    ets:tab2list(session).

-spec get_sessions(binary()) -> [#session{}].
get_sessions(LServer) ->
    mnesia:dirty_select(session,
			[{#session{usr = '$1', _ = '_'},
			  [{'==', {element, 2, '$1'}, LServer}], ['$_']}]).

-spec get_sessions(binary(), binary()) -> [#session{}].
get_sessions(LUser, LServer) ->
    mnesia:dirty_index_read(session, {LUser, LServer}, #session.us).

-spec get_sessions(binary(), binary(), binary()) -> [#session{}].
get_sessions(LUser, LServer, LResource) ->
    mnesia:dirty_index_read(session, {LUser, LServer, LResource}, #session.usr).

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

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    ets:select_delete(
      session,
      ets:fun2ms(
	fun(#session{sid = {_, Pid}}) ->
		node(Pid) == Node
	end)),
    {noreply, State};
handle_info(_Info, State) ->
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
