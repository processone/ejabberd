%%%-------------------------------------------------------------------
%%% Created : 12 Jan 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2021   ProcessOne
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
-module(mod_bosh_mnesia).

-behaviour(gen_server).
-behaviour(mod_bosh).

%% mod_bosh API
-export([init/0, open_session/2, close_session/1, find_session/1,
	 use_cache/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, start_link/0]).

-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(CALL_TIMEOUT, timer:minutes(10)).

-record(bosh, {sid = <<"">>      :: binary(),
               timestamp = erlang:timestamp() :: erlang:timestamp(),
               pid = self()      :: pid()}).

-record(state, {nodes = #{} :: #{node() => {pid(), reference()}}}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> ok | {error, any()}.
init() ->
    Spec = {?MODULE, {?MODULE, start_link, []},
	    transient, 5000, worker, [?MODULE]},
    case supervisor:start_child(ejabberd_backend_sup, Spec) of
	{ok, _Pid} -> ok;
	{error, {already_started, _}} -> ok;
	Err -> Err
    end.

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

use_cache() ->
    false.

-spec open_session(binary(), pid()) -> ok.
open_session(SID, Pid) ->
    Session = #bosh{sid = SID, timestamp = erlang:timestamp(), pid = Pid},
    gen_server:call(?MODULE, {write, Session}, ?CALL_TIMEOUT).

-spec close_session(binary()) -> ok.
close_session(SID) ->
    case mnesia:dirty_read(bosh, SID) of
	[Session] ->
	    gen_server:call(?MODULE, {delete, Session}, ?CALL_TIMEOUT);
	[] ->
	    ok
    end.

-spec find_session(binary()) -> {ok, pid()} | {error, notfound}.
find_session(SID) ->
    case mnesia:dirty_read(bosh, SID) of
        [#bosh{pid = Pid}] ->
            {ok, Pid};
        [] ->
            {error, notfound}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init([]) -> {ok, state()}.
init([]) ->
    setup_database(),
    multicast({join, node(), self()}),
    mnesia:subscribe(system),
    {ok, #state{}}.

-spec handle_call(_, _, state()) -> {reply, ok, state()} | {noreply, state()}.
handle_call({write, Session} = Msg, _From, State) ->
    write_session(Session),
    multicast(Msg),
    {reply, ok, State};
handle_call({delete, Session} = Msg, _From, State) ->
    delete_session(Session),
    multicast(Msg),
    {reply, ok, State};
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

-spec handle_info(_, state()) -> {noreply, state()}.
handle_info({write, Session}, State) ->
    write_session(Session),
    {noreply, State};
handle_info({delete, Session}, State) ->
    delete_session(Session),
    {noreply, State};
handle_info({join, Node, Pid}, State) ->
    ejabberd_cluster:send(Pid, {joined, node(), self()}),
    case maps:find(Node, State#state.nodes) of
	{ok, {Pid, _}} ->
	    ok;
	_ ->
	    ejabberd_cluster:send(Pid, {join, node(), self()})
    end,
    {noreply, State};
handle_info({joined, Node, Pid}, State) ->
    case maps:find(Node, State#state.nodes) of
	{ok, {Pid, _}} ->
	    {noreply, State};
	Ret ->
	    MRef = erlang:monitor(process, {?MODULE, Node}),
	    Nodes = maps:put(Node, {Pid, MRef}, State#state.nodes),
	    case Ret of
		error -> ejabberd_cluster:send(Pid, {first, self()});
		_ -> ok
	    end,
	    {noreply, State#state{nodes = Nodes}}
    end;
handle_info({first, From}, State) ->
    ejabberd_cluster:send(From, {replica, node(), first_session()}),
    {noreply, State};
handle_info({next, From, Key}, State) ->
    ejabberd_cluster:send(From, {replica, node(), next_session(Key)}),
    {noreply, State};
handle_info({replica, _From, '$end_of_table'}, State) ->
    {noreply, State};
handle_info({replica, From, Session}, State) ->
    write_session(Session),
    ejabberd_cluster:send(From, {next, self(), Session#bosh.sid}),
    {noreply, State};
handle_info({'DOWN', _, process, {?MODULE, _}, _Info}, State) ->
    {noreply, State};
handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    Sessions =
	ets:select(
	  bosh,
	  ets:fun2ms(
	    fun(#bosh{pid = Pid} = S) when node(Pid) == Node ->
		    S
	    end)),
    lists:foreach(
      fun(S) ->
	      mnesia:dirty_delete_object(S)
      end, Sessions),
    Nodes = maps:remove(Node, State#state.nodes),
    {noreply, State#state{nodes = Nodes}};
handle_info({mnesia_system_event, _}, State) ->
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
-spec write_session(#bosh{}) -> ok.
write_session(#bosh{pid = Pid1, sid = SID, timestamp = T1} = S1) ->
    case mnesia:dirty_read(bosh, SID) of
	[#bosh{pid = Pid2, timestamp = T2} = S2] ->
	    if Pid1 == Pid2 ->
		    mnesia:dirty_write(S1);
	       T1 < T2 ->
		    ejabberd_cluster:send(Pid2, replaced),
		    mnesia:dirty_write(S1);
	       true ->
		    ejabberd_cluster:send(Pid1, replaced),
		    mnesia:dirty_write(S2)
	    end;
	[] ->
	    mnesia:dirty_write(S1)
    end.

-spec delete_session(#bosh{}) -> ok.
delete_session(#bosh{sid = SID, pid = Pid1}) ->
    case mnesia:dirty_read(bosh, SID) of
	[#bosh{pid = Pid2}] ->
	    if Pid1 == Pid2 ->
		    mnesia:dirty_delete(bosh, SID);
	       true ->
		    ok
	    end;
	[] ->
	    ok
    end.

-spec multicast(_) -> ok.
multicast(Msg) ->
    lists:foreach(
      fun(Node) when Node /= node() ->
	      ejabberd_cluster:send({?MODULE, Node}, Msg);
	 (_) ->
	      ok
      end, ejabberd_cluster:get_nodes()).

setup_database() ->
    case catch mnesia:table_info(bosh, attributes) of
        [sid, pid] ->
            mnesia:delete_table(bosh);
        _ ->
            ok
    end,
    ejabberd_mnesia:create(?MODULE, bosh,
			[{ram_copies, [node()]}, {local_content, true},
			 {attributes, record_info(fields, bosh)}]).

-spec first_session() -> #bosh{} | '$end_of_table'.
first_session() ->
    case mnesia:dirty_first(bosh) of
	'$end_of_table' ->
	    '$end_of_table';
	First ->
	    read_session(First)
    end.

-spec next_session(binary()) -> #bosh{} | '$end_of_table'.
next_session(Prev) ->
    case mnesia:dirty_next(bosh, Prev) of
	'$end_of_table' ->
	    '$end_of_table';
	Next ->
	    read_session(Next)
    end.

-spec read_session(binary()) -> #bosh{} | '$end_of_table'.
read_session(Key) ->
    case mnesia:dirty_read(bosh, Key) of
	[#bosh{pid = Pid} = Session] when node(Pid) == node() ->
	    Session;
	_ ->
	    next_session(Key)
    end.
