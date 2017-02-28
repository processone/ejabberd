%%%-------------------------------------------------------------------
%%% Created : 12 Jan 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
-module(mod_bosh_mnesia).

-behaviour(gen_server).
-behaviour(mod_bosh).

%% mod_bosh API
-export([init/0, open_session/2, close_session/1, find_session/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, start_link/0]).

-include("logger.hrl").

-record(bosh, {sid = <<"">>      :: binary() | '_',
               timestamp = p1_time_compat:timestamp() :: erlang:timestamp() | '_',
               pid = self()      :: pid() | '$1'}).

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

open_session(SID, Pid) ->
    Session = #bosh{sid = SID, timestamp = p1_time_compat:timestamp(), pid = Pid},
    lists:foreach(
      fun(Node) when Node == node() ->
	      gen_server:call(?MODULE, {write, Session});
	 (Node) ->
	      cluster_send({?MODULE, Node}, {write, Session})
      end, ejabberd_cluster:get_nodes()).

close_session(SID) ->
    case mnesia:dirty_read(bosh, SID) of
	[Session] ->
	    lists:foreach(
	      fun(Node) when Node == node() ->
		      gen_server:call(?MODULE, {delete, Session});
		 (Node) ->
		      cluster_send({?MODULE, Node}, {delete, Session})
	      end, ejabberd_cluster:get_nodes());
	[] ->
	    ok
    end.

find_session(SID) ->
    case mnesia:dirty_read(bosh, SID) of
        [#bosh{pid = Pid}] ->
            {ok, Pid};
        [] ->
            error
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    setup_database(),
    {ok, #state{}}.

handle_call({write, Session}, _From, State) ->
    Res = write_session(Session),
    {reply, Res, State};
handle_call({delete, Session}, _From, State) ->
    Res = delete_session(Session),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({write, Session}, State) ->
    write_session(Session),
    {noreply, State};
handle_info({delete, Session}, State) ->
    delete_session(Session),
    {noreply, State};
handle_info(_Info, State) ->
    ?ERROR_MSG("got unexpected info: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
write_session(#bosh{pid = Pid1, sid = SID, timestamp = T1} = S1) ->
    case mnesia:dirty_read(bosh, SID) of
	[#bosh{pid = Pid2, timestamp = T2} = S2] ->
	    if Pid1 == Pid2 ->
		    mnesia:dirty_write(S1);
	       T1 < T2 ->
		    cluster_send(Pid2, replaced),
		    mnesia:dirty_write(S1);
	       true ->
		    cluster_send(Pid1, replaced),
		    mnesia:dirty_write(S2)
	    end;
	[] ->
	    mnesia:dirty_write(S1)
    end.

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

cluster_send(NodePid, Msg) ->
    erlang:send(NodePid, Msg, [noconnect, nosuspend]).

setup_database() ->
    case catch mnesia:table_info(bosh, attributes) of
        [sid, pid] ->
            mnesia:delete_table(bosh);
        _ ->
            ok
    end,
    ejabberd_mnesia:create(?MODULE, bosh,
			[{ram_copies, [node()]}, {local_content, true},
			 {attributes, record_info(fields, bosh)}]),
    mnesia:add_table_copy(bosh, node(), ram_copies).
