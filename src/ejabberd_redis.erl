%%%-------------------------------------------------------------------
%%% File    : ejabberd_redis.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  8 May 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(ejabberd_redis).

-behaviour(gen_server).
-behaviour(ejabberd_config).

%% API
-export([start_link/0, q/1, qp/1, config_reloaded/0, opt_type/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PROCNAME, 'ejabberd_redis_client').

-include("logger.hrl").
-include("ejabberd.hrl").

-record(state, {connection :: {pid(), reference()} | undefined}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

q(Command) ->
    try eredis:q(?PROCNAME, Command)
    catch _:Reason -> {error, Reason}
    end.

qp(Pipeline) ->
    try eredis:qp(?PROCNAME, Pipeline)
    catch _:Reason -> {error, Reason}
    end.

config_reloaded() ->
    case is_redis_configured() of
	true ->
	    ?MODULE ! connect;
	false ->
	    ?MODULE ! disconnect
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 20),
    process_flag(trap_exit, true),
    self() ! connect,
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(connect, #state{connection = undefined} = State) ->
    NewState = case is_redis_configured() of
		   true ->
		       case connect() of
			   {ok, Connection} ->
			       State#state{connection = Connection};
			   {error, _} ->
			       State
		       end;
		   false ->
		       State
	       end,
    {noreply, NewState};
handle_info(connect, State) ->
    %% Already connected
    {noreply, State};
handle_info(disconnect, #state{connection = {Pid, MRef}} = State) ->
    ?INFO_MSG("Disconnecting from Redis server", []),
    erlang:demonitor(MRef, [flush]),
    eredis:stop(Pid),
    {noreply, State#state{connection = undefined}};
handle_info(disconnect, State) ->
    %% Not connected
    {noreply, State};
handle_info({'DOWN', MRef, _Type, Pid, Reason},
	    #state{connection = {Pid, MRef}} = State) ->
    ?INFO_MSG("Redis connection has failed: ~p", [Reason]),
    connect(),
    {noreply, State#state{connection = undefined}};
handle_info({'EXIT', _, _}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    ?INFO_MSG("unexpected info = ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_hooks:delete(config_reloaded, ?MODULE, config_reloaded, 20).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
is_redis_configured() ->
    lists:any(fun is_redis_configured/1, ?MYHOSTS).

is_redis_configured(Host) ->
    ServerConfigured = ejabberd_config:has_option({redis_server, Host}),
    PortConfigured = ejabberd_config:has_option({redis_port, Host}),
    DBConfigured = ejabberd_config:has_option({redis_db, Host}),
    PassConfigured = ejabberd_config:has_option({redis_password, Host}),
    ReconnTimeoutConfigured = ejabberd_config:has_option(
				{redis_reconnect_timeout, Host}),
    ConnTimeoutConfigured = ejabberd_config:has_option(
			      {redis_connect_timeout, Host}),
    Modules = ejabberd_config:get_option(
		{modules, Host},
		fun(L) when is_list(L) -> L end, []),
    SMConfigured = ejabberd_config:get_option(
		     {sm_db_type, Host},
		     fun(V) -> V end) == redis,
    ModuleWithRedisDBConfigured =
	lists:any(
	  fun({Module, Opts}) ->
		  gen_mod:db_type(Host, Opts, Module) == redis
	  end, Modules),
    ServerConfigured or PortConfigured or DBConfigured or PassConfigured or
	ReconnTimeoutConfigured or ConnTimeoutConfigured or
	SMConfigured or ModuleWithRedisDBConfigured.

iolist_to_list(IOList) ->
    binary_to_list(iolist_to_binary(IOList)).

connect() ->
    Server = ejabberd_config:get_option(redis_server,
					fun iolist_to_list/1,
					"localhost"),
    Port = ejabberd_config:get_option(redis_port,
				      fun(P) when is_integer(P),
						  P>0, P<65536 ->
					      P
				      end, 6379),
    DB = ejabberd_config:get_option(redis_db,
				    fun(I) when is_integer(I), I >= 0 ->
					    I
				    end, 0),
    Pass = ejabberd_config:get_option(redis_password,
				      fun iolist_to_list/1,
				      ""),
    ReconnTimeout = timer:seconds(
		      ejabberd_config:get_option(
			redis_reconnect_timeout,
			fun(I) when is_integer(I), I>0 -> I end,
			1)),
    ConnTimeout = timer:seconds(
		    ejabberd_config:get_option(
		      redis_connect_timeout,
		      fun(I) when is_integer(I), I>0 -> I end,
		      1)),
    try case eredis:start_link(Server, Port, DB, Pass,
			       ReconnTimeout, ConnTimeout) of
	    {ok, Client} ->
		?INFO_MSG("Connected to Redis at ~s:~p", [Server, Port]),
		unlink(Client),
		MRef = erlang:monitor(process, Client),
		register(?PROCNAME, Client),
		{ok, {Client, MRef}};
	    {error, Why} ->
		erlang:error(Why)
	end
    catch _:Reason ->
	    Timeout = 10,
	    ?ERROR_MSG("Redis connection at ~s:~p has failed: ~p; "
		       "reconnecting in ~p seconds",
		       [Server, Port, Reason, Timeout]),
	    erlang:send_after(timer:seconds(Timeout), self(), connect),
	    {error, Reason}
    end.

opt_type(redis_connect_timeout) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(redis_db) ->
    fun (I) when is_integer(I), I >= 0 -> I end;
opt_type(redis_password) -> fun iolist_to_list/1;
opt_type(redis_port) ->
    fun (P) when is_integer(P), P > 0, P < 65536 -> P end;
opt_type(redis_reconnect_timeout) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(redis_server) -> fun iolist_to_list/1;
opt_type(_) ->
    [redis_connect_timeout, redis_db, redis_password,
     redis_port, redis_reconnect_timeout, redis_server].
