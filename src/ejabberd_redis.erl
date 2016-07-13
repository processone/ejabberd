%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  8 May 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(ejabberd_redis).

-behaviour(gen_server).
-behaviour(ejabberd_config).

%% API
-export([start/0, start_link/0, q/1, qp/1, opt_type/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PROCNAME, 'ejabberd_redis_client').

-include("logger.hrl").
-include("ejabberd.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    case lists:any(
	   fun(Host) ->
		   is_redis_configured(Host)
	   end, ?MYHOSTS) of
	true ->
	    Spec = {?MODULE, {?MODULE, start_link, []},
		    permanent, 2000, worker, [?MODULE]},
	    supervisor:start_child(ejabberd_sup, Spec);
	false ->
	    ok
    end.

q(Command) ->
    try eredis:q(?PROCNAME, Command)
    catch _:Reason -> {error, Reason}
    end.

qp(Pipeline) ->
    try eredis:qp(?PROCNAME, Pipeline)
    catch _:Reason -> {error, Reason}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    connect(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(connect, State) ->
    connect(),
    {noreply, State};
handle_info({'DOWN', _MRef, _Type, _Pid, Reason}, State) ->
    ?INFO_MSG("Redis connection has failed: ~p", [Reason]),
    connect(),
    {noreply, State};
handle_info({'EXIT', _, _}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    ?INFO_MSG("unexpected info = ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
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
		erlang:monitor(process, Client),
		register(?PROCNAME, Client),
		{ok, Client};
	    {error, Why} ->
		erlang:error(Why)
	end
    catch _:Reason ->
	    Timeout = 10,
	    ?ERROR_MSG("Redis connection at ~s:~p has failed: ~p; "
		       "reconnecting in ~p seconds",
		       [Server, Port, Reason, Timeout]),
	    erlang:send_after(timer:seconds(Timeout), self(), connect)
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
