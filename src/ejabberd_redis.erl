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
-ifndef(GEN_SERVER).
-define(GEN_SERVER, gen_server).
-endif.
-behaviour(?GEN_SERVER).

-compile({no_auto_import, [get/1, put/2]}).

%% API
-export([start_link/1, get_proc/1, q/1, qp/1, format_error/1]).
%% Commands
-export([multi/1, get/1, set/2, del/1,
	 sadd/2, srem/2, smembers/1, sismember/2, scard/1,
	 hget/2, hset/3, hdel/2, hlen/1, hgetall/1, hkeys/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PROCNAME, 'ejabberd_redis_client').
-define(TR_STACK, redis_transaction_stack).
-define(DEFAULT_MAX_QUEUE, 5000).
-define(MAX_RETRIES, 1).
-define(CALL_TIMEOUT, 60*1000). %% 60 seconds

-include("logger.hrl").
-include("ejabberd.hrl").

-record(state, {connection :: pid() | undefined,
		num :: pos_integer(),
		pending_q :: p1_queue:queue()}).

-type redis_error() :: {error, binary() | timeout | disconnected | overloaded}.
-type redis_reply() :: binary() | [binary()].
-type redis_command() :: [binary()].
-type redis_pipeline() :: [redis_command()].
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================
start_link(I) ->
    ?GEN_SERVER:start_link({local, get_proc(I)}, ?MODULE, [I], []).

get_proc(I) ->
    misc:binary_to_atom(
      iolist_to_binary(
	[atom_to_list(?MODULE), $_, integer_to_list(I)])).

get_connection(I) ->
    misc:binary_to_atom(
      iolist_to_binary(
	[atom_to_list(?MODULE), "_connection_", integer_to_list(I)])).

-spec q(redis_command()) -> {ok, redis_reply()} | redis_error().
q(Command) ->
    call(get_worker(), {q, Command}, ?MAX_RETRIES).

-spec qp(redis_pipeline()) -> {ok, [redis_reply()]} | redis_error().
qp(Pipeline) ->
    call(get_worker(), {qp, Pipeline}, ?MAX_RETRIES).

-spec multi(fun(() -> any())) -> {ok, [redis_reply()]} | redis_error().
multi(F) ->
    case erlang:get(?TR_STACK) of
	undefined ->
	    erlang:put(?TR_STACK, []),
	    try F() of
		_ ->
		    Stack = erlang:get(?TR_STACK),
		    erlang:erase(?TR_STACK),
		    Command = [["MULTI"]|lists:reverse([["EXEC"]|Stack])],
		    case qp(Command) of
			{error, _} = Err -> Err;
			Result -> get_result(Result)
		    end
	    catch E:R ->
		    erlang:erase(?TR_STACK),
		    erlang:raise(E, R, erlang:get_stacktrace())
	    end;
	_ ->
	    erlang:error(nested_transaction)
    end.

-spec format_error(atom() | binary()) -> binary().
format_error(Reason) when is_atom(Reason) ->
    format_error(misc:atom_to_binary(Reason));
format_error(Reason) ->
    Reason.

%%%===================================================================
%%% Redis commands API
%%%===================================================================
-spec get(iodata()) -> {ok, undefined | binary()} | redis_error().
get(Key) ->
    case erlang:get(?TR_STACK) of
	undefined ->
	    q([<<"GET">>, Key]);
	_ ->
	    erlang:error(transaction_unsupported)
    end.

-spec set(iodata(), iodata()) -> ok | redis_error() | queued.
set(Key, Val) ->
    Cmd = [<<"SET">>, Key, Val],
    case erlang:get(?TR_STACK) of
	undefined ->
	    case q(Cmd) of
		{ok, <<"OK">>} -> ok;
		{error, _} = Err -> Err
	    end;
	Stack ->
	    tr_enq(Cmd, Stack)
    end.

-spec del(list()) -> {ok, non_neg_integer()} | redis_error() | queued.
del([]) ->
    reply(0);
del(Keys) ->
    Cmd = [<<"DEL">>|Keys],
    case erlang:get(?TR_STACK) of
	undefined ->
	    case q(Cmd) of
		{ok, N} -> {ok, binary_to_integer(N)};
		{error, _} = Err -> Err
	    end;
	Stack ->
	    tr_enq(Cmd, Stack)
    end.

-spec sadd(iodata(), list()) -> {ok, non_neg_integer()} | redis_error() | queued.
sadd(_Set, []) ->
    reply(0);
sadd(Set, Members) ->
    Cmd = [<<"SADD">>, Set|Members],
    case erlang:get(?TR_STACK) of
	undefined ->
	    case q(Cmd) of
		{ok, N} -> {ok, binary_to_integer(N)};
		{error, _} = Err -> Err
	    end;
	Stack ->
	    tr_enq(Cmd, Stack)
    end.

-spec srem(iodata(), list()) -> {ok, non_neg_integer()} | redis_error() | queued.
srem(_Set, []) ->
    reply(0);
srem(Set, Members) ->
    Cmd = [<<"SREM">>, Set|Members],
    case erlang:get(?TR_STACK) of
	undefined ->
	    case q(Cmd) of
		{ok, N} -> {ok, binary_to_integer(N)};
		{error, _} = Err -> Err
	    end;
	Stack ->
	    tr_enq(Cmd, Stack)
    end.

-spec smembers(iodata()) -> {ok, [binary()]} | redis_error().
smembers(Set) ->
    case erlang:get(?TR_STACK) of
	undefined ->
	    q([<<"SMEMBERS">>, Set]);
	_ ->
	    erlang:error(transaction_unsupported)
    end.

-spec sismember(iodata(), iodata()) -> boolean() | redis_error().
sismember(Set, Member) ->
    case erlang:get(?TR_STACK) of
	undefined ->
	    case q([<<"SISMEMBER">>, Set, Member]) of
		{ok, Flag} -> {ok, dec_bool(Flag)};
		{error, _} = Err -> Err
	    end;
	_ ->
	    erlang:error(transaction_unsupported)
    end.

-spec scard(iodata()) -> {ok, non_neg_integer()} | redis_error().
scard(Set) ->
    case erlang:get(?TR_STACK) of
	undefined ->
	    case q([<<"SCARD">>, Set]) of
		{ok, N} ->
		    {ok, binary_to_integer(N)};
		{error, _} = Err ->
		    Err
	    end;
	_ ->
	    erlang:error(transaction_unsupported)
    end.

-spec hget(iodata(), iodata()) -> {ok, undefined | binary()} | redis_error().
hget(Key, Field) ->
    case erlang:get(?TR_STACK) of
	undefined ->
	    q([<<"HGET">>, Key, Field]);
	_ ->
	    erlang:error(transaction_unsupported)
    end.

-spec hset(iodata(), iodata(), iodata()) -> {ok, boolean()} | redis_error() | queued.
hset(Key, Field, Val) ->
    Cmd = [<<"HSET">>, Key, Field, Val],
    case erlang:get(?TR_STACK) of
	undefined ->
	    case q(Cmd) of
		{ok, Flag} -> {ok, dec_bool(Flag)};
		{error, _} = Err -> Err
	    end;
	Stack ->
	    tr_enq(Cmd, Stack)
    end.

-spec hdel(iodata(), list()) -> {ok, non_neg_integer()} | redis_error() | queued.
hdel(_Key, []) ->
    reply(0);
hdel(Key, Fields) ->
    Cmd = [<<"HDEL">>, Key|Fields],
    case erlang:get(?TR_STACK) of
	undefined ->
	    case q(Cmd) of
		{ok, N} -> {ok, binary_to_integer(N)};
		{error, _} = Err -> Err
	    end;
	Stack ->
	    tr_enq(Cmd, Stack)
    end.

-spec hgetall(iodata()) -> {ok, [{binary(), binary()}]} | redis_error().
hgetall(Key) ->
    case erlang:get(?TR_STACK) of
	undefined ->
	    case q([<<"HGETALL">>, Key]) of
		{ok, Pairs} -> {ok, decode_pairs(Pairs)};
		{error, _} = Err -> Err
	    end;
	_ ->
	    erlang:error(transaction_unsupported)
    end.

-spec hlen(iodata()) -> {ok, non_neg_integer()} | redis_error().
hlen(Key) ->
    case erlang:get(?TR_STACK) of
	undefined ->
	    case q([<<"HLEN">>, Key]) of
		{ok, N} -> {ok, binary_to_integer(N)};
		{error, _} = Err -> Err
	    end;
	_ ->
	    erlang:error(transaction_unsupported)
    end.

-spec hkeys(iodata()) -> {ok, [binary()]} | redis_error().
hkeys(Key) ->
    case erlang:get(?TR_STACK) of
	undefined ->
	    q([<<"HKEYS">>, Key]);
	_ ->
	    erlang:error(transaction_unsupported)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([I]) ->
    process_flag(trap_exit, true),
    QueueType = get_queue_type(),
    Limit = max_fsm_queue(),
    self() ! connect,
    {ok, #state{num = I, pending_q = p1_queue:new(QueueType, Limit)}}.

handle_call(connect, From, #state{connection = undefined,
				  pending_q = Q} = State) ->
    CurrTime = p1_time_compat:monotonic_time(milli_seconds),
    Q2 = try p1_queue:in({From, CurrTime}, Q)
	 catch error:full ->
		 Q1 = clean_queue(Q, CurrTime),
		 p1_queue:in({From, CurrTime}, Q1)
	 end,
    {noreply, State#state{pending_q = Q2}};
handle_call(connect, From, #state{connection = Pid} = State) ->
    case is_process_alive(Pid) of
	true ->
	    {reply, ok, State};
	false ->
	    self() ! connect,
	    handle_call(connect, From, State#state{connection = undefined})
    end;
handle_call(Request, _From, State) ->
    ?WARNING_MSG("unexepected call: ~p", [Request]),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(connect, #state{connection = undefined} = State) ->
    NewState = case connect(State) of
		   {ok, Connection} ->
		       Q1 = flush_queue(State#state.pending_q),
		       State#state{connection = Connection, pending_q = Q1};
		   {error, _} ->
		       State
	       end,
    {noreply, NewState};
handle_info(connect, State) ->
    %% Already connected
    {noreply, State};
handle_info({'EXIT', Pid, _}, State) ->
    case State#state.connection of
	Pid ->
	    self() ! connect,
	    {noreply, State#state{connection = undefined}};
	_ ->
	    {noreply, State}
    end;
handle_info(Info, State) ->
    ?WARNING_MSG("unexpected info = ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec connect(state()) -> {ok, pid()} | {error, any()}.
connect(#state{num = Num}) ->
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
    ConnTimeout = timer:seconds(
		    ejabberd_config:get_option(
		      redis_connect_timeout,
		      fun(I) when is_integer(I), I>0 -> I end,
		      1)),
    try case eredis:start_link(Server, Port, DB, Pass,
			       no_reconnect, ConnTimeout) of
	    {ok, Client} ->
		?DEBUG("Connection #~p established to Redis at ~s:~p",
		       [Num, Server, Port]),
		register(get_connection(Num), Client),
		{ok, Client};
	    {error, Why} ->
		erlang:error(Why)
	end
    catch _:Reason ->
	    Timeout = randoms:uniform(
			min(10, ejabberd_redis_sup:get_pool_size())),
	    ?ERROR_MSG("Redis connection #~p at ~s:~p has failed: ~p; "
		       "reconnecting in ~p seconds",
		       [Num, Server, Port, Reason, Timeout]),
	    erlang:send_after(timer:seconds(Timeout), self(), connect),
	    {error, Reason}
    end.

-spec call({atom(), atom()}, {q, redis_command()}, integer()) ->
		  {ok, redis_reply()} | redis_error();
	  ({atom(), atom()}, {qp, redis_pipeline()}, integer()) ->
		  {ok, [redis_reply()]} | redis_error().
call({Conn, Parent}, {F, Cmd}, Retries) ->
    ?DEBUG("redis query: ~p", [Cmd]),
    Res = try eredis:F(Conn, Cmd, ?CALL_TIMEOUT) of
	      {error, Reason} when is_atom(Reason) ->
		  try exit(whereis(Conn), kill) catch _:_ -> ok end,
		  {error, disconnected};
	      Other ->
		  Other
	  catch exit:{timeout, _} -> {error, timeout};
		exit:{_, {gen_server, call, _}} -> {error, disconnected}
	  end,
    case Res of
	{error, disconnected} when Retries > 0 ->
	    try ?GEN_SERVER:call(Parent, connect, ?CALL_TIMEOUT) of
		ok -> call({Conn, Parent}, {F, Cmd}, Retries-1);
		{error, _} = Err -> Err
	    catch exit:{Why, {?GEN_SERVER, call, _}} ->
		    Reason1 = case Why of
				 timeout -> timeout;
				 _ -> disconnected
			     end,
		    log_error(Cmd, Reason1),
		    {error, Reason1}
	    end;
	{error, Reason1} ->
	    log_error(Cmd, Reason1),
	    Res;
	_ ->
	    Res
    end.

-spec log_error(redis_command() | redis_pipeline(), atom() | binary()) -> ok.
log_error(Cmd, Reason) ->
    ?ERROR_MSG("Redis request has failed:~n"
	       "** request = ~p~n"
	       "** response = ~s",
	       [Cmd, format_error(Reason)]).

-spec get_worker() -> {atom(), atom()}.
get_worker() ->
    Time = p1_time_compat:system_time(),
    I = erlang:phash2(Time, ejabberd_redis_sup:get_pool_size()) + 1,
    {get_connection(I), get_proc(I)}.

-spec get_result([{error, atom() | binary()} | {ok, iodata()}]) ->
			{ok, [redis_reply()]} | {error, binary()}.
get_result([{error, _} = Err|_]) ->
    Err;
get_result([{ok, _} = OK]) ->
    OK;
get_result([_|T]) ->
    get_result(T).

-spec tr_enq([iodata()], list()) -> queued.
tr_enq(Cmd, Stack) ->
    erlang:put(?TR_STACK, [Cmd|Stack]),
    queued.

-spec decode_pairs([binary()]) -> [{binary(), binary()}].
decode_pairs(Pairs) ->
    decode_pairs(Pairs, []).

-spec decode_pairs([binary()], [{binary(), binary()}]) -> [{binary(), binary()}].
decode_pairs([Field, Val|Pairs], Acc) ->
    decode_pairs(Pairs, [{Field, Val}|Acc]);
decode_pairs([], Acc) ->
    lists:reverse(Acc).

dec_bool(<<$1>>) -> true;
dec_bool(<<$0>>) -> false.

-spec reply(T) -> {ok, T} | queued.
reply(Val) ->
    case erlang:get(?TR_STACK) of
	undefined -> {ok, Val};
	_ -> queued
    end.

-spec iolist_to_list(iodata()) -> string().
iolist_to_list(IOList) ->
    binary_to_list(iolist_to_binary(IOList)).

-spec max_fsm_queue() -> pos_integer().
max_fsm_queue() ->
    proplists:get_value(max_queue, fsm_limit_opts(), ?DEFAULT_MAX_QUEUE).

fsm_limit_opts() ->
    ejabberd_config:fsm_limit_opts([]).

get_queue_type() ->
    case ejabberd_config:get_option(
	   redis_queue_type,
	   ejabberd_redis_sup:opt_type(redis_queue_type)) of
	undefined ->
	    ejabberd_config:default_queue_type(global);
	Type ->
	    Type
    end.

-spec flush_queue(p1_queue:queue()) -> p1_queue:queue().
flush_queue(Q) ->
    CurrTime = p1_time_compat:monotonic_time(milli_seconds),
    p1_queue:dropwhile(
      fun({From, Time}) ->
	      if (CurrTime - Time) >= ?CALL_TIMEOUT ->
		      ok;
		 true ->
		      ?GEN_SERVER:reply(From, ok)
	      end,
	      true
      end, Q).

-spec clean_queue(p1_queue:queue(), integer()) -> p1_queue:queue().
clean_queue(Q, CurrTime) ->
    Q1 = p1_queue:dropwhile(
	   fun({_From, Time}) ->
		   (CurrTime - Time) >= ?CALL_TIMEOUT
	   end, Q),
    Len = p1_queue:len(Q1),
    Limit = p1_queue:get_limit(Q1),
    if Len >= Limit ->
	    ?ERROR_MSG("Redis request queue is overloaded", []),
	    p1_queue:dropwhile(
	      fun({From, _Time}) ->
		      ?GEN_SERVER:reply(From, {error, overloaded}),
		      true
	      end, Q1);
       true ->
	    Q1
    end.
