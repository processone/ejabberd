%%%-------------------------------------------------------------------
%%% File    : ejabberd_redis.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  8 May 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(ejabberd_redis).
-ifndef(GEN_SERVER).
-define(GEN_SERVER, gen_server).
-endif.
-behaviour(?GEN_SERVER).

-compile({no_auto_import, [get/1, put/2]}).

%% API
-export([start_link/1, get_proc/1, get_connection/1, q/1, qp/1, format_error/1]).
%% Commands
-export([multi/1, get/1, set/2, del/1, info/1,
	 sadd/2, srem/2, smembers/1, sismember/2, scard/1,
	 hget/2, hset/3, hdel/2, hlen/1, hgetall/1, hkeys/1,
	 subscribe/1, publish/2, script_load/1, evalsha/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(TR_STACK, redis_transaction_stack).
-define(DEFAULT_MAX_QUEUE, 10000).
-define(MAX_RETRIES, 1).
-define(CALL_TIMEOUT, 60*1000). %% 60 seconds

-include("logger.hrl").
-include("ejabberd_stacktrace.hrl").

-record(state, {connection :: pid() | undefined,
		num :: pos_integer(),
		subscriptions = #{} :: subscriptions(),
		pending_q :: queue()}).

-type queue() :: p1_queue:queue({{pid(), term()}, integer()}).
-type subscriptions() :: #{binary() => [pid()]}.
-type error_reason() :: binary() | timeout | disconnected | overloaded.
-type redis_error() :: {error, error_reason()}.
-type redis_reply() :: undefined | binary() | [binary()].
-type redis_command() :: [iodata() | integer()].
-type redis_pipeline() :: [redis_command()].
-type redis_info() :: server | clients | memory | persistence |
		      stats | replication | cpu | commandstats |
		      cluster | keyspace | default | all.
-type state() :: #state{}.

-export_type([error_reason/0]).

-ifdef(USE_OLD_HTTP_URI). % Erlang/OTP lower than 21
-dialyzer([{no_return, do_connect/6},
           {no_unused, flush_queue/1},
           {no_match, flush_queue/1},
           {no_unused, re_subscribe/2},
           {no_match, handle_info/2}]).
-endif.

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
    call(get_rnd_id(), {q, Command}, ?MAX_RETRIES).

-spec qp(redis_pipeline()) -> [{ok, redis_reply()} | redis_error()] | redis_error().
qp(Pipeline) ->
    call(get_rnd_id(), {qp, Pipeline}, ?MAX_RETRIES).

-spec multi(fun(() -> any())) -> {ok, redis_reply()} | redis_error().
multi(F) ->
    case erlang:get(?TR_STACK) of
	undefined ->
	    erlang:put(?TR_STACK, []),
	    try F() of
		_ ->
		    Stack = erlang:erase(?TR_STACK),
		    Command = [["MULTI"]|lists:reverse([["EXEC"]|Stack])],
		    case qp(Command) of
			{error, _} = Err -> Err;
			Result -> get_result(Result)
		    end
	    catch ?EX_RULE(E, R, St) ->
		    erlang:erase(?TR_STACK),
		    erlang:raise(E, R, ?EX_STACK(St))
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

-spec subscribe([binary()]) -> ok | redis_error().
subscribe(Channels) ->
    try gen_server_call(get_proc(1), {subscribe, self(), Channels})
    catch exit:{Why, {?GEN_SERVER, call, _}} ->
	    Reason = case Why of
			 timeout -> timeout;
			 _ -> disconnected
		     end,
	    {error, Reason}
    end.

-spec publish(iodata(), iodata()) -> {ok, non_neg_integer()} | redis_error() | queued.
publish(Channel, Data) ->
    Cmd = [<<"PUBLISH">>, Channel, Data],
    case erlang:get(?TR_STACK) of
	undefined ->
	    case q(Cmd) of
		{ok, N} -> {ok, binary_to_integer(N)};
		{error, _} = Err -> Err
	    end;
	Stack ->
	    tr_enq(Cmd, Stack)
    end.

-spec script_load(iodata()) -> {ok, binary()} | redis_error().
script_load(Data) ->
    case erlang:get(?TR_STACK) of
	undefined ->
	    q([<<"SCRIPT">>, <<"LOAD">>, Data]);
	_ ->
	    erlang:error(transaction_unsupported)
    end.

-spec evalsha(binary(), [iodata()], [iodata() | integer()]) -> {ok, binary()} | redis_error().
evalsha(SHA, Keys, Args) ->
    case erlang:get(?TR_STACK) of
	undefined ->
	    q([<<"EVALSHA">>, SHA, length(Keys)|Keys ++ Args]);
	_ ->
	    erlang:error(transaction_unsupported)
    end.

-spec info(redis_info()) -> {ok, [{atom(), binary()}]} | redis_error().
info(Type) ->
    case erlang:get(?TR_STACK) of
	undefined ->
	    case q([<<"INFO">>, misc:atom_to_binary(Type)]) of
		{ok, Info} ->
		    Lines = binary:split(Info, <<"\r\n">>, [global]),
		    KVs = [binary:split(Line, <<":">>) || Line <- Lines],
		    {ok, [{misc:binary_to_atom(K), V} || [K, V] <- KVs]};
		{error, _} = Err ->
		    Err
	    end;
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
    CurrTime = erlang:monotonic_time(millisecond),
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
handle_call({subscribe, Caller, Channels}, _From,
	    #state{connection = Pid, subscriptions = Subs} = State) ->
    Subs1 = lists:foldl(
	      fun(Channel, Acc) ->
		      Callers = maps:get(Channel, Acc, []) -- [Caller],
		      maps:put(Channel, [Caller|Callers], Acc)
	      end, Subs, Channels),
    eredis_subscribe(Pid, Channels),
    {reply, ok, State#state{subscriptions = Subs1}};
handle_call(Request, _From, State) ->
    ?WARNING_MSG("Unexpected call: ~p", [Request]),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(connect, #state{connection = undefined} = State) ->
    NewState = case connect(State) of
		   {ok, Connection} ->
		       Q1 = flush_queue(State#state.pending_q),
		       re_subscribe(Connection, State#state.subscriptions),
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
handle_info({subscribed, Channel, Pid}, State) ->
    case State#state.connection of
	Pid ->
	    case maps:is_key(Channel, State#state.subscriptions) of
		true -> eredis_sub:ack_message(Pid);
		false ->
		    ?WARNING_MSG("Got subscription ack for unknown channel ~ts",
				 [Channel])
	    end;
	_ ->
	    ok
    end,
    {noreply, State};
handle_info({message, Channel, Data, Pid}, State) ->
    case State#state.connection of
	Pid ->
	    lists:foreach(
	      fun(Subscriber) ->
		      erlang:send(Subscriber, {redis_message, Channel, Data})
	      end, maps:get(Channel, State#state.subscriptions, [])),
	    eredis_sub:ack_message(Pid);
	_ ->
	    ok
    end,
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info = ~p", [Info]),
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
    Server1 = ejabberd_option:redis_server(),
    Port = ejabberd_option:redis_port(),
    DB = ejabberd_option:redis_db(),
    Pass = ejabberd_option:redis_password(),
    ConnTimeout = ejabberd_option:redis_connect_timeout(),
    Server = parse_server(Server1),
    try case do_connect(Num, Server, Port, Pass, DB, ConnTimeout) of
	    {ok, Client} ->
		?DEBUG("Connection #~p established to Redis at ~ts:~p",
		       [Num, Server, Port]),
		register(get_connection(Num), Client),
		{ok, Client};
	    {error, Why} ->
		erlang:error(Why)
	end
    catch _:Reason ->
	    Timeout = p1_rand:uniform(
			min(10, ejabberd_redis_sup:get_pool_size())),
	    ?ERROR_MSG("Redis connection #~p at ~ts:~p has failed: ~p; "
		       "reconnecting in ~p seconds",
		       [Num, Server, Port, Reason, Timeout]),
	    erlang:send_after(timer:seconds(Timeout), self(), connect),
	    {error, Reason}
    end.

parse_server([$u,$n,$i,$x,$: | Path]) ->
    {local, Path};
parse_server(Server) ->
    Server.

do_connect(1, Server, Port, Pass, _DB, _ConnTimeout) ->
    %% First connection in the pool is always a subscriber
    Options = [{host, Server},
               {port, Port},
               {password, Pass},
               {reconnect_sleep, no_reconnect},
               {max_queue_size, infinity},
               {queue_behaviour, drop}],
    Res = eredis_sub:start_link(Options),
    case Res of
	{ok, Pid} -> eredis_sub:controlling_process(Pid);
	_ -> ok
    end,
    Res;
do_connect(_, Server, Port, Pass, DB, ConnTimeout) ->
    Options = [{host, Server},
               {port, Port},
               {database, DB},
               {password, Pass},
               {reconnect_sleep, no_reconnect},
               {connect_timeout, ConnTimeout}],
    eredis:start_link(Options).

-spec call(pos_integer(), {q, redis_command()}, integer()) ->
		  {ok, redis_reply()} | redis_error();
	  (pos_integer(), {qp, redis_pipeline()}, integer()) ->
		  [{ok, redis_reply()} | redis_error()] | redis_error().
call(I, {F, Cmd}, Retries) ->
    ?DEBUG("Redis query: ~p", [Cmd]),
    Conn = get_connection(I),
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
	    try gen_server_call(get_proc(I), connect) of
		ok -> call(I, {F, Cmd}, Retries-1);
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

gen_server_call(Proc, Msg) ->
    case ejabberd_redis_sup:start() of
	ok ->
	    ?GEN_SERVER:call(Proc, Msg, ?CALL_TIMEOUT);
	{error, _} ->
	    {error, disconnected}
    end.

-spec log_error(redis_command() | redis_pipeline(), atom() | binary()) -> ok.
log_error(Cmd, Reason) ->
    ?ERROR_MSG("Redis request has failed:~n"
	       "** request = ~p~n"
	       "** response = ~ts",
	       [Cmd, format_error(Reason)]).

-spec get_rnd_id() -> pos_integer().
get_rnd_id() ->
    p1_rand:round_robin(ejabberd_redis_sup:get_pool_size() - 1) + 2.

-spec get_result([{ok, redis_reply()} | redis_error()]) ->
			{ok, redis_reply()} | redis_error().
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

-spec max_fsm_queue() -> pos_integer().
max_fsm_queue() ->
    proplists:get_value(max_queue, fsm_limit_opts(), ?DEFAULT_MAX_QUEUE).

fsm_limit_opts() ->
    ejabberd_config:fsm_limit_opts([]).

get_queue_type() ->
    ejabberd_option:redis_queue_type().

-spec flush_queue(queue()) -> queue().
flush_queue(Q) ->
    CurrTime = erlang:monotonic_time(millisecond),
    p1_queue:dropwhile(
      fun({From, Time}) ->
	      if (CurrTime - Time) >= ?CALL_TIMEOUT ->
		      ok;
		 true ->
		      ?GEN_SERVER:reply(From, ok)
	      end,
	      true
      end, Q).

-spec clean_queue(queue(), integer()) -> queue().
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

re_subscribe(Pid, Subs) ->
    case maps:keys(Subs) of
	[] -> ok;
	Channels -> eredis_subscribe(Pid, Channels)
    end.

eredis_subscribe(Pid, Channels) ->
    ?DEBUG("Redis query: ~p", [[<<"SUBSCRIBE">>|Channels]]),
    eredis_sub:subscribe(Pid, Channels).
