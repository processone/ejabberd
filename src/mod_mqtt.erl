%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2002-2019 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
-module(mod_mqtt).
-behaviour(p1_server).
-behaviour(gen_mod).
-behaviour(ejabberd_listener).

%% gen_mod API
-export([start/2, stop/1, reload/3, depends/2, mod_options/1, mod_opt_type/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
%% ejabberd_listener API
-export([start_link/2, listen_opt_type/1, listen_options/0, accept/1]).
%% Legacy ejabberd_listener API
-export([become_controller/2, socket_type/0]).
%% API
-export([open_session/1, close_session/1, lookup_session/1,
	 publish/3, subscribe/4, unsubscribe/2, select_retained/4,
         check_publish_access/2, check_subscribe_access/2]).

-include("logger.hrl").
-include("mqtt.hrl").

-define(MQTT_TOPIC_CACHE, mqtt_topic_cache).
-define(MQTT_PAYLOAD_CACHE, mqtt_payload_cache).

-type continuation() :: term().
-type seconds() :: non_neg_integer().

%% RAM backend callbacks
-callback init() -> ok | {error, any()}.
-callback open_session(jid:ljid()) -> ok | {error, db_failure}.
-callback close_session(jid:ljid()) -> ok | {error, db_failure}.
-callback lookup_session(jid:ljid()) -> {ok, pid()} | {error, notfound | db_failure}.
-callback subscribe(jid:ljid(), binary(), sub_opts(), non_neg_integer()) -> ok | {error, db_failure}.
-callback unsubscribe(jid:ljid(), binary()) -> ok | {error, notfound | db_failure}.
-callback find_subscriber(binary(), binary() | continuation()) ->
          {ok, {pid(), qos()}, continuation()} | {error, notfound | db_failure}.
%% Disc backend callbacks
-callback init(binary(), gen_mod:opts()) -> ok | {error, any()}.
-callback publish(jid:ljid(), binary(), binary(), qos(), properties(), seconds()) ->
          ok | {error, db_failure}.
-callback delete_published(jid:ljid(), binary()) -> ok | {error, db_failure}.
-callback lookup_published(jid:ljid(), binary()) ->
          {ok, {binary(), qos(), properties(), seconds()}} |
          {error, notfound | db_failure}.
-callback list_topics(binary()) -> {ok, [binary()]} | {error, db_failure}.
-callback use_cache(binary()) -> boolean().
-callback cache_nodes(binary()) -> [node()].

-optional_callbacks([use_cache/1, cache_nodes/1]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start({SockMod, Sock}, ListenOpts) ->
    mod_mqtt_session:start(SockMod, Sock, ListenOpts);
start(Host, Opts) ->
    gen_mod:start_child(?MODULE, Host, Opts).

start_link({SockMod, Sock}, ListenOpts) ->
    mod_mqtt_session:start_link(SockMod, Sock, ListenOpts).

stop(Host) ->
    gen_mod:stop_child(?MODULE, Host).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

socket_type() ->
    raw.

become_controller(Pid, _) ->
    accept(Pid).

accept(Pid) ->
    mod_mqtt_session:accept(Pid).

open_session({U, S, R}) ->
    Mod = gen_mod:ram_db_mod(S, ?MODULE),
    Mod:open_session({U, S, R}).

close_session({U, S, R}) ->
    Mod = gen_mod:ram_db_mod(S, ?MODULE),
    Mod:close_session({U, S, R}).

lookup_session({U, S, R}) ->
    Mod = gen_mod:ram_db_mod(S, ?MODULE),
    Mod:lookup_session({U, S, R}).

-spec publish(jid:ljid(), publish(), seconds()) ->
              {ok, non_neg_integer()} | {error, db_failure | publish_forbidden}.
publish({_, S, _} = USR, Pkt, ExpiryTime) ->
    case check_publish_access(Pkt#publish.topic, USR) of
        allow ->
            case retain(USR, Pkt, ExpiryTime) of
                ok ->
                    Mod = gen_mod:ram_db_mod(S, ?MODULE),
                    route(Mod, S, Pkt, ExpiryTime);
                {error, _} = Err ->
                    Err
            end;
        deny ->
            {error, publish_forbidden}
    end.

-spec subscribe(jid:ljid(), binary(), sub_opts(), non_neg_integer()) ->
                       ok | {error, db_failure | subscribe_forbidden}.
subscribe({_, S, _} = USR, TopicFilter, SubOpts, ID) ->
    Mod = gen_mod:ram_db_mod(S, ?MODULE),
    Limit = gen_mod:get_module_opt(S, ?MODULE, max_topic_depth),
    case check_topic_depth(TopicFilter, Limit) of
	allow ->
            case check_subscribe_access(TopicFilter, USR) of
                allow ->
                    Mod:subscribe(USR, TopicFilter, SubOpts, ID);
                deny ->
                    {error, subscribe_forbidden}
            end;
	deny ->
	    {error, subscribe_forbidden}
    end.

-spec unsubscribe(jid:ljid(), binary()) -> ok | {error, notfound | db_failure}.
unsubscribe({U, S, R}, Topic) ->
    Mod = gen_mod:ram_db_mod(S, ?MODULE),
    Mod:unsubscribe({U, S, R}, Topic).

-spec select_retained(jid:ljid(), binary(), qos(), non_neg_integer()) ->
                             [{publish(), seconds()}].
select_retained({_, S, _} = USR, TopicFilter, QoS, SubID) ->
    Mod = gen_mod:db_mod(S, ?MODULE),
    Limit = gen_mod:get_module_opt(S, ?MODULE, match_retained_limit),
    select_retained(Mod, USR, TopicFilter, QoS, SubID, Limit).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Host, Opts]) ->
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    RMod = gen_mod:ram_db_mod(Host, Opts, ?MODULE),
    try
	ok = Mod:init(Host, Opts),
	ok = RMod:init(),
	ok = init_cache(Mod, Host, Opts),
	{ok, #state{}}
    catch _:{badmatch, {error, Why}} ->
	    {stop, Why}
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Options
%%%===================================================================
mod_options(Host) ->
    [{match_retained_limit, 1000},
     {max_topic_depth, 8},
     {max_topic_aliases, 100},
     {session_expiry, 300},
     {max_queue, 5000},
     {access_subscribe, []},
     {access_publish, []},
     {db_type, ejabberd_config:default_db(Host, ?MODULE)},
     {ram_db_type, ejabberd_config:default_ram_db(Host, ?MODULE)},
     {queue_type, ejabberd_config:default_queue_type(Host)},
     {use_cache, ejabberd_config:use_cache(Host)},
     {cache_size, ejabberd_config:cache_size(Host)},
     {cache_missed, ejabberd_config:cache_missed(Host)},
     {cache_life_time, ejabberd_config:cache_life_time(Host)}].

mod_opt_type(max_queue) ->
    fun(I) when is_integer(I), I > 0 -> I;
       (infinity) -> unlimited;
       (unlimited) -> unlimited
    end;
mod_opt_type(session_expiry) ->
    fun(I) when is_integer(I), I>= 0 -> I end;
mod_opt_type(match_retained_limit) ->
    fun(I) when is_integer(I), I>0 -> I;
       (unlimited) -> infinity;
       (infinity) -> infinity
    end;
mod_opt_type(max_topic_depth) ->
    fun(I) when is_integer(I), I>0 -> I;
       (unlimited) -> infinity;
       (infinity) -> infinity
    end;
mod_opt_type(max_topic_aliases) ->
    fun(I) when is_integer(I), I>=0, I<65536 -> I end;
mod_opt_type(access_subscribe) ->
    fun validate_topic_access/1;
mod_opt_type(access_publish) ->
    fun validate_topic_access/1;
mod_opt_type(db_type) ->
    fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(ram_db_type) ->
    fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(queue_type) ->
    fun(ram) -> ram; (file) -> file end;
mod_opt_type(O) when O == cache_life_time; O == cache_size ->
    fun(I) when is_integer(I), I > 0 -> I;
       (infinity) -> infinity
    end;
mod_opt_type(O) when O == use_cache; O == cache_missed ->
    fun (B) when is_boolean(B) -> B end.

listen_opt_type(tls_verify) ->
    fun(B) when is_boolean(B) -> B end;
listen_opt_type(max_payload_size) ->
    fun(I) when is_integer(I), I>0 -> I;
       (unlimited) -> infinity;
       (infinity) -> infinity
    end.

listen_options() ->
    [{max_fsm_queue, 5000},
     {max_payload_size, infinity},
     {tls, false},
     {tls_verify, false}].

%%%===================================================================
%%% Internal functions
%%%===================================================================
route(Mod, LServer, Pkt, ExpiryTime) ->
    route(Mod, LServer, Pkt, ExpiryTime, Pkt#publish.topic, 0).

route(Mod, LServer, Pkt, ExpiryTime, Continuation, Num) ->
    case Mod:find_subscriber(LServer, Continuation) of
        {ok, {Pid, #sub_opts{no_local = true}, _}, Continuation1}
          when Pid == self() ->
            route(Mod, LServer, Pkt, ExpiryTime, Continuation1, Num);
	{ok, {Pid, SubOpts, ID}, Continuation1} ->
	    ?DEBUG("Route to ~p: ~s", [Pid, Pkt#publish.topic]),
            MinQoS = min(SubOpts#sub_opts.qos, Pkt#publish.qos),
            Retain = case SubOpts#sub_opts.retain_as_published of
                         false -> false;
                         true -> Pkt#publish.retain
                     end,
            Props = set_sub_id(ID, Pkt#publish.properties),
	    mod_mqtt_session:route(
              Pid, {Pkt#publish{qos = MinQoS,
                                dup = false,
                                retain = Retain,
                                properties = Props},
                    ExpiryTime}),
	    route(Mod, LServer, Pkt, ExpiryTime, Continuation1, Num+1);
	{error, _} ->
	    {ok, Num}
    end.

select_retained(Mod, {_, LServer, _} = USR, TopicFilter, QoS, SubID, Limit) ->
    Topics = match_topics(TopicFilter, LServer, Limit),
    lists:filtermap(
      fun({{Filter, _}, Topic}) ->
	      case lookup_published(Mod, USR, Topic) of
		  {ok, {Payload, QoS1, Props, ExpiryTime}} ->
                      Props1 = set_sub_id(SubID, Props),
		      {true, {#publish{topic = Topic,
                                       payload = Payload,
                                       retain = true,
                                       properties = Props1,
                                       qos = min(QoS, QoS1)},
                              ExpiryTime}};
		  error ->
		      ets:delete(?MQTT_TOPIC_CACHE, {Filter, LServer}),
		      false;
		  _ ->
		      false
	      end
      end, Topics).

match_topics(Topic, LServer, Limit) ->
    Filter = topic_filter(Topic),
    case Limit of
	infinity ->
	    ets:match_object(?MQTT_TOPIC_CACHE, {{Filter, LServer}, '_'});
	_ ->
	    case ets:select(?MQTT_TOPIC_CACHE,
			    [{{{Filter, LServer}, '_'}, [], ['$_']}], Limit) of
		{Topics, _} -> Topics;
		'$end_of_table' -> []
	    end
    end.

retain({_, S, _} = USR, #publish{retain = true,
                                 topic = Topic, payload = Data,
                                 qos = QoS, properties = Props},
       ExpiryTime) ->
    Mod = gen_mod:db_mod(S, ?MODULE),
    TopicKey = topic_key(Topic),
    case Data of
	<<>> ->
	    ets:delete(?MQTT_TOPIC_CACHE, {TopicKey, S}),
	    case use_cache(Mod, S) of
		true ->
		    ets_cache:delete(?MQTT_PAYLOAD_CACHE, {S, Topic},
				     cache_nodes(Mod, S));
		false ->
		    ok
	    end,
	    Mod:delete_published(USR, Topic);
	_ ->
	    ets:insert(?MQTT_TOPIC_CACHE, {{TopicKey, S}, Topic}),
	    case use_cache(Mod, S) of
		true ->
		    case ets_cache:update(
			   ?MQTT_PAYLOAD_CACHE, {S, Topic},
			   {ok, {Data, QoS, Props, ExpiryTime}},
			   fun() ->
				   Mod:publish(USR, Topic, Data,
                                               QoS, Props, ExpiryTime)
			   end, cache_nodes(Mod, S)) of
			{ok, _} -> ok;
			{error, _} = Err -> Err
		    end;
		false ->
		    Mod:publish(USR, Topic, Data, QoS, Props, ExpiryTime)
	    end
    end;
retain(_, _, _) ->
    ok.

lookup_published(Mod, {_, LServer, _} = USR, Topic) ->
    case use_cache(Mod, LServer) of
	true ->
	    ets_cache:lookup(
	      ?MQTT_PAYLOAD_CACHE, {LServer, Topic},
	      fun() ->
		      Mod:lookup_published(USR, Topic)
	      end);
	false ->
	    Mod:lookup_published(USR, Topic)
    end.

set_sub_id(0, Props) ->
    Props;
set_sub_id(ID, Props) ->
    Props#{subscription_identifier => [ID]}.

%%%===================================================================
%%% Matching functions
%%%===================================================================
topic_key(S) ->
    Parts = split_path(S),
    case join_key(Parts) of
	[<<>>|T] -> T;
	T -> T
    end.

topic_filter(S) ->
    Parts = split_path(S),
    case join_filter(Parts) of
	[<<>>|T] -> T;
	T -> T
    end.

join_key([X,Y|T]) ->
    [X, $/|join_key([Y|T])];
join_key([X]) ->
    [X];
join_key([]) ->
    [].

join_filter([X, <<$#>>]) ->
    [wildcard(X)|'_'];
join_filter([X,Y|T]) ->
    [wildcard(X), $/|join_filter([Y|T])];
join_filter([<<>>]) ->
    [];
join_filter([<<$#>>]) ->
    '_';
join_filter([X]) ->
    [wildcard(X)];
join_filter([]) ->
    [].

wildcard(<<$+>>) -> '_';
wildcard(Bin) -> Bin.

check_topic_depth(_Topic, infinity) ->
    allow;
check_topic_depth(_, N) when N=<0 ->
    deny;
check_topic_depth(<<$/, T/binary>>, N) ->
    check_topic_depth(T, N-1);
check_topic_depth(<<_, T/binary>>, N) ->
    check_topic_depth(T, N);
check_topic_depth(<<>>, _) ->
    allow.

split_path(Path) ->
    binary:split(Path, <<$/>>, [global]).

%%%===================================================================
%%% Validators
%%%===================================================================
validate_topic_access(FilterRules) ->
    lists:map(
      fun({TopicFilter, Access}) ->
              Rule = acl:access_rules_validator(Access),
              try
                  mqtt_codec:topic_filter(TopicFilter),
                  {split_path(TopicFilter), Rule}
              catch _:_ ->
                      ?ERROR_MSG("Invalid topic filter: ~s", [TopicFilter]),
                      erlang:error(badarg)
              end
      end, lists:reverse(lists:keysort(1, FilterRules))).

%%%===================================================================
%%% ACL checks
%%%===================================================================
check_subscribe_access(Topic, {_, S, _} = USR) ->
    Rules = gen_mod:get_module_opt(S, mod_mqtt, access_subscribe),
    check_access(Topic, USR, Rules).

check_publish_access(<<$$, _/binary>>, _) ->
    deny;
check_publish_access(Topic, {_, S, _} = USR) ->
    Rules = gen_mod:get_module_opt(S, mod_mqtt, access_publish),
    check_access(Topic, USR, Rules).

check_access(_, _, []) ->
    allow;
check_access(Topic, {U, S, R} = USR, FilterRules) ->
    TopicParts = binary:split(Topic, <<$/>>, [global]),
    case lists:any(
           fun({FilterParts, Rule}) ->
                   case match(TopicParts, FilterParts, U, S, R) of
                       true ->
                           allow == acl:match_rule(S, Rule, USR);
                       false ->
                           false
                   end
           end, FilterRules) of
        true -> allow;
        false -> deny
    end.

match(_, [<<"#">>|_], _, _, _) ->
    true;
match([], [<<>>, <<"#">>|_], _, _, _) ->
    true;
match([_|T1], [<<"+">>|T2], U, S, R) ->
    match(T1, T2, U, S, R);
match([H|T1], [<<"%u">>|T2], U, S, R) ->
    case jid:nodeprep(H) of
        U -> match(T1, T2, U, S, R);
        _ -> false
    end;
match([H|T1], [<<"%d">>|T2], U, S, R) ->
    case jid:nameprep(H) of
        S -> match(T1, T2, U, S, R);
        _ -> false
    end;
match([H|T1], [<<"%c">>|T2], U, S, R) ->
    case jid:resourceprep(H) of
        R -> match(T1, T2, U, S, R);
        _ -> false
    end;
match([H|T1], [H|T2], U, S, R) ->
    match(T1, T2, U, S, R);
match([], [], _, _, _) ->
    true;
match(_, _, _, _, _) ->
    false.

%%%===================================================================
%%% Cache stuff
%%%===================================================================
-spec init_cache(module(), binary(), gen_mod:opts()) -> ok | {error, db_failure}.
init_cache(Mod, Host, Opts) ->
    init_payload_cache(Mod, Host, Opts),
    init_topic_cache(Mod, Host).

-spec init_topic_cache(module(), binary()) -> ok | {error, db_failure}.
init_topic_cache(Mod, Host) ->
    catch ets:new(?MQTT_TOPIC_CACHE,
                  [named_table, ordered_set, public,
                   {heir, erlang:group_leader(), none}]),
    ?INFO_MSG("Building MQTT cache for ~s, this may take a while", [Host]),
    case Mod:list_topics(Host) of
        {ok, Topics} ->
            lists:foreach(
              fun(Topic) ->
                      ets:insert(?MQTT_TOPIC_CACHE,
                                 {{topic_key(Topic), Host}, Topic})
              end, Topics);
        {error, _} = Err ->
            Err
    end.

-spec init_payload_cache(module(), binary(), gen_mod:opts()) -> ok.
init_payload_cache(Mod, Host, Opts) ->
    case use_cache(Mod, Host) of
        true ->
            CacheOpts = cache_opts(Opts),
            ets_cache:new(?MQTT_PAYLOAD_CACHE, CacheOpts);
        false ->
            ets_cache:delete(?MQTT_PAYLOAD_CACHE)
    end.

-spec cache_opts(gen_mod:opts()) -> [proplists:property()].
cache_opts(Opts) ->
    MaxSize = gen_mod:get_opt(cache_size, Opts),
    CacheMissed = gen_mod:get_opt(cache_missed, Opts),
    LifeTime = case gen_mod:get_opt(cache_life_time, Opts) of
                   infinity -> infinity;
                   I -> timer:seconds(I)
               end,
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec use_cache(module(), binary()) -> boolean().
use_cache(Mod, Host) ->
    case erlang:function_exported(Mod, use_cache, 1) of
        true -> Mod:use_cache(Host);
        false -> gen_mod:get_module_opt(Host, ?MODULE, use_cache)
    end.

-spec cache_nodes(module(), binary()) -> [node()].
cache_nodes(Mod, Host) ->
    case erlang:function_exported(Mod, cache_nodes, 1) of
        true -> Mod:cache_nodes(Host);
        false -> ejabberd_cluster:get_nodes()
    end.
