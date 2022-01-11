%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2002-2021 ProcessOne, SARL. All Rights Reserved.
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
-module(mod_mqtt_mnesia).
-behaviour(mod_mqtt).

%% API
-export([init/2, publish/6, delete_published/2, lookup_published/2]).
-export([list_topics/1, use_cache/1]).
-export([init/0]).
-export([subscribe/4, unsubscribe/2, find_subscriber/2, mqtree_match/1]).
-export([open_session/1, close_session/1, lookup_session/1, get_sessions/2]).

-include("logger.hrl").
-include("mqtt.hrl").

-record(mqtt_pub, {topic_server            :: {binary(), binary()},
                   user                    :: binary(),
                   resource                :: binary(),
                   qos                     :: 0..2,
                   payload                 :: binary(),
                   expiry                  :: non_neg_integer(),
                   payload_format = binary :: binary | utf8,
                   response_topic = <<>>   :: binary(),
                   correlation_data = <<>> :: binary(),
                   content_type = <<>>     :: binary(),
                   user_properties = []    :: [{binary(), binary()}]}).

-record(mqtt_sub, {topic     :: {binary(), binary(), binary(), binary()},
		   options   :: sub_opts(),
		   id        :: non_neg_integer(),
		   pid       :: pid(),
		   timestamp :: erlang:timestamp()}).

-record(mqtt_session, {usr       :: jid:ljid() | {'_', '_', '$1'},
		       pid       :: pid() | '_',
		       timestamp :: erlang:timestamp() | '_'}).

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    case ejabberd_mnesia:create(
           ?MODULE, mqtt_pub,
           [{disc_only_copies, [node()]},
            {attributes, record_info(fields, mqtt_pub)}]) of
        {atomic, _} ->
            ok;
        Err ->
            {error, Err}
    end.

use_cache(Host) ->
    case mnesia:table_info(mqtt_pub, storage_type) of
        disc_only_copies ->
            mod_mqtt_opt:use_cache(Host);
        _ ->
            false
    end.

publish({U, LServer, R}, Topic, Payload, QoS, Props, ExpiryTime) ->
    PayloadFormat = maps:get(payload_format_indicator, Props, binary),
    ResponseTopic = maps:get(response_topic, Props, <<"">>),
    CorrelationData = maps:get(correlation_data, Props, <<"">>),
    ContentType = maps:get(content_type, Props, <<"">>),
    UserProps = maps:get(user_property, Props, []),
    mnesia:dirty_write(#mqtt_pub{topic_server = {Topic, LServer},
                                 user = U,
                                 resource = R,
                                 qos = QoS,
                                 payload = Payload,
                                 expiry = ExpiryTime,
                                 payload_format = PayloadFormat,
                                 response_topic = ResponseTopic,
                                 correlation_data = CorrelationData,
                                 content_type = ContentType,
                                 user_properties = UserProps}).

delete_published({_, S, _}, Topic) ->
    mnesia:dirty_delete(mqtt_pub, {Topic, S}).

lookup_published({_, S, _}, Topic) ->
    case mnesia:dirty_read(mqtt_pub, {Topic, S}) of
        [#mqtt_pub{qos = QoS,
                   payload = Payload,
                   expiry = ExpiryTime,
                   payload_format = PayloadFormat,
                   response_topic = ResponseTopic,
                   correlation_data = CorrelationData,
                   content_type = ContentType,
                   user_properties = UserProps}] ->
            Props = #{payload_format_indicator => PayloadFormat,
                      response_topic => ResponseTopic,
                      correlation_data => CorrelationData,
                      content_type => ContentType,
                      user_property => UserProps},
            {ok, {Payload, QoS, Props, ExpiryTime}};
        [] ->
            {error, notfound}
    end.

list_topics(S) ->
    {ok, [Topic || {Topic, S1} <- mnesia:dirty_all_keys(mqtt_pub), S1 == S]}.

init() ->
    case mqtree:whereis(mqtt_sub_index) of
        undefined ->
            T = mqtree:new(),
            mqtree:register(mqtt_sub_index, T);
        _ ->
	    ok
    end,
    try
	{atomic, ok} = ejabberd_mnesia:create(
			 ?MODULE,
			 mqtt_session,
			 [{ram_copies, [node()]},
			  {attributes, record_info(fields, mqtt_session)}]),
	{atomic, ok} = ejabberd_mnesia:create(
			 ?MODULE,
			 mqtt_sub,
			 [{ram_copies, [node()]},
			  {type, ordered_set},
			  {attributes, record_info(fields, mqtt_sub)}]),
	ok
    catch _:{badmatch, Err} ->
	    {error, Err}
    end.

open_session(USR) ->
    TS1 = misc:unique_timestamp(),
    P1 = self(),
    F = fun() ->
		case mnesia:read(mqtt_session, USR) of
		    [#mqtt_session{pid = P2, timestamp = TS2}] ->
			if TS1 >= TS2 ->
				mod_mqtt_session:route(P2, {replaced, P1}),
				mnesia:write(
				  #mqtt_session{usr = USR,
						pid = P1,
						timestamp = TS1});
			   true ->
				case is_process_dead(P2) of
				    true ->
					mnesia:write(
					  #mqtt_session{usr = USR,
							pid = P1,
							timestamp = TS1});
				    false ->
					mod_mqtt_session:route(P1, {replaced, P2})
				end
			end;
		    [] ->
			mnesia:write(
			  #mqtt_session{usr = USR,
					pid = P1,
					timestamp = TS1})
		end
	end,
    case mnesia:transaction(F) of
	{atomic, _} -> ok;
	{aborted, Reason} ->
	    db_fail("Failed to register MQTT session for ~ts",
		    Reason, [jid:encode(USR)])
    end.

close_session(USR) ->
    close_session(USR, self()).

lookup_session(USR) ->
    case mnesia:dirty_read(mqtt_session, USR) of
	[#mqtt_session{pid = Pid}] ->
	    case is_process_dead(Pid) of
		true ->
		    %% Read-Repair
		    close_session(USR, Pid),
		    {error, notfound};
		false ->
		    {ok, Pid}
	    end;
	[] ->
	    {error, notfound}
    end.

get_sessions(U, S) ->
    Resources = mnesia:dirty_select(mqtt_session,
                                    [{#mqtt_session{usr = {U, S, '$1'},
                                                    _ = '_'},
                                      [],
                                      ['$1']}]),
    [{U, S, Resource} || Resource <- Resources].

subscribe({U, S, R} = USR, TopicFilter, SubOpts, ID) ->
    T1 = misc:unique_timestamp(),
    P1 = self(),
    Key = {TopicFilter, S, U, R},
    F = fun() ->
		case mnesia:read(mqtt_sub, Key) of
		    [#mqtt_sub{timestamp = T2}] when T1 < T2 ->
			ok;
		    _ ->
			Tree = mqtree:whereis(mqtt_sub_index),
			mqtree:insert(Tree, TopicFilter),
			mnesia:write(
			  #mqtt_sub{topic = {TopicFilter, S, U, R},
				    options = SubOpts,
				    id = ID,
				    pid = P1,
				    timestamp = T1})
		end
	end,
    case mnesia:transaction(F) of
	{atomic, _} -> ok;
	{aborted, Reason} ->
	    db_fail("Failed to subscribe ~ts to ~ts",
		    Reason, [jid:encode(USR), TopicFilter])
    end.

unsubscribe({U, S, R} = USR, Topic) ->
    Pid = self(),
    F = fun() ->
		Tree = mqtree:whereis(mqtt_sub_index),
		mqtree:delete(Tree, Topic),
		case mnesia:read(mqtt_sub, {Topic, S, U, R}) of
		    [#mqtt_sub{pid = Pid} = Obj] ->
			mnesia:delete_object(Obj);
		    _ ->
			ok
		end
	end,
    case mnesia:transaction(F) of
	{atomic, _} -> ok;
	{aborted, Reason} ->
	    db_fail("Failed to unsubscribe ~ts from ~ts",
		    Reason, [jid:encode(USR), Topic])
    end.

mqtree_match(Topic) ->
    Tree = mqtree:whereis(mqtt_sub_index),
    mqtree:match(Tree, Topic).

mqtree_multi_match(Topic) ->
    {Res, []} = ejabberd_cluster:multicall(?MODULE, mqtree_match, [Topic]),
    lists:umerge(Res).

find_subscriber(S, Topic) when is_binary(Topic) ->
    case mqtree_multi_match(Topic) of
        [Filter|Filters] ->
            find_subscriber(S, {Filters, {Filter, S, '_', '_'}});
        [] ->
            {error, notfound}
    end;
find_subscriber(S, {Filters, {Filter, S, _, _} = Prev}) ->
    case mnesia:dirty_next(mqtt_sub, Prev) of
        {Filter, S, _, _} = Next ->
            case mnesia:dirty_read(mqtt_sub, Next) of
		[#mqtt_sub{options = SubOpts, id = ID, pid = Pid}] ->
		    case is_process_dead(Pid) of
			true ->
			    find_subscriber(S, {Filters, Next});
			false ->
                            {ok, {Pid, SubOpts, ID}, {Filters, Next}}
		    end;
                [] ->
                    find_subscriber(S, {Filters, Next})
            end;
        _ ->
            case Filters of
                [] ->
                    {error, notfound};
                [Filter1|Filters1] ->
                    find_subscriber(S, {Filters1, {Filter1, S, '_', '_'}})
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
close_session(USR, Pid) ->
    F = fun() ->
		case mnesia:read(mqtt_session, USR) of
		    [#mqtt_session{pid = Pid} = Obj] ->
			mnesia:delete_object(Obj);
		    _ ->
			ok
		end
	end,
    case mnesia:transaction(F) of
	{atomic, _} -> ok;
	{aborted, Reason} ->
	    db_fail("Failed to unregister MQTT session for ~ts",
		    Reason, [jid:encode(USR)])
    end.

is_process_dead(Pid) ->
    node(Pid) == node() andalso not is_process_alive(Pid).

db_fail(Format, Reason, Args) ->
    ?ERROR_MSG(Format ++ ": ~p", Args ++ [Reason]),
    {error, db_failure}.
