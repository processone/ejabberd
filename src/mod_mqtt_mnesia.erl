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
-module(mod_mqtt_mnesia).
-behaviour(mod_mqtt).

%% API
-export([init/2, publish/6, delete_published/2, lookup_published/2]).
-export([list_topics/1, use_cache/1]).
%% Unsupported backend API
-export([init/0]).
-export([subscribe/4, unsubscribe/2, find_subscriber/2]).
-export([open_session/1, close_session/1, lookup_session/1]).

-include("logger.hrl").

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
            gen_mod:get_module_opt(Host, mod_mqtt, use_cache);
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
            Props = #{payload_format => PayloadFormat,
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
    erlang:nif_error(unsupported_db).

open_session(_) ->
    erlang:nif_error(unsupported_db).

close_session(_) ->
    erlang:nif_error(unsupported_db).

lookup_session(_) ->
    erlang:nif_error(unsupported_db).

subscribe(_, _, _, _) ->
    erlang:nif_error(unsupported_db).

unsubscribe(_, _) ->
    erlang:nif_error(unsupported_db).

find_subscriber(_, _) ->
    erlang:nif_error(unsupported_db).

%%%===================================================================
%%% Internal functions
%%%===================================================================
