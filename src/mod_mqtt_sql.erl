%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2002-2026 ProcessOne, SARL. All Rights Reserved.
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
-module(mod_mqtt_sql).
-behaviour(mod_mqtt).

%% API
-export([init/2, publish/6, delete_published/2, lookup_published/2]).
-export([list_topics/1]).
%% Unsupported backend API
-export([init/0]).
-export([subscribe/4, unsubscribe/2, find_subscriber/2]).
-export([open_session/1, close_session/1, lookup_session/1, get_sessions/2]).
-export([sql_schemas/0]).

-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    ?ERROR_MSG("Backend 'sql' is only supported for db_type", []),
    {error, db_failure}.

init(Host, _Opts) ->
    ejabberd_sql_schema:update_schema(Host, ?MODULE, sql_schemas()),
    ok.

sql_schemas() ->
    [#sql_schema{
        version = 1,
        tables =
            [#sql_table{
                name = <<"mqtt_pub">>,
                columns =
                    [#sql_column{name = <<"username">>, type = text},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"resource">>, type = text},
                     #sql_column{name = <<"topic">>, type = text},
                     #sql_column{name = <<"qos">>, type = smallint},
                     #sql_column{name = <<"payload">>, type = blob},
                     #sql_column{name = <<"payload_format">>, type = smallint},
                     #sql_column{name = <<"content_type">>, type = text},
                     #sql_column{name = <<"response_topic">>, type = text},
                     #sql_column{name = <<"correlation_data">>, type = blob},
                     #sql_column{name = <<"user_properties">>, type = blob},
                     #sql_column{name = <<"expiry">>, type = bigint}],
                indices = [#sql_index{
                              columns = [<<"topic">>, <<"server_host">>],
                              unique = true}]}]}].

publish({U, LServer, R}, Topic, Payload, QoS, Props, ExpiryTime) ->
    PayloadFormat = encode_pfi(maps:get(payload_format_indicator, Props, binary)),
    ResponseTopic = maps:get(response_topic, Props, <<"">>),
    CorrelationData = maps:get(correlation_data, Props, <<"">>),
    ContentType = maps:get(content_type, Props, <<"">>),
    UserProps = encode_props(maps:get(user_property, Props, [])),
    case ?SQL_UPSERT(LServer, "mqtt_pub",
		     ["!topic=%(Topic)s",
                      "!server_host=%(LServer)s",
		      "username=%(U)s",
		      "resource=%(R)s",
		      "payload=%(Payload)s",
		      "qos=%(QoS)d",
                      "payload_format=%(PayloadFormat)d",
                      "response_topic=%(ResponseTopic)s",
                      "correlation_data=%(CorrelationData)s",
                      "content_type=%(ContentType)s",
                      "user_properties=%(UserProps)s",
                      "expiry=%(ExpiryTime)d"]) of
	ok -> ok;
	_Err -> {error, db_failure}
    end.

delete_published({_, LServer, _}, Topic) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("delete from mqtt_pub where "
                "topic=%(Topic)s and %(LServer)H")) of
	{updated, _} -> ok;
	_Err -> {error, db_failure}
    end.

lookup_published({_, LServer, _}, Topic) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("select @(payload)s, @(qos)d, @(payload_format)d, "
                "@(content_type)s, @(response_topic)s, "
                "@(correlation_data)s, @(user_properties)s, @(expiry)d "
                "from mqtt_pub where topic=%(Topic)s and %(LServer)H")) of
	{selected, [{Payload, QoS, PayloadFormat, ContentType,
                     ResponseTopic, CorrelationData, EncProps, Expiry}]} ->
            try decode_props(EncProps) of
                UserProps ->
                    try decode_pfi(PayloadFormat) of
                        PFI ->
                            Props = #{payload_format_indicator => PFI,
                                      content_type => ContentType,
                                      response_topic => ResponseTopic,
                                      correlation_data => CorrelationData,
                                      user_property => UserProps},
                            {ok, {Payload, QoS, Props, Expiry}}
                    catch _:badarg ->
                            ?ERROR_MSG("Malformed value of 'payload_format' column "
                                       "for topic '~ts'", [Topic]),
                            {error, db_failure}
                    end
            catch _:badarg ->
                    ?ERROR_MSG("Malformed value of 'user_properties' column "
                               "for topic '~ts'", [Topic]),
                    {error, db_failure}
            end;
	{selected, []} ->
	    {error, notfound};
	_ ->
	    {error, db_failure}
    end.

list_topics(LServer) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("select @(topic)s from mqtt_pub where %(LServer)H")) of
	{selected, Res} ->
	    {ok, [Topic || {Topic} <- Res]};
	_ ->
	    {error, db_failure}
    end.

open_session(_) ->
    erlang:nif_error(unsupported_db).

close_session(_) ->
    erlang:nif_error(unsupported_db).

lookup_session(_) ->
    erlang:nif_error(unsupported_db).

get_sessions(_, _) ->
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
encode_pfi(binary) -> 0;
encode_pfi(utf8) -> 1.

decode_pfi(0) -> binary;
decode_pfi(1) -> utf8.

encode_props([]) -> <<"">>;
encode_props(L) -> term_to_binary(L).

decode_props(<<"">>) -> [];
decode_props(Bin) -> binary_to_term(Bin).
