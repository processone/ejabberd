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
-define(MQTT_VERSION_4, 4).
-define(MQTT_VERSION_5, 5).

-record(connect, {proto_level             = 4     :: non_neg_integer(),
		  will                            :: undefined | publish(),
		  clean_start             = true  :: boolean(),
		  keep_alive              = 0     :: non_neg_integer(),
		  client_id               = <<>>  :: binary(),
		  username                = <<>>  :: binary(),
		  password                = <<>>  :: binary(),
                  will_properties         = #{}   :: properties(),
                  properties              = #{}   :: properties()}).
-record(connack, {session_present = false    :: boolean(),
		  code            = success  :: reason_code(),
                  properties      = #{}      :: properties()}).

-record(publish, {id :: undefined | non_neg_integer(),
		  dup = false :: boolean(),
		  qos = 0 :: qos(),
		  retain = false :: boolean(),
		  topic :: binary(),
		  payload :: binary(),
                  properties = #{} :: properties(),
		  meta = #{} :: map()}).
-record(puback, {id :: non_neg_integer(),
                 code = success :: reason_code(),
                 properties = #{} :: properties()}).
-record(pubrec, {id :: non_neg_integer(),
                 code = success :: reason_code(),
                 properties = #{} :: properties()}).
-record(pubrel, {id :: non_neg_integer(),
                 code = success :: reason_code(),
                 properties = #{} :: properties(),
                 meta = #{} :: map()}).
-record(pubcomp, {id :: non_neg_integer(),
                  code = success :: reason_code(),
                  properties = #{} :: properties()}).

-record(subscribe, {id :: non_neg_integer(),
		    filters :: [{binary(), sub_opts()}],
                    properties = #{} :: properties(),
                    meta = #{} :: map()}).
-record(suback, {id :: non_neg_integer(),
		 codes = [] :: [char() | reason_code()],
                 properties = #{} :: properties()}).

-record(unsubscribe, {id :: non_neg_integer(),
		      filters :: [binary()],
                      properties = #{} :: properties(),
                      meta = #{} :: map()}).
-record(unsuback, {id :: non_neg_integer(),
                   codes = [] :: [reason_code()],
                   properties = #{} :: properties()}).

-record(pingreq, {meta = #{} :: map()}).
-record(pingresp, {}).

-record(disconnect, {code = 'normal-disconnection' :: reason_code(),
                     properties = #{} :: properties()}).

-record(auth, {code = success :: reason_code(),
               properties = #{} :: properties()}).

-record(sub_opts, {qos = 0 :: qos(),
                   no_local = false :: boolean(),
                   retain_as_published = false :: boolean(),
                   retain_handling = 0 :: 0..2}).

-type qos() :: 0|1|2.
-type sub_opts() :: #sub_opts{}.
-type utf8_pair() :: {binary(), binary()}.
-type properties() :: #{assigned_client_identifier => binary(),
			authentication_data => binary(),
			authentication_method => binary(),
			content_type => binary(),
			correlation_data => binary(),
			maximum_packet_size => pos_integer(),
			maximum_qos => 0|1,
			message_expiry_interval => non_neg_integer(),
			payload_format_indicator => binary | utf8,
			reason_string => binary(),
			receive_maximum => pos_integer(),
			request_problem_information => boolean(),
			request_response_information => boolean(),
			response_information => binary(),
			response_topic => binary(),
			retain_available => boolean(),
			server_keep_alive => non_neg_integer(),
			server_reference => binary(),
			session_expiry_interval => non_neg_integer(),
			shared_subscription_available => boolean(),
			subscription_identifier => [non_neg_integer()] | non_neg_integer(),
			subscription_identifiers_available => boolean(),
			topic_alias => pos_integer(),
			topic_alias_maximum => non_neg_integer(),
			user_property => [utf8_pair()],
			wildcard_subscription_available => boolean(),
			will_delay_interval => non_neg_integer()}.
-type property() :: assigned_client_identifier |
                    authentication_data |
                    authentication_method |
                    content_type |
                    correlation_data |
                    maximum_packet_size |
                    maximum_qos |
                    message_expiry_interval |
                    payload_format_indicator |
                    reason_string |
                    receive_maximum |
                    request_problem_information |
                    request_response_information |
                    response_information |
                    response_topic |
                    retain_available |
                    server_keep_alive |
                    server_reference |
                    session_expiry_interval |
                    shared_subscription_available |
                    subscription_identifier |
                    subscription_identifiers_available |
                    topic_alias |
                    topic_alias_maximum |
                    user_property |
                    wildcard_subscription_available |
                    will_delay_interval.
-type reason_code() :: 'success' |
                       'normal-disconnection' |
                       'granted-qos-0' |
                       'granted-qos-1' |
                       'granted-qos-2' |
                       'disconnect-with-will-message' |
                       'no-matching-subscribers' |
                       'no-subscription-existed' |
                       'continue-authentication' |
                       're-authenticate' |
                       'unspecified-error' |
                       'malformed-packet' |
                       'protocol-error' |
                       'implementation-specific-error' |
                       'unsupported-protocol-version' |
                       'client-identifier-not-valid' |
                       'bad-user-name-or-password' |
                       'not-authorized' |
                       'server-unavailable' |
                       'server-busy' |
                       'banned' |
                       'server-shutting-down' |
                       'bad-authentication-method' |
                       'keep-alive-timeout' |
                       'session-taken-over' |
                       'topic-filter-invalid' |
                       'topic-name-invalid' |
                       'packet-identifier-in-use' |
                       'packet-identifier-not-found' |
                       'receive-maximum-exceeded' |
                       'topic-alias-invalid' |
                       'packet-too-large' |
                       'message-rate-too-high' |
                       'quota-exceeded' |
                       'administrative-action' |
                       'payload-format-invalid' |
                       'retain-not-supported' |
                       'qos-not-supported' |
                       'use-another-server' |
                       'server-moved' |
                       'shared-subscriptions-not-supported' |
                       'connection-rate-exceeded' |
                       'maximum-connect-time' |
                       'subscription-identifiers-not-supported' |
                       'wildcard-subscriptions-not-supported'.

-type connect() :: #connect{}.
-type connack() :: #connack{}.
-type publish() :: #publish{}.
-type puback() :: #puback{}.
-type pubrel() :: #pubrel{}.
-type pubrec() :: #pubrec{}.
-type pubcomp() :: #pubcomp{}.
-type subscribe() :: #subscribe{}.
-type suback() :: #suback{}.
-type unsubscribe() :: #unsubscribe{}.
-type unsuback() :: #unsuback{}.
-type pingreq() :: #pingreq{}.
-type pingresp() :: #pingresp{}.
-type disconnect() :: #disconnect{}.
-type auth() :: #auth{}.

-type mqtt_packet() :: connect() | connack() | publish() | puback() |
		       pubrel() | pubrec() | pubcomp() | subscribe() |
		       suback() | unsubscribe() | unsuback() | pingreq() |
		       pingresp() | disconnect() | auth().
-type mqtt_version() :: ?MQTT_VERSION_4 | ?MQTT_VERSION_5.
