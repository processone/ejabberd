%%==============================================================================
%% Copyright 2011 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(last_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, last}].

groups() ->
     [{last, [sequence], [last_online_user,
                          last_offline_user,
                          last_server]}].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:make_everyone_friends(escalus:create_users(Config)).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Last tests
%%--------------------------------------------------------------------
last_online_user(Config) ->
    escalus:story(Config, [1, 1],
                  fun(Alice, Bob) ->
                          %% Alice asks about Bob's last activity
                          escalus_client:send(Alice, escalus_stanza:last_activity(Bob)),

                          %% server replies on Bob's behalf
                          Stanza = escalus_client:wait_for_stanza(Alice),
                          escalus:assert(is_last_result, Stanza),
                          0 = get_last_activity(Stanza)
                  end).

last_offline_user(Config) ->
    escalus:story(Config, [1],
                  fun(Alice) ->
                          %% Bob logs in
                          {ok, Bob} = escalus_client:start_for(Config, bob, <<"bob">>),

                          %% Bob logs out with a status
                          escalus_client:send(Bob, exmpp_presence:presence(unavailable, <<"I am a banana!">>)),
                          escalus_client:stop(Bob),
                          timer:sleep(1000),

                          %% Alice asks for Bob's last availability
                          escalus_client:send(Alice, escalus_stanza:last_activity(Bob)),

                          %% Alice receives Bob's status and last online time > 0
                          Stanza = escalus_client:wait_for_stanza(Alice),
                          escalus:assert(is_last_result, Stanza),
                          true = (get_last_activity(Stanza) > 0),
                          <<"I am a banana!">> = get_last_status(Stanza)
                  end).
last_server(Config) ->
    escalus:story(Config, [1],
                  fun(Alice) ->
                          %% Alice asks for server's uptime
                          escalus_client:send(Alice, escalus_stanza:last_activity(escalus_users:get_server(alice))),

                          %% Server replies with the uptime > 0
                          Stanza = escalus_client:wait_for_stanza(Alice),
                          escalus:assert(is_last_result, Stanza),
                          true = (get_last_activity(Stanza) > 0)
                  end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------
get_last_activity(Stanza) ->
    Query = exmpp_xml:get_element(Stanza, 'query'),
    list_to_integer(exmpp_xml:get_attribute_as_list(Query, <<"seconds">>, "undefined")).

get_last_status(Stanza) ->
    Query = exmpp_xml:get_element(Stanza, 'query'),
    exmpp_xml:get_cdata(Query).
