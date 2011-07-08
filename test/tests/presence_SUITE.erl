%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
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

-module(presence_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, presence}].

groups() ->
    [{presence, [sequence], [available, available_direct, additions]},
     {roster, [sequence], [subscription]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).


init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

available(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,_Bob) ->
        
        escalus_client:send(Alice, escalus_stanza:presence(available)),
        escalus_assert:is_presence_stanza(escalus_client:wait_for_stanza(Alice))
                                          
        end).

available_direct(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->
        
        escalus_client:send(Alice, escalus_stanza:presence_direct(Bob, available)),
        Received = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_presence_stanza(Received),
        escalus_assert:is_stanza_from(Alice, Received)
                                          
        end).

additions(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->
        
        Presence = escalus_stanza:presence_direct(Bob, available),
        PShow = escalus_stanza:presence_show(Presence, 'dnd'),
        PStatus = escalus_stanza:presence_status(PShow, "Short break"),
        PPriority = escalus_stanza:presence_priority(PStatus, 1),
        escalus_client:send(Alice, PPriority),
        
        Received = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_presence_stanza(Received),
        escalus_assert:is_presence_with_show("dnd", Received),
        escalus_assert:is_presence_with_status("Short break", Received),
        escalus_assert:is_presence_with_priority("1", Received)
                                          
        end).

subscription(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->
        
        escalus_client:send(Alice, escalus_stanza:presence_direct(Bob, subscribe)),
        Received = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_presence_type("subscribe", Received),
        escalus_client:send(Bob, escalus_stanza:presence_direct(Alice, subscribed)),
        ReceivedA = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_presence_type("subscribed", ReceivedA),
        
        escalus_client:send(Alice, escalus_stanza:presence(available)),
        escalus_assert:is_presence_stanza(escalus_client:wait_for_stanza(Bob))
                                          
        end).
        

unavailable(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        
        escalus_client:send(Alice, escalus_stanza:presence(unavailable)),
        escalus_assert:is_presence_stanza(escalus_client:wait_for_stanza(Bob))
                                          
        end).



