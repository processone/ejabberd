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
    [{group, presence},
     {group, roster}].

groups() ->
    [{presence, [sequence], [available, available_direct, additions]},
     {roster, [sequence], [get_roster, add_contact]}].

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

end_per_testcase(add_contact, Config) ->
    [{_, UserSpec} | _] = escalus_config:get_property(escalus_users, Config),
    [Username, Server, _Pass] = escalus_config:get_usp(UserSpec),
    rpc:call(ejabberd@localhost, mod_roster, remove_user, [Username, Server]),
    escalus:end_per_testcase(add_contact, Config);    
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

get_roster(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,_Bob) ->
    
        escalus_client:send(Alice, escalus_stanza:roster_get()),
        escalus_assert:is_roster_result(escalus_client:wait_for_stanza(Alice))
     
        end).

add_contact(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
    
        escalus_client:send(Alice, 
                            escalus_stanza:roster_add_contact(Bob, 
                                                              ["friends"], 
                                                              "Bobby")),
        Received = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_roster_result_set(Received),
        escalus_assert:count_roster_items(1, Received),
        escalus_client:send(Alice, escalus_stanza:iq_result(Received)),
        escalus_assert:is_roster_result_short(escalus_client:wait_for_stanza(Alice)),
        
        escalus_client:send(Alice, escalus_stanza:roster_get()),
        Received2 = escalus_client:wait_for_stanza(Alice),
     
        escalus_assert:is_roster_result(Received2),
        escalus_assert:roster_contains(Bob, Received2)
     
        end).



subscribtion(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->
        
        escalus_client:send(Alice, escalus_stanza:presence_direct(Bob, subscribe)),
        Received = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_presence_type("subscribe", Received),
        PushReq = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_roster_result_set(PushReq),
        escalus_client:send(Alice, escalus_stanza:iq_result(PushReq)),
        
        escalus_client:send(Bob, 
                            escalus_stanza:roster_add_contact(Alice, 
                                                              ["enemies"], 
                                                              "Alice")),
        PushReqB = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_roster_result_set(PushReqB),
        escalus_client:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus_assert:is_roster_result_short(escalus_client:wait_for_stanza(Bob)),
        escalus_client:send(Bob, escalus_stanza:presence_direct(Alice, subscribe)),
        escalus_client:send(Bob, escalus_stanza:presence_direct(Alice, subscribed)),
        
        PushReqB1 = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_roster_result_set(PushReqB1),
        Result = escalus_stanza:iq_result(PushReqB1),
        escalus_client:send(Bob, Result),
        
        %% TODO: make this test pass
        escalus_assert:is_presence_type("subscribe", 
                                        escalus_client:wait_for_stanza(Alice)),

        escalus_client:send(Alice, escalus_stanza:roster_get()),
        escalus_client:send(Bob, escalus_stanza:roster_get()),
        escalus_assert:is_roster_result(escalus_client:wait_for_stanza(Bob)),
        escalus_assert:is_roster_result(escalus_client:wait_for_stanza(Alice)),
        
        escalus_client:send(Bob, escalus_stanza:presence(available)),
        escalus_assert:is_presence_stanza(escalus_client:wait_for_stanza(Alice))
                                          
        end).
        

unavailable(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        
        escalus_client:send(Alice, escalus_stanza:presence(unavailable)),
        escalus_assert:is_presence_stanza(escalus_client:wait_for_stanza(Bob))
                                          
        end).





