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
  %%   {group, subscribe}].

groups() ->
    [{presence, [sequence], [available, available_direct, additions]},
     {roster, [sequence], [get_roster, add_contact, remove_contact]},
     {subscribe, [sequence], [subscription, subscription_decline]}].

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
    remove_roster(UserSpec),
    escalus:end_per_testcase(add_contact, Config);    
end_per_testcase(subscription, Config) ->
    [{_, UserSpec1}, {_, UserSpec2} | _] = 
        escalus_config:get_property(escalus_users, Config),
    remove_roster(UserSpec1),
    remove_roster(UserSpec2),
    escalus:end_per_testcase(subscription, Config);    
end_per_testcase(subscription_decline, Config) ->
    [{_, UserSpec1}, {_, UserSpec2} | _] = 
        escalus_config:get_property(escalus_users, Config),
    remove_roster(UserSpec1),
    remove_roster(UserSpec2),
    escalus:end_per_testcase(subscription_decline, Config);    
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
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
    
        %% add contact
        escalus_client:send(Alice, 
                            escalus_stanza:roster_add_contact(Bob, 
                                                              ["friends"], 
                                                              "Bobby")),
        Received = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_roster_result_set(Received),
        escalus_assert:count_roster_items(1, Received),
        escalus_client:send(Alice, escalus_stanza:iq_result(Received)),
        escalus_assert:is_result(escalus_client:wait_for_stanza(Alice)),
        
        %% check roster
        escalus_client:send(Alice, escalus_stanza:roster_get()),
        Received2 = escalus_client:wait_for_stanza(Alice),
     
        escalus_assert:is_roster_result(Received2),
        escalus_assert:roster_contains(Bob, Received2)
     
        end).

remove_contact(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
    
        %% add contact
        add_sample_contact(Alice, Bob),

        %% check roster
        escalus_client:send(Alice, escalus_stanza:roster_get()),
        escalus_assert:count_roster_items(1, escalus_client:wait_for_stanza(Alice)),

        %% remove contact
        %%[] = escalus_stanza:roster_remove_contact(Bob),
        escalus_client:send(Alice, escalus_stanza:roster_remove_contact(Bob)),
        escalus_assert:is_roster_result_set(escalus_client:wait_for_stanza(Alice)),
        escalus_assert:is_result(escalus_client:wait_for_stanza(Alice)),
                                          
        %% check roster
        escalus_client:send(Alice, escalus_stanza:roster_get()),
        escalus_assert:count_roster_items(0, escalus_client:wait_for_stanza(Alice))
        
    end).
    

subscription(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->

        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus_client:send(Alice, escalus_stanza:presence_direct(Bob, subscribe)),
        PushReq = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_roster_result_set(PushReq),
        escalus_client:send(Alice, escalus_stanza:iq_result(PushReq)),
        
        %% Bob receives subscription reqest
        Received = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_presence_type("subscribe", Received),
        
        %% Bob adds new contact to his roster
        escalus_client:send(Bob, 
                            escalus_stanza:roster_add_contact(Alice, 
                                                              ["enemies"], 
                                                              "Alice")),
        PushReqB = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_roster_result_set(PushReqB),
        escalus_client:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus_assert:is_result(escalus_client:wait_for_stanza(Bob)),
        
        %% Bob sends subscribed presence
        escalus_client:send(Bob, escalus_stanza:presence_direct(Alice, subscribed)),
        
        %% Alice receives subscribed 
        Stanzas = [escalus_client:wait_for_stanza(Alice), 
                   escalus_client:wait_for_stanza(Alice)],

        ok = check_subscription_stanzas(Stanzas, "subscribed"),
        escalus_assert:is_presence_stanza(escalus_client:wait_for_stanza(Alice)),
        
        %% Bob receives roster push
        PushReqB1 = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_roster_result_set(PushReqB1),
        Result = escalus_stanza:iq_result(PushReqB1),
         
        %% Bob sends presence
        escalus_client:send(Bob, escalus_stanza:presence(available)),
        escalus_assert:is_presence_stanza(escalus_client:wait_for_stanza(Alice)),
        
        %% Bob sends presence
        escalus_client:send(Bob, escalus_stanza:presence(unavailable)),
        escalus_assert:is_presence_type("unavailable",
                                        escalus_client:wait_for_stanza(Alice))
                                          
        end).

subscription_decline(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->

        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus_client:send(Alice, escalus_stanza:presence_direct(Bob, subscribe)),
        PushReq = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_roster_result_set(PushReq),
        escalus_client:send(Alice, escalus_stanza:iq_result(PushReq)),
        
        %% Bob receives subscription reqest
        Received = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_presence_type("subscribe", Received),
        
        %% Bob refuses subscription
        escalus_client:send(Bob, escalus_stanza:presence_direct(Alice, unsubscribed)),
        
        %% Alice receives subscribed 
        Stanzas = [escalus_client:wait_for_stanza(Alice), 
                   escalus_client:wait_for_stanza(Alice)],

        ok = check_subscription_stanzas(Stanzas, "unsubscribed")
        
        %% escalus_assert:is_presence_type("unsubscribed", 
        %%                                 escalus_client:wait_for_stanza(Alice)),
        %% PushReq1 = escalus_client:wait_for_stanza(Alice),
        %% escalus_assert:is_roster_result_set(PushReq1),
        %% escalus_client:send(Alice, escalus_stanza:iq_result(PushReq1))
        
    end).
        
        

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

add_sample_contact(Alice, Bob) ->
    escalus_client:send(Alice, 
                        escalus_stanza:roster_add_contact(Bob, 
                                                          ["friends"], 
                                                          "Bobby")),
    Received = escalus_client:wait_for_stanza(Alice),
    escalus_assert:is_roster_result_set(Received),
    escalus_client:send(Alice, escalus_stanza:iq_result(Received)),
    escalus_assert:is_result(escalus_client:wait_for_stanza(Alice)).

check_subscription_stanzas(Stanzas, Type) ->
    case {contains_presence(Stanzas, Type), contains_from(Stanzas)} of
        {true, true} -> ok;
        _ ->
            error
    end.

contains_presence(Stanzas, Type) ->
    lists:foldl(fun(El, Res) ->
                      try escalus_assert:is_presence_type(Type, El) of
                          _ -> true
                      catch
                          _:_ -> Res
                      end
               end, false, Stanzas).

contains_from(Stanzas) ->
    lists:foldl(fun(El, Res) ->
                      try escalus_assert:is_roster_result_set(El) of
                          _ -> true
                      catch
                          _:_ -> Res
                      end
               end, false, Stanzas).

remove_roster(UserSpec) ->
    [Username, Server, _Pass] = escalus_config:get_usp(UserSpec),
    rpc:call(ejabberd@localhost, mod_roster, remove_user, [Username, Server]).
