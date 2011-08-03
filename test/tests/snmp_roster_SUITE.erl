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

-module(snmp_roster_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, roster}].
     
groups() ->
    [{roster, [sequence], [get_roster,
                           add_contact
                           ]}].
     
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
%% Tests
%%--------------------------------------------------------------------

get_roster(Config) ->
    {value, Gets} = get_counter_value(modRosterGets),
    escalus:story(Config, [1, 1], fun(Alice,_Bob) ->
    
        escalus_client:send(Alice, escalus_stanza:roster_get()),
        escalus_client:wait_for_stanza(Alice),
                                          
        assert_counter(Gets + 1, modRosterGets)

        end).

add_contact(Config) ->
    {value, Sets} = get_counter_value(modRosterSets),
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
    
        %% add contact
        escalus_client:send(Alice, 
                            escalus_stanza:roster_add_contact(Bob, 
                                                              ["friends"], 
                                                              "Bobby")),
        Received = escalus_client:wait_for_stanza(Alice),
        escalus_client:send(Alice, escalus_stanza:iq_result(Received)),
        escalus_client:wait_for_stanza(Alice),
        
        assert_counter(Sets + 1, modRosterSets)
        
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
    escalus_client:send(Alice, escalus_stanza:iq_result(Received)),
    escalus_client:wait_for_stanza(Alice).

%% remove_roster(UserSpec) ->
%%     [Username, Server, _Pass] = escalus_config:get_usp(UserSpec),
%%     rpc:call(ejabberd@localhost, mod_roster, remove_user, [Username, Server]).


assert_counter(Value, Counter) ->
    {value, Value} = rpc:call(ct:get_config(ejabberd_node), 
                     mod_snmp, 
                     handle_entry, 
                     [get, Counter]).

get_counter_value(Counter) ->
    rpc:call(ct:get_config(ejabberd_node), 
             mod_snmp, 
             handle_entry, 
             [get, Counter]).
