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

-module(snmp_session_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-define(WAIT_TIME, 500).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, session}].

groups() ->
    [{session, [sequence], [login_one, login_many, auth_failed]}].
     
suite() ->
    [{require, ejabberd_node} | escalus:suite()].

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


login_one(Config) ->
    {value, Logins} = get_counter_value(sessionSuccessfulLogins),
    assert_counter(0, sessionCount),    
    escalus:story(Config, [1], fun(Alice) ->
        
        assert_counter(1, sessionCount),
        assert_counter(Logins + 1, sessionSuccessfulLogins)

        end),
    {value, Logouts} = get_counter_value(sessionLogouts),
    escalus_cleaner:stop(Config),
    timer:sleep(?WAIT_TIME),
    assert_counter(0, sessionCount),
    assert_counter(Logouts + 1, sessionLogouts).

login_many(Config) ->
    {value, Logins} = get_counter_value(sessionSuccessfulLogins),
    assert_counter(0, sessionCount),
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        
        assert_counter(2, sessionCount),
        assert_counter(Logins + 2, sessionSuccessfulLogins)

        end).
    

auth_failed(Config) ->
    {value, AuthFails} = get_counter_value(sessionAuthFails),
    assert_counter(0, sessionCount),
    
    [{_, UserSpec} | _] = escalus_config:get_property(escalus_users, Config),
    UserSpecM = proplists:delete(password, UserSpec) ++ [{password, "mazabe"}],
    
    catch escalus_client:start(Config, UserSpecM, "res1"),
    assert_counter(0, sessionCount),
    assert_counter(AuthFails + 1, sessionAuthFails).
    

messages_story(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        % Alice sends a message to Bob
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, "Hi!")),

        % Bob gets the message
        escalus_assert:is_chat_message("Hi!", escalus_client:wait_for_stanza(Bob))

    end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

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
