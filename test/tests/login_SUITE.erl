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

-module(login_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, messages},
     {group, unregistered}].

groups() ->
    [{messages, [sequence], [register, log_one, log_one_digest, messages_story]},
     {unregistered, [sequence], [check_unregistered]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(unregistered, Config) ->
    Users = escalus_users:get_users(all),
    [{escalus_users, Users} | Config];
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(unregistered, Config) ->
    ok;
end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).


init_per_testcase(log_one_digest, Config) ->
    Conf1 = [ {escalus_auth_method, "DIGEST-MD5"} | Config],
    escalus:init_per_testcase(log_one_digest, Conf1);
init_per_testcase(check_unregistered, Config) ->
    Config;
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(check_unregistered, Config) ->
    Config;
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

register(Config) ->
    %%user should be registered in an init function
    [{_, UserSpec}| _] = escalus_config:get_property(escalus_users, Config),
    [Username, Server, _Pass] = escalus_config:get_usp(UserSpec),
    true = rpc:call('ejabberd@localhost', 
             ejabberd_auth, 
             is_user_exists, 
             [Username, Server]).

log_one(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        
        escalus_client:send(Alice, escalus_stanza:chat_to(Alice, "Hi!")),
        escalus_assert:is_chat_message(["Hi!"], escalus_client:wait_for_stanza((Alice)))
        
        end).

log_one_digest(Config) ->
    log_one(Config).

check_unregistered(Config) ->
    %%user should be unregistered by previous end group function 
    [{_, UserSpec} | _] = escalus_config:get_property(escalus_users, Config),
    [Username, Server, _Pass] = escalus_config:get_usp(UserSpec),
    false = rpc:call('ejabberd@localhost', 
                     ejabberd_auth, 
                     is_user_exists, 
                     [Username, Server]).


messages_story(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        % Alice sends a message to Bob
        escalus_client:send_wait(Alice, escalus_stanza:chat_to(Bob, "Hi!")),

        % Bob gets the message
        escalus_assert:is_chat_message("Hi!", escalus_client:only_stanza(Bob))

    end).
