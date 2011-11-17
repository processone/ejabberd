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
    [{group, register},
     {group, login},
     {group, messages}].

groups() ->
    [{register, [sequence], [register,
                             check_unregistered]},
     {login, [sequence], [log_one,
                          log_one_digest]},
%%                          log_one_basic_plain,
%%                          log_one_basic_digest]},
     {messages, [sequence], [messages_story]}].

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

end_per_group(register, Config) ->
    ok;
end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).


init_per_testcase(log_one_digest, Config) ->
    Conf1 = [ {escalus_auth_method, "DIGEST-MD5"} | Config],
    escalus:init_per_testcase(log_one_digest, Conf1);
init_per_testcase(log_one_basic_digest, Config) ->
    Conf1 = [ {escalus_auth_method, digest} | Config],
    escalus:init_per_testcase(log_one_digest, Conf1);
init_per_testcase(log_one_basic_plain, Config) ->
    Conf1 = [ {escalus_auth_method, password} | Config],
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
    [{_, UserSpec} | _] = escalus_config:get_property(escalus_users, Config),
    [Username, Server, _Pass] = escalus_config:get_usp(UserSpec),
    true = rpc:call('ejabberd@localhost',
             ejabberd_auth,
             is_user_exists,
             [Username, Server]).

check_unregistered(Config) ->
    escalus:delete_users(Config),
    [{_, UserSpec}| _] = escalus_users:get_users(all),
    [Username, Server, _Pass] = escalus_config:get_usp(UserSpec),
    false = rpc:call('ejabberd@localhost',
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

log_one_basic_plain(Config) ->
    log_one(Config).

log_one_basic_digest(Config) ->
    log_one(Config).


messages_story(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        % Alice sends a message to Bob
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, "Hi!")),

        % Bob gets the message
        escalus_assert:is_chat_message("Hi!", escalus_client:wait_for_stanza(Bob))

    end).
