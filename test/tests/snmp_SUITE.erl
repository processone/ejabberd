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

-module(snmp_SUITE).
-compile(export_all).

-include_lib("escalus/deps/exmpp/include/exmpp.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-define(RPC(Mod, Fun, Args), escalus_ejabberd:rpc(Mod, Fun, Args)).
-define(RPC_LOOKUP(Table, Counter), escalus_ejabberd:rpc(ets, lookup, [Table, Counter])).

-import(privacy_helper, [config_list/2,
                         set_and_activate/2,
                         set_list/2,
                         activate_list/2,
                         set_default_list/2,
                         privacy_list/2,
                         privacy_list_item/1,
                         is_privacy_list_push/1,
                         is_presence_error/1,
                         verify_result/1,
                         verify_push/1,
                         verify_presence_error/1]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, core},
     {group, general},
     {group, c2s},
     {group, mod_privacy},
     {group, mod_roster},
     {group, mod_register}].

groups() ->
    [{core, [sequence], [generalUptime,
                         generalNodeName]},
     {mod_privacy, [sequence], [modPrivacyGets,
                                modPrivacySets,
                                modPrivacySetsActive,
                                modPrivacySetsDefault,
                                modPrivacyStanzaBlocked%,
                                %modPrivacyStanzaAll  % doesn't work yet
                               ]}].

suite() ->
    [{require, privacy_lists} | escalus:suite()].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    PrivacyLists = lists:map(
        fun(Name) ->
            { Name,
              privacy_list(Name,
                  [privacy_list_item(Item) || Item <- ct:get_config(Name)]) }
        end,
        ct:get_config(privacy_lists)),
    escalus:init_per_suite(PrivacyLists ++ Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus_ejabberd:rpc(ejabberd_snmp_core, reset_counters, []),
    escalus_ejabberd:rpc(mnesia, clear_table, [privacy]),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

modPrivacyGets(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Table = stats_mod_privacy,
        Counter = modPrivacyGets,

        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        escalus_client:send(Alice, escalus_stanza:privacy_get_all(Alice)),
        escalus_client:wait_for_stanzas(Alice, 1),
        [{Counter, 1}] = ?RPC_LOOKUP(Table, Counter)

        end).

modPrivacySets() -> [{require, alice_deny_bob}].

modPrivacySets(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        PrivacyList = config_list(alice_deny_bob, Config),

        Table = stats_mod_privacy,
        Counter = modPrivacySets,

        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        set_list(Alice, PrivacyList),
        [{Counter, 1}] = ?RPC_LOOKUP(Table, Counter)

        end).

modPrivacySetsActive() -> [{require, alice_deny_bob}].

modPrivacySetsActive(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        PrivacyList = config_list(alice_deny_bob, Config),

        Table = stats_mod_privacy,
        Counter = modPrivacySetsActive,

        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        set_list(Alice, PrivacyList),
        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        activate_list(Alice, PrivacyList),
        [{Counter, 1}] = ?RPC_LOOKUP(Table, Counter)

        end).

modPrivacySetsDefault() -> [{require, alice_deny_bob}].

modPrivacySetsDefault(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        PrivacyList = config_list(alice_deny_bob, Config),

        Table = stats_mod_privacy,
        Counter = modPrivacySetsDefault,

        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        set_list(Alice, PrivacyList),
        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        set_default_list(Alice, PrivacyList),
        [{Counter, 1}] = ?RPC_LOOKUP(Table, Counter)

        end).

modPrivacyStanzaBlocked() -> [{require, alice_deny_bob_message}].

modPrivacyStanzaBlocked(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        PrivacyList = config_list(alice_deny_bob_message, Config),

        Table = stats_mod_privacy,
        Counter = modPrivacyStanzaBlocked,

        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        chit_chat(Alice, Bob),
        %% No blocking yet
        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        set_and_activate(Alice, PrivacyList),
        chit_chat(Alice, Bob),
        %% One message blocked
        [{Counter, 1}] = ?RPC_LOOKUP(Table, Counter)

        end).

modPrivacyStanzaAll() -> [{require, alice_deny_bob_message}].

modPrivacyStanzaAll(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        PrivacyList = config_list(alice_deny_bob_message, Config),

        Table = stats_mod_privacy,
        Counter = modPrivacyStanzaAll,

        escalus_ejabberd:rpc(ejabberd_snmp_core, reset_counters, []),

        [{Counter, 0}] = ?RPC_LOOKUP(Table, Counter),
        chit_chat(Alice, Bob),
        [{Counter, 1}] = ?RPC_LOOKUP(Table, Counter),
        set_and_activate(Alice, PrivacyList),
        chit_chat(Alice, Bob),
        [{Counter, 1}] = ?RPC_LOOKUP(Table, Counter)

        end).

%% TODO:
%% - (?) modPrivacyStanzaAll
%% - modPrivacyStanzaPush
%% - modPrivacyStanzaListLength

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

%% Bob sends message to Alice.
%% Alice either receives or gracefully handles timeout.
chit_chat(Alice, Bob) ->
    Request = escalus_stanza:chat_to(Alice, "Hi! What's your name?"),
    escalus_client:send(Bob, Request),
    Responses = escalus_client:wait_for_stanzas(Alice, 1),
    case Responses of
    [Response] ->
        escalus_assert:is_chat_message(["Hi! What's your name?"], Response);
    [] ->
        ok
    end.
