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

-module(privacy_SUITE).
-compile(export_all).

-include_lib("escalus/deps/exmpp/include/exmpp.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-import(privacy_helper, [config_list/2,
                         set_and_activate/2,
                         set_list/2,
                         activate_list/2,
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
    [{group, management},
     {group, blocking}].

groups() ->
    [{management, [sequence], [get_all_lists,
                               get_existing_list,
                               get_many_lists,
                               get_nonexistent_list,
                               set_list,
                               activate,
                               activate_nonexistent,
                               deactivate,
                               default,
                               %default_conflict,  % fails, as of bug #7073
                               default_nonexistent,
                               no_default,
                               remove_list,
                               get_all_lists_with_active
                               %get_all_lists_with_default
                                 % not implemented (see testcase)
                              ]},
     {blocking, [sequence], [block_jid_message,
                             block_group_message,
                             block_subscription_message,
                             block_all_message,

                             block_jid_presence_in,

                             block_jid_presence_out,

                             block_jid_iq,

                             block_jid_all,

                             block_jid_message_but_not_presence]}].

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

init_per_group(blocking, Config) ->
    Timeout = {timeout, ct:get_config(timeout)},
    escalus:create_users([Timeout | Config]);
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

%% TODO:
%% x get all privacy lists
%% x get single privacy list
%%   x that exists
%%   x that doesn't exist (ensure server returns item-not-found)
%%   x request more than one at a time (ensure server returns bad-request)
%% x set new/edit privacy list (ensure server pushes notifications
%%   to all resources)
%% - remove existing list
%%   x remove existing list (ensure server push)
%%   - remove, but check conflict case
%% x manage active list(s)
%%   x activate
%%   x activate nonexistent (ensure item-not-found)
%%   x deactivate by sending empty <active />
%% - manage default list
%%   x set default
%%   - set default, but check the conflict case, i.e.:
%%     "Client attempts to change the default list but that list is in use
%%     by another resource",
%%     !!! ejabberd doesn't support this, bug filed (#7073)
%%   x set nonexistent default list
%%   x use domain's routing, i.e. no default list -> send empty <default />
%%   - set no default list, but check conflict case,
%%     when a resource currently uses the default list
%%
%% TODO later:
%% - big picture:
%%   - blocking can be done on jids, roster groups,
%%     subscription type or globally
%%   - a blocking rule may block one or more of {message, presence-in,
%%     presence-out, iqs} by specifying these as children to the list item
%%     or block all of them, when the item has no children
%% - blocking: messages, presence (in/out), iqs, all

get_all_lists(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        escalus_client:send(Alice, escalus_stanza:privacy_get_all(Alice)),
        escalus_assert:is_privacy_query_result(
            escalus_client:wait_for_stanza(Alice) )

        end).

get_all_lists_with_active() -> [{require, alice_deny_bob}].

get_all_lists_with_active(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->

        PrivacyList = {ListName,_} = config_list(alice_deny_bob, Config),
        set_and_activate(Alice, PrivacyList),

        escalus_client:send(Alice, escalus_stanza:privacy_get_all(Alice)),
        Response = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_privacy_query_result_with_active(
          ListName, Response)
        end).

get_all_lists_with_default() -> [{require, alice_deny_bob},
                                 {require, alice_allow_bob}].

%% Black box testing showed that this feature is not implemented,
%% i.e. the <default /> element is never returned by ejabberd.
%% However, I'm not 100% sure, as I didn't look inside the source.
get_all_lists_with_default(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->

        PrivacyList1 = config_list(alice_deny_bob, Config),
        PrivacyList2 = config_list(alice_allow_bob, Config),
        set_list(Alice, PrivacyList1),
        set_and_activate(Alice, PrivacyList2),

        escalus_client:send(Alice, escalus_stanza:privacy_get_all(Alice)),
        Response = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_privacy_query_result_with_default(Response)
        %, escalus_utils:log_stanzas("Response", [Response])

        end).

get_nonexistent_list(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        escalus_client:send(Alice,
            escalus_stanza:privacy_get_one(Alice, "public")),
        escalus_assert:is_privacy_list_nonexistent_error(
            escalus_client:wait_for_stanza(Alice))

        end).

get_many_lists(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Request = escalus_stanza:privacy_get_many(Alice, ["public", "private"]),
        escalus_client:send(Alice, Request),
        Response = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_error(Response, <<"modify">>, 'bad-request')
        %, escalus_utils:log_stanzas("Request", [Request])
        %, escalus_utils:log_stanzas("Response", [Response])

        end).

get_existing_list() -> [{require, alice_deny_bob}].

get_existing_list(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->

        PrivacyList = {Name,_} = config_list(alice_deny_bob, Config),
        set_list(Alice, PrivacyList),

        escalus_client:send(Alice,
            escalus_stanza:privacy_get_one(Alice, Name)),

        Response = escalus_client:wait_for_stanza(Alice),
        true = exmpp_xml:has_element(Response, 'query'),
        true = exmpp_xml:has_element(
            exmpp_xml:get_element(Response, 'query'), 'list'),
        List = exmpp_xml:get_element(
            exmpp_xml:get_element(Response, 'query'), 'list'),

        true = atom_to_list(Name)
            =:= exmpp_xml:get_attribute_as_list(List, <<"name">>, 2)

        %escalus_utils:log_stanzas("Created list", [AliceDenyBob]),
        %escalus_utils:log_stanzas("Actual response", [Response])

        end).

activate() -> [{require, alice_deny_bob}].

activate(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->

        PrivacyList = config_list(alice_deny_bob, Config),
        set_list(Alice, PrivacyList),

        Request = escalus_stanza:privacy_activate(Alice, alice_deny_bob),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        true = exmpp_iq:is_result(Response)

        %escalus_utils:log_stanzas("Request", [Request]),
        %escalus_utils:log_stanzas("Response", [Response])

        end).

activate_nonexistent(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Request = escalus_stanza:privacy_activate(Alice, some_list),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        true = exmpp_iq:is_error(Response),
        <<"cancel">> = exmpp_stanza:get_error_type(Response),
        'item-not-found' = exmpp_stanza:get_condition(Response)

        %escalus_utils:log_stanzas("Request", [Request]),
        %escalus_utils:log_stanzas("Response", [Response])

        end).

deactivate(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Request = escalus_stanza:privacy_deactivate(Alice),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        true = exmpp_iq:is_result(Response)

        %escalus_utils:log_stanzas("Request", [Request]),
        %escalus_utils:log_stanzas("Response", [Response])

        end).

default() -> [{require, alice_deny_bob}].

default(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->

        PrivacyList = config_list(alice_deny_bob, Config),
        set_list(Alice, PrivacyList),

        Request = escalus_stanza:privacy_default(Alice, alice_deny_bob),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        true = exmpp_iq:is_result(Response)

        %escalus_utils:log_stanzas("Request", [Request]),
        %escalus_utils:log_stanzas("Response", [Response])

        end).

default_conflict() -> [{require, alice_deny_bob},
                       {require, alice_allow_bob}].

default_conflict(Config) ->
    escalus:story(Config, [2, 1], fun(Alice, Alice2, _Bob) ->

        %% testcase setup
        %% setup list on server
        AliceDenyBob = ?config(alice_deny_bob, Config),
        AliceAllowBob = ?config(alice_allow_bob, Config),
        escalus_client:send(Alice,
            escalus_stanza:privacy_set_one(Alice, AliceDenyBob)),
        escalus_client:send(Alice,
            escalus_stanza:privacy_set_one(Alice, AliceAllowBob)),
        %% skip responses
        _AliceResponses = escalus_client:wait_for_stanzas(Alice, 4),
        %% make a default list for Alice2
        R1 = escalus_stanza:privacy_default(Alice2, alice_deny_bob),
        escalus_client:send(Alice2, R1),
        R2s = escalus_client:wait_for_stanzas(Alice2, 3),
        escalus_utils:log_stanzas("Alice2", [R1] ++ R2s),
        %% setup done

        Request = escalus_stanza:privacy_default(Alice, alice_allow_bob),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        %% TODO: should fail on this (result) and receive error
        %%       this is a bug and was filed to the esl redmine as Bug #7073
        %true = exmpp_iq:is_result(Response),
        %% but this should pass just fine
        true = exmpp_iq:is_error(Response),
        <<"cancel">> = exmpp_stanza:get_error_type(Response),
        'conflict' = exmpp_stanza:get_condition(Response),

        escalus_utils:log_stanzas("Request", [Request]),
        escalus_utils:log_stanzas("Response", [Response])

        end).

default_nonexistent(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Request = escalus_stanza:privacy_default(Alice, some_list),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_error(Response, <<"cancel">>, 'item-not-found')

        %escalus_utils:log_stanzas("Request", [Request]),
        %escalus_utils:log_stanzas("Response", [Response])

        end).

no_default(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Request = escalus_stanza:privacy_no_default(Alice),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        true = exmpp_iq:is_result(Response)

        %escalus_utils:log_stanzas("Request", [Request]),
        %escalus_utils:log_stanzas("Response", [Response])

        end).

set_list() -> [{require, alice_deny_bob}].

set_list(Config) ->
    escalus:story(Config, [3, 1], fun(Alice, Alice2, Alice3, _Bob) ->

        AliceDenyBob = ?config(alice_deny_bob, Config),
        Request = escalus_stanza:privacy_set_one(Alice, AliceDenyBob),
        %escalus_utils:log_stanzas("Alice sent", [Request]),
        escalus_client:send(Alice, Request),

        %% Verify that original Alice gets iq result and notification.
        %% It's a bit quirky as these come in undefined order
        %% (actually, they always came with 'push' first and then 'result',
        %% but I suppose it's not mandatory).
        AliceResponses = escalus_client:wait_for_stanzas(Alice, 2),
        %escalus_utils:log_stanzas("Alice got", AliceResponses),
        true = lists:any(fun privacy_helper:verify_result/1, AliceResponses)
            and lists:any(fun privacy_helper:verify_push/1, AliceResponses),

        %% Verify that other resources also get the push.
        AliceResourceResponses = [
            escalus_client:wait_for_stanza(Alice2),
            escalus_client:wait_for_stanza(Alice3)
        ],
        %escalus_utils:log_stanzas("Alice resources got", AliceResourceResponses),
        lists:foreach(fun privacy_helper:is_privacy_list_push/1,
            AliceResourceResponses)

        %% All in all, the spec requires the resources to reply
        %% (as to every iq), but it's omitted here.

        end).

remove_list() -> [{require, alice_deny_bob}].

remove_list(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->

        AliceDenyBob = ?config(alice_deny_bob, Config),
        escalus_client:send(Alice,
            escalus_stanza:privacy_set_one(Alice, AliceDenyBob)),

        %% These are the pushed notification and iq result.
        escalus_client:wait_for_stanzas(Alice, 2),

        %% Request list deletion by sending an empty list.
        RemoveRequest = escalus_stanza:privacy_set_one(Alice, privacy_list("someList", [])),
        %escalus_utils:log_stanzas("Alice deletes", [RemoveRequest]),
        escalus_client:send(Alice, RemoveRequest),

        %% These too are the pushed notification and iq result.
        _SkipTheseToo = escalus_client:wait_for_stanzas(Alice, 2),
        %SkipTheseToo = escalus_client:wait_for_stanzas(Alice, 2),
        %escalus_utils:log_stanzas("Alice got", SkipTheseToo),

        escalus_client:send(Alice,
            escalus_stanza:privacy_get_one(Alice, "someList")),

        %% Finally ensure that the list doesn't exist anymore.
        escalus_assert:is_privacy_list_nonexistent_error(
            escalus_client:wait_for_stanza(Alice))

        end).

block_jid_message() -> [{require, timeout},
                        {require, alice_deny_bob_message}].

block_jid_message(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        Timeout = ?config(timeout, Config),
        PrivacyList = config_list(alice_deny_bob_message, Config),

        %% Alice should receive message
        escalus_client:send(Bob,
            escalus_stanza:chat_to(Alice, "Hi! What's your name?")),
        escalus_assert:is_chat_message(["Hi! What's your name?"],
            escalus_client:wait_for_stanza(Alice)),

        %% set the list on server and make it active
        set_and_activate(Alice, PrivacyList),

        %% Alice should NOT receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, "Hi, Alice!")),
        timer:sleep(Timeout),
        %Responses = escalus_client:wait_for_stanzas(Alice, 2),
        %escalus_utils:log_stanzas("Responses", Responses),
        %[] = Responses
        escalus_assert:has_no_stanzas(Alice)

        end).

block_group_message() -> [{require, timeout},
                          {require, alice_deny_group_message}].

block_group_message(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        Timeout = ?config(timeout, Config),
        PrivacyList = config_list(alice_deny_group_message, Config),

        %% Alice should receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, "Hi!")),
        escalus_assert:is_chat_message(["Hi!"],
            escalus_client:wait_for_stanza(Alice)),

        %% add Bob to Alices' group 'ignored'
        add_sample_contact(Alice, Bob, [ignored], "Ugly Bastard"),

        %% set the list on server and make it active
        set_and_activate(Alice, PrivacyList),

        %% Alice should NOT receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, "Hi!")),
        timer:sleep(Timeout),
        escalus_assert:has_no_stanzas(Alice)

        end).

block_subscription_message() -> [{require, timeout},
                                 {require, alice_deny_unsubscribed_message}].

block_subscription_message(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        Timeout = ?config(timeout, Config),
        PrivacyList = config_list(alice_deny_unsubscribed_message, Config),

        %% Alice should receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, "Hi!")),
        escalus_assert:is_chat_message(["Hi!"],
            escalus_client:wait_for_stanza(Alice)),

        %% Alice sends unsubscribe
        escalus_client:send(Alice,
            escalus_stanza:presence_direct(Bob, unsubscribe)),

        %% set the list on server and make it active
        set_and_activate(Alice, PrivacyList),

        %% Alice should NOT receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, "Hi!")),
        timer:sleep(Timeout),
        escalus_assert:has_no_stanzas(Alice)

        end).

block_all_message() -> [{require, timeout},
                        {require, alice_deny_all_message}].

block_all_message(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        Timeout = ?config(timeout, Config),
        PrivacyList = config_list(alice_deny_all_message, Config),

        %% Alice should receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, "Hi!")),
        escalus_assert:is_chat_message(["Hi!"],
            escalus_client:wait_for_stanza(Alice)),

        %% set the list on server and make it active
        set_and_activate(Alice, PrivacyList),

        %% Alice should NOT receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, "Hi!")),
        timer:sleep(Timeout),
        escalus_assert:has_no_stanzas(Alice)

        end).

block_jid_presence_in() -> [{require, timeout},
                            {require, alice_deny_bob_presence_in}].

block_jid_presence_in(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        Timeout = ?config(timeout, Config),
        PrivacyList = config_list(alice_deny_bob_presence_in, Config),

        %% Alice should receive presence in
        escalus_client:send(Bob,
            escalus_stanza:presence_direct(Alice, available)),
        Received = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_presence_stanza(Received),
        escalus_assert:is_stanza_from(Bob, Received),

        set_and_activate(Alice, PrivacyList),

        %% Alice should NOT receive presence in
        escalus_client:send(Bob,
            escalus_stanza:presence_direct(Alice, available)),
        timer:sleep(Timeout),
        escalus_assert:has_no_stanzas(Alice)

        end).

block_jid_presence_out() -> [{require, timeout},
                             {require, alice_deny_bob_presence_out}].

block_jid_presence_out(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        Timeout = ?config(timeout, Config),
        PrivacyList = config_list(alice_deny_bob_presence_out, Config),

        %% Bob should receive presence in
        escalus_client:send(Alice,
            escalus_stanza:presence_direct(Bob, available)),
        Received = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_presence_stanza(Received),
        escalus_assert:is_stanza_from(Alice, Received),

        set_and_activate(Alice, PrivacyList),

        %% Bob should NOT receive presence in
        escalus_client:send(Alice,
            escalus_stanza:presence_direct(Bob, available)),
        timer:sleep(Timeout),
        escalus_assert:has_no_stanzas(Bob)

        end).

block_jid_iq() -> [{require, timeout},
                   {require, alice_deny_localhost_iq},
                   {require, alice_deny_bob}].

block_jid_iq(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->

        Timeout = ?config(timeout, Config),
        PrivacyList = {Name, _} =
            config_list(alice_deny_localhost_iq, Config),
        ToyList = ?config(alice_deny_bob, Config),

        set_list(Alice, PrivacyList),
        %% activate it
        escalus_client:send(Alice,
            escalus_stanza:privacy_activate(Alice, Name)),
        %% From now on no iq replies should reach Alice.
        %% That's also the reason why we couldn't use
        %% the set_and_activate helper - it waits for all replies.

        %% Just set the toy list and ensure that only
        %% the notification push comes back.
        escalus_client:send(Alice,
            escalus_stanza:privacy_set_one(Alice, ToyList)),
        Response = escalus_client:wait_for_stanza(Alice),
        true = verify_push(Response),
        timer:sleep(Timeout),
        escalus_assert:has_no_stanzas(Alice)

        end).

block_jid_all() -> [{require, timeout},
                    {require, alice_deny_jid_all},
                    {require, alice_deny_bob}].

block_jid_all(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        Timeout = ?config(timeout, Config),
        PrivacyList = {Name, _} =
            config_list(alice_deny_jid_all, Config),
        ToyList = ?config(alice_deny_bob, Config),

        set_list(Alice, PrivacyList),

        %% Alice blocks Bob
        escalus_client:send(Alice,
            escalus_stanza:privacy_activate(Alice, Name)),
        %% IQ response is blocked;
        %% do magic wait for the request to take effect
        timer:sleep(200),

        %% From now on nothing whatsoever sent by Bob should reach Alice.

        %% Alice should NOT receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, "Hi!")),

        %% Alice should NOT receive presence-in from Bob
        escalus_client:send(Bob,
            escalus_stanza:presence_direct(Alice, available)),

        %% Bob should NOT receive presence-in from Alice
        escalus_client:send(Alice,
            escalus_stanza:presence_direct(Bob, available)),

        %% Just set the toy list and ensure that only
        %% the notification push comes back.
        escalus_client:send(Alice,
            escalus_stanza:privacy_set_one(Alice, ToyList)),

        %Responses = escalus_client:wait_for_stanzas(Alice, 1),
        %escalus_utils:log_stanzas("Responses", Responses),
        %true = is_presence_error(hd(Responses))
        %true = verify_push(),

        %% verify
        timer:sleep(Timeout),
        %% ...that nothing reached Bob
        escalus_assert:has_no_stanzas(Bob),
        %% ...that Alice got exactly two responses
        _Responses = [R1,R2] = escalus_client:wait_for_stanzas(Alice, 2),
        %escalus_utils:log_stanzas("Responses", _Responses),
        %% one of which is a push
        true = verify_push(R1) or verify_push(R2),
        %% and the other a presence error
        true = verify_presence_error(R1) or verify_presence_error(R2),
        %% and Alice didn't get anything else
        escalus_assert:has_no_stanzas(Alice)

        end).

block_jid_message_but_not_presence() -> [{require, timeout},
                                         {require, alice_deny_bob_message}].

block_jid_message_but_not_presence(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        Timeout = ?config(timeout, Config),
        PrivacyList = config_list(alice_deny_bob_message, Config),

        %% Alice should receive message
        escalus_client:send(Bob,
            escalus_stanza:chat_to(Alice, "Hi! What's your name?")),
        escalus_assert:is_chat_message(["Hi! What's your name?"],
            escalus_client:wait_for_stanza(Alice)),

        %% set the list on server and make it active
        set_and_activate(Alice, PrivacyList),

        %% Alice should NOT receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, "Hi, Alice!")),
        timer:sleep(Timeout),
        escalus_assert:has_no_stanzas(Alice),

        %% ...but should receive presence in
        escalus_client:send(Bob,
            escalus_stanza:presence_direct(Alice, available)),
        Received = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_presence_stanza(Received),
        escalus_assert:is_stanza_from(Bob, Received)

        end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

add_sample_contact(Who, Whom, Groups, Nick) ->
    escalus_client:send(Who,
                        escalus_stanza:roster_add_contact(Whom,
                                                          Groups,
                                                          Nick)),
    Received = escalus_client:wait_for_stanza(Who),
    escalus_assert:is_roster_set(Received),
    escalus_client:send(Who, escalus_stanza:iq_result(Received)),
    escalus_assert:is_result(escalus_client:wait_for_stanza(Who)).
