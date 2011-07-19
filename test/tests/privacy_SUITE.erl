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

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, privacy}].

groups() ->
    [{privacy, [sequence], [get_all_lists,
                            get_existing_list,
                            get_many_lists,
                            get_nonexistent_list,
                            set_list,
                            remove_list]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

        %Items = lists:map(fun privacy_list_item/1,
        %[{"jid", Bob#client.jid, "deny", "1"}]),
        %privacy_list("someList", Items)

init_per_group(privacy, Config) ->
    PrivacyLists = lists:map(
        fun({Name,Items}) ->
            { Name,
              privacy_list(Name,
                lists:map(fun privacy_list_item/1, Items)) }
        end,
        ct:get_config(privacy_lists)),
    escalus:create_users([{privacy_lists, PrivacyLists} | Config]);
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName = set_list, Config) ->
    PrivacyLists = ?config(privacy_lists, Config),
    PrivacyList = lists:keyfind(alice_deny_bob, 1, PrivacyLists),
    escalus:init_per_testcase(CaseName, [ PrivacyList | Config]);
init_per_testcase(CaseName = remove_list, Config) ->
    PrivacyLists = ?config(privacy_lists, Config),
    PrivacyList = lists:keyfind(alice_deny_bob, 1, PrivacyLists),
    escalus:init_per_testcase(CaseName, [ PrivacyList | Config]);
init_per_testcase(CaseName = get_existing_list, Config) ->
    PrivacyLists = ?config(privacy_lists, Config),
    PrivacyList = lists:keyfind(alice_deny_bob, 1, PrivacyLists),
    escalus:init_per_testcase(CaseName, [ PrivacyList | Config]);
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
%% x remove existing list (ensure server push)
%% - manage active list(s)
%%   - activate
%%   - activate nonexistent (ensure item-not-found)
%%   - deactivate by sending empty <active />
%% - manage default list
%%   - set default,
%%     check the conflict case, i.e.:
%%     "Client attempts to change the default list but that list is in use
%%     by another resource",
%%     check the nonexistent list case
%%   - use domain's routing, i.e. no default list -> send empty <default />,
%%     check conflict case, when a resource currently uses the default list
%%
%% TODO later:
%% - blocking: messages, presence (in/out), iqs, all

get_all_lists(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        escalus_client:send(Alice, escalus_stanza:privacy_get_all(Alice)),
        escalus_assert:is_privacy_query_result(
            escalus_client:wait_for_stanza(Alice) )

        end).

get_nonexistent_list(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        escalus_client:send(Alice,
            escalus_stanza:privacy_get_one(Alice, "public")),
        escalus_assert:is_nonexistent_list_error(
            escalus_client:wait_for_stanza(Alice))

        end).

get_many_lists(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Request = escalus_stanza:privacy_get_many(Alice, ["public", "private"]),
        escalus_client:send(Alice, Request),
        Response = escalus_client:wait_for_stanza(Alice),

        true = exmpp_iq:is_error(Response),
        <<"modify">> = exmpp_stanza:get_error_type(Response),
        'bad-request' = exmpp_stanza:get_condition(Response)

        %escalus_utils:log_stanzas("Request", [Request]),
        %escalus_utils:log_stanzas("Response", [Response])

        end).

get_existing_list() -> [{require, privacy_lists}].

get_existing_list(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->

        AliceDenyBob = ?config(alice_deny_bob, Config),
        escalus_client:send(Alice,
            escalus_stanza:privacy_set_one(Alice, AliceDenyBob)),
        %% skip responses
        _AliceResponses = escalus_client:wait_for_stanzas(Alice, 2),

        escalus_client:send(Alice,
            escalus_stanza:privacy_get_one(Alice, alice_deny_bob)),

        Response = escalus_client:wait_for_stanza(Alice),
        true = exmpp_xml:has_element(Response, 'query'),
        true = exmpp_xml:has_element(
            exmpp_xml:get_element(Response, 'query'), 'list'),
        List = exmpp_xml:get_element(
            exmpp_xml:get_element(Response, 'query'), 'list'),

        true = exmpp_xml:get_attribute_as_list(AliceDenyBob, <<"name">>, 1)
            =:= exmpp_xml:get_attribute_as_list(List, <<"name">>, 2)

        %escalus_utils:log_stanzas("Created list", [AliceDenyBob]),
        %escalus_utils:log_stanzas("Actual response", [Response])

        end).

set_list() -> [{require, privacy_lists}].

set_list(Config) ->
    escalus:story(Config, [3, 1], fun(Alice, Alice2, Alice3, _Bob) ->

        AliceDenyBob = ?config(alice_deny_bob, Config),
        escalus_client:send(Alice,
            escalus_stanza:privacy_set_one(Alice, AliceDenyBob)),

        %% Verify that original Alice gets iq result and notification.
        %% It's a bit quirky as these come in undefined order
        %% (actually, they always came with 'push' first and then 'result',
        %% but I suppose it's not mandatory).
        AliceResponses = escalus_client:wait_for_stanzas(Alice, 2),
        %escalus_utils:log_stanzas("Alice got", AliceResponses),
        true = lists:any(fun verify_result/1, AliceResponses)
            and lists:any(fun verify_push/1, AliceResponses),

        %% Verify that other resources also get the push.
        AliceResourceResponses = [
            escalus_client:wait_for_stanza(Alice2),
            escalus_client:wait_for_stanza(Alice3)
        ],
        %escalus_utils:log_stanzas("Alice resources got", AliceResourceResponses),
        lists:foreach(fun is_privacy_list_push/1, AliceResourceResponses)

        %% All in all, the spec requires the resources to reply
        %% (as to every iq), but it's omitted here.

        end).

remove_list() -> [{require, privacy_lists}].

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
        escalus_assert:is_nonexistent_list_error(
            escalus_client:wait_for_stanza(Alice))

        end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

%% Create empty list element with given name.
privacy_list(Name, Items) ->
    exmpp_xml:append_children(
        exmpp_xml:set_attribute(
            exmpp_xml:remove_attribute(
                exmpp_xml:element('list'),
                <<"xmlns">>),
            {<<"name">>, Name}),
        Items).

%% Create a privacy list item element, wrapping up arguments as attributes.
privacy_list_item({Type, Value, Action, Order}) ->
    Attrs = [{<<"type">>, Type}, {<<"value">>, Value}, {<<"action">>, Action},
        {<<"order">>, Order}],
    exmpp_xml:set_attributes(exmpp_xml:element('item'), Attrs).

%% Is this iq a notification about a privacy list being changed?
is_privacy_list_push(Iq) ->
    escalus_assert:is_iq('set', Iq),
    Query = exmpp_xml:get_element(Iq, ?NS_PRIVACY, 'query'),
    true = exmpp_xml:has_element(Query, 'list').

verify_result(Stanza) ->
    try escalus_assert:is_iq('result', Stanza) of
        _ -> true
    catch
        _:_ -> false
    end.

verify_push(Stanza) ->
    try is_privacy_list_push(Stanza) of
        _ -> true
    catch
        _:_ -> false
    end.
