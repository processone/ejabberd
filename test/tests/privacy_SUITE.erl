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

%% template
%get_roster(Config) ->                                                                                                    
    %escalus:story(Config, [1, 1], fun(Alice,_Bob) ->

        %escalus_client:send(Alice, escalus_stanza:roster_get()),
        %escalus_assert:is_roster_result(escalus_client:wait_for_stanza(Alice))

        %end).

get_all_lists(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        escalus_client:send(Alice, escalus_stanza:privacy_get_all(Alice)),
        escalus_assert:is_privacy_query_result(
            escalus_client:wait_for_stanza(Alice) )

        end).

get_nonexistent_list(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        escalus_client:send(Alice, escalus_stanza:privacy_get_one(Alice, "public")),
        escalus_assert:is_nonexistent_list_error(
            escalus_client:wait_for_stanza(Alice))

        end).

set_list(Config) ->
    escalus:story(Config, [3, 1], fun(Alice, Alice2, Alice3, Bob) ->

        Items = lists:map(fun privacy_list_item/1,
            [{"jid", Bob#client.jid, "deny", "1"}]),
        escalus_client:send(Alice,
            escalus_stanza:privacy_set_one(Alice, privacy_list("someList", Items))),

        %% Verify that original Alice gets iq result and notification.
        %% It's a bit quirky as these come in undefined order
        %% (actually, they always came with 'push' first and then 'result',
        %% but I suppose it's not mandatory).
        AliceResponses = [
            escalus_client:wait_for_stanza(Alice),
            escalus_client:wait_for_stanza(Alice)
        ],
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

remove_list(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        Items = lists:map(fun privacy_list_item/1,
            [{"jid", Bob#client.jid, "deny", "1"}]),
        escalus_client:send(Alice,
            escalus_stanza:privacy_set_one(Alice, privacy_list("someList", Items))),

        %% These are the pushed notification and iq result.
        _SkipThese = [
            escalus_client:wait_for_stanza(Alice),
            escalus_client:wait_for_stanza(Alice)
        ],

        %% Request list deletion by sending an empty list.
        RemoveRequest = escalus_stanza:privacy_set_one(Alice, privacy_list("someList", [])),
        %escalus_utils:log_stanzas("Alice deletes", [RemoveRequest]),
        escalus_client:send(Alice, RemoveRequest),

        %% These too are the pushed notification and iq result.
        _SkipTheseToo = [
            escalus_client:wait_for_stanza(Alice),
            escalus_client:wait_for_stanza(Alice)
        ],
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
        exmpp_xml:set_attribute(exmpp_xml:element('list'), {<<"name">>, Name}),
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
