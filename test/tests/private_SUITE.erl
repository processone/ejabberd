%%==============================================================================
%% Copyright 2011 Erlang Solutions Ltd.
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
-module(private_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [{group, private_positive},
     {group, private_negative}].

groups() ->
     [{private_positive, [sequence], [store,
                                      retrieve]},
      {private_negative, [sequence], [get_other_user,
                                      set_other_user]}].
                                      %% FIXME: broken exmpp prevents us from sending
                                      %% out elements without NS set
                                      %% missing_ns]}].

suite() ->
    escalus:suite().

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
%% Private storage tests
%%--------------------------------------------------------------------
store(Config) ->
    escalus:story(Config, [1],
                  fun(Alice) ->
                          NS = 'alice:private:ns',

                          %% Alice stores some data in her private storage
                          escalus_client:send(Alice, escalus_stanza:private_set(NS, make_body('my_element',
                                                                                              'banana'))),

                          %% Alice receives store confirmation
                          escalus:assert(is_private_result, escalus_client:wait_for_stanza(Alice))
                  end).

retrieve(Config) ->
    escalus:story(Config, [1],
                  fun(Alice) ->
                          NS = 'alice:private:ns',

                          %% Alice asks for the data stored in the previous test
                          escalus_client:send(Alice, escalus_stanza:private_get(NS, 'my_element')),

                          %% Alice ensures data has not been changed
                          Stanza = escalus_client:wait_for_stanza(Alice),
                          escalus:assert(is_private_result, Stanza),
                          check_body(Stanza, ['my_element', 'banana']),

                          %% Alice asks for non-existing data
                          escalus_client:send(Alice, escalus_stanza:private_get('non_existing_ns',
                                                                                'my_element')),

                          %% Alice receives an empty response
                          Stanza2 = escalus_client:wait_for_stanza(Alice),
                          escalus:assert(is_private_result, Stanza2),
                          check_body(Stanza, ['my_element'])
                  end).

get_other_user(Config) ->
    escalus:story(Config, [1, 1],
                  fun(Alice, Bob) ->
                          NS = 'bob:private:ns',

                          %% Alice asks for Bob's private data
                          IQ = exmpp_stanza:set_recipient(escalus_stanza:private_get(NS, 'my_element'),
                                                          escalus_utils:get_short_jid(Bob)),
                          escalus_client:send(Alice, IQ),

                          %% Alice gets an error
                          Stanza = escalus_client:wait_for_stanza(Alice),
                          escalus:assert(is_private_error, Stanza),
                          escalus_pred:is_error(<<"cancel">>, forbidden, Stanza)
                  end).

set_other_user(Config) ->
    escalus:story(Config, [1, 1],
                  fun(Alice, Bob) ->
                          NS = 'bob:private:ns',

                          %% Alice asks for Bob's private data
                          IQ = exmpp_stanza:set_recipient(escalus_stanza:private_set(NS, make_body('my_element', 'banana')),
                                                          escalus_utils:get_short_jid(Bob)),
                          escalus_client:send(Alice, IQ),

                          %% Alice gets a forbidden error
                          Stanza = escalus_client:wait_for_stanza(Alice),
                          escalus:assert(is_private_error, Stanza),
                          escalus_pred:is_error(<<"cancel">>, forbidden, Stanza)
                  end).

missing_ns(Config) ->
    escalus:story(Config, [1],
                  fun(Alice) ->
                          NS = 'bob:private:ns',

                          %% Alice asks for her own private storage, without
                          %% providing a namespace for a child
                          IQ = escalus_stanza:private_get(NS, make_body('my_element', 'banana')),
                          NewIQ = remove_child_ns(IQ),
                          escalus_client:send(Alice, NewIQ),

                          %% Alice gets a bad-format error
                          Stanza = escalus_client:wait_for_stanza(Alice),
                          escalus:assert(is_private_error, Stanza),
                          escalus_pred:is_error(<<"modify">>, 'bad-format', Stanza)
                  end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------
make_body(Parent, Child) ->
    exmpp_xml:append_child(exmpp_xml:element(Parent),
                           exmpp_xml:element(Child)).

check_body(Stanza, Names) ->
    Query = exmpp_xml:get_element(Stanza, "query"),
    check_body_rec(Query, Names).

check_body_rec(_, []) ->
    ok;
check_body_rec(Element, [Name | Names]) ->
    [Child] = exmpp_xml:get_child_elements(Element),
    Name = exmpp_xml:get_name_as_atom(Child),
    check_body_rec(Child, Names).

remove_child_ns(Stanza) ->
    [Child] = exmpp_xml:get_child_elements(Stanza),
    NewChild = exmpp_xml:remove_attribute(Child, <<"xmlns">>),
    exmpp_xml:replace_child(Stanza, Child, NewChild).
