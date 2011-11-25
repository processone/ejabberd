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
     {group, roster},
     {group, subscribe}].

groups() ->
    [{presence, [sequence], [available,
                             available_direct,
                             additions]},
     {roster, [sequence], [get_roster,
                           add_contact,
                           remove_contact]},
     {subscribe, [sequence], [subscribe,
                              subscribe_decline,
                              unsubscribe,
                              remove_unsubscribe]}].

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
end_per_testcase(subscribe, Config) ->
    end_rosters_remove(Config);
end_per_testcase(subscribe_decline, Config) ->
    end_rosters_remove(Config);
end_per_testcase(unsubscribe, Config) ->
    end_rosters_remove(Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

end_rosters_remove(Config) ->
    [{_, UserSpec1}, {_, UserSpec2} | _] =
        escalus_config:get_property(escalus_users, Config),
    remove_roster(UserSpec1),
    remove_roster(UserSpec2),
    escalus:end_per_testcase(subscription, Config).


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
        Received = escalus_client:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_roster_set, is_result], Received),

        Result = hd([R || R <- Received, escalus_pred:is_roster_set(R)]),
        escalus_assert:count_roster_items(1, Result),
        escalus_client:send(Alice, escalus_stanza:iq_result(Result)),

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
        escalus_client:send(Alice, escalus_stanza:roster_remove_contact(Bob)),
        escalus_assert:is_roster_set(escalus_client:wait_for_stanza(Alice)),
        escalus_assert:is_result(escalus_client:wait_for_stanza(Alice)),

        %% check roster
        escalus_client:send(Alice, escalus_stanza:roster_get()),
        escalus_assert:count_roster_items(0, escalus_client:wait_for_stanza(Alice))

    end).


subscribe(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->

        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus_client:send(Alice, escalus_stanza:presence_direct(Bob, subscribe)),
        PushReq = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_roster_set(PushReq),
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
        escalus_assert:is_roster_set(PushReqB),
        escalus_client:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus_assert:is_result(escalus_client:wait_for_stanza(Bob)),

        %% Bob sends subscribed presence
        escalus_client:send(Bob, escalus_stanza:presence_direct(Alice, subscribed)),

        %% Alice receives subscribed
        Stanzas = escalus_client:wait_for_stanzas(Alice, 2),

        check_subscription_stanzas(Stanzas, "subscribed"),
        escalus_assert:is_presence_stanza(escalus_client:wait_for_stanza(Alice)),

        %% Bob receives roster push
        PushReqB1 = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_roster_set(PushReqB1),

        %% Bob sends presence
        escalus_client:send(Bob, escalus_stanza:presence(available)),
        escalus_assert:is_presence_stanza(escalus_client:wait_for_stanza(Alice)),

        %% Bob sends presence
        escalus_client:send(Bob, escalus_stanza:presence(unavailable)),
        escalus_assert:is_presence_type("unavailable",
                                        escalus_client:wait_for_stanza(Alice))

        end).

subscribe_decline(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->

        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus_client:send(Alice, escalus_stanza:presence_direct(Bob, subscribe)),
        PushReq = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_roster_set(PushReq),
        escalus_client:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        Received = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_presence_type("subscribe", Received),

        %% Bob refuses subscription
        escalus_client:send(Bob, escalus_stanza:presence_direct(Alice, unsubscribed)),

        %% Alice receives subscribed
        Stanzas = escalus_client:wait_for_stanzas(Alice, 2),

        check_subscription_stanzas(Stanzas, "unsubscribed")

    end).

unsubscribe(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->

        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus_client:send(Alice, escalus_stanza:presence_direct(Bob, subscribe)),
        PushReq = escalus_client:wait_for_stanza(Alice),
        escalus_client:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        escalus_assert:is_presence_type("subscribe",
                                        escalus_client:wait_for_stanza(Bob)),
        %% Bob adds new contact to his roster
        escalus_client:send(Bob,
                            escalus_stanza:roster_add_contact(Alice,
                                                              ["enemies"],
                                                              "Alice")),
        PushReqB = escalus_client:wait_for_stanza(Bob),
        escalus_client:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus_assert:is_result(escalus_client:wait_for_stanza(Bob)),

        %% Bob sends subscribed presence
        escalus_client:send(Bob, escalus_stanza:presence_direct(Alice, subscribed)),

        %% Alice receives subscribed
        Stanzas = escalus_client:wait_for_stanzas(Alice, 2),

        check_subscription_stanzas(Stanzas, "subscribed"),
        escalus_assert:is_presence_stanza(escalus_client:wait_for_stanza(Alice)),

        %% Bob receives roster push
        PushReqB1 = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_roster_set(PushReqB1),

        %% Alice sends unsubscribe
        escalus_client:send(Alice, escalus_stanza:presence_direct(Bob, unsubscribe)),

        PushReqA2 = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_roster_set(PushReqA2),
        escalus_client:send(Alice, escalus_stanza:iq_result(PushReqA2)),

        %% Bob receives unsubscribe

        StanzasB = escalus_client:wait_for_stanzas(Bob, 2),

        check_subscription_stanzas(StanzasB, "unsubscribe"),

        %% Alice receives unsubscribed
        escalus_assert:is_presence_type("unavailable",
                                        escalus_client:wait_for_stanza(Alice))
    end).

remove_unsubscribe(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->
        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus_client:send(Alice, escalus_stanza:presence_direct(Bob, subscribe)),
        PushReq = escalus_client:wait_for_stanza(Alice),
        escalus_client:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        escalus_assert:is_presence_type("subscribe",
                                        escalus_client:wait_for_stanza(Bob)),
        %% Bob adds new contact to his roster
        escalus_client:send(Bob,
                            escalus_stanza:roster_add_contact(Alice,
                                                              ["enemies"],
                                                              "Alice")),
        PushReqB = escalus_client:wait_for_stanza(Bob),
        escalus_client:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus_assert:is_result(escalus_client:wait_for_stanza(Bob)),

        %% Bob sends subscribed presence
        escalus_client:send(Bob, escalus_stanza:presence_direct(Alice, subscribed)),

        %% Alice receives subscribed
        Stanzas = [escalus_client:wait_for_stanza(Alice),
                   escalus_client:wait_for_stanza(Alice)],

        check_subscription_stanzas(Stanzas, "subscribed"),
        escalus_assert:is_presence_stanza(escalus_client:wait_for_stanza(Alice)),

        %% Bob receives roster push
        PushReqB1 = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_roster_set(PushReqB1),

        %% remove contact
        escalus_client:send(Alice, escalus_stanza:roster_remove_contact(Bob)),

        StanzasA = escalus_client:wait_for_stanzas(Alice, 3),
        true = contains_stanza(StanzasA, {escalus_assert, is_roster_set}),
        true = contains_stanza(StanzasA, {escalus_assert, is_result}),
        true = contains_stanza(StanzasA, fun(S) ->
                                escalus_assert:is_presence_type("unavailable",S) end),

        StanzasB = escalus_client:wait_for_stanzas(Bob, 2),
        true = contains_stanza(StanzasB, {escalus_assert, is_roster_set}),
        true = contains_stanza(StanzasB, fun(S) ->
                                escalus_assert:is_presence_type("unsubscribe",S) end)


    end).


%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

add_sample_contact(Alice, Bob) ->
    escalus_client:send(Alice,
                        escalus_stanza:roster_add_contact(Bob,
                                                          ["friends"],
                                                          "Bobby")),

    Received = escalus_client:wait_for_stanzas(Alice, 2),
    escalus:assert_many([is_roster_set, is_result], Received),

    Result = hd([R || R <- Received, escalus_pred:is_roster_set(R)]),
    escalus_assert:count_roster_items(1, Result),
    escalus_client:send(Alice, escalus_stanza:iq_result(Result)).

contains_stanza(Stanzas, F) ->
    lists:foldl(fun(El, Res) ->
                      try F(El) of
                          _ -> true
                      catch
                          _:_ -> Res
                      end
               end, false, Stanzas).

check_subscription_stanzas(Stanzas, Type) ->
    true = contains_stanza(Stanzas, {escalus_assert, is_roster_set}),
    true = contains_stanza(Stanzas, fun(S) ->
                                escalus_assert:is_presence_type(Type, S) end).

remove_roster(UserSpec) ->
    [SUsername, SServer, _Pass] = escalus_config:get_usp(UserSpec),
    Username = list_to_binary(SUsername),
    Server = list_to_binary(SServer),
    Mods = escalus_ejabberd:rpc(gen_mod, loaded_modules, [Server]),
    case lists:member(mod_roster, Mods) of
        true ->
            {atomic, _} = escalus_ejabberd:rpc(mod_roster, remove_user, [Username, Server]);
        false ->
            case lists:member(mod_roster_odbc, Mods) of
                true ->
                    ok = escalus_ejabberd:rpc(mod_roster_odbc, remove_user, [Username, Server]);
                false ->
                    throw(roster_not_loaded)
            end
    end.
