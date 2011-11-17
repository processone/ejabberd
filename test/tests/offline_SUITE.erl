%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Suite for testing mod_offline* modules
%%% @end
%%%===================================================================

-module(offline_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [{group, offline_tests}].

groups() ->
    [{offline_tests, [sequence], [simple_message]}].

suite() ->
    escalus:suite().

%%%===================================================================
%%% Init & teardown
%%%===================================================================

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

%%%===================================================================
%%% offline tests
%%%===================================================================

simple_message(Config) ->
    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [1], fun(Alice) ->
        escalus:send(Alice, Chat=escalus_stanza:chat_to(bob, "Hi, Offline!"))
    end),

    %% Bob logs in
    Bob = login_send_presence(Config, bob),

    %% He receives his initial presence and the message
    Stanzas = escalus:wait_for_stanzas(Bob, 2),
    escalus_new_assert:mix_match([is_presence,
                                  is_chat("Hi, Offline!")],
                                 Stanzas),
    escalus_cleaner:clean(Config).

%%%===================================================================
%%% Custom predicates
%%%===================================================================

is_chat(Content) ->
    fun(Stanza) -> escalus_pred:is_chat_message(Content, Stanza) end.

%%%===================================================================
%%% Helpers
%%%===================================================================

login_send_presence(Config, User) ->
    Spec = escalus_users:get_userspec(Config, User),
    Client = escalus_client:start(Config, Spec, "dummy"),
    escalus:send(Client, escalus_stanza:presence(available)),
    Client.
