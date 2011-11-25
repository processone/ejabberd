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
    [{group, mod_offline_tests}].
    %% FIXME: uncomment mod_offline_odbc_tests when the module is ready
    %% [{group, mod_offline_tests},
    %%  {group, mod_offline_odbc_tests}].

all_tests() ->
    [simple_message].

groups() ->
    [{mod_offline_tests, [sequence], all_tests()},
     {mod_offline_odbc_tests, [sequence], all_tests()}].

suite() ->
    escalus:suite().

%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(Config0) ->
    Config1 = escalus:init_per_suite(Config0),
    escalus:create_users(Config1).

end_per_suite(Config) ->
    escalus:delete_users(Config),
    escalus:end_per_suite(Config).

init_per_group(mod_offline_tests, Config) ->
    start_module(mod_offline, []),
    Config;
init_per_group(mod_offline_odbc_tests, Config) ->
    start_module(mod_offline_odbc, []),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(mod_offline_tests, _Config) ->
    stop_module(mod_offline);
end_per_group(mod_offline_odbc_tests, _Config) ->
    stop_module(mod_offline_odbc);
end_per_group(_GroupName, _Config) ->
    ok.

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
        escalus:send(Alice, escalus_stanza:chat_to(bob, "Hi, Offline!"))
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
    {ok, Client} = escalus_client:start(Config, Spec, "dummy"),
    escalus:send(Client, escalus_stanza:presence(available)),
    Client.

start_module(ModuleName, Options) ->
    Args = [ct:get_config(ejabberd_domain), ModuleName, Options],
    escalus_ejabberd:rpc(gen_mod, start_module, Args).

stop_module(ModuleName) ->
    Args = [ct:get_config(ejabberd_domain), ModuleName],
    escalus_ejabberd:rpc(gen_mod, stop_module, Args).
