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

-module(snmp_c2s_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-define(WAIT_TIME, 500).

-import(snmp_helper, [assert_counter/2,
                      get_counter_value/1]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, single},
     {group, multiple},
     {group, drop},
     {group, errors},
     {group, count}].

groups() ->
    [{single, [sequence], [message_one, 
                           stanza_one, 
                           presence_one,
                           presence_direct_one,
                           iq_one]},
     {multiple, [sequence], [messages]},
     {drop, [sequence], [bounced
                         ]},
     {errors, [sequence], [error_total,
                           error_mesg,
                           error_iq,
                           error_presence]},
     {count, [sequence], [stanza_count]}].
     
suite() ->
    [{require, ejabberd_node} | escalus:suite()].

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
%% Message tests
%%--------------------------------------------------------------------


message_one(Config) ->
    {value, MesgSent} = get_counter_value(xmppMessageSent),
    {value, MesgReceived} = get_counter_value(xmppMessageReceived),
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, "Hi!")),
        escalus_client:wait_for_stanza(Bob),

        assert_counter(MesgSent + 1, xmppMessageSent),
        assert_counter(MesgReceived + 1, xmppMessageReceived)
        
        end).

stanza_one(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        {value, StanzaSent} = get_counter_value(xmppStanzaSent),
        {value, StanzaReceived} = get_counter_value(xmppStanzaReceived),
    
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, "Hi!")),
        escalus_client:wait_for_stanza(Bob),

        assert_counter(StanzaSent + 1, xmppStanzaSent),
        assert_counter(StanzaReceived + 1, xmppStanzaReceived)
        
        end).

presence_one(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        {value, PresenceSent} = get_counter_value(xmppPresenceSent),
        {value, PresenceReceived} = get_counter_value(xmppPresenceReceived),
        {value, StanzaSent} = get_counter_value(xmppStanzaSent),
        {value, StanzaReceived} = get_counter_value(xmppStanzaReceived),
    
        escalus_client:send(Alice, escalus_stanza:presence(available)),
        escalus_client:wait_for_stanza(Alice),
        
        assert_counter(PresenceSent + 1, xmppPresenceSent),
        assert_counter(PresenceReceived + 1, xmppPresenceReceived),
        assert_counter(StanzaSent + 1, xmppStanzaSent),
        assert_counter(StanzaReceived + 1, xmppStanzaReceived)
        
        end).

presence_direct_one(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        {value, PresenceSent} = get_counter_value(xmppPresenceSent),
        {value, PresenceReceived} = get_counter_value(xmppPresenceReceived),
        {value, StanzaSent} = get_counter_value(xmppStanzaSent),
        {value, StanzaReceived} = get_counter_value(xmppStanzaReceived),
    
        escalus_client:send(Alice, 
                            escalus_stanza:presence_direct(Bob, available)),
        escalus_client:wait_for_stanza(Bob),
        
        assert_counter(PresenceSent + 1, xmppPresenceSent),
        assert_counter(PresenceReceived + 1, xmppPresenceReceived),
        assert_counter(StanzaSent + 1, xmppStanzaSent),
        assert_counter(StanzaReceived + 1, xmppStanzaReceived)
        
        end).

iq_one(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        {value, IqSent} = get_counter_value(xmppIqSent),
        {value, IqReceived} = get_counter_value(xmppIqReceived),
        {value, StanzaSent} = get_counter_value(xmppStanzaSent),
        {value, StanzaReceived} = get_counter_value(xmppStanzaReceived),
    
        escalus_client:send(Alice, 
                            escalus_stanza:roster_get()),
        escalus_client:wait_for_stanza(Alice),
        
        assert_counter(IqSent + 1, xmppIqSent),
        assert_counter(StanzaSent + 1, xmppStanzaSent),
        assert_counter(StanzaReceived + 1, xmppStanzaReceived),
        assert_counter(IqReceived + 1, xmppIqReceived)
        
        end).
    
messages(Config) ->    
    {value, MesgSent} = get_counter_value(xmppMessageSent),
    {value, MesgReceived} = get_counter_value(xmppMessageReceived),
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, "Hi!")),
        escalus_client:wait_for_stanza(Bob),
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, "Hi!")),
        escalus_client:wait_for_stanza(Alice),
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, "Hi!")),
        escalus_client:wait_for_stanza(Bob),
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, "Hi!")),
        escalus_client:wait_for_stanza(Alice),

        assert_counter(MesgSent + 4, xmppMessageSent),
        assert_counter(MesgReceived + 4, xmppMessageReceived)
        
        end).

bounced(Config) ->
    {value, MesgBounced} = get_counter_value(xmppMessageBounced),
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        escalus_client:stop(Bob),
        timer:sleep(?WAIT_TIME),
        
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, "Hi!")),
        timer:sleep(?WAIT_TIME),

        assert_counter(MesgBounced + 1, xmppMessageBounced)
        
        end).

stanza_count(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        {value, OldStanzaCount} = get_counter_value(xmppStanzaCount),

        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, "Hi!")),
        escalus_client:wait_for_stanza(Bob),
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, "Hi!")),
        escalus_client:wait_for_stanza(Alice),
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, "Hi!")),
        escalus_client:wait_for_stanza(Bob),
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, "Hi!")),
        escalus_client:wait_for_stanza(Alice),

        {value, StanzaCount} = get_counter_value(xmppStanzaCount),
        true = StanzaCount >= OldStanzaCount + 4
        
        end).


%%-----------------------------------------------------
%% Error tests
%%-----------------------------------------------------
    
error_total(Config) ->
    {value, Errors} = get_counter_value(xmppErrorTotal),
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        escalus_client:stop(Bob),
        timer:sleep(?WAIT_TIME),
        
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, "Hi!")),
        timer:sleep(?WAIT_TIME),
        
        assert_counter(Errors + 1, xmppErrorTotal)
        
        end).

error_mesg(Config) ->
    {value, Errors} = get_counter_value(xmppErrorMessage),
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        escalus_client:stop(Bob),
        timer:sleep(?WAIT_TIME),
        
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, "Hi!")),
        timer:sleep(?WAIT_TIME),
        
        assert_counter(Errors + 1, xmppErrorMessage)
        
        end).

error_presence(Config) ->
    {value, Errors} = get_counter_value(xmppErrorPresence),
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        escalus_client:send(Alice, escalus_stanza:presence_direct(Bob, available)),
        Presence = escalus_client:wait_for_stanza(Bob),

        escalus_client:send(Bob, escalus_stanza:presence_error(Presence, 'gone')),
        escalus_client:wait_for_stanza(Alice),
        
        assert_counter(Errors + 1, xmppErrorPresence)
        
        end).

error_iq(_Config) ->
    {value, Errors} = get_counter_value(xmppErrorIq),
    
    Alice = escalus_users:get_user_by_name(alice),
    escalus_users:create_user(Alice),
    
    timer:sleep(?WAIT_TIME),

    assert_counter(Errors + 1, xmppErrorIq).
        
