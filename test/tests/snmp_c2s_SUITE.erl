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

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, single},
     {group, multiple},
     {group, drop}].

groups() ->
    [{single, [sequence], [message_one, 
                           stanza_one, 
                           presence_one,
                           presence_direct_one,
                           iq_one]},
     {multiple, [sequence], [messages]},
     {drop, [sequence], [bounced]}].
     
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
    
    

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

assert_counter(Value, Counter) ->
    {value, Value} = rpc:call(ct:get_config(ejabberd_node), 
                     mod_snmp, 
                     handle_entry, 
                     [get, Counter]).

get_counter_value(Counter) ->
    rpc:call(ct:get_config(ejabberd_node), 
             mod_snmp, 
             handle_entry, 
             [get, Counter]).
