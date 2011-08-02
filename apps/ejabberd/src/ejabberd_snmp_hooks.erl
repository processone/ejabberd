%%%------------------------------------------------------------------------
%%% File:   ejabberd_smnp_hooks.erl
%%% Author: Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
%%%         Radoslaw Szymczyszyn <radoslaw.szymczyszyn@erlang-solutions.com>
%%% Description: Hooks that needs to be registered for SNMP counters to work.
%%%
%%% Created: 29 July by <aleksandra.lipiec@erlang-solutions.com>
%%%------------------------------------------------------------------------
-module(ejabberd_snmp_hooks).

-export([get_hooks/1]).

%%-------------------
%% Internal eksports
%%-------------------
-export([sm_register_connection_hook/3,
         sm_remove_connection_hook/3,
         auth_failed/3,
         user_send_packet/3,
         user_receive_packet/4,
         xmpp_bounce_message/1,
         xmpp_stanza_dropped/3]).


-include("ejabberd.hrl").


%%-------------------
%% Implementation
%%-------------------

%% Here will be declared which hooks should be registered
get_hooks(Host) ->
    [[sm_register_connection_hook, Host, ?MODULE, sm_register_connection_hook, 50],
     [sm_remove_connection_hook, Host, ?MODULE, sm_remove_connection_hook, 50],
     [auth_failed, Host, ?MODULE, auth_failed, 50],
     [user_send_packet, Host, ?MODULE, user_send_packet, 50],
     [user_receive_packet, Host, ?MODULE, user_receive_packet, 50],
     [xmpp_stanza_dropped, Host, ?MODULE, xmpp_stanza_dropped, 50],
     [xmpp_bounce_message, Host, ?MODULE, xmpp_bounce_message, 50]].


%%------------------------------
%% SNMP specific hook callbacks
%%------------------------------

-spec sm_register_connection_hook(tuple(), tuple(), term()) -> term().
sm_register_connection_hook(_,_,_) ->
    ejabberd_snmp_core:increment_counter(sessionSuccessfulLogins),
    ejabberd_snmp_core:increment_counter(sessionCount).

-spec sm_remove_connection_hook(tuple(), tuple(), term()) -> term().
sm_remove_connection_hook(_,_,_) ->
    ejabberd_snmp_core:increment_counter(sessionLogouts),
    ejabberd_snmp_core:decrement_counter(sessionCount).

-spec auth_failed(binary(), binary(), binary()) -> term().
auth_failed(_,_,_) ->
    ejabberd_snmp_core:increment_counter(sessionAuthFails).

-spec user_send_packet(tuple(), tuple(), tuple()) -> term().
user_send_packet(_,_,{xmlelement, <<"message">>,_,_}) ->
    ejabberd_snmp_core:increment_counter(xmppMessageSent),
    ejabberd_snmp_core:increment_counter(xmppStanzaSent);
user_send_packet(_,_,{xmlelement, <<"iq">>,_,_}) ->
    ejabberd_snmp_core:increment_counter(xmppIqSent),
    ejabberd_snmp_core:increment_counter(xmppStanzaSent);
user_send_packet(_,_,{xmlelement, <<"presence">>,_,_}) ->
    ejabberd_snmp_core:increment_counter(xmppPresenceSent),
    ejabberd_snmp_core:increment_counter(xmppStanzaSent).

-spec user_receive_packet(tuple(), tuple(), tuple(), tuple()) -> term().
user_receive_packet(_,_,_,{xmlelement, <<"message">>,_,_}) ->
    ejabberd_snmp_core:increment_counter(xmppMessageReceived),
    ejabberd_snmp_core:increment_counter(xmppStanzaReceived);
user_receive_packet(_,_,_,{xmlelement, <<"iq">>,_,_}) ->
    ejabberd_snmp_core:increment_counter(xmppIqReceived),
    ejabberd_snmp_core:increment_counter(xmppStanzaReceived);
user_receive_packet(_,_,_,{xmlelement, <<"presence">>,_,_}) ->
    ejabberd_snmp_core:increment_counter(xmppPresenceReceived),
    ejabberd_snmp_core:increment_counter(xmppStanzaReceived).

-spec xmpp_bounce_message(tuple()) -> term().
xmpp_bounce_message(_) ->
    ejabberd_snmp_core:increment_counter(xmppMessageBounced).

-spec xmpp_stanza_dropped(tuple(), tuple(), tuple()) -> term().
xmpp_stanza_dropped(_,_,_) ->
    ejabberd_snmp_core:increment_counter(xmppStanzaDropped).
