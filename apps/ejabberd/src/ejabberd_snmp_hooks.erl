%%%------------------------------------------------------------------------
%%% File:   ejabberd_smnp_hooks.erl
%%% Author: Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
%%%         Radoslaw Szymczyszyn <radoslaw.szymczyszyn@erlang-solutions.com>
%%% Description: Hooks that needs to be registered for SNMP counters to work.
%%%
%%% Created: 29 July by <aleksandra.lipiec@erlang-solutions.com>
%%%------------------------------------------------------------------------
-module(ejabberd_snmp_hooks).

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([get_hooks/1]).

%%-------------------
%% Internal exports
%%-------------------
-export([sm_register_connection_hook/3,
         sm_remove_connection_hook/3,
         auth_failed/3,
         user_send_packet/3,
         user_receive_packet/4,
         xmpp_bounce_message/1,
         xmpp_stanza_dropped/3,
         xmpp_send_element/1,
         roster_get/2,
         roster_set/3,
         privacy_iq_get/5,
         privacy_iq_set/4,
         privacy_check_packet/6]).

-define(CORE, ejabberd_snmp_core).

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
     [xmpp_bounce_message, Host, ?MODULE, xmpp_bounce_message, 50],
     [xmpp_send_element, Host, ?MODULE, xmpp_send_element, 50],
     [roster_get, Host, ?MODULE, roster_get, 55],
     [roster_set, Host, ?MODULE, roster_set, 50],
     [privacy_iq_get,         Host, ?MODULE, privacy_iq_get, 1],
     [privacy_iq_set,         Host, ?MODULE, privacy_iq_set, 1],
     [privacy_check_packet,   Host, ?MODULE, privacy_check_packet, 55]].

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
user_send_packet(_,_,Packet) ->
    ejabberd_snmp_core:increment_counter(xmppStanzaSent),
    user_send_packet_type(Packet).

user_send_packet_type({xmlelement, <<"message">>,_,_}) ->
    ejabberd_snmp_core:increment_counter(xmppMessageSent);
user_send_packet_type({xmlelement, <<"iq">>,_,_}) ->
    ejabberd_snmp_core:increment_counter(xmppIqSent);
user_send_packet_type({xmlelement, <<"presence">>,_,_}) ->
    ejabberd_snmp_core:increment_counter(xmppPresenceSent).

-spec user_receive_packet(tuple(), tuple(), tuple(), tuple()) -> term().
user_receive_packet(_,_,_,Packet) ->
    ejabberd_snmp_core:increment_counter(xmppStanzaReceived),
    user_receive_packet_type(Packet).

user_receive_packet_type({xmlelement, <<"message">>,_,_}) ->
    ejabberd_snmp_core:increment_counter(xmppMessageReceived);
user_receive_packet_type({xmlelement, <<"iq">>,_,_}) ->
    ejabberd_snmp_core:increment_counter(xmppIqReceived);
user_receive_packet_type({xmlelement, <<"presence">>,_,_}) ->
    ejabberd_snmp_core:increment_counter(xmppPresenceReceived).

-spec xmpp_bounce_message(tuple()) -> term().
xmpp_bounce_message(_) ->
    ejabberd_snmp_core:increment_counter(xmppMessageBounced).

-spec xmpp_stanza_dropped(tuple(), tuple(), tuple()) -> term().
xmpp_stanza_dropped(_,_,_) ->
    ejabberd_snmp_core:increment_counter(xmppStanzaDropped).

-spec xmpp_send_element(tuple()) -> term().
xmpp_send_element({xmlelement, Name, Attrs, _}) ->
    case proplists:get_value(<<"type">>, Attrs) of
        <<"error">> ->
            ejabberd_snmp_core:increment_counter(xmppErrorTotal),
            case Name of
                <<"iq">> ->
                    ejabberd_snmp_core:increment_counter(xmppErrorIq);
                <<"message">> ->
                    ejabberd_snmp_core:increment_counter(xmppErrorMessage);
                <<"presence">> ->
                    ejabberd_snmp_core:increment_counter(xmppErrorPresence)
            end;
        _ -> ok
    end;
xmpp_send_element(_) ->
    ok.


%% Roster

-spec roster_get(list(), term()) -> list().
roster_get(Acc, _) ->
    ejabberd_snmp_core:increment_counter(modRosterGets),
    Acc.

-spec roster_set(tuple(), tuple(), tuple()) -> list().
roster_set(_,_,_) ->
    ejabberd_snmp_core:increment_counter(modRosterSets).

%% Privacy

-spec privacy_iq_get(term(), term(), term(), term(), term()) -> term().
privacy_iq_get(Acc, _, _, _, _) ->
    ?CORE:increment_counter(modPrivacyGets),
    Acc.

-spec privacy_iq_set(term(), term(), term(), term()) -> term().        
privacy_iq_set(Acc, _, _, #iq{sub_el = SubEl}) ->
    {xmlelement, _, _, Els} = SubEl,
    case xml:remove_cdata(Els) of
	[{xmlelement, <<"active">>, _, _}] ->
            ?CORE:increment_counter(modPrivacySetsActive);
	[{xmlelement, <<"default">>, _, _}] ->
            ?CORE:increment_counter(modPrivacySetsDefault);
    _ ->
        ok
    end,
    ?CORE:increment_counter(modPrivacySets),
    Acc.

-spec privacy_check_packet(Acc :: allow | deny, term(), term(), term(), term(), term()) -> allow | deny.
privacy_check_packet(Acc, _, _, _, _, _) ->
    ?CORE:increment_counter(modPrivacyStanzaAll),
    case Acc of
    deny ->
        ?CORE:increment_counter(modPrivacyStanzaBlocked);
    allow ->
        ok
    end,
    Acc.

%%% vim: set sts=4 ts=4 sw=4 et filetype=erlang foldmarker=%%%',%%%. foldmethod=marker:
