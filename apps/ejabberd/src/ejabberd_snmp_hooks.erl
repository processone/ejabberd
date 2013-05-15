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
         xmpp_bounce_message/2,
         xmpp_stanza_dropped/3,
         xmpp_send_element/2,
         roster_get/2,
         roster_set/3,
         roster_push/2,
         roster_in_subscription/6,
         register_user/2,
         remove_user/2,
         privacy_iq_get/5,
         privacy_iq_set/4,
         privacy_check_packet/6,
         privacy_list_push/3]).

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
     [roster_push, Host, ?MODULE, roster_push, 50],
     [roster_in_subscription, Host, ?MODULE, roster_in_subscription, 55],
     [register_user, Host, ?MODULE, register_user, 50],
     [remove_user, Host, ?MODULE, remove_user, 50],
     [privacy_iq_get,         Host, ?MODULE, privacy_iq_get, 1],
     [privacy_iq_set,         Host, ?MODULE, privacy_iq_set, 1],
     [privacy_check_packet,   Host, ?MODULE, privacy_check_packet, 55],
     [sm_broadcast,           Host, ?MODULE, privacy_list_push, 1]].

%%------------------------------
%% SNMP specific hook callbacks
%%------------------------------

-spec sm_register_connection_hook(tuple(), tuple(), term()) -> term().
sm_register_connection_hook(_,_,_) ->
    ejabberd_snmp_core:increment_counter(sessionSuccessfulLogins),
    ejabberd_snmp_core:increment_window_counter(sessionSuccessfulLoginsW),
    ejabberd_snmp_core:increment_counter(sessionCount).

-spec sm_remove_connection_hook(tuple(), tuple(), term()) -> term().
sm_remove_connection_hook(_,_,_) ->
    ejabberd_snmp_core:increment_counter(sessionLogouts),
    ejabberd_snmp_core:increment_window_counter(sessionLogoutsW),
    ejabberd_snmp_core:decrement_counter(sessionCount).

-spec auth_failed(binary(), binary(), binary()) -> term().
auth_failed(_,_,_) ->
    ejabberd_snmp_core:increment_counter(sessionAuthFails),
    ejabberd_snmp_core:increment_window_counter(sessionAuthFailsW).

-spec user_send_packet(tuple(), tuple(), tuple()) -> term().
user_send_packet(_,_,Packet) ->
    ejabberd_snmp_core:increment_counter(xmppStanzaSent),
    ejabberd_snmp_core:increment_window_counter(xmppStanzaSentW),
    user_send_packet_type(Packet).

user_send_packet_type(#xmlel{name = <<"message">>}) ->
    ejabberd_snmp_core:increment_counter(xmppMessageSent),
    ejabberd_snmp_core:increment_window_counter(xmppMessageSentW);
user_send_packet_type(#xmlel{name = <<"iq">>}) ->
    ejabberd_snmp_core:increment_counter(xmppIqSent),
    ejabberd_snmp_core:increment_window_counter(xmppIqSentW);
user_send_packet_type(#xmlel{name = <<"presence">>}) ->
    ejabberd_snmp_core:increment_counter(xmppPresenceSent),
    ejabberd_snmp_core:increment_window_counter(xmppPresenceSentW).

-spec user_receive_packet(tuple(), tuple(), tuple(), tuple()) -> term().
user_receive_packet(_,_,_,Packet) ->
    ejabberd_snmp_core:increment_counter(xmppStanzaReceived),
    ejabberd_snmp_core:increment_window_counter(xmppStanzaReceivedW),
    user_receive_packet_type(Packet).

user_receive_packet_type(#xmlel{name = <<"message">>}) ->
    ejabberd_snmp_core:increment_counter(xmppMessageReceived),
    ejabberd_snmp_core:increment_window_counter(xmppMessageReceivedW);
user_receive_packet_type(#xmlel{name = <<"iq">>}) ->
    ejabberd_snmp_core:increment_counter(xmppIqReceived),
    ejabberd_snmp_core:increment_window_counter(xmppIqReceivedW);
user_receive_packet_type(#xmlel{name = <<"presence">>}) ->
    ejabberd_snmp_core:increment_counter(xmppPresenceReceived),
    ejabberd_snmp_core:increment_window_counter(xmppPresenceReceivedW).

-spec xmpp_bounce_message(binary(), tuple()) -> term().
xmpp_bounce_message(_, _) ->
    ejabberd_snmp_core:increment_counter(xmppMessageBounced).

-spec xmpp_stanza_dropped(tuple(), tuple(), tuple()) -> term().
xmpp_stanza_dropped(_,_,_) ->
    ejabberd_snmp_core:increment_counter(xmppStanzaDropped).

-spec xmpp_send_element(binary(), tuple()) -> term().
xmpp_send_element(_, #xmlel{name = Name, attrs = Attrs}) ->
    ejabberd_snmp_core:increment_counter(xmppStanzaCount),
    case proplists:get_value(<<"type">>, Attrs) of
        <<"error">> ->
            ejabberd_snmp_core:increment_counter(xmppErrorTotal),
            ejabberd_snmp_core:increment_window_counter(xmppErrorTotalW),
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
xmpp_send_element(_, _) ->
    ok.


%% Roster

-spec roster_get(list(), term()) -> list().
roster_get(Acc, _) ->
    ejabberd_snmp_core:increment_counter(modRosterGets),
    ejabberd_snmp_core:increment_window_counter(modRosterGetsW),
    Acc.

-spec roster_set(tuple(), tuple(), tuple()) -> list().
roster_set(_,_,_) ->
    ejabberd_snmp_core:increment_counter(modRosterSets),
    ejabberd_snmp_core:increment_window_counter(modRosterSetsW).

-spec roster_in_subscription(term(), binary(), binary(), tuple(), atom(), term()) -> term().
roster_in_subscription(Acc,_,_,_,subscribed,_) ->
    ejabberd_snmp_core:increment_counter(modPresenceSubscriptions),
    ejabberd_snmp_core:increment_window_counter(modPresenceSubscriptionsW),
    Acc;
roster_in_subscription(Acc,_,_,_,unsubscribed,_) ->
    ejabberd_snmp_core:increment_counter(modPresenceUnsubscriptions),
    ejabberd_snmp_core:increment_window_counter(modPresenceUnsubscriptionsW),
    Acc;
roster_in_subscription(Acc,_,_,_,_,_) ->
    Acc.

-spec roster_push(term(),term()) -> term().
roster_push(_,_) ->
    ejabberd_snmp_core:increment_counter(modRosterPush),
    ejabberd_snmp_core:increment_window_counter(modRosterPushW).

%% Register

-spec register_user(binary(),binary()) -> term().
register_user(_,_) ->
    ejabberd_snmp_core:increment_counter(modRegisterCount),
    ejabberd_snmp_core:increment_window_counter(modRegisterCountW).

-spec remove_user(binary(),binary()) -> term().
remove_user(_,_) ->
    ejabberd_snmp_core:increment_counter(modUnregisterCount),
    ejabberd_snmp_core:increment_window_counter(modUnregisterCountW).

%% Privacy

-spec privacy_iq_get(term(), term(), term(), term(), term()) -> term().
privacy_iq_get(Acc, _, _, _, _) ->
    ?CORE:increment_counter(modPrivacyGets),
    ?CORE:increment_window_counter(modPrivacyGetsW),
    Acc.

-spec privacy_iq_set(term(), term(), term(), term()) -> term().
privacy_iq_set(Acc, _From, _To, #iq{sub_el = SubEl}) ->
    #xmlel{children = Els} = SubEl,
    case xml:remove_cdata(Els) of
        [#xmlel{name = <<"active">>}] ->
            ?CORE:increment_counter(modPrivacySetsActive),
            ?CORE:increment_window_counter(modPrivacySetsActiveW);
        [#xmlel{name = <<"default">>}] ->
            ?CORE:increment_counter(modPrivacySetsDefault),
            ?CORE:increment_window_counter(modPrivacySetsDefaultW);
        _ ->
            ok
    end,
    ?CORE:increment_counter(modPrivacySets),
    ?CORE:increment_window_counter(modPrivacySetsW),
    Acc.

-spec privacy_list_push(term(), term(), term()) -> term().
privacy_list_push(_From, To, Packet) ->
    case Packet of
        #xmlel{name = <<"broadcast">>, children = [{privacy_list, _, _}]} ->
            #jid{user = User, server = Server} = To,
            Count = length(ejabberd_sm:get_user_resources(User, Server)),
            ?CORE:update_counter(modPrivacyPush, Count),
            ?CORE:update_window_counter(modPrivacyPushW, Count);
        _ ->
            ok
    end.

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
