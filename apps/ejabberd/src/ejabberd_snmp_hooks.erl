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
         privacy_iq_get/5,
         privacy_iq_set/4]).

-define(CORE, ejabberd_snmp_core).

%%-------------------
%% Implementation
%%-------------------

%% Here will be declared which hooks should be registered
get_hooks(Host) ->
    [[sm_register_connection_hook, Host, ?MODULE, sm_register_connection_hook, 50],
     [sm_remove_connection_hook, Host, ?MODULE, sm_remove_connection_hook, 50],
     [auth_failed, Host, ?MODULE, auth_failed, 50],
     [privacy_iq_get, Host, ?MODULE, privacy_iq_get, 1],
     [privacy_iq_set, Host, ?MODULE, privacy_iq_set, 1]].

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

%%% vim: set sts=4 ts=4 sw=4 et filetype=erlang foldmarker=%%%',%%%. foldmethod=marker:
