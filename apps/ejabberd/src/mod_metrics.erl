%%%-------------------------------------------------------------------
%%% @author Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Implementation of MongooseIM metrics.
%%%
%%% @end
%%% Created : 23 Apr 2013 by Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module (mod_metrics).

-behaviour (gen_mod).

-export ([start/2, stop/1]).


-spec start(binary(), list()) -> ok.
start(Host, _Opts) ->
    init_folsom(Host),
    metrics_hooks(add, Host),
    ok.

-spec stop(binary()) -> ok.
stop(Host) ->
    metrics_hooks(delete, Host),
    ok.

init_folsom(Host) ->
    folsom:start(),
    lists:foreach(fun(Name) ->
        folsom_metrics:new_spiral(Name),
        folsom_metrics:tag_metric(Name, Host)
    end, get_general_counters(Host)),

    lists:foreach(fun(Name) ->
        folsom_metrics:new_counter(Name),
        folsom_metrics:tag_metric(Name, Host)
    end, get_total_counters(Host)).

metrics_hooks(Op, Host) ->
    lists:foreach(fun(Hook) ->
        apply(ejabberd_hooks, Op, Hook)
    end, ejabberd_metrics_hooks:get_hooks(Host)).

-define (GENERAL_COUNTERS, [
         sessionSuccessfulLogins,
         sessionAuthAnonymous,
         sessionAuthFails,
         sessionLogouts,
         xmppMessageSent,
         xmppMessageReceived,
         xmppMessageBounced,
         xmppPresenceSent,
         xmppPresenceReceived,
         xmppIqSent,
         xmppIqReceived,
         xmppStanzaSent,
         xmppStanzaReceived,
         xmppStanzaDropped,
         xmppStanzaCount,
         xmppErrorTotal,
         xmppErrorBadRequest,
         xmppErrorIq,
         xmppErrorMessage,
         xmppErrorPresence,
         xmppIqTimeouts,
         modRosterSets,
         modRosterGets,
         modPresenceSubscriptions,
         modPresenceUnsubscriptions,
         modRosterPush,
         modRegisterCount,
         modUnregisterCount,
         modPrivacySets,
         modPrivacySetsActive,
         modPrivacySetsDefault,
         modPrivacyPush,
         modPrivacyGets,
         modPrivacyStanzaBlocked,
         modPrivacyStanzaAll
         ]).

get_general_counters(Host) ->
    [{Host, Counter} || Counter <- ?GENERAL_COUNTERS].

-define (TOTAL_COUNTERS, [
         sessionCount
         ]).

get_total_counters(Host) ->
    [{Host, Counter} || Counter <- ?TOTAL_COUNTERS].

