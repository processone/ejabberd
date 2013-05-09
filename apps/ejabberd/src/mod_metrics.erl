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

-define(REST_LISTENER, ejabberd_metrics_rest).

-spec start(binary(), list()) -> ok.
start(Host, Opts) ->
    init_folsom(Host),
    start_cowboy(Opts),
    metrics_hooks(add, Host),
    ok.

-spec stop(binary()) -> ok.
stop(Host) ->
    stop_cowboy(),
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

start_cowboy(Opts) ->
    NumAcceptors = gen_mod:get_opt(num_acceptors, Opts, 10),
    case gen_mod:get_opt(port, Opts, undefined) of
        undefined ->
            ok;
        Port ->
            Dispatch = cowboy_router:compile([{'_', [
                                {"/metrics", ?REST_LISTENER, [available_metrics]},
                                {"/metrics/m", ?REST_LISTENER, [sum_metrics]},
                                {"/metrics/m/:metric", ?REST_LISTENER, [sum_metric]},
                                {"/metrics/host/:host/:metric", ?REST_LISTENER, [host_metric]},
                                {"/metrics/host/:host", ?REST_LISTENER, [host_metrics]}
                                ]}]),
            case cowboy:start_http(?REST_LISTENER, NumAcceptors,
                                   [{port, Port}],
                                   [{env, [{dispatch, Dispatch}]}]) of
                {error, {already_started, _Pid}} ->
                    ok;
                {ok, _Pid} ->
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

stop_cowboy() ->
    cowboy:stop(?REST_LISTENER).
