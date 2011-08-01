-module(ejabberd_snmp_core).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("EJABBERD-MIB.hrl").

-export([start/1,
         stop/0,
         is_started/0,
         increment_counter/1,
         decrement_counter/1,
         counter_value/1,
         table_value/4]).

%%%' Helper defines (module local)

-define(STATS(Module), table_name(Module)).
-define(COUNTERS_FOR_MODULE, [
    {general,      [generalUptime,
                    generalNodeName]},
    {core,         [sessionCount,
                    sessionUniqueCount,
                    sessionSuccessfulLogin,
                    sessionAuthAnonymous,
                    sessionAuthFail,
                    sessionLogout]},
    {c2s,          [xmppMessageSent,
                    xmppMessageReceived,
                    xmppMessageBounced,
                    xmppPresenceSent,
                    xmppPresenceReceived,
                    xmppIqSent,
                    xmppIqReceived,
                    xmppStanzaSent,
                    xmppStanzaReceived,
                    xmppStanzaDenied,
                    xmppStanzaDropped,
                    xmppErrorTotal,
                    xmppErrorBadRequest,
                    xmppErrorIq,
                    xmppErrorMessage,
                    xmppErrorPresence,
                    xmppIqTimeouts]},
    {mod_roster,   [modRosterSets,
                    modRosterGets,
                    modPresenceSubscriptions,
                    modPresenceUnsubscriptions,
                    modRosterPush,
                    modRosterSize,
                    modRosterGroups]},
    {mod_register, [modRegisterCount,
                    modUnregisterCount,
                    modRegisterUserCount]},
    {mod_privacy,  [modPrivacySets,
                    modPrivacySetsActive,
                    modPrivacySetsDefault,
                    modPrivacyPush,
                    modPrivacyGets,
                    modPrivacyStanzaBlocked,
                    modPrivacyListLength]} ]).
-define(MODULE_FOR_COUNTERS, [
        {generalUptime,              general},
        {generalNodeName,            general},
        {sessionCount,               core},
        {sessionUniqueCount,         core},
        {sessionSuccessfulLogin,     core},
        {sessionAuthAnonymous,       core},
        {sessionAuthFail,            core},
        {sessionLogout,              core},
        {xmppMessageSent,            c2s},
        {xmppMessageReceived,        c2s},
        {xmppMessageBounced,         c2s},
        {xmppPresenceSent,           c2s},
        {xmppPresenceReceived,       c2s},
        {xmppIqSent,                 c2s},
        {xmppIqReceived,             c2s},
        {xmppStanzaSent,             c2s},
        {xmppStanzaReceived,         c2s},
        {xmppStanzaDenied,           c2s},
        {xmppStanzaDropped,          c2s},
        {xmppErrorTotal,             c2s},
        {xmppErrorBadRequest,        c2s},
        {xmppErrorIq,                c2s},
        {xmppErrorMessage,           c2s},
        {xmppErrorPresence,          c2s},
        {xmppIqTimeouts,             c2s},
        {modRosterSets,              mod_roster},
        {modRosterGets,              mod_roster},
        {modPresenceSubscriptions,   mod_roster},
        {modPresenceUnsubscriptions, mod_roster},
        {modRosterPush,              mod_roster},
        {modRosterSize,              mod_roster},
        {modRosterGroups,            mod_roster},
        {modRegisterCount,           mod_register},
        {modUnregisterCount,         mod_register},
        {modRegisterUserCount,       mod_register},
        {modPrivacySets,             mod_privacy},
        {modPrivacySetsActive,       mod_privacy},
        {modPrivacySetsDefault,      mod_privacy},
        {modPrivacyPush,             mod_privacy},
        {modPrivacyGets,             mod_privacy},
        {modPrivacyStanzaBlocked,    mod_privacy},
        {modPrivacyListLength,       mod_privacy} ]).

%%%.

start(Modules) ->
    initialize_tables(Modules).

stop() ->
    destroy_tables(),
    ok.

%%%' Helper functions (module local)

table_name(general)      -> stats_general;
table_name(core)         -> stats_core;
table_name(c2s)          -> stats_c2s;
table_name(mod_privacy)  -> stats_mod_privacy;
table_name(mod_register) -> stats_mod_register;
table_name(mod_roster)   -> stats_mod_roster.

%% Get a list of counters defined for the given module
counters_for(Module) ->
    {Module, Counters} = proplists:lookup(Module, ?COUNTERS_FOR_MODULE),
    Counters.

%% Get the name of the module the given counter is defined for
module_for(Counter) ->
    {Counter, Module} = proplists:lookup(Counter, ?MODULE_FOR_COUNTERS),
    Module.

%%%.

initialize_tables([]) ->
    AllModules = proplists:get_keys(?COUNTERS_FOR_MODULE),
    initialize_tables(AllModules);
initialize_tables(Modules) ->
    lists:foreach(fun initialize_table/1, Modules).

initialize_table(Module) ->
    ets:new(?STATS(Module), [public, named_table]),
    initialize_counters(Module).

initialize_counters(Module) ->
    Counters = counters_for(Module),
    lists:foreach(fun(C) -> ets:insert(?STATS(Module), {C, 0}) end,
                  Counters).

%% Delete a table if it exists
destroy_table(Tab) ->
    case ets:info(Tab) of
    undefined ->
        ok;
    _ ->
        ets:delete(Tab)
    end.

%% Delete all tables possibly used by this module
%% This operation won't error on tables which are not currently used.
destroy_tables() ->
    Tabs = [ ?STATS(Module) ||
        Module <- proplists:get_keys(?COUNTERS_FOR_MODULE)],
    lists:foreach(fun destroy_table/1, Tabs).

-spec is_started() -> boolean().
is_started() ->
    false.

increment_counter(Counter) ->
    update_counter(Counter, 1).

decrement_counter(Counter) ->
    update_counter(Counter, -1).

update_counter(Counter, How) ->
    Tab = ?STATS(module_for(Counter)),
    case ets:info(Tab) of
    undefined ->
        ok;
    _ ->
        ets:update_counter(Tab, Counter, How)
    end.

-spec counter_value(atom()) -> {value, term()}.
counter_value(Counter) ->
    Tab = ?STATS(module_for(Counter)),
    [{Counter, Value}] = ets:lookup(Tab, Counter),
    {value, Value}.

table_value(_,_,_,_) ->
    ok.

%%% vim: set sts=4 ts=4 sw=4 et filetype=erlang foldmarker=%%%',%%%. foldmethod=marker:
