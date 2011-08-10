-module(ejabberd_snmp_core).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("EJABBERD-MIB.hrl").

-export([start/1,
         stop/0,
         is_started/0,
         increment_counter/1,
         decrement_counter/1,
         update_counter/2,
         set_counter/2,
         reset_counters/0,
         increment_window_counter/1,
         update_window_counter/2,
         window_change/0,
         counter_value/1,
         table_value/4]).

%%%' Helper defines (module local)

-define(STATS(Module), table_name(Module)).
-define(STATS_W(Module), table_name_w(Module)).
-define(COUNTERS_FOR_MODULE, [
    {core,         [sessionCount,
                    globalSessionCount,
                    globalUniqueSessionCount,
                    sessionSuccessfulLogins,
                    sessionAuthAnonymous,
                    sessionAuthFails,
                    sessionLogouts]},
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
                    modPrivacyStanzaAll,
                    modPrivacyListLength]} ]).

-define(W_COUNTERS_FOR_MODULE, [
    {core,         [sessionSuccessfulLoginsW,
                    sessionAuthAnonymousW,
                    sessionAuthFailsW,
                    sessionLogoutsW]},
    {c2s,          [xmppMessageSentW,
                    xmppMessageReceivedW,
                    xmppPresenceSentW,
                    xmppPresenceReceivedW,
                    xmppIqSentW,
                    xmppIqReceivedW,
                    xmppStanzaSentW,
                    xmppStanzaReceivedW,
                    xmppErrorTotalW]},
    {mod_roster,   [modRosterSetsW,
                    modRosterGetsW,
                    modPresenceSubscriptionsW,
                    modPresenceUnsubscriptionsW,
                    modRosterPushW]},
    {mod_register, [modRegisterCountW,
                    modUnregisterCountW]},
    {mod_privacy,  [modPrivacySetsW,
                    modPrivacySetsActiveW,
                    modPrivacySetsDefaultW,
                    modPrivacyPushW,
                    modPrivacyGetsW]} ]).


-define(MODULE_FOR_COUNTERS, [
        {sessionCount,               core},
        {globalSessionCount,         core},
        {globalUniqueSessionCount,   core},
        {sessionSuccessfulLogins,    core},
        {sessionAuthAnonymous,       core},
        {sessionAuthFails,           core},
        {sessionLogouts,             core},
        {sessionSuccessfulLoginsW,   core},
        {sessionAuthAnonymousW,      core},
        {sessionAuthFailsW,          core},
        {sessionLogoutsW,            core},
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
        {xmppMessageSentW,            c2s},
        {xmppMessageReceivedW,        c2s},
        {xmppPresenceSentW,           c2s},
        {xmppPresenceReceivedW,       c2s},
        {xmppIqSentW,                 c2s},
        {xmppIqReceivedW,             c2s},
        {xmppStanzaSentW,             c2s},
        {xmppStanzaReceivedW,         c2s},
        {xmppStanzaDeniedW,           c2s},
        {xmppErrorTotalW,             c2s},
        {modRosterSets,              mod_roster},
        {modRosterGets,              mod_roster},
        {modPresenceSubscriptions,   mod_roster},
        {modPresenceUnsubscriptions, mod_roster},
        {modRosterPush,              mod_roster},
        {modRosterSize,              mod_roster},
        {modRosterGroups,            mod_roster},
        {modRosterSetsW,             mod_roster},
        {modRosterGetsW,             mod_roster},
        {modPresenceSubscriptionsW,  mod_roster},
        {modPresenceUnsubscriptionsW,mod_roster},
        {modRosterPushW,              mod_roster},
        {modRegisterCount,           mod_register},
        {modUnregisterCount,         mod_register},
        {modRegisterCountW,          mod_register},
        {modUnregisterCountW,        mod_register},
        {modRegisterUserCount,       mod_register},
        {modPrivacySets,             mod_privacy},
        {modPrivacySetsActive,       mod_privacy},
        {modPrivacySetsDefault,      mod_privacy},
        {modPrivacyPush,             mod_privacy},
        {modPrivacyGets,             mod_privacy},
        {modPrivacyStanzaBlocked,    mod_privacy},
        {modPrivacyStanzaAll,        mod_privacy},
        {modPrivacyListLength,       mod_privacy},
        {modPrivacySetsW,            mod_privacy},
        {modPrivacySetsActiveW,      mod_privacy},
        {modPrivacySetsDefaultW,     mod_privacy},
        {modPrivacyPushW,            mod_privacy},
        {modPrivacyGetsW,            mod_privacy}]).

%%%.

start(Modules) ->
    initialize_tables(Modules).

stop() ->
    destroy_tables(),
    ok.

%%%' Helper functions (module local)

table_name(core)         -> stats_core;
table_name(c2s)          -> stats_c2s;
table_name(mod_privacy)  -> stats_mod_privacy;
table_name(mod_register) -> stats_mod_register;
table_name(mod_roster)   -> stats_mod_roster.

table_name_w(core)         -> stats_w_core;
table_name_w(c2s)          -> stats_w_c2s;
table_name_w(mod_privacy)  -> stats_w_mod_privacy;
table_name_w(mod_register) -> stats_w_mod_register;
table_name_w(mod_roster)   -> stats_w_mod_roster.

%% Get a list of counters defined for the given module
counters_for(Module) ->
    {Module, Counters} = proplists:lookup(Module, ?COUNTERS_FOR_MODULE),
    {Module, WCounters} = proplists:lookup(Module, ?W_COUNTERS_FOR_MODULE),
    Counters ++ WCounters.

w_counters_for(Module) ->
    {Module, Counters} = proplists:lookup(Module, ?W_COUNTERS_FOR_MODULE),
    Counters.


%% Get the name of the module the given counter is defined for
module_for(Counter) ->
    {Counter, Module} = proplists:lookup(Counter, ?MODULE_FOR_COUNTERS),
    Module.

%%%.

initialize_tables([]) ->
    initialize_tables(get_all_modules());
initialize_tables(Modules) ->
    lists:foreach(fun initialize_table/1, Modules).

initialize_table(Module) ->
    ets:new(?STATS(Module), [public, named_table]),
    ets:new(?STATS_W(Module), [public, named_table]),
    initialize_counters(Module).

initialize_counters(Module) ->
    Counters = counters_for(Module),
    lists:foreach(fun(C) -> ets:insert(?STATS(Module), {C, 0}) end,
                  Counters),
    lists:foreach(fun(C) -> ets:insert(?STATS_W(Module), {C, 0}) end,
                  w_counters_for(Module)).

%% Reset all counters for initialized tables
reset_counters() ->
    lists:foreach(
        fun(Module) ->
            Tab = ?STATS(Module),
            case ets:info(Tab) of
            undefined ->
                ok;
            _ ->
                lists:foreach(fun(C) -> ets:insert(Tab, {C, 0}) end,
                              counters_for(Module))
            end
        end,
        get_all_modules()).

%% Moves temporary window values to main counter tables
%% and resets temporary tables
window_change() ->
    lists:foreach(
        fun(Module) ->
            Tab = ?STATS_W(Module),
            case ets:info(Tab) of
            undefined ->
                ok;
            _ ->
                lists:foreach(fun(C) ->
                                      [{C, Value}] = ets:lookup(Tab, C),
                                      set_counter(C, Value),
                                      ets:insert(Tab, {C, 0}) 
                              end,
                              w_counters_for(Module))
            end
        end,
        get_all_modules()).
    

%% Delete a table if it exists
destroy_table(Tab) ->
    case ets:info(Tab) of
    undefined ->
        ok;
    _ ->
        ets:delete(Tab)
    end.

get_all_modules() ->
    proplists:get_keys(?COUNTERS_FOR_MODULE).

get_all_tables() ->
    [ ?STATS(Module) || Module <- get_all_modules() ]
    ++ [ ?STATS_W(Module) || Module <- get_all_modules() ].


%% Delete all tables possibly used by this module
%% This operation won't error on tables which are not currently used.
destroy_tables() ->
    lists:foreach(fun destroy_table/1, get_all_tables()).

-spec is_started() -> boolean().
is_started() ->
    lists:any(
        fun(Tab) ->
            case ets:info(Tab) of
                undefined -> false;
                _ -> true
            end
        end,
        get_all_tables()).

increment_window_counter(Counter) ->
    update_window_counter(Counter, 1).

update_window_counter(Counter, How) ->
    Tab = ?STATS_W(module_for(Counter)),
    case ets:info(Tab) of
        undefined ->
            ok;
        _ ->
            ets:update_counter(Tab, Counter, How)
    end.

increment_counter(Counter) ->
    update_counter(Counter, 1).

decrement_counter(Counter) ->
    update_counter(Counter, {2, -1, 0, 0}).

set_counter(Counter, Value) ->
    modify_counter(Counter, {ets, update_element}, [{2, Value}]).

update_counter(Counter, How) ->
    modify_counter(Counter, {ets, update_counter}, [How]).

modify_counter(Counter, Fun, Args) ->
    Tab = ?STATS(module_for(Counter)),
    case ets:info(Tab) of
        undefined ->
            ok;
        _ ->
            ArgsNew = [ Tab, Counter | Args],
            apply(Fun, ArgsNew)
    end.

-spec counter_value(atom()) -> {value, term()}.
counter_value(generalUptime) ->
    {value, erlang:round(element(1, erlang:statistics(wall_clock))/1000)};
counter_value(generalNodeName) ->
    {value, atom_to_list(node())};
counter_value(Counter) ->
    Tab = ?STATS(module_for(Counter)),
    [{Counter, Value}] = ets:lookup(Tab, Counter),
    {value, Value}.

-spec table_value(atom(), list(), list(), atom()) -> list().
table_value(get, RowInd, Cols, Table) ->
    Row = get_row(RowInd, Table),
    get_cols(Row, Cols, Table);
table_value(get_next,RowInd,Cols,Table) ->
    lists:foldl(fun(Col, Res) ->
                      case {Res, get_next_value(RowInd, Col, Table)} of
                          {{genErr, _}, _} ->
                              Res;
                          {_, {genErr, Err}} ->
                              {genErr, Err};
                          {_, Value} ->
                              [Value| Res]
                      end     
              end,[], Cols);
table_value(_,_,_,_) ->
    ok.

%% gets full row with specified index
get_row([RowInd], routerRegisteredPathsTable) ->
    Routes = mnesia:dirty_all_keys(route),
    case RowInd of
        RInd when RInd > 0, RInd =< length(Routes) ->
            HName = lists:nth(RowInd, Routes),
            H = case HName of
                    N when is_binary(N) -> binary_to_list(N);
                    _ ->
                        HName
                end,
            {{value, H}, {value, length(mnesia:dirty_read(route, HName))}};
        _ ->
            {{noValue, noSuchInstance}, {noValue, noSuchInstance}}
    end.

%% gets column values from specified row row
get_cols(Row, Cols, Table) ->
    Mapping = column_mapping(Cols, get_column_map(Table)),
    lists:map(fun(Col) -> 
                      try element(Col, Row) of
                          Res -> Res
                      catch
                          _:_ ->
                              {noValue, noSuchInstance}
                      end
              end, Mapping).

%% gets value of specified cell in the table
get_cell_value(R, C, Table) ->
    Row = get_row([R], Table),
    [Col] = column_mapping([C], get_column_map(Table)),
    try element(Col, Row) of
        {value, Value} ->
            {[C, R], Value};
        _ ->
            {genErr, C}
    catch
        _:_ ->
            {genErr, Col}
    end.
            

%% gets value of next element (table get_next)
get_next_value(RowInd, Col, Table) ->
    case next_indexes(RowInd, Col, Table) of
        {R, C} ->
            get_cell_value(R, C, Table); 
        endOfTable ->
            endOfTable
    end.


%% finds indexes of next element
next_indexes(RowInd, Col, Table) ->
    case {next_row(RowInd, Table), 
          next_col(Col, get_column_map(Table))} of
        {{[], endOfTable}, _} ->
            endOfTable;
        {{last, _NextR}, {_C, endOfTable}} ->
            endOfTable;
        {{last, NextR}, {_C, NextC}} ->
            {NextR, NextC};
        {{_R, NextR}, {C, _NextC}} ->
            {NextR, C}
    end.

%% finds next column
next_col(0, [H | _T]) ->
    {Col, _} = H, 
    {Col, Col};
next_col(Col, [H | T]) ->
    case H of 
        {Col, _} when length(T) > 0->
            [{Next, _} | _T1] = T,
            {Col, Next};
        {Col, _} ->
            {Col, endOfTable};
        _ ->
            next_col(Col, T)
    end.

%% finds next row index
next_row(RowInd, routerRegisteredPathsTable) ->
    case { RowInd, length(mnesia:dirty_all_keys(route))} of
        {_, L} when L == 0 ->
            {[], endOfTable};
        {[], _L} ->
            {[], 1};
        {[RInd] ,L} when L < RInd; RInd < 0 ->
            {[], 1};
        {[RInd], L} when L == RInd ->
            {last, 1};
        {[RInd], _} ->
            {RInd, RInd +1}
    end.


column_mapping(Cols, Map) ->
    lists:map(fun(Col) -> proplists:get_value(Col, Map) end, Cols).

get_column_map(routerRegisteredPathsTable) ->
    [{?routeTo, 1},
     {?routeNum, 2}].
    

%%% vim: set sts=4 ts=4 sw=4 et filetype=erlang foldmarker=%%%',%%%. foldmethod=marker:
