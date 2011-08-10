%%%------------------------------------------------------------------------
%%% File:   ejabberd_snmp_backend.erl
%%% Author: Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
%%%         Radoslaw Szymczyszyn <radoslaw.szymczyszyn@erlang-solutions.com>
%%% Description: Backend specific calculations for SNMP counters
%%%
%%% Created: 9 Aug 2011 by <radoslaw.szymczyszyn@erlang-solutions.com>
%%%-----------------------------------------------------------------------
-module(ejabberd_snmp_backend).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-export([privacy_list_length/0]).

-spec privacy_backend() -> mnesia | odbc | none | {error, term()}.
privacy_backend() ->
    F = fun() ->
        [{local_config, _, Modules}] =
            mnesia:read(local_config, {modules, <<"localhost">>}),
        Select = fun({Mod,_}, Acc) ->
            %% ASSUMPTION: either mod_privacy or mod_privacy_odbc is used,
            %%             never both/all
            case Mod of
                mod_privacy -> mnesia;
                mod_privacy_odbc -> odbc;
                _ -> Acc
            end
        end,
        lists:foldl(Select, none, Modules)
    end,
    case mnesia:transaction(F) of
        {atomic, Backend} ->
            Backend;
        _ ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

privacy_list_length() ->
    case privacy_backend() of
        mnesia ->
            mnesia_privacy_list_length();
        odbc ->
            {error, odbc_not_implemented_yet};
        _ ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

mnesia_privacy_list_length() ->
    F = fun() ->
        TotalItemsAndListCount = fun(#privacy{lists = NamedLists}, Acc) ->
            {_Names, Lists} = lists:unzip(NamedLists),
            lists:foldl(fun(ListItems, {TotalItems, ListCount}) ->
                    {TotalItems + length(ListItems), ListCount+1}
                end,
                Acc, Lists)
        end,
        case mnesia:foldl(TotalItemsAndListCount, {0,0}, privacy) of
            {_, 0} ->
                0;
            {TotalItems, ListCount} ->
                erlang:round(TotalItems / ListCount)
        end
    end,
    case mnesia:transaction(F) of
        {atomic, AvgLength} ->
            AvgLength;
        _ ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.
