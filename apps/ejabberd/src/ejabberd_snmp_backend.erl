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
-include("mod_roster.hrl").

-export([privacy_list_length/0,
         roster_size/0]).

%% Internal exports (used below by dispatch/0)
-export([mnesia_privacy_list_length/0,
         mnesia_roster_size/0]).

%% This is one of the gen_mod modules with different backends
-type ejabberd_module() :: atom().

%% Determine backend for Module.
%%
%% This function is based on the assumption that different mod_sth backends
%% have different suffixes, e.g. mod_privacy for mnesia, mod_privacy_odbc
%% for ODBC.
%% Furthermore, they must be present in mnesia table local_config.
%% No module may appear with two or more different backends simultaneously
%% (impossible anyway, but mentioning it can't hurt).
-spec backend(ejabberd_module()) -> mnesia | odbc | none | {error, term()}.
backend(Module) ->
    %% extend if/when more backends appear (see also _1_)
    MnesiaBackend = Module,
    OdbcBackend = list_to_atom(atom_to_list(Module) ++ "_odbc"),
    F = fun() ->
        [{local_config, _, Modules}] =
            mnesia:read(local_config, {modules, <<"localhost">>}),
        Select = fun({Mod,_}, Acc) ->
            %% ASSUMPTION: either mod_something or mod_something_odbc
            %% (or some other backend) is used, never both/all
            case Mod of
                %% _1_ add cases for more backends
                MnesiaBackend -> mnesia;
                OdbcBackend -> odbc;
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

-spec dispatch(mnesia | odbc, atom()) -> term().
dispatch(Backend, Function) ->
    BackendFunction = list_to_atom(atom_to_list(Backend) ++ "_"
        ++ atom_to_list(Function)),
    case Backend of
        mnesia ->
            apply(?MODULE, BackendFunction, []);
        odbc ->
            {error, ?ERR_INTERNAL_SERVER_ERROR};
        _ ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

privacy_list_length() ->
    dispatch(backend(mod_privacy), privacy_list_length).

roster_size() ->
    dispatch(backend(mod_roster), roster_size).

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

mnesia_roster_size() ->
    F = fun() ->
        length(
            mnesia:foldl(fun(#roster{us = User}, Acc) ->
                case lists:member(User, Acc) of
                    false ->
                        [User | Acc];
                    _ ->
                        Acc
                end
            end,
            [], roster))
    end,
    case mnesia:transaction(F) of
        {atomic, 0} ->
            0;
        {atomic, UserCount} ->
            TableSize = mnesia:table_info(roster, size),
            erlang:round(TableSize / UserCount);
        _ ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.
