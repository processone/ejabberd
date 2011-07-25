%%%  Copyright (C) 2008  Pablo Polvorin <pablo.polvorin@process-one.net>
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%

%%% @copyright (C) 2008  Pablo Polvorin <pablo.polvorin@process-one.net>
%%% @author Pablo Polvorin <pablo.polvorin@process-one.net>
%%% @author Nicolas Niclausse <nicolas@niclux.org>
%%% @doc functions to manipulate dynamic variables, sort of  Abstract Data Type
%%% @end

%%% created on 2008-08-22
%%% modified by Nicolas Niclausse: Add merge and multi keys/values (new, set)

-module(ts_dynvars).

-export([new/0,new/2, merge/2, lookup/2,lookup/3,set/3,entries/1,map/4]).


-define(IS_DYNVARS(X),is_list(X)).

%% @type dynvar() = {Key::atom(), Value::string()} | [].
%% @type dynvars() = [dynvar()]

%% @spec new() -> [dynvar()]
new() ->
    [].

new(Key, Val) when is_atom(Key)->
    [{Key,Val}];
new(VarNames, Values) when is_list(VarNames),is_list(Values)->
    %% FIXME: check if VarNames is a list of atoms
    case {length(VarNames), length(Values)} of
        {A,A} ->
            lists:zip(VarNames,Values);
        {A,B} when A > B -> % more names than values, use empty values
            lists:zip(VarNames,Values ++ lists:duplicate(A-B,""));
        {C,D} when C < D -> % more values than names, remove unused values
            lists:zip(VarNames,lists:sublist(Values, C))
    end.

%% @spec lookup(Key::atom(), Dynvar::dynvars()) -> {ok,Value::term()} | false
lookup(Key, []) when  is_atom(Key)->
    false;
lookup(Key, DynVars) when ?IS_DYNVARS(DynVars), is_atom(Key)->
    case lists:keysearch(Key,1,DynVars) of
        {value,{Key,Value}} -> {ok,Value};
        false -> false
    end.

%% @doc same as lookup/2, only that if the key isn't present, the default
%%      value is returned instead of returning false.
lookup(Key, DynVars, Default) when ?IS_DYNVARS(DynVars), is_atom(Key)->
    case lookup(Key, DynVars) of
        false -> {ok,Default};
        R -> R
    end.

%% @spec set(Key::atom(), Value::term(), DynVars::dynvars()) -> dynvars()
set(Key,Value,DynVars) when ?IS_DYNVARS(DynVars),is_atom(Key) ->
    merge([{Key, Value}],DynVars);

%% optimization: only one key and one value
set([Key],[Value],DynVars) ->
    set(Key,Value,DynVars);
set([Key],Value,DynVars) -> %% for backward compatibility
    set(Key,Value,DynVars);
%% general case: list of keys and values
set(Keys,Values,DynVars) when ?IS_DYNVARS(DynVars),is_list(Keys),is_list(Values) ->
    merge(new(Keys,Values),DynVars).

entries(DynVars) when ?IS_DYNVARS(DynVars) ->
    DynVars.

%% @spec map(Fun::function(),Key::atom(),Default::term(),DynVars::dynvars())
%%             -> dynvars()
%% @doc The value associated to key Key is replaced with
%%      the result of applying function Fun to its previous value.
%%      If there is no such previous value, Fun is applied to the default
%%      value Default.
%%      map(fun(I) -> I +1 end,b,0,[{a,5}]) => [{a,5},{b,1}]
%%      map(fun(I) -> I +1 end,b,0,[{a,1}]) => [{a,5},{b,2}]
map(Fun,Key,Default,DynVars) when ?IS_DYNVARS(DynVars),is_atom(Key),is_function(Fun,1) ->
    do_map(Fun,Key,Default,DynVars,[]).

do_map(Fun,Key,Default,[],Acc) ->
    [{Key,Fun(Default)}| Acc];

do_map(Fun,Key,_Default,[{Key,Value}|Rest],Acc) ->
    lists:append([[{Key,Fun(Value)}], Rest, Acc]);

do_map(Fun,Key,Default,[H|Rest], Acc) ->
    do_map(Fun,Key,Default,Rest, [H|Acc]).


%% @spec merge(DynVars::dynvars(),DynVars::dynvars()) -> dynvars()
%% @doc merge two set of dynamic variables
merge(DynVars1, DynVars2) when ?IS_DYNVARS(DynVars1),?IS_DYNVARS(DynVars2) ->
    ts_utils:keyumerge(1,DynVars1,DynVars2).
