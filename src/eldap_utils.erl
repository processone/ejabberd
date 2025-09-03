%%%----------------------------------------------------------------------
%%% File    : eldap_utils.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : ejabberd LDAP helper functions
%%% Created : 12 Oct 2006 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(eldap_utils).

-author('mremond@process-one.net').

-export([generate_subfilter/1,
         find_ldap_attrs/2,
         check_filter/1,
         get_ldap_attr/2,
         get_user_part/2,
         make_filter/2,
         get_state/2,
         case_insensitive_match/2,
         decode_octet_string/3,
         uids_domain_subst/2]).

-include("logger.hrl").
-include("eldap.hrl").


%% Generate an 'or' LDAP query on one or several attributes
%% If there is only one attribute
generate_subfilter([UID]) ->
    subfilter(UID);
%% If there is several attributes
generate_subfilter(UIDs) ->
    iolist_to_binary(["(|", [ subfilter(UID) || UID <- UIDs ], ")"]).
%% Subfilter for a single attribute


subfilter({UIDAttr, UIDAttrFormat}) ->
    %% The default UiDAttrFormat is %u
    <<$(, UIDAttr/binary, $=, UIDAttrFormat/binary, $)>>;
%% The default UiDAttrFormat is <<"%u">>
subfilter({UIDAttr}) ->
    <<$(, UIDAttr/binary, $=, "%u)">>.


%% Not tail-recursive, but it is not very terribly.
%% It stops finding on the first not empty value.
-spec find_ldap_attrs([{binary()} | {binary(), binary()}],
                      [{binary(), [binary()]}]) -> <<>> | {binary(), binary()}.

find_ldap_attrs([{Attr} | Rest], Attributes) ->
    find_ldap_attrs([{Attr, <<"%u">>} | Rest], Attributes);
find_ldap_attrs([{Attr, Format} | Rest], Attributes) ->
    case get_ldap_attr(Attr, Attributes) of
        Value when is_binary(Value), Value /= <<>> ->
            {Value, Format};
        _ ->
            find_ldap_attrs(Rest, Attributes)
    end;
find_ldap_attrs([], _) ->
    <<>>.


-spec get_ldap_attr(binary(), [{binary(), [binary()]}]) -> binary().

get_ldap_attr(LDAPAttr, Attributes) ->
    Res = lists:filter(
            fun({Name, _}) ->
                    case_insensitive_match(Name, LDAPAttr)
            end,
            Attributes),
    case Res of
        [{_, [Value | _]}] -> Value;
        _ -> <<>>
    end.


-spec get_user_part(binary(), binary()) -> {ok, binary()} | {error, badmatch}.

get_user_part(String, Pattern) ->
    F = fun(S, P) ->
                First = str:str(P, <<"%u">>),
                TailLength = byte_size(P) - (First + 1),
                str:sub_string(S, First, byte_size(S) - TailLength)
        end,
    case catch F(String, Pattern) of
        {'EXIT', _} ->
            {error, badmatch};
        Result ->
            case catch ejabberd_regexp:replace(Pattern, <<"%u">>, Result) of
                {'EXIT', _} ->
                    {error, badmatch};
                StringRes ->
                    case case_insensitive_match(StringRes, String) of
                        true ->
                            {ok, Result};
                        false ->
                            {error, badmatch}
                    end
            end
    end.


-spec make_filter([{binary(), [binary()]}], [{binary(), binary()}]) -> any().

make_filter(Data, UIDs) ->
    NewUIDs = [ {U,
                 eldap_filter:do_sub(
                   UF, [{<<"%u">>, <<"*%u*">>, 1}])} || {U, UF} <- UIDs ],
    Filter = lists:flatmap(
               fun({Name, [Value | _]}) ->
                       case Name of
                           <<"%u">> when Value /= <<"">> ->
                               case eldap_filter:parse(
                                      generate_subfilter(NewUIDs),
                                      [{<<"%u">>, Value}]) of
                                   {ok, F} -> [F];
                                   _ -> []
                               end;
                           _ when Value /= <<"">> ->
                               [eldap:substrings(
                                  Name,
                                  [{any, Value}])];
                           _ ->
                               []
                       end
               end,
               Data),
    case Filter of
        [F] ->
            F;
        _ ->
            eldap:'and'(Filter)
    end.


check_filter(F) ->
    NewF = iolist_to_binary(F),
    {ok, _} = eldap_filter:parse(NewF),
    NewF.


-spec case_insensitive_match(binary(), binary()) -> boolean().

case_insensitive_match(X, Y) ->
    X1 = str:to_lower(X),
    Y1 = str:to_lower(Y),
    if
        X1 == Y1 -> true;
        true -> false
    end.


get_state(Server, Module) ->
    Proc = gen_mod:get_module_proc(Server, Module),
    gen_server:call(Proc, get_state).


%% From the list of uids attribute:
%% we look from alias domain (%d) and make the substitution
%% with the actual host domain
%% This help when you need to configure many virtual domains.
-spec uids_domain_subst(binary(), [{binary(), binary()}]) ->
          [{binary(), binary()}].

uids_domain_subst(Host, UIDs) ->
    lists:map(fun({U, V}) ->
                      {U, eldap_filter:do_sub(V, [{<<"%d">>, Host}])};
                 (A) -> A
              end,
              UIDs).


%%----------------------------------------
%% Borrowed from asn1rt_ber_bin_v2.erl
%%----------------------------------------

%%% The tag-number for universal types
-define(N_BOOLEAN,           1).
-define(N_INTEGER,           2).
-define(N_BIT_STRING,        3).
-define(N_OCTET_STRING,      4).
-define(N_NULL,              5).
-define(N_OBJECT_IDENTIFIER, 6).
-define(N_OBJECT_DESCRIPTOR, 7).
-define(N_EXTERNAL,          8).
-define(N_REAL,              9).
-define(N_ENUMERATED,        10).
-define(N_EMBEDDED_PDV,      11).
-define(N_SEQUENCE,          16).
-define(N_SET,               17).
-define(N_NumericString,     18).
-define(N_PrintableString,   19).
-define(N_TeletexString,     20).
-define(N_VideotexString,    21).
-define(N_IA5String,         22).
-define(N_UTCTime,           23).
-define(N_GeneralizedTime,   24).
-define(N_GraphicString,     25).
-define(N_VisibleString,     26).
-define(N_GeneralString,     27).
-define(N_UniversalString,   28).
-define(N_BMPString,         30).


decode_octet_string(Buffer, Range, Tags) ->
    %    NewTags = new_tags(HasTag,#tag{class=?UNIVERSAL,number=?N_OCTET_STRING}),
    decode_restricted_string(Buffer, Range, Tags).


decode_restricted_string(Tlv, Range, TagsIn) ->
    Val = match_tags(Tlv, TagsIn),
    Val2 =
        case Val of
            PartList = [_H | _T] ->  % constructed val
                collect_parts(PartList);
            Bin ->
                Bin
        end,
    check_and_convert_restricted_string(Val2, Range).


check_and_convert_restricted_string(Val, Range) ->
    {StrLen, NewVal} = if
                           is_binary(Val) ->
                               {size(Val), Val};
                           true ->
                               {length(Val), list_to_binary(Val)}
                       end,
    case Range of
        [] ->  % No length constraint
            NewVal;
        {Lb, Ub} when StrLen >= Lb, Ub >= StrLen ->  % variable length constraint
            NewVal;
        {{Lb, _Ub}, []} when StrLen >= Lb ->
            NewVal;
        {{Lb, _Ub}, _Ext = [Min | _]} when StrLen >= Lb; StrLen >= Min ->
            NewVal;
        {{Lb1, Ub1}, {Lb2, Ub2}} when StrLen >= Lb1,
                                      StrLen =< Ub1;
                                      StrLen =< Ub2,
                                      StrLen >= Lb2 ->
            NewVal;
        StrLen ->  % fixed length constraint
            NewVal;
        {_, _} ->
            exit({error, {asn1, {length, Range, Val}}});
        _Len when is_integer(_Len) ->
            exit({error, {asn1, {length, Range, Val}}});
        _ ->  % some strange constraint that we don't support yet
            NewVal
    end.


%%----------------------------------------
%% Decode the in buffer to bits
%%----------------------------------------
match_tags({T, V}, [T]) ->
    V;
match_tags({T, V}, [T | Tt]) ->
    match_tags(V, Tt);
match_tags([{T, V}], [T | Tt]) ->
    match_tags(V, Tt);
match_tags(Vlist = [{T, _V} | _], [T]) ->
    Vlist;
match_tags(Tlv, []) ->
    Tlv;
match_tags({Tag, _V}, [T | _Tt]) ->
    {error, {asn1, {wrong_tag, {Tag, T}}}}.


collect_parts(TlvList) ->
    collect_parts(TlvList, []).


collect_parts([{_, L} | Rest], Acc) when is_list(L) ->
    collect_parts(Rest, [collect_parts(L) | Acc]);
collect_parts([{?N_BIT_STRING, <<Unused, Bits/binary>>} | Rest], _Acc) ->
    collect_parts_bit(Rest, [Bits], Unused);
collect_parts([{_T, V} | Rest], Acc) ->
    collect_parts(Rest, [V | Acc]);
collect_parts([], Acc) ->
    list_to_binary(lists:reverse(Acc)).


collect_parts_bit([{?N_BIT_STRING, <<Unused, Bits/binary>>} | Rest], Acc, Uacc) ->
    collect_parts_bit(Rest, [Bits | Acc], Unused + Uacc);
collect_parts_bit([], Acc, Uacc) ->
    list_to_binary([Uacc | lists:reverse(Acc)]).
