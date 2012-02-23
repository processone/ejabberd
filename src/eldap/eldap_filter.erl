%%%----------------------------------------------------------------------
%%% File:    eldap_filter.erl
%%% Purpose: Converts String Representation of
%%%            LDAP Search Filter (RFC 2254)
%%%            to eldap's representation of filter
%%% Author:  Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------
-module(eldap_filter).

-export([parse/1, parse/2, do_sub/2]).

%%====================================================================
%% API
%%====================================================================
%%%-------------------------------------------------------------------
%%% Arity: parse/1
%%% Function: parse(RFC2254_Filter) -> {ok, EldapFilter}   |
%%%                                    {error, bad_filter}
%%%
%%%           RFC2254_Filter = string().
%%%
%%% Description: Converts String Representation of LDAP Search Filter (RFC 2254)
%%%              to eldap's representation of filter.
%%%
%%% Example:
%%%   > eldap_filter:parse("(&(!(uid<=100))(mail=*))").
%%%
%%%   {ok,{'and',[{'not',{lessOrEqual,{'AttributeValueAssertion',"uid","100"}}},
%%%           {present,"mail"}]}}
%%%-------------------------------------------------------------------
parse(L) when is_list(L) ->
    parse(L, []).

%%%-------------------------------------------------------------------
%%% Arity: parse/2
%%% Function: parse(RFC2254_Filter, [SubstValue |...]) ->
%%%                                  {ok, EldapFilter}                 |
%%%                                  {error, bad_filter}               |
%%%                                  {error, bad_regexp}               |
%%%                                  {error, max_substitute_recursion}
%%%
%%%           SubstValue = {RegExp, Value} | {RegExp, Value, N},
%%%           RFC2254_Filter = RegExp = Value = string(),
%%%           N = integer().
%%%
%%% Description: The same as parse/1, but substitutes N or all occurences
%%%              of RegExp with Value *after* parsing.
%%%
%%% Example:
%%%    > eldap_filter:parse(
%%%            "(|(mail=%u@%d)(jid=%u@%d))",
%%%            [{"%u", "xramtsov"},{"%d","gmail.com"}]).
%%%
%%%    {ok,{'or',[{equalityMatch,{'AttributeValueAssertion',
%%%                              "mail",
%%%                              "xramtsov@gmail.com"}},
%%%           {equalityMatch,{'AttributeValueAssertion',
%%%                              "jid",
%%%                              "xramtsov@gmail.com"}}]}}
%%%-------------------------------------------------------------------
parse(L, SList) when is_list(L), is_list(SList) ->
    case catch eldap_filter_yecc:parse(scan(L, SList)) of
        {'EXIT', _} = Err ->
            {error, Err};
	{error, {_, _, Msg}} ->
	    {error, Msg};
	{ok, Result} ->
	    {ok, Result};
	{regexp, Err} ->
	    {error, Err}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
-define(do_scan(L), scan(Rest, [], [{L, 1} | check(Buf, S) ++ Result], L, S)).

scan(L, SList) ->
    scan(L, "", [], undefined, SList).

scan("=*)" ++ Rest, Buf, Result, '(', S) ->
    scan(Rest, [], [{')', 1}, {'=*', 1} | check(Buf, S) ++ Result], ')', S);
scan(":dn" ++ Rest, Buf, Result, '(', S) -> ?do_scan(':dn');
scan(":=" ++ Rest, Buf, Result, '(', S) -> ?do_scan(':=');
scan(":=" ++ Rest, Buf, Result, ':dn', S) -> ?do_scan(':=');
scan(":=" ++ Rest, Buf, Result, ':', S) -> ?do_scan(':=');
scan("~=" ++ Rest, Buf, Result, '(', S) -> ?do_scan('~=');
scan(">=" ++ Rest, Buf, Result, '(', S) -> ?do_scan('>=');
scan("<=" ++ Rest, Buf, Result, '(', S) -> ?do_scan('<=');
scan("="  ++ Rest, Buf, Result, '(', S) -> ?do_scan('=');
scan(":"  ++ Rest, Buf, Result, '(', S) -> ?do_scan(':');
scan(":"  ++ Rest, Buf, Result, ':dn', S) -> ?do_scan(':');
scan("&"  ++ Rest, Buf, Result, '(', S) when Buf=="" -> ?do_scan('&');
scan("|"  ++ Rest, Buf, Result, '(', S) when Buf=="" -> ?do_scan('|');
scan("!"  ++ Rest, Buf, Result, '(', S) when Buf=="" -> ?do_scan('!');
scan("*"  ++ Rest, Buf, Result, '*', S) -> ?do_scan('*');
scan("*"  ++ Rest, Buf, Result, '=', S) -> ?do_scan('*');
scan("("  ++ Rest, Buf, Result, _, S) -> ?do_scan('(');
scan(")"  ++ Rest, Buf, Result, _, S) -> ?do_scan(')');
scan([Letter | Rest], Buf, Result, PreviosAtom, S) ->
    scan(Rest, [Letter|Buf], Result, PreviosAtom, S);
scan([], Buf, Result, _, S) ->
    lists:reverse(check(Buf, S) ++ Result).

check([], _) ->
    [];
check(Buf, S) ->
    [{str, 1, do_sub(lists:reverse(Buf), S)}].

-define(MAX_RECURSION, 100).

do_sub(S, []) ->
    S;

do_sub([], _) ->
    [];

do_sub(S, [{RegExp, New} | T]) ->
    Result = do_sub(S, {RegExp, replace_amps(New)}, 1),
    do_sub(Result, T);

do_sub(S, [{RegExp, New, Times} | T]) ->
    Result = do_sub(S, {RegExp, replace_amps(New), Times}, 1),
    do_sub(Result, T).

do_sub(S, {RegExp, New}, Iter) ->
    try re:replace(S, RegExp, New, [{return, list}]) of
	NewS when NewS == S ->
	    NewS;
	NewS when Iter =< ?MAX_RECURSION ->
	    do_sub(NewS, {RegExp, New}, Iter+1);
	_ when Iter > ?MAX_RECURSION ->
	    throw({regexp, max_substitute_recursion})
    catch
	_:_ ->
	    throw({regexp, bad_regexp})
    end;

do_sub(S, {_, _, N}, _) when N<1 ->
    S;

do_sub(S, {RegExp, New, Times}, Iter) ->
    try re:replace(S, RegExp, New, [{return, list}]) of
	NewS when NewS == S ->
	    NewS;
	NewS when Iter < Times ->
	    do_sub(NewS, {RegExp, New, Times}, Iter+1);
	NewS ->
	    NewS
    catch
	_:_ ->
	    throw({regexp, bad_regexp})
    end.

replace_amps(String) ->
    lists:flatmap(
      fun($&) -> "\\&";
	 (Chr) -> [Chr]
      end, String).
