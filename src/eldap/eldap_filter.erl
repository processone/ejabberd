%%%----------------------------------------------------------------------
%%% File:    eldap_filter.erl
%%% Purpose: Converts String Representation of
%%%            LDAP Search Filter (RFC 2254)
%%%            to eldap's representation of filter
%%% Author:  Evgeniy Khramtsov <xramtsov@gmail.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
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

%%%======================
%%% Export functions
%%%======================

-export([parse/1,
	 parse/2,
	 do_sub/2
	]).

%%%-------------------------------------------------------------------------
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
%%%    > eldap_filter:parse("(&(!(uid<=100))(mail=*))").
%%%
%%%    {ok,{'and',[{'not',{lessOrEqual,{'AttributeValueAssertion',"uid","100"}}},
%%%            {present,"mail"}]}}
%%%-------------------------------------------------------------------------
parse(RFC2254_Filter) ->
    parse(RFC2254_Filter, []).

%%%-------------------------------------------------------------------------
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
%%%--------------------------------------------------------------------------
parse(RFC2254_Filter, ListOfSubValues) ->
    case catch convert_filter(parse_filter(RFC2254_Filter), ListOfSubValues) of
	[EldapFilter] when is_tuple(EldapFilter) ->
	    {ok, EldapFilter};
	{regexp, Error} ->
	    {error, Error};
	_ ->
	    {error, bad_filter}
    end.

%%%==========================
%%% Internal functions
%%%==========================

%%%----------------------
%%% split/1,4
%%%----------------------
split(Filter) ->
    split(Filter, 0, [], []).

split([], _, _, Result) ->
    Result;

split([H|T], Num, Rest, Result) ->
    NewNum = case H of
		 $( -> Num + 1;
		    $) -> Num - 1;
		 _ -> Num
	     end,
    if
	NewNum == 0 ->
	    X = Rest++[H],
	    LenX = length(X),
	    if
		LenX > 2 ->
		    split(T, 0, [], Result ++ [lists:sublist(X, 2, LenX-2)]);
		true ->
		    split(T, 0, Rest, Result)
	    end;
	true ->
	    split(T, NewNum, Rest++[H], Result)
    end.

%%%-----------------------
%%% parse_filter/1
%%%-----------------------
parse_filter(Filter) ->
    case Filter of
	[$! | T] ->
	    {'not', parse_filter(T)};
	[$| | T] ->
	    {'or', parse_filter(T)};
	[$& | T] ->
	    {'and', parse_filter(T)};
	[$( | _] ->
		parse_filter(split(Filter));
	[List | _] when is_list(List) ->
	    [parse_filter(X) || X <- Filter];
	_ ->
	    Filter
    end.

%%%--------------------
%%% convert_filter/2
%%%--------------------
convert_filter({'not', [Val | _]}, Replace) ->
    eldap:'not'(convert_filter(Val, Replace));

convert_filter({'or', Vals}, Replace) ->
    eldap:'or'([convert_filter(X, Replace) || X <- Vals]);

convert_filter({'and', Vals}, Replace) ->
    eldap:'and'([convert_filter(X, Replace) || X <- Vals]);

convert_filter([H|_] = Filter, Replace) when is_integer(H) ->
    parse_attr(Filter, Replace);

convert_filter(Filter, Replace) when is_list(Filter) ->
    [convert_filter(X, Replace) || X <- Filter].

%%%-----------------
%%% parse_attr/2,3
%%%-----------------
parse_attr(Attr, ListOfSubValues) ->
    {Action, [_|_] = Name, [_|_] = Value} = split_attribute(Attr),
    parse_attr(Action, {Name, Value}, ListOfSubValues).

parse_attr(approx, {Name, Value}, ListOfSubValues) ->
    NewValue = do_sub(Value, ListOfSubValues),
    eldap:approxMatch(Name, NewValue);

parse_attr(greater, {Name, Value}, ListOfSubValues) ->
    NewValue = do_sub(Value, ListOfSubValues),
    eldap:greaterOrEqual(Name, NewValue);

parse_attr(less, {Name, Value}, ListOfSubValues) ->
    NewValue = do_sub(Value, ListOfSubValues),
    eldap:lessOrEqual(Name, NewValue);

parse_attr(equal, {Name, Value}, ListOfSubValues) ->
    RegSList = re:split(remove_extra_asterisks(Value), "[*]", [{return, list}]),
    Pattern = case [do_sub(X, ListOfSubValues) || X <- RegSList] of
		  [Head | Tail] when Tail /= [] ->
		      {Head, lists:sublist(Tail, length(Tail)-1), lists:last(Tail)};
		  R ->
		      R
	      end,
    case Pattern of
	[V] ->
	    eldap:equalityMatch(Name, V);
	{[], [], []} ->
	    eldap:present(Name);
	{"", Any, ""} ->
	    eldap:substrings(Name, [{any, X} || X<-Any]);
	{H, Any, ""} ->
	    eldap:substrings(Name, [{initial, H}]++[{any, X} || X<-Any]);
	{"", Any, T} ->
	    eldap:substrings(Name, [{any, X} || X<-Any]++[{final, T}]);
	{H, Any, T} ->
	    eldap:substrings(Name, [{initial, H}]++[{any, X} || X<-Any]++[{final, T}])
    end;

parse_attr(_, _, _) ->
    false.

%%%--------------------
%%% do_sub/2,3
%%%--------------------

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

remove_extra_asterisks(String) ->
    {Res, _} = lists:foldl(
		 fun(X, {Acc, Last}) ->
			 case X of
			     $* when Last==$* ->
				 {Acc, X};
			     _ ->
				 {Acc ++ [X], X}
			 end
		 end,
		 {"", ""}, String),
    Res.

replace_amps(String) ->
	lists:foldl(
		fun(X, Acc) ->
			if
				X == $& ->
					Acc ++ "\\&";
				true ->
					Acc ++ [X]
			end
		end,
	"", String).

split_attribute(String) ->
    split_attribute(String, "", $0).

split_attribute([], _, _) ->
    {error, "", ""};

split_attribute([H|Tail], Acc, Last) ->
    case H of
	$= when Last==$> ->
	    {greater, lists:sublist(Acc, 1, length(Acc)-1), Tail};
	$= when Last==$< ->
	    {less, lists:sublist(Acc, 1, length(Acc)-1), Tail};
	$= when Last==$~ ->
	    {approx, lists:sublist(Acc, 1, length(Acc)-1), Tail};
	$= when Last==$: ->
	    {equal, lists:sublist(Acc, 1, length(Acc)-1), Tail};
	$= ->
	    {equal, Acc, Tail};
	_ ->
	    split_attribute(Tail, Acc++[H], H)
    end.
