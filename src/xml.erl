%%%----------------------------------------------------------------------
%%% File    : xml.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : XML utils
%%% Created : 20 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(xml).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([element_to_string/1, crypt/1, remove_cdata/1, get_cdata/1,
	 get_attr/2, get_attr_s/2]).

element_to_string(El) ->
    case El of
	{xmlelement, Name, Attrs, Els} ->
	    if length(Els) > 0 ->
		    "<" ++ Name ++ attrs_to_string(Attrs) ++ ">" ++
			lists:append(
			  lists:map(fun(E) -> element_to_string(E) end, Els))
			++ "</" ++ Name ++ ">";
	       true ->
		    "<" ++ Name ++ attrs_to_string(Attrs) ++ "/>"
	       end;
	{xmlcdata, CData} -> crypt(CData)
    end.


attrs_to_string(Attrs) ->
    lists:append(lists:map(fun(A) -> attr_to_string(A) end, Attrs)).

attr_to_string({Name, Value}) ->
    " " ++ crypt(Name) ++ "='" ++ crypt(Value) ++ "'".

crypt(S) ->
    lists:reverse(crypt(S, "")).

crypt([$& | S], R) ->
    crypt(S, [$;, $p, $m, $a, $& | R]);
crypt([$< | S], R) ->
    crypt(S, [$;, $t, $l, $& | R]);
crypt([$> | S], R) ->
    crypt(S, [$;, $t, $g, $& | R]);
crypt([$" | S], R) ->
    crypt(S, [$;, $t, $o, $u, $q, $& | R]);
crypt([$' | S], R) ->
    crypt(S, [$;, $s, $o, $p, $a, $& | R]);
crypt([C | S], R) ->
    crypt(S, [C | R]);
crypt([], R) ->
    R.
    

remove_cdata(L) ->
    lists:reverse(remove_cdata(L, [])).

remove_cdata([{xmlelement, Name, Attrs, Els} | L], R) ->
    remove_cdata(L, [{xmlelement, Name, Attrs, Els} | R]);
remove_cdata([{xmlcdata, CData} | L], R) ->
    remove_cdata(L, R);
remove_cdata([], R) ->
    R.

get_cdata(L) ->
    get_cdata(L, "").

get_cdata([{xmlcdata, CData} | L], S) ->
    get_cdata(L, S ++ CData);
get_cdata([_ | L], S) ->
    get_cdata(L, S);
get_cdata([], S) ->
    S.
    

get_attr(AttrName, Attrs) ->
    case lists:keysearch(AttrName, 1, Attrs) of
	{value, {_, Val}} ->
	    {value, Val};
	_ ->
	    false
    end.

get_attr_s(AttrName, Attrs) ->
    case lists:keysearch(AttrName, 1, Attrs) of
	{value, {_, Val}} ->
	    Val;
	_ ->
	    ""
    end.

