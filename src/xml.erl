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

-export([element_to_string/1,
	 crypt/1,
	 remove_cdata/1,
	 get_cdata/1, get_tag_cdata/1,
	 get_attr/2, get_attr_s/2,
	 get_tag_attr/2, get_tag_attr_s/2,
	 get_subtag/2,
	 get_path_s/2,
	 replace_tag_attr/3]).

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


%element_to_string2(El) ->
%    lists:flatten(element_to_string21(El)).
%
%element_to_string21(El) ->
%    case El of
%	{xmlelement, Name, Attrs, Els} ->
%	    if length(Els) > 0 ->
%		    [[$< | Name], attrs_to_list(Attrs), ">",
%		     lists:map(fun(E) -> element_to_string21(E) end, Els),
%		     "</", Name, ">"];
%	       true ->
%		    ["<", Name, attrs_to_list(Attrs), "/>"]
%	       end;
%	{xmlcdata, CData} -> crypt(CData)
%    end.
%
%attrs_to_list(Attrs) ->
%    lists:map(fun(A) -> attr_to_list(A) end, Attrs).
%
%attr_to_list({Name, Value}) ->
%    [" ", crypt(Name), "='", crypt(Value), "'"].



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

%crypt1(S) ->
%    lists:flatten([case C of
%		       $& -> "&amp;";
%		       $< -> "&lt;";
%		       $> -> "&gt;";
%		       $" -> "&quot;";
%		       $' -> "&apos;";
%		       _ -> C
%		   end || C <- S]).

%crypt([$& | S]) ->
%    [$&, $a, $m, $p, $; | crypt(S)];
%crypt([$< | S]) ->
%    [$&, $l, $t, $; | crypt(S)];
%crypt([$> | S]) ->
%    [$&, $g, $t, $; | crypt(S)];
%crypt([$" | S]) ->
%    [$&, $q, $u, $o, $t, $; | crypt(S)];
%crypt([$' | S]) ->
%    [$&, $a, $p, $o, $s, $; | crypt(S)];
%crypt([C | S]) ->
%    [C | crypt(S)];
%crypt([]) ->
%    [].

	 

remove_cdata_p({xmlelement, Name, Attrs, Els}) -> true;
remove_cdata_p(_) -> false.

remove_cdata(L) -> [E || E <- L, remove_cdata_p(E)].


%remove_cdata(L) ->
%    lists:reverse(remove_cdata(L, [])).
%
%remove_cdata([{xmlelement, Name, Attrs, Els} | L], R) ->
%    remove_cdata(L, [{xmlelement, Name, Attrs, Els} | R]);
%remove_cdata([{xmlcdata, CData} | L], R) ->
%    remove_cdata(L, R);
%remove_cdata([], R) ->
%    R.

get_cdata(L) ->
    get_cdata(L, "").

get_cdata([{xmlcdata, CData} | L], S) ->
    get_cdata(L, S ++ CData);
get_cdata([_ | L], S) ->
    get_cdata(L, S);
get_cdata([], S) ->
    S.

get_tag_cdata({xmlelement, Name, Attrs, Els}) ->
    get_cdata(Els).

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

get_tag_attr(AttrName, {xmlelement, Name, Attrs, Els}) ->
    get_attr(AttrName, Attrs).

get_tag_attr_s(AttrName, {xmlelement, Name, Attrs, Els}) ->
    get_attr_s(AttrName, Attrs).


get_subtag({xmlelement, _, _, Els}, Name) ->
    get_subtag1(Els, Name).

get_subtag1([El | Els], Name) ->
    case El of
	{xmlelement, Name, _, _} ->
	    El;
	_ ->
	    get_subtag1(Els, Name)
    end;
get_subtag1([], _) ->
    false.


get_path_s(El, []) ->
    El;
get_path_s(El, [{elem, Name} | Path]) ->
    case get_subtag(El, Name) of
	false ->
	    "";
	SubEl ->
	    get_path_s(SubEl, Path)
    end;
get_path_s(El, [{attr, Name}]) ->
    get_tag_attr_s(Name, El);
get_path_s(El, [cdata]) ->
    get_tag_cdata(El).


replace_tag_attr(Attr, Value, {xmlelement, Name, Attrs, Els}) ->
    Attrs1 = lists:keydelete(Attr, 1, Attrs),
    Attrs2 = [{Attr, Value} | Attrs1],
    {xmlelement, Name, Attrs2, Els}.


