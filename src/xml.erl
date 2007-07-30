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
	 get_subtag/2, get_subtag_cdata/2,
	 get_path_s/2,
	 replace_tag_attr/3]).

element_to_string(El) ->
    case El of
	{xmlelement, Name, Attrs, Els} ->
	    if
		Els /= [] ->
		    [$<, Name, attrs_to_list(Attrs), $>,
		     [element_to_string(E) || E <- Els],
		     $<, $/, Name, $>];
	       true ->
		    [$<, Name, attrs_to_list(Attrs), $/, $>]
	       end;
	{xmlcdata, CData} when list(CData) ->
	    crypt(CData);
	%% We do not crypt CDATA binary, but we enclose it in XML CDATA
	{xmlcdata, CData} when binary(CData) ->
	    CDATA1 = <<"<![CDATA[">>,
	    CDATA2 = <<"]]>">>,
	    concat_binary([CDATA1, CData, CDATA2])	    
    end.

attrs_to_list(Attrs) ->
    [attr_to_list(A) || A <- Attrs].

attr_to_list({Name, Value}) ->
    [$\s, crypt(Name), $=, $', crypt(Value), $'].

crypt(S) when is_list(S) ->
    [case C of
	 $& -> "&amp;";
	 $< -> "&lt;";
	 $> -> "&gt;";
	 $" -> "&quot;";
	 $' -> "&apos;";
	 _ -> C
     end || C <- S];
crypt(S) when is_binary(S) ->
    crypt(binary_to_list(S)).
	 
remove_cdata_p({xmlelement, Name, Attrs, Els}) -> true;
remove_cdata_p(_) -> false.

remove_cdata(L) -> [E || E <- L, remove_cdata_p(E)].

get_cdata(L) ->
    binary_to_list(list_to_binary(get_cdata(L, ""))).

get_cdata([{xmlcdata, CData} | L], S) ->
    get_cdata(L, [S, CData]);
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

get_subtag_cdata(Tag, Name) ->
    case get_subtag(Tag, Name) of
	false ->
	    "";
	Subtag ->
	    get_tag_cdata(Subtag)
    end.

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


