%%%----------------------------------------------------------------------
%%% File    : xml.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : XML utils
%%% Created : 20 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(xml).
-author('alexey@process-one.net').

-export([element_to_string/1,
	 element_to_binary/1,
	 crypt/1, make_text_node/1,
	 remove_cdata/1,
	 get_cdata/1, get_tag_cdata/1,
	 get_attr/2, get_attr_s/2,
	 get_tag_attr/2, get_tag_attr_s/2,
	 get_subtag/2, get_subtag_cdata/2,
	 append_subtags/2,
	 get_path_s/2,
	 start/0,
	 replace_tag_attr/3]).

-include("ejabberd.hrl").

%% Select at compile time how to escape characters in binary text
%% nodes.
%% Can be choosen with ./configure --enable-full-xml
-ifdef(FULL_XML_SUPPORT).
-define(ESCAPE_BINARY(CData), make_text_node(CData)).
-else.
-define(ESCAPE_BINARY(CData), crypt(CData)).
-endif.

%% Replace element_to_binary/1 with NIF
%% Can be choosen with ./configure --enable-nif
-ifdef(NIF).
start() ->
    SOPath = filename:join(ejabberd:get_so_path(), "xml"),
    case catch erlang:load_nif(SOPath, 0) of
	ok ->
	    ok;
	Err ->
	    ?WARNING_MSG("unable to load xml NIF: ~p", [Err])
    end.
-else.
start() ->
    ok.
-endif.

element_to_binary(El) ->
    iolist_to_binary(element_to_string(El)).

element_to_string(El) ->
    case catch element_to_string_nocatch(El) of
	{'EXIT', Reason} ->
	    erlang:error({badxml, El, Reason});
	Result ->
	    Result
    end.

element_to_string_nocatch(El) ->
    case El of
	{xmlelement, Name, Attrs, Els} ->
	    if
		Els /= [] ->
		    [$<, Name, attrs_to_list(Attrs), $>,
		     [element_to_string_nocatch(E) || E <- Els],
		     $<, $/, Name, $>];
	       true ->
		    [$<, Name, attrs_to_list(Attrs), $/, $>]
	       end;
	%% We do not crypt CDATA binary, but we enclose it in XML CDATA
	{xmlcdata, CData} when is_binary(CData) ->
	    ?ESCAPE_BINARY(CData);
	%% We crypt list and possibly binaries if full XML usage is
	%% disabled unsupported (implies a conversion to list).
	{xmlcdata, CData} ->
	    crypt(CData)
    end.

attrs_to_list(Attrs) ->
    [attr_to_list(A) || A <- Attrs].

attr_to_list({Name, Value}) ->
    [$\s, Name, $=, $', crypt(Value), $'].

crypt(S) when is_list(S) ->
    [case C of
	 $& -> "&amp;";
	 $< -> "&lt;";
	 $> -> "&gt;";
	 $" -> "&quot;";
	 $' -> "&apos;";
         _ when is_list(C); is_binary(C) -> crypt(C);
	 _ -> C
     end || C <- S];
crypt(S) when is_binary(S) ->
    crypt(binary_to_list(S)).

%% Make a cdata_binary depending on what characters it contains
make_text_node(CData) ->
    case cdata_need_escape(CData) of
	cdata ->
	    CDATA1 = <<"<![CDATA[">>,
	    CDATA2 = <<"]]>">>,
	    list_to_binary([CDATA1, CData, CDATA2]);
	none ->
	    CData;
	{cdata, EndTokens} ->
	    EscapedCData = escape_cdata(CData, EndTokens),
	    list_to_binary(EscapedCData)
    end.

%% Returns escape type needed for the text node
%% none, cdata, {cdata, [Positions]}
%% Positions is a list a integer containing positions of CDATA end
%% tokens, so that they can be escaped
cdata_need_escape(CData) ->
    cdata_need_escape(CData, 0, false, []).
cdata_need_escape(<<>>, _, false, _) ->
    none;
cdata_need_escape(<<>>, _, true, []) ->
    cdata;
cdata_need_escape(<<>>, _, true, CDataEndTokens) ->
    {cdata, lists:reverse(CDataEndTokens)};
cdata_need_escape(<<$],$],$>,Rest/binary>>, CurrentPosition,
                  _XMLEscape, CDataEndTokens) ->
    NewPosition = CurrentPosition + 3,
    cdata_need_escape(Rest, NewPosition, true,
                      [CurrentPosition+1|CDataEndTokens]);
%% Only <, & need to be escaped in XML text node
%% See reference: http://www.w3.org/TR/xml11/#syntax
cdata_need_escape(<<$<,Rest/binary>>, CurrentPosition,
                  _XMLEscape, CDataEndTokens) ->
    cdata_need_escape(Rest, CurrentPosition+1, true, CDataEndTokens);
cdata_need_escape(<<$&,Rest/binary>>, CurrentPosition,
                  _XMLEscape, CDataEndTokens) ->
    cdata_need_escape(Rest, CurrentPosition+1, true, CDataEndTokens);
cdata_need_escape(<<_:8,Rest/binary>>, CurrentPosition,
		  XMLEscape, CDataEndTokens) ->
    cdata_need_escape(Rest, CurrentPosition+1, XMLEscape,
                      CDataEndTokens).

%% escape cdata that contain CDATA end tokens
%% EndTokens is a list of position of end tokens (integer)
%% This is supposed to be a very rare case: You need to generate several
%% fields, splitting it in the middle of the end token.
%% See example: http://en.wikipedia.org/wiki/CDATA#Uses_of_CDATA_sections
escape_cdata(CData, EndTokens) ->
    escape_cdata(CData, 0, EndTokens, []).
escape_cdata(<<>>, _CurrentPosition, [], Acc) ->
    lists:reverse(Acc);
escape_cdata(Rest, CurrentPosition, [], Acc) ->
    CDATA1 = <<"<![CDATA[">>,
    CDATA2 = <<"]]>">>,
    escape_cdata(<<>>, CurrentPosition, [], [CDATA2, Rest, CDATA1|Acc]);
escape_cdata(CData, Index, [Pos|Positions], Acc) ->
    CDATA1 = <<"<![CDATA[">>,
    CDATA2 = <<"]]>">>,
    Split = Pos-Index,
    {Part, Rest} = split_binary(CData, Split+1),
    %% Note: We build the list in reverse to optimize construction
    escape_cdata(Rest, Pos+1, Positions, [CDATA2, Part, CDATA1|Acc]).
	 
remove_cdata_p({xmlelement, _Name, _Attrs, _Els}) -> true;
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

get_tag_cdata({xmlelement, _Name, _Attrs, Els}) ->
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

get_tag_attr(AttrName, {xmlelement, _Name, Attrs, _Els}) ->
    get_attr(AttrName, Attrs).

get_tag_attr_s(AttrName, {xmlelement, _Name, Attrs, _Els}) ->
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

append_subtags({xmlelement, Name, Attrs, SubTags1}, SubTags2) ->
    {xmlelement, Name, Attrs, SubTags1 ++ SubTags2}.

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


