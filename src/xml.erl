%%%----------------------------------------------------------------------
%%% File    : xml.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : XML utils
%%% Created : 20 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
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

-export([element_to_binary/1,
	 crypt/1, make_text_node/1, remove_cdata/1,
	 remove_subtags/3, get_cdata/1, get_tag_cdata/1,
	 get_attr/2, get_attr_s/2, get_tag_attr/2,
	 get_tag_attr_s/2, get_subtag/2, get_subtag_cdata/2,
	 append_subtags/2, get_path_s/2, start/0,
	 replace_tag_attr/3, to_xmlel/1]).

-include("jlib.hrl").
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
      ok -> ok;
      Err -> ?WARNING_MSG("unable to load xml NIF: ~p", [Err])
    end.

-else.

start() -> ok.

-endif.

%%
-spec(element_to_binary/1 ::
(
  El :: xmlel() | cdata())
    -> binary()
).

element_to_binary(El) ->
    iolist_to_binary(element_to_string(El)).

%%
-spec(element_to_string/1 ::
(
  El :: xmlel() | cdata())
    -> string()
).

element_to_string(El) ->
    case catch element_to_string_nocatch(El) of
      {'EXIT', Reason} -> erlang:error({badxml, El, Reason});
      Result -> Result
    end.

-spec(element_to_string_nocatch/1 ::
(
  El :: xmlel() | cdata())
    -> iolist()
).

element_to_string_nocatch(El) ->
    case El of
      #xmlel{name = Name, attrs = Attrs, children = Els} ->
	  if Els /= [] ->
		 [$<, Name, attrs_to_list(Attrs), $>,
		  [element_to_string_nocatch(E) || E <- Els], $<, $/,
		  Name, $>];
	     true -> [$<, Name, attrs_to_list(Attrs), $/, $>]
	  end;
      %% We do not crypt CDATA binary, but we enclose it in XML CDATA
      {xmlcdata, CData} ->
	  ?ESCAPE_BINARY(CData)
    end.

attrs_to_list(Attrs) -> [attr_to_list(A) || A <- Attrs].

attr_to_list({Name, Value}) ->
    [$\s, Name, $=, $', crypt(Value), $'].

crypt(S) ->
    << <<(case C of
              $& -> <<"&amp;">>;
              $< -> <<"&lt;">>;
              $> -> <<"&gt;">>;
              $" -> <<"&quot;">>;
              $' -> <<"&apos;">>;
              _ -> <<C>>
          end)/binary>>
       || <<C>> <= S >>.

%%
-spec(make_text_node/1 ::
(
  CData :: binary())
    -> binary()
).

make_text_node(CData) ->
    case cdata_need_escape(CData) of
      cdata ->
	  CDATA1 = <<"<![CDATA[">>,
	  CDATA2 = <<"]]>">>,
	  iolist_to_binary([CDATA1, CData, CDATA2]);
      none -> CData;
      {cdata, EndTokens} ->
	  EscapedCData = escape_cdata(CData, EndTokens),
	  iolist_to_binary(EscapedCData)
    end.

cdata_need_escape(CData) ->
    cdata_need_escape(CData, 0, false, []).

cdata_need_escape(<<>>, _, false, _) -> none;
cdata_need_escape(<<>>, _, true, []) -> cdata;
cdata_need_escape(<<>>, _, true, CDataEndTokens) ->
    {cdata, lists:reverse(CDataEndTokens)};
cdata_need_escape(<<$], $], $>, Rest/binary>>,
		  CurrentPosition, _XMLEscape, CDataEndTokens) ->
    NewPosition = CurrentPosition + 3,
    cdata_need_escape(Rest, NewPosition, true,
		      [CurrentPosition + 1 | CDataEndTokens]);
%% Only <, & need to be escaped in XML text node
%% See reference: http://www.w3.org/TR/xml11/#syntax
cdata_need_escape(<<$<, Rest/binary>>, CurrentPosition,
		  _XMLEscape, CDataEndTokens) ->
    cdata_need_escape(Rest, CurrentPosition + 1, true,
		      CDataEndTokens);
cdata_need_escape(<<$&, Rest/binary>>, CurrentPosition,
		  _XMLEscape, CDataEndTokens) ->
    cdata_need_escape(Rest, CurrentPosition + 1, true,
		      CDataEndTokens);
cdata_need_escape(<<_:8, Rest/binary>>, CurrentPosition,
		  XMLEscape, CDataEndTokens) ->
    cdata_need_escape(Rest, CurrentPosition + 1, XMLEscape,
		      CDataEndTokens).

escape_cdata(CData, EndTokens) ->
    escape_cdata(CData, 0, EndTokens, []).

escape_cdata(<<>>, _CurrentPosition, [], Acc) ->
    lists:reverse(Acc);
escape_cdata(Rest, CurrentPosition, [], Acc) ->
    CDATA1 = <<"<![CDATA[">>,
    CDATA2 = <<"]]>">>,
    escape_cdata(<<>>, CurrentPosition, [],
		 [CDATA2, Rest, CDATA1 | Acc]);
escape_cdata(CData, Index, [Pos | Positions], Acc) ->
    CDATA1 = <<"<![CDATA[">>,
    CDATA2 = <<"]]>">>,
    Split = Pos - Index,
    {Part, Rest} = split_binary(CData, Split + 1),
    escape_cdata(Rest, Pos + 1, Positions,
		 [CDATA2, Part, CDATA1 | Acc]).

%%
-spec(remove_cdata_p/1 ::
(
  El :: xmlel() | cdata())
    -> boolean()
).

remove_cdata_p(#xmlel{}) -> true;
remove_cdata_p(_) -> false.

%%
-spec(remove_cdata/1 ::
(
  L :: [xmlel() | cdata()])
    -> [xmlel()]
).

remove_cdata(L) -> [E || E <- L, remove_cdata_p(E)].

-spec(remove_subtags/3 ::
(
  Xmlel :: xmlel(),
  Name  :: binary(),
  Attr  :: attr())
    -> Xmlel :: xmlel()
).

remove_subtags(#xmlel{name = TagName, attrs = TagAttrs, children = Els},
  Name, Attr) ->
    #xmlel{name = TagName, attrs = TagAttrs,
        children = remove_subtags1(Els, [], Name, Attr)}.

%%
-spec(remove_subtags1/4 ::
(
  Els    :: [xmlel() | cdata()],
  NewEls :: [xmlel()],
  Name   :: binary(),
  Attr   :: attr())
    -> NewEls :: [xmlel()]
).

remove_subtags1([], NewEls, _Name, _Attr) ->
    lists:reverse(NewEls);
remove_subtags1([El | Els], NewEls, Name,
		{AttrName, AttrValue} = Attr) ->
    case El of
      #xmlel{name = Name, attrs = Attrs} ->
	  case get_attr(AttrName, Attrs) of
	    false ->
		remove_subtags1(Els, [El | NewEls], Name, Attr);
	    {value, AttrValue} ->
		remove_subtags1(Els, NewEls, Name, Attr);
	    _ -> remove_subtags1(Els, [El | NewEls], Name, Attr)
	  end;
      _ -> remove_subtags1(Els, [El | NewEls], Name, Attr)
    end.

-spec(get_cdata/1 ::
(
  L :: [xmlel() | cdata()])
    -> binary()
).

get_cdata(L) ->
    (iolist_to_binary(get_cdata(L, <<"">>))).

-spec(get_cdata/2 ::
(
  L :: [xmlel() | cdata()],
  S :: binary() | iolist())
    -> binary() | iolist()
).

get_cdata([{xmlcdata, CData} | L], S) ->
     get_cdata(L, [S, CData]);
get_cdata([_ | L], S) -> get_cdata(L, S);
get_cdata([], S) -> S.

-spec(get_tag_cdata/1 ::
(
  Xmlel :: xmlel())
    -> binary()
).

get_tag_cdata(#xmlel{children = Els}) -> get_cdata(Els).

%%
-spec(get_attr/2 ::
(
  AttrName :: binary(),
  Attrs    :: [attr()])
    -> {value, binary()}
     | false
).

get_attr(AttrName, Attrs) ->
    case lists:keysearch(AttrName, 1, Attrs) of
      {value, {_, Val}} -> {value, Val};
      _ -> false
    end.

%%
-spec(get_attr_s/2 ::
(
  AttrName :: binary(),
  Attrs    :: [attr()])
    -> Val :: binary()
).

get_attr_s(AttrName, Attrs) ->
    case lists:keysearch(AttrName, 1, Attrs) of
      {value, {_, Val}} -> Val;
      _ -> <<"">>
    end.

%%
-spec(get_tag_attr/2 ::
(
  AttrName :: binary(),
  Xmlel    :: xmlel())
    -> {value, binary()}
     | false
).

get_tag_attr(AttrName, #xmlel{attrs = Attrs}) ->
    get_attr(AttrName, Attrs).

%%
-spec(get_tag_attr_s/2 ::
(
  AttrName :: binary(),
  Xmlel    :: xmlel())
    -> binary()
).

get_tag_attr_s(AttrName, #xmlel{attrs = Attrs}) ->
    get_attr_s(AttrName, Attrs).

%%
-spec(get_subtag/2 ::
(
  Xmlel :: xmlel(),
  Name  :: binary())
    -> xmlel() | false
).

get_subtag(#xmlel{children = Els}, Name) ->
    get_subtag1(Els, Name).

%%
-spec(get_subtag1/2 ::
(
  Els  :: [xmlel() | cdata()],
  Name :: binary())
    -> xmlel() | false
).

get_subtag1([El | Els], Name) ->
    case El of
      #xmlel{name = Name} -> El;
      _ -> get_subtag1(Els, Name)
    end;
get_subtag1([], _) -> false.

%%
-spec(get_subtag_cdata/2 ::
(
  Tag  :: xmlel(),
  Name :: binary())
    -> binary()
).

get_subtag_cdata(Tag, Name) ->
    case get_subtag(Tag, Name) of
      false -> <<"">>;
      Subtag -> get_tag_cdata(Subtag)
    end.

%%
-spec(append_subtags/2 ::
(
  Xmlel    :: xmlel(),
  SubTags2 :: [xmlel() | cdata()])
    -> Xmlel :: xmlel()
).

append_subtags(#xmlel{name = Name, attrs = Attrs, children = SubTags1}, SubTags2) ->
    #xmlel{name = Name, attrs = Attrs, children = SubTags1 ++ SubTags2}.

%%
-spec(get_path_s/2 ::
(
  El   :: xmlel(),
  Path :: [{elem, Name::binary()}
          |{attr, Name::binary()}
          |cdata])
    -> xmlel()
     | binary()
).

get_path_s(El, []) -> El;
get_path_s(El, [{elem, Name} | Path]) ->
    case get_subtag(El, Name) of
      false -> <<"">>;
      SubEl -> get_path_s(SubEl, Path)
    end;
get_path_s(El, [{attr, Name}]) ->
    get_tag_attr_s(Name, El);
get_path_s(El, [cdata]) -> get_tag_cdata(El).

%%
-spec(replace_tag_attr/3 ::
(
  Name  :: binary(),
  Value :: binary(),
  Xmlel :: xmlel())
    -> Xmlel :: #xmlel{
           name     :: binary(),
           attrs    :: [attr(),...],
           children :: [xmlel() | cdata()]
       }
).

replace_tag_attr(Name, Value, Xmlel) ->
    Xmlel#xmlel{
        attrs = [{Name, Value} | lists:keydelete(Name, 1, Xmlel#xmlel.attrs)]
    }.

-spec to_xmlel(xmlelement() | xmlel()) -> xmlel().

to_xmlel({_, Name, Attrs, Els}) ->
    #xmlel{name = iolist_to_binary(Name),
           attrs = [{iolist_to_binary(K), iolist_to_binary(V)}
                    || {K, V} <- Attrs],
           children = [to_xmlel(El) || El <- Els]};
to_xmlel({xmlcdata, CData}) ->
    {xmlcdata, iolist_to_binary(CData)}.
