%%%----------------------------------------------------------------------
%%% File    : xmpp_json.erl
%%% Author  : Eric Cestari <ecestari@process-one.net>
%%% Purpose : Converts {xmlelement,Name, A, Sub} to/from JSON as per protoxep 
%%% Created : 09-20-2010 
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2010   ProcessOne
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

-module (xmpp_json).

-export([to_json/1, from_json/1]).


  
%%% FROM JSON TO XML

from_json({struct, [{<<"stream">>, _Attr}]=Elems}) ->
  parse_start(Elems);

from_json({struct, Elems}) ->
  {xmlstreamelement, hd(from_json2({struct, Elems}))}.

from_json2({struct, Elems}) ->
      lists:map(fun parse_json/1, Elems).

parse_start([{BinName, {struct, JAttrs}}]) ->
 Name = binary_to_list(BinName),
 {FullName, Attrs} = lists:foldl(
   fun({<<"xml">>, {struct, XML}}, {N, Attrs}) ->
       XmlAttrs = parse_json_special_attrs("xml", XML),
       {N, lists:merge(Attrs, XmlAttrs)};
     ({<<"xmlns">>, {struct, XMLNS}}, {N, Attrs}) ->
       XmlNsAttrs = parse_json_special_attrs("xmlns", XMLNS),
       {N, lists:merge(Attrs, XmlNsAttrs)};
     ({<<"$$">>, BaseNS}, {N, Attrs})->
       {binary_to_list(BaseNS)++":"++N, Attrs};
     ({Key, Value}, {N, Attrs})->
       {N, [{ib2tol(Key), ib2tol(Value)}|Attrs]}
   end, {Name, []}, JAttrs),
 {xmlstreamstart, FullName, Attrs}.
       
parse_json({Name, CData}) when is_binary(CData)->
  {xmlelement, binary_to_list(Name), [], [{xmlcdata, CData}]};

parse_json({Name, CDatas}) when is_list(CDatas)->
  lists:map(fun(CData)->
    {xmlelement, binary_to_list(Name), [], [{xmlcdata, CData}]}
  end, CDatas);

parse_json({BinName, {struct, JAttrs}}) ->
  Name = binary_to_list(BinName),
  {FullName, Attrs, SubEls} = lists:foldl(
    fun({<<"$">>, Cdata}, {N, Attrs, _SubEls}) when is_binary(Cdata)->
        {N, Attrs, [{xmlcdata, Cdata}]};
      ({<<"$">>, {struct, Elems}}, {N, Attrs, _SubEls}) ->
        SE = lists:map(fun parse_json/1, Elems),
        {N, Attrs, lists:flatten(SE)}; % due to 4.2.3.3 
      ({<<"xml">>, {struct, XML}}, {N, Attrs, SubEls}) ->
        XmlAttrs = parse_json_special_attrs("xml", XML),
        {N, lists:merge(Attrs, XmlAttrs), SubEls};
      ({<<"xmlns">>, {struct, XMLNS}}, {N, Attrs, SubEls}) ->
        XmlNsAttrs = parse_json_special_attrs("xmlns", XMLNS),
        {N, lists:merge(Attrs, XmlNsAttrs), SubEls};
      ({Key, {struct, []}}, {N, Attrs, SubEls})->
        {N, Attrs, [{xmlelement, ib2tol(Key), [], []}|SubEls]};
      ({Key, Value}, {N, Attrs, SubEls})->
        {N, [{binary_to_list(Key), ib2tol(Value)}|Attrs], SubEls}
    end, {Name, [], []}, JAttrs),
  {xmlelement, FullName, Attrs, SubEls}.
  
parse_json_special_attrs(Prefix, XMLNS)->
  lists:reverse(lists:map(
  fun({<<"$">>, Value})->
      {Prefix, ib2tol(Value)};
    ({<<"@",NS/binary>>, Value})->
      {Prefix ++ ":"++binary_to_list(NS),  ib2tol(Value)}
  end, XMLNS)).
  
%%% FROM XML TO JSON
to_json({xmlstreamelement, XMLElement})->
  to_json(XMLElement);
to_json({xmlelement, _Name, [], []})->
  {struct, []};
to_json({xmlelement, Name, [], [{xmlcdata, Cdata}]})->
  {SName, JsonAttrs2} = parse_namespace(Name, []),
  {struct, [{SName, Cdata}|JsonAttrs2]};
to_json({xmlstreamstart, Name, Attrs})->
  JsonAttrs = parse_attrs(Attrs),
  {SName, Members2} = parse_namespace(Name, JsonAttrs),
  {struct, [{SName, {struct, Members2}}]};
to_json({xmlelement, Name, Attrs, SubEls})->
  JsonAttrs = parse_attrs(Attrs),
  Members = case parse_subels(SubEls) of
    [] ->
      JsonAttrs;
    [Elem] ->
      [{<<"$">>,Elem}|JsonAttrs];
    Elems ->
      [{<<"$">>,Elems}|JsonAttrs]
  end,
  {SName, Members2} = parse_namespace(Name, Members),
  {struct, [{SName, {struct, Members2}}]}.

parse_namespace(Name, AttrsList)->
  {l2b(Name), AttrsList}.

parse_subels([{xmlcdata, Cdata}])->
  l2b(Cdata);
parse_subels([])->
  [];
parse_subels(SubEls)->
  {struct, lists:reverse(lists:foldl(
  fun({xmlelement, SName, [], [{xmlcdata, UCdata}]}, Acc)->
      Cdata = l2b(UCdata),
      Name = l2b(SName),
      case lists:keyfind(Name, 1, Acc) of
        {Name, PrevCdata} when is_binary(PrevCdata) ->
          Acc1 = lists:keydelete(Name, 1, Acc),
          [{Name,[PrevCdata, Cdata]} | Acc1];
        {Name, CDatas} when is_list(CDatas) ->
          Acc1 = lists:keydelete(Name, 1, Acc),
          [{Name,lists:append(CDatas, [Cdata])} | Acc1];
        _ ->
          [{Name, Cdata}| Acc]
      end;
    ({xmlelement, SName, _, _} = Elem, Acc) ->
      E = case to_json(Elem) of %TODO There could be a better way to iterate
        {struct, [{_, ToKeep}]} -> ToKeep;
        {struct, []} = Empty -> Empty
      end,
      [{l2b(SName), E}|Acc];
    ({xmlcdata,<<"\n">>}, Acc) ->
      Acc
  end,[], SubEls))}.
    
  
parse_attrs(XmlAttrs)->
  {Normal, XMLNS} = lists:foldl(
    fun({"xmlns", NS}, {Attrs, XMLNS}) ->
        {Attrs,[{<<"$">>, l2b(NS)}| XMLNS]};
       ({"xmlns:" ++ Short, NS}, {Attrs, XMLNS})->
         AttrName = iolist_to_binary([<<"@">>,l2b(Short)]),
         {Attrs,[{AttrName, list_to_binary(NS)}| XMLNS]};
       ({"xml:" ++ Short, Val}, {Attrs, XMLNS})-> 
          % TODO currently tolerates only one xml:* attr per element
         AttrName = iolist_to_binary([<<"@">>,l2b(Short)]),
         {[{<<"xml">>,{struct, [{AttrName, l2b(Val)}]}}|Attrs], XMLNS};
       ({K, V}, {Attrs, XMLNS})->
         {[{l2b(K), l2b(V)}|Attrs], XMLNS}
    end,{[], []}, XmlAttrs),
   
  case XMLNS of
    [{<<"$">>, NS}]->
      [{<<"xmlns">>, NS}|Normal];
    []->
      Normal;
    _ ->
      [{<<"xmlns">>,{struct, XMLNS} }| Normal]
  end.

l2b(List) when is_list(List) -> list_to_binary(List);
l2b(Bin) when is_binary(Bin) -> Bin.

ib2tol(Bin) when is_binary(Bin) -> binary_to_list(Bin );
ib2tol(Integer) when is_integer(Integer) -> integer_to_list(Integer);
ib2tol(List) when is_list(List) -> List.

%%
%% Tests
%%  erlc -DTEST web/xmpp_json.erl && erl -pa web/ -run xmpp_json test -run init stop -noshell
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

% 4.2.3.1 Tag with text value
to_text_value_test()->
  In = {xmlstreamelement, {xmlelement, "tag", [], [{xmlcdata, <<"txt-value">>}]}},
  Out = {struct, [{<<"tag">>, <<"txt-value">>}]},
  ?assertEqual(Out, to_json(In)),
  ?assertEqual(In, from_json(Out)).

% 4.2.3.2 Tag with recursive tags
to_tag_with_recursive_tags_test()->
  In = {xmlstreamelement, {xmlelement, "tag", [], 
          [{xmlelement,"tag2",[], [{xmlcdata, <<"txt-value">>}]},
           {xmlelement,"tag3",[], [
                {xmlelement,"tag4",[], [{xmlcdata, <<"txt2-value">>}]}]}]}},
  Out = {struct, [{<<"tag">>, 
            {struct, [{<<"$">>, 
              {struct, [
                {<<"tag2">>,<<"txt-value">>},
                {<<"tag3">>,{struct, [{<<"$">>,{struct, [{<<"tag4">>,<<"txt2-value">>}]}}]}}
              ]}
            }]}
          }]
        },
  %io:format("~n~p", [list_to_binary(mochijson2:encode(to_json(In)))]),
  io:format("~n~p", [from_json(Out)]),
  io:format("~n~p", [to_json(In)]),
  ?assertEqual(Out, to_json(In)),
  ?assertEqual(In, from_json(Out)).
  
% 4.2.3.3 Multiple text value tags as array
multiple_text_value_tags_as_array_test()->
  In = {xmlstreamelement, {xmlelement, "tag", [], [
            {xmlelement,"tag2",[], [
              {xmlcdata, <<"txt-value">>}]},
            {xmlelement,"tag2",[], [
              {xmlcdata, <<"txt-value2">>}]}]}},
  Out = {struct, [{<<"tag">>, 
          {struct, [{<<"$">>, 
           {struct, [{<<"tag2">>,
            [<<"txt-value">>, <<"txt-value2">>]}]}
          }]}
        }]
       },
  io:format("~p~n", [to_json(In)]),
  io:format("~p~n", [from_json(Out)]),
  ?assertEqual(Out, to_json(In)),
  ?assertEqual(In, from_json(Out)).

% 4.2.3.4 Tag with attribute, no value
tag_attr_no_value_test() ->
  In = {xmlstreamelement, {xmlelement, "tag", [{"attr", "attr-value"}], []}},
  Out = {struct, [{<<"tag">>, {struct, [
          {<<"attr">>,<<"attr-value">>}
        ]}}]},
  io:format("~p", [list_to_binary(mochijson2:encode(to_json(In)))]),
  io:format("~p", [from_json(Out)]),
  ?assertEqual(Out, to_json(In)),
  ?assertEqual(In, from_json(Out)).
  
% 4.2.3.5 Tag with multiple attributes as array, no value
% Not wellformed XML.

% 4.2.3.6 Tags as array with unique attributes, no value


% 4.2.3.7 Tag with namespace attribute, no value
tag_with_namespace_no_value_test()->
  In = {xmlstreamelement, {xmlelement, "tag", [{"xmlns:ns", "ns-value"}], []}},
  Out = {struct, [{<<"tag">>, {struct, [
          {<<"xmlns">>,{struct, [{<<"@ns">>, <<"ns-value">>}]}}
        ]}}]},
  io:format("~p", [list_to_binary(mochijson2:encode(to_json(In)))]),
  ?assertEqual(Out, to_json(In)),
  ?assertEqual(In, from_json(Out)).


% 4.2.3.8 Tag with many attributes to namespace, no value
two_namespaces_tag_no_value_test()->
  In = {xmlstreamelement,{xmlelement, "tag", [{"xmlns:ns", "ns-value"}, 
                            {"xmlns", "root-value"}], []}},
  Out = {struct, [{<<"tag">>, {struct, [
          {<<"xmlns">>,{struct, [
              {<<"$">>, <<"root-value">>},
              {<<"@ns">>, <<"ns-value">>}]}}
        ]}}]},
  io:format("~p", [list_to_binary(mochijson2:encode(to_json(In)))]),
  ?assertEqual(Out, to_json(In)),
  ?assertEqual(In, from_json(Out)).

% 4.2.3.9 Tag with namespace attribute, no value
% Removed namespace handling. More complex on both sides.
namespaced_tag_no_value_test()->
  In = {xmlstreamelement,{xmlelement, "ns:tag", [{"attr", "attr-value"}], []}},
  Out = {struct, [{<<"ns:tag">>, {struct, [
          {<<"attr">>,<<"attr-value">>}
        ]}}]},
  io:format("~p", [list_to_binary(mochijson2:encode(to_json(In)))]),
  ?assertEqual(Out, to_json(In)),
  ?assertEqual(In, from_json(Out)).
  
% 4.2.3.10 Tag with attribute and text value
tag_with_attribute_and_value_test()->
  In = {xmlstreamelement,{xmlelement, "tag", [{"attr", "attr-value"}], 
                  [{xmlcdata, <<"txt-value">>}]}},
  Out = {struct, [{<<"tag">>, {struct, [
         {<<"$">>, <<"txt-value">>},
         {<<"attr">>,<<"attr-value">>}
       ]}}]},
  %io:format("~p", [list_to_binary(mochijson2:encode(to_json(In)))]),
  ?assertEqual(Out, to_json(In)),
  ?assertEqual(In, from_json(Out)).
  
% 4.2.3.11 Namespace tag with attribute and text value
% Removed namespace handling. More complex on both sides
namespaced_tag_with_value_test()->
  In = {xmlstreamelement,{xmlelement, "ns:tag", [{"attr", "attr-value"}], [{xmlcdata, <<"txt-value">>}]}},
  Out = {struct, [{<<"ns:tag">>, {struct, [
          {<<"$">>,<<"txt-value">>},
          {<<"attr">>,<<"attr-value">>}
        ]}}]},
  io:format("~p", [list_to_binary(mochijson2:encode(to_json(In)))]),
  ?assertEqual(Out, to_json(In)),
  ?assertEqual(In, from_json(Out)).

xml_lang_attr_test()->
  In = {xmlstreamelement,{xmlelement, "tag", [{"xml:lang", "en"}], []}},
  Out = {struct, [{<<"tag">>, {struct, [
          {<<"xml">>,{struct,[{<<"@lang">>,<<"en">>}]}}
        ]}}]},
  io:format("~p", [list_to_binary(mochijson2:encode(to_json(In)))]),
  ?assertEqual(Out, to_json(In)),
  ?assertEqual(In, from_json(Out)).

xmlns_tag_with_value_test()->
  Out = {struct,[{<<"response">>,
            {struct,[{<<"$">>,<<"dXNlcm5hbWU9I">>},
            {<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>}]}}
           ]},
  Out2 = {struct,[{<<"response">>,
            {struct,[{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>},
            {<<"$">>,<<"dXNlcm5hbWU9I">>}
            ]}}
           ]},
  In = {xmlstreamelement,{xmlelement,"response",
          [{"xmlns","urn:ietf:params:xml:ns:xmpp-sasl"}],
          [{xmlcdata, <<"dXNlcm5hbWU9I">>}]}},
  io:format("~p", [list_to_binary(mochijson2:encode(to_json(In)))]),
  ?assertEqual(Out, to_json(In)),
  ?assertEqual(In, from_json(Out)),
  ?assertEqual(In, from_json(Out2)).

no_attr_no_value_test()->
   In = {xmlstreamelement, {xmlelement,"failure",
                              [{"xmlns","urn:ietf:params:xml:ns:xmpp-sasl"}],
                              [{xmlelement,"not-authorized",[],[]}]}},
    Out = {struct, [{<<"failure">>,{struct, [ 
                {<<"$">>, {struct, [{<<"not-authorized">>, {struct, []}}]}},
                {<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>}
    ]}}]},
    io:format("~p", [list_to_binary(mochijson2:encode(to_json(In)))]),
    io:format("~p~n", [to_json(In)]),
    io:format("~p~n", [from_json(Out)]),
    ?assertEqual(Out, to_json(In)),
    ?assertEqual(In, from_json(Out)).

xmlstream_test()->
  In = {xmlstreamstart, "stream", [{"xml:lang", "en"}]},
  Out = {struct, [{<<"stream">>, {struct, [
          {<<"xml">>,{struct,[{<<"@lang">>,<<"en">>}]}}
        ]}}]},
  io:format("~p", [list_to_binary(mochijson2:encode(to_json(In)))]),
  ?assertEqual(Out, to_json(In)),
  ?assertEqual(In, from_json(Out)).
-endif.