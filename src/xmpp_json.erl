%%%----------------------------------------------------------------------
%%% File    : xmpp_json.erl
%%% Author  : Eric Cestari <ecestari@process-one.net>
%%% Purpose : Converts {xmlelement,Name, A, Sub} to/from JSON as per protoxep
%%% Created : 09-20-2010
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

-module(xmpp_json).

-export([to_json/1, from_json/1]).

-include("jlib.hrl").

%%% FROM JSON TO XML

from_json({[{<<"stream">>, _Attr}] = Elems}) ->
    parse_start(Elems);
from_json({Elems}) ->
    {xmlstreamelement, hd(from_json2({Elems}))}.

from_json2({Elems}) ->
    lists:map(fun parse_json/1, Elems).

parse_start([{BinName, {JAttrs}}]) ->
    Name = (BinName),
    {FullName, Attrs} = lists:foldl(fun ({<<"xml">>,
					  {XML}},
					 {N, Attrs}) ->
					    XmlAttrs =
						parse_json_special_attrs(<<"xml">>,
									 XML),
					    {N, lists:merge(Attrs, XmlAttrs)};
					({<<"xmlns">>, {XMLNS}},
					 {N, Attrs}) ->
					    XmlNsAttrs =
						parse_json_special_attrs(<<"xmlns">>,
									 XMLNS),
					    {N, lists:merge(Attrs, XmlNsAttrs)};
					({<<"$$">>, BaseNS}, {N, Attrs}) ->
					    {<<((BaseNS))/binary, ":",
					       N/binary>>,
					     Attrs};
					({Key, Value}, {N, Attrs}) ->
					    {N,
					     [{ib2tol(Key), ib2tol(Value)}
					      | Attrs]}
				    end,
				    {Name, []}, JAttrs),
    {xmlstreamstart, FullName, Attrs}.

parse_json({Name, CData}) when is_binary(CData) ->
    #xmlel{name = (Name), attrs = [],
	   children = [{xmlcdata, CData}]};
parse_json({Name, CDatas}) when is_list(CDatas) ->
    lists:map(fun (CData) ->
		      #xmlel{name = (Name), attrs = [],
			     children = [{xmlcdata, CData}]}
	      end,
	      CDatas);
parse_json({BinName, {JAttrs}}) ->
    Name = (BinName),
    {FullName, Attrs, SubEls} = lists:foldl(fun ({<<"$">>,
						  Cdata},
						 {N, Attrs, _SubEls})
						    when is_binary(Cdata) ->
						    {N, Attrs,
						     [{xmlcdata, Cdata}]};
						({<<"$">>, {Elems}},
						 {N, Attrs, _SubEls}) ->
						    SE =
							lists:map(fun parse_json/1,
								  Elems),
						    {N, Attrs,
						     lists:flatten(SE)};
						({<<"xml">>, {XML}},
						 {N, Attrs, SubEls}) ->
						    XmlAttrs =
							parse_json_special_attrs(<<"xml">>,
										 XML),
						    {N,
						     lists:merge(Attrs,
								 XmlAttrs),
						     SubEls};
						({<<"xmlns">>, {XMLNS}},
						 {N, Attrs, SubEls}) ->
						    XmlNsAttrs =
							parse_json_special_attrs(<<"xmlns">>,
										 XMLNS),
						    {N,
						     lists:merge(Attrs,
								 XmlNsAttrs),
						     SubEls};
						({Key, {[]}},
						 {N, Attrs, SubEls}) ->
						    {N, Attrs,
						     [#xmlel{name = ib2tol(Key),
							     attrs = [],
							     children = []}
						      | SubEls]};
						({Key, Value},
						 {N, Attrs, SubEls}) ->
						    {N,
						     [{(Key), ib2tol(Value)}
						      | Attrs],
						     SubEls}
					    end,
					    {Name, [], []}, JAttrs),
    #xmlel{name = FullName, attrs = Attrs,
	   children = SubEls}.

parse_json_special_attrs(Prefix, XMLNS) ->
    lists:reverse(lists:map(fun ({<<"$">>, Value}) ->
				    {Prefix, ib2tol(Value)};
				({<<"@", NS/binary>>, Value}) ->
				    {<<Prefix/binary, ":", ((NS))/binary>>,
				     ib2tol(Value)}
			    end,
			    XMLNS)).

to_json({xmlstreamelement, XMLElement}) ->
    to_json(XMLElement);
to_json(#xmlel{attrs = [], children = []}) ->
    {[]};
to_json(#xmlel{name = Name, attrs = [],
	       children = [{xmlcdata, Cdata}]}) ->
    {SName, JsonAttrs2} = parse_namespace(Name, []),
    {[{SName, Cdata} | JsonAttrs2]};
to_json({xmlstreamstart, Name, Attrs}) ->
    JsonAttrs = parse_attrs(Attrs),
    {SName, Members2} = parse_namespace(Name, JsonAttrs),
    {[{SName, {Members2}}]};
to_json(#xmlel{name = Name, attrs = Attrs,
	       children = SubEls}) ->
    JsonAttrs = parse_attrs(Attrs),
    Members = case parse_subels(SubEls) of
		[] -> JsonAttrs;
		Elems -> [{<<"$">>, Elems} | JsonAttrs]
	      end,
    {SName, Members2} = parse_namespace(Name, Members),
    {[{SName, {Members2}}]}.

parse_namespace(Name, AttrsList) ->
    {Name, AttrsList}.

parse_subels([{xmlcdata, Cdata}]) -> Cdata;
parse_subels([]) -> [];
parse_subels(SubEls) ->
    {lists:reverse(lists:foldl(fun (#xmlel{name = SName,
					   attrs = [],
					   children = [{xmlcdata, UCdata}]},
				    Acc) ->
				       Cdata = UCdata,
				       Name = SName,
				       case lists:keyfind(Name, 1, Acc) of
					 {Name, PrevCdata}
					     when is_binary(PrevCdata) ->
					     Acc1 = lists:keydelete(Name, 1,
								    Acc),
					     [{Name, [PrevCdata, Cdata]}
					      | Acc1];
					 {Name, CDatas}
					     when is_list(CDatas) ->
					     Acc1 = lists:keydelete(Name, 1,
								    Acc),
					     [{Name,
					       lists:append(CDatas, [Cdata])}
					      | Acc1];
					 _ -> [{Name, Cdata} | Acc]
				       end;
				   (#xmlel{name = SName} = Elem, Acc) ->
				       E = case to_json(Elem) of
					     {[{_, ToKeep}]} -> ToKeep;
					     {[]} = Empty -> Empty
					   end,
				       [{SName, E} | Acc];
				   ({xmlcdata, <<"\n">>}, Acc) -> Acc
			       end,
			       [], SubEls))}.

parse_attrs(XmlAttrs) ->
    {Normal, XMLNS} = lists:foldl(fun ({<<"xmlns">>, NS},
				       {Attrs, XMLNS}) ->
					  {Attrs, [{<<"$">>, NS} | XMLNS]};
				      ({<<"xmlns:", Short/binary>>, NS},
				       {Attrs, XMLNS}) ->
					  AttrName = <<$@, Short/binary>>,
					  {Attrs,
					   [{AttrName, NS}
					    | XMLNS]};
				      ({<<"xml:", Short/binary>>, Val},
				       {Attrs, XMLNS}) ->
					  AttrName = <<$@, Short/binary>>,
					  {[{<<"xml">>,
					     {[{AttrName, Val}]}}
					    | Attrs],
					   XMLNS};
				      ({K, V}, {Attrs, XMLNS}) ->
					  {[{K, V} | Attrs], XMLNS}
				  end,
				  {[], []}, XmlAttrs),
    case XMLNS of
      [{<<"$">>, NS}] -> [{<<"xmlns">>, NS} | Normal];
      [] -> Normal;
      _ -> [{<<"xmlns">>, {XMLNS}} | Normal]
    end.

ib2tol(Bin) when is_binary(Bin) -> Bin;
ib2tol(Integer) when is_integer(Integer) ->
    jlib:integer_to_binary(Integer).

%%
%% Tests
%%  erlc -DTEST web/xmpp_json.erl && erl -pa web/ -run xmpp_json test -run init stop -noshell
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

to_text_value_test() ->
    In = {xmlstreamelement,
	  #xmlel{name = <<"tag">>, attrs = [],
		 children = [{xmlcdata, <<"txt-value">>}]}},
    Out = {[{<<"tag">>, <<"txt-value">>}]},
    ?assertEqual(Out, (to_json(In))),
    ?assertEqual(In, (from_json(Out))).

to_tag_with_recursive_tags_test() ->
    In = {xmlstreamelement,
	  #xmlel{name = <<"tag">>, attrs = [],
		 children =
		     [#xmlel{name = <<"tag2">>, attrs = [],
			     children = [{xmlcdata, <<"txt-value">>}]},
		      #xmlel{name = <<"tag3">>, attrs = [],
			     children =
				 [#xmlel{name = <<"tag4">>, attrs = [],
					 children =
					     [{xmlcdata,
					       <<"txt2-value">>}]}]}]}},
    Out = {[{<<"tag">>,
	     {[{<<"$">>,
		{[{<<"tag2">>, <<"txt-value">>},
		  {<<"tag3">>,
		   {[{<<"$">>,
		      {[{<<"tag4">>, <<"txt2-value">>}]}}]}}]}}]}}]},
    io:format("~n~p", [from_json(Out)]),
    io:format("~n~p", [to_json(In)]),
    ?assertEqual(Out, (to_json(In))),
    ?assertEqual(In, (from_json(Out))).

multiple_text_value_tags_as_array_test() ->
    In = {xmlstreamelement,
	  #xmlel{name = <<"tag">>, attrs = [],
		 children =
		     [#xmlel{name = <<"tag2">>, attrs = [],
			     children = [{xmlcdata, <<"txt-value">>}]},
		      #xmlel{name = <<"tag2">>, attrs = [],
			     children = [{xmlcdata, <<"txt-value2">>}]}]}},
    Out = {[{<<"tag">>,
	     {[{<<"$">>,
		{[{<<"tag2">>,
		   [<<"txt-value">>, <<"txt-value2">>]}]}}]}}]},
    io:format("~p~n", [to_json(In)]),
    io:format("~p~n", [from_json(Out)]),
    ?assertEqual(Out, (to_json(In))),
    ?assertEqual(In, (from_json(Out))).

tag_attr_no_value_test() ->
    In = {xmlstreamelement,
	  #xmlel{name = <<"tag">>,
		 attrs = [{<<"attr">>, <<"attr-value">>}],
		 children = []}},
    Out = {[{<<"tag">>,
	     {[{<<"attr">>, <<"attr-value">>}]}}]},
    io:format("~s", [jiffy:encode(to_json(In))]),
    io:format("~p", [from_json(Out)]),
    ?assertEqual(Out, (to_json(In))),
    ?assertEqual(In, (from_json(Out))).

% 4.2.3.5 Tag with multiple attributes as array, no value
% Not wellformed XML.

% 4.2.3.6 Tags as array with unique attributes, no value

tag_with_namespace_no_value_test() ->
    In = {xmlstreamelement,
	  #xmlel{name = <<"tag">>,
		 attrs = [{<<"xmlns:ns">>, <<"ns-value">>}],
		 children = []}},
    Out = {[{<<"tag">>,
	     {[{<<"xmlns">>,
		{[{<<"@ns">>, <<"ns-value">>}]}}]}}]},
    io:format("~s", [jiffy:encode(to_json(In))]),
    ?assertEqual(Out, (to_json(In))),
    ?assertEqual(In, (from_json(Out))).

two_namespaces_tag_no_value_test() ->
    In = {xmlstreamelement,
	  #xmlel{name = <<"tag">>,
		 attrs =
		     [{<<"xmlns:ns">>, <<"ns-value">>},
		      {<<"xmlns">>, <<"root-value">>}],
		 children = []}},
    Out = {[{<<"tag">>,
	     {[{<<"xmlns">>,
		{[{<<"$">>, <<"root-value">>},
		  {<<"@ns">>, <<"ns-value">>}]}}]}}]},
    io:format("~s", [jiffy:encode(to_json(In))]),
    ?assertEqual(Out, (to_json(In))),
    ?assertEqual(In, (from_json(Out))).

namespaced_tag_no_value_test() ->
    In = {xmlstreamelement,
	  #xmlel{name = <<"ns:tag">>,
		 attrs = [{<<"attr">>, <<"attr-value">>}],
		 children = []}},
    Out = {[{<<"ns:tag">>,
	     {[{<<"attr">>, <<"attr-value">>}]}}]},
    io:format("~s", [jiffy:encode(to_json(In))]),
    ?assertEqual(Out, (to_json(In))),
    ?assertEqual(In, (from_json(Out))).

tag_with_attribute_and_value_test() ->
    In = {xmlstreamelement,
	  #xmlel{name = <<"tag">>,
		 attrs = [{<<"attr">>, <<"attr-value">>}],
		 children = [{xmlcdata, <<"txt-value">>}]}},
    Out = {[{<<"tag">>,
	     {[{<<"$">>, <<"txt-value">>},
	       {<<"attr">>, <<"attr-value">>}]}}]},
    ?assertEqual(Out, (to_json(In))),
    ?assertEqual(In, (from_json(Out))).

namespaced_tag_with_value_test() ->
    In = {xmlstreamelement,
	  #xmlel{name = <<"ns:tag">>,
		 attrs = [{<<"attr">>, <<"attr-value">>}],
		 children = [{xmlcdata, <<"txt-value">>}]}},
    Out = {[{<<"ns:tag">>,
	     {[{<<"$">>, <<"txt-value">>},
	       {<<"attr">>, <<"attr-value">>}]}}]},
    io:format("~s", [jiffy:encode(to_json(In))]),
    ?assertEqual(Out, (to_json(In))),
    ?assertEqual(In, (from_json(Out))).

xml_lang_attr_test() ->
    In = {xmlstreamelement,
	  #xmlel{name = <<"tag">>,
		 attrs = [{<<"xml:lang">>, <<"en">>}], children = []}},
    Out = {[{<<"tag">>,
	     {[{<<"xml">>, {[{<<"@lang">>, <<"en">>}]}}]}}]},
    io:format("~s", [jiffy:encode(to_json(In))]),
    ?assertEqual(Out, (to_json(In))),
    ?assertEqual(In, (from_json(Out))).

xmlns_tag_with_value_test() ->
    Out = {[{<<"response">>,
	     {[{<<"$">>, <<"dXNlcm5hbWU9I">>},
	       {<<"xmlns">>,
		<<"urn:ietf:params:xml:ns:xmpp-sasl">>}]}}]},
    Out2 = {[{<<"response">>,
	      {[{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>},
		{<<"$">>, <<"dXNlcm5hbWU9I">>}]}}]},
    In = {xmlstreamelement,
	  #xmlel{name = <<"response">>,
		 attrs =
		     [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>}],
		 children = [{xmlcdata, <<"dXNlcm5hbWU9I">>}]}},
    io:format("~s", [jiffy:encode(to_json(In))]),
    ?assertEqual(Out, (to_json(In))),
    ?assertEqual(In, (from_json(Out))),
    ?assertEqual(In, (from_json(Out2))).

no_attr_no_value_test() ->
    In = {xmlstreamelement,
	  #xmlel{name = <<"failure">>,
		 attrs =
		     [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>}],
		 children =
		     [#xmlel{name = <<"not-authorized">>, attrs = [],
			     children = []}]}},
    Out = {[{<<"failure">>,
	     {[{<<"$">>,
		{[{<<"not-authorized">>, {[]}}]}},
	       {<<"xmlns">>,
		<<"urn:ietf:params:xml:ns:xmpp-sasl">>}]}}]},
    io:format("~s", [jiffy:encode(to_json(In))]),
    io:format("~p~n", [to_json(In)]),
    io:format("~p~n", [from_json(Out)]),
    ?assertEqual(Out, (to_json(In))),
    ?assertEqual(In, (from_json(Out))).

xmlstream_test() ->
    In = {xmlstreamstart, <<"stream">>,
	  [{<<"xml:lang">>, <<"en">>}]},
    Out = {[{<<"stream">>,
	     {[{<<"xml">>, {[{<<"@lang">>, <<"en">>}]}}]}}]},
    io:format("~s", [jiffy:encode(to_json(In))]),
    ?assertEqual(Out, (to_json(In))),
    ?assertEqual(In, (from_json(Out))).

-endif.
