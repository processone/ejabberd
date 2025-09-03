%%%----------------------------------------------------------------------
%%% File    : ejabberd_commands_doc.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Management of ejabberd commands
%%% Created : 20 May 2008 by Badlop <badlop@process-one.net>
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

-module(ejabberd_commands_doc).
-author('pawel@process-one.net').

-export([generate_html_output/3]).
-export([generate_md_output/3]).
-export([generate_tags_md/1]).

-include("ejabberd_commands.hrl").

-define(RAW(V), if HTMLOutput -> fxml:crypt(iolist_to_binary(V)); true -> iolist_to_binary(V) end).
-define(TAG_BIN(N), (atom_to_binary(N, latin1)) / binary).
-define(TAG_STR(N), atom_to_list(N)).
-define(TAG(N), if HTMLOutput -> [<<"<", ?TAG_BIN(N), "/>">>]; true -> md_tag(N, <<"">>) end).
-define(TAG(N, V), if HTMLOutput -> [<<"<", ?TAG_BIN(N), ">">>, V, <<"</", ?TAG_BIN(N), ">">>]; true -> md_tag(N, V) end).
-define(TAG(N, C, V), if HTMLOutput -> [<<"<", ?TAG_BIN(N), " class='", C, "'>">>, V, <<"</", ?TAG_BIN(N), ">">>]; true -> md_tag(N, V) end).
-define(TAG_R(N, V), ?TAG(N, ?RAW(V))).
-define(TAG_R(N, C, V), ?TAG(N, C, ?RAW(V))).
-define(SPAN(N, V), ?TAG_R(span, ??N, V)).

-define(STR(A), ?SPAN(str, [<<"\"">>, A, <<"\"">>])).
-define(NUM(A), ?SPAN(num, integer_to_binary(A))).
-define(FIELD(A), ?SPAN(field, A)).
-define(ID(A), ?SPAN(id, A)).
-define(OP(A), ?SPAN(op, A)).
-define(ARG(A), ?FIELD(atom_to_list(A))).
-define(KW(A), ?SPAN(kw, A)).
-define(BR, <<"\n">>).

-define(ARG_S(A), ?STR(atom_to_list(A))).

-define(RAW_L(A), ?RAW(<<A>>)).
-define(STR_L(A), ?STR(<<A>>)).
-define(FIELD_L(A), ?FIELD(<<A>>)).
-define(ID_L(A), ?ID(<<A>>)).
-define(OP_L(A), ?OP(<<A>>)).
-define(KW_L(A), ?KW(<<A>>)).

-define(STR_A(A), ?STR(atom_to_list(A))).
-define(ID_A(A), ?ID(atom_to_list(A))).


list_join_with([], _M) ->
    [];
list_join_with([El | Tail], M) ->
    lists:reverse(lists:foldl(fun(E, Acc) ->
                                      [E, M | Acc]
                              end,
                              [El],
                              Tail)).


md_tag(dt, V) ->
    [<<"- ">>, V];
md_tag(dd, V) ->
    [<<" : ">>, V, <<"\n">>];
md_tag(li, V) ->
    [<<"- ">>, V, <<"\n">>];
md_tag(pre, V) ->
    [V, <<"\n">>];
md_tag(p, V) ->
    [<<"\n\n">>, V, <<"\n">>];
md_tag(h1, V) ->
    [<<"\n\n## ">>, V, <<"\n">>];
md_tag(h2, V) ->
    [<<"\n__">>, V, <<"__\n\n">>];
md_tag(strong, V) ->
    [<<"*">>, V, <<"*">>];
md_tag('div', V) ->
    [<<"*Note* about this command: ">>, V, <<".">>];
md_tag(_, V) ->
    V.


perl_gen({Name, integer}, Int, _Indent, HTMLOutput) ->
    [?ARG(Name), ?OP_L(" => "), ?NUM(Int)];
perl_gen({Name, string}, Str, _Indent, HTMLOutput) ->
    [?ARG(Name), ?OP_L(" => "), ?STR(Str)];
perl_gen({Name, binary}, Str, _Indent, HTMLOutput) ->
    [?ARG(Name), ?OP_L(" => "), ?STR(Str)];
perl_gen({Name, atom}, Atom, _Indent, HTMLOutput) ->
    [?ARG(Name), ?OP_L(" => "), ?STR_A(Atom)];
perl_gen({Name, {tuple, Fields}}, Tuple, Indent, HTMLOutput) ->
    Res = lists:map(fun({A, B}) -> perl_gen(A, B, Indent, HTMLOutput) end, lists:zip(Fields, tuple_to_list(Tuple))),
    [?ARG(Name), ?OP_L(" => {"), list_join_with(Res, [?OP_L(", ")]), ?OP_L("}")];
perl_gen({Name, {list, ElDesc}}, List, Indent, HTMLOutput) ->
    Res = lists:map(fun(E) -> [?OP_L("{"), perl_gen(ElDesc, E, Indent, HTMLOutput), ?OP_L("}")] end, List),
    [?ARG(Name), ?OP_L(" => ["), list_join_with(Res, [?OP_L(", ")]), ?OP_L("]")].


perl_call(Name, ArgsDesc, Values, HTMLOutput) ->
    {Indent, Preamble} = if HTMLOutput -> {<<"">>, []}; true -> {<<"    ">>, <<"~~~ perl\n">>} end,
    [Preamble,
     Indent,
     ?ID_L("XMLRPC::Lite"),
     ?OP_L("->"),
     ?ID_L("proxy"),
     ?OP_L("("),
     ?ID_L("$url"),
     ?OP_L(")->"),
     ?ID_L("call"),
     ?OP_L("("),
     ?STR_A(Name),
     ?OP_L(", {"),
     ?BR,
     Indent,
     <<"  ">>,
     list_join_with(lists:map(fun({A, B}) -> perl_gen(A, B, <<Indent/binary, "  ">>, HTMLOutput) end, lists:zip(ArgsDesc, Values)), [?OP_L(","), ?BR, Indent, <<"  ">>]),
     ?BR,
     Indent,
     ?OP_L("})->"),
     ?ID_L("results"),
     ?OP_L("()")].


java_gen_map(Vals, Indent, HTMLOutput) ->
    {Split, NL} = case Indent of
                      none -> {<<" ">>, <<" ">>};
                      _ -> {[?BR, <<"  ", Indent/binary>>], [?BR, Indent]}
                  end,
    [?KW_L("new "),
     ?ID_L("HashMap"),
     ?OP_L("<"),
     ?ID_L("String"),
     ?OP_L(", "),
     ?ID_L("Object"),
     ?OP_L(">() {{"),
     Split,
     list_join_with(Vals, Split),
     NL,
     ?OP_L("}}")].


java_gen({Name, integer}, Int, _Indent, HTMLOutput) ->
    [?ID_L("put"), ?OP_L("("), ?STR_A(Name), ?OP_L(", "), ?KW_L("new "), ?ID_L("Integer"), ?OP_L("("), ?NUM(Int), ?OP_L("));")];
java_gen({Name, string}, Str, _Indent, HTMLOutput) ->
    [?ID_L("put"), ?OP_L("("), ?STR_A(Name), ?OP_L(", "), ?STR(Str), ?OP_L(");")];
java_gen({Name, binary}, Str, _Indent, HTMLOutput) ->
    [?ID_L("put"), ?OP_L("("), ?STR_A(Name), ?OP_L(", "), ?STR(Str), ?OP_L(");")];
java_gen({Name, atom}, Atom, _Indent, HTMLOutput) ->
    [?ID_L("put"), ?OP_L("("), ?STR_A(Name), ?OP_L(", "), ?STR_A(Atom), ?OP_L(");")];
java_gen({Name, {tuple, Fields}}, Tuple, Indent, HTMLOutput) ->
    NewIndent = <<"  ", Indent/binary>>,
    Res = lists:map(fun({A, B}) -> [java_gen(A, B, NewIndent, HTMLOutput)] end, lists:zip(Fields, tuple_to_list(Tuple))),
    [?ID_L("put"), ?OP_L("("), ?STR_A(Name), ?OP_L(", "), java_gen_map(Res, Indent, HTMLOutput), ?OP_L(")")];
java_gen({Name, {list, ElDesc}}, List, Indent, HTMLOutput) ->
    {NI, NI2, I} = case List of
                       [_] -> {" ", " ", Indent};
                       _ ->
                           {[?BR, <<"    ", Indent/binary>>],
                            [?BR, <<"  ", Indent/binary>>],
                            <<"      ", Indent/binary>>}
                   end,
    Res = lists:map(fun(E) -> java_gen_map([java_gen(ElDesc, E, I, HTMLOutput)], none, HTMLOutput) end, List),
    [?ID_L("put"),
     ?OP_L("("),
     ?STR_A(Name),
     ?OP_L(", "),
     ?KW_L("new "),
     ?ID_L("Object"),
     ?OP_L("[] {"),
     NI,
     list_join_with(Res, [?OP_L(","), NI]),
     NI2,
     ?OP_L("});")].


java_call(Name, ArgsDesc, Values, HTMLOutput) ->
    {Indent, Preamble} = if HTMLOutput -> {<<"">>, []}; true -> {<<"    ">>, <<"~~~ java\n">>} end,
    [Preamble,
     Indent,
     ?ID_L("XmlRpcClientConfigImpl config"),
     ?OP_L(" = "),
     ?KW_L("new "),
     ?ID_L("XmlRpcClientConfigImpl"),
     ?OP_L("();"),
     ?BR,
     Indent,
     ?ID_L("config"),
     ?OP_L("."),
     ?ID_L("setServerURL"),
     ?OP_L("("),
     ?ID_L("url"),
     ?OP_L(");"),
     ?BR,
     Indent,
     ?BR,
     Indent,
     ?ID_L("XmlRpcClient client"),
     ?OP_L(" = "),
     ?KW_L("new "),
     ?ID_L("XmlRpcClient"),
     ?OP_L("();"),
     ?BR,
     Indent,
     ?ID_L("client"),
     ?OP_L("."),
     ?ID_L("setConfig"),
     ?OP_L("("),
     ?ID_L("config"),
     ?OP_L(");"),
     ?BR,
     Indent,
     ?BR,
     Indent,
     ?ID_L("client"),
     ?OP_L("."),
     ?ID_L("execute"),
     ?OP_L("("),
     ?STR_A(Name),
     ?OP_L(", "),
     java_gen_map(lists:map(fun({A, B}) -> java_gen(A, B, Indent, HTMLOutput) end, lists:zip(ArgsDesc, Values)), Indent, HTMLOutput),
     ?OP_L(");")].


-define(XML_S(N, V), ?OP_L("<"), ?FIELD_L(??N), ?OP_L(">"), V).
-define(XML_E(N), ?OP_L("</"), ?FIELD_L(??N), ?OP_L(">")).
-define(XML(N, Indent, V), ?BR, Indent, ?XML_S(N, V), ?BR, Indent, ?XML_E(N)).
-define(XML(N, Indent, D, V), ?XML(N, [Indent, lists:duplicate(D, <<"  ">>)], V)).
-define(XML_L(N, Indent, V), ?BR, Indent, ?XML_S(N, V), ?XML_E(N)).
-define(XML_L(N, Indent, D, V), ?XML_L(N, [Indent, lists:duplicate(D, <<"  ">>)], V)).


xml_gen({Name, integer}, Int, Indent, HTMLOutput) ->
    [?XML(member,
          Indent,
          [?XML_L(name, Indent, 1, ?ID_A(Name)),
           ?XML(value,
                Indent,
                1,
                [?XML_L(integer, Indent, 2, ?ID(integer_to_binary(Int)))])])];
xml_gen({Name, string}, Str, Indent, HTMLOutput) ->
    [?XML(member,
          Indent,
          [?XML_L(name, Indent, 1, ?ID_A(Name)),
           ?XML(value,
                Indent,
                1,
                [?XML_L(string, Indent, 2, ?ID(Str))])])];
xml_gen({Name, binary}, Str, Indent, HTMLOutput) ->
    [?XML(member,
          Indent,
          [?XML_L(name, Indent, 1, ?ID_A(Name)),
           ?XML(value,
                Indent,
                1,
                [?XML_L(string, Indent, 2, ?ID(Str))])])];
xml_gen({Name, atom}, Atom, Indent, HTMLOutput) ->
    [?XML(member,
          Indent,
          [?XML_L(name, Indent, 1, ?ID_A(Name)),
           ?XML(value,
                Indent,
                1,
                [?XML_L(string, Indent, 2, ?ID(atom_to_list(Atom)))])])];
xml_gen({Name, {tuple, Fields}}, Tuple, Indent, HTMLOutput) ->
    NewIndent = <<"    ", Indent/binary>>,
    Res = lists:map(fun({A, B}) -> xml_gen(A, B, NewIndent, HTMLOutput) end, lists:zip(Fields, tuple_to_list(Tuple))),
    [?XML(member,
          Indent,
          [?XML_L(name, Indent, 1, ?ID_A(Name)),
           ?XML(value, Indent, 1, [?XML(struct, NewIndent, Res)])])];
xml_gen({Name, {list, ElDesc}}, List, Indent, HTMLOutput) ->
    Ind1 = <<"        ", Indent/binary>>,
    Ind2 = <<"    ", Ind1/binary>>,
    Res = lists:map(fun(E) -> [?XML(value, Ind1, [?XML(struct, Ind1, 1, xml_gen(ElDesc, E, Ind2, HTMLOutput))])] end, List),
    [?XML(member,
          Indent,
          [?XML_L(name, Indent, 1, ?ID_A(Name)),
           ?XML(value, Indent, 1, [?XML(array, Indent, 2, [?XML(data, Indent, 3, Res)])])])].


xml_call(Name, ArgsDesc, Values, HTMLOutput) ->
    {Indent, Preamble} = if HTMLOutput -> {<<"">>, []}; true -> {<<"    ">>, <<"~~~ xml">>} end,
    Res = lists:map(fun({A, B}) -> xml_gen(A, B, <<Indent/binary, "          ">>, HTMLOutput) end, lists:zip(ArgsDesc, Values)),
    [Preamble,
     ?XML(methodCall,
          Indent,
          [?XML_L(methodName, Indent, 1, ?ID_A(Name)),
           ?XML(params,
                Indent,
                1,
                [?XML(param,
                      Indent,
                      2,
                      [?XML(value,
                            Indent,
                            3,
                            [?XML(struct, Indent, 4, Res)])])])])].


%    [?ARG_S(Name), ?OP_L(": "), ?STR(Str)];
json_gen({_Name, integer}, Int, _Indent, HTMLOutput) ->
    [?NUM(Int)];
json_gen({_Name, string}, Str, _Indent, HTMLOutput) ->
    [?STR(Str)];
json_gen({_Name, binary}, Str, _Indent, HTMLOutput) ->
    [?STR(Str)];
json_gen({_Name, atom}, Atom, _Indent, HTMLOutput) ->
    [?STR_A(Atom)];
json_gen({_Name, rescode}, Val, _Indent, HTMLOutput) ->
    [?ID_A(Val == ok orelse Val == true)];
json_gen({_Name, restuple}, {Val, Str}, _Indent, HTMLOutput) ->
    [?OP_L("{"),
     ?STR_L("res"),
     ?OP_L(": "),
     ?ID_A(Val == ok orelse Val == true),
     ?OP_L(", "),
     ?STR_L("text"),
     ?OP_L(": "),
     ?STR(Str),
     ?OP_L("}")];
json_gen({_Name, {list, {_, {tuple, [{_, atom}, ValFmt]}}}}, List, Indent, HTMLOutput) ->
    Indent2 = <<"  ", Indent/binary>>,
    Res = lists:map(fun({N, V}) -> [?STR_A(N), ?OP_L(": "), json_gen(ValFmt, V, Indent2, HTMLOutput)] end, List),
    [?OP_L("{"), ?BR, Indent2, list_join_with(Res, [?OP_L(","), ?BR, Indent2]), ?BR, Indent, ?OP_L("}")];
json_gen({_Name, {tuple, Fields}}, Tuple, Indent, HTMLOutput) ->
    Indent2 = <<"  ", Indent/binary>>,
    Res = lists:map(fun({{N, _} = A, B}) -> [?STR_A(N), ?OP_L(": "), json_gen(A, B, Indent2, HTMLOutput)] end,
                    lists:zip(Fields, tuple_to_list(Tuple))),
    [?OP_L("{"), ?BR, Indent2, list_join_with(Res, [?OP_L(","), ?BR, Indent2]), ?BR, Indent, ?OP_L("}")];
json_gen({_Name, {list, ElDesc}}, List, Indent, HTMLOutput) ->
    Indent2 = <<"  ", Indent/binary>>,
    Res = lists:map(fun(E) -> json_gen(ElDesc, E, Indent2, HTMLOutput) end, List),
    [?OP_L("["), ?BR, Indent2, list_join_with(Res, [?OP_L(","), ?BR, Indent2]), ?BR, Indent, ?OP_L("]")].


json_call(Name, ArgsDesc, Values, ResultDesc, Result, HTMLOutput) ->
    {Indent, Preamble} = if HTMLOutput -> {<<"">>, []}; true -> {<<"">>, <<"~~~ json\n">>} end,
    {Code, ResultStr} = case {ResultDesc, Result} of
                            {{_, rescode}, V} when V == true; V == ok ->
                                {200, [?STR_L("")]};
                            {{_, rescode}, _} ->
                                {500, [?STR_L("")]};
                            {{_, restuple}, {V1, Text1}} when V1 == true; V1 == ok ->
                                {200, [?STR(Text1)]};
                            {{_, restuple}, {_, Text2}} ->
                                {500, [?STR(Text2)]};
                            {{_, _}, _} ->
                                {200, json_gen(ResultDesc, Result, Indent, HTMLOutput)}
                        end,
    CodeStr = case Code of
                  200 -> <<" 200 OK">>;
                  500 -> <<" 500 Internal Server Error">>
              end,
    [Preamble,
     Indent,
     ?ID_L("POST /api/"),
     ?ID_A(Name),
     ?BR,
     Indent,
     ?OP_L("{"),
     ?BR,
     Indent,
     <<"  ">>,
     list_join_with(lists:map(fun({{N, _} = A, B}) -> [?STR_A(N), ?OP_L(": "), json_gen(A, B, <<Indent/binary, "  ">>, HTMLOutput)] end,
                              lists:zip(ArgsDesc, Values)),
                    [?OP_L(","), ?BR, Indent, <<"  ">>]),
     ?BR,
     Indent,
     ?OP_L("}"),
     ?BR,
     Indent,
     ?BR,
     Indent,
     ?ID_L("HTTP/1.1"),
     ?ID(CodeStr),
     ?BR,
     Indent,
     ResultStr].


generate_example_input({_Name, integer}, {LastStr, LastNum}) ->
    {LastNum + 1, {LastStr, LastNum + 1}};
generate_example_input({_Name, string}, {LastStr, LastNum}) ->
    {string:chars(LastStr + 1, 5), {LastStr + 1, LastNum}};
generate_example_input({_Name, binary}, {LastStr, LastNum}) ->
    {iolist_to_binary(string:chars(LastStr + 1, 5)), {LastStr + 1, LastNum}};
generate_example_input({_Name, atom}, {LastStr, LastNum}) ->
    {list_to_atom(string:chars(LastStr + 1, 5)), {LastStr + 1, LastNum}};
generate_example_input({_Name, rescode}, {LastStr, LastNum}) ->
    {ok, {LastStr, LastNum}};
generate_example_input({_Name, restuple}, {LastStr, LastNum}) ->
    {{ok, <<"Success">>}, {LastStr, LastNum}};
generate_example_input({_Name, {tuple, Fields}}, Data) ->
    {R, D} = lists:foldl(fun(Field, {Res2, Data2}) ->
                                 {Res3, Data3} = generate_example_input(Field, Data2),
                                 {[Res3 | Res2], Data3}
                         end,
                         {[], Data},
                         Fields),
    {list_to_tuple(lists:reverse(R)), D};
generate_example_input({_Name, {list, Desc}}, Data) ->
    {R1, D1} = generate_example_input(Desc, Data),
    {R2, D2} = generate_example_input(Desc, D1),
    {[R1, R2], D2}.


gen_calls(#ejabberd_commands{args_example = none, args = ArgsDesc} = C, HTMLOutput, Langs) ->
    {R, _} = lists:foldl(fun(Arg, {Res, Data}) ->
                                 {Res3, Data3} = generate_example_input(Arg, Data),
                                 {[Res3 | Res], Data3}
                         end,
                         {[], {$a - 1, 0}},
                         ArgsDesc),
    gen_calls(C#ejabberd_commands{args_example = lists:reverse(R)}, HTMLOutput, Langs);
gen_calls(#ejabberd_commands{result_example = none, result = ResultDesc} = C, HTMLOutput, Langs) ->
    {R, _} = generate_example_input(ResultDesc, {$a - 1, 0}),
    gen_calls(C#ejabberd_commands{result_example = R}, HTMLOutput, Langs);
gen_calls(#ejabberd_commands{
            args_example = Values,
            args = ArgsDesc,
            result_example = Result,
            result = ResultDesc,
            name = Name
           },
          HTMLOutput,
          Langs) ->
    Perl = perl_call(Name, ArgsDesc, Values, HTMLOutput),
    Java = java_call(Name, ArgsDesc, Values, HTMLOutput),
    XML = xml_call(Name, ArgsDesc, Values, HTMLOutput),
    JSON = json_call(Name, ArgsDesc, Values, ResultDesc, Result, HTMLOutput),
    if
        HTMLOutput ->
            [?TAG(ul,
                  "code-samples-names",
                  [case lists:member(<<"java">>, Langs) of true -> ?TAG(li, <<"Java">>); _ -> [] end,
                   case lists:member(<<"perl">>, Langs) of true -> ?TAG(li, <<"Perl">>); _ -> [] end,
                   case lists:member(<<"xmlrpc">>, Langs) of true -> ?TAG(li, <<"XML">>); _ -> [] end,
                   case lists:member(<<"json">>, Langs) of true -> ?TAG(li, <<"JSON">>); _ -> [] end]),
             ?TAG(ul,
                  "code-samples",
                  [case lists:member(<<"java">>, Langs) of true -> ?TAG(li, ?TAG(pre, Java)); _ -> [] end,
                   case lists:member(<<"perl">>, Langs) of true -> ?TAG(li, ?TAG(pre, Perl)); _ -> [] end,
                   case lists:member(<<"xmlrpc">>, Langs) of true -> ?TAG(li, ?TAG(pre, XML)); _ -> [] end,
                   case lists:member(<<"json">>, Langs) of true -> ?TAG(li, ?TAG(pre, JSON)); _ -> [] end])];
        true ->
            case Langs of
                Val when length(Val) == 0 orelse length(Val) == 1 ->
                    [case lists:member(<<"java">>, Langs) of true -> [<<"\n">>, ?TAG(pre, Java), <<"~~~\n">>]; _ -> [] end,
                     case lists:member(<<"perl">>, Langs) of true -> [<<"\n">>, ?TAG(pre, Perl), <<"~~~\n">>]; _ -> [] end,
                     case lists:member(<<"xmlrpc">>, Langs) of true -> [<<"\n">>, ?TAG(pre, XML), <<"~~~\n">>]; _ -> [] end,
                     case lists:member(<<"json">>, Langs) of true -> [<<"\n">>, ?TAG(pre, JSON), <<"~~~\n">>]; _ -> [] end,
                     <<"\n\n">>];
                _ ->
                    [<<"\n">>,
                     case lists:member(<<"java">>, Langs) of true -> <<"* Java\n">>; _ -> [] end,
                     case lists:member(<<"perl">>, Langs) of true -> <<"* Perl\n">>; _ -> [] end,
                     case lists:member(<<"xmlrpc">>, Langs) of true -> <<"* XmlRPC\n">>; _ -> [] end,
                     case lists:member(<<"json">>, Langs) of true -> <<"* JSON\n">>; _ -> [] end,
                     <<"{: .code-samples-labels}\n">>,
                     case lists:member(<<"java">>, Langs) of true -> [<<"\n* ">>, ?TAG(pre, Java), <<"~~~\n">>]; _ -> [] end,
                     case lists:member(<<"perl">>, Langs) of true -> [<<"\n* ">>, ?TAG(pre, Perl), <<"~~~\n">>]; _ -> [] end,
                     case lists:member(<<"xmlrpc">>, Langs) of true -> [<<"\n* ">>, ?TAG(pre, XML), <<"~~~\n">>]; _ -> [] end,
                     case lists:member(<<"json">>, Langs) of true -> [<<"\n* ">>, ?TAG(pre, JSON), <<"~~~\n">>]; _ -> [] end,
                     <<"{: .code-samples-tabs}\n\n">>]
            end
    end.


format_type({list, {_, {tuple, Els}}}) ->
    io_lib:format("[~ts]", [format_type({tuple, Els})]);
format_type({list, El}) ->
    io_lib:format("[~ts]", [format_type(El)]);
format_type({tuple, Els}) ->
    Args = [ format_type(El) || El <- Els ],
    io_lib:format("{~ts}", [string:join(Args, ", ")]);
format_type({Name, Type}) ->
    io_lib:format("~ts::~ts", [Name, format_type(Type)]);
format_type(binary) ->
    "string";
format_type(atom) ->
    "string";
format_type(Type) ->
    io_lib:format("~p", [Type]).


gen_param(Name, Type, undefined, HTMLOutput) ->
    [?TAG(li, [?TAG_R(strong, atom_to_list(Name)), <<" :: ">>, ?RAW(format_type(Type))])];
gen_param(Name, Type, Desc, HTMLOutput) ->
    [?TAG(dt, [?TAG_R(strong, atom_to_list(Name)), <<" :: ">>, ?RAW(format_type(Type))]),
     ?TAG(dd, ?RAW(Desc))].


make_tags(HTMLOutput) ->
    TagsList = ejabberd_commands:get_tags_commands(1000000),
    lists:map(fun(T) -> gen_tags(T, HTMLOutput) end, TagsList).


-dialyzer({no_match, gen_tags/2}).


gen_tags({TagName, Commands}, HTMLOutput) ->
    [?TAG(h1, TagName) | [ ?TAG(p, ?RAW("* _`" ++ C ++ "`_")) || C <- Commands ]].


gen_doc(#ejabberd_commands{
          name = Name,
          tags = Tags,
          desc = Desc,
          longdesc = LongDesc,
          args = Args,
          args_desc = ArgsDesc,
          note = Note,
          definer = Definer,
          result = Result,
          result_desc = ResultDesc
         } = Cmd,
        HTMLOutput,
        Langs) ->
    try
        ArgsText = case ArgsDesc of
                       none ->
                           [?TAG(ul,
                                 "args-list",
                                 [ gen_param(AName, Type, undefined, HTMLOutput)
                                   || {AName, Type} <- Args ])];
                       _ ->
                           [?TAG(dl,
                                 "args-list",
                                 [ gen_param(AName, Type, ADesc, HTMLOutput)
                                   || {{AName, Type}, ADesc} <- lists:zip(Args, ArgsDesc) ])]
                   end,
        ResultText = case Result of
                         {res, rescode} ->
                             [?TAG(dl,
                                   [gen_param(res,
                                              integer,
                                              "Status code (`0` on success, `1` otherwise)",
                                              HTMLOutput)])];
                         {res, restuple} ->
                             [?TAG(dl,
                                   [gen_param(res,
                                              string,
                                              "Raw result string",
                                              HTMLOutput)])];
                         {RName, Type} ->
                             case ResultDesc of
                                 none ->
                                     [?TAG(ul, [gen_param(RName, Type, undefined, HTMLOutput)])];
                                 _ ->
                                     [?TAG(dl, [gen_param(RName, Type, ResultDesc, HTMLOutput)])]
                             end
                     end,
        TagsText = ?RAW(string:join([ "_`" ++ atom_to_list(Tag) ++ "`_" || Tag <- Tags ], ", ")),
        IsDefinerMod = case Definer of
                           unknown -> false;
                           _ -> lists:member(gen_mod, lists:flatten(proplists:get_all_values(behaviour, Definer:module_info(attributes))))
                       end,
        ModuleText = case IsDefinerMod of
                         true ->
                             [?TAG(h2, <<"Module:">>), ?TAG(p, ?RAW("_`" ++ atom_to_list(Definer) ++ "`_"))];
                         false ->
                             []
                     end,
        NoteEl = case Note of
                     "" -> [];
                     _ -> ?TAG('div', "note-down", ?RAW(Note))
                 end,
        {NotePre, NotePost} =
            if
                HTMLOutput -> {[], NoteEl};
                true -> {NoteEl, []}
            end,

        [?TAG(h1, make_command_name(Name, Note)),
         NotePre,
         ?TAG(p, ?RAW(Desc)),
         case LongDesc of
             "" -> [];
             _ -> ?TAG(p, ?RAW(LongDesc))
         end,
         NotePost,
         ?TAG(h2, <<"Arguments:">>),
         ArgsText,
         ?TAG(h2, <<"Result:">>),
         ResultText,
         ?TAG(h2, <<"Tags:">>),
         ?TAG(p, TagsText)] ++
        ModuleText ++ [?TAG(h2, <<"Examples:">>), gen_calls(Cmd, HTMLOutput, Langs)]
    catch
        _:Ex ->
            throw(iolist_to_binary(io_lib:format(
                                     <<"Error when generating documentation for command '~p': ~p">>,
                                     [Name, Ex])))
    end.


get_version_mark("") ->
    "";
get_version_mark(Note) ->
    [XX, YY | _] = string:tokens(binary_to_list(ejabberd_option:version()), "."),
    XXYY = string:join([XX, YY], "."),
    case string:find(Note, XXYY) of
        nomatch -> "";
        _ -> " 🟤"
    end.


make_command_name(Name, Note) ->
    atom_to_list(Name) ++ get_version_mark(Note).


find_commands_definitions() ->
    lists:flatmap(
      fun(Mod) ->
              code:ensure_loaded(Mod),
              Cs = case erlang:function_exported(Mod, get_commands_spec, 0) of
                       true ->
                           apply(Mod, get_commands_spec, []);
                       _ ->
                           []
                   end,
              [ C#ejabberd_commands{definer = Mod} || C <- Cs ]
      end,
      ejabberd_config:beams(all)).


generate_html_output(File, RegExp, Languages) ->
    Cmds = find_commands_definitions(),
    {ok, RE} = re:compile(RegExp),
    Cmds2 = lists:filter(fun(#ejabberd_commands{name = Name, module = Module}) ->
                                 re:run(atom_to_list(Name), RE, [{capture, none}]) == match orelse
                                 re:run(atom_to_list(Module), RE, [{capture, none}]) == match
                         end,
                         Cmds),
    Cmds3 = lists:sort(fun(#ejabberd_commands{name = N1}, #ejabberd_commands{name = N2}) ->
                               N1 =< N2
                       end,
                       Cmds2),
    Cmds4 = [ maybe_add_policy_arguments(Cmd) || Cmd <- Cmds3 ],
    Langs = binary:split(Languages, <<",">>, [global]),
    Out = lists:map(fun(C) -> gen_doc(C, true, Langs) end, Cmds4),
    {ok, Fh} = file:open(File, [write]),
    io:format(Fh, "~ts", [[html_pre(), Out, html_post()]]),
    file:close(Fh),
    ok.


maybe_add_policy_arguments(#ejabberd_commands{args = Args1, policy = user} = Cmd) ->
    Args2 = [{user, binary}, {host, binary} | Args1],
    Cmd#ejabberd_commands{args = Args2};
maybe_add_policy_arguments(Cmd) ->
    Cmd.


generate_md_output(File, <<"runtime">>, Languages) ->
    Cmds = lists:map(fun({N, _, _}) ->
                             ejabberd_commands:get_command_definition(N)
                     end,
                     ejabberd_commands:list_commands()),
    generate_md_output(File, <<".">>, Languages, Cmds);
generate_md_output(File, RegExp, Languages) ->
    Cmds = find_commands_definitions(),
    generate_md_output(File, RegExp, Languages, Cmds).


generate_md_output(File, RegExp, Languages, Cmds) ->
    {ok, RE} = re:compile(RegExp),
    Cmds2 = lists:filter(fun(#ejabberd_commands{name = Name, module = Module}) ->
                                 re:run(atom_to_list(Name), RE, [{capture, none}]) == match orelse
                                 re:run(atom_to_list(Module), RE, [{capture, none}]) == match
                         end,
                         Cmds),
    Cmds3 = lists:sort(fun(#ejabberd_commands{name = N1}, #ejabberd_commands{name = N2}) ->
                               N1 =< N2
                       end,
                       Cmds2),
    Cmds4 = [ maybe_add_policy_arguments(Cmd) || Cmd <- Cmds3 ],
    Langs = binary:split(Languages, <<",">>, [global]),
    Version = binary_to_list(ejabberd_config:version()),
    Header = ["# API Reference\n\n"
              "This section describes API commands of ejabberd ",
              Version,
              ". "
              "The commands that changed in this version are marked with 🟤.\n\n"],
    Out = lists:map(fun(C) -> gen_doc(C, false, Langs) end, Cmds4),
    {ok, Fh} = file:open(File, [write, {encoding, utf8}]),
    io:format(Fh, "~ts~ts", [Header, Out]),
    file:close(Fh),
    ok.


generate_tags_md(File) ->
    Version = binary_to_list(ejabberd_config:version()),
    Header = ["# API Tags\n\n"
              "This section enumerates the API tags of ejabberd ",
              Version,
              ". \n\n"],
    Tags = make_tags(false),
    {ok, Fh} = file:open(File, [write, {encoding, utf8}]),
    io:format(Fh, "~ts~ts", [Header, Tags]),
    file:close(Fh),
    ok.


html_pre() ->
    "<!DOCTYPE>
<html>
  <head>
    <meta http-equiv='content-type' content='text/html; charset=utf-8' />
  <style>
    body {
      margin: 0 auto;
      font-family: Georgia, Palatino, serif;
      color: #000;
      line-height: 1;
      max-width: 80%;
      padding: 10px;
    }
    h1, h2, h3, h4 {
      color: #111111;
      font-weight: 400;
    }
    h1, h2, h3, h4, h5, p {
      margin-bottom: 24px;
      padding: 0;
    }
    h1 {
      margin-top: 80px;
      font-size: 36px;
    }
    h2 {
      font-size: 24px;
      margin: 24px 0 6px;
    }
    ul, ol {
      padding: 0;
      margin: 0;
    }
    li {
      line-height: 24px;
    }
    li ul, li ul {
      margin-left: 24px;
    }
    p, ul, ol {
      font-size: 16px;
      line-height: 24px;
      max-width: 80%;
    }
    .id {color: #bbb}
    .lit {color: #aaa}
    .op {color: #9f9}
    .str {color: #f00}
    .num {color: white}
    .field {color: #faa}
    .kw {font-weight: bold; color: #ff6}
    .code-samples li {
      font-family: Consolas, Monaco, Andale Mono, monospace;
      line-height: 1.5;
      font-size: 13px;
      background: #333;
      overflow: auto;
      margin: 0;
      padding: 0;
    }
    .code-samples pre {
      margin: 0;
      padding: 0.5em 0.5em;
    }
    .code-samples {
      position: relative;
    }
    .code-samples-names li {
      display: block;
    }
    .code-samples-names li {
      color: white;
      background: #9c1;
      float: left;
      margin: 0 1px -4px 0;
      position: relative;
      z-index: 1;
      border: 4px solid #9c1;
      border-bottom: 0;
      border-radius: 9px 9px 0 0;
      padding: 0.2em 1em 4px 1em;
      cursor: pointer;
    }
    .code-samples-names li.selected {
      background: #333;
    }
    .code-samples {
      clear: both;
    }
    .code-samples li {
      display: block;
      border: 4px solid #9c1;
      border-radius: 9px;
      border-top-left-radius: 0;
      width: 100%;
    }
    .args-list li {
      display: block;
    }
  </style>
</head>
  <body>
    <script>
      function changeTab2(tab, addClickHandlers) {
        var els = tab.parentNode.childNodes;
        var els2 = tab.parentNode.nextSibling.childNodes;
        for (var i = 0; i < els.length; i++) {
          if (addClickHandlers)
            els[i].addEventListener('click', changeTab, false);

          if (els[i] == tab) {
            els[i].setAttribute('class', 'selected');
            els2[i].style.display = 'block';
          } else {
            els[i].removeAttribute('class');
            els2[i].style.display = 'none';
          }
        }
      }
      function changeTab(event) {
        changeTab2(event.target);
      }
    </script>".


html_post() ->
    "<script>
    var ul = document.getElementsByTagName('ul');
    for (var i = 0; i < ul.length; i++) {
      if (ul[i].className == 'code-samples-names')
        changeTab2(ul[i].firstChild, true);
    }
  </script>
  </body>
</html>".
