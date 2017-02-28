%%%----------------------------------------------------------------------
%%% File    : ejabberd_commands_doc.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Management of ejabberd commands
%%% Created : 20 May 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-include("ejabberd_commands.hrl").
-include("ejabberd.hrl").

-define(RAW(V), if HTMLOutput -> fxml:crypt(iolist_to_binary(V)); true -> iolist_to_binary(V) end).
-define(TAG(N), if HTMLOutput -> [<<"<", ??N, "/>">>]; true -> md_tag(N, <<"">>) end).
-define(TAG(N, V), if HTMLOutput -> [<<"<", ??N, ">">>, V, <<"</", ??N, ">">>]; true -> md_tag(N, V) end).
-define(TAG(N, C, V), if HTMLOutput -> [<<"<", ??N, " class='", C, "'>">>, V, <<"</", ??N, ">">>]; true -> md_tag(N, V) end).
-define(TAG_R(N, V), ?TAG(N, ?RAW(V))).
-define(TAG_R(N, C, V), ?TAG(N, C, ?RAW(V))).
-define(SPAN(N, V), ?TAG_R(span, ??N, V)).

-define(STR(A), ?SPAN(str,[<<"\"">>, A, <<"\"">>])).
-define(NUM(A), ?SPAN(num,integer_to_binary(A))).
-define(FIELD(A), ?SPAN(field,A)).
-define(ID(A), ?SPAN(id,A)).
-define(OP(A), ?SPAN(op,A)).
-define(ARG(A), ?FIELD(atom_to_list(A))).
-define(KW(A), ?SPAN(kw,A)).
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
list_join_with([El|Tail], M) ->
    lists:reverse(lists:foldl(fun(E, Acc) ->
                                      [E, M | Acc]
                              end, [El], Tail)).

md_tag(dt, V) ->
    [<<"\n">>, V, <<"\n">>];
md_tag(dd, V) ->
    [<<"\n: ">>, V, <<"\n">>];
md_tag(li, V) ->
    [<<"- ">>, V, <<"\n">>];
md_tag(pre, V) ->
    [V, <<"\n">>];
md_tag(p, V) ->
    [<<"\n\n">>, V, <<"\n">>];
md_tag(h1, V) ->
    [<<"\n\n## ">>, V, <<"\n">>];
md_tag(h2, V) ->
    [<<"\n\n### ">>, V, <<"\n">>];
md_tag(strong, V) ->
    [<<"*">>, V, <<"*">>];
md_tag(_, V) ->
    V.


%% rescode_to_int(ok) ->
%%     0;
%% rescode_to_int(true) ->
%%     0;
%% rescode_to_int(_) ->
%%     1.

perl_gen({Name, integer}, Int, _Indent, HTMLOutput) ->
    [?ARG(Name), ?OP_L(" => "), ?NUM(Int)];
perl_gen({Name, string}, Str, _Indent, HTMLOutput) ->
    [?ARG(Name), ?OP_L(" => "), ?STR(Str)];
perl_gen({Name, binary}, Str, _Indent, HTMLOutput) ->
    [?ARG(Name), ?OP_L(" => "), ?STR(Str)];
perl_gen({Name, atom}, Atom, _Indent, HTMLOutput) ->
    [?ARG(Name), ?OP_L(" => "), ?STR_A(Atom)];
perl_gen({Name, {tuple, Fields}}, Tuple, Indent, HTMLOutput) ->
    Res = lists:map(fun({A,B})->perl_gen(A, B, Indent, HTMLOutput) end, lists:zip(Fields, tuple_to_list(Tuple))),
    [?ARG(Name), ?OP_L(" => {"), list_join_with(Res, [?OP_L(", ")]), ?OP_L("}")];
perl_gen({Name, {list, ElDesc}}, List, Indent, HTMLOutput) ->
    Res = lists:map(fun(E) -> [?OP_L("{"), perl_gen(ElDesc, E, Indent, HTMLOutput), ?OP_L("}")] end, List),
    [?ARG(Name), ?OP_L(" => ["), list_join_with(Res, [?OP_L(", ")]), ?OP_L("]")].

perl_call(Name, ArgsDesc, Values, HTMLOutput) ->
    {Indent, Preamble} = if HTMLOutput -> {<<"">>, []}; true -> {<<"    ">>, <<"~~~ perl\n">>} end,
    [Preamble,
     Indent, ?ID_L("XMLRPC::Lite"), ?OP_L("->"), ?ID_L("proxy"), ?OP_L("("), ?ID_L("$url"), ?OP_L(")->"),
     ?ID_L("call"), ?OP_L("("), ?STR_A(Name), ?OP_L(", {"), ?BR, Indent, <<"  ">>,
     list_join_with(lists:map(fun({A,B})->perl_gen(A, B, <<Indent/binary, "  ">>, HTMLOutput) end, lists:zip(ArgsDesc, Values)), [?OP_L(","), ?BR, Indent, <<"  ">>]),
     ?BR, Indent, ?OP_L("})->"), ?ID_L("results"), ?OP_L("()")].

java_gen_map(Vals, Indent, HTMLOutput) ->
    {Split, NL} = case Indent of
                      none -> {<<" ">>, <<" ">>};
                      _ -> {[?BR, <<"  ", Indent/binary>>], [?BR, Indent]}
                  end,
    [?KW_L("new "), ?ID_L("HashMap"), ?OP_L("<"), ?ID_L("String"), ?OP_L(", "), ?ID_L("Object"),
     ?OP_L(">() {{"), Split, list_join_with(Vals, Split), NL, ?OP_L("}}")].

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
                    _ -> {[?BR, <<"    ", Indent/binary>>],
                          [?BR, <<"  ", Indent/binary>>],
                          <<"      ", Indent/binary>>}
                end,
    Res = lists:map(fun(E) -> java_gen_map([java_gen(ElDesc, E, I, HTMLOutput)], none, HTMLOutput) end, List),
    [?ID_L("put"), ?OP_L("("), ?STR_A(Name), ?OP_L(", "), ?KW_L("new "), ?ID_L("Object"), ?OP_L("[] {"), NI,
     list_join_with(Res, [?OP_L(","), NI]), NI2, ?OP_L("});")].

java_call(Name, ArgsDesc, Values, HTMLOutput) ->
    {Indent, Preamble} = if HTMLOutput -> {<<"">>, []}; true -> {<<"    ">>, <<"~~~ java\n">>} end,
    [Preamble,
     Indent, ?ID_L("XmlRpcClientConfigImpl config"), ?OP_L(" = "), ?KW_L("new "), ?ID_L("XmlRpcClientConfigImpl"), ?OP_L("();"), ?BR,
     Indent, ?ID_L("config"), ?OP_L("."), ?ID_L("setServerURL"), ?OP_L("("), ?ID_L("url"), ?OP_L(");"), ?BR, Indent, ?BR,
     Indent, ?ID_L("XmlRpcClient client"), ?OP_L(" = "), ?KW_L("new "), ?ID_L("XmlRpcClient"), ?OP_L("();"), ?BR,
     Indent, ?ID_L("client"), ?OP_L("."), ?ID_L("setConfig"), ?OP_L("("), ?ID_L("config"), ?OP_L(");"), ?BR, Indent, ?BR,
     Indent, ?ID_L("client"), ?OP_L("."), ?ID_L("execute"), ?OP_L("("), ?STR_A(Name), ?OP_L(", "),
     java_gen_map(lists:map(fun({A,B})->java_gen(A, B, Indent, HTMLOutput) end, lists:zip(ArgsDesc, Values)), Indent, HTMLOutput),
     ?OP_L(");")].

-define(XML_S(N, V), ?OP_L("<"), ?FIELD_L(??N), ?OP_L(">"), V).
-define(XML_E(N), ?OP_L("</"), ?FIELD_L(??N), ?OP_L(">")).
-define(XML(N, Indent, V), ?BR, Indent, ?XML_S(N, V), ?BR, Indent, ?XML_E(N)).
-define(XML(N, Indent, D, V), ?XML(N, [Indent, lists:duplicate(D, <<"  ">>)], V)).
-define(XML_L(N, Indent, V), ?BR, Indent, ?XML_S(N, V), ?XML_E(N)).
-define(XML_L(N, Indent, D, V), ?XML_L(N, [Indent, lists:duplicate(D, <<"  ">>)], V)).

xml_gen({Name, integer}, Int, Indent, HTMLOutput) ->
    [?XML(member, Indent,
         [?XML_L(name, Indent, 1, ?ID_A(Name)),
          ?XML(value, Indent, 1,
               [?XML_L(integer, Indent, 2, ?ID(integer_to_binary(Int)))])])];
xml_gen({Name, string}, Str, Indent, HTMLOutput) ->
    [?XML(member, Indent,
         [?XML_L(name, Indent, 1, ?ID_A(Name)),
          ?XML(value, Indent, 1,
               [?XML_L(string, Indent, 2, ?ID(Str))])])];
xml_gen({Name, binary}, Str, Indent, HTMLOutput) ->
    [?XML(member, Indent,
         [?XML_L(name, Indent, 1, ?ID_A(Name)),
          ?XML(value, Indent, 1,
               [?XML_L(string, Indent, 2, ?ID(Str))])])];
xml_gen({Name, atom}, Atom, Indent, HTMLOutput) ->
    [?XML(member, Indent,
         [?XML_L(name, Indent, 1, ?ID_A(Name)),
          ?XML(value, Indent, 1,
               [?XML_L(string, Indent, 2, ?ID(atom_to_list(Atom)))])])];
xml_gen({Name, {tuple, Fields}}, Tuple, Indent, HTMLOutput) ->
    NewIndent = <<"    ", Indent/binary>>,
    Res = lists:map(fun({A, B}) -> xml_gen(A, B, NewIndent, HTMLOutput) end, lists:zip(Fields, tuple_to_list(Tuple))),
    [?XML(member, Indent,
         [?XML_L(name, Indent, 1, ?ID_A(Name)),
          ?XML(value, Indent, 1, [?XML(struct, NewIndent, Res)])])];
xml_gen({Name, {list, ElDesc}}, List, Indent, HTMLOutput) ->
    Ind1 = <<"        ", Indent/binary>>,
    Ind2 = <<"    ", Ind1/binary>>,
    Res = lists:map(fun(E) -> [?XML(value, Ind1, [?XML(struct, Ind1, 1, xml_gen(ElDesc, E, Ind2, HTMLOutput))])] end, List),
    [?XML(member, Indent,
         [?XML_L(name, Indent, 1, ?ID_A(Name)),
          ?XML(value, Indent, 1, [?XML(array, Indent, 2, [?XML(data, Indent, 3, Res)])])])].

xml_call(Name, ArgsDesc, Values, HTMLOutput) ->
    {Indent, Preamble} = if HTMLOutput -> {<<"">>, []}; true -> {<<"    ">>, <<"~~~ xml">>} end,
    Res = lists:map(fun({A, B}) -> xml_gen(A, B, <<Indent/binary, "          ">>, HTMLOutput) end, lists:zip(ArgsDesc, Values)),
    [Preamble,
     ?XML(methodCall, Indent,
          [?XML_L(methodName, Indent, 1, ?ID_A(Name)),
           ?XML(params, Indent, 1,
                [?XML(param, Indent, 2,
                      [?XML(value, Indent, 3,
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
    [?OP_L("{"), ?STR_L("res"), ?OP_L(": "), ?ID_A(Val == ok orelse Val == true), ?OP_L(", "),
     ?STR_L("text"), ?OP_L(": "), ?STR(Str), ?OP_L("}")];
json_gen({_Name, {list, {_, {tuple, [{_, atom}, ValFmt]}}}}, List, Indent, HTMLOutput) ->
    Indent2 = <<"  ", Indent/binary>>,
    Res = lists:map(fun({N, V})->[?STR_A(N), ?OP_L(": "), json_gen(ValFmt, V, Indent2, HTMLOutput)] end, List),
    [?OP_L("{"), ?BR, Indent2, list_join_with(Res, [?OP_L(","), ?BR, Indent2]), ?BR, Indent, ?OP_L("}")];
json_gen({_Name, {tuple, Fields}}, Tuple, Indent, HTMLOutput) ->
    Indent2 = <<"  ", Indent/binary>>,
    Res = lists:map(fun({{N, _} = A, B})->[?STR_A(N), ?OP_L(": "), json_gen(A, B, Indent2, HTMLOutput)] end,
                    lists:zip(Fields, tuple_to_list(Tuple))),
    [?OP_L("{"), ?BR, Indent2, list_join_with(Res, [?OP_L(","), ?BR, Indent2]), ?BR, Indent, ?OP_L("}")];
json_gen({_Name, {list, ElDesc}}, List, Indent, HTMLOutput) ->
    Indent2 = <<"  ", Indent/binary>>,
    Res = lists:map(fun(E) -> json_gen(ElDesc, E, Indent2, HTMLOutput) end, List),
    [?OP_L("["), ?BR, Indent2, list_join_with(Res, [?OP_L(","), ?BR, Indent2]), ?BR, Indent, ?OP_L("]")].

json_call(Name, ArgsDesc, Values, ResultDesc, Result, HTMLOutput) ->
    {Indent, Preamble} = if HTMLOutput -> {<<"">>, []}; true -> {<<"    ">>, <<"~~~ json\n">>} end,
    {Code, ResultStr} = case {ResultDesc, Result} of
                            {{_, rescode}, V} when V == true; V == ok ->
                                {200, [?STR_L("")]};
                            {{_, rescode}, _} ->
                                {500, [?STR_L("")]};
                            {{_, restuple}, {V1, Text1}} when V1 == true; V1 == ok ->
                                {200, [?STR(Text1)]};
                            {{_, restuple}, {_, Text2}} ->
                                {500, [?STR(Text2)]};
                            {{_, {list, _}}, _} ->
                                {200, json_gen(ResultDesc, Result, Indent, HTMLOutput)};
                            {{_, {tuple, _}}, _} ->
                                {200, json_gen(ResultDesc, Result, Indent, HTMLOutput)};
                            {{Name0, _}, _} ->
                                {200, [Indent, ?OP_L("{"), ?STR_A(Name0), ?OP_L(": "),
                                       json_gen(ResultDesc, Result, Indent, HTMLOutput), Indent, ?OP_L("}")]}
                        end,
    CodeStr = case Code of
                  200 -> <<" 200 OK">>;
                  500 -> <<" 500 Internal Server Error">>
              end,
    [Preamble,
     Indent, ?ID_L("POST /api/"), ?ID_A(Name), ?BR,
     Indent, ?OP_L("{"), ?BR, Indent, <<"  ">>,
     list_join_with(lists:map(fun({{N,_}=A,B})->[?STR_A(N), ?OP_L(": "), json_gen(A, B, <<Indent/binary, "  ">>, HTMLOutput)] end,
                              lists:zip(ArgsDesc, Values)), [?OP_L(","), ?BR, Indent, <<"  ">>]),
     ?BR, Indent, ?OP_L("}"), ?BR, Indent, ?BR, Indent,
     ?ID_L("HTTP/1.1"), ?ID(CodeStr), ?BR, Indent,
     ResultStr
    ].

generate_example_input({_Name, integer}, {LastStr, LastNum}) ->
    {LastNum+1, {LastStr, LastNum+1}};
generate_example_input({_Name, string}, {LastStr, LastNum}) ->
    {string:chars(LastStr+1, 5), {LastStr+1, LastNum}};
generate_example_input({_Name, binary}, {LastStr, LastNum}) ->
    {iolist_to_binary(string:chars(LastStr+1, 5)), {LastStr+1, LastNum}};
generate_example_input({_Name, atom}, {LastStr, LastNum}) ->
    {list_to_atom(string:chars(LastStr+1, 5)), {LastStr+1, LastNum}};
generate_example_input({_Name, rescode}, {LastStr, LastNum}) ->
    {ok, {LastStr, LastNum}};
generate_example_input({_Name, restuple}, {LastStr, LastNum}) ->
    {{ok, <<"Success">>}, {LastStr, LastNum}};
generate_example_input({_Name, {tuple, Fields}}, Data) ->
    {R, D} = lists:foldl(fun(Field, {Res2, Data2}) ->
                                 {Res3, Data3} = generate_example_input(Field, Data2),
                                 {[Res3 | Res2], Data3}
                         end, {[], Data}, Fields),
    {list_to_tuple(lists:reverse(R)), D};
generate_example_input({_Name, {list, Desc}}, Data) ->
    {R1, D1} = generate_example_input(Desc, Data),
    {R2, D2} = generate_example_input(Desc, D1),
    {[R1, R2], D2}.

gen_calls(#ejabberd_commands{args_example=none, args=ArgsDesc} = C, HTMLOutput, Langs) ->
    {R, _} = lists:foldl(fun(Arg, {Res, Data}) ->
                                 {Res3, Data3} = generate_example_input(Arg, Data),
                                 {[Res3 | Res], Data3}
                         end, {[], {$a-1, 0}}, ArgsDesc),
    gen_calls(C#ejabberd_commands{args_example=lists:reverse(R)}, HTMLOutput, Langs);
gen_calls(#ejabberd_commands{result_example=none, result=ResultDesc} = C, HTMLOutput, Langs) ->
    {R, _} = generate_example_input(ResultDesc, {$a-1, 0}),
    gen_calls(C#ejabberd_commands{result_example=R}, HTMLOutput, Langs);
gen_calls(#ejabberd_commands{args_example=Values, args=ArgsDesc,
                             result_example=Result, result=ResultDesc,
                             name=Name}, HTMLOutput, Langs) ->
    Perl = perl_call(Name, ArgsDesc, Values, HTMLOutput),
    Java = java_call(Name, ArgsDesc, Values, HTMLOutput),
    XML = xml_call(Name, ArgsDesc, Values, HTMLOutput),
    JSON = json_call(Name, ArgsDesc, Values, ResultDesc, Result, HTMLOutput),
    if HTMLOutput ->
            [?TAG(ul, "code-samples-names",
                  [case lists:member(<<"java">>, Langs) of true -> ?TAG(li, <<"Java">>); _ -> [] end,
                   case lists:member(<<"perl">>, Langs) of true -> ?TAG(li, <<"Perl">>); _ -> [] end,
                   case lists:member(<<"xmlrpc">>, Langs) of true -> ?TAG(li, <<"XML">>); _ -> [] end,
                   case lists:member(<<"json">>, Langs) of true -> ?TAG(li, <<"JSON">>); _ -> [] end]),
             ?TAG(ul, "code-samples",
                  [case lists:member(<<"java">>, Langs) of true -> ?TAG(li, ?TAG(pre, Java)); _ -> [] end,
                   case lists:member(<<"perl">>, Langs) of true -> ?TAG(li, ?TAG(pre, Perl)); _ -> [] end,
                   case lists:member(<<"xmlrpc">>, Langs) of true -> ?TAG(li, ?TAG(pre, XML)); _ -> [] end,
                   case lists:member(<<"json">>, Langs) of true -> ?TAG(li, ?TAG(pre, JSON)); _ -> [] end])];
       true ->
            [<<"\n">>, case lists:member(<<"java">>, Langs) of true -> <<"* Java\n">>; _ -> [] end,
             case lists:member(<<"perl">>, Langs) of true -> <<"* Perl\n">>; _ -> [] end,
             case lists:member(<<"xmlrpc">>, Langs) of true -> <<"* XmlRPC\n">>; _ -> [] end,
             case lists:member(<<"json">>, Langs) of true -> <<"* JSON\n">>; _ -> [] end,
             <<"{: .code-samples-labels}\n">>,
             case lists:member(<<"java">>, Langs) of true -> [<<"\n* ">>, ?TAG(pre, Java), <<"~~~\n">>]; _ -> [] end,
             case lists:member(<<"perl">>, Langs) of true -> [<<"\n* ">>, ?TAG(pre, Perl), <<"~~~\n">>]; _ -> [] end,
             case lists:member(<<"xmlrpc">>, Langs) of true -> [<<"\n* ">>, ?TAG(pre, XML), <<"~~~\n">>]; _ -> [] end,
             case lists:member(<<"json">>, Langs) of true -> [<<"\n* ">>, ?TAG(pre, JSON), <<"~~~\n">>]; _ -> [] end,
             <<"{: .code-samples-tabs}\n\n">>]
    end.

gen_doc(#ejabberd_commands{name=Name, tags=_Tags, desc=Desc, longdesc=LongDesc,
                           args=Args, args_desc=ArgsDesc,
                           result=Result, result_desc=ResultDesc}=Cmd, HTMLOutput, Langs) ->
    LDesc = case LongDesc of
                "" -> Desc;
                _ -> LongDesc
            end,
    ArgsText = case ArgsDesc of
                   none ->
                       [?TAG(ul, "args-list", lists:map(fun({AName, Type}) ->
                                                                [?TAG(li, [?TAG_R(strong, atom_to_list(AName)), <<" :: ">>,
                                                                           ?RAW(io_lib:format("~p", [Type]))])]
                                                        end, Args))];
                   _ ->
                       [?TAG(dl, "args-list", lists:map(fun({{AName, Type}, ADesc}) ->
                                                                [?TAG(dt, [?TAG_R(strong, atom_to_list(AName)), <<" :: ">>,
                                                                           ?RAW(io_lib:format("~p", [Type]))]),
                                                                 ?TAG(dd, ?RAW(ADesc))]
                                                        end, lists:zip(Args, ArgsDesc)))]
               end,
    ResultText = case ResultDesc of
                     none ->
                         [?RAW(io_lib:format("~p", [Result]))];
                     _ ->
                         [?TAG(dl, [
			       ?TAG(dt, io_lib:format("~p", [Result])),
			       ?TAG_R(dd, ResultDesc)])]
                 end,

    [?TAG(h1, [?TAG(strong, atom_to_list(Name)), <<" - ">>, ?RAW(Desc)]),
     ?TAG(p, ?RAW(LDesc)),
     ?TAG(h2, <<"Arguments:">>),
     ArgsText,
     ?TAG(h2, <<"Result:">>),
     ResultText,
     ?TAG(h2, <<"Examples:">>),
     gen_calls(Cmd, HTMLOutput, Langs)].

find_commands_definitions() ->
    case code:lib_dir(ejabberd, ebin) of
        {error, _} ->
            lists:map(fun({N, _, _}) ->
                              ejabberd_commands:get_command_definition(N)
                      end, ejabberd_commands:list_commands());
        Path ->
            lists:flatmap(fun(P) ->
                                  Mod = list_to_atom(filename:rootname(P)),
                                  code:ensure_loaded(Mod),
                                  case erlang:function_exported(Mod, get_commands_spec, 0) of
                                      true ->
                                          apply(Mod, get_commands_spec, []);
                                      _ ->
                                          []
                                  end
                          end, filelib:wildcard("*.beam", Path))
    end.

generate_html_output(File, RegExp, Languages) ->
    Cmds = find_commands_definitions(),
    {ok, RE} = re:compile(RegExp),
    Cmds2 = lists:filter(fun(#ejabberd_commands{name=Name, module=Module}) ->
                                 re:run(atom_to_list(Name), RE, [{capture, none}]) == match orelse
                                     re:run(atom_to_list(Module), RE, [{capture, none}]) == match
                         end, Cmds),
    Cmds3 = lists:sort(fun(#ejabberd_commands{name=N1}, #ejabberd_commands{name=N2}) ->
                               N1 =< N2
                       end, Cmds2),
    Cmds4 = [maybe_add_policy_arguments(Cmd) || Cmd <- Cmds3],
    Langs = binary:split(Languages, <<",">>, [global]),
    Out = lists:map(fun(C) -> gen_doc(C, true, Langs) end, Cmds4),
    {ok, Fh} = file:open(File, [write]),
    io:format(Fh, "~s", [[html_pre(), Out, html_post()]]),
    file:close(Fh),
    ok.

maybe_add_policy_arguments(#ejabberd_commands{args=Args1, policy=user}=Cmd) ->
    Args2 = [{user, binary}, {server, binary} | Args1],
    Cmd#ejabberd_commands{args = Args2};
maybe_add_policy_arguments(Cmd) ->
    Cmd.

generate_md_output(File, RegExp, Languages) ->
    Cmds = find_commands_definitions(),
    {ok, RE} = re:compile(RegExp),
    Cmds2 = lists:filter(fun(#ejabberd_commands{name=Name, module=Module}) ->
                                 re:run(atom_to_list(Name), RE, [{capture, none}]) == match orelse
                                     re:run(atom_to_list(Module), RE, [{capture, none}]) == match
                         end, Cmds),
    Cmds3 = lists:sort(fun(#ejabberd_commands{name=N1}, #ejabberd_commands{name=N2}) ->
                               N1 =< N2
                       end, Cmds2),
    Langs = binary:split(Languages, <<",">>, [global]),
    Header = <<"---\ntitle: Administration API reference\nbodyclass: nocomment\n---">>,
    Out = lists:map(fun(C) -> gen_doc(C, false, Langs) end, Cmds3),
    {ok, Fh} = file:open(File, [write]),
    io:format(Fh, "~s~s", [Header, Out]),
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
