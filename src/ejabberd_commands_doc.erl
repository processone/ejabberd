%%%----------------------------------------------------------------------
%%% File    : ejabberd_commands_doc.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Management of ejabberd commands
%%% Created : 20 May 2008 by Badlop <badlop@process-one.net>
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

-module(ejabberd_commands_doc).
-author('pawel@process-one.net').

-export([generate_output/2]).

-include("ejabberd_commands.hrl").
-include("ejabberd.hrl").

-define(RAW(V), xml:crypt(iolist_to_binary(V))).
-define(TAG(N), <<"<", ??N, "/>">>).
-define(TAG(N, V), <<"<", ??N, ">">>, V, <<"</", ??N, ">">>).
-define(TAG(N, C, V), <<"<", ??N, " class='", C, "'>">>, V, <<"</", ??N, ">">>).
-define(TAG_R(N, V), ?TAG(N, ?RAW(V))).
-define(TAG_R(N, C, V), ?TAG(N, C, ?RAW(V))).
-define(SPAN(N, V), ?TAG_R(span, ??N, V)).

-define(STR(A), ?SPAN(str,[<<"\"">>, A, <<"\"">>])).
-define(NUM(A), ?SPAN(num,jlib:integer_to_binary(A))).
-define(FIELD(A), ?SPAN(field,A)).
-define(ID(A), ?SPAN(id,A)).
-define(OP(A), ?SPAN(op,A)).
-define(ARG(A), ?FIELD(atom_to_list(A))).
-define(KW(A), ?SPAN(kw,A)).
-define(BR, <<"\n">>).

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

rescode_to_int(ok) ->
    0;
rescode_to_int(true) ->
    0;
rescode_to_int(_) ->
    1.

perl_gen({Name, integer}, Int, _Indent) ->
    [?ARG(Name), ?OP_L(" => "), ?NUM(Int)];
perl_gen({Name, string}, Str, _Indent) ->
    [?ARG(Name), ?OP_L(" => "), ?STR(Str)];
perl_gen({Name, binary}, Str, _Indent) ->
    [?ARG(Name), ?OP_L(" => "), ?STR(Str)];
perl_gen({Name, atom}, Atom, _Indent) ->
    [?ARG(Name), ?OP_L(" => "), ?STR(atom_to_list(Atom))];
perl_gen({Name, {tuple, Fields}}, Tuple, Indent) ->
    Res = lists:map(fun({A,B})->perl_gen(A, B, Indent) end, lists:zip(Fields, tuple_to_list(Tuple))),
    [?ARG(Name), ?OP_L(" => {"), list_join_with(Res, [?OP_L(", ")]), ?OP_L("}")];
perl_gen({Name, {list, ElDesc}}, List, Indent) ->
    Res = lists:map(fun(E) -> [?OP_L("{"), perl_gen(ElDesc, E, Indent), ?OP_L("}")] end, List),
    [?ARG(Name), ?OP_L(" => ["), list_join_with(Res, [?OP_L(", ")]), ?OP_L("]")].

perl_call(Name, ArgsDesc, Values) ->
    [?ID_L("XMLRPC::Lite"), ?OP_L("->"), ?ID_L("proxy"), ?OP_L("("), ?ID_L("$url"), ?OP_L(")->"),
     ?ID_L("call"), ?OP_L("("), ?STR_A(Name), ?OP_L(", {"), ?BR, <<"  ">>,
     list_join_with(lists:map(fun({A,B})->perl_gen(A, B, <<"  ">>) end, lists:zip(ArgsDesc, Values)), [?OP_L(","), ?BR, <<"  ">>]),
     ?BR, ?OP_L("})->"), ?ID_L("results"), ?OP_L("()")].

java_gen_map(Vals, Indent) ->
    {Split, NL} = case Indent of
                      none -> {<<" ">>, <<" ">>};
                      _ -> {[?BR, <<"  ", Indent/binary>>], [?BR, Indent]}
                  end,
    [?KW_L("new "), ?ID_L("HashMap"), ?OP_L("<"), ?ID_L("String"), ?OP_L(", "), ?ID_L("Object"),
     ?OP_L(">() {{"), Split, list_join_with(Vals, Split), NL, ?OP_L("}}")].

java_gen({Name, integer}, Int, _Indent) ->
    [?ID_L("put"), ?OP_L("("), ?STR_A(Name), ?OP_L(", "), ?KW_L("new "), ?ID_L("Integer"), ?OP_L("("), ?NUM(Int), ?OP_L("));")];
java_gen({Name, string}, Str, _Indent) ->
    [?ID_L("put"), ?OP_L("("), ?STR_A(Name), ?OP_L(", "), ?STR(Str), ?OP_L(");")];
java_gen({Name, binary}, Str, _Indent) ->
    [?ID_L("put"), ?OP_L("("), ?STR_A(Name), ?OP_L(", "), ?STR(Str), ?OP_L(");")];
java_gen({Name, atom}, Atom, _Indent) ->
    [?ID_L("put"), ?OP_L("("), ?STR_A(Name), ?OP_L(", "), ?STR(atom_to_list(Atom)), ?OP_L(");")];
java_gen({Name, {tuple, Fields}}, Tuple, Indent) ->
    NewIndent = <<"  ", Indent/binary>>,
    Res = lists:map(fun({A, B}) -> [java_gen(A, B, NewIndent)] end, lists:zip(Fields, tuple_to_list(Tuple))),
    [?ID_L("put"), ?OP_L("("), ?STR_A(Name), ?OP_L(", "), java_gen_map(Res, Indent), ?OP_L(")")];
java_gen({Name, {list, ElDesc}}, List, Indent) ->
    Res = lists:map(fun(E) -> java_gen_map([java_gen(ElDesc, E, Indent)], none) end, List),
    [?ID_L("put"), ?OP_L("("), ?STR_A(Name), ?OP_L(", "), ?KW_L("new "), ?ID_L("Object"), ?OP_L("[] { "),
     list_join_with(Res, [?OP_L(", ")]), ?OP_L(" });")].

java_call(Name, ArgsDesc, Values) ->
    [?ID_L("XmlRpcClientConfigImpl config"), ?OP_L(" = "), ?KW_L("new "), ?ID_L("XmlRpcClientConfigImpl"), ?OP_L("();"), ?BR,
     ?ID_L("config"), ?OP_L("."), ?ID_L("setServerURL"), ?OP_L("("), ?ID_L("url"), ?OP_L(");"), ?BR, ?BR,
     ?ID_L("XmlRpcClient client"), ?OP_L(" = "), ?KW_L("new "), ?ID_L("XmlRpcClient"), ?OP_L("();"), ?BR,
     ?ID_L("client"), ?OP_L("."), ?ID_L("setConfig"), ?OP_L("("), ?ID_L("config"), ?OP_L(");"), ?BR, ?BR,
     ?ID_L("client"), ?OP_L("."), ?ID_L("execute"), ?OP_L("("), ?STR_A(Name), ?OP_L(", "),
     java_gen_map(lists:map(fun({A,B})->java_gen(A, B, <<"">>) end, lists:zip(ArgsDesc, Values)), <<"">>), ?OP_L(");"), ?BR].

-define(XML_S(N, V), ?OP_L("<"), ?FIELD_L(??N), ?OP_L(">"), V).
-define(XML_E(N), ?OP_L("</"), ?FIELD_L(??N), ?OP_L(">")).
-define(XML(N, Indent, V), ?BR, Indent, ?XML_S(N, V), ?BR, Indent, ?XML_E(N)).
-define(XML(N, Indent, D, V), ?XML(N, [Indent, lists:duplicate(D, <<"  ">>)], V)).
-define(XML_L(N, Indent, V), ?BR, Indent, ?XML_S(N, V), ?XML_E(N)).
-define(XML_L(N, Indent, D, V), ?XML_L(N, [Indent, lists:duplicate(D, <<"  ">>)], V)).

xml_gen({Name, integer}, Int, Indent) ->
    [?XML(member, Indent,
         [?XML_L(name, Indent, 1, ?ID_A(Name)),
          ?XML(value, Indent, 1,
               [?XML_L(integer, Indent, 2, ?ID(jlib:integer_to_binary(Int)))])])];
xml_gen({Name, string}, Str, Indent) ->
    [?XML(member, Indent,
         [?XML_L(name, Indent, 1, ?ID_A(Name)),
          ?XML(value, Indent, 1,
               [?XML_L(string, Indent, 2, ?ID(Str))])])];
xml_gen({Name, binary}, Str, Indent) ->
    [?XML(member, Indent,
         [?XML_L(name, Indent, 1, ?ID_A(Name)),
          ?XML(value, Indent, 1,
               [?XML_L(string, Indent, 2, ?ID(Str))])])];
xml_gen({Name, atom}, Atom, Indent) ->
    [?XML(member, Indent,
         [?XML_L(name, Indent, 1, ?ID_A(Name)),
          ?XML(value, Indent, 1,
               [?XML_L(string, Indent, 2, ?ID(atom_to_list(Atom)))])])];
xml_gen({Name, {tuple, Fields}}, Tuple, Indent) ->
    NewIndent = <<"    ", Indent/binary>>,
    Res = lists:map(fun({A, B}) -> xml_gen(A, B, NewIndent) end, lists:zip(Fields, tuple_to_list(Tuple))),
    [?XML(member, Indent,
         [?XML_L(name, Indent, 1, ?ID_A(Name)),
          ?XML(value, Indent, 1, [?XML(struct, NewIndent, Res)])])];
xml_gen({Name, {list, ElDesc}}, List, Indent) ->
    Ind1 = <<"        ", Indent/binary>>,
    Ind2 = <<"    ", Ind1/binary>>,
    Res = lists:map(fun(E) -> [?XML(value, Ind1, [?XML(struct, Ind1, 1, xml_gen(ElDesc, E, Ind2))])] end, List),
    [?XML(member, Indent,
         [?XML_L(name, Indent, 1, ?ID_A(Name)),
          ?XML(value, Indent, 1, [?XML(array, Indent, 2, [?XML(data, Indent, 3, Res)])])])].

xml_call(Name, ArgsDesc, Values) ->
    Ind = <<"">>,
    Res = lists:map(fun({A, B}) -> xml_gen(A, B, <<"          ">>) end, lists:zip(ArgsDesc, Values)),
    [?XML(methodCall, Ind,
          [?XML_L(methodName, Ind, 1, ?ID_A(Name)),
           ?XML(params, Ind, 1,
                [?XML(param, Ind, 2,
                      [?XML(value, Ind, 3,
                            [?XML(struct, Ind, 4, Res)])])])])].


gen_calls(#ejabberd_commands{args_example=none}) ->
    <<"">>;
gen_calls(#ejabberd_commands{args_example=Values, args=ArgsDesc, name=Name}) ->
    Perl = perl_call(Name, ArgsDesc, Values),
    Java = java_call(Name, ArgsDesc, Values),
    XML = xml_call(Name, ArgsDesc, Values),
    [?TAG(ul, "code-samples-names",
          [?TAG(li, <<"Java">>),
           ?TAG(li, <<"Perl">>),
           ?TAG(li, <<"XML">>)]),
     ?TAG(ul, "code-samples",
          [?TAG(li, ?TAG(pre, Java)),
           ?TAG(li, ?TAG(pre, Perl)),
           ?TAG(li, ?TAG(pre, XML))])].

gen_doc(#ejabberd_commands{name=Name, tags=Tags, desc=Desc, longdesc=LongDesc,
                           args=Args, args_desc=ArgsDesc,
                           result=Result, result_desc=ResultDesc}=Cmd) ->
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
                         [?RAW(io_lib:format("~p", [Result])),
                          ?TAG_R(p, ResultDesc)]
                 end,

    [?TAG(h1, [?TAG(strong, atom_to_list(Name)), <<" - ">>, ?RAW(Desc)]),
     ?TAG(p, ?RAW(LDesc)),
     ?TAG(h2, <<"Arguments:">>),
     ArgsText,
     ?TAG(h2, <<"Result:">>),
     ResultText,
     ?TAG(h2, <<"Examples:">>),
     gen_calls(Cmd)].

generate_output(File, RegExp) ->
    Cmds = lists:map(fun({N, _, _}) ->
                             ejabberd_commands:get_command_definition(N)
                     end, ejabberd_commands:list_commands()),
    {ok, RE} = re:compile(RegExp),
    Cmds2 = lists:filter(fun(#ejabberd_commands{name=Name, module=Module}) ->
                                 re:run(atom_to_list(Name), RE, [{capture, none}]) == match orelse
                                     re:run(atom_to_list(Module), RE, [{capture, none}]) == match
                         end, Cmds),
    Cmds3 = lists:sort(fun(#ejabberd_commands{name=N1}, #ejabberd_commands{name=N2}) ->
                               N1 =< N2
                       end, Cmds2),
    Out = lists:map(fun(C) -> gen_doc(C) end, Cmds3),
    {ok, Fh} = file:open(File, [write]),
    io:format(Fh, "~s", [[html_pre(), Out, html_post()]]),
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
