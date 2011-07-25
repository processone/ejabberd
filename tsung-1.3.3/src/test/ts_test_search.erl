%%%-------------------------------------------------------------------
%%% File    : ts_test_search.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description : unit tests for ts_search module
%%%
%%% $Id$
%%%-------------------------------------------------------------------
-module(ts_test_search).

-compile(export_all).

-export([marketplace/1,namespace/1,sessionBucket/1, new/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ts_profile.hrl").
-include_lib("ts_config.hrl").


-define(MANY,20).

-define(FORMDATA,"<input type=\"hidden\" name=\"jsf_tree_64\" id=\"jsf_tree_64\" value=\"H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA\">").



test()->
    ok.
parse_dyn_var_test() ->
    myset_env(),
    Data=?FORMDATA,
    StrName="jsf_tree_64",
    Regexp = ?DEF_REGEXP_DYNVAR_BEGIN++ StrName ++?DEF_REGEXP_DYNVAR_END,%'
    ?assertMatch([{'jsf_tree_64',"H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA"}], ts_search:parse_dynvar([{regexp,'jsf_tree_64', Regexp} ],list_to_binary(Data))).


parse_dyn_var_jsonpath_test() ->
    myset_env(),
    Data="\r\n\r\n{\"titi\": [23,45]}",
    JSONPath = "titi[1]",
    ?assertEqual([{'myvar',45}], ts_search:parse_dynvar([{jsonpath,'myvar', JSONPath} ],list_to_binary(Data))).

parse_dyn_var_jsonpath2_test() ->
    myset_env(),
    Data="\r\n\r\n{\"titi\": [23,45]}",
    JSONPath = "titi[3]",
    ?assertEqual([{'myvar',undefined}], ts_search:parse_dynvar([{jsonpath,'myvar', JSONPath} ],list_to_binary(Data))).

parse_dyn_var_jsonpath3_test() ->
    myset_env(),
    Data="\r\n\r\n{\"titi\": [{\"val\": 123, \"name\": \"foo\"}, {\"val\": 42, \"name\": \"bar\"}]}",
    JSONPath = "titi[?name=bar].val",
    ?assertEqual([{'myvar',42}], ts_search:parse_dynvar([{jsonpath,'myvar', JSONPath} ],list_to_binary(Data))).

parse_dyn_var_jsonpath_int_test() ->
    myset_env(),
    Data="\r\n\r\n{\"titi\": [{\"val\": 123, \"name\": \"foo\"}, {\"val\": 42, \"name\": \"bar\"}]}",
    JSONPath = "titi[?val=123].name",
    ?assertEqual([{'myvar',<<"foo">>}], ts_search:parse_dynvar([{jsonpath,'myvar', JSONPath} ],list_to_binary(Data))).

parse_dyn_var_xpath_test() ->
    myset_env(),
    Data="\r\n\r\n<html><body>"++?FORMDATA++"</body></html>",
    XPath = "//input[@name='jsf_tree_64']/@value",
    ?assertMatch([{'jsf_tree_64',"H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA"}], ts_search:parse_dynvar([{xpath,'jsf_tree_64', XPath} ],list_to_binary(Data))).

parse_dyn_var_xpath_with_scripttag_test() ->
    myset_env(),
    Data= "\r\n\r\n<html><head><script type=\"text/javascript\"> "
          " A = B <= C </script>"
          "</head><body>"++?FORMDATA++"</body></html>",
    XPath = "//input[@name='jsf_tree_64']/@value",
    ?assertMatch([{'jsf_tree_64',"H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA"}], ts_search:parse_dynvar([{xpath,'jsf_tree_64', XPath} ],list_to_binary(Data))).


parse_dyn_var2_test() ->
    myset_env(),
    Data="<input type=\"hidden\" name=\"tree64\" id=\"tree64\" value=\"H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA\">",
    StrName="tree64",
    Regexp = ?DEF_REGEXP_DYNVAR_BEGIN++ StrName ++?DEF_REGEXP_DYNVAR_END,%'
    ?assertMatch([{tree64,"H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA"}], ts_search:parse_dynvar([{regexp,tree64, Regexp }],list_to_binary(Data))).

parse_dyn_var_xpath2_test() ->
    myset_env(),
    Data="\r\n\r\n<html><body><input type=\"hidden\" name=\"tree64\" id=\"tree64\" value=\"H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA\"></body></html>",
    XPath = "//input[@name='tree64']/@value",
    ?assertMatch([{tree64,"H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA"}], ts_search:parse_dynvar([{xpath,tree64, XPath }],list_to_binary(Data))).

parse_dyn_var3_test() ->
    myset_env(),
    Data="<hidden name=\"random\" value=\"42\"></form>",
    StrName="random",
    Regexp = ?DEF_REGEXP_DYNVAR_BEGIN++ StrName ++?DEF_REGEXP_DYNVAR_END,%'
    ?assertMatch([{random,"42"}], ts_search:parse_dynvar([{regexp, random, Regexp }],list_to_binary(Data))).

parse_dyn_var_xpath3_test() ->
    myset_env(),
    Data="\r\n\r\n<html><body><hidden name=\"random\" value=\"42\"></form></body></html>",
    XPath = "//hidden[@name='random']/@value",
    ?assertMatch([{random,"42"}], ts_search:parse_dynvar([{xpath, random, XPath }],list_to_binary(Data))).

parse_dyn_var4_test() ->
    myset_env(),
    Data="<hidden name='random' value='42'></form>",
    StrName="random",
    Regexp = ?DEF_REGEXP_DYNVAR_BEGIN++ StrName ++?DEF_REGEXP_DYNVAR_END,%'
    ?assertMatch([{random,"42"}], ts_search:parse_dynvar([{regexp, random, Regexp }],list_to_binary(Data))).

parse_dyn_var_xpath4_test() ->
    myset_env(),
    Data="\r\n\r\n<html><body><hidden name='random' value='42'></form></body>/<html>",
    XPath = "//hidden[@name='random']/@value",
    ?assertMatch([{random,"42"}], ts_search:parse_dynvar([{xpath, random, XPath }],list_to_binary(Data))).

parse_dyn_var_many_test() ->
    myset_env(),
    {Data, Res}= setdata(?MANY),
    RegexpFun = fun(A) -> {regexp,list_to_atom(A), ?DEF_REGEXP_DYNVAR_BEGIN++ A ++?DEF_REGEXP_DYNVAR_END} end,%'
    B=lists:map(fun(A)->"random"++integer_to_list(A) end, lists:seq(1,?MANY)),
    C=lists:map(RegexpFun, B),
    {Time, Out}=timer:tc( ts_search,parse_dynvar,[C,list_to_binary(Data)]),
    erlang:display([?MANY," regexp:", Time]),
    ?assertMatch(Res, Out).

parse_dyn_var_many_xpath_test() ->
    myset_env(),
    {Data, Res}= setdata(?MANY),
    B=lists:map(fun(A)->{xpath, list_to_atom("random"++integer_to_list(A)),
                         "//input[@type='hidden'][@name='random"++integer_to_list(A)++"']/@value"} end, lists:seq(1,?MANY)),
    {Time, Out}=timer:tc( ts_search,parse_dynvar,[B,list_to_binary(Data)]),
    erlang:display([?MANY," xpath:", Time]),
    ?assertMatch(Res, Out).

parse_dyn_var_many_xpath_explicit_test() ->
    myset_env(),
    {Data, Res}= setdata(?MANY),
    B=lists:map(fun(A)->{xpath, list_to_atom("random"++integer_to_list(A)),
                         "/html/body/form/input[@type='hidden'][@name='random"++integer_to_list(A)++"']/@value"} end, lists:seq(1,?MANY)),
    {Time, Out}=timer:tc( ts_search,parse_dynvar,[B,list_to_binary(Data)]),
    erlang:display([?MANY," xpath_explicit:", Time]),
    ?assertMatch(Res, Out).

parse_dyn_var_many_big_test() ->
    myset_env(),
    {Data, Res}= setdata_big(?MANY),
    RegexpFun = fun(A) -> {regexp,list_to_atom(A), ?DEF_REGEXP_DYNVAR_BEGIN++ A ++?DEF_REGEXP_DYNVAR_END} end,%'
    B=lists:map(fun(A)->"random"++integer_to_list(A) end, lists:seq(1,?MANY)),
    C=lists:map(RegexpFun, B),
    {Time, Out}=timer:tc( ts_search,parse_dynvar,[C,list_to_binary(Data)]),
    erlang:display([?MANY," regexp_big:", Time]),
    ?assertMatch(Res, Out).

parse_dyn_var_many_big_xpath_test() ->
    myset_env(),
    {Data, Res}= setdata_big(?MANY),
    B=lists:map(fun(A)->{xpath, list_to_atom("random"++integer_to_list(A)),
                         "//input[@type='hidden'][@name='random"++integer_to_list(A)++"']/@value"} end, lists:seq(1,?MANY)),
    {Time, Out}=timer:tc( ts_search,parse_dynvar,[B,list_to_binary(Data)]),
    erlang:display([?MANY," xpath_big:", Time]),
    ?assertMatch(Res, Out).

parse_dyn_var_many_big_xpath_explicit_test() ->
    myset_env(),
    {Data, Res}= setdata_big(?MANY),
    B=lists:map(fun(A)->{xpath, list_to_atom("random"++integer_to_list(A)),
                         "/html/body/form/input[@type='hidden'][@name='random"++integer_to_list(A)++"']/@value"} end, lists:seq(1,?MANY)),
    {Time, Out}=timer:tc( ts_search,parse_dynvar,[B,list_to_binary(Data)]),
    erlang:display([?MANY," xpath_explicit_big:", Time]),
    ?assertMatch(Res, Out).

setdata(N) ->
    {"\r\n\r\n<html><body><form>"++lists:flatmap(fun(A)->
                                           AI=integer_to_list(A),["<input type='hidden' name='random",AI,"'"," value='value",AI,"'>"] end,lists:seq(1,N))
++"</form></body>/<html>",  lists:reverse(lists:map(fun(A)->{list_to_atom("random"++integer_to_list(A)) , "value"++integer_to_list(A)} end, lists:seq(1,N)))}.

setdata_big(N) ->
    Head = "<head><title>ABCDERFDJSJS</title><script type='text/javascript'> "
           "Some javascript code</script><link rel='shortcut icon' "
           " href='favicon.ico' type='image/x-icon'></head>",
    Fields = lists:flatmap(fun(A)->
                           AI=integer_to_list(A),
                           ["<input type='hidden' name='random",AI,"'"," value='value",AI,"'>"]
                           end,lists:seq(1,N)),
    Form = "<form>" ++ Fields ++ "</form>",
    Content = "<div><p>This is a some random content</p>"
              "<ul><li>item1<li>item2</ul>"
              "<p>Some more text... not too big really...</p>"
              "<p>More text inside a paragraph element</p> "
              "</div>",
    HTML ="\r\n\r\n<html>" ++ Head ++ "<body>" ++ Content ++ Content ++ Form ++ Content ++ "</body></html>",
    {HTML,lists:reverse(lists:map(fun(A)->{list_to_atom("random"++integer_to_list(A)) , "value"++integer_to_list(A)} end, lists:seq(1,N)))}.

parse_subst1_test() ->
    myset_env(),
    Data=?FORMDATA,
    StrName="jsf_tree_64",
    Regexp = ?DEF_REGEXP_DYNVAR_BEGIN++ StrName ++?DEF_REGEXP_DYNVAR_END,%'
    [{Name,Value}] = ts_search:parse_dynvar([{regexp, 'jsf_tree_64', Regexp }],list_to_binary(Data)),
    ?assertMatch("H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA", ts_search:subst("%%_jsf_tree_64%%",[{Name,Value}])).

parse_extract_fun1_test() ->
    myset_env(),
    Data="/echo?symbol=%%ts_test_search:new%%",
    ?assertMatch("/echo?symbol=IBM", ts_search:subst(Data,[])).

parse_extract_fun2_test() ->
    myset_env(),
    Data="/stuff/%%ts_test_search:namespace%%/%%ts_test_search:marketplace%%/%%ts_test_search:sessionBucket%%/01/2000?keyA1=dataA1&amp;keyB1=dataB1",
    ?assertMatch("/stuff/namespace2/6/91/01/2000?keyA1=dataA1&amp;keyB1=dataB1", ts_search:subst(Data,[])).

parse_subst_var_fun_test() ->
    myset_env(),
    Data=?FORMDATA,
    StrName="jsf_tree_64",
    Regexp = ?DEF_REGEXP_DYNVAR_BEGIN++ StrName ++?DEF_REGEXP_DYNVAR_END,%'
    [{Name,Value}] = ts_search:parse_dynvar([{regexp, 'jsf_tree_64', Regexp }],list_to_binary(Data)),
    ?assertMatch("H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA-MSFT", ts_search:subst("%%_jsf_tree_64%%-%%ts_test_search:new%%",[{Name,Value}])).

parse_subst_badregexp_sid_test() ->
    myset_env(),
    Data="HTTP/1.1 200 OK\r\nServer: nginx/0.7.65\r\nDate: Fri, 05 Feb 2010 08:13:29 GMT\r\nContent-Type: text/xml; charset=utf-8\r\nConnection: keep-alive\r\nContent-Length: 373\r\n\r\n<body polling=\"10\" ver=\"1.6\" secure=\"true\" wait=\"20\" requests=\"2\" hold=\"1\" sid=\"5bfd2b59-3144-4e62-993b-d05d2ae3bee9\" xmpp:version=\"1.0\" xmlns:stream=\"http://etherx.jabber.org/streams\" authid=\"b65b29eb-99c0-4afd-8f97-d6d20f4ddba2\" maxpause=\"10\" from=\"tigase-test\" inactivity=\"10\" ack=\"2995502128855\" xmlns:xmpp=\"urn:xmpp:xbosh\" xmlns=\"http://jabber.org/protocol/httpbind\"/>",
    Regexp = "sid=\".*?\"",
    [{Name,Value}] = ts_search:parse_dynvar([{regexp, sid, Regexp }],list_to_binary(Data)),
    ?assertEqual({sid,""},{Name,Value}).

parse_subst_regexp_sid_test() ->
    myset_env(),
    Data="HTTP/1.1 200 OK\r\nServer: nginx/0.7.65\r\nDate: Fri, 05 Feb 2010 08:13:29 GMT\r\nContent-Type: text/xml; charset=utf-8\r\nConnection: keep-alive\r\nContent-Length: 373\r\n\r\n<body polling=\"10\" ver=\"1.6\" secure=\"true\" wait=\"20\" requests=\"2\" hold=\"1\" sid=\"5bfd2b59-3144-4e62-993b-d05d2ae3bee9\" xmpp:version=\"1.0\" xmlns:stream=\"http://etherx.jabber.org/streams\" authid=\"b65b29eb-99c0-4afd-8f97-d6d20f4ddba2\" maxpause=\"10\" from=\"tigase-test\" inactivity=\"10\" ack=\"2995502128855\" xmlns:xmpp=\"urn:xmpp:xbosh\" xmlns=\"http://jabber.org/protocol/httpbind\"/>",
    Regexp = "sid=\"\\([^\"]*\\)\"",
    [{Name,Value}] = ts_search:parse_dynvar([{regexp, sid, Regexp }],list_to_binary(Data)),
    ?assertEqual({sid,"5bfd2b59-3144-4e62-993b-d05d2ae3bee9"},{Name,Value}).


dynvars_urandom_test() ->
    myset_env(),
    ?assertMatch(["qxvmvtglimieyhemzlxc"],ts_client:set_dynvars(urandom,{string,20},[toto],[])).

dynvars_urandom_neg_test() ->
    myset_env(),
    ?assertError(function_clause,ts_client:set_dynvars(urandom,{string,-3},[toto],[])).

dynvars_urandom2_test() ->
    myset_env(),
    ?assertMatch(["qxvmvtglimieyhemzlxc","qxvmvtglimieyhemzlxc"],ts_client:set_dynvars(urandom,{string,20},[toto,tutu],[])).

dynvars_random_test() ->
    myset_env(),
    [String] = ts_client:set_dynvars(random,{string,20},[toto],[]),
    ?assertMatch(20,length(String)).

dynvars_random2_test() ->
    myset_env(),
    [String,String2] = ts_client:set_dynvars(random,{string,20},[toto,titi],[]),
    ?assertMatch({20,20},{length(String),length(String2)}).



%%TODO: out of order..
%parse_dynvar_xpath_collection_test() ->
%    myset_env(),
%    Data="<html><body>"
%        " <img src=img0'>"
%         "<div><img src='img1'> "
%         "      <img src='img2'> "
%         " </div> "
%         " <img src='img3'> "
%         " <img src='img4'></body></html>",
%    XPath = "//img/@src",
%    Tree = mochiweb_html:parse(list_to_binary(Data)),
%    R = mochiweb_xpath:execute(XPath,Tree),
%    erlang:display(R),
%    Expected = [<<"img0">>,<<"img1">>,<<"img2">>,<<"img3">>,<<"img4">>],
%    ?assertMatch(Expected, R).


myset_env()->
    myset_env(0).
myset_env(Level)->
    application:set_env(stdlib,debug_level,Level).

new({Pid, DynData}) ->
    case random:uniform(3) of
        1 -> "IBM";
        2 -> "MSFT";
        3 -> "RHAT"
    end.

marketplace({Pid,DynData}) ->
    integer_to_list( random:uniform(7) ).

namespace({Pid,DynData}) ->
    "namespace" ++ integer_to_list(random:uniform(3)).

sessionBucket({Pid,DynData}) ->
    case random:uniform(96) of
        96 -> "00";
        X when X < 10  -> "0" ++ integer_to_list( X );
        X -> integer_to_list( X )
    end.
