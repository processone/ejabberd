%%%-------------------------------------------------------------------
%%% Author  : Badlop <badlop@process-one.net>
%%% Created : 2 Jul 2024 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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
%%%-------------------------------------------------------------------

%%%% definitions

-module(commands_tests).

-compile(export_all).

-include("suite.hrl").

%%%==================================
%%%% setup

single_cases() ->
    {commands_single,
     [sequence],
     [single_test(setup),
      single_test(ejabberdctl),
      single_test(http_integer),
      single_test(http_string),
      single_test(http_binary),
      single_test(http_atom),
      single_test(http_rescode),
      single_test(http_restuple),
      single_test(http_list),
      single_test(http_tuple),
      single_test(http_list_tuple),
      single_test(http_list_tuple_map),
      single_test(adhoc_list_commands),
      single_test(adhoc_apiversion),
      single_test(adhoc_apizero),
      single_test(adhoc_apione),
      single_test(adhoc_integer),
      single_test(adhoc_string),
      single_test(adhoc_binary),
      single_test(adhoc_tuple),
      single_test(adhoc_list),
      single_test(adhoc_list_tuple),
      single_test(adhoc_atom),
      single_test(adhoc_rescode),
      single_test(adhoc_restuple),
      %%single_test(adhoc_all),
      single_test(clean)]}.

%% @format-begin

single_test(T) ->
    list_to_atom("commands_" ++ atom_to_list(T)).

setup(_Config) ->
    M = <<"mod_example">>,
    clean(_Config),
    case execute(module_install, [M]) of
        ok ->
            ok;
        {error, not_available} ->
            ?match(ok, execute(modules_update_specs, [])),
            ?match(ok, execute(module_install, [M]))
    end,
    Installed = execute(modules_installed, []),
    ?match(true, lists:keymember(mod_example, 1, Installed)).

clean(_Config) ->
    M = <<"mod_example">>,
    execute(module_uninstall, [M]),
    Installed = execute(modules_installed, []),
    ?match(false, lists:keymember(mod_example, 1, Installed)).

%%%==================================
%%%% ejabberdctl

%% TODO: find a way to read and check the output printed in the command line
ejabberdctl(_Config) ->
    R = ejabberd_ctl:process(["modules_installed"]),
    ct:pal("ejabberdctl: R ~p", [R]),
    ct:pal("ejabberdctl: Q ~p", [os:cmd("ejabberdctl modules_installed")]),
    Installed = execute(modules_installed, []),
    ?match(true, lists:keymember(mod_example, 1, Installed)).

execute(Name, Args) ->
    ejabberd_commands:execute_command2(Name, Args, #{caller_module => ejabberd_ctl}, 1000000).

%%%==================================
%%%% mod_http_api

http_integer(Config) ->
    Integer = 123456789,
    ?match(Integer, query(Config, "command_test_integer", #{arg_integer => Integer})).

http_string(Config) ->
    S = "This is a string.",
    B = iolist_to_binary(S),
    ?match(B, query(Config, "command_test_string", #{arg_string => S})),
    ?match(B, query(Config, "command_test_string", #{arg_string => B})).

http_binary(Config) ->
    B = <<"This is a binary.">>,
    ?match(B, query(Config, "command_test_binary", #{arg_binary => B})).

%% mod_http_api doesn't handle 'atom' result format
%% and formats the result as a binary by default
http_atom(Config) ->
    S = "Some string.",
    B = <<"Some string.">>,
    ?match(B, query(Config, "command_test_atom", #{arg_string => S})),
    ?match(B, query(Config, "command_test_atom", #{arg_string => B})).

http_rescode(Config) ->
    ?match(0, query(Config, "command_test_rescode", #{code => "true"})),
    ?match(0, query(Config, "command_test_rescode", #{code => "ok"})),
    ?match(1, query(Config, "command_test_rescode", #{code => "problem"})),
    ?match(1, query(Config, "command_test_rescode", #{code => "error"})).

http_restuple(Config) ->
    ?match(<<"Deleted 0 users: []">>, query(Config, "delete_old_users", #{days => 99})),
    ?match(<<"Good">>,
           query(Config, "command_test_restuple", #{code => "true", text => "Good"})),
    ?match(<<"OK!!">>,
           query(Config, "command_test_restuple", #{code => "ok", text => "OK!!"})).

http_list(Config) ->
    ListS = ["one", "first", "primary"],
    ListB = lists:sort([<<"one">>, <<"first">>, <<"primary">>]),
    ?match(ListB, query(Config, "command_test_list", #{arg_list => ListS})),
    ?match(ListB, query(Config, "command_test_list", #{arg_list => ListB})).

http_tuple(Config) ->
    MapA =
        #{element1 => "one",
          element2 => "first",
          element3 => "primary"},
    MapB =
        #{<<"element1">> => <<"one">>,
          <<"element2">> => <<"first">>,
          <<"element3">> => <<"primary">>},
    ?match(MapB, query(Config, "command_test_tuple", #{arg_tuple => MapA})),
    ?match(MapB, query(Config, "command_test_tuple", #{arg_tuple => MapB})).

http_list_tuple(Config) ->
    LTA = [#{element1 => "one", element2 => "uno"},
           #{element1 => "two", element2 => "dos"},
           #{element1 => "three", element2 => "tres"}],
    LTB = lists:sort([#{<<"element1">> => <<"one">>, <<"element2">> => <<"uno">>},
                      #{<<"element1">> => <<"two">>, <<"element2">> => <<"dos">>},
                      #{<<"element1">> => <<"three">>, <<"element2">> => <<"tres">>}]),
    ?match(LTB, query(Config, "command_test_list_tuple", #{arg_list => LTA})),
    ?match(LTB, query(Config, "command_test_list_tuple", #{arg_list => LTB})).

http_list_tuple_map(Config) ->
    LTA = #{<<"one">> => <<"uno">>,
            <<"two">> => <<"dos">>,
            <<"three">> => <<"tres">>},
    LTB = lists:sort([#{<<"element1">> => <<"one">>, <<"element2">> => <<"uno">>},
                      #{<<"element1">> => <<"two">>, <<"element2">> => <<"dos">>},
                      #{<<"element1">> => <<"three">>, <<"element2">> => <<"tres">>}]),
    ?match(LTB, lists:sort(query(Config, "command_test_list_tuple", #{arg_list => LTA}))).

%%% internal functions

query(Config, Tail, Map) ->
    BodyQ = misc:json_encode(Map),
    Body = make_query(Config, Tail, BodyQ),
    misc:json_decode(Body).

make_query(Config, Tail, BodyQ) ->
    ?match({ok, {{"HTTP/1.1", 200, _}, _, Body}},
           httpc:request(post,
                         {page(Config, Tail), [], "application/json", BodyQ},
                         [],
                         [{body_format, binary}]),
           Body).

page(Config, Tail) ->
    Server = ?config(server_host, Config),
    Port = ct:get_config(web_port, 5280),
    "http://" ++ Server ++ ":" ++ integer_to_list(Port) ++ "/api/" ++ Tail.

%%%==================================
%%%% ad-hoc

%%% list commands

adhoc_list_commands(Config) ->
    {ok, Result} = get_items(Config, <<"api-commands">>),
    {value, #disco_item{name = <<"command_test_binary">>}} =
        lists:keysearch(<<"command_test_binary">>, #disco_item.name, Result),
    suite:disconnect(Config).

get_items(Config, Node) ->
    case suite:send_recv(Config,
                         #iq{type = get,
                             to = server_jid(Config),
                             sub_els = [#disco_items{node = Node}]})
    of
        #iq{type = result, sub_els = [#disco_items{node = Node, items = Items}]} ->
            {ok, Items};
        #iq{type = result, sub_els = []} ->
            {empty, []};
        #iq{type = error} = Err ->
            xmpp:get_error(Err)
    end.

%%% apiversion

adhoc_apiversion(Config) ->
    Node = <<"api-commands/command_test_apiversion">>,
    ArgFields = make_fields_args([]),
    ResFields = make_fields_res([{<<"apiversion">>, <<"2">>}]),
    {ok, Sid, _FormFields} = get_form(Config, Node),
    ?match({ok, ResFields}, set_form(Config, Node, Sid, ArgFields)),
    suite:disconnect(Config).

%%% apizero

adhoc_apizero(Config) ->
    Node = <<"api-commands/command_test_apizero">>,
    ArgFields = make_fields_args([]),
    ResFields = make_fields_res([{<<"apiversion">>, <<"0">>}]),
    {ok, Sid, _FormFields} = get_form(Config, Node),
    ?match({ok, ResFields}, set_form(Config, Node, Sid, ArgFields)),
    suite:disconnect(Config).

%%% apione

adhoc_apione(Config) ->
    Node = <<"api-commands/command_test_apione">>,
    ArgFields = make_fields_args([]),
    ResFields = make_fields_res([{<<"apiversion">>, <<"1">>}]),
    {ok, Sid, _FormFields} = get_form(Config, Node),
    ?match({ok, ResFields}, set_form(Config, Node, Sid, ArgFields)),
    suite:disconnect(Config).

%%% integer

adhoc_integer(Config) ->
    Node = <<"api-commands/command_test_integer">>,
    ArgFields = make_fields_args([{<<"arg_integer">>, <<"12345">>}]),
    ResFields = make_fields_res([{<<"res_integer">>, <<"12345">>}]),
    {ok, Sid, _FormFields} = get_form(Config, Node),
    ?match({ok, ResFields}, set_form(Config, Node, Sid, ArgFields)),
    suite:disconnect(Config).

%%% string

adhoc_string(Config) ->
    Node = <<"api-commands/command_test_string">>,
    ArgFields = make_fields_args([{<<"arg_string">>, <<"Some string.">>}]),
    ResFields = make_fields_res([{<<"res_string">>, <<"Some string.">>}]),
    {ok, Sid, _FormFields} = get_form(Config, Node),
    ?match({ok, ResFields}, set_form(Config, Node, Sid, ArgFields)),
    suite:disconnect(Config).

%%% binary

adhoc_binary(Config) ->
    Node = <<"api-commands/command_test_binary">>,
    ArgFields = make_fields_args([{<<"arg_binary">>, <<"Some binary.">>}]),
    ResFields = make_fields_res([{<<"res_string">>, <<"Some binary.">>}]),
    {ok, Sid, _FormFields} = get_form(Config, Node),
    ?match({ok, ResFields}, set_form(Config, Node, Sid, ArgFields)),
    suite:disconnect(Config).

%%% tuple

adhoc_tuple(Config) ->
    Node = <<"api-commands/command_test_tuple">>,
    ArgFields = make_fields_args([{<<"arg_tuple">>, <<"one:two:three">>}]),
    {ok, Sid, _FormFields} = get_form(Config, Node),
    ?match({ok,
            [{xdata_field,
              <<"res_tuple">>,
              'text-single',
              <<"res_tuple">>,
              false,
              <<" {element1 : element2 : element3}">>,
              [<<"one : two : three">>],
              [],
              []}]},
           set_form(Config, Node, Sid, ArgFields)),
    suite:disconnect(Config).

%%% list

adhoc_list(Config) ->
    Node = <<"api-commands/command_test_list">>,
    ArgFields = make_fields_args([{<<"arg_list">>, [<<"one">>, <<"first">>, <<"primary">>]}]),
    ResFields =
        make_fields_res([{<<"res_list">>, lists:sort([<<"one">>, <<"first">>, <<"primary">>])}]),
    {ok, Sid, _FormFields} = get_form(Config, Node),
    ?match({ok, ResFields}, set_form(Config, Node, Sid, ArgFields)),
    suite:disconnect(Config).

%%% list_tuple

adhoc_list_tuple(Config) ->
    Node = <<"api-commands/command_test_list_tuple">>,
    ArgFields =
        make_fields_args([{<<"arg_list">>, [<<"one:uno">>, <<"two:dos">>, <<"three:tres">>]}]),
    ResFields =
        make_fields_res([{<<"res_list">>,
                          lists:sort([<<"one : uno">>, <<"two : dos">>, <<"three : tres">>])}]),
    {ok, Sid, _FormFields} = get_form(Config, Node),
    ?match({ok, ResFields}, set_form(Config, Node, Sid, ArgFields)),
    suite:disconnect(Config).

%%% atom

adhoc_atom(Config) ->
    Node = <<"api-commands/command_test_atom">>,
    ArgFields = make_fields_args([{<<"arg_string">>, <<"a_test_atom">>}]),
    ResFields = make_fields_res([{<<"res_atom">>, <<"a_test_atom">>}]),
    {ok, Sid, _FormFields} = get_form(Config, Node),
    ?match({ok, ResFields}, set_form(Config, Node, Sid, ArgFields)),
    suite:disconnect(Config).

%%% rescode

adhoc_rescode(Config) ->
    Node = <<"api-commands/command_test_rescode">>,
    ArgFields = make_fields_args([{<<"code">>, <<"ok">>}]),
    ResFields = make_fields_res([{<<"res_atom">>, <<"0">>}]),
    {ok, Sid, _FormFields} = get_form(Config, Node),
    ?match({ok, ResFields}, set_form(Config, Node, Sid, ArgFields)),
    suite:disconnect(Config).

%%% restuple

adhoc_restuple(Config) ->
    Node = <<"api-commands/command_test_restuple">>,
    ArgFields =
        make_fields_args([{<<"code">>, <<"ok">>}, {<<"text">>, <<"Just a result text">>}]),
    ResFields = make_fields_res([{<<"res_atom">>, <<"Just a result text">>}]),
    {ok, Sid, _FormFields} = get_form(Config, Node),
    ?match({ok, ResFields}, set_form(Config, Node, Sid, ArgFields)),
    suite:disconnect(Config).

%%% internal functions

server_jid(Config) ->
    jid:make(<<>>, ?config(server, Config), <<>>).

make_fields_args(Fields) ->
    lists:map(fun ({Var, Values}) when is_list(Values) ->
                      #xdata_field{label = Var,
                                   var = Var,
                                   required = true,
                                   type = 'text-multi',
                                   values = Values};
                  ({Var, Value}) ->
                      #xdata_field{label = Var,
                                   var = Var,
                                   required = true,
                                   type = 'text-single',
                                   values = [Value]}
              end,
              Fields).

make_fields_res(Fields) ->
    lists:map(fun ({Var, Values}) when is_list(Values) ->
                      #xdata_field{label = Var,
                                   var = Var,
                                   type = 'text-multi',
                                   values = Values};
                  ({Var, Value}) ->
                      #xdata_field{label = Var,
                                   var = Var,
                                   type = 'text-single',
                                   values = [Value]}
              end,
              Fields).

get_form(Config, Node) ->
    case suite:send_recv(Config,
                         #iq{type = set,
                             to = server_jid(Config),
                             sub_els = [#adhoc_command{node = Node}]})
    of
        #iq{type = result,
            sub_els =
                [#adhoc_command{node = Node,
                                action = execute,
                                status = executing,
                                sid = Sid,
                                actions = #adhoc_actions{execute = complete, complete = true},
                                xdata = #xdata{fields = Fields}}]} ->
            {ok, Sid, [F || F <- Fields, F#xdata_field.type /= fixed]};
        #iq{type = error} = Err ->
            xmpp:get_error(Err)
    end.

set_form(Config, Node, Sid, ArgFields) ->
    Xdata = #xdata{type = submit, fields = ArgFields},
    case suite:send_recv(Config,
                         #iq{type = set,
                             to = server_jid(Config),
                             sub_els =
                                 [#adhoc_command{node = Node,
                                                 action = complete,
                                                 sid = Sid,
                                                 xdata = Xdata}]})
    of
        #iq{type = result,
            sub_els =
                [#adhoc_command{node = Node,
                                action = execute,
                                status = completed,
                                sid = Sid,
                                xdata = #xdata{fields = ResFields}}]} ->
            ResFields2 = [F || F <- ResFields, F#xdata_field.type /= fixed],
            {ok, ResFields2 -- ArgFields};
        #iq{type = error} = Err ->
            xmpp:get_error(Err)
    end.

%%%==================================

%%% vim: set foldmethod=marker foldmarker=%%%%,%%%=:
