%%%-------------------------------------------------------------------
%%% Author  : Badlop <badlop@process-one.net>
%%% Created : 2 Jul 2024 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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

%% @format-begin

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
      single_test(http_list_tuple_map)]}.

setup(_Config) ->
    M = <<"mod_example">>,
    execute(module_uninstall, [M]),
    case execute(module_install, [M]) of
        ok ->
            ok;
        {error, not_available} ->
            ?match(ok, execute(modules_update_specs, [])),
            ?match(ok, execute(module_install, [M]))
    end,
    Installed = execute(modules_installed, []),
    ?match(true, lists:keymember(mod_example, 1, Installed)).

%%%==================================
%%%% ejabberdctl

%% TODO: find a way to read and check the output printed in the command line
ejabberdctl(_Config) ->
    R = ejabberd_ctl:process(["modules_installed"]),
    ct:pal("ejabberdctl: R ~p", [R]),
    ct:pal("ejabberdctl: Q ~p", [os:cmd("ejabberdctl modules_installed")]),
    Installed = execute(modules_installed, []),
    ?match(true, lists:keymember(mod_example, 1, Installed)).

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
    ListB = [<<"one">>, <<"first">>, <<"primary">>],
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
           #{element1 => "dos", element2 => "two"},
           #{element1 => "three", element2 => "tres"}],
    LTB = [#{<<"element1">> => <<"one">>, <<"element2">> => <<"uno">>},
           #{<<"element1">> => <<"dos">>, <<"element2">> => <<"two">>},
           #{<<"element1">> => <<"three">>, <<"element2">> => <<"tres">>}],
    ?match(LTB, query(Config, "command_test_list_tuple", #{arg_list => LTA})),
    ?match(LTB, query(Config, "command_test_list_tuple", #{arg_list => LTB})).

http_list_tuple_map(Config) ->
    LTA = #{<<"one">> => <<"uno">>, <<"dos">> => <<"two">>, <<"three">> => <<"tres">>},
    LTB = lists:sort([#{<<"element1">> => <<"one">>, <<"element2">> => <<"uno">>},
                      #{<<"element1">> => <<"dos">>, <<"element2">> => <<"two">>},
                      #{<<"element1">> => <<"three">>, <<"element2">> => <<"tres">>}]),
    ?match(LTB, lists:sort(query(Config, "command_test_list_tuple", #{arg_list => LTA}))).

%%%==================================
%%%% internal functions

single_test(T) ->
    list_to_atom("commands_" ++ atom_to_list(T)).

execute(Name, Args) ->
    ejabberd_commands:execute_command2(Name, Args, #{caller_module => ejabberd_ctl}, 1000000).

page(Config, Tail) ->
    Server = ?config(server_host, Config),
    Port = ct:get_config(web_port, 5280),
    "http://" ++ Server ++ ":" ++ integer_to_list(Port) ++ "/api/" ++ Tail.

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

%%%==================================

%%% vim: set foldmethod=marker foldmarker=%%%%,%%%=:
