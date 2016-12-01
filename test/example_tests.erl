%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 16 Nov 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(example_tests).

%% API
-compile(export_all).
-import(suite, []).

-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {example_single, [sequence],
     [single_test(foo)]}.

foo(Config) ->
    Config.

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {example_master_slave, [sequence],
     [master_slave_test(foo)]}.

foo_master(Config) ->
    Config.

foo_slave(Config) ->
    Config.

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("example_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("example_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("example_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("example_" ++ atom_to_list(T) ++ "_slave")]}.
