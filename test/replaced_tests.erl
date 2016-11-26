%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 16 Nov 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(replaced_tests).

%% API
-compile(export_all).
-import(suite, [bind/1, wait_for_slave/1, wait_for_master/1, recv/1,
		close_socket/1, disconnect/1]).

-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {replaced_single, [sequence], []}.

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {replaced_master_slave, [sequence], []}.
%% Disable tests for now due to a race condition
%% because ejabberd_sm:sid() is generated in ejabberd_s2s:init()
%%[master_slave_test(conflict)]}.

conflict_master(Config0) ->
    Config = bind(Config0),
    wait_for_slave(Config),
    #stream_error{reason = conflict} = recv(Config),
    {xmlstreamend, <<"stream:stream">>} = recv(Config),
    close_socket(Config).

conflict_slave(Config0) ->
    wait_for_master(Config0),
    Config = bind(Config0),
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("replaced_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("replaced_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("replaced_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("replaced_" ++ atom_to_list(T) ++ "_slave")]}.
