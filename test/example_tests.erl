%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 16 Nov 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
