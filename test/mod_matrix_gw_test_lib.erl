%%%----------------------------------------------------------------------
%%% File    : mod_matrix_gw_test_lib.erl
%%% Author  : Paavel A. <pd@narayana.im>
%%% Purpose : Start ejabberd in the eunit node for the mod_matrix_gw suites
%%% Created : 23 Nov 2026 by Paavel A. <pd@narayana.im>
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
%%%----------------------------------------------------------------------
-module(mod_matrix_gw_test_lib).
-export([in_peer/2, run_cases/1, start_ejabberd/1, stop_ejabberd/1]).

-include_lib("eunit/include/eunit.hrl").

%% The cases of Suite that need a running ejabberd, Cases() being their
%% eunit generator, run in a node of their own (peer) on this node's code
%% path, so that neither what earlier tests of this node left behind nor
%% what these leave reaches another suite.
in_peer(Suite, Cases) ->
    {timeout, 900,
     {setup, fun start_peer/0, fun peer:stop/1,
      fun(Peer) ->
              [{atom_to_list(Suite) ++ " on a running ejabberd",
                {timeout, 890,
                 fun() ->
                         case peer:call(Peer, ?MODULE, run_cases, [Cases], 880000) of
                             {ok, Log} ->
                                 Lines = binary:split(Log, <<"\n">>, [global, trim_all]),
                                 io:format(user, "~n  ~s: ~ts~n", [Suite, lists:last([<<>> | Lines])]);
                             {Result, Log} ->
                                 io:format(user, "~ts~n", [Log]),
                                 ?assertEqual(ok, Result)
                         end
                 end}}]
      end}}.

%% In the peer: the cases run with eunit's report kept, to be shown by
%% the node that asked when they fail.
run_cases(Cases) ->
    Log = filename:join(tmp_dir(), "mod_matrix_gw_eunit-" ++
                            integer_to_list(erlang:unique_integer([positive])) ++ ".log"),
    {ok, Dev} = file:open(Log, [write, {encoding, utf8}]),
    Old = group_leader(),
    group_leader(Dev, self()),
    Result = try eunit:test({generator, Cases}, [verbose])
             after group_leader(Old, self()), file:close(Dev)
             end,
    {ok, Text} = file:read_file(Log),
    _ = file:delete(Log),
    {Result, Text}.

start_peer() ->
    Paths = lists:append([["-pa", filename:absname(Path)] || Path <- code:get_path()]),
    {ok, Peer, _Node} = peer:start_link(#{connection => standard_io,
                                          args => ["+S", "2" | Paths]}),
    Peer.

%% Starts ejabberd with <Suite>.yml, found in the suite's source directory
%% or beside its beam; @DIR@ in it is a fresh directory of the test's own.
start_ejabberd(Suite) ->
    Dir = filename:join(tmp_dir(), atom_to_list(Suite) ++ "-" ++
                            integer_to_list(erlang:unique_integer([positive]))),
    ok = filelib:ensure_path(Dir ++ "/mnesia"),
    Config = Dir ++ "/ejabberd.yml",
    {ok, Yml} = file:read_file(template(Suite)),
    ok = file:write_file(Config, binary:replace(Yml, <<"@DIR@">>, list_to_binary(Dir), [global])),
    os:putenv("EJABBERD_CONFIG_PATH", Config),
    os:putenv("EJABBERD_LOG_PATH", Dir ++ "/ejabberd.log"),
    application:set_env(mnesia, dir, Dir ++ "/mnesia"),
    {ok, _} = application:ensure_all_started(ejabberd),
    Dir.

stop_ejabberd(_Dir) ->
    application:stop(ejabberd).

template(Suite) ->
    Name = atom_to_list(Suite) ++ ".yml",
    Candidates = [filename:join(filename:dirname(code:which(Suite)), Name)
                  | [filename:join(D, Name)
                     || D <- [proplists:get_value(source, Suite:module_info(compile), ""),
                              "test"],
                        D /= ""]],
    case [F || F <- [case filename:extension(C) of
                         ".erl" -> filename:rootname(C) ++ ".yml";
                         _ -> C
                     end || C <- Candidates], filelib:is_regular(F)] of
        [F | _] -> F;
        [] -> erlang:error({no_config, Name})
    end.

tmp_dir() ->
    case os:getenv("TMPDIR") of
        false -> "/tmp";
        "" -> "/tmp";
        T -> T
    end.
