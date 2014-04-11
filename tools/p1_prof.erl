%%%-------------------------------------------------------------------
%%% File    : p1_prof.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : Handy wrapper around eprof and fprof
%%%
%%% Created : 23 Jan 2010 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
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
-module(p1_prof).

%% API
-export([eprof_start/0, eprof_stop/0,
	 fprof_start/0, fprof_start/1,
	 fprof_stop/0, fprof_analyze/0,
	 queue/0, queue/1, memory/0, memory/1,
	 reds/0, reds/1, trace/1, help/0,
	 q/0, m/0, r/0, q/1, m/1, r/1]).

-define(TRACE_FILE, "/tmp/fprof.trace").
-define(ANALYSIS_FILE, "/tmp/fprof.analysis").

%%====================================================================
%% API
%%====================================================================
eprof_start() ->
    eprof:start(),
    case get_procs() of
        [] ->
            {error, no_procs_found};
        Procs ->
            eprof:start_profiling(Procs)
    end.

fprof_start() ->
    fprof_start(0).

fprof_start(Duration) ->
    case get_procs() of
        [] ->
            {error, no_procs_found};
        Procs ->
            case fprof:trace([start, {procs, Procs}, {file, ?TRACE_FILE}]) of
                ok ->
                    io:format("Profiling started, writing trace data to ~s~n",
                              [?TRACE_FILE]),
                    if Duration > 0 ->
                            timer:sleep(Duration*1000),
                            fprof:trace([stop]),
                            fprof:stop();
                       true->
                            ok
                    end;
                Err ->
                    io:format("Couldn't start profiling: ~p~n", [Err]),
                    Err
            end
    end.

fprof_stop() ->
    fprof:trace([stop]),
    case fprof:profile([{file, ?TRACE_FILE}]) of
        ok ->
            case fprof:analyse([totals, no_details, {sort, own},
                                no_callers, {dest, ?ANALYSIS_FILE}]) of
                ok ->
                    fprof:stop(),
                    format_fprof_analyze();
                Err ->
                    io:format("Couldn't analyze: ~p~n", [Err]),
                    Err
            end;
        Err ->
            io:format("Couldn't compile a trace into profile data: ~p~n",
                      [Err]),
            Err
    end.

fprof_analyze() ->
    fprof_stop().

eprof_stop() ->
    eprof:stop_profiling(),
    case erlang:function_exported(eprof, analyse, 0) of
	true ->
	    apply(eprof, analyse, []);
	false ->
	    eprof:analyze()
    end.

help() ->
    M = ?MODULE,
    io:format("Brief help:~n"
	      "~p:queue(N) - show top N pids sorted by queue length~n"
	      "~p:queue() - shorthand for ~p:queue(10)~n"
	      "~p:memory(N) - show top N pids sorted by memory usage~n"
	      "~p:memory() - shorthand for ~p:memory(10)~n"
	      "~p:reds(N) - show top N pids sorted by reductions~n"
	      "~p:reds() - shorthand for ~p:reds(10)~n"
	      "~p:q(N)|~p:q() - same as ~p:queue(N)|~p:queue()~n"
	      "~p:m(N)|~p:m() - same as ~p:memory(N)|~p:memory()~n"
	      "~p:r(N)|~p:r() - same as ~p:reds(N)|~p:reds()~n"
	      "~p:trace(Pid) - trace Pid; to stop tracing close "
	      "Erlang shell with Ctrl+C~n"
	      "~p:eprof_start() - start eprof on all available pids; "
	      "DO NOT use on production system!~n"
	      "~p:eprof_stop() - stop eprof and print result~n"
	      "~p:fprof_start() - start fprof on all available pids; "
	      "DO NOT use on production system!~n"
	      "~p:fprof_stop() - stop eprof and print formatted result~n"
	      "~p:fprof_start(N) - start and run fprof for N seconds; "
	      "use ~p:fprof_analyze() to analyze collected statistics and "
	      "print formatted result; use on production system with CARE~n"
	      "~p:fprof_analyze() - analyze previously collected statistics "
	      "using ~p:fprof_start(N) and print formatted result~n"
	      "~p:help() - print this help~n",
	      lists:duplicate(31, M)).

q() ->
    queue().

q(N) ->
    queue(N).

m() ->
    memory().

m(N) ->
    memory(N).

r() ->
    reds().

r(N) ->
    reds(N).

queue() ->
    queue(10).

memory() ->
    memory(10).

reds() ->
    reds(10).

queue(N) ->
    dump(N, lists:reverse(lists:ukeysort(1, all_pids(queue)))).

memory(N) ->
    dump(N, lists:reverse(lists:ukeysort(3, all_pids(memory)))).

reds(N) ->
    dump(N, lists:reverse(lists:ukeysort(4, all_pids(reductions)))).

trace(Pid) ->
    erlang:trace(Pid, true, [send, 'receive']),
    trace_loop().

trace_loop() ->
    receive
	M ->
	    io:format("~p~n", [M]),
	    trace_loop()
    end.

%%====================================================================
%% Internal functions
%%====================================================================
get_procs() ->
    processes().

format_fprof_analyze() ->
    case file:consult(?ANALYSIS_FILE) of
	{ok, [_, [{totals, _, _, TotalOWN}] | Rest]} ->
	    OWNs = lists:flatmap(
		     fun({MFA, _, _, OWN}) ->
			     Percent = OWN*100/TotalOWN,
			     case round(Percent) of
				 0 ->
				     [];
				 _ ->
				     [{mfa_to_list(MFA), Percent}]
			     end
		     end, Rest),
	    ACCs = collect_accs(Rest),
	    MaxACC = find_max(ACCs),
	    MaxOWN = find_max(OWNs),
	    io:format("=== Sorted by OWN:~n"),
	    lists:foreach(
	      fun({MFA, Per}) ->
		      L = length(MFA),
		      S = lists:duplicate(MaxOWN - L + 2, $ ),
		      io:format("~s~s~.2f%~n", [MFA, S, Per])
	      end, lists:reverse(lists:keysort(2, OWNs))),
	    io:format("~n=== Sorted by ACC:~n"),
	    lists:foreach(
	      fun({MFA, Per}) ->
		      L = length(MFA),
		      S = lists:duplicate(MaxACC - L + 2, $ ),
		      io:format("~s~s~.2f%~n", [MFA, S, Per])
	      end, lists:reverse(lists:keysort(2, ACCs)));
	Err ->
	    Err
    end.

mfa_to_list({M, F, A}) ->
    atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A);
mfa_to_list(F) when is_atom(F) ->
    atom_to_list(F).

find_max(List) ->
    find_max(List, 0).

find_max([{V, _}|Tail], Acc) ->
    find_max(Tail, lists:max([length(V), Acc]));
find_max([], Acc) ->
    Acc.

collect_accs(List) ->
    List1 = lists:filter(
	      fun({MFA, _, _, _}) ->
		      case MFA of
			  {sys, _, _} ->
			      false;
			  suspend ->
			      false;
			  {gen_fsm, _, _} ->
			      false;
			  {p1_fsm, _, _} ->
			      false;
			  {gen, _, _} ->
			      false;
			  {gen_server, _, _} ->
			      false;
			  {proc_lib, _, _} ->
			      false;
			  _ ->
			      true
		      end
	      end, List),
    TotalACC = lists:sum([A || {_, _, A, _} <- List1]),
    lists:flatmap(
      fun({MFA, _, ACC, _}) ->
	      Percent = ACC*100/TotalACC,
	      case round(Percent) of
		  0 ->
		      [];
		  _ ->
		      [{mfa_to_list(MFA), Percent}]
	      end
      end, List1).

all_pids(Type) ->
    lists:foldl(
      fun(P, Acc) when P == self() ->
	      %% exclude ourself from statistics
	      Acc;
	 (P, Acc) ->
	      case catch process_info(
			   P,
			   [message_queue_len,
			    memory,
			    reductions,
			    dictionary,
			    current_function,
			    registered_name]) of
		  [{_, Len}, {_, Memory}, {_, Reds},
		   {_, Dict}, {_, CurFun}, {_, RegName}] ->
                      IntQLen = case lists:keysearch('$internal_queue_len', 1, Dict) of
                                    {value, {_, N}} ->
                                        N;
                                    _ ->
                                        0
                                end,
		      if Type == queue andalso Len == 0 andalso IntQLen == 0 ->
			      Acc;
			 true ->
                              MaxLen = lists:max([Len, IntQLen]),
			      [{MaxLen, Len, Memory, Reds, Dict, CurFun, P, RegName}|Acc]
		      end;
		  _ ->
		      Acc
	      end
      end, [], processes()).

dump(N, Rs) ->
    lists:foreach(
      fun({_, MsgQLen, Memory, Reds, Dict, CurFun, Pid, RegName}) ->
	      PidStr = pid_to_list(Pid),
	      [_, Maj, Min] = string:tokens(
				string:substr(
				  PidStr, 2, length(PidStr) - 2), "."),
              io:format("** pid(0,~s,~s)~n"
			"** registered name: ~p~n"
			"** memory: ~p~n"
			"** reductions: ~p~n"
                        "** message queue len: ~p~n"
			"** current_function: ~p~n"
                        "** dictionary: ~p~n~n",
                        [Maj, Min, RegName, Memory, Reds, MsgQLen, CurFun, Dict])
      end, nthhead(N, Rs)).

nthhead(N, L) ->
    lists:reverse(nthhead(N, L, [])).

nthhead(0, _L, Acc) ->
    Acc;
nthhead(N, [H|T], Acc) ->
    nthhead(N-1, T, [H|Acc]);
nthhead(_N, [], Acc) ->
    Acc.
