%%%-------------------------------------------------------------------
%%% File    : p1_prof.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : Handly wrapper around eprof and fprof
%%%
%%% Created : 23 Jan 2010 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2010   ProcessOne
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
%%%-------------------------------------------------------------------
-module(p1_prof).

%% API
-export([eprof_start/0, eprof_stop/0,
	 fprof_start/0, fprof_start/1,
	 fprof_stop/0]).

-define(APPS, [ejabberd, mnesia]).

%%====================================================================
%% API
%%====================================================================
eprof_start() ->
    eprof:start(),
    case lists:keyfind(running, 1, application:info()) of
	{_, Apps} ->
	    case get_procs(?APPS, Apps) of
		[] ->
		    {error, no_procs_found};
		Procs ->
		    eprof:start_profiling(Procs)
	    end;
	_ ->
	    {error, no_app_info}
    end.

fprof_start() ->
    fprof_start(0).

fprof_start(Duration) ->
    case lists:keyfind(running, 1, application:info()) of
	{_, Apps} ->
	    case get_procs(?APPS, Apps) of
		[] ->
		    {error, no_procs_found};
		Procs ->
		    fprof:trace([start, {procs, Procs}]),
		    if Duration > 0 ->
			    timer:sleep(Duration*1000),
			    fprof:trace([stop]);
		       true->
			    ok
		    end
	    end;
	_ ->
	    {error, no_app_info}
    end.

fprof_stop() ->
    fprof:trace([stop]),
    fprof:profile(),
    fprof:analyse([totals, no_details, {sort, own},
		   no_callers, {dest, "fprof.analysis"}]),
    fprof:stop(),
    format_fprof_analyze().

eprof_stop() ->
    eprof:stop_profiling(),
    eprof:analyse().

%%====================================================================
%% Internal functions
%%====================================================================
get_procs(Apps, AppList) ->
    io:format("Searching for processes to profile...~n", []),
    Procs = lists:flatmap(
	      fun({App, Leader}) when is_pid(Leader) ->
		      case lists:member(App, Apps) of
			  true ->
			      get_procs(Leader);
			  false ->
			      []
		      end;
		 (_) ->
		      []
	      end, AppList),
    io:format("Found ~p processes~n", [length(Procs)]),
    Procs.

get_procs(Leader) ->
    lists:filter(
      fun(Pid) ->
	      case process_info(Pid, group_leader) of
		  {_, Leader} ->
		      true;
		  _ ->
		      false
	      end
      end, processes()).

format_fprof_analyze() ->
    case file:consult("fprof.analysis") of
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
