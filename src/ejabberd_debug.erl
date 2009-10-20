%%%----------------------------------------------------------------------
%%% File    : ejabberd_debug.erl
%%% Author  : Mickael Remond
%%% Purpose : ejabberd's application callback module
%%% Created : 6 may 2009 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
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

-module(ejabberd_debug).

-export([eprof_start/0, fprof_start/0, stop/0]).
-export([pids/0]).

eprof_start() ->
    eprof:start(),
    eprof:profile(pids()).

fprof_start() ->
    fprof:trace([start, {file, "/tmp/fprof"}, {procs, pids()}]).

%% Stop all profilers
stop() ->
    catch eprof:stop(),
    catch fprof:stop(),
    ok.

pids() ->
    lists:zf(
      fun(Pid) ->
	      case process_info(Pid) of
		  ProcessInfo when is_list(ProcessInfo) ->
		      CurrentFunction = current_function(ProcessInfo),
		      InitialCall = initial_call(ProcessInfo),
		      RegisteredName = registered_name(ProcessInfo),
                      Ancestor = ancestor(ProcessInfo),
		      filter_pid(Pid, CurrentFunction, InitialCall, RegisteredName, Ancestor);
		  _ ->
		      false
	      end
      end,
      processes()).

current_function(ProcessInfo) ->
    {value, {_, {CurrentFunction, _,_}}} =
	lists:keysearch(current_function, 1, ProcessInfo),
    atom_to_list(CurrentFunction).

initial_call(ProcessInfo) ->
    {value, {_, {InitialCall, _,_}}} =
	lists:keysearch(initial_call, 1, ProcessInfo),
    atom_to_list(InitialCall).

registered_name(ProcessInfo) ->
    case lists:keysearch(registered_name, 1, ProcessInfo) of
	{value, {_, Name}} when is_atom(Name) -> atom_to_list(Name);
	_ -> ""
    end.

ancestor(ProcessInfo) ->
    {value, {_, Dictionary}} = lists:keysearch(dictionary, 1, ProcessInfo),
    case lists:keysearch('$ancestors', 1, Dictionary) of
	{value, {_, [Ancestor|_T]}} when is_atom(Ancestor) ->
	    atom_to_list(Ancestor);
	_ ->
	    ""
    end.

filter_pid(Pid, "ejabberd" ++ _, _InitialCall, _RegisteredName, _Ancestor) ->
    {true, Pid};
filter_pid(Pid, _CurrentFunction, "ejabberd" ++ _, _RegisteredName, _Ancestor) ->
    {true, Pid};
filter_pid(Pid, _CurrentFunction, _InitialCall, "ejabberd"++_, _Ancestor) ->
    {true, Pid};
filter_pid(Pid, _CurrentFunction, _InitialCall, "stringprep"++_, _Ancestor) ->
    {true, Pid};
filter_pid(Pid, _CurrentFunction, _InitialCall, _RegisteredName, "ejabberd"++_) ->
    {true, Pid};
filter_pid(_Pid, _CurrentFunction, _InitialCall, _RegisteredName, _Ancestor) ->
    false.
