%%%----------------------------------------------------------------------
%%% File    : ejabberd_hooks.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage hooks
%%% Created :  8 Aug 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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

-module(ejabberd_hooks).
-author('alexey@process-one.net').

-behaviour(gen_server).

%% External exports
-export([start_link/0,
	 add/4,
	 delete/4,
	 run/2,
	 run_fold/3,
	 add/5,
	 delete/5,
	 run/3,
	 run_fold/4]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 code_change/3,
	 handle_info/2,
	 terminate/2]).

-include("ejabberd.hrl").

-record(state, {}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ejabberd_hooks}, ejabberd_hooks, [], []).

add(Hook, Module, Function, Seq) ->
    add(Hook, global, Module, Function, Seq).

add(Hook, Host, Module, Function, Seq) ->
    gen_server:call(ejabberd_hooks, {add, Hook, Host, Module, Function, Seq}).

delete(Hook, Module, Function, Seq) ->
    delete(Hook, global, Module, Function, Seq).

delete(Hook, Host, Module, Function, Seq) ->
    gen_server:call(ejabberd_hooks, {delete, Hook, Host, Module, Function, Seq}).

run(Hook, Args) ->
    run(Hook, global, Args).

run(Hook, Host, Args) ->
    case ets:lookup(hooks, {Hook, Host}) of
	[{_, Ls}] ->
	    run1(Ls, Hook, Args);
	[] ->
	    ok
    end.

run_fold(Hook, Val, Args) ->
    run_fold(Hook, global, Val, Args).

run_fold(Hook, Host, Val, Args) ->
    case ets:lookup(hooks, {Hook, Host}) of
	[{_, Ls}] ->
	    run_fold1(Ls, Hook, Val, Args);
	[] ->
	    Val
    end.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    ets:new(hooks, [named_table]),
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({add, Hook, Host, Module, Function, Seq}, _From, State) ->
    Reply = case ets:lookup(hooks, {Hook, Host}) of
		[{_, Ls}] ->
		    El = {Seq, Module, Function},
		    case lists:member(El, Ls) of
			true ->
			    ok;
			false ->
			    NewLs = lists:merge(Ls, [El]),
			    ets:insert(hooks, {{Hook, Host}, NewLs}),
			    ok
		    end;
		[] ->
		    NewLs = [{Seq, Module, Function}],
		    ets:insert(hooks, {{Hook, Host}, NewLs}),
		    ok
	    end,
    {reply, Reply, State};
handle_call({delete, Hook, Host, Module, Function, Seq}, _From, State) ->
    Reply = case ets:lookup(hooks, {Hook, Host}) of
		[{_, Ls}] ->
		    NewLs = lists:delete({Seq, Module, Function}, Ls),
		    ets:insert(hooks, {{Hook, Host}, NewLs}),
		    ok;
		[] ->
		    ok
	    end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

run1([], _Hook, _Args) ->
    ok;
run1([{_Seq, Module, Function} | Ls], Hook, Args) ->
    case catch apply(Module, Function, Args) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nrunning hook: ~p",
		       [Reason, {Hook, Args}]),
	    run1(Ls, Hook, Args);
	stop ->
	    ok;
	_ ->
	    run1(Ls, Hook, Args)
    end.


run_fold1([], _Hook, Val, _Args) ->
    Val;
run_fold1([{_Seq, Module, Function} | Ls], Hook, Val, Args) ->
    case catch apply(Module, Function, [Val | Args]) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nrunning hook: ~p",
		       [Reason, {Hook, Args}]),
	    run_fold1(Ls, Hook, Val, Args);
	stop ->
	    stopped;
	{stop, NewVal} ->
	    NewVal;
	NewVal ->
	    run_fold1(Ls, Hook, NewVal, Args)
    end.



