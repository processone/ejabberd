%%%----------------------------------------------------------------------
%%% File    : ejabberd_hooks.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Manage hooks
%%% Created :  8 Aug 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_hooks).
-author('alexey@sevcom.net').

-behaviour(gen_server).

%% External exports
-export([start_link/0,
	 add/4,
	 delete/4,
	 run/2,
	 run_fold/3]).

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
    gen_server:call(ejabberd_hooks, {add, Hook, Module, Function, Seq}).

delete(Hook, Module, Function, Seq) ->
    gen_server:call(ejabberd_hooks, {delete, Hook, Module, Function, Seq}).

run(Hook, Args) ->
    case ets:lookup(hooks, Hook) of
	[{_, Ls}] ->
	    run1(Ls, Hook, Args);
	[] ->
	    ok
    end.

run_fold(Hook, Val, Args) ->
    case ets:lookup(hooks, Hook) of
	[{_, Ls}] ->
	    run_fold1(Ls, Hook, Val, Args);
	[] ->
	    ok
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
handle_call({add, Hook, Module, Function, Seq}, From, State) ->
    Reply = case ets:lookup(hooks, Hook) of
		[{_, Ls}] ->
		    El = {Seq, Module, Function},
		    case lists:member(El, Ls) of
			true ->
			    ok;
			false ->
			    NewLs = lists:merge(Ls, [El]),
			    ets:insert(hooks, {Hook, NewLs}),
			    ok
		    end;
		[] ->
		    NewLs = [{Seq, Module, Function}],
		    ets:insert(hooks, {Hook, NewLs}),
		    ok
	    end,
    {reply, Reply, State};
handle_call({delete, Hook, Module, Function, Seq}, From, State) ->
    Reply = case ets:lookup(hooks, Hook) of
		[{_, Ls}] ->
		    NewLs = lists:delete({Seq, Module, Function}, Ls),
		    ets:insert(hooks, {Hook, NewLs}),
		    ok;
		[] ->
		    ok
	    end,
    {reply, Reply, State};
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

run1([], Hook, Args) ->
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


run_fold1([], Hook, Val, Args) ->
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



