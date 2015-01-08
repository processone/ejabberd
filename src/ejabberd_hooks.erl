%%%----------------------------------------------------------------------
%%% File    : ejabberd_hooks.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage hooks
%%% Created :  8 Aug 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-module(ejabberd_hooks).
-author('alexey@process-one.net').

-behaviour(gen_server).

%% External exports
-export([start_link/0,
	 add/3,
	 add/4,
	 add_dist/5,
	 delete/3,
	 delete/4,
	 delete_dist/5,
	 run/2,
	 run_fold/3,
	 add/5,
	 add_dist/6,
	 delete/5,
	 delete_dist/6,
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
-include("logger.hrl").

%% Timeout of 5 seconds in calls to distributed hooks
-define(TIMEOUT_DISTRIBUTED_HOOK, 5000).

-record(state, {}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ejabberd_hooks}, ejabberd_hooks, [], []).

-spec add(atom(), fun(), number()) -> any().

%% @doc See add/4.
add(Hook, Function, Seq) when is_function(Function) ->
    add(Hook, global, undefined, Function, Seq).

-spec add(atom(), binary() | atom(), fun() | atom() , number()) -> any().
add(Hook, Host, Function, Seq) when is_function(Function) ->
    add(Hook, Host, undefined, Function, Seq);

%% @doc Add a module and function to this hook.
%% The integer sequence is used to sort the calls: low number is called before high number.
add(Hook, Module, Function, Seq) ->
    add(Hook, global, Module, Function, Seq).

-spec add(atom(), binary() | global, atom(), atom() | fun(), number()) -> any().

add(Hook, Host, Module, Function, Seq) ->
    gen_server:call(ejabberd_hooks, {add, Hook, Host, Module, Function, Seq}).

-spec add_dist(atom(), atom(), atom(), atom() | fun(), number()) -> any().

add_dist(Hook, Node, Module, Function, Seq) ->
    gen_server:call(ejabberd_hooks, {add, Hook, global, Node, Module, Function, Seq}).

-spec add_dist(atom(), binary() | global, atom(), atom(), atom() | fun(), number()) -> any().

add_dist(Hook, Host, Node, Module, Function, Seq) ->
    gen_server:call(ejabberd_hooks, {add, Hook, Host, Node, Module, Function, Seq}).

-spec delete(atom(), fun(), number()) -> ok.

%% @doc See del/4.
delete(Hook, Function, Seq) when is_function(Function) ->
    delete(Hook, global, undefined, Function, Seq).

-spec delete(atom(), binary() | atom(), atom() | fun(), number()) -> ok.

delete(Hook, Host, Function, Seq) when is_function(Function) ->
    delete(Hook, Host, undefined, Function, Seq);

%% @doc Delete a module and function from this hook.
%% It is important to indicate exactly the same information than when the call was added.
delete(Hook, Module, Function, Seq) ->
    delete(Hook, global, Module, Function, Seq).

-spec delete(atom(), binary() | global, atom(), atom() | fun(), number()) -> ok.

delete(Hook, Host, Module, Function, Seq) ->
    gen_server:call(ejabberd_hooks, {delete, Hook, Host, Module, Function, Seq}).

-spec delete_dist(atom(), atom(), atom(), atom() | fun(), number()) -> ok.

delete_dist(Hook, Node, Module, Function, Seq) ->
    delete_dist(Hook, global, Node, Module, Function, Seq).

-spec delete_dist(atom(), binary() | global, atom(), atom(), atom() | fun(), number()) -> ok.

delete_dist(Hook, Host, Node, Module, Function, Seq) ->
    gen_server:call(ejabberd_hooks, {delete, Hook, Host, Node, Module, Function, Seq}).

-spec run(atom(), list()) -> ok.

%% @doc Run the calls of this hook in order, don't care about function results.
%% If a call returns stop, no more calls are performed.
run(Hook, Args) ->
    run(Hook, global, Args).

-spec run(atom(), binary() | global, list()) -> ok.

run(Hook, Host, Args) ->
    case ets:lookup(hooks, {Hook, Host}) of
	[{_, Ls}] ->
	    run1(Ls, Hook, Args);
	[] ->
	    ok
    end.

-spec run_fold(atom(), any(), list()) -> any().

%% @doc Run the calls of this hook in order.
%% The arguments passed to the function are: [Val | Args].
%% The result of a call is used as Val for the next call.
%% If a call returns 'stop', no more calls are performed and 'stopped' is returned.
%% If a call returns {stop, NewVal}, no more calls are performed and NewVal is returned.
run_fold(Hook, Val, Args) ->
    run_fold(Hook, global, Val, Args).

-spec run_fold(atom(), binary() | global, any(), list()) -> any().

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
handle_call({add, Hook, Host, Node, Module, Function, Seq}, _From, State) ->
    Reply = case ets:lookup(hooks, {Hook, Host}) of
		[{_, Ls}] ->
		    El = {Seq, Node, Module, Function},
		    case lists:member(El, Ls) of
			true ->
			    ok;
			false ->
			    NewLs = lists:merge(Ls, [El]),
			    ets:insert(hooks, {{Hook, Host}, NewLs}),
			    ok
		    end;
		[] ->
		    NewLs = [{Seq, Node, Module, Function}],
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
handle_call({delete, Hook, Host, Node, Module, Function, Seq}, _From, State) ->
    Reply = case ets:lookup(hooks, {Hook, Host}) of
		[{_, Ls}] ->
		    NewLs = lists:delete({Seq, Node, Module, Function}, Ls),
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
run1([{_Seq, Node, Module, Function} | Ls], Hook, Args) ->
    case rpc:call(Node, Module, Function, Args, ?TIMEOUT_DISTRIBUTED_HOOK) of
	timeout ->
	    ?ERROR_MSG("Timeout on RPC to ~p~nrunning hook: ~p",
		       [Node, {Hook, Args}]),
	    run1(Ls, Hook, Args);
	{badrpc, Reason} ->
	    ?ERROR_MSG("Bad RPC error to ~p: ~p~nrunning hook: ~p",
		       [Node, Reason, {Hook, Args}]),
	    run1(Ls, Hook, Args);
	stop ->
	    ?INFO_MSG("~nThe process ~p in node ~p ran a hook in node ~p.~n"
		      "Stop.", [self(), node(), Node]), % debug code
	    ok;
	Res ->
	    ?INFO_MSG("~nThe process ~p in node ~p ran a hook in node ~p.~n"
		      "The response is:~n~s", [self(), node(), Node, Res]), % debug code
	    run1(Ls, Hook, Args)
    end;
run1([{_Seq, Module, Function} | Ls], Hook, Args) ->
    Res = if is_function(Function) ->
		  catch apply(Function, Args);
	     true ->
		  catch apply(Module, Function, Args)
	  end,
    case Res of
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
run_fold1([{_Seq, Node, Module, Function} | Ls], Hook, Val, Args) ->
    case rpc:call(Node, Module, Function, [Val | Args], ?TIMEOUT_DISTRIBUTED_HOOK) of
	{badrpc, Reason} ->
	    ?ERROR_MSG("Bad RPC error to ~p: ~p~nrunning hook: ~p",
		       [Node, Reason, {Hook, Args}]),
	    run_fold1(Ls, Hook, Val, Args);
	timeout ->
	    ?ERROR_MSG("Timeout on RPC to ~p~nrunning hook: ~p",
		       [Node, {Hook, Args}]),
	    run_fold1(Ls, Hook, Val, Args);
	stop ->
	    stopped;
	{stop, NewVal} ->
	    ?INFO_MSG("~nThe process ~p in node ~p ran a hook in node ~p.~n"
		      "Stop, and the NewVal is:~n~p", [self(), node(), Node, NewVal]), % debug code
	    NewVal;
	NewVal ->
	    ?INFO_MSG("~nThe process ~p in node ~p ran a hook in node ~p.~n"
		      "The NewVal is:~n~p", [self(), node(), Node, NewVal]), % debug code
	    run_fold1(Ls, Hook, NewVal, Args)
    end;
run_fold1([{_Seq, Module, Function} | Ls], Hook, Val, Args) ->
    Res = if is_function(Function) ->
		  catch apply(Function, [Val | Args]);
	     true ->
		  catch apply(Module, Function, [Val | Args])
	  end,
    case Res of
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
