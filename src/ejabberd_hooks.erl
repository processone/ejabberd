%%%----------------------------------------------------------------------
%%% File    : ejabberd_hooks.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage hooks
%%% Created :  8 Aug 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2020   ProcessOne
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
	 add/5,
	 delete/3,
	 delete/4,
	 delete/5,
	 run/2,
	 run/3,
	 run_fold/3,
	 run_fold/4]).
%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 code_change/3,
	 handle_info/2,
	 terminate/2]).

-include("logger.hrl").
-include("ejabberd_stacktrace.hrl").

-record(state, {}).
-type hook() :: {Seq :: integer(), Module :: atom(), Function :: atom() | fun()}.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add(atom(), fun(), integer()) -> ok.
%% @doc See add/4.
add(Hook, Function, Seq) when is_function(Function) ->
    add(Hook, global, undefined, Function, Seq).

-spec add(atom(), HostOrModule :: binary() | atom(), fun() | atom() , integer()) -> ok.
add(Hook, Host, Function, Seq) when is_function(Function) ->
    add(Hook, Host, undefined, Function, Seq);

%% @doc Add a module and function to this hook.
%% The integer sequence is used to sort the calls: low number is called before high number.
add(Hook, Module, Function, Seq) ->
    add(Hook, global, Module, Function, Seq).

-spec add(atom(), binary() | global, atom(), atom() | fun(), integer()) -> ok.
add(Hook, Host, Module, Function, Seq) ->
    gen_server:call(?MODULE, {add, Hook, Host, Module, Function, Seq}).

-spec delete(atom(), fun(), integer()) -> ok.
%% @doc See del/4.
delete(Hook, Function, Seq) when is_function(Function) ->
    delete(Hook, global, undefined, Function, Seq).

-spec delete(atom(), binary() | atom(), atom() | fun(), integer()) -> ok.
delete(Hook, Host, Function, Seq) when is_function(Function) ->
    delete(Hook, Host, undefined, Function, Seq);

%% @doc Delete a module and function from this hook.
%% It is important to indicate exactly the same information than when the call was added.
delete(Hook, Module, Function, Seq) ->
    delete(Hook, global, Module, Function, Seq).

-spec delete(atom(), binary() | global, atom(), atom() | fun(), integer()) -> ok.
delete(Hook, Host, Module, Function, Seq) ->
    gen_server:call(?MODULE, {delete, Hook, Host, Module, Function, Seq}).

-spec run(atom(), list()) -> ok.
%% @doc Run the calls of this hook in order, don't care about function results.
%% If a call returns stop, no more calls are performed.
run(Hook, Args) ->
    run(Hook, global, Args).

-spec run(atom(), binary() | global, list()) -> ok.
run(Hook, Host, Args) ->
    try ets:lookup(hooks, {Hook, Host}) of
	[{_, Ls}] ->
	    run1(Ls, Hook, Args);
	[] ->
	    ok
    catch _:badarg ->
	    ok
    end.

-spec run_fold(atom(), T, list()) -> T.
%% @doc Run the calls of this hook in order.
%% The arguments passed to the function are: [Val | Args].
%% The result of a call is used as Val for the next call.
%% If a call returns 'stop', no more calls are performed.
%% If a call returns {stop, NewVal}, no more calls are performed and NewVal is returned.
run_fold(Hook, Val, Args) ->
    run_fold(Hook, global, Val, Args).

-spec run_fold(atom(), binary() | global, T, list()) -> T.
run_fold(Hook, Host, Val, Args) ->
    try ets:lookup(hooks, {Hook, Host}) of
	[{_, Ls}] ->
	    run_fold1(Ls, Hook, Val, Args);
	[] ->
	    Val
    catch _:badarg ->
	    Val
    end.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------
init([]) ->
    _ = ets:new(hooks, [named_table, {read_concurrency, true}]),
    {ok, #state{}}.

handle_call({add, Hook, Host, Module, Function, Seq}, _From, State) ->
    HookFormat = {Seq, Module, Function},
    Reply = handle_add(Hook, Host, HookFormat),
    {reply, Reply, State};
handle_call({delete, Hook, Host, Module, Function, Seq}, _From, State) ->
    HookFormat = {Seq, Module, Function},
    Reply = handle_delete(Hook, Host, HookFormat),
    {reply, Reply, State};
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_add(atom(), atom(), hook()) -> ok.
handle_add(Hook, Host, El) ->
    case ets:lookup(hooks, {Hook, Host}) of
        [{_, Ls}] ->
            case lists:member(El, Ls) of
                true ->
                    ok;
                false ->
                    NewLs = lists:merge(Ls, [El]),
                    ets:insert(hooks, {{Hook, Host}, NewLs}),
                    ok
            end;
        [] ->
            NewLs = [El],
            ets:insert(hooks, {{Hook, Host}, NewLs}),
            ok
    end.

-spec handle_delete(atom(), atom(), hook()) -> ok.
handle_delete(Hook, Host, El) ->
    case ets:lookup(hooks, {Hook, Host}) of
        [{_, Ls}] ->
            NewLs = lists:delete(El, Ls),
            ets:insert(hooks, {{Hook, Host}, NewLs}),
            ok;
        [] ->
            ok
    end.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
-spec run1([hook()], atom(), list()) -> ok.
run1([], _Hook, _Args) ->
    ok;
run1([{_Seq, Module, Function} | Ls], Hook, Args) ->
    Res = safe_apply(Hook, Module, Function, Args),
    case Res of
	'EXIT' ->
	    run1(Ls, Hook, Args);
	stop ->
	    ok;
	_ ->
	    run1(Ls, Hook, Args)
    end.

-spec run_fold1([hook()], atom(), T, list()) -> T.
run_fold1([], _Hook, Val, _Args) ->
    Val;
run_fold1([{_Seq, Module, Function} | Ls], Hook, Val, Args) ->
    Res = safe_apply(Hook, Module, Function, [Val | Args]),
    case Res of
	'EXIT' ->
	    run_fold1(Ls, Hook, Val, Args);
	stop ->
	    Val;
	{stop, NewVal} ->
	    NewVal;
	NewVal ->
	    run_fold1(Ls, Hook, NewVal, Args)
    end.

-spec safe_apply(atom(), atom(), atom() | fun(), list()) -> any().
safe_apply(Hook, Module, Function, Args) ->
    ?DEBUG("Running hook ~p: ~p:~p/~B",
	   [Hook, Module, Function, length(Args)]),
    try if is_function(Function) ->
		apply(Function, Args);
       true ->
		apply(Module, Function, Args)
	end
    catch ?EX_RULE(E, R, St) when E /= exit; R /= normal ->
	    Stack = ?EX_STACK(St),
	    ?ERROR_MSG("Hook ~p crashed when running ~p:~p/~p:~n" ++
			   string:join(
			     ["** ~ts"|
			      ["** Arg " ++ integer_to_list(I) ++ " = ~p"
			       || I <- lists:seq(1, length(Args))]],
			     "~n"),
		       [Hook, Module, Function, length(Args),
			misc:format_exception(2, E, R, Stack)|Args]),
	    'EXIT'
    end.
