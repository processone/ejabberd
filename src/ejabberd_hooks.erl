%%%----------------------------------------------------------------------
%%% File    : ejabberd_hooks.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage hooks
%%% Created :  8 Aug 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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


-export(
    [
        get_tracing_options/3,
        trace_off/3,
        trace_on/5,human_readable_time_string/1
    ]
).

-include("logger.hrl").
-include("ejabberd_stacktrace.hrl").

-record(state, {}).
-type hook() :: {Seq :: integer(), Module :: atom(), Function :: atom() | fun()}.

-define(TRACE_HOOK_KEY, '$trace_hook').
-define(TIMING_KEY, '$trace_hook_timer').
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
        case erlang:get(?TRACE_HOOK_KEY) of
            undefined ->
                run1(Ls, Hook, Args);
            TracingHooksOpts ->
                case do_get_tracing_options(Hook, Host, TracingHooksOpts) of
                    undefined ->
                        run1(Ls, Hook, Args);
                    TracingOpts ->
                        foreach_start_hook_tracing(TracingOpts, Hook, Host, Args),
                        run2(Ls, Hook, Args, Host, TracingOpts)
                end
        end;
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
        case erlang:get(?TRACE_HOOK_KEY) of
            undefined ->
                run_fold1(Ls, Hook, Val, Args);
            TracingHooksOpts ->
                case do_get_tracing_options(Hook, Host, TracingHooksOpts) of
                    undefined ->
                        run_fold1(Ls, Hook, Val, Args);
                    TracingOpts ->
                        fold_start_hook_tracing(TracingOpts, Hook, Host, [Val | Args]),
                        run_fold2(Ls, Hook, Val, Args, Host, TracingOpts)
                end
        end;
	[] ->
	    Val
    catch _:badarg ->
	    Val
    end.

get_tracing_options(Hook, Host, Pid) when Pid == erlang:self() ->
    do_get_tracing_options(Hook, Host, erlang:get(?TRACE_HOOK_KEY));
get_tracing_options(Hook, Host, Pid) when erlang:is_pid(Pid) ->
    case erlang:process_info(Pid, dictionary) of
        {_, DictPropList} ->
            case lists:keyfind(?TRACE_HOOK_KEY, 1, DictPropList) of
                {_, TracingHooksOpts} ->
                    do_get_tracing_options(Hook, Host, TracingHooksOpts);
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

trace_on(Hook, Host, Pid, #{}=Opts, Timeout) when Pid == erlang:self() ->
    do_trace_on(Hook, Host, Opts, Timeout);
trace_on(Hook, Host, Proc, #{}=Opts, Timeout) ->
    try sys:replace_state(
        Proc,
        fun(State) ->
            do_trace_on(Hook, Host, Opts, Timeout),
            State
        end,
        15000
    ) of
        _ -> % process state
            ok
    catch
        _:Reason ->
            {error, Reason}
    end.

trace_off(Hook, Host, Pid) when Pid == erlang:self() ->
    do_trace_off(Hook, Host);
trace_off(Hook, Host, Proc) ->
    try sys:replace_state(
        Proc,
        fun(State) ->
            do_trace_off(Hook, Host),
            State
        end,
        15000
    ) of
        _ -> % process state
            ok
    catch
        _:Reason ->
            {error, Reason}
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

%%%----------------------------------------------------------------------
%%% Internal tracing functions
%%%----------------------------------------------------------------------

do_trace_on(Hook, Host, Opts, Timeout) when erlang:is_list(Host) ->
    do_trace_on(Hook, erlang:list_to_binary(Host), Opts, Timeout);
do_trace_on(Hook, Host, Opts, undefined) ->
    case erlang:get(?TRACE_HOOK_KEY) of
        _ when Hook == all andalso Host == <<"*">> ->
            % Trace everything:
            erlang:put(?TRACE_HOOK_KEY, #{all => #{<<"*">> => Opts}});
        #{all := #{<<"*">> := _}} -> % Already tracing everything
            % Update Opts:
            erlang:put(?TRACE_HOOK_KEY, #{all => #{<<"*">> => Opts}});
        #{all := HostOpts} when Hook == all -> % Already Tracing everything for some hosts
            % Add/Update Host and Opts:
            erlang:put(?TRACE_HOOK_KEY, #{all => HostOpts#{Host => Opts}});
        #{all := _} -> % Already tracing everything and Hook is not all
            ok;
        #{} when Hook == all ->
            % Remove other hooks by just adding all:
            erlang:put(?TRACE_HOOK_KEY, #{all => #{Host => Opts}});
        #{}=TraceHooksOpts when Host == <<"*">> -> % Want to trace a hook for all hosts
            erlang:put(?TRACE_HOOK_KEY, TraceHooksOpts#{Hook => #{Host => Opts}});
        #{}=TraceHooksOpts ->
            case maps:get(Hook, TraceHooksOpts, #{}) of
                #{<<"*">> := _} -> % Already tracing this hook for all hosts
                    ok;
                HostOpts ->
                    erlang:put(?TRACE_HOOK_KEY, TraceHooksOpts#{Hook => HostOpts#{Host => Opts}})
            end;
        undefined ->
            erlang:put(?TRACE_HOOK_KEY, #{Hook => #{Host => Opts}})
    end,
    ok;
do_trace_on(Hook, Host, Opts, TimeoutSeconds) -> % Trace myself `Timeout` time
    Timeout = timer:seconds(TimeoutSeconds),
    ParentPid = erlang:self(),
    try erlang:spawn(
        fun() ->
            MonitorRef = erlang:monitor(process, ParentPid),
            receive
                {_, MonitorRef, _, _, _} ->
                    ok
            after Timeout ->
                trace_off(Hook, Host, ParentPid)
            end,
            erlang:exit(normal)
        end
    ) of
        _ ->
            do_trace_on(Hook, Host, Opts, undefined) % ok
    catch
        _:Reason -> % system_limit
            {error, Reason}
    end.

do_trace_off(Hook, Host) when erlang:is_list(Host) ->
    do_trace_off(Hook, erlang:list_to_binary(Host));
do_trace_off(Hook, Host) ->
    case erlang:get(?TRACE_HOOK_KEY) of
        _ when Hook == all andalso Host == <<"*">> ->
            % Remove all tracing:
            erlang:erase(?TRACE_HOOK_KEY);
        #{all := HostOpts} when Hook == all -> % Already tracing all hooks
            % Remove Host:
            HostOpts2 = maps:remove(Host, HostOpts),
            if
                HostOpts2 == #{} ->
                    % Remove all tracing:
                    erlang:erase(?TRACE_HOOK_KEY);
                true ->
                    erlang:put(?TRACE_HOOK_KEY, #{all => HostOpts2})
            end;
        #{}=TraceHooksOpts when Host == <<"*">> ->
            % Remove tracing of this hook for all hosts:
            TraceHooksOpts2 = maps:remove(Hook, TraceHooksOpts),
            if
                TraceHooksOpts2 == #{} ->
                    % Remove all tracing:
                    erlang:erase(?TRACE_HOOK_KEY);
                true ->
                    erlang:put(?TRACE_HOOK_KEY, TraceHooksOpts2)
            end;
        #{}=TraceHooksOpts ->
            case maps:get(Hook, TraceHooksOpts, undefined) of
                #{}=HostOpts ->
                    NewHostOpts = maps:remove(Host, HostOpts),
                    if
                        NewHostOpts == #{} ->
                            % Remove hook:
                            erlang:put(?TRACE_HOOK_KEY, maps:remove(Hook, TraceHooksOpts));
                        true ->
                            erlang:put(?TRACE_HOOK_KEY, TraceHooksOpts#{Hook => NewHostOpts})
                    end;
                _ ->
                    ok
            end;
        undefined ->
            ok
    end,
    ok.

do_get_tracing_options(Hook, Host, MaybeMap) ->
    case MaybeMap of
        undefined ->
            undefined;
        #{all := #{<<"*">> := Opts}} -> % Tracing everything
            Opts;
        #{all := HostOpts} -> % Tracing all hooks for some hosts
            maps:get(Host, HostOpts, undefined);
        #{}=TraceHooksOpts ->
            HostOpts = maps:get(Hook, TraceHooksOpts, #{}),
            case maps:get(Host, HostOpts, undefined) of
                undefined ->
                    maps:get(<<"*">>, HostOpts, undefined);
                Opts ->
                    Opts
            end
    end.

run2([], Hook, Args, Host, Opts) ->
    foreach_stop_hook_tracing(Opts, Hook, Host, Args, undefined),
    ok;
run2([{Seq, Module, Function} | Ls], Hook, Args, Host, TracingOpts) ->
    foreach_start_callback_tracing(TracingOpts, Hook, Host, Module, Function, Args, Seq),
    Res = safe_apply(Hook, Module, Function, Args),
    foreach_stop_callback_tracing(TracingOpts, Hook, Host, Module, Function, Args, Seq, Res),
    case Res of
        'EXIT' ->
            run2(Ls, Hook, Args, Host, TracingOpts);
        stop ->
            foreach_stop_hook_tracing(TracingOpts, Hook, Host, Args, {Module, Function, Seq, Ls}),
            ok;
        _ ->
            run2(Ls, Hook, Args, Host, TracingOpts)
    end.

run_fold2([], Hook, Val, Args, Host, Opts) ->
    fold_stop_hook_tracing(Opts, Hook, Host, [Val | Args], undefined),
    Val;
run_fold2([{Seq, Module, Function} | Ls], Hook, Val, Args, Host, TracingOpts) ->
    fold_start_callback_tracing(TracingOpts, Hook, Host, Module, Function, [Val | Args], Seq),
    Res = safe_apply(Hook, Module, Function, [Val | Args]),
    fold_stop_callback_tracing(TracingOpts, Hook, Host, Module, Function, [Val | Args], Seq, Res),
    case Res of
        'EXIT' ->
            run_fold2(Ls, Hook, Val, Args, Host, TracingOpts);
        stop ->
            fold_stop_hook_tracing(TracingOpts, Hook, Host, [Val | Args], {Module, Function, Seq, {old, Val}, Ls}),
            Val;
        {stop, NewVal} ->
            fold_stop_hook_tracing(TracingOpts, Hook, Host, [Val | Args], {Module, Function, Seq, {new, NewVal}, Ls}),
            NewVal;
        NewVal ->
            run_fold2(Ls, Hook, NewVal, Args, Host, TracingOpts)
    end.

foreach_start_hook_tracing(TracingOpts, Hook, Host, Args) ->
    run_event_handlers(TracingOpts, Hook, Host, start_hook, [Args], foreach).

foreach_stop_hook_tracing(TracingOpts, Hook, Host, Args, BreakCallback) ->
    run_event_handlers(TracingOpts, Hook, Host, stop_hook, [Args, BreakCallback], foreach).

foreach_start_callback_tracing(TracingOpts, Hook, Host, Mod, Func, Args, Seq) ->
    run_event_handlers(TracingOpts, Hook, Host, start_callback, [Mod, Func, Args, Seq], foreach).

foreach_stop_callback_tracing(TracingOpts, Hook, Host, Mod, Func, Args, Seq, Res) ->
    run_event_handlers(TracingOpts, Hook, Host, stop_callback, [Mod, Func, Args, Seq, Res], foreach).

fold_start_hook_tracing(TracingOpts, Hook, Host, Args) ->
    run_event_handlers(TracingOpts, Hook, Host, start_hook, [Args], fold).

fold_stop_hook_tracing(TracingOpts, Hook, Host, Args, BreakCallback) ->
    run_event_handlers(TracingOpts, Hook, Host, stop_hook, [Args, BreakCallback], fold).

fold_start_callback_tracing(TracingOpts, Hook, Host, Mod, Func, Args, Seq) ->
    run_event_handlers(TracingOpts, Hook, Host, start_callback, [Mod, Func, Args, Seq], fold).

fold_stop_callback_tracing(TracingOpts, Hook, Host, Mod, Func, Args, Seq, Res) ->
    run_event_handlers(TracingOpts, Hook, Host, stop_callback, [Mod, Func, Args, Seq, Res], fold).

run_event_handlers(TracingOpts, Hook, Host, Event, EventArgs, RunType) ->
    EventHandlerList = maps:get(event_handler_list, TracingOpts, default_tracing_event_handler_list()),
    EventHandlerOpts = maps:get(event_handler_options, TracingOpts, #{}),
    if
        erlang:is_list(EventHandlerList) ->
            lists:foreach(
                fun(EventHandler) ->
                    try
                        if
                            erlang:is_function(EventHandler) ->
                                erlang:apply(
                                    EventHandler,
                                    [Event, EventArgs, RunType, Hook, Host, EventHandlerOpts, TracingOpts]
                                );
                            true ->
                                EventHandler:handle_hook_tracing_event(
                                    Event,
                                    EventArgs,
                                    RunType,
                                    Hook,
                                    Host,
                                    EventHandlerOpts,
                                    TracingOpts
                                )
                        end
                    of
                        _ ->
                            ok
                    catch
                        ?EX_RULE(E, R, St) ->
                            Stack = ?EX_STACK(St),
                            ?ERROR_MSG(
                                "(~0p|~ts|~0p) Tracing event '~0p' handler exception(~0p): ~0p: ~0p",
                                [Hook, Host, erlang:self(), EventHandler, E, R, Stack]
                            ),
                            ok
                    end
                end,
                EventHandlerList
            ); % ok
        true ->
            ?ERROR_MSG("(~0p|~ts|~0p) Bad event handler list: ~0p", [Hook, Host, erlang:self(), EventHandlerList]),
            ok
    end.

default_tracing_event_handler_list() ->
    [fun tracing_timing_event_handler/7].

tracing_timing_event_handler(start_hook, EventArgs, RunType, Hook, Host, _, TracingOpts) ->
    HookStart = erlang:system_time(nanosecond),
    % Generate new event:
    run_event_handlers(TracingOpts, Hook, Host, start_hook_timing, EventArgs ++ [HookStart], RunType);
tracing_timing_event_handler(stop_hook, EventArgs, RunType, Hook, Host, _, TracingOpts) ->
    HookStop = erlang:system_time(nanosecond),
    TimingMap = #{} = erlang:get(?TIMING_KEY),
    {HookStart, CallbackList} = maps:get({Hook, Host}, TimingMap),
    {CallbackListTiming, CallbackListTotal} = lists:foldl(
        fun({_, _, _, CallbackStart, CallbackStop}=CallbackTimingInfo, {CallbackListTimingX, Total}) ->
            {CallbackListTimingX ++ [CallbackTimingInfo], Total + (CallbackStop - CallbackStart)}
        end,
        {[], 0},
        CallbackList
    ),
    % Generate new event:
    run_event_handlers(
        TracingOpts,
        Hook,
        Host,
        stop_hook_timing,
        EventArgs ++ [HookStart, HookStop, CallbackListTiming, CallbackListTotal],
        RunType
    );
tracing_timing_event_handler(start_callback, EventArgs, RunType, Hook, Host, _, TracingOpts) ->
    CallbackStart = erlang:system_time(nanosecond),
    % Generate new event:
    run_event_handlers(TracingOpts, Hook, Host, start_callback_timing, EventArgs ++ [CallbackStart], RunType);
tracing_timing_event_handler(stop_callback, EventArgs, RunType, Hook, Host, _, TracingOpts) ->
    CallbackStop = erlang:system_time(nanosecond),
    TimingMap = #{} = erlang:get(?TIMING_KEY),
    {_, [{_, _, _, CallbackStart} | _]} = maps:get({Hook, Host}, TimingMap),
    run_event_handlers(
        TracingOpts,
        Hook,
        Host,
        stop_callback_timing,
        EventArgs ++ [CallbackStart, CallbackStop],
        RunType
    ),
    ok;
tracing_timing_event_handler(start_hook_timing, [_, HookStart], RunType, Hook, Host, EventHandlerOpts, _) ->
    tracing_output(EventHandlerOpts, "(~0p|~ts|~0p|~0p) Timing started\n", [Hook, Host, erlang:self(), RunType]),
    case erlang:get(?TIMING_KEY) of
        #{}=TimingMap ->
            erlang:put(?TIMING_KEY, TimingMap#{{Hook, Host} => {HookStart, []}});
        _ ->
            erlang:put(?TIMING_KEY, #{{Hook, Host} => {HookStart, []}})
    end,
    ok;
tracing_timing_event_handler(
    stop_hook_timing,
    [_, _, HookStart, HookStop, CallbackListTiming, CallbackListTotal],
    RunType,
    Hook,
    Host,
    EventHandlerOpts,
    _
) ->
    if
        erlang:length(CallbackListTiming) < 2 -> % We don't need sorted timing result
            ok;
        true ->
            CallbackListTimingText =
                lists:foldl(
                    fun({Mod, Func, Arity, Diff}, CallbackListTimingText) ->
                        CallbackListTimingText
                            ++ "\n\t"
                            ++ mfa_string({Mod, Func, Arity})
                            ++ " -> "
                            ++ human_readable_time_string(Diff)
                    end,
                    "",
                    lists:keysort(
                        4,
                        [
                            {Mod, Func, Arity, CallbackStop - CallbackStart} ||
                            {Mod, Func, Arity, CallbackStart, CallbackStop} <- CallbackListTiming
                        ]
                    )
                ),
            tracing_output(
                EventHandlerOpts,
                "(~0p|~ts|~0p|~0p) All callbacks took ~ts to run. Sorted running time:"
                    ++ CallbackListTimingText
                    ++ "\n",
                [Hook, Host, erlang:self(), RunType, human_readable_time_string(CallbackListTotal)]
            ),
            tracing_output(
                EventHandlerOpts,
                "(~0p|~ts|~0p|~0p) Time calculations for all callbacks took ~ts\n",
                [
                    Hook,
                    Host,
                    erlang:self(),
                    RunType,
                    human_readable_time_string((HookStop - HookStart) - CallbackListTotal)
                ]
            )
    end,
    tracing_output(EventHandlerOpts, "(~0p|~ts|~0p|~0p) Timing stopped\n", [Hook, Host, erlang:self(), RunType]),
    TimingMap = #{} = erlang:get(?TIMING_KEY),
    NewTimingMap = maps:remove({Hook, Host}, TimingMap),
    if
        NewTimingMap == #{} ->
            erlang:erase(?TIMING_KEY);
        true ->
            erlang:put(?TIMING_KEY, NewTimingMap)
    end,
    ok;
tracing_timing_event_handler(start_callback_timing, [Mod, Func, Args, _, CallbackStart], _, Hook, Host, _, _) ->
    TimingMap = #{} = erlang:get(?TIMING_KEY),
    {HookStart, Callbacks} = maps:get({Hook, Host}, TimingMap),
    erlang:put(
        ?TIMING_KEY,
        TimingMap#{
            {Hook, Host} => {HookStart, [{Mod, Func, erlang:length(Args), CallbackStart} | Callbacks]}
        }
    ),
    ok;
tracing_timing_event_handler(
    stop_callback_timing,
    [Mod, Func, _, _, _, CallbackStart, CallbackStop],
    RunType,
    Hook,
    Host,
    EventHandlerOpts,
    _
) ->
    TimingMap = #{} = erlang:get(?TIMING_KEY),
    {HookStart, [{Mod, Func, Arity, CallbackStart} | Callbacks]} = maps:get({Hook, Host}, TimingMap),
    maps:get(output_for_each_callback, maps:get(timing, EventHandlerOpts, #{}), false) andalso tracing_output(
        EventHandlerOpts,
        "(~0p|~ts|~0p|~0p) "
            ++ mfa_string({Mod, Func, Arity})
            ++ " took "
            ++ human_readable_time_string(CallbackStop - CallbackStart)
            ++ "\n",
        [Hook, Host, erlang:self(), RunType]
    ),
    erlang:put(
        ?TIMING_KEY,
        TimingMap#{
            {Hook, Host} => {HookStart, [{Mod, Func, Arity, CallbackStart, CallbackStop} | Callbacks]}
        }
    ),
    ok;
tracing_timing_event_handler(_, _, _, _, _, _, _) ->
    ok.

tracing_output(#{output_function := OutputF}, Text, Args) ->
    try
        OutputF(Text, Args)
    of
        _ ->
            ok
    catch
        ?EX_RULE(E, R, St) ->
            Stack = ?EX_STACK(St),
            ?ERROR_MSG("Tracing output function exception(~0p): ~0p: ~0p", [E, R, Stack]),
            ok
    end;
tracing_output(#{output_log_level := Output}, Text, Args) ->
    if
        Output == debug ->
            ?DEBUG(Text, Args);
        true -> % info
            ?INFO_MSG(Text, Args)
    end,
    ok;
tracing_output(Opts, Text, Args) ->
    tracing_output(Opts#{output_log_level => info}, Text, Args).

mfa_string({_, Fun, _}) when erlang:is_function(Fun) ->
    io_lib:format("~0p", [Fun]);
mfa_string({Mod, Func, Arity}) ->
    erlang:atom_to_list(Mod) ++ ":" ++ erlang:atom_to_list(Func) ++ "/" ++ erlang:integer_to_list(Arity).

human_readable_time_string(TimeNS) ->
    {Time, Unit, Decimals} =
        if
            TimeNS >= 1000000000 ->
                {TimeNS / 1000000000, "", 10};
            TimeNS >= 1000000  ->
                {TimeNS / 1000000, "m", 7};
            TimeNS >= 1000 ->
                {TimeNS / 1000, "μ", 4};
            true ->
                {TimeNS / 1, "n", 0}
    end,
    erlang:float_to_list(Time, [{decimals, Decimals}, compact]) ++ Unit ++ "s".
