%%%-------------------------------------------------------------------
%%% File    : ejabberd_system_monitor.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Description : ejabberd watchdog
%%% Created : 21 Mar 2007 by Alexey Shchepin <alexey@process-one.net>
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
%%%-------------------------------------------------------------------

-module(ejabberd_system_monitor).
-behaviour(gen_event).

-author('alexey@process-one.net').
-author('ekhramtsov@process-one.net').

%% API
-export([start/0, config_reloaded/0, stop/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
	 handle_info/2, terminate/2, code_change/3]).

-include("logger.hrl").

-define(CHECK_INTERVAL, timer:seconds(30)).

-record(state, {tref :: undefined | reference(),
		mref :: undefined | reference()}).
-record(proc_stat, {qlen :: non_neg_integer(),
		    memory :: non_neg_integer(),
		    initial_call :: mfa(),
		    current_function :: mfa(),
		    ancestors :: [pid() | atom()],
		    application :: pid() | atom(),
		    name :: pid() | atom()}).
-type state() :: #state{}.
-type proc_stat() :: #proc_stat{}.
-type app_pids() :: #{pid() => atom()}.

%%%===================================================================
%%% API
%%%===================================================================
-spec start() -> ok.
start() ->
    gen_event:add_handler(alarm_handler, ?MODULE, []),
    gen_event:swap_handler(alarm_handler, {alarm_handler, swap}, {?MODULE, []}),
    application:load(os_mon),
    application:set_env(os_mon, start_cpu_sup, false),
    application:set_env(os_mon, start_os_sup, false),
    application:set_env(os_mon, start_memsup, true),
    application:set_env(os_mon, start_disksup, false),
    ejabberd:start_app(os_mon),
    set_oom_watermark().

-spec stop() -> term().
stop() ->
    gen_event:delete_handler(alarm_handler, ?MODULE, []).

excluded_apps() ->
    [os_mon, mnesia, sasl, stdlib, kernel].

-spec config_reloaded() -> ok.
config_reloaded() ->
    set_oom_watermark().

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================
init({[], _}) -> % Called by gen_event:swap_handler
    {ok, #state{}};
init([]) -> % Called by gen_event:add_handler
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 50),
    {ok, #state{}}.

handle_event({set_alarm, {system_memory_high_watermark, _}}, State) ->
    handle_overload(State),
    {ok, restart_timer(State)};
handle_event({clear_alarm, system_memory_high_watermark}, State) ->
    misc:cancel_timer(State#state.tref),
    {ok, State#state{tref = undefined}};
handle_event({set_alarm, {process_memory_high_watermark, Pid}}, State) ->
    case proc_stat(Pid, get_app_pids()) of
	#proc_stat{name = Name} = ProcStat ->
	    ?WARNING_MSG(
	      "Process ~p consumes more than 5% of OS memory (~ts)~n",
	      [Name, format_proc(ProcStat)]),
	    handle_overload(State),
	    {ok, State};
	_ ->
	    {ok, State}
    end;
handle_event({clear_alarm, process_memory_high_watermark}, State) ->
    {ok, State};
handle_event(Event, State) ->
    ?WARNING_MSG("unexpected event: ~p~n", [Event]),
    {ok, State}.

handle_call(_Request, State) ->
    {ok, {error, badarg}, State}.

handle_info({timeout, _TRef, handle_overload}, State) ->
    handle_overload(State),
    {ok, restart_timer(State)};
handle_info(Info, State) ->
    ?WARNING_MSG("unexpected info: ~p~n", [Info]),
    {ok, State}.

terminate(_Reason, State) ->
    misc:cancel_timer(State#state.tref),
    ejabberd_hooks:delete(config_reloaded, ?MODULE, config_reloaded, 50).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec handle_overload(state()) -> ok.
handle_overload(State) ->
    handle_overload(State, processes()).

-spec handle_overload(state(), [pid()]) -> ok.
handle_overload(_State, Procs) ->
    AppPids = get_app_pids(),
    {TotalMsgs, ProcsNum, Apps, Stats} = overloaded_procs(AppPids, Procs),
    MaxMsgs = ejabberd_option:oom_queue(),
    if TotalMsgs >= MaxMsgs ->
	    SortedStats = lists:reverse(lists:keysort(#proc_stat.qlen, Stats)),
	    ?WARNING_MSG(
	      "The system is overloaded with ~b messages "
	      "queued by ~b process(es) (~b%) "
	      "from the following applications: ~ts; "
	      "the top processes are:~n~ts~n",
	      [TotalMsgs, ProcsNum,
	       round(ProcsNum*100/length(Procs)),
	       format_apps(Apps),
	       format_top_procs(SortedStats)]),
	    kill(SortedStats, round(TotalMsgs/ProcsNum));
       true ->
	    ok
    end,
    lists:foreach(fun erlang:garbage_collect/1, Procs).

-spec get_app_pids() -> app_pids().
get_app_pids() ->
    try application:info() of
	Info ->
	    case lists:keyfind(running, 1, Info) of
		{_, Apps} ->
		    lists:foldl(
		      fun({Name, Pid}, M) when is_pid(Pid) ->
			      maps:put(Pid, Name, M);
			 (_, M) ->
			      M
		      end, #{}, Apps);
		false ->
		    #{}
	    end
    catch _:_ ->
	    #{}
    end.

-spec overloaded_procs(app_pids(), [pid()])
      -> {non_neg_integer(), non_neg_integer(), dict:dict(), [proc_stat()]}.
overloaded_procs(AppPids, AllProcs) ->
    lists:foldl(
      fun(Pid, {TotalMsgs, ProcsNum, Apps, Stats}) ->
	      case proc_stat(Pid, AppPids) of
		  #proc_stat{qlen = QLen, application = App} = Stat
		    when QLen > 0 ->
		      {TotalMsgs + QLen, ProcsNum + 1,
		       dict:update_counter(App, QLen, Apps),
		       [Stat|Stats]};
		  _ ->
		      {TotalMsgs, ProcsNum, Apps, Stats}
	      end
      end, {0, 0, dict:new(), []}, AllProcs).

-spec proc_stat(pid(), app_pids()) -> proc_stat() | undefined.
proc_stat(Pid, AppPids) ->
    case process_info(Pid, [message_queue_len,
			    memory,
			    initial_call,
			    current_function,
			    dictionary,
			    group_leader,
			    registered_name]) of
	[{_, MsgLen}, {_, Mem}, {_, InitCall},
	 {_, CurrFun}, {_, Dict}, {_, GL}, {_, Name}] ->
	    IntLen = proplists:get_value('$internal_queue_len', Dict, 0),
	    TrueInitCall = proplists:get_value('$initial_call', Dict, InitCall),
	    Ancestors = proplists:get_value('$ancestors', Dict, []),
	    Len = IntLen + MsgLen,
	    App = maps:get(GL, AppPids, kernel),
	    RegName = case Name of
			  [] -> Pid;
			  _ -> Name
		      end,
	    #proc_stat{qlen = Len,
		       memory = Mem,
		       initial_call = TrueInitCall,
		       current_function = CurrFun,
		       ancestors = Ancestors,
		       application = App,
		       name = RegName};
	_ ->
	    undefined
    end.

-spec restart_timer(#state{}) -> #state{}.
restart_timer(State) ->
    misc:cancel_timer(State#state.tref),
    TRef = erlang:start_timer(?CHECK_INTERVAL, self(), handle_overload),
    State#state{tref = TRef}.

-spec format_apps(dict:dict()) -> iodata().
format_apps(Apps) ->
    AppList = lists:reverse(lists:keysort(2, dict:to_list(Apps))),
    string:join(
      [io_lib:format("~p (~b msgs)", [App, Msgs]) || {App, Msgs} <- AppList],
      ", ").

-spec format_top_procs([proc_stat()]) -> iodata().
format_top_procs(Stats) ->
    Stats1 = lists:sublist(Stats, 5),
    string:join(
      lists:map(
	fun(#proc_stat{name = Name} = Stat) ->
		[io_lib:format("** ~w: ", [Name]), format_proc(Stat)]
	end,Stats1),
      io_lib:nl()).

-spec format_proc(proc_stat()) -> iodata().
format_proc(#proc_stat{qlen = Len, memory = Mem, initial_call = InitCall,
		       current_function = CurrFun, ancestors = Ancs,
		       application = App}) ->
    io_lib:format(
      "msgs = ~b, memory = ~b, initial_call = ~ts, "
      "current_function = ~ts, ancestors = ~w, application = ~w",
      [Len, Mem, format_mfa(InitCall), format_mfa(CurrFun), Ancs, App]).

-spec format_mfa(mfa()) -> iodata().
format_mfa({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
    io_lib:format("~ts:~ts/~b", [M, F, A]);
format_mfa(WTF) ->
    io_lib:format("~w", [WTF]).

-spec kill([proc_stat()], non_neg_integer()) -> ok.
kill(Stats, Threshold) ->
    case ejabberd_option:oom_killer() of
	true ->
	    do_kill(Stats, Threshold);
	false ->
	    ok
    end.

-spec do_kill([proc_stat()], non_neg_integer()) -> ok.
do_kill(Stats, Threshold) ->
    Killed = lists:filtermap(
	       fun(#proc_stat{qlen = Len, name = Name, application = App})
		     when Len >= Threshold ->
		       case lists:member(App, excluded_apps()) of
			   true ->
			       ?WARNING_MSG(
				 "Unable to kill process ~p from whitelisted "
				 "application ~p~n", [Name, App]),
			       false;
			   false ->
			       case kill_proc(Name) of
				   false ->
				       false;
				   Pid ->
				       {true, Pid}
			       end
		       end;
		  (_) ->
		       false
	       end, Stats),
    TotalKilled = length(Killed),
    if TotalKilled > 0 ->
	    ?ERROR_MSG(
	      "Killed ~b process(es) consuming more than ~b message(s) each~n",
	      [TotalKilled, Threshold]);
       true ->
	    ok
    end.

-spec kill_proc(pid() | atom()) -> false | pid().
kill_proc(undefined) ->
    false;
kill_proc(Name) when is_atom(Name) ->
    kill_proc(whereis(Name));
kill_proc(Pid) ->
    exit(Pid, kill),
    Pid.

-spec set_oom_watermark() -> ok.
set_oom_watermark() ->
    WaterMark = ejabberd_option:oom_watermark(),
    memsup:set_sysmem_high_watermark(WaterMark/100).
