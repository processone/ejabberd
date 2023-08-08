%%%----------------------------------------------------------------------
%%% File    : ejabberd_systemd.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Integrate with systemd
%%% Created :  5 Jan 2021 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2021   ProcessOne
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

-module(ejabberd_systemd).
-author('holger@zedat.fu-berlin.de').
-behaviour(gen_server).

-export([start_link/0,
	 ready/0,
	 reloading/0,
	 stopping/0]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include("logger.hrl").

-record(state,
	{socket :: gen_udp:socket() | undefined,
	 destination :: inet:local_address() | undefined,
	 interval :: pos_integer() | undefined,
	 timer :: reference() | undefined}).

-type state() :: #state{}.

%%--------------------------------------------------------------------
%% API.
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec ready() -> ok.
ready() ->
    cast_notification(<<"READY=1">>).

-spec reloading() -> ok.
reloading() ->
    cast_notification(<<"RELOADING=1">>).

-spec stopping() -> ok.
stopping() ->
    cast_notification(<<"STOPPING=1">>).

%%--------------------------------------------------------------------
%% gen_server callbacks.
%%--------------------------------------------------------------------
-spec init(any()) -> {ok, state()} | {stop, term()}.
init(_Opts) ->
    process_flag(trap_exit, true),
    case os:getenv("NOTIFY_SOCKET") of
	[$@ | _Abstract] ->
	    ?CRITICAL_MSG("Abstract NOTIFY_SOCKET not supported", []),
	    {stop, esocktnosupport};
	Path when is_list(Path), length(Path) > 0 ->
	    ?DEBUG("Got NOTIFY_SOCKET: ~s", [Path]),
	    Destination = {local, Path},
	    case gen_udp:open(0, [local]) of
		{ok, Socket} ->
		    State = #state{socket = Socket,
				   destination = Destination,
				   interval = get_watchdog_interval()},
		    {ok, maybe_start_timer(State)};
		{error, Reason} ->
		    ?CRITICAL_MSG("Cannot open IPC socket: ~p", [Reason]),
		    {stop, Reason}
	    end;
	_ ->
	    ?INFO_MSG("Got no NOTIFY_SOCKET, notifications disabled", []),
	    {ok, #state{}}
    end.

-spec handle_call(term(), {pid(), term()}, state())
      -> {reply, {error, badarg}, state()}.
handle_call(Request, From, State) ->
    ?ERROR_MSG("Got unexpected request from ~p: ~p", [From, Request]),
    {reply, {error, badarg}, State}.

-spec handle_cast({notify, binary()} | term(), state()) -> {noreply, state()}.
handle_cast({notify, Notification},
	    #state{destination = undefined} = State) ->
    ?DEBUG("No NOTIFY_SOCKET, dropping ~s notification", [Notification]),
    {noreply, State};
handle_cast({notify, Notification}, State) ->
    try notify(State, Notification)
    catch _:Err ->
	    ?ERROR_MSG("Cannot send ~s notification: ~p", [Notification, Err])
    end,
    {noreply, State};
handle_cast(Msg, State) ->
    ?ERROR_MSG("Got unexpected message: ~p", [Msg]),
    {noreply, State}.

-spec handle_info(ping_watchdog | term(), state()) -> {noreply, state()}.
handle_info(ping_watchdog, #state{interval = Interval} = State)
  when is_integer(Interval), Interval > 0 ->
    try notify(State, <<"WATCHDOG=1">>)
    catch _:Err ->
	    ?ERROR_MSG("Cannot ping watchdog: ~p", [Err])
    end,
    {noreply, start_timer(State)};
handle_info(Info, State) ->
    ?ERROR_MSG("Got unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> ok.
terminate(Reason, #state{socket = Socket} = State) ->
    ?DEBUG("Terminating ~s (~p)", [?MODULE, Reason]),
    cancel_timer(State),
    case Socket of
	undefined ->
	    ok;
	_Socket ->
	    gen_udp:close(Socket)
    end.

-spec code_change({down, term()} | term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    ?INFO_MSG("Got code change request", []),
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec get_watchdog_interval() -> integer() | undefined.
get_watchdog_interval() ->
    case os:getenv("WATCHDOG_USEC") of
	WatchdogUSec when is_list(WatchdogUSec), length(WatchdogUSec) > 0 ->
	    Interval = round(0.5 * list_to_integer(WatchdogUSec)),
	    ?DEBUG("Watchdog interval: ~B microseconds", [Interval]),
	    erlang:convert_time_unit(Interval, microsecond, millisecond);
	_ ->
	    undefined
    end.

-spec maybe_start_timer(state()) -> state().
maybe_start_timer(#state{interval = Interval} = State)
  when is_integer(Interval), Interval > 0 ->
    ?INFO_MSG("Watchdog notifications enabled", []),
    start_timer(State);
maybe_start_timer(State) ->
    ?INFO_MSG("Watchdog notifications disabled", []),
    State.

-spec start_timer(state()) -> state().
start_timer(#state{interval = Interval} = State) ->
    ?DEBUG("Pinging watchdog in ~B milliseconds", [Interval]),
    State#state{timer = erlang:send_after(Interval, self(), ping_watchdog)}.

-spec cancel_timer(state()) -> ok.
cancel_timer(#state{timer = Timer}) ->
    ?DEBUG("Cancelling watchdog timer", []),
    misc:cancel_timer(Timer).

-spec notify(state(), binary()) -> ok.
notify(#state{socket = Socket, destination = Destination},
       Notification) ->
    ?DEBUG("Notifying systemd: ~s", [Notification]),
    ok = gen_udp:send(Socket, Destination, 0, Notification).

-spec cast_notification(binary()) -> ok.
cast_notification(Notification) ->
    ?DEBUG("Closing NOTIFY_SOCKET", []),
    gen_server:cast(?MODULE, {notify, Notification}).
