%%%----------------------------------------------------------------------
%%% File    : ejabberd_websocket.erl
%%% Author  : Eric Cestari <ecestari@process-one.net>
%%% Purpose : JSON - XMPP Websocket module support
%%% Created : 09-10-2010 by Eric Cestari <ecestari@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
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

-module(ejabberd_http_wsjson).

-author('ecestari@process-one.net').

-behaviour(gen_fsm).

% External exports
-export([start/1, start_link/1, init/1, handle_event/3,
	 handle_sync_event/4, code_change/4, handle_info/3,
	 terminate/3, send_xml/2, setopts/2, sockname/1,
	 peername/1, controlling_process/2, become_controller/2,
	 close/1, socket_handoff/6]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("ejabberd_http.hrl").

-define(WEBSOCKET_TIMEOUT, 300).

-record(state,
	{socket                       :: ejabberd_http_ws:ws_socket(),
         timeout = ?WEBSOCKET_TIMEOUT :: pos_integer(),
         timer = make_ref()           :: reference(),
         input = []                   :: list(),
	 waiting_input = false        :: false | pid(),
         last_receiver                :: pid(),
         ws                           :: atom()}).

%-define(DBGFSM, true).

-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

-endif.

start(WS) ->
    supervisor:start_child(ejabberd_wsloop_sup, [WS]).

start_link(WS) ->
    gen_fsm:start_link(?MODULE, [WS], ?FSMOPTS).

send_xml({http_ws, FsmRef, _IP}, Packet) ->
    gen_fsm:sync_send_all_state_event(FsmRef,
				      {send, Packet}).

setopts({http_ws, FsmRef, _IP}, Opts) ->
    case lists:member({active, once}, Opts) of
      true ->
	  gen_fsm:send_all_state_event(FsmRef,
				       {activate, self()});
      _ -> ok
    end.

sockname(_Socket) -> {ok, {{0, 0, 0, 0}, 0}}.

peername({http_ws, _FsmRef, IP}) -> {ok, IP}.

controlling_process(_Socket, _Pid) -> ok.

become_controller(FsmRef, C2SPid) ->
    gen_fsm:send_all_state_event(FsmRef,
				 {become_controller, C2SPid}).

close({http_ws, FsmRef, _IP}) ->
    catch gen_fsm:sync_send_all_state_event(FsmRef, close).

socket_handoff(LocalPath, Request, Socket, SockMod, Buf, Opts) ->
    ejabberd_websocket:socket_handoff(LocalPath, Request, Socket, SockMod,
                                      Buf, Opts, ?MODULE).

%%% Internal

init([WS]) ->
    Opts = [{xml_socket, true}
	    | ejabberd_c2s_config:get_c2s_limits()],
    WSTimeout = ejabberd_config:get_local_option(
                  {websocket_timeout, ?MYNAME},
                  fun(I) when is_integer(I), I>0 -> I end,
                  ?WEBSOCKET_TIMEOUT) * 1000,
    Socket = {http_ws, self(), ejabberd_ws:get(WS, ip)},
    ?DEBUG("Client connected through websocket ~p",
	   [Socket]),
    ejabberd_socket:start(ejabberd_c2s, ?MODULE, Socket,
			  Opts),
    Timer = erlang:start_timer(WSTimeout, self(), []),
    {ok, loop,
     #state{socket = Socket, timeout = WSTimeout,
	    timer = Timer, ws = WS}}.

handle_event({activate, From}, StateName, StateData) ->
    case StateData#state.input of
      [] ->
	  {next_state, StateName,
	   StateData#state{waiting_input = From}};
      Input ->
	  Receiver = From,
	  lists:reverse(lists:map(fun (Packet) ->
					  Receiver !
					    {tcp, StateData#state.socket,
					     [Packet]}
				  end,
				  Input)),
	  {next_state, StateName,
	   StateData#state{input = [], waiting_input = false,
			   last_receiver = Receiver}}
    end.

handle_sync_event({send, Packet}, _From, StateName,
		  #state{ws = WS} = StateData) ->
    EJson = xmpp_json:to_json(Packet),
    Json = jiffy:encode(EJson),
    ejabberd_ws:send(WS, Json),
    {reply, ok, StateName, StateData};
handle_sync_event(close, _From, _StateName,
		  StateData) ->
    Reply = ok, {stop, normal, Reply, StateData}.

handle_info({browser, <<"\n">>}, StateName,
	    StateData) ->
    NewState = case StateData#state.waiting_input of
		 false -> ok;
		 Receiver ->
		     Receiver ! {tcp, StateData#state.socket, <<"\n">>},
		     cancel_timer(StateData#state.timer),
		     Timer = erlang:start_timer(StateData#state.timeout,
						self(), []),
		     StateData#state{waiting_input = false,
				     last_receiver = Receiver, timer = Timer}
	       end,
    {next_state, StateName, NewState};
handle_info({browser, JsonPacket}, StateName,
	    StateData) ->
    NewState = case StateData#state.waiting_input of
		 false ->
		     EJson = jiffy:decode(JsonPacket),
		     Packet = xmpp_json:from_json(EJson),
		     Input = [Packet | StateData#state.input],
		     StateData#state{input = Input};
		 Receiver ->
		     EJson = jiffy:decode(JsonPacket),
		     Packet = xmpp_json:from_json(EJson),
		     Receiver ! {tcp, StateData#state.socket, [Packet]},
		     cancel_timer(StateData#state.timer),
		     Timer = erlang:start_timer(StateData#state.timeout,
						self(), []),
		     StateData#state{waiting_input = false,
				     last_receiver = Receiver, timer = Timer}
	       end,
    {next_state, StateName, NewState};
handle_info({timeout, Timer, _}, _StateName,
	    #state{timer = Timer} = StateData) ->
    {stop, normal, StateData};
handle_info(_, StateName, StateData) ->
    {next_state, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) -> ok.

cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    receive {timeout, Timer, _} -> ok after 0 -> ok end.
