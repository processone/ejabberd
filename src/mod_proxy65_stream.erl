%%%----------------------------------------------------------------------
%%% File    : mod_proxy65_stream.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : Bytestream process.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

-module(mod_proxy65_stream).

-author('xram@jabber.ru').

-behaviour(p1_fsm).
-behaviour(ejabberd_listener).

%% gen_fsm callbacks.
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         code_change/4,
         handle_info/3,
         terminate/3]).

%% gen_fsm states.
-export([accepting/2,
         wait_for_init/2,
         wait_for_auth/2,
         wait_for_request/2,
         wait_for_activation/2,
         stream_established/2]).

-export([start/3,
         stop/1,
         start_link/3,
         activate/2,
         relay/3,
         accept/1,
         listen_options/0]).

-include("mod_proxy65.hrl").

-include("logger.hrl").

-define(WAIT_TIMEOUT, 60000).

-record(state, {
          socket :: inet:socket(),
          timer = make_ref() :: reference(),
          sha1 = <<"">> :: binary(),
          host = <<"">> :: binary(),
          auth_type = anonymous :: plain | anonymous,
          shaper = none :: ejabberd_shaper:shaper()
         }).


%% Unused callbacks
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%%-------------------------------


start(gen_tcp, Socket, Opts) ->
    Host = proplists:get_value(server_host, Opts),
    p1_fsm:start(?MODULE, [Socket, Host], []).


start_link(gen_tcp, Socket, Opts) ->
    Host = proplists:get_value(server_host, Opts),
    p1_fsm:start_link(?MODULE, [Socket, Host], []).


init([Socket, Host]) ->
    process_flag(trap_exit, true),
    AuthType = mod_proxy65_opt:auth_type(Host),
    Shaper = mod_proxy65_opt:shaper(Host),
    RecvBuf = mod_proxy65_opt:recbuf(Host),
    SendBuf = mod_proxy65_opt:sndbuf(Host),
    TRef = erlang:send_after(?WAIT_TIMEOUT, self(), stop),
    inet:setopts(Socket, [{recbuf, RecvBuf}, {sndbuf, SendBuf}]),
    {ok, accepting,
         #state{
           host = Host,
           auth_type = AuthType,
           socket = Socket,
           shaper = Shaper,
           timer = TRef
          }}.


terminate(_Reason, StateName, #state{sha1 = SHA1}) ->
    Mod = gen_mod:ram_db_mod(global, mod_proxy65),
    Mod:unregister_stream(SHA1),
    if
        StateName == stream_established ->
            ?INFO_MSG("(~w) Bytestream terminated", [self()]);
        true -> ok
    end.


%%%------------------------------
%%% API.
%%%------------------------------
accept(StreamPid) ->
    p1_fsm:send_event(StreamPid, accept).


stop(StreamPid) -> StreamPid ! stop.


activate({P1, J1}, {P2, J2}) ->
    case catch {p1_fsm:sync_send_all_state_event(P1,
                                                 get_socket),
                p1_fsm:sync_send_all_state_event(P2, get_socket)} of
        {S1, S2} when is_port(S1), is_port(S2) ->
            P1 ! {activate, P2, S2, J1, J2},
            P2 ! {activate, P1, S1, J1, J2},
            JID1 = jid:encode(J1),
            JID2 = jid:encode(J2),
            ?INFO_MSG("(~w:~w) Activated bytestream for ~ts "
                      "-> ~ts",
                      [P1, P2, JID1, JID2]),
            ok;
        _ -> error
    end.


%%%-----------------------
%%% States
%%%-----------------------
accepting(accept, State) ->
    inet:setopts(State#state.socket, [{active, true}]),
    {next_state, wait_for_init, State}.


wait_for_init(Packet,
              #state{socket = Socket, auth_type = AuthType} =
                  StateData) ->
    case mod_proxy65_lib:unpack_init_message(Packet) of
        {ok, AuthMethods} ->
            Method = select_auth_method(AuthType, AuthMethods),
            gen_tcp:send(Socket,
                         mod_proxy65_lib:make_init_reply(Method)),
            case Method of
                ?AUTH_ANONYMOUS ->
                    {next_state, wait_for_request, StateData};
                ?AUTH_PLAIN -> {next_state, wait_for_auth, StateData};
                ?AUTH_NO_METHODS -> {stop, normal, StateData}
            end;
        error -> {stop, normal, StateData}
    end.


wait_for_auth(Packet,
              #state{socket = Socket, host = Host} = StateData) ->
    case mod_proxy65_lib:unpack_auth_request(Packet) of
        {User, Pass} ->
            Result = ejabberd_auth:check_password(User, <<"">>, Host, Pass),
            gen_tcp:send(Socket,
                         mod_proxy65_lib:make_auth_reply(Result)),
            case Result of
                true -> {next_state, wait_for_request, StateData};
                false -> {stop, normal, StateData}
            end;
        _ -> {stop, normal, StateData}
    end.


wait_for_request(Packet,
                 #state{socket = Socket} = StateData) ->
    Request = mod_proxy65_lib:unpack_request(Packet),
    case Request of
        #s5_request{sha1 = SHA1, cmd = connect} ->
            Mod = gen_mod:ram_db_mod(global, mod_proxy65),
            case Mod:register_stream(SHA1, self()) of
                ok ->
                    inet:setopts(Socket, [{active, false}]),
                    gen_tcp:send(Socket,
                                 mod_proxy65_lib:make_reply(Request)),
                    {next_state, wait_for_activation,
                                 StateData#state{sha1 = SHA1}};
                _ ->
                    Err = mod_proxy65_lib:make_error_reply(Request),
                    gen_tcp:send(Socket, Err),
                    {stop, normal, StateData}
            end;
        #s5_request{cmd = udp} ->
            Err = mod_proxy65_lib:make_error_reply(Request,
                                                   ?ERR_COMMAND_NOT_SUPPORTED),
            gen_tcp:send(Socket, Err),
            {stop, normal, StateData};
        _ -> {stop, normal, StateData}
    end.


wait_for_activation(_Data, StateData) ->
    {next_state, wait_for_activation, StateData}.


stream_established(_Data, StateData) ->
    {next_state, stream_established, StateData}.


%%%-----------------------
%%% Callbacks processing
%%%-----------------------


%% SOCKS5 packets.
handle_info({tcp, _S, Data}, StateName, StateData)
  when StateName /= wait_for_activation ->
    misc:cancel_timer(StateData#state.timer),
    TRef = erlang:send_after(?WAIT_TIMEOUT, self(), stop),
    p1_fsm:send_event(self(), Data),
    {next_state, StateName, StateData#state{timer = TRef}};
%% Activation message.
handle_info({activate, PeerPid, PeerSocket, IJid, TJid},
            wait_for_activation,
            StateData) ->
    erlang:monitor(process, PeerPid),
    misc:cancel_timer(StateData#state.timer),
    MySocket = StateData#state.socket,
    Shaper = StateData#state.shaper,
    Host = StateData#state.host,
    MaxRate = find_maxrate(Shaper, IJid, TJid, Host),
    spawn_link(?MODULE,
               relay,
               [MySocket, PeerSocket, MaxRate]),
    {next_state, stream_established, StateData};
%% Socket closed
handle_info({tcp_closed, _Socket},
            _StateName,
            StateData) ->
    {stop, normal, StateData};
handle_info({tcp_error, _Socket, _Reason},
            _StateName,
            StateData) ->
    {stop, normal, StateData};
%% Got stop message.
handle_info(stop, _StateName, StateData) ->
    {stop, normal, StateData};
%% Either linked process or peer process died.
handle_info({'EXIT', _, _}, _StateName, StateData) ->
    {stop, normal, StateData};
handle_info({'DOWN', _, _, _, _},
            _StateName,
            StateData) ->
    {stop, normal, StateData};
%% Packets of no interest
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.


%% Socket request.
handle_sync_event(get_socket,
                  _From,
                  wait_for_activation,
                  StateData) ->
    Socket = StateData#state.socket,
    {reply, Socket, wait_for_activation, StateData};
handle_sync_event(_Event,
                  _From,
                  StateName,
                  StateData) ->
    {reply, error, StateName, StateData}.


%%%-------------------------------------------------
%%% Relay Process.
%%%-------------------------------------------------
relay(MySocket, PeerSocket, Shaper) ->
    case gen_tcp:recv(MySocket, 0) of
        {ok, Data} ->
            case gen_tcp:send(PeerSocket, Data) of
                ok ->
                    {NewShaper, Pause} = ejabberd_shaper:update(Shaper, byte_size(Data)),
                    if
                        Pause > 0 -> timer:sleep(Pause);
                        true -> pass
                    end,
                    relay(MySocket, PeerSocket, NewShaper);
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.


%%%------------------------
%%% Auxiliary functions
%%%------------------------
select_auth_method(plain, AuthMethods) ->
    case lists:member(?AUTH_PLAIN, AuthMethods) of
        true -> ?AUTH_PLAIN;
        false -> ?AUTH_NO_METHODS
    end;
select_auth_method(anonymous, AuthMethods) ->
    case lists:member(?AUTH_ANONYMOUS, AuthMethods) of
        true -> ?AUTH_ANONYMOUS;
        false -> ?AUTH_NO_METHODS
    end.


%% Obviously, we must use shaper with maximum rate.
find_maxrate(Shaper, JID1, JID2, Host) ->
    R1 = ejabberd_shaper:match(Host, Shaper, JID1),
    R2 = ejabberd_shaper:match(Host, Shaper, JID2),
    R = case ejabberd_shaper:get_max_rate(R1) >= ejabberd_shaper:get_max_rate(R2) of
            true -> R1;
            false -> R2
        end,
    ejabberd_shaper:new(R).


listen_options() ->
    [].
