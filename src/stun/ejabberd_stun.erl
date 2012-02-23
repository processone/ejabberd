%%%-------------------------------------------------------------------
%%% File    : ejabberd_stun.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : RFC5389 implementation.
%%%               Currently only Binding usage is supported.
%%%
%%% Created :  8 Aug 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%%-------------------------------------------------------------------
-module(ejabberd_stun).

-behaviour(gen_fsm).

%% API
-export([start_link/2,
	 start/2,
	 socket_type/0,
	 udp_recv/5]).

%% gen_fsm callbacks
-export([init/1,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 code_change/4]).

%% gen_fsm states
-export([wait_for_tls/2,
	 session_established/2]).

-include("ejabberd.hrl").
-include("stun.hrl").

-define(MAX_BUF_SIZE, 64*1024). %% 64kb
-define(TIMEOUT, 10000). %% 10 sec

-record(state, {sock,
		sock_mod = gen_tcp,
		certfile,
		peer,
		tref,
		buf = <<>>}).

%%====================================================================
%% API
%%====================================================================
start({gen_tcp, Sock}, Opts) ->
    supervisor:start_child(ejabberd_stun_sup, [Sock, Opts]).

start_link(Sock, Opts) ->
    gen_fsm:start_link(?MODULE, [Sock, Opts], []).

socket_type() ->
    raw.

udp_recv(Sock, Addr, Port, Data, _Opts) ->
    case stun_codec:decode(Data) of
	{ok, Msg, <<>>} ->
	    ?DEBUG("got:~n~s", [stun_codec:pp(Msg)]),
	    case process(Addr, Port, Msg) of
		RespMsg when is_record(RespMsg, stun) ->
		    ?DEBUG("sent:~n~s", [stun_codec:pp(RespMsg)]),
		    Data1 = stun_codec:encode(RespMsg),
		    gen_udp:send(Sock, Addr, Port, Data1);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end.

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([Sock, Opts]) ->
    case inet:peername(Sock) of
	{ok, Addr} ->
	    inet:setopts(Sock, [{active, once}]),
	    TRef = erlang:start_timer(?TIMEOUT, self(), stop),
	    State = #state{sock = Sock, peer = Addr, tref = TRef},
	    case proplists:get_value(certfile, Opts) of
		undefined ->
		    {ok, session_established, State};
		CertFile ->
		    {ok, wait_for_tls, State#state{certfile = CertFile}}
	    end;
	Err ->
	    Err
    end.

wait_for_tls(Event, State) ->
    ?INFO_MSG("unexpected event in wait_for_tls: ~p", [Event]),
    {next_state, wait_for_tls, State}.

session_established(Msg, State) when is_record(Msg, stun) ->
    ?DEBUG("got:~n~s", [stun_codec:pp(Msg)]),
    {Addr, Port} = State#state.peer,
    case process(Addr, Port, Msg) of
	Resp when is_record(Resp, stun) ->
	    ?DEBUG("sent:~n~s", [stun_codec:pp(Resp)]),
	    Data = stun_codec:encode(Resp),
	    (State#state.sock_mod):send(State#state.sock, Data);
	_ ->
	    ok
    end,
    {next_state, session_established, State};
session_established(Event, State) ->
    ?INFO_MSG("unexpected event in session_established: ~p", [Event]),
    {next_state, session_established, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, badarg}, StateName, State}.

handle_info({tcp, Sock, TLSData}, wait_for_tls, State) ->
    Buf = <<(State#state.buf)/binary, TLSData/binary>>,
    %% Check if the initial message is a TLS handshake
    case Buf of
	_ when size(Buf) < 3 ->
	    {next_state, wait_for_tls,
	     update_state(State#state{buf = Buf})};
	<<_:16, 1, _/binary>> ->
	    TLSOpts = [{certfile, State#state.certfile}],
	    {ok, TLSSock} = tls:tcp_to_tls(Sock, TLSOpts),
	    NewState = State#state{sock = TLSSock,
				   buf = <<>>,
				   sock_mod = tls},
	    case tls:recv_data(TLSSock, Buf) of
		{ok, Data} ->
		    process_data(session_established, NewState, Data);
		_Err ->
		    {stop, normal, NewState}
	    end;
	_ ->
	    process_data(session_established, State, TLSData)
    end;
handle_info({tcp, _Sock, TLSData}, StateName,
	    #state{sock_mod = tls} = State) ->
    case tls:recv_data(State#state.sock, TLSData) of
	{ok, Data} ->
	    process_data(StateName, State, Data);
	_Err ->
	    {stop, normal, State}
    end;
handle_info({tcp, _Sock, Data}, StateName, State) ->
    process_data(StateName, State, Data);
handle_info({tcp_closed, _Sock}, _StateName, State) ->
    ?DEBUG("connection reset by peer", []),
    {stop, normal, State};
handle_info({tcp_error, _Sock, Reason}, _StateName, State) ->
    ?DEBUG("connection error: ~p", [Reason]),
    {stop, normal, State};
handle_info({timeout, TRef, stop}, _StateName,
	    #state{tref = TRef} = State) ->
    {stop, normal, State};
handle_info(Info, StateName, State) ->
    ?INFO_MSG("unexpected info: ~p", [Info]),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, State) ->
    catch (State#state.sock_mod):close(State#state.sock),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
process(Addr, Port, #stun{class = request, unsupported = []} = Msg) ->
    Resp = prepare_response(Msg),
    if Msg#stun.method == ?STUN_METHOD_BINDING ->
	    case stun_codec:version(Msg) of
		old ->
		    Resp#stun{class = response,
			      'MAPPED-ADDRESS' = {Addr, Port}};
		new ->
		    Resp#stun{class = response,
			      'XOR-MAPPED-ADDRESS' = {Addr, Port}}
	    end;
       true ->
	    Resp#stun{class = error,
		      'ERROR-CODE' = {405, <<"Method Not Allowed">>}}
    end;
process(_Addr, _Port, #stun{class = request} = Msg) ->
    Resp = prepare_response(Msg),
    Resp#stun{class = error,
	      'UNKNOWN-ATTRIBUTES' = Msg#stun.unsupported,
	      'ERROR-CODE' = {420, stun_codec:reason(420)}};
process(_Addr, _Port, _Msg) ->
    pass.

prepare_response(Msg) ->
    Version = list_to_binary("ejabberd " ++ ?VERSION),
    #stun{method = Msg#stun.method,
	  magic = Msg#stun.magic,
	  trid = Msg#stun.trid,
	  'SOFTWARE' = Version}.

process_data(NextStateName, #state{buf = Buf} = State, Data) ->
    NewBuf = <<Buf/binary, Data/binary>>,
    case stun_codec:decode(NewBuf) of
	{ok, Msg, Tail} ->
	    gen_fsm:send_event(self(), Msg),
	    process_data(NextStateName, State#state{buf = <<>>}, Tail);
	empty ->
	    NewState = State#state{buf = <<>>},
	    {next_state, NextStateName, update_state(NewState)};
	more when size(NewBuf) < ?MAX_BUF_SIZE ->
	    NewState = State#state{buf = NewBuf},
	    {next_state, NextStateName, update_state(NewState)};
	_ ->
	    {stop, normal, State}
    end.

update_state(#state{sock = Sock} = State) ->
    case State#state.sock_mod of
	gen_tcp ->
	    inet:setopts(Sock, [{active, once}]);
	SockMod ->
	    SockMod:setopts(Sock, [{active, once}])
    end,
    cancel_timer(State#state.tref),
    TRef = erlang:start_timer(?TIMEOUT, self(), stop),
    State#state{tref = TRef}.

cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
	false ->
	    receive
                {timeout, TRef, _} ->
                    ok
            after 0 ->
                    ok
            end;
        _ ->
            ok
    end.
