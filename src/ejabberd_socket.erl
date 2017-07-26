%%%----------------------------------------------------------------------
%%% File    : ejabberd_socket.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Socket with zlib and TLS support library
%%% Created : 23 Aug 2006 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-module(ejabberd_socket).

-author('alexey@process-one.net').

%% API
-export([start/4,
	 connect/3,
	 connect/4,
	 connect/5,
	 starttls/2,
	 compress/1,
	 compress/2,
	 reset_stream/1,
	 send_element/2,
	 send_header/2,
	 send_trailer/1,
	 send/2,
	 send_xml/2,
	 change_shaper/2,
	 monitor/1,
	 get_sockmod/1,
	 get_transport/1,
	 get_peer_certificate/2,
	 get_verify_result/1,
	 close/1,
	 pp/1,
	 sockname/1, peername/1]).

-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").

-type sockmod() :: ejabberd_bosh |
                   ejabberd_http_ws |
                   gen_tcp | fast_tls | ezlib.
-type receiver() :: pid () | atom().
-type socket() :: pid() | inet:socket() |
                  fast_tls:tls_socket() |
		  ezlib:zlib_socket() |
		  ejabberd_bosh:bosh_socket() |
		  ejabberd_http_ws:ws_socket().

-record(socket_state, {sockmod = gen_tcp :: sockmod(),
                       socket = self() :: socket(),
                       receiver = self() :: receiver()}).

-type socket_state() :: #socket_state{}.

-export_type([socket/0, socket_state/0, sockmod/0]).

-callback start({module(), socket_state()},
		[proplists:property()]) -> {ok, pid()} | {error, term()} | ignore.
-callback start_link({module(), socket_state()},
		     [proplists:property()]) -> {ok, pid()} | {error, term()} | ignore.
-callback socket_type() -> xml_stream | independent | raw.

-define(is_http_socket(S),
	(S#socket_state.sockmod == ejabberd_bosh orelse
	 S#socket_state.sockmod == ejabberd_http_ws)).

%%====================================================================
%% API
%%====================================================================
-spec start(atom(), sockmod(), socket(), [proplists:property()])
      -> {ok, pid() | independent} | {error, inet:posix() | any()} | ignore.
start(Module, SockMod, Socket, Opts) ->
    case Module:socket_type() of
	independent -> {ok, independent};
      xml_stream ->
	    MaxStanzaSize = proplists:get_value(max_stanza_size, Opts, infinity),
	    {ReceiverMod, Receiver, RecRef} =
		try SockMod:custom_receiver(Socket) of
					      {receiver, RecMod, RecPid} ->
			{RecMod, RecPid, RecMod}
		catch _:_ ->
			RecPid = ejabberd_receiver:start(
				   Socket, SockMod, none, MaxStanzaSize),
			{ejabberd_receiver, RecPid, RecPid}
					    end,
	  SocketData = #socket_state{sockmod = SockMod,
				     socket = Socket, receiver = RecRef},
	  case Module:start({?MODULE, SocketData}, Opts) of
	    {ok, Pid} ->
		case SockMod:controlling_process(Socket, Receiver) of
			ok ->
			    ReceiverMod:become_controller(Receiver, Pid),
			    {ok, Receiver};
			Err ->
			    SockMod:close(Socket),
			    Err
		    end;
		Err ->
		SockMod:close(Socket),
		case ReceiverMod of
		  ejabberd_receiver -> ReceiverMod:close(Receiver);
		  _ -> ok
		    end,
		    Err
	  end;
      raw ->
	  case Module:start({SockMod, Socket}, Opts) of
	    {ok, Pid} ->
		case SockMod:controlling_process(Socket, Pid) of
			ok ->
			    {ok, Pid};
			{error, _} = Err ->
			    SockMod:close(Socket),
			    Err
		end;
		Err ->
		    SockMod:close(Socket),
		    Err
	  end
    end.

connect(Addr, Port, Opts) ->
    connect(Addr, Port, Opts, infinity, self()).

connect(Addr, Port, Opts, Timeout) ->
    connect(Addr, Port, Opts, Timeout, self()).

connect(Addr, Port, Opts, Timeout, Owner) ->
    case gen_tcp:connect(Addr, Port, Opts, Timeout) of
      {ok, Socket} ->
	  Receiver = ejabberd_receiver:start(Socket, gen_tcp,
					     none),
	  SocketData = #socket_state{sockmod = gen_tcp,
				     socket = Socket, receiver = Receiver},
	  case gen_tcp:controlling_process(Socket, Receiver) of
	    ok ->
		ejabberd_receiver:become_controller(Receiver, Owner),
		{ok, SocketData};
	    {error, _Reason} = Error -> gen_tcp:close(Socket), Error
	  end;
      {error, _Reason} = Error -> Error
    end.

starttls(#socket_state{socket = Socket,
		       receiver = Receiver} = SocketData, TLSOpts) ->
    case fast_tls:tcp_to_tls(Socket, TLSOpts) of
	{ok, TLSSocket} ->
	    case ejabberd_receiver:starttls(Receiver, TLSSocket) of
		ok ->
		    {ok, SocketData#socket_state{socket = TLSSocket,
						 sockmod = fast_tls}};
		{error, _} = Err ->
		    Err
	    end;
	{error, _} = Err ->
	    Err
    end.

compress(SocketData) -> compress(SocketData, undefined).

compress(SocketData, Data) ->
    case ejabberd_receiver:compress(SocketData#socket_state.receiver, Data) of
	{ok, ZlibSocket} ->
	    {ok, SocketData#socket_state{socket = ZlibSocket, sockmod = ezlib}};
	Err ->
	    ?ERROR_MSG("compress failed: ~p", [Err]),
	    Err
    end.

reset_stream(SocketData)
    when is_pid(SocketData#socket_state.receiver) ->
    ejabberd_receiver:reset_stream(SocketData#socket_state.receiver);
reset_stream(SocketData)
    when is_atom(SocketData#socket_state.receiver) ->
    (SocketData#socket_state.receiver):reset_stream(SocketData#socket_state.socket).

-spec send_element(socket_state(), fxml:xmlel()) -> ok | {error, inet:posix()}.
send_element(SocketData, El) when ?is_http_socket(SocketData) ->
    send_xml(SocketData, {xmlstreamelement, El});
send_element(SocketData, El) ->
    send(SocketData, fxml:element_to_binary(El)).

-spec send_header(socket_state(), fxml:xmlel()) -> ok | {error, inet:posix()}.
send_header(SocketData, El) when ?is_http_socket(SocketData) ->
    send_xml(SocketData, {xmlstreamstart, El#xmlel.name, El#xmlel.attrs});
send_header(SocketData, El) ->
    send(SocketData, fxml:element_to_header(El)).

-spec send_trailer(socket_state()) -> ok | {error, inet:posix()}.
send_trailer(SocketData) when ?is_http_socket(SocketData) ->
    send_xml(SocketData, {xmlstreamend, <<"stream:stream">>});
send_trailer(SocketData) ->
    send(SocketData, <<"</stream:stream>">>).

-spec send(socket_state(), iodata()) -> ok | {error, closed | inet:posix()}.
send(#socket_state{sockmod = SockMod, socket = Socket} = SocketData, Data) ->
    ?DEBUG("(~s) Send XML on stream = ~p", [pp(SocketData), Data]),
    try SockMod:send(Socket, Data) of
	{error, einval} -> {error, closed};
	Result -> Result
    catch _:badarg ->
	    %% Some modules throw badarg exceptions on closed sockets
	    %% TODO: their code should be improved
	    {error, closed}
    end.

-spec send_xml(socket_state(),
	       {xmlstreamelement, fxml:xmlel()} |
	       {xmlstreamstart, binary(), [{binary(), binary()}]} |
	       {xmlstreamend, binary()} |
	       {xmlstreamraw, iodata()}) -> term().
send_xml(SocketData, El) ->
    (SocketData#socket_state.sockmod):send_xml(SocketData#socket_state.socket, El).

change_shaper(SocketData, Shaper)
    when is_pid(SocketData#socket_state.receiver) ->
    ejabberd_receiver:change_shaper(SocketData#socket_state.receiver,
				    Shaper);
change_shaper(SocketData, Shaper)
    when is_atom(SocketData#socket_state.receiver) ->
    (SocketData#socket_state.receiver):change_shaper(SocketData#socket_state.socket,
						     Shaper).

monitor(SocketData)
    when is_pid(SocketData#socket_state.receiver) ->
    erlang:monitor(process,
		   SocketData#socket_state.receiver);
monitor(SocketData)
    when is_atom(SocketData#socket_state.receiver) ->
    (SocketData#socket_state.receiver):monitor(SocketData#socket_state.socket).

get_sockmod(SocketData) ->
    SocketData#socket_state.sockmod.

get_transport(#socket_state{sockmod = SockMod,
			    socket = Socket}) ->
    case SockMod of
	gen_tcp -> tcp;
	fast_tls -> tls;
	ezlib ->
	    case ezlib:get_sockmod(Socket) of
		gen_tcp -> tcp_zlib;
		fast_tls -> tls_zlib
	    end;
	ejabberd_bosh -> http_bind;
	ejabberd_http_ws -> websocket
    end.

get_peer_certificate(SocketData, Type) ->
    fast_tls:get_peer_certificate(SocketData#socket_state.socket, Type).

get_verify_result(SocketData) ->
    fast_tls:get_verify_result(SocketData#socket_state.socket).

close(SocketData) ->
    ejabberd_receiver:close(SocketData#socket_state.receiver).

sockname(#socket_state{sockmod = SockMod,
		       socket = Socket}) ->
    case SockMod of
      gen_tcp -> inet:sockname(Socket);
      _ -> SockMod:sockname(Socket)
    end.

peername(#socket_state{sockmod = SockMod,
		       socket = Socket}) ->
    case SockMod of
      gen_tcp -> inet:peername(Socket);
      _ -> SockMod:peername(Socket)
    end.

pp(#socket_state{receiver = Receiver} = State) ->
    Transport = get_transport(State),
    io_lib:format("~s|~w", [Transport, Receiver]).
