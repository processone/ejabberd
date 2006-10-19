%%%----------------------------------------------------------------------
%%% File    : ejabberd_socket.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Socket with zlib and TLS support library
%%% Created : 23 Aug 2006 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_socket).
-author('alexey@process-one.net').

%% API
-export([start/4,
	 connect/3,
	 starttls/2,
	 starttls/3,
	 compress/1,
	 compress/2,
	 reset_stream/1,
	 send/2,
	 change_shaper/2,
	 get_sockmod/1,
	 get_peer_certificate/1,
	 get_verify_result/1,
	 close/1,
	 sockname/1, peername/1]).

-record(socket_state, {sockmod, socket, receiver}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
start(Module, SockMod, Socket, Opts) ->
    case Module:socket_type() of
	xml_stream ->
	    MaxStanzaSize =
		case lists:keysearch(max_stanza_size, 1, Opts) of
		    {value, {_, Size}} -> Size;
		    _ -> infinity
		end,
	    Receiver = ejabberd_receiver:start(Socket, SockMod, none, MaxStanzaSize),
	    SocketData = #socket_state{sockmod = SockMod,
				       socket = Socket,
				       receiver = Receiver},
	    {ok, Pid} = Module:start({?MODULE, SocketData}, Opts),
	    case SockMod:controlling_process(Socket, Receiver) of
		ok ->
		    ok;
		{error, _Reason} ->
		    SockMod:close(Socket)
	    end,
	    ejabberd_receiver:become_controller(Receiver, Pid);
	raw ->
	    {ok, Pid} = Module:start({SockMod, Socket}, Opts),
	    case SockMod:controlling_process(Socket, Pid) of
		ok ->
		    ok;
		{error, _Reason} ->
		    SockMod:close(Socket)
	    end
    end.

connect(Addr, Port, Opts) ->
    case gen_tcp:connect(Addr, Port, Opts) of
	{ok, Socket} ->
	    Receiver = ejabberd_receiver:start(Socket, gen_tcp, none),
	    SocketData = #socket_state{sockmod = gen_tcp,
				       socket = Socket,
				       receiver = Receiver},
	    Pid = self(),
	    case gen_tcp:controlling_process(Socket, Receiver) of
		ok ->
		    ejabberd_receiver:become_controller(Receiver, Pid),
		    {ok, SocketData};
		{error, _Reason} = Error ->
		    gen_tcp:close(Socket),
		    Error
	    end;
	{error, _Reason} = Error ->
	    Error
    end.

starttls(SocketData, TLSOpts) ->
    {ok, TLSSocket} = tls:tcp_to_tls(SocketData#socket_state.socket, TLSOpts),
    ejabberd_receiver:starttls(SocketData#socket_state.receiver, TLSSocket),
    SocketData#socket_state{socket = TLSSocket, sockmod = tls}.

starttls(SocketData, TLSOpts, Data) ->
    {ok, TLSSocket} = tls:tcp_to_tls(SocketData#socket_state.socket, TLSOpts),
    ejabberd_receiver:starttls(SocketData#socket_state.receiver, TLSSocket),
    send(SocketData, Data),
    SocketData#socket_state{socket = TLSSocket, sockmod = tls}.

compress(SocketData) ->
    {ok, ZlibSocket} = ejabberd_zlib:enable_zlib(
			 SocketData#socket_state.sockmod,
			 SocketData#socket_state.socket),
    ejabberd_receiver:compress(SocketData#socket_state.receiver, ZlibSocket),
    SocketData#socket_state{socket = ZlibSocket, sockmod = ejabberd_zlib}.

compress(SocketData, Data) ->
    {ok, ZlibSocket} = ejabberd_zlib:enable_zlib(
			 SocketData#socket_state.sockmod,
			 SocketData#socket_state.socket),
    ejabberd_receiver:compress(SocketData#socket_state.receiver, ZlibSocket),
    send(SocketData, Data),
    SocketData#socket_state{socket = ZlibSocket, sockmod = ejabberd_zlib}.

reset_stream(SocketData) ->
    ejabberd_receiver:reset_stream(SocketData#socket_state.receiver).

send(SocketData, Data) ->
    catch (SocketData#socket_state.sockmod):send(
	    SocketData#socket_state.socket, Data).

change_shaper(SocketData, Shaper) ->
    ejabberd_receiver:change_shaper(SocketData#socket_state.receiver, Shaper).

get_sockmod(SocketData) ->
    SocketData#socket_state.sockmod.

get_peer_certificate(SocketData) ->
    tls:get_peer_certificate(SocketData#socket_state.socket).

get_verify_result(SocketData) ->
    tls:get_verify_result(SocketData#socket_state.socket).

close(SocketData) ->
    ejabberd_receiver:close(SocketData#socket_state.receiver).

sockname(#socket_state{sockmod = SockMod, socket = Socket}) ->
    case SockMod of
	gen_tcp ->
	    inet:sockname(Socket);
	_ ->
	    SockMod:sockname(Socket)
    end.

peername(#socket_state{sockmod = SockMod, socket = Socket}) ->
    case SockMod of
	gen_tcp ->
	    inet:peername(Socket);
	_ ->
	    SockMod:peername(Socket)
    end.

%%====================================================================
%% Internal functions
%%====================================================================
