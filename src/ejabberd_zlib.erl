%%%----------------------------------------------------------------------
%%% File    : ejabberd_zlib.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Interface to exmpp_compress
%%% Created : 19 Jan 2006 by Alexey Shchepin <alexey@process-one.net>
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
%%%----------------------------------------------------------------------

-module(ejabberd_zlib).
-author('alexey@process-one.net').

-export([start/0, start_link/0,
	 enable_zlib/2, disable_zlib/1,
	 send/2,
	 recv/2, recv/3, recv_data/2,
	 setopts/2,
	 get_sockmod/1,
	 sockname/1, peername/1,
	 controlling_process/2,
	 close/1]).

-define(DEFLATE, 1).
-define(INFLATE, 2).

%% Copied from exmpp/src/core/exmpp_compress.erl
-record(compress_socket, {socket,
                          packet_mode = binary,
                          port
                         }).

start() ->
    exmpp_compress:start().

start_link() ->
    exmpp_compress:start_link().


enable_zlib(SockMod, Socket) ->
    try
	ZlibSock = exmpp_compress:enable_compression({SockMod, Socket},
						     [{compress_method, zlib}]),
	{ok, ZlibSock}
    catch
	Exception ->
	    {error, Exception}
    end.

disable_zlib(ZlibSock) ->
    exmpp_compress:disable_compression(ZlibSock).

recv(Socket, Length) ->
    recv(Socket, Length, infinity).
recv(ZlibSock, _Length, Timeout) ->
    exmpp_compress:recv(ZlibSock, Timeout).

recv_data(#compress_socket{socket = {SockMod, Socket}} = ZlibSock, Packet) ->
    case SockMod of
       gen_tcp ->
           recv_data2(ZlibSock, Packet);
       _ ->
           case SockMod:recv_data(Socket, Packet) of
               {ok, Packet2} ->
                   recv_data2(ZlibSock, Packet2);
               Error ->
                   Error
           end
    end.

recv_data2(ZlibSock, Packet) ->
    exmpp_compress:recv_data(ZlibSock, Packet).

send(ZlibSock, Packet) ->
    exmpp_compress:send(ZlibSock, Packet).


setopts(ZlibSock, Opts) ->
    exmpp_compress:setopts(ZlibSock, Opts).

get_sockmod(#compress_socket{socket = {SockMod, _Port}}) ->
    SockMod.

sockname(ZlibSock) ->
    exmpp_compress:sockname(ZlibSock).

peername(ZlibSock) ->
    exmpp_compress:peername(ZlibSock).

controlling_process(ZlibSock, Pid) ->
    exmpp_compress:controlling_process(ZlibSock, Pid).

close(ZlibSock) ->
    exmpp_compress:close(ZlibSock).


