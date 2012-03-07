%%%----------------------------------------------------------------------
%%% File    : ejabberd_zlib.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Interface to zlib
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

-behaviour(gen_server).

-export([start/0, start_link/0,
	 enable_zlib/2, disable_zlib/1,
	 send/2,
	 recv/2, recv/3, recv_data/2,
	 setopts/2,
	 sockname/1, peername/1,
	 get_sockmod/1,
	 controlling_process/2,
	 close/1]).

%% Internal exports, call-back functions.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).

-define(DEFLATE, 1).
-define(INFLATE, 2).

-record(zlibsock, {sockmod, socket, zlibport}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    case erl_ddll:load_driver(ejabberd:get_so_path(), ejabberd_zlib_drv) of
	ok -> ok;
	{error, already_loaded} -> ok
    end,
    Port = open_port({spawn, "ejabberd_zlib_drv"}, [binary]),
    {ok, Port}.


%%% --------------------------------------------------------
%%% The call-back functions.
%%% --------------------------------------------------------

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, Port) ->
    {stop, {port_died, Reason}, Port};

handle_info({'EXIT', _Pid, _Reason}, Port) ->
    {noreply, Port};

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, Port) ->
    Port ! {self, close},
    ok.


enable_zlib(SockMod, Socket) ->
    case erl_ddll:load_driver(ejabberd:get_so_path(), ejabberd_zlib_drv) of
	ok -> ok;
	{error, already_loaded} -> ok
    end,
    Port = open_port({spawn, "ejabberd_zlib_drv"}, [binary]),
    {ok, #zlibsock{sockmod = SockMod, socket = Socket, zlibport = Port}}.

disable_zlib(#zlibsock{sockmod = SockMod, socket = Socket, zlibport = Port}) ->
    port_close(Port),
    {SockMod, Socket}.

recv(Socket, Length) ->
    recv(Socket, Length, infinity).
recv(#zlibsock{sockmod = SockMod, socket = Socket} = ZlibSock,
     Length, Timeout) ->
    case SockMod:recv(Socket, Length, Timeout) of
	{ok, Packet} ->
	    recv_data(ZlibSock, Packet);
	{error, _Reason} = Error ->
	    Error
    end.

recv_data(#zlibsock{sockmod = SockMod, socket = Socket} = ZlibSock, Packet) ->
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
    case catch recv_data1(ZlibSock, Packet) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Res ->
	    Res
    end.

recv_data1(#zlibsock{zlibport = Port} = _ZlibSock, Packet) ->
    case port_control(Port, ?INFLATE, Packet) of
	<<0, In/binary>> ->
	    {ok, In};
	<<1, Error/binary>> ->
	    {error, binary_to_list(Error)}
    end.

send(#zlibsock{sockmod = SockMod, socket = Socket, zlibport = Port},
     Packet) ->
    case port_control(Port, ?DEFLATE, Packet) of
	<<0, Out/binary>> ->
	    SockMod:send(Socket, Out);
	<<1, Error/binary>> ->
	    {error, binary_to_list(Error)}
    end.


setopts(#zlibsock{sockmod = SockMod, socket = Socket}, Opts) ->
    case SockMod of
	gen_tcp ->
	    inet:setopts(Socket, Opts);
	_ ->
	    SockMod:setopts(Socket, Opts)
    end.

sockname(#zlibsock{sockmod = SockMod, socket = Socket}) ->
    case SockMod of
	gen_tcp ->
	    inet:sockname(Socket);
	_ ->
	    SockMod:sockname(Socket)
    end.

get_sockmod(#zlibsock{sockmod = SockMod}) ->
    SockMod.

peername(#zlibsock{sockmod = SockMod, socket = Socket}) ->
    case SockMod of
	gen_tcp ->
	    inet:peername(Socket);
	_ ->
	    SockMod:peername(Socket)
    end.

controlling_process(#zlibsock{sockmod = SockMod, socket = Socket}, Pid) ->
    SockMod:controlling_process(Socket, Pid).

close(#zlibsock{sockmod = SockMod, socket = Socket, zlibport = Port}) ->
    SockMod:close(Socket),
    port_close(Port).


