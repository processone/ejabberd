%%%-------------------------------------------------------------------
%%% File    : ejabberd_frontend_socket.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Frontend socket with zlib and TLS support library
%%% Created : 23 Aug 2006 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-module(ejabberd_frontend_socket).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start/4,
	 start_link/5,
	 %connect/3,
	 starttls/2,
	 starttls/3,
	 compress/1,
	 compress/2,
	 reset_stream/1,
	 send/2,
	 change_shaper/2,
	 monitor/1,
	 get_sockmod/1,
	 get_peer_certificate/1,
	 get_verify_result/1,
	 close/1,
	 sockname/1, peername/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-record(state, {sockmod, socket, receiver}).

-define(HIBERNATE_TIMEOUT, 90000).

%%====================================================================
%% API
%%====================================================================
start_link(Module, SockMod, Socket, Opts, Receiver) ->
    gen_server:start_link(?MODULE,
			  [Module, SockMod, Socket, Opts, Receiver], []).

start(Module, SockMod, Socket, Opts) ->
    case Module:socket_type() of
      xml_stream ->
	  MaxStanzaSize = case lists:keysearch(max_stanza_size, 1,
					       Opts)
			      of
			    {value, {_, Size}} -> Size;
			    _ -> infinity
			  end,
	  Receiver = ejabberd_receiver:start(Socket, SockMod,
					     none, MaxStanzaSize),
	  case SockMod:controlling_process(Socket, Receiver) of
	    ok -> ok;
	    {error, _Reason} -> SockMod:close(Socket)
	  end,
	  supervisor:start_child(ejabberd_frontend_socket_sup,
				 [Module, SockMod, Socket, Opts, Receiver]);
      raw ->
	  %{ok, Pid} = Module:start({SockMod, Socket}, Opts),
	  %case SockMod:controlling_process(Socket, Pid) of
	  %    ok ->
	  %        ok;
	  %    {error, _Reason} ->
	  %        SockMod:close(Socket)
	  %end
	  todo
    end.

starttls(FsmRef, _TLSOpts) ->
    %% TODO: Frontend improvements planned by Aleksey
    %%gen_server:call(FsmRef, {starttls, TLSOpts}),
    FsmRef.

starttls(FsmRef, TLSOpts, Data) ->
    gen_server:call(FsmRef, {starttls, TLSOpts, Data}),
    FsmRef.

compress(FsmRef) -> compress(FsmRef, undefined).

compress(FsmRef, Data) ->
    gen_server:call(FsmRef, {compress, Data}), FsmRef.

reset_stream(FsmRef) ->
    gen_server:call(FsmRef, reset_stream).

send(FsmRef, Data) ->
    gen_server:call(FsmRef, {send, Data}).

change_shaper(FsmRef, Shaper) ->
    gen_server:call(FsmRef, {change_shaper, Shaper}).

monitor(FsmRef) -> erlang:monitor(process, FsmRef).

get_sockmod(FsmRef) ->
    gen_server:call(FsmRef, get_sockmod).

get_peer_certificate(FsmRef) ->
    gen_server:call(FsmRef, get_peer_certificate).

get_verify_result(FsmRef) ->
    gen_server:call(FsmRef, get_verify_result).

close(FsmRef) -> gen_server:call(FsmRef, close).

sockname(FsmRef) -> gen_server:call(FsmRef, sockname).

peername(_FsmRef) ->
    %% TODO: Frontend improvements planned by Aleksey
    %%gen_server:call(FsmRef, peername).
    {ok, {{0, 0, 0, 0}, 0}}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Module, SockMod, Socket, Opts, Receiver]) ->
    Node = ejabberd_node_groups:get_closest_node(backend),
    {SockMod2, Socket2} = check_starttls(SockMod, Socket, Receiver, Opts),
    {ok, Pid} =
	rpc:call(Node, Module, start, [{?MODULE, self()}, Opts]),
    ejabberd_receiver:become_controller(Receiver, Pid),
    {ok, #state{sockmod = SockMod2,
		socket = Socket2,
		receiver = Receiver}}.

handle_call({starttls, TLSOpts}, _From, State) ->
    {ok, TLSSocket} = p1_tls:tcp_to_tls(State#state.socket, TLSOpts),
    ejabberd_receiver:starttls(State#state.receiver, TLSSocket),
    Reply = ok,
    {reply, Reply, State#state{socket = TLSSocket, sockmod = p1_tls},
     ?HIBERNATE_TIMEOUT};

handle_call({starttls, TLSOpts, Data}, _From, State) ->
    {ok, TLSSocket} = p1_tls:tcp_to_tls(State#state.socket, TLSOpts),
    ejabberd_receiver:starttls(State#state.receiver, TLSSocket),
    catch (State#state.sockmod):send(
	    State#state.socket, Data),
    Reply = ok,
    {reply, Reply,
     State#state{socket = TLSSocket, sockmod = p1_tls},
     ?HIBERNATE_TIMEOUT};
handle_call({compress, Data}, _From, State) ->
    {ok, ZlibSocket} =
        ejabberd_receiver:compress(State#state.receiver, Data),
    Reply = ok,
    {reply, Reply,
     State#state{socket = ZlibSocket, sockmod = ezlib},
     ?HIBERNATE_TIMEOUT};
handle_call(reset_stream, _From, State) ->
    ejabberd_receiver:reset_stream(State#state.receiver),
    Reply = ok,
    {reply, Reply, State, ?HIBERNATE_TIMEOUT};
handle_call({send, Data}, _From, State) ->
    catch (State#state.sockmod):send(State#state.socket, Data),
    Reply = ok,
    {reply, Reply, State, ?HIBERNATE_TIMEOUT};
handle_call({change_shaper, Shaper}, _From, State) ->
    ejabberd_receiver:change_shaper(State#state.receiver,
				    Shaper),
    Reply = ok,
    {reply, Reply, State, ?HIBERNATE_TIMEOUT};
handle_call(get_sockmod, _From, State) ->
    Reply = State#state.sockmod,
    {reply, Reply, State, ?HIBERNATE_TIMEOUT};
handle_call(get_peer_certificate, _From, State) ->
    Reply = p1_tls:get_peer_certificate(State#state.socket),
    {reply, Reply, State, ?HIBERNATE_TIMEOUT};
handle_call(get_verify_result, _From, State) ->
    Reply = p1_tls:get_verify_result(State#state.socket),
    {reply, Reply, State, ?HIBERNATE_TIMEOUT};
handle_call(close, _From, State) ->
    ejabberd_receiver:close(State#state.receiver),
    Reply = ok,
    {stop, normal, Reply, State};
handle_call(sockname, _From, State) ->
    #state{sockmod = SockMod, socket = Socket} = State,
    Reply =
	case SockMod of
	    gen_tcp ->
		inet:sockname(Socket);
	    _ ->
		SockMod:sockname(Socket)
	end,
    {reply, Reply, State, ?HIBERNATE_TIMEOUT};
handle_call(peername, _From, State) ->
    #state{sockmod = SockMod, socket = Socket} = State,
    Reply = case SockMod of
	      gen_tcp -> inet:peername(Socket);
	      _ -> SockMod:peername(Socket)
	    end,
    {reply, Reply, State, ?HIBERNATE_TIMEOUT};
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State, ?HIBERNATE_TIMEOUT}.

handle_cast(_Msg, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

handle_info(timeout, State) ->
    proc_lib:hibernate(gen_server, enter_loop,
		       [?MODULE, [], State]),
    {noreply, State, ?HIBERNATE_TIMEOUT};
handle_info(_Info, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

check_starttls(SockMod, Socket, Receiver, Opts) ->
    TLSEnabled = proplists:get_bool(tls, Opts),
    TLSOpts = lists:filter(fun({certfile, _}) -> true;
			      (_) -> false
			   end, Opts),
    if TLSEnabled ->
	    {ok, TLSSocket} = p1_tls:tcp_to_tls(Socket, TLSOpts),
	    ejabberd_receiver:starttls(Receiver, TLSSocket),
	    {p1_tls, TLSSocket};
	true ->
	    {SockMod, Socket}
    end.
