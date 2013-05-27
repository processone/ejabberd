%%%----------------------------------------------------------------------
%%% File    : ejabberd_receiver.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Socket receiver for C2S and S2S connections
%%% Created : 10 Nov 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_receiver).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/4, start/3, start/4,
	 change_shaper/2, reset_stream/1, starttls/2, starttls/3,
	 compress/2, send/2, become_controller/2,
	 change_controller/2, setopts/2, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-record(state,
	{socket :: inet:socket() | tls:tls_socket() | ezlib:zlib_socket(),
         sock_mod = gen_tcp :: gen_tcp | tls | ezlib,
         shaper_state = none :: shaper:shaper(),
         c2s_pid :: pid(),
	 max_stanza_size = infinity :: non_neg_integer() | infinity,
         xml_stream_state :: xml_stream:xml_stream_state(),
         tref :: reference(),
         timeout = infinity:: timeout()}).

-define(HIBERNATE_TIMEOUT, 90000).

-spec start_link(inet:socket(), atom(), shaper:shaper(),
                 non_neg_integer() | infinity) -> ignore |
                                                  {error, any()} |
                                                  {ok, pid()}.

start_link(Socket, SockMod, Shaper, MaxStanzaSize) ->
    gen_server:start_link(?MODULE,
			  [Socket, SockMod, Shaper, MaxStanzaSize], []).

-spec start(inet:socket(), atom(), shaper:shaper()) -> undefined | pid().

start(Socket, SockMod, Shaper) ->
    start(Socket, SockMod, Shaper, infinity).

-spec start(inet:socket(), atom(), shaper:shaper(),
            non_neg_integer() | infinity) -> undefined | pid().

start(Socket, SockMod, Shaper, MaxStanzaSize) ->
    {ok, Pid} =
	supervisor:start_child(ejabberd_receiver_sup,
			       [Socket, SockMod, Shaper, MaxStanzaSize]),
    Pid.

-spec change_shaper(pid(), shaper:shaper()) -> ok.

change_shaper(Pid, Shaper) ->
    gen_server:cast(Pid, {change_shaper, Shaper}).

-spec reset_stream(pid()) -> ok | {error, any()}.

reset_stream(Pid) -> do_call(Pid, reset_stream).

-spec starttls(pid(), iodata()) -> {ok, tls:tls_socket()} | {error, any()}.

starttls(Pid, TLSOpts) ->
    starttls(Pid, TLSOpts, undefined).

-spec starttls(pid(), list(), iodata() | undefined) -> {error, any()} |
                                                       {ok, tls:tls_socket()}.

starttls(Pid, TLSOpts, Data) ->
    do_call(Pid, {starttls, TLSOpts, Data}).

-spec compress(pid(), iodata() | undefined) -> {error, any()} |
                                               {ok, ezlib:zlib_socket()}.

compress(Pid, Data) -> do_call(Pid, {compress, Data}).

-spec become_controller(pid(), pid()) -> ok | {error, any()}.

become_controller(Pid, C2SPid) ->
    do_call(Pid, {become_controller, C2SPid}).

-spec change_controller(pid(), pid()) -> ok | {error, any()}.

change_controller(Pid, C2SPid) ->
    do_call(Pid, {change_controller, C2SPid}).

-spec setopts(pid(), list()) -> ok | {error, any()}.

setopts(Pid, Opts) ->
    case lists:member({active, false}, Opts) of
      true ->
            do_call(Pid, deactivate_socket);
      false -> ok
    end.

-spec send(pid(), iodata()) -> ok | {error, any()}.

send(Pid, Data) -> do_call(Pid, {send, Data}).

-spec close(pid()) -> ok.

close(Pid) -> gen_server:cast(Pid, close).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Socket, SockMod, Shaper, MaxStanzaSize]) ->
    ShaperState = shaper:new(Shaper),
    Timeout = case SockMod of
		ssl -> 20;
		_ -> infinity
	      end,
    {ok,
     #state{socket = Socket, sock_mod = SockMod,
	    shaper_state = ShaperState,
	    max_stanza_size = MaxStanzaSize, timeout = Timeout}}.

handle_call({starttls, TLSOpts, Data}, _From,
	    #state{xml_stream_state = XMLStreamState,
		   c2s_pid = C2SPid, socket = Socket,
		   max_stanza_size = MaxStanzaSize} =
		State) ->
    {ok, TLSSocket} = tls:tcp_to_tls(Socket, TLSOpts),
    if Data /= undefined -> do_send(State, Data);
       true -> ok
    end,
    close_stream(XMLStreamState),
    NewXMLStreamState = xml_stream:new(C2SPid,
				       MaxStanzaSize),
    NewState = State#state{socket = TLSSocket,
			   sock_mod = tls,
			   xml_stream_state = NewXMLStreamState},
    case tls:recv_data(TLSSocket, <<"">>) of
      {ok, TLSData} ->
	  {reply, {ok, TLSSocket},
	   process_data(TLSData, NewState), ?HIBERNATE_TIMEOUT};
      {error, _Reason} -> {stop, normal, ok, NewState}
    end;
handle_call({compress, Data}, _From,
	    #state{xml_stream_state = XMLStreamState,
		   c2s_pid = C2SPid, socket = Socket, sock_mod = SockMod,
		   max_stanza_size = MaxStanzaSize} =
		State) ->
    {ok, ZlibSocket} = ezlib:enable_zlib(SockMod,
						 Socket),
    if Data /= undefined -> do_send(State, Data);
       true -> ok
    end,
    close_stream(XMLStreamState),
    NewXMLStreamState = xml_stream:new(C2SPid,
				       MaxStanzaSize),
    NewState = State#state{socket = ZlibSocket,
			   sock_mod = ezlib,
			   xml_stream_state = NewXMLStreamState},
    case ezlib:recv_data(ZlibSocket, <<"">>) of
      {ok, ZlibData} ->
	  {reply, {ok, ZlibSocket},
	   process_data(ZlibData, NewState), ?HIBERNATE_TIMEOUT};
      {error, _Reason} -> {stop, normal, ok, NewState}
    end;
handle_call(reset_stream, _From,
	    #state{xml_stream_state = XMLStreamState,
		   c2s_pid = C2SPid, max_stanza_size = MaxStanzaSize} =
		State) ->
    close_stream(XMLStreamState),
    NewXMLStreamState = xml_stream:new(C2SPid,
				       MaxStanzaSize),
    Reply = ok,
    {reply, Reply,
     State#state{xml_stream_state = NewXMLStreamState},
     ?HIBERNATE_TIMEOUT};
handle_call({become_controller, C2SPid}, _From,
	    State) ->
    erlang:monitor(process, C2SPid),
    XMLStreamState = xml_stream:new(C2SPid,
				    State#state.max_stanza_size),
    NewState = State#state{c2s_pid = C2SPid,
			   xml_stream_state = XMLStreamState},
    activate_socket(NewState),
    Reply = ok,
    {reply, Reply, NewState, ?HIBERNATE_TIMEOUT};
handle_call({change_controller, C2SPid}, _From,
	    State) ->
    erlang:monitor(process, C2SPid),
    NewXMLStreamState =
	xml_stream:change_callback_pid(State#state.xml_stream_state,
				       C2SPid),
    NewState = State#state{c2s_pid = C2SPid,
			   xml_stream_state = NewXMLStreamState},
    activate_socket(NewState),
    {reply, ok, NewState, ?HIBERNATE_TIMEOUT};
handle_call({send, Data}, _From, State) ->
    case do_send(State, Data) of
      ok -> {reply, ok, State, ?HIBERNATE_TIMEOUT};
      {error, _Reason} = Err -> {stop, normal, Err, State}
    end;
handle_call(deactivate_socket, _From, State) ->
    deactivate_socket(State),
    {reply, ok, State, ?HIBERNATE_TIMEOUT};
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State, ?HIBERNATE_TIMEOUT}.

handle_cast({change_shaper, Shaper}, State) ->
    NewShaperState = shaper:new(Shaper),
    {noreply, State#state{shaper_state = NewShaperState},
     ?HIBERNATE_TIMEOUT};
handle_cast(close, State) -> {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

handle_info({Tag, _TCPSocket, Data},
	    #state{socket = Socket, sock_mod = SockMod} = State)
    when (Tag == tcp) or (Tag == ssl) or
	   (Tag == ejabberd_xml) ->
    case SockMod of
      tls ->
	  case tls:recv_data(Socket, Data) of
	    {ok, TLSData} ->
		{noreply, process_data(TLSData, State),
		 ?HIBERNATE_TIMEOUT};
	    {error, _Reason} -> {stop, normal, State}
	  end;
      ezlib ->
	  case ezlib:recv_data(Socket, Data) of
	    {ok, ZlibData} ->
		{noreply, process_data(ZlibData, State),
		 ?HIBERNATE_TIMEOUT};
	    {error, _Reason} -> {stop, normal, State}
	  end;
      _ ->
	  {noreply, process_data(Data, State), ?HIBERNATE_TIMEOUT}
    end;
handle_info({Tag, _TCPSocket}, State)
    when (Tag == tcp_closed) or (Tag == ssl_closed) ->
    {stop, normal, State};
handle_info({Tag, _TCPSocket, Reason}, State)
    when (Tag == tcp_error) or (Tag == ssl_error) ->
    case Reason of
      timeout -> {noreply, State, ?HIBERNATE_TIMEOUT};
      _ -> {stop, normal, State}
    end;
handle_info({'DOWN', _MRef, process, C2SPid, _},
	    #state{c2s_pid = C2SPid} = State) ->
    {stop, normal, State};
handle_info({timeout, _Ref, activate}, State) ->
    activate_socket(State),
    {noreply, State, ?HIBERNATE_TIMEOUT};
handle_info(timeout, State) ->
    proc_lib:hibernate(gen_server, enter_loop,
		       [?MODULE, [], State]),
    {noreply, State, ?HIBERNATE_TIMEOUT};
handle_info(_Info, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

terminate(_Reason,
	  #state{xml_stream_state = XMLStreamState,
		 c2s_pid = C2SPid} =
	      State) ->
    close_stream(XMLStreamState),
    if C2SPid /= undefined ->
	   gen_fsm:send_event(C2SPid, closed);
       true -> ok
    end,
    catch (State#state.sock_mod):close(State#state.socket),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

activate_socket(#state{socket = Socket,
		       sock_mod = SockMod}) ->
    PeerName = case SockMod of
		 gen_tcp ->
		     inet:setopts(Socket, [{active, once}]),
		     inet:peername(Socket);
		 _ ->
		     SockMod:setopts(Socket, [{active, once}]),
		     SockMod:peername(Socket)
	       end,
    case PeerName of
      {error, _Reason} -> self() ! {tcp_closed, Socket};
      {ok, _} -> ok
    end.

deactivate_socket(#state{socket = Socket, tref = TRef,
			 sock_mod = SockMod}) ->
    cancel_timer(TRef),
    case SockMod of
      gen_tcp -> inet:setopts(Socket, [{active, false}]);
      _ -> SockMod:setopts(Socket, [{active, false}])
    end.

process_data([], State) ->
    activate_socket(State), State;
process_data([Element | Els],
	     #state{c2s_pid = C2SPid} = State)
    when element(1, Element) == xmlel;
	 element(1, Element) == xmlstreamstart;
	 element(1, Element) == xmlstreamelement;
	 element(1, Element) == xmlstreamend ->
    if C2SPid == undefined -> State;
       true ->
	   catch gen_fsm:send_event(C2SPid,
				    element_wrapper(Element)),
	   process_data(Els, State)
    end;
%% Data processing for connectors receivind data as string.
process_data(Data,
	     #state{xml_stream_state = XMLStreamState, tref = TRef,
		    shaper_state = ShaperState, c2s_pid = C2SPid} =
		 State) ->
    ?DEBUG("Received XML on stream = ~p", [(Data)]),
    XMLStreamState1 = xml_stream:parse(XMLStreamState,
				       Data),
    {NewShaperState, Pause} = shaper:update(ShaperState,
					    byte_size(Data)),
    NewTRef = if C2SPid == undefined -> TRef;
		 Pause > 0 ->
		     erlang:start_timer(Pause, self(), activate);
		 true -> activate_socket(State), TRef
	      end,
    State#state{xml_stream_state = XMLStreamState1,
		tref = NewTRef, shaper_state = NewShaperState}.

element_wrapper(XMLElement)
    when element(1, XMLElement) == xmlel ->
    {xmlstreamelement, XMLElement};
element_wrapper(Element) -> Element.

close_stream(undefined) -> ok;
close_stream(XMLStreamState) ->
    xml_stream:close(XMLStreamState).

do_send(State, Data) ->
    (State#state.sock_mod):send(State#state.socket, Data).

cancel_timer(TRef) when is_reference(TRef) ->
    case erlang:cancel_timer(TRef) of
      false ->
	  receive {timeout, TRef, _} -> ok after 0 -> ok end;
      _ -> ok
    end;
cancel_timer(_) -> ok.

do_call(Pid, Msg) ->
    case catch gen_server:call(Pid, Msg) of
      {'EXIT', Why} -> {error, Why};
      Res -> Res
    end.
