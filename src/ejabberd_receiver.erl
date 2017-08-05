%%%----------------------------------------------------------------------
%%% File    : ejabberd_receiver.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Socket receiver for C2S and S2S connections
%%% Created : 10 Nov 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_receiver).

-author('alexey@process-one.net').

-ifndef(GEN_SERVER).
-define(GEN_SERVER, gen_server).
-endif.
-behaviour(?GEN_SERVER).
-behaviour(ejabberd_config).

%% API
-export([start_link/4,
	 start/3,
	 start/4,
	 change_shaper/2,
	 reset_stream/1,
	 starttls/2,
	 compress/2,
	 become_controller/2,
	 close/1,
	 opt_type/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-record(state,
	{socket :: inet:socket() | fast_tls:tls_socket() | ezlib:zlib_socket(),
         sock_mod = gen_tcp :: gen_tcp | fast_tls | ezlib,
         shaper_state = none :: shaper:shaper(),
         c2s_pid :: pid() | undefined,
	 max_stanza_size = infinity :: non_neg_integer() | infinity,
         xml_stream_state :: fxml_stream:xml_stream_state() | undefined,
         timeout = infinity:: timeout()}).

-spec start_link(inet:socket(), atom(), shaper:shaper(),
                 non_neg_integer() | infinity) -> ignore |
                                                  {error, any()} |
                                                  {ok, pid()}.

start_link(Socket, SockMod, Shaper, MaxStanzaSize) ->
    ?GEN_SERVER:start_link(?MODULE,
			  [Socket, SockMod, Shaper, MaxStanzaSize], []).

-spec start(inet:socket(), atom(), shaper:shaper()) -> undefined | pid().

start(Socket, SockMod, Shaper) ->
    start(Socket, SockMod, Shaper, infinity).

-spec start(inet:socket(), atom(), shaper:shaper(),
            non_neg_integer() | infinity) -> undefined | pid().

start(Socket, SockMod, Shaper, MaxStanzaSize) ->
    {ok, Pid} = ?GEN_SERVER:start(ejabberd_receiver,
				 [Socket, SockMod, Shaper, MaxStanzaSize], []),
    Pid.

-spec change_shaper(pid(), shaper:shaper()) -> ok.

change_shaper(Pid, Shaper) ->
    ?GEN_SERVER:cast(Pid, {change_shaper, Shaper}).

-spec reset_stream(pid()) -> ok | {error, any()}.

reset_stream(Pid) -> do_call(Pid, reset_stream).

-spec starttls(pid(), fast_tls:tls_socket()) -> ok | {error, any()}.

starttls(Pid, TLSSocket) ->
    do_call(Pid, {starttls, TLSSocket}).

-spec compress(pid(), iodata() | undefined) -> {error, any()} |
                                               {ok, ezlib:zlib_socket()}.

compress(Pid, Data) ->
    do_call(Pid, {compress, Data}).

-spec become_controller(pid(), pid()) -> ok | {error, any()}.

become_controller(Pid, C2SPid) ->
    do_call(Pid, {become_controller, C2SPid}).

-spec close(pid()) -> ok.

close(Pid) ->
    ?GEN_SERVER:cast(Pid, close).


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

handle_call({starttls, TLSSocket}, _From, State) ->
    State1 = reset_parser(State),
    NewState = State1#state{socket = TLSSocket,
                            sock_mod = fast_tls},
    case fast_tls:recv_data(TLSSocket, <<"">>) of
	{ok, TLSData} ->
	    {reply, ok,
		process_data(TLSData, NewState), hibernate_timeout()};
	{error, _} = Err ->
	    {stop, normal, Err, NewState}
    end;
handle_call({compress, Data}, _From,
	    #state{socket = Socket, sock_mod = SockMod} =
		State) ->
    ejabberd:start_app(ezlib),
    {ok, ZlibSocket} = ezlib:enable_zlib(SockMod,
						 Socket),
    if Data /= undefined -> do_send(State, Data);
       true -> ok
    end,
    State1 = reset_parser(State),
    NewState = State1#state{socket = ZlibSocket,
			   sock_mod = ezlib},
    case ezlib:recv_data(ZlibSocket, <<"">>) of
      {ok, ZlibData} ->
	    {reply, {ok, ZlibSocket},
		process_data(ZlibData, NewState), hibernate_timeout()};
      {error, _} = Err ->
	    {stop, normal, Err, NewState}
    end;
handle_call(reset_stream, _From, State) ->
    NewState = reset_parser(State),
    Reply = ok,
    {reply, Reply, NewState, hibernate_timeout()};
handle_call({become_controller, C2SPid}, _From, State) ->
    XMLStreamState = fxml_stream:new(C2SPid, State#state.max_stanza_size),
    NewState = State#state{c2s_pid = C2SPid,
			   xml_stream_state = XMLStreamState},
    activate_socket(NewState),
    Reply = ok,
    {reply, Reply, NewState, hibernate_timeout()};
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State, hibernate_timeout()}.

handle_cast({change_shaper, Shaper}, State) ->
    NewShaperState = shaper:new(Shaper),
    {noreply, State#state{shaper_state = NewShaperState},
     hibernate_timeout()};
handle_cast(close, State) -> {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State, hibernate_timeout()}.

handle_info({Tag, _TCPSocket, Data},
	    #state{socket = Socket, sock_mod = SockMod} = State)
    when (Tag == tcp) or (Tag == ssl) or
	   (Tag == ejabberd_xml) ->
    case SockMod of
      fast_tls ->
	  case fast_tls:recv_data(Socket, Data) of
	    {ok, TLSData} ->
		{noreply, process_data(TLSData, State),
		 hibernate_timeout()};
	    {error, Reason} ->
		  if is_binary(Reason) ->
			  ?DEBUG("TLS error = ~s", [Reason]);
		     true ->
			  ok
		  end,
		  {stop, normal, State}
	  end;
      ezlib ->
	  case ezlib:recv_data(Socket, Data) of
	    {ok, ZlibData} ->
		{noreply, process_data(ZlibData, State),
		 hibernate_timeout()};
	    {error, _Reason} -> {stop, normal, State}
	  end;
      _ ->
	  {noreply, process_data(Data, State), hibernate_timeout()}
    end;
handle_info({Tag, _TCPSocket}, State)
    when (Tag == tcp_closed) or (Tag == ssl_closed) ->
    {stop, normal, State};
handle_info({Tag, _TCPSocket, Reason}, State)
    when (Tag == tcp_error) or (Tag == ssl_error) ->
    case Reason of
      timeout -> {noreply, State, hibernate_timeout()};
      _ -> {stop, normal, State}
    end;
handle_info({timeout, _Ref, activate}, State) ->
    activate_socket(State),
    {noreply, State, hibernate_timeout()};
handle_info(timeout, State) ->
    proc_lib:hibernate(?GEN_SERVER, enter_loop,
		       [?MODULE, [], State]),
    {noreply, State, hibernate_timeout()};
handle_info(_Info, State) ->
    {noreply, State, hibernate_timeout()}.

terminate(_Reason,
	  #state{xml_stream_state = XMLStreamState,
		 c2s_pid = C2SPid} =
	      State) ->
    close_stream(XMLStreamState),
    if C2SPid /= undefined ->
	   p1_fsm:send_event(C2SPid, closed);
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
    Res = case SockMod of
	      gen_tcp ->
		  inet:setopts(Socket, [{active, once}]);
	      _ ->
		  SockMod:setopts(Socket, [{active, once}])
	  end,
    case Res of
      {error, _Reason} -> self() ! {tcp_closed, Socket};
      ok -> ok
    end.

%% Data processing for connectors directly generating xmlelement in
%% Erlang data structure.
%% WARNING: Shaper does not work with Erlang data structure.
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
	   catch p1_fsm:send_event(C2SPid,
				    element_wrapper(Element)),
	   process_data(Els, State)
    end;
%% Data processing for connectors receivind data as string.
process_data(Data,
	     #state{xml_stream_state = XMLStreamState,
		    shaper_state = ShaperState, c2s_pid = C2SPid} =
		 State) ->
    ?DEBUG("Received XML on stream = ~p", [(Data)]),
    XMLStreamState1 = case XMLStreamState of
                          undefined ->
                              XMLStreamState;
                          _ ->
                              fxml_stream:parse(XMLStreamState, Data)
                      end,
    {NewShaperState, Pause} = shaper:update(ShaperState, byte_size(Data)),
    if
	C2SPid == undefined ->
	    ok;
	Pause > 0 ->
	    erlang:start_timer(Pause, self(), activate);
	true ->
	    activate_socket(State)
    end,
    State#state{xml_stream_state = XMLStreamState1,
		shaper_state = NewShaperState}.

%% Element coming from XML parser are wrapped inside xmlstreamelement
%% When we receive directly xmlelement tuple (from a socket module
%% speaking directly Erlang XML), we wrap it inside the same
%% xmlstreamelement coming from the XML parser.
element_wrapper(XMLElement)
    when element(1, XMLElement) == xmlel ->
    {xmlstreamelement, XMLElement};
element_wrapper(Element) -> Element.

close_stream(undefined) -> ok;
close_stream(XMLStreamState) ->
    fxml_stream:close(XMLStreamState).

reset_parser(#state{xml_stream_state = undefined} = State) ->
    State;
reset_parser(#state{c2s_pid = C2SPid,
                    max_stanza_size = MaxStanzaSize,
                    xml_stream_state = XMLStreamState}
             = State) ->
    NewStreamState = try fxml_stream:reset(XMLStreamState)
                     catch error:_ ->
                             close_stream(XMLStreamState),
                             case C2SPid of
                                 undefined ->
                                     undefined;
                                 _ ->
                                     fxml_stream:new(C2SPid, MaxStanzaSize)
                             end
                     end,
    State#state{xml_stream_state = NewStreamState}.

do_send(State, Data) ->
    (State#state.sock_mod):send(State#state.socket, Data).

do_call(Pid, Msg) ->
    try ?GEN_SERVER:call(Pid, Msg) of
	Res -> Res
    catch _:{timeout, _} ->
	    {error, timeout};
	  _:_ ->
	    {error, einval}
    end.

hibernate_timeout() ->
    ejabberd_config:get_option(receiver_hibernate, timer:seconds(90)).

-spec opt_type(receiver_hibernate) -> fun((pos_integer() | hibernate) ->
					   pos_integer() | hibernate);
	      (atom()) -> [atom()].
opt_type(receiver_hibernate) ->
    fun(I) when is_integer(I), I>0 -> I;
       (hibernate) -> hibernate
    end;
opt_type(_) ->
    [receiver_hibernate].
