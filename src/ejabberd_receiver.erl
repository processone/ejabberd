%%%----------------------------------------------------------------------
%%% File    : ejabberd_receiver.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Socket receiver for C2S and S2S connections
%%% Created : 10 Nov 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_receiver).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_server).

%% API
-export([start_link/4,
	 start/3,
	 change_shaper/2,
	 reset_stream/1,
	 starttls/2,
	 compress/2,
	 become_controller/1,
	 close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").

-record(state, {socket,
		sock_mod,
		shaper_state,
		c2s_pid,
		xml_stream_state,
		timeout}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Socket, SockMod, Shaper, C2SPid) ->
    gen_server:start_link(?MODULE, [Socket, SockMod, Shaper, C2SPid], []).

%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Socket, SockMod, Shaper) ->
    {ok, Pid} = supervisor:start_child(ejabberd_receiver_sup,
				       [Socket, SockMod, Shaper, self()]),
    Pid.

change_shaper(Pid, Shaper) ->
    gen_server:cast(Pid, {change_shaper, Shaper}).

reset_stream(Pid) ->
    gen_server:call(Pid, reset_stream).

starttls(Pid, TLSSocket) ->
    gen_server:call(Pid, {starttls, TLSSocket}).

compress(Pid, ZlibSocket) ->
    gen_server:call(Pid, {compress, ZlibSocket}).

become_controller(Pid) ->
    gen_server:call(Pid, become_controller).

close(Pid) ->
    gen_server:cast(Pid, close).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Socket, SockMod, Shaper, C2SPid]) ->
    XMLStreamState = xml_stream:new(C2SPid),
    ShaperState = shaper:new(Shaper),
    Timeout = case SockMod of
		  ssl ->
		      20;
		  _ ->
		      infinity
	      end,
    {ok, #state{socket = Socket,
		sock_mod = SockMod,
		shaper_state = ShaperState,
		c2s_pid = C2SPid,
		xml_stream_state = XMLStreamState,
		timeout = Timeout}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({starttls, TLSSocket}, _From,
	    #state{xml_stream_state = XMLStreamState,
		   c2s_pid = C2SPid} = State) ->
    xml_stream:close(XMLStreamState),
    NewXMLStreamState = xml_stream:new(C2SPid),
    NewState = State#state{socket = TLSSocket,
			   sock_mod = tls,
			   xml_stream_state = NewXMLStreamState},
    case tls:recv_data(TLSSocket, "") of
	{ok, TLSData} ->
	    {reply, ok, process_data(TLSData, NewState)};
	{error, _Reason} ->
	    {stop, normal, ok, NewState}
    end;
handle_call({compress, ZlibSocket}, _From,
	    #state{xml_stream_state = XMLStreamState,
		   c2s_pid = C2SPid} = State) ->
    xml_stream:close(XMLStreamState),
    NewXMLStreamState = xml_stream:new(C2SPid),
    NewState = State#state{socket = ZlibSocket,
			   sock_mod = ejabberd_zlib,
			   xml_stream_state = NewXMLStreamState},
    case ejabberd_zlib:recv_data(ZlibSocket, "") of
	{ok, ZlibData} ->
	    {reply, ok, process_data(ZlibData, NewState)};
	{error, _Reason} ->
	    {stop, normal, ok, NewState}
    end;
handle_call(reset_stream, _From,
	    #state{xml_stream_state = XMLStreamState,
		   c2s_pid = C2SPid} = State) ->
    xml_stream:close(XMLStreamState),
    NewXMLStreamState = xml_stream:new(C2SPid),
    Reply = ok,
    {reply, Reply, State#state{xml_stream_state = NewXMLStreamState}};
handle_call(become_controller, _From, State) ->
    activate_socket(State),
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({change_shaper, Shaper}, State) ->
    NewShaperState = shaper:new(Shaper),
    {noreply, State#state{shaper_state = NewShaperState}};
handle_cast(close, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Tag, _TCPSocket, Data},
	    #state{socket = Socket,
		   sock_mod = SockMod} = State)
  when (Tag == tcp) or (Tag == ssl) ->
    case SockMod of
	tls ->
	    case tls:recv_data(Socket, Data) of
		{ok, TLSData} ->
		    {noreply, process_data(TLSData, State)};
		{error, _Reason} ->
		    {stop, normal, State}
	    end;
	ejabberd_zlib ->
	    case ejabberd_zlib:recv_data(Socket, Data) of
		{ok, ZlibData} ->
		    {noreply, process_data(ZlibData, State)};
		{error, _Reason} ->
		    {stop, normal, State}
	    end;
	_ ->
	    {noreply, process_data(Data, State)}
    end;
handle_info({Tag, _TCPSocket}, State)
  when (Tag == tcp_closed) or (Tag == ssl_closed) ->
    {stop, normal, State};
handle_info({Tag, _TCPSocket, Reason}, State)
  when (Tag == tcp_error) or (Tag == ssl_error) ->
    case Reason of
	timeout ->
	    {noreply, State};
	_ ->
	    {stop, normal, State}
    end;
handle_info({timeout, _Ref, activate}, State) ->
    activate_socket(State),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{xml_stream_state = XMLStreamState,
			  c2s_pid = C2SPid} = State) ->
    xml_stream:close(XMLStreamState),
    gen_fsm:send_event(C2SPid, closed),
    catch (State#state.sock_mod):close(State#state.socket),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

activate_socket(#state{socket = Socket,
		       sock_mod = SockMod}) ->
    case SockMod of
	gen_tcp ->
	    inet:setopts(Socket, [{active, once}]);
	_ ->
	    SockMod:setopts(Socket, [{active, once}])
    end.

process_data(Data,
	     #state{xml_stream_state = XMLStreamState,
		    shaper_state = ShaperState} = State) ->
    ?DEBUG("Received XML on stream = ~p", [binary_to_list(Data)]),
    XMLStreamState1 = xml_stream:parse(XMLStreamState, Data),
    {NewShaperState, Pause} = shaper:update(ShaperState, size(Data)),
    if
	Pause > 0 ->
	    erlang:start_timer(Pause, self(), activate);
	true ->
	    activate_socket(State)
    end,
    State#state{xml_stream_state = XMLStreamState1,
		shaper_state = NewShaperState}.

