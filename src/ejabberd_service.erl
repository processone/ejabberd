%%%----------------------------------------------------------------------
%%% File    : ejabberd_service.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  6 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_service).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_fsm).

%% External exports
-export([start/3, receiver/2, send_text/2, send_element/2]).

%% gen_fsm callbacks
-export([init/1,
	 wait_for_stream/2,
	 wait_for_handshake/2,
	 stream_established/2,
	 handle_event/3,
	 handle_sync_event/4,
	 code_change/4,
	 handle_info/3,
	 terminate/3]).

-include("ejabberd.hrl").

-record(state, {socket, receiver, streamid,
		host, password}).

-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-define(STREAM_HEADER,
	"<?xml version='1.0'?>"
	"<stream:stream "
	"xmlns:stream='http://etherx.jabber.org/streams' "
	"xmlns='jabber:component:accept' "
	"id='~s' from='~s'>"
       ).

-define(STREAM_TRAILER, "</stream:stream>").

-define(INVALID_HEADER_ERR,
	"<stream:stream>"
	"<stream:error>Invalid Stream Header</stream:error>"
	"</stream:stream>"
       ).

-define(INVALID_HANDSHAKE_ERR,
	"<stream:error>Invalid Handshake</stream:error>"
	"</stream:stream>"
       ).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Socket, Host, Password) ->
    gen_fsm:start(ejabberd_service, [Socket, Host, Password], ?FSMOPTS).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}                   
%%----------------------------------------------------------------------
init([Socket, Host, Password]) ->
    ReceiverPid = spawn(?MODULE, receiver, [Socket, self()]),
    {ok, wait_for_stream, #state{socket = Socket,
				 receiver = ReceiverPid,
				 streamid = new_id(),
				 host = Host,
				 password = Password
				 }}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
%state_name(Event, StateData) ->
%    {next_state, state_name, StateData}.

wait_for_stream({xmlstreamstart, Name, Attrs}, StateData) ->
    % TODO
    case xml:get_attr_s("xmlns", Attrs) of
	"jabber:component:accept" ->
	    Header = io_lib:format(?STREAM_HEADER,
				   [StateData#state.streamid, ?MYNAME]),
	    send_text(StateData#state.socket, Header),
	    {next_state, wait_for_handshake, StateData};
	_ ->
	    send_text(StateData#state.socket, ?INVALID_HEADER_ERR),
	    {stop, normal, StateData}
    end;

wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_handshake({xmlstreamelement, El}, StateData) ->
    {xmlelement, Name, Attrs, Els} = El,
    case {Name, xml:get_cdata(Els)} of
	{"handshake", Digest} ->
	    case sha:sha(StateData#state.streamid ++
			 StateData#state.password) of
		Digest ->
		    send_text(StateData#state.socket, "<handshake/>"),
		    ejabberd_router:register_local_route(StateData#state.host),
		    {next_state, stream_established, StateData};
		_ ->
		    send_text(StateData#state.socket, ?INVALID_HEADER_ERR),
		    {stop, normal, StateData}
	    end;
	_ ->
	    {next_state, wait_for_key, StateData}
    end;

wait_for_handshake({xmlstreamend, Name}, StateData) ->
    {stop, normal, StateData};

wait_for_handshake(closed, StateData) ->
    {stop, normal, StateData}.


stream_established({xmlstreamelement, El}, StateData) ->
    {xmlelement, Name, Attrs, Els} = El,
    From = xml:get_attr_s("from", Attrs),
    FromJID1 = jlib:string_to_jid(From),
    FromJID = case FromJID1 of
		  {Node, Server, Resource} ->
		      if Server == StateData#state.host -> FromJID1;
			 true -> error
		      end;
		  _ -> error
	      end,
    To = xml:get_attr_s("to", Attrs),
    ToJID = case To of
		"" -> error;
		_ -> jlib:string_to_jid(To)
	    end,
    if ((Name == "iq") or
	(Name == "message") or
	(Name == "presence")) and
       (ToJID /= error) and (FromJID /= error) ->
	    ejabberd_router:route(FromJID, ToJID, El);
       true ->
	    error
    end,
    {next_state, stream_established, StateData};

stream_established({xmlstreamend, Name}, StateData) ->
    % TODO
    {stop, normal, StateData};

stream_established(closed, StateData) ->
    % TODO
    {stop, normal, StateData}.



%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
%state_name(Event, From, StateData) ->
%    Reply = ok,
%    {reply, Reply, state_name, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData#state.socket, Text),
    {next_state, StateName, StateData};
handle_info({send_element, El}, StateName, StateData) ->
    send_element(StateData#state.socket, El),
    {next_state, StateName, StateData};
handle_info({route, From, To, Packet}, StateName, StateData) ->
    {xmlelement, Name, Attrs, Els} = Packet,
    Attrs2 = jlib:replace_from_to_attrs(jlib:jid_to_string(From),
					jlib:jid_to_string(To),
					Attrs),
    Text = xml:element_to_string({xmlelement, Name, Attrs2, Els}),
    send_text(StateData#state.socket, Text),
    {next_state, StateName, StateData}.


%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
    case StateName of
	stream_established ->
	    ejabberd_router:unregister_local_route(StateData#state.host);
	_ ->
	    ok
    end,
    gen_tcp:close(StateData#state.socket),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

receiver(Socket, C2SPid) ->
    XMLStreamPid = xml_stream:start(C2SPid),
    receiver(Socket, C2SPid, XMLStreamPid).

receiver(Socket, C2SPid, XMLStreamPid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Text} ->
	    xml_stream:send_text(XMLStreamPid, Text),
	    receiver(Socket, C2SPid, XMLStreamPid);
        {error, closed} ->
	    exit(XMLStreamPid, closed),
	    gen_fsm:send_event(C2SPid, closed),
	    ok
    end.

send_text(Socket, Text) ->
    gen_tcp:send(Socket,Text).

send_element(Socket, El) ->
    send_text(Socket, xml:element_to_string(El)).


new_id() ->
    randoms:get_string().

