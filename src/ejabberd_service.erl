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
-export([start/2, start_link/2, receiver/3, send_text/2, send_element/2]).

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
-include("jlib.hrl").

-record(state, {socket, receiver, streamid, sockmod,
		hosts, password}).

%-define(DBGFSM, true).

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

-define(INVALID_XML_ERR,
	xml:element_to_string(?SERR_XML_NOT_WELL_FORMED)).
-define(INVALID_NS_ERR,
	xml:element_to_string(?SERR_INVALID_NAMESPACE)).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(SockData, Opts) ->
    supervisor:start_child(ejabberd_service_sup, [SockData, Opts]).

start_link(SockData, Opts) ->
    gen_fsm:start_link(ejabberd_service, [SockData, Opts], ?FSMOPTS).

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
init([{SockMod, Socket}, Opts]) ->
    {Hosts, Password} =
	case lists:keysearch(hosts, 1, Opts) of
	    {value, {_, Hs, HOpts}} ->
		case lists:keysearch(password, 1, HOpts) of
		    {value, {_, P}} ->
			{Hs, P};
		    _ ->
			% TODO: generate error
			false
		end;
	    _ ->
		case lists:keysearch(host, 1, Opts) of
		    {value, {_, H, HOpts}} ->
			case lists:keysearch(password, 1, HOpts) of
			    {value, {_, P}} ->
				{[H], P};
			    _ ->
				% TODO: generate error
				false
			end;
		    _ ->
			% TODO: generate error
			false
		end
	end,
    ReceiverPid = spawn(?MODULE, receiver, [Socket, SockMod, self()]),
    {ok, wait_for_stream, #state{socket = Socket,
				 receiver = ReceiverPid,
				 streamid = new_id(),
				 sockmod = SockMod,
				 hosts = Hosts,
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

wait_for_stream({xmlstreamstart, _Name, Attrs}, StateData) ->
    % TODO
    case xml:get_attr_s("xmlns", Attrs) of
	"jabber:component:accept" ->
	    Header = io_lib:format(?STREAM_HEADER,
				   [StateData#state.streamid, ?MYNAME]),
	    send_text(StateData, Header),
	    {next_state, wait_for_handshake, StateData};
	_ ->
	    send_text(StateData, ?INVALID_HEADER_ERR),
	    {stop, normal, StateData}
    end;

wait_for_stream({xmlstreamerror, _}, StateData) ->
    Header = io_lib:format(?STREAM_HEADER,
			   ["none", ?MYNAME]),
    send_text(StateData,
	      Header ++ ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_handshake({xmlstreamelement, El}, StateData) ->
    {xmlelement, Name, _Attrs, Els} = El,
    case {Name, xml:get_cdata(Els)} of
	{"handshake", Digest} ->
	    case sha:sha(StateData#state.streamid ++
			 StateData#state.password) of
		Digest ->
		    send_text(StateData, "<handshake/>"),
		    lists:foreach(
		      fun(H) ->
			      ejabberd_router:register_local_route(H)
		      end, StateData#state.hosts),
		    {next_state, stream_established, StateData};
		_ ->
		    send_text(StateData, ?INVALID_HEADER_ERR),
		    {stop, normal, StateData}
	    end;
	_ ->
	    {next_state, wait_for_key, StateData}
    end;

wait_for_handshake({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};

wait_for_handshake({xmlstreamerror, _}, StateData) ->
    send_text(StateData, ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_handshake(closed, StateData) ->
    {stop, normal, StateData}.


stream_established({xmlstreamelement, El}, StateData) ->
    {xmlelement, Name, Attrs, _Els} = El,
    From = xml:get_attr_s("from", Attrs),
    FromJID1 = jlib:string_to_jid(From),
    FromJID = case FromJID1 of
		  #jid{lserver = Server} ->
		      case lists:member(Server, StateData#state.hosts) of
			  true -> FromJID1;
			  false -> error
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
	    Err = jlib:make_error_reply(El, ?ERR_BAD_REQUEST),
	    send_element(StateData, Err),
	    error
    end,
    {next_state, stream_established, StateData};

stream_established({xmlstreamend, _Name}, StateData) ->
    % TODO
    {stop, normal, StateData};

stream_established({xmlstreamerror, _}, StateData) ->
    send_text(StateData, ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
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
handle_event(_Event, StateName, StateData) ->
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
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData, Text),
    {next_state, StateName, StateData};
handle_info({send_element, El}, StateName, StateData) ->
    send_element(StateData, El),
    {next_state, StateName, StateData};
handle_info({route, From, To, Packet}, StateName, StateData) ->
    {xmlelement, Name, Attrs, Els} = Packet,
    Attrs2 = jlib:replace_from_to_attrs(jlib:jid_to_string(From),
					jlib:jid_to_string(To),
					Attrs),
    Text = xml:element_to_string({xmlelement, Name, Attrs2, Els}),
    send_text(StateData, Text),
    {next_state, StateName, StateData}.


%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, StateName, StateData) ->
    case StateName of
	stream_established ->
	    lists:foreach(
	      fun(H) ->
		      ejabberd_router:unregister_local_route(H)
	      end, StateData#state.hosts);
	_ ->
	    ok
    end,
    (StateData#state.sockmod):close(StateData#state.socket),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

receiver(Socket, SockMod, C2SPid) ->
    XMLStreamPid = xml_stream:start(C2SPid),
    receiver(Socket, SockMod, C2SPid, XMLStreamPid).

receiver(Socket, SockMod, C2SPid, XMLStreamPid) ->
    case SockMod:recv(Socket, 0) of
        {ok, Text} ->
	    xml_stream:send_text(XMLStreamPid, Text),
	    receiver(Socket, SockMod, C2SPid, XMLStreamPid);
        {error, _Reason} ->
	    exit(XMLStreamPid, closed),
	    gen_fsm:send_event(C2SPid, closed),
	    ok
    end.

send_text(StateData, Text) ->
    (StateData#state.sockmod):send(StateData#state.socket,Text).

send_element(StateData, El) ->
    send_text(StateData, xml:element_to_string(El)).


new_id() ->
    randoms:get_string().

