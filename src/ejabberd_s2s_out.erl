%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s_out.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  6 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_s2s_out).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_fsm).

%% External exports
-export([start_link/3, send_text/2, send_element/2]).

%% gen_fsm callbacks
-export([init/1,
	 open_socket/2,
	 wait_for_stream/2,
	 wait_for_validation/2,
	 stream_established/2,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 code_change/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, {socket, receiver, streamid,
		myname, server, xmlpid, queue,
		new = false, verify = false}).

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
	"xmlns='jabber:server' "
	"xmlns:db='jabber:server:dialback' "
	"to='~s'>"
       ).

-define(STREAM_TRAILER, "</stream:stream>").

-define(INVALID_NAMESPACE_ERR,
	xml:element_to_string(?SERR_INVALID_NAMESPACE)).

-define(HOST_UNKNOWN_ERR,
	xml:element_to_string(?SERR_HOST_UNKNOWN)).

-define(INVALID_XML_ERR,
	xml:element_to_string(?SERR_XML_NOT_WELL_FORMED)).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(From, Host, Type) ->
    {ok, Pid} = gen_fsm:start_link(ejabberd_s2s_out, [From, Host, Type],
				   ?FSMOPTS),
    Pid.

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
init([From, Server, Type]) ->
    ?INFO_MSG("started: ~p", [{From, Server, Type}]),
    gen_fsm:send_event(self(), init),
    {New, Verify} = case Type of
			{new, Key} ->
			    {Key, false};
			{verify, Pid, Key} ->
			    {false, {Pid, Key}}
		    end,
    {ok, open_socket, #state{queue = queue:new(),
			     myname = From,
			     server = Server,
			     new = New,
			     verify = Verify},
     ?S2STIMEOUT}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
open_socket(init, StateData) ->
    {Addr, Port} = get_addr_port(StateData#state.server),
    ?DEBUG("s2s_out: connecting to ~s:~p~n", [Addr, Port]),
    case gen_tcp:connect(Addr,
			 Port,
			 [binary, {packet, 0}]) of
	{ok, Socket} ->
	    XMLStreamPid = xml_stream:start(self()),
	    send_text(Socket, io_lib:format(?STREAM_HEADER,
					    [StateData#state.server])),
	    {next_state, wait_for_stream,
	     StateData#state{socket = Socket,
			     xmlpid = XMLStreamPid,
			     streamid = new_id()},
	     ?S2STIMEOUT};
	{error, Reason} ->
	    ?DEBUG("s2s_out: connect return ~p~n", [Reason]),
	    Error = case Reason of
			timeout -> ?ERR_REMOTE_SERVER_TIMEOUT;
			_ -> ?ERR_REMOTE_SERVER_NOT_FOUND
		    end,
	    bounce_messages(Error),
	    {stop, normal, StateData}
    end.


wait_for_stream({xmlstreamstart, Name, Attrs}, StateData) ->
    % TODO
    case {xml:get_attr_s("xmlns", Attrs), xml:get_attr_s("xmlns:db", Attrs)} of
	{"jabber:server", "jabber:server:dialback"} ->
	    Server = StateData#state.server,
	    New = case StateData#state.new of
		      false ->
			  case
			      ejabberd_s2s:try_register(
				{StateData#state.myname, Server}) of
			      {key, Key} ->
				  Key;
			      false ->
				  false
			  end;
		      Key ->
			  Key
		  end,
	    case New of
		false ->
		    ok;
		Key1 ->
		    send_element(StateData#state.socket,
				 {xmlelement,
				  "db:result",
				  [{"from", StateData#state.myname},
				   {"to", Server}],
				  [{xmlcdata, Key1}]})
	    end,
	    case StateData#state.verify of
		false ->
		    ok;
		{Pid, Key2} ->
		    send_element(StateData#state.socket,
				 {xmlelement,
				  "db:verify",
				  [{"from", StateData#state.myname},
				   {"to", StateData#state.server}],
				  [{xmlcdata, Key2}]})
	    end,
	    {next_state, wait_for_validation,
	     StateData#state{new = New}, ?S2STIMEOUT};
	_ ->
	    send_text(StateData#state.socket, ?INVALID_NAMESPACE_ERR),
	    {stop, normal, StateData}
    end;

wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_text(StateData#state.socket,
	      ?STREAM_HEADER ++ ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_stream(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.



wait_for_validation({xmlstreamelement, El}, StateData) ->
    case is_verify_res(El) of
	{result, To, From, Id, Type} ->
	    ?INFO_MSG("recv result: ~p", [{From, To, Id, Type}]),
	    case Type of
		"valid" ->
		    send_queue(StateData#state.socket, StateData#state.queue),
		    {next_state, stream_established, StateData, ?S2STIMEOUT};
		_ ->
		    % TODO: bounce packets
		    {stop, normal, StateData}
	    end;
	{verify, To, From, Id, Type} ->
	    ?INFO_MSG("recv verify: ~p", [{From, To, Id, Type}]),
	    case StateData#state.verify of
		false ->
		    {next_state, wait_for_validation, StateData, ?S2STIMEOUT};
		{Pid, Key} ->
		    case Type of
			"valid" ->
			    gen_fsm:send_event(Pid, valid);
			_ ->
			    gen_fsm:send_event(Pid, invalid)
		    end,
		    {stop, normal, StateData}
	    end;
	_ ->
	    {next_state, wait_for_validation, StateData, ?S2STIMEOUT}
    end;

wait_for_validation({xmlstreamend, Name}, StateData) ->
    {stop, normal, StateData};

wait_for_validation({xmlstreamerror, _}, StateData) ->
    send_text(StateData#state.socket,
	      ?STREAM_HEADER ++ ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_validation(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_validation(closed, StateData) ->
    {stop, normal, StateData}.


stream_established({xmlstreamelement, El}, StateData) ->
    ?INFO_MSG("stream established", []),
    case is_verify_res(El) of
	{verify, VTo, VFrom, VId, VType} ->
	    ?INFO_MSG("recv verify: ~p", [{VFrom, VTo, VId, VType}]),
	    case StateData#state.verify of
		{VPid, VKey} ->
		    case VType of
			"valid" ->
			    gen_fsm:send_event(VPid, valid);
			_ ->
			    gen_fsm:send_event(VPid, invalid)
		    end;
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end,
    %{xmlelement, Name, Attrs, Els} = El,
    %From = xml:get_attr_s("from", Attrs),
    %FromJID1 = jlib:string_to_jid(From),
    %FromJID = case FromJID1 of
    %    	  {Node, Server, Resource} ->
    %    	      if Server == StateData#state.server -> FromJID1;
    %    		 true -> error
    %    	      end;
    %    	  _ -> error
    %          end,
    %To = xml:get_attr_s("to", Attrs),
    %ToJID = case To of
    %    	"" -> error;
    %    	_ -> jlib:string_to_jid(To)
    %        end,
    %if ((Name == "iq") or (Name == "message") or (Name == "presence")) and
    %   (ToJID /= error) and (FromJID /= error) ->
    %        ejabberd_router:route(FromJID, ToJID, El);
    %   true ->
    %        error
    %end,
    {next_state, stream_established, StateData, ?S2STIMEOUT};

stream_established({xmlstreamend, Name}, StateData) ->
    {stop, normal, StateData};

stream_established({xmlstreamerror, _}, StateData) ->
    send_text(StateData#state.socket,
	      ?STREAM_HEADER ++ ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

stream_established(timeout, StateData) ->
    {stop, normal, StateData};

stream_established(closed, StateData) ->
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
    case StateName of
	stream_established ->
	    send_element(StateData#state.socket, El),
	    {next_state, StateName, StateData};
	_ ->
	    Q = queue:in(El, StateData#state.queue),
	    {next_state, StateName, StateData#state{queue = Q}}
    end;
handle_info({tcp, Socket, Data}, StateName, StateData) ->
    xml_stream:send_text(StateData#state.xmlpid, Data),
    {next_state, StateName, StateData};
handle_info({tcp_closed, Socket}, StateName, StateData) ->
    gen_fsm:send_event(self(), closed),
    {next_state, StateName, StateData};
handle_info({tcp_error, Socket, Reason}, StateName, StateData) ->
    gen_fsm:send_event(self(), closed),
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
    ?INFO_MSG("terminated: ~p", [Reason]),
    case StateData#state.new of
	false ->
	    ok;
	Key ->
	    ejabberd_s2s ! {closed_conection, {StateData#state.myname,
					       StateData#state.server}}
    end,
    case StateData#state.socket of
	undefined ->
	    ok;
	Socket ->
	    gen_tcp:close(Socket),
	    exit(StateData#state.xmlpid, closed)
    end,
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

send_text(Socket, Text) ->
    gen_tcp:send(Socket,Text).

send_element(Socket, El) ->
    send_text(Socket, xml:element_to_string(El)).

send_queue(Socket, Q) ->
    case queue:out(Q) of
	{{value, El}, Q1} ->
	    send_element(Socket, El),
	    send_queue(Socket, Q1);
	{empty, Q1} ->
	    ok
    end.

new_id() ->
    randoms:get_string().

bounce_messages(Error) ->
    receive
	{send_element, El} ->
	    {xmlelement, Name, Attrs, SubTags} = El,
	    case xml:get_attr_s("type", Attrs) of
	        "error" ->
	            ok;
	        _ ->
	            Err = jlib:make_error_reply(El, Error),
		    From = jlib:string_to_jid(xml:get_attr_s("from", Attrs)),
		    To = jlib:string_to_jid(xml:get_attr_s("to", Attrs)),
		    ejabberd_router ! {route, To, From, Err}
	    end,
	    bounce_messages(Error)
    after 0 ->
	    ok
    end.

%is_key_packet({xmlelement, Name, Attrs, Els}) when Name == "db:result" ->
%    {key,
%     xml:get_attr_s("to", Attrs),
%     xml:get_attr_s("from", Attrs),
%     xml:get_attr_s("id", Attrs),
%     xml:get_cdata(Els)};
%is_key_packet({xmlelement, Name, Attrs, Els}) when Name == "db:verify" ->
%    {verify,
%     xml:get_attr_s("to", Attrs),
%     xml:get_attr_s("from", Attrs),
%     xml:get_attr_s("id", Attrs),
%     xml:get_cdata(Els)};
%is_key_packet(_) ->
%    false.

is_verify_res({xmlelement, Name, Attrs, Els}) when Name == "db:result" ->
    {result,
     xml:get_attr_s("to", Attrs),
     xml:get_attr_s("from", Attrs),
     xml:get_attr_s("id", Attrs),
     xml:get_attr_s("type", Attrs)};
is_verify_res({xmlelement, Name, Attrs, Els}) when Name == "db:verify" ->
    {verify,
     xml:get_attr_s("to", Attrs),
     xml:get_attr_s("from", Attrs),
     xml:get_attr_s("id", Attrs),
     xml:get_attr_s("type", Attrs)};
is_verify_res(_) ->
    false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SRV support

-include_lib("kernel/include/inet.hrl").

get_addr_port(Server) ->
    case inet_res:getbyname("_jabber._tcp." ++ Server, srv) of
	{error, Reason} ->
	    ?DEBUG("srv lookup of '~s' failed: ~p~n", [Server, Reason]),
	    {Server, ejabberd_config:get_local_option(outgoing_s2s_port)};
	{ok, HEnt} ->
	    ?DEBUG("srv lookup of '~s': ~p~n",
		   [Server, HEnt#hostent.h_addr_list]),
	    case HEnt#hostent.h_addr_list of
		[] ->
		    {Server,
		     ejabberd_config:get_local_option(outgoing_s2s_port)};
		[{_, _, Port, Host} | _] ->
		    {Host, Port}
	    end
    end.



