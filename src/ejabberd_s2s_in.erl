%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s_in.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  6 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_s2s_in).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_fsm).

%% External exports
-export([start/1, receiver/2, send_text/2, send_element/2]).

%% gen_fsm callbacks
-export([init/1,
	 wait_for_stream/2,
	 wait_for_key/2,
	 wait_for_verification/2,
	 stream_established/2,
	 handle_info/3,
	 terminate/3]).

-include("ejabberd.hrl").

-record(state, {socket, receiver, streamid,
		myname, server, queue}).

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
	"xmlns:db='jabber:server:dialback'>"
       ).

-define(STREAM_TRAILER, "</stream:stream>").

-define(INVALID_HEADER_ERR,
	"<stream:stream>"
	"<stream:error>Invalid Stream Header</stream:error>"
	"</stream:stream>"
       ).

-define(INVALID_DOMAIN_ERR,
	"<stream:stream>"
	"<stream:error>Invalid Destination</stream:error>"
	"</stream:stream>"
       ).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Socket) ->
    gen_fsm:start(ejabberd_s2s_in, [Socket], ?FSMOPTS).

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
init([Socket]) ->
    ReceiverPid = spawn(?MODULE, receiver, [Socket, self()]),
    {ok, wait_for_stream, #state{socket = Socket,
				 receiver = ReceiverPid,
				 streamid = new_id(),
				 queue = queue:new()}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
state_name(Event, StateData) ->
    {next_state, state_name, StateData}.

wait_for_stream({xmlstreamstart, Name, Attrs}, StateData) ->
    % TODO
    case {xml:get_attr_s("xmlns", Attrs), xml:get_attr_s("xmlns:db", Attrs)} of
	{"jabber:server", "jabber:server:dialback"} ->
	    Me = case xml:get_attr_s("to", Attrs) of
		     "" -> ?MYNAME;
		     Dom -> Dom
		 end,
	    case lists:member(Me, ejabberd_router:dirty_get_all_domains()) of
		true ->
		    send_text(StateData#state.socket, ?STREAM_HEADER),
		    {next_state, wait_for_key, StateData#state{myname = Me}};
		_ ->
		    send_text(StateData#state.socket, ?INVALID_DOMAIN_ERR),
		    {stop, normal, StateData}
	    end;

	_ ->
	    send_text(StateData#state.socket, ?INVALID_HEADER_ERR),
	    {stop, normal, StateData}
    end;

wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_key({xmlstreamelement, El}, StateData) ->
    case is_key_packet(El) of
	{key, To, From, Id, Key} ->
	    io:format("GET KEY: ~p~n", [{To, From, Id, Key}]),
	    ejabberd_s2s_out:start(StateData#state.myname, From,
				   {verify, self(), Key}),
	    {next_state,
	     wait_for_verification,
	     StateData#state{server = From}};
	{verify, To, From, Id, Key} ->
	    io:format("VERIFY KEY: ~p~n", [{To, From, Id, Key}]),
	    Key1 = ejabberd_s2s:get_key({StateData#state.myname, From}),
	    Type = if Key == Key1 -> "valid";
		      true -> "invalid"
		   end,
	    send_element(StateData#state.socket,
			 {xmlelement,
			  "db:verify",
			  [{"from", StateData#state.myname},
			   {"to", From},
			   {"id", Id},
			   {"type", Type}],
			  []}),
	    {next_state, wait_for_key, StateData};
	_ ->
	    {next_state, wait_for_key, StateData}
    end;

wait_for_key({xmlstreamend, Name}, StateData) ->
    % TODO
    {stop, normal, StateData};

wait_for_key(closed, StateData) ->
    {stop, normal, StateData}.

wait_for_verification(valid, StateData) ->
    send_element(StateData#state.socket,
		 {xmlelement,
		  "db:result",
		  [{"from", StateData#state.myname},
		   {"to", StateData#state.server},
		   {"type", "valid"}],
		  []}),
    send_queue(StateData#state.socket, StateData#state.queue),
    {next_state, stream_established, StateData};

wait_for_verification(invalid, StateData) ->
    send_element(StateData#state.socket,
		 {xmlelement,
		  "db:result",
		  [{"from", StateData#state.myname},
		   {"to", StateData#state.server},
		   {"type", "invalid"}],
		  []}),
    {stop, normal, StateData};

wait_for_verification({xmlstreamelement, El}, StateData) ->
    case is_key_packet(El) of
	{verify, To, From, Id, Key} ->
	    io:format("VERIFY KEY: ~p~n", [{To, From, Id, Key}]),
	    Key1 = ejabberd_s2s:get_key({StateData#state.myname, From}),
	    Type = if Key == Key1 -> "valid";
		      true -> "invalid"
		   end,
	    send_element(StateData#state.socket,
			 {xmlelement,
			  "db:verify",
			  [{"from", StateData#state.myname},
			   {"to", From},
			   {"id", Id},
			   {"type", Type}],
			  []}),
	    {next_state, wait_for_verification, StateData};
	_ ->
	    {next_state, wait_for_verification, StateData}
    end;

wait_for_verification({xmlstreamend, Name}, StateData) ->
    % TODO
    {stop, normal, StateData};

wait_for_verification(closed, StateData) ->
    {stop, normal, StateData}.

stream_established({xmlstreamelement, El}, StateData) ->
    {xmlelement, Name, Attrs, Els} = El,
    case Name of
	"db:verify" ->
	    case is_key_packet(El) of
		{verify, To, From, Id, Key} ->
		    io:format("VERIFY KEY: ~p~n", [{To, From, Id, Key}]),
		    Key1 = ejabberd_s2s:get_key({StateData#state.myname,
						 From}),
		    Type = if Key == Key1 -> "valid";
			      true -> "invalid"
			   end,
		    send_element(StateData#state.socket,
				 {xmlelement,
				  "db:verify",
				  [{"from", StateData#state.myname},
				   {"to", From},
				   {"id", Id},
				   {"type", Type}],
				  []});
		_ ->
		    ok
	    end;
	_ ->
	    From = xml:get_attr_s("from", Attrs),
	    FromJID1 = jlib:string_to_jid(From),
	    FromJID = case FromJID1 of
			  {Node, Server, Resource} ->
			      if Server == StateData#state.server -> FromJID1;
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
	    end
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
state_name(Event, From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.

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
    end.


%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
%    case StateData#state.user of
%	"" ->
%	    ok;
%	_ ->
%	    %ejabberd_sm:close_session(StateData#state.user,
%	    %    		      StateData#state.resource)
%    end,
    %ejabberd_s2s ! {closed_conection, StateData#state.server},
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
        {error, Reason} ->
	    exit(XMLStreamPid, closed),
	    gen_fsm:send_event(C2SPid, closed),
	    ok
    end.

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


is_key_packet({xmlelement, Name, Attrs, Els}) when Name == "db:result" ->
    {key,
     xml:get_attr_s("to", Attrs),
     xml:get_attr_s("from", Attrs),
     xml:get_attr_s("id", Attrs),
     xml:get_cdata(Els)};
is_key_packet({xmlelement, Name, Attrs, Els}) when Name == "db:verify" ->
    {verify,
     xml:get_attr_s("to", Attrs),
     xml:get_attr_s("from", Attrs),
     xml:get_attr_s("id", Attrs),
     xml:get_cdata(Els)};
is_key_packet(_) ->
    false.



