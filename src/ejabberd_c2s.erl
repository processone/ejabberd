%%%----------------------------------------------------------------------
%%% File    : ejabberd_c2s.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_c2s).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_fsm).

%% External exports
-export([start/1, receiver/2, sender/1, send_text/2]).

%% gen_fsm callbacks
%-export([init/1, state_name/2, state_name/3, handle_event/3,
%	 handle_sync_event/4, handle_info/3, terminate/3]).
%
-export([init/1, wait_for_stream/2, wait_for_auth/2, session_established/2,
	 terminate/3]).

-record(state, {socket, sender, receiver, streamid,
		user = "", server = "localhost", resource = ""}).

-include("ejabberd.hrl").

-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-define(STREAM_HEADER,
	"<?xml version='1.0'?>"
	"<stream:stream xmlns='jabber:client' "
	"xmlns:stream='http://etherx.jabber.org/streams' "
	"id='~s' from='~s'>"
       ).

-define(STREAM_TRAILER, "</stream:stream>").

-define(INVALID_NS_ERR, "<stream:error>Invalid Namespace</stream:error>").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Socket) ->
    gen_fsm:start(ejabberd_c2s, [Socket], ?FSMOPTS).

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
    SenderPid = spawn(?MODULE, sender, [Socket]),
    ReceiverPid = spawn(?MODULE, receiver, [Socket, self()]),
    {ok, wait_for_stream, #state{socket = Socket,
				 receiver = ReceiverPid,
				 sender = SenderPid,
				 streamid = new_id()}}.

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
    Header = io_lib:format(?STREAM_HEADER,
			   [StateData#state.streamid, "localhost"]),
    send_text(StateData#state.sender, Header),
    case lists:keysearch("xmlns:stream", 1, Attrs) of
	{value, {"xmlns:stream", "http://etherx.jabber.org/streams"}} ->
	    % TODO
	    {next_state, wait_for_auth, StateData};
	_ ->
	    send_text(StateData#state.sender, ?INVALID_NS_ERR ?STREAM_TRAILER),
	    {stop, normal, StateData}
    end;

wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_auth({xmlstreamelement, El}, StateData) ->
    case is_auth_packet(El) of
	{auth, ID, {U, P, D, R}} ->
	    io:format("AUTH: ~p~n", [{U, P, D, R}]),
	    % TODO: digested password
	    case ejabberd_auth:check_password(U, P) of
		true ->
		    % TODO
		    {next_state, session_established,
		     StateData#state{user = U, resource = R}};
		_ ->
		    Err = jlib:make_error_reply(El, "404", "Unauthorized"),
		    send_element(StateData#state.sender, Err),
		    {next_state, wait_for_auth, StateData}
	    end;
	_ ->
	    {next_state, wait_for_auth, StateData}
    end;

wait_for_auth({xmlstreamend, Name}, StateData) ->
    % TODO
    {stop, normal, StateData};

wait_for_auth(closed, StateData) ->
    {stop, normal, StateData}.

session_established({xmlstreamelement, El}, StateData) ->
    {xmlelement, Name, Attrs, Els} = El,
    % TODO
    FromJID = {StateData#state.user, "localhost", StateData#state.resource},
    ToJID = jlib:string_to_jid(xml:get_attr_s("to", Attrs)),
    ejabberd_router:route(FromJID, ToJID, El),
    {next_state, session_established, StateData};

session_established(closed, StateData) ->
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
handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
    StateData#state.sender ! close,
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

sender(Socket) ->
    receive
	{text, Text} ->
	    gen_tcp:send(Socket,Text),
	    sender(Socket);
	close ->
	    gen_tcp:close(Socket),
	    ok
    end.

send_text(Pid, Text) ->
    Pid ! {text, Text}.

send_element(Pid, El) ->
    send_text(Pid, xml:element_to_string(El)).

new_id() ->
    lists:flatten(io_lib:format("~p", [random:uniform(65536*65536)])).


is_auth_packet({xmlelement, Name, Attrs, Els}) when Name == "iq" ->
    case xml:get_attr_s("type", Attrs) of
	"set" ->
	    case xml:remove_cdata(Els) of
		[{xmlelement, "query", Attrs2, Els2}] ->
		    case xml:get_attr_s("xmlns", Attrs2) of
			"jabber:iq:auth" ->
			    {auth,
			     xml:get_attr_s("id", Attrs),
			     get_auth_tags(Els2, "", "", "", "")};
			_ -> false
		    end;
		_ ->
		    false
	    end;
	true ->
	    false
    end;
is_auth_packet(_) ->
    false.

get_auth_tags([{xmlelement, Name, Attrs, Els}| L], U, P, D, R) ->
    CData = xml:get_cdata(Els),
    case Name of
	"username" ->
	    get_auth_tags(L, CData, P, D, R);
	"password" ->
	    get_auth_tags(L, U, CData, D, R);
	"digest" ->
	    get_auth_tags(L, U, P, CData, R);
	"resource" ->
	    get_auth_tags(L, U, P, D, CData);
	_ ->
	    get_auth_tags(L, U, P, D, R)
    end;
get_auth_tags([_ | L], U, P, D, R) ->
    get_auth_tags(L, U, P, D, R);
get_auth_tags([], U, P, D, R) ->
    {U, P, D, R}.




