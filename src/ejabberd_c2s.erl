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
-export([start/1, receiver/2, sender/1, send_text/2, send_element/2]).

%% gen_fsm callbacks
%-export([init/1, state_name/2, state_name/3, handle_event/3,
%	 handle_sync_event/4, handle_info/3, terminate/3]).
%
-export([init/1, wait_for_stream/2, wait_for_auth/2, session_established/2,
	 handle_info/3,
	 terminate/3]).

-include("ejabberd.hrl").

-define(SETS, gb_sets).

-record(state, {socket, sender, receiver, streamid,
		user = "", server = ?MYNAME, resource = "",
		pres_t = ?SETS:new(),
		pres_f = ?SETS:new(),
		pres_a = ?SETS:new(),
		pres_i = ?SETS:new(),
		pres_last, pres_pri,
		pres_timestamp,
		pres_invis = false}).

%-define(DBGFSM, true).

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
			   [StateData#state.streamid, ?MYNAME]),
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
		    ejabberd_sm:open_session(U, R),
		    Res = jlib:make_result_iq_reply(El),
		    send_element(StateData#state.sender, Res),
		    {Fs, Ts} = mod_roster:get_subscription_lists(U),
		    {next_state, session_established,
		     StateData#state{user = U,
				     resource = R,
				     pres_f = ?SETS:from_list(Fs),
				     pres_t = ?SETS:from_list(Ts)}};
		_ ->
		    Err = jlib:make_error_reply(El, "401", "Unauthorized"),
		    send_element(StateData#state.sender, Err),
		    {next_state, wait_for_auth, StateData}
	    end;
	_ ->
	    case jlib:iq_query_info(El) of
		{iq, ID, Type, "jabber:iq:register", SubEl} ->
		    ResIQ = mod_register:process_iq(
			      {"", "", ""}, {"", ?MYNAME, ""},
			      {iq, ID, Type, "jabber:iq:register", SubEl}),
		    Res1 = jlib:replace_from_to({"", ?MYNAME, ""},
						{"", "", ""},
						jlib:iq_to_xml(ResIQ)),
		    Res = jlib:remove_attr("to", Res1),
		    send_element(StateData#state.sender, Res),
		    {next_state, wait_for_auth, StateData};
		_ ->
		    {next_state, wait_for_auth, StateData}
	    end
    end;

wait_for_auth({xmlstreamend, Name}, StateData) ->
    % TODO
    {stop, normal, StateData};

wait_for_auth(closed, StateData) ->
    {stop, normal, StateData}.

session_established({xmlstreamelement, El}, StateData) ->
    {xmlelement, Name, Attrs, Els} = El,
    Server = StateData#state.server,
    FromJID = {StateData#state.user,
	       Server,
	       StateData#state.resource},
    To = xml:get_attr_s("to", Attrs),
    ToJID = case To of
		"" ->
		    {"", Server, ""};
		_ ->
		    jlib:string_to_jid(To)
	    end,
    NewState =
	case ToJID of
	    error ->
		% TODO
		StateData;
	    _ ->
		case Name of
		    "presence" ->
			case ToJID of
			    {"", Server, ""} ->
				?DEBUG("presence_update(~p,~n\t~p,~n\t~p)",
				       [FromJID, El, StateData]),
				presence_update(FromJID, El, StateData);
			    _ ->
				StateData
			end;
		    _ ->
			ejabberd_router:route(FromJID, ToJID, El),
			StateData
		end
	end,
    {next_state, session_established, NewState};

session_established({xmlstreamend, Name}, StateData) ->
    {stop, normal, StateData};

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
handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData#state.sender, Text),
    {next_state, StateName, StateData};
handle_info({route, From, To, Packet}, StateName, StateData) ->
    {xmlelement, Name, Attrs, Els} = Packet,
    % TODO
    {Pass, NewAttrs, NewState} =
	case Name of
	    "presence" ->
		case xml:get_attr_s("type", Attrs) of
		    "probe" ->
			process_presence_probe(From, To, StateData),
			{false, Attrs, StateData};
		    "error" ->
			case ?SETS:is_element(From, StateData#state.pres_a) of
			    true ->
				A = ?SETS:del_element(From,
						      StateData#state.pres_a),
				{true, Attrs, StateData#state{pres_a = A}};
			    _ ->
				{true, Attrs, StateData}
			end;
		    "invisible" ->
			Attrs1 = lists:keydelete("type", 1, Attrs),
			{true, [{"type", "unavailable"} | Attrs1], StateData};
		    "subscribe" ->
			% TODO
			{true, Attrs, StateData};
		    "unsubscribe" ->
			% TODO
			{true, Attrs, StateData};
		    _ ->
			{true, Attrs, StateData}
		end;
	    _ ->
		{true, Attrs, StateData}
	end,
    if Pass ->
	    Attrs2 = jlib:replace_from_to_attrs(jlib:jid_to_string(From),
						jlib:jid_to_string(To),
						NewAttrs),
	    Text = xml:element_to_string({xmlelement, Name, Attrs2, Els}),
	    send_text(StateData#state.sender, Text);
       true ->
	    ok
    end,
    {next_state, StateName, NewState}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
    case StateData#state.user of
	"" ->
	    ok;
	_ ->
	    ejabberd_sm:close_session(StateData#state.user,
				      StateData#state.resource)
    end,
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
	{send_text, Text} ->
	    gen_tcp:send(Socket,Text),
	    sender(Socket);
	close ->
	    gen_tcp:close(Socket),
	    ok
    end.

send_text(Pid, Text) ->
    Pid ! {send_text, Text}.

send_element(Pid, El) ->
    send_text(Pid, xml:element_to_string(El)).

new_id() ->
    randoms:get_string().


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


process_presence_probe(From, To, StateData) ->
    case StateData#state.pres_last of
	undefined ->
	    ok;
	_ ->
	    Cond1 = (not StateData#state.pres_invis)
		and ?SETS:is_element(From, StateData#state.pres_f)
		and (not ?SETS:is_element(From, StateData#state.pres_i)),
	    Cond2 = StateData#state.pres_invis
		and ?SETS:is_element(From, StateData#state.pres_f)
		and ?SETS:is_element(From, StateData#state.pres_a),
	    if
		Cond1 ->
		    ejabberd_router:route(To, From,
					  StateData#state.pres_last);
		Cond2 ->
		    ejabberd_router:route(To, From,
					  {xmlelement, "presence",
					   [],
					   []});
		true ->
		    ok
	    end
    end.

presence_update(From, Packet, StateData) ->
    {xmlelement, Name, Attrs, Els} = Packet,
    case xml:get_attr_s("type", Attrs) of
	"unavailable" ->
	    presence_broadcast(From, StateData#state.pres_a, Packet),
	    presence_broadcast(From, StateData#state.pres_i, Packet),
	    StateData#state{pres_last = undefined,
			    pres_a = ?SETS:new(),
			    pres_i = ?SETS:new(),
			    pres_invis = false};
	"invisible" ->
	    NewState =
		if
		    not StateData#state.pres_invis ->
			presence_broadcast(From, StateData#state.pres_a,
					   Packet),
			presence_broadcast(From, StateData#state.pres_i,
					   Packet),
			S1 = StateData#state{pres_last = undefined,
					     pres_a = ?SETS:new(),
					     pres_i = ?SETS:new(),
					     pres_invis = false},
			presence_broadcast_first(From, S1, Packet);
		    true ->
			StateData
		end,
	    
	    StateData;
	"error" ->
	    StateData;
	_ ->
	    FromUnavail = (StateData#state.pres_last == undefined) or
		StateData#state.pres_invis,
	    ?DEBUG("from unavail = ~p~n", [FromUnavail]),
	    NewState =
		if
		    FromUnavail ->
			% TODO: watching ourself

			presence_broadcast_first(From, StateData, Packet);
		    true ->
			presence_broadcast_to_trusted(From,
						      StateData#state.pres_f,
						      StateData#state.pres_a,
						      Packet),
			StateData
		end,

	    NewState#state{pres_last = Packet,
			   pres_invis = false
			  }
    end.


presence_broadcast(From, JIDSet, Packet) ->
    lists:foreach(fun(JID) ->
			  ejabberd_router:route(From, JID, Packet)
		  end, ?SETS:to_list(JIDSet)).

presence_broadcast_to_trusted(From, T, A, Packet) ->
    lists:foreach(fun(JID) ->
			  case ?SETS:is_element(JID, T) of
			      true ->
				  ejabberd_router:route(From, JID, Packet);
			      _ ->
				  ok
			  end
		  end, ?SETS:to_list(A)).


presence_broadcast_first(From, StateData, Packet) ->
    {U, S, _} = From,
    ?SETS:fold(fun(JID, X) ->
		       ejabberd_router:route({U, S, ""}, JID,
					     {xmlelement, "presence",
					      [{"type", "probe"}],
					      []}),
		       X
	       end,
	       [],
	       StateData#state.pres_t),
    if
	StateData#state.pres_invis ->
	    StateData;
	true ->
	    As = ?SETS:fold(fun(JID, A) ->
				    ejabberd_router:route(From, JID, Packet),
				    ?SETS:add_element(JID, A)
			    end,
			    StateData#state.pres_a,
			    StateData#state.pres_f),
	    StateData#state{pres_a = As}
    end.


    



