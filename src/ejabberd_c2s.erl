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
-export([start/2, receiver/3, sender/2, send_text/2, send_element/2]).

%% gen_fsm callbacks
-export([init/1, wait_for_stream/2, wait_for_auth/2, session_established/2,
	 handle_event/3,
	 handle_sync_event/4,
	 code_change/4,
	 handle_info/3,
	 terminate/3]).

-include("ejabberd.hrl").
-include("namespaces.hrl").

-define(SETS, gb_sets).

-record(state, {socket, sender, receiver, streamid,
		access,
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
start(SockData, Opts) ->
    gen_fsm:start(ejabberd_c2s, [SockData, Opts], ?FSMOPTS).

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
    SenderPid = spawn(?MODULE, sender, [Socket, SockMod]),
    ReceiverPid = spawn(?MODULE, receiver, [Socket, SockMod, self()]),
    Access = case lists:keysearch(access, 1, Opts) of
		 {value, {_, A}} ->
		     A;
		 _ ->
		     all
	     end,
    {ok, wait_for_stream, #state{socket   = Socket,
				 receiver = ReceiverPid,
				 sender   = SenderPid,
				 streamid = new_id(),
				 access   = Access}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------

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
	{auth, ID, {U, P, D, ""}} ->
	    Err = jlib:make_error_reply(El, "406", "Not Acceptable"),
	    send_element(StateData#state.sender, Err),
	    {next_state, wait_for_auth, StateData};
	{auth, ID, {U, P, D, R}} ->
	    io:format("AUTH: ~p~n", [{U, P, D, R}]),
	    case acl:match_rule(StateData#state.access, {U, ?MYNAME, R}) of
		allow ->
		    case ejabberd_auth:check_password(
			   U, P, StateData#state.streamid, D) of
			true ->
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
			    Err = jlib:make_error_reply(
				    El, "401", "Unauthorized"),
			    send_element(StateData#state.sender, Err),
			    {next_state, wait_for_auth, StateData}
		    end;
		_ ->
		    Err = jlib:make_error_reply(El, "405", "Not Allowed"),
		    send_element(StateData#state.sender, Err),
		    {next_state, wait_for_auth, StateData}
	    end;
	_ ->
	    case jlib:iq_query_info(El) of
		{iq, ID, Type, ?NS_REGISTER, SubEl} ->
		    ResIQ = mod_register:process_iq(
			      {"", "", ""}, {"", ?MYNAME, ""},
			      {iq, ID, Type, ?NS_REGISTER, SubEl}),
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
				presence_track(FromJID, ToJID, El, StateData)
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
    send_text(StateData#state.sender, Text),
    {next_state, StateName, StateData};
handle_info(replaced, StateName, StateData) ->
    % TODO
    %send_text(StateData#state.sender, Text),
    {stop, normal, StateData#state{user = ""}};
handle_info({route, From, To, Packet}, StateName, StateData) ->
    {xmlelement, Name, Attrs, Els} = Packet,
    {FU, FS, FR} = From,
    {Pass, NewAttrs, NewState} =
	case Name of
	    "presence" ->
		case xml:get_attr_s("type", Attrs) of
		    "probe" ->
			process_presence_probe(From, To, StateData),
			{false, Attrs, StateData};
		    "error" ->
			NewA = remove_element(From, StateData#state.pres_a),
			{true, Attrs, StateData#state{pres_a = NewA}};
		    "invisible" ->
			Attrs1 = lists:keydelete("type", 1, Attrs),
			{true, [{"type", "unavailable"} | Attrs1], StateData};
		    "subscribe" ->
			{true, Attrs, StateData};
		    "subscribed" ->
			{true, Attrs, StateData};
		    "unsubscribe" ->
			{true, Attrs, StateData};
		    "unsubscribed" ->
			{true, Attrs, StateData};
		    _ ->
			{true, Attrs, StateData}
		end;
	    "broadcast" ->
		?DEBUG("broadcast!!!!!!!!!!!~n~p~n", [Els]),
		NewSt = case Els of
			    [{item, IJID, ISubscription}] ->
				{false, Attrs,
				 roster_change(IJID, ISubscription,
					       StateData)};
			    [{exit, Reason}] ->
				{exit, Attrs, Reason};
			    _ ->
				{false, Attrs, StateData}
			end;
	    "iq" ->
		IQ = jlib:iq_query_info(Packet),
		case IQ of
		    {iq, ID, Type, ?NS_VCARD, SubEl} ->
			ResIQ = mod_vcard:process_sm_iq(From, To, IQ),
			ejabberd_router:route(To,
					      From,
					      jlib:iq_to_xml(ResIQ)),
			{false, Attrs, StateData};
		    %{iq, ID, Type, ?NS_VCARD, SubEl} ->
			% TODO: don't pass packets until roster loaded
			%{true, Attrs, StateData};
		    _ ->
			{true, Attrs, StateData}
		end;
	    _ ->
		{true, Attrs, StateData}
	end,
    if
	Pass == exit ->
	    {stop, normal, StateData};
	Pass ->
	    Attrs2 = jlib:replace_from_to_attrs(jlib:jid_to_string(From),
						jlib:jid_to_string(To),
						NewAttrs),
	    Text = xml:element_to_string({xmlelement, Name, Attrs2, Els}),
	    send_text(StateData#state.sender, Text),
	    {next_state, StateName, NewState};
	true ->
	    {next_state, StateName, NewState}
    end.

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
				      StateData#state.resource),
            From = {StateData#state.user,
                    StateData#state.server,
                    StateData#state.resource},
            Packet = {xmlelement, "presence", [{"type", "unavailable"}], []},
            ejabberd_sm:unset_presence(StateData#state.user,
                		       StateData#state.resource),
            presence_broadcast(From, StateData#state.pres_a, Packet),
            presence_broadcast(From, StateData#state.pres_i, Packet)
    end,
    StateData#state.sender ! close,
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
        {error, Reason} ->
	    exit(XMLStreamPid, closed),
	    gen_fsm:send_event(C2SPid, closed),
	    ok
    end.

sender(Socket, SockMod) ->
    receive
	{send_text, Text} ->
	    SockMod:send(Socket,Text),
	    sender(Socket, SockMod);
	close ->
	    SockMod:close(Socket),
	    ok
    end.

send_text(Pid, Text) ->
    Pid ! {send_text, Text}.

send_element(Pid, El) ->
    send_text(Pid, xml:element_to_string(El)).

new_id() ->
    randoms:get_string().


is_auth_packet(El) ->
    case jlib:iq_query_info(El) of
	{iq, ID, Type, ?NS_AUTH, SubEl} ->
	    {xmlelement, _, _, Els} = SubEl,
	    {auth, ID,
	     get_auth_tags(Els, "", "", "", "")};
	_ ->
	    false
    end.


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
	    ejabberd_sm:unset_presence(StateData#state.user,
				       StateData#state.resource),
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
					     pres_invis = true},
			presence_broadcast_first(From, S1, Packet);
		    true ->
			StateData
		end,
	    NewState;
	"error" ->
	    StateData;
	"subscribe" ->
	    StateData;
	"subscribed" ->
	    StateData;
	"unsubscribe" ->
	    StateData;
	"unsubscribed" ->
	    StateData;
	_ ->
	    update_priority(xml:get_subtag(Packet, "priority"), StateData),
	    FromUnavail = (StateData#state.pres_last == undefined) or
		StateData#state.pres_invis,
	    ?DEBUG("from unavail = ~p~n", [FromUnavail]),
	    NewState =
		if
		    FromUnavail ->
			% TODO: watching ourself
			
			mod_offline:resend_offline_messages(
			  StateData#state.user),
			presence_broadcast_first(
			  From, StateData#state{pres_last = Packet,
						pres_invis = false
					       }, Packet);
		    true ->
			presence_broadcast_to_trusted(From,
						      StateData#state.pres_f,
						      StateData#state.pres_a,
						      Packet),
			StateData#state{pres_last = Packet,
					pres_invis = false
				       }
		end,
	    NewState
    end.

presence_track(From, To, Packet, StateData) ->
    {xmlelement, Name, Attrs, Els} = Packet,
    LTo = jlib:jid_tolower(To),
    User = StateData#state.user,
    case xml:get_attr_s("type", Attrs) of
	"unavailable" ->
	    ejabberd_router:route(From, To, Packet),
	    I = remove_element(LTo, StateData#state.pres_i),
	    A = remove_element(LTo, StateData#state.pres_a),
	    StateData#state{pres_i = I,
			    pres_a = A};
	"invisible" ->
	    ejabberd_router:route(From, To, Packet),
	    I = ?SETS:add_element(LTo, StateData#state.pres_i),
	    A = remove_element(LTo, StateData#state.pres_a),
	    StateData#state{pres_i = I,
			    pres_a = A};
	"subscribe" ->
	    ejabberd_router:route(jlib:jid_remove_resource(From), To, Packet),
	    mod_roster:out_subscription(User, To, subscribe),
	    StateData;
	"subscribed" ->
	    ejabberd_router:route(jlib:jid_remove_resource(From), To, Packet),
	    mod_roster:out_subscription(User, To, subscribed),
	    StateData;
	"unsubscribe" ->
	    ejabberd_router:route(jlib:jid_remove_resource(From), To, Packet),
	    mod_roster:out_subscription(User, To, unsubscribe),
	    StateData;
	"unsubscribed" ->
	    ejabberd_router:route(jlib:jid_remove_resource(From), To, Packet),
	    mod_roster:out_subscription(User, To, unsubscribed),
	    StateData;
	"error" ->
	    ejabberd_router:route(From, To, Packet),
	    StateData;
	_ ->
	    ejabberd_router:route(From, To, Packet),
	    I = remove_element(LTo, StateData#state.pres_i),
	    A = ?SETS:add_element(LTo, StateData#state.pres_a),
	    StateData#state{pres_i = I,
			    pres_a = A}
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


remove_element(E, Set) ->
    case ?SETS:is_element(E, Set) of
	true ->
	    ?SETS:del_element(E, Set);
	_ ->
	    Set
    end.


roster_change(IJID, ISubscription, StateData) ->
    LIJID = jlib:jid_tolower(IJID),
    IsFrom = (ISubscription == both) or (ISubscription == from),
    IsTo   = (ISubscription == both) or (ISubscription == to),
    FSet = if
	       IsFrom ->
		   ?SETS:add_element(LIJID, StateData#state.pres_f);
	       true ->
		   remove_element(LIJID, StateData#state.pres_f)
	   end,
    TSet = if
	       IsTo ->
		   ?SETS:add_element(LIJID, StateData#state.pres_t);
	       true ->
		   remove_element(LIJID, StateData#state.pres_t)
	   end,
    case StateData#state.pres_last of
	unknown ->
	    StateData#state{pres_f = FSet, pres_t = TSet};
	P ->
	    ?DEBUG("roster changed for ~p~n", [StateData#state.user]),
	    From = {StateData#state.user,
		    StateData#state.server,
		    StateData#state.resource},
	    Cond1 = (not StateData#state.pres_invis) and IsFrom,
	    Cond2 = (not IsFrom)
		and (?SETS:is_element(LIJID, StateData#state.pres_a) or
		     ?SETS:is_element(LIJID, StateData#state.pres_i)),
	    if
		Cond1 ->
		    ?DEBUG("C1: ~p~n", [LIJID]),
		    ejabberd_router:route(From, IJID, P),
		    A = ?SETS:add_element(LIJID,
					  StateData#state.pres_a),
		    StateData#state{pres_a = A,
				    pres_f = FSet,
				    pres_t = TSet};
		Cond2 ->
		    ?DEBUG("C2: ~p~n", [LIJID]),
		    ejabberd_router:route(From, IJID,
					  {xmlelement, "presence",
					   [{"type", "unavailable"}], []}),
		    I = remove_element(LIJID,
				       StateData#state.pres_i),
		    A = remove_element(LIJID,
				       StateData#state.pres_a),
		    StateData#state{pres_i = I,
				    pres_a = A,
				    pres_f = FSet,
				    pres_t = TSet};
		true ->
		    StateData#state{pres_f = FSet, pres_t = TSet}
	    end
    end.


update_priority(El, StateData) ->
    Pri = case El of
	      false ->
		  0;
	      _ ->
		  case catch list_to_integer(xml:get_tag_cdata(El)) of
		      P when is_integer(P) ->
			  P;
		      _ ->
			  0
		  end
	  end,
    ejabberd_sm:set_presence(StateData#state.user,
			     StateData#state.resource,
			     Pri).
		  
