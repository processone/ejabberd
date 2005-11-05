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
-export([start/3, start_link/3]).

%% gen_fsm callbacks
-export([init/1,
	 open_socket/2,
	 wait_for_stream/2,
	 wait_for_validation/2,
	 wait_for_features/2,
	 wait_for_auth_result/2,
	 wait_for_starttls_proceed/2,
	 stream_established/2,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 code_change/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, {socket, receiver,
		sockmod,
		streamid,
		use_v10,
		tls = false,
		tls_required = false,
		tls_enabled = false,
		tls_options = [],
		authenticated = false,
		try_auth = true,
		myname, server, queue,
		new = false, verify = false,
		timer}).

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
	"to='~s'~s>"
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
start(From, Host, Type) ->
    supervisor:start_child(ejabberd_s2s_out_sup, [From, Host, Type]).

start_link(From, Host, Type) ->
    gen_fsm:start_link(ejabberd_s2s_out, [From, Host, Type], ?FSMOPTS).

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
    TLS = case ejabberd_config:get_local_option(s2s_use_starttls) of
	      undefined ->
		  false;
	      UseStartTLS ->
		  UseStartTLS
	  end,
    UseV10 = TLS,
    TLSOpts = case ejabberd_config:get_local_option(s2s_certfile) of
		  undefined ->
		      [];
		  CertFile ->
		      [{certfile, CertFile}, connect]
	      end,
    {New, Verify} = case Type of
			{new, Key} ->
			    {Key, false};
			{verify, Pid, Key, SID} ->
			    {false, {Pid, Key, SID}}
		    end,
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
    {ok, open_socket, #state{use_v10 = UseV10,
			     tls = TLS,
			     tls_options = TLSOpts,
			     queue = queue:new(),
			     myname = From,
			     server = Server,
			     new = New,
			     verify = Verify,
			     timer = Timer}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
open_socket(init, StateData) ->
    {Addr, Port} = get_addr_port(StateData#state.server),
    Res = case idna:domain_utf8_to_ascii(Addr) of
	      false -> {error, badarg};
	      ASCIIAddr ->
		  ?DEBUG("s2s_out: connecting to ~s:~p~n", [ASCIIAddr, Port]),
		  case gen_tcp:connect(ASCIIAddr, Port,
				       [binary, {packet, 0},
					{active, false}]) of
		      {ok, _Socket} = R -> R;
		      {error, Reason1} ->
			  ?DEBUG("s2s_out: connect return ~p~n", [Reason1]),
			  catch gen_tcp:connect(Addr, Port,
						[binary, {packet, 0},
						 {active, false}, inet6])
		  end
	  end,
    case Res of
	{ok, Socket} ->
	    ReceiverPid = ejabberd_receiver:start(Socket, gen_tcp, none),
	    Version = if
			  StateData#state.use_v10 ->
			      " version='1.0'";
			  true ->
			      ""
		      end,
	    NewStateData = StateData#state{socket = Socket,
					   sockmod = gen_tcp,
					   tls_enabled = false,
					   receiver = ReceiverPid,
					   streamid = new_id()},
	    send_text(NewStateData, io_lib:format(?STREAM_HEADER,
					    [StateData#state.server,
					     Version])),
	    {next_state, wait_for_stream, NewStateData};
	{error, Reason} ->
	    ?DEBUG("s2s_out: inet6 connect return ~p~n", [Reason]),
	    Error = ?ERR_REMOTE_SERVER_NOT_FOUND,
	    bounce_messages(Error),
	    {stop, normal, StateData};
	{'EXIT', Reason} ->
	    ?DEBUG("s2s_out: inet6 connect crashed ~p~n", [Reason]),
	    Error = ?ERR_REMOTE_SERVER_NOT_FOUND,
	    bounce_messages(Error),
	    {stop, normal, StateData}
    end;
open_socket(_, StateData) ->
    {next_state, open_socket, StateData}.


wait_for_stream({xmlstreamstart, Name, Attrs}, StateData) ->
    case {xml:get_attr_s("xmlns", Attrs),
	  xml:get_attr_s("xmlns:db", Attrs),
	  xml:get_attr_s("version", Attrs) == "1.0"} of
	{"jabber:server", "jabber:server:dialback", false} ->
	    send_db_request(StateData);
	{"jabber:server", "jabber:server:dialback", true} when
	      StateData#state.use_v10 ->
	    {next_state, wait_for_features, StateData};
	{"jabber:server", "", true} when StateData#state.use_v10 ->
	    ?INFO_MSG("restarted: ~p", [{StateData#state.myname,
					 StateData#state.server}]),
	    % TODO: clear message queue
	    (StateData#state.sockmod):close(StateData#state.socket),
	    gen_fsm:send_event(self(), init),
	    {next_state, open_socket, StateData#state{socket = undefined,
						      use_v10 = false}};
	_ ->
	    send_text(StateData, ?INVALID_NAMESPACE_ERR),
	    {stop, normal, StateData}
    end;

wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
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
		    send_queue(StateData, StateData#state.queue),
		    {next_state, stream_established,
		     StateData#state{queue = queue:new()}};
		_ ->
		    % TODO: bounce packets
		    {stop, normal, StateData}
	    end;
	{verify, To, From, Id, Type} ->
	    ?INFO_MSG("recv verify: ~p", [{From, To, Id, Type}]),
	    case StateData#state.verify of
		false ->
		    {next_state, wait_for_validation, StateData};
		{Pid, _Key, _SID} ->
		    case Type of
			"valid" ->
			    gen_fsm:send_event(
			      Pid, {valid,
				    StateData#state.server,
				    StateData#state.myname});
			_ ->
			    gen_fsm:send_event(
			      Pid, {invalid,
				    StateData#state.server,
				    StateData#state.myname})
		    end,
		    if
			StateData#state.verify == false ->
			    {stop, normal, StateData};
			true ->
			    {next_state, wait_for_validation, StateData}
		    end
	    end;
	_ ->
	    {next_state, wait_for_validation, StateData}
    end;

wait_for_validation({xmlstreamend, Name}, StateData) ->
    {stop, normal, StateData};

wait_for_validation({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_validation(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_validation(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_features({xmlstreamelement, El}, StateData) ->
    case El of
	{xmlelement, "stream:features", _Attrs, Els} ->
	    {SASLEXT, StartTLS, StartTLSRequired} =
		lists:foldl(
		  fun({xmlelement, "mechanisms", Attrs1, Els1} = El1,
		      {SEXT, STLS, STLSReq} = Acc) ->
			  case xml:get_attr_s("xmlns", Attrs1) of
			      ?NS_SASL ->
				  NewSEXT =
				      lists:any(
					fun({xmlelement, "mechanism", _, Els2}) ->
						case xml:get_cdata(Els2) of
						    "EXTERNAL" -> true;
						    _ -> false
						end;
					   (_) -> false
					end, Els1),
				  {NewSEXT, STLS, STLSReq};
			      _ ->
				  Acc
			  end;
		     ({xmlelement, "starttls", Attrs1, Els1} = El1,
		      {SEXT, STLS, STLSReq} = Acc) ->
			  case xml:get_attr_s("xmlns", Attrs1) of
			      ?NS_TLS ->
				  Req = case xml:get_subtag(El1, "required") of
					    {xmlelement, _, _, _} -> true;
					    false -> false
					end,
				  {SEXT, true, Req};
			      _ ->
				  Acc
			  end;
		     (_, Acc) ->
			  Acc
		  end, {false, false, false}, Els),
	    if
		(not SASLEXT) and (not StartTLS) and
		StateData#state.authenticated ->
		    send_queue(StateData, StateData#state.queue),
		    {next_state, stream_established,
		     StateData#state{queue = queue:new()}};
		SASLEXT and StateData#state.try_auth and
		(StateData#state.new /= false) ->
		    send_element(StateData,
				 {xmlelement, "auth",
				  [{"xmlns", ?NS_SASL},
				   {"mechanism", "EXTERNAL"}],
				  [{xmlcdata,
				    jlib:encode_base64(
				      StateData#state.myname)}]}),
		    {next_state, wait_for_auth_result,
		     StateData#state{try_auth = false}};
		StartTLS and StateData#state.tls and
		(not StateData#state.tls_enabled) ->
		    StateData#state.receiver ! {change_timeout, 100},
		    send_element(StateData,
				 {xmlelement, "starttls",
				  [{"xmlns", ?NS_TLS}], []}),
		    {next_state, wait_for_starttls_proceed, StateData};
		StartTLSRequired and (not StateData#state.tls) ->
		    ?INFO_MSG("restarted: ~p", [{StateData#state.myname,
						 StateData#state.server}]),
		    (StateData#state.sockmod):close(StateData#state.socket),
		    gen_fsm:send_event(self(), init),
		    {next_state, open_socket,
		     StateData#state{socket = undefined,
				     use_v10 = false}};
		true ->
		    send_db_request(StateData)
	    end;
	_ ->
	    send_text(StateData,
		      xml:element_to_string(?SERR_BAD_FORMAT) ++
		      ?STREAM_TRAILER),
	    {stop, normal, StateData}
    end;

wait_for_features({xmlstreamend, Name}, StateData) ->
    {stop, normal, StateData};

wait_for_features({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_features(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_features(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_auth_result({xmlstreamelement, El}, StateData) ->
    case El of
	{xmlelement, "success", Attrs, _Els} ->
	    case xml:get_attr_s("xmlns", Attrs) of
		?NS_SASL ->
		    ?INFO_MSG("auth: ~p", [{StateData#state.myname,
					    StateData#state.server}]),
		    ejabberd_receiver:reset_stream(
		      StateData#state.receiver),
		    send_text(StateData,
			      io_lib:format(?STREAM_HEADER,
					    [StateData#state.server,
					     " version='1.0'"])),
		    {next_state, wait_for_stream,
		     StateData#state{streamid = new_id(),
				     authenticated = true
				    }};
		_ ->
		    send_text(StateData,
			      xml:element_to_string(?SERR_BAD_FORMAT) ++
			      ?STREAM_TRAILER),
		    {stop, normal, StateData}
	    end;
	{xmlelement, "failure", Attrs, _Els} ->
	    case xml:get_attr_s("xmlns", Attrs) of
		?NS_SASL ->
		    ?INFO_MSG("restarted: ~p", [{StateData#state.myname,
						 StateData#state.server}]),
		    (StateData#state.sockmod):close(StateData#state.socket),
		    gen_fsm:send_event(self(), init),
		    {next_state, open_socket,
		     StateData#state{socket = undefined}};
		_ ->
		    send_text(StateData,
			      xml:element_to_string(?SERR_BAD_FORMAT) ++
			      ?STREAM_TRAILER),
		    {stop, normal, StateData}
	    end;
	_ ->
	    send_text(StateData,
		      xml:element_to_string(?SERR_BAD_FORMAT) ++
		      ?STREAM_TRAILER),
	    {stop, normal, StateData}
    end;

wait_for_auth_result({xmlstreamend, Name}, StateData) ->
    {stop, normal, StateData};

wait_for_auth_result({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_auth_result(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_auth_result(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_starttls_proceed({xmlstreamelement, El}, StateData) ->
    case El of
	{xmlelement, "proceed", Attrs, _Els} ->
	    case xml:get_attr_s("xmlns", Attrs) of
		?NS_TLS ->
		    ?INFO_MSG("starttls: ~p", [{StateData#state.myname,
						StateData#state.server}]),
		    Socket = StateData#state.socket,
		    TLSOpts = case ejabberd_config:get_local_option(
				     {domain_certfile,
				      StateData#state.server}) of
				  undefined ->
				      StateData#state.tls_options;
				  CertFile ->
				      [{certfile, CertFile} |
				       lists:keydelete(
					 certfile, 1,
					 StateData#state.tls_options)]
			      end,
		    {ok, TLSSocket} = tls:tcp_to_tls(Socket, TLSOpts),
		    ejabberd_receiver:starttls(
		      StateData#state.receiver, TLSSocket),
		    StateData#state.receiver ! {change_timeout, infinity},
		    NewStateData = StateData#state{sockmod = tls,
						   socket = TLSSocket,
						   streamid = new_id(),
						   tls_enabled = true
						  },
		    send_text(NewStateData,
			      io_lib:format(?STREAM_HEADER,
					    [StateData#state.server,
					     " version='1.0'"])),
		    {next_state, wait_for_stream, NewStateData};
		_ ->
		    send_text(StateData,
			      xml:element_to_string(?SERR_BAD_FORMAT) ++
			      ?STREAM_TRAILER),
		    {stop, normal, StateData}
	    end;
	_ ->
	    {stop, normal, StateData}
    end;

wait_for_starttls_proceed({xmlstreamend, Name}, StateData) ->
    {stop, normal, StateData};

wait_for_starttls_proceed({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_starttls_proceed(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_starttls_proceed(closed, StateData) ->
    {stop, normal, StateData}.


stream_established({xmlstreamelement, El}, StateData) ->
    ?INFO_MSG("stream established", []),
    case is_verify_res(El) of
	{verify, VTo, VFrom, VId, VType} ->
	    ?INFO_MSG("recv verify: ~p", [{VFrom, VTo, VId, VType}]),
	    case StateData#state.verify of
		{VPid, _VKey, _SID} ->
		    case VType of
			"valid" ->
			    gen_fsm:send_event(
			      VPid, {valid,
				     StateData#state.server,
				     StateData#state.myname});
			_ ->
			    gen_fsm:send_event(
			      VPid, {invalid,
				     StateData#state.server,
				     StateData#state.myname})
		    end;
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end,
    {next_state, stream_established, StateData};

stream_established({xmlstreamend, Name}, StateData) ->
    {stop, normal, StateData};

stream_established({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
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
    send_text(StateData, Text),
    cancel_timer(StateData#state.timer),
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
    {next_state, StateName, StateData#state{timer = Timer}};

handle_info({send_element, El}, StateName, StateData) ->
    cancel_timer(StateData#state.timer),
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
    case StateName of
	stream_established ->
	    send_element(StateData, El),
	    {next_state, StateName, StateData#state{timer = Timer}};
	_ ->
	    Q = queue:in(El, StateData#state.queue),
	    {next_state, StateName, StateData#state{queue = Q,
						    timer = Timer}}
    end;

%handle_info({tcp, Socket, Data}, StateName, StateData) ->
%    xml_stream:send_text(StateData#state.xmlpid, Data),
%    {next_state, StateName, StateData};
%
%handle_info({tcp_closed, Socket}, StateName, StateData) ->
%    gen_fsm:send_event(self(), closed),
%    {next_state, StateName, StateData};
%
%handle_info({tcp_error, Socket, Reason}, StateName, StateData) ->
%    gen_fsm:send_event(self(), closed),
%    {next_state, StateName, StateData};

handle_info({timeout, Timer, _}, StateName,
	    #state{timer = Timer} = StateData) ->
    {stop, normal, StateData};

handle_info(_, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
    ?INFO_MSG("terminated: ~p", [Reason]),
    Error = ?ERR_REMOTE_SERVER_NOT_FOUND,
    bounce_queue(StateData#state.queue, Error),
    case StateData#state.new of
	false ->
	    ok;
	Key ->
	    ejabberd_s2s:remove_connection({StateData#state.myname,
	        			    StateData#state.server})
    end,
    case StateData#state.socket of
	undefined ->
	    ok;
	Socket ->
	    (StateData#state.sockmod):close(Socket)
    end,
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

send_text(StateData, Text) ->
    (StateData#state.sockmod):send(StateData#state.socket, Text).

send_element(StateData, El) ->
    send_text(StateData, xml:element_to_string(El)).

send_queue(StateData, Q) ->
    case queue:out(Q) of
	{{value, El}, Q1} ->
	    send_element(StateData, El),
	    send_queue(StateData, Q1);
	{empty, Q1} ->
	    ok
    end.

bounce_queue(Q, Error) ->
    case queue:out(Q) of
	{{value, El}, Q1} ->
	    Err = jlib:make_error_reply(El, Error),
	    From = jlib:string_to_jid(xml:get_tag_attr_s("from", El)),
	    To = jlib:string_to_jid(xml:get_tag_attr_s("to", El)),
	    ejabberd_router:route(To, From, Err),
	    bounce_queue(Q1, Error);
	{empty, Q1} ->
	    ok
    end.

new_id() ->
    randoms:get_string().

cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    receive
	{timeout, Timer, _} ->
	    ok
    after 0 ->
	    ok
    end.

bounce_messages(Error) ->
    receive
	{send_element, El} ->
	    {xmlelement, _Name, Attrs, _SubTags} = El,
	    case xml:get_attr_s("type", Attrs) of
	        "error" ->
	            ok;
	        _ ->
	            Err = jlib:make_error_reply(El, Error),
		    From = jlib:string_to_jid(xml:get_attr_s("from", Attrs)),
		    To = jlib:string_to_jid(xml:get_attr_s("to", Attrs)),
		    ejabberd_router:route(To, From, Err)
	    end,
	    bounce_messages(Error)
    after 0 ->
	    ok
    end.


send_db_request(StateData) ->
    Server = StateData#state.server,
    New = case StateData#state.new of
	      false ->
		  case ejabberd_s2s:try_register(
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
	    send_element(StateData,
			 {xmlelement,
			  "db:result",
			  [{"from", StateData#state.myname},
			   {"to", Server}],
			  [{xmlcdata, Key1}]})
    end,
    case StateData#state.verify of
	false ->
	    ok;
	{Pid, Key2, SID} ->
	    send_element(StateData,
			 {xmlelement,
			  "db:verify",
			  [{"from", StateData#state.myname},
			   {"to", StateData#state.server},
			   {"id", SID}],
			  [{xmlcdata, Key2}]})
    end,
    {next_state, wait_for_validation, StateData#state{new = New}}.


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
    Res = case inet_res:getbyname("_xmpp-server._tcp." ++ Server, srv) of
	      {error, _Reason} ->
		  inet_res:getbyname("_jabber._tcp." ++ Server, srv);
	      {ok, _HEnt} = R -> R
	  end,
    case Res of
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



