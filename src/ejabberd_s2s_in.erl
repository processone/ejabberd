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
-export([start/2,
	 start_link/2]).

%% gen_fsm callbacks
-export([init/1,
	 wait_for_stream/2,
	 wait_for_feature_request/2,
	 stream_established/2,
	 handle_event/3,
	 handle_sync_event/4,
	 code_change/4,
	 handle_info/3,
	 terminate/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
%-include_lib("ssl/pkix/SSL-PKIX.hrl").
-include_lib("ssl/pkix/PKIX1Explicit88.hrl").
-include_lib("ssl/pkix/PKIX1Implicit88.hrl").
-include("tls/XmppAddr.hrl").

-define(DICT, dict).

-record(state, {socket,
		sockmod,
		receiver,
		streamid,
		shaper,
		tls = false,
		tls_enabled = false,
		tls_options = [],
		authenticated = false,
		auth_domain,
	        connections = ?DICT:new(),
		timer}).


%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-define(STREAM_HEADER(Version),
	("<?xml version='1.0'?>"
	 "<stream:stream "
	 "xmlns:stream='http://etherx.jabber.org/streams' "
	 "xmlns='jabber:server' "
	 "xmlns:db='jabber:server:dialback' "
	 "id='" ++ StateData#state.streamid ++ "'" ++ Version ++ ">")
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
start(SockData, Opts) ->
    supervisor:start_child(ejabberd_s2s_in_sup, [SockData, Opts]).

start_link(SockData, Opts) ->
    gen_fsm:start_link(ejabberd_s2s_in, [SockData, Opts], ?FSMOPTS).

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
    ?INFO_MSG("started: ~p", [{SockMod, Socket}]),
    ReceiverPid = ejabberd_receiver:start(Socket, SockMod, none),
    Shaper = case lists:keysearch(shaper, 1, Opts) of
		 {value, {_, S}} -> S;
		 _ -> none
	     end,
    StartTLS = case ejabberd_config:get_local_option(s2s_use_starttls) of
		   undefined ->
		       false;
		   UseStartTLS ->
		       UseStartTLS
	       end,
    TLSOpts = case ejabberd_config:get_local_option(s2s_certfile) of
		  undefined ->
		      [];
		  CertFile ->
		      [{certfile, CertFile}]
	      end,
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
    {ok, wait_for_stream,
     #state{socket = Socket,
	    sockmod = SockMod,
	    receiver = ReceiverPid,
	    streamid = new_id(),
	    shaper = Shaper,
	    tls = StartTLS,
	    tls_enabled = false,
	    tls_options = TLSOpts,
	    timer = Timer}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------

wait_for_stream({xmlstreamstart, _Name, Attrs}, StateData) ->
    case {xml:get_attr_s("xmlns", Attrs),
	  xml:get_attr_s("xmlns:db", Attrs),
	  xml:get_attr_s("version", Attrs) == "1.0"} of
	{"jabber:server", "jabber:server:dialback", true} when
	      StateData#state.tls and (not StateData#state.authenticated) ->
	    send_text(StateData, ?STREAM_HEADER(" version='1.0'")),
	    SASL =
		if
		    StateData#state.tls_enabled ->
			case tls:get_peer_certificate(StateData#state.socket) of
			    {ok, _Cert} ->
				case tls:get_verify_result(
				       StateData#state.socket) of
				    0 ->
					[{xmlelement, "mechanisms",
					  [{"xmlns", ?NS_SASL}],
					  [{xmlelement, "mechanism", [],
					    [{xmlcdata, "EXTERNAL"}]}]}];
				    _ ->
					[]
				end;
			    error ->
				[]
			end;
		    true ->
			[]
		end,
	    StartTLS = if
			   StateData#state.tls_enabled ->
			       [];
			   true ->
			       [{xmlelement, "starttls",
				 [{"xmlns", ?NS_TLS}], []}]
		       end,
	    send_element(StateData,
			 {xmlelement, "stream:features", [],
			  SASL ++ StartTLS}),
	    {next_state, wait_for_feature_request, StateData};
	{"jabber:server", _, true} when
	      StateData#state.authenticated ->
	    send_text(StateData, ?STREAM_HEADER(" version='1.0'")),
	    send_element(StateData,
			 {xmlelement, "stream:features", [], []}),
	    {next_state, stream_established, StateData};
	{"jabber:server", "jabber:server:dialback", _} ->
	    send_text(StateData, ?STREAM_HEADER("")),
	    {next_state, stream_established, StateData};
	_ ->
	    send_text(StateData, ?INVALID_NAMESPACE_ERR),
	    {stop, normal, StateData}
    end;

wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      ?STREAM_HEADER("") ++ ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_stream(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_feature_request({xmlstreamelement, El}, StateData) ->
    {xmlelement, Name, Attrs, Els} = El,
    TLS = StateData#state.tls,
    TLSEnabled = StateData#state.tls_enabled,
    SockMod = StateData#state.sockmod,
    case {xml:get_attr_s("xmlns", Attrs), Name} of
	{?NS_TLS, "starttls"} when TLS == true,
				   TLSEnabled == false,
				   SockMod == gen_tcp ->
	    ?INFO_MSG("starttls", []),
	    Socket = StateData#state.socket,
	    TLSOpts = StateData#state.tls_options,
	    {ok, TLSSocket} = tls:tcp_to_tls(Socket, TLSOpts),
	    ejabberd_receiver:starttls(StateData#state.receiver, TLSSocket),
	    send_element(StateData,
			 {xmlelement, "proceed", [{"xmlns", ?NS_TLS}], []}),
	    {next_state, wait_for_stream,
	     StateData#state{sockmod = tls,
			     socket = TLSSocket,
			     streamid = new_id(),
			     tls_enabled = true
			    }};
	{?NS_SASL, "auth"} when TLSEnabled ->
	    Mech = xml:get_attr_s("mechanism", Attrs),
	    case Mech of
		"EXTERNAL" ->
		    Auth = jlib:decode_base64(xml:get_cdata(Els)),
		    AuthDomain = jlib:nameprep(Auth),
		    AuthRes =
			case tls:get_peer_certificate(StateData#state.socket) of
			    {ok, Cert} ->
				case tls:get_verify_result(
				       StateData#state.socket) of
				    0 ->
					case AuthDomain of
					    error ->
						false;
					    _ ->
						lists:member(
						  AuthDomain,
						  get_cert_domains(Cert))
					end;
				    _ ->
					false
				end;
			    error ->
				false
			end,
		    if
			AuthRes ->
			    ejabberd_receiver:reset_stream(
			      StateData#state.receiver),
			    send_element(StateData,
					 {xmlelement, "success",
					  [{"xmlns", ?NS_SASL}], []}),
			    ?INFO_MSG("(~w) Accepted s2s authentication for ~s",
				      [StateData#state.socket, AuthDomain]),
			    {next_state, wait_for_stream,
			     StateData#state{streamid = new_id(),
					     authenticated = true,
					     auth_domain = AuthDomain
					    }};
			true ->
			    send_element(StateData,
					 {xmlelement, "failure",
					  [{"xmlns", ?NS_SASL}], []}),
			    send_text(StateData, ?STREAM_TRAILER),
			    {stop, normal, StateData}
		    end;
		_ ->
		    send_element(StateData,
				 {xmlelement, "failure",
				  [{"xmlns", ?NS_SASL}],
				  [{xmlelement, "invalid-mechanism", [], []}]}),
		    {stop, normal, StateData}
	    end;
	_ ->
	    stream_established({xmlstreamelement, El}, StateData)
    end;

wait_for_feature_request({xmlstreamend, _Name}, StateData) ->
    send_text(StateData, ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_feature_request({xmlstreamerror, _}, StateData) ->
    send_text(StateData, ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_feature_request(closed, StateData) ->
    {stop, normal, StateData}.


stream_established({xmlstreamelement, El}, StateData) ->
    cancel_timer(StateData#state.timer),
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
    case is_key_packet(El) of
	{key, To, From, Id, Key} ->
	    ?INFO_MSG("GET KEY: ~p", [{To, From, Id, Key}]),
	    LTo = jlib:nameprep(To),
	    LFrom = jlib:nameprep(From),
	    case lists:member(LTo, ejabberd_router:dirty_get_all_domains()) of
		true ->
		    ejabberd_s2s_out:start(To, From,
					   {verify, self(),
					    Key, StateData#state.streamid}),
		    Conns = ?DICT:store({LFrom, LTo}, wait_for_verification,
					StateData#state.connections),
		    change_shaper(StateData, LTo, jlib:make_jid("", LFrom, "")),
		    {next_state,
		     stream_established,
		     StateData#state{connections = Conns,
				     timer = Timer}};
		_ ->
		    send_text(StateData, ?HOST_UNKNOWN_ERR),
		    {stop, normal, StateData}
	    end;
	{verify, To, From, Id, Key} ->
	    ?INFO_MSG("VERIFY KEY: ~p", [{To, From, Id, Key}]),
	    LTo = jlib:nameprep(To),
	    LFrom = jlib:nameprep(From),
	    Key1 = ejabberd_s2s:get_key({LTo, LFrom}),
	    Type = if Key == Key1 -> "valid";
		      true -> "invalid"
		   end,
	    send_element(StateData,
			 {xmlelement,
			  "db:verify",
			  [{"from", To},
			   {"to", From},
			   {"id", Id},
			   {"type", Type}],
			  []}),
	    {next_state, stream_established, StateData#state{timer = Timer}};
	_ ->
	    {xmlelement, Name, Attrs, _Els} = El,
	    From_s = xml:get_attr_s("from", Attrs),
	    From = jlib:string_to_jid(From_s),
	    To_s = xml:get_attr_s("to", Attrs),
	    To = jlib:string_to_jid(To_s),
	    if
		(To /= error) and (From /= error) ->
		    LFrom = From#jid.lserver,
		    LTo = To#jid.lserver,
		    if
			StateData#state.authenticated ->
			    case (LFrom == StateData#state.auth_domain)
				andalso
				lists:member(
				  LTo,
				  ejabberd_router:dirty_get_all_domains()) of
				true ->
				    if ((Name == "iq") or
					(Name == "message") or
					(Name == "presence")) ->
					    ejabberd_router:route(From, To, El);
				       true ->
					    error
				    end;
				false ->
				    error
			    end;
			true ->
			    case ?DICT:find({LFrom, LTo},
					    StateData#state.connections) of
				{ok, established} ->
				    if ((Name == "iq") or
					(Name == "message") or
					(Name == "presence")) ->
					    ejabberd_router:route(From, To, El);
				       true ->
					    error
				    end;
				_ ->
				    error
			    end
		    end;
		true ->
		    error
	    end,
	    {next_state, stream_established, StateData#state{timer = Timer}}
    end;

stream_established({valid, From, To}, StateData) ->
    send_element(StateData,
		 {xmlelement,
		  "db:result",
		  [{"from", To},
		   {"to", From},
		   {"type", "valid"}],
		  []}),
    LFrom = jlib:nameprep(From),
    LTo = jlib:nameprep(To),
    NSD = StateData#state{
	    connections = ?DICT:store({LFrom, LTo}, established,
				      StateData#state.connections)},
    {next_state, stream_established, NSD};

stream_established({invalid, From, To}, StateData) ->
    send_element(StateData,
		 {xmlelement,
		  "db:result",
		  [{"from", To},
		   {"to", From},
		   {"type", "invalid"}],
		  []}),
    LFrom = jlib:nameprep(From),
    LTo = jlib:nameprep(To),
    NSD = StateData#state{
	    connections = ?DICT:erase({LFrom, LTo},
				      StateData#state.connections)},
    {next_state, stream_established, NSD};

stream_established({xmlstreamend, _Name}, StateData) ->
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

handle_info({timeout, Timer, _}, _StateName,
	    #state{timer = Timer} = StateData) ->
    {stop, normal, StateData};

handle_info(_, StateName, StateData) ->
    {next_state, StateName, StateData}.


%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, _StateName, StateData) ->
    ?INFO_MSG("terminated: ~p", [Reason]),
    (StateData#state.sockmod):close(StateData#state.socket),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

send_text(StateData, Text) ->
    (StateData#state.sockmod):send(StateData#state.socket, Text).

send_element(StateData, El) ->
    send_text(StateData, xml:element_to_string(El)).


change_shaper(StateData, Host, JID) ->
    Shaper = acl:match_rule(Host, StateData#state.shaper, JID),
    ejabberd_receiver:change_shaper(StateData#state.receiver, Shaper).


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


get_cert_domains(Cert) ->
    {rdnSequence, Subject} =
	(Cert#'Certificate'.tbsCertificate)#'TBSCertificate'.subject,
    Extensions =
	(Cert#'Certificate'.tbsCertificate)#'TBSCertificate'.extensions,
    lists:flatmap(
      fun(#'AttributeTypeAndValue'{type = ?'id-at-commonName',
				   value = Val}) ->
	      case 'PKIX1Explicit88':decode(
				      'X520CommonName', Val) of
		  {ok, {_, D1}} ->
		      D = if
			      is_list(D1) -> D1;
			      is_binary(D1) -> binary_to_list(D1);
			      true -> error
			  end,
		      if
			  D /= error ->
			      case jlib:string_to_jid(D) of
				  #jid{luser = "",
				       lserver = LD,
				       lresource = ""} ->
				      [LD];
				  _ ->
				      []
			      end;
			  true ->
			      []
		      end;
		  _ ->
		      []
	      end;
	 (_) ->
	      []
      end, lists:flatten(Subject)) ++
	lists:flatmap(
	  fun(#'Extension'{extnID = ?'id-ce-subjectAltName',
			   extnValue = Val}) ->
		  BVal = if
			     is_list(Val) -> list_to_binary(Val);
			     is_binary(Val) -> Val;
			     true -> Val
			 end,
		  case 'PKIX1Implicit88':decode('SubjectAltName', BVal) of
		      {ok, SANs} ->
			  lists:flatmap(
			    fun({otherName,
				 #'AnotherName'{'type-id' = ?'id-on-xmppAddr',
						value = XmppAddr
					       }}) ->
				    case 'XmppAddr':decode(
					   'XmppAddr', XmppAddr) of
					{ok, D} when is_binary(D) ->
					    case jlib:string_to_jid(
						   binary_to_list(D)) of
						#jid{luser = "",
						     lserver = LD,
						     lresource = ""} ->
						    [LD];
						_ ->
						    []
					    end;
					_ ->
					    []
				    end;
			       (_) ->
				    []
			    end, SANs);
		      _ ->
			  []
		  end;
	     (_) ->
		  []
	  end, Extensions).



