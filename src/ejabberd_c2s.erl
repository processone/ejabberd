%%%----------------------------------------------------------------------
%%% File    : ejabberd_c2s.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve C2S connection
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   Process-one
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%                         
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_c2s).
-author('alexey@process-one.net').
-update_info({update, 0}).

-behaviour(gen_fsm).

%% External exports
-export([start/2,
	 start_link/2,
	 send_text/2,
	 send_element/2,
	 socket_type/0,
	 get_presence/1,
	 get_subscribed_and_online/1]).

%% gen_fsm callbacks
-export([init/1,
	 wait_for_stream/2,
	 wait_for_auth/2,
	 wait_for_feature_request/2,
	 wait_for_bind/2,
	 wait_for_session/2,
	 wait_for_sasl_response/2,
	 session_established/2,
	 handle_event/3,
	 handle_sync_event/4,
	 code_change/4,
	 handle_info/3,
	 terminate/3]).

-include("exmpp.hrl").

-include("ejabberd.hrl").
-include("mod_privacy.hrl").

-define(SETS, gb_sets).
-define(DICT, dict).

-record(state, {socket,
		sockmod,
		socket_monitor,
		streamid,
		sasl_state,
		access,
		shaper,
		zlib = false,
		tls = false,
		tls_required = false,
		tls_enabled = false,
		tls_options = [],
		authenticated = false,
		jid,
		user = "", server = ?MYNAME, resource = "",
		sid,
		pres_t = ?SETS:new(),
		pres_f = ?SETS:new(),
		pres_a = ?SETS:new(),
		pres_i = ?SETS:new(),
		pres_available = ?DICT:new(),
		pres_last, pres_pri,
		pres_timestamp,
		pres_invis = false,
		privacy_list = #userlist{},
		conn = unknown,
		auth_module = unknown,
		ip,
		lang}).

%-define(DBGFSM, true).

%-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
%-else.
%-define(FSMOPTS, []).
%-endif.

%% Module start with or without supervisor:
-ifdef(NO_TRANSIENT_SUPERVISORS).
-define(SUPERVISOR_START, gen_fsm:start(ejabberd_c2s, [SockData, Opts],
					?FSMOPTS)).
-else.
-define(SUPERVISOR_START, supervisor:start_child(ejabberd_c2s_sup,
						 [SockData, Opts])).
-endif.

%% This is the timeout to apply between event when starting a new
%% session:
-define(C2S_OPEN_TIMEOUT, 60000).
-define(C2S_HIBERNATE_TIMEOUT, 90000).

% These are the namespace already declared by the stream opening. This is
% used at serialization time.
-define(DEFAULT_NS, ['jabber:client']).
-define(PREFIXED_NS, [{?NS_XMPP, "stream"}]).

-define(STREAM_HEADER,
	"<?xml version='1.0'?>"
	"<stream:stream xmlns='jabber:client' "
	"xmlns:stream='http://etherx.jabber.org/streams' "
	"id='~s' from='~s'~s~s>"
       ).

-define(STREAM_TRAILER, "</stream:stream>").

-define(INVALID_NS_ERR,
	xml:element_to_string(?SERR_INVALID_NAMESPACE)).
-define(INVALID_XML_ERR,
	xml:element_to_string(?SERR_XML_NOT_WELL_FORMED)).
-define(HOST_UNKNOWN_ERR,
	xml:element_to_string(?SERR_HOST_UNKNOWN)).
-define(POLICY_VIOLATION_ERR(Lang, Text),
	xml:element_to_string(?SERRT_POLICY_VIOLATION(Lang, Text))).

% XXX OLD FORMAT
-define(NS_STREAM, "http://etherx.jabber.org/streams").
-define(NS_AUTH, "jabber:iq:auth").
-define(NS_FEATURE_COMPRESS, "http://jabber.org/features/compress").
-define(NS_PRIVACY, "jabber:iq:privacy").

-define(STREAM_ERROR(Condition),
  exmpp_xml:xmlel_to_xmlelement(exmpp_stream:error(Condition),
    [?NS_JABBER_CLIENT], [{?NS_XMPP, "stream"}])).
-define(SERR_XML_NOT_WELL_FORMED, ?STREAM_ERROR('xml-not-well-formed')).
-define(SERR_HOST_UNKNOWN, ?STREAM_ERROR('host-unknown')).
-define(SERR_INVALID_NAMESPACE, ?STREAM_ERROR('invalid-namespace')).

-define(STREAM_ERRORT(Condition, Lang, Text),
  exmpp_xml:xmlel_to_xmlelement(exmpp_stream:error(Condition, {Lang, Text}),
    [?NS_JABBER_CLIENT], [{?NS_XMPP, "stream"}])).
-define(SERRT_POLICY_VIOLATION(Lang, Text),
  ?STREAM_ERRORT('policy-violation', Lang, Text)).
-define(SERRT_CONFLICT(Lang, Text),
  ?STREAM_ERRORT('conflict', Lang, Text)).

-define(STANZA_ERROR(Condition),
  exmpp_xml:xmlel_to_xmlelement(exmpp_stanza:error(Condition),
    [?NS_JABBER_CLIENT], [{?NS_XMPP, "stream"}])).
-define(ERR_BAD_REQUEST, ?STANZA_ERROR('bad-request')).
-define(ERR_NOT_ALLOWED, ?STANZA_ERROR('not-allowed')).
-define(ERR_JID_MALFORMED, ?STANZA_ERROR('jid-malformed')).
-define(ERR_NOT_AUTHORIZED, ?STANZA_ERROR('not-authorized')).
-define(ERR_FEATURE_NOT_IMPLEMENTED, ?STANZA_ERROR('feature-not-implemented')).
-define(ERR_SERVICE_UNAVAILABLE, ?STANZA_ERROR('service-unavialable')).

-define(STANZA_ERRORT(Condition, Lang, Text),
  exmpp_xml:xmlel_to_xmlelement(exmpp_stanza:error(Condition, {Lang, Text}),
    [?NS_JABBER_CLIENT], [{?NS_XMPP, "stream"}])).
-define(ERR_AUTH_NO_RESOURCE_PROVIDED(Lang),
  ?STANZA_ERRORT('not-acceptable', Lang, "No resource provided")).

-record(iq, {id = "",
             type,
             xmlns = "",
             lang = "",
             sub_el}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(SockData, Opts) ->
    ?SUPERVISOR_START.

start_link(SockData, Opts) ->
    gen_fsm:start_link(ejabberd_c2s, [SockData, Opts], ?FSMOPTS).

socket_type() ->
    xml_stream.

%% Return Username, Resource and presence information
get_presence(FsmRef) ->
    gen_fsm:sync_send_all_state_event(FsmRef, {get_presence}, 1000).

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
    Access = case lists:keysearch(access, 1, Opts) of
		 {value, {_, A}} -> A;
		 _ -> all
	     end,
    Shaper = case lists:keysearch(shaper, 1, Opts) of
		 {value, {_, S}} -> S;
		 _ -> none
	     end,
    Zlib = lists:member(zlib, Opts),
    StartTLS = lists:member(starttls, Opts),
    StartTLSRequired = lists:member(starttls_required, Opts),
    TLSEnabled = lists:member(tls, Opts),
    TLS = StartTLS orelse StartTLSRequired orelse TLSEnabled,
    TLSOpts = lists:filter(fun({certfile, _}) -> true;
			      (_) -> false
			   end, Opts),
    IP = peerip(SockMod, Socket),
    %% Check if IP is blacklisted:
    case is_ip_blacklisted(IP) of
	true ->
	    ?INFO_MSG("Connection attempt from blacklisted IP: ~s",
		      [jlib:ip_to_list(IP)]),
	    {stop, normal};
	false ->
	    Socket1 =
		if
		    TLSEnabled ->
			SockMod:starttls(Socket, TLSOpts);
		    true ->
			Socket
		end,
	    SocketMonitor = SockMod:monitor(Socket1),
	    {ok, wait_for_stream, #state{socket         = Socket1,
					 sockmod        = SockMod,
					 socket_monitor = SocketMonitor,
					 zlib           = Zlib,
					 tls            = TLS,
					 tls_required   = StartTLSRequired,
					 tls_enabled    = TLSEnabled,
					 tls_options    = TLSOpts,
					 streamid       = new_id(),
					 access         = Access,
					 shaper         = Shaper,
					 ip             = IP},
	     ?C2S_OPEN_TIMEOUT}
    end.

%% Return list of all available resources of contacts,
%% in form [{JID, Caps}].
get_subscribed_and_online(FsmRef) ->
    gen_fsm:sync_send_all_state_event(
      FsmRef, get_subscribed_and_online, 1000).


%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

wait_for_stream({xmlstreamstart, #xmlel{ns = NS} = Opening}, StateData) ->
    DefaultLang = case ?MYLANG of
		      undefined ->
			  "en";
		      DL ->
			  DL
		  end,
    Header = exmpp_stream:opening_reply(Opening,
      StateData#state.streamid),
    Header1 = exmpp_stream:set_lang(Header, DefaultLang),
    case NS of
	?NS_XMPP ->
	    Server = exmpp_stringprep:nameprep(
	      exmpp_stream:get_receiving_entity(Opening)),
	    case lists:member(Server, ?MYHOSTS) of
		true ->
		    Lang = exmpp_stream:get_lang(Opening),
		    % OLD FORMAT: JID with empty strings.
		    change_shaper(StateData,
		      exmpp_jid:make_bare_jid(undefined, Server)),
		    case exmpp_stream:get_version(Opening) of
			{1, 0} ->
			    send_element(StateData, Header1),
			    case StateData#state.authenticated of
				false ->
				    SASLState =
					cyrsasl:server_new(
					  "jabber", Server, "", [],
					  fun(U) ->
						  ejabberd_auth:get_password_with_authmodule(
						    U, Server)
					  end,
					  fun(U, P) ->
						  ejabberd_auth:check_password_with_authmodule(
						    U, Server, P)
					  end),
				    SASL_Mechs = [exmpp_server_sasl:feature(
					cyrsasl:listmech(Server))],
				    SockMod =
					(StateData#state.sockmod):get_sockmod(
					  StateData#state.socket),
				    Zlib = StateData#state.zlib,
				    CompressFeature =
					case Zlib andalso
					    (SockMod == gen_tcp) of
					    true ->
						[exmpp_server_compression:feature(["zlib"])];
					    _ ->
						[]
					end,
				    TLS = StateData#state.tls,
				    TLSEnabled = StateData#state.tls_enabled,
				    TLSRequired = StateData#state.tls_required,
				    TLSFeature =
					case (TLS == true) andalso
					    (TLSEnabled == false) andalso
					    (SockMod == gen_tcp) of
					    true ->
						[exmpp_server_tls:feature(TLSRequired)];
					    false ->
						[]
					end,
				    send_element(StateData,
				      exmpp_stream:features(
					TLSFeature ++
					CompressFeature ++
					SASL_Mechs ++
					ejabberd_hooks:run_fold(
					  c2s_stream_features,
					  Server,
					  [], []))),
				    fsm_next_state(wait_for_feature_request,
					       StateData#state{
						 server = Server,
						 sasl_state = SASLState,
						 lang = Lang});
				_ ->
				    case StateData#state.resource of
					"" ->
					    send_element(
					      StateData,
					      exmpp_stream:features([
						  exmpp_server_binding:feature(),
						  exmpp_server_session:feature()
						])),
					    fsm_next_state(wait_for_bind,
						       StateData#state{
							 server = Server,
							 lang = Lang});
					_ ->
					    send_element(
					      StateData,
					      exmpp_stream:features([])),
					    fsm_next_state(wait_for_session,
						       StateData#state{
							 server = Server,
							 lang = Lang})
				    end
			    end;
			_ ->
			    if
				(not StateData#state.tls_enabled) and
				StateData#state.tls_required ->
				    send_element(StateData,
				      exmpp_xml:append_child(Header1,
					exmpp_stream:error('policy-violation',
					  "en", "Use of STARTTLS required"))),
				    {stop, normal, StateData};
				true ->
				    send_element(StateData, Header1),
				    fsm_next_state(wait_for_auth,
						   StateData#state{
						     server = Server,
						     lang = Lang})
			    end
		    end;
		_ ->
		    Header2 = exmpp_stream:set_initiating_entity(Header1,
		      ?MYNAME),
		    send_element(StateData, exmpp_xml:append_child(Header2,
			exmpp_stream:error('host-unknown'))),
		    {stop, normal, StateData}
	    end;
	_ ->
	    Header2 = exmpp_stream:set_initiating_entity(Header1, ?MYNAME),
	    send_element(StateData, exmpp_xml:append_child(Header2,
		exmpp_stream:error('invalid-namespace'))),
	    {stop, normal, StateData}
    end;

wait_for_stream(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_stream({xmlstreamelement, _}, StateData) ->
    send_element(StateData, exmpp_stream:error('xml-not-well-formed')),
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

wait_for_stream({xmlstreamend, _}, StateData) ->
    send_element(StateData, exmpp_stream:error('xml-not-well-formed')),
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

wait_for_stream({xmlstreamerror, _}, StateData) ->
    Header = exmpp_stream:opening_reply(?MYNAME, 'jabber:client', "1.0",
      "none"),
    Header1 = exmpp_xml:append_child(Header,
      exmpp_stream:error('xml-not-well-formed')),
    send_element(StateData, Header1),
    {stop, normal, StateData};

wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_auth({xmlstreamelement, El}, StateData) ->
    case is_auth_packet(El) of
	{auth, _ID, get, {_U, _, _, _}} ->
	    Fields = case ejabberd_auth:plain_password_required(
	      StateData#state.server) of
		false -> both;
		true  -> plain
	    end,
	    send_element(StateData,
	      exmpp_server_legacy_auth:fields(El, Fields)),
	    fsm_next_state(wait_for_auth, StateData);
	{auth, _ID, set, {_U, _P, _D, ""}} ->
	    Err = exmpp_stanza:error('not-acceptable',
	      {"en", "No resource provided"}),
	    send_element(StateData, exmpp_iq:error(El, Err)),
	    fsm_next_state(wait_for_auth, StateData);
	{auth, _ID, set, {U, P, D, R}} ->
	    try
		JID = exmpp_jid:make_jid(U, StateData#state.server, R),
		case acl:match_rule(StateData#state.server,
		  StateData#state.access, JID) of
		    allow ->
			case ejabberd_auth:check_password_with_authmodule(
			  U, StateData#state.server, P,
			  StateData#state.streamid, D) of
			    {true, AuthModule} ->
				?INFO_MSG(
				  "(~w) Accepted legacy authentication for ~s",
				  [StateData#state.socket,
				    exmpp_jid:jid_to_string(JID)]),
				SID = {now(), self()},
				Conn = get_conn_type(StateData),
				Info = [{ip, StateData#state.ip}, {conn, Conn},
				  {auth_module, AuthModule}],
				ejabberd_sm:open_session(
				  SID, U, StateData#state.server, R, Info),
				Res = exmpp_server_legacy_auth:success(El),
				send_element(StateData, Res),
				change_shaper(StateData, JID),
				{Fs, Ts} = ejabberd_hooks:run_fold(
				  roster_get_subscription_lists,
				  StateData#state.server,
				  {[], []},
				  [U, StateData#state.server]),
				LJID = {JID#jid.lnode, JID#jid.ldomain,
				  undefined},
				Fs1 = [LJID | Fs],
				Ts1 = [LJID | Ts],
				PrivList = ejabberd_hooks:run_fold(
				  privacy_get_user_list, StateData#state.server,
				  #userlist{},
				  [U, StateData#state.server]),
				fsm_next_state(session_established,
				  StateData#state{
				    user = U,
				    resource = R,
				    jid = JID,
				    sid = SID,
				    conn = Conn,
				    auth_module = AuthModule,
				    pres_f = ?SETS:from_list(Fs1),
				    pres_t = ?SETS:from_list(Ts1),
				    privacy_list = PrivList});
			    _ ->
				?INFO_MSG(
				  "(~w) Failed legacy authentication for ~s",
				  [StateData#state.socket,
				    exmpp_jid:jid_to_string(JID)]),
				Err = exmpp_stanza:error('not-authorized'),
				Res = exmpp_iq:error_without_original(El, Err),
				send_element(StateData, Res),
				fsm_next_state(wait_for_auth, StateData)
			end;
		    _ ->
			?INFO_MSG(
			  "(~w) Forbidden legacy authentication for ~s",
			  [StateData#state.socket,
			    exmpp_jid:jid_to_string(JID)]),
			Err = exmpp_stanza:error('not-allowed'),
			Res = exmpp_iq:error_without_original(El, Err),
			send_element(StateData, Res),
			fsm_next_state(wait_for_auth, StateData)
		end
	    catch
		throw:_Exception ->
		    ?INFO_MSG(
		      "(~w) Forbidden legacy authentication for "
		      "username '~s' with resource '~s'",
		      [StateData#state.socket, U, R]),
		    Err1 = exmpp_stanza:error('jid-malformed'),
		    Res1 = exmpp_iq:error_without_original(El, Err1),
		    send_element(StateData, Res1),
		    fsm_next_state(wait_for_auth, StateData)
	    end;
	_ ->
	    process_unauthenticated_stanza(StateData, El),
	    fsm_next_state(wait_for_auth, StateData)
    end;

wait_for_auth(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_auth({xmlstreamend, _Name}, StateData) ->
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

wait_for_auth({xmlstreamerror, _}, StateData) ->
    send_element(StateData, exmpp_stream:error('xml-not-well-formed')),
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

wait_for_auth(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_feature_request({xmlstreamelement, #xmlel{ns = NS, name = Name} = El},
  StateData) ->
    Zlib = StateData#state.zlib,
    TLS = StateData#state.tls,
    TLSEnabled = StateData#state.tls_enabled,
    TLSRequired = StateData#state.tls_required,
    SockMod = (StateData#state.sockmod):get_sockmod(StateData#state.socket),
    case {NS, Name} of
	{?NS_SASL, 'auth'} when not ((SockMod == gen_tcp) and TLSRequired) ->
	    {auth, Mech, ClientIn} = exmpp_server_sasl:next_step(El),
	    case cyrsasl:server_start(StateData#state.sasl_state,
				      Mech,
				      ClientIn) of
		{ok, Props} ->
		    (StateData#state.sockmod):reset_stream(
		      StateData#state.socket),
		    send_element(StateData, exmpp_server_sasl:success()),
		    U = proplists:get_value(username, Props),
		    ?INFO_MSG("(~w) Accepted authentication for ~s",
			      [StateData#state.socket, U]),
		    fsm_next_state(wait_for_stream,
				   StateData#state{
				     streamid = new_id(),
				     authenticated = true,
				     user = U });
		{continue, ServerOut, NewSASLState} ->
		    send_element(StateData,
		      exmpp_server_sasl:challenge(ServerOut)),
		    fsm_next_state(wait_for_sasl_response,
				   StateData#state{
				     sasl_state = NewSASLState});
		{error, Error, Username} ->
		    ?INFO_MSG(
		       "(~w) Failed authentication for ~s@~s",
		       [StateData#state.socket,
			Username, StateData#state.server]),
		    % XXX OLD FORMAT: list_to_atom(Error).
		    send_element(StateData,
		      exmpp_server_sasl:failure(list_to_atom(Error))),
		    {next_state, wait_for_feature_request, StateData,
		     ?C2S_OPEN_TIMEOUT};
		{error, Error} ->
		    % XXX OLD FORMAT: list_to_atom(Error).
		    send_element(StateData,
		      exmpp_server_sasl:failure(list_to_atom(Error))),
		    fsm_next_state(wait_for_feature_request, StateData)
	    end;
	{?NS_TLS, 'starttls'} when TLS == true,
				   TLSEnabled == false,
				   SockMod == gen_tcp ->
	    TLSOpts = case ejabberd_config:get_local_option(
			     {domain_certfile, StateData#state.server}) of
			  undefined ->
			      StateData#state.tls_options;
			  CertFile ->
			      [{certfile, CertFile} |
			       lists:keydelete(
				 certfile, 1, StateData#state.tls_options)]
		      end,
	    Socket = StateData#state.socket,
	    Proceed = exmpp_xml:document_fragment_to_list(
	      exmpp_server_tls:proceed(), ?DEFAULT_NS, ?PREFIXED_NS),
	    TLSSocket = (StateData#state.sockmod):starttls(
			  Socket, TLSOpts,
			  Proceed),
	    fsm_next_state(wait_for_stream,
			   StateData#state{socket = TLSSocket,
					   streamid = new_id(),
					   tls_enabled = true
					  });
	{?NS_COMPRESS, 'compress'} when Zlib == true,
					SockMod == gen_tcp ->
	    case exmpp_server_compression:selected_method(El) of
		undefined ->
		    send_element(StateData,
		      exmpp_server_compression:failure('steup-failed')),
		    fsm_next_state(wait_for_feature_request, StateData);
		"zlib" ->
		    Socket = StateData#state.socket,
		    ZlibSocket = (StateData#state.sockmod):compress(
				   Socket,
				   exmpp_server_compression:compressed()),
		    fsm_next_state(wait_for_stream,
		     StateData#state{socket = ZlibSocket,
				     streamid = new_id()
				    });
		_ ->
		    send_element(StateData,
		      exmpp_server_compression:failure('unsupported-method')),
		    fsm_next_state(wait_for_feature_request,
				   StateData)
	    end;
	_ ->
	    if
		(SockMod == gen_tcp) and TLSRequired ->
		    send_element(StateData, exmpp_stream:error(
			'policy-violation', "en", "Use of STARTTLS required")),
		    send_element(StateData, exmpp_stream:closing()),
		    {stop, normal, StateData};
		true ->
		    process_unauthenticated_stanza(StateData, El),
		    fsm_next_state(wait_for_feature_request, StateData)
	    end
    end;

wait_for_feature_request(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_feature_request({xmlstreamend, _Name}, StateData) ->
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

wait_for_feature_request({xmlstreamerror, _}, StateData) ->
    send_element(StateData, exmpp_stream:error('xml-not-well-formed')),
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

wait_for_feature_request(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_sasl_response({xmlstreamelement, #xmlel{ns = NS, name = Name} = El},
  StateData) ->
    case {NS, Name} of
	{?NS_SASL, 'response'} ->
	    {response, ClientIn} = exmpp_server_sasl:next_step(El),
	    case cyrsasl:server_step(StateData#state.sasl_state,
				     ClientIn) of
		{ok, Props} ->
		    (StateData#state.sockmod):reset_stream(
		      StateData#state.socket),
		    send_element(StateData, exmpp_server_sasl:success()),
		    U = proplists:get_value(username, Props),
		    AuthModule = proplists:get_value(auth_module, Props),
		    ?INFO_MSG("(~w) Accepted authentication for ~s",
			      [StateData#state.socket, U]),
		    fsm_next_state(wait_for_stream,
				   StateData#state{
				     streamid = new_id(),
				     authenticated = true,
				     auth_module = AuthModule,
				     user = U});
		{continue, ServerOut, NewSASLState} ->
		    send_element(StateData,
		      exmpp_server_sasl:challenge(ServerOut)),
		    fsm_next_state(wait_for_sasl_response,
		     StateData#state{sasl_state = NewSASLState});
		{error, Error, Username} ->
		    ?INFO_MSG(
		       "(~w) Failed authentication for ~s@~s",
		       [StateData#state.socket,
			Username, StateData#state.server]),
		    % XXX OLD FORMAT: list_to_atom(Error).
		    send_element(StateData,
		      exmpp_server_sasl:failure(list_to_atom(Error))),
		    fsm_next_state(wait_for_feature_request, StateData);
		{error, Error} ->
		    % XXX OLD FORMAT: list_to_atom(Error).
		    send_element(StateData,
		      exmpp_server_sasl:failure(list_to_atom(Error))),
		    fsm_next_state(wait_for_feature_request, StateData)
	    end;
	_ ->
	    process_unauthenticated_stanza(StateData, El),
	    fsm_next_state(wait_for_feature_request, StateData)
    end;

wait_for_sasl_response(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_sasl_response({xmlstreamend, _Name}, StateData) ->
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

wait_for_sasl_response({xmlstreamerror, _}, StateData) ->
    send_element(StateData, exmpp_stream:error('xml-not-well-formed')),
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

wait_for_sasl_response(closed, StateData) ->
    {stop, normal, StateData}.



wait_for_bind({xmlstreamelement, El}, StateData) ->
    try
	U = StateData#state.user,
	R = case exmpp_server_binding:wished_resource(El) of
	    undefined ->
		lists:concat([randoms:get_string() | tuple_to_list(now())]);
	    Resource ->
		Resource
	end,
	JID = exmpp_jid:make_jid(U, StateData#state.server, R),
	Res = exmpp_server_binding:bind(El, JID),
	send_element(StateData, Res),
	fsm_next_state(wait_for_session,
		       StateData#state{resource = R, jid = JID})
    catch
	throw:{stringprep, resourceprep, _, _} ->
	    Err = exmpp_server_binding:error(El, 'bad-request'),
	    send_element(StateData, Err),
	    fsm_next_state(wait_for_bind, StateData);
	throw:_Exception ->
	    fsm_next_state(wait_for_bind, StateData)
    end;

wait_for_bind(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_bind({xmlstreamend, _Name}, StateData) ->
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

wait_for_bind({xmlstreamerror, _}, StateData) ->
    send_element(StateData, exmpp_stream:error('xml-not-well-formed')),
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

wait_for_bind(closed, StateData) ->
    {stop, normal, StateData}.



wait_for_session({xmlstreamelement, El}, StateData) ->
    try
	U = StateData#state.user,
	R = StateData#state.resource,
	JID = StateData#state.jid,
	exmpp_server_session:want_establishment(El),
	case acl:match_rule(StateData#state.server,
			    StateData#state.access, JID) of
	    allow ->
		?INFO_MSG("(~w) Opened session for ~s",
			  [StateData#state.socket,
			   exmpp_jid:jid_to_string(JID)]),
		SID = {now(), self()},
		Conn = get_conn_type(StateData),
		Info = [{ip, StateData#state.ip}, {conn, Conn},
			{auth_module, StateData#state.auth_module}],
		ejabberd_sm:open_session(
		  SID, U, StateData#state.server, R, Info),
		Res = exmpp_server_session:establish(El),
		send_element(StateData, Res),
		change_shaper(StateData, JID),
		{Fs, Ts} = ejabberd_hooks:run_fold(
			     roster_get_subscription_lists,
			     StateData#state.server,
			     {[], []},
			     [U, StateData#state.server]),
		LJID = {JID#jid.lnode, JID#jid.ldomain, undefined},
		Fs1 = [LJID | Fs],
		Ts1 = [LJID | Ts],
		PrivList =
		    ejabberd_hooks:run_fold(
		      privacy_get_user_list, StateData#state.server,
		      #userlist{},
		      [U, StateData#state.server]),
		fsm_next_state(session_established,
			       StateData#state{
				 sid = SID,
				 conn = Conn,
				 pres_f = ?SETS:from_list(Fs1),
				 pres_t = ?SETS:from_list(Ts1),
				 privacy_list = PrivList});
	    _ ->
		% OLD FORMAT: Jid may use 'undefined' instead of empty
		% strings.
		ejabberd_hooks:run(forbidden_session_hook, 
				   StateData#state.server, [JID]),
		?INFO_MSG("(~w) Forbidden session for ~s",
			  [StateData#state.socket,
			   exmpp_jid:jid_to_string(JID)]),
		Err = exmpp_server_session:error(El, 'not-allowed'),
		send_element(StateData, Err),
		fsm_next_state(wait_for_session, StateData)
	end
    catch
	throw:_Exception ->
	    fsm_next_state(wait_for_session, StateData)
    end;

wait_for_session(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_session({xmlstreamend, _Name}, StateData) ->
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

wait_for_session({xmlstreamerror, _}, StateData) ->
    send_element(StateData, exmpp_stream:error('xml-not-well-formed')),
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

wait_for_session(closed, StateData) ->
    {stop, normal, StateData}.


session_established({xmlstreamelement, El}, StateData) ->
    try
	User = StateData#state.user,
	Server = StateData#state.server,
	% TODO: check 'from' attribute in stanza
	FromJID = StateData#state.jid,
	To = exmpp_stanza:get_recipient(El),
	ToJID = case To of
		    undefined ->
			exmpp_jid:make_bare_jid(User, Server);
		    _ ->
			exmpp_jid:string_to_jid(To)
		end,
	NewEl = case exmpp_stanza:get_lang(El) of
		    undefined ->
			case StateData#state.lang of
			    undefined -> El;
			    Lang ->
				exmpp_stanza:set_lang(El, Lang)
			end;
		    _ ->
			El
		end,
	NewState = case El of
	    #xmlel{ns = ?NS_JABBER_CLIENT, name = 'presence'} ->
		% XXX OLD FORMAT: NewEl.
		PresenceElOld = ejabberd_hooks:run_fold(
			       c2s_update_presence,
			       Server,
			       exmpp_xml:xmlel_to_xmlelement(NewEl,
				 ?DEFAULT_NS, ?PREFIXED_NS),
			       [User, Server]),
		PresenceEl = exmpp_xml:xmlelement_to_xmlel(PresenceElOld,
		  ?DEFAULT_NS, ?PREFIXED_NS),
		% XXX OLD FORMAT: PresenceElOld, *JID.
                FromJIDOld = exmpp_jid:to_ejabberd_jid(FromJID),
                ToJIDOld = exmpp_jid:to_ejabberd_jid(ToJID),
		ejabberd_hooks:run(
		  user_send_packet,
		  Server,
		  [FromJIDOld, ToJIDOld, PresenceElOld]),
		case ToJID of
		    #jid{node = User,
			 domain = Server,
			 resource = undefined} ->
			?DEBUG("presence_update(~p,~n\t~p,~n\t~p)",
			       [FromJID, PresenceEl, StateData]),
			% XXX OLD FORMAT: PresenceElOld.
			presence_update(FromJIDOld, PresenceElOld,
					StateData);
		    _ ->
			% XXX OLD FORMAT: PresenceElOld.
			presence_track(FromJIDOld, ToJIDOld, PresenceElOld,
				       StateData)
		end;
	    #xmlel{ns = ?NS_JABBER_CLIENT, name = 'iq'} ->
                % XXX OLD FORMAT: JIDs.
                FromJIDOld = exmpp_jid:to_ejabberd_jid(FromJID),
                ToJIDOld = exmpp_jid:to_ejabberd_jid(ToJID),
		case exmpp_iq:get_payload(El) of
		    #xmlel{ns = ?NS_PRIVACY} ->
			% XXX OLD FORMAT: IQ was #iq.
			IQ_Record = exmpp_iq:to_ejabberd_iq(El),
			process_privacy_iq(
			  FromJIDOld, ToJIDOld, IQ_Record, StateData);
		    _ ->
			% XXX OLD FORMAT: NewElOld.
			NewElOld = exmpp_xml:xmlel_to_xmlelement(NewEl,
			  ?DEFAULT_NS, ?PREFIXED_NS),
			ejabberd_hooks:run(
			  user_send_packet,
			  Server,
			  [FromJIDOld, ToJIDOld, NewElOld]),
			% XXX OLD FORMAT: NewElOld.
			ejabberd_router:route(
			  FromJIDOld, ToJIDOld, NewElOld),
			StateData
		end;
	    #xmlel{ns = ?NS_JABBER_CLIENT, name = 'message'} ->
		% XXX OLD FORMAT: NewElOld, JIDs.
		NewElOld = exmpp_xml:xmlel_to_xmlelement(NewEl,
		  ?DEFAULT_NS, ?PREFIXED_NS),
                FromJIDOld = exmpp_jid:to_ejabberd_jid(FromJID),
                ToJIDOld = exmpp_jid:to_ejabberd_jid(ToJID),
		ejabberd_hooks:run(user_send_packet,
				   Server,
				   [FromJIDOld, ToJIDOld, NewElOld]),
		% XXX OLD FORMAT: NewElOld.
		ejabberd_router:route(FromJIDOld, ToJIDOld, NewElOld),
		StateData;
	    _ ->
		StateData
	end,
	% XXX OLD FORMAT: El.
	ElOld = exmpp_xml:xmlel_to_xmlelement(El,
	  ?DEFAULT_NS, ?PREFIXED_NS),
	ejabberd_hooks:run(c2s_loop_debug, [{xmlstreamelement, ElOld}]),
	fsm_next_state(session_established, NewState)
    catch
	throw:{stringprep, _, _, _} ->
	    case exmpp_stanza:get_type(El) of
		"error" ->
		    ok;
		"result" ->
		    ok;
		_ ->
		    Err = exmpp_stanza:reply_with_error(El, 'jid-malformed'),
		    send_element(StateData, Err)
	    end,
	    % XXX OLD FORMAT: ElOld1.
	    ElOld1 = exmpp_xml:xmlel_to_xmlelement(El,
	      ?DEFAULT_NS, ?PREFIXED_NS),
	    ejabberd_hooks:run(c2s_loop_debug, [{xmlstreamelement, ElOld1}]),
	    fsm_next_state(session_established, StateData);
	throw:Exception ->
	    io:format("SESSION ESTABLISHED: Exception=~p~n", [Exception]),
	    fsm_next_state(session_established, StateData)
    end;

%% We hibernate the process to reduce memory consumption after a
%% configurable activity timeout
session_established(timeout, StateData) ->
    %% TODO: Options must be stored in state:
    Options = [],
    proc_lib:hibernate(gen_fsm, enter_loop,
		       [?MODULE, Options, session_established, StateData]),
    fsm_next_state(session_established, StateData);

session_established({xmlstreamend, _Name}, StateData) ->
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

session_established({xmlstreamerror, _}, StateData) ->
    send_element(StateData, exmpp_stream:error('xml-not-well-formed')),
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

session_established(closed, StateData) ->
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
    fsm_next_state(StateName, StateData).

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
handle_sync_event({get_presence}, _From, StateName, StateData) ->
    User = StateData#state.user,
    PresLast = StateData#state.pres_last,

    Show = get_showtag(PresLast),
    Status = get_statustag(PresLast),
    Resource = StateData#state.resource,

    Reply = {User, Resource, Show, Status},
    fsm_reply(Reply, StateName, StateData);

handle_sync_event(get_subscribed_and_online, _From, StateName, StateData) ->
    % XXX OLD FORMAT: This function needs update. User has the form {N, D, R}.
    Subscribed = StateData#state.pres_f,
    Online = StateData#state.pres_available,
    Pred = fun(User, _Caps) ->
		   ?SETS:is_element(jlib:jid_remove_resource(User),
				    Subscribed) orelse
		       ?SETS:is_element(User, Subscribed)
	   end,
    SubscribedAndOnline = ?DICT:filter(Pred, Online),
    {reply, ?DICT:to_list(SubscribedAndOnline), StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    fsm_reply(Reply, StateName, StateData).

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info({send_text, Text}, StateName, StateData) ->
    % XXX OLD FORMAT: This clause should be removed.
    send_text(StateData, Text),
    ejabberd_hooks:run(c2s_loop_debug, [Text]),
    fsm_next_state(StateName, StateData);
handle_info(replaced, _StateName, StateData) ->
    send_element(StateData, exmpp_stream:error('conflict')),
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData#state{authenticated = replaced}};
handle_info({route, From, To, Packet}, StateName, StateData) ->
    {xmlelement, Name, Attrs, Els} = Packet,
    {Pass, NewAttrs, NewState} =
	case Name of
	    "presence" ->
		case xml:get_attr_s("type", Attrs) of
		    "probe" ->
			LFrom = jlib:jid_tolower(From),
			LBFrom = jlib:jid_remove_resource(LFrom),
			NewStateData =
			    case ?SETS:is_element(
				    LFrom, StateData#state.pres_a) orelse
				?SETS:is_element(
				   LBFrom, StateData#state.pres_a) of
				true ->
				    StateData;
				false ->
				    case ?SETS:is_element(
					    LFrom, StateData#state.pres_f) of
					true ->
					    A = ?SETS:add_element(
						   LFrom,
						   StateData#state.pres_a),
					    StateData#state{pres_a = A};
					false ->
					    case ?SETS:is_element(
						    LBFrom, StateData#state.pres_f) of
						true ->
						    A = ?SETS:add_element(
							   LBFrom,
							   StateData#state.pres_a),
						    StateData#state{pres_a = A};
						false ->
						    StateData
					    end
				    end
			    end,
			process_presence_probe(From, To, NewStateData),
			{false, Attrs, NewStateData};
		    "error" ->
			NewA = remove_element(jlib:jid_tolower(From),
					      StateData#state.pres_a),
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
			case ejabberd_hooks:run_fold(
			       privacy_check_packet, StateData#state.server,
			       allow,
			       [StateData#state.user,
				StateData#state.server,
				StateData#state.privacy_list,
				{From, To, Packet},
				in]) of
			    allow ->
				LFrom = jlib:jid_tolower(From),
				LBFrom = jlib:jid_remove_resource(LFrom),
				%% Note contact availability
				Caps = mod_caps:read_caps(Els),
				mod_caps:note_caps(StateData#state.server, From, Caps),
				NewAvailable = case xml:get_attr_s("type", Attrs) of
						   "unavailable" ->
						       ?DICT:erase(LFrom, StateData#state.pres_available);
						   _ ->
						       ?DICT:store(LFrom, Caps, StateData#state.pres_available)
					       end,
				NewStateData = StateData#state{pres_available = NewAvailable},
				case ?SETS:is_element(
					LFrom, NewStateData#state.pres_a) orelse
				    ?SETS:is_element(
				       LBFrom, NewStateData#state.pres_a) of
				    true ->
					{true, Attrs, NewStateData};
				    false ->
					case ?SETS:is_element(
						LFrom, NewStateData#state.pres_f) of
					    true ->
						A = ?SETS:add_element(
						       LFrom,
						       NewStateData#state.pres_a),
						{true, Attrs,
						 NewStateData#state{pres_a = A}};
					    false ->
						case ?SETS:is_element(
							LBFrom, NewStateData#state.pres_f) of
						    true ->
							A = ?SETS:add_element(
							       LBFrom,
							       NewStateData#state.pres_a),
							{true, Attrs,
							 NewStateData#state{pres_a = A}};
						    false ->
							{true, Attrs, NewStateData}
						end
					end
				end;
			    deny ->
				{false, Attrs, StateData}
			end
		end;
	    "broadcast" ->
		?DEBUG("broadcast~n~p~n", [Els]),
		case Els of
		    [{item, IJID, ISubscription}] ->
			{false, Attrs,
			 roster_change(IJID, ISubscription,
				       StateData)};
		    [{exit, Reason}] ->
			{exit, Attrs, Reason};
		    [{privacy_list, PrivList, PrivListName}] ->
			case ejabberd_hooks:run_fold(
			       privacy_updated_list, StateData#state.server,
			       false,
			       [StateData#state.privacy_list,
				PrivList]) of
			    false ->
				{false, Attrs, StateData};
			    NewPL ->
				PrivPushIQ =
				    #iq{type = set, xmlns = ?NS_PRIVACY,
					id = "push",
					sub_el = [{xmlelement, "query",
						   [{"xmlns", ?NS_PRIVACY}],
						   [{xmlelement, "list",
						     [{"name", PrivListName}],
						     []}]}]},
				PrivPushEl =
				    jlib:replace_from_to(
				      jlib:jid_remove_resource(
					StateData#state.jid),
				      StateData#state.jid,
				      jlib:iq_to_xml(PrivPushIQ)),
				send_element(StateData, PrivPushEl),
				{false, Attrs, StateData#state{privacy_list = NewPL}}
			end;
		    _ ->
			{false, Attrs, StateData}
		end;
	    "iq" ->
		IQ = jlib:iq_query_info(Packet),
		case IQ of
		    #iq{xmlns = ?NS_VCARD} ->
			Host = StateData#state.server,
			case ets:lookup(sm_iqtable, {?NS_VCARD, Host}) of
			    [{_, Module, Function, Opts}] ->
				gen_iq_handler:handle(Host, Module, Function, Opts,
						      From, To, IQ);
			    [] ->
				Err = jlib:make_error_reply(
					Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
				ejabberd_router:route(To, From, Err)
			end,
			{false, Attrs, StateData};
		    #iq{} ->
			case ejabberd_hooks:run_fold(
			       privacy_check_packet, StateData#state.server,
			       allow,
			       [StateData#state.user,
				StateData#state.server,
				StateData#state.privacy_list,
				{From, To, Packet},
				in]) of
			    allow ->
				{true, Attrs, StateData};
			    deny ->
				Err = jlib:make_error_reply(
					Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
				ejabberd_router:route(To, From, Err),
				{false, Attrs, StateData}
			end;
		    _ ->
			{true, Attrs, StateData}
		end;
	    "message" ->
		case ejabberd_hooks:run_fold(
		       privacy_check_packet, StateData#state.server,
		       allow,
		       [StateData#state.user,
			StateData#state.server,
			StateData#state.privacy_list,
			{From, To, Packet},
			in]) of
		    allow ->
			{true, Attrs, StateData};
		    deny ->
			{false, Attrs, StateData}
		end;
	    _ ->
		{true, Attrs, StateData}
	end,
    if
	Pass == exit ->
	    catch send_text(StateData, ?STREAM_TRAILER),
	    {stop, normal, StateData};
	Pass ->
	    Attrs2 = jlib:replace_from_to_attrs(jlib:jid_to_string(From),
						jlib:jid_to_string(To),
						NewAttrs),
	    FixedPacket = {xmlelement, Name, Attrs2, Els},
	    Text = xml:element_to_string(FixedPacket),
	    send_text(StateData, Text),
	    ejabberd_hooks:run(user_receive_packet,
			       StateData#state.server,
			       [StateData#state.jid, From, To, FixedPacket]),
	    ejabberd_hooks:run(c2s_loop_debug, [{route, From, To, Packet}]),
	    fsm_next_state(StateName, NewState);
	true ->
	    ejabberd_hooks:run(c2s_loop_debug, [{route, From, To, Packet}]),
	    fsm_next_state(StateName, NewState)
    end;
handle_info({'DOWN', Monitor, _Type, _Object, _Info}, _StateName, StateData)
  when Monitor == StateData#state.socket_monitor ->
    {stop, normal, StateData};
handle_info(Info, StateName, StateData) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    fsm_next_state(StateName, StateData).

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, StateName, StateData) ->
    case StateName of
	session_established ->
	    case StateData#state.authenticated of
		replaced ->
		    ?INFO_MSG("(~w) Replaced session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(StateData#state.jid)]),
		    From = StateData#state.jid,
		    Packet = {xmlelement, "presence",
			      [{"type", "unavailable"}],
			      [{xmlelement, "status", [],
				[{xmlcdata, "Replaced by new connection"}]}]},
		    ejabberd_sm:close_session_unset_presence(
		      StateData#state.sid,
		      StateData#state.user,
		      StateData#state.server,
		      StateData#state.resource,
		      "Replaced by new connection"),
		    presence_broadcast(
		      StateData, From, StateData#state.pres_a, Packet),
		    presence_broadcast(
		      StateData, From, StateData#state.pres_i, Packet);
		_ ->
		    ?INFO_MSG("(~w) Close session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(StateData#state.jid)]),

		    EmptySet = ?SETS:new(),
		    case StateData of
			#state{pres_last = undefined,
			       pres_a = EmptySet,
			       pres_i = EmptySet,
			       pres_invis = false} ->
			    ejabberd_sm:close_session(StateData#state.sid,
						      StateData#state.user,
						      StateData#state.server,
						      StateData#state.resource);
			_ ->
			    From = StateData#state.jid,
			    Packet = {xmlelement, "presence",
				      [{"type", "unavailable"}], []},
			    ejabberd_sm:close_session_unset_presence(
			      StateData#state.sid,
			      StateData#state.user,
			      StateData#state.server,
			      StateData#state.resource,
			      ""),
			    presence_broadcast(
			      StateData, From, StateData#state.pres_a, Packet),
			    presence_broadcast(
			      StateData, From, StateData#state.pres_i, Packet)
		    end
	    end;
	_ ->
	    ok
    end,
    (StateData#state.sockmod):close(StateData#state.socket),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

change_shaper(StateData, JID) ->
    Shaper = acl:match_rule(StateData#state.server,
			    StateData#state.shaper, JID),
    (StateData#state.sockmod):change_shaper(StateData#state.socket, Shaper).

send_text(StateData, Text) ->
    ?DEBUG("Send XML on stream = ~p", [lists:flatten(Text)]),
    (StateData#state.sockmod):send(StateData#state.socket, Text).

send_element(StateData, El) ->
    send_text(StateData, xml:element_to_string(El)).


new_id() ->
    randoms:get_string().


is_auth_packet(El) ->
    case jlib:iq_query_info(El) of
	#iq{id = ID, type = Type, xmlns = ?NS_AUTH, sub_el = SubEl} ->
	    {xmlelement, _, _, Els} = SubEl,
	    {auth, ID, Type,
	     get_auth_tags(Els, "", "", "", "")};
	_ ->
	    false
    end.


get_auth_tags([{xmlelement, Name, _Attrs, Els}| L], U, P, D, R) ->
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

get_conn_type(StateData) ->
    case (StateData#state.sockmod):get_sockmod(StateData#state.socket) of
    gen_tcp -> c2s;
    tls -> c2s_tls;
    ejabberd_zlib -> c2s_compressed;
    ejabberd_http_poll -> http_poll;
    ejabberd_http_bind -> http_bind;
    _ -> unknown
    end.

process_presence_probe(From, To, StateData) ->
    LFrom = jlib:jid_tolower(From),
    LBFrom = setelement(3, LFrom, ""),
    case StateData#state.pres_last of
	undefined ->
	    ok;
	_ ->
	    Cond1 = (not StateData#state.pres_invis)
		andalso (?SETS:is_element(LFrom, StateData#state.pres_f)
			 orelse
			 ((LFrom /= LBFrom) andalso
			  ?SETS:is_element(LBFrom, StateData#state.pres_f)))
		andalso (not
			 (?SETS:is_element(LFrom, StateData#state.pres_i)
			  orelse
			  ((LFrom /= LBFrom) andalso
			   ?SETS:is_element(LBFrom, StateData#state.pres_i)))),
	    Cond2 = StateData#state.pres_invis
		andalso ?SETS:is_element(LFrom, StateData#state.pres_f)
		andalso ?SETS:is_element(LFrom, StateData#state.pres_a),
	    if
		Cond1 ->
		    Packet = StateData#state.pres_last,
		    case ejabberd_hooks:run_fold(
			   privacy_check_packet, StateData#state.server,
			   allow,
			   [StateData#state.user,
			    StateData#state.server,
			    StateData#state.privacy_list,
			    {To, From, Packet},
			    out]) of
			deny ->
			    ok;
			allow ->
			    Pid=element(2, StateData#state.sid),
			    ejabberd_hooks:run(presence_probe_hook, StateData#state.server, [From, To, Pid]),
			    %% Don't route a presence probe to oneself
			    case From == To of
				false ->
				    ejabberd_router:route(To, From, Packet);
			    	true ->
				    ok
			    end
		    end;
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
    {xmlelement, _Name, Attrs, _Els} = Packet,
    case xml:get_attr_s("type", Attrs) of
	"unavailable" ->
	    Status = case xml:get_subtag(Packet, "status") of
			 false ->
			    "";
			 StatusTag ->
			    xml:get_tag_cdata(StatusTag)
		     end,
	    Info = [{ip, StateData#state.ip},{conn, StateData#state.conn}],
	    ejabberd_sm:unset_presence(StateData#state.sid,
				       StateData#state.user,
				       StateData#state.server,
				       StateData#state.resource,
				       Status,
				       Info),
	    presence_broadcast(StateData, From, StateData#state.pres_a, Packet),
	    presence_broadcast(StateData, From, StateData#state.pres_i, Packet),
	    StateData#state{pres_last = undefined,
			    pres_a = ?SETS:new(),
			    pres_i = ?SETS:new(),
			    pres_invis = false};
	"invisible" ->
	    NewPriority = get_priority_from_presence(Packet),
	    update_priority(NewPriority, Packet, StateData),
	    NewState =
		if
		    not StateData#state.pres_invis ->
			presence_broadcast(StateData, From,
					   StateData#state.pres_a,
					   Packet),
			presence_broadcast(StateData, From,
					   StateData#state.pres_i,
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
	"probe" ->
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
	    OldPriority = case StateData#state.pres_last of
			      undefined ->
				  0;
			      OldPresence ->
				  get_priority_from_presence(OldPresence)
			  end,
	    NewPriority = get_priority_from_presence(Packet),
	    update_priority(NewPriority, Packet, StateData),
	    FromUnavail = (StateData#state.pres_last == undefined) or
		StateData#state.pres_invis,
	    ?DEBUG("from unavail = ~p~n", [FromUnavail]),
	    NewState =
		if
		    FromUnavail ->
			ejabberd_hooks:run(user_available_hook,
					   StateData#state.server,
					   [StateData#state.jid]),
			if NewPriority >= 0 ->
				resend_offline_messages(StateData),
				resend_subscription_requests(StateData);
			   true ->
				ok
			end,
			presence_broadcast_first(
			  From, StateData#state{pres_last = Packet,
						pres_invis = false
					       }, Packet);
		    true ->
			presence_broadcast_to_trusted(StateData,
						      From,
						      StateData#state.pres_f,
						      StateData#state.pres_a,
						      Packet),
			if OldPriority < 0, NewPriority >= 0 ->
				resend_offline_messages(StateData);
			   true ->
				ok
			end,
			StateData#state{pres_last = Packet,
					pres_invis = false
				       }
		end,
	    NewState
    end.

presence_track(From, To, Packet, StateData) ->
    {xmlelement, _Name, Attrs, _Els} = Packet,
    LTo = jlib:jid_tolower(To),
    User = StateData#state.user,
    Server = StateData#state.server,
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
	    ejabberd_hooks:run(roster_out_subscription,
			       Server,
			       [User, Server, To, subscribe]),
	    ejabberd_router:route(jlib:jid_remove_resource(From), To, Packet),
	    StateData;
	"subscribed" ->
	    ejabberd_hooks:run(roster_out_subscription,
			       Server,
			       [User, Server, To, subscribed]),
	    ejabberd_router:route(jlib:jid_remove_resource(From), To, Packet),
	    StateData;
	"unsubscribe" ->
	    ejabberd_hooks:run(roster_out_subscription,
			       Server,
			       [User, Server, To, unsubscribe]),
	    ejabberd_router:route(jlib:jid_remove_resource(From), To, Packet),
	    StateData;
	"unsubscribed" ->
	    ejabberd_hooks:run(roster_out_subscription,
			       Server,
			       [User, Server, To, unsubscribed]),
	    ejabberd_router:route(jlib:jid_remove_resource(From), To, Packet),
	    StateData;
	"error" ->
	    ejabberd_router:route(From, To, Packet),
	    StateData;
	"probe" ->
	    ejabberd_router:route(From, To, Packet),
	    StateData;
	_ ->
	    case ejabberd_hooks:run_fold(
		   privacy_check_packet, StateData#state.server,
		   allow,
		   [StateData#state.user,
		    StateData#state.server,
		    StateData#state.privacy_list,
		    {From, To, Packet},
		    out]) of
		deny ->
		    ok;
		allow ->
		    ejabberd_router:route(From, To, Packet)
	    end,
	    I = remove_element(LTo, StateData#state.pres_i),
	    A = ?SETS:add_element(LTo, StateData#state.pres_a),
	    StateData#state{pres_i = I,
			    pres_a = A}
    end.

presence_broadcast(StateData, From, JIDSet, Packet) ->
    lists:foreach(fun(JID) ->
			  FJID = jlib:make_jid(JID),
			  case ejabberd_hooks:run_fold(
				 privacy_check_packet, StateData#state.server,
				 allow,
				 [StateData#state.user,
				  StateData#state.server,
				  StateData#state.privacy_list,
				  {From, FJID, Packet},
				  out]) of
			      deny ->
				  ok;
			      allow ->
				  ejabberd_router:route(From, FJID, Packet)
			  end
		  end, ?SETS:to_list(JIDSet)).

presence_broadcast_to_trusted(StateData, From, T, A, Packet) ->
    lists:foreach(
      fun(JID) ->
	      case ?SETS:is_element(JID, T) of
		  true ->
		      FJID = jlib:make_jid(JID),
		      case ejabberd_hooks:run_fold(
			     privacy_check_packet, StateData#state.server,
			     allow,
			     [StateData#state.user,
			      StateData#state.server,
			      StateData#state.privacy_list,
			      {From, FJID, Packet},
			      out]) of
			  deny ->
			      ok;
			  allow ->
			      ejabberd_router:route(From, FJID, Packet)
		      end;
		  _ ->
		      ok
	      end
      end, ?SETS:to_list(A)).


presence_broadcast_first(From, StateData, Packet) ->
    ?SETS:fold(fun(JID, X) ->
		       ejabberd_router:route(
			 From,
			 jlib:make_jid(JID),
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
	    As = ?SETS:fold(
		    fun(JID, A) ->
			    FJID = jlib:make_jid(JID),
			    case ejabberd_hooks:run_fold(
				   privacy_check_packet, StateData#state.server,
				   allow,
				   [StateData#state.user,
				    StateData#state.server,
				    StateData#state.privacy_list,
				    {From, FJID, Packet},
				    out]) of
				deny ->
				    ok;
				allow ->
				    ejabberd_router:route(From, FJID, Packet)
			    end,
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
    OldIsFrom = ?SETS:is_element(LIJID, StateData#state.pres_f),
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
	undefined ->
	    StateData#state{pres_f = FSet, pres_t = TSet};
	P ->
	    ?DEBUG("roster changed for ~p~n", [StateData#state.user]),
	    From = StateData#state.jid,
	    To = jlib:make_jid(IJID),
	    Cond1 = (not StateData#state.pres_invis) and IsFrom
		and (not OldIsFrom),
	    Cond2 = (not IsFrom) and OldIsFrom
		and (?SETS:is_element(LIJID, StateData#state.pres_a) or
		     ?SETS:is_element(LIJID, StateData#state.pres_i)),
	    if
		Cond1 ->
		    ?DEBUG("C1: ~p~n", [LIJID]),
		    case ejabberd_hooks:run_fold(
			   privacy_check_packet, StateData#state.server,
			   allow,
			   [StateData#state.user,
			    StateData#state.server,
			    StateData#state.privacy_list,
			    {From, To, P},
			    out]) of
			deny ->
			    ok;
			allow ->
			    ejabberd_router:route(From, To, P)
		    end,
		    A = ?SETS:add_element(LIJID,
					  StateData#state.pres_a),
		    StateData#state{pres_a = A,
				    pres_f = FSet,
				    pres_t = TSet};
		Cond2 ->
		    ?DEBUG("C2: ~p~n", [LIJID]),
		    PU = {xmlelement, "presence",
			  [{"type", "unavailable"}], []},
		    case ejabberd_hooks:run_fold(
			   privacy_check_packet, StateData#state.server,
			   allow,
			   [StateData#state.user,
			    StateData#state.server,
			    StateData#state.privacy_list,
			    {From, To, PU},
			    out]) of
			deny ->
			    ok;
			allow ->
			    ejabberd_router:route(From, To, PU)
		    end,
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


update_priority(Priority, Packet, StateData) ->
    Info = [{ip, StateData#state.ip},{conn, StateData#state.conn}],
    ejabberd_sm:set_presence(StateData#state.sid,
			     StateData#state.user,
			     StateData#state.server,
			     StateData#state.resource,
			     Priority,
			     Packet,
			     Info).

get_priority_from_presence(PresencePacket) ->
    case xml:get_subtag(PresencePacket, "priority") of
	false ->
	    0;
	SubEl ->
	    case catch list_to_integer(xml:get_tag_cdata(SubEl)) of
		P when is_integer(P) ->
		    P;
		_ ->
		    0
	    end
    end.

process_privacy_iq(From, To,
		   #iq{type = Type, sub_el = SubEl} = IQ,
		   StateData) ->
    {Res, NewStateData} =
	case Type of
	    get ->
		R = ejabberd_hooks:run_fold(
		      privacy_iq_get, StateData#state.server,
		      {error, ?ERR_FEATURE_NOT_IMPLEMENTED},
		      [From, To, IQ, StateData#state.privacy_list]),
		{R, StateData};
	    set ->
		case ejabberd_hooks:run_fold(
		       privacy_iq_set, StateData#state.server,
		       {error, ?ERR_FEATURE_NOT_IMPLEMENTED},
		       [From, To, IQ]) of
		    {result, R, NewPrivList} ->
			{{result, R},
			 StateData#state{privacy_list = NewPrivList}};
		    R -> {R, StateData}
		end
	end,
    IQRes =
	case Res of
	    {result, Result} ->
		IQ#iq{type = result, sub_el = Result};
	    {error, Error} ->
		IQ#iq{type = error, sub_el = [SubEl, Error]}
	end,
    ejabberd_router:route(
      To, From, jlib:iq_to_xml(IQRes)),
    NewStateData.


resend_offline_messages(#state{user = User,
			       server = Server,
			       privacy_list = PrivList} = StateData) ->
    case ejabberd_hooks:run_fold(resend_offline_messages_hook,
				 Server,
				 [],
				 [User, Server]) of
	Rs when list(Rs) ->
	    lists:foreach(
	      fun({route,
		   From, To, {xmlelement, Name, Attrs, Els} = Packet}) ->
		      Pass = case ejabberd_hooks:run_fold(
				    privacy_check_packet, Server,
				    allow,
				    [User,
				     Server,
				     PrivList,
				     {From, To, Packet},
				     in]) of
				 allow ->
				     true;
				 deny ->
				     false
			     end,
		      if
			  Pass ->
			      Attrs2 = jlib:replace_from_to_attrs(
					 jlib:jid_to_string(From),
					 jlib:jid_to_string(To),
					 Attrs),
			      send_element(StateData,
					   {xmlelement, Name, Attrs2, Els});
			  true ->
			      ok
		      end
	      end, Rs)
    end.

resend_subscription_requests(#state{user = User,
				    server = Server} = StateData) ->
    PendingSubscriptions = ejabberd_hooks:run_fold(
			     resend_subscription_requests_hook,
			     Server,
			     [],
			     [User, Server]),
    lists:foreach(fun(XMLPacket) ->
			  send_element(StateData,
				       XMLPacket)
		  end,
		  PendingSubscriptions).

get_showtag(undefined) ->
    "unavailable";
get_showtag(Presence) ->
    case xml:get_path_s(Presence, [{elem, "show"}, cdata]) of
	""      -> "available";
	ShowTag -> ShowTag
    end.

get_statustag(undefined) ->
    "";
get_statustag(Presence) ->
    case xml:get_path_s(Presence, [{elem, "status"}, cdata]) of
	ShowTag -> ShowTag
    end.

process_unauthenticated_stanza(StateData, El) ->
    case jlib:iq_query_info(El) of
	#iq{} = IQ ->
	    Res = ejabberd_hooks:run_fold(c2s_unauthenticated_iq,
					  StateData#state.server,
					  empty,
					  [StateData#state.server, IQ,
					   StateData#state.ip]),
	    case Res of
		empty ->
		    % The only reasonable IQ's here are auth and register IQ's
		    % They contain secrets, so don't include subelements to response
		    ResIQ = IQ#iq{type = error,
				  sub_el = [?ERR_SERVICE_UNAVAILABLE]},
		    Res1 = jlib:replace_from_to(
			     jlib:make_jid("", StateData#state.server, ""),
			     jlib:make_jid("", "", ""),
			     jlib:iq_to_xml(ResIQ)),
		    send_element(StateData, jlib:remove_attr("to", Res1));
		_ ->
		    send_element(StateData, Res)
	    end;
	_ ->
	    % Drop any stanza, which isn't IQ stanza
	    ok
    end.

peerip(SockMod, Socket) ->
    IP = case SockMod of
	     gen_tcp -> inet:peername(Socket);
	     _ -> SockMod:peername(Socket)
	 end,
    case IP of
	{ok, IPOK} -> IPOK;
	_ -> undefined
    end.

%% fsm_next_state: Generate the next_state FSM tuple with different
%% timeout, depending on the future state
fsm_next_state(session_established, StateData) ->
    {next_state, session_established, StateData, ?C2S_HIBERNATE_TIMEOUT};
fsm_next_state(StateName, StateData) ->
    {next_state, StateName, StateData, ?C2S_OPEN_TIMEOUT}.

%% fsm_reply: Generate the reply FSM tuple with different timeout,
%% depending on the future state
fsm_reply(Reply, session_established, StateData) ->
    {reply, Reply, session_established, StateData, ?C2S_HIBERNATE_TIMEOUT};
fsm_reply(Reply, StateName, StateData) ->
    {reply, Reply, StateName, StateData, ?C2S_OPEN_TIMEOUT}.

%% Used by c2s blacklist plugins
is_ip_blacklisted({IP,_Port}) ->
    ejabberd_hooks:run_fold(check_bl_c2s, false, [IP]).	
