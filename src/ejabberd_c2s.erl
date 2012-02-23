%%%----------------------------------------------------------------------
%%% File    : ejabberd_c2s.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve C2S connection
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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

-define(GEN_FSM, p1_fsm).

-behaviour(?GEN_FSM).

%% External exports
-export([start/2,
	 stop/1,
	 start_link/3,
	 send_text/2,
	 send_element/2,
	 socket_type/0,
	 get_presence/1,
	 get_aux_field/2,
	 set_aux_field/3,
	 del_aux_field/2,
	 get_subscription/2,
	 broadcast/4,
	 get_subscribed/1]).

%% API:
-export([add_rosteritem/3, del_rosteritem/2]).

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
	 terminate/3,
	 print_state/1,
	 migrate/3
	]).


-export([get_state/1]).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").

-include("ejabberd.hrl").
-include("mod_privacy.hrl").

%% Copied from ejabberd_socket.erl
-record(socket_state, {sockmod, socket, receiver}).

-define(SETS, gb_sets).
-define(DICT, dict).

%% pres_a contains all the presence available send (either through roster mechanism or directed).
%% Directed presence unavailable remove user from pres_a.
-record(state, {socket,
		sockmod,
		socket_monitor,
		xml_socket,
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
		user = undefined, server = list_to_binary(?MYNAME), resource = undefined,
		sid,
		pres_t = ?SETS:new(),
		pres_f = ?SETS:new(),
		pres_a = ?SETS:new(),
		pres_i = ?SETS:new(),
		pres_last, pres_pri,
		pres_timestamp,
		privacy_list = #userlist{},
		conn = unknown,
		auth_module = unknown,
		ip,
		aux_fields = [],
		fsm_limit_opts,
		lang,
                flash_connection = false}).

%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, [{spawn_opt,[{fullsweep_after,10}]}]).
-endif.

%% Module start with or without supervisor:
-ifdef(NO_TRANSIENT_SUPERVISORS).
-define(SUPERVISOR_START, ?GEN_FSM:start(ejabberd_c2s,
					 [SockData, Opts, FSMLimitOpts],
					 FSMLimitOpts ++ ?FSMOPTS)).
-else.
-define(SUPERVISOR_START, supervisor:start_child(ejabberd_c2s_sup,
						 [SockData, Opts, FSMLimitOpts])).
-endif.

%% This is the timeout to apply between event when starting a new
%% session:
-define(C2S_OPEN_TIMEOUT, 60000).
-define(C2S_HIBERNATE_TIMEOUT, 90000).

% These are the namespace already declared by the stream opening. This is
% used at serialization time.
-define(DEFAULT_NS, ?NS_JABBER_CLIENT).
-define(PREFIXED_NS, [{?NS_XMPP, ?NS_XMPP_pfx}]).

-define(STREAM_HEADER,
	"<?xml version='1.0'?>"
	"<stream:stream xmlns='jabber:client' "
	"xmlns:stream='http://etherx.jabber.org/streams' "
	"id='~s' from='~s'~s~s>"
       ).

-define(FLASH_STREAM_HEADER,
       "<?xml version='1.0'?>"
       "<flash:stream xmlns='jabber:client' "
       "xmlns:stream='http://etherx.jabber.org/streams' "
       "id='~s' from='~s'~s~s>"
       ).
-define(NS_FLASH_STREAM, "http://www.jabber.com/streams/flash").

-define(INVALID_NS_ERR, exmpp_stream:error('invalid-namespace')).
-define(INVALID_XML_ERR,  exmpp_stream:error('xml-not-well-formed')).
-define(HOST_UNKNOWN_ERR, exmpp_stream:error('host-unknown')).
-define(SERRT_CONFLICT, exmpp_stream:error('conflict')).
-define(POLICY_VIOLATION_ERR(Lang, Text),
	exmpp_stream:error('policy-violation', {Lang, Text})).

-define(INVALID_FROM, exmpp_stream:error('invalid-from')).

-define(STANZA_ERROR(NS, Condition),
%  exmpp_xml:xmlel_to_xmlelement(exmpp_stanza:error(NS, Condition),
%    [?NS_JABBER_CLIENT], [{?NS_XMPP, "stream"}])).
  exmpp_stanza:error(NS, Condition)).
-define(ERR_FEATURE_NOT_IMPLEMENTED(NS),
  ?STANZA_ERROR(NS, 'feature-not-implemented')).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(StateName, #state{fsm_limit_opts = Opts} = State) ->
    start(StateName, State, Opts);
start(SockData, Opts) ->
    start(SockData, Opts, fsm_limit_opts(Opts)).

start(SockData, Opts, FSMLimitOpts) ->
    ?SUPERVISOR_START.

start_link(SockData, Opts, FSMLimitOpts) ->
    ?GEN_FSM:start_link(ejabberd_c2s, [SockData, Opts, FSMLimitOpts],
			FSMLimitOpts ++ ?FSMOPTS).

socket_type() ->
    xml_stream.

%% Return Username, Resource and presence information
get_presence(FsmRef) ->
    ?GEN_FSM:sync_send_all_state_event(FsmRef, {get_presence}, 1000).

get_aux_field(Key, #state{aux_fields = Opts}) ->
    case lists:keysearch(Key, 1, Opts) of
	{value, {_, Val}} ->
	    {ok, Val};
	_ ->
	    error
    end.

set_aux_field(Key, Val, #state{aux_fields = Opts} = State) ->
    Opts1 = lists:keydelete(Key, 1, Opts),
    State#state{aux_fields = [{Key, Val}|Opts1]}.

del_aux_field(Key, #state{aux_fields = Opts} = State) ->
    Opts1 = lists:keydelete(Key, 1, Opts),
    State#state{aux_fields = Opts1}.

get_subscription(From = #jid{}, StateData) ->
    get_subscription(exmpp_jid:to_lower(From), StateData);
get_subscription(LFrom, StateData) ->
    LBFrom = setelement(3, LFrom, undefined),
    F = ?SETS:is_element(LFrom, StateData#state.pres_f) orelse
        ?SETS:is_element(LBFrom, StateData#state.pres_f),
    T = ?SETS:is_element(LFrom, StateData#state.pres_t) orelse
        ?SETS:is_element(LBFrom, StateData#state.pres_t),
    if F and T -> both;
	F -> from;
	T -> to;
	true -> none
    end.

broadcast(FsmRef, Type, From, Packet) ->
    FsmRef ! {broadcast, Type, From, Packet}.

%%TODO: for debug only
get_state(FsmRef) ->
    ?GEN_FSM:sync_send_all_state_event(FsmRef, get_state, 1000).

add_rosteritem(FsmRef, IJID, ISubscription) when is_binary(ISubscription) ->
    ?GEN_FSM:send_all_state_event(FsmRef, {add_rosteritem, IJID, ISubscription}).

del_rosteritem(FsmRef, IJID) ->
    ?GEN_FSM:send_all_state_event(FsmRef, {del_rosteritem, IJID}).

stop(FsmRef) ->
    ?GEN_FSM:send_event(FsmRef, closed).

migrate(FsmRef, Node, After) ->
    ?GEN_FSM:send_all_state_event(FsmRef, {migrate, Node, After}).

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
init([{SockMod, Socket}, Opts, FSMLimitOpts]) ->
    Access = case lists:keysearch(access, 1, Opts) of
		 {value, {_, A}} -> A;
		 _ -> all
	     end,
    Shaper = case lists:keysearch(shaper, 1, Opts) of
		 {value, {_, S}} -> S;
		 _ -> none
	     end,
    XMLSocket =
	case lists:keysearch(xml_socket, 1, Opts) of
	    {value, {_, XS}} -> XS;
	    _ -> false
	end,
    Zlib = lists:member(zlib, Opts),
    StartTLS = lists:member(starttls, Opts),
    StartTLSRequired = lists:member(starttls_required, Opts),
    TLSEnabled = lists:member(tls, Opts),
    TLS = StartTLS orelse StartTLSRequired orelse TLSEnabled,
    TLSOpts1 =
	lists:filter(fun({certfile, _}) -> true;
			(_) -> false
		     end, Opts),
    TLSOpts = [verify_none | TLSOpts1],
    IP = case lists:keysearch(frontend_ip, 1, Opts) of
	     {value, {_, IP1}} ->
		 IP1;
	     _ ->
		 peerip(SockMod, Socket)
	 end,
    %% Check if IP is blacklisted:
    case is_ip_blacklisted(IP) of
	true ->
	    ?INFO_MSG("Connection attempt from blacklisted IP: ~s (~w)",
		      [jlib:ip_to_list(IP), IP]),
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
					 xml_socket     = XMLSocket,
					 zlib           = Zlib,
					 tls            = TLS,
					 tls_required   = StartTLSRequired,
					 tls_enabled    = TLSEnabled,
					 tls_options    = TLSOpts,
					 streamid       = new_id(),
					 access         = Access,
					 shaper         = Shaper,
					 ip             = IP,
					 fsm_limit_opts = FSMLimitOpts},
	     ?C2S_OPEN_TIMEOUT}
    end;
init([StateName, StateData, _FSMLimitOpts]) ->
    MRef = (StateData#state.sockmod):monitor(StateData#state.socket),
    if StateName == session_established ->
	    Conn = get_conn_type(StateData),
	    Info = [{ip, StateData#state.ip}, {conn, Conn},
		    {auth_module, StateData#state.auth_module}],
	    {Time, _} = StateData#state.sid,
	    SID = {Time, self()},
	    Priority = case StateData#state.pres_last of
			   undefined ->
			       undefined;
			   El ->
			       exmpp_presence:get_priority(El)
		       end,
	    ejabberd_sm:open_session(SID, StateData#state.jid, Priority, Info),
	    NewStateData = StateData#state{sid = SID, socket_monitor = MRef},
	    {ok, StateName, NewStateData};
       true ->
        %% pack, as sharing is not preserved accross message passing
	    {ok, StateName, pack(StateData#state{socket_monitor = MRef})}
    end.

%% Return list of all available resources of contacts,
get_subscribed(FsmRef) ->
    ?GEN_FSM:sync_send_all_state_event(FsmRef, get_subscribed, 1000).


%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

wait_for_stream({xmlstreamstart, #xmlel{ns = NS, name = Name} = Opening},
                StateData) ->
    DefaultLang = case ?MYLANG of
		      undefined ->
			  "en";
		      DL ->
			  DL
		  end,
    Header = exmpp_stream:opening_reply(Opening,
      StateData#state.streamid, DefaultLang),
    case {NS, exmpp_xml:is_ns_declared_here(Opening, ?NS_FLASH_STREAM),
          ?FLASH_HACK,
          StateData#state.flash_connection} of
        {_, true, true, false} ->
            %% Flash client connecting - attention!
            %% Some of them don't provide an xmlns:stream attribute -
            %% compensate for that.
            wait_for_stream({xmlstreamstart, Opening#xmlel{ns = ?NS_XMPP}},
                            StateData#state{flash_connection = true});
	{?NS_XMPP, _, _, _} ->
	    ServerB = exmpp_stringprep:nameprep(
	      exmpp_stream:get_receiving_entity(Opening)),
            Server = binary_to_list(ServerB),
	    case ?IS_MY_HOST(Server) of
		true ->
		    Lang = case exmpp_stream:get_lang(Opening) of
			       Lang1 when is_binary(Lang1) andalso size(Lang1) =< 35 ->
				   %% As stated in BCP47, 4.4.1:
				   %% Protocols or specifications that
				   %% specify limited buffer sizes for
				   %% language tags MUST allow for
				   %% language tags of at least 35 characters.
				   Lang1;
			       _ ->
				   %% Do not store long language tag to
				   %% avoid possible DoS/flood attacks
				   undefined
			   end,
		    change_shaper(StateData,
		      exmpp_jid:make(ServerB)),
		    case exmpp_stream:get_version(Opening) of
			{1, 0} ->
			    send_header(StateData, Server, "1.0", DefaultLang),
			    case StateData#state.authenticated of
				false ->
				    Realm =
					case ejabberd_config:get_local_option({sasl_realm, Server}) of
					    undefined ->
						"";
					    Realm0 ->
						Realm0
					end,
				    SASLState =
					cyrsasl:server_new(
					  "jabber", Server, Realm, [],
					  fun(U) ->
						  ejabberd_auth:get_password_with_authmodule(
						    U, Server)
					  end,
					  fun(U, P) ->
						  ejabberd_auth:check_password_with_authmodule(
						    U, Server, P)
					  end,
					  fun(U, P, D, DG) ->
						  ejabberd_auth:check_password_with_authmodule(
						    U, Server, P, D, DG)
					  end,
					  StateData#state.socket),
				    MechsPrepared = [exmpp_server_sasl:feature(
					cyrsasl:listmech(Server))],
				    SockMod =
					(StateData#state.sockmod):get_sockmod(
					  StateData#state.socket),
				    TLSRequired = StateData#state.tls_required,
				    Mechs =
					case TLSRequired of
					    true ->
						case (SockMod == gen_tcp) of
						    true ->
							[];
						    false ->
							MechsPrepared
						end;
					    false ->
						MechsPrepared
					end,
				    SockMod =
					(StateData#state.sockmod):get_sockmod(
					  StateData#state.socket),
				    Zlib = StateData#state.zlib,
				    CompressFeature =
					case Zlib andalso
					    ((SockMod == gen_tcp) orelse
					     (SockMod == tls)) of
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
                                    Other_Feats = ejabberd_hooks:run_fold(
						    c2s_stream_features,
						    ServerB,
						    [], [ServerB]),
				    send_element(StateData,
				      exmpp_stream:features(
					TLSFeature ++
					CompressFeature ++
					Mechs ++
					Other_Feats)),
				    fsm_next_state(wait_for_feature_request,
					       StateData#state{
						 server = ServerB,
						 sasl_state = SASLState,
						 lang = Lang});
				_ ->
				    case StateData#state.resource of
					undefined ->
					    RosterVersioningFeature =
						ejabberd_hooks:run_fold(
						  roster_get_versioning_feature,
						  ServerB,
						  [], [ServerB]),
					    Other_Feats = ejabberd_hooks:run_fold(
							    c2s_stream_features,
							    ServerB,
							    [], [ServerB]),
					    send_element(
					      StateData,
					      exmpp_stream:features(
						[exmpp_server_binding:feature(),
						 exmpp_server_session:feature()]
						++ RosterVersioningFeature
						++ Other_Feats)),
					    fsm_next_state(wait_for_bind,
							   StateData#state{
							     server = ServerB,
							     lang = Lang});
					_ ->
					    send_element(
					      StateData,
					      exmpp_stream:features([])),
					    fsm_next_state(wait_for_session,
						       StateData#state{
							 server = ServerB,
							 lang = Lang})
				    end
			    end;
			_ ->
			    send_header(StateData, Server, "", DefaultLang),
			    if
				(not StateData#state.tls_enabled) and
				StateData#state.tls_required ->
				    send_element(StateData,
				      exmpp_xml:append_child(Header,
					exmpp_stream:error('policy-violation',
					  {"en", "Use of STARTTLS required"}))),
				    send_trailer(StateData),
				    {stop, normal, StateData};
				true ->
				    fsm_next_state(wait_for_auth,
						   StateData#state{
						     server = ServerB,
						     lang = Lang})
			    end
		    end;
		_ ->
		    send_header(StateData, ?MYNAME, "", DefaultLang),
		    send_element(StateData, ?HOST_UNKNOWN_ERR),
		    send_trailer(StateData),
		    {stop, normal, StateData}
	    end;
        _ ->
	    case Name of
		"policy-file-request" ->
		    send_text(StateData, flash_policy_string()),
		    {stop, normal, StateData};
		_ ->
		    send_header(StateData, ?MYNAME, "", DefaultLang),
		    send_element(StateData, ?INVALID_NS_ERR),
		    send_trailer(StateData),
		    {stop, normal, StateData}
	    end
    end;

wait_for_stream(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_stream({xmlstreamelement, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_stream({xmlstreamend, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_header(StateData, ?MYNAME, "1.0", ""),
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_auth({xmlstreamelement, El}, StateData) ->
    ServerString = binary_to_list(StateData#state.server),
    case is_auth_packet(El) of
	{auth, _ID, get, {_U, _, _, _}} ->
	    Fields = case ejabberd_auth:plain_password_required(
	      ServerString) of
		false -> both;
		true  -> plain
	    end,
	    send_element(StateData,
	      exmpp_server_legacy_auth:fields(El, Fields)),
	    fsm_next_state(wait_for_auth, StateData);
	{auth, _ID, set, {_U, _P, _D, undefined}} ->
            Err = exmpp_stanza:reply_with_error(El,
	          exmpp_stanza:error(El#xmlel.ns, 'not-acceptable',
	      {"en", "No resource provided"})),
	    send_element(StateData, Err),
	    fsm_next_state(wait_for_auth, StateData);
	{auth, _ID, set, {U, P, D, R}} ->
	    try
		JID = exmpp_jid:make(U, StateData#state.server, R),
		UBinary = exmpp_jid:prep_node(JID),
		case acl:match_rule(ServerString,
		  StateData#state.access, JID) of
		    allow ->
                        DGen = fun(PW) ->
                             sha:sha(StateData#state.streamid ++ PW) end,
			case ejabberd_auth:check_password_with_authmodule(
			  U, ServerString, P,
			  D, DGen) of
			    {true, AuthModule} ->
				?INFO_MSG(
				  "(~w) Accepted legacy authentication for ~s by ~s",
				  [StateData#state.socket,
				    exmpp_jid:to_binary(JID), AuthModule]),
			    erlang:link((StateData#state.socket)#socket_state.receiver),
				SID = {now(), self()},
				Conn = get_conn_type(StateData),
				%% Info = [{ip, StateData#state.ip}, {conn, Conn},
				%%   {auth_module, AuthModule}],
				Res = exmpp_server_legacy_auth:success(El),
				send_element(StateData, Res),
				%% ejabberd_sm:open_session(
				%%   SID, exmpp_jid:make(U, StateData#state.server, R), Info),
				change_shaper(StateData, JID),
				{Fs, Ts} = ejabberd_hooks:run_fold(
				  roster_get_subscription_lists,
				  StateData#state.server,
				  {[], []},
				  [UBinary, StateData#state.server]),
				LJID = jlib:short_prepd_bare_jid(JID),
				Fs1 = [LJID | Fs],
				Ts1 = [LJID | Ts],
				PrivList = ejabberd_hooks:run_fold(
				  privacy_get_user_list, StateData#state.server,
				  #userlist{},
				  [UBinary, StateData#state.server]),
				maybe_migrate(session_established,
					      StateData#state{
						sasl_state = 'undefined', 
						%not used anymore, let the GC work.
						user = list_to_binary(U),
						resource = list_to_binary(R),
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
				    exmpp_jid:to_binary(JID)]),
				Res = exmpp_iq:error_without_original(El,
                                  'not-authorized'),
				send_element(StateData, Res),
				fsm_next_state(wait_for_auth, StateData)
			end;
		    {false, ReasonAuthFail} when is_list(ReasonAuthFail) ->
			?INFO_MSG(
			  "(~w) Forbidden legacy authentication for ~s due to ~s",
			  [StateData#state.socket,
			    exmpp_jid:to_binary(JID), ReasonAuthFail]),
			    ErrorType = case ReasonAuthFail of
				"not-authorized" -> 'not-authorized';
				"temporary-auth-failure" -> 'internal-server-error';
				_ -> 'not-authorized'
			    end,
			Res = exmpp_iq:error_without_original(El,
                          ErrorType),
			send_element(StateData, Res),
			fsm_next_state(wait_for_auth, StateData);
		    _ ->
			?INFO_MSG(
			  "(~w) Forbidden legacy authentication for ~s",
			  [StateData#state.socket,
			    exmpp_jid:to_binary(JID)]),
			Res = exmpp_iq:error_without_original(El,
                          'not-allowed'),
			send_element(StateData, Res),
			fsm_next_state(wait_for_auth, StateData)
		end
	    catch
		throw:_Exception ->
		    ?INFO_MSG(
		      "(~w) Forbidden legacy authentication for "
		      "username '~s' with resource '~s'",
		      [StateData#state.socket, U, R]),
		    Res1 = exmpp_iq:error_without_original(El, 'jid-malformed'),
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
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_auth({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
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
		    catch (StateData#state.sockmod):reset_stream(
			    StateData#state.socket),
		    send_element(StateData, exmpp_server_sasl:success()),
		    U = proplists:get_value(username, Props),
		    AuthModule = proplists:get_value(auth_module, Props),
		    ?INFO_MSG("(~w) Accepted authentication for ~s by ~s",
			      [StateData#state.socket, U, AuthModule]),
		    fsm_next_state(wait_for_stream,
				   StateData#state{
				     streamid = new_id(),
				     authenticated = true,
				     auth_module = AuthModule,
				     user = list_to_binary(U) });
		{continue, ServerOut, NewSASLState} ->
		    send_element(StateData,
		      exmpp_server_sasl:challenge(ServerOut)),
		    fsm_next_state(wait_for_sasl_response,
				   StateData#state{
				     sasl_state = NewSASLState});
		{error, Error, Text, Username} ->
		    ?INFO_MSG(
		       "(~w) Failed authentication for ~s@~s due to ~p ~s",
		       [StateData#state.socket,
			Username, StateData#state.server, Error, Text]),
		    send_element(StateData,
		      exmpp_server_sasl:failure(Error, Text)),
		    {next_state, wait_for_feature_request, StateData,
		     ?C2S_OPEN_TIMEOUT};
		{error, Error} ->
		    send_element(StateData,
		      exmpp_server_sasl:failure(Error)),
		    fsm_next_state(wait_for_feature_request, StateData)
	    end;
	{?NS_TLS, 'starttls'} when TLS == true,
				   TLSEnabled == false,
				   SockMod == gen_tcp ->
        ServerString = binary_to_list(StateData#state.server),
	    TLSOpts = case ejabberd_config:get_local_option
                          ({domain_certfile, ServerString}) of
			  undefined ->
			      StateData#state.tls_options;
			  CertFile ->
			      [{certfile, CertFile} |
			       lists:keydelete(
				 certfile, 1, StateData#state.tls_options)]
		      end,
	    Socket = StateData#state.socket,
	    Proceed = exmpp_xml:node_to_list(
	      exmpp_server_tls:proceed(), [?DEFAULT_NS], ?PREFIXED_NS),
	    TLSSocket = (StateData#state.sockmod):starttls(
			  Socket, TLSOpts,
			  Proceed),
	    fsm_next_state(wait_for_stream,
			   StateData#state{socket = TLSSocket,
					   streamid = new_id(),
					   tls_enabled = true
					  });
	{?NS_COMPRESS, 'compress'} when Zlib == true,
					((SockMod == gen_tcp) or
					 (SockMod == tls)) ->
	    case exmpp_server_compression:selected_method(El) of
		undefined ->
		    send_element(StateData,
		      exmpp_server_compression:failure('setup-failed')),
		    fsm_next_state(wait_for_feature_request, StateData);
		<<"zlib">> ->
		    Socket = StateData#state.socket,
		    Compressed = exmpp_xml:node_to_list(
		      exmpp_server_compression:compressed(), [?DEFAULT_NS], ?PREFIXED_NS),
		    ZlibSocket = (StateData#state.sockmod):compress(
				   Socket,
				   Compressed),
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
		    Lang = StateData#state.lang,
		    send_element(StateData, exmpp_stream:error(
			'policy-violation', {Lang, "Use of STARTTLS required"})),
		    send_trailer(StateData),
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
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_feature_request({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
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
		    catch (StateData#state.sockmod):reset_stream(
			    StateData#state.socket),
		    send_element(StateData, exmpp_server_sasl:success()),
		    U = proplists:get_value(username, Props),
		    AuthModule = proplists:get_value(auth_module, Props),
		    ?INFO_MSG("(~w) Accepted authentication for ~s by ~s",
			      [StateData#state.socket, U, AuthModule]),
		    fsm_next_state(wait_for_stream,
				   StateData#state{
				     streamid = new_id(),
				     authenticated = true,
				     auth_module = AuthModule,
				     user = list_to_binary(U)});
		{ok, Props, ServerOut} ->
		    catch (StateData#state.sockmod):reset_stream(
		      StateData#state.socket),
		    send_element(StateData, exmpp_server_sasl:success(ServerOut)),
		    U = proplists:get_value(username, Props),

		    AuthModule = proplists:get_value(auth_module, Props),
		    ?INFO_MSG("(~w) Accepted authentication for ~s by ~s",
			      [StateData#state.socket, U, AuthModule]),
		    fsm_next_state(wait_for_stream,
				   StateData#state{
				     streamid = new_id(),
				     authenticated = true,
				     auth_module = AuthModule,
				     user = list_to_binary(U)});
		{continue, ServerOut, NewSASLState} ->
		    send_element(StateData,
		      exmpp_server_sasl:challenge(ServerOut)),
		    fsm_next_state(wait_for_sasl_response,
		     StateData#state{sasl_state = NewSASLState});
		{error, Error, Text, Username} ->
		    ?INFO_MSG(
		       "(~w) Failed authentication for ~s@~s due to ~p ~s",
		       [StateData#state.socket,
			Username, StateData#state.server, Error, Text]),
		    send_element(StateData,
		      exmpp_server_sasl:failure(Error, Text)),
		    fsm_next_state(wait_for_feature_request, StateData);
		{error, Error} ->
		    send_element(StateData,
		      exmpp_server_sasl:failure(Error)),
		    fsm_next_state(wait_for_feature_request, StateData)
	    end;
	_ ->
	    process_unauthenticated_stanza(StateData, El),
	    fsm_next_state(wait_for_feature_request, StateData)
    end;

wait_for_sasl_response(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_sasl_response({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_sasl_response({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_sasl_response(closed, StateData) ->
    {stop, normal, StateData}.


resource_conflict_action(U, S, R) ->
    OptionRaw = case ejabberd_sm:is_existing_resource(U, S, R) of
		    true ->
			ejabberd_config:get_local_option({resource_conflict,binary_to_list(S)});
		    false ->
			acceptnew
		end,
    Option = case OptionRaw of
		 setresource -> setresource;
		 closeold -> acceptnew; %% ejabberd_sm will close old session
		 closenew -> closenew;
		 acceptnew -> acceptnew;
		 _ -> acceptnew %% default ejabberd behavior
	     end,
    case Option of
	acceptnew ->
	    {accept_resource, R};
	closenew ->
	    closenew;
	setresource ->
	    Rnew = lists:concat([randoms:get_string() | tuple_to_list(now())]),
	    {accept_resource, Rnew}
    end.

wait_for_bind({xmlstreamelement, El}, StateData) ->
    try
	R = case exmpp_server_binding:wished_resource(El) of
	    undefined ->
		lists:concat([randoms:get_string() | tuple_to_list(now())]);
	    Resource ->
		Resource
	end,
	%%ServerB = StateData#state.server,
	%%RosterVersioningFeature =
	%%	ejabberd_hooks:run_fold(roster_get_versioning_feature,
	%%				ServerB, [], [ServerB]),
	%%send_element(StateData,
	%%	     exmpp_stream:features([exmpp_server_session:feature()
	%%				    | RosterVersioningFeature])),
	case resource_conflict_action(StateData#state.user, StateData#state.server, list_to_binary(R)) of
	closenew ->
		send_element(StateData, ?SERRT_CONFLICT), %% (Lang, "Replaced by new connection")),
		fsm_next_state(wait_for_bind, StateData);
	{accept_resource, R2} ->
		JID = exmpp_jid:make(StateData#state.user, StateData#state.server, R2),
		Res = exmpp_server_binding:bind(El, JID),
		send_element(StateData, Res),
		fsm_next_state(wait_for_session,
		       StateData#state{resource = exmpp_jid:resource(JID), jid = JID})
	end
    catch
	throw:{stringprep, resourceprep, _, _} ->
	    Err = exmpp_server_binding:error(El, 'bad-request'),
	    send_element(StateData, Err),
	    fsm_next_state(wait_for_bind, StateData);
	throw:Exception ->
	    ?DEBUG("When processing:~n  ~p~nThis exception was catched:~n  ~p", [El, Exception]),
	    fsm_next_state(wait_for_bind, StateData)
    end;

wait_for_bind(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_bind({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_bind({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_bind(closed, StateData) ->
    {stop, normal, StateData}.



wait_for_session({xmlstreamelement, El}, StateData) ->
    try
    ServerString = binary_to_list(StateData#state.server),
	JID = StateData#state.jid,
	true = exmpp_server_session:want_establishment(El),
	case acl:match_rule(ServerString,
			    StateData#state.access, JID) of
	    allow ->
		?INFO_MSG("(~w) Opened session for ~s",
			  [StateData#state.socket,
			   exmpp_jid:to_binary(JID)]),
		%%send_element(StateData, exmpp_stream:features([])),
		Res = exmpp_server_session:establish(El),
		send_element(StateData, Res),
		change_shaper(StateData, JID),
		{Fs, Ts} = ejabberd_hooks:run_fold(
			     roster_get_subscription_lists,
			     StateData#state.server,
			     {[], []},
			     [StateData#state.user, StateData#state.server]),
		LJID = jlib:short_prepd_bare_jid(JID),
		Fs1 = [LJID | Fs],
		Ts1 = [LJID | Ts],
		PrivList =
		    ejabberd_hooks:run_fold(
		      privacy_get_user_list, StateData#state.server,
		      #userlist{},
		      [StateData#state.user, StateData#state.server]),
		SID = {now(), self()},
		Conn = get_conn_type(StateData),
		%% Info = [{ip, StateData#state.ip}, {conn, Conn},
		%% 	{auth_module, StateData#state.auth_module}],
		%% ejabberd_sm:open_session(
		%%   SID, JID, Info),
		maybe_migrate(session_established,
			      StateData#state{
				sasl_state = 'undefined',
				%%not used anymore, let the GC work.
				sid = SID,
				conn = Conn,
				pres_f = ?SETS:from_list(Fs1),
				pres_t = ?SETS:from_list(Ts1),
				privacy_list = PrivList});
	    _ ->
		ejabberd_hooks:run(forbidden_session_hook,
				   StateData#state.server, [JID]),
		?INFO_MSG("(~w) Forbidden session for ~s",
			  [StateData#state.socket,
			   exmpp_jid:to_binary(JID)]),
		Err = exmpp_server_session:error(El, 'not-allowed'),
		send_element(StateData, Err),
		fsm_next_state(wait_for_session, StateData)
	end
    catch
	_Exception ->
	    fsm_next_state(wait_for_session, StateData)
    end;

wait_for_session(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_session({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_session({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_session(closed, StateData) ->
    {stop, normal, StateData}.


session_established({xmlstreamelement, El}, StateData) ->
    %% Check 'from' attribute in stanza RFC 3920 Section 9.1.2
    case check_from(El, StateData#state.jid) of
        'invalid-from' ->
	    send_element(StateData, ?INVALID_FROM),
	    send_trailer(StateData),
            {stop, normal, StateData};
         _ ->
            session_established2(El, StateData)
    end;

%% We hibernate the process to reduce memory consumption after a
%% configurable activity timeout
session_established(timeout, StateData) ->
    %% TODO: Options must be stored in state:
    Options = [],
    proc_lib:hibernate(?GEN_FSM, enter_loop,
		       [?MODULE, Options, session_established, StateData]),
    fsm_next_state(session_established, StateData);

session_established({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData),
    {stop, normal, StateData};

session_established({xmlstreamerror, "XML stanza is too big" = E}, StateData) ->
    send_element(StateData, ?POLICY_VIOLATION_ERR(StateData#state.lang, E)),
    send_trailer(StateData),
    {stop, normal, StateData};

session_established({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};

session_established(closed, StateData) ->
    {stop, normal, StateData}.

%% Process packets sent by user (coming from user on c2s XMPP
%% connection)
session_established2(El, StateData) ->
    try
	User = StateData#state.user,
	Server = StateData#state.server,
    
	FromJID = StateData#state.jid,
	To = exmpp_stanza:get_recipient(El),
	ToJID = case To of
		    undefined ->
            exmpp_jid:bare(StateData#state.jid);
		    _ ->
			exmpp_jid:parse(To)
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
		PresenceEl = ejabberd_hooks:run_fold(
			       c2s_update_presence,
			       Server,
			       NewEl,
			       [User, Server]),
		ejabberd_hooks:run(
		  user_send_packet,
		  Server,
		  [FromJID, ToJID, PresenceEl]),
		case {exmpp_jid:node(ToJID), 
              exmpp_jid:domain(ToJID), 
              exmpp_jid:resource(ToJID)} of
		    {User, Server,undefined} ->
			?DEBUG("presence_update(~p,~n\t~p,~n\t~p)",
			       [FromJID, PresenceEl, StateData]),
			presence_update(FromJID, PresenceEl,
					StateData);
		    _ ->
			presence_track(FromJID, ToJID, PresenceEl,
				       StateData)
		end;
	    #xmlel{ns = ?NS_JABBER_CLIENT, name = 'iq'} ->
		case exmpp_iq:xmlel_to_iq(El) of
		    #iq{kind = request, ns = Xmlns} = IQ_Rec
			    when Xmlns == ?NS_PRIVACY;
				 Xmlns == ?NS_BLOCKING ->
			process_privacy_iq(
			  FromJID, ToJID, IQ_Rec, StateData);
		    _ ->
			ejabberd_hooks:run(
			  user_send_packet,
			  Server,
			  [FromJID, ToJID, NewEl]),
			check_privacy_route(FromJID, StateData, FromJID,
					    ToJID, NewEl),
			StateData
		end;
	    #xmlel{ns = ?NS_JABBER_CLIENT, name = 'message'} ->
		ejabberd_hooks:run(user_send_packet,
				   Server,
				   [FromJID, ToJID, NewEl]),
		check_privacy_route(FromJID, StateData, FromJID,
				    ToJID, NewEl),
		StateData;
	    _ ->
		StateData
	end,
	ejabberd_hooks:run(c2s_loop_debug, [{xmlstreamelement, El}]),
	fsm_next_state(session_established, NewState)
    catch
	throw:{stringprep, _, _, _} ->
	    case exmpp_stanza:get_type(El) of
		<<"error">> ->
		    ok;
		<<"result">> ->
		    ok;
		_ ->
		    Err = exmpp_stanza:reply_with_error(El, 'jid-malformed'),
		    send_element(StateData, Err)
	    end,
	    ejabberd_hooks:run(c2s_loop_debug, [{xmlstreamelement, El}]),
	    fsm_next_state(session_established, StateData);
	throw:Exception ->
	    io:format("SESSION ESTABLISHED: Exception=~p~n", [Exception]),
	    fsm_next_state(session_established, StateData)
    end.


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
handle_event({migrate, Node, After}, StateName, StateData) when Node /= node() ->
    fsm_migrate(StateName, StateData, Node, After * 2);

handle_event({add_rosteritem, IJID, ISubscription}, StateName, StateData) ->
    NewStateData = roster_change(IJID, ISubscription, StateData),
    fsm_next_state(StateName, NewStateData);

handle_event({del_rosteritem, IJID}, StateName, StateData) ->
    NewStateData = roster_change(IJID, <<"none">>, StateData),
    fsm_next_state(StateName, NewStateData);

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
%TODO: for debug only
handle_sync_event(get_state,_From,StateName,StateData) ->
    {reply,{StateName, StateData}, StateName, StateData};

handle_sync_event({get_presence}, _From, StateName, StateData) ->
    User = binary_to_list(StateData#state.user),
    PresLast = StateData#state.pres_last,

    Show = case PresLast of
	undefined -> "unavailable";
	_         -> exmpp_presence:get_show(PresLast)
    end,
    Status = case PresLast of
	undefined -> "";
	_         -> exmpp_presence:get_status(PresLast)
    end,
    Resource = binary_to_list(StateData#state.resource),

    Reply = {User, Resource, atom_to_list(Show), Status},
    fsm_reply(Reply, StateName, StateData);

handle_sync_event(get_subscribed, _From, StateName, StateData) ->
    Subscribed = ?SETS:to_list(StateData#state.pres_f),
    {reply, Subscribed, StateName, StateData};


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
    _Lang = StateData#state.lang,
    send_element(StateData,
		 ?SERRT_CONFLICT), %% (Lang, "Replaced by new connection")),
    send_trailer(StateData),
    {stop, normal, StateData#state{authenticated = replaced}};
%% Process Packets that are to be send to the user
handle_info({route, From, To, Packet}, StateName, StateData) ->
    {Pass, NewAttrs, NewState} =
	case Packet of
	    #xmlel{attrs = Attrs} when ?IS_PRESENCE(Packet) ->
		State = ejabberd_hooks:run_fold(
			c2s_presence_in, StateData#state.server,
			StateData,
			[{From, To, Packet}]),
		case exmpp_presence:get_type(Packet) of
		    'probe' ->
			LFrom = jlib:short_prepd_jid(From),
			LBFrom = jlib:short_prepd_bare_jid(From),
			NewStateData =
			    case ?SETS:is_element(
				    LFrom, State#state.pres_a) orelse
				?SETS:is_element(
				   LBFrom, State#state.pres_a) of
				true ->
				    State;
				false ->
				    case ?SETS:is_element(
					    LFrom, State#state.pres_f) of
					true ->
					    A = ?SETS:add_element(
						   LFrom,
						   State#state.pres_a),
					    State#state{pres_a = A};
					false ->
					    case ?SETS:is_element(
						    LBFrom, State#state.pres_f) of
						true ->
						    A = ?SETS:add_element(
							   LBFrom,
							   State#state.pres_a),
						    State#state{pres_a = A};
						false ->
						    State
					    end
				    end
			    end,
			process_presence_probe(From, To, NewStateData),
			{false, Attrs, NewStateData};
		    'error' ->
			LFrom = jlib:short_prepd_jid(From),
			NewA = remove_element(LFrom,
					      State#state.pres_a),
			{true, Attrs, State#state{pres_a = NewA}};
		    'subscribe' ->
			SRes = is_privacy_allow(State, From, To, Packet, in),
			{SRes, Attrs, State};
		    'subscribed' ->
			SRes = is_privacy_allow(State, From, To, Packet, in),
			{SRes, Attrs, State};
		    'unsubscribe' ->
			SRes = is_privacy_allow(State, From, To, Packet, in),
			{SRes, Attrs, State};
		    'unsubscribed' ->
			SRes = is_privacy_allow(State, From, To, Packet, in),
			{SRes, Attrs, State};
		    _ ->
			case privacy_check_packet(State, From, To, Packet, in) of
			    allow ->
				LFrom = jlib:short_prepd_jid(From),
				LBFrom = jlib:short_prepd_bare_jid(From),
				case ?SETS:is_element(
					LFrom, State#state.pres_a) orelse
				    ?SETS:is_element(
				       LBFrom, State#state.pres_a) of
				    true ->
					{true, Attrs, State};
				    false ->
					case ?SETS:is_element(
						LFrom, State#state.pres_f) of
					    true ->
						A = ?SETS:add_element(
						       LFrom,
						       State#state.pres_a),
						{true, Attrs,
						 State#state{pres_a = A}};
					    false ->
						case ?SETS:is_element(
							LBFrom, State#state.pres_f) of
						    true ->
							A = ?SETS:add_element(
							       LBFrom,
							       State#state.pres_a),
							{true, Attrs,
							 State#state{pres_a = A}};
						    false ->
							{true, Attrs, State}
						end
					end
				end;
			    deny ->
				{false, Attrs, State}
			end
		end;
	    #xmlel{name = broadcast, attrs = Attrs} ->
		?DEBUG("broadcast~n~p~n", [Packet#xmlel.children]),
		case Packet#xmlel.ns of
		    roster_item ->
			IJID = exmpp_jid:make(exmpp_xml:get_attribute(Packet, <<"u">>, <<"">>),
                                      exmpp_xml:get_attribute(Packet, <<"s">>, <<"">>),
                                      exmpp_xml:get_attribute(Packet, <<"r">>, <<"">>)),
                        ISubscription = exmpp_xml:get_attribute(Packet, <<"subs">>, <<"none">>),
			{false, Attrs,
			 roster_change(IJID, ISubscription, StateData)};
		    exit ->
			Reason = exmpp_xml:get_attribute_as_list(Packet, <<"reason">>, "Unknown reason"),
			{exit, Attrs, Reason};
		    privacy_list ->
		        PrivListName = exmpp_xml:get_attribute_as_list(Packet, <<"list_name">>, "Unknown list name"),
			CDataString = exmpp_xml:get_cdata_as_list(Packet),
			{ok, A2, _} = erl_scan:string(CDataString),
			{_, W} = erl_parse:parse_exprs(A2),
			{value, PrivList, []} = erl_eval:exprs(W, []),
			case ejabberd_hooks:run_fold(
			       privacy_updated_list, StateData#state.server,
			       false,
			       [StateData#state.privacy_list,
				PrivList]) of
			    false ->
				{false, Attrs, StateData};
			    NewPL ->
				PrivPushEl = exmpp_server_privacy:list_push(
				  StateData#state.jid, PrivListName),
				send_element(StateData, PrivPushEl),
				{false, Attrs, StateData#state{privacy_list = NewPL}}
			end;
		    blocking ->
			CDataString = exmpp_xml:get_cdata_as_list(Packet),
			{ok, A2, _} = erl_scan:string(CDataString),
			{_, W} = erl_parse:parse_exprs(A2),
			{value, What, []} = erl_eval:exprs(W, []),
			route_blocking(What, StateData),
			{false, Attrs, StateData};
		    _ ->
			{false, Attrs, StateData}
		end;
	    #xmlel{attrs = Attrs} when ?IS_IQ(Packet) ->
		case exmpp_iq:is_request(Packet) of
		    true ->
			case exmpp_iq:get_request(Packet) of
			    #xmlel{ns = ?NS_LAST_ACTIVITY} ->
				LFrom = jlib:short_prepd_jid(From),
				LBFrom = jlib:short_prepd_bare_jid(From),
				DummyPresence = exmpp_presence:presence(available, ""),
			       HasFromSub = (?SETS:is_element(LFrom, StateData#state.pres_f) orelse ?SETS:is_element(LBFrom, StateData#state.pres_f))
				   andalso is_privacy_allow(StateData, To, From, DummyPresence, out),
			       case HasFromSub of
				   true ->
				       case privacy_check_packet(StateData, From, To, Packet, in) of
					   allow ->
					       {true, Attrs, StateData};
					   deny ->
					       {false, Attrs, StateData}
				       end;
				   _ ->
					Err = exmpp_server_session:error(Packet, 'forbidden'),
					send_element(StateData, Err),
				       {false, Attrs, StateData}
			       end;
			    _ ->
				case privacy_check_packet(StateData, From, To, Packet, in) of
				    allow ->
					{true, Attrs, StateData};
				    deny ->
					Res = exmpp_iq:error(Packet, 'feature-not-implemented'),
					ejabberd_router:route(To, From, Res),
					{false, Attrs, StateData}
				end
			end;
		    false ->
			{true, Attrs, StateData}
		end;
	    #xmlel{attrs = Attrs} when ?IS_MESSAGE(Packet) ->
		case privacy_check_packet(StateData, From, To, Packet, in) of
		    allow ->
			{true, Attrs, StateData};
		    deny ->
			{false, Attrs, StateData}
		end;
	    #xmlel{attrs = Attrs} ->
		{true, Attrs, StateData}
	end,
    if
	Pass == exit ->
	    %% When Pass==exit, NewState contains a string instead of a #state{}
	    _Lang = StateData#state.lang,
	    send_element(StateData, ?SERRT_CONFLICT), %% (Lang, NewState)),
	    send_trailer(StateData),
	    {stop, normal, StateData};
	Pass ->
	    Attrs2 = exmpp_stanza:set_sender_in_attrs(NewAttrs, From),
	    Attrs3 = exmpp_stanza:set_recipient_in_attrs(Attrs2, To),
	    FixedPacket = Packet#xmlel{attrs = Attrs3},
	    send_element(StateData, FixedPacket),
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
handle_info(system_shutdown, StateName, StateData) ->
    case StateName of
	wait_for_stream ->
	    send_header(StateData#state.streamid, ?MYNAME, "1.0", ""),
	    send_element(StateData, exmpp_stream:error('system-shutdown')),
	    send_trailer(StateData),
	    ok;
	_ ->
	    send_element(StateData, exmpp_stream:error('system-shutdown')),
	    send_trailer(StateData),
	    ok
    end,
    {stop, normal, StateData};
handle_info({force_update_presence, LUser}, StateName,
            #state{user = LUser, server = LServer} = StateData) ->
    NewStateData =
	case exmpp_presence:is_presence(StateData#state.pres_last) of
	    true ->
		PresenceEl = ejabberd_hooks:run_fold(
			       c2s_update_presence,
			       LServer,
			       StateData#state.pres_last,
			       [LUser, LServer]),
		StateData2 = StateData#state{pres_last = PresenceEl},
		presence_update(StateData2#state.jid,
				PresenceEl,
				StateData2),
		StateData2;
	    false ->
		StateData
	end,
    {next_state, StateName, NewStateData};
handle_info({broadcast, Type, From, Packet}, StateName, StateData) ->
    Recipients = ejabberd_hooks:run_fold(
		    c2s_broadcast_recipients, StateData#state.server,
		    [],
		    [StateData, Type, From, Packet]),
    lists:foreach(fun(USR) ->
	ejabberd_router:route(From, exmpp_jid:make(USR), Packet)
    end, lists:usort(Recipients)),
    fsm_next_state(StateName, StateData);
handle_info(Info, StateName, StateData) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    fsm_next_state(StateName, StateData).


%%----------------------------------------------------------------------
%% Func: print_state/1
%% Purpose: Prepare the state to be printed on error log
%% Returns: State to print
%%----------------------------------------------------------------------
print_state(State = #state{pres_t = T, pres_f = F, pres_a = A, pres_i = I}) ->
   State#state{pres_t = {pres_t, ?SETS:size(T)},
               pres_f = {pres_f, ?SETS:size(F)},
               pres_a = {pres_a, ?SETS:size(A)},
               pres_i = {pres_i, ?SETS:size(I)}
               }.
    
%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate({migrated, ClonePid}, StateName, StateData) ->
    if StateName == session_established ->
	    ?INFO_MSG("(~w) Migrating ~s to ~p on node ~p",
		      [StateData#state.socket,
		       exmpp_jid:to_binary(StateData#state.jid),
		       ClonePid, node(ClonePid)]),
	    ejabberd_sm:close_session(StateData#state.sid,
				      StateData#state.jid);
       true ->
	    ok
    end,
    (StateData#state.sockmod):change_controller(
      StateData#state.socket, ClonePid),
    ok;
terminate(_Reason, StateName, StateData) ->
    %%TODO: resource could be 'undefined' if terminate before bind?
    case StateName of
	session_established ->
	    case StateData#state.authenticated of
		replaced ->
		    ?INFO_MSG("(~w) Replaced session for ~s",
			      [StateData#state.socket,
			       exmpp_jid:to_binary(StateData#state.jid)]),
		    From = StateData#state.jid,
		    Packet = exmpp_presence:unavailable(),
		    Packet1 = exmpp_presence:set_status(Packet,
		      "Replaced by new connection"),
		    ejabberd_sm:close_session_unset_presence(
		      StateData#state.sid,
		      StateData#state.jid,
		      "Replaced by new connection"),
		    presence_broadcast(
		      StateData, From, StateData#state.pres_a, Packet1),
		    presence_broadcast(
		      StateData, From, StateData#state.pres_i, Packet1);
		_ ->
		    ?INFO_MSG("(~w) Close session for ~s",
			      [StateData#state.socket,
			       exmpp_jid:to_binary(StateData#state.jid)]),

		    EmptySet = ?SETS:new(),
		    case StateData of
			#state{pres_last = undefined,
			       pres_a = EmptySet,
			       pres_i = EmptySet} ->
			    ejabberd_sm:close_session(StateData#state.sid,
                      StateData#state.jid);
			_ ->
			    From = StateData#state.jid,
			    Packet = exmpp_presence:unavailable(),
			    ejabberd_sm:close_session_unset_presence(
			      StateData#state.sid,
                  StateData#state.jid,
			      ""),
			    presence_broadcast(
			      StateData, From, StateData#state.pres_a, Packet),
			    presence_broadcast(
			      StateData, From, StateData#state.pres_i, Packet)
		    end
	    end,
	    bounce_messages();
	_ ->
	    ok
    end,
    (StateData#state.sockmod):close(StateData#state.socket),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

change_shaper(StateData, JID) ->
    Shaper = acl:match_rule(binary_to_list(StateData#state.server),
			    StateData#state.shaper, JID),
    (StateData#state.sockmod):change_shaper(StateData#state.socket, Shaper).

send_text(StateData, Text) when StateData#state.xml_socket ->
    ?DEBUG("Send Text on stream = ~p", [lists:flatten(Text)]),
    Text1 =
        if ?FLASH_HACK and StateData#state.flash_connection ->
                %% send a null byte after each stanza to Flash clients
                [Text, 0];
           true ->
                Text
        end,
    (StateData#state.sockmod):send_xml(StateData#state.socket, 
				       {xmlstreamraw, Text1});
send_text(StateData, Text) ->
    ?DEBUG("Send XML on stream = ~s", [Text]),
    (StateData#state.sockmod):send(StateData#state.socket, Text).

send_element(StateData, #xmlel{ns = ?NS_XMPP, name = 'stream'} = El) ->
    send_text(StateData, exmpp_stream:to_iolist(El));
send_element(StateData, El) when StateData#state.xml_socket ->
    (StateData#state.sockmod):send_xml(StateData#state.socket,
				       {xmlstreamelement, El});
send_element(StateData, El) ->
    send_text(StateData, exmpp_stanza:to_iolist(El)).

send_header(StateData,Server, Version, Lang)
  when StateData#state.flash_connection ->
    Header = io_lib:format(?FLASH_STREAM_HEADER,
			   [StateData#state.streamid,
			    Server,
			    Version,
			    Lang]),
    send_text(StateData, Header);

send_header(StateData, Server, Version, Lang)
  when StateData#state.xml_socket ->
    VersionAttr =
	case Version of
	    "" -> [];
	    _ -> [?XMLATTR(<<"version">>, Version)]
	end,
    LangAttr =
	case Lang of
	    "" -> [];
	    _ -> [?XMLATTR(<<"xml:lang">>, Lang)]
	end,
    Header =
	{xmlstreamstart,
	 "stream:stream",
	 VersionAttr ++
	 LangAttr ++
	 [?XMLATTR(<<"xmlns">>, "jabber:client"),
	  ?XMLATTR(<<"xmlns:stream">>, "http://etherx.jabber.org/streams"),
	  ?XMLATTR(<<"id">>, StateData#state.streamid),
	  ?XMLATTR(<<"from">>, Server)]},
    (StateData#state.sockmod):send_xml(
      StateData#state.socket, Header);
send_header(StateData, Server, Version, Lang) ->
    VersionStr =
	case Version of
	    "" -> "";
	    _ -> [" version='", Version, "'"]
	end,
    LangStr =
	case Lang of
	    "" -> "";
	    _ -> [" xml:lang='", Lang, "'"]
	end,
    Header = io_lib:format(?STREAM_HEADER,
			   [StateData#state.streamid,
			    Server,
			    VersionStr,
			    LangStr]),
    send_text(StateData, Header).

send_trailer(StateData) when StateData#state.xml_socket ->
    (StateData#state.sockmod):send_xml(
      StateData#state.socket,
      {xmlstreamend, "stream:stream"});
send_trailer(StateData) ->
    send_element(StateData, exmpp_stream:closing()).


new_id() ->
    randoms:get_string().


is_auth_packet(El) when ?IS_IQ(El) ->
    case exmpp_iq:xmlel_to_iq(El) of
	#iq{ns = ?NS_LEGACY_AUTH, kind = 'request'} = IQ_Rec ->
	    Children = exmpp_xml:get_child_elements(IQ_Rec#iq.payload),
	    {auth, IQ_Rec#iq.id, IQ_Rec#iq.type,
	     get_auth_tags(Children , undefined, undefined, 
			   undefined, undefined)};
	 _ ->
	   false
    end;
is_auth_packet(_El) ->
    false.


get_auth_tags([#xmlel{ns = ?NS_LEGACY_AUTH, name = Name, children = Els} | L],
  U, P, D, R) ->
    CData = exmpp_xml:get_cdata_from_list_as_list(Els),
    case Name of
	'username' ->
	    get_auth_tags(L, CData, P, D, R);
	'password' ->
	    get_auth_tags(L, U, CData, D, R);
	'digest' ->
	    get_auth_tags(L, U, P, CData, R);
	'resource' ->
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
	ejabberd_zlib ->
	    if is_pid(StateData#state.socket) ->
		    unknown;
	       true ->
		    case ejabberd_zlib:get_sockmod
                        ((StateData#state.socket)#socket_state.socket) of
			gen_tcp -> c2s_compressed;
			tls -> c2s_compressed_tls
		    end
	    end;
	ejabberd_http_poll -> http_poll;
	ejabberd_http_bind -> http_bind;
	_ -> unknown
    end.

process_presence_probe(From, To, StateData) ->
    LFrom = jlib:short_prepd_jid(From),
    LBFrom = jlib:short_prepd_bare_jid(From),
    case StateData#state.pres_last of
	undefined ->
	    ok;
	_ ->
	    Cond1 = (?SETS:is_element(LFrom, StateData#state.pres_f)
			 orelse
			 ((LFrom /= LBFrom) andalso
			  ?SETS:is_element(LBFrom, StateData#state.pres_f)))
		andalso (not
			 (?SETS:is_element(LFrom, StateData#state.pres_i)
			  orelse
			  ((LFrom /= LBFrom) andalso
			   ?SETS:is_element(LBFrom, StateData#state.pres_i)))),
	    if
		Cond1 ->
		    Timestamp = StateData#state.pres_timestamp,
		    Packet = exmpp_xml:append_children(
			       StateData#state.pres_last,
			       %% To is the one sending the presence (the target of the probe)
			       [jlib:timestamp_to_xml(Timestamp, utc, To, ""),
				%% TODO: Delete the next line once XEP-0091 is Obsolete
				jlib:timestamp_to_xml(Timestamp)]),
		    case privacy_check_packet(StateData, To, From, Packet, out) of
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
		true ->
		    ok
	    end
    end.

%% User updates his presence (non-directed presence packet)
presence_update(From, Packet, StateData) ->
    case exmpp_presence:get_type(Packet) of
	'unavailable' ->
	    Status = case exmpp_presence:get_status(Packet) of
		undefined -> <<>>;
		S         -> S
	    end,
	    Info = [{ip, StateData#state.ip}, {conn, StateData#state.conn},
		    {auth_module, StateData#state.auth_module}],
	    ejabberd_sm:unset_presence(StateData#state.sid,
				       StateData#state.jid,
				       Status,
				       Info),
	    presence_broadcast(StateData, From, StateData#state.pres_a, Packet),
	    presence_broadcast(StateData, From, StateData#state.pres_i, Packet),
	    StateData#state{pres_last = undefined,
			    pres_timestamp = undefined,
			    pres_a = ?SETS:new(),
			    pres_i = ?SETS:new()};
	'error' ->
	    StateData;
	'probe' ->
	    StateData;
	'subscribe' ->
	    StateData;
	'subscribed' ->
	    StateData;
	'unsubscribe' ->
	    StateData;
	'unsubscribed' ->
	    StateData;
	_ ->
	    OldPriority = case StateData#state.pres_last of
			      undefined ->
				  0;
			      OldPresence ->
				  try
				      exmpp_presence:get_priority(OldPresence)
				  catch
				      _Exception -> 0
				  end
			  end,
	    NewPriority = try
		exmpp_presence:get_priority(Packet)
	    catch
		_Exception1 -> 0
	    end,
	    Timestamp = calendar:now_to_universal_time(now()),
	    update_priority(NewPriority, Packet, StateData),
	    FromUnavail = (StateData#state.pres_last == undefined),
	    ?DEBUG("from unavail = ~p~n", [FromUnavail]),
            NewStateData = StateData#state{pres_last = Packet,
                                           pres_timestamp = Timestamp},
	    NewState =
		if
		    FromUnavail ->
			ejabberd_hooks:run(user_available_hook,
					   NewStateData#state.server,
					   [NewStateData#state.jid]),
			if NewPriority >= 0 ->
				resend_offline_messages(NewStateData),
				resend_subscription_requests(NewStateData);
			   true ->
				ok
			end,
			presence_broadcast_first(From, NewStateData, Packet);
		    true ->
			presence_broadcast_to_trusted(NewStateData,
						      From,
						      NewStateData#state.pres_f,
						      NewStateData#state.pres_a,
						      Packet),
			if OldPriority < 0, NewPriority >= 0 ->
				resend_offline_messages(NewStateData);
			   true ->
				ok
			end,
                        NewStateData
		end,
	    NewState
    end.

%% User sends a directed presence packet
presence_track(From, To, Packet, StateData) ->
    LTo = jlib:short_prepd_jid(To),
    case exmpp_presence:get_type(Packet) of
	'unavailable' ->
	    check_privacy_route(From, StateData, From, To, Packet),
	    I = remove_element(LTo, StateData#state.pres_i),
	    A = remove_element(LTo, StateData#state.pres_a),
	    StateData#state{pres_i = I,
			    pres_a = A};
	'subscribe' ->
	    try_check_privacy_route(subscribe, StateData#state.user, StateData#state.server,
		From, StateData, exmpp_jid:bare(From), To, Packet),
	    StateData;
	'unsubscribe' ->
	    try_check_privacy_route(unsubscribe, StateData#state.user, StateData#state.server,
		From, StateData, exmpp_jid:bare(From), To, Packet),
	    StateData;
	'subscribed' ->
	    ejabberd_hooks:run(roster_out_subscription,
	             StateData#state.server,
			     [StateData#state.user, StateData#state.server, To, subscribed]),
	    check_privacy_route(From, StateData, exmpp_jid:bare(From),
				To, Packet),
	    StateData;
	'unsubscribed' ->
	    ejabberd_hooks:run(roster_out_subscription,
			     StateData#state.server,
			     [StateData#state.user, StateData#state.server, To, unsubscribed]),
	    check_privacy_route(From, StateData, exmpp_jid:bare(From),
				To, Packet),
	    StateData;
	'error' ->
	    check_privacy_route(From, StateData, From, To, Packet),
	    StateData;
	'probe' ->
	    check_privacy_route(From, StateData, From, To, Packet),
	    StateData;
	_ ->
	    check_privacy_route(From, StateData, From, To, Packet),
	    I = remove_element(LTo, StateData#state.pres_i),
	    A = ?SETS:add_element(LTo, StateData#state.pres_a),
	    StateData#state{pres_i = I,
			    pres_a = A}
    end.

%%% Check ACL before allowing to send a subscription stanza
try_check_privacy_route(Type, User, Server, From, StateData, FromRoute, To, Packet) ->
    JID1 = exmpp_jid:make(User, Server, undefined),
    Access = gen_mod:get_module_opt(Server, mod_roster, access, all),
    case acl:match_rule(Server, Access, JID1) of
	deny ->
	    %% Silently drop this (un)subscription request
	    ok;
	allow ->
	    ejabberd_hooks:run(roster_out_subscription,
			       Server,
			       [User, Server, To, Type]),
	    check_privacy_route(From, StateData, FromRoute,
				To, Packet)
    end.

check_privacy_route(From, StateData, FromRoute, To, Packet) ->
    case privacy_check_packet(StateData, From, To, Packet, out) of
	deny ->
	    Lang = StateData#state.lang,
	    ErrText = "Routing of this stanza was denied by your active privacy list",
            Err = exmpp_stanza:reply_with_error(Packet,
	       exmpp_stanza:error(Packet#xmlel.ns, 'not-acceptable',
	      {Lang, ErrText})),
	    send_element(StateData, Err),
	    ok;
	allow ->
	    ejabberd_router:route(FromRoute, To, Packet)
    end.

privacy_check_packet(StateData, From, To, Packet, Dir) ->
    ejabberd_hooks:run_fold(
      privacy_check_packet, StateData#state.server,
      allow,
      [StateData#state.user,
       StateData#state.server,
       StateData#state.privacy_list,
       {From, To, Packet},
       Dir]).

%% Check if privacy rules allow this delivery
is_privacy_allow(StateData, From, To, Packet, Dir) ->
    allow == privacy_check_packet(StateData, From, To, Packet, Dir).

%% Send presence when disconnecting
presence_broadcast(StateData, From, JIDSet, Packet) ->
    JIDs = ?SETS:to_list(JIDSet),
    JIDs2 = format_and_check_privacy(From, StateData, Packet, JIDs),
    Server = StateData#state.server,
    send_multiple(From, Server, JIDs2, Packet).

%% Send presence when updating presence
presence_broadcast_to_trusted(StateData, From, Trusted, JIDSet, Packet) ->
    JIDs = ?SETS:to_list(JIDSet),
    JIDs_trusted = [JID || JID <- JIDs, ?SETS:is_element(JID, Trusted)],
    JIDs2 = format_and_check_privacy(From, StateData, Packet, JIDs_trusted),
    Server = StateData#state.server,
    send_multiple(From, Server, JIDs2, Packet).

%% Send presence when connecting
presence_broadcast_first(From, StateData, Packet) ->
    JIDsProbe = 
	?SETS:fold(
	   fun(JID, L) -> [JID | L] end,
	   [],
	   StateData#state.pres_t),
    PacketProbe = exmpp_presence:probe(),
    JIDs2Probe = format_and_check_privacy(From, StateData, Packet, JIDsProbe),
    Server = StateData#state.server,
    send_multiple(From, Server, JIDs2Probe, PacketProbe),
	    {As, JIDs} = 
		?SETS:fold(
		   fun(JID, {A, JID_list}) ->
			   {?SETS:add_element(JID, A), JID_list++[JID]}
		   end,
		   {StateData#state.pres_a, []},
		   StateData#state.pres_f),
	    JIDs2 = format_and_check_privacy(From, StateData, Packet, JIDs),
	    Server = StateData#state.server,
	    send_multiple(From, Server, JIDs2, Packet),
	    StateData#state{pres_a = As}.
 
format_and_check_privacy(From, StateData, Packet, JIDs) ->
    FJIDs = [exmpp_jid:make(JID) || JID <- JIDs],
    lists:filter(
      fun(FJID) ->
	      case privacy_check_packet(StateData, From, FJID, Packet, out) of
		  deny -> false;
		  allow -> true
	      end
      end,
      FJIDs).

send_multiple(From, Server, JIDs, Packet) ->
    ejabberd_router_multicast:route_multicast(From, Server, JIDs, Packet).


remove_element(E, Set) ->
    case ?SETS:is_element(E, Set) of
	true ->
	    ?SETS:del_element(E, Set);
	_ ->
	    Set
    end.


roster_change(IJID, ISubscription, StateData) ->
    LIJID = jlib:short_prepd_jid(IJID),
    IsFrom = (ISubscription == <<"both">>) or (ISubscription == <<"from">>),
    IsTo   = (ISubscription == <<"both">>) or (ISubscription == <<"to">>),
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
	    To = IJID,
	    Cond1 = IsFrom and (not OldIsFrom),
	    Cond2 = (not IsFrom) and OldIsFrom
		and (?SETS:is_element(LIJID, StateData#state.pres_a) or
		     ?SETS:is_element(LIJID, StateData#state.pres_i)),
	    if
		Cond1 ->
		    ?DEBUG("C1: ~p~n", [LIJID]),
		    case privacy_check_packet(StateData, From, To, P, out) of
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
		    PU = exmpp_presence:unavailable(),
		    case privacy_check_packet(StateData, From, To, PU, out) of
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
    Info = [{ip, StateData#state.ip}, {conn, StateData#state.conn},
	    {auth_module, StateData#state.auth_module}],
    ejabberd_sm:set_presence(StateData#state.sid,
			     StateData#state.jid,
			     Priority,
			     Packet,
			     Info).

process_privacy_iq(From, To,
		   #iq{type = Type, iq_ns = IQ_NS} = IQ_Rec,
		   StateData) ->
    {Res, NewStateData} =
	case Type of
	    get ->
		R = ejabberd_hooks:run_fold(
		      privacy_iq_get, StateData#state.server ,
		      {error, ?ERR_FEATURE_NOT_IMPLEMENTED(IQ_NS)},
		      [From, To, IQ_Rec, StateData#state.privacy_list]),
		{R, StateData};
	    set ->
		case ejabberd_hooks:run_fold(
		       privacy_iq_set, StateData#state.server,
		       {error, ?ERR_FEATURE_NOT_IMPLEMENTED(IQ_NS)},
		       [From, To, IQ_Rec]) of
		    {result, R, NewPrivList} ->
			{{result, R},
			 StateData#state{privacy_list = NewPrivList}};
		    R -> {R, StateData}
		end
	end,
    IQRes =
	case Res of
	    {result, []} ->
		exmpp_iq:result(IQ_Rec);
	    {result, Result} ->
		exmpp_iq:result(IQ_Rec, Result);
	    {error, Error} ->
		exmpp_iq:error(IQ_Rec, Error)
	end,
    ejabberd_router:route(
      To, From, exmpp_iq:iq_to_xmlel(IQRes)),
    NewStateData.


resend_offline_messages(#state{user = UserB,
			       server = ServerB} = StateData) ->
    case ejabberd_hooks:run_fold(resend_offline_messages_hook,
				 StateData#state.server,
				 [],
				 [UserB, ServerB]) of
	Rs when is_list(Rs) ->
	    lists:foreach(
	      fun({route,
		   From, To, Packet}) ->
		      Pass = case privacy_check_packet(StateData, From, To, Packet, in) of
				 allow ->
				     true;
				 deny ->
				     false
			     end,
		      if
			  Pass ->
			      Attrs1 = exmpp_stanza:set_sender_in_attrs(
				Packet#xmlel.attrs, From),
			      Attrs2 = exmpp_stanza:set_recipient_in_attrs(
				Attrs1, To),
			      FixedPacket = Packet#xmlel{attrs = Attrs2},
			      send_element(StateData, FixedPacket),
			      ejabberd_hooks:run(user_receive_packet,
						 StateData#state.server,
						 [StateData#state.jid,
						  From, To, FixedPacket]);
			  true ->
			      ok
		      end
	      end, Rs)
    end.

resend_subscription_requests(#state{user = UserB,
				    server = ServerB} = StateData) ->
    PendingSubscriptions = ejabberd_hooks:run_fold(
			     resend_subscription_requests_hook,
			     StateData#state.server,
			     [],
			     [UserB, ServerB]),
    lists:foreach(fun(XMLPacket) ->
			  send_element(StateData,
				       XMLPacket)
		  end,
		  PendingSubscriptions).

process_unauthenticated_stanza(StateData, El) when ?IS_IQ(El) ->
    NewEl = case exmpp_stream:get_lang(El) of
		undefined ->
		    case StateData#state.lang of
			undefined -> El;
			Lang ->
			    exmpp_stanza:set_lang(El, Lang)
		    end;
		_ ->
		    El
	    end,
    case exmpp_iq:get_kind(NewEl) of
	request ->
            IQ_Rec = exmpp_iq:xmlel_to_iq(El),
	    Res = ejabberd_hooks:run_fold(c2s_unauthenticated_iq,
					  StateData#state.server,
					  empty,
					  [StateData#state.server, IQ_Rec,
					   StateData#state.ip]),
	    case Res of
		empty ->
		    % The only reasonable IQ's here are auth and register IQ's
		    % They contain secrets, so don't include subelements to response
		    ResIQ = exmpp_iq:error_without_original(El,
                      'service-unavailable'),
		    Res1 = exmpp_stanza:set_sender(ResIQ,
		      exmpp_jid:make(StateData#state.server)),
		    Res2 = exmpp_stanza:remove_recipient(Res1),
		    send_element(StateData, Res2);
		_ ->
		    send_element(StateData, Res)
	    end;
	_ ->
	    % Drop any stanza, which isn't an IQ request stanza
	    ok
    end;
process_unauthenticated_stanza(_StateData,_El) ->
    ok.


peerip(SockMod, Socket) ->
    IP = case SockMod of
	     gen_tcp -> inet:peername(Socket);
	     _ -> SockMod:peername(Socket)
	 end,
    case IP of
	{ok, IPOK} -> IPOK;
	_ -> undefined
    end.

maybe_migrate(StateName, StateData) ->
    case ejabberd_cluster:get_node({StateData#state.user,
				    StateData#state.server}) of
	Node when Node == node() ->
	    Conn = get_conn_type(StateData),
	    Info = [{ip, StateData#state.ip}, {conn, Conn},
		    {auth_module, StateData#state.auth_module}],
	    #state{user = U, server = S, jid = JID, sid = SID} = StateData,
	    ejabberd_sm:open_session(SID, JID, Info),
	    case ejabberd_cluster:get_node_new({U, S}) of
		Node ->
		    ok;
		NewNode ->
		    After = ejabberd_cluster:rehash_timeout(),
		    migrate(self(), NewNode, After)
	    end,
	    fsm_next_state_pack(StateName, StateData); 
	Node ->
	    fsm_migrate(StateName, StateData, Node, 0)
    end.

%% fsm_next_state_pack: Pack the StateData structure to improve
%% sharing.
fsm_next_state_pack(StateName, StateData) ->
    fsm_next_state_gc(StateName, pack(StateData)).

%% fsm_next_state_gc: Garbage collect the process heap to make use of
%% the newly packed StateData structure.
fsm_next_state_gc(StateName, PackedStateData) ->
    erlang:garbage_collect(),
    fsm_next_state(StateName, PackedStateData).

%% fsm_next_state: Generate the next_state FSM tuple with different
%% timeout, depending on the future state
fsm_next_state(session_established, StateData) ->
    {next_state, session_established, StateData, ?C2S_HIBERNATE_TIMEOUT};
fsm_next_state(StateName, StateData) ->
    {next_state, StateName, StateData, ?C2S_OPEN_TIMEOUT}.

fsm_migrate(StateName, StateData, Node, Timeout) ->
    {migrate, StateData,
     {Node, ?MODULE, start, [StateName, StateData]}, Timeout}.

%% fsm_reply: Generate the reply FSM tuple with different timeout,
%% depending on the future state
fsm_reply(Reply, session_established, StateData) ->
    {reply, Reply, session_established, StateData, ?C2S_HIBERNATE_TIMEOUT};
fsm_reply(Reply, StateName, StateData) ->
    {reply, Reply, StateName, StateData, ?C2S_OPEN_TIMEOUT}.

%% Used by c2s blacklist plugins
is_ip_blacklisted(undefined) ->
    false;
is_ip_blacklisted({IP,_Port}) ->
    ejabberd_hooks:run_fold(check_bl_c2s, false, [IP]).

%% Check from attributes
%% returns invalid-from|NewElement
check_from(El, FromJID) ->
    case exmpp_stanza:get_sender(El) of
	undefined ->
	    El;
	SJID when is_binary(SJID) ->
	    try
		JIDEl = exmpp_jid:parse(SJID),
		case exmpp_jid:prep_resource(JIDEl) of 
		    undefined ->
			%% Matching JID: The stanza is ok
			case exmpp_jid:bare_compare(JIDEl, FromJID) of
			    true ->
				El;
			   false ->
				'invalid-from'
			end;
		    _ ->
			%% Matching JID: The stanza is ok
			case exmpp_jid:compare(JIDEl, FromJID) of
			    true ->
				El;
			    false ->
			       'invalid-from'
			end
		end
	    catch
		_:_ ->
		    'invalid-from'
	    end
    end.

fsm_limit_opts(Opts) ->
    case lists:keysearch(max_fsm_queue, 1, Opts) of
	{value, {_, N}} when is_integer(N) ->
	    [{max_queue, N}];
	_ ->
	    case ejabberd_config:get_local_option(max_fsm_queue) of
		N when is_integer(N) ->
		    [{max_queue, N}];
		_ ->
		    []
	    end
    end.

bounce_messages() ->
    receive
	{route, From, To, El} ->
	    ejabberd_router:route(From, To, El),
	    bounce_messages()
    after 0 ->
	    ok
    end.


%%%----------------------------------------------------------------------
%%% XEP-0191
%%%----------------------------------------------------------------------

route_blocking(What, StateData) ->
    SubEl =
	case What of
	    {Action, JIDs} when (Action == block) or (Action == unblock) ->
		UnblockJids =
		    lists:map(
		      fun(JidString) ->
			      exmpp_xml:set_attribute(#xmlel{ns = ?NS_BLOCKING,
							     name = item},
						      <<"jid">>,
						      JidString)
		      end, JIDs),
		#xmlel{ns = ?NS_BLOCKING, name = Action,
		       children = UnblockJids};
	    unblock_all ->
		#xmlel{ns = ?NS_BLOCKING, name = 'unblock'}
	end,
    El1 = exmpp_iq:set(?NS_BLOCKING, SubEl, random),
    El2 = exmpp_stanza:set_sender(El1, exmpp_jid:bare(StateData#state.jid)),
    El3 = exmpp_stanza:set_recipient(El2, StateData#state.jid),
    send_element(StateData, El3),
    %% No need to replace active privacy list here,
    %% blocking pushes are always accompanied by
    %% Privacy List pushes
    ok.

%%%----------------------------------------------------------------------
%%% JID Set memory footprint reduction code
%%%----------------------------------------------------------------------

%% Try to reduce the heap footprint of the four presence sets
%% by ensuring that we re-use strings and Jids wherever possible.
pack(S = #state{pres_a=A,
                pres_i=I,
                pres_f=F,
                pres_t=T}) ->
    {NewA, Pack1} = pack_jid_set(A, gb_trees:empty()),
    {NewI, Pack2} = pack_jid_set(I, Pack1),
    {NewF, Pack3} = pack_jid_set(F, Pack2),
    {NewT, _Pack4} = pack_jid_set(T, Pack3),
    %% Throw away Pack4 so that if we delete references to
    %% Strings or Jids in any of the sets there will be
    %% no live references for the GC to find.
    S#state{pres_a=NewA,
            pres_i=NewI,
            pres_f=NewF,
            pres_t=NewT}.

pack_jid_set(Set, Pack) ->
    Jids = ?SETS:to_list(Set),
    {PackedJids, NewPack} = pack_jids(Jids, Pack, []),
    {?SETS:from_list(PackedJids), NewPack}.

pack_jids([], Pack, Acc) -> {Acc, Pack};
pack_jids([{U,S,R}=Jid | Jids], Pack, Acc) ->
    case gb_trees:lookup(Jid, Pack) of
        {value, PackedJid} ->
            pack_jids(Jids, Pack, [PackedJid | Acc]);
        none ->
            {NewU, Pack1} = pack_string(U, Pack),
            {NewS, Pack2} = pack_string(S, Pack1),
            {NewR, Pack3} = pack_string(R, Pack2),
            NewJid = {NewU, NewS, NewR},
            NewPack = gb_trees:insert(NewJid, NewJid, Pack3),
            pack_jids(Jids, NewPack, [NewJid | Acc])
    end.

pack_string(String, Pack) ->
    case gb_trees:lookup(String, Pack) of
        {value, PackedString} ->
            {PackedString, Pack};
        none ->
            {String, gb_trees:insert(String, String, Pack)}
    end.


%% @spec () -> string()
%% @doc Build the content of a Flash policy file.
%% It specifies as domain "*".
%% It specifies as to-ports the ports that serve ejabberd_c2s.
flash_policy_string() ->
    Listen = ejabberd_config:get_local_option(listen),
    ClientPortsDeep = ["," ++ integer_to_list(Port)
		       || {{Port,_,_}, ejabberd_c2s, _Opts} <- Listen],
    %% NOTE: The function string:join/2 was introduced in Erlang/OTP R12B-0
    %% so it can't be used yet in ejabberd.
    ToPortsString = case lists:flatten(ClientPortsDeep) of
			[$, | Tail] -> Tail;
			_ -> []
		    end,

    "<?xml version=\"1.0\"?>\n"
	"<!DOCTYPE cross-domain-policy SYSTEM "
        "\"http://www.macromedia.com/xml/dtds/cross-domain-policy.dtd\">\n"
	"<cross-domain-policy>\n"
	"  <allow-access-from domain=\"*\" to-ports=\""
	++ ToPortsString ++
	"\"/>\n"
	"</cross-domain-policy>\n\0".
