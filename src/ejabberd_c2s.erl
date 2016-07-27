%%%----------------------------------------------------------------------
%%% File    : ejabberd_c2s.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve C2S connection
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_c2s).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-protocol({xep, 78, '2.5'}).
-protocol({xep, 138, '2.0'}).
-protocol({xep, 198, '1.3'}).

-update_info({update, 0}).

-define(GEN_FSM, p1_fsm).

-behaviour(?GEN_FSM).

%% External exports
-export([start/2,
	 stop/1,
	 start_link/2,
	 close/1,
	 send_text/2,
	 send_element/2,
	 socket_type/0,
	 get_presence/1,
	 get_aux_field/2,
	 set_aux_field/3,
	 del_aux_field/2,
	 get_subscription/2,
	 send_filtered/5,
	 broadcast/4,
	 get_subscribed/1,
         transform_listen_option/2]).

-export([init/1, wait_for_stream/2, wait_for_auth/2,
	 wait_for_feature_request/2, wait_for_bind/2,
	 wait_for_sasl_response/2,
	 wait_for_resume/2, session_established/2,
	 handle_event/3, handle_sync_event/4, code_change/4,
	 handle_info/3, terminate/3, print_state/1, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").
%%-include("legacy.hrl").

-include("mod_privacy.hrl").

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
		user = <<"">>, server = <<"">>, resource = <<"">>,
		sid,
		pres_t = ?SETS:new(),
		pres_f = ?SETS:new(),
		pres_a = ?SETS:new(),
		pres_last,
		pres_timestamp,
		privacy_list = #userlist{},
		conn = unknown,
		auth_module = unknown,
		ip,
		aux_fields = [],
		csi_state = active,
		mgmt_state,
		mgmt_xmlns,
		mgmt_queue,
		mgmt_max_queue,
		mgmt_pending_since,
		mgmt_timeout,
		mgmt_max_timeout,
		mgmt_resend,
		mgmt_stanzas_in = 0,
		mgmt_stanzas_out = 0,
		ask_offline = true,
		lang = <<"">>}).

-type state_name() :: wait_for_stream | wait_for_auth |
		      wait_for_feature_request | wait_for_bind |
		      wait_for_sasl_response | wait_for_resume |
		      session_established.
-type state() :: #state{}.
-type fsm_stop() :: {stop, normal, state()}.
-type fsm_next() :: {next_state, state_name(), state(), non_neg_integer()}.
-type fsm_transition() :: fsm_stop() | fsm_next().
-export_type([state/0]).

%-define(DBGFSM, true).

-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

-endif.

%% This is the timeout to apply between event when starting a new
%% session:
-define(C2S_OPEN_TIMEOUT, 60000).

-define(C2S_HIBERNATE_TIMEOUT, ejabberd_config:get_option(c2s_hibernate, fun(X) when is_integer(X); X == hibernate-> X end, 90000)).

-define(STREAM_HEADER,
	<<"<?xml version='1.0'?><stream:stream "
	  "xmlns='jabber:client' xmlns:stream='http://et"
	  "herx.jabber.org/streams' id='~s' from='~s'~s"
	  "~s>">>).

-define(STREAM_TRAILER, <<"</stream:stream>">>).

%% XEP-0198:

-define(IS_STREAM_MGMT_PACKET(Pkt),
	is_record(Pkt, sm_enable) or
	is_record(Pkt, sm_resume) or
	is_record(Pkt, sm_a) or
	is_record(Pkt, sm_r)).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(SockData, Opts) ->
    ?GEN_FSM:start(ejabberd_c2s,
		   [SockData, Opts],
		   fsm_limit_opts(Opts) ++ ?FSMOPTS).

start_link(SockData, Opts) ->
    (?GEN_FSM):start_link(ejabberd_c2s,
			  [SockData, Opts],
			  fsm_limit_opts(Opts) ++ ?FSMOPTS).

socket_type() -> xml_stream.

%% Return Username, Resource and presence information
get_presence(FsmRef) ->
    (?GEN_FSM):sync_send_all_state_event(FsmRef,
					 {get_presence}, 1000).

-spec get_aux_field(any(), state()) -> {ok, any()} | error.
get_aux_field(Key, #state{aux_fields = Opts}) ->
    case lists:keysearch(Key, 1, Opts) of
      {value, {_, Val}} -> {ok, Val};
      _ -> error
    end.

-spec set_aux_field(any(), any(), state()) -> state().
set_aux_field(Key, Val,
	      #state{aux_fields = Opts} = State) ->
    Opts1 = lists:keydelete(Key, 1, Opts),
    State#state{aux_fields = [{Key, Val} | Opts1]}.

-spec del_aux_field(any(), state()) -> state().
del_aux_field(Key, #state{aux_fields = Opts} = State) ->
    Opts1 = lists:keydelete(Key, 1, Opts),
    State#state{aux_fields = Opts1}.

-spec get_subscription(jid() | ljid(), state()) -> both | from | to | none.
get_subscription(From = #jid{}, StateData) ->
    get_subscription(jid:tolower(From), StateData);
get_subscription(LFrom, StateData) ->
    LBFrom = setelement(3, LFrom, <<"">>),
    F = (?SETS):is_element(LFrom, StateData#state.pres_f)
	  orelse
	  (?SETS):is_element(LBFrom, StateData#state.pres_f),
    T = (?SETS):is_element(LFrom, StateData#state.pres_t)
	  orelse
	  (?SETS):is_element(LBFrom, StateData#state.pres_t),
    if F and T -> both;
       F -> from;
       T -> to;
       true -> none
    end.

-spec send_filtered(pid(), binary(), jid(), jid(), stanza()) -> any().
send_filtered(FsmRef, Feature, From, To, Packet) ->
    FsmRef ! {send_filtered, Feature, From, To, Packet}.

-spec broadcast(pid(), any(), jid(), stanza()) -> any().
broadcast(FsmRef, Type, From, Packet) ->
    FsmRef ! {broadcast, Type, From, Packet}.

-spec stop(pid()) -> any().
stop(FsmRef) -> (?GEN_FSM):send_event(FsmRef, stop).

-spec close(pid()) -> any().
%% What is the difference between stop and close???
close(FsmRef) -> (?GEN_FSM):send_event(FsmRef, closed).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

init([{SockMod, Socket}, Opts]) ->
    Access = gen_mod:get_opt(access, Opts,
			     fun acl:access_rules_validator/1, all),
    Shaper = gen_mod:get_opt(shaper, Opts,
			     fun acl:shaper_rules_validator/1, none),
    XMLSocket = case lists:keysearch(xml_socket, 1, Opts) of
		  {value, {_, XS}} -> XS;
		  _ -> false
		end,
    Zlib = proplists:get_bool(zlib, Opts),
    StartTLS = proplists:get_bool(starttls, Opts),
    StartTLSRequired = proplists:get_bool(starttls_required, Opts),
    TLSEnabled = proplists:get_bool(tls, Opts),
    TLS = StartTLS orelse
	    StartTLSRequired orelse TLSEnabled,
    TLSOpts1 = lists:filter(fun ({certfile, _}) -> true;
				({ciphers, _}) -> true;
				({dhfile, _}) -> true;
				(_) -> false
			    end,
			    Opts),
    TLSOpts2 = case lists:keysearch(protocol_options, 1, Opts) of
                   {value, {_, O}} ->
                       [_|ProtocolOptions] = lists:foldl(
                                    fun(X, Acc) -> X ++ Acc end, [],
                                    [["|" | binary_to_list(Opt)] || Opt <- O, is_binary(Opt)]
                                   ),
                        [{protocol_options, iolist_to_binary(ProtocolOptions)} | TLSOpts1];
                   _ -> TLSOpts1
               end,
    TLSOpts3 = case proplists:get_bool(tls_compression, Opts) of
                   false -> [compression_none | TLSOpts2];
                   true -> TLSOpts2
               end,
    TLSOpts = [verify_none | TLSOpts3],
    StreamMgmtEnabled = proplists:get_value(stream_management, Opts, true),
    StreamMgmtState = if StreamMgmtEnabled -> inactive;
			 true -> disabled
		      end,
    MaxAckQueue = case proplists:get_value(max_ack_queue, Opts) of
		    Limit when is_integer(Limit), Limit > 0 -> Limit;
		    infinity -> infinity;
		    _ -> 1000
		  end,
    ResumeTimeout = case proplists:get_value(resume_timeout, Opts) of
		      Timeout when is_integer(Timeout), Timeout >= 0 -> Timeout;
		      _ -> 300
		    end,
    MaxResumeTimeout = case proplists:get_value(max_resume_timeout, Opts) of
			 Max when is_integer(Max), Max >= ResumeTimeout -> Max;
			 _ -> ResumeTimeout
		       end,
    ResendOnTimeout = case proplists:get_value(resend_on_timeout, Opts) of
			Resend when is_boolean(Resend) -> Resend;
			if_offline -> if_offline;
			_ -> false
		      end,
    IP = peerip(SockMod, Socket),
    Socket1 = if TLSEnabled andalso
		 SockMod /= ejabberd_frontend_socket ->
		      SockMod:starttls(Socket, TLSOpts);
		 true -> Socket
	      end,
    SocketMonitor = SockMod:monitor(Socket1),
    StateData = #state{socket = Socket1, sockmod = SockMod,
		       socket_monitor = SocketMonitor,
		       xml_socket = XMLSocket, zlib = Zlib, tls = TLS,
		       tls_required = StartTLSRequired,
		       tls_enabled = TLSEnabled, tls_options = TLSOpts,
		       sid = ejabberd_sm:make_sid(), streamid = new_id(),
		       access = Access, shaper = Shaper, ip = IP,
		       mgmt_state = StreamMgmtState,
		       mgmt_max_queue = MaxAckQueue,
		       mgmt_timeout = ResumeTimeout,
		       mgmt_max_timeout = MaxResumeTimeout,
		       mgmt_resend = ResendOnTimeout},
    {ok, wait_for_stream, StateData, ?C2S_OPEN_TIMEOUT}.

-spec get_subscribed(pid()) -> [ljid()].
%% Return list of all available resources of contacts,
get_subscribed(FsmRef) ->
    (?GEN_FSM):sync_send_all_state_event(FsmRef,
					 get_subscribed, 1000).

wait_for_stream({xmlstreamstart, Name, Attrs}, StateData) ->
    try xmpp:decode(#xmlel{name = Name, attrs = Attrs}) of
	#stream_start{xmlns = NS_CLIENT, stream_xmlns = NS_STREAM, lang = Lang}
          when NS_CLIENT /= ?NS_CLIENT; NS_STREAM /= ?NS_STREAM ->
	    send_header(StateData, ?MYNAME, <<"">>, Lang),
            send_element(StateData, xmpp:serr_invalid_namespace()),
            {stop, normal, StateData};
	#stream_start{lang = Lang} when byte_size(Lang) > 35 ->
	    %% As stated in BCP47, 4.4.1:
	    %% Protocols or specifications that specify limited buffer sizes for
	    %% language tags MUST allow for language tags of at least 35 characters.
	    %% Do not store long language tag to avoid possible DoS/flood attacks
	    send_header(StateData, ?MYNAME, <<"">>, ?MYLANG),
	    Txt = <<"Too long value of 'xml:lang' attribute">>,
	    send_element(StateData,
			 xmpp:serr_policy_violation(Txt, ?MYLANG)),
	    {stop, normal, StateData};
	#stream_start{to = undefined, lang = Lang} ->
	    Txt = <<"Missing 'to' attribute">>,
	    send_header(StateData, ?MYNAME, <<"">>, Lang),
	    send_element(StateData,
			 xmpp:serr_improper_addressing(Txt, Lang)),
	    {stop, normal, StateData};
	#stream_start{to = #jid{lserver = To}, lang = Lang,
		      version = Version} ->
	    Server = case StateData#state.server of
			 <<"">> -> To;
			 S -> S
		     end,
	    StreamVersion = case Version of
				<<"1.0">> -> <<"1.0">>;
				_ -> <<"">>
			    end,
	    IsBlacklistedIP = is_ip_blacklisted(StateData#state.ip, Lang),
	    case lists:member(Server, ?MYHOSTS) of
		true when IsBlacklistedIP == false ->
		    change_shaper(StateData, jid:make(<<"">>, Server, <<"">>)),
		    case StreamVersion of
			<<"1.0">> ->
			    send_header(StateData, Server, <<"1.0">>, ?MYLANG),
			    case StateData#state.authenticated of
				false ->
				    TLS = StateData#state.tls,
				    TLSEnabled = StateData#state.tls_enabled,
				    TLSRequired = StateData#state.tls_required,
				    SASLState = cyrsasl:server_new(
					    <<"jabber">>, Server, <<"">>, [],
					    fun (U) ->
						    ejabberd_auth:get_password_with_authmodule(
							U, Server)
					    end,
					  fun(U, AuthzId, P) ->
						    ejabberd_auth:check_password_with_authmodule(
						    U, AuthzId, Server, P)
					    end,
					  fun(U, AuthzId, P, D, DG) ->
						    ejabberd_auth:check_password_with_authmodule(
						    U, AuthzId, Server, P, D, DG)
					    end),
				    Mechs =
					case TLSEnabled or not TLSRequired of
					true ->
					    [#sasl_mechanisms{list = cyrsasl:listmech(Server)}];
					false ->
					    []
				    end,
				    SockMod =
					(StateData#state.sockmod):get_sockmod(StateData#state.socket),
				    Zlib = StateData#state.zlib,
				    CompressFeature = case Zlib andalso
					((SockMod == gen_tcp) orelse (SockMod == fast_tls)) of
					true ->
					    [#compression{methods = [<<"zlib">>]}];
					_ ->
					    []
				    end,
				    TLSFeature =
					case (TLS == true) andalso
					(TLSEnabled == false) andalso
					(SockMod == gen_tcp) of
					true ->
					    [#starttls{required = TLSRequired}];
					false ->
					    []
				    end,
				    StreamFeatures1 = TLSFeature ++ CompressFeature ++ Mechs,
				    StreamFeatures = ejabberd_hooks:run_fold(c2s_stream_features,
					    Server, StreamFeatures1, [Server]),
				    send_element(StateData,
						 #stream_features{sub_els = StreamFeatures}),
				    fsm_next_state(wait_for_feature_request,
					StateData#state{server = Server,
					    sasl_state = SASLState,
					    lang = Lang});
				_ ->
				    case StateData#state.resource of
					<<"">> ->
					    RosterVersioningFeature =
						ejabberd_hooks:run_fold(roster_get_versioning_feature,
						    Server, [],
						    [Server]),
					    StreamManagementFeature =
						case stream_mgmt_enabled(StateData) of
						true ->
						    [#feature_sm{xmlns = ?NS_STREAM_MGMT_2},
						     #feature_sm{xmlns = ?NS_STREAM_MGMT_3}];
						false ->
						    []
					    end,
					    SockMod =
						(StateData#state.sockmod):get_sockmod(
						  StateData#state.socket),
					    Zlib = StateData#state.zlib,
					    CompressFeature =
						case Zlib andalso
						    ((SockMod == gen_tcp) orelse (SockMod == fast_tls)) of
						    true ->
							[#compression{methods = [<<"zlib">>]}];
						    _ ->
							[]
						end,
					    StreamFeatures1 =
						[#bind{}, #xmpp_session{optional = true}]
						++
						RosterVersioningFeature ++
						StreamManagementFeature ++
						CompressFeature ++
						ejabberd_hooks:run_fold(c2s_post_auth_features,
						    Server, [], [Server]),
					    StreamFeatures = ejabberd_hooks:run_fold(c2s_stream_features,
						    Server, StreamFeatures1, [Server]),
					    send_element(StateData,
							 #stream_features{sub_els = StreamFeatures}),
					    fsm_next_state(wait_for_bind,
						StateData#state{server = Server, lang = Lang});
					_ ->
					    send_element(StateData, #stream_features{}),
					    fsm_next_state(session_established,
						StateData#state{server = Server, lang = Lang})
				    end
			    end;
			_ ->
			    send_header(StateData, Server, <<"">>, ?MYLANG),
			    if not StateData#state.tls_enabled and
					StateData#state.tls_required ->
				    send_element(
				      StateData,
				      xmpp:serr_policy_violation(
					<<"Use of STARTTLS required">>, Lang)),
				    {stop, normal, StateData};
				true ->
				    fsm_next_state(wait_for_auth,
					StateData#state{server = Server,
					    lang = Lang})
			    end
		    end;
		true ->
		    IP = StateData#state.ip,
		    {true, LogReason, ReasonT} = IsBlacklistedIP,
		    ?INFO_MSG("Connection attempt from blacklisted IP ~s: ~s",
			[jlib:ip_to_list(IP), LogReason]),
		    send_header(StateData, Server, StreamVersion, ?MYLANG),
		    send_element(StateData, xmpp:serr_policy_violation(ReasonT, Lang)),
		    {stop, normal, StateData};
		_ ->
		    send_header(StateData, ?MYNAME, StreamVersion, ?MYLANG),
		    send_element(StateData, xmpp:serr_host_unknown()),
		    {stop, normal, StateData}
	    end
    catch _:{xmpp_codec, Why} ->
	    Txt = xmpp:format_error(Why),
	    send_header(StateData, ?MYNAME, <<"">>, ?MYLANG),
	    send_element(StateData, xmpp:serr_not_well_formed(Txt, ?MYLANG)),
	    {stop, normal, StateData}
    end;
wait_for_stream(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_stream({xmlstreamelement, _}, StateData) ->
    send_element(StateData, xmpp:serr_not_well_formed()),
    {stop, normal, StateData};
wait_for_stream({xmlstreamend, _}, StateData) ->
    send_element(StateData, xmpp:serr_not_well_formed()),
    {stop, normal, StateData};
wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_header(StateData, ?MYNAME, <<"1.0">>, <<"">>),
    send_element(StateData, xmpp:serr_not_well_formed()),
    {stop, normal, StateData};
wait_for_stream(closed, StateData) ->
    {stop, normal, StateData};
wait_for_stream(stop, StateData) ->
    {stop, normal, StateData}.

wait_for_auth({xmlstreamelement, #xmlel{} = El}, StateData) ->
    decode_element(El, wait_for_auth, StateData);
wait_for_auth(Pkt, StateData) when ?IS_STREAM_MGMT_PACKET(Pkt) ->
    fsm_next_state(wait_for_auth, dispatch_stream_mgmt(Pkt, StateData));
wait_for_auth(#iq{type = get,
		  sub_els = [#legacy_auth{username = U}]} = IQ, StateData) ->
    Username = case U of
		   undefined -> none;
		   _ -> U
	       end,
    Auth = #legacy_auth{username = Username, password = none, resource = none},
    Res = case ejabberd_auth:plain_password_required(StateData#state.server) of
	      false ->
		  xmpp:make_iq_result(IQ, Auth#legacy_auth{digest = none});
	      true ->
		  xmpp:make_iq_result(IQ, Auth)
	  end,
    send_element(StateData, Res),
    fsm_next_state(wait_for_auth, StateData);
wait_for_auth(#iq{type = set, sub_els = [#legacy_auth{resource = <<"">>}]} = IQ,
	      StateData) ->
    Lang = StateData#state.lang,
    Txt = <<"No resource provided">>,
    Err = xmpp:make_error(IQ, xmpp:err_not_acceptable(Txt, Lang)),
    send_element(StateData, Err),
    fsm_next_state(wait_for_auth, StateData);
wait_for_auth(#iq{type = set, sub_els = [#legacy_auth{username = U,
						      password = P0,
						      digest = D0,
						      resource = R}]} = IQ,
	      StateData) when is_binary(U), is_binary(R) ->
    JID = jid:make(U, StateData#state.server, R),
    case (JID /= error) andalso
	acl:access_matches(StateData#state.access,
			   #{usr => jid:split(JID), ip => StateData#state.ip},
			   StateData#state.server) == allow of
	true ->
	    DGen = fun (PW) ->
			   p1_sha:sha(<<(StateData#state.streamid)/binary, PW/binary>>)
		   end,
	    P = if is_binary(P0) -> P0; true -> <<>> end,
	    D = if is_binary(D0) -> D0; true -> <<>> end,
	    case ejabberd_auth:check_password_with_authmodule(
		   U, U, StateData#state.server, P, D, DGen) of
		{true, AuthModule} ->
		    ?INFO_MSG("(~w) Accepted legacy authentication for ~s by ~p from ~s",
			      [StateData#state.socket,
			       jid:to_string(JID), AuthModule,
			       ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
		    ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
				       [true, U, StateData#state.server,
					StateData#state.ip]),
		    Conn = get_conn_type(StateData),
		    Info = [{ip, StateData#state.ip}, {conn, Conn},
			    {auth_module, AuthModule}],
		    Res = xmpp:make_iq_result(IQ),
		    send_element(StateData, Res),
		    ejabberd_sm:open_session(StateData#state.sid, U,
					     StateData#state.server, R,
					     Info),
		    change_shaper(StateData, JID),
		    {Fs, Ts} = ejabberd_hooks:run_fold(
				 roster_get_subscription_lists,
				 StateData#state.server,
				 {[], []},
				 [U, StateData#state.server]),
		    LJID = jid:tolower(jid:remove_resource(JID)),
		    Fs1 = [LJID | Fs],
		    Ts1 = [LJID | Ts],
		    PrivList = ejabberd_hooks:run_fold(privacy_get_user_list,
						       StateData#state.server,
						       #userlist{},
						       [U, StateData#state.server]),
		    NewStateData = StateData#state{
				     user = U,
				     resource = R,
				     jid = JID,
				     conn = Conn,
				     auth_module = AuthModule,
				     pres_f = (?SETS):from_list(Fs1),
				     pres_t = (?SETS):from_list(Ts1),
				     privacy_list = PrivList},
		    fsm_next_state(session_established, NewStateData);
		_ ->
		    ?INFO_MSG("(~w) Failed legacy authentication for ~s from ~s",
			      [StateData#state.socket,
			       jid:to_string(JID),
			       ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
		    ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
				       [false, U, StateData#state.server,
					StateData#state.ip]),
		    Lang = StateData#state.lang,
		    Txt = <<"Legacy authentication failed">>,
		    Err = xmpp:make_error(IQ, xmpp:err_not_authorized(Txt, Lang)),
		    send_element(StateData, Err),
		    fsm_next_state(wait_for_auth, StateData)
	    end;
	false when JID == error ->
	    ?INFO_MSG("(~w) Forbidden legacy authentication "
		      "for username '~s' with resource '~s'",
		      [StateData#state.socket, U, R]),
	    Err = xmpp:make_error(IQ, xmpp:err_jid_malformed()),
	    send_element(StateData, Err),
	    fsm_next_state(wait_for_auth, StateData);
	false ->
	    ?INFO_MSG("(~w) Forbidden legacy authentication for ~s from ~s",
		      [StateData#state.socket,
		       jid:to_string(JID),
		       ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
	    ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
			       [false, U, StateData#state.server,
				StateData#state.ip]),
	    Lang = StateData#state.lang,
	    Txt = <<"Legacy authentication forbidden">>,
	    Err = xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang)),
	    send_element(StateData, Err),
	    fsm_next_state(wait_for_auth, StateData)
    end;
wait_for_auth(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_auth({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};
wait_for_auth({xmlstreamerror, _}, StateData) ->
    send_element(StateData, xmpp:serr_not_well_formed()),
    {stop, normal, StateData};
wait_for_auth(closed, StateData) ->
    {stop, normal, StateData};
wait_for_auth(stop, StateData) ->
    {stop, normal, StateData};
wait_for_auth(Pkt, StateData) ->
    process_unauthenticated_stanza(StateData, Pkt),
    fsm_next_state(wait_for_auth, StateData).

wait_for_feature_request({xmlstreamelement, El}, StateData) ->
    decode_element(El, wait_for_feature_request, StateData);
wait_for_feature_request(Pkt, StateData) when ?IS_STREAM_MGMT_PACKET(Pkt) ->
    fsm_next_state(wait_for_feature_request,
		   dispatch_stream_mgmt(Pkt, StateData));
wait_for_feature_request(#sasl_auth{mechanism = Mech,
				    text = ClientIn},
			 #state{tls_enabled = TLSEnabled,
				tls_required = TLSRequired} = StateData)
  when TLSEnabled or not TLSRequired ->
    case cyrsasl:server_start(StateData#state.sasl_state, Mech, ClientIn) of
	{ok, Props} ->
	    (StateData#state.sockmod):reset_stream(StateData#state.socket),
	    U = identity(Props),
	    AuthModule = proplists:get_value(auth_module, Props, undefined),
	    ?INFO_MSG("(~w) Accepted authentication for ~s by ~p from ~s",
		      [StateData#state.socket, U, AuthModule,
		       ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
	    ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
			       [true, U, StateData#state.server,
				StateData#state.ip]),
	    send_element(StateData, #sasl_success{}),
	    fsm_next_state(wait_for_stream,
			   StateData#state{streamid = new_id(),
					   authenticated = true,
					   auth_module = AuthModule,
					   sasl_state = undefined,
					   user = U});
	{continue, ServerOut, NewSASLState} ->
	    send_element(StateData, #sasl_challenge{text = ServerOut}),
	    fsm_next_state(wait_for_sasl_response,
			   StateData#state{sasl_state = NewSASLState});
	{error, Error, Username} ->
	    ?INFO_MSG("(~w) Failed authentication for ~s@~s from ~s",
		      [StateData#state.socket,
		       Username, StateData#state.server,
		       ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
	    ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
			       [false, Username, StateData#state.server,
				StateData#state.ip]),
	    send_element(StateData, #sasl_failure{reason = Error}),
	    fsm_next_state(wait_for_feature_request, StateData);
	{error, Error} ->
	    send_element(StateData, #sasl_failure{reason = Error}),
	    fsm_next_state(wait_for_feature_request, StateData)
    end;
wait_for_feature_request(#starttls{},
			 #state{tls = true, tls_enabled = false} = StateData) ->
    case (StateData#state.sockmod):get_sockmod(StateData#state.socket) of
	gen_tcp ->
	    TLSOpts = case ejabberd_config:get_option(
			     {domain_certfile, StateData#state.server},
			     fun iolist_to_binary/1) of
			  undefined ->
			      StateData#state.tls_options;
			  CertFile ->
			      lists:keystore(certfile, 1,
					     StateData#state.tls_options,
					     {certfile, CertFile})
		      end,
	    Socket = StateData#state.socket,
	    BProceed = fxml:element_to_binary(xmpp:encode(#starttls_proceed{})),
	    TLSSocket = (StateData#state.sockmod):starttls(Socket, TLSOpts, BProceed),
	    fsm_next_state(wait_for_stream,
			   StateData#state{socket = TLSSocket,
					   streamid = new_id(),
					   tls_enabled = true});
	_ ->
	    Lang = StateData#state.lang,
	    Txt = <<"Unsupported TLS transport">>,
	    send_element(StateData, xmpp:serr_policy_violation(Txt, Lang)),
	    {stop, normal, StateData}
    end;
wait_for_feature_request(#compress{} = Comp, StateData) ->
    Zlib = StateData#state.zlib,
    SockMod = (StateData#state.sockmod):get_sockmod(StateData#state.socket),
    if Zlib == true, (SockMod == gen_tcp) or (SockMod == fast_tls) ->
	    process_compression_request(Comp, wait_for_feature_request, StateData);
       true ->
	    send_element(StateData, #compress_failure{reason = 'setup-failed'}),
	    fsm_next_state(wait_for_feature_request, StateData)
    end;
wait_for_feature_request(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_feature_request({xmlstreamend, _Name},
			 StateData) ->
    {stop, normal, StateData};
wait_for_feature_request({xmlstreamerror, _},
			 StateData) ->
    send_element(StateData, xmpp:serr_not_well_formed()),
    {stop, normal, StateData};
wait_for_feature_request(closed, StateData) ->
    {stop, normal, StateData};
wait_for_feature_request(stop, StateData) ->
    {stop, normal, StateData};
wait_for_feature_request(_Pkt,
			 #state{tls_required = TLSRequired,
				tls_enabled = TLSEnabled} = StateData)
  when TLSRequired and not TLSEnabled ->
    Lang = StateData#state.lang,
    Txt = <<"Use of STARTTLS required">>,
    send_element(StateData, xmpp:serr_policy_violation(Txt, Lang)),
    {stop, normal, StateData};
wait_for_feature_request(Pkt, StateData) ->
    process_unauthenticated_stanza(StateData, Pkt),
    fsm_next_state(wait_for_feature_request, StateData).

wait_for_sasl_response({xmlstreamelement, El}, StateData) ->
    decode_element(El, wait_for_sasl_response, StateData);
wait_for_sasl_response(Pkt, StateData) when ?IS_STREAM_MGMT_PACKET(Pkt) ->
    fsm_next_state(wait_for_sasl_response,
		   dispatch_stream_mgmt(Pkt, StateData));
wait_for_sasl_response(#sasl_response{text = ClientIn}, StateData) ->
    case cyrsasl:server_step(StateData#state.sasl_state, ClientIn) of
	{ok, Props} ->
	    catch (StateData#state.sockmod):reset_stream(StateData#state.socket),
	    U = identity(Props),
	    AuthModule = proplists:get_value(auth_module, Props, <<>>),
	    ?INFO_MSG("(~w) Accepted authentication for ~s by ~p from ~s",
		      [StateData#state.socket, U, AuthModule,
		       ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
	    ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
			       [true, U, StateData#state.server,
				StateData#state.ip]),
	    send_element(StateData, #sasl_success{}),
	    fsm_next_state(wait_for_stream,
			   StateData#state{streamid = new_id(),
					   authenticated = true,
					   auth_module = AuthModule,
					   sasl_state = undefined,
					   user = U});
	{ok, Props, ServerOut} ->
	    (StateData#state.sockmod):reset_stream(StateData#state.socket),
	    U = identity(Props),
	    AuthModule = proplists:get_value(auth_module, Props, undefined),
	    ?INFO_MSG("(~w) Accepted authentication for ~s by ~p from ~s",
		      [StateData#state.socket, U, AuthModule,
		       ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
	    ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
			       [true, U, StateData#state.server,
				StateData#state.ip]),
	    send_element(StateData, #sasl_success{text = ServerOut}),
	    fsm_next_state(wait_for_stream,
			   StateData#state{streamid = new_id(),
					   authenticated = true,
					   auth_module = AuthModule,
					   sasl_state = undefined,
					   user = U});
	{continue, ServerOut, NewSASLState} ->
	    send_element(StateData, #sasl_challenge{text = ServerOut}),
	    fsm_next_state(wait_for_sasl_response,
			   StateData#state{sasl_state = NewSASLState});
	{error, Error, Username} ->
	    ?INFO_MSG("(~w) Failed authentication for ~s@~s from ~s",
		      [StateData#state.socket,
		       Username, StateData#state.server,
		       ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
	    ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
			       [false, Username, StateData#state.server,
				StateData#state.ip]),
	    send_element(StateData, #sasl_failure{reason = Error}),
	    fsm_next_state(wait_for_feature_request, StateData);
	{error, Error} ->
	    send_element(StateData, #sasl_failure{reason = Error}),
	    fsm_next_state(wait_for_feature_request, StateData)
    end;
wait_for_sasl_response(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_sasl_response({xmlstreamend, _Name},
		       StateData) ->
    {stop, normal, StateData};
wait_for_sasl_response({xmlstreamerror, _},
		       StateData) ->
    send_element(StateData, xmpp:serr_not_well_formed()),
    {stop, normal, StateData};
wait_for_sasl_response(closed, StateData) ->
    {stop, normal, StateData};
wait_for_sasl_response(stop, StateData) ->
    {stop, normal, StateData};
wait_for_sasl_response(Pkt, StateData) ->
    process_unauthenticated_stanza(StateData, Pkt),
    fsm_next_state(wait_for_feature_request, StateData).

-spec resource_conflict_action(binary(), binary(), binary()) ->
				      {accept_resource, binary()} | closenew.
resource_conflict_action(U, S, R) ->
    OptionRaw = case ejabberd_sm:is_existing_resource(U, S, R) of
		  true ->
		      ejabberd_config:get_option(
                        {resource_conflict, S},
                        fun(setresource) -> setresource;
                           (closeold) -> closeold;
                           (closenew) -> closenew;
                           (acceptnew) -> acceptnew
                        end);
                  false ->
                      acceptnew
		end,
    Option = case OptionRaw of
	       setresource -> setresource;
	       closeold ->
		   acceptnew; %% ejabberd_sm will close old session
	       closenew -> closenew;
	       acceptnew -> acceptnew;
	       _ -> acceptnew %% default ejabberd behavior
	     end,
    case Option of
      acceptnew -> {accept_resource, R};
      closenew -> closenew;
      setresource ->
	  Rnew = new_uniq_id(),
	  {accept_resource, Rnew}
    end.

-spec decode_element(xmlel(), state_name(), state()) -> fsm_transition().
decode_element(#xmlel{} = El, StateName, StateData) ->
    try case xmpp:decode(El, [ignore_els]) of
	    #iq{sub_els = [_], type = T} = Pkt when T == set; T == get ->
		NewPkt = xmpp:decode_els(
			   Pkt,
			   fun(SubEl) when StateName == session_established ->
				   case xmpp:get_ns(SubEl) of
				       ?NS_PRIVACY -> true;
				       ?NS_BLOCKING -> true;
				       _ -> false
				   end;
			      (SubEl) ->
				   xmpp_codec:is_known_tag(SubEl)
			   end),
		?MODULE:StateName(NewPkt, StateData);
	    Pkt ->
		?MODULE:StateName(Pkt, StateData)
	end
    catch error:{xmpp_codec, Why} ->
	    NS = xmpp:get_ns(El),
	    case xmpp:is_stanza(El) of
		true ->
		    Lang = xmpp:get_lang(El),
		    Txt = xmpp:format_error(Why),
		    send_error(StateData, El, xmpp:err_bad_request(Txt, Lang));
		false when NS == ?NS_STREAM_MGMT_2; NS == ?NS_STREAM_MGMT_3 ->
		    Err = #sm_failed{reason = 'bad-request', xmlns = NS},
		    send_element(StateData, Err);
		false ->
		    ok
	    end,
	    fsm_next_state(StateName, StateData)
    end.

wait_for_bind({xmlstreamelement, El}, StateData) ->
    decode_element(El, wait_for_bind, StateData);
wait_for_bind(#sm_resume{} = Pkt, StateData) ->
    case handle_resume(StateData, Pkt) of
	{ok, ResumedState} ->
	    fsm_next_state(session_established, ResumedState);
	error ->
	    fsm_next_state(wait_for_bind, StateData)
    end;
wait_for_bind(Pkt, StateData) when ?IS_STREAM_MGMT_PACKET(Pkt) ->
    fsm_next_state(wait_for_bind, dispatch_stream_mgmt(Pkt, StateData));
wait_for_bind(#iq{type = set,
		  sub_els = [#bind{resource = R}]} = IQ, StateData) ->
    U = StateData#state.user,
    case resource_conflict_action(U, StateData#state.server, R) of
	closenew ->
	    Err = xmpp:make_error(IQ, xmpp:err_conflict()),
	    send_element(StateData, Err),
	    fsm_next_state(wait_for_bind, StateData);
	{accept_resource, R2} ->
	    JID = jid:make(U, StateData#state.server, R2),
	    StateData2 = StateData#state{resource = R2, jid = JID},
	    case open_session(StateData2) of
		{ok, StateData3} ->
		    Res = xmpp:make_iq_result(IQ, #bind{jid = JID}),
		    try
			send_element(StateData3, Res)
		    catch
			exit:normal -> close(self())
		    end,
		    fsm_next_state_pack(session_established,StateData3);
		{error, Error} ->
		    Err = xmpp:make_error(IQ, Error),
		    send_element(StateData, Err),
		    fsm_next_state(wait_for_bind, StateData)
	    end
    end;
wait_for_bind(#compress{} = Comp, StateData) ->
    Zlib = StateData#state.zlib,
    SockMod = (StateData#state.sockmod):get_sockmod(StateData#state.socket),
    if Zlib == true, (SockMod == gen_tcp) or (SockMod == fast_tls) ->
	    process_compression_request(Comp, wait_for_bind, StateData);
       true ->
	    send_element(StateData, #compress_failure{reason = 'setup-failed'}),
	    fsm_next_state(wait_for_bind, StateData)
    end;
wait_for_bind(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_bind({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};
wait_for_bind({xmlstreamerror, _}, StateData) ->
    send_element(StateData, xmpp:serr_not_well_formed()),
    {stop, normal, StateData};
wait_for_bind(closed, StateData) ->
    {stop, normal, StateData};
wait_for_bind(stop, StateData) ->
    {stop, normal, StateData};
wait_for_bind(Pkt, StateData) ->
    case xmpp:is_stanza(Pkt) of
	true ->
	    send_error(StateData, Pkt, xmpp:err_not_acceptable());
	false ->
	    ok
    end,
    fsm_next_state(wait_for_bind, StateData).

-spec open_session(state()) -> {ok, state()} | {error, error()}.
open_session(StateData) ->
    U = StateData#state.user,
    R = StateData#state.resource,
    JID = StateData#state.jid,
    Lang = StateData#state.lang,
    IP = StateData#state.ip,
    case acl:access_matches(StateData#state.access,
			    #{usr => jid:split(JID), ip => IP},
			    StateData#state.server) of
        allow ->
            ?INFO_MSG("(~w) Opened session for ~s",
                      [StateData#state.socket, jid:to_string(JID)]),
            change_shaper(StateData, JID),
            {Fs, Ts} = ejabberd_hooks:run_fold(
                         roster_get_subscription_lists,
                         StateData#state.server,
                         {[], []},
                         [U, StateData#state.server]),
            LJID = jid:tolower(jid:remove_resource(JID)),
            Fs1 = [LJID | Fs],
            Ts1 = [LJID | Ts],
            PrivList =
                ejabberd_hooks:run_fold(
                  privacy_get_user_list,
                  StateData#state.server,
                  #userlist{},
                  [U, StateData#state.server]),
            Conn = get_conn_type(StateData),
            Info = [{ip, StateData#state.ip}, {conn, Conn},
                    {auth_module, StateData#state.auth_module}],
            ejabberd_sm:open_session(
              StateData#state.sid, U, StateData#state.server, R, Info),
            UpdatedStateData =
                StateData#state{
                  conn = Conn,
                  pres_f = ?SETS:from_list(Fs1),
                  pres_t = ?SETS:from_list(Ts1),
                  privacy_list = PrivList},
            {ok, UpdatedStateData};
        _ ->
            ejabberd_hooks:run(forbidden_session_hook,
                               StateData#state.server, [JID]),
            ?INFO_MSG("(~w) Forbidden session for ~s",
                      [StateData#state.socket, jid:to_string(JID)]),
	    Txt = <<"Denied by ACL">>,
	    {error, xmpp:err_not_allowed(Txt, Lang)}
    end.

session_established({xmlstreamelement, El}, StateData) ->
    decode_element(El, session_established, StateData);
session_established(Pkt, StateData) when ?IS_STREAM_MGMT_PACKET(Pkt) ->
    fsm_next_state(session_established, dispatch_stream_mgmt(Pkt, StateData));
session_established(#csi{type = active}, StateData) ->
    NewStateData = csi_flush_queue(StateData),
    fsm_next_state(session_established, NewStateData#state{csi_state = active});
session_established(#csi{type = inactive}, StateData) ->
    fsm_next_state(session_established, StateData#state{csi_state = inactive});
%% We hibernate the process to reduce memory consumption after a
%% configurable activity timeout
session_established(timeout, StateData) ->
    Options = [],
    proc_lib:hibernate(?GEN_FSM, enter_loop,
		       [?MODULE, Options, session_established, StateData]),
    fsm_next_state(session_established, StateData);
session_established({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};
session_established({xmlstreamerror,
		     <<"XML stanza is too big">> = E},
		    StateData) ->
    send_element(StateData,
		 xmpp:serr_policy_violation(E, StateData#state.lang)),
    {stop, normal, StateData};
session_established({xmlstreamerror, _}, StateData) ->
    send_element(StateData, xmpp:serr_not_well_formed()),
    {stop, normal, StateData};
session_established(closed, #state{mgmt_state = active} = StateData) ->
    catch (StateData#state.sockmod):close(StateData#state.socket),
    fsm_next_state(wait_for_resume, StateData);
session_established(closed, StateData) ->
    {stop, normal, StateData};
session_established(stop, StateData) ->
    {stop, normal, StateData};
session_established(Pkt, StateData) when ?is_stanza(Pkt) ->
    FromJID = StateData#state.jid,
    case check_from(Pkt, FromJID) of
	'invalid-from' ->
	    send_element(StateData, xmpp:serr_invalid_from()),
	    {stop, normal, StateData};
	_ ->
	    NewStateData = update_num_stanzas_in(StateData, Pkt),
	    session_established2(Pkt, NewStateData)
    end;
session_established(_Pkt, StateData) ->
    fsm_next_state(session_established, StateData).

-spec session_established2(xmpp_element(), state()) -> fsm_next().
%% Process packets sent by user (coming from user on c2s XMPP connection)
session_established2(Pkt, StateData) ->
    User = StateData#state.user,
    Server = StateData#state.server,
    FromJID = StateData#state.jid,
    ToJID = case xmpp:get_to(Pkt) of
		undefined -> jid:make(User, Server, <<"">>);
		J -> J
	    end,
    Lang = case xmpp:get_lang(Pkt) of
	       undefined -> StateData#state.lang;
	       <<"">> -> StateData#state.lang;
	       L -> L
	   end,
    NewPkt = xmpp:set_lang(Pkt, Lang),
    NewState =
	case NewPkt of
	    #presence{} ->
		Presence0 = ejabberd_hooks:run_fold(
			      c2s_update_presence, Server, NewPkt,
			      [User, Server]),
		Presence = ejabberd_hooks:run_fold(
			     user_send_packet, Server, Presence0,
			     [StateData, FromJID, ToJID]),
		case ToJID of
		    #jid{user = User, server = Server, resource = <<"">>} ->
			?DEBUG("presence_update(~p,~n\t~p,~n\t~p)",
			       [FromJID, Presence, StateData]),
			presence_update(FromJID, Presence,
					StateData);
		    _ ->
			presence_track(FromJID, ToJID, Presence,
				       StateData)
		end;
	    #iq{type = T, sub_els = [El]} when T == set; T == get ->
		NS = xmpp:get_ns(El),
		if NS == ?NS_BLOCKING; NS == ?NS_PRIVACY ->
			IQ = xmpp:set_from_to(Pkt, FromJID, ToJID),
			process_privacy_iq(IQ, StateData);
		   NS == ?NS_SESSION ->
			Res = xmpp:make_iq_result(Pkt),
			send_stanza(StateData, Res);
		   true ->
			NewPkt0 = ejabberd_hooks:run_fold(
				    user_send_packet, Server, NewPkt,
				    [StateData, FromJID, ToJID]),
			check_privacy_route(FromJID, StateData, FromJID,
					    ToJID, NewPkt0)
		end;
	    _ ->
		NewPkt0 = ejabberd_hooks:run_fold(
			    user_send_packet, Server, NewPkt,
			    [StateData, FromJID, ToJID]),
		check_privacy_route(FromJID, StateData, FromJID,
				    ToJID, NewPkt0)
	end,
    ejabberd_hooks:run(c2s_loop_debug,
		       [{xmlstreamelement, Pkt}]),
    fsm_next_state(session_established, NewState).

wait_for_resume({xmlstreamelement, _El} = Event, StateData) ->
    Result = session_established(Event, StateData),
    fsm_next_state(wait_for_resume, element(3, Result));
wait_for_resume(timeout, StateData) ->
    ?DEBUG("Timed out waiting for resumption of stream for ~s",
	   [jid:to_string(StateData#state.jid)]),
    {stop, normal, StateData#state{mgmt_state = timeout}};
wait_for_resume(Event, StateData) ->
    ?DEBUG("Ignoring event while waiting for resumption: ~p", [Event]),
    fsm_next_state(wait_for_resume, StateData).

handle_event(_Event, StateName, StateData) ->
    fsm_next_state(StateName, StateData).

handle_sync_event({get_presence}, _From, StateName,
		  StateData) ->
    User = StateData#state.user,
    PresLast = StateData#state.pres_last,
    Show = get_showtag(PresLast),
    Status = get_statustag(PresLast),
    Resource = StateData#state.resource,
    Reply = {User, Resource, Show, Status},
    fsm_reply(Reply, StateName, StateData);
handle_sync_event(get_subscribed, _From, StateName,
		  StateData) ->
    Subscribed = (?SETS):to_list(StateData#state.pres_f),
    {reply, Subscribed, StateName, StateData};
handle_sync_event({resume_session, Time}, _From, _StateName,
		  StateData) when element(1, StateData#state.sid) == Time ->
    %% The old session should be closed before the new one is opened, so we do
    %% this here instead of leaving it to the terminate callback
    ejabberd_sm:close_session(StateData#state.sid,
			      StateData#state.user,
			      StateData#state.server,
			      StateData#state.resource),
    {stop, normal, {ok, StateData}, StateData#state{mgmt_state = resumed}};
handle_sync_event({resume_session, _Time}, _From, StateName,
		  StateData) ->
    {reply, {error, <<"Previous session not found">>}, StateName, StateData};
handle_sync_event(_Event, _From, StateName,
		  StateData) ->
    Reply = ok, fsm_reply(Reply, StateName, StateData).

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData, Text),
    ejabberd_hooks:run(c2s_loop_debug, [Text]),
    fsm_next_state(StateName, StateData);
handle_info(replaced, StateName, StateData) ->
    Lang = StateData#state.lang,
    Pkt = xmpp:serr_conflict(<<"Replaced by new connection">>, Lang),
    handle_info({kick, replaced, Pkt}, StateName, StateData);
handle_info(kick, StateName, StateData) ->
    Lang = StateData#state.lang,
    Pkt = xmpp:serr_policy_violation(<<"has been kicked">>, Lang),
    handle_info({kick, kicked_by_admin, Pkt}, StateName, StateData);
handle_info({kick, Reason, Pkt}, _StateName, StateData) ->
    send_element(StateData, Pkt),
    {stop, normal,
     StateData#state{authenticated = Reason}};
handle_info({route, _From, _To, {broadcast, Data}},
            StateName, StateData) ->
    ?DEBUG("broadcast~n~p~n", [Data]),
    case Data of
        {item, IJID, ISubscription} ->
            fsm_next_state(StateName,
                           roster_change(IJID, ISubscription, StateData));
        {exit, Reason} ->
            Lang = StateData#state.lang,
            send_element(StateData, xmpp:serr_conflict(Reason, Lang)),
            {stop, normal, StateData};
        {privacy_list, PrivList, PrivListName} ->
            case ejabberd_hooks:run_fold(privacy_updated_list,
                                         StateData#state.server,
                                         false,
                                         [StateData#state.privacy_list,
                                          PrivList]) of
                false ->
                    fsm_next_state(StateName, StateData);
                NewPL ->
		    PrivPushIQ =
			#iq{type = set,
			    from = jid:remove_resource(StateData#state.jid),
			    to = StateData#state.jid,
			    id = <<"push", (randoms:get_string())/binary>>,
			    sub_els = [#privacy_query{
					  lists = [#privacy_list{
						      name = PrivListName}]}]},
                    NewState = send_stanza(StateData, PrivPushIQ),
                    fsm_next_state(StateName,
                                   NewState#state{privacy_list = NewPL})
            end;
        {blocking, What} ->
            NewState = route_blocking(What, StateData),
            fsm_next_state(StateName, NewState);
        _ ->
            fsm_next_state(StateName, StateData)
    end;
%% Process Packets that are to be send to the user
handle_info({route, From, To, Packet}, StateName, StateData) when ?is_stanza(Packet) ->
    {Pass, NewState} =
	case Packet of
	    #presence{type = T} ->
		State = ejabberd_hooks:run_fold(c2s_presence_in,
						StateData#state.server,
						StateData,
						[{From, To, Packet}]),
		case T of
		    probe ->
			LFrom = jid:tolower(From),
			LBFrom = jid:remove_resource(LFrom),
			NewStateData =
			    case (?SETS):is_element(LFrom, State#state.pres_a)
				orelse (?SETS):is_element(LBFrom, State#state.pres_a) of
				true -> State;
				false ->
				    case (?SETS):is_element(LFrom, State#state.pres_f) of
					true ->
					    A = (?SETS):add_element(LFrom, State#state.pres_a),
					    State#state{pres_a = A};
					false ->
					    case (?SETS):is_element(LBFrom, State#state.pres_f) of
						true ->
						    A = (?SETS):add_element(LBFrom, State#state.pres_a),
						    State#state{pres_a = A};
						false ->
						    State
					    end
				    end
			    end,
			process_presence_probe(From, To, NewStateData),
			{false, NewStateData};
		    error ->
			NewA = remove_element(jid:tolower(From), State#state.pres_a),
			{true, State#state{pres_a = NewA}};
		    subscribe ->
			SRes = is_privacy_allow(State, From, To, Packet, in),
			{SRes, State};
		    subscribed ->
			SRes = is_privacy_allow(State, From, To, Packet, in),
			{SRes, State};
		    unsubscribe ->
			SRes = is_privacy_allow(State, From, To, Packet, in),
			{SRes, State};
		    unsubscribed ->
			SRes = is_privacy_allow(State, From, To, Packet, in),
			{SRes, State};
		    _ ->
			case privacy_check_packet(State, From, To, Packet, in) of
			    allow ->
				LFrom = jid:tolower(From),
				LBFrom = jid:remove_resource(LFrom),
				case (?SETS):is_element(LFrom, State#state.pres_a)
				    orelse (?SETS):is_element(LBFrom, State#state.pres_a) of
				    true ->
					{true, State};
				    false ->
					case (?SETS):is_element(LFrom, State#state.pres_f) of
					    true ->
						A = (?SETS):add_element(LFrom, State#state.pres_a),
						{true, State#state{pres_a = A}};
					    false ->
						case (?SETS):is_element(LBFrom,
									State#state.pres_f) of
						    true ->
							A = (?SETS):add_element(
							      LBFrom,
							      State#state.pres_a),
							{true, State#state{pres_a = A}};
						    false ->
							{true, State}
						end
					end
				end;
			    deny -> {false, State}
			end
		end;
	    #iq{type = T} ->
		case xmpp:has_subtag(Packet, #last{}) of
		    true when T == get; T == set ->
			LFrom = jid:tolower(From),
			LBFrom = jid:remove_resource(LFrom),
			HasFromSub = ((?SETS):is_element(LFrom, StateData#state.pres_f)
				      orelse (?SETS):is_element(LBFrom, StateData#state.pres_f))
			    andalso is_privacy_allow(StateData, To, From, #presence{}, out),
			case HasFromSub of
			    true ->
				case privacy_check_packet(
				       StateData, From, To, Packet, in) of
				    allow ->
					{true, StateData};
				    deny ->
					Err = xmpp:make_error(
						Packet,
						xmpp:err_service_unavailable()),
					ejabberd_router:route(To, From, Err),
					{false, StateData}
				end;
			    _ ->
				Err = xmpp:make_error(Packet, xmpp:err_forbidden()),
				ejabberd_router:route(To, From, Err),
				{false, StateData}
			end;
		    _ ->
			case privacy_check_packet(StateData, From, To, Packet, in) of
			    allow ->
				{true, StateData};
			    deny when T == get; T == set ->
				Err = xmpp:make_error(
					Packet, xmpp:err_service_unavailable()),
				ejabberd_router:route(To, From, Err),
				{false, StateData};
			    deny ->
				{false, StateData}
			end
		end;
	    #message{type = T} ->
		case privacy_check_packet(StateData, From, To, Packet, in) of
		    allow ->
			{true, StateData};
		    deny ->
			case T of
			    error -> ok;
			    groupchat -> ok;
			    headline -> ok;
			    _ ->
				Err = xmpp:make_error(
					Packet, xmpp:err_service_unavailable()),
				ejabberd_router:route(To, From, Err)
			end,
			{false, StateData}
		end
	end,
    if Pass ->
	    FixedPacket0 = xmpp:set_from_to(Packet, From, To),
	    FixedPacket = ejabberd_hooks:run_fold(
			    user_receive_packet,
			    NewState#state.server,
			    FixedPacket0,
			    [NewState, NewState#state.jid, From, To]),
	    SentStateData = send_packet(NewState, FixedPacket),
	    ejabberd_hooks:run(c2s_loop_debug, [{route, From, To, Packet}]),
	    fsm_next_state(StateName, SentStateData);
	true ->
	    ejabberd_hooks:run(c2s_loop_debug, [{route, From, To, Packet}]),
	    fsm_next_state(StateName, NewState)
    end;
handle_info({'DOWN', Monitor, _Type, _Object, _Info},
	    _StateName, StateData)
    when Monitor == StateData#state.socket_monitor ->
    if StateData#state.mgmt_state == active;
       StateData#state.mgmt_state == pending ->
	   fsm_next_state(wait_for_resume, StateData);
       true ->
	   {stop, normal, StateData}
    end;
handle_info(system_shutdown, StateName, StateData) ->
    case StateName of
      wait_for_stream ->
	  send_header(StateData, ?MYNAME, <<"1.0">>, <<"en">>),
	  send_element(StateData, xmpp:serr_system_shutdown()),
	  ok;
      _ ->
	  send_element(StateData, xmpp:serr_system_shutdown()),
	  ok
    end,
    {stop, normal, StateData};
handle_info({route_xmlstreamelement, El}, _StateName, StateData) ->
    {next_state, NStateName, NStateData, _Timeout} =
	session_established({xmlstreamelement, El}, StateData),
    fsm_next_state(NStateName, NStateData);
handle_info({force_update_presence, LUser, LServer}, StateName,
	    #state{jid = #jid{luser = LUser, lserver = LServer}} = StateData) ->
    NewStateData = case StateData#state.pres_last of
		       #presence{} ->
			   Presence =
			       ejabberd_hooks:run_fold(c2s_update_presence,
						       LServer,
						       StateData#state.pres_last,
						       [LUser, LServer]),
			   StateData2 = StateData#state{pres_last = Presence},
			   presence_update(StateData2#state.jid, Presence,
					   StateData2),
			   StateData2;
		       undefined -> StateData
		   end,
    fsm_next_state(StateName, NewStateData);
handle_info({send_filtered, Feature, From, To, Packet}, StateName, StateData) ->
    Drop = ejabberd_hooks:run_fold(c2s_filter_packet, StateData#state.server,
				   true, [StateData#state.server, StateData,
					  Feature, To, Packet]),
    NewStateData = if Drop ->
			  ?DEBUG("Dropping packet from ~p to ~p",
				 [jid:to_string(From),
				  jid:to_string(To)]),
			  StateData;
		      true ->
			  FinalPacket = xmpp:set_from_to(Packet, From, To),
			  case StateData#state.jid of
			    To ->
				case privacy_check_packet(StateData, From, To,
							  FinalPacket, in) of
				  deny ->
				      StateData;
				  allow ->
				      send_stanza(StateData, FinalPacket)
				end;
			    _ ->
				ejabberd_router:route(From, To, FinalPacket),
				StateData
			  end
		   end,
    fsm_next_state(StateName, NewStateData);
handle_info({broadcast, Type, From, Packet}, StateName, StateData) ->
    Recipients = ejabberd_hooks:run_fold(
		   c2s_broadcast_recipients, StateData#state.server,
		   [],
		   [StateData#state.server, StateData, Type, From, Packet]),
    lists:foreach(
      fun(USR) ->
	      ejabberd_router:route(
		From, jid:make(USR), Packet)
      end, lists:usort(Recipients)),
    fsm_next_state(StateName, StateData);
handle_info(dont_ask_offline, StateName, StateData) ->
    fsm_next_state(StateName, StateData#state{ask_offline = false});
handle_info(Info, StateName, StateData) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    fsm_next_state(StateName, StateData).

-spec print_state(state()) -> state().
print_state(State = #state{pres_t = T, pres_f = F, pres_a = A}) ->
    State#state{pres_t = {pres_t, (?SETS):size(T)},
		pres_f = {pres_f, (?SETS):size(F)},
		pres_a = {pres_a, (?SETS):size(A)}}.

terminate(_Reason, StateName, StateData) ->
    case StateData#state.mgmt_state of
      resumed ->
	  ?INFO_MSG("Closing former stream of resumed session for ~s",
		    [jid:to_string(StateData#state.jid)]);
      _ ->
	  if StateName == session_established;
	     StateName == wait_for_resume ->
		 case StateData#state.authenticated of
		   replaced ->
		       ?INFO_MSG("(~w) Replaced session for ~s",
				 [StateData#state.socket,
				  jid:to_string(StateData#state.jid)]),
		       From = StateData#state.jid,
		       Lang = StateData#state.lang,
		       Status = <<"Replaced by new connection">>,
		       Packet = #presence{
				   type = unavailable,
				   status = xmpp:mk_text(Status, Lang)},
		       ejabberd_sm:close_session_unset_presence(StateData#state.sid,
								StateData#state.user,
								StateData#state.server,
								StateData#state.resource,
								Status),
		       presence_broadcast(StateData, From,
					  StateData#state.pres_a, Packet);
		   _ ->
		       ?INFO_MSG("(~w) Close session for ~s",
				 [StateData#state.socket,
				  jid:to_string(StateData#state.jid)]),
		       EmptySet = (?SETS):new(),
		       case StateData of
			 #state{pres_last = undefined, pres_a = EmptySet} ->
			     ejabberd_sm:close_session(StateData#state.sid,
						       StateData#state.user,
						       StateData#state.server,
						       StateData#state.resource);
			 _ ->
			     From = StateData#state.jid,
			     Packet = #presence{type = unavailable},
			     ejabberd_sm:close_session_unset_presence(StateData#state.sid,
								      StateData#state.user,
								      StateData#state.server,
								      StateData#state.resource,
								      <<"">>),
			     presence_broadcast(StateData, From,
						StateData#state.pres_a, Packet)
		       end,
		       case StateData#state.mgmt_state of
			 timeout ->
			     Info = [{num_stanzas_in,
				      StateData#state.mgmt_stanzas_in}],
			     ejabberd_sm:set_offline_info(StateData#state.sid,
							  StateData#state.user,
							  StateData#state.server,
							  StateData#state.resource,
							  Info);
			 _ ->
			    ok
		       end
		 end,
		 handle_unacked_stanzas(StateData),
		 bounce_messages();
	     true ->
		 ok
	  end
    end,
    catch send_trailer(StateData),
    (StateData#state.sockmod):close(StateData#state.socket),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
-spec change_shaper(state(), jid()) -> ok.
change_shaper(StateData, JID) ->
    Shaper = acl:access_matches(StateData#state.shaper,
				#{usr => jid:split(JID), ip => StateData#state.ip},
				StateData#state.server),
    (StateData#state.sockmod):change_shaper(StateData#state.socket,
					    Shaper).

-spec send_text(state(), iodata()) -> ok | {error, any()}.
send_text(StateData, Text) when StateData#state.mgmt_state == pending ->
    ?DEBUG("Cannot send text while waiting for resumption: ~p", [Text]);
send_text(StateData, Text) when StateData#state.xml_socket ->
    ?DEBUG("Send Text on stream = ~p", [Text]),
    (StateData#state.sockmod):send_xml(StateData#state.socket,
				       {xmlstreamraw, Text});
send_text(StateData, Text) when StateData#state.mgmt_state == active ->
    ?DEBUG("Send XML on stream = ~p", [Text]),
    case catch (StateData#state.sockmod):send(StateData#state.socket, Text) of
      {'EXIT', _} ->
	  (StateData#state.sockmod):close(StateData#state.socket),
	  {error, closed};
      _ ->
	  ok
    end;
send_text(StateData, Text) ->
    ?DEBUG("Send XML on stream = ~p", [Text]),
    (StateData#state.sockmod):send(StateData#state.socket, Text).

-spec send_element(state(), xmlel() | xmpp_element()) -> ok | {error, any()}.
send_element(StateData, El) when StateData#state.mgmt_state == pending ->
    ?DEBUG("Cannot send element while waiting for resumption: ~p", [El]);
send_element(StateData, #xmlel{} = El) when StateData#state.xml_socket ->
    (StateData#state.sockmod):send_xml(StateData#state.socket,
				       {xmlstreamelement, El});
send_element(StateData, #xmlel{} = El) ->
    send_text(StateData, fxml:element_to_binary(El));
send_element(StateData, Pkt) ->
    send_element(StateData, xmpp:encode(Pkt)).

-spec send_error(state(), xmlel() | stanza(), error()) -> ok.
send_error(StateData, Stanza, Error) ->
    Type = xmpp:get_type(Stanza),
    if Type == error; Type == result;
       Type == <<"error">>; Type == <<"result">> ->
	    ok;
       true ->
	    send_element(StateData, xmpp:make_error(Stanza, Error))
    end.

-spec send_stanza(state(), xmpp_element()) -> state().
send_stanza(StateData, Stanza) when StateData#state.csi_state == inactive ->
    csi_filter_stanza(StateData, Stanza);
send_stanza(StateData, Stanza) when StateData#state.mgmt_state == pending ->
    mgmt_queue_add(StateData, Stanza);
send_stanza(StateData, Stanza) when StateData#state.mgmt_state == active ->
    NewStateData = send_stanza_and_ack_req(StateData, Stanza),
    mgmt_queue_add(NewStateData, Stanza);
send_stanza(StateData, Stanza) ->
    send_element(StateData, Stanza),
    StateData.

-spec send_packet(state(), xmpp_element()) -> state().
send_packet(StateData, Packet) ->
    case xmpp:is_stanza(Packet) of
      true ->
	  send_stanza(StateData, Packet);
      false ->
	  send_element(StateData, Packet),
	  StateData
    end.

-spec send_header(state(), binary(), binary(), binary()) -> ok | {error, any()}.
send_header(StateData, Server, Version, Lang)
    when StateData#state.xml_socket ->
    VersionAttr = case Version of
		    <<"">> -> [];
		    _ -> [{<<"version">>, Version}]
		  end,
    LangAttr = case Lang of
		 <<"">> -> [];
		 _ -> [{<<"xml:lang">>, Lang}]
	       end,
    Header = {xmlstreamstart, <<"stream:stream">>,
	      VersionAttr ++
		LangAttr ++
		  [{<<"xmlns">>, <<"jabber:client">>},
		   {<<"xmlns:stream">>,
		    <<"http://etherx.jabber.org/streams">>},
		   {<<"id">>, StateData#state.streamid},
		   {<<"from">>, Server}]},
    (StateData#state.sockmod):send_xml(StateData#state.socket,
				       Header);
send_header(StateData, Server, Version, Lang) ->
    VersionStr = case Version of
		   <<"">> -> <<"">>;
		   _ -> [<<" version='">>, Version, <<"'">>]
		 end,
    LangStr = case Lang of
		<<"">> -> <<"">>;
		_ -> [<<" xml:lang='">>, Lang, <<"'">>]
	      end,
    Header = io_lib:format(?STREAM_HEADER,
			   [StateData#state.streamid, Server, VersionStr,
			    LangStr]),
    send_text(StateData, iolist_to_binary(Header)).

-spec send_trailer(state()) -> ok | {error, any()}.
send_trailer(StateData)
    when StateData#state.mgmt_state == pending ->
    ?DEBUG("Cannot send stream trailer while waiting for resumption", []);
send_trailer(StateData)
    when StateData#state.xml_socket ->
    (StateData#state.sockmod):send_xml(StateData#state.socket,
				       {xmlstreamend, <<"stream:stream">>});
send_trailer(StateData) ->
    send_text(StateData, ?STREAM_TRAILER).

-spec new_id() -> binary().
new_id() -> randoms:get_string().

-spec new_uniq_id() -> binary().
new_uniq_id() ->
    iolist_to_binary([randoms:get_string(),
		      jlib:integer_to_binary(p1_time_compat:unique_integer([positive]))]).

-spec get_conn_type(state()) -> c2s | c2s_tls | c2s_compressed | websocket |
				c2s_compressed_tls | http_bind.
get_conn_type(StateData) ->
    case (StateData#state.sockmod):get_transport(StateData#state.socket) of
	tcp -> c2s;
	tls -> c2s_tls;
	tcp_zlib -> c2s_compressed;
	tls_zlib -> c2s_compressed_tls;
	http_bind -> http_bind;
	websocket -> websocket
    end.

process_presence_probe(From, To, StateData) ->
    LFrom = jid:tolower(From),
    LBFrom = setelement(3, LFrom, <<"">>),
    case StateData#state.pres_last of
	undefined ->
	    ok;
	_ ->
	    Cond = ((?SETS):is_element(LFrom, StateData#state.pres_f)
		    orelse
		    ((LFrom /= LBFrom) andalso
		     (?SETS):is_element(LBFrom, StateData#state.pres_f))),
	    if Cond ->
		    %% To is the one sending the presence (the probe target)
		    Packet = xmpp_util:add_delay_info(
			       StateData#state.pres_last, To,
			       StateData#state.pres_timestamp),
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
    #presence{type = Type} = Packet,
    case Type of
      unavailable ->
	  Status = xmpp:get_text(Packet#presence.status),
	  Info = [{ip, StateData#state.ip},
		  {conn, StateData#state.conn},
		  {auth_module, StateData#state.auth_module}],
	  ejabberd_sm:unset_presence(StateData#state.sid,
				     StateData#state.user,
				     StateData#state.server,
				     StateData#state.resource, Status, Info),
	  presence_broadcast(StateData, From,
			     StateData#state.pres_a, Packet),
	  StateData#state{pres_last = undefined,
			  pres_timestamp = undefined, pres_a = (?SETS):new()};
      error -> StateData;
      probe -> StateData;
      subscribe -> StateData;
      subscribed -> StateData;
      unsubscribe -> StateData;
      unsubscribed -> StateData;
      _ ->
	  OldPriority = case StateData#state.pres_last of
			  undefined -> 0;
			  OldPresence -> get_priority_from_presence(OldPresence)
			end,
	  NewPriority = get_priority_from_presence(Packet),
	  update_priority(NewPriority, Packet, StateData),
	  FromUnavail = (StateData#state.pres_last == undefined),
	  ?DEBUG("from unavail = ~p~n", [FromUnavail]),
	  NewStateData = StateData#state{pres_last = Packet,
					 pres_timestamp = p1_time_compat:timestamp()},
	  NewState = if FromUnavail ->
			    ejabberd_hooks:run(user_available_hook,
					       NewStateData#state.server,
					       [NewStateData#state.jid]),
			    ResentStateData = if NewPriority >= 0 ->
						     resend_offline_messages(NewStateData),
						     resend_subscription_requests(NewStateData);
						 true -> NewStateData
					      end,
			    presence_broadcast_first(From, ResentStateData,
						     Packet);
			true ->
			    presence_broadcast_to_trusted(NewStateData, From,
							  NewStateData#state.pres_f,
							  NewStateData#state.pres_a,
							  Packet),
			    if OldPriority < 0, NewPriority >= 0 ->
				   resend_offline_messages(NewStateData);
			       true -> ok
			    end,
			    NewStateData
		     end,
	  NewState
    end.

%% User sends a directed presence packet
presence_track(From, To, Packet, StateData) ->
    #presence{type = Type} = Packet,
    LTo = jid:tolower(To),
    User = StateData#state.user,
    Server = StateData#state.server,
    case Type of
      unavailable ->
	  A = remove_element(LTo, StateData#state.pres_a),
	  check_privacy_route(From, StateData#state{pres_a = A}, From, To, Packet);
      subscribe ->
	  try_roster_subscribe(subscribe, User, Server, From, To, Packet, StateData);
      subscribed ->
	  ejabberd_hooks:run(roster_out_subscription, Server,
			     [User, Server, To, subscribed]),
	  check_privacy_route(From, StateData,
			      jid:remove_resource(From), To, Packet);
      unsubscribe ->
	  try_roster_subscribe(unsubscribe, User, Server, From, To, Packet, StateData);
      unsubscribed ->
	  ejabberd_hooks:run(roster_out_subscription, Server,
			     [User, Server, To, unsubscribed]),
	  check_privacy_route(From, StateData,
			      jid:remove_resource(From), To, Packet);
      error ->
	  check_privacy_route(From, StateData, From, To, Packet);
      probe ->
	  check_privacy_route(From, StateData, From, To, Packet);
      _ ->
	  A = (?SETS):add_element(LTo, StateData#state.pres_a),
	  check_privacy_route(From, StateData#state{pres_a = A}, From, To, Packet)
    end.

check_privacy_route(From, StateData, FromRoute, To,
		    Packet) ->
    case privacy_check_packet(StateData, From, To, Packet,
			      out)
	of
        deny ->
            Lang = StateData#state.lang,
            ErrText = <<"Your active privacy list has denied "
                       "the routing of this stanza.">>,
	    Err = xmpp:make_error(
		    xmpp:set_from_to(Packet, From, To),
		    xmpp:err_not_acceptable(ErrText, Lang)),
            send_stanza(StateData, Err);
        allow ->
	    ejabberd_router:route(FromRoute, To, Packet),
            StateData
    end.

%% Check if privacy rules allow this delivery
privacy_check_packet(StateData, From, To, Packet,
		     Dir) ->
    ejabberd_hooks:run_fold(privacy_check_packet,
			    StateData#state.server, allow,
			    [StateData#state.user, StateData#state.server,
			     StateData#state.privacy_list, {From, To, Packet},
			     Dir]).

is_privacy_allow(StateData, From, To, Packet, Dir) ->
    allow ==
      privacy_check_packet(StateData, From, To, Packet, Dir).

%%% Check ACL before allowing to send a subscription stanza
try_roster_subscribe(Type, User, Server, From, To, Packet, StateData) ->
    JID1 = jid:make(User, Server, <<"">>),
    Access = gen_mod:get_module_opt(Server, mod_roster, access, fun(A) when is_atom(A) -> A end, all),
    case acl:match_rule(Server, Access, JID1) of
	deny ->
	    %% Silently drop this (un)subscription request
	    StateData;
	allow ->
	    ejabberd_hooks:run(roster_out_subscription,
			       Server,
			       [User, Server, To, Type]),
	    check_privacy_route(From, StateData, jid:remove_resource(From),
				To, Packet)
    end.

%% Send presence when disconnecting
presence_broadcast(StateData, From, JIDSet, Packet) ->
    JIDs = ?SETS:to_list(JIDSet),
    JIDs2 = format_and_check_privacy(From, StateData, Packet, JIDs, out),
    Server = StateData#state.server,
    send_multiple(From, Server, JIDs2, Packet).

%% Send presence when updating presence
presence_broadcast_to_trusted(StateData, From, Trusted, JIDSet, Packet) ->
    JIDs = ?SETS:to_list(JIDSet),
    JIDs_trusted = [JID || JID <- JIDs, ?SETS:is_element(JID, Trusted)],
    JIDs2 = format_and_check_privacy(From, StateData, Packet, JIDs_trusted, out),
    Server = StateData#state.server,
    send_multiple(From, Server, JIDs2, Packet).

%% Send presence when connecting
presence_broadcast_first(From, StateData, Packet) ->
    JIDsProbe =
	?SETS:fold(
	   fun(JID, L) -> [JID | L] end,
	   [],
	   StateData#state.pres_t),
    PacketProbe = #presence{type = probe},
    JIDs2Probe = format_and_check_privacy(From, StateData, PacketProbe, JIDsProbe, out),
    Server = StateData#state.server,
    send_multiple(From, Server, JIDs2Probe, PacketProbe),
    {As, JIDs} =
	?SETS:fold(
	   fun(JID, {A, JID_list}) ->
		   {?SETS:add_element(JID, A), JID_list++[JID]}
	   end,
	   {StateData#state.pres_a, []},
	   StateData#state.pres_f),
    JIDs2 = format_and_check_privacy(From, StateData, Packet, JIDs, out),
    send_multiple(From, Server, JIDs2, Packet),
    StateData#state{pres_a = As}.

format_and_check_privacy(From, StateData, Packet, JIDs, Dir) ->
    FJIDs = [jid:make(JID) || JID <- JIDs],
    lists:filter(
      fun(FJID) ->
	      case ejabberd_hooks:run_fold(
		     privacy_check_packet, StateData#state.server,
		     allow,
		     [StateData#state.user,
		      StateData#state.server,
		      StateData#state.privacy_list,
		      {From, FJID, Packet},
		      Dir]) of
		  deny -> false;
		  allow -> true
	      end
      end,
      FJIDs).

send_multiple(From, Server, JIDs, Packet) ->
    ejabberd_router_multicast:route_multicast(From, Server, JIDs, Packet).

remove_element(E, Set) ->
    case (?SETS):is_element(E, Set) of
      true -> (?SETS):del_element(E, Set);
      _ -> Set
    end.

roster_change(IJID, ISubscription, StateData) ->
    LIJID = jid:tolower(IJID),
    IsFrom = (ISubscription == both) or (ISubscription == from),
    IsTo = (ISubscription == both) or (ISubscription == to),
    OldIsFrom = (?SETS):is_element(LIJID, StateData#state.pres_f),
    FSet = if
	       IsFrom -> (?SETS):add_element(LIJID, StateData#state.pres_f);
	       true -> remove_element(LIJID, StateData#state.pres_f)
	   end,
    TSet = if
	       IsTo -> (?SETS):add_element(LIJID, StateData#state.pres_t);
	       true -> remove_element(LIJID, StateData#state.pres_t)
	   end,
    case StateData#state.pres_last of
      undefined ->
	  StateData#state{pres_f = FSet, pres_t = TSet};
      P ->
	  ?DEBUG("roster changed for ~p~n",
		 [StateData#state.user]),
	  From = StateData#state.jid,
	  To = jid:make(IJID),
	  Cond1 = IsFrom andalso not OldIsFrom,
	  Cond2 = not IsFrom andalso OldIsFrom andalso
		    ((?SETS):is_element(LIJID, StateData#state.pres_a)),
	  if Cond1 ->
		 ?DEBUG("C1: ~p~n", [LIJID]),
		 case privacy_check_packet(StateData, From, To, P, out)
		     of
		   deny -> ok;
		   allow -> ejabberd_router:route(From, To, P)
		 end,
		 A = (?SETS):add_element(LIJID, StateData#state.pres_a),
		 StateData#state{pres_a = A, pres_f = FSet,
				 pres_t = TSet};
	     Cond2 ->
		 ?DEBUG("C2: ~p~n", [LIJID]),
		 PU = #presence{type = unavailable},
		 case privacy_check_packet(StateData, From, To, PU, out)
		     of
		   deny -> ok;
		   allow -> ejabberd_router:route(From, To, PU)
		 end,
		 A = remove_element(LIJID, StateData#state.pres_a),
		 StateData#state{pres_a = A, pres_f = FSet,
				 pres_t = TSet};
	     true -> StateData#state{pres_f = FSet, pres_t = TSet}
	  end
    end.

update_priority(Priority, Packet, StateData) ->
    Info = [{ip, StateData#state.ip}, {conn, StateData#state.conn},
	    {auth_module, StateData#state.auth_module}],
    ejabberd_sm:set_presence(StateData#state.sid,
			     StateData#state.user, StateData#state.server,
			     StateData#state.resource, Priority, Packet, Info).

get_priority_from_presence(#presence{priority = Prio}) ->
    case Prio of
	undefined -> 0;
	_ -> Prio
    end.

process_privacy_iq(#iq{from = From, to = To,
		       type = Type, lang = Lang} = IQ, StateData) ->
    Txt = <<"No module is handling this query">>,
    {Res, NewStateData} =
	case Type of
	    get ->
		R = ejabberd_hooks:run_fold(
		      privacy_iq_get,
		      StateData#state.server,
		      {error, xmpp:err_feature_not_implemented(Txt, Lang)},
		      [IQ, StateData#state.privacy_list]),
		{R, StateData};
	    set ->
		case ejabberd_hooks:run_fold(
		       privacy_iq_set,
		       StateData#state.server,
		       {error, xmpp:err_feature_not_implemented(Txt, Lang)},
		       [IQ])
		of
		    {result, R, NewPrivList} ->
			{{result, R},
			 StateData#state{privacy_list =
					     NewPrivList}};
		    R -> {R, StateData}
		end
	end,
    IQRes = case Res of
	      {result, Result} ->
		  xmpp:make_iq_result(IQ, Result);
	      {error, Error} ->
		  xmpp:make_error(IQ, Error)
	    end,
    ejabberd_router:route(To, From, IQRes),
    NewStateData.

resend_offline_messages(#state{ask_offline = true} = StateData) ->
    case ejabberd_hooks:run_fold(resend_offline_messages_hook,
				 StateData#state.server, [],
				 [StateData#state.user, StateData#state.server])
    of
      Rs -> %%when is_list(Rs) ->
	  lists:foreach(fun ({route, From, To, Packet}) ->
				Pass = case privacy_check_packet(StateData,
								 From, To,
								 Packet, in)
					   of
					 allow -> true;
					 deny -> false
				       end,
				if Pass ->
				       ejabberd_router:route(From, To, Packet);
				   true -> ok
				end
			end,
			Rs)
    end;
resend_offline_messages(_StateData) ->
    ok.

resend_subscription_requests(#state{user = User,
				    server = Server} = StateData) ->
    PendingSubscriptions =
	ejabberd_hooks:run_fold(resend_subscription_requests_hook,
				Server, [], [User, Server]),
    lists:foldl(fun (XMLPacket, AccStateData) ->
			send_packet(AccStateData, XMLPacket)
		end,
		StateData,
		PendingSubscriptions).

get_showtag(undefined) -> <<"unavailable">>;
get_showtag(#presence{show = undefined}) -> <<"available">>;
get_showtag(#presence{show = Show}) -> atom_to_binary(Show, utf8).

get_statustag(#presence{status = [#text{data = Status}|_]}) -> Status;
get_statustag(_) -> <<"">>.

process_unauthenticated_stanza(StateData, #iq{type = T, lang = L} = IQ)
  when T == set; T == get ->
    Lang = if L == undefined; L == <<"">> -> StateData#state.lang;
	      true -> L
	   end,
    NewIQ = IQ#iq{lang = Lang},
    Res = ejabberd_hooks:run_fold(c2s_unauthenticated_iq,
				  StateData#state.server, empty,
				  [StateData#state.server, NewIQ,
				   StateData#state.ip]),
    case Res of
	empty ->
	    Txt = <<"Authentication required">>,
	    Err0 = xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)),
	    Err1 = Err0#iq{from = jid:make(<<>>, StateData#state.server, <<>>),
			   to = undefined},
	    send_element(StateData, Err1);
	_ ->
	    send_element(StateData, Res)
    end;
process_unauthenticated_stanza(_StateData, _) ->
    %% Drop any stanza, which isn't IQ stanza
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

%% fsm_next_state_pack: Pack the StateData structure to improve
%% sharing.
-spec fsm_next_state_pack(state_name(), state()) -> fsm_transition().
fsm_next_state_pack(StateName, StateData) ->
    fsm_next_state_gc(StateName, pack(StateData)).

-spec fsm_next_state_gc(state_name(), state()) -> fsm_transition().
%% fsm_next_state_gc: Garbage collect the process heap to make use of
%% the newly packed StateData structure.
fsm_next_state_gc(StateName, PackedStateData) ->
    erlang:garbage_collect(),
    fsm_next_state(StateName, PackedStateData).

%% fsm_next_state: Generate the next_state FSM tuple with different
%% timeout, depending on the future state
-spec fsm_next_state(state_name(), state()) -> fsm_transition().
fsm_next_state(session_established, #state{mgmt_max_queue = exceeded} =
	       StateData) ->
    ?WARNING_MSG("ACK queue too long, terminating session for ~s",
		 [jid:to_string(StateData#state.jid)]),
    Err = xmpp:serr_policy_violation(<<"Too many unacked stanzas">>,
				     StateData#state.lang),
    send_element(StateData, Err),
    {stop, normal, StateData#state{mgmt_resend = false}};
fsm_next_state(session_established, #state{mgmt_state = pending} = StateData) ->
    fsm_next_state(wait_for_resume, StateData);
fsm_next_state(session_established, StateData) ->
    {next_state, session_established, StateData,
     ?C2S_HIBERNATE_TIMEOUT};
fsm_next_state(wait_for_resume, #state{mgmt_timeout = 0} = StateData) ->
    {stop, normal, StateData};
fsm_next_state(wait_for_resume, #state{mgmt_pending_since = undefined} =
	       StateData) ->
    ?INFO_MSG("Waiting for resumption of stream for ~s",
	      [jid:to_string(StateData#state.jid)]),
    {next_state, wait_for_resume,
     StateData#state{mgmt_state = pending, mgmt_pending_since = os:timestamp()},
     StateData#state.mgmt_timeout};
fsm_next_state(wait_for_resume, StateData) ->
    Diff = timer:now_diff(os:timestamp(), StateData#state.mgmt_pending_since),
    Timeout = max(StateData#state.mgmt_timeout - Diff div 1000, 1),
    {next_state, wait_for_resume, StateData, Timeout};
fsm_next_state(StateName, StateData) ->
    {next_state, StateName, StateData, ?C2S_OPEN_TIMEOUT}.

%% fsm_reply: Generate the reply FSM tuple with different timeout,
%% depending on the future state
fsm_reply(Reply, session_established, StateData) ->
    {reply, Reply, session_established, StateData,
     ?C2S_HIBERNATE_TIMEOUT};
fsm_reply(Reply, wait_for_resume, StateData) ->
    Diff = timer:now_diff(os:timestamp(), StateData#state.mgmt_pending_since),
    Timeout = max(StateData#state.mgmt_timeout - Diff div 1000, 1),
    {reply, Reply, wait_for_resume, StateData, Timeout};
fsm_reply(Reply, StateName, StateData) ->
    {reply, Reply, StateName, StateData, ?C2S_OPEN_TIMEOUT}.

%% Used by c2s blacklist plugins
is_ip_blacklisted(undefined, _Lang) -> false;
is_ip_blacklisted({IP, _Port}, Lang) ->
    ejabberd_hooks:run_fold(check_bl_c2s, false, [IP, Lang]).

%% Check from attributes
%% returns invalid-from|NewElement
-spec check_from(stanza(), jid()) -> 'invalid-from' | stanza().
check_from(Pkt, FromJID) ->
    JID = xmpp:get_from(Pkt),
    case JID of
	undefined ->
	    Pkt;
	#jid{} ->
	    if
		(JID#jid.luser == FromJID#jid.luser) and
		(JID#jid.lserver == FromJID#jid.lserver) and
		(JID#jid.lresource == FromJID#jid.lresource) ->
		    Pkt;
		(JID#jid.luser == FromJID#jid.luser) and
		(JID#jid.lserver == FromJID#jid.lserver) and
		(JID#jid.lresource == <<"">>) ->
		    Pkt;
		true ->
		    'invalid-from'
	    end
    end.

fsm_limit_opts(Opts) ->
    case lists:keysearch(max_fsm_queue, 1, Opts) of
      {value, {_, N}} when is_integer(N) -> [{max_queue, N}];
      _ ->
	  case ejabberd_config:get_option(
                 max_fsm_queue,
                 fun(I) when is_integer(I), I > 0 -> I end) of
            undefined -> [];
	    N -> [{max_queue, N}]
	  end
    end.

bounce_messages() ->
    receive
      {route, From, To, El} ->
	  ejabberd_router:route(From, To, El), bounce_messages()
      after 0 -> ok
    end.

process_compression_request(#compress{methods = []}, StateName, StateData) ->
    send_element(StateData, #compress_failure{reason = 'setup-failed'}),
    fsm_next_state(StateName, StateData);
process_compression_request(#compress{methods = Ms}, StateName, StateData) ->
    case lists:member(<<"zlib">>, Ms) of
	true ->
	    Socket = StateData#state.socket,
	    BCompressed = fxml:element_to_binary(xmpp:encode(#compressed{})),
	    ZlibSocket = (StateData#state.sockmod):compress(Socket, BCompressed),
	    fsm_next_state(wait_for_stream,
			   StateData#state{socket = ZlibSocket,
					   streamid = new_id()});
	false ->
	    send_element(StateData,
			 #compress_failure{reason = 'unsupported-method'}),
	    fsm_next_state(StateName, StateData)
    end.

%%%----------------------------------------------------------------------
%%% XEP-0191
%%%----------------------------------------------------------------------

route_blocking(What, StateData) ->
    SubEl = case What of
		{block, JIDs} ->
		    #block{items = JIDs};
		{unblock, JIDs} ->
		    #unblock{items = JIDs};
		unblock_all ->
		    #unblock{}
	    end,
    PrivPushIQ = #iq{type = set, id = <<"push">>, sub_els = [SubEl],
		     from = jid:remove_resource(StateData#state.jid),
		     to = StateData#state.jid},
    %% No need to replace active privacy list here,
    %% blocking pushes are always accompanied by
    %% Privacy List pushes
    send_stanza(StateData, PrivPushIQ).

%%%----------------------------------------------------------------------
%%% XEP-0198
%%%----------------------------------------------------------------------

stream_mgmt_enabled(#state{mgmt_state = disabled}) ->
    false;
stream_mgmt_enabled(_StateData) ->
    true.

dispatch_stream_mgmt(El, #state{mgmt_state = MgmtState} = StateData)
    when MgmtState == active;
	 MgmtState == pending ->
    perform_stream_mgmt(El, StateData);
dispatch_stream_mgmt(El, StateData) ->
    negotiate_stream_mgmt(El, StateData).

negotiate_stream_mgmt(_El, #state{resource = <<"">>} = StateData) ->
    %% XEP-0198 says: "For client-to-server connections, the client MUST NOT
    %% attempt to enable stream management until after it has completed Resource
    %% Binding unless it is resuming a previous session".  However, it also
    %% says: "Stream management errors SHOULD be considered recoverable", so we
    %% won't bail out.
    send_element(StateData, #sm_failed{reason = 'unexpected-request',
				       xmlns = ?NS_STREAM_MGMT_3}),
    StateData;
negotiate_stream_mgmt(Pkt, StateData) ->
    Xmlns = xmpp:get_ns(Pkt),
    case stream_mgmt_enabled(StateData) of
	true ->
	    case Pkt of
		#sm_enable{} ->
		    handle_enable(StateData#state{mgmt_xmlns = Xmlns}, Pkt);
		_ ->
		    Res = if is_record(Pkt, sm_a);
			     is_record(Pkt, sm_r);
			     is_record(Pkt, sm_resume) ->
				  #sm_failed{reason = 'unexpected-request',
					     xmlns = Xmlns};
			     true ->
				  #sm_failed{reason = 'bad-request',
					     xmlns = Xmlns}
			  end,
		    send_element(StateData, Res),
		    StateData
	    end;
	false ->
	    send_element(StateData,
			 #sm_failed{reason = 'service-unavailable',
				    xmlns = Xmlns}),
	    StateData
    end.

perform_stream_mgmt(Pkt, StateData) ->
    case xmpp:get_ns(Pkt) of
	Xmlns when Xmlns == StateData#state.mgmt_xmlns ->
	    case Pkt of
		#sm_r{} ->
		    handle_r(StateData);
		#sm_a{} ->
		    handle_a(StateData, Pkt);
		_ ->
		    Res = if is_record(Pkt, sm_enable);
			     is_record(Pkt, sm_resume) ->
				  #sm_failed{reason = 'unexpected-request',
					     xmlns = Xmlns};
			     true ->
				  #sm_failed{reason = 'bad-request',
					     xmlns = Xmlns}
			  end,
		    send_element(StateData, Res),
		    StateData
	    end;
	_ ->
	    send_element(StateData,
			 #sm_failed{reason = 'unsupported-version',
				    xmlns = StateData#state.mgmt_xmlns})
    end.

handle_enable(#state{mgmt_timeout = DefaultTimeout,
		     mgmt_max_timeout = MaxTimeout} = StateData,
	      #sm_enable{resume = Resume, max = Max}) ->
    Timeout = if Resume == false ->
		      0;
		 Max /= undefined, Max > 0, Max =< MaxTimeout ->
		      Max;
		 true ->
		      DefaultTimeout
	      end,
    Res = if Timeout > 0 ->
		  ?INFO_MSG("Stream management with resumption enabled for ~s",
			    [jid:to_string(StateData#state.jid)]),
		  #sm_enabled{xmlns = StateData#state.mgmt_xmlns,
			      id = make_resume_id(StateData),
			      resume = true,
			      max = Timeout};
	     true ->
		  ?INFO_MSG("Stream management without resumption enabled for ~s",
			    [jid:to_string(StateData#state.jid)]),
		  #sm_enabled{xmlns = StateData#state.mgmt_xmlns}
	  end,
    send_element(StateData, Res),
    StateData#state{mgmt_state = active,
		    mgmt_queue = queue:new(),
		    mgmt_timeout = Timeout * 1000}.

handle_r(StateData) ->
    Res = #sm_a{xmlns = StateData#state.mgmt_xmlns,
		h = StateData#state.mgmt_stanzas_in},
    send_element(StateData, Res),
    StateData.

handle_a(StateData, #sm_a{h = H}) ->
    check_h_attribute(StateData, H).

handle_resume(StateData, #sm_resume{h = H, previd = PrevID, xmlns = Xmlns}) ->
    R = case stream_mgmt_enabled(StateData) of
	    true ->
		case inherit_session_state(StateData, PrevID) of
		    {ok, InheritedState} ->
			{ok, InheritedState, H};
		    {error, Err, InH} ->
			{error, #sm_failed{reason = 'item-not-found',
					   h = InH, xmlns = Xmlns}, Err};
		    {error, Err} ->
			{error, #sm_failed{reason = 'item-not-found',
					   xmlns = Xmlns}, Err}
		end;
	    false ->
		{error, #sm_failed{reason = 'service-unavailable',
				   xmlns = Xmlns},
		 <<"XEP-0198 disabled">>}
	end,
    case R of
      {ok, ResumedState, NumHandled} ->
	  NewState = check_h_attribute(ResumedState, NumHandled),
	  AttrXmlns = NewState#state.mgmt_xmlns,
	  AttrId = make_resume_id(NewState),
	  AttrH = NewState#state.mgmt_stanzas_in,
	  send_element(NewState, #sm_resumed{xmlns = AttrXmlns,
					     h = AttrH,
					     previd = AttrId}),
	  SendFun = fun(_F, _T, El, Time) ->
			    NewEl = add_resent_delay_info(NewState, El, Time),
			    send_element(NewState, NewEl)
		    end,
	  handle_unacked_stanzas(NewState, SendFun),
	  send_element(NewState, #sm_r{xmlns = AttrXmlns}),
	  FlushedState = csi_flush_queue(NewState),
	  NewStateData = FlushedState#state{csi_state = active},
	  ?INFO_MSG("Resumed session for ~s",
		    [jid:to_string(NewStateData#state.jid)]),
	  {ok, NewStateData};
      {error, El, Msg} ->
	  send_element(StateData, El),
	  ?INFO_MSG("Cannot resume session for ~s@~s: ~s",
		    [StateData#state.user, StateData#state.server, Msg]),
	  error
    end.

check_h_attribute(#state{mgmt_stanzas_out = NumStanzasOut} = StateData, H)
    when H > NumStanzasOut ->
    ?DEBUG("~s acknowledged ~B stanzas, but only ~B were sent",
	   [jid:to_string(StateData#state.jid), H, NumStanzasOut]),
    mgmt_queue_drop(StateData#state{mgmt_stanzas_out = H}, NumStanzasOut);
check_h_attribute(#state{mgmt_stanzas_out = NumStanzasOut} = StateData, H) ->
    ?DEBUG("~s acknowledged ~B of ~B stanzas",
	   [jid:to_string(StateData#state.jid), H, NumStanzasOut]),
    mgmt_queue_drop(StateData, H).

update_num_stanzas_in(#state{mgmt_state = MgmtState} = StateData, El)
    when MgmtState == active;
	 MgmtState == pending ->
    NewNum = case {xmpp:is_stanza(El), StateData#state.mgmt_stanzas_in} of
	       {true, 4294967295} ->
		   0;
	       {true, Num} ->
		   Num + 1;
	       {false, Num} ->
		   Num
	     end,
    StateData#state{mgmt_stanzas_in = NewNum};
update_num_stanzas_in(StateData, _El) ->
    StateData.

send_stanza_and_ack_req(StateData, Stanza) ->
    AckReq = #sm_r{xmlns = StateData#state.mgmt_xmlns},
    case send_element(StateData, Stanza) == ok andalso
	 send_element(StateData, AckReq) == ok of
      true ->
	  StateData;
      false ->
	  StateData#state{mgmt_state = pending}
    end.

mgmt_queue_add(StateData, El) ->
    NewNum = case StateData#state.mgmt_stanzas_out of
	       4294967295 ->
		   0;
	       Num ->
		   Num + 1
	     end,
    NewQueue = queue:in({NewNum, p1_time_compat:timestamp(), El}, StateData#state.mgmt_queue),
    NewState = StateData#state{mgmt_queue = NewQueue,
			       mgmt_stanzas_out = NewNum},
    check_queue_length(NewState).

mgmt_queue_drop(StateData, NumHandled) ->
    NewQueue = jlib:queue_drop_while(fun({N, _T, _E}) -> N =< NumHandled end,
				     StateData#state.mgmt_queue),
    StateData#state{mgmt_queue = NewQueue}.

check_queue_length(#state{mgmt_max_queue = Limit} = StateData)
    when Limit == infinity;
	 Limit == exceeded ->
    StateData;
check_queue_length(#state{mgmt_queue = Queue,
			  mgmt_max_queue = Limit} = StateData) ->
    case queue:len(Queue) > Limit of
      true ->
	  StateData#state{mgmt_max_queue = exceeded};
      false ->
	  StateData
    end.

handle_unacked_stanzas(#state{mgmt_state = MgmtState} = StateData, F)
    when MgmtState == active;
	 MgmtState == pending;
	 MgmtState == timeout ->
    Queue = StateData#state.mgmt_queue,
    case queue:len(Queue) of
      0 ->
	  ok;
      N ->
	  ?DEBUG("~B stanza(s) were not acknowledged by ~s",
		 [N, jid:to_string(StateData#state.jid)]),
	  lists:foreach(
	    fun({_, Time, Pkt}) ->
		    From = xmpp:get_from(Pkt),
		    To = xmpp:get_to(Pkt),
		    F(From, To, Pkt, Time)
	    end, queue:to_list(Queue))
    end;
handle_unacked_stanzas(_StateData, _F) ->
    ok.

handle_unacked_stanzas(#state{mgmt_state = MgmtState} = StateData)
    when MgmtState == active;
	 MgmtState == pending;
	 MgmtState == timeout ->
    ResendOnTimeout =
	case StateData#state.mgmt_resend of
	  Resend when is_boolean(Resend) ->
	      Resend;
	  if_offline ->
	      Resource = StateData#state.resource,
	      case ejabberd_sm:get_user_resources(StateData#state.user,
						  StateData#state.server) of
		[Resource] -> % Same resource opened new session
		    true;
		[] ->
		    true;
		_ ->
		    false
	      end
	end,
    Lang = StateData#state.lang,
    ReRoute = case ResendOnTimeout of
		true ->
		    fun(From, To, El, Time) ->
			    NewEl = add_resent_delay_info(StateData, El, Time),
			    ejabberd_router:route(From, To, NewEl)
		    end;
		false ->
		    fun(From, To, El, _Time) ->
			    Txt = <<"User session terminated">>,
			    Err = xmpp:make_error(
				    El, xmpp:err_service_unavailable(Txt, Lang)),
			    ejabberd_router:route(To, From, Err)
		    end
	      end,
    F = fun(From, _To, #presence{}, _Time) ->
		?DEBUG("Dropping presence stanza from ~s",
		       [jid:to_string(From)]);
	   (From, To, #iq{} = El, _Time) ->
		Txt = <<"User session terminated">>,
		Err = xmpp:make_error(
			El, xmpp:err_service_unavailable(Txt, Lang)),
		ejabberd_router:route(To, From, Err);
	   (From, To, El, Time) ->
		%% We'll drop the stanza if it was <forwarded/> by some
		%% encapsulating protocol as per XEP-0297.  One such protocol is
		%% XEP-0280, which says: "When a receiving server attempts to
		%% deliver a forked message, and that message bounces with an
		%% error for any reason, the receiving server MUST NOT forward
		%% that error back to the original sender."  Resending such a
		%% stanza could easily lead to unexpected results as well.
		case is_encapsulated_forward(El) of
		  true ->
		      ?DEBUG("Dropping forwarded message stanza from ~s",
			     [jid:to_string(From)]);
		  false ->
		      case ejabberd_hooks:run_fold(message_is_archived,
						   StateData#state.server,
						   false,
						   [StateData, From,
						    StateData#state.jid, El]) of
			true ->
			    ok;
			false ->
			    ReRoute(From, To, El, Time)
		      end
		end
	end,
    handle_unacked_stanzas(StateData, F);
handle_unacked_stanzas(_StateData) ->
    ok.

is_encapsulated_forward(#message{} = Msg) ->
    xmpp:has_subtag(Msg, #forwarded{}) orelse
	xmpp:has_subtag(Msg, #carbons_sent{}) orelse
	xmpp:has_subtag(Msg, #carbons_received{});
is_encapsulated_forward(_El) ->
    false.

inherit_session_state(#state{user = U, server = S} = StateData, ResumeID) ->
    case jlib:base64_to_term(ResumeID) of
      {term, {R, Time}} ->
	  case ejabberd_sm:get_session_pid(U, S, R) of
	    none ->
		case ejabberd_sm:get_offline_info(Time, U, S, R) of
		  none ->
		      {error, <<"Previous session PID not found">>};
		  Info ->
		      case proplists:get_value(num_stanzas_in, Info) of
			undefined ->
			    {error, <<"Previous session timed out">>};
			H ->
			    {error, <<"Previous session timed out">>, H}
		      end
		end;
	    OldPID ->
		OldSID = {Time, OldPID},
		case catch resume_session(OldSID) of
		  {ok, OldStateData} ->
		      NewSID = {Time, self()}, % Old time, new PID
		      Priority = case OldStateData#state.pres_last of
				   undefined ->
				       0;
				   Presence ->
				       get_priority_from_presence(Presence)
				 end,
		      Conn = get_conn_type(StateData),
		      Info = [{ip, StateData#state.ip}, {conn, Conn},
			      {auth_module, StateData#state.auth_module}],
		      ejabberd_sm:open_session(NewSID, U, S, R,
					       Priority, Info),
		      {ok, StateData#state{conn = Conn,
					   sid = NewSID,
					   jid = OldStateData#state.jid,
					   resource = OldStateData#state.resource,
					   pres_t = OldStateData#state.pres_t,
					   pres_f = OldStateData#state.pres_f,
					   pres_a = OldStateData#state.pres_a,
					   pres_last = OldStateData#state.pres_last,
					   pres_timestamp = OldStateData#state.pres_timestamp,
					   privacy_list = OldStateData#state.privacy_list,
					   aux_fields = OldStateData#state.aux_fields,
					   csi_state = OldStateData#state.csi_state,
					   mgmt_xmlns = OldStateData#state.mgmt_xmlns,
					   mgmt_queue = OldStateData#state.mgmt_queue,
					   mgmt_timeout = OldStateData#state.mgmt_timeout,
					   mgmt_stanzas_in = OldStateData#state.mgmt_stanzas_in,
					   mgmt_stanzas_out = OldStateData#state.mgmt_stanzas_out,
					   mgmt_state = active}};
		  {error, Msg} ->
		      {error, Msg};
		  _ ->
		      {error, <<"Cannot grab session state">>}
		end
	  end;
      _ ->
	  {error, <<"Invalid 'previd' value">>}
    end.

resume_session({Time, PID}) ->
    (?GEN_FSM):sync_send_all_state_event(PID, {resume_session, Time}, 5000).

make_resume_id(StateData) ->
    {Time, _} = StateData#state.sid,
    jlib:term_to_base64({StateData#state.resource, Time}).

add_resent_delay_info(_State, #iq{} = El, _Time) ->
    El;
add_resent_delay_info(#state{server = From}, El, Time) ->
    xmpp_util:add_delay_info(El, From, Time, <<"Resent">>).

%%%----------------------------------------------------------------------
%%% XEP-0352
%%%----------------------------------------------------------------------

csi_filter_stanza(#state{csi_state = CsiState, server = Server} = StateData,
		  Stanza) ->
    {StateData1, Stanzas} = ejabberd_hooks:run_fold(csi_filter_stanza, Server,
						    {StateData, [Stanza]},
						    [Server, Stanza]),
    StateData2 = lists:foldl(fun(CurStanza, AccState) ->
				     send_stanza(AccState, CurStanza)
			     end, StateData1#state{csi_state = active},
			     Stanzas),
    StateData2#state{csi_state = CsiState}.

csi_flush_queue(#state{csi_state = CsiState, server = Server} = StateData) ->
    {StateData1, Stanzas} = ejabberd_hooks:run_fold(csi_flush_queue, Server,
						    {StateData, []}, [Server]),
    StateData2 = lists:foldl(fun(CurStanza, AccState) ->
				     send_stanza(AccState, CurStanza)
			     end, StateData1#state{csi_state = active},
			     Stanzas),
    StateData2#state{csi_state = CsiState}.

%%%----------------------------------------------------------------------
%%% JID Set memory footprint reduction code
%%%----------------------------------------------------------------------

%% Try to reduce the heap footprint of the four presence sets
%% by ensuring that we re-use strings and Jids wherever possible.
pack(S = #state{pres_a = A, pres_f = F,
		pres_t = T}) ->
    {NewA, Pack2} = pack_jid_set(A, gb_trees:empty()),
    {NewF, Pack3} = pack_jid_set(F, Pack2),
    {NewT, _Pack4} = pack_jid_set(T, Pack3),
    S#state{pres_a = NewA, pres_f = NewF,
	    pres_t = NewT}.

pack_jid_set(Set, Pack) ->
    Jids = (?SETS):to_list(Set),
    {PackedJids, NewPack} = pack_jids(Jids, Pack, []),
    {(?SETS):from_list(PackedJids), NewPack}.

pack_jids([], Pack, Acc) -> {Acc, Pack};
pack_jids([{U, S, R} = Jid | Jids], Pack, Acc) ->
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
      {value, PackedString} -> {PackedString, Pack};
      none -> {String, gb_trees:insert(String, String, Pack)}
    end.

transform_listen_option(Opt, Opts) ->
    [Opt|Opts].

identity(Props) ->
    case proplists:get_value(authzid, Props, <<>>) of
	<<>> -> proplists:get_value(username, Props, <<>>);
	AuthzId -> AuthzId
    end.

opt_type(domain_certfile) -> fun iolist_to_binary/1;
opt_type(max_fsm_queue) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(resource_conflict) ->
    fun (setresource) -> setresource;
	(closeold) -> closeold;
	(closenew) -> closenew;
	(acceptnew) -> acceptnew
    end;
opt_type(_) ->
    [domain_certfile, max_fsm_queue, resource_conflict].
