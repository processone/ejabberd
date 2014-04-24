%%%----------------------------------------------------------------------
%%% File    : ejabberd_c2s.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve C2S connection
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
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

-author('alexey@process-one.net').

-update_info({update, 0}).

-define(GEN_FSM, p1_fsm).

-behaviour(?GEN_FSM).

%% External exports
-export([start/2,
	 stop/1,
	 start_link/2,
	 send_text/2,
	 send_element/2,
	 socket_type/0,
	 get_presence/1,
	 get_aux_field/2,
	 set_aux_field/3,
	 del_aux_field/2,
	 get_subscription/2,
	 broadcast/4,
	 get_subscribed/1,
         transform_listen_option/2]).

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
	 print_state/1
     ]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

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
		user = "", server = <<"">>, resource = <<"">>,
		sid,
		pres_t = ?SETS:new(),
		pres_f = ?SETS:new(),
		pres_a = ?SETS:new(),
		pres_i = ?SETS:new(),
		pres_last, pres_pri,
		pres_timestamp,
		pres_invis = false,
		privacy_list = #userlist{},
		conn = unknown,
		auth_module = unknown,
		ip,
		aux_fields = [],
		lang}).

%-define(DBGFSM, true).

-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

-endif.

%% Module start with or without supervisor:
-ifdef(NO_TRANSIENT_SUPERVISORS).
-define(SUPERVISOR_START, ?GEN_FSM:start(ejabberd_c2s, [SockData, Opts],
					 fsm_limit_opts(Opts) ++ ?FSMOPTS)).
-else.
-define(SUPERVISOR_START, supervisor:start_child(ejabberd_c2s_sup,
						 [SockData, Opts])).
-endif.

%% This is the timeout to apply between event when starting a new
%% session:
-define(C2S_OPEN_TIMEOUT, 60000).

-define(C2S_HIBERNATE_TIMEOUT, 90000).

-define(STREAM_HEADER,
	<<"<?xml version='1.0'?><stream:stream "
	  "xmlns='jabber:client' xmlns:stream='http://et"
	  "herx.jabber.org/streams' id='~s' from='~s'~s~"
	  "s>">>).

-define(STREAM_TRAILER, <<"</stream:stream>">>).

-define(INVALID_NS_ERR, ?SERR_INVALID_NAMESPACE).

-define(INVALID_XML_ERR, ?SERR_XML_NOT_WELL_FORMED).

-define(HOST_UNKNOWN_ERR, ?SERR_HOST_UNKNOWN).

-define(POLICY_VIOLATION_ERR(Lang, Text),
	?SERRT_POLICY_VIOLATION(Lang, Text)).

-define(INVALID_FROM, ?SERR_INVALID_FROM).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(SockData, Opts) ->
    ?SUPERVISOR_START.

start_link(SockData, Opts) ->
    ?GEN_FSM:start_link(ejabberd_c2s, [SockData, Opts],
			fsm_limit_opts(Opts) ++ ?FSMOPTS).

socket_type() -> xml_stream.

%% Return Username, Resource and presence information
get_presence(FsmRef) ->
    (?GEN_FSM):sync_send_all_state_event(FsmRef,
					 {get_presence}, 1000).

get_aux_field(Key, #state{aux_fields = Opts}) ->
    case lists:keysearch(Key, 1, Opts) of
      {value, {_, Val}} -> {ok, Val};
      _ -> error
    end.

set_aux_field(Key, Val,
	      #state{aux_fields = Opts} = State) ->
    Opts1 = lists:keydelete(Key, 1, Opts),
    State#state{aux_fields = [{Key, Val} | Opts1]}.

del_aux_field(Key, #state{aux_fields = Opts} = State) ->
    Opts1 = lists:keydelete(Key, 1, Opts),
    State#state{aux_fields = Opts1}.

get_subscription(From = #jid{}, StateData) ->
    get_subscription(jlib:jid_tolower(From), StateData);
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

broadcast(FsmRef, Type, From, Packet) ->
    FsmRef ! {broadcast, Type, From, Packet}.

stop(FsmRef) -> (?GEN_FSM):send_event(FsmRef, closed).

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
    IP = peerip(SockMod, Socket),
    %% Check if IP is blacklisted:
    case is_ip_blacklisted(IP) of
      true ->
	  ?INFO_MSG("Connection attempt from blacklisted "
		    "IP: ~s (~w)",
		    [jlib:ip_to_list(IP), IP]),
	  {stop, normal};
      false ->
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
			     streamid = new_id(), access = Access,
			     shaper = Shaper, ip = IP},
	  {ok, wait_for_stream, StateData, ?C2S_OPEN_TIMEOUT}
    end.

%% Return list of all available resources of contacts,
get_subscribed(FsmRef) ->
    (?GEN_FSM):sync_send_all_state_event(FsmRef,
					 get_subscribed, 1000).

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

wait_for_stream({xmlstreamstart, _Name, Attrs}, StateData) ->
    DefaultLang = ?MYLANG,
    case xml:get_attr_s(<<"xmlns:stream">>, Attrs) of
	?NS_STREAM ->
            Server =
                case StateData#state.server of
                    <<"">> ->
                        jlib:nameprep(xml:get_attr_s(<<"to">>, Attrs));
                    S -> S
                end,
	    case lists:member(Server, ?MYHOSTS) of
		true ->
		    Lang = case xml:get_attr_s(<<"xml:lang">>, Attrs) of
			       Lang1 when size(Lang1) =< 35 ->
				   %% As stated in BCP47, 4.4.1:
				   %% Protocols or specifications that
				   %% specify limited buffer sizes for
				   %% language tags MUST allow for
				   %% language tags of at least 35 characters.
				   Lang1;
			       _ ->
				   %% Do not store long language tag to
				   %% avoid possible DoS/flood attacks
				   <<"">>
			   end,
		    change_shaper(StateData, jlib:make_jid(<<"">>, Server, <<"">>)),
		    case xml:get_attr_s(<<"version">>, Attrs) of
			<<"1.0">> ->
			    send_header(StateData, Server, <<"1.0">>, DefaultLang),
			    case StateData#state.authenticated of
				false ->
				    SASLState =
					cyrsasl:server_new(
					  <<"jabber">>, Server, <<"">>, [],
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
					  end),
				    Mechs = lists:map(fun (S) ->
						      #xmlel{name = <<"mechanism">>,
							     attrs = [],
							     children = [{xmlcdata, S}]}
					      end,
					      cyrsasl:listmech(Server)),
				    SockMod =
					(StateData#state.sockmod):get_sockmod(
					  StateData#state.socket),
				    Zlib = StateData#state.zlib,
				    CompressFeature =
					case Zlib andalso
					    ((SockMod == gen_tcp) orelse
					     (SockMod == p1_tls)) of
					    true ->
						[#xmlel{name = <<"compression">>,
							attrs = [{<<"xmlns">>, ?NS_FEATURE_COMPRESS}],
							children = [#xmlel{name = <<"method">>,
								    attrs = [],
								    children = [{xmlcdata, <<"zlib">>}]}]}];
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
						case TLSRequired of
						    true ->
							[#xmlel{name = <<"starttls">>,
								attrs = [{<<"xmlns">>, ?NS_TLS}],
								children = [#xmlel{name = <<"required">>,
									    attrs = [],
									    children = []}]}];
						    _ ->
							[#xmlel{name = <<"starttls">>,
								attrs = [{<<"xmlns">>, ?NS_TLS}],
								children = []}]
						end;
					    false ->
						[]
					end,
				    send_element(StateData,
					    #xmlel{name = <<"stream:features">>,
						   attrs = [],
						   children =
						    TLSFeature ++ CompressFeature ++
						    [#xmlel{name = <<"mechanisms">>,
							    attrs = [{<<"xmlns">>, ?NS_SASL}],
							    children = Mechs}]
						    ++
						    ejabberd_hooks:run_fold(c2s_stream_features,
							Server, [], [Server])}),
				    fsm_next_state(wait_for_feature_request,
					       StateData#state{
						 server = Server,
						 sasl_state = SASLState,
						 lang = Lang});
				_ ->
				    case StateData#state.resource of
				    <<"">> ->
					RosterVersioningFeature =
					    ejabberd_hooks:run_fold(roster_get_versioning_feature,
								    Server, [],
								    [Server]),
					StreamFeatures = [#xmlel{name = <<"bind">>,
								attrs = [{<<"xmlns">>, ?NS_BIND}],
								children = []},
							    #xmlel{name = <<"session">>,
								attrs = [{<<"xmlns">>, ?NS_SESSION}],
								children = []}]
							    ++
							    RosterVersioningFeature ++
							    ejabberd_hooks:run_fold(c2s_stream_features,
								Server, [], [Server]),
					send_element(StateData,
						    #xmlel{name = <<"stream:features">>,
							    attrs = [],
							    children = StreamFeatures}),
					fsm_next_state(wait_for_bind,
							StateData#state{server = Server, lang = Lang});
				    _ ->
					send_element(StateData,
						    #xmlel{name = <<"stream:features">>,
							    attrs = [],
							    children = []}),
					fsm_next_state(wait_for_session,
							StateData#state{server = Server, lang = Lang})
				    end
			    end;
		_ ->
		    send_header(StateData, Server, <<"">>, DefaultLang),
		    if not StateData#state.tls_enabled and
			StateData#state.tls_required ->
			    send_element(StateData,
					?POLICY_VIOLATION_ERR(Lang,
							    <<"Use of STARTTLS required">>)),
			    send_trailer(StateData),
			    {stop, normal, StateData};
			true ->
			    fsm_next_state(wait_for_auth,
					StateData#state{server = Server,
							lang = Lang})
		    end
	    end;
	_ ->
	    send_header(StateData, ?MYNAME, <<"">>, DefaultLang),
	    send_element(StateData, ?HOST_UNKNOWN_ERR),
	    send_trailer(StateData),
	    {stop, normal, StateData}
	end;
    _ ->
	send_header(StateData, ?MYNAME, <<"">>, DefaultLang),
	send_element(StateData, ?INVALID_NS_ERR),
	send_trailer(StateData),
	{stop, normal, StateData}
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
    send_header(StateData, ?MYNAME, <<"1.0">>, <<"">>),
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.

wait_for_auth({xmlstreamelement, El}, StateData) ->
    case is_auth_packet(El) of
      {auth, _ID, get, {U, _, _, _}} ->
	  #xmlel{name = Name, attrs = Attrs} =
	      jlib:make_result_iq_reply(El),
	  case U of
	    <<"">> -> UCdata = [];
	    _ -> UCdata = [{xmlcdata, U}]
	  end,
	  Res = case
		  ejabberd_auth:plain_password_required(StateData#state.server)
		    of
		  false ->
		      #xmlel{name = Name, attrs = Attrs,
			     children =
				 [#xmlel{name = <<"query">>,
					 attrs = [{<<"xmlns">>, ?NS_AUTH}],
					 children =
					     [#xmlel{name = <<"username">>,
						     attrs = [],
						     children = UCdata},
					      #xmlel{name = <<"password">>,
						     attrs = [], children = []},
					      #xmlel{name = <<"digest">>,
						     attrs = [], children = []},
					      #xmlel{name = <<"resource">>,
						     attrs = [],
						     children = []}]}]};
		  true ->
		      #xmlel{name = Name, attrs = Attrs,
			     children =
				 [#xmlel{name = <<"query">>,
					 attrs = [{<<"xmlns">>, ?NS_AUTH}],
					 children =
					     [#xmlel{name = <<"username">>,
						     attrs = [],
						     children = UCdata},
					      #xmlel{name = <<"password">>,
						     attrs = [], children = []},
					      #xmlel{name = <<"resource">>,
						     attrs = [],
						     children = []}]}]}
		end,
	  send_element(StateData, Res),
	  fsm_next_state(wait_for_auth, StateData);
      {auth, _ID, set, {_U, _P, _D, <<"">>}} ->
	  Err = jlib:make_error_reply(El,
				      ?ERR_AUTH_NO_RESOURCE_PROVIDED((StateData#state.lang))),
	  send_element(StateData, Err),
	  fsm_next_state(wait_for_auth, StateData);
      {auth, _ID, set, {U, P, D, R}} ->
	  JID = jlib:make_jid(U, StateData#state.server, R),
	  case JID /= error andalso
		 acl:match_rule(StateData#state.server,
				StateData#state.access, JID)
		   == allow
	      of
	    true ->
		DGen = fun (PW) ->
			       p1_sha:sha(<<(StateData#state.streamid)/binary, PW/binary>>)
		       end,
		case ejabberd_auth:check_password_with_authmodule(U,
								  StateData#state.server,
								  P, D, DGen)
		    of
		  {true, AuthModule} ->
			?INFO_MSG("(~w) Accepted legacy authentication for ~s by ~p",
				[StateData#state.socket,
				 jlib:jid_to_string(JID), AuthModule]),
			SID = {now(), self()},
			Conn = get_conn_type(StateData),
			Info = [{ip, StateData#state.ip}, {conn, Conn},
				    {auth_module, AuthModule}],
                        Res = jlib:make_result_iq_reply(
                                El#xmlel{children = []}),
			send_element(StateData, Res),
			ejabberd_sm:open_session(SID, U, StateData#state.server, R, Info),
			change_shaper(StateData, JID),
			{Fs, Ts} =
			    ejabberd_hooks:run_fold(roster_get_subscription_lists,
						    StateData#state.server,
						    {[], []},
						    [U,
							StateData#state.server]),
			LJID =
			    jlib:jid_tolower(jlib:jid_remove_resource(JID)),
			Fs1 = [LJID | Fs],
			Ts1 = [LJID | Ts],
			PrivList = ejabberd_hooks:run_fold(privacy_get_user_list,
						    StateData#state.server,
						    #userlist{},
						    [U, StateData#state.server]),
			NewStateData = StateData#state{user = U,
							resource = R,
							jid = JID, sid = SID,
							conn = Conn,
							auth_module = AuthModule,
							pres_f = (?SETS):from_list(Fs1),
							pres_t = (?SETS):from_list(Ts1),
							privacy_list = PrivList},
			fsm_next_state(session_established, NewStateData);
		  _ ->
		      IP = peerip(StateData#state.sockmod,
				  StateData#state.socket),
		      ?INFO_MSG("(~w) Failed legacy authentication for "
				"~s from IP ~s",
				[StateData#state.socket,
				    jlib:jid_to_string(JID), jlib:ip_to_list(IP)]),
		      Err = jlib:make_error_reply(El, ?ERR_NOT_AUTHORIZED),
		      send_element(StateData, Err),
		      fsm_next_state(wait_for_auth, StateData)
		end;
	    _ ->
		if JID == error ->
		       ?INFO_MSG("(~w) Forbidden legacy authentication "
				 "for username '~s' with resource '~s'",
				 [StateData#state.socket, U, R]),
		       Err = jlib:make_error_reply(El, ?ERR_JID_MALFORMED),
		       send_element(StateData, Err),
		       fsm_next_state(wait_for_auth, StateData);
		   true ->
		       ?INFO_MSG("(~w) Forbidden legacy authentication "
				 "for ~s",
				 [StateData#state.socket,
				  jlib:jid_to_string(JID)]),
		       Err = jlib:make_error_reply(El, ?ERR_NOT_ALLOWED),
		       send_element(StateData, Err),
		       fsm_next_state(wait_for_auth, StateData)
		end
	  end;
      _ ->
	  process_unauthenticated_stanza(StateData, El),
	  fsm_next_state(wait_for_auth, StateData)
    end;
wait_for_auth(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_auth({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData), {stop, normal, StateData};
wait_for_auth({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_auth(closed, StateData) ->
    {stop, normal, StateData}.

wait_for_feature_request({xmlstreamelement, El},
			 StateData) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} = El,
    Zlib = StateData#state.zlib,
    TLS = StateData#state.tls,
    TLSEnabled = StateData#state.tls_enabled,
    TLSRequired = StateData#state.tls_required,
    SockMod =
	(StateData#state.sockmod):get_sockmod(StateData#state.socket),
    case {xml:get_attr_s(<<"xmlns">>, Attrs), Name} of
      {?NS_SASL, <<"auth">>}
	  when not ((SockMod == gen_tcp) and TLSRequired) ->
	  Mech = xml:get_attr_s(<<"mechanism">>, Attrs),
	  ClientIn = jlib:decode_base64(xml:get_cdata(Els)),
	  case cyrsasl:server_start(StateData#state.sasl_state,
				    Mech, ClientIn)
	      of
	    {ok, Props} ->
		(StateData#state.sockmod):reset_stream(StateData#state.socket),
		%U = xml:get_attr_s(username, Props),
		U = proplists:get_value(username, Props, <<>>),
		%AuthModule = xml:get_attr_s(auth_module, Props),
		AuthModule = proplists:get_value(auth_module, Props, undefined),
		?INFO_MSG("(~w) Accepted authentication for ~s "
			  "by ~p",
			  [StateData#state.socket, U, AuthModule]),
		send_element(StateData,
			    #xmlel{name = <<"success">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children = []}),
		fsm_next_state(wait_for_stream,
				StateData#state{streamid = new_id(),
						authenticated = true,
						auth_module = AuthModule,
                                                sasl_state = undefined,
						user = U});
	    {continue, ServerOut, NewSASLState} ->
		send_element(StateData,
			     #xmlel{name = <<"challenge">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children =
					[{xmlcdata,
					  jlib:encode_base64(ServerOut)}]}),
		fsm_next_state(wait_for_sasl_response,
			       StateData#state{sasl_state = NewSASLState});
	    {error, Error, Username} ->
		IP = peerip(StateData#state.sockmod, StateData#state.socket),
		?INFO_MSG("(~w) Failed authentication for ~s@~s from IP ~s",
		       [StateData#state.socket,
			Username, StateData#state.server, jlib:ip_to_list(IP)]),
		send_element(StateData,
			     #xmlel{name = <<"failure">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children =
					[#xmlel{name = Error, attrs = [],
						children = []}]}),
		{next_state, wait_for_feature_request, StateData,
		 ?C2S_OPEN_TIMEOUT};
	    {error, Error} ->
		send_element(StateData,
			     #xmlel{name = <<"failure">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children =
					[#xmlel{name = Error, attrs = [],
						children = []}]}),
		fsm_next_state(wait_for_feature_request, StateData)
	  end;
      {?NS_TLS, <<"starttls">>}
	  when TLS == true, TLSEnabled == false,
	       SockMod == gen_tcp ->
	  TLSOpts = case
		      ejabberd_config:get_option(
                        {domain_certfile, StateData#state.server},
                        fun iolist_to_binary/1)
			of
		      undefined -> StateData#state.tls_options;
		      CertFile ->
			  [{certfile, CertFile} | lists:keydelete(certfile, 1,
								  StateData#state.tls_options)]
		    end,
	  Socket = StateData#state.socket,
	  TLSSocket = (StateData#state.sockmod):starttls(Socket,
							 TLSOpts,
							 xml:element_to_binary(#xmlel{name
											  =
											  <<"proceed">>,
										      attrs
											  =
											  [{<<"xmlns">>,
											    ?NS_TLS}],
										      children
											  =
											  []})),
	  fsm_next_state(wait_for_stream,
			 StateData#state{socket = TLSSocket,
					 streamid = new_id(),
					 tls_enabled = true});
      {?NS_COMPRESS, <<"compress">>}
	  when Zlib == true,
	       (SockMod == gen_tcp) or (SockMod == p1_tls) ->
	  case xml:get_subtag(El, <<"method">>) of
	    false ->
		send_element(StateData,
			     #xmlel{name = <<"failure">>,
				    attrs = [{<<"xmlns">>, ?NS_COMPRESS}],
				    children =
					[#xmlel{name = <<"setup-failed">>,
						attrs = [], children = []}]}),
		fsm_next_state(wait_for_feature_request, StateData);
	    Method ->
		case xml:get_tag_cdata(Method) of
		  <<"zlib">> ->
		      Socket = StateData#state.socket,
		      ZlibSocket = (StateData#state.sockmod):compress(Socket,
								      xml:element_to_binary(#xmlel{name
												       =
												       <<"compressed">>,
												   attrs
												       =
												       [{<<"xmlns">>,
													 ?NS_COMPRESS}],
												   children
												       =
												       []})),
		      fsm_next_state(wait_for_stream,
				     StateData#state{socket = ZlibSocket,
						     streamid = new_id()});
		  _ ->
		      send_element(StateData,
				   #xmlel{name = <<"failure">>,
					  attrs = [{<<"xmlns">>, ?NS_COMPRESS}],
					  children =
					      [#xmlel{name =
							  <<"unsupported-method">>,
						      attrs = [],
						      children = []}]}),
		      fsm_next_state(wait_for_feature_request, StateData)
		end
	  end;
      _ ->
	  if (SockMod == gen_tcp) and TLSRequired ->
		 Lang = StateData#state.lang,
		 send_element(StateData,
			      ?POLICY_VIOLATION_ERR(Lang,
						    <<"Use of STARTTLS required">>)),
		 send_trailer(StateData),
		 {stop, normal, StateData};
	     true ->
		 process_unauthenticated_stanza(StateData, El),
		 fsm_next_state(wait_for_feature_request, StateData)
	  end
    end;
wait_for_feature_request(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_feature_request({xmlstreamend, _Name},
			 StateData) ->
    send_trailer(StateData), {stop, normal, StateData};
wait_for_feature_request({xmlstreamerror, _},
			 StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_feature_request(closed, StateData) ->
    {stop, normal, StateData}.

wait_for_sasl_response({xmlstreamelement, El},
		       StateData) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} = El,
    case {xml:get_attr_s(<<"xmlns">>, Attrs), Name} of
      {?NS_SASL, <<"response">>} ->
	  ClientIn = jlib:decode_base64(xml:get_cdata(Els)),
	  case cyrsasl:server_step(StateData#state.sasl_state,
				   ClientIn)
	      of
	    {ok, Props} ->
		catch
		  (StateData#state.sockmod):reset_stream(StateData#state.socket),
%		U = xml:get_attr_s(username, Props),
		U = proplists:get_value(username, Props, <<>>),
%		AuthModule = xml:get_attr_s(auth_module, Props),
		AuthModule = proplists:get_value(auth_module, Props, <<>>),
		?INFO_MSG("(~w) Accepted authentication for ~s "
			  "by ~p",
			  [StateData#state.socket, U, AuthModule]),
		send_element(StateData,
			    #xmlel{name = <<"success">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children = []}),
		fsm_next_state(wait_for_stream,
				StateData#state{streamid = new_id(),
						authenticated = true,
						auth_module = AuthModule,
                                                sasl_state = undefined,
						user = U});
	    {ok, Props, ServerOut} ->
		(StateData#state.sockmod):reset_stream(StateData#state.socket),
%		U = xml:get_attr_s(username, Props),
		U = proplists:get_value(username, Props, <<>>),
%		AuthModule = xml:get_attr_s(auth_module, Props),
		AuthModule = proplists:get_value(auth_module, Props, undefined),
		?INFO_MSG("(~w) Accepted authentication for ~s "
			  "by ~p",
			  [StateData#state.socket, U, AuthModule]),
		send_element(StateData,
			    #xmlel{name = <<"success">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children =
					[{xmlcdata,
					jlib:encode_base64(ServerOut)}]}),
		fsm_next_state(wait_for_stream,
				StateData#state{streamid = new_id(),
						authenticated = true,
						auth_module = AuthModule,
                                                sasl_state = undefined,
						user = U});
	    {continue, ServerOut, NewSASLState} ->
		send_element(StateData,
			     #xmlel{name = <<"challenge">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children =
					[{xmlcdata,
					  jlib:encode_base64(ServerOut)}]}),
		fsm_next_state(wait_for_sasl_response,
			       StateData#state{sasl_state = NewSASLState});
	    {error, Error, Username} ->
		IP = peerip(StateData#state.sockmod, StateData#state.socket),
		?INFO_MSG("(~w) Failed authentication for ~s@~s from IP ~s",
		       [StateData#state.socket,
			Username, StateData#state.server, jlib:ip_to_list(IP)]),
		send_element(StateData,
			     #xmlel{name = <<"failure">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children =
					[#xmlel{name = Error, attrs = [],
						children = []}]}),
		fsm_next_state(wait_for_feature_request, StateData);
	    {error, Error} ->
		send_element(StateData,
			     #xmlel{name = <<"failure">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children =
					[#xmlel{name = Error, attrs = [],
						children = []}]}),
		fsm_next_state(wait_for_feature_request, StateData)
	  end;
      _ ->
	  process_unauthenticated_stanza(StateData, El),
	  fsm_next_state(wait_for_feature_request, StateData)
    end;
wait_for_sasl_response(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_sasl_response({xmlstreamend, _Name},
		       StateData) ->
    send_trailer(StateData), {stop, normal, StateData};
wait_for_sasl_response({xmlstreamerror, _},
		       StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_sasl_response(closed, StateData) ->
    {stop, normal, StateData}.

resource_conflict_action(U, S, R) ->
    OptionRaw = case ejabberd_sm:is_existing_resource(U, S,
						      R)
		    of
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
	  Rnew = iolist_to_binary([randoms:get_string()
                                   | [jlib:integer_to_binary(X)
                                      || X <- tuple_to_list(now())]]),
	  {accept_resource, Rnew}
    end.

wait_for_bind({xmlstreamelement, El}, StateData) ->
    case jlib:iq_query_info(El) of
      #iq{type = set, xmlns = ?NS_BIND, sub_el = SubEl} =
	  IQ ->
	  U = StateData#state.user,
	  R1 = xml:get_path_s(SubEl,
			      [{elem, <<"resource">>}, cdata]),
	  R = case jlib:resourceprep(R1) of
		error -> error;
		<<"">> ->
                      iolist_to_binary([randoms:get_string()
                                        | [jlib:integer_to_binary(X)
                                           || X <- tuple_to_list(now())]]);
		Resource -> Resource
	      end,
	  case R of
	    error ->
		Err = jlib:make_error_reply(El, ?ERR_BAD_REQUEST),
		send_element(StateData, Err),
		fsm_next_state(wait_for_bind, StateData);
	    _ ->
		case resource_conflict_action(U, StateData#state.server,
					      R)
		    of
		  closenew ->
		      Err = jlib:make_error_reply(El,
						  ?STANZA_ERROR(<<"409">>,
								<<"modify">>,
								<<"conflict">>)),
		      send_element(StateData, Err),
		      fsm_next_state(wait_for_bind, StateData);
		  {accept_resource, R2} ->
		      JID = jlib:make_jid(U, StateData#state.server, R2),
		      Res = IQ#iq{type = result,
				  sub_el =
				      [#xmlel{name = <<"bind">>,
					      attrs = [{<<"xmlns">>, ?NS_BIND}],
					      children =
						  [#xmlel{name = <<"jid">>,
							  attrs = [],
							  children =
							      [{xmlcdata,
								jlib:jid_to_string(JID)}]}]}]},
		      send_element(StateData, jlib:iq_to_xml(Res)),
		      fsm_next_state(wait_for_session,
				     StateData#state{resource = R2, jid = JID})
		end
	  end;
      _ -> fsm_next_state(wait_for_bind, StateData)
    end;
wait_for_bind(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_bind({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData), {stop, normal, StateData};
wait_for_bind({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_bind(closed, StateData) ->
    {stop, normal, StateData}.

wait_for_session({xmlstreamelement, El}, StateData) ->
    case jlib:iq_query_info(El) of
	#iq{type = set, xmlns = ?NS_SESSION} ->
	    U = StateData#state.user,
	    R = StateData#state.resource,
	    JID = StateData#state.jid,
	    case acl:match_rule(StateData#state.server,
				StateData#state.access, JID) of
		allow ->
		    ?INFO_MSG("(~w) Opened session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(JID)]),
                    Res = jlib:make_result_iq_reply(El#xmlel{children = []}),
		    send_element(StateData, Res),
		    change_shaper(StateData, JID),
		    {Fs, Ts} = ejabberd_hooks:run_fold(
				 roster_get_subscription_lists,
				 StateData#state.server,
				 {[], []},
				 [U, StateData#state.server]),
		    LJID = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
		    Fs1 = [LJID | Fs],
		    Ts1 = [LJID | Ts],
		    PrivList =
			ejabberd_hooks:run_fold(
			  privacy_get_user_list, StateData#state.server,
			  #userlist{},
			  [U, StateData#state.server]),
		    SID = {now(), self()},
		    Conn = get_conn_type(StateData),
		    Info = [{ip, StateData#state.ip}, {conn, Conn},
			    {auth_module, StateData#state.auth_module}],
		    ejabberd_sm:open_session(
		      SID, U, StateData#state.server, R, Info),
                    NewStateData =
                        StateData#state{
				     sid = SID,
				     conn = Conn,
				     pres_f = ?SETS:from_list(Fs1),
				     pres_t = ?SETS:from_list(Ts1),
				     privacy_list = PrivList},
		    fsm_next_state_pack(session_established,
                                        NewStateData);
		_ ->
		    ejabberd_hooks:run(forbidden_session_hook,
				       StateData#state.server, [JID]),
		    ?INFO_MSG("(~w) Forbidden session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(JID)]),
		    Err = jlib:make_error_reply(El, ?ERR_NOT_ALLOWED),
		    send_element(StateData, Err),
		    fsm_next_state(wait_for_session, StateData)
	    end;
	_ ->
	    fsm_next_state(wait_for_session, StateData)
    end;

wait_for_session(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_session({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData), {stop, normal, StateData};
wait_for_session({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_session(closed, StateData) ->
    {stop, normal, StateData}.

session_established({xmlstreamelement, El},
		    StateData) ->
    FromJID = StateData#state.jid,
    case check_from(El, FromJID) of
	'invalid-from' ->
	    send_element(StateData, ?INVALID_FROM),
	    send_trailer(StateData),
	    {stop, normal, StateData};
	_NewEl ->
	    session_established2(El, StateData)
    end;
%% We hibernate the process to reduce memory consumption after a
%% configurable activity timeout
session_established(timeout, StateData) ->
    Options = [],
    proc_lib:hibernate(?GEN_FSM, enter_loop,
		       [?MODULE, Options, session_established, StateData]),
    fsm_next_state(session_established, StateData);
session_established({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData), {stop, normal, StateData};
session_established({xmlstreamerror,
		     <<"XML stanza is too big">> = E},
		    StateData) ->
    send_element(StateData,
		 ?POLICY_VIOLATION_ERR((StateData#state.lang), E)),
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
    #xmlel{name = Name, attrs = Attrs} = El,
    User = StateData#state.user,
    Server = StateData#state.server,
    FromJID = StateData#state.jid,
    To = xml:get_attr_s(<<"to">>, Attrs),
    ToJID = case To of
	      <<"">> -> jlib:make_jid(User, Server, <<"">>);
	      _ -> jlib:string_to_jid(To)
	    end,
    NewEl1 = jlib:remove_attr(<<"xmlns">>, El),
    NewEl = case xml:get_attr_s(<<"xml:lang">>, Attrs) of
	      <<"">> ->
		  case StateData#state.lang of
		    <<"">> -> NewEl1;
		    Lang ->
			xml:replace_tag_attr(<<"xml:lang">>, Lang, NewEl1)
		  end;
	      _ -> NewEl1
	    end,
    NewState = case ToJID of
		 error ->
		     case xml:get_attr_s(<<"type">>, Attrs) of
		       <<"error">> -> StateData;
		       <<"result">> -> StateData;
		       _ ->
			   Err = jlib:make_error_reply(NewEl,
						       ?ERR_JID_MALFORMED),
			   send_element(StateData, Err),
			   StateData
		     end;
		 _ ->
		     case Name of
		       <<"presence">> ->
			   PresenceEl =
			       ejabberd_hooks:run_fold(c2s_update_presence,
						       Server, NewEl,
						       [User, Server]),
			   ejabberd_hooks:run(user_send_packet, Server,
					      [FromJID, ToJID, PresenceEl]),
			   case ToJID of
			     #jid{user = User, server = Server,
				  resource = <<"">>} ->
				 ?DEBUG("presence_update(~p,~n\t~p,~n\t~p)",
					[FromJID, PresenceEl, StateData]),
				 presence_update(FromJID, PresenceEl,
						 StateData);
			     _ ->
				 presence_track(FromJID, ToJID, PresenceEl,
						StateData)
			   end;
		       <<"iq">> ->
			   case jlib:iq_query_info(NewEl) of
			     #iq{xmlns = Xmlns} = IQ
				 when Xmlns == (?NS_PRIVACY);
				      Xmlns == (?NS_BLOCKING) ->
				 process_privacy_iq(FromJID, ToJID, IQ,
						    StateData);
			     _ ->
				 ejabberd_hooks:run(user_send_packet, Server,
						    [FromJID, ToJID, NewEl]),
				 check_privacy_route(FromJID, StateData,
						     FromJID, ToJID, NewEl),
				 StateData
			   end;
		       <<"message">> ->
			   ejabberd_hooks:run(user_send_packet, Server,
					      [FromJID, ToJID, NewEl]),
			   check_privacy_route(FromJID, StateData, FromJID,
					       ToJID, NewEl),
			   StateData;
		       _ -> StateData
		     end
	       end,
    ejabberd_hooks:run(c2s_loop_debug,
		       [{xmlstreamelement, El}]),
    fsm_next_state(session_established, NewState).

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
handle_sync_event(_Event, _From, StateName,
		  StateData) ->
    Reply = ok, fsm_reply(Reply, StateName, StateData).

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
    ejabberd_hooks:run(c2s_loop_debug, [Text]),
    fsm_next_state(StateName, StateData);
handle_info(replaced, StateName, StateData) ->
    Lang = StateData#state.lang,
    Xmlelement = ?SERRT_CONFLICT(Lang, <<"Replaced by new connection">>),
    handle_info({kick, replaced, Xmlelement}, StateName, StateData);
handle_info({kick, Reason, Xmlelement}, _StateName, StateData) ->
    send_element(StateData, Xmlelement),
    send_trailer(StateData),
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
            send_element(StateData, ?SERRT_CONFLICT(Lang, Reason)),
            catch send_trailer(StateData),
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
                    PrivPushIQ = #iq{type = set,
                                     xmlns = ?NS_PRIVACY,
                                     id = <<"push",
                                            (randoms:get_string())/binary>>,
                                     sub_el =
                                         [#xmlel{name = <<"query">>,
                                                 attrs = [{<<"xmlns">>,
                                                           ?NS_PRIVACY}],
                                                 children =
                                                     [#xmlel{name = <<"list">>,
                                                             attrs = [{<<"name">>,
                                                                       PrivListName}],
                                                             children = []}]}]},
                    PrivPushEl = jlib:replace_from_to(
                                   jlib:jid_remove_resource(StateData#state.jid),
                                   StateData#state.jid,
                                   jlib:iq_to_xml(PrivPushIQ)),
                    send_element(StateData, PrivPushEl),
                    fsm_next_state(StateName,
                                   StateData#state{privacy_list = NewPL})
            end;
        {blocking, What} ->
            route_blocking(What, StateData),
            fsm_next_state(StateName, StateData);
        _ ->
            fsm_next_state(StateName, StateData)
    end;
%% Process Packets that are to be send to the user
handle_info({route, From, To,
             #xmlel{name = Name, attrs = Attrs, children = Els} = Packet},
            StateName, StateData) ->
    {Pass, NewAttrs, NewState} = case Name of
				   <<"presence">> ->
				       State =
					   ejabberd_hooks:run_fold(c2s_presence_in,
								   StateData#state.server,
								   StateData,
								   [{From, To,
								     Packet}]),
				       case xml:get_attr_s(<<"type">>, Attrs) of
					 <<"probe">> ->
					     LFrom = jlib:jid_tolower(From),
					     LBFrom =
						 jlib:jid_remove_resource(LFrom),
					     NewStateData = case
							      (?SETS):is_element(LFrom,
										 State#state.pres_a)
								orelse
								(?SETS):is_element(LBFrom,
										   State#state.pres_a)
								of
							      true -> State;
							      false ->
								  case
								    (?SETS):is_element(LFrom,
										       State#state.pres_f)
								      of
								    true ->
									A =
									    (?SETS):add_element(LFrom,
												State#state.pres_a),
									State#state{pres_a
											=
											A};
								    false ->
									case
									  (?SETS):is_element(LBFrom,
											     State#state.pres_f)
									    of
									  true ->
									      A =
										  (?SETS):add_element(LBFrom,
												      State#state.pres_a),
									      State#state{pres_a
											      =
											      A};
									  false ->
									      State
									end
								  end
							    end,
					     process_presence_probe(From, To,
								    NewStateData),
					     {false, Attrs, NewStateData};
					 <<"error">> ->
					     NewA =
						 remove_element(jlib:jid_tolower(From),
								State#state.pres_a),
					     {true, Attrs,
					      State#state{pres_a = NewA}};
					 <<"subscribe">> ->
					     SRes = is_privacy_allow(State,
								     From, To,
								     Packet,
								     in),
					     {SRes, Attrs, State};
					 <<"subscribed">> ->
					     SRes = is_privacy_allow(State,
								     From, To,
								     Packet,
								     in),
					     {SRes, Attrs, State};
					 <<"unsubscribe">> ->
					     SRes = is_privacy_allow(State,
								     From, To,
								     Packet,
								     in),
					     {SRes, Attrs, State};
					 <<"unsubscribed">> ->
					     SRes = is_privacy_allow(State,
								     From, To,
								     Packet,
								     in),
					     {SRes, Attrs, State};
					 _ ->
					     case privacy_check_packet(State,
								       From, To,
								       Packet,
								       in)
						 of
					       allow ->
						   LFrom =
						       jlib:jid_tolower(From),
						   LBFrom =
						       jlib:jid_remove_resource(LFrom),
						   case
						     (?SETS):is_element(LFrom,
									State#state.pres_a)
						       orelse
						       (?SETS):is_element(LBFrom,
									  State#state.pres_a)
						       of
						     true ->
							 {true, Attrs, State};
						     false ->
							 case
							   (?SETS):is_element(LFrom,
									      State#state.pres_f)
							     of
							   true ->
							       A =
								   (?SETS):add_element(LFrom,
										       State#state.pres_a),
							       {true, Attrs,
								State#state{pres_a
										=
										A}};
							   false ->
							       case
								 (?SETS):is_element(LBFrom,
										    State#state.pres_f)
								   of
								 true ->
								     A =
									 (?SETS):add_element(LBFrom,
											     State#state.pres_a),
								     {true,
								      Attrs,
								      State#state{pres_a
										      =
										      A}};
								 false ->
								     {true,
								      Attrs,
								      State}
							       end
							 end
						   end;
					       deny -> {false, Attrs, State}
					     end
				       end;
				   <<"iq">> ->
				       IQ = jlib:iq_query_info(Packet),
				       case IQ of
					 #iq{xmlns = ?NS_LAST} ->
					     LFrom = jlib:jid_tolower(From),
					     LBFrom =
						 jlib:jid_remove_resource(LFrom),
					     HasFromSub =
						 ((?SETS):is_element(LFrom,
								     StateData#state.pres_f)
						    orelse
						    (?SETS):is_element(LBFrom,
								       StateData#state.pres_f))
						   andalso
						   is_privacy_allow(StateData,
								    To, From,
								    #xmlel{name
									       =
									       <<"presence">>,
									   attrs
									       =
									       [],
									   children
									       =
									       []},
								    out),
					     case HasFromSub of
					       true ->
						   case
						     privacy_check_packet(StateData,
									  From,
									  To,
									  Packet,
									  in)
						       of
						     allow ->
							 {true, Attrs,
							  StateData};
						     deny ->
							 {false, Attrs,
							  StateData}
						   end;
					       _ ->
						   Err =
						       jlib:make_error_reply(Packet,
									     ?ERR_FORBIDDEN),
						   ejabberd_router:route(To,
									 From,
									 Err),
						   {false, Attrs, StateData}
					     end;
					 IQ
					     when is_record(IQ, iq) or
						    (IQ == reply) ->
					     case
					       privacy_check_packet(StateData,
								    From, To,
								    Packet, in)
						 of
					       allow ->
						   {true, Attrs, StateData};
					       deny when is_record(IQ, iq) ->
						   Err =
						       jlib:make_error_reply(Packet,
									     ?ERR_SERVICE_UNAVAILABLE),
						   ejabberd_router:route(To,
									 From,
									 Err),
						   {false, Attrs, StateData};
					       deny when IQ == reply ->
						   {false, Attrs, StateData}
					     end;
					 IQ
					     when (IQ == invalid) or
						    (IQ == not_iq) ->
					     {false, Attrs, StateData}
				       end;
				   <<"message">> ->
				       case privacy_check_packet(StateData,
								 From, To,
								 Packet, in)
					   of
					 allow -> {true, Attrs, StateData};
					 deny -> {false, Attrs, StateData}
				       end;
				   _ -> {true, Attrs, StateData}
				 end,
    if Pass == exit ->
	    %% When Pass==exit, NewState contains a string instead of a #state{}
	    Lang = StateData#state.lang,
	    send_element(StateData, ?SERRT_CONFLICT(Lang, NewState)),
	    send_trailer(StateData),
	    {stop, normal, StateData};
	Pass ->
	    Attrs2 =
	       jlib:replace_from_to_attrs(jlib:jid_to_string(From),
					  jlib:jid_to_string(To), NewAttrs),
	    FixedPacket = #xmlel{name = Name, attrs = Attrs2, children = Els},
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
handle_info({'DOWN', Monitor, _Type, _Object, _Info},
	    _StateName, StateData)
    when Monitor == StateData#state.socket_monitor ->
    {stop, normal, StateData};
handle_info(system_shutdown, StateName, StateData) ->
    case StateName of
      wait_for_stream ->
	  send_header(StateData, ?MYNAME, <<"1.0">>, <<"en">>),
	  send_element(StateData, ?SERR_SYSTEM_SHUTDOWN),
	  send_trailer(StateData),
	  ok;
      _ ->
	  send_element(StateData, ?SERR_SYSTEM_SHUTDOWN),
	  send_trailer(StateData),
	  ok
    end,
    {stop, normal, StateData};
handle_info({force_update_presence, LUser}, StateName,
	    #state{user = LUser, server = LServer} = StateData) ->
    NewStateData = case StateData#state.pres_last of
		     #xmlel{name = <<"presence">>} ->
			 PresenceEl =
			     ejabberd_hooks:run_fold(c2s_update_presence,
						     LServer,
						     StateData#state.pres_last,
						     [LUser, LServer]),
			 StateData2 = StateData#state{pres_last = PresenceEl},
			 presence_update(StateData2#state.jid, PresenceEl,
					 StateData2),
			 StateData2;
		     _ -> StateData
		   end,
    {next_state, StateName, NewStateData};
handle_info({broadcast, Type, From, Packet}, StateName, StateData) ->
    Recipients = ejabberd_hooks:run_fold(
		   c2s_broadcast_recipients, StateData#state.server,
		   [],
		   [StateData, Type, From, Packet]),
    lists:foreach(
      fun(USR) ->
	      ejabberd_router:route(
		From, jlib:make_jid(USR), Packet)
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
terminate(_Reason, StateName, StateData) ->
    case StateName of
      session_established ->
	  case StateData#state.authenticated of
	    replaced ->
		?INFO_MSG("(~w) Replaced session for ~s",
			  [StateData#state.socket,
			   jlib:jid_to_string(StateData#state.jid)]),
		From = StateData#state.jid,
		Packet = #xmlel{name = <<"presence">>,
				attrs = [{<<"type">>, <<"unavailable">>}],
				children =
				    [#xmlel{name = <<"status">>, attrs = [],
					    children =
						[{xmlcdata,
						  <<"Replaced by new connection">>}]}]},
		ejabberd_sm:close_session_unset_presence(StateData#state.sid,
							 StateData#state.user,
							 StateData#state.server,
							 StateData#state.resource,
							 <<"Replaced by new connection">>),
		presence_broadcast(StateData, From,
				   StateData#state.pres_a, Packet),
		presence_broadcast(StateData, From,
				   StateData#state.pres_i, Packet);
	    _ ->
		?INFO_MSG("(~w) Close session for ~s",
			  [StateData#state.socket,
			   jlib:jid_to_string(StateData#state.jid)]),
		EmptySet = (?SETS):new(),
		case StateData of
		  #state{pres_last = undefined, pres_a = EmptySet, pres_i = EmptySet, pres_invis = false} ->
		      ejabberd_sm:close_session(StateData#state.sid,
						StateData#state.user,
						StateData#state.server,
						StateData#state.resource);
		  _ ->
		      From = StateData#state.jid,
		      Packet = #xmlel{name = <<"presence">>,
				      attrs = [{<<"type">>, <<"unavailable">>}],
				      children = []},
		      ejabberd_sm:close_session_unset_presence(StateData#state.sid,
							       StateData#state.user,
							       StateData#state.server,
							       StateData#state.resource,
							       <<"">>),
		      presence_broadcast(StateData, From,
					 StateData#state.pres_a, Packet),
		      presence_broadcast(StateData, From,
					 StateData#state.pres_i, Packet)
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
    Shaper = acl:match_rule(StateData#state.server,
			    StateData#state.shaper, JID),
    (StateData#state.sockmod):change_shaper(StateData#state.socket,
					    Shaper).

send_text(StateData, Text) when StateData#state.xml_socket ->
    ?DEBUG("Send Text on stream = ~p", [Text]),
    (StateData#state.sockmod):send_xml(StateData#state.socket, 
				       {xmlstreamraw, Text});
send_text(StateData, Text) ->
    ?DEBUG("Send XML on stream = ~p", [Text]),
    (StateData#state.sockmod):send(StateData#state.socket, Text).

send_element(StateData, El) when StateData#state.xml_socket ->
    (StateData#state.sockmod):send_xml(StateData#state.socket,
				       {xmlstreamelement, El});
send_element(StateData, El) ->
    send_text(StateData, xml:element_to_binary(El)).

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

send_trailer(StateData)
    when StateData#state.xml_socket ->
    (StateData#state.sockmod):send_xml(StateData#state.socket,
				       {xmlstreamend, <<"stream:stream">>});
send_trailer(StateData) ->
    send_text(StateData, ?STREAM_TRAILER).

new_id() -> randoms:get_string().

is_auth_packet(El) ->
    case jlib:iq_query_info(El) of
      #iq{id = ID, type = Type, xmlns = ?NS_AUTH,
	  sub_el = SubEl} ->
	  #xmlel{children = Els} = SubEl,
	  {auth, ID, Type,
	   get_auth_tags(Els, <<"">>, <<"">>, <<"">>, <<"">>)};
      _ -> false
    end.

get_auth_tags([#xmlel{name = Name, children = Els} | L],
	      U, P, D, R) ->
    CData = xml:get_cdata(Els),
    case Name of
      <<"username">> -> get_auth_tags(L, CData, P, D, R);
      <<"password">> -> get_auth_tags(L, U, CData, D, R);
      <<"digest">> -> get_auth_tags(L, U, P, CData, R);
      <<"resource">> -> get_auth_tags(L, U, P, D, CData);
      _ -> get_auth_tags(L, U, P, D, R)
    end;
get_auth_tags([_ | L], U, P, D, R) ->
    get_auth_tags(L, U, P, D, R);
get_auth_tags([], U, P, D, R) ->
    {U, P, D, R}.

%% Copied from ejabberd_socket.erl
-record(socket_state, {sockmod, socket, receiver}).

get_conn_type(StateData) ->
    case (StateData#state.sockmod):get_sockmod(StateData#state.socket) of
    gen_tcp -> c2s;
    p1_tls -> c2s_tls;
    ezlib ->
	case ezlib:get_sockmod((StateData#state.socket)#socket_state.socket) of
	    gen_tcp -> c2s_compressed;
	    p1_tls -> c2s_compressed_tls
	end;
    ejabberd_http_poll -> http_poll;
    ejabberd_http_bind -> http_bind;
    _ -> unknown
    end.

process_presence_probe(From, To, StateData) ->
    LFrom = jlib:jid_tolower(From),
    LBFrom = setelement(3, LFrom, <<"">>),
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
		    Timestamp = StateData#state.pres_timestamp,
		    Packet = xml:append_subtags(
			       StateData#state.pres_last,
			       %% To is the one sending the presence (the target of the probe)
			       [jlib:timestamp_to_xml(Timestamp, utc, To, <<"">>),
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
		Cond2 ->
		    ejabberd_router:route(To, From,
			#xmlel{name = <<"presence">>,
			       attrs = [],
			       children = []});
		true ->
		    ok
	    end
    end.

%% User updates his presence (non-directed presence packet)
presence_update(From, Packet, StateData) ->
    #xmlel{attrs = Attrs} = Packet,
    case xml:get_attr_s(<<"type">>, Attrs) of
      <<"unavailable">> ->
	  Status = case xml:get_subtag(Packet, <<"status">>) of
		     false -> <<"">>;
		     StatusTag -> xml:get_tag_cdata(StatusTag)
		   end,
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
      <<"error">> -> StateData;
      <<"probe">> -> StateData;
      <<"subscribe">> -> StateData;
      <<"subscribed">> -> StateData;
      <<"unsubscribe">> -> StateData;
      <<"unsubscribed">> -> StateData;
      _ ->
	  OldPriority = case StateData#state.pres_last of
			  undefined -> 0;
			  OldPresence -> get_priority_from_presence(OldPresence)
			end,
	  NewPriority = get_priority_from_presence(Packet),
	  Timestamp = calendar:now_to_universal_time(now()),
	  update_priority(NewPriority, Packet, StateData),
	  FromUnavail = (StateData#state.pres_last == undefined),
	  ?DEBUG("from unavail = ~p~n", [FromUnavail]),
	  NewStateData = StateData#state{pres_last = Packet,
					 pres_timestamp = Timestamp},
	  NewState = if FromUnavail ->
			    ejabberd_hooks:run(user_available_hook,
					       NewStateData#state.server,
					       [NewStateData#state.jid]),
			    if NewPriority >= 0 ->
				   resend_offline_messages(NewStateData),
				   resend_subscription_requests(NewStateData);
			       true -> ok
			    end,
			    presence_broadcast_first(From, NewStateData,
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
    #xmlel{attrs = Attrs} = Packet,
    LTo = jlib:jid_tolower(To),
    User = StateData#state.user,
    Server = StateData#state.server,
    case xml:get_attr_s(<<"type">>, Attrs) of
      <<"unavailable">> ->
	  check_privacy_route(From, StateData, From, To, Packet),
	  A = remove_element(LTo, StateData#state.pres_a),
	  StateData#state{pres_a = A};
      <<"subscribe">> ->
	  try_roster_subscribe(subscribe, User, Server, From, To, Packet, StateData),
	  StateData;
      <<"subscribed">> ->
	  ejabberd_hooks:run(roster_out_subscription, Server,
			     [User, Server, To, subscribed]),
	  check_privacy_route(From, StateData,
			      jlib:jid_remove_resource(From), To, Packet),
	  StateData;
      <<"unsubscribe">> ->
	  try_roster_subscribe(unsubscribe, User, Server, From, To, Packet, StateData),
	  StateData;
      <<"unsubscribed">> ->
	  ejabberd_hooks:run(roster_out_subscription, Server,
			     [User, Server, To, unsubscribed]),
	  check_privacy_route(From, StateData,
			      jlib:jid_remove_resource(From), To, Packet),
	  StateData;
      <<"error">> ->
	  check_privacy_route(From, StateData, From, To, Packet),
	  StateData;
      <<"probe">> ->
	  check_privacy_route(From, StateData, From, To, Packet),
	  StateData;
      _ ->
	  check_privacy_route(From, StateData, From, To, Packet),
	  A = (?SETS):add_element(LTo, StateData#state.pres_a),
	  StateData#state{pres_a = A}
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
	  Err = jlib:make_error_reply(Packet,
				      ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
	  ejabberd_router:route(To, From, Err),
	  ok;
      allow -> ejabberd_router:route(FromRoute, To, Packet)
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
    JID1 = jlib:make_jid(User, Server, <<"">>),
    Access = gen_mod:get_module_opt(Server, mod_roster, access, fun(A) when is_atom(A) -> A end, all),
    case acl:match_rule(Server, Access, JID1) of
	deny ->
	    %% Silently drop this (un)subscription request
	    ok;
	allow ->
	    ejabberd_hooks:run(roster_out_subscription,
			       Server,
			       [User, Server, To, Type]),
	    check_privacy_route(From, StateData, jlib:jid_remove_resource(From),
				To, Packet)
    end.

%% Send presence when disconnecting
presence_broadcast(StateData, From, JIDSet, Packet) ->
    JIDs = ?SETS:to_list(JIDSet),
    JIDs2 = format_and_check_privacy(From, StateData, Packet, JIDs, out),
    send_multiple(StateData, From, JIDs2, Packet).

%% Send presence when updating presence
presence_broadcast_to_trusted(StateData, From, Trusted, JIDSet, Packet) ->
    JIDs = ?SETS:to_list(JIDSet),
    JIDs_trusted = [JID || JID <- JIDs, ?SETS:is_element(JID, Trusted)],
    JIDs2 = format_and_check_privacy(From, StateData, Packet, JIDs_trusted, out),
    send_multiple(StateData, From, JIDs2, Packet).

%% Send presence when connecting
presence_broadcast_first(From, StateData, Packet) ->
    JIDsProbe =
	?SETS:fold(
	   fun(JID, L) -> [JID | L] end,
	   [],
	   StateData#state.pres_t),
    PacketProbe = #xmlel{name = <<"presence">>, attrs = [{<<"type">>,<<"probe">>}], children = []},
    JIDs2Probe = format_and_check_privacy(From, StateData, Packet, JIDsProbe, out),
    Server = StateData#state.server,
    send_multiple(StateData, From, JIDs2Probe, PacketProbe),
    {As, JIDs} =
	?SETS:fold(
	   fun(JID, {A, JID_list}) ->
		   {?SETS:add_element(JID, A), JID_list++[JID]}
	   end,
	   {StateData#state.pres_a, []},
	   StateData#state.pres_f),
    JIDs2 = format_and_check_privacy(From, StateData, Packet, JIDs, out),
    Server = StateData#state.server,
    send_multiple(StateData, From, JIDs2, Packet),
    StateData#state{pres_a = As}.

format_and_check_privacy(From, StateData, Packet, JIDs, Dir) ->
    FJIDs = [jlib:make_jid(JID) || JID <- JIDs],
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

send_multiple(StateData, From, JIDs, Packet) ->
    lists:foreach(
      fun(JID) ->
              case privacy_check_packet(StateData, From, JID, Packet, out) of
                  deny ->
                      ok;
                  allow ->
                      ejabberd_router:route(From, JID, Packet)
              end
      end, JIDs).

remove_element(E, Set) ->
    case (?SETS):is_element(E, Set) of
      true -> (?SETS):del_element(E, Set);
      _ -> Set
    end.

roster_change(IJID, ISubscription, StateData) ->
    LIJID = jlib:jid_tolower(IJID),
    IsFrom = (ISubscription == both) or
	       (ISubscription == from),
    IsTo = (ISubscription == both) or (ISubscription == to),
    OldIsFrom = (?SETS):is_element(LIJID,
				   StateData#state.pres_f),
    FSet = if IsFrom ->
		  (?SETS):add_element(LIJID, StateData#state.pres_f);
	      true -> remove_element(LIJID, StateData#state.pres_f)
	   end,
    TSet = if IsTo ->
		  (?SETS):add_element(LIJID, StateData#state.pres_t);
	      true -> remove_element(LIJID, StateData#state.pres_t)
	   end,
    case StateData#state.pres_last of
      undefined ->
	  StateData#state{pres_f = FSet, pres_t = TSet};
      P ->
	  ?DEBUG("roster changed for ~p~n",
		 [StateData#state.user]),
	  From = StateData#state.jid,
	  To = jlib:make_jid(IJID),
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
		 PU = #xmlel{name = <<"presence">>,
			     attrs = [{<<"type">>, <<"unavailable">>}],
			     children = []},
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

get_priority_from_presence(PresencePacket) ->
    case xml:get_subtag(PresencePacket, <<"priority">>) of
      false -> 0;
      SubEl ->
	  case catch
		 jlib:binary_to_integer(xml:get_tag_cdata(SubEl))
	      of
	    P when is_integer(P) -> P;
	    _ -> 0
	  end
    end.

process_privacy_iq(From, To,
		   #iq{type = Type, sub_el = SubEl} = IQ, StateData) ->
    {Res, NewStateData} = case Type of
			    get ->
				R = ejabberd_hooks:run_fold(privacy_iq_get,
							    StateData#state.server,
							    {error,
							     ?ERR_FEATURE_NOT_IMPLEMENTED},
							    [From, To, IQ,
							     StateData#state.privacy_list]),
				{R, StateData};
			    set ->
				case ejabberd_hooks:run_fold(privacy_iq_set,
							     StateData#state.server,
							     {error,
							      ?ERR_FEATURE_NOT_IMPLEMENTED},
							     [From, To, IQ])
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
		  IQ#iq{type = result, sub_el = Result};
	      {error, Error} ->
		  IQ#iq{type = error, sub_el = [SubEl, Error]}
	    end,
    ejabberd_router:route(To, From, jlib:iq_to_xml(IQRes)),
    NewStateData.

resend_offline_messages(StateData) ->
    case
      ejabberd_hooks:run_fold(resend_offline_messages_hook,
			      StateData#state.server, [],
			      [StateData#state.user, StateData#state.server])
	of
      Rs -> %%when is_list(Rs) ->
	  lists:foreach(fun ({route, From, To,
			      #xmlel{} = Packet}) ->
				Pass = case privacy_check_packet(StateData,
								 From, To,
								 Packet, in)
					   of
					 allow -> true;
					 deny -> false
				       end,
				if Pass ->
				       ejabberd_router:route(From, To, Packet);
				   %% send_element(StateData, FixedPacket),
				   %% ejabberd_hooks:run(user_receive_packet,
				   %%			 StateData#state.server,
				   %%			 [StateData#state.jid,
				   %%			  From, To, FixedPacket]);
				   true -> ok
				end
			end,
			Rs)
    end.

resend_subscription_requests(#state{user = User,
				    server = Server} =
				 StateData) ->
    PendingSubscriptions =
	ejabberd_hooks:run_fold(resend_subscription_requests_hook,
				Server, [], [User, Server]),
    lists:foreach(fun (XMLPacket) ->
			  send_element(StateData, XMLPacket)
		  end,
		  PendingSubscriptions).

get_showtag(undefined) -> <<"unavailable">>;
get_showtag(Presence) ->
    case xml:get_path_s(Presence,
			[{elem, <<"show">>}, cdata])
	of
      <<"">> -> <<"available">>;
      ShowTag -> ShowTag
    end.

get_statustag(undefined) -> <<"">>;
get_statustag(Presence) ->
    case xml:get_path_s(Presence,
			[{elem, <<"status">>}, cdata])
	of
      ShowTag -> ShowTag
    end.

process_unauthenticated_stanza(StateData, El) ->
    NewEl = case xml:get_tag_attr_s(<<"xml:lang">>, El) of
	      <<"">> ->
		  case StateData#state.lang of
		    <<"">> -> El;
		    Lang -> xml:replace_tag_attr(<<"xml:lang">>, Lang, El)
		  end;
	      _ -> El
	    end,
    case jlib:iq_query_info(NewEl) of
      #iq{} = IQ ->
	  Res = ejabberd_hooks:run_fold(c2s_unauthenticated_iq,
					StateData#state.server, empty,
					[StateData#state.server, IQ,
					 StateData#state.ip]),
	  case Res of
	    empty ->
		ResIQ = IQ#iq{type = error,
			      sub_el = [?ERR_SERVICE_UNAVAILABLE]},
		Res1 = jlib:replace_from_to(jlib:make_jid(<<"">>,
							  StateData#state.server,
							  <<"">>),
					    jlib:make_jid(<<"">>, <<"">>,
							  <<"">>),
					    jlib:iq_to_xml(ResIQ)),
		send_element(StateData,
			     jlib:remove_attr(<<"to">>, Res1));
	    _ -> send_element(StateData, Res)
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
    {next_state, session_established, StateData,
     ?C2S_HIBERNATE_TIMEOUT};
fsm_next_state(StateName, StateData) ->
    {next_state, StateName, StateData, ?C2S_OPEN_TIMEOUT}.

%% fsm_reply: Generate the reply FSM tuple with different timeout,
%% depending on the future state
fsm_reply(Reply, session_established, StateData) ->
    {reply, Reply, session_established, StateData,
     ?C2S_HIBERNATE_TIMEOUT};
fsm_reply(Reply, StateName, StateData) ->
    {reply, Reply, StateName, StateData, ?C2S_OPEN_TIMEOUT}.

%% Used by c2s blacklist plugins
is_ip_blacklisted(undefined) -> false;
is_ip_blacklisted({IP, _Port}) ->
    ejabberd_hooks:run_fold(check_bl_c2s, false, [IP]).

%% Check from attributes
%% returns invalid-from|NewElement
check_from(El, FromJID) ->
    case xml:get_tag_attr(<<"from">>, El) of
      false -> El;
      {value, SJID} ->
	  JID = jlib:string_to_jid(SJID),
	  case JID of
	    error -> 'invalid-from';
	    #jid{} ->
		if (JID#jid.luser == FromJID#jid.luser) and
		     (JID#jid.lserver == FromJID#jid.lserver)
		     and (JID#jid.lresource == FromJID#jid.lresource) ->
		       El;
		   (JID#jid.luser == FromJID#jid.luser) and
		     (JID#jid.lserver == FromJID#jid.lserver)
		     and (JID#jid.lresource == <<"">>) ->
		       El;
		   true -> 'invalid-from'
		end
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

%%%----------------------------------------------------------------------
%%% XEP-0191
%%%----------------------------------------------------------------------

route_blocking(What, StateData) ->
    SubEl = case What of
	      {block, JIDs} ->
		  #xmlel{name = <<"block">>,
			 attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
			 children =
			     lists:map(fun (JID) ->
					       #xmlel{name = <<"item">>,
						      attrs =
							  [{<<"jid">>,
							    jlib:jid_to_string(JID)}],
						      children = []}
				       end,
				       JIDs)};
	      {unblock, JIDs} ->
		  #xmlel{name = <<"unblock">>,
			 attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
			 children =
			     lists:map(fun (JID) ->
					       #xmlel{name = <<"item">>,
						      attrs =
							  [{<<"jid">>,
							    jlib:jid_to_string(JID)}],
						      children = []}
				       end,
				       JIDs)};
	      unblock_all ->
		  #xmlel{name = <<"unblock">>,
			 attrs = [{<<"xmlns">>, ?NS_BLOCKING}], children = []}
	    end,
    PrivPushIQ = #iq{type = set, xmlns = ?NS_BLOCKING,
		     id = <<"push">>, sub_el = [SubEl]},
    PrivPushEl =
	jlib:replace_from_to(jlib:jid_remove_resource(StateData#state.jid),
			     StateData#state.jid, jlib:iq_to_xml(PrivPushIQ)),
    send_element(StateData, PrivPushEl),
    %% No need to replace active privacy list here,
    %% blocking pushes are always accompanied by
    %% Privacy List pushes
    ok.

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
