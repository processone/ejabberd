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
-export([start/2, stop_or_detach/1, start_link/3, send_text/2,
	 send_element/2, socket_type/0, get_presence/1,
	 get_aux_field/2, set_aux_field/3, del_aux_field/2,
	 get_subscription/2, broadcast/4, get_subscribed/1]).

%% API:
-export([add_rosteritem/3, del_rosteritem/2]).

%% gen_fsm callbacks
-export([init/1, wait_for_stream/2, wait_for_stream/3,
	 wait_for_auth/2, wait_for_auth/3,
	 wait_for_feature_request/2, wait_for_feature_request/3,
	 wait_for_bind/2, wait_for_bind/3,
	 wait_for_session/2, wait_for_session/3,
	 wait_for_sasl_response/2, wait_for_sasl_response/3,
	 session_established/2, session_established/3, handle_event/3,
	 handle_sync_event/4, code_change/4, handle_info/3,
	 terminate/3, print_state/1, migrate/3,
	 migrate_shutdown/3]).

-include("ejabberd.hrl").

-include("jlib.hrl").

-include("mod_privacy.hrl").

-include("ejabberd_c2s.hrl").

%-define(DBGFSM, true).

-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

-endif.

%% Module start with or without supervisor:
-ifdef(NO_TRANSIENT_SUPERVISORS).

-define(SUPERVISOR_START,
	(?GEN_FSM):start(ejabberd_c2s,
			 [SockData, Opts, FSMLimitOpts],
			 FSMLimitOpts ++ (?FSMOPTS))).

-else.

-define(SUPERVISOR_START,
	supervisor:start_child(ejabberd_c2s_sup,
			       [SockData, Opts, FSMLimitOpts])).

-endif.

-define(C2S_OPEN_TIMEOUT, 60000).

-define(C2S_HIBERNATE_TIMEOUT, 90000).

-define(STREAM_HEADER,
	<<"<?xml version='1.0'?><stream:stream "
	  "xmlns='jabber:client' xmlns:stream='http://et"
	  "herx.jabber.org/streams' id='~s' from='~s'~s~"
	  "s>">>).

-define(FLASH_STREAM_HEADER,
	<<"<?xml version='1.0'?><flash:stream xmlns='jab"
	  "ber:client' xmlns:stream='http://etherx.jabbe"
	  "r.org/streams' id='~s' from='~s'~s~s>">>).

-define(STREAM_TRAILER, <<"</stream:stream>">>).

-define(INVALID_NS_ERR, ?SERR_INVALID_NAMESPACE).

-define(INVALID_XML_ERR, ?SERR_XML_NOT_WELL_FORMED).

-define(HOST_UNKNOWN_ERR, ?SERR_HOST_UNKNOWN).

-define(POLICY_VIOLATION_ERR(Lang, Text),
	?SERRT_POLICY_VIOLATION(Lang, Text)).

-define(INVALID_FROM, ?SERR_INVALID_FROM).

-define(NS_P1_REBIND, <<"p1:rebind">>).

-define(NS_P1_PUSH, <<"p1:push">>).

-define(NS_P1_ACK, <<"p1:ack">>).

-define(NS_P1_PUSHED, <<"p1:pushed">>).

-define(NS_P1_ATTACHMENT,
	<<"http://process-one.net/attachement">>).

-define(C2S_P1_ACK_TIMEOUT, 10000).

-define(MAX_OOR_TIMEOUT, 1440).

-define(MAX_OOR_MESSAGES, 1000).

start(StateName,
      #state{fsm_limit_opts = Opts} = State) ->
    start(StateName, State, Opts);
start(SockData, Opts) ->
    start(SockData, Opts, fsm_limit_opts(Opts)).

start(SockData, Opts, FSMLimitOpts) ->
    ?SUPERVISOR_START.

start_link(SockData, Opts, FSMLimitOpts) ->
    (?GEN_FSM):start_link(ejabberd_c2s,
			  [SockData, Opts, FSMLimitOpts],
			  FSMLimitOpts ++ (?FSMOPTS)).

socket_type() -> xml_stream.

get_presence(FsmRef) ->
    (?GEN_FSM):sync_send_all_state_event(FsmRef,
					 {get_presence}, 1000).

add_rosteritem(FsmRef, IJID, ISubscription) ->
    (?GEN_FSM):send_all_state_event(FsmRef,
				    {add_rosteritem, IJID, ISubscription}).

del_rosteritem(FsmRef, IJID) ->
    (?GEN_FSM):send_all_state_event(FsmRef,
				    {del_rosteritem, IJID}).

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

%% Used by mod_ack and mod_ping.  
%% If the client is not oor capable, we must stop the session,
%% and be sure to not return until the c2s process has really stopped. This
%% is to avoid race conditions when resending messages in mod_ack (EJABS-1677). 
%% In the other side, if the client is oor capable, then this just
%% switch reception to false, and returns inmediately.
stop_or_detach(FsmRef) ->
    case ?GEN_FSM:sync_send_event(FsmRef, stop_or_detach) of
	stopped ->
		MRef = erlang:monitor(process, FsmRef),
		receive 
		   {'DOWN', MRef, process, FsmRef, _Reason}->
			ok
		after 5 ->
			catch exit(FsmRef, kill)
		end,
		erlang:demonitor(MRef, [flush]),
		ok;
	detached ->
		ok
    end.


migrate(FsmRef, Node, After) when node(FsmRef) == node() ->
    erlang:send_after(After, FsmRef, {migrate, Node});
migrate(_FsmRef, _Node, _After) ->
    ok.

migrate_shutdown(FsmRef, Node, After) ->
    FsmRef ! {migrate_shutdown, Node, After}.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

init([{SockMod, Socket}, Opts, FSMLimitOpts]) ->
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
    Zlib = lists:member(zlib, Opts),
    StartTLS = lists:member(starttls, Opts),
    StartTLSRequired = lists:member(starttls_required,
				    Opts),
    TLSEnabled = lists:member(tls, Opts),
    TLS = StartTLS orelse
	    StartTLSRequired orelse TLSEnabled,
    TLSOpts1 = lists:filter(fun ({certfile, _}) -> true;
				(_) -> false
			    end,
			    Opts),
    TLSOpts = [verify_none | TLSOpts1],
    Redirect = case lists:keysearch(redirect, 1, Opts) of
		 {value, {_, true}} -> true;
		 _ -> false
	       end,
    IP = case lists:keysearch(frontend_ip, 1, Opts) of
	   {value, {_, IP1}} -> IP1;
	   _ -> peerip(SockMod, Socket)
	 end,
    FlashHack = ejabberd_config:get_local_option(
                  flash_hack, fun(V) -> V end, false),
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
			     shaper = Shaper, ip = IP, redirect = Redirect,
                             flash_hack = FlashHack,
			     fsm_limit_opts = FSMLimitOpts},
	  erlang:send_after(?C2S_OPEN_TIMEOUT, self(),
			    open_timeout),
	  case get_jid_from_opts(Opts) of
	    {ok,
	     #jid{user = U, server = Server, resource = R} = JID} ->
		(?GEN_FSM):send_event(self(), open_session),
		{ok, wait_for_session,
		 StateData#state{user = U, server = Server, resource = R,
				 jid = JID, lang = <<"">>}};
	    _ -> {ok, wait_for_stream, StateData, ?C2S_OPEN_TIMEOUT}
	  end
    end;
init([StateName, StateData, _FSMLimitOpts]) ->
    MRef =
	(StateData#state.sockmod):monitor(StateData#state.socket),
    if StateName == session_established ->
	   Conn = (StateData#state.sockmod):get_conn_type(
                    StateData#state.socket),
	   Info = [{ip, StateData#state.ip}, {conn, Conn},
		   {auth_module, StateData#state.auth_module}],
	   {Time, _} = StateData#state.sid,
	   SID = {Time, self()},
	   Priority = case StateData#state.pres_last of
			undefined -> undefined;
			El -> get_priority_from_presence(El)
		      end,
	   ejabberd_sm:drop_session(StateData#state.sid),
	   ejabberd_sm:open_session(SID, StateData#state.user,
				    StateData#state.server,
				    StateData#state.resource, Priority, Info),
	   NewStateData = StateData#state{sid = SID,
					  socket_monitor = MRef},
	   StateData2 = change_reception(NewStateData, true),
	   StateData3 = start_keepalive_timer(StateData2),
	   {ok, StateName, StateData3};
       true ->
	   {ok, StateName, StateData#state{socket_monitor = MRef}}
    end.

get_subscribed(FsmRef) ->
    (?GEN_FSM):sync_send_all_state_event(FsmRef,
					 get_subscribed, 1000).

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

wait_for_stream({xmlstreamstart, Name, Attrs},
		StateData) ->
    DefaultLang = ?MYLANG,
    case {xml:get_attr_s(<<"xmlns:stream">>, Attrs),
	  xml:get_attr_s(<<"xmlns:flash">>, Attrs),
          StateData#state.flash_hack,
	  StateData#state.flash_connection}
	of
      {_, ?NS_FLASH_STREAM, true, false} ->
	  wait_for_stream({xmlstreamstart, Name,
			   [{<<"xmlns:stream">>, ?NS_STREAM} | Attrs]},
			  StateData#state{flash_connection = true});
      {?NS_STREAM, _, _, _} ->
	  Server = jlib:nameprep(xml:get_attr_s(<<"to">>, Attrs)),
	  case lists:member(Server, ?MYHOSTS) of
	    true ->
		Lang = case xml:get_attr_s(<<"xml:lang">>, Attrs) of
			 Lang1 when byte_size(Lang1) =< 35 ->
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
		change_shaper(StateData,
			      jlib:make_jid(<<"">>, Server, <<"">>)),
		case xml:get_attr_s(<<"version">>, Attrs) of
		  <<"1.0">> ->
		      send_header(StateData, Server, <<"1.0">>, DefaultLang),
		      case StateData#state.authenticated of
			false ->
			    SASLState = cyrsasl:server_new(<<"jabber">>, Server,
							   <<"">>, [],
							   fun (U) ->
								   ejabberd_auth:get_password_with_authmodule(U,
													      Server)
							   end,
							   fun (U, P) ->
								   ejabberd_auth:check_password_with_authmodule(U,
														Server,
														P)
							   end,
							   fun (U, P, D, DG) ->
								   ejabberd_auth:check_password_with_authmodule(U,
														Server,
														P,
														D,
														DG)
							   end),
			    Mechs = lists:map(fun (S) ->
						      #xmlel{name =
								 <<"mechanism">>,
							     attrs = [],
							     children =
								 [{xmlcdata,
								   S}]}
					      end,
					      cyrsasl:listmech(Server)),
			    SockMod =
				(StateData#state.sockmod):get_sockmod(StateData#state.socket),
			    Zlib = StateData#state.zlib,
			    CompressFeature = case Zlib andalso
						     (SockMod == gen_tcp orelse
							SockMod == tls)
						  of
						true ->
						    [#xmlel{name =
								<<"compression">>,
							    attrs =
								[{<<"xmlns">>,
								  ?NS_FEATURE_COMPRESS}],
							    children =
								[#xmlel{name =
									    <<"method">>,
									attrs =
									    [],
									children
									    =
									    [{xmlcdata,
									      <<"zlib">>}]}]}];
						_ -> []
					      end,
			    TLS = StateData#state.tls,
			    TLSEnabled = StateData#state.tls_enabled,
			    TLSRequired = StateData#state.tls_required,
			    TLSFeature = case TLS == true andalso
						TLSEnabled == false andalso
						  SockMod == gen_tcp
					     of
					   true ->
					       case TLSRequired of
						 true ->
						     [#xmlel{name =
								 <<"starttls">>,
							     attrs =
								 [{<<"xmlns">>,
								   ?NS_TLS}],
							     children =
								 [#xmlel{name =
									     <<"required">>,
									 attrs =
									     [],
									 children
									     =
									     []}]}];
						 _ ->
						     [#xmlel{name =
								 <<"starttls">>,
							     attrs =
								 [{<<"xmlns">>,
								   ?NS_TLS}],
							     children = []}]
					       end;
					   false -> []
					 end,
			    P1PushFeature = [#xmlel{name = <<"push">>,
						    attrs =
							[{<<"xmlns">>,
							  ?NS_P1_PUSH}],
						    children = []}],
			    P1RebindFeature = [#xmlel{name = <<"rebind">>,
						      attrs =
							  [{<<"xmlns">>,
							    ?NS_P1_REBIND}],
						      children = []}],
			    P1AckFeature = [#xmlel{name = <<"ack">>,
						   attrs =
						       [{<<"xmlns">>,
							 ?NS_P1_ACK}],
						   children = []}],
			    send_element(StateData,
					 #xmlel{name = <<"stream:features">>,
						attrs = [],
						children =
						    TLSFeature ++
						      CompressFeature ++
							P1PushFeature ++
							  P1RebindFeature ++
							    P1AckFeature ++
							      [#xmlel{name =
									  <<"mechanisms">>,
								      attrs =
									  [{<<"xmlns">>,
									    ?NS_SASL}],
								      children =
									  Mechs}]
								++
								ejabberd_hooks:run_fold(c2s_stream_features,
											Server,
											[],
											[Server])}),
			    fsm_next_state(wait_for_feature_request,
					   StateData#state{server = Server,
							   sasl_state =
							       SASLState,
							   lang = Lang});
			_ ->
			    case StateData#state.resource of
			      <<"">> ->
				  RosterVersioningFeature =
				      ejabberd_hooks:run_fold(roster_get_versioning_feature,
							      Server, [],
							      [Server]),
				  StreamFeatures = [#xmlel{name = <<"push">>,
							   attrs =
							       [{<<"xmlns">>,
								 ?NS_P1_PUSH}],
							   children = []},
						    #xmlel{name = <<"bind">>,
							   attrs =
							       [{<<"xmlns">>,
								 ?NS_BIND}],
							   children = []},
						    #xmlel{name = <<"session">>,
							   attrs =
							       [{<<"xmlns">>,
								 ?NS_SESSION}],
							   children = []}]
						     ++
						     RosterVersioningFeature ++
						       ejabberd_hooks:run_fold(c2s_stream_features,
									       Server,
									       [],
									       [Server]),
				  send_element(StateData,
					       #xmlel{name =
							  <<"stream:features">>,
						      attrs = [],
						      children =
							  StreamFeatures}),
				  fsm_next_state(wait_for_bind,
						 StateData#state{server =
								     Server,
								 lang = Lang});
			      _ ->
				  send_element(StateData,
					       #xmlel{name =
							  <<"stream:features">>,
						      attrs = [],
						      children = []}),
				  fsm_next_state(wait_for_session,
						 StateData#state{server =
								     Server,
								 lang = Lang})
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
	  case Name of
	    <<"policy-file-request">> ->
		send_text(StateData, flash_policy_string()),
		{stop, normal, StateData};
	    _ ->
		send_header(StateData, ?MYNAME, <<"">>, DefaultLang),
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
    send_header(StateData, ?MYNAME, <<"1.0">>, <<"">>),
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.
wait_for_stream(stop_or_detach, _From, StateData) ->
    {stop, normal, stopped, StateData}.

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
			       sha:sha(<<(StateData#state.streamid)/binary, PW/binary>>)
		       end,
		case ejabberd_auth:check_password_with_authmodule(U,
								  StateData#state.server,
								  P, D, DGen)
		    of
		  {true, AuthModule} ->
		      ?INFO_MSG("(~w) Accepted legacy authentication "
				"for ~s by ~p",
				[StateData#state.socket,
				 jlib:jid_to_string(JID), AuthModule]),
		      case need_redirect(StateData#state{user = U}) of
			{true, Host} ->
			    ?INFO_MSG("(~w) Redirecting ~s to ~s",
				      [StateData#state.socket,
				       jlib:jid_to_string(JID), Host]),
			    send_element(StateData, ?SERR_SEE_OTHER_HOST(Host)),
			    send_trailer(StateData),
			    {stop, normal, StateData};
			false ->
			    SID = {now(), self()},
			    Conn = (StateData#state.sockmod):get_conn_type(
                                     StateData#state.socket),
			    Res1 = jlib:make_result_iq_reply(El),
			    Res = Res1#xmlel{children = []},
			    send_element(StateData, Res),
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
			    PrivList =
				ejabberd_hooks:run_fold(privacy_get_user_list,
							StateData#state.server,
							#userlist{},
							[U,
							 StateData#state.server]),
			    NewStateData = StateData#state{user = U,
							   resource = R,
							   jid = JID, sid = SID,
							   conn = Conn,
							   auth_module =
							       AuthModule,
							   pres_f =
							       (?SETS):from_list(Fs1),
							   pres_t =
							       (?SETS):from_list(Ts1),
							   privacy_list =
							       PrivList},
			    DebugFlag =
				ejabberd_hooks:run_fold(c2s_debug_start_hook,
							NewStateData#state.server,
							false,
							[self(), NewStateData]),
			    maybe_migrate(session_established,
					  NewStateData#state{debug = DebugFlag})
		      end;
		  _ ->
		      ?INFO_MSG("(~w) Failed legacy authentication for ~s",
				[StateData#state.socket,
				 jlib:jid_to_string(JID)]),
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
	  #xmlel{name = Name, attrs = Attrs} = El,
	  case {xml:get_attr_s(<<"xmlns">>, Attrs), Name} of
	    {?NS_P1_REBIND, <<"rebind">>} ->
		SJID = xml:get_path_s(El, [{elem, <<"jid">>}, cdata]),
		SID = xml:get_path_s(El, [{elem, <<"sid">>}, cdata]),
		case jlib:string_to_jid(SJID) of
		  error ->
		      send_element(StateData,
				   #xmlel{name = <<"failure">>,
					  attrs =
					      [{<<"xmlns">>, ?NS_P1_REBIND}],
					  children =
					      [{xmlcdata, <<"Invalid JID">>}]}),
		      fsm_next_state(wait_for_auth, StateData);
		  JID ->
		      case rebind(StateData, JID, SID) of
			{next_state, wait_for_feature_request, NewStateData,
			 Timeout} ->
			    {next_state, wait_for_auth, NewStateData, Timeout};
			Res -> Res
		      end
		end;
	    _ ->
		process_unauthenticated_stanza(StateData, El),
		fsm_next_state(wait_for_auth, StateData)
	  end
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
wait_for_auth(stop_or_detach,_From, StateData) ->
    {stop, normal, stopped, StateData}.

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
		catch
		  (StateData#state.sockmod):reset_stream(StateData#state.socket),
		%U = xml:get_attr_s(username, Props),
		U = proplists:get_value(username, Props, <<>>),
		%AuthModule = xml:get_attr_s(auth_module, Props),
		AuthModule = proplists:get_value(auth_module, Props, undefined),
		?INFO_MSG("(~w) Accepted authentication for ~s "
			  "by ~p",
			  [StateData#state.socket, U, AuthModule]),
		case need_redirect(StateData#state{user = U}) of
		  {true, Host} ->
		      ?INFO_MSG("(~w) Redirecting ~s to ~s",
				[StateData#state.socket, U, Host]),
		      send_element(StateData, ?SERR_SEE_OTHER_HOST(Host)),
		      send_trailer(StateData),
		      {stop, normal, StateData};
		  false ->
		      send_element(StateData,
				   #xmlel{name = <<"success">>,
					  attrs = [{<<"xmlns">>, ?NS_SASL}],
					  children = []}),
		      fsm_next_state(wait_for_stream,
				     StateData#state{streamid = new_id(),
						     authenticated = true,
						     auth_module = AuthModule,
						     user = U})
		end;
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
		?INFO_MSG("(~w) Failed authentication for ~s@~s",
			  [StateData#state.socket, Username,
			   StateData#state.server]),
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
		      ejabberd_config:get_local_option(
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
	       (SockMod == gen_tcp) or (SockMod == tls) ->
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
      {?NS_P1_REBIND, <<"rebind">>} ->
	  SJID = xml:get_path_s(El, [{elem, <<"jid">>}, cdata]),
	  SID = xml:get_path_s(El, [{elem, <<"sid">>}, cdata]),
	  case jlib:string_to_jid(SJID) of
	    error ->
		send_element(StateData,
			     #xmlel{name = <<"failure">>,
				    attrs = [{<<"xmlns">>, ?NS_P1_REBIND}],
				    children =
					[{xmlcdata, <<"Invalid JID">>}]}),
		fsm_next_state(wait_for_feature_request, StateData);
	    JID -> rebind(StateData, JID, SID)
	  end;
      {?NS_P1_ACK, <<"ack">>} ->
	  fsm_next_state(wait_for_feature_request,
			 StateData#state{ack_enabled = true});
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
wait_for_feature_request(stop_or_detach, _From, StateData) ->
    {stop, normal, stopped, StateData}.

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
		case need_redirect(StateData#state{user = U}) of
		  {true, Host} ->
		      ?INFO_MSG("(~w) Redirecting ~s to ~s",
				[StateData#state.socket, U, Host]),
		      send_element(StateData, ?SERR_SEE_OTHER_HOST(Host)),
		      send_trailer(StateData),
		      {stop, normal, StateData};
		  false ->
		      send_element(StateData,
				   #xmlel{name = <<"success">>,
					  attrs = [{<<"xmlns">>, ?NS_SASL}],
					  children = []}),
		      fsm_next_state(wait_for_stream,
				     StateData#state{streamid = new_id(),
						     authenticated = true,
						     auth_module = AuthModule,
						     user = U})
		end;
	    {ok, Props, ServerOut} ->
		(StateData#state.sockmod):reset_stream(StateData#state.socket),
%		U = xml:get_attr_s(username, Props),
		U = proplists:get_value(username, Props, <<>>),
%		AuthModule = xml:get_attr_s(auth_module, Props),
		AuthModule = proplists:get_value(auth_module, Props, undefined),
		?INFO_MSG("(~w) Accepted authentication for ~s "
			  "by ~p",
			  [StateData#state.socket, U, AuthModule]),
		case need_redirect(StateData#state{user = U}) of
		  {true, Host} ->
		      ?INFO_MSG("(~w) Redirecting ~s to ~s",
				[StateData#state.socket, U, Host]),
		      send_element(StateData, ?SERR_SEE_OTHER_HOST(Host)),
		      send_trailer(StateData),
		      {stop, normal, StateData};
		  false ->
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
						     user = U})
		end;
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
		?INFO_MSG("(~w) Failed authentication for ~s@~s",
			  [StateData#state.socket, Username,
			   StateData#state.server]),
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
wait_for_sasl_response(stop_or_detach, _From, StateData) ->
    {stop, normal, stopped, StateData}.

resource_conflict_action(U, S, R) ->
    OptionRaw = case ejabberd_sm:is_existing_resource(U, S,
						      R)
		    of
		  true ->
		      ejabberd_config:get_local_option(
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
	  Rnew = randoms:get_string(),
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
		<<"">> -> randoms:get_string();
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
wait_for_bind(stop_or_detach, _From, StateData) ->
    {stop, normal, stopped, StateData}.

wait_for_session({xmlstreamelement, El}, StateData) ->
    case jlib:iq_query_info(El) of
      #iq{type = set, xmlns = ?NS_SESSION} ->
	  U = StateData#state.user,
	  JID = StateData#state.jid,
	  case acl:match_rule(StateData#state.server,
			      StateData#state.access, JID)
	      of
	    allow ->
		?INFO_MSG("(~w) Opened session for ~s",
			  [StateData#state.socket, jlib:jid_to_string(JID)]),
		Res = jlib:make_result_iq_reply(El),
		send_element(StateData, Res),
		change_shaper(StateData, JID),
		{Fs, Ts} =
		    ejabberd_hooks:run_fold(roster_get_subscription_lists,
					    StateData#state.server, {[], []},
					    [U, StateData#state.server]),
		LJID = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
		Fs1 = [LJID | Fs],
		Ts1 = [LJID | Ts],
		PrivList =
		    ejabberd_hooks:run_fold(privacy_get_user_list,
					    StateData#state.server, #userlist{},
					    [U, StateData#state.server]),
		SID = {now(), self()},
		Conn = (StateData#state.sockmod):get_conn_type(
                         StateData#state.socket),
		NewStateData = StateData#state{sid = SID, conn = Conn,
					       pres_f = (?SETS):from_list(Fs1),
					       pres_t = (?SETS):from_list(Ts1),
					       privacy_list = PrivList},
		DebugFlag =
		    ejabberd_hooks:run_fold(c2s_debug_start_hook,
					    NewStateData#state.server, false,
					    [self(), NewStateData]),
		maybe_migrate(session_established,
			      NewStateData#state{debug = DebugFlag});
	    _ ->
		ejabberd_hooks:run(forbidden_session_hook,
				   StateData#state.server, [JID]),
		?INFO_MSG("(~w) Forbidden session for ~s",
			  [StateData#state.socket, jlib:jid_to_string(JID)]),
		Err = jlib:make_error_reply(El, ?ERR_NOT_ALLOWED),
		send_element(StateData, Err),
		fsm_next_state(wait_for_session, StateData)
	  end;
      _ -> fsm_next_state(wait_for_session, StateData)
    end;
wait_for_session(open_session, StateData) ->
    El = #xmlel{name = <<"iq">>,
		attrs =
		    [{<<"type">>, <<"set">>}, {<<"id">>, <<"session">>}],
		children =
		    [#xmlel{name = <<"session">>,
			    attrs = [{<<"xmlns">>, ?NS_SESSION}],
			    children = []}]},
    wait_for_session({xmlstreamelement, El}, StateData);
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
wait_for_session(stop_or_detach, _From, StateData) ->
    {stop, normal, stopped, StateData}.

session_established({xmlstreamelement, El},
		    StateData) ->
    FromJID = StateData#state.jid,
    case check_from(El, FromJID) of
      'invalid-from' ->
	  send_element(StateData, ?INVALID_FROM),
	  send_trailer(StateData),
	  {stop, normal, StateData};
      _NewEl ->
	  NSD1 = change_reception(StateData, true),
	  NSD2 = start_keepalive_timer(NSD1),
	  session_established2(El, NSD2)
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
    if not StateData#state.reception ->
	   fsm_next_state(session_established, StateData);
       StateData#state.keepalive_timer /= undefined ->
	   NewState1 = change_reception(StateData, false),
	   NewState = start_keepalive_timer(NewState1),
	   fsm_next_state(session_established, NewState);
       true -> {stop, normal, StateData}
    end.
session_established(stop_or_detach, From, StateData) ->
    if
	not StateData#state.reception ->
	    ?GEN_FSM:reply(From, detached),
	    fsm_next_state(session_established, StateData);
	(StateData#state.keepalive_timer /= undefined) ->
	    NewState1 = change_reception(StateData, false),
	    NewState = start_keepalive_timer(NewState1),
	    ?GEN_FSM:reply(From, detached),
	    fsm_next_state(session_established, NewState);
	true ->
	    {stop, normal, stopped, StateData}
    end.

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
					      [StateData#state.debug, FromJID,
					       ToJID, PresenceEl]),
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
				 ejabberd_hooks:run(user_send_packet, Server,
						    [StateData#state.debug,
						     FromJID, ToJID, NewEl]),
				 process_privacy_iq(FromJID, ToJID, IQ,
						    StateData);
			     #iq{xmlns = ?NS_P1_PUSH} = IQ ->
				 process_push_iq(FromJID, ToJID, IQ, StateData);
			     _ ->
				 ejabberd_hooks:run(user_send_packet, Server,
						    [StateData#state.debug,
						     FromJID, ToJID, NewEl]),
				 check_privacy_route(FromJID, StateData,
						     FromJID, ToJID, NewEl),
				 StateData
			   end;
		       <<"message">> ->
			   ejabberd_hooks:run(user_send_packet, Server,
					      [StateData#state.debug, FromJID,
					       ToJID, NewEl]),
			   check_privacy_route(FromJID, StateData, FromJID,
					       ToJID, NewEl),
			   StateData;
		       <<"standby">> ->
			   StandBy = xml:get_tag_cdata(NewEl) == <<"true">>,
			   change_standby(StateData, StandBy);
		       <<"a">> ->
			   SCounter = xml:get_tag_attr_s(<<"h">>, NewEl),
			   receive_ack(StateData, SCounter);
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

handle_event({add_rosteritem, IJID, ISubscription},
	     StateName, StateData) ->
    NewStateData = roster_change(IJID, ISubscription,
				 StateData),
    fsm_next_state(StateName, NewStateData);
handle_event({del_rosteritem, IJID}, StateName,
	     StateData) ->
    NewStateData = roster_change(IJID, none, StateData),
    fsm_next_state(StateName, NewStateData);
handle_event({xmlstreamcdata, _},
	     session_established = StateName, StateData) ->
    ?DEBUG("cdata ping", []),
    NSD1 = change_reception(StateData, true),
    NSD2 = start_keepalive_timer(NSD1),
    fsm_next_state(StateName, NSD2);
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
handle_sync_event(_Event, _From, StateName,
		  StateData) ->
    Reply = ok, fsm_reply(Reply, StateName, StateData).

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData, Text),
    ejabberd_hooks:run(c2s_loop_debug, [Text]),
    fsm_next_state(StateName, StateData);
handle_info(replaced, _StateName, StateData) ->
    Lang = StateData#state.lang,
    send_element(StateData,
		 ?SERRT_CONFLICT(Lang,
				 <<"Replaced by new connection">>)),
    send_trailer(StateData),
    {stop, normal,
     StateData#state{authenticated = replaced}};
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
                    PrivFrom = jlib:jid_remove_resource(StateData#state.jid),
                    PrivTo = StateData#state.jid,
                    PrivPushEl = jlib:replace_from_to(
                                   PrivFrom,
                                   PrivTo,
                                   jlib:iq_to_xml(PrivPushIQ)),
                    StateData2 = send_or_enqueue_packet(
                                   StateData, PrivFrom, PrivTo, PrivPushEl),
                    fsm_next_state(StateName,
                                   StateData2#state{privacy_list = NewPL})
            end;
        {blocking, What} ->
            route_blocking(What, StateData),
            fsm_next_state(StateName, StateData);
        {rebind, Pid2, StreamID2} ->
            if StreamID2 == StateData#state.streamid ->
                    Pid2 ! {rebind, prepare_acks_for_rebind(StateData)},
                    receive after 1000 -> ok end,
                    catch send_trailer(StateData),
                    {stop, normal, StateData#state{authenticated = rebinded}};
               true ->
                    Pid2 ! {rebind, false},
                    fsm_next_state(StateName, StateData)
            end;
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
					 allow ->
					     if StateData#state.reception ->
						    case
						      ejabberd_hooks:run_fold(feature_check_packet,
									      StateData#state.server,
									      allow,
									      [StateData#state.jid,
									       StateData#state.server,
									       StateData#state.pres_last,
									       {From,
										To,
										Packet},
									       in])
							of
						      allow ->
							  {true, Attrs,
							   StateData};
						      deny ->
							  {false, Attrs,
							   StateData}
						    end;
						true -> {true, Attrs, StateData}
					     end;
					 deny -> {false, Attrs, StateData}
				       end;
				   _ -> {true, Attrs, StateData}
				 end,
       if Pass ->
	   Attrs2 =
	       jlib:replace_from_to_attrs(jlib:jid_to_string(From),
					  jlib:jid_to_string(To), NewAttrs),
	   FixedPacket = #xmlel{name = Name, attrs = Attrs2,
				children = Els},
	   NewState2 = send_or_enqueue_packet(NewState, From, To, FixedPacket),
	   ejabberd_hooks:run(user_receive_packet,
			      StateData#state.server,
			      [StateData#state.debug, StateData#state.jid, From,
			       To, FixedPacket]),
	   ejabberd_hooks:run(c2s_loop_debug,
			      [{route, From, To, Packet}]),
	   fsm_next_state(StateName, NewState2);
       true ->
	   ejabberd_hooks:run(c2s_loop_debug,
			      [{route, From, To, Packet}]),
	   fsm_next_state(StateName, NewState)
    end;
handle_info({timeout, Timer, _}, StateName,
	    #state{keepalive_timer = Timer, reception = true} =
		StateData) ->
    NewState1 = change_reception(StateData, false),
    NewState = start_keepalive_timer(NewState1),
    fsm_next_state(StateName, NewState);
handle_info({timeout, Timer, _}, _StateName,
	    #state{keepalive_timer = Timer, reception = false} =
		StateData) ->
    {stop, normal, StateData};
handle_info({timeout, Timer, PrevCounter}, StateName,
	    #state{ack_timer = Timer} = StateData) ->
    AckCounter = StateData#state.ack_counter,
    NewState = if PrevCounter >= AckCounter ->
		      StateData#state{ack_timer = undefined};
		  true ->
		      send_ack_request(StateData#state{ack_timer = undefined})
	       end,
    fsm_next_state(StateName, NewState);
handle_info({ack_timeout, Counter}, StateName,
	    StateData) ->
    AckQueue = StateData#state.ack_queue,
    case queue:is_empty(AckQueue) of
      true -> fsm_next_state(StateName, StateData);
      false ->
	  C = element(1, queue:head(AckQueue)),
	  if C =< Counter -> {stop, normal, StateData};
	     true -> fsm_next_state(StateName, StateData)
	  end
    end;
handle_info(open_timeout, StateName, StateData) ->
    case StateName of
      session_established ->
	  fsm_next_state(StateName, StateData);
      _ -> {stop, normal, StateData}
    end;
handle_info({'DOWN', Monitor, _Type, _Object, _Info},
	    StateName, StateData)
    when Monitor == StateData#state.socket_monitor ->
    if (StateName == session_established) and
	 not StateData#state.reception ->
	   fsm_next_state(StateName, StateData);
       (StateName == session_established) and
	 (StateData#state.keepalive_timer /= undefined) ->
	   NewState1 = change_reception(StateData, false),
	   NewState = start_keepalive_timer(NewState1),
	   fsm_next_state(StateName, NewState);
       true -> {stop, normal, StateData}
    end;
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
handle_info({migrate, Node}, StateName, StateData) ->
    if Node /= node() ->
	   fsm_migrate(StateName, StateData, Node, 0);
       true -> fsm_next_state(StateName, StateData)
    end;
handle_info({migrate_shutdown, Node, After}, StateName,
	    StateData) ->
    case StateData#state.sockmod == ejabberd_frontend_socket
	   orelse
	   StateData#state.xml_socket == true orelse
	     (StateData#state.sockmod):is_remote_receiver(
               StateData#state.socket)
	of
      true -> migrate(self(), Node, After);
      false -> self() ! system_shutdown
    end,
    fsm_next_state(StateName, StateData);
handle_info({broadcast, Type, From, Packet}, StateName,
	    StateData) ->
    Recipients =
	ejabberd_hooks:run_fold(c2s_broadcast_recipients,
				StateData#state.server, [],
				[StateData#state.server, StateData,
                                 Type, From, Packet]),
    lists:foreach(fun (USR) ->
			  ejabberd_router:route(From, jlib:make_jid(USR),
						Packet)
		  end,
		  lists:usort(Recipients)),
    fsm_next_state(StateName, StateData);
handle_info({change_socket, Socket}, StateName,
	    StateData) ->
    erlang:demonitor(StateData#state.socket_monitor),
    NewSocket =
	(StateData#state.sockmod):change_socket(StateData#state.socket,
						Socket),
    MRef = (StateData#state.sockmod):monitor(NewSocket),
    fsm_next_state(StateName,
		   StateData#state{socket = NewSocket,
				   socket_monitor = MRef});
handle_info(Info, StateName, StateData) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    fsm_next_state(StateName, StateData).

print_state(State = #state{pres_t = T, pres_f = F,
			   pres_a = A}) ->
    State#state{pres_t = {pres_t, (?SETS):size(T)},
		pres_f = {pres_f, (?SETS):size(F)},
		pres_a = {pres_a, (?SETS):size(A)}}.

terminate({migrated, ClonePid}, StateName, StateData) ->
    ejabberd_hooks:run(c2s_debug_stop_hook,
		       StateData#state.server, [self(), StateData]),
    if StateName == session_established ->
	   ?INFO_MSG("(~w) Migrating ~s to ~p on node ~p",
		     [StateData#state.socket,
		      jlib:jid_to_string(StateData#state.jid), ClonePid,
		      node(ClonePid)]),
	   ejabberd_sm:close_migrated_session(StateData#state.sid,
					      StateData#state.user,
					      StateData#state.server,
					      StateData#state.resource);
       true -> ok
    end,
    (StateData#state.sockmod):change_controller(StateData#state.socket,
						ClonePid),
    ok;
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
				   StateData#state.pres_a, Packet);
	    rebinded ->
		ejabberd_sm:close_migrated_session(StateData#state.sid,
						   StateData#state.user,
						   StateData#state.server,
						   StateData#state.resource),
		ok;
	    _ ->
		?INFO_MSG("(~w) Close session for ~s",
			  [StateData#state.socket,
			   jlib:jid_to_string(StateData#state.jid)]),
		EmptySet = (?SETS):new(),
		case StateData of
		  #state{pres_last = undefined, pres_a = EmptySet} ->
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
					 StateData#state.pres_a, Packet)
		end
	  end,
	  case StateData#state.authenticated of
	    rebinded -> ok;
	    _ ->
		if not StateData#state.reception,
		   not StateData#state.oor_offline ->
		       SFrom = jlib:jid_to_string(StateData#state.jid),
		       ejabberd_hooks:run(p1_push_notification,
					  StateData#state.server,
					  [StateData#state.server,
					   StateData#state.jid,
					   StateData#state.oor_notification,
					   <<"Instant messaging session expired">>,
					   0, false, StateData#state.oor_appid,
					   SFrom]);
		   true -> ok
		end,
		lists:foreach(fun ({_Counter, From, To, FixedPacket}) ->
				      ejabberd_router:route(From, To,
							    FixedPacket)
			      end,
			      queue:to_list(StateData#state.ack_queue)),
		lists:foreach(fun ({From, To, FixedPacket}) ->
				      ejabberd_router:route(From, To,
							    FixedPacket)
			      end,
			      queue:to_list(StateData#state.queue))
	  end,
	  bounce_messages();
      _ -> ok
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

-spec send_text(c2s_state(), binary()) -> any().

send_text(StateData, Text)
    when StateData#state.xml_socket ->
    ?DEBUG("Send Text on stream = ~p", [Text]),
    (StateData#state.sockmod):send_xml(StateData#state.socket,
				       {xmlstreamraw, Text});
send_text(StateData, Text) ->
    ?DEBUG("Send XML on stream = ~p", [Text]),
    Text1 = if StateData#state.flash_hack and
		 StateData#state.flash_connection ->
		   <<Text/binary, 0>>;
	       true -> Text
	    end,
    (StateData#state.sockmod):send(StateData#state.socket,
				   Text1).

-spec send_element(c2s_state(), xmlel()) -> any().

send_element(StateData, El)
    when StateData#state.xml_socket ->
    ejabberd_hooks:run(feature_inspect_packet,
		       StateData#state.server,
		       [StateData#state.jid, StateData#state.server,
			StateData#state.pres_last, El]),
    (StateData#state.sockmod):send_xml(StateData#state.socket,
				       {xmlstreamelement, El});
send_element(StateData, El) ->
    ejabberd_hooks:run(feature_inspect_packet,
		       StateData#state.server,
		       [StateData#state.jid, StateData#state.server,
			StateData#state.pres_last, El]),
    send_text(StateData, xml:element_to_binary(El)).

send_header(StateData, Server, Version, Lang)
    when StateData#state.flash_connection ->
    Header = io_lib:format(?FLASH_STREAM_HEADER,
			   [StateData#state.streamid, Server, Version, Lang]),
    send_text(StateData, iolist_to_binary(Header));
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

send_or_enqueue_packet(State, From, To, Packet) ->
    #xmlel{name = Name} = Packet,
    if State#state.reception and
       not (State#state.standby and
            (Name /= <<"message">>)) ->
            send_element(State, Packet),
            ack(State, From, To, Packet);
       true ->
            NewState = send_out_of_reception_message(State, From, To, Packet),
            enqueue(NewState, From, To, Packet)
    end.


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
get_auth_tags([], U, P, D, R) -> {U, P, D, R}.

process_presence_probe(From, To, StateData) ->
    LFrom = jlib:jid_tolower(From),
    LBFrom = setelement(3, LFrom, <<"">>),
    case StateData#state.pres_last of
      undefined -> ok;
      _ ->
	  Cond1 =
		    ((?SETS):is_element(LFrom, StateData#state.pres_f)
		       orelse
		       LFrom /= LBFrom andalso
			 (?SETS):is_element(LBFrom, StateData#state.pres_f)),
	  if Cond1 ->
		 Packet = case StateData#state.reception of
			    true -> StateData#state.pres_last;
			    false ->
				case StateData#state.oor_show of
				  <<"">> -> StateData#state.pres_last;
				  _ ->
				      #xmlel{attrs = PresAttrs,
					     children = PresEls} =
					  StateData#state.pres_last,
				      PresEls1 = lists:flatmap(fun (#xmlel{name
									       =
									       Name})
								       when Name
									      ==
									      <<"show">>;
									    Name
									      ==
									      <<"status">> ->
								       [];
								   (E) -> [E]
							       end,
							       PresEls),
				      make_oor_presence(StateData, PresAttrs,
							PresEls1)
				end
			  end,
		 Timestamp = StateData#state.pres_timestamp,
		 Packet1 = maybe_add_delay(Packet, utc, To, <<"">>,
					   Timestamp),
		 case ejabberd_hooks:run_fold(privacy_check_packet,
					      StateData#state.server, allow,
					      [StateData#state.user,
					       StateData#state.server,
					       StateData#state.privacy_list,
					       {To, From, Packet1}, out])
		     of
		   deny -> ok;
		   allow ->
		       Pid = element(2, StateData#state.sid),
		       ejabberd_hooks:run(presence_probe_hook,
					  StateData#state.server,
					  [From, To, Pid]),
		       case From == To of
			 false -> ejabberd_router:route(To, From, Packet1);
			 true -> ok
		       end
		 end;
	     true -> ok
	  end
    end.

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
	  ejabberd_hooks:run(roster_out_subscription, Server,
			     [User, Server, To, subscribe]),
	  check_privacy_route(From, StateData,
			      jlib:jid_remove_resource(From), To, Packet),
	  StateData;
      <<"subscribed">> ->
	  ejabberd_hooks:run(roster_out_subscription, Server,
			     [User, Server, To, subscribed]),
	  check_privacy_route(From, StateData,
			      jlib:jid_remove_resource(From), To, Packet),
	  StateData;
      <<"unsubscribe">> ->
	  ejabberd_hooks:run(roster_out_subscription, Server,
			     [User, Server, To, unsubscribe]),
	  check_privacy_route(From, StateData,
			      jlib:jid_remove_resource(From), To, Packet),
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
    PacketProbe = #xmlel{name = <<"presence">>, attrs = [{<<"type">>,<<"probe">>}], children = []},
    JIDs2Probe = format_and_check_privacy(From, StateData, Packet, JIDsProbe, out),
    Server = StateData#state.server,
    send_multiple(From, Server, JIDs2Probe, PacketProbe),
    As = StateData#state.pres_f, %%Reuse existing structure, don't create a new one
    JIDs = ?SETS:to_list(StateData#state.pres_f),
    JIDs2 = format_and_check_privacy(From, StateData, Packet, JIDs, out),
    Server = StateData#state.server,
    send_multiple(From, Server, JIDs2, Packet),
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

send_multiple(From, Server, JIDs, Packet) ->
    ejabberd_router_multicast:route_multicast(From, Server, JIDs, Packet).


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
    Info1 = [{ip, StateData#state.ip},
	     {conn, StateData#state.conn},
	     {auth_module, StateData#state.auth_module}],
    Info = case StateData#state.reception of
	     false -> [{oor, true} | Info1];
	     _ -> Info1
	   end,
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

maybe_migrate(StateName, StateData) ->
    PackedStateData = pack(StateData),
    #state{user = U, server = S, resource = R, sid = SID} =
	StateData,
    case ejabberd_cluster:get_node({jlib:nodeprep(U),
				    jlib:nameprep(S)})
	of
      Node when Node == node() ->
	  Conn = ejabberd_socket:get_conn_type(StateData#state.socket),
	  Info = [{ip, StateData#state.ip}, {conn, Conn},
		  {auth_module, StateData#state.auth_module}],
	  Presence = StateData#state.pres_last,
	  Priority = case Presence of
		       undefined -> undefined;
		       _ -> get_priority_from_presence(Presence)
		     end,
	  ejabberd_sm:open_session(SID, U, S, R, Priority, Info),
	  StateData2 = change_reception(PackedStateData, true),
	  StateData3 = start_keepalive_timer(StateData2),
	  erlang:garbage_collect(),
	  fsm_next_state(StateName, StateData3);
      Node -> fsm_migrate(StateName, PackedStateData, Node, 0)
    end.

fsm_next_state(session_established, StateData) ->
    {next_state, session_established, StateData,
     ?C2S_HIBERNATE_TIMEOUT};
fsm_next_state(StateName, StateData) ->
    {next_state, StateName, StateData, ?C2S_OPEN_TIMEOUT}.

fsm_migrate(StateName, StateData, Node, Timeout) ->
    {migrate, StateData,
     {Node, ?MODULE, start, [StateName, StateData]},
     Timeout}.

fsm_reply(Reply, session_established, StateData) ->
    {reply, Reply, session_established, StateData,
     ?C2S_HIBERNATE_TIMEOUT};
fsm_reply(Reply, StateName, StateData) ->
    {reply, Reply, StateName, StateData, ?C2S_OPEN_TIMEOUT}.

is_ip_blacklisted(undefined) -> false;
is_ip_blacklisted({IP, _Port}) ->
    ejabberd_hooks:run_fold(check_bl_c2s, false, [IP]).

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

start_keepalive_timer(StateData) ->
    if is_reference(StateData#state.keepalive_timer) ->
	   cancel_timer(StateData#state.keepalive_timer);
       true -> ok
    end,
    Timeout = if StateData#state.reception ->
		     StateData#state.keepalive_timeout;
		 true -> StateData#state.oor_timeout
	      end,
    Timer = if is_integer(Timeout) ->
		   erlang:start_timer(Timeout * 1000, self(), []);
	       true -> undefined
	    end,
    StateData#state{keepalive_timer = Timer}.

change_reception(#state{reception = Reception} =
		     StateData,
		 Reception) ->
    StateData;
change_reception(#state{reception = true} = StateData,
		 false) ->
    ?DEBUG("reception -> false", []),
    case StateData#state.oor_show of
      <<"">> -> ok;
      _ ->
	  Packet = make_oor_presence(StateData),
	  update_priority(0, Packet,
			  StateData#state{reception = false}),
	  presence_broadcast_to_trusted(StateData,
					StateData#state.jid,
					StateData#state.pres_f,
					StateData#state.pres_a, Packet)
    end,
    StateData#state{reception = false};
change_reception(#state{reception = false,
			standby = true} =
		     StateData,
		 true) ->
    ?DEBUG("reception -> standby", []),
    NewQueue = lists:foldl(fun ({_From, _To,
				 #xmlel{name = <<"message">>} = FixedPacket},
				Q) ->
				   send_element(StateData, FixedPacket), Q;
			       (Item, Q) -> queue:in(Item, Q)
			   end,
			   queue:new(), queue:to_list(StateData#state.queue)),
    StateData#state{queue = NewQueue,
		    queue_len = queue:len(NewQueue), reception = true,
		    oor_unread = 0, oor_unread_users = (?SETS):new()};
change_reception(#state{reception = false} = StateData,
		 true) ->
    ?DEBUG("reception -> true", []),
    case StateData#state.oor_show of
      <<"">> -> ok;
      _ ->
	  Packet = StateData#state.pres_last,
	  NewPriority = get_priority_from_presence(Packet),
	  update_priority(NewPriority, Packet,
			  StateData#state{reception = true}),
	  presence_broadcast_to_trusted(StateData,
					StateData#state.jid,
					StateData#state.pres_f,
					StateData#state.pres_a, Packet)
    end,
    lists:foreach(fun ({_From, _To, FixedPacket}) ->
			  send_element(StateData, FixedPacket)
		  end,
		  queue:to_list(StateData#state.queue)),
    lists:foreach(fun (FixedPacket) ->
			  send_element(StateData, FixedPacket)
		  end,
		  gb_trees:values(StateData#state.pres_queue)),
    StateData#state{queue = queue:new(), queue_len = 0,
		    pres_queue = gb_trees:empty(), reception = true,
		    oor_unread = 0, oor_unread_users = (?SETS):new()}.

change_standby(#state{standby = StandBy} = StateData,
	       StandBy) ->
    StateData;
change_standby(#state{standby = false} = StateData,
	       true) ->
    ?DEBUG("standby -> true", []),
    StateData#state{standby = true};
change_standby(#state{standby = true} = StateData,
	       false) ->
    ?DEBUG("standby -> false", []),
    lists:foreach(fun ({_From, _To, FixedPacket}) ->
			  send_element(StateData, FixedPacket)
		  end,
		  queue:to_list(StateData#state.queue)),
    lists:foreach(fun (FixedPacket) ->
			  send_element(StateData, FixedPacket)
		  end,
		  gb_trees:values(StateData#state.pres_queue)),
    StateData#state{queue = queue:new(), queue_len = 0,
		    pres_queue = gb_trees:empty(), standby = false}.

send_out_of_reception_message(StateData, From, To,
			      #xmlel{name = <<"message">>} = Packet) ->
    Type = xml:get_tag_attr_s(<<"type">>, Packet),
    if (Type == <<"normal">>) or (Type == <<"">>) or
	 (Type == <<"chat">>)
	 or
	 StateData#state.oor_send_groupchat and
	   (Type == <<"groupchat">>) ->
	   Body1 = xml:get_path_s(Packet,
				  [{elem, <<"body">>}, cdata]),
	   Body = case check_x_attachment(Packet) of
		    true ->
			case Body1 of
			  <<"">> -> <<238, 128, 136>>;
			  _ -> <<238, 128, 136, 32, Body1/binary>>
			end;
		    false -> Body1
		  end,
	   Pushed = check_x_pushed(Packet),
	   if Body == <<"">>; Pushed -> StateData;
	      true ->
		  BFrom = jlib:jid_remove_resource(From),
		  LBFrom = jlib:jid_tolower(BFrom),
		  UnreadUsers = (?SETS):add_element(LBFrom,
						    StateData#state.oor_unread_users),
		  IncludeBody = case StateData#state.oor_send_body of
				  all -> true;
				  first_per_user ->
				      not
					(?SETS):is_element(LBFrom,
							   StateData#state.oor_unread_users);
				  first -> StateData#state.oor_unread == 0;
				  none -> false
				end,
		  Unread = StateData#state.oor_unread + 1,
		  SFrom = jlib:jid_to_string(BFrom),
		  Msg = if IncludeBody ->
			       CBody = utf8_cut(Body, 100),
			       case StateData#state.oor_send_from of
				 jid -> <<SFrom/binary, ": ", (iolist_to_binary(CBody))/binary>>;
				 username ->
				     UnescapedFrom = unescape(BFrom#jid.user),
				     <<UnescapedFrom/binary, ": ",
				       (iolist_to_binary(CBody))/binary>>;
				 name ->
				     Name = get_roster_name(StateData, BFrom),
				     <<Name/binary, ": ", (iolist_to_binary(CBody))/binary>>;
				 _ -> <<(iolist_to_binary(CBody))/binary>>
			       end;
			   true -> <<"">>
			end,
		  Sound = IncludeBody,
		  AppID = StateData#state.oor_appid,
		  ejabberd_hooks:run(p1_push_notification,
				     StateData#state.server,
				     [StateData#state.server,
				      StateData#state.jid,
				      StateData#state.oor_notification, Msg,
				      Unread +
					StateData#state.oor_unread_client,
				      Sound, AppID, SFrom]),
		  ejabberd_hooks:run(delayed_message_hook,
				     StateData#state.server,
				     [From, To, Packet]),
		  StateData#state{oor_unread = Unread,
				  oor_unread_users = UnreadUsers}
	   end;
       true -> StateData
    end;
send_out_of_reception_message(StateData, _From, _To,
			      _Packet) ->
    StateData.

make_oor_presence(StateData) ->
    make_oor_presence(StateData, [], []).

make_oor_presence(StateData, PresenceAttrs,
		  PresenceEls) ->
    ShowEl = case StateData#state.oor_show of
	       <<"available">> -> [];
	       _ ->
		   [#xmlel{name = <<"show">>, attrs = [],
			   children = [{xmlcdata, StateData#state.oor_show}]}]
	     end,
    #xmlel{name = <<"presence">>, attrs = PresenceAttrs,
	   children =
	       ShowEl ++
		 [#xmlel{name = <<"status">>, attrs = [],
			 children = [{xmlcdata, StateData#state.oor_status}]}]
		   ++ PresenceEls}.

utf8_cut(S, Bytes) -> utf8_cut(S, <<>>, <<>>, Bytes + 1).

utf8_cut(_S, _Cur, Prev, 0) -> Prev;
utf8_cut(<<>>, Cur, _Prev, _Bytes) -> Cur;
utf8_cut(<<C, S/binary>>, Cur, Prev, Bytes) ->
    if C bsr 6 == 2 ->
	   utf8_cut(S, <<Cur/binary, C>>, Prev, Bytes - 1);
       true -> utf8_cut(S, <<Cur/binary, C>>, Cur, Bytes - 1)
    end.

-include("mod_roster.hrl").

get_roster_name(StateData, JID) ->
    User = StateData#state.user,
    Server = StateData#state.server,
    RosterItems = ejabberd_hooks:run_fold(roster_get,
					  Server, [], [{User, Server}]),
    JUser = JID#jid.luser,
    JServer = JID#jid.lserver,
    Item = lists:foldl(fun (_, Res = #roster{}) -> Res;
			   (I, false) ->
			       case I#roster.jid of
				 {JUser, JServer, _} -> I;
				 _ -> false
			       end
		       end,
		       false, RosterItems),
    case Item of
      false -> unescape(JID#jid.user);
      #roster{} -> Item#roster.name
    end.

unescape(<<"">>) -> <<"">>;
unescape(<<"\\20", S/binary>>) ->
    <<"\s", (unescape(S))/binary>>;
unescape(<<"\\22", S/binary>>) ->
    <<"\"", (unescape(S))/binary>>;
unescape(<<"\\26", S/binary>>) ->
    <<"&", (unescape(S))/binary>>;
unescape(<<"\\27", S/binary>>) ->
    <<"'", (unescape(S))/binary>>;
unescape(<<"\\2f", S/binary>>) ->
    <<"/", (unescape(S))/binary>>;
unescape(<<"\\3a", S/binary>>) ->
    <<":", (unescape(S))/binary>>;
unescape(<<"\\3c", S/binary>>) ->
    <<"<", (unescape(S))/binary>>;
unescape(<<"\\3e", S/binary>>) ->
    <<">", (unescape(S))/binary>>;
unescape(<<"\\40", S/binary>>) ->
    <<"@", (unescape(S))/binary>>;
unescape(<<"\\5c", S/binary>>) ->
    <<"\\", (unescape(S))/binary>>;
unescape(<<C, S/binary>>) -> <<C, (unescape(S))/binary>>.

cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    receive {timeout, Timer, _} -> ok after 0 -> ok end.

enqueue(StateData, From, To, Packet) ->
    IsPresence = case Packet of
		   #xmlel{name = <<"presence">>} ->
		       case xml:get_tag_attr_s(<<"type">>, Packet) of
			 <<"subscribe">> -> false;
			 <<"subscribed">> -> false;
			 <<"unsubscribe">> -> false;
			 <<"unsubscribed">> -> false;
			 _ -> true
		       end;
		   _ -> false
		 end,
    Messages = StateData#state.queue_len +
		 gb_trees:size(StateData#state.pres_queue),
    if Messages >= (?MAX_OOR_MESSAGES) ->
	   self() ! {timeout, StateData#state.keepalive_timer, []};
       true -> ok
    end,
    if IsPresence ->
	   LFrom = jlib:jid_tolower(From),
	   case jlib:jid_tolower(StateData#state.jid) == LFrom of
	     true -> StateData;
	     false ->
		 NewQueue = gb_trees:enter(LFrom, Packet,
					   StateData#state.pres_queue),
		 StateData#state{pres_queue = NewQueue}
	   end;
       true ->
	   CleanPacket = xml:remove_subtags(Packet, <<"x">>,
					    {<<"xmlns">>, ?NS_P1_PUSHED}),
	   Packet2 = case CleanPacket of
		       #xmlel{name = <<"message">>} ->
			   xml:append_subtags(maybe_add_delay(CleanPacket, utc,
							      To, <<"">>),
					      [#xmlel{name = <<"x">>,
						      attrs =
							  [{<<"xmlns">>,
							    ?NS_P1_PUSHED}],
						      children = []}]);
		       _ -> Packet
		     end,
	   NewQueue = queue:in({From, To, Packet2},
			       StateData#state.queue),
	   NewQueueLen = StateData#state.queue_len + 1,
	   StateData#state{queue = NewQueue,
			   queue_len = NewQueueLen}
    end.

ack(StateData, From, To, Packet) ->
    if StateData#state.ack_enabled ->
	   NeedsAck = case Packet of
			#xmlel{name = <<"presence">>} ->
			    case xml:get_tag_attr_s(<<"type">>, Packet) of
			      <<"subscribe">> -> true;
			      <<"subscribed">> -> true;
			      <<"unsubscribe">> -> true;
			      <<"unsubscribed">> -> true;
			      _ -> false
			    end;
			#xmlel{name = <<"message">>} -> true;
			_ -> false
		      end,
	   if NeedsAck ->
		  Counter = StateData#state.ack_counter + 1,
		  NewAckQueue = queue:in({Counter, From, To, Packet},
					 StateData#state.ack_queue),
		  send_ack_request(StateData#state{ack_queue =
						       NewAckQueue,
						   ack_counter = Counter});
	      true -> StateData
	   end;
       true -> StateData
    end.

send_ack_request(StateData) ->
    case StateData#state.ack_timer of
      undefined ->
	  AckCounter = StateData#state.ack_counter,
	  AckTimer = erlang:start_timer(?C2S_P1_ACK_TIMEOUT,
					self(), AckCounter),
	  AckTimeout = StateData#state.keepalive_timeout +
			 StateData#state.oor_timeout,
	  erlang:send_after(AckTimeout * 1000, self(),
			    {ack_timeout, AckTimeout}),
	  send_element(StateData,
		       #xmlel{name = <<"r">>,
			      attrs =
				  [{<<"h">>,
				    iolist_to_binary(integer_to_list(AckCounter))}],
			      children = []}),
	  StateData#state{ack_timer = AckTimer};
      _ -> StateData
    end.

receive_ack(StateData, SCounter) ->
    case catch jlib:binary_to_integer(SCounter) of
      Counter when is_integer(Counter) ->
	  NewQueue = clean_queue(StateData#state.ack_queue,
				 Counter),
	  StateData#state{ack_queue = NewQueue};
      _ -> StateData
    end.

clean_queue(Queue, Counter) ->
    case queue:is_empty(Queue) of
      true -> Queue;
      false ->
	  C = element(1, queue:head(Queue)),
	  if C =< Counter ->
		 clean_queue(queue:tail(Queue), Counter);
	     true -> Queue
	  end
    end.

prepare_acks_for_rebind(StateData) ->
    AckQueue = StateData#state.ack_queue,
    case queue:is_empty(AckQueue) of
      true -> StateData;
      false ->
	  Unsent = lists:map(fun ({_Counter, From, To,
				   FixedPacket}) ->
				     {From, To, FixedPacket}
			     end,
			     queue:to_list(AckQueue)),
	  NewQueue = queue:join(queue:from_list(Unsent),
				StateData#state.queue),
	  StateData#state{queue = NewQueue,
			  queue_len = queue:len(NewQueue),
			  ack_queue = queue:new(), reception = false}
    end.

rebind(StateData, JID, StreamID) ->
    case JID#jid.lresource of
      <<"">> ->
	  send_element(StateData,
		       #xmlel{name = <<"failure">>,
			      attrs = [{<<"xmlns">>, ?NS_P1_REBIND}],
			      children = [{xmlcdata, <<"Invalid JID">>}]}),
	  fsm_next_state(wait_for_feature_request, StateData);
      _ ->
	  ejabberd_sm:route(jlib:make_jid(<<"">>, StateData#state.server, <<"">>),
                            JID, {broadcast, {rebind, self(), StreamID}}),
	  receive
	    {rebind, false} ->
		send_element(StateData,
			     #xmlel{name = <<"failure">>,
				    attrs = [{<<"xmlns">>, ?NS_P1_REBIND}],
				    children =
					[{xmlcdata, <<"Session not found">>}]}),
		fsm_next_state(wait_for_feature_request, StateData);
	    {rebind, NewStateData} ->
		?INFO_MSG("(~w) Reopened session for ~s",
			  [StateData#state.socket, jlib:jid_to_string(JID)]),
		SID = {now(), self()},
		StateData2 = NewStateData#state{socket =
						    StateData#state.socket,
						sockmod =
						    StateData#state.sockmod,
						socket_monitor =
						    StateData#state.socket_monitor,
						sid = SID,
						ip = StateData#state.ip,
						keepalive_timer =
						    StateData#state.keepalive_timer,
						ack_timer = undefined},
		send_element(StateData2,
			     #xmlel{name = <<"rebind">>,
				    attrs = [{<<"xmlns">>, ?NS_P1_REBIND}],
				    children = []}),
		maybe_migrate(session_established, StateData2)
	    after 1000 ->
		      send_element(StateData,
				   #xmlel{name = <<"failure">>,
					  attrs =
					      [{<<"xmlns">>, ?NS_P1_REBIND}],
					  children =
					      [{xmlcdata,
						<<"Session not found">>}]}),
		      fsm_next_state(wait_for_feature_request, StateData)
	  end
    end.

process_push_iq(From, To,
		#iq{type = _Type, sub_el = El} = IQ, StateData) ->
    {Res, NewStateData} = case El of
			    #xmlel{name = <<"push">>} ->
				SKeepAlive = xml:get_path_s(El,
							    [{elem,
							      <<"keepalive">>},
							     {attr,
							      <<"max">>}]),
				SOORTimeout = xml:get_path_s(El,
							     [{elem,
							       <<"session">>},
							      {attr,
							       <<"duration">>}]),
				Status = xml:get_path_s(El,
							[{elem, <<"status">>},
							 cdata]),
				Show = xml:get_path_s(El,
						      [{elem, <<"status">>},
						       {attr, <<"type">>}]),
				SSendBody = xml:get_path_s(El,
							   [{elem, <<"body">>},
							    {attr,
							     <<"send">>}]),
				SendBody = case SSendBody of
					     <<"all">> -> all;
					     <<"first-per-user">> ->
						 first_per_user;
					     <<"first">> -> first;
					     <<"none">> -> none;
					     _ -> none
					   end,
				SendGroupchat = xml:get_path_s(El,
							       [{elem,
								 <<"body">>},
								{attr,
								 <<"groupchat">>}])
						  == <<"true">>,
				SendFrom = send_from(El),
				AppID = xml:get_path_s(El,
						       [{elem, <<"appid">>},
							cdata]),
				{Offline, Keep} = case xml:get_path_s(El,
								      [{elem,
									<<"offline">>},
								       cdata])
						      of
						    <<"true">> -> {true, false};
						    <<"keep">> -> {false, true};
						    _ -> {false, false}
						  end,
				Notification1 = xml:get_path_s(El,
							       [{elem,
								 <<"notification">>}]),
				Notification = case Notification1 of
						 #xmlel{} -> Notification1;
						 _ ->
						     #xmlel{name =
								<<"notification">>,
							    attrs = [],
							    children =
								[#xmlel{name =
									    <<"type">>,
									attrs =
									    [],
									children
									    =
									    [{xmlcdata,
									      <<"none">>}]}]}
					       end,
				case catch
				       {jlib:binary_to_integer(SKeepAlive),
					jlib:binary_to_integer(SOORTimeout)}
				    of
				  {KeepAlive, OORTimeout}
				      when OORTimeout =< (?MAX_OOR_TIMEOUT) ->
				      if Offline ->
					     ejabberd_hooks:run(p1_push_enable_offline,
								StateData#state.server,
								[StateData#state.jid,
								 Notification,
								 SendBody,
								 SendFrom,
								 AppID]);
					 Keep -> ok;
					 true ->
					     ejabberd_hooks:run(p1_push_disable,
								StateData#state.server,
								[StateData#state.jid,
								 Notification,
								 AppID])
				      end,
				      NSD1 = StateData#state{keepalive_timeout =
								 KeepAlive,
							     oor_timeout =
								 OORTimeout *
								   60,
							     oor_status =
								 Status,
							     oor_show = Show,
							     oor_notification =
								 Notification,
							     oor_send_body =
								 SendBody,
							     oor_send_groupchat
								 =
								 SendGroupchat,
							     oor_send_from =
								 SendFrom,
							     oor_appid = AppID,
							     oor_offline =
								 Offline},
				      NSD2 = start_keepalive_timer(NSD1),
				      {{result, []}, NSD2};
				  _ -> {{error, ?ERR_BAD_REQUEST}, StateData}
				end;
			    #xmlel{name = <<"disable">>} ->
				ejabberd_hooks:run(p1_push_disable,
						   StateData#state.server,
						   [StateData#state.jid,
						    StateData#state.oor_notification,
						    StateData#state.oor_appid]),
				NSD1 = StateData#state{keepalive_timeout =
							   undefined,
						       oor_timeout = undefined,
						       oor_status = <<"">>,
						       oor_show = <<"">>,
						       oor_notification =
							   undefined,
						       oor_send_body = all},
				NSD2 = start_keepalive_timer(NSD1),
				{{result, []}, NSD2};
			    #xmlel{name = <<"badge">>} ->
				SBadge = xml:get_path_s(El,
							[{attr, <<"unread">>}]),
				Badge = case catch
					       jlib:binary_to_integer(SBadge)
					    of
					  B when is_integer(B) -> B;
					  _ -> 0
					end,
				NSD1 = StateData#state{oor_unread_client =
							   Badge},
				{{result, []}, NSD1};
			    _ -> {{error, ?ERR_BAD_REQUEST}, StateData}
			  end,
    IQRes = case Res of
	      {result, Result} ->
		  IQ#iq{type = result, sub_el = Result};
	      {error, Error} ->
		  IQ#iq{type = error, sub_el = [El, Error]}
	    end,
    ejabberd_router:route(To, From, jlib:iq_to_xml(IQRes)),
    NewStateData.

check_x_pushed(#xmlel{children = Els}) ->
    check_x_pushed1(Els).

check_x_pushed1([]) -> false;
check_x_pushed1([{xmlcdata, _} | Els]) ->
    check_x_pushed1(Els);
check_x_pushed1([El | Els]) ->
    case xml:get_tag_attr_s(<<"xmlns">>, El) of
      ?NS_P1_PUSHED -> true;
      _ -> check_x_pushed1(Els)
    end.

check_x_attachment(#xmlel{children = Els}) ->
    check_x_attachment1(Els).

check_x_attachment1([]) -> false;
check_x_attachment1([{xmlcdata, _} | Els]) ->
    check_x_attachment1(Els);
check_x_attachment1([El | Els]) ->
    case xml:get_tag_attr_s(<<"xmlns">>, El) of
      ?NS_P1_ATTACHMENT -> true;
      _ -> check_x_attachment1(Els)
    end.

maybe_add_delay(El, TZ, From, Desc) ->
    maybe_add_delay(El, TZ, From, Desc,
		    calendar:now_to_universal_time(now())).

maybe_add_delay(#xmlel{children = Els} = El, TZ, From,
		Desc, TimeStamp) ->
    HasOldTS = lists:any(fun (#xmlel{name = <<"x">>,
				     attrs = Attrs}) ->
				 xml:get_attr_s(<<"xmlns">>, Attrs) ==
				   (?NS_DELAY91);
			     (_) -> false
			 end,
			 Els),
    HasNewTS = lists:any(fun (#xmlel{name = <<"delay">>,
				     attrs = Attrs}) ->
				 xml:get_attr_s(<<"xmlns">>, Attrs) ==
				   (?NS_DELAY);
			     (_) -> false
			 end,
			 Els),
    El1 = if not HasOldTS ->
		 xml:append_subtags(El,
				    [jlib:timestamp_to_xml(TimeStamp)]);
	     true -> El
	  end,
    if not HasNewTS ->
	   xml:append_subtags(El1,
			      [jlib:timestamp_to_xml(TimeStamp, TZ, From,
						     Desc)]);
       true -> El1
    end.

send_from(El) ->
    case xml:get_path_s(El,
			[{elem, <<"body">>}, {attr, <<"jid">>}])
	of
      <<"false">> -> none;
      <<"true">> -> jid;
      <<"">> ->
	  case xml:get_path_s(El,
			      [{elem, <<"body">>}, {attr, <<"from">>}])
	      of
	    <<"jid">> -> jid;
	    <<"username">> -> username;
	    <<"name">> -> name;
	    <<"none">> -> none;
	    _ -> jid
	  end
    end.

fsm_limit_opts(Opts) ->
    case lists:keysearch(max_fsm_queue, 1, Opts) of
      {value, {_, N}} when is_integer(N) -> [{max_queue, N}];
      _ ->
	  case ejabberd_config:get_local_option(
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

flash_policy_string() ->
    Listen = ejabberd_config:get_local_option(listen, fun(V) -> V end),
    ClientPortsDeep = [<<",",
			 (iolist_to_binary(integer_to_list(Port)))/binary>>
		       || {{Port, _, _}, ejabberd_c2s, _Opts} <- Listen],
    ToPortsString = case iolist_to_binary(lists:flatten(ClientPortsDeep)) of
		      <<",", Tail/binary>> -> Tail;
		      _ -> <<>>
		    end,
    <<"<?xml version=\"1.0\"?>\n<!DOCTYPE cross-doma"
      "in-policy SYSTEM \"http://www.macromedia.com/"
      "xml/dtds/cross-domain-policy.dtd\">\n<cross-d"
      "omain-policy>\n  <allow-access-from "
      "domain=\"*\" to-ports=\"",
      ToPortsString/binary,
      "\"/>\n</cross-domain-policy>\n\000">>.

need_redirect(#state{redirect = true, user = User,
		     server = Server}) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    case ejabberd_cluster:get_node({LUser, LServer}) of
      Node when node() == Node -> false;
      Node ->
	  case rpc:call(Node, ejabberd_config, get_local_option,
			[hostname], 5000)
	      of
	    Host when is_binary(Host) -> {true, Host};
	    _ -> false
	  end
    end;
need_redirect(_) -> false.

get_jid_from_opts(Opts) ->
    case lists:keysearch(jid, 1, Opts) of
      {value, {_, JIDValue}} ->
	  JID = case JIDValue of
		  {_U, _S, _R} -> jlib:make_jid(JIDValue);
		  _ when is_list(JIDValue) ->
		      jlib:string_to_jid(list_to_binary(JIDValue));
		  _ when is_binary(JIDValue) ->
		      jlib:string_to_jid(JIDValue);
		  _ -> JIDValue
		end,
	  {ok, JID};
      _ -> error
    end.
