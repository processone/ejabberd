%%%-------------------------------------------------------------------
%%% Created :  8 Dec 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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
%%%-------------------------------------------------------------------
-module(ejabberd_c2s).
-behaviour(xmpp_stream_in).
-behaviour(ejabberd_config).
-behaviour(xmpp_socket).

-protocol({rfc, 6121}).

%% xmpp_socket callbacks
-export([start/2, start_link/2, socket_type/0]).
%% ejabberd_config callbacks
-export([opt_type/1, listen_opt_type/1, transform_listen_option/2]).
%% xmpp_stream_in callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).
-export([tls_options/1, tls_required/1, tls_verify/1, tls_enabled/1,
	 compress_methods/1, bind/2, sasl_mechanisms/2,
	 get_password_fun/1, check_password_fun/1, check_password_digest_fun/1,
	 unauthenticated_stream_features/1, authenticated_stream_features/1,
	 handle_stream_start/2, handle_stream_end/2,
	 handle_unauthenticated_packet/2, handle_authenticated_packet/2,
	 handle_auth_success/4, handle_auth_failure/4, handle_send/3,
	 handle_recv/3, handle_cdata/2, handle_unbinded_packet/2]).
%% Hooks
-export([handle_unexpected_cast/2,
	 reject_unauthenticated_packet/2, process_closed/2,
	 process_terminated/2, process_info/2]).
%% API
-export([get_presence/1, set_presence/2, resend_presence/1, resend_presence/2,
	 open_session/1, call/3, cast/2, send/2, close/1, close/2, stop/1,
	 reply/2, copy_state/2, set_timeout/2, route/2,
	 host_up/1, host_down/1]).

-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").
-include("mod_roster.hrl").

-define(SETS, gb_sets).

-type state() :: map().
-export_type([state/0]).

%%%===================================================================
%%% xmpp_socket API
%%%===================================================================
start(SockData, Opts) ->
    case proplists:get_value(supervisor, Opts, true) of
	true ->
	    case supervisor:start_child(ejabberd_c2s_sup, [SockData, Opts]) of
		{ok, undefined} -> ignore;
		Res -> Res
	    end;
	_ ->
	    xmpp_stream_in:start(?MODULE, [SockData, Opts],
				 ejabberd_config:fsm_limit_opts(Opts))
    end.

start_link(SockData, Opts) ->
    xmpp_stream_in:start_link(?MODULE, [SockData, Opts],
			      ejabberd_config:fsm_limit_opts(Opts)).

socket_type() ->
    xml_stream.

%%%===================================================================
%%% Common API
%%%===================================================================
-spec call(pid(), term(), non_neg_integer() | infinity) -> term().
call(Ref, Msg, Timeout) ->
    xmpp_stream_in:call(Ref, Msg, Timeout).

-spec cast(pid(), term()) -> ok.
cast(Ref, Msg) ->
    xmpp_stream_in:cast(Ref, Msg).

reply(Ref, Reply) ->
    xmpp_stream_in:reply(Ref, Reply).

-spec get_presence(pid()) -> presence().
get_presence(Ref) ->
    call(Ref, get_presence, 1000).

-spec set_presence(pid(), presence()) -> ok.
set_presence(Ref, Pres) ->
    call(Ref, {set_presence, Pres}, 1000).

-spec resend_presence(pid()) -> ok.
resend_presence(Pid) ->
    resend_presence(Pid, undefined).

-spec resend_presence(pid(), jid() | undefined) -> boolean().
resend_presence(Pid, To) ->
    route(Pid, {resend_presence, To}).

-spec close(pid()) -> ok;
	   (state()) -> state().
close(Ref) ->
    xmpp_stream_in:close(Ref).

-spec close(pid(), atom()) -> ok;
	   (state(), atom()) -> state().
close(Ref, Reason) ->
    xmpp_stream_in:close(Ref, Reason).

-spec stop(pid()) -> ok;
	  (state()) -> no_return().
stop(Ref) ->
    xmpp_stream_in:stop(Ref).

-spec send(pid(), xmpp_element()) -> ok;
	  (state(), xmpp_element()) -> state().
send(Pid, Pkt) when is_pid(Pid) ->
    xmpp_stream_in:send(Pid, Pkt);
send(#{lserver := LServer} = State, Pkt) ->
    Pkt1 = fix_from_to(Pkt, State),
    case ejabberd_hooks:run_fold(c2s_filter_send, LServer, {Pkt1, State}, []) of
	{drop, State1} -> State1;
	{Pkt2, State1} -> xmpp_stream_in:send(State1, Pkt2)
    end.

-spec send_error(state(), xmpp_element(), stanza_error()) -> state().
send_error(#{lserver := LServer} = State, Pkt, Err) ->
    case ejabberd_hooks:run_fold(c2s_filter_send, LServer, {Pkt, State}, []) of
	{drop, State1} -> State1;
	{Pkt1, State1} -> xmpp_stream_in:send_error(State1, Pkt1, Err)
    end.

-spec route(pid(), term()) -> boolean().
route(Pid, Term) ->
    ejabberd_cluster:send(Pid, Term).

-spec set_timeout(state(), timeout()) -> state().
set_timeout(State, Timeout) ->
    xmpp_stream_in:set_timeout(State, Timeout).

-spec host_up(binary()) -> ok.
host_up(Host) ->
    ejabberd_hooks:add(c2s_closed, Host, ?MODULE, process_closed, 100),
    ejabberd_hooks:add(c2s_terminated, Host, ?MODULE,
		       process_terminated, 100),
    ejabberd_hooks:add(c2s_unauthenticated_packet, Host, ?MODULE,
		       reject_unauthenticated_packet, 100),
    ejabberd_hooks:add(c2s_handle_info, Host, ?MODULE,
		       process_info, 100),
    ejabberd_hooks:add(c2s_handle_cast, Host, ?MODULE,
		       handle_unexpected_cast, 100).

-spec host_down(binary()) -> ok.
host_down(Host) ->
    ejabberd_hooks:delete(c2s_closed, Host, ?MODULE, process_closed, 100),
    ejabberd_hooks:delete(c2s_terminated, Host, ?MODULE,
			  process_terminated, 100),
    ejabberd_hooks:delete(c2s_unauthenticated_packet, Host, ?MODULE,
			  reject_unauthenticated_packet, 100),
    ejabberd_hooks:delete(c2s_handle_info, Host, ?MODULE,
			  process_info, 100),
    ejabberd_hooks:delete(c2s_handle_cast, Host, ?MODULE,
			  handle_unexpected_cast, 100).

%% Copies content of one c2s state to another.
%% This is needed for session migration from one pid to another.
-spec copy_state(state(), state()) -> state().
copy_state(#{owner := Owner} = NewState,
	   #{jid := JID, resource := Resource, sid := {Time, _},
	     auth_module := AuthModule, lserver := LServer,
	     pres_a := PresA} = OldState) ->
    State1 = case OldState of
		 #{pres_last := Pres, pres_timestamp := PresTS} ->
		     NewState#{pres_last => Pres, pres_timestamp => PresTS};
		 _ ->
		     NewState
	     end,
    Conn = get_conn_type(State1),
    State2 = State1#{jid => JID, resource => Resource,
		     conn => Conn,
		     sid => {Time, Owner},
		     auth_module => AuthModule,
		     pres_a => PresA},
    ejabberd_hooks:run_fold(c2s_copy_session, LServer, State2, [OldState]).

-spec open_session(state()) -> {ok, state()} | state().
open_session(#{user := U, server := S, resource := R,
	       sid := SID, ip := IP, auth_module := AuthModule} = State) ->
    JID = jid:make(U, S, R),
    State1 = change_shaper(State),
    Conn = get_conn_type(State1),
    State2 = State1#{conn => Conn, resource => R, jid => JID},
    Prio = case maps:get(pres_last, State, undefined) of
	       undefined -> undefined;
	       Pres -> get_priority_from_presence(Pres)
	   end,
    Info = [{ip, IP}, {conn, Conn}, {auth_module, AuthModule}],
    ejabberd_sm:open_session(SID, U, S, R, Prio, Info),
    xmpp_stream_in:establish(State2).

%%%===================================================================
%%% Hooks
%%%===================================================================
process_info(#{lserver := LServer} = State, {route, Packet}) ->
    {Pass, State1} = case Packet of
			 #presence{} ->
			     process_presence_in(State, Packet);
			 #message{} ->
			     process_message_in(State, Packet);
			 #iq{} ->
			     process_iq_in(State, Packet)
		     end,
    if Pass ->
	    {Packet1, State2} = ejabberd_hooks:run_fold(
				  user_receive_packet, LServer,
				  {Packet, State1}, []),
	    case Packet1 of
		drop -> State2;
		_ -> send(State2, Packet1)
	    end;
       true ->
	    State1
    end;
process_info(#{jid := JID} = State, {resend_presence, To}) ->
    case maps:get(pres_last, State, error) of
	error -> State;
	Pres when To == undefined ->
	    process_self_presence(State, Pres);
	Pres when To#jid.luser == JID#jid.luser andalso
		  To#jid.lserver == JID#jid.lserver andalso
		  To#jid.lresource == <<"">> ->
	    process_self_presence(State, Pres);
	Pres ->
	    process_presence_out(State, xmpp:set_to(Pres, To))
    end;
process_info(State, Info) ->
    ?WARNING_MSG("got unexpected info: ~p", [Info]),
    State.

handle_unexpected_cast(State, Msg) ->
    ?WARNING_MSG("got unexpected cast: ~p", [Msg]),
    State.

reject_unauthenticated_packet(State, _Pkt) ->
    Err = xmpp:serr_not_authorized(),
    send(State, Err).

process_closed(State, Reason) ->
    stop(State#{stop_reason => Reason}).

process_terminated(#{sid := SID, socket := Socket,
		     jid := JID, user := U, server := S, resource := R} = State,
		   Reason) ->
    Status = format_reason(State, Reason),
    ?INFO_MSG("(~s) Closing c2s session for ~s: ~s",
	      [xmpp_socket:pp(Socket), jid:encode(JID), Status]),
    State1 = case maps:is_key(pres_last, State) of
		 true ->
		     Pres = #presence{type = unavailable,
				      status = xmpp:mk_text(Status),
				      from = JID,
				      to = jid:remove_resource(JID)},
		     ejabberd_sm:close_session_unset_presence(SID, U, S, R,
							      Status),
		     broadcast_presence_unavailable(State, Pres);
		 false ->
		     ejabberd_sm:close_session(SID, U, S, R),
		     State
	     end,
    bounce_message_queue(),
    State1;
process_terminated(#{socket := Socket,
		     stop_reason := {tls, _}} = State, Reason) ->
    ?WARNING_MSG("(~s) Failed to secure c2s connection: ~s",
		 [xmpp_socket:pp(Socket), format_reason(State, Reason)]),
    State;
process_terminated(State, _Reason) ->
    State.

%%%===================================================================
%%% xmpp_stream_in callbacks
%%%===================================================================
tls_options(#{lserver := LServer, tls_options := DefaultOpts,
	      stream_encrypted := Encrypted}) ->
    TLSOpts1 = case {Encrypted, proplists:get_value(certfile, DefaultOpts)} of
		   {true, CertFile} when CertFile /= undefined -> DefaultOpts;
		   {_, _} ->
		       case get_certfile(LServer) of
			   undefined -> DefaultOpts;
			   CertFile -> lists:keystore(certfile, 1, DefaultOpts,
						      {certfile, CertFile})
		       end
	       end,
    TLSOpts2 = case ejabberd_config:get_option(
                      {c2s_ciphers, LServer}) of
                   undefined -> TLSOpts1;
                   Ciphers -> lists:keystore(ciphers, 1, TLSOpts1,
					     {ciphers, Ciphers})
               end,
    TLSOpts3 = case ejabberd_config:get_option(
                      {c2s_protocol_options, LServer}) of
                   undefined -> TLSOpts2;
                   ProtoOpts -> lists:keystore(protocol_options, 1, TLSOpts2,
					       {protocol_options, ProtoOpts})
               end,
    TLSOpts4 = case ejabberd_config:get_option(
                      {c2s_dhfile, LServer}) of
                   undefined -> TLSOpts3;
                   DHFile -> lists:keystore(dhfile, 1, TLSOpts3,
					    {dhfile, DHFile})
               end,
    TLSOpts5 = case ejabberd_config:get_option(
		      {c2s_cafile, LServer}) of
		   undefined -> TLSOpts4;
		   CAFile -> lists:keystore(cafile, 1, TLSOpts4,
					    {cafile, CAFile})
	       end,
    case ejabberd_config:get_option({c2s_tls_compression, LServer}) of
	undefined -> TLSOpts5;
	false -> [compression_none | TLSOpts5];
	true -> lists:delete(compression_none, TLSOpts5)
    end.

tls_required(#{tls_required := TLSRequired}) ->
    TLSRequired.

tls_verify(#{tls_verify := TLSVerify}) ->
    TLSVerify.

tls_enabled(#{tls_enabled := TLSEnabled,
	      tls_required := TLSRequired,
	      tls_verify := TLSVerify}) ->
    TLSEnabled or TLSRequired or TLSVerify.

compress_methods(#{zlib := true}) ->
    [<<"zlib">>];
compress_methods(_) ->
    [].

unauthenticated_stream_features(#{lserver := LServer}) ->
    ejabberd_hooks:run_fold(c2s_pre_auth_features, LServer, [], [LServer]).

authenticated_stream_features(#{lserver := LServer}) ->
    ejabberd_hooks:run_fold(c2s_post_auth_features, LServer, [], [LServer]).

sasl_mechanisms(Mechs, #{lserver := LServer}) ->
    Mechs1 = ejabberd_config:get_option({disable_sasl_mechanisms, LServer}, []),
    Mechs2 = case ejabberd_auth_anonymous:is_sasl_anonymous_enabled(LServer) of
		 true -> Mechs1;
		 false -> [<<"ANONYMOUS">>|Mechs1]
	     end,
    Mechs -- Mechs2.

get_password_fun(#{lserver := LServer}) ->
    fun(U) ->
	    ejabberd_auth:get_password_with_authmodule(U, LServer)
    end.

check_password_fun(#{lserver := LServer}) ->
    fun(U, AuthzId, P) ->
	    ejabberd_auth:check_password_with_authmodule(U, AuthzId, LServer, P)
    end.

check_password_digest_fun(#{lserver := LServer}) ->
    fun(U, AuthzId, P, D, DG) ->
	    ejabberd_auth:check_password_with_authmodule(U, AuthzId, LServer, P, D, DG)
    end.

bind(<<"">>, State) ->
    bind(new_uniq_id(), State);
bind(R, #{user := U, server := S, access := Access, lang := Lang,
	  lserver := LServer, socket := Socket,
	  ip := IP} = State) ->
    case resource_conflict_action(U, S, R) of
	closenew ->
	    {error, xmpp:err_conflict(), State};
	{accept_resource, Resource} ->
	    JID = jid:make(U, S, Resource),
	    case acl:access_matches(Access,
				    #{usr => jid:split(JID), ip => IP},
				    LServer) of
		allow ->
		    State1 = open_session(State#{resource => Resource,
						 sid => ejabberd_sm:make_sid()}),
		    State2 = ejabberd_hooks:run_fold(
			       c2s_session_opened, LServer, State1, []),
		    ?INFO_MSG("(~s) Opened c2s session for ~s",
			      [xmpp_socket:pp(Socket), jid:encode(JID)]),
		    {ok, State2};
		deny ->
		    ejabberd_hooks:run(forbidden_session_hook, LServer, [JID]),
		    ?INFO_MSG("(~s) Forbidden c2s session for ~s",
			      [xmpp_socket:pp(Socket), jid:encode(JID)]),
		    Txt = <<"Access denied by service policy">>,
		    {error, xmpp:err_not_allowed(Txt, Lang), State}
	    end
    end.

handle_stream_start(StreamStart, #{lserver := LServer} = State) ->
    case ejabberd_router:is_my_host(LServer) of
	false ->
	    send(State#{lserver => ?MYNAME}, xmpp:serr_host_unknown());
	true ->
	    State1 = change_shaper(State),
	    Opts = ejabberd_config:codec_options(LServer),
	    State2 = State1#{codec_options => Opts},
	    ejabberd_hooks:run_fold(
	      c2s_stream_started, LServer, State2, [StreamStart])
    end.

handle_stream_end(Reason, #{lserver := LServer} = State) ->
    State1 = State#{stop_reason => Reason},
    ejabberd_hooks:run_fold(c2s_closed, LServer, State1, [Reason]).

handle_auth_success(User, Mech, AuthModule,
		    #{socket := Socket,
		      ip := IP, lserver := LServer} = State) ->
    ?INFO_MSG("(~s) Accepted c2s ~s authentication for ~s@~s by ~s backend from ~s",
	      [xmpp_socket:pp(Socket), Mech, User, LServer,
	       ejabberd_auth:backend_type(AuthModule),
	       ejabberd_config:may_hide_data(misc:ip_to_list(IP))]),
    State1 = State#{auth_module => AuthModule},
    ejabberd_hooks:run_fold(c2s_auth_result, LServer, State1, [true, User]).

handle_auth_failure(User, Mech, Reason,
		    #{socket := Socket,
		      ip := IP, lserver := LServer} = State) ->
    ?INFO_MSG("(~s) Failed c2s ~s authentication ~sfrom ~s: ~s",
	      [xmpp_socket:pp(Socket), Mech,
	       if User /= <<"">> -> ["for ", User, "@", LServer, " "];
		  true -> ""
	       end,
	       ejabberd_config:may_hide_data(misc:ip_to_list(IP)), Reason]),
    ejabberd_hooks:run_fold(c2s_auth_result, LServer, State, [false, User]).

handle_unbinded_packet(Pkt, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_unbinded_packet, LServer, State, [Pkt]).

handle_unauthenticated_packet(Pkt, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_unauthenticated_packet, LServer, State, [Pkt]).

handle_authenticated_packet(Pkt, #{lserver := LServer} = State) when not ?is_stanza(Pkt) ->
    ejabberd_hooks:run_fold(c2s_authenticated_packet,
			    LServer, State, [Pkt]);
handle_authenticated_packet(Pkt, #{lserver := LServer, jid := JID,
				   ip := {IP, _}} = State) ->
    Pkt1 = xmpp:put_meta(Pkt, ip, IP),
    State1 = ejabberd_hooks:run_fold(c2s_authenticated_packet,
				     LServer, State, [Pkt1]),
    #jid{luser = LUser} = JID,
    {Pkt2, State2} = ejabberd_hooks:run_fold(
		       user_send_packet, LServer, {Pkt1, State1}, []),
    case Pkt2 of
	drop ->
	    State2;
	#iq{type = set, sub_els = [_]} ->
	    try xmpp:try_subtag(Pkt2, #xmpp_session{}) of
		#xmpp_session{} ->
		    send(State2, xmpp:make_iq_result(Pkt2));
		_ ->
		    check_privacy_then_route(State2, Pkt2)
	    catch _:{xmpp_codec, Why} ->
		    Txt = xmpp:io_format_error(Why),
		    Lang = maps:get(lang, State),
		    Err = xmpp:err_bad_request(Txt, Lang),
		    send_error(State2, Pkt2, Err)
	    end;
	#presence{to = #jid{luser = LUser, lserver = LServer,
			    lresource = <<"">>}} ->
	    process_self_presence(State2, Pkt2);
	#presence{} ->
	    process_presence_out(State2, Pkt2);
	_ ->
	    check_privacy_then_route(State2, Pkt2)
    end.

handle_cdata(Data, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_handle_cdata, LServer,
			    State, [Data]).

handle_recv(El, Pkt, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_handle_recv, LServer, State, [El, Pkt]).

handle_send(Pkt, Result, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_handle_send, LServer, State, [Pkt, Result]).

init([State, Opts]) ->
    Access = proplists:get_value(access, Opts, all),
    Shaper = proplists:get_value(shaper, Opts, none),
    TLSOpts1 = lists:filter(
		 fun({certfile, _}) -> true;
		    ({ciphers, _}) -> true;
		    ({dhfile, _}) -> true;
		    ({cafile, _}) -> true;
		    ({protocol_options, _}) -> true;
		    (_) -> false
		 end, Opts),
    TLSOpts2 = case proplists:get_bool(tls_compression, Opts) of
                   false -> [compression_none | TLSOpts1];
                   true -> TLSOpts1
               end,
    TLSEnabled = proplists:get_bool(starttls, Opts),
    TLSRequired = proplists:get_bool(starttls_required, Opts),
    TLSVerify = proplists:get_bool(tls_verify, Opts),
    Zlib = proplists:get_bool(zlib, Opts),
    Timeout = ejabberd_config:negotiation_timeout(),
    State1 = State#{tls_options => TLSOpts2,
		    tls_required => TLSRequired,
		    tls_enabled => TLSEnabled,
		    tls_verify => TLSVerify,
		    pres_a => ?SETS:new(),
		    zlib => Zlib,
		    lang => ?MYLANG,
		    server => ?MYNAME,
		    lserver => ?MYNAME,
		    access => Access,
		    shaper => Shaper},
    State2 = xmpp_stream_in:set_timeout(State1, Timeout),
    ejabberd_hooks:run_fold(c2s_init, {ok, State2}, [Opts]).

handle_call(get_presence, From, #{jid := JID} = State) ->
    Pres = case maps:get(pres_last, State, error) of
	       error ->
		   BareJID = jid:remove_resource(JID),
		   #presence{from = JID, to = BareJID, type = unavailable};
	       P -> P
	   end,
    reply(From, Pres),
    State;
handle_call({set_presence, Pres}, From, State) ->
    reply(From, ok),
    process_self_presence(State, Pres);
handle_call(Request, From, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(
      c2s_handle_call, LServer, State, [Request, From]).

handle_cast(Msg, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_handle_cast, LServer, State, [Msg]).

handle_info(Info, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_handle_info, LServer, State, [Info]).

terminate(Reason, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_terminated, LServer, State, [Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec process_iq_in(state(), iq()) -> {boolean(), state()}.
process_iq_in(State, #iq{} = IQ) ->
    case privacy_check_packet(State, IQ, in) of
	allow ->
	    {true, State};
	deny ->
	    ejabberd_router:route_error(IQ, xmpp:err_service_unavailable()),
	    {false, State}
    end.

-spec process_message_in(state(), message()) -> {boolean(), state()}.
process_message_in(State, #message{type = T} = Msg) ->
    %% This function should be as simple as process_iq_in/2,
    %% however, we don't route errors to MUC rooms in order
    %% to avoid kicking us, because having a MUC room's JID blocked
    %% most likely means having only some particular participant
    %% blocked, i.e. room@conference.server.org/participant.
    case privacy_check_packet(State, Msg, in) of
	allow ->
	    {true, State};
	deny when T == groupchat; T == headline ->
	    {false, State};
	deny ->
	    case xmpp:has_subtag(Msg, #muc_user{}) of
		true ->
		    ok;
		false ->
		    ejabberd_router:route_error(
		      Msg, xmpp:err_service_unavailable())
	    end,
	    {false, State}
    end.

-spec process_presence_in(state(), presence()) -> {boolean(), state()}.
process_presence_in(#{lserver := LServer, pres_a := PresA} = State0,
		    #presence{from = From, type = T} = Pres) ->
    State = ejabberd_hooks:run_fold(c2s_presence_in, LServer, State0, [Pres]),
    case T of
	probe ->
	    route_probe_reply(From, State),
	    {false, State};
	error ->
	    A = ?SETS:del_element(jid:tolower(From), PresA),
	    {true, State#{pres_a => A}};
	_ ->
	    case privacy_check_packet(State, Pres, in) of
		allow ->
		    {true, State};
		deny ->
		    {false, State}
	    end
    end.

-spec route_probe_reply(jid(), state()) -> ok.
route_probe_reply(From, #{jid := To,
			  pres_last := LastPres,
			  pres_timestamp := TS} = State) ->
    {LUser, LServer, LResource} = jid:tolower(To),
    IsAnotherResource = case jid:tolower(From) of
			    {LUser, LServer, R} when R /= LResource -> true;
			    _ -> false
			end,
    Subscription = get_subscription(To, From),
    if IsAnotherResource orelse
       Subscription == both orelse Subscription == from ->
	    Packet = xmpp_util:add_delay_info(LastPres, To, TS),
	    case privacy_check_packet(State, Packet, out) of
		deny ->
		    ok;
		allow ->
		    ejabberd_hooks:run(presence_probe_hook,
				       LServer,
				       [From, To, self()]),
		    ejabberd_router:route(xmpp:set_from_to(Packet, To, From))
	    end;
       true ->
	    ok
    end;
route_probe_reply(_, _) ->
    ok.

-spec process_presence_out(state(), presence()) -> state().
process_presence_out(#{lserver := LServer, jid := JID,
		       lang := Lang, pres_a := PresA} = State,
		     #presence{from = From, to = To, type = Type} = Pres) ->
    if Type == subscribe; Type == subscribed;
       Type == unsubscribe; Type == unsubscribed ->
	    Access = gen_mod:get_module_opt(LServer, mod_roster, access),
	    MyBareJID = jid:remove_resource(JID),
	    case acl:match_rule(LServer, Access, MyBareJID) of
		deny ->
		    AccessErrTxt = <<"Access denied by service policy">>,
		    AccessErr = xmpp:err_forbidden(AccessErrTxt, Lang),
		    send_error(State, Pres, AccessErr);
		allow ->
		    ejabberd_hooks:run(roster_out_subscription, LServer, [Pres])
	    end;
	true -> ok
    end,
    case privacy_check_packet(State, Pres, out) of
	deny ->
	    PrivErrTxt = <<"Your active privacy list has denied "
			   "the routing of this stanza.">>,
	    PrivErr = xmpp:err_not_acceptable(PrivErrTxt, Lang),
	    send_error(State, Pres, PrivErr);
	allow when Type == subscribe; Type == subscribed;
		   Type == unsubscribe; Type == unsubscribed ->
	    BareFrom = jid:remove_resource(From),
	    ejabberd_router:route(xmpp:set_from_to(Pres, BareFrom, To)),
	    State;
	allow when Type == error; Type == probe ->
	    ejabberd_router:route(Pres),
	    State;
	allow ->
	    ejabberd_router:route(Pres),
	    LTo = jid:tolower(To),
	    LBareTo = jid:remove_resource(LTo),
	    LBareFrom = jid:remove_resource(jid:tolower(From)),
	    if LBareTo /= LBareFrom ->
		    Subscription = get_subscription(From, To),
		    if Subscription /= both andalso Subscription /= from ->
			    A = case Type of
				    available -> ?SETS:add_element(LTo, PresA);
				    unavailable -> ?SETS:del_element(LTo, PresA)
				end,
			    State#{pres_a => A};
		       true ->
			    State
		    end;
	       true ->
		    State
	    end
    end.

-spec process_self_presence(state(), presence()) -> state().
process_self_presence(#{ip := IP, conn := Conn, lserver := LServer,
			auth_module := AuthMod, sid := SID,
			user := U, server := S,	resource := R} = State,
		      #presence{type = unavailable} = Pres) ->
    Status = xmpp:get_text(Pres#presence.status),
    Info = [{ip, IP}, {conn, Conn}, {auth_module, AuthMod}],
    ejabberd_sm:unset_presence(SID, U, S, R, Status, Info),
    {Pres1, State1} = ejabberd_hooks:run_fold(
			c2s_self_presence, LServer, {Pres, State}, []),
    State2 = broadcast_presence_unavailable(State1, Pres1),
    maps:remove(pres_last, maps:remove(pres_timestamp, State2));
process_self_presence(#{lserver := LServer} = State,
		      #presence{type = available} = Pres) ->
    PreviousPres = maps:get(pres_last, State, undefined),
    update_priority(State, Pres),
    {Pres1, State1} = ejabberd_hooks:run_fold(
			c2s_self_presence, LServer, {Pres, State}, []),
    State2 = State1#{pres_last => Pres1,
		     pres_timestamp => p1_time_compat:timestamp()},
    FromUnavailable = PreviousPres == undefined,
    broadcast_presence_available(State2, Pres1, FromUnavailable);
process_self_presence(State, _Pres) ->
    State.

-spec update_priority(state(), presence()) -> ok.
update_priority(#{ip := IP, conn := Conn, auth_module := AuthMod,
		  sid := SID, user := U, server := S, resource := R},
		Pres) ->
    Priority = get_priority_from_presence(Pres),
    Info = [{ip, IP}, {conn, Conn}, {auth_module, AuthMod}],
    ejabberd_sm:set_presence(SID, U, S, R, Priority, Pres, Info).

-spec broadcast_presence_unavailable(state(), presence()) -> state().
broadcast_presence_unavailable(#{jid := JID, pres_a := PresA} = State, Pres) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    BareJID = jid:remove_resource(JID),
    Items1 = ejabberd_hooks:run_fold(roster_get, LServer,
				     [], [{LUser, LServer}]),
    Items2 = ?SETS:fold(
		fun(LJID, Items) ->
			[#roster{jid = LJID, subscription = from}|Items]
		end, Items1, PresA),
    JIDs = lists:foldl(
	     fun(#roster{jid = LJID, subscription = Sub}, Tos)
		   when Sub == both orelse Sub == from ->
		     To = jid:make(LJID),
		     P = xmpp:set_to(Pres, jid:make(LJID)),
		     case privacy_check_packet(State, P, out) of
			 allow -> [To|Tos];
			 deny -> Tos
		     end;
		(_, Tos) ->
		     Tos
	     end, [BareJID], Items2),
    route_multiple(State, JIDs, Pres),
    State#{pres_a => ?SETS:new()}.

-spec broadcast_presence_available(state(), presence(), boolean()) -> state().
broadcast_presence_available(#{jid := JID} = State,
			     Pres, _FromUnavailable = true) ->
    Probe = #presence{from = JID, type = probe},
    #jid{luser = LUser, lserver = LServer} = JID,
    BareJID = jid:remove_resource(JID),
    Items = ejabberd_hooks:run_fold(roster_get, LServer,
				    [], [{LUser, LServer}]),
    {FJIDs, TJIDs} =
	lists:foldl(
	  fun(#roster{jid = LJID, subscription = Sub}, {F, T}) ->
		  To = jid:make(LJID),
		  F1 = if Sub == both orelse Sub == from ->
			       Pres1 = xmpp:set_to(Pres, To),
			       case privacy_check_packet(State, Pres1, out) of
				   allow -> [To|F];
				   deny -> F
			       end;
			  true -> F
		       end,
		  T1 = if Sub == both orelse Sub == to ->
			       Probe1 = xmpp:set_to(Probe, To),
			       case privacy_check_packet(State, Probe1, out) of
				   allow -> [To|T];
				   deny -> T
			       end;
			  true -> T
		       end,
		  {F1, T1}
	  end, {[BareJID], [BareJID]}, Items),
    route_multiple(State, TJIDs, Probe),
    route_multiple(State, FJIDs, Pres),
    State;
broadcast_presence_available(#{jid := JID} = State,
			     Pres, _FromUnavailable = false) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    BareJID = jid:remove_resource(JID),
    Items = ejabberd_hooks:run_fold(
	      roster_get, LServer, [], [{LUser, LServer}]),
    JIDs = lists:foldl(
	     fun(#roster{jid = LJID, subscription = Sub}, Tos)
		   when Sub == both orelse Sub == from ->
		     To = jid:make(LJID),
		     P = xmpp:set_to(Pres, jid:make(LJID)),
		     case privacy_check_packet(State, P, out) of
			 allow -> [To|Tos];
			 deny -> Tos
		     end;
		(_, Tos) ->
		     Tos
	     end, [BareJID], Items),
    route_multiple(State, JIDs, Pres),
    State.

-spec check_privacy_then_route(state(), stanza()) -> state().
check_privacy_then_route(#{lang := Lang} = State, Pkt) ->
    case privacy_check_packet(State, Pkt, out) of
        deny ->
            ErrText = <<"Your active privacy list has denied "
			"the routing of this stanza.">>,
	    Err = xmpp:err_not_acceptable(ErrText, Lang),
	    send_error(State, Pkt, Err);
        allow ->
	    ejabberd_router:route(Pkt),
	    State
    end.

-spec privacy_check_packet(state(), stanza(), in | out) -> allow | deny.
privacy_check_packet(#{lserver := LServer} = State, Pkt, Dir) ->
    ejabberd_hooks:run_fold(privacy_check_packet, LServer, allow, [State, Pkt, Dir]).

-spec get_priority_from_presence(presence()) -> integer().
get_priority_from_presence(#presence{priority = Prio}) ->
    case Prio of
	undefined -> 0;
	_ -> Prio
    end.

-spec route_multiple(state(), [jid()], stanza()) -> ok.
route_multiple(#{lserver := LServer}, JIDs, Pkt) ->
    From = xmpp:get_from(Pkt),
    ejabberd_router_multicast:route_multicast(From, LServer, JIDs, Pkt).

get_subscription(#jid{luser = LUser, lserver = LServer}, JID) ->
    {Subscription, _, _} = ejabberd_hooks:run_fold(
			     roster_get_jid_info, LServer, {none, none, []},
			     [LUser, LServer, JID]),
    Subscription.

-spec resource_conflict_action(binary(), binary(), binary()) ->
				      {accept_resource, binary()} | closenew.
resource_conflict_action(U, S, R) ->
    OptionRaw = case ejabberd_sm:is_existing_resource(U, S, R) of
		    true ->
			ejabberd_config:get_option(
			  {resource_conflict, S}, acceptnew);
		    false ->
			acceptnew
		end,
    Option = case OptionRaw of
		 setresource -> setresource;
		 closeold -> acceptnew; %% ejabberd_sm will close old session
		 closenew -> closenew;
		 acceptnew -> acceptnew
	     end,
    case Option of
	acceptnew -> {accept_resource, R};
	closenew -> closenew;
	setresource ->
	    Rnew = new_uniq_id(),
	    {accept_resource, Rnew}
    end.

-spec bounce_message_queue() -> ok.
bounce_message_queue() ->
    receive {route, Pkt} ->
	    ejabberd_router:route(Pkt),
	    bounce_message_queue()
    after 0 ->
	    ok
    end.

-spec new_uniq_id() -> binary().
new_uniq_id() ->
    iolist_to_binary(
      [randoms:get_string(),
       integer_to_binary(p1_time_compat:unique_integer([positive]))]).

-spec get_conn_type(state()) -> c2s | c2s_tls | c2s_compressed | websocket |
				c2s_compressed_tls | http_bind.
get_conn_type(State) ->
    case xmpp_stream_in:get_transport(State) of
	tcp -> c2s;
	tls -> c2s_tls;
	tcp_zlib -> c2s_compressed;
	tls_zlib -> c2s_compressed_tls;
	http_bind -> http_bind;
	websocket -> websocket
    end.

-spec fix_from_to(xmpp_element(), state()) -> stanza().
fix_from_to(Pkt, #{jid := JID}) when ?is_stanza(Pkt) ->
    #jid{luser = U, lserver = S, lresource = R} = JID,
    case xmpp:get_from(Pkt) of
	undefined ->
	    Pkt;
	From ->
	    From1 = case jid:tolower(From) of
			{U, S, R} -> JID;
			{U, S, _} -> jid:replace_resource(JID, From#jid.resource);
			_ -> From
		    end,
	    xmpp:set_from_to(Pkt, From1, JID)
    end;
fix_from_to(Pkt, _State) ->
    Pkt.

-spec change_shaper(state()) -> state().
change_shaper(#{shaper := ShaperName, ip := IP, lserver := LServer,
		user := U, server := S, resource := R} = State) ->
    JID = jid:make(U, S, R),
    Shaper = acl:access_matches(ShaperName,
				#{usr => jid:split(JID), ip => IP},
				LServer),
    xmpp_stream_in:change_shaper(State, Shaper).

-spec format_reason(state(), term()) -> binary().
format_reason(#{stop_reason := Reason}, _) ->
    xmpp_stream_in:format_error(Reason);
format_reason(_, normal) ->
    <<"unknown reason">>;
format_reason(_, shutdown) ->
    <<"stopped by supervisor">>;
format_reason(_, {shutdown, _}) ->
    <<"stopped by supervisor">>;
format_reason(_, _) ->
    <<"internal server error">>.

-spec get_certfile(binary()) -> file:filename_all().
get_certfile(LServer) ->
    case ejabberd_pkix:get_certfile(LServer) of
	{ok, CertFile} ->
	    CertFile;
	error ->
	    ejabberd_config:get_option(
	      {domain_certfile, LServer},
	      ejabberd_config:get_option({c2s_certfile, LServer}))
    end.

transform_listen_option(Opt, Opts) ->
    [Opt|Opts].

-type resource_conflict() :: setresource | closeold | closenew | acceptnew.
-spec opt_type(c2s_ciphers) -> fun((binary()) -> binary());
	      (c2s_dhfile) -> fun((binary()) -> binary());
	      (c2s_cafile) -> fun((binary()) -> binary());
	      (c2s_protocol_options) -> fun(([binary()]) -> binary());
	      (c2s_tls_compression) -> fun((boolean()) -> boolean());
	      (resource_conflict) -> fun((resource_conflict()) -> resource_conflict());
	      (disable_sasl_mechanisms) -> fun((binary() | [binary()]) -> [binary()]);
	      (atom()) -> [atom()].
opt_type(c2s_ciphers) -> fun iolist_to_binary/1;
opt_type(c2s_dhfile) -> fun misc:try_read_file/1;
opt_type(c2s_cafile) -> fun misc:try_read_file/1;
opt_type(c2s_protocol_options) ->
    fun (Options) -> str:join(Options, <<"|">>) end;
opt_type(c2s_tls_compression) ->
    fun (true) -> true;
	(false) -> false
    end;
opt_type(resource_conflict) ->
    fun (setresource) -> setresource;
	(closeold) -> closeold;
	(closenew) -> closenew;
	(acceptnew) -> acceptnew
    end;
opt_type(disable_sasl_mechanisms) ->
    fun (V) when is_list(V) ->
	    lists:map(fun (M) -> str:to_upper(M) end, V);
	(V) -> [str:to_upper(V)]
    end;
opt_type(_) ->
    [c2s_ciphers, c2s_cafile, c2s_dhfile,
     c2s_protocol_options, c2s_tls_compression, resource_conflict,
     disable_sasl_mechanisms].

-spec listen_opt_type(access) -> fun((any()) -> any());
		     (shaper) -> fun((any()) -> any());
		     (certfile) -> fun((binary()) -> binary());
		     (ciphers) -> fun((binary()) -> binary());
		     (dhfile) -> fun((binary()) -> binary());
		     (cafile) -> fun((binary()) -> binary());
		     (protocol_options) -> fun(([binary()]) -> binary());
		     (tls_compression) -> fun((boolean()) -> boolean());
		     (tls) -> fun((boolean()) -> boolean());
		     (starttls) -> fun((boolean()) -> boolean());
		     (tls_verify) -> fun((boolean()) -> boolean());
		     (zlib) -> fun((boolean()) -> boolean());
		     (supervisor) -> fun((boolean()) -> boolean());
		     (max_stanza_size) -> fun((timeout()) -> timeout());
		     (max_fsm_queue) -> fun((timeout()) -> timeout());
		     (stream_management) -> fun((boolean()) -> boolean());
		     (inet) -> fun((boolean()) -> boolean());
		     (inet6) -> fun((boolean()) -> boolean());
		     (backlog) -> fun((timeout()) -> timeout());
		     (atom()) -> [atom()].
listen_opt_type(access) -> fun acl:access_rules_validator/1;
listen_opt_type(shaper) -> fun acl:shaper_rules_validator/1;
listen_opt_type(certfile = Opt) ->
    fun(S) ->
	    ?WARNING_MSG("Listening option '~s' for ~s is deprecated, use "
			 "'certfiles' global option instead", [Opt, ?MODULE]),
	    ejabberd_pkix:add_certfile(S),
	    iolist_to_binary(S)
    end;
listen_opt_type(ciphers) -> opt_type(c2s_ciphers);
listen_opt_type(dhfile) -> opt_type(c2s_dhfile);
listen_opt_type(cafile) -> opt_type(c2s_cafile);
listen_opt_type(protocol_options) -> opt_type(c2s_protocol_options);
listen_opt_type(tls_compression) -> opt_type(c2s_tls_compression);
listen_opt_type(tls) -> fun(B) when is_boolean(B) -> B end;
listen_opt_type(starttls) -> fun(B) when is_boolean(B) -> B end;
listen_opt_type(starttls_required) -> fun(B) when is_boolean(B) -> B end;
listen_opt_type(tls_verify) -> fun(B) when is_boolean(B) -> B end;
listen_opt_type(zlib) -> fun(B) when is_boolean(B) -> B end;
listen_opt_type(supervisor) -> fun(B) when is_boolean(B) -> B end;
listen_opt_type(max_stanza_size) ->
    fun(I) when is_integer(I), I>0 -> I;
       (unlimited) -> infinity;
       (infinity) -> infinity
    end;
listen_opt_type(max_fsm_queue) ->
    fun(I) when is_integer(I), I>0 -> I end;
listen_opt_type(stream_management) ->
    ?ERROR_MSG("listening option 'stream_management' is ignored: "
	       "use mod_stream_mgmt module", []),
    fun(B) when is_boolean(B) -> B end;
listen_opt_type(inet) -> fun(B) when is_boolean(B) -> B end;
listen_opt_type(inet6) -> fun(B) when is_boolean(B) -> B end;
listen_opt_type(backlog) ->
    fun(I) when is_integer(I), I>0 -> I end;
listen_opt_type(O) ->
    StreamOpts = mod_stream_mgmt:mod_options(?MYNAME),
    case lists:keyfind(O, 1, StreamOpts) of
	false ->
	    [access, shaper, certfile, ciphers, dhfile, cafile,
	     protocol_options, tls, tls_compression, starttls,
	     starttls_required, tls_verify, zlib, max_fsm_queue,
	     backlog, inet, inet6];
	_ ->
	    ?ERROR_MSG("Listening option '~s' is ignored: use '~s' "
		       "option from mod_stream_mgmt module", [O, O]),
	    mod_stream_mgmt:mod_opt_type(O)
    end.
