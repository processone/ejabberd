%%%-------------------------------------------------------------------
%%% Created :  8 Dec 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
-behaviour(ejabberd_listener).

-protocol({rfc, 3920}).
-protocol({rfc, 3921}).
-protocol({rfc, 6120}).
-protocol({rfc, 6121}).
-protocol({xep, 138, '2.1', '1.1.0', "complete", ""}).

%% ejabberd_listener callbacks
-export([start/3, start_link/3, accept/1, listen_opt_type/1, listen_options/0]).
%% xmpp_stream_in callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).
-export([tls_options/1, tls_required/1, tls_enabled/1,
	 allow_unencrypted_sasl2/1, compress_methods/1, bind/2,
	 sasl_mechanisms/2, get_password_fun/2, check_password_fun/2,
	 check_password_digest_fun/2, unauthenticated_stream_features/1,
	 authenticated_stream_features/1, handle_stream_start/2,
	 handle_stream_end/2, handle_unauthenticated_packet/2,
	 handle_authenticated_packet/2, handle_auth_success/4,
	 handle_auth_failure/4, handle_send/3, handle_recv/3, handle_cdata/2,
	 handle_unbinded_packet/2, inline_stream_features/1,
	 handle_sasl2_inline/2, handle_sasl2_inline_post/3,
	 handle_bind2_inline/2, handle_bind2_inline_post/3, sasl_options/1,
	 handle_sasl2_task_next/4, handle_sasl2_task_data/3,
	 get_fast_tokens_fun/2, fast_mechanisms/1]).
%% Hooks
-export([handle_unexpected_cast/2, handle_unexpected_call/3,
	 process_auth_result/3, c2s_handle_bind/1,
     reject_unauthenticated_packet/2, process_closed/2,
     process_terminated/2, process_info/2]).
%% API
-export([get_presence/1, set_presence/2, resend_presence/1, resend_presence/2,
	 open_session/1, call/3, cast/2, send/2, close/1, close/2, stop_async/1,
	 reply/2, copy_state/2, set_timeout/2, route/2, format_reason/2,
	 host_up/1, host_down/1, send_ws_ping/1, bounce_message_queue/2,
	 reset_vcard_xupdate_resend_presence/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("mod_roster.hrl").
-include("translate.hrl").

-define(SETS, gb_sets).

-type state() :: xmpp_stream_in:state().
-export_type([state/0]).

%%%===================================================================
%%% ejabberd_listener API
%%%===================================================================
start(SockMod, Socket, Opts) ->
    xmpp_stream_in:start(?MODULE, [{SockMod, Socket}, Opts],
			 ejabberd_config:fsm_limit_opts(Opts)).

start_link(SockMod, Socket, Opts) ->
    xmpp_stream_in:start_link(?MODULE, [{SockMod, Socket}, Opts],
			      ejabberd_config:fsm_limit_opts(Opts)).

accept(Ref) ->
    xmpp_stream_in:accept(Ref).

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

-spec resend_presence(pid()) -> boolean().
resend_presence(Pid) ->
    resend_presence(Pid, undefined).

-spec resend_presence(pid(), jid() | undefined) -> boolean().
resend_presence(Pid, To) ->
    route(Pid, {resend_presence, To}).

-spec reset_vcard_xupdate_resend_presence(pid()) -> boolean().
reset_vcard_xupdate_resend_presence(Pid) ->
    route(Pid, reset_vcard_xupdate_resend_presence).

-spec close(pid()) -> ok;
	   (state()) -> state().
close(Ref) ->
    xmpp_stream_in:close(Ref).

-spec close(pid(), atom()) -> ok.
close(Ref, Reason) ->
    xmpp_stream_in:close(Ref, Reason).

-spec stop_async(pid()) -> ok.
stop_async(Pid) ->
    xmpp_stream_in:stop_async(Pid).

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

-spec send_ws_ping(pid()) -> ok;
		  (state()) -> state().
send_ws_ping(Ref) ->
    xmpp_stream_in:send_ws_ping(Ref).

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
    ejabberd_hooks:add(c2s_handle_bind, Host, ?MODULE, c2s_handle_bind, 100),
    ejabberd_hooks:add(c2s_unauthenticated_packet, Host, ?MODULE,
		       reject_unauthenticated_packet, 100),
    ejabberd_hooks:add(c2s_handle_info, Host, ?MODULE,
		       process_info, 100),
    ejabberd_hooks:add(c2s_auth_result, Host, ?MODULE,
		       process_auth_result, 100),
    ejabberd_hooks:add(c2s_handle_cast, Host, ?MODULE,
		       handle_unexpected_cast, 100),
    ejabberd_hooks:add(c2s_handle_call, Host, ?MODULE,
		       handle_unexpected_call, 100).

-spec host_down(binary()) -> ok.
host_down(Host) ->
    ejabberd_hooks:delete(c2s_closed, Host, ?MODULE, process_closed, 100),
    ejabberd_hooks:delete(c2s_terminated, Host, ?MODULE,
			  process_terminated, 100),
    ejabberd_hooks:delete(c2s_handle_bind, Host, ?MODULE, c2s_handle_bind, 100),
    ejabberd_hooks:delete(c2s_unauthenticated_packet, Host, ?MODULE,
			  reject_unauthenticated_packet, 100),
    ejabberd_hooks:delete(c2s_handle_info, Host, ?MODULE,
			  process_info, 100),
    ejabberd_hooks:delete(c2s_auth_result, Host, ?MODULE,
			  process_auth_result, 100),
    ejabberd_hooks:delete(c2s_handle_cast, Host, ?MODULE,
			  handle_unexpected_cast, 100),
    ejabberd_hooks:delete(c2s_handle_call, Host, ?MODULE,
			  handle_unexpected_call, 100).

%% Copies content of one c2s state to another.
%% This is needed for session migration from one pid to another.
-spec copy_state(state(), state()) -> state().
copy_state(NewState,
	   #{jid := JID, resource := Resource, auth_module := AuthModule,
	     lserver := LServer, pres_a := PresA} = OldState) ->
    State1 = case OldState of
		 #{pres_last := Pres, pres_timestamp := PresTS} ->
		     NewState#{pres_last => Pres, pres_timestamp => PresTS};
		 _ ->
		     NewState
	     end,
    Conn = get_conn_type(State1),
    State2 = State1#{jid => JID, resource => Resource,
		     conn => Conn,
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
    case State of
	#{bind2_session_id := Tag} ->
	    ejabberd_sm:open_session(SID, U, S, R, Prio, Info, Tag);
	_ ->
	    ejabberd_sm:open_session(SID, U, S, R, Prio, Info)
    end,
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
process_info(State, reset_vcard_xupdate_resend_presence) ->
    case maps:get(pres_last, State, error) of
	error -> State;
	Pres ->
	    Pres2 = xmpp:remove_subtag(Pres, #vcard_xupdate{}),
	    process_self_presence(State#{pres_last => Pres2}, Pres2)
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
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    State.

handle_unexpected_call(State, From, Msg) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Msg]),
    State.

handle_unexpected_cast(State, Msg) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    State.

c2s_handle_bind({<<"">>, {ok, State}}) ->
    {new_uniq_id(), {ok, State}};
c2s_handle_bind(Acc) ->
    Acc.

reject_unauthenticated_packet(State, _Pkt) ->
    Err = xmpp:serr_not_authorized(),
    send(State, Err).

process_auth_result(#{sasl_mech := Mech, auth_module := AuthModule,
		      socket := Socket, ip := IP, lserver := LServer} = State,
		    true, User) ->
    misc:set_proc_label({?MODULE, User, LServer}),
    ?INFO_MSG("(~ts) Accepted c2s ~ts authentication for ~ts@~ts by ~ts backend from ~ts",
              [xmpp_socket:pp(Socket), Mech, User, LServer,
               ejabberd_auth:backend_type(AuthModule),
               ejabberd_config:may_hide_data(misc:ip_to_list(IP))]),
    State;
process_auth_result(#{sasl_mech := Mech,
		      socket := Socket, ip := IP, lserver := LServer} = State,
		    {false, Reason}, User) ->
    ?WARNING_MSG("(~ts) Failed c2s ~ts authentication ~tsfrom ~ts: ~ts",
                 [xmpp_socket:pp(Socket), Mech,
                  if User /= <<"">> -> ["for ", User, "@", LServer, " "];
                     true -> ""
                  end,
                  ejabberd_config:may_hide_data(misc:ip_to_list(IP)), Reason]),
    State.

process_closed(State, Reason) ->
    stop_async(self()),
    State#{stop_reason => Reason}.

process_terminated(#{sid := SID, jid := JID, user := U, server := S, resource := R} = State,
		   Reason) ->
    Status = format_reason(State, Reason),
    ?INFO_MSG("(~ts) Closing c2s session for ~ts: ~ts",
	      [case maps:find(socket, State) of
		   {ok, Socket} -> xmpp_socket:pp(Socket);
		   _ -> <<"unknown">>
	       end, jid:encode(JID), Status]),
    Pres = #presence{type = unavailable,
		     from = JID,
		     to = jid:remove_resource(JID)},
    State1 = case maps:is_key(pres_last, State) of
		 true ->
		     ejabberd_sm:close_session_unset_presence(SID, U, S, R,
							      Status),
		     broadcast_presence_unavailable(State, Pres, true);
		 false ->
		     ejabberd_sm:close_session(SID, U, S, R),
		     broadcast_presence_unavailable(State, Pres, false)
	     end,
    bounce_message_queue(SID, JID),
    State1;
process_terminated(#{stop_reason := {tls, _}} = State, Reason) ->
    ?WARNING_MSG("(~ts) Failed to secure c2s connection: ~ts",
		 [case maps:find(socket, State) of
		      {ok, Socket} -> xmpp_socket:pp(Socket);
		      _ -> <<"unknown">>
		  end, format_reason(State, Reason)]),
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
		       case ejabberd_pkix:get_certfile(LServer) of
			   error -> DefaultOpts;
			   {ok, CertFile} ->
			       lists:keystore(certfile, 1, DefaultOpts,
					      {certfile, CertFile})
		       end
	       end,
    TLSOpts2 = case ejabberd_option:c2s_ciphers(LServer) of
                   undefined -> TLSOpts1;
                   Ciphers -> lists:keystore(ciphers, 1, TLSOpts1,
					     {ciphers, Ciphers})
               end,
    TLSOpts3 = case ejabberd_option:c2s_protocol_options(LServer) of
                   undefined -> TLSOpts2;
                   ProtoOpts -> lists:keystore(protocol_options, 1, TLSOpts2,
					       {protocol_options, ProtoOpts})
               end,
    TLSOpts4 = case ejabberd_option:c2s_dhfile(LServer) of
                   undefined -> TLSOpts3;
                   DHFile -> lists:keystore(dhfile, 1, TLSOpts3,
					    {dhfile, DHFile})
               end,
    TLSOpts5 = case ejabberd_option:c2s_cafile(LServer) of
		   undefined -> TLSOpts4;
		   CAFile -> lists:keystore(cafile, 1, TLSOpts4,
					    {cafile, CAFile})
	       end,
    case ejabberd_option:c2s_tls_compression(LServer) of
	undefined -> TLSOpts5;
	false -> [compression_none | TLSOpts5];
	true -> lists:delete(compression_none, TLSOpts5)
    end.

tls_required(#{tls_required := TLSRequired}) ->
    TLSRequired.

tls_enabled(#{tls_enabled := TLSEnabled,
	      tls_required := TLSRequired,
	      tls_verify := TLSVerify}) ->
    TLSEnabled or TLSRequired or TLSVerify.

allow_unencrypted_sasl2(#{allow_unencrypted_sasl2 := AllowUnencryptedSasl2}) ->
    AllowUnencryptedSasl2.

compress_methods(#{zlib := true}) ->
    [<<"zlib">>];
compress_methods(_) ->
    [].

unauthenticated_stream_features(#{lserver := LServer}) ->
    ejabberd_hooks:run_fold(c2s_pre_auth_features, LServer, [], [LServer]).

authenticated_stream_features(#{lserver := LServer}) ->
    ejabberd_hooks:run_fold(c2s_post_auth_features, LServer, [], [LServer]).

inline_stream_features(#{lserver := LServer}) ->
    ejabberd_hooks:run_fold(c2s_inline_features, LServer, {[], [], []}, [LServer]).

sasl_mechanisms(Mechs, #{lserver := LServer, stream_encrypted := Encrypted} = State) ->
    Type = ejabberd_auth:store_type(LServer),
    Mechs1 = ejabberd_option:disable_sasl_mechanisms(LServer),

    ScramHash = ejabberd_option:auth_scram_hash(LServer),
    ShaAv = Type == plain orelse (Type == scram andalso ScramHash == sha),
    Sha256Av = Type == plain orelse (Type == scram andalso ScramHash == sha256),
    Sha512Av = Type == plain orelse (Type == scram andalso ScramHash == sha512),
    %% I re-created it from cyrsasl ets magic, but I think it's wrong
    %% TODO: need to check before 18.09 release
    lists:filter(
      fun(<<"ANONYMOUS">>) ->
	      ejabberd_auth_anonymous:is_sasl_anonymous_enabled(LServer);
	 (<<"DIGEST-MD5">>) -> Type == plain;
	 (<<"SCRAM-SHA-1">>) -> ShaAv;
	 (<<"SCRAM-SHA-1-PLUS">>) -> ShaAv andalso Encrypted;
	 (<<"SCRAM-SHA-256">>) -> Sha256Av;
	 (<<"SCRAM-SHA-256-PLUS">>) -> Sha256Av andalso Encrypted;
	 (<<"SCRAM-SHA-512">>) -> Sha512Av;
	 (<<"SCRAM-SHA-512-PLUS">>) -> Sha512Av andalso Encrypted;
	 (<<"PLAIN">>) -> true;
	 (<<"X-OAUTH2">>) -> [ejabberd_auth_anonymous] /= ejabberd_auth:auth_modules(LServer);
	 (<<"EXTERNAL">>) -> maps:get(tls_verify, State, false);
	 (_) -> false
      end, Mechs -- Mechs1).

sasl_options(#{lserver := LServer}) ->
    case ejabberd_option:disable_sasl_scram_downgrade_protection(LServer) of
	true -> [{scram_downgrade_protection, false}];
	_ -> []
    end.

get_password_fun(_Mech, #{lserver := LServer}) ->
    fun(U) ->
	    ejabberd_auth:get_password_with_authmodule(U, LServer)
    end.

check_password_fun(<<"X-OAUTH2">>, #{lserver := LServer}) ->
    fun(User, _AuthzId, Token) ->
	    case ejabberd_oauth:check_token(
		   User, LServer, [<<"sasl_auth">>], Token) of
		true -> {true, ejabberd_oauth};
		_ -> {false, ejabberd_oauth}
	    end
    end;
check_password_fun(_Mech, #{lserver := LServer}) ->
    fun(U, AuthzId, P) ->
	    ejabberd_auth:check_password_with_authmodule(U, AuthzId, LServer, P)
    end.

check_password_digest_fun(_Mech, #{lserver := LServer}) ->
    fun(U, AuthzId, P, D, DG) ->
	    ejabberd_auth:check_password_with_authmodule(U, AuthzId, LServer, P, D, DG)
    end.

get_fast_tokens_fun(_Mech, #{lserver := LServer}) ->
    fun(User, UA) ->
	case gen_mod:is_loaded(LServer, mod_auth_fast) of
	    false -> false;
	    _  -> mod_auth_fast:get_tokens(LServer, User, UA)
	end
    end.

fast_mechanisms(#{lserver := LServer}) ->
    case gen_mod:is_loaded(LServer, mod_auth_fast) of
	false -> [];
	_  -> mod_auth_fast:get_mechanisms(LServer)
    end.

bind(
    R,
    #{
        user := U,
        server := S,
        lserver := LServer,
        access := Access,
        lang := Lang,
        socket := Socket,
        ip := IP
    }=State
) ->
    case ejabberd_hooks:run_fold(c2s_handle_bind, LServer, {R, {ok, State}}, []) of
        {R2, {ok, State2}} ->
            case resource_conflict_action(U, S, R2) of
                closenew ->
                    {error, xmpp:err_conflict(), State2};
                {accept_resource, Resource} ->
                    JID = jid:make(U, S, Resource),
                    case acl:match_rule(LServer, Access, #{usr => jid:split(JID), ip => IP}) of
                        allow ->
                            State3 = open_session(
                                State2#{resource => Resource, sid => ejabberd_sm:make_sid()}
                            ),
                            State4 = ejabberd_hooks:run_fold(
                                c2s_session_opened, LServer, State3, []
                            ),
                            ?INFO_MSG(
                                "(~ts) Opened c2s session for ~ts", [xmpp_socket:pp(Socket), jid:encode(JID)]
                            ),
                            {ok, State4};
                        deny ->
                            ejabberd_hooks:run(forbidden_session_hook, LServer, [JID]),
                            ?WARNING_MSG(
                                "(~ts) Forbidden c2s session for ~ts",
                                [xmpp_socket:pp(Socket), jid:encode(JID)]
                            ),
                            Txt = ?T("Access denied by service policy"),
                            {error, xmpp:err_not_allowed(Txt, Lang), State2}
                    end
            end;
        {R2, {error, XmppErr, _State2}=Err} ->
            case XmppErr of
                #stanza_error{reason = 'not-allowed'} ->
                    JID = jid:make(U, S, R2),
                    ejabberd_hooks:run(forbidden_session_hook, LServer, [JID]),
                    ?WARNING_MSG(
                        "(~ts) Forbidden c2s session for ~ts",
                        [xmpp_socket:pp(Socket), jid:encode(JID)]
                    );
                _ ->
                    ok
            end,
            Err
    end.

handle_stream_start(StreamStart, #{lserver := LServer} = State) ->
    case ejabberd_router:is_my_host(LServer) of
	false ->
	    send(State#{lserver => ejabberd_config:get_myname()}, xmpp:serr_host_unknown());
	true ->
	    State1 = change_shaper(State),
	    Opts = ejabberd_config:codec_options(),
	    State2 = State1#{codec_options => Opts},
	    ejabberd_hooks:run_fold(
	      c2s_stream_started, LServer, State2, [StreamStart])
    end.

handle_stream_end(Reason, #{lserver := LServer} = State) ->
    State1 = State#{stop_reason => Reason},
    ejabberd_hooks:run_fold(c2s_closed, LServer, State1, [Reason]).

handle_auth_success(User, _Mech, AuthModule,
		    #{lserver := LServer} = State) ->
    State1 = State#{auth_module => AuthModule},
    ejabberd_hooks:run_fold(c2s_auth_result, LServer, State1, [true, User]).

handle_auth_failure(User, _Mech, Reason,
		    #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_auth_result, LServer, State, [{false, Reason}, User]).

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
		    % It seems that some client are expecting to have response
		    % to session request be sent from server jid, let's make
		    % sure it is that.
		    Pkt3 = xmpp:set_to(Pkt2, jid:make(<<>>, LServer, <<>>)),
		    send(State2, xmpp:make_iq_result(Pkt3));
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

handle_sasl2_inline(Els, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_handle_sasl2_inline, LServer,
			    {State, Els, []}, []).

handle_sasl2_inline_post(Els, Results, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_handle_sasl2_inline_post, LServer,
			    State, [Els, Results]).

handle_bind2_inline(Els, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_handle_bind2_inline, LServer,
			    {State, Els, []}, []).

handle_bind2_inline_post(Els, Results, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_handle_bind2_inline_post, LServer,
			    State, [Els, Results]).

handle_sasl2_task_next(Task, Els, InlineEls, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_handle_sasl2_task_next, LServer,
			    {abort, State}, [Task, Els, InlineEls]).

handle_sasl2_task_data(Els, InlineEls, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_handle_sasl2_task_data, LServer,
			    {abort, State}, [Els, InlineEls]).

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
    AllowUnencryptedSasl2 = proplists:get_bool(allow_unencrypted_sasl2, Opts),
    Zlib = proplists:get_bool(zlib, Opts),
    Timeout = ejabberd_option:negotiation_timeout(),
    State1 = State#{tls_options => TLSOpts2,
		    tls_required => TLSRequired,
		    tls_enabled => TLSEnabled,
		    tls_verify => TLSVerify,
		    allow_unencrypted_sasl2 => AllowUnencryptedSasl2,
		    pres_a => ?SETS:new(),
		    zlib => Zlib,
		    lang => ejabberd_option:language(),
		    server => ejabberd_config:get_myname(),
		    lserver => ejabberd_config:get_myname(),
		    access => Access,
		    shaper => Shaper},
    State2 = xmpp_stream_in:set_timeout(State1, Timeout),
    misc:set_proc_label({?MODULE, init_state}),
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
	    Packet = xmpp:set_from_to(LastPres, To, From),
	    Packet2 = misc:add_delay_info(Packet, To, TS),
	    case privacy_check_packet(State, Packet2, out) of
		deny ->
		    ok;
		allow ->
		    ejabberd_hooks:run(presence_probe_hook,
				       LServer,
				       [From, To, self()]),
		    ejabberd_router:route(Packet2)
	    end;
       true ->
	    ok
    end;
route_probe_reply(_, _) ->
    ok.

-spec process_presence_out(state(), presence()) -> state().
process_presence_out(#{lserver := LServer, jid := JID,
		       lang := Lang, pres_a := PresA} = State0,
		     #presence{from = From, to = To, type = Type} = Pres) ->
    State1 =
	if Type == subscribe; Type == subscribed;
	   Type == unsubscribe; Type == unsubscribed ->
		Access = mod_roster_opt:access(LServer),
		MyBareJID = jid:remove_resource(JID),
		case acl:match_rule(LServer, Access, MyBareJID) of
		    deny ->
			AccessErrTxt = ?T("Access denied by service policy"),
			AccessErr = xmpp:err_forbidden(AccessErrTxt, Lang),
			send_error(State0, Pres, AccessErr);
		    allow ->
			ejabberd_hooks:run(roster_out_subscription, LServer, [Pres]),
			State0
		end;
	   true ->
		State0
	end,
    case privacy_check_packet(State1, Pres, out) of
	deny ->
	    PrivErrTxt = ?T("Your active privacy list has denied "
			    "the routing of this stanza."),
	    PrivErr = xmpp:err_not_acceptable(PrivErrTxt, Lang),
	    send_error(State1, Pres, PrivErr);
	allow when Type == subscribe; Type == subscribed;
		   Type == unsubscribe; Type == unsubscribed ->
	    BareFrom = jid:remove_resource(From),
	    ejabberd_router:route(xmpp:set_from_to(Pres, BareFrom, To)),
	    State1;
	allow when Type == error; Type == probe ->
	    ejabberd_router:route(Pres),
	    State1;
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
			    State1#{pres_a => A};
		       true ->
			    State1
		    end;
	       true ->
		    State1
	    end
    end.

-spec process_self_presence(state(), presence()) -> state().
process_self_presence(#{lserver := LServer, sid := SID,
			user := U, server := S,	resource := R} = State,
		      #presence{type = unavailable} = Pres) ->
    Status = xmpp:get_text(Pres#presence.status),
    _ = ejabberd_sm:unset_presence(SID, U, S, R, Status),
    {Pres1, State1} = ejabberd_hooks:run_fold(
			c2s_self_presence, LServer, {Pres, State}, []),
    State2 = broadcast_presence_unavailable(State1, Pres1, true),
    maps:remove(pres_last, maps:remove(pres_timestamp, State2));
process_self_presence(#{lserver := LServer} = State,
		      #presence{type = available} = Pres) ->
    PreviousPres = maps:get(pres_last, State, undefined),
    _ = update_priority(State, Pres),
    {Pres1, State1} = ejabberd_hooks:run_fold(
			c2s_self_presence, LServer, {Pres, State}, []),
    State2 = State1#{pres_last => Pres1,
		     pres_timestamp => erlang:timestamp()},
    FromUnavailable = PreviousPres == undefined,
    broadcast_presence_available(State2, Pres1, FromUnavailable);
process_self_presence(State, _Pres) ->
    State.

-spec update_priority(state(), presence()) -> ok | {error, notfound}.
update_priority(#{sid := SID, user := U, server := S, resource := R},
		Pres) ->
    Priority = get_priority_from_presence(Pres),
    ejabberd_sm:set_presence(SID, U, S, R, Priority, Pres).

-spec broadcast_presence_unavailable(state(), presence(), boolean()) -> state().
broadcast_presence_unavailable(#{jid := JID, pres_a := PresA} = State, Pres,
			       BroadcastToRoster) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    BareJID = jid:tolower(jid:remove_resource(JID)),
    Items1 = case BroadcastToRoster of
		true ->
		    Roster = ejabberd_hooks:run_fold(roster_get, LServer,
						     [], [{LUser, LServer}]),
		    lists:foldl(
			fun(#roster_item{jid = ItemJID, subscription = Sub}, Acc)
			       when Sub == both; Sub == from ->
			    maps:put(jid:tolower(ItemJID), 1, Acc);
			   (_, Acc) ->
			       Acc
			end, #{BareJID => 1}, Roster);
		_ ->
		    #{BareJID => 1}
	    end,
    Items2 = ?SETS:fold(
	fun(LJID, Acc) ->
	    maps:put(LJID, 1, Acc)
	end, Items1, PresA),

    JIDs = lists:filtermap(
	fun(LJid) ->
	    To = jid:make(LJid),
	    P = xmpp:set_to(Pres, To),
	    case privacy_check_packet(State, P, out) of
		allow -> {true, To};
		deny -> false
	    end
	end, maps:keys(Items2)),
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
	  fun(#roster_item{jid = To, subscription = Sub}, {F, T}) ->
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
	     fun(#roster_item{jid = To, subscription = Sub}, Tos)
		   when Sub == both orelse Sub == from ->
		     P = xmpp:set_to(Pres, To),
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
            ErrText = ?T("Your active privacy list has denied "
			 "the routing of this stanza."),
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
    ejabberd_router_multicast:route_multicast(From, LServer, JIDs, Pkt, false).

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
			ejabberd_option:resource_conflict(S);
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

-spec bounce_message_queue(ejabberd_sm:sid(), jid:jid()) -> ok.
bounce_message_queue({_, Pid} = SID, JID) ->
    {U, S, R} = jid:tolower(JID),
    SIDs = ejabberd_sm:get_session_sids(U, S, R),
    case lists:member(SID, SIDs) of
	true ->
	    ?WARNING_MSG("The session for ~ts@~ts/~ts is supposed to "
			 "be unregistered, but session identifier ~p "
			 "still presents in the 'session' table",
			 [U, S, R, Pid]);
	false ->
	    receive {route, Pkt} ->
		    ejabberd_router:route(Pkt),
		    bounce_message_queue(SID, JID)
	    after 100 ->
		    ok
	    end
    end.

-spec new_uniq_id() -> binary().
new_uniq_id() ->
    iolist_to_binary(
      [p1_rand:get_string(),
       integer_to_binary(erlang:unique_integer([positive]))]).

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

-spec fix_from_to(xmpp_element(), state()) -> stanza() | xmpp_element().
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
	    To1 = case xmpp:get_to(Pkt) of
			#jid{lresource = <<>>} = To2 -> To2;
			_ -> JID
		    end,
	    xmpp:set_from_to(Pkt, From1, To1)
    end;
fix_from_to(Pkt, _State) ->
    Pkt.

-spec change_shaper(state()) -> state().
change_shaper(#{shaper := ShaperName, ip := {IP, _}, lserver := LServer,
		user := U, server := S, resource := R} = State) ->
    JID = jid:make(U, S, R),
    Shaper = ejabberd_shaper:match(LServer, ShaperName,
				   #{usr => jid:split(JID), ip => IP}),
    xmpp_stream_in:change_shaper(State, ejabberd_shaper:new(Shaper)).

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

listen_opt_type(starttls) ->
    econf:bool();
listen_opt_type(starttls_required) ->
    econf:bool();
listen_opt_type(allow_unencrypted_sasl2) ->
    econf:bool();
listen_opt_type(tls_verify) ->
    econf:bool();
listen_opt_type(zlib) ->
    econf:and_then(
      econf:bool(),
      fun(false) -> false;
	 (true) ->
	      ejabberd:start_app(ezlib),
	      true
      end).

listen_options() ->
    [{access, all},
     {shaper, none},
     {ciphers, undefined},
     {dhfile, undefined},
     {cafile, undefined},
     {protocol_options, undefined},
     {tls, false},
     {tls_compression, false},
     {starttls, false},
     {starttls_required, false},
     {allow_unencrypted_sasl2, false},
     {tls_verify, false},
     {zlib, false},
     {max_stanza_size, infinity},
     {max_fsm_queue, 10000}].
