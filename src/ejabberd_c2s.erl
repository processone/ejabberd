%%%-------------------------------------------------------------------
%%% Created :  8 Dec 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
%%%-------------------------------------------------------------------
-module(ejabberd_c2s).
-behaviour(xmpp_stream_in).
-behaviour(ejabberd_config).

-protocol({rfc, 6121}).

%% ejabberd_socket callbacks
-export([start/2, socket_type/0]).
%% ejabberd_config callbacks
-export([opt_type/1, transform_listen_option/2]).
%% xmpp_stream_in callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).
-export([tls_options/1, tls_required/1, compress_methods/1,
	 sasl_mechanisms/1, init_sasl/1, bind/2, handshake/2,
	 unauthenticated_stream_features/1, authenticated_stream_features/1,
	 handle_stream_start/1, handle_stream_end/1, handle_stream_close/1,
	 handle_unauthenticated_packet/2, handle_authenticated_packet/2,
	 handle_auth_success/4, handle_auth_failure/4, handle_send/5,
	 handle_unbinded_packet/2, handle_cdata/2]).
%% API
-export([get_presence/1, get_subscription/2, get_subscribed/1,
	 send/2, close/1]).

-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").

-define(SETS, gb_sets).

%%-define(DBGFSM, true).
-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-type state() :: map().
-type next_state() :: {noreply, state()} | {stop, term(), state()}.
-export_type([state/0, next_state/0]).

%%%===================================================================
%%% ejabberd_socket API
%%%===================================================================
start(SockData, Opts) ->
    xmpp_stream_in:start(?MODULE, [SockData, Opts],
			 fsm_limit_opts(Opts) ++ ?FSMOPTS).

socket_type() ->
    xml_stream.

-spec get_presence(pid()) -> presence().
get_presence(Ref) ->
    xmpp_stream_in:call(Ref, get_presence, 1000).

-spec get_subscription(jid() | ljid(), state()) -> both | from | to | none.
get_subscription(#jid{} = From, State) ->
    get_subscription(jid:tolower(From), State);
get_subscription(LFrom, #{pres_f := PresF, pres_t := PresT}) ->
    LBFrom = jid:remove_resource(LFrom),
    F = ?SETS:is_element(LFrom, PresF) orelse ?SETS:is_element(LBFrom, PresF),
    T = ?SETS:is_element(LFrom, PresT) orelse ?SETS:is_element(LBFrom, PresT),
    if F and T -> both;
       F -> from;
       T -> to;
       true -> none
    end.

-spec get_subscribed(pid()) -> [ljid()].
%% Return list of all available resources of contacts
get_subscribed(Ref) ->
    xmpp_stream_in:call(Ref, get_subscribed, 1000).

-spec close(pid()) -> ok.
close(Ref) ->
    xmpp_stream_in:cast(Ref, closed).

-spec send(state(), xmpp_element()) -> next_state().
send(State, Pkt) ->
    xmpp_stream_in:send(State, Pkt).

%%%===================================================================
%%% xmpp_stream_in callbacks
%%%===================================================================
tls_options(#{lserver := LServer, tls_options := TLSOpts}) ->
    case ejabberd_config:get_option({domain_certfile, LServer},
				    fun iolist_to_binary/1) of
	undefined ->
	    TLSOpts;
	CertFile ->
	    lists:keystore(certfile, 1, TLSOpts, {certfile, CertFile})
    end.

tls_required(#{tls_required := TLSRequired}) ->
    TLSRequired.

compress_methods(#{zlib := true}) ->
    [<<"zlib">>];
compress_methods(_) ->
    [].

sasl_mechanisms(#{lserver := LServer}) ->
    cyrsasl:listmech(LServer).

unauthenticated_stream_features(#{lserver := LServer}) ->
    ejabberd_hooks:run_fold(c2s_pre_auth_features, LServer, [], [LServer]).

authenticated_stream_features(#{lserver := LServer}) ->
    ejabberd_hooks:run_fold(c2s_post_auth_features, LServer, [], [LServer]).

init_sasl(#{lserver := LServer}) ->
    cyrsasl:server_new(
      <<"jabber">>, LServer, <<"">>, [],
      fun(U) ->
	      ejabberd_auth:get_password_with_authmodule(U, LServer)
      end,
      fun(U, AuthzId, P) ->
	      ejabberd_auth:check_password_with_authmodule(U, AuthzId, LServer, P)
      end,
      fun(U, AuthzId, P, D, DG) ->
	      ejabberd_auth:check_password_with_authmodule(U, AuthzId, LServer, P, D, DG)
      end).

bind(<<"">>, State) ->
    bind(new_uniq_id(), State);
bind(R, #{user := U, server := S} = State) ->
    case resource_conflict_action(U, S, R) of
	closenew ->
	    {error, xmpp:err_conflict(), State};
	{accept_resource, Resource} ->
	    open_session(State, Resource)
    end.

handshake(_Data, State) ->
    %% This is only for jabber component
    {ok, State}.

handle_stream_start(#{lserver := LServer, ip := IP, lang := Lang} = State) ->
    case lists:member(LServer, ?MYHOSTS) of
	false ->
	    xmpp_stream_in:send(State, xmpp:serr_host_unknown());
	true ->
	    case check_bl_c2s(IP, Lang) of
		false ->
		    change_shaper(State),
		    {noreply, State};
		{true, LogReason, ReasonT} ->
		    ?INFO_MSG("Connection attempt from blacklisted IP ~s: ~s",
			      [jlib:ip_to_list(IP), LogReason]),
		    Err = xmpp:serr_policy_violation(ReasonT, Lang),
		    xmpp_stream_in:send(State, Err)
	    end
    end.

handle_stream_end(State) ->
    {stop, normal, State}.

handle_stream_close(State) ->
    {stop, normal, State}.

handle_auth_success(User, Mech, AuthModule,
		    #{socket := Socket, ip := IP, lserver := LServer} = State) ->
    ?INFO_MSG("(~w) Accepted ~s authentication for ~s@~s by ~p from ~s",
	      [Socket, Mech, User, LServer, AuthModule,
	       ejabberd_config:may_hide_data(jlib:ip_to_list(IP))]),
    State1 = State#{auth_module => AuthModule},
    ejabberd_hooks:run_fold(c2s_auth_result, LServer,
			    {noreply, State1}, [true, User]).

handle_auth_failure(User, Mech, Reason,
		    #{socket := Socket, ip := IP, lserver := LServer} = State) ->
    ?INFO_MSG("(~w) Failed ~s authentication ~sfrom ~s: ~s",
	      [Socket, Mech,
	       if User /= <<"">> -> ["for ", User, "@", LServer, " "];
		  true -> ""
	       end,
	       ejabberd_config:may_hide_data(jlib:ip_to_list(IP)), Reason]),
    ejabberd_hooks:run_fold(c2s_auth_result, LServer,
			    {noreply, State}, [false, User]).

handle_unbinded_packet(Pkt, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_unbinded_packet, LServer,
			    {noreply, State}, [Pkt]).

handle_unauthenticated_packet(Pkt, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_unauthenticated_packet,
			    LServer, {noreply, State}, [Pkt]).

handle_authenticated_packet(Pkt, #{lserver := LServer} = State) when not ?is_stanza(Pkt) ->
    ejabberd_hooks:run_fold(c2s_authenticated_packet,
			    LServer, {noreply, State}, [Pkt]);
handle_authenticated_packet(Pkt, #{lserver := LServer} = State) ->
    case ejabberd_hooks:run_fold(c2s_authenticated_packet,
				 LServer, {noreply, State}, [Pkt]) of
	{noreply, State1} ->
	    Pkt1 = ejabberd_hooks:run_fold(user_send_packet, LServer, Pkt, [State1]),
	    Res = case Pkt1 of
		      #presence{to = #jid{lresource = <<"">>}} ->
			  process_self_presence(State1, Pkt1);
		      #presence{} ->
			  process_presence_out(State1, Pkt1);
		      _ ->
			  check_privacy_then_route(State1, Pkt1)
		  end,
	    ejabberd_hooks:run(c2s_loop_debug, [{xmlstreamelement, Pkt}]),
	    Res;
	Err ->
	    ejabberd_hooks:run(c2s_loop_debug, [{xmlstreamelement, Pkt}]),
	    Err
    end.

handle_cdata(Data, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_handle_cdata, LServer,
			    {noreply, State}, [Data]).

handle_send(Reason, Pkt, El, Data, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_handle_send, LServer,
			    {noreply, State}, [Reason, Pkt, El, Data]).

init([State, Opts]) ->
    Access = gen_mod:get_opt(access, Opts, fun acl:access_rules_validator/1, all),
    Shaper = gen_mod:get_opt(shaper, Opts, fun acl:shaper_rules_validator/1, none),
    TLSOpts = lists:filter(
		fun({certfile, _}) -> true;
		   ({ciphers, _}) -> true;
		   ({dhfile, _}) -> true;
		   (_) -> false
		end, Opts),
    TLSRequired = proplists:get_bool(starttls_required, Opts),
    TLSVerify = proplists:get_bool(tls_verify, Opts),
    Zlib = proplists:get_bool(zlib, Opts),
    State1 = State#{tls_options => TLSOpts,
		    tls_required => TLSRequired,
		    tls_verify => TLSVerify,
		    pres_a => ?SETS:new(),
		    pres_f => ?SETS:new(),
		    pres_t => ?SETS:new(),
		    sid => ejabberd_sm:make_sid(),
		    zlib => Zlib,
		    lang => ?MYLANG,
		    server => ?MYNAME,
		    access => Access,
		    shaper => Shaper},
    ejabberd_hooks:run_fold(c2s_init, {ok, State1}, []).

handle_call(get_presence, _From, #{jid := JID} = State) ->
    Pres = case maps:get(pres_last, State, undefined) of
	       undefined ->
		   BareJID = jid:remove_resource(JID),
		   #presence{from = JID, to = BareJID, type = unavailable};
	       P ->
		   P
	   end,
    {reply, Pres, State};
handle_call(get_subscribed, _From, #{pres_f := PresF} = State) ->
    Subscribed = ?SETS:to_list(PresF),
    {reply, Subscribed, State};
handle_call(Request, From, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(
      c2s_handle_call, LServer, {noreply, State}, [Request, From]).

handle_cast(closed, State) ->
    handle_stream_close(State);
handle_cast(Msg, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_handle_cast, LServer, {noreply, State}, [Msg]).

handle_info({route, From, To, Packet0}, #{lserver := LServer} = State) ->
    Packet = xmpp:set_from_to(Packet0, From, To),
    {Pass, NewState} = case Packet of
			   #presence{} ->
			       process_presence_in(State, Packet);
			   #message{} ->
			       process_message_in(State, Packet);
			   #iq{} ->
			       process_iq_in(State, Packet)
		       end,
    if Pass ->
	    Packet1 = ejabberd_hooks:run_fold(
			user_receive_packet, LServer, Packet, [NewState]),
	    ejabberd_hooks:run(c2s_loop_debug, [{route, From, To, Packet}]),
	    xmpp_stream_in:send(NewState, Packet1);
       true ->
	    ejabberd_hooks:run(c2s_loop_debug, [{route, From, To, Packet}]),
	    {noreply, NewState}
    end;
handle_info(system_shutdown, State) ->
    xmpp_stream_in:send(State, xmpp:serr_system_shutdown());
handle_info(Info, #{lserver := LServer} = State) ->
    ejabberd_hooks:run_fold(c2s_handle_info, LServer, {noreply, State}, [Info]).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec check_bl_c2s({inet:ip_address(), non_neg_integer()}, binary())
      -> false | {true, binary(), binary()}.
check_bl_c2s({IP, _Port}, Lang) ->
    ejabberd_hooks:run_fold(check_bl_c2s, false, [IP, Lang]).

-spec open_session(state(), binary()) -> {ok, state()} | {error, stanza_error(), state()}.
open_session(#{user := U, server := S, lserver := LServer, sid := SID,
	       socket := Socket, ip := IP, auth_module := AuthMod,
	       access := Access, lang := Lang} = State, R) ->
    JID = jid:make(U, S, R),
    case acl:access_matches(Access,
			    #{usr => jid:split(JID), ip => IP},
			    LServer) of
	allow ->
	    ?INFO_MSG("(~w) Opened session for ~s",
		      [Socket, jid:to_string(JID)]),
	    change_shaper(State),
	    Conn = get_conn_type(State),
            Info = [{ip, IP}, {conn, Conn}, {auth_module, AuthMod}],
	    ejabberd_sm:open_session(SID, U, LServer, R, Info),
	    State1 = State#{conn => Conn, resource => R, jid => JID},
	    State2 = ejabberd_hooks:run_fold(
		       c2s_session_opened, LServer, State1, []),
	    {ok, State2};
	deny ->
	    ejabberd_hooks:run(forbidden_session_hook, LServer, [JID]),
            ?INFO_MSG("(~w) Forbidden session for ~s",
                      [Socket, jid:to_string(JID)]),
	    Txt = <<"Denied by ACL">>,
	    {error, xmpp:err_not_allowed(Txt, Lang), State}
    end.

-spec process_iq_in(state(), iq()) -> {boolean(), state()}.
process_iq_in(State, #iq{} = IQ) ->
    case privacy_check_packet(State, IQ, in) of
	allow ->
	    {true, State};
	deny ->
	    route_error(IQ, xmpp:err_service_unavailable()),
	    {false, State}
    end.

-spec process_message_in(state(), message()) -> {boolean(), state()}.
process_message_in(State, #message{type = T} = Msg) ->
    case privacy_check_packet(State, Msg, in) of
	allow ->
	    {true, State};
	deny when T == groupchat; T == headline ->
	    ok;
	deny ->
	    case xmpp:has_subtag(Msg, #muc_user{}) of
		true ->
		    ok;
		false ->
		    route_error(Msg, xmpp:err_service_unavailable())
	    end,
	    {false, State}
    end.

-spec process_presence_in(state(), presence()) -> {boolean(), state()}.
process_presence_in(#{lserver := LServer, pres_a := PresA} = State0,
		    #presence{from = From, to = To, type = T} = Pres) ->
    State = ejabberd_hooks:run_fold(c2s_presence_in, LServer, State0, [Pres]),
    case T of
	probe ->
	    NewState = do_some_magic(State, From),
	    route_probe_reply(From, To, NewState),
	    {false, NewState};
	error ->
	    A = ?SETS:del_element(jid:tolower(From), PresA),
	    {true, State#{pres_a => A}};
	_ ->
	    case privacy_check_packet(State, Pres, in) of
		allow when T == error ->
		    {true, State};
		allow ->
		    NewState = do_some_magic(State, From),
		    {true, NewState};
		deny ->
		    {false, State}
	    end
    end.

-spec route_probe_reply(jid(), jid(), state()) -> ok.
route_probe_reply(From, To, #{lserver := LServer, pres_f := PresF,
			      pres_last := LastPres,
			      pres_timestamp := TS} = State) ->
    LFrom = jid:tolower(From),
    LBFrom = jid:remove_resource(LFrom),
    case ?SETS:is_element(LFrom, PresF)
	orelse ?SETS:is_element(LBFrom, PresF) of
	true ->
	    %% To is my JID
	    Packet = xmpp_util:add_delay_info(LastPres, To, TS),
	    case privacy_check_packet(State, Packet, out) of
		deny ->
		    ok;
		allow ->
		    ejabberd_hooks:run(presence_probe_hook,
				       LServer,
				       [From, To, self()]),
		    %% Don't route a presence probe to oneself
		    case From == To of
			false ->
			    route(xmpp:set_from_to(Packet, To, From));
			true ->
			    ok
		    end
	    end;
	false ->
	    ok
    end;
route_probe_reply(_, _, _) ->
    ok.

-spec process_presence_out(state(), presence()) -> next_state().
process_presence_out(#{user := User, server := Server, lserver := LServer,
		       jid := JID, lang := Lang, pres_a := PresA} = State,
		     #presence{from = From, to = To, type = Type} = Pres) ->
    LTo = jid:tolower(To),
    case privacy_check_packet(State, Pres, out) of
	deny ->
            ErrText = <<"Your active privacy list has denied "
			"the routing of this stanza.">>,
	    Err = xmpp:err_not_acceptable(ErrText, Lang),
	    xmpp_stream_in:send_error(State, Pres, Err);
	allow when Type == subscribe; Type == subscribed;
		   Type == unsubscribe; Type == unsubscribed ->
	    Access = gen_mod:get_module_opt(LServer, mod_roster, access,
					    fun(A) when is_atom(A) -> A end,
					    all),
	    MyBareJID = jid:remove_resource(JID),
	    case acl:match_rule(LServer, Access, MyBareJID) of
		deny ->
		    ErrText = <<"Denied by ACL">>,
		    Err = xmpp:err_forbidden(ErrText, Lang),
		    xmpp_stream_in:send_error(State, Pres, Err);
		allow ->
		    ejabberd_hooks:run(roster_out_subscription,
				       LServer,
				       [User, Server, To, Type]),
		    BareFrom = jid:remove_resource(From),
		    route(xmpp:set_from_to(Pres, BareFrom, To)),
		    {noreply, State}
	    end;
	allow when Type == error; Type == probe ->
	    route(Pres),
	    {noreply, State};
	allow ->
	    route(Pres),
	    A = case Type of
		    available -> ?SETS:add_element(LTo, PresA);
		    unavailable -> ?SETS:del_element(LTo, PresA)
		end,
	    {noreply, State#{pres_a => A}}
    end.

-spec process_self_presence(state(), presence()) -> {noreply, state()}.
process_self_presence(#{ip := IP, conn := Conn,
			auth_module := AuthMod, sid := SID,
			user := U, server := S,	resource := R} = State,
		      #presence{type = unavailable} = Pres) ->
    Status = xmpp:get_text(Pres#presence.status),
    Info = [{ip, IP}, {conn, Conn}, {auth_module, AuthMod}],
    ejabberd_sm:unset_presence(SID, U, S, R, Status, Info),
    State1 = broadcast_presence_unavailable(State, Pres),
    State2 = maps:remove(pres_last, maps:remove(pres_timestamp, State1)),
    {noreply, State2};
process_self_presence(#{lserver := LServer} = State,
		      #presence{type = available} = Pres) ->
    PreviousPres = maps:get(pres_last, State, undefined),
    update_priority(State, Pres),
    State1 = ejabberd_hooks:run_fold(user_available_hook, LServer, State, [Pres]),
    State2 = State1#{pres_last => Pres,
		     pres_timestamp => p1_time_compat:timestamp()},
    FromUnavailable = PreviousPres == undefined,
    State3 = broadcast_presence_available(State2, Pres, FromUnavailable),
    {noreply, State3};
process_self_presence(State, _Pres) ->
    {noreply, State}.

-spec update_priority(state(), presence()) -> ok.
update_priority(#{ip := IP, conn := Conn, auth_module := AuthMod,
		  sid := SID, user := U, server := S, resource := R},
		Pres) ->
    Priority = get_priority_from_presence(Pres),
    Info = [{ip, IP}, {conn, Conn}, {auth_module, AuthMod}],
    ejabberd_sm:set_presence(SID, U, S, R, Priority, Pres, Info).

-spec broadcast_presence_unavailable(state(), presence()) -> state().
broadcast_presence_unavailable(#{pres_a := PresA} = State, Pres) ->
    JIDs = filter_blocked(State, Pres, PresA),
    route_multiple(State, JIDs, Pres),
    State#{pres_a => ?SETS:new()}.

-spec broadcast_presence_available(state(), presence(), boolean()) -> state().
broadcast_presence_available(#{pres_a := PresA, pres_f := PresF,
			       pres_t := PresT} = State,
			     Pres, _FromUnavailable = true) ->
    Probe = #presence{type = probe},
    TJIDs = filter_blocked(State, Probe, PresT),
    FJIDs = filter_blocked(State, Pres, PresF),
    route_multiple(State, TJIDs, Probe),
    route_multiple(State, FJIDs, Pres),
    State#{pres_a => ?SETS:union(PresA, PresF)};
broadcast_presence_available(#{pres_a := PresA, pres_f := PresF} = State,
			     Pres, _FromUnavailable = false) ->
    JIDs = filter_blocked(State, Pres, ?SETS:intersection(PresA, PresF)),
    route_multiple(State, JIDs, Pres),
    State.

-spec check_privacy_then_route(state(), stanza()) -> next_state().
check_privacy_then_route(#{lang := Lang} = State, Pkt) ->
    case privacy_check_packet(State, Pkt, out) of
        deny ->
            ErrText = <<"Your active privacy list has denied "
			"the routing of this stanza.">>,
	    Err = xmpp:err_not_acceptable(ErrText, Lang),
	    xmpp_stream_in:send_error(State, Pkt, Err);
        allow ->
	    route(Pkt),
	    {noreply, State}
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

-spec filter_blocked(state(), presence(), ?SETS:set()) -> [jid()].
filter_blocked(#{jid := From} = State, Pres, LJIDSet) ->
    ?SETS:fold(
       fun(LJID, Acc) ->
	       To = jid:make(LJID),
	       Pkt = xmpp:set_from_to(Pres, From, To),
	       case privacy_check_packet(State, Pkt, out) of
		   allow -> [To|Acc];
		   deny -> Acc
	       end
       end, [], LJIDSet).

-spec route(stanza()) -> ok.
route(Pkt) ->
    From = xmpp:get_from(Pkt),
    To = xmpp:get_to(Pkt),
    ejabberd_router:route(From, To, Pkt).

-spec route_error(stanza(), stanza_error()) -> ok.
route_error(Pkt, Err) ->
    From = xmpp:get_from(Pkt),
    To = xmpp:get_to(Pkt),
    ejabberd_router:route_error(To, From, Pkt, Err).

-spec route_multiple(state(), [jid()], stanza()) -> ok.
route_multiple(#{lserver := LServer}, JIDs, Pkt) ->
    From = xmpp:get_from(Pkt),
    ejabberd_router_multicast:route_multicast(From, LServer, JIDs, Pkt).

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

-spec change_shaper(state()) -> ok.
change_shaper(#{shaper := ShaperName, ip := IP, lserver := LServer,
		user := U, server := S, resource := R} = State) ->
    JID = jid:make(U, S, R),
    Shaper = acl:access_matches(ShaperName,
				#{usr => jid:split(JID), ip => IP},
				LServer),
    xmpp_stream_in:change_shaper(State, Shaper).

-spec do_some_magic(state(), jid()) -> state().
do_some_magic(#{pres_a := PresA, pres_f := PresF} = State, From) ->
    LFrom = jid:tolower(From),
    LBFrom = jid:remove_resource(LFrom),
    case (?SETS):is_element(LFrom, PresA) orelse
	 (?SETS):is_element(LBFrom, PresA) of
	true ->
	    State;
	false ->
	    case (?SETS):is_element(LFrom, PresF) of
		true ->
		    A = (?SETS):add_element(LFrom, PresA),
		    State#{pres_a => A};
		false ->
		    case (?SETS):is_element(LBFrom, PresF) of
			true ->
			    A = (?SETS):add_element(LBFrom, PresA),
			    State#{pres_a => A};
			false ->
			    State
		    end
	    end
    end.

-spec fsm_limit_opts([proplists:property()]) -> [proplists:property()].
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

transform_listen_option(Opt, Opts) ->
    [Opt|Opts].

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
