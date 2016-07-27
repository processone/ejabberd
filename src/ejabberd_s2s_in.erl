%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s_in.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve incoming s2s connection
%%% Created :  6 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_s2s_in).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-behaviour(p1_fsm).

%% External exports
-export([start/2, start_link/2, socket_type/0]).

-export([init/1, wait_for_stream/2,
	 wait_for_feature_request/2, stream_established/2,
	 handle_event/3, handle_sync_event/4, code_change/4,
	 handle_info/3, print_state/1, terminate/3, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

-define(DICT, dict).

-record(state,
	{socket                      :: ejabberd_socket:socket_state(),
         sockmod = ejabberd_socket   :: ejabberd_socket | ejabberd_frontend_socket,
         streamid = <<"">>           :: binary(),
         shaper = none               :: shaper:shaper(),
         tls = false                 :: boolean(),
	 tls_enabled = false         :: boolean(),
         tls_required = false        :: boolean(),
	 tls_certverify = false      :: boolean(),
         tls_options = []            :: list(),
         server = <<"">>             :: binary(),
	 authenticated = false       :: boolean(),
         auth_domain = <<"">>        :: binary(),
	 connections = (?DICT):new() :: ?TDICT,
         timer = make_ref()          :: reference()}).

-type state_name() :: wait_for_stream | wait_for_features | stream_established.
-type state() :: #state{}.
-type fsm_next() :: {next_state, state_name(), state()}.
-type fsm_stop() :: {stop, normal, state()}.
-type fsm_transition() :: fsm_stop() | fsm_next().

%%-define(DBGFSM, true).
-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

start(SockData, Opts) ->
    supervisor:start_child(ejabberd_s2s_in_sup,
                            [SockData, Opts]).

start_link(SockData, Opts) ->
    p1_fsm:start_link(ejabberd_s2s_in, [SockData, Opts],
                      ?FSMOPTS ++ fsm_limit_opts(Opts)).

socket_type() -> xml_stream.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

init([{SockMod, Socket}, Opts]) ->
    ?DEBUG("started: ~p", [{SockMod, Socket}]),
    Shaper = case lists:keysearch(shaper, 1, Opts) of
	       {value, {_, S}} -> S;
	       _ -> none
	     end,
    {StartTLS, TLSRequired, TLSCertverify} =
        case ejabberd_config:get_option(
               s2s_use_starttls,
               fun(false) -> false;
                  (true) -> true;
                  (optional) -> optional;
                  (required) -> required;
                  (required_trusted) -> required_trusted
               end,
               false) of
            UseTls
              when (UseTls == undefined) or
                   (UseTls == false) ->
                {false, false, false};
            UseTls
              when (UseTls == true) or
                   (UseTls ==
                        optional) ->
                {true, false, false};
            required -> {true, true, false};
            required_trusted ->
                {true, true, true}
        end,
    TLSOpts1 = case ejabberd_config:get_option(
                     s2s_certfile,
                     fun iolist_to_binary/1) of
                  undefined -> [];
                  CertFile -> [{certfile, CertFile}]
	      end,
    TLSOpts2 = case ejabberd_config:get_option(
                      s2s_ciphers, fun iolist_to_binary/1) of
                   undefined -> TLSOpts1;
                   Ciphers -> [{ciphers, Ciphers} | TLSOpts1]
               end,
    TLSOpts3 = case ejabberd_config:get_option(
                      s2s_protocol_options,
                      fun (Options) ->
                              [_|O] = lists:foldl(
                                           fun(X, Acc) -> X ++ Acc end, [],
                                           [["|" | binary_to_list(Opt)] || Opt <- Options, is_binary(Opt)]
                                          ),
                              iolist_to_binary(O)
                      end) of
                   undefined -> TLSOpts2;
                   ProtocolOpts -> [{protocol_options, ProtocolOpts} | TLSOpts2]
               end,
    TLSOpts4 = case ejabberd_config:get_option(
                      s2s_dhfile, fun iolist_to_binary/1) of
                   undefined -> TLSOpts3;
                   DHFile -> [{dhfile, DHFile} | TLSOpts3]
               end,
    TLSOpts = case proplists:get_bool(tls_compression, Opts) of
                  false -> [compression_none | TLSOpts4];
                  true -> TLSOpts4
              end,
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
    {ok, wait_for_stream,
     #state{socket = Socket, sockmod = SockMod,
	    streamid = new_id(), shaper = Shaper, tls = StartTLS,
	    tls_enabled = false, tls_required = TLSRequired,
	    tls_certverify = TLSCertverify, tls_options = TLSOpts,
	    timer = Timer}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
wait_for_stream({xmlstreamstart, Name, Attrs}, StateData) ->
    try xmpp:decode(#xmlel{name = Name, attrs = Attrs}) of
	#stream_start{xmlns = NS_SERVER, stream_xmlns = NS_STREAM}
	  when NS_SERVER /= ?NS_SERVER; NS_STREAM /= ?NS_STREAM ->
	    send_header(StateData, <<" version='1.0'">>),
	    send_element(StateData, xmpp:serr_invalid_namespace()),
	    {stop, normal, StateData};
	#stream_start{to = #jid{lserver = Server},
		      from = #jid{lserver = From},
		      version = <<"1.0">>}
	  when StateData#state.tls and not StateData#state.authenticated ->
	    send_header(StateData, <<" version='1.0'">>),
	    Auth = if StateData#state.tls_enabled ->
			   {Result, Message} =
			       ejabberd_s2s:check_peer_certificate(
				 StateData#state.sockmod,
				 StateData#state.socket,
				 From),
			   {Result, From, Message};
		      true ->
			   {no_verify, <<"(unknown)">>, <<"TLS not (yet) enabled">>}
		   end,
	    StartTLS = if StateData#state.tls_enabled -> [];
			  not StateData#state.tls_enabled and
			  not StateData#state.tls_required ->
			       [#starttls{required = false}];
			  not StateData#state.tls_enabled and
			  StateData#state.tls_required ->
			       [#starttls{required = true}]
		       end,
	    case Auth of
		{error, RemoteServer, CertError}
		  when StateData#state.tls_certverify ->
		    ?INFO_MSG("Closing s2s connection: ~s <--> ~s (~s)",
			      [StateData#state.server, RemoteServer, CertError]),
		    send_element(StateData,
				 xmpp:serr_policy_violation(CertError, ?MYLANG)),
		    {stop, normal, StateData};
		{VerifyResult, RemoteServer, Msg} ->
		    {SASL, NewStateData} =
			case VerifyResult of
			    ok ->
				{[#sasl_mechanisms{list = [<<"EXTERNAL">>]}],
				 StateData#state{auth_domain = RemoteServer}};
			    error ->
				?DEBUG("Won't accept certificate of ~s: ~s",
				       [RemoteServer, Msg]),
				{[], StateData};
			    no_verify ->
				{[], StateData}
			end,
		    send_element(NewStateData,
				 #stream_features{
				    sub_els = SASL ++ StartTLS ++
					ejabberd_hooks:run_fold(
					  s2s_stream_features, Server, [],
					  [Server])}),
		    {next_state, wait_for_feature_request,
		     NewStateData#state{server = Server}}
	    end;
	#stream_start{to = #jid{lserver = Server},
		      version = <<"1.0">>} when StateData#state.authenticated ->
	    send_header(StateData, <<" version='1.0'">>),
	    send_element(StateData,
			 #stream_features{
			    sub_els = ejabberd_hooks:run_fold(
					s2s_stream_features, Server, [],
					[Server])}),
	    {next_state, stream_established, StateData};
	#stream_start{db_xmlns = ?NS_SERVER_DIALBACK}
	  when (StateData#state.tls_required and StateData#state.tls_enabled)
	       or (not StateData#state.tls_required) ->
	    send_header(StateData, <<"">>),
	    {next_state, stream_established, StateData};
	#stream_start{} ->
	    send_header(StateData, <<" version='1.0'">>),
	    send_element(StateData, xmpp:serr_undefined_condition()),
	    {stop, normal, StateData}
    catch _:{xmpp_codec, Why} ->
	    Txt = xmpp:format_error(Why),
	    send_header(StateData, <<" version='1.0'">>),
	    send_element(StateData, xmpp:serr_not_well_formed(Txt, ?MYLANG)),
	    {stop, normal, StateData}
    end;
wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_header(StateData, <<"">>),
    send_element(StateData, xmpp:serr_not_well_formed()),
    {stop, normal, StateData};
wait_for_stream(timeout, StateData) ->
    send_header(StateData, <<"">>),
    send_element(StateData, xmpp:serr_connection_timeout()),
    {stop, normal, StateData};
wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.

wait_for_feature_request({xmlstreamelement, El}, StateData) ->
    decode_element(El, wait_for_feature_request, StateData);
wait_for_feature_request(#starttls{},
			 #state{tls = true, tls_enabled = false} = StateData) ->
    case (StateData#state.sockmod):get_sockmod(StateData#state.socket) of
	gen_tcp ->
	    ?DEBUG("starttls", []),
	    Socket = StateData#state.socket,
	    TLSOpts1 = case
			   ejabberd_config:get_option(
			     {domain_certfile, StateData#state.server},
			     fun iolist_to_binary/1) of
			   undefined -> StateData#state.tls_options;
			   CertFile ->
			       lists:keystore(certfile, 1,
					      StateData#state.tls_options,
					      {certfile, CertFile})
		       end,
	    TLSOpts = case ejabberd_config:get_option(
			     {s2s_tls_compression, StateData#state.server},
			     fun(true) -> true;
				(false) -> false
			     end, false) of
			  true -> lists:delete(compression_none, TLSOpts1);
			  false -> [compression_none | TLSOpts1]
		      end,
	    TLSSocket = (StateData#state.sockmod):starttls(
			  Socket, TLSOpts,
			  fxml:element_to_binary(#starttls_proceed{})),
	    {next_state, wait_for_stream,
	     StateData#state{socket = TLSSocket, streamid = new_id(),
			     tls_enabled = true, tls_options = TLSOpts}};
	_ ->
            Txt = <<"Unsupported TLS transport">>,
            send_element(StateData, xmpp:serr_policy_violation(Txt, ?MYLANG)),
            {stop, normal, StateData}
    end;
wait_for_feature_request(#sasl_auth{mechanism = Mech},
			 #state{tls_enabled = true} = StateData) ->
    case Mech of
	<<"EXTERNAL">> when StateData#state.auth_domain /= <<"">> ->
	    AuthDomain = StateData#state.auth_domain,
	    AllowRemoteHost = ejabberd_s2s:allow_host(<<"">>, AuthDomain),
	    if AllowRemoteHost ->
		    (StateData#state.sockmod):reset_stream(StateData#state.socket),
		    send_element(StateData, #sasl_success{}),
		    ?INFO_MSG("Accepted s2s EXTERNAL authentication for ~s (TLS=~p)",
			      [AuthDomain, StateData#state.tls_enabled]),
		    change_shaper(StateData, <<"">>, jid:make(AuthDomain)),
		    {next_state, wait_for_stream,
		     StateData#state{streamid = new_id(),
				     authenticated = true}};
	       true ->
		    send_element(StateData, #sasl_failure{}),
		    {stop, normal, StateData}
	    end;
	_ ->
	    send_element(StateData, #sasl_failure{reason = 'invalid-mechanism'}),
	    {stop, normal, StateData}
    end;
wait_for_feature_request({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};
wait_for_feature_request({xmlstreamerror, _}, StateData) ->
    send_element(StateData, xmpp:serr_not_well_formed()),
    {stop, normal, StateData};
wait_for_feature_request(closed, StateData) ->
    {stop, normal, StateData};
wait_for_feature_request(_Pkt, #state{tls_required = TLSRequired,
				      tls_enabled = TLSEnabled} = StateData)
  when TLSRequired and not TLSEnabled ->
    Txt = <<"Use of STARTTLS required">>,
    send_element(StateData, xmpp:serr_policy_violation(Txt, ?MYLANG)),
    {stop, normal, StateData};
wait_for_feature_request(El, StateData) ->
    stream_established({xmlstreamelement, El}, StateData).

stream_established({xmlstreamelement, El}, StateData) ->
    cancel_timer(StateData#state.timer),
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
    decode_element(El, stream_established, StateData#state{timer = Timer});
stream_established(#db_result{to = To, from = From, key = Key},
		   StateData) ->
    ?DEBUG("GET KEY: ~p", [{To, From, Key}]),
    LTo = To#jid.lserver,
    LFrom = From#jid.lserver,
    case {ejabberd_s2s:allow_host(LTo, LFrom),
	  lists:member(LTo, ejabberd_router:dirty_get_all_domains())} of
	{true, true} ->
	    ejabberd_s2s_out:terminate_if_waiting_delay(LTo, LFrom),
	    ejabberd_s2s_out:start(LTo, LFrom,
				   {verify, self(), Key,
				    StateData#state.streamid}),
	    Conns = (?DICT):store({LFrom, LTo},
				  wait_for_verification,
				  StateData#state.connections),
	    change_shaper(StateData, LTo, jid:make(LFrom)),
	    {next_state, stream_established,
	     StateData#state{connections = Conns}};
	{_, false} ->
	    send_element(StateData, xmpp:serr_host_unknown()),
	    {stop, normal, StateData};
	{false, _} ->
	    send_element(StateData, xmpp:serr_invalid_from()),
	    {stop, normal, StateData}
    end;
stream_established(#db_verify{to = To, from = From, id = Id, key = Key},
		   StateData) ->
    ?DEBUG("VERIFY KEY: ~p", [{To, From, Id, Key}]),
    LTo = jid:nameprep(To),
    LFrom = jid:nameprep(From),
    Type = case ejabberd_s2s:make_key({LTo, LFrom}, Id) of
	       Key -> valid;
	       _ -> invalid
	   end,
    send_element(StateData,
		 #db_verify{from = To, to = From, id = Id, type = Type}),
    {next_state, stream_established, StateData};
stream_established(Pkt, StateData) when ?is_stanza(Pkt) ->
    From = xmpp:get_from(Pkt),
    To = xmpp:get_to(Pkt),
    if To /= undefined, From /= undefined ->
	    LFrom = From#jid.lserver,
	    LTo = To#jid.lserver,
	    if StateData#state.authenticated ->
		    case LFrom == StateData#state.auth_domain andalso
			lists:member(LTo, ejabberd_router:dirty_get_all_domains()) of
			true ->
			    ejabberd_hooks:run(s2s_receive_packet, LTo,
					       [From, To, Pkt]),
			    ejabberd_router:route(From, To, Pkt);
			false ->
			    send_error(StateData, Pkt, xmpp:err_not_authorized())
		    end;
	       true ->
		    case (?DICT):find({LFrom, LTo}, StateData#state.connections) of
			{ok, established} ->
			    ejabberd_hooks:run(s2s_receive_packet, LTo,
					       [From, To, Pkt]),
			    ejabberd_router:route(From, To, Pkt);
			_ ->
			    send_error(StateData, Pkt, xmpp:err_not_authorized())
		    end
	    end;
       true ->
	    send_error(StateData, Pkt, xmpp:err_jid_malformed())
    end,
    ejabberd_hooks:run(s2s_loop_debug, [{xmlstreamelement, Pkt}]),
    {next_state, stream_established, StateData};
stream_established({valid, From, To}, StateData) ->
    send_element(StateData,
		 #db_result{from = To, to = From, type = valid}),
    ?INFO_MSG("Accepted s2s dialback authentication for ~s (TLS=~p)",
	      [From, StateData#state.tls_enabled]),
    LFrom = jid:nameprep(From),
    LTo = jid:nameprep(To),
    NSD = StateData#state{connections =
			      (?DICT):store({LFrom, LTo}, established,
					    StateData#state.connections)},
    {next_state, stream_established, NSD};
stream_established({invalid, From, To}, StateData) ->
    send_element(StateData,
		 #db_result{from = To, to = From, type = invalid}),
    LFrom = jid:nameprep(From),
    LTo = jid:nameprep(To),
    NSD = StateData#state{connections =
			      (?DICT):erase({LFrom, LTo},
					    StateData#state.connections)},
    {next_state, stream_established, NSD};
stream_established({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};
stream_established({xmlstreamerror, _}, StateData) ->
    send_element(StateData, xmpp:serr_not_well_formed()),
    {stop, normal, StateData};
stream_established(timeout, StateData) ->
    send_element(StateData, xmpp:serr_connection_timeout()),
    {stop, normal, StateData};
stream_established(closed, StateData) ->
    {stop, normal, StateData};
stream_established(Pkt, StateData) ->
    ejabberd_hooks:run(s2s_loop_debug, [{xmlstreamelement, Pkt}]),
    {next_state, stream_established, StateData}.

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

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(get_state_infos, _From, StateName,
		  StateData) ->
    SockMod = StateData#state.sockmod,
    {Addr, Port} = try
		     SockMod:peername(StateData#state.socket)
		   of
		     {ok, {A, P}} -> {A, P};
		     {error, _} -> {unknown, unknown}
		   catch
		     _:_ -> {unknown, unknown}
		   end,
    Domains = get_external_hosts(StateData),
    Infos = [{direction, in}, {statename, StateName},
	     {addr, Addr}, {port, Port},
	     {streamid, StateData#state.streamid},
	     {tls, StateData#state.tls},
	     {tls_enabled, StateData#state.tls_enabled},
	     {tls_options, StateData#state.tls_options},
	     {authenticated, StateData#state.authenticated},
	     {shaper, StateData#state.shaper}, {sockmod, SockMod},
	     {domains, Domains}],
    Reply = {state_infos, Infos},
    {reply, Reply, StateName, StateData};
%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName,
		  StateData) ->
    Reply = ok, {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData, Text),
    {next_state, StateName, StateData};
handle_info({timeout, Timer, _}, StateName,
	    #state{timer = Timer} = StateData) ->
    if StateName == wait_for_stream ->
	    send_header(StateData, <<"">>);
       true ->
	    ok
    end,
    send_element(StateData, xmpp:serr_connection_timeout()),
    {stop, normal, StateData};
handle_info(_, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(Reason, _StateName, StateData) ->
    ?DEBUG("terminated: ~p", [Reason]),
    case Reason of
      {process_limit, _} ->
	  [ejabberd_s2s:external_host_overloaded(Host)
	   || Host <- get_external_hosts(StateData)];
      _ -> ok
    end,
    catch send_trailer(StateData),
    (StateData#state.sockmod):close(StateData#state.socket),
    ok.

get_external_hosts(StateData) ->
    case StateData#state.authenticated of
      true -> [StateData#state.auth_domain];
      false ->
	  Connections = StateData#state.connections,
	  [D
	   || {{D, _}, established} <- dict:to_list(Connections)]
    end.

print_state(State) -> State.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec send_text(state(), iodata()) -> ok.
send_text(StateData, Text) ->
    (StateData#state.sockmod):send(StateData#state.socket,
				   Text).

-spec send_element(state(), xmpp_element()) -> ok.
send_element(StateData, El) ->
    El1 = fix_ns(xmpp:encode(El)),
    send_text(StateData, fxml:element_to_binary(El1)).

-spec send_error(state(), xmlel() | stanza(), error()) -> ok.
send_error(StateData, Stanza, Error) ->
    Type = xmpp:get_type(Stanza),
    if Type == error; Type == result;
       Type == <<"error">>; Type == <<"result">> ->
	    ok;
       true ->
	    send_element(StateData, xmpp:make_error(Stanza, Error))
    end.

-spec send_trailer(state()) -> ok.
send_trailer(StateData) ->
    send_text(StateData, <<"</stream:stream>">>).

-spec send_header(state(), binary()) -> ok.
send_header(StateData, Version) ->
    send_text(StateData,
	      <<"<?xml version='1.0'?><stream:stream "
		"xmlns:stream='http://etherx.jabber.org/stream"
		"s' xmlns='jabber:server' xmlns:db='jabber:ser"
		"ver:dialback' id='",
		(StateData#state.streamid)/binary, "'", Version/binary,
		">">>).

-spec change_shaper(state(), binary(), jid()) -> ok.
change_shaper(StateData, Host, JID) ->
    Shaper = acl:match_rule(Host, StateData#state.shaper,
			    JID),
    (StateData#state.sockmod):change_shaper(StateData#state.socket,
					    Shaper).

-spec fix_ns(xmlel()) -> xmlel().
fix_ns(#xmlel{name = Name} = El) when Name == <<"message">>;
                                      Name == <<"iq">>;
                                      Name == <<"presence">>;
                                      Name == <<"db:verify">>,
                                      Name == <<"db:result">> ->
    Attrs = lists:filter(
              fun({<<"xmlns">>, _}) -> false;
                 (_) -> true
              end, El#xmlel.attrs),
    El#xmlel{attrs = Attrs};
fix_ns(El) ->
    El.

-spec new_id() -> binary().
new_id() -> randoms:get_string().

-spec cancel_timer(reference()) -> ok.
cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    receive {timeout, Timer, _} -> ok after 0 -> ok end.

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

-spec decode_element(xmlel(), state_name(), state()) -> fsm_transition().
decode_element(#xmlel{} = El, StateName, StateData) ->
    try xmpp:decode(El) of
	Pkt -> ?MODULE:StateName(Pkt, StateData)
    catch error:{xmpp_codec, Why} ->
            case xmpp:is_stanza(El) of
                true ->
		    Lang = xmpp:get_lang(El),
                    Txt = xmpp:format_error(Why),
		    send_error(StateData, El, xmpp:err_bad_request(Txt, Lang));
                false ->
                    ok
            end,
            {next_state, StateName, StateData}
    end.

opt_type(domain_certfile) -> fun iolist_to_binary/1;
opt_type(max_fsm_queue) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(s2s_certfile) -> fun iolist_to_binary/1;
opt_type(s2s_ciphers) -> fun iolist_to_binary/1;
opt_type(s2s_dhfile) -> fun iolist_to_binary/1;
opt_type(s2s_protocol_options) ->
    fun (Options) ->
	    [_ | O] = lists:foldl(fun (X, Acc) -> X ++ Acc end, [],
				  [["|" | binary_to_list(Opt)]
				   || Opt <- Options, is_binary(Opt)]),
	    iolist_to_binary(O)
    end;
opt_type(s2s_tls_compression) ->
    fun (true) -> true;
	(false) -> false
    end;
opt_type(s2s_use_starttls) ->
    fun (false) -> false;
	(true) -> true;
	(optional) -> optional;
	(required) -> required;
	(required_trusted) -> required_trusted
    end;
opt_type(_) ->
    [domain_certfile, max_fsm_queue, s2s_certfile,
     s2s_ciphers, s2s_dhfile, s2s_protocol_options,
     s2s_tls_compression, s2s_use_starttls].
