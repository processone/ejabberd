%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s_in.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve incoming s2s connection
%%% Created :  6 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_s2s_in).

-author('alexey@process-one.net').

-behaviour(p1_fsm).

%% External exports
-export([start/2, start_link/2, match_domain/2,
	 socket_type/0]).

%% gen_fsm callbacks
-export([init/1, wait_for_stream/2,
	 wait_for_feature_request/2, stream_established/2,
	 handle_event/3, handle_sync_event/4, code_change/4,
	 handle_info/3, print_state/1, terminate/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include_lib("public_key/include/public_key.hrl").

-define(PKIXEXPLICIT, 'OTP-PUB-KEY').

-define(PKIXIMPLICIT, 'OTP-PUB-KEY').

-include("XmppAddr.hrl").

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
	 connections = (?DICT):new() :: dict(),
         timer = make_ref()          :: reference()}).

%-define(DBGFSM, true).

-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

-endif.

%% Module start with or without supervisor:
-ifdef(NO_TRANSIENT_SUPERVISORS).

-define(SUPERVISOR_START,
	p1_fsm:start(ejabberd_s2s_in, [SockData, Opts],
                     ?FSMOPTS ++ fsm_limit_opts(Opts))).

-else.

-define(SUPERVISOR_START,
	supervisor:start_child(ejabberd_s2s_in_sup,
			       [SockData, Opts])).

-endif.

-define(STREAM_HEADER(Version),
	<<"<?xml version='1.0'?><stream:stream "
	  "xmlns:stream='http://etherx.jabber.org/stream"
	  "s' xmlns='jabber:server' xmlns:db='jabber:ser"
	  "ver:dialback' id='",
	  (StateData#state.streamid)/binary, "'", Version/binary,
	  ">">>).

-define(STREAM_TRAILER, <<"</stream:stream>">>).

-define(INVALID_NAMESPACE_ERR,
	xml:element_to_binary(?SERR_INVALID_NAMESPACE)).

-define(HOST_UNKNOWN_ERR,
	xml:element_to_binary(?SERR_HOST_UNKNOWN)).

-define(INVALID_FROM_ERR,
	xml:element_to_binary(?SERR_INVALID_FROM)).

-define(INVALID_XML_ERR,
	xml:element_to_binary(?SERR_XML_NOT_WELL_FORMED)).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(SockData, Opts) -> ?SUPERVISOR_START.

start_link(SockData, Opts) ->
    p1_fsm:start_link(ejabberd_s2s_in, [SockData, Opts],
                      ?FSMOPTS ++ fsm_limit_opts(Opts)).

socket_type() -> xml_stream.

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
    TLSOpts = case proplists:get_bool(tls_compression, Opts) of
                  false -> [compression_none | TLSOpts3];
                  true -> TLSOpts3
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

wait_for_stream({xmlstreamstart, _Name, Attrs},
		StateData) ->
    case {xml:get_attr_s(<<"xmlns">>, Attrs),
	  xml:get_attr_s(<<"xmlns:db">>, Attrs),
	  xml:get_attr_s(<<"to">>, Attrs),
	  xml:get_attr_s(<<"version">>, Attrs) == <<"1.0">>}
	of
      {<<"jabber:server">>, _, Server, true}
	  when StateData#state.tls and
		 not StateData#state.authenticated ->
	  send_text(StateData,
		    ?STREAM_HEADER(<<" version='1.0'">>)),
	  SASL = if StateData#state.tls_enabled ->
			case
			  (StateData#state.sockmod):get_peer_certificate(StateData#state.socket)
			    of
			  {ok, Cert} ->
			      case
				(StateData#state.sockmod):get_verify_result(StateData#state.socket)
				  of
				0 ->
				    [#xmlel{name = <<"mechanisms">>,
					    attrs = [{<<"xmlns">>, ?NS_SASL}],
					    children =
						[#xmlel{name = <<"mechanism">>,
							attrs = [],
							children =
							    [{xmlcdata,
							      <<"EXTERNAL">>}]}]}];
				CertVerifyRes ->
				    case StateData#state.tls_certverify of
				      true ->
					  {error_cert_verif, CertVerifyRes,
					   Cert};
				      false -> []
				    end
			      end;
			  error -> []
			end;
		    true -> []
		 end,
	  StartTLS = if StateData#state.tls_enabled -> [];
			not StateData#state.tls_enabled and
			  not StateData#state.tls_required ->
			    [#xmlel{name = <<"starttls">>,
				    attrs = [{<<"xmlns">>, ?NS_TLS}],
				    children = []}];
			not StateData#state.tls_enabled and
			  StateData#state.tls_required ->
			    [#xmlel{name = <<"starttls">>,
				    attrs = [{<<"xmlns">>, ?NS_TLS}],
				    children =
					[#xmlel{name = <<"required">>,
						attrs = [], children = []}]}]
		     end,
	  case SASL of
	    {error_cert_verif, CertVerifyResult, Certificate} ->
		CertError = p1_tls:get_cert_verify_string(CertVerifyResult,
						       Certificate),
		RemoteServer = xml:get_attr_s(<<"from">>, Attrs),
		?INFO_MSG("Closing s2s connection: ~s <--> ~s (~s)",
			  [StateData#state.server, RemoteServer, CertError]),
		send_text(StateData,
			  xml:element_to_binary(?SERRT_POLICY_VIOLATION(<<"en">>,
									CertError))),
		{atomic, Pid} =
		    ejabberd_s2s:find_connection(jlib:make_jid(<<"">>,
							       Server, <<"">>),
						 jlib:make_jid(<<"">>,
							       RemoteServer,
							       <<"">>)),
		ejabberd_s2s_out:stop_connection(Pid),
		{stop, normal, StateData};
	    _ ->
		send_element(StateData,
			     #xmlel{name = <<"stream:features">>, attrs = [],
				    children =
					SASL ++
					  StartTLS ++
					    ejabberd_hooks:run_fold(s2s_stream_features,
								    Server, [],
								    [Server])}),
		{next_state, wait_for_feature_request,
		 StateData#state{server = Server}}
	  end;
      {<<"jabber:server">>, _, Server, true}
	  when StateData#state.authenticated ->
	  send_text(StateData,
		    ?STREAM_HEADER(<<" version='1.0'">>)),
	  send_element(StateData,
		       #xmlel{name = <<"stream:features">>, attrs = [],
			      children =
				  ejabberd_hooks:run_fold(s2s_stream_features,
							  Server, [],
							  [Server])}),
	  {next_state, stream_established, StateData};
      {<<"jabber:server">>, <<"jabber:server:dialback">>,
       _Server, _} when
	      (StateData#state.tls_required and StateData#state.tls_enabled)
	      or (not StateData#state.tls_required) ->
	  send_text(StateData, ?STREAM_HEADER(<<"">>)),
	  {next_state, stream_established, StateData};
      _ ->
	  send_text(StateData, ?INVALID_NAMESPACE_ERR),
	  {stop, normal, StateData}
    end;
wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      <<(?STREAM_HEADER(<<"">>))/binary,
		(?INVALID_XML_ERR)/binary, (?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
wait_for_stream(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.

wait_for_feature_request({xmlstreamelement, El},
			 StateData) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} = El,
    TLS = StateData#state.tls,
    TLSEnabled = StateData#state.tls_enabled,
    SockMod =
	(StateData#state.sockmod):get_sockmod(StateData#state.socket),
    case {xml:get_attr_s(<<"xmlns">>, Attrs), Name} of
      {?NS_TLS, <<"starttls">>}
	  when TLS == true, TLSEnabled == false,
	       SockMod == gen_tcp ->
	  ?DEBUG("starttls", []),
	  Socket = StateData#state.socket,
	  TLSOpts1 = case
		      ejabberd_config:get_option(
                        {domain_certfile, StateData#state.server},
                        fun iolist_to_binary/1) of
		      undefined -> StateData#state.tls_options;
		      CertFile ->
			  [{certfile, CertFile} | lists:keydelete(certfile, 1,
								  StateData#state.tls_options)]
		    end,
          TLSOpts = case ejabberd_config:get_option(
                           {s2s_tls_compression, StateData#state.server},
                           fun(true) -> true;
                              (false) -> false
                           end, true) of
                        true -> lists:delete(compression_none, TLSOpts1);
                        false -> [compression_none | TLSOpts1]
                    end,
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
	  {next_state, wait_for_stream,
	   StateData#state{socket = TLSSocket, streamid = new_id(),
			   tls_enabled = true, tls_options = TLSOpts}};
      {?NS_SASL, <<"auth">>} when TLSEnabled ->
	  Mech = xml:get_attr_s(<<"mechanism">>, Attrs),
	  case Mech of
	    <<"EXTERNAL">> ->
		Auth = jlib:decode_base64(xml:get_cdata(Els)),
		AuthDomain = jlib:nameprep(Auth),
		AuthRes = case
			    (StateData#state.sockmod):get_peer_certificate(StateData#state.socket)
			      of
			    {ok, Cert} ->
				case
				  (StateData#state.sockmod):get_verify_result(StateData#state.socket)
				    of
				  0 ->
				      case AuthDomain of
					error -> false;
					_ ->
					    case
					      idna:domain_utf8_to_ascii(AuthDomain)
						of
					      false -> false;
					      PCAuthDomain ->
						  lists:any(fun (D) ->
								    match_domain(PCAuthDomain,
										 D)
							    end,
							    get_cert_domains(Cert))
					    end
				      end;
				  _ -> false
				end;
			    error -> false
			  end,
		AllowRemoteHost = ejabberd_s2s:allow_host(<<"">>,
							  AuthDomain),
		if AuthRes andalso AllowRemoteHost ->
		       (StateData#state.sockmod):reset_stream(StateData#state.socket),
		       send_element(StateData,
				    #xmlel{name = <<"success">>,
					   attrs = [{<<"xmlns">>, ?NS_SASL}],
					   children = []}),
		       ?DEBUG("(~w) Accepted s2s authentication for ~s",
			      [StateData#state.socket, AuthDomain]),
		       change_shaper(StateData, <<"">>,
				     jlib:make_jid(<<"">>, AuthDomain, <<"">>)),
		       {next_state, wait_for_stream,
			StateData#state{streamid = new_id(),
					authenticated = true,
					auth_domain = AuthDomain}};
		   true ->
		       send_element(StateData,
				    #xmlel{name = <<"failure">>,
					   attrs = [{<<"xmlns">>, ?NS_SASL}],
					   children = []}),
		       send_text(StateData, ?STREAM_TRAILER),
		       {stop, normal, StateData}
		end;
	    _ ->
		send_element(StateData,
			     #xmlel{name = <<"failure">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children =
					[#xmlel{name = <<"invalid-mechanism">>,
						attrs = [], children = []}]}),
		{stop, normal, StateData}
	  end;
      _ ->
	  stream_established({xmlstreamelement, El}, StateData)
    end;
wait_for_feature_request({xmlstreamend, _Name},
			 StateData) ->
    send_text(StateData, ?STREAM_TRAILER),
    {stop, normal, StateData};
wait_for_feature_request({xmlstreamerror, _},
			 StateData) ->
    send_text(StateData,
	      <<(?INVALID_XML_ERR)/binary,
		(?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
wait_for_feature_request(closed, StateData) ->
    {stop, normal, StateData}.

stream_established({xmlstreamelement, El}, StateData) ->
    cancel_timer(StateData#state.timer),
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
    case is_key_packet(El) of
      {key, To, From, Id, Key} ->
	  ?DEBUG("GET KEY: ~p", [{To, From, Id, Key}]),
	  LTo = jlib:nameprep(To),
	  LFrom = jlib:nameprep(From),
	  case {ejabberd_s2s:allow_host(LTo, LFrom),
		lists:member(LTo,
			     ejabberd_router:dirty_get_all_domains())}
	      of
	    {true, true} ->
		ejabberd_s2s_out:terminate_if_waiting_delay(LTo, LFrom),
		ejabberd_s2s_out:start(LTo, LFrom,
				       {verify, self(), Key,
					StateData#state.streamid}),
		Conns = (?DICT):store({LFrom, LTo},
				      wait_for_verification,
				      StateData#state.connections),
		change_shaper(StateData, LTo,
			      jlib:make_jid(<<"">>, LFrom, <<"">>)),
		{next_state, stream_established,
		 StateData#state{connections = Conns, timer = Timer}};
	    {_, false} ->
		send_text(StateData, ?HOST_UNKNOWN_ERR),
		{stop, normal, StateData};
	    {false, _} ->
		send_text(StateData, ?INVALID_FROM_ERR),
		{stop, normal, StateData}
	  end;
      {verify, To, From, Id, Key} ->
	  ?DEBUG("VERIFY KEY: ~p", [{To, From, Id, Key}]),
	  LTo = jlib:nameprep(To),
	  LFrom = jlib:nameprep(From),
	  Type = case ejabberd_s2s:has_key({LTo, LFrom}, Key) of
		   true -> <<"valid">>;
		   _ -> <<"invalid">>
		 end,
	  send_element(StateData,
		       #xmlel{name = <<"db:verify">>,
			      attrs =
				  [{<<"from">>, To}, {<<"to">>, From},
				   {<<"id">>, Id}, {<<"type">>, Type}],
			      children = []}),
	  {next_state, stream_established,
	   StateData#state{timer = Timer}};
      _ ->
	  NewEl = jlib:remove_attr(<<"xmlns">>, El),
	  #xmlel{name = Name, attrs = Attrs} = NewEl,
	  From_s = xml:get_attr_s(<<"from">>, Attrs),
	  From = jlib:string_to_jid(From_s),
	  To_s = xml:get_attr_s(<<"to">>, Attrs),
	  To = jlib:string_to_jid(To_s),
	  if (To /= error) and (From /= error) ->
		 LFrom = From#jid.lserver,
		 LTo = To#jid.lserver,
		 if StateData#state.authenticated ->
			case LFrom == StateData#state.auth_domain andalso
			       lists:member(LTo,
					    ejabberd_router:dirty_get_all_domains())
			    of
			  true ->
			      if (Name == <<"iq">>) or (Name == <<"message">>)
				   or (Name == <<"presence">>) ->
				     ejabberd_hooks:run(s2s_receive_packet, LTo,
							[From, To, NewEl]),
				     ejabberd_router:route(From, To, NewEl);
				 true -> error
			      end;
			  false -> error
			end;
		    true ->
			case (?DICT):find({LFrom, LTo},
					  StateData#state.connections)
			    of
			  {ok, established} ->
			      if (Name == <<"iq">>) or (Name == <<"message">>)
				   or (Name == <<"presence">>) ->
				     ejabberd_hooks:run(s2s_receive_packet, LTo,
							[From, To, NewEl]),
				     ejabberd_router:route(From, To, NewEl);
				 true -> error
			      end;
			  _ -> error
			end
		 end;
	     true -> error
	  end,
	  ejabberd_hooks:run(s2s_loop_debug,
			     [{xmlstreamelement, El}]),
	  {next_state, stream_established,
	   StateData#state{timer = Timer}}
    end;
stream_established({valid, From, To}, StateData) ->
    send_element(StateData,
		 #xmlel{name = <<"db:result">>,
			attrs =
			    [{<<"from">>, To}, {<<"to">>, From},
			     {<<"type">>, <<"valid">>}],
			children = []}),
    LFrom = jlib:nameprep(From),
    LTo = jlib:nameprep(To),
    NSD = StateData#state{connections =
			      (?DICT):store({LFrom, LTo}, established,
					    StateData#state.connections)},
    {next_state, stream_established, NSD};
stream_established({invalid, From, To}, StateData) ->
    send_element(StateData,
		 #xmlel{name = <<"db:result">>,
			attrs =
			    [{<<"from">>, To}, {<<"to">>, From},
			     {<<"type">>, <<"invalid">>}],
			children = []}),
    LFrom = jlib:nameprep(From),
    LTo = jlib:nameprep(To),
    NSD = StateData#state{connections =
			      (?DICT):erase({LFrom, LTo},
					    StateData#state.connections)},
    {next_state, stream_established, NSD};
stream_established({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};
stream_established({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      <<(?INVALID_XML_ERR)/binary,
		(?STREAM_TRAILER)/binary>>),
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
%% Returns: The associated StateData for this connection
%%   {reply, Reply, NextStateName, NextStateData}
%%   Reply = {state_infos, [{InfoName::atom(), InfoValue::any()]
%%----------------------------------------------------------------------

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
    ?DEBUG("terminated: ~p", [Reason]),
    case Reason of
      {process_limit, _} ->
	  [ejabberd_s2s:external_host_overloaded(Host)
	   || Host <- get_external_hosts(StateData)];
      _ -> ok
    end,
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

%%----------------------------------------------------------------------
%% Func: print_state/1
%% Purpose: Prepare the state to be printed on error log
%% Returns: State to print
%%----------------------------------------------------------------------
print_state(State) -> State.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

send_text(StateData, Text) ->
    (StateData#state.sockmod):send(StateData#state.socket,
				   Text).

send_element(StateData, El) ->
    send_text(StateData, xml:element_to_binary(El)).

change_shaper(StateData, Host, JID) ->
    Shaper = acl:match_rule(Host, StateData#state.shaper,
			    JID),
    (StateData#state.sockmod):change_shaper(StateData#state.socket,
					    Shaper).

new_id() -> randoms:get_string().

cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    receive {timeout, Timer, _} -> ok after 0 -> ok end.

is_key_packet(#xmlel{name = Name, attrs = Attrs,
		     children = Els})
    when Name == <<"db:result">> ->
    {key, xml:get_attr_s(<<"to">>, Attrs),
     xml:get_attr_s(<<"from">>, Attrs),
     xml:get_attr_s(<<"id">>, Attrs), xml:get_cdata(Els)};
is_key_packet(#xmlel{name = Name, attrs = Attrs,
		     children = Els})
    when Name == <<"db:verify">> ->
    {verify, xml:get_attr_s(<<"to">>, Attrs),
     xml:get_attr_s(<<"from">>, Attrs),
     xml:get_attr_s(<<"id">>, Attrs), xml:get_cdata(Els)};
is_key_packet(_) -> false.

get_cert_domains(Cert) ->
    {rdnSequence, Subject} =
	(Cert#'Certificate'.tbsCertificate)#'TBSCertificate'.subject,
    Extensions =
	(Cert#'Certificate'.tbsCertificate)#'TBSCertificate'.extensions,
    lists:flatmap(fun (#'AttributeTypeAndValue'{type =
						    ?'id-at-commonName',
						value = Val}) ->
			  case 'OTP-PUB-KEY':decode('X520CommonName', Val) of
			    {ok, {_, D1}} ->
				D = if is_binary(D1) -> D1;
				       is_binary(D1) -> (D1);
				       true -> error
				    end,
				if D /= error ->
				       case jlib:string_to_jid(D) of
					 #jid{luser = <<"">>, lserver = LD,
					      lresource = <<"">>} ->
					     [LD];
					 _ -> []
				       end;
				   true -> []
				end;
			    _ -> []
			  end;
		      (_) -> []
		  end,
		  lists:flatten(Subject))
      ++
      lists:flatmap(fun (#'Extension'{extnID =
					  ?'id-ce-subjectAltName',
				      extnValue = Val}) ->
			    BVal = if is_binary(Val) -> iolist_to_binary(Val);
				      is_binary(Val) -> Val;
				      true -> Val
				   end,
			    case 'OTP-PUB-KEY':decode('SubjectAltName', BVal)
				of
			      {ok, SANs} ->
				  lists:flatmap(fun ({otherName,
						      #'AnotherName'{'type-id' =
									 ?'id-on-xmppAddr',
								     value =
									 XmppAddr}}) ->
							case
							  'XmppAddr':decode('XmppAddr',
									    XmppAddr)
							    of
							  {ok, D}
							      when
								is_binary(D) ->
							      case
								jlib:string_to_jid((D))
								  of
								#jid{luser =
									 <<"">>,
								     lserver =
									 LD,
								     lresource =
									 <<"">>} ->
								    case
								      idna:domain_utf8_to_ascii(LD)
									of
								      false ->
									  [];
								      PCLD ->
									  [PCLD]
								    end;
								_ -> []
							      end;
							  _ -> []
							end;
						    ({dNSName, D})
							when is_binary(D) ->
							case
							  jlib:string_to_jid(D)
							    of
							  #jid{luser = <<"">>,
							       lserver = LD,
							       lresource =
								   <<"">>} ->
							      [LD];
							  _ -> []
							end;
						    (_) -> []
						end,
						SANs);
			      _ -> []
			    end;
			(_) -> []
		    end,
		    Extensions).

match_domain(Domain, Domain) -> true;
match_domain(Domain, Pattern) ->
    DLabels = str:tokens(Domain, <<".">>),
    PLabels = str:tokens(Pattern, <<".">>),
    match_labels(DLabels, PLabels).

match_labels([], []) -> true;
match_labels([], [_ | _]) -> false;
match_labels([_ | _], []) -> false;
match_labels([DL | DLabels], [PL | PLabels]) ->
    case lists:all(fun (C) ->
			   $a =< C andalso C =< $z orelse
			     $0 =< C andalso C =< $9 orelse
			       C == $- orelse C == $*
		   end,
		   binary_to_list(PL))
	of
      true ->
	  Regexp = ejabberd_regexp:sh_to_awk(PL),
	  case ejabberd_regexp:run(DL, Regexp) of
	    match -> match_labels(DLabels, PLabels);
	    nomatch -> false
	  end;
      false -> false
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
