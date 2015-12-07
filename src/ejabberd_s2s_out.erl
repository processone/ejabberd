%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s_out.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage outgoing server-to-server connections
%%% Created :  6 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-module(ejabberd_s2s_out).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-behaviour(p1_fsm).

%% External exports
-export([start/3,
	 start_link/3,
	 start_connection/1,
	 terminate_if_waiting_delay/2,
	 stop_connection/1,
	 transform_options/1]).

-export([init/1, open_socket/2, wait_for_stream/2,
	 wait_for_validation/2, wait_for_features/2,
	 wait_for_auth_result/2, wait_for_starttls_proceed/2,
	 relay_to_bridge/2, reopen_socket/2, wait_before_retry/2,
	 stream_established/2, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3,
	 print_state/1, code_change/4, test_get_addr_port/1,
	 get_addr_port/1, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-record(state,
	{socket                           :: ejabberd_socket:socket_state(),
         streamid = <<"">>                :: binary(),
         use_v10 = true                   :: boolean(),
         tls = false                      :: boolean(),
	 tls_required = false             :: boolean(),
	 tls_certverify = false           :: boolean(),
         tls_enabled = false              :: boolean(),
	 tls_options = [connect]          :: list(),
         authenticated = false            :: boolean(),
	 db_enabled = true                :: boolean(),
         try_auth = true                  :: boolean(),
         myname = <<"">>                  :: binary(),
         server = <<"">>                  :: binary(),
	 queue = queue:new()              :: ?TQUEUE,
         delay_to_retry = undefined_delay :: undefined_delay | non_neg_integer(),
         new = false                      :: false | binary(),
	 verify = false                   :: false | {pid(), binary(), binary()},
         bridge                           :: {atom(), atom()},
         timer = make_ref()               :: reference()}).

%%-define(DBGFSM, true).

-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

-endif.

-define(FSMTIMEOUT, 30000).

%% We do not block on send anymore.
-define(TCP_SEND_TIMEOUT, 15000).

%% Maximum delay to wait before retrying to connect after a failed attempt.
%% Specified in miliseconds. Default value is 5 minutes.
-define(MAX_RETRY_DELAY, 300000).

-define(STREAM_HEADER,
	<<"<?xml version='1.0'?><stream:stream "
	  "xmlns:stream='http://etherx.jabber.org/stream"
	  "s' xmlns='jabber:server' xmlns:db='jabber:ser"
	  "ver:dialback' from='~s' to='~s'~s>">>).

-define(STREAM_TRAILER, <<"</stream:stream>">>).

-define(INVALID_NAMESPACE_ERR,
	xml:element_to_binary(?SERR_INVALID_NAMESPACE)).

-define(HOST_UNKNOWN_ERR,
	xml:element_to_binary(?SERR_HOST_UNKNOWN)).

-define(INVALID_XML_ERR,
	xml:element_to_binary(?SERR_XML_NOT_WELL_FORMED)).

-define(SOCKET_DEFAULT_RESULT, {error, badarg}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(From, Host, Type) ->
    supervisor:start_child(ejabberd_s2s_out_sup,
			   [From, Host, Type]).

start_link(From, Host, Type) ->
    p1_fsm:start_link(ejabberd_s2s_out, [From, Host, Type],
		      fsm_limit_opts() ++ (?FSMOPTS)).

start_connection(Pid) -> p1_fsm:send_event(Pid, init).

stop_connection(Pid) -> p1_fsm:send_event(Pid, closed).

%%%----------------------------------------------------------------------
%%% Callback functions from p1_fsm
%%%----------------------------------------------------------------------

init([From, Server, Type]) ->
    process_flag(trap_exit, true),
    ?DEBUG("started: ~p", [{From, Server, Type}]),
    {TLS, TLSRequired, TLSCertverify} =
	case ejabberd_config:get_option(
	       s2s_use_starttls,
	       fun(true) -> true;
		  (false) -> false;
		  (optional) -> optional;
		  (required) -> required;
		  (required_trusted) -> required_trusted
	       end)
	    of
	  UseTls
	      when (UseTls == undefined) or (UseTls == false) ->
	      {false, false, false};
	  UseTls
	      when (UseTls == true) or (UseTls == optional) ->
	      {true, false, false};
	  required ->
	      {true, true, false};
	  required_trusted ->
	      {true, true, true}
	end,
    UseV10 = TLS,
    TLSOpts1 = case
		ejabberd_config:get_option(
                  s2s_certfile, fun iolist_to_binary/1)
		  of
		undefined -> [connect];
		CertFile -> [{certfile, CertFile}, connect]
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
    TLSOpts = case ejabberd_config:get_option(
                     {s2s_tls_compression, From},
                     fun(true) -> true;
                        (false) -> false
                     end, true) of
                  false -> [compression_none | TLSOpts4];
                  true -> TLSOpts4
              end,
    {New, Verify} = case Type of
		      {new, Key} -> {Key, false};
		      {verify, Pid, Key, SID} ->
			  start_connection(self()), {false, {Pid, Key, SID}}
		    end,
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
    {ok, open_socket,
     #state{use_v10 = UseV10, tls = TLS,
	    tls_required = TLSRequired, tls_certverify = TLSCertverify,
	    tls_options = TLSOpts, queue = queue:new(), myname = From,
	    server = Server, new = New, verify = Verify, timer = Timer}}.

open_socket(init, StateData) ->
    log_s2s_out(StateData#state.new, StateData#state.myname,
		StateData#state.server, StateData#state.tls),
    ?DEBUG("open_socket: ~p",
	   [{StateData#state.myname, StateData#state.server,
	     StateData#state.new, StateData#state.verify}]),
    AddrList = case
		 ejabberd_idna:domain_utf8_to_ascii(StateData#state.server)
		   of
		 false -> [];
		 ASCIIAddr -> get_addr_port(ASCIIAddr)
	       end,
    case lists:foldl(fun ({Addr, Port}, Acc) ->
			     case Acc of
			       {ok, Socket} -> {ok, Socket};
			       _ -> open_socket1(Addr, Port)
			     end
		     end,
		     ?SOCKET_DEFAULT_RESULT, AddrList)
	of
      {ok, Socket} ->
	  Version = if StateData#state.use_v10 ->
			   <<" version='1.0'">>;
		       true -> <<"">>
		    end,
	  NewStateData = StateData#state{socket = Socket,
					 tls_enabled = false,
					 streamid = new_id()},
	  send_text(NewStateData,
		    io_lib:format(?STREAM_HEADER,
				  [StateData#state.myname,
				   StateData#state.server, Version])),
	  {next_state, wait_for_stream, NewStateData,
	   ?FSMTIMEOUT};
      {error, _Reason} ->
	  ?INFO_MSG("s2s connection: ~s -> ~s (remote server "
		    "not found)",
		    [StateData#state.myname, StateData#state.server]),
	  case ejabberd_hooks:run_fold(find_s2s_bridge, undefined,
				       [StateData#state.myname,
					StateData#state.server])
	      of
	    {Mod, Fun, Type} ->
		?INFO_MSG("found a bridge to ~s for: ~s -> ~s",
			  [Type, StateData#state.myname,
			   StateData#state.server]),
		NewStateData = StateData#state{bridge = {Mod, Fun}},
		{next_state, relay_to_bridge, NewStateData};
	    _ -> wait_before_reconnect(StateData)
	  end
    end;
open_socket(closed, StateData) ->
    ?INFO_MSG("s2s connection: ~s -> ~s (stopped in "
	      "open socket)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
open_socket(timeout, StateData) ->
    ?INFO_MSG("s2s connection: ~s -> ~s (timeout in "
	      "open socket)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
open_socket(_, StateData) ->
    {next_state, open_socket, StateData}.

open_socket1({_, _, _, _} = Addr, Port) ->
    open_socket2(inet, Addr, Port);
%% IPv6
open_socket1({_, _, _, _, _, _, _, _} = Addr, Port) ->
    open_socket2(inet6, Addr, Port);
%% Hostname
open_socket1(Host, Port) ->
    lists:foldl(fun (_Family, {ok, _Socket} = R) -> R;
		    (Family, _) ->
			Addrs = get_addrs(Host, Family),
			lists:foldl(fun (_Addr, {ok, _Socket} = R) -> R;
					(Addr, _) -> open_socket1(Addr, Port)
				    end,
				    ?SOCKET_DEFAULT_RESULT, Addrs)
		end,
		?SOCKET_DEFAULT_RESULT, outgoing_s2s_families()).

open_socket2(Type, Addr, Port) ->
    ?DEBUG("s2s_out: connecting to ~p:~p~n", [Addr, Port]),
    Timeout = outgoing_s2s_timeout(),
    case catch ejabberd_socket:connect(Addr, Port,
				       [binary, {packet, 0},
					{send_timeout, ?TCP_SEND_TIMEOUT},
					{send_timeout_close, true},
					{active, false}, Type],
				       Timeout)
	of
      {ok, _Socket} = R -> R;
      {error, Reason} = R ->
	  ?DEBUG("s2s_out: connect return ~p~n", [Reason]), R;
      {'EXIT', Reason} ->
	  ?DEBUG("s2s_out: connect crashed ~p~n", [Reason]),
	  {error, Reason}
    end.

%%----------------------------------------------------------------------

wait_for_stream({xmlstreamstart, _Name, Attrs},
		StateData) ->
    {CertCheckRes, CertCheckMsg, NewStateData} =
	if StateData#state.tls_certverify, StateData#state.tls_enabled ->
	       {Res, Msg} =
		   ejabberd_s2s:check_peer_certificate(ejabberd_socket,
						       StateData#state.socket,
						       StateData#state.server),
	       ?DEBUG("Certificate verification result for ~s: ~s",
		      [StateData#state.server, Msg]),
	       {Res, Msg, StateData#state{tls_certverify = false}};
	   true ->
	       {no_verify, <<"Not verified">>, StateData}
	end,
    case {xml:get_attr_s(<<"xmlns">>, Attrs),
	  xml:get_attr_s(<<"xmlns:db">>, Attrs),
	  xml:get_attr_s(<<"version">>, Attrs) == <<"1.0">>}
	of
      _ when CertCheckRes == error ->
	  send_text(NewStateData,
		    <<(xml:element_to_binary(?SERRT_POLICY_VIOLATION(<<"en">>,
								     CertCheckMsg)))/binary,
		      (?STREAM_TRAILER)/binary>>),
	  ?INFO_MSG("Closing s2s connection: ~s -> ~s (~s)",
		    [NewStateData#state.myname,
		     NewStateData#state.server,
		     CertCheckMsg]),
	  {stop, normal, NewStateData};
      {<<"jabber:server">>, <<"jabber:server:dialback">>,
       false} ->
	  send_db_request(NewStateData);
      {<<"jabber:server">>, <<"jabber:server:dialback">>,
       true}
	  when NewStateData#state.use_v10 ->
	  {next_state, wait_for_features, NewStateData, ?FSMTIMEOUT};
      %% Clause added to handle Tigase's workaround for an old ejabberd bug:
      {<<"jabber:server">>, <<"jabber:server:dialback">>,
       true}
	  when not NewStateData#state.use_v10 ->
	  send_db_request(NewStateData);
      {<<"jabber:server">>, <<"">>, true}
	  when NewStateData#state.use_v10 ->
	  {next_state, wait_for_features,
	   NewStateData#state{db_enabled = false}, ?FSMTIMEOUT};
      {NSProvided, DB, _} ->
	  send_text(NewStateData, ?INVALID_NAMESPACE_ERR),
	  ?INFO_MSG("Closing s2s connection: ~s -> ~s (invalid "
		    "namespace).~nNamespace provided: ~p~nNamespac"
		    "e expected: \"jabber:server\"~nxmlns:db "
		    "provided: ~p~nAll attributes: ~p",
		    [NewStateData#state.myname, NewStateData#state.server,
		     NSProvided, DB, Attrs]),
	  {stop, normal, NewStateData}
    end;
wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      <<(?INVALID_XML_ERR)/binary,
		(?STREAM_TRAILER)/binary>>),
    ?INFO_MSG("Closing s2s connection: ~s -> ~s (invalid "
	      "xml)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
wait_for_stream({xmlstreamend, _Name}, StateData) ->
    ?INFO_MSG("Closing s2s connection: ~s -> ~s (xmlstreamend)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
wait_for_stream(timeout, StateData) ->
    ?INFO_MSG("Closing s2s connection: ~s -> ~s (timeout "
	      "in wait_for_stream)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
wait_for_stream(closed, StateData) ->
    ?INFO_MSG("Closing s2s connection: ~s -> ~s (close "
	      "in wait_for_stream)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData}.

wait_for_validation({xmlstreamelement, El},
		    StateData) ->
    case is_verify_res(El) of
      {result, To, From, Id, Type} ->
	  ?DEBUG("recv result: ~p", [{From, To, Id, Type}]),
	  case {Type, StateData#state.tls_enabled,
		StateData#state.tls_required}
	      of
	    {<<"valid">>, Enabled, Required}
		when (Enabled == true) or (Required == false) ->
		send_queue(StateData, StateData#state.queue),
		?INFO_MSG("Connection established: ~s -> ~s with "
			  "TLS=~p",
			  [StateData#state.myname, StateData#state.server,
			   StateData#state.tls_enabled]),
		ejabberd_hooks:run(s2s_connect_hook,
				   [StateData#state.myname,
				    StateData#state.server]),
		{next_state, stream_established,
		 StateData#state{queue = queue:new()}};
	    {<<"valid">>, Enabled, Required}
		when (Enabled == false) and (Required == true) ->
		?INFO_MSG("Closing s2s connection: ~s -> ~s (TLS "
			  "is required but unavailable)",
			  [StateData#state.myname, StateData#state.server]),
		{stop, normal, StateData};
	    _ ->
		?INFO_MSG("Closing s2s connection: ~s -> ~s (invalid "
			  "dialback key)",
			  [StateData#state.myname, StateData#state.server]),
		{stop, normal, StateData}
	  end;
      {verify, To, From, Id, Type} ->
	  ?DEBUG("recv verify: ~p", [{From, To, Id, Type}]),
	  case StateData#state.verify of
	    false ->
		NextState = wait_for_validation,
		{next_state, NextState, StateData,
		 get_timeout_interval(NextState)};
	    {Pid, _Key, _SID} ->
		case Type of
		  <<"valid">> ->
		      p1_fsm:send_event(Pid,
					{valid, StateData#state.server,
					 StateData#state.myname});
		  _ ->
		      p1_fsm:send_event(Pid,
					{invalid, StateData#state.server,
					 StateData#state.myname})
		end,
		if StateData#state.verify == false ->
		       {stop, normal, StateData};
		   true ->
		       NextState = wait_for_validation,
		       {next_state, NextState, StateData,
			get_timeout_interval(NextState)}
		end
	  end;
      _ ->
	  {next_state, wait_for_validation, StateData,
	   (?FSMTIMEOUT) * 3}
    end;
wait_for_validation({xmlstreamend, _Name}, StateData) ->
    ?INFO_MSG("wait for validation: ~s -> ~s (xmlstreamend)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
wait_for_validation({xmlstreamerror, _}, StateData) ->
    ?INFO_MSG("wait for validation: ~s -> ~s (xmlstreamerror)",
	      [StateData#state.myname, StateData#state.server]),
    send_text(StateData,
	      <<(?INVALID_XML_ERR)/binary,
		(?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
wait_for_validation(timeout,
		    #state{verify = {VPid, VKey, SID}} = StateData)
    when is_pid(VPid) and is_binary(VKey) and
	   is_binary(SID) ->
    ?DEBUG("wait_for_validation: ~s -> ~s (timeout "
	   "in verify connection)",
	   [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
wait_for_validation(timeout, StateData) ->
    ?INFO_MSG("wait_for_validation: ~s -> ~s (connect "
	      "timeout)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
wait_for_validation(closed, StateData) ->
    ?INFO_MSG("wait for validation: ~s -> ~s (closed)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData}.

wait_for_features({xmlstreamelement, El}, StateData) ->
    case El of
      #xmlel{name = <<"stream:features">>, children = Els} ->
	  {SASLEXT, StartTLS, StartTLSRequired} = lists:foldl(fun
								(#xmlel{name =
									    <<"mechanisms">>,
									attrs =
									    Attrs1,
									children
									    =
									    Els1} =
								     _El1,
								 {_SEXT, STLS,
								  STLSReq} =
								     Acc) ->
								    case
								      xml:get_attr_s(<<"xmlns">>,
										     Attrs1)
									of
								      ?NS_SASL ->
									  NewSEXT =
									      lists:any(fun
											  (#xmlel{name
												      =
												      <<"mechanism">>,
												  children
												      =
												      Els2}) ->
											      case
												xml:get_cdata(Els2)
												  of
												<<"EXTERNAL">> ->
												    true;
												_ ->
												    false
											      end;
											  (_) ->
											      false
											end,
											Els1),
									  {NewSEXT,
									   STLS,
									   STLSReq};
								      _ -> Acc
								    end;
								(#xmlel{name =
									    <<"starttls">>,
									attrs =
									    Attrs1} =
								     El1,
								 {SEXT, _STLS,
								  _STLSReq} =
								     Acc) ->
								    case
								      xml:get_attr_s(<<"xmlns">>,
										     Attrs1)
									of
								      ?NS_TLS ->
									  Req =
									      case
										xml:get_subtag(El1,
											       <<"required">>)
										  of
										#xmlel{} ->
										    true;
										false ->
										    false
									      end,
									  {SEXT,
									   true,
									   Req};
								      _ -> Acc
								    end;
								(_, Acc) -> Acc
							      end,
							      {false, false,
							       false},
							      Els),
	  if not SASLEXT and not StartTLS and
	       StateData#state.authenticated ->
		 send_queue(StateData, StateData#state.queue),
		 ?INFO_MSG("Connection established: ~s -> ~s with "
			   "SASL EXTERNAL and TLS=~p",
			   [StateData#state.myname, StateData#state.server,
			    StateData#state.tls_enabled]),
		 ejabberd_hooks:run(s2s_connect_hook,
				    [StateData#state.myname,
				     StateData#state.server]),
		 {next_state, stream_established,
		  StateData#state{queue = queue:new()}};
	     SASLEXT and StateData#state.try_auth and
	       (StateData#state.new /= false) and
		 (StateData#state.tls_enabled or
		   not StateData#state.tls_required) ->
		 send_element(StateData,
			      #xmlel{name = <<"auth">>,
				     attrs =
					 [{<<"xmlns">>, ?NS_SASL},
					  {<<"mechanism">>, <<"EXTERNAL">>}],
				     children =
					 [{xmlcdata,
					   jlib:encode_base64(StateData#state.myname)}]}),
		 {next_state, wait_for_auth_result,
		  StateData#state{try_auth = false}, ?FSMTIMEOUT};
	     StartTLS and StateData#state.tls and
	       not StateData#state.tls_enabled ->
		 send_element(StateData,
			      #xmlel{name = <<"starttls">>,
				     attrs = [{<<"xmlns">>, ?NS_TLS}],
				     children = []}),
		 {next_state, wait_for_starttls_proceed, StateData,
		  ?FSMTIMEOUT};
	     StartTLSRequired and not StateData#state.tls ->
		 ?DEBUG("restarted: ~p",
			[{StateData#state.myname, StateData#state.server}]),
		 ejabberd_socket:close(StateData#state.socket),
		 {next_state, reopen_socket,
		  StateData#state{socket = undefined, use_v10 = false},
		  ?FSMTIMEOUT};
	     StateData#state.db_enabled ->
		 send_db_request(StateData);
	     true ->
		 ?DEBUG("restarted: ~p",
			[{StateData#state.myname, StateData#state.server}]),
		 ejabberd_socket:close(StateData#state.socket),
		 {next_state, reopen_socket,
		  StateData#state{socket = undefined, use_v10 = false},
		  ?FSMTIMEOUT}
	  end;
      _ ->
	  send_text(StateData,
		    <<(xml:element_to_binary(?SERR_BAD_FORMAT))/binary,
		      (?STREAM_TRAILER)/binary>>),
	  ?INFO_MSG("Closing s2s connection: ~s -> ~s (bad "
		    "format)",
		    [StateData#state.myname, StateData#state.server]),
	  {stop, normal, StateData}
    end;
wait_for_features({xmlstreamend, _Name}, StateData) ->
    ?INFO_MSG("wait_for_features: xmlstreamend", []),
    {stop, normal, StateData};
wait_for_features({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      <<(?INVALID_XML_ERR)/binary,
		(?STREAM_TRAILER)/binary>>),
    ?INFO_MSG("wait for features: xmlstreamerror", []),
    {stop, normal, StateData};
wait_for_features(timeout, StateData) ->
    ?INFO_MSG("wait for features: timeout", []),
    {stop, normal, StateData};
wait_for_features(closed, StateData) ->
    ?INFO_MSG("wait for features: closed", []),
    {stop, normal, StateData}.

wait_for_auth_result({xmlstreamelement, El},
		     StateData) ->
    case El of
      #xmlel{name = <<"success">>, attrs = Attrs} ->
	  case xml:get_attr_s(<<"xmlns">>, Attrs) of
	    ?NS_SASL ->
		?DEBUG("auth: ~p",
		       [{StateData#state.myname, StateData#state.server}]),
		ejabberd_socket:reset_stream(StateData#state.socket),
		send_text(StateData,
			  io_lib:format(?STREAM_HEADER,
					[StateData#state.myname,
					 StateData#state.server,
					 <<" version='1.0'">>])),
		{next_state, wait_for_stream,
		 StateData#state{streamid = new_id(),
				 authenticated = true},
		 ?FSMTIMEOUT};
	    _ ->
		send_text(StateData,
			  <<(xml:element_to_binary(?SERR_BAD_FORMAT))/binary,
			    (?STREAM_TRAILER)/binary>>),
		?INFO_MSG("Closing s2s connection: ~s -> ~s (bad "
			  "format)",
			  [StateData#state.myname, StateData#state.server]),
		{stop, normal, StateData}
	  end;
      #xmlel{name = <<"failure">>, attrs = Attrs} ->
	  case xml:get_attr_s(<<"xmlns">>, Attrs) of
	    ?NS_SASL ->
		?DEBUG("restarted: ~p",
		       [{StateData#state.myname, StateData#state.server}]),
		ejabberd_socket:close(StateData#state.socket),
		{next_state, reopen_socket,
		 StateData#state{socket = undefined}, ?FSMTIMEOUT};
	    _ ->
		send_text(StateData,
			  <<(xml:element_to_binary(?SERR_BAD_FORMAT))/binary,
			    (?STREAM_TRAILER)/binary>>),
		?INFO_MSG("Closing s2s connection: ~s -> ~s (bad "
			  "format)",
			  [StateData#state.myname, StateData#state.server]),
		{stop, normal, StateData}
	  end;
      _ ->
	  send_text(StateData,
		    <<(xml:element_to_binary(?SERR_BAD_FORMAT))/binary,
		      (?STREAM_TRAILER)/binary>>),
	  ?INFO_MSG("Closing s2s connection: ~s -> ~s (bad "
		    "format)",
		    [StateData#state.myname, StateData#state.server]),
	  {stop, normal, StateData}
    end;
wait_for_auth_result({xmlstreamend, _Name},
		     StateData) ->
    ?INFO_MSG("wait for auth result: xmlstreamend", []),
    {stop, normal, StateData};
wait_for_auth_result({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      <<(?INVALID_XML_ERR)/binary,
		(?STREAM_TRAILER)/binary>>),
    ?INFO_MSG("wait for auth result: xmlstreamerror", []),
    {stop, normal, StateData};
wait_for_auth_result(timeout, StateData) ->
    ?INFO_MSG("wait for auth result: timeout", []),
    {stop, normal, StateData};
wait_for_auth_result(closed, StateData) ->
    ?INFO_MSG("wait for auth result: closed", []),
    {stop, normal, StateData}.

wait_for_starttls_proceed({xmlstreamelement, El},
			  StateData) ->
    case El of
      #xmlel{name = <<"proceed">>, attrs = Attrs} ->
	  case xml:get_attr_s(<<"xmlns">>, Attrs) of
	    ?NS_TLS ->
		?DEBUG("starttls: ~p",
		       [{StateData#state.myname, StateData#state.server}]),
		Socket = StateData#state.socket,
		TLSOpts = case
			    ejabberd_config:get_option(
                              {domain_certfile, StateData#state.myname},
                              fun iolist_to_binary/1)
			      of
			    undefined -> StateData#state.tls_options;
			    CertFile ->
				[{certfile, CertFile}
				 | lists:keydelete(certfile, 1,
						   StateData#state.tls_options)]
			  end,
		TLSSocket = ejabberd_socket:starttls(Socket, TLSOpts),
		NewStateData = StateData#state{socket = TLSSocket,
					       streamid = new_id(),
					       tls_enabled = true,
					       tls_options = TLSOpts},
		send_text(NewStateData,
			  io_lib:format(?STREAM_HEADER,
					[NewStateData#state.myname,
					 NewStateData#state.server,
					 <<" version='1.0'">>])),
		{next_state, wait_for_stream, NewStateData,
		 ?FSMTIMEOUT};
	    _ ->
		send_text(StateData,
			  <<(xml:element_to_binary(?SERR_BAD_FORMAT))/binary,
			    (?STREAM_TRAILER)/binary>>),
		?INFO_MSG("Closing s2s connection: ~s -> ~s (bad "
			  "format)",
			  [StateData#state.myname, StateData#state.server]),
		{stop, normal, StateData}
	  end;
      _ ->
	  ?INFO_MSG("Closing s2s connection: ~s -> ~s (bad "
		    "format)",
		    [StateData#state.myname, StateData#state.server]),
	  {stop, normal, StateData}
    end;
wait_for_starttls_proceed({xmlstreamend, _Name},
			  StateData) ->
    ?INFO_MSG("wait for starttls proceed: xmlstreamend",
	      []),
    {stop, normal, StateData};
wait_for_starttls_proceed({xmlstreamerror, _},
			  StateData) ->
    send_text(StateData,
	      <<(?INVALID_XML_ERR)/binary,
		(?STREAM_TRAILER)/binary>>),
    ?INFO_MSG("wait for starttls proceed: xmlstreamerror",
	      []),
    {stop, normal, StateData};
wait_for_starttls_proceed(timeout, StateData) ->
    ?INFO_MSG("wait for starttls proceed: timeout", []),
    {stop, normal, StateData};
wait_for_starttls_proceed(closed, StateData) ->
    ?INFO_MSG("wait for starttls proceed: closed", []),
    {stop, normal, StateData}.

reopen_socket({xmlstreamelement, _El}, StateData) ->
    {next_state, reopen_socket, StateData, ?FSMTIMEOUT};
reopen_socket({xmlstreamend, _Name}, StateData) ->
    {next_state, reopen_socket, StateData, ?FSMTIMEOUT};
reopen_socket({xmlstreamerror, _}, StateData) ->
    {next_state, reopen_socket, StateData, ?FSMTIMEOUT};
reopen_socket(timeout, StateData) ->
    ?INFO_MSG("reopen socket: timeout", []),
    {stop, normal, StateData};
reopen_socket(closed, StateData) ->
    p1_fsm:send_event(self(), init),
    {next_state, open_socket, StateData, ?FSMTIMEOUT}.

%% This state is use to avoid reconnecting to often to bad sockets
wait_before_retry(_Event, StateData) ->
    {next_state, wait_before_retry, StateData, ?FSMTIMEOUT}.

relay_to_bridge(stop, StateData) ->
    wait_before_reconnect(StateData);
relay_to_bridge(closed, StateData) ->
    ?INFO_MSG("relay to bridge: ~s -> ~s (closed)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
relay_to_bridge(_Event, StateData) ->
    {next_state, relay_to_bridge, StateData}.

stream_established({xmlstreamelement, El}, StateData) ->
    ?DEBUG("s2S stream established", []),
    case is_verify_res(El) of
      {verify, VTo, VFrom, VId, VType} ->
	  ?DEBUG("recv verify: ~p", [{VFrom, VTo, VId, VType}]),
	  case StateData#state.verify of
	    {VPid, _VKey, _SID} ->
		case VType of
		  <<"valid">> ->
		      p1_fsm:send_event(VPid,
					{valid, StateData#state.server,
					 StateData#state.myname});
		  _ ->
		      p1_fsm:send_event(VPid,
					{invalid, StateData#state.server,
					 StateData#state.myname})
		end;
	    _ -> ok
	  end;
      _ -> ok
    end,
    {next_state, stream_established, StateData};
stream_established({xmlstreamend, _Name}, StateData) ->
    ?INFO_MSG("Connection closed in stream established: "
	      "~s -> ~s (xmlstreamend)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
stream_established({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      <<(?INVALID_XML_ERR)/binary,
		(?STREAM_TRAILER)/binary>>),
    ?INFO_MSG("stream established: ~s -> ~s (xmlstreamerror)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
stream_established(timeout, StateData) ->
    ?INFO_MSG("stream established: ~s -> ~s (timeout)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
stream_established(closed, StateData) ->
    ?INFO_MSG("stream established: ~s -> ~s (closed)",
	      [StateData#state.myname, StateData#state.server]),
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
%%state_name(Event, From, StateData) ->
%%    Reply = ok,
%%    {reply, Reply, state_name, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData,
     get_timeout_interval(StateName)}.

handle_sync_event(get_state_infos, _From, StateName,
		  StateData) ->
    {Addr, Port} = try
		     ejabberd_socket:peername(StateData#state.socket)
		   of
		     {ok, {A, P}} -> {A, P};
		     {error, _} -> {unknown, unknown}
		   catch
		     _:_ -> {unknown, unknown}
		   end,
    Infos = [{direction, out}, {statename, StateName},
	     {addr, Addr}, {port, Port},
	     {streamid, StateData#state.streamid},
	     {use_v10, StateData#state.use_v10},
	     {tls, StateData#state.tls},
	     {tls_required, StateData#state.tls_required},
	     {tls_enabled, StateData#state.tls_enabled},
	     {tls_options, StateData#state.tls_options},
	     {authenticated, StateData#state.authenticated},
	     {db_enabled, StateData#state.db_enabled},
	     {try_auth, StateData#state.try_auth},
	     {myname, StateData#state.myname},
	     {server, StateData#state.server},
	     {delay_to_retry, StateData#state.delay_to_retry},
	     {verify, StateData#state.verify}],
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
    Reply = ok,
    {reply, Reply, StateName, StateData,
     get_timeout_interval(StateName)}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData, Text),
    cancel_timer(StateData#state.timer),
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
    {next_state, StateName, StateData#state{timer = Timer},
     get_timeout_interval(StateName)};
handle_info({send_element, El}, StateName, StateData) ->
    case StateName of
      stream_established ->
	  cancel_timer(StateData#state.timer),
	  Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
	  send_element(StateData, El),
	  {next_state, StateName, StateData#state{timer = Timer}};
      %% In this state we bounce all message: We are waiting before
      %% trying to reconnect
      wait_before_retry ->
	  bounce_element(El, ?ERR_REMOTE_SERVER_NOT_FOUND),
	  {next_state, StateName, StateData};
      relay_to_bridge ->
	  {Mod, Fun} = StateData#state.bridge,
	  ?DEBUG("relaying stanza via ~p:~p/1", [Mod, Fun]),
	  case catch Mod:Fun(El) of
	    {'EXIT', Reason} ->
		?ERROR_MSG("Error while relaying to bridge: ~p",
			   [Reason]),
		bounce_element(El, ?ERR_INTERNAL_SERVER_ERROR),
		wait_before_reconnect(StateData);
	    _ -> {next_state, StateName, StateData}
	  end;
      _ ->
	  Q = queue:in(El, StateData#state.queue),
	  {next_state, StateName, StateData#state{queue = Q},
	   get_timeout_interval(StateName)}
    end;
handle_info({timeout, Timer, _}, wait_before_retry,
	    #state{timer = Timer} = StateData) ->
    ?INFO_MSG("Reconnect delay expired: Will now retry "
	      "to connect to ~s when needed.",
	      [StateData#state.server]),
    {stop, normal, StateData};
handle_info({timeout, Timer, _}, _StateName,
	    #state{timer = Timer} = StateData) ->
    ?INFO_MSG("Closing connection with ~s: timeout",
	      [StateData#state.server]),
    {stop, normal, StateData};
handle_info(terminate_if_waiting_before_retry,
	    wait_before_retry, StateData) ->
    {stop, normal, StateData};
handle_info(terminate_if_waiting_before_retry,
	    StateName, StateData) ->
    {next_state, StateName, StateData,
     get_timeout_interval(StateName)};
handle_info(_, StateName, StateData) ->
    {next_state, StateName, StateData,
     get_timeout_interval(StateName)}.

terminate(Reason, StateName, StateData) ->
    ?DEBUG("terminated: ~p", [{Reason, StateName}]),
    case StateData#state.new of
      false -> ok;
      Key ->
	  ejabberd_s2s:remove_connection({StateData#state.myname,
					  StateData#state.server},
					 self(), Key)
    end,
    bounce_queue(StateData#state.queue,
		 ?ERR_REMOTE_SERVER_NOT_FOUND),
    bounce_messages(?ERR_REMOTE_SERVER_NOT_FOUND),
    case StateData#state.socket of
      undefined -> ok;
      _Socket -> ejabberd_socket:close(StateData#state.socket)
    end,
    ok.

print_state(State) -> State.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

send_text(StateData, Text) ->
    ejabberd_socket:send(StateData#state.socket, Text).

send_element(StateData, El) ->
    send_text(StateData, xml:element_to_binary(El)).

send_queue(StateData, Q) ->
    case queue:out(Q) of
      {{value, El}, Q1} ->
	  send_element(StateData, El), send_queue(StateData, Q1);
      {empty, _Q1} -> ok
    end.

%% Bounce a single message (xmlelement)
bounce_element(El, Error) ->
    #xmlel{attrs = Attrs} = El,
    case xml:get_attr_s(<<"type">>, Attrs) of
      <<"error">> -> ok;
      <<"result">> -> ok;
      _ ->
	  Err = jlib:make_error_reply(El, Error),
	  From = jid:from_string(xml:get_tag_attr_s(<<"from">>,
						       El)),
	  To = jid:from_string(xml:get_tag_attr_s(<<"to">>,
						     El)),
	  ejabberd_router:route(To, From, Err)
    end.

bounce_queue(Q, Error) ->
    case queue:out(Q) of
      {{value, El}, Q1} ->
	  bounce_element(El, Error), bounce_queue(Q1, Error);
      {empty, _} -> ok
    end.

new_id() -> randoms:get_string().

cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    receive {timeout, Timer, _} -> ok after 0 -> ok end.

bounce_messages(Error) ->
    receive
      {send_element, El} ->
	  bounce_element(El, Error), bounce_messages(Error)
      after 0 -> ok
    end.

send_db_request(StateData) ->
    Server = StateData#state.server,
    New = case StateData#state.new of
	    false ->
		case ejabberd_s2s:try_register({StateData#state.myname,
						Server})
		    of
		  {key, Key} -> Key;
		  false -> false
		end;
	    Key -> Key
	  end,
    NewStateData = StateData#state{new = New},
    try case New of
	  false -> ok;
	  Key1 ->
	      send_element(StateData,
			   #xmlel{name = <<"db:result">>,
				  attrs =
				      [{<<"from">>, StateData#state.myname},
				       {<<"to">>, Server}],
				  children = [{xmlcdata, Key1}]})
	end,
	case StateData#state.verify of
	  false -> ok;
	  {_Pid, Key2, SID} ->
	      send_element(StateData,
			   #xmlel{name = <<"db:verify">>,
				  attrs =
				      [{<<"from">>, StateData#state.myname},
				       {<<"to">>, StateData#state.server},
				       {<<"id">>, SID}],
				  children = [{xmlcdata, Key2}]})
	end,
	{next_state, wait_for_validation, NewStateData,
	 (?FSMTIMEOUT) * 6}
    catch
      _:_ -> {stop, normal, NewStateData}
    end.

is_verify_res(#xmlel{name = Name, attrs = Attrs})
    when Name == <<"db:result">> ->
    {result, xml:get_attr_s(<<"to">>, Attrs),
     xml:get_attr_s(<<"from">>, Attrs),
     xml:get_attr_s(<<"id">>, Attrs),
     xml:get_attr_s(<<"type">>, Attrs)};
is_verify_res(#xmlel{name = Name, attrs = Attrs})
    when Name == <<"db:verify">> ->
    {verify, xml:get_attr_s(<<"to">>, Attrs),
     xml:get_attr_s(<<"from">>, Attrs),
     xml:get_attr_s(<<"id">>, Attrs),
     xml:get_attr_s(<<"type">>, Attrs)};
is_verify_res(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SRV support

-include_lib("kernel/include/inet.hrl").

-spec get_addr_port(binary()) -> [{binary(), inet:port_number()}].
get_addr_port(Server) ->
    Res = srv_lookup(Server),
    case Res of
      {error, Reason} ->
	  ?DEBUG("srv lookup of '~s' failed: ~p~n",
		 [Server, Reason]),
	  [{Server, outgoing_s2s_port()}];
      {ok, HEnt} ->
	  ?DEBUG("srv lookup of '~s': ~p~n",
		 [Server, HEnt#hostent.h_addr_list]),
	  AddrList = HEnt#hostent.h_addr_list,
	  random:seed(p1_time_compat:timestamp()),
	  case catch lists:map(fun ({Priority, Weight, Port,
				     Host}) ->
				       N = case Weight of
					     0 -> 0;
					     _ ->
						 (Weight + 1) * random:uniform()
					   end,
				       {Priority * 65536 - N, Host, Port}
			       end,
			       AddrList)
	      of
	    SortedList = [_ | _] ->
		List = lists:map(fun ({_, Host, Port}) ->
                                         {list_to_binary(Host), Port}
				 end,
				 lists:keysort(1, SortedList)),
		?DEBUG("srv lookup of '~s': ~p~n", [Server, List]),
		List;
	    _ -> [{Server, outgoing_s2s_port()}]
	  end
    end.

srv_lookup(Server) ->
    TimeoutMs = timer:seconds(
                  ejabberd_config:get_option(
                    s2s_dns_timeout,
                    fun(I) when is_integer(I), I>=0 -> I end,
                    10)),
    Retries = ejabberd_config:get_option(
                s2s_dns_retries,
                fun(I) when is_integer(I), I>=0 -> I end,
                2),
    srv_lookup(binary_to_list(Server), TimeoutMs, Retries).

%% XXX - this behaviour is suboptimal in the case that the domain
%% has a "_xmpp-server._tcp." but not a "_jabber._tcp." record and
%% we don't get a DNS reply for the "_xmpp-server._tcp." lookup. In this
%% case we'll give up when we get the "_jabber._tcp." nxdomain reply.
srv_lookup(_Server, _Timeout, Retries)
    when Retries < 1 ->
    {error, timeout};
srv_lookup(Server, Timeout, Retries) ->
    case inet_res:getbyname("_xmpp-server._tcp." ++ Server,
			    srv, Timeout)
	of
      {error, _Reason} ->
	  case inet_res:getbyname("_jabber._tcp." ++ Server, srv,
				  Timeout)
	      of
	    {error, timeout} ->
		?ERROR_MSG("The DNS servers~n  ~p~ntimed out on "
			   "request for ~p IN SRV. You should check "
			   "your DNS configuration.",
			   [inet_db:res_option(nameserver), Server]),
		srv_lookup(Server, Timeout, Retries - 1);
	    R -> R
	  end;
      {ok, _HEnt} = R -> R
    end.

test_get_addr_port(Server) ->
    lists:foldl(fun (_, Acc) ->
			[HostPort | _] = get_addr_port(Server),
			case lists:keysearch(HostPort, 1, Acc) of
			  false -> [{HostPort, 1} | Acc];
			  {value, {_, Num}} ->
			      lists:keyreplace(HostPort, 1, Acc,
					       {HostPort, Num + 1})
			end
		end,
		[], lists:seq(1, 100000)).

get_addrs(Host, Family) ->
    Type = case Family of
	     inet4 -> inet;
	     ipv4 -> inet;
	     inet6 -> inet6;
	     ipv6 -> inet6
	   end,
    case inet:gethostbyname(binary_to_list(Host), Type) of
      {ok, #hostent{h_addr_list = Addrs}} ->
	  ?DEBUG("~s of ~s resolved to: ~p~n",
		 [Type, Host, Addrs]),
	  Addrs;
      {error, Reason} ->
	  ?DEBUG("~s lookup of '~s' failed: ~p~n",
		 [Type, Host, Reason]),
	  []
    end.

outgoing_s2s_port() ->
    ejabberd_config:get_option(
      outgoing_s2s_port,
      fun(I) when is_integer(I), I > 0, I =< 65536 -> I end,
      5269).

outgoing_s2s_families() ->
    ejabberd_config:get_option(
      outgoing_s2s_families,
      fun(Families) ->
              true = lists:all(
                       fun(ipv4) -> true;
                          (ipv6) -> true
                       end, Families),
              Families
      end, [ipv4, ipv6]).

outgoing_s2s_timeout() ->
    ejabberd_config:get_option(
      outgoing_s2s_timeout,
      fun(TimeOut) when is_integer(TimeOut), TimeOut > 0 ->
              TimeOut;
         (infinity) ->
              infinity
      end, 10000).

transform_options(Opts) ->
    lists:foldl(fun transform_options/2, [], Opts).

transform_options({outgoing_s2s_options, Families, Timeout}, Opts) ->
    ?WARNING_MSG("Option 'outgoing_s2s_options' is deprecated. "
                 "The option is still supported "
                 "but it is better to fix your config: "
                 "use 'outgoing_s2s_timeout' and "
                 "'outgoing_s2s_families' instead.", []),
    [{outgoing_s2s_families, Families},
     {outgoing_s2s_timeout, Timeout}
     | Opts];
transform_options({s2s_dns_options, S2SDNSOpts}, AllOpts) ->
    ?WARNING_MSG("Option 's2s_dns_options' is deprecated. "
                 "The option is still supported "
                 "but it is better to fix your config: "
                 "use 's2s_dns_timeout' and "
                 "'s2s_dns_retries' instead", []),
    lists:foldr(
      fun({timeout, T}, AccOpts) ->
              [{s2s_dns_timeout, T}|AccOpts];
         ({retries, R}, AccOpts) ->
              [{s2s_dns_retries, R}|AccOpts];
         (_, AccOpts) ->
              AccOpts
      end, AllOpts, S2SDNSOpts);
transform_options(Opt, Opts) ->
    [Opt|Opts].

%% Human readable S2S logging: Log only new outgoing connections as INFO
%% Do not log dialback
log_s2s_out(false, _, _, _) -> ok;
%% Log new outgoing connections:
log_s2s_out(_, Myname, Server, Tls) ->
    ?INFO_MSG("Trying to open s2s connection: ~s -> "
	      "~s with TLS=~p",
	      [Myname, Server, Tls]).

%% Calculate timeout depending on which state we are in:
%% Can return integer > 0 | infinity
get_timeout_interval(StateName) ->
    case StateName of
      %% Validation implies dialback: Networking can take longer:
      wait_for_validation -> (?FSMTIMEOUT) * 6;
      %% When stream is established, we only rely on S2S Timeout timer:
      stream_established -> infinity;
      _ -> ?FSMTIMEOUT
    end.

%% This function is intended to be called at the end of a state
%% function that want to wait for a reconnect delay before stopping.
wait_before_reconnect(StateData) ->
    bounce_queue(StateData#state.queue,
		 ?ERR_REMOTE_SERVER_NOT_FOUND),
    bounce_messages(?ERR_REMOTE_SERVER_NOT_FOUND),
    cancel_timer(StateData#state.timer),
    Delay = case StateData#state.delay_to_retry of
	      undefined_delay ->
		  {_, _, MicroSecs} = p1_time_compat:timestamp(), MicroSecs rem 14000 + 1000;
	      D1 -> lists:min([D1 * 2, get_max_retry_delay()])
	    end,
    Timer = erlang:start_timer(Delay, self(), []),
    {next_state, wait_before_retry,
     StateData#state{timer = Timer, delay_to_retry = Delay,
		     queue = queue:new()}}.

get_max_retry_delay() ->
    case ejabberd_config:get_option(
           s2s_max_retry_delay,
           fun(I) when is_integer(I), I > 0 -> I end) of
        undefined -> ?MAX_RETRY_DELAY;
        Seconds -> Seconds * 1000
    end.

%% Terminate s2s_out connections that are in state wait_before_retry
terminate_if_waiting_delay(From, To) ->
    FromTo = {From, To},
    Pids = ejabberd_s2s:get_connections_pids(FromTo),
    lists:foreach(fun (Pid) ->
			  Pid ! terminate_if_waiting_before_retry
		  end,
		  Pids).

fsm_limit_opts() ->
    case ejabberd_config:get_option(
           max_fsm_queue,
           fun(I) when is_integer(I), I > 0 -> I end) of
        undefined -> [];
        N -> [{max_queue, N}]
    end.

opt_type(domain_certfile) -> fun iolist_to_binary/1;
opt_type(max_fsm_queue) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(outgoing_s2s_families) ->
    fun (Families) ->
	    true = lists:all(fun (ipv4) -> true;
				 (ipv6) -> true
			     end,
			     Families),
	    Families
    end;
opt_type(outgoing_s2s_port) ->
    fun (I) when is_integer(I), I > 0, I =< 65536 -> I end;
opt_type(outgoing_s2s_timeout) ->
    fun (TimeOut) when is_integer(TimeOut), TimeOut > 0 ->
	    TimeOut;
	(infinity) -> infinity
    end;
opt_type(s2s_certfile) -> fun iolist_to_binary/1;
opt_type(s2s_ciphers) -> fun iolist_to_binary/1;
opt_type(s2s_dhfile) -> fun iolist_to_binary/1;
opt_type(s2s_dns_retries) ->
    fun (I) when is_integer(I), I >= 0 -> I end;
opt_type(s2s_dns_timeout) ->
    fun (I) when is_integer(I), I >= 0 -> I end;
opt_type(s2s_max_retry_delay) ->
    fun (I) when is_integer(I), I > 0 -> I end;
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
    fun (true) -> true;
	(false) -> false;
	(optional) -> optional;
	(required) -> required;
	(required_trusted) -> required_trusted
    end;
opt_type(_) ->
    [domain_certfile, max_fsm_queue, outgoing_s2s_families,
     outgoing_s2s_port, outgoing_s2s_timeout, s2s_certfile,
     s2s_ciphers, s2s_dhfile, s2s_dns_retries, s2s_dns_timeout,
     s2s_max_retry_delay, s2s_protocol_options,
     s2s_tls_compression, s2s_use_starttls].
