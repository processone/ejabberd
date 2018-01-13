%%%-------------------------------------------------------------------
%%% Created : 26 Nov 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
-module(xmpp_stream_in).
-define(GEN_SERVER, p1_server).
-behaviour(?GEN_SERVER).

-protocol({rfc, 6120}).
-protocol({xep, 114, '1.6'}).

%% API
-export([start/3, start_link/3, call/3, cast/2, reply/2, stop/1,
	 send/2, close/1, close/2, send_error/3, establish/1,
	 get_transport/1, change_shaper/2, set_timeout/2, format_error/1]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
	 terminate/2, code_change/3]).

%%-define(DBGFSM, true).
-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-include("xmpp.hrl").
-type state() :: map().
-type stop_reason() :: {stream, reset | {in | out, stream_error()}} |
		       {tls, inet:posix() | atom() | binary()} |
		       {socket, inet:posix() | atom()} |
		       internal_failure.
-export_type([state/0, stop_reason/0]).
-callback init(list()) -> {ok, state()} | {error, term()} | ignore.
-callback handle_cast(term(), state()) -> state().
-callback handle_call(term(), term(), state()) -> state().
-callback handle_info(term(), state()) -> state().
-callback terminate(term(), state()) -> any().
-callback code_change(term(), state(), term()) -> {ok, state()} | {error, term()}.
-callback handle_stream_start(stream_start(), state()) -> state().
-callback handle_stream_established(state()) -> state().
-callback handle_stream_end(stop_reason(), state()) -> state().
-callback handle_cdata(binary(), state()) -> state().
-callback handle_unauthenticated_packet(xmpp_element(), state()) -> state().
-callback handle_authenticated_packet(xmpp_element(), state()) -> state().
-callback handle_unbinded_packet(xmpp_element(), state()) -> state().
-callback handle_auth_success(binary(), binary(), module(), state()) -> state().
-callback handle_auth_failure(binary(), binary(), binary(), state()) -> state().
-callback handle_send(xmpp_element(), ok | {error, inet:posix()}, state()) -> state().
-callback handle_recv(fxml:xmlel(), xmpp_element() | {error, term()}, state()) -> state().
-callback handle_timeout(state()) -> state().
-callback get_password_fun(state()) -> fun().
-callback check_password_fun(state()) -> fun().
-callback check_password_digest_fun(state()) -> fun().
-callback bind(binary(), state()) -> {ok, state()} | {error, stanza_error(), state()}.
-callback compress_methods(state()) -> [binary()].
-callback tls_options(state()) -> [proplists:property()].
-callback tls_required(state()) -> boolean().
-callback tls_verify(state()) -> boolean().
-callback tls_enabled(state()) -> boolean().
-callback sasl_mechanisms([cyrsasl:mechanism()], state()) -> [cyrsasl:mechanism()].
-callback unauthenticated_stream_features(state()) -> [xmpp_element()].
-callback authenticated_stream_features(state()) -> [xmpp_element()].

%% All callbacks are optional
-optional_callbacks([init/1,
		     handle_cast/2,
		     handle_call/3,
		     handle_info/2,
		     terminate/2,
		     code_change/3,
		     handle_stream_start/2,
		     handle_stream_established/1,
		     handle_stream_end/2,
		     handle_cdata/2,
		     handle_authenticated_packet/2,
		     handle_unauthenticated_packet/2,
		     handle_unbinded_packet/2,
		     handle_auth_success/4,
		     handle_auth_failure/4,
		     handle_send/3,
		     handle_recv/3,
		     handle_timeout/1,
		     get_password_fun/1,
		     check_password_fun/1,
		     check_password_digest_fun/1,
		     bind/2,
		     compress_methods/1,
		     tls_options/1,
		     tls_required/1,
		     tls_verify/1,
		     tls_enabled/1,
		     sasl_mechanisms/2,
		     unauthenticated_stream_features/1,
		     authenticated_stream_features/1]).

%%%===================================================================
%%% API
%%%===================================================================
start(Mod, Args, Opts) ->
    ?GEN_SERVER:start(?MODULE, [Mod|Args], Opts ++ ?FSMOPTS).

start_link(Mod, Args, Opts) ->
    ?GEN_SERVER:start_link(?MODULE, [Mod|Args], Opts ++ ?FSMOPTS).

call(Ref, Msg, Timeout) ->
    ?GEN_SERVER:call(Ref, Msg, Timeout).

cast(Ref, Msg) ->
    ?GEN_SERVER:cast(Ref, Msg).

reply(Ref, Reply) ->
    ?GEN_SERVER:reply(Ref, Reply).

-spec stop(pid()) -> ok;
	  (state()) -> no_return().
stop(Pid) when is_pid(Pid) ->
    cast(Pid, stop);
stop(#{owner := Owner} = State) when Owner == self() ->
    terminate(normal, State),
    exit(normal);
stop(_) ->
    erlang:error(badarg).

-spec send(pid(), xmpp_element()) -> ok;
	  (state(), xmpp_element()) -> state().
send(Pid, Pkt) when is_pid(Pid) ->
    cast(Pid, {send, Pkt});
send(#{owner := Owner} = State, Pkt) when Owner == self() ->
    send_pkt(State, Pkt);
send(_, _) ->
    erlang:error(badarg).

-spec close(pid()) -> ok;
	   (state()) -> state().
close(Pid) when is_pid(Pid) ->
    close(Pid, closed);
close(#{owner := Owner} = State) when Owner == self() ->
    close_socket(State);
close(_) ->
    erlang:error(badarg).

-spec close(pid(), atom()) -> ok.
close(Pid, Reason) ->
    cast(Pid, {close, Reason}).

-spec establish(state()) -> state().
establish(State) ->
    process_stream_established(State).

-spec set_timeout(state(), non_neg_integer() | infinity) -> state().
set_timeout(#{owner := Owner} = State, Timeout) when Owner == self() ->
    case Timeout of
	infinity -> State#{stream_timeout => infinity};
	_ ->
	    Time = p1_time_compat:monotonic_time(milli_seconds),
	    State#{stream_timeout => {Timeout, Time}}
    end;
set_timeout(_, _) ->
    erlang:error(badarg).

get_transport(#{socket := Socket, owner := Owner})
  when Owner == self() ->
    xmpp_socket:get_transport(Socket);
get_transport(_) ->
    erlang:error(badarg).

-spec change_shaper(state(), shaper:shaper()) -> state().
change_shaper(#{socket := Socket, owner := Owner} = State, Shaper)
  when Owner == self() ->
    Socket1 = xmpp_socket:change_shaper(Socket, Shaper),
    State#{socket => Socket1};
change_shaper(_, _) ->
    erlang:error(badarg).

-spec format_error(stop_reason()) ->  binary().
format_error({socket, Reason}) ->
    format("Connection failed: ~s", [format_inet_error(Reason)]);
format_error({stream, reset}) ->
    <<"Stream reset by peer">>;
format_error({stream, {in, #stream_error{reason = Reason, text = Txt}}}) ->
    format("Stream closed by peer: ~s", [format_stream_error(Reason, Txt)]);
format_error({stream, {out, #stream_error{reason = Reason, text = Txt}}}) ->
    format("Stream closed by us: ~s", [format_stream_error(Reason, Txt)]);
format_error({tls, Reason}) ->
    format("TLS failed: ~s", [format_tls_error(Reason)]);
format_error(internal_failure) ->
    <<"Internal server error">>;
format_error(Err) ->
    format("Unrecognized error: ~w", [Err]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Module, {_SockMod, Socket}, Opts]) ->
    Encrypted = proplists:get_bool(tls, Opts),
    SocketMonitor = xmpp_socket:monitor(Socket),
    case xmpp_socket:peername(Socket) of
	{ok, IP} ->
	    Time = p1_time_compat:monotonic_time(milli_seconds),
	    State = #{owner => self(),
		      mod => Module,
		      socket => Socket,
		      socket_monitor => SocketMonitor,
		      stream_timeout => {timer:seconds(30), Time},
		      stream_direction => in,
		      stream_id => new_id(),
		      stream_state => wait_for_stream,
		      stream_header_sent => false,
		      stream_restarted => false,
		      stream_compressed => false,
		      stream_encrypted => Encrypted,
		      stream_version => {1,0},
		      stream_authenticated => false,
		      xmlns => ?NS_CLIENT,
		      lang => <<"">>,
		      user => <<"">>,
		      server => <<"">>,
		      resource => <<"">>,
		      lserver => <<"">>,
		      ip => IP},
	    case try Module:init([State, Opts])
		 catch _:undef -> {ok, State}
		 end of
		{ok, State1} when not Encrypted ->
		    {_, State2, Timeout} = noreply(State1),
		    {ok, State2, Timeout};
		{ok, State1} when Encrypted ->
		    TLSOpts = try Module:tls_options(State1)
			      catch _:undef -> []
			      end,
		    case xmpp_socket:starttls(Socket, TLSOpts) of
			{ok, TLSSocket} ->
			    State2 = State1#{socket => TLSSocket},
			    {_, State3, Timeout} = noreply(State2),
			    {ok, State3, Timeout};
			{error, Reason} ->
			    {stop, Reason}
		    end;
		{error, Reason} ->
		    {stop, Reason};
		ignore ->
		    ignore
	    end;
	{error, _Reason} ->
	    ignore
    end.

handle_cast({send, Pkt}, State) ->
    noreply(send_pkt(State, Pkt));
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({close, Reason}, State) ->
    State1 = close_socket(State),
    noreply(
      case is_disconnected(State) of
	  true -> State1;
	  false -> process_stream_end({socket, Reason}, State)
      end);
handle_cast(Cast, #{mod := Mod} = State) ->
    noreply(try Mod:handle_cast(Cast, State)
	      catch _:undef -> State
	      end).

handle_call(Call, From, #{mod := Mod} = State) ->
    noreply(try Mod:handle_call(Call, From, State)
	    catch _:undef -> State
	    end).

handle_info({'$gen_event', {xmlstreamstart, Name, Attrs}},
	    #{stream_state := wait_for_stream,
	      xmlns := XMLNS, lang := MyLang} = State) ->
    El = #xmlel{name = Name, attrs = Attrs},
    noreply(
      try xmpp:decode(El, XMLNS, []) of
	  #stream_start{} = Pkt ->
	      State1 = send_header(State, Pkt),
	      case is_disconnected(State1) of
		  true -> State1;
		  false -> process_stream(Pkt, State1)
	      end;
	  _ ->
	      State1 = send_header(State),
	      case is_disconnected(State1) of
		  true -> State1;
		  false -> send_pkt(State1, xmpp:serr_invalid_xml())
	      end
      catch _:{xmpp_codec, Why} ->
	      State1 = send_header(State),
	      case is_disconnected(State1) of
		  true -> State1;
		  false ->
		      Txt = xmpp:io_format_error(Why),
		      Lang = select_lang(MyLang, xmpp:get_lang(El)),
		      Err = xmpp:serr_invalid_xml(Txt, Lang),
		      send_pkt(State1, Err)
	      end
      end);
handle_info({'$gen_event', {xmlstreamend, _}}, State) ->
    noreply(process_stream_end({stream, reset}, State));
handle_info({'$gen_event', closed}, State) ->
    noreply(process_stream_end({socket, closed}, State));
handle_info({'$gen_event', {xmlstreamerror, Reason}}, #{lang := Lang}= State) ->
    State1 = send_header(State),
    noreply(
      case is_disconnected(State1) of
	  true -> State1;
	  false ->
	      Err = case Reason of
			<<"XML stanza is too big">> ->
			    xmpp:serr_policy_violation(Reason, Lang);
			{_, Txt} ->
			    xmpp:serr_not_well_formed(Txt, Lang)
		    end,
	      send_pkt(State1, Err)
      end);
handle_info({'$gen_event', El}, #{stream_state := wait_for_stream} = State) ->
    error_logger:error_msg("unexpected event from XML driver: ~p; "
			   "xmlstreamstart was expected", [El]),
    State1 = send_header(State),
    noreply(
      case is_disconnected(State1) of
	  true -> State1;
	  false -> send_pkt(State1, xmpp:serr_invalid_xml())
      end);
handle_info({'$gen_event', {xmlstreamelement, El}},
	    #{xmlns := NS, mod := Mod} = State) ->
    noreply(
      try xmpp:decode(El, NS, [ignore_els]) of
	  Pkt ->
	      State1 = try Mod:handle_recv(El, Pkt, State)
		       catch _:undef -> State
		       end,
	      case is_disconnected(State1) of
		  true -> State1;
		  false -> process_element(Pkt, State1)
	      end
      catch _:{xmpp_codec, Why} ->
	      State1 = try Mod:handle_recv(El, {error, Why}, State)
		       catch _:undef -> State
		       end,
	      case is_disconnected(State1) of
		  true -> State1;
		  false -> process_invalid_xml(State1, El, Why)
	      end
      end);
handle_info({'$gen_all_state_event', {xmlstreamcdata, Data}},
	    #{mod := Mod} = State) ->
    noreply(try Mod:handle_cdata(Data, State)
	    catch _:undef -> State
	    end);
handle_info(timeout, #{mod := Mod} = State) ->
    Disconnected = is_disconnected(State),
    noreply(try Mod:handle_timeout(State)
	    catch _:undef when not Disconnected ->
		    send_pkt(State, xmpp:serr_connection_timeout());
		  _:undef ->
		    stop(State)
	    end);
handle_info({'DOWN', MRef, _Type, _Object, _Info},
	    #{socket_monitor := MRef} = State) ->
    noreply(process_stream_end({socket, closed}, State));
handle_info({tcp, _, Data}, #{socket := Socket} = State) ->
    noreply(
      case xmpp_socket:recv(Socket, Data) of
	  {ok, NewSocket} ->
	      State#{socket => NewSocket};
	  {error, Reason} when is_atom(Reason) ->
	      process_stream_end({socket, Reason}, State);
	  {error, Reason} ->
	      %% TODO: make fast_tls return atoms
	      process_stream_end({tls, Reason}, State)
      end);
handle_info({tcp_closed, _}, State) ->
    handle_info({'$gen_event', closed}, State);
handle_info({tcp_error, _, Reason}, State) ->
    noreply(process_stream_end({socket, Reason}, State));
handle_info(Info, #{mod := Mod} = State) ->
    noreply(try Mod:handle_info(Info, State)
	    catch _:undef -> State
	    end).

terminate(Reason, #{mod := Mod} = State) ->
    case get(already_terminated) of
	true ->
	    State;
	_ ->
	    put(already_terminated, true),
	    try Mod:terminate(Reason, State)
	    catch _:undef -> ok
	    end,
	    send_trailer(State)
    end.

code_change(OldVsn, #{mod := Mod} = State, Extra) ->
    Mod:code_change(OldVsn, State, Extra).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec noreply(state()) -> {noreply, state(), non_neg_integer() | infinity}.
noreply(#{stream_timeout := infinity} = State) ->
    {noreply, State, infinity};
noreply(#{stream_timeout := {MSecs, StartTime}} = State) ->
    CurrentTime = p1_time_compat:monotonic_time(milli_seconds),
    Timeout = max(0, MSecs - CurrentTime + StartTime),
    {noreply, State, Timeout}.

-spec new_id() -> binary().
new_id() ->
    randoms:get_string().

-spec is_disconnected(state()) -> boolean().
is_disconnected(#{stream_state := StreamState}) ->
    StreamState == disconnected.

-spec process_invalid_xml(state(), fxml:xmlel(), term()) -> state().
process_invalid_xml(#{lang := MyLang} = State, El, Reason) ->
    case xmpp:is_stanza(El) of
	true ->
	    Txt = xmpp:io_format_error(Reason),
	    Lang = select_lang(MyLang, xmpp:get_lang(El)),
	    send_error(State, El, xmpp:err_bad_request(Txt, Lang));
	false ->
	    case {xmpp:get_name(El), xmpp:get_ns(El)} of
		{Tag, ?NS_SASL} when Tag == <<"auth">>;
				     Tag == <<"response">>;
				     Tag == <<"abort">> ->
		    Txt = xmpp:io_format_error(Reason),
		    Err = #sasl_failure{reason = 'malformed-request',
					text = xmpp:mk_text(Txt, MyLang)},
		    send_pkt(State, Err);
		{<<"starttls">>, ?NS_TLS} ->
		    send_pkt(State, #starttls_failure{});
		{<<"compress">>, ?NS_COMPRESS} ->
		    Err = #compress_failure{reason = 'setup-failed'},
		    send_pkt(State, Err);
		_ ->
		    %% Maybe add something more?
		    State
	    end
    end.

-spec process_stream_end(stop_reason(), state()) -> state().
process_stream_end(_, #{stream_state := disconnected} = State) ->
    State;
process_stream_end(Reason, #{mod := Mod} = State) ->
    State1 = State#{stream_timeout => infinity,
		    stream_state => disconnected},
    try Mod:handle_stream_end(Reason, State1)
    catch _:undef -> stop(State1)
    end.

-spec process_stream(stream_start(), state()) -> state().
process_stream(#stream_start{xmlns = XML_NS,
			     stream_xmlns = STREAM_NS},
	       #{xmlns := NS} = State)
  when XML_NS /= NS; STREAM_NS /= ?NS_STREAM ->
    send_pkt(State, xmpp:serr_invalid_namespace());
process_stream(#stream_start{version = {N, _}}, State) when N > 1 ->
    send_pkt(State, xmpp:serr_unsupported_version());
process_stream(#stream_start{lang = Lang},
	       #{xmlns := ?NS_CLIENT, lang := DefaultLang} = State)
  when size(Lang) > 35 ->
    %% As stated in BCP47, 4.4.1:
    %% Protocols or specifications that specify limited buffer sizes for
    %% language tags MUST allow for language tags of at least 35 characters.
    %% Do not store long language tag to avoid possible DoS/flood attacks
    Txt = <<"Too long value of 'xml:lang' attribute">>,
    send_pkt(State, xmpp:serr_policy_violation(Txt, DefaultLang));
process_stream(#stream_start{to = undefined, version = Version} = StreamStart,
	       #{lang := Lang, server := Server, xmlns := NS} = State) ->
    if Version < {1,0} andalso NS /= ?NS_COMPONENT ->
	    %% Work-around for gmail servers
	    To = jid:make(Server),
	    process_stream(StreamStart#stream_start{to = To}, State);
       true ->
	    Txt = <<"Missing 'to' attribute">>,
	    send_pkt(State, xmpp:serr_improper_addressing(Txt, Lang))
    end;
process_stream(#stream_start{to = #jid{luser = U, lresource = R}},
	       #{lang := Lang} = State) when U /= <<"">>; R /= <<"">> ->
    Txt = <<"Improper 'to' attribute">>,
    send_pkt(State, xmpp:serr_improper_addressing(Txt, Lang));
process_stream(#stream_start{to = #jid{lserver = RemoteServer}} = StreamStart,
	       #{xmlns := ?NS_COMPONENT, mod := Mod} = State) ->
    State1 = State#{remote_server => RemoteServer,
		    stream_state => wait_for_handshake},
    try Mod:handle_stream_start(StreamStart, State1)
    catch _:undef -> State1
    end;
process_stream(#stream_start{to = #jid{server = Server, lserver = LServer},
			     from = From} = StreamStart,
	       #{stream_authenticated := Authenticated,
		 stream_restarted := StreamWasRestarted,
		 mod := Mod, xmlns := NS, resource := Resource,
		 stream_encrypted := Encrypted} = State) ->
    State1 = if not StreamWasRestarted ->
		     State#{server => Server, lserver => LServer};
		true ->
		     State
	     end,
    State2 = case From of
		 #jid{lserver = RemoteServer} when NS == ?NS_SERVER ->
		     State1#{remote_server => RemoteServer};
		 _ ->
		     State1
	     end,
    State3 = try Mod:handle_stream_start(StreamStart, State2)
	     catch _:undef -> State2
	     end,
    case is_disconnected(State3) of
	true -> State3;
	false ->
	    State4 = send_features(State3),
	    case is_disconnected(State4) of
		true -> State4;
		false ->
		    TLSRequired = is_starttls_required(State4),
		    if not Authenticated and (TLSRequired and not Encrypted) ->
			    State4#{stream_state => wait_for_starttls};
		       not Authenticated ->
			    State4#{stream_state => wait_for_sasl_request};
		       (NS == ?NS_CLIENT) and (Resource == <<"">>) ->
			    State4#{stream_state => wait_for_bind};
		       true ->
			    process_stream_established(State4)
		    end
	    end
    end.

-spec process_element(xmpp_element(), state()) -> state().
process_element(Pkt, #{stream_state := StateName, lang := Lang} = State) ->
    case Pkt of
	#starttls{} when StateName == wait_for_starttls;
			 StateName == wait_for_sasl_request ->
	    process_starttls(State);
	#starttls{} ->
	    process_starttls_failure(unexpected_starttls_request, State);
	#sasl_auth{} when StateName == wait_for_starttls ->
	    send_pkt(State, #sasl_failure{reason = 'encryption-required'});
	#sasl_auth{} when StateName == wait_for_sasl_request ->
	    process_sasl_request(Pkt, State);
	#sasl_auth{} when StateName == wait_for_sasl_response ->
	    process_sasl_request(Pkt, maps:remove(sasl_state, State));
	#sasl_auth{} ->
	    Txt = <<"SASL negotiation is not allowed in this state">>,
	    send_pkt(State, #sasl_failure{reason = 'not-authorized',
					      text = xmpp:mk_text(Txt, Lang)});
	#sasl_response{} when StateName == wait_for_starttls ->
	    send_pkt(State, #sasl_failure{reason = 'encryption-required'});
	#sasl_response{} when StateName == wait_for_sasl_response ->
	    process_sasl_response(Pkt, State);
	#sasl_response{} ->
	    Txt = <<"SASL negotiation is not allowed in this state">>,
	    send_pkt(State, #sasl_failure{reason = 'not-authorized',
					      text = xmpp:mk_text(Txt, Lang)});
	#sasl_abort{} when StateName == wait_for_sasl_response ->
	    process_sasl_abort(State);
	#sasl_abort{} ->
	    send_pkt(State, #sasl_failure{reason = 'aborted'});
	#sasl_success{} ->
	    State;
	#compress{} ->
	    process_compress(Pkt, State);
	#handshake{} when StateName == wait_for_handshake ->
	    process_handshake(Pkt, State);
	#handshake{} ->
	    State;
	#stream_error{} ->
	    process_stream_end({stream, {in, Pkt}}, State);
	_ when StateName == wait_for_sasl_request;
	       StateName == wait_for_handshake;
	       StateName == wait_for_sasl_response ->
	    process_unauthenticated_packet(Pkt, State);
	_ when StateName == wait_for_starttls ->
	    Txt = <<"Use of STARTTLS required">>,
	    Err = xmpp:serr_policy_violation(Txt, Lang),
	    send_pkt(State, Err);
	_ when StateName == wait_for_bind ->
	    process_bind(Pkt, State);
	_ when StateName == established ->
	    process_authenticated_packet(Pkt, State)
    end.

-spec process_unauthenticated_packet(xmpp_element(), state()) -> state().
process_unauthenticated_packet(Pkt, #{mod := Mod} = State) ->
    NewPkt = set_lang(Pkt, State),
    try Mod:handle_unauthenticated_packet(NewPkt, State)
    catch _:undef ->
	    Err = xmpp:serr_not_authorized(),
	    send(State, Err)
    end.

-spec process_authenticated_packet(xmpp_element(), state()) -> state().
process_authenticated_packet(Pkt, #{mod := Mod} = State) ->
    Pkt1 = set_lang(Pkt, State),
    case set_from_to(Pkt1, State) of
	{ok, Pkt2} ->
	    try Mod:handle_authenticated_packet(Pkt2, State)
	    catch _:undef ->
		    Err = xmpp:err_service_unavailable(),
		    send_error(State, Pkt, Err)
	    end;
	{error, Err} ->
	    send_pkt(State, Err)
    end.

-spec process_bind(xmpp_element(), state()) -> state().
process_bind(#iq{type = set, sub_els = [_]} = Pkt,
	     #{xmlns := ?NS_CLIENT, mod := Mod, lang := MyLang} = State) ->
    try xmpp:try_subtag(Pkt, #bind{}) of
	#bind{resource = R} ->
	    case Mod:bind(R, State) of
		{ok, #{user := U, server := S, resource := NewR} = State1}
		  when NewR /= <<"">> ->
		    Reply = #bind{jid = jid:make(U, S, NewR)},
		    State2 = send_pkt(State1, xmpp:make_iq_result(Pkt, Reply)),
		    process_stream_established(State2);
		{error, #stanza_error{} = Err, State1} ->
		    send_error(State1, Pkt, Err)
	    end;
	_ ->
	    try Mod:handle_unbinded_packet(Pkt, State)
	    catch _:undef ->
		    Err = xmpp:err_not_authorized(),
		    send_error(State, Pkt, Err)
	    end
    catch _:{xmpp_codec, Why} ->
	    Txt = xmpp:io_format_error(Why),
	    Lang = select_lang(MyLang, xmpp:get_lang(Pkt)),
	    Err = xmpp:err_bad_request(Txt, Lang),
	    send_error(State, Pkt, Err)
    end;
process_bind(Pkt, #{mod := Mod} = State) ->
    try Mod:handle_unbinded_packet(Pkt, State)
    catch _:undef ->
	    Err = xmpp:err_not_authorized(),
	    send_error(State, Pkt, Err)
    end.

-spec process_handshake(handshake(), state()) -> state().
process_handshake(#handshake{data = Digest},
		  #{mod := Mod, stream_id := StreamID,
		    remote_server := RemoteServer} = State) ->
    GetPW = try Mod:get_password_fun(State)
	    catch _:undef -> fun(_) -> {false, undefined} end
	    end,
    AuthRes = case GetPW(<<"">>) of
		  {false, _} ->
		      false;
		  {Password, _} ->
		      str:sha(<<StreamID/binary, Password/binary>>) == Digest
	      end,
    case AuthRes of
	true ->
	    State1 = try Mod:handle_auth_success(
			   RemoteServer, <<"handshake">>, undefined, State)
		     catch _:undef -> State
		     end,
	    case is_disconnected(State1) of
		true -> State1;
		false ->
		    State2 = send_pkt(State1, #handshake{}),
		    process_stream_established(State2)
	    end;
	false ->
	    State1 = try Mod:handle_auth_failure(
			   RemoteServer, <<"handshake">>, <<"not authorized">>, State)
		     catch _:undef -> State
		     end,
	    case is_disconnected(State1) of
		true -> State1;
		false -> send_pkt(State1, xmpp:serr_not_authorized())
	    end
    end.

-spec process_stream_established(state()) -> state().
process_stream_established(#{stream_state := StateName} = State)
  when StateName == disconnected; StateName == established ->
    State;
process_stream_established(#{mod := Mod} = State) ->
    State1 = State#{stream_authenticated => true,
		    stream_state => established,
		    stream_timeout => infinity},
    try Mod:handle_stream_established(State1)
    catch _:undef -> State1
    end.

-spec process_compress(compress(), state()) -> state().
process_compress(#compress{},
		 #{stream_compressed := Compressed,
		   stream_authenticated := Authenticated} = State)
  when Compressed or not Authenticated ->
    send_pkt(State, #compress_failure{reason = 'setup-failed'});
process_compress(#compress{methods = HisMethods},
		 #{socket := Socket, mod := Mod} = State) ->
    MyMethods = try Mod:compress_methods(State)
		catch _:undef -> []
		end,
    CommonMethods = lists_intersection(MyMethods, HisMethods),
    case lists:member(<<"zlib">>, CommonMethods) of
	true ->
	    case xmpp_socket:compress(Socket) of
		{ok, ZlibSocket} ->
		    State1 = send_pkt(State, #compressed{}),
		    case is_disconnected(State1) of
			true -> State1;
			false ->
			    State1#{socket => ZlibSocket,
				    stream_id => new_id(),
				    stream_header_sent => false,
				    stream_restarted => true,
				    stream_state => wait_for_stream,
				    stream_compressed => true}
		    end;
		{error, _} ->
		    Err = #compress_failure{reason = 'setup-failed'},
		    send_pkt(State, Err)
	    end;
	false ->
	    send_pkt(State, #compress_failure{reason = 'unsupported-method'})
    end.

-spec process_starttls(state()) -> state().
process_starttls(#{stream_encrypted := true} = State) ->
    process_starttls_failure(already_encrypted, State);
process_starttls(#{socket := Socket,
		   mod := Mod} = State) ->
    case is_starttls_available(State) of
	true ->
	    TLSOpts = try Mod:tls_options(State)
		      catch _:undef -> []
		      end,
	    case xmpp_socket:starttls(Socket, TLSOpts) of
		{ok, TLSSocket} ->
		    State1 = send_pkt(State, #starttls_proceed{}),
		    case is_disconnected(State1) of
			true -> State1;
			false ->
			    State1#{socket => TLSSocket,
				    stream_id => new_id(),
				    stream_header_sent => false,
				    stream_restarted => true,
				    stream_state => wait_for_stream,
				    stream_encrypted => true}
		    end;
		{error, Reason} ->
		    process_starttls_failure(Reason, State)
	    end;
	false ->
	    process_starttls_failure(starttls_unsupported, State)
    end.

-spec process_starttls_failure(term(), state()) -> state().
process_starttls_failure(Why, State) ->
    State1 = send_pkt(State, #starttls_failure{}),
    case is_disconnected(State1) of
	true -> State1;
	false -> process_stream_end({tls, Why}, State1)
    end.

-spec process_sasl_request(sasl_auth(), state()) -> state().
process_sasl_request(#sasl_auth{mechanism = Mech, text = ClientIn},
		     #{mod := Mod, lserver := LServer} = State) ->
    State1 = State#{sasl_mech => Mech},
    Mechs = get_sasl_mechanisms(State1),
    case lists:member(Mech, Mechs) of
	true when Mech == <<"EXTERNAL">> ->
	    Res = case xmpp_stream_pkix:authenticate(State1, ClientIn) of
		      {ok, Peer} ->
			  {ok, [{auth_module, pkix}, {username, Peer}]};
		      {error, Reason, Peer} ->
			  {error, Reason, Peer}
		  end,
	    process_sasl_result(Res, State1);
	true ->
	    GetPW = try Mod:get_password_fun(State1)
		    catch _:undef -> fun(_) -> false end
		    end,
	    CheckPW = try Mod:check_password_fun(State1)
		      catch _:undef -> fun(_, _, _) -> false end
		      end,
	    CheckPWDigest = try Mod:check_password_digest_fun(State1)
			    catch _:undef -> fun(_, _, _, _, _) -> false end
			    end,
	    SASLState = cyrsasl:server_new(<<"jabber">>, LServer, <<"">>, [],
					   GetPW, CheckPW, CheckPWDigest),
	    Res = cyrsasl:server_start(SASLState, Mech, ClientIn),
	    process_sasl_result(Res, State1#{sasl_state => SASLState});
	false ->
	    process_sasl_result({error, unsupported_mechanism, <<"">>}, State1)
    end.

-spec process_sasl_response(sasl_response(), state()) -> state().
process_sasl_response(#sasl_response{text = ClientIn},
		      #{sasl_state := SASLState} = State) ->
    SASLResult = cyrsasl:server_step(SASLState, ClientIn),
    process_sasl_result(SASLResult, State).

-spec process_sasl_result(cyrsasl:sasl_return(), state()) -> state().
process_sasl_result({ok, Props}, State) ->
    process_sasl_success(Props, <<"">>, State);
process_sasl_result({ok, Props, ServerOut}, State) ->
    process_sasl_success(Props, ServerOut, State);
process_sasl_result({continue, ServerOut, NewSASLState}, State) ->
    process_sasl_continue(ServerOut, NewSASLState, State);
process_sasl_result({error, Reason, User}, State) ->
    process_sasl_failure(Reason, User, State).

-spec process_sasl_success([cyrsasl:sasl_property()], binary(), state()) -> state().
process_sasl_success(Props, ServerOut,
		     #{socket := Socket,
		       mod := Mod, sasl_mech := Mech} = State) ->
    User = identity(Props),
    AuthModule = proplists:get_value(auth_module, Props),
    Socket1 = xmpp_socket:reset_stream(Socket),
    State0 = State#{socket => Socket1},
    State1 = send_pkt(State0, #sasl_success{text = ServerOut}),
    case is_disconnected(State1) of
	true -> State1;
	false ->
	    State2 = try Mod:handle_auth_success(User, Mech, AuthModule, State1)
		     catch _:undef -> State1
		     end,
	    case is_disconnected(State2) of
		true -> State2;
		false ->
		    State3 = maps:remove(sasl_state,
					 maps:remove(sasl_mech, State2)),
		    State3#{stream_id => new_id(),
			    stream_authenticated => true,
			    stream_header_sent => false,
			    stream_restarted => true,
			    stream_state => wait_for_stream,
			    user => User}
	    end
    end.

-spec process_sasl_continue(binary(), cyrsasl:sasl_state(), state()) -> state().
process_sasl_continue(ServerOut, NewSASLState, State) ->
    State1 = State#{sasl_state => NewSASLState,
		    stream_state => wait_for_sasl_response},
    send_pkt(State1, #sasl_challenge{text = ServerOut}).

-spec process_sasl_failure(atom(), binary(), state()) -> state().
process_sasl_failure(Err, User,
		     #{mod := Mod, sasl_mech := Mech, lang := Lang} = State) ->
    {Reason, Text} = format_sasl_error(Mech, Err),
    State1 = send_pkt(State, #sasl_failure{reason = Reason,
					   text = xmpp:mk_text(Text, Lang)}),
    case is_disconnected(State1) of
	true -> State1;
	false ->
	    State2 = try Mod:handle_auth_failure(User, Mech, Text, State1)
		     catch _:undef -> State1
		     end,
	    State3 = maps:remove(sasl_state, maps:remove(sasl_mech, State2)),
	    State3#{stream_state => wait_for_sasl_request}
    end.

-spec process_sasl_abort(state()) -> state().
process_sasl_abort(State) ->
    process_sasl_failure(aborted, <<"">>, State).

-spec send_features(state()) -> state().
send_features(#{stream_version := {1,0},
		stream_encrypted := Encrypted} = State) ->
    TLSRequired = is_starttls_required(State),
    Features = if TLSRequired and not Encrypted ->
		       get_tls_feature(State);
		  true ->
		       get_sasl_feature(State) ++ get_compress_feature(State)
			   ++ get_tls_feature(State) ++ get_bind_feature(State)
			   ++ get_session_feature(State) ++ get_other_features(State)
	       end,
    send_pkt(State, #stream_features{sub_els = Features});
send_features(State) ->
    %% clients and servers from stone age
    State.

-spec get_sasl_mechanisms(state()) -> [cyrsasl:mechanism()].
get_sasl_mechanisms(#{stream_encrypted := Encrypted, mod := Mod,
		      xmlns := NS, lserver := LServer} = State) ->
    Mechs = if NS == ?NS_CLIENT -> cyrsasl:listmech(LServer);
	       true -> []
	    end,
    TLSVerify = try Mod:tls_verify(State)
		catch _:undef -> false
		end,
    Mechs1 = if Encrypted andalso (TLSVerify orelse NS == ?NS_SERVER) ->
		     [<<"EXTERNAL">>|Mechs];
		true ->
		     Mechs
	     end,
    try Mod:sasl_mechanisms(Mechs1, State)
    catch _:undef -> Mechs1
    end.

-spec get_sasl_feature(state()) -> [sasl_mechanisms()].
get_sasl_feature(#{stream_authenticated := false,
		   stream_encrypted := Encrypted} = State) ->
    TLSRequired = is_starttls_required(State),
    if Encrypted or not TLSRequired ->
	    Mechs = get_sasl_mechanisms(State),
	    [#sasl_mechanisms{list = Mechs}];
       true ->
	    []
    end;
get_sasl_feature(_) ->
    [].

-spec get_compress_feature(state()) -> [compression()].
get_compress_feature(#{stream_compressed := false, mod := Mod,
		       stream_authenticated := true} = State) ->
    try Mod:compress_methods(State) of
	[] -> [];
	Ms -> [#compression{methods = Ms}]
    catch _:undef ->
	    []
    end;
get_compress_feature(_) ->
    [].

-spec get_tls_feature(state()) -> [starttls()].
get_tls_feature(#{stream_authenticated := false,
		  stream_encrypted := false} = State) ->
    case is_starttls_available(State) of
	true ->
	    TLSRequired = is_starttls_required(State),
	    [#starttls{required = TLSRequired}];
	false ->
	    []
    end;
get_tls_feature(_) ->
    [].

-spec get_bind_feature(state()) -> [bind()].
get_bind_feature(#{xmlns := ?NS_CLIENT,
		   stream_authenticated := true,
		   resource := <<"">>}) ->
    [#bind{}];
get_bind_feature(_) ->
    [].

-spec get_session_feature(state()) -> [xmpp_session()].
get_session_feature(#{xmlns := ?NS_CLIENT,
		      stream_authenticated := true,
		      resource := <<"">>}) ->
    [#xmpp_session{optional = true}];
get_session_feature(_) ->
    [].

-spec get_other_features(state()) -> [xmpp_element()].
get_other_features(#{stream_authenticated := Auth, mod := Mod} = State) ->
    try
	if Auth -> Mod:authenticated_stream_features(State);
	   true -> Mod:unauthenticated_stream_features(State)
	end
    catch _:undef ->
	    []
    end.

-spec is_starttls_available(state()) -> boolean().
is_starttls_available(#{mod := Mod} = State) ->
    try Mod:tls_enabled(State)
    catch _:undef -> true
    end.

-spec is_starttls_required(state()) -> boolean().
is_starttls_required(#{mod := Mod} = State) ->
    try Mod:tls_required(State)
    catch _:undef -> false
    end.

-spec set_from_to(xmpp_element(), state()) -> {ok, xmpp_element()} |
					      {error, stream_error()}.
set_from_to(Pkt, _State) when not ?is_stanza(Pkt) ->
    {ok, Pkt};
set_from_to(Pkt, #{user := U, server := S, resource := R,
		   lang := Lang, xmlns := ?NS_CLIENT}) ->
    JID = jid:make(U, S, R),
    From = case xmpp:get_from(Pkt) of
	       undefined -> JID;
	       F -> F
	   end,
    if JID#jid.luser == From#jid.luser andalso
       JID#jid.lserver == From#jid.lserver andalso
       (JID#jid.lresource == From#jid.lresource
	orelse From#jid.lresource == <<"">>) ->
	    To = case xmpp:get_to(Pkt) of
		     undefined -> jid:make(U, S);
		     T -> T
		 end,
	    {ok, xmpp:set_from_to(Pkt, JID, To)};
       true ->
	    Txt = <<"Improper 'from' attribute">>,
	    {error, xmpp:serr_invalid_from(Txt, Lang)}
    end;
set_from_to(Pkt, #{lang := Lang}) ->
    From = xmpp:get_from(Pkt),
    To = xmpp:get_to(Pkt),
    if From == undefined ->
	    Txt = <<"Missing 'from' attribute">>,
	    {error, xmpp:serr_improper_addressing(Txt, Lang)};
       To == undefined ->
	    Txt = <<"Missing 'to' attribute">>,
	    {error, xmpp:serr_improper_addressing(Txt, Lang)};
       true ->
	    {ok, Pkt}
    end.

-spec send_header(state()) -> state().
send_header(#{stream_version := Version} = State) ->
    send_header(State, #stream_start{version = Version}).

-spec send_header(state(), stream_start()) -> state().
send_header(#{stream_id := StreamID,
	      stream_version := MyVersion,
	      stream_header_sent := false,
	      lang := MyLang,
	      xmlns := NS} = State,
	    #stream_start{to = HisTo, from = HisFrom,
			  lang = HisLang, version = HisVersion}) ->
    Lang = select_lang(MyLang, HisLang),
    NS_DB = if NS == ?NS_SERVER -> ?NS_SERVER_DIALBACK;
	       true -> <<"">>
	    end,
    Version = case HisVersion of
		  undefined -> undefined;
		  {0,_} -> HisVersion;
		  _ -> MyVersion
	      end,
    StreamStart = #stream_start{version = Version,
				lang = Lang,
				xmlns = NS,
				stream_xmlns = ?NS_STREAM,
				db_xmlns = NS_DB,
				id = StreamID,
				to = HisFrom,
				from = HisTo},
    State1 = State#{lang => Lang,
		    stream_version => Version,
		    stream_header_sent => true},
    case socket_send(State1, StreamStart) of
	ok -> State1;
	{error, Why} -> process_stream_end({socket, Why}, State1)
    end;
send_header(State, _) ->
    State.

-spec send_pkt(state(), xmpp_element() | xmlel()) -> state().
send_pkt(#{mod := Mod} = State, Pkt) ->
    Result = socket_send(State, Pkt),
    State1 = try Mod:handle_send(Pkt, Result, State)
	     catch _:undef -> State
	     end,
    case Result of
	_ when is_record(Pkt, stream_error) ->
	    process_stream_end({stream, {out, Pkt}}, State1);
	ok ->
	    State1;
	{error, Why} ->
	    process_stream_end({socket, Why}, State1)
    end.

-spec send_error(state(), xmpp_element() | xmlel(), stanza_error()) -> state().
send_error(State, Pkt, Err) ->
    case xmpp:is_stanza(Pkt) of
	true ->
	    case xmpp:get_type(Pkt) of
		result -> State;
		error -> State;
		<<"result">> -> State;
		<<"error">> -> State;
		_ ->
		    ErrPkt = xmpp:make_error(Pkt, Err),
		    send_pkt(State, ErrPkt)
	    end;
	false ->
	    State
    end.

-spec send_trailer(state()) -> state().
send_trailer(State) ->
    socket_send(State, trailer),
    close_socket(State).

-spec socket_send(state(), xmpp_element() | xmlel() | trailer) -> ok | {error, inet:posix()}.
socket_send(#{socket := Sock,
	      stream_state := StateName,
	      xmlns := NS,
	      stream_header_sent := true}, Pkt) ->
    case Pkt of
	trailer ->
	    xmpp_socket:send_trailer(Sock);
	#stream_start{} when StateName /= disconnected ->
	    xmpp_socket:send_header(Sock, xmpp:encode(Pkt));
	_ when StateName /= disconnected ->
	    xmpp_socket:send_element(Sock, xmpp:encode(Pkt, NS));
	_ ->
	    {error, closed}
    end;
socket_send(_, _) ->
    {error, closed}.

-spec close_socket(state()) -> state().
close_socket(#{socket := Socket} = State) ->
    xmpp_socket:close(Socket),
    State#{stream_timeout => infinity,
	   stream_state => disconnected}.

-spec select_lang(binary(), binary()) -> binary().
select_lang(Lang, <<"">>) -> Lang;
select_lang(_, Lang) -> Lang.

-spec set_lang(xmpp_element(), state()) -> xmpp_element().
set_lang(Pkt, #{lang := MyLang, xmlns := ?NS_CLIENT}) when ?is_stanza(Pkt) ->
    HisLang = xmpp:get_lang(Pkt),
    Lang = select_lang(MyLang, HisLang),
    xmpp:set_lang(Pkt, Lang);
set_lang(Pkt, _) ->
    Pkt.

-spec format_inet_error(atom()) -> string().
format_inet_error(closed) ->
    "connection closed";
format_inet_error(Reason) ->
    case inet:format_error(Reason) of
	"unknown POSIX error" -> atom_to_list(Reason);
	Txt -> Txt
    end.

-spec format_stream_error(atom() | 'see-other-host'(), [text()]) -> string().
format_stream_error(Reason, Txt) ->
    Slogan = case Reason of
		 undefined -> "no reason";
		 #'see-other-host'{} -> "see-other-host";
		 _ -> atom_to_list(Reason)
	     end,
    case xmpp:get_text(Txt) of
	<<"">> ->
	    Slogan;
	Data ->
	    binary_to_list(Data) ++ " (" ++ Slogan ++ ")"
    end.

-spec format_sasl_error(cyrsasl:mechanism(), atom()) -> {atom(), binary()}.
format_sasl_error(<<"EXTERNAL">>, Err) ->
    xmpp_stream_pkix:format_error(Err);
format_sasl_error(Mech, Err) ->
    cyrsasl:format_error(Mech, Err).

-spec format_tls_error(atom() | binary()) -> list().
format_tls_error(Reason) when is_atom(Reason) ->
    format_inet_error(Reason);
format_tls_error(Reason) ->
    Reason.

-spec format(io:format(), list()) -> binary().
format(Fmt, Args) ->
    iolist_to_binary(io_lib:format(Fmt, Args)).

-spec lists_intersection(list(), list()) -> list().
lists_intersection(L1, L2) ->
    lists:filter(
      fun(E) ->
	      lists:member(E, L2)
      end, L1).

-spec identity([cyrsasl:sasl_property()]) -> binary().
identity(Props) ->
    case proplists:get_value(authzid, Props, <<>>) of
	<<>> -> proplists:get_value(username, Props, <<>>);
	AuthzId -> AuthzId
    end.
