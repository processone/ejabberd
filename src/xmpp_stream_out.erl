%%%-------------------------------------------------------------------
%%% Created : 14 Dec 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
-module(xmpp_stream_out).
-define(GEN_SERVER, p1_server).
-behaviour(?GEN_SERVER).

-protocol({rfc, 6120}).
-protocol({xep, 114, '1.6'}).

%% API
-export([start/3, start_link/3, call/3, cast/2, reply/2, connect/1,
	 stop/1, send/2, close/1, close/2, establish/1, format_error/1,
	 set_timeout/2, get_transport/1, change_shaper/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%-define(DBGFSM, true).
-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-define(TCP_SEND_TIMEOUT, 15000).

-include("xmpp.hrl").
-include_lib("kernel/include/inet.hrl").

-type state() :: map().
-type noreply() :: {noreply, state(), timeout()}.
-type host_port() :: {inet:hostname(), inet:port_number()}.
-type ip_port() :: {inet:ip_address(), inet:port_number()}.
-type network_error() :: {error, inet:posix() | inet_res:res_error()}.
-type stop_reason() :: {idna, bad_string} |
		       {dns, inet:posix() | inet_res:res_error()} |
		       {stream, reset | {in | out, stream_error()}} |
		       {tls, inet:posix() | atom() | binary()} |
		       {pkix, binary()} |
		       {auth, atom() | binary() | string()} |
		       {socket, inet:posix() | closed | timeout} |
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
-callback handle_stream_downgraded(stream_start(), state()) -> state().
-callback handle_stream_end(stop_reason(), state()) -> state().
-callback handle_cdata(binary(), state()) -> state().
-callback handle_send(xmpp_element(), ok | {error, inet:posix()}, state()) -> state().
-callback handle_recv(fxml:xmlel(), xmpp_element() | {error, term()}, state()) -> state().
-callback handle_timeout(state()) -> state().
-callback handle_authenticated_features(stream_features(), state()) -> state().
-callback handle_unauthenticated_features(stream_features(), state()) -> state().
-callback handle_auth_success(cyrsasl:mechanism(), state()) -> state().
-callback handle_auth_failure(cyrsasl:mechanism(), binary(), state()) -> state().
-callback handle_packet(xmpp_element(), state()) -> state().
-callback tls_options(state()) -> [proplists:property()].
-callback tls_required(state()) -> boolean().
-callback tls_verify(state()) -> boolean().
-callback tls_enabled(state()) -> boolean().
-callback dns_timeout(state()) -> timeout().
-callback dns_retries(state()) -> non_neg_integer().
-callback default_port(state()) -> inet:port_number().
-callback address_families(state()) -> [inet:address_family()].
-callback connect_timeout(state()) -> timeout().

-optional_callbacks([init/1,
		     handle_cast/2,
		     handle_call/3,
		     handle_info/2,
		     terminate/2,
		     code_change/3,
		     handle_stream_start/2,
		     handle_stream_established/1,
		     handle_stream_downgraded/2,
		     handle_stream_end/2,
		     handle_cdata/2,
		     handle_send/3,
		     handle_recv/3,
		     handle_timeout/1,
		     handle_authenticated_features/2,
		     handle_unauthenticated_features/2,
		     handle_auth_success/2,
		     handle_auth_failure/3,
		     handle_packet/2,
		     tls_options/1,
		     tls_required/1,
		     tls_verify/1,
		     tls_enabled/1,
		     dns_timeout/1,
		     dns_retries/1,
		     default_port/1,
		     address_families/1,
		     connect_timeout/1]).

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

-spec connect(pid()) -> ok.
connect(Ref) ->
    cast(Ref, connect).

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
close(Ref) ->
    close(Ref, true).

-spec close(pid(), boolean()) -> ok;
	   (state(), boolean()) -> state().
close(Pid, SendTrailer) when is_pid(Pid) ->
    cast(Pid, {close, SendTrailer});
close(#{owner := Owner} = State, SendTrailer) when Owner == self() ->
    if SendTrailer -> send_trailer(State);
       true -> close_socket(State)
    end;
close(_, _) ->
    erlang:error(badarg).

-spec establish(state()) -> state().
establish(State) ->
    process_stream_established(State).

-spec set_timeout(state(), timeout()) -> state().
set_timeout(#{owner := Owner} = State, Timeout) when Owner == self() ->
    case Timeout of
	infinity -> State#{stream_timeout => infinity};
	_ ->
	    Time = p1_time_compat:monotonic_time(milli_seconds),
	    State#{stream_timeout => {Timeout, Time}}
    end;
set_timeout(_, _) ->
    erlang:error(badarg).

get_transport(#{sockmod := SockMod, socket := Socket, owner := Owner})
  when Owner == self() ->
    SockMod:get_transport(Socket);
get_transport(_) ->
    erlang:error(badarg).

-spec change_shaper(state(), shaper:shaper()) -> ok.
change_shaper(#{sockmod := SockMod, socket := Socket, owner := Owner}, Shaper)
  when Owner == self() ->
    SockMod:change_shaper(Socket, Shaper);
change_shaper(_, _) ->
    erlang:error(badarg).

-spec format_error(stop_reason()) ->  binary().
format_error({idna, _}) ->
    <<"Remote domain is not an IDN hostname">>;
format_error({dns, Reason}) ->
    format("DNS lookup failed: ~s", [format_inet_error(Reason)]);
format_error({socket, Reason}) ->
    format("Connection failed: ~s", [format_inet_error(Reason)]);
format_error({pkix, Reason}) ->
    {_, ErrTxt} = xmpp_stream_pkix:format_error(Reason),
    format("Peer certificate rejected: ~s", [ErrTxt]);
format_error({stream, reset}) ->
    <<"Stream reset by peer">>;
format_error({stream, {in, #stream_error{reason = Reason, text = Txt}}}) ->
    format("Stream closed by peer: ~s", [format_stream_error(Reason, Txt)]);
format_error({stream, {out, #stream_error{reason = Reason, text = Txt}}}) ->
    format("Stream closed by us: ~s", [format_stream_error(Reason, Txt)]);
format_error({tls, Reason}) ->
    format("TLS failed: ~s", [format_tls_error(Reason)]);
format_error({auth, Reason}) ->
    format("Authentication failed: ~s", [Reason]);
format_error(internal_failure) ->
    <<"Internal server error">>;
format_error(Err) ->
    format("Unrecognized error: ~w", [Err]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init(list()) -> {ok, state(), timeout()} | {stop, term()} | ignore.
init([Mod, SockMod, From, To, Opts]) ->
    Time = p1_time_compat:monotonic_time(milli_seconds),
    State = #{owner => self(),
	      mod => Mod,
	      sockmod => SockMod,
	      server => From,
	      user => <<"">>,
	      resource => <<"">>,
	      lang => <<"">>,
	      remote_server => To,
	      xmlns => ?NS_SERVER,
	      stream_direction => out,
	      stream_timeout => {timer:seconds(30), Time},
	      stream_id => new_id(),
	      stream_encrypted => false,
	      stream_verified => false,
	      stream_authenticated => false,
	      stream_restarted => false,
	      stream_state => connecting},
    case try Mod:init([State, Opts])
	 catch _:undef -> {ok, State}
	 end of
	{ok, State1} ->
	    {_, State2, Timeout} = noreply(State1),
	    {ok, State2, Timeout};
	{error, Reason} ->
	    {stop, Reason};
	ignore ->
	    ignore
    end.

-spec handle_call(term(), term(), state()) -> noreply().
handle_call(Call, From, #{mod := Mod} = State) ->
    noreply(try Mod:handle_call(Call, From, State)
	    catch _:undef -> State
	    end).

-spec handle_cast(term(), state()) -> noreply().
handle_cast(connect, #{remote_server := RemoteServer,
		       sockmod := SockMod,
		       stream_state := connecting} = State) ->
    noreply(
      case idna_to_ascii(RemoteServer) of
	  false ->
	      process_stream_end({idna, bad_string}, State);
	  ASCIIName ->
	      case resolve(binary_to_list(ASCIIName), State) of
		  {ok, AddrPorts} ->
		      case connect(AddrPorts, State) of
			  {ok, Socket, AddrPort} ->
			      SocketMonitor = SockMod:monitor(Socket),
			      State1 = State#{ip => AddrPort,
					      socket => Socket,
					      socket_monitor => SocketMonitor},
			      State2 = State1#{stream_state => wait_for_stream},
			      send_header(State2);
			  {error, Why} ->
			      process_stream_end({socket, Why}, State)
		      end;
		  {error, Why} ->
		      process_stream_end({dns, Why}, State)
	      end
      end);
handle_cast(connect, State) ->
    %% Ignoring connection attempts in other states
    noreply(State);
handle_cast({send, Pkt}, State) ->
    noreply(send_pkt(State, Pkt));
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({close, SendTrailer}, #{mod := Mod} = State) ->
    noreply(
      case is_disconnected(State) of
	  true -> State;
	  false ->
	      State1 = close(State, SendTrailer),
	      try Mod:handle_stream_end({socket, closed}, State1)
	      catch _:undef -> stop(State1)
	      end
      end);
handle_cast(Cast, #{mod := Mod} = State) ->
    noreply(try Mod:handle_cast(Cast, State)
	    catch _:undef -> State
	    end).

-spec handle_info(term(), state()) -> noreply().
handle_info({'$gen_event', {xmlstreamstart, Name, Attrs}},
	    #{stream_state := wait_for_stream,
	      xmlns := XMLNS, lang := MyLang} = State) ->
    El = #xmlel{name = Name, attrs = Attrs},
    noreply(
      try xmpp:decode(El, XMLNS, []) of
	  #stream_start{} = Pkt ->
	      process_stream(Pkt, State);
	  _ ->
	      send_pkt(State, xmpp:serr_invalid_xml())
      catch _:{xmpp_codec, Why} ->
	      Txt = xmpp:io_format_error(Why),
	      Lang = select_lang(MyLang, xmpp:get_lang(El)),
	      Err = xmpp:serr_invalid_xml(Txt, Lang),
	      send_pkt(State, Err)
      end);
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
handle_info({'$gen_event', {xmlstreamend, _}}, State) ->
    noreply(process_stream_end({stream, reset}, State));
handle_info({'$gen_event', closed}, State) ->
    noreply(process_stream_end({socket, closed}, State));
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
handle_info(Info, #{mod := Mod} = State) ->
    noreply(try Mod:handle_info(Info, State)
	    catch _:undef -> State
	    end).

-spec terminate(term(), state()) -> any().
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
-spec noreply(state()) -> noreply().
noreply(#{stream_timeout := infinity} = State) ->
    {noreply, State, infinity};
noreply(#{stream_timeout := {MSecs, OldTime}} = State) ->
    NewTime = p1_time_compat:monotonic_time(milli_seconds),
    Timeout = max(0, MSecs - NewTime + OldTime),
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
	    State
    end.

-spec process_stream_end(stop_reason(), state()) -> state().
process_stream_end(_, #{stream_state := disconnected} = State) ->
    State;
process_stream_end(Reason, #{mod := Mod} = State) ->
    State1 = send_trailer(State),
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
process_stream(#stream_start{lang = Lang, id = ID,
			     version = Version} = StreamStart,
	       #{mod := Mod} = State) ->
    State1 = State#{stream_remote_id => ID, lang => Lang},
    State2 = try Mod:handle_stream_start(StreamStart, State1)
	     catch _:undef -> State1
	     end,
    case is_disconnected(State2) of
	true -> State2;
	false ->
	    case Version of
		{1, _} ->
		    State2#{stream_state => wait_for_features};
		_ ->
		    process_stream_downgrade(StreamStart, State2)
	    end
    end.

-spec process_element(xmpp_element(), state()) -> state().
process_element(Pkt, #{stream_state := StateName} = State) ->
    case Pkt of
	#stream_features{} when StateName == wait_for_features ->
	    process_features(Pkt, State);
	#starttls_proceed{} when StateName == wait_for_starttls_response ->
	    process_starttls(State);
	#sasl_success{} when StateName == wait_for_sasl_response ->
	    process_sasl_success(State);
	#sasl_failure{} when StateName == wait_for_sasl_response ->
	    process_sasl_failure(Pkt, State);
	#stream_error{} ->
	    process_stream_end({stream, {in, Pkt}}, State);
	_ when is_record(Pkt, stream_features);
	       is_record(Pkt, starttls_proceed);
	       is_record(Pkt, starttls);
	       is_record(Pkt, sasl_auth);
	       is_record(Pkt, sasl_success);
	       is_record(Pkt, sasl_failure);
	       is_record(Pkt, sasl_response);
	       is_record(Pkt, sasl_abort);
	       is_record(Pkt, compress);
	       is_record(Pkt, handshake) ->
	    %% Do not pass this crap upstream
	    State;
	_ ->
	    process_packet(Pkt, State)
    end.

-spec process_features(stream_features(), state()) -> state().
process_features(StreamFeatures,
		 #{stream_authenticated := true, mod := Mod} = State) ->
    State1 = try Mod:handle_authenticated_features(StreamFeatures, State)
	     catch _:undef -> State
	     end,
    process_stream_established(State1);
process_features(#stream_features{sub_els = Els} = StreamFeatures,
		 #{stream_encrypted := Encrypted,
		   mod := Mod, lang := Lang} = State) ->
    State1 = try Mod:handle_unauthenticated_features(StreamFeatures, State)
	     catch _:undef -> State
	     end,
    case is_disconnected(State1) of
	true -> State1;
	false ->
	    TLSRequired = is_starttls_required(State1),
	    TLSAvailable = is_starttls_available(State1),
	    %% TODO: improve xmpp.erl
	    Msg = #message{sub_els = Els},
	    case xmpp:get_subtag(Msg, #starttls{}) of
		false when TLSRequired and not Encrypted ->
		    Txt = <<"Use of STARTTLS required">>,
		    send_pkt(State1, xmpp:serr_policy_violation(Txt, Lang));
		false when not Encrypted ->
		    process_sasl_failure(
		      <<"Peer doesn't support STARTTLS">>, State1);
		#starttls{required = true} when not TLSAvailable and not Encrypted ->
		    Txt = <<"Use of STARTTLS forbidden">>,
		    send_pkt(State1, xmpp:serr_unsupported_feature(Txt, Lang));
		#starttls{} when TLSAvailable and not Encrypted ->
		    State2 = State1#{stream_state => wait_for_starttls_response},
		    send_pkt(State2, #starttls{});
		#starttls{} when not Encrypted ->
		    process_sasl_failure(
		      <<"STARTTLS is disabled in local configuration">>, State1);
		_ ->
		    State2 = process_cert_verification(State1),
		    case is_disconnected(State2) of
			true -> State2;
			false ->
			    case xmpp:get_subtag(Msg, #sasl_mechanisms{}) of
				#sasl_mechanisms{list = Mechs} ->
				    process_sasl_mechanisms(Mechs, State2);
				false ->
				    process_sasl_failure(
				      <<"Peer provided no SASL mechanisms">>, State2)
			    end
		    end
	    end
    end.

-spec process_stream_established(state()) -> state().
process_stream_established(#{stream_state := StateName} = State)
  when StateName == disconnected; StateName == established ->
    State;
process_stream_established(#{mod := Mod} = State) ->
    State1 = State#{stream_authenticated := true,
		    stream_state => established,
		    stream_timeout => infinity},
    try Mod:handle_stream_established(State1)
    catch _:undef -> State1
    end.

-spec process_sasl_mechanisms([binary()], state()) -> state().
process_sasl_mechanisms(Mechs, #{user := User, server := Server} = State) ->
    %% TODO: support other mechanisms
    Mech = <<"EXTERNAL">>,
    case lists:member(<<"EXTERNAL">>, Mechs) of
	true ->
	    State1 = State#{stream_state => wait_for_sasl_response},
	    Authzid = jid:encode(jid:make(User, Server)),
	    send_pkt(State1, #sasl_auth{mechanism = Mech, text = Authzid});
	false ->
	    process_sasl_failure(
	      <<"Peer doesn't support EXTERNAL authentication">>, State)
    end.

-spec process_starttls(state()) -> state().
process_starttls(#{sockmod := SockMod, socket := Socket, mod := Mod} = State) ->
    TLSOpts = try Mod:tls_options(State)
	      catch _:undef -> []
	      end,
    case SockMod:starttls(Socket, [connect|TLSOpts]) of
	{ok, TLSSocket} ->
	    State1 = State#{socket => TLSSocket,
			    stream_id => new_id(),
			    stream_restarted => true,
			    stream_state => wait_for_stream,
			    stream_encrypted => true},
	    send_header(State1);
	{error, Why} ->
	    process_stream_end({tls, Why}, State)
    end.

-spec process_stream_downgrade(stream_start(), state()) -> state().
process_stream_downgrade(StreamStart,
			 #{mod := Mod, lang := Lang,
			   stream_encrypted := Encrypted} = State) ->
    TLSRequired = is_starttls_required(State),
    if not Encrypted and TLSRequired ->
	    Txt = <<"Use of STARTTLS required">>,
	    send_pkt(State, xmpp:serr_policy_violation(Txt, Lang));
       true ->
	    State1 = State#{stream_state => downgraded},
	    try Mod:handle_stream_downgraded(StreamStart, State1)
	    catch _:undef ->
		    send_pkt(State1, xmpp:serr_unsupported_version())
	    end
    end.

-spec process_cert_verification(state()) -> state().
process_cert_verification(#{stream_encrypted := true,
			    stream_verified := false,
			    mod := Mod} = State) ->
    case try Mod:tls_verify(State)
	 catch _:undef -> true
	 end of
	true ->
	    case xmpp_stream_pkix:authenticate(State) of
		{ok, _} ->
		    State#{stream_verified => true};
		{error, Why, _Peer} ->
		    process_stream_end({pkix, Why}, State)
	    end;
	false ->
	    State#{stream_verified => true}
    end;
process_cert_verification(State) ->
    State.

-spec process_sasl_success(state()) -> state().
process_sasl_success(#{mod := Mod,
		       sockmod := SockMod,
		       socket := Socket} = State) ->
    SockMod:reset_stream(Socket),
    State1 = State#{stream_id => new_id(),
		    stream_restarted => true,
		    stream_state => wait_for_stream,
		    stream_authenticated => true},
    State2 = send_header(State1),
    case is_disconnected(State2) of
	true -> State2;
	false ->
	    try Mod:handle_auth_success(<<"EXTERNAL">>, State2)
	    catch _:undef -> State2
	    end
    end.

-spec process_sasl_failure(sasl_failure() | binary(), state()) -> state().
process_sasl_failure(#sasl_failure{} = Failure, State) ->
    Reason = format("Peer responded with error: ~s",
		    [format_sasl_failure(Failure)]),
    process_sasl_failure(Reason, State);
process_sasl_failure(Reason, #{mod := Mod} = State) ->
    try Mod:handle_auth_failure(<<"EXTERNAL">>, {auth, Reason}, State)
    catch _:undef -> process_stream_end({auth, Reason}, State)
    end.

-spec process_packet(xmpp_element(), state()) -> state().
process_packet(Pkt, #{mod := Mod} = State) ->
    try Mod:handle_packet(Pkt, State)
    catch _:undef -> State
    end.

-spec is_starttls_required(state()) -> boolean().
is_starttls_required(#{mod := Mod} = State) ->
    try Mod:tls_required(State)
    catch _:undef -> false
    end.

-spec is_starttls_available(state()) -> boolean().
is_starttls_available(#{mod := Mod} = State) ->
    try Mod:tls_enabled(State)
    catch _:undef -> true
    end.

-spec send_header(state()) -> state().
send_header(#{remote_server := RemoteServer,
	      stream_encrypted := Encrypted,
	      lang := Lang,
	      xmlns := NS,
	      user := User,
	      resource := Resource,
	      server := Server} = State) ->
    NS_DB = if NS == ?NS_SERVER -> ?NS_SERVER_DIALBACK;
	       true -> <<"">>
	    end,
    From = if Encrypted ->
		   jid:make(User, Server, Resource);
	      NS == ?NS_SERVER ->
		   jid:make(Server);
	      true ->
		   undefined
	   end,
    StreamStart = #stream_start{xmlns = NS,
				lang = Lang,
				stream_xmlns = ?NS_STREAM,
				db_xmlns = NS_DB,
				from = From,
				to = jid:make(RemoteServer),
				version = {1,0}},
    case socket_send(State, StreamStart) of
	ok -> State;
	{error, Why} -> process_stream_end({socket, Why}, State)
    end.

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

-spec socket_send(state(), xmpp_element() | xmlel() | trailer) -> ok | {error, inet:posix()}.
socket_send(#{sockmod := SockMod, socket := Socket, xmlns := NS,
	      stream_state := StateName}, Pkt) when StateName /= disconnected ->
    case Pkt of
	trailer ->
	    SockMod:send_trailer(Socket);
	#stream_start{} ->
	    SockMod:send_header(Socket, xmpp:encode(Pkt));
	_ ->
	    SockMod:send_element(Socket, xmpp:encode(Pkt, NS))
    end;
socket_send(_, _) ->
    {error, closed}.

-spec send_trailer(state()) -> state().
send_trailer(State) ->
    socket_send(State, trailer),
    close_socket(State).

-spec close_socket(state()) -> state().
close_socket(#{stream_state := disconnected} = State) ->
    State;
close_socket(State) ->
    case State of
	#{sockmod := SockMod, socket := Socket} ->
	    SockMod:close(Socket);
	_ ->
	    ok
    end,
    State#{stream_timeout => infinity,
	   stream_state => disconnected}.

-spec select_lang(binary(), binary()) -> binary().
select_lang(Lang, <<"">>) -> Lang;
select_lang(_, Lang) -> Lang.

-spec format_inet_error(atom()) -> string().
format_inet_error(closed) ->
    "connection closed";
format_inet_error(Reason) ->
    case inet:format_error(Reason) of
	"unknown POSIX error" -> atom_to_list(Reason);
	Txt -> Txt
    end.

-spec format_stream_error(atom() | 'see-other-host'(), undefined | text()) -> string().
format_stream_error(Reason, Txt) ->
    Slogan = case Reason of
		 undefined -> "no reason";
		 #'see-other-host'{} -> "see-other-host";
		 _ -> atom_to_list(Reason)
	     end,
    case Txt of
	undefined -> Slogan;
	#text{data = <<"">>} -> Slogan;
	#text{data = Data} ->
	    binary_to_list(Data) ++ " (" ++ Slogan ++ ")"
    end.

-spec format_tls_error(atom() | binary()) -> list().
format_tls_error(Reason) when is_atom(Reason) ->
    format_inet_error(Reason);
format_tls_error(Reason) ->
    binary_to_list(Reason).

format_sasl_failure(#sasl_failure{reason = Reason, text = Txt}) ->
    Slogan = case Reason of
		 undefined -> "no reason";
		 _ -> atom_to_list(Reason)
	     end,
    case xmpp:get_text(Txt) of
	<<"">> -> Slogan;
	Data ->
	    binary_to_list(Data) ++ " (" ++ Slogan ++ ")"
    end.
		      
-spec format(io:format(), list()) -> binary().
format(Fmt, Args) ->
    iolist_to_binary(io_lib:format(Fmt, Args)).

%%%===================================================================
%%% Connection stuff
%%%===================================================================
-spec idna_to_ascii(binary()) -> binary() | false.
idna_to_ascii(<<$[, _/binary>> = Host) ->
    %% This is an IPv6 address in 'IP-literal' format (as per RFC7622)
    %% We remove brackets here
    case binary:last(Host) of
	$] ->
	    IPv6 = binary:part(Host, {1, size(Host)-2}),
	    case inet:parse_ipv6strict_address(binary_to_list(IPv6)) of
		{ok, _} -> IPv6;
		{error, _} -> false
	    end;
	_ ->
	    false
    end;
idna_to_ascii(Host) ->
    case inet:parse_address(binary_to_list(Host)) of
	{ok, _} -> Host;
	{error, _} -> ejabberd_idna:domain_utf8_to_ascii(Host)
    end.

-spec resolve(string(), state()) -> {ok, [ip_port()]} | network_error().
resolve(Host, State) ->
    case srv_lookup(Host, State) of
	{error, _Reason} ->
	    DefaultPort = get_default_port(State),
	    a_lookup([{Host, DefaultPort}], State);
	{ok, HostPorts} ->
	    a_lookup(HostPorts, State)
    end.

-spec srv_lookup(string(), state()) -> {ok, [host_port()]} | network_error().
srv_lookup(_Host, #{xmlns := ?NS_COMPONENT}) ->
    %% Do not attempt to lookup SRV for component connections
    {error, nxdomain};
srv_lookup(Host, State) ->
    %% Only perform SRV lookups for FQDN names
    case string:chr(Host, $.) of
	0 ->
	    {error, nxdomain};
	_ ->
	    case inet:parse_address(Host) of
		{ok, _} ->
		    {error, nxdomain};
		{error, _} ->
		    Timeout = get_dns_timeout(State),
		    Retries = get_dns_retries(State),
		    srv_lookup(Host, Timeout, Retries)
	    end
    end.

-spec srv_lookup(string(), timeout(), integer()) ->
			{ok, [host_port()]} | network_error().
srv_lookup(_Host, _Timeout, Retries) when Retries < 1 ->
    {error, timeout};
srv_lookup(Host, Timeout, Retries) ->
    SRVName = "_xmpp-server._tcp." ++ Host,
    case inet_res:getbyname(SRVName, srv, Timeout) of
	{ok, HostEntry} ->
	    host_entry_to_host_ports(HostEntry);
	{error, timeout} ->
	    srv_lookup(Host, Timeout, Retries - 1);
	{error, _} = Err ->
	    Err
    end.

-spec a_lookup([{inet:hostname(), inet:port_number()}], state()) ->
		      {ok, [ip_port()]} | network_error().
a_lookup(HostPorts, State) ->
    HostPortFamilies = [{Host, Port, Family}
			|| {Host, Port} <- HostPorts,
			   Family <- get_address_families(State)],
    a_lookup(HostPortFamilies, State, [], {error, nxdomain}).

-spec a_lookup([{inet:hostname(), inet:port_number(), inet:address_family()}],
	       state(), [ip_port()], network_error()) -> {ok, [ip_port()]} | network_error().
a_lookup([{Host, Port, Family}|HostPortFamilies], State, Acc, Err) ->
    Timeout = get_dns_timeout(State),
    Retries = get_dns_retries(State),
    case a_lookup(Host, Port, Family, Timeout, Retries) of
	{error, Reason} ->
	    a_lookup(HostPortFamilies, State, Acc, {error, Reason});
	{ok, AddrPorts} ->
	    a_lookup(HostPortFamilies, State, Acc ++ AddrPorts, Err)
    end;
a_lookup([], _State, [], Err) ->
    Err;
a_lookup([], _State, Acc, _) ->
    {ok, Acc}.

-spec a_lookup(inet:hostname(), inet:port_number(), inet:address_family(),
	       timeout(), integer()) -> {ok, [ip_port()]} | network_error().
a_lookup(_Host, _Port, _Family, _Timeout, Retries) when Retries < 1 ->
    {error, timeout};
a_lookup(Host, Port, Family, Timeout, Retries) ->
    Start = p1_time_compat:monotonic_time(milli_seconds),
    case inet:gethostbyname(Host, Family, Timeout) of
	{error, nxdomain} = Err ->
	    %% inet:gethostbyname/3 doesn't return {error, timeout},
	    %% so we should check if 'nxdomain' is in fact a result
	    %% of a timeout.
	    %% We also cannot use inet_res:gethostbyname/3 because
	    %% it ignores DNS configuration settings (/etc/hosts, etc)
	    End = p1_time_compat:monotonic_time(milli_seconds),
	    if (End - Start) >= Timeout ->
		    a_lookup(Host, Port, Family, Timeout, Retries - 1);
	       true ->
		    Err
	    end;
	{error, _} = Err ->
	    Err;
	{ok, HostEntry} ->
	    host_entry_to_addr_ports(HostEntry, Port)
    end.

-spec host_entry_to_host_ports(inet:hostent()) -> {ok, [host_port()]} |
						  {error, nxdomain}.
host_entry_to_host_ports(#hostent{h_addr_list = AddrList}) ->
    PrioHostPorts = lists:flatmap(
		      fun({Priority, Weight, Port, Host}) ->
			      N = case Weight of
				      0 -> 0;
				      _ -> (Weight + 1) * randoms:uniform()
				  end,
			      [{Priority * 65536 - N, Host, Port}];
			 (_) ->
			      []
		      end, AddrList),
    HostPorts = [{Host, Port}
		 || {_Priority, Host, Port} <- lists:usort(PrioHostPorts)],
    case HostPorts of
	[] -> {error, nxdomain};
	_ -> {ok, HostPorts}
    end.

-spec host_entry_to_addr_ports(inet:hostent(), inet:port_number()) ->
				      {ok, [ip_port()]} | {error, nxdomain}.
host_entry_to_addr_ports(#hostent{h_addr_list = AddrList}, Port) ->
    AddrPorts = lists:flatmap(
		  fun(Addr) ->
			  try get_addr_type(Addr) of
			      _ -> [{Addr, Port}]
			  catch _:_ ->
				  []
			  end
		  end, AddrList),
    case AddrPorts of
	[] -> {error, nxdomain};
	_ -> {ok, AddrPorts}
    end.

-spec connect([ip_port()], state()) -> {ok, term(), ip_port()} | network_error().
connect(AddrPorts, #{sockmod := SockMod} = State) ->
    Timeout = get_connect_timeout(State),
    connect(AddrPorts, SockMod, Timeout, {error, nxdomain}).

-spec connect([ip_port()], module(), timeout(), network_error()) ->
		     {ok, term(), ip_port()} | network_error().
connect([{Addr, Port}|AddrPorts], SockMod, Timeout, _) ->
    Type = get_addr_type(Addr),
    try SockMod:connect(Addr, Port,
			[binary, {packet, 0},
			 {send_timeout, ?TCP_SEND_TIMEOUT},
			 {send_timeout_close, true},
			 {active, false}, Type],
			Timeout) of
	{ok, Socket} ->
	    {ok, Socket, {Addr, Port}};
	Err ->
	    connect(AddrPorts, SockMod, Timeout, Err)
    catch _:badarg ->
	    connect(AddrPorts, SockMod, Timeout, {error, einval})
    end;
connect([], _SockMod, _Timeout, Err) ->
    Err.

-spec get_addr_type(inet:ip_address()) -> inet:address_family().
get_addr_type({_, _, _, _}) -> inet;
get_addr_type({_, _, _, _, _, _, _, _}) -> inet6.

-spec get_dns_timeout(state()) -> timeout().
get_dns_timeout(#{mod := Mod} = State) ->
    try Mod:dns_timeout(State)
    catch _:undef -> timer:seconds(10)
    end.

-spec get_dns_retries(state()) -> non_neg_integer().
get_dns_retries(#{mod := Mod} = State) ->
    try Mod:dns_retries(State)
    catch _:undef -> 2
    end.

-spec get_default_port(state()) -> inet:port_number().
get_default_port(#{mod := Mod, xmlns := NS} = State) ->
    try Mod:default_port(State)
    catch _:undef when NS == ?NS_SERVER -> 5269;
	  _:undef when NS == ?NS_CLIENT -> 5222
    end.

-spec get_address_families(state()) -> [inet:address_family()].
get_address_families(#{mod := Mod} = State) ->
    try Mod:address_families(State)
    catch _:undef -> [inet, inet6]
    end.

-spec get_connect_timeout(state()) -> timeout().
get_connect_timeout(#{mod := Mod} = State) ->
    try Mod:connect_timeout(State)
    catch _:undef -> timer:seconds(10)
    end.
