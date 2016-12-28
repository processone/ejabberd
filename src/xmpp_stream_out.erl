%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 14 Dec 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(xmpp_stream_out).
-behaviour(gen_server).

-protocol({rfc, 6120}).

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
-include("logger.hrl").
-include_lib("kernel/include/inet.hrl").

-type state() :: map().
-type host_port() :: {inet:hostname(), inet:port_number()}.
-type ip_port() :: {inet:ip_address(), inet:port_number()}.
-type network_error() :: {error, inet:posix() | inet_res:res_error()}.
-type stop_reason() :: {idna, bad_string} |
		       {dns, inet:posix() | inet_res:res_error()} |
		       {stream, reset | stream_error()} |
		       {tls, term()} |
		       {pkix, binary()} |
		       {auth, atom() | binary() | string()} |
		       {socket, inet:posix() | closed | timeout}.

-callback init(list()) -> {ok, state()} | {stop, term()} | ignore.

%%%===================================================================
%%% API
%%%===================================================================
start(Mod, Args, Opts) ->
    gen_server:start(?MODULE, [Mod|Args], Opts ++ ?FSMOPTS).

start_link(Mod, Args, Opts) ->
    gen_server:start_link(?MODULE, [Mod|Args], Opts ++ ?FSMOPTS).

call(Ref, Msg, Timeout) ->
    gen_server:call(Ref, Msg, Timeout).

cast(Ref, Msg) ->
    gen_server:cast(Ref, Msg).

reply(Ref, Reply) ->
    gen_server:reply(Ref, Reply).

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
    send_element(State, Pkt);
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
    <<"Not an IDN hostname">>;
format_error({dns, Reason}) ->
    format("DNS lookup failed: ~s", [format_inet_error(Reason)]);
format_error({socket, Reason}) ->
    format("Connection failed: ~s", [format_inet_error(Reason)]);
format_error({pkix, Reason}) ->
    format("Peer certificate rejected: ~s", [Reason]);
format_error({stream, reset}) ->
    <<"Stream reset by peer">>;
format_error({stream, #stream_error{reason = Reason, text = Txt}}) ->
    format("Stream failed: ~s", [format_stream_error(Reason, Txt)]);
format_error({tls, Reason}) ->
    format("TLS failed: ~w", [Reason]);
format_error({auth, Reason}) ->
    format("Authentication failed: ~s", [Reason]);
format_error(Err) ->
    format("Unrecognized error: ~w", [Err]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
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
	Err ->
	    Err
    end.

handle_call(Call, From, #{mod := Mod} = State) ->
    noreply(try Mod:handle_call(Call, From, State)
	    catch _:undef -> State
	    end).

handle_cast(connect, #{remote_server := RemoteServer,
		       sockmod := SockMod,
		       stream_state := connecting} = State) ->
    case ejabberd_idna:domain_utf8_to_ascii(RemoteServer) of
	false ->
	    noreply(process_stream_close({error, {idna, bad_string}}, State));
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
			    noreply(send_header(State2));
			{error, Why} ->
			    Err = {error, {socket, Why}},
			    noreply(process_stream_close(Err, State))
		    end;
		{error, Why} ->
		    noreply(process_stream_close({error, {dns, Why}}, State))
	    end
    end;
handle_cast(connect, State) ->
    %% Ignoring connection attempts in other states
    noreply(State);
handle_cast({send, Pkt}, State) ->
    noreply(send_element(State, Pkt));
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Cast, #{mod := Mod} = State) ->
    noreply(try Mod:handle_cast(Cast, State)
	    catch _:undef -> State
	    end).

handle_info({'$gen_event', {xmlstreamstart, Name, Attrs}},
	    #{stream_state := wait_for_stream,
	      xmlns := XMLNS, lang := MyLang} = State) ->
    El = #xmlel{name = Name, attrs = Attrs},
    try xmpp:decode(El, XMLNS, []) of
	#stream_start{} = Pkt ->
	    noreply(process_stream(Pkt, State));
	_ ->
	    noreply(send_element(State, xmpp:serr_invalid_xml()))
    catch _:{xmpp_codec, Why} ->
	    Txt = xmpp:io_format_error(Why),
	    Lang = select_lang(MyLang, xmpp:get_lang(El)),
	    Err = xmpp:serr_invalid_xml(Txt, Lang),
	    noreply(send_element(State, Err))
    end;
handle_info({'$gen_event', {xmlstreamerror, Reason}}, #{lang := Lang}= State) ->
    State1 = send_header(State),
    case is_disconnected(State1) of
	true -> State1;
	false ->
	    Err = case Reason of
		      <<"XML stanza is too big">> ->
			  xmpp:serr_policy_violation(Reason, Lang);
		      _ ->
			  xmpp:serr_not_well_formed()
		  end,
	    noreply(send_element(State1, Err))
    end;
handle_info({'$gen_event', {xmlstreamelement, El}},
	    #{xmlns := NS, lang := MyLang, mod := Mod} = State) ->
    try xmpp:decode(El, NS, [ignore_els]) of
	Pkt ->
	    State1 = try Mod:handle_recv(El, Pkt, State)
		     catch _:undef -> State
		     end,
	    case is_disconnected(State1) of
		true -> State1;
		false -> noreply(process_element(Pkt, State1))
	    end
    catch _:{xmpp_codec, Why} ->
	    State1 = try Mod:handle_recv(El, undefined, State)
		     catch _:undef -> State
		     end,
	    case is_disconnected(State1) of
		true -> State1;
		false ->
		    Txt = xmpp:io_format_error(Why),
		    Lang = select_lang(MyLang, xmpp:get_lang(El)),
		    noreply(send_error(State1, El, xmpp:err_bad_request(Txt, Lang)))
	    end
    end;
handle_info({'$gen_all_state_event', {xmlstreamcdata, Data}},
	    #{mod := Mod} = State) ->
    noreply(try Mod:handle_cdata(Data, State)
	    catch _:undef -> State
	    end);
handle_info({'$gen_event', {xmlstreamend, _}}, State) ->
    noreply(process_stream_end({error, {stream, reset}}, State));
handle_info({'$gen_event', closed}, State) ->
    noreply(process_stream_close({error, {socket, closed}}, State));
handle_info(timeout, #{mod := Mod} = State) ->
    Disconnected = is_disconnected(State),
    noreply(try Mod:handle_timeout(State)
	    catch _:undef when not Disconnected ->
		    send_element(State, xmpp:serr_connection_timeout());
		  _:undef ->
		    stop(State)
	    end);
handle_info({'DOWN', MRef, _Type, _Object, _Info},
	    #{socket_monitor := MRef} = State) ->
    noreply(process_stream_close({error, {socket, closed}}, State));
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

-spec process_stream_close(stop_reason(), state()) -> state().
process_stream_close(_, #{stream_state := disconnected} = State) ->
    State;
process_stream_close(Reason, #{mod := Mod} = State) ->
    State1 = send_trailer(State),
    try Mod:handle_stream_close(Reason, State1)
    catch _:undef -> stop(State1)
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
    send_element(State, xmpp:serr_invalid_namespace());
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
		{1,0} -> State2#{stream_state => wait_for_features};
		_ -> process_stream_downgrade(StreamStart, State)
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
	    process_stream_end({error, {stream, Pkt}}, State);
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
	    %% TODO: improve xmpp.erl
	    Msg = #message{sub_els = Els},
	    case xmpp:get_subtag(Msg, #starttls{}) of
		false when TLSRequired and not Encrypted ->
		    Txt = <<"Use of STARTTLS required">>,
		    send_element(State1, xmpp:err_policy_violation(Txt, Lang));
		#starttls{} when not Encrypted ->
		    State2 = State1#{stream_state => wait_for_starttls_response},
		    send_element(State2, #starttls{});
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
				      #sasl_failure{reason = 'invalid-mechanism'},
				      State2)
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
	    Authzid = jid:to_string(jid:make(User, Server)),
	    send_element(State1, #sasl_auth{mechanism = Mech, text = Authzid});
	false ->
	    process_sasl_failure(
	      #sasl_failure{reason = 'invalid-mechanism'}, State)
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
	    process_stream_close({error, {tls, Why}}, State)
    end.

-spec process_stream_downgrade(stream_start(), state()) -> state().
process_stream_downgrade(StreamStart, #{mod := Mod} = State) ->
    try Mod:downgrade_stream(StreamStart, State)
    catch _:undef ->
	    send_element(State, xmpp:serr_unsupported_version())
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
		    process_stream_close({error, {pkix, Why}}, State)
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
    State1 = try Mod:handle_auth_success(<<"EXTERNAL">>, State)
	     catch _:undef -> State
	     end,
    case is_disconnected(State1) of
	true -> State1;
	false ->
	    SockMod:reset_stream(Socket),
	    State2 = State1#{stream_id => new_id(),
			     stream_restarted => true,
			     stream_state => wait_for_stream,
			     stream_authenticated => true},
	    send_header(State2)
    end.

-spec process_sasl_failure(sasl_failure(), state()) -> state().
process_sasl_failure(#sasl_failure{reason = Reason}, #{mod := Mod} = State) ->
    try Mod:handle_auth_failure(<<"EXTERNAL">>, Reason, State)
    catch _:undef -> process_stream_close({error, {auth, Reason}}, State)
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
    Header = xmpp:encode(
	       #stream_start{xmlns = NS,
			     lang = Lang,
			     stream_xmlns = ?NS_STREAM,
			     db_xmlns = NS_DB,
			     from = From,
			     to = jid:make(RemoteServer),
			     version = {1,0}}),
    case send_text(State, fxml:element_to_header(Header)) of
	ok -> State;
	{error, Why} -> process_stream_close({error, {socket, Why}}, State)
    end.

-spec send_element(state(), xmpp_element()) -> state().
send_element(#{xmlns := NS, mod := Mod} = State, Pkt) ->
    El = xmpp:encode(Pkt, NS),
    Data = fxml:element_to_binary(El),
    State1 = try Mod:handle_send(Pkt, El, Data, State)
	     catch _:undef -> State
	     end,
    case is_disconnected(State1) of
	true -> State1;
	false ->
	    case send_text(State1, Data) of
		_ when is_record(Pkt, stream_error) ->
		    process_stream_end({error, {stream, Pkt}}, State1);
		ok ->
		    State1;
		{error, Why} ->
		    process_stream_close({error, {socket, Why}}, State1)
	    end
    end.

-spec send_error(state(), xmpp_element(), stanza_error()) -> state().
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
		    send_element(State, ErrPkt)
	    end;
	false ->
	    State
    end.

-spec send_text(state(), binary()) -> ok | {error, inet:posix()}.
send_text(#{sockmod := SockMod, socket := Socket,
	    stream_state := StateName}, Data) when StateName /= disconnected ->
    SockMod:send(Socket, Data);
send_text(_, _) ->
    {error, einval}.

-spec send_trailer(state()) -> state().
send_trailer(State) ->
    send_text(State, <<"</stream:stream>">>),
    close_socket(State).

-spec close_socket(state()) -> state().
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
format_inet_error(Reason) ->
    case inet:format_error(Reason) of
	"unknown POSIX error" -> atom_to_list(Reason);
	Txt -> Txt
    end.

-spec format_stream_error(atom() | 'see-other-host'(), undefined | text()) -> string().
format_stream_error(Reason, Txt) ->
    Slogan = case Reason of
		 #'see-other-host'{} -> "see-other-host";
		 _ -> atom_to_list(Reason)
	     end,
    case Txt of
	undefined -> Slogan;
	#text{data = <<"">>} -> Slogan;
	#text{data = Data} ->
	    binary_to_list(Data) ++ " (" ++ Slogan ++ ")"
    end.

-spec format(io:format(), list()) -> binary().
format(Fmt, Args) ->
    iolist_to_binary(io_lib:format(Fmt, Args)).

%%%===================================================================
%%% Connection stuff
%%%===================================================================
-spec resolve(string(), state()) -> {ok, [host_port()]} | network_error().
resolve(Host, State) ->
    case srv_lookup(Host, State) of
	{error, _Reason} ->
	    DefaultPort = get_default_port(State),
	    a_lookup([{Host, DefaultPort}], State);
	{ok, HostPorts} ->
	    a_lookup(HostPorts, State)
    end.

-spec srv_lookup(string(), state()) -> {ok, [host_port()]} | network_error().
srv_lookup(Host, State) ->
    %% Only perform SRV lookups for FQDN names
    case string:chr(Host, $.) of
	0 ->
	    {error, nxdomain};
	_ ->
	    case inet_parse:address(Host) of
		{ok, _} ->
		    {error, nxdomain};
		{error, _} ->
		    Timeout = get_dns_timeout(State),
		    Retries = get_dns_retries(State),
		    srv_lookup(Host, Timeout, Retries)
	    end
    end.

-spec srv_lookup(string(), non_neg_integer(), integer()) ->
			{ok, [host_port()]} | network_error().
srv_lookup(_Host, _Timeout, Retries) when Retries < 1 ->
    {error, timeout};
srv_lookup(Host, Timeout, Retries) ->
    SRVName = "_xmpp-server._tcp." ++ Host,
    case inet_res:getbyname(SRVName, srv, Timeout) of
	{ok, HostEntry} ->
	    host_entry_to_host_ports(HostEntry);
	{error, _} ->
	    LegacySRVName = "_jabber._tcp." ++ Host,
	    case inet_res:getbyname(LegacySRVName, srv, Timeout) of
		{error, timeout} ->
		    srv_lookup(Host, Timeout, Retries - 1);
		{error, _} = Err ->
		    Err;
		{ok, HostEntry} ->
		    host_entry_to_host_ports(HostEntry)
	    end
    end.

-spec a_lookup([{inet:hostname(), inet:port_number()}], state()) ->
		      {ok, [ip_port()]} | network_error().
a_lookup(HostPorts, State) ->
    HostPortFamilies = [{Host, Port, Family}
			|| {Host, Port} <- HostPorts,
			   Family <- get_address_families(State)],
    a_lookup(HostPortFamilies, State, {error, nxdomain}).

-spec a_lookup([{inet:hostname(), inet:port_number(), inet:address_family()}],
	       state(), network_error()) -> {ok, [ip_port()]} | network_error().
a_lookup([{Host, Port, Family}|HostPortFamilies], State, _) ->
    Timeout = get_dns_timeout(State),
    Retries = get_dns_retries(State),
    case a_lookup(Host, Port, Family, Timeout, Retries) of
	{error, _} = Err ->
	    a_lookup(HostPortFamilies, State, Err);
	{ok, AddrPorts} ->
	    {ok, AddrPorts}
    end;
a_lookup([], _State, Err) ->
    Err.

-spec a_lookup(inet:hostname(), inet:port_number(), inet:address_family(),
	       non_neg_integer(), integer()) -> {ok, [ip_port()]} | network_error().
a_lookup(_Host, _Port, _Family, _Timeout, Retries) when Retries < 1 ->
    {error, timeout};
a_lookup(Host, Port, Family, Timeout, Retries) ->
    case inet:gethostbyname(Host, Family, Timeout) of
	{error, timeout} ->
	    a_lookup(Host, Port, Family, Timeout, Retries - 1);
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

-spec connect([ip_port()], module(), non_neg_integer(), network_error()) ->
		     {ok, term(), ip_port()} | network_error().
connect([{Addr, Port}|AddrPorts], SockMod, Timeout, _) ->
    Type = get_addr_type(Addr),
    case SockMod:connect(Addr, Port,
			 [binary, {packet, 0},
			  {send_timeout, ?TCP_SEND_TIMEOUT},
			  {send_timeout_close, true},
			  {active, false}, Type],
			 Timeout) of
	{ok, Socket} ->
	    {ok, Socket, {Addr, Port}};
	Err ->
	    connect(AddrPorts, SockMod, Timeout, Err)
    end;
connect([], _SockMod, _Timeout, Err) ->
    Err.

-spec get_addr_type(inet:ip_address()) -> inet:address_family().
get_addr_type({_, _, _, _}) -> inet;
get_addr_type({_, _, _, _, _, _, _, _}) -> inet6.

-spec get_dns_timeout(state()) -> non_neg_integer().
get_dns_timeout(#{mod := Mod} = State) ->
    timer:seconds(
      try Mod:dns_timeout(State)
      catch _:undef -> 10
      end).

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

-spec get_connect_timeout(state()) -> non_neg_integer().
get_connect_timeout(#{mod := Mod} = State) ->
    timer:seconds(
      try Mod:connect_timeout(State)
      catch _:undef -> 10
      end).
