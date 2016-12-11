%%%-------------------------------------------------------------------
%%% Created : 26 Nov 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
-module(xmpp_stream_in).
-behaviour(gen_server).

-protocol({rfc, 6120}).

%% API
-export([start/3, call/3, cast/2, reply/2, send/2, send_error/3,
	 get_transport/1, change_shaper/2]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
	 terminate/2, code_change/3]).

-include("xmpp.hrl").
-type state() :: map().
-type next_state() :: {noreply, state()} | {stop, term(), state()}.

-callback init(list()) -> {ok, state()} | {stop, term()} | ignore.
-callback handle_authenticated_packet(xmpp_element(), state()) -> next_state().

%%%===================================================================
%%% API
%%%===================================================================
start(Mod, Args, Opts) ->
    gen_server:start(?MODULE, [Mod|Args], Opts).

call(Ref, Msg, Timeout) ->
    gen_server:call(Ref, Msg, Timeout).

cast(Ref, Msg) ->
    gen_server:cast(Ref, Msg).

reply(Ref, Reply) ->
    gen_server:reply(Ref, Reply).

-spec send(state(), xmpp_element()) -> next_state().
send(State, Pkt) ->
    send_element(State, Pkt).

get_transport(#{sockmod := SockMod, socket := Socket}) ->
    SockMod:get_transport(Socket).

-spec change_shaper(state(), shaper:shaper()) -> ok.
change_shaper(#{sockmod := SockMod, socket := Socket}, Shaper) ->
    SockMod:change_shaper(Socket, Shaper).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Module, {SockMod, Socket}, Opts]) ->
    XMLSocket = case lists:keyfind(xml_socket, 1, Opts) of
		    {_, XS} -> XS;
		    false -> false
		end,
    TLSEnabled = proplists:get_bool(tls, Opts),
    SocketMonitor = SockMod:monitor(Socket),
    case peername(SockMod, Socket) of
	{ok, IP} ->
	    State = #{mod => Module,
		      socket => Socket,
		      sockmod => SockMod,
		      socket_monitor => SocketMonitor,
		      stream_id => new_id(),
		      stream_state => wait_for_stream,
		      stream_restarted => false,
		      stream_compressed => false,
		      stream_tlsed => TLSEnabled,
		      stream_version => {1,0},
		      stream_authenticated => false,
		      xml_socket => XMLSocket,
		      xmlns => ?NS_CLIENT,
		      lang => <<"">>,
		      user => <<"">>,
		      server => <<"">>,
		      resource => <<"">>,
		      ip => IP},
	    Module:init([State, Opts]);
	{error, Reason} ->
	    {stop, Reason}
    end.

handle_cast(Cast, #{mod := Mod} = State) ->
    Mod:handle_cast(Cast, State).

handle_call(Call, From, #{mod := Mod} = State) ->
    Mod:handle_call(Call, From, State).

handle_info({'$gen_event', {xmlstreamstart, Name, Attrs}},
	    #{stream_state := wait_for_stream} = State) ->
    try xmpp:decode(#xmlel{name = Name, attrs = Attrs}) of
	#stream_start{} = Pkt ->
	    case send_header(State, Pkt) of
		{noreply, State1} ->
		    process_stream(Pkt, State1);
		Err ->
		    Err
	    end;
	_ ->
	    case send_header(State) of
		{noreply, State1} ->
		    send_element(State1, xmpp:serr_invalid_xml());
		Err ->
		    Err
	    end
    catch _:{xmpp_codec, Why} ->
	    case send_header(State) of
		{noreply, State1} -> process_invalid_xml(Why, State1);
		Err -> Err
	    end
    end;
handle_info({'$gen_event', {xmlstreamend, _}}, #{mod := Mod} = State) ->
    try Mod:handle_stream_end(State)
    catch _:undef -> {stop, normal, State}
    end;
handle_info({'$gen_event', {xmlstreamerror, Reason}}, #{lang := Lang}= State) ->
    case send_header(State) of
	{noreply, State1} ->
	    Err = case Reason of
		      <<"XML stanza is too big">> ->
			  xmpp:serr_policy_violation(Reason, Lang);
		      _ ->
			  xmpp:serr_not_well_formed()
		  end,
	    send_element(State1, Err);
	Err ->
	    Err
    end;
handle_info({'$gen_event', {xmlstreamelement, El}},
	    #{xmlns := NS} = State) ->
    try xmpp:decode(El, NS, [ignore_els]) of
	Pkt ->
	    process_element(Pkt, State)
    catch _:{xmpp_codec, Why} ->
	    process_invalid_xml(Why, State)
    end;
handle_info({'$gen_all_state_event', {xmlstreamcdata, Data}},
	    #{mod := Mod} = State) ->
    try Mod:handle_cdata(Data, State)
    catch _:undef -> {noreply, State}
    end;
handle_info(closed, #{mod := Mod} = State) ->
    try Mod:handle_stream_close(State)
    catch _:undef -> {stop, normal, State}
    end;
handle_info({'DOWN', MRef, _Type, _Object, _Info},
	    #{socket_monitor := MRef, mod := Mod} = State) ->
    try Mod:handle_stream_close(State)
    catch _:undef -> {stop, normal, State}
    end;
handle_info(Info, #{mod := Mod} = State) ->
    Mod:handle_info(Info, State).

terminate(Reason, #{mod := Mod, socket := Socket,
		    sockmod := SockMod} = State) ->
    Mod:terminate(Reason, State),
    send_text(State, <<"</stream:stream>">>),
    SockMod:close(Socket).

code_change(OldVsn, #{mod := Mod} = State, Extra) ->
    Mod:code_change(OldVsn, State, Extra).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec new_id() -> binary().
new_id() ->
    randoms:get_string().

peername(SockMod, Socket) ->
    case SockMod of
	gen_tcp -> inet:peername(Socket);
	_ -> SockMod:peername(Socket)
    end.

process_invalid_xml(Reason, #{lang := Lang} = State) ->
    Txt = xmpp:io_format_error(Reason),
    send_element(State, xmpp:serr_invalid_xml(Txt, Lang)).

process_stream(#stream_start{xmlns = XML_NS,
			     stream_xmlns = STREAM_NS},
	       #{xmlns := NS} = State)
  when XML_NS /= NS; STREAM_NS /= ?NS_STREAM ->
    send_element(State, xmpp:serr_invalid_namespace());
process_stream(#stream_start{lang = Lang},
	       #{xmlns := ?NS_CLIENT, lang := DefaultLang} = State)
  when size(Lang) > 35 ->
    %% As stated in BCP47, 4.4.1:
    %% Protocols or specifications that specify limited buffer sizes for
    %% language tags MUST allow for language tags of at least 35 characters.
    %% Do not store long language tag to avoid possible DoS/flood attacks
    Txt = <<"Too long value of 'xml:lang' attribute">>,
    send_element(State, xmpp:serr_policy_violation(Txt, DefaultLang));
process_stream(#stream_start{to = undefined}, #{lang := Lang} = State) ->
    Txt = <<"Missing 'to' attribute">>,
    send_element(State, xmpp:serr_improper_addressing(Txt, Lang));
process_stream(#stream_start{from = undefined, version = {1,0}},
	       #{lang := Lang, xmlns := ?NS_SERVER,
		 stream_tlsed := true} = State) ->
    Txt = <<"Missing 'from' attribute">>,
    send_element(State, xmpp:serr_invalid_from(Txt, Lang));
process_stream(#stream_start{to = #jid{luser = U, lresource = R}},
	       #{lang := Lang} = State) when U /= <<"">>; R /= <<"">> ->
    Txt = <<"Improper 'to' attribute">>,
    send_element(State, xmpp:serr_improper_addressing(Txt, Lang));
process_stream(#stream_start{to = #jid{lserver = RemoteServer}},
	       #{xmlns := ?NS_COMPONENT, mod := Mod} = State) ->
    State1 = State#{remote_server => RemoteServer},
    case try Mod:handle_stream_start(State1)
	 catch _:undef -> {noreply, State1}
	 end of
	{noreply, State2} ->
	    {noreply, State2#{stream_state => wait_for_handshake}};
	Err ->
	    Err
    end;
process_stream(#stream_start{to = #jid{server = Server}, from = From},
	       #{stream_authenticated := Authenticated,
		 stream_restarted := StreamWasRestarted,
		 mod := Mod, xmlns := NS, resource := Resource,
		 stream_tlsed := TLSEnabled} = State) ->
    case if not StreamWasRestarted ->
		 State1 = State#{server => Server},
		 try Mod:handle_stream_start(State1)
		 catch _:undef -> {noreply, State1}
		 end;
	    true ->
		 {noreply, State}
	 end of
	{noreply, State2} ->
	    State3 = if NS == ?NS_SERVER andalso TLSEnabled ->
			     State2#{remote_server => From#jid.lserver};
			true ->
			     State2
		     end,
	    case send_features(State3) of
		{noreply, State4} ->
		    TLSRequired = is_starttls_required(State4),
		    NewStreamState =
			if not Authenticated and
			   (not TLSEnabled and TLSRequired) ->
				wait_for_starttls;
			   not Authenticated ->
				wait_for_sasl_request;
			   (NS == ?NS_CLIENT) and (Resource == <<"">>) ->
				wait_for_bind;
			   true ->
				session_established
			end,
		    {noreply, State4#{stream_state => NewStreamState}};
		Err ->
		    Err
	    end;
	Err ->
	    Err
    end.

process_element(Pkt, #{stream_state := StateName, lang := Lang} = State) ->
    case Pkt of
	#starttls{} when StateName == wait_for_starttls;
			 StateName == wait_for_sasl_request ->
	    process_starttls(State);
	#starttls{} ->
	    send_element(State, #starttls_failure{});
	#sasl_auth{} when StateName == wait_for_starttls ->
	    send_element(State, #sasl_failure{reason = 'encryption-required'});
	#sasl_auth{} when StateName == wait_for_sasl_request ->
	    process_sasl_request(Pkt, State);
	#sasl_auth{} ->
	    Txt = <<"SASL negotiation is not allowed in this state">>,
	    send_element(State, #sasl_failure{reason = 'not-authorized',
					      text = xmpp:mk_text(Txt, Lang)});
	#sasl_response{} when StateName == wait_for_starttls ->
	    send_element(State, #sasl_failure{reason = 'encryption-required'});
	#sasl_response{} when StateName == wait_for_sasl_response ->
	    process_sasl_response(Pkt, State);
	#sasl_response{} ->
	    Txt = <<"SASL negotiation is not allowed in this state">>,
	    send_element(State, #sasl_failure{reason = 'not-authorized',
					      text = xmpp:mk_text(Txt, Lang)});
	#sasl_abort{} when StateName == wait_for_sasl_response ->
	    process_sasl_abort(State);
	#sasl_abort{} ->
	    send_element(State, #sasl_failure{reason = 'aborted'});
	#sasl_success{} ->
	    {noreply, State};
	#compress{} when StateName == wait_for_sasl_response ->
	    send_element(State, #compress_failure{reason = 'setup-failed'});
	#compress{} ->
	    process_compress(Pkt, State);
	#handshake{} when StateName == wait_for_handshake ->
	    process_handshake(Pkt, State);
	#handshake{} ->
	    {noreply, State};
	_ when StateName == wait_for_sasl_request;
	       StateName == wait_for_handshake;
	       StateName == wait_for_sasl_response ->
	    process_unauthenticated_packet(Pkt, State);
	_ when StateName == wait_for_starttls ->
	    Txt = <<"Use of STARTTLS required">>,
	    Err = xmpp:err_policy_violation(Txt, Lang),
	    send_error(State, Pkt, Err);
	_ when StateName == wait_for_bind ->
	    process_bind(Pkt, State);
	_ when StateName == session_established ->
	    process_authenticated_packet(Pkt, State)
    end.

process_unauthenticated_packet(Pkt, #{mod := Mod} = State) ->
    NewPkt = set_lang(Pkt, State),
    try Mod:handle_unauthenticated_packet(NewPkt, State)
    catch _:undef ->
	    Err = xmpp:err_not_authorized(),
	    send_error(State, Pkt, Err)
    end.

process_authenticated_packet(Pkt, #{xmlns := NS, mod := Mod} = State) ->
    Pkt1 = set_lang(Pkt, State),
    case set_from_to(Pkt1, State) of
	{ok, #iq{type = set, sub_els = [_]} = Pkt2} when NS == ?NS_CLIENT ->
	    case xmpp:get_subtag(Pkt2, #xmpp_session{}) of
		#xmpp_session{} ->
		    send_element(State, xmpp:make_iq_result(Pkt2));
		_ ->
		    Mod:handle_authenticated_packet(Pkt2, State)
	    end;
	{ok, Pkt2} ->
	    Mod:handle_authenticated_packet(Pkt2, State);
	{error, Err} ->
	    send_element(State, Err)
    end.

process_bind(#iq{type = set, sub_els = [_]} = Pkt,
	     #{xmlns := ?NS_CLIENT, mod := Mod, lang := Lang} = State) ->
    case xmpp:get_subtag(Pkt, #bind{}) of
	#bind{resource = R} ->
	    case jid:resourceprep(R) of
		error ->
		    Txt = <<"Malformed resource">>,
		    Err = xmpp:err_bad_request(Txt, Lang),
		    send_error(State, Pkt, Err);
		_ ->
		    case Mod:bind(R, State) of
			{ok, #{user := U,
			       server := S,
			       resource := NewR} = State1} when NewR /= <<"">> ->
			    Reply = #bind{jid = jid:make(U, S, NewR)},
			    State2 = State1#{stream_state => session_established},
			    send_element(State2, xmpp:make_iq_result(Pkt, Reply));
			{error, #stanza_error{}, State1} = Err ->
			    send_error(State1, Pkt, Err)
		    end
	    end;
	_ ->
	    try Mod:handle_unbinded_packet(Pkt, State)
	    catch _:undef ->
		    Err = xmpp:err_not_authorized(),
		    send_error(State, Pkt, Err)
	    end
    end;
process_bind(Pkt, #{mod := Mod} = State) ->
    try Mod:handle_unbinded_packet(Pkt, State)
    catch _:undef ->
	    Err = xmpp:err_not_authorized(),
	    send_error(State, Pkt, Err)
    end.

process_handshake(#handshake{} = Pkt, #{mod := Mod} = State) ->
    Mod:handle_handshake(Pkt, State).

process_compress(#compress{}, #{stream_compressed := true} = State) ->
    send_element(State, #compress_failure{reason = 'setup-failed'});
process_compress(#compress{methods = HisMethods},
		 #{socket := Socket, sockmod := SockMod, mod := Mod} = State) ->
    MyMethods = try Mod:compress_methods(State)
		catch _:undef -> []
		end,
    CommonMethods = lists_intersection(MyMethods, HisMethods),
    case lists:member(<<"zlib">>, CommonMethods) of
	true ->
	    BCompressed = fxml:element_to_binary(xmpp:encode(#compressed{})),
	    ZlibSocket = SockMod:compress(Socket, BCompressed),
	    State1 = State#{socket => ZlibSocket,
			    stream_id => new_id(),
			    stream_restarted => true,
			    stream_state => wait_for_stream,
			    stream_compressed => true},
	    {noreply, State1};
	false ->
	    send_element(State, #compress_failure{reason = 'unsupported-method'})
    end.

process_starttls(#{socket := Socket,
		   sockmod := SockMod, mod := Mod} = State) ->
    TLSOpts = try Mod:tls_options(State)
	      catch _:undef -> []
	      end,
    case SockMod:starttls(Socket, TLSOpts) of
	{ok, TLSSocket} ->
	    case send_element(State, #starttls_proceed{}) of
		{noreply, State1} ->
		    {noreply, State1#{socket => TLSSocket,
				      stream_id => new_id(),
				      stream_restarted => true,
				      stream_state => wait_for_stream,
				      stream_tlsed => true}};
		Err ->
		    Err
	    end;
	{error, _Reason} ->
	    send_element(State, #starttls_failure{})
    end.

process_sasl_request(#sasl_auth{mechanism = <<"EXTERNAL">>},
		     #{stream_tlsed := false} = State) ->
    process_sasl_failure('encryption-required', <<"">>, State);
process_sasl_request(#sasl_auth{mechanism = Mech, text = ClientIn},
		     #{mod := Mod} = State) ->
    SASLState = Mod:init_sasl(State),
    SASLResult = cyrsasl:server_start(SASLState, Mech, ClientIn),
    process_sasl_result(SASLResult, State).

process_sasl_response(#sasl_response{text = ClientIn},
		      #{sasl_state := SASLState} = State) ->
    SASLResult = cyrsasl:server_step(SASLState, ClientIn),
    process_sasl_result(SASLResult, State).

process_sasl_result({ok, Props}, State) ->
    process_sasl_success(Props, <<"">>, State);
process_sasl_result({ok, Props, ServerOut}, State) ->
    process_sasl_success(Props, ServerOut, State);
process_sasl_result({continue, ServerOut, NewSASLState}, State) ->
    process_sasl_continue(ServerOut, NewSASLState, State);
process_sasl_result({error, Reason, User}, State) ->
    process_sasl_failure(Reason, User, State);
process_sasl_result({error, Reason}, State) ->
    process_sasl_failure(Reason, <<"">>, State).

process_sasl_success(Props, ServerOut,
		     #{socket := Socket, sockmod := SockMod,
		       mod := Mod, sasl_state := SASLState} = State) ->
    Mech = cyrsasl:get_mech(SASLState),
    User = identity(Props),
    AuthModule = proplists:get_value(auth_module, Props),
    case try Mod:handle_auth_success(User, Mech, AuthModule, State)
	 catch _:undef -> {noreply, State}
	 end of
	{noreply, State1} ->
	    SockMod:reset_stream(Socket),
	    case send_element(State1, #sasl_success{text = ServerOut}) of
		{noreply, State2} ->
		    State3 = maps:remove(sasl_state, State2),
		    {noreply, State3#{stream_id => new_id(),
				      stream_authenticated => true,
				      stream_restarted => true,
				      stream_state => wait_for_stream,
				      user => User}};
		Err ->
		    Err
	    end;
	Err ->
	    Err
    end.

process_sasl_continue(ServerOut, NewSASLState, State) ->
    send_element(State, #sasl_challenge{text = ServerOut}),
    {noreply, State#{sasl_state => NewSASLState,
		     stream_state => wait_for_sasl_response}}.

process_sasl_failure(Reason, User,
		     #{mod := Mod, sasl_state := SASLState} = State) ->
    Mech = cyrsasl:get_mech(SASLState),
    case try Mod:handle_auth_failure(User, Mech, Reason, State)
	 catch _:undef -> {noreply, State}
	 end of
	{noreply, State1} ->
	    State2 = maps:remove(sasl_state, State1),
	    State3 = State2#{stream_state => wait_for_sasl_request},
	    send_element(State3, #sasl_failure{reason = Reason});
	Err ->
	    Err
    end.

process_sasl_abort(State) ->
    process_sasl_failure('aborted', <<"">>, State).

send_features(#{stream_version := {1,0},
		stream_tlsed := TLSEnabled} = State) ->
    TLSRequired = is_starttls_required(State),
    Features = if TLSRequired and not TLSEnabled ->
		       get_tls_feature(State);
		  true ->
		       get_sasl_feature(State) ++ get_compress_feature(State)
			   ++ get_tls_feature(State) ++ get_bind_feature(State)
			   ++ get_session_feature(State) ++ get_other_features(State)
	       end,
    send_element(State, #stream_features{sub_els = Features});
send_features(State) ->
    %% clients from stone age
    {noreply, State}.

get_sasl_feature(#{stream_authenticated := false,
		   mod := Mod,
		   stream_tlsed := TLSEnabled} = State) ->
    TLSRequired = is_starttls_required(State),
    if TLSEnabled or not TLSRequired ->
	    try Mod:sasl_mechanisms(State) of
		[] -> [];
		List -> [#sasl_mechanisms{list = List}]
	    catch _:undef ->
		    []
	    end;
       true ->
	    []
    end;
get_sasl_feature(_) ->
    [].

get_compress_feature(#{stream_compressed := false, mod := Mod} = State) ->
    try Mod:compress_methods(State) of
	[] -> [];
	Ms -> [#compression{methods = Ms}]
    catch _:undef ->
	    []
    end;
get_compress_feature(_) ->
    [].

get_tls_feature(#{stream_authenticated := false,
		  stream_tlsed := false} = State) ->
    TLSRequired = is_starttls_required(State),
    [#starttls{required = TLSRequired}];
get_tls_feature(_) ->
    [].

get_bind_feature(#{stream_authenticated := true, resource := <<"">>}) ->
    [#bind{}];
get_bind_feature(_) ->
    [].

get_session_feature(#{stream_authenticated := true, resource := <<"">>}) ->
    [#xmpp_session{optional = true}];
get_session_feature(_) ->
    [].

get_other_features(#{stream_authenticated := Auth, mod := Mod} = State) ->
    try
	if Auth -> Mod:authenticated_stream_features(State);
	   true -> Mod:unauthenticated_stream_features(State)
	end
    catch _:undef ->
	    []
    end.

is_starttls_required(#{mod := Mod} = State) ->
    try Mod:tls_required(State)
    catch _:undef -> false
    end.

set_from_to(Pkt, _State) when not ?is_stanza(Pkt) ->
    {ok, Pkt};
set_from_to(Pkt, #{user := U, server := S, resource := R,
		     xmlns := ?NS_CLIENT}) ->
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
	    {error, xmpp:serr_invalid_from()}
    end;
set_from_to(Pkt, #{lang := Lang}) ->
    From = xmpp:get_from(Pkt),
    To = xmpp:get_to(Pkt),
    if From == undefined ->
	    Txt = <<"Missing 'from' attribute">>,
	    {error, xmpp:serr_invalid_from(Txt, Lang)};
       To == undefined ->
	    Txt = <<"Missing 'to' attribute">>,
	    {error, xmpp:serr_improper_addressing(Txt, Lang)};
       true ->
	    {ok, Pkt}
    end.

send_header(State) ->
    send_header(State, #stream_start{}).

send_header(#{stream_state := wait_for_stream,
	      stream_id := StreamID,
	      stream_version := MyVersion,
	      lang := MyLang,
	      xmlns := NS,
	      server := DefaultServer} = State,
	    #stream_start{to = To, lang = HisLang, version = HisVersion}) ->
    Lang = choose_lang(MyLang, HisLang),
    From = case To of
	       #jid{} -> To;
	       undefined -> jid:make(DefaultServer)
	   end,
    Version = case HisVersion of
		  undefined -> MyVersion;
		  _ -> HisVersion
	      end,
    Header = xmpp:encode(#stream_start{version = Version,
				       lang = Lang,
				       xmlns = NS,
				       stream_xmlns = ?NS_STREAM,
				       id = StreamID,
				       from = From}),
    State1 = State#{lang => Lang},
    case send_text(State1, fxml:element_to_header(Header)) of
	ok -> {noreply, State1};
	{error, _} -> {stop, normal, State1}
    end;
send_header(State, _) ->
    {noreply, State}.

send_element(#{xmlns := NS, mod := Mod} = State, Pkt) ->
    El = xmpp:encode(Pkt, NS),
    Data = fxml:element_to_binary(El),
    case send_text(State, Data) of
	ok when is_record(Pkt, stream_error) ->
	    {stop, normal, State};
	ok when is_record(Pkt, starttls_failure) ->
	    {stop, normal, State};
	Res ->
	    try Mod:handle_send(Res, Pkt, El, Data, State)
	    catch _:undef when Res == ok ->
		    {noreply, State};
		  _:undef ->
		    {stop, normal, State}
	    end
    end.

send_error(State, Pkt, Err) when ?is_stanza(Pkt) ->
    case xmpp:get_type(Pkt) of
	result -> {noreply, State};
	error -> {noreply, State};
	_ ->
	    ErrPkt = xmpp:make_error(Pkt, Err),
	    send_element(State, ErrPkt)
    end;
send_error(State, _, _) ->
    {noreply, State}.

send_text(#{socket := Sock, sockmod := SockMod}, Data) ->
    SockMod:send(Sock, Data).

choose_lang(Lang, <<"">>) -> Lang;
choose_lang(_, Lang) -> Lang.

set_lang(Pkt, #{lang := MyLang, xmlns := ?NS_CLIENT}) when ?is_stanza(Pkt) ->
    HisLang = xmpp:get_lang(Pkt),
    Lang = choose_lang(MyLang, HisLang),
    xmpp:set_lang(Pkt, Lang);
set_lang(Pkt, _) ->
    Pkt.

lists_intersection(L1, L2) ->
    lists:filter(
      fun(E) ->
	      lists:member(E, L2)
      end, L1).

identity(Props) ->
    case proplists:get_value(authzid, Props, <<>>) of
	<<>> -> proplists:get_value(username, Props, <<>>);
	AuthzId -> AuthzId
    end.
