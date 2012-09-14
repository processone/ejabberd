%%%----------------------------------------------------------------------
%%% File    : ejabberd_websocket.erl
%%% Author  : Eric Cestari <ecestari@process-one.net>
%%% Purpose : XMPP Websocket support
%%% Created : 09-10-2010 by Eric Cestari <ecestari@process-one.net>
%%%
%%% Some code lifted from MISULTIN - WebSocket misultin_websocket.erl - >-|-|-(°>
%%% (http://github.com/ostinelli/misultin/blob/master/src/misultin_websocket.erl)
%%% Copyright (C) 2010, Roberto Ostinelli <roberto@ostinelli.net>, Joe Armstrong.
%%% All rights reserved.
%%%
%%% Code portions from Joe Armstrong have been originally taken under MIT license at the address:
%%% <http://armstrongonsoftware.blogspot.com/2009/12/comet-is-dead-long-live-websockets.html>
%%%
%%% BSD License
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted provided
%%% that the following conditions are met:
%%%
%%%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%%%	 following disclaimer.
%%%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%%%	 the following disclaimer in the documentation and/or other materials provided with the distribution.
%%%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%%%	 products derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
%%% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%%% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
%%% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ==========================================================================================================
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
%%%----------------------------------------------------------------------

-module(ejabberd_websocket).

-author('ecestari@process-one.net').

-export([connect/2, check/2, is_acceptable/1]).

-include("ejabberd.hrl").

-include("jlib.hrl").

-include("ejabberd_http.hrl").

check(_Path, Headers) ->
    VsnSupported = [{'draft-hybi', 8}, {'draft-hybi', 13},
		    {'draft-hixie', 0}, {'draft-hixie', 68}],
    check_websockets(VsnSupported, Headers).

is_acceptable(#ws{origin = Origin, protocol = Protocol,
		  headers = Headers, acceptable_origins = Origins,
		  auth_module = undefined}) ->
    ClientProtocol =
	lists:keyfind(<<"Sec-Websocket-Protocol">>, 1, Headers),
    case {(Origins == []) or lists:member(Origin, Origins),
	  ClientProtocol, Protocol}
	of
      {false, _, _} ->
	  ?INFO_MSG("client does not come from authorized "
		    "origin",
		    []),
	  false;
      {_, false, _} -> true;
      {_, {_, P}, P} -> true;
      _ = E ->
	  ?INFO_MSG("Wrong protocol requested : ~p", [E]), false
    end;
is_acceptable(#ws{local_path = LocalPath,
		  origin = Origin, ip = IP, q = Q, protocol = Protocol,
		  headers = Headers, auth_module = Module}) ->
    Module:is_acceptable(LocalPath, Q, Origin, Protocol, IP,
			 Headers).

connect(#ws{vsn = Vsn, socket = Socket, q = Q,
	    origin = Origin, host = Host, port = Port,
	    sockmod = SockMod, path = Path, headers = Headers,
	    ws_autoexit = WsAutoExit} =
	    Ws,
	WsLoop) ->
    HandshakeServer = handshake(Vsn, Socket, SockMod,
				Headers, {Path, Q, Origin, Host, Port}),
    SockMod:send(Socket, HandshakeServer),
    ?DEBUG("Sent handshake response : ~p",
	   [HandshakeServer]),
    Ws0 = ejabberd_ws:new(Ws#ws{origin = Origin,
				host = Host},
			  self()),
    {ok, WsHandleLoopPid} = WsLoop:start_link(Ws0),
    erlang:monitor(process, WsHandleLoopPid),
    case SockMod of
      gen_tcp ->
	  inet:setopts(Socket, [{packet, 0}, {active, true}]);
      _ ->
	  SockMod:setopts(Socket, [{packet, 0}, {active, true}])
    end,
    ws_loop(Vsn, none, Socket, WsHandleLoopPid, SockMod,
	    WsAutoExit).

check_websockets([], _Headers) -> false;
check_websockets([Vsn | T], Headers) ->
    case check_websocket(Vsn, Headers) of
      false -> check_websockets(T, Headers);
      Value -> Value
    end.

check_websocket({'draft-hixie', 0} = Vsn, Headers) ->
    RequiredHeaders = [{'Upgrade', <<"WebSocket">>},
		       {'Connection', <<"Upgrade">>}, {'Host', ignore},
		       {<<"Origin">>, ignore},
		       {<<"Sec-Websocket-Key1">>, ignore},
		       {<<"Sec-Websocket-Key2">>, ignore}],
    case check_headers(Headers, RequiredHeaders) of
      true -> {true, Vsn};
      _RemainingHeaders ->
	  %?DEBUG("not protocol ~p, remaining headers: ~p", [Vsn, _RemainingHeaders]),
	  false
    end;
check_websocket({'draft-hixie', 68} = Vsn, Headers) ->
    RequiredHeaders = [{'Upgrade', <<"WebSocket">>},
		       {'Connection', <<"Upgrade">>}, {'Host', ignore},
		       {<<"Origin">>, ignore}],
    case check_headers(Headers, RequiredHeaders) of
      true -> {true, Vsn};
      _RemainingHeaders ->
	  %?DEBUG("not protocol ~p, remaining headers: ~p", [Vsn, _RemainingHeaders]),
	  false
    end;
check_websocket({'draft-hybi', 8} = Vsn, Headers) ->
    RequiredHeaders = [{'Upgrade', <<"websocket">>},
		       {'Connection', ignore}, {'Host', ignore},
		       {<<"Sec-Websocket-Key">>, ignore},
		       {<<"Sec-Websocket-Version">>, <<"8">>}],
    case check_headers(Headers, RequiredHeaders) of
      true -> {true, Vsn};
      _RemainingHeaders ->
	  %%?INFO_MSG("not protocol ~p, remaining headers: ~p", [Vsn, RemainingHeaders]),
	  false
    end;
check_websocket({'draft-hybi', 13} = Vsn, Headers) ->
    RequiredHeaders = [{'Upgrade', <<"websocket">>},
		       {'Connection', ignore}, {'Host', ignore},
		       {<<"Sec-Websocket-Key">>, ignore},
		       {<<"Sec-Websocket-Version">>, <<"13">>}],
    case check_headers(Headers, RequiredHeaders) of
      true -> {true, Vsn};
      _RemainingHeaders ->
	  %%?INFO_MSG("not protocol ~p, remaining headers: ~p", [Vsn, RemainingHeaders]),
	  false
    end.

check_headers(Headers, RequiredHeaders) ->
    F = fun ({Tag, Val}) ->
		case lists:keyfind(Tag, 1, Headers) of
		  false -> true; % header not found, keep in list
		  {_, HVal} ->
		      case Val of
			ignore -> false; % ignore value -> ok, remove from list
			HVal -> false;   % expected val -> ok, remove from list
			_ ->
			    true                % val is different, keep in list
		      end
		end
	end,
    case lists:filter(F, RequiredHeaders) of
      [] -> true;
      MissingHeaders -> MissingHeaders
    end.

handshake({'draft-hixie', 0}, Sock, SocketMod, Headers,
	  {Path, Q, Origin, Host, Port}) ->
    {_, Key1} = lists:keyfind(<<"Sec-Websocket-Key1">>, 1,
			      Headers),
    {_, Key2} = lists:keyfind(<<"Sec-Websocket-Key2">>, 1,
			      Headers),
    HostPort = case lists:keyfind('Host', 1, Headers) of
		 {_, Value} -> Value;
		 _ ->
		     str:join([Host,
			       jlib:integer_to_binary(Port)],
			      <<":">>)
	       end,
    case SocketMod of
      gen_tcp ->
	  inet:setopts(Sock, [{packet, raw}, {active, false}]);
      _ ->
	  SocketMod:setopts(Sock,
			    [{packet, raw}, {active, false}])
    end,
    Body = case SocketMod:recv(Sock, 8, 30 * 1000) of
	     {ok, Bin} -> Bin;
	     {error, timeout} ->
		 ?WARNING_MSG("timeout in reading websocket body", []),
		 <<>>;
	     _Other ->
		 ?ERROR_MSG("tcp error treating data: ~p", [_Other]),
		 <<>>
	   end,
    QParams = lists:map(fun ({nokey, <<>>}) -> none;
			    ({K, V}) -> <<K/binary, "=", V/binary>>
			end,
			Q),
    QString = case QParams of
		[none] -> <<"">>;
		QParams -> <<"?", (str:join(QParams, <<"&">>))/binary>>
	      end,
    [<<"HTTP/1.1 101 WebSocket Protocol Handshake\r\n">>,
     <<"Upgrade: WebSocket\r\n">>,
     <<"Connection: Upgrade\r\n">>,
     <<"Sec-WebSocket-Origin: ">>, Origin, <<"\r\n">>,
     <<"Sec-WebSocket-Location: ws://">>, HostPort, <<"/">>,
     str:join(Path, <<"/">>), QString, <<"\r\n\r\n">>,
     build_challenge({'draft-hixie', 0},
		     {Key1, Key2, Body})];
handshake({'draft-hixie', 68}, _Sock, _SocketMod,
	  Headers, {Path, _Q, Origin, Host, Port}) ->
    HostPort = case lists:keyfind('Host', 1, Headers) of
		 {_, Value} -> Value;
		 _ ->
		     str:join([Host,
			       iolist_to_binary(integer_to_list(Port))],
			      <<":">>)
	       end,
    [<<"HTTP/1.1 101 Web Socket Protocol Handshake\r\n">>,
     <<"Upgrade: WebSocket\r\n">>,
     <<"Connection: Upgrade\r\n">>, <<"WebSocket-Origin: ">>,
     Origin, <<"\r\n">>, <<"WebSocket-Location: ws://">>,
     HostPort, <<"/">>, str:join(Path, <<"/">>),
     <<"\r\n\r\n">>];
handshake({'draft-hybi', _}, _Sock, _SocketMod, Headers,
	  {_Path, _Q, _Origin, _Host, _Port}) ->
    {_, Key} = lists:keyfind(<<"Sec-Websocket-Key">>, 1,
			     Headers),
    Hash = jlib:encode_base64(
             sha:sha1(<<Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)),
    [<<"HTTP/1.1 101 Switching Protocols\r\n">>,
     <<"Upgrade: websocket\r\n">>,
     <<"Connection: Upgrade\r\n">>,
     <<"Sec-WebSocket-Accept: ">>, Hash, <<"\r\n\r\n">>].

build_challenge({'draft-hixie', 0},
		{Key1, Key2, Key3}) ->
    Ikey1 = << <<D>> || <<D>> <= Key1, $0 =< D, D =< $9>>,
    Ikey2 = << <<D>> || <<D>> <= Key2, $0 =< D, D =< $9>>,
    Blank1 = byte_size(<< <<D>> || <<D>> <= Key1, D =:= 32>>),
    Blank2 = byte_size(<< <<D>> || <<D>> <= Key2, D =:= 32>>),
    Part1 = jlib:binary_to_integer(Ikey1) div Blank1,
    Part2 = jlib:binary_to_integer(Ikey2) div Blank2,
    Ckey = <<Part1:4/big-unsigned-integer-unit:8,
	     Part2:4/big-unsigned-integer-unit:8, Key3/binary>>,
    erlang:md5(Ckey).

ws_loop(Vsn, HandlerState, Socket, WsHandleLoopPid,
	SocketMode, WsAutoExit) ->
    receive
      {tcp, Socket, Data} ->
	  {NewHandlerState, ToSend} = handle_data(Vsn,
						  HandlerState, Data, Socket,
						  WsHandleLoopPid, SocketMode,
						  WsAutoExit),
	  lists:foreach(fun (Pkt) -> SocketMode:send(Socket, Pkt)
			end,
			ToSend),
	  ws_loop(Vsn, NewHandlerState, Socket, WsHandleLoopPid,
		  SocketMode, WsAutoExit);
      {tcp_closed, Socket} ->
	  ?DEBUG("tcp connection was closed, exit", []),
	  websocket_close(Socket, WsHandleLoopPid, SocketMode,
			  WsAutoExit);
      {'DOWN', Ref, process, WsHandleLoopPid, Reason} ->
	  case Reason of
	    normal ->
		%?DEBUG("linked websocket controlling loop stopped.", []);
		ok;
	    _ ->
		?ERROR_MSG("linked websocket controlling loop crashed "
			   "with reason: ~p",
			   [Reason])
	  end,
	  erlang:demonitor(Ref),
	  websocket_close(Socket, WsHandleLoopPid, SocketMode,
			  WsAutoExit);
      {send, Data} ->
	  SocketMode:send(Socket, encode_frame(Vsn, Data)),
	  ws_loop(Vsn, HandlerState, Socket, WsHandleLoopPid,
		  SocketMode, WsAutoExit);
      shutdown ->
	  ?DEBUG("shutdown request received, closing websocket "
		 "with pid ~p",
		 [self()]),
	  websocket_close(Socket, WsHandleLoopPid, SocketMode,
			  WsAutoExit);
      _Ignored ->
	  ?WARNING_MSG("received unexpected message, ignoring: ~p",
		       [_Ignored]),
	  ws_loop(Vsn, HandlerState, Socket, WsHandleLoopPid,
		  SocketMode, WsAutoExit)
    end.

encode_frame({'draft-hybi', _}, Data, Opcode) ->
    case byte_size(Data) of
      S1 when S1 < 126 ->
	  <<1:1, 0:3, Opcode:4, 0:1, S1:7, Data/binary>>;
      S2 when S2 < 65536 ->
	  <<1:1, 0:3, Opcode:4, 0:1, 126:7, S2:16, Data/binary>>;
      S3 ->
	  <<1:1, 0:3, Opcode:4, 0:1, 127:7, S3:64, Data/binary>>
    end.

encode_frame({'draft-hybi', _} = Vsn, Data) ->
    encode_frame(Vsn, Data, 1);
encode_frame(_, Data) -> <<0, Data/binary, 255>>.

process_hixie_68(none, Data) ->
    process_hixie_68({false, <<>>}, Data);
process_hixie_68({false, <<>>}, <<0, T/binary>>) ->
    process_hixie_68({true, <<>>}, T);
process_hixie_68(L, <<>>) -> {L, [], []};
process_hixie_68({_, L}, <<255, T/binary>>) ->
    {L2, Recv, Send} = process_hixie_68({false, <<>>}, T),
    {L2, [L | Recv], Send};
process_hixie_68({true, L}, <<H/utf8, T/binary>>) ->
    process_hixie_68({true, <<L/binary, H>>}, T).

-record(hybi_8_state,
	{mask = none, offset = 0, left, final_frame = true,
	 opcode, unprocessed = <<>>, unmasked = <<>>,
	 unmasked_msg = <<>>}).

decode_hybi_8_header(<<Final:1, _:3, Opcode:4, 0:1,
		       Len:7, Data/binary>>)
    when Len < 126 ->
    {Len, Final, Opcode, none, Data};
decode_hybi_8_header(<<Final:1, _:3, Opcode:4, 0:1,
		       126:7, Len:16/integer, Data/binary>>) ->
    {Len, Final, Opcode, none, Data};
decode_hybi_8_header(<<Final:1, _:3, Opcode:4, 0:1,
		       127:7, Len:64/integer, Data/binary>>) ->
    {Len, Final, Opcode, none, Data};
decode_hybi_8_header(<<Final:1, _:3, Opcode:4, 1:1,
		       Len:7, Mask:4/binary, Data/binary>>)
    when Len < 126 ->
    {Len, Final, Opcode, Mask, Data};
decode_hybi_8_header(<<Final:1, _:3, Opcode:4, 1:1,
		       126:7, Len:16/integer, Mask:4/binary, Data/binary>>) ->
    {Len, Final, Opcode, Mask, Data};
decode_hybi_8_header(<<Final:1, _:3, Opcode:4, 1:1,
		       127:7, Len:64/integer, Mask:4/binary, Data/binary>>) ->
    {Len, Final, Opcode, Mask, Data};
decode_hybi_8_header(_) -> none.

unmask_hybi_8_int(Offset, _, <<>>, Acc) ->
    {Acc, Offset};
unmask_hybi_8_int(0, <<M:32>> = Mask,
		  <<N:32, Rest/binary>>, Acc) ->
    unmask_hybi_8_int(0, Mask, Rest,
		      <<Acc/binary, (M bxor N):32>>);
unmask_hybi_8_int(0, <<M:8, _/binary>> = Mask,
		  <<N:8, Rest/binary>>, Acc) ->
    unmask_hybi_8_int(1, Mask, Rest,
		      <<Acc/binary, (M bxor N):8>>);
unmask_hybi_8_int(1, <<_:8, M:8, _/binary>> = Mask,
		  <<N:8, Rest/binary>>, Acc) ->
    unmask_hybi_8_int(2, Mask, Rest,
		      <<Acc/binary, (M bxor N):8>>);
unmask_hybi_8_int(2, <<_:16, M:8, _/binary>> = Mask,
		  <<N:8, Rest/binary>>, Acc) ->
    unmask_hybi_8_int(3, Mask, Rest,
		      <<Acc/binary, (M bxor N):8>>);
unmask_hybi_8_int(3, <<_:24, M:8>> = Mask,
		  <<N:8, Rest/binary>>, Acc) ->
    unmask_hybi_8_int(0, Mask, Rest,
		      <<Acc/binary, (M bxor N):8>>).

unmask_hybi_8(#hybi_8_state{mask = none} = State,
	      Data) ->
    {State, Data};
unmask_hybi_8(#hybi_8_state{mask = Mask,
			    offset = Offset} =
		  State,
	      Data) ->
    {Unmasked, NewOffset} = unmask_hybi_8_int(Offset, Mask,
					      Data, <<>>),
    {State#hybi_8_state{offset = NewOffset}, Unmasked}.

process_hybi_8(none, Data) ->
    process_hybi_8(#hybi_8_state{}, Data);
process_hybi_8(State, <<>>) -> {State, [], []};
process_hybi_8(#hybi_8_state{unprocessed = none,
			     unmasked = UnmaskedPre, left = Left} =
		   State,
	       Data)
    when byte_size(Data) < Left ->
    {State2, Unmasked} = unmask_hybi_8(State, Data),
    {State2#hybi_8_state{left = Left - byte_size(Data),
			 unmasked = [UnmaskedPre, Unmasked]},
     [], []};
process_hybi_8(#hybi_8_state{unprocessed = none,
			     unmasked = UnmaskedPre, opcode = Opcode,
			     final_frame = Final, left = Left,
			     unmasked_msg = UnmaskedMsg} =
		   State,
	       Data) ->
    <<ToProcess:(Left)/binary, Unprocessed/binary>> = Data,
    {_State, Unmasked} = unmask_hybi_8(State, ToProcess),
    case Final of
      true ->
	  {State3, Recv, Send} = process_hybi_8(#hybi_8_state{},
						Unprocessed),
	  case Opcode of
	    X when X < 3 ->
		{State3,
		 [iolist_to_binary([UnmaskedMsg, UnmaskedPre, Unmasked])
		  | Recv],
		 Send};
	    9 -> % Ping
		Frame = encode_frame({'draft-hybi', 8}, Unprocessed,
				     10),
		{State3#hybi_8_state{unmasked_msg = UnmaskedMsg}, Recv,
		 [Frame | Send]};
            8 -> % Close
                CloseCode = case Unmasked of
                                <<Code:16/integer-big, Message/binary>> ->
                                    ?DEBUG("WebSocket close op: ~p ~s",
                                           [Code, Message]),
                                    Code;
                                <<Code:16/integer-big>> ->
                                    ?DEBUG("WebSocket close op: ~p", [Code]),
                                    Code;
                                _ ->
                                    ?DEBUG("WebSocket close op unknown: ~p",
                                           [Unmasked]),
                                    1000
                            end,

                Frame = encode_frame({'draft-hybi', 8},
                                     <<CloseCode:16/integer-big>>, 8),
                {State3#hybi_8_state{unmasked_msg=UnmaskedMsg}, Recv,
                 [Frame | Send]};
	    _ ->
		{State3#hybi_8_state{unmasked_msg = UnmaskedMsg}, Recv,
		 Send}
	  end;
      _ ->
	  process_hybi_8(#hybi_8_state{unmasked_msg =
					   [UnmaskedMsg, UnmaskedPre,
					    Unmasked]},
			 Unprocessed)
    end;
process_hybi_8(#hybi_8_state{unprocessed = <<>>} =
		   State,
	       Data) ->
    case decode_hybi_8_header(Data) of
      none ->
	  {State#hybi_8_state{unprocessed = Data}, [], []};
      {Len, Final, Opcode, Mask, Rest} ->
	  process_hybi_8(State#hybi_8_state{mask = Mask,
					    final_frame = Final == 1,
					    left = Len, opcode = Opcode,
					    unprocessed = none},
			 Rest)
    end;
process_hybi_8(#hybi_8_state{unprocessed =
				 UnprocessedPre} =
		   State,
	       Data) ->
    process_hybi_8(State#hybi_8_state{unprocessed = <<>>},
		   <<UnprocessedPre/binary, Data/binary>>).

handle_data({'draft-hybi', _}, State, Data, _Socket,
	    WsHandleLoopPid, _SocketMode, _WsAutoExit) ->
    {NewState, Recv, Send} = process_hybi_8(State, Data),
    lists:foreach(fun (El) ->
			  WsHandleLoopPid ! {browser, El}
		  end,
		  Recv),
    {NewState, Send};
handle_data(_Vsn, State, Data, _Socket, WsHandleLoopPid,
	    _SocketMode, _WsAutoExit) ->
    {NewState, Recv, Send} = process_hixie_68(State, Data),
    lists:foreach(fun (El) ->
			  WsHandleLoopPid ! {browser, El}
		  end,
		  Recv),
    {NewState, Send}.

websocket_close(Socket, WsHandleLoopPid, SocketMode,
		WsAutoExit) ->
    case WsAutoExit of
      true ->
	  % kill custom handling loop process
	  exit(WsHandleLoopPid, kill);
      false -> WsHandleLoopPid ! closed
    end,
    SocketMode:close(Socket).
