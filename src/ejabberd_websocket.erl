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
%%% ejabberd, Copyright (C) 2002-2021   ProcessOne
%%%----------------------------------------------------------------------

-module(ejabberd_websocket).
-protocol({rfc, 6455}).

-author('ecestari@process-one.net').

-export([socket_handoff/5]).

-include("logger.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-include("ejabberd_http.hrl").

-define(CT_XML, {<<"Content-Type">>, <<"text/xml; charset=utf-8">>}).
-define(CT_PLAIN, {<<"Content-Type">>, <<"text/plain">>}).

-define(AC_ALLOW_ORIGIN, {<<"Access-Control-Allow-Origin">>, <<"*">>}).
-define(AC_ALLOW_METHODS, {<<"Access-Control-Allow-Methods">>, <<"GET, OPTIONS">>}).
-define(AC_ALLOW_HEADERS, {<<"Access-Control-Allow-Headers">>, <<"Content-Type">>}).
-define(AC_MAX_AGE, {<<"Access-Control-Max-Age">>, <<"86400">>}).

-define(OPTIONS_HEADER, [?CT_PLAIN, ?AC_ALLOW_ORIGIN, ?AC_ALLOW_METHODS,
                          ?AC_ALLOW_HEADERS, ?AC_MAX_AGE]).
-define(HEADER, [?CT_XML, ?AC_ALLOW_ORIGIN, ?AC_ALLOW_HEADERS]).

is_valid_websocket_upgrade(_Path, Headers) ->
    HeadersToValidate = [{'Upgrade', <<"websocket">>},
                         {'Connection', ignore},
                         {'Host', ignore},
                         {<<"Sec-Websocket-Key">>, ignore},
                         {<<"Sec-Websocket-Version">>, <<"13">>}],
    Res = lists:all(
        fun({Tag, Val}) ->
            case lists:keyfind(Tag, 1, Headers) of
                false ->
                    false;
                {_, _} when Val == ignore ->
                    true;
                {_, HVal} ->
                    str:to_lower(HVal) == Val
            end
        end, HeadersToValidate),

    case {Res, lists:keyfind(<<"Origin">>, 1, Headers), get_origin()} of
        {false, _, _} ->
            false;
        {true, _, []} ->
            true;
        {true, {_, HVal}, Origins} ->
            HValLow = str:to_lower(HVal),
            case lists:any(fun(V) -> V == HValLow end, Origins) of
                true ->
                    true;
                _ ->
                    invalid_origin
            end;
        {true, false, _} ->
            true
    end.

socket_handoff(LocalPath, #request{method = 'GET', ip = IP, q = Q, path = Path,
                                   headers = Headers, host = Host, port = Port,
				   socket = Socket, sockmod = SockMod,
				   data = Buf, opts = HOpts},
               _Opts, HandlerModule, InfoMsgFun) ->
    case is_valid_websocket_upgrade(LocalPath, Headers) of
        true ->
            WS = #ws{socket = Socket,
                     sockmod = SockMod,
                     ip = IP,
                     q = Q,
                     host = Host,
                     port = Port,
                     path = Path,
                     headers = Headers,
                     local_path = LocalPath,
                     buf = Buf,
                     http_opts = HOpts},

            connect(WS, HandlerModule);
        false ->
            {200, ?HEADER, InfoMsgFun()};
        invalid_origin ->
            {403, ?HEADER, #xmlel{name = <<"h1">>,
                                  children = [{xmlcdata, <<"403 Bad Request - Invalid origin">>}]}}
    end;
socket_handoff(_, #request{method = 'OPTIONS'}, _, _, _) ->
    {200, ?OPTIONS_HEADER, []};
socket_handoff(_, #request{method = 'HEAD'}, _, _, _) ->
    {200, ?HEADER, []};
socket_handoff(_, _, _, _, _) ->
    {400, ?HEADER, #xmlel{name = <<"h1">>,
                          children = [{xmlcdata, <<"400 Bad Request">>}]}}.

connect(#ws{socket = Socket, sockmod = SockMod} = Ws, WsLoop) ->
    {NewWs, HandshakeResponse} = handshake(Ws),
    SockMod:send(Socket, HandshakeResponse),

    ?DEBUG("Sent handshake response : ~p",
	   [HandshakeResponse]),
    Ws0 = {Ws, self()},
    {ok, WsHandleLoopPid} = WsLoop:start_link(Ws0),
    erlang:monitor(process, WsHandleLoopPid),

    case NewWs#ws.buf of
        <<>> ->
            ok;
        Data ->
            self() ! {raw, Socket, Data}
    end,

    % set opts
    case SockMod of
      gen_tcp ->
	  inet:setopts(Socket, [{packet, 0}, {active, true}]);
      _ ->
	  SockMod:setopts(Socket, [{packet, 0}, {active, true}])
    end,
    ws_loop(none, Socket, WsHandleLoopPid, SockMod, none).

handshake(#ws{headers = Headers} = State) ->
    {_, Key} = lists:keyfind(<<"Sec-Websocket-Key">>, 1,
			     Headers),
    SubProtocolHeader = case find_subprotocol(Headers) of
                            false ->
                                [];
                            V ->
                                [<<"Sec-Websocket-Protocol:">>, V, <<"\r\n">>]
                        end,
    Hash = base64:encode(
             crypto:hash(sha, <<Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)),
    {State, [<<"HTTP/1.1 101 Switching Protocols\r\n">>,
             <<"Upgrade: websocket\r\n">>,
             <<"Connection: Upgrade\r\n">>,
             SubProtocolHeader,
             <<"Sec-WebSocket-Accept: ">>, Hash, <<"\r\n\r\n">>]}.

find_subprotocol(Headers) ->
    case lists:keysearch(<<"Sec-Websocket-Protocol">>, 1, Headers) of
        false ->
            case lists:keysearch(<<"Websocket-Protocol">>, 1, Headers) of
                false ->
                    false;
                {value, {_, Protocol2}} ->
                    Protocol2
            end;
        {value, {_, Protocol}} ->
            Protocol
    end.


ws_loop(FrameInfo, Socket, WsHandleLoopPid, SocketMode, Shaper) ->
    receive
        {DataType, _Socket, Data} when DataType =:= tcp orelse DataType =:= raw ->
            case handle_data(DataType, FrameInfo, Data, Socket, WsHandleLoopPid, SocketMode, Shaper) of
                {error, Error} ->
                    ?DEBUG("TLS decode error ~p", [Error]),
                    websocket_close(Socket, WsHandleLoopPid, SocketMode, 1002); % protocol error
                {NewFrameInfo, ToSend, NewShaper} ->
                    lists:foreach(fun(Pkt) -> SocketMode:send(Socket, Pkt)
                                  end, ToSend),
                    ws_loop(NewFrameInfo, Socket, WsHandleLoopPid, SocketMode, NewShaper)
            end;
        {new_shaper, NewShaper} ->
            NewShaper = case NewShaper of
                none when Shaper /= none ->
                    activate(Socket, SocketMode, true), none;
                _ ->
                    NewShaper
            end,
            ws_loop(FrameInfo, Socket, WsHandleLoopPid, SocketMode, NewShaper);
        {tcp_closed, _Socket} ->
            ?DEBUG("TCP connection was closed, exit", []),
            websocket_close(Socket, WsHandleLoopPid, SocketMode, 0);
	{tcp_error, Socket, Reason} ->
	    ?DEBUG("TCP connection error: ~ts", [inet:format_error(Reason)]),
	    websocket_close(Socket, WsHandleLoopPid, SocketMode, 0);
        {'DOWN', Ref, process, WsHandleLoopPid, Reason} ->
            Code = case Reason of
                       normal ->
                           1000; % normal close
                       _ ->
                           ?ERROR_MSG("Linked websocket controlling loop crashed "
                                      "with reason: ~p",
                                      [Reason]),
                           1011 % internal error
                   end,
            erlang:demonitor(Ref),
            websocket_close(Socket, WsHandleLoopPid, SocketMode, Code);
        {text_with_reply, Data, Sender} ->
            SocketMode:send(Socket, encode_frame(Data, 1)),
            Sender ! {text_reply, self()},
            ws_loop(FrameInfo, Socket, WsHandleLoopPid,
                    SocketMode, Shaper);
        {data_with_reply, Data, Sender} ->
            SocketMode:send(Socket, encode_frame(Data, 2)),
            Sender ! {data_reply, self()},
            ws_loop(FrameInfo, Socket, WsHandleLoopPid,
                    SocketMode, Shaper);
        {text, Data} ->
            SocketMode:send(Socket, encode_frame(Data, 1)),
            ws_loop(FrameInfo, Socket, WsHandleLoopPid,
                    SocketMode, Shaper);
	{data, Data} ->
	    SocketMode:send(Socket, encode_frame(Data, 2)),
            ws_loop(FrameInfo, Socket, WsHandleLoopPid,
                    SocketMode, Shaper);
        {ping, Data} ->
            SocketMode:send(Socket, encode_frame(Data, 9)),
            ws_loop(FrameInfo, Socket, WsHandleLoopPid,
                    SocketMode, Shaper);
        shutdown ->
	  ?DEBUG("Shutdown request received, closing websocket "
		 "with pid ~p",
		 [self()]),
            websocket_close(Socket, WsHandleLoopPid, SocketMode, 1001); % going away
        _Ignored ->
            ?WARNING_MSG("Received unexpected message, ignoring: ~p",
                         [_Ignored]),
            ws_loop(FrameInfo, Socket, WsHandleLoopPid,
                    SocketMode, Shaper)
    end.

encode_frame(Data, Opcode) ->
    case byte_size(Data) of
      S1 when S1 < 126 ->
	  <<1:1, 0:3, Opcode:4, 0:1, S1:7, Data/binary>>;
      S2 when S2 < 65536 ->
	  <<1:1, 0:3, Opcode:4, 0:1, 126:7, S2:16, Data/binary>>;
      S3 ->
	  <<1:1, 0:3, Opcode:4, 0:1, 127:7, S3:64, Data/binary>>
    end.

-record(frame_info,
	{mask = none, offset = 0, left, final_frame = true,
	 opcode, unprocessed = <<>>, unmasked = <<>>,
	 unmasked_msg = <<>>}).

decode_header(<<Final:1, _:3, Opcode:4, 0:1,
		       Len:7, Data/binary>>)
    when Len < 126 ->
    {Len, Final, Opcode, none, Data};
decode_header(<<Final:1, _:3, Opcode:4, 0:1,
		       126:7, Len:16/integer, Data/binary>>) ->
    {Len, Final, Opcode, none, Data};
decode_header(<<Final:1, _:3, Opcode:4, 0:1,
		       127:7, Len:64/integer, Data/binary>>) ->
    {Len, Final, Opcode, none, Data};
decode_header(<<Final:1, _:3, Opcode:4, 1:1,
		       Len:7, Mask:4/binary, Data/binary>>)
    when Len < 126 ->
    {Len, Final, Opcode, Mask, Data};
decode_header(<<Final:1, _:3, Opcode:4, 1:1,
		       126:7, Len:16/integer, Mask:4/binary, Data/binary>>) ->
    {Len, Final, Opcode, Mask, Data};
decode_header(<<Final:1, _:3, Opcode:4, 1:1,
		       127:7, Len:64/integer, Mask:4/binary, Data/binary>>) ->
    {Len, Final, Opcode, Mask, Data};
decode_header(_) -> none.

unmask_int(Offset, _, <<>>, Acc) ->
    {Acc, Offset};
unmask_int(0, <<M:32>> = Mask,
		  <<N:32, Rest/binary>>, Acc) ->
    unmask_int(0, Mask, Rest,
		      <<Acc/binary, (M bxor N):32>>);
unmask_int(0, <<M:8, _/binary>> = Mask,
		  <<N:8, Rest/binary>>, Acc) ->
    unmask_int(1, Mask, Rest,
		      <<Acc/binary, (M bxor N):8>>);
unmask_int(1, <<_:8, M:8, _/binary>> = Mask,
		  <<N:8, Rest/binary>>, Acc) ->
    unmask_int(2, Mask, Rest,
		      <<Acc/binary, (M bxor N):8>>);
unmask_int(2, <<_:16, M:8, _/binary>> = Mask,
		  <<N:8, Rest/binary>>, Acc) ->
    unmask_int(3, Mask, Rest,
		      <<Acc/binary, (M bxor N):8>>);
unmask_int(3, <<_:24, M:8>> = Mask,
		  <<N:8, Rest/binary>>, Acc) ->
    unmask_int(0, Mask, Rest,
		      <<Acc/binary, (M bxor N):8>>).

unmask(#frame_info{mask = none} = State, Data) ->
    {State, Data};
unmask(#frame_info{mask = Mask, offset = Offset} = State, Data) ->
    {Unmasked, NewOffset} = unmask_int(Offset, Mask,
					      Data, <<>>),
    {State#frame_info{offset = NewOffset}, Unmasked}.

process_frame(none, Data) ->
    process_frame(#frame_info{}, Data);
process_frame(#frame_info{left = Left} = FrameInfo, <<>>) when Left > 0 ->
    {FrameInfo, [], []};
process_frame(#frame_info{unprocessed = none,
			     unmasked = UnmaskedPre, left = Left} =
		   State,
	       Data)
    when byte_size(Data) < Left ->
    {State2, Unmasked} = unmask(State, Data),
    {State2#frame_info{left = Left - byte_size(Data),
			 unmasked = [UnmaskedPre, Unmasked]},
     [], []};
process_frame(#frame_info{unprocessed = none,
                          unmasked = UnmaskedPre, opcode = Opcode,
                          final_frame = Final, left = Left,
                          unmasked_msg = UnmaskedMsg} =
                  FrameInfo,
              Data) ->
    <<ToProcess:(Left)/binary, Unprocessed/binary>> = Data,
    {_, Unmasked} = unmask(FrameInfo, ToProcess),
    case Final of
        true ->
            {FrameInfo3, Recv, Send} = process_frame(#frame_info{},
                                                 Unprocessed),
            case Opcode of
                X when X < 3 ->
                    {FrameInfo3,
                     [iolist_to_binary([UnmaskedMsg, UnmaskedPre, Unmasked])
                      | Recv],
                     Send};
                9 -> % Ping
                    Frame = encode_frame(Unmasked, 10),
                    {FrameInfo3#frame_info{unmasked_msg = UnmaskedMsg}, [ping | Recv],
                     [Frame | Send]};
                10 -> % Pong
                    {FrameInfo3, [pong | Recv], Send};
                8 -> % Close
                    CloseCode = case Unmasked of
                                    <<Code:16/integer-big, Message/binary>> ->
                                        ?DEBUG("WebSocket close op: ~p ~ts",
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

                    Frame = encode_frame(<<CloseCode:16/integer-big>>, 8),
                    {FrameInfo3#frame_info{unmasked_msg=UnmaskedMsg}, Recv,
                     [Frame | Send]};
                _ ->
                    {FrameInfo3#frame_info{unmasked_msg = UnmaskedMsg}, Recv,
                     Send}
            end;
        _ ->
            process_frame(#frame_info{unmasked_msg =
                                          [UnmaskedMsg, UnmaskedPre,
                                           Unmasked]},
                          Unprocessed)
    end;
process_frame(#frame_info{unprocessed = <<>>} =
                  FrameInfo,
              Data) ->
    case decode_header(Data) of
        none ->
            {FrameInfo#frame_info{unprocessed = Data}, [], []};
        {Len, Final, Opcode, Mask, Rest} ->
            process_frame(FrameInfo#frame_info{mask = Mask,
                                               final_frame = Final == 1,
                                               left = Len, opcode = Opcode,
                                               unprocessed = none},
                          Rest)
    end;
process_frame(#frame_info{unprocessed =
                              UnprocessedPre} =
                  FrameInfo,
              Data) ->
    process_frame(FrameInfo#frame_info{unprocessed = <<>>},
                  <<UnprocessedPre/binary, Data/binary>>).

handle_data(tcp, FrameInfo, Data, Socket, WsHandleLoopPid, fast_tls, Shaper) ->
    case fast_tls:recv_data(Socket, Data) of
        {ok, NewData} ->
            handle_data_int(FrameInfo, NewData, Socket, WsHandleLoopPid, fast_tls, Shaper);
        {error, Error} ->
            {error, Error}
    end;
handle_data(_, FrameInfo, Data, Socket, WsHandleLoopPid, SockMod, Shaper) ->
    handle_data_int(FrameInfo, Data, Socket, WsHandleLoopPid, SockMod, Shaper).

handle_data_int(FrameInfo, Data, Socket, WsHandleLoopPid, SocketMode, Shaper) ->
    {NewFrameInfo, Recv, Send} = process_frame(FrameInfo, Data),
    lists:foreach(fun (El) ->
                          case El of
                              pong ->
                                  WsHandleLoopPid ! pong;
                              ping ->
                                  WsHandleLoopPid ! ping;
                              _ ->
                                  WsHandleLoopPid ! {received, El}
                          end
		  end,
		  Recv),
    {NewFrameInfo, Send, handle_shaping(Data, Socket, SocketMode, Shaper)}.

websocket_close(Socket, WsHandleLoopPid,
                SocketMode, CloseCode) when CloseCode > 0 ->
    Frame = encode_frame(<<CloseCode:16/integer-big>>, 8),
    SocketMode:send(Socket, Frame),
    websocket_close(Socket, WsHandleLoopPid, SocketMode, 0);
websocket_close(Socket, WsHandleLoopPid, SocketMode, _CloseCode) ->
    WsHandleLoopPid ! closed,
    SocketMode:close(Socket).

get_origin() ->
    ejabberd_option:websocket_origin().

handle_shaping(_Data, _Socket, _SocketMode, none) ->
    none;
handle_shaping(Data, Socket, SocketMode, Shaper) ->
    {NewShaper, Pause} = ejabberd_shaper:update(Shaper, byte_size(Data)),
    if Pause > 0 ->
        activate_after(Socket, self(), Pause);
        true -> activate(Socket, SocketMode, once)
    end,
    NewShaper.

activate(Socket, SockMod, ActiveState) ->
    case SockMod of
        gen_tcp -> inet:setopts(Socket, [{active, ActiveState}]);
        _ -> SockMod:setopts(Socket, [{active, ActiveState}])
    end.

activate_after(Socket, Pid, Pause) ->
    if Pause > 0 ->
        erlang:send_after(Pause, Pid, {tcp, Socket, <<>>});
        true ->
            Pid ! {tcp, Socket, <<>>}
    end,
    ok.
