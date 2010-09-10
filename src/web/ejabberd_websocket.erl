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
%%% ejabberd, Copyright (C) 2002-2010   ProcessOne
%%%----------------------------------------------------------------------

-module (ejabberd_websocket).
-author('ecestari@process-one.net').
-compile(export_all).

-include ("ejabberd_http.hrl").
-include ("ejabberd.hrl").

check(Headers)->
  ?DEBUG("testing for a websocket request path: ~p headers: ~p", [_Path, Headers]),
	% set supported websocket protocols, order does matter
	VsnSupported = [{'draft-hixie', 76}, {'draft-hixie', 68}],	
	% checks
	check_websockets(VsnSupported, Headers).
	
check_websockets([], _Headers) -> false;
check_websockets([Vsn|T], Headers) ->
	case check_websocket(Vsn, Headers) of
		false -> check_websockets(T, Headers);
		{true, Vsn} -> {true, Vsn}
	end.

% Function: {true, Vsn} | false
% Description: Check if the incoming request is a websocket request.
check_websocket({'draft-hixie', 76} = Vsn, Headers) ->
	?DEBUG("testing for websocket protocol ~p", [Vsn]),
	% set required headers
	RequiredHeaders = [
		{'Upgrade', "WebSocket"}, {'Connection', "Upgrade"}, {'Host', ignore}, {'Origin', ignore},
		{'Sec-WebSocket-Key1', ignore}, {'Sec-WebSocket-Key2', ignore}
	],
	% check for headers existance
	case check_headers(Headers, RequiredHeaders) of
		true ->
			% return
			{true, Vsn};
		_RemainingHeaders ->
			?LOG_DEBUG("not protocol ~p, remaining headers: ~p", [Vsn, _RemainingHeaders]),
			false
	end;
check_websocket({'draft-hixie', 68} = Vsn, Headers) ->
	?DEBUG("testing for websocket protocol ~p", [Vsn]),
	% set required headers
	RequiredHeaders = [
		{'Upgrade', "WebSocket"}, {'Connection', "Upgrade"}, {'Host', ignore}, {'Origin', ignore}
	],
	% check for headers existance
	case check_headers(Headers, RequiredHeaders) of
		true -> {true, Vsn};
		_RemainingHeaders ->
			?LOG_DEBUG("not protocol ~p, remaining headers: ~p", [Vsn, _RemainingHeaders]),
			false
	end;
check_websocket(_Vsn, _Headers) -> false. % not implemented

% Function: true | [{RequiredTag, RequiredVal}, ..]
% Description: Check if headers correspond to headers requirements.
check_headers(Headers, RequiredHeaders) ->
	F = fun({Tag, Val}) ->
		% see if the required Tag is in the Headers
		case lists:keysearch(Tag, 1, Headers) of
			false -> true; % header not found, keep in list
			HVal ->
				case Val of
					ignore -> false; % ignore value -> ok, remove from list
					HVal -> false;	 % expected val -> ok, remove from list
					_ -> true		 % val is different, keep in list
				end		
		end
	end,
	case lists:filter(F, RequiredHeaders) of
		[] -> true;
		MissingHeaders -> MissingHeaders
	end.

% Function: List
% Description: Builds the server handshake response.
handshake({'draft-hixie', 76}, #req{socket = Sock, socket_mode = SocketMode}, Headers, {Path, Origin, Host}) ->
	% build data
	Key1 = lists:keysearch('Sec-WebSocket-Key1',1, Headers),
	Key2 = lists:keysearch('Sec-WebSocket-Key2',1, Headers),
	% handshake needs body of the request, still need to read it [TODO: default recv timeout hard set, will be exported when WS protocol is final]
	misultin_socket:setopts(Sock, [{packet, raw}, {active, false}], SocketMode),
	Body = case misultin_socket:recv(Sock, 8, 30*1000, SocketMode) of
		{ok, Bin} -> Bin;
		{error, timeout} ->
			?WARNING_MSG("timeout in reading websocket body", []),
			<<>>; 
		_Other ->
			?ERROR_MSG("tcp error treating data: ~p", [_Other]),
			<<>>
	end,
	?DEBUG("got content in body of websocket request: ~p", [Body]),	
	% prepare handhsake response
	["HTTP/1.1 101 WebSocket Protocol Handshake\r\n",
		"Upgrade: WebSocket\r\n",
		"Connection: Upgrade\r\n",
		"Sec-WebSocket-Origin: ", Origin, "\r\n",
		"Sec-WebSocket-Location: ws://", lists:concat([Host, Path]), "\r\n\r\n",
		build_challenge({'draft-hixie', 76}, {Key1, Key2, Body})
	];
handshake({'draft-hixie', 68}, _Req, _Headers, {Path, Origin, Host}) ->
	% prepare handhsake response
	["HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
		"Upgrade: WebSocket\r\n",
		"Connection: Upgrade\r\n",
		"WebSocket-Origin: ", Origin , "\r\n",
		"WebSocket-Location: ws://", lists:concat([Host, Path]), "\r\n\r\n"
	].

% Function: List
% Description: Builds the challenge for a handshake response.
% Code portions from Sergio Veiga <http://sergioveiga.com/index.php/2010/06/17/websocket-handshake-76-in-erlang/>
build_challenge({'draft-hixie', 76}, {Key1, Key2, Key3}) ->
	Ikey1 = [D || D <- Key1, $0 =< D, D =< $9],
	Ikey2 = [D || D <- Key2, $0 =< D, D =< $9],
	Blank1 = length([D || D <- Key1, D =:= 32]),
	Blank2 = length([D || D <- Key2, D =:= 32]),
	Part1 = list_to_integer(Ikey1) div Blank1,
	Part2 = list_to_integer(Ikey2) div Blank2,
	Ckey = <<Part1:4/big-unsigned-integer-unit:8, Part2:4/big-unsigned-integer-unit:8, Key3/binary>>,
	erlang:md5(Ckey).