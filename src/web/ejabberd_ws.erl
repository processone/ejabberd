%%%----------------------------------------------------------------------
%%% File    : ejabberd_websocket.erl
%%% Author  : Eric Cestari <ecestari@process-one.net>
%%% Purpose : XMPP Websocket support
%%% Created : 09-10-2010 by Eric Cestari <ecestari@process-one.net>
%%% Slightly adapted from :
% ==========================================================================================================
% MISULTIN - Websocket Request
%
% >-|-|-(°>
% 
% Copyright (C) 2010, Roberto Ostinelli <roberto@ostinelli.net>.
% All rights reserved.
%
% BSD License
% 
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%	 following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%	 the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%	 products derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
% ==========================================================================================================
-module(ejabberd_ws, [Ws, SocketPid]).
-vsn("0.6.1").

% API
-export([raw/0, get/1, send/1]).

% includes
-include("ejabberd_http.hrl").


% ============================ \/ API ======================================================================

% Description: Returns raw websocket content.
raw() ->
	Ws.

% Description: Get websocket info.
get(socket) ->
	Ws#ws.socket;
get(socket_mode) ->
	Ws#ws.sockmod;
get(peer_addr) ->
	Ws#ws.peer_addr;
get(peer_port) ->
	Ws#ws.peer_port;
get(peer_cert) ->
	Ws#ws.peer_cert;
get(vsn) ->
	Ws#ws.vsn;
get(origin) ->
	Ws#ws.origin;
get(host) ->
	Ws#ws.host;
get(path) ->
	Ws#ws.path;
get(headers) ->
	Ws#ws.headers.

% send data
send(Data) ->
	SocketPid ! {send, Data}.
		
% ============================ /\ API ======================================================================



% ============================ \/ INTERNAL FUNCTIONS =======================================================

% ============================ /\ INTERNAL FUNCTIONS =======================================================
