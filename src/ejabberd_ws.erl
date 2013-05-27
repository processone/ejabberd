%%%----------------------------------------------------------------------
%%% File    : ejabberd_websocket.erl
%%% Author  : Eric Cestari <ecestari@process-one.net>
%%% Purpose : Websocket support
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
-module(ejabberd_ws).

-vsn("0.6.1").

% API
-export([raw/1, get/2, send/2]).

% includes
-include("ejabberd_http.hrl").

% ============================ \/ API ======================================================================

raw({Ws, _SocketPid}) -> Ws.

get({Ws, _SocketPid}, socket) -> Ws#ws.socket;
get({Ws, _SocketPid}, socket_mode) -> Ws#ws.sockmod;
get({Ws, _SocketPid}, ip) -> Ws#ws.ip;
get({Ws, _SocketPid}, vsn) -> Ws#ws.vsn;
get({Ws, _SocketPid}, origin) -> Ws#ws.origin;
get({Ws, _SocketPid}, host) -> Ws#ws.host;
get({Ws, _SocketPid}, path) -> Ws#ws.path;
get({Ws, _SocketPid}, headers) -> Ws#ws.headers.

send({_Ws, SocketPid}, Data) -> SocketPid ! {send, Data}.

% ============================ /\ API ======================================================================

% ============================ \/ INTERNAL FUNCTIONS =======================================================

% ============================ /\ INTERNAL FUNCTIONS =======================================================

