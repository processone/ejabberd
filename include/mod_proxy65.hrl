%%%----------------------------------------------------------------------
%%% RFC 1928 constants.
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

%% Version
-define(VERSION_5, 5).

%% Authentication methods
-define(AUTH_ANONYMOUS, 0).

-define(AUTH_GSSAPI, 1).

-define(AUTH_PLAIN, 2).

%% Address Type
-define(AUTH_NO_METHODS, 255).

-define(ATYP_IPV4, 1).

-define(ATYP_DOMAINNAME, 3).

-define(ATYP_IPV6, 4).

%% Commands
-define(CMD_CONNECT, 1).

-define(CMD_BIND, 2).

-define(CMD_UDP, 3).

%% RFC 1928 replies
-define(SUCCESS, 0).

-define(ERR_GENERAL_FAILURE, 1).

-define(ERR_NOT_ALLOWED, 2).

-define(ERR_NETWORK_UNREACHABLE, 3).

-define(ERR_HOST_UNREACHABLE, 4).

-define(ERR_CONNECTION_REFUSED, 5).

-define(ERR_TTL_EXPIRED, 6).

-define(ERR_COMMAND_NOT_SUPPORTED, 7).

-define(ERR_ADDRESS_TYPE_NOT_SUPPORTED, 8).

%% RFC 1928 defined timeout.
-define(SOCKS5_REPLY_TIMEOUT, 10000).

-record(s5_request, {rsv = 0 :: integer(),
                     cmd = connect :: connect | udp,
                     sha1 = <<"">> :: binary()}).
