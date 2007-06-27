%%%----------------------------------------------------------------------
%%% File    : mod_proxy65.hrl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : RFC 1928 constants.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

%% Version
-define(VERSION_5, 5).

%% Authentication methods
-define(AUTH_ANONYMOUS, 0).
-define(AUTH_GSSAPI, 1).
-define(AUTH_PLAIN, 2).
-define(AUTH_NO_METHODS, 16#FF).

%% Address Type
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

-record(s5_request, {
	  rsv = 0,
	  cmd,
	  sha1
	 }).
