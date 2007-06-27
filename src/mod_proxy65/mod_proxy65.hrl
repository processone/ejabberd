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
-define(SOCKS5_ERR_GENERAL_FAILURE, 1).
-define(SOCKS5_ERR_NOT_ALLOWED, 2).
-define(SOCKS5_ERR_NETWORK_UNREACHABLE, 3).
-define(SOCKS5_ERR_HOST_UNREACHABLE, 4).
-define(SOCKS5_ERR_CONNECTION_REFUSED, 5).
-define(SOCKS5_ERR_TTL_EXPIRED, 6).
-define(SOCKS5_ERR_COMMAND_NOT_SUPPORTED, 7).
-define(SOCKS5_ERR_ADDRESS_TYPE_NOT_SUPPORTED, 8).

%% RFC 1928 defined timeout.
-define(SOCKS5_REPLY_TIMEOUT, 10000).

-record(bytestream, {
	  sha1,           %% SHA1 key
	  target,         %% Target Pid
	  initiator,      %% Initiator Pid
	  active = false, %% Activity flag
	  jid_i,          %% Initiator's JID
	  jid_t,          %% Target's JID (for http file transfert)
	  file,           %% store status of file (for http file transfert)
	  myhost          %% proxy's jid
	 }).

-record(s5_request, {
	  rsv = 0,
	  cmd,
	  sha1
	 }).

% For http transfer
-define(NS_HTTP_BYTESTREAMS, "http://oneteam.im/bs-proxy").
-define(DEFAULT_HTTP_BASE_PATH, "/proxy").
-define(DEFAULT_HTTP_UPLOAD_PATH, "/upload").
-define(DEFAULT_STORE_PATH, "/tmp").
