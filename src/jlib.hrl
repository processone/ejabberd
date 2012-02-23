%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-define(NS_DISCO_ITEMS,  "http://jabber.org/protocol/disco#items").
-define(NS_DISCO_INFO,   "http://jabber.org/protocol/disco#info").
-define(NS_VCARD,        "vcard-temp").
-define(NS_VCARD_UPDATE, "vcard-temp:x:update").
-define(NS_AUTH,         "jabber:iq:auth").
-define(NS_AUTH_ERROR,   "jabber:iq:auth:error").
-define(NS_REGISTER,     "jabber:iq:register").
-define(NS_SEARCH,       "jabber:iq:search").
-define(NS_ROSTER,       "jabber:iq:roster").
-define(NS_ROSTER_VER,   "urn:xmpp:features:rosterver").
-define(NS_PRIVACY,      "jabber:iq:privacy").
-define(NS_BLOCKING,     "urn:xmpp:blocking").
-define(NS_PRIVATE,      "jabber:iq:private").
-define(NS_VERSION,      "jabber:iq:version").
-define(NS_TIME90,       "jabber:iq:time"). % TODO: Remove once XEP-0090 is Obsolete
-define(NS_TIME,         "urn:xmpp:time").
-define(NS_LAST,         "jabber:iq:last").
-define(NS_XDATA,        "jabber:x:data").
-define(NS_IQDATA,       "jabber:iq:data").
-define(NS_DELAY91,      "jabber:x:delay"). % TODO: Remove once XEP-0091 is Obsolete
-define(NS_DELAY,        "urn:xmpp:delay").
-define(NS_EXPIRE,       "jabber:x:expire").
-define(NS_EVENT,        "jabber:x:event").
-define(NS_CHATSTATES,   "http://jabber.org/protocol/chatstates").
-define(NS_XCONFERENCE,  "jabber:x:conference").
-define(NS_STATS,        "http://jabber.org/protocol/stats").
-define(NS_MUC,          "http://jabber.org/protocol/muc").
-define(NS_MUC_USER,     "http://jabber.org/protocol/muc#user").
-define(NS_MUC_ADMIN,    "http://jabber.org/protocol/muc#admin").
-define(NS_MUC_OWNER,    "http://jabber.org/protocol/muc#owner").
-define(NS_MUC_UNIQUE,   "http://jabber.org/protocol/muc#unique").
-define(NS_PUBSUB,       "http://jabber.org/protocol/pubsub").
-define(NS_PUBSUB_EVENT, "http://jabber.org/protocol/pubsub#event").
-define(NS_PUBSUB_OWNER, "http://jabber.org/protocol/pubsub#owner").
-define(NS_PUBSUB_NMI,   "http://jabber.org/protocol/pubsub#node-meta-info").
-define(NS_PUBSUB_ERRORS,"http://jabber.org/protocol/pubsub#errors").
-define(NS_PUBSUB_NODE_CONFIG, "http://jabber.org/protocol/pubsub#node_config").
-define(NS_PUBSUB_SUB_OPTIONS, "http://jabber.org/protocol/pubsub#subscribe_options").
-define(NS_PUBSUB_SUB_AUTH, "http://jabber.org/protocol/pubsub#subscribe_authorization").
-define(NS_PUBSUB_GET_PENDING, "http://jabber.org/protocol/pubsub#get-pending").
-define(NS_COMMANDS,     "http://jabber.org/protocol/commands").
-define(NS_BYTESTREAMS,  "http://jabber.org/protocol/bytestreams").
-define(NS_ADMIN,        "http://jabber.org/protocol/admin").
-define(NS_SERVERINFO,   "http://jabber.org/network/serverinfo").

-define(NS_RSM,          "http://jabber.org/protocol/rsm").
-define(NS_EJABBERD_CONFIG, "ejabberd:config").

-define(NS_STREAM,       "http://etherx.jabber.org/streams").

-define(NS_STANZAS,      "urn:ietf:params:xml:ns:xmpp-stanzas").
-define(NS_STREAMS,      "urn:ietf:params:xml:ns:xmpp-streams").

-define(NS_TLS,          "urn:ietf:params:xml:ns:xmpp-tls").
-define(NS_SASL,         "urn:ietf:params:xml:ns:xmpp-sasl").
-define(NS_SESSION,      "urn:ietf:params:xml:ns:xmpp-session").
-define(NS_BIND,         "urn:ietf:params:xml:ns:xmpp-bind").

-define(NS_FEATURE_IQAUTH, "http://jabber.org/features/iq-auth").
-define(NS_FEATURE_IQREGISTER, "http://jabber.org/features/iq-register").
-define(NS_FEATURE_COMPRESS, "http://jabber.org/features/compress").
-define(NS_FEATURE_MSGOFFLINE, "msgoffline").

-define(NS_COMPRESS,     "http://jabber.org/protocol/compress").

-define(NS_CAPS,          "http://jabber.org/protocol/caps").
-define(NS_SHIM,          "http://jabber.org/protocol/shim").
-define(NS_ADDRESS,       "http://jabber.org/protocol/address").

%% CAPTCHA related NSes.
-define(NS_OOB, "jabber:x:oob").
-define(NS_CAPTCHA, "urn:xmpp:captcha").
-define(NS_MEDIA, "urn:xmpp:media-element").
-define(NS_BOB, "urn:xmpp:bob").

% TODO: remove "code" attribute (currently it used for backward-compatibility)
-define(STANZA_ERROR(Code, Type, Condition),
	{xmlelement, "error",
	 [{"code", Code}, {"type", Type}],
	 [{xmlelement, Condition, [{"xmlns", ?NS_STANZAS}], []}]}).

-define(ERR_BAD_FORMAT,
	?STANZA_ERROR("406", "modify", "bad-format")).
-define(ERR_BAD_REQUEST,
	?STANZA_ERROR("400", "modify", "bad-request")).
-define(ERR_CONFLICT,
	?STANZA_ERROR("409", "cancel", "conflict")).
-define(ERR_FEATURE_NOT_IMPLEMENTED,
	?STANZA_ERROR("501", "cancel", "feature-not-implemented")).
-define(ERR_FORBIDDEN,
	?STANZA_ERROR("403", "auth",   "forbidden")).
-define(ERR_GONE,
	?STANZA_ERROR("302", "modify", "gone")).
-define(ERR_INTERNAL_SERVER_ERROR,
	?STANZA_ERROR("500", "wait",   "internal-server-error")).
-define(ERR_ITEM_NOT_FOUND,
	?STANZA_ERROR("404", "cancel", "item-not-found")).
-define(ERR_JID_MALFORMED,
	?STANZA_ERROR("400", "modify", "jid-malformed")).
-define(ERR_NOT_ACCEPTABLE,
	?STANZA_ERROR("406", "modify", "not-acceptable")).
-define(ERR_NOT_ALLOWED,
	?STANZA_ERROR("405", "cancel", "not-allowed")).
-define(ERR_NOT_AUTHORIZED,
	?STANZA_ERROR("401", "auth",   "not-authorized")).
-define(ERR_PAYMENT_REQUIRED,
	?STANZA_ERROR("402", "auth",   "payment-required")).
-define(ERR_RECIPIENT_UNAVAILABLE,
	?STANZA_ERROR("404", "wait",   "recipient-unavailable")).
-define(ERR_REDIRECT,
	?STANZA_ERROR("302", "modify", "redirect")).
-define(ERR_REGISTRATION_REQUIRED,
	?STANZA_ERROR("407", "auth",   "registration-required")).
-define(ERR_REMOTE_SERVER_NOT_FOUND,
	?STANZA_ERROR("404", "cancel", "remote-server-not-found")).
-define(ERR_REMOTE_SERVER_TIMEOUT,
	?STANZA_ERROR("504", "wait",   "remote-server-timeout")).
-define(ERR_RESOURCE_CONSTRAINT,
	?STANZA_ERROR("500", "wait",   "resource-constraint")).
-define(ERR_SERVICE_UNAVAILABLE,
	?STANZA_ERROR("503", "cancel", "service-unavailable")).
-define(ERR_SUBSCRIPTION_REQUIRED,
	?STANZA_ERROR("407", "auth",   "subscription-required")).
-define(ERR_UNEXPECTED_REQUEST,
	?STANZA_ERROR("400", "wait",   "unexpected-request")).
-define(ERR_UNEXPECTED_REQUEST_CANCEL,
  ?STANZA_ERROR("401", "cancel", "unexpected-request")).
%-define(ERR_,
%	?STANZA_ERROR("", "", "")).

-define(STANZA_ERRORT(Code, Type, Condition, Lang, Text),
	{xmlelement, "error",
	 [{"code", Code}, {"type", Type}],
	 [{xmlelement, Condition, [{"xmlns", ?NS_STANZAS}], []},
	  {xmlelement, "text", [{"xmlns", ?NS_STANZAS}],
	   [{xmlcdata, translate:translate(Lang, Text)}]}]}).

-define(ERRT_BAD_FORMAT(Lang, Text),
	?STANZA_ERRORT("406", "modify", "bad-format", Lang, Text)).
-define(ERRT_BAD_REQUEST(Lang, Text),
	?STANZA_ERRORT("400", "modify", "bad-request", Lang, Text)).
-define(ERRT_CONFLICT(Lang, Text),
	?STANZA_ERRORT("409", "cancel", "conflict", Lang, Text)).
-define(ERRT_FEATURE_NOT_IMPLEMENTED(Lang, Text),
	?STANZA_ERRORT("501", "cancel", "feature-not-implemented", Lang, Text)).
-define(ERRT_FORBIDDEN(Lang, Text),
	?STANZA_ERRORT("403", "auth",   "forbidden", Lang, Text)).
-define(ERRT_GONE(Lang, Text),
	?STANZA_ERRORT("302", "modify", "gone", Lang, Text)).
-define(ERRT_INTERNAL_SERVER_ERROR(Lang, Text),
	?STANZA_ERRORT("500", "wait",   "internal-server-error", Lang, Text)).
-define(ERRT_ITEM_NOT_FOUND(Lang, Text),
	?STANZA_ERRORT("404", "cancel", "item-not-found", Lang, Text)).
-define(ERRT_JID_MALFORMED(Lang, Text),
	?STANZA_ERRORT("400", "modify", "jid-malformed", Lang, Text)).
-define(ERRT_NOT_ACCEPTABLE(Lang, Text),
	?STANZA_ERRORT("406", "modify", "not-acceptable", Lang, Text)).
-define(ERRT_NOT_ALLOWED(Lang, Text),
	?STANZA_ERRORT("405", "cancel", "not-allowed", Lang, Text)).
-define(ERRT_NOT_AUTHORIZED(Lang, Text),
	?STANZA_ERRORT("401", "auth",   "not-authorized", Lang, Text)).
-define(ERRT_PAYMENT_REQUIRED(Lang, Text),
	?STANZA_ERRORT("402", "auth",   "payment-required", Lang, Text)).
-define(ERRT_RECIPIENT_UNAVAILABLE(Lang, Text),
	?STANZA_ERRORT("404", "wait",   "recipient-unavailable", Lang, Text)).
-define(ERRT_REDIRECT(Lang, Text),
	?STANZA_ERRORT("302", "modify", "redirect", Lang, Text)).
-define(ERRT_REGISTRATION_REQUIRED(Lang, Text),
	?STANZA_ERRORT("407", "auth",   "registration-required", Lang, Text)).
-define(ERRT_REMOTE_SERVER_NOT_FOUND(Lang, Text),
	?STANZA_ERRORT("404", "cancel", "remote-server-not-found", Lang, Text)).
-define(ERRT_REMOTE_SERVER_TIMEOUT(Lang, Text),
	?STANZA_ERRORT("504", "wait",   "remote-server-timeout", Lang, Text)).
-define(ERRT_RESOURCE_CONSTRAINT(Lang, Text),
	?STANZA_ERRORT("500", "wait",   "resource-constraint", Lang, Text)).
-define(ERRT_SERVICE_UNAVAILABLE(Lang, Text),
	?STANZA_ERRORT("503", "cancel", "service-unavailable", Lang, Text)).
-define(ERRT_SUBSCRIPTION_REQUIRED(Lang, Text),
	?STANZA_ERRORT("407", "auth",   "subscription-required", Lang, Text)).
-define(ERRT_UNEXPECTED_REQUEST(Lang, Text),
	?STANZA_ERRORT("400", "wait",   "unexpected-request", Lang, Text)).

% Auth stanza errors
-define(ERR_AUTH_NO_RESOURCE_PROVIDED(Lang),
	?ERRT_NOT_ACCEPTABLE(Lang, "No resource provided")).
-define(ERR_AUTH_BAD_RESOURCE_FORMAT(Lang),
	?ERRT_NOT_ACCEPTABLE(Lang, "Illegal resource format")).
-define(ERR_AUTH_RESOURCE_CONFLICT(Lang),
	?ERRT_CONFLICT(Lang, "Resource conflict")).


-define(STREAM_ERROR(Condition),
	{xmlelement, "stream:error",
	 [],
	 [{xmlelement, Condition, [{"xmlns", ?NS_STREAMS}], []}]}).

-define(SERR_BAD_FORMAT,
	?STREAM_ERROR("bad-format")).
-define(SERR_BAD_NAMESPACE_PREFIX,
	?STREAM_ERROR("bad-namespace-prefix")).
-define(SERR_CONFLICT,
	?STREAM_ERROR("conflict")).
-define(SERR_CONNECTION_TIMEOUT,
	?STREAM_ERROR("connection-timeout")).
-define(SERR_HOST_GONE,
	?STREAM_ERROR("host-gone")).
-define(SERR_HOST_UNKNOWN,
	?STREAM_ERROR("host-unknown")).
-define(SERR_IMPROPER_ADDRESSING,
	?STREAM_ERROR("improper-addressing")).
-define(SERR_INTERNAL_SERVER_ERROR,
	?STREAM_ERROR("internal-server-error")).
-define(SERR_INVALID_FROM,
	?STREAM_ERROR("invalid-from")).
-define(SERR_INVALID_ID,
	?STREAM_ERROR("invalid-id")).
-define(SERR_INVALID_NAMESPACE,
	?STREAM_ERROR("invalid-namespace")).
-define(SERR_INVALID_XML,
	?STREAM_ERROR("invalid-xml")).
-define(SERR_NOT_AUTHORIZED,
	?STREAM_ERROR("not-authorized")).
-define(SERR_POLICY_VIOLATION,
	?STREAM_ERROR("policy-violation")).
-define(SERR_REMOTE_CONNECTION_FAILED,
	?STREAM_ERROR("remote-connection-failed")).
-define(SERR_RESOURSE_CONSTRAINT,
	?STREAM_ERROR("resource-constraint")).
-define(SERR_RESTRICTED_XML,
	?STREAM_ERROR("restricted-xml")).
% TODO: include hostname or IP
-define(SERR_SEE_OTHER_HOST,
	?STREAM_ERROR("see-other-host")).
-define(SERR_SYSTEM_SHUTDOWN,
	?STREAM_ERROR("system-shutdown")).
-define(SERR_UNSUPPORTED_ENCODING,
	?STREAM_ERROR("unsupported-encoding")).
-define(SERR_UNSUPPORTED_STANZA_TYPE,
	?STREAM_ERROR("unsupported-stanza-type")).
-define(SERR_UNSUPPORTED_VERSION,
	?STREAM_ERROR("unsupported-version")).
-define(SERR_XML_NOT_WELL_FORMED,
	?STREAM_ERROR("xml-not-well-formed")).
%-define(SERR_,
%	?STREAM_ERROR("")).

-define(STREAM_ERRORT(Condition, Lang, Text),
	{xmlelement, "stream:error",
	 [],
	 [{xmlelement, Condition, [{"xmlns", ?NS_STREAMS}], []},
	  {xmlelement, "text", [{"xml:lang", Lang}, {"xmlns", ?NS_STREAMS}],
	   [{xmlcdata, translate:translate(Lang, Text)}]}]}).

-define(SERRT_BAD_FORMAT(Lang, Text),
	?STREAM_ERRORT("bad-format", Lang, Text)).
-define(SERRT_BAD_NAMESPACE_PREFIX(Lang, Text),
	?STREAM_ERRORT("bad-namespace-prefix", Lang, Text)).
-define(SERRT_CONFLICT(Lang, Text),
	?STREAM_ERRORT("conflict", Lang, Text)).
-define(SERRT_CONNECTION_TIMEOUT(Lang, Text),
	?STREAM_ERRORT("connection-timeout", Lang, Text)).
-define(SERRT_HOST_GONE(Lang, Text),
	?STREAM_ERRORT("host-gone", Lang, Text)).
-define(SERRT_HOST_UNKNOWN(Lang, Text),
	?STREAM_ERRORT("host-unknown", Lang, Text)).
-define(SERRT_IMPROPER_ADDRESSING(Lang, Text),
	?STREAM_ERRORT("improper-addressing", Lang, Text)).
-define(SERRT_INTERNAL_SERVER_ERROR(Lang, Text),
	?STREAM_ERRORT("internal-server-error", Lang, Text)).
-define(SERRT_INVALID_FROM(Lang, Text),
	?STREAM_ERRORT("invalid-from", Lang, Text)).
-define(SERRT_INVALID_ID(Lang, Text),
	?STREAM_ERRORT("invalid-id", Lang, Text)).
-define(SERRT_INVALID_NAMESPACE(Lang, Text),
	?STREAM_ERRORT("invalid-namespace", Lang, Text)).
-define(SERRT_INVALID_XML(Lang, Text),
	?STREAM_ERRORT("invalid-xml", Lang, Text)).
-define(SERRT_NOT_AUTHORIZED(Lang, Text),
	?STREAM_ERRORT("not-authorized", Lang, Text)).
-define(SERRT_POLICY_VIOLATION(Lang, Text),
	?STREAM_ERRORT("policy-violation", Lang, Text)).
-define(SERRT_REMOTE_CONNECTION_FAILED(Lang, Text),
	?STREAM_ERRORT("remote-connection-failed", Lang, Text)).
-define(SERRT_RESOURSE_CONSTRAINT(Lang, Text),
	?STREAM_ERRORT("resource-constraint", Lang, Text)).
-define(SERRT_RESTRICTED_XML(Lang, Text),
	?STREAM_ERRORT("restricted-xml", Lang, Text)).
% TODO: include hostname or IP
-define(SERRT_SEE_OTHER_HOST(Lang, Text),
	?STREAM_ERRORT("see-other-host", Lang, Text)).
-define(SERRT_SYSTEM_SHUTDOWN(Lang, Text),
	?STREAM_ERRORT("system-shutdown", Lang, Text)).
-define(SERRT_UNSUPPORTED_ENCODING(Lang, Text),
	?STREAM_ERRORT("unsupported-encoding", Lang, Text)).
-define(SERRT_UNSUPPORTED_STANZA_TYPE(Lang, Text),
	?STREAM_ERRORT("unsupported-stanza-type", Lang, Text)).
-define(SERRT_UNSUPPORTED_VERSION(Lang, Text),
	?STREAM_ERRORT("unsupported-version", Lang, Text)).
-define(SERRT_XML_NOT_WELL_FORMED(Lang, Text),
	?STREAM_ERRORT("xml-not-well-formed", Lang, Text)).
%-define(SERRT_(Lang, Text),
%	?STREAM_ERRORT("", Lang, Text)).


-record(jid, {user, server, resource,
	      luser, lserver, lresource}).

-record(iq, {id = "",
	     type,
	     xmlns = "",
	     lang = "",
	     sub_el}).

-record(rsm_in, {max, direction, id, index}).
-record(rsm_out, {count, index, first, last}).
