%%%----------------------------------------------------------------------
%%% File    : jlib.hrl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  5 Mar 2003 by Alexey Shchepin <alexey@sevcom.net>
%%%----------------------------------------------------------------------

-define(NS_DISCO_ITEMS, "http://jabber.org/protocol/disco#items").
-define(NS_DISCO_INFO,  "http://jabber.org/protocol/disco#info").
-define(NS_VCARD,       "vcard-temp").
-define(NS_AUTH,        "jabber:iq:auth").
-define(NS_REGISTER,    "jabber:iq:register").
-define(NS_SEARCH,      "jabber:iq:search").
-define(NS_ROSTER,      "jabber:iq:roster").
-define(NS_PRIVATE,     "jabber:iq:private").
-define(NS_VERSION,     "jabber:iq:version").
-define(NS_TIME,        "jabber:iq:time").
-define(NS_XDATA,       "jabber:x:data").
-define(NS_IQDATA,      "jabber:iq:data").
-define(NS_DELAY,       "jabber:x:delay").
-define(NS_EVENT,       "jabber:x:event").
-define(NS_STATS,       "http://jabber.org/protocol/stats").
-define(NS_MUC,         "http://jabber.org/protocol/muc").
-define(NS_MUC_USER,    "http://jabber.org/protocol/muc#user").
-define(NS_MUC_ADMIN,   "http://jabber.org/protocol/muc#admin").
-define(NS_MUC_OWNER,   "http://jabber.org/protocol/muc#owner").

-define(NS_STREAM,      "http://etherx.jabber.org/streams").

% TODO: replace "xmppcore-rfc-number" with real RFC number
-define(NS_STANZAS,     "urn:ietf:rfc:xmppcore-rfc-number:stanzas").
-define(NS_STREAMS,     "urn:ietf:rfc:xmppcore-rfc-number:streams").

-define(NS_SASL_MECHANISMS, "http://www.iana.org/assignments/sasl-mechanisms").

% TODO: remove "code" attribute (currently it used for backward-compatibility)
-define(STANZA_ERROR(Code, Class, Condition),
	{xmlelement, "error",
	 [{"code", Code}, {"class", Class}],
	 [{xmlelement, "stanza-condition",
	   [{"xmlns", ?NS_STANZAS}],
	   [{xmlelement, Condition, [], []}]}]}).

-define(ERR_BAD_REQUEST,
	?STANZA_ERROR("400", "format",    "bad-request")).
-define(ERR_FEATURE_NOT_IMPLEMENTED,
	?STANZA_ERROR("501", "recipient", "feature-not-implemented")).
-define(ERR_FORBIDDEN,
	?STANZA_ERROR("403", "format",    "forbidden")).
-define(ERR_INTERNAL_SERVER_ERROR,
	?STANZA_ERROR("403", "server",    "internal-server-error")).
-define(ERR_JID_MALFORMED,
	?STANZA_ERROR("400", "address",   "jid-malformed")).
-define(ERR_JID_NOT_FOUND,
	?STANZA_ERROR("404", "address",   "jid-not-found")).
-define(ERR_NOT_ALLOWED,
	?STANZA_ERROR("405", "access",    "not-allowed")).
-define(ERR_RECIPIENT_UNAVAILABLE,
	?STANZA_ERROR("503", "recipient", "recipient-unavailable")).
-define(ERR_REGISTRATION_REQUIRED,
	?STANZA_ERROR("407", "access",    "registration-required")).
-define(ERR_REMOTE_SERVER_NOT_FOUND,
	?STANZA_ERROR("502", "address",   "remote-server-not-found")).
-define(ERR_REMOTE_SERVER_TIMEOUT,
	?STANZA_ERROR("504", "server",    "remote-server-timeout")).
-define(ERR_SERVICE_UNAVAILABLE,
	?STANZA_ERROR("503", "server",    "service-unavailable")).
%-define(ERR_,
%	?STANZA_ERROR("", "", "")).


-define(STREAM_ERROR(Class, Condition),
	{xmlelement, "stream:error",
	 [{"class", Class}],
	 [{xmlelement, "stream-condition",
	   [{"xmlns", ?NS_STANZAS}],
	   [{xmlelement, Condition, [], []}]}]}).

-define(SERR_HOST_GONE,
	?STREAM_ERROR("address",  "host-gone")).
-define(SERR_HOST_UNKNOWN,
	?STREAM_ERROR("address",  "host-unknown")).
-define(SERR_INTERNAL_SERVER_ERROR,
	?STREAM_ERROR("server",   "internal-server-error")).
-define(SERR_RESOURSE_CONSTRAINT,
	?STREAM_ERROR("server",   "resource-constraint")).
% TODO: include hostname or IP
-define(SERR_SEE_OTHER_HOST,
	?STREAM_ERROR("redirect", "see-other-host")).
-define(SERR_SYSTEM_SHUTDOWN,
	?STREAM_ERROR("server",   "system-shutdown")).
-define(SERR_UNSUPPORTED_STANZA_TYPE,
	?STREAM_ERROR("format",   "unsupported-stanza-type")).
-define(SERR_UNSUPPORTED_VERSION,
	?STREAM_ERROR("format",   "unsupported-version")).
-define(SERR_XML_NOT_WELL_FORMED,
	?STREAM_ERROR("format",   "xml-not-well-formed")).
%-define(SERR_,
%	?STREAM_ERROR("", "")).



