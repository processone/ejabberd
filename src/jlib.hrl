%%%----------------------------------------------------------------------
%%% File    : jlib.hrl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  5 Mar 2003 by Alexey Shchepin <alexey@sevcom.net>
%%%----------------------------------------------------------------------

-define(NS_DISCO_ITEMS,  "http://jabber.org/protocol/disco#items").
-define(NS_DISCO_INFO,   "http://jabber.org/protocol/disco#info").
-define(NS_VCARD,        "vcard-temp").
-define(NS_AUTH,         "jabber:iq:auth").
-define(NS_AUTH_ERROR,   "jabber:iq:auth:error").
-define(NS_REGISTER,     "jabber:iq:register").
-define(NS_SEARCH,       "jabber:iq:search").
-define(NS_ROSTER,       "jabber:iq:roster").
-define(NS_PRIVATE,      "jabber:iq:private").
-define(NS_VERSION,      "jabber:iq:version").
-define(NS_TIME,         "jabber:iq:time").
-define(NS_XDATA,        "jabber:x:data").
-define(NS_IQDATA,       "jabber:iq:data").
-define(NS_DELAY,        "jabber:x:delay").
-define(NS_EVENT,        "jabber:x:event").
-define(NS_XCONFERENCE,  "jabber:x:conference").
-define(NS_STATS,        "http://jabber.org/protocol/stats").
-define(NS_MUC,          "http://jabber.org/protocol/muc").
-define(NS_MUC_USER,     "http://jabber.org/protocol/muc#user").
-define(NS_MUC_ADMIN,    "http://jabber.org/protocol/muc#admin").
-define(NS_MUC_OWNER,    "http://jabber.org/protocol/muc#owner").
-define(NS_PUBSUB,       "http://jabber.org/protocol/pubsub").
-define(NS_PUBSUB_EVENT, "http://jabber.org/protocol/pubsub#event").
-define(NS_PUBSUB_OWNER, "http://jabber.org/protocol/pubsub#owner").

-define(NS_STREAM,       "http://etherx.jabber.org/streams").

-define(NS_STANZAS,      "urn:ietf:params:xml:ns:xmpp-stanzas").
-define(NS_STREAMS,      "urn:ietf:params:xml:ns:xmpp-streams").

-define(NS_SASL,         "urn:ietf:params:xml:ns:xmpp-sasl").
-define(NS_SESSION,      "urn:ietf:params:xml:ns:xmpp-session").

% TODO: remove "code" attribute (currently it used for backward-compatibility)
-define(STANZA_ERROR(Code, Type, Condition),
	{xmlelement, "error",
	 [{"code", Code}, {"type", Type}],
	 [{xmlelement, Condition, [{"xmlns", ?NS_STANZAS}], []}]}).

-define(ERR_BAD_REQUEST,
	?STANZA_ERROR("400", "modify",    "bad-request")).
-define(ERR_CONFLICT,
	?STANZA_ERROR("409", "cancel",    "conflict")).
-define(ERR_FEATURE_NOT_IMPLEMENTED,
	?STANZA_ERROR("501", "cancel",    "feature-not-implemented")).
-define(ERR_FORBIDDEN,
	?STANZA_ERROR("403", "auth",      "forbidden")).
-define(ERR_INTERNAL_SERVER_ERROR,
	?STANZA_ERROR("500", "wait",      "internal-server-error")).
-define(ERR_ITEM_NOT_FOUND,
	?STANZA_ERROR("404", "cancel",    "item-not-found")).
-define(ERR_JID_MALFORMED,
	?STANZA_ERROR("400", "modify",    "jid-malformed")).
-define(ERR_NOT_ALLOWED,
	?STANZA_ERROR("405", "cancel",    "not-allowed")).
-define(ERR_RECIPIENT_UNAVAILABLE,
	?STANZA_ERROR("404", "wait",      "recipient-unavailable")).
-define(ERR_REGISTRATION_REQUIRED,
	?STANZA_ERROR("407", "auth",      "registration-required")).
-define(ERR_REMOTE_SERVER_NOT_FOUND,
	?STANZA_ERROR("404", "cancel",    "remote-server-not-found")).
-define(ERR_REMOTE_SERVER_TIMEOUT,
	?STANZA_ERROR("504", "wait",      "remote-server-timeout")).
-define(ERR_RESOURCE_CONSTRAINT,
	?STANZA_ERROR("0",   "wait",      "resource-constraint")).
-define(ERR_SERVICE_UNAVAILABLE,
	?STANZA_ERROR("503", "cancel",    "service-unavailable")).
-define(ERR_SUBSCRIPTION_REQUIRED,
	?STANZA_ERROR("0",   "auth",      "subscription-required")).
-define(ERR_UNEXPECTED_REQUEST,
	?STANZA_ERROR("0",   "wait",      "unexpected-request")).
%-define(ERR_,
%	?STANZA_ERROR("", "", "")).

% TODO: update to new-style
% Application-specific stanza errors
-define(AUTH_STANZA_ERROR(Condition),
	{xmlelement, "error",
	 [{"code", "406"}, {"class", "app"}],
	 [{xmlelement, "auth-condition",
	   [{"xmlns", ?NS_AUTH_ERROR}],
	   [{xmlelement, Condition, [], []}]}]}).

-define(ERR_AUTH_NO_RESOURCE_PROVIDED,
	?AUTH_STANZA_ERROR("no-resource-provided")).
-define(ERR_AUTH_BAD_RESOURCE_FORMAT,
	?AUTH_STANZA_ERROR("bad-resource-format")).
-define(ERR_AUTH_RESOURCE_CONFLICT,
	?AUTH_STANZA_ERROR("resource-conflict")).


-define(STREAM_ERROR(Condition),
	{xmlelement, "stream:error",
	 [],
	 [{xmlelement, Condition, [{"xmlns", ?NS_STANZAS}], []}]}).

-define(SERR_HOST_GONE,
	?STREAM_ERROR("host-gone")).
-define(SERR_HOST_UNKNOWN,
	?STREAM_ERROR("host-unknown")).
-define(SERR_IMPROPER_ADDRESSING,
	?STREAM_ERROR("improper-addressing")).
-define(SERR_INTERNAL_SERVER_ERROR,
	?STREAM_ERROR("internal-server-error")).
-define(SERR_INVALID_ID,
	?STREAM_ERROR("invalid-id")).
-define(SERR_INVALID_NAMESPACE,
	?STREAM_ERROR("invalid-namespace")).
-define(SERR_NONMATCHING_HOSTS,
	?STREAM_ERROR("nonmatching-hosts")).
-define(SERR_NOT_AUTHORIZED,
	?STREAM_ERROR("not-authorized")).
-define(SERR_REMOTE_CONNECTION_FAILED,
	?STREAM_ERROR("remote-connection-failed")).
-define(SERR_RESOURSE_CONSTRAINT,
	?STREAM_ERROR("resource-constraint")).
% TODO: include hostname or IP
-define(SERR_SEE_OTHER_HOST,
	?STREAM_ERROR("see-other-host")).
-define(SERR_SYSTEM_SHUTDOWN,
	?STREAM_ERROR("system-shutdown")).
-define(SERR_UNSUPPORTED_STANZA_TYPE,
	?STREAM_ERROR("unsupported-stanza-type")).
-define(SERR_UNSUPPORTED_VERSION,
	?STREAM_ERROR("unsupported-version")).
-define(SERR_XML_NOT_WELL_FORMED,
	?STREAM_ERROR("xml-not-well-formed")).
%-define(SERR_,
%	?STREAM_ERROR("")).



