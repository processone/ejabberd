%%%----------------------------------------------------------------------
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

-include("ns.hrl").
-include_lib("p1_xml/include/xml.hrl").

-define(STANZA_ERROR(Code, Type, Condition),
	#xmlel{name = <<"error">>,
	       attrs = [{<<"code">>, Code}, {<<"type">>, Type}],
	       children =
		   [#xmlel{name = Condition,
			   attrs = [{<<"xmlns">>, ?NS_STANZAS}],
			   children = []}]}).

-define(ERR_BAD_FORMAT,
	?STANZA_ERROR(<<"406">>, <<"modify">>,
		      <<"bad-format">>)).

-define(ERR_BAD_REQUEST,
	?STANZA_ERROR(<<"400">>, <<"modify">>,
		      <<"bad-request">>)).

-define(ERR_CONFLICT,
	?STANZA_ERROR(<<"409">>, <<"cancel">>, <<"conflict">>)).

-define(ERR_FEATURE_NOT_IMPLEMENTED,
	?STANZA_ERROR(<<"501">>, <<"cancel">>,
		      <<"feature-not-implemented">>)).

-define(ERR_FORBIDDEN,
	?STANZA_ERROR(<<"403">>, <<"auth">>, <<"forbidden">>)).

-define(ERR_GONE,
	?STANZA_ERROR(<<"302">>, <<"modify">>, <<"gone">>)).

-define(ERR_INTERNAL_SERVER_ERROR,
	?STANZA_ERROR(<<"500">>, <<"wait">>,
		      <<"internal-server-error">>)).

-define(ERR_ITEM_NOT_FOUND,
	?STANZA_ERROR(<<"404">>, <<"cancel">>,
		      <<"item-not-found">>)).

-define(ERR_JID_MALFORMED,
	?STANZA_ERROR(<<"400">>, <<"modify">>,
		      <<"jid-malformed">>)).

-define(ERR_NOT_ACCEPTABLE,
	?STANZA_ERROR(<<"406">>, <<"modify">>,
		      <<"not-acceptable">>)).

-define(ERR_NOT_ALLOWED,
	?STANZA_ERROR(<<"405">>, <<"cancel">>,
		      <<"not-allowed">>)).

-define(ERR_NOT_AUTHORIZED,
	?STANZA_ERROR(<<"401">>, <<"auth">>,
		      <<"not-authorized">>)).

-define(ERR_PAYMENT_REQUIRED,
	?STANZA_ERROR(<<"402">>, <<"auth">>,
		      <<"payment-required">>)).

-define(ERR_RECIPIENT_UNAVAILABLE,
	?STANZA_ERROR(<<"404">>, <<"wait">>,
		      <<"recipient-unavailable">>)).

-define(ERR_REDIRECT,
	?STANZA_ERROR(<<"302">>, <<"modify">>, <<"redirect">>)).

-define(ERR_REGISTRATION_REQUIRED,
	?STANZA_ERROR(<<"407">>, <<"auth">>,
		      <<"registration-required">>)).

-define(ERR_REMOTE_SERVER_NOT_FOUND,
	?STANZA_ERROR(<<"404">>, <<"cancel">>,
		      <<"remote-server-not-found">>)).

-define(ERR_REMOTE_SERVER_TIMEOUT,
	?STANZA_ERROR(<<"504">>, <<"wait">>,
		      <<"remote-server-timeout">>)).

-define(ERR_RESOURCE_CONSTRAINT,
	?STANZA_ERROR(<<"500">>, <<"wait">>,
		      <<"resource-constraint">>)).

-define(ERR_SERVICE_UNAVAILABLE,
	?STANZA_ERROR(<<"503">>, <<"cancel">>,
		      <<"service-unavailable">>)).

-define(ERR_SUBSCRIPTION_REQUIRED,
	?STANZA_ERROR(<<"407">>, <<"auth">>,
		      <<"subscription-required">>)).

-define(ERR_UNEXPECTED_REQUEST,
	?STANZA_ERROR(<<"400">>, <<"wait">>,
		      <<"unexpected-request">>)).

-define(ERR_UNEXPECTED_REQUEST_CANCEL,
	?STANZA_ERROR(<<"401">>, <<"cancel">>,
		      <<"unexpected-request">>)).

%-define(ERR_,
%	?STANZA_ERROR("", "", "")).

-define(STANZA_ERRORT(Code, Type, Condition, Lang,
		      Text),
	#xmlel{name = <<"error">>,
	       attrs = [{<<"code">>, Code}, {<<"type">>, Type}],
	       children =
		   [#xmlel{name = Condition,
			   attrs = [{<<"xmlns">>, ?NS_STANZAS}], children = []},
		    #xmlel{name = <<"text">>,
			   attrs = [{<<"xmlns">>, ?NS_STANZAS}],
			   children =
			       [{xmlcdata,
				 translate:translate(Lang, Text)}]}]}).

-define(ERRT_BAD_FORMAT(Lang, Text),
	?STANZA_ERRORT(<<"406">>, <<"modify">>,
		       <<"bad-format">>, Lang, Text)).

-define(ERRT_BAD_REQUEST(Lang, Text),
	?STANZA_ERRORT(<<"400">>, <<"modify">>,
		       <<"bad-request">>, Lang, Text)).

-define(ERRT_CONFLICT(Lang, Text),
	?STANZA_ERRORT(<<"409">>, <<"cancel">>, <<"conflict">>,
		       Lang, Text)).

-define(ERRT_FEATURE_NOT_IMPLEMENTED(Lang, Text),
	?STANZA_ERRORT(<<"501">>, <<"cancel">>,
		       <<"feature-not-implemented">>, Lang, Text)).

-define(ERRT_FORBIDDEN(Lang, Text),
	?STANZA_ERRORT(<<"403">>, <<"auth">>, <<"forbidden">>,
		       Lang, Text)).

-define(ERRT_GONE(Lang, Text),
	?STANZA_ERRORT(<<"302">>, <<"modify">>, <<"gone">>,
		       Lang, Text)).

-define(ERRT_INTERNAL_SERVER_ERROR(Lang, Text),
	?STANZA_ERRORT(<<"500">>, <<"wait">>,
		       <<"internal-server-error">>, Lang, Text)).

-define(ERRT_ITEM_NOT_FOUND(Lang, Text),
	?STANZA_ERRORT(<<"404">>, <<"cancel">>,
		       <<"item-not-found">>, Lang, Text)).

-define(ERRT_JID_MALFORMED(Lang, Text),
	?STANZA_ERRORT(<<"400">>, <<"modify">>,
		       <<"jid-malformed">>, Lang, Text)).

-define(ERRT_NOT_ACCEPTABLE(Lang, Text),
	?STANZA_ERRORT(<<"406">>, <<"modify">>,
		       <<"not-acceptable">>, Lang, Text)).

-define(ERRT_NOT_ALLOWED(Lang, Text),
	?STANZA_ERRORT(<<"405">>, <<"cancel">>,
		       <<"not-allowed">>, Lang, Text)).

-define(ERRT_NOT_AUTHORIZED(Lang, Text),
	?STANZA_ERRORT(<<"401">>, <<"auth">>,
		       <<"not-authorized">>, Lang, Text)).

-define(ERRT_PAYMENT_REQUIRED(Lang, Text),
	?STANZA_ERRORT(<<"402">>, <<"auth">>,
		       <<"payment-required">>, Lang, Text)).

-define(ERRT_RECIPIENT_UNAVAILABLE(Lang, Text),
	?STANZA_ERRORT(<<"404">>, <<"wait">>,
		       <<"recipient-unavailable">>, Lang, Text)).

-define(ERRT_REDIRECT(Lang, Text),
	?STANZA_ERRORT(<<"302">>, <<"modify">>, <<"redirect">>,
		       Lang, Text)).

-define(ERRT_REGISTRATION_REQUIRED(Lang, Text),
	?STANZA_ERRORT(<<"407">>, <<"auth">>,
		       <<"registration-required">>, Lang, Text)).

-define(ERRT_REMOTE_SERVER_NOT_FOUND(Lang, Text),
	?STANZA_ERRORT(<<"404">>, <<"cancel">>,
		       <<"remote-server-not-found">>, Lang, Text)).

-define(ERRT_REMOTE_SERVER_TIMEOUT(Lang, Text),
	?STANZA_ERRORT(<<"504">>, <<"wait">>,
		       <<"remote-server-timeout">>, Lang, Text)).

-define(ERRT_RESOURCE_CONSTRAINT(Lang, Text),
	?STANZA_ERRORT(<<"500">>, <<"wait">>,
		       <<"resource-constraint">>, Lang, Text)).

-define(ERRT_SERVICE_UNAVAILABLE(Lang, Text),
	?STANZA_ERRORT(<<"503">>, <<"cancel">>,
		       <<"service-unavailable">>, Lang, Text)).

-define(ERRT_SUBSCRIPTION_REQUIRED(Lang, Text),
	?STANZA_ERRORT(<<"407">>, <<"auth">>,
		       <<"subscription-required">>, Lang, Text)).

-define(ERRT_UNEXPECTED_REQUEST(Lang, Text),
	?STANZA_ERRORT(<<"400">>, <<"wait">>,
		       <<"unexpected-request">>, Lang, Text)).

-define(ERR_AUTH_NO_RESOURCE_PROVIDED(Lang),
	?ERRT_NOT_ACCEPTABLE(Lang, <<"No resource provided">>)).

-define(ERR_AUTH_BAD_RESOURCE_FORMAT(Lang),
	?ERRT_NOT_ACCEPTABLE(Lang,
			     <<"Illegal resource format">>)).

-define(ERR_AUTH_RESOURCE_CONFLICT(Lang),
	?ERRT_CONFLICT(Lang, <<"Resource conflict">>)).

-define(STREAM_ERROR(Condition, Cdata),
	#xmlel{name = <<"stream:error">>, attrs = [],
	       children =
		   [#xmlel{name = Condition,
			   attrs = [{<<"xmlns">>, ?NS_STREAMS}],
			   children = [{xmlcdata, Cdata}]}]}).

-define(SERR_BAD_FORMAT,
	?STREAM_ERROR(<<"bad-format">>, <<"">>)).

-define(SERR_BAD_NAMESPACE_PREFIX,
	?STREAM_ERROR(<<"bad-namespace-prefix">>, <<"">>)).

-define(SERR_CONFLICT,
	?STREAM_ERROR(<<"conflict">>, <<"">>)).

-define(SERR_CONNECTION_TIMEOUT,
	?STREAM_ERROR(<<"connection-timeout">>, <<"">>)).

-define(SERR_HOST_GONE,
	?STREAM_ERROR(<<"host-gone">>, <<"">>)).

-define(SERR_HOST_UNKNOWN,
	?STREAM_ERROR(<<"host-unknown">>, <<"">>)).

-define(SERR_IMPROPER_ADDRESSING,
	?STREAM_ERROR(<<"improper-addressing">>, <<"">>)).

-define(SERR_INTERNAL_SERVER_ERROR,
	?STREAM_ERROR(<<"internal-server-error">>, <<"">>)).

-define(SERR_INVALID_FROM,
	?STREAM_ERROR(<<"invalid-from">>, <<"">>)).

-define(SERR_INVALID_ID,
	?STREAM_ERROR(<<"invalid-id">>, <<"">>)).

-define(SERR_INVALID_NAMESPACE,
	?STREAM_ERROR(<<"invalid-namespace">>, <<"">>)).

-define(SERR_INVALID_XML,
	?STREAM_ERROR(<<"invalid-xml">>, <<"">>)).

-define(SERR_NOT_AUTHORIZED,
	?STREAM_ERROR(<<"not-authorized">>, <<"">>)).

-define(SERR_POLICY_VIOLATION,
	?STREAM_ERROR(<<"policy-violation">>, <<"">>)).

-define(SERR_REMOTE_CONNECTION_FAILED,
	?STREAM_ERROR(<<"remote-connection-failed">>, <<"">>)).

-define(SERR_RESOURSE_CONSTRAINT,
	?STREAM_ERROR(<<"resource-constraint">>, <<"">>)).

-define(SERR_RESTRICTED_XML,
	?STREAM_ERROR(<<"restricted-xml">>, <<"">>)).

-define(SERR_SEE_OTHER_HOST(Host),
	?STREAM_ERROR(<<"see-other-host">>, Host)).

-define(SERR_SYSTEM_SHUTDOWN,
	?STREAM_ERROR(<<"system-shutdown">>, <<"">>)).

-define(SERR_UNSUPPORTED_ENCODING,
	?STREAM_ERROR(<<"unsupported-encoding">>, <<"">>)).

-define(SERR_UNSUPPORTED_STANZA_TYPE,
	?STREAM_ERROR(<<"unsupported-stanza-type">>, <<"">>)).

-define(SERR_UNSUPPORTED_VERSION,
	?STREAM_ERROR(<<"unsupported-version">>, <<"">>)).

-define(SERR_XML_NOT_WELL_FORMED,
	?STREAM_ERROR(<<"xml-not-well-formed">>, <<"">>)).

%-define(SERR_,
%	?STREAM_ERROR("", "")).

-define(STREAM_ERRORT(Condition, Cdata, Lang, Text),
	#xmlel{name = <<"stream:error">>, attrs = [],
	       children =
		   [#xmlel{name = Condition,
			   attrs = [{<<"xmlns">>, ?NS_STREAMS}],
			   children = [{xmlcdata, Cdata}]},
		    #xmlel{name = <<"text">>,
			   attrs =
			       [{<<"xml:lang">>, Lang},
				{<<"xmlns">>, ?NS_STREAMS}],
			   children =
			       [{xmlcdata,
				 translate:translate(Lang, Text)}]}]}).

-define(SERRT_BAD_FORMAT(Lang, Text),
	?STREAM_ERRORT(<<"bad-format">>, <<"">>, Lang, Text)).

-define(SERRT_BAD_NAMESPACE_PREFIX(Lang, Text),
	?STREAM_ERRORT(<<"bad-namespace-prefix">>, <<"">>, Lang,
		       Text)).

-define(SERRT_CONFLICT(Lang, Text),
	?STREAM_ERRORT(<<"conflict">>, <<"">>, Lang, Text)).

-define(SERRT_CONNECTION_TIMEOUT(Lang, Text),
	?STREAM_ERRORT(<<"connection-timeout">>, <<"">>, Lang,
		       Text)).

-define(SERRT_HOST_GONE(Lang, Text),
	?STREAM_ERRORT(<<"host-gone">>, <<"">>, Lang, Text)).

-define(SERRT_HOST_UNKNOWN(Lang, Text),
	?STREAM_ERRORT(<<"host-unknown">>, <<"">>, Lang, Text)).

-define(SERRT_IMPROPER_ADDRESSING(Lang, Text),
	?STREAM_ERRORT(<<"improper-addressing">>, <<"">>, Lang,
		       Text)).

-define(SERRT_INTERNAL_SERVER_ERROR(Lang, Text),
	?STREAM_ERRORT(<<"internal-server-error">>, <<"">>,
		       Lang, Text)).

-define(SERRT_INVALID_FROM(Lang, Text),
	?STREAM_ERRORT(<<"invalid-from">>, <<"">>, Lang, Text)).

-define(SERRT_INVALID_ID(Lang, Text),
	?STREAM_ERRORT(<<"invalid-id">>, <<"">>, Lang, Text)).

-define(SERRT_INVALID_NAMESPACE(Lang, Text),
	?STREAM_ERRORT(<<"invalid-namespace">>, <<"">>, Lang,
		       Text)).

-define(SERRT_INVALID_XML(Lang, Text),
	?STREAM_ERRORT(<<"invalid-xml">>, <<"">>, Lang, Text)).

-define(SERRT_NOT_AUTHORIZED(Lang, Text),
	?STREAM_ERRORT(<<"not-authorized">>, <<"">>, Lang,
		       Text)).

-define(SERRT_POLICY_VIOLATION(Lang, Text),
	?STREAM_ERRORT(<<"policy-violation">>, <<"">>, Lang,
		       Text)).

-define(SERRT_REMOTE_CONNECTION_FAILED(Lang, Text),
	?STREAM_ERRORT(<<"remote-connection-failed">>, <<"">>,
		       Lang, Text)).

-define(SERRT_RESOURSE_CONSTRAINT(Lang, Text),
	?STREAM_ERRORT(<<"resource-constraint">>, <<"">>, Lang,
		       Text)).

-define(SERRT_RESTRICTED_XML(Lang, Text),
	?STREAM_ERRORT(<<"restricted-xml">>, <<"">>, Lang,
		       Text)).

-define(SERRT_SEE_OTHER_HOST(Host, Lang, Text),
	?STREAM_ERRORT(<<"see-other-host">>, Host, Lang, Text)).

-define(SERRT_SYSTEM_SHUTDOWN(Lang, Text),
	?STREAM_ERRORT(<<"system-shutdown">>, <<"">>, Lang,
		       Text)).

-define(SERRT_UNSUPPORTED_ENCODING(Lang, Text),
	?STREAM_ERRORT(<<"unsupported-encoding">>, <<"">>, Lang,
		       Text)).

-define(SERRT_UNSUPPORTED_STANZA_TYPE(Lang, Text),
	?STREAM_ERRORT(<<"unsupported-stanza-type">>, <<"">>,
		       Lang, Text)).

-define(SERRT_UNSUPPORTED_VERSION(Lang, Text),
	?STREAM_ERRORT(<<"unsupported-version">>, <<"">>, Lang,
		       Text)).

-define(SERRT_XML_NOT_WELL_FORMED(Lang, Text),
	?STREAM_ERRORT(<<"xml-not-well-formed">>, <<"">>, Lang,
		       Text)).

-record(jid, {user = <<"">> :: binary(),
              server = <<"">> :: binary(),
              resource = <<"">> :: binary(),
              luser = <<"">> :: binary(),
              lserver = <<"">> :: binary(),
              lresource = <<"">> :: binary()}).

-type(jid() :: #jid{}).

-type(ljid() :: {binary(), binary(), binary()}).

-record(iq, {id = <<"">>       :: binary(),
             type = get        :: get | set | result | error,
             xmlns = <<"">>    :: binary(),
             lang  = <<"">>    :: binary(),
             sub_el = #xmlel{} :: xmlel() | [xmlel()]}).

-type(iq_get()
  :: #iq{
         id     :: binary(),
         type   :: get,
         xmlns  :: binary(),
         lang   :: binary(),
         sub_el :: xmlel()
     }
).

-type(iq_set()
  :: #iq{
         id     :: binary(),
         type   :: set,
         xmlns  :: binary(),
         lang   :: binary(),
         sub_el :: xmlel()
     }
).

-type iq_request() :: iq_get() | iq_set().

-type(iq_result()
  :: #iq{
         id     :: binary(),
         type   :: result,
         xmlns  :: binary(),
         lang   :: binary(),
         sub_el :: [xmlel()]
     }
).

-type(iq_error()
  :: #iq{
         id     :: binary(),
         type   :: error,
         xmlns  :: binary(),
         lang   :: binary(),
         sub_el :: [xmlel()]
     }
).

-type iq_reply() :: iq_result() | iq_error() .

-type(iq() :: iq_request() | iq_reply()).

-record(rsm_in, {max :: integer(),
                 direction :: before | aft,
                 id :: binary(),
                 index :: integer()}).

-record(rsm_out, {count :: integer(),
                  index :: integer(),
                  first :: binary(),
                  last :: binary()}).

-type(rsm_in() :: #rsm_in{}).

-type(rsm_out() :: #rsm_out{}).

-type broadcast() :: {broadcast, broadcast_data()}.

-type broadcast_data() ::
        {rebind, pid(), binary()} | %% ejabberd_c2s
        {item, ljid(), mod_roster:subscription()} | %% mod_roster/mod_shared_roster
        {exit, binary()} | %% mod_roster/mod_shared_roster
        {privacy_list, mod_privacy:userlist(), binary()} | %% mod_privacy
        {blocking, unblock_all | {block | unblock, [ljid()]}}. %% mod_blocking

-record(xmlelement, {name = ""     :: string(),
                     attrs = []    :: [{string(), string()}],
                     children = [] :: [{xmlcdata, iodata()} | xmlelement()]}).

-type xmlelement() :: #xmlelement{}.
