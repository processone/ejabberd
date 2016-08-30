%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2015, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  9 Dec 2015 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(xmpp).

%% API
-export([make_iq_result/1, make_iq_result/2, make_error/2,
	 decode/1, decode/2, encode/1,
	 get_type/1, get_to/1, get_from/1, get_id/1,
	 get_lang/1, get_error/1, get_els/1, get_ns/1,
	 set_type/2, set_to/2, set_from/2, set_id/2,
	 set_lang/2, set_error/2, set_els/2, set_from_to/3,
	 format_error/1, is_stanza/1, set_subtag/2, get_subtag/2,
	 remove_subtag/2, has_subtag/2, decode_els/1, decode_els/2,
	 pp/1, get_name/1, get_text/1, mk_text/1, mk_text/2]).

%% XMPP errors
-export([err_bad_request/0, err_bad_request/2,
         err_conflict/0, err_conflict/2,
         err_feature_not_implemented/0, err_feature_not_implemented/2,
         err_forbidden/0, err_forbidden/2,
         err_gone/0, err_gone/2,
         err_internal_server_error/0, err_internal_server_error/2,
         err_item_not_found/0, err_item_not_found/2,
         err_jid_malformed/0, err_jid_malformed/2,
         err_not_acceptable/0, err_not_acceptable/2,
         err_not_allowed/0, err_not_allowed/2,
         err_not_authorized/0, err_not_authorized/2,
	 err_payment_required/0, err_payment_required/2,
         err_policy_violation/0, err_policy_violation/2,
         err_recipient_unavailable/0, err_recipient_unavailable/2,
         err_redirect/0, err_redirect/2,
         err_registration_required/0, err_registration_required/2,
         err_remote_server_not_found/0, err_remote_server_not_found/2,
         err_remote_server_timeout/0, err_remote_server_timeout/2,
         err_resource_constraint/0, err_resource_constraint/2,
         err_service_unavailable/0, err_service_unavailable/2,
         err_subscription_required/0, err_subscription_required/2,
         err_undefined_condition/1, err_undefined_condition/3,
         err_unexpected_request/0, err_unexpected_request/2]).

%% XMPP stream errors
-export([serr_bad_format/0, serr_bad_format/2,
         serr_bad_namespace_prefix/0, serr_bad_namespace_prefix/2,
         serr_conflict/0, serr_conflict/2,
         serr_connection_timeout/0, serr_connection_timeout/2,
         serr_host_gone/0, serr_host_gone/2,
         serr_host_unknown/0, serr_host_unknown/2,
         serr_improper_addressing/0, serr_improper_addressing/2,
         serr_internal_server_error/0, serr_internal_server_error/2,
         serr_invalid_from/0, serr_invalid_from/2,
         serr_invalid_id/0, serr_invalid_id/2,
         serr_invalid_namespace/0, serr_invalid_namespace/2,
         serr_invalid_xml/0, serr_invalid_xml/2,
         serr_not_authorized/0, serr_not_authorized/2,
         serr_not_well_formed/0, serr_not_well_formed/2,
         serr_policy_violation/0, serr_policy_violation/2,
         serr_remote_connection_failed/0, serr_remote_connection_failed/2,
         serr_reset/0, serr_reset/2,
         serr_resource_constraint/0, serr_resource_constraint/2,
         serr_restricted_xml/0, serr_restricted_xml/2,
         serr_see_other_host/0, serr_see_other_host/2,
         serr_system_shutdown/0, serr_system_shutdown/2,
         serr_undefined_condition/0, serr_undefined_condition/2,
         serr_unsupported_encoding/0, serr_unsupported_encoding/2,
         serr_unsupported_stanza_type/0, serr_unsupported_stanza_type/2,
         serr_unsupported_version/0, serr_unsupported_version/2]).

-include("xmpp.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec make_iq_result(iq()) -> iq().
make_iq_result(IQ) ->
    make_iq_result(IQ, undefined).

-spec make_iq_result(iq(), xmpp_element() | xmlel() | undefined) -> iq().
make_iq_result(#iq{type = Type, from = From, to = To} = IQ, El)
  when Type == get; Type == set ->
    SubEls = if El == undefined -> [];
		true -> [El]
	     end,
    IQ#iq{type = result, to = From, from = To, sub_els = SubEls}.

-spec make_error(message(), error() | xmlel()) -> message();
		(presence(), error() | xmlel()) -> presence();
		(iq(), error() | xmlel()) -> iq();
		(xmlel(), error() | xmlel()) -> xmlel().
make_error(#message{type = Type, from = From, to = To, sub_els = Els} = Msg,
	   Err) when Type /= error ->
    Msg#message{type = error, from = To, to = From, sub_els = Els ++ [Err]};
make_error(#presence{type = Type, from = From, to = To, sub_els = Els} = Pres,
	   Err) when Type /= error ->
    Pres#presence{type = error, from = To, to = From, sub_els = Els ++ [Err]};
make_error(#iq{type = Type, from = From, to = To, sub_els = Els} = IQ,
	   Err) when Type /= result, Type /= error ->
    IQ#iq{type = error, from = To, to = From, sub_els = Els ++ [Err]};
make_error(#xmlel{attrs = Attrs, children = Els} = El, Err) ->
    To = fxml:get_attr(<<"to">>, Attrs),
    From = fxml:get_attr(<<"from">>, Attrs),
    Attrs1 = case To of
		 {value, T} ->
		     lists:keystore(<<"from">>, 1, Attrs, {<<"from">>, T});
		 _ ->
		     Attrs
	     end,
    Attrs2 = case From of
		 {value, F} ->
		     lists:keystore(<<"to">>, 1, Attrs1, {<<"to">>, F});
		 _ ->
		     Attrs
	     end,
    Attrs3 = lists:keystore(<<"type">>, 1, Attrs2, {<<"type">>, <<"error">>}),
    El#xmlel{attrs = Attrs3, children = Els ++ [encode(Err)]}.

-spec get_id(iq() | message() | presence() | xmlel()) -> binary().
get_id(#iq{id = ID}) -> ID;
get_id(#message{id = ID}) -> ID;
get_id(#presence{id = ID}) -> ID;
get_id(#xmlel{attrs = Attrs}) -> fxml:get_attr_s(<<"id">>, Attrs).

-spec get_type(iq()) -> iq_type();
	      (message()) -> message_type();
	      (presence()) -> presence_type();
	      (xmlel()) -> binary().
get_type(#iq{type = T}) -> T;
get_type(#message{type = T}) -> T;
get_type(#presence{type = T}) -> T;
get_type(#xmlel{attrs = Attrs}) -> fxml:get_attr_s(<<"type">>, Attrs).

-spec get_lang(iq() | message() | presence() | xmlel()) -> binary().
get_lang(#iq{lang = L}) -> L;
get_lang(#message{lang = L}) -> L;
get_lang(#presence{lang = L}) -> L;
get_lang(#xmlel{attrs = Attrs}) -> fxml:get_attr_s(<<"xml:lang">>, Attrs).

-spec get_from(iq() | message() | presence()) -> undefined | jid:jid().
get_from(#iq{from = J}) -> J;
get_from(#message{from = J}) -> J;
get_from(#presence{from = J}) -> J.

-spec get_to(iq() | message() | presence()) -> undefined | jid:jid().
get_to(#iq{to = J}) -> J;
get_to(#message{to = J}) -> J;
get_to(#presence{to = J}) -> J.

-spec get_error(iq() | message() | presence()) -> undefined | error().
get_error(Stanza) ->
    case get_subtag(Stanza, #error{}) of
	false -> undefined;
	Error -> Error
    end.

-spec get_els(iq() | message() | presence()) -> [xmpp_element() | xmlel()];
	     (xmlel()) -> [xmlel()].
get_els(#iq{sub_els = Els}) -> Els;
get_els(#message{sub_els = Els}) -> Els;
get_els(#presence{sub_els = Els}) -> Els;
get_els(#xmlel{children = Els}) -> [El || El = #xmlel{} <- Els].

-spec set_id(iq(), binary()) -> iq();
	    (message(), binary()) -> message();
	    (presence(), binary()) -> presence().
set_id(#iq{} = IQ, I) -> IQ#iq{id = I};
set_id(#message{} = Msg, I) -> Msg#message{id = I};
set_id(#presence{} = Pres, I) -> Pres#presence{id = I}.

-spec set_type(iq(), iq_type()) -> iq();
	      (message(), message_type()) -> message();
	      (presence(), presence_type()) -> presence().
set_type(#iq{} = IQ, T) -> IQ#iq{type = T};
set_type(#message{} = Msg, T) -> Msg#message{type = T};
set_type(#presence{} = Pres, T) -> Pres#presence{type = T}.

-spec set_lang(iq(), binary()) -> iq();
	      (message(), binary()) -> message();
	      (presence(), binary()) -> presence().
set_lang(#iq{} = IQ, L) -> IQ#iq{lang = L};
set_lang(#message{} = Msg, L) -> Msg#message{lang = L};
set_lang(#presence{} = Pres, L) -> Pres#presence{lang = L}.

-spec set_from(iq(), jid:jid()) -> iq();
	      (message(), jid:jid()) -> message();
	      (presence(), jid:jid()) -> presence().
set_from(#iq{} = IQ, J) -> IQ#iq{from = J};
set_from(#message{} = Msg, J) -> Msg#message{from = J};
set_from(#presence{} = Pres, J) -> Pres#presence{from = J}.

-spec set_to(iq(), jid:jid()) -> iq();
	    (message(), jid:jid()) -> message();
	    (presence(), jid:jid()) -> presence().
set_to(#iq{} = IQ, J) -> IQ#iq{to = J};
set_to(#message{} = Msg, J) -> Msg#message{to = J};
set_to(#presence{} = Pres, J) -> Pres#presence{to = J}.

-spec set_from_to(iq(), undefined | jid:jid(), undefined | jid:jid()) -> iq();
		 (message(), undefined | jid:jid(), undefined | jid:jid()) -> message();
		 (presence(), undefined | jid:jid(), undefined | jid:jid()) -> presence().
set_from_to(#iq{} = IQ, F, T) -> IQ#iq{from = F, to = T};
set_from_to(#message{} = Msg, F, T) -> Msg#message{from = F, to = T};
set_from_to(#presence{} = Pres, F, T) -> Pres#presence{from = F, to = T}.

-spec set_error(iq(), error()) -> iq();
	       (message(), error()) -> message();
	       (presence(), error()) -> presence().
set_error(Stanza, E) -> set_subtag(Stanza, E).

-spec set_els(iq(), [xmpp_element() | xmlel()]) -> iq();
	     (message(), [xmpp_element() | xmlel()]) -> message();
	     (presence(), [xmpp_element() | xmlel()]) -> presence().
set_els(#iq{} = IQ, Els) -> IQ#iq{sub_els = Els};
set_els(#message{} = Msg, Els) -> Msg#message{sub_els = Els};
set_els(#presence{} = Pres, Els) -> Pres#presence{sub_els = Els}.

-spec get_ns(xmpp_element() | xmlel()) -> binary().
get_ns(#xmlel{attrs = Attrs}) ->
    fxml:get_attr_s(<<"xmlns">>, Attrs);
get_ns(Pkt) ->
    xmpp_codec:get_ns(Pkt).

-spec get_name(xmpp_element() | xmlel()) -> binary().
get_name(#xmlel{name = Name}) ->
    Name;
get_name(Pkt) ->
    xmpp_codec:get_name(Pkt).

-spec decode(xmlel() | xmpp_element()) -> {ok, xmpp_element()} | {error, any()}.
decode(El) ->
    decode(El, []).

-spec decode(xmlel() | xmpp_element(),
	     [proplists:property()] |
	     fun((xmlel() | xmpp_element()) -> boolean())) ->
		    {ok, xmpp_element()} | {error, any()}.
decode(#xmlel{} = El, MatchFun) when is_function(MatchFun) ->
    Pkt = xmpp_codec:decode(add_ns(El), [ignore_els]),
    decode_els(Pkt, MatchFun);
decode(#xmlel{} = El, Opts) when is_list(Opts) ->
    xmpp_codec:decode(add_ns(El), Opts);
decode(Pkt, _Opts) ->
    Pkt.

-spec decode_els(iq()) -> iq();
		(message()) -> message();
		(presence()) -> presence().
decode_els(Stanza) ->
    decode_els(Stanza, fun xmpp_codec:is_known_tag/1).

-type match_fun() :: fun((xmlel()) -> boolean()).
-spec decode_els(iq(), match_fun()) -> iq();
		(message(), match_fun()) -> message();
		(presence(), match_fun()) -> presence().
decode_els(Stanza, MatchFun) ->
    Els = lists:map(
	    fun(#xmlel{} = El) ->
		    case MatchFun(El) of
			true ->	decode(El);
			false -> El
		    end;
	       (Pkt) ->
		    Pkt
	    end, get_els(Stanza)),
    set_els(Stanza, Els).

-spec encode(xmpp_element() | xmlel()) -> xmlel().
encode(Pkt) ->
    xmpp_codec:encode(Pkt).

format_error(Reason) ->
    xmpp_codec:format_error(Reason).

-spec is_stanza(any()) -> boolean().
is_stanza(#message{}) -> true;
is_stanza(#iq{}) -> true;
is_stanza(#presence{}) -> true;
is_stanza(#xmlel{name = Name}) ->
    (Name == <<"iq">>) or (Name == <<"message">>) or (Name == <<"presence">>);
is_stanza(_) -> false.

-spec set_subtag(iq(), xmpp_element()) -> iq();
		(message(), xmpp_element()) -> message();
		(presence(), xmpp_element()) -> presence().
set_subtag(Stanza, Tag) ->
    TagName = xmpp_codec:get_name(Tag),
    XMLNS = xmpp_codec:get_ns(Tag),
    Els = get_els(Stanza),
    NewEls = set_subtag(Els, Tag, TagName, XMLNS),
    set_els(Stanza, NewEls).

set_subtag([El|Els], Tag, TagName, XMLNS) ->
    case match_tag(El, TagName, XMLNS) of
	true ->
	    [Tag|Els];
	false ->
	    [El|set_subtag(Els, Tag, TagName, XMLNS)]
    end;
set_subtag([], Tag, _, _) ->
    [Tag].

-spec get_subtag(stanza(), xmpp_element()) -> xmpp_element() | false.
get_subtag(Stanza, Tag) ->
    Els = get_els(Stanza),
    TagName = xmpp_codec:get_name(Tag),
    XMLNS = xmpp_codec:get_ns(Tag),
    get_subtag(Els, TagName, XMLNS).

get_subtag([El|Els], TagName, XMLNS) ->
    case match_tag(El, TagName, XMLNS) of
	true ->
	    try
		decode(El)
	    catch _:{xmpp_codec, _Why} ->
		    get_subtag(Els, TagName, XMLNS)
	    end;
	false ->
	    get_subtag(Els, TagName, XMLNS)
    end;
get_subtag([], _, _) ->
    false.

-spec remove_subtag(iq(), xmpp_element()) -> iq();
		   (message(), xmpp_element()) -> message();
		   (presence(), xmpp_element()) -> presence().
remove_subtag(Stanza, Tag) ->
    Els = get_els(Stanza),
    TagName = xmpp_codec:get_name(Tag),
    XMLNS = xmpp_codec:get_ns(Tag),
    NewEls = remove_subtag(Els, TagName, XMLNS),
    set_els(Stanza, NewEls).

remove_subtag([El|Els], TagName, XMLNS) ->
    case match_tag(El, TagName, XMLNS) of
	true ->
	    remove_subtag(Els, TagName, XMLNS);
	false ->
	    [El|remove_subtag(Els, TagName, XMLNS)]
    end;
remove_subtag([], _, _) ->
    [].

-spec has_subtag(stanza(), xmpp_element()) -> boolean().
has_subtag(Stanza, Tag) ->
    Els = get_els(Stanza),
    TagName = xmpp_codec:get_name(Tag),
    XMLNS = xmpp_codec:get_ns(Tag),
    has_subtag(Els, TagName, XMLNS).

has_subtag([El|Els], TagName, XMLNS) ->
    case match_tag(El, TagName, XMLNS) of
	true ->
	    true;
	false ->
	    has_subtag(Els, TagName, XMLNS)
    end;
has_subtag([], _, _) ->
    false.

-spec get_text([text()]) -> binary().
get_text([]) -> <<"">>;
get_text([#text{data = Data}|_]) -> Data.

-spec mk_text(binary()) -> [text()].
mk_text(Text) ->
    mk_text(Text, <<"">>).

-spec mk_text(binary(), binary()) -> [text()].
mk_text(<<"">>, _) ->
    [];
mk_text(Text, Lang) ->
    [#text{lang = Lang,
	   data = translate:translate(Lang, Text)}].

-spec pp(any()) -> iodata().
pp(Term) ->
    xmpp_codec:pp(Term).

%%%===================================================================
%%% Functions to construct general XMPP errors
%%%===================================================================
-spec err_bad_request() -> error().
err_bad_request() ->
    err(modify, 'bad-request', 400).

-spec err_bad_request(binary(), binary()) -> error().
err_bad_request(Text, Lang) ->
    err(modify, 'bad-request', 400, Text, Lang).

-spec err_conflict() -> error().
err_conflict() ->
    err(cancel, 'conflict', 409).

-spec err_conflict(binary(), binary()) -> error().
err_conflict(Text, Lang) ->
    err(cancel, 'conflict', 409, Text, Lang).

-spec err_feature_not_implemented() -> error().
err_feature_not_implemented() ->
    err(cancel, 'feature-not-implemented', 501).

-spec err_feature_not_implemented(binary(), binary()) -> error().
err_feature_not_implemented(Text, Lang) ->
    err(cancel, 'feature-not-implemented', 501, Text, Lang).

-spec err_forbidden() -> error().
err_forbidden() ->
    err(auth, 'forbidden', 403).

-spec err_forbidden(binary(), binary()) -> error().
err_forbidden(Text, Lang) ->
    err(auth, 'forbidden', 403, Text, Lang).

%% RFC 6120 says error type SHOULD be "cancel".
%% RFC 3920 and XEP-0082 says it SHOULD be "modify".
-spec err_gone() -> error().
err_gone() ->
    err(modify, 'gone', 302).

-spec err_gone(binary(), binary()) -> error().
err_gone(Text, Lang) ->
    err(modify, 'gone', 302, Text, Lang).

%% RFC 6120 sasy error type SHOULD be "cancel".
%% RFC 3920 and XEP-0082 says it SHOULD be "wait".
-spec err_internal_server_error() -> error().
err_internal_server_error() ->
    err(wait, 'internal-server-error', 500).

-spec err_internal_server_error(binary(), binary()) -> error().
err_internal_server_error(Text, Lang) ->
    err(wait, 'internal-server-error', 500, Text, Lang).

-spec err_item_not_found() -> error().
err_item_not_found() ->
    err(cancel, 'item-not-found', 404).

-spec err_item_not_found(binary(), binary()) -> error().
err_item_not_found(Text, Lang) ->
    err(cancel, 'item-not-found', 404, Text, Lang).

-spec err_jid_malformed() -> error().
err_jid_malformed() ->
    err(modify, 'jid-malformed', 400).

-spec err_jid_malformed(binary(), binary()) -> error().
err_jid_malformed(Text, Lang) ->
    err(modify, 'jid-malformed', 400, Text, Lang).

-spec err_not_acceptable() -> error().
err_not_acceptable() ->
    err(modify, 'not-acceptable', 406).

-spec err_not_acceptable(binary(), binary()) -> error().
err_not_acceptable(Text, Lang) ->
    err(modify, 'not-acceptable', 406, Text, Lang).

-spec err_not_allowed() -> error().
err_not_allowed() ->
    err(cancel, 'not-allowed', 405).

-spec err_not_allowed(binary(), binary()) -> error().
err_not_allowed(Text, Lang) ->
    err(cancel, 'not-allowed', 405, Text, Lang).

-spec err_not_authorized() -> error().
err_not_authorized() ->
    err(auth, 'not-authorized', 401).

-spec err_not_authorized(binary(), binary()) -> error().
err_not_authorized(Text, Lang) ->
    err(auth, 'not-authorized', 401, Text, Lang).

-spec err_payment_required() -> error().
err_payment_required() ->
    err(auth, 'not-authorized', 402).

-spec err_payment_required(binary(), binary()) -> error().
err_payment_required(Text, Lang) ->
    err(auth, 'not-authorized', 402, Text, Lang).

%% <policy-violation/> is defined in neither RFC 3920 nor XEP-0086.
%% We choose '403' error code (as in <forbidden/>).
-spec err_policy_violation() -> error().
err_policy_violation() ->
    err(modify, 'policy-violation', 403).

-spec err_policy_violation(binary(), binary()) -> error().
err_policy_violation(Text, Lang) ->
    err(modify, 'policy-violation', 403, Text, Lang).

-spec err_recipient_unavailable() -> error().
err_recipient_unavailable() ->
    err(wait, 'recipient-unavailable', 404).

-spec err_recipient_unavailable(binary(), binary()) -> error().
err_recipient_unavailable(Text, Lang) ->
    err(wait, 'recipient-unavailable', 404, Text, Lang).

-spec err_redirect() -> error().
err_redirect() ->
    err(modify, 'redirect', 302).

-spec err_redirect(binary(), binary()) -> error().
err_redirect(Text, Lang) ->
    err(modify, 'redirect', 302, Text, Lang).

-spec err_registration_required() -> error().
err_registration_required() ->
    err(auth, 'registration-required', 407).

-spec err_registration_required(binary(), binary()) -> error().
err_registration_required(Text, Lang) ->
    err(auth, 'registration-required', 407, Text, Lang).

-spec err_remote_server_not_found() -> error().
err_remote_server_not_found() ->
    err(cancel, 'remote-server-not-found', 404).

-spec err_remote_server_not_found(binary(), binary()) -> error().
err_remote_server_not_found(Text, Lang) ->
    err(cancel, 'remote-server-not-found', 404, Text, Lang).

-spec err_remote_server_timeout() -> error().
err_remote_server_timeout() ->
    err(wait, 'remote-server-timeout', 504).

-spec err_remote_server_timeout(binary(), binary()) -> error().
err_remote_server_timeout(Text, Lang) ->
    err(wait, 'remote-server-timeout', 504, Text, Lang).

-spec err_resource_constraint() -> error().
err_resource_constraint() ->
    err(wait, 'resource-constraint', 500).

-spec err_resource_constraint(binary(), binary()) -> error().
err_resource_constraint(Text, Lang) ->
    err(wait, 'resource-constraint', 500, Text, Lang).

-spec err_service_unavailable() -> error().
err_service_unavailable() ->
    err(cancel, 'service-unavailable', 503).

-spec err_service_unavailable(binary(), binary()) -> error().
err_service_unavailable(Text, Lang) ->
    err(cancel, 'service-unavailable', 503, Text, Lang).

-spec err_subscription_required() -> error().
err_subscription_required() ->
    err(auth, 'subscription-required', 407).

-spec err_subscription_required(binary(), binary()) -> error().
err_subscription_required(Text, Lang) ->
    err(auth, 'subscription-required', 407, Text, Lang).

%% No error type is defined for <undefined-confition/>.
%% Let user provide the type.
-spec err_undefined_condition('auth' | 'cancel' | 'continue' |
			      'modify' | 'wait') -> error().
err_undefined_condition(Type) ->
    err(Type, 'undefined-condition', 500).

-spec err_undefined_condition('auth' | 'cancel' | 'continue' | 'modify' | 'wait',
			      binary(), binary()) -> error().
err_undefined_condition(Type, Text, Lang) ->
    err(Type, 'undefined-condition', 500, Text, Lang).

%% RFC 6120 says error type SHOULD be "wait" or "modify".
%% RFC 3920 and XEP-0082 says it SHOULD be "wait".
-spec err_unexpected_request() -> error().
err_unexpected_request() ->
    err(wait, 'unexpected-request', 400).

-spec err_unexpected_request(binary(), binary()) -> error().
err_unexpected_request(Text, Lang) ->
    err(wait, 'unexpected-request', 400, Text, Lang).

%%%===================================================================
%%% Functions to construct stream errors
%%%===================================================================
-spec serr_bad_format() -> stream_error().
serr_bad_format() ->
    serr('bad-format').

-spec serr_bad_format(binary(), binary()) -> stream_error().
serr_bad_format(Text, Lang) ->
    serr('bad-format', Text, Lang).

-spec serr_bad_namespace_prefix() -> stream_error().
serr_bad_namespace_prefix() ->
    serr('bad-namespace-prefix').

-spec serr_bad_namespace_prefix(binary(), binary()) -> stream_error().
serr_bad_namespace_prefix(Text, Lang) ->
    serr('bad-namespace-prefix', Text, Lang).

-spec serr_conflict() -> stream_error().
serr_conflict() ->
    serr('conflict').

-spec serr_conflict(binary(), binary()) -> stream_error().
serr_conflict(Text, Lang) ->
    serr('conflict', Text, Lang).

-spec serr_connection_timeout() -> stream_error().
serr_connection_timeout() ->
    serr('connection-timeout').

-spec serr_connection_timeout(binary(), binary()) -> stream_error().
serr_connection_timeout(Text, Lang) ->
    serr('connection-timeout', Text, Lang).

-spec serr_host_gone() -> stream_error().
serr_host_gone() ->
    serr('host-gone').

-spec serr_host_gone(binary(), binary()) -> stream_error().
serr_host_gone(Text, Lang) ->
    serr('host-gone', Text, Lang).

-spec serr_host_unknown() -> stream_error().
serr_host_unknown() ->
    serr('host-unknown').

-spec serr_host_unknown(binary(), binary()) -> stream_error().
serr_host_unknown(Text, Lang) ->
    serr('host-unknown', Text, Lang).

-spec serr_improper_addressing() -> stream_error().
serr_improper_addressing() ->
    serr('improper-addressing').

-spec serr_improper_addressing(binary(), binary()) -> stream_error().
serr_improper_addressing(Text, Lang) ->
    serr('improper-addressing', Text, Lang).

-spec serr_internal_server_error() -> stream_error().
serr_internal_server_error() ->
    serr('internal-server-error').

-spec serr_internal_server_error(binary(), binary()) -> stream_error().
serr_internal_server_error(Text, Lang) ->
    serr('internal-server-error', Text, Lang).

-spec serr_invalid_from() -> stream_error().
serr_invalid_from() ->
    serr('invalid-from').

-spec serr_invalid_from(binary(), binary()) -> stream_error().
serr_invalid_from(Text, Lang) ->
    serr('invalid-from', Text, Lang).

-spec serr_invalid_id() -> stream_error().
serr_invalid_id() ->
    serr('invalid-id').

-spec serr_invalid_id(binary(), binary()) -> stream_error().
serr_invalid_id(Text, Lang) ->
    serr('invalid-id', Text, Lang).

-spec serr_invalid_namespace() -> stream_error().
serr_invalid_namespace() ->
    serr('invalid-namespace').

-spec serr_invalid_namespace(binary(), binary()) -> stream_error().
serr_invalid_namespace(Text, Lang) ->
    serr('invalid-namespace', Text, Lang).

-spec serr_invalid_xml() -> stream_error().
serr_invalid_xml() ->
    serr('invalid-xml').

-spec serr_invalid_xml(binary(), binary()) -> stream_error().
serr_invalid_xml(Text, Lang) ->
    serr('invalid-xml', Text, Lang).

-spec serr_not_authorized() -> stream_error().
serr_not_authorized() ->
    serr('not-authorized').

-spec serr_not_authorized(binary(), binary()) -> stream_error().
serr_not_authorized(Text, Lang) ->
    serr('not-authorized', Text, Lang).

-spec serr_not_well_formed() -> stream_error().
serr_not_well_formed() ->
    serr('not-well-formed').

-spec serr_not_well_formed(binary(), binary()) -> stream_error().
serr_not_well_formed(Text, Lang) ->
    serr('not-well-formed', Text, Lang).

-spec serr_policy_violation() -> stream_error().
serr_policy_violation() ->
    serr('policy-violation').

-spec serr_policy_violation(binary(), binary()) -> stream_error().
serr_policy_violation(Text, Lang) ->
    serr('policy-violation', Text, Lang).

-spec serr_remote_connection_failed() -> stream_error().
serr_remote_connection_failed() ->
    serr('remote-connection-failed').

-spec serr_remote_connection_failed(binary(), binary()) -> stream_error().
serr_remote_connection_failed(Text, Lang) ->
    serr('remote-connection-failed', Text, Lang).

-spec serr_reset() -> stream_error().
serr_reset() ->
    serr('reset').

-spec serr_reset(binary(), binary()) -> stream_error().
serr_reset(Text, Lang) ->
    serr('reset', Text, Lang).

-spec serr_resource_constraint() -> stream_error().
serr_resource_constraint() ->
    serr('resource-constraint').

-spec serr_resource_constraint(binary(), binary()) -> stream_error().
serr_resource_constraint(Text, Lang) ->
    serr('resource-constraint', Text, Lang).

-spec serr_restricted_xml() -> stream_error().
serr_restricted_xml() ->
    serr('restricted-xml').

-spec serr_restricted_xml(binary(), binary()) -> stream_error().
serr_restricted_xml(Text, Lang) ->
    serr('restricted-xml', Text, Lang).

-spec serr_see_other_host() -> stream_error().
serr_see_other_host() ->
    serr('see-other-host').

-spec serr_see_other_host(binary(), binary()) -> stream_error().
serr_see_other_host(Text, Lang) ->
    serr('see-other-host', Text, Lang).

-spec serr_system_shutdown() -> stream_error().
serr_system_shutdown() ->
    serr('system-shutdown').

-spec serr_system_shutdown(binary(), binary()) -> stream_error().
serr_system_shutdown(Text, Lang) ->
    serr('system-shutdown', Text, Lang).

-spec serr_undefined_condition() -> stream_error().
serr_undefined_condition() ->
    serr('undefined-condition').

-spec serr_undefined_condition(binary(), binary()) -> stream_error().
serr_undefined_condition(Text, Lang) ->
    serr('undefined-condition', Text, Lang).

-spec serr_unsupported_encoding() -> stream_error().
serr_unsupported_encoding() ->
    serr('unsupported-encoding').

-spec serr_unsupported_encoding(binary(), binary()) -> stream_error().
serr_unsupported_encoding(Text, Lang) ->
    serr('unsupported-encoding', Text, Lang).

-spec serr_unsupported_stanza_type() -> stream_error().
serr_unsupported_stanza_type() ->
    serr('unsupported-stanza-type').

-spec serr_unsupported_stanza_type(binary(), binary()) -> stream_error().
serr_unsupported_stanza_type(Text, Lang) ->
    serr('unsupported-stanza-type', Text, Lang).

-spec serr_unsupported_version() -> stream_error().
serr_unsupported_version() ->
    serr('unsupported-version').

-spec serr_unsupported_version(binary(), binary()) -> stream_error().
serr_unsupported_version(Text, Lang) ->
    serr('unsupported-version', Text, Lang).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec err('auth' | 'cancel' | 'continue' | 'modify' | 'wait',
	  atom() | gone() | redirect(), non_neg_integer()) -> error().
err(Type, Reason, Code) ->
    #error{type = Type, reason = Reason, code = Code}.

-spec err('auth' | 'cancel' | 'continue' | 'modify' | 'wait',
	  atom() | gone() | redirect(), non_neg_integer(),
	  binary(), binary()) -> error().
err(Type, Reason, Code, Text, Lang) ->
    #error{type = Type, reason = Reason, code = Code,
	   text = #text{lang = Lang,
			data = translate:translate(Lang, Text)}}.

-spec serr(atom() | 'see-other-host'()) -> stream_error().
serr(Reason) ->
    #stream_error{reason = Reason}.

-spec serr(atom() | 'see-other-host'(), binary(),
	   binary()) -> stream_error().
serr(Reason, Text, Lang) ->
    #stream_error{reason = Reason,
		  text = #text{lang = Lang,
			       data = translate:translate(Lang, Text)}}.

-spec add_ns(xmlel()) -> xmlel().
add_ns(#xmlel{name = Name} = El) when Name == <<"message">>;
				      Name == <<"presence">>;
				      Name == <<"iq">> ->
    Attrs = lists:keystore(<<"xmlns">>, 1, El#xmlel.attrs,
			   {<<"xmlns">>, ?NS_CLIENT}),
    El#xmlel{attrs = Attrs};
add_ns(El) ->
    El.

-spec match_tag(xmlel() | xmpp_element(), binary(), binary()) -> boolean().
match_tag(El, TagName, XMLNS) ->
    get_name(El) == TagName andalso get_ns(El) == XMLNS.
