%% Created automatically by XML generator (xml_gen.erl)
%% Source: xmpp_codec.spec

-module(xmpp_codec).

-compile({nowarn_unused_function,
	  [{dec_int, 3}, {dec_int, 1}, {dec_enum, 2},
	   {enc_int, 1}, {get_attr, 2}, {enc_enum, 1}]}).

-export([pp/1, format_error/1, decode/1, is_known_tag/1,
	 encode/1]).

decode({xmlel, _name, _attrs, _} = _el) ->
    case {_name, get_attr(<<"xmlns">>, _attrs)} of
      {<<"x">>, <<"http://jabber.org/protocol/muc">>} ->
	  decode_muc(_el);
      {<<"query">>,
       <<"http://jabber.org/protocol/muc#owner">>} ->
	  decode_muc_owner(_el);
      {<<"destroy">>,
       <<"http://jabber.org/protocol/muc#owner">>} ->
	  decode_muc_owner_destroy(_el);
      {<<"reason">>,
       <<"http://jabber.org/protocol/muc#owner">>} ->
	  decode_muc_owner_reason(_el);
      {<<"password">>,
       <<"http://jabber.org/protocol/muc#owner">>} ->
	  decode_muc_owner_password(_el);
      {<<"x">>, <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user(_el);
      {<<"item">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_item(_el);
      {<<"status">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_status(_el);
      {<<"continue">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_continue(_el);
      {<<"actor">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_actor(_el);
      {<<"invite">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_invite(_el);
      {<<"destroy">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_destroy(_el);
      {<<"decline">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_decline(_el);
      {<<"reason">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_reason(_el);
      {<<"history">>, <<"http://jabber.org/protocol/muc">>} ->
	  decode_muc_history(_el);
      {<<"query">>,
       <<"http://jabber.org/protocol/bytestreams">>} ->
	  decode_bytestreams(_el);
      {<<"activate">>,
       <<"http://jabber.org/protocol/bytestreams">>} ->
	  decode_bytestreams_activate(_el);
      {<<"streamhost-used">>,
       <<"http://jabber.org/protocol/bytestreams">>} ->
	  decode_bytestreams_streamhost_used(_el);
      {<<"streamhost">>,
       <<"http://jabber.org/protocol/bytestreams">>} ->
	  decode_bytestreams_streamhost(_el);
      {<<"x">>, <<"jabber:x:delay">>} ->
	  decode_legacy_delay(_el);
      {<<"delay">>, <<"urn:xmpp:delay">>} ->
	  decode_delay(_el);
      {<<"headers">>,
       <<"http://jabber.org/protocol/shim">>} ->
	  decode_shim_headers(_el);
      {<<"header">>, <<"http://jabber.org/protocol/shim">>} ->
	  decode_shim_header(_el);
      {<<"pubsub">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub(_el);
      {<<"retract">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_retract(_el);
      {<<"options">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_options(_el);
      {<<"publish">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_publish(_el);
      {<<"unsubscribe">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_unsubscribe(_el);
      {<<"subscribe">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_subscribe(_el);
      {<<"affiliations">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_affiliations(_el);
      {<<"subscriptions">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_subscriptions(_el);
      {<<"event">>,
       <<"http://jabber.org/protocol/pubsub#event">>} ->
	  decode_pubsub_event(_el);
      {<<"items">>,
       <<"http://jabber.org/protocol/pubsub#event">>} ->
	  decode_pubsub_event_items(_el);
      {<<"item">>,
       <<"http://jabber.org/protocol/pubsub#event">>} ->
	  decode_pubsub_event_item(_el);
      {<<"retract">>,
       <<"http://jabber.org/protocol/pubsub#event">>} ->
	  decode_pubsub_event_retract(_el);
      {<<"items">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_items(_el);
      {<<"item">>, <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_item(_el);
      {<<"affiliation">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_affiliation(_el);
      {<<"subscription">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_subscription(_el);
      {<<"x">>, <<"jabber:x:data">>} -> decode_xdata(_el);
      {<<"item">>, <<"jabber:x:data">>} ->
	  decode_xdata_item(_el);
      {<<"reported">>, <<"jabber:x:data">>} ->
	  decode_xdata_reported(_el);
      {<<"title">>, <<"jabber:x:data">>} ->
	  decode_xdata_title(_el);
      {<<"instructions">>, <<"jabber:x:data">>} ->
	  decode_xdata_instructions(_el);
      {<<"field">>, <<"jabber:x:data">>} ->
	  decode_xdata_field(_el);
      {<<"option">>, <<"jabber:x:data">>} ->
	  decode_xdata_field_option(_el);
      {<<"value">>, <<"jabber:x:data">>} ->
	  decode_xdata_field_value(_el);
      {<<"desc">>, <<"jabber:x:data">>} ->
	  decode_xdata_field_desc(_el);
      {<<"required">>, <<"jabber:x:data">>} ->
	  decode_xdata_field_required(_el);
      {<<"vCard">>, <<"vcard-temp">>} -> decode_vcard(_el);
      {<<"CLASS">>, <<"vcard-temp">>} ->
	  decode_vcard_CLASS(_el);
      {<<"CATEGORIES">>, <<"vcard-temp">>} ->
	  decode_vcard_CATEGORIES(_el);
      {<<"KEY">>, <<"vcard-temp">>} -> decode_vcard_KEY(_el);
      {<<"SOUND">>, <<"vcard-temp">>} ->
	  decode_vcard_SOUND(_el);
      {<<"ORG">>, <<"vcard-temp">>} -> decode_vcard_ORG(_el);
      {<<"PHOTO">>, <<"vcard-temp">>} ->
	  decode_vcard_PHOTO(_el);
      {<<"LOGO">>, <<"vcard-temp">>} ->
	  decode_vcard_LOGO(_el);
      {<<"BINVAL">>, <<"vcard-temp">>} ->
	  decode_vcard_BINVAL(_el);
      {<<"GEO">>, <<"vcard-temp">>} -> decode_vcard_GEO(_el);
      {<<"EMAIL">>, <<"vcard-temp">>} ->
	  decode_vcard_EMAIL(_el);
      {<<"TEL">>, <<"vcard-temp">>} -> decode_vcard_TEL(_el);
      {<<"LABEL">>, <<"vcard-temp">>} ->
	  decode_vcard_LABEL(_el);
      {<<"ADR">>, <<"vcard-temp">>} -> decode_vcard_ADR(_el);
      {<<"N">>, <<"vcard-temp">>} -> decode_vcard_N(_el);
      {<<"CONFIDENTIAL">>, <<"vcard-temp">>} ->
	  decode_vcard_CONFIDENTIAL(_el);
      {<<"PRIVATE">>, <<"vcard-temp">>} ->
	  decode_vcard_PRIVATE(_el);
      {<<"PUBLIC">>, <<"vcard-temp">>} ->
	  decode_vcard_PUBLIC(_el);
      {<<"EXTVAL">>, <<"vcard-temp">>} ->
	  decode_vcard_EXTVAL(_el);
      {<<"TYPE">>, <<"vcard-temp">>} ->
	  decode_vcard_TYPE(_el);
      {<<"DESC">>, <<"vcard-temp">>} ->
	  decode_vcard_DESC(_el);
      {<<"URL">>, <<"vcard-temp">>} -> decode_vcard_URL(_el);
      {<<"UID">>, <<"vcard-temp">>} -> decode_vcard_UID(_el);
      {<<"SORT-STRING">>, <<"vcard-temp">>} ->
	  decode_vcard_SORT_STRING(_el);
      {<<"REV">>, <<"vcard-temp">>} -> decode_vcard_REV(_el);
      {<<"PRODID">>, <<"vcard-temp">>} ->
	  decode_vcard_PRODID(_el);
      {<<"NOTE">>, <<"vcard-temp">>} ->
	  decode_vcard_NOTE(_el);
      {<<"KEYWORD">>, <<"vcard-temp">>} ->
	  decode_vcard_KEYWORD(_el);
      {<<"ROLE">>, <<"vcard-temp">>} ->
	  decode_vcard_ROLE(_el);
      {<<"TITLE">>, <<"vcard-temp">>} ->
	  decode_vcard_TITLE(_el);
      {<<"TZ">>, <<"vcard-temp">>} -> decode_vcard_TZ(_el);
      {<<"MAILER">>, <<"vcard-temp">>} ->
	  decode_vcard_MAILER(_el);
      {<<"JABBERID">>, <<"vcard-temp">>} ->
	  decode_vcard_JABBERID(_el);
      {<<"BDAY">>, <<"vcard-temp">>} ->
	  decode_vcard_BDAY(_el);
      {<<"NICKNAME">>, <<"vcard-temp">>} ->
	  decode_vcard_NICKNAME(_el);
      {<<"FN">>, <<"vcard-temp">>} -> decode_vcard_FN(_el);
      {<<"VERSION">>, <<"vcard-temp">>} ->
	  decode_vcard_VERSION(_el);
      {<<"CRED">>, <<"vcard-temp">>} ->
	  decode_vcard_CRED(_el);
      {<<"PHONETIC">>, <<"vcard-temp">>} ->
	  decode_vcard_PHONETIC(_el);
      {<<"ORGUNIT">>, <<"vcard-temp">>} ->
	  decode_vcard_ORGUNIT(_el);
      {<<"ORGNAME">>, <<"vcard-temp">>} ->
	  decode_vcard_ORGNAME(_el);
      {<<"LON">>, <<"vcard-temp">>} -> decode_vcard_LON(_el);
      {<<"LAT">>, <<"vcard-temp">>} -> decode_vcard_LAT(_el);
      {<<"USERID">>, <<"vcard-temp">>} ->
	  decode_vcard_USERID(_el);
      {<<"NUMBER">>, <<"vcard-temp">>} ->
	  decode_vcard_NUMBER(_el);
      {<<"LINE">>, <<"vcard-temp">>} ->
	  decode_vcard_LINE(_el);
      {<<"CTRY">>, <<"vcard-temp">>} ->
	  decode_vcard_CTRY(_el);
      {<<"PCODE">>, <<"vcard-temp">>} ->
	  decode_vcard_PCODE(_el);
      {<<"REGION">>, <<"vcard-temp">>} ->
	  decode_vcard_REGION(_el);
      {<<"LOCALITY">>, <<"vcard-temp">>} ->
	  decode_vcard_LOCALITY(_el);
      {<<"STREET">>, <<"vcard-temp">>} ->
	  decode_vcard_STREET(_el);
      {<<"EXTADD">>, <<"vcard-temp">>} ->
	  decode_vcard_EXTADD(_el);
      {<<"POBOX">>, <<"vcard-temp">>} ->
	  decode_vcard_POBOX(_el);
      {<<"SUFFIX">>, <<"vcard-temp">>} ->
	  decode_vcard_SUFFIX(_el);
      {<<"PREFIX">>, <<"vcard-temp">>} ->
	  decode_vcard_PREFIX(_el);
      {<<"MIDDLE">>, <<"vcard-temp">>} ->
	  decode_vcard_MIDDLE(_el);
      {<<"GIVEN">>, <<"vcard-temp">>} ->
	  decode_vcard_GIVEN(_el);
      {<<"FAMILY">>, <<"vcard-temp">>} ->
	  decode_vcard_FAMILY(_el);
      {<<"X400">>, <<"vcard-temp">>} ->
	  decode_vcard_X400(_el);
      {<<"INTERNET">>, <<"vcard-temp">>} ->
	  decode_vcard_INTERNET(_el);
      {<<"PREF">>, <<"vcard-temp">>} ->
	  decode_vcard_PREF(_el);
      {<<"INTL">>, <<"vcard-temp">>} ->
	  decode_vcard_INTL(_el);
      {<<"DOM">>, <<"vcard-temp">>} -> decode_vcard_DOM(_el);
      {<<"PARCEL">>, <<"vcard-temp">>} ->
	  decode_vcard_PARCEL(_el);
      {<<"POSTAL">>, <<"vcard-temp">>} ->
	  decode_vcard_POSTAL(_el);
      {<<"PCS">>, <<"vcard-temp">>} -> decode_vcard_PCS(_el);
      {<<"ISDN">>, <<"vcard-temp">>} ->
	  decode_vcard_ISDN(_el);
      {<<"MODEM">>, <<"vcard-temp">>} ->
	  decode_vcard_MODEM(_el);
      {<<"BBS">>, <<"vcard-temp">>} -> decode_vcard_BBS(_el);
      {<<"VIDEO">>, <<"vcard-temp">>} ->
	  decode_vcard_VIDEO(_el);
      {<<"CELL">>, <<"vcard-temp">>} ->
	  decode_vcard_CELL(_el);
      {<<"MSG">>, <<"vcard-temp">>} -> decode_vcard_MSG(_el);
      {<<"PAGER">>, <<"vcard-temp">>} ->
	  decode_vcard_PAGER(_el);
      {<<"FAX">>, <<"vcard-temp">>} -> decode_vcard_FAX(_el);
      {<<"VOICE">>, <<"vcard-temp">>} ->
	  decode_vcard_VOICE(_el);
      {<<"WORK">>, <<"vcard-temp">>} ->
	  decode_vcard_WORK(_el);
      {<<"HOME">>, <<"vcard-temp">>} ->
	  decode_vcard_HOME(_el);
      {<<"stream:error">>,
       <<"http://etherx.jabber.org/streams">>} ->
	  decode_stream_error(_el);
      {<<"unsupported-version">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_unsupported_version(_el);
      {<<"unsupported-stanza-type">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_unsupported_stanza_type(_el);
      {<<"unsupported-encoding">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_unsupported_encoding(_el);
      {<<"undefined-condition">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_undefined_condition(_el);
      {<<"system-shutdown">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_system_shutdown(_el);
      {<<"see-other-host">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_see_other_host(_el);
      {<<"restricted-xml">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_restricted_xml(_el);
      {<<"resource-constraint">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_resource_constraint(_el);
      {<<"reset">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_reset(_el);
      {<<"remote-connection-failed">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_remote_connection_failed(_el);
      {<<"policy-violation">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_policy_violation(_el);
      {<<"not-well-formed">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_not_well_formed(_el);
      {<<"not-authorized">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_not_authorized(_el);
      {<<"invalid-xml">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_invalid_xml(_el);
      {<<"invalid-namespace">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_invalid_namespace(_el);
      {<<"invalid-id">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_invalid_id(_el);
      {<<"invalid-from">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_invalid_from(_el);
      {<<"internal-server-error">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_internal_server_error(_el);
      {<<"improper-addressing">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_improper_addressing(_el);
      {<<"host-unknown">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_host_unknown(_el);
      {<<"host-gone">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_host_gone(_el);
      {<<"connection-timeout">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_connection_timeout(_el);
      {<<"conflict">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_conflict(_el);
      {<<"bad-namespace-prefix">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_bad_namespace_prefix(_el);
      {<<"bad-format">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_bad_format(_el);
      {<<"text">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_text(_el);
      {<<"time">>, <<"urn:xmpp:time">>} -> decode_time(_el);
      {<<"tzo">>, <<"urn:xmpp:time">>} ->
	  decode_time_tzo(_el);
      {<<"utc">>, <<"urn:xmpp:time">>} ->
	  decode_time_utc(_el);
      {<<"ping">>, <<"urn:xmpp:ping">>} -> decode_ping(_el);
      {<<"session">>,
       <<"urn:ietf:params:xml:ns:xmpp-session">>} ->
	  decode_session(_el);
      {<<"query">>, <<"jabber:iq:register">>} ->
	  decode_register(_el);
      {<<"key">>, <<"jabber:iq:register">>} ->
	  decode_register_key(_el);
      {<<"text">>, <<"jabber:iq:register">>} ->
	  decode_register_text(_el);
      {<<"misc">>, <<"jabber:iq:register">>} ->
	  decode_register_misc(_el);
      {<<"date">>, <<"jabber:iq:register">>} ->
	  decode_register_date(_el);
      {<<"url">>, <<"jabber:iq:register">>} ->
	  decode_register_url(_el);
      {<<"phone">>, <<"jabber:iq:register">>} ->
	  decode_register_phone(_el);
      {<<"zip">>, <<"jabber:iq:register">>} ->
	  decode_register_zip(_el);
      {<<"state">>, <<"jabber:iq:register">>} ->
	  decode_register_state(_el);
      {<<"city">>, <<"jabber:iq:register">>} ->
	  decode_register_city(_el);
      {<<"address">>, <<"jabber:iq:register">>} ->
	  decode_register_address(_el);
      {<<"email">>, <<"jabber:iq:register">>} ->
	  decode_register_email(_el);
      {<<"last">>, <<"jabber:iq:register">>} ->
	  decode_register_last(_el);
      {<<"first">>, <<"jabber:iq:register">>} ->
	  decode_register_first(_el);
      {<<"name">>, <<"jabber:iq:register">>} ->
	  decode_register_name(_el);
      {<<"password">>, <<"jabber:iq:register">>} ->
	  decode_register_password(_el);
      {<<"nick">>, <<"jabber:iq:register">>} ->
	  decode_register_nick(_el);
      {<<"username">>, <<"jabber:iq:register">>} ->
	  decode_register_username(_el);
      {<<"instructions">>, <<"jabber:iq:register">>} ->
	  decode_register_instructions(_el);
      {<<"remove">>, <<"jabber:iq:register">>} ->
	  decode_register_remove(_el);
      {<<"registered">>, <<"jabber:iq:register">>} ->
	  decode_register_registered(_el);
      {<<"register">>,
       <<"http://jabber.org/features/iq-register">>} ->
	  decode_feature_register(_el);
      {<<"c">>, <<"http://jabber.org/protocol/caps">>} ->
	  decode_caps(_el);
      {<<"ack">>, <<"p1:ack">>} -> decode_p1_ack(_el);
      {<<"rebind">>, <<"p1:rebind">>} ->
	  decode_p1_rebind(_el);
      {<<"push">>, <<"p1:push">>} -> decode_p1_push(_el);
      {<<"stream:features">>,
       <<"http://etherx.jabber.org/streams">>} ->
	  decode_stream_features(_el);
      {<<"compression">>,
       <<"http://jabber.org/features/compress">>} ->
	  decode_compression(_el);
      {<<"method">>,
       <<"http://jabber.org/features/compress">>} ->
	  decode_compression_method(_el);
      {<<"compressed">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  decode_compressed(_el);
      {<<"compress">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  decode_compress(_el);
      {<<"method">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  decode_compress_method(_el);
      {<<"failure">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  decode_compress_failure(_el);
      {<<"unsupported-method">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  decode_compress_failure_unsupported_method(_el);
      {<<"processing-failed">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  decode_compress_failure_processing_failed(_el);
      {<<"setup-failed">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  decode_compress_failure_setup_failed(_el);
      {<<"failure">>,
       <<"urn:ietf:params:xml:ns:xmpp-tls">>} ->
	  decode_starttls_failure(_el);
      {<<"proceed">>,
       <<"urn:ietf:params:xml:ns:xmpp-tls">>} ->
	  decode_starttls_proceed(_el);
      {<<"starttls">>,
       <<"urn:ietf:params:xml:ns:xmpp-tls">>} ->
	  decode_starttls(_el);
      {<<"required">>,
       <<"urn:ietf:params:xml:ns:xmpp-tls">>} ->
	  decode_starttls_required(_el);
      {<<"mechanisms">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_mechanisms(_el);
      {<<"mechanism">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_mechanism(_el);
      {<<"failure">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure(_el);
      {<<"temporary-auth-failure">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_temporary_auth_failure(_el);
      {<<"not-authorized">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_not_authorized(_el);
      {<<"mechanism-too-weak">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_mechanism_too_weak(_el);
      {<<"malformed-request">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_malformed_request(_el);
      {<<"invalid-mechanism">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_invalid_mechanism(_el);
      {<<"invalid-authzid">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_invalid_authzid(_el);
      {<<"incorrect-encoding">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_incorrect_encoding(_el);
      {<<"encryption-required">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_encryption_required(_el);
      {<<"credentials-expired">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_credentials_expired(_el);
      {<<"account-disabled">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_account_disabled(_el);
      {<<"aborted">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_aborted(_el);
      {<<"text">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_text(_el);
      {<<"success">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_success(_el);
      {<<"response">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_response(_el);
      {<<"challenge">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_challenge(_el);
      {<<"abort">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_abort(_el);
      {<<"auth">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_auth(_el);
      {<<"bind">>, <<"urn:ietf:params:xml:ns:xmpp-bind">>} ->
	  decode_bind(_el);
      {<<"resource">>,
       <<"urn:ietf:params:xml:ns:xmpp-bind">>} ->
	  decode_bind_resource(_el);
      {<<"jid">>, <<"urn:ietf:params:xml:ns:xmpp-bind">>} ->
	  decode_bind_jid(_el);
      {<<"error">>, <<"jabber:client">>} -> decode_error(_el);
      {<<"text">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_text(_el);
      {<<"unexpected-request">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_unexpected_request(_el);
      {<<"undefined-condition">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_undefined_condition(_el);
      {<<"subscription-required">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_subscription_required(_el);
      {<<"service-unavailable">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_service_unavailable(_el);
      {<<"resource-constraint">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_resource_constraint(_el);
      {<<"remote-server-timeout">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_remote_server_timeout(_el);
      {<<"remote-server-not-found">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_remote_server_not_found(_el);
      {<<"registration-required">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_registration_required(_el);
      {<<"redirect">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_redirect(_el);
      {<<"recipient-unavailable">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_recipient_unavailable(_el);
      {<<"policy-violation">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_policy_violation(_el);
      {<<"not-authorized">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_not_authorized(_el);
      {<<"not-allowed">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_not_allowed(_el);
      {<<"not-acceptable">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_not_acceptable(_el);
      {<<"jid-malformed">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_jid_malformed(_el);
      {<<"item-not-found">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_item_not_found(_el);
      {<<"internal-server-error">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_internal_server_error(_el);
      {<<"gone">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_gone(_el);
      {<<"forbidden">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_forbidden(_el);
      {<<"feature-not-implemented">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_feature_not_implemented(_el);
      {<<"conflict">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_conflict(_el);
      {<<"bad-request">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_bad_request(_el);
      {<<"presence">>, <<"jabber:client">>} ->
	  decode_presence(_el);
      {<<"priority">>, <<"jabber:client">>} ->
	  decode_presence_priority(_el);
      {<<"status">>, <<"jabber:client">>} ->
	  decode_presence_status(_el);
      {<<"show">>, <<"jabber:client">>} ->
	  decode_presence_show(_el);
      {<<"message">>, <<"jabber:client">>} ->
	  decode_message(_el);
      {<<"thread">>, <<"jabber:client">>} ->
	  decode_message_thread(_el);
      {<<"body">>, <<"jabber:client">>} ->
	  decode_message_body(_el);
      {<<"subject">>, <<"jabber:client">>} ->
	  decode_message_subject(_el);
      {<<"iq">>, <<"jabber:client">>} -> decode_iq(_el);
      {<<"query">>, <<"http://jabber.org/protocol/stats">>} ->
	  decode_stats(_el);
      {<<"stat">>, <<"http://jabber.org/protocol/stats">>} ->
	  decode_stat(_el);
      {<<"error">>, <<"http://jabber.org/protocol/stats">>} ->
	  decode_stat_error(_el);
      {<<"storage">>, <<"storage:bookmarks">>} ->
	  decode_bookmarks_storage(_el);
      {<<"url">>, <<"storage:bookmarks">>} ->
	  decode_bookmark_url(_el);
      {<<"conference">>, <<"storage:bookmarks">>} ->
	  decode_bookmark_conference(_el);
      {<<"password">>, <<"storage:bookmarks">>} ->
	  decode_conference_password(_el);
      {<<"nick">>, <<"storage:bookmarks">>} ->
	  decode_conference_nick(_el);
      {<<"query">>, <<"jabber:iq:private">>} ->
	  decode_private(_el);
      {<<"query">>,
       <<"http://jabber.org/protocol/disco#items">>} ->
	  decode_disco_items(_el);
      {<<"item">>,
       <<"http://jabber.org/protocol/disco#items">>} ->
	  decode_disco_item(_el);
      {<<"query">>,
       <<"http://jabber.org/protocol/disco#info">>} ->
	  decode_disco_info(_el);
      {<<"feature">>,
       <<"http://jabber.org/protocol/disco#info">>} ->
	  decode_disco_feature(_el);
      {<<"identity">>,
       <<"http://jabber.org/protocol/disco#info">>} ->
	  decode_disco_identity(_el);
      {<<"blocklist">>, <<"urn:xmpp:blocking">>} ->
	  decode_block_list(_el);
      {<<"unblock">>, <<"urn:xmpp:blocking">>} ->
	  decode_unblock(_el);
      {<<"block">>, <<"urn:xmpp:blocking">>} ->
	  decode_block(_el);
      {<<"item">>, <<"urn:xmpp:blocking">>} ->
	  decode_block_item(_el);
      {<<"query">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy(_el);
      {<<"active">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_active_list(_el);
      {<<"default">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_default_list(_el);
      {<<"list">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_list(_el);
      {<<"item">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_item(_el);
      {<<"presence-out">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_presence_out(_el);
      {<<"presence-in">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_presence_in(_el);
      {<<"iq">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_iq(_el);
      {<<"message">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_message(_el);
      {<<"query">>, <<"jabber:iq:roster">>} ->
	  decode_roster(_el);
      {<<"item">>, <<"jabber:iq:roster">>} ->
	  decode_roster_item(_el);
      {<<"group">>, <<"jabber:iq:roster">>} ->
	  decode_roster_group(_el);
      {<<"query">>, <<"jabber:iq:version">>} ->
	  decode_version(_el);
      {<<"os">>, <<"jabber:iq:version">>} ->
	  decode_version_os(_el);
      {<<"version">>, <<"jabber:iq:version">>} ->
	  decode_version_ver(_el);
      {<<"name">>, <<"jabber:iq:version">>} ->
	  decode_version_name(_el);
      {<<"query">>, <<"jabber:iq:last">>} -> decode_last(_el);
      {_name, _xmlns} ->
	  erlang:error({xmpp_codec, {unknown_tag, _name, _xmlns}})
    end.

is_known_tag({xmlel, _name, _attrs, _} = _el) ->
    case {_name, get_attr(<<"xmlns">>, _attrs)} of
      {<<"x">>, <<"http://jabber.org/protocol/muc">>} -> true;
      {<<"query">>,
       <<"http://jabber.org/protocol/muc#owner">>} ->
	  true;
      {<<"destroy">>,
       <<"http://jabber.org/protocol/muc#owner">>} ->
	  true;
      {<<"reason">>,
       <<"http://jabber.org/protocol/muc#owner">>} ->
	  true;
      {<<"password">>,
       <<"http://jabber.org/protocol/muc#owner">>} ->
	  true;
      {<<"x">>, <<"http://jabber.org/protocol/muc#user">>} ->
	  true;
      {<<"item">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  true;
      {<<"status">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  true;
      {<<"continue">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  true;
      {<<"actor">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  true;
      {<<"invite">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  true;
      {<<"destroy">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  true;
      {<<"decline">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  true;
      {<<"reason">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  true;
      {<<"history">>, <<"http://jabber.org/protocol/muc">>} ->
	  true;
      {<<"query">>,
       <<"http://jabber.org/protocol/bytestreams">>} ->
	  true;
      {<<"activate">>,
       <<"http://jabber.org/protocol/bytestreams">>} ->
	  true;
      {<<"streamhost-used">>,
       <<"http://jabber.org/protocol/bytestreams">>} ->
	  true;
      {<<"streamhost">>,
       <<"http://jabber.org/protocol/bytestreams">>} ->
	  true;
      {<<"x">>, <<"jabber:x:delay">>} -> true;
      {<<"delay">>, <<"urn:xmpp:delay">>} -> true;
      {<<"headers">>,
       <<"http://jabber.org/protocol/shim">>} ->
	  true;
      {<<"header">>, <<"http://jabber.org/protocol/shim">>} ->
	  true;
      {<<"pubsub">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  true;
      {<<"retract">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  true;
      {<<"options">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  true;
      {<<"publish">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  true;
      {<<"unsubscribe">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  true;
      {<<"subscribe">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  true;
      {<<"affiliations">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  true;
      {<<"subscriptions">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  true;
      {<<"event">>,
       <<"http://jabber.org/protocol/pubsub#event">>} ->
	  true;
      {<<"items">>,
       <<"http://jabber.org/protocol/pubsub#event">>} ->
	  true;
      {<<"item">>,
       <<"http://jabber.org/protocol/pubsub#event">>} ->
	  true;
      {<<"retract">>,
       <<"http://jabber.org/protocol/pubsub#event">>} ->
	  true;
      {<<"items">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  true;
      {<<"item">>, <<"http://jabber.org/protocol/pubsub">>} ->
	  true;
      {<<"affiliation">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  true;
      {<<"subscription">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  true;
      {<<"x">>, <<"jabber:x:data">>} -> true;
      {<<"item">>, <<"jabber:x:data">>} -> true;
      {<<"reported">>, <<"jabber:x:data">>} -> true;
      {<<"title">>, <<"jabber:x:data">>} -> true;
      {<<"instructions">>, <<"jabber:x:data">>} -> true;
      {<<"field">>, <<"jabber:x:data">>} -> true;
      {<<"option">>, <<"jabber:x:data">>} -> true;
      {<<"value">>, <<"jabber:x:data">>} -> true;
      {<<"desc">>, <<"jabber:x:data">>} -> true;
      {<<"required">>, <<"jabber:x:data">>} -> true;
      {<<"vCard">>, <<"vcard-temp">>} -> true;
      {<<"CLASS">>, <<"vcard-temp">>} -> true;
      {<<"CATEGORIES">>, <<"vcard-temp">>} -> true;
      {<<"KEY">>, <<"vcard-temp">>} -> true;
      {<<"SOUND">>, <<"vcard-temp">>} -> true;
      {<<"ORG">>, <<"vcard-temp">>} -> true;
      {<<"PHOTO">>, <<"vcard-temp">>} -> true;
      {<<"LOGO">>, <<"vcard-temp">>} -> true;
      {<<"BINVAL">>, <<"vcard-temp">>} -> true;
      {<<"GEO">>, <<"vcard-temp">>} -> true;
      {<<"EMAIL">>, <<"vcard-temp">>} -> true;
      {<<"TEL">>, <<"vcard-temp">>} -> true;
      {<<"LABEL">>, <<"vcard-temp">>} -> true;
      {<<"ADR">>, <<"vcard-temp">>} -> true;
      {<<"N">>, <<"vcard-temp">>} -> true;
      {<<"CONFIDENTIAL">>, <<"vcard-temp">>} -> true;
      {<<"PRIVATE">>, <<"vcard-temp">>} -> true;
      {<<"PUBLIC">>, <<"vcard-temp">>} -> true;
      {<<"EXTVAL">>, <<"vcard-temp">>} -> true;
      {<<"TYPE">>, <<"vcard-temp">>} -> true;
      {<<"DESC">>, <<"vcard-temp">>} -> true;
      {<<"URL">>, <<"vcard-temp">>} -> true;
      {<<"UID">>, <<"vcard-temp">>} -> true;
      {<<"SORT-STRING">>, <<"vcard-temp">>} -> true;
      {<<"REV">>, <<"vcard-temp">>} -> true;
      {<<"PRODID">>, <<"vcard-temp">>} -> true;
      {<<"NOTE">>, <<"vcard-temp">>} -> true;
      {<<"KEYWORD">>, <<"vcard-temp">>} -> true;
      {<<"ROLE">>, <<"vcard-temp">>} -> true;
      {<<"TITLE">>, <<"vcard-temp">>} -> true;
      {<<"TZ">>, <<"vcard-temp">>} -> true;
      {<<"MAILER">>, <<"vcard-temp">>} -> true;
      {<<"JABBERID">>, <<"vcard-temp">>} -> true;
      {<<"BDAY">>, <<"vcard-temp">>} -> true;
      {<<"NICKNAME">>, <<"vcard-temp">>} -> true;
      {<<"FN">>, <<"vcard-temp">>} -> true;
      {<<"VERSION">>, <<"vcard-temp">>} -> true;
      {<<"CRED">>, <<"vcard-temp">>} -> true;
      {<<"PHONETIC">>, <<"vcard-temp">>} -> true;
      {<<"ORGUNIT">>, <<"vcard-temp">>} -> true;
      {<<"ORGNAME">>, <<"vcard-temp">>} -> true;
      {<<"LON">>, <<"vcard-temp">>} -> true;
      {<<"LAT">>, <<"vcard-temp">>} -> true;
      {<<"USERID">>, <<"vcard-temp">>} -> true;
      {<<"NUMBER">>, <<"vcard-temp">>} -> true;
      {<<"LINE">>, <<"vcard-temp">>} -> true;
      {<<"CTRY">>, <<"vcard-temp">>} -> true;
      {<<"PCODE">>, <<"vcard-temp">>} -> true;
      {<<"REGION">>, <<"vcard-temp">>} -> true;
      {<<"LOCALITY">>, <<"vcard-temp">>} -> true;
      {<<"STREET">>, <<"vcard-temp">>} -> true;
      {<<"EXTADD">>, <<"vcard-temp">>} -> true;
      {<<"POBOX">>, <<"vcard-temp">>} -> true;
      {<<"SUFFIX">>, <<"vcard-temp">>} -> true;
      {<<"PREFIX">>, <<"vcard-temp">>} -> true;
      {<<"MIDDLE">>, <<"vcard-temp">>} -> true;
      {<<"GIVEN">>, <<"vcard-temp">>} -> true;
      {<<"FAMILY">>, <<"vcard-temp">>} -> true;
      {<<"X400">>, <<"vcard-temp">>} -> true;
      {<<"INTERNET">>, <<"vcard-temp">>} -> true;
      {<<"PREF">>, <<"vcard-temp">>} -> true;
      {<<"INTL">>, <<"vcard-temp">>} -> true;
      {<<"DOM">>, <<"vcard-temp">>} -> true;
      {<<"PARCEL">>, <<"vcard-temp">>} -> true;
      {<<"POSTAL">>, <<"vcard-temp">>} -> true;
      {<<"PCS">>, <<"vcard-temp">>} -> true;
      {<<"ISDN">>, <<"vcard-temp">>} -> true;
      {<<"MODEM">>, <<"vcard-temp">>} -> true;
      {<<"BBS">>, <<"vcard-temp">>} -> true;
      {<<"VIDEO">>, <<"vcard-temp">>} -> true;
      {<<"CELL">>, <<"vcard-temp">>} -> true;
      {<<"MSG">>, <<"vcard-temp">>} -> true;
      {<<"PAGER">>, <<"vcard-temp">>} -> true;
      {<<"FAX">>, <<"vcard-temp">>} -> true;
      {<<"VOICE">>, <<"vcard-temp">>} -> true;
      {<<"WORK">>, <<"vcard-temp">>} -> true;
      {<<"HOME">>, <<"vcard-temp">>} -> true;
      {<<"stream:error">>,
       <<"http://etherx.jabber.org/streams">>} ->
	  true;
      {<<"unsupported-version">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"unsupported-stanza-type">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"unsupported-encoding">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"undefined-condition">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"system-shutdown">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"see-other-host">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"restricted-xml">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"resource-constraint">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"reset">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"remote-connection-failed">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"policy-violation">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"not-well-formed">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"not-authorized">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"invalid-xml">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"invalid-namespace">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"invalid-id">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"invalid-from">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"internal-server-error">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"improper-addressing">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"host-unknown">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"host-gone">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"connection-timeout">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"conflict">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"bad-namespace-prefix">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"bad-format">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"text">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  true;
      {<<"time">>, <<"urn:xmpp:time">>} -> true;
      {<<"tzo">>, <<"urn:xmpp:time">>} -> true;
      {<<"utc">>, <<"urn:xmpp:time">>} -> true;
      {<<"ping">>, <<"urn:xmpp:ping">>} -> true;
      {<<"session">>,
       <<"urn:ietf:params:xml:ns:xmpp-session">>} ->
	  true;
      {<<"query">>, <<"jabber:iq:register">>} -> true;
      {<<"key">>, <<"jabber:iq:register">>} -> true;
      {<<"text">>, <<"jabber:iq:register">>} -> true;
      {<<"misc">>, <<"jabber:iq:register">>} -> true;
      {<<"date">>, <<"jabber:iq:register">>} -> true;
      {<<"url">>, <<"jabber:iq:register">>} -> true;
      {<<"phone">>, <<"jabber:iq:register">>} -> true;
      {<<"zip">>, <<"jabber:iq:register">>} -> true;
      {<<"state">>, <<"jabber:iq:register">>} -> true;
      {<<"city">>, <<"jabber:iq:register">>} -> true;
      {<<"address">>, <<"jabber:iq:register">>} -> true;
      {<<"email">>, <<"jabber:iq:register">>} -> true;
      {<<"last">>, <<"jabber:iq:register">>} -> true;
      {<<"first">>, <<"jabber:iq:register">>} -> true;
      {<<"name">>, <<"jabber:iq:register">>} -> true;
      {<<"password">>, <<"jabber:iq:register">>} -> true;
      {<<"nick">>, <<"jabber:iq:register">>} -> true;
      {<<"username">>, <<"jabber:iq:register">>} -> true;
      {<<"instructions">>, <<"jabber:iq:register">>} -> true;
      {<<"remove">>, <<"jabber:iq:register">>} -> true;
      {<<"registered">>, <<"jabber:iq:register">>} -> true;
      {<<"register">>,
       <<"http://jabber.org/features/iq-register">>} ->
	  true;
      {<<"c">>, <<"http://jabber.org/protocol/caps">>} ->
	  true;
      {<<"ack">>, <<"p1:ack">>} -> true;
      {<<"rebind">>, <<"p1:rebind">>} -> true;
      {<<"push">>, <<"p1:push">>} -> true;
      {<<"stream:features">>,
       <<"http://etherx.jabber.org/streams">>} ->
	  true;
      {<<"compression">>,
       <<"http://jabber.org/features/compress">>} ->
	  true;
      {<<"method">>,
       <<"http://jabber.org/features/compress">>} ->
	  true;
      {<<"compressed">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  true;
      {<<"compress">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  true;
      {<<"method">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  true;
      {<<"failure">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  true;
      {<<"unsupported-method">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  true;
      {<<"processing-failed">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  true;
      {<<"setup-failed">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  true;
      {<<"failure">>,
       <<"urn:ietf:params:xml:ns:xmpp-tls">>} ->
	  true;
      {<<"proceed">>,
       <<"urn:ietf:params:xml:ns:xmpp-tls">>} ->
	  true;
      {<<"starttls">>,
       <<"urn:ietf:params:xml:ns:xmpp-tls">>} ->
	  true;
      {<<"required">>,
       <<"urn:ietf:params:xml:ns:xmpp-tls">>} ->
	  true;
      {<<"mechanisms">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"mechanism">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"failure">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"temporary-auth-failure">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"not-authorized">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"mechanism-too-weak">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"malformed-request">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"invalid-mechanism">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"invalid-authzid">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"incorrect-encoding">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"encryption-required">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"credentials-expired">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"account-disabled">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"aborted">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"text">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"success">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"response">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"challenge">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"abort">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"auth">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  true;
      {<<"bind">>, <<"urn:ietf:params:xml:ns:xmpp-bind">>} ->
	  true;
      {<<"resource">>,
       <<"urn:ietf:params:xml:ns:xmpp-bind">>} ->
	  true;
      {<<"jid">>, <<"urn:ietf:params:xml:ns:xmpp-bind">>} ->
	  true;
      {<<"error">>, <<"jabber:client">>} -> true;
      {<<"text">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"unexpected-request">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"undefined-condition">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"subscription-required">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"service-unavailable">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"resource-constraint">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"remote-server-timeout">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"remote-server-not-found">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"registration-required">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"redirect">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"recipient-unavailable">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"policy-violation">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"not-authorized">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"not-allowed">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"not-acceptable">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"jid-malformed">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"item-not-found">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"internal-server-error">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"gone">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"forbidden">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"feature-not-implemented">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"conflict">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"bad-request">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  true;
      {<<"presence">>, <<"jabber:client">>} -> true;
      {<<"priority">>, <<"jabber:client">>} -> true;
      {<<"status">>, <<"jabber:client">>} -> true;
      {<<"show">>, <<"jabber:client">>} -> true;
      {<<"message">>, <<"jabber:client">>} -> true;
      {<<"thread">>, <<"jabber:client">>} -> true;
      {<<"body">>, <<"jabber:client">>} -> true;
      {<<"subject">>, <<"jabber:client">>} -> true;
      {<<"iq">>, <<"jabber:client">>} -> true;
      {<<"query">>, <<"http://jabber.org/protocol/stats">>} ->
	  true;
      {<<"stat">>, <<"http://jabber.org/protocol/stats">>} ->
	  true;
      {<<"error">>, <<"http://jabber.org/protocol/stats">>} ->
	  true;
      {<<"storage">>, <<"storage:bookmarks">>} -> true;
      {<<"url">>, <<"storage:bookmarks">>} -> true;
      {<<"conference">>, <<"storage:bookmarks">>} -> true;
      {<<"password">>, <<"storage:bookmarks">>} -> true;
      {<<"nick">>, <<"storage:bookmarks">>} -> true;
      {<<"query">>, <<"jabber:iq:private">>} -> true;
      {<<"query">>,
       <<"http://jabber.org/protocol/disco#items">>} ->
	  true;
      {<<"item">>,
       <<"http://jabber.org/protocol/disco#items">>} ->
	  true;
      {<<"query">>,
       <<"http://jabber.org/protocol/disco#info">>} ->
	  true;
      {<<"feature">>,
       <<"http://jabber.org/protocol/disco#info">>} ->
	  true;
      {<<"identity">>,
       <<"http://jabber.org/protocol/disco#info">>} ->
	  true;
      {<<"blocklist">>, <<"urn:xmpp:blocking">>} -> true;
      {<<"unblock">>, <<"urn:xmpp:blocking">>} -> true;
      {<<"block">>, <<"urn:xmpp:blocking">>} -> true;
      {<<"item">>, <<"urn:xmpp:blocking">>} -> true;
      {<<"query">>, <<"jabber:iq:privacy">>} -> true;
      {<<"active">>, <<"jabber:iq:privacy">>} -> true;
      {<<"default">>, <<"jabber:iq:privacy">>} -> true;
      {<<"list">>, <<"jabber:iq:privacy">>} -> true;
      {<<"item">>, <<"jabber:iq:privacy">>} -> true;
      {<<"presence-out">>, <<"jabber:iq:privacy">>} -> true;
      {<<"presence-in">>, <<"jabber:iq:privacy">>} -> true;
      {<<"iq">>, <<"jabber:iq:privacy">>} -> true;
      {<<"message">>, <<"jabber:iq:privacy">>} -> true;
      {<<"query">>, <<"jabber:iq:roster">>} -> true;
      {<<"item">>, <<"jabber:iq:roster">>} -> true;
      {<<"group">>, <<"jabber:iq:roster">>} -> true;
      {<<"query">>, <<"jabber:iq:version">>} -> true;
      {<<"os">>, <<"jabber:iq:version">>} -> true;
      {<<"version">>, <<"jabber:iq:version">>} -> true;
      {<<"name">>, <<"jabber:iq:version">>} -> true;
      {<<"query">>, <<"jabber:iq:last">>} -> true;
      _ -> false
    end.

encode({muc, _, _} = X) ->
    encode_muc(X,
	       [{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]);
encode({muc_owner, _, _} = Query) ->
    encode_muc_owner(Query,
		     [{<<"xmlns">>,
		       <<"http://jabber.org/protocol/muc#owner">>}]);
encode({muc_owner_destroy, _, _, _} = Destroy) ->
    encode_muc_owner_destroy(Destroy,
			     [{<<"xmlns">>,
			       <<"http://jabber.org/protocol/muc#owner">>}]);
encode({muc_user, _, _, _, _, _, _} = X) ->
    encode_muc_user(X,
		    [{<<"xmlns">>,
		      <<"http://jabber.org/protocol/muc#user">>}]);
encode({muc_item, _, _, _, _, _, _, _} = Item) ->
    encode_muc_user_item(Item,
			 [{<<"xmlns">>,
			   <<"http://jabber.org/protocol/muc#user">>}]);
encode({muc_actor, _, _} = Actor) ->
    encode_muc_user_actor(Actor,
			  [{<<"xmlns">>,
			    <<"http://jabber.org/protocol/muc#user">>}]);
encode({muc_invite, _, _, _} = Invite) ->
    encode_muc_user_invite(Invite,
			   [{<<"xmlns">>,
			     <<"http://jabber.org/protocol/muc#user">>}]);
encode({muc_user_destroy, _, _} = Destroy) ->
    encode_muc_user_destroy(Destroy,
			    [{<<"xmlns">>,
			      <<"http://jabber.org/protocol/muc#user">>}]);
encode({muc_decline, _, _, _} = Decline) ->
    encode_muc_user_decline(Decline,
			    [{<<"xmlns">>,
			      <<"http://jabber.org/protocol/muc#user">>}]);
encode({muc_history, _, _, _, _} = History) ->
    encode_muc_history(History,
		       [{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]);
encode({bytestreams, _, _, _, _, _, _} = Query) ->
    encode_bytestreams(Query,
		       [{<<"xmlns">>,
			 <<"http://jabber.org/protocol/bytestreams">>}]);
encode({streamhost, _, _, _} = Streamhost) ->
    encode_bytestreams_streamhost(Streamhost,
				  [{<<"xmlns">>,
				    <<"http://jabber.org/protocol/bytestreams">>}]);
encode({legacy_delay, _, _} = X) ->
    encode_legacy_delay(X,
			[{<<"xmlns">>, <<"jabber:x:delay">>}]);
encode({delay, _, _} = Delay) ->
    encode_delay(Delay,
		 [{<<"xmlns">>, <<"urn:xmpp:delay">>}]);
encode({shim, _} = Headers) ->
    encode_shim_headers(Headers,
			[{<<"xmlns">>, <<"http://jabber.org/protocol/shim">>}]);
encode({pubsub, _, _, _, _, _, _, _, _} = Pubsub) ->
    encode_pubsub(Pubsub,
		  [{<<"xmlns">>,
		    <<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_retract, _, _, _} = Retract) ->
    encode_pubsub_retract(Retract,
			  [{<<"xmlns">>,
			    <<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_options, _, _, _, _} = Options) ->
    encode_pubsub_options(Options,
			  [{<<"xmlns">>,
			    <<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_publish, _, _} = Publish) ->
    encode_pubsub_publish(Publish,
			  [{<<"xmlns">>,
			    <<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_unsubscribe, _, _, _} = Unsubscribe) ->
    encode_pubsub_unsubscribe(Unsubscribe,
			      [{<<"xmlns">>,
				<<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_subscribe, _, _} = Subscribe) ->
    encode_pubsub_subscribe(Subscribe,
			    [{<<"xmlns">>,
			      <<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_event, _} = Event) ->
    encode_pubsub_event(Event,
			[{<<"xmlns">>,
			  <<"http://jabber.org/protocol/pubsub#event">>}]);
encode({pubsub_event_items, _, _, _} = Items) ->
    encode_pubsub_event_items(Items,
			      [{<<"xmlns">>,
				<<"http://jabber.org/protocol/pubsub#event">>}]);
encode({pubsub_event_item, _, _, _} = Item) ->
    encode_pubsub_event_item(Item,
			     [{<<"xmlns">>,
			       <<"http://jabber.org/protocol/pubsub#event">>}]);
encode({pubsub_items, _, _, _, _} = Items) ->
    encode_pubsub_items(Items,
			[{<<"xmlns">>,
			  <<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_item, _, _} = Item) ->
    encode_pubsub_item(Item,
		       [{<<"xmlns">>,
			 <<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_affiliation, _, _} = Affiliation) ->
    encode_pubsub_affiliation(Affiliation,
			      [{<<"xmlns">>,
				<<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_subscription, _, _, _, _} =
	   Subscription) ->
    encode_pubsub_subscription(Subscription,
			       [{<<"xmlns">>,
				 <<"http://jabber.org/protocol/pubsub">>}]);
encode({xdata, _, _, _, _, _, _} = X) ->
    encode_xdata(X, [{<<"xmlns">>, <<"jabber:x:data">>}]);
encode({xdata_field, _, _, _, _, _, _, _} = Field) ->
    encode_xdata_field(Field,
		       [{<<"xmlns">>, <<"jabber:x:data">>}]);
encode({vcard, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
	_, _, _, _, _, _, _, _, _, _, _, _, _, _, _} =
	   Vcard) ->
    encode_vcard(Vcard, [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_key, _, _} = Key) ->
    encode_vcard_KEY(Key,
		     [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_sound, _, _, _} = Sound) ->
    encode_vcard_SOUND(Sound,
		       [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_org, _, _} = Org) ->
    encode_vcard_ORG(Org,
		     [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_photo, _, _, _} = Photo) ->
    encode_vcard_PHOTO(Photo,
		       [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_logo, _, _, _} = Logo) ->
    encode_vcard_LOGO(Logo,
		      [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_geo, _, _} = Geo) ->
    encode_vcard_GEO(Geo,
		     [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_email, _, _, _, _, _, _} = Email) ->
    encode_vcard_EMAIL(Email,
		       [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_tel, _, _, _, _, _, _, _, _, _, _, _, _,
	_, _} =
	   Tel) ->
    encode_vcard_TEL(Tel,
		     [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_label, _, _, _, _, _, _, _, _} = Label) ->
    encode_vcard_LABEL(Label,
		       [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_adr, _, _, _, _, _, _, _, _, _, _, _, _,
	_, _} =
	   Adr) ->
    encode_vcard_ADR(Adr,
		     [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_name, _, _, _, _, _} = N) ->
    encode_vcard_N(N, [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({stream_error, _, _} = Stream_error) ->
    encode_stream_error(Stream_error,
			[{<<"xmlns">>,
			  <<"http://etherx.jabber.org/streams">>}]);
encode({'see-other-host', _} = See_other_host) ->
    encode_stream_error_see_other_host(See_other_host,
				       [{<<"xmlns">>,
					 <<"urn:ietf:params:xml:ns:xmpp-streams">>}]);
encode({time, _, _} = Time) ->
    encode_time(Time, [{<<"xmlns">>, <<"urn:xmpp:time">>}]);
encode({ping} = Ping) ->
    encode_ping(Ping, [{<<"xmlns">>, <<"urn:xmpp:ping">>}]);
encode({session} = Session) ->
    encode_session(Session,
		   [{<<"xmlns">>,
		     <<"urn:ietf:params:xml:ns:xmpp-session">>}]);
encode({register, _, _, _, _, _, _, _, _, _, _, _, _, _,
	_, _, _, _, _, _, _} =
	   Query) ->
    encode_register(Query,
		    [{<<"xmlns">>, <<"jabber:iq:register">>}]);
encode({feature_register} = Register) ->
    encode_feature_register(Register,
			    [{<<"xmlns">>,
			      <<"http://jabber.org/features/iq-register">>}]);
encode({caps, _, _, _} = C) ->
    encode_caps(C,
		[{<<"xmlns">>, <<"http://jabber.org/protocol/caps">>}]);
encode({p1_ack} = Ack) ->
    encode_p1_ack(Ack, [{<<"xmlns">>, <<"p1:ack">>}]);
encode({p1_rebind} = Rebind) ->
    encode_p1_rebind(Rebind,
		     [{<<"xmlns">>, <<"p1:rebind">>}]);
encode({p1_push} = Push) ->
    encode_p1_push(Push, [{<<"xmlns">>, <<"p1:push">>}]);
encode({stream_features, _} = Stream_features) ->
    encode_stream_features(Stream_features,
			   [{<<"xmlns">>,
			     <<"http://etherx.jabber.org/streams">>}]);
encode({compression, _} = Compression) ->
    encode_compression(Compression,
		       [{<<"xmlns">>,
			 <<"http://jabber.org/features/compress">>}]);
encode({compressed} = Compressed) ->
    encode_compressed(Compressed,
		      [{<<"xmlns">>,
			<<"http://jabber.org/protocol/compress">>}]);
encode({compress, _} = Compress) ->
    encode_compress(Compress,
		    [{<<"xmlns">>,
		      <<"http://jabber.org/protocol/compress">>}]);
encode({compress_failure, _} = Failure) ->
    encode_compress_failure(Failure,
			    [{<<"xmlns">>,
			      <<"http://jabber.org/protocol/compress">>}]);
encode({starttls_failure} = Failure) ->
    encode_starttls_failure(Failure,
			    [{<<"xmlns">>,
			      <<"urn:ietf:params:xml:ns:xmpp-tls">>}]);
encode({starttls_proceed} = Proceed) ->
    encode_starttls_proceed(Proceed,
			    [{<<"xmlns">>,
			      <<"urn:ietf:params:xml:ns:xmpp-tls">>}]);
encode({starttls, _} = Starttls) ->
    encode_starttls(Starttls,
		    [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-tls">>}]);
encode({sasl_mechanisms, _} = Mechanisms) ->
    encode_sasl_mechanisms(Mechanisms,
			   [{<<"xmlns">>,
			     <<"urn:ietf:params:xml:ns:xmpp-sasl">>}]);
encode({sasl_failure, _, _} = Failure) ->
    encode_sasl_failure(Failure,
			[{<<"xmlns">>,
			  <<"urn:ietf:params:xml:ns:xmpp-sasl">>}]);
encode({sasl_success, _} = Success) ->
    encode_sasl_success(Success,
			[{<<"xmlns">>,
			  <<"urn:ietf:params:xml:ns:xmpp-sasl">>}]);
encode({sasl_response, _} = Response) ->
    encode_sasl_response(Response,
			 [{<<"xmlns">>,
			   <<"urn:ietf:params:xml:ns:xmpp-sasl">>}]);
encode({sasl_challenge, _} = Challenge) ->
    encode_sasl_challenge(Challenge,
			  [{<<"xmlns">>,
			    <<"urn:ietf:params:xml:ns:xmpp-sasl">>}]);
encode({sasl_abort} = Abort) ->
    encode_sasl_abort(Abort,
		      [{<<"xmlns">>,
			<<"urn:ietf:params:xml:ns:xmpp-sasl">>}]);
encode({sasl_auth, _, _} = Auth) ->
    encode_sasl_auth(Auth,
		     [{<<"xmlns">>,
		       <<"urn:ietf:params:xml:ns:xmpp-sasl">>}]);
encode({bind, _, _} = Bind) ->
    encode_bind(Bind,
		[{<<"xmlns">>,
		  <<"urn:ietf:params:xml:ns:xmpp-bind">>}]);
encode({error, _, _, _, _} = Error) ->
    encode_error(Error,
		 [{<<"xmlns">>, <<"jabber:client">>}]);
encode({redirect, _} = Redirect) ->
    encode_error_redirect(Redirect,
			  [{<<"xmlns">>,
			    <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}]);
encode({gone, _} = Gone) ->
    encode_error_gone(Gone,
		      [{<<"xmlns">>,
			<<"urn:ietf:params:xml:ns:xmpp-stanzas">>}]);
encode({presence, _, _, _, _, _, _, _, _, _, _} =
	   Presence) ->
    encode_presence(Presence,
		    [{<<"xmlns">>, <<"jabber:client">>}]);
encode({message, _, _, _, _, _, _, _, _, _, _} =
	   Message) ->
    encode_message(Message,
		   [{<<"xmlns">>, <<"jabber:client">>}]);
encode({iq, _, _, _, _, _, _, _} = Iq) ->
    encode_iq(Iq, [{<<"xmlns">>, <<"jabber:client">>}]);
encode({stats, _} = Query) ->
    encode_stats(Query,
		 [{<<"xmlns">>,
		   <<"http://jabber.org/protocol/stats">>}]);
encode({stat, _, _, _, _} = Stat) ->
    encode_stat(Stat,
		[{<<"xmlns">>,
		  <<"http://jabber.org/protocol/stats">>}]);
encode({bookmark_storage, _, _} = Storage) ->
    encode_bookmarks_storage(Storage,
			     [{<<"xmlns">>, <<"storage:bookmarks">>}]);
encode({bookmark_url, _, _} = Url) ->
    encode_bookmark_url(Url,
			[{<<"xmlns">>, <<"storage:bookmarks">>}]);
encode({bookmark_conference, _, _, _, _, _} =
	   Conference) ->
    encode_bookmark_conference(Conference,
			       [{<<"xmlns">>, <<"storage:bookmarks">>}]);
encode({private, _} = Query) ->
    encode_private(Query,
		   [{<<"xmlns">>, <<"jabber:iq:private">>}]);
encode({disco_items, _, _} = Query) ->
    encode_disco_items(Query,
		       [{<<"xmlns">>,
			 <<"http://jabber.org/protocol/disco#items">>}]);
encode({disco_item, _, _, _} = Item) ->
    encode_disco_item(Item,
		      [{<<"xmlns">>,
			<<"http://jabber.org/protocol/disco#items">>}]);
encode({disco_info, _, _, _, _} = Query) ->
    encode_disco_info(Query,
		      [{<<"xmlns">>,
			<<"http://jabber.org/protocol/disco#info">>}]);
encode({identity, _, _, _, _} = Identity) ->
    encode_disco_identity(Identity,
			  [{<<"xmlns">>,
			    <<"http://jabber.org/protocol/disco#info">>}]);
encode({block_list} = Blocklist) ->
    encode_block_list(Blocklist,
		      [{<<"xmlns">>, <<"urn:xmpp:blocking">>}]);
encode({unblock, _} = Unblock) ->
    encode_unblock(Unblock,
		   [{<<"xmlns">>, <<"urn:xmpp:blocking">>}]);
encode({block, _} = Block) ->
    encode_block(Block,
		 [{<<"xmlns">>, <<"urn:xmpp:blocking">>}]);
encode({privacy, _, _, _} = Query) ->
    encode_privacy(Query,
		   [{<<"xmlns">>, <<"jabber:iq:privacy">>}]);
encode({privacy_list, _, _} = List) ->
    encode_privacy_list(List,
			[{<<"xmlns">>, <<"jabber:iq:privacy">>}]);
encode({privacy_item, _, _, _, _, _} = Item) ->
    encode_privacy_item(Item,
			[{<<"xmlns">>, <<"jabber:iq:privacy">>}]);
encode({roster, _, _} = Query) ->
    encode_roster(Query,
		  [{<<"xmlns">>, <<"jabber:iq:roster">>}]);
encode({roster_item, _, _, _, _, _} = Item) ->
    encode_roster_item(Item,
		       [{<<"xmlns">>, <<"jabber:iq:roster">>}]);
encode({version, _, _, _} = Query) ->
    encode_version(Query,
		   [{<<"xmlns">>, <<"jabber:iq:version">>}]);
encode({last, _, _} = Query) ->
    encode_last(Query,
		[{<<"xmlns">>, <<"jabber:iq:last">>}]).

dec_int(Val) -> dec_int(Val, infinity, infinity).

dec_int(Val, Min, Max) ->
    case list_to_integer(binary_to_list(Val)) of
      Int when Int =< Max, Min == infinity -> Int;
      Int when Int =< Max, Int >= Min -> Int
    end.

enc_int(Int) -> list_to_binary(integer_to_list(Int)).

dec_enum(Val, Enums) ->
    AtomVal = erlang:binary_to_existing_atom(Val, utf8),
    case lists:member(AtomVal, Enums) of
      true -> AtomVal
    end.

enc_enum(Atom) -> erlang:atom_to_binary(Atom, utf8).

format_error({bad_attr_value, Attr, Tag, XMLNS}) ->
    <<"Bad value of attribute '", Attr/binary, "' in tag <",
      Tag/binary, "/> qualified by namespace '", XMLNS/binary,
      "'">>;
format_error({bad_cdata_value, <<>>, Tag, XMLNS}) ->
    <<"Bad value of cdata in tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({missing_tag, Tag, XMLNS}) ->
    <<"Missing tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({missing_attr, Attr, Tag, XMLNS}) ->
    <<"Missing attribute '", Attr/binary, "' in tag <",
      Tag/binary, "/> qualified by namespace '", XMLNS/binary,
      "'">>;
format_error({missing_cdata, <<>>, Tag, XMLNS}) ->
    <<"Missing cdata in tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({unknown_tag, Tag, XMLNS}) ->
    <<"Unknown tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>.

get_attr(Attr, Attrs) ->
    case lists:keyfind(Attr, 1, Attrs) of
      {_, Val} -> Val;
      false -> <<>>
    end.

pp(Term) -> io_lib_pretty:print(Term, fun pp/2).

pp(last, 2) -> [seconds, text];
pp(version, 3) -> [name, ver, os];
pp(roster_item, 5) ->
    [jid, name, groups, subscription, ask];
pp(roster, 2) -> [items, ver];
pp(privacy_item, 5) ->
    [order, action, type, value, kinds];
pp(privacy_list, 2) -> [name, items];
pp(privacy, 3) -> [lists, default, active];
pp(block, 1) -> [items];
pp(unblock, 1) -> [items];
pp(block_list, 0) -> [];
pp(identity, 4) -> [category, type, lang, name];
pp(disco_info, 4) ->
    [node, identities, features, xdata];
pp(disco_item, 3) -> [jid, name, node];
pp(disco_items, 2) -> [node, items];
pp(private, 1) -> [xml_els];
pp(bookmark_conference, 5) ->
    [name, jid, autojoin, nick, password];
pp(bookmark_url, 2) -> [name, url];
pp(bookmark_storage, 2) -> [conference, url];
pp(stat, 4) -> [name, units, value, error];
pp(stats, 1) -> [stat];
pp(iq, 7) -> [id, type, lang, from, to, error, sub_els];
pp(message, 10) ->
    [id, type, lang, from, to, subject, body, thread, error,
     sub_els];
pp(presence, 10) ->
    [id, type, lang, from, to, show, status, priority,
     error, sub_els];
pp(gone, 1) -> [uri];
pp(redirect, 1) -> [uri];
pp(error, 4) -> [type, by, reason, text];
pp(bind, 2) -> [jid, resource];
pp(sasl_auth, 2) -> [mechanism, text];
pp(sasl_abort, 0) -> [];
pp(sasl_challenge, 1) -> [text];
pp(sasl_response, 1) -> [text];
pp(sasl_success, 1) -> [text];
pp(sasl_failure, 2) -> [reason, text];
pp(sasl_mechanisms, 1) -> [list];
pp(starttls, 1) -> [required];
pp(starttls_proceed, 0) -> [];
pp(starttls_failure, 0) -> [];
pp(compress_failure, 1) -> [reason];
pp(compress, 1) -> [methods];
pp(compressed, 0) -> [];
pp(compression, 1) -> [methods];
pp(stream_features, 1) -> [sub_els];
pp(p1_push, 0) -> [];
pp(p1_rebind, 0) -> [];
pp(p1_ack, 0) -> [];
pp(caps, 3) -> [hash, node, ver];
pp(feature_register, 0) -> [];
pp(register, 20) ->
    [registered, remove, instructions, username, nick,
     password, name, first, last, email, address, city,
     state, zip, phone, url, date, misc, text, key];
pp(session, 0) -> [];
pp(ping, 0) -> [];
pp(time, 2) -> [tzo, utc];
pp(text, 2) -> [lang, data];
pp('see-other-host', 1) -> [host];
pp(stream_error, 2) -> [reason, text];
pp(vcard_name, 5) ->
    [family, given, middle, prefix, suffix];
pp(vcard_adr, 14) ->
    [home, work, postal, parcel, dom, intl, pref, pobox,
     extadd, street, locality, region, pcode, ctry];
pp(vcard_label, 8) ->
    [home, work, postal, parcel, dom, intl, pref, line];
pp(vcard_tel, 14) ->
    [home, work, voice, fax, pager, msg, cell, video, bbs,
     modem, isdn, pcs, pref, number];
pp(vcard_email, 6) ->
    [home, work, internet, pref, x400, userid];
pp(vcard_geo, 2) -> [lat, lon];
pp(vcard_logo, 3) -> [type, binval, extval];
pp(vcard_photo, 3) -> [type, binval, extval];
pp(vcard_org, 2) -> [name, units];
pp(vcard_sound, 3) -> [phonetic, binval, extval];
pp(vcard_key, 2) -> [type, cred];
pp(vcard, 29) ->
    [version, fn, n, nickname, photo, bday, adr, label, tel,
     email, jabberid, mailer, tz, geo, title, role, logo,
     org, categories, note, prodid, rev, sort_string, sound,
     uid, url, class, key, desc];
pp(xdata_field, 7) ->
    [label, type, var, required, desc, values, options];
pp(xdata, 6) ->
    [type, instructions, title, reported, items, fields];
pp(pubsub_subscription, 4) -> [jid, node, subid, type];
pp(pubsub_affiliation, 2) -> [node, type];
pp(pubsub_item, 2) -> [id, xml_els];
pp(pubsub_items, 4) -> [node, max_items, subid, items];
pp(pubsub_event_item, 3) -> [id, node, publisher];
pp(pubsub_event_items, 3) -> [node, retract, items];
pp(pubsub_event, 1) -> [items];
pp(pubsub_subscribe, 2) -> [node, jid];
pp(pubsub_unsubscribe, 3) -> [node, jid, subid];
pp(pubsub_publish, 2) -> [node, items];
pp(pubsub_options, 4) -> [node, jid, subid, xdata];
pp(pubsub_retract, 3) -> [node, notify, items];
pp(pubsub, 8) ->
    [subscriptions, affiliations, publish, subscribe,
     unsubscribe, options, items, retract];
pp(shim, 1) -> [headers];
pp(delay, 2) -> [stamp, from];
pp(legacy_delay, 2) -> [stamp, from];
pp(streamhost, 3) -> [jid, host, port];
pp(bytestreams, 6) ->
    [hosts, used, activate, dstaddr, mode, sid];
pp(muc_history, 4) ->
    [maxchars, maxstanzas, seconds, since];
pp(muc_decline, 3) -> [reason, from, to];
pp(muc_user_destroy, 2) -> [reason, jid];
pp(muc_invite, 3) -> [reason, from, to];
pp(muc_actor, 2) -> [jid, nick];
pp(muc_item, 7) ->
    [actor, continue, reason, affiliation, role, jid, nick];
pp(muc_user, 6) ->
    [decline, destroy, invites, items, status_codes,
     password];
pp(muc_owner_destroy, 3) -> [jid, reason, password];
pp(muc_owner, 2) -> [destroy, config];
pp(muc, 2) -> [history, password];
pp(_, _) -> no.

enc_bool(false) -> <<"false">>;
enc_bool(true) -> <<"true">>.

dec_bool(<<"false">>) -> false;
dec_bool(<<"true">>) -> true.

resourceprep(R) ->
    case jlib:resourceprep(R) of
      error -> erlang:error(badarg);
      R1 -> R1
    end.

enc_jid(J) -> jlib:jid_to_string(J).

dec_jid(Val) ->
    case jlib:string_to_jid(Val) of
      error -> erlang:error(badarg);
      J -> J
    end.

enc_utc(Val) -> jlib:now_to_utc_string(Val).

dec_utc(Val) ->
    {_, _, _} = jlib:datetime_string_to_timestamp(Val).

enc_tzo({H, M}) ->
    Sign = if H >= 0 -> <<>>;
	      true -> <<"-">>
	   end,
    list_to_binary([Sign,
		    io_lib:format("~2..0w:~2..0w", [H, M])]).

dec_tzo(Val) ->
    [H1, M1] = str:tokens(Val, <<":">>),
    H = jlib:binary_to_integer(H1),
    M = jlib:binary_to_integer(M1),
    if H >= -12, H =< 12, M >= 0, M < 60 -> {H, M} end.

decode_muc({xmlel, <<"x">>, _attrs, _els}) ->
    History = decode_muc_els(_els, undefined),
    Password = decode_muc_attrs(_attrs, undefined),
    {muc, History, Password}.

decode_muc_els([], History) -> History;
decode_muc_els([{xmlel, <<"history">>, _attrs, _} = _el
		| _els],
	       History) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/muc">> ->
	   decode_muc_els(_els, decode_muc_history(_el));
       true -> decode_muc_els(_els, History)
    end;
decode_muc_els([_ | _els], History) ->
    decode_muc_els(_els, History).

decode_muc_attrs([{<<"password">>, _val} | _attrs],
		 _Password) ->
    decode_muc_attrs(_attrs, _val);
decode_muc_attrs([_ | _attrs], Password) ->
    decode_muc_attrs(_attrs, Password);
decode_muc_attrs([], Password) ->
    decode_muc_attr_password(Password).

encode_muc({muc, History, Password}, _xmlns_attrs) ->
    _els = 'encode_muc_$history'(History, []),
    _attrs = encode_muc_attr_password(Password,
				      _xmlns_attrs),
    {xmlel, <<"x">>, _attrs, _els}.

'encode_muc_$history'(undefined, _acc) -> _acc;
'encode_muc_$history'(History, _acc) ->
    [encode_muc_history(History, []) | _acc].

decode_muc_attr_password(undefined) -> undefined;
decode_muc_attr_password(_val) -> _val.

encode_muc_attr_password(undefined, _acc) -> _acc;
encode_muc_attr_password(_val, _acc) ->
    [{<<"password">>, _val} | _acc].

decode_muc_owner({xmlel, <<"query">>, _attrs, _els}) ->
    {Config, Destroy} = decode_muc_owner_els(_els,
					     undefined, undefined),
    {muc_owner, Destroy, Config}.

decode_muc_owner_els([], Config, Destroy) ->
    {Config, Destroy};
decode_muc_owner_els([{xmlel, <<"destroy">>, _attrs,
		       _} =
			  _el
		      | _els],
		     Config, Destroy) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/muc#owner">> ->
	   decode_muc_owner_els(_els, Config,
				decode_muc_owner_destroy(_el));
       true -> decode_muc_owner_els(_els, Config, Destroy)
    end;
decode_muc_owner_els([{xmlel, <<"x">>, _attrs, _} = _el
		      | _els],
		     Config, Destroy) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<"jabber:x:data">> ->
	   decode_muc_owner_els(_els, decode_xdata(_el), Destroy);
       true -> decode_muc_owner_els(_els, Config, Destroy)
    end;
decode_muc_owner_els([_ | _els], Config, Destroy) ->
    decode_muc_owner_els(_els, Config, Destroy).

encode_muc_owner({muc_owner, Destroy, Config},
		 _xmlns_attrs) ->
    _els = 'encode_muc_owner_$destroy'(Destroy,
				       'encode_muc_owner_$config'(Config, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"query">>, _attrs, _els}.

'encode_muc_owner_$config'(undefined, _acc) -> _acc;
'encode_muc_owner_$config'(Config, _acc) ->
    [encode_xdata(Config,
		  [{<<"xmlns">>, <<"jabber:x:data">>}])
     | _acc].

'encode_muc_owner_$destroy'(undefined, _acc) -> _acc;
'encode_muc_owner_$destroy'(Destroy, _acc) ->
    [encode_muc_owner_destroy(Destroy, []) | _acc].

decode_muc_owner_destroy({xmlel, <<"destroy">>, _attrs,
			  _els}) ->
    {Password, Reason} = decode_muc_owner_destroy_els(_els,
						      undefined, undefined),
    Jid = decode_muc_owner_destroy_attrs(_attrs, undefined),
    {muc_owner_destroy, Jid, Reason, Password}.

decode_muc_owner_destroy_els([], Password, Reason) ->
    {Password, Reason};
decode_muc_owner_destroy_els([{xmlel, <<"password">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Password, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/muc#owner">> ->
	   decode_muc_owner_destroy_els(_els,
					decode_muc_owner_password(_el), Reason);
       true ->
	   decode_muc_owner_destroy_els(_els, Password, Reason)
    end;
decode_muc_owner_destroy_els([{xmlel, <<"reason">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Password, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/muc#owner">> ->
	   decode_muc_owner_destroy_els(_els, Password,
					decode_muc_owner_reason(_el));
       true ->
	   decode_muc_owner_destroy_els(_els, Password, Reason)
    end;
decode_muc_owner_destroy_els([_ | _els], Password,
			     Reason) ->
    decode_muc_owner_destroy_els(_els, Password, Reason).

decode_muc_owner_destroy_attrs([{<<"jid">>, _val}
				| _attrs],
			       _Jid) ->
    decode_muc_owner_destroy_attrs(_attrs, _val);
decode_muc_owner_destroy_attrs([_ | _attrs], Jid) ->
    decode_muc_owner_destroy_attrs(_attrs, Jid);
decode_muc_owner_destroy_attrs([], Jid) ->
    decode_muc_owner_destroy_attr_jid(Jid).

encode_muc_owner_destroy({muc_owner_destroy, Jid,
			  Reason, Password},
			 _xmlns_attrs) ->
    _els = 'encode_muc_owner_destroy_$reason'(Reason,
					      'encode_muc_owner_destroy_$password'(Password,
										   [])),
    _attrs = encode_muc_owner_destroy_attr_jid(Jid,
					       _xmlns_attrs),
    {xmlel, <<"destroy">>, _attrs, _els}.

'encode_muc_owner_destroy_$password'(undefined, _acc) ->
    _acc;
'encode_muc_owner_destroy_$password'(Password, _acc) ->
    [encode_muc_owner_password(Password, []) | _acc].

'encode_muc_owner_destroy_$reason'(undefined, _acc) ->
    _acc;
'encode_muc_owner_destroy_$reason'(Reason, _acc) ->
    [encode_muc_owner_reason(Reason, []) | _acc].

decode_muc_owner_destroy_attr_jid(undefined) ->
    undefined;
decode_muc_owner_destroy_attr_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"destroy">>,
			 <<"http://jabber.org/protocol/muc#owner">>}});
      _res -> _res
    end.

encode_muc_owner_destroy_attr_jid(undefined, _acc) ->
    _acc;
encode_muc_owner_destroy_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_muc_owner_reason({xmlel, <<"reason">>, _attrs,
			 _els}) ->
    Cdata = decode_muc_owner_reason_els(_els, <<>>), Cdata.

decode_muc_owner_reason_els([], Cdata) ->
    decode_muc_owner_reason_cdata(Cdata);
decode_muc_owner_reason_els([{xmlcdata, _data} | _els],
			    Cdata) ->
    decode_muc_owner_reason_els(_els,
				<<Cdata/binary, _data/binary>>);
decode_muc_owner_reason_els([_ | _els], Cdata) ->
    decode_muc_owner_reason_els(_els, Cdata).

encode_muc_owner_reason(Cdata, _xmlns_attrs) ->
    _els = encode_muc_owner_reason_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"reason">>, _attrs, _els}.

decode_muc_owner_reason_cdata(<<>>) -> undefined;
decode_muc_owner_reason_cdata(_val) -> _val.

encode_muc_owner_reason_cdata(undefined, _acc) -> _acc;
encode_muc_owner_reason_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_muc_owner_password({xmlel, <<"password">>,
			   _attrs, _els}) ->
    Cdata = decode_muc_owner_password_els(_els, <<>>),
    Cdata.

decode_muc_owner_password_els([], Cdata) ->
    decode_muc_owner_password_cdata(Cdata);
decode_muc_owner_password_els([{xmlcdata, _data}
			       | _els],
			      Cdata) ->
    decode_muc_owner_password_els(_els,
				  <<Cdata/binary, _data/binary>>);
decode_muc_owner_password_els([_ | _els], Cdata) ->
    decode_muc_owner_password_els(_els, Cdata).

encode_muc_owner_password(Cdata, _xmlns_attrs) ->
    _els = encode_muc_owner_password_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"password">>, _attrs, _els}.

decode_muc_owner_password_cdata(<<>>) -> undefined;
decode_muc_owner_password_cdata(_val) -> _val.

encode_muc_owner_password_cdata(undefined, _acc) ->
    _acc;
encode_muc_owner_password_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_muc_user({xmlel, <<"x">>, _attrs, _els}) ->
    {Status_codes, Items, Invites, Decline, Destroy} =
	decode_muc_user_els(_els, [], [], [], undefined,
			    undefined),
    Password = decode_muc_user_attrs(_attrs, undefined),
    {muc_user, Decline, Destroy, Invites, Items,
     Status_codes, Password}.

decode_muc_user_els([], Status_codes, Items, Invites,
		    Decline, Destroy) ->
    {lists:reverse(Status_codes), lists:reverse(Items),
     lists:reverse(Invites), Decline, Destroy};
decode_muc_user_els([{xmlel, <<"decline">>, _attrs, _} =
			 _el
		     | _els],
		    Status_codes, Items, Invites, Decline, Destroy) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/muc#user">> ->
	   decode_muc_user_els(_els, Status_codes, Items, Invites,
			       decode_muc_user_decline(_el), Destroy);
       true ->
	   decode_muc_user_els(_els, Status_codes, Items, Invites,
			       Decline, Destroy)
    end;
decode_muc_user_els([{xmlel, <<"destroy">>, _attrs, _} =
			 _el
		     | _els],
		    Status_codes, Items, Invites, Decline, Destroy) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/muc#user">> ->
	   decode_muc_user_els(_els, Status_codes, Items, Invites,
			       Decline, decode_muc_user_destroy(_el));
       true ->
	   decode_muc_user_els(_els, Status_codes, Items, Invites,
			       Decline, Destroy)
    end;
decode_muc_user_els([{xmlel, <<"invite">>, _attrs, _} =
			 _el
		     | _els],
		    Status_codes, Items, Invites, Decline, Destroy) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/muc#user">> ->
	   decode_muc_user_els(_els, Status_codes, Items,
			       [decode_muc_user_invite(_el) | Invites], Decline,
			       Destroy);
       true ->
	   decode_muc_user_els(_els, Status_codes, Items, Invites,
			       Decline, Destroy)
    end;
decode_muc_user_els([{xmlel, <<"item">>, _attrs, _} =
			 _el
		     | _els],
		    Status_codes, Items, Invites, Decline, Destroy) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/muc#user">> ->
	   decode_muc_user_els(_els, Status_codes,
			       [decode_muc_user_item(_el) | Items], Invites,
			       Decline, Destroy);
       true ->
	   decode_muc_user_els(_els, Status_codes, Items, Invites,
			       Decline, Destroy)
    end;
decode_muc_user_els([{xmlel, <<"status">>, _attrs, _} =
			 _el
		     | _els],
		    Status_codes, Items, Invites, Decline, Destroy) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/muc#user">> ->
	   decode_muc_user_els(_els,
			       case decode_muc_user_status(_el) of
				 undefined -> Status_codes;
				 _new_el -> [_new_el | Status_codes]
			       end,
			       Items, Invites, Decline, Destroy);
       true ->
	   decode_muc_user_els(_els, Status_codes, Items, Invites,
			       Decline, Destroy)
    end;
decode_muc_user_els([_ | _els], Status_codes, Items,
		    Invites, Decline, Destroy) ->
    decode_muc_user_els(_els, Status_codes, Items, Invites,
			Decline, Destroy).

decode_muc_user_attrs([{<<"password">>, _val} | _attrs],
		      _Password) ->
    decode_muc_user_attrs(_attrs, _val);
decode_muc_user_attrs([_ | _attrs], Password) ->
    decode_muc_user_attrs(_attrs, Password);
decode_muc_user_attrs([], Password) ->
    decode_muc_user_attr_password(Password).

encode_muc_user({muc_user, Decline, Destroy, Invites,
		 Items, Status_codes, Password},
		_xmlns_attrs) ->
    _els = 'encode_muc_user_$destroy'(Destroy,
				      'encode_muc_user_$decline'(Decline,
								 'encode_muc_user_$invites'(Invites,
											    'encode_muc_user_$items'(Items,
														     'encode_muc_user_$status_codes'(Status_codes,
																		     []))))),
    _attrs = encode_muc_user_attr_password(Password,
					   _xmlns_attrs),
    {xmlel, <<"x">>, _attrs, _els}.

'encode_muc_user_$status_codes'([], _acc) -> _acc;
'encode_muc_user_$status_codes'([Status_codes | _els],
				_acc) ->
    'encode_muc_user_$status_codes'(_els,
				    [encode_muc_user_status(Status_codes, [])
				     | _acc]).

'encode_muc_user_$items'([], _acc) -> _acc;
'encode_muc_user_$items'([Items | _els], _acc) ->
    'encode_muc_user_$items'(_els,
			     [encode_muc_user_item(Items, []) | _acc]).

'encode_muc_user_$invites'([], _acc) -> _acc;
'encode_muc_user_$invites'([Invites | _els], _acc) ->
    'encode_muc_user_$invites'(_els,
			       [encode_muc_user_invite(Invites, []) | _acc]).

'encode_muc_user_$decline'(undefined, _acc) -> _acc;
'encode_muc_user_$decline'(Decline, _acc) ->
    [encode_muc_user_decline(Decline, []) | _acc].

'encode_muc_user_$destroy'(undefined, _acc) -> _acc;
'encode_muc_user_$destroy'(Destroy, _acc) ->
    [encode_muc_user_destroy(Destroy, []) | _acc].

decode_muc_user_attr_password(undefined) -> undefined;
decode_muc_user_attr_password(_val) -> _val.

encode_muc_user_attr_password(undefined, _acc) -> _acc;
encode_muc_user_attr_password(_val, _acc) ->
    [{<<"password">>, _val} | _acc].

decode_muc_user_item({xmlel, <<"item">>, _attrs,
		      _els}) ->
    {Actor, Continue, Reason} =
	decode_muc_user_item_els(_els, undefined, undefined,
				 undefined),
    {Affiliation, Role, Jid, Nick} =
	decode_muc_user_item_attrs(_attrs, undefined, undefined,
				   undefined, undefined),
    {muc_item, Actor, Continue, Reason, Affiliation, Role,
     Jid, Nick}.

decode_muc_user_item_els([], Actor, Continue, Reason) ->
    {Actor, Continue, Reason};
decode_muc_user_item_els([{xmlel, <<"actor">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Actor, Continue, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/muc#user">> ->
	   decode_muc_user_item_els(_els,
				    decode_muc_user_actor(_el), Continue,
				    Reason);
       true ->
	   decode_muc_user_item_els(_els, Actor, Continue, Reason)
    end;
decode_muc_user_item_els([{xmlel, <<"continue">>,
			   _attrs, _} =
			      _el
			  | _els],
			 Actor, Continue, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/muc#user">> ->
	   decode_muc_user_item_els(_els, Actor,
				    decode_muc_user_continue(_el), Reason);
       true ->
	   decode_muc_user_item_els(_els, Actor, Continue, Reason)
    end;
decode_muc_user_item_els([{xmlel, <<"reason">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Actor, Continue, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/muc#user">> ->
	   decode_muc_user_item_els(_els, Actor, Continue,
				    decode_muc_user_reason(_el));
       true ->
	   decode_muc_user_item_els(_els, Actor, Continue, Reason)
    end;
decode_muc_user_item_els([_ | _els], Actor, Continue,
			 Reason) ->
    decode_muc_user_item_els(_els, Actor, Continue, Reason).

decode_muc_user_item_attrs([{<<"affiliation">>, _val}
			    | _attrs],
			   _Affiliation, Role, Jid, Nick) ->
    decode_muc_user_item_attrs(_attrs, _val, Role, Jid,
			       Nick);
decode_muc_user_item_attrs([{<<"role">>, _val}
			    | _attrs],
			   Affiliation, _Role, Jid, Nick) ->
    decode_muc_user_item_attrs(_attrs, Affiliation, _val,
			       Jid, Nick);
decode_muc_user_item_attrs([{<<"jid">>, _val} | _attrs],
			   Affiliation, Role, _Jid, Nick) ->
    decode_muc_user_item_attrs(_attrs, Affiliation, Role,
			       _val, Nick);
decode_muc_user_item_attrs([{<<"nick">>, _val}
			    | _attrs],
			   Affiliation, Role, Jid, _Nick) ->
    decode_muc_user_item_attrs(_attrs, Affiliation, Role,
			       Jid, _val);
decode_muc_user_item_attrs([_ | _attrs], Affiliation,
			   Role, Jid, Nick) ->
    decode_muc_user_item_attrs(_attrs, Affiliation, Role,
			       Jid, Nick);
decode_muc_user_item_attrs([], Affiliation, Role, Jid,
			   Nick) ->
    {decode_muc_user_item_attr_affiliation(Affiliation),
     decode_muc_user_item_attr_role(Role),
     decode_muc_user_item_attr_jid(Jid),
     decode_muc_user_item_attr_nick(Nick)}.

encode_muc_user_item({muc_item, Actor, Continue, Reason,
		      Affiliation, Role, Jid, Nick},
		     _xmlns_attrs) ->
    _els = 'encode_muc_user_item_$reason'(Reason,
					  'encode_muc_user_item_$continue'(Continue,
									   'encode_muc_user_item_$actor'(Actor,
													 []))),
    _attrs = encode_muc_user_item_attr_nick(Nick,
					    encode_muc_user_item_attr_jid(Jid,
									  encode_muc_user_item_attr_role(Role,
													 encode_muc_user_item_attr_affiliation(Affiliation,
																	       _xmlns_attrs)))),
    {xmlel, <<"item">>, _attrs, _els}.

'encode_muc_user_item_$actor'(undefined, _acc) -> _acc;
'encode_muc_user_item_$actor'(Actor, _acc) ->
    [encode_muc_user_actor(Actor, []) | _acc].

'encode_muc_user_item_$continue'(undefined, _acc) ->
    _acc;
'encode_muc_user_item_$continue'(Continue, _acc) ->
    [encode_muc_user_continue(Continue, []) | _acc].

'encode_muc_user_item_$reason'(undefined, _acc) -> _acc;
'encode_muc_user_item_$reason'(Reason, _acc) ->
    [encode_muc_user_reason(Reason, []) | _acc].

decode_muc_user_item_attr_affiliation(undefined) ->
    undefined;
decode_muc_user_item_attr_affiliation(_val) ->
    case catch dec_enum(_val,
			[admin, member, none, outcast, owner])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"affiliation">>, <<"item">>,
			 <<"http://jabber.org/protocol/muc#user">>}});
      _res -> _res
    end.

encode_muc_user_item_attr_affiliation(undefined,
				      _acc) ->
    _acc;
encode_muc_user_item_attr_affiliation(_val, _acc) ->
    [{<<"affiliation">>, enc_enum(_val)} | _acc].

decode_muc_user_item_attr_role(undefined) -> undefined;
decode_muc_user_item_attr_role(_val) ->
    case catch dec_enum(_val,
			[moderator, none, participant, visitor])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"role">>, <<"item">>,
			 <<"http://jabber.org/protocol/muc#user">>}});
      _res -> _res
    end.

encode_muc_user_item_attr_role(undefined, _acc) -> _acc;
encode_muc_user_item_attr_role(_val, _acc) ->
    [{<<"role">>, enc_enum(_val)} | _acc].

decode_muc_user_item_attr_jid(undefined) -> undefined;
decode_muc_user_item_attr_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"item">>,
			 <<"http://jabber.org/protocol/muc#user">>}});
      _res -> _res
    end.

encode_muc_user_item_attr_jid(undefined, _acc) -> _acc;
encode_muc_user_item_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_muc_user_item_attr_nick(undefined) -> undefined;
decode_muc_user_item_attr_nick(_val) -> _val.

encode_muc_user_item_attr_nick(undefined, _acc) -> _acc;
encode_muc_user_item_attr_nick(_val, _acc) ->
    [{<<"nick">>, _val} | _acc].

decode_muc_user_status({xmlel, <<"status">>, _attrs,
			_els}) ->
    Code = decode_muc_user_status_attrs(_attrs, undefined),
    Code.

decode_muc_user_status_attrs([{<<"code">>, _val}
			      | _attrs],
			     _Code) ->
    decode_muc_user_status_attrs(_attrs, _val);
decode_muc_user_status_attrs([_ | _attrs], Code) ->
    decode_muc_user_status_attrs(_attrs, Code);
decode_muc_user_status_attrs([], Code) ->
    decode_muc_user_status_attr_code(Code).

encode_muc_user_status(Code, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_muc_user_status_attr_code(Code,
					      _xmlns_attrs),
    {xmlel, <<"status">>, _attrs, _els}.

decode_muc_user_status_attr_code(undefined) ->
    undefined;
decode_muc_user_status_attr_code(_val) ->
    case catch dec_int(_val, 100, 999) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"code">>, <<"status">>,
			 <<"http://jabber.org/protocol/muc#user">>}});
      _res -> _res
    end.

encode_muc_user_status_attr_code(undefined, _acc) ->
    _acc;
encode_muc_user_status_attr_code(_val, _acc) ->
    [{<<"code">>, enc_int(_val)} | _acc].

decode_muc_user_continue({xmlel, <<"continue">>, _attrs,
			  _els}) ->
    Thread = decode_muc_user_continue_attrs(_attrs,
					    undefined),
    Thread.

decode_muc_user_continue_attrs([{<<"thread">>, _val}
				| _attrs],
			       _Thread) ->
    decode_muc_user_continue_attrs(_attrs, _val);
decode_muc_user_continue_attrs([_ | _attrs], Thread) ->
    decode_muc_user_continue_attrs(_attrs, Thread);
decode_muc_user_continue_attrs([], Thread) ->
    decode_muc_user_continue_attr_thread(Thread).

encode_muc_user_continue(Thread, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_muc_user_continue_attr_thread(Thread,
						  _xmlns_attrs),
    {xmlel, <<"continue">>, _attrs, _els}.

decode_muc_user_continue_attr_thread(undefined) ->
    undefined;
decode_muc_user_continue_attr_thread(_val) -> _val.

encode_muc_user_continue_attr_thread(undefined, _acc) ->
    _acc;
encode_muc_user_continue_attr_thread(_val, _acc) ->
    [{<<"thread">>, _val} | _acc].

decode_muc_user_actor({xmlel, <<"actor">>, _attrs,
		       _els}) ->
    {Jid, Nick} = decode_muc_user_actor_attrs(_attrs,
					      undefined, undefined),
    {muc_actor, Jid, Nick}.

decode_muc_user_actor_attrs([{<<"jid">>, _val}
			     | _attrs],
			    _Jid, Nick) ->
    decode_muc_user_actor_attrs(_attrs, _val, Nick);
decode_muc_user_actor_attrs([{<<"nick">>, _val}
			     | _attrs],
			    Jid, _Nick) ->
    decode_muc_user_actor_attrs(_attrs, Jid, _val);
decode_muc_user_actor_attrs([_ | _attrs], Jid, Nick) ->
    decode_muc_user_actor_attrs(_attrs, Jid, Nick);
decode_muc_user_actor_attrs([], Jid, Nick) ->
    {decode_muc_user_actor_attr_jid(Jid),
     decode_muc_user_actor_attr_nick(Nick)}.

encode_muc_user_actor({muc_actor, Jid, Nick},
		      _xmlns_attrs) ->
    _els = [],
    _attrs = encode_muc_user_actor_attr_nick(Nick,
					     encode_muc_user_actor_attr_jid(Jid,
									    _xmlns_attrs)),
    {xmlel, <<"actor">>, _attrs, _els}.

decode_muc_user_actor_attr_jid(undefined) -> undefined;
decode_muc_user_actor_attr_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"actor">>,
			 <<"http://jabber.org/protocol/muc#user">>}});
      _res -> _res
    end.

encode_muc_user_actor_attr_jid(undefined, _acc) -> _acc;
encode_muc_user_actor_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_muc_user_actor_attr_nick(undefined) -> undefined;
decode_muc_user_actor_attr_nick(_val) -> _val.

encode_muc_user_actor_attr_nick(undefined, _acc) ->
    _acc;
encode_muc_user_actor_attr_nick(_val, _acc) ->
    [{<<"nick">>, _val} | _acc].

decode_muc_user_invite({xmlel, <<"invite">>, _attrs,
			_els}) ->
    Reason = decode_muc_user_invite_els(_els, undefined),
    {To, From} = decode_muc_user_invite_attrs(_attrs,
					      undefined, undefined),
    {muc_invite, Reason, From, To}.

decode_muc_user_invite_els([], Reason) -> Reason;
decode_muc_user_invite_els([{xmlel, <<"reason">>,
			     _attrs, _} =
				_el
			    | _els],
			   Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/muc#user">> ->
	   decode_muc_user_invite_els(_els,
				      decode_muc_user_reason(_el));
       true -> decode_muc_user_invite_els(_els, Reason)
    end;
decode_muc_user_invite_els([_ | _els], Reason) ->
    decode_muc_user_invite_els(_els, Reason).

decode_muc_user_invite_attrs([{<<"to">>, _val}
			      | _attrs],
			     _To, From) ->
    decode_muc_user_invite_attrs(_attrs, _val, From);
decode_muc_user_invite_attrs([{<<"from">>, _val}
			      | _attrs],
			     To, _From) ->
    decode_muc_user_invite_attrs(_attrs, To, _val);
decode_muc_user_invite_attrs([_ | _attrs], To, From) ->
    decode_muc_user_invite_attrs(_attrs, To, From);
decode_muc_user_invite_attrs([], To, From) ->
    {decode_muc_user_invite_attr_to(To),
     decode_muc_user_invite_attr_from(From)}.

encode_muc_user_invite({muc_invite, Reason, From, To},
		       _xmlns_attrs) ->
    _els = 'encode_muc_user_invite_$reason'(Reason, []),
    _attrs = encode_muc_user_invite_attr_from(From,
					      encode_muc_user_invite_attr_to(To,
									     _xmlns_attrs)),
    {xmlel, <<"invite">>, _attrs, _els}.

'encode_muc_user_invite_$reason'(undefined, _acc) ->
    _acc;
'encode_muc_user_invite_$reason'(Reason, _acc) ->
    [encode_muc_user_reason(Reason, []) | _acc].

decode_muc_user_invite_attr_to(undefined) -> undefined;
decode_muc_user_invite_attr_to(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"to">>, <<"invite">>,
			 <<"http://jabber.org/protocol/muc#user">>}});
      _res -> _res
    end.

encode_muc_user_invite_attr_to(undefined, _acc) -> _acc;
encode_muc_user_invite_attr_to(_val, _acc) ->
    [{<<"to">>, enc_jid(_val)} | _acc].

decode_muc_user_invite_attr_from(undefined) ->
    undefined;
decode_muc_user_invite_attr_from(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"from">>, <<"invite">>,
			 <<"http://jabber.org/protocol/muc#user">>}});
      _res -> _res
    end.

encode_muc_user_invite_attr_from(undefined, _acc) ->
    _acc;
encode_muc_user_invite_attr_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_muc_user_destroy({xmlel, <<"destroy">>, _attrs,
			 _els}) ->
    Reason = decode_muc_user_destroy_els(_els, undefined),
    Jid = decode_muc_user_destroy_attrs(_attrs, undefined),
    {muc_user_destroy, Reason, Jid}.

decode_muc_user_destroy_els([], Reason) -> Reason;
decode_muc_user_destroy_els([{xmlel, <<"reason">>,
			      _attrs, _} =
				 _el
			     | _els],
			    Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/muc#user">> ->
	   decode_muc_user_destroy_els(_els,
				       decode_muc_user_reason(_el));
       true -> decode_muc_user_destroy_els(_els, Reason)
    end;
decode_muc_user_destroy_els([_ | _els], Reason) ->
    decode_muc_user_destroy_els(_els, Reason).

decode_muc_user_destroy_attrs([{<<"jid">>, _val}
			       | _attrs],
			      _Jid) ->
    decode_muc_user_destroy_attrs(_attrs, _val);
decode_muc_user_destroy_attrs([_ | _attrs], Jid) ->
    decode_muc_user_destroy_attrs(_attrs, Jid);
decode_muc_user_destroy_attrs([], Jid) ->
    decode_muc_user_destroy_attr_jid(Jid).

encode_muc_user_destroy({muc_user_destroy, Reason, Jid},
			_xmlns_attrs) ->
    _els = 'encode_muc_user_destroy_$reason'(Reason, []),
    _attrs = encode_muc_user_destroy_attr_jid(Jid,
					      _xmlns_attrs),
    {xmlel, <<"destroy">>, _attrs, _els}.

'encode_muc_user_destroy_$reason'(undefined, _acc) ->
    _acc;
'encode_muc_user_destroy_$reason'(Reason, _acc) ->
    [encode_muc_user_reason(Reason, []) | _acc].

decode_muc_user_destroy_attr_jid(undefined) ->
    undefined;
decode_muc_user_destroy_attr_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"destroy">>,
			 <<"http://jabber.org/protocol/muc#user">>}});
      _res -> _res
    end.

encode_muc_user_destroy_attr_jid(undefined, _acc) ->
    _acc;
encode_muc_user_destroy_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_muc_user_decline({xmlel, <<"decline">>, _attrs,
			 _els}) ->
    Reason = decode_muc_user_decline_els(_els, undefined),
    {To, From} = decode_muc_user_decline_attrs(_attrs,
					       undefined, undefined),
    {muc_decline, Reason, From, To}.

decode_muc_user_decline_els([], Reason) -> Reason;
decode_muc_user_decline_els([{xmlel, <<"reason">>,
			      _attrs, _} =
				 _el
			     | _els],
			    Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/muc#user">> ->
	   decode_muc_user_decline_els(_els,
				       decode_muc_user_reason(_el));
       true -> decode_muc_user_decline_els(_els, Reason)
    end;
decode_muc_user_decline_els([_ | _els], Reason) ->
    decode_muc_user_decline_els(_els, Reason).

decode_muc_user_decline_attrs([{<<"to">>, _val}
			       | _attrs],
			      _To, From) ->
    decode_muc_user_decline_attrs(_attrs, _val, From);
decode_muc_user_decline_attrs([{<<"from">>, _val}
			       | _attrs],
			      To, _From) ->
    decode_muc_user_decline_attrs(_attrs, To, _val);
decode_muc_user_decline_attrs([_ | _attrs], To, From) ->
    decode_muc_user_decline_attrs(_attrs, To, From);
decode_muc_user_decline_attrs([], To, From) ->
    {decode_muc_user_decline_attr_to(To),
     decode_muc_user_decline_attr_from(From)}.

encode_muc_user_decline({muc_decline, Reason, From, To},
			_xmlns_attrs) ->
    _els = 'encode_muc_user_decline_$reason'(Reason, []),
    _attrs = encode_muc_user_decline_attr_from(From,
					       encode_muc_user_decline_attr_to(To,
									       _xmlns_attrs)),
    {xmlel, <<"decline">>, _attrs, _els}.

'encode_muc_user_decline_$reason'(undefined, _acc) ->
    _acc;
'encode_muc_user_decline_$reason'(Reason, _acc) ->
    [encode_muc_user_reason(Reason, []) | _acc].

decode_muc_user_decline_attr_to(undefined) -> undefined;
decode_muc_user_decline_attr_to(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"to">>, <<"decline">>,
			 <<"http://jabber.org/protocol/muc#user">>}});
      _res -> _res
    end.

encode_muc_user_decline_attr_to(undefined, _acc) ->
    _acc;
encode_muc_user_decline_attr_to(_val, _acc) ->
    [{<<"to">>, enc_jid(_val)} | _acc].

decode_muc_user_decline_attr_from(undefined) ->
    undefined;
decode_muc_user_decline_attr_from(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"from">>, <<"decline">>,
			 <<"http://jabber.org/protocol/muc#user">>}});
      _res -> _res
    end.

encode_muc_user_decline_attr_from(undefined, _acc) ->
    _acc;
encode_muc_user_decline_attr_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_muc_user_reason({xmlel, <<"reason">>, _attrs,
			_els}) ->
    Cdata = decode_muc_user_reason_els(_els, <<>>), Cdata.

decode_muc_user_reason_els([], Cdata) ->
    decode_muc_user_reason_cdata(Cdata);
decode_muc_user_reason_els([{xmlcdata, _data} | _els],
			   Cdata) ->
    decode_muc_user_reason_els(_els,
			       <<Cdata/binary, _data/binary>>);
decode_muc_user_reason_els([_ | _els], Cdata) ->
    decode_muc_user_reason_els(_els, Cdata).

encode_muc_user_reason(Cdata, _xmlns_attrs) ->
    _els = encode_muc_user_reason_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"reason">>, _attrs, _els}.

decode_muc_user_reason_cdata(<<>>) -> undefined;
decode_muc_user_reason_cdata(_val) -> _val.

encode_muc_user_reason_cdata(undefined, _acc) -> _acc;
encode_muc_user_reason_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_muc_history({xmlel, <<"history">>, _attrs,
		    _els}) ->
    {Maxchars, Maxstanzas, Seconds, Since} =
	decode_muc_history_attrs(_attrs, undefined, undefined,
				 undefined, undefined),
    {muc_history, Maxchars, Maxstanzas, Seconds, Since}.

decode_muc_history_attrs([{<<"maxchars">>, _val}
			  | _attrs],
			 _Maxchars, Maxstanzas, Seconds, Since) ->
    decode_muc_history_attrs(_attrs, _val, Maxstanzas,
			     Seconds, Since);
decode_muc_history_attrs([{<<"maxstanzas">>, _val}
			  | _attrs],
			 Maxchars, _Maxstanzas, Seconds, Since) ->
    decode_muc_history_attrs(_attrs, Maxchars, _val,
			     Seconds, Since);
decode_muc_history_attrs([{<<"seconds">>, _val}
			  | _attrs],
			 Maxchars, Maxstanzas, _Seconds, Since) ->
    decode_muc_history_attrs(_attrs, Maxchars, Maxstanzas,
			     _val, Since);
decode_muc_history_attrs([{<<"since">>, _val} | _attrs],
			 Maxchars, Maxstanzas, Seconds, _Since) ->
    decode_muc_history_attrs(_attrs, Maxchars, Maxstanzas,
			     Seconds, _val);
decode_muc_history_attrs([_ | _attrs], Maxchars,
			 Maxstanzas, Seconds, Since) ->
    decode_muc_history_attrs(_attrs, Maxchars, Maxstanzas,
			     Seconds, Since);
decode_muc_history_attrs([], Maxchars, Maxstanzas,
			 Seconds, Since) ->
    {decode_muc_history_attr_maxchars(Maxchars),
     decode_muc_history_attr_maxstanzas(Maxstanzas),
     decode_muc_history_attr_seconds(Seconds),
     decode_muc_history_attr_since(Since)}.

encode_muc_history({muc_history, Maxchars, Maxstanzas,
		    Seconds, Since},
		   _xmlns_attrs) ->
    _els = [],
    _attrs = encode_muc_history_attr_since(Since,
					   encode_muc_history_attr_seconds(Seconds,
									   encode_muc_history_attr_maxstanzas(Maxstanzas,
													      encode_muc_history_attr_maxchars(Maxchars,
																	       _xmlns_attrs)))),
    {xmlel, <<"history">>, _attrs, _els}.

decode_muc_history_attr_maxchars(undefined) ->
    undefined;
decode_muc_history_attr_maxchars(_val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"maxchars">>, <<"history">>,
			 <<"http://jabber.org/protocol/muc">>}});
      _res -> _res
    end.

encode_muc_history_attr_maxchars(undefined, _acc) ->
    _acc;
encode_muc_history_attr_maxchars(_val, _acc) ->
    [{<<"maxchars">>, enc_int(_val)} | _acc].

decode_muc_history_attr_maxstanzas(undefined) ->
    undefined;
decode_muc_history_attr_maxstanzas(_val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"maxstanzas">>, <<"history">>,
			 <<"http://jabber.org/protocol/muc">>}});
      _res -> _res
    end.

encode_muc_history_attr_maxstanzas(undefined, _acc) ->
    _acc;
encode_muc_history_attr_maxstanzas(_val, _acc) ->
    [{<<"maxstanzas">>, enc_int(_val)} | _acc].

decode_muc_history_attr_seconds(undefined) -> undefined;
decode_muc_history_attr_seconds(_val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"seconds">>, <<"history">>,
			 <<"http://jabber.org/protocol/muc">>}});
      _res -> _res
    end.

encode_muc_history_attr_seconds(undefined, _acc) ->
    _acc;
encode_muc_history_attr_seconds(_val, _acc) ->
    [{<<"seconds">>, enc_int(_val)} | _acc].

decode_muc_history_attr_since(undefined) -> undefined;
decode_muc_history_attr_since(_val) ->
    case catch dec_utc(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"since">>, <<"history">>,
			 <<"http://jabber.org/protocol/muc">>}});
      _res -> _res
    end.

encode_muc_history_attr_since(undefined, _acc) -> _acc;
encode_muc_history_attr_since(_val, _acc) ->
    [{<<"since">>, enc_utc(_val)} | _acc].

decode_bytestreams({xmlel, <<"query">>, _attrs,
		    _els}) ->
    {Hosts, Used, Activate} = decode_bytestreams_els(_els,
						     [], undefined, undefined),
    {Dstaddr, Sid, Mode} = decode_bytestreams_attrs(_attrs,
						    undefined, undefined,
						    undefined),
    {bytestreams, Hosts, Used, Activate, Dstaddr, Mode,
     Sid}.

decode_bytestreams_els([], Hosts, Used, Activate) ->
    {lists:reverse(Hosts), Used, Activate};
decode_bytestreams_els([{xmlel, <<"streamhost">>,
			 _attrs, _} =
			    _el
			| _els],
		       Hosts, Used, Activate) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns ==
	 <<"http://jabber.org/protocol/bytestreams">> ->
	   decode_bytestreams_els(_els,
				  [decode_bytestreams_streamhost(_el) | Hosts],
				  Used, Activate);
       true ->
	   decode_bytestreams_els(_els, Hosts, Used, Activate)
    end;
decode_bytestreams_els([{xmlel, <<"streamhost-used">>,
			 _attrs, _} =
			    _el
			| _els],
		       Hosts, Used, Activate) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns ==
	 <<"http://jabber.org/protocol/bytestreams">> ->
	   decode_bytestreams_els(_els, Hosts,
				  decode_bytestreams_streamhost_used(_el),
				  Activate);
       true ->
	   decode_bytestreams_els(_els, Hosts, Used, Activate)
    end;
decode_bytestreams_els([{xmlel, <<"activate">>, _attrs,
			 _} =
			    _el
			| _els],
		       Hosts, Used, Activate) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns ==
	 <<"http://jabber.org/protocol/bytestreams">> ->
	   decode_bytestreams_els(_els, Hosts, Used,
				  decode_bytestreams_activate(_el));
       true ->
	   decode_bytestreams_els(_els, Hosts, Used, Activate)
    end;
decode_bytestreams_els([_ | _els], Hosts, Used,
		       Activate) ->
    decode_bytestreams_els(_els, Hosts, Used, Activate).

decode_bytestreams_attrs([{<<"dstaddr">>, _val}
			  | _attrs],
			 _Dstaddr, Sid, Mode) ->
    decode_bytestreams_attrs(_attrs, _val, Sid, Mode);
decode_bytestreams_attrs([{<<"sid">>, _val} | _attrs],
			 Dstaddr, _Sid, Mode) ->
    decode_bytestreams_attrs(_attrs, Dstaddr, _val, Mode);
decode_bytestreams_attrs([{<<"mode">>, _val} | _attrs],
			 Dstaddr, Sid, _Mode) ->
    decode_bytestreams_attrs(_attrs, Dstaddr, Sid, _val);
decode_bytestreams_attrs([_ | _attrs], Dstaddr, Sid,
			 Mode) ->
    decode_bytestreams_attrs(_attrs, Dstaddr, Sid, Mode);
decode_bytestreams_attrs([], Dstaddr, Sid, Mode) ->
    {decode_bytestreams_attr_dstaddr(Dstaddr),
     decode_bytestreams_attr_sid(Sid),
     decode_bytestreams_attr_mode(Mode)}.

encode_bytestreams({bytestreams, Hosts, Used, Activate,
		    Dstaddr, Mode, Sid},
		   _xmlns_attrs) ->
    _els = 'encode_bytestreams_$activate'(Activate,
					  'encode_bytestreams_$used'(Used,
								     'encode_bytestreams_$hosts'(Hosts,
												 []))),
    _attrs = encode_bytestreams_attr_mode(Mode,
					  encode_bytestreams_attr_sid(Sid,
								      encode_bytestreams_attr_dstaddr(Dstaddr,
												      _xmlns_attrs))),
    {xmlel, <<"query">>, _attrs, _els}.

'encode_bytestreams_$hosts'([], _acc) -> _acc;
'encode_bytestreams_$hosts'([Hosts | _els], _acc) ->
    'encode_bytestreams_$hosts'(_els,
				[encode_bytestreams_streamhost(Hosts, [])
				 | _acc]).

'encode_bytestreams_$used'(undefined, _acc) -> _acc;
'encode_bytestreams_$used'(Used, _acc) ->
    [encode_bytestreams_streamhost_used(Used, []) | _acc].

'encode_bytestreams_$activate'(undefined, _acc) -> _acc;
'encode_bytestreams_$activate'(Activate, _acc) ->
    [encode_bytestreams_activate(Activate, []) | _acc].

decode_bytestreams_attr_dstaddr(undefined) -> undefined;
decode_bytestreams_attr_dstaddr(_val) -> _val.

encode_bytestreams_attr_dstaddr(undefined, _acc) ->
    _acc;
encode_bytestreams_attr_dstaddr(_val, _acc) ->
    [{<<"dstaddr">>, _val} | _acc].

decode_bytestreams_attr_sid(undefined) -> undefined;
decode_bytestreams_attr_sid(_val) -> _val.

encode_bytestreams_attr_sid(undefined, _acc) -> _acc;
encode_bytestreams_attr_sid(_val, _acc) ->
    [{<<"sid">>, _val} | _acc].

decode_bytestreams_attr_mode(undefined) -> tcp;
decode_bytestreams_attr_mode(_val) ->
    case catch dec_enum(_val, [tcp, udp]) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"mode">>, <<"query">>,
			 <<"http://jabber.org/protocol/bytestreams">>}});
      _res -> _res
    end.

encode_bytestreams_attr_mode(tcp, _acc) -> _acc;
encode_bytestreams_attr_mode(_val, _acc) ->
    [{<<"mode">>, enc_enum(_val)} | _acc].

decode_bytestreams_activate({xmlel, <<"activate">>,
			     _attrs, _els}) ->
    Cdata = decode_bytestreams_activate_els(_els, <<>>),
    Cdata.

decode_bytestreams_activate_els([], Cdata) ->
    decode_bytestreams_activate_cdata(Cdata);
decode_bytestreams_activate_els([{xmlcdata, _data}
				 | _els],
				Cdata) ->
    decode_bytestreams_activate_els(_els,
				    <<Cdata/binary, _data/binary>>);
decode_bytestreams_activate_els([_ | _els], Cdata) ->
    decode_bytestreams_activate_els(_els, Cdata).

encode_bytestreams_activate(Cdata, _xmlns_attrs) ->
    _els = encode_bytestreams_activate_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"activate">>, _attrs, _els}.

decode_bytestreams_activate_cdata(<<>>) -> undefined;
decode_bytestreams_activate_cdata(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"activate">>,
			 <<"http://jabber.org/protocol/bytestreams">>}});
      _res -> _res
    end.

encode_bytestreams_activate_cdata(undefined, _acc) ->
    _acc;
encode_bytestreams_activate_cdata(_val, _acc) ->
    [{xmlcdata, enc_jid(_val)} | _acc].

decode_bytestreams_streamhost_used({xmlel,
				    <<"streamhost-used">>, _attrs, _els}) ->
    Jid = decode_bytestreams_streamhost_used_attrs(_attrs,
						   undefined),
    Jid.

decode_bytestreams_streamhost_used_attrs([{<<"jid">>,
					   _val}
					  | _attrs],
					 _Jid) ->
    decode_bytestreams_streamhost_used_attrs(_attrs, _val);
decode_bytestreams_streamhost_used_attrs([_ | _attrs],
					 Jid) ->
    decode_bytestreams_streamhost_used_attrs(_attrs, Jid);
decode_bytestreams_streamhost_used_attrs([], Jid) ->
    decode_bytestreams_streamhost_used_attr_jid(Jid).

encode_bytestreams_streamhost_used(Jid, _xmlns_attrs) ->
    _els = [],
    _attrs =
	encode_bytestreams_streamhost_used_attr_jid(Jid,
						    _xmlns_attrs),
    {xmlel, <<"streamhost-used">>, _attrs, _els}.

decode_bytestreams_streamhost_used_attr_jid(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"streamhost-used">>,
		   <<"http://jabber.org/protocol/bytestreams">>}});
decode_bytestreams_streamhost_used_attr_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"streamhost-used">>,
			 <<"http://jabber.org/protocol/bytestreams">>}});
      _res -> _res
    end.

encode_bytestreams_streamhost_used_attr_jid(_val,
					    _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_bytestreams_streamhost({xmlel, <<"streamhost">>,
			       _attrs, _els}) ->
    {Jid, Host, Port} =
	decode_bytestreams_streamhost_attrs(_attrs, undefined,
					    undefined, undefined),
    {streamhost, Jid, Host, Port}.

decode_bytestreams_streamhost_attrs([{<<"jid">>, _val}
				     | _attrs],
				    _Jid, Host, Port) ->
    decode_bytestreams_streamhost_attrs(_attrs, _val, Host,
					Port);
decode_bytestreams_streamhost_attrs([{<<"host">>, _val}
				     | _attrs],
				    Jid, _Host, Port) ->
    decode_bytestreams_streamhost_attrs(_attrs, Jid, _val,
					Port);
decode_bytestreams_streamhost_attrs([{<<"port">>, _val}
				     | _attrs],
				    Jid, Host, _Port) ->
    decode_bytestreams_streamhost_attrs(_attrs, Jid, Host,
					_val);
decode_bytestreams_streamhost_attrs([_ | _attrs], Jid,
				    Host, Port) ->
    decode_bytestreams_streamhost_attrs(_attrs, Jid, Host,
					Port);
decode_bytestreams_streamhost_attrs([], Jid, Host,
				    Port) ->
    {decode_bytestreams_streamhost_attr_jid(Jid),
     decode_bytestreams_streamhost_attr_host(Host),
     decode_bytestreams_streamhost_attr_port(Port)}.

encode_bytestreams_streamhost({streamhost, Jid, Host,
			       Port},
			      _xmlns_attrs) ->
    _els = [],
    _attrs = encode_bytestreams_streamhost_attr_port(Port,
						     encode_bytestreams_streamhost_attr_host(Host,
											     encode_bytestreams_streamhost_attr_jid(Jid,
																    _xmlns_attrs))),
    {xmlel, <<"streamhost">>, _attrs, _els}.

decode_bytestreams_streamhost_attr_jid(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"streamhost">>,
		   <<"http://jabber.org/protocol/bytestreams">>}});
decode_bytestreams_streamhost_attr_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"streamhost">>,
			 <<"http://jabber.org/protocol/bytestreams">>}});
      _res -> _res
    end.

encode_bytestreams_streamhost_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_bytestreams_streamhost_attr_host(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"host">>, <<"streamhost">>,
		   <<"http://jabber.org/protocol/bytestreams">>}});
decode_bytestreams_streamhost_attr_host(_val) -> _val.

encode_bytestreams_streamhost_attr_host(_val, _acc) ->
    [{<<"host">>, _val} | _acc].

decode_bytestreams_streamhost_attr_port(undefined) ->
    1080;
decode_bytestreams_streamhost_attr_port(_val) ->
    case catch dec_int(_val, 0, 65535) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"port">>, <<"streamhost">>,
			 <<"http://jabber.org/protocol/bytestreams">>}});
      _res -> _res
    end.

encode_bytestreams_streamhost_attr_port(1080, _acc) ->
    _acc;
encode_bytestreams_streamhost_attr_port(_val, _acc) ->
    [{<<"port">>, enc_int(_val)} | _acc].

decode_legacy_delay({xmlel, <<"x">>, _attrs, _els}) ->
    {Stamp, From} = decode_legacy_delay_attrs(_attrs,
					      undefined, undefined),
    {legacy_delay, Stamp, From}.

decode_legacy_delay_attrs([{<<"stamp">>, _val}
			   | _attrs],
			  _Stamp, From) ->
    decode_legacy_delay_attrs(_attrs, _val, From);
decode_legacy_delay_attrs([{<<"from">>, _val} | _attrs],
			  Stamp, _From) ->
    decode_legacy_delay_attrs(_attrs, Stamp, _val);
decode_legacy_delay_attrs([_ | _attrs], Stamp, From) ->
    decode_legacy_delay_attrs(_attrs, Stamp, From);
decode_legacy_delay_attrs([], Stamp, From) ->
    {decode_legacy_delay_attr_stamp(Stamp),
     decode_legacy_delay_attr_from(From)}.

encode_legacy_delay({legacy_delay, Stamp, From},
		    _xmlns_attrs) ->
    _els = [],
    _attrs = encode_legacy_delay_attr_from(From,
					   encode_legacy_delay_attr_stamp(Stamp,
									  _xmlns_attrs)),
    {xmlel, <<"x">>, _attrs, _els}.

decode_legacy_delay_attr_stamp(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"stamp">>, <<"x">>,
		   <<"jabber:x:delay">>}});
decode_legacy_delay_attr_stamp(_val) -> _val.

encode_legacy_delay_attr_stamp(_val, _acc) ->
    [{<<"stamp">>, _val} | _acc].

decode_legacy_delay_attr_from(undefined) -> undefined;
decode_legacy_delay_attr_from(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"from">>, <<"x">>,
			 <<"jabber:x:delay">>}});
      _res -> _res
    end.

encode_legacy_delay_attr_from(undefined, _acc) -> _acc;
encode_legacy_delay_attr_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_delay({xmlel, <<"delay">>, _attrs, _els}) ->
    {Stamp, From} = decode_delay_attrs(_attrs, undefined,
				       undefined),
    {delay, Stamp, From}.

decode_delay_attrs([{<<"stamp">>, _val} | _attrs],
		   _Stamp, From) ->
    decode_delay_attrs(_attrs, _val, From);
decode_delay_attrs([{<<"from">>, _val} | _attrs], Stamp,
		   _From) ->
    decode_delay_attrs(_attrs, Stamp, _val);
decode_delay_attrs([_ | _attrs], Stamp, From) ->
    decode_delay_attrs(_attrs, Stamp, From);
decode_delay_attrs([], Stamp, From) ->
    {decode_delay_attr_stamp(Stamp),
     decode_delay_attr_from(From)}.

encode_delay({delay, Stamp, From}, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_delay_attr_from(From,
				    encode_delay_attr_stamp(Stamp,
							    _xmlns_attrs)),
    {xmlel, <<"delay">>, _attrs, _els}.

decode_delay_attr_stamp(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"stamp">>, <<"delay">>,
		   <<"urn:xmpp:delay">>}});
decode_delay_attr_stamp(_val) ->
    case catch dec_utc(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"stamp">>, <<"delay">>,
			 <<"urn:xmpp:delay">>}});
      _res -> _res
    end.

encode_delay_attr_stamp(_val, _acc) ->
    [{<<"stamp">>, enc_utc(_val)} | _acc].

decode_delay_attr_from(undefined) -> undefined;
decode_delay_attr_from(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"from">>, <<"delay">>,
			 <<"urn:xmpp:delay">>}});
      _res -> _res
    end.

encode_delay_attr_from(undefined, _acc) -> _acc;
encode_delay_attr_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_shim_headers({xmlel, <<"headers">>, _attrs,
		     _els}) ->
    Headers = decode_shim_headers_els(_els, []),
    {shim, Headers}.

decode_shim_headers_els([], Headers) ->
    lists:reverse(Headers);
decode_shim_headers_els([{xmlel, <<"header">>, _attrs,
			  _} =
			     _el
			 | _els],
			Headers) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/shim">> ->
	   decode_shim_headers_els(_els,
				   [decode_shim_header(_el) | Headers]);
       true -> decode_shim_headers_els(_els, Headers)
    end;
decode_shim_headers_els([_ | _els], Headers) ->
    decode_shim_headers_els(_els, Headers).

encode_shim_headers({shim, Headers}, _xmlns_attrs) ->
    _els = 'encode_shim_headers_$headers'(Headers, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"headers">>, _attrs, _els}.

'encode_shim_headers_$headers'([], _acc) -> _acc;
'encode_shim_headers_$headers'([Headers | _els],
			       _acc) ->
    'encode_shim_headers_$headers'(_els,
				   [encode_shim_header(Headers, []) | _acc]).

decode_shim_header({xmlel, <<"header">>, _attrs,
		    _els}) ->
    Cdata = decode_shim_header_els(_els, <<>>),
    Name = decode_shim_header_attrs(_attrs, undefined),
    {Name, Cdata}.

decode_shim_header_els([], Cdata) ->
    decode_shim_header_cdata(Cdata);
decode_shim_header_els([{xmlcdata, _data} | _els],
		       Cdata) ->
    decode_shim_header_els(_els,
			   <<Cdata/binary, _data/binary>>);
decode_shim_header_els([_ | _els], Cdata) ->
    decode_shim_header_els(_els, Cdata).

decode_shim_header_attrs([{<<"name">>, _val} | _attrs],
			 _Name) ->
    decode_shim_header_attrs(_attrs, _val);
decode_shim_header_attrs([_ | _attrs], Name) ->
    decode_shim_header_attrs(_attrs, Name);
decode_shim_header_attrs([], Name) ->
    decode_shim_header_attr_name(Name).

encode_shim_header({Name, Cdata}, _xmlns_attrs) ->
    _els = encode_shim_header_cdata(Cdata, []),
    _attrs = encode_shim_header_attr_name(Name,
					  _xmlns_attrs),
    {xmlel, <<"header">>, _attrs, _els}.

decode_shim_header_attr_name(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"name">>, <<"header">>,
		   <<"http://jabber.org/protocol/shim">>}});
decode_shim_header_attr_name(_val) -> _val.

encode_shim_header_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_shim_header_cdata(<<>>) -> undefined;
decode_shim_header_cdata(_val) -> _val.

encode_shim_header_cdata(undefined, _acc) -> _acc;
encode_shim_header_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_pubsub({xmlel, <<"pubsub">>, _attrs, _els}) ->
    {Items, Options, Affiliations, Subscriptions, Retract,
     Unsubscribe, Subscribe, Publish} =
	decode_pubsub_els(_els, undefined, undefined, undefined,
			  undefined, undefined, undefined, undefined,
			  undefined),
    {pubsub, Subscriptions, Affiliations, Publish,
     Subscribe, Unsubscribe, Options, Items, Retract}.

decode_pubsub_els([], Items, Options, Affiliations,
		  Subscriptions, Retract, Unsubscribe, Subscribe,
		  Publish) ->
    {Items, Options, Affiliations, Subscriptions, Retract,
     Unsubscribe, Subscribe, Publish};
decode_pubsub_els([{xmlel, <<"subscriptions">>, _attrs,
		    _} =
		       _el
		   | _els],
		  Items, Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/pubsub">> ->
	   decode_pubsub_els(_els, Items, Options, Affiliations,
			     decode_pubsub_subscriptions(_el), Retract,
			     Unsubscribe, Subscribe, Publish);
       true ->
	   decode_pubsub_els(_els, Items, Options, Affiliations,
			     Subscriptions, Retract, Unsubscribe, Subscribe,
			     Publish)
    end;
decode_pubsub_els([{xmlel, <<"affiliations">>, _attrs,
		    _} =
		       _el
		   | _els],
		  Items, Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/pubsub">> ->
	   decode_pubsub_els(_els, Items, Options,
			     decode_pubsub_affiliations(_el), Subscriptions,
			     Retract, Unsubscribe, Subscribe, Publish);
       true ->
	   decode_pubsub_els(_els, Items, Options, Affiliations,
			     Subscriptions, Retract, Unsubscribe, Subscribe,
			     Publish)
    end;
decode_pubsub_els([{xmlel, <<"subscribe">>, _attrs, _} =
		       _el
		   | _els],
		  Items, Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/pubsub">> ->
	   decode_pubsub_els(_els, Items, Options, Affiliations,
			     Subscriptions, Retract, Unsubscribe,
			     decode_pubsub_subscribe(_el), Publish);
       true ->
	   decode_pubsub_els(_els, Items, Options, Affiliations,
			     Subscriptions, Retract, Unsubscribe, Subscribe,
			     Publish)
    end;
decode_pubsub_els([{xmlel, <<"unsubscribe">>, _attrs,
		    _} =
		       _el
		   | _els],
		  Items, Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/pubsub">> ->
	   decode_pubsub_els(_els, Items, Options, Affiliations,
			     Subscriptions, Retract,
			     decode_pubsub_unsubscribe(_el), Subscribe,
			     Publish);
       true ->
	   decode_pubsub_els(_els, Items, Options, Affiliations,
			     Subscriptions, Retract, Unsubscribe, Subscribe,
			     Publish)
    end;
decode_pubsub_els([{xmlel, <<"options">>, _attrs, _} =
		       _el
		   | _els],
		  Items, Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/pubsub">> ->
	   decode_pubsub_els(_els, Items,
			     decode_pubsub_options(_el), Affiliations,
			     Subscriptions, Retract, Unsubscribe, Subscribe,
			     Publish);
       true ->
	   decode_pubsub_els(_els, Items, Options, Affiliations,
			     Subscriptions, Retract, Unsubscribe, Subscribe,
			     Publish)
    end;
decode_pubsub_els([{xmlel, <<"items">>, _attrs, _} = _el
		   | _els],
		  Items, Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/pubsub">> ->
	   decode_pubsub_els(_els, decode_pubsub_items(_el),
			     Options, Affiliations, Subscriptions, Retract,
			     Unsubscribe, Subscribe, Publish);
       true ->
	   decode_pubsub_els(_els, Items, Options, Affiliations,
			     Subscriptions, Retract, Unsubscribe, Subscribe,
			     Publish)
    end;
decode_pubsub_els([{xmlel, <<"retract">>, _attrs, _} =
		       _el
		   | _els],
		  Items, Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/pubsub">> ->
	   decode_pubsub_els(_els, Items, Options, Affiliations,
			     Subscriptions, decode_pubsub_retract(_el),
			     Unsubscribe, Subscribe, Publish);
       true ->
	   decode_pubsub_els(_els, Items, Options, Affiliations,
			     Subscriptions, Retract, Unsubscribe, Subscribe,
			     Publish)
    end;
decode_pubsub_els([{xmlel, <<"publish">>, _attrs, _} =
		       _el
		   | _els],
		  Items, Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/pubsub">> ->
	   decode_pubsub_els(_els, Items, Options, Affiliations,
			     Subscriptions, Retract, Unsubscribe, Subscribe,
			     decode_pubsub_publish(_el));
       true ->
	   decode_pubsub_els(_els, Items, Options, Affiliations,
			     Subscriptions, Retract, Unsubscribe, Subscribe,
			     Publish)
    end;
decode_pubsub_els([_ | _els], Items, Options,
		  Affiliations, Subscriptions, Retract, Unsubscribe,
		  Subscribe, Publish) ->
    decode_pubsub_els(_els, Items, Options, Affiliations,
		      Subscriptions, Retract, Unsubscribe, Subscribe,
		      Publish).

encode_pubsub({pubsub, Subscriptions, Affiliations,
	       Publish, Subscribe, Unsubscribe, Options, Items,
	       Retract},
	      _xmlns_attrs) ->
    _els = 'encode_pubsub_$publish'(Publish,
				    'encode_pubsub_$subscribe'(Subscribe,
							       'encode_pubsub_$unsubscribe'(Unsubscribe,
											    'encode_pubsub_$retract'(Retract,
														     'encode_pubsub_$subscriptions'(Subscriptions,
																		    'encode_pubsub_$affiliations'(Affiliations,
																						  'encode_pubsub_$options'(Options,
																									   'encode_pubsub_$items'(Items,
																												  [])))))))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"pubsub">>, _attrs, _els}.

'encode_pubsub_$items'(undefined, _acc) -> _acc;
'encode_pubsub_$items'(Items, _acc) ->
    [encode_pubsub_items(Items, []) | _acc].

'encode_pubsub_$options'(undefined, _acc) -> _acc;
'encode_pubsub_$options'(Options, _acc) ->
    [encode_pubsub_options(Options, []) | _acc].

'encode_pubsub_$affiliations'(undefined, _acc) -> _acc;
'encode_pubsub_$affiliations'(Affiliations, _acc) ->
    [encode_pubsub_affiliations(Affiliations, []) | _acc].

'encode_pubsub_$subscriptions'(undefined, _acc) -> _acc;
'encode_pubsub_$subscriptions'(Subscriptions, _acc) ->
    [encode_pubsub_subscriptions(Subscriptions, []) | _acc].

'encode_pubsub_$retract'(undefined, _acc) -> _acc;
'encode_pubsub_$retract'(Retract, _acc) ->
    [encode_pubsub_retract(Retract, []) | _acc].

'encode_pubsub_$unsubscribe'(undefined, _acc) -> _acc;
'encode_pubsub_$unsubscribe'(Unsubscribe, _acc) ->
    [encode_pubsub_unsubscribe(Unsubscribe, []) | _acc].

'encode_pubsub_$subscribe'(undefined, _acc) -> _acc;
'encode_pubsub_$subscribe'(Subscribe, _acc) ->
    [encode_pubsub_subscribe(Subscribe, []) | _acc].

'encode_pubsub_$publish'(undefined, _acc) -> _acc;
'encode_pubsub_$publish'(Publish, _acc) ->
    [encode_pubsub_publish(Publish, []) | _acc].

decode_pubsub_retract({xmlel, <<"retract">>, _attrs,
		       _els}) ->
    Items = decode_pubsub_retract_els(_els, []),
    {Node, Notify} = decode_pubsub_retract_attrs(_attrs,
						 undefined, undefined),
    {pubsub_retract, Node, Notify, Items}.

decode_pubsub_retract_els([], Items) ->
    lists:reverse(Items);
decode_pubsub_retract_els([{xmlel, <<"item">>, _attrs,
			    _} =
			       _el
			   | _els],
			  Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/pubsub">> ->
	   decode_pubsub_retract_els(_els,
				     [decode_pubsub_item(_el) | Items]);
       true -> decode_pubsub_retract_els(_els, Items)
    end;
decode_pubsub_retract_els([_ | _els], Items) ->
    decode_pubsub_retract_els(_els, Items).

decode_pubsub_retract_attrs([{<<"node">>, _val}
			     | _attrs],
			    _Node, Notify) ->
    decode_pubsub_retract_attrs(_attrs, _val, Notify);
decode_pubsub_retract_attrs([{<<"notify">>, _val}
			     | _attrs],
			    Node, _Notify) ->
    decode_pubsub_retract_attrs(_attrs, Node, _val);
decode_pubsub_retract_attrs([_ | _attrs], Node,
			    Notify) ->
    decode_pubsub_retract_attrs(_attrs, Node, Notify);
decode_pubsub_retract_attrs([], Node, Notify) ->
    {decode_pubsub_retract_attr_node(Node),
     decode_pubsub_retract_attr_notify(Notify)}.

encode_pubsub_retract({pubsub_retract, Node, Notify,
		       Items},
		      _xmlns_attrs) ->
    _els = 'encode_pubsub_retract_$items'(Items, []),
    _attrs = encode_pubsub_retract_attr_notify(Notify,
					       encode_pubsub_retract_attr_node(Node,
									       _xmlns_attrs)),
    {xmlel, <<"retract">>, _attrs, _els}.

'encode_pubsub_retract_$items'([], _acc) -> _acc;
'encode_pubsub_retract_$items'([Items | _els], _acc) ->
    'encode_pubsub_retract_$items'(_els,
				   [encode_pubsub_item(Items, []) | _acc]).

decode_pubsub_retract_attr_node(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"node">>, <<"retract">>,
		   <<"http://jabber.org/protocol/pubsub">>}});
decode_pubsub_retract_attr_node(_val) -> _val.

encode_pubsub_retract_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_retract_attr_notify(undefined) -> false;
decode_pubsub_retract_attr_notify(_val) ->
    case catch dec_bool(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"notify">>, <<"retract">>,
			 <<"http://jabber.org/protocol/pubsub">>}});
      _res -> _res
    end.

encode_pubsub_retract_attr_notify(false, _acc) -> _acc;
encode_pubsub_retract_attr_notify(_val, _acc) ->
    [{<<"notify">>, enc_bool(_val)} | _acc].

decode_pubsub_options({xmlel, <<"options">>, _attrs,
		       _els}) ->
    Xdata = decode_pubsub_options_els(_els, undefined),
    {Node, Subid, Jid} = decode_pubsub_options_attrs(_attrs,
						     undefined, undefined,
						     undefined),
    {pubsub_options, Node, Jid, Subid, Xdata}.

decode_pubsub_options_els([], Xdata) -> Xdata;
decode_pubsub_options_els([{xmlel, <<"x">>, _attrs, _} =
			       _el
			   | _els],
			  Xdata) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<"jabber:x:data">> ->
	   decode_pubsub_options_els(_els, decode_xdata(_el));
       true -> decode_pubsub_options_els(_els, Xdata)
    end;
decode_pubsub_options_els([_ | _els], Xdata) ->
    decode_pubsub_options_els(_els, Xdata).

decode_pubsub_options_attrs([{<<"node">>, _val}
			     | _attrs],
			    _Node, Subid, Jid) ->
    decode_pubsub_options_attrs(_attrs, _val, Subid, Jid);
decode_pubsub_options_attrs([{<<"subid">>, _val}
			     | _attrs],
			    Node, _Subid, Jid) ->
    decode_pubsub_options_attrs(_attrs, Node, _val, Jid);
decode_pubsub_options_attrs([{<<"jid">>, _val}
			     | _attrs],
			    Node, Subid, _Jid) ->
    decode_pubsub_options_attrs(_attrs, Node, Subid, _val);
decode_pubsub_options_attrs([_ | _attrs], Node, Subid,
			    Jid) ->
    decode_pubsub_options_attrs(_attrs, Node, Subid, Jid);
decode_pubsub_options_attrs([], Node, Subid, Jid) ->
    {decode_pubsub_options_attr_node(Node),
     decode_pubsub_options_attr_subid(Subid),
     decode_pubsub_options_attr_jid(Jid)}.

encode_pubsub_options({pubsub_options, Node, Jid, Subid,
		       Xdata},
		      _xmlns_attrs) ->
    _els = 'encode_pubsub_options_$xdata'(Xdata, []),
    _attrs = encode_pubsub_options_attr_jid(Jid,
					    encode_pubsub_options_attr_subid(Subid,
									     encode_pubsub_options_attr_node(Node,
													     _xmlns_attrs))),
    {xmlel, <<"options">>, _attrs, _els}.

'encode_pubsub_options_$xdata'(undefined, _acc) -> _acc;
'encode_pubsub_options_$xdata'(Xdata, _acc) ->
    [encode_xdata(Xdata,
		  [{<<"xmlns">>, <<"jabber:x:data">>}])
     | _acc].

decode_pubsub_options_attr_node(undefined) -> undefined;
decode_pubsub_options_attr_node(_val) -> _val.

encode_pubsub_options_attr_node(undefined, _acc) ->
    _acc;
encode_pubsub_options_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_options_attr_subid(undefined) ->
    undefined;
decode_pubsub_options_attr_subid(_val) -> _val.

encode_pubsub_options_attr_subid(undefined, _acc) ->
    _acc;
encode_pubsub_options_attr_subid(_val, _acc) ->
    [{<<"subid">>, _val} | _acc].

decode_pubsub_options_attr_jid(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"options">>,
		   <<"http://jabber.org/protocol/pubsub">>}});
decode_pubsub_options_attr_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"options">>,
			 <<"http://jabber.org/protocol/pubsub">>}});
      _res -> _res
    end.

encode_pubsub_options_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_pubsub_publish({xmlel, <<"publish">>, _attrs,
		       _els}) ->
    Items = decode_pubsub_publish_els(_els, []),
    Node = decode_pubsub_publish_attrs(_attrs, undefined),
    {pubsub_publish, Node, Items}.

decode_pubsub_publish_els([], Items) ->
    lists:reverse(Items);
decode_pubsub_publish_els([{xmlel, <<"item">>, _attrs,
			    _} =
			       _el
			   | _els],
			  Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/pubsub">> ->
	   decode_pubsub_publish_els(_els,
				     [decode_pubsub_item(_el) | Items]);
       true -> decode_pubsub_publish_els(_els, Items)
    end;
decode_pubsub_publish_els([_ | _els], Items) ->
    decode_pubsub_publish_els(_els, Items).

decode_pubsub_publish_attrs([{<<"node">>, _val}
			     | _attrs],
			    _Node) ->
    decode_pubsub_publish_attrs(_attrs, _val);
decode_pubsub_publish_attrs([_ | _attrs], Node) ->
    decode_pubsub_publish_attrs(_attrs, Node);
decode_pubsub_publish_attrs([], Node) ->
    decode_pubsub_publish_attr_node(Node).

encode_pubsub_publish({pubsub_publish, Node, Items},
		      _xmlns_attrs) ->
    _els = 'encode_pubsub_publish_$items'(Items, []),
    _attrs = encode_pubsub_publish_attr_node(Node,
					     _xmlns_attrs),
    {xmlel, <<"publish">>, _attrs, _els}.

'encode_pubsub_publish_$items'([], _acc) -> _acc;
'encode_pubsub_publish_$items'([Items | _els], _acc) ->
    'encode_pubsub_publish_$items'(_els,
				   [encode_pubsub_item(Items, []) | _acc]).

decode_pubsub_publish_attr_node(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"node">>, <<"publish">>,
		   <<"http://jabber.org/protocol/pubsub">>}});
decode_pubsub_publish_attr_node(_val) -> _val.

encode_pubsub_publish_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_unsubscribe({xmlel, <<"unsubscribe">>,
			   _attrs, _els}) ->
    {Node, Subid, Jid} =
	decode_pubsub_unsubscribe_attrs(_attrs, undefined,
					undefined, undefined),
    {pubsub_unsubscribe, Node, Jid, Subid}.

decode_pubsub_unsubscribe_attrs([{<<"node">>, _val}
				 | _attrs],
				_Node, Subid, Jid) ->
    decode_pubsub_unsubscribe_attrs(_attrs, _val, Subid,
				    Jid);
decode_pubsub_unsubscribe_attrs([{<<"subid">>, _val}
				 | _attrs],
				Node, _Subid, Jid) ->
    decode_pubsub_unsubscribe_attrs(_attrs, Node, _val,
				    Jid);
decode_pubsub_unsubscribe_attrs([{<<"jid">>, _val}
				 | _attrs],
				Node, Subid, _Jid) ->
    decode_pubsub_unsubscribe_attrs(_attrs, Node, Subid,
				    _val);
decode_pubsub_unsubscribe_attrs([_ | _attrs], Node,
				Subid, Jid) ->
    decode_pubsub_unsubscribe_attrs(_attrs, Node, Subid,
				    Jid);
decode_pubsub_unsubscribe_attrs([], Node, Subid, Jid) ->
    {decode_pubsub_unsubscribe_attr_node(Node),
     decode_pubsub_unsubscribe_attr_subid(Subid),
     decode_pubsub_unsubscribe_attr_jid(Jid)}.

encode_pubsub_unsubscribe({pubsub_unsubscribe, Node,
			   Jid, Subid},
			  _xmlns_attrs) ->
    _els = [],
    _attrs = encode_pubsub_unsubscribe_attr_jid(Jid,
						encode_pubsub_unsubscribe_attr_subid(Subid,
										     encode_pubsub_unsubscribe_attr_node(Node,
															 _xmlns_attrs))),
    {xmlel, <<"unsubscribe">>, _attrs, _els}.

decode_pubsub_unsubscribe_attr_node(undefined) ->
    undefined;
decode_pubsub_unsubscribe_attr_node(_val) -> _val.

encode_pubsub_unsubscribe_attr_node(undefined, _acc) ->
    _acc;
encode_pubsub_unsubscribe_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_unsubscribe_attr_subid(undefined) ->
    undefined;
decode_pubsub_unsubscribe_attr_subid(_val) -> _val.

encode_pubsub_unsubscribe_attr_subid(undefined, _acc) ->
    _acc;
encode_pubsub_unsubscribe_attr_subid(_val, _acc) ->
    [{<<"subid">>, _val} | _acc].

decode_pubsub_unsubscribe_attr_jid(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"unsubscribe">>,
		   <<"http://jabber.org/protocol/pubsub">>}});
decode_pubsub_unsubscribe_attr_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"unsubscribe">>,
			 <<"http://jabber.org/protocol/pubsub">>}});
      _res -> _res
    end.

encode_pubsub_unsubscribe_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_pubsub_subscribe({xmlel, <<"subscribe">>, _attrs,
			 _els}) ->
    {Node, Jid} = decode_pubsub_subscribe_attrs(_attrs,
						undefined, undefined),
    {pubsub_subscribe, Node, Jid}.

decode_pubsub_subscribe_attrs([{<<"node">>, _val}
			       | _attrs],
			      _Node, Jid) ->
    decode_pubsub_subscribe_attrs(_attrs, _val, Jid);
decode_pubsub_subscribe_attrs([{<<"jid">>, _val}
			       | _attrs],
			      Node, _Jid) ->
    decode_pubsub_subscribe_attrs(_attrs, Node, _val);
decode_pubsub_subscribe_attrs([_ | _attrs], Node,
			      Jid) ->
    decode_pubsub_subscribe_attrs(_attrs, Node, Jid);
decode_pubsub_subscribe_attrs([], Node, Jid) ->
    {decode_pubsub_subscribe_attr_node(Node),
     decode_pubsub_subscribe_attr_jid(Jid)}.

encode_pubsub_subscribe({pubsub_subscribe, Node, Jid},
			_xmlns_attrs) ->
    _els = [],
    _attrs = encode_pubsub_subscribe_attr_jid(Jid,
					      encode_pubsub_subscribe_attr_node(Node,
										_xmlns_attrs)),
    {xmlel, <<"subscribe">>, _attrs, _els}.

decode_pubsub_subscribe_attr_node(undefined) ->
    undefined;
decode_pubsub_subscribe_attr_node(_val) -> _val.

encode_pubsub_subscribe_attr_node(undefined, _acc) ->
    _acc;
encode_pubsub_subscribe_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_subscribe_attr_jid(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"subscribe">>,
		   <<"http://jabber.org/protocol/pubsub">>}});
decode_pubsub_subscribe_attr_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"subscribe">>,
			 <<"http://jabber.org/protocol/pubsub">>}});
      _res -> _res
    end.

encode_pubsub_subscribe_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_pubsub_affiliations({xmlel, <<"affiliations">>,
			    _attrs, _els}) ->
    Affiliations = decode_pubsub_affiliations_els(_els, []),
    Affiliations.

decode_pubsub_affiliations_els([], Affiliations) ->
    lists:reverse(Affiliations);
decode_pubsub_affiliations_els([{xmlel,
				 <<"affiliation">>, _attrs, _} =
				    _el
				| _els],
			       Affiliations) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/pubsub">> ->
	   decode_pubsub_affiliations_els(_els,
					  [decode_pubsub_affiliation(_el)
					   | Affiliations]);
       true ->
	   decode_pubsub_affiliations_els(_els, Affiliations)
    end;
decode_pubsub_affiliations_els([_ | _els],
			       Affiliations) ->
    decode_pubsub_affiliations_els(_els, Affiliations).

encode_pubsub_affiliations(Affiliations,
			   _xmlns_attrs) ->
    _els =
	'encode_pubsub_affiliations_$affiliations'(Affiliations,
						   []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"affiliations">>, _attrs, _els}.

'encode_pubsub_affiliations_$affiliations'([], _acc) ->
    _acc;
'encode_pubsub_affiliations_$affiliations'([Affiliations
					    | _els],
					   _acc) ->
    'encode_pubsub_affiliations_$affiliations'(_els,
					       [encode_pubsub_affiliation(Affiliations,
									  [])
						| _acc]).

decode_pubsub_subscriptions({xmlel, <<"subscriptions">>,
			     _attrs, _els}) ->
    Subscriptions = decode_pubsub_subscriptions_els(_els,
						    []),
    Node = decode_pubsub_subscriptions_attrs(_attrs,
					     undefined),
    {Node, Subscriptions}.

decode_pubsub_subscriptions_els([], Subscriptions) ->
    lists:reverse(Subscriptions);
decode_pubsub_subscriptions_els([{xmlel,
				  <<"subscription">>, _attrs, _} =
				     _el
				 | _els],
				Subscriptions) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/pubsub">> ->
	   decode_pubsub_subscriptions_els(_els,
					   [decode_pubsub_subscription(_el)
					    | Subscriptions]);
       true ->
	   decode_pubsub_subscriptions_els(_els, Subscriptions)
    end;
decode_pubsub_subscriptions_els([_ | _els],
				Subscriptions) ->
    decode_pubsub_subscriptions_els(_els, Subscriptions).

decode_pubsub_subscriptions_attrs([{<<"node">>, _val}
				   | _attrs],
				  _Node) ->
    decode_pubsub_subscriptions_attrs(_attrs, _val);
decode_pubsub_subscriptions_attrs([_ | _attrs], Node) ->
    decode_pubsub_subscriptions_attrs(_attrs, Node);
decode_pubsub_subscriptions_attrs([], Node) ->
    decode_pubsub_subscriptions_attr_node(Node).

encode_pubsub_subscriptions({Node, Subscriptions},
			    _xmlns_attrs) ->
    _els =
	'encode_pubsub_subscriptions_$subscriptions'(Subscriptions,
						     []),
    _attrs = encode_pubsub_subscriptions_attr_node(Node,
						   _xmlns_attrs),
    {xmlel, <<"subscriptions">>, _attrs, _els}.

'encode_pubsub_subscriptions_$subscriptions'([],
					     _acc) ->
    _acc;
'encode_pubsub_subscriptions_$subscriptions'([Subscriptions
					      | _els],
					     _acc) ->
    'encode_pubsub_subscriptions_$subscriptions'(_els,
						 [encode_pubsub_subscription(Subscriptions,
									     [])
						  | _acc]).

decode_pubsub_subscriptions_attr_node(undefined) ->
    none;
decode_pubsub_subscriptions_attr_node(_val) -> _val.

encode_pubsub_subscriptions_attr_node(none, _acc) ->
    _acc;
encode_pubsub_subscriptions_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_event({xmlel, <<"event">>, _attrs,
		     _els}) ->
    Items = decode_pubsub_event_els(_els, []),
    {pubsub_event, Items}.

decode_pubsub_event_els([], Items) ->
    lists:reverse(Items);
decode_pubsub_event_els([{xmlel, <<"items">>, _attrs,
			  _} =
			     _el
			 | _els],
			Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns ==
	 <<"http://jabber.org/protocol/pubsub#event">> ->
	   decode_pubsub_event_els(_els,
				   [decode_pubsub_event_items(_el) | Items]);
       true -> decode_pubsub_event_els(_els, Items)
    end;
decode_pubsub_event_els([_ | _els], Items) ->
    decode_pubsub_event_els(_els, Items).

encode_pubsub_event({pubsub_event, Items},
		    _xmlns_attrs) ->
    _els = 'encode_pubsub_event_$items'(Items, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"event">>, _attrs, _els}.

'encode_pubsub_event_$items'([], _acc) -> _acc;
'encode_pubsub_event_$items'([Items | _els], _acc) ->
    'encode_pubsub_event_$items'(_els,
				 [encode_pubsub_event_items(Items, []) | _acc]).

decode_pubsub_event_items({xmlel, <<"items">>, _attrs,
			   _els}) ->
    {Items, Retract} = decode_pubsub_event_items_els(_els,
						     [], []),
    Node = decode_pubsub_event_items_attrs(_attrs,
					   undefined),
    {pubsub_event_items, Node, Retract, Items}.

decode_pubsub_event_items_els([], Items, Retract) ->
    {lists:reverse(Items), lists:reverse(Retract)};
decode_pubsub_event_items_els([{xmlel, <<"retract">>,
				_attrs, _} =
				   _el
			       | _els],
			      Items, Retract) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns ==
	 <<"http://jabber.org/protocol/pubsub#event">> ->
	   decode_pubsub_event_items_els(_els, Items,
					 [decode_pubsub_event_retract(_el)
					  | Retract]);
       true ->
	   decode_pubsub_event_items_els(_els, Items, Retract)
    end;
decode_pubsub_event_items_els([{xmlel, <<"item">>,
				_attrs, _} =
				   _el
			       | _els],
			      Items, Retract) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns ==
	 <<"http://jabber.org/protocol/pubsub#event">> ->
	   decode_pubsub_event_items_els(_els,
					 [decode_pubsub_event_item(_el)
					  | Items],
					 Retract);
       true ->
	   decode_pubsub_event_items_els(_els, Items, Retract)
    end;
decode_pubsub_event_items_els([_ | _els], Items,
			      Retract) ->
    decode_pubsub_event_items_els(_els, Items, Retract).

decode_pubsub_event_items_attrs([{<<"node">>, _val}
				 | _attrs],
				_Node) ->
    decode_pubsub_event_items_attrs(_attrs, _val);
decode_pubsub_event_items_attrs([_ | _attrs], Node) ->
    decode_pubsub_event_items_attrs(_attrs, Node);
decode_pubsub_event_items_attrs([], Node) ->
    decode_pubsub_event_items_attr_node(Node).

encode_pubsub_event_items({pubsub_event_items, Node,
			   Retract, Items},
			  _xmlns_attrs) ->
    _els = 'encode_pubsub_event_items_$retract'(Retract,
						'encode_pubsub_event_items_$items'(Items,
										   [])),
    _attrs = encode_pubsub_event_items_attr_node(Node,
						 _xmlns_attrs),
    {xmlel, <<"items">>, _attrs, _els}.

'encode_pubsub_event_items_$items'([], _acc) -> _acc;
'encode_pubsub_event_items_$items'([Items | _els],
				   _acc) ->
    'encode_pubsub_event_items_$items'(_els,
				       [encode_pubsub_event_item(Items, [])
					| _acc]).

'encode_pubsub_event_items_$retract'([], _acc) -> _acc;
'encode_pubsub_event_items_$retract'([Retract | _els],
				     _acc) ->
    'encode_pubsub_event_items_$retract'(_els,
					 [encode_pubsub_event_retract(Retract,
								      [])
					  | _acc]).

decode_pubsub_event_items_attr_node(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"node">>, <<"items">>,
		   <<"http://jabber.org/protocol/pubsub#event">>}});
decode_pubsub_event_items_attr_node(_val) -> _val.

encode_pubsub_event_items_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_event_item({xmlel, <<"item">>, _attrs,
			  _els}) ->
    {Id, Node, Publisher} =
	decode_pubsub_event_item_attrs(_attrs, undefined,
				       undefined, undefined),
    {pubsub_event_item, Id, Node, Publisher}.

decode_pubsub_event_item_attrs([{<<"id">>, _val}
				| _attrs],
			       _Id, Node, Publisher) ->
    decode_pubsub_event_item_attrs(_attrs, _val, Node,
				   Publisher);
decode_pubsub_event_item_attrs([{<<"node">>, _val}
				| _attrs],
			       Id, _Node, Publisher) ->
    decode_pubsub_event_item_attrs(_attrs, Id, _val,
				   Publisher);
decode_pubsub_event_item_attrs([{<<"publisher">>, _val}
				| _attrs],
			       Id, Node, _Publisher) ->
    decode_pubsub_event_item_attrs(_attrs, Id, Node, _val);
decode_pubsub_event_item_attrs([_ | _attrs], Id, Node,
			       Publisher) ->
    decode_pubsub_event_item_attrs(_attrs, Id, Node,
				   Publisher);
decode_pubsub_event_item_attrs([], Id, Node,
			       Publisher) ->
    {decode_pubsub_event_item_attr_id(Id),
     decode_pubsub_event_item_attr_node(Node),
     decode_pubsub_event_item_attr_publisher(Publisher)}.

encode_pubsub_event_item({pubsub_event_item, Id, Node,
			  Publisher},
			 _xmlns_attrs) ->
    _els = [],
    _attrs =
	encode_pubsub_event_item_attr_publisher(Publisher,
						encode_pubsub_event_item_attr_node(Node,
										   encode_pubsub_event_item_attr_id(Id,
														    _xmlns_attrs))),
    {xmlel, <<"item">>, _attrs, _els}.

decode_pubsub_event_item_attr_id(undefined) ->
    undefined;
decode_pubsub_event_item_attr_id(_val) -> _val.

encode_pubsub_event_item_attr_id(undefined, _acc) ->
    _acc;
encode_pubsub_event_item_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_pubsub_event_item_attr_node(undefined) ->
    undefined;
decode_pubsub_event_item_attr_node(_val) -> _val.

encode_pubsub_event_item_attr_node(undefined, _acc) ->
    _acc;
encode_pubsub_event_item_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_event_item_attr_publisher(undefined) ->
    undefined;
decode_pubsub_event_item_attr_publisher(_val) -> _val.

encode_pubsub_event_item_attr_publisher(undefined,
					_acc) ->
    _acc;
encode_pubsub_event_item_attr_publisher(_val, _acc) ->
    [{<<"publisher">>, _val} | _acc].

decode_pubsub_event_retract({xmlel, <<"retract">>,
			     _attrs, _els}) ->
    Id = decode_pubsub_event_retract_attrs(_attrs,
					   undefined),
    Id.

decode_pubsub_event_retract_attrs([{<<"id">>, _val}
				   | _attrs],
				  _Id) ->
    decode_pubsub_event_retract_attrs(_attrs, _val);
decode_pubsub_event_retract_attrs([_ | _attrs], Id) ->
    decode_pubsub_event_retract_attrs(_attrs, Id);
decode_pubsub_event_retract_attrs([], Id) ->
    decode_pubsub_event_retract_attr_id(Id).

encode_pubsub_event_retract(Id, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_pubsub_event_retract_attr_id(Id,
						 _xmlns_attrs),
    {xmlel, <<"retract">>, _attrs, _els}.

decode_pubsub_event_retract_attr_id(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"id">>, <<"retract">>,
		   <<"http://jabber.org/protocol/pubsub#event">>}});
decode_pubsub_event_retract_attr_id(_val) -> _val.

encode_pubsub_event_retract_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_pubsub_items({xmlel, <<"items">>, _attrs,
		     _els}) ->
    Items = decode_pubsub_items_els(_els, []),
    {Max_items, Node, Subid} =
	decode_pubsub_items_attrs(_attrs, undefined, undefined,
				  undefined),
    {pubsub_items, Node, Max_items, Subid, Items}.

decode_pubsub_items_els([], Items) ->
    lists:reverse(Items);
decode_pubsub_items_els([{xmlel, <<"item">>, _attrs,
			  _} =
			     _el
			 | _els],
			Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/pubsub">> ->
	   decode_pubsub_items_els(_els,
				   [decode_pubsub_item(_el) | Items]);
       true -> decode_pubsub_items_els(_els, Items)
    end;
decode_pubsub_items_els([_ | _els], Items) ->
    decode_pubsub_items_els(_els, Items).

decode_pubsub_items_attrs([{<<"max_items">>, _val}
			   | _attrs],
			  _Max_items, Node, Subid) ->
    decode_pubsub_items_attrs(_attrs, _val, Node, Subid);
decode_pubsub_items_attrs([{<<"node">>, _val} | _attrs],
			  Max_items, _Node, Subid) ->
    decode_pubsub_items_attrs(_attrs, Max_items, _val,
			      Subid);
decode_pubsub_items_attrs([{<<"subid">>, _val}
			   | _attrs],
			  Max_items, Node, _Subid) ->
    decode_pubsub_items_attrs(_attrs, Max_items, Node,
			      _val);
decode_pubsub_items_attrs([_ | _attrs], Max_items, Node,
			  Subid) ->
    decode_pubsub_items_attrs(_attrs, Max_items, Node,
			      Subid);
decode_pubsub_items_attrs([], Max_items, Node, Subid) ->
    {decode_pubsub_items_attr_max_items(Max_items),
     decode_pubsub_items_attr_node(Node),
     decode_pubsub_items_attr_subid(Subid)}.

encode_pubsub_items({pubsub_items, Node, Max_items,
		     Subid, Items},
		    _xmlns_attrs) ->
    _els = 'encode_pubsub_items_$items'(Items, []),
    _attrs = encode_pubsub_items_attr_subid(Subid,
					    encode_pubsub_items_attr_node(Node,
									  encode_pubsub_items_attr_max_items(Max_items,
													     _xmlns_attrs))),
    {xmlel, <<"items">>, _attrs, _els}.

'encode_pubsub_items_$items'([], _acc) -> _acc;
'encode_pubsub_items_$items'([Items | _els], _acc) ->
    'encode_pubsub_items_$items'(_els,
				 [encode_pubsub_item(Items, []) | _acc]).

decode_pubsub_items_attr_max_items(undefined) ->
    undefined;
decode_pubsub_items_attr_max_items(_val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"max_items">>, <<"items">>,
			 <<"http://jabber.org/protocol/pubsub">>}});
      _res -> _res
    end.

encode_pubsub_items_attr_max_items(undefined, _acc) ->
    _acc;
encode_pubsub_items_attr_max_items(_val, _acc) ->
    [{<<"max_items">>, enc_int(_val)} | _acc].

decode_pubsub_items_attr_node(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"node">>, <<"items">>,
		   <<"http://jabber.org/protocol/pubsub">>}});
decode_pubsub_items_attr_node(_val) -> _val.

encode_pubsub_items_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_items_attr_subid(undefined) -> undefined;
decode_pubsub_items_attr_subid(_val) -> _val.

encode_pubsub_items_attr_subid(undefined, _acc) -> _acc;
encode_pubsub_items_attr_subid(_val, _acc) ->
    [{<<"subid">>, _val} | _acc].

decode_pubsub_item({xmlel, <<"item">>, _attrs, _els}) ->
    __Xmls = decode_pubsub_item_els(_els, []),
    Id = decode_pubsub_item_attrs(_attrs, undefined),
    {pubsub_item, Id, __Xmls}.

decode_pubsub_item_els([], __Xmls) ->
    lists:reverse(__Xmls);
decode_pubsub_item_els([{xmlel, _, _, _} = _el | _els],
		       __Xmls) ->
    decode_pubsub_item_els(_els, [_el | __Xmls]);
decode_pubsub_item_els([_ | _els], __Xmls) ->
    decode_pubsub_item_els(_els, __Xmls).

decode_pubsub_item_attrs([{<<"id">>, _val} | _attrs],
			 _Id) ->
    decode_pubsub_item_attrs(_attrs, _val);
decode_pubsub_item_attrs([_ | _attrs], Id) ->
    decode_pubsub_item_attrs(_attrs, Id);
decode_pubsub_item_attrs([], Id) ->
    decode_pubsub_item_attr_id(Id).

encode_pubsub_item({pubsub_item, Id, __Xmls},
		   _xmlns_attrs) ->
    _els = __Xmls,
    _attrs = encode_pubsub_item_attr_id(Id, _xmlns_attrs),
    {xmlel, <<"item">>, _attrs, _els}.

decode_pubsub_item_attr_id(undefined) -> undefined;
decode_pubsub_item_attr_id(_val) -> _val.

encode_pubsub_item_attr_id(undefined, _acc) -> _acc;
encode_pubsub_item_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_pubsub_affiliation({xmlel, <<"affiliation">>,
			   _attrs, _els}) ->
    {Node, Type} = decode_pubsub_affiliation_attrs(_attrs,
						   undefined, undefined),
    {pubsub_affiliation, Node, Type}.

decode_pubsub_affiliation_attrs([{<<"node">>, _val}
				 | _attrs],
				_Node, Type) ->
    decode_pubsub_affiliation_attrs(_attrs, _val, Type);
decode_pubsub_affiliation_attrs([{<<"affiliation">>,
				  _val}
				 | _attrs],
				Node, _Type) ->
    decode_pubsub_affiliation_attrs(_attrs, Node, _val);
decode_pubsub_affiliation_attrs([_ | _attrs], Node,
				Type) ->
    decode_pubsub_affiliation_attrs(_attrs, Node, Type);
decode_pubsub_affiliation_attrs([], Node, Type) ->
    {decode_pubsub_affiliation_attr_node(Node),
     decode_pubsub_affiliation_attr_affiliation(Type)}.

encode_pubsub_affiliation({pubsub_affiliation, Node,
			   Type},
			  _xmlns_attrs) ->
    _els = [],
    _attrs =
	encode_pubsub_affiliation_attr_affiliation(Type,
						   encode_pubsub_affiliation_attr_node(Node,
										       _xmlns_attrs)),
    {xmlel, <<"affiliation">>, _attrs, _els}.

decode_pubsub_affiliation_attr_node(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"node">>, <<"affiliation">>,
		   <<"http://jabber.org/protocol/pubsub">>}});
decode_pubsub_affiliation_attr_node(_val) -> _val.

encode_pubsub_affiliation_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_affiliation_attr_affiliation(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"affiliation">>, <<"affiliation">>,
		   <<"http://jabber.org/protocol/pubsub">>}});
decode_pubsub_affiliation_attr_affiliation(_val) ->
    case catch dec_enum(_val,
			[member, none, outcast, owner, publisher,
			 'publish-only'])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"affiliation">>, <<"affiliation">>,
			 <<"http://jabber.org/protocol/pubsub">>}});
      _res -> _res
    end.

encode_pubsub_affiliation_attr_affiliation(_val,
					   _acc) ->
    [{<<"affiliation">>, enc_enum(_val)} | _acc].

decode_pubsub_subscription({xmlel, <<"subscription">>,
			    _attrs, _els}) ->
    {Jid, Node, Subid, Type} =
	decode_pubsub_subscription_attrs(_attrs, undefined,
					 undefined, undefined, undefined),
    {pubsub_subscription, Jid, Node, Subid, Type}.

decode_pubsub_subscription_attrs([{<<"jid">>, _val}
				  | _attrs],
				 _Jid, Node, Subid, Type) ->
    decode_pubsub_subscription_attrs(_attrs, _val, Node,
				     Subid, Type);
decode_pubsub_subscription_attrs([{<<"node">>, _val}
				  | _attrs],
				 Jid, _Node, Subid, Type) ->
    decode_pubsub_subscription_attrs(_attrs, Jid, _val,
				     Subid, Type);
decode_pubsub_subscription_attrs([{<<"subid">>, _val}
				  | _attrs],
				 Jid, Node, _Subid, Type) ->
    decode_pubsub_subscription_attrs(_attrs, Jid, Node,
				     _val, Type);
decode_pubsub_subscription_attrs([{<<"subscription">>,
				   _val}
				  | _attrs],
				 Jid, Node, Subid, _Type) ->
    decode_pubsub_subscription_attrs(_attrs, Jid, Node,
				     Subid, _val);
decode_pubsub_subscription_attrs([_ | _attrs], Jid,
				 Node, Subid, Type) ->
    decode_pubsub_subscription_attrs(_attrs, Jid, Node,
				     Subid, Type);
decode_pubsub_subscription_attrs([], Jid, Node, Subid,
				 Type) ->
    {decode_pubsub_subscription_attr_jid(Jid),
     decode_pubsub_subscription_attr_node(Node),
     decode_pubsub_subscription_attr_subid(Subid),
     decode_pubsub_subscription_attr_subscription(Type)}.

encode_pubsub_subscription({pubsub_subscription, Jid,
			    Node, Subid, Type},
			   _xmlns_attrs) ->
    _els = [],
    _attrs =
	encode_pubsub_subscription_attr_subscription(Type,
						     encode_pubsub_subscription_attr_subid(Subid,
											   encode_pubsub_subscription_attr_node(Node,
																encode_pubsub_subscription_attr_jid(Jid,
																				    _xmlns_attrs)))),
    {xmlel, <<"subscription">>, _attrs, _els}.

decode_pubsub_subscription_attr_jid(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"subscription">>,
		   <<"http://jabber.org/protocol/pubsub">>}});
decode_pubsub_subscription_attr_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"subscription">>,
			 <<"http://jabber.org/protocol/pubsub">>}});
      _res -> _res
    end.

encode_pubsub_subscription_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_pubsub_subscription_attr_node(undefined) ->
    undefined;
decode_pubsub_subscription_attr_node(_val) -> _val.

encode_pubsub_subscription_attr_node(undefined, _acc) ->
    _acc;
encode_pubsub_subscription_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_subscription_attr_subid(undefined) ->
    undefined;
decode_pubsub_subscription_attr_subid(_val) -> _val.

encode_pubsub_subscription_attr_subid(undefined,
				      _acc) ->
    _acc;
encode_pubsub_subscription_attr_subid(_val, _acc) ->
    [{<<"subid">>, _val} | _acc].

decode_pubsub_subscription_attr_subscription(undefined) ->
    undefined;
decode_pubsub_subscription_attr_subscription(_val) ->
    case catch dec_enum(_val,
			[none, pending, subscribed, unconfigured])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"subscription">>, <<"subscription">>,
			 <<"http://jabber.org/protocol/pubsub">>}});
      _res -> _res
    end.

encode_pubsub_subscription_attr_subscription(undefined,
					     _acc) ->
    _acc;
encode_pubsub_subscription_attr_subscription(_val,
					     _acc) ->
    [{<<"subscription">>, enc_enum(_val)} | _acc].

decode_xdata({xmlel, <<"x">>, _attrs, _els}) ->
    {Fields, Items, Instructions, Reported, Title} =
	decode_xdata_els(_els, [], [], [], undefined,
			 undefined),
    Type = decode_xdata_attrs(_attrs, undefined),
    {xdata, Type, Instructions, Title, Reported, Items,
     Fields}.

decode_xdata_els([], Fields, Items, Instructions,
		 Reported, Title) ->
    {lists:reverse(Fields), lists:reverse(Items),
     lists:reverse(Instructions), Reported, Title};
decode_xdata_els([{xmlel, <<"instructions">>, _attrs,
		   _} =
		      _el
		  | _els],
		 Fields, Items, Instructions, Reported, Title) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:x:data">> ->
	   decode_xdata_els(_els, Fields, Items,
			    case decode_xdata_instructions(_el) of
			      undefined -> Instructions;
			      _new_el -> [_new_el | Instructions]
			    end,
			    Reported, Title);
       true ->
	   decode_xdata_els(_els, Fields, Items, Instructions,
			    Reported, Title)
    end;
decode_xdata_els([{xmlel, <<"title">>, _attrs, _} = _el
		  | _els],
		 Fields, Items, Instructions, Reported, Title) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:x:data">> ->
	   decode_xdata_els(_els, Fields, Items, Instructions,
			    Reported, decode_xdata_title(_el));
       true ->
	   decode_xdata_els(_els, Fields, Items, Instructions,
			    Reported, Title)
    end;
decode_xdata_els([{xmlel, <<"reported">>, _attrs, _} =
		      _el
		  | _els],
		 Fields, Items, Instructions, Reported, Title) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:x:data">> ->
	   decode_xdata_els(_els, Fields, Items, Instructions,
			    decode_xdata_reported(_el), Title);
       true ->
	   decode_xdata_els(_els, Fields, Items, Instructions,
			    Reported, Title)
    end;
decode_xdata_els([{xmlel, <<"item">>, _attrs, _} = _el
		  | _els],
		 Fields, Items, Instructions, Reported, Title) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:x:data">> ->
	   decode_xdata_els(_els, Fields,
			    [decode_xdata_item(_el) | Items], Instructions,
			    Reported, Title);
       true ->
	   decode_xdata_els(_els, Fields, Items, Instructions,
			    Reported, Title)
    end;
decode_xdata_els([{xmlel, <<"field">>, _attrs, _} = _el
		  | _els],
		 Fields, Items, Instructions, Reported, Title) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:x:data">> ->
	   decode_xdata_els(_els,
			    [decode_xdata_field(_el) | Fields], Items,
			    Instructions, Reported, Title);
       true ->
	   decode_xdata_els(_els, Fields, Items, Instructions,
			    Reported, Title)
    end;
decode_xdata_els([_ | _els], Fields, Items,
		 Instructions, Reported, Title) ->
    decode_xdata_els(_els, Fields, Items, Instructions,
		     Reported, Title).

decode_xdata_attrs([{<<"type">>, _val} | _attrs],
		   _Type) ->
    decode_xdata_attrs(_attrs, _val);
decode_xdata_attrs([_ | _attrs], Type) ->
    decode_xdata_attrs(_attrs, Type);
decode_xdata_attrs([], Type) ->
    decode_xdata_attr_type(Type).

encode_xdata({xdata, Type, Instructions, Title,
	      Reported, Items, Fields},
	     _xmlns_attrs) ->
    _els = 'encode_xdata_$title'(Title,
				 'encode_xdata_$reported'(Reported,
							  'encode_xdata_$instructions'(Instructions,
										       'encode_xdata_$items'(Items,
													     'encode_xdata_$fields'(Fields,
																    []))))),
    _attrs = encode_xdata_attr_type(Type, _xmlns_attrs),
    {xmlel, <<"x">>, _attrs, _els}.

'encode_xdata_$fields'([], _acc) -> _acc;
'encode_xdata_$fields'([Fields | _els], _acc) ->
    'encode_xdata_$fields'(_els,
			   [encode_xdata_field(Fields, []) | _acc]).

'encode_xdata_$items'([], _acc) -> _acc;
'encode_xdata_$items'([Items | _els], _acc) ->
    'encode_xdata_$items'(_els,
			  [encode_xdata_item(Items, []) | _acc]).

'encode_xdata_$instructions'([], _acc) -> _acc;
'encode_xdata_$instructions'([Instructions | _els],
			     _acc) ->
    'encode_xdata_$instructions'(_els,
				 [encode_xdata_instructions(Instructions, [])
				  | _acc]).

'encode_xdata_$reported'(undefined, _acc) -> _acc;
'encode_xdata_$reported'(Reported, _acc) ->
    [encode_xdata_reported(Reported, []) | _acc].

'encode_xdata_$title'(undefined, _acc) -> _acc;
'encode_xdata_$title'(Title, _acc) ->
    [encode_xdata_title(Title, []) | _acc].

decode_xdata_attr_type(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"type">>, <<"x">>,
		   <<"jabber:x:data">>}});
decode_xdata_attr_type(_val) ->
    case catch dec_enum(_val,
			[cancel, form, result, submit])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"type">>, <<"x">>,
			 <<"jabber:x:data">>}});
      _res -> _res
    end.

encode_xdata_attr_type(_val, _acc) ->
    [{<<"type">>, enc_enum(_val)} | _acc].

decode_xdata_item({xmlel, <<"item">>, _attrs, _els}) ->
    Fields = decode_xdata_item_els(_els, []), Fields.

decode_xdata_item_els([], Fields) ->
    lists:reverse(Fields);
decode_xdata_item_els([{xmlel, <<"field">>, _attrs, _} =
			   _el
		       | _els],
		      Fields) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:x:data">> ->
	   decode_xdata_item_els(_els,
				 [decode_xdata_field(_el) | Fields]);
       true -> decode_xdata_item_els(_els, Fields)
    end;
decode_xdata_item_els([_ | _els], Fields) ->
    decode_xdata_item_els(_els, Fields).

encode_xdata_item(Fields, _xmlns_attrs) ->
    _els = 'encode_xdata_item_$fields'(Fields, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"item">>, _attrs, _els}.

'encode_xdata_item_$fields'([], _acc) -> _acc;
'encode_xdata_item_$fields'([Fields | _els], _acc) ->
    'encode_xdata_item_$fields'(_els,
				[encode_xdata_field(Fields, []) | _acc]).

decode_xdata_reported({xmlel, <<"reported">>, _attrs,
		       _els}) ->
    Fields = decode_xdata_reported_els(_els, []), Fields.

decode_xdata_reported_els([], Fields) ->
    lists:reverse(Fields);
decode_xdata_reported_els([{xmlel, <<"field">>, _attrs,
			    _} =
			       _el
			   | _els],
			  Fields) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:x:data">> ->
	   decode_xdata_reported_els(_els,
				     [decode_xdata_field(_el) | Fields]);
       true -> decode_xdata_reported_els(_els, Fields)
    end;
decode_xdata_reported_els([_ | _els], Fields) ->
    decode_xdata_reported_els(_els, Fields).

encode_xdata_reported(Fields, _xmlns_attrs) ->
    _els = 'encode_xdata_reported_$fields'(Fields, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"reported">>, _attrs, _els}.

'encode_xdata_reported_$fields'([], _acc) -> _acc;
'encode_xdata_reported_$fields'([Fields | _els],
				_acc) ->
    'encode_xdata_reported_$fields'(_els,
				    [encode_xdata_field(Fields, []) | _acc]).

decode_xdata_title({xmlel, <<"title">>, _attrs,
		    _els}) ->
    Cdata = decode_xdata_title_els(_els, <<>>), Cdata.

decode_xdata_title_els([], Cdata) ->
    decode_xdata_title_cdata(Cdata);
decode_xdata_title_els([{xmlcdata, _data} | _els],
		       Cdata) ->
    decode_xdata_title_els(_els,
			   <<Cdata/binary, _data/binary>>);
decode_xdata_title_els([_ | _els], Cdata) ->
    decode_xdata_title_els(_els, Cdata).

encode_xdata_title(Cdata, _xmlns_attrs) ->
    _els = encode_xdata_title_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"title">>, _attrs, _els}.

decode_xdata_title_cdata(<<>>) -> undefined;
decode_xdata_title_cdata(_val) -> _val.

encode_xdata_title_cdata(undefined, _acc) -> _acc;
encode_xdata_title_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_xdata_instructions({xmlel, <<"instructions">>,
			   _attrs, _els}) ->
    Cdata = decode_xdata_instructions_els(_els, <<>>),
    Cdata.

decode_xdata_instructions_els([], Cdata) ->
    decode_xdata_instructions_cdata(Cdata);
decode_xdata_instructions_els([{xmlcdata, _data}
			       | _els],
			      Cdata) ->
    decode_xdata_instructions_els(_els,
				  <<Cdata/binary, _data/binary>>);
decode_xdata_instructions_els([_ | _els], Cdata) ->
    decode_xdata_instructions_els(_els, Cdata).

encode_xdata_instructions(Cdata, _xmlns_attrs) ->
    _els = encode_xdata_instructions_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"instructions">>, _attrs, _els}.

decode_xdata_instructions_cdata(<<>>) -> undefined;
decode_xdata_instructions_cdata(_val) -> _val.

encode_xdata_instructions_cdata(undefined, _acc) ->
    _acc;
encode_xdata_instructions_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_xdata_field({xmlel, <<"field">>, _attrs,
		    _els}) ->
    {Options, Values, Desc, Required} =
	decode_xdata_field_els(_els, [], [], undefined, false),
    {Label, Type, Var} = decode_xdata_field_attrs(_attrs,
						  undefined, undefined,
						  undefined),
    {xdata_field, Label, Type, Var, Required, Desc, Values,
     Options}.

decode_xdata_field_els([], Options, Values, Desc,
		       Required) ->
    {lists:reverse(Options), lists:reverse(Values), Desc,
     Required};
decode_xdata_field_els([{xmlel, <<"required">>, _attrs,
			 _} =
			    _el
			| _els],
		       Options, Values, Desc, Required) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:x:data">> ->
	   decode_xdata_field_els(_els, Options, Values, Desc,
				  decode_xdata_field_required(_el));
       true ->
	   decode_xdata_field_els(_els, Options, Values, Desc,
				  Required)
    end;
decode_xdata_field_els([{xmlel, <<"desc">>, _attrs, _} =
			    _el
			| _els],
		       Options, Values, Desc, Required) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:x:data">> ->
	   decode_xdata_field_els(_els, Options, Values,
				  decode_xdata_field_desc(_el), Required);
       true ->
	   decode_xdata_field_els(_els, Options, Values, Desc,
				  Required)
    end;
decode_xdata_field_els([{xmlel, <<"value">>, _attrs,
			 _} =
			    _el
			| _els],
		       Options, Values, Desc, Required) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:x:data">> ->
	   decode_xdata_field_els(_els, Options,
				  case decode_xdata_field_value(_el) of
				    undefined -> Values;
				    _new_el -> [_new_el | Values]
				  end,
				  Desc, Required);
       true ->
	   decode_xdata_field_els(_els, Options, Values, Desc,
				  Required)
    end;
decode_xdata_field_els([{xmlel, <<"option">>, _attrs,
			 _} =
			    _el
			| _els],
		       Options, Values, Desc, Required) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:x:data">> ->
	   decode_xdata_field_els(_els,
				  case decode_xdata_field_option(_el) of
				    undefined -> Options;
				    _new_el -> [_new_el | Options]
				  end,
				  Values, Desc, Required);
       true ->
	   decode_xdata_field_els(_els, Options, Values, Desc,
				  Required)
    end;
decode_xdata_field_els([_ | _els], Options, Values,
		       Desc, Required) ->
    decode_xdata_field_els(_els, Options, Values, Desc,
			   Required).

decode_xdata_field_attrs([{<<"label">>, _val} | _attrs],
			 _Label, Type, Var) ->
    decode_xdata_field_attrs(_attrs, _val, Type, Var);
decode_xdata_field_attrs([{<<"type">>, _val} | _attrs],
			 Label, _Type, Var) ->
    decode_xdata_field_attrs(_attrs, Label, _val, Var);
decode_xdata_field_attrs([{<<"var">>, _val} | _attrs],
			 Label, Type, _Var) ->
    decode_xdata_field_attrs(_attrs, Label, Type, _val);
decode_xdata_field_attrs([_ | _attrs], Label, Type,
			 Var) ->
    decode_xdata_field_attrs(_attrs, Label, Type, Var);
decode_xdata_field_attrs([], Label, Type, Var) ->
    {decode_xdata_field_attr_label(Label),
     decode_xdata_field_attr_type(Type),
     decode_xdata_field_attr_var(Var)}.

encode_xdata_field({xdata_field, Label, Type, Var,
		    Required, Desc, Values, Options},
		   _xmlns_attrs) ->
    _els = 'encode_xdata_field_$required'(Required,
					  'encode_xdata_field_$desc'(Desc,
								     'encode_xdata_field_$values'(Values,
												  'encode_xdata_field_$options'(Options,
																[])))),
    _attrs = encode_xdata_field_attr_var(Var,
					 encode_xdata_field_attr_type(Type,
								      encode_xdata_field_attr_label(Label,
												    _xmlns_attrs))),
    {xmlel, <<"field">>, _attrs, _els}.

'encode_xdata_field_$options'([], _acc) -> _acc;
'encode_xdata_field_$options'([Options | _els], _acc) ->
    'encode_xdata_field_$options'(_els,
				  [encode_xdata_field_option(Options, [])
				   | _acc]).

'encode_xdata_field_$values'([], _acc) -> _acc;
'encode_xdata_field_$values'([Values | _els], _acc) ->
    'encode_xdata_field_$values'(_els,
				 [encode_xdata_field_value(Values, []) | _acc]).

'encode_xdata_field_$desc'(undefined, _acc) -> _acc;
'encode_xdata_field_$desc'(Desc, _acc) ->
    [encode_xdata_field_desc(Desc, []) | _acc].

'encode_xdata_field_$required'(false, _acc) -> _acc;
'encode_xdata_field_$required'(Required, _acc) ->
    [encode_xdata_field_required(Required, []) | _acc].

decode_xdata_field_attr_label(undefined) -> undefined;
decode_xdata_field_attr_label(_val) -> _val.

encode_xdata_field_attr_label(undefined, _acc) -> _acc;
encode_xdata_field_attr_label(_val, _acc) ->
    [{<<"label">>, _val} | _acc].

decode_xdata_field_attr_type(undefined) -> undefined;
decode_xdata_field_attr_type(_val) ->
    case catch dec_enum(_val,
			[boolean, fixed, hidden, 'jid-multi', 'jid-single',
			 'list-multi', 'list-single', 'text-multi',
			 'text-private', 'text-single'])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"type">>, <<"field">>,
			 <<"jabber:x:data">>}});
      _res -> _res
    end.

encode_xdata_field_attr_type(undefined, _acc) -> _acc;
encode_xdata_field_attr_type(_val, _acc) ->
    [{<<"type">>, enc_enum(_val)} | _acc].

decode_xdata_field_attr_var(undefined) -> undefined;
decode_xdata_field_attr_var(_val) -> _val.

encode_xdata_field_attr_var(undefined, _acc) -> _acc;
encode_xdata_field_attr_var(_val, _acc) ->
    [{<<"var">>, _val} | _acc].

decode_xdata_field_option({xmlel, <<"option">>, _attrs,
			   _els}) ->
    Value = decode_xdata_field_option_els(_els, error),
    Value.

decode_xdata_field_option_els([], Value) ->
    case Value of
      error ->
	  erlang:error({xmpp_codec,
			{missing_tag, <<"value">>, <<"jabber:x:data">>}});
      {value, Value1} -> Value1
    end;
decode_xdata_field_option_els([{xmlel, <<"value">>,
				_attrs, _} =
				   _el
			       | _els],
			      Value) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:x:data">> ->
	   decode_xdata_field_option_els(_els,
					 {value,
					  decode_xdata_field_value(_el)});
       true -> decode_xdata_field_option_els(_els, Value)
    end;
decode_xdata_field_option_els([_ | _els], Value) ->
    decode_xdata_field_option_els(_els, Value).

encode_xdata_field_option(Value, _xmlns_attrs) ->
    _els = 'encode_xdata_field_option_$value'(Value, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"option">>, _attrs, _els}.

'encode_xdata_field_option_$value'(Value, _acc) ->
    [encode_xdata_field_value(Value, []) | _acc].

decode_xdata_field_value({xmlel, <<"value">>, _attrs,
			  _els}) ->
    Cdata = decode_xdata_field_value_els(_els, <<>>), Cdata.

decode_xdata_field_value_els([], Cdata) ->
    decode_xdata_field_value_cdata(Cdata);
decode_xdata_field_value_els([{xmlcdata, _data} | _els],
			     Cdata) ->
    decode_xdata_field_value_els(_els,
				 <<Cdata/binary, _data/binary>>);
decode_xdata_field_value_els([_ | _els], Cdata) ->
    decode_xdata_field_value_els(_els, Cdata).

encode_xdata_field_value(Cdata, _xmlns_attrs) ->
    _els = encode_xdata_field_value_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"value">>, _attrs, _els}.

decode_xdata_field_value_cdata(<<>>) -> undefined;
decode_xdata_field_value_cdata(_val) -> _val.

encode_xdata_field_value_cdata(undefined, _acc) -> _acc;
encode_xdata_field_value_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_xdata_field_desc({xmlel, <<"desc">>, _attrs,
			 _els}) ->
    Cdata = decode_xdata_field_desc_els(_els, <<>>), Cdata.

decode_xdata_field_desc_els([], Cdata) ->
    decode_xdata_field_desc_cdata(Cdata);
decode_xdata_field_desc_els([{xmlcdata, _data} | _els],
			    Cdata) ->
    decode_xdata_field_desc_els(_els,
				<<Cdata/binary, _data/binary>>);
decode_xdata_field_desc_els([_ | _els], Cdata) ->
    decode_xdata_field_desc_els(_els, Cdata).

encode_xdata_field_desc(Cdata, _xmlns_attrs) ->
    _els = encode_xdata_field_desc_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"desc">>, _attrs, _els}.

decode_xdata_field_desc_cdata(<<>>) -> undefined;
decode_xdata_field_desc_cdata(_val) -> _val.

encode_xdata_field_desc_cdata(undefined, _acc) -> _acc;
encode_xdata_field_desc_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_xdata_field_required({xmlel, <<"required">>,
			     _attrs, _els}) ->
    true.

encode_xdata_field_required(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"required">>, _attrs, _els}.

decode_vcard({xmlel, <<"vCard">>, _attrs, _els}) ->
    {Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
     Jabberid, Sound, Note, Role, Title, Nickname, Rev,
     Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
     Fn, Version, N, Photo, Logo, Geo} =
	decode_vcard_els(_els, undefined, [], undefined, [],
			 undefined, undefined, undefined, undefined, undefined,
			 undefined, undefined, undefined, undefined, undefined,
			 undefined, undefined, undefined, undefined, undefined,
			 undefined, [], [], [], undefined, undefined, undefined,
			 undefined, undefined, undefined),
    {vcard, Version, Fn, N, Nickname, Photo, Bday, Adr,
     Label, Tel, Email, Jabberid, Mailer, Tz, Geo, Title,
     Role, Logo, Org, Categories, Note, Prodid, Rev,
     Sort_string, Sound, Uid, Url, Class, Key, Desc}.

decode_vcard_els([], Mailer, Adr, Class, Categories,
		 Desc, Uid, Prodid, Jabberid, Sound, Note, Role, Title,
		 Nickname, Rev, Sort_string, Org, Bday, Key, Tz, Url,
		 Email, Tel, Label, Fn, Version, N, Photo, Logo, Geo) ->
    {Mailer, lists:reverse(Adr), Class, Categories, Desc,
     Uid, Prodid, Jabberid, Sound, Note, Role, Title,
     Nickname, Rev, Sort_string, Org, Bday, Key, Tz, Url,
     lists:reverse(Email), lists:reverse(Tel),
     lists:reverse(Label), Fn, Version, N, Photo, Logo, Geo};
decode_vcard_els([{xmlel, <<"N">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version,
			    decode_vcard_N(_el), Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"ADR">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer,
			    [decode_vcard_ADR(_el) | Adr], Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"LABEL">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel,
			    [decode_vcard_LABEL(_el) | Label], Fn, Version, N,
			    Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"TEL">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, [decode_vcard_TEL(_el) | Tel],
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"EMAIL">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, [decode_vcard_EMAIL(_el) | Email], Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"GEO">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, decode_vcard_GEO(_el));
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"LOGO">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    decode_vcard_LOGO(_el), Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"PHOTO">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N,
			    decode_vcard_PHOTO(_el), Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"ORG">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string,
			    decode_vcard_ORG(_el), Bday, Key, Tz, Url, Email,
			    Tel, Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"SOUND">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid,
			    decode_vcard_SOUND(_el), Note, Role, Title,
			    Nickname, Rev, Sort_string, Org, Bday, Key, Tz, Url,
			    Email, Tel, Label, Fn, Version, N, Photo, Logo,
			    Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"KEY">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday,
			    decode_vcard_KEY(_el), Tz, Url, Email, Tel, Label,
			    Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"VERSION">>, _attrs, _} =
		      _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn,
			    decode_vcard_VERSION(_el), N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"FN">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, decode_vcard_FN(_el),
			    Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"NICKNAME">>, _attrs, _} =
		      _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, decode_vcard_NICKNAME(_el), Rev, Sort_string,
			    Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn,
			    Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"BDAY">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org,
			    decode_vcard_BDAY(_el), Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"JABBERID">>, _attrs, _} =
		      _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, decode_vcard_JABBERID(_el),
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"MAILER">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, decode_vcard_MAILER(_el), Adr,
			    Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"TZ">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    decode_vcard_TZ(_el), Url, Email, Tel, Label, Fn,
			    Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"TITLE">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    decode_vcard_TITLE(_el), Nickname, Rev, Sort_string,
			    Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn,
			    Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"ROLE">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note,
			    decode_vcard_ROLE(_el), Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"NOTE">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound,
			    decode_vcard_NOTE(_el), Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"PRODID">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, decode_vcard_PRODID(_el), Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"REV">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, decode_vcard_REV(_el), Sort_string,
			    Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn,
			    Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"SORT-STRING">>, _attrs,
		   _} =
		      _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, decode_vcard_SORT_STRING(_el),
			    Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn,
			    Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"UID">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, decode_vcard_UID(_el), Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"URL">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, decode_vcard_URL(_el), Email, Tel, Label, Fn,
			    Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"DESC">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    decode_vcard_DESC(_el), Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"CATEGORIES">>, _attrs, _} =
		      _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr, Class,
			    decode_vcard_CATEGORIES(_el), Desc, Uid, Prodid,
			    Jabberid, Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([{xmlel, <<"CLASS">>, _attrs, _} = _el
		  | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_els(_els, Mailer, Adr,
			    decode_vcard_CLASS(_el), Categories, Desc, Uid,
			    Prodid, Jabberid, Sound, Note, Role, Title,
			    Nickname, Rev, Sort_string, Org, Bday, Key, Tz, Url,
			    Email, Tel, Label, Fn, Version, N, Photo, Logo,
			    Geo);
       true ->
	   decode_vcard_els(_els, Mailer, Adr, Class, Categories,
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo)
    end;
decode_vcard_els([_ | _els], Mailer, Adr, Class,
		 Categories, Desc, Uid, Prodid, Jabberid, Sound, Note,
		 Role, Title, Nickname, Rev, Sort_string, Org, Bday, Key,
		 Tz, Url, Email, Tel, Label, Fn, Version, N, Photo, Logo,
		 Geo) ->
    decode_vcard_els(_els, Mailer, Adr, Class, Categories,
		     Desc, Uid, Prodid, Jabberid, Sound, Note, Role, Title,
		     Nickname, Rev, Sort_string, Org, Bday, Key, Tz, Url,
		     Email, Tel, Label, Fn, Version, N, Photo, Logo, Geo).

encode_vcard({vcard, Version, Fn, N, Nickname, Photo,
	      Bday, Adr, Label, Tel, Email, Jabberid, Mailer, Tz, Geo,
	      Title, Role, Logo, Org, Categories, Note, Prodid, Rev,
	      Sort_string, Sound, Uid, Url, Class, Key, Desc},
	     _xmlns_attrs) ->
    _els = 'encode_vcard_$geo'(Geo,
			       'encode_vcard_$logo'(Logo,
						    'encode_vcard_$photo'(Photo,
									  'encode_vcard_$n'(N,
											    'encode_vcard_$version'(Version,
														    'encode_vcard_$fn'(Fn,
																       'encode_vcard_$label'(Label,
																			     'encode_vcard_$tel'(Tel,
																						 'encode_vcard_$email'(Email,
																								       'encode_vcard_$url'(Url,
																											   'encode_vcard_$tz'(Tz,
																													      'encode_vcard_$key'(Key,
																																  'encode_vcard_$bday'(Bday,
																																		       'encode_vcard_$org'(Org,
																																					   'encode_vcard_$sort_string'(Sort_string,
																																								       'encode_vcard_$rev'(Rev,
																																											   'encode_vcard_$nickname'(Nickname,
																																														    'encode_vcard_$title'(Title,
																																																	  'encode_vcard_$role'(Role,
																																																			       'encode_vcard_$note'(Note,
																																																						    'encode_vcard_$sound'(Sound,
																																																									  'encode_vcard_$jabberid'(Jabberid,
																																																												   'encode_vcard_$prodid'(Prodid,
																																																															  'encode_vcard_$uid'(Uid,
																																																																	      'encode_vcard_$desc'(Desc,
																																																																				   'encode_vcard_$categories'(Categories,
																																																																							      'encode_vcard_$class'(Class,
																																																																										    'encode_vcard_$adr'(Adr,
																																																																													'encode_vcard_$mailer'(Mailer,
																																																																															       []))))))))))))))))))))))))))))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"vCard">>, _attrs, _els}.

'encode_vcard_$mailer'(undefined, _acc) -> _acc;
'encode_vcard_$mailer'(Mailer, _acc) ->
    [encode_vcard_MAILER(Mailer, []) | _acc].

'encode_vcard_$adr'([], _acc) -> _acc;
'encode_vcard_$adr'([Adr | _els], _acc) ->
    'encode_vcard_$adr'(_els,
			[encode_vcard_ADR(Adr, []) | _acc]).

'encode_vcard_$class'(undefined, _acc) -> _acc;
'encode_vcard_$class'(Class, _acc) ->
    [encode_vcard_CLASS(Class, []) | _acc].

'encode_vcard_$categories'([], _acc) -> _acc;
'encode_vcard_$categories'(Categories, _acc) ->
    [encode_vcard_CATEGORIES(Categories, []) | _acc].

'encode_vcard_$desc'(undefined, _acc) -> _acc;
'encode_vcard_$desc'(Desc, _acc) ->
    [encode_vcard_DESC(Desc, []) | _acc].

'encode_vcard_$uid'(undefined, _acc) -> _acc;
'encode_vcard_$uid'(Uid, _acc) ->
    [encode_vcard_UID(Uid, []) | _acc].

'encode_vcard_$prodid'(undefined, _acc) -> _acc;
'encode_vcard_$prodid'(Prodid, _acc) ->
    [encode_vcard_PRODID(Prodid, []) | _acc].

'encode_vcard_$jabberid'(undefined, _acc) -> _acc;
'encode_vcard_$jabberid'(Jabberid, _acc) ->
    [encode_vcard_JABBERID(Jabberid, []) | _acc].

'encode_vcard_$sound'(undefined, _acc) -> _acc;
'encode_vcard_$sound'(Sound, _acc) ->
    [encode_vcard_SOUND(Sound, []) | _acc].

'encode_vcard_$note'(undefined, _acc) -> _acc;
'encode_vcard_$note'(Note, _acc) ->
    [encode_vcard_NOTE(Note, []) | _acc].

'encode_vcard_$role'(undefined, _acc) -> _acc;
'encode_vcard_$role'(Role, _acc) ->
    [encode_vcard_ROLE(Role, []) | _acc].

'encode_vcard_$title'(undefined, _acc) -> _acc;
'encode_vcard_$title'(Title, _acc) ->
    [encode_vcard_TITLE(Title, []) | _acc].

'encode_vcard_$nickname'(undefined, _acc) -> _acc;
'encode_vcard_$nickname'(Nickname, _acc) ->
    [encode_vcard_NICKNAME(Nickname, []) | _acc].

'encode_vcard_$rev'(undefined, _acc) -> _acc;
'encode_vcard_$rev'(Rev, _acc) ->
    [encode_vcard_REV(Rev, []) | _acc].

'encode_vcard_$sort_string'(undefined, _acc) -> _acc;
'encode_vcard_$sort_string'(Sort_string, _acc) ->
    [encode_vcard_SORT_STRING(Sort_string, []) | _acc].

'encode_vcard_$org'(undefined, _acc) -> _acc;
'encode_vcard_$org'(Org, _acc) ->
    [encode_vcard_ORG(Org, []) | _acc].

'encode_vcard_$bday'(undefined, _acc) -> _acc;
'encode_vcard_$bday'(Bday, _acc) ->
    [encode_vcard_BDAY(Bday, []) | _acc].

'encode_vcard_$key'(undefined, _acc) -> _acc;
'encode_vcard_$key'(Key, _acc) ->
    [encode_vcard_KEY(Key, []) | _acc].

'encode_vcard_$tz'(undefined, _acc) -> _acc;
'encode_vcard_$tz'(Tz, _acc) ->
    [encode_vcard_TZ(Tz, []) | _acc].

'encode_vcard_$url'(undefined, _acc) -> _acc;
'encode_vcard_$url'(Url, _acc) ->
    [encode_vcard_URL(Url, []) | _acc].

'encode_vcard_$email'([], _acc) -> _acc;
'encode_vcard_$email'([Email | _els], _acc) ->
    'encode_vcard_$email'(_els,
			  [encode_vcard_EMAIL(Email, []) | _acc]).

'encode_vcard_$tel'([], _acc) -> _acc;
'encode_vcard_$tel'([Tel | _els], _acc) ->
    'encode_vcard_$tel'(_els,
			[encode_vcard_TEL(Tel, []) | _acc]).

'encode_vcard_$label'([], _acc) -> _acc;
'encode_vcard_$label'([Label | _els], _acc) ->
    'encode_vcard_$label'(_els,
			  [encode_vcard_LABEL(Label, []) | _acc]).

'encode_vcard_$fn'(undefined, _acc) -> _acc;
'encode_vcard_$fn'(Fn, _acc) ->
    [encode_vcard_FN(Fn, []) | _acc].

'encode_vcard_$version'(undefined, _acc) -> _acc;
'encode_vcard_$version'(Version, _acc) ->
    [encode_vcard_VERSION(Version, []) | _acc].

'encode_vcard_$n'(undefined, _acc) -> _acc;
'encode_vcard_$n'(N, _acc) ->
    [encode_vcard_N(N, []) | _acc].

'encode_vcard_$photo'(undefined, _acc) -> _acc;
'encode_vcard_$photo'(Photo, _acc) ->
    [encode_vcard_PHOTO(Photo, []) | _acc].

'encode_vcard_$logo'(undefined, _acc) -> _acc;
'encode_vcard_$logo'(Logo, _acc) ->
    [encode_vcard_LOGO(Logo, []) | _acc].

'encode_vcard_$geo'(undefined, _acc) -> _acc;
'encode_vcard_$geo'(Geo, _acc) ->
    [encode_vcard_GEO(Geo, []) | _acc].

decode_vcard_CLASS({xmlel, <<"CLASS">>, _attrs,
		    _els}) ->
    Class = decode_vcard_CLASS_els(_els, undefined), Class.

decode_vcard_CLASS_els([], Class) -> Class;
decode_vcard_CLASS_els([{xmlel, <<"PUBLIC">>, _attrs,
			 _} =
			    _el
			| _els],
		       Class) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_CLASS_els(_els, decode_vcard_PUBLIC(_el));
       true -> decode_vcard_CLASS_els(_els, Class)
    end;
decode_vcard_CLASS_els([{xmlel, <<"PRIVATE">>, _attrs,
			 _} =
			    _el
			| _els],
		       Class) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_CLASS_els(_els, decode_vcard_PRIVATE(_el));
       true -> decode_vcard_CLASS_els(_els, Class)
    end;
decode_vcard_CLASS_els([{xmlel, <<"CONFIDENTIAL">>,
			 _attrs, _} =
			    _el
			| _els],
		       Class) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_CLASS_els(_els,
				  decode_vcard_CONFIDENTIAL(_el));
       true -> decode_vcard_CLASS_els(_els, Class)
    end;
decode_vcard_CLASS_els([_ | _els], Class) ->
    decode_vcard_CLASS_els(_els, Class).

encode_vcard_CLASS(Class, _xmlns_attrs) ->
    _els = 'encode_vcard_CLASS_$class'(Class, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"CLASS">>, _attrs, _els}.

'encode_vcard_CLASS_$class'(undefined, _acc) -> _acc;
'encode_vcard_CLASS_$class'(public = Class, _acc) ->
    [encode_vcard_PUBLIC(Class, []) | _acc];
'encode_vcard_CLASS_$class'(private = Class, _acc) ->
    [encode_vcard_PRIVATE(Class, []) | _acc];
'encode_vcard_CLASS_$class'(confidential = Class,
			    _acc) ->
    [encode_vcard_CONFIDENTIAL(Class, []) | _acc].

decode_vcard_CATEGORIES({xmlel, <<"CATEGORIES">>,
			 _attrs, _els}) ->
    Keywords = decode_vcard_CATEGORIES_els(_els, []),
    Keywords.

decode_vcard_CATEGORIES_els([], Keywords) ->
    lists:reverse(Keywords);
decode_vcard_CATEGORIES_els([{xmlel, <<"KEYWORD">>,
			      _attrs, _} =
				 _el
			     | _els],
			    Keywords) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_CATEGORIES_els(_els,
				       case decode_vcard_KEYWORD(_el) of
					 undefined -> Keywords;
					 _new_el -> [_new_el | Keywords]
				       end);
       true -> decode_vcard_CATEGORIES_els(_els, Keywords)
    end;
decode_vcard_CATEGORIES_els([_ | _els], Keywords) ->
    decode_vcard_CATEGORIES_els(_els, Keywords).

encode_vcard_CATEGORIES(Keywords, _xmlns_attrs) ->
    _els = 'encode_vcard_CATEGORIES_$keywords'(Keywords,
					       []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"CATEGORIES">>, _attrs, _els}.

'encode_vcard_CATEGORIES_$keywords'([], _acc) -> _acc;
'encode_vcard_CATEGORIES_$keywords'([Keywords | _els],
				    _acc) ->
    'encode_vcard_CATEGORIES_$keywords'(_els,
					[encode_vcard_KEYWORD(Keywords, [])
					 | _acc]).

decode_vcard_KEY({xmlel, <<"KEY">>, _attrs, _els}) ->
    {Cred, Type} = decode_vcard_KEY_els(_els, undefined,
					undefined),
    {vcard_key, Type, Cred}.

decode_vcard_KEY_els([], Cred, Type) -> {Cred, Type};
decode_vcard_KEY_els([{xmlel, <<"TYPE">>, _attrs, _} =
			  _el
		      | _els],
		     Cred, Type) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_KEY_els(_els, Cred,
				decode_vcard_TYPE(_el));
       true -> decode_vcard_KEY_els(_els, Cred, Type)
    end;
decode_vcard_KEY_els([{xmlel, <<"CRED">>, _attrs, _} =
			  _el
		      | _els],
		     Cred, Type) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_KEY_els(_els, decode_vcard_CRED(_el),
				Type);
       true -> decode_vcard_KEY_els(_els, Cred, Type)
    end;
decode_vcard_KEY_els([_ | _els], Cred, Type) ->
    decode_vcard_KEY_els(_els, Cred, Type).

encode_vcard_KEY({vcard_key, Type, Cred},
		 _xmlns_attrs) ->
    _els = 'encode_vcard_KEY_$type'(Type,
				    'encode_vcard_KEY_$cred'(Cred, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"KEY">>, _attrs, _els}.

'encode_vcard_KEY_$cred'(undefined, _acc) -> _acc;
'encode_vcard_KEY_$cred'(Cred, _acc) ->
    [encode_vcard_CRED(Cred, []) | _acc].

'encode_vcard_KEY_$type'(undefined, _acc) -> _acc;
'encode_vcard_KEY_$type'(Type, _acc) ->
    [encode_vcard_TYPE(Type, []) | _acc].

decode_vcard_SOUND({xmlel, <<"SOUND">>, _attrs,
		    _els}) ->
    {Phonetic, Extval, Binval} =
	decode_vcard_SOUND_els(_els, undefined, undefined,
			       undefined),
    {vcard_sound, Phonetic, Binval, Extval}.

decode_vcard_SOUND_els([], Phonetic, Extval, Binval) ->
    {Phonetic, Extval, Binval};
decode_vcard_SOUND_els([{xmlel, <<"BINVAL">>, _attrs,
			 _} =
			    _el
			| _els],
		       Phonetic, Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_SOUND_els(_els, Phonetic, Extval,
				  decode_vcard_BINVAL(_el));
       true ->
	   decode_vcard_SOUND_els(_els, Phonetic, Extval, Binval)
    end;
decode_vcard_SOUND_els([{xmlel, <<"EXTVAL">>, _attrs,
			 _} =
			    _el
			| _els],
		       Phonetic, Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_SOUND_els(_els, Phonetic,
				  decode_vcard_EXTVAL(_el), Binval);
       true ->
	   decode_vcard_SOUND_els(_els, Phonetic, Extval, Binval)
    end;
decode_vcard_SOUND_els([{xmlel, <<"PHONETIC">>, _attrs,
			 _} =
			    _el
			| _els],
		       Phonetic, Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_SOUND_els(_els, decode_vcard_PHONETIC(_el),
				  Extval, Binval);
       true ->
	   decode_vcard_SOUND_els(_els, Phonetic, Extval, Binval)
    end;
decode_vcard_SOUND_els([_ | _els], Phonetic, Extval,
		       Binval) ->
    decode_vcard_SOUND_els(_els, Phonetic, Extval, Binval).

encode_vcard_SOUND({vcard_sound, Phonetic, Binval,
		    Extval},
		   _xmlns_attrs) ->
    _els = 'encode_vcard_SOUND_$binval'(Binval,
					'encode_vcard_SOUND_$extval'(Extval,
								     'encode_vcard_SOUND_$phonetic'(Phonetic,
												    []))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"SOUND">>, _attrs, _els}.

'encode_vcard_SOUND_$phonetic'(undefined, _acc) -> _acc;
'encode_vcard_SOUND_$phonetic'(Phonetic, _acc) ->
    [encode_vcard_PHONETIC(Phonetic, []) | _acc].

'encode_vcard_SOUND_$extval'(undefined, _acc) -> _acc;
'encode_vcard_SOUND_$extval'(Extval, _acc) ->
    [encode_vcard_EXTVAL(Extval, []) | _acc].

'encode_vcard_SOUND_$binval'(undefined, _acc) -> _acc;
'encode_vcard_SOUND_$binval'(Binval, _acc) ->
    [encode_vcard_BINVAL(Binval, []) | _acc].

decode_vcard_ORG({xmlel, <<"ORG">>, _attrs, _els}) ->
    {Units, Name} = decode_vcard_ORG_els(_els, [],
					 undefined),
    {vcard_org, Name, Units}.

decode_vcard_ORG_els([], Units, Name) ->
    {lists:reverse(Units), Name};
decode_vcard_ORG_els([{xmlel, <<"ORGNAME">>, _attrs,
		       _} =
			  _el
		      | _els],
		     Units, Name) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ORG_els(_els, Units,
				decode_vcard_ORGNAME(_el));
       true -> decode_vcard_ORG_els(_els, Units, Name)
    end;
decode_vcard_ORG_els([{xmlel, <<"ORGUNIT">>, _attrs,
		       _} =
			  _el
		      | _els],
		     Units, Name) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ORG_els(_els,
				case decode_vcard_ORGUNIT(_el) of
				  undefined -> Units;
				  _new_el -> [_new_el | Units]
				end,
				Name);
       true -> decode_vcard_ORG_els(_els, Units, Name)
    end;
decode_vcard_ORG_els([_ | _els], Units, Name) ->
    decode_vcard_ORG_els(_els, Units, Name).

encode_vcard_ORG({vcard_org, Name, Units},
		 _xmlns_attrs) ->
    _els = 'encode_vcard_ORG_$name'(Name,
				    'encode_vcard_ORG_$units'(Units, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"ORG">>, _attrs, _els}.

'encode_vcard_ORG_$units'([], _acc) -> _acc;
'encode_vcard_ORG_$units'([Units | _els], _acc) ->
    'encode_vcard_ORG_$units'(_els,
			      [encode_vcard_ORGUNIT(Units, []) | _acc]).

'encode_vcard_ORG_$name'(undefined, _acc) -> _acc;
'encode_vcard_ORG_$name'(Name, _acc) ->
    [encode_vcard_ORGNAME(Name, []) | _acc].

decode_vcard_PHOTO({xmlel, <<"PHOTO">>, _attrs,
		    _els}) ->
    {Type, Extval, Binval} = decode_vcard_PHOTO_els(_els,
						    undefined, undefined,
						    undefined),
    {vcard_photo, Type, Binval, Extval}.

decode_vcard_PHOTO_els([], Type, Extval, Binval) ->
    {Type, Extval, Binval};
decode_vcard_PHOTO_els([{xmlel, <<"TYPE">>, _attrs, _} =
			    _el
			| _els],
		       Type, Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_PHOTO_els(_els, decode_vcard_TYPE(_el),
				  Extval, Binval);
       true ->
	   decode_vcard_PHOTO_els(_els, Type, Extval, Binval)
    end;
decode_vcard_PHOTO_els([{xmlel, <<"BINVAL">>, _attrs,
			 _} =
			    _el
			| _els],
		       Type, Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_PHOTO_els(_els, Type, Extval,
				  decode_vcard_BINVAL(_el));
       true ->
	   decode_vcard_PHOTO_els(_els, Type, Extval, Binval)
    end;
decode_vcard_PHOTO_els([{xmlel, <<"EXTVAL">>, _attrs,
			 _} =
			    _el
			| _els],
		       Type, Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_PHOTO_els(_els, Type,
				  decode_vcard_EXTVAL(_el), Binval);
       true ->
	   decode_vcard_PHOTO_els(_els, Type, Extval, Binval)
    end;
decode_vcard_PHOTO_els([_ | _els], Type, Extval,
		       Binval) ->
    decode_vcard_PHOTO_els(_els, Type, Extval, Binval).

encode_vcard_PHOTO({vcard_photo, Type, Binval, Extval},
		   _xmlns_attrs) ->
    _els = 'encode_vcard_PHOTO_$binval'(Binval,
					'encode_vcard_PHOTO_$extval'(Extval,
								     'encode_vcard_PHOTO_$type'(Type,
												[]))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"PHOTO">>, _attrs, _els}.

'encode_vcard_PHOTO_$type'(undefined, _acc) -> _acc;
'encode_vcard_PHOTO_$type'(Type, _acc) ->
    [encode_vcard_TYPE(Type, []) | _acc].

'encode_vcard_PHOTO_$extval'(undefined, _acc) -> _acc;
'encode_vcard_PHOTO_$extval'(Extval, _acc) ->
    [encode_vcard_EXTVAL(Extval, []) | _acc].

'encode_vcard_PHOTO_$binval'(undefined, _acc) -> _acc;
'encode_vcard_PHOTO_$binval'(Binval, _acc) ->
    [encode_vcard_BINVAL(Binval, []) | _acc].

decode_vcard_LOGO({xmlel, <<"LOGO">>, _attrs, _els}) ->
    {Type, Extval, Binval} = decode_vcard_LOGO_els(_els,
						   undefined, undefined,
						   undefined),
    {vcard_logo, Type, Binval, Extval}.

decode_vcard_LOGO_els([], Type, Extval, Binval) ->
    {Type, Extval, Binval};
decode_vcard_LOGO_els([{xmlel, <<"TYPE">>, _attrs, _} =
			   _el
		       | _els],
		      Type, Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_LOGO_els(_els, decode_vcard_TYPE(_el),
				 Extval, Binval);
       true ->
	   decode_vcard_LOGO_els(_els, Type, Extval, Binval)
    end;
decode_vcard_LOGO_els([{xmlel, <<"BINVAL">>, _attrs,
			_} =
			   _el
		       | _els],
		      Type, Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_LOGO_els(_els, Type, Extval,
				 decode_vcard_BINVAL(_el));
       true ->
	   decode_vcard_LOGO_els(_els, Type, Extval, Binval)
    end;
decode_vcard_LOGO_els([{xmlel, <<"EXTVAL">>, _attrs,
			_} =
			   _el
		       | _els],
		      Type, Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_LOGO_els(_els, Type,
				 decode_vcard_EXTVAL(_el), Binval);
       true ->
	   decode_vcard_LOGO_els(_els, Type, Extval, Binval)
    end;
decode_vcard_LOGO_els([_ | _els], Type, Extval,
		      Binval) ->
    decode_vcard_LOGO_els(_els, Type, Extval, Binval).

encode_vcard_LOGO({vcard_logo, Type, Binval, Extval},
		  _xmlns_attrs) ->
    _els = 'encode_vcard_LOGO_$binval'(Binval,
				       'encode_vcard_LOGO_$extval'(Extval,
								   'encode_vcard_LOGO_$type'(Type,
											     []))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"LOGO">>, _attrs, _els}.

'encode_vcard_LOGO_$type'(undefined, _acc) -> _acc;
'encode_vcard_LOGO_$type'(Type, _acc) ->
    [encode_vcard_TYPE(Type, []) | _acc].

'encode_vcard_LOGO_$extval'(undefined, _acc) -> _acc;
'encode_vcard_LOGO_$extval'(Extval, _acc) ->
    [encode_vcard_EXTVAL(Extval, []) | _acc].

'encode_vcard_LOGO_$binval'(undefined, _acc) -> _acc;
'encode_vcard_LOGO_$binval'(Binval, _acc) ->
    [encode_vcard_BINVAL(Binval, []) | _acc].

decode_vcard_BINVAL({xmlel, <<"BINVAL">>, _attrs,
		     _els}) ->
    Cdata = decode_vcard_BINVAL_els(_els, <<>>), Cdata.

decode_vcard_BINVAL_els([], Cdata) ->
    decode_vcard_BINVAL_cdata(Cdata);
decode_vcard_BINVAL_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_vcard_BINVAL_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_BINVAL_els([_ | _els], Cdata) ->
    decode_vcard_BINVAL_els(_els, Cdata).

encode_vcard_BINVAL(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_BINVAL_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"BINVAL">>, _attrs, _els}.

decode_vcard_BINVAL_cdata(<<>>) -> undefined;
decode_vcard_BINVAL_cdata(_val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"BINVAL">>,
			 <<"vcard-temp">>}});
      _res -> _res
    end.

encode_vcard_BINVAL_cdata(undefined, _acc) -> _acc;
encode_vcard_BINVAL_cdata(_val, _acc) ->
    [{xmlcdata, base64:encode(_val)} | _acc].

decode_vcard_GEO({xmlel, <<"GEO">>, _attrs, _els}) ->
    {Lat, Lon} = decode_vcard_GEO_els(_els, undefined,
				      undefined),
    {vcard_geo, Lat, Lon}.

decode_vcard_GEO_els([], Lat, Lon) -> {Lat, Lon};
decode_vcard_GEO_els([{xmlel, <<"LAT">>, _attrs, _} =
			  _el
		      | _els],
		     Lat, Lon) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_GEO_els(_els, decode_vcard_LAT(_el), Lon);
       true -> decode_vcard_GEO_els(_els, Lat, Lon)
    end;
decode_vcard_GEO_els([{xmlel, <<"LON">>, _attrs, _} =
			  _el
		      | _els],
		     Lat, Lon) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_GEO_els(_els, Lat, decode_vcard_LON(_el));
       true -> decode_vcard_GEO_els(_els, Lat, Lon)
    end;
decode_vcard_GEO_els([_ | _els], Lat, Lon) ->
    decode_vcard_GEO_els(_els, Lat, Lon).

encode_vcard_GEO({vcard_geo, Lat, Lon}, _xmlns_attrs) ->
    _els = 'encode_vcard_GEO_$lon'(Lon,
				   'encode_vcard_GEO_$lat'(Lat, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"GEO">>, _attrs, _els}.

'encode_vcard_GEO_$lat'(undefined, _acc) -> _acc;
'encode_vcard_GEO_$lat'(Lat, _acc) ->
    [encode_vcard_LAT(Lat, []) | _acc].

'encode_vcard_GEO_$lon'(undefined, _acc) -> _acc;
'encode_vcard_GEO_$lon'(Lon, _acc) ->
    [encode_vcard_LON(Lon, []) | _acc].

decode_vcard_EMAIL({xmlel, <<"EMAIL">>, _attrs,
		    _els}) ->
    {X400, Userid, Internet, Home, Pref, Work} =
	decode_vcard_EMAIL_els(_els, false, undefined, false,
			       false, false, false),
    {vcard_email, Home, Work, Internet, Pref, X400, Userid}.

decode_vcard_EMAIL_els([], X400, Userid, Internet, Home,
		       Pref, Work) ->
    {X400, Userid, Internet, Home, Pref, Work};
decode_vcard_EMAIL_els([{xmlel, <<"HOME">>, _attrs, _} =
			    _el
			| _els],
		       X400, Userid, Internet, Home, Pref, Work) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_EMAIL_els(_els, X400, Userid, Internet,
				  decode_vcard_HOME(_el), Pref, Work);
       true ->
	   decode_vcard_EMAIL_els(_els, X400, Userid, Internet,
				  Home, Pref, Work)
    end;
decode_vcard_EMAIL_els([{xmlel, <<"WORK">>, _attrs, _} =
			    _el
			| _els],
		       X400, Userid, Internet, Home, Pref, Work) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_EMAIL_els(_els, X400, Userid, Internet,
				  Home, Pref, decode_vcard_WORK(_el));
       true ->
	   decode_vcard_EMAIL_els(_els, X400, Userid, Internet,
				  Home, Pref, Work)
    end;
decode_vcard_EMAIL_els([{xmlel, <<"INTERNET">>, _attrs,
			 _} =
			    _el
			| _els],
		       X400, Userid, Internet, Home, Pref, Work) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_EMAIL_els(_els, X400, Userid,
				  decode_vcard_INTERNET(_el), Home, Pref, Work);
       true ->
	   decode_vcard_EMAIL_els(_els, X400, Userid, Internet,
				  Home, Pref, Work)
    end;
decode_vcard_EMAIL_els([{xmlel, <<"PREF">>, _attrs, _} =
			    _el
			| _els],
		       X400, Userid, Internet, Home, Pref, Work) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_EMAIL_els(_els, X400, Userid, Internet,
				  Home, decode_vcard_PREF(_el), Work);
       true ->
	   decode_vcard_EMAIL_els(_els, X400, Userid, Internet,
				  Home, Pref, Work)
    end;
decode_vcard_EMAIL_els([{xmlel, <<"X400">>, _attrs, _} =
			    _el
			| _els],
		       X400, Userid, Internet, Home, Pref, Work) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_EMAIL_els(_els, decode_vcard_X400(_el),
				  Userid, Internet, Home, Pref, Work);
       true ->
	   decode_vcard_EMAIL_els(_els, X400, Userid, Internet,
				  Home, Pref, Work)
    end;
decode_vcard_EMAIL_els([{xmlel, <<"USERID">>, _attrs,
			 _} =
			    _el
			| _els],
		       X400, Userid, Internet, Home, Pref, Work) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_EMAIL_els(_els, X400,
				  decode_vcard_USERID(_el), Internet, Home,
				  Pref, Work);
       true ->
	   decode_vcard_EMAIL_els(_els, X400, Userid, Internet,
				  Home, Pref, Work)
    end;
decode_vcard_EMAIL_els([_ | _els], X400, Userid,
		       Internet, Home, Pref, Work) ->
    decode_vcard_EMAIL_els(_els, X400, Userid, Internet,
			   Home, Pref, Work).

encode_vcard_EMAIL({vcard_email, Home, Work, Internet,
		    Pref, X400, Userid},
		   _xmlns_attrs) ->
    _els = 'encode_vcard_EMAIL_$work'(Work,
				      'encode_vcard_EMAIL_$pref'(Pref,
								 'encode_vcard_EMAIL_$home'(Home,
											    'encode_vcard_EMAIL_$internet'(Internet,
															   'encode_vcard_EMAIL_$userid'(Userid,
																			'encode_vcard_EMAIL_$x400'(X400,
																						   [])))))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"EMAIL">>, _attrs, _els}.

'encode_vcard_EMAIL_$x400'(false, _acc) -> _acc;
'encode_vcard_EMAIL_$x400'(X400, _acc) ->
    [encode_vcard_X400(X400, []) | _acc].

'encode_vcard_EMAIL_$userid'(undefined, _acc) -> _acc;
'encode_vcard_EMAIL_$userid'(Userid, _acc) ->
    [encode_vcard_USERID(Userid, []) | _acc].

'encode_vcard_EMAIL_$internet'(false, _acc) -> _acc;
'encode_vcard_EMAIL_$internet'(Internet, _acc) ->
    [encode_vcard_INTERNET(Internet, []) | _acc].

'encode_vcard_EMAIL_$home'(false, _acc) -> _acc;
'encode_vcard_EMAIL_$home'(Home, _acc) ->
    [encode_vcard_HOME(Home, []) | _acc].

'encode_vcard_EMAIL_$pref'(false, _acc) -> _acc;
'encode_vcard_EMAIL_$pref'(Pref, _acc) ->
    [encode_vcard_PREF(Pref, []) | _acc].

'encode_vcard_EMAIL_$work'(false, _acc) -> _acc;
'encode_vcard_EMAIL_$work'(Work, _acc) ->
    [encode_vcard_WORK(Work, []) | _acc].

decode_vcard_TEL({xmlel, <<"TEL">>, _attrs, _els}) ->
    {Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
     Work, Cell, Modem, Isdn, Video} =
	decode_vcard_TEL_els(_els, undefined, false, false,
			     false, false, false, false, false, false, false,
			     false, false, false, false),
    {vcard_tel, Home, Work, Voice, Fax, Pager, Msg, Cell,
     Video, Bbs, Modem, Isdn, Pcs, Pref, Number}.

decode_vcard_TEL_els([], Number, Pager, Pcs, Bbs, Voice,
		     Home, Pref, Msg, Fax, Work, Cell, Modem, Isdn, Video) ->
    {Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
     Work, Cell, Modem, Isdn, Video};
decode_vcard_TEL_els([{xmlel, <<"HOME">>, _attrs, _} =
			  _el
		      | _els],
		     Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
		     Work, Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, decode_vcard_HOME(_el), Pref, Msg, Fax,
				Work, Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, Video)
    end;
decode_vcard_TEL_els([{xmlel, <<"WORK">>, _attrs, _} =
			  _el
		      | _els],
		     Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
		     Work, Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax,
				decode_vcard_WORK(_el), Cell, Modem, Isdn,
				Video);
       true ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, Video)
    end;
decode_vcard_TEL_els([{xmlel, <<"VOICE">>, _attrs, _} =
			  _el
		      | _els],
		     Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
		     Work, Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				decode_vcard_VOICE(_el), Home, Pref, Msg, Fax,
				Work, Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, Video)
    end;
decode_vcard_TEL_els([{xmlel, <<"FAX">>, _attrs, _} =
			  _el
		      | _els],
		     Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
		     Work, Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, decode_vcard_FAX(_el),
				Work, Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, Video)
    end;
decode_vcard_TEL_els([{xmlel, <<"PAGER">>, _attrs, _} =
			  _el
		      | _els],
		     Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
		     Work, Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_TEL_els(_els, Number,
				decode_vcard_PAGER(_el), Pcs, Bbs, Voice, Home,
				Pref, Msg, Fax, Work, Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, Video)
    end;
decode_vcard_TEL_els([{xmlel, <<"MSG">>, _attrs, _} =
			  _el
		      | _els],
		     Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
		     Work, Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, decode_vcard_MSG(_el), Fax,
				Work, Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, Video)
    end;
decode_vcard_TEL_els([{xmlel, <<"CELL">>, _attrs, _} =
			  _el
		      | _els],
		     Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
		     Work, Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work,
				decode_vcard_CELL(_el), Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, Video)
    end;
decode_vcard_TEL_els([{xmlel, <<"VIDEO">>, _attrs, _} =
			  _el
		      | _els],
		     Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
		     Work, Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, decode_vcard_VIDEO(_el));
       true ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, Video)
    end;
decode_vcard_TEL_els([{xmlel, <<"BBS">>, _attrs, _} =
			  _el
		      | _els],
		     Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
		     Work, Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs,
				decode_vcard_BBS(_el), Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, Video)
    end;
decode_vcard_TEL_els([{xmlel, <<"MODEM">>, _attrs, _} =
			  _el
		      | _els],
		     Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
		     Work, Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell,
				decode_vcard_MODEM(_el), Isdn, Video);
       true ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, Video)
    end;
decode_vcard_TEL_els([{xmlel, <<"ISDN">>, _attrs, _} =
			  _el
		      | _els],
		     Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
		     Work, Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				decode_vcard_ISDN(_el), Video);
       true ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, Video)
    end;
decode_vcard_TEL_els([{xmlel, <<"PCS">>, _attrs, _} =
			  _el
		      | _els],
		     Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
		     Work, Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_TEL_els(_els, Number, Pager,
				decode_vcard_PCS(_el), Bbs, Voice, Home, Pref,
				Msg, Fax, Work, Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, Video)
    end;
decode_vcard_TEL_els([{xmlel, <<"PREF">>, _attrs, _} =
			  _el
		      | _els],
		     Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
		     Work, Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, decode_vcard_PREF(_el), Msg, Fax,
				Work, Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, Video)
    end;
decode_vcard_TEL_els([{xmlel, <<"NUMBER">>, _attrs, _} =
			  _el
		      | _els],
		     Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
		     Work, Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_TEL_els(_els, decode_vcard_NUMBER(_el),
				Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
				Work, Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, Video)
    end;
decode_vcard_TEL_els([_ | _els], Number, Pager, Pcs,
		     Bbs, Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
		     Isdn, Video) ->
    decode_vcard_TEL_els(_els, Number, Pager, Pcs, Bbs,
			 Voice, Home, Pref, Msg, Fax, Work, Cell, Modem, Isdn,
			 Video).

encode_vcard_TEL({vcard_tel, Home, Work, Voice, Fax,
		  Pager, Msg, Cell, Video, Bbs, Modem, Isdn, Pcs, Pref,
		  Number},
		 _xmlns_attrs) ->
    _els = 'encode_vcard_TEL_$video'(Video,
				     'encode_vcard_TEL_$isdn'(Isdn,
							      'encode_vcard_TEL_$modem'(Modem,
											'encode_vcard_TEL_$cell'(Cell,
														 'encode_vcard_TEL_$work'(Work,
																	  'encode_vcard_TEL_$fax'(Fax,
																				  'encode_vcard_TEL_$msg'(Msg,
																							  'encode_vcard_TEL_$pref'(Pref,
																										   'encode_vcard_TEL_$home'(Home,
																													    'encode_vcard_TEL_$voice'(Voice,
																																      'encode_vcard_TEL_$bbs'(Bbs,
																																			      'encode_vcard_TEL_$pcs'(Pcs,
																																						      'encode_vcard_TEL_$pager'(Pager,
																																										'encode_vcard_TEL_$number'(Number,
																																													   [])))))))))))))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"TEL">>, _attrs, _els}.

'encode_vcard_TEL_$number'(undefined, _acc) -> _acc;
'encode_vcard_TEL_$number'(Number, _acc) ->
    [encode_vcard_NUMBER(Number, []) | _acc].

'encode_vcard_TEL_$pager'(false, _acc) -> _acc;
'encode_vcard_TEL_$pager'(Pager, _acc) ->
    [encode_vcard_PAGER(Pager, []) | _acc].

'encode_vcard_TEL_$pcs'(false, _acc) -> _acc;
'encode_vcard_TEL_$pcs'(Pcs, _acc) ->
    [encode_vcard_PCS(Pcs, []) | _acc].

'encode_vcard_TEL_$bbs'(false, _acc) -> _acc;
'encode_vcard_TEL_$bbs'(Bbs, _acc) ->
    [encode_vcard_BBS(Bbs, []) | _acc].

'encode_vcard_TEL_$voice'(false, _acc) -> _acc;
'encode_vcard_TEL_$voice'(Voice, _acc) ->
    [encode_vcard_VOICE(Voice, []) | _acc].

'encode_vcard_TEL_$home'(false, _acc) -> _acc;
'encode_vcard_TEL_$home'(Home, _acc) ->
    [encode_vcard_HOME(Home, []) | _acc].

'encode_vcard_TEL_$pref'(false, _acc) -> _acc;
'encode_vcard_TEL_$pref'(Pref, _acc) ->
    [encode_vcard_PREF(Pref, []) | _acc].

'encode_vcard_TEL_$msg'(false, _acc) -> _acc;
'encode_vcard_TEL_$msg'(Msg, _acc) ->
    [encode_vcard_MSG(Msg, []) | _acc].

'encode_vcard_TEL_$fax'(false, _acc) -> _acc;
'encode_vcard_TEL_$fax'(Fax, _acc) ->
    [encode_vcard_FAX(Fax, []) | _acc].

'encode_vcard_TEL_$work'(false, _acc) -> _acc;
'encode_vcard_TEL_$work'(Work, _acc) ->
    [encode_vcard_WORK(Work, []) | _acc].

'encode_vcard_TEL_$cell'(false, _acc) -> _acc;
'encode_vcard_TEL_$cell'(Cell, _acc) ->
    [encode_vcard_CELL(Cell, []) | _acc].

'encode_vcard_TEL_$modem'(false, _acc) -> _acc;
'encode_vcard_TEL_$modem'(Modem, _acc) ->
    [encode_vcard_MODEM(Modem, []) | _acc].

'encode_vcard_TEL_$isdn'(false, _acc) -> _acc;
'encode_vcard_TEL_$isdn'(Isdn, _acc) ->
    [encode_vcard_ISDN(Isdn, []) | _acc].

'encode_vcard_TEL_$video'(false, _acc) -> _acc;
'encode_vcard_TEL_$video'(Video, _acc) ->
    [encode_vcard_VIDEO(Video, []) | _acc].

decode_vcard_LABEL({xmlel, <<"LABEL">>, _attrs,
		    _els}) ->
    {Line, Home, Pref, Work, Intl, Parcel, Postal, Dom} =
	decode_vcard_LABEL_els(_els, [], false, false, false,
			       false, false, false, false),
    {vcard_label, Home, Work, Postal, Parcel, Dom, Intl,
     Pref, Line}.

decode_vcard_LABEL_els([], Line, Home, Pref, Work, Intl,
		       Parcel, Postal, Dom) ->
    {lists:reverse(Line), Home, Pref, Work, Intl, Parcel,
     Postal, Dom};
decode_vcard_LABEL_els([{xmlel, <<"HOME">>, _attrs, _} =
			    _el
			| _els],
		       Line, Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_LABEL_els(_els, Line,
				  decode_vcard_HOME(_el), Pref, Work, Intl,
				  Parcel, Postal, Dom);
       true ->
	   decode_vcard_LABEL_els(_els, Line, Home, Pref, Work,
				  Intl, Parcel, Postal, Dom)
    end;
decode_vcard_LABEL_els([{xmlel, <<"WORK">>, _attrs, _} =
			    _el
			| _els],
		       Line, Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_LABEL_els(_els, Line, Home, Pref,
				  decode_vcard_WORK(_el), Intl, Parcel, Postal,
				  Dom);
       true ->
	   decode_vcard_LABEL_els(_els, Line, Home, Pref, Work,
				  Intl, Parcel, Postal, Dom)
    end;
decode_vcard_LABEL_els([{xmlel, <<"POSTAL">>, _attrs,
			 _} =
			    _el
			| _els],
		       Line, Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_LABEL_els(_els, Line, Home, Pref, Work,
				  Intl, Parcel, decode_vcard_POSTAL(_el), Dom);
       true ->
	   decode_vcard_LABEL_els(_els, Line, Home, Pref, Work,
				  Intl, Parcel, Postal, Dom)
    end;
decode_vcard_LABEL_els([{xmlel, <<"PARCEL">>, _attrs,
			 _} =
			    _el
			| _els],
		       Line, Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_LABEL_els(_els, Line, Home, Pref, Work,
				  Intl, decode_vcard_PARCEL(_el), Postal, Dom);
       true ->
	   decode_vcard_LABEL_els(_els, Line, Home, Pref, Work,
				  Intl, Parcel, Postal, Dom)
    end;
decode_vcard_LABEL_els([{xmlel, <<"DOM">>, _attrs, _} =
			    _el
			| _els],
		       Line, Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_LABEL_els(_els, Line, Home, Pref, Work,
				  Intl, Parcel, Postal, decode_vcard_DOM(_el));
       true ->
	   decode_vcard_LABEL_els(_els, Line, Home, Pref, Work,
				  Intl, Parcel, Postal, Dom)
    end;
decode_vcard_LABEL_els([{xmlel, <<"INTL">>, _attrs, _} =
			    _el
			| _els],
		       Line, Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_LABEL_els(_els, Line, Home, Pref, Work,
				  decode_vcard_INTL(_el), Parcel, Postal, Dom);
       true ->
	   decode_vcard_LABEL_els(_els, Line, Home, Pref, Work,
				  Intl, Parcel, Postal, Dom)
    end;
decode_vcard_LABEL_els([{xmlel, <<"PREF">>, _attrs, _} =
			    _el
			| _els],
		       Line, Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_LABEL_els(_els, Line, Home,
				  decode_vcard_PREF(_el), Work, Intl, Parcel,
				  Postal, Dom);
       true ->
	   decode_vcard_LABEL_els(_els, Line, Home, Pref, Work,
				  Intl, Parcel, Postal, Dom)
    end;
decode_vcard_LABEL_els([{xmlel, <<"LINE">>, _attrs, _} =
			    _el
			| _els],
		       Line, Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_LABEL_els(_els,
				  case decode_vcard_LINE(_el) of
				    undefined -> Line;
				    _new_el -> [_new_el | Line]
				  end,
				  Home, Pref, Work, Intl, Parcel, Postal, Dom);
       true ->
	   decode_vcard_LABEL_els(_els, Line, Home, Pref, Work,
				  Intl, Parcel, Postal, Dom)
    end;
decode_vcard_LABEL_els([_ | _els], Line, Home, Pref,
		       Work, Intl, Parcel, Postal, Dom) ->
    decode_vcard_LABEL_els(_els, Line, Home, Pref, Work,
			   Intl, Parcel, Postal, Dom).

encode_vcard_LABEL({vcard_label, Home, Work, Postal,
		    Parcel, Dom, Intl, Pref, Line},
		   _xmlns_attrs) ->
    _els = 'encode_vcard_LABEL_$dom'(Dom,
				     'encode_vcard_LABEL_$postal'(Postal,
								  'encode_vcard_LABEL_$parcel'(Parcel,
											       'encode_vcard_LABEL_$intl'(Intl,
															  'encode_vcard_LABEL_$work'(Work,
																		     'encode_vcard_LABEL_$pref'(Pref,
																						'encode_vcard_LABEL_$home'(Home,
																									   'encode_vcard_LABEL_$line'(Line,
																												      [])))))))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"LABEL">>, _attrs, _els}.

'encode_vcard_LABEL_$line'([], _acc) -> _acc;
'encode_vcard_LABEL_$line'([Line | _els], _acc) ->
    'encode_vcard_LABEL_$line'(_els,
			       [encode_vcard_LINE(Line, []) | _acc]).

'encode_vcard_LABEL_$home'(false, _acc) -> _acc;
'encode_vcard_LABEL_$home'(Home, _acc) ->
    [encode_vcard_HOME(Home, []) | _acc].

'encode_vcard_LABEL_$pref'(false, _acc) -> _acc;
'encode_vcard_LABEL_$pref'(Pref, _acc) ->
    [encode_vcard_PREF(Pref, []) | _acc].

'encode_vcard_LABEL_$work'(false, _acc) -> _acc;
'encode_vcard_LABEL_$work'(Work, _acc) ->
    [encode_vcard_WORK(Work, []) | _acc].

'encode_vcard_LABEL_$intl'(false, _acc) -> _acc;
'encode_vcard_LABEL_$intl'(Intl, _acc) ->
    [encode_vcard_INTL(Intl, []) | _acc].

'encode_vcard_LABEL_$parcel'(false, _acc) -> _acc;
'encode_vcard_LABEL_$parcel'(Parcel, _acc) ->
    [encode_vcard_PARCEL(Parcel, []) | _acc].

'encode_vcard_LABEL_$postal'(false, _acc) -> _acc;
'encode_vcard_LABEL_$postal'(Postal, _acc) ->
    [encode_vcard_POSTAL(Postal, []) | _acc].

'encode_vcard_LABEL_$dom'(false, _acc) -> _acc;
'encode_vcard_LABEL_$dom'(Dom, _acc) ->
    [encode_vcard_DOM(Dom, []) | _acc].

decode_vcard_ADR({xmlel, <<"ADR">>, _attrs, _els}) ->
    {Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
     Locality, Work, Intl, Parcel, Postal, Dom, Region} =
	decode_vcard_ADR_els(_els, undefined, undefined,
			     undefined, false, false, undefined, undefined,
			     undefined, false, false, false, false, false,
			     undefined),
    {vcard_adr, Home, Work, Postal, Parcel, Dom, Intl, Pref,
     Pobox, Extadd, Street, Locality, Region, Pcode, Ctry}.

decode_vcard_ADR_els([], Street, Extadd, Pcode, Home,
		     Pref, Pobox, Ctry, Locality, Work, Intl, Parcel, Postal,
		     Dom, Region) ->
    {Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
     Locality, Work, Intl, Parcel, Postal, Dom, Region};
decode_vcard_ADR_els([{xmlel, <<"HOME">>, _attrs, _} =
			  _el
		      | _els],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode,
				decode_vcard_HOME(_el), Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region);
       true ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region)
    end;
decode_vcard_ADR_els([{xmlel, <<"WORK">>, _attrs, _} =
			  _el
		      | _els],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality,
				decode_vcard_WORK(_el), Intl, Parcel, Postal,
				Dom, Region);
       true ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region)
    end;
decode_vcard_ADR_els([{xmlel, <<"POSTAL">>, _attrs, _} =
			  _el
		      | _els],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				decode_vcard_POSTAL(_el), Dom, Region);
       true ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region)
    end;
decode_vcard_ADR_els([{xmlel, <<"PARCEL">>, _attrs, _} =
			  _el
		      | _els],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl,
				decode_vcard_PARCEL(_el), Postal, Dom, Region);
       true ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region)
    end;
decode_vcard_ADR_els([{xmlel, <<"DOM">>, _attrs, _} =
			  _el
		      | _els],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, decode_vcard_DOM(_el), Region);
       true ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region)
    end;
decode_vcard_ADR_els([{xmlel, <<"INTL">>, _attrs, _} =
			  _el
		      | _els],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work,
				decode_vcard_INTL(_el), Parcel, Postal, Dom,
				Region);
       true ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region)
    end;
decode_vcard_ADR_els([{xmlel, <<"PREF">>, _attrs, _} =
			  _el
		      | _els],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				decode_vcard_PREF(_el), Pobox, Ctry, Locality,
				Work, Intl, Parcel, Postal, Dom, Region);
       true ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region)
    end;
decode_vcard_ADR_els([{xmlel, <<"POBOX">>, _attrs, _} =
			  _el
		      | _els],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, decode_vcard_POBOX(_el), Ctry, Locality,
				Work, Intl, Parcel, Postal, Dom, Region);
       true ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region)
    end;
decode_vcard_ADR_els([{xmlel, <<"EXTADD">>, _attrs, _} =
			  _el
		      | _els],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ADR_els(_els, Street,
				decode_vcard_EXTADD(_el), Pcode, Home, Pref,
				Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region);
       true ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region)
    end;
decode_vcard_ADR_els([{xmlel, <<"STREET">>, _attrs, _} =
			  _el
		      | _els],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ADR_els(_els, decode_vcard_STREET(_el),
				Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region);
       true ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region)
    end;
decode_vcard_ADR_els([{xmlel, <<"LOCALITY">>, _attrs,
		       _} =
			  _el
		      | _els],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, decode_vcard_LOCALITY(_el),
				Work, Intl, Parcel, Postal, Dom, Region);
       true ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region)
    end;
decode_vcard_ADR_els([{xmlel, <<"REGION">>, _attrs, _} =
			  _el
		      | _els],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, decode_vcard_REGION(_el));
       true ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region)
    end;
decode_vcard_ADR_els([{xmlel, <<"PCODE">>, _attrs, _} =
			  _el
		      | _els],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ADR_els(_els, Street, Extadd,
				decode_vcard_PCODE(_el), Home, Pref, Pobox,
				Ctry, Locality, Work, Intl, Parcel, Postal, Dom,
				Region);
       true ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region)
    end;
decode_vcard_ADR_els([{xmlel, <<"CTRY">>, _attrs, _} =
			  _el
		      | _els],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, decode_vcard_CTRY(_el), Locality,
				Work, Intl, Parcel, Postal, Dom, Region);
       true ->
	   decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region)
    end;
decode_vcard_ADR_els([_ | _els], Street, Extadd, Pcode,
		     Home, Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
		     Postal, Dom, Region) ->
    decode_vcard_ADR_els(_els, Street, Extadd, Pcode, Home,
			 Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
			 Postal, Dom, Region).

encode_vcard_ADR({vcard_adr, Home, Work, Postal, Parcel,
		  Dom, Intl, Pref, Pobox, Extadd, Street, Locality,
		  Region, Pcode, Ctry},
		 _xmlns_attrs) ->
    _els = 'encode_vcard_ADR_$region'(Region,
				      'encode_vcard_ADR_$dom'(Dom,
							      'encode_vcard_ADR_$postal'(Postal,
											 'encode_vcard_ADR_$parcel'(Parcel,
														    'encode_vcard_ADR_$intl'(Intl,
																	     'encode_vcard_ADR_$work'(Work,
																				      'encode_vcard_ADR_$locality'(Locality,
																								   'encode_vcard_ADR_$ctry'(Ctry,
																											    'encode_vcard_ADR_$pobox'(Pobox,
																														      'encode_vcard_ADR_$pref'(Pref,
																																	       'encode_vcard_ADR_$home'(Home,
																																					'encode_vcard_ADR_$pcode'(Pcode,
																																								  'encode_vcard_ADR_$extadd'(Extadd,
																																											     'encode_vcard_ADR_$street'(Street,
																																															[])))))))))))))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"ADR">>, _attrs, _els}.

'encode_vcard_ADR_$street'(undefined, _acc) -> _acc;
'encode_vcard_ADR_$street'(Street, _acc) ->
    [encode_vcard_STREET(Street, []) | _acc].

'encode_vcard_ADR_$extadd'(undefined, _acc) -> _acc;
'encode_vcard_ADR_$extadd'(Extadd, _acc) ->
    [encode_vcard_EXTADD(Extadd, []) | _acc].

'encode_vcard_ADR_$pcode'(undefined, _acc) -> _acc;
'encode_vcard_ADR_$pcode'(Pcode, _acc) ->
    [encode_vcard_PCODE(Pcode, []) | _acc].

'encode_vcard_ADR_$home'(false, _acc) -> _acc;
'encode_vcard_ADR_$home'(Home, _acc) ->
    [encode_vcard_HOME(Home, []) | _acc].

'encode_vcard_ADR_$pref'(false, _acc) -> _acc;
'encode_vcard_ADR_$pref'(Pref, _acc) ->
    [encode_vcard_PREF(Pref, []) | _acc].

'encode_vcard_ADR_$pobox'(undefined, _acc) -> _acc;
'encode_vcard_ADR_$pobox'(Pobox, _acc) ->
    [encode_vcard_POBOX(Pobox, []) | _acc].

'encode_vcard_ADR_$ctry'(undefined, _acc) -> _acc;
'encode_vcard_ADR_$ctry'(Ctry, _acc) ->
    [encode_vcard_CTRY(Ctry, []) | _acc].

'encode_vcard_ADR_$locality'(undefined, _acc) -> _acc;
'encode_vcard_ADR_$locality'(Locality, _acc) ->
    [encode_vcard_LOCALITY(Locality, []) | _acc].

'encode_vcard_ADR_$work'(false, _acc) -> _acc;
'encode_vcard_ADR_$work'(Work, _acc) ->
    [encode_vcard_WORK(Work, []) | _acc].

'encode_vcard_ADR_$intl'(false, _acc) -> _acc;
'encode_vcard_ADR_$intl'(Intl, _acc) ->
    [encode_vcard_INTL(Intl, []) | _acc].

'encode_vcard_ADR_$parcel'(false, _acc) -> _acc;
'encode_vcard_ADR_$parcel'(Parcel, _acc) ->
    [encode_vcard_PARCEL(Parcel, []) | _acc].

'encode_vcard_ADR_$postal'(false, _acc) -> _acc;
'encode_vcard_ADR_$postal'(Postal, _acc) ->
    [encode_vcard_POSTAL(Postal, []) | _acc].

'encode_vcard_ADR_$dom'(false, _acc) -> _acc;
'encode_vcard_ADR_$dom'(Dom, _acc) ->
    [encode_vcard_DOM(Dom, []) | _acc].

'encode_vcard_ADR_$region'(undefined, _acc) -> _acc;
'encode_vcard_ADR_$region'(Region, _acc) ->
    [encode_vcard_REGION(Region, []) | _acc].

decode_vcard_N({xmlel, <<"N">>, _attrs, _els}) ->
    {Middle, Suffix, Prefix, Family, Given} =
	decode_vcard_N_els(_els, undefined, undefined,
			   undefined, undefined, undefined),
    {vcard_name, Family, Given, Middle, Prefix, Suffix}.

decode_vcard_N_els([], Middle, Suffix, Prefix, Family,
		   Given) ->
    {Middle, Suffix, Prefix, Family, Given};
decode_vcard_N_els([{xmlel, <<"FAMILY">>, _attrs, _} =
			_el
		    | _els],
		   Middle, Suffix, Prefix, Family, Given) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_N_els(_els, Middle, Suffix, Prefix,
			      decode_vcard_FAMILY(_el), Given);
       true ->
	   decode_vcard_N_els(_els, Middle, Suffix, Prefix, Family,
			      Given)
    end;
decode_vcard_N_els([{xmlel, <<"GIVEN">>, _attrs, _} =
			_el
		    | _els],
		   Middle, Suffix, Prefix, Family, Given) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_N_els(_els, Middle, Suffix, Prefix, Family,
			      decode_vcard_GIVEN(_el));
       true ->
	   decode_vcard_N_els(_els, Middle, Suffix, Prefix, Family,
			      Given)
    end;
decode_vcard_N_els([{xmlel, <<"MIDDLE">>, _attrs, _} =
			_el
		    | _els],
		   Middle, Suffix, Prefix, Family, Given) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_N_els(_els, decode_vcard_MIDDLE(_el),
			      Suffix, Prefix, Family, Given);
       true ->
	   decode_vcard_N_els(_els, Middle, Suffix, Prefix, Family,
			      Given)
    end;
decode_vcard_N_els([{xmlel, <<"PREFIX">>, _attrs, _} =
			_el
		    | _els],
		   Middle, Suffix, Prefix, Family, Given) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_N_els(_els, Middle, Suffix,
			      decode_vcard_PREFIX(_el), Family, Given);
       true ->
	   decode_vcard_N_els(_els, Middle, Suffix, Prefix, Family,
			      Given)
    end;
decode_vcard_N_els([{xmlel, <<"SUFFIX">>, _attrs, _} =
			_el
		    | _els],
		   Middle, Suffix, Prefix, Family, Given) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"vcard-temp">> ->
	   decode_vcard_N_els(_els, Middle,
			      decode_vcard_SUFFIX(_el), Prefix, Family, Given);
       true ->
	   decode_vcard_N_els(_els, Middle, Suffix, Prefix, Family,
			      Given)
    end;
decode_vcard_N_els([_ | _els], Middle, Suffix, Prefix,
		   Family, Given) ->
    decode_vcard_N_els(_els, Middle, Suffix, Prefix, Family,
		       Given).

encode_vcard_N({vcard_name, Family, Given, Middle,
		Prefix, Suffix},
	       _xmlns_attrs) ->
    _els = 'encode_vcard_N_$given'(Given,
				   'encode_vcard_N_$family'(Family,
							    'encode_vcard_N_$prefix'(Prefix,
										     'encode_vcard_N_$suffix'(Suffix,
													      'encode_vcard_N_$middle'(Middle,
																       []))))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"N">>, _attrs, _els}.

'encode_vcard_N_$middle'(undefined, _acc) -> _acc;
'encode_vcard_N_$middle'(Middle, _acc) ->
    [encode_vcard_MIDDLE(Middle, []) | _acc].

'encode_vcard_N_$suffix'(undefined, _acc) -> _acc;
'encode_vcard_N_$suffix'(Suffix, _acc) ->
    [encode_vcard_SUFFIX(Suffix, []) | _acc].

'encode_vcard_N_$prefix'(undefined, _acc) -> _acc;
'encode_vcard_N_$prefix'(Prefix, _acc) ->
    [encode_vcard_PREFIX(Prefix, []) | _acc].

'encode_vcard_N_$family'(undefined, _acc) -> _acc;
'encode_vcard_N_$family'(Family, _acc) ->
    [encode_vcard_FAMILY(Family, []) | _acc].

'encode_vcard_N_$given'(undefined, _acc) -> _acc;
'encode_vcard_N_$given'(Given, _acc) ->
    [encode_vcard_GIVEN(Given, []) | _acc].

decode_vcard_CONFIDENTIAL({xmlel, <<"CONFIDENTIAL">>,
			   _attrs, _els}) ->
    confidential.

encode_vcard_CONFIDENTIAL(confidential, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"CONFIDENTIAL">>, _attrs, _els}.

decode_vcard_PRIVATE({xmlel, <<"PRIVATE">>, _attrs,
		      _els}) ->
    private.

encode_vcard_PRIVATE(private, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"PRIVATE">>, _attrs, _els}.

decode_vcard_PUBLIC({xmlel, <<"PUBLIC">>, _attrs,
		     _els}) ->
    public.

encode_vcard_PUBLIC(public, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"PUBLIC">>, _attrs, _els}.

decode_vcard_EXTVAL({xmlel, <<"EXTVAL">>, _attrs,
		     _els}) ->
    Cdata = decode_vcard_EXTVAL_els(_els, <<>>), Cdata.

decode_vcard_EXTVAL_els([], Cdata) ->
    decode_vcard_EXTVAL_cdata(Cdata);
decode_vcard_EXTVAL_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_vcard_EXTVAL_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_EXTVAL_els([_ | _els], Cdata) ->
    decode_vcard_EXTVAL_els(_els, Cdata).

encode_vcard_EXTVAL(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_EXTVAL_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"EXTVAL">>, _attrs, _els}.

decode_vcard_EXTVAL_cdata(<<>>) -> undefined;
decode_vcard_EXTVAL_cdata(_val) -> _val.

encode_vcard_EXTVAL_cdata(undefined, _acc) -> _acc;
encode_vcard_EXTVAL_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_TYPE({xmlel, <<"TYPE">>, _attrs, _els}) ->
    Cdata = decode_vcard_TYPE_els(_els, <<>>), Cdata.

decode_vcard_TYPE_els([], Cdata) ->
    decode_vcard_TYPE_cdata(Cdata);
decode_vcard_TYPE_els([{xmlcdata, _data} | _els],
		      Cdata) ->
    decode_vcard_TYPE_els(_els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_TYPE_els([_ | _els], Cdata) ->
    decode_vcard_TYPE_els(_els, Cdata).

encode_vcard_TYPE(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_TYPE_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"TYPE">>, _attrs, _els}.

decode_vcard_TYPE_cdata(<<>>) -> undefined;
decode_vcard_TYPE_cdata(_val) -> _val.

encode_vcard_TYPE_cdata(undefined, _acc) -> _acc;
encode_vcard_TYPE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_DESC({xmlel, <<"DESC">>, _attrs, _els}) ->
    Cdata = decode_vcard_DESC_els(_els, <<>>), Cdata.

decode_vcard_DESC_els([], Cdata) ->
    decode_vcard_DESC_cdata(Cdata);
decode_vcard_DESC_els([{xmlcdata, _data} | _els],
		      Cdata) ->
    decode_vcard_DESC_els(_els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_DESC_els([_ | _els], Cdata) ->
    decode_vcard_DESC_els(_els, Cdata).

encode_vcard_DESC(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_DESC_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"DESC">>, _attrs, _els}.

decode_vcard_DESC_cdata(<<>>) -> undefined;
decode_vcard_DESC_cdata(_val) -> _val.

encode_vcard_DESC_cdata(undefined, _acc) -> _acc;
encode_vcard_DESC_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_URL({xmlel, <<"URL">>, _attrs, _els}) ->
    Cdata = decode_vcard_URL_els(_els, <<>>), Cdata.

decode_vcard_URL_els([], Cdata) ->
    decode_vcard_URL_cdata(Cdata);
decode_vcard_URL_els([{xmlcdata, _data} | _els],
		     Cdata) ->
    decode_vcard_URL_els(_els,
			 <<Cdata/binary, _data/binary>>);
decode_vcard_URL_els([_ | _els], Cdata) ->
    decode_vcard_URL_els(_els, Cdata).

encode_vcard_URL(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_URL_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"URL">>, _attrs, _els}.

decode_vcard_URL_cdata(<<>>) -> undefined;
decode_vcard_URL_cdata(_val) -> _val.

encode_vcard_URL_cdata(undefined, _acc) -> _acc;
encode_vcard_URL_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_UID({xmlel, <<"UID">>, _attrs, _els}) ->
    Cdata = decode_vcard_UID_els(_els, <<>>), Cdata.

decode_vcard_UID_els([], Cdata) ->
    decode_vcard_UID_cdata(Cdata);
decode_vcard_UID_els([{xmlcdata, _data} | _els],
		     Cdata) ->
    decode_vcard_UID_els(_els,
			 <<Cdata/binary, _data/binary>>);
decode_vcard_UID_els([_ | _els], Cdata) ->
    decode_vcard_UID_els(_els, Cdata).

encode_vcard_UID(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_UID_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"UID">>, _attrs, _els}.

decode_vcard_UID_cdata(<<>>) -> undefined;
decode_vcard_UID_cdata(_val) -> _val.

encode_vcard_UID_cdata(undefined, _acc) -> _acc;
encode_vcard_UID_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_SORT_STRING({xmlel, <<"SORT-STRING">>,
			  _attrs, _els}) ->
    Cdata = decode_vcard_SORT_STRING_els(_els, <<>>), Cdata.

decode_vcard_SORT_STRING_els([], Cdata) ->
    decode_vcard_SORT_STRING_cdata(Cdata);
decode_vcard_SORT_STRING_els([{xmlcdata, _data} | _els],
			     Cdata) ->
    decode_vcard_SORT_STRING_els(_els,
				 <<Cdata/binary, _data/binary>>);
decode_vcard_SORT_STRING_els([_ | _els], Cdata) ->
    decode_vcard_SORT_STRING_els(_els, Cdata).

encode_vcard_SORT_STRING(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_SORT_STRING_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"SORT-STRING">>, _attrs, _els}.

decode_vcard_SORT_STRING_cdata(<<>>) -> undefined;
decode_vcard_SORT_STRING_cdata(_val) -> _val.

encode_vcard_SORT_STRING_cdata(undefined, _acc) -> _acc;
encode_vcard_SORT_STRING_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_REV({xmlel, <<"REV">>, _attrs, _els}) ->
    Cdata = decode_vcard_REV_els(_els, <<>>), Cdata.

decode_vcard_REV_els([], Cdata) ->
    decode_vcard_REV_cdata(Cdata);
decode_vcard_REV_els([{xmlcdata, _data} | _els],
		     Cdata) ->
    decode_vcard_REV_els(_els,
			 <<Cdata/binary, _data/binary>>);
decode_vcard_REV_els([_ | _els], Cdata) ->
    decode_vcard_REV_els(_els, Cdata).

encode_vcard_REV(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_REV_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"REV">>, _attrs, _els}.

decode_vcard_REV_cdata(<<>>) -> undefined;
decode_vcard_REV_cdata(_val) -> _val.

encode_vcard_REV_cdata(undefined, _acc) -> _acc;
encode_vcard_REV_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_PRODID({xmlel, <<"PRODID">>, _attrs,
		     _els}) ->
    Cdata = decode_vcard_PRODID_els(_els, <<>>), Cdata.

decode_vcard_PRODID_els([], Cdata) ->
    decode_vcard_PRODID_cdata(Cdata);
decode_vcard_PRODID_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_vcard_PRODID_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_PRODID_els([_ | _els], Cdata) ->
    decode_vcard_PRODID_els(_els, Cdata).

encode_vcard_PRODID(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_PRODID_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"PRODID">>, _attrs, _els}.

decode_vcard_PRODID_cdata(<<>>) -> undefined;
decode_vcard_PRODID_cdata(_val) -> _val.

encode_vcard_PRODID_cdata(undefined, _acc) -> _acc;
encode_vcard_PRODID_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_NOTE({xmlel, <<"NOTE">>, _attrs, _els}) ->
    Cdata = decode_vcard_NOTE_els(_els, <<>>), Cdata.

decode_vcard_NOTE_els([], Cdata) ->
    decode_vcard_NOTE_cdata(Cdata);
decode_vcard_NOTE_els([{xmlcdata, _data} | _els],
		      Cdata) ->
    decode_vcard_NOTE_els(_els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_NOTE_els([_ | _els], Cdata) ->
    decode_vcard_NOTE_els(_els, Cdata).

encode_vcard_NOTE(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_NOTE_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"NOTE">>, _attrs, _els}.

decode_vcard_NOTE_cdata(<<>>) -> undefined;
decode_vcard_NOTE_cdata(_val) -> _val.

encode_vcard_NOTE_cdata(undefined, _acc) -> _acc;
encode_vcard_NOTE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_KEYWORD({xmlel, <<"KEYWORD">>, _attrs,
		      _els}) ->
    Cdata = decode_vcard_KEYWORD_els(_els, <<>>), Cdata.

decode_vcard_KEYWORD_els([], Cdata) ->
    decode_vcard_KEYWORD_cdata(Cdata);
decode_vcard_KEYWORD_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_vcard_KEYWORD_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_vcard_KEYWORD_els([_ | _els], Cdata) ->
    decode_vcard_KEYWORD_els(_els, Cdata).

encode_vcard_KEYWORD(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_KEYWORD_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"KEYWORD">>, _attrs, _els}.

decode_vcard_KEYWORD_cdata(<<>>) -> undefined;
decode_vcard_KEYWORD_cdata(_val) -> _val.

encode_vcard_KEYWORD_cdata(undefined, _acc) -> _acc;
encode_vcard_KEYWORD_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_ROLE({xmlel, <<"ROLE">>, _attrs, _els}) ->
    Cdata = decode_vcard_ROLE_els(_els, <<>>), Cdata.

decode_vcard_ROLE_els([], Cdata) ->
    decode_vcard_ROLE_cdata(Cdata);
decode_vcard_ROLE_els([{xmlcdata, _data} | _els],
		      Cdata) ->
    decode_vcard_ROLE_els(_els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_ROLE_els([_ | _els], Cdata) ->
    decode_vcard_ROLE_els(_els, Cdata).

encode_vcard_ROLE(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_ROLE_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"ROLE">>, _attrs, _els}.

decode_vcard_ROLE_cdata(<<>>) -> undefined;
decode_vcard_ROLE_cdata(_val) -> _val.

encode_vcard_ROLE_cdata(undefined, _acc) -> _acc;
encode_vcard_ROLE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_TITLE({xmlel, <<"TITLE">>, _attrs,
		    _els}) ->
    Cdata = decode_vcard_TITLE_els(_els, <<>>), Cdata.

decode_vcard_TITLE_els([], Cdata) ->
    decode_vcard_TITLE_cdata(Cdata);
decode_vcard_TITLE_els([{xmlcdata, _data} | _els],
		       Cdata) ->
    decode_vcard_TITLE_els(_els,
			   <<Cdata/binary, _data/binary>>);
decode_vcard_TITLE_els([_ | _els], Cdata) ->
    decode_vcard_TITLE_els(_els, Cdata).

encode_vcard_TITLE(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_TITLE_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"TITLE">>, _attrs, _els}.

decode_vcard_TITLE_cdata(<<>>) -> undefined;
decode_vcard_TITLE_cdata(_val) -> _val.

encode_vcard_TITLE_cdata(undefined, _acc) -> _acc;
encode_vcard_TITLE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_TZ({xmlel, <<"TZ">>, _attrs, _els}) ->
    Cdata = decode_vcard_TZ_els(_els, <<>>), Cdata.

decode_vcard_TZ_els([], Cdata) ->
    decode_vcard_TZ_cdata(Cdata);
decode_vcard_TZ_els([{xmlcdata, _data} | _els],
		    Cdata) ->
    decode_vcard_TZ_els(_els,
			<<Cdata/binary, _data/binary>>);
decode_vcard_TZ_els([_ | _els], Cdata) ->
    decode_vcard_TZ_els(_els, Cdata).

encode_vcard_TZ(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_TZ_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"TZ">>, _attrs, _els}.

decode_vcard_TZ_cdata(<<>>) -> undefined;
decode_vcard_TZ_cdata(_val) -> _val.

encode_vcard_TZ_cdata(undefined, _acc) -> _acc;
encode_vcard_TZ_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_MAILER({xmlel, <<"MAILER">>, _attrs,
		     _els}) ->
    Cdata = decode_vcard_MAILER_els(_els, <<>>), Cdata.

decode_vcard_MAILER_els([], Cdata) ->
    decode_vcard_MAILER_cdata(Cdata);
decode_vcard_MAILER_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_vcard_MAILER_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_MAILER_els([_ | _els], Cdata) ->
    decode_vcard_MAILER_els(_els, Cdata).

encode_vcard_MAILER(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_MAILER_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"MAILER">>, _attrs, _els}.

decode_vcard_MAILER_cdata(<<>>) -> undefined;
decode_vcard_MAILER_cdata(_val) -> _val.

encode_vcard_MAILER_cdata(undefined, _acc) -> _acc;
encode_vcard_MAILER_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_JABBERID({xmlel, <<"JABBERID">>, _attrs,
		       _els}) ->
    Cdata = decode_vcard_JABBERID_els(_els, <<>>), Cdata.

decode_vcard_JABBERID_els([], Cdata) ->
    decode_vcard_JABBERID_cdata(Cdata);
decode_vcard_JABBERID_els([{xmlcdata, _data} | _els],
			  Cdata) ->
    decode_vcard_JABBERID_els(_els,
			      <<Cdata/binary, _data/binary>>);
decode_vcard_JABBERID_els([_ | _els], Cdata) ->
    decode_vcard_JABBERID_els(_els, Cdata).

encode_vcard_JABBERID(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_JABBERID_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"JABBERID">>, _attrs, _els}.

decode_vcard_JABBERID_cdata(<<>>) -> undefined;
decode_vcard_JABBERID_cdata(_val) -> _val.

encode_vcard_JABBERID_cdata(undefined, _acc) -> _acc;
encode_vcard_JABBERID_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_BDAY({xmlel, <<"BDAY">>, _attrs, _els}) ->
    Cdata = decode_vcard_BDAY_els(_els, <<>>), Cdata.

decode_vcard_BDAY_els([], Cdata) ->
    decode_vcard_BDAY_cdata(Cdata);
decode_vcard_BDAY_els([{xmlcdata, _data} | _els],
		      Cdata) ->
    decode_vcard_BDAY_els(_els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_BDAY_els([_ | _els], Cdata) ->
    decode_vcard_BDAY_els(_els, Cdata).

encode_vcard_BDAY(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_BDAY_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"BDAY">>, _attrs, _els}.

decode_vcard_BDAY_cdata(<<>>) -> undefined;
decode_vcard_BDAY_cdata(_val) -> _val.

encode_vcard_BDAY_cdata(undefined, _acc) -> _acc;
encode_vcard_BDAY_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_NICKNAME({xmlel, <<"NICKNAME">>, _attrs,
		       _els}) ->
    Cdata = decode_vcard_NICKNAME_els(_els, <<>>), Cdata.

decode_vcard_NICKNAME_els([], Cdata) ->
    decode_vcard_NICKNAME_cdata(Cdata);
decode_vcard_NICKNAME_els([{xmlcdata, _data} | _els],
			  Cdata) ->
    decode_vcard_NICKNAME_els(_els,
			      <<Cdata/binary, _data/binary>>);
decode_vcard_NICKNAME_els([_ | _els], Cdata) ->
    decode_vcard_NICKNAME_els(_els, Cdata).

encode_vcard_NICKNAME(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_NICKNAME_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"NICKNAME">>, _attrs, _els}.

decode_vcard_NICKNAME_cdata(<<>>) -> undefined;
decode_vcard_NICKNAME_cdata(_val) -> _val.

encode_vcard_NICKNAME_cdata(undefined, _acc) -> _acc;
encode_vcard_NICKNAME_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_FN({xmlel, <<"FN">>, _attrs, _els}) ->
    Cdata = decode_vcard_FN_els(_els, <<>>), Cdata.

decode_vcard_FN_els([], Cdata) ->
    decode_vcard_FN_cdata(Cdata);
decode_vcard_FN_els([{xmlcdata, _data} | _els],
		    Cdata) ->
    decode_vcard_FN_els(_els,
			<<Cdata/binary, _data/binary>>);
decode_vcard_FN_els([_ | _els], Cdata) ->
    decode_vcard_FN_els(_els, Cdata).

encode_vcard_FN(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_FN_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"FN">>, _attrs, _els}.

decode_vcard_FN_cdata(<<>>) -> undefined;
decode_vcard_FN_cdata(_val) -> _val.

encode_vcard_FN_cdata(undefined, _acc) -> _acc;
encode_vcard_FN_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_VERSION({xmlel, <<"VERSION">>, _attrs,
		      _els}) ->
    Cdata = decode_vcard_VERSION_els(_els, <<>>), Cdata.

decode_vcard_VERSION_els([], Cdata) ->
    decode_vcard_VERSION_cdata(Cdata);
decode_vcard_VERSION_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_vcard_VERSION_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_vcard_VERSION_els([_ | _els], Cdata) ->
    decode_vcard_VERSION_els(_els, Cdata).

encode_vcard_VERSION(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_VERSION_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"VERSION">>, _attrs, _els}.

decode_vcard_VERSION_cdata(<<>>) -> undefined;
decode_vcard_VERSION_cdata(_val) -> _val.

encode_vcard_VERSION_cdata(undefined, _acc) -> _acc;
encode_vcard_VERSION_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_CRED({xmlel, <<"CRED">>, _attrs, _els}) ->
    Cdata = decode_vcard_CRED_els(_els, <<>>), Cdata.

decode_vcard_CRED_els([], Cdata) ->
    decode_vcard_CRED_cdata(Cdata);
decode_vcard_CRED_els([{xmlcdata, _data} | _els],
		      Cdata) ->
    decode_vcard_CRED_els(_els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_CRED_els([_ | _els], Cdata) ->
    decode_vcard_CRED_els(_els, Cdata).

encode_vcard_CRED(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_CRED_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"CRED">>, _attrs, _els}.

decode_vcard_CRED_cdata(<<>>) -> undefined;
decode_vcard_CRED_cdata(_val) -> _val.

encode_vcard_CRED_cdata(undefined, _acc) -> _acc;
encode_vcard_CRED_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_PHONETIC({xmlel, <<"PHONETIC">>, _attrs,
		       _els}) ->
    Cdata = decode_vcard_PHONETIC_els(_els, <<>>), Cdata.

decode_vcard_PHONETIC_els([], Cdata) ->
    decode_vcard_PHONETIC_cdata(Cdata);
decode_vcard_PHONETIC_els([{xmlcdata, _data} | _els],
			  Cdata) ->
    decode_vcard_PHONETIC_els(_els,
			      <<Cdata/binary, _data/binary>>);
decode_vcard_PHONETIC_els([_ | _els], Cdata) ->
    decode_vcard_PHONETIC_els(_els, Cdata).

encode_vcard_PHONETIC(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_PHONETIC_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"PHONETIC">>, _attrs, _els}.

decode_vcard_PHONETIC_cdata(<<>>) -> undefined;
decode_vcard_PHONETIC_cdata(_val) -> _val.

encode_vcard_PHONETIC_cdata(undefined, _acc) -> _acc;
encode_vcard_PHONETIC_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_ORGUNIT({xmlel, <<"ORGUNIT">>, _attrs,
		      _els}) ->
    Cdata = decode_vcard_ORGUNIT_els(_els, <<>>), Cdata.

decode_vcard_ORGUNIT_els([], Cdata) ->
    decode_vcard_ORGUNIT_cdata(Cdata);
decode_vcard_ORGUNIT_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_vcard_ORGUNIT_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_vcard_ORGUNIT_els([_ | _els], Cdata) ->
    decode_vcard_ORGUNIT_els(_els, Cdata).

encode_vcard_ORGUNIT(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_ORGUNIT_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"ORGUNIT">>, _attrs, _els}.

decode_vcard_ORGUNIT_cdata(<<>>) -> undefined;
decode_vcard_ORGUNIT_cdata(_val) -> _val.

encode_vcard_ORGUNIT_cdata(undefined, _acc) -> _acc;
encode_vcard_ORGUNIT_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_ORGNAME({xmlel, <<"ORGNAME">>, _attrs,
		      _els}) ->
    Cdata = decode_vcard_ORGNAME_els(_els, <<>>), Cdata.

decode_vcard_ORGNAME_els([], Cdata) ->
    decode_vcard_ORGNAME_cdata(Cdata);
decode_vcard_ORGNAME_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_vcard_ORGNAME_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_vcard_ORGNAME_els([_ | _els], Cdata) ->
    decode_vcard_ORGNAME_els(_els, Cdata).

encode_vcard_ORGNAME(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_ORGNAME_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"ORGNAME">>, _attrs, _els}.

decode_vcard_ORGNAME_cdata(<<>>) -> undefined;
decode_vcard_ORGNAME_cdata(_val) -> _val.

encode_vcard_ORGNAME_cdata(undefined, _acc) -> _acc;
encode_vcard_ORGNAME_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_LON({xmlel, <<"LON">>, _attrs, _els}) ->
    Cdata = decode_vcard_LON_els(_els, <<>>), Cdata.

decode_vcard_LON_els([], Cdata) ->
    decode_vcard_LON_cdata(Cdata);
decode_vcard_LON_els([{xmlcdata, _data} | _els],
		     Cdata) ->
    decode_vcard_LON_els(_els,
			 <<Cdata/binary, _data/binary>>);
decode_vcard_LON_els([_ | _els], Cdata) ->
    decode_vcard_LON_els(_els, Cdata).

encode_vcard_LON(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_LON_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"LON">>, _attrs, _els}.

decode_vcard_LON_cdata(<<>>) -> undefined;
decode_vcard_LON_cdata(_val) -> _val.

encode_vcard_LON_cdata(undefined, _acc) -> _acc;
encode_vcard_LON_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_LAT({xmlel, <<"LAT">>, _attrs, _els}) ->
    Cdata = decode_vcard_LAT_els(_els, <<>>), Cdata.

decode_vcard_LAT_els([], Cdata) ->
    decode_vcard_LAT_cdata(Cdata);
decode_vcard_LAT_els([{xmlcdata, _data} | _els],
		     Cdata) ->
    decode_vcard_LAT_els(_els,
			 <<Cdata/binary, _data/binary>>);
decode_vcard_LAT_els([_ | _els], Cdata) ->
    decode_vcard_LAT_els(_els, Cdata).

encode_vcard_LAT(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_LAT_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"LAT">>, _attrs, _els}.

decode_vcard_LAT_cdata(<<>>) -> undefined;
decode_vcard_LAT_cdata(_val) -> _val.

encode_vcard_LAT_cdata(undefined, _acc) -> _acc;
encode_vcard_LAT_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_USERID({xmlel, <<"USERID">>, _attrs,
		     _els}) ->
    Cdata = decode_vcard_USERID_els(_els, <<>>), Cdata.

decode_vcard_USERID_els([], Cdata) ->
    decode_vcard_USERID_cdata(Cdata);
decode_vcard_USERID_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_vcard_USERID_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_USERID_els([_ | _els], Cdata) ->
    decode_vcard_USERID_els(_els, Cdata).

encode_vcard_USERID(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_USERID_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"USERID">>, _attrs, _els}.

decode_vcard_USERID_cdata(<<>>) -> undefined;
decode_vcard_USERID_cdata(_val) -> _val.

encode_vcard_USERID_cdata(undefined, _acc) -> _acc;
encode_vcard_USERID_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_NUMBER({xmlel, <<"NUMBER">>, _attrs,
		     _els}) ->
    Cdata = decode_vcard_NUMBER_els(_els, <<>>), Cdata.

decode_vcard_NUMBER_els([], Cdata) ->
    decode_vcard_NUMBER_cdata(Cdata);
decode_vcard_NUMBER_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_vcard_NUMBER_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_NUMBER_els([_ | _els], Cdata) ->
    decode_vcard_NUMBER_els(_els, Cdata).

encode_vcard_NUMBER(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_NUMBER_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"NUMBER">>, _attrs, _els}.

decode_vcard_NUMBER_cdata(<<>>) -> undefined;
decode_vcard_NUMBER_cdata(_val) -> _val.

encode_vcard_NUMBER_cdata(undefined, _acc) -> _acc;
encode_vcard_NUMBER_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_LINE({xmlel, <<"LINE">>, _attrs, _els}) ->
    Cdata = decode_vcard_LINE_els(_els, <<>>), Cdata.

decode_vcard_LINE_els([], Cdata) ->
    decode_vcard_LINE_cdata(Cdata);
decode_vcard_LINE_els([{xmlcdata, _data} | _els],
		      Cdata) ->
    decode_vcard_LINE_els(_els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_LINE_els([_ | _els], Cdata) ->
    decode_vcard_LINE_els(_els, Cdata).

encode_vcard_LINE(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_LINE_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"LINE">>, _attrs, _els}.

decode_vcard_LINE_cdata(<<>>) -> undefined;
decode_vcard_LINE_cdata(_val) -> _val.

encode_vcard_LINE_cdata(undefined, _acc) -> _acc;
encode_vcard_LINE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_CTRY({xmlel, <<"CTRY">>, _attrs, _els}) ->
    Cdata = decode_vcard_CTRY_els(_els, <<>>), Cdata.

decode_vcard_CTRY_els([], Cdata) ->
    decode_vcard_CTRY_cdata(Cdata);
decode_vcard_CTRY_els([{xmlcdata, _data} | _els],
		      Cdata) ->
    decode_vcard_CTRY_els(_els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_CTRY_els([_ | _els], Cdata) ->
    decode_vcard_CTRY_els(_els, Cdata).

encode_vcard_CTRY(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_CTRY_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"CTRY">>, _attrs, _els}.

decode_vcard_CTRY_cdata(<<>>) -> undefined;
decode_vcard_CTRY_cdata(_val) -> _val.

encode_vcard_CTRY_cdata(undefined, _acc) -> _acc;
encode_vcard_CTRY_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_PCODE({xmlel, <<"PCODE">>, _attrs,
		    _els}) ->
    Cdata = decode_vcard_PCODE_els(_els, <<>>), Cdata.

decode_vcard_PCODE_els([], Cdata) ->
    decode_vcard_PCODE_cdata(Cdata);
decode_vcard_PCODE_els([{xmlcdata, _data} | _els],
		       Cdata) ->
    decode_vcard_PCODE_els(_els,
			   <<Cdata/binary, _data/binary>>);
decode_vcard_PCODE_els([_ | _els], Cdata) ->
    decode_vcard_PCODE_els(_els, Cdata).

encode_vcard_PCODE(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_PCODE_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"PCODE">>, _attrs, _els}.

decode_vcard_PCODE_cdata(<<>>) -> undefined;
decode_vcard_PCODE_cdata(_val) -> _val.

encode_vcard_PCODE_cdata(undefined, _acc) -> _acc;
encode_vcard_PCODE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_REGION({xmlel, <<"REGION">>, _attrs,
		     _els}) ->
    Cdata = decode_vcard_REGION_els(_els, <<>>), Cdata.

decode_vcard_REGION_els([], Cdata) ->
    decode_vcard_REGION_cdata(Cdata);
decode_vcard_REGION_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_vcard_REGION_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_REGION_els([_ | _els], Cdata) ->
    decode_vcard_REGION_els(_els, Cdata).

encode_vcard_REGION(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_REGION_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"REGION">>, _attrs, _els}.

decode_vcard_REGION_cdata(<<>>) -> undefined;
decode_vcard_REGION_cdata(_val) -> _val.

encode_vcard_REGION_cdata(undefined, _acc) -> _acc;
encode_vcard_REGION_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_LOCALITY({xmlel, <<"LOCALITY">>, _attrs,
		       _els}) ->
    Cdata = decode_vcard_LOCALITY_els(_els, <<>>), Cdata.

decode_vcard_LOCALITY_els([], Cdata) ->
    decode_vcard_LOCALITY_cdata(Cdata);
decode_vcard_LOCALITY_els([{xmlcdata, _data} | _els],
			  Cdata) ->
    decode_vcard_LOCALITY_els(_els,
			      <<Cdata/binary, _data/binary>>);
decode_vcard_LOCALITY_els([_ | _els], Cdata) ->
    decode_vcard_LOCALITY_els(_els, Cdata).

encode_vcard_LOCALITY(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_LOCALITY_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"LOCALITY">>, _attrs, _els}.

decode_vcard_LOCALITY_cdata(<<>>) -> undefined;
decode_vcard_LOCALITY_cdata(_val) -> _val.

encode_vcard_LOCALITY_cdata(undefined, _acc) -> _acc;
encode_vcard_LOCALITY_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_STREET({xmlel, <<"STREET">>, _attrs,
		     _els}) ->
    Cdata = decode_vcard_STREET_els(_els, <<>>), Cdata.

decode_vcard_STREET_els([], Cdata) ->
    decode_vcard_STREET_cdata(Cdata);
decode_vcard_STREET_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_vcard_STREET_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_STREET_els([_ | _els], Cdata) ->
    decode_vcard_STREET_els(_els, Cdata).

encode_vcard_STREET(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_STREET_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"STREET">>, _attrs, _els}.

decode_vcard_STREET_cdata(<<>>) -> undefined;
decode_vcard_STREET_cdata(_val) -> _val.

encode_vcard_STREET_cdata(undefined, _acc) -> _acc;
encode_vcard_STREET_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_EXTADD({xmlel, <<"EXTADD">>, _attrs,
		     _els}) ->
    Cdata = decode_vcard_EXTADD_els(_els, <<>>), Cdata.

decode_vcard_EXTADD_els([], Cdata) ->
    decode_vcard_EXTADD_cdata(Cdata);
decode_vcard_EXTADD_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_vcard_EXTADD_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_EXTADD_els([_ | _els], Cdata) ->
    decode_vcard_EXTADD_els(_els, Cdata).

encode_vcard_EXTADD(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_EXTADD_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"EXTADD">>, _attrs, _els}.

decode_vcard_EXTADD_cdata(<<>>) -> undefined;
decode_vcard_EXTADD_cdata(_val) -> _val.

encode_vcard_EXTADD_cdata(undefined, _acc) -> _acc;
encode_vcard_EXTADD_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_POBOX({xmlel, <<"POBOX">>, _attrs,
		    _els}) ->
    Cdata = decode_vcard_POBOX_els(_els, <<>>), Cdata.

decode_vcard_POBOX_els([], Cdata) ->
    decode_vcard_POBOX_cdata(Cdata);
decode_vcard_POBOX_els([{xmlcdata, _data} | _els],
		       Cdata) ->
    decode_vcard_POBOX_els(_els,
			   <<Cdata/binary, _data/binary>>);
decode_vcard_POBOX_els([_ | _els], Cdata) ->
    decode_vcard_POBOX_els(_els, Cdata).

encode_vcard_POBOX(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_POBOX_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"POBOX">>, _attrs, _els}.

decode_vcard_POBOX_cdata(<<>>) -> undefined;
decode_vcard_POBOX_cdata(_val) -> _val.

encode_vcard_POBOX_cdata(undefined, _acc) -> _acc;
encode_vcard_POBOX_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_SUFFIX({xmlel, <<"SUFFIX">>, _attrs,
		     _els}) ->
    Cdata = decode_vcard_SUFFIX_els(_els, <<>>), Cdata.

decode_vcard_SUFFIX_els([], Cdata) ->
    decode_vcard_SUFFIX_cdata(Cdata);
decode_vcard_SUFFIX_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_vcard_SUFFIX_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_SUFFIX_els([_ | _els], Cdata) ->
    decode_vcard_SUFFIX_els(_els, Cdata).

encode_vcard_SUFFIX(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_SUFFIX_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"SUFFIX">>, _attrs, _els}.

decode_vcard_SUFFIX_cdata(<<>>) -> undefined;
decode_vcard_SUFFIX_cdata(_val) -> _val.

encode_vcard_SUFFIX_cdata(undefined, _acc) -> _acc;
encode_vcard_SUFFIX_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_PREFIX({xmlel, <<"PREFIX">>, _attrs,
		     _els}) ->
    Cdata = decode_vcard_PREFIX_els(_els, <<>>), Cdata.

decode_vcard_PREFIX_els([], Cdata) ->
    decode_vcard_PREFIX_cdata(Cdata);
decode_vcard_PREFIX_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_vcard_PREFIX_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_PREFIX_els([_ | _els], Cdata) ->
    decode_vcard_PREFIX_els(_els, Cdata).

encode_vcard_PREFIX(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_PREFIX_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"PREFIX">>, _attrs, _els}.

decode_vcard_PREFIX_cdata(<<>>) -> undefined;
decode_vcard_PREFIX_cdata(_val) -> _val.

encode_vcard_PREFIX_cdata(undefined, _acc) -> _acc;
encode_vcard_PREFIX_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_MIDDLE({xmlel, <<"MIDDLE">>, _attrs,
		     _els}) ->
    Cdata = decode_vcard_MIDDLE_els(_els, <<>>), Cdata.

decode_vcard_MIDDLE_els([], Cdata) ->
    decode_vcard_MIDDLE_cdata(Cdata);
decode_vcard_MIDDLE_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_vcard_MIDDLE_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_MIDDLE_els([_ | _els], Cdata) ->
    decode_vcard_MIDDLE_els(_els, Cdata).

encode_vcard_MIDDLE(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_MIDDLE_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"MIDDLE">>, _attrs, _els}.

decode_vcard_MIDDLE_cdata(<<>>) -> undefined;
decode_vcard_MIDDLE_cdata(_val) -> _val.

encode_vcard_MIDDLE_cdata(undefined, _acc) -> _acc;
encode_vcard_MIDDLE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_GIVEN({xmlel, <<"GIVEN">>, _attrs,
		    _els}) ->
    Cdata = decode_vcard_GIVEN_els(_els, <<>>), Cdata.

decode_vcard_GIVEN_els([], Cdata) ->
    decode_vcard_GIVEN_cdata(Cdata);
decode_vcard_GIVEN_els([{xmlcdata, _data} | _els],
		       Cdata) ->
    decode_vcard_GIVEN_els(_els,
			   <<Cdata/binary, _data/binary>>);
decode_vcard_GIVEN_els([_ | _els], Cdata) ->
    decode_vcard_GIVEN_els(_els, Cdata).

encode_vcard_GIVEN(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_GIVEN_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"GIVEN">>, _attrs, _els}.

decode_vcard_GIVEN_cdata(<<>>) -> undefined;
decode_vcard_GIVEN_cdata(_val) -> _val.

encode_vcard_GIVEN_cdata(undefined, _acc) -> _acc;
encode_vcard_GIVEN_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_FAMILY({xmlel, <<"FAMILY">>, _attrs,
		     _els}) ->
    Cdata = decode_vcard_FAMILY_els(_els, <<>>), Cdata.

decode_vcard_FAMILY_els([], Cdata) ->
    decode_vcard_FAMILY_cdata(Cdata);
decode_vcard_FAMILY_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_vcard_FAMILY_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_FAMILY_els([_ | _els], Cdata) ->
    decode_vcard_FAMILY_els(_els, Cdata).

encode_vcard_FAMILY(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_FAMILY_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"FAMILY">>, _attrs, _els}.

decode_vcard_FAMILY_cdata(<<>>) -> undefined;
decode_vcard_FAMILY_cdata(_val) -> _val.

encode_vcard_FAMILY_cdata(undefined, _acc) -> _acc;
encode_vcard_FAMILY_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_X400({xmlel, <<"X400">>, _attrs, _els}) ->
    true.

encode_vcard_X400(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"X400">>, _attrs, _els}.

decode_vcard_INTERNET({xmlel, <<"INTERNET">>, _attrs,
		       _els}) ->
    true.

encode_vcard_INTERNET(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"INTERNET">>, _attrs, _els}.

decode_vcard_PREF({xmlel, <<"PREF">>, _attrs, _els}) ->
    true.

encode_vcard_PREF(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"PREF">>, _attrs, _els}.

decode_vcard_INTL({xmlel, <<"INTL">>, _attrs, _els}) ->
    true.

encode_vcard_INTL(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"INTL">>, _attrs, _els}.

decode_vcard_DOM({xmlel, <<"DOM">>, _attrs, _els}) ->
    true.

encode_vcard_DOM(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"DOM">>, _attrs, _els}.

decode_vcard_PARCEL({xmlel, <<"PARCEL">>, _attrs,
		     _els}) ->
    true.

encode_vcard_PARCEL(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"PARCEL">>, _attrs, _els}.

decode_vcard_POSTAL({xmlel, <<"POSTAL">>, _attrs,
		     _els}) ->
    true.

encode_vcard_POSTAL(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"POSTAL">>, _attrs, _els}.

decode_vcard_PCS({xmlel, <<"PCS">>, _attrs, _els}) ->
    true.

encode_vcard_PCS(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"PCS">>, _attrs, _els}.

decode_vcard_ISDN({xmlel, <<"ISDN">>, _attrs, _els}) ->
    true.

encode_vcard_ISDN(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"ISDN">>, _attrs, _els}.

decode_vcard_MODEM({xmlel, <<"MODEM">>, _attrs,
		    _els}) ->
    true.

encode_vcard_MODEM(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"MODEM">>, _attrs, _els}.

decode_vcard_BBS({xmlel, <<"BBS">>, _attrs, _els}) ->
    true.

encode_vcard_BBS(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"BBS">>, _attrs, _els}.

decode_vcard_VIDEO({xmlel, <<"VIDEO">>, _attrs,
		    _els}) ->
    true.

encode_vcard_VIDEO(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"VIDEO">>, _attrs, _els}.

decode_vcard_CELL({xmlel, <<"CELL">>, _attrs, _els}) ->
    true.

encode_vcard_CELL(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"CELL">>, _attrs, _els}.

decode_vcard_MSG({xmlel, <<"MSG">>, _attrs, _els}) ->
    true.

encode_vcard_MSG(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"MSG">>, _attrs, _els}.

decode_vcard_PAGER({xmlel, <<"PAGER">>, _attrs,
		    _els}) ->
    true.

encode_vcard_PAGER(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"PAGER">>, _attrs, _els}.

decode_vcard_FAX({xmlel, <<"FAX">>, _attrs, _els}) ->
    true.

encode_vcard_FAX(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"FAX">>, _attrs, _els}.

decode_vcard_VOICE({xmlel, <<"VOICE">>, _attrs,
		    _els}) ->
    true.

encode_vcard_VOICE(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"VOICE">>, _attrs, _els}.

decode_vcard_WORK({xmlel, <<"WORK">>, _attrs, _els}) ->
    true.

encode_vcard_WORK(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"WORK">>, _attrs, _els}.

decode_vcard_HOME({xmlel, <<"HOME">>, _attrs, _els}) ->
    true.

encode_vcard_HOME(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"HOME">>, _attrs, _els}.

decode_stream_error({xmlel, <<"stream:error">>, _attrs,
		     _els}) ->
    {Text, Reason} = decode_stream_error_els(_els,
					     undefined, undefined),
    {stream_error, Reason, Text}.

decode_stream_error_els([], Text, Reason) ->
    {Text, Reason};
decode_stream_error_els([{xmlel, <<"text">>, _attrs,
			  _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els,
				   decode_stream_error_text(_el), Reason);
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel, <<"bad-format">>,
			  _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_bad_format(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel,
			  <<"bad-namespace-prefix">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_bad_namespace_prefix(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel, <<"conflict">>, _attrs,
			  _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_conflict(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel,
			  <<"connection-timeout">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_connection_timeout(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel, <<"host-gone">>,
			  _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_host_gone(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel, <<"host-unknown">>,
			  _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_host_unknown(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel,
			  <<"improper-addressing">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_improper_addressing(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel,
			  <<"internal-server-error">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_internal_server_error(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel, <<"invalid-from">>,
			  _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_invalid_from(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel, <<"invalid-id">>,
			  _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_invalid_id(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel,
			  <<"invalid-namespace">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_invalid_namespace(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel, <<"invalid-xml">>,
			  _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_invalid_xml(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel, <<"not-authorized">>,
			  _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_not_authorized(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel, <<"not-well-formed">>,
			  _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_not_well_formed(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel, <<"policy-violation">>,
			  _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_policy_violation(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel,
			  <<"remote-connection-failed">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_remote_connection_failed(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel, <<"reset">>, _attrs,
			  _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_reset(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel,
			  <<"resource-constraint">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_resource_constraint(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel, <<"restricted-xml">>,
			  _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_restricted_xml(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel, <<"see-other-host">>,
			  _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_see_other_host(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel, <<"system-shutdown">>,
			  _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_system_shutdown(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel,
			  <<"undefined-condition">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_undefined_condition(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel,
			  <<"unsupported-encoding">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_unsupported_encoding(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel,
			  <<"unsupported-stanza-type">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_unsupported_stanza_type(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([{xmlel,
			  <<"unsupported-version">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(_els, Text,
				   decode_stream_error_unsupported_version(_el));
       true -> decode_stream_error_els(_els, Text, Reason)
    end;
decode_stream_error_els([_ | _els], Text, Reason) ->
    decode_stream_error_els(_els, Text, Reason).

encode_stream_error({stream_error, Reason, Text},
		    _xmlns_attrs) ->
    _els = 'encode_stream_error_$reason'(Reason,
					 'encode_stream_error_$text'(Text, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"stream:error">>, _attrs, _els}.

'encode_stream_error_$text'(undefined, _acc) -> _acc;
'encode_stream_error_$text'(Text, _acc) ->
    [encode_stream_error_text(Text,
			      [{<<"xmlns">>,
				<<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc].

'encode_stream_error_$reason'(undefined, _acc) -> _acc;
'encode_stream_error_$reason'('bad-format' = Reason,
			      _acc) ->
    [encode_stream_error_bad_format(Reason,
				    [{<<"xmlns">>,
				      <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('bad-namespace-prefix' =
				  Reason,
			      _acc) ->
    [encode_stream_error_bad_namespace_prefix(Reason,
					      [{<<"xmlns">>,
						<<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'(conflict = Reason,
			      _acc) ->
    [encode_stream_error_conflict(Reason,
				  [{<<"xmlns">>,
				    <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('connection-timeout' =
				  Reason,
			      _acc) ->
    [encode_stream_error_connection_timeout(Reason,
					    [{<<"xmlns">>,
					      <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('host-gone' = Reason,
			      _acc) ->
    [encode_stream_error_host_gone(Reason,
				   [{<<"xmlns">>,
				     <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('host-unknown' = Reason,
			      _acc) ->
    [encode_stream_error_host_unknown(Reason,
				      [{<<"xmlns">>,
					<<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('improper-addressing' =
				  Reason,
			      _acc) ->
    [encode_stream_error_improper_addressing(Reason,
					     [{<<"xmlns">>,
					       <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('internal-server-error' =
				  Reason,
			      _acc) ->
    [encode_stream_error_internal_server_error(Reason,
					       [{<<"xmlns">>,
						 <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('invalid-from' = Reason,
			      _acc) ->
    [encode_stream_error_invalid_from(Reason,
				      [{<<"xmlns">>,
					<<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('invalid-id' = Reason,
			      _acc) ->
    [encode_stream_error_invalid_id(Reason,
				    [{<<"xmlns">>,
				      <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('invalid-namespace' =
				  Reason,
			      _acc) ->
    [encode_stream_error_invalid_namespace(Reason,
					   [{<<"xmlns">>,
					     <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('invalid-xml' = Reason,
			      _acc) ->
    [encode_stream_error_invalid_xml(Reason,
				     [{<<"xmlns">>,
				       <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('not-authorized' = Reason,
			      _acc) ->
    [encode_stream_error_not_authorized(Reason,
					[{<<"xmlns">>,
					  <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('not-well-formed' =
				  Reason,
			      _acc) ->
    [encode_stream_error_not_well_formed(Reason,
					 [{<<"xmlns">>,
					   <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('policy-violation' =
				  Reason,
			      _acc) ->
    [encode_stream_error_policy_violation(Reason,
					  [{<<"xmlns">>,
					    <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('remote-connection-failed' =
				  Reason,
			      _acc) ->
    [encode_stream_error_remote_connection_failed(Reason,
						  [{<<"xmlns">>,
						    <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'(reset = Reason, _acc) ->
    [encode_stream_error_reset(Reason,
			       [{<<"xmlns">>,
				 <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('resource-constraint' =
				  Reason,
			      _acc) ->
    [encode_stream_error_resource_constraint(Reason,
					     [{<<"xmlns">>,
					       <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('restricted-xml' = Reason,
			      _acc) ->
    [encode_stream_error_restricted_xml(Reason,
					[{<<"xmlns">>,
					  <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'({'see-other-host', _} =
				  Reason,
			      _acc) ->
    [encode_stream_error_see_other_host(Reason,
					[{<<"xmlns">>,
					  <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('system-shutdown' =
				  Reason,
			      _acc) ->
    [encode_stream_error_system_shutdown(Reason,
					 [{<<"xmlns">>,
					   <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('undefined-condition' =
				  Reason,
			      _acc) ->
    [encode_stream_error_undefined_condition(Reason,
					     [{<<"xmlns">>,
					       <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('unsupported-encoding' =
				  Reason,
			      _acc) ->
    [encode_stream_error_unsupported_encoding(Reason,
					      [{<<"xmlns">>,
						<<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('unsupported-stanza-type' =
				  Reason,
			      _acc) ->
    [encode_stream_error_unsupported_stanza_type(Reason,
						 [{<<"xmlns">>,
						   <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc];
'encode_stream_error_$reason'('unsupported-version' =
				  Reason,
			      _acc) ->
    [encode_stream_error_unsupported_version(Reason,
					     [{<<"xmlns">>,
					       <<"urn:ietf:params:xml:ns:xmpp-streams">>}])
     | _acc].

decode_stream_error_unsupported_version({xmlel,
					 <<"unsupported-version">>, _attrs,
					 _els}) ->
    'unsupported-version'.

encode_stream_error_unsupported_version('unsupported-version',
					_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"unsupported-version">>, _attrs, _els}.

decode_stream_error_unsupported_stanza_type({xmlel,
					     <<"unsupported-stanza-type">>,
					     _attrs, _els}) ->
    'unsupported-stanza-type'.

encode_stream_error_unsupported_stanza_type('unsupported-stanza-type',
					    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"unsupported-stanza-type">>, _attrs, _els}.

decode_stream_error_unsupported_encoding({xmlel,
					  <<"unsupported-encoding">>, _attrs,
					  _els}) ->
    'unsupported-encoding'.

encode_stream_error_unsupported_encoding('unsupported-encoding',
					 _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"unsupported-encoding">>, _attrs, _els}.

decode_stream_error_undefined_condition({xmlel,
					 <<"undefined-condition">>, _attrs,
					 _els}) ->
    'undefined-condition'.

encode_stream_error_undefined_condition('undefined-condition',
					_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"undefined-condition">>, _attrs, _els}.

decode_stream_error_system_shutdown({xmlel,
				     <<"system-shutdown">>, _attrs, _els}) ->
    'system-shutdown'.

encode_stream_error_system_shutdown('system-shutdown',
				    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"system-shutdown">>, _attrs, _els}.

decode_stream_error_see_other_host({xmlel,
				    <<"see-other-host">>, _attrs, _els}) ->
    Host = decode_stream_error_see_other_host_els(_els,
						  <<>>),
    {'see-other-host', Host}.

decode_stream_error_see_other_host_els([], Host) ->
    decode_stream_error_see_other_host_cdata(Host);
decode_stream_error_see_other_host_els([{xmlcdata,
					 _data}
					| _els],
				       Host) ->
    decode_stream_error_see_other_host_els(_els,
					   <<Host/binary, _data/binary>>);
decode_stream_error_see_other_host_els([_ | _els],
				       Host) ->
    decode_stream_error_see_other_host_els(_els, Host).

encode_stream_error_see_other_host({'see-other-host',
				    Host},
				   _xmlns_attrs) ->
    _els = encode_stream_error_see_other_host_cdata(Host,
						    []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"see-other-host">>, _attrs, _els}.

decode_stream_error_see_other_host_cdata(<<>>) ->
    erlang:error({xmpp_codec,
		  {missing_cdata, <<>>, <<"see-other-host">>,
		   <<"urn:ietf:params:xml:ns:xmpp-streams">>}});
decode_stream_error_see_other_host_cdata(_val) -> _val.

encode_stream_error_see_other_host_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_stream_error_restricted_xml({xmlel,
				    <<"restricted-xml">>, _attrs, _els}) ->
    'restricted-xml'.

encode_stream_error_restricted_xml('restricted-xml',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"restricted-xml">>, _attrs, _els}.

decode_stream_error_resource_constraint({xmlel,
					 <<"resource-constraint">>, _attrs,
					 _els}) ->
    'resource-constraint'.

encode_stream_error_resource_constraint('resource-constraint',
					_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"resource-constraint">>, _attrs, _els}.

decode_stream_error_reset({xmlel, <<"reset">>, _attrs,
			   _els}) ->
    reset.

encode_stream_error_reset(reset, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"reset">>, _attrs, _els}.

decode_stream_error_remote_connection_failed({xmlel,
					      <<"remote-connection-failed">>,
					      _attrs, _els}) ->
    'remote-connection-failed'.

encode_stream_error_remote_connection_failed('remote-connection-failed',
					     _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"remote-connection-failed">>, _attrs, _els}.

decode_stream_error_policy_violation({xmlel,
				      <<"policy-violation">>, _attrs, _els}) ->
    'policy-violation'.

encode_stream_error_policy_violation('policy-violation',
				     _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"policy-violation">>, _attrs, _els}.

decode_stream_error_not_well_formed({xmlel,
				     <<"not-well-formed">>, _attrs, _els}) ->
    'not-well-formed'.

encode_stream_error_not_well_formed('not-well-formed',
				    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"not-well-formed">>, _attrs, _els}.

decode_stream_error_not_authorized({xmlel,
				    <<"not-authorized">>, _attrs, _els}) ->
    'not-authorized'.

encode_stream_error_not_authorized('not-authorized',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"not-authorized">>, _attrs, _els}.

decode_stream_error_invalid_xml({xmlel,
				 <<"invalid-xml">>, _attrs, _els}) ->
    'invalid-xml'.

encode_stream_error_invalid_xml('invalid-xml',
				_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"invalid-xml">>, _attrs, _els}.

decode_stream_error_invalid_namespace({xmlel,
				       <<"invalid-namespace">>, _attrs,
				       _els}) ->
    'invalid-namespace'.

encode_stream_error_invalid_namespace('invalid-namespace',
				      _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"invalid-namespace">>, _attrs, _els}.

decode_stream_error_invalid_id({xmlel, <<"invalid-id">>,
				_attrs, _els}) ->
    'invalid-id'.

encode_stream_error_invalid_id('invalid-id',
			       _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"invalid-id">>, _attrs, _els}.

decode_stream_error_invalid_from({xmlel,
				  <<"invalid-from">>, _attrs, _els}) ->
    'invalid-from'.

encode_stream_error_invalid_from('invalid-from',
				 _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"invalid-from">>, _attrs, _els}.

decode_stream_error_internal_server_error({xmlel,
					   <<"internal-server-error">>, _attrs,
					   _els}) ->
    'internal-server-error'.

encode_stream_error_internal_server_error('internal-server-error',
					  _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"internal-server-error">>, _attrs, _els}.

decode_stream_error_improper_addressing({xmlel,
					 <<"improper-addressing">>, _attrs,
					 _els}) ->
    'improper-addressing'.

encode_stream_error_improper_addressing('improper-addressing',
					_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"improper-addressing">>, _attrs, _els}.

decode_stream_error_host_unknown({xmlel,
				  <<"host-unknown">>, _attrs, _els}) ->
    'host-unknown'.

encode_stream_error_host_unknown('host-unknown',
				 _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"host-unknown">>, _attrs, _els}.

decode_stream_error_host_gone({xmlel, <<"host-gone">>,
			       _attrs, _els}) ->
    'host-gone'.

encode_stream_error_host_gone('host-gone',
			      _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"host-gone">>, _attrs, _els}.

decode_stream_error_connection_timeout({xmlel,
					<<"connection-timeout">>, _attrs,
					_els}) ->
    'connection-timeout'.

encode_stream_error_connection_timeout('connection-timeout',
				       _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"connection-timeout">>, _attrs, _els}.

decode_stream_error_conflict({xmlel, <<"conflict">>,
			      _attrs, _els}) ->
    conflict.

encode_stream_error_conflict(conflict, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"conflict">>, _attrs, _els}.

decode_stream_error_bad_namespace_prefix({xmlel,
					  <<"bad-namespace-prefix">>, _attrs,
					  _els}) ->
    'bad-namespace-prefix'.

encode_stream_error_bad_namespace_prefix('bad-namespace-prefix',
					 _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"bad-namespace-prefix">>, _attrs, _els}.

decode_stream_error_bad_format({xmlel, <<"bad-format">>,
				_attrs, _els}) ->
    'bad-format'.

encode_stream_error_bad_format('bad-format',
			       _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"bad-format">>, _attrs, _els}.

decode_stream_error_text({xmlel, <<"text">>, _attrs,
			  _els}) ->
    Data = decode_stream_error_text_els(_els, <<>>),
    Lang = decode_stream_error_text_attrs(_attrs,
					  undefined),
    {text, Lang, Data}.

decode_stream_error_text_els([], Data) ->
    decode_stream_error_text_cdata(Data);
decode_stream_error_text_els([{xmlcdata, _data} | _els],
			     Data) ->
    decode_stream_error_text_els(_els,
				 <<Data/binary, _data/binary>>);
decode_stream_error_text_els([_ | _els], Data) ->
    decode_stream_error_text_els(_els, Data).

decode_stream_error_text_attrs([{<<"xml:lang">>, _val}
				| _attrs],
			       _Lang) ->
    decode_stream_error_text_attrs(_attrs, _val);
decode_stream_error_text_attrs([_ | _attrs], Lang) ->
    decode_stream_error_text_attrs(_attrs, Lang);
decode_stream_error_text_attrs([], Lang) ->
    'decode_stream_error_text_attr_xml:lang'(Lang).

encode_stream_error_text({text, Lang, Data},
			 _xmlns_attrs) ->
    _els = encode_stream_error_text_cdata(Data, []),
    _attrs = 'encode_stream_error_text_attr_xml:lang'(Lang,
						      _xmlns_attrs),
    {xmlel, <<"text">>, _attrs, _els}.

'decode_stream_error_text_attr_xml:lang'(undefined) ->
    undefined;
'decode_stream_error_text_attr_xml:lang'(_val) -> _val.

'encode_stream_error_text_attr_xml:lang'(undefined,
					 _acc) ->
    _acc;
'encode_stream_error_text_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_stream_error_text_cdata(<<>>) -> undefined;
decode_stream_error_text_cdata(_val) -> _val.

encode_stream_error_text_cdata(undefined, _acc) -> _acc;
encode_stream_error_text_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_time({xmlel, <<"time">>, _attrs, _els}) ->
    {Utc, Tzo} = decode_time_els(_els, undefined,
				 undefined),
    {time, Tzo, Utc}.

decode_time_els([], Utc, Tzo) -> {Utc, Tzo};
decode_time_els([{xmlel, <<"tzo">>, _attrs, _} = _el
		 | _els],
		Utc, Tzo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"urn:xmpp:time">> ->
	   decode_time_els(_els, Utc, decode_time_tzo(_el));
       true -> decode_time_els(_els, Utc, Tzo)
    end;
decode_time_els([{xmlel, <<"utc">>, _attrs, _} = _el
		 | _els],
		Utc, Tzo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"urn:xmpp:time">> ->
	   decode_time_els(_els, decode_time_utc(_el), Tzo);
       true -> decode_time_els(_els, Utc, Tzo)
    end;
decode_time_els([_ | _els], Utc, Tzo) ->
    decode_time_els(_els, Utc, Tzo).

encode_time({time, Tzo, Utc}, _xmlns_attrs) ->
    _els = 'encode_time_$tzo'(Tzo,
			      'encode_time_$utc'(Utc, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"time">>, _attrs, _els}.

'encode_time_$utc'(undefined, _acc) -> _acc;
'encode_time_$utc'(Utc, _acc) ->
    [encode_time_utc(Utc, []) | _acc].

'encode_time_$tzo'(undefined, _acc) -> _acc;
'encode_time_$tzo'(Tzo, _acc) ->
    [encode_time_tzo(Tzo, []) | _acc].

decode_time_tzo({xmlel, <<"tzo">>, _attrs, _els}) ->
    Cdata = decode_time_tzo_els(_els, <<>>), Cdata.

decode_time_tzo_els([], Cdata) ->
    decode_time_tzo_cdata(Cdata);
decode_time_tzo_els([{xmlcdata, _data} | _els],
		    Cdata) ->
    decode_time_tzo_els(_els,
			<<Cdata/binary, _data/binary>>);
decode_time_tzo_els([_ | _els], Cdata) ->
    decode_time_tzo_els(_els, Cdata).

encode_time_tzo(Cdata, _xmlns_attrs) ->
    _els = encode_time_tzo_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"tzo">>, _attrs, _els}.

decode_time_tzo_cdata(<<>>) -> undefined;
decode_time_tzo_cdata(_val) ->
    case catch dec_tzo(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"tzo">>,
			 <<"urn:xmpp:time">>}});
      _res -> _res
    end.

encode_time_tzo_cdata(undefined, _acc) -> _acc;
encode_time_tzo_cdata(_val, _acc) ->
    [{xmlcdata, enc_tzo(_val)} | _acc].

decode_time_utc({xmlel, <<"utc">>, _attrs, _els}) ->
    Cdata = decode_time_utc_els(_els, <<>>), Cdata.

decode_time_utc_els([], Cdata) ->
    decode_time_utc_cdata(Cdata);
decode_time_utc_els([{xmlcdata, _data} | _els],
		    Cdata) ->
    decode_time_utc_els(_els,
			<<Cdata/binary, _data/binary>>);
decode_time_utc_els([_ | _els], Cdata) ->
    decode_time_utc_els(_els, Cdata).

encode_time_utc(Cdata, _xmlns_attrs) ->
    _els = encode_time_utc_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"utc">>, _attrs, _els}.

decode_time_utc_cdata(<<>>) -> undefined;
decode_time_utc_cdata(_val) ->
    case catch dec_utc(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"utc">>,
			 <<"urn:xmpp:time">>}});
      _res -> _res
    end.

encode_time_utc_cdata(undefined, _acc) -> _acc;
encode_time_utc_cdata(_val, _acc) ->
    [{xmlcdata, enc_utc(_val)} | _acc].

decode_ping({xmlel, <<"ping">>, _attrs, _els}) ->
    {ping}.

encode_ping({ping}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"ping">>, _attrs, _els}.

decode_session({xmlel, <<"session">>, _attrs, _els}) ->
    {session}.

encode_session({session}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"session">>, _attrs, _els}.

decode_register({xmlel, <<"query">>, _attrs, _els}) ->
    {Zip, Misc, Address, Instructions, Text, Last, First,
     Password, Registered, Date, Phone, State, Name,
     Username, Remove, Key, City, Nick, Url, Email} =
	decode_register_els(_els, undefined, undefined,
			    undefined, undefined, undefined, undefined,
			    undefined, undefined, false, undefined, undefined,
			    undefined, undefined, undefined, false, undefined,
			    undefined, undefined, undefined, undefined),
    {register, Registered, Remove, Instructions, Username,
     Nick, Password, Name, First, Last, Email, Address, City,
     State, Zip, Phone, Url, Date, Misc, Text, Key}.

decode_register_els([], Zip, Misc, Address,
		    Instructions, Text, Last, First, Password, Registered,
		    Date, Phone, State, Name, Username, Remove, Key, City,
		    Nick, Url, Email) ->
    {Zip, Misc, Address, Instructions, Text, Last, First,
     Password, Registered, Date, Phone, State, Name,
     Username, Remove, Key, City, Nick, Url, Email};
decode_register_els([{xmlel, <<"registered">>, _attrs,
		      _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       decode_register_registered(_el), Date, Phone,
			       State, Name, Username, Remove, Key, City, Nick,
			       Url, Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"remove">>, _attrs, _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       decode_register_remove(_el), Key, City, Nick,
			       Url, Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"instructions">>, _attrs,
		      _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       decode_register_instructions(_el), Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"username">>, _attrs,
		      _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name,
			       decode_register_username(_el), Remove, Key, City,
			       Nick, Url, Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"nick">>, _attrs, _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, decode_register_nick(_el),
			       Url, Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"password">>, _attrs,
		      _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First,
			       decode_register_password(_el), Registered, Date,
			       Phone, State, Name, Username, Remove, Key, City,
			       Nick, Url, Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"name">>, _attrs, _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State,
			       decode_register_name(_el), Username, Remove, Key,
			       City, Nick, Url, Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"first">>, _attrs, _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last,
			       decode_register_first(_el), Password, Registered,
			       Date, Phone, State, Name, Username, Remove, Key,
			       City, Nick, Url, Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"last">>, _attrs, _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, decode_register_last(_el),
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"email">>, _attrs, _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url,
			       decode_register_email(_el));
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"address">>, _attrs, _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc,
			       decode_register_address(_el), Instructions, Text,
			       Last, First, Password, Registered, Date, Phone,
			       State, Name, Username, Remove, Key, City, Nick,
			       Url, Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"city">>, _attrs, _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, decode_register_city(_el), Nick,
			       Url, Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"state">>, _attrs, _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone,
			       decode_register_state(_el), Name, Username,
			       Remove, Key, City, Nick, Url, Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"zip">>, _attrs, _} = _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, decode_register_zip(_el),
			       Misc, Address, Instructions, Text, Last, First,
			       Password, Registered, Date, Phone, State, Name,
			       Username, Remove, Key, City, Nick, Url, Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"phone">>, _attrs, _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, decode_register_phone(_el),
			       State, Name, Username, Remove, Key, City, Nick,
			       Url, Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"url">>, _attrs, _} = _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick,
			       decode_register_url(_el), Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"date">>, _attrs, _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, decode_register_date(_el), Phone,
			       State, Name, Username, Remove, Key, City, Nick,
			       Url, Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"misc">>, _attrs, _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip,
			       decode_register_misc(_el), Address, Instructions,
			       Text, Last, First, Password, Registered, Date,
			       Phone, State, Name, Username, Remove, Key, City,
			       Nick, Url, Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"text">>, _attrs, _} =
			 _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, decode_register_text(_el), Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([{xmlel, <<"key">>, _attrs, _} = _el
		     | _els],
		    Zip, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:register">> ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, decode_register_key(_el), City, Nick,
			       Url, Email);
       true ->
	   decode_register_els(_els, Zip, Misc, Address,
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email)
    end;
decode_register_els([_ | _els], Zip, Misc, Address,
		    Instructions, Text, Last, First, Password, Registered,
		    Date, Phone, State, Name, Username, Remove, Key, City,
		    Nick, Url, Email) ->
    decode_register_els(_els, Zip, Misc, Address,
			Instructions, Text, Last, First, Password, Registered,
			Date, Phone, State, Name, Username, Remove, Key, City,
			Nick, Url, Email).

encode_register({register, Registered, Remove,
		 Instructions, Username, Nick, Password, Name, First,
		 Last, Email, Address, City, State, Zip, Phone, Url,
		 Date, Misc, Text, Key},
		_xmlns_attrs) ->
    _els = 'encode_register_$email'(Email,
				    'encode_register_$url'(Url,
							   'encode_register_$nick'(Nick,
										   'encode_register_$city'(City,
													   'encode_register_$key'(Key,
																  'encode_register_$remove'(Remove,
																			    'encode_register_$username'(Username,
																							'encode_register_$name'(Name,
																										'encode_register_$state'(State,
																													 'encode_register_$phone'(Phone,
																																  'encode_register_$date'(Date,
																																			  'encode_register_$registered'(Registered,
																																							'encode_register_$password'(Password,
																																										    'encode_register_$first'(First,
																																													     'encode_register_$last'(Last,
																																																     'encode_register_$text'(Text,
																																																			     'encode_register_$instructions'(Instructions,
																																																							     'encode_register_$address'(Address,
																																																											'encode_register_$misc'(Misc,
																																																														'encode_register_$zip'(Zip,
																																																																       [])))))))))))))))))))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"query">>, _attrs, _els}.

'encode_register_$zip'(undefined, _acc) -> _acc;
'encode_register_$zip'(Zip, _acc) ->
    [encode_register_zip(Zip, []) | _acc].

'encode_register_$misc'(undefined, _acc) -> _acc;
'encode_register_$misc'(Misc, _acc) ->
    [encode_register_misc(Misc, []) | _acc].

'encode_register_$address'(undefined, _acc) -> _acc;
'encode_register_$address'(Address, _acc) ->
    [encode_register_address(Address, []) | _acc].

'encode_register_$instructions'(undefined, _acc) ->
    _acc;
'encode_register_$instructions'(Instructions, _acc) ->
    [encode_register_instructions(Instructions, []) | _acc].

'encode_register_$text'(undefined, _acc) -> _acc;
'encode_register_$text'(Text, _acc) ->
    [encode_register_text(Text, []) | _acc].

'encode_register_$last'(undefined, _acc) -> _acc;
'encode_register_$last'(Last, _acc) ->
    [encode_register_last(Last, []) | _acc].

'encode_register_$first'(undefined, _acc) -> _acc;
'encode_register_$first'(First, _acc) ->
    [encode_register_first(First, []) | _acc].

'encode_register_$password'(undefined, _acc) -> _acc;
'encode_register_$password'(Password, _acc) ->
    [encode_register_password(Password, []) | _acc].

'encode_register_$registered'(false, _acc) -> _acc;
'encode_register_$registered'(Registered, _acc) ->
    [encode_register_registered(Registered, []) | _acc].

'encode_register_$date'(undefined, _acc) -> _acc;
'encode_register_$date'(Date, _acc) ->
    [encode_register_date(Date, []) | _acc].

'encode_register_$phone'(undefined, _acc) -> _acc;
'encode_register_$phone'(Phone, _acc) ->
    [encode_register_phone(Phone, []) | _acc].

'encode_register_$state'(undefined, _acc) -> _acc;
'encode_register_$state'(State, _acc) ->
    [encode_register_state(State, []) | _acc].

'encode_register_$name'(undefined, _acc) -> _acc;
'encode_register_$name'(Name, _acc) ->
    [encode_register_name(Name, []) | _acc].

'encode_register_$username'(undefined, _acc) -> _acc;
'encode_register_$username'(Username, _acc) ->
    [encode_register_username(Username, []) | _acc].

'encode_register_$remove'(false, _acc) -> _acc;
'encode_register_$remove'(Remove, _acc) ->
    [encode_register_remove(Remove, []) | _acc].

'encode_register_$key'(undefined, _acc) -> _acc;
'encode_register_$key'(Key, _acc) ->
    [encode_register_key(Key, []) | _acc].

'encode_register_$city'(undefined, _acc) -> _acc;
'encode_register_$city'(City, _acc) ->
    [encode_register_city(City, []) | _acc].

'encode_register_$nick'(undefined, _acc) -> _acc;
'encode_register_$nick'(Nick, _acc) ->
    [encode_register_nick(Nick, []) | _acc].

'encode_register_$url'(undefined, _acc) -> _acc;
'encode_register_$url'(Url, _acc) ->
    [encode_register_url(Url, []) | _acc].

'encode_register_$email'(undefined, _acc) -> _acc;
'encode_register_$email'(Email, _acc) ->
    [encode_register_email(Email, []) | _acc].

decode_register_key({xmlel, <<"key">>, _attrs, _els}) ->
    Cdata = decode_register_key_els(_els, <<>>), Cdata.

decode_register_key_els([], Cdata) ->
    decode_register_key_cdata(Cdata);
decode_register_key_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_register_key_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_register_key_els([_ | _els], Cdata) ->
    decode_register_key_els(_els, Cdata).

encode_register_key(Cdata, _xmlns_attrs) ->
    _els = encode_register_key_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"key">>, _attrs, _els}.

decode_register_key_cdata(<<>>) -> none;
decode_register_key_cdata(_val) -> _val.

encode_register_key_cdata(none, _acc) -> _acc;
encode_register_key_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_text({xmlel, <<"text">>, _attrs,
		      _els}) ->
    Cdata = decode_register_text_els(_els, <<>>), Cdata.

decode_register_text_els([], Cdata) ->
    decode_register_text_cdata(Cdata);
decode_register_text_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_register_text_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_register_text_els([_ | _els], Cdata) ->
    decode_register_text_els(_els, Cdata).

encode_register_text(Cdata, _xmlns_attrs) ->
    _els = encode_register_text_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"text">>, _attrs, _els}.

decode_register_text_cdata(<<>>) -> none;
decode_register_text_cdata(_val) -> _val.

encode_register_text_cdata(none, _acc) -> _acc;
encode_register_text_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_misc({xmlel, <<"misc">>, _attrs,
		      _els}) ->
    Cdata = decode_register_misc_els(_els, <<>>), Cdata.

decode_register_misc_els([], Cdata) ->
    decode_register_misc_cdata(Cdata);
decode_register_misc_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_register_misc_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_register_misc_els([_ | _els], Cdata) ->
    decode_register_misc_els(_els, Cdata).

encode_register_misc(Cdata, _xmlns_attrs) ->
    _els = encode_register_misc_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"misc">>, _attrs, _els}.

decode_register_misc_cdata(<<>>) -> none;
decode_register_misc_cdata(_val) -> _val.

encode_register_misc_cdata(none, _acc) -> _acc;
encode_register_misc_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_date({xmlel, <<"date">>, _attrs,
		      _els}) ->
    Cdata = decode_register_date_els(_els, <<>>), Cdata.

decode_register_date_els([], Cdata) ->
    decode_register_date_cdata(Cdata);
decode_register_date_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_register_date_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_register_date_els([_ | _els], Cdata) ->
    decode_register_date_els(_els, Cdata).

encode_register_date(Cdata, _xmlns_attrs) ->
    _els = encode_register_date_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"date">>, _attrs, _els}.

decode_register_date_cdata(<<>>) -> none;
decode_register_date_cdata(_val) -> _val.

encode_register_date_cdata(none, _acc) -> _acc;
encode_register_date_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_url({xmlel, <<"url">>, _attrs, _els}) ->
    Cdata = decode_register_url_els(_els, <<>>), Cdata.

decode_register_url_els([], Cdata) ->
    decode_register_url_cdata(Cdata);
decode_register_url_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_register_url_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_register_url_els([_ | _els], Cdata) ->
    decode_register_url_els(_els, Cdata).

encode_register_url(Cdata, _xmlns_attrs) ->
    _els = encode_register_url_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"url">>, _attrs, _els}.

decode_register_url_cdata(<<>>) -> none;
decode_register_url_cdata(_val) -> _val.

encode_register_url_cdata(none, _acc) -> _acc;
encode_register_url_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_phone({xmlel, <<"phone">>, _attrs,
		       _els}) ->
    Cdata = decode_register_phone_els(_els, <<>>), Cdata.

decode_register_phone_els([], Cdata) ->
    decode_register_phone_cdata(Cdata);
decode_register_phone_els([{xmlcdata, _data} | _els],
			  Cdata) ->
    decode_register_phone_els(_els,
			      <<Cdata/binary, _data/binary>>);
decode_register_phone_els([_ | _els], Cdata) ->
    decode_register_phone_els(_els, Cdata).

encode_register_phone(Cdata, _xmlns_attrs) ->
    _els = encode_register_phone_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"phone">>, _attrs, _els}.

decode_register_phone_cdata(<<>>) -> none;
decode_register_phone_cdata(_val) -> _val.

encode_register_phone_cdata(none, _acc) -> _acc;
encode_register_phone_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_zip({xmlel, <<"zip">>, _attrs, _els}) ->
    Cdata = decode_register_zip_els(_els, <<>>), Cdata.

decode_register_zip_els([], Cdata) ->
    decode_register_zip_cdata(Cdata);
decode_register_zip_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_register_zip_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_register_zip_els([_ | _els], Cdata) ->
    decode_register_zip_els(_els, Cdata).

encode_register_zip(Cdata, _xmlns_attrs) ->
    _els = encode_register_zip_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"zip">>, _attrs, _els}.

decode_register_zip_cdata(<<>>) -> none;
decode_register_zip_cdata(_val) -> _val.

encode_register_zip_cdata(none, _acc) -> _acc;
encode_register_zip_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_state({xmlel, <<"state">>, _attrs,
		       _els}) ->
    Cdata = decode_register_state_els(_els, <<>>), Cdata.

decode_register_state_els([], Cdata) ->
    decode_register_state_cdata(Cdata);
decode_register_state_els([{xmlcdata, _data} | _els],
			  Cdata) ->
    decode_register_state_els(_els,
			      <<Cdata/binary, _data/binary>>);
decode_register_state_els([_ | _els], Cdata) ->
    decode_register_state_els(_els, Cdata).

encode_register_state(Cdata, _xmlns_attrs) ->
    _els = encode_register_state_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"state">>, _attrs, _els}.

decode_register_state_cdata(<<>>) -> none;
decode_register_state_cdata(_val) -> _val.

encode_register_state_cdata(none, _acc) -> _acc;
encode_register_state_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_city({xmlel, <<"city">>, _attrs,
		      _els}) ->
    Cdata = decode_register_city_els(_els, <<>>), Cdata.

decode_register_city_els([], Cdata) ->
    decode_register_city_cdata(Cdata);
decode_register_city_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_register_city_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_register_city_els([_ | _els], Cdata) ->
    decode_register_city_els(_els, Cdata).

encode_register_city(Cdata, _xmlns_attrs) ->
    _els = encode_register_city_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"city">>, _attrs, _els}.

decode_register_city_cdata(<<>>) -> none;
decode_register_city_cdata(_val) -> _val.

encode_register_city_cdata(none, _acc) -> _acc;
encode_register_city_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_address({xmlel, <<"address">>, _attrs,
			 _els}) ->
    Cdata = decode_register_address_els(_els, <<>>), Cdata.

decode_register_address_els([], Cdata) ->
    decode_register_address_cdata(Cdata);
decode_register_address_els([{xmlcdata, _data} | _els],
			    Cdata) ->
    decode_register_address_els(_els,
				<<Cdata/binary, _data/binary>>);
decode_register_address_els([_ | _els], Cdata) ->
    decode_register_address_els(_els, Cdata).

encode_register_address(Cdata, _xmlns_attrs) ->
    _els = encode_register_address_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"address">>, _attrs, _els}.

decode_register_address_cdata(<<>>) -> none;
decode_register_address_cdata(_val) -> _val.

encode_register_address_cdata(none, _acc) -> _acc;
encode_register_address_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_email({xmlel, <<"email">>, _attrs,
		       _els}) ->
    Cdata = decode_register_email_els(_els, <<>>), Cdata.

decode_register_email_els([], Cdata) ->
    decode_register_email_cdata(Cdata);
decode_register_email_els([{xmlcdata, _data} | _els],
			  Cdata) ->
    decode_register_email_els(_els,
			      <<Cdata/binary, _data/binary>>);
decode_register_email_els([_ | _els], Cdata) ->
    decode_register_email_els(_els, Cdata).

encode_register_email(Cdata, _xmlns_attrs) ->
    _els = encode_register_email_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"email">>, _attrs, _els}.

decode_register_email_cdata(<<>>) -> none;
decode_register_email_cdata(_val) -> _val.

encode_register_email_cdata(none, _acc) -> _acc;
encode_register_email_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_last({xmlel, <<"last">>, _attrs,
		      _els}) ->
    Cdata = decode_register_last_els(_els, <<>>), Cdata.

decode_register_last_els([], Cdata) ->
    decode_register_last_cdata(Cdata);
decode_register_last_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_register_last_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_register_last_els([_ | _els], Cdata) ->
    decode_register_last_els(_els, Cdata).

encode_register_last(Cdata, _xmlns_attrs) ->
    _els = encode_register_last_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"last">>, _attrs, _els}.

decode_register_last_cdata(<<>>) -> none;
decode_register_last_cdata(_val) -> _val.

encode_register_last_cdata(none, _acc) -> _acc;
encode_register_last_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_first({xmlel, <<"first">>, _attrs,
		       _els}) ->
    Cdata = decode_register_first_els(_els, <<>>), Cdata.

decode_register_first_els([], Cdata) ->
    decode_register_first_cdata(Cdata);
decode_register_first_els([{xmlcdata, _data} | _els],
			  Cdata) ->
    decode_register_first_els(_els,
			      <<Cdata/binary, _data/binary>>);
decode_register_first_els([_ | _els], Cdata) ->
    decode_register_first_els(_els, Cdata).

encode_register_first(Cdata, _xmlns_attrs) ->
    _els = encode_register_first_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"first">>, _attrs, _els}.

decode_register_first_cdata(<<>>) -> none;
decode_register_first_cdata(_val) -> _val.

encode_register_first_cdata(none, _acc) -> _acc;
encode_register_first_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_name({xmlel, <<"name">>, _attrs,
		      _els}) ->
    Cdata = decode_register_name_els(_els, <<>>), Cdata.

decode_register_name_els([], Cdata) ->
    decode_register_name_cdata(Cdata);
decode_register_name_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_register_name_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_register_name_els([_ | _els], Cdata) ->
    decode_register_name_els(_els, Cdata).

encode_register_name(Cdata, _xmlns_attrs) ->
    _els = encode_register_name_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"name">>, _attrs, _els}.

decode_register_name_cdata(<<>>) -> none;
decode_register_name_cdata(_val) -> _val.

encode_register_name_cdata(none, _acc) -> _acc;
encode_register_name_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_password({xmlel, <<"password">>, _attrs,
			  _els}) ->
    Cdata = decode_register_password_els(_els, <<>>), Cdata.

decode_register_password_els([], Cdata) ->
    decode_register_password_cdata(Cdata);
decode_register_password_els([{xmlcdata, _data} | _els],
			     Cdata) ->
    decode_register_password_els(_els,
				 <<Cdata/binary, _data/binary>>);
decode_register_password_els([_ | _els], Cdata) ->
    decode_register_password_els(_els, Cdata).

encode_register_password(Cdata, _xmlns_attrs) ->
    _els = encode_register_password_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"password">>, _attrs, _els}.

decode_register_password_cdata(<<>>) -> none;
decode_register_password_cdata(_val) -> _val.

encode_register_password_cdata(none, _acc) -> _acc;
encode_register_password_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_nick({xmlel, <<"nick">>, _attrs,
		      _els}) ->
    Cdata = decode_register_nick_els(_els, <<>>), Cdata.

decode_register_nick_els([], Cdata) ->
    decode_register_nick_cdata(Cdata);
decode_register_nick_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_register_nick_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_register_nick_els([_ | _els], Cdata) ->
    decode_register_nick_els(_els, Cdata).

encode_register_nick(Cdata, _xmlns_attrs) ->
    _els = encode_register_nick_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"nick">>, _attrs, _els}.

decode_register_nick_cdata(<<>>) -> none;
decode_register_nick_cdata(_val) -> _val.

encode_register_nick_cdata(none, _acc) -> _acc;
encode_register_nick_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_username({xmlel, <<"username">>, _attrs,
			  _els}) ->
    Cdata = decode_register_username_els(_els, <<>>), Cdata.

decode_register_username_els([], Cdata) ->
    decode_register_username_cdata(Cdata);
decode_register_username_els([{xmlcdata, _data} | _els],
			     Cdata) ->
    decode_register_username_els(_els,
				 <<Cdata/binary, _data/binary>>);
decode_register_username_els([_ | _els], Cdata) ->
    decode_register_username_els(_els, Cdata).

encode_register_username(Cdata, _xmlns_attrs) ->
    _els = encode_register_username_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"username">>, _attrs, _els}.

decode_register_username_cdata(<<>>) -> none;
decode_register_username_cdata(_val) -> _val.

encode_register_username_cdata(none, _acc) -> _acc;
encode_register_username_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_instructions({xmlel, <<"instructions">>,
			      _attrs, _els}) ->
    Cdata = decode_register_instructions_els(_els, <<>>),
    Cdata.

decode_register_instructions_els([], Cdata) ->
    decode_register_instructions_cdata(Cdata);
decode_register_instructions_els([{xmlcdata, _data}
				  | _els],
				 Cdata) ->
    decode_register_instructions_els(_els,
				     <<Cdata/binary, _data/binary>>);
decode_register_instructions_els([_ | _els], Cdata) ->
    decode_register_instructions_els(_els, Cdata).

encode_register_instructions(Cdata, _xmlns_attrs) ->
    _els = encode_register_instructions_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"instructions">>, _attrs, _els}.

decode_register_instructions_cdata(<<>>) -> undefined;
decode_register_instructions_cdata(_val) -> _val.

encode_register_instructions_cdata(undefined, _acc) ->
    _acc;
encode_register_instructions_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_remove({xmlel, <<"remove">>, _attrs,
			_els}) ->
    true.

encode_register_remove(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"remove">>, _attrs, _els}.

decode_register_registered({xmlel, <<"registered">>,
			    _attrs, _els}) ->
    true.

encode_register_registered(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"registered">>, _attrs, _els}.

decode_feature_register({xmlel, <<"register">>, _attrs,
			 _els}) ->
    {feature_register}.

encode_feature_register({feature_register},
			_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"register">>, _attrs, _els}.

decode_caps({xmlel, <<"c">>, _attrs, _els}) ->
    {Hash, Node, Ver} = decode_caps_attrs(_attrs, undefined,
					  undefined, undefined),
    {caps, Hash, Node, Ver}.

decode_caps_attrs([{<<"hash">>, _val} | _attrs], _Hash,
		  Node, Ver) ->
    decode_caps_attrs(_attrs, _val, Node, Ver);
decode_caps_attrs([{<<"node">>, _val} | _attrs], Hash,
		  _Node, Ver) ->
    decode_caps_attrs(_attrs, Hash, _val, Ver);
decode_caps_attrs([{<<"ver">>, _val} | _attrs], Hash,
		  Node, _Ver) ->
    decode_caps_attrs(_attrs, Hash, Node, _val);
decode_caps_attrs([_ | _attrs], Hash, Node, Ver) ->
    decode_caps_attrs(_attrs, Hash, Node, Ver);
decode_caps_attrs([], Hash, Node, Ver) ->
    {decode_caps_attr_hash(Hash),
     decode_caps_attr_node(Node), decode_caps_attr_ver(Ver)}.

encode_caps({caps, Hash, Node, Ver}, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_caps_attr_ver(Ver,
				  encode_caps_attr_node(Node,
							encode_caps_attr_hash(Hash,
									      _xmlns_attrs))),
    {xmlel, <<"c">>, _attrs, _els}.

decode_caps_attr_hash(undefined) -> undefined;
decode_caps_attr_hash(_val) -> _val.

encode_caps_attr_hash(undefined, _acc) -> _acc;
encode_caps_attr_hash(_val, _acc) ->
    [{<<"hash">>, _val} | _acc].

decode_caps_attr_node(undefined) -> undefined;
decode_caps_attr_node(_val) -> _val.

encode_caps_attr_node(undefined, _acc) -> _acc;
encode_caps_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_caps_attr_ver(undefined) -> undefined;
decode_caps_attr_ver(_val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"ver">>, <<"c">>,
			 <<"http://jabber.org/protocol/caps">>}});
      _res -> _res
    end.

encode_caps_attr_ver(undefined, _acc) -> _acc;
encode_caps_attr_ver(_val, _acc) ->
    [{<<"ver">>, base64:encode(_val)} | _acc].

decode_p1_ack({xmlel, <<"ack">>, _attrs, _els}) ->
    {p1_ack}.

encode_p1_ack({p1_ack}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"ack">>, _attrs, _els}.

decode_p1_rebind({xmlel, <<"rebind">>, _attrs, _els}) ->
    {p1_rebind}.

encode_p1_rebind({p1_rebind}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"rebind">>, _attrs, _els}.

decode_p1_push({xmlel, <<"push">>, _attrs, _els}) ->
    {p1_push}.

encode_p1_push({p1_push}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"push">>, _attrs, _els}.

decode_stream_features({xmlel, <<"stream:features">>,
			_attrs, _els}) ->
    __Els = decode_stream_features_els(_els, []),
    {stream_features, __Els}.

decode_stream_features_els([], __Els) ->
    lists:reverse(__Els);
decode_stream_features_els([{xmlel, _, _, _} = _el
			    | _els],
			   __Els) ->
    case is_known_tag(_el) of
      true ->
	  decode_stream_features_els(_els, [decode(_el) | __Els]);
      false -> decode_stream_features_els(_els, __Els)
    end;
decode_stream_features_els([_ | _els], __Els) ->
    decode_stream_features_els(_els, __Els).

encode_stream_features({stream_features, __Els},
		       _xmlns_attrs) ->
    _els = [encode(_el) || _el <- __Els],
    _attrs = _xmlns_attrs,
    {xmlel, <<"stream:features">>, _attrs, _els}.

decode_compression({xmlel, <<"compression">>, _attrs,
		    _els}) ->
    Methods = decode_compression_els(_els, []),
    {compression, Methods}.

decode_compression_els([], Methods) ->
    lists:reverse(Methods);
decode_compression_els([{xmlel, <<"method">>, _attrs,
			 _} =
			    _el
			| _els],
		       Methods) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/features/compress">> ->
	   decode_compression_els(_els,
				  case decode_compression_method(_el) of
				    undefined -> Methods;
				    _new_el -> [_new_el | Methods]
				  end);
       true -> decode_compression_els(_els, Methods)
    end;
decode_compression_els([_ | _els], Methods) ->
    decode_compression_els(_els, Methods).

encode_compression({compression, Methods},
		   _xmlns_attrs) ->
    _els = 'encode_compression_$methods'(Methods, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"compression">>, _attrs, _els}.

'encode_compression_$methods'([], _acc) -> _acc;
'encode_compression_$methods'([Methods | _els], _acc) ->
    'encode_compression_$methods'(_els,
				  [encode_compression_method(Methods, [])
				   | _acc]).

decode_compression_method({xmlel, <<"method">>, _attrs,
			   _els}) ->
    Cdata = decode_compression_method_els(_els, <<>>),
    Cdata.

decode_compression_method_els([], Cdata) ->
    decode_compression_method_cdata(Cdata);
decode_compression_method_els([{xmlcdata, _data}
			       | _els],
			      Cdata) ->
    decode_compression_method_els(_els,
				  <<Cdata/binary, _data/binary>>);
decode_compression_method_els([_ | _els], Cdata) ->
    decode_compression_method_els(_els, Cdata).

encode_compression_method(Cdata, _xmlns_attrs) ->
    _els = encode_compression_method_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"method">>, _attrs, _els}.

decode_compression_method_cdata(<<>>) -> undefined;
decode_compression_method_cdata(_val) -> _val.

encode_compression_method_cdata(undefined, _acc) ->
    _acc;
encode_compression_method_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_compressed({xmlel, <<"compressed">>, _attrs,
		   _els}) ->
    {compressed}.

encode_compressed({compressed}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"compressed">>, _attrs, _els}.

decode_compress({xmlel, <<"compress">>, _attrs,
		 _els}) ->
    Methods = decode_compress_els(_els, []),
    {compress, Methods}.

decode_compress_els([], Methods) ->
    lists:reverse(Methods);
decode_compress_els([{xmlel, <<"method">>, _attrs, _} =
			 _el
		     | _els],
		    Methods) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/compress">> ->
	   decode_compress_els(_els,
			       case decode_compress_method(_el) of
				 undefined -> Methods;
				 _new_el -> [_new_el | Methods]
			       end);
       true -> decode_compress_els(_els, Methods)
    end;
decode_compress_els([_ | _els], Methods) ->
    decode_compress_els(_els, Methods).

encode_compress({compress, Methods}, _xmlns_attrs) ->
    _els = 'encode_compress_$methods'(Methods, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"compress">>, _attrs, _els}.

'encode_compress_$methods'([], _acc) -> _acc;
'encode_compress_$methods'([Methods | _els], _acc) ->
    'encode_compress_$methods'(_els,
			       [encode_compress_method(Methods, []) | _acc]).

decode_compress_method({xmlel, <<"method">>, _attrs,
			_els}) ->
    Cdata = decode_compress_method_els(_els, <<>>), Cdata.

decode_compress_method_els([], Cdata) ->
    decode_compress_method_cdata(Cdata);
decode_compress_method_els([{xmlcdata, _data} | _els],
			   Cdata) ->
    decode_compress_method_els(_els,
			       <<Cdata/binary, _data/binary>>);
decode_compress_method_els([_ | _els], Cdata) ->
    decode_compress_method_els(_els, Cdata).

encode_compress_method(Cdata, _xmlns_attrs) ->
    _els = encode_compress_method_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"method">>, _attrs, _els}.

decode_compress_method_cdata(<<>>) -> undefined;
decode_compress_method_cdata(_val) -> _val.

encode_compress_method_cdata(undefined, _acc) -> _acc;
encode_compress_method_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_compress_failure({xmlel, <<"failure">>, _attrs,
			 _els}) ->
    Reason = decode_compress_failure_els(_els, undefined),
    {compress_failure, Reason}.

decode_compress_failure_els([], Reason) -> Reason;
decode_compress_failure_els([{xmlel, <<"setup-failed">>,
			      _attrs, _} =
				 _el
			     | _els],
			    Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/compress">> ->
	   decode_compress_failure_els(_els,
				       decode_compress_failure_setup_failed(_el));
       true -> decode_compress_failure_els(_els, Reason)
    end;
decode_compress_failure_els([{xmlel,
			      <<"processing-failed">>, _attrs, _} =
				 _el
			     | _els],
			    Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/compress">> ->
	   decode_compress_failure_els(_els,
				       decode_compress_failure_processing_failed(_el));
       true -> decode_compress_failure_els(_els, Reason)
    end;
decode_compress_failure_els([{xmlel,
			      <<"unsupported-method">>, _attrs, _} =
				 _el
			     | _els],
			    Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/compress">> ->
	   decode_compress_failure_els(_els,
				       decode_compress_failure_unsupported_method(_el));
       true -> decode_compress_failure_els(_els, Reason)
    end;
decode_compress_failure_els([_ | _els], Reason) ->
    decode_compress_failure_els(_els, Reason).

encode_compress_failure({compress_failure, Reason},
			_xmlns_attrs) ->
    _els = 'encode_compress_failure_$reason'(Reason, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"failure">>, _attrs, _els}.

'encode_compress_failure_$reason'(undefined, _acc) ->
    _acc;
'encode_compress_failure_$reason'('setup-failed' =
				      Reason,
				  _acc) ->
    [encode_compress_failure_setup_failed(Reason, [])
     | _acc];
'encode_compress_failure_$reason'('processing-failed' =
				      Reason,
				  _acc) ->
    [encode_compress_failure_processing_failed(Reason, [])
     | _acc];
'encode_compress_failure_$reason'('unsupported-method' =
				      Reason,
				  _acc) ->
    [encode_compress_failure_unsupported_method(Reason, [])
     | _acc].

decode_compress_failure_unsupported_method({xmlel,
					    <<"unsupported-method">>, _attrs,
					    _els}) ->
    'unsupported-method'.

encode_compress_failure_unsupported_method('unsupported-method',
					   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"unsupported-method">>, _attrs, _els}.

decode_compress_failure_processing_failed({xmlel,
					   <<"processing-failed">>, _attrs,
					   _els}) ->
    'processing-failed'.

encode_compress_failure_processing_failed('processing-failed',
					  _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"processing-failed">>, _attrs, _els}.

decode_compress_failure_setup_failed({xmlel,
				      <<"setup-failed">>, _attrs, _els}) ->
    'setup-failed'.

encode_compress_failure_setup_failed('setup-failed',
				     _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"setup-failed">>, _attrs, _els}.

decode_starttls_failure({xmlel, <<"failure">>, _attrs,
			 _els}) ->
    {starttls_failure}.

encode_starttls_failure({starttls_failure},
			_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"failure">>, _attrs, _els}.

decode_starttls_proceed({xmlel, <<"proceed">>, _attrs,
			 _els}) ->
    {starttls_proceed}.

encode_starttls_proceed({starttls_proceed},
			_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"proceed">>, _attrs, _els}.

decode_starttls({xmlel, <<"starttls">>, _attrs,
		 _els}) ->
    Required = decode_starttls_els(_els, false),
    {starttls, Required}.

decode_starttls_els([], Required) -> Required;
decode_starttls_els([{xmlel, <<"required">>, _attrs,
		      _} =
			 _el
		     | _els],
		    Required) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-tls">> ->
	   decode_starttls_els(_els,
			       decode_starttls_required(_el));
       true -> decode_starttls_els(_els, Required)
    end;
decode_starttls_els([_ | _els], Required) ->
    decode_starttls_els(_els, Required).

encode_starttls({starttls, Required}, _xmlns_attrs) ->
    _els = 'encode_starttls_$required'(Required, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"starttls">>, _attrs, _els}.

'encode_starttls_$required'(false, _acc) -> _acc;
'encode_starttls_$required'(Required, _acc) ->
    [encode_starttls_required(Required, []) | _acc].

decode_starttls_required({xmlel, <<"required">>, _attrs,
			  _els}) ->
    true.

encode_starttls_required(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"required">>, _attrs, _els}.

decode_sasl_mechanisms({xmlel, <<"mechanisms">>, _attrs,
			_els}) ->
    List = decode_sasl_mechanisms_els(_els, []),
    {sasl_mechanisms, List}.

decode_sasl_mechanisms_els([], List) ->
    lists:reverse(List);
decode_sasl_mechanisms_els([{xmlel, <<"mechanism">>,
			     _attrs, _} =
				_el
			    | _els],
			   List) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-sasl">> ->
	   decode_sasl_mechanisms_els(_els,
				      case decode_sasl_mechanism(_el) of
					undefined -> List;
					_new_el -> [_new_el | List]
				      end);
       true -> decode_sasl_mechanisms_els(_els, List)
    end;
decode_sasl_mechanisms_els([_ | _els], List) ->
    decode_sasl_mechanisms_els(_els, List).

encode_sasl_mechanisms({sasl_mechanisms, List},
		       _xmlns_attrs) ->
    _els = 'encode_sasl_mechanisms_$list'(List, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"mechanisms">>, _attrs, _els}.

'encode_sasl_mechanisms_$list'([], _acc) -> _acc;
'encode_sasl_mechanisms_$list'([List | _els], _acc) ->
    'encode_sasl_mechanisms_$list'(_els,
				   [encode_sasl_mechanism(List, []) | _acc]).

decode_sasl_mechanism({xmlel, <<"mechanism">>, _attrs,
		       _els}) ->
    Cdata = decode_sasl_mechanism_els(_els, <<>>), Cdata.

decode_sasl_mechanism_els([], Cdata) ->
    decode_sasl_mechanism_cdata(Cdata);
decode_sasl_mechanism_els([{xmlcdata, _data} | _els],
			  Cdata) ->
    decode_sasl_mechanism_els(_els,
			      <<Cdata/binary, _data/binary>>);
decode_sasl_mechanism_els([_ | _els], Cdata) ->
    decode_sasl_mechanism_els(_els, Cdata).

encode_sasl_mechanism(Cdata, _xmlns_attrs) ->
    _els = encode_sasl_mechanism_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"mechanism">>, _attrs, _els}.

decode_sasl_mechanism_cdata(<<>>) -> undefined;
decode_sasl_mechanism_cdata(_val) -> _val.

encode_sasl_mechanism_cdata(undefined, _acc) -> _acc;
encode_sasl_mechanism_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_sasl_failure({xmlel, <<"failure">>, _attrs,
		     _els}) ->
    {Text, Reason} = decode_sasl_failure_els(_els, [],
					     undefined),
    {sasl_failure, Reason, Text}.

decode_sasl_failure_els([], Text, Reason) ->
    {lists:reverse(Text), Reason};
decode_sasl_failure_els([{xmlel, <<"text">>, _attrs,
			  _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-sasl">> ->
	   decode_sasl_failure_els(_els,
				   [decode_sasl_failure_text(_el) | Text],
				   Reason);
       true -> decode_sasl_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_els([{xmlel, <<"aborted">>, _attrs,
			  _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-sasl">> ->
	   decode_sasl_failure_els(_els, Text,
				   decode_sasl_failure_aborted(_el));
       true -> decode_sasl_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_els([{xmlel, <<"account-disabled">>,
			  _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-sasl">> ->
	   decode_sasl_failure_els(_els, Text,
				   decode_sasl_failure_account_disabled(_el));
       true -> decode_sasl_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_els([{xmlel,
			  <<"credentials-expired">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-sasl">> ->
	   decode_sasl_failure_els(_els, Text,
				   decode_sasl_failure_credentials_expired(_el));
       true -> decode_sasl_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_els([{xmlel,
			  <<"encryption-required">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-sasl">> ->
	   decode_sasl_failure_els(_els, Text,
				   decode_sasl_failure_encryption_required(_el));
       true -> decode_sasl_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_els([{xmlel,
			  <<"incorrect-encoding">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-sasl">> ->
	   decode_sasl_failure_els(_els, Text,
				   decode_sasl_failure_incorrect_encoding(_el));
       true -> decode_sasl_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_els([{xmlel, <<"invalid-authzid">>,
			  _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-sasl">> ->
	   decode_sasl_failure_els(_els, Text,
				   decode_sasl_failure_invalid_authzid(_el));
       true -> decode_sasl_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_els([{xmlel,
			  <<"invalid-mechanism">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-sasl">> ->
	   decode_sasl_failure_els(_els, Text,
				   decode_sasl_failure_invalid_mechanism(_el));
       true -> decode_sasl_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_els([{xmlel,
			  <<"malformed-request">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-sasl">> ->
	   decode_sasl_failure_els(_els, Text,
				   decode_sasl_failure_malformed_request(_el));
       true -> decode_sasl_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_els([{xmlel,
			  <<"mechanism-too-weak">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-sasl">> ->
	   decode_sasl_failure_els(_els, Text,
				   decode_sasl_failure_mechanism_too_weak(_el));
       true -> decode_sasl_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_els([{xmlel, <<"not-authorized">>,
			  _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-sasl">> ->
	   decode_sasl_failure_els(_els, Text,
				   decode_sasl_failure_not_authorized(_el));
       true -> decode_sasl_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_els([{xmlel,
			  <<"temporary-auth-failure">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-sasl">> ->
	   decode_sasl_failure_els(_els, Text,
				   decode_sasl_failure_temporary_auth_failure(_el));
       true -> decode_sasl_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_els([_ | _els], Text, Reason) ->
    decode_sasl_failure_els(_els, Text, Reason).

encode_sasl_failure({sasl_failure, Reason, Text},
		    _xmlns_attrs) ->
    _els = 'encode_sasl_failure_$reason'(Reason,
					 'encode_sasl_failure_$text'(Text, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"failure">>, _attrs, _els}.

'encode_sasl_failure_$text'([], _acc) -> _acc;
'encode_sasl_failure_$text'([Text | _els], _acc) ->
    'encode_sasl_failure_$text'(_els,
				[encode_sasl_failure_text(Text, []) | _acc]).

'encode_sasl_failure_$reason'(undefined, _acc) -> _acc;
'encode_sasl_failure_$reason'(aborted = Reason, _acc) ->
    [encode_sasl_failure_aborted(Reason, []) | _acc];
'encode_sasl_failure_$reason'('account-disabled' =
				  Reason,
			      _acc) ->
    [encode_sasl_failure_account_disabled(Reason, [])
     | _acc];
'encode_sasl_failure_$reason'('credentials-expired' =
				  Reason,
			      _acc) ->
    [encode_sasl_failure_credentials_expired(Reason, [])
     | _acc];
'encode_sasl_failure_$reason'('encryption-required' =
				  Reason,
			      _acc) ->
    [encode_sasl_failure_encryption_required(Reason, [])
     | _acc];
'encode_sasl_failure_$reason'('incorrect-encoding' =
				  Reason,
			      _acc) ->
    [encode_sasl_failure_incorrect_encoding(Reason, [])
     | _acc];
'encode_sasl_failure_$reason'('invalid-authzid' =
				  Reason,
			      _acc) ->
    [encode_sasl_failure_invalid_authzid(Reason, [])
     | _acc];
'encode_sasl_failure_$reason'('invalid-mechanism' =
				  Reason,
			      _acc) ->
    [encode_sasl_failure_invalid_mechanism(Reason, [])
     | _acc];
'encode_sasl_failure_$reason'('malformed-request' =
				  Reason,
			      _acc) ->
    [encode_sasl_failure_malformed_request(Reason, [])
     | _acc];
'encode_sasl_failure_$reason'('mechanism-too-weak' =
				  Reason,
			      _acc) ->
    [encode_sasl_failure_mechanism_too_weak(Reason, [])
     | _acc];
'encode_sasl_failure_$reason'('not-authorized' = Reason,
			      _acc) ->
    [encode_sasl_failure_not_authorized(Reason, []) | _acc];
'encode_sasl_failure_$reason'('temporary-auth-failure' =
				  Reason,
			      _acc) ->
    [encode_sasl_failure_temporary_auth_failure(Reason, [])
     | _acc].

decode_sasl_failure_temporary_auth_failure({xmlel,
					    <<"temporary-auth-failure">>,
					    _attrs, _els}) ->
    'temporary-auth-failure'.

encode_sasl_failure_temporary_auth_failure('temporary-auth-failure',
					   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"temporary-auth-failure">>, _attrs, _els}.

decode_sasl_failure_not_authorized({xmlel,
				    <<"not-authorized">>, _attrs, _els}) ->
    'not-authorized'.

encode_sasl_failure_not_authorized('not-authorized',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"not-authorized">>, _attrs, _els}.

decode_sasl_failure_mechanism_too_weak({xmlel,
					<<"mechanism-too-weak">>, _attrs,
					_els}) ->
    'mechanism-too-weak'.

encode_sasl_failure_mechanism_too_weak('mechanism-too-weak',
				       _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"mechanism-too-weak">>, _attrs, _els}.

decode_sasl_failure_malformed_request({xmlel,
				       <<"malformed-request">>, _attrs,
				       _els}) ->
    'malformed-request'.

encode_sasl_failure_malformed_request('malformed-request',
				      _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"malformed-request">>, _attrs, _els}.

decode_sasl_failure_invalid_mechanism({xmlel,
				       <<"invalid-mechanism">>, _attrs,
				       _els}) ->
    'invalid-mechanism'.

encode_sasl_failure_invalid_mechanism('invalid-mechanism',
				      _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"invalid-mechanism">>, _attrs, _els}.

decode_sasl_failure_invalid_authzid({xmlel,
				     <<"invalid-authzid">>, _attrs, _els}) ->
    'invalid-authzid'.

encode_sasl_failure_invalid_authzid('invalid-authzid',
				    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"invalid-authzid">>, _attrs, _els}.

decode_sasl_failure_incorrect_encoding({xmlel,
					<<"incorrect-encoding">>, _attrs,
					_els}) ->
    'incorrect-encoding'.

encode_sasl_failure_incorrect_encoding('incorrect-encoding',
				       _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"incorrect-encoding">>, _attrs, _els}.

decode_sasl_failure_encryption_required({xmlel,
					 <<"encryption-required">>, _attrs,
					 _els}) ->
    'encryption-required'.

encode_sasl_failure_encryption_required('encryption-required',
					_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"encryption-required">>, _attrs, _els}.

decode_sasl_failure_credentials_expired({xmlel,
					 <<"credentials-expired">>, _attrs,
					 _els}) ->
    'credentials-expired'.

encode_sasl_failure_credentials_expired('credentials-expired',
					_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"credentials-expired">>, _attrs, _els}.

decode_sasl_failure_account_disabled({xmlel,
				      <<"account-disabled">>, _attrs, _els}) ->
    'account-disabled'.

encode_sasl_failure_account_disabled('account-disabled',
				     _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"account-disabled">>, _attrs, _els}.

decode_sasl_failure_aborted({xmlel, <<"aborted">>,
			     _attrs, _els}) ->
    aborted.

encode_sasl_failure_aborted(aborted, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"aborted">>, _attrs, _els}.

decode_sasl_failure_text({xmlel, <<"text">>, _attrs,
			  _els}) ->
    Data = decode_sasl_failure_text_els(_els, <<>>),
    Lang = decode_sasl_failure_text_attrs(_attrs,
					  undefined),
    {text, Lang, Data}.

decode_sasl_failure_text_els([], Data) ->
    decode_sasl_failure_text_cdata(Data);
decode_sasl_failure_text_els([{xmlcdata, _data} | _els],
			     Data) ->
    decode_sasl_failure_text_els(_els,
				 <<Data/binary, _data/binary>>);
decode_sasl_failure_text_els([_ | _els], Data) ->
    decode_sasl_failure_text_els(_els, Data).

decode_sasl_failure_text_attrs([{<<"xml:lang">>, _val}
				| _attrs],
			       _Lang) ->
    decode_sasl_failure_text_attrs(_attrs, _val);
decode_sasl_failure_text_attrs([_ | _attrs], Lang) ->
    decode_sasl_failure_text_attrs(_attrs, Lang);
decode_sasl_failure_text_attrs([], Lang) ->
    'decode_sasl_failure_text_attr_xml:lang'(Lang).

encode_sasl_failure_text({text, Lang, Data},
			 _xmlns_attrs) ->
    _els = encode_sasl_failure_text_cdata(Data, []),
    _attrs = 'encode_sasl_failure_text_attr_xml:lang'(Lang,
						      _xmlns_attrs),
    {xmlel, <<"text">>, _attrs, _els}.

'decode_sasl_failure_text_attr_xml:lang'(undefined) ->
    undefined;
'decode_sasl_failure_text_attr_xml:lang'(_val) -> _val.

'encode_sasl_failure_text_attr_xml:lang'(undefined,
					 _acc) ->
    _acc;
'encode_sasl_failure_text_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_sasl_failure_text_cdata(<<>>) -> undefined;
decode_sasl_failure_text_cdata(_val) -> _val.

encode_sasl_failure_text_cdata(undefined, _acc) -> _acc;
encode_sasl_failure_text_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_sasl_success({xmlel, <<"success">>, _attrs,
		     _els}) ->
    Text = decode_sasl_success_els(_els, <<>>),
    {sasl_success, Text}.

decode_sasl_success_els([], Text) ->
    decode_sasl_success_cdata(Text);
decode_sasl_success_els([{xmlcdata, _data} | _els],
			Text) ->
    decode_sasl_success_els(_els,
			    <<Text/binary, _data/binary>>);
decode_sasl_success_els([_ | _els], Text) ->
    decode_sasl_success_els(_els, Text).

encode_sasl_success({sasl_success, Text},
		    _xmlns_attrs) ->
    _els = encode_sasl_success_cdata(Text, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"success">>, _attrs, _els}.

decode_sasl_success_cdata(<<>>) -> undefined;
decode_sasl_success_cdata(_val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"success">>,
			 <<"urn:ietf:params:xml:ns:xmpp-sasl">>}});
      _res -> _res
    end.

encode_sasl_success_cdata(undefined, _acc) -> _acc;
encode_sasl_success_cdata(_val, _acc) ->
    [{xmlcdata, base64:encode(_val)} | _acc].

decode_sasl_response({xmlel, <<"response">>, _attrs,
		      _els}) ->
    Text = decode_sasl_response_els(_els, <<>>),
    {sasl_response, Text}.

decode_sasl_response_els([], Text) ->
    decode_sasl_response_cdata(Text);
decode_sasl_response_els([{xmlcdata, _data} | _els],
			 Text) ->
    decode_sasl_response_els(_els,
			     <<Text/binary, _data/binary>>);
decode_sasl_response_els([_ | _els], Text) ->
    decode_sasl_response_els(_els, Text).

encode_sasl_response({sasl_response, Text},
		     _xmlns_attrs) ->
    _els = encode_sasl_response_cdata(Text, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"response">>, _attrs, _els}.

decode_sasl_response_cdata(<<>>) -> undefined;
decode_sasl_response_cdata(_val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"response">>,
			 <<"urn:ietf:params:xml:ns:xmpp-sasl">>}});
      _res -> _res
    end.

encode_sasl_response_cdata(undefined, _acc) -> _acc;
encode_sasl_response_cdata(_val, _acc) ->
    [{xmlcdata, base64:encode(_val)} | _acc].

decode_sasl_challenge({xmlel, <<"challenge">>, _attrs,
		       _els}) ->
    Text = decode_sasl_challenge_els(_els, <<>>),
    {sasl_challenge, Text}.

decode_sasl_challenge_els([], Text) ->
    decode_sasl_challenge_cdata(Text);
decode_sasl_challenge_els([{xmlcdata, _data} | _els],
			  Text) ->
    decode_sasl_challenge_els(_els,
			      <<Text/binary, _data/binary>>);
decode_sasl_challenge_els([_ | _els], Text) ->
    decode_sasl_challenge_els(_els, Text).

encode_sasl_challenge({sasl_challenge, Text},
		      _xmlns_attrs) ->
    _els = encode_sasl_challenge_cdata(Text, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"challenge">>, _attrs, _els}.

decode_sasl_challenge_cdata(<<>>) -> undefined;
decode_sasl_challenge_cdata(_val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"challenge">>,
			 <<"urn:ietf:params:xml:ns:xmpp-sasl">>}});
      _res -> _res
    end.

encode_sasl_challenge_cdata(undefined, _acc) -> _acc;
encode_sasl_challenge_cdata(_val, _acc) ->
    [{xmlcdata, base64:encode(_val)} | _acc].

decode_sasl_abort({xmlel, <<"abort">>, _attrs, _els}) ->
    {sasl_abort}.

encode_sasl_abort({sasl_abort}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"abort">>, _attrs, _els}.

decode_sasl_auth({xmlel, <<"auth">>, _attrs, _els}) ->
    Text = decode_sasl_auth_els(_els, <<>>),
    Mechanism = decode_sasl_auth_attrs(_attrs, undefined),
    {sasl_auth, Mechanism, Text}.

decode_sasl_auth_els([], Text) ->
    decode_sasl_auth_cdata(Text);
decode_sasl_auth_els([{xmlcdata, _data} | _els],
		     Text) ->
    decode_sasl_auth_els(_els,
			 <<Text/binary, _data/binary>>);
decode_sasl_auth_els([_ | _els], Text) ->
    decode_sasl_auth_els(_els, Text).

decode_sasl_auth_attrs([{<<"mechanism">>, _val}
			| _attrs],
		       _Mechanism) ->
    decode_sasl_auth_attrs(_attrs, _val);
decode_sasl_auth_attrs([_ | _attrs], Mechanism) ->
    decode_sasl_auth_attrs(_attrs, Mechanism);
decode_sasl_auth_attrs([], Mechanism) ->
    decode_sasl_auth_attr_mechanism(Mechanism).

encode_sasl_auth({sasl_auth, Mechanism, Text},
		 _xmlns_attrs) ->
    _els = encode_sasl_auth_cdata(Text, []),
    _attrs = encode_sasl_auth_attr_mechanism(Mechanism,
					     _xmlns_attrs),
    {xmlel, <<"auth">>, _attrs, _els}.

decode_sasl_auth_attr_mechanism(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"mechanism">>, <<"auth">>,
		   <<"urn:ietf:params:xml:ns:xmpp-sasl">>}});
decode_sasl_auth_attr_mechanism(_val) -> _val.

encode_sasl_auth_attr_mechanism(_val, _acc) ->
    [{<<"mechanism">>, _val} | _acc].

decode_sasl_auth_cdata(<<>>) -> undefined;
decode_sasl_auth_cdata(_val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"auth">>,
			 <<"urn:ietf:params:xml:ns:xmpp-sasl">>}});
      _res -> _res
    end.

encode_sasl_auth_cdata(undefined, _acc) -> _acc;
encode_sasl_auth_cdata(_val, _acc) ->
    [{xmlcdata, base64:encode(_val)} | _acc].

decode_bind({xmlel, <<"bind">>, _attrs, _els}) ->
    {Jid, Resource} = decode_bind_els(_els, undefined,
				      undefined),
    {bind, Jid, Resource}.

decode_bind_els([], Jid, Resource) -> {Jid, Resource};
decode_bind_els([{xmlel, <<"jid">>, _attrs, _} = _el
		 | _els],
		Jid, Resource) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-bind">> ->
	   decode_bind_els(_els, decode_bind_jid(_el), Resource);
       true -> decode_bind_els(_els, Jid, Resource)
    end;
decode_bind_els([{xmlel, <<"resource">>, _attrs, _} =
		     _el
		 | _els],
		Jid, Resource) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"urn:ietf:params:xml:ns:xmpp-bind">> ->
	   decode_bind_els(_els, Jid, decode_bind_resource(_el));
       true -> decode_bind_els(_els, Jid, Resource)
    end;
decode_bind_els([_ | _els], Jid, Resource) ->
    decode_bind_els(_els, Jid, Resource).

encode_bind({bind, Jid, Resource}, _xmlns_attrs) ->
    _els = 'encode_bind_$resource'(Resource,
				   'encode_bind_$jid'(Jid, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"bind">>, _attrs, _els}.

'encode_bind_$jid'(undefined, _acc) -> _acc;
'encode_bind_$jid'(Jid, _acc) ->
    [encode_bind_jid(Jid, []) | _acc].

'encode_bind_$resource'(undefined, _acc) -> _acc;
'encode_bind_$resource'(Resource, _acc) ->
    [encode_bind_resource(Resource, []) | _acc].

decode_bind_resource({xmlel, <<"resource">>, _attrs,
		      _els}) ->
    Cdata = decode_bind_resource_els(_els, <<>>), Cdata.

decode_bind_resource_els([], Cdata) ->
    decode_bind_resource_cdata(Cdata);
decode_bind_resource_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_bind_resource_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_bind_resource_els([_ | _els], Cdata) ->
    decode_bind_resource_els(_els, Cdata).

encode_bind_resource(Cdata, _xmlns_attrs) ->
    _els = encode_bind_resource_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"resource">>, _attrs, _els}.

decode_bind_resource_cdata(<<>>) -> undefined;
decode_bind_resource_cdata(_val) ->
    case catch resourceprep(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"resource">>,
			 <<"urn:ietf:params:xml:ns:xmpp-bind">>}});
      _res -> _res
    end.

encode_bind_resource_cdata(undefined, _acc) -> _acc;
encode_bind_resource_cdata(_val, _acc) ->
    [{xmlcdata, resourceprep(_val)} | _acc].

decode_bind_jid({xmlel, <<"jid">>, _attrs, _els}) ->
    Cdata = decode_bind_jid_els(_els, <<>>), Cdata.

decode_bind_jid_els([], Cdata) ->
    decode_bind_jid_cdata(Cdata);
decode_bind_jid_els([{xmlcdata, _data} | _els],
		    Cdata) ->
    decode_bind_jid_els(_els,
			<<Cdata/binary, _data/binary>>);
decode_bind_jid_els([_ | _els], Cdata) ->
    decode_bind_jid_els(_els, Cdata).

encode_bind_jid(Cdata, _xmlns_attrs) ->
    _els = encode_bind_jid_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"jid">>, _attrs, _els}.

decode_bind_jid_cdata(<<>>) -> undefined;
decode_bind_jid_cdata(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"jid">>,
			 <<"urn:ietf:params:xml:ns:xmpp-bind">>}});
      _res -> _res
    end.

encode_bind_jid_cdata(undefined, _acc) -> _acc;
encode_bind_jid_cdata(_val, _acc) ->
    [{xmlcdata, enc_jid(_val)} | _acc].

decode_error({xmlel, <<"error">>, _attrs, _els}) ->
    {Text, Reason} = decode_error_els(_els, undefined,
				      undefined),
    {Type, By} = decode_error_attrs(_attrs, undefined,
				    undefined),
    {error, Type, By, Reason, Text}.

decode_error_els([], Text, Reason) -> {Text, Reason};
decode_error_els([{xmlel, <<"text">>, _attrs, _} = _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, decode_error_text(_el), Reason);
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"bad-request">>, _attrs,
		   _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_bad_request(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"conflict">>, _attrs, _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_conflict(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"feature-not-implemented">>,
		   _attrs, _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_feature_not_implemented(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"forbidden">>, _attrs, _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_forbidden(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"gone">>, _attrs, _} = _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text, decode_error_gone(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"internal-server-error">>,
		   _attrs, _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_internal_server_error(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"item-not-found">>, _attrs,
		   _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_item_not_found(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"jid-malformed">>, _attrs,
		   _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_jid_malformed(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"not-acceptable">>, _attrs,
		   _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_not_acceptable(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"not-allowed">>, _attrs,
		   _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_not_allowed(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"not-authorized">>, _attrs,
		   _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_not_authorized(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"policy-violation">>,
		   _attrs, _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_policy_violation(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"recipient-unavailable">>,
		   _attrs, _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_recipient_unavailable(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"redirect">>, _attrs, _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_redirect(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"registration-required">>,
		   _attrs, _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_registration_required(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"remote-server-not-found">>,
		   _attrs, _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_remote_server_not_found(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"remote-server-timeout">>,
		   _attrs, _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_remote_server_timeout(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"resource-constraint">>,
		   _attrs, _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_resource_constraint(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"service-unavailable">>,
		   _attrs, _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_service_unavailable(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"subscription-required">>,
		   _attrs, _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_subscription_required(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"undefined-condition">>,
		   _attrs, _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_undefined_condition(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([{xmlel, <<"unexpected-request">>,
		   _attrs, _} =
		      _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(_els, Text,
			    decode_error_unexpected_request(_el));
       true -> decode_error_els(_els, Text, Reason)
    end;
decode_error_els([_ | _els], Text, Reason) ->
    decode_error_els(_els, Text, Reason).

decode_error_attrs([{<<"type">>, _val} | _attrs], _Type,
		   By) ->
    decode_error_attrs(_attrs, _val, By);
decode_error_attrs([{<<"by">>, _val} | _attrs], Type,
		   _By) ->
    decode_error_attrs(_attrs, Type, _val);
decode_error_attrs([_ | _attrs], Type, By) ->
    decode_error_attrs(_attrs, Type, By);
decode_error_attrs([], Type, By) ->
    {decode_error_attr_type(Type),
     decode_error_attr_by(By)}.

encode_error({error, Type, By, Reason, Text},
	     _xmlns_attrs) ->
    _els = 'encode_error_$reason'(Reason,
				  'encode_error_$text'(Text, [])),
    _attrs = encode_error_attr_by(By,
				  encode_error_attr_type(Type, _xmlns_attrs)),
    {xmlel, <<"error">>, _attrs, _els}.

'encode_error_$text'(undefined, _acc) -> _acc;
'encode_error_$text'(Text, _acc) ->
    [encode_error_text(Text,
		       [{<<"xmlns">>,
			 <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc].

'encode_error_$reason'(undefined, _acc) -> _acc;
'encode_error_$reason'('bad-request' = Reason, _acc) ->
    [encode_error_bad_request(Reason,
			      [{<<"xmlns">>,
				<<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'(conflict = Reason, _acc) ->
    [encode_error_conflict(Reason,
			   [{<<"xmlns">>,
			     <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('feature-not-implemented' =
			   Reason,
		       _acc) ->
    [encode_error_feature_not_implemented(Reason,
					  [{<<"xmlns">>,
					    <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'(forbidden = Reason, _acc) ->
    [encode_error_forbidden(Reason,
			    [{<<"xmlns">>,
			      <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'({gone, _} = Reason, _acc) ->
    [encode_error_gone(Reason,
		       [{<<"xmlns">>,
			 <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('internal-server-error' = Reason,
		       _acc) ->
    [encode_error_internal_server_error(Reason,
					[{<<"xmlns">>,
					  <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('item-not-found' = Reason,
		       _acc) ->
    [encode_error_item_not_found(Reason,
				 [{<<"xmlns">>,
				   <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('jid-malformed' = Reason,
		       _acc) ->
    [encode_error_jid_malformed(Reason,
				[{<<"xmlns">>,
				  <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('not-acceptable' = Reason,
		       _acc) ->
    [encode_error_not_acceptable(Reason,
				 [{<<"xmlns">>,
				   <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('not-allowed' = Reason, _acc) ->
    [encode_error_not_allowed(Reason,
			      [{<<"xmlns">>,
				<<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('not-authorized' = Reason,
		       _acc) ->
    [encode_error_not_authorized(Reason,
				 [{<<"xmlns">>,
				   <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('policy-violation' = Reason,
		       _acc) ->
    [encode_error_policy_violation(Reason,
				   [{<<"xmlns">>,
				     <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('recipient-unavailable' = Reason,
		       _acc) ->
    [encode_error_recipient_unavailable(Reason,
					[{<<"xmlns">>,
					  <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'({redirect, _} = Reason, _acc) ->
    [encode_error_redirect(Reason,
			   [{<<"xmlns">>,
			     <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('registration-required' = Reason,
		       _acc) ->
    [encode_error_registration_required(Reason,
					[{<<"xmlns">>,
					  <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('remote-server-not-found' =
			   Reason,
		       _acc) ->
    [encode_error_remote_server_not_found(Reason,
					  [{<<"xmlns">>,
					    <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('remote-server-timeout' = Reason,
		       _acc) ->
    [encode_error_remote_server_timeout(Reason,
					[{<<"xmlns">>,
					  <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('resource-constraint' = Reason,
		       _acc) ->
    [encode_error_resource_constraint(Reason,
				      [{<<"xmlns">>,
					<<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('service-unavailable' = Reason,
		       _acc) ->
    [encode_error_service_unavailable(Reason,
				      [{<<"xmlns">>,
					<<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('subscription-required' = Reason,
		       _acc) ->
    [encode_error_subscription_required(Reason,
					[{<<"xmlns">>,
					  <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('undefined-condition' = Reason,
		       _acc) ->
    [encode_error_undefined_condition(Reason,
				      [{<<"xmlns">>,
					<<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_error_$reason'('unexpected-request' = Reason,
		       _acc) ->
    [encode_error_unexpected_request(Reason,
				     [{<<"xmlns">>,
				       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc].

decode_error_attr_type(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"type">>, <<"error">>,
		   <<"jabber:client">>}});
decode_error_attr_type(_val) ->
    case catch dec_enum(_val,
			[auth, cancel, continue, modify, wait])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"type">>, <<"error">>,
			 <<"jabber:client">>}});
      _res -> _res
    end.

encode_error_attr_type(_val, _acc) ->
    [{<<"type">>, enc_enum(_val)} | _acc].

decode_error_attr_by(undefined) -> undefined;
decode_error_attr_by(_val) -> _val.

encode_error_attr_by(undefined, _acc) -> _acc;
encode_error_attr_by(_val, _acc) ->
    [{<<"by">>, _val} | _acc].

decode_error_text({xmlel, <<"text">>, _attrs, _els}) ->
    Data = decode_error_text_els(_els, <<>>),
    Lang = decode_error_text_attrs(_attrs, undefined),
    {text, Lang, Data}.

decode_error_text_els([], Data) ->
    decode_error_text_cdata(Data);
decode_error_text_els([{xmlcdata, _data} | _els],
		      Data) ->
    decode_error_text_els(_els,
			  <<Data/binary, _data/binary>>);
decode_error_text_els([_ | _els], Data) ->
    decode_error_text_els(_els, Data).

decode_error_text_attrs([{<<"xml:lang">>, _val}
			 | _attrs],
			_Lang) ->
    decode_error_text_attrs(_attrs, _val);
decode_error_text_attrs([_ | _attrs], Lang) ->
    decode_error_text_attrs(_attrs, Lang);
decode_error_text_attrs([], Lang) ->
    'decode_error_text_attr_xml:lang'(Lang).

encode_error_text({text, Lang, Data}, _xmlns_attrs) ->
    _els = encode_error_text_cdata(Data, []),
    _attrs = 'encode_error_text_attr_xml:lang'(Lang,
					       _xmlns_attrs),
    {xmlel, <<"text">>, _attrs, _els}.

'decode_error_text_attr_xml:lang'(undefined) ->
    undefined;
'decode_error_text_attr_xml:lang'(_val) -> _val.

'encode_error_text_attr_xml:lang'(undefined, _acc) ->
    _acc;
'encode_error_text_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_error_text_cdata(<<>>) -> undefined;
decode_error_text_cdata(_val) -> _val.

encode_error_text_cdata(undefined, _acc) -> _acc;
encode_error_text_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_error_unexpected_request({xmlel,
				 <<"unexpected-request">>, _attrs, _els}) ->
    'unexpected-request'.

encode_error_unexpected_request('unexpected-request',
				_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"unexpected-request">>, _attrs, _els}.

decode_error_undefined_condition({xmlel,
				  <<"undefined-condition">>, _attrs, _els}) ->
    'undefined-condition'.

encode_error_undefined_condition('undefined-condition',
				 _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"undefined-condition">>, _attrs, _els}.

decode_error_subscription_required({xmlel,
				    <<"subscription-required">>, _attrs,
				    _els}) ->
    'subscription-required'.

encode_error_subscription_required('subscription-required',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"subscription-required">>, _attrs, _els}.

decode_error_service_unavailable({xmlel,
				  <<"service-unavailable">>, _attrs, _els}) ->
    'service-unavailable'.

encode_error_service_unavailable('service-unavailable',
				 _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"service-unavailable">>, _attrs, _els}.

decode_error_resource_constraint({xmlel,
				  <<"resource-constraint">>, _attrs, _els}) ->
    'resource-constraint'.

encode_error_resource_constraint('resource-constraint',
				 _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"resource-constraint">>, _attrs, _els}.

decode_error_remote_server_timeout({xmlel,
				    <<"remote-server-timeout">>, _attrs,
				    _els}) ->
    'remote-server-timeout'.

encode_error_remote_server_timeout('remote-server-timeout',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"remote-server-timeout">>, _attrs, _els}.

decode_error_remote_server_not_found({xmlel,
				      <<"remote-server-not-found">>, _attrs,
				      _els}) ->
    'remote-server-not-found'.

encode_error_remote_server_not_found('remote-server-not-found',
				     _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"remote-server-not-found">>, _attrs, _els}.

decode_error_registration_required({xmlel,
				    <<"registration-required">>, _attrs,
				    _els}) ->
    'registration-required'.

encode_error_registration_required('registration-required',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"registration-required">>, _attrs, _els}.

decode_error_redirect({xmlel, <<"redirect">>, _attrs,
		       _els}) ->
    Uri = decode_error_redirect_els(_els, <<>>),
    {redirect, Uri}.

decode_error_redirect_els([], Uri) ->
    decode_error_redirect_cdata(Uri);
decode_error_redirect_els([{xmlcdata, _data} | _els],
			  Uri) ->
    decode_error_redirect_els(_els,
			      <<Uri/binary, _data/binary>>);
decode_error_redirect_els([_ | _els], Uri) ->
    decode_error_redirect_els(_els, Uri).

encode_error_redirect({redirect, Uri}, _xmlns_attrs) ->
    _els = encode_error_redirect_cdata(Uri, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"redirect">>, _attrs, _els}.

decode_error_redirect_cdata(<<>>) -> undefined;
decode_error_redirect_cdata(_val) -> _val.

encode_error_redirect_cdata(undefined, _acc) -> _acc;
encode_error_redirect_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_error_recipient_unavailable({xmlel,
				    <<"recipient-unavailable">>, _attrs,
				    _els}) ->
    'recipient-unavailable'.

encode_error_recipient_unavailable('recipient-unavailable',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"recipient-unavailable">>, _attrs, _els}.

decode_error_policy_violation({xmlel,
			       <<"policy-violation">>, _attrs, _els}) ->
    'policy-violation'.

encode_error_policy_violation('policy-violation',
			      _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"policy-violation">>, _attrs, _els}.

decode_error_not_authorized({xmlel,
			     <<"not-authorized">>, _attrs, _els}) ->
    'not-authorized'.

encode_error_not_authorized('not-authorized',
			    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"not-authorized">>, _attrs, _els}.

decode_error_not_allowed({xmlel, <<"not-allowed">>,
			  _attrs, _els}) ->
    'not-allowed'.

encode_error_not_allowed('not-allowed', _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"not-allowed">>, _attrs, _els}.

decode_error_not_acceptable({xmlel,
			     <<"not-acceptable">>, _attrs, _els}) ->
    'not-acceptable'.

encode_error_not_acceptable('not-acceptable',
			    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"not-acceptable">>, _attrs, _els}.

decode_error_jid_malformed({xmlel, <<"jid-malformed">>,
			    _attrs, _els}) ->
    'jid-malformed'.

encode_error_jid_malformed('jid-malformed',
			   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"jid-malformed">>, _attrs, _els}.

decode_error_item_not_found({xmlel,
			     <<"item-not-found">>, _attrs, _els}) ->
    'item-not-found'.

encode_error_item_not_found('item-not-found',
			    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"item-not-found">>, _attrs, _els}.

decode_error_internal_server_error({xmlel,
				    <<"internal-server-error">>, _attrs,
				    _els}) ->
    'internal-server-error'.

encode_error_internal_server_error('internal-server-error',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"internal-server-error">>, _attrs, _els}.

decode_error_gone({xmlel, <<"gone">>, _attrs, _els}) ->
    Uri = decode_error_gone_els(_els, <<>>), {gone, Uri}.

decode_error_gone_els([], Uri) ->
    decode_error_gone_cdata(Uri);
decode_error_gone_els([{xmlcdata, _data} | _els],
		      Uri) ->
    decode_error_gone_els(_els,
			  <<Uri/binary, _data/binary>>);
decode_error_gone_els([_ | _els], Uri) ->
    decode_error_gone_els(_els, Uri).

encode_error_gone({gone, Uri}, _xmlns_attrs) ->
    _els = encode_error_gone_cdata(Uri, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"gone">>, _attrs, _els}.

decode_error_gone_cdata(<<>>) -> undefined;
decode_error_gone_cdata(_val) -> _val.

encode_error_gone_cdata(undefined, _acc) -> _acc;
encode_error_gone_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_error_forbidden({xmlel, <<"forbidden">>, _attrs,
			_els}) ->
    forbidden.

encode_error_forbidden(forbidden, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"forbidden">>, _attrs, _els}.

decode_error_feature_not_implemented({xmlel,
				      <<"feature-not-implemented">>, _attrs,
				      _els}) ->
    'feature-not-implemented'.

encode_error_feature_not_implemented('feature-not-implemented',
				     _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"feature-not-implemented">>, _attrs, _els}.

decode_error_conflict({xmlel, <<"conflict">>, _attrs,
		       _els}) ->
    conflict.

encode_error_conflict(conflict, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"conflict">>, _attrs, _els}.

decode_error_bad_request({xmlel, <<"bad-request">>,
			  _attrs, _els}) ->
    'bad-request'.

encode_error_bad_request('bad-request', _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"bad-request">>, _attrs, _els}.

decode_presence({xmlel, <<"presence">>, _attrs,
		 _els}) ->
    {Error, Status, Show, Priority, __Els} =
	decode_presence_els(_els, undefined, [], undefined,
			    undefined, []),
    {Id, Type, From, To, Lang} =
	decode_presence_attrs(_attrs, undefined, undefined,
			      undefined, undefined, undefined),
    {presence, Id, Type, Lang, From, To, Show, Status,
     Priority, Error, __Els}.

decode_presence_els([], Error, Status, Show, Priority,
		    __Els) ->
    {Error, lists:reverse(Status), Show, Priority,
     lists:reverse(__Els)};
decode_presence_els([{xmlel, <<"error">>, _attrs, _} =
			 _el
		     | _els],
		    Error, Status, Show, Priority, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:client">> ->
	   decode_presence_els(_els, decode_error(_el), Status,
			       Show, Priority, __Els);
       true ->
	   decode_presence_els(_els, Error, Status, Show, Priority,
			       __Els)
    end;
decode_presence_els([{xmlel, <<"show">>, _attrs, _} =
			 _el
		     | _els],
		    Error, Status, Show, Priority, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:client">> ->
	   decode_presence_els(_els, Error, Status,
			       decode_presence_show(_el), Priority, __Els);
       true ->
	   decode_presence_els(_els, Error, Status, Show, Priority,
			       __Els)
    end;
decode_presence_els([{xmlel, <<"status">>, _attrs, _} =
			 _el
		     | _els],
		    Error, Status, Show, Priority, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:client">> ->
	   decode_presence_els(_els, Error,
			       [decode_presence_status(_el) | Status], Show,
			       Priority, __Els);
       true ->
	   decode_presence_els(_els, Error, Status, Show, Priority,
			       __Els)
    end;
decode_presence_els([{xmlel, <<"priority">>, _attrs,
		      _} =
			 _el
		     | _els],
		    Error, Status, Show, Priority, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:client">> ->
	   decode_presence_els(_els, Error, Status, Show,
			       decode_presence_priority(_el), __Els);
       true ->
	   decode_presence_els(_els, Error, Status, Show, Priority,
			       __Els)
    end;
decode_presence_els([{xmlel, _, _, _} = _el | _els],
		    Error, Status, Show, Priority, __Els) ->
    case is_known_tag(_el) of
      true ->
	  decode_presence_els(_els, Error, Status, Show, Priority,
			      [decode(_el) | __Els]);
      false ->
	  decode_presence_els(_els, Error, Status, Show, Priority,
			      __Els)
    end;
decode_presence_els([_ | _els], Error, Status, Show,
		    Priority, __Els) ->
    decode_presence_els(_els, Error, Status, Show, Priority,
			__Els).

decode_presence_attrs([{<<"id">>, _val} | _attrs], _Id,
		      Type, From, To, Lang) ->
    decode_presence_attrs(_attrs, _val, Type, From, To,
			  Lang);
decode_presence_attrs([{<<"type">>, _val} | _attrs], Id,
		      _Type, From, To, Lang) ->
    decode_presence_attrs(_attrs, Id, _val, From, To, Lang);
decode_presence_attrs([{<<"from">>, _val} | _attrs], Id,
		      Type, _From, To, Lang) ->
    decode_presence_attrs(_attrs, Id, Type, _val, To, Lang);
decode_presence_attrs([{<<"to">>, _val} | _attrs], Id,
		      Type, From, _To, Lang) ->
    decode_presence_attrs(_attrs, Id, Type, From, _val,
			  Lang);
decode_presence_attrs([{<<"xml:lang">>, _val} | _attrs],
		      Id, Type, From, To, _Lang) ->
    decode_presence_attrs(_attrs, Id, Type, From, To, _val);
decode_presence_attrs([_ | _attrs], Id, Type, From, To,
		      Lang) ->
    decode_presence_attrs(_attrs, Id, Type, From, To, Lang);
decode_presence_attrs([], Id, Type, From, To, Lang) ->
    {decode_presence_attr_id(Id),
     decode_presence_attr_type(Type),
     decode_presence_attr_from(From),
     decode_presence_attr_to(To),
     'decode_presence_attr_xml:lang'(Lang)}.

encode_presence({presence, Id, Type, Lang, From, To,
		 Show, Status, Priority, Error, __Els},
		_xmlns_attrs) ->
    _els = 'encode_presence_$priority'(Priority,
				       'encode_presence_$show'(Show,
							       'encode_presence_$status'(Status,
											 'encode_presence_$error'(Error,
														  [encode(_el)
														   || _el
															  <- __Els])))),
    _attrs = 'encode_presence_attr_xml:lang'(Lang,
					     encode_presence_attr_to(To,
								     encode_presence_attr_from(From,
											       encode_presence_attr_type(Type,
															 encode_presence_attr_id(Id,
																		 _xmlns_attrs))))),
    {xmlel, <<"presence">>, _attrs, _els}.

'encode_presence_$error'(undefined, _acc) -> _acc;
'encode_presence_$error'(Error, _acc) ->
    [encode_error(Error, []) | _acc].

'encode_presence_$status'([], _acc) -> _acc;
'encode_presence_$status'([Status | _els], _acc) ->
    'encode_presence_$status'(_els,
			      [encode_presence_status(Status, []) | _acc]).

'encode_presence_$show'(undefined, _acc) -> _acc;
'encode_presence_$show'(Show, _acc) ->
    [encode_presence_show(Show, []) | _acc].

'encode_presence_$priority'(undefined, _acc) -> _acc;
'encode_presence_$priority'(Priority, _acc) ->
    [encode_presence_priority(Priority, []) | _acc].

decode_presence_attr_id(undefined) -> undefined;
decode_presence_attr_id(_val) -> _val.

encode_presence_attr_id(undefined, _acc) -> _acc;
encode_presence_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_presence_attr_type(undefined) -> undefined;
decode_presence_attr_type(_val) ->
    case catch dec_enum(_val,
			[unavailable, subscribe, subscribed, unsubscribe,
			 unsubscribed, probe, error])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"type">>, <<"presence">>,
			 <<"jabber:client">>}});
      _res -> _res
    end.

encode_presence_attr_type(undefined, _acc) -> _acc;
encode_presence_attr_type(_val, _acc) ->
    [{<<"type">>, enc_enum(_val)} | _acc].

decode_presence_attr_from(undefined) -> undefined;
decode_presence_attr_from(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"from">>, <<"presence">>,
			 <<"jabber:client">>}});
      _res -> _res
    end.

encode_presence_attr_from(undefined, _acc) -> _acc;
encode_presence_attr_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_presence_attr_to(undefined) -> undefined;
decode_presence_attr_to(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"to">>, <<"presence">>,
			 <<"jabber:client">>}});
      _res -> _res
    end.

encode_presence_attr_to(undefined, _acc) -> _acc;
encode_presence_attr_to(_val, _acc) ->
    [{<<"to">>, enc_jid(_val)} | _acc].

'decode_presence_attr_xml:lang'(undefined) -> undefined;
'decode_presence_attr_xml:lang'(_val) -> _val.

'encode_presence_attr_xml:lang'(undefined, _acc) ->
    _acc;
'encode_presence_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_presence_priority({xmlel, <<"priority">>, _attrs,
			  _els}) ->
    Cdata = decode_presence_priority_els(_els, <<>>), Cdata.

decode_presence_priority_els([], Cdata) ->
    decode_presence_priority_cdata(Cdata);
decode_presence_priority_els([{xmlcdata, _data} | _els],
			     Cdata) ->
    decode_presence_priority_els(_els,
				 <<Cdata/binary, _data/binary>>);
decode_presence_priority_els([_ | _els], Cdata) ->
    decode_presence_priority_els(_els, Cdata).

encode_presence_priority(Cdata, _xmlns_attrs) ->
    _els = encode_presence_priority_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"priority">>, _attrs, _els}.

decode_presence_priority_cdata(<<>>) -> undefined;
decode_presence_priority_cdata(_val) ->
    case catch dec_int(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"priority">>,
			 <<"jabber:client">>}});
      _res -> _res
    end.

encode_presence_priority_cdata(undefined, _acc) -> _acc;
encode_presence_priority_cdata(_val, _acc) ->
    [{xmlcdata, enc_int(_val)} | _acc].

decode_presence_status({xmlel, <<"status">>, _attrs,
			_els}) ->
    Data = decode_presence_status_els(_els, <<>>),
    Lang = decode_presence_status_attrs(_attrs, undefined),
    {text, Lang, Data}.

decode_presence_status_els([], Data) ->
    decode_presence_status_cdata(Data);
decode_presence_status_els([{xmlcdata, _data} | _els],
			   Data) ->
    decode_presence_status_els(_els,
			       <<Data/binary, _data/binary>>);
decode_presence_status_els([_ | _els], Data) ->
    decode_presence_status_els(_els, Data).

decode_presence_status_attrs([{<<"xml:lang">>, _val}
			      | _attrs],
			     _Lang) ->
    decode_presence_status_attrs(_attrs, _val);
decode_presence_status_attrs([_ | _attrs], Lang) ->
    decode_presence_status_attrs(_attrs, Lang);
decode_presence_status_attrs([], Lang) ->
    'decode_presence_status_attr_xml:lang'(Lang).

encode_presence_status({text, Lang, Data},
		       _xmlns_attrs) ->
    _els = encode_presence_status_cdata(Data, []),
    _attrs = 'encode_presence_status_attr_xml:lang'(Lang,
						    _xmlns_attrs),
    {xmlel, <<"status">>, _attrs, _els}.

'decode_presence_status_attr_xml:lang'(undefined) ->
    undefined;
'decode_presence_status_attr_xml:lang'(_val) -> _val.

'encode_presence_status_attr_xml:lang'(undefined,
				       _acc) ->
    _acc;
'encode_presence_status_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_presence_status_cdata(<<>>) -> undefined;
decode_presence_status_cdata(_val) -> _val.

encode_presence_status_cdata(undefined, _acc) -> _acc;
encode_presence_status_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_presence_show({xmlel, <<"show">>, _attrs,
		      _els}) ->
    Cdata = decode_presence_show_els(_els, <<>>), Cdata.

decode_presence_show_els([], Cdata) ->
    decode_presence_show_cdata(Cdata);
decode_presence_show_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_presence_show_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_presence_show_els([_ | _els], Cdata) ->
    decode_presence_show_els(_els, Cdata).

encode_presence_show(Cdata, _xmlns_attrs) ->
    _els = encode_presence_show_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"show">>, _attrs, _els}.

decode_presence_show_cdata(<<>>) -> undefined;
decode_presence_show_cdata(_val) ->
    case catch dec_enum(_val, [away, chat, dnd, xa]) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"show">>,
			 <<"jabber:client">>}});
      _res -> _res
    end.

encode_presence_show_cdata(undefined, _acc) -> _acc;
encode_presence_show_cdata(_val, _acc) ->
    [{xmlcdata, enc_enum(_val)} | _acc].

decode_message({xmlel, <<"message">>, _attrs, _els}) ->
    {Error, Thread, Subject, Body, __Els} =
	decode_message_els(_els, undefined, undefined, [], [],
			   []),
    {Id, Type, From, To, Lang} =
	decode_message_attrs(_attrs, undefined, undefined,
			     undefined, undefined, undefined),
    {message, Id, Type, Lang, From, To, Subject, Body,
     Thread, Error, __Els}.

decode_message_els([], Error, Thread, Subject, Body,
		   __Els) ->
    {Error, Thread, lists:reverse(Subject),
     lists:reverse(Body), lists:reverse(__Els)};
decode_message_els([{xmlel, <<"error">>, _attrs, _} =
			_el
		    | _els],
		   Error, Thread, Subject, Body, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:client">> ->
	   decode_message_els(_els, decode_error(_el), Thread,
			      Subject, Body, __Els);
       true ->
	   decode_message_els(_els, Error, Thread, Subject, Body,
			      __Els)
    end;
decode_message_els([{xmlel, <<"subject">>, _attrs, _} =
			_el
		    | _els],
		   Error, Thread, Subject, Body, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:client">> ->
	   decode_message_els(_els, Error, Thread,
			      [decode_message_subject(_el) | Subject], Body,
			      __Els);
       true ->
	   decode_message_els(_els, Error, Thread, Subject, Body,
			      __Els)
    end;
decode_message_els([{xmlel, <<"thread">>, _attrs, _} =
			_el
		    | _els],
		   Error, Thread, Subject, Body, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:client">> ->
	   decode_message_els(_els, Error,
			      decode_message_thread(_el), Subject, Body, __Els);
       true ->
	   decode_message_els(_els, Error, Thread, Subject, Body,
			      __Els)
    end;
decode_message_els([{xmlel, <<"body">>, _attrs, _} = _el
		    | _els],
		   Error, Thread, Subject, Body, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:client">> ->
	   decode_message_els(_els, Error, Thread, Subject,
			      [decode_message_body(_el) | Body], __Els);
       true ->
	   decode_message_els(_els, Error, Thread, Subject, Body,
			      __Els)
    end;
decode_message_els([{xmlel, _, _, _} = _el | _els],
		   Error, Thread, Subject, Body, __Els) ->
    case is_known_tag(_el) of
      true ->
	  decode_message_els(_els, Error, Thread, Subject, Body,
			     [decode(_el) | __Els]);
      false ->
	  decode_message_els(_els, Error, Thread, Subject, Body,
			     __Els)
    end;
decode_message_els([_ | _els], Error, Thread, Subject,
		   Body, __Els) ->
    decode_message_els(_els, Error, Thread, Subject, Body,
		       __Els).

decode_message_attrs([{<<"id">>, _val} | _attrs], _Id,
		     Type, From, To, Lang) ->
    decode_message_attrs(_attrs, _val, Type, From, To,
			 Lang);
decode_message_attrs([{<<"type">>, _val} | _attrs], Id,
		     _Type, From, To, Lang) ->
    decode_message_attrs(_attrs, Id, _val, From, To, Lang);
decode_message_attrs([{<<"from">>, _val} | _attrs], Id,
		     Type, _From, To, Lang) ->
    decode_message_attrs(_attrs, Id, Type, _val, To, Lang);
decode_message_attrs([{<<"to">>, _val} | _attrs], Id,
		     Type, From, _To, Lang) ->
    decode_message_attrs(_attrs, Id, Type, From, _val,
			 Lang);
decode_message_attrs([{<<"xml:lang">>, _val} | _attrs],
		     Id, Type, From, To, _Lang) ->
    decode_message_attrs(_attrs, Id, Type, From, To, _val);
decode_message_attrs([_ | _attrs], Id, Type, From, To,
		     Lang) ->
    decode_message_attrs(_attrs, Id, Type, From, To, Lang);
decode_message_attrs([], Id, Type, From, To, Lang) ->
    {decode_message_attr_id(Id),
     decode_message_attr_type(Type),
     decode_message_attr_from(From),
     decode_message_attr_to(To),
     'decode_message_attr_xml:lang'(Lang)}.

encode_message({message, Id, Type, Lang, From, To,
		Subject, Body, Thread, Error, __Els},
	       _xmlns_attrs) ->
    _els = 'encode_message_$body'(Body,
				  'encode_message_$subject'(Subject,
							    'encode_message_$thread'(Thread,
										     'encode_message_$error'(Error,
													     [encode(_el)
													      || _el
														     <- __Els])))),
    _attrs = 'encode_message_attr_xml:lang'(Lang,
					    encode_message_attr_to(To,
								   encode_message_attr_from(From,
											    encode_message_attr_type(Type,
														     encode_message_attr_id(Id,
																	    _xmlns_attrs))))),
    {xmlel, <<"message">>, _attrs, _els}.

'encode_message_$error'(undefined, _acc) -> _acc;
'encode_message_$error'(Error, _acc) ->
    [encode_error(Error, []) | _acc].

'encode_message_$thread'(undefined, _acc) -> _acc;
'encode_message_$thread'(Thread, _acc) ->
    [encode_message_thread(Thread, []) | _acc].

'encode_message_$subject'([], _acc) -> _acc;
'encode_message_$subject'([Subject | _els], _acc) ->
    'encode_message_$subject'(_els,
			      [encode_message_subject(Subject, []) | _acc]).

'encode_message_$body'([], _acc) -> _acc;
'encode_message_$body'([Body | _els], _acc) ->
    'encode_message_$body'(_els,
			   [encode_message_body(Body, []) | _acc]).

decode_message_attr_id(undefined) -> undefined;
decode_message_attr_id(_val) -> _val.

encode_message_attr_id(undefined, _acc) -> _acc;
encode_message_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_message_attr_type(undefined) -> normal;
decode_message_attr_type(_val) ->
    case catch dec_enum(_val,
			[chat, normal, groupchat, headline, error])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"type">>, <<"message">>,
			 <<"jabber:client">>}});
      _res -> _res
    end.

encode_message_attr_type(normal, _acc) -> _acc;
encode_message_attr_type(_val, _acc) ->
    [{<<"type">>, enc_enum(_val)} | _acc].

decode_message_attr_from(undefined) -> undefined;
decode_message_attr_from(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"from">>, <<"message">>,
			 <<"jabber:client">>}});
      _res -> _res
    end.

encode_message_attr_from(undefined, _acc) -> _acc;
encode_message_attr_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_message_attr_to(undefined) -> undefined;
decode_message_attr_to(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"to">>, <<"message">>,
			 <<"jabber:client">>}});
      _res -> _res
    end.

encode_message_attr_to(undefined, _acc) -> _acc;
encode_message_attr_to(_val, _acc) ->
    [{<<"to">>, enc_jid(_val)} | _acc].

'decode_message_attr_xml:lang'(undefined) -> undefined;
'decode_message_attr_xml:lang'(_val) -> _val.

'encode_message_attr_xml:lang'(undefined, _acc) -> _acc;
'encode_message_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_message_thread({xmlel, <<"thread">>, _attrs,
		       _els}) ->
    Cdata = decode_message_thread_els(_els, <<>>), Cdata.

decode_message_thread_els([], Cdata) ->
    decode_message_thread_cdata(Cdata);
decode_message_thread_els([{xmlcdata, _data} | _els],
			  Cdata) ->
    decode_message_thread_els(_els,
			      <<Cdata/binary, _data/binary>>);
decode_message_thread_els([_ | _els], Cdata) ->
    decode_message_thread_els(_els, Cdata).

encode_message_thread(Cdata, _xmlns_attrs) ->
    _els = encode_message_thread_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"thread">>, _attrs, _els}.

decode_message_thread_cdata(<<>>) -> undefined;
decode_message_thread_cdata(_val) -> _val.

encode_message_thread_cdata(undefined, _acc) -> _acc;
encode_message_thread_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_message_body({xmlel, <<"body">>, _attrs,
		     _els}) ->
    Data = decode_message_body_els(_els, <<>>),
    Lang = decode_message_body_attrs(_attrs, undefined),
    {text, Lang, Data}.

decode_message_body_els([], Data) ->
    decode_message_body_cdata(Data);
decode_message_body_els([{xmlcdata, _data} | _els],
			Data) ->
    decode_message_body_els(_els,
			    <<Data/binary, _data/binary>>);
decode_message_body_els([_ | _els], Data) ->
    decode_message_body_els(_els, Data).

decode_message_body_attrs([{<<"xml:lang">>, _val}
			   | _attrs],
			  _Lang) ->
    decode_message_body_attrs(_attrs, _val);
decode_message_body_attrs([_ | _attrs], Lang) ->
    decode_message_body_attrs(_attrs, Lang);
decode_message_body_attrs([], Lang) ->
    'decode_message_body_attr_xml:lang'(Lang).

encode_message_body({text, Lang, Data}, _xmlns_attrs) ->
    _els = encode_message_body_cdata(Data, []),
    _attrs = 'encode_message_body_attr_xml:lang'(Lang,
						 _xmlns_attrs),
    {xmlel, <<"body">>, _attrs, _els}.

'decode_message_body_attr_xml:lang'(undefined) ->
    undefined;
'decode_message_body_attr_xml:lang'(_val) -> _val.

'encode_message_body_attr_xml:lang'(undefined, _acc) ->
    _acc;
'encode_message_body_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_message_body_cdata(<<>>) -> undefined;
decode_message_body_cdata(_val) -> _val.

encode_message_body_cdata(undefined, _acc) -> _acc;
encode_message_body_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_message_subject({xmlel, <<"subject">>, _attrs,
			_els}) ->
    Data = decode_message_subject_els(_els, <<>>),
    Lang = decode_message_subject_attrs(_attrs, undefined),
    {text, Lang, Data}.

decode_message_subject_els([], Data) ->
    decode_message_subject_cdata(Data);
decode_message_subject_els([{xmlcdata, _data} | _els],
			   Data) ->
    decode_message_subject_els(_els,
			       <<Data/binary, _data/binary>>);
decode_message_subject_els([_ | _els], Data) ->
    decode_message_subject_els(_els, Data).

decode_message_subject_attrs([{<<"xml:lang">>, _val}
			      | _attrs],
			     _Lang) ->
    decode_message_subject_attrs(_attrs, _val);
decode_message_subject_attrs([_ | _attrs], Lang) ->
    decode_message_subject_attrs(_attrs, Lang);
decode_message_subject_attrs([], Lang) ->
    'decode_message_subject_attr_xml:lang'(Lang).

encode_message_subject({text, Lang, Data},
		       _xmlns_attrs) ->
    _els = encode_message_subject_cdata(Data, []),
    _attrs = 'encode_message_subject_attr_xml:lang'(Lang,
						    _xmlns_attrs),
    {xmlel, <<"subject">>, _attrs, _els}.

'decode_message_subject_attr_xml:lang'(undefined) ->
    undefined;
'decode_message_subject_attr_xml:lang'(_val) -> _val.

'encode_message_subject_attr_xml:lang'(undefined,
				       _acc) ->
    _acc;
'encode_message_subject_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_message_subject_cdata(<<>>) -> undefined;
decode_message_subject_cdata(_val) -> _val.

encode_message_subject_cdata(undefined, _acc) -> _acc;
encode_message_subject_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_iq({xmlel, <<"iq">>, _attrs, _els}) ->
    {Error, __Els} = decode_iq_els(_els, undefined, []),
    {Id, Type, From, To, Lang} = decode_iq_attrs(_attrs,
						 undefined, undefined,
						 undefined, undefined,
						 undefined),
    {iq, Id, Type, Lang, From, To, Error, __Els}.

decode_iq_els([], Error, __Els) ->
    {Error, lists:reverse(__Els)};
decode_iq_els([{xmlel, <<"error">>, _attrs, _} = _el
	       | _els],
	      Error, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:client">> ->
	   decode_iq_els(_els, decode_error(_el), __Els);
       true -> decode_iq_els(_els, Error, __Els)
    end;
decode_iq_els([{xmlel, _, _, _} = _el | _els], Error,
	      __Els) ->
    case is_known_tag(_el) of
      true ->
	  decode_iq_els(_els, Error, [decode(_el) | __Els]);
      false -> decode_iq_els(_els, Error, __Els)
    end;
decode_iq_els([_ | _els], Error, __Els) ->
    decode_iq_els(_els, Error, __Els).

decode_iq_attrs([{<<"id">>, _val} | _attrs], _Id, Type,
		From, To, Lang) ->
    decode_iq_attrs(_attrs, _val, Type, From, To, Lang);
decode_iq_attrs([{<<"type">>, _val} | _attrs], Id,
		_Type, From, To, Lang) ->
    decode_iq_attrs(_attrs, Id, _val, From, To, Lang);
decode_iq_attrs([{<<"from">>, _val} | _attrs], Id, Type,
		_From, To, Lang) ->
    decode_iq_attrs(_attrs, Id, Type, _val, To, Lang);
decode_iq_attrs([{<<"to">>, _val} | _attrs], Id, Type,
		From, _To, Lang) ->
    decode_iq_attrs(_attrs, Id, Type, From, _val, Lang);
decode_iq_attrs([{<<"xml:lang">>, _val} | _attrs], Id,
		Type, From, To, _Lang) ->
    decode_iq_attrs(_attrs, Id, Type, From, To, _val);
decode_iq_attrs([_ | _attrs], Id, Type, From, To,
		Lang) ->
    decode_iq_attrs(_attrs, Id, Type, From, To, Lang);
decode_iq_attrs([], Id, Type, From, To, Lang) ->
    {decode_iq_attr_id(Id), decode_iq_attr_type(Type),
     decode_iq_attr_from(From), decode_iq_attr_to(To),
     'decode_iq_attr_xml:lang'(Lang)}.

encode_iq({iq, Id, Type, Lang, From, To, Error, __Els},
	  _xmlns_attrs) ->
    _els = 'encode_iq_$error'(Error,
			      [encode(_el) || _el <- __Els]),
    _attrs = 'encode_iq_attr_xml:lang'(Lang,
				       encode_iq_attr_to(To,
							 encode_iq_attr_from(From,
									     encode_iq_attr_type(Type,
												 encode_iq_attr_id(Id,
														   _xmlns_attrs))))),
    {xmlel, <<"iq">>, _attrs, _els}.

'encode_iq_$error'(undefined, _acc) -> _acc;
'encode_iq_$error'(Error, _acc) ->
    [encode_error(Error, []) | _acc].

decode_iq_attr_id(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"id">>, <<"iq">>,
		   <<"jabber:client">>}});
decode_iq_attr_id(_val) -> _val.

encode_iq_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_iq_attr_type(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"type">>, <<"iq">>,
		   <<"jabber:client">>}});
decode_iq_attr_type(_val) ->
    case catch dec_enum(_val, [get, set, result, error]) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"type">>, <<"iq">>,
			 <<"jabber:client">>}});
      _res -> _res
    end.

encode_iq_attr_type(_val, _acc) ->
    [{<<"type">>, enc_enum(_val)} | _acc].

decode_iq_attr_from(undefined) -> undefined;
decode_iq_attr_from(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"from">>, <<"iq">>,
			 <<"jabber:client">>}});
      _res -> _res
    end.

encode_iq_attr_from(undefined, _acc) -> _acc;
encode_iq_attr_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_iq_attr_to(undefined) -> undefined;
decode_iq_attr_to(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"to">>, <<"iq">>,
			 <<"jabber:client">>}});
      _res -> _res
    end.

encode_iq_attr_to(undefined, _acc) -> _acc;
encode_iq_attr_to(_val, _acc) ->
    [{<<"to">>, enc_jid(_val)} | _acc].

'decode_iq_attr_xml:lang'(undefined) -> undefined;
'decode_iq_attr_xml:lang'(_val) -> _val.

'encode_iq_attr_xml:lang'(undefined, _acc) -> _acc;
'encode_iq_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_stats({xmlel, <<"query">>, _attrs, _els}) ->
    Stat = decode_stats_els(_els, []), {stats, Stat}.

decode_stats_els([], Stat) -> lists:reverse(Stat);
decode_stats_els([{xmlel, <<"stat">>, _attrs, _} = _el
		  | _els],
		 Stat) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/stats">> ->
	   decode_stats_els(_els, [decode_stat(_el) | Stat]);
       true -> decode_stats_els(_els, Stat)
    end;
decode_stats_els([_ | _els], Stat) ->
    decode_stats_els(_els, Stat).

encode_stats({stats, Stat}, _xmlns_attrs) ->
    _els = 'encode_stats_$stat'(Stat, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"query">>, _attrs, _els}.

'encode_stats_$stat'([], _acc) -> _acc;
'encode_stats_$stat'([Stat | _els], _acc) ->
    'encode_stats_$stat'(_els,
			 [encode_stat(Stat, []) | _acc]).

decode_stat({xmlel, <<"stat">>, _attrs, _els}) ->
    Error = decode_stat_els(_els, []),
    {Name, Units, Value} = decode_stat_attrs(_attrs,
					     undefined, undefined, undefined),
    {stat, Name, Units, Value, Error}.

decode_stat_els([], Error) -> lists:reverse(Error);
decode_stat_els([{xmlel, <<"error">>, _attrs, _} = _el
		 | _els],
		Error) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/stats">> ->
	   decode_stat_els(_els, [decode_stat_error(_el) | Error]);
       true -> decode_stat_els(_els, Error)
    end;
decode_stat_els([_ | _els], Error) ->
    decode_stat_els(_els, Error).

decode_stat_attrs([{<<"name">>, _val} | _attrs], _Name,
		  Units, Value) ->
    decode_stat_attrs(_attrs, _val, Units, Value);
decode_stat_attrs([{<<"units">>, _val} | _attrs], Name,
		  _Units, Value) ->
    decode_stat_attrs(_attrs, Name, _val, Value);
decode_stat_attrs([{<<"value">>, _val} | _attrs], Name,
		  Units, _Value) ->
    decode_stat_attrs(_attrs, Name, Units, _val);
decode_stat_attrs([_ | _attrs], Name, Units, Value) ->
    decode_stat_attrs(_attrs, Name, Units, Value);
decode_stat_attrs([], Name, Units, Value) ->
    {decode_stat_attr_name(Name),
     decode_stat_attr_units(Units),
     decode_stat_attr_value(Value)}.

encode_stat({stat, Name, Units, Value, Error},
	    _xmlns_attrs) ->
    _els = 'encode_stat_$error'(Error, []),
    _attrs = encode_stat_attr_value(Value,
				    encode_stat_attr_units(Units,
							   encode_stat_attr_name(Name,
										 _xmlns_attrs))),
    {xmlel, <<"stat">>, _attrs, _els}.

'encode_stat_$error'([], _acc) -> _acc;
'encode_stat_$error'([Error | _els], _acc) ->
    'encode_stat_$error'(_els,
			 [encode_stat_error(Error, []) | _acc]).

decode_stat_attr_name(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"name">>, <<"stat">>,
		   <<"http://jabber.org/protocol/stats">>}});
decode_stat_attr_name(_val) -> _val.

encode_stat_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_stat_attr_units(undefined) -> undefined;
decode_stat_attr_units(_val) -> _val.

encode_stat_attr_units(undefined, _acc) -> _acc;
encode_stat_attr_units(_val, _acc) ->
    [{<<"units">>, _val} | _acc].

decode_stat_attr_value(undefined) -> undefined;
decode_stat_attr_value(_val) -> _val.

encode_stat_attr_value(undefined, _acc) -> _acc;
encode_stat_attr_value(_val, _acc) ->
    [{<<"value">>, _val} | _acc].

decode_stat_error({xmlel, <<"error">>, _attrs, _els}) ->
    Cdata = decode_stat_error_els(_els, <<>>),
    Code = decode_stat_error_attrs(_attrs, undefined),
    {Code, Cdata}.

decode_stat_error_els([], Cdata) ->
    decode_stat_error_cdata(Cdata);
decode_stat_error_els([{xmlcdata, _data} | _els],
		      Cdata) ->
    decode_stat_error_els(_els,
			  <<Cdata/binary, _data/binary>>);
decode_stat_error_els([_ | _els], Cdata) ->
    decode_stat_error_els(_els, Cdata).

decode_stat_error_attrs([{<<"code">>, _val} | _attrs],
			_Code) ->
    decode_stat_error_attrs(_attrs, _val);
decode_stat_error_attrs([_ | _attrs], Code) ->
    decode_stat_error_attrs(_attrs, Code);
decode_stat_error_attrs([], Code) ->
    decode_stat_error_attr_code(Code).

encode_stat_error({Code, Cdata}, _xmlns_attrs) ->
    _els = encode_stat_error_cdata(Cdata, []),
    _attrs = encode_stat_error_attr_code(Code,
					 _xmlns_attrs),
    {xmlel, <<"error">>, _attrs, _els}.

decode_stat_error_attr_code(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"code">>, <<"error">>,
		   <<"http://jabber.org/protocol/stats">>}});
decode_stat_error_attr_code(_val) ->
    case catch dec_int(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"code">>, <<"error">>,
			 <<"http://jabber.org/protocol/stats">>}});
      _res -> _res
    end.

encode_stat_error_attr_code(_val, _acc) ->
    [{<<"code">>, enc_int(_val)} | _acc].

decode_stat_error_cdata(<<>>) -> undefined;
decode_stat_error_cdata(_val) -> _val.

encode_stat_error_cdata(undefined, _acc) -> _acc;
encode_stat_error_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_bookmarks_storage({xmlel, <<"storage">>, _attrs,
			  _els}) ->
    {Conference, Url} = decode_bookmarks_storage_els(_els,
						     [], []),
    {bookmark_storage, Conference, Url}.

decode_bookmarks_storage_els([], Conference, Url) ->
    {lists:reverse(Conference), lists:reverse(Url)};
decode_bookmarks_storage_els([{xmlel, <<"conference">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Conference, Url) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"storage:bookmarks">> ->
	   decode_bookmarks_storage_els(_els,
					[decode_bookmark_conference(_el)
					 | Conference],
					Url);
       true ->
	   decode_bookmarks_storage_els(_els, Conference, Url)
    end;
decode_bookmarks_storage_els([{xmlel, <<"url">>, _attrs,
			       _} =
				  _el
			      | _els],
			     Conference, Url) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"storage:bookmarks">> ->
	   decode_bookmarks_storage_els(_els, Conference,
					[decode_bookmark_url(_el) | Url]);
       true ->
	   decode_bookmarks_storage_els(_els, Conference, Url)
    end;
decode_bookmarks_storage_els([_ | _els], Conference,
			     Url) ->
    decode_bookmarks_storage_els(_els, Conference, Url).

encode_bookmarks_storage({bookmark_storage, Conference,
			  Url},
			 _xmlns_attrs) ->
    _els = 'encode_bookmarks_storage_$url'(Url,
					   'encode_bookmarks_storage_$conference'(Conference,
										  [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"storage">>, _attrs, _els}.

'encode_bookmarks_storage_$conference'([], _acc) ->
    _acc;
'encode_bookmarks_storage_$conference'([Conference
					| _els],
				       _acc) ->
    'encode_bookmarks_storage_$conference'(_els,
					   [encode_bookmark_conference(Conference,
								       [])
					    | _acc]).

'encode_bookmarks_storage_$url'([], _acc) -> _acc;
'encode_bookmarks_storage_$url'([Url | _els], _acc) ->
    'encode_bookmarks_storage_$url'(_els,
				    [encode_bookmark_url(Url, []) | _acc]).

decode_bookmark_url({xmlel, <<"url">>, _attrs, _els}) ->
    {Name, Url} = decode_bookmark_url_attrs(_attrs,
					    undefined, undefined),
    {bookmark_url, Name, Url}.

decode_bookmark_url_attrs([{<<"name">>, _val} | _attrs],
			  _Name, Url) ->
    decode_bookmark_url_attrs(_attrs, _val, Url);
decode_bookmark_url_attrs([{<<"url">>, _val} | _attrs],
			  Name, _Url) ->
    decode_bookmark_url_attrs(_attrs, Name, _val);
decode_bookmark_url_attrs([_ | _attrs], Name, Url) ->
    decode_bookmark_url_attrs(_attrs, Name, Url);
decode_bookmark_url_attrs([], Name, Url) ->
    {decode_bookmark_url_attr_name(Name),
     decode_bookmark_url_attr_url(Url)}.

encode_bookmark_url({bookmark_url, Name, Url},
		    _xmlns_attrs) ->
    _els = [],
    _attrs = encode_bookmark_url_attr_url(Url,
					  encode_bookmark_url_attr_name(Name,
									_xmlns_attrs)),
    {xmlel, <<"url">>, _attrs, _els}.

decode_bookmark_url_attr_name(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"name">>, <<"url">>,
		   <<"storage:bookmarks">>}});
decode_bookmark_url_attr_name(_val) -> _val.

encode_bookmark_url_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_bookmark_url_attr_url(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"url">>, <<"url">>,
		   <<"storage:bookmarks">>}});
decode_bookmark_url_attr_url(_val) -> _val.

encode_bookmark_url_attr_url(_val, _acc) ->
    [{<<"url">>, _val} | _acc].

decode_bookmark_conference({xmlel, <<"conference">>,
			    _attrs, _els}) ->
    {Password, Nick} = decode_bookmark_conference_els(_els,
						      undefined, undefined),
    {Name, Jid, Autojoin} =
	decode_bookmark_conference_attrs(_attrs, undefined,
					 undefined, undefined),
    {bookmark_conference, Name, Jid, Autojoin, Nick,
     Password}.

decode_bookmark_conference_els([], Password, Nick) ->
    {Password, Nick};
decode_bookmark_conference_els([{xmlel, <<"nick">>,
				 _attrs, _} =
				    _el
				| _els],
			       Password, Nick) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"storage:bookmarks">> ->
	   decode_bookmark_conference_els(_els, Password,
					  decode_conference_nick(_el));
       true ->
	   decode_bookmark_conference_els(_els, Password, Nick)
    end;
decode_bookmark_conference_els([{xmlel, <<"password">>,
				 _attrs, _} =
				    _el
				| _els],
			       Password, Nick) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"storage:bookmarks">> ->
	   decode_bookmark_conference_els(_els,
					  decode_conference_password(_el),
					  Nick);
       true ->
	   decode_bookmark_conference_els(_els, Password, Nick)
    end;
decode_bookmark_conference_els([_ | _els], Password,
			       Nick) ->
    decode_bookmark_conference_els(_els, Password, Nick).

decode_bookmark_conference_attrs([{<<"name">>, _val}
				  | _attrs],
				 _Name, Jid, Autojoin) ->
    decode_bookmark_conference_attrs(_attrs, _val, Jid,
				     Autojoin);
decode_bookmark_conference_attrs([{<<"jid">>, _val}
				  | _attrs],
				 Name, _Jid, Autojoin) ->
    decode_bookmark_conference_attrs(_attrs, Name, _val,
				     Autojoin);
decode_bookmark_conference_attrs([{<<"autojoin">>, _val}
				  | _attrs],
				 Name, Jid, _Autojoin) ->
    decode_bookmark_conference_attrs(_attrs, Name, Jid,
				     _val);
decode_bookmark_conference_attrs([_ | _attrs], Name,
				 Jid, Autojoin) ->
    decode_bookmark_conference_attrs(_attrs, Name, Jid,
				     Autojoin);
decode_bookmark_conference_attrs([], Name, Jid,
				 Autojoin) ->
    {decode_bookmark_conference_attr_name(Name),
     decode_bookmark_conference_attr_jid(Jid),
     decode_bookmark_conference_attr_autojoin(Autojoin)}.

encode_bookmark_conference({bookmark_conference, Name,
			    Jid, Autojoin, Nick, Password},
			   _xmlns_attrs) ->
    _els = 'encode_bookmark_conference_$nick'(Nick,
					      'encode_bookmark_conference_$password'(Password,
										     [])),
    _attrs =
	encode_bookmark_conference_attr_autojoin(Autojoin,
						 encode_bookmark_conference_attr_jid(Jid,
										     encode_bookmark_conference_attr_name(Name,
															  _xmlns_attrs))),
    {xmlel, <<"conference">>, _attrs, _els}.

'encode_bookmark_conference_$password'(undefined,
				       _acc) ->
    _acc;
'encode_bookmark_conference_$password'(Password,
				       _acc) ->
    [encode_conference_password(Password, []) | _acc].

'encode_bookmark_conference_$nick'(undefined, _acc) ->
    _acc;
'encode_bookmark_conference_$nick'(Nick, _acc) ->
    [encode_conference_nick(Nick, []) | _acc].

decode_bookmark_conference_attr_name(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"name">>, <<"conference">>,
		   <<"storage:bookmarks">>}});
decode_bookmark_conference_attr_name(_val) -> _val.

encode_bookmark_conference_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_bookmark_conference_attr_jid(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"conference">>,
		   <<"storage:bookmarks">>}});
decode_bookmark_conference_attr_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"conference">>,
			 <<"storage:bookmarks">>}});
      _res -> _res
    end.

encode_bookmark_conference_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_bookmark_conference_attr_autojoin(undefined) ->
    false;
decode_bookmark_conference_attr_autojoin(_val) ->
    case catch dec_bool(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"autojoin">>, <<"conference">>,
			 <<"storage:bookmarks">>}});
      _res -> _res
    end.

encode_bookmark_conference_attr_autojoin(false, _acc) ->
    _acc;
encode_bookmark_conference_attr_autojoin(_val, _acc) ->
    [{<<"autojoin">>, enc_bool(_val)} | _acc].

decode_conference_password({xmlel, <<"password">>,
			    _attrs, _els}) ->
    Cdata = decode_conference_password_els(_els, <<>>),
    Cdata.

decode_conference_password_els([], Cdata) ->
    decode_conference_password_cdata(Cdata);
decode_conference_password_els([{xmlcdata, _data}
				| _els],
			       Cdata) ->
    decode_conference_password_els(_els,
				   <<Cdata/binary, _data/binary>>);
decode_conference_password_els([_ | _els], Cdata) ->
    decode_conference_password_els(_els, Cdata).

encode_conference_password(Cdata, _xmlns_attrs) ->
    _els = encode_conference_password_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"password">>, _attrs, _els}.

decode_conference_password_cdata(<<>>) -> undefined;
decode_conference_password_cdata(_val) -> _val.

encode_conference_password_cdata(undefined, _acc) ->
    _acc;
encode_conference_password_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_conference_nick({xmlel, <<"nick">>, _attrs,
			_els}) ->
    Cdata = decode_conference_nick_els(_els, <<>>), Cdata.

decode_conference_nick_els([], Cdata) ->
    decode_conference_nick_cdata(Cdata);
decode_conference_nick_els([{xmlcdata, _data} | _els],
			   Cdata) ->
    decode_conference_nick_els(_els,
			       <<Cdata/binary, _data/binary>>);
decode_conference_nick_els([_ | _els], Cdata) ->
    decode_conference_nick_els(_els, Cdata).

encode_conference_nick(Cdata, _xmlns_attrs) ->
    _els = encode_conference_nick_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"nick">>, _attrs, _els}.

decode_conference_nick_cdata(<<>>) -> undefined;
decode_conference_nick_cdata(_val) -> _val.

encode_conference_nick_cdata(undefined, _acc) -> _acc;
encode_conference_nick_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_private({xmlel, <<"query">>, _attrs, _els}) ->
    __Xmls = decode_private_els(_els, []),
    {private, __Xmls}.

decode_private_els([], __Xmls) -> lists:reverse(__Xmls);
decode_private_els([{xmlel, _, _, _} = _el | _els],
		   __Xmls) ->
    decode_private_els(_els, [_el | __Xmls]);
decode_private_els([_ | _els], __Xmls) ->
    decode_private_els(_els, __Xmls).

encode_private({private, __Xmls}, _xmlns_attrs) ->
    _els = __Xmls,
    _attrs = _xmlns_attrs,
    {xmlel, <<"query">>, _attrs, _els}.

decode_disco_items({xmlel, <<"query">>, _attrs,
		    _els}) ->
    Items = decode_disco_items_els(_els, []),
    Node = decode_disco_items_attrs(_attrs, undefined),
    {disco_items, Node, Items}.

decode_disco_items_els([], Items) ->
    lists:reverse(Items);
decode_disco_items_els([{xmlel, <<"item">>, _attrs, _} =
			    _el
			| _els],
		       Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns ==
	 <<"http://jabber.org/protocol/disco#items">> ->
	   decode_disco_items_els(_els,
				  [decode_disco_item(_el) | Items]);
       true -> decode_disco_items_els(_els, Items)
    end;
decode_disco_items_els([_ | _els], Items) ->
    decode_disco_items_els(_els, Items).

decode_disco_items_attrs([{<<"node">>, _val} | _attrs],
			 _Node) ->
    decode_disco_items_attrs(_attrs, _val);
decode_disco_items_attrs([_ | _attrs], Node) ->
    decode_disco_items_attrs(_attrs, Node);
decode_disco_items_attrs([], Node) ->
    decode_disco_items_attr_node(Node).

encode_disco_items({disco_items, Node, Items},
		   _xmlns_attrs) ->
    _els = 'encode_disco_items_$items'(Items, []),
    _attrs = encode_disco_items_attr_node(Node,
					  _xmlns_attrs),
    {xmlel, <<"query">>, _attrs, _els}.

'encode_disco_items_$items'([], _acc) -> _acc;
'encode_disco_items_$items'([Items | _els], _acc) ->
    'encode_disco_items_$items'(_els,
				[encode_disco_item(Items, []) | _acc]).

decode_disco_items_attr_node(undefined) -> undefined;
decode_disco_items_attr_node(_val) -> _val.

encode_disco_items_attr_node(undefined, _acc) -> _acc;
encode_disco_items_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_disco_item({xmlel, <<"item">>, _attrs, _els}) ->
    {Jid, Name, Node} = decode_disco_item_attrs(_attrs,
						undefined, undefined,
						undefined),
    {disco_item, Jid, Name, Node}.

decode_disco_item_attrs([{<<"jid">>, _val} | _attrs],
			_Jid, Name, Node) ->
    decode_disco_item_attrs(_attrs, _val, Name, Node);
decode_disco_item_attrs([{<<"name">>, _val} | _attrs],
			Jid, _Name, Node) ->
    decode_disco_item_attrs(_attrs, Jid, _val, Node);
decode_disco_item_attrs([{<<"node">>, _val} | _attrs],
			Jid, Name, _Node) ->
    decode_disco_item_attrs(_attrs, Jid, Name, _val);
decode_disco_item_attrs([_ | _attrs], Jid, Name,
			Node) ->
    decode_disco_item_attrs(_attrs, Jid, Name, Node);
decode_disco_item_attrs([], Jid, Name, Node) ->
    {decode_disco_item_attr_jid(Jid),
     decode_disco_item_attr_name(Name),
     decode_disco_item_attr_node(Node)}.

encode_disco_item({disco_item, Jid, Name, Node},
		  _xmlns_attrs) ->
    _els = [],
    _attrs = encode_disco_item_attr_node(Node,
					 encode_disco_item_attr_name(Name,
								     encode_disco_item_attr_jid(Jid,
												_xmlns_attrs))),
    {xmlel, <<"item">>, _attrs, _els}.

decode_disco_item_attr_jid(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"item">>,
		   <<"http://jabber.org/protocol/disco#items">>}});
decode_disco_item_attr_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"item">>,
			 <<"http://jabber.org/protocol/disco#items">>}});
      _res -> _res
    end.

encode_disco_item_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_disco_item_attr_name(undefined) -> undefined;
decode_disco_item_attr_name(_val) -> _val.

encode_disco_item_attr_name(undefined, _acc) -> _acc;
encode_disco_item_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_disco_item_attr_node(undefined) -> undefined;
decode_disco_item_attr_node(_val) -> _val.

encode_disco_item_attr_node(undefined, _acc) -> _acc;
encode_disco_item_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_disco_info({xmlel, <<"query">>, _attrs, _els}) ->
    {Xdata, Features, Identities} =
	decode_disco_info_els(_els, [], [], []),
    Node = decode_disco_info_attrs(_attrs, undefined),
    {disco_info, Node, Identities, Features, Xdata}.

decode_disco_info_els([], Xdata, Features,
		      Identities) ->
    {lists:reverse(Xdata), lists:reverse(Features),
     lists:reverse(Identities)};
decode_disco_info_els([{xmlel, <<"identity">>, _attrs,
			_} =
			   _el
		       | _els],
		      Xdata, Features, Identities) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/disco#info">> ->
	   decode_disco_info_els(_els, Xdata, Features,
				 [decode_disco_identity(_el) | Identities]);
       true ->
	   decode_disco_info_els(_els, Xdata, Features, Identities)
    end;
decode_disco_info_els([{xmlel, <<"feature">>, _attrs,
			_} =
			   _el
		       | _els],
		      Xdata, Features, Identities) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>;
       _xmlns == <<"http://jabber.org/protocol/disco#info">> ->
	   decode_disco_info_els(_els, Xdata,
				 [decode_disco_feature(_el) | Features],
				 Identities);
       true ->
	   decode_disco_info_els(_els, Xdata, Features, Identities)
    end;
decode_disco_info_els([{xmlel, <<"x">>, _attrs, _} = _el
		       | _els],
		      Xdata, Features, Identities) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<"jabber:x:data">> ->
	   decode_disco_info_els(_els, [decode_xdata(_el) | Xdata],
				 Features, Identities);
       true ->
	   decode_disco_info_els(_els, Xdata, Features, Identities)
    end;
decode_disco_info_els([_ | _els], Xdata, Features,
		      Identities) ->
    decode_disco_info_els(_els, Xdata, Features,
			  Identities).

decode_disco_info_attrs([{<<"node">>, _val} | _attrs],
			_Node) ->
    decode_disco_info_attrs(_attrs, _val);
decode_disco_info_attrs([_ | _attrs], Node) ->
    decode_disco_info_attrs(_attrs, Node);
decode_disco_info_attrs([], Node) ->
    decode_disco_info_attr_node(Node).

encode_disco_info({disco_info, Node, Identities,
		   Features, Xdata},
		  _xmlns_attrs) ->
    _els = 'encode_disco_info_$identities'(Identities,
					   'encode_disco_info_$features'(Features,
									 'encode_disco_info_$xdata'(Xdata,
												    []))),
    _attrs = encode_disco_info_attr_node(Node,
					 _xmlns_attrs),
    {xmlel, <<"query">>, _attrs, _els}.

'encode_disco_info_$xdata'([], _acc) -> _acc;
'encode_disco_info_$xdata'([Xdata | _els], _acc) ->
    'encode_disco_info_$xdata'(_els,
			       [encode_xdata(Xdata,
					     [{<<"xmlns">>,
					       <<"jabber:x:data">>}])
				| _acc]).

'encode_disco_info_$features'([], _acc) -> _acc;
'encode_disco_info_$features'([Features | _els],
			      _acc) ->
    'encode_disco_info_$features'(_els,
				  [encode_disco_feature(Features, []) | _acc]).

'encode_disco_info_$identities'([], _acc) -> _acc;
'encode_disco_info_$identities'([Identities | _els],
				_acc) ->
    'encode_disco_info_$identities'(_els,
				    [encode_disco_identity(Identities, [])
				     | _acc]).

decode_disco_info_attr_node(undefined) -> undefined;
decode_disco_info_attr_node(_val) -> _val.

encode_disco_info_attr_node(undefined, _acc) -> _acc;
encode_disco_info_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_disco_feature({xmlel, <<"feature">>, _attrs,
		      _els}) ->
    Var = decode_disco_feature_attrs(_attrs, undefined),
    Var.

decode_disco_feature_attrs([{<<"var">>, _val} | _attrs],
			   _Var) ->
    decode_disco_feature_attrs(_attrs, _val);
decode_disco_feature_attrs([_ | _attrs], Var) ->
    decode_disco_feature_attrs(_attrs, Var);
decode_disco_feature_attrs([], Var) ->
    decode_disco_feature_attr_var(Var).

encode_disco_feature(Var, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_disco_feature_attr_var(Var,
					   _xmlns_attrs),
    {xmlel, <<"feature">>, _attrs, _els}.

decode_disco_feature_attr_var(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"var">>, <<"feature">>,
		   <<"http://jabber.org/protocol/disco#info">>}});
decode_disco_feature_attr_var(_val) -> _val.

encode_disco_feature_attr_var(_val, _acc) ->
    [{<<"var">>, _val} | _acc].

decode_disco_identity({xmlel, <<"identity">>, _attrs,
		       _els}) ->
    {Category, Type, Lang, Name} =
	decode_disco_identity_attrs(_attrs, undefined,
				    undefined, undefined, undefined),
    {identity, Category, Type, Lang, Name}.

decode_disco_identity_attrs([{<<"category">>, _val}
			     | _attrs],
			    _Category, Type, Lang, Name) ->
    decode_disco_identity_attrs(_attrs, _val, Type, Lang,
				Name);
decode_disco_identity_attrs([{<<"type">>, _val}
			     | _attrs],
			    Category, _Type, Lang, Name) ->
    decode_disco_identity_attrs(_attrs, Category, _val,
				Lang, Name);
decode_disco_identity_attrs([{<<"xml:lang">>, _val}
			     | _attrs],
			    Category, Type, _Lang, Name) ->
    decode_disco_identity_attrs(_attrs, Category, Type,
				_val, Name);
decode_disco_identity_attrs([{<<"name">>, _val}
			     | _attrs],
			    Category, Type, Lang, _Name) ->
    decode_disco_identity_attrs(_attrs, Category, Type,
				Lang, _val);
decode_disco_identity_attrs([_ | _attrs], Category,
			    Type, Lang, Name) ->
    decode_disco_identity_attrs(_attrs, Category, Type,
				Lang, Name);
decode_disco_identity_attrs([], Category, Type, Lang,
			    Name) ->
    {decode_disco_identity_attr_category(Category),
     decode_disco_identity_attr_type(Type),
     'decode_disco_identity_attr_xml:lang'(Lang),
     decode_disco_identity_attr_name(Name)}.

encode_disco_identity({identity, Category, Type, Lang,
		       Name},
		      _xmlns_attrs) ->
    _els = [],
    _attrs = encode_disco_identity_attr_name(Name,
					     'encode_disco_identity_attr_xml:lang'(Lang,
										   encode_disco_identity_attr_type(Type,
														   encode_disco_identity_attr_category(Category,
																		       _xmlns_attrs)))),
    {xmlel, <<"identity">>, _attrs, _els}.

decode_disco_identity_attr_category(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"category">>, <<"identity">>,
		   <<"http://jabber.org/protocol/disco#info">>}});
decode_disco_identity_attr_category(_val) -> _val.

encode_disco_identity_attr_category(_val, _acc) ->
    [{<<"category">>, _val} | _acc].

decode_disco_identity_attr_type(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"type">>, <<"identity">>,
		   <<"http://jabber.org/protocol/disco#info">>}});
decode_disco_identity_attr_type(_val) -> _val.

encode_disco_identity_attr_type(_val, _acc) ->
    [{<<"type">>, _val} | _acc].

'decode_disco_identity_attr_xml:lang'(undefined) ->
    undefined;
'decode_disco_identity_attr_xml:lang'(_val) -> _val.

'encode_disco_identity_attr_xml:lang'(undefined,
				      _acc) ->
    _acc;
'encode_disco_identity_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_disco_identity_attr_name(undefined) -> undefined;
decode_disco_identity_attr_name(_val) -> _val.

encode_disco_identity_attr_name(undefined, _acc) ->
    _acc;
encode_disco_identity_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_block_list({xmlel, <<"blocklist">>, _attrs,
		   _els}) ->
    {block_list}.

encode_block_list({block_list}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"blocklist">>, _attrs, _els}.

decode_unblock({xmlel, <<"unblock">>, _attrs, _els}) ->
    Items = decode_unblock_els(_els, []), {unblock, Items}.

decode_unblock_els([], Items) -> lists:reverse(Items);
decode_unblock_els([{xmlel, <<"item">>, _attrs, _} = _el
		    | _els],
		   Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"urn:xmpp:blocking">> ->
	   decode_unblock_els(_els,
			      case decode_block_item(_el) of
				undefined -> Items;
				_new_el -> [_new_el | Items]
			      end);
       true -> decode_unblock_els(_els, Items)
    end;
decode_unblock_els([_ | _els], Items) ->
    decode_unblock_els(_els, Items).

encode_unblock({unblock, Items}, _xmlns_attrs) ->
    _els = 'encode_unblock_$items'(Items, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"unblock">>, _attrs, _els}.

'encode_unblock_$items'([], _acc) -> _acc;
'encode_unblock_$items'([Items | _els], _acc) ->
    'encode_unblock_$items'(_els,
			    [encode_block_item(Items, []) | _acc]).

decode_block({xmlel, <<"block">>, _attrs, _els}) ->
    Items = decode_block_els(_els, []), {block, Items}.

decode_block_els([], Items) -> lists:reverse(Items);
decode_block_els([{xmlel, <<"item">>, _attrs, _} = _el
		  | _els],
		 Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"urn:xmpp:blocking">> ->
	   decode_block_els(_els,
			    case decode_block_item(_el) of
			      undefined -> Items;
			      _new_el -> [_new_el | Items]
			    end);
       true -> decode_block_els(_els, Items)
    end;
decode_block_els([_ | _els], Items) ->
    decode_block_els(_els, Items).

encode_block({block, Items}, _xmlns_attrs) ->
    _els = 'encode_block_$items'(Items, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"block">>, _attrs, _els}.

'encode_block_$items'([], _acc) -> _acc;
'encode_block_$items'([Items | _els], _acc) ->
    'encode_block_$items'(_els,
			  [encode_block_item(Items, []) | _acc]).

decode_block_item({xmlel, <<"item">>, _attrs, _els}) ->
    Jid = decode_block_item_attrs(_attrs, undefined), Jid.

decode_block_item_attrs([{<<"jid">>, _val} | _attrs],
			_Jid) ->
    decode_block_item_attrs(_attrs, _val);
decode_block_item_attrs([_ | _attrs], Jid) ->
    decode_block_item_attrs(_attrs, Jid);
decode_block_item_attrs([], Jid) ->
    decode_block_item_attr_jid(Jid).

encode_block_item(Jid, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_block_item_attr_jid(Jid, _xmlns_attrs),
    {xmlel, <<"item">>, _attrs, _els}.

decode_block_item_attr_jid(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"item">>,
		   <<"urn:xmpp:blocking">>}});
decode_block_item_attr_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"item">>,
			 <<"urn:xmpp:blocking">>}});
      _res -> _res
    end.

encode_block_item_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_privacy({xmlel, <<"query">>, _attrs, _els}) ->
    {Lists, Default, Active} = decode_privacy_els(_els, [],
						  undefined, undefined),
    {privacy, Lists, Default, Active}.

decode_privacy_els([], Lists, Default, Active) ->
    {lists:reverse(Lists), Default, Active};
decode_privacy_els([{xmlel, <<"list">>, _attrs, _} = _el
		    | _els],
		   Lists, Default, Active) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:privacy">> ->
	   decode_privacy_els(_els,
			      [decode_privacy_list(_el) | Lists], Default,
			      Active);
       true -> decode_privacy_els(_els, Lists, Default, Active)
    end;
decode_privacy_els([{xmlel, <<"default">>, _attrs, _} =
			_el
		    | _els],
		   Lists, Default, Active) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:privacy">> ->
	   decode_privacy_els(_els, Lists,
			      decode_privacy_default_list(_el), Active);
       true -> decode_privacy_els(_els, Lists, Default, Active)
    end;
decode_privacy_els([{xmlel, <<"active">>, _attrs, _} =
			_el
		    | _els],
		   Lists, Default, Active) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:privacy">> ->
	   decode_privacy_els(_els, Lists, Default,
			      decode_privacy_active_list(_el));
       true -> decode_privacy_els(_els, Lists, Default, Active)
    end;
decode_privacy_els([_ | _els], Lists, Default,
		   Active) ->
    decode_privacy_els(_els, Lists, Default, Active).

encode_privacy({privacy, Lists, Default, Active},
	       _xmlns_attrs) ->
    _els = 'encode_privacy_$active'(Active,
				    'encode_privacy_$default'(Default,
							      'encode_privacy_$lists'(Lists,
										      []))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"query">>, _attrs, _els}.

'encode_privacy_$lists'([], _acc) -> _acc;
'encode_privacy_$lists'([Lists | _els], _acc) ->
    'encode_privacy_$lists'(_els,
			    [encode_privacy_list(Lists, []) | _acc]).

'encode_privacy_$default'(undefined, _acc) -> _acc;
'encode_privacy_$default'(Default, _acc) ->
    [encode_privacy_default_list(Default, []) | _acc].

'encode_privacy_$active'(undefined, _acc) -> _acc;
'encode_privacy_$active'(Active, _acc) ->
    [encode_privacy_active_list(Active, []) | _acc].

decode_privacy_active_list({xmlel, <<"active">>, _attrs,
			    _els}) ->
    Name = decode_privacy_active_list_attrs(_attrs,
					    undefined),
    Name.

decode_privacy_active_list_attrs([{<<"name">>, _val}
				  | _attrs],
				 _Name) ->
    decode_privacy_active_list_attrs(_attrs, _val);
decode_privacy_active_list_attrs([_ | _attrs], Name) ->
    decode_privacy_active_list_attrs(_attrs, Name);
decode_privacy_active_list_attrs([], Name) ->
    decode_privacy_active_list_attr_name(Name).

encode_privacy_active_list(Name, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_privacy_active_list_attr_name(Name,
						  _xmlns_attrs),
    {xmlel, <<"active">>, _attrs, _els}.

decode_privacy_active_list_attr_name(undefined) -> none;
decode_privacy_active_list_attr_name(_val) -> _val.

encode_privacy_active_list_attr_name(none, _acc) ->
    _acc;
encode_privacy_active_list_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_privacy_default_list({xmlel, <<"default">>,
			     _attrs, _els}) ->
    Name = decode_privacy_default_list_attrs(_attrs,
					     undefined),
    Name.

decode_privacy_default_list_attrs([{<<"name">>, _val}
				   | _attrs],
				  _Name) ->
    decode_privacy_default_list_attrs(_attrs, _val);
decode_privacy_default_list_attrs([_ | _attrs], Name) ->
    decode_privacy_default_list_attrs(_attrs, Name);
decode_privacy_default_list_attrs([], Name) ->
    decode_privacy_default_list_attr_name(Name).

encode_privacy_default_list(Name, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_privacy_default_list_attr_name(Name,
						   _xmlns_attrs),
    {xmlel, <<"default">>, _attrs, _els}.

decode_privacy_default_list_attr_name(undefined) ->
    none;
decode_privacy_default_list_attr_name(_val) -> _val.

encode_privacy_default_list_attr_name(none, _acc) ->
    _acc;
encode_privacy_default_list_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_privacy_list({xmlel, <<"list">>, _attrs,
		     _els}) ->
    Items = decode_privacy_list_els(_els, []),
    Name = decode_privacy_list_attrs(_attrs, undefined),
    {privacy_list, Name, Items}.

decode_privacy_list_els([], Items) ->
    lists:reverse(Items);
decode_privacy_list_els([{xmlel, <<"item">>, _attrs,
			  _} =
			     _el
			 | _els],
			Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:privacy">> ->
	   decode_privacy_list_els(_els,
				   [decode_privacy_item(_el) | Items]);
       true -> decode_privacy_list_els(_els, Items)
    end;
decode_privacy_list_els([_ | _els], Items) ->
    decode_privacy_list_els(_els, Items).

decode_privacy_list_attrs([{<<"name">>, _val} | _attrs],
			  _Name) ->
    decode_privacy_list_attrs(_attrs, _val);
decode_privacy_list_attrs([_ | _attrs], Name) ->
    decode_privacy_list_attrs(_attrs, Name);
decode_privacy_list_attrs([], Name) ->
    decode_privacy_list_attr_name(Name).

encode_privacy_list({privacy_list, Name, Items},
		    _xmlns_attrs) ->
    _els = 'encode_privacy_list_$items'(Items, []),
    _attrs = encode_privacy_list_attr_name(Name,
					   _xmlns_attrs),
    {xmlel, <<"list">>, _attrs, _els}.

'encode_privacy_list_$items'([], _acc) -> _acc;
'encode_privacy_list_$items'([Items | _els], _acc) ->
    'encode_privacy_list_$items'(_els,
				 [encode_privacy_item(Items, []) | _acc]).

decode_privacy_list_attr_name(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"name">>, <<"list">>,
		   <<"jabber:iq:privacy">>}});
decode_privacy_list_attr_name(_val) -> _val.

encode_privacy_list_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_privacy_item({xmlel, <<"item">>, _attrs,
		     _els}) ->
    Kinds = decode_privacy_item_els(_els, []),
    {Action, Order, Type, Value} =
	decode_privacy_item_attrs(_attrs, undefined, undefined,
				  undefined, undefined),
    {privacy_item, Order, Action, Type, Value, Kinds}.

decode_privacy_item_els([], Kinds) ->
    lists:reverse(Kinds);
decode_privacy_item_els([{xmlel, <<"message">>, _attrs,
			  _} =
			     _el
			 | _els],
			Kinds) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:privacy">> ->
	   decode_privacy_item_els(_els, Kinds);
       true -> decode_privacy_item_els(_els, Kinds)
    end;
decode_privacy_item_els([{xmlel, <<"iq">>, _attrs, _} =
			     _el
			 | _els],
			Kinds) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:privacy">> ->
	   decode_privacy_item_els(_els, Kinds);
       true -> decode_privacy_item_els(_els, Kinds)
    end;
decode_privacy_item_els([{xmlel, <<"presence-in">>,
			  _attrs, _} =
			     _el
			 | _els],
			Kinds) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:privacy">> ->
	   decode_privacy_item_els(_els, Kinds);
       true -> decode_privacy_item_els(_els, Kinds)
    end;
decode_privacy_item_els([{xmlel, <<"presence-out">>,
			  _attrs, _} =
			     _el
			 | _els],
			Kinds) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:privacy">> ->
	   decode_privacy_item_els(_els, Kinds);
       true -> decode_privacy_item_els(_els, Kinds)
    end;
decode_privacy_item_els([_ | _els], Kinds) ->
    decode_privacy_item_els(_els, Kinds).

decode_privacy_item_attrs([{<<"action">>, _val}
			   | _attrs],
			  _Action, Order, Type, Value) ->
    decode_privacy_item_attrs(_attrs, _val, Order, Type,
			      Value);
decode_privacy_item_attrs([{<<"order">>, _val}
			   | _attrs],
			  Action, _Order, Type, Value) ->
    decode_privacy_item_attrs(_attrs, Action, _val, Type,
			      Value);
decode_privacy_item_attrs([{<<"type">>, _val} | _attrs],
			  Action, Order, _Type, Value) ->
    decode_privacy_item_attrs(_attrs, Action, Order, _val,
			      Value);
decode_privacy_item_attrs([{<<"value">>, _val}
			   | _attrs],
			  Action, Order, Type, _Value) ->
    decode_privacy_item_attrs(_attrs, Action, Order, Type,
			      _val);
decode_privacy_item_attrs([_ | _attrs], Action, Order,
			  Type, Value) ->
    decode_privacy_item_attrs(_attrs, Action, Order, Type,
			      Value);
decode_privacy_item_attrs([], Action, Order, Type,
			  Value) ->
    {decode_privacy_item_attr_action(Action),
     decode_privacy_item_attr_order(Order),
     decode_privacy_item_attr_type(Type),
     decode_privacy_item_attr_value(Value)}.

encode_privacy_item({privacy_item, Order, Action, Type,
		     Value, Kinds},
		    _xmlns_attrs) ->
    _els = 'encode_privacy_item_$kinds'(Kinds, []),
    _attrs = encode_privacy_item_attr_value(Value,
					    encode_privacy_item_attr_type(Type,
									  encode_privacy_item_attr_order(Order,
													 encode_privacy_item_attr_action(Action,
																	 _xmlns_attrs)))),
    {xmlel, <<"item">>, _attrs, _els}.

'encode_privacy_item_$kinds'([], _acc) -> _acc;
'encode_privacy_item_$kinds'([message = Kinds | _els],
			     _acc) ->
    'encode_privacy_item_$kinds'(_els,
				 [encode_privacy_message(Kinds, []) | _acc]);
'encode_privacy_item_$kinds'([iq = Kinds | _els],
			     _acc) ->
    'encode_privacy_item_$kinds'(_els,
				 [encode_privacy_iq(Kinds, []) | _acc]);
'encode_privacy_item_$kinds'(['presence-in' = Kinds
			      | _els],
			     _acc) ->
    'encode_privacy_item_$kinds'(_els,
				 [encode_privacy_presence_in(Kinds, [])
				  | _acc]);
'encode_privacy_item_$kinds'(['presence-out' = Kinds
			      | _els],
			     _acc) ->
    'encode_privacy_item_$kinds'(_els,
				 [encode_privacy_presence_out(Kinds, [])
				  | _acc]).

decode_privacy_item_attr_action(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"action">>, <<"item">>,
		   <<"jabber:iq:privacy">>}});
decode_privacy_item_attr_action(_val) ->
    case catch dec_enum(_val, [allow, deny]) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"action">>, <<"item">>,
			 <<"jabber:iq:privacy">>}});
      _res -> _res
    end.

encode_privacy_item_attr_action(_val, _acc) ->
    [{<<"action">>, enc_enum(_val)} | _acc].

decode_privacy_item_attr_order(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"order">>, <<"item">>,
		   <<"jabber:iq:privacy">>}});
decode_privacy_item_attr_order(_val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"order">>, <<"item">>,
			 <<"jabber:iq:privacy">>}});
      _res -> _res
    end.

encode_privacy_item_attr_order(_val, _acc) ->
    [{<<"order">>, enc_int(_val)} | _acc].

decode_privacy_item_attr_type(undefined) -> undefined;
decode_privacy_item_attr_type(_val) ->
    case catch dec_enum(_val, [group, jid, subscription]) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"type">>, <<"item">>,
			 <<"jabber:iq:privacy">>}});
      _res -> _res
    end.

encode_privacy_item_attr_type(undefined, _acc) -> _acc;
encode_privacy_item_attr_type(_val, _acc) ->
    [{<<"type">>, enc_enum(_val)} | _acc].

decode_privacy_item_attr_value(undefined) -> undefined;
decode_privacy_item_attr_value(_val) -> _val.

encode_privacy_item_attr_value(undefined, _acc) -> _acc;
encode_privacy_item_attr_value(_val, _acc) ->
    [{<<"value">>, _val} | _acc].

decode_privacy_presence_out({xmlel, <<"presence-out">>,
			     _attrs, _els}) ->
    'presence-out'.

encode_privacy_presence_out('presence-out',
			    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"presence-out">>, _attrs, _els}.

decode_privacy_presence_in({xmlel, <<"presence-in">>,
			    _attrs, _els}) ->
    'presence-in'.

encode_privacy_presence_in('presence-in',
			   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"presence-in">>, _attrs, _els}.

decode_privacy_iq({xmlel, <<"iq">>, _attrs, _els}) ->
    iq.

encode_privacy_iq(iq, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"iq">>, _attrs, _els}.

decode_privacy_message({xmlel, <<"message">>, _attrs,
			_els}) ->
    message.

encode_privacy_message(message, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"message">>, _attrs, _els}.

decode_roster({xmlel, <<"query">>, _attrs, _els}) ->
    Items = decode_roster_els(_els, []),
    Ver = decode_roster_attrs(_attrs, undefined),
    {roster, Items, Ver}.

decode_roster_els([], Items) -> lists:reverse(Items);
decode_roster_els([{xmlel, <<"item">>, _attrs, _} = _el
		   | _els],
		  Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:roster">> ->
	   decode_roster_els(_els,
			     [decode_roster_item(_el) | Items]);
       true -> decode_roster_els(_els, Items)
    end;
decode_roster_els([_ | _els], Items) ->
    decode_roster_els(_els, Items).

decode_roster_attrs([{<<"ver">>, _val} | _attrs],
		    _Ver) ->
    decode_roster_attrs(_attrs, _val);
decode_roster_attrs([_ | _attrs], Ver) ->
    decode_roster_attrs(_attrs, Ver);
decode_roster_attrs([], Ver) ->
    decode_roster_attr_ver(Ver).

encode_roster({roster, Items, Ver}, _xmlns_attrs) ->
    _els = 'encode_roster_$items'(Items, []),
    _attrs = encode_roster_attr_ver(Ver, _xmlns_attrs),
    {xmlel, <<"query">>, _attrs, _els}.

'encode_roster_$items'([], _acc) -> _acc;
'encode_roster_$items'([Items | _els], _acc) ->
    'encode_roster_$items'(_els,
			   [encode_roster_item(Items, []) | _acc]).

decode_roster_attr_ver(undefined) -> undefined;
decode_roster_attr_ver(_val) -> _val.

encode_roster_attr_ver(undefined, _acc) -> _acc;
encode_roster_attr_ver(_val, _acc) ->
    [{<<"ver">>, _val} | _acc].

decode_roster_item({xmlel, <<"item">>, _attrs, _els}) ->
    Groups = decode_roster_item_els(_els, []),
    {Jid, Name, Subscription, Ask} =
	decode_roster_item_attrs(_attrs, undefined, undefined,
				 undefined, undefined),
    {roster_item, Jid, Name, Groups, Subscription, Ask}.

decode_roster_item_els([], Groups) ->
    lists:reverse(Groups);
decode_roster_item_els([{xmlel, <<"group">>, _attrs,
			 _} =
			    _el
			| _els],
		       Groups) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:roster">> ->
	   decode_roster_item_els(_els,
				  [decode_roster_group(_el) | Groups]);
       true -> decode_roster_item_els(_els, Groups)
    end;
decode_roster_item_els([_ | _els], Groups) ->
    decode_roster_item_els(_els, Groups).

decode_roster_item_attrs([{<<"jid">>, _val} | _attrs],
			 _Jid, Name, Subscription, Ask) ->
    decode_roster_item_attrs(_attrs, _val, Name,
			     Subscription, Ask);
decode_roster_item_attrs([{<<"name">>, _val} | _attrs],
			 Jid, _Name, Subscription, Ask) ->
    decode_roster_item_attrs(_attrs, Jid, _val,
			     Subscription, Ask);
decode_roster_item_attrs([{<<"subscription">>, _val}
			  | _attrs],
			 Jid, Name, _Subscription, Ask) ->
    decode_roster_item_attrs(_attrs, Jid, Name, _val, Ask);
decode_roster_item_attrs([{<<"ask">>, _val} | _attrs],
			 Jid, Name, Subscription, _Ask) ->
    decode_roster_item_attrs(_attrs, Jid, Name,
			     Subscription, _val);
decode_roster_item_attrs([_ | _attrs], Jid, Name,
			 Subscription, Ask) ->
    decode_roster_item_attrs(_attrs, Jid, Name,
			     Subscription, Ask);
decode_roster_item_attrs([], Jid, Name, Subscription,
			 Ask) ->
    {decode_roster_item_attr_jid(Jid),
     decode_roster_item_attr_name(Name),
     decode_roster_item_attr_subscription(Subscription),
     decode_roster_item_attr_ask(Ask)}.

encode_roster_item({roster_item, Jid, Name, Groups,
		    Subscription, Ask},
		   _xmlns_attrs) ->
    _els = 'encode_roster_item_$groups'(Groups, []),
    _attrs = encode_roster_item_attr_ask(Ask,
					 encode_roster_item_attr_subscription(Subscription,
									      encode_roster_item_attr_name(Name,
													   encode_roster_item_attr_jid(Jid,
																       _xmlns_attrs)))),
    {xmlel, <<"item">>, _attrs, _els}.

'encode_roster_item_$groups'([], _acc) -> _acc;
'encode_roster_item_$groups'([Groups | _els], _acc) ->
    'encode_roster_item_$groups'(_els,
				 [encode_roster_group(Groups, []) | _acc]).

decode_roster_item_attr_jid(undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"item">>,
		   <<"jabber:iq:roster">>}});
decode_roster_item_attr_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"item">>,
			 <<"jabber:iq:roster">>}});
      _res -> _res
    end.

encode_roster_item_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_roster_item_attr_name(undefined) -> undefined;
decode_roster_item_attr_name(_val) -> _val.

encode_roster_item_attr_name(undefined, _acc) -> _acc;
encode_roster_item_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_roster_item_attr_subscription(undefined) -> none;
decode_roster_item_attr_subscription(_val) ->
    case catch dec_enum(_val,
			[none, to, from, both, remove])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"subscription">>, <<"item">>,
			 <<"jabber:iq:roster">>}});
      _res -> _res
    end.

encode_roster_item_attr_subscription(none, _acc) ->
    _acc;
encode_roster_item_attr_subscription(_val, _acc) ->
    [{<<"subscription">>, enc_enum(_val)} | _acc].

decode_roster_item_attr_ask(undefined) -> undefined;
decode_roster_item_attr_ask(_val) ->
    case catch dec_enum(_val, [subscribe]) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"ask">>, <<"item">>,
			 <<"jabber:iq:roster">>}});
      _res -> _res
    end.

encode_roster_item_attr_ask(undefined, _acc) -> _acc;
encode_roster_item_attr_ask(_val, _acc) ->
    [{<<"ask">>, enc_enum(_val)} | _acc].

decode_roster_group({xmlel, <<"group">>, _attrs,
		     _els}) ->
    Cdata = decode_roster_group_els(_els, <<>>), Cdata.

decode_roster_group_els([], Cdata) ->
    decode_roster_group_cdata(Cdata);
decode_roster_group_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_roster_group_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_roster_group_els([_ | _els], Cdata) ->
    decode_roster_group_els(_els, Cdata).

encode_roster_group(Cdata, _xmlns_attrs) ->
    _els = encode_roster_group_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"group">>, _attrs, _els}.

decode_roster_group_cdata(<<>>) ->
    erlang:error({xmpp_codec,
		  {missing_cdata, <<>>, <<"group">>,
		   <<"jabber:iq:roster">>}});
decode_roster_group_cdata(_val) -> _val.

encode_roster_group_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_version({xmlel, <<"query">>, _attrs, _els}) ->
    {Ver, Os, Name} = decode_version_els(_els, undefined,
					 undefined, undefined),
    {version, Name, Ver, Os}.

decode_version_els([], Ver, Os, Name) ->
    {Ver, Os, Name};
decode_version_els([{xmlel, <<"name">>, _attrs, _} = _el
		    | _els],
		   Ver, Os, Name) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:version">> ->
	   decode_version_els(_els, Ver, Os,
			      decode_version_name(_el));
       true -> decode_version_els(_els, Ver, Os, Name)
    end;
decode_version_els([{xmlel, <<"version">>, _attrs, _} =
			_el
		    | _els],
		   Ver, Os, Name) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:version">> ->
	   decode_version_els(_els, decode_version_ver(_el), Os,
			      Name);
       true -> decode_version_els(_els, Ver, Os, Name)
    end;
decode_version_els([{xmlel, <<"os">>, _attrs, _} = _el
		    | _els],
		   Ver, Os, Name) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == <<"jabber:iq:version">> ->
	   decode_version_els(_els, Ver, decode_version_os(_el),
			      Name);
       true -> decode_version_els(_els, Ver, Os, Name)
    end;
decode_version_els([_ | _els], Ver, Os, Name) ->
    decode_version_els(_els, Ver, Os, Name).

encode_version({version, Name, Ver, Os},
	       _xmlns_attrs) ->
    _els = 'encode_version_$name'(Name,
				  'encode_version_$os'(Os,
						       'encode_version_$ver'(Ver,
									     []))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"query">>, _attrs, _els}.

'encode_version_$ver'(undefined, _acc) -> _acc;
'encode_version_$ver'(Ver, _acc) ->
    [encode_version_ver(Ver, []) | _acc].

'encode_version_$os'(undefined, _acc) -> _acc;
'encode_version_$os'(Os, _acc) ->
    [encode_version_os(Os, []) | _acc].

'encode_version_$name'(undefined, _acc) -> _acc;
'encode_version_$name'(Name, _acc) ->
    [encode_version_name(Name, []) | _acc].

decode_version_os({xmlel, <<"os">>, _attrs, _els}) ->
    Cdata = decode_version_os_els(_els, <<>>), Cdata.

decode_version_os_els([], Cdata) ->
    decode_version_os_cdata(Cdata);
decode_version_os_els([{xmlcdata, _data} | _els],
		      Cdata) ->
    decode_version_os_els(_els,
			  <<Cdata/binary, _data/binary>>);
decode_version_os_els([_ | _els], Cdata) ->
    decode_version_os_els(_els, Cdata).

encode_version_os(Cdata, _xmlns_attrs) ->
    _els = encode_version_os_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"os">>, _attrs, _els}.

decode_version_os_cdata(<<>>) ->
    erlang:error({xmpp_codec,
		  {missing_cdata, <<>>, <<"os">>,
		   <<"jabber:iq:version">>}});
decode_version_os_cdata(_val) -> _val.

encode_version_os_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_version_ver({xmlel, <<"version">>, _attrs,
		    _els}) ->
    Cdata = decode_version_ver_els(_els, <<>>), Cdata.

decode_version_ver_els([], Cdata) ->
    decode_version_ver_cdata(Cdata);
decode_version_ver_els([{xmlcdata, _data} | _els],
		       Cdata) ->
    decode_version_ver_els(_els,
			   <<Cdata/binary, _data/binary>>);
decode_version_ver_els([_ | _els], Cdata) ->
    decode_version_ver_els(_els, Cdata).

encode_version_ver(Cdata, _xmlns_attrs) ->
    _els = encode_version_ver_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"version">>, _attrs, _els}.

decode_version_ver_cdata(<<>>) ->
    erlang:error({xmpp_codec,
		  {missing_cdata, <<>>, <<"version">>,
		   <<"jabber:iq:version">>}});
decode_version_ver_cdata(_val) -> _val.

encode_version_ver_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_version_name({xmlel, <<"name">>, _attrs,
		     _els}) ->
    Cdata = decode_version_name_els(_els, <<>>), Cdata.

decode_version_name_els([], Cdata) ->
    decode_version_name_cdata(Cdata);
decode_version_name_els([{xmlcdata, _data} | _els],
			Cdata) ->
    decode_version_name_els(_els,
			    <<Cdata/binary, _data/binary>>);
decode_version_name_els([_ | _els], Cdata) ->
    decode_version_name_els(_els, Cdata).

encode_version_name(Cdata, _xmlns_attrs) ->
    _els = encode_version_name_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"name">>, _attrs, _els}.

decode_version_name_cdata(<<>>) ->
    erlang:error({xmpp_codec,
		  {missing_cdata, <<>>, <<"name">>,
		   <<"jabber:iq:version">>}});
decode_version_name_cdata(_val) -> _val.

encode_version_name_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_last({xmlel, <<"query">>, _attrs, _els}) ->
    Text = decode_last_els(_els, <<>>),
    Seconds = decode_last_attrs(_attrs, undefined),
    {last, Seconds, Text}.

decode_last_els([], Text) -> decode_last_cdata(Text);
decode_last_els([{xmlcdata, _data} | _els], Text) ->
    decode_last_els(_els, <<Text/binary, _data/binary>>);
decode_last_els([_ | _els], Text) ->
    decode_last_els(_els, Text).

decode_last_attrs([{<<"seconds">>, _val} | _attrs],
		  _Seconds) ->
    decode_last_attrs(_attrs, _val);
decode_last_attrs([_ | _attrs], Seconds) ->
    decode_last_attrs(_attrs, Seconds);
decode_last_attrs([], Seconds) ->
    decode_last_attr_seconds(Seconds).

encode_last({last, Seconds, Text}, _xmlns_attrs) ->
    _els = encode_last_cdata(Text, []),
    _attrs = encode_last_attr_seconds(Seconds,
				      _xmlns_attrs),
    {xmlel, <<"query">>, _attrs, _els}.

decode_last_attr_seconds(undefined) -> undefined;
decode_last_attr_seconds(_val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"seconds">>, <<"query">>,
			 <<"jabber:iq:last">>}});
      _res -> _res
    end.

encode_last_attr_seconds(undefined, _acc) -> _acc;
encode_last_attr_seconds(_val, _acc) ->
    [{<<"seconds">>, enc_int(_val)} | _acc].

decode_last_cdata(<<>>) -> undefined;
decode_last_cdata(_val) -> _val.

encode_last_cdata(undefined, _acc) -> _acc;
encode_last_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].
