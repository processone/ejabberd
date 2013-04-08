-module(xmpp_codec).

-export([decode/1, encode/1]).

decode({xmlel, _name, _attrs, _} = _el) ->
    case {_name, xml:get_attr_s(<<"xmlns">>, _attrs)} of
      {<<"delay">>, <<"urn:xmpp:delay">>} ->
	  decode_delay_delay(_el);
      {<<"pubsub">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_pubsub(_el);
      {<<"event">>,
       <<"http://jabber.org/protocol/pubsub#event">>} ->
	  decode_pubsub_event_event(_el);
      {<<"items">>, <<>>} -> decode_pubsub_items_items(_el);
      {<<"item">>, <<>>} -> decode_pubsub_item_item(_el);
      {<<"affiliation">>, <<>>} ->
	  decode_pubsub_affiliation_affiliation(_el);
      {<<"subscription">>, <<>>} ->
	  decode_pubsub_subscription_subscription(_el);
      {<<"x">>, <<"jabber:x:data">>} -> decode_xdata_x(_el);
      {<<"field">>, <<>>} -> decode_xfield_field(_el);
      {<<"vCard">>, <<"vcard-temp">>} ->
	  decode_vcard_vCard(_el);
      {<<"KEY">>, <<>>} -> decode_vcard_key_KEY(_el);
      {<<"SOUND">>, <<>>} -> decode_vcard_sound_SOUND(_el);
      {<<"ORG">>, <<>>} -> decode_vcard_org_ORG(_el);
      {<<"PHOTO">>, <<>>} -> decode_vcard_photo_PHOTO(_el);
      {<<"LOGO">>, <<>>} -> decode_vcard_logo_LOGO(_el);
      {<<"EXTVAL">>, <<>>} -> decode_vcard_extval_EXTVAL(_el);
      {<<"BINVAL">>, <<>>} -> decode_vcard_binval_BINVAL(_el);
      {<<"TYPE">>, <<>>} -> decode_vcard_type_TYPE(_el);
      {<<"GEO">>, <<>>} -> decode_vcard_geo_GEO(_el);
      {<<"EMAIL">>, <<>>} -> decode_vcard_email_EMAIL(_el);
      {<<"TEL">>, <<>>} -> decode_vcard_tel_TEL(_el);
      {<<"LABEL">>, <<>>} -> decode_vcard_label_LABEL(_el);
      {<<"ADR">>, <<>>} -> decode_vcard_adr_ADR(_el);
      {<<"N">>, <<>>} -> decode_vcard_name_N(_el);
      {<<"stream:error">>, <<>>} ->
	  'decode_stream_error_stream:error'(_el);
      {<<"time">>, <<"urn:xmpp:time">>} ->
	  decode_time_time(_el);
      {<<"ping">>, <<"urn:xmpp:ping">>} ->
	  decode_ping_ping(_el);
      {<<"session">>,
       <<"urn:ietf:params:xml:ns:xmpp-session">>} ->
	  decode_session_session(_el);
      {<<"register">>,
       <<"http://jabber.org/features/iq-register">>} ->
	  decode_register_register(_el);
      {<<"c">>, <<"http://jabber.org/protocol/caps">>} ->
	  decode_caps_c(_el);
      {<<"ack">>, <<"p1:ack">>} -> decode_p1_ack_ack(_el);
      {<<"rebind">>, <<"p1:rebind">>} ->
	  decode_p1_rebind_rebind(_el);
      {<<"push">>, <<"p1:push">>} -> decode_p1_push_push(_el);
      {<<"stream:features">>, <<>>} ->
	  'decode_stream_features_stream:features'(_el);
      {<<"failure">>,
       <<"urn:ietf:params:xml:ns:xmpp-tls">>} ->
	  decode_starttls_failure_failure(_el);
      {<<"proceed">>,
       <<"urn:ietf:params:xml:ns:xmpp-tls">>} ->
	  decode_starttls_proceed_proceed(_el);
      {<<"starttls">>,
       <<"urn:ietf:params:xml:ns:xmpp-tls">>} ->
	  decode_starttls_starttls(_el);
      {<<"mechanisms">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_mechanisms_mechanisms(_el);
      {<<"mechanism">>, <<>>} ->
	  decode_sasl_mechanism_mechanism(_el);
      {<<"failure">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_failure(_el);
      {<<"success">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_success_success(_el);
      {<<"response">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_response_response(_el);
      {<<"challenge">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_challenge_challenge(_el);
      {<<"abort">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_abort_abort(_el);
      {<<"auth">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_auth_auth(_el);
      {<<"bind">>, <<"urn:ietf:params:xml:ns:xmpp-bind">>} ->
	  decode_bind_bind(_el);
      {<<"error">>, <<>>} -> decode_error_error(_el);
      {<<"presence">>, <<>>} -> decode_presence_presence(_el);
      {<<"message">>, <<>>} -> decode_message_message(_el);
      {<<"iq">>, <<>>} -> decode_iq_iq(_el);
      {<<"query">>, <<"http://jabber.org/protocol/stats">>} ->
	  decode_stats_query(_el);
      {<<"storage">>, <<"storage:bookmarks">>} ->
	  decode_storage_bookmarks_storage(_el);
      {<<"conference">>, <<>>} ->
	  decode_bookmark_conference_conference(_el);
      {<<"query">>, <<"jabber:iq:private">>} ->
	  decode_private_query(_el);
      {<<"query">>,
       <<"http://jabber.org/protocol/disco#items">>} ->
	  decode_disco_items_query(_el);
      {<<"query">>,
       <<"http://jabber.org/protocol/disco#info">>} ->
	  decode_disco_info_query(_el);
      {<<"blocklist">>, <<"urn:xmpp:blocking">>} ->
	  decode_block_list_blocklist(_el);
      {<<"unblock">>, <<"urn:xmpp:blocking">>} ->
	  decode_unblock_unblock(_el);
      {<<"block">>, <<"urn:xmpp:blocking">>} ->
	  decode_block_block(_el);
      {<<"item">>, <<>>} -> decode_block_item_item(_el);
      {<<"query">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_query(_el);
      {<<"item">>, <<>>} -> decode_privacy_item_item(_el);
      {<<"query">>, <<"jabber:iq:roster">>} ->
	  decode_roster_query(_el);
      {<<"query">>, <<"jabber:iq:version">>} ->
	  decode_version_query(_el);
      {<<"query">>, <<"jabber:iq:last">>} ->
	  decode_last_query(_el);
      {_name, _xmlns} ->
	  erlang:error({unknown_tag, _name, _xmlns})
    end.

encode({delay, _, _} = _r) ->
    hd(encode_delay_delay(_r, []));
encode({pubsub, _, _, _, _} = _r) ->
    hd(encode_pubsub_pubsub(_r, []));
encode({pubsub_event, _} = _r) ->
    hd(encode_pubsub_event_event(_r, []));
encode({pubsub_items, _, _, _, _} = _r) ->
    hd(encode_pubsub_items_items(_r, []));
encode({pubsub_item, _, _} = _r) ->
    hd(encode_pubsub_item_item(_r, []));
encode({pubsub_affiliation, _, _} = _r) ->
    hd(encode_pubsub_affiliation_affiliation(_r, []));
encode({pubsub_subscription, _, _, _, _} = _r) ->
    hd(encode_pubsub_subscription_subscription(_r, []));
encode({xdata, _, _, _, _, _, _} = _r) ->
    hd(encode_xdata_x(_r, []));
encode({xfield, _, _, _, _, _, _, _} = _r) ->
    hd(encode_xfield_field(_r, []));
encode({vcard, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
	_, _, _, _, _, _, _, _, _, _, _, _, _, _, _} =
	   _r) ->
    hd(encode_vcard_vCard(_r, []));
encode({vcard_key, _, _} = _r) ->
    hd(encode_vcard_key_KEY(_r, []));
encode({vcard_sound, _, _, _} = _r) ->
    hd(encode_vcard_sound_SOUND(_r, []));
encode({vcard_org, _, _} = _r) ->
    hd(encode_vcard_org_ORG(_r, []));
encode({vcard_photo, _, _, _} = _r) ->
    hd(encode_vcard_photo_PHOTO(_r, []));
encode({vcard_logo, _, _, _} = _r) ->
    hd(encode_vcard_logo_LOGO(_r, []));
encode({vcard_geo, _, _} = _r) ->
    hd(encode_vcard_geo_GEO(_r, []));
encode({vcard_email, _, _, _, _, _, _} = _r) ->
    hd(encode_vcard_email_EMAIL(_r, []));
encode({vcard_tel, _, _, _, _, _, _, _, _, _, _, _, _,
	_, _} =
	   _r) ->
    hd(encode_vcard_tel_TEL(_r, []));
encode({vcard_label, _, _, _, _, _, _, _, _} = _r) ->
    hd(encode_vcard_label_LABEL(_r, []));
encode({vcard_adr, _, _, _, _, _, _, _, _, _, _, _, _,
	_, _} =
	   _r) ->
    hd(encode_vcard_adr_ADR(_r, []));
encode({vcard_name, _, _, _, _, _} = _r) ->
    hd(encode_vcard_name_N(_r, []));
encode({stream_error, _, _} = _r) ->
    hd('encode_stream_error_stream:error'(_r, []));
encode({time, _, _} = _r) ->
    hd(encode_time_time(_r, []));
encode({ping} = _r) -> hd(encode_ping_ping(_r, []));
encode({session} = _r) ->
    hd(encode_session_session(_r, []));
encode({register} = _r) ->
    hd(encode_register_register(_r, []));
encode({caps, _, _, _} = _r) ->
    hd(encode_caps_c(_r, []));
encode({p1_ack} = _r) -> hd(encode_p1_ack_ack(_r, []));
encode({p1_rebind} = _r) ->
    hd(encode_p1_rebind_rebind(_r, []));
encode({p1_push} = _r) ->
    hd(encode_p1_push_push(_r, []));
encode({stream_features, _} = _r) ->
    hd('encode_stream_features_stream:features'(_r, []));
encode({starttls_failure} = _r) ->
    hd(encode_starttls_failure_failure(_r, []));
encode({starttls_proceed} = _r) ->
    hd(encode_starttls_proceed_proceed(_r, []));
encode({starttls, _} = _r) ->
    hd(encode_starttls_starttls(_r, []));
encode({sasl_mechanisms, _} = _r) ->
    hd(encode_sasl_mechanisms_mechanisms(_r, []));
encode({sasl_failure, _, _} = _r) ->
    hd(encode_sasl_failure_failure(_r, []));
encode({sasl_success, _} = _r) ->
    hd(encode_sasl_success_success(_r, []));
encode({sasl_response, _} = _r) ->
    hd(encode_sasl_response_response(_r, []));
encode({sasl_challenge, _} = _r) ->
    hd(encode_sasl_challenge_challenge(_r, []));
encode({sasl_abort} = _r) ->
    hd(encode_sasl_abort_abort(_r, []));
encode({sasl_auth, _, _} = _r) ->
    hd(encode_sasl_auth_auth(_r, []));
encode({bind, _, _} = _r) ->
    hd(encode_bind_bind(_r, []));
encode({error, _, _, _, _} = _r) ->
    hd(encode_error_error(_r, []));
encode({presence, _, _, _, _, _, _, _, _, _, _} = _r) ->
    hd(encode_presence_presence(_r, []));
encode({message, _, _, _, _, _, _, _, _, _, _} = _r) ->
    hd(encode_message_message(_r, []));
encode({iq, _, _, _, _, _, _, _} = _r) ->
    hd(encode_iq_iq(_r, []));
encode({stats, _} = _r) ->
    hd(encode_stats_query(_r, []));
encode({bookmark_storage, _, _} = _r) ->
    hd(encode_storage_bookmarks_storage(_r, []));
encode({bookmark_conference, _, _, _, _, _} = _r) ->
    hd(encode_bookmark_conference_conference(_r, []));
encode({private, _} = _r) ->
    hd(encode_private_query(_r, []));
encode({disco_items, _, _} = _r) ->
    hd(encode_disco_items_query(_r, []));
encode({disco_info, _, _, _, _} = _r) ->
    hd(encode_disco_info_query(_r, []));
encode({block_list} = _r) ->
    hd(encode_block_list_blocklist(_r, []));
encode({unblock, _} = _r) ->
    hd(encode_unblock_unblock(_r, []));
encode({block, _} = _r) ->
    hd(encode_block_block(_r, []));
encode({privacy, _, _, _} = _r) ->
    hd(encode_privacy_query(_r, []));
encode({privacy_item, _, _, _, _, _} = _r) ->
    hd(encode_privacy_item_item(_r, []));
encode({roster, _, _} = _r) ->
    hd(encode_roster_query(_r, []));
encode({version, _, _, _} = _r) ->
    hd(encode_version_query(_r, []));
encode({last, _, _} = _r) ->
    hd(encode_last_query(_r, [])).

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
    H = erlang:binary_to_integer(H1),
    M = erlang:binary_to_integer(M1),
    if H >= -12, H =< 12, M >= 0, M < 60 -> {H, M} end.

decode_last_query({xmlel, _, _attrs, _els}) ->
    Seconds = decode_last_query_attrs(_attrs, undefined),
    Text = decode_last_query_els(_els, <<>>),
    {last, Seconds, Text}.

decode_last_query_els([{xmlcdata, _data} | _els],
		      Text) ->
    decode_last_query_els(_els,
			  <<Text/binary, _data/binary>>);
decode_last_query_els([_ | _els], Text) ->
    decode_last_query_els(_els, Text);
decode_last_query_els([], Text) ->
    decode_last_query_cdata(Text).

decode_last_query_attrs([{<<"seconds">>, _val}
			 | _attrs],
			_Seconds) ->
    decode_last_query_attrs(_attrs, _val);
decode_last_query_attrs([_ | _attrs], Seconds) ->
    decode_last_query_attrs(_attrs, Seconds);
decode_last_query_attrs([], Seconds) ->
    decode_last_query_seconds(Seconds).

encode_last_query(undefined, _acc) -> _acc;
encode_last_query({last, Seconds, Text}, _acc) ->
    _els = encode_last_query_cdata(Text, []),
    _attrs = encode_last_query_seconds(Seconds,
				       [{<<"xmlns">>, <<"jabber:iq:last">>}]),
    [{xmlel, <<"query">>, _attrs, _els} | _acc].

decode_last_query_seconds(undefined) -> undefined;
decode_last_query_seconds(_val) ->
    case catch xml_gen:dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"seconds">>,
			<<"query">>, <<"jabber:iq:last">>});
      _res -> _res
    end.

encode_last_query_seconds(undefined, _acc) -> _acc;
encode_last_query_seconds(_val, _acc) ->
    [{<<"seconds">>, xml_gen:enc_int(_val)} | _acc].

decode_last_query_cdata(<<>>) -> undefined;
decode_last_query_cdata(_val) -> _val.

encode_last_query_cdata(undefined, _acc) -> _acc;
encode_last_query_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_version_query({xmlel, _, _attrs, _els}) ->
    {Os, Version, Name} = decode_version_query_els(_els,
						   undefined, undefined,
						   undefined),
    {version, Name, Version, Os}.

decode_version_query_els([{xmlel, <<"os">>, _attrs, _} =
			      _el
			  | _els],
			 Os, Version, Name) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_version_query_els(_els,
				   decode_version_query_os(_el), Version, Name);
      _ -> decode_version_query_els(_els, Os, Version, Name)
    end;
decode_version_query_els([{xmlel, <<"version">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Os, Version, Name) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_version_query_els(_els, Os,
				   decode_version_query_version(_el), Name);
      _ -> decode_version_query_els(_els, Os, Version, Name)
    end;
decode_version_query_els([{xmlel, <<"name">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Os, Version, Name) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_version_query_els(_els, Os, Version,
				   decode_version_query_name(_el));
      _ -> decode_version_query_els(_els, Os, Version, Name)
    end;
decode_version_query_els([_ | _els], Os, Version,
			 Name) ->
    decode_version_query_els(_els, Os, Version, Name);
decode_version_query_els([], Os, Version, Name) ->
    {Os, Version, Name}.

encode_version_query(undefined, _acc) -> _acc;
encode_version_query({version, Name, Version, Os},
		     _acc) ->
    _els = encode_version_query_name(Name,
				     encode_version_query_version(Version,
								  encode_version_query_os(Os,
											  []))),
    _attrs = [{<<"xmlns">>, <<"jabber:iq:version">>}],
    [{xmlel, <<"query">>, _attrs, _els} | _acc].

decode_version_query_os({xmlel, _, _attrs, _els}) ->
    Cdata = decode_version_query_os_els(_els, <<>>), Cdata.

decode_version_query_os_els([{xmlcdata, _data} | _els],
			    Cdata) ->
    decode_version_query_os_els(_els,
				<<Cdata/binary, _data/binary>>);
decode_version_query_os_els([_ | _els], Cdata) ->
    decode_version_query_os_els(_els, Cdata);
decode_version_query_os_els([], Cdata) ->
    decode_version_query_os_cdata(Cdata).

encode_version_query_os(undefined, _acc) -> _acc;
encode_version_query_os(Cdata, _acc) ->
    _els = encode_version_query_os_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"os">>, _attrs, _els} | _acc].

decode_version_query_os_cdata(<<>>) ->
    erlang:error({missing_cdata, <<>>, <<"os">>, <<>>});
decode_version_query_os_cdata(_val) -> _val.

encode_version_query_os_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_version_query_version({xmlel, _, _attrs,
			      _els}) ->
    Cdata = decode_version_query_version_els(_els, <<>>),
    Cdata.

decode_version_query_version_els([{xmlcdata, _data}
				  | _els],
				 Cdata) ->
    decode_version_query_version_els(_els,
				     <<Cdata/binary, _data/binary>>);
decode_version_query_version_els([_ | _els], Cdata) ->
    decode_version_query_version_els(_els, Cdata);
decode_version_query_version_els([], Cdata) ->
    decode_version_query_version_cdata(Cdata).

encode_version_query_version(undefined, _acc) -> _acc;
encode_version_query_version(Cdata, _acc) ->
    _els = encode_version_query_version_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"version">>, _attrs, _els} | _acc].

decode_version_query_version_cdata(<<>>) ->
    erlang:error({missing_cdata, <<>>, <<"version">>,
		  <<>>});
decode_version_query_version_cdata(_val) -> _val.

encode_version_query_version_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_version_query_name({xmlel, _, _attrs, _els}) ->
    Cdata = decode_version_query_name_els(_els, <<>>),
    Cdata.

decode_version_query_name_els([{xmlcdata, _data}
			       | _els],
			      Cdata) ->
    decode_version_query_name_els(_els,
				  <<Cdata/binary, _data/binary>>);
decode_version_query_name_els([_ | _els], Cdata) ->
    decode_version_query_name_els(_els, Cdata);
decode_version_query_name_els([], Cdata) ->
    decode_version_query_name_cdata(Cdata).

encode_version_query_name(undefined, _acc) -> _acc;
encode_version_query_name(Cdata, _acc) ->
    _els = encode_version_query_name_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"name">>, _attrs, _els} | _acc].

decode_version_query_name_cdata(<<>>) ->
    erlang:error({missing_cdata, <<>>, <<"name">>, <<>>});
decode_version_query_name_cdata(_val) -> _val.

encode_version_query_name_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_roster_query({xmlel, _, _attrs, _els}) ->
    Ver = decode_roster_query_attrs(_attrs, undefined),
    Item = decode_roster_query_els(_els, []),
    {roster, Item, Ver}.

decode_roster_query_els([{xmlel, <<"item">>, _attrs,
			  _} =
			     _el
			 | _els],
			Item) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_roster_query_els(_els,
				  [decode_roster_query_item(_el) | Item]);
      _ -> decode_roster_query_els(_els, Item)
    end;
decode_roster_query_els([_ | _els], Item) ->
    decode_roster_query_els(_els, Item);
decode_roster_query_els([], Item) ->
    lists:reverse(Item).

decode_roster_query_attrs([{<<"ver">>, _val} | _attrs],
			  _Ver) ->
    decode_roster_query_attrs(_attrs, _val);
decode_roster_query_attrs([_ | _attrs], Ver) ->
    decode_roster_query_attrs(_attrs, Ver);
decode_roster_query_attrs([], Ver) ->
    decode_roster_query_ver(Ver).

encode_roster_query(undefined, _acc) -> _acc;
encode_roster_query({roster, Item, Ver}, _acc) ->
    _els = encode_roster_query_item(Item, []),
    _attrs = encode_roster_query_ver(Ver,
				     [{<<"xmlns">>, <<"jabber:iq:roster">>}]),
    [{xmlel, <<"query">>, _attrs, _els} | _acc].

decode_roster_query_ver(undefined) -> undefined;
decode_roster_query_ver(_val) -> _val.

encode_roster_query_ver(undefined, _acc) -> _acc;
encode_roster_query_ver(_val, _acc) ->
    [{<<"ver">>, _val} | _acc].

decode_roster_query_item({xmlel, _, _attrs, _els}) ->
    {Ask, Subscription, Name, Jid} =
	decode_roster_query_item_attrs(_attrs, undefined,
				       undefined, undefined, undefined),
    Groups = decode_roster_query_item_els(_els, []),
    {roster_item, Jid, Name, Groups, Subscription, Ask}.

decode_roster_query_item_els([{xmlel, <<"group">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Groups) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_roster_query_item_els(_els,
				       [decode_roster_query_item_group(_el)
					| Groups]);
      _ -> decode_roster_query_item_els(_els, Groups)
    end;
decode_roster_query_item_els([_ | _els], Groups) ->
    decode_roster_query_item_els(_els, Groups);
decode_roster_query_item_els([], Groups) ->
    lists:reverse(Groups).

decode_roster_query_item_attrs([{<<"ask">>, _val}
				| _attrs],
			       _Ask, Subscription, Name, Jid) ->
    decode_roster_query_item_attrs(_attrs, _val,
				   Subscription, Name, Jid);
decode_roster_query_item_attrs([{<<"subscription">>,
				 _val}
				| _attrs],
			       Ask, _Subscription, Name, Jid) ->
    decode_roster_query_item_attrs(_attrs, Ask, _val, Name,
				   Jid);
decode_roster_query_item_attrs([{<<"name">>, _val}
				| _attrs],
			       Ask, Subscription, _Name, Jid) ->
    decode_roster_query_item_attrs(_attrs, Ask,
				   Subscription, _val, Jid);
decode_roster_query_item_attrs([{<<"jid">>, _val}
				| _attrs],
			       Ask, Subscription, Name, _Jid) ->
    decode_roster_query_item_attrs(_attrs, Ask,
				   Subscription, Name, _val);
decode_roster_query_item_attrs([_ | _attrs], Ask,
			       Subscription, Name, Jid) ->
    decode_roster_query_item_attrs(_attrs, Ask,
				   Subscription, Name, Jid);
decode_roster_query_item_attrs([], Ask, Subscription,
			       Name, Jid) ->
    {decode_roster_query_item_ask(Ask),
     decode_roster_query_item_subscription(Subscription),
     decode_roster_query_item_name(Name),
     decode_roster_query_item_jid(Jid)}.

encode_roster_query_item([], _acc) -> _acc;
encode_roster_query_item([{roster_item, Jid, Name,
			   Groups, Subscription, Ask}
			  | _tail],
			 _acc) ->
    _els = encode_roster_query_item_group(Groups, []),
    _attrs = encode_roster_query_item_jid(Jid,
					  encode_roster_query_item_name(Name,
									encode_roster_query_item_subscription(Subscription,
													      encode_roster_query_item_ask(Ask,
																	   [])))),
    encode_roster_query_item(_tail,
			     [{xmlel, <<"item">>, _attrs, _els} | _acc]).

decode_roster_query_item_jid(undefined) ->
    erlang:error({missing_attr, <<"jid">>, <<"item">>,
		  <<>>});
decode_roster_query_item_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"jid">>, <<"item">>,
			<<>>});
      _res -> _res
    end.

encode_roster_query_item_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_roster_query_item_name(undefined) -> undefined;
decode_roster_query_item_name(_val) -> _val.

encode_roster_query_item_name(undefined, _acc) -> _acc;
encode_roster_query_item_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_roster_query_item_subscription(undefined) ->
    none;
decode_roster_query_item_subscription(_val) ->
    case catch xml_gen:dec_enum(_val,
				[none, to, from, both, remove])
	of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"subscription">>,
			<<"item">>, <<>>});
      _res -> _res
    end.

encode_roster_query_item_subscription(none, _acc) ->
    _acc;
encode_roster_query_item_subscription(_val, _acc) ->
    [{<<"subscription">>, xml_gen:enc_enum(_val)} | _acc].

decode_roster_query_item_ask(undefined) -> undefined;
decode_roster_query_item_ask(_val) ->
    case catch xml_gen:dec_enum(_val, [subscribe]) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"ask">>, <<"item">>,
			<<>>});
      _res -> _res
    end.

encode_roster_query_item_ask(undefined, _acc) -> _acc;
encode_roster_query_item_ask(_val, _acc) ->
    [{<<"ask">>, xml_gen:enc_enum(_val)} | _acc].

decode_roster_query_item_group({xmlel, _, _attrs,
				_els}) ->
    Cdata = decode_roster_query_item_group_els(_els, <<>>),
    Cdata.

decode_roster_query_item_group_els([{xmlcdata, _data}
				    | _els],
				   Cdata) ->
    decode_roster_query_item_group_els(_els,
				       <<Cdata/binary, _data/binary>>);
decode_roster_query_item_group_els([_ | _els], Cdata) ->
    decode_roster_query_item_group_els(_els, Cdata);
decode_roster_query_item_group_els([], Cdata) ->
    decode_roster_query_item_group_cdata(Cdata).

encode_roster_query_item_group([], _acc) -> _acc;
encode_roster_query_item_group([Cdata | _tail], _acc) ->
    _els = encode_roster_query_item_group_cdata(Cdata, []),
    _attrs = [],
    encode_roster_query_item_group(_tail,
				   [{xmlel, <<"group">>, _attrs, _els} | _acc]).

decode_roster_query_item_group_cdata(<<>>) ->
    erlang:error({missing_cdata, <<>>, <<"group">>, <<>>});
decode_roster_query_item_group_cdata(_val) -> _val.

encode_roster_query_item_group_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_privacy_item_item({xmlel, _, _attrs, _els}) ->
    {Value, Type, Action, Order} =
	decode_privacy_item_item_attrs(_attrs, undefined,
				       undefined, undefined, undefined),
    Stanza = decode_privacy_item_item_els(_els, undefined),
    {privacy_item, Order, Action, Type, Value, Stanza}.

decode_privacy_item_item_els([{xmlel,
			       <<"presence-out">>, _attrs, _} =
				  _el
			      | _els],
			     Stanza) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_privacy_item_item_els(_els,
				       'decode_privacy_item_item_presence-out'(_el));
      _ -> decode_privacy_item_item_els(_els, Stanza)
    end;
decode_privacy_item_item_els([{xmlel, <<"presence-in">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Stanza) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_privacy_item_item_els(_els,
				       'decode_privacy_item_item_presence-in'(_el));
      _ -> decode_privacy_item_item_els(_els, Stanza)
    end;
decode_privacy_item_item_els([{xmlel, <<"iq">>, _attrs,
			       _} =
				  _el
			      | _els],
			     Stanza) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_privacy_item_item_els(_els,
				       decode_privacy_item_item_iq(_el));
      _ -> decode_privacy_item_item_els(_els, Stanza)
    end;
decode_privacy_item_item_els([{xmlel, <<"message">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Stanza) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_privacy_item_item_els(_els,
				       decode_privacy_item_item_message(_el));
      _ -> decode_privacy_item_item_els(_els, Stanza)
    end;
decode_privacy_item_item_els([_ | _els], Stanza) ->
    decode_privacy_item_item_els(_els, Stanza);
decode_privacy_item_item_els([], Stanza) -> Stanza.

decode_privacy_item_item_attrs([{<<"value">>, _val}
				| _attrs],
			       _Value, Type, Action, Order) ->
    decode_privacy_item_item_attrs(_attrs, _val, Type,
				   Action, Order);
decode_privacy_item_item_attrs([{<<"type">>, _val}
				| _attrs],
			       Value, _Type, Action, Order) ->
    decode_privacy_item_item_attrs(_attrs, Value, _val,
				   Action, Order);
decode_privacy_item_item_attrs([{<<"action">>, _val}
				| _attrs],
			       Value, Type, _Action, Order) ->
    decode_privacy_item_item_attrs(_attrs, Value, Type,
				   _val, Order);
decode_privacy_item_item_attrs([{<<"order">>, _val}
				| _attrs],
			       Value, Type, Action, _Order) ->
    decode_privacy_item_item_attrs(_attrs, Value, Type,
				   Action, _val);
decode_privacy_item_item_attrs([_ | _attrs], Value,
			       Type, Action, Order) ->
    decode_privacy_item_item_attrs(_attrs, Value, Type,
				   Action, Order);
decode_privacy_item_item_attrs([], Value, Type, Action,
			       Order) ->
    {decode_privacy_item_item_value(Value),
     decode_privacy_item_item_type(Type),
     decode_privacy_item_item_action(Action),
     decode_privacy_item_item_order(Order)}.

'encode_privacy_item_item_$stanza'(undefined, _acc) ->
    _acc;
'encode_privacy_item_item_$stanza'('presence-out' = _r,
				   _acc) ->
    'encode_privacy_item_item_presence-out'(_r, _acc);
'encode_privacy_item_item_$stanza'('presence-in' = _r,
				   _acc) ->
    'encode_privacy_item_item_presence-in'(_r, _acc);
'encode_privacy_item_item_$stanza'(iq = _r, _acc) ->
    encode_privacy_item_item_iq(_r, _acc);
'encode_privacy_item_item_$stanza'(message = _r,
				   _acc) ->
    encode_privacy_item_item_message(_r, _acc).

encode_privacy_item_item([], _acc) -> _acc;
encode_privacy_item_item([{privacy_item, Order, Action,
			   Type, Value, Stanza}
			  | _tail],
			 _acc) ->
    _els = 'encode_privacy_item_item_$stanza'(Stanza, []),
    _attrs = encode_privacy_item_item_order(Order,
					    encode_privacy_item_item_action(Action,
									    encode_privacy_item_item_type(Type,
													  encode_privacy_item_item_value(Value,
																	 [])))),
    encode_privacy_item_item(_tail,
			     [{xmlel, <<"item">>, _attrs, _els} | _acc]).

decode_privacy_item_item_action(undefined) ->
    erlang:error({missing_attr, <<"action">>, <<"item">>,
		  <<>>});
decode_privacy_item_item_action(_val) ->
    case catch xml_gen:dec_enum(_val, [allow, deny]) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"action">>, <<"item">>,
			<<>>});
      _res -> _res
    end.

encode_privacy_item_item_action(_val, _acc) ->
    [{<<"action">>, xml_gen:enc_enum(_val)} | _acc].

decode_privacy_item_item_order(undefined) ->
    erlang:error({missing_attr, <<"order">>, <<"item">>,
		  <<>>});
decode_privacy_item_item_order(_val) ->
    case catch xml_gen:dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"order">>, <<"item">>,
			<<>>});
      _res -> _res
    end.

encode_privacy_item_item_order(_val, _acc) ->
    [{<<"order">>, xml_gen:enc_int(_val)} | _acc].

decode_privacy_item_item_type(undefined) -> undefined;
decode_privacy_item_item_type(_val) ->
    case catch xml_gen:dec_enum(_val,
				[group, jid, subscription])
	of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"type">>, <<"item">>,
			<<>>});
      _res -> _res
    end.

encode_privacy_item_item_type(undefined, _acc) -> _acc;
encode_privacy_item_item_type(_val, _acc) ->
    [{<<"type">>, xml_gen:enc_enum(_val)} | _acc].

decode_privacy_item_item_value(undefined) -> undefined;
decode_privacy_item_item_value(_val) -> _val.

encode_privacy_item_item_value(undefined, _acc) -> _acc;
encode_privacy_item_item_value(_val, _acc) ->
    [{<<"value">>, _val} | _acc].

'decode_privacy_item_item_presence-out'({xmlel, _,
					 _attrs, _els}) ->
    'presence-out'.

'encode_privacy_item_item_presence-out'(undefined,
					_acc) ->
    _acc;
'encode_privacy_item_item_presence-out'('presence-out',
					_acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"presence-out">>, _attrs, _els} | _acc].

'decode_privacy_item_item_presence-in'({xmlel, _,
					_attrs, _els}) ->
    'presence-in'.

'encode_privacy_item_item_presence-in'(undefined,
				       _acc) ->
    _acc;
'encode_privacy_item_item_presence-in'('presence-in',
				       _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"presence-in">>, _attrs, _els} | _acc].

decode_privacy_item_item_iq({xmlel, _, _attrs, _els}) ->
    iq.

encode_privacy_item_item_iq(undefined, _acc) -> _acc;
encode_privacy_item_item_iq(iq, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"iq">>, _attrs, _els} | _acc].

decode_privacy_item_item_message({xmlel, _, _attrs,
				  _els}) ->
    message.

encode_privacy_item_item_message(undefined, _acc) ->
    _acc;
encode_privacy_item_item_message(message, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"message">>, _attrs, _els} | _acc].

decode_privacy_query({xmlel, _, _attrs, _els}) ->
    {Active, Default, List} = decode_privacy_query_els(_els,
						       undefined, undefined,
						       []),
    {privacy, List, Default, Active}.

decode_privacy_query_els([{xmlel, <<"active">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Active, Default, List) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_privacy_query_els(_els,
				   decode_privacy_query_active(_el), Default,
				   List);
      _ ->
	  decode_privacy_query_els(_els, Active, Default, List)
    end;
decode_privacy_query_els([{xmlel, <<"default">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Active, Default, List) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_privacy_query_els(_els, Active,
				   decode_privacy_query_default(_el), List);
      _ ->
	  decode_privacy_query_els(_els, Active, Default, List)
    end;
decode_privacy_query_els([{xmlel, <<"list">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Active, Default, List) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_privacy_query_els(_els, Active, Default,
				   [decode_privacy_query_list(_el) | List]);
      _ ->
	  decode_privacy_query_els(_els, Active, Default, List)
    end;
decode_privacy_query_els([_ | _els], Active, Default,
			 List) ->
    decode_privacy_query_els(_els, Active, Default, List);
decode_privacy_query_els([], Active, Default, List) ->
    {Active, Default, lists:reverse(List)}.

encode_privacy_query(undefined, _acc) -> _acc;
encode_privacy_query({privacy, List, Default, Active},
		     _acc) ->
    _els = encode_privacy_query_list(List,
				     encode_privacy_query_default(Default,
								  encode_privacy_query_active(Active,
											      []))),
    _attrs = [{<<"xmlns">>, <<"jabber:iq:privacy">>}],
    [{xmlel, <<"query">>, _attrs, _els} | _acc].

decode_privacy_query_active({xmlel, _, _attrs, _els}) ->
    Name = decode_privacy_query_active_attrs(_attrs,
					     undefined),
    Name.

decode_privacy_query_active_attrs([{<<"name">>, _val}
				   | _attrs],
				  _Name) ->
    decode_privacy_query_active_attrs(_attrs, _val);
decode_privacy_query_active_attrs([_ | _attrs], Name) ->
    decode_privacy_query_active_attrs(_attrs, Name);
decode_privacy_query_active_attrs([], Name) ->
    decode_privacy_query_active_name(Name).

encode_privacy_query_active(undefined, _acc) -> _acc;
encode_privacy_query_active(Name, _acc) ->
    _els = [],
    _attrs = encode_privacy_query_active_name(Name, []),
    [{xmlel, <<"active">>, _attrs, _els} | _acc].

decode_privacy_query_active_name(undefined) -> none;
decode_privacy_query_active_name(_val) -> _val.

encode_privacy_query_active_name(none, _acc) -> _acc;
encode_privacy_query_active_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_privacy_query_default({xmlel, _, _attrs,
			      _els}) ->
    Name = decode_privacy_query_default_attrs(_attrs,
					      undefined),
    Name.

decode_privacy_query_default_attrs([{<<"name">>, _val}
				    | _attrs],
				   _Name) ->
    decode_privacy_query_default_attrs(_attrs, _val);
decode_privacy_query_default_attrs([_ | _attrs],
				   Name) ->
    decode_privacy_query_default_attrs(_attrs, Name);
decode_privacy_query_default_attrs([], Name) ->
    decode_privacy_query_default_name(Name).

encode_privacy_query_default(undefined, _acc) -> _acc;
encode_privacy_query_default(Name, _acc) ->
    _els = [],
    _attrs = encode_privacy_query_default_name(Name, []),
    [{xmlel, <<"default">>, _attrs, _els} | _acc].

decode_privacy_query_default_name(undefined) -> none;
decode_privacy_query_default_name(_val) -> _val.

encode_privacy_query_default_name(none, _acc) -> _acc;
encode_privacy_query_default_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_privacy_query_list({xmlel, _, _attrs, _els}) ->
    Name = decode_privacy_query_list_attrs(_attrs,
					   undefined),
    Privacy_item = decode_privacy_query_list_els(_els, []),
    {privacy_list, Name, Privacy_item}.

decode_privacy_query_list_els([{xmlel, <<"item">>,
				_attrs, _} =
				   _el
			       | _els],
			      Privacy_item) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_privacy_query_list_els(_els,
					[decode_privacy_item_item(_el)
					 | Privacy_item]);
      _ -> decode_privacy_query_list_els(_els, Privacy_item)
    end;
decode_privacy_query_list_els([_ | _els],
			      Privacy_item) ->
    decode_privacy_query_list_els(_els, Privacy_item);
decode_privacy_query_list_els([], Privacy_item) ->
    lists:reverse(Privacy_item).

decode_privacy_query_list_attrs([{<<"name">>, _val}
				 | _attrs],
				_Name) ->
    decode_privacy_query_list_attrs(_attrs, _val);
decode_privacy_query_list_attrs([_ | _attrs], Name) ->
    decode_privacy_query_list_attrs(_attrs, Name);
decode_privacy_query_list_attrs([], Name) ->
    decode_privacy_query_list_name(Name).

encode_privacy_query_list([], _acc) -> _acc;
encode_privacy_query_list([{privacy_list, Name,
			    Privacy_item}
			   | _tail],
			  _acc) ->
    _els = encode_privacy_item_item(Privacy_item, []),
    _attrs = encode_privacy_query_list_name(Name, []),
    encode_privacy_query_list(_tail,
			      [{xmlel, <<"list">>, _attrs, _els} | _acc]).

decode_privacy_query_list_name(undefined) ->
    erlang:error({missing_attr, <<"name">>, <<"list">>,
		  <<>>});
decode_privacy_query_list_name(_val) -> _val.

encode_privacy_query_list_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_block_item_item({xmlel, _, _attrs, _els}) ->
    Jid = decode_block_item_item_attrs(_attrs, undefined),
    Jid.

decode_block_item_item_attrs([{<<"jid">>, _val}
			      | _attrs],
			     _Jid) ->
    decode_block_item_item_attrs(_attrs, _val);
decode_block_item_item_attrs([_ | _attrs], Jid) ->
    decode_block_item_item_attrs(_attrs, Jid);
decode_block_item_item_attrs([], Jid) ->
    decode_block_item_item_jid(Jid).

encode_block_item_item([], _acc) -> _acc;
encode_block_item_item([Jid | _tail], _acc) ->
    _els = [],
    _attrs = encode_block_item_item_jid(Jid, []),
    encode_block_item_item(_tail,
			   [{xmlel, <<"item">>, _attrs, _els} | _acc]).

decode_block_item_item_jid(undefined) ->
    erlang:error({missing_attr, <<"jid">>, <<"item">>,
		  <<>>});
decode_block_item_item_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"jid">>, <<"item">>,
			<<>>});
      _res -> _res
    end.

encode_block_item_item_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_block_block({xmlel, _, _attrs, _els}) ->
    Block_item = decode_block_block_els(_els, []),
    {block, Block_item}.

decode_block_block_els([{xmlel, <<"item">>, _attrs, _} =
			    _el
			| _els],
		       Block_item) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_block_block_els(_els,
				 [decode_block_item_item(_el) | Block_item]);
      _ -> decode_block_block_els(_els, Block_item)
    end;
decode_block_block_els([_ | _els], Block_item) ->
    decode_block_block_els(_els, Block_item);
decode_block_block_els([], Block_item) ->
    lists:reverse(Block_item).

encode_block_block(undefined, _acc) -> _acc;
encode_block_block({block, Block_item}, _acc) ->
    _els = encode_block_item_item(Block_item, []),
    _attrs = [{<<"xmlns">>, <<"urn:xmpp:blocking">>}],
    [{xmlel, <<"block">>, _attrs, _els} | _acc].

decode_unblock_unblock({xmlel, _, _attrs, _els}) ->
    Block_item = decode_unblock_unblock_els(_els, []),
    {unblock, Block_item}.

decode_unblock_unblock_els([{xmlel, <<"item">>, _attrs,
			     _} =
				_el
			    | _els],
			   Block_item) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_unblock_unblock_els(_els,
				     [decode_block_item_item(_el)
				      | Block_item]);
      _ -> decode_unblock_unblock_els(_els, Block_item)
    end;
decode_unblock_unblock_els([_ | _els], Block_item) ->
    decode_unblock_unblock_els(_els, Block_item);
decode_unblock_unblock_els([], Block_item) ->
    lists:reverse(Block_item).

encode_unblock_unblock(undefined, _acc) -> _acc;
encode_unblock_unblock({unblock, Block_item}, _acc) ->
    _els = encode_block_item_item(Block_item, []),
    _attrs = [{<<"xmlns">>, <<"urn:xmpp:blocking">>}],
    [{xmlel, <<"unblock">>, _attrs, _els} | _acc].

decode_block_list_blocklist({xmlel, _, _attrs, _els}) ->
    {block_list}.

encode_block_list_blocklist(undefined, _acc) -> _acc;
encode_block_list_blocklist({block_list}, _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>, <<"urn:xmpp:blocking">>}],
    [{xmlel, <<"blocklist">>, _attrs, _els} | _acc].

decode_disco_info_query({xmlel, _, _attrs, _els}) ->
    Node = decode_disco_info_query_attrs(_attrs, undefined),
    {Xdata, Feature, Identity} =
	decode_disco_info_query_els(_els, [], [], []),
    {disco_info, Node, Identity, Feature, Xdata}.

decode_disco_info_query_els([{xmlel, <<"x">>, _attrs,
			      _} =
				 _el
			     | _els],
			    Xdata, Feature, Identity) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"jabber:x:data">> ->
	  decode_disco_info_query_els(_els,
				      [decode_xdata_x(_el) | Xdata], Feature,
				      Identity);
      _ ->
	  decode_disco_info_query_els(_els, Xdata, Feature,
				      Identity)
    end;
decode_disco_info_query_els([{xmlel, <<"feature">>,
			      _attrs, _} =
				 _el
			     | _els],
			    Xdata, Feature, Identity) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_disco_info_query_els(_els, Xdata,
				      [decode_disco_info_query_feature(_el)
				       | Feature],
				      Identity);
      _ ->
	  decode_disco_info_query_els(_els, Xdata, Feature,
				      Identity)
    end;
decode_disco_info_query_els([{xmlel, <<"identity">>,
			      _attrs, _} =
				 _el
			     | _els],
			    Xdata, Feature, Identity) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_disco_info_query_els(_els, Xdata, Feature,
				      [decode_disco_info_query_identity(_el)
				       | Identity]);
      _ ->
	  decode_disco_info_query_els(_els, Xdata, Feature,
				      Identity)
    end;
decode_disco_info_query_els([_ | _els], Xdata, Feature,
			    Identity) ->
    decode_disco_info_query_els(_els, Xdata, Feature,
				Identity);
decode_disco_info_query_els([], Xdata, Feature,
			    Identity) ->
    {lists:reverse(Xdata), lists:reverse(Feature),
     lists:reverse(Identity)}.

decode_disco_info_query_attrs([{<<"node">>, _val}
			       | _attrs],
			      _Node) ->
    decode_disco_info_query_attrs(_attrs, _val);
decode_disco_info_query_attrs([_ | _attrs], Node) ->
    decode_disco_info_query_attrs(_attrs, Node);
decode_disco_info_query_attrs([], Node) ->
    decode_disco_info_query_node(Node).

encode_disco_info_query(undefined, _acc) -> _acc;
encode_disco_info_query({disco_info, Node, Identity,
			 Feature, Xdata},
			_acc) ->
    _els = encode_disco_info_query_identity(Identity,
					    encode_disco_info_query_feature(Feature,
									    encode_xdata_x(Xdata,
											   []))),
    _attrs = encode_disco_info_query_node(Node,
					  [{<<"xmlns">>,
					    <<"http://jabber.org/protocol/disco#info">>}]),
    [{xmlel, <<"query">>, _attrs, _els} | _acc].

decode_disco_info_query_node(undefined) -> undefined;
decode_disco_info_query_node(_val) -> _val.

encode_disco_info_query_node(undefined, _acc) -> _acc;
encode_disco_info_query_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_disco_info_query_feature({xmlel, _, _attrs,
				 _els}) ->
    Var = decode_disco_info_query_feature_attrs(_attrs,
						undefined),
    Var.

decode_disco_info_query_feature_attrs([{<<"var">>, _val}
				       | _attrs],
				      _Var) ->
    decode_disco_info_query_feature_attrs(_attrs, _val);
decode_disco_info_query_feature_attrs([_ | _attrs],
				      Var) ->
    decode_disco_info_query_feature_attrs(_attrs, Var);
decode_disco_info_query_feature_attrs([], Var) ->
    decode_disco_info_query_feature_var(Var).

encode_disco_info_query_feature([], _acc) -> _acc;
encode_disco_info_query_feature([Var | _tail], _acc) ->
    _els = [],
    _attrs = encode_disco_info_query_feature_var(Var, []),
    encode_disco_info_query_feature(_tail,
				    [{xmlel, <<"feature">>, _attrs, _els}
				     | _acc]).

decode_disco_info_query_feature_var(undefined) ->
    erlang:error({missing_attr, <<"var">>, <<"feature">>,
		  <<>>});
decode_disco_info_query_feature_var(_val) -> _val.

encode_disco_info_query_feature_var(_val, _acc) ->
    [{<<"var">>, _val} | _acc].

decode_disco_info_query_identity({xmlel, _, _attrs,
				  _els}) ->
    {Name, Type, Category} =
	decode_disco_info_query_identity_attrs(_attrs,
					       undefined, undefined, undefined),
    {Category, Type, Name}.

decode_disco_info_query_identity_attrs([{<<"name">>,
					 _val}
					| _attrs],
				       _Name, Type, Category) ->
    decode_disco_info_query_identity_attrs(_attrs, _val,
					   Type, Category);
decode_disco_info_query_identity_attrs([{<<"type">>,
					 _val}
					| _attrs],
				       Name, _Type, Category) ->
    decode_disco_info_query_identity_attrs(_attrs, Name,
					   _val, Category);
decode_disco_info_query_identity_attrs([{<<"category">>,
					 _val}
					| _attrs],
				       Name, Type, _Category) ->
    decode_disco_info_query_identity_attrs(_attrs, Name,
					   Type, _val);
decode_disco_info_query_identity_attrs([_ | _attrs],
				       Name, Type, Category) ->
    decode_disco_info_query_identity_attrs(_attrs, Name,
					   Type, Category);
decode_disco_info_query_identity_attrs([], Name, Type,
				       Category) ->
    {decode_disco_info_query_identity_name(Name),
     decode_disco_info_query_identity_type(Type),
     decode_disco_info_query_identity_category(Category)}.

encode_disco_info_query_identity([], _acc) -> _acc;
encode_disco_info_query_identity([{Category, Type, Name}
				  | _tail],
				 _acc) ->
    _els = [],
    _attrs =
	encode_disco_info_query_identity_category(Category,
						  encode_disco_info_query_identity_type(Type,
											encode_disco_info_query_identity_name(Name,
															      []))),
    encode_disco_info_query_identity(_tail,
				     [{xmlel, <<"identity">>, _attrs, _els}
				      | _acc]).

decode_disco_info_query_identity_category(undefined) ->
    erlang:error({missing_attr, <<"category">>,
		  <<"identity">>, <<>>});
decode_disco_info_query_identity_category(_val) -> _val.

encode_disco_info_query_identity_category(_val, _acc) ->
    [{<<"category">>, _val} | _acc].

decode_disco_info_query_identity_type(undefined) ->
    erlang:error({missing_attr, <<"type">>, <<"identity">>,
		  <<>>});
decode_disco_info_query_identity_type(_val) -> _val.

encode_disco_info_query_identity_type(_val, _acc) ->
    [{<<"type">>, _val} | _acc].

decode_disco_info_query_identity_name(undefined) ->
    undefined;
decode_disco_info_query_identity_name(_val) -> _val.

encode_disco_info_query_identity_name(undefined,
				      _acc) ->
    _acc;
encode_disco_info_query_identity_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_disco_items_query({xmlel, _, _attrs, _els}) ->
    Node = decode_disco_items_query_attrs(_attrs,
					  undefined),
    Items = decode_disco_items_query_els(_els, []),
    {disco_items, Node, Items}.

decode_disco_items_query_els([{xmlel, <<"item">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Items) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_disco_items_query_els(_els,
				       [decode_disco_items_query_item(_el)
					| Items]);
      _ -> decode_disco_items_query_els(_els, Items)
    end;
decode_disco_items_query_els([_ | _els], Items) ->
    decode_disco_items_query_els(_els, Items);
decode_disco_items_query_els([], Items) ->
    lists:reverse(Items).

decode_disco_items_query_attrs([{<<"node">>, _val}
				| _attrs],
			       _Node) ->
    decode_disco_items_query_attrs(_attrs, _val);
decode_disco_items_query_attrs([_ | _attrs], Node) ->
    decode_disco_items_query_attrs(_attrs, Node);
decode_disco_items_query_attrs([], Node) ->
    decode_disco_items_query_node(Node).

encode_disco_items_query(undefined, _acc) -> _acc;
encode_disco_items_query({disco_items, Node, Items},
			 _acc) ->
    _els = encode_disco_items_query_item(Items, []),
    _attrs = encode_disco_items_query_node(Node,
					   [{<<"xmlns">>,
					     <<"http://jabber.org/protocol/disco#items">>}]),
    [{xmlel, <<"query">>, _attrs, _els} | _acc].

decode_disco_items_query_node(undefined) -> undefined;
decode_disco_items_query_node(_val) -> _val.

encode_disco_items_query_node(undefined, _acc) -> _acc;
encode_disco_items_query_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_disco_items_query_item({xmlel, _, _attrs,
			       _els}) ->
    {Node, Name, Jid} =
	decode_disco_items_query_item_attrs(_attrs, undefined,
					    undefined, undefined),
    {disco_item, Jid, Name, Node}.

decode_disco_items_query_item_attrs([{<<"node">>, _val}
				     | _attrs],
				    _Node, Name, Jid) ->
    decode_disco_items_query_item_attrs(_attrs, _val, Name,
					Jid);
decode_disco_items_query_item_attrs([{<<"name">>, _val}
				     | _attrs],
				    Node, _Name, Jid) ->
    decode_disco_items_query_item_attrs(_attrs, Node, _val,
					Jid);
decode_disco_items_query_item_attrs([{<<"jid">>, _val}
				     | _attrs],
				    Node, Name, _Jid) ->
    decode_disco_items_query_item_attrs(_attrs, Node, Name,
					_val);
decode_disco_items_query_item_attrs([_ | _attrs], Node,
				    Name, Jid) ->
    decode_disco_items_query_item_attrs(_attrs, Node, Name,
					Jid);
decode_disco_items_query_item_attrs([], Node, Name,
				    Jid) ->
    {decode_disco_items_query_item_node(Node),
     decode_disco_items_query_item_name(Name),
     decode_disco_items_query_item_jid(Jid)}.

encode_disco_items_query_item([], _acc) -> _acc;
encode_disco_items_query_item([{disco_item, Jid, Name,
				Node}
			       | _tail],
			      _acc) ->
    _els = [],
    _attrs = encode_disco_items_query_item_jid(Jid,
					       encode_disco_items_query_item_name(Name,
										  encode_disco_items_query_item_node(Node,
														     []))),
    encode_disco_items_query_item(_tail,
				  [{xmlel, <<"item">>, _attrs, _els} | _acc]).

decode_disco_items_query_item_jid(undefined) ->
    erlang:error({missing_attr, <<"jid">>, <<"item">>,
		  <<>>});
decode_disco_items_query_item_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"jid">>, <<"item">>,
			<<>>});
      _res -> _res
    end.

encode_disco_items_query_item_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_disco_items_query_item_name(undefined) ->
    undefined;
decode_disco_items_query_item_name(_val) -> _val.

encode_disco_items_query_item_name(undefined, _acc) ->
    _acc;
encode_disco_items_query_item_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_disco_items_query_item_node(undefined) ->
    undefined;
decode_disco_items_query_item_node(_val) -> _val.

encode_disco_items_query_item_node(undefined, _acc) ->
    _acc;
encode_disco_items_query_item_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_private_query({xmlel, _, _attrs, _els}) ->
    __Els = decode_private_query_els(_els, []),
    {private, __Els}.

decode_private_query_els([{xmlel, _, _, _} = _el
			  | _els],
			 __Els) ->
    decode_private_query_els(_els, [decode(_el) | __Els]);
decode_private_query_els([_ | _els], __Els) ->
    decode_private_query_els(_els, __Els);
decode_private_query_els([], __Els) ->
    lists:reverse(__Els).

encode_private_query(undefined, _acc) -> _acc;
encode_private_query({private, __Els}, _acc) ->
    _els = [encode(_subel) || _subel <- __Els] ++ [],
    _attrs = [{<<"xmlns">>, <<"jabber:iq:private">>}],
    [{xmlel, <<"query">>, _attrs, _els} | _acc].

decode_bookmark_conference_conference({xmlel, _, _attrs,
				       _els}) ->
    {Autojoin, Jid, Name} =
	decode_bookmark_conference_conference_attrs(_attrs,
						    undefined, undefined,
						    undefined),
    {Password, Nick} =
	decode_bookmark_conference_conference_els(_els,
						  undefined, undefined),
    {bookmark_conference, Name, Jid, Autojoin, Nick,
     Password}.

decode_bookmark_conference_conference_els([{xmlel,
					    <<"password">>, _attrs, _} =
					       _el
					   | _els],
					  Password, Nick) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_bookmark_conference_conference_els(_els,
						    decode_bookmark_conference_conference_password(_el),
						    Nick);
      _ ->
	  decode_bookmark_conference_conference_els(_els,
						    Password, Nick)
    end;
decode_bookmark_conference_conference_els([{xmlel,
					    <<"nick">>, _attrs, _} =
					       _el
					   | _els],
					  Password, Nick) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_bookmark_conference_conference_els(_els,
						    Password,
						    decode_bookmark_conference_conference_nick(_el));
      _ ->
	  decode_bookmark_conference_conference_els(_els,
						    Password, Nick)
    end;
decode_bookmark_conference_conference_els([_ | _els],
					  Password, Nick) ->
    decode_bookmark_conference_conference_els(_els,
					      Password, Nick);
decode_bookmark_conference_conference_els([], Password,
					  Nick) ->
    {Password, Nick}.

decode_bookmark_conference_conference_attrs([{<<"autojoin">>,
					      _val}
					     | _attrs],
					    _Autojoin, Jid, Name) ->
    decode_bookmark_conference_conference_attrs(_attrs,
						_val, Jid, Name);
decode_bookmark_conference_conference_attrs([{<<"jid">>,
					      _val}
					     | _attrs],
					    Autojoin, _Jid, Name) ->
    decode_bookmark_conference_conference_attrs(_attrs,
						Autojoin, _val, Name);
decode_bookmark_conference_conference_attrs([{<<"name">>,
					      _val}
					     | _attrs],
					    Autojoin, Jid, _Name) ->
    decode_bookmark_conference_conference_attrs(_attrs,
						Autojoin, Jid, _val);
decode_bookmark_conference_conference_attrs([_
					     | _attrs],
					    Autojoin, Jid, Name) ->
    decode_bookmark_conference_conference_attrs(_attrs,
						Autojoin, Jid, Name);
decode_bookmark_conference_conference_attrs([],
					    Autojoin, Jid, Name) ->
    {decode_bookmark_conference_conference_autojoin(Autojoin),
     decode_bookmark_conference_conference_jid(Jid),
     decode_bookmark_conference_conference_name(Name)}.

encode_bookmark_conference_conference([], _acc) -> _acc;
encode_bookmark_conference_conference([{bookmark_conference,
					Name, Jid, Autojoin, Nick, Password}
				       | _tail],
				      _acc) ->
    _els = encode_bookmark_conference_conference_nick(Nick,
						      encode_bookmark_conference_conference_password(Password,
												     [])),
    _attrs =
	encode_bookmark_conference_conference_name(Name,
						   encode_bookmark_conference_conference_jid(Jid,
											     encode_bookmark_conference_conference_autojoin(Autojoin,
																	    []))),
    encode_bookmark_conference_conference(_tail,
					  [{xmlel, <<"conference">>, _attrs,
					    _els}
					   | _acc]).

decode_bookmark_conference_conference_name(undefined) ->
    erlang:error({missing_attr, <<"name">>,
		  <<"conference">>, <<>>});
decode_bookmark_conference_conference_name(_val) ->
    _val.

encode_bookmark_conference_conference_name(_val,
					   _acc) ->
    [{<<"name">>, _val} | _acc].

decode_bookmark_conference_conference_jid(undefined) ->
    erlang:error({missing_attr, <<"jid">>, <<"conference">>,
		  <<>>});
decode_bookmark_conference_conference_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"jid">>,
			<<"conference">>, <<>>});
      _res -> _res
    end.

encode_bookmark_conference_conference_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_bookmark_conference_conference_autojoin(undefined) ->
    false;
decode_bookmark_conference_conference_autojoin(_val) ->
    case catch dec_bool(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"autojoin">>,
			<<"conference">>, <<>>});
      _res -> _res
    end.

encode_bookmark_conference_conference_autojoin(false,
					       _acc) ->
    _acc;
encode_bookmark_conference_conference_autojoin(_val,
					       _acc) ->
    [{<<"autojoin">>, enc_bool(_val)} | _acc].

decode_bookmark_conference_conference_password({xmlel,
						_, _attrs, _els}) ->
    Cdata =
	decode_bookmark_conference_conference_password_els(_els,
							   <<>>),
    Cdata.

decode_bookmark_conference_conference_password_els([{xmlcdata,
						     _data}
						    | _els],
						   Cdata) ->
    decode_bookmark_conference_conference_password_els(_els,
						       <<Cdata/binary,
							 _data/binary>>);
decode_bookmark_conference_conference_password_els([_
						    | _els],
						   Cdata) ->
    decode_bookmark_conference_conference_password_els(_els,
						       Cdata);
decode_bookmark_conference_conference_password_els([],
						   Cdata) ->
    decode_bookmark_conference_conference_password_cdata(Cdata).

encode_bookmark_conference_conference_password(undefined,
					       _acc) ->
    _acc;
encode_bookmark_conference_conference_password(Cdata,
					       _acc) ->
    _els =
	encode_bookmark_conference_conference_password_cdata(Cdata,
							     []),
    _attrs = [],
    [{xmlel, <<"password">>, _attrs, _els} | _acc].

decode_bookmark_conference_conference_password_cdata(<<>>) ->
    undefined;
decode_bookmark_conference_conference_password_cdata(_val) ->
    _val.

encode_bookmark_conference_conference_password_cdata(undefined,
						     _acc) ->
    _acc;
encode_bookmark_conference_conference_password_cdata(_val,
						     _acc) ->
    [{xmlcdata, _val} | _acc].

decode_bookmark_conference_conference_nick({xmlel, _,
					    _attrs, _els}) ->
    Cdata =
	decode_bookmark_conference_conference_nick_els(_els,
						       <<>>),
    Cdata.

decode_bookmark_conference_conference_nick_els([{xmlcdata,
						 _data}
						| _els],
					       Cdata) ->
    decode_bookmark_conference_conference_nick_els(_els,
						   <<Cdata/binary,
						     _data/binary>>);
decode_bookmark_conference_conference_nick_els([_
						| _els],
					       Cdata) ->
    decode_bookmark_conference_conference_nick_els(_els,
						   Cdata);
decode_bookmark_conference_conference_nick_els([],
					       Cdata) ->
    decode_bookmark_conference_conference_nick_cdata(Cdata).

encode_bookmark_conference_conference_nick(undefined,
					   _acc) ->
    _acc;
encode_bookmark_conference_conference_nick(Cdata,
					   _acc) ->
    _els =
	encode_bookmark_conference_conference_nick_cdata(Cdata,
							 []),
    _attrs = [],
    [{xmlel, <<"nick">>, _attrs, _els} | _acc].

decode_bookmark_conference_conference_nick_cdata(<<>>) ->
    undefined;
decode_bookmark_conference_conference_nick_cdata(_val) ->
    _val.

encode_bookmark_conference_conference_nick_cdata(undefined,
						 _acc) ->
    _acc;
encode_bookmark_conference_conference_nick_cdata(_val,
						 _acc) ->
    [{xmlcdata, _val} | _acc].

decode_storage_bookmarks_storage({xmlel, _, _attrs,
				  _els}) ->
    {Url, Conference} =
	decode_storage_bookmarks_storage_els(_els, [], []),
    {bookmark_storage, Conference, Url}.

decode_storage_bookmarks_storage_els([{xmlel, <<"url">>,
				       _attrs, _} =
					  _el
				      | _els],
				     Url, Conference) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_storage_bookmarks_storage_els(_els,
					       [decode_storage_bookmarks_storage_url(_el)
						| Url],
					       Conference);
      _ ->
	  decode_storage_bookmarks_storage_els(_els, Url,
					       Conference)
    end;
decode_storage_bookmarks_storage_els([{xmlel,
				       <<"conference">>, _attrs, _} =
					  _el
				      | _els],
				     Url, Conference) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_storage_bookmarks_storage_els(_els, Url,
					       [decode_bookmark_conference_conference(_el)
						| Conference]);
      _ ->
	  decode_storage_bookmarks_storage_els(_els, Url,
					       Conference)
    end;
decode_storage_bookmarks_storage_els([_ | _els], Url,
				     Conference) ->
    decode_storage_bookmarks_storage_els(_els, Url,
					 Conference);
decode_storage_bookmarks_storage_els([], Url,
				     Conference) ->
    {lists:reverse(Url), lists:reverse(Conference)}.

encode_storage_bookmarks_storage(undefined, _acc) ->
    _acc;
encode_storage_bookmarks_storage({bookmark_storage,
				  Conference, Url},
				 _acc) ->
    _els = encode_bookmark_conference_conference(Conference,
						 encode_storage_bookmarks_storage_url(Url,
										      [])),
    _attrs = [{<<"xmlns">>, <<"storage:bookmarks">>}],
    [{xmlel, <<"storage">>, _attrs, _els} | _acc].

decode_storage_bookmarks_storage_url({xmlel, _, _attrs,
				      _els}) ->
    {Url, Name} =
	decode_storage_bookmarks_storage_url_attrs(_attrs,
						   undefined, undefined),
    {bookmark_url, Name, Url}.

decode_storage_bookmarks_storage_url_attrs([{<<"url">>,
					     _val}
					    | _attrs],
					   _Url, Name) ->
    decode_storage_bookmarks_storage_url_attrs(_attrs, _val,
					       Name);
decode_storage_bookmarks_storage_url_attrs([{<<"name">>,
					     _val}
					    | _attrs],
					   Url, _Name) ->
    decode_storage_bookmarks_storage_url_attrs(_attrs, Url,
					       _val);
decode_storage_bookmarks_storage_url_attrs([_ | _attrs],
					   Url, Name) ->
    decode_storage_bookmarks_storage_url_attrs(_attrs, Url,
					       Name);
decode_storage_bookmarks_storage_url_attrs([], Url,
					   Name) ->
    {decode_storage_bookmarks_storage_url_url(Url),
     decode_storage_bookmarks_storage_url_name(Name)}.

encode_storage_bookmarks_storage_url([], _acc) -> _acc;
encode_storage_bookmarks_storage_url([{bookmark_url,
				       Name, Url}
				      | _tail],
				     _acc) ->
    _els = [],
    _attrs = encode_storage_bookmarks_storage_url_name(Name,
						       encode_storage_bookmarks_storage_url_url(Url,
												[])),
    encode_storage_bookmarks_storage_url(_tail,
					 [{xmlel, <<"url">>, _attrs, _els}
					  | _acc]).

decode_storage_bookmarks_storage_url_name(undefined) ->
    erlang:error({missing_attr, <<"name">>, <<"url">>,
		  <<>>});
decode_storage_bookmarks_storage_url_name(_val) -> _val.

encode_storage_bookmarks_storage_url_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_storage_bookmarks_storage_url_url(undefined) ->
    erlang:error({missing_attr, <<"url">>, <<"url">>,
		  <<>>});
decode_storage_bookmarks_storage_url_url(_val) -> _val.

encode_storage_bookmarks_storage_url_url(_val, _acc) ->
    [{<<"url">>, _val} | _acc].

decode_stats_query({xmlel, _, _attrs, _els}) ->
    Stat = decode_stats_query_els(_els, []), {stats, Stat}.

decode_stats_query_els([{xmlel, <<"stat">>, _attrs, _} =
			    _el
			| _els],
		       Stat) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_stats_query_els(_els,
				 [decode_stats_query_stat(_el) | Stat]);
      _ -> decode_stats_query_els(_els, Stat)
    end;
decode_stats_query_els([_ | _els], Stat) ->
    decode_stats_query_els(_els, Stat);
decode_stats_query_els([], Stat) -> lists:reverse(Stat).

encode_stats_query(undefined, _acc) -> _acc;
encode_stats_query({stats, Stat}, _acc) ->
    _els = encode_stats_query_stat(Stat, []),
    _attrs = [{<<"xmlns">>,
	       <<"http://jabber.org/protocol/stats">>}],
    [{xmlel, <<"query">>, _attrs, _els} | _acc].

decode_stats_query_stat({xmlel, _, _attrs, _els}) ->
    {Value, Units, Name} =
	decode_stats_query_stat_attrs(_attrs, undefined,
				      undefined, undefined),
    Error = decode_stats_query_stat_els(_els, []),
    {stat, Name, Units, Value, Error}.

decode_stats_query_stat_els([{xmlel, <<"error">>,
			      _attrs, _} =
				 _el
			     | _els],
			    Error) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_stats_query_stat_els(_els,
				      [decode_stats_query_stat_error(_el)
				       | Error]);
      _ -> decode_stats_query_stat_els(_els, Error)
    end;
decode_stats_query_stat_els([_ | _els], Error) ->
    decode_stats_query_stat_els(_els, Error);
decode_stats_query_stat_els([], Error) ->
    lists:reverse(Error).

decode_stats_query_stat_attrs([{<<"value">>, _val}
			       | _attrs],
			      _Value, Units, Name) ->
    decode_stats_query_stat_attrs(_attrs, _val, Units,
				  Name);
decode_stats_query_stat_attrs([{<<"units">>, _val}
			       | _attrs],
			      Value, _Units, Name) ->
    decode_stats_query_stat_attrs(_attrs, Value, _val,
				  Name);
decode_stats_query_stat_attrs([{<<"name">>, _val}
			       | _attrs],
			      Value, Units, _Name) ->
    decode_stats_query_stat_attrs(_attrs, Value, Units,
				  _val);
decode_stats_query_stat_attrs([_ | _attrs], Value,
			      Units, Name) ->
    decode_stats_query_stat_attrs(_attrs, Value, Units,
				  Name);
decode_stats_query_stat_attrs([], Value, Units, Name) ->
    {decode_stats_query_stat_value(Value),
     decode_stats_query_stat_units(Units),
     decode_stats_query_stat_name(Name)}.

encode_stats_query_stat([], _acc) -> _acc;
encode_stats_query_stat([{stat, Name, Units, Value,
			  Error}
			 | _tail],
			_acc) ->
    _els = encode_stats_query_stat_error(Error, []),
    _attrs = encode_stats_query_stat_name(Name,
					  encode_stats_query_stat_units(Units,
									encode_stats_query_stat_value(Value,
												      []))),
    encode_stats_query_stat(_tail,
			    [{xmlel, <<"stat">>, _attrs, _els} | _acc]).

decode_stats_query_stat_name(undefined) ->
    erlang:error({missing_attr, <<"name">>, <<"stat">>,
		  <<>>});
decode_stats_query_stat_name(_val) -> _val.

encode_stats_query_stat_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_stats_query_stat_units(undefined) -> undefined;
decode_stats_query_stat_units(_val) -> _val.

encode_stats_query_stat_units(undefined, _acc) -> _acc;
encode_stats_query_stat_units(_val, _acc) ->
    [{<<"units">>, _val} | _acc].

decode_stats_query_stat_value(undefined) -> undefined;
decode_stats_query_stat_value(_val) -> _val.

encode_stats_query_stat_value(undefined, _acc) -> _acc;
encode_stats_query_stat_value(_val, _acc) ->
    [{<<"value">>, _val} | _acc].

decode_stats_query_stat_error({xmlel, _, _attrs,
			       _els}) ->
    Code = decode_stats_query_stat_error_attrs(_attrs,
					       undefined),
    Cdata = decode_stats_query_stat_error_els(_els, <<>>),
    {Code, Cdata}.

decode_stats_query_stat_error_els([{xmlcdata, _data}
				   | _els],
				  Cdata) ->
    decode_stats_query_stat_error_els(_els,
				      <<Cdata/binary, _data/binary>>);
decode_stats_query_stat_error_els([_ | _els], Cdata) ->
    decode_stats_query_stat_error_els(_els, Cdata);
decode_stats_query_stat_error_els([], Cdata) ->
    decode_stats_query_stat_error_cdata(Cdata).

decode_stats_query_stat_error_attrs([{<<"code">>, _val}
				     | _attrs],
				    _Code) ->
    decode_stats_query_stat_error_attrs(_attrs, _val);
decode_stats_query_stat_error_attrs([_ | _attrs],
				    Code) ->
    decode_stats_query_stat_error_attrs(_attrs, Code);
decode_stats_query_stat_error_attrs([], Code) ->
    decode_stats_query_stat_error_code(Code).

encode_stats_query_stat_error([], _acc) -> _acc;
encode_stats_query_stat_error([{Code, Cdata} | _tail],
			      _acc) ->
    _els = encode_stats_query_stat_error_cdata(Cdata, []),
    _attrs = encode_stats_query_stat_error_code(Code, []),
    encode_stats_query_stat_error(_tail,
				  [{xmlel, <<"error">>, _attrs, _els} | _acc]).

decode_stats_query_stat_error_code(undefined) ->
    erlang:error({missing_attr, <<"code">>, <<"error">>,
		  <<>>});
decode_stats_query_stat_error_code(_val) ->
    case catch xml_gen:dec_int(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"code">>, <<"error">>,
			<<>>});
      _res -> _res
    end.

encode_stats_query_stat_error_code(_val, _acc) ->
    [{<<"code">>, xml_gen:enc_int(_val)} | _acc].

decode_stats_query_stat_error_cdata(<<>>) -> undefined;
decode_stats_query_stat_error_cdata(_val) -> _val.

encode_stats_query_stat_error_cdata(undefined, _acc) ->
    _acc;
encode_stats_query_stat_error_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_iq_iq({xmlel, _, _attrs, _els}) ->
    {To, From, Lang, Type, Id} = decode_iq_iq_attrs(_attrs,
						    undefined, undefined,
						    undefined, undefined,
						    undefined),
    {__Els, Error} = decode_iq_iq_els(_els, [], undefined),
    {iq, Id, Type, Lang, From, To, Error, __Els}.

decode_iq_iq_els([{xmlel, <<"error">>, _attrs, _} = _el
		  | _els],
		 __Els, Error) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_iq_iq_els(_els, __Els, decode_error_error(_el));
      _ ->
	  decode_iq_iq_els(_els, [decode(_el) | __Els], Error)
    end;
decode_iq_iq_els([{xmlel, _, _, _} = _el | _els], __Els,
		 Error) ->
    decode_iq_iq_els(_els, [decode(_el) | __Els], Error);
decode_iq_iq_els([_ | _els], __Els, Error) ->
    decode_iq_iq_els(_els, __Els, Error);
decode_iq_iq_els([], __Els, Error) ->
    {lists:reverse(__Els), Error}.

decode_iq_iq_attrs([{<<"to">>, _val} | _attrs], _To,
		   From, Lang, Type, Id) ->
    decode_iq_iq_attrs(_attrs, _val, From, Lang, Type, Id);
decode_iq_iq_attrs([{<<"from">>, _val} | _attrs], To,
		   _From, Lang, Type, Id) ->
    decode_iq_iq_attrs(_attrs, To, _val, Lang, Type, Id);
decode_iq_iq_attrs([{<<"xml:lang">>, _val} | _attrs],
		   To, From, _Lang, Type, Id) ->
    decode_iq_iq_attrs(_attrs, To, From, _val, Type, Id);
decode_iq_iq_attrs([{<<"type">>, _val} | _attrs], To,
		   From, Lang, _Type, Id) ->
    decode_iq_iq_attrs(_attrs, To, From, Lang, _val, Id);
decode_iq_iq_attrs([{<<"id">>, _val} | _attrs], To,
		   From, Lang, Type, _Id) ->
    decode_iq_iq_attrs(_attrs, To, From, Lang, Type, _val);
decode_iq_iq_attrs([_ | _attrs], To, From, Lang, Type,
		   Id) ->
    decode_iq_iq_attrs(_attrs, To, From, Lang, Type, Id);
decode_iq_iq_attrs([], To, From, Lang, Type, Id) ->
    {decode_iq_iq_to(To), decode_iq_iq_from(From),
     'decode_iq_iq_xml:lang'(Lang), decode_iq_iq_type(Type),
     decode_iq_iq_id(Id)}.

encode_iq_iq(undefined, _acc) -> _acc;
encode_iq_iq({iq, Id, Type, Lang, From, To, Error,
	      __Els},
	     _acc) ->
    _els = encode_error_error(Error,
			      [encode(_subel) || _subel <- __Els] ++ []),
    _attrs = encode_iq_iq_id(Id,
			     encode_iq_iq_type(Type,
					       'encode_iq_iq_xml:lang'(Lang,
								       encode_iq_iq_from(From,
											 encode_iq_iq_to(To,
													 []))))),
    [{xmlel, <<"iq">>, _attrs, _els} | _acc].

decode_iq_iq_id(undefined) ->
    erlang:error({missing_attr, <<"id">>, <<"iq">>, <<>>});
decode_iq_iq_id(_val) -> _val.

encode_iq_iq_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_iq_iq_type(undefined) ->
    erlang:error({missing_attr, <<"type">>, <<"iq">>,
		  <<>>});
decode_iq_iq_type(_val) ->
    case catch xml_gen:dec_enum(_val,
				[get, set, result, error])
	of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"type">>, <<"iq">>,
			<<>>});
      _res -> _res
    end.

encode_iq_iq_type(_val, _acc) ->
    [{<<"type">>, xml_gen:enc_enum(_val)} | _acc].

decode_iq_iq_from(undefined) -> undefined;
decode_iq_iq_from(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"from">>, <<"iq">>,
			<<>>});
      _res -> _res
    end.

encode_iq_iq_from(undefined, _acc) -> _acc;
encode_iq_iq_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_iq_iq_to(undefined) -> undefined;
decode_iq_iq_to(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"to">>, <<"iq">>,
			<<>>});
      _res -> _res
    end.

encode_iq_iq_to(undefined, _acc) -> _acc;
encode_iq_iq_to(_val, _acc) ->
    [{<<"to">>, enc_jid(_val)} | _acc].

'decode_iq_iq_xml:lang'(undefined) -> undefined;
'decode_iq_iq_xml:lang'(_val) -> _val.

'encode_iq_iq_xml:lang'(undefined, _acc) -> _acc;
'encode_iq_iq_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_message_message({xmlel, _, _attrs, _els}) ->
    {To, From, Lang, Type, Id} =
	decode_message_message_attrs(_attrs, undefined,
				     undefined, undefined, undefined,
				     undefined),
    {__Els, Error, Thread, Body, Subject} =
	decode_message_message_els(_els, [], undefined,
				   undefined, [], []),
    {message, Id, Type, Lang, From, To, Subject, Body,
     Thread, Error, __Els}.

decode_message_message_els([{xmlel, <<"error">>, _attrs,
			     _} =
				_el
			    | _els],
			   __Els, Error, Thread, Body, Subject) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_message_message_els(_els, __Els,
				     decode_error_error(_el), Thread, Body,
				     Subject);
      _ ->
	  decode_message_message_els(_els, [decode(_el) | __Els],
				     Error, Thread, Body, Subject)
    end;
decode_message_message_els([{xmlel, <<"thread">>,
			     _attrs, _} =
				_el
			    | _els],
			   __Els, Error, Thread, Body, Subject) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_message_message_els(_els, __Els, Error,
				     decode_message_message_thread(_el), Body,
				     Subject);
      _ ->
	  decode_message_message_els(_els, [decode(_el) | __Els],
				     Error, Thread, Body, Subject)
    end;
decode_message_message_els([{xmlel, <<"body">>, _attrs,
			     _} =
				_el
			    | _els],
			   __Els, Error, Thread, Body, Subject) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_message_message_els(_els, __Els, Error, Thread,
				     [decode_message_message_body(_el) | Body],
				     Subject);
      _ ->
	  decode_message_message_els(_els, [decode(_el) | __Els],
				     Error, Thread, Body, Subject)
    end;
decode_message_message_els([{xmlel, <<"subject">>,
			     _attrs, _} =
				_el
			    | _els],
			   __Els, Error, Thread, Body, Subject) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_message_message_els(_els, __Els, Error, Thread,
				     Body,
				     [decode_message_message_subject(_el)
				      | Subject]);
      _ ->
	  decode_message_message_els(_els, [decode(_el) | __Els],
				     Error, Thread, Body, Subject)
    end;
decode_message_message_els([{xmlel, _, _, _} = _el
			    | _els],
			   __Els, Error, Thread, Body, Subject) ->
    decode_message_message_els(_els, [decode(_el) | __Els],
			       Error, Thread, Body, Subject);
decode_message_message_els([_ | _els], __Els, Error,
			   Thread, Body, Subject) ->
    decode_message_message_els(_els, __Els, Error, Thread,
			       Body, Subject);
decode_message_message_els([], __Els, Error, Thread,
			   Body, Subject) ->
    {lists:reverse(__Els), Error, Thread,
     lists:reverse(Body), lists:reverse(Subject)}.

decode_message_message_attrs([{<<"to">>, _val}
			      | _attrs],
			     _To, From, Lang, Type, Id) ->
    decode_message_message_attrs(_attrs, _val, From, Lang,
				 Type, Id);
decode_message_message_attrs([{<<"from">>, _val}
			      | _attrs],
			     To, _From, Lang, Type, Id) ->
    decode_message_message_attrs(_attrs, To, _val, Lang,
				 Type, Id);
decode_message_message_attrs([{<<"xml:lang">>, _val}
			      | _attrs],
			     To, From, _Lang, Type, Id) ->
    decode_message_message_attrs(_attrs, To, From, _val,
				 Type, Id);
decode_message_message_attrs([{<<"type">>, _val}
			      | _attrs],
			     To, From, Lang, _Type, Id) ->
    decode_message_message_attrs(_attrs, To, From, Lang,
				 _val, Id);
decode_message_message_attrs([{<<"id">>, _val}
			      | _attrs],
			     To, From, Lang, Type, _Id) ->
    decode_message_message_attrs(_attrs, To, From, Lang,
				 Type, _val);
decode_message_message_attrs([_ | _attrs], To, From,
			     Lang, Type, Id) ->
    decode_message_message_attrs(_attrs, To, From, Lang,
				 Type, Id);
decode_message_message_attrs([], To, From, Lang, Type,
			     Id) ->
    {decode_message_message_to(To),
     decode_message_message_from(From),
     'decode_message_message_xml:lang'(Lang),
     decode_message_message_type(Type),
     decode_message_message_id(Id)}.

encode_message_message(undefined, _acc) -> _acc;
encode_message_message({message, Id, Type, Lang, From,
			To, Subject, Body, Thread, Error, __Els},
		       _acc) ->
    _els = encode_message_message_subject(Subject,
					  encode_message_message_body(Body,
								      encode_message_message_thread(Thread,
												    encode_error_error(Error,
														       [encode(_subel)
															|| _subel
															       <- __Els]
															 ++
															 [])))),
    _attrs = encode_message_message_id(Id,
				       encode_message_message_type(Type,
								   'encode_message_message_xml:lang'(Lang,
												     encode_message_message_from(From,
																 encode_message_message_to(To,
																			   []))))),
    [{xmlel, <<"message">>, _attrs, _els} | _acc].

decode_message_message_id(undefined) -> undefined;
decode_message_message_id(_val) -> _val.

encode_message_message_id(undefined, _acc) -> _acc;
encode_message_message_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_message_message_type(undefined) -> normal;
decode_message_message_type(_val) ->
    case catch xml_gen:dec_enum(_val,
				[chat, normal, groupchat, headline, error])
	of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"type">>, <<"message">>,
			<<>>});
      _res -> _res
    end.

encode_message_message_type(normal, _acc) -> _acc;
encode_message_message_type(_val, _acc) ->
    [{<<"type">>, xml_gen:enc_enum(_val)} | _acc].

decode_message_message_from(undefined) -> undefined;
decode_message_message_from(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"from">>, <<"message">>,
			<<>>});
      _res -> _res
    end.

encode_message_message_from(undefined, _acc) -> _acc;
encode_message_message_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_message_message_to(undefined) -> undefined;
decode_message_message_to(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"to">>, <<"message">>,
			<<>>});
      _res -> _res
    end.

encode_message_message_to(undefined, _acc) -> _acc;
encode_message_message_to(_val, _acc) ->
    [{<<"to">>, enc_jid(_val)} | _acc].

'decode_message_message_xml:lang'(undefined) ->
    undefined;
'decode_message_message_xml:lang'(_val) -> _val.

'encode_message_message_xml:lang'(undefined, _acc) ->
    _acc;
'encode_message_message_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_message_message_thread({xmlel, _, _attrs,
			       _els}) ->
    Cdata = decode_message_message_thread_els(_els, <<>>),
    Cdata.

decode_message_message_thread_els([{xmlcdata, _data}
				   | _els],
				  Cdata) ->
    decode_message_message_thread_els(_els,
				      <<Cdata/binary, _data/binary>>);
decode_message_message_thread_els([_ | _els], Cdata) ->
    decode_message_message_thread_els(_els, Cdata);
decode_message_message_thread_els([], Cdata) ->
    decode_message_message_thread_cdata(Cdata).

encode_message_message_thread(undefined, _acc) -> _acc;
encode_message_message_thread(Cdata, _acc) ->
    _els = encode_message_message_thread_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"thread">>, _attrs, _els} | _acc].

decode_message_message_thread_cdata(<<>>) -> undefined;
decode_message_message_thread_cdata(_val) -> _val.

encode_message_message_thread_cdata(undefined, _acc) ->
    _acc;
encode_message_message_thread_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_message_message_body({xmlel, _, _attrs, _els}) ->
    Body_lang = decode_message_message_body_attrs(_attrs,
						  undefined),
    Cdata = decode_message_message_body_els(_els, <<>>),
    {Body_lang, Cdata}.

decode_message_message_body_els([{xmlcdata, _data}
				 | _els],
				Cdata) ->
    decode_message_message_body_els(_els,
				    <<Cdata/binary, _data/binary>>);
decode_message_message_body_els([_ | _els], Cdata) ->
    decode_message_message_body_els(_els, Cdata);
decode_message_message_body_els([], Cdata) ->
    decode_message_message_body_cdata(Cdata).

decode_message_message_body_attrs([{<<"xml:lang">>,
				    _val}
				   | _attrs],
				  _Body_lang) ->
    decode_message_message_body_attrs(_attrs, _val);
decode_message_message_body_attrs([_ | _attrs],
				  Body_lang) ->
    decode_message_message_body_attrs(_attrs, Body_lang);
decode_message_message_body_attrs([], Body_lang) ->
    'decode_message_message_body_xml:lang'(Body_lang).

encode_message_message_body([], _acc) -> _acc;
encode_message_message_body([{Body_lang, Cdata}
			     | _tail],
			    _acc) ->
    _els = encode_message_message_body_cdata(Cdata, []),
    _attrs =
	'encode_message_message_body_xml:lang'(Body_lang, []),
    encode_message_message_body(_tail,
				[{xmlel, <<"body">>, _attrs, _els} | _acc]).

'decode_message_message_body_xml:lang'(undefined) ->
    undefined;
'decode_message_message_body_xml:lang'(_val) -> _val.

'encode_message_message_body_xml:lang'(undefined,
				       _acc) ->
    _acc;
'encode_message_message_body_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_message_message_body_cdata(<<>>) -> undefined;
decode_message_message_body_cdata(_val) -> _val.

encode_message_message_body_cdata(undefined, _acc) ->
    _acc;
encode_message_message_body_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_message_message_subject({xmlel, _, _attrs,
				_els}) ->
    Subject_lang =
	decode_message_message_subject_attrs(_attrs, undefined),
    Cdata = decode_message_message_subject_els(_els, <<>>),
    {Subject_lang, Cdata}.

decode_message_message_subject_els([{xmlcdata, _data}
				    | _els],
				   Cdata) ->
    decode_message_message_subject_els(_els,
				       <<Cdata/binary, _data/binary>>);
decode_message_message_subject_els([_ | _els], Cdata) ->
    decode_message_message_subject_els(_els, Cdata);
decode_message_message_subject_els([], Cdata) ->
    decode_message_message_subject_cdata(Cdata).

decode_message_message_subject_attrs([{<<"xml:lang">>,
				       _val}
				      | _attrs],
				     _Subject_lang) ->
    decode_message_message_subject_attrs(_attrs, _val);
decode_message_message_subject_attrs([_ | _attrs],
				     Subject_lang) ->
    decode_message_message_subject_attrs(_attrs,
					 Subject_lang);
decode_message_message_subject_attrs([],
				     Subject_lang) ->
    'decode_message_message_subject_xml:lang'(Subject_lang).

encode_message_message_subject([], _acc) -> _acc;
encode_message_message_subject([{Subject_lang, Cdata}
				| _tail],
			       _acc) ->
    _els = encode_message_message_subject_cdata(Cdata, []),
    _attrs =
	'encode_message_message_subject_xml:lang'(Subject_lang,
						  []),
    encode_message_message_subject(_tail,
				   [{xmlel, <<"subject">>, _attrs, _els}
				    | _acc]).

'decode_message_message_subject_xml:lang'(undefined) ->
    undefined;
'decode_message_message_subject_xml:lang'(_val) -> _val.

'encode_message_message_subject_xml:lang'(undefined,
					  _acc) ->
    _acc;
'encode_message_message_subject_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_message_message_subject_cdata(<<>>) -> undefined;
decode_message_message_subject_cdata(_val) -> _val.

encode_message_message_subject_cdata(undefined, _acc) ->
    _acc;
encode_message_message_subject_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_presence_presence({xmlel, _, _attrs, _els}) ->
    {To, From, Lang, Type, Id} =
	decode_presence_presence_attrs(_attrs, undefined,
				       undefined, undefined, undefined,
				       undefined),
    {__Els, Error, Priority, Status, Show} =
	decode_presence_presence_els(_els, [], undefined,
				     undefined, [], undefined),
    {presence, Id, Type, Lang, From, To, Show, Status,
     Priority, Error, __Els}.

decode_presence_presence_els([{xmlel, <<"error">>,
			       _attrs, _} =
				  _el
			      | _els],
			     __Els, Error, Priority, Status, Show) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_presence_presence_els(_els, __Els,
				       decode_error_error(_el), Priority,
				       Status, Show);
      _ ->
	  decode_presence_presence_els(_els,
				       [decode(_el) | __Els], Error, Priority,
				       Status, Show)
    end;
decode_presence_presence_els([{xmlel, <<"priority">>,
			       _attrs, _} =
				  _el
			      | _els],
			     __Els, Error, Priority, Status, Show) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_presence_presence_els(_els, __Els, Error,
				       decode_presence_presence_priority(_el),
				       Status, Show);
      _ ->
	  decode_presence_presence_els(_els,
				       [decode(_el) | __Els], Error, Priority,
				       Status, Show)
    end;
decode_presence_presence_els([{xmlel, <<"status">>,
			       _attrs, _} =
				  _el
			      | _els],
			     __Els, Error, Priority, Status, Show) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_presence_presence_els(_els, __Els, Error,
				       Priority,
				       [decode_presence_presence_status(_el)
					| Status],
				       Show);
      _ ->
	  decode_presence_presence_els(_els,
				       [decode(_el) | __Els], Error, Priority,
				       Status, Show)
    end;
decode_presence_presence_els([{xmlel, <<"show">>,
			       _attrs, _} =
				  _el
			      | _els],
			     __Els, Error, Priority, Status, Show) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_presence_presence_els(_els, __Els, Error,
				       Priority, Status,
				       decode_presence_presence_show(_el));
      _ ->
	  decode_presence_presence_els(_els,
				       [decode(_el) | __Els], Error, Priority,
				       Status, Show)
    end;
decode_presence_presence_els([{xmlel, _, _, _} = _el
			      | _els],
			     __Els, Error, Priority, Status, Show) ->
    decode_presence_presence_els(_els,
				 [decode(_el) | __Els], Error, Priority, Status,
				 Show);
decode_presence_presence_els([_ | _els], __Els, Error,
			     Priority, Status, Show) ->
    decode_presence_presence_els(_els, __Els, Error,
				 Priority, Status, Show);
decode_presence_presence_els([], __Els, Error, Priority,
			     Status, Show) ->
    {lists:reverse(__Els), Error, Priority,
     lists:reverse(Status), Show}.

decode_presence_presence_attrs([{<<"to">>, _val}
				| _attrs],
			       _To, From, Lang, Type, Id) ->
    decode_presence_presence_attrs(_attrs, _val, From, Lang,
				   Type, Id);
decode_presence_presence_attrs([{<<"from">>, _val}
				| _attrs],
			       To, _From, Lang, Type, Id) ->
    decode_presence_presence_attrs(_attrs, To, _val, Lang,
				   Type, Id);
decode_presence_presence_attrs([{<<"xml:lang">>, _val}
				| _attrs],
			       To, From, _Lang, Type, Id) ->
    decode_presence_presence_attrs(_attrs, To, From, _val,
				   Type, Id);
decode_presence_presence_attrs([{<<"type">>, _val}
				| _attrs],
			       To, From, Lang, _Type, Id) ->
    decode_presence_presence_attrs(_attrs, To, From, Lang,
				   _val, Id);
decode_presence_presence_attrs([{<<"id">>, _val}
				| _attrs],
			       To, From, Lang, Type, _Id) ->
    decode_presence_presence_attrs(_attrs, To, From, Lang,
				   Type, _val);
decode_presence_presence_attrs([_ | _attrs], To, From,
			       Lang, Type, Id) ->
    decode_presence_presence_attrs(_attrs, To, From, Lang,
				   Type, Id);
decode_presence_presence_attrs([], To, From, Lang, Type,
			       Id) ->
    {decode_presence_presence_to(To),
     decode_presence_presence_from(From),
     'decode_presence_presence_xml:lang'(Lang),
     decode_presence_presence_type(Type),
     decode_presence_presence_id(Id)}.

encode_presence_presence(undefined, _acc) -> _acc;
encode_presence_presence({presence, Id, Type, Lang,
			  From, To, Show, Status, Priority, Error, __Els},
			 _acc) ->
    _els = encode_presence_presence_show(Show,
					 encode_presence_presence_status(Status,
									 encode_presence_presence_priority(Priority,
													   encode_error_error(Error,
															      [encode(_subel)
															       || _subel
																      <- __Els]
																++
																[])))),
    _attrs = encode_presence_presence_id(Id,
					 encode_presence_presence_type(Type,
								       'encode_presence_presence_xml:lang'(Lang,
													   encode_presence_presence_from(From,
																	 encode_presence_presence_to(To,
																				     []))))),
    [{xmlel, <<"presence">>, _attrs, _els} | _acc].

decode_presence_presence_id(undefined) -> undefined;
decode_presence_presence_id(_val) -> _val.

encode_presence_presence_id(undefined, _acc) -> _acc;
encode_presence_presence_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_presence_presence_type(undefined) -> undefined;
decode_presence_presence_type(_val) ->
    case catch xml_gen:dec_enum(_val,
				[unavailable, subscribe, subscribed,
				 unsubscribe, unsubscribed, probe, error])
	of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"type">>,
			<<"presence">>, <<>>});
      _res -> _res
    end.

encode_presence_presence_type(undefined, _acc) -> _acc;
encode_presence_presence_type(_val, _acc) ->
    [{<<"type">>, xml_gen:enc_enum(_val)} | _acc].

decode_presence_presence_from(undefined) -> undefined;
decode_presence_presence_from(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"from">>,
			<<"presence">>, <<>>});
      _res -> _res
    end.

encode_presence_presence_from(undefined, _acc) -> _acc;
encode_presence_presence_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_presence_presence_to(undefined) -> undefined;
decode_presence_presence_to(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"to">>, <<"presence">>,
			<<>>});
      _res -> _res
    end.

encode_presence_presence_to(undefined, _acc) -> _acc;
encode_presence_presence_to(_val, _acc) ->
    [{<<"to">>, enc_jid(_val)} | _acc].

'decode_presence_presence_xml:lang'(undefined) ->
    undefined;
'decode_presence_presence_xml:lang'(_val) -> _val.

'encode_presence_presence_xml:lang'(undefined, _acc) ->
    _acc;
'encode_presence_presence_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_presence_presence_priority({xmlel, _, _attrs,
				   _els}) ->
    Cdata = decode_presence_presence_priority_els(_els,
						  <<>>),
    Cdata.

decode_presence_presence_priority_els([{xmlcdata, _data}
				       | _els],
				      Cdata) ->
    decode_presence_presence_priority_els(_els,
					  <<Cdata/binary, _data/binary>>);
decode_presence_presence_priority_els([_ | _els],
				      Cdata) ->
    decode_presence_presence_priority_els(_els, Cdata);
decode_presence_presence_priority_els([], Cdata) ->
    decode_presence_presence_priority_cdata(Cdata).

encode_presence_presence_priority(undefined, _acc) ->
    _acc;
encode_presence_presence_priority(Cdata, _acc) ->
    _els = encode_presence_presence_priority_cdata(Cdata,
						   []),
    _attrs = [],
    [{xmlel, <<"priority">>, _attrs, _els} | _acc].

decode_presence_presence_priority_cdata(<<>>) ->
    undefined;
decode_presence_presence_priority_cdata(_val) ->
    case catch xml_gen:dec_int(_val, -128, 127) of
      {'EXIT', _} ->
	  erlang:error({bad_cdata_value, <<>>, <<"priority">>,
			<<>>});
      _res -> _res
    end.

encode_presence_presence_priority_cdata(undefined,
					_acc) ->
    _acc;
encode_presence_presence_priority_cdata(_val, _acc) ->
    [{xmlcdata, xml_gen:enc_int(_val)} | _acc].

decode_presence_presence_status({xmlel, _, _attrs,
				 _els}) ->
    Status_lang =
	decode_presence_presence_status_attrs(_attrs,
					      undefined),
    Cdata = decode_presence_presence_status_els(_els, <<>>),
    {Status_lang, Cdata}.

decode_presence_presence_status_els([{xmlcdata, _data}
				     | _els],
				    Cdata) ->
    decode_presence_presence_status_els(_els,
					<<Cdata/binary, _data/binary>>);
decode_presence_presence_status_els([_ | _els],
				    Cdata) ->
    decode_presence_presence_status_els(_els, Cdata);
decode_presence_presence_status_els([], Cdata) ->
    decode_presence_presence_status_cdata(Cdata).

decode_presence_presence_status_attrs([{<<"xml:lang">>,
					_val}
				       | _attrs],
				      _Status_lang) ->
    decode_presence_presence_status_attrs(_attrs, _val);
decode_presence_presence_status_attrs([_ | _attrs],
				      Status_lang) ->
    decode_presence_presence_status_attrs(_attrs,
					  Status_lang);
decode_presence_presence_status_attrs([],
				      Status_lang) ->
    'decode_presence_presence_status_xml:lang'(Status_lang).

encode_presence_presence_status([], _acc) -> _acc;
encode_presence_presence_status([{Status_lang, Cdata}
				 | _tail],
				_acc) ->
    _els = encode_presence_presence_status_cdata(Cdata, []),
    _attrs =
	'encode_presence_presence_status_xml:lang'(Status_lang,
						   []),
    encode_presence_presence_status(_tail,
				    [{xmlel, <<"status">>, _attrs, _els}
				     | _acc]).

'decode_presence_presence_status_xml:lang'(undefined) ->
    undefined;
'decode_presence_presence_status_xml:lang'(_val) ->
    _val.

'encode_presence_presence_status_xml:lang'(undefined,
					   _acc) ->
    _acc;
'encode_presence_presence_status_xml:lang'(_val,
					   _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_presence_presence_status_cdata(<<>>) ->
    undefined;
decode_presence_presence_status_cdata(_val) -> _val.

encode_presence_presence_status_cdata(undefined,
				      _acc) ->
    _acc;
encode_presence_presence_status_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_presence_presence_show({xmlel, _, _attrs,
			       _els}) ->
    Cdata = decode_presence_presence_show_els(_els, <<>>),
    Cdata.

decode_presence_presence_show_els([{xmlcdata, _data}
				   | _els],
				  Cdata) ->
    decode_presence_presence_show_els(_els,
				      <<Cdata/binary, _data/binary>>);
decode_presence_presence_show_els([_ | _els], Cdata) ->
    decode_presence_presence_show_els(_els, Cdata);
decode_presence_presence_show_els([], Cdata) ->
    decode_presence_presence_show_cdata(Cdata).

encode_presence_presence_show(undefined, _acc) -> _acc;
encode_presence_presence_show(Cdata, _acc) ->
    _els = encode_presence_presence_show_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"show">>, _attrs, _els} | _acc].

decode_presence_presence_show_cdata(<<>>) -> undefined;
decode_presence_presence_show_cdata(_val) ->
    case catch xml_gen:dec_enum(_val, [away, chat, dnd, xa])
	of
      {'EXIT', _} ->
	  erlang:error({bad_cdata_value, <<>>, <<"show">>, <<>>});
      _res -> _res
    end.

encode_presence_presence_show_cdata(undefined, _acc) ->
    _acc;
encode_presence_presence_show_cdata(_val, _acc) ->
    [{xmlcdata, xml_gen:enc_enum(_val)} | _acc].

decode_error_error({xmlel, _, _attrs, _els}) ->
    {By, Error_type} = decode_error_error_attrs(_attrs,
						undefined, undefined),
    {Text, Reason} = decode_error_error_els(_els, undefined,
					    undefined),
    {error, Error_type, By, Reason, Text}.

decode_error_error_els([{xmlel, <<"text">>, _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els,
				 decode_error_error_text(_el), Reason);
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel,
			 <<"unexpected-request">>, _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_unexpected-request'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel,
			 <<"undefined-condition">>, _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_undefined-condition'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel,
			 <<"subscription-required">>, _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_subscription-required'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel,
			 <<"service-unavailable">>, _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_service-unavailable'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel,
			 <<"resource-constraint">>, _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_resource-constraint'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel,
			 <<"remote-server-timeout">>, _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_remote-server-timeout'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel,
			 <<"remote-server-not-found">>, _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_remote-server-not-found'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel,
			 <<"registration-required">>, _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_registration-required'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel, <<"redirect">>, _attrs,
			 _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 decode_error_error_redirect(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel,
			 <<"recipient-unavailable">>, _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_recipient-unavailable'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel, <<"policy-violation">>,
			 _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_policy-violation'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel, <<"not-authorized">>,
			 _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_not-authorized'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel, <<"not-allowed">>,
			 _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_not-allowed'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel, <<"not-acceptable">>,
			 _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_not-acceptable'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel, <<"jid-malformed">>,
			 _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_jid-malformed'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel, <<"item-not-found">>,
			 _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_item-not-found'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel,
			 <<"internal-server-error">>, _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_internal-server-error'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel, <<"gone">>, _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 decode_error_error_gone(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel, <<"forbidden">>, _attrs,
			 _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 decode_error_error_forbidden(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel,
			 <<"feature-not-implemented">>, _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_feature-not-implemented'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel, <<"conflict">>, _attrs,
			 _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 decode_error_error_conflict(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([{xmlel, <<"bad-request">>,
			 _attrs, _} =
			    _el
			| _els],
		       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	  decode_error_error_els(_els, Text,
				 'decode_error_error_bad-request'(_el));
      _ -> decode_error_error_els(_els, Text, Reason)
    end;
decode_error_error_els([_ | _els], Text, Reason) ->
    decode_error_error_els(_els, Text, Reason);
decode_error_error_els([], Text, Reason) ->
    {Text, Reason}.

decode_error_error_attrs([{<<"by">>, _val} | _attrs],
			 _By, Error_type) ->
    decode_error_error_attrs(_attrs, _val, Error_type);
decode_error_error_attrs([{<<"type">>, _val} | _attrs],
			 By, _Error_type) ->
    decode_error_error_attrs(_attrs, By, _val);
decode_error_error_attrs([_ | _attrs], By,
			 Error_type) ->
    decode_error_error_attrs(_attrs, By, Error_type);
decode_error_error_attrs([], By, Error_type) ->
    {decode_error_error_by(By),
     decode_error_error_type(Error_type)}.

'encode_error_error_$reason'(undefined, _acc) -> _acc;
'encode_error_error_$reason'('unexpected-request' = _r,
			     _acc) ->
    'encode_error_error_unexpected-request'(_r, _acc);
'encode_error_error_$reason'('undefined-condition' = _r,
			     _acc) ->
    'encode_error_error_undefined-condition'(_r, _acc);
'encode_error_error_$reason'('subscription-required' =
				 _r,
			     _acc) ->
    'encode_error_error_subscription-required'(_r, _acc);
'encode_error_error_$reason'('service-unavailable' = _r,
			     _acc) ->
    'encode_error_error_service-unavailable'(_r, _acc);
'encode_error_error_$reason'('resource-constraint' = _r,
			     _acc) ->
    'encode_error_error_resource-constraint'(_r, _acc);
'encode_error_error_$reason'('remote-server-timeout' =
				 _r,
			     _acc) ->
    'encode_error_error_remote-server-timeout'(_r, _acc);
'encode_error_error_$reason'('remote-server-not-found' =
				 _r,
			     _acc) ->
    'encode_error_error_remote-server-not-found'(_r, _acc);
'encode_error_error_$reason'('registration-required' =
				 _r,
			     _acc) ->
    'encode_error_error_registration-required'(_r, _acc);
'encode_error_error_$reason'({redirect, _} = _r,
			     _acc) ->
    encode_error_error_redirect(_r, _acc);
'encode_error_error_$reason'('recipient-unavailable' =
				 _r,
			     _acc) ->
    'encode_error_error_recipient-unavailable'(_r, _acc);
'encode_error_error_$reason'('policy-violation' = _r,
			     _acc) ->
    'encode_error_error_policy-violation'(_r, _acc);
'encode_error_error_$reason'('not-authorized' = _r,
			     _acc) ->
    'encode_error_error_not-authorized'(_r, _acc);
'encode_error_error_$reason'('not-allowed' = _r,
			     _acc) ->
    'encode_error_error_not-allowed'(_r, _acc);
'encode_error_error_$reason'('not-acceptable' = _r,
			     _acc) ->
    'encode_error_error_not-acceptable'(_r, _acc);
'encode_error_error_$reason'('jid-malformed' = _r,
			     _acc) ->
    'encode_error_error_jid-malformed'(_r, _acc);
'encode_error_error_$reason'('item-not-found' = _r,
			     _acc) ->
    'encode_error_error_item-not-found'(_r, _acc);
'encode_error_error_$reason'('internal-server-error' =
				 _r,
			     _acc) ->
    'encode_error_error_internal-server-error'(_r, _acc);
'encode_error_error_$reason'({gone, _} = _r, _acc) ->
    encode_error_error_gone(_r, _acc);
'encode_error_error_$reason'(forbidden = _r, _acc) ->
    encode_error_error_forbidden(_r, _acc);
'encode_error_error_$reason'('feature-not-implemented' =
				 _r,
			     _acc) ->
    'encode_error_error_feature-not-implemented'(_r, _acc);
'encode_error_error_$reason'(conflict = _r, _acc) ->
    encode_error_error_conflict(_r, _acc);
'encode_error_error_$reason'('bad-request' = _r,
			     _acc) ->
    'encode_error_error_bad-request'(_r, _acc).

encode_error_error(undefined, _acc) -> _acc;
encode_error_error({error, Error_type, By, Reason,
		    Text},
		   _acc) ->
    _els = 'encode_error_error_$reason'(Reason,
					encode_error_error_text(Text, [])),
    _attrs = encode_error_error_type(Error_type,
				     encode_error_error_by(By, [])),
    [{xmlel, <<"error">>, _attrs, _els} | _acc].

decode_error_error_type(undefined) ->
    erlang:error({missing_attr, <<"type">>, <<"error">>,
		  <<>>});
decode_error_error_type(_val) ->
    case catch xml_gen:dec_enum(_val,
				[auth, cancel, continue, modify, wait])
	of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"type">>, <<"error">>,
			<<>>});
      _res -> _res
    end.

encode_error_error_type(_val, _acc) ->
    [{<<"type">>, xml_gen:enc_enum(_val)} | _acc].

decode_error_error_by(undefined) -> undefined;
decode_error_error_by(_val) -> _val.

encode_error_error_by(undefined, _acc) -> _acc;
encode_error_error_by(_val, _acc) ->
    [{<<"by">>, _val} | _acc].

'decode_error_error_unexpected-request'({xmlel, _,
					 _attrs, _els}) ->
    'unexpected-request'.

'encode_error_error_unexpected-request'(undefined,
					_acc) ->
    _acc;
'encode_error_error_unexpected-request'('unexpected-request',
					_acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"unexpected-request">>, _attrs, _els}
     | _acc].

'decode_error_error_undefined-condition'({xmlel, _,
					  _attrs, _els}) ->
    'undefined-condition'.

'encode_error_error_undefined-condition'(undefined,
					 _acc) ->
    _acc;
'encode_error_error_undefined-condition'('undefined-condition',
					 _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"undefined-condition">>, _attrs, _els}
     | _acc].

'decode_error_error_subscription-required'({xmlel, _,
					    _attrs, _els}) ->
    'subscription-required'.

'encode_error_error_subscription-required'(undefined,
					   _acc) ->
    _acc;
'encode_error_error_subscription-required'('subscription-required',
					   _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"subscription-required">>, _attrs, _els}
     | _acc].

'decode_error_error_service-unavailable'({xmlel, _,
					  _attrs, _els}) ->
    'service-unavailable'.

'encode_error_error_service-unavailable'(undefined,
					 _acc) ->
    _acc;
'encode_error_error_service-unavailable'('service-unavailable',
					 _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"service-unavailable">>, _attrs, _els}
     | _acc].

'decode_error_error_resource-constraint'({xmlel, _,
					  _attrs, _els}) ->
    'resource-constraint'.

'encode_error_error_resource-constraint'(undefined,
					 _acc) ->
    _acc;
'encode_error_error_resource-constraint'('resource-constraint',
					 _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"resource-constraint">>, _attrs, _els}
     | _acc].

'decode_error_error_remote-server-timeout'({xmlel, _,
					    _attrs, _els}) ->
    'remote-server-timeout'.

'encode_error_error_remote-server-timeout'(undefined,
					   _acc) ->
    _acc;
'encode_error_error_remote-server-timeout'('remote-server-timeout',
					   _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"remote-server-timeout">>, _attrs, _els}
     | _acc].

'decode_error_error_remote-server-not-found'({xmlel, _,
					      _attrs, _els}) ->
    'remote-server-not-found'.

'encode_error_error_remote-server-not-found'(undefined,
					     _acc) ->
    _acc;
'encode_error_error_remote-server-not-found'('remote-server-not-found',
					     _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"remote-server-not-found">>, _attrs, _els}
     | _acc].

'decode_error_error_registration-required'({xmlel, _,
					    _attrs, _els}) ->
    'registration-required'.

'encode_error_error_registration-required'(undefined,
					   _acc) ->
    _acc;
'encode_error_error_registration-required'('registration-required',
					   _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"registration-required">>, _attrs, _els}
     | _acc].

decode_error_error_redirect({xmlel, _, _attrs, _els}) ->
    Cdata = decode_error_error_redirect_els(_els, <<>>),
    {redirect, Cdata}.

decode_error_error_redirect_els([{xmlcdata, _data}
				 | _els],
				Cdata) ->
    decode_error_error_redirect_els(_els,
				    <<Cdata/binary, _data/binary>>);
decode_error_error_redirect_els([_ | _els], Cdata) ->
    decode_error_error_redirect_els(_els, Cdata);
decode_error_error_redirect_els([], Cdata) ->
    decode_error_error_redirect_cdata(Cdata).

encode_error_error_redirect(undefined, _acc) -> _acc;
encode_error_error_redirect({redirect, Cdata}, _acc) ->
    _els = encode_error_error_redirect_cdata(Cdata, []),
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"redirect">>, _attrs, _els} | _acc].

decode_error_error_redirect_cdata(<<>>) -> undefined;
decode_error_error_redirect_cdata(_val) -> _val.

encode_error_error_redirect_cdata(undefined, _acc) ->
    _acc;
encode_error_error_redirect_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

'decode_error_error_recipient-unavailable'({xmlel, _,
					    _attrs, _els}) ->
    'recipient-unavailable'.

'encode_error_error_recipient-unavailable'(undefined,
					   _acc) ->
    _acc;
'encode_error_error_recipient-unavailable'('recipient-unavailable',
					   _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"recipient-unavailable">>, _attrs, _els}
     | _acc].

'decode_error_error_policy-violation'({xmlel, _, _attrs,
				       _els}) ->
    'policy-violation'.

'encode_error_error_policy-violation'(undefined,
				      _acc) ->
    _acc;
'encode_error_error_policy-violation'('policy-violation',
				      _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"policy-violation">>, _attrs, _els} | _acc].

'decode_error_error_not-authorized'({xmlel, _, _attrs,
				     _els}) ->
    'not-authorized'.

'encode_error_error_not-authorized'(undefined, _acc) ->
    _acc;
'encode_error_error_not-authorized'('not-authorized',
				    _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"not-authorized">>, _attrs, _els} | _acc].

'decode_error_error_not-allowed'({xmlel, _, _attrs,
				  _els}) ->
    'not-allowed'.

'encode_error_error_not-allowed'(undefined, _acc) ->
    _acc;
'encode_error_error_not-allowed'('not-allowed', _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"not-allowed">>, _attrs, _els} | _acc].

'decode_error_error_not-acceptable'({xmlel, _, _attrs,
				     _els}) ->
    'not-acceptable'.

'encode_error_error_not-acceptable'(undefined, _acc) ->
    _acc;
'encode_error_error_not-acceptable'('not-acceptable',
				    _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"not-acceptable">>, _attrs, _els} | _acc].

'decode_error_error_jid-malformed'({xmlel, _, _attrs,
				    _els}) ->
    'jid-malformed'.

'encode_error_error_jid-malformed'(undefined, _acc) ->
    _acc;
'encode_error_error_jid-malformed'('jid-malformed',
				   _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"jid-malformed">>, _attrs, _els} | _acc].

'decode_error_error_item-not-found'({xmlel, _, _attrs,
				     _els}) ->
    'item-not-found'.

'encode_error_error_item-not-found'(undefined, _acc) ->
    _acc;
'encode_error_error_item-not-found'('item-not-found',
				    _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"item-not-found">>, _attrs, _els} | _acc].

'decode_error_error_internal-server-error'({xmlel, _,
					    _attrs, _els}) ->
    'internal-server-error'.

'encode_error_error_internal-server-error'(undefined,
					   _acc) ->
    _acc;
'encode_error_error_internal-server-error'('internal-server-error',
					   _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"internal-server-error">>, _attrs, _els}
     | _acc].

decode_error_error_gone({xmlel, _, _attrs, _els}) ->
    Cdata = decode_error_error_gone_els(_els, <<>>),
    {gone, Cdata}.

decode_error_error_gone_els([{xmlcdata, _data} | _els],
			    Cdata) ->
    decode_error_error_gone_els(_els,
				<<Cdata/binary, _data/binary>>);
decode_error_error_gone_els([_ | _els], Cdata) ->
    decode_error_error_gone_els(_els, Cdata);
decode_error_error_gone_els([], Cdata) ->
    decode_error_error_gone_cdata(Cdata).

encode_error_error_gone(undefined, _acc) -> _acc;
encode_error_error_gone({gone, Cdata}, _acc) ->
    _els = encode_error_error_gone_cdata(Cdata, []),
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"gone">>, _attrs, _els} | _acc].

decode_error_error_gone_cdata(<<>>) -> undefined;
decode_error_error_gone_cdata(_val) -> _val.

encode_error_error_gone_cdata(undefined, _acc) -> _acc;
encode_error_error_gone_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_error_error_forbidden({xmlel, _, _attrs,
			      _els}) ->
    forbidden.

encode_error_error_forbidden(undefined, _acc) -> _acc;
encode_error_error_forbidden(forbidden, _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"forbidden">>, _attrs, _els} | _acc].

'decode_error_error_feature-not-implemented'({xmlel, _,
					      _attrs, _els}) ->
    'feature-not-implemented'.

'encode_error_error_feature-not-implemented'(undefined,
					     _acc) ->
    _acc;
'encode_error_error_feature-not-implemented'('feature-not-implemented',
					     _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"feature-not-implemented">>, _attrs, _els}
     | _acc].

decode_error_error_conflict({xmlel, _, _attrs, _els}) ->
    conflict.

encode_error_error_conflict(undefined, _acc) -> _acc;
encode_error_error_conflict(conflict, _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"conflict">>, _attrs, _els} | _acc].

'decode_error_error_bad-request'({xmlel, _, _attrs,
				  _els}) ->
    'bad-request'.

'encode_error_error_bad-request'(undefined, _acc) ->
    _acc;
'encode_error_error_bad-request'('bad-request', _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}],
    [{xmlel, <<"bad-request">>, _attrs, _els} | _acc].

decode_error_error_text({xmlel, _, _attrs, _els}) ->
    Text_lang = decode_error_error_text_attrs(_attrs,
					      undefined),
    Cdata = decode_error_error_text_els(_els, <<>>),
    {Text_lang, Cdata}.

decode_error_error_text_els([{xmlcdata, _data} | _els],
			    Cdata) ->
    decode_error_error_text_els(_els,
				<<Cdata/binary, _data/binary>>);
decode_error_error_text_els([_ | _els], Cdata) ->
    decode_error_error_text_els(_els, Cdata);
decode_error_error_text_els([], Cdata) ->
    decode_error_error_text_cdata(Cdata).

decode_error_error_text_attrs([{<<"xml:lang">>, _val}
			       | _attrs],
			      _Text_lang) ->
    decode_error_error_text_attrs(_attrs, _val);
decode_error_error_text_attrs([_ | _attrs],
			      Text_lang) ->
    decode_error_error_text_attrs(_attrs, Text_lang);
decode_error_error_text_attrs([], Text_lang) ->
    'decode_error_error_text_xml:lang'(Text_lang).

encode_error_error_text(undefined, _acc) -> _acc;
encode_error_error_text({Text_lang, Cdata}, _acc) ->
    _els = encode_error_error_text_cdata(Cdata, []),
    _attrs = 'encode_error_error_text_xml:lang'(Text_lang,
						[{<<"xmlns">>,
						  <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}]),
    [{xmlel, <<"text">>, _attrs, _els} | _acc].

'decode_error_error_text_xml:lang'(undefined) ->
    undefined;
'decode_error_error_text_xml:lang'(_val) -> _val.

'encode_error_error_text_xml:lang'(undefined, _acc) ->
    _acc;
'encode_error_error_text_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_error_error_text_cdata(<<>>) -> undefined;
decode_error_error_text_cdata(_val) -> _val.

encode_error_error_text_cdata(undefined, _acc) -> _acc;
encode_error_error_text_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_bind_bind({xmlel, _, _attrs, _els}) ->
    {Resource, Jid} = decode_bind_bind_els(_els, undefined,
					   undefined),
    {bind, Jid, Resource}.

decode_bind_bind_els([{xmlel, <<"resource">>, _attrs,
		       _} =
			  _el
		      | _els],
		     Resource, Jid) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_bind_bind_els(_els,
			       decode_bind_bind_resource(_el), Jid);
      _ -> decode_bind_bind_els(_els, Resource, Jid)
    end;
decode_bind_bind_els([{xmlel, <<"jid">>, _attrs, _} =
			  _el
		      | _els],
		     Resource, Jid) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_bind_bind_els(_els, Resource,
			       decode_bind_bind_jid(_el));
      _ -> decode_bind_bind_els(_els, Resource, Jid)
    end;
decode_bind_bind_els([_ | _els], Resource, Jid) ->
    decode_bind_bind_els(_els, Resource, Jid);
decode_bind_bind_els([], Resource, Jid) ->
    {Resource, Jid}.

encode_bind_bind(undefined, _acc) -> _acc;
encode_bind_bind({bind, Jid, Resource}, _acc) ->
    _els = encode_bind_bind_jid(Jid,
				encode_bind_bind_resource(Resource, [])),
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-bind">>}],
    [{xmlel, <<"bind">>, _attrs, _els} | _acc].

decode_bind_bind_resource({xmlel, _, _attrs, _els}) ->
    Cdata = decode_bind_bind_resource_els(_els, <<>>),
    Cdata.

decode_bind_bind_resource_els([{xmlcdata, _data}
			       | _els],
			      Cdata) ->
    decode_bind_bind_resource_els(_els,
				  <<Cdata/binary, _data/binary>>);
decode_bind_bind_resource_els([_ | _els], Cdata) ->
    decode_bind_bind_resource_els(_els, Cdata);
decode_bind_bind_resource_els([], Cdata) ->
    decode_bind_bind_resource_cdata(Cdata).

encode_bind_bind_resource(undefined, _acc) -> _acc;
encode_bind_bind_resource(Cdata, _acc) ->
    _els = encode_bind_bind_resource_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"resource">>, _attrs, _els} | _acc].

decode_bind_bind_resource_cdata(<<>>) -> undefined;
decode_bind_bind_resource_cdata(_val) ->
    case catch resourceprep(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_cdata_value, <<>>, <<"resource">>,
			<<>>});
      _res -> _res
    end.

encode_bind_bind_resource_cdata(undefined, _acc) ->
    _acc;
encode_bind_bind_resource_cdata(_val, _acc) ->
    [{xmlcdata, resourceprep(_val)} | _acc].

decode_bind_bind_jid({xmlel, _, _attrs, _els}) ->
    Cdata = decode_bind_bind_jid_els(_els, <<>>), Cdata.

decode_bind_bind_jid_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_bind_bind_jid_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_bind_bind_jid_els([_ | _els], Cdata) ->
    decode_bind_bind_jid_els(_els, Cdata);
decode_bind_bind_jid_els([], Cdata) ->
    decode_bind_bind_jid_cdata(Cdata).

encode_bind_bind_jid(undefined, _acc) -> _acc;
encode_bind_bind_jid(Cdata, _acc) ->
    _els = encode_bind_bind_jid_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"jid">>, _attrs, _els} | _acc].

decode_bind_bind_jid_cdata(<<>>) -> undefined;
decode_bind_bind_jid_cdata(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_cdata_value, <<>>, <<"jid">>, <<>>});
      _res -> _res
    end.

encode_bind_bind_jid_cdata(undefined, _acc) -> _acc;
encode_bind_bind_jid_cdata(_val, _acc) ->
    [{xmlcdata, enc_jid(_val)} | _acc].

decode_sasl_auth_auth({xmlel, _, _attrs, _els}) ->
    Mechanism = decode_sasl_auth_auth_attrs(_attrs,
					    undefined),
    Cdata = decode_sasl_auth_auth_els(_els, <<>>),
    {sasl_auth, Mechanism, Cdata}.

decode_sasl_auth_auth_els([{xmlcdata, _data} | _els],
			  Cdata) ->
    decode_sasl_auth_auth_els(_els,
			      <<Cdata/binary, _data/binary>>);
decode_sasl_auth_auth_els([_ | _els], Cdata) ->
    decode_sasl_auth_auth_els(_els, Cdata);
decode_sasl_auth_auth_els([], Cdata) ->
    decode_sasl_auth_auth_cdata(Cdata).

decode_sasl_auth_auth_attrs([{<<"mechanism">>, _val}
			     | _attrs],
			    _Mechanism) ->
    decode_sasl_auth_auth_attrs(_attrs, _val);
decode_sasl_auth_auth_attrs([_ | _attrs], Mechanism) ->
    decode_sasl_auth_auth_attrs(_attrs, Mechanism);
decode_sasl_auth_auth_attrs([], Mechanism) ->
    decode_sasl_auth_auth_mechanism(Mechanism).

encode_sasl_auth_auth(undefined, _acc) -> _acc;
encode_sasl_auth_auth({sasl_auth, Mechanism, Cdata},
		      _acc) ->
    _els = encode_sasl_auth_auth_cdata(Cdata, []),
    _attrs = encode_sasl_auth_auth_mechanism(Mechanism,
					     [{<<"xmlns">>,
					       <<"urn:ietf:params:xml:ns:xmpp-sasl">>}]),
    [{xmlel, <<"auth">>, _attrs, _els} | _acc].

decode_sasl_auth_auth_mechanism(undefined) ->
    erlang:error({missing_attr, <<"mechanism">>, <<"auth">>,
		  <<"urn:ietf:params:xml:ns:xmpp-sasl">>});
decode_sasl_auth_auth_mechanism(_val) -> _val.

encode_sasl_auth_auth_mechanism(_val, _acc) ->
    [{<<"mechanism">>, _val} | _acc].

decode_sasl_auth_auth_cdata(<<>>) -> undefined;
decode_sasl_auth_auth_cdata(_val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_cdata_value, <<>>, <<"auth">>,
			<<"urn:ietf:params:xml:ns:xmpp-sasl">>});
      _res -> _res
    end.

encode_sasl_auth_auth_cdata(undefined, _acc) -> _acc;
encode_sasl_auth_auth_cdata(_val, _acc) ->
    [{xmlcdata, base64:encode(_val)} | _acc].

decode_sasl_abort_abort({xmlel, _, _attrs, _els}) ->
    {sasl_abort}.

encode_sasl_abort_abort(undefined, _acc) -> _acc;
encode_sasl_abort_abort({sasl_abort}, _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-sasl">>}],
    [{xmlel, <<"abort">>, _attrs, _els} | _acc].

decode_sasl_challenge_challenge({xmlel, _, _attrs,
				 _els}) ->
    Cdata = decode_sasl_challenge_challenge_els(_els, <<>>),
    {sasl_challenge, Cdata}.

decode_sasl_challenge_challenge_els([{xmlcdata, _data}
				     | _els],
				    Cdata) ->
    decode_sasl_challenge_challenge_els(_els,
					<<Cdata/binary, _data/binary>>);
decode_sasl_challenge_challenge_els([_ | _els],
				    Cdata) ->
    decode_sasl_challenge_challenge_els(_els, Cdata);
decode_sasl_challenge_challenge_els([], Cdata) ->
    decode_sasl_challenge_challenge_cdata(Cdata).

encode_sasl_challenge_challenge(undefined, _acc) ->
    _acc;
encode_sasl_challenge_challenge({sasl_challenge, Cdata},
				_acc) ->
    _els = encode_sasl_challenge_challenge_cdata(Cdata, []),
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-sasl">>}],
    [{xmlel, <<"challenge">>, _attrs, _els} | _acc].

decode_sasl_challenge_challenge_cdata(<<>>) ->
    undefined;
decode_sasl_challenge_challenge_cdata(_val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_cdata_value, <<>>, <<"challenge">>,
			<<"urn:ietf:params:xml:ns:xmpp-sasl">>});
      _res -> _res
    end.

encode_sasl_challenge_challenge_cdata(undefined,
				      _acc) ->
    _acc;
encode_sasl_challenge_challenge_cdata(_val, _acc) ->
    [{xmlcdata, base64:encode(_val)} | _acc].

decode_sasl_response_response({xmlel, _, _attrs,
			       _els}) ->
    Cdata = decode_sasl_response_response_els(_els, <<>>),
    {sasl_response, Cdata}.

decode_sasl_response_response_els([{xmlcdata, _data}
				   | _els],
				  Cdata) ->
    decode_sasl_response_response_els(_els,
				      <<Cdata/binary, _data/binary>>);
decode_sasl_response_response_els([_ | _els], Cdata) ->
    decode_sasl_response_response_els(_els, Cdata);
decode_sasl_response_response_els([], Cdata) ->
    decode_sasl_response_response_cdata(Cdata).

encode_sasl_response_response(undefined, _acc) -> _acc;
encode_sasl_response_response({sasl_response, Cdata},
			      _acc) ->
    _els = encode_sasl_response_response_cdata(Cdata, []),
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-sasl">>}],
    [{xmlel, <<"response">>, _attrs, _els} | _acc].

decode_sasl_response_response_cdata(<<>>) -> undefined;
decode_sasl_response_response_cdata(_val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_cdata_value, <<>>, <<"response">>,
			<<"urn:ietf:params:xml:ns:xmpp-sasl">>});
      _res -> _res
    end.

encode_sasl_response_response_cdata(undefined, _acc) ->
    _acc;
encode_sasl_response_response_cdata(_val, _acc) ->
    [{xmlcdata, base64:encode(_val)} | _acc].

decode_sasl_success_success({xmlel, _, _attrs, _els}) ->
    Cdata = decode_sasl_success_success_els(_els, <<>>),
    {sasl_success, Cdata}.

decode_sasl_success_success_els([{xmlcdata, _data}
				 | _els],
				Cdata) ->
    decode_sasl_success_success_els(_els,
				    <<Cdata/binary, _data/binary>>);
decode_sasl_success_success_els([_ | _els], Cdata) ->
    decode_sasl_success_success_els(_els, Cdata);
decode_sasl_success_success_els([], Cdata) ->
    decode_sasl_success_success_cdata(Cdata).

encode_sasl_success_success(undefined, _acc) -> _acc;
encode_sasl_success_success({sasl_success, Cdata},
			    _acc) ->
    _els = encode_sasl_success_success_cdata(Cdata, []),
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-sasl">>}],
    [{xmlel, <<"success">>, _attrs, _els} | _acc].

decode_sasl_success_success_cdata(<<>>) -> undefined;
decode_sasl_success_success_cdata(_val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_cdata_value, <<>>, <<"success">>,
			<<"urn:ietf:params:xml:ns:xmpp-sasl">>});
      _res -> _res
    end.

encode_sasl_success_success_cdata(undefined, _acc) ->
    _acc;
encode_sasl_success_success_cdata(_val, _acc) ->
    [{xmlcdata, base64:encode(_val)} | _acc].

decode_sasl_failure_failure({xmlel, _, _attrs, _els}) ->
    {Text, Reason} = decode_sasl_failure_failure_els(_els,
						     undefined, undefined),
    {sasl_failure, Reason, Text}.

decode_sasl_failure_failure_els([{xmlel, <<"text">>,
				  _attrs, _} =
				     _el
				 | _els],
				Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_sasl_failure_failure_els(_els,
					  decode_sasl_failure_failure_text(_el),
					  Reason);
      _ -> decode_sasl_failure_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_failure_els([{xmlel,
				  <<"temporary-auth-failure">>, _attrs, _} =
				     _el
				 | _els],
				Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_sasl_failure_failure_els(_els, Text,
					  'decode_sasl_failure_failure_temporary-auth-failure'(_el));
      _ -> decode_sasl_failure_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_failure_els([{xmlel,
				  <<"not-authorized">>, _attrs, _} =
				     _el
				 | _els],
				Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_sasl_failure_failure_els(_els, Text,
					  'decode_sasl_failure_failure_not-authorized'(_el));
      _ -> decode_sasl_failure_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_failure_els([{xmlel,
				  <<"mechanism-too-weak">>, _attrs, _} =
				     _el
				 | _els],
				Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_sasl_failure_failure_els(_els, Text,
					  'decode_sasl_failure_failure_mechanism-too-weak'(_el));
      _ -> decode_sasl_failure_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_failure_els([{xmlel,
				  <<"malformed-request">>, _attrs, _} =
				     _el
				 | _els],
				Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_sasl_failure_failure_els(_els, Text,
					  'decode_sasl_failure_failure_malformed-request'(_el));
      _ -> decode_sasl_failure_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_failure_els([{xmlel,
				  <<"invalid-mechanism">>, _attrs, _} =
				     _el
				 | _els],
				Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_sasl_failure_failure_els(_els, Text,
					  'decode_sasl_failure_failure_invalid-mechanism'(_el));
      _ -> decode_sasl_failure_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_failure_els([{xmlel,
				  <<"invalid-authzid">>, _attrs, _} =
				     _el
				 | _els],
				Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_sasl_failure_failure_els(_els, Text,
					  'decode_sasl_failure_failure_invalid-authzid'(_el));
      _ -> decode_sasl_failure_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_failure_els([{xmlel,
				  <<"incorrect-encoding">>, _attrs, _} =
				     _el
				 | _els],
				Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_sasl_failure_failure_els(_els, Text,
					  'decode_sasl_failure_failure_incorrect-encoding'(_el));
      _ -> decode_sasl_failure_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_failure_els([{xmlel,
				  <<"encryption-required">>, _attrs, _} =
				     _el
				 | _els],
				Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_sasl_failure_failure_els(_els, Text,
					  'decode_sasl_failure_failure_encryption-required'(_el));
      _ -> decode_sasl_failure_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_failure_els([{xmlel,
				  <<"credentials-expired">>, _attrs, _} =
				     _el
				 | _els],
				Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_sasl_failure_failure_els(_els, Text,
					  'decode_sasl_failure_failure_credentials-expired'(_el));
      _ -> decode_sasl_failure_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_failure_els([{xmlel,
				  <<"account-disabled">>, _attrs, _} =
				     _el
				 | _els],
				Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_sasl_failure_failure_els(_els, Text,
					  'decode_sasl_failure_failure_account-disabled'(_el));
      _ -> decode_sasl_failure_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_failure_els([{xmlel, <<"aborted">>,
				  _attrs, _} =
				     _el
				 | _els],
				Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_sasl_failure_failure_els(_els, Text,
					  decode_sasl_failure_failure_aborted(_el));
      _ -> decode_sasl_failure_failure_els(_els, Text, Reason)
    end;
decode_sasl_failure_failure_els([_ | _els], Text,
				Reason) ->
    decode_sasl_failure_failure_els(_els, Text, Reason);
decode_sasl_failure_failure_els([], Text, Reason) ->
    {Text, Reason}.

'encode_sasl_failure_failure_$reason'(undefined,
				      _acc) ->
    _acc;
'encode_sasl_failure_failure_$reason'('temporary-auth-failure' =
					  _r,
				      _acc) ->
    'encode_sasl_failure_failure_temporary-auth-failure'(_r,
							 _acc);
'encode_sasl_failure_failure_$reason'('not-authorized' =
					  _r,
				      _acc) ->
    'encode_sasl_failure_failure_not-authorized'(_r, _acc);
'encode_sasl_failure_failure_$reason'('mechanism-too-weak' =
					  _r,
				      _acc) ->
    'encode_sasl_failure_failure_mechanism-too-weak'(_r,
						     _acc);
'encode_sasl_failure_failure_$reason'('malformed-request' =
					  _r,
				      _acc) ->
    'encode_sasl_failure_failure_malformed-request'(_r,
						    _acc);
'encode_sasl_failure_failure_$reason'('invalid-mechanism' =
					  _r,
				      _acc) ->
    'encode_sasl_failure_failure_invalid-mechanism'(_r,
						    _acc);
'encode_sasl_failure_failure_$reason'('invalid-authzid' =
					  _r,
				      _acc) ->
    'encode_sasl_failure_failure_invalid-authzid'(_r, _acc);
'encode_sasl_failure_failure_$reason'('incorrect-encoding' =
					  _r,
				      _acc) ->
    'encode_sasl_failure_failure_incorrect-encoding'(_r,
						     _acc);
'encode_sasl_failure_failure_$reason'('encryption-required' =
					  _r,
				      _acc) ->
    'encode_sasl_failure_failure_encryption-required'(_r,
						      _acc);
'encode_sasl_failure_failure_$reason'('credentials-expired' =
					  _r,
				      _acc) ->
    'encode_sasl_failure_failure_credentials-expired'(_r,
						      _acc);
'encode_sasl_failure_failure_$reason'('account-disabled' =
					  _r,
				      _acc) ->
    'encode_sasl_failure_failure_account-disabled'(_r,
						   _acc);
'encode_sasl_failure_failure_$reason'(aborted = _r,
				      _acc) ->
    encode_sasl_failure_failure_aborted(_r, _acc).

encode_sasl_failure_failure(undefined, _acc) -> _acc;
encode_sasl_failure_failure({sasl_failure, Reason,
			     Text},
			    _acc) ->
    _els = 'encode_sasl_failure_failure_$reason'(Reason,
						 encode_sasl_failure_failure_text(Text,
										  [])),
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-sasl">>}],
    [{xmlel, <<"failure">>, _attrs, _els} | _acc].

'decode_sasl_failure_failure_temporary-auth-failure'({xmlel,
						      _, _attrs, _els}) ->
    'temporary-auth-failure'.

'encode_sasl_failure_failure_temporary-auth-failure'(undefined,
						     _acc) ->
    _acc;
'encode_sasl_failure_failure_temporary-auth-failure'('temporary-auth-failure',
						     _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"temporary-auth-failure">>, _attrs, _els}
     | _acc].

'decode_sasl_failure_failure_not-authorized'({xmlel, _,
					      _attrs, _els}) ->
    'not-authorized'.

'encode_sasl_failure_failure_not-authorized'(undefined,
					     _acc) ->
    _acc;
'encode_sasl_failure_failure_not-authorized'('not-authorized',
					     _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"not-authorized">>, _attrs, _els} | _acc].

'decode_sasl_failure_failure_mechanism-too-weak'({xmlel,
						  _, _attrs, _els}) ->
    'mechanism-too-weak'.

'encode_sasl_failure_failure_mechanism-too-weak'(undefined,
						 _acc) ->
    _acc;
'encode_sasl_failure_failure_mechanism-too-weak'('mechanism-too-weak',
						 _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"mechanism-too-weak">>, _attrs, _els}
     | _acc].

'decode_sasl_failure_failure_malformed-request'({xmlel,
						 _, _attrs, _els}) ->
    'malformed-request'.

'encode_sasl_failure_failure_malformed-request'(undefined,
						_acc) ->
    _acc;
'encode_sasl_failure_failure_malformed-request'('malformed-request',
						_acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"malformed-request">>, _attrs, _els} | _acc].

'decode_sasl_failure_failure_invalid-mechanism'({xmlel,
						 _, _attrs, _els}) ->
    'invalid-mechanism'.

'encode_sasl_failure_failure_invalid-mechanism'(undefined,
						_acc) ->
    _acc;
'encode_sasl_failure_failure_invalid-mechanism'('invalid-mechanism',
						_acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"invalid-mechanism">>, _attrs, _els} | _acc].

'decode_sasl_failure_failure_invalid-authzid'({xmlel, _,
					       _attrs, _els}) ->
    'invalid-authzid'.

'encode_sasl_failure_failure_invalid-authzid'(undefined,
					      _acc) ->
    _acc;
'encode_sasl_failure_failure_invalid-authzid'('invalid-authzid',
					      _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"invalid-authzid">>, _attrs, _els} | _acc].

'decode_sasl_failure_failure_incorrect-encoding'({xmlel,
						  _, _attrs, _els}) ->
    'incorrect-encoding'.

'encode_sasl_failure_failure_incorrect-encoding'(undefined,
						 _acc) ->
    _acc;
'encode_sasl_failure_failure_incorrect-encoding'('incorrect-encoding',
						 _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"incorrect-encoding">>, _attrs, _els}
     | _acc].

'decode_sasl_failure_failure_encryption-required'({xmlel,
						   _, _attrs, _els}) ->
    'encryption-required'.

'encode_sasl_failure_failure_encryption-required'(undefined,
						  _acc) ->
    _acc;
'encode_sasl_failure_failure_encryption-required'('encryption-required',
						  _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"encryption-required">>, _attrs, _els}
     | _acc].

'decode_sasl_failure_failure_credentials-expired'({xmlel,
						   _, _attrs, _els}) ->
    'credentials-expired'.

'encode_sasl_failure_failure_credentials-expired'(undefined,
						  _acc) ->
    _acc;
'encode_sasl_failure_failure_credentials-expired'('credentials-expired',
						  _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"credentials-expired">>, _attrs, _els}
     | _acc].

'decode_sasl_failure_failure_account-disabled'({xmlel,
						_, _attrs, _els}) ->
    'account-disabled'.

'encode_sasl_failure_failure_account-disabled'(undefined,
					       _acc) ->
    _acc;
'encode_sasl_failure_failure_account-disabled'('account-disabled',
					       _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"account-disabled">>, _attrs, _els} | _acc].

decode_sasl_failure_failure_aborted({xmlel, _, _attrs,
				     _els}) ->
    aborted.

encode_sasl_failure_failure_aborted(undefined, _acc) ->
    _acc;
encode_sasl_failure_failure_aborted(aborted, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"aborted">>, _attrs, _els} | _acc].

decode_sasl_failure_failure_text({xmlel, _, _attrs,
				  _els}) ->
    Text_lang =
	decode_sasl_failure_failure_text_attrs(_attrs,
					       undefined),
    Cdata = decode_sasl_failure_failure_text_els(_els,
						 <<>>),
    {Text_lang, Cdata}.

decode_sasl_failure_failure_text_els([{xmlcdata, _data}
				      | _els],
				     Cdata) ->
    decode_sasl_failure_failure_text_els(_els,
					 <<Cdata/binary, _data/binary>>);
decode_sasl_failure_failure_text_els([_ | _els],
				     Cdata) ->
    decode_sasl_failure_failure_text_els(_els, Cdata);
decode_sasl_failure_failure_text_els([], Cdata) ->
    decode_sasl_failure_failure_text_cdata(Cdata).

decode_sasl_failure_failure_text_attrs([{<<"xml:lang">>,
					 _val}
					| _attrs],
				       _Text_lang) ->
    decode_sasl_failure_failure_text_attrs(_attrs, _val);
decode_sasl_failure_failure_text_attrs([_ | _attrs],
				       Text_lang) ->
    decode_sasl_failure_failure_text_attrs(_attrs,
					   Text_lang);
decode_sasl_failure_failure_text_attrs([], Text_lang) ->
    'decode_sasl_failure_failure_text_xml:lang'(Text_lang).

encode_sasl_failure_failure_text(undefined, _acc) ->
    _acc;
encode_sasl_failure_failure_text({Text_lang, Cdata},
				 _acc) ->
    _els = encode_sasl_failure_failure_text_cdata(Cdata,
						  []),
    _attrs =
	'encode_sasl_failure_failure_text_xml:lang'(Text_lang,
						    []),
    [{xmlel, <<"text">>, _attrs, _els} | _acc].

'decode_sasl_failure_failure_text_xml:lang'(undefined) ->
    undefined;
'decode_sasl_failure_failure_text_xml:lang'(_val) ->
    _val.

'encode_sasl_failure_failure_text_xml:lang'(undefined,
					    _acc) ->
    _acc;
'encode_sasl_failure_failure_text_xml:lang'(_val,
					    _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_sasl_failure_failure_text_cdata(<<>>) ->
    undefined;
decode_sasl_failure_failure_text_cdata(_val) -> _val.

encode_sasl_failure_failure_text_cdata(undefined,
				       _acc) ->
    _acc;
encode_sasl_failure_failure_text_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_sasl_mechanism_mechanism({xmlel, _, _attrs,
				 _els}) ->
    Cdata = decode_sasl_mechanism_mechanism_els(_els, <<>>),
    Cdata.

decode_sasl_mechanism_mechanism_els([{xmlcdata, _data}
				     | _els],
				    Cdata) ->
    decode_sasl_mechanism_mechanism_els(_els,
					<<Cdata/binary, _data/binary>>);
decode_sasl_mechanism_mechanism_els([_ | _els],
				    Cdata) ->
    decode_sasl_mechanism_mechanism_els(_els, Cdata);
decode_sasl_mechanism_mechanism_els([], Cdata) ->
    decode_sasl_mechanism_mechanism_cdata(Cdata).

encode_sasl_mechanism_mechanism([], _acc) -> _acc;
encode_sasl_mechanism_mechanism([Cdata | _tail],
				_acc) ->
    _els = encode_sasl_mechanism_mechanism_cdata(Cdata, []),
    _attrs = [],
    encode_sasl_mechanism_mechanism(_tail,
				    [{xmlel, <<"mechanism">>, _attrs, _els}
				     | _acc]).

decode_sasl_mechanism_mechanism_cdata(<<>>) ->
    undefined;
decode_sasl_mechanism_mechanism_cdata(_val) -> _val.

encode_sasl_mechanism_mechanism_cdata(undefined,
				      _acc) ->
    _acc;
encode_sasl_mechanism_mechanism_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_sasl_mechanisms_mechanisms({xmlel, _, _attrs,
				   _els}) ->
    Mechanism = decode_sasl_mechanisms_mechanisms_els(_els,
						      []),
    {sasl_mechanisms, Mechanism}.

decode_sasl_mechanisms_mechanisms_els([{xmlel,
					<<"mechanism">>, _attrs, _} =
					   _el
				       | _els],
				      Mechanism) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_sasl_mechanisms_mechanisms_els(_els,
						[decode_sasl_mechanism_mechanism(_el)
						 | Mechanism]);
      _ ->
	  decode_sasl_mechanisms_mechanisms_els(_els, Mechanism)
    end;
decode_sasl_mechanisms_mechanisms_els([_ | _els],
				      Mechanism) ->
    decode_sasl_mechanisms_mechanisms_els(_els, Mechanism);
decode_sasl_mechanisms_mechanisms_els([], Mechanism) ->
    xml_gen:reverse(Mechanism, 1, infinity).

encode_sasl_mechanisms_mechanisms(undefined, _acc) ->
    _acc;
encode_sasl_mechanisms_mechanisms({sasl_mechanisms,
				   Mechanism},
				  _acc) ->
    _els = encode_sasl_mechanism_mechanism(Mechanism, []),
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-sasl">>}],
    [{xmlel, <<"mechanisms">>, _attrs, _els} | _acc].

decode_starttls_starttls({xmlel, _, _attrs, _els}) ->
    Required = decode_starttls_starttls_els(_els, false),
    {starttls, Required}.

decode_starttls_starttls_els([{xmlel, <<"required">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Required) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_starttls_starttls_els(_els,
				       decode_starttls_starttls_required(_el));
      _ -> decode_starttls_starttls_els(_els, Required)
    end;
decode_starttls_starttls_els([_ | _els], Required) ->
    decode_starttls_starttls_els(_els, Required);
decode_starttls_starttls_els([], Required) -> Required.

encode_starttls_starttls(undefined, _acc) -> _acc;
encode_starttls_starttls({starttls, Required}, _acc) ->
    _els = encode_starttls_starttls_required(Required, []),
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-tls">>}],
    [{xmlel, <<"starttls">>, _attrs, _els} | _acc].

decode_starttls_starttls_required({xmlel, _, _attrs,
				   _els}) ->
    true.

encode_starttls_starttls_required(false, _acc) -> _acc;
encode_starttls_starttls_required(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"required">>, _attrs, _els} | _acc].

decode_starttls_proceed_proceed({xmlel, _, _attrs,
				 _els}) ->
    {starttls_proceed}.

encode_starttls_proceed_proceed(undefined, _acc) ->
    _acc;
encode_starttls_proceed_proceed({starttls_proceed},
				_acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-tls">>}],
    [{xmlel, <<"proceed">>, _attrs, _els} | _acc].

decode_starttls_failure_failure({xmlel, _, _attrs,
				 _els}) ->
    {starttls_failure}.

encode_starttls_failure_failure(undefined, _acc) ->
    _acc;
encode_starttls_failure_failure({starttls_failure},
				_acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-tls">>}],
    [{xmlel, <<"failure">>, _attrs, _els} | _acc].

'decode_stream_features_stream:features'({xmlel, _,
					  _attrs, _els}) ->
    __Els =
	'decode_stream_features_stream:features_els'(_els, []),
    {stream_features, __Els}.

'decode_stream_features_stream:features_els'([{xmlel, _,
					       _, _} =
						  _el
					      | _els],
					     __Els) ->
    'decode_stream_features_stream:features_els'(_els,
						 [decode(_el) | __Els]);
'decode_stream_features_stream:features_els'([_ | _els],
					     __Els) ->
    'decode_stream_features_stream:features_els'(_els,
						 __Els);
'decode_stream_features_stream:features_els'([],
					     __Els) ->
    lists:reverse(__Els).

'encode_stream_features_stream:features'(undefined,
					 _acc) ->
    _acc;
'encode_stream_features_stream:features'({stream_features,
					  __Els},
					 _acc) ->
    _els = [encode(_subel) || _subel <- __Els] ++ [],
    _attrs = [],
    [{xmlel, <<"stream:features">>, _attrs, _els} | _acc].

decode_p1_push_push({xmlel, _, _attrs, _els}) ->
    {p1_push}.

encode_p1_push_push(undefined, _acc) -> _acc;
encode_p1_push_push({p1_push}, _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>, <<"p1:push">>}],
    [{xmlel, <<"push">>, _attrs, _els} | _acc].

decode_p1_rebind_rebind({xmlel, _, _attrs, _els}) ->
    {p1_rebind}.

encode_p1_rebind_rebind(undefined, _acc) -> _acc;
encode_p1_rebind_rebind({p1_rebind}, _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>, <<"p1:rebind">>}],
    [{xmlel, <<"rebind">>, _attrs, _els} | _acc].

decode_p1_ack_ack({xmlel, _, _attrs, _els}) -> {p1_ack}.

encode_p1_ack_ack(undefined, _acc) -> _acc;
encode_p1_ack_ack({p1_ack}, _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>, <<"p1:ack">>}],
    [{xmlel, <<"ack">>, _attrs, _els} | _acc].

decode_caps_c({xmlel, _, _attrs, _els}) ->
    {Ver, Node, Hash} = decode_caps_c_attrs(_attrs,
					    undefined, undefined, undefined),
    {caps, Hash, Node, Ver}.

decode_caps_c_attrs([{<<"ver">>, _val} | _attrs], _Ver,
		    Node, Hash) ->
    decode_caps_c_attrs(_attrs, _val, Node, Hash);
decode_caps_c_attrs([{<<"node">>, _val} | _attrs], Ver,
		    _Node, Hash) ->
    decode_caps_c_attrs(_attrs, Ver, _val, Hash);
decode_caps_c_attrs([{<<"hash">>, _val} | _attrs], Ver,
		    Node, _Hash) ->
    decode_caps_c_attrs(_attrs, Ver, Node, _val);
decode_caps_c_attrs([_ | _attrs], Ver, Node, Hash) ->
    decode_caps_c_attrs(_attrs, Ver, Node, Hash);
decode_caps_c_attrs([], Ver, Node, Hash) ->
    {decode_caps_c_ver(Ver), decode_caps_c_node(Node),
     decode_caps_c_hash(Hash)}.

encode_caps_c(undefined, _acc) -> _acc;
encode_caps_c({caps, Hash, Node, Ver}, _acc) ->
    _els = [],
    _attrs = encode_caps_c_hash(Hash,
				encode_caps_c_node(Node,
						   encode_caps_c_ver(Ver,
								     [{<<"xmlns">>,
								       <<"http://jabber.org/protocol/caps">>}]))),
    [{xmlel, <<"c">>, _attrs, _els} | _acc].

decode_caps_c_hash(undefined) -> undefined;
decode_caps_c_hash(_val) -> _val.

encode_caps_c_hash(undefined, _acc) -> _acc;
encode_caps_c_hash(_val, _acc) ->
    [{<<"hash">>, _val} | _acc].

decode_caps_c_node(undefined) -> undefined;
decode_caps_c_node(_val) -> _val.

encode_caps_c_node(undefined, _acc) -> _acc;
encode_caps_c_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_caps_c_ver(undefined) -> undefined;
decode_caps_c_ver(_val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"ver">>, <<"c">>,
			<<"http://jabber.org/protocol/caps">>});
      _res -> _res
    end.

encode_caps_c_ver(undefined, _acc) -> _acc;
encode_caps_c_ver(_val, _acc) ->
    [{<<"ver">>, base64:encode(_val)} | _acc].

decode_register_register({xmlel, _, _attrs, _els}) ->
    {register}.

encode_register_register(undefined, _acc) -> _acc;
encode_register_register({register}, _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"http://jabber.org/features/iq-register">>}],
    [{xmlel, <<"register">>, _attrs, _els} | _acc].

decode_session_session({xmlel, _, _attrs, _els}) ->
    {session}.

encode_session_session(undefined, _acc) -> _acc;
encode_session_session({session}, _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-session">>}],
    [{xmlel, <<"session">>, _attrs, _els} | _acc].

decode_ping_ping({xmlel, _, _attrs, _els}) -> {ping}.

encode_ping_ping(undefined, _acc) -> _acc;
encode_ping_ping({ping}, _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>, <<"urn:xmpp:ping">>}],
    [{xmlel, <<"ping">>, _attrs, _els} | _acc].

decode_time_time({xmlel, _, _attrs, _els}) ->
    {Utc, Tzo} = decode_time_time_els(_els, undefined,
				      undefined),
    {time, Tzo, Utc}.

decode_time_time_els([{xmlel, <<"utc">>, _attrs, _} =
			  _el
		      | _els],
		     Utc, Tzo) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_time_time_els(_els, decode_time_time_utc(_el),
			       Tzo);
      _ -> decode_time_time_els(_els, Utc, Tzo)
    end;
decode_time_time_els([{xmlel, <<"tzo">>, _attrs, _} =
			  _el
		      | _els],
		     Utc, Tzo) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_time_time_els(_els, Utc,
			       decode_time_time_tzo(_el));
      _ -> decode_time_time_els(_els, Utc, Tzo)
    end;
decode_time_time_els([_ | _els], Utc, Tzo) ->
    decode_time_time_els(_els, Utc, Tzo);
decode_time_time_els([], Utc, Tzo) -> {Utc, Tzo}.

encode_time_time(undefined, _acc) -> _acc;
encode_time_time({time, Tzo, Utc}, _acc) ->
    _els = encode_time_time_tzo(Tzo,
				encode_time_time_utc(Utc, [])),
    _attrs = [{<<"xmlns">>, <<"urn:xmpp:time">>}],
    [{xmlel, <<"time">>, _attrs, _els} | _acc].

decode_time_time_utc({xmlel, _, _attrs, _els}) ->
    Cdata = decode_time_time_utc_els(_els, <<>>), Cdata.

decode_time_time_utc_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_time_time_utc_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_time_time_utc_els([_ | _els], Cdata) ->
    decode_time_time_utc_els(_els, Cdata);
decode_time_time_utc_els([], Cdata) ->
    decode_time_time_utc_cdata(Cdata).

encode_time_time_utc(undefined, _acc) -> _acc;
encode_time_time_utc(Cdata, _acc) ->
    _els = encode_time_time_utc_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"utc">>, _attrs, _els} | _acc].

decode_time_time_utc_cdata(<<>>) -> undefined;
decode_time_time_utc_cdata(_val) ->
    case catch dec_utc(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_cdata_value, <<>>, <<"utc">>, <<>>});
      _res -> _res
    end.

encode_time_time_utc_cdata(undefined, _acc) -> _acc;
encode_time_time_utc_cdata(_val, _acc) ->
    [{xmlcdata, enc_utc(_val)} | _acc].

decode_time_time_tzo({xmlel, _, _attrs, _els}) ->
    Cdata = decode_time_time_tzo_els(_els, <<>>), Cdata.

decode_time_time_tzo_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_time_time_tzo_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_time_time_tzo_els([_ | _els], Cdata) ->
    decode_time_time_tzo_els(_els, Cdata);
decode_time_time_tzo_els([], Cdata) ->
    decode_time_time_tzo_cdata(Cdata).

encode_time_time_tzo(undefined, _acc) -> _acc;
encode_time_time_tzo(Cdata, _acc) ->
    _els = encode_time_time_tzo_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"tzo">>, _attrs, _els} | _acc].

decode_time_time_tzo_cdata(<<>>) -> undefined;
decode_time_time_tzo_cdata(_val) ->
    case catch dec_tzo(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_cdata_value, <<>>, <<"tzo">>, <<>>});
      _res -> _res
    end.

encode_time_time_tzo_cdata(undefined, _acc) -> _acc;
encode_time_time_tzo_cdata(_val, _acc) ->
    [{xmlcdata, enc_tzo(_val)} | _acc].

'decode_stream_error_stream:error'({xmlel, _, _attrs,
				    _els}) ->
    {Text, Reason} =
	'decode_stream_error_stream:error_els'(_els, undefined,
					       undefined),
    {stream_error, Reason, Text}.

'decode_stream_error_stream:error_els'([{xmlel,
					 <<"text">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els,
						 'decode_stream_error_stream:error_text'(_el),
						 Reason);
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"unsupported-version">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_unsupported-version'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"unsupported-stanza-type">>, _attrs,
					 _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_unsupported-stanza-type'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"unsupported-encoding">>, _attrs,
					 _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_unsupported-encoding'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"undefined-condition">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_undefined-condition'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"system-shutdown">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_system-shutdown'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"see-other-host">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_see-other-host'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"restricted-xml">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_restricted-xml'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"resource-constraint">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_resource-constraint'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"reset">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_reset'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"remote-connection-failed">>, _attrs,
					 _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_remote-connection-failed'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"policy-violation">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_policy-violation'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"not-well-formed">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_not-well-formed'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"not-authorized">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_not-authorized'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"invalid-xml">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_invalid-xml'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"invalid-namespace">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_invalid-namespace'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"invalid-id">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_invalid-id'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"invalid-from">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_invalid-from'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"internal-server-error">>, _attrs,
					 _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_internal-server-error'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"improper-addressing">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_improper-addressing'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"host-unknown">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_host-unknown'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"host-gone">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_host-gone'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"connection-timeout">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_connection-timeout'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"conflict">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_conflict'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"bad-namespace-prefix">>, _attrs,
					 _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_bad-namespace-prefix'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([{xmlel,
					 <<"bad-format">>, _attrs, _} =
					    _el
					| _els],
				       Text, Reason) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 'decode_stream_error_stream:error_bad-format'(_el));
      _ ->
	  'decode_stream_error_stream:error_els'(_els, Text,
						 Reason)
    end;
'decode_stream_error_stream:error_els'([_ | _els], Text,
				       Reason) ->
    'decode_stream_error_stream:error_els'(_els, Text,
					   Reason);
'decode_stream_error_stream:error_els'([], Text,
				       Reason) ->
    {Text, Reason}.

'encode_stream_error_stream:error_$reason'(undefined,
					   _acc) ->
    _acc;
'encode_stream_error_stream:error_$reason'('unsupported-version' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_unsupported-version'(_r,
							   _acc);
'encode_stream_error_stream:error_$reason'('unsupported-stanza-type' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_unsupported-stanza-type'(_r,
							       _acc);
'encode_stream_error_stream:error_$reason'('unsupported-encoding' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_unsupported-encoding'(_r,
							    _acc);
'encode_stream_error_stream:error_$reason'('undefined-condition' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_undefined-condition'(_r,
							   _acc);
'encode_stream_error_stream:error_$reason'('system-shutdown' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_system-shutdown'(_r,
						       _acc);
'encode_stream_error_stream:error_$reason'({'see-other-host',
					    _} =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_see-other-host'(_r,
						      _acc);
'encode_stream_error_stream:error_$reason'('restricted-xml' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_restricted-xml'(_r,
						      _acc);
'encode_stream_error_stream:error_$reason'('resource-constraint' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_resource-constraint'(_r,
							   _acc);
'encode_stream_error_stream:error_$reason'(reset = _r,
					   _acc) ->
    'encode_stream_error_stream:error_reset'(_r, _acc);
'encode_stream_error_stream:error_$reason'('remote-connection-failed' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_remote-connection-failed'(_r,
								_acc);
'encode_stream_error_stream:error_$reason'('policy-violation' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_policy-violation'(_r,
							_acc);
'encode_stream_error_stream:error_$reason'('not-well-formed' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_not-well-formed'(_r,
						       _acc);
'encode_stream_error_stream:error_$reason'('not-authorized' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_not-authorized'(_r,
						      _acc);
'encode_stream_error_stream:error_$reason'('invalid-xml' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_invalid-xml'(_r,
						   _acc);
'encode_stream_error_stream:error_$reason'('invalid-namespace' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_invalid-namespace'(_r,
							 _acc);
'encode_stream_error_stream:error_$reason'('invalid-id' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_invalid-id'(_r, _acc);
'encode_stream_error_stream:error_$reason'('invalid-from' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_invalid-from'(_r,
						    _acc);
'encode_stream_error_stream:error_$reason'('internal-server-error' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_internal-server-error'(_r,
							     _acc);
'encode_stream_error_stream:error_$reason'('improper-addressing' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_improper-addressing'(_r,
							   _acc);
'encode_stream_error_stream:error_$reason'('host-unknown' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_host-unknown'(_r,
						    _acc);
'encode_stream_error_stream:error_$reason'('host-gone' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_host-gone'(_r, _acc);
'encode_stream_error_stream:error_$reason'('connection-timeout' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_connection-timeout'(_r,
							  _acc);
'encode_stream_error_stream:error_$reason'(conflict =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_conflict'(_r, _acc);
'encode_stream_error_stream:error_$reason'('bad-namespace-prefix' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_bad-namespace-prefix'(_r,
							    _acc);
'encode_stream_error_stream:error_$reason'('bad-format' =
					       _r,
					   _acc) ->
    'encode_stream_error_stream:error_bad-format'(_r, _acc).

'encode_stream_error_stream:error'(undefined, _acc) ->
    _acc;
'encode_stream_error_stream:error'({stream_error,
				    Reason, Text},
				   _acc) ->
    _els =
	'encode_stream_error_stream:error_$reason'(Reason,
						   'encode_stream_error_stream:error_text'(Text,
											   [])),
    _attrs = [],
    [{xmlel, <<"stream:error">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_unsupported-version'({xmlel,
							_, _attrs, _els}) ->
    'unsupported-version'.

'encode_stream_error_stream:error_unsupported-version'(undefined,
						       _acc) ->
    _acc;
'encode_stream_error_stream:error_unsupported-version'('unsupported-version',
						       _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"unsupported-version">>, _attrs, _els}
     | _acc].

'decode_stream_error_stream:error_unsupported-stanza-type'({xmlel,
							    _, _attrs, _els}) ->
    'unsupported-stanza-type'.

'encode_stream_error_stream:error_unsupported-stanza-type'(undefined,
							   _acc) ->
    _acc;
'encode_stream_error_stream:error_unsupported-stanza-type'('unsupported-stanza-type',
							   _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"unsupported-stanza-type">>, _attrs, _els}
     | _acc].

'decode_stream_error_stream:error_unsupported-encoding'({xmlel,
							 _, _attrs, _els}) ->
    'unsupported-encoding'.

'encode_stream_error_stream:error_unsupported-encoding'(undefined,
							_acc) ->
    _acc;
'encode_stream_error_stream:error_unsupported-encoding'('unsupported-encoding',
							_acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"unsupported-encoding">>, _attrs, _els}
     | _acc].

'decode_stream_error_stream:error_undefined-condition'({xmlel,
							_, _attrs, _els}) ->
    'undefined-condition'.

'encode_stream_error_stream:error_undefined-condition'(undefined,
						       _acc) ->
    _acc;
'encode_stream_error_stream:error_undefined-condition'('undefined-condition',
						       _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"undefined-condition">>, _attrs, _els}
     | _acc].

'decode_stream_error_stream:error_system-shutdown'({xmlel,
						    _, _attrs, _els}) ->
    'system-shutdown'.

'encode_stream_error_stream:error_system-shutdown'(undefined,
						   _acc) ->
    _acc;
'encode_stream_error_stream:error_system-shutdown'('system-shutdown',
						   _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"system-shutdown">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_see-other-host'({xmlel,
						   _, _attrs, _els}) ->
    Cdata =
	'decode_stream_error_stream:error_see-other-host_els'(_els,
							      <<>>),
    {'see-other-host', Cdata}.

'decode_stream_error_stream:error_see-other-host_els'([{xmlcdata,
							_data}
						       | _els],
						      Cdata) ->
    'decode_stream_error_stream:error_see-other-host_els'(_els,
							  <<Cdata/binary,
							    _data/binary>>);
'decode_stream_error_stream:error_see-other-host_els'([_
						       | _els],
						      Cdata) ->
    'decode_stream_error_stream:error_see-other-host_els'(_els,
							  Cdata);
'decode_stream_error_stream:error_see-other-host_els'([],
						      Cdata) ->
    'decode_stream_error_stream:error_see-other-host_cdata'(Cdata).

'encode_stream_error_stream:error_see-other-host'(undefined,
						  _acc) ->
    _acc;
'encode_stream_error_stream:error_see-other-host'({'see-other-host',
						   Cdata},
						  _acc) ->
    _els =
	'encode_stream_error_stream:error_see-other-host_cdata'(Cdata,
								[]),
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"see-other-host">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_see-other-host_cdata'(<<>>) ->
    undefined;
'decode_stream_error_stream:error_see-other-host_cdata'(_val) ->
    _val.

'encode_stream_error_stream:error_see-other-host_cdata'(undefined,
							_acc) ->
    _acc;
'encode_stream_error_stream:error_see-other-host_cdata'(_val,
							_acc) ->
    [{xmlcdata, _val} | _acc].

'decode_stream_error_stream:error_restricted-xml'({xmlel,
						   _, _attrs, _els}) ->
    'restricted-xml'.

'encode_stream_error_stream:error_restricted-xml'(undefined,
						  _acc) ->
    _acc;
'encode_stream_error_stream:error_restricted-xml'('restricted-xml',
						  _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"restricted-xml">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_resource-constraint'({xmlel,
							_, _attrs, _els}) ->
    'resource-constraint'.

'encode_stream_error_stream:error_resource-constraint'(undefined,
						       _acc) ->
    _acc;
'encode_stream_error_stream:error_resource-constraint'('resource-constraint',
						       _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"resource-constraint">>, _attrs, _els}
     | _acc].

'decode_stream_error_stream:error_reset'({xmlel, _,
					  _attrs, _els}) ->
    reset.

'encode_stream_error_stream:error_reset'(undefined,
					 _acc) ->
    _acc;
'encode_stream_error_stream:error_reset'(reset, _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"reset">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_remote-connection-failed'({xmlel,
							     _, _attrs,
							     _els}) ->
    'remote-connection-failed'.

'encode_stream_error_stream:error_remote-connection-failed'(undefined,
							    _acc) ->
    _acc;
'encode_stream_error_stream:error_remote-connection-failed'('remote-connection-failed',
							    _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"remote-connection-failed">>, _attrs, _els}
     | _acc].

'decode_stream_error_stream:error_policy-violation'({xmlel,
						     _, _attrs, _els}) ->
    'policy-violation'.

'encode_stream_error_stream:error_policy-violation'(undefined,
						    _acc) ->
    _acc;
'encode_stream_error_stream:error_policy-violation'('policy-violation',
						    _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"policy-violation">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_not-well-formed'({xmlel,
						    _, _attrs, _els}) ->
    'not-well-formed'.

'encode_stream_error_stream:error_not-well-formed'(undefined,
						   _acc) ->
    _acc;
'encode_stream_error_stream:error_not-well-formed'('not-well-formed',
						   _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"not-well-formed">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_not-authorized'({xmlel,
						   _, _attrs, _els}) ->
    'not-authorized'.

'encode_stream_error_stream:error_not-authorized'(undefined,
						  _acc) ->
    _acc;
'encode_stream_error_stream:error_not-authorized'('not-authorized',
						  _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"not-authorized">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_invalid-xml'({xmlel,
						_, _attrs, _els}) ->
    'invalid-xml'.

'encode_stream_error_stream:error_invalid-xml'(undefined,
					       _acc) ->
    _acc;
'encode_stream_error_stream:error_invalid-xml'('invalid-xml',
					       _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"invalid-xml">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_invalid-namespace'({xmlel,
						      _, _attrs, _els}) ->
    'invalid-namespace'.

'encode_stream_error_stream:error_invalid-namespace'(undefined,
						     _acc) ->
    _acc;
'encode_stream_error_stream:error_invalid-namespace'('invalid-namespace',
						     _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"invalid-namespace">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_invalid-id'({xmlel, _,
					       _attrs, _els}) ->
    'invalid-id'.

'encode_stream_error_stream:error_invalid-id'(undefined,
					      _acc) ->
    _acc;
'encode_stream_error_stream:error_invalid-id'('invalid-id',
					      _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"invalid-id">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_invalid-from'({xmlel,
						 _, _attrs, _els}) ->
    'invalid-from'.

'encode_stream_error_stream:error_invalid-from'(undefined,
						_acc) ->
    _acc;
'encode_stream_error_stream:error_invalid-from'('invalid-from',
						_acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"invalid-from">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_internal-server-error'({xmlel,
							  _, _attrs, _els}) ->
    'internal-server-error'.

'encode_stream_error_stream:error_internal-server-error'(undefined,
							 _acc) ->
    _acc;
'encode_stream_error_stream:error_internal-server-error'('internal-server-error',
							 _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"internal-server-error">>, _attrs, _els}
     | _acc].

'decode_stream_error_stream:error_improper-addressing'({xmlel,
							_, _attrs, _els}) ->
    'improper-addressing'.

'encode_stream_error_stream:error_improper-addressing'(undefined,
						       _acc) ->
    _acc;
'encode_stream_error_stream:error_improper-addressing'('improper-addressing',
						       _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"improper-addressing">>, _attrs, _els}
     | _acc].

'decode_stream_error_stream:error_host-unknown'({xmlel,
						 _, _attrs, _els}) ->
    'host-unknown'.

'encode_stream_error_stream:error_host-unknown'(undefined,
						_acc) ->
    _acc;
'encode_stream_error_stream:error_host-unknown'('host-unknown',
						_acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"host-unknown">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_host-gone'({xmlel, _,
					      _attrs, _els}) ->
    'host-gone'.

'encode_stream_error_stream:error_host-gone'(undefined,
					     _acc) ->
    _acc;
'encode_stream_error_stream:error_host-gone'('host-gone',
					     _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"host-gone">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_connection-timeout'({xmlel,
						       _, _attrs, _els}) ->
    'connection-timeout'.

'encode_stream_error_stream:error_connection-timeout'(undefined,
						      _acc) ->
    _acc;
'encode_stream_error_stream:error_connection-timeout'('connection-timeout',
						      _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"connection-timeout">>, _attrs, _els}
     | _acc].

'decode_stream_error_stream:error_conflict'({xmlel, _,
					     _attrs, _els}) ->
    conflict.

'encode_stream_error_stream:error_conflict'(undefined,
					    _acc) ->
    _acc;
'encode_stream_error_stream:error_conflict'(conflict,
					    _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"conflict">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_bad-namespace-prefix'({xmlel,
							 _, _attrs, _els}) ->
    'bad-namespace-prefix'.

'encode_stream_error_stream:error_bad-namespace-prefix'(undefined,
							_acc) ->
    _acc;
'encode_stream_error_stream:error_bad-namespace-prefix'('bad-namespace-prefix',
							_acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"bad-namespace-prefix">>, _attrs, _els}
     | _acc].

'decode_stream_error_stream:error_bad-format'({xmlel, _,
					       _attrs, _els}) ->
    'bad-format'.

'encode_stream_error_stream:error_bad-format'(undefined,
					      _acc) ->
    _acc;
'encode_stream_error_stream:error_bad-format'('bad-format',
					      _acc) ->
    _els = [],
    _attrs = [{<<"xmlns">>,
	       <<"urn:ietf:params:xml:ns:xmpp-streams">>}],
    [{xmlel, <<"bad-format">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_text'({xmlel, _,
					 _attrs, _els}) ->
    Text_lang =
	'decode_stream_error_stream:error_text_attrs'(_attrs,
						      undefined),
    Cdata =
	'decode_stream_error_stream:error_text_els'(_els, <<>>),
    {Text_lang, Cdata}.

'decode_stream_error_stream:error_text_els'([{xmlcdata,
					      _data}
					     | _els],
					    Cdata) ->
    'decode_stream_error_stream:error_text_els'(_els,
						<<Cdata/binary, _data/binary>>);
'decode_stream_error_stream:error_text_els'([_ | _els],
					    Cdata) ->
    'decode_stream_error_stream:error_text_els'(_els,
						Cdata);
'decode_stream_error_stream:error_text_els'([],
					    Cdata) ->
    'decode_stream_error_stream:error_text_cdata'(Cdata).

'decode_stream_error_stream:error_text_attrs'([{<<"xml:lang">>,
						_val}
					       | _attrs],
					      _Text_lang) ->
    'decode_stream_error_stream:error_text_attrs'(_attrs,
						  _val);
'decode_stream_error_stream:error_text_attrs'([_
					       | _attrs],
					      Text_lang) ->
    'decode_stream_error_stream:error_text_attrs'(_attrs,
						  Text_lang);
'decode_stream_error_stream:error_text_attrs'([],
					      Text_lang) ->
    'decode_stream_error_stream:error_text_xml:lang'(Text_lang).

'encode_stream_error_stream:error_text'(undefined,
					_acc) ->
    _acc;
'encode_stream_error_stream:error_text'({Text_lang,
					 Cdata},
					_acc) ->
    _els =
	'encode_stream_error_stream:error_text_cdata'(Cdata,
						      []),
    _attrs =
	'encode_stream_error_stream:error_text_xml:lang'(Text_lang,
							 [{<<"xmlns">>,
							   <<"urn:ietf:params:xml:ns:xmpp-streams">>}]),
    [{xmlel, <<"text">>, _attrs, _els} | _acc].

'decode_stream_error_stream:error_text_xml:lang'(undefined) ->
    undefined;
'decode_stream_error_stream:error_text_xml:lang'(_val) ->
    _val.

'encode_stream_error_stream:error_text_xml:lang'(undefined,
						 _acc) ->
    _acc;
'encode_stream_error_stream:error_text_xml:lang'(_val,
						 _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

'decode_stream_error_stream:error_text_cdata'(<<>>) ->
    undefined;
'decode_stream_error_stream:error_text_cdata'(_val) ->
    _val.

'encode_stream_error_stream:error_text_cdata'(undefined,
					      _acc) ->
    _acc;
'encode_stream_error_stream:error_text_cdata'(_val,
					      _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_name_N({xmlel, _, _attrs, _els}) ->
    {Suffix, Prefix, Middle, Given, Family} =
	decode_vcard_name_N_els(_els, undefined, undefined,
				undefined, undefined, undefined),
    {vcard_name, Family, Given, Middle, Prefix, Suffix}.

decode_vcard_name_N_els([{xmlel, <<"SUFFIX">>, _attrs,
			  _} =
			     _el
			 | _els],
			Suffix, Prefix, Middle, Given, Family) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_name_N_els(_els,
				  decode_vcard_name_N_SUFFIX(_el), Prefix,
				  Middle, Given, Family);
      _ ->
	  decode_vcard_name_N_els(_els, Suffix, Prefix, Middle,
				  Given, Family)
    end;
decode_vcard_name_N_els([{xmlel, <<"PREFIX">>, _attrs,
			  _} =
			     _el
			 | _els],
			Suffix, Prefix, Middle, Given, Family) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_name_N_els(_els, Suffix,
				  decode_vcard_name_N_PREFIX(_el), Middle,
				  Given, Family);
      _ ->
	  decode_vcard_name_N_els(_els, Suffix, Prefix, Middle,
				  Given, Family)
    end;
decode_vcard_name_N_els([{xmlel, <<"MIDDLE">>, _attrs,
			  _} =
			     _el
			 | _els],
			Suffix, Prefix, Middle, Given, Family) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_name_N_els(_els, Suffix, Prefix,
				  decode_vcard_name_N_MIDDLE(_el), Given,
				  Family);
      _ ->
	  decode_vcard_name_N_els(_els, Suffix, Prefix, Middle,
				  Given, Family)
    end;
decode_vcard_name_N_els([{xmlel, <<"GIVEN">>, _attrs,
			  _} =
			     _el
			 | _els],
			Suffix, Prefix, Middle, Given, Family) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_name_N_els(_els, Suffix, Prefix, Middle,
				  decode_vcard_name_N_GIVEN(_el), Family);
      _ ->
	  decode_vcard_name_N_els(_els, Suffix, Prefix, Middle,
				  Given, Family)
    end;
decode_vcard_name_N_els([{xmlel, <<"FAMILY">>, _attrs,
			  _} =
			     _el
			 | _els],
			Suffix, Prefix, Middle, Given, Family) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_name_N_els(_els, Suffix, Prefix, Middle,
				  Given, decode_vcard_name_N_FAMILY(_el));
      _ ->
	  decode_vcard_name_N_els(_els, Suffix, Prefix, Middle,
				  Given, Family)
    end;
decode_vcard_name_N_els([_ | _els], Suffix, Prefix,
			Middle, Given, Family) ->
    decode_vcard_name_N_els(_els, Suffix, Prefix, Middle,
			    Given, Family);
decode_vcard_name_N_els([], Suffix, Prefix, Middle,
			Given, Family) ->
    {Suffix, Prefix, Middle, Given, Family}.

encode_vcard_name_N(undefined, _acc) -> _acc;
encode_vcard_name_N({vcard_name, Family, Given, Middle,
		     Prefix, Suffix},
		    _acc) ->
    _els = encode_vcard_name_N_FAMILY(Family,
				      encode_vcard_name_N_GIVEN(Given,
								encode_vcard_name_N_MIDDLE(Middle,
											   encode_vcard_name_N_PREFIX(Prefix,
														      encode_vcard_name_N_SUFFIX(Suffix,
																		 []))))),
    _attrs = [],
    [{xmlel, <<"N">>, _attrs, _els} | _acc].

decode_vcard_name_N_SUFFIX({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_name_N_SUFFIX_els(_els, <<>>),
    Cdata.

decode_vcard_name_N_SUFFIX_els([{xmlcdata, _data}
				| _els],
			       Cdata) ->
    decode_vcard_name_N_SUFFIX_els(_els,
				   <<Cdata/binary, _data/binary>>);
decode_vcard_name_N_SUFFIX_els([_ | _els], Cdata) ->
    decode_vcard_name_N_SUFFIX_els(_els, Cdata);
decode_vcard_name_N_SUFFIX_els([], Cdata) ->
    decode_vcard_name_N_SUFFIX_cdata(Cdata).

encode_vcard_name_N_SUFFIX(undefined, _acc) -> _acc;
encode_vcard_name_N_SUFFIX(Cdata, _acc) ->
    _els = encode_vcard_name_N_SUFFIX_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"SUFFIX">>, _attrs, _els} | _acc].

decode_vcard_name_N_SUFFIX_cdata(<<>>) -> undefined;
decode_vcard_name_N_SUFFIX_cdata(_val) -> _val.

encode_vcard_name_N_SUFFIX_cdata(undefined, _acc) ->
    _acc;
encode_vcard_name_N_SUFFIX_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_name_N_PREFIX({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_name_N_PREFIX_els(_els, <<>>),
    Cdata.

decode_vcard_name_N_PREFIX_els([{xmlcdata, _data}
				| _els],
			       Cdata) ->
    decode_vcard_name_N_PREFIX_els(_els,
				   <<Cdata/binary, _data/binary>>);
decode_vcard_name_N_PREFIX_els([_ | _els], Cdata) ->
    decode_vcard_name_N_PREFIX_els(_els, Cdata);
decode_vcard_name_N_PREFIX_els([], Cdata) ->
    decode_vcard_name_N_PREFIX_cdata(Cdata).

encode_vcard_name_N_PREFIX(undefined, _acc) -> _acc;
encode_vcard_name_N_PREFIX(Cdata, _acc) ->
    _els = encode_vcard_name_N_PREFIX_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"PREFIX">>, _attrs, _els} | _acc].

decode_vcard_name_N_PREFIX_cdata(<<>>) -> undefined;
decode_vcard_name_N_PREFIX_cdata(_val) -> _val.

encode_vcard_name_N_PREFIX_cdata(undefined, _acc) ->
    _acc;
encode_vcard_name_N_PREFIX_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_name_N_MIDDLE({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_name_N_MIDDLE_els(_els, <<>>),
    Cdata.

decode_vcard_name_N_MIDDLE_els([{xmlcdata, _data}
				| _els],
			       Cdata) ->
    decode_vcard_name_N_MIDDLE_els(_els,
				   <<Cdata/binary, _data/binary>>);
decode_vcard_name_N_MIDDLE_els([_ | _els], Cdata) ->
    decode_vcard_name_N_MIDDLE_els(_els, Cdata);
decode_vcard_name_N_MIDDLE_els([], Cdata) ->
    decode_vcard_name_N_MIDDLE_cdata(Cdata).

encode_vcard_name_N_MIDDLE(undefined, _acc) -> _acc;
encode_vcard_name_N_MIDDLE(Cdata, _acc) ->
    _els = encode_vcard_name_N_MIDDLE_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"MIDDLE">>, _attrs, _els} | _acc].

decode_vcard_name_N_MIDDLE_cdata(<<>>) -> undefined;
decode_vcard_name_N_MIDDLE_cdata(_val) -> _val.

encode_vcard_name_N_MIDDLE_cdata(undefined, _acc) ->
    _acc;
encode_vcard_name_N_MIDDLE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_name_N_GIVEN({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_name_N_GIVEN_els(_els, <<>>),
    Cdata.

decode_vcard_name_N_GIVEN_els([{xmlcdata, _data}
			       | _els],
			      Cdata) ->
    decode_vcard_name_N_GIVEN_els(_els,
				  <<Cdata/binary, _data/binary>>);
decode_vcard_name_N_GIVEN_els([_ | _els], Cdata) ->
    decode_vcard_name_N_GIVEN_els(_els, Cdata);
decode_vcard_name_N_GIVEN_els([], Cdata) ->
    decode_vcard_name_N_GIVEN_cdata(Cdata).

encode_vcard_name_N_GIVEN(undefined, _acc) -> _acc;
encode_vcard_name_N_GIVEN(Cdata, _acc) ->
    _els = encode_vcard_name_N_GIVEN_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"GIVEN">>, _attrs, _els} | _acc].

decode_vcard_name_N_GIVEN_cdata(<<>>) -> undefined;
decode_vcard_name_N_GIVEN_cdata(_val) -> _val.

encode_vcard_name_N_GIVEN_cdata(undefined, _acc) ->
    _acc;
encode_vcard_name_N_GIVEN_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_name_N_FAMILY({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_name_N_FAMILY_els(_els, <<>>),
    Cdata.

decode_vcard_name_N_FAMILY_els([{xmlcdata, _data}
				| _els],
			       Cdata) ->
    decode_vcard_name_N_FAMILY_els(_els,
				   <<Cdata/binary, _data/binary>>);
decode_vcard_name_N_FAMILY_els([_ | _els], Cdata) ->
    decode_vcard_name_N_FAMILY_els(_els, Cdata);
decode_vcard_name_N_FAMILY_els([], Cdata) ->
    decode_vcard_name_N_FAMILY_cdata(Cdata).

encode_vcard_name_N_FAMILY(undefined, _acc) -> _acc;
encode_vcard_name_N_FAMILY(Cdata, _acc) ->
    _els = encode_vcard_name_N_FAMILY_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"FAMILY">>, _attrs, _els} | _acc].

decode_vcard_name_N_FAMILY_cdata(<<>>) -> undefined;
decode_vcard_name_N_FAMILY_cdata(_val) -> _val.

encode_vcard_name_N_FAMILY_cdata(undefined, _acc) ->
    _acc;
encode_vcard_name_N_FAMILY_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_adr_ADR({xmlel, _, _attrs, _els}) ->
    {Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
     Pref, Intl, Dom, Parcel, Postal, Work, Home} =
	decode_vcard_adr_ADR_els(_els, undefined, undefined,
				 undefined, undefined, undefined, undefined,
				 undefined, false, false, false, false, false,
				 false, false),
    {vcard_adr, Home, Work, Postal, Parcel, Dom, Intl, Pref,
     Pobox, Extadd, Street, Locality, Region, Pcode, Ctry}.

decode_vcard_adr_ADR_els([{xmlel, <<"CTRY">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
			 Pref, Intl, Dom, Parcel, Postal, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_adr_ADR_els(_els,
				   decode_vcard_adr_ADR_CTRY(_el), Pcode,
				   Region, Locality, Street, Extadd, Pobox,
				   Pref, Intl, Dom, Parcel, Postal, Work, Home);
      _ ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_adr_ADR_els([{xmlel, <<"PCODE">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
			 Pref, Intl, Dom, Parcel, Postal, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_adr_ADR_els(_els, Ctry,
				   decode_vcard_adr_ADR_PCODE(_el), Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work, Home);
      _ ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_adr_ADR_els([{xmlel, <<"REGION">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
			 Pref, Intl, Dom, Parcel, Postal, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode,
				   decode_vcard_adr_ADR_REGION(_el), Locality,
				   Street, Extadd, Pobox, Pref, Intl, Dom,
				   Parcel, Postal, Work, Home);
      _ ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_adr_ADR_els([{xmlel, <<"LOCALITY">>,
			   _attrs, _} =
			      _el
			  | _els],
			 Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
			 Pref, Intl, Dom, Parcel, Postal, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   decode_vcard_adr_ADR_LOCALITY(_el), Street,
				   Extadd, Pobox, Pref, Intl, Dom, Parcel,
				   Postal, Work, Home);
      _ ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_adr_ADR_els([{xmlel, <<"STREET">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
			 Pref, Intl, Dom, Parcel, Postal, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, decode_vcard_adr_ADR_STREET(_el),
				   Extadd, Pobox, Pref, Intl, Dom, Parcel,
				   Postal, Work, Home);
      _ ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_adr_ADR_els([{xmlel, <<"EXTADD">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
			 Pref, Intl, Dom, Parcel, Postal, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street,
				   decode_vcard_adr_ADR_EXTADD(_el), Pobox,
				   Pref, Intl, Dom, Parcel, Postal, Work, Home);
      _ ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_adr_ADR_els([{xmlel, <<"POBOX">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
			 Pref, Intl, Dom, Parcel, Postal, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd,
				   decode_vcard_adr_ADR_POBOX(_el), Pref, Intl,
				   Dom, Parcel, Postal, Work, Home);
      _ ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_adr_ADR_els([{xmlel, <<"PREF">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
			 Pref, Intl, Dom, Parcel, Postal, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox,
				   decode_vcard_adr_ADR_PREF(_el), Intl, Dom,
				   Parcel, Postal, Work, Home);
      _ ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_adr_ADR_els([{xmlel, <<"INTL">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
			 Pref, Intl, Dom, Parcel, Postal, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref,
				   decode_vcard_adr_ADR_INTL(_el), Dom, Parcel,
				   Postal, Work, Home);
      _ ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_adr_ADR_els([{xmlel, <<"DOM">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
			 Pref, Intl, Dom, Parcel, Postal, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   decode_vcard_adr_ADR_DOM(_el), Parcel,
				   Postal, Work, Home);
      _ ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_adr_ADR_els([{xmlel, <<"PARCEL">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
			 Pref, Intl, Dom, Parcel, Postal, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, decode_vcard_adr_ADR_PARCEL(_el),
				   Postal, Work, Home);
      _ ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_adr_ADR_els([{xmlel, <<"POSTAL">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
			 Pref, Intl, Dom, Parcel, Postal, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel,
				   decode_vcard_adr_ADR_POSTAL(_el), Work,
				   Home);
      _ ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_adr_ADR_els([{xmlel, <<"WORK">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
			 Pref, Intl, Dom, Parcel, Postal, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal,
				   decode_vcard_adr_ADR_WORK(_el), Home);
      _ ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_adr_ADR_els([{xmlel, <<"HOME">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
			 Pref, Intl, Dom, Parcel, Postal, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work,
				   decode_vcard_adr_ADR_HOME(_el));
      _ ->
	  decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
				   Locality, Street, Extadd, Pobox, Pref, Intl,
				   Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_adr_ADR_els([_ | _els], Ctry, Pcode,
			 Region, Locality, Street, Extadd, Pobox, Pref, Intl,
			 Dom, Parcel, Postal, Work, Home) ->
    decode_vcard_adr_ADR_els(_els, Ctry, Pcode, Region,
			     Locality, Street, Extadd, Pobox, Pref, Intl, Dom,
			     Parcel, Postal, Work, Home);
decode_vcard_adr_ADR_els([], Ctry, Pcode, Region,
			 Locality, Street, Extadd, Pobox, Pref, Intl, Dom,
			 Parcel, Postal, Work, Home) ->
    {Ctry, Pcode, Region, Locality, Street, Extadd, Pobox,
     Pref, Intl, Dom, Parcel, Postal, Work, Home}.

encode_vcard_adr_ADR([], _acc) -> _acc;
encode_vcard_adr_ADR([{vcard_adr, Home, Work, Postal,
		       Parcel, Dom, Intl, Pref, Pobox, Extadd, Street,
		       Locality, Region, Pcode, Ctry}
		      | _tail],
		     _acc) ->
    _els = encode_vcard_adr_ADR_HOME(Home,
				     encode_vcard_adr_ADR_WORK(Work,
							       encode_vcard_adr_ADR_POSTAL(Postal,
											   encode_vcard_adr_ADR_PARCEL(Parcel,
														       encode_vcard_adr_ADR_DOM(Dom,
																		encode_vcard_adr_ADR_INTL(Intl,
																					  encode_vcard_adr_ADR_PREF(Pref,
																								    encode_vcard_adr_ADR_POBOX(Pobox,
																											       encode_vcard_adr_ADR_EXTADD(Extadd,
																															   encode_vcard_adr_ADR_STREET(Street,
																																		       encode_vcard_adr_ADR_LOCALITY(Locality,
																																						     encode_vcard_adr_ADR_REGION(Region,
																																										 encode_vcard_adr_ADR_PCODE(Pcode,
																																													    encode_vcard_adr_ADR_CTRY(Ctry,
																																																      [])))))))))))))),
    _attrs = [],
    encode_vcard_adr_ADR(_tail,
			 [{xmlel, <<"ADR">>, _attrs, _els} | _acc]).

decode_vcard_adr_ADR_CTRY({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_adr_ADR_CTRY_els(_els, <<>>),
    Cdata.

decode_vcard_adr_ADR_CTRY_els([{xmlcdata, _data}
			       | _els],
			      Cdata) ->
    decode_vcard_adr_ADR_CTRY_els(_els,
				  <<Cdata/binary, _data/binary>>);
decode_vcard_adr_ADR_CTRY_els([_ | _els], Cdata) ->
    decode_vcard_adr_ADR_CTRY_els(_els, Cdata);
decode_vcard_adr_ADR_CTRY_els([], Cdata) ->
    decode_vcard_adr_ADR_CTRY_cdata(Cdata).

encode_vcard_adr_ADR_CTRY(undefined, _acc) -> _acc;
encode_vcard_adr_ADR_CTRY(Cdata, _acc) ->
    _els = encode_vcard_adr_ADR_CTRY_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"CTRY">>, _attrs, _els} | _acc].

decode_vcard_adr_ADR_CTRY_cdata(<<>>) -> undefined;
decode_vcard_adr_ADR_CTRY_cdata(_val) -> _val.

encode_vcard_adr_ADR_CTRY_cdata(undefined, _acc) ->
    _acc;
encode_vcard_adr_ADR_CTRY_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_adr_ADR_PCODE({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_adr_ADR_PCODE_els(_els, <<>>),
    Cdata.

decode_vcard_adr_ADR_PCODE_els([{xmlcdata, _data}
				| _els],
			       Cdata) ->
    decode_vcard_adr_ADR_PCODE_els(_els,
				   <<Cdata/binary, _data/binary>>);
decode_vcard_adr_ADR_PCODE_els([_ | _els], Cdata) ->
    decode_vcard_adr_ADR_PCODE_els(_els, Cdata);
decode_vcard_adr_ADR_PCODE_els([], Cdata) ->
    decode_vcard_adr_ADR_PCODE_cdata(Cdata).

encode_vcard_adr_ADR_PCODE(undefined, _acc) -> _acc;
encode_vcard_adr_ADR_PCODE(Cdata, _acc) ->
    _els = encode_vcard_adr_ADR_PCODE_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"PCODE">>, _attrs, _els} | _acc].

decode_vcard_adr_ADR_PCODE_cdata(<<>>) -> undefined;
decode_vcard_adr_ADR_PCODE_cdata(_val) -> _val.

encode_vcard_adr_ADR_PCODE_cdata(undefined, _acc) ->
    _acc;
encode_vcard_adr_ADR_PCODE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_adr_ADR_REGION({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_adr_ADR_REGION_els(_els, <<>>),
    Cdata.

decode_vcard_adr_ADR_REGION_els([{xmlcdata, _data}
				 | _els],
				Cdata) ->
    decode_vcard_adr_ADR_REGION_els(_els,
				    <<Cdata/binary, _data/binary>>);
decode_vcard_adr_ADR_REGION_els([_ | _els], Cdata) ->
    decode_vcard_adr_ADR_REGION_els(_els, Cdata);
decode_vcard_adr_ADR_REGION_els([], Cdata) ->
    decode_vcard_adr_ADR_REGION_cdata(Cdata).

encode_vcard_adr_ADR_REGION(undefined, _acc) -> _acc;
encode_vcard_adr_ADR_REGION(Cdata, _acc) ->
    _els = encode_vcard_adr_ADR_REGION_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"REGION">>, _attrs, _els} | _acc].

decode_vcard_adr_ADR_REGION_cdata(<<>>) -> undefined;
decode_vcard_adr_ADR_REGION_cdata(_val) -> _val.

encode_vcard_adr_ADR_REGION_cdata(undefined, _acc) ->
    _acc;
encode_vcard_adr_ADR_REGION_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_adr_ADR_LOCALITY({xmlel, _, _attrs,
			       _els}) ->
    Cdata = decode_vcard_adr_ADR_LOCALITY_els(_els, <<>>),
    Cdata.

decode_vcard_adr_ADR_LOCALITY_els([{xmlcdata, _data}
				   | _els],
				  Cdata) ->
    decode_vcard_adr_ADR_LOCALITY_els(_els,
				      <<Cdata/binary, _data/binary>>);
decode_vcard_adr_ADR_LOCALITY_els([_ | _els], Cdata) ->
    decode_vcard_adr_ADR_LOCALITY_els(_els, Cdata);
decode_vcard_adr_ADR_LOCALITY_els([], Cdata) ->
    decode_vcard_adr_ADR_LOCALITY_cdata(Cdata).

encode_vcard_adr_ADR_LOCALITY(undefined, _acc) -> _acc;
encode_vcard_adr_ADR_LOCALITY(Cdata, _acc) ->
    _els = encode_vcard_adr_ADR_LOCALITY_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"LOCALITY">>, _attrs, _els} | _acc].

decode_vcard_adr_ADR_LOCALITY_cdata(<<>>) -> undefined;
decode_vcard_adr_ADR_LOCALITY_cdata(_val) -> _val.

encode_vcard_adr_ADR_LOCALITY_cdata(undefined, _acc) ->
    _acc;
encode_vcard_adr_ADR_LOCALITY_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_adr_ADR_STREET({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_adr_ADR_STREET_els(_els, <<>>),
    Cdata.

decode_vcard_adr_ADR_STREET_els([{xmlcdata, _data}
				 | _els],
				Cdata) ->
    decode_vcard_adr_ADR_STREET_els(_els,
				    <<Cdata/binary, _data/binary>>);
decode_vcard_adr_ADR_STREET_els([_ | _els], Cdata) ->
    decode_vcard_adr_ADR_STREET_els(_els, Cdata);
decode_vcard_adr_ADR_STREET_els([], Cdata) ->
    decode_vcard_adr_ADR_STREET_cdata(Cdata).

encode_vcard_adr_ADR_STREET(undefined, _acc) -> _acc;
encode_vcard_adr_ADR_STREET(Cdata, _acc) ->
    _els = encode_vcard_adr_ADR_STREET_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"STREET">>, _attrs, _els} | _acc].

decode_vcard_adr_ADR_STREET_cdata(<<>>) -> undefined;
decode_vcard_adr_ADR_STREET_cdata(_val) -> _val.

encode_vcard_adr_ADR_STREET_cdata(undefined, _acc) ->
    _acc;
encode_vcard_adr_ADR_STREET_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_adr_ADR_EXTADD({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_adr_ADR_EXTADD_els(_els, <<>>),
    Cdata.

decode_vcard_adr_ADR_EXTADD_els([{xmlcdata, _data}
				 | _els],
				Cdata) ->
    decode_vcard_adr_ADR_EXTADD_els(_els,
				    <<Cdata/binary, _data/binary>>);
decode_vcard_adr_ADR_EXTADD_els([_ | _els], Cdata) ->
    decode_vcard_adr_ADR_EXTADD_els(_els, Cdata);
decode_vcard_adr_ADR_EXTADD_els([], Cdata) ->
    decode_vcard_adr_ADR_EXTADD_cdata(Cdata).

encode_vcard_adr_ADR_EXTADD(undefined, _acc) -> _acc;
encode_vcard_adr_ADR_EXTADD(Cdata, _acc) ->
    _els = encode_vcard_adr_ADR_EXTADD_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"EXTADD">>, _attrs, _els} | _acc].

decode_vcard_adr_ADR_EXTADD_cdata(<<>>) -> undefined;
decode_vcard_adr_ADR_EXTADD_cdata(_val) -> _val.

encode_vcard_adr_ADR_EXTADD_cdata(undefined, _acc) ->
    _acc;
encode_vcard_adr_ADR_EXTADD_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_adr_ADR_POBOX({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_adr_ADR_POBOX_els(_els, <<>>),
    Cdata.

decode_vcard_adr_ADR_POBOX_els([{xmlcdata, _data}
				| _els],
			       Cdata) ->
    decode_vcard_adr_ADR_POBOX_els(_els,
				   <<Cdata/binary, _data/binary>>);
decode_vcard_adr_ADR_POBOX_els([_ | _els], Cdata) ->
    decode_vcard_adr_ADR_POBOX_els(_els, Cdata);
decode_vcard_adr_ADR_POBOX_els([], Cdata) ->
    decode_vcard_adr_ADR_POBOX_cdata(Cdata).

encode_vcard_adr_ADR_POBOX(undefined, _acc) -> _acc;
encode_vcard_adr_ADR_POBOX(Cdata, _acc) ->
    _els = encode_vcard_adr_ADR_POBOX_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"POBOX">>, _attrs, _els} | _acc].

decode_vcard_adr_ADR_POBOX_cdata(<<>>) -> undefined;
decode_vcard_adr_ADR_POBOX_cdata(_val) -> _val.

encode_vcard_adr_ADR_POBOX_cdata(undefined, _acc) ->
    _acc;
encode_vcard_adr_ADR_POBOX_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_adr_ADR_PREF({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_adr_ADR_PREF(false, _acc) -> _acc;
encode_vcard_adr_ADR_PREF(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"PREF">>, _attrs, _els} | _acc].

decode_vcard_adr_ADR_INTL({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_adr_ADR_INTL(false, _acc) -> _acc;
encode_vcard_adr_ADR_INTL(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"INTL">>, _attrs, _els} | _acc].

decode_vcard_adr_ADR_DOM({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_adr_ADR_DOM(false, _acc) -> _acc;
encode_vcard_adr_ADR_DOM(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"DOM">>, _attrs, _els} | _acc].

decode_vcard_adr_ADR_PARCEL({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_adr_ADR_PARCEL(false, _acc) -> _acc;
encode_vcard_adr_ADR_PARCEL(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"PARCEL">>, _attrs, _els} | _acc].

decode_vcard_adr_ADR_POSTAL({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_adr_ADR_POSTAL(false, _acc) -> _acc;
encode_vcard_adr_ADR_POSTAL(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"POSTAL">>, _attrs, _els} | _acc].

decode_vcard_adr_ADR_WORK({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_adr_ADR_WORK(false, _acc) -> _acc;
encode_vcard_adr_ADR_WORK(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"WORK">>, _attrs, _els} | _acc].

decode_vcard_adr_ADR_HOME({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_adr_ADR_HOME(false, _acc) -> _acc;
encode_vcard_adr_ADR_HOME(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"HOME">>, _attrs, _els} | _acc].

decode_vcard_label_LABEL({xmlel, _, _attrs, _els}) ->
    {Line, Pref, Intl, Dom, Parcel, Postal, Work, Home} =
	decode_vcard_label_LABEL_els(_els, [], false, false,
				     false, false, false, false, false),
    {vcard_label, Home, Work, Postal, Parcel, Dom, Intl,
     Pref, Line}.

decode_vcard_label_LABEL_els([{xmlel, <<"LINE">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Line, Pref, Intl, Dom, Parcel, Postal, Work,
			     Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_label_LABEL_els(_els,
				       [decode_vcard_label_LABEL_LINE(_el)
					| Line],
				       Pref, Intl, Dom, Parcel, Postal, Work,
				       Home);
      _ ->
	  decode_vcard_label_LABEL_els(_els, Line, Pref, Intl,
				       Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_label_LABEL_els([{xmlel, <<"PREF">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Line, Pref, Intl, Dom, Parcel, Postal, Work,
			     Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_label_LABEL_els(_els, Line,
				       decode_vcard_label_LABEL_PREF(_el), Intl,
				       Dom, Parcel, Postal, Work, Home);
      _ ->
	  decode_vcard_label_LABEL_els(_els, Line, Pref, Intl,
				       Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_label_LABEL_els([{xmlel, <<"INTL">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Line, Pref, Intl, Dom, Parcel, Postal, Work,
			     Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_label_LABEL_els(_els, Line, Pref,
				       decode_vcard_label_LABEL_INTL(_el), Dom,
				       Parcel, Postal, Work, Home);
      _ ->
	  decode_vcard_label_LABEL_els(_els, Line, Pref, Intl,
				       Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_label_LABEL_els([{xmlel, <<"DOM">>, _attrs,
			       _} =
				  _el
			      | _els],
			     Line, Pref, Intl, Dom, Parcel, Postal, Work,
			     Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_label_LABEL_els(_els, Line, Pref, Intl,
				       decode_vcard_label_LABEL_DOM(_el),
				       Parcel, Postal, Work, Home);
      _ ->
	  decode_vcard_label_LABEL_els(_els, Line, Pref, Intl,
				       Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_label_LABEL_els([{xmlel, <<"PARCEL">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Line, Pref, Intl, Dom, Parcel, Postal, Work,
			     Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_label_LABEL_els(_els, Line, Pref, Intl,
				       Dom,
				       decode_vcard_label_LABEL_PARCEL(_el),
				       Postal, Work, Home);
      _ ->
	  decode_vcard_label_LABEL_els(_els, Line, Pref, Intl,
				       Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_label_LABEL_els([{xmlel, <<"POSTAL">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Line, Pref, Intl, Dom, Parcel, Postal, Work,
			     Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_label_LABEL_els(_els, Line, Pref, Intl,
				       Dom, Parcel,
				       decode_vcard_label_LABEL_POSTAL(_el),
				       Work, Home);
      _ ->
	  decode_vcard_label_LABEL_els(_els, Line, Pref, Intl,
				       Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_label_LABEL_els([{xmlel, <<"WORK">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Line, Pref, Intl, Dom, Parcel, Postal, Work,
			     Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_label_LABEL_els(_els, Line, Pref, Intl,
				       Dom, Parcel, Postal,
				       decode_vcard_label_LABEL_WORK(_el),
				       Home);
      _ ->
	  decode_vcard_label_LABEL_els(_els, Line, Pref, Intl,
				       Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_label_LABEL_els([{xmlel, <<"HOME">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Line, Pref, Intl, Dom, Parcel, Postal, Work,
			     Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_label_LABEL_els(_els, Line, Pref, Intl,
				       Dom, Parcel, Postal, Work,
				       decode_vcard_label_LABEL_HOME(_el));
      _ ->
	  decode_vcard_label_LABEL_els(_els, Line, Pref, Intl,
				       Dom, Parcel, Postal, Work, Home)
    end;
decode_vcard_label_LABEL_els([_ | _els], Line, Pref,
			     Intl, Dom, Parcel, Postal, Work, Home) ->
    decode_vcard_label_LABEL_els(_els, Line, Pref, Intl,
				 Dom, Parcel, Postal, Work, Home);
decode_vcard_label_LABEL_els([], Line, Pref, Intl, Dom,
			     Parcel, Postal, Work, Home) ->
    {lists:reverse(Line), Pref, Intl, Dom, Parcel, Postal,
     Work, Home}.

encode_vcard_label_LABEL([], _acc) -> _acc;
encode_vcard_label_LABEL([{vcard_label, Home, Work,
			   Postal, Parcel, Dom, Intl, Pref, Line}
			  | _tail],
			 _acc) ->
    _els = encode_vcard_label_LABEL_HOME(Home,
					 encode_vcard_label_LABEL_WORK(Work,
								       encode_vcard_label_LABEL_POSTAL(Postal,
												       encode_vcard_label_LABEL_PARCEL(Parcel,
																       encode_vcard_label_LABEL_DOM(Dom,
																				    encode_vcard_label_LABEL_INTL(Intl,
																								  encode_vcard_label_LABEL_PREF(Pref,
																												encode_vcard_label_LABEL_LINE(Line,
																															      [])))))))),
    _attrs = [],
    encode_vcard_label_LABEL(_tail,
			     [{xmlel, <<"LABEL">>, _attrs, _els} | _acc]).

decode_vcard_label_LABEL_LINE({xmlel, _, _attrs,
			       _els}) ->
    Cdata = decode_vcard_label_LABEL_LINE_els(_els, <<>>),
    Cdata.

decode_vcard_label_LABEL_LINE_els([{xmlcdata, _data}
				   | _els],
				  Cdata) ->
    decode_vcard_label_LABEL_LINE_els(_els,
				      <<Cdata/binary, _data/binary>>);
decode_vcard_label_LABEL_LINE_els([_ | _els], Cdata) ->
    decode_vcard_label_LABEL_LINE_els(_els, Cdata);
decode_vcard_label_LABEL_LINE_els([], Cdata) ->
    decode_vcard_label_LABEL_LINE_cdata(Cdata).

encode_vcard_label_LABEL_LINE([], _acc) -> _acc;
encode_vcard_label_LABEL_LINE([Cdata | _tail], _acc) ->
    _els = encode_vcard_label_LABEL_LINE_cdata(Cdata, []),
    _attrs = [],
    encode_vcard_label_LABEL_LINE(_tail,
				  [{xmlel, <<"LINE">>, _attrs, _els} | _acc]).

decode_vcard_label_LABEL_LINE_cdata(<<>>) -> undefined;
decode_vcard_label_LABEL_LINE_cdata(_val) -> _val.

encode_vcard_label_LABEL_LINE_cdata(undefined, _acc) ->
    _acc;
encode_vcard_label_LABEL_LINE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_label_LABEL_PREF({xmlel, _, _attrs,
			       _els}) ->
    true.

encode_vcard_label_LABEL_PREF(false, _acc) -> _acc;
encode_vcard_label_LABEL_PREF(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"PREF">>, _attrs, _els} | _acc].

decode_vcard_label_LABEL_INTL({xmlel, _, _attrs,
			       _els}) ->
    true.

encode_vcard_label_LABEL_INTL(false, _acc) -> _acc;
encode_vcard_label_LABEL_INTL(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"INTL">>, _attrs, _els} | _acc].

decode_vcard_label_LABEL_DOM({xmlel, _, _attrs,
			      _els}) ->
    true.

encode_vcard_label_LABEL_DOM(false, _acc) -> _acc;
encode_vcard_label_LABEL_DOM(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"DOM">>, _attrs, _els} | _acc].

decode_vcard_label_LABEL_PARCEL({xmlel, _, _attrs,
				 _els}) ->
    true.

encode_vcard_label_LABEL_PARCEL(false, _acc) -> _acc;
encode_vcard_label_LABEL_PARCEL(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"PARCEL">>, _attrs, _els} | _acc].

decode_vcard_label_LABEL_POSTAL({xmlel, _, _attrs,
				 _els}) ->
    true.

encode_vcard_label_LABEL_POSTAL(false, _acc) -> _acc;
encode_vcard_label_LABEL_POSTAL(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"POSTAL">>, _attrs, _els} | _acc].

decode_vcard_label_LABEL_WORK({xmlel, _, _attrs,
			       _els}) ->
    true.

encode_vcard_label_LABEL_WORK(false, _acc) -> _acc;
encode_vcard_label_LABEL_WORK(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"WORK">>, _attrs, _els} | _acc].

decode_vcard_label_LABEL_HOME({xmlel, _, _attrs,
			       _els}) ->
    true.

encode_vcard_label_LABEL_HOME(false, _acc) -> _acc;
encode_vcard_label_LABEL_HOME(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"HOME">>, _attrs, _els} | _acc].

decode_vcard_tel_TEL({xmlel, _, _attrs, _els}) ->
    {Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
     Pager, Fax, Voice, Work, Home} =
	decode_vcard_tel_TEL_els(_els, [], false, false, false,
				 false, false, false, false, false, false,
				 false, false, false, false),
    {vcard_tel, Home, Work, Voice, Fax, Pager, Msg, Cell,
     Video, Bbs, Modem, Isdn, Pcs, Pref, Number}.

decode_vcard_tel_TEL_els([{xmlel, <<"NUMBER">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
			 Pager, Fax, Voice, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_tel_TEL_els(_els,
				   [decode_vcard_tel_TEL_NUMBER(_el) | Number],
				   Pref, Pcs, Isdn, Modem, Bbs, Video, Cell,
				   Msg, Pager, Fax, Voice, Work, Home);
      _ ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, Home)
    end;
decode_vcard_tel_TEL_els([{xmlel, <<"PREF">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
			 Pager, Fax, Voice, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_tel_TEL_els(_els, Number,
				   decode_vcard_tel_TEL_PREF(_el), Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, Home);
      _ ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, Home)
    end;
decode_vcard_tel_TEL_els([{xmlel, <<"PCS">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
			 Pager, Fax, Voice, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref,
				   decode_vcard_tel_TEL_PCS(_el), Isdn, Modem,
				   Bbs, Video, Cell, Msg, Pager, Fax, Voice,
				   Work, Home);
      _ ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, Home)
    end;
decode_vcard_tel_TEL_els([{xmlel, <<"ISDN">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
			 Pager, Fax, Voice, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs,
				   decode_vcard_tel_TEL_ISDN(_el), Modem, Bbs,
				   Video, Cell, Msg, Pager, Fax, Voice, Work,
				   Home);
      _ ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, Home)
    end;
decode_vcard_tel_TEL_els([{xmlel, <<"MODEM">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
			 Pager, Fax, Voice, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   decode_vcard_tel_TEL_MODEM(_el), Bbs, Video,
				   Cell, Msg, Pager, Fax, Voice, Work, Home);
      _ ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, Home)
    end;
decode_vcard_tel_TEL_els([{xmlel, <<"BBS">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
			 Pager, Fax, Voice, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, decode_vcard_tel_TEL_BBS(_el), Video,
				   Cell, Msg, Pager, Fax, Voice, Work, Home);
      _ ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, Home)
    end;
decode_vcard_tel_TEL_els([{xmlel, <<"VIDEO">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
			 Pager, Fax, Voice, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, decode_vcard_tel_TEL_VIDEO(_el),
				   Cell, Msg, Pager, Fax, Voice, Work, Home);
      _ ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, Home)
    end;
decode_vcard_tel_TEL_els([{xmlel, <<"CELL">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
			 Pager, Fax, Voice, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video,
				   decode_vcard_tel_TEL_CELL(_el), Msg, Pager,
				   Fax, Voice, Work, Home);
      _ ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, Home)
    end;
decode_vcard_tel_TEL_els([{xmlel, <<"MSG">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
			 Pager, Fax, Voice, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell,
				   decode_vcard_tel_TEL_MSG(_el), Pager, Fax,
				   Voice, Work, Home);
      _ ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, Home)
    end;
decode_vcard_tel_TEL_els([{xmlel, <<"PAGER">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
			 Pager, Fax, Voice, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg,
				   decode_vcard_tel_TEL_PAGER(_el), Fax, Voice,
				   Work, Home);
      _ ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, Home)
    end;
decode_vcard_tel_TEL_els([{xmlel, <<"FAX">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
			 Pager, Fax, Voice, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager,
				   decode_vcard_tel_TEL_FAX(_el), Voice, Work,
				   Home);
      _ ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, Home)
    end;
decode_vcard_tel_TEL_els([{xmlel, <<"VOICE">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
			 Pager, Fax, Voice, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   decode_vcard_tel_TEL_VOICE(_el), Work, Home);
      _ ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, Home)
    end;
decode_vcard_tel_TEL_els([{xmlel, <<"WORK">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
			 Pager, Fax, Voice, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, decode_vcard_tel_TEL_WORK(_el), Home);
      _ ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, Home)
    end;
decode_vcard_tel_TEL_els([{xmlel, <<"HOME">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
			 Pager, Fax, Voice, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, decode_vcard_tel_TEL_HOME(_el));
      _ ->
	  decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
				   Modem, Bbs, Video, Cell, Msg, Pager, Fax,
				   Voice, Work, Home)
    end;
decode_vcard_tel_TEL_els([_ | _els], Number, Pref, Pcs,
			 Isdn, Modem, Bbs, Video, Cell, Msg, Pager, Fax, Voice,
			 Work, Home) ->
    decode_vcard_tel_TEL_els(_els, Number, Pref, Pcs, Isdn,
			     Modem, Bbs, Video, Cell, Msg, Pager, Fax, Voice,
			     Work, Home);
decode_vcard_tel_TEL_els([], [Number], Pref, Pcs, Isdn,
			 Modem, Bbs, Video, Cell, Msg, Pager, Fax, Voice, Work,
			 Home) ->
    {Number, Pref, Pcs, Isdn, Modem, Bbs, Video, Cell, Msg,
     Pager, Fax, Voice, Work, Home}.

encode_vcard_tel_TEL([], _acc) -> _acc;
encode_vcard_tel_TEL([{vcard_tel, Home, Work, Voice,
		       Fax, Pager, Msg, Cell, Video, Bbs, Modem, Isdn, Pcs,
		       Pref, Number}
		      | _tail],
		     _acc) ->
    _els = encode_vcard_tel_TEL_HOME(Home,
				     encode_vcard_tel_TEL_WORK(Work,
							       encode_vcard_tel_TEL_VOICE(Voice,
											  encode_vcard_tel_TEL_FAX(Fax,
														   encode_vcard_tel_TEL_PAGER(Pager,
																	      encode_vcard_tel_TEL_MSG(Msg,
																				       encode_vcard_tel_TEL_CELL(Cell,
																								 encode_vcard_tel_TEL_VIDEO(Video,
																											    encode_vcard_tel_TEL_BBS(Bbs,
																														     encode_vcard_tel_TEL_MODEM(Modem,
																																		encode_vcard_tel_TEL_ISDN(Isdn,
																																					  encode_vcard_tel_TEL_PCS(Pcs,
																																								   encode_vcard_tel_TEL_PREF(Pref,
																																											     encode_vcard_tel_TEL_NUMBER(Number,
																																															 [])))))))))))))),
    _attrs = [],
    encode_vcard_tel_TEL(_tail,
			 [{xmlel, <<"TEL">>, _attrs, _els} | _acc]).

decode_vcard_tel_TEL_NUMBER({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_tel_TEL_NUMBER_els(_els, <<>>),
    Cdata.

decode_vcard_tel_TEL_NUMBER_els([{xmlcdata, _data}
				 | _els],
				Cdata) ->
    decode_vcard_tel_TEL_NUMBER_els(_els,
				    <<Cdata/binary, _data/binary>>);
decode_vcard_tel_TEL_NUMBER_els([_ | _els], Cdata) ->
    decode_vcard_tel_TEL_NUMBER_els(_els, Cdata);
decode_vcard_tel_TEL_NUMBER_els([], Cdata) ->
    decode_vcard_tel_TEL_NUMBER_cdata(Cdata).

encode_vcard_tel_TEL_NUMBER(Cdata, _acc) ->
    _els = encode_vcard_tel_TEL_NUMBER_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"NUMBER">>, _attrs, _els} | _acc].

decode_vcard_tel_TEL_NUMBER_cdata(<<>>) -> undefined;
decode_vcard_tel_TEL_NUMBER_cdata(_val) -> _val.

encode_vcard_tel_TEL_NUMBER_cdata(undefined, _acc) ->
    _acc;
encode_vcard_tel_TEL_NUMBER_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_tel_TEL_PREF({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_tel_TEL_PREF(false, _acc) -> _acc;
encode_vcard_tel_TEL_PREF(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"PREF">>, _attrs, _els} | _acc].

decode_vcard_tel_TEL_PCS({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_tel_TEL_PCS(false, _acc) -> _acc;
encode_vcard_tel_TEL_PCS(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"PCS">>, _attrs, _els} | _acc].

decode_vcard_tel_TEL_ISDN({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_tel_TEL_ISDN(false, _acc) -> _acc;
encode_vcard_tel_TEL_ISDN(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"ISDN">>, _attrs, _els} | _acc].

decode_vcard_tel_TEL_MODEM({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_tel_TEL_MODEM(false, _acc) -> _acc;
encode_vcard_tel_TEL_MODEM(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"MODEM">>, _attrs, _els} | _acc].

decode_vcard_tel_TEL_BBS({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_tel_TEL_BBS(false, _acc) -> _acc;
encode_vcard_tel_TEL_BBS(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"BBS">>, _attrs, _els} | _acc].

decode_vcard_tel_TEL_VIDEO({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_tel_TEL_VIDEO(false, _acc) -> _acc;
encode_vcard_tel_TEL_VIDEO(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"VIDEO">>, _attrs, _els} | _acc].

decode_vcard_tel_TEL_CELL({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_tel_TEL_CELL(false, _acc) -> _acc;
encode_vcard_tel_TEL_CELL(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"CELL">>, _attrs, _els} | _acc].

decode_vcard_tel_TEL_MSG({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_tel_TEL_MSG(false, _acc) -> _acc;
encode_vcard_tel_TEL_MSG(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"MSG">>, _attrs, _els} | _acc].

decode_vcard_tel_TEL_PAGER({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_tel_TEL_PAGER(false, _acc) -> _acc;
encode_vcard_tel_TEL_PAGER(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"PAGER">>, _attrs, _els} | _acc].

decode_vcard_tel_TEL_FAX({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_tel_TEL_FAX(false, _acc) -> _acc;
encode_vcard_tel_TEL_FAX(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"FAX">>, _attrs, _els} | _acc].

decode_vcard_tel_TEL_VOICE({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_tel_TEL_VOICE(false, _acc) -> _acc;
encode_vcard_tel_TEL_VOICE(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"VOICE">>, _attrs, _els} | _acc].

decode_vcard_tel_TEL_WORK({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_tel_TEL_WORK(false, _acc) -> _acc;
encode_vcard_tel_TEL_WORK(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"WORK">>, _attrs, _els} | _acc].

decode_vcard_tel_TEL_HOME({xmlel, _, _attrs, _els}) ->
    true.

encode_vcard_tel_TEL_HOME(false, _acc) -> _acc;
encode_vcard_tel_TEL_HOME(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"HOME">>, _attrs, _els} | _acc].

decode_vcard_email_EMAIL({xmlel, _, _attrs, _els}) ->
    {Userid, X400, Pref, Internet, Work, Home} =
	decode_vcard_email_EMAIL_els(_els, [], false, false,
				     false, false, false),
    {vcard_email, Home, Work, Internet, Pref, X400, Userid}.

decode_vcard_email_EMAIL_els([{xmlel, <<"USERID">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Userid, X400, Pref, Internet, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_email_EMAIL_els(_els,
				       [decode_vcard_email_EMAIL_USERID(_el)
					| Userid],
				       X400, Pref, Internet, Work, Home);
      _ ->
	  decode_vcard_email_EMAIL_els(_els, Userid, X400, Pref,
				       Internet, Work, Home)
    end;
decode_vcard_email_EMAIL_els([{xmlel, <<"X400">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Userid, X400, Pref, Internet, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_email_EMAIL_els(_els, Userid,
				       decode_vcard_email_EMAIL_X400(_el), Pref,
				       Internet, Work, Home);
      _ ->
	  decode_vcard_email_EMAIL_els(_els, Userid, X400, Pref,
				       Internet, Work, Home)
    end;
decode_vcard_email_EMAIL_els([{xmlel, <<"PREF">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Userid, X400, Pref, Internet, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_email_EMAIL_els(_els, Userid, X400,
				       decode_vcard_email_EMAIL_PREF(_el),
				       Internet, Work, Home);
      _ ->
	  decode_vcard_email_EMAIL_els(_els, Userid, X400, Pref,
				       Internet, Work, Home)
    end;
decode_vcard_email_EMAIL_els([{xmlel, <<"INTERNET">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Userid, X400, Pref, Internet, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_email_EMAIL_els(_els, Userid, X400, Pref,
				       decode_vcard_email_EMAIL_INTERNET(_el),
				       Work, Home);
      _ ->
	  decode_vcard_email_EMAIL_els(_els, Userid, X400, Pref,
				       Internet, Work, Home)
    end;
decode_vcard_email_EMAIL_els([{xmlel, <<"WORK">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Userid, X400, Pref, Internet, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_email_EMAIL_els(_els, Userid, X400, Pref,
				       Internet,
				       decode_vcard_email_EMAIL_WORK(_el),
				       Home);
      _ ->
	  decode_vcard_email_EMAIL_els(_els, Userid, X400, Pref,
				       Internet, Work, Home)
    end;
decode_vcard_email_EMAIL_els([{xmlel, <<"HOME">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Userid, X400, Pref, Internet, Work, Home) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_email_EMAIL_els(_els, Userid, X400, Pref,
				       Internet, Work,
				       decode_vcard_email_EMAIL_HOME(_el));
      _ ->
	  decode_vcard_email_EMAIL_els(_els, Userid, X400, Pref,
				       Internet, Work, Home)
    end;
decode_vcard_email_EMAIL_els([_ | _els], Userid, X400,
			     Pref, Internet, Work, Home) ->
    decode_vcard_email_EMAIL_els(_els, Userid, X400, Pref,
				 Internet, Work, Home);
decode_vcard_email_EMAIL_els([], [Userid], X400, Pref,
			     Internet, Work, Home) ->
    {Userid, X400, Pref, Internet, Work, Home}.

encode_vcard_email_EMAIL([], _acc) -> _acc;
encode_vcard_email_EMAIL([{vcard_email, Home, Work,
			   Internet, Pref, X400, Userid}
			  | _tail],
			 _acc) ->
    _els = encode_vcard_email_EMAIL_HOME(Home,
					 encode_vcard_email_EMAIL_WORK(Work,
								       encode_vcard_email_EMAIL_INTERNET(Internet,
													 encode_vcard_email_EMAIL_PREF(Pref,
																       encode_vcard_email_EMAIL_X400(X400,
																				     encode_vcard_email_EMAIL_USERID(Userid,
																								     [])))))),
    _attrs = [],
    encode_vcard_email_EMAIL(_tail,
			     [{xmlel, <<"EMAIL">>, _attrs, _els} | _acc]).

decode_vcard_email_EMAIL_USERID({xmlel, _, _attrs,
				 _els}) ->
    Cdata = decode_vcard_email_EMAIL_USERID_els(_els, <<>>),
    Cdata.

decode_vcard_email_EMAIL_USERID_els([{xmlcdata, _data}
				     | _els],
				    Cdata) ->
    decode_vcard_email_EMAIL_USERID_els(_els,
					<<Cdata/binary, _data/binary>>);
decode_vcard_email_EMAIL_USERID_els([_ | _els],
				    Cdata) ->
    decode_vcard_email_EMAIL_USERID_els(_els, Cdata);
decode_vcard_email_EMAIL_USERID_els([], Cdata) ->
    decode_vcard_email_EMAIL_USERID_cdata(Cdata).

encode_vcard_email_EMAIL_USERID(Cdata, _acc) ->
    _els = encode_vcard_email_EMAIL_USERID_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"USERID">>, _attrs, _els} | _acc].

decode_vcard_email_EMAIL_USERID_cdata(<<>>) ->
    undefined;
decode_vcard_email_EMAIL_USERID_cdata(_val) -> _val.

encode_vcard_email_EMAIL_USERID_cdata(undefined,
				      _acc) ->
    _acc;
encode_vcard_email_EMAIL_USERID_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_email_EMAIL_X400({xmlel, _, _attrs,
			       _els}) ->
    true.

encode_vcard_email_EMAIL_X400(false, _acc) -> _acc;
encode_vcard_email_EMAIL_X400(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"X400">>, _attrs, _els} | _acc].

decode_vcard_email_EMAIL_PREF({xmlel, _, _attrs,
			       _els}) ->
    true.

encode_vcard_email_EMAIL_PREF(false, _acc) -> _acc;
encode_vcard_email_EMAIL_PREF(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"PREF">>, _attrs, _els} | _acc].

decode_vcard_email_EMAIL_INTERNET({xmlel, _, _attrs,
				   _els}) ->
    true.

encode_vcard_email_EMAIL_INTERNET(false, _acc) -> _acc;
encode_vcard_email_EMAIL_INTERNET(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"INTERNET">>, _attrs, _els} | _acc].

decode_vcard_email_EMAIL_WORK({xmlel, _, _attrs,
			       _els}) ->
    true.

encode_vcard_email_EMAIL_WORK(false, _acc) -> _acc;
encode_vcard_email_EMAIL_WORK(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"WORK">>, _attrs, _els} | _acc].

decode_vcard_email_EMAIL_HOME({xmlel, _, _attrs,
			       _els}) ->
    true.

encode_vcard_email_EMAIL_HOME(false, _acc) -> _acc;
encode_vcard_email_EMAIL_HOME(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"HOME">>, _attrs, _els} | _acc].

decode_vcard_geo_GEO({xmlel, _, _attrs, _els}) ->
    {Lon, Lat} = decode_vcard_geo_GEO_els(_els, [], []),
    {vcard_geo, Lat, Lon}.

decode_vcard_geo_GEO_els([{xmlel, <<"LON">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Lon, Lat) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_geo_GEO_els(_els,
				   [decode_vcard_geo_GEO_LON(_el) | Lon], Lat);
      _ -> decode_vcard_geo_GEO_els(_els, Lon, Lat)
    end;
decode_vcard_geo_GEO_els([{xmlel, <<"LAT">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Lon, Lat) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_geo_GEO_els(_els, Lon,
				   [decode_vcard_geo_GEO_LAT(_el) | Lat]);
      _ -> decode_vcard_geo_GEO_els(_els, Lon, Lat)
    end;
decode_vcard_geo_GEO_els([_ | _els], Lon, Lat) ->
    decode_vcard_geo_GEO_els(_els, Lon, Lat);
decode_vcard_geo_GEO_els([], [Lon], [Lat]) ->
    {Lon, Lat}.

encode_vcard_geo_GEO(undefined, _acc) -> _acc;
encode_vcard_geo_GEO({vcard_geo, Lat, Lon}, _acc) ->
    _els = encode_vcard_geo_GEO_LAT(Lat,
				    encode_vcard_geo_GEO_LON(Lon, [])),
    _attrs = [],
    [{xmlel, <<"GEO">>, _attrs, _els} | _acc].

decode_vcard_geo_GEO_LON({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_geo_GEO_LON_els(_els, <<>>), Cdata.

decode_vcard_geo_GEO_LON_els([{xmlcdata, _data} | _els],
			     Cdata) ->
    decode_vcard_geo_GEO_LON_els(_els,
				 <<Cdata/binary, _data/binary>>);
decode_vcard_geo_GEO_LON_els([_ | _els], Cdata) ->
    decode_vcard_geo_GEO_LON_els(_els, Cdata);
decode_vcard_geo_GEO_LON_els([], Cdata) ->
    decode_vcard_geo_GEO_LON_cdata(Cdata).

encode_vcard_geo_GEO_LON(Cdata, _acc) ->
    _els = encode_vcard_geo_GEO_LON_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"LON">>, _attrs, _els} | _acc].

decode_vcard_geo_GEO_LON_cdata(<<>>) -> undefined;
decode_vcard_geo_GEO_LON_cdata(_val) -> _val.

encode_vcard_geo_GEO_LON_cdata(undefined, _acc) -> _acc;
encode_vcard_geo_GEO_LON_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_geo_GEO_LAT({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_geo_GEO_LAT_els(_els, <<>>), Cdata.

decode_vcard_geo_GEO_LAT_els([{xmlcdata, _data} | _els],
			     Cdata) ->
    decode_vcard_geo_GEO_LAT_els(_els,
				 <<Cdata/binary, _data/binary>>);
decode_vcard_geo_GEO_LAT_els([_ | _els], Cdata) ->
    decode_vcard_geo_GEO_LAT_els(_els, Cdata);
decode_vcard_geo_GEO_LAT_els([], Cdata) ->
    decode_vcard_geo_GEO_LAT_cdata(Cdata).

encode_vcard_geo_GEO_LAT(Cdata, _acc) ->
    _els = encode_vcard_geo_GEO_LAT_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"LAT">>, _attrs, _els} | _acc].

decode_vcard_geo_GEO_LAT_cdata(<<>>) -> undefined;
decode_vcard_geo_GEO_LAT_cdata(_val) -> _val.

encode_vcard_geo_GEO_LAT_cdata(undefined, _acc) -> _acc;
encode_vcard_geo_GEO_LAT_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_type_TYPE({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_type_TYPE_els(_els, <<>>), Cdata.

decode_vcard_type_TYPE_els([{xmlcdata, _data} | _els],
			   Cdata) ->
    decode_vcard_type_TYPE_els(_els,
			       <<Cdata/binary, _data/binary>>);
decode_vcard_type_TYPE_els([_ | _els], Cdata) ->
    decode_vcard_type_TYPE_els(_els, Cdata);
decode_vcard_type_TYPE_els([], Cdata) ->
    decode_vcard_type_TYPE_cdata(Cdata).

encode_vcard_type_TYPE(undefined, _acc) -> _acc;
encode_vcard_type_TYPE(Cdata, _acc) ->
    _els = encode_vcard_type_TYPE_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"TYPE">>, _attrs, _els} | _acc].

decode_vcard_type_TYPE_cdata(<<>>) -> undefined;
decode_vcard_type_TYPE_cdata(_val) -> _val.

encode_vcard_type_TYPE_cdata(undefined, _acc) -> _acc;
encode_vcard_type_TYPE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_binval_BINVAL({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_binval_BINVAL_els(_els, <<>>),
    Cdata.

decode_vcard_binval_BINVAL_els([{xmlcdata, _data}
				| _els],
			       Cdata) ->
    decode_vcard_binval_BINVAL_els(_els,
				   <<Cdata/binary, _data/binary>>);
decode_vcard_binval_BINVAL_els([_ | _els], Cdata) ->
    decode_vcard_binval_BINVAL_els(_els, Cdata);
decode_vcard_binval_BINVAL_els([], Cdata) ->
    decode_vcard_binval_BINVAL_cdata(Cdata).

encode_vcard_binval_BINVAL(undefined, _acc) -> _acc;
encode_vcard_binval_BINVAL(Cdata, _acc) ->
    _els = encode_vcard_binval_BINVAL_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"BINVAL">>, _attrs, _els} | _acc].

decode_vcard_binval_BINVAL_cdata(<<>>) -> undefined;
decode_vcard_binval_BINVAL_cdata(_val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_cdata_value, <<>>, <<"BINVAL">>,
			<<>>});
      _res -> _res
    end.

encode_vcard_binval_BINVAL_cdata(undefined, _acc) ->
    _acc;
encode_vcard_binval_BINVAL_cdata(_val, _acc) ->
    [{xmlcdata, base64:encode(_val)} | _acc].

decode_vcard_extval_EXTVAL({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_extval_EXTVAL_els(_els, <<>>),
    Cdata.

decode_vcard_extval_EXTVAL_els([{xmlcdata, _data}
				| _els],
			       Cdata) ->
    decode_vcard_extval_EXTVAL_els(_els,
				   <<Cdata/binary, _data/binary>>);
decode_vcard_extval_EXTVAL_els([_ | _els], Cdata) ->
    decode_vcard_extval_EXTVAL_els(_els, Cdata);
decode_vcard_extval_EXTVAL_els([], Cdata) ->
    decode_vcard_extval_EXTVAL_cdata(Cdata).

encode_vcard_extval_EXTVAL(undefined, _acc) -> _acc;
encode_vcard_extval_EXTVAL(Cdata, _acc) ->
    _els = encode_vcard_extval_EXTVAL_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"EXTVAL">>, _attrs, _els} | _acc].

decode_vcard_extval_EXTVAL_cdata(<<>>) -> undefined;
decode_vcard_extval_EXTVAL_cdata(_val) -> _val.

encode_vcard_extval_EXTVAL_cdata(undefined, _acc) ->
    _acc;
encode_vcard_extval_EXTVAL_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_logo_LOGO({xmlel, _, _attrs, _els}) ->
    {Extval, Binval, Type} =
	decode_vcard_logo_LOGO_els(_els, undefined, undefined,
				   undefined),
    {vcard_logo, Type, Binval, Extval}.

decode_vcard_logo_LOGO_els([{xmlel, <<"EXTVAL">>,
			     _attrs, _} =
				_el
			    | _els],
			   Extval, Binval, Type) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_logo_LOGO_els(_els,
				     decode_vcard_extval_EXTVAL(_el), Binval,
				     Type);
      _ ->
	  decode_vcard_logo_LOGO_els(_els, Extval, Binval, Type)
    end;
decode_vcard_logo_LOGO_els([{xmlel, <<"BINVAL">>,
			     _attrs, _} =
				_el
			    | _els],
			   Extval, Binval, Type) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_logo_LOGO_els(_els, Extval,
				     decode_vcard_binval_BINVAL(_el), Type);
      _ ->
	  decode_vcard_logo_LOGO_els(_els, Extval, Binval, Type)
    end;
decode_vcard_logo_LOGO_els([{xmlel, <<"TYPE">>, _attrs,
			     _} =
				_el
			    | _els],
			   Extval, Binval, Type) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_logo_LOGO_els(_els, Extval, Binval,
				     decode_vcard_type_TYPE(_el));
      _ ->
	  decode_vcard_logo_LOGO_els(_els, Extval, Binval, Type)
    end;
decode_vcard_logo_LOGO_els([_ | _els], Extval, Binval,
			   Type) ->
    decode_vcard_logo_LOGO_els(_els, Extval, Binval, Type);
decode_vcard_logo_LOGO_els([], Extval, Binval, Type) ->
    {Extval, Binval, Type}.

encode_vcard_logo_LOGO(undefined, _acc) -> _acc;
encode_vcard_logo_LOGO({vcard_logo, Type, Binval,
			Extval},
		       _acc) ->
    _els = encode_vcard_type_TYPE(Type,
				  encode_vcard_binval_BINVAL(Binval,
							     encode_vcard_extval_EXTVAL(Extval,
											[]))),
    _attrs = [],
    [{xmlel, <<"LOGO">>, _attrs, _els} | _acc].

decode_vcard_photo_PHOTO({xmlel, _, _attrs, _els}) ->
    {Extval, Binval, Type} =
	decode_vcard_photo_PHOTO_els(_els, undefined, undefined,
				     undefined),
    {vcard_photo, Type, Binval, Extval}.

decode_vcard_photo_PHOTO_els([{xmlel, <<"EXTVAL">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Extval, Binval, Type) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_photo_PHOTO_els(_els,
				       decode_vcard_extval_EXTVAL(_el), Binval,
				       Type);
      _ ->
	  decode_vcard_photo_PHOTO_els(_els, Extval, Binval, Type)
    end;
decode_vcard_photo_PHOTO_els([{xmlel, <<"BINVAL">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Extval, Binval, Type) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_photo_PHOTO_els(_els, Extval,
				       decode_vcard_binval_BINVAL(_el), Type);
      _ ->
	  decode_vcard_photo_PHOTO_els(_els, Extval, Binval, Type)
    end;
decode_vcard_photo_PHOTO_els([{xmlel, <<"TYPE">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Extval, Binval, Type) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_photo_PHOTO_els(_els, Extval, Binval,
				       decode_vcard_type_TYPE(_el));
      _ ->
	  decode_vcard_photo_PHOTO_els(_els, Extval, Binval, Type)
    end;
decode_vcard_photo_PHOTO_els([_ | _els], Extval, Binval,
			     Type) ->
    decode_vcard_photo_PHOTO_els(_els, Extval, Binval,
				 Type);
decode_vcard_photo_PHOTO_els([], Extval, Binval,
			     Type) ->
    {Extval, Binval, Type}.

encode_vcard_photo_PHOTO(undefined, _acc) -> _acc;
encode_vcard_photo_PHOTO({vcard_photo, Type, Binval,
			  Extval},
			 _acc) ->
    _els = encode_vcard_type_TYPE(Type,
				  encode_vcard_binval_BINVAL(Binval,
							     encode_vcard_extval_EXTVAL(Extval,
											[]))),
    _attrs = [],
    [{xmlel, <<"PHOTO">>, _attrs, _els} | _acc].

decode_vcard_org_ORG({xmlel, _, _attrs, _els}) ->
    {Units, Name} = decode_vcard_org_ORG_els(_els, [], []),
    {vcard_org, Name, Units}.

decode_vcard_org_ORG_els([{xmlel, <<"ORGUNIT">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Units, Name) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_org_ORG_els(_els,
				   [decode_vcard_org_ORG_ORGUNIT(_el) | Units],
				   Name);
      _ -> decode_vcard_org_ORG_els(_els, Units, Name)
    end;
decode_vcard_org_ORG_els([{xmlel, <<"ORGNAME">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Units, Name) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_org_ORG_els(_els, Units,
				   [decode_vcard_org_ORG_ORGNAME(_el) | Name]);
      _ -> decode_vcard_org_ORG_els(_els, Units, Name)
    end;
decode_vcard_org_ORG_els([_ | _els], Units, Name) ->
    decode_vcard_org_ORG_els(_els, Units, Name);
decode_vcard_org_ORG_els([], Units, [Name]) ->
    {lists:reverse(Units), Name}.

encode_vcard_org_ORG(undefined, _acc) -> _acc;
encode_vcard_org_ORG({vcard_org, Name, Units}, _acc) ->
    _els = encode_vcard_org_ORG_ORGNAME(Name,
					encode_vcard_org_ORG_ORGUNIT(Units,
								     [])),
    _attrs = [],
    [{xmlel, <<"ORG">>, _attrs, _els} | _acc].

decode_vcard_org_ORG_ORGUNIT({xmlel, _, _attrs,
			      _els}) ->
    Cdata = decode_vcard_org_ORG_ORGUNIT_els(_els, <<>>),
    Cdata.

decode_vcard_org_ORG_ORGUNIT_els([{xmlcdata, _data}
				  | _els],
				 Cdata) ->
    decode_vcard_org_ORG_ORGUNIT_els(_els,
				     <<Cdata/binary, _data/binary>>);
decode_vcard_org_ORG_ORGUNIT_els([_ | _els], Cdata) ->
    decode_vcard_org_ORG_ORGUNIT_els(_els, Cdata);
decode_vcard_org_ORG_ORGUNIT_els([], Cdata) ->
    decode_vcard_org_ORG_ORGUNIT_cdata(Cdata).

encode_vcard_org_ORG_ORGUNIT([], _acc) -> _acc;
encode_vcard_org_ORG_ORGUNIT([Cdata | _tail], _acc) ->
    _els = encode_vcard_org_ORG_ORGUNIT_cdata(Cdata, []),
    _attrs = [],
    encode_vcard_org_ORG_ORGUNIT(_tail,
				 [{xmlel, <<"ORGUNIT">>, _attrs, _els} | _acc]).

decode_vcard_org_ORG_ORGUNIT_cdata(<<>>) -> undefined;
decode_vcard_org_ORG_ORGUNIT_cdata(_val) -> _val.

encode_vcard_org_ORG_ORGUNIT_cdata(undefined, _acc) ->
    _acc;
encode_vcard_org_ORG_ORGUNIT_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_org_ORG_ORGNAME({xmlel, _, _attrs,
			      _els}) ->
    Cdata = decode_vcard_org_ORG_ORGNAME_els(_els, <<>>),
    Cdata.

decode_vcard_org_ORG_ORGNAME_els([{xmlcdata, _data}
				  | _els],
				 Cdata) ->
    decode_vcard_org_ORG_ORGNAME_els(_els,
				     <<Cdata/binary, _data/binary>>);
decode_vcard_org_ORG_ORGNAME_els([_ | _els], Cdata) ->
    decode_vcard_org_ORG_ORGNAME_els(_els, Cdata);
decode_vcard_org_ORG_ORGNAME_els([], Cdata) ->
    decode_vcard_org_ORG_ORGNAME_cdata(Cdata).

encode_vcard_org_ORG_ORGNAME(Cdata, _acc) ->
    _els = encode_vcard_org_ORG_ORGNAME_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"ORGNAME">>, _attrs, _els} | _acc].

decode_vcard_org_ORG_ORGNAME_cdata(<<>>) -> undefined;
decode_vcard_org_ORG_ORGNAME_cdata(_val) -> _val.

encode_vcard_org_ORG_ORGNAME_cdata(undefined, _acc) ->
    _acc;
encode_vcard_org_ORG_ORGNAME_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_sound_SOUND({xmlel, _, _attrs, _els}) ->
    {Extval, Binval, Phonetic} =
	decode_vcard_sound_SOUND_els(_els, undefined, undefined,
				     undefined),
    {vcard_sound, Phonetic, Binval, Extval}.

decode_vcard_sound_SOUND_els([{xmlel, <<"EXTVAL">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Extval, Binval, Phonetic) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_sound_SOUND_els(_els,
				       decode_vcard_extval_EXTVAL(_el), Binval,
				       Phonetic);
      _ ->
	  decode_vcard_sound_SOUND_els(_els, Extval, Binval,
				       Phonetic)
    end;
decode_vcard_sound_SOUND_els([{xmlel, <<"BINVAL">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Extval, Binval, Phonetic) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_sound_SOUND_els(_els, Extval,
				       decode_vcard_binval_BINVAL(_el),
				       Phonetic);
      _ ->
	  decode_vcard_sound_SOUND_els(_els, Extval, Binval,
				       Phonetic)
    end;
decode_vcard_sound_SOUND_els([{xmlel, <<"PHONETIC">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Extval, Binval, Phonetic) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_sound_SOUND_els(_els, Extval, Binval,
				       decode_vcard_sound_SOUND_PHONETIC(_el));
      _ ->
	  decode_vcard_sound_SOUND_els(_els, Extval, Binval,
				       Phonetic)
    end;
decode_vcard_sound_SOUND_els([_ | _els], Extval, Binval,
			     Phonetic) ->
    decode_vcard_sound_SOUND_els(_els, Extval, Binval,
				 Phonetic);
decode_vcard_sound_SOUND_els([], Extval, Binval,
			     Phonetic) ->
    {Extval, Binval, Phonetic}.

encode_vcard_sound_SOUND(undefined, _acc) -> _acc;
encode_vcard_sound_SOUND({vcard_sound, Phonetic, Binval,
			  Extval},
			 _acc) ->
    _els = encode_vcard_sound_SOUND_PHONETIC(Phonetic,
					     encode_vcard_binval_BINVAL(Binval,
									encode_vcard_extval_EXTVAL(Extval,
												   []))),
    _attrs = [],
    [{xmlel, <<"SOUND">>, _attrs, _els} | _acc].

decode_vcard_sound_SOUND_PHONETIC({xmlel, _, _attrs,
				   _els}) ->
    Cdata = decode_vcard_sound_SOUND_PHONETIC_els(_els,
						  <<>>),
    Cdata.

decode_vcard_sound_SOUND_PHONETIC_els([{xmlcdata, _data}
				       | _els],
				      Cdata) ->
    decode_vcard_sound_SOUND_PHONETIC_els(_els,
					  <<Cdata/binary, _data/binary>>);
decode_vcard_sound_SOUND_PHONETIC_els([_ | _els],
				      Cdata) ->
    decode_vcard_sound_SOUND_PHONETIC_els(_els, Cdata);
decode_vcard_sound_SOUND_PHONETIC_els([], Cdata) ->
    decode_vcard_sound_SOUND_PHONETIC_cdata(Cdata).

encode_vcard_sound_SOUND_PHONETIC(undefined, _acc) ->
    _acc;
encode_vcard_sound_SOUND_PHONETIC(Cdata, _acc) ->
    _els = encode_vcard_sound_SOUND_PHONETIC_cdata(Cdata,
						   []),
    _attrs = [],
    [{xmlel, <<"PHONETIC">>, _attrs, _els} | _acc].

decode_vcard_sound_SOUND_PHONETIC_cdata(<<>>) ->
    undefined;
decode_vcard_sound_SOUND_PHONETIC_cdata(_val) -> _val.

encode_vcard_sound_SOUND_PHONETIC_cdata(undefined,
					_acc) ->
    _acc;
encode_vcard_sound_SOUND_PHONETIC_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_key_KEY({xmlel, _, _attrs, _els}) ->
    {Cred, Type} = decode_vcard_key_KEY_els(_els, [],
					    undefined),
    {vcard_key, Type, Cred}.

decode_vcard_key_KEY_els([{xmlel, <<"CRED">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Cred, Type) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_key_KEY_els(_els,
				   [decode_vcard_key_KEY_CRED(_el) | Cred],
				   Type);
      _ -> decode_vcard_key_KEY_els(_els, Cred, Type)
    end;
decode_vcard_key_KEY_els([{xmlel, <<"TYPE">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Cred, Type) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_key_KEY_els(_els, Cred,
				   decode_vcard_type_TYPE(_el));
      _ -> decode_vcard_key_KEY_els(_els, Cred, Type)
    end;
decode_vcard_key_KEY_els([_ | _els], Cred, Type) ->
    decode_vcard_key_KEY_els(_els, Cred, Type);
decode_vcard_key_KEY_els([], [Cred], Type) ->
    {Cred, Type}.

encode_vcard_key_KEY(undefined, _acc) -> _acc;
encode_vcard_key_KEY({vcard_key, Type, Cred}, _acc) ->
    _els = encode_vcard_type_TYPE(Type,
				  encode_vcard_key_KEY_CRED(Cred, [])),
    _attrs = [],
    [{xmlel, <<"KEY">>, _attrs, _els} | _acc].

decode_vcard_key_KEY_CRED({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_key_KEY_CRED_els(_els, <<>>),
    Cdata.

decode_vcard_key_KEY_CRED_els([{xmlcdata, _data}
			       | _els],
			      Cdata) ->
    decode_vcard_key_KEY_CRED_els(_els,
				  <<Cdata/binary, _data/binary>>);
decode_vcard_key_KEY_CRED_els([_ | _els], Cdata) ->
    decode_vcard_key_KEY_CRED_els(_els, Cdata);
decode_vcard_key_KEY_CRED_els([], Cdata) ->
    decode_vcard_key_KEY_CRED_cdata(Cdata).

encode_vcard_key_KEY_CRED(Cdata, _acc) ->
    _els = encode_vcard_key_KEY_CRED_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"CRED">>, _attrs, _els} | _acc].

decode_vcard_key_KEY_CRED_cdata(<<>>) -> undefined;
decode_vcard_key_KEY_CRED_cdata(_val) -> _val.

encode_vcard_key_KEY_CRED_cdata(undefined, _acc) ->
    _acc;
encode_vcard_key_KEY_CRED_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard({xmlel, _, _attrs, _els}) ->
    {Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
     Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
     Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
     Photo, Nickname, N, Fn, Version} =
	decode_vcard_vCard_els(_els, undefined, undefined,
			       undefined, undefined, undefined, undefined,
			       undefined, undefined, undefined, undefined, [],
			       undefined, undefined, undefined, undefined,
			       undefined, undefined, undefined, undefined, [],
			       [], [], [], undefined, undefined, undefined,
			       undefined, undefined, undefined),
    {vcard, Version, Fn, N, Nickname, Photo, Bday, Adr,
     Label, Tel, Email, Jabberid, Mailer, Tz, Geo, Title,
     Role, Logo, Org, Categories, Note, Prodid, Rev,
     Sort_string, Sound, Uid, Url, Class, Key, Desc}.

decode_vcard_vCard_els([{xmlel, <<"DESC">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els,
				 decode_vcard_vCard_DESC(_el), Key, Class, Url,
				 Uid, Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"KEY">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc,
				 decode_vcard_key_KEY(_el), Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"CLASS">>, _attrs,
			 _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key,
				 decode_vcard_vCard_CLASS(_el), Url, Uid, Sound,
				 Sort_string, Rev, Prodid, Note, Categories,
				 Org, Logo, Role, Title, Geo, Tz, Mailer,
				 Jabberid, Email, Tel, Label, Adr, Bday, Photo,
				 Nickname, N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"URL">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class,
				 decode_vcard_vCard_URL(_el), Uid, Sound,
				 Sort_string, Rev, Prodid, Note, Categories,
				 Org, Logo, Role, Title, Geo, Tz, Mailer,
				 Jabberid, Email, Tel, Label, Adr, Bday, Photo,
				 Nickname, N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"UID">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url,
				 decode_vcard_vCard_UID(_el), Sound,
				 Sort_string, Rev, Prodid, Note, Categories,
				 Org, Logo, Role, Title, Geo, Tz, Mailer,
				 Jabberid, Email, Tel, Label, Adr, Bday, Photo,
				 Nickname, N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"SOUND">>, _attrs,
			 _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 decode_vcard_sound_SOUND(_el), Sort_string,
				 Rev, Prodid, Note, Categories, Org, Logo, Role,
				 Title, Geo, Tz, Mailer, Jabberid, Email, Tel,
				 Label, Adr, Bday, Photo, Nickname, N, Fn,
				 Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"SORT-STRING">>,
			 _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, 'decode_vcard_vCard_SORT-STRING'(_el),
				 Rev, Prodid, Note, Categories, Org, Logo, Role,
				 Title, Geo, Tz, Mailer, Jabberid, Email, Tel,
				 Label, Adr, Bday, Photo, Nickname, N, Fn,
				 Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"REV">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string,
				 decode_vcard_vCard_REV(_el), Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"PRODID">>, _attrs,
			 _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev,
				 decode_vcard_vCard_PRODID(_el), Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"NOTE">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid,
				 decode_vcard_vCard_NOTE(_el), Categories, Org,
				 Logo, Role, Title, Geo, Tz, Mailer, Jabberid,
				 Email, Tel, Label, Adr, Bday, Photo, Nickname,
				 N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"CATEGORIES">>,
			 _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 decode_vcard_vCard_CATEGORIES(_el), Org, Logo,
				 Role, Title, Geo, Tz, Mailer, Jabberid, Email,
				 Tel, Label, Adr, Bday, Photo, Nickname, N, Fn,
				 Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"ORG">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, decode_vcard_org_ORG(_el), Logo,
				 Role, Title, Geo, Tz, Mailer, Jabberid, Email,
				 Tel, Label, Adr, Bday, Photo, Nickname, N, Fn,
				 Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"LOGO">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, decode_vcard_logo_LOGO(_el),
				 Role, Title, Geo, Tz, Mailer, Jabberid, Email,
				 Tel, Label, Adr, Bday, Photo, Nickname, N, Fn,
				 Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"ROLE">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo,
				 decode_vcard_vCard_ROLE(_el), Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"TITLE">>, _attrs,
			 _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role,
				 decode_vcard_vCard_TITLE(_el), Geo, Tz, Mailer,
				 Jabberid, Email, Tel, Label, Adr, Bday, Photo,
				 Nickname, N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"GEO">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title,
				 decode_vcard_geo_GEO(_el), Tz, Mailer,
				 Jabberid, Email, Tel, Label, Adr, Bday, Photo,
				 Nickname, N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"TZ">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo,
				 decode_vcard_vCard_TZ(_el), Mailer, Jabberid,
				 Email, Tel, Label, Adr, Bday, Photo, Nickname,
				 N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"MAILER">>, _attrs,
			 _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 decode_vcard_vCard_MAILER(_el), Jabberid,
				 Email, Tel, Label, Adr, Bday, Photo, Nickname,
				 N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"JABBERID">>, _attrs,
			 _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, decode_vcard_vCard_JABBERID(_el),
				 Email, Tel, Label, Adr, Bday, Photo, Nickname,
				 N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"EMAIL">>, _attrs,
			 _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid,
				 [decode_vcard_email_EMAIL(_el) | Email], Tel,
				 Label, Adr, Bday, Photo, Nickname, N, Fn,
				 Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"TEL">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email,
				 [decode_vcard_tel_TEL(_el) | Tel], Label, Adr,
				 Bday, Photo, Nickname, N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"LABEL">>, _attrs,
			 _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel,
				 [decode_vcard_label_LABEL(_el) | Label], Adr,
				 Bday, Photo, Nickname, N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"ADR">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label,
				 [decode_vcard_adr_ADR(_el) | Adr], Bday, Photo,
				 Nickname, N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"BDAY">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr,
				 decode_vcard_vCard_BDAY(_el), Photo, Nickname,
				 N, Fn, Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"PHOTO">>, _attrs,
			 _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 decode_vcard_photo_PHOTO(_el), Nickname, N, Fn,
				 Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"NICKNAME">>, _attrs,
			 _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, decode_vcard_vCard_NICKNAME(_el), N, Fn,
				 Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"N">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, decode_vcard_name_N(_el), Fn,
				 Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"FN">>, _attrs, _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, decode_vcard_vCard_FN(_el),
				 Version);
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([{xmlel, <<"VERSION">>, _attrs,
			 _} =
			    _el
			| _els],
		       Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
		       Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
		       Tz, Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
		       Photo, Nickname, N, Fn, Version) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn,
				 decode_vcard_vCard_VERSION(_el));
      _ ->
	  decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
				 Sound, Sort_string, Rev, Prodid, Note,
				 Categories, Org, Logo, Role, Title, Geo, Tz,
				 Mailer, Jabberid, Email, Tel, Label, Adr, Bday,
				 Photo, Nickname, N, Fn, Version)
    end;
decode_vcard_vCard_els([_ | _els], Desc, Key, Class,
		       Url, Uid, Sound, Sort_string, Rev, Prodid, Note,
		       Categories, Org, Logo, Role, Title, Geo, Tz, Mailer,
		       Jabberid, Email, Tel, Label, Adr, Bday, Photo, Nickname,
		       N, Fn, Version) ->
    decode_vcard_vCard_els(_els, Desc, Key, Class, Url, Uid,
			   Sound, Sort_string, Rev, Prodid, Note, Categories,
			   Org, Logo, Role, Title, Geo, Tz, Mailer, Jabberid,
			   Email, Tel, Label, Adr, Bday, Photo, Nickname, N, Fn,
			   Version);
decode_vcard_vCard_els([], Desc, Key, Class, Url, Uid,
		       Sound, Sort_string, Rev, Prodid, Note, Categories, Org,
		       Logo, Role, Title, Geo, Tz, Mailer, Jabberid, Email,
		       Tel, Label, Adr, Bday, Photo, Nickname, N, Fn,
		       Version) ->
    {Desc, Key, Class, Url, Uid, Sound, Sort_string, Rev,
     Prodid, Note, Categories, Org, Logo, Role, Title, Geo,
     Tz, Mailer, Jabberid, lists:reverse(Email),
     lists:reverse(Tel), lists:reverse(Label),
     lists:reverse(Adr), Bday, Photo, Nickname, N, Fn,
     Version}.

encode_vcard_vCard(undefined, _acc) -> _acc;
encode_vcard_vCard({vcard, Version, Fn, N, Nickname,
		    Photo, Bday, Adr, Label, Tel, Email, Jabberid, Mailer,
		    Tz, Geo, Title, Role, Logo, Org, Categories, Note,
		    Prodid, Rev, Sort_string, Sound, Uid, Url, Class, Key,
		    Desc},
		   _acc) ->
    _els = encode_vcard_vCard_VERSION(Version,
				      encode_vcard_vCard_FN(Fn,
							    encode_vcard_name_N(N,
										encode_vcard_vCard_NICKNAME(Nickname,
													    encode_vcard_photo_PHOTO(Photo,
																     encode_vcard_vCard_BDAY(Bday,
																			     encode_vcard_adr_ADR(Adr,
																						  encode_vcard_label_LABEL(Label,
																									   encode_vcard_tel_TEL(Tel,
																												encode_vcard_email_EMAIL(Email,
																															 encode_vcard_vCard_JABBERID(Jabberid,
																																		     encode_vcard_vCard_MAILER(Mailer,
																																					       encode_vcard_vCard_TZ(Tz,
																																								     encode_vcard_geo_GEO(Geo,
																																											  encode_vcard_vCard_TITLE(Title,
																																														   encode_vcard_vCard_ROLE(Role,
																																																	   encode_vcard_logo_LOGO(Logo,
																																																				  encode_vcard_org_ORG(Org,
																																																						       encode_vcard_vCard_CATEGORIES(Categories,
																																																										     encode_vcard_vCard_NOTE(Note,
																																																													     encode_vcard_vCard_PRODID(Prodid,
																																																																       encode_vcard_vCard_REV(Rev,
																																																																			      'encode_vcard_vCard_SORT-STRING'(Sort_string,
																																																																							       encode_vcard_sound_SOUND(Sound,
																																																																											encode_vcard_vCard_UID(Uid,
																																																																													       encode_vcard_vCard_URL(Url,
																																																																																      encode_vcard_vCard_CLASS(Class,
																																																																																			       encode_vcard_key_KEY(Key,
																																																																																						    encode_vcard_vCard_DESC(Desc,
																																																																																									    []))))))))))))))))))))))))))))),
    _attrs = [{<<"xmlns">>, <<"vcard-temp">>}],
    [{xmlel, <<"vCard">>, _attrs, _els} | _acc].

decode_vcard_vCard_DESC({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_vCard_DESC_els(_els, <<>>), Cdata.

decode_vcard_vCard_DESC_els([{xmlcdata, _data} | _els],
			    Cdata) ->
    decode_vcard_vCard_DESC_els(_els,
				<<Cdata/binary, _data/binary>>);
decode_vcard_vCard_DESC_els([_ | _els], Cdata) ->
    decode_vcard_vCard_DESC_els(_els, Cdata);
decode_vcard_vCard_DESC_els([], Cdata) ->
    decode_vcard_vCard_DESC_cdata(Cdata).

encode_vcard_vCard_DESC(undefined, _acc) -> _acc;
encode_vcard_vCard_DESC(Cdata, _acc) ->
    _els = encode_vcard_vCard_DESC_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"DESC">>, _attrs, _els} | _acc].

decode_vcard_vCard_DESC_cdata(<<>>) -> undefined;
decode_vcard_vCard_DESC_cdata(_val) -> _val.

encode_vcard_vCard_DESC_cdata(undefined, _acc) -> _acc;
encode_vcard_vCard_DESC_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard_CLASS({xmlel, _, _attrs, _els}) ->
    Value = decode_vcard_vCard_CLASS_els(_els, undefined),
    Value.

decode_vcard_vCard_CLASS_els([{xmlel,
			       <<"CONFIDENTIAL">>, _attrs, _} =
				  _el
			      | _els],
			     Value) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_CLASS_els(_els,
				       decode_vcard_vCard_CLASS_CONFIDENTIAL(_el));
      _ -> decode_vcard_vCard_CLASS_els(_els, Value)
    end;
decode_vcard_vCard_CLASS_els([{xmlel, <<"PRIVATE">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Value) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_CLASS_els(_els,
				       decode_vcard_vCard_CLASS_PRIVATE(_el));
      _ -> decode_vcard_vCard_CLASS_els(_els, Value)
    end;
decode_vcard_vCard_CLASS_els([{xmlel, <<"PUBLIC">>,
			       _attrs, _} =
				  _el
			      | _els],
			     Value) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_CLASS_els(_els,
				       decode_vcard_vCard_CLASS_PUBLIC(_el));
      _ -> decode_vcard_vCard_CLASS_els(_els, Value)
    end;
decode_vcard_vCard_CLASS_els([_ | _els], Value) ->
    decode_vcard_vCard_CLASS_els(_els, Value);
decode_vcard_vCard_CLASS_els([], Value) -> Value.

'encode_vcard_vCard_CLASS_$value'(undefined, _acc) ->
    _acc;
'encode_vcard_vCard_CLASS_$value'(confidential = _r,
				  _acc) ->
    encode_vcard_vCard_CLASS_CONFIDENTIAL(_r, _acc);
'encode_vcard_vCard_CLASS_$value'(private = _r, _acc) ->
    encode_vcard_vCard_CLASS_PRIVATE(_r, _acc);
'encode_vcard_vCard_CLASS_$value'(public = _r, _acc) ->
    encode_vcard_vCard_CLASS_PUBLIC(_r, _acc).

encode_vcard_vCard_CLASS(undefined, _acc) -> _acc;
encode_vcard_vCard_CLASS(Value, _acc) ->
    _els = 'encode_vcard_vCard_CLASS_$value'(Value, []),
    _attrs = [],
    [{xmlel, <<"CLASS">>, _attrs, _els} | _acc].

decode_vcard_vCard_CLASS_CONFIDENTIAL({xmlel, _, _attrs,
				       _els}) ->
    confidential.

encode_vcard_vCard_CLASS_CONFIDENTIAL(undefined,
				      _acc) ->
    _acc;
encode_vcard_vCard_CLASS_CONFIDENTIAL(confidential,
				      _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"CONFIDENTIAL">>, _attrs, _els} | _acc].

decode_vcard_vCard_CLASS_PRIVATE({xmlel, _, _attrs,
				  _els}) ->
    private.

encode_vcard_vCard_CLASS_PRIVATE(undefined, _acc) ->
    _acc;
encode_vcard_vCard_CLASS_PRIVATE(private, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"PRIVATE">>, _attrs, _els} | _acc].

decode_vcard_vCard_CLASS_PUBLIC({xmlel, _, _attrs,
				 _els}) ->
    public.

encode_vcard_vCard_CLASS_PUBLIC(undefined, _acc) ->
    _acc;
encode_vcard_vCard_CLASS_PUBLIC(public, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"PUBLIC">>, _attrs, _els} | _acc].

decode_vcard_vCard_URL({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_vCard_URL_els(_els, <<>>), Cdata.

decode_vcard_vCard_URL_els([{xmlcdata, _data} | _els],
			   Cdata) ->
    decode_vcard_vCard_URL_els(_els,
			       <<Cdata/binary, _data/binary>>);
decode_vcard_vCard_URL_els([_ | _els], Cdata) ->
    decode_vcard_vCard_URL_els(_els, Cdata);
decode_vcard_vCard_URL_els([], Cdata) ->
    decode_vcard_vCard_URL_cdata(Cdata).

encode_vcard_vCard_URL(undefined, _acc) -> _acc;
encode_vcard_vCard_URL(Cdata, _acc) ->
    _els = encode_vcard_vCard_URL_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"URL">>, _attrs, _els} | _acc].

decode_vcard_vCard_URL_cdata(<<>>) -> undefined;
decode_vcard_vCard_URL_cdata(_val) -> _val.

encode_vcard_vCard_URL_cdata(undefined, _acc) -> _acc;
encode_vcard_vCard_URL_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard_UID({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_vCard_UID_els(_els, <<>>), Cdata.

decode_vcard_vCard_UID_els([{xmlcdata, _data} | _els],
			   Cdata) ->
    decode_vcard_vCard_UID_els(_els,
			       <<Cdata/binary, _data/binary>>);
decode_vcard_vCard_UID_els([_ | _els], Cdata) ->
    decode_vcard_vCard_UID_els(_els, Cdata);
decode_vcard_vCard_UID_els([], Cdata) ->
    decode_vcard_vCard_UID_cdata(Cdata).

encode_vcard_vCard_UID(undefined, _acc) -> _acc;
encode_vcard_vCard_UID(Cdata, _acc) ->
    _els = encode_vcard_vCard_UID_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"UID">>, _attrs, _els} | _acc].

decode_vcard_vCard_UID_cdata(<<>>) -> undefined;
decode_vcard_vCard_UID_cdata(_val) -> _val.

encode_vcard_vCard_UID_cdata(undefined, _acc) -> _acc;
encode_vcard_vCard_UID_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

'decode_vcard_vCard_SORT-STRING'({xmlel, _, _attrs,
				  _els}) ->
    Cdata = 'decode_vcard_vCard_SORT-STRING_els'(_els,
						 <<>>),
    Cdata.

'decode_vcard_vCard_SORT-STRING_els'([{xmlcdata, _data}
				      | _els],
				     Cdata) ->
    'decode_vcard_vCard_SORT-STRING_els'(_els,
					 <<Cdata/binary, _data/binary>>);
'decode_vcard_vCard_SORT-STRING_els'([_ | _els],
				     Cdata) ->
    'decode_vcard_vCard_SORT-STRING_els'(_els, Cdata);
'decode_vcard_vCard_SORT-STRING_els'([], Cdata) ->
    'decode_vcard_vCard_SORT-STRING_cdata'(Cdata).

'encode_vcard_vCard_SORT-STRING'(undefined, _acc) ->
    _acc;
'encode_vcard_vCard_SORT-STRING'(Cdata, _acc) ->
    _els = 'encode_vcard_vCard_SORT-STRING_cdata'(Cdata,
						  []),
    _attrs = [],
    [{xmlel, <<"SORT-STRING">>, _attrs, _els} | _acc].

'decode_vcard_vCard_SORT-STRING_cdata'(<<>>) ->
    undefined;
'decode_vcard_vCard_SORT-STRING_cdata'(_val) -> _val.

'encode_vcard_vCard_SORT-STRING_cdata'(undefined,
				       _acc) ->
    _acc;
'encode_vcard_vCard_SORT-STRING_cdata'(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard_REV({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_vCard_REV_els(_els, <<>>), Cdata.

decode_vcard_vCard_REV_els([{xmlcdata, _data} | _els],
			   Cdata) ->
    decode_vcard_vCard_REV_els(_els,
			       <<Cdata/binary, _data/binary>>);
decode_vcard_vCard_REV_els([_ | _els], Cdata) ->
    decode_vcard_vCard_REV_els(_els, Cdata);
decode_vcard_vCard_REV_els([], Cdata) ->
    decode_vcard_vCard_REV_cdata(Cdata).

encode_vcard_vCard_REV(undefined, _acc) -> _acc;
encode_vcard_vCard_REV(Cdata, _acc) ->
    _els = encode_vcard_vCard_REV_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"REV">>, _attrs, _els} | _acc].

decode_vcard_vCard_REV_cdata(<<>>) -> undefined;
decode_vcard_vCard_REV_cdata(_val) -> _val.

encode_vcard_vCard_REV_cdata(undefined, _acc) -> _acc;
encode_vcard_vCard_REV_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard_PRODID({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_vCard_PRODID_els(_els, <<>>),
    Cdata.

decode_vcard_vCard_PRODID_els([{xmlcdata, _data}
			       | _els],
			      Cdata) ->
    decode_vcard_vCard_PRODID_els(_els,
				  <<Cdata/binary, _data/binary>>);
decode_vcard_vCard_PRODID_els([_ | _els], Cdata) ->
    decode_vcard_vCard_PRODID_els(_els, Cdata);
decode_vcard_vCard_PRODID_els([], Cdata) ->
    decode_vcard_vCard_PRODID_cdata(Cdata).

encode_vcard_vCard_PRODID(undefined, _acc) -> _acc;
encode_vcard_vCard_PRODID(Cdata, _acc) ->
    _els = encode_vcard_vCard_PRODID_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"PRODID">>, _attrs, _els} | _acc].

decode_vcard_vCard_PRODID_cdata(<<>>) -> undefined;
decode_vcard_vCard_PRODID_cdata(_val) -> _val.

encode_vcard_vCard_PRODID_cdata(undefined, _acc) ->
    _acc;
encode_vcard_vCard_PRODID_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard_NOTE({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_vCard_NOTE_els(_els, <<>>), Cdata.

decode_vcard_vCard_NOTE_els([{xmlcdata, _data} | _els],
			    Cdata) ->
    decode_vcard_vCard_NOTE_els(_els,
				<<Cdata/binary, _data/binary>>);
decode_vcard_vCard_NOTE_els([_ | _els], Cdata) ->
    decode_vcard_vCard_NOTE_els(_els, Cdata);
decode_vcard_vCard_NOTE_els([], Cdata) ->
    decode_vcard_vCard_NOTE_cdata(Cdata).

encode_vcard_vCard_NOTE(undefined, _acc) -> _acc;
encode_vcard_vCard_NOTE(Cdata, _acc) ->
    _els = encode_vcard_vCard_NOTE_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"NOTE">>, _attrs, _els} | _acc].

decode_vcard_vCard_NOTE_cdata(<<>>) -> undefined;
decode_vcard_vCard_NOTE_cdata(_val) -> _val.

encode_vcard_vCard_NOTE_cdata(undefined, _acc) -> _acc;
encode_vcard_vCard_NOTE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard_CATEGORIES({xmlel, _, _attrs,
			       _els}) ->
    Keywords = decode_vcard_vCard_CATEGORIES_els(_els, []),
    Keywords.

decode_vcard_vCard_CATEGORIES_els([{xmlel,
				    <<"KEYWORD">>, _attrs, _} =
				       _el
				   | _els],
				  Keywords) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_vcard_vCard_CATEGORIES_els(_els,
					    [decode_vcard_vCard_CATEGORIES_KEYWORD(_el)
					     | Keywords]);
      _ -> decode_vcard_vCard_CATEGORIES_els(_els, Keywords)
    end;
decode_vcard_vCard_CATEGORIES_els([_ | _els],
				  Keywords) ->
    decode_vcard_vCard_CATEGORIES_els(_els, Keywords);
decode_vcard_vCard_CATEGORIES_els([], Keywords) ->
    lists:reverse(Keywords).

encode_vcard_vCard_CATEGORIES([], _acc) -> _acc;
encode_vcard_vCard_CATEGORIES(Keywords, _acc) ->
    _els = encode_vcard_vCard_CATEGORIES_KEYWORD(Keywords,
						 []),
    _attrs = [],
    [{xmlel, <<"CATEGORIES">>, _attrs, _els} | _acc].

decode_vcard_vCard_CATEGORIES_KEYWORD({xmlel, _, _attrs,
				       _els}) ->
    Cdata = decode_vcard_vCard_CATEGORIES_KEYWORD_els(_els,
						      <<>>),
    Cdata.

decode_vcard_vCard_CATEGORIES_KEYWORD_els([{xmlcdata,
					    _data}
					   | _els],
					  Cdata) ->
    decode_vcard_vCard_CATEGORIES_KEYWORD_els(_els,
					      <<Cdata/binary, _data/binary>>);
decode_vcard_vCard_CATEGORIES_KEYWORD_els([_ | _els],
					  Cdata) ->
    decode_vcard_vCard_CATEGORIES_KEYWORD_els(_els, Cdata);
decode_vcard_vCard_CATEGORIES_KEYWORD_els([], Cdata) ->
    decode_vcard_vCard_CATEGORIES_KEYWORD_cdata(Cdata).

encode_vcard_vCard_CATEGORIES_KEYWORD([], _acc) -> _acc;
encode_vcard_vCard_CATEGORIES_KEYWORD([Cdata | _tail],
				      _acc) ->
    _els =
	encode_vcard_vCard_CATEGORIES_KEYWORD_cdata(Cdata, []),
    _attrs = [],
    encode_vcard_vCard_CATEGORIES_KEYWORD(_tail,
					  [{xmlel, <<"KEYWORD">>, _attrs, _els}
					   | _acc]).

decode_vcard_vCard_CATEGORIES_KEYWORD_cdata(<<>>) ->
    undefined;
decode_vcard_vCard_CATEGORIES_KEYWORD_cdata(_val) ->
    _val.

encode_vcard_vCard_CATEGORIES_KEYWORD_cdata(undefined,
					    _acc) ->
    _acc;
encode_vcard_vCard_CATEGORIES_KEYWORD_cdata(_val,
					    _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard_ROLE({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_vCard_ROLE_els(_els, <<>>), Cdata.

decode_vcard_vCard_ROLE_els([{xmlcdata, _data} | _els],
			    Cdata) ->
    decode_vcard_vCard_ROLE_els(_els,
				<<Cdata/binary, _data/binary>>);
decode_vcard_vCard_ROLE_els([_ | _els], Cdata) ->
    decode_vcard_vCard_ROLE_els(_els, Cdata);
decode_vcard_vCard_ROLE_els([], Cdata) ->
    decode_vcard_vCard_ROLE_cdata(Cdata).

encode_vcard_vCard_ROLE(undefined, _acc) -> _acc;
encode_vcard_vCard_ROLE(Cdata, _acc) ->
    _els = encode_vcard_vCard_ROLE_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"ROLE">>, _attrs, _els} | _acc].

decode_vcard_vCard_ROLE_cdata(<<>>) -> undefined;
decode_vcard_vCard_ROLE_cdata(_val) -> _val.

encode_vcard_vCard_ROLE_cdata(undefined, _acc) -> _acc;
encode_vcard_vCard_ROLE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard_TITLE({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_vCard_TITLE_els(_els, <<>>), Cdata.

decode_vcard_vCard_TITLE_els([{xmlcdata, _data} | _els],
			     Cdata) ->
    decode_vcard_vCard_TITLE_els(_els,
				 <<Cdata/binary, _data/binary>>);
decode_vcard_vCard_TITLE_els([_ | _els], Cdata) ->
    decode_vcard_vCard_TITLE_els(_els, Cdata);
decode_vcard_vCard_TITLE_els([], Cdata) ->
    decode_vcard_vCard_TITLE_cdata(Cdata).

encode_vcard_vCard_TITLE(undefined, _acc) -> _acc;
encode_vcard_vCard_TITLE(Cdata, _acc) ->
    _els = encode_vcard_vCard_TITLE_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"TITLE">>, _attrs, _els} | _acc].

decode_vcard_vCard_TITLE_cdata(<<>>) -> undefined;
decode_vcard_vCard_TITLE_cdata(_val) -> _val.

encode_vcard_vCard_TITLE_cdata(undefined, _acc) -> _acc;
encode_vcard_vCard_TITLE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard_TZ({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_vCard_TZ_els(_els, <<>>), Cdata.

decode_vcard_vCard_TZ_els([{xmlcdata, _data} | _els],
			  Cdata) ->
    decode_vcard_vCard_TZ_els(_els,
			      <<Cdata/binary, _data/binary>>);
decode_vcard_vCard_TZ_els([_ | _els], Cdata) ->
    decode_vcard_vCard_TZ_els(_els, Cdata);
decode_vcard_vCard_TZ_els([], Cdata) ->
    decode_vcard_vCard_TZ_cdata(Cdata).

encode_vcard_vCard_TZ(undefined, _acc) -> _acc;
encode_vcard_vCard_TZ(Cdata, _acc) ->
    _els = encode_vcard_vCard_TZ_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"TZ">>, _attrs, _els} | _acc].

decode_vcard_vCard_TZ_cdata(<<>>) -> undefined;
decode_vcard_vCard_TZ_cdata(_val) -> _val.

encode_vcard_vCard_TZ_cdata(undefined, _acc) -> _acc;
encode_vcard_vCard_TZ_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard_MAILER({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_vCard_MAILER_els(_els, <<>>),
    Cdata.

decode_vcard_vCard_MAILER_els([{xmlcdata, _data}
			       | _els],
			      Cdata) ->
    decode_vcard_vCard_MAILER_els(_els,
				  <<Cdata/binary, _data/binary>>);
decode_vcard_vCard_MAILER_els([_ | _els], Cdata) ->
    decode_vcard_vCard_MAILER_els(_els, Cdata);
decode_vcard_vCard_MAILER_els([], Cdata) ->
    decode_vcard_vCard_MAILER_cdata(Cdata).

encode_vcard_vCard_MAILER(undefined, _acc) -> _acc;
encode_vcard_vCard_MAILER(Cdata, _acc) ->
    _els = encode_vcard_vCard_MAILER_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"MAILER">>, _attrs, _els} | _acc].

decode_vcard_vCard_MAILER_cdata(<<>>) -> undefined;
decode_vcard_vCard_MAILER_cdata(_val) -> _val.

encode_vcard_vCard_MAILER_cdata(undefined, _acc) ->
    _acc;
encode_vcard_vCard_MAILER_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard_JABBERID({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_vCard_JABBERID_els(_els, <<>>),
    Cdata.

decode_vcard_vCard_JABBERID_els([{xmlcdata, _data}
				 | _els],
				Cdata) ->
    decode_vcard_vCard_JABBERID_els(_els,
				    <<Cdata/binary, _data/binary>>);
decode_vcard_vCard_JABBERID_els([_ | _els], Cdata) ->
    decode_vcard_vCard_JABBERID_els(_els, Cdata);
decode_vcard_vCard_JABBERID_els([], Cdata) ->
    decode_vcard_vCard_JABBERID_cdata(Cdata).

encode_vcard_vCard_JABBERID(undefined, _acc) -> _acc;
encode_vcard_vCard_JABBERID(Cdata, _acc) ->
    _els = encode_vcard_vCard_JABBERID_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"JABBERID">>, _attrs, _els} | _acc].

decode_vcard_vCard_JABBERID_cdata(<<>>) -> undefined;
decode_vcard_vCard_JABBERID_cdata(_val) -> _val.

encode_vcard_vCard_JABBERID_cdata(undefined, _acc) ->
    _acc;
encode_vcard_vCard_JABBERID_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard_BDAY({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_vCard_BDAY_els(_els, <<>>), Cdata.

decode_vcard_vCard_BDAY_els([{xmlcdata, _data} | _els],
			    Cdata) ->
    decode_vcard_vCard_BDAY_els(_els,
				<<Cdata/binary, _data/binary>>);
decode_vcard_vCard_BDAY_els([_ | _els], Cdata) ->
    decode_vcard_vCard_BDAY_els(_els, Cdata);
decode_vcard_vCard_BDAY_els([], Cdata) ->
    decode_vcard_vCard_BDAY_cdata(Cdata).

encode_vcard_vCard_BDAY(undefined, _acc) -> _acc;
encode_vcard_vCard_BDAY(Cdata, _acc) ->
    _els = encode_vcard_vCard_BDAY_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"BDAY">>, _attrs, _els} | _acc].

decode_vcard_vCard_BDAY_cdata(<<>>) -> undefined;
decode_vcard_vCard_BDAY_cdata(_val) -> _val.

encode_vcard_vCard_BDAY_cdata(undefined, _acc) -> _acc;
encode_vcard_vCard_BDAY_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard_NICKNAME({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_vCard_NICKNAME_els(_els, <<>>),
    Cdata.

decode_vcard_vCard_NICKNAME_els([{xmlcdata, _data}
				 | _els],
				Cdata) ->
    decode_vcard_vCard_NICKNAME_els(_els,
				    <<Cdata/binary, _data/binary>>);
decode_vcard_vCard_NICKNAME_els([_ | _els], Cdata) ->
    decode_vcard_vCard_NICKNAME_els(_els, Cdata);
decode_vcard_vCard_NICKNAME_els([], Cdata) ->
    decode_vcard_vCard_NICKNAME_cdata(Cdata).

encode_vcard_vCard_NICKNAME(undefined, _acc) -> _acc;
encode_vcard_vCard_NICKNAME(Cdata, _acc) ->
    _els = encode_vcard_vCard_NICKNAME_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"NICKNAME">>, _attrs, _els} | _acc].

decode_vcard_vCard_NICKNAME_cdata(<<>>) -> undefined;
decode_vcard_vCard_NICKNAME_cdata(_val) -> _val.

encode_vcard_vCard_NICKNAME_cdata(undefined, _acc) ->
    _acc;
encode_vcard_vCard_NICKNAME_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard_FN({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_vCard_FN_els(_els, <<>>), Cdata.

decode_vcard_vCard_FN_els([{xmlcdata, _data} | _els],
			  Cdata) ->
    decode_vcard_vCard_FN_els(_els,
			      <<Cdata/binary, _data/binary>>);
decode_vcard_vCard_FN_els([_ | _els], Cdata) ->
    decode_vcard_vCard_FN_els(_els, Cdata);
decode_vcard_vCard_FN_els([], Cdata) ->
    decode_vcard_vCard_FN_cdata(Cdata).

encode_vcard_vCard_FN(undefined, _acc) -> _acc;
encode_vcard_vCard_FN(Cdata, _acc) ->
    _els = encode_vcard_vCard_FN_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"FN">>, _attrs, _els} | _acc].

decode_vcard_vCard_FN_cdata(<<>>) -> undefined;
decode_vcard_vCard_FN_cdata(_val) -> _val.

encode_vcard_vCard_FN_cdata(undefined, _acc) -> _acc;
encode_vcard_vCard_FN_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_vCard_VERSION({xmlel, _, _attrs, _els}) ->
    Cdata = decode_vcard_vCard_VERSION_els(_els, <<>>),
    Cdata.

decode_vcard_vCard_VERSION_els([{xmlcdata, _data}
				| _els],
			       Cdata) ->
    decode_vcard_vCard_VERSION_els(_els,
				   <<Cdata/binary, _data/binary>>);
decode_vcard_vCard_VERSION_els([_ | _els], Cdata) ->
    decode_vcard_vCard_VERSION_els(_els, Cdata);
decode_vcard_vCard_VERSION_els([], Cdata) ->
    decode_vcard_vCard_VERSION_cdata(Cdata).

encode_vcard_vCard_VERSION(undefined, _acc) -> _acc;
encode_vcard_vCard_VERSION(Cdata, _acc) ->
    _els = encode_vcard_vCard_VERSION_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"VERSION">>, _attrs, _els} | _acc].

decode_vcard_vCard_VERSION_cdata(<<>>) -> undefined;
decode_vcard_vCard_VERSION_cdata(_val) -> _val.

encode_vcard_vCard_VERSION_cdata(undefined, _acc) ->
    _acc;
encode_vcard_vCard_VERSION_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_xfield_field({xmlel, _, _attrs, _els}) ->
    {Var, Type, Label} = decode_xfield_field_attrs(_attrs,
						   undefined, undefined,
						   undefined),
    {Options, Values, Desc, Required} =
	decode_xfield_field_els(_els, [], [], undefined, false),
    {xfield, Label, Type, Var, Required, Desc, Values,
     Options}.

decode_xfield_field_els([{xmlel, <<"option">>, _attrs,
			  _} =
			     _el
			 | _els],
			Options, Values, Desc, Required) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_xfield_field_els(_els,
				  [decode_xfield_field_option(_el) | Options],
				  Values, Desc, Required);
      _ ->
	  decode_xfield_field_els(_els, Options, Values, Desc,
				  Required)
    end;
decode_xfield_field_els([{xmlel, <<"value">>, _attrs,
			  _} =
			     _el
			 | _els],
			Options, Values, Desc, Required) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_xfield_field_els(_els, Options,
				  [decode_xfield_field_value(_el) | Values],
				  Desc, Required);
      _ ->
	  decode_xfield_field_els(_els, Options, Values, Desc,
				  Required)
    end;
decode_xfield_field_els([{xmlel, <<"desc">>, _attrs,
			  _} =
			     _el
			 | _els],
			Options, Values, Desc, Required) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_xfield_field_els(_els, Options, Values,
				  decode_xfield_field_desc(_el), Required);
      _ ->
	  decode_xfield_field_els(_els, Options, Values, Desc,
				  Required)
    end;
decode_xfield_field_els([{xmlel, <<"required">>, _attrs,
			  _} =
			     _el
			 | _els],
			Options, Values, Desc, Required) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_xfield_field_els(_els, Options, Values, Desc,
				  decode_xfield_field_required(_el));
      _ ->
	  decode_xfield_field_els(_els, Options, Values, Desc,
				  Required)
    end;
decode_xfield_field_els([_ | _els], Options, Values,
			Desc, Required) ->
    decode_xfield_field_els(_els, Options, Values, Desc,
			    Required);
decode_xfield_field_els([], Options, Values, Desc,
			Required) ->
    {lists:reverse(Options), lists:reverse(Values), Desc,
     Required}.

decode_xfield_field_attrs([{<<"var">>, _val} | _attrs],
			  _Var, Type, Label) ->
    decode_xfield_field_attrs(_attrs, _val, Type, Label);
decode_xfield_field_attrs([{<<"type">>, _val} | _attrs],
			  Var, _Type, Label) ->
    decode_xfield_field_attrs(_attrs, Var, _val, Label);
decode_xfield_field_attrs([{<<"label">>, _val}
			   | _attrs],
			  Var, Type, _Label) ->
    decode_xfield_field_attrs(_attrs, Var, Type, _val);
decode_xfield_field_attrs([_ | _attrs], Var, Type,
			  Label) ->
    decode_xfield_field_attrs(_attrs, Var, Type, Label);
decode_xfield_field_attrs([], Var, Type, Label) ->
    {decode_xfield_field_var(Var),
     decode_xfield_field_type(Type),
     decode_xfield_field_label(Label)}.

encode_xfield_field([], _acc) -> _acc;
encode_xfield_field([{xfield, Label, Type, Var,
		      Required, Desc, Values, Options}
		     | _tail],
		    _acc) ->
    _els = encode_xfield_field_required(Required,
					encode_xfield_field_desc(Desc,
								 encode_xfield_field_value(Values,
											   encode_xfield_field_option(Options,
														      [])))),
    _attrs = encode_xfield_field_label(Label,
				       encode_xfield_field_type(Type,
								encode_xfield_field_var(Var,
											[]))),
    encode_xfield_field(_tail,
			[{xmlel, <<"field">>, _attrs, _els} | _acc]).

decode_xfield_field_label(undefined) -> undefined;
decode_xfield_field_label(_val) -> _val.

encode_xfield_field_label(undefined, _acc) -> _acc;
encode_xfield_field_label(_val, _acc) ->
    [{<<"label">>, _val} | _acc].

decode_xfield_field_type(undefined) -> undefined;
decode_xfield_field_type(_val) ->
    case catch xml_gen:dec_enum(_val,
				[boolean, fixed, hidden, 'jid-multi',
				 'jid-single', 'list-multi', 'list-single',
				 'text-multi', 'text-private', 'text-single'])
	of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"type">>, <<"field">>,
			<<>>});
      _res -> _res
    end.

encode_xfield_field_type(undefined, _acc) -> _acc;
encode_xfield_field_type(_val, _acc) ->
    [{<<"type">>, xml_gen:enc_enum(_val)} | _acc].

decode_xfield_field_var(undefined) -> undefined;
decode_xfield_field_var(_val) -> _val.

encode_xfield_field_var(undefined, _acc) -> _acc;
encode_xfield_field_var(_val, _acc) ->
    [{<<"var">>, _val} | _acc].

decode_xfield_field_option({xmlel, _, _attrs, _els}) ->
    Value = decode_xfield_field_option_els(_els, []), Value.

decode_xfield_field_option_els([{xmlel, <<"value">>,
				 _attrs, _} =
				    _el
				| _els],
			       Value) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_xfield_field_option_els(_els,
					 [decode_xfield_field_option_value(_el)
					  | Value]);
      _ -> decode_xfield_field_option_els(_els, Value)
    end;
decode_xfield_field_option_els([_ | _els], Value) ->
    decode_xfield_field_option_els(_els, Value);
decode_xfield_field_option_els([], [Value]) -> Value.

encode_xfield_field_option([], _acc) -> _acc;
encode_xfield_field_option([Value | _tail], _acc) ->
    _els = encode_xfield_field_option_value(Value, []),
    _attrs = [],
    encode_xfield_field_option(_tail,
			       [{xmlel, <<"option">>, _attrs, _els} | _acc]).

decode_xfield_field_option_value({xmlel, _, _attrs,
				  _els}) ->
    Cdata = decode_xfield_field_option_value_els(_els,
						 <<>>),
    Cdata.

decode_xfield_field_option_value_els([{xmlcdata, _data}
				      | _els],
				     Cdata) ->
    decode_xfield_field_option_value_els(_els,
					 <<Cdata/binary, _data/binary>>);
decode_xfield_field_option_value_els([_ | _els],
				     Cdata) ->
    decode_xfield_field_option_value_els(_els, Cdata);
decode_xfield_field_option_value_els([], Cdata) ->
    decode_xfield_field_option_value_cdata(Cdata).

encode_xfield_field_option_value(Cdata, _acc) ->
    _els = encode_xfield_field_option_value_cdata(Cdata,
						  []),
    _attrs = [],
    [{xmlel, <<"value">>, _attrs, _els} | _acc].

decode_xfield_field_option_value_cdata(<<>>) ->
    undefined;
decode_xfield_field_option_value_cdata(_val) -> _val.

encode_xfield_field_option_value_cdata(undefined,
				       _acc) ->
    _acc;
encode_xfield_field_option_value_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_xfield_field_value({xmlel, _, _attrs, _els}) ->
    Cdata = decode_xfield_field_value_els(_els, <<>>),
    Cdata.

decode_xfield_field_value_els([{xmlcdata, _data}
			       | _els],
			      Cdata) ->
    decode_xfield_field_value_els(_els,
				  <<Cdata/binary, _data/binary>>);
decode_xfield_field_value_els([_ | _els], Cdata) ->
    decode_xfield_field_value_els(_els, Cdata);
decode_xfield_field_value_els([], Cdata) ->
    decode_xfield_field_value_cdata(Cdata).

encode_xfield_field_value([], _acc) -> _acc;
encode_xfield_field_value([Cdata | _tail], _acc) ->
    _els = encode_xfield_field_value_cdata(Cdata, []),
    _attrs = [],
    encode_xfield_field_value(_tail,
			      [{xmlel, <<"value">>, _attrs, _els} | _acc]).

decode_xfield_field_value_cdata(<<>>) -> undefined;
decode_xfield_field_value_cdata(_val) -> _val.

encode_xfield_field_value_cdata(undefined, _acc) ->
    _acc;
encode_xfield_field_value_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_xfield_field_desc({xmlel, _, _attrs, _els}) ->
    Cdata = decode_xfield_field_desc_els(_els, <<>>), Cdata.

decode_xfield_field_desc_els([{xmlcdata, _data} | _els],
			     Cdata) ->
    decode_xfield_field_desc_els(_els,
				 <<Cdata/binary, _data/binary>>);
decode_xfield_field_desc_els([_ | _els], Cdata) ->
    decode_xfield_field_desc_els(_els, Cdata);
decode_xfield_field_desc_els([], Cdata) ->
    decode_xfield_field_desc_cdata(Cdata).

encode_xfield_field_desc(undefined, _acc) -> _acc;
encode_xfield_field_desc(Cdata, _acc) ->
    _els = encode_xfield_field_desc_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"desc">>, _attrs, _els} | _acc].

decode_xfield_field_desc_cdata(<<>>) -> undefined;
decode_xfield_field_desc_cdata(_val) -> _val.

encode_xfield_field_desc_cdata(undefined, _acc) -> _acc;
encode_xfield_field_desc_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_xfield_field_required({xmlel, _, _attrs,
			      _els}) ->
    true.

encode_xfield_field_required(false, _acc) -> _acc;
encode_xfield_field_required(true, _acc) ->
    _els = [],
    _attrs = [],
    [{xmlel, <<"required">>, _attrs, _els} | _acc].

decode_xdata_x({xmlel, _, _attrs, _els}) ->
    Type = decode_xdata_x_attrs(_attrs, undefined),
    {Fields, Items, Reported, Title, Instructions} =
	decode_xdata_x_els(_els, [], [], undefined, undefined,
			   []),
    {xdata, Type, Instructions, Title, Reported, Items,
     Fields}.

decode_xdata_x_els([{xmlel, <<"field">>, _attrs, _} =
			_el
		    | _els],
		   Fields, Items, Reported, Title, Instructions) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_xdata_x_els(_els,
			     [decode_xfield_field(_el) | Fields], Items,
			     Reported, Title, Instructions);
      _ ->
	  decode_xdata_x_els(_els, Fields, Items, Reported, Title,
			     Instructions)
    end;
decode_xdata_x_els([{xmlel, <<"item">>, _attrs, _} = _el
		    | _els],
		   Fields, Items, Reported, Title, Instructions) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_xdata_x_els(_els, Fields,
			     [decode_xdata_x_item(_el) | Items], Reported,
			     Title, Instructions);
      _ ->
	  decode_xdata_x_els(_els, Fields, Items, Reported, Title,
			     Instructions)
    end;
decode_xdata_x_els([{xmlel, <<"reported">>, _attrs, _} =
			_el
		    | _els],
		   Fields, Items, Reported, Title, Instructions) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_xdata_x_els(_els, Fields, Items,
			     decode_xdata_x_reported(_el), Title, Instructions);
      _ ->
	  decode_xdata_x_els(_els, Fields, Items, Reported, Title,
			     Instructions)
    end;
decode_xdata_x_els([{xmlel, <<"title">>, _attrs, _} =
			_el
		    | _els],
		   Fields, Items, Reported, Title, Instructions) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_xdata_x_els(_els, Fields, Items, Reported,
			     decode_xdata_x_title(_el), Instructions);
      _ ->
	  decode_xdata_x_els(_els, Fields, Items, Reported, Title,
			     Instructions)
    end;
decode_xdata_x_els([{xmlel, <<"instructions">>, _attrs,
		     _} =
			_el
		    | _els],
		   Fields, Items, Reported, Title, Instructions) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_xdata_x_els(_els, Fields, Items, Reported, Title,
			     [decode_xdata_x_instructions(_el) | Instructions]);
      _ ->
	  decode_xdata_x_els(_els, Fields, Items, Reported, Title,
			     Instructions)
    end;
decode_xdata_x_els([_ | _els], Fields, Items, Reported,
		   Title, Instructions) ->
    decode_xdata_x_els(_els, Fields, Items, Reported, Title,
		       Instructions);
decode_xdata_x_els([], Fields, Items, Reported, Title,
		   Instructions) ->
    {lists:reverse(Fields), lists:reverse(Items), Reported,
     Title, lists:reverse(Instructions)}.

decode_xdata_x_attrs([{<<"type">>, _val} | _attrs],
		     _Type) ->
    decode_xdata_x_attrs(_attrs, _val);
decode_xdata_x_attrs([_ | _attrs], Type) ->
    decode_xdata_x_attrs(_attrs, Type);
decode_xdata_x_attrs([], Type) ->
    decode_xdata_x_type(Type).

encode_xdata_x([], _acc) -> _acc;
encode_xdata_x([{xdata, Type, Instructions, Title,
		 Reported, Items, Fields}
		| _tail],
	       _acc) ->
    _els = encode_xdata_x_instructions(Instructions,
				       encode_xdata_x_title(Title,
							    encode_xdata_x_reported(Reported,
										    encode_xdata_x_item(Items,
													encode_xfield_field(Fields,
															    []))))),
    _attrs = encode_xdata_x_type(Type,
				 [{<<"xmlns">>, <<"jabber:x:data">>}]),
    encode_xdata_x(_tail,
		   [{xmlel, <<"x">>, _attrs, _els} | _acc]).

decode_xdata_x_type(undefined) ->
    erlang:error({missing_attr, <<"type">>, <<"x">>,
		  <<"jabber:x:data">>});
decode_xdata_x_type(_val) ->
    case catch xml_gen:dec_enum(_val,
				[cancel, form, result, submit])
	of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"type">>, <<"x">>,
			<<"jabber:x:data">>});
      _res -> _res
    end.

encode_xdata_x_type(_val, _acc) ->
    [{<<"type">>, xml_gen:enc_enum(_val)} | _acc].

decode_xdata_x_item({xmlel, _, _attrs, _els}) ->
    Fields = decode_xdata_x_item_els(_els, []), Fields.

decode_xdata_x_item_els([{xmlel, <<"field">>, _attrs,
			  _} =
			     _el
			 | _els],
			Fields) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_xdata_x_item_els(_els,
				  [decode_xfield_field(_el) | Fields]);
      _ -> decode_xdata_x_item_els(_els, Fields)
    end;
decode_xdata_x_item_els([_ | _els], Fields) ->
    decode_xdata_x_item_els(_els, Fields);
decode_xdata_x_item_els([], Fields) ->
    lists:reverse(Fields).

encode_xdata_x_item([], _acc) -> _acc;
encode_xdata_x_item([Fields | _tail], _acc) ->
    _els = encode_xfield_field(Fields, []),
    _attrs = [],
    encode_xdata_x_item(_tail,
			[{xmlel, <<"item">>, _attrs, _els} | _acc]).

decode_xdata_x_reported({xmlel, _, _attrs, _els}) ->
    Fields = decode_xdata_x_reported_els(_els, []), Fields.

decode_xdata_x_reported_els([{xmlel, <<"field">>,
			      _attrs, _} =
				 _el
			     | _els],
			    Fields) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_xdata_x_reported_els(_els,
				      [decode_xfield_field(_el) | Fields]);
      _ -> decode_xdata_x_reported_els(_els, Fields)
    end;
decode_xdata_x_reported_els([_ | _els], Fields) ->
    decode_xdata_x_reported_els(_els, Fields);
decode_xdata_x_reported_els([], Fields) ->
    lists:reverse(Fields).

encode_xdata_x_reported(undefined, _acc) -> _acc;
encode_xdata_x_reported(Fields, _acc) ->
    _els = encode_xfield_field(Fields, []),
    _attrs = [],
    [{xmlel, <<"reported">>, _attrs, _els} | _acc].

decode_xdata_x_title({xmlel, _, _attrs, _els}) ->
    Cdata = decode_xdata_x_title_els(_els, <<>>), Cdata.

decode_xdata_x_title_els([{xmlcdata, _data} | _els],
			 Cdata) ->
    decode_xdata_x_title_els(_els,
			     <<Cdata/binary, _data/binary>>);
decode_xdata_x_title_els([_ | _els], Cdata) ->
    decode_xdata_x_title_els(_els, Cdata);
decode_xdata_x_title_els([], Cdata) ->
    decode_xdata_x_title_cdata(Cdata).

encode_xdata_x_title(undefined, _acc) -> _acc;
encode_xdata_x_title(Cdata, _acc) ->
    _els = encode_xdata_x_title_cdata(Cdata, []),
    _attrs = [],
    [{xmlel, <<"title">>, _attrs, _els} | _acc].

decode_xdata_x_title_cdata(<<>>) -> undefined;
decode_xdata_x_title_cdata(_val) -> _val.

encode_xdata_x_title_cdata(undefined, _acc) -> _acc;
encode_xdata_x_title_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_xdata_x_instructions({xmlel, _, _attrs, _els}) ->
    Cdata = decode_xdata_x_instructions_els(_els, <<>>),
    Cdata.

decode_xdata_x_instructions_els([{xmlcdata, _data}
				 | _els],
				Cdata) ->
    decode_xdata_x_instructions_els(_els,
				    <<Cdata/binary, _data/binary>>);
decode_xdata_x_instructions_els([_ | _els], Cdata) ->
    decode_xdata_x_instructions_els(_els, Cdata);
decode_xdata_x_instructions_els([], Cdata) ->
    decode_xdata_x_instructions_cdata(Cdata).

encode_xdata_x_instructions([], _acc) -> _acc;
encode_xdata_x_instructions([Cdata | _tail], _acc) ->
    _els = encode_xdata_x_instructions_cdata(Cdata, []),
    _attrs = [],
    encode_xdata_x_instructions(_tail,
				[{xmlel, <<"instructions">>, _attrs, _els}
				 | _acc]).

decode_xdata_x_instructions_cdata(<<>>) -> undefined;
decode_xdata_x_instructions_cdata(_val) -> _val.

encode_xdata_x_instructions_cdata(undefined, _acc) ->
    _acc;
encode_xdata_x_instructions_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_pubsub_subscription_subscription({xmlel, _,
					 _attrs, _els}) ->
    {Type, Subid, Node, Jid} =
	decode_pubsub_subscription_subscription_attrs(_attrs,
						      undefined, undefined,
						      undefined, undefined),
    {pubsub_subscription, Jid, Node, Subid, Type}.

decode_pubsub_subscription_subscription_attrs([{<<"subscription">>,
						_val}
					       | _attrs],
					      _Type, Subid, Node, Jid) ->
    decode_pubsub_subscription_subscription_attrs(_attrs,
						  _val, Subid, Node, Jid);
decode_pubsub_subscription_subscription_attrs([{<<"subid">>,
						_val}
					       | _attrs],
					      Type, _Subid, Node, Jid) ->
    decode_pubsub_subscription_subscription_attrs(_attrs,
						  Type, _val, Node, Jid);
decode_pubsub_subscription_subscription_attrs([{<<"node">>,
						_val}
					       | _attrs],
					      Type, Subid, _Node, Jid) ->
    decode_pubsub_subscription_subscription_attrs(_attrs,
						  Type, Subid, _val, Jid);
decode_pubsub_subscription_subscription_attrs([{<<"jid">>,
						_val}
					       | _attrs],
					      Type, Subid, Node, _Jid) ->
    decode_pubsub_subscription_subscription_attrs(_attrs,
						  Type, Subid, Node, _val);
decode_pubsub_subscription_subscription_attrs([_
					       | _attrs],
					      Type, Subid, Node, Jid) ->
    decode_pubsub_subscription_subscription_attrs(_attrs,
						  Type, Subid, Node, Jid);
decode_pubsub_subscription_subscription_attrs([], Type,
					      Subid, Node, Jid) ->
    {decode_pubsub_subscription_subscription_subscription(Type),
     decode_pubsub_subscription_subscription_subid(Subid),
     decode_pubsub_subscription_subscription_node(Node),
     decode_pubsub_subscription_subscription_jid(Jid)}.

encode_pubsub_subscription_subscription([], _acc) ->
    _acc;
encode_pubsub_subscription_subscription([{pubsub_subscription,
					  Jid, Node, Subid, Type}
					 | _tail],
					_acc) ->
    _els = [],
    _attrs =
	encode_pubsub_subscription_subscription_jid(Jid,
						    encode_pubsub_subscription_subscription_node(Node,
												 encode_pubsub_subscription_subscription_subid(Subid,
																	       encode_pubsub_subscription_subscription_subscription(Type,
																								    [])))),
    encode_pubsub_subscription_subscription(_tail,
					    [{xmlel, <<"subscription">>, _attrs,
					      _els}
					     | _acc]).

decode_pubsub_subscription_subscription_jid(undefined) ->
    erlang:error({missing_attr, <<"jid">>,
		  <<"subscription">>, <<>>});
decode_pubsub_subscription_subscription_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"jid">>,
			<<"subscription">>, <<>>});
      _res -> _res
    end.

encode_pubsub_subscription_subscription_jid(_val,
					    _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_pubsub_subscription_subscription_node(undefined) ->
    undefined;
decode_pubsub_subscription_subscription_node(_val) ->
    _val.

encode_pubsub_subscription_subscription_node(undefined,
					     _acc) ->
    _acc;
encode_pubsub_subscription_subscription_node(_val,
					     _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_subscription_subscription_subid(undefined) ->
    undefined;
decode_pubsub_subscription_subscription_subid(_val) ->
    _val.

encode_pubsub_subscription_subscription_subid(undefined,
					      _acc) ->
    _acc;
encode_pubsub_subscription_subscription_subid(_val,
					      _acc) ->
    [{<<"subid">>, _val} | _acc].

decode_pubsub_subscription_subscription_subscription(undefined) ->
    undefined;
decode_pubsub_subscription_subscription_subscription(_val) ->
    case catch xml_gen:dec_enum(_val,
				[none, pending, subscribed, unconfigured])
	of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"subscription">>,
			<<"subscription">>, <<>>});
      _res -> _res
    end.

encode_pubsub_subscription_subscription_subscription(undefined,
						     _acc) ->
    _acc;
encode_pubsub_subscription_subscription_subscription(_val,
						     _acc) ->
    [{<<"subscription">>, xml_gen:enc_enum(_val)} | _acc].

decode_pubsub_affiliation_affiliation({xmlel, _, _attrs,
				       _els}) ->
    {Type, Node} =
	decode_pubsub_affiliation_affiliation_attrs(_attrs,
						    undefined, undefined),
    {pubsub_affiliation, Node, Type}.

decode_pubsub_affiliation_affiliation_attrs([{<<"affiliation">>,
					      _val}
					     | _attrs],
					    _Type, Node) ->
    decode_pubsub_affiliation_affiliation_attrs(_attrs,
						_val, Node);
decode_pubsub_affiliation_affiliation_attrs([{<<"node">>,
					      _val}
					     | _attrs],
					    Type, _Node) ->
    decode_pubsub_affiliation_affiliation_attrs(_attrs,
						Type, _val);
decode_pubsub_affiliation_affiliation_attrs([_
					     | _attrs],
					    Type, Node) ->
    decode_pubsub_affiliation_affiliation_attrs(_attrs,
						Type, Node);
decode_pubsub_affiliation_affiliation_attrs([], Type,
					    Node) ->
    {decode_pubsub_affiliation_affiliation_affiliation(Type),
     decode_pubsub_affiliation_affiliation_node(Node)}.

encode_pubsub_affiliation_affiliation([], _acc) -> _acc;
encode_pubsub_affiliation_affiliation([{pubsub_affiliation,
					Node, Type}
				       | _tail],
				      _acc) ->
    _els = [],
    _attrs =
	encode_pubsub_affiliation_affiliation_node(Node,
						   encode_pubsub_affiliation_affiliation_affiliation(Type,
												     [])),
    encode_pubsub_affiliation_affiliation(_tail,
					  [{xmlel, <<"affiliation">>, _attrs,
					    _els}
					   | _acc]).

decode_pubsub_affiliation_affiliation_node(undefined) ->
    erlang:error({missing_attr, <<"node">>,
		  <<"affiliation">>, <<>>});
decode_pubsub_affiliation_affiliation_node(_val) ->
    _val.

encode_pubsub_affiliation_affiliation_node(_val,
					   _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_affiliation_affiliation_affiliation(undefined) ->
    erlang:error({missing_attr, <<"affiliation">>,
		  <<"affiliation">>, <<>>});
decode_pubsub_affiliation_affiliation_affiliation(_val) ->
    case catch xml_gen:dec_enum(_val,
				[member, none, outcast, owner, publisher,
				 'publish-only'])
	of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"affiliation">>,
			<<"affiliation">>, <<>>});
      _res -> _res
    end.

encode_pubsub_affiliation_affiliation_affiliation(_val,
						  _acc) ->
    [{<<"affiliation">>, xml_gen:enc_enum(_val)} | _acc].

decode_pubsub_item_item({xmlel, _, _attrs, _els}) ->
    Id = decode_pubsub_item_item_attrs(_attrs, undefined),
    __Els = decode_pubsub_item_item_els(_els, []),
    {pubsub_item, Id, __Els}.

decode_pubsub_item_item_els([{xmlel, _, _, _} = _el
			     | _els],
			    __Els) ->
    decode_pubsub_item_item_els(_els,
				[decode(_el) | __Els]);
decode_pubsub_item_item_els([_ | _els], __Els) ->
    decode_pubsub_item_item_els(_els, __Els);
decode_pubsub_item_item_els([], __Els) ->
    lists:reverse(__Els).

decode_pubsub_item_item_attrs([{<<"id">>, _val}
			       | _attrs],
			      _Id) ->
    decode_pubsub_item_item_attrs(_attrs, _val);
decode_pubsub_item_item_attrs([_ | _attrs], Id) ->
    decode_pubsub_item_item_attrs(_attrs, Id);
decode_pubsub_item_item_attrs([], Id) ->
    decode_pubsub_item_item_id(Id).

encode_pubsub_item_item([], _acc) -> _acc;
encode_pubsub_item_item([{pubsub_item, Id, __Els}
			 | _tail],
			_acc) ->
    _els = [encode(_subel) || _subel <- __Els] ++ [],
    _attrs = encode_pubsub_item_item_id(Id, []),
    encode_pubsub_item_item(_tail,
			    [{xmlel, <<"item">>, _attrs, _els} | _acc]).

decode_pubsub_item_item_id(undefined) -> undefined;
decode_pubsub_item_item_id(_val) -> _val.

encode_pubsub_item_item_id(undefined, _acc) -> _acc;
encode_pubsub_item_item_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_pubsub_items_items({xmlel, _, _attrs, _els}) ->
    {Subid, Max_items, Node} =
	decode_pubsub_items_items_attrs(_attrs, undefined,
					undefined, undefined),
    Item = decode_pubsub_items_items_els(_els, []),
    {pubsub_items, Node, Max_items, Subid, Item}.

decode_pubsub_items_items_els([{xmlel, <<"item">>,
				_attrs, _} =
				   _el
			       | _els],
			      Item) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_pubsub_items_items_els(_els,
					[decode_pubsub_item_item(_el) | Item]);
      _ -> decode_pubsub_items_items_els(_els, Item)
    end;
decode_pubsub_items_items_els([_ | _els], Item) ->
    decode_pubsub_items_items_els(_els, Item);
decode_pubsub_items_items_els([], Item) ->
    lists:reverse(Item).

decode_pubsub_items_items_attrs([{<<"subid">>, _val}
				 | _attrs],
				_Subid, Max_items, Node) ->
    decode_pubsub_items_items_attrs(_attrs, _val, Max_items,
				    Node);
decode_pubsub_items_items_attrs([{<<"max_items">>, _val}
				 | _attrs],
				Subid, _Max_items, Node) ->
    decode_pubsub_items_items_attrs(_attrs, Subid, _val,
				    Node);
decode_pubsub_items_items_attrs([{<<"node">>, _val}
				 | _attrs],
				Subid, Max_items, _Node) ->
    decode_pubsub_items_items_attrs(_attrs, Subid,
				    Max_items, _val);
decode_pubsub_items_items_attrs([_ | _attrs], Subid,
				Max_items, Node) ->
    decode_pubsub_items_items_attrs(_attrs, Subid,
				    Max_items, Node);
decode_pubsub_items_items_attrs([], Subid, Max_items,
				Node) ->
    {decode_pubsub_items_items_subid(Subid),
     decode_pubsub_items_items_max_items(Max_items),
     decode_pubsub_items_items_node(Node)}.

encode_pubsub_items_items([], _acc) -> _acc;
encode_pubsub_items_items([{pubsub_items, Node,
			    Max_items, Subid, Item}
			   | _tail],
			  _acc) ->
    _els = encode_pubsub_item_item(Item, []),
    _attrs = encode_pubsub_items_items_node(Node,
					    encode_pubsub_items_items_max_items(Max_items,
										encode_pubsub_items_items_subid(Subid,
														[]))),
    encode_pubsub_items_items(_tail,
			      [{xmlel, <<"items">>, _attrs, _els} | _acc]).

decode_pubsub_items_items_max_items(undefined) ->
    undefined;
decode_pubsub_items_items_max_items(_val) ->
    case catch xml_gen:dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"max_items">>,
			<<"items">>, <<>>});
      _res -> _res
    end.

encode_pubsub_items_items_max_items(undefined, _acc) ->
    _acc;
encode_pubsub_items_items_max_items(_val, _acc) ->
    [{<<"max_items">>, xml_gen:enc_int(_val)} | _acc].

decode_pubsub_items_items_node(undefined) ->
    erlang:error({missing_attr, <<"node">>, <<"items">>,
		  <<>>});
decode_pubsub_items_items_node(_val) -> _val.

encode_pubsub_items_items_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_items_items_subid(undefined) -> undefined;
decode_pubsub_items_items_subid(_val) -> _val.

encode_pubsub_items_items_subid(undefined, _acc) ->
    _acc;
encode_pubsub_items_items_subid(_val, _acc) ->
    [{<<"subid">>, _val} | _acc].

decode_pubsub_event_event({xmlel, _, _attrs, _els}) ->
    Items = decode_pubsub_event_event_els(_els, []),
    {pubsub_event, Items}.

decode_pubsub_event_event_els([{xmlel, <<"items">>,
				_attrs, _} =
				   _el
			       | _els],
			      Items) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_pubsub_event_event_els(_els,
					[decode_pubsub_items_items(_el)
					 | Items]);
      _ -> decode_pubsub_event_event_els(_els, Items)
    end;
decode_pubsub_event_event_els([_ | _els], Items) ->
    decode_pubsub_event_event_els(_els, Items);
decode_pubsub_event_event_els([], Items) ->
    lists:reverse(Items).

encode_pubsub_event_event(undefined, _acc) -> _acc;
encode_pubsub_event_event({pubsub_event, Items},
			  _acc) ->
    _els = encode_pubsub_items_items(Items, []),
    _attrs = [{<<"xmlns">>,
	       <<"http://jabber.org/protocol/pubsub#event">>}],
    [{xmlel, <<"event">>, _attrs, _els} | _acc].

decode_pubsub_pubsub({xmlel, _, _attrs, _els}) ->
    {Subscribe, Publish, Affiliations, Subscriptions} =
	decode_pubsub_pubsub_els(_els, undefined, undefined,
				 undefined, undefined),
    {pubsub, Subscriptions, Affiliations, Publish,
     Subscribe}.

decode_pubsub_pubsub_els([{xmlel, <<"subscribe">>,
			   _attrs, _} =
			      _el
			  | _els],
			 Subscribe, Publish, Affiliations, Subscriptions) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_pubsub_pubsub_els(_els,
				   decode_pubsub_pubsub_subscribe(_el), Publish,
				   Affiliations, Subscriptions);
      _ ->
	  decode_pubsub_pubsub_els(_els, Subscribe, Publish,
				   Affiliations, Subscriptions)
    end;
decode_pubsub_pubsub_els([{xmlel, <<"publish">>, _attrs,
			   _} =
			      _el
			  | _els],
			 Subscribe, Publish, Affiliations, Subscriptions) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_pubsub_pubsub_els(_els, Subscribe,
				   decode_pubsub_pubsub_publish(_el),
				   Affiliations, Subscriptions);
      _ ->
	  decode_pubsub_pubsub_els(_els, Subscribe, Publish,
				   Affiliations, Subscriptions)
    end;
decode_pubsub_pubsub_els([{xmlel, <<"affiliations">>,
			   _attrs, _} =
			      _el
			  | _els],
			 Subscribe, Publish, Affiliations, Subscriptions) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_pubsub_pubsub_els(_els, Subscribe, Publish,
				   decode_pubsub_pubsub_affiliations(_el),
				   Subscriptions);
      _ ->
	  decode_pubsub_pubsub_els(_els, Subscribe, Publish,
				   Affiliations, Subscriptions)
    end;
decode_pubsub_pubsub_els([{xmlel, <<"subscriptions">>,
			   _attrs, _} =
			      _el
			  | _els],
			 Subscribe, Publish, Affiliations, Subscriptions) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_pubsub_pubsub_els(_els, Subscribe, Publish,
				   Affiliations,
				   decode_pubsub_pubsub_subscriptions(_el));
      _ ->
	  decode_pubsub_pubsub_els(_els, Subscribe, Publish,
				   Affiliations, Subscriptions)
    end;
decode_pubsub_pubsub_els([_ | _els], Subscribe, Publish,
			 Affiliations, Subscriptions) ->
    decode_pubsub_pubsub_els(_els, Subscribe, Publish,
			     Affiliations, Subscriptions);
decode_pubsub_pubsub_els([], Subscribe, Publish,
			 Affiliations, Subscriptions) ->
    {Subscribe, Publish, Affiliations, Subscriptions}.

encode_pubsub_pubsub(undefined, _acc) -> _acc;
encode_pubsub_pubsub({pubsub, Subscriptions,
		      Affiliations, Publish, Subscribe},
		     _acc) ->
    _els = encode_pubsub_pubsub_subscriptions(Subscriptions,
					      encode_pubsub_pubsub_affiliations(Affiliations,
										encode_pubsub_pubsub_publish(Publish,
													     encode_pubsub_pubsub_subscribe(Subscribe,
																	    [])))),
    _attrs = [{<<"xmlns">>,
	       <<"http://jabber.org/protocol/pubsub">>}],
    [{xmlel, <<"pubsub">>, _attrs, _els} | _acc].

decode_pubsub_pubsub_publish({xmlel, _, _attrs,
			      _els}) ->
    Node = decode_pubsub_pubsub_publish_attrs(_attrs,
					      undefined),
    Item = decode_pubsub_pubsub_publish_els(_els, []),
    {Node, Item}.

decode_pubsub_pubsub_publish_els([{xmlel, <<"item">>,
				   _attrs, _} =
				      _el
				  | _els],
				 Item) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_pubsub_pubsub_publish_els(_els,
					   [decode_pubsub_item_item(_el)
					    | Item]);
      _ -> decode_pubsub_pubsub_publish_els(_els, Item)
    end;
decode_pubsub_pubsub_publish_els([_ | _els], Item) ->
    decode_pubsub_pubsub_publish_els(_els, Item);
decode_pubsub_pubsub_publish_els([], Item) ->
    lists:reverse(Item).

decode_pubsub_pubsub_publish_attrs([{<<"node">>, _val}
				    | _attrs],
				   _Node) ->
    decode_pubsub_pubsub_publish_attrs(_attrs, _val);
decode_pubsub_pubsub_publish_attrs([_ | _attrs],
				   Node) ->
    decode_pubsub_pubsub_publish_attrs(_attrs, Node);
decode_pubsub_pubsub_publish_attrs([], Node) ->
    decode_pubsub_pubsub_publish_node(Node).

encode_pubsub_pubsub_publish(undefined, _acc) -> _acc;
encode_pubsub_pubsub_publish({Node, Item}, _acc) ->
    _els = encode_pubsub_item_item(Item, []),
    _attrs = encode_pubsub_pubsub_publish_node(Node, []),
    [{xmlel, <<"publish">>, _attrs, _els} | _acc].

decode_pubsub_pubsub_publish_node(undefined) ->
    erlang:error({missing_attr, <<"node">>, <<"publish">>,
		  <<>>});
decode_pubsub_pubsub_publish_node(_val) -> _val.

encode_pubsub_pubsub_publish_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_pubsub_subscribe({xmlel, _, _attrs,
				_els}) ->
    {Jid, Node} =
	decode_pubsub_pubsub_subscribe_attrs(_attrs, undefined,
					     undefined),
    {Node, Jid}.

decode_pubsub_pubsub_subscribe_attrs([{<<"jid">>, _val}
				      | _attrs],
				     _Jid, Node) ->
    decode_pubsub_pubsub_subscribe_attrs(_attrs, _val,
					 Node);
decode_pubsub_pubsub_subscribe_attrs([{<<"node">>, _val}
				      | _attrs],
				     Jid, _Node) ->
    decode_pubsub_pubsub_subscribe_attrs(_attrs, Jid, _val);
decode_pubsub_pubsub_subscribe_attrs([_ | _attrs], Jid,
				     Node) ->
    decode_pubsub_pubsub_subscribe_attrs(_attrs, Jid, Node);
decode_pubsub_pubsub_subscribe_attrs([], Jid, Node) ->
    {decode_pubsub_pubsub_subscribe_jid(Jid),
     decode_pubsub_pubsub_subscribe_node(Node)}.

encode_pubsub_pubsub_subscribe(undefined, _acc) -> _acc;
encode_pubsub_pubsub_subscribe({Node, Jid}, _acc) ->
    _els = [],
    _attrs = encode_pubsub_pubsub_subscribe_node(Node,
						 encode_pubsub_pubsub_subscribe_jid(Jid,
										    [])),
    [{xmlel, <<"subscribe">>, _attrs, _els} | _acc].

decode_pubsub_pubsub_subscribe_node(undefined) ->
    undefined;
decode_pubsub_pubsub_subscribe_node(_val) -> _val.

encode_pubsub_pubsub_subscribe_node(undefined, _acc) ->
    _acc;
encode_pubsub_pubsub_subscribe_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_pubsub_subscribe_jid(undefined) ->
    erlang:error({missing_attr, <<"jid">>, <<"subscribe">>,
		  <<>>});
decode_pubsub_pubsub_subscribe_jid(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"jid">>,
			<<"subscribe">>, <<>>});
      _res -> _res
    end.

encode_pubsub_pubsub_subscribe_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_pubsub_pubsub_affiliations({xmlel, _, _attrs,
				   _els}) ->
    Pubsub_affiliations =
	decode_pubsub_pubsub_affiliations_els(_els, []),
    Pubsub_affiliations.

decode_pubsub_pubsub_affiliations_els([{xmlel,
					<<"affiliation">>, _attrs, _} =
					   _el
				       | _els],
				      Pubsub_affiliations) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_pubsub_pubsub_affiliations_els(_els,
						[decode_pubsub_affiliation_affiliation(_el)
						 | Pubsub_affiliations]);
      _ ->
	  decode_pubsub_pubsub_affiliations_els(_els,
						Pubsub_affiliations)
    end;
decode_pubsub_pubsub_affiliations_els([_ | _els],
				      Pubsub_affiliations) ->
    decode_pubsub_pubsub_affiliations_els(_els,
					  Pubsub_affiliations);
decode_pubsub_pubsub_affiliations_els([],
				      Pubsub_affiliations) ->
    lists:reverse(Pubsub_affiliations).

encode_pubsub_pubsub_affiliations(undefined, _acc) ->
    _acc;
encode_pubsub_pubsub_affiliations(Pubsub_affiliations,
				  _acc) ->
    _els =
	encode_pubsub_affiliation_affiliation(Pubsub_affiliations,
					      []),
    _attrs = [],
    [{xmlel, <<"affiliations">>, _attrs, _els} | _acc].

decode_pubsub_pubsub_subscriptions({xmlel, _, _attrs,
				    _els}) ->
    Node = decode_pubsub_pubsub_subscriptions_attrs(_attrs,
						    undefined),
    Pubsub_subscriptions =
	decode_pubsub_pubsub_subscriptions_els(_els, []),
    {Node, Pubsub_subscriptions}.

decode_pubsub_pubsub_subscriptions_els([{xmlel,
					 <<"subscription">>, _attrs, _} =
					    _el
					| _els],
				       Pubsub_subscriptions) ->
    case xml:get_attr_s(<<"xmlns">>, _attrs) of
      <<>> ->
	  decode_pubsub_pubsub_subscriptions_els(_els,
						 [decode_pubsub_subscription_subscription(_el)
						  | Pubsub_subscriptions]);
      _ ->
	  decode_pubsub_pubsub_subscriptions_els(_els,
						 Pubsub_subscriptions)
    end;
decode_pubsub_pubsub_subscriptions_els([_ | _els],
				       Pubsub_subscriptions) ->
    decode_pubsub_pubsub_subscriptions_els(_els,
					   Pubsub_subscriptions);
decode_pubsub_pubsub_subscriptions_els([],
				       Pubsub_subscriptions) ->
    lists:reverse(Pubsub_subscriptions).

decode_pubsub_pubsub_subscriptions_attrs([{<<"node">>,
					   _val}
					  | _attrs],
					 _Node) ->
    decode_pubsub_pubsub_subscriptions_attrs(_attrs, _val);
decode_pubsub_pubsub_subscriptions_attrs([_ | _attrs],
					 Node) ->
    decode_pubsub_pubsub_subscriptions_attrs(_attrs, Node);
decode_pubsub_pubsub_subscriptions_attrs([], Node) ->
    decode_pubsub_pubsub_subscriptions_node(Node).

encode_pubsub_pubsub_subscriptions(undefined, _acc) ->
    _acc;
encode_pubsub_pubsub_subscriptions({Node,
				    Pubsub_subscriptions},
				   _acc) ->
    _els =
	encode_pubsub_subscription_subscription(Pubsub_subscriptions,
						[]),
    _attrs = encode_pubsub_pubsub_subscriptions_node(Node,
						     []),
    [{xmlel, <<"subscriptions">>, _attrs, _els} | _acc].

decode_pubsub_pubsub_subscriptions_node(undefined) ->
    none;
decode_pubsub_pubsub_subscriptions_node(_val) -> _val.

encode_pubsub_pubsub_subscriptions_node(none, _acc) ->
    _acc;
encode_pubsub_pubsub_subscriptions_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_delay_delay({xmlel, _, _attrs, _els}) ->
    {From, Stamp} = decode_delay_delay_attrs(_attrs,
					     undefined, undefined),
    {delay, Stamp, From}.

decode_delay_delay_attrs([{<<"from">>, _val} | _attrs],
			 _From, Stamp) ->
    decode_delay_delay_attrs(_attrs, _val, Stamp);
decode_delay_delay_attrs([{<<"stamp">>, _val} | _attrs],
			 From, _Stamp) ->
    decode_delay_delay_attrs(_attrs, From, _val);
decode_delay_delay_attrs([_ | _attrs], From, Stamp) ->
    decode_delay_delay_attrs(_attrs, From, Stamp);
decode_delay_delay_attrs([], From, Stamp) ->
    {decode_delay_delay_from(From),
     decode_delay_delay_stamp(Stamp)}.

encode_delay_delay(undefined, _acc) -> _acc;
encode_delay_delay({delay, Stamp, From}, _acc) ->
    _els = [],
    _attrs = encode_delay_delay_stamp(Stamp,
				      encode_delay_delay_from(From,
							      [{<<"xmlns">>,
								<<"urn:xmpp:delay">>}])),
    [{xmlel, <<"delay">>, _attrs, _els} | _acc].

decode_delay_delay_stamp(undefined) ->
    erlang:error({missing_attr, <<"stamp">>, <<"delay">>,
		  <<"urn:xmpp:delay">>});
decode_delay_delay_stamp(_val) ->
    case catch dec_utc(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"stamp">>, <<"delay">>,
			<<"urn:xmpp:delay">>});
      _res -> _res
    end.

encode_delay_delay_stamp(_val, _acc) ->
    [{<<"stamp">>, enc_utc(_val)} | _acc].

decode_delay_delay_from(undefined) -> undefined;
decode_delay_delay_from(_val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({bad_attr_value, <<"from">>, <<"delay">>,
			<<"urn:xmpp:delay">>});
      _res -> _res
    end.

encode_delay_delay_from(undefined, _acc) -> _acc;
encode_delay_delay_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].
