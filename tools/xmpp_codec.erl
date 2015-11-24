%% Created automatically by XML generator (xml_gen.erl)
%% Source: xmpp_codec.spec

-module(xmpp_codec).

-compile({nowarn_unused_function,
	  [{dec_int, 3}, {dec_int, 1}, {dec_enum, 2},
	   {enc_int, 1}, {get_attr, 2}, {enc_enum, 1}]}).

-export([pp/1, format_error/1, decode/1, decode/2,
	 is_known_tag/1, encode/1, get_ns/1]).

decode(_el) -> decode(_el, []).

decode({xmlel, _name, _attrs, _} = _el, Opts) ->
    IgnoreEls = proplists:get_bool(ignore_els, Opts),
    case {_name, get_attr(<<"xmlns">>, _attrs)} of
      {<<"failed">>, <<"urn:xmpp:sm:2">>} ->
	  decode_sm_failed(<<"urn:xmpp:sm:2">>, IgnoreEls, _el);
      {<<"failed">>, <<"urn:xmpp:sm:3">>} ->
	  decode_sm_failed(<<"urn:xmpp:sm:3">>, IgnoreEls, _el);
      {<<"a">>, <<"urn:xmpp:sm:2">>} ->
	  decode_sm_a(<<"urn:xmpp:sm:2">>, IgnoreEls, _el);
      {<<"a">>, <<"urn:xmpp:sm:3">>} ->
	  decode_sm_a(<<"urn:xmpp:sm:3">>, IgnoreEls, _el);
      {<<"r">>, <<"urn:xmpp:sm:2">>} ->
	  decode_sm_r(<<"urn:xmpp:sm:2">>, IgnoreEls, _el);
      {<<"r">>, <<"urn:xmpp:sm:3">>} ->
	  decode_sm_r(<<"urn:xmpp:sm:3">>, IgnoreEls, _el);
      {<<"resumed">>, <<"urn:xmpp:sm:2">>} ->
	  decode_sm_resumed(<<"urn:xmpp:sm:2">>, IgnoreEls, _el);
      {<<"resumed">>, <<"urn:xmpp:sm:3">>} ->
	  decode_sm_resumed(<<"urn:xmpp:sm:3">>, IgnoreEls, _el);
      {<<"resume">>, <<"urn:xmpp:sm:2">>} ->
	  decode_sm_resume(<<"urn:xmpp:sm:2">>, IgnoreEls, _el);
      {<<"resume">>, <<"urn:xmpp:sm:3">>} ->
	  decode_sm_resume(<<"urn:xmpp:sm:3">>, IgnoreEls, _el);
      {<<"enabled">>, <<"urn:xmpp:sm:2">>} ->
	  decode_sm_enabled(<<"urn:xmpp:sm:2">>, IgnoreEls, _el);
      {<<"enabled">>, <<"urn:xmpp:sm:3">>} ->
	  decode_sm_enabled(<<"urn:xmpp:sm:3">>, IgnoreEls, _el);
      {<<"enable">>, <<"urn:xmpp:sm:2">>} ->
	  decode_sm_enable(<<"urn:xmpp:sm:2">>, IgnoreEls, _el);
      {<<"enable">>, <<"urn:xmpp:sm:3">>} ->
	  decode_sm_enable(<<"urn:xmpp:sm:3">>, IgnoreEls, _el);
      {<<"sm">>, <<"urn:xmpp:sm:2">>} ->
	  decode_feature_sm(<<"urn:xmpp:sm:2">>, IgnoreEls, _el);
      {<<"sm">>, <<"urn:xmpp:sm:3">>} ->
	  decode_feature_sm(<<"urn:xmpp:sm:3">>, IgnoreEls, _el);
      {<<"inactive">>, <<"urn:xmpp:csi:0">>} ->
	  decode_csi_inactive(<<"urn:xmpp:csi:0">>, IgnoreEls,
			      _el);
      {<<"active">>, <<"urn:xmpp:csi:0">>} ->
	  decode_csi_active(<<"urn:xmpp:csi:0">>, IgnoreEls, _el);
      {<<"csi">>, <<"urn:xmpp:csi:0">>} ->
	  decode_feature_csi(<<"urn:xmpp:csi:0">>, IgnoreEls,
			     _el);
      {<<"sent">>, <<"urn:xmpp:carbons:2">>} ->
	  decode_carbons_sent(<<"urn:xmpp:carbons:2">>, IgnoreEls,
			      _el);
      {<<"received">>, <<"urn:xmpp:carbons:2">>} ->
	  decode_carbons_received(<<"urn:xmpp:carbons:2">>,
				  IgnoreEls, _el);
      {<<"private">>, <<"urn:xmpp:carbons:2">>} ->
	  decode_carbons_private(<<"urn:xmpp:carbons:2">>,
				 IgnoreEls, _el);
      {<<"enable">>, <<"urn:xmpp:carbons:2">>} ->
	  decode_carbons_enable(<<"urn:xmpp:carbons:2">>,
				IgnoreEls, _el);
      {<<"disable">>, <<"urn:xmpp:carbons:2">>} ->
	  decode_carbons_disable(<<"urn:xmpp:carbons:2">>,
				 IgnoreEls, _el);
      {<<"forwarded">>, <<"urn:xmpp:forward:0">>} ->
	  decode_forwarded(<<"urn:xmpp:forward:0">>, IgnoreEls,
			   _el);
      {<<"fin">>, <<"urn:xmpp:mam:0">>} ->
	  decode_mam_fin(<<"urn:xmpp:mam:0">>, IgnoreEls, _el);
      {<<"prefs">>, <<"urn:xmpp:mam:0">>} ->
	  decode_mam_prefs(<<"urn:xmpp:mam:0">>, IgnoreEls, _el);
      {<<"prefs">>, <<"urn:xmpp:mam:tmp">>} ->
	  decode_mam_prefs(<<"urn:xmpp:mam:tmp">>, IgnoreEls,
			   _el);
      {<<"always">>, <<"urn:xmpp:mam:tmp">>} ->
	  decode_mam_always(<<"urn:xmpp:mam:tmp">>, IgnoreEls,
			    _el);
      {<<"never">>, <<"urn:xmpp:mam:tmp">>} ->
	  decode_mam_never(<<"urn:xmpp:mam:tmp">>, IgnoreEls,
			   _el);
      {<<"jid">>, <<"urn:xmpp:mam:tmp">>} ->
	  decode_mam_jid(<<"urn:xmpp:mam:tmp">>, IgnoreEls, _el);
      {<<"result">>, <<"urn:xmpp:mam:0">>} ->
	  decode_mam_result(<<"urn:xmpp:mam:0">>, IgnoreEls, _el);
      {<<"result">>, <<"urn:xmpp:mam:tmp">>} ->
	  decode_mam_result(<<"urn:xmpp:mam:tmp">>, IgnoreEls,
			    _el);
      {<<"archived">>, <<"urn:xmpp:mam:tmp">>} ->
	  decode_mam_archived(<<"urn:xmpp:mam:tmp">>, IgnoreEls,
			      _el);
      {<<"query">>, <<"urn:xmpp:mam:0">>} ->
	  decode_mam_query(<<"urn:xmpp:mam:0">>, IgnoreEls, _el);
      {<<"query">>, <<"urn:xmpp:mam:tmp">>} ->
	  decode_mam_query(<<"urn:xmpp:mam:tmp">>, IgnoreEls,
			   _el);
      {<<"with">>, <<"urn:xmpp:mam:tmp">>} ->
	  decode_mam_with(<<"urn:xmpp:mam:tmp">>, IgnoreEls, _el);
      {<<"end">>, <<"urn:xmpp:mam:tmp">>} ->
	  decode_mam_end(<<"urn:xmpp:mam:tmp">>, IgnoreEls, _el);
      {<<"start">>, <<"urn:xmpp:mam:tmp">>} ->
	  decode_mam_start(<<"urn:xmpp:mam:tmp">>, IgnoreEls,
			   _el);
      {<<"set">>, <<"http://jabber.org/protocol/rsm">>} ->
	  decode_rsm_set(<<"http://jabber.org/protocol/rsm">>,
			 IgnoreEls, _el);
      {<<"first">>, <<"http://jabber.org/protocol/rsm">>} ->
	  decode_rsm_first(<<"http://jabber.org/protocol/rsm">>,
			   IgnoreEls, _el);
      {<<"max">>, <<"http://jabber.org/protocol/rsm">>} ->
	  decode_rsm_max(<<"http://jabber.org/protocol/rsm">>,
			 IgnoreEls, _el);
      {<<"index">>, <<"http://jabber.org/protocol/rsm">>} ->
	  decode_rsm_index(<<"http://jabber.org/protocol/rsm">>,
			   IgnoreEls, _el);
      {<<"count">>, <<"http://jabber.org/protocol/rsm">>} ->
	  decode_rsm_count(<<"http://jabber.org/protocol/rsm">>,
			   IgnoreEls, _el);
      {<<"last">>, <<"http://jabber.org/protocol/rsm">>} ->
	  decode_rsm_last(<<"http://jabber.org/protocol/rsm">>,
			  IgnoreEls, _el);
      {<<"before">>, <<"http://jabber.org/protocol/rsm">>} ->
	  decode_rsm_before(<<"http://jabber.org/protocol/rsm">>,
			    IgnoreEls, _el);
      {<<"after">>, <<"http://jabber.org/protocol/rsm">>} ->
	  decode_rsm_after(<<"http://jabber.org/protocol/rsm">>,
			   IgnoreEls, _el);
      {<<"x">>, <<"http://jabber.org/protocol/muc">>} ->
	  decode_muc(<<"http://jabber.org/protocol/muc">>,
		     IgnoreEls, _el);
      {<<"query">>,
       <<"http://jabber.org/protocol/muc#admin">>} ->
	  decode_muc_admin(<<"http://jabber.org/protocol/muc#admin">>,
			   IgnoreEls, _el);
      {<<"reason">>,
       <<"http://jabber.org/protocol/muc#admin">>} ->
	  decode_muc_admin_reason(<<"http://jabber.org/protocol/muc#admin">>,
				  IgnoreEls, _el);
      {<<"continue">>,
       <<"http://jabber.org/protocol/muc#admin">>} ->
	  decode_muc_admin_continue(<<"http://jabber.org/protocol/muc#admin">>,
				    IgnoreEls, _el);
      {<<"actor">>,
       <<"http://jabber.org/protocol/muc#admin">>} ->
	  decode_muc_admin_actor(<<"http://jabber.org/protocol/muc#admin">>,
				 IgnoreEls, _el);
      {<<"item">>,
       <<"http://jabber.org/protocol/muc#admin">>} ->
	  decode_muc_admin_item(<<"http://jabber.org/protocol/muc#admin">>,
				IgnoreEls, _el);
      {<<"query">>,
       <<"http://jabber.org/protocol/muc#owner">>} ->
	  decode_muc_owner(<<"http://jabber.org/protocol/muc#owner">>,
			   IgnoreEls, _el);
      {<<"destroy">>,
       <<"http://jabber.org/protocol/muc#owner">>} ->
	  decode_muc_owner_destroy(<<"http://jabber.org/protocol/muc#owner">>,
				   IgnoreEls, _el);
      {<<"reason">>,
       <<"http://jabber.org/protocol/muc#owner">>} ->
	  decode_muc_owner_reason(<<"http://jabber.org/protocol/muc#owner">>,
				  IgnoreEls, _el);
      {<<"password">>,
       <<"http://jabber.org/protocol/muc#owner">>} ->
	  decode_muc_owner_password(<<"http://jabber.org/protocol/muc#owner">>,
				    IgnoreEls, _el);
      {<<"x">>, <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user(<<"http://jabber.org/protocol/muc#user">>,
			  IgnoreEls, _el);
      {<<"item">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_item(<<"http://jabber.org/protocol/muc#user">>,
			       IgnoreEls, _el);
      {<<"status">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_status(<<"http://jabber.org/protocol/muc#user">>,
				 IgnoreEls, _el);
      {<<"continue">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_continue(<<"http://jabber.org/protocol/muc#user">>,
				   IgnoreEls, _el);
      {<<"actor">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_actor(<<"http://jabber.org/protocol/muc#user">>,
				IgnoreEls, _el);
      {<<"invite">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_invite(<<"http://jabber.org/protocol/muc#user">>,
				 IgnoreEls, _el);
      {<<"destroy">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_destroy(<<"http://jabber.org/protocol/muc#user">>,
				  IgnoreEls, _el);
      {<<"decline">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_decline(<<"http://jabber.org/protocol/muc#user">>,
				  IgnoreEls, _el);
      {<<"reason">>,
       <<"http://jabber.org/protocol/muc#user">>} ->
	  decode_muc_user_reason(<<"http://jabber.org/protocol/muc#user">>,
				 IgnoreEls, _el);
      {<<"history">>, <<"http://jabber.org/protocol/muc">>} ->
	  decode_muc_history(<<"http://jabber.org/protocol/muc">>,
			     IgnoreEls, _el);
      {<<"query">>,
       <<"http://jabber.org/protocol/bytestreams">>} ->
	  decode_bytestreams(<<"http://jabber.org/protocol/bytestreams">>,
			     IgnoreEls, _el);
      {<<"activate">>,
       <<"http://jabber.org/protocol/bytestreams">>} ->
	  decode_bytestreams_activate(<<"http://jabber.org/protocol/bytestreams">>,
				      IgnoreEls, _el);
      {<<"streamhost-used">>,
       <<"http://jabber.org/protocol/bytestreams">>} ->
	  decode_bytestreams_streamhost_used(<<"http://jabber.org/protocol/bytestreams">>,
					     IgnoreEls, _el);
      {<<"streamhost">>,
       <<"http://jabber.org/protocol/bytestreams">>} ->
	  decode_bytestreams_streamhost(<<"http://jabber.org/protocol/bytestreams">>,
					IgnoreEls, _el);
      {<<"delay">>, <<"urn:xmpp:delay">>} ->
	  decode_delay(<<"urn:xmpp:delay">>, IgnoreEls, _el);
      {<<"paused">>,
       <<"http://jabber.org/protocol/chatstates">>} ->
	  decode_chatstate_paused(<<"http://jabber.org/protocol/chatstates">>,
				  IgnoreEls, _el);
      {<<"inactive">>,
       <<"http://jabber.org/protocol/chatstates">>} ->
	  decode_chatstate_inactive(<<"http://jabber.org/protocol/chatstates">>,
				    IgnoreEls, _el);
      {<<"gone">>,
       <<"http://jabber.org/protocol/chatstates">>} ->
	  decode_chatstate_gone(<<"http://jabber.org/protocol/chatstates">>,
				IgnoreEls, _el);
      {<<"composing">>,
       <<"http://jabber.org/protocol/chatstates">>} ->
	  decode_chatstate_composing(<<"http://jabber.org/protocol/chatstates">>,
				     IgnoreEls, _el);
      {<<"active">>,
       <<"http://jabber.org/protocol/chatstates">>} ->
	  decode_chatstate_active(<<"http://jabber.org/protocol/chatstates">>,
				  IgnoreEls, _el);
      {<<"headers">>,
       <<"http://jabber.org/protocol/shim">>} ->
	  decode_shim_headers(<<"http://jabber.org/protocol/shim">>,
			      IgnoreEls, _el);
      {<<"header">>, <<"http://jabber.org/protocol/shim">>} ->
	  decode_shim_header(<<"http://jabber.org/protocol/shim">>,
			     IgnoreEls, _el);
      {<<"pubsub">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub(<<"http://jabber.org/protocol/pubsub">>,
			IgnoreEls, _el);
      {<<"retract">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_retract(<<"http://jabber.org/protocol/pubsub">>,
				IgnoreEls, _el);
      {<<"options">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_options(<<"http://jabber.org/protocol/pubsub">>,
				IgnoreEls, _el);
      {<<"publish">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_publish(<<"http://jabber.org/protocol/pubsub">>,
				IgnoreEls, _el);
      {<<"unsubscribe">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_unsubscribe(<<"http://jabber.org/protocol/pubsub">>,
				    IgnoreEls, _el);
      {<<"subscribe">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_subscribe(<<"http://jabber.org/protocol/pubsub">>,
				  IgnoreEls, _el);
      {<<"affiliations">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_affiliations(<<"http://jabber.org/protocol/pubsub">>,
				     IgnoreEls, _el);
      {<<"subscriptions">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_subscriptions(<<"http://jabber.org/protocol/pubsub">>,
				      IgnoreEls, _el);
      {<<"event">>,
       <<"http://jabber.org/protocol/pubsub#event">>} ->
	  decode_pubsub_event(<<"http://jabber.org/protocol/pubsub#event">>,
			      IgnoreEls, _el);
      {<<"items">>,
       <<"http://jabber.org/protocol/pubsub#event">>} ->
	  decode_pubsub_event_items(<<"http://jabber.org/protocol/pubsub#event">>,
				    IgnoreEls, _el);
      {<<"item">>,
       <<"http://jabber.org/protocol/pubsub#event">>} ->
	  decode_pubsub_event_item(<<"http://jabber.org/protocol/pubsub#event">>,
				   IgnoreEls, _el);
      {<<"retract">>,
       <<"http://jabber.org/protocol/pubsub#event">>} ->
	  decode_pubsub_event_retract(<<"http://jabber.org/protocol/pubsub#event">>,
				      IgnoreEls, _el);
      {<<"items">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_items(<<"http://jabber.org/protocol/pubsub">>,
			      IgnoreEls, _el);
      {<<"item">>, <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_item(<<"http://jabber.org/protocol/pubsub">>,
			     IgnoreEls, _el);
      {<<"affiliation">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_affiliation(<<"http://jabber.org/protocol/pubsub">>,
				    IgnoreEls, _el);
      {<<"subscription">>,
       <<"http://jabber.org/protocol/pubsub">>} ->
	  decode_pubsub_subscription(<<"http://jabber.org/protocol/pubsub">>,
				     IgnoreEls, _el);
      {<<"x">>, <<"jabber:x:data">>} ->
	  decode_xdata(<<"jabber:x:data">>, IgnoreEls, _el);
      {<<"item">>, <<"jabber:x:data">>} ->
	  decode_xdata_item(<<"jabber:x:data">>, IgnoreEls, _el);
      {<<"reported">>, <<"jabber:x:data">>} ->
	  decode_xdata_reported(<<"jabber:x:data">>, IgnoreEls,
				_el);
      {<<"title">>, <<"jabber:x:data">>} ->
	  decode_xdata_title(<<"jabber:x:data">>, IgnoreEls, _el);
      {<<"instructions">>, <<"jabber:x:data">>} ->
	  decode_xdata_instructions(<<"jabber:x:data">>,
				    IgnoreEls, _el);
      {<<"field">>, <<"jabber:x:data">>} ->
	  decode_xdata_field(<<"jabber:x:data">>, IgnoreEls, _el);
      {<<"option">>, <<"jabber:x:data">>} ->
	  decode_xdata_field_option(<<"jabber:x:data">>,
				    IgnoreEls, _el);
      {<<"value">>, <<"jabber:x:data">>} ->
	  decode_xdata_field_value(<<"jabber:x:data">>, IgnoreEls,
				   _el);
      {<<"desc">>, <<"jabber:x:data">>} ->
	  decode_xdata_field_desc(<<"jabber:x:data">>, IgnoreEls,
				  _el);
      {<<"required">>, <<"jabber:x:data">>} ->
	  decode_xdata_field_required(<<"jabber:x:data">>,
				      IgnoreEls, _el);
      {<<"x">>, <<"vcard-temp:x:update">>} ->
	  decode_vcard_xupdate(<<"vcard-temp:x:update">>,
			       IgnoreEls, _el);
      {<<"photo">>, <<"vcard-temp:x:update">>} ->
	  decode_vcard_xupdate_photo(<<"vcard-temp:x:update">>,
				     IgnoreEls, _el);
      {<<"vCard">>, <<"vcard-temp">>} ->
	  decode_vcard(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"CLASS">>, <<"vcard-temp">>} ->
	  decode_vcard_CLASS(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"CATEGORIES">>, <<"vcard-temp">>} ->
	  decode_vcard_CATEGORIES(<<"vcard-temp">>, IgnoreEls,
				  _el);
      {<<"KEY">>, <<"vcard-temp">>} ->
	  decode_vcard_KEY(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"SOUND">>, <<"vcard-temp">>} ->
	  decode_vcard_SOUND(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"ORG">>, <<"vcard-temp">>} ->
	  decode_vcard_ORG(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"PHOTO">>, <<"vcard-temp">>} ->
	  decode_vcard_PHOTO(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"LOGO">>, <<"vcard-temp">>} ->
	  decode_vcard_LOGO(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"BINVAL">>, <<"vcard-temp">>} ->
	  decode_vcard_BINVAL(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"GEO">>, <<"vcard-temp">>} ->
	  decode_vcard_GEO(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"EMAIL">>, <<"vcard-temp">>} ->
	  decode_vcard_EMAIL(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"TEL">>, <<"vcard-temp">>} ->
	  decode_vcard_TEL(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"LABEL">>, <<"vcard-temp">>} ->
	  decode_vcard_LABEL(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"ADR">>, <<"vcard-temp">>} ->
	  decode_vcard_ADR(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"N">>, <<"vcard-temp">>} ->
	  decode_vcard_N(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"CONFIDENTIAL">>, <<"vcard-temp">>} ->
	  decode_vcard_CONFIDENTIAL(<<"vcard-temp">>, IgnoreEls,
				    _el);
      {<<"PRIVATE">>, <<"vcard-temp">>} ->
	  decode_vcard_PRIVATE(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"PUBLIC">>, <<"vcard-temp">>} ->
	  decode_vcard_PUBLIC(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"EXTVAL">>, <<"vcard-temp">>} ->
	  decode_vcard_EXTVAL(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"TYPE">>, <<"vcard-temp">>} ->
	  decode_vcard_TYPE(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"DESC">>, <<"vcard-temp">>} ->
	  decode_vcard_DESC(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"URL">>, <<"vcard-temp">>} ->
	  decode_vcard_URL(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"UID">>, <<"vcard-temp">>} ->
	  decode_vcard_UID(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"SORT-STRING">>, <<"vcard-temp">>} ->
	  decode_vcard_SORT_STRING(<<"vcard-temp">>, IgnoreEls,
				   _el);
      {<<"REV">>, <<"vcard-temp">>} ->
	  decode_vcard_REV(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"PRODID">>, <<"vcard-temp">>} ->
	  decode_vcard_PRODID(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"NOTE">>, <<"vcard-temp">>} ->
	  decode_vcard_NOTE(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"KEYWORD">>, <<"vcard-temp">>} ->
	  decode_vcard_KEYWORD(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"ROLE">>, <<"vcard-temp">>} ->
	  decode_vcard_ROLE(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"TITLE">>, <<"vcard-temp">>} ->
	  decode_vcard_TITLE(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"TZ">>, <<"vcard-temp">>} ->
	  decode_vcard_TZ(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"MAILER">>, <<"vcard-temp">>} ->
	  decode_vcard_MAILER(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"JABBERID">>, <<"vcard-temp">>} ->
	  decode_vcard_JABBERID(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"BDAY">>, <<"vcard-temp">>} ->
	  decode_vcard_BDAY(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"NICKNAME">>, <<"vcard-temp">>} ->
	  decode_vcard_NICKNAME(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"FN">>, <<"vcard-temp">>} ->
	  decode_vcard_FN(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"VERSION">>, <<"vcard-temp">>} ->
	  decode_vcard_VERSION(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"CRED">>, <<"vcard-temp">>} ->
	  decode_vcard_CRED(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"PHONETIC">>, <<"vcard-temp">>} ->
	  decode_vcard_PHONETIC(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"ORGUNIT">>, <<"vcard-temp">>} ->
	  decode_vcard_ORGUNIT(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"ORGNAME">>, <<"vcard-temp">>} ->
	  decode_vcard_ORGNAME(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"LON">>, <<"vcard-temp">>} ->
	  decode_vcard_LON(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"LAT">>, <<"vcard-temp">>} ->
	  decode_vcard_LAT(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"USERID">>, <<"vcard-temp">>} ->
	  decode_vcard_USERID(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"NUMBER">>, <<"vcard-temp">>} ->
	  decode_vcard_NUMBER(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"LINE">>, <<"vcard-temp">>} ->
	  decode_vcard_LINE(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"CTRY">>, <<"vcard-temp">>} ->
	  decode_vcard_CTRY(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"PCODE">>, <<"vcard-temp">>} ->
	  decode_vcard_PCODE(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"REGION">>, <<"vcard-temp">>} ->
	  decode_vcard_REGION(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"LOCALITY">>, <<"vcard-temp">>} ->
	  decode_vcard_LOCALITY(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"STREET">>, <<"vcard-temp">>} ->
	  decode_vcard_STREET(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"EXTADD">>, <<"vcard-temp">>} ->
	  decode_vcard_EXTADD(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"POBOX">>, <<"vcard-temp">>} ->
	  decode_vcard_POBOX(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"SUFFIX">>, <<"vcard-temp">>} ->
	  decode_vcard_SUFFIX(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"PREFIX">>, <<"vcard-temp">>} ->
	  decode_vcard_PREFIX(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"MIDDLE">>, <<"vcard-temp">>} ->
	  decode_vcard_MIDDLE(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"GIVEN">>, <<"vcard-temp">>} ->
	  decode_vcard_GIVEN(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"FAMILY">>, <<"vcard-temp">>} ->
	  decode_vcard_FAMILY(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"X400">>, <<"vcard-temp">>} ->
	  decode_vcard_X400(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"INTERNET">>, <<"vcard-temp">>} ->
	  decode_vcard_INTERNET(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"PREF">>, <<"vcard-temp">>} ->
	  decode_vcard_PREF(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"INTL">>, <<"vcard-temp">>} ->
	  decode_vcard_INTL(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"DOM">>, <<"vcard-temp">>} ->
	  decode_vcard_DOM(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"PARCEL">>, <<"vcard-temp">>} ->
	  decode_vcard_PARCEL(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"POSTAL">>, <<"vcard-temp">>} ->
	  decode_vcard_POSTAL(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"PCS">>, <<"vcard-temp">>} ->
	  decode_vcard_PCS(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"ISDN">>, <<"vcard-temp">>} ->
	  decode_vcard_ISDN(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"MODEM">>, <<"vcard-temp">>} ->
	  decode_vcard_MODEM(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"BBS">>, <<"vcard-temp">>} ->
	  decode_vcard_BBS(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"VIDEO">>, <<"vcard-temp">>} ->
	  decode_vcard_VIDEO(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"CELL">>, <<"vcard-temp">>} ->
	  decode_vcard_CELL(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"MSG">>, <<"vcard-temp">>} ->
	  decode_vcard_MSG(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"PAGER">>, <<"vcard-temp">>} ->
	  decode_vcard_PAGER(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"FAX">>, <<"vcard-temp">>} ->
	  decode_vcard_FAX(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"VOICE">>, <<"vcard-temp">>} ->
	  decode_vcard_VOICE(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"WORK">>, <<"vcard-temp">>} ->
	  decode_vcard_WORK(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"HOME">>, <<"vcard-temp">>} ->
	  decode_vcard_HOME(<<"vcard-temp">>, IgnoreEls, _el);
      {<<"stream:error">>,
       <<"http://etherx.jabber.org/streams">>} ->
	  decode_stream_error(<<"http://etherx.jabber.org/streams">>,
			      IgnoreEls, _el);
      {<<"unsupported-version">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_unsupported_version(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
						  IgnoreEls, _el);
      {<<"unsupported-stanza-type">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_unsupported_stanza_type(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
						      IgnoreEls, _el);
      {<<"unsupported-encoding">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_unsupported_encoding(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
						   IgnoreEls, _el);
      {<<"undefined-condition">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_undefined_condition(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
						  IgnoreEls, _el);
      {<<"system-shutdown">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_system_shutdown(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
					      IgnoreEls, _el);
      {<<"see-other-host">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_see_other_host(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
					     IgnoreEls, _el);
      {<<"restricted-xml">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_restricted_xml(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
					     IgnoreEls, _el);
      {<<"resource-constraint">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_resource_constraint(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
						  IgnoreEls, _el);
      {<<"reset">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_reset(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
				    IgnoreEls, _el);
      {<<"remote-connection-failed">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_remote_connection_failed(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
						       IgnoreEls, _el);
      {<<"policy-violation">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_policy_violation(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
					       IgnoreEls, _el);
      {<<"not-well-formed">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_not_well_formed(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
					      IgnoreEls, _el);
      {<<"not-authorized">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_not_authorized(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
					     IgnoreEls, _el);
      {<<"invalid-xml">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_invalid_xml(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
					  IgnoreEls, _el);
      {<<"invalid-namespace">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_invalid_namespace(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
						IgnoreEls, _el);
      {<<"invalid-id">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_invalid_id(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
					 IgnoreEls, _el);
      {<<"invalid-from">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_invalid_from(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
					   IgnoreEls, _el);
      {<<"internal-server-error">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_internal_server_error(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
						    IgnoreEls, _el);
      {<<"improper-addressing">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_improper_addressing(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
						  IgnoreEls, _el);
      {<<"host-unknown">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_host_unknown(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
					   IgnoreEls, _el);
      {<<"host-gone">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_host_gone(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
					IgnoreEls, _el);
      {<<"connection-timeout">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_connection_timeout(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
						 IgnoreEls, _el);
      {<<"conflict">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_conflict(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
				       IgnoreEls, _el);
      {<<"bad-namespace-prefix">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_bad_namespace_prefix(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
						   IgnoreEls, _el);
      {<<"bad-format">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_bad_format(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
					 IgnoreEls, _el);
      {<<"text">>,
       <<"urn:ietf:params:xml:ns:xmpp-streams">>} ->
	  decode_stream_error_text(<<"urn:ietf:params:xml:ns:xmpp-streams">>,
				   IgnoreEls, _el);
      {<<"time">>, <<"urn:xmpp:time">>} ->
	  decode_time(<<"urn:xmpp:time">>, IgnoreEls, _el);
      {<<"tzo">>, <<"urn:xmpp:time">>} ->
	  decode_time_tzo(<<"urn:xmpp:time">>, IgnoreEls, _el);
      {<<"utc">>, <<"urn:xmpp:time">>} ->
	  decode_time_utc(<<"urn:xmpp:time">>, IgnoreEls, _el);
      {<<"ping">>, <<"urn:xmpp:ping">>} ->
	  decode_ping(<<"urn:xmpp:ping">>, IgnoreEls, _el);
      {<<"session">>,
       <<"urn:ietf:params:xml:ns:xmpp-session">>} ->
	  decode_session(<<"urn:ietf:params:xml:ns:xmpp-session">>,
			 IgnoreEls, _el);
      {<<"query">>, <<"jabber:iq:register">>} ->
	  decode_register(<<"jabber:iq:register">>, IgnoreEls,
			  _el);
      {<<"key">>, <<"jabber:iq:register">>} ->
	  decode_register_key(<<"jabber:iq:register">>, IgnoreEls,
			      _el);
      {<<"text">>, <<"jabber:iq:register">>} ->
	  decode_register_text(<<"jabber:iq:register">>,
			       IgnoreEls, _el);
      {<<"misc">>, <<"jabber:iq:register">>} ->
	  decode_register_misc(<<"jabber:iq:register">>,
			       IgnoreEls, _el);
      {<<"date">>, <<"jabber:iq:register">>} ->
	  decode_register_date(<<"jabber:iq:register">>,
			       IgnoreEls, _el);
      {<<"url">>, <<"jabber:iq:register">>} ->
	  decode_register_url(<<"jabber:iq:register">>, IgnoreEls,
			      _el);
      {<<"phone">>, <<"jabber:iq:register">>} ->
	  decode_register_phone(<<"jabber:iq:register">>,
				IgnoreEls, _el);
      {<<"zip">>, <<"jabber:iq:register">>} ->
	  decode_register_zip(<<"jabber:iq:register">>, IgnoreEls,
			      _el);
      {<<"state">>, <<"jabber:iq:register">>} ->
	  decode_register_state(<<"jabber:iq:register">>,
				IgnoreEls, _el);
      {<<"city">>, <<"jabber:iq:register">>} ->
	  decode_register_city(<<"jabber:iq:register">>,
			       IgnoreEls, _el);
      {<<"address">>, <<"jabber:iq:register">>} ->
	  decode_register_address(<<"jabber:iq:register">>,
				  IgnoreEls, _el);
      {<<"email">>, <<"jabber:iq:register">>} ->
	  decode_register_email(<<"jabber:iq:register">>,
				IgnoreEls, _el);
      {<<"last">>, <<"jabber:iq:register">>} ->
	  decode_register_last(<<"jabber:iq:register">>,
			       IgnoreEls, _el);
      {<<"first">>, <<"jabber:iq:register">>} ->
	  decode_register_first(<<"jabber:iq:register">>,
				IgnoreEls, _el);
      {<<"name">>, <<"jabber:iq:register">>} ->
	  decode_register_name(<<"jabber:iq:register">>,
			       IgnoreEls, _el);
      {<<"password">>, <<"jabber:iq:register">>} ->
	  decode_register_password(<<"jabber:iq:register">>,
				   IgnoreEls, _el);
      {<<"nick">>, <<"jabber:iq:register">>} ->
	  decode_register_nick(<<"jabber:iq:register">>,
			       IgnoreEls, _el);
      {<<"username">>, <<"jabber:iq:register">>} ->
	  decode_register_username(<<"jabber:iq:register">>,
				   IgnoreEls, _el);
      {<<"instructions">>, <<"jabber:iq:register">>} ->
	  decode_register_instructions(<<"jabber:iq:register">>,
				       IgnoreEls, _el);
      {<<"remove">>, <<"jabber:iq:register">>} ->
	  decode_register_remove(<<"jabber:iq:register">>,
				 IgnoreEls, _el);
      {<<"registered">>, <<"jabber:iq:register">>} ->
	  decode_register_registered(<<"jabber:iq:register">>,
				     IgnoreEls, _el);
      {<<"register">>,
       <<"http://jabber.org/features/iq-register">>} ->
	  decode_feature_register(<<"http://jabber.org/features/iq-register">>,
				  IgnoreEls, _el);
      {<<"c">>, <<"http://jabber.org/protocol/caps">>} ->
	  decode_caps(<<"http://jabber.org/protocol/caps">>,
		      IgnoreEls, _el);
      {<<"ack">>, <<"p1:ack">>} ->
	  decode_p1_ack(<<"p1:ack">>, IgnoreEls, _el);
      {<<"rebind">>, <<"p1:rebind">>} ->
	  decode_p1_rebind(<<"p1:rebind">>, IgnoreEls, _el);
      {<<"push">>, <<"p1:push">>} ->
	  decode_p1_push(<<"p1:push">>, IgnoreEls, _el);
      {<<"stream:features">>,
       <<"http://etherx.jabber.org/streams">>} ->
	  decode_stream_features(<<"http://etherx.jabber.org/streams">>,
				 IgnoreEls, _el);
      {<<"compression">>,
       <<"http://jabber.org/features/compress">>} ->
	  decode_compression(<<"http://jabber.org/features/compress">>,
			     IgnoreEls, _el);
      {<<"method">>,
       <<"http://jabber.org/features/compress">>} ->
	  decode_compression_method(<<"http://jabber.org/features/compress">>,
				    IgnoreEls, _el);
      {<<"compressed">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  decode_compressed(<<"http://jabber.org/protocol/compress">>,
			    IgnoreEls, _el);
      {<<"compress">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  decode_compress(<<"http://jabber.org/protocol/compress">>,
			  IgnoreEls, _el);
      {<<"method">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  decode_compress_method(<<"http://jabber.org/protocol/compress">>,
				 IgnoreEls, _el);
      {<<"failure">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  decode_compress_failure(<<"http://jabber.org/protocol/compress">>,
				  IgnoreEls, _el);
      {<<"unsupported-method">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  decode_compress_failure_unsupported_method(<<"http://jabber.org/protocol/compress">>,
						     IgnoreEls, _el);
      {<<"processing-failed">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  decode_compress_failure_processing_failed(<<"http://jabber.org/protocol/compress">>,
						    IgnoreEls, _el);
      {<<"setup-failed">>,
       <<"http://jabber.org/protocol/compress">>} ->
	  decode_compress_failure_setup_failed(<<"http://jabber.org/protocol/compress">>,
					       IgnoreEls, _el);
      {<<"failure">>,
       <<"urn:ietf:params:xml:ns:xmpp-tls">>} ->
	  decode_starttls_failure(<<"urn:ietf:params:xml:ns:xmpp-tls">>,
				  IgnoreEls, _el);
      {<<"proceed">>,
       <<"urn:ietf:params:xml:ns:xmpp-tls">>} ->
	  decode_starttls_proceed(<<"urn:ietf:params:xml:ns:xmpp-tls">>,
				  IgnoreEls, _el);
      {<<"starttls">>,
       <<"urn:ietf:params:xml:ns:xmpp-tls">>} ->
	  decode_starttls(<<"urn:ietf:params:xml:ns:xmpp-tls">>,
			  IgnoreEls, _el);
      {<<"required">>,
       <<"urn:ietf:params:xml:ns:xmpp-tls">>} ->
	  decode_starttls_required(<<"urn:ietf:params:xml:ns:xmpp-tls">>,
				   IgnoreEls, _el);
      {<<"mechanisms">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_mechanisms(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
				 IgnoreEls, _el);
      {<<"mechanism">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_mechanism(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
				IgnoreEls, _el);
      {<<"failure">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
			      IgnoreEls, _el);
      {<<"temporary-auth-failure">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_temporary_auth_failure(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
						     IgnoreEls, _el);
      {<<"not-authorized">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_not_authorized(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
					     IgnoreEls, _el);
      {<<"mechanism-too-weak">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_mechanism_too_weak(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
						 IgnoreEls, _el);
      {<<"malformed-request">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_malformed_request(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
						IgnoreEls, _el);
      {<<"invalid-mechanism">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_invalid_mechanism(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
						IgnoreEls, _el);
      {<<"invalid-authzid">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_invalid_authzid(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
					      IgnoreEls, _el);
      {<<"incorrect-encoding">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_incorrect_encoding(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
						 IgnoreEls, _el);
      {<<"encryption-required">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_encryption_required(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
						  IgnoreEls, _el);
      {<<"credentials-expired">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_credentials_expired(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
						  IgnoreEls, _el);
      {<<"account-disabled">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_account_disabled(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
					       IgnoreEls, _el);
      {<<"aborted">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_aborted(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
				      IgnoreEls, _el);
      {<<"text">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_failure_text(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
				   IgnoreEls, _el);
      {<<"success">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_success(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
			      IgnoreEls, _el);
      {<<"response">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_response(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
			       IgnoreEls, _el);
      {<<"challenge">>,
       <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_challenge(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
				IgnoreEls, _el);
      {<<"abort">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_abort(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
			    IgnoreEls, _el);
      {<<"auth">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>} ->
	  decode_sasl_auth(<<"urn:ietf:params:xml:ns:xmpp-sasl">>,
			   IgnoreEls, _el);
      {<<"bind">>, <<"urn:ietf:params:xml:ns:xmpp-bind">>} ->
	  decode_bind(<<"urn:ietf:params:xml:ns:xmpp-bind">>,
		      IgnoreEls, _el);
      {<<"resource">>,
       <<"urn:ietf:params:xml:ns:xmpp-bind">>} ->
	  decode_bind_resource(<<"urn:ietf:params:xml:ns:xmpp-bind">>,
			       IgnoreEls, _el);
      {<<"jid">>, <<"urn:ietf:params:xml:ns:xmpp-bind">>} ->
	  decode_bind_jid(<<"urn:ietf:params:xml:ns:xmpp-bind">>,
			  IgnoreEls, _el);
      {<<"error">>, <<"jabber:client">>} ->
	  decode_error(<<"jabber:client">>, IgnoreEls, _el);
      {<<"text">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_text(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
			    IgnoreEls, _el);
      {<<"unexpected-request">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_unexpected_request(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
					  IgnoreEls, _el);
      {<<"undefined-condition">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_undefined_condition(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
					   IgnoreEls, _el);
      {<<"subscription-required">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_subscription_required(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
					     IgnoreEls, _el);
      {<<"service-unavailable">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_service_unavailable(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
					   IgnoreEls, _el);
      {<<"resource-constraint">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_resource_constraint(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
					   IgnoreEls, _el);
      {<<"remote-server-timeout">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_remote_server_timeout(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
					     IgnoreEls, _el);
      {<<"remote-server-not-found">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_remote_server_not_found(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
					       IgnoreEls, _el);
      {<<"registration-required">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_registration_required(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
					     IgnoreEls, _el);
      {<<"redirect">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_redirect(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
				IgnoreEls, _el);
      {<<"recipient-unavailable">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_recipient_unavailable(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
					     IgnoreEls, _el);
      {<<"policy-violation">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_policy_violation(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
					IgnoreEls, _el);
      {<<"not-authorized">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_not_authorized(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
				      IgnoreEls, _el);
      {<<"not-allowed">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_not_allowed(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
				   IgnoreEls, _el);
      {<<"not-acceptable">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_not_acceptable(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
				      IgnoreEls, _el);
      {<<"jid-malformed">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_jid_malformed(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
				     IgnoreEls, _el);
      {<<"item-not-found">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_item_not_found(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
				      IgnoreEls, _el);
      {<<"internal-server-error">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_internal_server_error(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
					     IgnoreEls, _el);
      {<<"gone">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_gone(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
			    IgnoreEls, _el);
      {<<"forbidden">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_forbidden(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
				 IgnoreEls, _el);
      {<<"feature-not-implemented">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_feature_not_implemented(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
					       IgnoreEls, _el);
      {<<"conflict">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_conflict(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
				IgnoreEls, _el);
      {<<"bad-request">>,
       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>} ->
	  decode_error_bad_request(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
				   IgnoreEls, _el);
      {<<"presence">>, <<"jabber:client">>} ->
	  decode_presence(<<"jabber:client">>, IgnoreEls, _el);
      {<<"priority">>, <<"jabber:client">>} ->
	  decode_presence_priority(<<"jabber:client">>, IgnoreEls,
				   _el);
      {<<"status">>, <<"jabber:client">>} ->
	  decode_presence_status(<<"jabber:client">>, IgnoreEls,
				 _el);
      {<<"show">>, <<"jabber:client">>} ->
	  decode_presence_show(<<"jabber:client">>, IgnoreEls,
			       _el);
      {<<"message">>, <<"jabber:client">>} ->
	  decode_message(<<"jabber:client">>, IgnoreEls, _el);
      {<<"thread">>, <<"jabber:client">>} ->
	  decode_message_thread(<<"jabber:client">>, IgnoreEls,
				_el);
      {<<"body">>, <<"jabber:client">>} ->
	  decode_message_body(<<"jabber:client">>, IgnoreEls,
			      _el);
      {<<"subject">>, <<"jabber:client">>} ->
	  decode_message_subject(<<"jabber:client">>, IgnoreEls,
				 _el);
      {<<"iq">>, <<"jabber:client">>} ->
	  decode_iq(<<"jabber:client">>, IgnoreEls, _el);
      {<<"query">>, <<"http://jabber.org/protocol/stats">>} ->
	  decode_stats(<<"http://jabber.org/protocol/stats">>,
		       IgnoreEls, _el);
      {<<"stat">>, <<"http://jabber.org/protocol/stats">>} ->
	  decode_stat(<<"http://jabber.org/protocol/stats">>,
		      IgnoreEls, _el);
      {<<"error">>, <<"http://jabber.org/protocol/stats">>} ->
	  decode_stat_error(<<"http://jabber.org/protocol/stats">>,
			    IgnoreEls, _el);
      {<<"storage">>, <<"storage:bookmarks">>} ->
	  decode_bookmarks_storage(<<"storage:bookmarks">>,
				   IgnoreEls, _el);
      {<<"url">>, <<"storage:bookmarks">>} ->
	  decode_bookmark_url(<<"storage:bookmarks">>, IgnoreEls,
			      _el);
      {<<"conference">>, <<"storage:bookmarks">>} ->
	  decode_bookmark_conference(<<"storage:bookmarks">>,
				     IgnoreEls, _el);
      {<<"password">>, <<"storage:bookmarks">>} ->
	  decode_conference_password(<<"storage:bookmarks">>,
				     IgnoreEls, _el);
      {<<"nick">>, <<"storage:bookmarks">>} ->
	  decode_conference_nick(<<"storage:bookmarks">>,
				 IgnoreEls, _el);
      {<<"query">>, <<"jabber:iq:private">>} ->
	  decode_private(<<"jabber:iq:private">>, IgnoreEls, _el);
      {<<"query">>,
       <<"http://jabber.org/protocol/disco#items">>} ->
	  decode_disco_items(<<"http://jabber.org/protocol/disco#items">>,
			     IgnoreEls, _el);
      {<<"item">>,
       <<"http://jabber.org/protocol/disco#items">>} ->
	  decode_disco_item(<<"http://jabber.org/protocol/disco#items">>,
			    IgnoreEls, _el);
      {<<"query">>,
       <<"http://jabber.org/protocol/disco#info">>} ->
	  decode_disco_info(<<"http://jabber.org/protocol/disco#info">>,
			    IgnoreEls, _el);
      {<<"feature">>,
       <<"http://jabber.org/protocol/disco#info">>} ->
	  decode_disco_feature(<<"http://jabber.org/protocol/disco#info">>,
			       IgnoreEls, _el);
      {<<"identity">>,
       <<"http://jabber.org/protocol/disco#info">>} ->
	  decode_disco_identity(<<"http://jabber.org/protocol/disco#info">>,
				IgnoreEls, _el);
      {<<"blocklist">>, <<"urn:xmpp:blocking">>} ->
	  decode_block_list(<<"urn:xmpp:blocking">>, IgnoreEls,
			    _el);
      {<<"unblock">>, <<"urn:xmpp:blocking">>} ->
	  decode_unblock(<<"urn:xmpp:blocking">>, IgnoreEls, _el);
      {<<"block">>, <<"urn:xmpp:blocking">>} ->
	  decode_block(<<"urn:xmpp:blocking">>, IgnoreEls, _el);
      {<<"item">>, <<"urn:xmpp:blocking">>} ->
	  decode_block_item(<<"urn:xmpp:blocking">>, IgnoreEls,
			    _el);
      {<<"query">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy(<<"jabber:iq:privacy">>, IgnoreEls, _el);
      {<<"active">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_active_list(<<"jabber:iq:privacy">>,
				     IgnoreEls, _el);
      {<<"default">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_default_list(<<"jabber:iq:privacy">>,
				      IgnoreEls, _el);
      {<<"list">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_list(<<"jabber:iq:privacy">>, IgnoreEls,
			      _el);
      {<<"item">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_item(<<"jabber:iq:privacy">>, IgnoreEls,
			      _el);
      {<<"presence-out">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_presence_out(<<"jabber:iq:privacy">>,
				      IgnoreEls, _el);
      {<<"presence-in">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_presence_in(<<"jabber:iq:privacy">>,
				     IgnoreEls, _el);
      {<<"iq">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_iq(<<"jabber:iq:privacy">>, IgnoreEls,
			    _el);
      {<<"message">>, <<"jabber:iq:privacy">>} ->
	  decode_privacy_message(<<"jabber:iq:privacy">>,
				 IgnoreEls, _el);
      {<<"query">>, <<"jabber:iq:roster">>} ->
	  decode_roster(<<"jabber:iq:roster">>, IgnoreEls, _el);
      {<<"item">>, <<"jabber:iq:roster">>} ->
	  decode_roster_item(<<"jabber:iq:roster">>, IgnoreEls,
			     _el);
      {<<"group">>, <<"jabber:iq:roster">>} ->
	  decode_roster_group(<<"jabber:iq:roster">>, IgnoreEls,
			      _el);
      {<<"query">>, <<"jabber:iq:version">>} ->
	  decode_version(<<"jabber:iq:version">>, IgnoreEls, _el);
      {<<"os">>, <<"jabber:iq:version">>} ->
	  decode_version_os(<<"jabber:iq:version">>, IgnoreEls,
			    _el);
      {<<"version">>, <<"jabber:iq:version">>} ->
	  decode_version_ver(<<"jabber:iq:version">>, IgnoreEls,
			     _el);
      {<<"name">>, <<"jabber:iq:version">>} ->
	  decode_version_name(<<"jabber:iq:version">>, IgnoreEls,
			      _el);
      {<<"query">>, <<"jabber:iq:last">>} ->
	  decode_last(<<"jabber:iq:last">>, IgnoreEls, _el);
      {_name, _xmlns} ->
	  erlang:error({xmpp_codec, {unknown_tag, _name, _xmlns}})
    end.

is_known_tag({xmlel, _name, _attrs, _} = _el) ->
    case {_name, get_attr(<<"xmlns">>, _attrs)} of
      {<<"failed">>, <<"urn:xmpp:sm:2">>} -> true;
      {<<"failed">>, <<"urn:xmpp:sm:3">>} -> true;
      {<<"a">>, <<"urn:xmpp:sm:2">>} -> true;
      {<<"a">>, <<"urn:xmpp:sm:3">>} -> true;
      {<<"r">>, <<"urn:xmpp:sm:2">>} -> true;
      {<<"r">>, <<"urn:xmpp:sm:3">>} -> true;
      {<<"resumed">>, <<"urn:xmpp:sm:2">>} -> true;
      {<<"resumed">>, <<"urn:xmpp:sm:3">>} -> true;
      {<<"resume">>, <<"urn:xmpp:sm:2">>} -> true;
      {<<"resume">>, <<"urn:xmpp:sm:3">>} -> true;
      {<<"enabled">>, <<"urn:xmpp:sm:2">>} -> true;
      {<<"enabled">>, <<"urn:xmpp:sm:3">>} -> true;
      {<<"enable">>, <<"urn:xmpp:sm:2">>} -> true;
      {<<"enable">>, <<"urn:xmpp:sm:3">>} -> true;
      {<<"sm">>, <<"urn:xmpp:sm:2">>} -> true;
      {<<"sm">>, <<"urn:xmpp:sm:3">>} -> true;
      {<<"inactive">>, <<"urn:xmpp:csi:0">>} -> true;
      {<<"active">>, <<"urn:xmpp:csi:0">>} -> true;
      {<<"csi">>, <<"urn:xmpp:csi:0">>} -> true;
      {<<"sent">>, <<"urn:xmpp:carbons:2">>} -> true;
      {<<"received">>, <<"urn:xmpp:carbons:2">>} -> true;
      {<<"private">>, <<"urn:xmpp:carbons:2">>} -> true;
      {<<"enable">>, <<"urn:xmpp:carbons:2">>} -> true;
      {<<"disable">>, <<"urn:xmpp:carbons:2">>} -> true;
      {<<"forwarded">>, <<"urn:xmpp:forward:0">>} -> true;
      {<<"fin">>, <<"urn:xmpp:mam:0">>} -> true;
      {<<"prefs">>, <<"urn:xmpp:mam:0">>} -> true;
      {<<"prefs">>, <<"urn:xmpp:mam:tmp">>} -> true;
      {<<"always">>, <<"urn:xmpp:mam:tmp">>} -> true;
      {<<"never">>, <<"urn:xmpp:mam:tmp">>} -> true;
      {<<"jid">>, <<"urn:xmpp:mam:tmp">>} -> true;
      {<<"result">>, <<"urn:xmpp:mam:0">>} -> true;
      {<<"result">>, <<"urn:xmpp:mam:tmp">>} -> true;
      {<<"archived">>, <<"urn:xmpp:mam:tmp">>} -> true;
      {<<"query">>, <<"urn:xmpp:mam:0">>} -> true;
      {<<"query">>, <<"urn:xmpp:mam:tmp">>} -> true;
      {<<"with">>, <<"urn:xmpp:mam:tmp">>} -> true;
      {<<"end">>, <<"urn:xmpp:mam:tmp">>} -> true;
      {<<"start">>, <<"urn:xmpp:mam:tmp">>} -> true;
      {<<"set">>, <<"http://jabber.org/protocol/rsm">>} ->
	  true;
      {<<"first">>, <<"http://jabber.org/protocol/rsm">>} ->
	  true;
      {<<"max">>, <<"http://jabber.org/protocol/rsm">>} ->
	  true;
      {<<"index">>, <<"http://jabber.org/protocol/rsm">>} ->
	  true;
      {<<"count">>, <<"http://jabber.org/protocol/rsm">>} ->
	  true;
      {<<"last">>, <<"http://jabber.org/protocol/rsm">>} ->
	  true;
      {<<"before">>, <<"http://jabber.org/protocol/rsm">>} ->
	  true;
      {<<"after">>, <<"http://jabber.org/protocol/rsm">>} ->
	  true;
      {<<"x">>, <<"http://jabber.org/protocol/muc">>} -> true;
      {<<"query">>,
       <<"http://jabber.org/protocol/muc#admin">>} ->
	  true;
      {<<"reason">>,
       <<"http://jabber.org/protocol/muc#admin">>} ->
	  true;
      {<<"continue">>,
       <<"http://jabber.org/protocol/muc#admin">>} ->
	  true;
      {<<"actor">>,
       <<"http://jabber.org/protocol/muc#admin">>} ->
	  true;
      {<<"item">>,
       <<"http://jabber.org/protocol/muc#admin">>} ->
	  true;
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
      {<<"delay">>, <<"urn:xmpp:delay">>} -> true;
      {<<"paused">>,
       <<"http://jabber.org/protocol/chatstates">>} ->
	  true;
      {<<"inactive">>,
       <<"http://jabber.org/protocol/chatstates">>} ->
	  true;
      {<<"gone">>,
       <<"http://jabber.org/protocol/chatstates">>} ->
	  true;
      {<<"composing">>,
       <<"http://jabber.org/protocol/chatstates">>} ->
	  true;
      {<<"active">>,
       <<"http://jabber.org/protocol/chatstates">>} ->
	  true;
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
      {<<"x">>, <<"vcard-temp:x:update">>} -> true;
      {<<"photo">>, <<"vcard-temp:x:update">>} -> true;
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

encode({xmlel, _, _, _} = El) -> El;
encode({last, _, _} = Query) ->
    encode_last(Query,
		[{<<"xmlns">>, <<"jabber:iq:last">>}]);
encode({version, _, _, _} = Query) ->
    encode_version(Query,
		   [{<<"xmlns">>, <<"jabber:iq:version">>}]);
encode({roster_item, _, _, _, _, _} = Item) ->
    encode_roster_item(Item,
		       [{<<"xmlns">>, <<"jabber:iq:roster">>}]);
encode({roster, _, _} = Query) ->
    encode_roster(Query,
		  [{<<"xmlns">>, <<"jabber:iq:roster">>}]);
encode({privacy_item, _, _, _, _, _} = Item) ->
    encode_privacy_item(Item,
			[{<<"xmlns">>, <<"jabber:iq:privacy">>}]);
encode({privacy_list, _, _} = List) ->
    encode_privacy_list(List,
			[{<<"xmlns">>, <<"jabber:iq:privacy">>}]);
encode({privacy, _, _, _} = Query) ->
    encode_privacy(Query,
		   [{<<"xmlns">>, <<"jabber:iq:privacy">>}]);
encode({block, _} = Block) ->
    encode_block(Block,
		 [{<<"xmlns">>, <<"urn:xmpp:blocking">>}]);
encode({unblock, _} = Unblock) ->
    encode_unblock(Unblock,
		   [{<<"xmlns">>, <<"urn:xmpp:blocking">>}]);
encode({block_list} = Blocklist) ->
    encode_block_list(Blocklist,
		      [{<<"xmlns">>, <<"urn:xmpp:blocking">>}]);
encode({identity, _, _, _, _} = Identity) ->
    encode_disco_identity(Identity,
			  [{<<"xmlns">>,
			    <<"http://jabber.org/protocol/disco#info">>}]);
encode({disco_info, _, _, _, _} = Query) ->
    encode_disco_info(Query,
		      [{<<"xmlns">>,
			<<"http://jabber.org/protocol/disco#info">>}]);
encode({disco_item, _, _, _} = Item) ->
    encode_disco_item(Item,
		      [{<<"xmlns">>,
			<<"http://jabber.org/protocol/disco#items">>}]);
encode({disco_items, _, _} = Query) ->
    encode_disco_items(Query,
		       [{<<"xmlns">>,
			 <<"http://jabber.org/protocol/disco#items">>}]);
encode({private, _} = Query) ->
    encode_private(Query,
		   [{<<"xmlns">>, <<"jabber:iq:private">>}]);
encode({bookmark_conference, _, _, _, _, _} =
	   Conference) ->
    encode_bookmark_conference(Conference,
			       [{<<"xmlns">>, <<"storage:bookmarks">>}]);
encode({bookmark_url, _, _} = Url) ->
    encode_bookmark_url(Url,
			[{<<"xmlns">>, <<"storage:bookmarks">>}]);
encode({bookmark_storage, _, _} = Storage) ->
    encode_bookmarks_storage(Storage,
			     [{<<"xmlns">>, <<"storage:bookmarks">>}]);
encode({stat, _, _, _, _} = Stat) ->
    encode_stat(Stat,
		[{<<"xmlns">>,
		  <<"http://jabber.org/protocol/stats">>}]);
encode({stats, _} = Query) ->
    encode_stats(Query,
		 [{<<"xmlns">>,
		   <<"http://jabber.org/protocol/stats">>}]);
encode({iq, _, _, _, _, _, _, _} = Iq) ->
    encode_iq(Iq, [{<<"xmlns">>, <<"jabber:client">>}]);
encode({message, _, _, _, _, _, _, _, _, _, _} =
	   Message) ->
    encode_message(Message,
		   [{<<"xmlns">>, <<"jabber:client">>}]);
encode({presence, _, _, _, _, _, _, _, _, _, _} =
	   Presence) ->
    encode_presence(Presence,
		    [{<<"xmlns">>, <<"jabber:client">>}]);
encode({gone, _} = Gone) ->
    encode_error_gone(Gone,
		      [{<<"xmlns">>,
			<<"urn:ietf:params:xml:ns:xmpp-stanzas">>}]);
encode({redirect, _} = Redirect) ->
    encode_error_redirect(Redirect,
			  [{<<"xmlns">>,
			    <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}]);
encode({error, _, _, _, _} = Error) ->
    encode_error(Error,
		 [{<<"xmlns">>, <<"jabber:client">>}]);
encode({bind, _, _} = Bind) ->
    encode_bind(Bind,
		[{<<"xmlns">>,
		  <<"urn:ietf:params:xml:ns:xmpp-bind">>}]);
encode({sasl_auth, _, _} = Auth) ->
    encode_sasl_auth(Auth,
		     [{<<"xmlns">>,
		       <<"urn:ietf:params:xml:ns:xmpp-sasl">>}]);
encode({sasl_abort} = Abort) ->
    encode_sasl_abort(Abort,
		      [{<<"xmlns">>,
			<<"urn:ietf:params:xml:ns:xmpp-sasl">>}]);
encode({sasl_challenge, _} = Challenge) ->
    encode_sasl_challenge(Challenge,
			  [{<<"xmlns">>,
			    <<"urn:ietf:params:xml:ns:xmpp-sasl">>}]);
encode({sasl_response, _} = Response) ->
    encode_sasl_response(Response,
			 [{<<"xmlns">>,
			   <<"urn:ietf:params:xml:ns:xmpp-sasl">>}]);
encode({sasl_success, _} = Success) ->
    encode_sasl_success(Success,
			[{<<"xmlns">>,
			  <<"urn:ietf:params:xml:ns:xmpp-sasl">>}]);
encode({sasl_failure, _, _} = Failure) ->
    encode_sasl_failure(Failure,
			[{<<"xmlns">>,
			  <<"urn:ietf:params:xml:ns:xmpp-sasl">>}]);
encode({sasl_mechanisms, _} = Mechanisms) ->
    encode_sasl_mechanisms(Mechanisms,
			   [{<<"xmlns">>,
			     <<"urn:ietf:params:xml:ns:xmpp-sasl">>}]);
encode({starttls, _} = Starttls) ->
    encode_starttls(Starttls,
		    [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-tls">>}]);
encode({starttls_proceed} = Proceed) ->
    encode_starttls_proceed(Proceed,
			    [{<<"xmlns">>,
			      <<"urn:ietf:params:xml:ns:xmpp-tls">>}]);
encode({starttls_failure} = Failure) ->
    encode_starttls_failure(Failure,
			    [{<<"xmlns">>,
			      <<"urn:ietf:params:xml:ns:xmpp-tls">>}]);
encode({compress_failure, _} = Failure) ->
    encode_compress_failure(Failure,
			    [{<<"xmlns">>,
			      <<"http://jabber.org/protocol/compress">>}]);
encode({compress, _} = Compress) ->
    encode_compress(Compress,
		    [{<<"xmlns">>,
		      <<"http://jabber.org/protocol/compress">>}]);
encode({compressed} = Compressed) ->
    encode_compressed(Compressed,
		      [{<<"xmlns">>,
			<<"http://jabber.org/protocol/compress">>}]);
encode({compression, _} = Compression) ->
    encode_compression(Compression,
		       [{<<"xmlns">>,
			 <<"http://jabber.org/features/compress">>}]);
encode({stream_features, _} = Stream_features) ->
    encode_stream_features(Stream_features,
			   [{<<"xmlns">>,
			     <<"http://etherx.jabber.org/streams">>}]);
encode({p1_push} = Push) ->
    encode_p1_push(Push, [{<<"xmlns">>, <<"p1:push">>}]);
encode({p1_rebind} = Rebind) ->
    encode_p1_rebind(Rebind,
		     [{<<"xmlns">>, <<"p1:rebind">>}]);
encode({p1_ack} = Ack) ->
    encode_p1_ack(Ack, [{<<"xmlns">>, <<"p1:ack">>}]);
encode({caps, _, _, _} = C) ->
    encode_caps(C,
		[{<<"xmlns">>, <<"http://jabber.org/protocol/caps">>}]);
encode({feature_register} = Register) ->
    encode_feature_register(Register,
			    [{<<"xmlns">>,
			      <<"http://jabber.org/features/iq-register">>}]);
encode({register, _, _, _, _, _, _, _, _, _, _, _, _, _,
	_, _, _, _, _, _, _, _} =
	   Query) ->
    encode_register(Query,
		    [{<<"xmlns">>, <<"jabber:iq:register">>}]);
encode({session} = Session) ->
    encode_session(Session,
		   [{<<"xmlns">>,
		     <<"urn:ietf:params:xml:ns:xmpp-session">>}]);
encode({ping} = Ping) ->
    encode_ping(Ping, [{<<"xmlns">>, <<"urn:xmpp:ping">>}]);
encode({time, _, _} = Time) ->
    encode_time(Time, [{<<"xmlns">>, <<"urn:xmpp:time">>}]);
encode({text, _, _} = Text) ->
    encode_stream_error_text(Text, []);
encode({'see-other-host', _} = See_other_host) ->
    encode_stream_error_see_other_host(See_other_host,
				       [{<<"xmlns">>,
					 <<"urn:ietf:params:xml:ns:xmpp-streams">>}]);
encode({stream_error, _, _} = Stream_error) ->
    encode_stream_error(Stream_error,
			[{<<"xmlns">>,
			  <<"http://etherx.jabber.org/streams">>}]);
encode({vcard_name, _, _, _, _, _} = N) ->
    encode_vcard_N(N, [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_adr, _, _, _, _, _, _, _, _, _, _, _, _,
	_, _} =
	   Adr) ->
    encode_vcard_ADR(Adr,
		     [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_label, _, _, _, _, _, _, _, _} = Label) ->
    encode_vcard_LABEL(Label,
		       [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_tel, _, _, _, _, _, _, _, _, _, _, _, _,
	_, _} =
	   Tel) ->
    encode_vcard_TEL(Tel,
		     [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_email, _, _, _, _, _, _} = Email) ->
    encode_vcard_EMAIL(Email,
		       [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_geo, _, _} = Geo) ->
    encode_vcard_GEO(Geo,
		     [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_logo, _, _, _} = Logo) ->
    encode_vcard_LOGO(Logo,
		      [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_photo, _, _, _} = Photo) ->
    encode_vcard_PHOTO(Photo,
		       [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_org, _, _} = Org) ->
    encode_vcard_ORG(Org,
		     [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_sound, _, _, _} = Sound) ->
    encode_vcard_SOUND(Sound,
		       [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_key, _, _} = Key) ->
    encode_vcard_KEY(Key,
		     [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
	_, _, _, _, _, _, _, _, _, _, _, _, _, _, _} =
	   Vcard) ->
    encode_vcard(Vcard, [{<<"xmlns">>, <<"vcard-temp">>}]);
encode({vcard_xupdate, _} = X) ->
    encode_vcard_xupdate(X,
			 [{<<"xmlns">>, <<"vcard-temp:x:update">>}]);
encode({xdata_field, _, _, _, _, _, _, _} = Field) ->
    encode_xdata_field(Field,
		       [{<<"xmlns">>, <<"jabber:x:data">>}]);
encode({xdata, _, _, _, _, _, _} = X) ->
    encode_xdata(X, [{<<"xmlns">>, <<"jabber:x:data">>}]);
encode({pubsub_subscription, _, _, _, _} =
	   Subscription) ->
    encode_pubsub_subscription(Subscription,
			       [{<<"xmlns">>,
				 <<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_affiliation, _, _} = Affiliation) ->
    encode_pubsub_affiliation(Affiliation,
			      [{<<"xmlns">>,
				<<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_item, _, _} = Item) ->
    encode_pubsub_item(Item,
		       [{<<"xmlns">>,
			 <<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_items, _, _, _, _} = Items) ->
    encode_pubsub_items(Items,
			[{<<"xmlns">>,
			  <<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_event_item, _, _, _} = Item) ->
    encode_pubsub_event_item(Item,
			     [{<<"xmlns">>,
			       <<"http://jabber.org/protocol/pubsub#event">>}]);
encode({pubsub_event_items, _, _, _} = Items) ->
    encode_pubsub_event_items(Items,
			      [{<<"xmlns">>,
				<<"http://jabber.org/protocol/pubsub#event">>}]);
encode({pubsub_event, _} = Event) ->
    encode_pubsub_event(Event,
			[{<<"xmlns">>,
			  <<"http://jabber.org/protocol/pubsub#event">>}]);
encode({pubsub_subscribe, _, _} = Subscribe) ->
    encode_pubsub_subscribe(Subscribe,
			    [{<<"xmlns">>,
			      <<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_unsubscribe, _, _, _} = Unsubscribe) ->
    encode_pubsub_unsubscribe(Unsubscribe,
			      [{<<"xmlns">>,
				<<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_publish, _, _} = Publish) ->
    encode_pubsub_publish(Publish,
			  [{<<"xmlns">>,
			    <<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_options, _, _, _, _} = Options) ->
    encode_pubsub_options(Options,
			  [{<<"xmlns">>,
			    <<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub_retract, _, _, _} = Retract) ->
    encode_pubsub_retract(Retract,
			  [{<<"xmlns">>,
			    <<"http://jabber.org/protocol/pubsub">>}]);
encode({pubsub, _, _, _, _, _, _, _, _} = Pubsub) ->
    encode_pubsub(Pubsub,
		  [{<<"xmlns">>,
		    <<"http://jabber.org/protocol/pubsub">>}]);
encode({shim, _} = Headers) ->
    encode_shim_headers(Headers,
			[{<<"xmlns">>, <<"http://jabber.org/protocol/shim">>}]);
encode({chatstate, active} = Active) ->
    encode_chatstate_active(Active,
			    [{<<"xmlns">>,
			      <<"http://jabber.org/protocol/chatstates">>}]);
encode({chatstate, composing} = Composing) ->
    encode_chatstate_composing(Composing,
			       [{<<"xmlns">>,
				 <<"http://jabber.org/protocol/chatstates">>}]);
encode({chatstate, gone} = Gone) ->
    encode_chatstate_gone(Gone,
			  [{<<"xmlns">>,
			    <<"http://jabber.org/protocol/chatstates">>}]);
encode({chatstate, inactive} = Inactive) ->
    encode_chatstate_inactive(Inactive,
			      [{<<"xmlns">>,
				<<"http://jabber.org/protocol/chatstates">>}]);
encode({chatstate, paused} = Paused) ->
    encode_chatstate_paused(Paused,
			    [{<<"xmlns">>,
			      <<"http://jabber.org/protocol/chatstates">>}]);
encode({delay, _, _} = Delay) ->
    encode_delay(Delay,
		 [{<<"xmlns">>, <<"urn:xmpp:delay">>}]);
encode({streamhost, _, _, _} = Streamhost) ->
    encode_bytestreams_streamhost(Streamhost,
				  [{<<"xmlns">>,
				    <<"http://jabber.org/protocol/bytestreams">>}]);
encode({bytestreams, _, _, _, _, _, _} = Query) ->
    encode_bytestreams(Query,
		       [{<<"xmlns">>,
			 <<"http://jabber.org/protocol/bytestreams">>}]);
encode({muc_history, _, _, _, _} = History) ->
    encode_muc_history(History,
		       [{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]);
encode({muc_decline, _, _, _} = Decline) ->
    encode_muc_user_decline(Decline,
			    [{<<"xmlns">>,
			      <<"http://jabber.org/protocol/muc#user">>}]);
encode({muc_user_destroy, _, _} = Destroy) ->
    encode_muc_user_destroy(Destroy,
			    [{<<"xmlns">>,
			      <<"http://jabber.org/protocol/muc#user">>}]);
encode({muc_invite, _, _, _} = Invite) ->
    encode_muc_user_invite(Invite,
			   [{<<"xmlns">>,
			     <<"http://jabber.org/protocol/muc#user">>}]);
encode({muc_user, _, _, _, _, _, _} = X) ->
    encode_muc_user(X,
		    [{<<"xmlns">>,
		      <<"http://jabber.org/protocol/muc#user">>}]);
encode({muc_owner_destroy, _, _, _} = Destroy) ->
    encode_muc_owner_destroy(Destroy,
			     [{<<"xmlns">>,
			       <<"http://jabber.org/protocol/muc#owner">>}]);
encode({muc_owner, _, _} = Query) ->
    encode_muc_owner(Query,
		     [{<<"xmlns">>,
		       <<"http://jabber.org/protocol/muc#owner">>}]);
encode({muc_item, _, _, _, _, _, _, _} = Item) ->
    encode_muc_admin_item(Item, []);
encode({muc_actor, _, _} = Actor) ->
    encode_muc_admin_actor(Actor, []);
encode({muc_admin, _} = Query) ->
    encode_muc_admin(Query,
		     [{<<"xmlns">>,
		       <<"http://jabber.org/protocol/muc#admin">>}]);
encode({muc, _, _} = X) ->
    encode_muc(X,
	       [{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]);
encode({rsm_first, _, _} = First) ->
    encode_rsm_first(First,
		     [{<<"xmlns">>, <<"http://jabber.org/protocol/rsm">>}]);
encode({rsm_set, _, _, _, _, _, _, _} = Set) ->
    encode_rsm_set(Set,
		   [{<<"xmlns">>, <<"http://jabber.org/protocol/rsm">>}]);
encode({mam_query, _, _, _, _, _, _, _} = Query) ->
    encode_mam_query(Query, []);
encode({mam_archived, _, _} = Archived) ->
    encode_mam_archived(Archived,
			[{<<"xmlns">>, <<"urn:xmpp:mam:tmp">>}]);
encode({mam_result, _, _, _, _} = Result) ->
    encode_mam_result(Result, []);
encode({mam_prefs, _, _, _, _} = Prefs) ->
    encode_mam_prefs(Prefs, []);
encode({mam_fin, _, _, _, _} = Fin) ->
    encode_mam_fin(Fin,
		   [{<<"xmlns">>, <<"urn:xmpp:mam:0">>}]);
encode({forwarded, _, _} = Forwarded) ->
    encode_forwarded(Forwarded,
		     [{<<"xmlns">>, <<"urn:xmpp:forward:0">>}]);
encode({carbons_disable} = Disable) ->
    encode_carbons_disable(Disable,
			   [{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}]);
encode({carbons_enable} = Enable) ->
    encode_carbons_enable(Enable,
			  [{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}]);
encode({carbons_private} = Private) ->
    encode_carbons_private(Private,
			   [{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}]);
encode({carbons_received, _} = Received) ->
    encode_carbons_received(Received,
			    [{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}]);
encode({carbons_sent, _} = Sent) ->
    encode_carbons_sent(Sent,
			[{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}]);
encode({feature_csi, _} = Csi) ->
    encode_feature_csi(Csi, []);
encode({csi, active} = Active) ->
    encode_csi_active(Active,
		      [{<<"xmlns">>, <<"urn:xmpp:csi:0">>}]);
encode({csi, inactive} = Inactive) ->
    encode_csi_inactive(Inactive,
			[{<<"xmlns">>, <<"urn:xmpp:csi:0">>}]);
encode({feature_sm, _} = Sm) ->
    encode_feature_sm(Sm, []);
encode({sm_enable, _, _, _} = Enable) ->
    encode_sm_enable(Enable, []);
encode({sm_enabled, _, _, _, _, _} = Enabled) ->
    encode_sm_enabled(Enabled, []);
encode({sm_resume, _, _, _} = Resume) ->
    encode_sm_resume(Resume, []);
encode({sm_resumed, _, _, _} = Resumed) ->
    encode_sm_resumed(Resumed, []);
encode({sm_r, _} = R) -> encode_sm_r(R, []);
encode({sm_a, _, _} = A) -> encode_sm_a(A, []);
encode({sm_failed, _, _} = Failed) ->
    encode_sm_failed(Failed, []).

get_ns({last, _, _}) -> <<"jabber:iq:last">>;
get_ns({version, _, _, _}) -> <<"jabber:iq:version">>;
get_ns({roster_item, _, _, _, _, _}) ->
    <<"jabber:iq:roster">>;
get_ns({roster, _, _}) -> <<"jabber:iq:roster">>;
get_ns({privacy_item, _, _, _, _, _}) ->
    <<"jabber:iq:privacy">>;
get_ns({privacy_list, _, _}) -> <<"jabber:iq:privacy">>;
get_ns({privacy, _, _, _}) -> <<"jabber:iq:privacy">>;
get_ns({block, _}) -> <<"urn:xmpp:blocking">>;
get_ns({unblock, _}) -> <<"urn:xmpp:blocking">>;
get_ns({block_list}) -> <<"urn:xmpp:blocking">>;
get_ns({identity, _, _, _, _}) ->
    <<"http://jabber.org/protocol/disco#info">>;
get_ns({disco_info, _, _, _, _}) ->
    <<"http://jabber.org/protocol/disco#info">>;
get_ns({disco_item, _, _, _}) ->
    <<"http://jabber.org/protocol/disco#items">>;
get_ns({disco_items, _, _}) ->
    <<"http://jabber.org/protocol/disco#items">>;
get_ns({private, _}) -> <<"jabber:iq:private">>;
get_ns({bookmark_conference, _, _, _, _, _}) ->
    <<"storage:bookmarks">>;
get_ns({bookmark_url, _, _}) -> <<"storage:bookmarks">>;
get_ns({bookmark_storage, _, _}) ->
    <<"storage:bookmarks">>;
get_ns({stat, _, _, _, _}) ->
    <<"http://jabber.org/protocol/stats">>;
get_ns({stats, _}) ->
    <<"http://jabber.org/protocol/stats">>;
get_ns({iq, _, _, _, _, _, _, _}) ->
    <<"jabber:client">>;
get_ns({message, _, _, _, _, _, _, _, _, _, _}) ->
    <<"jabber:client">>;
get_ns({presence, _, _, _, _, _, _, _, _, _, _}) ->
    <<"jabber:client">>;
get_ns({gone, _}) ->
    <<"urn:ietf:params:xml:ns:xmpp-stanzas">>;
get_ns({redirect, _}) ->
    <<"urn:ietf:params:xml:ns:xmpp-stanzas">>;
get_ns({error, _, _, _, _}) -> <<"jabber:client">>;
get_ns({bind, _, _}) ->
    <<"urn:ietf:params:xml:ns:xmpp-bind">>;
get_ns({sasl_auth, _, _}) ->
    <<"urn:ietf:params:xml:ns:xmpp-sasl">>;
get_ns({sasl_abort}) ->
    <<"urn:ietf:params:xml:ns:xmpp-sasl">>;
get_ns({sasl_challenge, _}) ->
    <<"urn:ietf:params:xml:ns:xmpp-sasl">>;
get_ns({sasl_response, _}) ->
    <<"urn:ietf:params:xml:ns:xmpp-sasl">>;
get_ns({sasl_success, _}) ->
    <<"urn:ietf:params:xml:ns:xmpp-sasl">>;
get_ns({sasl_failure, _, _}) ->
    <<"urn:ietf:params:xml:ns:xmpp-sasl">>;
get_ns({sasl_mechanisms, _}) ->
    <<"urn:ietf:params:xml:ns:xmpp-sasl">>;
get_ns({starttls, _}) ->
    <<"urn:ietf:params:xml:ns:xmpp-tls">>;
get_ns({starttls_proceed}) ->
    <<"urn:ietf:params:xml:ns:xmpp-tls">>;
get_ns({starttls_failure}) ->
    <<"urn:ietf:params:xml:ns:xmpp-tls">>;
get_ns({compress_failure, _}) ->
    <<"http://jabber.org/protocol/compress">>;
get_ns({compress, _}) ->
    <<"http://jabber.org/protocol/compress">>;
get_ns({compressed}) ->
    <<"http://jabber.org/protocol/compress">>;
get_ns({compression, _}) ->
    <<"http://jabber.org/features/compress">>;
get_ns({stream_features, _}) ->
    <<"http://etherx.jabber.org/streams">>;
get_ns({p1_push}) -> <<"p1:push">>;
get_ns({p1_rebind}) -> <<"p1:rebind">>;
get_ns({p1_ack}) -> <<"p1:ack">>;
get_ns({caps, _, _, _}) ->
    <<"http://jabber.org/protocol/caps">>;
get_ns({feature_register}) ->
    <<"http://jabber.org/features/iq-register">>;
get_ns({register, _, _, _, _, _, _, _, _, _, _, _, _, _,
	_, _, _, _, _, _, _, _}) ->
    <<"jabber:iq:register">>;
get_ns({session}) ->
    <<"urn:ietf:params:xml:ns:xmpp-session">>;
get_ns({ping}) -> <<"urn:xmpp:ping">>;
get_ns({time, _, _}) -> <<"urn:xmpp:time">>;
get_ns({'see-other-host', _}) ->
    <<"urn:ietf:params:xml:ns:xmpp-streams">>;
get_ns({stream_error, _, _}) ->
    <<"http://etherx.jabber.org/streams">>;
get_ns({vcard_name, _, _, _, _, _}) -> <<"vcard-temp">>;
get_ns({vcard_adr, _, _, _, _, _, _, _, _, _, _, _, _,
	_, _}) ->
    <<"vcard-temp">>;
get_ns({vcard_label, _, _, _, _, _, _, _, _}) ->
    <<"vcard-temp">>;
get_ns({vcard_tel, _, _, _, _, _, _, _, _, _, _, _, _,
	_, _}) ->
    <<"vcard-temp">>;
get_ns({vcard_email, _, _, _, _, _, _}) ->
    <<"vcard-temp">>;
get_ns({vcard_geo, _, _}) -> <<"vcard-temp">>;
get_ns({vcard_logo, _, _, _}) -> <<"vcard-temp">>;
get_ns({vcard_photo, _, _, _}) -> <<"vcard-temp">>;
get_ns({vcard_org, _, _}) -> <<"vcard-temp">>;
get_ns({vcard_sound, _, _, _}) -> <<"vcard-temp">>;
get_ns({vcard_key, _, _}) -> <<"vcard-temp">>;
get_ns({vcard, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
	_, _, _, _, _, _, _, _, _, _, _, _, _, _, _}) ->
    <<"vcard-temp">>;
get_ns({vcard_xupdate, _}) -> <<"vcard-temp:x:update">>;
get_ns({xdata_field, _, _, _, _, _, _, _}) ->
    <<"jabber:x:data">>;
get_ns({xdata, _, _, _, _, _, _}) ->
    <<"jabber:x:data">>;
get_ns({pubsub_subscription, _, _, _, _}) ->
    <<"http://jabber.org/protocol/pubsub">>;
get_ns({pubsub_affiliation, _, _}) ->
    <<"http://jabber.org/protocol/pubsub">>;
get_ns({pubsub_item, _, _}) ->
    <<"http://jabber.org/protocol/pubsub">>;
get_ns({pubsub_items, _, _, _, _}) ->
    <<"http://jabber.org/protocol/pubsub">>;
get_ns({pubsub_event_item, _, _, _}) ->
    <<"http://jabber.org/protocol/pubsub#event">>;
get_ns({pubsub_event_items, _, _, _}) ->
    <<"http://jabber.org/protocol/pubsub#event">>;
get_ns({pubsub_event, _}) ->
    <<"http://jabber.org/protocol/pubsub#event">>;
get_ns({pubsub_subscribe, _, _}) ->
    <<"http://jabber.org/protocol/pubsub">>;
get_ns({pubsub_unsubscribe, _, _, _}) ->
    <<"http://jabber.org/protocol/pubsub">>;
get_ns({pubsub_publish, _, _}) ->
    <<"http://jabber.org/protocol/pubsub">>;
get_ns({pubsub_options, _, _, _, _}) ->
    <<"http://jabber.org/protocol/pubsub">>;
get_ns({pubsub_retract, _, _, _}) ->
    <<"http://jabber.org/protocol/pubsub">>;
get_ns({pubsub, _, _, _, _, _, _, _, _}) ->
    <<"http://jabber.org/protocol/pubsub">>;
get_ns({shim, _}) ->
    <<"http://jabber.org/protocol/shim">>;
get_ns({chatstate, active}) ->
    <<"http://jabber.org/protocol/chatstates">>;
get_ns({chatstate, composing}) ->
    <<"http://jabber.org/protocol/chatstates">>;
get_ns({chatstate, gone}) ->
    <<"http://jabber.org/protocol/chatstates">>;
get_ns({chatstate, inactive}) ->
    <<"http://jabber.org/protocol/chatstates">>;
get_ns({chatstate, paused}) ->
    <<"http://jabber.org/protocol/chatstates">>;
get_ns({delay, _, _}) -> <<"urn:xmpp:delay">>;
get_ns({streamhost, _, _, _}) ->
    <<"http://jabber.org/protocol/bytestreams">>;
get_ns({bytestreams, _, _, _, _, _, _}) ->
    <<"http://jabber.org/protocol/bytestreams">>;
get_ns({muc_history, _, _, _, _}) ->
    <<"http://jabber.org/protocol/muc">>;
get_ns({muc_decline, _, _, _}) ->
    <<"http://jabber.org/protocol/muc#user">>;
get_ns({muc_user_destroy, _, _}) ->
    <<"http://jabber.org/protocol/muc#user">>;
get_ns({muc_invite, _, _, _}) ->
    <<"http://jabber.org/protocol/muc#user">>;
get_ns({muc_user, _, _, _, _, _, _}) ->
    <<"http://jabber.org/protocol/muc#user">>;
get_ns({muc_owner_destroy, _, _, _}) ->
    <<"http://jabber.org/protocol/muc#owner">>;
get_ns({muc_owner, _, _}) ->
    <<"http://jabber.org/protocol/muc#owner">>;
get_ns({muc_admin, _}) ->
    <<"http://jabber.org/protocol/muc#admin">>;
get_ns({muc, _, _}) ->
    <<"http://jabber.org/protocol/muc">>;
get_ns({rsm_first, _, _}) ->
    <<"http://jabber.org/protocol/rsm">>;
get_ns({rsm_set, _, _, _, _, _, _, _}) ->
    <<"http://jabber.org/protocol/rsm">>;
get_ns({mam_archived, _, _}) -> <<"urn:xmpp:mam:tmp">>;
get_ns({mam_fin, _, _, _, _}) -> <<"urn:xmpp:mam:0">>;
get_ns({forwarded, _, _}) -> <<"urn:xmpp:forward:0">>;
get_ns({carbons_disable}) -> <<"urn:xmpp:carbons:2">>;
get_ns({carbons_enable}) -> <<"urn:xmpp:carbons:2">>;
get_ns({carbons_private}) -> <<"urn:xmpp:carbons:2">>;
get_ns({carbons_received, _}) ->
    <<"urn:xmpp:carbons:2">>;
get_ns({carbons_sent, _}) -> <<"urn:xmpp:carbons:2">>;
get_ns({feature_csi, _}) -> <<"urn:xmpp:csi:0">>;
get_ns({csi, active}) -> <<"urn:xmpp:csi:0">>;
get_ns({csi, inactive}) -> <<"urn:xmpp:csi:0">>;
get_ns(_) -> <<>>.

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
pp(register, 21) ->
    [registered, remove, instructions, username, nick,
     password, name, first, last, email, address, city,
     state, zip, phone, url, date, misc, text, key, xdata];
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
pp(vcard_xupdate, 1) -> [photo];
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
pp(chatstate, 1) -> [type];
pp(delay, 2) -> [stamp, from];
pp(streamhost, 3) -> [jid, host, port];
pp(bytestreams, 6) ->
    [hosts, used, activate, dstaddr, mode, sid];
pp(muc_history, 4) ->
    [maxchars, maxstanzas, seconds, since];
pp(muc_decline, 3) -> [reason, from, to];
pp(muc_user_destroy, 2) -> [reason, jid];
pp(muc_invite, 3) -> [reason, from, to];
pp(muc_user, 6) ->
    [decline, destroy, invites, items, status_codes,
     password];
pp(muc_owner_destroy, 3) -> [jid, reason, password];
pp(muc_owner, 2) -> [destroy, config];
pp(muc_item, 7) ->
    [actor, continue, reason, affiliation, role, jid, nick];
pp(muc_actor, 2) -> [jid, nick];
pp(muc_admin, 1) -> [items];
pp(muc, 2) -> [history, password];
pp(rsm_first, 2) -> [index, data];
pp(rsm_set, 7) ->
    ['after', before, count, first, index, last, max];
pp(mam_query, 7) ->
    [xmlns, id, start, 'end', with, rsm, xdata];
pp(mam_archived, 2) -> [by, id];
pp(mam_result, 4) -> [xmlns, queryid, id, sub_els];
pp(mam_prefs, 4) -> [xmlns, default, always, never];
pp(mam_fin, 4) -> [id, rsm, stable, complete];
pp(forwarded, 2) -> [delay, sub_els];
pp(carbons_disable, 0) -> [];
pp(carbons_enable, 0) -> [];
pp(carbons_private, 0) -> [];
pp(carbons_received, 1) -> [forwarded];
pp(carbons_sent, 1) -> [forwarded];
pp(feature_csi, 1) -> [xmlns];
pp(csi, 1) -> [type];
pp(feature_sm, 1) -> [xmlns];
pp(sm_enable, 3) -> [max, resume, xmlns];
pp(sm_enabled, 5) -> [id, location, max, resume, xmlns];
pp(sm_resume, 3) -> [h, previd, xmlns];
pp(sm_resumed, 3) -> [h, previd, xmlns];
pp(sm_r, 1) -> [xmlns];
pp(sm_a, 2) -> [h, xmlns];
pp(sm_failed, 2) -> [reason, xmlns];
pp(_, _) -> no.

enc_bool(false) -> <<"false">>;
enc_bool(true) -> <<"true">>.

dec_bool(<<"false">>) -> false;
dec_bool(<<"0">>) -> false;
dec_bool(<<"true">>) -> true;
dec_bool(<<"1">>) -> true.

resourceprep(R) ->
    case jid:resourceprep(R) of
      error -> erlang:error(badarg);
      R1 -> R1
    end.

enc_jid(J) -> jid:to_string(J).

dec_jid(Val) ->
    case jid:from_string(Val) of
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

decode_sm_failed(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"failed">>, _attrs, _els}) ->
    Reason = decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
				  _els, undefined),
    Xmlns = decode_sm_failed_attrs(__TopXMLNS, _attrs,
				   undefined),
    {sm_failed, Reason, Xmlns}.

decode_sm_failed_els(__TopXMLNS, __IgnoreEls, [],
		     Reason) ->
    Reason;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"bad-request">>, _attrs, _} = _el | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_bad_request(_xmlns, __IgnoreEls,
							 _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"conflict">>, _attrs, _} = _el | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_conflict(_xmlns, __IgnoreEls,
						      _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"feature-not-implemented">>, _attrs, _} = _el
		      | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_feature_not_implemented(_xmlns,
								     __IgnoreEls,
								     _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"forbidden">>, _attrs, _} = _el | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_forbidden(_xmlns, __IgnoreEls,
						       _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"gone">>, _attrs, _} = _el | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_gone(_xmlns, __IgnoreEls, _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"internal-server-error">>, _attrs, _} = _el
		      | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_internal_server_error(_xmlns,
								   __IgnoreEls,
								   _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"item-not-found">>, _attrs, _} = _el | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_item_not_found(_xmlns, __IgnoreEls,
							    _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"jid-malformed">>, _attrs, _} = _el | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_jid_malformed(_xmlns, __IgnoreEls,
							   _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"not-acceptable">>, _attrs, _} = _el | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_not_acceptable(_xmlns, __IgnoreEls,
							    _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"not-allowed">>, _attrs, _} = _el | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_not_allowed(_xmlns, __IgnoreEls,
							 _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"not-authorized">>, _attrs, _} = _el | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_not_authorized(_xmlns, __IgnoreEls,
							    _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"policy-violation">>, _attrs, _} = _el
		      | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_policy_violation(_xmlns,
							      __IgnoreEls,
							      _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"recipient-unavailable">>, _attrs, _} = _el
		      | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_recipient_unavailable(_xmlns,
								   __IgnoreEls,
								   _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"redirect">>, _attrs, _} = _el | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_redirect(_xmlns, __IgnoreEls,
						      _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"registration-required">>, _attrs, _} = _el
		      | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_registration_required(_xmlns,
								   __IgnoreEls,
								   _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"remote-server-not-found">>, _attrs, _} = _el
		      | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_remote_server_not_found(_xmlns,
								     __IgnoreEls,
								     _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"remote-server-timeout">>, _attrs, _} = _el
		      | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_remote_server_timeout(_xmlns,
								   __IgnoreEls,
								   _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"resource-constraint">>, _attrs, _} = _el
		      | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_resource_constraint(_xmlns,
								 __IgnoreEls,
								 _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"service-unavailable">>, _attrs, _} = _el
		      | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_service_unavailable(_xmlns,
								 __IgnoreEls,
								 _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"subscription-required">>, _attrs, _} = _el
		      | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_subscription_required(_xmlns,
								   __IgnoreEls,
								   _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"undefined-condition">>, _attrs, _} = _el
		      | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_undefined_condition(_xmlns,
								 __IgnoreEls,
								 _el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"unexpected-request">>, _attrs, _} = _el
		      | _els],
		     Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				decode_error_unexpected_request(_xmlns,
								__IgnoreEls,
								_el));
       true ->
	   decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
				Reason)
    end;
decode_sm_failed_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Reason) ->
    decode_sm_failed_els(__TopXMLNS, __IgnoreEls, _els,
			 Reason).

decode_sm_failed_attrs(__TopXMLNS,
		       [{<<"xmlns">>, _val} | _attrs], _Xmlns) ->
    decode_sm_failed_attrs(__TopXMLNS, _attrs, _val);
decode_sm_failed_attrs(__TopXMLNS, [_ | _attrs],
		       Xmlns) ->
    decode_sm_failed_attrs(__TopXMLNS, _attrs, Xmlns);
decode_sm_failed_attrs(__TopXMLNS, [], Xmlns) ->
    decode_sm_failed_attr_xmlns(__TopXMLNS, Xmlns).

encode_sm_failed({sm_failed, Reason, Xmlns},
		 _xmlns_attrs) ->
    _els = lists:reverse('encode_sm_failed_$reason'(Reason,
						    [])),
    _attrs = encode_sm_failed_attr_xmlns(Xmlns,
					 _xmlns_attrs),
    {xmlel, <<"failed">>, _attrs, _els}.

'encode_sm_failed_$reason'(undefined, _acc) -> _acc;
'encode_sm_failed_$reason'('bad-request' = Reason,
			   _acc) ->
    [encode_error_bad_request(Reason,
			      [{<<"xmlns">>,
				<<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'(conflict = Reason, _acc) ->
    [encode_error_conflict(Reason,
			   [{<<"xmlns">>,
			     <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('feature-not-implemented' =
			       Reason,
			   _acc) ->
    [encode_error_feature_not_implemented(Reason,
					  [{<<"xmlns">>,
					    <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'(forbidden = Reason, _acc) ->
    [encode_error_forbidden(Reason,
			    [{<<"xmlns">>,
			      <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'({gone, _} = Reason, _acc) ->
    [encode_error_gone(Reason,
		       [{<<"xmlns">>,
			 <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('internal-server-error' =
			       Reason,
			   _acc) ->
    [encode_error_internal_server_error(Reason,
					[{<<"xmlns">>,
					  <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('item-not-found' = Reason,
			   _acc) ->
    [encode_error_item_not_found(Reason,
				 [{<<"xmlns">>,
				   <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('jid-malformed' = Reason,
			   _acc) ->
    [encode_error_jid_malformed(Reason,
				[{<<"xmlns">>,
				  <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('not-acceptable' = Reason,
			   _acc) ->
    [encode_error_not_acceptable(Reason,
				 [{<<"xmlns">>,
				   <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('not-allowed' = Reason,
			   _acc) ->
    [encode_error_not_allowed(Reason,
			      [{<<"xmlns">>,
				<<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('not-authorized' = Reason,
			   _acc) ->
    [encode_error_not_authorized(Reason,
				 [{<<"xmlns">>,
				   <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('policy-violation' = Reason,
			   _acc) ->
    [encode_error_policy_violation(Reason,
				   [{<<"xmlns">>,
				     <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('recipient-unavailable' =
			       Reason,
			   _acc) ->
    [encode_error_recipient_unavailable(Reason,
					[{<<"xmlns">>,
					  <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'({redirect, _} = Reason,
			   _acc) ->
    [encode_error_redirect(Reason,
			   [{<<"xmlns">>,
			     <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('registration-required' =
			       Reason,
			   _acc) ->
    [encode_error_registration_required(Reason,
					[{<<"xmlns">>,
					  <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('remote-server-not-found' =
			       Reason,
			   _acc) ->
    [encode_error_remote_server_not_found(Reason,
					  [{<<"xmlns">>,
					    <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('remote-server-timeout' =
			       Reason,
			   _acc) ->
    [encode_error_remote_server_timeout(Reason,
					[{<<"xmlns">>,
					  <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('resource-constraint' =
			       Reason,
			   _acc) ->
    [encode_error_resource_constraint(Reason,
				      [{<<"xmlns">>,
					<<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('service-unavailable' =
			       Reason,
			   _acc) ->
    [encode_error_service_unavailable(Reason,
				      [{<<"xmlns">>,
					<<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('subscription-required' =
			       Reason,
			   _acc) ->
    [encode_error_subscription_required(Reason,
					[{<<"xmlns">>,
					  <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('undefined-condition' =
			       Reason,
			   _acc) ->
    [encode_error_undefined_condition(Reason,
				      [{<<"xmlns">>,
					<<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc];
'encode_sm_failed_$reason'('unexpected-request' =
			       Reason,
			   _acc) ->
    [encode_error_unexpected_request(Reason,
				     [{<<"xmlns">>,
				       <<"urn:ietf:params:xml:ns:xmpp-stanzas">>}])
     | _acc].

decode_sm_failed_attr_xmlns(__TopXMLNS, undefined) ->
    undefined;
decode_sm_failed_attr_xmlns(__TopXMLNS, _val) -> _val.

encode_sm_failed_attr_xmlns(undefined, _acc) -> _acc;
encode_sm_failed_attr_xmlns(_val, _acc) ->
    [{<<"xmlns">>, _val} | _acc].

decode_sm_a(__TopXMLNS, __IgnoreEls,
	    {xmlel, <<"a">>, _attrs, _els}) ->
    {H, Xmlns} = decode_sm_a_attrs(__TopXMLNS, _attrs,
				   undefined, undefined),
    {sm_a, H, Xmlns}.

decode_sm_a_attrs(__TopXMLNS,
		  [{<<"h">>, _val} | _attrs], _H, Xmlns) ->
    decode_sm_a_attrs(__TopXMLNS, _attrs, _val, Xmlns);
decode_sm_a_attrs(__TopXMLNS,
		  [{<<"xmlns">>, _val} | _attrs], H, _Xmlns) ->
    decode_sm_a_attrs(__TopXMLNS, _attrs, H, _val);
decode_sm_a_attrs(__TopXMLNS, [_ | _attrs], H, Xmlns) ->
    decode_sm_a_attrs(__TopXMLNS, _attrs, H, Xmlns);
decode_sm_a_attrs(__TopXMLNS, [], H, Xmlns) ->
    {decode_sm_a_attr_h(__TopXMLNS, H),
     decode_sm_a_attr_xmlns(__TopXMLNS, Xmlns)}.

encode_sm_a({sm_a, H, Xmlns}, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_sm_a_attr_xmlns(Xmlns,
				    encode_sm_a_attr_h(H, _xmlns_attrs)),
    {xmlel, <<"a">>, _attrs, _els}.

decode_sm_a_attr_h(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"h">>, <<"a">>, __TopXMLNS}});
decode_sm_a_attr_h(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"h">>, <<"a">>, __TopXMLNS}});
      _res -> _res
    end.

encode_sm_a_attr_h(_val, _acc) ->
    [{<<"h">>, enc_int(_val)} | _acc].

decode_sm_a_attr_xmlns(__TopXMLNS, undefined) ->
    undefined;
decode_sm_a_attr_xmlns(__TopXMLNS, _val) -> _val.

encode_sm_a_attr_xmlns(undefined, _acc) -> _acc;
encode_sm_a_attr_xmlns(_val, _acc) ->
    [{<<"xmlns">>, _val} | _acc].

decode_sm_r(__TopXMLNS, __IgnoreEls,
	    {xmlel, <<"r">>, _attrs, _els}) ->
    Xmlns = decode_sm_r_attrs(__TopXMLNS, _attrs,
			      undefined),
    {sm_r, Xmlns}.

decode_sm_r_attrs(__TopXMLNS,
		  [{<<"xmlns">>, _val} | _attrs], _Xmlns) ->
    decode_sm_r_attrs(__TopXMLNS, _attrs, _val);
decode_sm_r_attrs(__TopXMLNS, [_ | _attrs], Xmlns) ->
    decode_sm_r_attrs(__TopXMLNS, _attrs, Xmlns);
decode_sm_r_attrs(__TopXMLNS, [], Xmlns) ->
    decode_sm_r_attr_xmlns(__TopXMLNS, Xmlns).

encode_sm_r({sm_r, Xmlns}, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_sm_r_attr_xmlns(Xmlns, _xmlns_attrs),
    {xmlel, <<"r">>, _attrs, _els}.

decode_sm_r_attr_xmlns(__TopXMLNS, undefined) ->
    undefined;
decode_sm_r_attr_xmlns(__TopXMLNS, _val) -> _val.

encode_sm_r_attr_xmlns(undefined, _acc) -> _acc;
encode_sm_r_attr_xmlns(_val, _acc) ->
    [{<<"xmlns">>, _val} | _acc].

decode_sm_resumed(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"resumed">>, _attrs, _els}) ->
    {H, Xmlns, Previd} = decode_sm_resumed_attrs(__TopXMLNS,
						 _attrs, undefined, undefined,
						 undefined),
    {sm_resumed, H, Previd, Xmlns}.

decode_sm_resumed_attrs(__TopXMLNS,
			[{<<"h">>, _val} | _attrs], _H, Xmlns, Previd) ->
    decode_sm_resumed_attrs(__TopXMLNS, _attrs, _val, Xmlns,
			    Previd);
decode_sm_resumed_attrs(__TopXMLNS,
			[{<<"xmlns">>, _val} | _attrs], H, _Xmlns, Previd) ->
    decode_sm_resumed_attrs(__TopXMLNS, _attrs, H, _val,
			    Previd);
decode_sm_resumed_attrs(__TopXMLNS,
			[{<<"previd">>, _val} | _attrs], H, Xmlns, _Previd) ->
    decode_sm_resumed_attrs(__TopXMLNS, _attrs, H, Xmlns,
			    _val);
decode_sm_resumed_attrs(__TopXMLNS, [_ | _attrs], H,
			Xmlns, Previd) ->
    decode_sm_resumed_attrs(__TopXMLNS, _attrs, H, Xmlns,
			    Previd);
decode_sm_resumed_attrs(__TopXMLNS, [], H, Xmlns,
			Previd) ->
    {decode_sm_resumed_attr_h(__TopXMLNS, H),
     decode_sm_resumed_attr_xmlns(__TopXMLNS, Xmlns),
     decode_sm_resumed_attr_previd(__TopXMLNS, Previd)}.

encode_sm_resumed({sm_resumed, H, Previd, Xmlns},
		  _xmlns_attrs) ->
    _els = [],
    _attrs = encode_sm_resumed_attr_previd(Previd,
					   encode_sm_resumed_attr_xmlns(Xmlns,
									encode_sm_resumed_attr_h(H,
												 _xmlns_attrs))),
    {xmlel, <<"resumed">>, _attrs, _els}.

decode_sm_resumed_attr_h(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"h">>, <<"resumed">>, __TopXMLNS}});
decode_sm_resumed_attr_h(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"h">>, <<"resumed">>, __TopXMLNS}});
      _res -> _res
    end.

encode_sm_resumed_attr_h(_val, _acc) ->
    [{<<"h">>, enc_int(_val)} | _acc].

decode_sm_resumed_attr_xmlns(__TopXMLNS, undefined) ->
    undefined;
decode_sm_resumed_attr_xmlns(__TopXMLNS, _val) -> _val.

encode_sm_resumed_attr_xmlns(undefined, _acc) -> _acc;
encode_sm_resumed_attr_xmlns(_val, _acc) ->
    [{<<"xmlns">>, _val} | _acc].

decode_sm_resumed_attr_previd(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"previd">>, <<"resumed">>,
		   __TopXMLNS}});
decode_sm_resumed_attr_previd(__TopXMLNS, _val) -> _val.

encode_sm_resumed_attr_previd(_val, _acc) ->
    [{<<"previd">>, _val} | _acc].

decode_sm_resume(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"resume">>, _attrs, _els}) ->
    {H, Xmlns, Previd} = decode_sm_resume_attrs(__TopXMLNS,
						_attrs, undefined, undefined,
						undefined),
    {sm_resume, H, Previd, Xmlns}.

decode_sm_resume_attrs(__TopXMLNS,
		       [{<<"h">>, _val} | _attrs], _H, Xmlns, Previd) ->
    decode_sm_resume_attrs(__TopXMLNS, _attrs, _val, Xmlns,
			   Previd);
decode_sm_resume_attrs(__TopXMLNS,
		       [{<<"xmlns">>, _val} | _attrs], H, _Xmlns, Previd) ->
    decode_sm_resume_attrs(__TopXMLNS, _attrs, H, _val,
			   Previd);
decode_sm_resume_attrs(__TopXMLNS,
		       [{<<"previd">>, _val} | _attrs], H, Xmlns, _Previd) ->
    decode_sm_resume_attrs(__TopXMLNS, _attrs, H, Xmlns,
			   _val);
decode_sm_resume_attrs(__TopXMLNS, [_ | _attrs], H,
		       Xmlns, Previd) ->
    decode_sm_resume_attrs(__TopXMLNS, _attrs, H, Xmlns,
			   Previd);
decode_sm_resume_attrs(__TopXMLNS, [], H, Xmlns,
		       Previd) ->
    {decode_sm_resume_attr_h(__TopXMLNS, H),
     decode_sm_resume_attr_xmlns(__TopXMLNS, Xmlns),
     decode_sm_resume_attr_previd(__TopXMLNS, Previd)}.

encode_sm_resume({sm_resume, H, Previd, Xmlns},
		 _xmlns_attrs) ->
    _els = [],
    _attrs = encode_sm_resume_attr_previd(Previd,
					  encode_sm_resume_attr_xmlns(Xmlns,
								      encode_sm_resume_attr_h(H,
											      _xmlns_attrs))),
    {xmlel, <<"resume">>, _attrs, _els}.

decode_sm_resume_attr_h(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"h">>, <<"resume">>, __TopXMLNS}});
decode_sm_resume_attr_h(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"h">>, <<"resume">>, __TopXMLNS}});
      _res -> _res
    end.

encode_sm_resume_attr_h(_val, _acc) ->
    [{<<"h">>, enc_int(_val)} | _acc].

decode_sm_resume_attr_xmlns(__TopXMLNS, undefined) ->
    undefined;
decode_sm_resume_attr_xmlns(__TopXMLNS, _val) -> _val.

encode_sm_resume_attr_xmlns(undefined, _acc) -> _acc;
encode_sm_resume_attr_xmlns(_val, _acc) ->
    [{<<"xmlns">>, _val} | _acc].

decode_sm_resume_attr_previd(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"previd">>, <<"resume">>,
		   __TopXMLNS}});
decode_sm_resume_attr_previd(__TopXMLNS, _val) -> _val.

encode_sm_resume_attr_previd(_val, _acc) ->
    [{<<"previd">>, _val} | _acc].

decode_sm_enabled(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"enabled">>, _attrs, _els}) ->
    {Id, Location, Xmlns, Max, Resume} =
	decode_sm_enabled_attrs(__TopXMLNS, _attrs, undefined,
				undefined, undefined, undefined, undefined),
    {sm_enabled, Id, Location, Max, Resume, Xmlns}.

decode_sm_enabled_attrs(__TopXMLNS,
			[{<<"id">>, _val} | _attrs], _Id, Location, Xmlns, Max,
			Resume) ->
    decode_sm_enabled_attrs(__TopXMLNS, _attrs, _val,
			    Location, Xmlns, Max, Resume);
decode_sm_enabled_attrs(__TopXMLNS,
			[{<<"location">>, _val} | _attrs], Id, _Location, Xmlns,
			Max, Resume) ->
    decode_sm_enabled_attrs(__TopXMLNS, _attrs, Id, _val,
			    Xmlns, Max, Resume);
decode_sm_enabled_attrs(__TopXMLNS,
			[{<<"xmlns">>, _val} | _attrs], Id, Location, _Xmlns,
			Max, Resume) ->
    decode_sm_enabled_attrs(__TopXMLNS, _attrs, Id,
			    Location, _val, Max, Resume);
decode_sm_enabled_attrs(__TopXMLNS,
			[{<<"max">>, _val} | _attrs], Id, Location, Xmlns, _Max,
			Resume) ->
    decode_sm_enabled_attrs(__TopXMLNS, _attrs, Id,
			    Location, Xmlns, _val, Resume);
decode_sm_enabled_attrs(__TopXMLNS,
			[{<<"resume">>, _val} | _attrs], Id, Location, Xmlns,
			Max, _Resume) ->
    decode_sm_enabled_attrs(__TopXMLNS, _attrs, Id,
			    Location, Xmlns, Max, _val);
decode_sm_enabled_attrs(__TopXMLNS, [_ | _attrs], Id,
			Location, Xmlns, Max, Resume) ->
    decode_sm_enabled_attrs(__TopXMLNS, _attrs, Id,
			    Location, Xmlns, Max, Resume);
decode_sm_enabled_attrs(__TopXMLNS, [], Id, Location,
			Xmlns, Max, Resume) ->
    {decode_sm_enabled_attr_id(__TopXMLNS, Id),
     decode_sm_enabled_attr_location(__TopXMLNS, Location),
     decode_sm_enabled_attr_xmlns(__TopXMLNS, Xmlns),
     decode_sm_enabled_attr_max(__TopXMLNS, Max),
     decode_sm_enabled_attr_resume(__TopXMLNS, Resume)}.

encode_sm_enabled({sm_enabled, Id, Location, Max,
		   Resume, Xmlns},
		  _xmlns_attrs) ->
    _els = [],
    _attrs = encode_sm_enabled_attr_resume(Resume,
					   encode_sm_enabled_attr_max(Max,
								      encode_sm_enabled_attr_xmlns(Xmlns,
												   encode_sm_enabled_attr_location(Location,
																   encode_sm_enabled_attr_id(Id,
																			     _xmlns_attrs))))),
    {xmlel, <<"enabled">>, _attrs, _els}.

decode_sm_enabled_attr_id(__TopXMLNS, undefined) ->
    undefined;
decode_sm_enabled_attr_id(__TopXMLNS, _val) -> _val.

encode_sm_enabled_attr_id(undefined, _acc) -> _acc;
encode_sm_enabled_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_sm_enabled_attr_location(__TopXMLNS,
				undefined) ->
    undefined;
decode_sm_enabled_attr_location(__TopXMLNS, _val) ->
    _val.

encode_sm_enabled_attr_location(undefined, _acc) ->
    _acc;
encode_sm_enabled_attr_location(_val, _acc) ->
    [{<<"location">>, _val} | _acc].

decode_sm_enabled_attr_xmlns(__TopXMLNS, undefined) ->
    undefined;
decode_sm_enabled_attr_xmlns(__TopXMLNS, _val) -> _val.

encode_sm_enabled_attr_xmlns(undefined, _acc) -> _acc;
encode_sm_enabled_attr_xmlns(_val, _acc) ->
    [{<<"xmlns">>, _val} | _acc].

decode_sm_enabled_attr_max(__TopXMLNS, undefined) ->
    undefined;
decode_sm_enabled_attr_max(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"max">>, <<"enabled">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_sm_enabled_attr_max(undefined, _acc) -> _acc;
encode_sm_enabled_attr_max(_val, _acc) ->
    [{<<"max">>, enc_int(_val)} | _acc].

decode_sm_enabled_attr_resume(__TopXMLNS, undefined) ->
    false;
decode_sm_enabled_attr_resume(__TopXMLNS, _val) ->
    case catch dec_bool(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"resume">>, <<"enabled">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_sm_enabled_attr_resume(false, _acc) -> _acc;
encode_sm_enabled_attr_resume(_val, _acc) ->
    [{<<"resume">>, enc_bool(_val)} | _acc].

decode_sm_enable(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"enable">>, _attrs, _els}) ->
    {Max, Xmlns, Resume} =
	decode_sm_enable_attrs(__TopXMLNS, _attrs, undefined,
			       undefined, undefined),
    {sm_enable, Max, Resume, Xmlns}.

decode_sm_enable_attrs(__TopXMLNS,
		       [{<<"max">>, _val} | _attrs], _Max, Xmlns, Resume) ->
    decode_sm_enable_attrs(__TopXMLNS, _attrs, _val, Xmlns,
			   Resume);
decode_sm_enable_attrs(__TopXMLNS,
		       [{<<"xmlns">>, _val} | _attrs], Max, _Xmlns, Resume) ->
    decode_sm_enable_attrs(__TopXMLNS, _attrs, Max, _val,
			   Resume);
decode_sm_enable_attrs(__TopXMLNS,
		       [{<<"resume">>, _val} | _attrs], Max, Xmlns, _Resume) ->
    decode_sm_enable_attrs(__TopXMLNS, _attrs, Max, Xmlns,
			   _val);
decode_sm_enable_attrs(__TopXMLNS, [_ | _attrs], Max,
		       Xmlns, Resume) ->
    decode_sm_enable_attrs(__TopXMLNS, _attrs, Max, Xmlns,
			   Resume);
decode_sm_enable_attrs(__TopXMLNS, [], Max, Xmlns,
		       Resume) ->
    {decode_sm_enable_attr_max(__TopXMLNS, Max),
     decode_sm_enable_attr_xmlns(__TopXMLNS, Xmlns),
     decode_sm_enable_attr_resume(__TopXMLNS, Resume)}.

encode_sm_enable({sm_enable, Max, Resume, Xmlns},
		 _xmlns_attrs) ->
    _els = [],
    _attrs = encode_sm_enable_attr_resume(Resume,
					  encode_sm_enable_attr_xmlns(Xmlns,
								      encode_sm_enable_attr_max(Max,
												_xmlns_attrs))),
    {xmlel, <<"enable">>, _attrs, _els}.

decode_sm_enable_attr_max(__TopXMLNS, undefined) ->
    undefined;
decode_sm_enable_attr_max(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"max">>, <<"enable">>, __TopXMLNS}});
      _res -> _res
    end.

encode_sm_enable_attr_max(undefined, _acc) -> _acc;
encode_sm_enable_attr_max(_val, _acc) ->
    [{<<"max">>, enc_int(_val)} | _acc].

decode_sm_enable_attr_xmlns(__TopXMLNS, undefined) ->
    undefined;
decode_sm_enable_attr_xmlns(__TopXMLNS, _val) -> _val.

encode_sm_enable_attr_xmlns(undefined, _acc) -> _acc;
encode_sm_enable_attr_xmlns(_val, _acc) ->
    [{<<"xmlns">>, _val} | _acc].

decode_sm_enable_attr_resume(__TopXMLNS, undefined) ->
    false;
decode_sm_enable_attr_resume(__TopXMLNS, _val) ->
    case catch dec_bool(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"resume">>, <<"enable">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_sm_enable_attr_resume(false, _acc) -> _acc;
encode_sm_enable_attr_resume(_val, _acc) ->
    [{<<"resume">>, enc_bool(_val)} | _acc].

decode_feature_sm(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"sm">>, _attrs, _els}) ->
    Xmlns = decode_feature_sm_attrs(__TopXMLNS, _attrs,
				    undefined),
    {feature_sm, Xmlns}.

decode_feature_sm_attrs(__TopXMLNS,
			[{<<"xmlns">>, _val} | _attrs], _Xmlns) ->
    decode_feature_sm_attrs(__TopXMLNS, _attrs, _val);
decode_feature_sm_attrs(__TopXMLNS, [_ | _attrs],
			Xmlns) ->
    decode_feature_sm_attrs(__TopXMLNS, _attrs, Xmlns);
decode_feature_sm_attrs(__TopXMLNS, [], Xmlns) ->
    decode_feature_sm_attr_xmlns(__TopXMLNS, Xmlns).

encode_feature_sm({feature_sm, Xmlns}, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_feature_sm_attr_xmlns(Xmlns,
					  _xmlns_attrs),
    {xmlel, <<"sm">>, _attrs, _els}.

decode_feature_sm_attr_xmlns(__TopXMLNS, undefined) ->
    undefined;
decode_feature_sm_attr_xmlns(__TopXMLNS, _val) -> _val.

encode_feature_sm_attr_xmlns(undefined, _acc) -> _acc;
encode_feature_sm_attr_xmlns(_val, _acc) ->
    [{<<"xmlns">>, _val} | _acc].

decode_csi_inactive(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"inactive">>, _attrs, _els}) ->
    {csi, inactive}.

encode_csi_inactive({csi, inactive}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"inactive">>, _attrs, _els}.

decode_csi_active(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"active">>, _attrs, _els}) ->
    {csi, active}.

encode_csi_active({csi, active}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"active">>, _attrs, _els}.

decode_feature_csi(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"csi">>, _attrs, _els}) ->
    Xmlns = decode_feature_csi_attrs(__TopXMLNS, _attrs,
				     undefined),
    {feature_csi, Xmlns}.

decode_feature_csi_attrs(__TopXMLNS,
			 [{<<"xmlns">>, _val} | _attrs], _Xmlns) ->
    decode_feature_csi_attrs(__TopXMLNS, _attrs, _val);
decode_feature_csi_attrs(__TopXMLNS, [_ | _attrs],
			 Xmlns) ->
    decode_feature_csi_attrs(__TopXMLNS, _attrs, Xmlns);
decode_feature_csi_attrs(__TopXMLNS, [], Xmlns) ->
    decode_feature_csi_attr_xmlns(__TopXMLNS, Xmlns).

encode_feature_csi({feature_csi, Xmlns},
		   _xmlns_attrs) ->
    _els = [],
    _attrs = encode_feature_csi_attr_xmlns(Xmlns,
					   _xmlns_attrs),
    {xmlel, <<"csi">>, _attrs, _els}.

decode_feature_csi_attr_xmlns(__TopXMLNS, undefined) ->
    undefined;
decode_feature_csi_attr_xmlns(__TopXMLNS, _val) -> _val.

encode_feature_csi_attr_xmlns(undefined, _acc) -> _acc;
encode_feature_csi_attr_xmlns(_val, _acc) ->
    [{<<"xmlns">>, _val} | _acc].

decode_carbons_sent(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"sent">>, _attrs, _els}) ->
    Forwarded = decode_carbons_sent_els(__TopXMLNS,
					__IgnoreEls, _els, error),
    {carbons_sent, Forwarded}.

decode_carbons_sent_els(__TopXMLNS, __IgnoreEls, [],
			Forwarded) ->
    case Forwarded of
      error ->
	  erlang:error({xmpp_codec,
			{missing_tag, <<"forwarded">>, __TopXMLNS}});
      {value, Forwarded1} -> Forwarded1
    end;
decode_carbons_sent_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"forwarded">>, _attrs, _} = _el | _els],
			Forwarded) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<"urn:xmpp:forward:0">> ->
	   decode_carbons_sent_els(__TopXMLNS, __IgnoreEls, _els,
				   {value,
				    decode_forwarded(_xmlns, __IgnoreEls,
						     _el)});
       true ->
	   decode_carbons_sent_els(__TopXMLNS, __IgnoreEls, _els,
				   Forwarded)
    end;
decode_carbons_sent_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Forwarded) ->
    decode_carbons_sent_els(__TopXMLNS, __IgnoreEls, _els,
			    Forwarded).

encode_carbons_sent({carbons_sent, Forwarded},
		    _xmlns_attrs) ->
    _els =
	lists:reverse('encode_carbons_sent_$forwarded'(Forwarded,
						       [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"sent">>, _attrs, _els}.

'encode_carbons_sent_$forwarded'(Forwarded, _acc) ->
    [encode_forwarded(Forwarded,
		      [{<<"xmlns">>, <<"urn:xmpp:forward:0">>}])
     | _acc].

decode_carbons_received(__TopXMLNS, __IgnoreEls,
			{xmlel, <<"received">>, _attrs, _els}) ->
    Forwarded = decode_carbons_received_els(__TopXMLNS,
					    __IgnoreEls, _els, error),
    {carbons_received, Forwarded}.

decode_carbons_received_els(__TopXMLNS, __IgnoreEls, [],
			    Forwarded) ->
    case Forwarded of
      error ->
	  erlang:error({xmpp_codec,
			{missing_tag, <<"forwarded">>, __TopXMLNS}});
      {value, Forwarded1} -> Forwarded1
    end;
decode_carbons_received_els(__TopXMLNS, __IgnoreEls,
			    [{xmlel, <<"forwarded">>, _attrs, _} = _el | _els],
			    Forwarded) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<"urn:xmpp:forward:0">> ->
	   decode_carbons_received_els(__TopXMLNS, __IgnoreEls,
				       _els,
				       {value,
					decode_forwarded(_xmlns, __IgnoreEls,
							 _el)});
       true ->
	   decode_carbons_received_els(__TopXMLNS, __IgnoreEls,
				       _els, Forwarded)
    end;
decode_carbons_received_els(__TopXMLNS, __IgnoreEls,
			    [_ | _els], Forwarded) ->
    decode_carbons_received_els(__TopXMLNS, __IgnoreEls,
				_els, Forwarded).

encode_carbons_received({carbons_received, Forwarded},
			_xmlns_attrs) ->
    _els =
	lists:reverse('encode_carbons_received_$forwarded'(Forwarded,
							   [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"received">>, _attrs, _els}.

'encode_carbons_received_$forwarded'(Forwarded, _acc) ->
    [encode_forwarded(Forwarded,
		      [{<<"xmlns">>, <<"urn:xmpp:forward:0">>}])
     | _acc].

decode_carbons_private(__TopXMLNS, __IgnoreEls,
		       {xmlel, <<"private">>, _attrs, _els}) ->
    {carbons_private}.

encode_carbons_private({carbons_private},
		       _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"private">>, _attrs, _els}.

decode_carbons_enable(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"enable">>, _attrs, _els}) ->
    {carbons_enable}.

encode_carbons_enable({carbons_enable}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"enable">>, _attrs, _els}.

decode_carbons_disable(__TopXMLNS, __IgnoreEls,
		       {xmlel, <<"disable">>, _attrs, _els}) ->
    {carbons_disable}.

encode_carbons_disable({carbons_disable},
		       _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"disable">>, _attrs, _els}.

decode_forwarded(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"forwarded">>, _attrs, _els}) ->
    {Delay, __Els} = decode_forwarded_els(__TopXMLNS,
					  __IgnoreEls, _els, undefined, []),
    {forwarded, Delay, __Els}.

decode_forwarded_els(__TopXMLNS, __IgnoreEls, [], Delay,
		     __Els) ->
    {Delay, lists:reverse(__Els)};
decode_forwarded_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"delay">>, _attrs, _} = _el | _els], Delay,
		     __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<"urn:xmpp:delay">> ->
	   decode_forwarded_els(__TopXMLNS, __IgnoreEls, _els,
				decode_delay(_xmlns, __IgnoreEls, _el), __Els);
       true ->
	   decode_forwarded_els(__TopXMLNS, __IgnoreEls, _els,
				Delay, __Els)
    end;
decode_forwarded_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, _, _, _} = _el | _els], Delay, __Els) ->
    if __IgnoreEls ->
	   decode_forwarded_els(__TopXMLNS, __IgnoreEls, _els,
				Delay, [_el | __Els]);
       true ->
	   case is_known_tag(_el) of
	     true ->
		 decode_forwarded_els(__TopXMLNS, __IgnoreEls, _els,
				      Delay, [decode(_el) | __Els]);
	     false ->
		 decode_forwarded_els(__TopXMLNS, __IgnoreEls, _els,
				      Delay, __Els)
	   end
    end;
decode_forwarded_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Delay, __Els) ->
    decode_forwarded_els(__TopXMLNS, __IgnoreEls, _els,
			 Delay, __Els).

encode_forwarded({forwarded, Delay, __Els},
		 _xmlns_attrs) ->
    _els = [encode(_el) || _el <- __Els] ++
	     lists:reverse('encode_forwarded_$delay'(Delay, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"forwarded">>, _attrs, _els}.

'encode_forwarded_$delay'(undefined, _acc) -> _acc;
'encode_forwarded_$delay'(Delay, _acc) ->
    [encode_delay(Delay,
		  [{<<"xmlns">>, <<"urn:xmpp:delay">>}])
     | _acc].

decode_mam_fin(__TopXMLNS, __IgnoreEls,
	       {xmlel, <<"fin">>, _attrs, _els}) ->
    Rsm = decode_mam_fin_els(__TopXMLNS, __IgnoreEls, _els,
			     undefined),
    {Id, Stable, Complete} =
	decode_mam_fin_attrs(__TopXMLNS, _attrs, undefined,
			     undefined, undefined),
    {mam_fin, Id, Rsm, Stable, Complete}.

decode_mam_fin_els(__TopXMLNS, __IgnoreEls, [], Rsm) ->
    Rsm;
decode_mam_fin_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"set">>, _attrs, _} = _el | _els], Rsm) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<"http://jabber.org/protocol/rsm">> ->
	   decode_mam_fin_els(__TopXMLNS, __IgnoreEls, _els,
			      decode_rsm_set(_xmlns, __IgnoreEls, _el));
       true ->
	   decode_mam_fin_els(__TopXMLNS, __IgnoreEls, _els, Rsm)
    end;
decode_mam_fin_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		   Rsm) ->
    decode_mam_fin_els(__TopXMLNS, __IgnoreEls, _els, Rsm).

decode_mam_fin_attrs(__TopXMLNS,
		     [{<<"queryid">>, _val} | _attrs], _Id, Stable,
		     Complete) ->
    decode_mam_fin_attrs(__TopXMLNS, _attrs, _val, Stable,
			 Complete);
decode_mam_fin_attrs(__TopXMLNS,
		     [{<<"stable">>, _val} | _attrs], Id, _Stable,
		     Complete) ->
    decode_mam_fin_attrs(__TopXMLNS, _attrs, Id, _val,
			 Complete);
decode_mam_fin_attrs(__TopXMLNS,
		     [{<<"complete">>, _val} | _attrs], Id, Stable,
		     _Complete) ->
    decode_mam_fin_attrs(__TopXMLNS, _attrs, Id, Stable,
			 _val);
decode_mam_fin_attrs(__TopXMLNS, [_ | _attrs], Id,
		     Stable, Complete) ->
    decode_mam_fin_attrs(__TopXMLNS, _attrs, Id, Stable,
			 Complete);
decode_mam_fin_attrs(__TopXMLNS, [], Id, Stable,
		     Complete) ->
    {decode_mam_fin_attr_queryid(__TopXMLNS, Id),
     decode_mam_fin_attr_stable(__TopXMLNS, Stable),
     decode_mam_fin_attr_complete(__TopXMLNS, Complete)}.

encode_mam_fin({mam_fin, Id, Rsm, Stable, Complete},
	       _xmlns_attrs) ->
    _els = lists:reverse('encode_mam_fin_$rsm'(Rsm, [])),
    _attrs = encode_mam_fin_attr_complete(Complete,
					  encode_mam_fin_attr_stable(Stable,
								     encode_mam_fin_attr_queryid(Id,
												 _xmlns_attrs))),
    {xmlel, <<"fin">>, _attrs, _els}.

'encode_mam_fin_$rsm'(undefined, _acc) -> _acc;
'encode_mam_fin_$rsm'(Rsm, _acc) ->
    [encode_rsm_set(Rsm,
		    [{<<"xmlns">>, <<"http://jabber.org/protocol/rsm">>}])
     | _acc].

decode_mam_fin_attr_queryid(__TopXMLNS, undefined) ->
    undefined;
decode_mam_fin_attr_queryid(__TopXMLNS, _val) -> _val.

encode_mam_fin_attr_queryid(undefined, _acc) -> _acc;
encode_mam_fin_attr_queryid(_val, _acc) ->
    [{<<"queryid">>, _val} | _acc].

decode_mam_fin_attr_stable(__TopXMLNS, undefined) ->
    undefined;
decode_mam_fin_attr_stable(__TopXMLNS, _val) ->
    case catch dec_bool(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"stable">>, <<"fin">>, __TopXMLNS}});
      _res -> _res
    end.

encode_mam_fin_attr_stable(undefined, _acc) -> _acc;
encode_mam_fin_attr_stable(_val, _acc) ->
    [{<<"stable">>, enc_bool(_val)} | _acc].

decode_mam_fin_attr_complete(__TopXMLNS, undefined) ->
    undefined;
decode_mam_fin_attr_complete(__TopXMLNS, _val) ->
    case catch dec_bool(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"complete">>, <<"fin">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_mam_fin_attr_complete(undefined, _acc) -> _acc;
encode_mam_fin_attr_complete(_val, _acc) ->
    [{<<"complete">>, enc_bool(_val)} | _acc].

decode_mam_prefs(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"prefs">>, _attrs, _els}) ->
    {Never, Always} = decode_mam_prefs_els(__TopXMLNS,
					   __IgnoreEls, _els, [], []),
    {Default, Xmlns} = decode_mam_prefs_attrs(__TopXMLNS,
					      _attrs, undefined, undefined),
    {mam_prefs, Xmlns, Default, Always, Never}.

decode_mam_prefs_els(__TopXMLNS, __IgnoreEls, [], Never,
		     Always) ->
    {Never, Always};
decode_mam_prefs_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"always">>, _attrs, _} = _el | _els], Never,
		     Always) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_mam_prefs_els(__TopXMLNS, __IgnoreEls, _els,
				Never,
				decode_mam_always(__TopXMLNS, __IgnoreEls,
						  _el));
       true ->
	   decode_mam_prefs_els(__TopXMLNS, __IgnoreEls, _els,
				Never, Always)
    end;
decode_mam_prefs_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"never">>, _attrs, _} = _el | _els], Never,
		     Always) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_mam_prefs_els(__TopXMLNS, __IgnoreEls, _els,
				decode_mam_never(__TopXMLNS, __IgnoreEls, _el),
				Always);
       true ->
	   decode_mam_prefs_els(__TopXMLNS, __IgnoreEls, _els,
				Never, Always)
    end;
decode_mam_prefs_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Never, Always) ->
    decode_mam_prefs_els(__TopXMLNS, __IgnoreEls, _els,
			 Never, Always).

decode_mam_prefs_attrs(__TopXMLNS,
		       [{<<"default">>, _val} | _attrs], _Default, Xmlns) ->
    decode_mam_prefs_attrs(__TopXMLNS, _attrs, _val, Xmlns);
decode_mam_prefs_attrs(__TopXMLNS,
		       [{<<"xmlns">>, _val} | _attrs], Default, _Xmlns) ->
    decode_mam_prefs_attrs(__TopXMLNS, _attrs, Default,
			   _val);
decode_mam_prefs_attrs(__TopXMLNS, [_ | _attrs],
		       Default, Xmlns) ->
    decode_mam_prefs_attrs(__TopXMLNS, _attrs, Default,
			   Xmlns);
decode_mam_prefs_attrs(__TopXMLNS, [], Default,
		       Xmlns) ->
    {decode_mam_prefs_attr_default(__TopXMLNS, Default),
     decode_mam_prefs_attr_xmlns(__TopXMLNS, Xmlns)}.

encode_mam_prefs({mam_prefs, Xmlns, Default, Always,
		  Never},
		 _xmlns_attrs) ->
    _els = lists:reverse('encode_mam_prefs_$never'(Never,
						   'encode_mam_prefs_$always'(Always,
									      []))),
    _attrs = encode_mam_prefs_attr_xmlns(Xmlns,
					 encode_mam_prefs_attr_default(Default,
								       _xmlns_attrs)),
    {xmlel, <<"prefs">>, _attrs, _els}.

'encode_mam_prefs_$never'([], _acc) -> _acc;
'encode_mam_prefs_$never'(Never, _acc) ->
    [encode_mam_never(Never,
		      [{<<"xmlns">>, <<"urn:xmpp:mam:tmp">>}])
     | _acc].

'encode_mam_prefs_$always'([], _acc) -> _acc;
'encode_mam_prefs_$always'(Always, _acc) ->
    [encode_mam_always(Always,
		       [{<<"xmlns">>, <<"urn:xmpp:mam:tmp">>}])
     | _acc].

decode_mam_prefs_attr_default(__TopXMLNS, undefined) ->
    undefined;
decode_mam_prefs_attr_default(__TopXMLNS, _val) ->
    case catch dec_enum(_val, [always, never, roster]) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"default">>, <<"prefs">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_mam_prefs_attr_default(undefined, _acc) -> _acc;
encode_mam_prefs_attr_default(_val, _acc) ->
    [{<<"default">>, enc_enum(_val)} | _acc].

decode_mam_prefs_attr_xmlns(__TopXMLNS, undefined) ->
    undefined;
decode_mam_prefs_attr_xmlns(__TopXMLNS, _val) -> _val.

encode_mam_prefs_attr_xmlns(undefined, _acc) -> _acc;
encode_mam_prefs_attr_xmlns(_val, _acc) ->
    [{<<"xmlns">>, _val} | _acc].

decode_mam_always(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"always">>, _attrs, _els}) ->
    Jids = decode_mam_always_els(__TopXMLNS, __IgnoreEls,
				 _els, []),
    Jids.

decode_mam_always_els(__TopXMLNS, __IgnoreEls, [],
		      Jids) ->
    lists:reverse(Jids);
decode_mam_always_els(__TopXMLNS, __IgnoreEls,
		      [{xmlel, <<"jid">>, _attrs, _} = _el | _els], Jids) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_mam_always_els(__TopXMLNS, __IgnoreEls, _els,
				 case decode_mam_jid(__TopXMLNS, __IgnoreEls,
						     _el)
				     of
				   [] -> Jids;
				   _new_el -> [_new_el | Jids]
				 end);
       true ->
	   decode_mam_always_els(__TopXMLNS, __IgnoreEls, _els,
				 Jids)
    end;
decode_mam_always_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Jids) ->
    decode_mam_always_els(__TopXMLNS, __IgnoreEls, _els,
			  Jids).

encode_mam_always(Jids, _xmlns_attrs) ->
    _els = lists:reverse('encode_mam_always_$jids'(Jids,
						   [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"always">>, _attrs, _els}.

'encode_mam_always_$jids'([], _acc) -> _acc;
'encode_mam_always_$jids'([Jids | _els], _acc) ->
    'encode_mam_always_$jids'(_els,
			      [encode_mam_jid(Jids, []) | _acc]).

decode_mam_never(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"never">>, _attrs, _els}) ->
    Jids = decode_mam_never_els(__TopXMLNS, __IgnoreEls,
				_els, []),
    Jids.

decode_mam_never_els(__TopXMLNS, __IgnoreEls, [],
		     Jids) ->
    lists:reverse(Jids);
decode_mam_never_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"jid">>, _attrs, _} = _el | _els], Jids) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_mam_never_els(__TopXMLNS, __IgnoreEls, _els,
				case decode_mam_jid(__TopXMLNS, __IgnoreEls,
						    _el)
				    of
				  [] -> Jids;
				  _new_el -> [_new_el | Jids]
				end);
       true ->
	   decode_mam_never_els(__TopXMLNS, __IgnoreEls, _els,
				Jids)
    end;
decode_mam_never_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Jids) ->
    decode_mam_never_els(__TopXMLNS, __IgnoreEls, _els,
			 Jids).

encode_mam_never(Jids, _xmlns_attrs) ->
    _els = lists:reverse('encode_mam_never_$jids'(Jids,
						  [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"never">>, _attrs, _els}.

'encode_mam_never_$jids'([], _acc) -> _acc;
'encode_mam_never_$jids'([Jids | _els], _acc) ->
    'encode_mam_never_$jids'(_els,
			     [encode_mam_jid(Jids, []) | _acc]).

decode_mam_jid(__TopXMLNS, __IgnoreEls,
	       {xmlel, <<"jid">>, _attrs, _els}) ->
    Cdata = decode_mam_jid_els(__TopXMLNS, __IgnoreEls,
			       _els, <<>>),
    Cdata.

decode_mam_jid_els(__TopXMLNS, __IgnoreEls, [],
		   Cdata) ->
    decode_mam_jid_cdata(__TopXMLNS, Cdata);
decode_mam_jid_els(__TopXMLNS, __IgnoreEls,
		   [{xmlcdata, _data} | _els], Cdata) ->
    decode_mam_jid_els(__TopXMLNS, __IgnoreEls, _els,
		       <<Cdata/binary, _data/binary>>);
decode_mam_jid_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		   Cdata) ->
    decode_mam_jid_els(__TopXMLNS, __IgnoreEls, _els,
		       Cdata).

encode_mam_jid(Cdata, _xmlns_attrs) ->
    _els = encode_mam_jid_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"jid">>, _attrs, _els}.

decode_mam_jid_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmpp_codec,
		  {missing_cdata, <<>>, <<"jid">>, __TopXMLNS}});
decode_mam_jid_cdata(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"jid">>, __TopXMLNS}});
      _res -> _res
    end.

encode_mam_jid_cdata(_val, _acc) ->
    [{xmlcdata, enc_jid(_val)} | _acc].

decode_mam_result(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"result">>, _attrs, _els}) ->
    __Els = decode_mam_result_els(__TopXMLNS, __IgnoreEls,
				  _els, []),
    {Queryid, Xmlns, Id} =
	decode_mam_result_attrs(__TopXMLNS, _attrs, undefined,
				undefined, undefined),
    {mam_result, Xmlns, Queryid, Id, __Els}.

decode_mam_result_els(__TopXMLNS, __IgnoreEls, [],
		      __Els) ->
    lists:reverse(__Els);
decode_mam_result_els(__TopXMLNS, __IgnoreEls,
		      [{xmlel, _, _, _} = _el | _els], __Els) ->
    if __IgnoreEls ->
	   decode_mam_result_els(__TopXMLNS, __IgnoreEls, _els,
				 [_el | __Els]);
       true ->
	   case is_known_tag(_el) of
	     true ->
		 decode_mam_result_els(__TopXMLNS, __IgnoreEls, _els,
				       [decode(_el) | __Els]);
	     false ->
		 decode_mam_result_els(__TopXMLNS, __IgnoreEls, _els,
				       __Els)
	   end
    end;
decode_mam_result_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], __Els) ->
    decode_mam_result_els(__TopXMLNS, __IgnoreEls, _els,
			  __Els).

decode_mam_result_attrs(__TopXMLNS,
			[{<<"queryid">>, _val} | _attrs], _Queryid, Xmlns,
			Id) ->
    decode_mam_result_attrs(__TopXMLNS, _attrs, _val, Xmlns,
			    Id);
decode_mam_result_attrs(__TopXMLNS,
			[{<<"xmlns">>, _val} | _attrs], Queryid, _Xmlns, Id) ->
    decode_mam_result_attrs(__TopXMLNS, _attrs, Queryid,
			    _val, Id);
decode_mam_result_attrs(__TopXMLNS,
			[{<<"id">>, _val} | _attrs], Queryid, Xmlns, _Id) ->
    decode_mam_result_attrs(__TopXMLNS, _attrs, Queryid,
			    Xmlns, _val);
decode_mam_result_attrs(__TopXMLNS, [_ | _attrs],
			Queryid, Xmlns, Id) ->
    decode_mam_result_attrs(__TopXMLNS, _attrs, Queryid,
			    Xmlns, Id);
decode_mam_result_attrs(__TopXMLNS, [], Queryid, Xmlns,
			Id) ->
    {decode_mam_result_attr_queryid(__TopXMLNS, Queryid),
     decode_mam_result_attr_xmlns(__TopXMLNS, Xmlns),
     decode_mam_result_attr_id(__TopXMLNS, Id)}.

encode_mam_result({mam_result, Xmlns, Queryid, Id,
		   __Els},
		  _xmlns_attrs) ->
    _els = [encode(_el) || _el <- __Els],
    _attrs = encode_mam_result_attr_id(Id,
				       encode_mam_result_attr_xmlns(Xmlns,
								    encode_mam_result_attr_queryid(Queryid,
												   _xmlns_attrs))),
    {xmlel, <<"result">>, _attrs, _els}.

decode_mam_result_attr_queryid(__TopXMLNS, undefined) ->
    undefined;
decode_mam_result_attr_queryid(__TopXMLNS, _val) ->
    _val.

encode_mam_result_attr_queryid(undefined, _acc) -> _acc;
encode_mam_result_attr_queryid(_val, _acc) ->
    [{<<"queryid">>, _val} | _acc].

decode_mam_result_attr_xmlns(__TopXMLNS, undefined) ->
    undefined;
decode_mam_result_attr_xmlns(__TopXMLNS, _val) -> _val.

encode_mam_result_attr_xmlns(undefined, _acc) -> _acc;
encode_mam_result_attr_xmlns(_val, _acc) ->
    [{<<"xmlns">>, _val} | _acc].

decode_mam_result_attr_id(__TopXMLNS, undefined) ->
    undefined;
decode_mam_result_attr_id(__TopXMLNS, _val) -> _val.

encode_mam_result_attr_id(undefined, _acc) -> _acc;
encode_mam_result_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_mam_archived(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"archived">>, _attrs, _els}) ->
    {Id, By} = decode_mam_archived_attrs(__TopXMLNS, _attrs,
					 undefined, undefined),
    {mam_archived, By, Id}.

decode_mam_archived_attrs(__TopXMLNS,
			  [{<<"id">>, _val} | _attrs], _Id, By) ->
    decode_mam_archived_attrs(__TopXMLNS, _attrs, _val, By);
decode_mam_archived_attrs(__TopXMLNS,
			  [{<<"by">>, _val} | _attrs], Id, _By) ->
    decode_mam_archived_attrs(__TopXMLNS, _attrs, Id, _val);
decode_mam_archived_attrs(__TopXMLNS, [_ | _attrs], Id,
			  By) ->
    decode_mam_archived_attrs(__TopXMLNS, _attrs, Id, By);
decode_mam_archived_attrs(__TopXMLNS, [], Id, By) ->
    {decode_mam_archived_attr_id(__TopXMLNS, Id),
     decode_mam_archived_attr_by(__TopXMLNS, By)}.

encode_mam_archived({mam_archived, By, Id},
		    _xmlns_attrs) ->
    _els = [],
    _attrs = encode_mam_archived_attr_by(By,
					 encode_mam_archived_attr_id(Id,
								     _xmlns_attrs)),
    {xmlel, <<"archived">>, _attrs, _els}.

decode_mam_archived_attr_id(__TopXMLNS, undefined) ->
    undefined;
decode_mam_archived_attr_id(__TopXMLNS, _val) -> _val.

encode_mam_archived_attr_id(undefined, _acc) -> _acc;
encode_mam_archived_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_mam_archived_attr_by(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"by">>, <<"archived">>, __TopXMLNS}});
decode_mam_archived_attr_by(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"by">>, <<"archived">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_mam_archived_attr_by(_val, _acc) ->
    [{<<"by">>, enc_jid(_val)} | _acc].

decode_mam_query(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"query">>, _attrs, _els}) ->
    {Xdata, End, Start, With, Rsm} =
	decode_mam_query_els(__TopXMLNS, __IgnoreEls, _els,
			     undefined, undefined, undefined, undefined,
			     undefined),
    {Id, Xmlns} = decode_mam_query_attrs(__TopXMLNS, _attrs,
					 undefined, undefined),
    {mam_query, Xmlns, Id, Start, End, With, Rsm, Xdata}.

decode_mam_query_els(__TopXMLNS, __IgnoreEls, [], Xdata,
		     End, Start, With, Rsm) ->
    {Xdata, End, Start, With, Rsm};
decode_mam_query_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"start">>, _attrs, _} = _el | _els], Xdata,
		     End, Start, With, Rsm) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_mam_query_els(__TopXMLNS, __IgnoreEls, _els,
				Xdata, End,
				decode_mam_start(__TopXMLNS, __IgnoreEls, _el),
				With, Rsm);
       true ->
	   decode_mam_query_els(__TopXMLNS, __IgnoreEls, _els,
				Xdata, End, Start, With, Rsm)
    end;
decode_mam_query_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"end">>, _attrs, _} = _el | _els], Xdata,
		     End, Start, With, Rsm) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_mam_query_els(__TopXMLNS, __IgnoreEls, _els,
				Xdata,
				decode_mam_end(__TopXMLNS, __IgnoreEls, _el),
				Start, With, Rsm);
       true ->
	   decode_mam_query_els(__TopXMLNS, __IgnoreEls, _els,
				Xdata, End, Start, With, Rsm)
    end;
decode_mam_query_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"with">>, _attrs, _} = _el | _els], Xdata,
		     End, Start, With, Rsm) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_mam_query_els(__TopXMLNS, __IgnoreEls, _els,
				Xdata, End, Start,
				decode_mam_with(__TopXMLNS, __IgnoreEls, _el),
				Rsm);
       true ->
	   decode_mam_query_els(__TopXMLNS, __IgnoreEls, _els,
				Xdata, End, Start, With, Rsm)
    end;
decode_mam_query_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"set">>, _attrs, _} = _el | _els], Xdata,
		     End, Start, With, Rsm) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<"http://jabber.org/protocol/rsm">> ->
	   decode_mam_query_els(__TopXMLNS, __IgnoreEls, _els,
				Xdata, End, Start, With,
				decode_rsm_set(_xmlns, __IgnoreEls, _el));
       true ->
	   decode_mam_query_els(__TopXMLNS, __IgnoreEls, _els,
				Xdata, End, Start, With, Rsm)
    end;
decode_mam_query_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"x">>, _attrs, _} = _el | _els], Xdata, End,
		     Start, With, Rsm) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<"jabber:x:data">> ->
	   decode_mam_query_els(__TopXMLNS, __IgnoreEls, _els,
				decode_xdata(_xmlns, __IgnoreEls, _el), End,
				Start, With, Rsm);
       true ->
	   decode_mam_query_els(__TopXMLNS, __IgnoreEls, _els,
				Xdata, End, Start, With, Rsm)
    end;
decode_mam_query_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Xdata, End, Start, With, Rsm) ->
    decode_mam_query_els(__TopXMLNS, __IgnoreEls, _els,
			 Xdata, End, Start, With, Rsm).

decode_mam_query_attrs(__TopXMLNS,
		       [{<<"queryid">>, _val} | _attrs], _Id, Xmlns) ->
    decode_mam_query_attrs(__TopXMLNS, _attrs, _val, Xmlns);
decode_mam_query_attrs(__TopXMLNS,
		       [{<<"xmlns">>, _val} | _attrs], Id, _Xmlns) ->
    decode_mam_query_attrs(__TopXMLNS, _attrs, Id, _val);
decode_mam_query_attrs(__TopXMLNS, [_ | _attrs], Id,
		       Xmlns) ->
    decode_mam_query_attrs(__TopXMLNS, _attrs, Id, Xmlns);
decode_mam_query_attrs(__TopXMLNS, [], Id, Xmlns) ->
    {decode_mam_query_attr_queryid(__TopXMLNS, Id),
     decode_mam_query_attr_xmlns(__TopXMLNS, Xmlns)}.

encode_mam_query({mam_query, Xmlns, Id, Start, End,
		  With, Rsm, Xdata},
		 _xmlns_attrs) ->
    _els = lists:reverse('encode_mam_query_$xdata'(Xdata,
						   'encode_mam_query_$end'(End,
									   'encode_mam_query_$start'(Start,
												     'encode_mam_query_$with'(With,
															      'encode_mam_query_$rsm'(Rsm,
																		      [])))))),
    _attrs = encode_mam_query_attr_xmlns(Xmlns,
					 encode_mam_query_attr_queryid(Id,
								       _xmlns_attrs)),
    {xmlel, <<"query">>, _attrs, _els}.

'encode_mam_query_$xdata'(undefined, _acc) -> _acc;
'encode_mam_query_$xdata'(Xdata, _acc) ->
    [encode_xdata(Xdata,
		  [{<<"xmlns">>, <<"jabber:x:data">>}])
     | _acc].

'encode_mam_query_$end'(undefined, _acc) -> _acc;
'encode_mam_query_$end'(End, _acc) ->
    [encode_mam_end(End,
		    [{<<"xmlns">>, <<"urn:xmpp:mam:tmp">>}])
     | _acc].

'encode_mam_query_$start'(undefined, _acc) -> _acc;
'encode_mam_query_$start'(Start, _acc) ->
    [encode_mam_start(Start,
		      [{<<"xmlns">>, <<"urn:xmpp:mam:tmp">>}])
     | _acc].

'encode_mam_query_$with'(undefined, _acc) -> _acc;
'encode_mam_query_$with'(With, _acc) ->
    [encode_mam_with(With,
		     [{<<"xmlns">>, <<"urn:xmpp:mam:tmp">>}])
     | _acc].

'encode_mam_query_$rsm'(undefined, _acc) -> _acc;
'encode_mam_query_$rsm'(Rsm, _acc) ->
    [encode_rsm_set(Rsm,
		    [{<<"xmlns">>, <<"http://jabber.org/protocol/rsm">>}])
     | _acc].

decode_mam_query_attr_queryid(__TopXMLNS, undefined) ->
    undefined;
decode_mam_query_attr_queryid(__TopXMLNS, _val) -> _val.

encode_mam_query_attr_queryid(undefined, _acc) -> _acc;
encode_mam_query_attr_queryid(_val, _acc) ->
    [{<<"queryid">>, _val} | _acc].

decode_mam_query_attr_xmlns(__TopXMLNS, undefined) ->
    undefined;
decode_mam_query_attr_xmlns(__TopXMLNS, _val) -> _val.

encode_mam_query_attr_xmlns(undefined, _acc) -> _acc;
encode_mam_query_attr_xmlns(_val, _acc) ->
    [{<<"xmlns">>, _val} | _acc].

decode_mam_with(__TopXMLNS, __IgnoreEls,
		{xmlel, <<"with">>, _attrs, _els}) ->
    Cdata = decode_mam_with_els(__TopXMLNS, __IgnoreEls,
				_els, <<>>),
    Cdata.

decode_mam_with_els(__TopXMLNS, __IgnoreEls, [],
		    Cdata) ->
    decode_mam_with_cdata(__TopXMLNS, Cdata);
decode_mam_with_els(__TopXMLNS, __IgnoreEls,
		    [{xmlcdata, _data} | _els], Cdata) ->
    decode_mam_with_els(__TopXMLNS, __IgnoreEls, _els,
			<<Cdata/binary, _data/binary>>);
decode_mam_with_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		    Cdata) ->
    decode_mam_with_els(__TopXMLNS, __IgnoreEls, _els,
			Cdata).

encode_mam_with(Cdata, _xmlns_attrs) ->
    _els = encode_mam_with_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"with">>, _attrs, _els}.

decode_mam_with_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmpp_codec,
		  {missing_cdata, <<>>, <<"with">>, __TopXMLNS}});
decode_mam_with_cdata(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"with">>, __TopXMLNS}});
      _res -> _res
    end.

encode_mam_with_cdata(_val, _acc) ->
    [{xmlcdata, enc_jid(_val)} | _acc].

decode_mam_end(__TopXMLNS, __IgnoreEls,
	       {xmlel, <<"end">>, _attrs, _els}) ->
    Cdata = decode_mam_end_els(__TopXMLNS, __IgnoreEls,
			       _els, <<>>),
    Cdata.

decode_mam_end_els(__TopXMLNS, __IgnoreEls, [],
		   Cdata) ->
    decode_mam_end_cdata(__TopXMLNS, Cdata);
decode_mam_end_els(__TopXMLNS, __IgnoreEls,
		   [{xmlcdata, _data} | _els], Cdata) ->
    decode_mam_end_els(__TopXMLNS, __IgnoreEls, _els,
		       <<Cdata/binary, _data/binary>>);
decode_mam_end_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		   Cdata) ->
    decode_mam_end_els(__TopXMLNS, __IgnoreEls, _els,
		       Cdata).

encode_mam_end(Cdata, _xmlns_attrs) ->
    _els = encode_mam_end_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"end">>, _attrs, _els}.

decode_mam_end_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmpp_codec,
		  {missing_cdata, <<>>, <<"end">>, __TopXMLNS}});
decode_mam_end_cdata(__TopXMLNS, _val) ->
    case catch dec_utc(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"end">>, __TopXMLNS}});
      _res -> _res
    end.

encode_mam_end_cdata(_val, _acc) ->
    [{xmlcdata, enc_utc(_val)} | _acc].

decode_mam_start(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"start">>, _attrs, _els}) ->
    Cdata = decode_mam_start_els(__TopXMLNS, __IgnoreEls,
				 _els, <<>>),
    Cdata.

decode_mam_start_els(__TopXMLNS, __IgnoreEls, [],
		     Cdata) ->
    decode_mam_start_cdata(__TopXMLNS, Cdata);
decode_mam_start_els(__TopXMLNS, __IgnoreEls,
		     [{xmlcdata, _data} | _els], Cdata) ->
    decode_mam_start_els(__TopXMLNS, __IgnoreEls, _els,
			 <<Cdata/binary, _data/binary>>);
decode_mam_start_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Cdata) ->
    decode_mam_start_els(__TopXMLNS, __IgnoreEls, _els,
			 Cdata).

encode_mam_start(Cdata, _xmlns_attrs) ->
    _els = encode_mam_start_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"start">>, _attrs, _els}.

decode_mam_start_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmpp_codec,
		  {missing_cdata, <<>>, <<"start">>, __TopXMLNS}});
decode_mam_start_cdata(__TopXMLNS, _val) ->
    case catch dec_utc(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"start">>, __TopXMLNS}});
      _res -> _res
    end.

encode_mam_start_cdata(_val, _acc) ->
    [{xmlcdata, enc_utc(_val)} | _acc].

decode_rsm_set(__TopXMLNS, __IgnoreEls,
	       {xmlel, <<"set">>, _attrs, _els}) ->
    {After, Last, First, Count, Before, Max, Index} =
	decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els,
			   undefined, undefined, undefined, undefined,
			   undefined, undefined, undefined),
    {rsm_set, After, Before, Count, First, Index, Last,
     Max}.

decode_rsm_set_els(__TopXMLNS, __IgnoreEls, [], After,
		   Last, First, Count, Before, Max, Index) ->
    {After, Last, First, Count, Before, Max, Index};
decode_rsm_set_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"after">>, _attrs, _} = _el | _els], After,
		   Last, First, Count, Before, Max, Index) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els,
			      decode_rsm_after(__TopXMLNS, __IgnoreEls, _el),
			      Last, First, Count, Before, Max, Index);
       true ->
	   decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els, After,
			      Last, First, Count, Before, Max, Index)
    end;
decode_rsm_set_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"before">>, _attrs, _} = _el | _els], After,
		   Last, First, Count, Before, Max, Index) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els, After,
			      Last, First, Count,
			      decode_rsm_before(__TopXMLNS, __IgnoreEls, _el),
			      Max, Index);
       true ->
	   decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els, After,
			      Last, First, Count, Before, Max, Index)
    end;
decode_rsm_set_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"count">>, _attrs, _} = _el | _els], After,
		   Last, First, Count, Before, Max, Index) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els, After,
			      Last, First,
			      decode_rsm_count(__TopXMLNS, __IgnoreEls, _el),
			      Before, Max, Index);
       true ->
	   decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els, After,
			      Last, First, Count, Before, Max, Index)
    end;
decode_rsm_set_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"first">>, _attrs, _} = _el | _els], After,
		   Last, First, Count, Before, Max, Index) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els, After,
			      Last,
			      decode_rsm_first(__TopXMLNS, __IgnoreEls, _el),
			      Count, Before, Max, Index);
       true ->
	   decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els, After,
			      Last, First, Count, Before, Max, Index)
    end;
decode_rsm_set_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"index">>, _attrs, _} = _el | _els], After,
		   Last, First, Count, Before, Max, Index) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els, After,
			      Last, First, Count, Before, Max,
			      decode_rsm_index(__TopXMLNS, __IgnoreEls, _el));
       true ->
	   decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els, After,
			      Last, First, Count, Before, Max, Index)
    end;
decode_rsm_set_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"last">>, _attrs, _} = _el | _els], After,
		   Last, First, Count, Before, Max, Index) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els, After,
			      decode_rsm_last(__TopXMLNS, __IgnoreEls, _el),
			      First, Count, Before, Max, Index);
       true ->
	   decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els, After,
			      Last, First, Count, Before, Max, Index)
    end;
decode_rsm_set_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"max">>, _attrs, _} = _el | _els], After,
		   Last, First, Count, Before, Max, Index) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els, After,
			      Last, First, Count, Before,
			      decode_rsm_max(__TopXMLNS, __IgnoreEls, _el),
			      Index);
       true ->
	   decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els, After,
			      Last, First, Count, Before, Max, Index)
    end;
decode_rsm_set_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		   After, Last, First, Count, Before, Max, Index) ->
    decode_rsm_set_els(__TopXMLNS, __IgnoreEls, _els, After,
		       Last, First, Count, Before, Max, Index).

encode_rsm_set({rsm_set, After, Before, Count, First,
		Index, Last, Max},
	       _xmlns_attrs) ->
    _els = lists:reverse('encode_rsm_set_$after'(After,
						 'encode_rsm_set_$last'(Last,
									'encode_rsm_set_$first'(First,
												'encode_rsm_set_$count'(Count,
															'encode_rsm_set_$before'(Before,
																		 'encode_rsm_set_$max'(Max,
																				       'encode_rsm_set_$index'(Index,
																							       [])))))))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"set">>, _attrs, _els}.

'encode_rsm_set_$after'(undefined, _acc) -> _acc;
'encode_rsm_set_$after'(After, _acc) ->
    [encode_rsm_after(After, []) | _acc].

'encode_rsm_set_$last'(undefined, _acc) -> _acc;
'encode_rsm_set_$last'(Last, _acc) ->
    [encode_rsm_last(Last, []) | _acc].

'encode_rsm_set_$first'(undefined, _acc) -> _acc;
'encode_rsm_set_$first'(First, _acc) ->
    [encode_rsm_first(First, []) | _acc].

'encode_rsm_set_$count'(undefined, _acc) -> _acc;
'encode_rsm_set_$count'(Count, _acc) ->
    [encode_rsm_count(Count, []) | _acc].

'encode_rsm_set_$before'(undefined, _acc) -> _acc;
'encode_rsm_set_$before'(Before, _acc) ->
    [encode_rsm_before(Before, []) | _acc].

'encode_rsm_set_$max'(undefined, _acc) -> _acc;
'encode_rsm_set_$max'(Max, _acc) ->
    [encode_rsm_max(Max, []) | _acc].

'encode_rsm_set_$index'(undefined, _acc) -> _acc;
'encode_rsm_set_$index'(Index, _acc) ->
    [encode_rsm_index(Index, []) | _acc].

decode_rsm_first(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"first">>, _attrs, _els}) ->
    Data = decode_rsm_first_els(__TopXMLNS, __IgnoreEls,
				_els, <<>>),
    Index = decode_rsm_first_attrs(__TopXMLNS, _attrs,
				   undefined),
    {rsm_first, Index, Data}.

decode_rsm_first_els(__TopXMLNS, __IgnoreEls, [],
		     Data) ->
    decode_rsm_first_cdata(__TopXMLNS, Data);
decode_rsm_first_els(__TopXMLNS, __IgnoreEls,
		     [{xmlcdata, _data} | _els], Data) ->
    decode_rsm_first_els(__TopXMLNS, __IgnoreEls, _els,
			 <<Data/binary, _data/binary>>);
decode_rsm_first_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Data) ->
    decode_rsm_first_els(__TopXMLNS, __IgnoreEls, _els,
			 Data).

decode_rsm_first_attrs(__TopXMLNS,
		       [{<<"index">>, _val} | _attrs], _Index) ->
    decode_rsm_first_attrs(__TopXMLNS, _attrs, _val);
decode_rsm_first_attrs(__TopXMLNS, [_ | _attrs],
		       Index) ->
    decode_rsm_first_attrs(__TopXMLNS, _attrs, Index);
decode_rsm_first_attrs(__TopXMLNS, [], Index) ->
    decode_rsm_first_attr_index(__TopXMLNS, Index).

encode_rsm_first({rsm_first, Index, Data},
		 _xmlns_attrs) ->
    _els = encode_rsm_first_cdata(Data, []),
    _attrs = encode_rsm_first_attr_index(Index,
					 _xmlns_attrs),
    {xmlel, <<"first">>, _attrs, _els}.

decode_rsm_first_attr_index(__TopXMLNS, undefined) ->
    undefined;
decode_rsm_first_attr_index(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"index">>, <<"first">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_rsm_first_attr_index(undefined, _acc) -> _acc;
encode_rsm_first_attr_index(_val, _acc) ->
    [{<<"index">>, enc_int(_val)} | _acc].

decode_rsm_first_cdata(__TopXMLNS, <<>>) -> undefined;
decode_rsm_first_cdata(__TopXMLNS, _val) -> _val.

encode_rsm_first_cdata(undefined, _acc) -> _acc;
encode_rsm_first_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_rsm_max(__TopXMLNS, __IgnoreEls,
	       {xmlel, <<"max">>, _attrs, _els}) ->
    Cdata = decode_rsm_max_els(__TopXMLNS, __IgnoreEls,
			       _els, <<>>),
    Cdata.

decode_rsm_max_els(__TopXMLNS, __IgnoreEls, [],
		   Cdata) ->
    decode_rsm_max_cdata(__TopXMLNS, Cdata);
decode_rsm_max_els(__TopXMLNS, __IgnoreEls,
		   [{xmlcdata, _data} | _els], Cdata) ->
    decode_rsm_max_els(__TopXMLNS, __IgnoreEls, _els,
		       <<Cdata/binary, _data/binary>>);
decode_rsm_max_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		   Cdata) ->
    decode_rsm_max_els(__TopXMLNS, __IgnoreEls, _els,
		       Cdata).

encode_rsm_max(Cdata, _xmlns_attrs) ->
    _els = encode_rsm_max_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"max">>, _attrs, _els}.

decode_rsm_max_cdata(__TopXMLNS, <<>>) -> undefined;
decode_rsm_max_cdata(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"max">>, __TopXMLNS}});
      _res -> _res
    end.

encode_rsm_max_cdata(undefined, _acc) -> _acc;
encode_rsm_max_cdata(_val, _acc) ->
    [{xmlcdata, enc_int(_val)} | _acc].

decode_rsm_index(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"index">>, _attrs, _els}) ->
    Cdata = decode_rsm_index_els(__TopXMLNS, __IgnoreEls,
				 _els, <<>>),
    Cdata.

decode_rsm_index_els(__TopXMLNS, __IgnoreEls, [],
		     Cdata) ->
    decode_rsm_index_cdata(__TopXMLNS, Cdata);
decode_rsm_index_els(__TopXMLNS, __IgnoreEls,
		     [{xmlcdata, _data} | _els], Cdata) ->
    decode_rsm_index_els(__TopXMLNS, __IgnoreEls, _els,
			 <<Cdata/binary, _data/binary>>);
decode_rsm_index_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Cdata) ->
    decode_rsm_index_els(__TopXMLNS, __IgnoreEls, _els,
			 Cdata).

encode_rsm_index(Cdata, _xmlns_attrs) ->
    _els = encode_rsm_index_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"index">>, _attrs, _els}.

decode_rsm_index_cdata(__TopXMLNS, <<>>) -> undefined;
decode_rsm_index_cdata(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"index">>, __TopXMLNS}});
      _res -> _res
    end.

encode_rsm_index_cdata(undefined, _acc) -> _acc;
encode_rsm_index_cdata(_val, _acc) ->
    [{xmlcdata, enc_int(_val)} | _acc].

decode_rsm_count(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"count">>, _attrs, _els}) ->
    Cdata = decode_rsm_count_els(__TopXMLNS, __IgnoreEls,
				 _els, <<>>),
    Cdata.

decode_rsm_count_els(__TopXMLNS, __IgnoreEls, [],
		     Cdata) ->
    decode_rsm_count_cdata(__TopXMLNS, Cdata);
decode_rsm_count_els(__TopXMLNS, __IgnoreEls,
		     [{xmlcdata, _data} | _els], Cdata) ->
    decode_rsm_count_els(__TopXMLNS, __IgnoreEls, _els,
			 <<Cdata/binary, _data/binary>>);
decode_rsm_count_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Cdata) ->
    decode_rsm_count_els(__TopXMLNS, __IgnoreEls, _els,
			 Cdata).

encode_rsm_count(Cdata, _xmlns_attrs) ->
    _els = encode_rsm_count_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"count">>, _attrs, _els}.

decode_rsm_count_cdata(__TopXMLNS, <<>>) -> undefined;
decode_rsm_count_cdata(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"count">>, __TopXMLNS}});
      _res -> _res
    end.

encode_rsm_count_cdata(undefined, _acc) -> _acc;
encode_rsm_count_cdata(_val, _acc) ->
    [{xmlcdata, enc_int(_val)} | _acc].

decode_rsm_last(__TopXMLNS, __IgnoreEls,
		{xmlel, <<"last">>, _attrs, _els}) ->
    Cdata = decode_rsm_last_els(__TopXMLNS, __IgnoreEls,
				_els, <<>>),
    Cdata.

decode_rsm_last_els(__TopXMLNS, __IgnoreEls, [],
		    Cdata) ->
    decode_rsm_last_cdata(__TopXMLNS, Cdata);
decode_rsm_last_els(__TopXMLNS, __IgnoreEls,
		    [{xmlcdata, _data} | _els], Cdata) ->
    decode_rsm_last_els(__TopXMLNS, __IgnoreEls, _els,
			<<Cdata/binary, _data/binary>>);
decode_rsm_last_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		    Cdata) ->
    decode_rsm_last_els(__TopXMLNS, __IgnoreEls, _els,
			Cdata).

encode_rsm_last(Cdata, _xmlns_attrs) ->
    _els = encode_rsm_last_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"last">>, _attrs, _els}.

decode_rsm_last_cdata(__TopXMLNS, <<>>) -> undefined;
decode_rsm_last_cdata(__TopXMLNS, _val) -> _val.

encode_rsm_last_cdata(undefined, _acc) -> _acc;
encode_rsm_last_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_rsm_before(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"before">>, _attrs, _els}) ->
    Cdata = decode_rsm_before_els(__TopXMLNS, __IgnoreEls,
				  _els, <<>>),
    Cdata.

decode_rsm_before_els(__TopXMLNS, __IgnoreEls, [],
		      Cdata) ->
    decode_rsm_before_cdata(__TopXMLNS, Cdata);
decode_rsm_before_els(__TopXMLNS, __IgnoreEls,
		      [{xmlcdata, _data} | _els], Cdata) ->
    decode_rsm_before_els(__TopXMLNS, __IgnoreEls, _els,
			  <<Cdata/binary, _data/binary>>);
decode_rsm_before_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Cdata) ->
    decode_rsm_before_els(__TopXMLNS, __IgnoreEls, _els,
			  Cdata).

encode_rsm_before(Cdata, _xmlns_attrs) ->
    _els = encode_rsm_before_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"before">>, _attrs, _els}.

decode_rsm_before_cdata(__TopXMLNS, <<>>) -> none;
decode_rsm_before_cdata(__TopXMLNS, _val) -> _val.

encode_rsm_before_cdata(none, _acc) -> _acc;
encode_rsm_before_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_rsm_after(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"after">>, _attrs, _els}) ->
    Cdata = decode_rsm_after_els(__TopXMLNS, __IgnoreEls,
				 _els, <<>>),
    Cdata.

decode_rsm_after_els(__TopXMLNS, __IgnoreEls, [],
		     Cdata) ->
    decode_rsm_after_cdata(__TopXMLNS, Cdata);
decode_rsm_after_els(__TopXMLNS, __IgnoreEls,
		     [{xmlcdata, _data} | _els], Cdata) ->
    decode_rsm_after_els(__TopXMLNS, __IgnoreEls, _els,
			 <<Cdata/binary, _data/binary>>);
decode_rsm_after_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Cdata) ->
    decode_rsm_after_els(__TopXMLNS, __IgnoreEls, _els,
			 Cdata).

encode_rsm_after(Cdata, _xmlns_attrs) ->
    _els = encode_rsm_after_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"after">>, _attrs, _els}.

decode_rsm_after_cdata(__TopXMLNS, <<>>) -> undefined;
decode_rsm_after_cdata(__TopXMLNS, _val) -> _val.

encode_rsm_after_cdata(undefined, _acc) -> _acc;
encode_rsm_after_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_muc(__TopXMLNS, __IgnoreEls,
	   {xmlel, <<"x">>, _attrs, _els}) ->
    History = decode_muc_els(__TopXMLNS, __IgnoreEls, _els,
			     undefined),
    Password = decode_muc_attrs(__TopXMLNS, _attrs,
				undefined),
    {muc, History, Password}.

decode_muc_els(__TopXMLNS, __IgnoreEls, [], History) ->
    History;
decode_muc_els(__TopXMLNS, __IgnoreEls,
	       [{xmlel, <<"history">>, _attrs, _} = _el | _els],
	       History) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_els(__TopXMLNS, __IgnoreEls, _els,
			  decode_muc_history(__TopXMLNS, __IgnoreEls, _el));
       true ->
	   decode_muc_els(__TopXMLNS, __IgnoreEls, _els, History)
    end;
decode_muc_els(__TopXMLNS, __IgnoreEls, [_ | _els],
	       History) ->
    decode_muc_els(__TopXMLNS, __IgnoreEls, _els, History).

decode_muc_attrs(__TopXMLNS,
		 [{<<"password">>, _val} | _attrs], _Password) ->
    decode_muc_attrs(__TopXMLNS, _attrs, _val);
decode_muc_attrs(__TopXMLNS, [_ | _attrs], Password) ->
    decode_muc_attrs(__TopXMLNS, _attrs, Password);
decode_muc_attrs(__TopXMLNS, [], Password) ->
    decode_muc_attr_password(__TopXMLNS, Password).

encode_muc({muc, History, Password}, _xmlns_attrs) ->
    _els = lists:reverse('encode_muc_$history'(History,
					       [])),
    _attrs = encode_muc_attr_password(Password,
				      _xmlns_attrs),
    {xmlel, <<"x">>, _attrs, _els}.

'encode_muc_$history'(undefined, _acc) -> _acc;
'encode_muc_$history'(History, _acc) ->
    [encode_muc_history(History, []) | _acc].

decode_muc_attr_password(__TopXMLNS, undefined) ->
    undefined;
decode_muc_attr_password(__TopXMLNS, _val) -> _val.

encode_muc_attr_password(undefined, _acc) -> _acc;
encode_muc_attr_password(_val, _acc) ->
    [{<<"password">>, _val} | _acc].

decode_muc_admin(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"query">>, _attrs, _els}) ->
    Items = decode_muc_admin_els(__TopXMLNS, __IgnoreEls,
				 _els, []),
    {muc_admin, Items}.

decode_muc_admin_els(__TopXMLNS, __IgnoreEls, [],
		     Items) ->
    lists:reverse(Items);
decode_muc_admin_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"item">>, _attrs, _} = _el | _els], Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_admin_els(__TopXMLNS, __IgnoreEls, _els,
				[decode_muc_admin_item(__TopXMLNS, __IgnoreEls,
						       _el)
				 | Items]);
       true ->
	   decode_muc_admin_els(__TopXMLNS, __IgnoreEls, _els,
				Items)
    end;
decode_muc_admin_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Items) ->
    decode_muc_admin_els(__TopXMLNS, __IgnoreEls, _els,
			 Items).

encode_muc_admin({muc_admin, Items}, _xmlns_attrs) ->
    _els = lists:reverse('encode_muc_admin_$items'(Items,
						   [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"query">>, _attrs, _els}.

'encode_muc_admin_$items'([], _acc) -> _acc;
'encode_muc_admin_$items'([Items | _els], _acc) ->
    'encode_muc_admin_$items'(_els,
			      [encode_muc_admin_item(Items, []) | _acc]).

decode_muc_admin_reason(__TopXMLNS, __IgnoreEls,
			{xmlel, <<"reason">>, _attrs, _els}) ->
    Cdata = decode_muc_admin_reason_els(__TopXMLNS,
					__IgnoreEls, _els, <<>>),
    Cdata.

decode_muc_admin_reason_els(__TopXMLNS, __IgnoreEls, [],
			    Cdata) ->
    decode_muc_admin_reason_cdata(__TopXMLNS, Cdata);
decode_muc_admin_reason_els(__TopXMLNS, __IgnoreEls,
			    [{xmlcdata, _data} | _els], Cdata) ->
    decode_muc_admin_reason_els(__TopXMLNS, __IgnoreEls,
				_els, <<Cdata/binary, _data/binary>>);
decode_muc_admin_reason_els(__TopXMLNS, __IgnoreEls,
			    [_ | _els], Cdata) ->
    decode_muc_admin_reason_els(__TopXMLNS, __IgnoreEls,
				_els, Cdata).

encode_muc_admin_reason(Cdata, _xmlns_attrs) ->
    _els = encode_muc_admin_reason_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"reason">>, _attrs, _els}.

decode_muc_admin_reason_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_muc_admin_reason_cdata(__TopXMLNS, _val) -> _val.

encode_muc_admin_reason_cdata(undefined, _acc) -> _acc;
encode_muc_admin_reason_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_muc_admin_continue(__TopXMLNS, __IgnoreEls,
			  {xmlel, <<"continue">>, _attrs, _els}) ->
    Thread = decode_muc_admin_continue_attrs(__TopXMLNS,
					     _attrs, undefined),
    Thread.

decode_muc_admin_continue_attrs(__TopXMLNS,
				[{<<"thread">>, _val} | _attrs], _Thread) ->
    decode_muc_admin_continue_attrs(__TopXMLNS, _attrs,
				    _val);
decode_muc_admin_continue_attrs(__TopXMLNS,
				[_ | _attrs], Thread) ->
    decode_muc_admin_continue_attrs(__TopXMLNS, _attrs,
				    Thread);
decode_muc_admin_continue_attrs(__TopXMLNS, [],
				Thread) ->
    decode_muc_admin_continue_attr_thread(__TopXMLNS,
					  Thread).

encode_muc_admin_continue(Thread, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_muc_admin_continue_attr_thread(Thread,
						   _xmlns_attrs),
    {xmlel, <<"continue">>, _attrs, _els}.

decode_muc_admin_continue_attr_thread(__TopXMLNS,
				      undefined) ->
    undefined;
decode_muc_admin_continue_attr_thread(__TopXMLNS,
				      _val) ->
    _val.

encode_muc_admin_continue_attr_thread(undefined,
				      _acc) ->
    _acc;
encode_muc_admin_continue_attr_thread(_val, _acc) ->
    [{<<"thread">>, _val} | _acc].

decode_muc_admin_actor(__TopXMLNS, __IgnoreEls,
		       {xmlel, <<"actor">>, _attrs, _els}) ->
    {Jid, Nick} = decode_muc_admin_actor_attrs(__TopXMLNS,
					       _attrs, undefined, undefined),
    {muc_actor, Jid, Nick}.

decode_muc_admin_actor_attrs(__TopXMLNS,
			     [{<<"jid">>, _val} | _attrs], _Jid, Nick) ->
    decode_muc_admin_actor_attrs(__TopXMLNS, _attrs, _val,
				 Nick);
decode_muc_admin_actor_attrs(__TopXMLNS,
			     [{<<"nick">>, _val} | _attrs], Jid, _Nick) ->
    decode_muc_admin_actor_attrs(__TopXMLNS, _attrs, Jid,
				 _val);
decode_muc_admin_actor_attrs(__TopXMLNS, [_ | _attrs],
			     Jid, Nick) ->
    decode_muc_admin_actor_attrs(__TopXMLNS, _attrs, Jid,
				 Nick);
decode_muc_admin_actor_attrs(__TopXMLNS, [], Jid,
			     Nick) ->
    {decode_muc_admin_actor_attr_jid(__TopXMLNS, Jid),
     decode_muc_admin_actor_attr_nick(__TopXMLNS, Nick)}.

encode_muc_admin_actor({muc_actor, Jid, Nick},
		       _xmlns_attrs) ->
    _els = [],
    _attrs = encode_muc_admin_actor_attr_nick(Nick,
					      encode_muc_admin_actor_attr_jid(Jid,
									      _xmlns_attrs)),
    {xmlel, <<"actor">>, _attrs, _els}.

decode_muc_admin_actor_attr_jid(__TopXMLNS,
				undefined) ->
    undefined;
decode_muc_admin_actor_attr_jid(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"actor">>, __TopXMLNS}});
      _res -> _res
    end.

encode_muc_admin_actor_attr_jid(undefined, _acc) ->
    _acc;
encode_muc_admin_actor_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_muc_admin_actor_attr_nick(__TopXMLNS,
				 undefined) ->
    undefined;
decode_muc_admin_actor_attr_nick(__TopXMLNS, _val) ->
    _val.

encode_muc_admin_actor_attr_nick(undefined, _acc) ->
    _acc;
encode_muc_admin_actor_attr_nick(_val, _acc) ->
    [{<<"nick">>, _val} | _acc].

decode_muc_admin_item(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"item">>, _attrs, _els}) ->
    {Actor, Continue, Reason} =
	decode_muc_admin_item_els(__TopXMLNS, __IgnoreEls, _els,
				  undefined, undefined, undefined),
    {Affiliation, Role, Jid, Nick} =
	decode_muc_admin_item_attrs(__TopXMLNS, _attrs,
				    undefined, undefined, undefined, undefined),
    {muc_item, Actor, Continue, Reason, Affiliation, Role,
     Jid, Nick}.

decode_muc_admin_item_els(__TopXMLNS, __IgnoreEls, [],
			  Actor, Continue, Reason) ->
    {Actor, Continue, Reason};
decode_muc_admin_item_els(__TopXMLNS, __IgnoreEls,
			  [{xmlel, <<"actor">>, _attrs, _} = _el | _els], Actor,
			  Continue, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_admin_item_els(__TopXMLNS, __IgnoreEls, _els,
				     decode_muc_admin_actor(__TopXMLNS,
							    __IgnoreEls, _el),
				     Continue, Reason);
       true ->
	   decode_muc_admin_item_els(__TopXMLNS, __IgnoreEls, _els,
				     Actor, Continue, Reason)
    end;
decode_muc_admin_item_els(__TopXMLNS, __IgnoreEls,
			  [{xmlel, <<"continue">>, _attrs, _} = _el | _els],
			  Actor, Continue, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_admin_item_els(__TopXMLNS, __IgnoreEls, _els,
				     Actor,
				     decode_muc_admin_continue(__TopXMLNS,
							       __IgnoreEls,
							       _el),
				     Reason);
       true ->
	   decode_muc_admin_item_els(__TopXMLNS, __IgnoreEls, _els,
				     Actor, Continue, Reason)
    end;
decode_muc_admin_item_els(__TopXMLNS, __IgnoreEls,
			  [{xmlel, <<"reason">>, _attrs, _} = _el | _els],
			  Actor, Continue, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_admin_item_els(__TopXMLNS, __IgnoreEls, _els,
				     Actor, Continue,
				     decode_muc_admin_reason(__TopXMLNS,
							     __IgnoreEls, _el));
       true ->
	   decode_muc_admin_item_els(__TopXMLNS, __IgnoreEls, _els,
				     Actor, Continue, Reason)
    end;
decode_muc_admin_item_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Actor, Continue, Reason) ->
    decode_muc_admin_item_els(__TopXMLNS, __IgnoreEls, _els,
			      Actor, Continue, Reason).

decode_muc_admin_item_attrs(__TopXMLNS,
			    [{<<"affiliation">>, _val} | _attrs], _Affiliation,
			    Role, Jid, Nick) ->
    decode_muc_admin_item_attrs(__TopXMLNS, _attrs, _val,
				Role, Jid, Nick);
decode_muc_admin_item_attrs(__TopXMLNS,
			    [{<<"role">>, _val} | _attrs], Affiliation, _Role,
			    Jid, Nick) ->
    decode_muc_admin_item_attrs(__TopXMLNS, _attrs,
				Affiliation, _val, Jid, Nick);
decode_muc_admin_item_attrs(__TopXMLNS,
			    [{<<"jid">>, _val} | _attrs], Affiliation, Role,
			    _Jid, Nick) ->
    decode_muc_admin_item_attrs(__TopXMLNS, _attrs,
				Affiliation, Role, _val, Nick);
decode_muc_admin_item_attrs(__TopXMLNS,
			    [{<<"nick">>, _val} | _attrs], Affiliation, Role,
			    Jid, _Nick) ->
    decode_muc_admin_item_attrs(__TopXMLNS, _attrs,
				Affiliation, Role, Jid, _val);
decode_muc_admin_item_attrs(__TopXMLNS, [_ | _attrs],
			    Affiliation, Role, Jid, Nick) ->
    decode_muc_admin_item_attrs(__TopXMLNS, _attrs,
				Affiliation, Role, Jid, Nick);
decode_muc_admin_item_attrs(__TopXMLNS, [], Affiliation,
			    Role, Jid, Nick) ->
    {decode_muc_admin_item_attr_affiliation(__TopXMLNS,
					    Affiliation),
     decode_muc_admin_item_attr_role(__TopXMLNS, Role),
     decode_muc_admin_item_attr_jid(__TopXMLNS, Jid),
     decode_muc_admin_item_attr_nick(__TopXMLNS, Nick)}.

encode_muc_admin_item({muc_item, Actor, Continue,
		       Reason, Affiliation, Role, Jid, Nick},
		      _xmlns_attrs) ->
    _els =
	lists:reverse('encode_muc_admin_item_$actor'(Actor,
						     'encode_muc_admin_item_$continue'(Continue,
										       'encode_muc_admin_item_$reason'(Reason,
														       [])))),
    _attrs = encode_muc_admin_item_attr_nick(Nick,
					     encode_muc_admin_item_attr_jid(Jid,
									    encode_muc_admin_item_attr_role(Role,
													    encode_muc_admin_item_attr_affiliation(Affiliation,
																		   _xmlns_attrs)))),
    {xmlel, <<"item">>, _attrs, _els}.

'encode_muc_admin_item_$actor'(undefined, _acc) -> _acc;
'encode_muc_admin_item_$actor'(Actor, _acc) ->
    [encode_muc_admin_actor(Actor, []) | _acc].

'encode_muc_admin_item_$continue'(undefined, _acc) ->
    _acc;
'encode_muc_admin_item_$continue'(Continue, _acc) ->
    [encode_muc_admin_continue(Continue, []) | _acc].

'encode_muc_admin_item_$reason'(undefined, _acc) ->
    _acc;
'encode_muc_admin_item_$reason'(Reason, _acc) ->
    [encode_muc_admin_reason(Reason, []) | _acc].

decode_muc_admin_item_attr_affiliation(__TopXMLNS,
				       undefined) ->
    undefined;
decode_muc_admin_item_attr_affiliation(__TopXMLNS,
				       _val) ->
    case catch dec_enum(_val,
			[admin, member, none, outcast, owner])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"affiliation">>, <<"item">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_muc_admin_item_attr_affiliation(undefined,
				       _acc) ->
    _acc;
encode_muc_admin_item_attr_affiliation(_val, _acc) ->
    [{<<"affiliation">>, enc_enum(_val)} | _acc].

decode_muc_admin_item_attr_role(__TopXMLNS,
				undefined) ->
    undefined;
decode_muc_admin_item_attr_role(__TopXMLNS, _val) ->
    case catch dec_enum(_val,
			[moderator, none, participant, visitor])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"role">>, <<"item">>, __TopXMLNS}});
      _res -> _res
    end.

encode_muc_admin_item_attr_role(undefined, _acc) ->
    _acc;
encode_muc_admin_item_attr_role(_val, _acc) ->
    [{<<"role">>, enc_enum(_val)} | _acc].

decode_muc_admin_item_attr_jid(__TopXMLNS, undefined) ->
    undefined;
decode_muc_admin_item_attr_jid(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"item">>, __TopXMLNS}});
      _res -> _res
    end.

encode_muc_admin_item_attr_jid(undefined, _acc) -> _acc;
encode_muc_admin_item_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_muc_admin_item_attr_nick(__TopXMLNS,
				undefined) ->
    undefined;
decode_muc_admin_item_attr_nick(__TopXMLNS, _val) ->
    _val.

encode_muc_admin_item_attr_nick(undefined, _acc) ->
    _acc;
encode_muc_admin_item_attr_nick(_val, _acc) ->
    [{<<"nick">>, _val} | _acc].

decode_muc_owner(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"query">>, _attrs, _els}) ->
    {Config, Destroy} = decode_muc_owner_els(__TopXMLNS,
					     __IgnoreEls, _els, undefined,
					     undefined),
    {muc_owner, Destroy, Config}.

decode_muc_owner_els(__TopXMLNS, __IgnoreEls, [],
		     Config, Destroy) ->
    {Config, Destroy};
decode_muc_owner_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"destroy">>, _attrs, _} = _el | _els],
		     Config, Destroy) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_owner_els(__TopXMLNS, __IgnoreEls, _els,
				Config,
				decode_muc_owner_destroy(__TopXMLNS,
							 __IgnoreEls, _el));
       true ->
	   decode_muc_owner_els(__TopXMLNS, __IgnoreEls, _els,
				Config, Destroy)
    end;
decode_muc_owner_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"x">>, _attrs, _} = _el | _els], Config,
		     Destroy) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<"jabber:x:data">> ->
	   decode_muc_owner_els(__TopXMLNS, __IgnoreEls, _els,
				decode_xdata(_xmlns, __IgnoreEls, _el),
				Destroy);
       true ->
	   decode_muc_owner_els(__TopXMLNS, __IgnoreEls, _els,
				Config, Destroy)
    end;
decode_muc_owner_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Config, Destroy) ->
    decode_muc_owner_els(__TopXMLNS, __IgnoreEls, _els,
			 Config, Destroy).

encode_muc_owner({muc_owner, Destroy, Config},
		 _xmlns_attrs) ->
    _els = lists:reverse('encode_muc_owner_$config'(Config,
						    'encode_muc_owner_$destroy'(Destroy,
										[]))),
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

decode_muc_owner_destroy(__TopXMLNS, __IgnoreEls,
			 {xmlel, <<"destroy">>, _attrs, _els}) ->
    {Password, Reason} =
	decode_muc_owner_destroy_els(__TopXMLNS, __IgnoreEls,
				     _els, undefined, undefined),
    Jid = decode_muc_owner_destroy_attrs(__TopXMLNS, _attrs,
					 undefined),
    {muc_owner_destroy, Jid, Reason, Password}.

decode_muc_owner_destroy_els(__TopXMLNS, __IgnoreEls,
			     [], Password, Reason) ->
    {Password, Reason};
decode_muc_owner_destroy_els(__TopXMLNS, __IgnoreEls,
			     [{xmlel, <<"password">>, _attrs, _} = _el | _els],
			     Password, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_owner_destroy_els(__TopXMLNS, __IgnoreEls,
					_els,
					decode_muc_owner_password(__TopXMLNS,
								  __IgnoreEls,
								  _el),
					Reason);
       true ->
	   decode_muc_owner_destroy_els(__TopXMLNS, __IgnoreEls,
					_els, Password, Reason)
    end;
decode_muc_owner_destroy_els(__TopXMLNS, __IgnoreEls,
			     [{xmlel, <<"reason">>, _attrs, _} = _el | _els],
			     Password, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_owner_destroy_els(__TopXMLNS, __IgnoreEls,
					_els, Password,
					decode_muc_owner_reason(__TopXMLNS,
								__IgnoreEls,
								_el));
       true ->
	   decode_muc_owner_destroy_els(__TopXMLNS, __IgnoreEls,
					_els, Password, Reason)
    end;
decode_muc_owner_destroy_els(__TopXMLNS, __IgnoreEls,
			     [_ | _els], Password, Reason) ->
    decode_muc_owner_destroy_els(__TopXMLNS, __IgnoreEls,
				 _els, Password, Reason).

decode_muc_owner_destroy_attrs(__TopXMLNS,
			       [{<<"jid">>, _val} | _attrs], _Jid) ->
    decode_muc_owner_destroy_attrs(__TopXMLNS, _attrs,
				   _val);
decode_muc_owner_destroy_attrs(__TopXMLNS, [_ | _attrs],
			       Jid) ->
    decode_muc_owner_destroy_attrs(__TopXMLNS, _attrs, Jid);
decode_muc_owner_destroy_attrs(__TopXMLNS, [], Jid) ->
    decode_muc_owner_destroy_attr_jid(__TopXMLNS, Jid).

encode_muc_owner_destroy({muc_owner_destroy, Jid,
			  Reason, Password},
			 _xmlns_attrs) ->
    _els =
	lists:reverse('encode_muc_owner_destroy_$password'(Password,
							   'encode_muc_owner_destroy_$reason'(Reason,
											      []))),
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

decode_muc_owner_destroy_attr_jid(__TopXMLNS,
				  undefined) ->
    undefined;
decode_muc_owner_destroy_attr_jid(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"destroy">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_muc_owner_destroy_attr_jid(undefined, _acc) ->
    _acc;
encode_muc_owner_destroy_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_muc_owner_reason(__TopXMLNS, __IgnoreEls,
			{xmlel, <<"reason">>, _attrs, _els}) ->
    Cdata = decode_muc_owner_reason_els(__TopXMLNS,
					__IgnoreEls, _els, <<>>),
    Cdata.

decode_muc_owner_reason_els(__TopXMLNS, __IgnoreEls, [],
			    Cdata) ->
    decode_muc_owner_reason_cdata(__TopXMLNS, Cdata);
decode_muc_owner_reason_els(__TopXMLNS, __IgnoreEls,
			    [{xmlcdata, _data} | _els], Cdata) ->
    decode_muc_owner_reason_els(__TopXMLNS, __IgnoreEls,
				_els, <<Cdata/binary, _data/binary>>);
decode_muc_owner_reason_els(__TopXMLNS, __IgnoreEls,
			    [_ | _els], Cdata) ->
    decode_muc_owner_reason_els(__TopXMLNS, __IgnoreEls,
				_els, Cdata).

encode_muc_owner_reason(Cdata, _xmlns_attrs) ->
    _els = encode_muc_owner_reason_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"reason">>, _attrs, _els}.

decode_muc_owner_reason_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_muc_owner_reason_cdata(__TopXMLNS, _val) -> _val.

encode_muc_owner_reason_cdata(undefined, _acc) -> _acc;
encode_muc_owner_reason_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_muc_owner_password(__TopXMLNS, __IgnoreEls,
			  {xmlel, <<"password">>, _attrs, _els}) ->
    Cdata = decode_muc_owner_password_els(__TopXMLNS,
					  __IgnoreEls, _els, <<>>),
    Cdata.

decode_muc_owner_password_els(__TopXMLNS, __IgnoreEls,
			      [], Cdata) ->
    decode_muc_owner_password_cdata(__TopXMLNS, Cdata);
decode_muc_owner_password_els(__TopXMLNS, __IgnoreEls,
			      [{xmlcdata, _data} | _els], Cdata) ->
    decode_muc_owner_password_els(__TopXMLNS, __IgnoreEls,
				  _els, <<Cdata/binary, _data/binary>>);
decode_muc_owner_password_els(__TopXMLNS, __IgnoreEls,
			      [_ | _els], Cdata) ->
    decode_muc_owner_password_els(__TopXMLNS, __IgnoreEls,
				  _els, Cdata).

encode_muc_owner_password(Cdata, _xmlns_attrs) ->
    _els = encode_muc_owner_password_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"password">>, _attrs, _els}.

decode_muc_owner_password_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_muc_owner_password_cdata(__TopXMLNS, _val) ->
    _val.

encode_muc_owner_password_cdata(undefined, _acc) ->
    _acc;
encode_muc_owner_password_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_muc_user(__TopXMLNS, __IgnoreEls,
		{xmlel, <<"x">>, _attrs, _els}) ->
    {Status_codes, Items, Invites, Decline, Destroy} =
	decode_muc_user_els(__TopXMLNS, __IgnoreEls, _els, [],
			    [], [], undefined, undefined),
    Password = decode_muc_user_attrs(__TopXMLNS, _attrs,
				     undefined),
    {muc_user, Decline, Destroy, Invites, Items,
     Status_codes, Password}.

decode_muc_user_els(__TopXMLNS, __IgnoreEls, [],
		    Status_codes, Items, Invites, Decline, Destroy) ->
    {lists:reverse(Status_codes), lists:reverse(Items),
     lists:reverse(Invites), Decline, Destroy};
decode_muc_user_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"decline">>, _attrs, _} = _el | _els],
		    Status_codes, Items, Invites, Decline, Destroy) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_user_els(__TopXMLNS, __IgnoreEls, _els,
			       Status_codes, Items, Invites,
			       decode_muc_user_decline(__TopXMLNS, __IgnoreEls,
						       _el),
			       Destroy);
       true ->
	   decode_muc_user_els(__TopXMLNS, __IgnoreEls, _els,
			       Status_codes, Items, Invites, Decline, Destroy)
    end;
decode_muc_user_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"destroy">>, _attrs, _} = _el | _els],
		    Status_codes, Items, Invites, Decline, Destroy) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_user_els(__TopXMLNS, __IgnoreEls, _els,
			       Status_codes, Items, Invites, Decline,
			       decode_muc_user_destroy(__TopXMLNS, __IgnoreEls,
						       _el));
       true ->
	   decode_muc_user_els(__TopXMLNS, __IgnoreEls, _els,
			       Status_codes, Items, Invites, Decline, Destroy)
    end;
decode_muc_user_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"invite">>, _attrs, _} = _el | _els],
		    Status_codes, Items, Invites, Decline, Destroy) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_user_els(__TopXMLNS, __IgnoreEls, _els,
			       Status_codes, Items,
			       [decode_muc_user_invite(__TopXMLNS, __IgnoreEls,
						       _el)
				| Invites],
			       Decline, Destroy);
       true ->
	   decode_muc_user_els(__TopXMLNS, __IgnoreEls, _els,
			       Status_codes, Items, Invites, Decline, Destroy)
    end;
decode_muc_user_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"item">>, _attrs, _} = _el | _els],
		    Status_codes, Items, Invites, Decline, Destroy) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_user_els(__TopXMLNS, __IgnoreEls, _els,
			       Status_codes,
			       [decode_muc_user_item(__TopXMLNS, __IgnoreEls,
						     _el)
				| Items],
			       Invites, Decline, Destroy);
       true ->
	   decode_muc_user_els(__TopXMLNS, __IgnoreEls, _els,
			       Status_codes, Items, Invites, Decline, Destroy)
    end;
decode_muc_user_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"status">>, _attrs, _} = _el | _els],
		    Status_codes, Items, Invites, Decline, Destroy) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_user_els(__TopXMLNS, __IgnoreEls, _els,
			       case decode_muc_user_status(__TopXMLNS,
							   __IgnoreEls, _el)
				   of
				 undefined -> Status_codes;
				 _new_el -> [_new_el | Status_codes]
			       end,
			       Items, Invites, Decline, Destroy);
       true ->
	   decode_muc_user_els(__TopXMLNS, __IgnoreEls, _els,
			       Status_codes, Items, Invites, Decline, Destroy)
    end;
decode_muc_user_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		    Status_codes, Items, Invites, Decline, Destroy) ->
    decode_muc_user_els(__TopXMLNS, __IgnoreEls, _els,
			Status_codes, Items, Invites, Decline, Destroy).

decode_muc_user_attrs(__TopXMLNS,
		      [{<<"password">>, _val} | _attrs], _Password) ->
    decode_muc_user_attrs(__TopXMLNS, _attrs, _val);
decode_muc_user_attrs(__TopXMLNS, [_ | _attrs],
		      Password) ->
    decode_muc_user_attrs(__TopXMLNS, _attrs, Password);
decode_muc_user_attrs(__TopXMLNS, [], Password) ->
    decode_muc_user_attr_password(__TopXMLNS, Password).

encode_muc_user({muc_user, Decline, Destroy, Invites,
		 Items, Status_codes, Password},
		_xmlns_attrs) ->
    _els =
	lists:reverse('encode_muc_user_$status_codes'(Status_codes,
						      'encode_muc_user_$items'(Items,
									       'encode_muc_user_$invites'(Invites,
													  'encode_muc_user_$decline'(Decline,
																     'encode_muc_user_$destroy'(Destroy,
																				[])))))),
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

decode_muc_user_attr_password(__TopXMLNS, undefined) ->
    undefined;
decode_muc_user_attr_password(__TopXMLNS, _val) -> _val.

encode_muc_user_attr_password(undefined, _acc) -> _acc;
encode_muc_user_attr_password(_val, _acc) ->
    [{<<"password">>, _val} | _acc].

decode_muc_user_item(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"item">>, _attrs, _els}) ->
    {Actor, Continue, Reason} =
	decode_muc_user_item_els(__TopXMLNS, __IgnoreEls, _els,
				 undefined, undefined, undefined),
    {Affiliation, Role, Jid, Nick} =
	decode_muc_user_item_attrs(__TopXMLNS, _attrs,
				   undefined, undefined, undefined, undefined),
    {muc_item, Actor, Continue, Reason, Affiliation, Role,
     Jid, Nick}.

decode_muc_user_item_els(__TopXMLNS, __IgnoreEls, [],
			 Actor, Continue, Reason) ->
    {Actor, Continue, Reason};
decode_muc_user_item_els(__TopXMLNS, __IgnoreEls,
			 [{xmlel, <<"actor">>, _attrs, _} = _el | _els], Actor,
			 Continue, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_user_item_els(__TopXMLNS, __IgnoreEls, _els,
				    decode_muc_user_actor(__TopXMLNS,
							  __IgnoreEls, _el),
				    Continue, Reason);
       true ->
	   decode_muc_user_item_els(__TopXMLNS, __IgnoreEls, _els,
				    Actor, Continue, Reason)
    end;
decode_muc_user_item_els(__TopXMLNS, __IgnoreEls,
			 [{xmlel, <<"continue">>, _attrs, _} = _el | _els],
			 Actor, Continue, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_user_item_els(__TopXMLNS, __IgnoreEls, _els,
				    Actor,
				    decode_muc_user_continue(__TopXMLNS,
							     __IgnoreEls, _el),
				    Reason);
       true ->
	   decode_muc_user_item_els(__TopXMLNS, __IgnoreEls, _els,
				    Actor, Continue, Reason)
    end;
decode_muc_user_item_els(__TopXMLNS, __IgnoreEls,
			 [{xmlel, <<"reason">>, _attrs, _} = _el | _els], Actor,
			 Continue, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_user_item_els(__TopXMLNS, __IgnoreEls, _els,
				    Actor, Continue,
				    decode_muc_user_reason(__TopXMLNS,
							   __IgnoreEls, _el));
       true ->
	   decode_muc_user_item_els(__TopXMLNS, __IgnoreEls, _els,
				    Actor, Continue, Reason)
    end;
decode_muc_user_item_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Actor, Continue, Reason) ->
    decode_muc_user_item_els(__TopXMLNS, __IgnoreEls, _els,
			     Actor, Continue, Reason).

decode_muc_user_item_attrs(__TopXMLNS,
			   [{<<"affiliation">>, _val} | _attrs], _Affiliation,
			   Role, Jid, Nick) ->
    decode_muc_user_item_attrs(__TopXMLNS, _attrs, _val,
			       Role, Jid, Nick);
decode_muc_user_item_attrs(__TopXMLNS,
			   [{<<"role">>, _val} | _attrs], Affiliation, _Role,
			   Jid, Nick) ->
    decode_muc_user_item_attrs(__TopXMLNS, _attrs,
			       Affiliation, _val, Jid, Nick);
decode_muc_user_item_attrs(__TopXMLNS,
			   [{<<"jid">>, _val} | _attrs], Affiliation, Role,
			   _Jid, Nick) ->
    decode_muc_user_item_attrs(__TopXMLNS, _attrs,
			       Affiliation, Role, _val, Nick);
decode_muc_user_item_attrs(__TopXMLNS,
			   [{<<"nick">>, _val} | _attrs], Affiliation, Role,
			   Jid, _Nick) ->
    decode_muc_user_item_attrs(__TopXMLNS, _attrs,
			       Affiliation, Role, Jid, _val);
decode_muc_user_item_attrs(__TopXMLNS, [_ | _attrs],
			   Affiliation, Role, Jid, Nick) ->
    decode_muc_user_item_attrs(__TopXMLNS, _attrs,
			       Affiliation, Role, Jid, Nick);
decode_muc_user_item_attrs(__TopXMLNS, [], Affiliation,
			   Role, Jid, Nick) ->
    {decode_muc_user_item_attr_affiliation(__TopXMLNS,
					   Affiliation),
     decode_muc_user_item_attr_role(__TopXMLNS, Role),
     decode_muc_user_item_attr_jid(__TopXMLNS, Jid),
     decode_muc_user_item_attr_nick(__TopXMLNS, Nick)}.

encode_muc_user_item({muc_item, Actor, Continue, Reason,
		      Affiliation, Role, Jid, Nick},
		     _xmlns_attrs) ->
    _els =
	lists:reverse('encode_muc_user_item_$actor'(Actor,
						    'encode_muc_user_item_$continue'(Continue,
										     'encode_muc_user_item_$reason'(Reason,
														    [])))),
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

decode_muc_user_item_attr_affiliation(__TopXMLNS,
				      undefined) ->
    undefined;
decode_muc_user_item_attr_affiliation(__TopXMLNS,
				      _val) ->
    case catch dec_enum(_val,
			[admin, member, none, outcast, owner])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"affiliation">>, <<"item">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_muc_user_item_attr_affiliation(undefined,
				      _acc) ->
    _acc;
encode_muc_user_item_attr_affiliation(_val, _acc) ->
    [{<<"affiliation">>, enc_enum(_val)} | _acc].

decode_muc_user_item_attr_role(__TopXMLNS, undefined) ->
    undefined;
decode_muc_user_item_attr_role(__TopXMLNS, _val) ->
    case catch dec_enum(_val,
			[moderator, none, participant, visitor])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"role">>, <<"item">>, __TopXMLNS}});
      _res -> _res
    end.

encode_muc_user_item_attr_role(undefined, _acc) -> _acc;
encode_muc_user_item_attr_role(_val, _acc) ->
    [{<<"role">>, enc_enum(_val)} | _acc].

decode_muc_user_item_attr_jid(__TopXMLNS, undefined) ->
    undefined;
decode_muc_user_item_attr_jid(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"item">>, __TopXMLNS}});
      _res -> _res
    end.

encode_muc_user_item_attr_jid(undefined, _acc) -> _acc;
encode_muc_user_item_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_muc_user_item_attr_nick(__TopXMLNS, undefined) ->
    undefined;
decode_muc_user_item_attr_nick(__TopXMLNS, _val) ->
    _val.

encode_muc_user_item_attr_nick(undefined, _acc) -> _acc;
encode_muc_user_item_attr_nick(_val, _acc) ->
    [{<<"nick">>, _val} | _acc].

decode_muc_user_status(__TopXMLNS, __IgnoreEls,
		       {xmlel, <<"status">>, _attrs, _els}) ->
    Code = decode_muc_user_status_attrs(__TopXMLNS, _attrs,
					undefined),
    Code.

decode_muc_user_status_attrs(__TopXMLNS,
			     [{<<"code">>, _val} | _attrs], _Code) ->
    decode_muc_user_status_attrs(__TopXMLNS, _attrs, _val);
decode_muc_user_status_attrs(__TopXMLNS, [_ | _attrs],
			     Code) ->
    decode_muc_user_status_attrs(__TopXMLNS, _attrs, Code);
decode_muc_user_status_attrs(__TopXMLNS, [], Code) ->
    decode_muc_user_status_attr_code(__TopXMLNS, Code).

encode_muc_user_status(Code, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_muc_user_status_attr_code(Code,
					      _xmlns_attrs),
    {xmlel, <<"status">>, _attrs, _els}.

decode_muc_user_status_attr_code(__TopXMLNS,
				 undefined) ->
    undefined;
decode_muc_user_status_attr_code(__TopXMLNS, _val) ->
    case catch dec_int(_val, 100, 999) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"code">>, <<"status">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_muc_user_status_attr_code(undefined, _acc) ->
    _acc;
encode_muc_user_status_attr_code(_val, _acc) ->
    [{<<"code">>, enc_int(_val)} | _acc].

decode_muc_user_continue(__TopXMLNS, __IgnoreEls,
			 {xmlel, <<"continue">>, _attrs, _els}) ->
    Thread = decode_muc_user_continue_attrs(__TopXMLNS,
					    _attrs, undefined),
    Thread.

decode_muc_user_continue_attrs(__TopXMLNS,
			       [{<<"thread">>, _val} | _attrs], _Thread) ->
    decode_muc_user_continue_attrs(__TopXMLNS, _attrs,
				   _val);
decode_muc_user_continue_attrs(__TopXMLNS, [_ | _attrs],
			       Thread) ->
    decode_muc_user_continue_attrs(__TopXMLNS, _attrs,
				   Thread);
decode_muc_user_continue_attrs(__TopXMLNS, [],
			       Thread) ->
    decode_muc_user_continue_attr_thread(__TopXMLNS,
					 Thread).

encode_muc_user_continue(Thread, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_muc_user_continue_attr_thread(Thread,
						  _xmlns_attrs),
    {xmlel, <<"continue">>, _attrs, _els}.

decode_muc_user_continue_attr_thread(__TopXMLNS,
				     undefined) ->
    undefined;
decode_muc_user_continue_attr_thread(__TopXMLNS,
				     _val) ->
    _val.

encode_muc_user_continue_attr_thread(undefined, _acc) ->
    _acc;
encode_muc_user_continue_attr_thread(_val, _acc) ->
    [{<<"thread">>, _val} | _acc].

decode_muc_user_actor(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"actor">>, _attrs, _els}) ->
    {Jid, Nick} = decode_muc_user_actor_attrs(__TopXMLNS,
					      _attrs, undefined, undefined),
    {muc_actor, Jid, Nick}.

decode_muc_user_actor_attrs(__TopXMLNS,
			    [{<<"jid">>, _val} | _attrs], _Jid, Nick) ->
    decode_muc_user_actor_attrs(__TopXMLNS, _attrs, _val,
				Nick);
decode_muc_user_actor_attrs(__TopXMLNS,
			    [{<<"nick">>, _val} | _attrs], Jid, _Nick) ->
    decode_muc_user_actor_attrs(__TopXMLNS, _attrs, Jid,
				_val);
decode_muc_user_actor_attrs(__TopXMLNS, [_ | _attrs],
			    Jid, Nick) ->
    decode_muc_user_actor_attrs(__TopXMLNS, _attrs, Jid,
				Nick);
decode_muc_user_actor_attrs(__TopXMLNS, [], Jid,
			    Nick) ->
    {decode_muc_user_actor_attr_jid(__TopXMLNS, Jid),
     decode_muc_user_actor_attr_nick(__TopXMLNS, Nick)}.

encode_muc_user_actor({muc_actor, Jid, Nick},
		      _xmlns_attrs) ->
    _els = [],
    _attrs = encode_muc_user_actor_attr_nick(Nick,
					     encode_muc_user_actor_attr_jid(Jid,
									    _xmlns_attrs)),
    {xmlel, <<"actor">>, _attrs, _els}.

decode_muc_user_actor_attr_jid(__TopXMLNS, undefined) ->
    undefined;
decode_muc_user_actor_attr_jid(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"actor">>, __TopXMLNS}});
      _res -> _res
    end.

encode_muc_user_actor_attr_jid(undefined, _acc) -> _acc;
encode_muc_user_actor_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_muc_user_actor_attr_nick(__TopXMLNS,
				undefined) ->
    undefined;
decode_muc_user_actor_attr_nick(__TopXMLNS, _val) ->
    _val.

encode_muc_user_actor_attr_nick(undefined, _acc) ->
    _acc;
encode_muc_user_actor_attr_nick(_val, _acc) ->
    [{<<"nick">>, _val} | _acc].

decode_muc_user_invite(__TopXMLNS, __IgnoreEls,
		       {xmlel, <<"invite">>, _attrs, _els}) ->
    Reason = decode_muc_user_invite_els(__TopXMLNS,
					__IgnoreEls, _els, undefined),
    {To, From} = decode_muc_user_invite_attrs(__TopXMLNS,
					      _attrs, undefined, undefined),
    {muc_invite, Reason, From, To}.

decode_muc_user_invite_els(__TopXMLNS, __IgnoreEls, [],
			   Reason) ->
    Reason;
decode_muc_user_invite_els(__TopXMLNS, __IgnoreEls,
			   [{xmlel, <<"reason">>, _attrs, _} = _el | _els],
			   Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_user_invite_els(__TopXMLNS, __IgnoreEls,
				      _els,
				      decode_muc_user_reason(__TopXMLNS,
							     __IgnoreEls, _el));
       true ->
	   decode_muc_user_invite_els(__TopXMLNS, __IgnoreEls,
				      _els, Reason)
    end;
decode_muc_user_invite_els(__TopXMLNS, __IgnoreEls,
			   [_ | _els], Reason) ->
    decode_muc_user_invite_els(__TopXMLNS, __IgnoreEls,
			       _els, Reason).

decode_muc_user_invite_attrs(__TopXMLNS,
			     [{<<"to">>, _val} | _attrs], _To, From) ->
    decode_muc_user_invite_attrs(__TopXMLNS, _attrs, _val,
				 From);
decode_muc_user_invite_attrs(__TopXMLNS,
			     [{<<"from">>, _val} | _attrs], To, _From) ->
    decode_muc_user_invite_attrs(__TopXMLNS, _attrs, To,
				 _val);
decode_muc_user_invite_attrs(__TopXMLNS, [_ | _attrs],
			     To, From) ->
    decode_muc_user_invite_attrs(__TopXMLNS, _attrs, To,
				 From);
decode_muc_user_invite_attrs(__TopXMLNS, [], To,
			     From) ->
    {decode_muc_user_invite_attr_to(__TopXMLNS, To),
     decode_muc_user_invite_attr_from(__TopXMLNS, From)}.

encode_muc_user_invite({muc_invite, Reason, From, To},
		       _xmlns_attrs) ->
    _els =
	lists:reverse('encode_muc_user_invite_$reason'(Reason,
						       [])),
    _attrs = encode_muc_user_invite_attr_from(From,
					      encode_muc_user_invite_attr_to(To,
									     _xmlns_attrs)),
    {xmlel, <<"invite">>, _attrs, _els}.

'encode_muc_user_invite_$reason'(undefined, _acc) ->
    _acc;
'encode_muc_user_invite_$reason'(Reason, _acc) ->
    [encode_muc_user_reason(Reason, []) | _acc].

decode_muc_user_invite_attr_to(__TopXMLNS, undefined) ->
    undefined;
decode_muc_user_invite_attr_to(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"to">>, <<"invite">>, __TopXMLNS}});
      _res -> _res
    end.

encode_muc_user_invite_attr_to(undefined, _acc) -> _acc;
encode_muc_user_invite_attr_to(_val, _acc) ->
    [{<<"to">>, enc_jid(_val)} | _acc].

decode_muc_user_invite_attr_from(__TopXMLNS,
				 undefined) ->
    undefined;
decode_muc_user_invite_attr_from(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"from">>, <<"invite">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_muc_user_invite_attr_from(undefined, _acc) ->
    _acc;
encode_muc_user_invite_attr_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_muc_user_destroy(__TopXMLNS, __IgnoreEls,
			{xmlel, <<"destroy">>, _attrs, _els}) ->
    Reason = decode_muc_user_destroy_els(__TopXMLNS,
					 __IgnoreEls, _els, undefined),
    Jid = decode_muc_user_destroy_attrs(__TopXMLNS, _attrs,
					undefined),
    {muc_user_destroy, Reason, Jid}.

decode_muc_user_destroy_els(__TopXMLNS, __IgnoreEls, [],
			    Reason) ->
    Reason;
decode_muc_user_destroy_els(__TopXMLNS, __IgnoreEls,
			    [{xmlel, <<"reason">>, _attrs, _} = _el | _els],
			    Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_user_destroy_els(__TopXMLNS, __IgnoreEls,
				       _els,
				       decode_muc_user_reason(__TopXMLNS,
							      __IgnoreEls,
							      _el));
       true ->
	   decode_muc_user_destroy_els(__TopXMLNS, __IgnoreEls,
				       _els, Reason)
    end;
decode_muc_user_destroy_els(__TopXMLNS, __IgnoreEls,
			    [_ | _els], Reason) ->
    decode_muc_user_destroy_els(__TopXMLNS, __IgnoreEls,
				_els, Reason).

decode_muc_user_destroy_attrs(__TopXMLNS,
			      [{<<"jid">>, _val} | _attrs], _Jid) ->
    decode_muc_user_destroy_attrs(__TopXMLNS, _attrs, _val);
decode_muc_user_destroy_attrs(__TopXMLNS, [_ | _attrs],
			      Jid) ->
    decode_muc_user_destroy_attrs(__TopXMLNS, _attrs, Jid);
decode_muc_user_destroy_attrs(__TopXMLNS, [], Jid) ->
    decode_muc_user_destroy_attr_jid(__TopXMLNS, Jid).

encode_muc_user_destroy({muc_user_destroy, Reason, Jid},
			_xmlns_attrs) ->
    _els =
	lists:reverse('encode_muc_user_destroy_$reason'(Reason,
							[])),
    _attrs = encode_muc_user_destroy_attr_jid(Jid,
					      _xmlns_attrs),
    {xmlel, <<"destroy">>, _attrs, _els}.

'encode_muc_user_destroy_$reason'(undefined, _acc) ->
    _acc;
'encode_muc_user_destroy_$reason'(Reason, _acc) ->
    [encode_muc_user_reason(Reason, []) | _acc].

decode_muc_user_destroy_attr_jid(__TopXMLNS,
				 undefined) ->
    undefined;
decode_muc_user_destroy_attr_jid(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"destroy">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_muc_user_destroy_attr_jid(undefined, _acc) ->
    _acc;
encode_muc_user_destroy_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_muc_user_decline(__TopXMLNS, __IgnoreEls,
			{xmlel, <<"decline">>, _attrs, _els}) ->
    Reason = decode_muc_user_decline_els(__TopXMLNS,
					 __IgnoreEls, _els, undefined),
    {To, From} = decode_muc_user_decline_attrs(__TopXMLNS,
					       _attrs, undefined, undefined),
    {muc_decline, Reason, From, To}.

decode_muc_user_decline_els(__TopXMLNS, __IgnoreEls, [],
			    Reason) ->
    Reason;
decode_muc_user_decline_els(__TopXMLNS, __IgnoreEls,
			    [{xmlel, <<"reason">>, _attrs, _} = _el | _els],
			    Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_muc_user_decline_els(__TopXMLNS, __IgnoreEls,
				       _els,
				       decode_muc_user_reason(__TopXMLNS,
							      __IgnoreEls,
							      _el));
       true ->
	   decode_muc_user_decline_els(__TopXMLNS, __IgnoreEls,
				       _els, Reason)
    end;
decode_muc_user_decline_els(__TopXMLNS, __IgnoreEls,
			    [_ | _els], Reason) ->
    decode_muc_user_decline_els(__TopXMLNS, __IgnoreEls,
				_els, Reason).

decode_muc_user_decline_attrs(__TopXMLNS,
			      [{<<"to">>, _val} | _attrs], _To, From) ->
    decode_muc_user_decline_attrs(__TopXMLNS, _attrs, _val,
				  From);
decode_muc_user_decline_attrs(__TopXMLNS,
			      [{<<"from">>, _val} | _attrs], To, _From) ->
    decode_muc_user_decline_attrs(__TopXMLNS, _attrs, To,
				  _val);
decode_muc_user_decline_attrs(__TopXMLNS, [_ | _attrs],
			      To, From) ->
    decode_muc_user_decline_attrs(__TopXMLNS, _attrs, To,
				  From);
decode_muc_user_decline_attrs(__TopXMLNS, [], To,
			      From) ->
    {decode_muc_user_decline_attr_to(__TopXMLNS, To),
     decode_muc_user_decline_attr_from(__TopXMLNS, From)}.

encode_muc_user_decline({muc_decline, Reason, From, To},
			_xmlns_attrs) ->
    _els =
	lists:reverse('encode_muc_user_decline_$reason'(Reason,
							[])),
    _attrs = encode_muc_user_decline_attr_from(From,
					       encode_muc_user_decline_attr_to(To,
									       _xmlns_attrs)),
    {xmlel, <<"decline">>, _attrs, _els}.

'encode_muc_user_decline_$reason'(undefined, _acc) ->
    _acc;
'encode_muc_user_decline_$reason'(Reason, _acc) ->
    [encode_muc_user_reason(Reason, []) | _acc].

decode_muc_user_decline_attr_to(__TopXMLNS,
				undefined) ->
    undefined;
decode_muc_user_decline_attr_to(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"to">>, <<"decline">>, __TopXMLNS}});
      _res -> _res
    end.

encode_muc_user_decline_attr_to(undefined, _acc) ->
    _acc;
encode_muc_user_decline_attr_to(_val, _acc) ->
    [{<<"to">>, enc_jid(_val)} | _acc].

decode_muc_user_decline_attr_from(__TopXMLNS,
				  undefined) ->
    undefined;
decode_muc_user_decline_attr_from(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"from">>, <<"decline">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_muc_user_decline_attr_from(undefined, _acc) ->
    _acc;
encode_muc_user_decline_attr_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_muc_user_reason(__TopXMLNS, __IgnoreEls,
		       {xmlel, <<"reason">>, _attrs, _els}) ->
    Cdata = decode_muc_user_reason_els(__TopXMLNS,
				       __IgnoreEls, _els, <<>>),
    Cdata.

decode_muc_user_reason_els(__TopXMLNS, __IgnoreEls, [],
			   Cdata) ->
    decode_muc_user_reason_cdata(__TopXMLNS, Cdata);
decode_muc_user_reason_els(__TopXMLNS, __IgnoreEls,
			   [{xmlcdata, _data} | _els], Cdata) ->
    decode_muc_user_reason_els(__TopXMLNS, __IgnoreEls,
			       _els, <<Cdata/binary, _data/binary>>);
decode_muc_user_reason_els(__TopXMLNS, __IgnoreEls,
			   [_ | _els], Cdata) ->
    decode_muc_user_reason_els(__TopXMLNS, __IgnoreEls,
			       _els, Cdata).

encode_muc_user_reason(Cdata, _xmlns_attrs) ->
    _els = encode_muc_user_reason_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"reason">>, _attrs, _els}.

decode_muc_user_reason_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_muc_user_reason_cdata(__TopXMLNS, _val) -> _val.

encode_muc_user_reason_cdata(undefined, _acc) -> _acc;
encode_muc_user_reason_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_muc_history(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"history">>, _attrs, _els}) ->
    {Maxchars, Maxstanzas, Seconds, Since} =
	decode_muc_history_attrs(__TopXMLNS, _attrs, undefined,
				 undefined, undefined, undefined),
    {muc_history, Maxchars, Maxstanzas, Seconds, Since}.

decode_muc_history_attrs(__TopXMLNS,
			 [{<<"maxchars">>, _val} | _attrs], _Maxchars,
			 Maxstanzas, Seconds, Since) ->
    decode_muc_history_attrs(__TopXMLNS, _attrs, _val,
			     Maxstanzas, Seconds, Since);
decode_muc_history_attrs(__TopXMLNS,
			 [{<<"maxstanzas">>, _val} | _attrs], Maxchars,
			 _Maxstanzas, Seconds, Since) ->
    decode_muc_history_attrs(__TopXMLNS, _attrs, Maxchars,
			     _val, Seconds, Since);
decode_muc_history_attrs(__TopXMLNS,
			 [{<<"seconds">>, _val} | _attrs], Maxchars, Maxstanzas,
			 _Seconds, Since) ->
    decode_muc_history_attrs(__TopXMLNS, _attrs, Maxchars,
			     Maxstanzas, _val, Since);
decode_muc_history_attrs(__TopXMLNS,
			 [{<<"since">>, _val} | _attrs], Maxchars, Maxstanzas,
			 Seconds, _Since) ->
    decode_muc_history_attrs(__TopXMLNS, _attrs, Maxchars,
			     Maxstanzas, Seconds, _val);
decode_muc_history_attrs(__TopXMLNS, [_ | _attrs],
			 Maxchars, Maxstanzas, Seconds, Since) ->
    decode_muc_history_attrs(__TopXMLNS, _attrs, Maxchars,
			     Maxstanzas, Seconds, Since);
decode_muc_history_attrs(__TopXMLNS, [], Maxchars,
			 Maxstanzas, Seconds, Since) ->
    {decode_muc_history_attr_maxchars(__TopXMLNS, Maxchars),
     decode_muc_history_attr_maxstanzas(__TopXMLNS,
					Maxstanzas),
     decode_muc_history_attr_seconds(__TopXMLNS, Seconds),
     decode_muc_history_attr_since(__TopXMLNS, Since)}.

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

decode_muc_history_attr_maxchars(__TopXMLNS,
				 undefined) ->
    undefined;
decode_muc_history_attr_maxchars(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"maxchars">>, <<"history">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_muc_history_attr_maxchars(undefined, _acc) ->
    _acc;
encode_muc_history_attr_maxchars(_val, _acc) ->
    [{<<"maxchars">>, enc_int(_val)} | _acc].

decode_muc_history_attr_maxstanzas(__TopXMLNS,
				   undefined) ->
    undefined;
decode_muc_history_attr_maxstanzas(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"maxstanzas">>, <<"history">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_muc_history_attr_maxstanzas(undefined, _acc) ->
    _acc;
encode_muc_history_attr_maxstanzas(_val, _acc) ->
    [{<<"maxstanzas">>, enc_int(_val)} | _acc].

decode_muc_history_attr_seconds(__TopXMLNS,
				undefined) ->
    undefined;
decode_muc_history_attr_seconds(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"seconds">>, <<"history">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_muc_history_attr_seconds(undefined, _acc) ->
    _acc;
encode_muc_history_attr_seconds(_val, _acc) ->
    [{<<"seconds">>, enc_int(_val)} | _acc].

decode_muc_history_attr_since(__TopXMLNS, undefined) ->
    undefined;
decode_muc_history_attr_since(__TopXMLNS, _val) ->
    case catch dec_utc(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"since">>, <<"history">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_muc_history_attr_since(undefined, _acc) -> _acc;
encode_muc_history_attr_since(_val, _acc) ->
    [{<<"since">>, enc_utc(_val)} | _acc].

decode_bytestreams(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"query">>, _attrs, _els}) ->
    {Hosts, Used, Activate} =
	decode_bytestreams_els(__TopXMLNS, __IgnoreEls, _els,
			       [], undefined, undefined),
    {Dstaddr, Sid, Mode} =
	decode_bytestreams_attrs(__TopXMLNS, _attrs, undefined,
				 undefined, undefined),
    {bytestreams, Hosts, Used, Activate, Dstaddr, Mode,
     Sid}.

decode_bytestreams_els(__TopXMLNS, __IgnoreEls, [],
		       Hosts, Used, Activate) ->
    {lists:reverse(Hosts), Used, Activate};
decode_bytestreams_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"streamhost">>, _attrs, _} = _el | _els],
		       Hosts, Used, Activate) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_bytestreams_els(__TopXMLNS, __IgnoreEls, _els,
				  [decode_bytestreams_streamhost(__TopXMLNS,
								 __IgnoreEls,
								 _el)
				   | Hosts],
				  Used, Activate);
       true ->
	   decode_bytestreams_els(__TopXMLNS, __IgnoreEls, _els,
				  Hosts, Used, Activate)
    end;
decode_bytestreams_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"streamhost-used">>, _attrs, _} = _el
			| _els],
		       Hosts, Used, Activate) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_bytestreams_els(__TopXMLNS, __IgnoreEls, _els,
				  Hosts,
				  decode_bytestreams_streamhost_used(__TopXMLNS,
								     __IgnoreEls,
								     _el),
				  Activate);
       true ->
	   decode_bytestreams_els(__TopXMLNS, __IgnoreEls, _els,
				  Hosts, Used, Activate)
    end;
decode_bytestreams_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"activate">>, _attrs, _} = _el | _els],
		       Hosts, Used, Activate) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_bytestreams_els(__TopXMLNS, __IgnoreEls, _els,
				  Hosts, Used,
				  decode_bytestreams_activate(__TopXMLNS,
							      __IgnoreEls,
							      _el));
       true ->
	   decode_bytestreams_els(__TopXMLNS, __IgnoreEls, _els,
				  Hosts, Used, Activate)
    end;
decode_bytestreams_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Hosts, Used, Activate) ->
    decode_bytestreams_els(__TopXMLNS, __IgnoreEls, _els,
			   Hosts, Used, Activate).

decode_bytestreams_attrs(__TopXMLNS,
			 [{<<"dstaddr">>, _val} | _attrs], _Dstaddr, Sid,
			 Mode) ->
    decode_bytestreams_attrs(__TopXMLNS, _attrs, _val, Sid,
			     Mode);
decode_bytestreams_attrs(__TopXMLNS,
			 [{<<"sid">>, _val} | _attrs], Dstaddr, _Sid, Mode) ->
    decode_bytestreams_attrs(__TopXMLNS, _attrs, Dstaddr,
			     _val, Mode);
decode_bytestreams_attrs(__TopXMLNS,
			 [{<<"mode">>, _val} | _attrs], Dstaddr, Sid, _Mode) ->
    decode_bytestreams_attrs(__TopXMLNS, _attrs, Dstaddr,
			     Sid, _val);
decode_bytestreams_attrs(__TopXMLNS, [_ | _attrs],
			 Dstaddr, Sid, Mode) ->
    decode_bytestreams_attrs(__TopXMLNS, _attrs, Dstaddr,
			     Sid, Mode);
decode_bytestreams_attrs(__TopXMLNS, [], Dstaddr, Sid,
			 Mode) ->
    {decode_bytestreams_attr_dstaddr(__TopXMLNS, Dstaddr),
     decode_bytestreams_attr_sid(__TopXMLNS, Sid),
     decode_bytestreams_attr_mode(__TopXMLNS, Mode)}.

encode_bytestreams({bytestreams, Hosts, Used, Activate,
		    Dstaddr, Mode, Sid},
		   _xmlns_attrs) ->
    _els = lists:reverse('encode_bytestreams_$hosts'(Hosts,
						     'encode_bytestreams_$used'(Used,
										'encode_bytestreams_$activate'(Activate,
													       [])))),
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

decode_bytestreams_attr_dstaddr(__TopXMLNS,
				undefined) ->
    undefined;
decode_bytestreams_attr_dstaddr(__TopXMLNS, _val) ->
    _val.

encode_bytestreams_attr_dstaddr(undefined, _acc) ->
    _acc;
encode_bytestreams_attr_dstaddr(_val, _acc) ->
    [{<<"dstaddr">>, _val} | _acc].

decode_bytestreams_attr_sid(__TopXMLNS, undefined) ->
    undefined;
decode_bytestreams_attr_sid(__TopXMLNS, _val) -> _val.

encode_bytestreams_attr_sid(undefined, _acc) -> _acc;
encode_bytestreams_attr_sid(_val, _acc) ->
    [{<<"sid">>, _val} | _acc].

decode_bytestreams_attr_mode(__TopXMLNS, undefined) ->
    tcp;
decode_bytestreams_attr_mode(__TopXMLNS, _val) ->
    case catch dec_enum(_val, [tcp, udp]) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"mode">>, <<"query">>, __TopXMLNS}});
      _res -> _res
    end.

encode_bytestreams_attr_mode(tcp, _acc) -> _acc;
encode_bytestreams_attr_mode(_val, _acc) ->
    [{<<"mode">>, enc_enum(_val)} | _acc].

decode_bytestreams_activate(__TopXMLNS, __IgnoreEls,
			    {xmlel, <<"activate">>, _attrs, _els}) ->
    Cdata = decode_bytestreams_activate_els(__TopXMLNS,
					    __IgnoreEls, _els, <<>>),
    Cdata.

decode_bytestreams_activate_els(__TopXMLNS, __IgnoreEls,
				[], Cdata) ->
    decode_bytestreams_activate_cdata(__TopXMLNS, Cdata);
decode_bytestreams_activate_els(__TopXMLNS, __IgnoreEls,
				[{xmlcdata, _data} | _els], Cdata) ->
    decode_bytestreams_activate_els(__TopXMLNS, __IgnoreEls,
				    _els, <<Cdata/binary, _data/binary>>);
decode_bytestreams_activate_els(__TopXMLNS, __IgnoreEls,
				[_ | _els], Cdata) ->
    decode_bytestreams_activate_els(__TopXMLNS, __IgnoreEls,
				    _els, Cdata).

encode_bytestreams_activate(Cdata, _xmlns_attrs) ->
    _els = encode_bytestreams_activate_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"activate">>, _attrs, _els}.

decode_bytestreams_activate_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_bytestreams_activate_cdata(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"activate">>, __TopXMLNS}});
      _res -> _res
    end.

encode_bytestreams_activate_cdata(undefined, _acc) ->
    _acc;
encode_bytestreams_activate_cdata(_val, _acc) ->
    [{xmlcdata, enc_jid(_val)} | _acc].

decode_bytestreams_streamhost_used(__TopXMLNS,
				   __IgnoreEls,
				   {xmlel, <<"streamhost-used">>, _attrs,
				    _els}) ->
    Jid =
	decode_bytestreams_streamhost_used_attrs(__TopXMLNS,
						 _attrs, undefined),
    Jid.

decode_bytestreams_streamhost_used_attrs(__TopXMLNS,
					 [{<<"jid">>, _val} | _attrs], _Jid) ->
    decode_bytestreams_streamhost_used_attrs(__TopXMLNS,
					     _attrs, _val);
decode_bytestreams_streamhost_used_attrs(__TopXMLNS,
					 [_ | _attrs], Jid) ->
    decode_bytestreams_streamhost_used_attrs(__TopXMLNS,
					     _attrs, Jid);
decode_bytestreams_streamhost_used_attrs(__TopXMLNS, [],
					 Jid) ->
    decode_bytestreams_streamhost_used_attr_jid(__TopXMLNS,
						Jid).

encode_bytestreams_streamhost_used(Jid, _xmlns_attrs) ->
    _els = [],
    _attrs =
	encode_bytestreams_streamhost_used_attr_jid(Jid,
						    _xmlns_attrs),
    {xmlel, <<"streamhost-used">>, _attrs, _els}.

decode_bytestreams_streamhost_used_attr_jid(__TopXMLNS,
					    undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"streamhost-used">>,
		   __TopXMLNS}});
decode_bytestreams_streamhost_used_attr_jid(__TopXMLNS,
					    _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"streamhost-used">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_bytestreams_streamhost_used_attr_jid(_val,
					    _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_bytestreams_streamhost(__TopXMLNS, __IgnoreEls,
			      {xmlel, <<"streamhost">>, _attrs, _els}) ->
    {Jid, Host, Port} =
	decode_bytestreams_streamhost_attrs(__TopXMLNS, _attrs,
					    undefined, undefined, undefined),
    {streamhost, Jid, Host, Port}.

decode_bytestreams_streamhost_attrs(__TopXMLNS,
				    [{<<"jid">>, _val} | _attrs], _Jid, Host,
				    Port) ->
    decode_bytestreams_streamhost_attrs(__TopXMLNS, _attrs,
					_val, Host, Port);
decode_bytestreams_streamhost_attrs(__TopXMLNS,
				    [{<<"host">>, _val} | _attrs], Jid, _Host,
				    Port) ->
    decode_bytestreams_streamhost_attrs(__TopXMLNS, _attrs,
					Jid, _val, Port);
decode_bytestreams_streamhost_attrs(__TopXMLNS,
				    [{<<"port">>, _val} | _attrs], Jid, Host,
				    _Port) ->
    decode_bytestreams_streamhost_attrs(__TopXMLNS, _attrs,
					Jid, Host, _val);
decode_bytestreams_streamhost_attrs(__TopXMLNS,
				    [_ | _attrs], Jid, Host, Port) ->
    decode_bytestreams_streamhost_attrs(__TopXMLNS, _attrs,
					Jid, Host, Port);
decode_bytestreams_streamhost_attrs(__TopXMLNS, [], Jid,
				    Host, Port) ->
    {decode_bytestreams_streamhost_attr_jid(__TopXMLNS,
					    Jid),
     decode_bytestreams_streamhost_attr_host(__TopXMLNS,
					     Host),
     decode_bytestreams_streamhost_attr_port(__TopXMLNS,
					     Port)}.

encode_bytestreams_streamhost({streamhost, Jid, Host,
			       Port},
			      _xmlns_attrs) ->
    _els = [],
    _attrs = encode_bytestreams_streamhost_attr_port(Port,
						     encode_bytestreams_streamhost_attr_host(Host,
											     encode_bytestreams_streamhost_attr_jid(Jid,
																    _xmlns_attrs))),
    {xmlel, <<"streamhost">>, _attrs, _els}.

decode_bytestreams_streamhost_attr_jid(__TopXMLNS,
				       undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"streamhost">>,
		   __TopXMLNS}});
decode_bytestreams_streamhost_attr_jid(__TopXMLNS,
				       _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"streamhost">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_bytestreams_streamhost_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_bytestreams_streamhost_attr_host(__TopXMLNS,
					undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"host">>, <<"streamhost">>,
		   __TopXMLNS}});
decode_bytestreams_streamhost_attr_host(__TopXMLNS,
					_val) ->
    _val.

encode_bytestreams_streamhost_attr_host(_val, _acc) ->
    [{<<"host">>, _val} | _acc].

decode_bytestreams_streamhost_attr_port(__TopXMLNS,
					undefined) ->
    1080;
decode_bytestreams_streamhost_attr_port(__TopXMLNS,
					_val) ->
    case catch dec_int(_val, 0, 65535) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"port">>, <<"streamhost">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_bytestreams_streamhost_attr_port(1080, _acc) ->
    _acc;
encode_bytestreams_streamhost_attr_port(_val, _acc) ->
    [{<<"port">>, enc_int(_val)} | _acc].

decode_delay(__TopXMLNS, __IgnoreEls,
	     {xmlel, <<"delay">>, _attrs, _els}) ->
    {Stamp, From} = decode_delay_attrs(__TopXMLNS, _attrs,
				       undefined, undefined),
    {delay, Stamp, From}.

decode_delay_attrs(__TopXMLNS,
		   [{<<"stamp">>, _val} | _attrs], _Stamp, From) ->
    decode_delay_attrs(__TopXMLNS, _attrs, _val, From);
decode_delay_attrs(__TopXMLNS,
		   [{<<"from">>, _val} | _attrs], Stamp, _From) ->
    decode_delay_attrs(__TopXMLNS, _attrs, Stamp, _val);
decode_delay_attrs(__TopXMLNS, [_ | _attrs], Stamp,
		   From) ->
    decode_delay_attrs(__TopXMLNS, _attrs, Stamp, From);
decode_delay_attrs(__TopXMLNS, [], Stamp, From) ->
    {decode_delay_attr_stamp(__TopXMLNS, Stamp),
     decode_delay_attr_from(__TopXMLNS, From)}.

encode_delay({delay, Stamp, From}, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_delay_attr_from(From,
				    encode_delay_attr_stamp(Stamp,
							    _xmlns_attrs)),
    {xmlel, <<"delay">>, _attrs, _els}.

decode_delay_attr_stamp(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"stamp">>, <<"delay">>, __TopXMLNS}});
decode_delay_attr_stamp(__TopXMLNS, _val) ->
    case catch dec_utc(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"stamp">>, <<"delay">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_delay_attr_stamp(_val, _acc) ->
    [{<<"stamp">>, enc_utc(_val)} | _acc].

decode_delay_attr_from(__TopXMLNS, undefined) ->
    undefined;
decode_delay_attr_from(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"from">>, <<"delay">>, __TopXMLNS}});
      _res -> _res
    end.

encode_delay_attr_from(undefined, _acc) -> _acc;
encode_delay_attr_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_chatstate_paused(__TopXMLNS, __IgnoreEls,
			{xmlel, <<"paused">>, _attrs, _els}) ->
    {chatstate, paused}.

encode_chatstate_paused({chatstate, paused},
			_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"paused">>, _attrs, _els}.

decode_chatstate_inactive(__TopXMLNS, __IgnoreEls,
			  {xmlel, <<"inactive">>, _attrs, _els}) ->
    {chatstate, inactive}.

encode_chatstate_inactive({chatstate, inactive},
			  _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"inactive">>, _attrs, _els}.

decode_chatstate_gone(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"gone">>, _attrs, _els}) ->
    {chatstate, gone}.

encode_chatstate_gone({chatstate, gone},
		      _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"gone">>, _attrs, _els}.

decode_chatstate_composing(__TopXMLNS, __IgnoreEls,
			   {xmlel, <<"composing">>, _attrs, _els}) ->
    {chatstate, composing}.

encode_chatstate_composing({chatstate, composing},
			   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"composing">>, _attrs, _els}.

decode_chatstate_active(__TopXMLNS, __IgnoreEls,
			{xmlel, <<"active">>, _attrs, _els}) ->
    {chatstate, active}.

encode_chatstate_active({chatstate, active},
			_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"active">>, _attrs, _els}.

decode_shim_headers(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"headers">>, _attrs, _els}) ->
    Headers = decode_shim_headers_els(__TopXMLNS,
				      __IgnoreEls, _els, []),
    {shim, Headers}.

decode_shim_headers_els(__TopXMLNS, __IgnoreEls, [],
			Headers) ->
    lists:reverse(Headers);
decode_shim_headers_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"header">>, _attrs, _} = _el | _els],
			Headers) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_shim_headers_els(__TopXMLNS, __IgnoreEls, _els,
				   [decode_shim_header(__TopXMLNS, __IgnoreEls,
						       _el)
				    | Headers]);
       true ->
	   decode_shim_headers_els(__TopXMLNS, __IgnoreEls, _els,
				   Headers)
    end;
decode_shim_headers_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Headers) ->
    decode_shim_headers_els(__TopXMLNS, __IgnoreEls, _els,
			    Headers).

encode_shim_headers({shim, Headers}, _xmlns_attrs) ->
    _els =
	lists:reverse('encode_shim_headers_$headers'(Headers,
						     [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"headers">>, _attrs, _els}.

'encode_shim_headers_$headers'([], _acc) -> _acc;
'encode_shim_headers_$headers'([Headers | _els],
			       _acc) ->
    'encode_shim_headers_$headers'(_els,
				   [encode_shim_header(Headers, []) | _acc]).

decode_shim_header(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"header">>, _attrs, _els}) ->
    Cdata = decode_shim_header_els(__TopXMLNS, __IgnoreEls,
				   _els, <<>>),
    Name = decode_shim_header_attrs(__TopXMLNS, _attrs,
				    undefined),
    {Name, Cdata}.

decode_shim_header_els(__TopXMLNS, __IgnoreEls, [],
		       Cdata) ->
    decode_shim_header_cdata(__TopXMLNS, Cdata);
decode_shim_header_els(__TopXMLNS, __IgnoreEls,
		       [{xmlcdata, _data} | _els], Cdata) ->
    decode_shim_header_els(__TopXMLNS, __IgnoreEls, _els,
			   <<Cdata/binary, _data/binary>>);
decode_shim_header_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Cdata) ->
    decode_shim_header_els(__TopXMLNS, __IgnoreEls, _els,
			   Cdata).

decode_shim_header_attrs(__TopXMLNS,
			 [{<<"name">>, _val} | _attrs], _Name) ->
    decode_shim_header_attrs(__TopXMLNS, _attrs, _val);
decode_shim_header_attrs(__TopXMLNS, [_ | _attrs],
			 Name) ->
    decode_shim_header_attrs(__TopXMLNS, _attrs, Name);
decode_shim_header_attrs(__TopXMLNS, [], Name) ->
    decode_shim_header_attr_name(__TopXMLNS, Name).

encode_shim_header({Name, Cdata}, _xmlns_attrs) ->
    _els = encode_shim_header_cdata(Cdata, []),
    _attrs = encode_shim_header_attr_name(Name,
					  _xmlns_attrs),
    {xmlel, <<"header">>, _attrs, _els}.

decode_shim_header_attr_name(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"name">>, <<"header">>, __TopXMLNS}});
decode_shim_header_attr_name(__TopXMLNS, _val) -> _val.

encode_shim_header_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_shim_header_cdata(__TopXMLNS, <<>>) -> undefined;
decode_shim_header_cdata(__TopXMLNS, _val) -> _val.

encode_shim_header_cdata(undefined, _acc) -> _acc;
encode_shim_header_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_pubsub(__TopXMLNS, __IgnoreEls,
	      {xmlel, <<"pubsub">>, _attrs, _els}) ->
    {Items, Options, Affiliations, Subscriptions, Retract,
     Unsubscribe, Subscribe, Publish} =
	decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els,
			  undefined, undefined, undefined, undefined, undefined,
			  undefined, undefined, undefined),
    {pubsub, Subscriptions, Affiliations, Publish,
     Subscribe, Unsubscribe, Options, Items, Retract}.

decode_pubsub_els(__TopXMLNS, __IgnoreEls, [], Items,
		  Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    {Items, Options, Affiliations, Subscriptions, Retract,
     Unsubscribe, Subscribe, Publish};
decode_pubsub_els(__TopXMLNS, __IgnoreEls,
		  [{xmlel, <<"subscriptions">>, _attrs, _} = _el | _els],
		  Items, Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
			     Options, Affiliations,
			     decode_pubsub_subscriptions(__TopXMLNS,
							 __IgnoreEls, _el),
			     Retract, Unsubscribe, Subscribe, Publish);
       true ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
			     Options, Affiliations, Subscriptions, Retract,
			     Unsubscribe, Subscribe, Publish)
    end;
decode_pubsub_els(__TopXMLNS, __IgnoreEls,
		  [{xmlel, <<"affiliations">>, _attrs, _} = _el | _els],
		  Items, Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
			     Options,
			     decode_pubsub_affiliations(__TopXMLNS, __IgnoreEls,
							_el),
			     Subscriptions, Retract, Unsubscribe, Subscribe,
			     Publish);
       true ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
			     Options, Affiliations, Subscriptions, Retract,
			     Unsubscribe, Subscribe, Publish)
    end;
decode_pubsub_els(__TopXMLNS, __IgnoreEls,
		  [{xmlel, <<"subscribe">>, _attrs, _} = _el | _els],
		  Items, Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
			     Options, Affiliations, Subscriptions, Retract,
			     Unsubscribe,
			     decode_pubsub_subscribe(__TopXMLNS, __IgnoreEls,
						     _el),
			     Publish);
       true ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
			     Options, Affiliations, Subscriptions, Retract,
			     Unsubscribe, Subscribe, Publish)
    end;
decode_pubsub_els(__TopXMLNS, __IgnoreEls,
		  [{xmlel, <<"unsubscribe">>, _attrs, _} = _el | _els],
		  Items, Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
			     Options, Affiliations, Subscriptions, Retract,
			     decode_pubsub_unsubscribe(__TopXMLNS, __IgnoreEls,
						       _el),
			     Subscribe, Publish);
       true ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
			     Options, Affiliations, Subscriptions, Retract,
			     Unsubscribe, Subscribe, Publish)
    end;
decode_pubsub_els(__TopXMLNS, __IgnoreEls,
		  [{xmlel, <<"options">>, _attrs, _} = _el | _els], Items,
		  Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
			     decode_pubsub_options(__TopXMLNS, __IgnoreEls,
						   _el),
			     Affiliations, Subscriptions, Retract, Unsubscribe,
			     Subscribe, Publish);
       true ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
			     Options, Affiliations, Subscriptions, Retract,
			     Unsubscribe, Subscribe, Publish)
    end;
decode_pubsub_els(__TopXMLNS, __IgnoreEls,
		  [{xmlel, <<"items">>, _attrs, _} = _el | _els], Items,
		  Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els,
			     decode_pubsub_items(__TopXMLNS, __IgnoreEls, _el),
			     Options, Affiliations, Subscriptions, Retract,
			     Unsubscribe, Subscribe, Publish);
       true ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
			     Options, Affiliations, Subscriptions, Retract,
			     Unsubscribe, Subscribe, Publish)
    end;
decode_pubsub_els(__TopXMLNS, __IgnoreEls,
		  [{xmlel, <<"retract">>, _attrs, _} = _el | _els], Items,
		  Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
			     Options, Affiliations, Subscriptions,
			     decode_pubsub_retract(__TopXMLNS, __IgnoreEls,
						   _el),
			     Unsubscribe, Subscribe, Publish);
       true ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
			     Options, Affiliations, Subscriptions, Retract,
			     Unsubscribe, Subscribe, Publish)
    end;
decode_pubsub_els(__TopXMLNS, __IgnoreEls,
		  [{xmlel, <<"publish">>, _attrs, _} = _el | _els], Items,
		  Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
			     Options, Affiliations, Subscriptions, Retract,
			     Unsubscribe, Subscribe,
			     decode_pubsub_publish(__TopXMLNS, __IgnoreEls,
						   _el));
       true ->
	   decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
			     Options, Affiliations, Subscriptions, Retract,
			     Unsubscribe, Subscribe, Publish)
    end;
decode_pubsub_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		  Items, Options, Affiliations, Subscriptions, Retract,
		  Unsubscribe, Subscribe, Publish) ->
    decode_pubsub_els(__TopXMLNS, __IgnoreEls, _els, Items,
		      Options, Affiliations, Subscriptions, Retract,
		      Unsubscribe, Subscribe, Publish).

encode_pubsub({pubsub, Subscriptions, Affiliations,
	       Publish, Subscribe, Unsubscribe, Options, Items,
	       Retract},
	      _xmlns_attrs) ->
    _els = lists:reverse('encode_pubsub_$items'(Items,
						'encode_pubsub_$options'(Options,
									 'encode_pubsub_$affiliations'(Affiliations,
												       'encode_pubsub_$subscriptions'(Subscriptions,
																      'encode_pubsub_$retract'(Retract,
																			       'encode_pubsub_$unsubscribe'(Unsubscribe,
																							    'encode_pubsub_$subscribe'(Subscribe,
																										       'encode_pubsub_$publish'(Publish,
																														[]))))))))),
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

decode_pubsub_retract(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"retract">>, _attrs, _els}) ->
    Items = decode_pubsub_retract_els(__TopXMLNS,
				      __IgnoreEls, _els, []),
    {Node, Notify} = decode_pubsub_retract_attrs(__TopXMLNS,
						 _attrs, undefined, undefined),
    {pubsub_retract, Node, Notify, Items}.

decode_pubsub_retract_els(__TopXMLNS, __IgnoreEls, [],
			  Items) ->
    lists:reverse(Items);
decode_pubsub_retract_els(__TopXMLNS, __IgnoreEls,
			  [{xmlel, <<"item">>, _attrs, _} = _el | _els],
			  Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_retract_els(__TopXMLNS, __IgnoreEls, _els,
				     [decode_pubsub_item(__TopXMLNS,
							 __IgnoreEls, _el)
				      | Items]);
       true ->
	   decode_pubsub_retract_els(__TopXMLNS, __IgnoreEls, _els,
				     Items)
    end;
decode_pubsub_retract_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Items) ->
    decode_pubsub_retract_els(__TopXMLNS, __IgnoreEls, _els,
			      Items).

decode_pubsub_retract_attrs(__TopXMLNS,
			    [{<<"node">>, _val} | _attrs], _Node, Notify) ->
    decode_pubsub_retract_attrs(__TopXMLNS, _attrs, _val,
				Notify);
decode_pubsub_retract_attrs(__TopXMLNS,
			    [{<<"notify">>, _val} | _attrs], Node, _Notify) ->
    decode_pubsub_retract_attrs(__TopXMLNS, _attrs, Node,
				_val);
decode_pubsub_retract_attrs(__TopXMLNS, [_ | _attrs],
			    Node, Notify) ->
    decode_pubsub_retract_attrs(__TopXMLNS, _attrs, Node,
				Notify);
decode_pubsub_retract_attrs(__TopXMLNS, [], Node,
			    Notify) ->
    {decode_pubsub_retract_attr_node(__TopXMLNS, Node),
     decode_pubsub_retract_attr_notify(__TopXMLNS, Notify)}.

encode_pubsub_retract({pubsub_retract, Node, Notify,
		       Items},
		      _xmlns_attrs) ->
    _els =
	lists:reverse('encode_pubsub_retract_$items'(Items,
						     [])),
    _attrs = encode_pubsub_retract_attr_notify(Notify,
					       encode_pubsub_retract_attr_node(Node,
									       _xmlns_attrs)),
    {xmlel, <<"retract">>, _attrs, _els}.

'encode_pubsub_retract_$items'([], _acc) -> _acc;
'encode_pubsub_retract_$items'([Items | _els], _acc) ->
    'encode_pubsub_retract_$items'(_els,
				   [encode_pubsub_item(Items, []) | _acc]).

decode_pubsub_retract_attr_node(__TopXMLNS,
				undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"node">>, <<"retract">>, __TopXMLNS}});
decode_pubsub_retract_attr_node(__TopXMLNS, _val) ->
    _val.

encode_pubsub_retract_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_retract_attr_notify(__TopXMLNS,
				  undefined) ->
    false;
decode_pubsub_retract_attr_notify(__TopXMLNS, _val) ->
    case catch dec_bool(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"notify">>, <<"retract">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_pubsub_retract_attr_notify(false, _acc) -> _acc;
encode_pubsub_retract_attr_notify(_val, _acc) ->
    [{<<"notify">>, enc_bool(_val)} | _acc].

decode_pubsub_options(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"options">>, _attrs, _els}) ->
    Xdata = decode_pubsub_options_els(__TopXMLNS,
				      __IgnoreEls, _els, undefined),
    {Node, Subid, Jid} =
	decode_pubsub_options_attrs(__TopXMLNS, _attrs,
				    undefined, undefined, undefined),
    {pubsub_options, Node, Jid, Subid, Xdata}.

decode_pubsub_options_els(__TopXMLNS, __IgnoreEls, [],
			  Xdata) ->
    Xdata;
decode_pubsub_options_els(__TopXMLNS, __IgnoreEls,
			  [{xmlel, <<"x">>, _attrs, _} = _el | _els], Xdata) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<"jabber:x:data">> ->
	   decode_pubsub_options_els(__TopXMLNS, __IgnoreEls, _els,
				     decode_xdata(_xmlns, __IgnoreEls, _el));
       true ->
	   decode_pubsub_options_els(__TopXMLNS, __IgnoreEls, _els,
				     Xdata)
    end;
decode_pubsub_options_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Xdata) ->
    decode_pubsub_options_els(__TopXMLNS, __IgnoreEls, _els,
			      Xdata).

decode_pubsub_options_attrs(__TopXMLNS,
			    [{<<"node">>, _val} | _attrs], _Node, Subid, Jid) ->
    decode_pubsub_options_attrs(__TopXMLNS, _attrs, _val,
				Subid, Jid);
decode_pubsub_options_attrs(__TopXMLNS,
			    [{<<"subid">>, _val} | _attrs], Node, _Subid,
			    Jid) ->
    decode_pubsub_options_attrs(__TopXMLNS, _attrs, Node,
				_val, Jid);
decode_pubsub_options_attrs(__TopXMLNS,
			    [{<<"jid">>, _val} | _attrs], Node, Subid, _Jid) ->
    decode_pubsub_options_attrs(__TopXMLNS, _attrs, Node,
				Subid, _val);
decode_pubsub_options_attrs(__TopXMLNS, [_ | _attrs],
			    Node, Subid, Jid) ->
    decode_pubsub_options_attrs(__TopXMLNS, _attrs, Node,
				Subid, Jid);
decode_pubsub_options_attrs(__TopXMLNS, [], Node, Subid,
			    Jid) ->
    {decode_pubsub_options_attr_node(__TopXMLNS, Node),
     decode_pubsub_options_attr_subid(__TopXMLNS, Subid),
     decode_pubsub_options_attr_jid(__TopXMLNS, Jid)}.

encode_pubsub_options({pubsub_options, Node, Jid, Subid,
		       Xdata},
		      _xmlns_attrs) ->
    _els =
	lists:reverse('encode_pubsub_options_$xdata'(Xdata,
						     [])),
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

decode_pubsub_options_attr_node(__TopXMLNS,
				undefined) ->
    undefined;
decode_pubsub_options_attr_node(__TopXMLNS, _val) ->
    _val.

encode_pubsub_options_attr_node(undefined, _acc) ->
    _acc;
encode_pubsub_options_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_options_attr_subid(__TopXMLNS,
				 undefined) ->
    undefined;
decode_pubsub_options_attr_subid(__TopXMLNS, _val) ->
    _val.

encode_pubsub_options_attr_subid(undefined, _acc) ->
    _acc;
encode_pubsub_options_attr_subid(_val, _acc) ->
    [{<<"subid">>, _val} | _acc].

decode_pubsub_options_attr_jid(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"options">>, __TopXMLNS}});
decode_pubsub_options_attr_jid(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"options">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_pubsub_options_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_pubsub_publish(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"publish">>, _attrs, _els}) ->
    Items = decode_pubsub_publish_els(__TopXMLNS,
				      __IgnoreEls, _els, []),
    Node = decode_pubsub_publish_attrs(__TopXMLNS, _attrs,
				       undefined),
    {pubsub_publish, Node, Items}.

decode_pubsub_publish_els(__TopXMLNS, __IgnoreEls, [],
			  Items) ->
    lists:reverse(Items);
decode_pubsub_publish_els(__TopXMLNS, __IgnoreEls,
			  [{xmlel, <<"item">>, _attrs, _} = _el | _els],
			  Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_publish_els(__TopXMLNS, __IgnoreEls, _els,
				     [decode_pubsub_item(__TopXMLNS,
							 __IgnoreEls, _el)
				      | Items]);
       true ->
	   decode_pubsub_publish_els(__TopXMLNS, __IgnoreEls, _els,
				     Items)
    end;
decode_pubsub_publish_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Items) ->
    decode_pubsub_publish_els(__TopXMLNS, __IgnoreEls, _els,
			      Items).

decode_pubsub_publish_attrs(__TopXMLNS,
			    [{<<"node">>, _val} | _attrs], _Node) ->
    decode_pubsub_publish_attrs(__TopXMLNS, _attrs, _val);
decode_pubsub_publish_attrs(__TopXMLNS, [_ | _attrs],
			    Node) ->
    decode_pubsub_publish_attrs(__TopXMLNS, _attrs, Node);
decode_pubsub_publish_attrs(__TopXMLNS, [], Node) ->
    decode_pubsub_publish_attr_node(__TopXMLNS, Node).

encode_pubsub_publish({pubsub_publish, Node, Items},
		      _xmlns_attrs) ->
    _els =
	lists:reverse('encode_pubsub_publish_$items'(Items,
						     [])),
    _attrs = encode_pubsub_publish_attr_node(Node,
					     _xmlns_attrs),
    {xmlel, <<"publish">>, _attrs, _els}.

'encode_pubsub_publish_$items'([], _acc) -> _acc;
'encode_pubsub_publish_$items'([Items | _els], _acc) ->
    'encode_pubsub_publish_$items'(_els,
				   [encode_pubsub_item(Items, []) | _acc]).

decode_pubsub_publish_attr_node(__TopXMLNS,
				undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"node">>, <<"publish">>, __TopXMLNS}});
decode_pubsub_publish_attr_node(__TopXMLNS, _val) ->
    _val.

encode_pubsub_publish_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_unsubscribe(__TopXMLNS, __IgnoreEls,
			  {xmlel, <<"unsubscribe">>, _attrs, _els}) ->
    {Node, Subid, Jid} =
	decode_pubsub_unsubscribe_attrs(__TopXMLNS, _attrs,
					undefined, undefined, undefined),
    {pubsub_unsubscribe, Node, Jid, Subid}.

decode_pubsub_unsubscribe_attrs(__TopXMLNS,
				[{<<"node">>, _val} | _attrs], _Node, Subid,
				Jid) ->
    decode_pubsub_unsubscribe_attrs(__TopXMLNS, _attrs,
				    _val, Subid, Jid);
decode_pubsub_unsubscribe_attrs(__TopXMLNS,
				[{<<"subid">>, _val} | _attrs], Node, _Subid,
				Jid) ->
    decode_pubsub_unsubscribe_attrs(__TopXMLNS, _attrs,
				    Node, _val, Jid);
decode_pubsub_unsubscribe_attrs(__TopXMLNS,
				[{<<"jid">>, _val} | _attrs], Node, Subid,
				_Jid) ->
    decode_pubsub_unsubscribe_attrs(__TopXMLNS, _attrs,
				    Node, Subid, _val);
decode_pubsub_unsubscribe_attrs(__TopXMLNS,
				[_ | _attrs], Node, Subid, Jid) ->
    decode_pubsub_unsubscribe_attrs(__TopXMLNS, _attrs,
				    Node, Subid, Jid);
decode_pubsub_unsubscribe_attrs(__TopXMLNS, [], Node,
				Subid, Jid) ->
    {decode_pubsub_unsubscribe_attr_node(__TopXMLNS, Node),
     decode_pubsub_unsubscribe_attr_subid(__TopXMLNS, Subid),
     decode_pubsub_unsubscribe_attr_jid(__TopXMLNS, Jid)}.

encode_pubsub_unsubscribe({pubsub_unsubscribe, Node,
			   Jid, Subid},
			  _xmlns_attrs) ->
    _els = [],
    _attrs = encode_pubsub_unsubscribe_attr_jid(Jid,
						encode_pubsub_unsubscribe_attr_subid(Subid,
										     encode_pubsub_unsubscribe_attr_node(Node,
															 _xmlns_attrs))),
    {xmlel, <<"unsubscribe">>, _attrs, _els}.

decode_pubsub_unsubscribe_attr_node(__TopXMLNS,
				    undefined) ->
    undefined;
decode_pubsub_unsubscribe_attr_node(__TopXMLNS, _val) ->
    _val.

encode_pubsub_unsubscribe_attr_node(undefined, _acc) ->
    _acc;
encode_pubsub_unsubscribe_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_unsubscribe_attr_subid(__TopXMLNS,
				     undefined) ->
    undefined;
decode_pubsub_unsubscribe_attr_subid(__TopXMLNS,
				     _val) ->
    _val.

encode_pubsub_unsubscribe_attr_subid(undefined, _acc) ->
    _acc;
encode_pubsub_unsubscribe_attr_subid(_val, _acc) ->
    [{<<"subid">>, _val} | _acc].

decode_pubsub_unsubscribe_attr_jid(__TopXMLNS,
				   undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"unsubscribe">>,
		   __TopXMLNS}});
decode_pubsub_unsubscribe_attr_jid(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"unsubscribe">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_pubsub_unsubscribe_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_pubsub_subscribe(__TopXMLNS, __IgnoreEls,
			{xmlel, <<"subscribe">>, _attrs, _els}) ->
    {Node, Jid} = decode_pubsub_subscribe_attrs(__TopXMLNS,
						_attrs, undefined, undefined),
    {pubsub_subscribe, Node, Jid}.

decode_pubsub_subscribe_attrs(__TopXMLNS,
			      [{<<"node">>, _val} | _attrs], _Node, Jid) ->
    decode_pubsub_subscribe_attrs(__TopXMLNS, _attrs, _val,
				  Jid);
decode_pubsub_subscribe_attrs(__TopXMLNS,
			      [{<<"jid">>, _val} | _attrs], Node, _Jid) ->
    decode_pubsub_subscribe_attrs(__TopXMLNS, _attrs, Node,
				  _val);
decode_pubsub_subscribe_attrs(__TopXMLNS, [_ | _attrs],
			      Node, Jid) ->
    decode_pubsub_subscribe_attrs(__TopXMLNS, _attrs, Node,
				  Jid);
decode_pubsub_subscribe_attrs(__TopXMLNS, [], Node,
			      Jid) ->
    {decode_pubsub_subscribe_attr_node(__TopXMLNS, Node),
     decode_pubsub_subscribe_attr_jid(__TopXMLNS, Jid)}.

encode_pubsub_subscribe({pubsub_subscribe, Node, Jid},
			_xmlns_attrs) ->
    _els = [],
    _attrs = encode_pubsub_subscribe_attr_jid(Jid,
					      encode_pubsub_subscribe_attr_node(Node,
										_xmlns_attrs)),
    {xmlel, <<"subscribe">>, _attrs, _els}.

decode_pubsub_subscribe_attr_node(__TopXMLNS,
				  undefined) ->
    undefined;
decode_pubsub_subscribe_attr_node(__TopXMLNS, _val) ->
    _val.

encode_pubsub_subscribe_attr_node(undefined, _acc) ->
    _acc;
encode_pubsub_subscribe_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_subscribe_attr_jid(__TopXMLNS,
				 undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"subscribe">>,
		   __TopXMLNS}});
decode_pubsub_subscribe_attr_jid(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"subscribe">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_pubsub_subscribe_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_pubsub_affiliations(__TopXMLNS, __IgnoreEls,
			   {xmlel, <<"affiliations">>, _attrs, _els}) ->
    Affiliations =
	decode_pubsub_affiliations_els(__TopXMLNS, __IgnoreEls,
				       _els, []),
    Affiliations.

decode_pubsub_affiliations_els(__TopXMLNS, __IgnoreEls,
			       [], Affiliations) ->
    lists:reverse(Affiliations);
decode_pubsub_affiliations_els(__TopXMLNS, __IgnoreEls,
			       [{xmlel, <<"affiliation">>, _attrs, _} = _el
				| _els],
			       Affiliations) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_affiliations_els(__TopXMLNS, __IgnoreEls,
					  _els,
					  [decode_pubsub_affiliation(__TopXMLNS,
								     __IgnoreEls,
								     _el)
					   | Affiliations]);
       true ->
	   decode_pubsub_affiliations_els(__TopXMLNS, __IgnoreEls,
					  _els, Affiliations)
    end;
decode_pubsub_affiliations_els(__TopXMLNS, __IgnoreEls,
			       [_ | _els], Affiliations) ->
    decode_pubsub_affiliations_els(__TopXMLNS, __IgnoreEls,
				   _els, Affiliations).

encode_pubsub_affiliations(Affiliations,
			   _xmlns_attrs) ->
    _els =
	lists:reverse('encode_pubsub_affiliations_$affiliations'(Affiliations,
								 [])),
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

decode_pubsub_subscriptions(__TopXMLNS, __IgnoreEls,
			    {xmlel, <<"subscriptions">>, _attrs, _els}) ->
    Subscriptions =
	decode_pubsub_subscriptions_els(__TopXMLNS, __IgnoreEls,
					_els, []),
    Node = decode_pubsub_subscriptions_attrs(__TopXMLNS,
					     _attrs, undefined),
    {Node, Subscriptions}.

decode_pubsub_subscriptions_els(__TopXMLNS, __IgnoreEls,
				[], Subscriptions) ->
    lists:reverse(Subscriptions);
decode_pubsub_subscriptions_els(__TopXMLNS, __IgnoreEls,
				[{xmlel, <<"subscription">>, _attrs, _} = _el
				 | _els],
				Subscriptions) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_subscriptions_els(__TopXMLNS, __IgnoreEls,
					   _els,
					   [decode_pubsub_subscription(__TopXMLNS,
								       __IgnoreEls,
								       _el)
					    | Subscriptions]);
       true ->
	   decode_pubsub_subscriptions_els(__TopXMLNS, __IgnoreEls,
					   _els, Subscriptions)
    end;
decode_pubsub_subscriptions_els(__TopXMLNS, __IgnoreEls,
				[_ | _els], Subscriptions) ->
    decode_pubsub_subscriptions_els(__TopXMLNS, __IgnoreEls,
				    _els, Subscriptions).

decode_pubsub_subscriptions_attrs(__TopXMLNS,
				  [{<<"node">>, _val} | _attrs], _Node) ->
    decode_pubsub_subscriptions_attrs(__TopXMLNS, _attrs,
				      _val);
decode_pubsub_subscriptions_attrs(__TopXMLNS,
				  [_ | _attrs], Node) ->
    decode_pubsub_subscriptions_attrs(__TopXMLNS, _attrs,
				      Node);
decode_pubsub_subscriptions_attrs(__TopXMLNS, [],
				  Node) ->
    decode_pubsub_subscriptions_attr_node(__TopXMLNS, Node).

encode_pubsub_subscriptions({Node, Subscriptions},
			    _xmlns_attrs) ->
    _els =
	lists:reverse('encode_pubsub_subscriptions_$subscriptions'(Subscriptions,
								   [])),
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

decode_pubsub_subscriptions_attr_node(__TopXMLNS,
				      undefined) ->
    none;
decode_pubsub_subscriptions_attr_node(__TopXMLNS,
				      _val) ->
    _val.

encode_pubsub_subscriptions_attr_node(none, _acc) ->
    _acc;
encode_pubsub_subscriptions_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_event(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"event">>, _attrs, _els}) ->
    Items = decode_pubsub_event_els(__TopXMLNS, __IgnoreEls,
				    _els, []),
    {pubsub_event, Items}.

decode_pubsub_event_els(__TopXMLNS, __IgnoreEls, [],
			Items) ->
    lists:reverse(Items);
decode_pubsub_event_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"items">>, _attrs, _} = _el | _els],
			Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_event_els(__TopXMLNS, __IgnoreEls, _els,
				   [decode_pubsub_event_items(__TopXMLNS,
							      __IgnoreEls, _el)
				    | Items]);
       true ->
	   decode_pubsub_event_els(__TopXMLNS, __IgnoreEls, _els,
				   Items)
    end;
decode_pubsub_event_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Items) ->
    decode_pubsub_event_els(__TopXMLNS, __IgnoreEls, _els,
			    Items).

encode_pubsub_event({pubsub_event, Items},
		    _xmlns_attrs) ->
    _els = lists:reverse('encode_pubsub_event_$items'(Items,
						      [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"event">>, _attrs, _els}.

'encode_pubsub_event_$items'([], _acc) -> _acc;
'encode_pubsub_event_$items'([Items | _els], _acc) ->
    'encode_pubsub_event_$items'(_els,
				 [encode_pubsub_event_items(Items, []) | _acc]).

decode_pubsub_event_items(__TopXMLNS, __IgnoreEls,
			  {xmlel, <<"items">>, _attrs, _els}) ->
    {Items, Retract} =
	decode_pubsub_event_items_els(__TopXMLNS, __IgnoreEls,
				      _els, [], []),
    Node = decode_pubsub_event_items_attrs(__TopXMLNS,
					   _attrs, undefined),
    {pubsub_event_items, Node, Retract, Items}.

decode_pubsub_event_items_els(__TopXMLNS, __IgnoreEls,
			      [], Items, Retract) ->
    {lists:reverse(Items), lists:reverse(Retract)};
decode_pubsub_event_items_els(__TopXMLNS, __IgnoreEls,
			      [{xmlel, <<"retract">>, _attrs, _} = _el | _els],
			      Items, Retract) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_event_items_els(__TopXMLNS, __IgnoreEls,
					 _els, Items,
					 [decode_pubsub_event_retract(__TopXMLNS,
								      __IgnoreEls,
								      _el)
					  | Retract]);
       true ->
	   decode_pubsub_event_items_els(__TopXMLNS, __IgnoreEls,
					 _els, Items, Retract)
    end;
decode_pubsub_event_items_els(__TopXMLNS, __IgnoreEls,
			      [{xmlel, <<"item">>, _attrs, _} = _el | _els],
			      Items, Retract) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_event_items_els(__TopXMLNS, __IgnoreEls,
					 _els,
					 [decode_pubsub_event_item(__TopXMLNS,
								   __IgnoreEls,
								   _el)
					  | Items],
					 Retract);
       true ->
	   decode_pubsub_event_items_els(__TopXMLNS, __IgnoreEls,
					 _els, Items, Retract)
    end;
decode_pubsub_event_items_els(__TopXMLNS, __IgnoreEls,
			      [_ | _els], Items, Retract) ->
    decode_pubsub_event_items_els(__TopXMLNS, __IgnoreEls,
				  _els, Items, Retract).

decode_pubsub_event_items_attrs(__TopXMLNS,
				[{<<"node">>, _val} | _attrs], _Node) ->
    decode_pubsub_event_items_attrs(__TopXMLNS, _attrs,
				    _val);
decode_pubsub_event_items_attrs(__TopXMLNS,
				[_ | _attrs], Node) ->
    decode_pubsub_event_items_attrs(__TopXMLNS, _attrs,
				    Node);
decode_pubsub_event_items_attrs(__TopXMLNS, [], Node) ->
    decode_pubsub_event_items_attr_node(__TopXMLNS, Node).

encode_pubsub_event_items({pubsub_event_items, Node,
			   Retract, Items},
			  _xmlns_attrs) ->
    _els =
	lists:reverse('encode_pubsub_event_items_$items'(Items,
							 'encode_pubsub_event_items_$retract'(Retract,
											      []))),
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

decode_pubsub_event_items_attr_node(__TopXMLNS,
				    undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"node">>, <<"items">>, __TopXMLNS}});
decode_pubsub_event_items_attr_node(__TopXMLNS, _val) ->
    _val.

encode_pubsub_event_items_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_event_item(__TopXMLNS, __IgnoreEls,
			 {xmlel, <<"item">>, _attrs, _els}) ->
    {Id, Node, Publisher} =
	decode_pubsub_event_item_attrs(__TopXMLNS, _attrs,
				       undefined, undefined, undefined),
    {pubsub_event_item, Id, Node, Publisher}.

decode_pubsub_event_item_attrs(__TopXMLNS,
			       [{<<"id">>, _val} | _attrs], _Id, Node,
			       Publisher) ->
    decode_pubsub_event_item_attrs(__TopXMLNS, _attrs, _val,
				   Node, Publisher);
decode_pubsub_event_item_attrs(__TopXMLNS,
			       [{<<"node">>, _val} | _attrs], Id, _Node,
			       Publisher) ->
    decode_pubsub_event_item_attrs(__TopXMLNS, _attrs, Id,
				   _val, Publisher);
decode_pubsub_event_item_attrs(__TopXMLNS,
			       [{<<"publisher">>, _val} | _attrs], Id, Node,
			       _Publisher) ->
    decode_pubsub_event_item_attrs(__TopXMLNS, _attrs, Id,
				   Node, _val);
decode_pubsub_event_item_attrs(__TopXMLNS, [_ | _attrs],
			       Id, Node, Publisher) ->
    decode_pubsub_event_item_attrs(__TopXMLNS, _attrs, Id,
				   Node, Publisher);
decode_pubsub_event_item_attrs(__TopXMLNS, [], Id, Node,
			       Publisher) ->
    {decode_pubsub_event_item_attr_id(__TopXMLNS, Id),
     decode_pubsub_event_item_attr_node(__TopXMLNS, Node),
     decode_pubsub_event_item_attr_publisher(__TopXMLNS,
					     Publisher)}.

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

decode_pubsub_event_item_attr_id(__TopXMLNS,
				 undefined) ->
    undefined;
decode_pubsub_event_item_attr_id(__TopXMLNS, _val) ->
    _val.

encode_pubsub_event_item_attr_id(undefined, _acc) ->
    _acc;
encode_pubsub_event_item_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_pubsub_event_item_attr_node(__TopXMLNS,
				   undefined) ->
    undefined;
decode_pubsub_event_item_attr_node(__TopXMLNS, _val) ->
    _val.

encode_pubsub_event_item_attr_node(undefined, _acc) ->
    _acc;
encode_pubsub_event_item_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_event_item_attr_publisher(__TopXMLNS,
					undefined) ->
    undefined;
decode_pubsub_event_item_attr_publisher(__TopXMLNS,
					_val) ->
    _val.

encode_pubsub_event_item_attr_publisher(undefined,
					_acc) ->
    _acc;
encode_pubsub_event_item_attr_publisher(_val, _acc) ->
    [{<<"publisher">>, _val} | _acc].

decode_pubsub_event_retract(__TopXMLNS, __IgnoreEls,
			    {xmlel, <<"retract">>, _attrs, _els}) ->
    Id = decode_pubsub_event_retract_attrs(__TopXMLNS,
					   _attrs, undefined),
    Id.

decode_pubsub_event_retract_attrs(__TopXMLNS,
				  [{<<"id">>, _val} | _attrs], _Id) ->
    decode_pubsub_event_retract_attrs(__TopXMLNS, _attrs,
				      _val);
decode_pubsub_event_retract_attrs(__TopXMLNS,
				  [_ | _attrs], Id) ->
    decode_pubsub_event_retract_attrs(__TopXMLNS, _attrs,
				      Id);
decode_pubsub_event_retract_attrs(__TopXMLNS, [], Id) ->
    decode_pubsub_event_retract_attr_id(__TopXMLNS, Id).

encode_pubsub_event_retract(Id, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_pubsub_event_retract_attr_id(Id,
						 _xmlns_attrs),
    {xmlel, <<"retract">>, _attrs, _els}.

decode_pubsub_event_retract_attr_id(__TopXMLNS,
				    undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"id">>, <<"retract">>, __TopXMLNS}});
decode_pubsub_event_retract_attr_id(__TopXMLNS, _val) ->
    _val.

encode_pubsub_event_retract_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_pubsub_items(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"items">>, _attrs, _els}) ->
    Items = decode_pubsub_items_els(__TopXMLNS, __IgnoreEls,
				    _els, []),
    {Max_items, Node, Subid} =
	decode_pubsub_items_attrs(__TopXMLNS, _attrs, undefined,
				  undefined, undefined),
    {pubsub_items, Node, Max_items, Subid, Items}.

decode_pubsub_items_els(__TopXMLNS, __IgnoreEls, [],
			Items) ->
    lists:reverse(Items);
decode_pubsub_items_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"item">>, _attrs, _} = _el | _els], Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_pubsub_items_els(__TopXMLNS, __IgnoreEls, _els,
				   [decode_pubsub_item(__TopXMLNS, __IgnoreEls,
						       _el)
				    | Items]);
       true ->
	   decode_pubsub_items_els(__TopXMLNS, __IgnoreEls, _els,
				   Items)
    end;
decode_pubsub_items_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Items) ->
    decode_pubsub_items_els(__TopXMLNS, __IgnoreEls, _els,
			    Items).

decode_pubsub_items_attrs(__TopXMLNS,
			  [{<<"max_items">>, _val} | _attrs], _Max_items, Node,
			  Subid) ->
    decode_pubsub_items_attrs(__TopXMLNS, _attrs, _val,
			      Node, Subid);
decode_pubsub_items_attrs(__TopXMLNS,
			  [{<<"node">>, _val} | _attrs], Max_items, _Node,
			  Subid) ->
    decode_pubsub_items_attrs(__TopXMLNS, _attrs, Max_items,
			      _val, Subid);
decode_pubsub_items_attrs(__TopXMLNS,
			  [{<<"subid">>, _val} | _attrs], Max_items, Node,
			  _Subid) ->
    decode_pubsub_items_attrs(__TopXMLNS, _attrs, Max_items,
			      Node, _val);
decode_pubsub_items_attrs(__TopXMLNS, [_ | _attrs],
			  Max_items, Node, Subid) ->
    decode_pubsub_items_attrs(__TopXMLNS, _attrs, Max_items,
			      Node, Subid);
decode_pubsub_items_attrs(__TopXMLNS, [], Max_items,
			  Node, Subid) ->
    {decode_pubsub_items_attr_max_items(__TopXMLNS,
					Max_items),
     decode_pubsub_items_attr_node(__TopXMLNS, Node),
     decode_pubsub_items_attr_subid(__TopXMLNS, Subid)}.

encode_pubsub_items({pubsub_items, Node, Max_items,
		     Subid, Items},
		    _xmlns_attrs) ->
    _els = lists:reverse('encode_pubsub_items_$items'(Items,
						      [])),
    _attrs = encode_pubsub_items_attr_subid(Subid,
					    encode_pubsub_items_attr_node(Node,
									  encode_pubsub_items_attr_max_items(Max_items,
													     _xmlns_attrs))),
    {xmlel, <<"items">>, _attrs, _els}.

'encode_pubsub_items_$items'([], _acc) -> _acc;
'encode_pubsub_items_$items'([Items | _els], _acc) ->
    'encode_pubsub_items_$items'(_els,
				 [encode_pubsub_item(Items, []) | _acc]).

decode_pubsub_items_attr_max_items(__TopXMLNS,
				   undefined) ->
    undefined;
decode_pubsub_items_attr_max_items(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"max_items">>, <<"items">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_pubsub_items_attr_max_items(undefined, _acc) ->
    _acc;
encode_pubsub_items_attr_max_items(_val, _acc) ->
    [{<<"max_items">>, enc_int(_val)} | _acc].

decode_pubsub_items_attr_node(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"node">>, <<"items">>, __TopXMLNS}});
decode_pubsub_items_attr_node(__TopXMLNS, _val) -> _val.

encode_pubsub_items_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_items_attr_subid(__TopXMLNS, undefined) ->
    undefined;
decode_pubsub_items_attr_subid(__TopXMLNS, _val) ->
    _val.

encode_pubsub_items_attr_subid(undefined, _acc) -> _acc;
encode_pubsub_items_attr_subid(_val, _acc) ->
    [{<<"subid">>, _val} | _acc].

decode_pubsub_item(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"item">>, _attrs, _els}) ->
    __Xmls = decode_pubsub_item_els(__TopXMLNS, __IgnoreEls,
				    _els, []),
    Id = decode_pubsub_item_attrs(__TopXMLNS, _attrs,
				  undefined),
    {pubsub_item, Id, __Xmls}.

decode_pubsub_item_els(__TopXMLNS, __IgnoreEls, [],
		       __Xmls) ->
    lists:reverse(__Xmls);
decode_pubsub_item_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, _, _, _} = _el | _els], __Xmls) ->
    decode_pubsub_item_els(__TopXMLNS, __IgnoreEls, _els,
			   [_el | __Xmls]);
decode_pubsub_item_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], __Xmls) ->
    decode_pubsub_item_els(__TopXMLNS, __IgnoreEls, _els,
			   __Xmls).

decode_pubsub_item_attrs(__TopXMLNS,
			 [{<<"id">>, _val} | _attrs], _Id) ->
    decode_pubsub_item_attrs(__TopXMLNS, _attrs, _val);
decode_pubsub_item_attrs(__TopXMLNS, [_ | _attrs],
			 Id) ->
    decode_pubsub_item_attrs(__TopXMLNS, _attrs, Id);
decode_pubsub_item_attrs(__TopXMLNS, [], Id) ->
    decode_pubsub_item_attr_id(__TopXMLNS, Id).

encode_pubsub_item({pubsub_item, Id, __Xmls},
		   _xmlns_attrs) ->
    _els = __Xmls,
    _attrs = encode_pubsub_item_attr_id(Id, _xmlns_attrs),
    {xmlel, <<"item">>, _attrs, _els}.

decode_pubsub_item_attr_id(__TopXMLNS, undefined) ->
    undefined;
decode_pubsub_item_attr_id(__TopXMLNS, _val) -> _val.

encode_pubsub_item_attr_id(undefined, _acc) -> _acc;
encode_pubsub_item_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_pubsub_affiliation(__TopXMLNS, __IgnoreEls,
			  {xmlel, <<"affiliation">>, _attrs, _els}) ->
    {Node, Type} =
	decode_pubsub_affiliation_attrs(__TopXMLNS, _attrs,
					undefined, undefined),
    {pubsub_affiliation, Node, Type}.

decode_pubsub_affiliation_attrs(__TopXMLNS,
				[{<<"node">>, _val} | _attrs], _Node, Type) ->
    decode_pubsub_affiliation_attrs(__TopXMLNS, _attrs,
				    _val, Type);
decode_pubsub_affiliation_attrs(__TopXMLNS,
				[{<<"affiliation">>, _val} | _attrs], Node,
				_Type) ->
    decode_pubsub_affiliation_attrs(__TopXMLNS, _attrs,
				    Node, _val);
decode_pubsub_affiliation_attrs(__TopXMLNS,
				[_ | _attrs], Node, Type) ->
    decode_pubsub_affiliation_attrs(__TopXMLNS, _attrs,
				    Node, Type);
decode_pubsub_affiliation_attrs(__TopXMLNS, [], Node,
				Type) ->
    {decode_pubsub_affiliation_attr_node(__TopXMLNS, Node),
     decode_pubsub_affiliation_attr_affiliation(__TopXMLNS,
						Type)}.

encode_pubsub_affiliation({pubsub_affiliation, Node,
			   Type},
			  _xmlns_attrs) ->
    _els = [],
    _attrs =
	encode_pubsub_affiliation_attr_affiliation(Type,
						   encode_pubsub_affiliation_attr_node(Node,
										       _xmlns_attrs)),
    {xmlel, <<"affiliation">>, _attrs, _els}.

decode_pubsub_affiliation_attr_node(__TopXMLNS,
				    undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"node">>, <<"affiliation">>,
		   __TopXMLNS}});
decode_pubsub_affiliation_attr_node(__TopXMLNS, _val) ->
    _val.

encode_pubsub_affiliation_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_affiliation_attr_affiliation(__TopXMLNS,
					   undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"affiliation">>, <<"affiliation">>,
		   __TopXMLNS}});
decode_pubsub_affiliation_attr_affiliation(__TopXMLNS,
					   _val) ->
    case catch dec_enum(_val,
			[member, none, outcast, owner, publisher,
			 'publish-only'])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"affiliation">>, <<"affiliation">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_pubsub_affiliation_attr_affiliation(_val,
					   _acc) ->
    [{<<"affiliation">>, enc_enum(_val)} | _acc].

decode_pubsub_subscription(__TopXMLNS, __IgnoreEls,
			   {xmlel, <<"subscription">>, _attrs, _els}) ->
    {Jid, Node, Subid, Type} =
	decode_pubsub_subscription_attrs(__TopXMLNS, _attrs,
					 undefined, undefined, undefined,
					 undefined),
    {pubsub_subscription, Jid, Node, Subid, Type}.

decode_pubsub_subscription_attrs(__TopXMLNS,
				 [{<<"jid">>, _val} | _attrs], _Jid, Node,
				 Subid, Type) ->
    decode_pubsub_subscription_attrs(__TopXMLNS, _attrs,
				     _val, Node, Subid, Type);
decode_pubsub_subscription_attrs(__TopXMLNS,
				 [{<<"node">>, _val} | _attrs], Jid, _Node,
				 Subid, Type) ->
    decode_pubsub_subscription_attrs(__TopXMLNS, _attrs,
				     Jid, _val, Subid, Type);
decode_pubsub_subscription_attrs(__TopXMLNS,
				 [{<<"subid">>, _val} | _attrs], Jid, Node,
				 _Subid, Type) ->
    decode_pubsub_subscription_attrs(__TopXMLNS, _attrs,
				     Jid, Node, _val, Type);
decode_pubsub_subscription_attrs(__TopXMLNS,
				 [{<<"subscription">>, _val} | _attrs], Jid,
				 Node, Subid, _Type) ->
    decode_pubsub_subscription_attrs(__TopXMLNS, _attrs,
				     Jid, Node, Subid, _val);
decode_pubsub_subscription_attrs(__TopXMLNS,
				 [_ | _attrs], Jid, Node, Subid, Type) ->
    decode_pubsub_subscription_attrs(__TopXMLNS, _attrs,
				     Jid, Node, Subid, Type);
decode_pubsub_subscription_attrs(__TopXMLNS, [], Jid,
				 Node, Subid, Type) ->
    {decode_pubsub_subscription_attr_jid(__TopXMLNS, Jid),
     decode_pubsub_subscription_attr_node(__TopXMLNS, Node),
     decode_pubsub_subscription_attr_subid(__TopXMLNS,
					   Subid),
     decode_pubsub_subscription_attr_subscription(__TopXMLNS,
						  Type)}.

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

decode_pubsub_subscription_attr_jid(__TopXMLNS,
				    undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"subscription">>,
		   __TopXMLNS}});
decode_pubsub_subscription_attr_jid(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"subscription">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_pubsub_subscription_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_pubsub_subscription_attr_node(__TopXMLNS,
				     undefined) ->
    undefined;
decode_pubsub_subscription_attr_node(__TopXMLNS,
				     _val) ->
    _val.

encode_pubsub_subscription_attr_node(undefined, _acc) ->
    _acc;
encode_pubsub_subscription_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_pubsub_subscription_attr_subid(__TopXMLNS,
				      undefined) ->
    undefined;
decode_pubsub_subscription_attr_subid(__TopXMLNS,
				      _val) ->
    _val.

encode_pubsub_subscription_attr_subid(undefined,
				      _acc) ->
    _acc;
encode_pubsub_subscription_attr_subid(_val, _acc) ->
    [{<<"subid">>, _val} | _acc].

decode_pubsub_subscription_attr_subscription(__TopXMLNS,
					     undefined) ->
    undefined;
decode_pubsub_subscription_attr_subscription(__TopXMLNS,
					     _val) ->
    case catch dec_enum(_val,
			[none, pending, subscribed, unconfigured])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"subscription">>, <<"subscription">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_pubsub_subscription_attr_subscription(undefined,
					     _acc) ->
    _acc;
encode_pubsub_subscription_attr_subscription(_val,
					     _acc) ->
    [{<<"subscription">>, enc_enum(_val)} | _acc].

decode_xdata(__TopXMLNS, __IgnoreEls,
	     {xmlel, <<"x">>, _attrs, _els}) ->
    {Fields, Items, Instructions, Reported, Title} =
	decode_xdata_els(__TopXMLNS, __IgnoreEls, _els, [], [],
			 [], undefined, undefined),
    Type = decode_xdata_attrs(__TopXMLNS, _attrs,
			      undefined),
    {xdata, Type, Instructions, Title, Reported, Items,
     Fields}.

decode_xdata_els(__TopXMLNS, __IgnoreEls, [], Fields,
		 Items, Instructions, Reported, Title) ->
    {lists:reverse(Fields), lists:reverse(Items),
     lists:reverse(Instructions), Reported, Title};
decode_xdata_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"instructions">>, _attrs, _} = _el | _els],
		 Fields, Items, Instructions, Reported, Title) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_xdata_els(__TopXMLNS, __IgnoreEls, _els, Fields,
			    Items,
			    case decode_xdata_instructions(__TopXMLNS,
							   __IgnoreEls, _el)
				of
			      undefined -> Instructions;
			      _new_el -> [_new_el | Instructions]
			    end,
			    Reported, Title);
       true ->
	   decode_xdata_els(__TopXMLNS, __IgnoreEls, _els, Fields,
			    Items, Instructions, Reported, Title)
    end;
decode_xdata_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"title">>, _attrs, _} = _el | _els], Fields,
		 Items, Instructions, Reported, Title) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_xdata_els(__TopXMLNS, __IgnoreEls, _els, Fields,
			    Items, Instructions, Reported,
			    decode_xdata_title(__TopXMLNS, __IgnoreEls, _el));
       true ->
	   decode_xdata_els(__TopXMLNS, __IgnoreEls, _els, Fields,
			    Items, Instructions, Reported, Title)
    end;
decode_xdata_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"reported">>, _attrs, _} = _el | _els],
		 Fields, Items, Instructions, Reported, Title) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_xdata_els(__TopXMLNS, __IgnoreEls, _els, Fields,
			    Items, Instructions,
			    decode_xdata_reported(__TopXMLNS, __IgnoreEls, _el),
			    Title);
       true ->
	   decode_xdata_els(__TopXMLNS, __IgnoreEls, _els, Fields,
			    Items, Instructions, Reported, Title)
    end;
decode_xdata_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"item">>, _attrs, _} = _el | _els], Fields,
		 Items, Instructions, Reported, Title) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_xdata_els(__TopXMLNS, __IgnoreEls, _els, Fields,
			    [decode_xdata_item(__TopXMLNS, __IgnoreEls, _el)
			     | Items],
			    Instructions, Reported, Title);
       true ->
	   decode_xdata_els(__TopXMLNS, __IgnoreEls, _els, Fields,
			    Items, Instructions, Reported, Title)
    end;
decode_xdata_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"field">>, _attrs, _} = _el | _els], Fields,
		 Items, Instructions, Reported, Title) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_xdata_els(__TopXMLNS, __IgnoreEls, _els,
			    [decode_xdata_field(__TopXMLNS, __IgnoreEls, _el)
			     | Fields],
			    Items, Instructions, Reported, Title);
       true ->
	   decode_xdata_els(__TopXMLNS, __IgnoreEls, _els, Fields,
			    Items, Instructions, Reported, Title)
    end;
decode_xdata_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		 Fields, Items, Instructions, Reported, Title) ->
    decode_xdata_els(__TopXMLNS, __IgnoreEls, _els, Fields,
		     Items, Instructions, Reported, Title).

decode_xdata_attrs(__TopXMLNS,
		   [{<<"type">>, _val} | _attrs], _Type) ->
    decode_xdata_attrs(__TopXMLNS, _attrs, _val);
decode_xdata_attrs(__TopXMLNS, [_ | _attrs], Type) ->
    decode_xdata_attrs(__TopXMLNS, _attrs, Type);
decode_xdata_attrs(__TopXMLNS, [], Type) ->
    decode_xdata_attr_type(__TopXMLNS, Type).

encode_xdata({xdata, Type, Instructions, Title,
	      Reported, Items, Fields},
	     _xmlns_attrs) ->
    _els = lists:reverse('encode_xdata_$fields'(Fields,
						'encode_xdata_$items'(Items,
								      'encode_xdata_$instructions'(Instructions,
												   'encode_xdata_$reported'(Reported,
															    'encode_xdata_$title'(Title,
																		  [])))))),
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

decode_xdata_attr_type(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"type">>, <<"x">>, __TopXMLNS}});
decode_xdata_attr_type(__TopXMLNS, _val) ->
    case catch dec_enum(_val,
			[cancel, form, result, submit])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"type">>, <<"x">>, __TopXMLNS}});
      _res -> _res
    end.

encode_xdata_attr_type(_val, _acc) ->
    [{<<"type">>, enc_enum(_val)} | _acc].

decode_xdata_item(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"item">>, _attrs, _els}) ->
    Fields = decode_xdata_item_els(__TopXMLNS, __IgnoreEls,
				   _els, []),
    Fields.

decode_xdata_item_els(__TopXMLNS, __IgnoreEls, [],
		      Fields) ->
    lists:reverse(Fields);
decode_xdata_item_els(__TopXMLNS, __IgnoreEls,
		      [{xmlel, <<"field">>, _attrs, _} = _el | _els],
		      Fields) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_xdata_item_els(__TopXMLNS, __IgnoreEls, _els,
				 [decode_xdata_field(__TopXMLNS, __IgnoreEls,
						     _el)
				  | Fields]);
       true ->
	   decode_xdata_item_els(__TopXMLNS, __IgnoreEls, _els,
				 Fields)
    end;
decode_xdata_item_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Fields) ->
    decode_xdata_item_els(__TopXMLNS, __IgnoreEls, _els,
			  Fields).

encode_xdata_item(Fields, _xmlns_attrs) ->
    _els = lists:reverse('encode_xdata_item_$fields'(Fields,
						     [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"item">>, _attrs, _els}.

'encode_xdata_item_$fields'([], _acc) -> _acc;
'encode_xdata_item_$fields'([Fields | _els], _acc) ->
    'encode_xdata_item_$fields'(_els,
				[encode_xdata_field(Fields, []) | _acc]).

decode_xdata_reported(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"reported">>, _attrs, _els}) ->
    Fields = decode_xdata_reported_els(__TopXMLNS,
				       __IgnoreEls, _els, []),
    Fields.

decode_xdata_reported_els(__TopXMLNS, __IgnoreEls, [],
			  Fields) ->
    lists:reverse(Fields);
decode_xdata_reported_els(__TopXMLNS, __IgnoreEls,
			  [{xmlel, <<"field">>, _attrs, _} = _el | _els],
			  Fields) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_xdata_reported_els(__TopXMLNS, __IgnoreEls, _els,
				     [decode_xdata_field(__TopXMLNS,
							 __IgnoreEls, _el)
				      | Fields]);
       true ->
	   decode_xdata_reported_els(__TopXMLNS, __IgnoreEls, _els,
				     Fields)
    end;
decode_xdata_reported_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Fields) ->
    decode_xdata_reported_els(__TopXMLNS, __IgnoreEls, _els,
			      Fields).

encode_xdata_reported(Fields, _xmlns_attrs) ->
    _els =
	lists:reverse('encode_xdata_reported_$fields'(Fields,
						      [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"reported">>, _attrs, _els}.

'encode_xdata_reported_$fields'([], _acc) -> _acc;
'encode_xdata_reported_$fields'([Fields | _els],
				_acc) ->
    'encode_xdata_reported_$fields'(_els,
				    [encode_xdata_field(Fields, []) | _acc]).

decode_xdata_title(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"title">>, _attrs, _els}) ->
    Cdata = decode_xdata_title_els(__TopXMLNS, __IgnoreEls,
				   _els, <<>>),
    Cdata.

decode_xdata_title_els(__TopXMLNS, __IgnoreEls, [],
		       Cdata) ->
    decode_xdata_title_cdata(__TopXMLNS, Cdata);
decode_xdata_title_els(__TopXMLNS, __IgnoreEls,
		       [{xmlcdata, _data} | _els], Cdata) ->
    decode_xdata_title_els(__TopXMLNS, __IgnoreEls, _els,
			   <<Cdata/binary, _data/binary>>);
decode_xdata_title_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Cdata) ->
    decode_xdata_title_els(__TopXMLNS, __IgnoreEls, _els,
			   Cdata).

encode_xdata_title(Cdata, _xmlns_attrs) ->
    _els = encode_xdata_title_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"title">>, _attrs, _els}.

decode_xdata_title_cdata(__TopXMLNS, <<>>) -> undefined;
decode_xdata_title_cdata(__TopXMLNS, _val) -> _val.

encode_xdata_title_cdata(undefined, _acc) -> _acc;
encode_xdata_title_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_xdata_instructions(__TopXMLNS, __IgnoreEls,
			  {xmlel, <<"instructions">>, _attrs, _els}) ->
    Cdata = decode_xdata_instructions_els(__TopXMLNS,
					  __IgnoreEls, _els, <<>>),
    Cdata.

decode_xdata_instructions_els(__TopXMLNS, __IgnoreEls,
			      [], Cdata) ->
    decode_xdata_instructions_cdata(__TopXMLNS, Cdata);
decode_xdata_instructions_els(__TopXMLNS, __IgnoreEls,
			      [{xmlcdata, _data} | _els], Cdata) ->
    decode_xdata_instructions_els(__TopXMLNS, __IgnoreEls,
				  _els, <<Cdata/binary, _data/binary>>);
decode_xdata_instructions_els(__TopXMLNS, __IgnoreEls,
			      [_ | _els], Cdata) ->
    decode_xdata_instructions_els(__TopXMLNS, __IgnoreEls,
				  _els, Cdata).

encode_xdata_instructions(Cdata, _xmlns_attrs) ->
    _els = encode_xdata_instructions_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"instructions">>, _attrs, _els}.

decode_xdata_instructions_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_xdata_instructions_cdata(__TopXMLNS, _val) ->
    _val.

encode_xdata_instructions_cdata(undefined, _acc) ->
    _acc;
encode_xdata_instructions_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_xdata_field(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"field">>, _attrs, _els}) ->
    {Options, Values, Desc, Required} =
	decode_xdata_field_els(__TopXMLNS, __IgnoreEls, _els,
			       [], [], undefined, false),
    {Label, Type, Var} =
	decode_xdata_field_attrs(__TopXMLNS, _attrs, undefined,
				 undefined, undefined),
    {xdata_field, Label, Type, Var, Required, Desc, Values,
     Options}.

decode_xdata_field_els(__TopXMLNS, __IgnoreEls, [],
		       Options, Values, Desc, Required) ->
    {lists:reverse(Options), lists:reverse(Values), Desc,
     Required};
decode_xdata_field_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"required">>, _attrs, _} = _el | _els],
		       Options, Values, Desc, Required) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_xdata_field_els(__TopXMLNS, __IgnoreEls, _els,
				  Options, Values, Desc,
				  decode_xdata_field_required(__TopXMLNS,
							      __IgnoreEls,
							      _el));
       true ->
	   decode_xdata_field_els(__TopXMLNS, __IgnoreEls, _els,
				  Options, Values, Desc, Required)
    end;
decode_xdata_field_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"desc">>, _attrs, _} = _el | _els], Options,
		       Values, Desc, Required) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_xdata_field_els(__TopXMLNS, __IgnoreEls, _els,
				  Options, Values,
				  decode_xdata_field_desc(__TopXMLNS,
							  __IgnoreEls, _el),
				  Required);
       true ->
	   decode_xdata_field_els(__TopXMLNS, __IgnoreEls, _els,
				  Options, Values, Desc, Required)
    end;
decode_xdata_field_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"value">>, _attrs, _} = _el | _els], Options,
		       Values, Desc, Required) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_xdata_field_els(__TopXMLNS, __IgnoreEls, _els,
				  Options,
				  case decode_xdata_field_value(__TopXMLNS,
								__IgnoreEls,
								_el)
				      of
				    undefined -> Values;
				    _new_el -> [_new_el | Values]
				  end,
				  Desc, Required);
       true ->
	   decode_xdata_field_els(__TopXMLNS, __IgnoreEls, _els,
				  Options, Values, Desc, Required)
    end;
decode_xdata_field_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"option">>, _attrs, _} = _el | _els],
		       Options, Values, Desc, Required) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_xdata_field_els(__TopXMLNS, __IgnoreEls, _els,
				  case decode_xdata_field_option(__TopXMLNS,
								 __IgnoreEls,
								 _el)
				      of
				    undefined -> Options;
				    _new_el -> [_new_el | Options]
				  end,
				  Values, Desc, Required);
       true ->
	   decode_xdata_field_els(__TopXMLNS, __IgnoreEls, _els,
				  Options, Values, Desc, Required)
    end;
decode_xdata_field_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Options, Values, Desc, Required) ->
    decode_xdata_field_els(__TopXMLNS, __IgnoreEls, _els,
			   Options, Values, Desc, Required).

decode_xdata_field_attrs(__TopXMLNS,
			 [{<<"label">>, _val} | _attrs], _Label, Type, Var) ->
    decode_xdata_field_attrs(__TopXMLNS, _attrs, _val, Type,
			     Var);
decode_xdata_field_attrs(__TopXMLNS,
			 [{<<"type">>, _val} | _attrs], Label, _Type, Var) ->
    decode_xdata_field_attrs(__TopXMLNS, _attrs, Label,
			     _val, Var);
decode_xdata_field_attrs(__TopXMLNS,
			 [{<<"var">>, _val} | _attrs], Label, Type, _Var) ->
    decode_xdata_field_attrs(__TopXMLNS, _attrs, Label,
			     Type, _val);
decode_xdata_field_attrs(__TopXMLNS, [_ | _attrs],
			 Label, Type, Var) ->
    decode_xdata_field_attrs(__TopXMLNS, _attrs, Label,
			     Type, Var);
decode_xdata_field_attrs(__TopXMLNS, [], Label, Type,
			 Var) ->
    {decode_xdata_field_attr_label(__TopXMLNS, Label),
     decode_xdata_field_attr_type(__TopXMLNS, Type),
     decode_xdata_field_attr_var(__TopXMLNS, Var)}.

encode_xdata_field({xdata_field, Label, Type, Var,
		    Required, Desc, Values, Options},
		   _xmlns_attrs) ->
    _els =
	lists:reverse('encode_xdata_field_$options'(Options,
						    'encode_xdata_field_$values'(Values,
										 'encode_xdata_field_$desc'(Desc,
													    'encode_xdata_field_$required'(Required,
																	   []))))),
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

decode_xdata_field_attr_label(__TopXMLNS, undefined) ->
    undefined;
decode_xdata_field_attr_label(__TopXMLNS, _val) -> _val.

encode_xdata_field_attr_label(undefined, _acc) -> _acc;
encode_xdata_field_attr_label(_val, _acc) ->
    [{<<"label">>, _val} | _acc].

decode_xdata_field_attr_type(__TopXMLNS, undefined) ->
    undefined;
decode_xdata_field_attr_type(__TopXMLNS, _val) ->
    case catch dec_enum(_val,
			[boolean, fixed, hidden, 'jid-multi', 'jid-single',
			 'list-multi', 'list-single', 'text-multi',
			 'text-private', 'text-single'])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"type">>, <<"field">>, __TopXMLNS}});
      _res -> _res
    end.

encode_xdata_field_attr_type(undefined, _acc) -> _acc;
encode_xdata_field_attr_type(_val, _acc) ->
    [{<<"type">>, enc_enum(_val)} | _acc].

decode_xdata_field_attr_var(__TopXMLNS, undefined) ->
    undefined;
decode_xdata_field_attr_var(__TopXMLNS, _val) -> _val.

encode_xdata_field_attr_var(undefined, _acc) -> _acc;
encode_xdata_field_attr_var(_val, _acc) ->
    [{<<"var">>, _val} | _acc].

decode_xdata_field_option(__TopXMLNS, __IgnoreEls,
			  {xmlel, <<"option">>, _attrs, _els}) ->
    Value = decode_xdata_field_option_els(__TopXMLNS,
					  __IgnoreEls, _els, error),
    Value.

decode_xdata_field_option_els(__TopXMLNS, __IgnoreEls,
			      [], Value) ->
    case Value of
      error ->
	  erlang:error({xmpp_codec,
			{missing_tag, <<"value">>, __TopXMLNS}});
      {value, Value1} -> Value1
    end;
decode_xdata_field_option_els(__TopXMLNS, __IgnoreEls,
			      [{xmlel, <<"value">>, _attrs, _} = _el | _els],
			      Value) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_xdata_field_option_els(__TopXMLNS, __IgnoreEls,
					 _els,
					 {value,
					  decode_xdata_field_value(__TopXMLNS,
								   __IgnoreEls,
								   _el)});
       true ->
	   decode_xdata_field_option_els(__TopXMLNS, __IgnoreEls,
					 _els, Value)
    end;
decode_xdata_field_option_els(__TopXMLNS, __IgnoreEls,
			      [_ | _els], Value) ->
    decode_xdata_field_option_els(__TopXMLNS, __IgnoreEls,
				  _els, Value).

encode_xdata_field_option(Value, _xmlns_attrs) ->
    _els =
	lists:reverse('encode_xdata_field_option_$value'(Value,
							 [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"option">>, _attrs, _els}.

'encode_xdata_field_option_$value'(Value, _acc) ->
    [encode_xdata_field_value(Value, []) | _acc].

decode_xdata_field_value(__TopXMLNS, __IgnoreEls,
			 {xmlel, <<"value">>, _attrs, _els}) ->
    Cdata = decode_xdata_field_value_els(__TopXMLNS,
					 __IgnoreEls, _els, <<>>),
    Cdata.

decode_xdata_field_value_els(__TopXMLNS, __IgnoreEls,
			     [], Cdata) ->
    decode_xdata_field_value_cdata(__TopXMLNS, Cdata);
decode_xdata_field_value_els(__TopXMLNS, __IgnoreEls,
			     [{xmlcdata, _data} | _els], Cdata) ->
    decode_xdata_field_value_els(__TopXMLNS, __IgnoreEls,
				 _els, <<Cdata/binary, _data/binary>>);
decode_xdata_field_value_els(__TopXMLNS, __IgnoreEls,
			     [_ | _els], Cdata) ->
    decode_xdata_field_value_els(__TopXMLNS, __IgnoreEls,
				 _els, Cdata).

encode_xdata_field_value(Cdata, _xmlns_attrs) ->
    _els = encode_xdata_field_value_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"value">>, _attrs, _els}.

decode_xdata_field_value_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_xdata_field_value_cdata(__TopXMLNS, _val) ->
    _val.

encode_xdata_field_value_cdata(undefined, _acc) -> _acc;
encode_xdata_field_value_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_xdata_field_desc(__TopXMLNS, __IgnoreEls,
			{xmlel, <<"desc">>, _attrs, _els}) ->
    Cdata = decode_xdata_field_desc_els(__TopXMLNS,
					__IgnoreEls, _els, <<>>),
    Cdata.

decode_xdata_field_desc_els(__TopXMLNS, __IgnoreEls, [],
			    Cdata) ->
    decode_xdata_field_desc_cdata(__TopXMLNS, Cdata);
decode_xdata_field_desc_els(__TopXMLNS, __IgnoreEls,
			    [{xmlcdata, _data} | _els], Cdata) ->
    decode_xdata_field_desc_els(__TopXMLNS, __IgnoreEls,
				_els, <<Cdata/binary, _data/binary>>);
decode_xdata_field_desc_els(__TopXMLNS, __IgnoreEls,
			    [_ | _els], Cdata) ->
    decode_xdata_field_desc_els(__TopXMLNS, __IgnoreEls,
				_els, Cdata).

encode_xdata_field_desc(Cdata, _xmlns_attrs) ->
    _els = encode_xdata_field_desc_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"desc">>, _attrs, _els}.

decode_xdata_field_desc_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_xdata_field_desc_cdata(__TopXMLNS, _val) -> _val.

encode_xdata_field_desc_cdata(undefined, _acc) -> _acc;
encode_xdata_field_desc_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_xdata_field_required(__TopXMLNS, __IgnoreEls,
			    {xmlel, <<"required">>, _attrs, _els}) ->
    true.

encode_xdata_field_required(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"required">>, _attrs, _els}.

decode_vcard_xupdate(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"x">>, _attrs, _els}) ->
    Photo = decode_vcard_xupdate_els(__TopXMLNS,
				     __IgnoreEls, _els, undefined),
    {vcard_xupdate, Photo}.

decode_vcard_xupdate_els(__TopXMLNS, __IgnoreEls, [],
			 Photo) ->
    Photo;
decode_vcard_xupdate_els(__TopXMLNS, __IgnoreEls,
			 [{xmlel, <<"photo">>, _attrs, _} = _el | _els],
			 Photo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_xupdate_els(__TopXMLNS, __IgnoreEls, _els,
				    decode_vcard_xupdate_photo(__TopXMLNS,
							       __IgnoreEls,
							       _el));
       true ->
	   decode_vcard_xupdate_els(__TopXMLNS, __IgnoreEls, _els,
				    Photo)
    end;
decode_vcard_xupdate_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Photo) ->
    decode_vcard_xupdate_els(__TopXMLNS, __IgnoreEls, _els,
			     Photo).

encode_vcard_xupdate({vcard_xupdate, Photo},
		     _xmlns_attrs) ->
    _els =
	lists:reverse('encode_vcard_xupdate_$photo'(Photo, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"x">>, _attrs, _els}.

'encode_vcard_xupdate_$photo'(undefined, _acc) -> _acc;
'encode_vcard_xupdate_$photo'(Photo, _acc) ->
    [encode_vcard_xupdate_photo(Photo, []) | _acc].

decode_vcard_xupdate_photo(__TopXMLNS, __IgnoreEls,
			   {xmlel, <<"photo">>, _attrs, _els}) ->
    Cdata = decode_vcard_xupdate_photo_els(__TopXMLNS,
					   __IgnoreEls, _els, <<>>),
    Cdata.

decode_vcard_xupdate_photo_els(__TopXMLNS, __IgnoreEls,
			       [], Cdata) ->
    decode_vcard_xupdate_photo_cdata(__TopXMLNS, Cdata);
decode_vcard_xupdate_photo_els(__TopXMLNS, __IgnoreEls,
			       [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_xupdate_photo_els(__TopXMLNS, __IgnoreEls,
				   _els, <<Cdata/binary, _data/binary>>);
decode_vcard_xupdate_photo_els(__TopXMLNS, __IgnoreEls,
			       [_ | _els], Cdata) ->
    decode_vcard_xupdate_photo_els(__TopXMLNS, __IgnoreEls,
				   _els, Cdata).

encode_vcard_xupdate_photo(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_xupdate_photo_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"photo">>, _attrs, _els}.

decode_vcard_xupdate_photo_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_xupdate_photo_cdata(__TopXMLNS, _val) ->
    _val.

encode_vcard_xupdate_photo_cdata(undefined, _acc) ->
    _acc;
encode_vcard_xupdate_photo_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard(__TopXMLNS, __IgnoreEls,
	     {xmlel, <<"vCard">>, _attrs, _els}) ->
    {Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
     Jabberid, Sound, Note, Role, Title, Nickname, Rev,
     Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
     Fn, Version, N, Photo, Logo, Geo} =
	decode_vcard_els(__TopXMLNS, __IgnoreEls, _els,
			 undefined, [], undefined, [], undefined, undefined,
			 undefined, undefined, undefined, undefined, undefined,
			 undefined, undefined, undefined, undefined, undefined,
			 undefined, undefined, undefined, undefined, [], [], [],
			 undefined, undefined, undefined, undefined, undefined,
			 undefined),
    {vcard, Version, Fn, N, Nickname, Photo, Bday, Adr,
     Label, Tel, Email, Jabberid, Mailer, Tz, Geo, Title,
     Role, Logo, Org, Categories, Note, Prodid, Rev,
     Sort_string, Sound, Uid, Url, Class, Key, Desc}.

decode_vcard_els(__TopXMLNS, __IgnoreEls, [], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    {Mailer, lists:reverse(Adr), Class, Categories, Desc,
     Uid, Prodid, Jabberid, Sound, Note, Role, Title,
     Nickname, Rev, Sort_string, Org, Bday, Key, Tz, Url,
     lists:reverse(Email), lists:reverse(Tel),
     lists:reverse(Label), Fn, Version, N, Photo, Logo, Geo};
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"N">>, _attrs, _} = _el | _els], Mailer, Adr,
		 Class, Categories, Desc, Uid, Prodid, Jabberid, Sound,
		 Note, Role, Title, Nickname, Rev, Sort_string, Org,
		 Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version, N,
		 Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version,
			    decode_vcard_N(__TopXMLNS, __IgnoreEls, _el), Photo,
			    Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"ADR">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    [decode_vcard_ADR(__TopXMLNS, __IgnoreEls, _el)
			     | Adr],
			    Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"LABEL">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    [decode_vcard_LABEL(__TopXMLNS, __IgnoreEls, _el)
			     | Label],
			    Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"TEL">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email,
			    [decode_vcard_TEL(__TopXMLNS, __IgnoreEls, _el)
			     | Tel],
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"EMAIL">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url,
			    [decode_vcard_EMAIL(__TopXMLNS, __IgnoreEls, _el)
			     | Email],
			    Tel, Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"GEO">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo,
			    decode_vcard_GEO(__TopXMLNS, __IgnoreEls, _el));
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"LOGO">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo,
			    decode_vcard_LOGO(__TopXMLNS, __IgnoreEls, _el),
			    Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"PHOTO">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N,
			    decode_vcard_PHOTO(__TopXMLNS, __IgnoreEls, _el),
			    Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"ORG">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string,
			    decode_vcard_ORG(__TopXMLNS, __IgnoreEls, _el),
			    Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
			    N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"SOUND">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    decode_vcard_SOUND(__TopXMLNS, __IgnoreEls, _el),
			    Note, Role, Title, Nickname, Rev, Sort_string, Org,
			    Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
			    N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"KEY">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday,
			    decode_vcard_KEY(__TopXMLNS, __IgnoreEls, _el), Tz,
			    Url, Email, Tel, Label, Fn, Version, N, Photo, Logo,
			    Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"VERSION">>, _attrs, _} = _el | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn,
			    decode_vcard_VERSION(__TopXMLNS, __IgnoreEls, _el),
			    N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"FN">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label,
			    decode_vcard_FN(__TopXMLNS, __IgnoreEls, _el),
			    Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"NICKNAME">>, _attrs, _} = _el | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title,
			    decode_vcard_NICKNAME(__TopXMLNS, __IgnoreEls, _el),
			    Rev, Sort_string, Org, Bday, Key, Tz, Url, Email,
			    Tel, Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"BDAY">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org,
			    decode_vcard_BDAY(__TopXMLNS, __IgnoreEls, _el),
			    Key, Tz, Url, Email, Tel, Label, Fn, Version, N,
			    Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"JABBERID">>, _attrs, _} = _el | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid,
			    decode_vcard_JABBERID(__TopXMLNS, __IgnoreEls, _el),
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"MAILER">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els,
			    decode_vcard_MAILER(__TopXMLNS, __IgnoreEls, _el),
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"TZ">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key,
			    decode_vcard_TZ(__TopXMLNS, __IgnoreEls, _el), Url,
			    Email, Tel, Label, Fn, Version, N, Photo, Logo,
			    Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"TITLE">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role,
			    decode_vcard_TITLE(__TopXMLNS, __IgnoreEls, _el),
			    Nickname, Rev, Sort_string, Org, Bday, Key, Tz, Url,
			    Email, Tel, Label, Fn, Version, N, Photo, Logo,
			    Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"ROLE">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note,
			    decode_vcard_ROLE(__TopXMLNS, __IgnoreEls, _el),
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"NOTE">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound,
			    decode_vcard_NOTE(__TopXMLNS, __IgnoreEls, _el),
			    Role, Title, Nickname, Rev, Sort_string, Org, Bday,
			    Key, Tz, Url, Email, Tel, Label, Fn, Version, N,
			    Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"PRODID">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid,
			    decode_vcard_PRODID(__TopXMLNS, __IgnoreEls, _el),
			    Jabberid, Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"REV">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname,
			    decode_vcard_REV(__TopXMLNS, __IgnoreEls, _el),
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"SORT-STRING">>, _attrs, _} = _el | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    decode_vcard_SORT_STRING(__TopXMLNS, __IgnoreEls,
						     _el),
			    Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn,
			    Version, N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"UID">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc,
			    decode_vcard_UID(__TopXMLNS, __IgnoreEls, _el),
			    Prodid, Jabberid, Sound, Note, Role, Title,
			    Nickname, Rev, Sort_string, Org, Bday, Key, Tz, Url,
			    Email, Tel, Label, Fn, Version, N, Photo, Logo,
			    Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"URL">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz,
			    decode_vcard_URL(__TopXMLNS, __IgnoreEls, _el),
			    Email, Tel, Label, Fn, Version, N, Photo, Logo,
			    Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"DESC">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories,
			    decode_vcard_DESC(__TopXMLNS, __IgnoreEls, _el),
			    Uid, Prodid, Jabberid, Sound, Note, Role, Title,
			    Nickname, Rev, Sort_string, Org, Bday, Key, Tz, Url,
			    Email, Tel, Label, Fn, Version, N, Photo, Logo,
			    Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"CATEGORIES">>, _attrs, _} = _el | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class,
			    decode_vcard_CATEGORIES(__TopXMLNS, __IgnoreEls,
						    _el),
			    Desc, Uid, Prodid, Jabberid, Sound, Note, Role,
			    Title, Nickname, Rev, Sort_string, Org, Bday, Key,
			    Tz, Url, Email, Tel, Label, Fn, Version, N, Photo,
			    Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"CLASS">>, _attrs, _} = _el | _els], Mailer,
		 Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		 Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		 Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		 N, Photo, Logo, Geo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr,
			    decode_vcard_CLASS(__TopXMLNS, __IgnoreEls, _el),
			    Categories, Desc, Uid, Prodid, Jabberid, Sound,
			    Note, Role, Title, Nickname, Rev, Sort_string, Org,
			    Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
			    N, Photo, Logo, Geo);
       true ->
	   decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
			    Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
			    Sound, Note, Role, Title, Nickname, Rev,
			    Sort_string, Org, Bday, Key, Tz, Url, Email, Tel,
			    Label, Fn, Version, N, Photo, Logo, Geo)
    end;
decode_vcard_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		 Mailer, Adr, Class, Categories, Desc, Uid, Prodid,
		 Jabberid, Sound, Note, Role, Title, Nickname, Rev,
		 Sort_string, Org, Bday, Key, Tz, Url, Email, Tel, Label,
		 Fn, Version, N, Photo, Logo, Geo) ->
    decode_vcard_els(__TopXMLNS, __IgnoreEls, _els, Mailer,
		     Adr, Class, Categories, Desc, Uid, Prodid, Jabberid,
		     Sound, Note, Role, Title, Nickname, Rev, Sort_string,
		     Org, Bday, Key, Tz, Url, Email, Tel, Label, Fn, Version,
		     N, Photo, Logo, Geo).

encode_vcard({vcard, Version, Fn, N, Nickname, Photo,
	      Bday, Adr, Label, Tel, Email, Jabberid, Mailer, Tz, Geo,
	      Title, Role, Logo, Org, Categories, Note, Prodid, Rev,
	      Sort_string, Sound, Uid, Url, Class, Key, Desc},
	     _xmlns_attrs) ->
    _els = lists:reverse('encode_vcard_$mailer'(Mailer,
						'encode_vcard_$adr'(Adr,
								    'encode_vcard_$class'(Class,
											  'encode_vcard_$categories'(Categories,
														     'encode_vcard_$desc'(Desc,
																	  'encode_vcard_$uid'(Uid,
																			      'encode_vcard_$prodid'(Prodid,
																						     'encode_vcard_$jabberid'(Jabberid,
																									      'encode_vcard_$sound'(Sound,
																												    'encode_vcard_$note'(Note,
																															 'encode_vcard_$role'(Role,
																																	      'encode_vcard_$title'(Title,
																																				    'encode_vcard_$nickname'(Nickname,
																																							     'encode_vcard_$rev'(Rev,
																																										 'encode_vcard_$sort_string'(Sort_string,
																																													     'encode_vcard_$org'(Org,
																																																 'encode_vcard_$bday'(Bday,
																																																		      'encode_vcard_$key'(Key,
																																																					  'encode_vcard_$tz'(Tz,
																																																							     'encode_vcard_$url'(Url,
																																																										 'encode_vcard_$email'(Email,
																																																												       'encode_vcard_$tel'(Tel,
																																																															   'encode_vcard_$label'(Label,
																																																																		 'encode_vcard_$fn'(Fn,
																																																																				    'encode_vcard_$version'(Version,
																																																																							    'encode_vcard_$n'(N,
																																																																									      'encode_vcard_$photo'(Photo,
																																																																												    'encode_vcard_$logo'(Logo,
																																																																															 'encode_vcard_$geo'(Geo,
																																																																																	     [])))))))))))))))))))))))))))))),
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

decode_vcard_CLASS(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"CLASS">>, _attrs, _els}) ->
    Class = decode_vcard_CLASS_els(__TopXMLNS, __IgnoreEls,
				   _els, undefined),
    Class.

decode_vcard_CLASS_els(__TopXMLNS, __IgnoreEls, [],
		       Class) ->
    Class;
decode_vcard_CLASS_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"PUBLIC">>, _attrs, _} = _el | _els],
		       Class) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_CLASS_els(__TopXMLNS, __IgnoreEls, _els,
				  decode_vcard_PUBLIC(__TopXMLNS, __IgnoreEls,
						      _el));
       true ->
	   decode_vcard_CLASS_els(__TopXMLNS, __IgnoreEls, _els,
				  Class)
    end;
decode_vcard_CLASS_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"PRIVATE">>, _attrs, _} = _el | _els],
		       Class) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_CLASS_els(__TopXMLNS, __IgnoreEls, _els,
				  decode_vcard_PRIVATE(__TopXMLNS, __IgnoreEls,
						       _el));
       true ->
	   decode_vcard_CLASS_els(__TopXMLNS, __IgnoreEls, _els,
				  Class)
    end;
decode_vcard_CLASS_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"CONFIDENTIAL">>, _attrs, _} = _el | _els],
		       Class) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_CLASS_els(__TopXMLNS, __IgnoreEls, _els,
				  decode_vcard_CONFIDENTIAL(__TopXMLNS,
							    __IgnoreEls, _el));
       true ->
	   decode_vcard_CLASS_els(__TopXMLNS, __IgnoreEls, _els,
				  Class)
    end;
decode_vcard_CLASS_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Class) ->
    decode_vcard_CLASS_els(__TopXMLNS, __IgnoreEls, _els,
			   Class).

encode_vcard_CLASS(Class, _xmlns_attrs) ->
    _els = lists:reverse('encode_vcard_CLASS_$class'(Class,
						     [])),
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

decode_vcard_CATEGORIES(__TopXMLNS, __IgnoreEls,
			{xmlel, <<"CATEGORIES">>, _attrs, _els}) ->
    Keywords = decode_vcard_CATEGORIES_els(__TopXMLNS,
					   __IgnoreEls, _els, []),
    Keywords.

decode_vcard_CATEGORIES_els(__TopXMLNS, __IgnoreEls, [],
			    Keywords) ->
    lists:reverse(Keywords);
decode_vcard_CATEGORIES_els(__TopXMLNS, __IgnoreEls,
			    [{xmlel, <<"KEYWORD">>, _attrs, _} = _el | _els],
			    Keywords) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_CATEGORIES_els(__TopXMLNS, __IgnoreEls,
				       _els,
				       case decode_vcard_KEYWORD(__TopXMLNS,
								 __IgnoreEls,
								 _el)
					   of
					 undefined -> Keywords;
					 _new_el -> [_new_el | Keywords]
				       end);
       true ->
	   decode_vcard_CATEGORIES_els(__TopXMLNS, __IgnoreEls,
				       _els, Keywords)
    end;
decode_vcard_CATEGORIES_els(__TopXMLNS, __IgnoreEls,
			    [_ | _els], Keywords) ->
    decode_vcard_CATEGORIES_els(__TopXMLNS, __IgnoreEls,
				_els, Keywords).

encode_vcard_CATEGORIES(Keywords, _xmlns_attrs) ->
    _els =
	lists:reverse('encode_vcard_CATEGORIES_$keywords'(Keywords,
							  [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"CATEGORIES">>, _attrs, _els}.

'encode_vcard_CATEGORIES_$keywords'([], _acc) -> _acc;
'encode_vcard_CATEGORIES_$keywords'([Keywords | _els],
				    _acc) ->
    'encode_vcard_CATEGORIES_$keywords'(_els,
					[encode_vcard_KEYWORD(Keywords, [])
					 | _acc]).

decode_vcard_KEY(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"KEY">>, _attrs, _els}) ->
    {Cred, Type} = decode_vcard_KEY_els(__TopXMLNS,
					__IgnoreEls, _els, undefined,
					undefined),
    {vcard_key, Type, Cred}.

decode_vcard_KEY_els(__TopXMLNS, __IgnoreEls, [], Cred,
		     Type) ->
    {Cred, Type};
decode_vcard_KEY_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"TYPE">>, _attrs, _} = _el | _els], Cred,
		     Type) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_KEY_els(__TopXMLNS, __IgnoreEls, _els,
				Cred,
				decode_vcard_TYPE(__TopXMLNS, __IgnoreEls,
						  _el));
       true ->
	   decode_vcard_KEY_els(__TopXMLNS, __IgnoreEls, _els,
				Cred, Type)
    end;
decode_vcard_KEY_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"CRED">>, _attrs, _} = _el | _els], Cred,
		     Type) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_KEY_els(__TopXMLNS, __IgnoreEls, _els,
				decode_vcard_CRED(__TopXMLNS, __IgnoreEls, _el),
				Type);
       true ->
	   decode_vcard_KEY_els(__TopXMLNS, __IgnoreEls, _els,
				Cred, Type)
    end;
decode_vcard_KEY_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Cred, Type) ->
    decode_vcard_KEY_els(__TopXMLNS, __IgnoreEls, _els,
			 Cred, Type).

encode_vcard_KEY({vcard_key, Type, Cred},
		 _xmlns_attrs) ->
    _els = lists:reverse('encode_vcard_KEY_$cred'(Cred,
						  'encode_vcard_KEY_$type'(Type,
									   []))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"KEY">>, _attrs, _els}.

'encode_vcard_KEY_$cred'(undefined, _acc) -> _acc;
'encode_vcard_KEY_$cred'(Cred, _acc) ->
    [encode_vcard_CRED(Cred, []) | _acc].

'encode_vcard_KEY_$type'(undefined, _acc) -> _acc;
'encode_vcard_KEY_$type'(Type, _acc) ->
    [encode_vcard_TYPE(Type, []) | _acc].

decode_vcard_SOUND(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"SOUND">>, _attrs, _els}) ->
    {Phonetic, Extval, Binval} =
	decode_vcard_SOUND_els(__TopXMLNS, __IgnoreEls, _els,
			       undefined, undefined, undefined),
    {vcard_sound, Phonetic, Binval, Extval}.

decode_vcard_SOUND_els(__TopXMLNS, __IgnoreEls, [],
		       Phonetic, Extval, Binval) ->
    {Phonetic, Extval, Binval};
decode_vcard_SOUND_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"BINVAL">>, _attrs, _} = _el | _els],
		       Phonetic, Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_SOUND_els(__TopXMLNS, __IgnoreEls, _els,
				  Phonetic, Extval,
				  decode_vcard_BINVAL(__TopXMLNS, __IgnoreEls,
						      _el));
       true ->
	   decode_vcard_SOUND_els(__TopXMLNS, __IgnoreEls, _els,
				  Phonetic, Extval, Binval)
    end;
decode_vcard_SOUND_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"EXTVAL">>, _attrs, _} = _el | _els],
		       Phonetic, Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_SOUND_els(__TopXMLNS, __IgnoreEls, _els,
				  Phonetic,
				  decode_vcard_EXTVAL(__TopXMLNS, __IgnoreEls,
						      _el),
				  Binval);
       true ->
	   decode_vcard_SOUND_els(__TopXMLNS, __IgnoreEls, _els,
				  Phonetic, Extval, Binval)
    end;
decode_vcard_SOUND_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"PHONETIC">>, _attrs, _} = _el | _els],
		       Phonetic, Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_SOUND_els(__TopXMLNS, __IgnoreEls, _els,
				  decode_vcard_PHONETIC(__TopXMLNS, __IgnoreEls,
							_el),
				  Extval, Binval);
       true ->
	   decode_vcard_SOUND_els(__TopXMLNS, __IgnoreEls, _els,
				  Phonetic, Extval, Binval)
    end;
decode_vcard_SOUND_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Phonetic, Extval, Binval) ->
    decode_vcard_SOUND_els(__TopXMLNS, __IgnoreEls, _els,
			   Phonetic, Extval, Binval).

encode_vcard_SOUND({vcard_sound, Phonetic, Binval,
		    Extval},
		   _xmlns_attrs) ->
    _els =
	lists:reverse('encode_vcard_SOUND_$phonetic'(Phonetic,
						     'encode_vcard_SOUND_$extval'(Extval,
										  'encode_vcard_SOUND_$binval'(Binval,
													       [])))),
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

decode_vcard_ORG(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"ORG">>, _attrs, _els}) ->
    {Units, Name} = decode_vcard_ORG_els(__TopXMLNS,
					 __IgnoreEls, _els, [], undefined),
    {vcard_org, Name, Units}.

decode_vcard_ORG_els(__TopXMLNS, __IgnoreEls, [], Units,
		     Name) ->
    {lists:reverse(Units), Name};
decode_vcard_ORG_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"ORGNAME">>, _attrs, _} = _el | _els], Units,
		     Name) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ORG_els(__TopXMLNS, __IgnoreEls, _els,
				Units,
				decode_vcard_ORGNAME(__TopXMLNS, __IgnoreEls,
						     _el));
       true ->
	   decode_vcard_ORG_els(__TopXMLNS, __IgnoreEls, _els,
				Units, Name)
    end;
decode_vcard_ORG_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"ORGUNIT">>, _attrs, _} = _el | _els], Units,
		     Name) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ORG_els(__TopXMLNS, __IgnoreEls, _els,
				case decode_vcard_ORGUNIT(__TopXMLNS,
							  __IgnoreEls, _el)
				    of
				  undefined -> Units;
				  _new_el -> [_new_el | Units]
				end,
				Name);
       true ->
	   decode_vcard_ORG_els(__TopXMLNS, __IgnoreEls, _els,
				Units, Name)
    end;
decode_vcard_ORG_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Units, Name) ->
    decode_vcard_ORG_els(__TopXMLNS, __IgnoreEls, _els,
			 Units, Name).

encode_vcard_ORG({vcard_org, Name, Units},
		 _xmlns_attrs) ->
    _els = lists:reverse('encode_vcard_ORG_$units'(Units,
						   'encode_vcard_ORG_$name'(Name,
									    []))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"ORG">>, _attrs, _els}.

'encode_vcard_ORG_$units'([], _acc) -> _acc;
'encode_vcard_ORG_$units'([Units | _els], _acc) ->
    'encode_vcard_ORG_$units'(_els,
			      [encode_vcard_ORGUNIT(Units, []) | _acc]).

'encode_vcard_ORG_$name'(undefined, _acc) -> _acc;
'encode_vcard_ORG_$name'(Name, _acc) ->
    [encode_vcard_ORGNAME(Name, []) | _acc].

decode_vcard_PHOTO(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"PHOTO">>, _attrs, _els}) ->
    {Type, Extval, Binval} =
	decode_vcard_PHOTO_els(__TopXMLNS, __IgnoreEls, _els,
			       undefined, undefined, undefined),
    {vcard_photo, Type, Binval, Extval}.

decode_vcard_PHOTO_els(__TopXMLNS, __IgnoreEls, [],
		       Type, Extval, Binval) ->
    {Type, Extval, Binval};
decode_vcard_PHOTO_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"TYPE">>, _attrs, _} = _el | _els], Type,
		       Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_PHOTO_els(__TopXMLNS, __IgnoreEls, _els,
				  decode_vcard_TYPE(__TopXMLNS, __IgnoreEls,
						    _el),
				  Extval, Binval);
       true ->
	   decode_vcard_PHOTO_els(__TopXMLNS, __IgnoreEls, _els,
				  Type, Extval, Binval)
    end;
decode_vcard_PHOTO_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"BINVAL">>, _attrs, _} = _el | _els], Type,
		       Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_PHOTO_els(__TopXMLNS, __IgnoreEls, _els,
				  Type, Extval,
				  decode_vcard_BINVAL(__TopXMLNS, __IgnoreEls,
						      _el));
       true ->
	   decode_vcard_PHOTO_els(__TopXMLNS, __IgnoreEls, _els,
				  Type, Extval, Binval)
    end;
decode_vcard_PHOTO_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"EXTVAL">>, _attrs, _} = _el | _els], Type,
		       Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_PHOTO_els(__TopXMLNS, __IgnoreEls, _els,
				  Type,
				  decode_vcard_EXTVAL(__TopXMLNS, __IgnoreEls,
						      _el),
				  Binval);
       true ->
	   decode_vcard_PHOTO_els(__TopXMLNS, __IgnoreEls, _els,
				  Type, Extval, Binval)
    end;
decode_vcard_PHOTO_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Type, Extval, Binval) ->
    decode_vcard_PHOTO_els(__TopXMLNS, __IgnoreEls, _els,
			   Type, Extval, Binval).

encode_vcard_PHOTO({vcard_photo, Type, Binval, Extval},
		   _xmlns_attrs) ->
    _els = lists:reverse('encode_vcard_PHOTO_$type'(Type,
						    'encode_vcard_PHOTO_$extval'(Extval,
										 'encode_vcard_PHOTO_$binval'(Binval,
													      [])))),
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

decode_vcard_LOGO(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"LOGO">>, _attrs, _els}) ->
    {Type, Extval, Binval} =
	decode_vcard_LOGO_els(__TopXMLNS, __IgnoreEls, _els,
			      undefined, undefined, undefined),
    {vcard_logo, Type, Binval, Extval}.

decode_vcard_LOGO_els(__TopXMLNS, __IgnoreEls, [], Type,
		      Extval, Binval) ->
    {Type, Extval, Binval};
decode_vcard_LOGO_els(__TopXMLNS, __IgnoreEls,
		      [{xmlel, <<"TYPE">>, _attrs, _} = _el | _els], Type,
		      Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_LOGO_els(__TopXMLNS, __IgnoreEls, _els,
				 decode_vcard_TYPE(__TopXMLNS, __IgnoreEls,
						   _el),
				 Extval, Binval);
       true ->
	   decode_vcard_LOGO_els(__TopXMLNS, __IgnoreEls, _els,
				 Type, Extval, Binval)
    end;
decode_vcard_LOGO_els(__TopXMLNS, __IgnoreEls,
		      [{xmlel, <<"BINVAL">>, _attrs, _} = _el | _els], Type,
		      Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_LOGO_els(__TopXMLNS, __IgnoreEls, _els,
				 Type, Extval,
				 decode_vcard_BINVAL(__TopXMLNS, __IgnoreEls,
						     _el));
       true ->
	   decode_vcard_LOGO_els(__TopXMLNS, __IgnoreEls, _els,
				 Type, Extval, Binval)
    end;
decode_vcard_LOGO_els(__TopXMLNS, __IgnoreEls,
		      [{xmlel, <<"EXTVAL">>, _attrs, _} = _el | _els], Type,
		      Extval, Binval) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_LOGO_els(__TopXMLNS, __IgnoreEls, _els,
				 Type,
				 decode_vcard_EXTVAL(__TopXMLNS, __IgnoreEls,
						     _el),
				 Binval);
       true ->
	   decode_vcard_LOGO_els(__TopXMLNS, __IgnoreEls, _els,
				 Type, Extval, Binval)
    end;
decode_vcard_LOGO_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Type, Extval, Binval) ->
    decode_vcard_LOGO_els(__TopXMLNS, __IgnoreEls, _els,
			  Type, Extval, Binval).

encode_vcard_LOGO({vcard_logo, Type, Binval, Extval},
		  _xmlns_attrs) ->
    _els = lists:reverse('encode_vcard_LOGO_$type'(Type,
						   'encode_vcard_LOGO_$extval'(Extval,
									       'encode_vcard_LOGO_$binval'(Binval,
													   [])))),
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

decode_vcard_BINVAL(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"BINVAL">>, _attrs, _els}) ->
    Cdata = decode_vcard_BINVAL_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_vcard_BINVAL_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_vcard_BINVAL_cdata(__TopXMLNS, Cdata);
decode_vcard_BINVAL_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_BINVAL_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_BINVAL_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_vcard_BINVAL_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_vcard_BINVAL(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_BINVAL_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"BINVAL">>, _attrs, _els}.

decode_vcard_BINVAL_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_BINVAL_cdata(__TopXMLNS, _val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"BINVAL">>, __TopXMLNS}});
      _res -> _res
    end.

encode_vcard_BINVAL_cdata(undefined, _acc) -> _acc;
encode_vcard_BINVAL_cdata(_val, _acc) ->
    [{xmlcdata, base64:encode(_val)} | _acc].

decode_vcard_GEO(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"GEO">>, _attrs, _els}) ->
    {Lat, Lon} = decode_vcard_GEO_els(__TopXMLNS,
				      __IgnoreEls, _els, undefined, undefined),
    {vcard_geo, Lat, Lon}.

decode_vcard_GEO_els(__TopXMLNS, __IgnoreEls, [], Lat,
		     Lon) ->
    {Lat, Lon};
decode_vcard_GEO_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"LAT">>, _attrs, _} = _el | _els], Lat,
		     Lon) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_GEO_els(__TopXMLNS, __IgnoreEls, _els,
				decode_vcard_LAT(__TopXMLNS, __IgnoreEls, _el),
				Lon);
       true ->
	   decode_vcard_GEO_els(__TopXMLNS, __IgnoreEls, _els, Lat,
				Lon)
    end;
decode_vcard_GEO_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"LON">>, _attrs, _} = _el | _els], Lat,
		     Lon) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_GEO_els(__TopXMLNS, __IgnoreEls, _els, Lat,
				decode_vcard_LON(__TopXMLNS, __IgnoreEls, _el));
       true ->
	   decode_vcard_GEO_els(__TopXMLNS, __IgnoreEls, _els, Lat,
				Lon)
    end;
decode_vcard_GEO_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Lat, Lon) ->
    decode_vcard_GEO_els(__TopXMLNS, __IgnoreEls, _els, Lat,
			 Lon).

encode_vcard_GEO({vcard_geo, Lat, Lon}, _xmlns_attrs) ->
    _els = lists:reverse('encode_vcard_GEO_$lat'(Lat,
						 'encode_vcard_GEO_$lon'(Lon,
									 []))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"GEO">>, _attrs, _els}.

'encode_vcard_GEO_$lat'(undefined, _acc) -> _acc;
'encode_vcard_GEO_$lat'(Lat, _acc) ->
    [encode_vcard_LAT(Lat, []) | _acc].

'encode_vcard_GEO_$lon'(undefined, _acc) -> _acc;
'encode_vcard_GEO_$lon'(Lon, _acc) ->
    [encode_vcard_LON(Lon, []) | _acc].

decode_vcard_EMAIL(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"EMAIL">>, _attrs, _els}) ->
    {X400, Userid, Internet, Home, Pref, Work} =
	decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls, _els,
			       false, undefined, false, false, false, false),
    {vcard_email, Home, Work, Internet, Pref, X400, Userid}.

decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls, [],
		       X400, Userid, Internet, Home, Pref, Work) ->
    {X400, Userid, Internet, Home, Pref, Work};
decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"HOME">>, _attrs, _} = _el | _els], X400,
		       Userid, Internet, Home, Pref, Work) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls, _els,
				  X400, Userid, Internet,
				  decode_vcard_HOME(__TopXMLNS, __IgnoreEls,
						    _el),
				  Pref, Work);
       true ->
	   decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls, _els,
				  X400, Userid, Internet, Home, Pref, Work)
    end;
decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"WORK">>, _attrs, _} = _el | _els], X400,
		       Userid, Internet, Home, Pref, Work) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls, _els,
				  X400, Userid, Internet, Home, Pref,
				  decode_vcard_WORK(__TopXMLNS, __IgnoreEls,
						    _el));
       true ->
	   decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls, _els,
				  X400, Userid, Internet, Home, Pref, Work)
    end;
decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"INTERNET">>, _attrs, _} = _el | _els], X400,
		       Userid, Internet, Home, Pref, Work) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls, _els,
				  X400, Userid,
				  decode_vcard_INTERNET(__TopXMLNS, __IgnoreEls,
							_el),
				  Home, Pref, Work);
       true ->
	   decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls, _els,
				  X400, Userid, Internet, Home, Pref, Work)
    end;
decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"PREF">>, _attrs, _} = _el | _els], X400,
		       Userid, Internet, Home, Pref, Work) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls, _els,
				  X400, Userid, Internet, Home,
				  decode_vcard_PREF(__TopXMLNS, __IgnoreEls,
						    _el),
				  Work);
       true ->
	   decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls, _els,
				  X400, Userid, Internet, Home, Pref, Work)
    end;
decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"X400">>, _attrs, _} = _el | _els], X400,
		       Userid, Internet, Home, Pref, Work) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls, _els,
				  decode_vcard_X400(__TopXMLNS, __IgnoreEls,
						    _el),
				  Userid, Internet, Home, Pref, Work);
       true ->
	   decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls, _els,
				  X400, Userid, Internet, Home, Pref, Work)
    end;
decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"USERID">>, _attrs, _} = _el | _els], X400,
		       Userid, Internet, Home, Pref, Work) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls, _els,
				  X400,
				  decode_vcard_USERID(__TopXMLNS, __IgnoreEls,
						      _el),
				  Internet, Home, Pref, Work);
       true ->
	   decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls, _els,
				  X400, Userid, Internet, Home, Pref, Work)
    end;
decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], X400, Userid, Internet, Home, Pref, Work) ->
    decode_vcard_EMAIL_els(__TopXMLNS, __IgnoreEls, _els,
			   X400, Userid, Internet, Home, Pref, Work).

encode_vcard_EMAIL({vcard_email, Home, Work, Internet,
		    Pref, X400, Userid},
		   _xmlns_attrs) ->
    _els = lists:reverse('encode_vcard_EMAIL_$x400'(X400,
						    'encode_vcard_EMAIL_$userid'(Userid,
										 'encode_vcard_EMAIL_$internet'(Internet,
														'encode_vcard_EMAIL_$home'(Home,
																	   'encode_vcard_EMAIL_$pref'(Pref,
																				      'encode_vcard_EMAIL_$work'(Work,
																								 []))))))),
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

decode_vcard_TEL(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"TEL">>, _attrs, _els}) ->
    {Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
     Work, Cell, Modem, Isdn, Video} =
	decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
			     undefined, false, false, false, false, false,
			     false, false, false, false, false, false, false,
			     false),
    {vcard_tel, Home, Work, Voice, Fax, Pager, Msg, Cell,
     Video, Bbs, Modem, Isdn, Pcs, Pref, Number}.

decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, [],
		     Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
		     Work, Cell, Modem, Isdn, Video) ->
    {Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
     Work, Cell, Modem, Isdn, Video};
decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"HOME">>, _attrs, _} = _el | _els], Number,
		     Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax, Work,
		     Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice,
				decode_vcard_HOME(__TopXMLNS, __IgnoreEls, _el),
				Pref, Msg, Fax, Work, Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn, Video)
    end;
decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"WORK">>, _attrs, _} = _el | _els], Number,
		     Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax, Work,
		     Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax,
				decode_vcard_WORK(__TopXMLNS, __IgnoreEls, _el),
				Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn, Video)
    end;
decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"VOICE">>, _attrs, _} = _el | _els], Number,
		     Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax, Work,
		     Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs,
				decode_vcard_VOICE(__TopXMLNS, __IgnoreEls,
						   _el),
				Home, Pref, Msg, Fax, Work, Cell, Modem, Isdn,
				Video);
       true ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn, Video)
    end;
decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"FAX">>, _attrs, _} = _el | _els], Number,
		     Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax, Work,
		     Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				decode_vcard_FAX(__TopXMLNS, __IgnoreEls, _el),
				Work, Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn, Video)
    end;
decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"PAGER">>, _attrs, _} = _el | _els], Number,
		     Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax, Work,
		     Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number,
				decode_vcard_PAGER(__TopXMLNS, __IgnoreEls,
						   _el),
				Pcs, Bbs, Voice, Home, Pref, Msg, Fax, Work,
				Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn, Video)
    end;
decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"MSG">>, _attrs, _} = _el | _els], Number,
		     Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax, Work,
		     Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref,
				decode_vcard_MSG(__TopXMLNS, __IgnoreEls, _el),
				Fax, Work, Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn, Video)
    end;
decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"CELL">>, _attrs, _} = _el | _els], Number,
		     Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax, Work,
		     Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work,
				decode_vcard_CELL(__TopXMLNS, __IgnoreEls, _el),
				Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn, Video)
    end;
decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"VIDEO">>, _attrs, _} = _el | _els], Number,
		     Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax, Work,
		     Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn,
				decode_vcard_VIDEO(__TopXMLNS, __IgnoreEls,
						   _el));
       true ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn, Video)
    end;
decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"BBS">>, _attrs, _} = _el | _els], Number,
		     Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax, Work,
		     Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs,
				decode_vcard_BBS(__TopXMLNS, __IgnoreEls, _el),
				Voice, Home, Pref, Msg, Fax, Work, Cell, Modem,
				Isdn, Video);
       true ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn, Video)
    end;
decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"MODEM">>, _attrs, _} = _el | _els], Number,
		     Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax, Work,
		     Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell,
				decode_vcard_MODEM(__TopXMLNS, __IgnoreEls,
						   _el),
				Isdn, Video);
       true ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn, Video)
    end;
decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"ISDN">>, _attrs, _} = _el | _els], Number,
		     Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax, Work,
		     Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem,
				decode_vcard_ISDN(__TopXMLNS, __IgnoreEls, _el),
				Video);
       true ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn, Video)
    end;
decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"PCS">>, _attrs, _} = _el | _els], Number,
		     Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax, Work,
		     Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager,
				decode_vcard_PCS(__TopXMLNS, __IgnoreEls, _el),
				Bbs, Voice, Home, Pref, Msg, Fax, Work, Cell,
				Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn, Video)
    end;
decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"PREF">>, _attrs, _} = _el | _els], Number,
		     Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax, Work,
		     Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home,
				decode_vcard_PREF(__TopXMLNS, __IgnoreEls, _el),
				Msg, Fax, Work, Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn, Video)
    end;
decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"NUMBER">>, _attrs, _} = _el | _els], Number,
		     Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax, Work,
		     Cell, Modem, Isdn, Video) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				decode_vcard_NUMBER(__TopXMLNS, __IgnoreEls,
						    _el),
				Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
				Work, Cell, Modem, Isdn, Video);
       true ->
	   decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
				Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg,
				Fax, Work, Cell, Modem, Isdn, Video)
    end;
decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Number, Pager, Pcs, Bbs, Voice, Home, Pref,
		     Msg, Fax, Work, Cell, Modem, Isdn, Video) ->
    decode_vcard_TEL_els(__TopXMLNS, __IgnoreEls, _els,
			 Number, Pager, Pcs, Bbs, Voice, Home, Pref, Msg, Fax,
			 Work, Cell, Modem, Isdn, Video).

encode_vcard_TEL({vcard_tel, Home, Work, Voice, Fax,
		  Pager, Msg, Cell, Video, Bbs, Modem, Isdn, Pcs, Pref,
		  Number},
		 _xmlns_attrs) ->
    _els = lists:reverse('encode_vcard_TEL_$number'(Number,
						    'encode_vcard_TEL_$pager'(Pager,
									      'encode_vcard_TEL_$pcs'(Pcs,
												      'encode_vcard_TEL_$bbs'(Bbs,
															      'encode_vcard_TEL_$voice'(Voice,
																			'encode_vcard_TEL_$home'(Home,
																						 'encode_vcard_TEL_$pref'(Pref,
																									  'encode_vcard_TEL_$msg'(Msg,
																												  'encode_vcard_TEL_$fax'(Fax,
																															  'encode_vcard_TEL_$work'(Work,
																																		   'encode_vcard_TEL_$cell'(Cell,
																																					    'encode_vcard_TEL_$modem'(Modem,
																																								      'encode_vcard_TEL_$isdn'(Isdn,
																																											       'encode_vcard_TEL_$video'(Video,
																																															 []))))))))))))))),
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

decode_vcard_LABEL(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"LABEL">>, _attrs, _els}) ->
    {Line, Home, Pref, Work, Intl, Parcel, Postal, Dom} =
	decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
			       [], false, false, false, false, false, false,
			       false),
    {vcard_label, Home, Work, Postal, Parcel, Dom, Intl,
     Pref, Line}.

decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, [],
		       Line, Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    {lists:reverse(Line), Home, Pref, Work, Intl, Parcel,
     Postal, Dom};
decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"HOME">>, _attrs, _} = _el | _els], Line,
		       Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  Line,
				  decode_vcard_HOME(__TopXMLNS, __IgnoreEls,
						    _el),
				  Pref, Work, Intl, Parcel, Postal, Dom);
       true ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  Line, Home, Pref, Work, Intl, Parcel, Postal,
				  Dom)
    end;
decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"WORK">>, _attrs, _} = _el | _els], Line,
		       Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  Line, Home, Pref,
				  decode_vcard_WORK(__TopXMLNS, __IgnoreEls,
						    _el),
				  Intl, Parcel, Postal, Dom);
       true ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  Line, Home, Pref, Work, Intl, Parcel, Postal,
				  Dom)
    end;
decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"POSTAL">>, _attrs, _} = _el | _els], Line,
		       Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  Line, Home, Pref, Work, Intl, Parcel,
				  decode_vcard_POSTAL(__TopXMLNS, __IgnoreEls,
						      _el),
				  Dom);
       true ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  Line, Home, Pref, Work, Intl, Parcel, Postal,
				  Dom)
    end;
decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"PARCEL">>, _attrs, _} = _el | _els], Line,
		       Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  Line, Home, Pref, Work, Intl,
				  decode_vcard_PARCEL(__TopXMLNS, __IgnoreEls,
						      _el),
				  Postal, Dom);
       true ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  Line, Home, Pref, Work, Intl, Parcel, Postal,
				  Dom)
    end;
decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"DOM">>, _attrs, _} = _el | _els], Line,
		       Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  Line, Home, Pref, Work, Intl, Parcel, Postal,
				  decode_vcard_DOM(__TopXMLNS, __IgnoreEls,
						   _el));
       true ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  Line, Home, Pref, Work, Intl, Parcel, Postal,
				  Dom)
    end;
decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"INTL">>, _attrs, _} = _el | _els], Line,
		       Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  Line, Home, Pref, Work,
				  decode_vcard_INTL(__TopXMLNS, __IgnoreEls,
						    _el),
				  Parcel, Postal, Dom);
       true ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  Line, Home, Pref, Work, Intl, Parcel, Postal,
				  Dom)
    end;
decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"PREF">>, _attrs, _} = _el | _els], Line,
		       Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  Line, Home,
				  decode_vcard_PREF(__TopXMLNS, __IgnoreEls,
						    _el),
				  Work, Intl, Parcel, Postal, Dom);
       true ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  Line, Home, Pref, Work, Intl, Parcel, Postal,
				  Dom)
    end;
decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"LINE">>, _attrs, _} = _el | _els], Line,
		       Home, Pref, Work, Intl, Parcel, Postal, Dom) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  case decode_vcard_LINE(__TopXMLNS,
							 __IgnoreEls, _el)
				      of
				    undefined -> Line;
				    _new_el -> [_new_el | Line]
				  end,
				  Home, Pref, Work, Intl, Parcel, Postal, Dom);
       true ->
	   decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
				  Line, Home, Pref, Work, Intl, Parcel, Postal,
				  Dom)
    end;
decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Line, Home, Pref, Work, Intl, Parcel,
		       Postal, Dom) ->
    decode_vcard_LABEL_els(__TopXMLNS, __IgnoreEls, _els,
			   Line, Home, Pref, Work, Intl, Parcel, Postal, Dom).

encode_vcard_LABEL({vcard_label, Home, Work, Postal,
		    Parcel, Dom, Intl, Pref, Line},
		   _xmlns_attrs) ->
    _els = lists:reverse('encode_vcard_LABEL_$line'(Line,
						    'encode_vcard_LABEL_$home'(Home,
									       'encode_vcard_LABEL_$pref'(Pref,
													  'encode_vcard_LABEL_$work'(Work,
																     'encode_vcard_LABEL_$intl'(Intl,
																				'encode_vcard_LABEL_$parcel'(Parcel,
																							     'encode_vcard_LABEL_$postal'(Postal,
																											  'encode_vcard_LABEL_$dom'(Dom,
																														    []))))))))),
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

decode_vcard_ADR(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"ADR">>, _attrs, _els}) ->
    {Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
     Locality, Work, Intl, Parcel, Postal, Dom, Region} =
	decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
			     undefined, undefined, undefined, false, false,
			     undefined, undefined, undefined, false, false,
			     false, false, false, undefined),
    {vcard_adr, Home, Work, Postal, Parcel, Dom, Intl, Pref,
     Pobox, Extadd, Street, Locality, Region, Pcode, Ctry}.

decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, [],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    {Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
     Locality, Work, Intl, Parcel, Postal, Dom, Region};
decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"HOME">>, _attrs, _} = _el | _els], Street,
		     Extadd, Pcode, Home, Pref, Pobox, Ctry, Locality, Work,
		     Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode,
				decode_vcard_HOME(__TopXMLNS, __IgnoreEls, _el),
				Pref, Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region);
       true ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region)
    end;
decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"WORK">>, _attrs, _} = _el | _els], Street,
		     Extadd, Pcode, Home, Pref, Pobox, Ctry, Locality, Work,
		     Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality,
				decode_vcard_WORK(__TopXMLNS, __IgnoreEls, _el),
				Intl, Parcel, Postal, Dom, Region);
       true ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region)
    end;
decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"POSTAL">>, _attrs, _} = _el | _els], Street,
		     Extadd, Pcode, Home, Pref, Pobox, Ctry, Locality, Work,
		     Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel,
				decode_vcard_POSTAL(__TopXMLNS, __IgnoreEls,
						    _el),
				Dom, Region);
       true ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region)
    end;
decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"PARCEL">>, _attrs, _} = _el | _els], Street,
		     Extadd, Pcode, Home, Pref, Pobox, Ctry, Locality, Work,
		     Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl,
				decode_vcard_PARCEL(__TopXMLNS, __IgnoreEls,
						    _el),
				Postal, Dom, Region);
       true ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region)
    end;
decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"DOM">>, _attrs, _} = _el | _els], Street,
		     Extadd, Pcode, Home, Pref, Pobox, Ctry, Locality, Work,
		     Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal,
				decode_vcard_DOM(__TopXMLNS, __IgnoreEls, _el),
				Region);
       true ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region)
    end;
decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"INTL">>, _attrs, _} = _el | _els], Street,
		     Extadd, Pcode, Home, Pref, Pobox, Ctry, Locality, Work,
		     Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work,
				decode_vcard_INTL(__TopXMLNS, __IgnoreEls, _el),
				Parcel, Postal, Dom, Region);
       true ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region)
    end;
decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"PREF">>, _attrs, _} = _el | _els], Street,
		     Extadd, Pcode, Home, Pref, Pobox, Ctry, Locality, Work,
		     Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home,
				decode_vcard_PREF(__TopXMLNS, __IgnoreEls, _el),
				Pobox, Ctry, Locality, Work, Intl, Parcel,
				Postal, Dom, Region);
       true ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region)
    end;
decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"POBOX">>, _attrs, _} = _el | _els], Street,
		     Extadd, Pcode, Home, Pref, Pobox, Ctry, Locality, Work,
		     Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref,
				decode_vcard_POBOX(__TopXMLNS, __IgnoreEls,
						   _el),
				Ctry, Locality, Work, Intl, Parcel, Postal, Dom,
				Region);
       true ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region)
    end;
decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"EXTADD">>, _attrs, _} = _el | _els], Street,
		     Extadd, Pcode, Home, Pref, Pobox, Ctry, Locality, Work,
		     Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street,
				decode_vcard_EXTADD(__TopXMLNS, __IgnoreEls,
						    _el),
				Pcode, Home, Pref, Pobox, Ctry, Locality, Work,
				Intl, Parcel, Postal, Dom, Region);
       true ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region)
    end;
decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"STREET">>, _attrs, _} = _el | _els], Street,
		     Extadd, Pcode, Home, Pref, Pobox, Ctry, Locality, Work,
		     Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				decode_vcard_STREET(__TopXMLNS, __IgnoreEls,
						    _el),
				Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region);
       true ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region)
    end;
decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"LOCALITY">>, _attrs, _} = _el | _els],
		     Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
		     Locality, Work, Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				decode_vcard_LOCALITY(__TopXMLNS, __IgnoreEls,
						      _el),
				Work, Intl, Parcel, Postal, Dom, Region);
       true ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region)
    end;
decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"REGION">>, _attrs, _} = _el | _els], Street,
		     Extadd, Pcode, Home, Pref, Pobox, Ctry, Locality, Work,
		     Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				decode_vcard_REGION(__TopXMLNS, __IgnoreEls,
						    _el));
       true ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region)
    end;
decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"PCODE">>, _attrs, _} = _el | _els], Street,
		     Extadd, Pcode, Home, Pref, Pobox, Ctry, Locality, Work,
		     Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd,
				decode_vcard_PCODE(__TopXMLNS, __IgnoreEls,
						   _el),
				Home, Pref, Pobox, Ctry, Locality, Work, Intl,
				Parcel, Postal, Dom, Region);
       true ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region)
    end;
decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls,
		     [{xmlel, <<"CTRY">>, _attrs, _} = _el | _els], Street,
		     Extadd, Pcode, Home, Pref, Pobox, Ctry, Locality, Work,
		     Intl, Parcel, Postal, Dom, Region) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox,
				decode_vcard_CTRY(__TopXMLNS, __IgnoreEls, _el),
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region);
       true ->
	   decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
				Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
				Locality, Work, Intl, Parcel, Postal, Dom,
				Region)
    end;
decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Street, Extadd, Pcode, Home, Pref, Pobox,
		     Ctry, Locality, Work, Intl, Parcel, Postal, Dom,
		     Region) ->
    decode_vcard_ADR_els(__TopXMLNS, __IgnoreEls, _els,
			 Street, Extadd, Pcode, Home, Pref, Pobox, Ctry,
			 Locality, Work, Intl, Parcel, Postal, Dom, Region).

encode_vcard_ADR({vcard_adr, Home, Work, Postal, Parcel,
		  Dom, Intl, Pref, Pobox, Extadd, Street, Locality,
		  Region, Pcode, Ctry},
		 _xmlns_attrs) ->
    _els = lists:reverse('encode_vcard_ADR_$street'(Street,
						    'encode_vcard_ADR_$extadd'(Extadd,
									       'encode_vcard_ADR_$pcode'(Pcode,
													 'encode_vcard_ADR_$home'(Home,
																  'encode_vcard_ADR_$pref'(Pref,
																			   'encode_vcard_ADR_$pobox'(Pobox,
																						     'encode_vcard_ADR_$ctry'(Ctry,
																									      'encode_vcard_ADR_$locality'(Locality,
																													   'encode_vcard_ADR_$work'(Work,
																																    'encode_vcard_ADR_$intl'(Intl,
																																			     'encode_vcard_ADR_$parcel'(Parcel,
																																							'encode_vcard_ADR_$postal'(Postal,
																																										   'encode_vcard_ADR_$dom'(Dom,
																																													   'encode_vcard_ADR_$region'(Region,
																																																      []))))))))))))))),
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

decode_vcard_N(__TopXMLNS, __IgnoreEls,
	       {xmlel, <<"N">>, _attrs, _els}) ->
    {Middle, Suffix, Prefix, Family, Given} =
	decode_vcard_N_els(__TopXMLNS, __IgnoreEls, _els,
			   undefined, undefined, undefined, undefined,
			   undefined),
    {vcard_name, Family, Given, Middle, Prefix, Suffix}.

decode_vcard_N_els(__TopXMLNS, __IgnoreEls, [], Middle,
		   Suffix, Prefix, Family, Given) ->
    {Middle, Suffix, Prefix, Family, Given};
decode_vcard_N_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"FAMILY">>, _attrs, _} = _el | _els], Middle,
		   Suffix, Prefix, Family, Given) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_N_els(__TopXMLNS, __IgnoreEls, _els,
			      Middle, Suffix, Prefix,
			      decode_vcard_FAMILY(__TopXMLNS, __IgnoreEls, _el),
			      Given);
       true ->
	   decode_vcard_N_els(__TopXMLNS, __IgnoreEls, _els,
			      Middle, Suffix, Prefix, Family, Given)
    end;
decode_vcard_N_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"GIVEN">>, _attrs, _} = _el | _els], Middle,
		   Suffix, Prefix, Family, Given) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_N_els(__TopXMLNS, __IgnoreEls, _els,
			      Middle, Suffix, Prefix, Family,
			      decode_vcard_GIVEN(__TopXMLNS, __IgnoreEls, _el));
       true ->
	   decode_vcard_N_els(__TopXMLNS, __IgnoreEls, _els,
			      Middle, Suffix, Prefix, Family, Given)
    end;
decode_vcard_N_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"MIDDLE">>, _attrs, _} = _el | _els], Middle,
		   Suffix, Prefix, Family, Given) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_N_els(__TopXMLNS, __IgnoreEls, _els,
			      decode_vcard_MIDDLE(__TopXMLNS, __IgnoreEls, _el),
			      Suffix, Prefix, Family, Given);
       true ->
	   decode_vcard_N_els(__TopXMLNS, __IgnoreEls, _els,
			      Middle, Suffix, Prefix, Family, Given)
    end;
decode_vcard_N_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"PREFIX">>, _attrs, _} = _el | _els], Middle,
		   Suffix, Prefix, Family, Given) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_N_els(__TopXMLNS, __IgnoreEls, _els,
			      Middle, Suffix,
			      decode_vcard_PREFIX(__TopXMLNS, __IgnoreEls, _el),
			      Family, Given);
       true ->
	   decode_vcard_N_els(__TopXMLNS, __IgnoreEls, _els,
			      Middle, Suffix, Prefix, Family, Given)
    end;
decode_vcard_N_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"SUFFIX">>, _attrs, _} = _el | _els], Middle,
		   Suffix, Prefix, Family, Given) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_vcard_N_els(__TopXMLNS, __IgnoreEls, _els,
			      Middle,
			      decode_vcard_SUFFIX(__TopXMLNS, __IgnoreEls, _el),
			      Prefix, Family, Given);
       true ->
	   decode_vcard_N_els(__TopXMLNS, __IgnoreEls, _els,
			      Middle, Suffix, Prefix, Family, Given)
    end;
decode_vcard_N_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		   Middle, Suffix, Prefix, Family, Given) ->
    decode_vcard_N_els(__TopXMLNS, __IgnoreEls, _els,
		       Middle, Suffix, Prefix, Family, Given).

encode_vcard_N({vcard_name, Family, Given, Middle,
		Prefix, Suffix},
	       _xmlns_attrs) ->
    _els = lists:reverse('encode_vcard_N_$middle'(Middle,
						  'encode_vcard_N_$suffix'(Suffix,
									   'encode_vcard_N_$prefix'(Prefix,
												    'encode_vcard_N_$family'(Family,
															     'encode_vcard_N_$given'(Given,
																		     [])))))),
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

decode_vcard_CONFIDENTIAL(__TopXMLNS, __IgnoreEls,
			  {xmlel, <<"CONFIDENTIAL">>, _attrs, _els}) ->
    confidential.

encode_vcard_CONFIDENTIAL(confidential, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"CONFIDENTIAL">>, _attrs, _els}.

decode_vcard_PRIVATE(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"PRIVATE">>, _attrs, _els}) ->
    private.

encode_vcard_PRIVATE(private, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"PRIVATE">>, _attrs, _els}.

decode_vcard_PUBLIC(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"PUBLIC">>, _attrs, _els}) ->
    public.

encode_vcard_PUBLIC(public, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"PUBLIC">>, _attrs, _els}.

decode_vcard_EXTVAL(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"EXTVAL">>, _attrs, _els}) ->
    Cdata = decode_vcard_EXTVAL_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_vcard_EXTVAL_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_vcard_EXTVAL_cdata(__TopXMLNS, Cdata);
decode_vcard_EXTVAL_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_EXTVAL_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_EXTVAL_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_vcard_EXTVAL_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_vcard_EXTVAL(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_EXTVAL_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"EXTVAL">>, _attrs, _els}.

decode_vcard_EXTVAL_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_EXTVAL_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_EXTVAL_cdata(undefined, _acc) -> _acc;
encode_vcard_EXTVAL_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_TYPE(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"TYPE">>, _attrs, _els}) ->
    Cdata = decode_vcard_TYPE_els(__TopXMLNS, __IgnoreEls,
				  _els, <<>>),
    Cdata.

decode_vcard_TYPE_els(__TopXMLNS, __IgnoreEls, [],
		      Cdata) ->
    decode_vcard_TYPE_cdata(__TopXMLNS, Cdata);
decode_vcard_TYPE_els(__TopXMLNS, __IgnoreEls,
		      [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_TYPE_els(__TopXMLNS, __IgnoreEls, _els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_TYPE_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Cdata) ->
    decode_vcard_TYPE_els(__TopXMLNS, __IgnoreEls, _els,
			  Cdata).

encode_vcard_TYPE(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_TYPE_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"TYPE">>, _attrs, _els}.

decode_vcard_TYPE_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_TYPE_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_TYPE_cdata(undefined, _acc) -> _acc;
encode_vcard_TYPE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_DESC(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"DESC">>, _attrs, _els}) ->
    Cdata = decode_vcard_DESC_els(__TopXMLNS, __IgnoreEls,
				  _els, <<>>),
    Cdata.

decode_vcard_DESC_els(__TopXMLNS, __IgnoreEls, [],
		      Cdata) ->
    decode_vcard_DESC_cdata(__TopXMLNS, Cdata);
decode_vcard_DESC_els(__TopXMLNS, __IgnoreEls,
		      [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_DESC_els(__TopXMLNS, __IgnoreEls, _els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_DESC_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Cdata) ->
    decode_vcard_DESC_els(__TopXMLNS, __IgnoreEls, _els,
			  Cdata).

encode_vcard_DESC(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_DESC_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"DESC">>, _attrs, _els}.

decode_vcard_DESC_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_DESC_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_DESC_cdata(undefined, _acc) -> _acc;
encode_vcard_DESC_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_URL(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"URL">>, _attrs, _els}) ->
    Cdata = decode_vcard_URL_els(__TopXMLNS, __IgnoreEls,
				 _els, <<>>),
    Cdata.

decode_vcard_URL_els(__TopXMLNS, __IgnoreEls, [],
		     Cdata) ->
    decode_vcard_URL_cdata(__TopXMLNS, Cdata);
decode_vcard_URL_els(__TopXMLNS, __IgnoreEls,
		     [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_URL_els(__TopXMLNS, __IgnoreEls, _els,
			 <<Cdata/binary, _data/binary>>);
decode_vcard_URL_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Cdata) ->
    decode_vcard_URL_els(__TopXMLNS, __IgnoreEls, _els,
			 Cdata).

encode_vcard_URL(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_URL_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"URL">>, _attrs, _els}.

decode_vcard_URL_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_URL_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_URL_cdata(undefined, _acc) -> _acc;
encode_vcard_URL_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_UID(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"UID">>, _attrs, _els}) ->
    Cdata = decode_vcard_UID_els(__TopXMLNS, __IgnoreEls,
				 _els, <<>>),
    Cdata.

decode_vcard_UID_els(__TopXMLNS, __IgnoreEls, [],
		     Cdata) ->
    decode_vcard_UID_cdata(__TopXMLNS, Cdata);
decode_vcard_UID_els(__TopXMLNS, __IgnoreEls,
		     [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_UID_els(__TopXMLNS, __IgnoreEls, _els,
			 <<Cdata/binary, _data/binary>>);
decode_vcard_UID_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Cdata) ->
    decode_vcard_UID_els(__TopXMLNS, __IgnoreEls, _els,
			 Cdata).

encode_vcard_UID(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_UID_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"UID">>, _attrs, _els}.

decode_vcard_UID_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_UID_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_UID_cdata(undefined, _acc) -> _acc;
encode_vcard_UID_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_SORT_STRING(__TopXMLNS, __IgnoreEls,
			 {xmlel, <<"SORT-STRING">>, _attrs, _els}) ->
    Cdata = decode_vcard_SORT_STRING_els(__TopXMLNS,
					 __IgnoreEls, _els, <<>>),
    Cdata.

decode_vcard_SORT_STRING_els(__TopXMLNS, __IgnoreEls,
			     [], Cdata) ->
    decode_vcard_SORT_STRING_cdata(__TopXMLNS, Cdata);
decode_vcard_SORT_STRING_els(__TopXMLNS, __IgnoreEls,
			     [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_SORT_STRING_els(__TopXMLNS, __IgnoreEls,
				 _els, <<Cdata/binary, _data/binary>>);
decode_vcard_SORT_STRING_els(__TopXMLNS, __IgnoreEls,
			     [_ | _els], Cdata) ->
    decode_vcard_SORT_STRING_els(__TopXMLNS, __IgnoreEls,
				 _els, Cdata).

encode_vcard_SORT_STRING(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_SORT_STRING_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"SORT-STRING">>, _attrs, _els}.

decode_vcard_SORT_STRING_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_SORT_STRING_cdata(__TopXMLNS, _val) ->
    _val.

encode_vcard_SORT_STRING_cdata(undefined, _acc) -> _acc;
encode_vcard_SORT_STRING_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_REV(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"REV">>, _attrs, _els}) ->
    Cdata = decode_vcard_REV_els(__TopXMLNS, __IgnoreEls,
				 _els, <<>>),
    Cdata.

decode_vcard_REV_els(__TopXMLNS, __IgnoreEls, [],
		     Cdata) ->
    decode_vcard_REV_cdata(__TopXMLNS, Cdata);
decode_vcard_REV_els(__TopXMLNS, __IgnoreEls,
		     [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_REV_els(__TopXMLNS, __IgnoreEls, _els,
			 <<Cdata/binary, _data/binary>>);
decode_vcard_REV_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Cdata) ->
    decode_vcard_REV_els(__TopXMLNS, __IgnoreEls, _els,
			 Cdata).

encode_vcard_REV(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_REV_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"REV">>, _attrs, _els}.

decode_vcard_REV_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_REV_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_REV_cdata(undefined, _acc) -> _acc;
encode_vcard_REV_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_PRODID(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"PRODID">>, _attrs, _els}) ->
    Cdata = decode_vcard_PRODID_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_vcard_PRODID_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_vcard_PRODID_cdata(__TopXMLNS, Cdata);
decode_vcard_PRODID_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_PRODID_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_PRODID_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_vcard_PRODID_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_vcard_PRODID(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_PRODID_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"PRODID">>, _attrs, _els}.

decode_vcard_PRODID_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_PRODID_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_PRODID_cdata(undefined, _acc) -> _acc;
encode_vcard_PRODID_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_NOTE(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"NOTE">>, _attrs, _els}) ->
    Cdata = decode_vcard_NOTE_els(__TopXMLNS, __IgnoreEls,
				  _els, <<>>),
    Cdata.

decode_vcard_NOTE_els(__TopXMLNS, __IgnoreEls, [],
		      Cdata) ->
    decode_vcard_NOTE_cdata(__TopXMLNS, Cdata);
decode_vcard_NOTE_els(__TopXMLNS, __IgnoreEls,
		      [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_NOTE_els(__TopXMLNS, __IgnoreEls, _els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_NOTE_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Cdata) ->
    decode_vcard_NOTE_els(__TopXMLNS, __IgnoreEls, _els,
			  Cdata).

encode_vcard_NOTE(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_NOTE_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"NOTE">>, _attrs, _els}.

decode_vcard_NOTE_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_NOTE_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_NOTE_cdata(undefined, _acc) -> _acc;
encode_vcard_NOTE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_KEYWORD(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"KEYWORD">>, _attrs, _els}) ->
    Cdata = decode_vcard_KEYWORD_els(__TopXMLNS,
				     __IgnoreEls, _els, <<>>),
    Cdata.

decode_vcard_KEYWORD_els(__TopXMLNS, __IgnoreEls, [],
			 Cdata) ->
    decode_vcard_KEYWORD_cdata(__TopXMLNS, Cdata);
decode_vcard_KEYWORD_els(__TopXMLNS, __IgnoreEls,
			 [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_KEYWORD_els(__TopXMLNS, __IgnoreEls, _els,
			     <<Cdata/binary, _data/binary>>);
decode_vcard_KEYWORD_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Cdata) ->
    decode_vcard_KEYWORD_els(__TopXMLNS, __IgnoreEls, _els,
			     Cdata).

encode_vcard_KEYWORD(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_KEYWORD_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"KEYWORD">>, _attrs, _els}.

decode_vcard_KEYWORD_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_KEYWORD_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_KEYWORD_cdata(undefined, _acc) -> _acc;
encode_vcard_KEYWORD_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_ROLE(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"ROLE">>, _attrs, _els}) ->
    Cdata = decode_vcard_ROLE_els(__TopXMLNS, __IgnoreEls,
				  _els, <<>>),
    Cdata.

decode_vcard_ROLE_els(__TopXMLNS, __IgnoreEls, [],
		      Cdata) ->
    decode_vcard_ROLE_cdata(__TopXMLNS, Cdata);
decode_vcard_ROLE_els(__TopXMLNS, __IgnoreEls,
		      [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_ROLE_els(__TopXMLNS, __IgnoreEls, _els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_ROLE_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Cdata) ->
    decode_vcard_ROLE_els(__TopXMLNS, __IgnoreEls, _els,
			  Cdata).

encode_vcard_ROLE(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_ROLE_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"ROLE">>, _attrs, _els}.

decode_vcard_ROLE_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_ROLE_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_ROLE_cdata(undefined, _acc) -> _acc;
encode_vcard_ROLE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_TITLE(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"TITLE">>, _attrs, _els}) ->
    Cdata = decode_vcard_TITLE_els(__TopXMLNS, __IgnoreEls,
				   _els, <<>>),
    Cdata.

decode_vcard_TITLE_els(__TopXMLNS, __IgnoreEls, [],
		       Cdata) ->
    decode_vcard_TITLE_cdata(__TopXMLNS, Cdata);
decode_vcard_TITLE_els(__TopXMLNS, __IgnoreEls,
		       [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_TITLE_els(__TopXMLNS, __IgnoreEls, _els,
			   <<Cdata/binary, _data/binary>>);
decode_vcard_TITLE_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Cdata) ->
    decode_vcard_TITLE_els(__TopXMLNS, __IgnoreEls, _els,
			   Cdata).

encode_vcard_TITLE(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_TITLE_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"TITLE">>, _attrs, _els}.

decode_vcard_TITLE_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_TITLE_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_TITLE_cdata(undefined, _acc) -> _acc;
encode_vcard_TITLE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_TZ(__TopXMLNS, __IgnoreEls,
		{xmlel, <<"TZ">>, _attrs, _els}) ->
    Cdata = decode_vcard_TZ_els(__TopXMLNS, __IgnoreEls,
				_els, <<>>),
    Cdata.

decode_vcard_TZ_els(__TopXMLNS, __IgnoreEls, [],
		    Cdata) ->
    decode_vcard_TZ_cdata(__TopXMLNS, Cdata);
decode_vcard_TZ_els(__TopXMLNS, __IgnoreEls,
		    [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_TZ_els(__TopXMLNS, __IgnoreEls, _els,
			<<Cdata/binary, _data/binary>>);
decode_vcard_TZ_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		    Cdata) ->
    decode_vcard_TZ_els(__TopXMLNS, __IgnoreEls, _els,
			Cdata).

encode_vcard_TZ(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_TZ_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"TZ">>, _attrs, _els}.

decode_vcard_TZ_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_TZ_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_TZ_cdata(undefined, _acc) -> _acc;
encode_vcard_TZ_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_MAILER(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"MAILER">>, _attrs, _els}) ->
    Cdata = decode_vcard_MAILER_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_vcard_MAILER_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_vcard_MAILER_cdata(__TopXMLNS, Cdata);
decode_vcard_MAILER_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_MAILER_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_MAILER_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_vcard_MAILER_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_vcard_MAILER(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_MAILER_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"MAILER">>, _attrs, _els}.

decode_vcard_MAILER_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_MAILER_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_MAILER_cdata(undefined, _acc) -> _acc;
encode_vcard_MAILER_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_JABBERID(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"JABBERID">>, _attrs, _els}) ->
    Cdata = decode_vcard_JABBERID_els(__TopXMLNS,
				      __IgnoreEls, _els, <<>>),
    Cdata.

decode_vcard_JABBERID_els(__TopXMLNS, __IgnoreEls, [],
			  Cdata) ->
    decode_vcard_JABBERID_cdata(__TopXMLNS, Cdata);
decode_vcard_JABBERID_els(__TopXMLNS, __IgnoreEls,
			  [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_JABBERID_els(__TopXMLNS, __IgnoreEls, _els,
			      <<Cdata/binary, _data/binary>>);
decode_vcard_JABBERID_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Cdata) ->
    decode_vcard_JABBERID_els(__TopXMLNS, __IgnoreEls, _els,
			      Cdata).

encode_vcard_JABBERID(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_JABBERID_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"JABBERID">>, _attrs, _els}.

decode_vcard_JABBERID_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_JABBERID_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_JABBERID_cdata(undefined, _acc) -> _acc;
encode_vcard_JABBERID_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_BDAY(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"BDAY">>, _attrs, _els}) ->
    Cdata = decode_vcard_BDAY_els(__TopXMLNS, __IgnoreEls,
				  _els, <<>>),
    Cdata.

decode_vcard_BDAY_els(__TopXMLNS, __IgnoreEls, [],
		      Cdata) ->
    decode_vcard_BDAY_cdata(__TopXMLNS, Cdata);
decode_vcard_BDAY_els(__TopXMLNS, __IgnoreEls,
		      [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_BDAY_els(__TopXMLNS, __IgnoreEls, _els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_BDAY_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Cdata) ->
    decode_vcard_BDAY_els(__TopXMLNS, __IgnoreEls, _els,
			  Cdata).

encode_vcard_BDAY(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_BDAY_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"BDAY">>, _attrs, _els}.

decode_vcard_BDAY_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_BDAY_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_BDAY_cdata(undefined, _acc) -> _acc;
encode_vcard_BDAY_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_NICKNAME(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"NICKNAME">>, _attrs, _els}) ->
    Cdata = decode_vcard_NICKNAME_els(__TopXMLNS,
				      __IgnoreEls, _els, <<>>),
    Cdata.

decode_vcard_NICKNAME_els(__TopXMLNS, __IgnoreEls, [],
			  Cdata) ->
    decode_vcard_NICKNAME_cdata(__TopXMLNS, Cdata);
decode_vcard_NICKNAME_els(__TopXMLNS, __IgnoreEls,
			  [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_NICKNAME_els(__TopXMLNS, __IgnoreEls, _els,
			      <<Cdata/binary, _data/binary>>);
decode_vcard_NICKNAME_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Cdata) ->
    decode_vcard_NICKNAME_els(__TopXMLNS, __IgnoreEls, _els,
			      Cdata).

encode_vcard_NICKNAME(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_NICKNAME_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"NICKNAME">>, _attrs, _els}.

decode_vcard_NICKNAME_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_NICKNAME_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_NICKNAME_cdata(undefined, _acc) -> _acc;
encode_vcard_NICKNAME_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_FN(__TopXMLNS, __IgnoreEls,
		{xmlel, <<"FN">>, _attrs, _els}) ->
    Cdata = decode_vcard_FN_els(__TopXMLNS, __IgnoreEls,
				_els, <<>>),
    Cdata.

decode_vcard_FN_els(__TopXMLNS, __IgnoreEls, [],
		    Cdata) ->
    decode_vcard_FN_cdata(__TopXMLNS, Cdata);
decode_vcard_FN_els(__TopXMLNS, __IgnoreEls,
		    [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_FN_els(__TopXMLNS, __IgnoreEls, _els,
			<<Cdata/binary, _data/binary>>);
decode_vcard_FN_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		    Cdata) ->
    decode_vcard_FN_els(__TopXMLNS, __IgnoreEls, _els,
			Cdata).

encode_vcard_FN(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_FN_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"FN">>, _attrs, _els}.

decode_vcard_FN_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_FN_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_FN_cdata(undefined, _acc) -> _acc;
encode_vcard_FN_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_VERSION(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"VERSION">>, _attrs, _els}) ->
    Cdata = decode_vcard_VERSION_els(__TopXMLNS,
				     __IgnoreEls, _els, <<>>),
    Cdata.

decode_vcard_VERSION_els(__TopXMLNS, __IgnoreEls, [],
			 Cdata) ->
    decode_vcard_VERSION_cdata(__TopXMLNS, Cdata);
decode_vcard_VERSION_els(__TopXMLNS, __IgnoreEls,
			 [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_VERSION_els(__TopXMLNS, __IgnoreEls, _els,
			     <<Cdata/binary, _data/binary>>);
decode_vcard_VERSION_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Cdata) ->
    decode_vcard_VERSION_els(__TopXMLNS, __IgnoreEls, _els,
			     Cdata).

encode_vcard_VERSION(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_VERSION_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"VERSION">>, _attrs, _els}.

decode_vcard_VERSION_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_VERSION_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_VERSION_cdata(undefined, _acc) -> _acc;
encode_vcard_VERSION_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_CRED(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"CRED">>, _attrs, _els}) ->
    Cdata = decode_vcard_CRED_els(__TopXMLNS, __IgnoreEls,
				  _els, <<>>),
    Cdata.

decode_vcard_CRED_els(__TopXMLNS, __IgnoreEls, [],
		      Cdata) ->
    decode_vcard_CRED_cdata(__TopXMLNS, Cdata);
decode_vcard_CRED_els(__TopXMLNS, __IgnoreEls,
		      [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_CRED_els(__TopXMLNS, __IgnoreEls, _els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_CRED_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Cdata) ->
    decode_vcard_CRED_els(__TopXMLNS, __IgnoreEls, _els,
			  Cdata).

encode_vcard_CRED(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_CRED_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"CRED">>, _attrs, _els}.

decode_vcard_CRED_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_CRED_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_CRED_cdata(undefined, _acc) -> _acc;
encode_vcard_CRED_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_PHONETIC(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"PHONETIC">>, _attrs, _els}) ->
    Cdata = decode_vcard_PHONETIC_els(__TopXMLNS,
				      __IgnoreEls, _els, <<>>),
    Cdata.

decode_vcard_PHONETIC_els(__TopXMLNS, __IgnoreEls, [],
			  Cdata) ->
    decode_vcard_PHONETIC_cdata(__TopXMLNS, Cdata);
decode_vcard_PHONETIC_els(__TopXMLNS, __IgnoreEls,
			  [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_PHONETIC_els(__TopXMLNS, __IgnoreEls, _els,
			      <<Cdata/binary, _data/binary>>);
decode_vcard_PHONETIC_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Cdata) ->
    decode_vcard_PHONETIC_els(__TopXMLNS, __IgnoreEls, _els,
			      Cdata).

encode_vcard_PHONETIC(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_PHONETIC_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"PHONETIC">>, _attrs, _els}.

decode_vcard_PHONETIC_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_PHONETIC_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_PHONETIC_cdata(undefined, _acc) -> _acc;
encode_vcard_PHONETIC_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_ORGUNIT(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"ORGUNIT">>, _attrs, _els}) ->
    Cdata = decode_vcard_ORGUNIT_els(__TopXMLNS,
				     __IgnoreEls, _els, <<>>),
    Cdata.

decode_vcard_ORGUNIT_els(__TopXMLNS, __IgnoreEls, [],
			 Cdata) ->
    decode_vcard_ORGUNIT_cdata(__TopXMLNS, Cdata);
decode_vcard_ORGUNIT_els(__TopXMLNS, __IgnoreEls,
			 [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_ORGUNIT_els(__TopXMLNS, __IgnoreEls, _els,
			     <<Cdata/binary, _data/binary>>);
decode_vcard_ORGUNIT_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Cdata) ->
    decode_vcard_ORGUNIT_els(__TopXMLNS, __IgnoreEls, _els,
			     Cdata).

encode_vcard_ORGUNIT(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_ORGUNIT_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"ORGUNIT">>, _attrs, _els}.

decode_vcard_ORGUNIT_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_ORGUNIT_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_ORGUNIT_cdata(undefined, _acc) -> _acc;
encode_vcard_ORGUNIT_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_ORGNAME(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"ORGNAME">>, _attrs, _els}) ->
    Cdata = decode_vcard_ORGNAME_els(__TopXMLNS,
				     __IgnoreEls, _els, <<>>),
    Cdata.

decode_vcard_ORGNAME_els(__TopXMLNS, __IgnoreEls, [],
			 Cdata) ->
    decode_vcard_ORGNAME_cdata(__TopXMLNS, Cdata);
decode_vcard_ORGNAME_els(__TopXMLNS, __IgnoreEls,
			 [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_ORGNAME_els(__TopXMLNS, __IgnoreEls, _els,
			     <<Cdata/binary, _data/binary>>);
decode_vcard_ORGNAME_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Cdata) ->
    decode_vcard_ORGNAME_els(__TopXMLNS, __IgnoreEls, _els,
			     Cdata).

encode_vcard_ORGNAME(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_ORGNAME_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"ORGNAME">>, _attrs, _els}.

decode_vcard_ORGNAME_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_ORGNAME_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_ORGNAME_cdata(undefined, _acc) -> _acc;
encode_vcard_ORGNAME_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_LON(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"LON">>, _attrs, _els}) ->
    Cdata = decode_vcard_LON_els(__TopXMLNS, __IgnoreEls,
				 _els, <<>>),
    Cdata.

decode_vcard_LON_els(__TopXMLNS, __IgnoreEls, [],
		     Cdata) ->
    decode_vcard_LON_cdata(__TopXMLNS, Cdata);
decode_vcard_LON_els(__TopXMLNS, __IgnoreEls,
		     [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_LON_els(__TopXMLNS, __IgnoreEls, _els,
			 <<Cdata/binary, _data/binary>>);
decode_vcard_LON_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Cdata) ->
    decode_vcard_LON_els(__TopXMLNS, __IgnoreEls, _els,
			 Cdata).

encode_vcard_LON(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_LON_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"LON">>, _attrs, _els}.

decode_vcard_LON_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_LON_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_LON_cdata(undefined, _acc) -> _acc;
encode_vcard_LON_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_LAT(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"LAT">>, _attrs, _els}) ->
    Cdata = decode_vcard_LAT_els(__TopXMLNS, __IgnoreEls,
				 _els, <<>>),
    Cdata.

decode_vcard_LAT_els(__TopXMLNS, __IgnoreEls, [],
		     Cdata) ->
    decode_vcard_LAT_cdata(__TopXMLNS, Cdata);
decode_vcard_LAT_els(__TopXMLNS, __IgnoreEls,
		     [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_LAT_els(__TopXMLNS, __IgnoreEls, _els,
			 <<Cdata/binary, _data/binary>>);
decode_vcard_LAT_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Cdata) ->
    decode_vcard_LAT_els(__TopXMLNS, __IgnoreEls, _els,
			 Cdata).

encode_vcard_LAT(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_LAT_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"LAT">>, _attrs, _els}.

decode_vcard_LAT_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_LAT_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_LAT_cdata(undefined, _acc) -> _acc;
encode_vcard_LAT_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_USERID(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"USERID">>, _attrs, _els}) ->
    Cdata = decode_vcard_USERID_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_vcard_USERID_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_vcard_USERID_cdata(__TopXMLNS, Cdata);
decode_vcard_USERID_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_USERID_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_USERID_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_vcard_USERID_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_vcard_USERID(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_USERID_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"USERID">>, _attrs, _els}.

decode_vcard_USERID_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_USERID_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_USERID_cdata(undefined, _acc) -> _acc;
encode_vcard_USERID_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_NUMBER(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"NUMBER">>, _attrs, _els}) ->
    Cdata = decode_vcard_NUMBER_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_vcard_NUMBER_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_vcard_NUMBER_cdata(__TopXMLNS, Cdata);
decode_vcard_NUMBER_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_NUMBER_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_NUMBER_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_vcard_NUMBER_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_vcard_NUMBER(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_NUMBER_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"NUMBER">>, _attrs, _els}.

decode_vcard_NUMBER_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_NUMBER_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_NUMBER_cdata(undefined, _acc) -> _acc;
encode_vcard_NUMBER_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_LINE(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"LINE">>, _attrs, _els}) ->
    Cdata = decode_vcard_LINE_els(__TopXMLNS, __IgnoreEls,
				  _els, <<>>),
    Cdata.

decode_vcard_LINE_els(__TopXMLNS, __IgnoreEls, [],
		      Cdata) ->
    decode_vcard_LINE_cdata(__TopXMLNS, Cdata);
decode_vcard_LINE_els(__TopXMLNS, __IgnoreEls,
		      [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_LINE_els(__TopXMLNS, __IgnoreEls, _els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_LINE_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Cdata) ->
    decode_vcard_LINE_els(__TopXMLNS, __IgnoreEls, _els,
			  Cdata).

encode_vcard_LINE(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_LINE_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"LINE">>, _attrs, _els}.

decode_vcard_LINE_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_LINE_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_LINE_cdata(undefined, _acc) -> _acc;
encode_vcard_LINE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_CTRY(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"CTRY">>, _attrs, _els}) ->
    Cdata = decode_vcard_CTRY_els(__TopXMLNS, __IgnoreEls,
				  _els, <<>>),
    Cdata.

decode_vcard_CTRY_els(__TopXMLNS, __IgnoreEls, [],
		      Cdata) ->
    decode_vcard_CTRY_cdata(__TopXMLNS, Cdata);
decode_vcard_CTRY_els(__TopXMLNS, __IgnoreEls,
		      [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_CTRY_els(__TopXMLNS, __IgnoreEls, _els,
			  <<Cdata/binary, _data/binary>>);
decode_vcard_CTRY_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Cdata) ->
    decode_vcard_CTRY_els(__TopXMLNS, __IgnoreEls, _els,
			  Cdata).

encode_vcard_CTRY(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_CTRY_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"CTRY">>, _attrs, _els}.

decode_vcard_CTRY_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_CTRY_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_CTRY_cdata(undefined, _acc) -> _acc;
encode_vcard_CTRY_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_PCODE(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"PCODE">>, _attrs, _els}) ->
    Cdata = decode_vcard_PCODE_els(__TopXMLNS, __IgnoreEls,
				   _els, <<>>),
    Cdata.

decode_vcard_PCODE_els(__TopXMLNS, __IgnoreEls, [],
		       Cdata) ->
    decode_vcard_PCODE_cdata(__TopXMLNS, Cdata);
decode_vcard_PCODE_els(__TopXMLNS, __IgnoreEls,
		       [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_PCODE_els(__TopXMLNS, __IgnoreEls, _els,
			   <<Cdata/binary, _data/binary>>);
decode_vcard_PCODE_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Cdata) ->
    decode_vcard_PCODE_els(__TopXMLNS, __IgnoreEls, _els,
			   Cdata).

encode_vcard_PCODE(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_PCODE_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"PCODE">>, _attrs, _els}.

decode_vcard_PCODE_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_PCODE_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_PCODE_cdata(undefined, _acc) -> _acc;
encode_vcard_PCODE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_REGION(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"REGION">>, _attrs, _els}) ->
    Cdata = decode_vcard_REGION_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_vcard_REGION_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_vcard_REGION_cdata(__TopXMLNS, Cdata);
decode_vcard_REGION_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_REGION_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_REGION_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_vcard_REGION_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_vcard_REGION(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_REGION_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"REGION">>, _attrs, _els}.

decode_vcard_REGION_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_REGION_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_REGION_cdata(undefined, _acc) -> _acc;
encode_vcard_REGION_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_LOCALITY(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"LOCALITY">>, _attrs, _els}) ->
    Cdata = decode_vcard_LOCALITY_els(__TopXMLNS,
				      __IgnoreEls, _els, <<>>),
    Cdata.

decode_vcard_LOCALITY_els(__TopXMLNS, __IgnoreEls, [],
			  Cdata) ->
    decode_vcard_LOCALITY_cdata(__TopXMLNS, Cdata);
decode_vcard_LOCALITY_els(__TopXMLNS, __IgnoreEls,
			  [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_LOCALITY_els(__TopXMLNS, __IgnoreEls, _els,
			      <<Cdata/binary, _data/binary>>);
decode_vcard_LOCALITY_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Cdata) ->
    decode_vcard_LOCALITY_els(__TopXMLNS, __IgnoreEls, _els,
			      Cdata).

encode_vcard_LOCALITY(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_LOCALITY_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"LOCALITY">>, _attrs, _els}.

decode_vcard_LOCALITY_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_LOCALITY_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_LOCALITY_cdata(undefined, _acc) -> _acc;
encode_vcard_LOCALITY_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_STREET(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"STREET">>, _attrs, _els}) ->
    Cdata = decode_vcard_STREET_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_vcard_STREET_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_vcard_STREET_cdata(__TopXMLNS, Cdata);
decode_vcard_STREET_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_STREET_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_STREET_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_vcard_STREET_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_vcard_STREET(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_STREET_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"STREET">>, _attrs, _els}.

decode_vcard_STREET_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_STREET_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_STREET_cdata(undefined, _acc) -> _acc;
encode_vcard_STREET_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_EXTADD(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"EXTADD">>, _attrs, _els}) ->
    Cdata = decode_vcard_EXTADD_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_vcard_EXTADD_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_vcard_EXTADD_cdata(__TopXMLNS, Cdata);
decode_vcard_EXTADD_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_EXTADD_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_EXTADD_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_vcard_EXTADD_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_vcard_EXTADD(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_EXTADD_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"EXTADD">>, _attrs, _els}.

decode_vcard_EXTADD_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_EXTADD_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_EXTADD_cdata(undefined, _acc) -> _acc;
encode_vcard_EXTADD_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_POBOX(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"POBOX">>, _attrs, _els}) ->
    Cdata = decode_vcard_POBOX_els(__TopXMLNS, __IgnoreEls,
				   _els, <<>>),
    Cdata.

decode_vcard_POBOX_els(__TopXMLNS, __IgnoreEls, [],
		       Cdata) ->
    decode_vcard_POBOX_cdata(__TopXMLNS, Cdata);
decode_vcard_POBOX_els(__TopXMLNS, __IgnoreEls,
		       [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_POBOX_els(__TopXMLNS, __IgnoreEls, _els,
			   <<Cdata/binary, _data/binary>>);
decode_vcard_POBOX_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Cdata) ->
    decode_vcard_POBOX_els(__TopXMLNS, __IgnoreEls, _els,
			   Cdata).

encode_vcard_POBOX(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_POBOX_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"POBOX">>, _attrs, _els}.

decode_vcard_POBOX_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_POBOX_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_POBOX_cdata(undefined, _acc) -> _acc;
encode_vcard_POBOX_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_SUFFIX(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"SUFFIX">>, _attrs, _els}) ->
    Cdata = decode_vcard_SUFFIX_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_vcard_SUFFIX_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_vcard_SUFFIX_cdata(__TopXMLNS, Cdata);
decode_vcard_SUFFIX_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_SUFFIX_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_SUFFIX_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_vcard_SUFFIX_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_vcard_SUFFIX(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_SUFFIX_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"SUFFIX">>, _attrs, _els}.

decode_vcard_SUFFIX_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_SUFFIX_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_SUFFIX_cdata(undefined, _acc) -> _acc;
encode_vcard_SUFFIX_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_PREFIX(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"PREFIX">>, _attrs, _els}) ->
    Cdata = decode_vcard_PREFIX_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_vcard_PREFIX_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_vcard_PREFIX_cdata(__TopXMLNS, Cdata);
decode_vcard_PREFIX_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_PREFIX_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_PREFIX_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_vcard_PREFIX_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_vcard_PREFIX(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_PREFIX_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"PREFIX">>, _attrs, _els}.

decode_vcard_PREFIX_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_PREFIX_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_PREFIX_cdata(undefined, _acc) -> _acc;
encode_vcard_PREFIX_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_MIDDLE(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"MIDDLE">>, _attrs, _els}) ->
    Cdata = decode_vcard_MIDDLE_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_vcard_MIDDLE_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_vcard_MIDDLE_cdata(__TopXMLNS, Cdata);
decode_vcard_MIDDLE_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_MIDDLE_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_MIDDLE_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_vcard_MIDDLE_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_vcard_MIDDLE(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_MIDDLE_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"MIDDLE">>, _attrs, _els}.

decode_vcard_MIDDLE_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_MIDDLE_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_MIDDLE_cdata(undefined, _acc) -> _acc;
encode_vcard_MIDDLE_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_GIVEN(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"GIVEN">>, _attrs, _els}) ->
    Cdata = decode_vcard_GIVEN_els(__TopXMLNS, __IgnoreEls,
				   _els, <<>>),
    Cdata.

decode_vcard_GIVEN_els(__TopXMLNS, __IgnoreEls, [],
		       Cdata) ->
    decode_vcard_GIVEN_cdata(__TopXMLNS, Cdata);
decode_vcard_GIVEN_els(__TopXMLNS, __IgnoreEls,
		       [{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_GIVEN_els(__TopXMLNS, __IgnoreEls, _els,
			   <<Cdata/binary, _data/binary>>);
decode_vcard_GIVEN_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Cdata) ->
    decode_vcard_GIVEN_els(__TopXMLNS, __IgnoreEls, _els,
			   Cdata).

encode_vcard_GIVEN(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_GIVEN_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"GIVEN">>, _attrs, _els}.

decode_vcard_GIVEN_cdata(__TopXMLNS, <<>>) -> undefined;
decode_vcard_GIVEN_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_GIVEN_cdata(undefined, _acc) -> _acc;
encode_vcard_GIVEN_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_FAMILY(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"FAMILY">>, _attrs, _els}) ->
    Cdata = decode_vcard_FAMILY_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_vcard_FAMILY_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_vcard_FAMILY_cdata(__TopXMLNS, Cdata);
decode_vcard_FAMILY_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_vcard_FAMILY_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_vcard_FAMILY_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_vcard_FAMILY_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_vcard_FAMILY(Cdata, _xmlns_attrs) ->
    _els = encode_vcard_FAMILY_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"FAMILY">>, _attrs, _els}.

decode_vcard_FAMILY_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_vcard_FAMILY_cdata(__TopXMLNS, _val) -> _val.

encode_vcard_FAMILY_cdata(undefined, _acc) -> _acc;
encode_vcard_FAMILY_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_vcard_X400(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"X400">>, _attrs, _els}) ->
    true.

encode_vcard_X400(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"X400">>, _attrs, _els}.

decode_vcard_INTERNET(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"INTERNET">>, _attrs, _els}) ->
    true.

encode_vcard_INTERNET(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"INTERNET">>, _attrs, _els}.

decode_vcard_PREF(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"PREF">>, _attrs, _els}) ->
    true.

encode_vcard_PREF(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"PREF">>, _attrs, _els}.

decode_vcard_INTL(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"INTL">>, _attrs, _els}) ->
    true.

encode_vcard_INTL(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"INTL">>, _attrs, _els}.

decode_vcard_DOM(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"DOM">>, _attrs, _els}) ->
    true.

encode_vcard_DOM(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"DOM">>, _attrs, _els}.

decode_vcard_PARCEL(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"PARCEL">>, _attrs, _els}) ->
    true.

encode_vcard_PARCEL(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"PARCEL">>, _attrs, _els}.

decode_vcard_POSTAL(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"POSTAL">>, _attrs, _els}) ->
    true.

encode_vcard_POSTAL(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"POSTAL">>, _attrs, _els}.

decode_vcard_PCS(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"PCS">>, _attrs, _els}) ->
    true.

encode_vcard_PCS(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"PCS">>, _attrs, _els}.

decode_vcard_ISDN(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"ISDN">>, _attrs, _els}) ->
    true.

encode_vcard_ISDN(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"ISDN">>, _attrs, _els}.

decode_vcard_MODEM(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"MODEM">>, _attrs, _els}) ->
    true.

encode_vcard_MODEM(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"MODEM">>, _attrs, _els}.

decode_vcard_BBS(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"BBS">>, _attrs, _els}) ->
    true.

encode_vcard_BBS(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"BBS">>, _attrs, _els}.

decode_vcard_VIDEO(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"VIDEO">>, _attrs, _els}) ->
    true.

encode_vcard_VIDEO(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"VIDEO">>, _attrs, _els}.

decode_vcard_CELL(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"CELL">>, _attrs, _els}) ->
    true.

encode_vcard_CELL(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"CELL">>, _attrs, _els}.

decode_vcard_MSG(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"MSG">>, _attrs, _els}) ->
    true.

encode_vcard_MSG(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"MSG">>, _attrs, _els}.

decode_vcard_PAGER(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"PAGER">>, _attrs, _els}) ->
    true.

encode_vcard_PAGER(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"PAGER">>, _attrs, _els}.

decode_vcard_FAX(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"FAX">>, _attrs, _els}) ->
    true.

encode_vcard_FAX(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"FAX">>, _attrs, _els}.

decode_vcard_VOICE(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"VOICE">>, _attrs, _els}) ->
    true.

encode_vcard_VOICE(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"VOICE">>, _attrs, _els}.

decode_vcard_WORK(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"WORK">>, _attrs, _els}) ->
    true.

encode_vcard_WORK(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"WORK">>, _attrs, _els}.

decode_vcard_HOME(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"HOME">>, _attrs, _els}) ->
    true.

encode_vcard_HOME(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"HOME">>, _attrs, _els}.

decode_stream_error(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"stream:error">>, _attrs, _els}) ->
    {Text, Reason} = decode_stream_error_els(__TopXMLNS,
					     __IgnoreEls, _els, undefined,
					     undefined),
    {stream_error, Reason, Text}.

decode_stream_error_els(__TopXMLNS, __IgnoreEls, [],
			Text, Reason) ->
    {Text, Reason};
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"text">>, _attrs, _} = _el | _els], Text,
			Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   decode_stream_error_text(_xmlns, __IgnoreEls,
							    _el),
				   Reason);
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"bad-format">>, _attrs, _} = _el | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_bad_format(_xmlns,
								  __IgnoreEls,
								  _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"bad-namespace-prefix">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_bad_namespace_prefix(_xmlns,
									    __IgnoreEls,
									    _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"conflict">>, _attrs, _} = _el | _els], Text,
			Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_conflict(_xmlns,
								__IgnoreEls,
								_el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"connection-timeout">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_connection_timeout(_xmlns,
									  __IgnoreEls,
									  _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"host-gone">>, _attrs, _} = _el | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_host_gone(_xmlns,
								 __IgnoreEls,
								 _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"host-unknown">>, _attrs, _} = _el | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_host_unknown(_xmlns,
								    __IgnoreEls,
								    _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"improper-addressing">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_improper_addressing(_xmlns,
									   __IgnoreEls,
									   _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"internal-server-error">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_internal_server_error(_xmlns,
									     __IgnoreEls,
									     _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"invalid-from">>, _attrs, _} = _el | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_invalid_from(_xmlns,
								    __IgnoreEls,
								    _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"invalid-id">>, _attrs, _} = _el | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_invalid_id(_xmlns,
								  __IgnoreEls,
								  _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"invalid-namespace">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_invalid_namespace(_xmlns,
									 __IgnoreEls,
									 _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"invalid-xml">>, _attrs, _} = _el | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_invalid_xml(_xmlns,
								   __IgnoreEls,
								   _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"not-authorized">>, _attrs, _} = _el | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_not_authorized(_xmlns,
								      __IgnoreEls,
								      _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"not-well-formed">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_not_well_formed(_xmlns,
								       __IgnoreEls,
								       _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"policy-violation">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_policy_violation(_xmlns,
									__IgnoreEls,
									_el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"remote-connection-failed">>, _attrs, _} =
			     _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_remote_connection_failed(_xmlns,
										__IgnoreEls,
										_el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"reset">>, _attrs, _} = _el | _els], Text,
			Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_reset(_xmlns,
							     __IgnoreEls, _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"resource-constraint">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_resource_constraint(_xmlns,
									   __IgnoreEls,
									   _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"restricted-xml">>, _attrs, _} = _el | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_restricted_xml(_xmlns,
								      __IgnoreEls,
								      _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"see-other-host">>, _attrs, _} = _el | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_see_other_host(_xmlns,
								      __IgnoreEls,
								      _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"system-shutdown">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_system_shutdown(_xmlns,
								       __IgnoreEls,
								       _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"undefined-condition">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_undefined_condition(_xmlns,
									   __IgnoreEls,
									   _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"unsupported-encoding">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_unsupported_encoding(_xmlns,
									    __IgnoreEls,
									    _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"unsupported-stanza-type">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_unsupported_stanza_type(_xmlns,
									       __IgnoreEls,
									       _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"unsupported-version">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-streams">> ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_stream_error_unsupported_version(_xmlns,
									   __IgnoreEls,
									   _el));
       true ->
	   decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_stream_error_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Text, Reason) ->
    decode_stream_error_els(__TopXMLNS, __IgnoreEls, _els,
			    Text, Reason).

encode_stream_error({stream_error, Reason, Text},
		    _xmlns_attrs) ->
    _els = lists:reverse('encode_stream_error_$text'(Text,
						     'encode_stream_error_$reason'(Reason,
										   []))),
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

decode_stream_error_unsupported_version(__TopXMLNS,
					__IgnoreEls,
					{xmlel, <<"unsupported-version">>,
					 _attrs, _els}) ->
    'unsupported-version'.

encode_stream_error_unsupported_version('unsupported-version',
					_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"unsupported-version">>, _attrs, _els}.

decode_stream_error_unsupported_stanza_type(__TopXMLNS,
					    __IgnoreEls,
					    {xmlel,
					     <<"unsupported-stanza-type">>,
					     _attrs, _els}) ->
    'unsupported-stanza-type'.

encode_stream_error_unsupported_stanza_type('unsupported-stanza-type',
					    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"unsupported-stanza-type">>, _attrs, _els}.

decode_stream_error_unsupported_encoding(__TopXMLNS,
					 __IgnoreEls,
					 {xmlel, <<"unsupported-encoding">>,
					  _attrs, _els}) ->
    'unsupported-encoding'.

encode_stream_error_unsupported_encoding('unsupported-encoding',
					 _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"unsupported-encoding">>, _attrs, _els}.

decode_stream_error_undefined_condition(__TopXMLNS,
					__IgnoreEls,
					{xmlel, <<"undefined-condition">>,
					 _attrs, _els}) ->
    'undefined-condition'.

encode_stream_error_undefined_condition('undefined-condition',
					_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"undefined-condition">>, _attrs, _els}.

decode_stream_error_system_shutdown(__TopXMLNS,
				    __IgnoreEls,
				    {xmlel, <<"system-shutdown">>, _attrs,
				     _els}) ->
    'system-shutdown'.

encode_stream_error_system_shutdown('system-shutdown',
				    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"system-shutdown">>, _attrs, _els}.

decode_stream_error_see_other_host(__TopXMLNS,
				   __IgnoreEls,
				   {xmlel, <<"see-other-host">>, _attrs,
				    _els}) ->
    Host =
	decode_stream_error_see_other_host_els(__TopXMLNS,
					       __IgnoreEls, _els, <<>>),
    {'see-other-host', Host}.

decode_stream_error_see_other_host_els(__TopXMLNS,
				       __IgnoreEls, [], Host) ->
    decode_stream_error_see_other_host_cdata(__TopXMLNS,
					     Host);
decode_stream_error_see_other_host_els(__TopXMLNS,
				       __IgnoreEls, [{xmlcdata, _data} | _els],
				       Host) ->
    decode_stream_error_see_other_host_els(__TopXMLNS,
					   __IgnoreEls, _els,
					   <<Host/binary, _data/binary>>);
decode_stream_error_see_other_host_els(__TopXMLNS,
				       __IgnoreEls, [_ | _els], Host) ->
    decode_stream_error_see_other_host_els(__TopXMLNS,
					   __IgnoreEls, _els, Host).

encode_stream_error_see_other_host({'see-other-host',
				    Host},
				   _xmlns_attrs) ->
    _els = encode_stream_error_see_other_host_cdata(Host,
						    []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"see-other-host">>, _attrs, _els}.

decode_stream_error_see_other_host_cdata(__TopXMLNS,
					 <<>>) ->
    erlang:error({xmpp_codec,
		  {missing_cdata, <<>>, <<"see-other-host">>,
		   __TopXMLNS}});
decode_stream_error_see_other_host_cdata(__TopXMLNS,
					 _val) ->
    _val.

encode_stream_error_see_other_host_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_stream_error_restricted_xml(__TopXMLNS,
				   __IgnoreEls,
				   {xmlel, <<"restricted-xml">>, _attrs,
				    _els}) ->
    'restricted-xml'.

encode_stream_error_restricted_xml('restricted-xml',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"restricted-xml">>, _attrs, _els}.

decode_stream_error_resource_constraint(__TopXMLNS,
					__IgnoreEls,
					{xmlel, <<"resource-constraint">>,
					 _attrs, _els}) ->
    'resource-constraint'.

encode_stream_error_resource_constraint('resource-constraint',
					_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"resource-constraint">>, _attrs, _els}.

decode_stream_error_reset(__TopXMLNS, __IgnoreEls,
			  {xmlel, <<"reset">>, _attrs, _els}) ->
    reset.

encode_stream_error_reset(reset, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"reset">>, _attrs, _els}.

decode_stream_error_remote_connection_failed(__TopXMLNS,
					     __IgnoreEls,
					     {xmlel,
					      <<"remote-connection-failed">>,
					      _attrs, _els}) ->
    'remote-connection-failed'.

encode_stream_error_remote_connection_failed('remote-connection-failed',
					     _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"remote-connection-failed">>, _attrs, _els}.

decode_stream_error_policy_violation(__TopXMLNS,
				     __IgnoreEls,
				     {xmlel, <<"policy-violation">>, _attrs,
				      _els}) ->
    'policy-violation'.

encode_stream_error_policy_violation('policy-violation',
				     _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"policy-violation">>, _attrs, _els}.

decode_stream_error_not_well_formed(__TopXMLNS,
				    __IgnoreEls,
				    {xmlel, <<"not-well-formed">>, _attrs,
				     _els}) ->
    'not-well-formed'.

encode_stream_error_not_well_formed('not-well-formed',
				    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"not-well-formed">>, _attrs, _els}.

decode_stream_error_not_authorized(__TopXMLNS,
				   __IgnoreEls,
				   {xmlel, <<"not-authorized">>, _attrs,
				    _els}) ->
    'not-authorized'.

encode_stream_error_not_authorized('not-authorized',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"not-authorized">>, _attrs, _els}.

decode_stream_error_invalid_xml(__TopXMLNS, __IgnoreEls,
				{xmlel, <<"invalid-xml">>, _attrs, _els}) ->
    'invalid-xml'.

encode_stream_error_invalid_xml('invalid-xml',
				_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"invalid-xml">>, _attrs, _els}.

decode_stream_error_invalid_namespace(__TopXMLNS,
				      __IgnoreEls,
				      {xmlel, <<"invalid-namespace">>, _attrs,
				       _els}) ->
    'invalid-namespace'.

encode_stream_error_invalid_namespace('invalid-namespace',
				      _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"invalid-namespace">>, _attrs, _els}.

decode_stream_error_invalid_id(__TopXMLNS, __IgnoreEls,
			       {xmlel, <<"invalid-id">>, _attrs, _els}) ->
    'invalid-id'.

encode_stream_error_invalid_id('invalid-id',
			       _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"invalid-id">>, _attrs, _els}.

decode_stream_error_invalid_from(__TopXMLNS,
				 __IgnoreEls,
				 {xmlel, <<"invalid-from">>, _attrs, _els}) ->
    'invalid-from'.

encode_stream_error_invalid_from('invalid-from',
				 _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"invalid-from">>, _attrs, _els}.

decode_stream_error_internal_server_error(__TopXMLNS,
					  __IgnoreEls,
					  {xmlel, <<"internal-server-error">>,
					   _attrs, _els}) ->
    'internal-server-error'.

encode_stream_error_internal_server_error('internal-server-error',
					  _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"internal-server-error">>, _attrs, _els}.

decode_stream_error_improper_addressing(__TopXMLNS,
					__IgnoreEls,
					{xmlel, <<"improper-addressing">>,
					 _attrs, _els}) ->
    'improper-addressing'.

encode_stream_error_improper_addressing('improper-addressing',
					_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"improper-addressing">>, _attrs, _els}.

decode_stream_error_host_unknown(__TopXMLNS,
				 __IgnoreEls,
				 {xmlel, <<"host-unknown">>, _attrs, _els}) ->
    'host-unknown'.

encode_stream_error_host_unknown('host-unknown',
				 _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"host-unknown">>, _attrs, _els}.

decode_stream_error_host_gone(__TopXMLNS, __IgnoreEls,
			      {xmlel, <<"host-gone">>, _attrs, _els}) ->
    'host-gone'.

encode_stream_error_host_gone('host-gone',
			      _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"host-gone">>, _attrs, _els}.

decode_stream_error_connection_timeout(__TopXMLNS,
				       __IgnoreEls,
				       {xmlel, <<"connection-timeout">>, _attrs,
					_els}) ->
    'connection-timeout'.

encode_stream_error_connection_timeout('connection-timeout',
				       _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"connection-timeout">>, _attrs, _els}.

decode_stream_error_conflict(__TopXMLNS, __IgnoreEls,
			     {xmlel, <<"conflict">>, _attrs, _els}) ->
    conflict.

encode_stream_error_conflict(conflict, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"conflict">>, _attrs, _els}.

decode_stream_error_bad_namespace_prefix(__TopXMLNS,
					 __IgnoreEls,
					 {xmlel, <<"bad-namespace-prefix">>,
					  _attrs, _els}) ->
    'bad-namespace-prefix'.

encode_stream_error_bad_namespace_prefix('bad-namespace-prefix',
					 _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"bad-namespace-prefix">>, _attrs, _els}.

decode_stream_error_bad_format(__TopXMLNS, __IgnoreEls,
			       {xmlel, <<"bad-format">>, _attrs, _els}) ->
    'bad-format'.

encode_stream_error_bad_format('bad-format',
			       _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"bad-format">>, _attrs, _els}.

decode_stream_error_text(__TopXMLNS, __IgnoreEls,
			 {xmlel, <<"text">>, _attrs, _els}) ->
    Data = decode_stream_error_text_els(__TopXMLNS,
					__IgnoreEls, _els, <<>>),
    Lang = decode_stream_error_text_attrs(__TopXMLNS,
					  _attrs, undefined),
    {text, Lang, Data}.

decode_stream_error_text_els(__TopXMLNS, __IgnoreEls,
			     [], Data) ->
    decode_stream_error_text_cdata(__TopXMLNS, Data);
decode_stream_error_text_els(__TopXMLNS, __IgnoreEls,
			     [{xmlcdata, _data} | _els], Data) ->
    decode_stream_error_text_els(__TopXMLNS, __IgnoreEls,
				 _els, <<Data/binary, _data/binary>>);
decode_stream_error_text_els(__TopXMLNS, __IgnoreEls,
			     [_ | _els], Data) ->
    decode_stream_error_text_els(__TopXMLNS, __IgnoreEls,
				 _els, Data).

decode_stream_error_text_attrs(__TopXMLNS,
			       [{<<"xml:lang">>, _val} | _attrs], _Lang) ->
    decode_stream_error_text_attrs(__TopXMLNS, _attrs,
				   _val);
decode_stream_error_text_attrs(__TopXMLNS, [_ | _attrs],
			       Lang) ->
    decode_stream_error_text_attrs(__TopXMLNS, _attrs,
				   Lang);
decode_stream_error_text_attrs(__TopXMLNS, [], Lang) ->
    'decode_stream_error_text_attr_xml:lang'(__TopXMLNS,
					     Lang).

encode_stream_error_text({text, Lang, Data},
			 _xmlns_attrs) ->
    _els = encode_stream_error_text_cdata(Data, []),
    _attrs = 'encode_stream_error_text_attr_xml:lang'(Lang,
						      _xmlns_attrs),
    {xmlel, <<"text">>, _attrs, _els}.

'decode_stream_error_text_attr_xml:lang'(__TopXMLNS,
					 undefined) ->
    undefined;
'decode_stream_error_text_attr_xml:lang'(__TopXMLNS,
					 _val) ->
    _val.

'encode_stream_error_text_attr_xml:lang'(undefined,
					 _acc) ->
    _acc;
'encode_stream_error_text_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_stream_error_text_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_stream_error_text_cdata(__TopXMLNS, _val) ->
    _val.

encode_stream_error_text_cdata(undefined, _acc) -> _acc;
encode_stream_error_text_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_time(__TopXMLNS, __IgnoreEls,
	    {xmlel, <<"time">>, _attrs, _els}) ->
    {Utc, Tzo} = decode_time_els(__TopXMLNS, __IgnoreEls,
				 _els, undefined, undefined),
    {time, Tzo, Utc}.

decode_time_els(__TopXMLNS, __IgnoreEls, [], Utc,
		Tzo) ->
    {Utc, Tzo};
decode_time_els(__TopXMLNS, __IgnoreEls,
		[{xmlel, <<"tzo">>, _attrs, _} = _el | _els], Utc,
		Tzo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_time_els(__TopXMLNS, __IgnoreEls, _els, Utc,
			   decode_time_tzo(__TopXMLNS, __IgnoreEls, _el));
       true ->
	   decode_time_els(__TopXMLNS, __IgnoreEls, _els, Utc, Tzo)
    end;
decode_time_els(__TopXMLNS, __IgnoreEls,
		[{xmlel, <<"utc">>, _attrs, _} = _el | _els], Utc,
		Tzo) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_time_els(__TopXMLNS, __IgnoreEls, _els,
			   decode_time_utc(__TopXMLNS, __IgnoreEls, _el), Tzo);
       true ->
	   decode_time_els(__TopXMLNS, __IgnoreEls, _els, Utc, Tzo)
    end;
decode_time_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		Utc, Tzo) ->
    decode_time_els(__TopXMLNS, __IgnoreEls, _els, Utc,
		    Tzo).

encode_time({time, Tzo, Utc}, _xmlns_attrs) ->
    _els = lists:reverse('encode_time_$utc'(Utc,
					    'encode_time_$tzo'(Tzo, []))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"time">>, _attrs, _els}.

'encode_time_$utc'(undefined, _acc) -> _acc;
'encode_time_$utc'(Utc, _acc) ->
    [encode_time_utc(Utc, []) | _acc].

'encode_time_$tzo'(undefined, _acc) -> _acc;
'encode_time_$tzo'(Tzo, _acc) ->
    [encode_time_tzo(Tzo, []) | _acc].

decode_time_tzo(__TopXMLNS, __IgnoreEls,
		{xmlel, <<"tzo">>, _attrs, _els}) ->
    Cdata = decode_time_tzo_els(__TopXMLNS, __IgnoreEls,
				_els, <<>>),
    Cdata.

decode_time_tzo_els(__TopXMLNS, __IgnoreEls, [],
		    Cdata) ->
    decode_time_tzo_cdata(__TopXMLNS, Cdata);
decode_time_tzo_els(__TopXMLNS, __IgnoreEls,
		    [{xmlcdata, _data} | _els], Cdata) ->
    decode_time_tzo_els(__TopXMLNS, __IgnoreEls, _els,
			<<Cdata/binary, _data/binary>>);
decode_time_tzo_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		    Cdata) ->
    decode_time_tzo_els(__TopXMLNS, __IgnoreEls, _els,
			Cdata).

encode_time_tzo(Cdata, _xmlns_attrs) ->
    _els = encode_time_tzo_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"tzo">>, _attrs, _els}.

decode_time_tzo_cdata(__TopXMLNS, <<>>) -> undefined;
decode_time_tzo_cdata(__TopXMLNS, _val) ->
    case catch dec_tzo(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"tzo">>, __TopXMLNS}});
      _res -> _res
    end.

encode_time_tzo_cdata(undefined, _acc) -> _acc;
encode_time_tzo_cdata(_val, _acc) ->
    [{xmlcdata, enc_tzo(_val)} | _acc].

decode_time_utc(__TopXMLNS, __IgnoreEls,
		{xmlel, <<"utc">>, _attrs, _els}) ->
    Cdata = decode_time_utc_els(__TopXMLNS, __IgnoreEls,
				_els, <<>>),
    Cdata.

decode_time_utc_els(__TopXMLNS, __IgnoreEls, [],
		    Cdata) ->
    decode_time_utc_cdata(__TopXMLNS, Cdata);
decode_time_utc_els(__TopXMLNS, __IgnoreEls,
		    [{xmlcdata, _data} | _els], Cdata) ->
    decode_time_utc_els(__TopXMLNS, __IgnoreEls, _els,
			<<Cdata/binary, _data/binary>>);
decode_time_utc_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		    Cdata) ->
    decode_time_utc_els(__TopXMLNS, __IgnoreEls, _els,
			Cdata).

encode_time_utc(Cdata, _xmlns_attrs) ->
    _els = encode_time_utc_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"utc">>, _attrs, _els}.

decode_time_utc_cdata(__TopXMLNS, <<>>) -> undefined;
decode_time_utc_cdata(__TopXMLNS, _val) ->
    case catch dec_utc(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"utc">>, __TopXMLNS}});
      _res -> _res
    end.

encode_time_utc_cdata(undefined, _acc) -> _acc;
encode_time_utc_cdata(_val, _acc) ->
    [{xmlcdata, enc_utc(_val)} | _acc].

decode_ping(__TopXMLNS, __IgnoreEls,
	    {xmlel, <<"ping">>, _attrs, _els}) ->
    {ping}.

encode_ping({ping}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"ping">>, _attrs, _els}.

decode_session(__TopXMLNS, __IgnoreEls,
	       {xmlel, <<"session">>, _attrs, _els}) ->
    {session}.

encode_session({session}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"session">>, _attrs, _els}.

decode_register(__TopXMLNS, __IgnoreEls,
		{xmlel, <<"query">>, _attrs, _els}) ->
    {Zip, Xdata, Misc, Address, Instructions, Text, Last,
     First, Password, Registered, Date, Phone, State, Name,
     Username, Remove, Key, City, Nick, Url, Email} =
	decode_register_els(__TopXMLNS, __IgnoreEls, _els,
			    undefined, undefined, undefined, undefined,
			    undefined, undefined, undefined, undefined,
			    undefined, false, undefined, undefined, undefined,
			    undefined, undefined, false, undefined, undefined,
			    undefined, undefined, undefined),
    {register, Registered, Remove, Instructions, Username,
     Nick, Password, Name, First, Last, Email, Address, City,
     State, Zip, Phone, Url, Date, Misc, Text, Key, Xdata}.

decode_register_els(__TopXMLNS, __IgnoreEls, [], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    {Zip, Xdata, Misc, Address, Instructions, Text, Last,
     First, Password, Registered, Date, Phone, State, Name,
     Username, Remove, Key, City, Nick, Url, Email};
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"x">>, _attrs, _} = _el | _els], Zip, Xdata,
		    Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<"jabber:x:data">> ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       decode_xdata(_xmlns, __IgnoreEls, _el), Misc,
			       Address, Instructions, Text, Last, First,
			       Password, Registered, Date, Phone, State, Name,
			       Username, Remove, Key, City, Nick, Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"registered">>, _attrs, _} = _el | _els],
		    Zip, Xdata, Misc, Address, Instructions, Text, Last,
		    First, Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password,
			       decode_register_registered(__TopXMLNS,
							  __IgnoreEls, _el),
			       Date, Phone, State, Name, Username, Remove, Key,
			       City, Nick, Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"remove">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username,
			       decode_register_remove(__TopXMLNS, __IgnoreEls,
						      _el),
			       Key, City, Nick, Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"instructions">>, _attrs, _} = _el | _els],
		    Zip, Xdata, Misc, Address, Instructions, Text, Last,
		    First, Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address,
			       decode_register_instructions(__TopXMLNS,
							    __IgnoreEls, _el),
			       Text, Last, First, Password, Registered, Date,
			       Phone, State, Name, Username, Remove, Key, City,
			       Nick, Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"username">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name,
			       decode_register_username(__TopXMLNS, __IgnoreEls,
							_el),
			       Remove, Key, City, Nick, Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"nick">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City,
			       decode_register_nick(__TopXMLNS, __IgnoreEls,
						    _el),
			       Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"password">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First,
			       decode_register_password(__TopXMLNS, __IgnoreEls,
							_el),
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"name">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       decode_register_name(__TopXMLNS, __IgnoreEls,
						    _el),
			       Username, Remove, Key, City, Nick, Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"first">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       decode_register_first(__TopXMLNS, __IgnoreEls,
						     _el),
			       Password, Registered, Date, Phone, State, Name,
			       Username, Remove, Key, City, Nick, Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"last">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text,
			       decode_register_last(__TopXMLNS, __IgnoreEls,
						    _el),
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"email">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       decode_register_email(__TopXMLNS, __IgnoreEls,
						     _el));
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"address">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc,
			       decode_register_address(__TopXMLNS, __IgnoreEls,
						       _el),
			       Instructions, Text, Last, First, Password,
			       Registered, Date, Phone, State, Name, Username,
			       Remove, Key, City, Nick, Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"city">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key,
			       decode_register_city(__TopXMLNS, __IgnoreEls,
						    _el),
			       Nick, Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"state">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone,
			       decode_register_state(__TopXMLNS, __IgnoreEls,
						     _el),
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"zip">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els,
			       decode_register_zip(__TopXMLNS, __IgnoreEls,
						   _el),
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"phone">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date,
			       decode_register_phone(__TopXMLNS, __IgnoreEls,
						     _el),
			       State, Name, Username, Remove, Key, City, Nick,
			       Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"url">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick,
			       decode_register_url(__TopXMLNS, __IgnoreEls,
						   _el),
			       Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"date">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered,
			       decode_register_date(__TopXMLNS, __IgnoreEls,
						    _el),
			       Phone, State, Name, Username, Remove, Key, City,
			       Nick, Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"misc">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata,
			       decode_register_misc(__TopXMLNS, __IgnoreEls,
						    _el),
			       Address, Instructions, Text, Last, First,
			       Password, Registered, Date, Phone, State, Name,
			       Username, Remove, Key, City, Nick, Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"text">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions,
			       decode_register_text(__TopXMLNS, __IgnoreEls,
						    _el),
			       Last, First, Password, Registered, Date, Phone,
			       State, Name, Username, Remove, Key, City, Nick,
			       Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"key">>, _attrs, _} = _el | _els], Zip,
		    Xdata, Misc, Address, Instructions, Text, Last, First,
		    Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove,
			       decode_register_key(__TopXMLNS, __IgnoreEls,
						   _el),
			       City, Nick, Url, Email);
       true ->
	   decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			       Xdata, Misc, Address, Instructions, Text, Last,
			       First, Password, Registered, Date, Phone, State,
			       Name, Username, Remove, Key, City, Nick, Url,
			       Email)
    end;
decode_register_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		    Zip, Xdata, Misc, Address, Instructions, Text, Last,
		    First, Password, Registered, Date, Phone, State, Name,
		    Username, Remove, Key, City, Nick, Url, Email) ->
    decode_register_els(__TopXMLNS, __IgnoreEls, _els, Zip,
			Xdata, Misc, Address, Instructions, Text, Last, First,
			Password, Registered, Date, Phone, State, Name,
			Username, Remove, Key, City, Nick, Url, Email).

encode_register({register, Registered, Remove,
		 Instructions, Username, Nick, Password, Name, First,
		 Last, Email, Address, City, State, Zip, Phone, Url,
		 Date, Misc, Text, Key, Xdata},
		_xmlns_attrs) ->
    _els = lists:reverse('encode_register_$zip'(Zip,
						'encode_register_$xdata'(Xdata,
									 'encode_register_$misc'(Misc,
												 'encode_register_$address'(Address,
															    'encode_register_$instructions'(Instructions,
																			    'encode_register_$text'(Text,
																						    'encode_register_$last'(Last,
																									    'encode_register_$first'(First,
																												     'encode_register_$password'(Password,
																																 'encode_register_$registered'(Registered,
																																			       'encode_register_$date'(Date,
																																						       'encode_register_$phone'(Phone,
																																										'encode_register_$state'(State,
																																													 'encode_register_$name'(Name,
																																																 'encode_register_$username'(Username,
																																																			     'encode_register_$remove'(Remove,
																																																						       'encode_register_$key'(Key,
																																																									      'encode_register_$city'(City,
																																																												      'encode_register_$nick'(Nick,
																																																															      'encode_register_$url'(Url,
																																																																		     'encode_register_$email'(Email,
																																																																					      [])))))))))))))))))))))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"query">>, _attrs, _els}.

'encode_register_$zip'(undefined, _acc) -> _acc;
'encode_register_$zip'(Zip, _acc) ->
    [encode_register_zip(Zip, []) | _acc].

'encode_register_$xdata'(undefined, _acc) -> _acc;
'encode_register_$xdata'(Xdata, _acc) ->
    [encode_xdata(Xdata,
		  [{<<"xmlns">>, <<"jabber:x:data">>}])
     | _acc].

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

decode_register_key(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"key">>, _attrs, _els}) ->
    Cdata = decode_register_key_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_register_key_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_register_key_cdata(__TopXMLNS, Cdata);
decode_register_key_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_register_key_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_register_key_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_register_key_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_register_key(Cdata, _xmlns_attrs) ->
    _els = encode_register_key_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"key">>, _attrs, _els}.

decode_register_key_cdata(__TopXMLNS, <<>>) -> none;
decode_register_key_cdata(__TopXMLNS, _val) -> _val.

encode_register_key_cdata(none, _acc) -> _acc;
encode_register_key_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_text(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"text">>, _attrs, _els}) ->
    Cdata = decode_register_text_els(__TopXMLNS,
				     __IgnoreEls, _els, <<>>),
    Cdata.

decode_register_text_els(__TopXMLNS, __IgnoreEls, [],
			 Cdata) ->
    decode_register_text_cdata(__TopXMLNS, Cdata);
decode_register_text_els(__TopXMLNS, __IgnoreEls,
			 [{xmlcdata, _data} | _els], Cdata) ->
    decode_register_text_els(__TopXMLNS, __IgnoreEls, _els,
			     <<Cdata/binary, _data/binary>>);
decode_register_text_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Cdata) ->
    decode_register_text_els(__TopXMLNS, __IgnoreEls, _els,
			     Cdata).

encode_register_text(Cdata, _xmlns_attrs) ->
    _els = encode_register_text_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"text">>, _attrs, _els}.

decode_register_text_cdata(__TopXMLNS, <<>>) -> none;
decode_register_text_cdata(__TopXMLNS, _val) -> _val.

encode_register_text_cdata(none, _acc) -> _acc;
encode_register_text_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_misc(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"misc">>, _attrs, _els}) ->
    Cdata = decode_register_misc_els(__TopXMLNS,
				     __IgnoreEls, _els, <<>>),
    Cdata.

decode_register_misc_els(__TopXMLNS, __IgnoreEls, [],
			 Cdata) ->
    decode_register_misc_cdata(__TopXMLNS, Cdata);
decode_register_misc_els(__TopXMLNS, __IgnoreEls,
			 [{xmlcdata, _data} | _els], Cdata) ->
    decode_register_misc_els(__TopXMLNS, __IgnoreEls, _els,
			     <<Cdata/binary, _data/binary>>);
decode_register_misc_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Cdata) ->
    decode_register_misc_els(__TopXMLNS, __IgnoreEls, _els,
			     Cdata).

encode_register_misc(Cdata, _xmlns_attrs) ->
    _els = encode_register_misc_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"misc">>, _attrs, _els}.

decode_register_misc_cdata(__TopXMLNS, <<>>) -> none;
decode_register_misc_cdata(__TopXMLNS, _val) -> _val.

encode_register_misc_cdata(none, _acc) -> _acc;
encode_register_misc_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_date(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"date">>, _attrs, _els}) ->
    Cdata = decode_register_date_els(__TopXMLNS,
				     __IgnoreEls, _els, <<>>),
    Cdata.

decode_register_date_els(__TopXMLNS, __IgnoreEls, [],
			 Cdata) ->
    decode_register_date_cdata(__TopXMLNS, Cdata);
decode_register_date_els(__TopXMLNS, __IgnoreEls,
			 [{xmlcdata, _data} | _els], Cdata) ->
    decode_register_date_els(__TopXMLNS, __IgnoreEls, _els,
			     <<Cdata/binary, _data/binary>>);
decode_register_date_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Cdata) ->
    decode_register_date_els(__TopXMLNS, __IgnoreEls, _els,
			     Cdata).

encode_register_date(Cdata, _xmlns_attrs) ->
    _els = encode_register_date_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"date">>, _attrs, _els}.

decode_register_date_cdata(__TopXMLNS, <<>>) -> none;
decode_register_date_cdata(__TopXMLNS, _val) -> _val.

encode_register_date_cdata(none, _acc) -> _acc;
encode_register_date_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_url(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"url">>, _attrs, _els}) ->
    Cdata = decode_register_url_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_register_url_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_register_url_cdata(__TopXMLNS, Cdata);
decode_register_url_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_register_url_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_register_url_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_register_url_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_register_url(Cdata, _xmlns_attrs) ->
    _els = encode_register_url_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"url">>, _attrs, _els}.

decode_register_url_cdata(__TopXMLNS, <<>>) -> none;
decode_register_url_cdata(__TopXMLNS, _val) -> _val.

encode_register_url_cdata(none, _acc) -> _acc;
encode_register_url_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_phone(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"phone">>, _attrs, _els}) ->
    Cdata = decode_register_phone_els(__TopXMLNS,
				      __IgnoreEls, _els, <<>>),
    Cdata.

decode_register_phone_els(__TopXMLNS, __IgnoreEls, [],
			  Cdata) ->
    decode_register_phone_cdata(__TopXMLNS, Cdata);
decode_register_phone_els(__TopXMLNS, __IgnoreEls,
			  [{xmlcdata, _data} | _els], Cdata) ->
    decode_register_phone_els(__TopXMLNS, __IgnoreEls, _els,
			      <<Cdata/binary, _data/binary>>);
decode_register_phone_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Cdata) ->
    decode_register_phone_els(__TopXMLNS, __IgnoreEls, _els,
			      Cdata).

encode_register_phone(Cdata, _xmlns_attrs) ->
    _els = encode_register_phone_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"phone">>, _attrs, _els}.

decode_register_phone_cdata(__TopXMLNS, <<>>) -> none;
decode_register_phone_cdata(__TopXMLNS, _val) -> _val.

encode_register_phone_cdata(none, _acc) -> _acc;
encode_register_phone_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_zip(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"zip">>, _attrs, _els}) ->
    Cdata = decode_register_zip_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_register_zip_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_register_zip_cdata(__TopXMLNS, Cdata);
decode_register_zip_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_register_zip_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_register_zip_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_register_zip_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_register_zip(Cdata, _xmlns_attrs) ->
    _els = encode_register_zip_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"zip">>, _attrs, _els}.

decode_register_zip_cdata(__TopXMLNS, <<>>) -> none;
decode_register_zip_cdata(__TopXMLNS, _val) -> _val.

encode_register_zip_cdata(none, _acc) -> _acc;
encode_register_zip_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_state(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"state">>, _attrs, _els}) ->
    Cdata = decode_register_state_els(__TopXMLNS,
				      __IgnoreEls, _els, <<>>),
    Cdata.

decode_register_state_els(__TopXMLNS, __IgnoreEls, [],
			  Cdata) ->
    decode_register_state_cdata(__TopXMLNS, Cdata);
decode_register_state_els(__TopXMLNS, __IgnoreEls,
			  [{xmlcdata, _data} | _els], Cdata) ->
    decode_register_state_els(__TopXMLNS, __IgnoreEls, _els,
			      <<Cdata/binary, _data/binary>>);
decode_register_state_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Cdata) ->
    decode_register_state_els(__TopXMLNS, __IgnoreEls, _els,
			      Cdata).

encode_register_state(Cdata, _xmlns_attrs) ->
    _els = encode_register_state_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"state">>, _attrs, _els}.

decode_register_state_cdata(__TopXMLNS, <<>>) -> none;
decode_register_state_cdata(__TopXMLNS, _val) -> _val.

encode_register_state_cdata(none, _acc) -> _acc;
encode_register_state_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_city(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"city">>, _attrs, _els}) ->
    Cdata = decode_register_city_els(__TopXMLNS,
				     __IgnoreEls, _els, <<>>),
    Cdata.

decode_register_city_els(__TopXMLNS, __IgnoreEls, [],
			 Cdata) ->
    decode_register_city_cdata(__TopXMLNS, Cdata);
decode_register_city_els(__TopXMLNS, __IgnoreEls,
			 [{xmlcdata, _data} | _els], Cdata) ->
    decode_register_city_els(__TopXMLNS, __IgnoreEls, _els,
			     <<Cdata/binary, _data/binary>>);
decode_register_city_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Cdata) ->
    decode_register_city_els(__TopXMLNS, __IgnoreEls, _els,
			     Cdata).

encode_register_city(Cdata, _xmlns_attrs) ->
    _els = encode_register_city_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"city">>, _attrs, _els}.

decode_register_city_cdata(__TopXMLNS, <<>>) -> none;
decode_register_city_cdata(__TopXMLNS, _val) -> _val.

encode_register_city_cdata(none, _acc) -> _acc;
encode_register_city_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_address(__TopXMLNS, __IgnoreEls,
			{xmlel, <<"address">>, _attrs, _els}) ->
    Cdata = decode_register_address_els(__TopXMLNS,
					__IgnoreEls, _els, <<>>),
    Cdata.

decode_register_address_els(__TopXMLNS, __IgnoreEls, [],
			    Cdata) ->
    decode_register_address_cdata(__TopXMLNS, Cdata);
decode_register_address_els(__TopXMLNS, __IgnoreEls,
			    [{xmlcdata, _data} | _els], Cdata) ->
    decode_register_address_els(__TopXMLNS, __IgnoreEls,
				_els, <<Cdata/binary, _data/binary>>);
decode_register_address_els(__TopXMLNS, __IgnoreEls,
			    [_ | _els], Cdata) ->
    decode_register_address_els(__TopXMLNS, __IgnoreEls,
				_els, Cdata).

encode_register_address(Cdata, _xmlns_attrs) ->
    _els = encode_register_address_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"address">>, _attrs, _els}.

decode_register_address_cdata(__TopXMLNS, <<>>) -> none;
decode_register_address_cdata(__TopXMLNS, _val) -> _val.

encode_register_address_cdata(none, _acc) -> _acc;
encode_register_address_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_email(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"email">>, _attrs, _els}) ->
    Cdata = decode_register_email_els(__TopXMLNS,
				      __IgnoreEls, _els, <<>>),
    Cdata.

decode_register_email_els(__TopXMLNS, __IgnoreEls, [],
			  Cdata) ->
    decode_register_email_cdata(__TopXMLNS, Cdata);
decode_register_email_els(__TopXMLNS, __IgnoreEls,
			  [{xmlcdata, _data} | _els], Cdata) ->
    decode_register_email_els(__TopXMLNS, __IgnoreEls, _els,
			      <<Cdata/binary, _data/binary>>);
decode_register_email_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Cdata) ->
    decode_register_email_els(__TopXMLNS, __IgnoreEls, _els,
			      Cdata).

encode_register_email(Cdata, _xmlns_attrs) ->
    _els = encode_register_email_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"email">>, _attrs, _els}.

decode_register_email_cdata(__TopXMLNS, <<>>) -> none;
decode_register_email_cdata(__TopXMLNS, _val) -> _val.

encode_register_email_cdata(none, _acc) -> _acc;
encode_register_email_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_last(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"last">>, _attrs, _els}) ->
    Cdata = decode_register_last_els(__TopXMLNS,
				     __IgnoreEls, _els, <<>>),
    Cdata.

decode_register_last_els(__TopXMLNS, __IgnoreEls, [],
			 Cdata) ->
    decode_register_last_cdata(__TopXMLNS, Cdata);
decode_register_last_els(__TopXMLNS, __IgnoreEls,
			 [{xmlcdata, _data} | _els], Cdata) ->
    decode_register_last_els(__TopXMLNS, __IgnoreEls, _els,
			     <<Cdata/binary, _data/binary>>);
decode_register_last_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Cdata) ->
    decode_register_last_els(__TopXMLNS, __IgnoreEls, _els,
			     Cdata).

encode_register_last(Cdata, _xmlns_attrs) ->
    _els = encode_register_last_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"last">>, _attrs, _els}.

decode_register_last_cdata(__TopXMLNS, <<>>) -> none;
decode_register_last_cdata(__TopXMLNS, _val) -> _val.

encode_register_last_cdata(none, _acc) -> _acc;
encode_register_last_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_first(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"first">>, _attrs, _els}) ->
    Cdata = decode_register_first_els(__TopXMLNS,
				      __IgnoreEls, _els, <<>>),
    Cdata.

decode_register_first_els(__TopXMLNS, __IgnoreEls, [],
			  Cdata) ->
    decode_register_first_cdata(__TopXMLNS, Cdata);
decode_register_first_els(__TopXMLNS, __IgnoreEls,
			  [{xmlcdata, _data} | _els], Cdata) ->
    decode_register_first_els(__TopXMLNS, __IgnoreEls, _els,
			      <<Cdata/binary, _data/binary>>);
decode_register_first_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Cdata) ->
    decode_register_first_els(__TopXMLNS, __IgnoreEls, _els,
			      Cdata).

encode_register_first(Cdata, _xmlns_attrs) ->
    _els = encode_register_first_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"first">>, _attrs, _els}.

decode_register_first_cdata(__TopXMLNS, <<>>) -> none;
decode_register_first_cdata(__TopXMLNS, _val) -> _val.

encode_register_first_cdata(none, _acc) -> _acc;
encode_register_first_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_name(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"name">>, _attrs, _els}) ->
    Cdata = decode_register_name_els(__TopXMLNS,
				     __IgnoreEls, _els, <<>>),
    Cdata.

decode_register_name_els(__TopXMLNS, __IgnoreEls, [],
			 Cdata) ->
    decode_register_name_cdata(__TopXMLNS, Cdata);
decode_register_name_els(__TopXMLNS, __IgnoreEls,
			 [{xmlcdata, _data} | _els], Cdata) ->
    decode_register_name_els(__TopXMLNS, __IgnoreEls, _els,
			     <<Cdata/binary, _data/binary>>);
decode_register_name_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Cdata) ->
    decode_register_name_els(__TopXMLNS, __IgnoreEls, _els,
			     Cdata).

encode_register_name(Cdata, _xmlns_attrs) ->
    _els = encode_register_name_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"name">>, _attrs, _els}.

decode_register_name_cdata(__TopXMLNS, <<>>) -> none;
decode_register_name_cdata(__TopXMLNS, _val) -> _val.

encode_register_name_cdata(none, _acc) -> _acc;
encode_register_name_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_password(__TopXMLNS, __IgnoreEls,
			 {xmlel, <<"password">>, _attrs, _els}) ->
    Cdata = decode_register_password_els(__TopXMLNS,
					 __IgnoreEls, _els, <<>>),
    Cdata.

decode_register_password_els(__TopXMLNS, __IgnoreEls,
			     [], Cdata) ->
    decode_register_password_cdata(__TopXMLNS, Cdata);
decode_register_password_els(__TopXMLNS, __IgnoreEls,
			     [{xmlcdata, _data} | _els], Cdata) ->
    decode_register_password_els(__TopXMLNS, __IgnoreEls,
				 _els, <<Cdata/binary, _data/binary>>);
decode_register_password_els(__TopXMLNS, __IgnoreEls,
			     [_ | _els], Cdata) ->
    decode_register_password_els(__TopXMLNS, __IgnoreEls,
				 _els, Cdata).

encode_register_password(Cdata, _xmlns_attrs) ->
    _els = encode_register_password_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"password">>, _attrs, _els}.

decode_register_password_cdata(__TopXMLNS, <<>>) ->
    none;
decode_register_password_cdata(__TopXMLNS, _val) ->
    _val.

encode_register_password_cdata(none, _acc) -> _acc;
encode_register_password_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_nick(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"nick">>, _attrs, _els}) ->
    Cdata = decode_register_nick_els(__TopXMLNS,
				     __IgnoreEls, _els, <<>>),
    Cdata.

decode_register_nick_els(__TopXMLNS, __IgnoreEls, [],
			 Cdata) ->
    decode_register_nick_cdata(__TopXMLNS, Cdata);
decode_register_nick_els(__TopXMLNS, __IgnoreEls,
			 [{xmlcdata, _data} | _els], Cdata) ->
    decode_register_nick_els(__TopXMLNS, __IgnoreEls, _els,
			     <<Cdata/binary, _data/binary>>);
decode_register_nick_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Cdata) ->
    decode_register_nick_els(__TopXMLNS, __IgnoreEls, _els,
			     Cdata).

encode_register_nick(Cdata, _xmlns_attrs) ->
    _els = encode_register_nick_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"nick">>, _attrs, _els}.

decode_register_nick_cdata(__TopXMLNS, <<>>) -> none;
decode_register_nick_cdata(__TopXMLNS, _val) -> _val.

encode_register_nick_cdata(none, _acc) -> _acc;
encode_register_nick_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_username(__TopXMLNS, __IgnoreEls,
			 {xmlel, <<"username">>, _attrs, _els}) ->
    Cdata = decode_register_username_els(__TopXMLNS,
					 __IgnoreEls, _els, <<>>),
    Cdata.

decode_register_username_els(__TopXMLNS, __IgnoreEls,
			     [], Cdata) ->
    decode_register_username_cdata(__TopXMLNS, Cdata);
decode_register_username_els(__TopXMLNS, __IgnoreEls,
			     [{xmlcdata, _data} | _els], Cdata) ->
    decode_register_username_els(__TopXMLNS, __IgnoreEls,
				 _els, <<Cdata/binary, _data/binary>>);
decode_register_username_els(__TopXMLNS, __IgnoreEls,
			     [_ | _els], Cdata) ->
    decode_register_username_els(__TopXMLNS, __IgnoreEls,
				 _els, Cdata).

encode_register_username(Cdata, _xmlns_attrs) ->
    _els = encode_register_username_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"username">>, _attrs, _els}.

decode_register_username_cdata(__TopXMLNS, <<>>) ->
    none;
decode_register_username_cdata(__TopXMLNS, _val) ->
    _val.

encode_register_username_cdata(none, _acc) -> _acc;
encode_register_username_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_instructions(__TopXMLNS, __IgnoreEls,
			     {xmlel, <<"instructions">>, _attrs, _els}) ->
    Cdata = decode_register_instructions_els(__TopXMLNS,
					     __IgnoreEls, _els, <<>>),
    Cdata.

decode_register_instructions_els(__TopXMLNS,
				 __IgnoreEls, [], Cdata) ->
    decode_register_instructions_cdata(__TopXMLNS, Cdata);
decode_register_instructions_els(__TopXMLNS,
				 __IgnoreEls, [{xmlcdata, _data} | _els],
				 Cdata) ->
    decode_register_instructions_els(__TopXMLNS,
				     __IgnoreEls, _els,
				     <<Cdata/binary, _data/binary>>);
decode_register_instructions_els(__TopXMLNS,
				 __IgnoreEls, [_ | _els], Cdata) ->
    decode_register_instructions_els(__TopXMLNS,
				     __IgnoreEls, _els, Cdata).

encode_register_instructions(Cdata, _xmlns_attrs) ->
    _els = encode_register_instructions_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"instructions">>, _attrs, _els}.

decode_register_instructions_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_register_instructions_cdata(__TopXMLNS, _val) ->
    _val.

encode_register_instructions_cdata(undefined, _acc) ->
    _acc;
encode_register_instructions_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_register_remove(__TopXMLNS, __IgnoreEls,
		       {xmlel, <<"remove">>, _attrs, _els}) ->
    true.

encode_register_remove(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"remove">>, _attrs, _els}.

decode_register_registered(__TopXMLNS, __IgnoreEls,
			   {xmlel, <<"registered">>, _attrs, _els}) ->
    true.

encode_register_registered(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"registered">>, _attrs, _els}.

decode_feature_register(__TopXMLNS, __IgnoreEls,
			{xmlel, <<"register">>, _attrs, _els}) ->
    {feature_register}.

encode_feature_register({feature_register},
			_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"register">>, _attrs, _els}.

decode_caps(__TopXMLNS, __IgnoreEls,
	    {xmlel, <<"c">>, _attrs, _els}) ->
    {Hash, Node, Ver} = decode_caps_attrs(__TopXMLNS,
					  _attrs, undefined, undefined,
					  undefined),
    {caps, Hash, Node, Ver}.

decode_caps_attrs(__TopXMLNS,
		  [{<<"hash">>, _val} | _attrs], _Hash, Node, Ver) ->
    decode_caps_attrs(__TopXMLNS, _attrs, _val, Node, Ver);
decode_caps_attrs(__TopXMLNS,
		  [{<<"node">>, _val} | _attrs], Hash, _Node, Ver) ->
    decode_caps_attrs(__TopXMLNS, _attrs, Hash, _val, Ver);
decode_caps_attrs(__TopXMLNS,
		  [{<<"ver">>, _val} | _attrs], Hash, Node, _Ver) ->
    decode_caps_attrs(__TopXMLNS, _attrs, Hash, Node, _val);
decode_caps_attrs(__TopXMLNS, [_ | _attrs], Hash, Node,
		  Ver) ->
    decode_caps_attrs(__TopXMLNS, _attrs, Hash, Node, Ver);
decode_caps_attrs(__TopXMLNS, [], Hash, Node, Ver) ->
    {decode_caps_attr_hash(__TopXMLNS, Hash),
     decode_caps_attr_node(__TopXMLNS, Node),
     decode_caps_attr_ver(__TopXMLNS, Ver)}.

encode_caps({caps, Hash, Node, Ver}, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_caps_attr_ver(Ver,
				  encode_caps_attr_node(Node,
							encode_caps_attr_hash(Hash,
									      _xmlns_attrs))),
    {xmlel, <<"c">>, _attrs, _els}.

decode_caps_attr_hash(__TopXMLNS, undefined) ->
    undefined;
decode_caps_attr_hash(__TopXMLNS, _val) -> _val.

encode_caps_attr_hash(undefined, _acc) -> _acc;
encode_caps_attr_hash(_val, _acc) ->
    [{<<"hash">>, _val} | _acc].

decode_caps_attr_node(__TopXMLNS, undefined) ->
    undefined;
decode_caps_attr_node(__TopXMLNS, _val) -> _val.

encode_caps_attr_node(undefined, _acc) -> _acc;
encode_caps_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_caps_attr_ver(__TopXMLNS, undefined) ->
    undefined;
decode_caps_attr_ver(__TopXMLNS, _val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"ver">>, <<"c">>, __TopXMLNS}});
      _res -> _res
    end.

encode_caps_attr_ver(undefined, _acc) -> _acc;
encode_caps_attr_ver(_val, _acc) ->
    [{<<"ver">>, base64:encode(_val)} | _acc].

decode_p1_ack(__TopXMLNS, __IgnoreEls,
	      {xmlel, <<"ack">>, _attrs, _els}) ->
    {p1_ack}.

encode_p1_ack({p1_ack}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"ack">>, _attrs, _els}.

decode_p1_rebind(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"rebind">>, _attrs, _els}) ->
    {p1_rebind}.

encode_p1_rebind({p1_rebind}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"rebind">>, _attrs, _els}.

decode_p1_push(__TopXMLNS, __IgnoreEls,
	       {xmlel, <<"push">>, _attrs, _els}) ->
    {p1_push}.

encode_p1_push({p1_push}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"push">>, _attrs, _els}.

decode_stream_features(__TopXMLNS, __IgnoreEls,
		       {xmlel, <<"stream:features">>, _attrs, _els}) ->
    __Els = decode_stream_features_els(__TopXMLNS,
				       __IgnoreEls, _els, []),
    {stream_features, __Els}.

decode_stream_features_els(__TopXMLNS, __IgnoreEls, [],
			   __Els) ->
    lists:reverse(__Els);
decode_stream_features_els(__TopXMLNS, __IgnoreEls,
			   [{xmlel, _, _, _} = _el | _els], __Els) ->
    if __IgnoreEls ->
	   decode_stream_features_els(__TopXMLNS, __IgnoreEls,
				      _els, [_el | __Els]);
       true ->
	   case is_known_tag(_el) of
	     true ->
		 decode_stream_features_els(__TopXMLNS, __IgnoreEls,
					    _els, [decode(_el) | __Els]);
	     false ->
		 decode_stream_features_els(__TopXMLNS, __IgnoreEls,
					    _els, __Els)
	   end
    end;
decode_stream_features_els(__TopXMLNS, __IgnoreEls,
			   [_ | _els], __Els) ->
    decode_stream_features_els(__TopXMLNS, __IgnoreEls,
			       _els, __Els).

encode_stream_features({stream_features, __Els},
		       _xmlns_attrs) ->
    _els = [encode(_el) || _el <- __Els],
    _attrs = _xmlns_attrs,
    {xmlel, <<"stream:features">>, _attrs, _els}.

decode_compression(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"compression">>, _attrs, _els}) ->
    Methods = decode_compression_els(__TopXMLNS,
				     __IgnoreEls, _els, []),
    {compression, Methods}.

decode_compression_els(__TopXMLNS, __IgnoreEls, [],
		       Methods) ->
    lists:reverse(Methods);
decode_compression_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"method">>, _attrs, _} = _el | _els],
		       Methods) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_compression_els(__TopXMLNS, __IgnoreEls, _els,
				  case decode_compression_method(__TopXMLNS,
								 __IgnoreEls,
								 _el)
				      of
				    undefined -> Methods;
				    _new_el -> [_new_el | Methods]
				  end);
       true ->
	   decode_compression_els(__TopXMLNS, __IgnoreEls, _els,
				  Methods)
    end;
decode_compression_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Methods) ->
    decode_compression_els(__TopXMLNS, __IgnoreEls, _els,
			   Methods).

encode_compression({compression, Methods},
		   _xmlns_attrs) ->
    _els =
	lists:reverse('encode_compression_$methods'(Methods,
						    [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"compression">>, _attrs, _els}.

'encode_compression_$methods'([], _acc) -> _acc;
'encode_compression_$methods'([Methods | _els], _acc) ->
    'encode_compression_$methods'(_els,
				  [encode_compression_method(Methods, [])
				   | _acc]).

decode_compression_method(__TopXMLNS, __IgnoreEls,
			  {xmlel, <<"method">>, _attrs, _els}) ->
    Cdata = decode_compression_method_els(__TopXMLNS,
					  __IgnoreEls, _els, <<>>),
    Cdata.

decode_compression_method_els(__TopXMLNS, __IgnoreEls,
			      [], Cdata) ->
    decode_compression_method_cdata(__TopXMLNS, Cdata);
decode_compression_method_els(__TopXMLNS, __IgnoreEls,
			      [{xmlcdata, _data} | _els], Cdata) ->
    decode_compression_method_els(__TopXMLNS, __IgnoreEls,
				  _els, <<Cdata/binary, _data/binary>>);
decode_compression_method_els(__TopXMLNS, __IgnoreEls,
			      [_ | _els], Cdata) ->
    decode_compression_method_els(__TopXMLNS, __IgnoreEls,
				  _els, Cdata).

encode_compression_method(Cdata, _xmlns_attrs) ->
    _els = encode_compression_method_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"method">>, _attrs, _els}.

decode_compression_method_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_compression_method_cdata(__TopXMLNS, _val) ->
    _val.

encode_compression_method_cdata(undefined, _acc) ->
    _acc;
encode_compression_method_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_compressed(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"compressed">>, _attrs, _els}) ->
    {compressed}.

encode_compressed({compressed}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"compressed">>, _attrs, _els}.

decode_compress(__TopXMLNS, __IgnoreEls,
		{xmlel, <<"compress">>, _attrs, _els}) ->
    Methods = decode_compress_els(__TopXMLNS, __IgnoreEls,
				  _els, []),
    {compress, Methods}.

decode_compress_els(__TopXMLNS, __IgnoreEls, [],
		    Methods) ->
    lists:reverse(Methods);
decode_compress_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"method">>, _attrs, _} = _el | _els],
		    Methods) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_compress_els(__TopXMLNS, __IgnoreEls, _els,
			       case decode_compress_method(__TopXMLNS,
							   __IgnoreEls, _el)
				   of
				 undefined -> Methods;
				 _new_el -> [_new_el | Methods]
			       end);
       true ->
	   decode_compress_els(__TopXMLNS, __IgnoreEls, _els,
			       Methods)
    end;
decode_compress_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		    Methods) ->
    decode_compress_els(__TopXMLNS, __IgnoreEls, _els,
			Methods).

encode_compress({compress, Methods}, _xmlns_attrs) ->
    _els = lists:reverse('encode_compress_$methods'(Methods,
						    [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"compress">>, _attrs, _els}.

'encode_compress_$methods'([], _acc) -> _acc;
'encode_compress_$methods'([Methods | _els], _acc) ->
    'encode_compress_$methods'(_els,
			       [encode_compress_method(Methods, []) | _acc]).

decode_compress_method(__TopXMLNS, __IgnoreEls,
		       {xmlel, <<"method">>, _attrs, _els}) ->
    Cdata = decode_compress_method_els(__TopXMLNS,
				       __IgnoreEls, _els, <<>>),
    Cdata.

decode_compress_method_els(__TopXMLNS, __IgnoreEls, [],
			   Cdata) ->
    decode_compress_method_cdata(__TopXMLNS, Cdata);
decode_compress_method_els(__TopXMLNS, __IgnoreEls,
			   [{xmlcdata, _data} | _els], Cdata) ->
    decode_compress_method_els(__TopXMLNS, __IgnoreEls,
			       _els, <<Cdata/binary, _data/binary>>);
decode_compress_method_els(__TopXMLNS, __IgnoreEls,
			   [_ | _els], Cdata) ->
    decode_compress_method_els(__TopXMLNS, __IgnoreEls,
			       _els, Cdata).

encode_compress_method(Cdata, _xmlns_attrs) ->
    _els = encode_compress_method_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"method">>, _attrs, _els}.

decode_compress_method_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_compress_method_cdata(__TopXMLNS, _val) -> _val.

encode_compress_method_cdata(undefined, _acc) -> _acc;
encode_compress_method_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_compress_failure(__TopXMLNS, __IgnoreEls,
			{xmlel, <<"failure">>, _attrs, _els}) ->
    Reason = decode_compress_failure_els(__TopXMLNS,
					 __IgnoreEls, _els, undefined),
    {compress_failure, Reason}.

decode_compress_failure_els(__TopXMLNS, __IgnoreEls, [],
			    Reason) ->
    Reason;
decode_compress_failure_els(__TopXMLNS, __IgnoreEls,
			    [{xmlel, <<"setup-failed">>, _attrs, _} = _el
			     | _els],
			    Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_compress_failure_els(__TopXMLNS, __IgnoreEls,
				       _els,
				       decode_compress_failure_setup_failed(__TopXMLNS,
									    __IgnoreEls,
									    _el));
       true ->
	   decode_compress_failure_els(__TopXMLNS, __IgnoreEls,
				       _els, Reason)
    end;
decode_compress_failure_els(__TopXMLNS, __IgnoreEls,
			    [{xmlel, <<"processing-failed">>, _attrs, _} = _el
			     | _els],
			    Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_compress_failure_els(__TopXMLNS, __IgnoreEls,
				       _els,
				       decode_compress_failure_processing_failed(__TopXMLNS,
										 __IgnoreEls,
										 _el));
       true ->
	   decode_compress_failure_els(__TopXMLNS, __IgnoreEls,
				       _els, Reason)
    end;
decode_compress_failure_els(__TopXMLNS, __IgnoreEls,
			    [{xmlel, <<"unsupported-method">>, _attrs, _} = _el
			     | _els],
			    Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_compress_failure_els(__TopXMLNS, __IgnoreEls,
				       _els,
				       decode_compress_failure_unsupported_method(__TopXMLNS,
										  __IgnoreEls,
										  _el));
       true ->
	   decode_compress_failure_els(__TopXMLNS, __IgnoreEls,
				       _els, Reason)
    end;
decode_compress_failure_els(__TopXMLNS, __IgnoreEls,
			    [_ | _els], Reason) ->
    decode_compress_failure_els(__TopXMLNS, __IgnoreEls,
				_els, Reason).

encode_compress_failure({compress_failure, Reason},
			_xmlns_attrs) ->
    _els =
	lists:reverse('encode_compress_failure_$reason'(Reason,
							[])),
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

decode_compress_failure_unsupported_method(__TopXMLNS,
					   __IgnoreEls,
					   {xmlel, <<"unsupported-method">>,
					    _attrs, _els}) ->
    'unsupported-method'.

encode_compress_failure_unsupported_method('unsupported-method',
					   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"unsupported-method">>, _attrs, _els}.

decode_compress_failure_processing_failed(__TopXMLNS,
					  __IgnoreEls,
					  {xmlel, <<"processing-failed">>,
					   _attrs, _els}) ->
    'processing-failed'.

encode_compress_failure_processing_failed('processing-failed',
					  _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"processing-failed">>, _attrs, _els}.

decode_compress_failure_setup_failed(__TopXMLNS,
				     __IgnoreEls,
				     {xmlel, <<"setup-failed">>, _attrs,
				      _els}) ->
    'setup-failed'.

encode_compress_failure_setup_failed('setup-failed',
				     _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"setup-failed">>, _attrs, _els}.

decode_starttls_failure(__TopXMLNS, __IgnoreEls,
			{xmlel, <<"failure">>, _attrs, _els}) ->
    {starttls_failure}.

encode_starttls_failure({starttls_failure},
			_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"failure">>, _attrs, _els}.

decode_starttls_proceed(__TopXMLNS, __IgnoreEls,
			{xmlel, <<"proceed">>, _attrs, _els}) ->
    {starttls_proceed}.

encode_starttls_proceed({starttls_proceed},
			_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"proceed">>, _attrs, _els}.

decode_starttls(__TopXMLNS, __IgnoreEls,
		{xmlel, <<"starttls">>, _attrs, _els}) ->
    Required = decode_starttls_els(__TopXMLNS, __IgnoreEls,
				   _els, false),
    {starttls, Required}.

decode_starttls_els(__TopXMLNS, __IgnoreEls, [],
		    Required) ->
    Required;
decode_starttls_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"required">>, _attrs, _} = _el | _els],
		    Required) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_starttls_els(__TopXMLNS, __IgnoreEls, _els,
			       decode_starttls_required(__TopXMLNS, __IgnoreEls,
							_el));
       true ->
	   decode_starttls_els(__TopXMLNS, __IgnoreEls, _els,
			       Required)
    end;
decode_starttls_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		    Required) ->
    decode_starttls_els(__TopXMLNS, __IgnoreEls, _els,
			Required).

encode_starttls({starttls, Required}, _xmlns_attrs) ->
    _els =
	lists:reverse('encode_starttls_$required'(Required,
						  [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"starttls">>, _attrs, _els}.

'encode_starttls_$required'(false, _acc) -> _acc;
'encode_starttls_$required'(Required, _acc) ->
    [encode_starttls_required(Required, []) | _acc].

decode_starttls_required(__TopXMLNS, __IgnoreEls,
			 {xmlel, <<"required">>, _attrs, _els}) ->
    true.

encode_starttls_required(true, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"required">>, _attrs, _els}.

decode_sasl_mechanisms(__TopXMLNS, __IgnoreEls,
		       {xmlel, <<"mechanisms">>, _attrs, _els}) ->
    List = decode_sasl_mechanisms_els(__TopXMLNS,
				      __IgnoreEls, _els, []),
    {sasl_mechanisms, List}.

decode_sasl_mechanisms_els(__TopXMLNS, __IgnoreEls, [],
			   List) ->
    lists:reverse(List);
decode_sasl_mechanisms_els(__TopXMLNS, __IgnoreEls,
			   [{xmlel, <<"mechanism">>, _attrs, _} = _el | _els],
			   List) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_sasl_mechanisms_els(__TopXMLNS, __IgnoreEls,
				      _els,
				      case decode_sasl_mechanism(__TopXMLNS,
								 __IgnoreEls,
								 _el)
					  of
					undefined -> List;
					_new_el -> [_new_el | List]
				      end);
       true ->
	   decode_sasl_mechanisms_els(__TopXMLNS, __IgnoreEls,
				      _els, List)
    end;
decode_sasl_mechanisms_els(__TopXMLNS, __IgnoreEls,
			   [_ | _els], List) ->
    decode_sasl_mechanisms_els(__TopXMLNS, __IgnoreEls,
			       _els, List).

encode_sasl_mechanisms({sasl_mechanisms, List},
		       _xmlns_attrs) ->
    _els =
	lists:reverse('encode_sasl_mechanisms_$list'(List, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"mechanisms">>, _attrs, _els}.

'encode_sasl_mechanisms_$list'([], _acc) -> _acc;
'encode_sasl_mechanisms_$list'([List | _els], _acc) ->
    'encode_sasl_mechanisms_$list'(_els,
				   [encode_sasl_mechanism(List, []) | _acc]).

decode_sasl_mechanism(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"mechanism">>, _attrs, _els}) ->
    Cdata = decode_sasl_mechanism_els(__TopXMLNS,
				      __IgnoreEls, _els, <<>>),
    Cdata.

decode_sasl_mechanism_els(__TopXMLNS, __IgnoreEls, [],
			  Cdata) ->
    decode_sasl_mechanism_cdata(__TopXMLNS, Cdata);
decode_sasl_mechanism_els(__TopXMLNS, __IgnoreEls,
			  [{xmlcdata, _data} | _els], Cdata) ->
    decode_sasl_mechanism_els(__TopXMLNS, __IgnoreEls, _els,
			      <<Cdata/binary, _data/binary>>);
decode_sasl_mechanism_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Cdata) ->
    decode_sasl_mechanism_els(__TopXMLNS, __IgnoreEls, _els,
			      Cdata).

encode_sasl_mechanism(Cdata, _xmlns_attrs) ->
    _els = encode_sasl_mechanism_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"mechanism">>, _attrs, _els}.

decode_sasl_mechanism_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_sasl_mechanism_cdata(__TopXMLNS, _val) -> _val.

encode_sasl_mechanism_cdata(undefined, _acc) -> _acc;
encode_sasl_mechanism_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_sasl_failure(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"failure">>, _attrs, _els}) ->
    {Text, Reason} = decode_sasl_failure_els(__TopXMLNS,
					     __IgnoreEls, _els, [], undefined),
    {sasl_failure, Reason, Text}.

decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, [],
			Text, Reason) ->
    {lists:reverse(Text), Reason};
decode_sasl_failure_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"text">>, _attrs, _} = _el | _els], Text,
			Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   [decode_sasl_failure_text(__TopXMLNS,
							     __IgnoreEls, _el)
				    | Text],
				   Reason);
       true ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_sasl_failure_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"aborted">>, _attrs, _} = _el | _els], Text,
			Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_sasl_failure_aborted(__TopXMLNS,
							       __IgnoreEls,
							       _el));
       true ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_sasl_failure_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"account-disabled">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_sasl_failure_account_disabled(__TopXMLNS,
									__IgnoreEls,
									_el));
       true ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_sasl_failure_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"credentials-expired">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_sasl_failure_credentials_expired(__TopXMLNS,
									   __IgnoreEls,
									   _el));
       true ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_sasl_failure_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"encryption-required">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_sasl_failure_encryption_required(__TopXMLNS,
									   __IgnoreEls,
									   _el));
       true ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_sasl_failure_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"incorrect-encoding">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_sasl_failure_incorrect_encoding(__TopXMLNS,
									  __IgnoreEls,
									  _el));
       true ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_sasl_failure_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"invalid-authzid">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_sasl_failure_invalid_authzid(__TopXMLNS,
								       __IgnoreEls,
								       _el));
       true ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_sasl_failure_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"invalid-mechanism">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_sasl_failure_invalid_mechanism(__TopXMLNS,
									 __IgnoreEls,
									 _el));
       true ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_sasl_failure_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"malformed-request">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_sasl_failure_malformed_request(__TopXMLNS,
									 __IgnoreEls,
									 _el));
       true ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_sasl_failure_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"mechanism-too-weak">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_sasl_failure_mechanism_too_weak(__TopXMLNS,
									  __IgnoreEls,
									  _el));
       true ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_sasl_failure_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"not-authorized">>, _attrs, _} = _el | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_sasl_failure_not_authorized(__TopXMLNS,
								      __IgnoreEls,
								      _el));
       true ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_sasl_failure_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"temporary-auth-failure">>, _attrs, _} = _el
			 | _els],
			Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text,
				   decode_sasl_failure_temporary_auth_failure(__TopXMLNS,
									      __IgnoreEls,
									      _el));
       true ->
	   decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
				   Text, Reason)
    end;
decode_sasl_failure_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Text, Reason) ->
    decode_sasl_failure_els(__TopXMLNS, __IgnoreEls, _els,
			    Text, Reason).

encode_sasl_failure({sasl_failure, Reason, Text},
		    _xmlns_attrs) ->
    _els = lists:reverse('encode_sasl_failure_$text'(Text,
						     'encode_sasl_failure_$reason'(Reason,
										   []))),
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

decode_sasl_failure_temporary_auth_failure(__TopXMLNS,
					   __IgnoreEls,
					   {xmlel, <<"temporary-auth-failure">>,
					    _attrs, _els}) ->
    'temporary-auth-failure'.

encode_sasl_failure_temporary_auth_failure('temporary-auth-failure',
					   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"temporary-auth-failure">>, _attrs, _els}.

decode_sasl_failure_not_authorized(__TopXMLNS,
				   __IgnoreEls,
				   {xmlel, <<"not-authorized">>, _attrs,
				    _els}) ->
    'not-authorized'.

encode_sasl_failure_not_authorized('not-authorized',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"not-authorized">>, _attrs, _els}.

decode_sasl_failure_mechanism_too_weak(__TopXMLNS,
				       __IgnoreEls,
				       {xmlel, <<"mechanism-too-weak">>, _attrs,
					_els}) ->
    'mechanism-too-weak'.

encode_sasl_failure_mechanism_too_weak('mechanism-too-weak',
				       _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"mechanism-too-weak">>, _attrs, _els}.

decode_sasl_failure_malformed_request(__TopXMLNS,
				      __IgnoreEls,
				      {xmlel, <<"malformed-request">>, _attrs,
				       _els}) ->
    'malformed-request'.

encode_sasl_failure_malformed_request('malformed-request',
				      _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"malformed-request">>, _attrs, _els}.

decode_sasl_failure_invalid_mechanism(__TopXMLNS,
				      __IgnoreEls,
				      {xmlel, <<"invalid-mechanism">>, _attrs,
				       _els}) ->
    'invalid-mechanism'.

encode_sasl_failure_invalid_mechanism('invalid-mechanism',
				      _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"invalid-mechanism">>, _attrs, _els}.

decode_sasl_failure_invalid_authzid(__TopXMLNS,
				    __IgnoreEls,
				    {xmlel, <<"invalid-authzid">>, _attrs,
				     _els}) ->
    'invalid-authzid'.

encode_sasl_failure_invalid_authzid('invalid-authzid',
				    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"invalid-authzid">>, _attrs, _els}.

decode_sasl_failure_incorrect_encoding(__TopXMLNS,
				       __IgnoreEls,
				       {xmlel, <<"incorrect-encoding">>, _attrs,
					_els}) ->
    'incorrect-encoding'.

encode_sasl_failure_incorrect_encoding('incorrect-encoding',
				       _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"incorrect-encoding">>, _attrs, _els}.

decode_sasl_failure_encryption_required(__TopXMLNS,
					__IgnoreEls,
					{xmlel, <<"encryption-required">>,
					 _attrs, _els}) ->
    'encryption-required'.

encode_sasl_failure_encryption_required('encryption-required',
					_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"encryption-required">>, _attrs, _els}.

decode_sasl_failure_credentials_expired(__TopXMLNS,
					__IgnoreEls,
					{xmlel, <<"credentials-expired">>,
					 _attrs, _els}) ->
    'credentials-expired'.

encode_sasl_failure_credentials_expired('credentials-expired',
					_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"credentials-expired">>, _attrs, _els}.

decode_sasl_failure_account_disabled(__TopXMLNS,
				     __IgnoreEls,
				     {xmlel, <<"account-disabled">>, _attrs,
				      _els}) ->
    'account-disabled'.

encode_sasl_failure_account_disabled('account-disabled',
				     _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"account-disabled">>, _attrs, _els}.

decode_sasl_failure_aborted(__TopXMLNS, __IgnoreEls,
			    {xmlel, <<"aborted">>, _attrs, _els}) ->
    aborted.

encode_sasl_failure_aborted(aborted, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"aborted">>, _attrs, _els}.

decode_sasl_failure_text(__TopXMLNS, __IgnoreEls,
			 {xmlel, <<"text">>, _attrs, _els}) ->
    Data = decode_sasl_failure_text_els(__TopXMLNS,
					__IgnoreEls, _els, <<>>),
    Lang = decode_sasl_failure_text_attrs(__TopXMLNS,
					  _attrs, undefined),
    {text, Lang, Data}.

decode_sasl_failure_text_els(__TopXMLNS, __IgnoreEls,
			     [], Data) ->
    decode_sasl_failure_text_cdata(__TopXMLNS, Data);
decode_sasl_failure_text_els(__TopXMLNS, __IgnoreEls,
			     [{xmlcdata, _data} | _els], Data) ->
    decode_sasl_failure_text_els(__TopXMLNS, __IgnoreEls,
				 _els, <<Data/binary, _data/binary>>);
decode_sasl_failure_text_els(__TopXMLNS, __IgnoreEls,
			     [_ | _els], Data) ->
    decode_sasl_failure_text_els(__TopXMLNS, __IgnoreEls,
				 _els, Data).

decode_sasl_failure_text_attrs(__TopXMLNS,
			       [{<<"xml:lang">>, _val} | _attrs], _Lang) ->
    decode_sasl_failure_text_attrs(__TopXMLNS, _attrs,
				   _val);
decode_sasl_failure_text_attrs(__TopXMLNS, [_ | _attrs],
			       Lang) ->
    decode_sasl_failure_text_attrs(__TopXMLNS, _attrs,
				   Lang);
decode_sasl_failure_text_attrs(__TopXMLNS, [], Lang) ->
    'decode_sasl_failure_text_attr_xml:lang'(__TopXMLNS,
					     Lang).

encode_sasl_failure_text({text, Lang, Data},
			 _xmlns_attrs) ->
    _els = encode_sasl_failure_text_cdata(Data, []),
    _attrs = 'encode_sasl_failure_text_attr_xml:lang'(Lang,
						      _xmlns_attrs),
    {xmlel, <<"text">>, _attrs, _els}.

'decode_sasl_failure_text_attr_xml:lang'(__TopXMLNS,
					 undefined) ->
    undefined;
'decode_sasl_failure_text_attr_xml:lang'(__TopXMLNS,
					 _val) ->
    _val.

'encode_sasl_failure_text_attr_xml:lang'(undefined,
					 _acc) ->
    _acc;
'encode_sasl_failure_text_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_sasl_failure_text_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_sasl_failure_text_cdata(__TopXMLNS, _val) ->
    _val.

encode_sasl_failure_text_cdata(undefined, _acc) -> _acc;
encode_sasl_failure_text_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_sasl_success(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"success">>, _attrs, _els}) ->
    Text = decode_sasl_success_els(__TopXMLNS, __IgnoreEls,
				   _els, <<>>),
    {sasl_success, Text}.

decode_sasl_success_els(__TopXMLNS, __IgnoreEls, [],
			Text) ->
    decode_sasl_success_cdata(__TopXMLNS, Text);
decode_sasl_success_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Text) ->
    decode_sasl_success_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Text/binary, _data/binary>>);
decode_sasl_success_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Text) ->
    decode_sasl_success_els(__TopXMLNS, __IgnoreEls, _els,
			    Text).

encode_sasl_success({sasl_success, Text},
		    _xmlns_attrs) ->
    _els = encode_sasl_success_cdata(Text, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"success">>, _attrs, _els}.

decode_sasl_success_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_sasl_success_cdata(__TopXMLNS, _val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"success">>, __TopXMLNS}});
      _res -> _res
    end.

encode_sasl_success_cdata(undefined, _acc) -> _acc;
encode_sasl_success_cdata(_val, _acc) ->
    [{xmlcdata, base64:encode(_val)} | _acc].

decode_sasl_response(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"response">>, _attrs, _els}) ->
    Text = decode_sasl_response_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    {sasl_response, Text}.

decode_sasl_response_els(__TopXMLNS, __IgnoreEls, [],
			 Text) ->
    decode_sasl_response_cdata(__TopXMLNS, Text);
decode_sasl_response_els(__TopXMLNS, __IgnoreEls,
			 [{xmlcdata, _data} | _els], Text) ->
    decode_sasl_response_els(__TopXMLNS, __IgnoreEls, _els,
			     <<Text/binary, _data/binary>>);
decode_sasl_response_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Text) ->
    decode_sasl_response_els(__TopXMLNS, __IgnoreEls, _els,
			     Text).

encode_sasl_response({sasl_response, Text},
		     _xmlns_attrs) ->
    _els = encode_sasl_response_cdata(Text, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"response">>, _attrs, _els}.

decode_sasl_response_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_sasl_response_cdata(__TopXMLNS, _val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"response">>, __TopXMLNS}});
      _res -> _res
    end.

encode_sasl_response_cdata(undefined, _acc) -> _acc;
encode_sasl_response_cdata(_val, _acc) ->
    [{xmlcdata, base64:encode(_val)} | _acc].

decode_sasl_challenge(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"challenge">>, _attrs, _els}) ->
    Text = decode_sasl_challenge_els(__TopXMLNS,
				     __IgnoreEls, _els, <<>>),
    {sasl_challenge, Text}.

decode_sasl_challenge_els(__TopXMLNS, __IgnoreEls, [],
			  Text) ->
    decode_sasl_challenge_cdata(__TopXMLNS, Text);
decode_sasl_challenge_els(__TopXMLNS, __IgnoreEls,
			  [{xmlcdata, _data} | _els], Text) ->
    decode_sasl_challenge_els(__TopXMLNS, __IgnoreEls, _els,
			      <<Text/binary, _data/binary>>);
decode_sasl_challenge_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Text) ->
    decode_sasl_challenge_els(__TopXMLNS, __IgnoreEls, _els,
			      Text).

encode_sasl_challenge({sasl_challenge, Text},
		      _xmlns_attrs) ->
    _els = encode_sasl_challenge_cdata(Text, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"challenge">>, _attrs, _els}.

decode_sasl_challenge_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_sasl_challenge_cdata(__TopXMLNS, _val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"challenge">>, __TopXMLNS}});
      _res -> _res
    end.

encode_sasl_challenge_cdata(undefined, _acc) -> _acc;
encode_sasl_challenge_cdata(_val, _acc) ->
    [{xmlcdata, base64:encode(_val)} | _acc].

decode_sasl_abort(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"abort">>, _attrs, _els}) ->
    {sasl_abort}.

encode_sasl_abort({sasl_abort}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"abort">>, _attrs, _els}.

decode_sasl_auth(__TopXMLNS, __IgnoreEls,
		 {xmlel, <<"auth">>, _attrs, _els}) ->
    Text = decode_sasl_auth_els(__TopXMLNS, __IgnoreEls,
				_els, <<>>),
    Mechanism = decode_sasl_auth_attrs(__TopXMLNS, _attrs,
				       undefined),
    {sasl_auth, Mechanism, Text}.

decode_sasl_auth_els(__TopXMLNS, __IgnoreEls, [],
		     Text) ->
    decode_sasl_auth_cdata(__TopXMLNS, Text);
decode_sasl_auth_els(__TopXMLNS, __IgnoreEls,
		     [{xmlcdata, _data} | _els], Text) ->
    decode_sasl_auth_els(__TopXMLNS, __IgnoreEls, _els,
			 <<Text/binary, _data/binary>>);
decode_sasl_auth_els(__TopXMLNS, __IgnoreEls,
		     [_ | _els], Text) ->
    decode_sasl_auth_els(__TopXMLNS, __IgnoreEls, _els,
			 Text).

decode_sasl_auth_attrs(__TopXMLNS,
		       [{<<"mechanism">>, _val} | _attrs], _Mechanism) ->
    decode_sasl_auth_attrs(__TopXMLNS, _attrs, _val);
decode_sasl_auth_attrs(__TopXMLNS, [_ | _attrs],
		       Mechanism) ->
    decode_sasl_auth_attrs(__TopXMLNS, _attrs, Mechanism);
decode_sasl_auth_attrs(__TopXMLNS, [], Mechanism) ->
    decode_sasl_auth_attr_mechanism(__TopXMLNS, Mechanism).

encode_sasl_auth({sasl_auth, Mechanism, Text},
		 _xmlns_attrs) ->
    _els = encode_sasl_auth_cdata(Text, []),
    _attrs = encode_sasl_auth_attr_mechanism(Mechanism,
					     _xmlns_attrs),
    {xmlel, <<"auth">>, _attrs, _els}.

decode_sasl_auth_attr_mechanism(__TopXMLNS,
				undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"mechanism">>, <<"auth">>,
		   __TopXMLNS}});
decode_sasl_auth_attr_mechanism(__TopXMLNS, _val) ->
    _val.

encode_sasl_auth_attr_mechanism(_val, _acc) ->
    [{<<"mechanism">>, _val} | _acc].

decode_sasl_auth_cdata(__TopXMLNS, <<>>) -> undefined;
decode_sasl_auth_cdata(__TopXMLNS, _val) ->
    case catch base64:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"auth">>, __TopXMLNS}});
      _res -> _res
    end.

encode_sasl_auth_cdata(undefined, _acc) -> _acc;
encode_sasl_auth_cdata(_val, _acc) ->
    [{xmlcdata, base64:encode(_val)} | _acc].

decode_bind(__TopXMLNS, __IgnoreEls,
	    {xmlel, <<"bind">>, _attrs, _els}) ->
    {Jid, Resource} = decode_bind_els(__TopXMLNS,
				      __IgnoreEls, _els, undefined, undefined),
    {bind, Jid, Resource}.

decode_bind_els(__TopXMLNS, __IgnoreEls, [], Jid,
		Resource) ->
    {Jid, Resource};
decode_bind_els(__TopXMLNS, __IgnoreEls,
		[{xmlel, <<"jid">>, _attrs, _} = _el | _els], Jid,
		Resource) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_bind_els(__TopXMLNS, __IgnoreEls, _els,
			   decode_bind_jid(__TopXMLNS, __IgnoreEls, _el),
			   Resource);
       true ->
	   decode_bind_els(__TopXMLNS, __IgnoreEls, _els, Jid,
			   Resource)
    end;
decode_bind_els(__TopXMLNS, __IgnoreEls,
		[{xmlel, <<"resource">>, _attrs, _} = _el | _els], Jid,
		Resource) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_bind_els(__TopXMLNS, __IgnoreEls, _els, Jid,
			   decode_bind_resource(__TopXMLNS, __IgnoreEls, _el));
       true ->
	   decode_bind_els(__TopXMLNS, __IgnoreEls, _els, Jid,
			   Resource)
    end;
decode_bind_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		Jid, Resource) ->
    decode_bind_els(__TopXMLNS, __IgnoreEls, _els, Jid,
		    Resource).

encode_bind({bind, Jid, Resource}, _xmlns_attrs) ->
    _els = lists:reverse('encode_bind_$jid'(Jid,
					    'encode_bind_$resource'(Resource,
								    []))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"bind">>, _attrs, _els}.

'encode_bind_$jid'(undefined, _acc) -> _acc;
'encode_bind_$jid'(Jid, _acc) ->
    [encode_bind_jid(Jid, []) | _acc].

'encode_bind_$resource'(undefined, _acc) -> _acc;
'encode_bind_$resource'(Resource, _acc) ->
    [encode_bind_resource(Resource, []) | _acc].

decode_bind_resource(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"resource">>, _attrs, _els}) ->
    Cdata = decode_bind_resource_els(__TopXMLNS,
				     __IgnoreEls, _els, <<>>),
    Cdata.

decode_bind_resource_els(__TopXMLNS, __IgnoreEls, [],
			 Cdata) ->
    decode_bind_resource_cdata(__TopXMLNS, Cdata);
decode_bind_resource_els(__TopXMLNS, __IgnoreEls,
			 [{xmlcdata, _data} | _els], Cdata) ->
    decode_bind_resource_els(__TopXMLNS, __IgnoreEls, _els,
			     <<Cdata/binary, _data/binary>>);
decode_bind_resource_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Cdata) ->
    decode_bind_resource_els(__TopXMLNS, __IgnoreEls, _els,
			     Cdata).

encode_bind_resource(Cdata, _xmlns_attrs) ->
    _els = encode_bind_resource_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"resource">>, _attrs, _els}.

decode_bind_resource_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_bind_resource_cdata(__TopXMLNS, _val) ->
    case catch resourceprep(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"resource">>, __TopXMLNS}});
      _res -> _res
    end.

encode_bind_resource_cdata(undefined, _acc) -> _acc;
encode_bind_resource_cdata(_val, _acc) ->
    [{xmlcdata, resourceprep(_val)} | _acc].

decode_bind_jid(__TopXMLNS, __IgnoreEls,
		{xmlel, <<"jid">>, _attrs, _els}) ->
    Cdata = decode_bind_jid_els(__TopXMLNS, __IgnoreEls,
				_els, <<>>),
    Cdata.

decode_bind_jid_els(__TopXMLNS, __IgnoreEls, [],
		    Cdata) ->
    decode_bind_jid_cdata(__TopXMLNS, Cdata);
decode_bind_jid_els(__TopXMLNS, __IgnoreEls,
		    [{xmlcdata, _data} | _els], Cdata) ->
    decode_bind_jid_els(__TopXMLNS, __IgnoreEls, _els,
			<<Cdata/binary, _data/binary>>);
decode_bind_jid_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		    Cdata) ->
    decode_bind_jid_els(__TopXMLNS, __IgnoreEls, _els,
			Cdata).

encode_bind_jid(Cdata, _xmlns_attrs) ->
    _els = encode_bind_jid_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"jid">>, _attrs, _els}.

decode_bind_jid_cdata(__TopXMLNS, <<>>) -> undefined;
decode_bind_jid_cdata(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"jid">>, __TopXMLNS}});
      _res -> _res
    end.

encode_bind_jid_cdata(undefined, _acc) -> _acc;
encode_bind_jid_cdata(_val, _acc) ->
    [{xmlcdata, enc_jid(_val)} | _acc].

decode_error(__TopXMLNS, __IgnoreEls,
	     {xmlel, <<"error">>, _attrs, _els}) ->
    {Text, Reason} = decode_error_els(__TopXMLNS,
				      __IgnoreEls, _els, undefined, undefined),
    {Type, By} = decode_error_attrs(__TopXMLNS, _attrs,
				    undefined, undefined),
    {error, Type, By, Reason, Text}.

decode_error_els(__TopXMLNS, __IgnoreEls, [], Text,
		 Reason) ->
    {Text, Reason};
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"text">>, _attrs, _} = _el | _els], Text,
		 Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els,
			    decode_error_text(_xmlns, __IgnoreEls, _el),
			    Reason);
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"bad-request">>, _attrs, _} = _el | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_bad_request(_xmlns, __IgnoreEls, _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"conflict">>, _attrs, _} = _el | _els], Text,
		 Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_conflict(_xmlns, __IgnoreEls, _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"feature-not-implemented">>, _attrs, _} = _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_feature_not_implemented(_xmlns,
								 __IgnoreEls,
								 _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"forbidden">>, _attrs, _} = _el | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_forbidden(_xmlns, __IgnoreEls, _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"gone">>, _attrs, _} = _el | _els], Text,
		 Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_gone(_xmlns, __IgnoreEls, _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"internal-server-error">>, _attrs, _} = _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_internal_server_error(_xmlns,
							       __IgnoreEls,
							       _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"item-not-found">>, _attrs, _} = _el | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_item_not_found(_xmlns, __IgnoreEls,
							_el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"jid-malformed">>, _attrs, _} = _el | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_jid_malformed(_xmlns, __IgnoreEls,
						       _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"not-acceptable">>, _attrs, _} = _el | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_not_acceptable(_xmlns, __IgnoreEls,
							_el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"not-allowed">>, _attrs, _} = _el | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_not_allowed(_xmlns, __IgnoreEls, _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"not-authorized">>, _attrs, _} = _el | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_not_authorized(_xmlns, __IgnoreEls,
							_el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"policy-violation">>, _attrs, _} = _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_policy_violation(_xmlns, __IgnoreEls,
							  _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"recipient-unavailable">>, _attrs, _} = _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_recipient_unavailable(_xmlns,
							       __IgnoreEls,
							       _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"redirect">>, _attrs, _} = _el | _els], Text,
		 Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_redirect(_xmlns, __IgnoreEls, _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"registration-required">>, _attrs, _} = _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_registration_required(_xmlns,
							       __IgnoreEls,
							       _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"remote-server-not-found">>, _attrs, _} = _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_remote_server_not_found(_xmlns,
								 __IgnoreEls,
								 _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"remote-server-timeout">>, _attrs, _} = _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_remote_server_timeout(_xmlns,
							       __IgnoreEls,
							       _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"resource-constraint">>, _attrs, _} = _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_resource_constraint(_xmlns,
							     __IgnoreEls, _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"service-unavailable">>, _attrs, _} = _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_service_unavailable(_xmlns,
							     __IgnoreEls, _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"subscription-required">>, _attrs, _} = _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_subscription_required(_xmlns,
							       __IgnoreEls,
							       _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"undefined-condition">>, _attrs, _} = _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_undefined_condition(_xmlns,
							     __IgnoreEls, _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"unexpected-request">>, _attrs, _} = _el
		  | _els],
		 Text, Reason) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns ==
	 <<"urn:ietf:params:xml:ns:xmpp-stanzas">> ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    decode_error_unexpected_request(_xmlns, __IgnoreEls,
							    _el));
       true ->
	   decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
			    Reason)
    end;
decode_error_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		 Text, Reason) ->
    decode_error_els(__TopXMLNS, __IgnoreEls, _els, Text,
		     Reason).

decode_error_attrs(__TopXMLNS,
		   [{<<"type">>, _val} | _attrs], _Type, By) ->
    decode_error_attrs(__TopXMLNS, _attrs, _val, By);
decode_error_attrs(__TopXMLNS,
		   [{<<"by">>, _val} | _attrs], Type, _By) ->
    decode_error_attrs(__TopXMLNS, _attrs, Type, _val);
decode_error_attrs(__TopXMLNS, [_ | _attrs], Type,
		   By) ->
    decode_error_attrs(__TopXMLNS, _attrs, Type, By);
decode_error_attrs(__TopXMLNS, [], Type, By) ->
    {decode_error_attr_type(__TopXMLNS, Type),
     decode_error_attr_by(__TopXMLNS, By)}.

encode_error({error, Type, By, Reason, Text},
	     _xmlns_attrs) ->
    _els = lists:reverse('encode_error_$text'(Text,
					      'encode_error_$reason'(Reason,
								     []))),
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

decode_error_attr_type(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"type">>, <<"error">>, __TopXMLNS}});
decode_error_attr_type(__TopXMLNS, _val) ->
    case catch dec_enum(_val,
			[auth, cancel, continue, modify, wait])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"type">>, <<"error">>, __TopXMLNS}});
      _res -> _res
    end.

encode_error_attr_type(_val, _acc) ->
    [{<<"type">>, enc_enum(_val)} | _acc].

decode_error_attr_by(__TopXMLNS, undefined) ->
    undefined;
decode_error_attr_by(__TopXMLNS, _val) -> _val.

encode_error_attr_by(undefined, _acc) -> _acc;
encode_error_attr_by(_val, _acc) ->
    [{<<"by">>, _val} | _acc].

decode_error_text(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"text">>, _attrs, _els}) ->
    Data = decode_error_text_els(__TopXMLNS, __IgnoreEls,
				 _els, <<>>),
    Lang = decode_error_text_attrs(__TopXMLNS, _attrs,
				   undefined),
    {text, Lang, Data}.

decode_error_text_els(__TopXMLNS, __IgnoreEls, [],
		      Data) ->
    decode_error_text_cdata(__TopXMLNS, Data);
decode_error_text_els(__TopXMLNS, __IgnoreEls,
		      [{xmlcdata, _data} | _els], Data) ->
    decode_error_text_els(__TopXMLNS, __IgnoreEls, _els,
			  <<Data/binary, _data/binary>>);
decode_error_text_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Data) ->
    decode_error_text_els(__TopXMLNS, __IgnoreEls, _els,
			  Data).

decode_error_text_attrs(__TopXMLNS,
			[{<<"xml:lang">>, _val} | _attrs], _Lang) ->
    decode_error_text_attrs(__TopXMLNS, _attrs, _val);
decode_error_text_attrs(__TopXMLNS, [_ | _attrs],
			Lang) ->
    decode_error_text_attrs(__TopXMLNS, _attrs, Lang);
decode_error_text_attrs(__TopXMLNS, [], Lang) ->
    'decode_error_text_attr_xml:lang'(__TopXMLNS, Lang).

encode_error_text({text, Lang, Data}, _xmlns_attrs) ->
    _els = encode_error_text_cdata(Data, []),
    _attrs = 'encode_error_text_attr_xml:lang'(Lang,
					       _xmlns_attrs),
    {xmlel, <<"text">>, _attrs, _els}.

'decode_error_text_attr_xml:lang'(__TopXMLNS,
				  undefined) ->
    undefined;
'decode_error_text_attr_xml:lang'(__TopXMLNS, _val) ->
    _val.

'encode_error_text_attr_xml:lang'(undefined, _acc) ->
    _acc;
'encode_error_text_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_error_text_cdata(__TopXMLNS, <<>>) -> undefined;
decode_error_text_cdata(__TopXMLNS, _val) -> _val.

encode_error_text_cdata(undefined, _acc) -> _acc;
encode_error_text_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_error_unexpected_request(__TopXMLNS, __IgnoreEls,
				{xmlel, <<"unexpected-request">>, _attrs,
				 _els}) ->
    'unexpected-request'.

encode_error_unexpected_request('unexpected-request',
				_xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"unexpected-request">>, _attrs, _els}.

decode_error_undefined_condition(__TopXMLNS,
				 __IgnoreEls,
				 {xmlel, <<"undefined-condition">>, _attrs,
				  _els}) ->
    'undefined-condition'.

encode_error_undefined_condition('undefined-condition',
				 _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"undefined-condition">>, _attrs, _els}.

decode_error_subscription_required(__TopXMLNS,
				   __IgnoreEls,
				   {xmlel, <<"subscription-required">>, _attrs,
				    _els}) ->
    'subscription-required'.

encode_error_subscription_required('subscription-required',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"subscription-required">>, _attrs, _els}.

decode_error_service_unavailable(__TopXMLNS,
				 __IgnoreEls,
				 {xmlel, <<"service-unavailable">>, _attrs,
				  _els}) ->
    'service-unavailable'.

encode_error_service_unavailable('service-unavailable',
				 _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"service-unavailable">>, _attrs, _els}.

decode_error_resource_constraint(__TopXMLNS,
				 __IgnoreEls,
				 {xmlel, <<"resource-constraint">>, _attrs,
				  _els}) ->
    'resource-constraint'.

encode_error_resource_constraint('resource-constraint',
				 _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"resource-constraint">>, _attrs, _els}.

decode_error_remote_server_timeout(__TopXMLNS,
				   __IgnoreEls,
				   {xmlel, <<"remote-server-timeout">>, _attrs,
				    _els}) ->
    'remote-server-timeout'.

encode_error_remote_server_timeout('remote-server-timeout',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"remote-server-timeout">>, _attrs, _els}.

decode_error_remote_server_not_found(__TopXMLNS,
				     __IgnoreEls,
				     {xmlel, <<"remote-server-not-found">>,
				      _attrs, _els}) ->
    'remote-server-not-found'.

encode_error_remote_server_not_found('remote-server-not-found',
				     _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"remote-server-not-found">>, _attrs, _els}.

decode_error_registration_required(__TopXMLNS,
				   __IgnoreEls,
				   {xmlel, <<"registration-required">>, _attrs,
				    _els}) ->
    'registration-required'.

encode_error_registration_required('registration-required',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"registration-required">>, _attrs, _els}.

decode_error_redirect(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"redirect">>, _attrs, _els}) ->
    Uri = decode_error_redirect_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    {redirect, Uri}.

decode_error_redirect_els(__TopXMLNS, __IgnoreEls, [],
			  Uri) ->
    decode_error_redirect_cdata(__TopXMLNS, Uri);
decode_error_redirect_els(__TopXMLNS, __IgnoreEls,
			  [{xmlcdata, _data} | _els], Uri) ->
    decode_error_redirect_els(__TopXMLNS, __IgnoreEls, _els,
			      <<Uri/binary, _data/binary>>);
decode_error_redirect_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Uri) ->
    decode_error_redirect_els(__TopXMLNS, __IgnoreEls, _els,
			      Uri).

encode_error_redirect({redirect, Uri}, _xmlns_attrs) ->
    _els = encode_error_redirect_cdata(Uri, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"redirect">>, _attrs, _els}.

decode_error_redirect_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_error_redirect_cdata(__TopXMLNS, _val) -> _val.

encode_error_redirect_cdata(undefined, _acc) -> _acc;
encode_error_redirect_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_error_recipient_unavailable(__TopXMLNS,
				   __IgnoreEls,
				   {xmlel, <<"recipient-unavailable">>, _attrs,
				    _els}) ->
    'recipient-unavailable'.

encode_error_recipient_unavailable('recipient-unavailable',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"recipient-unavailable">>, _attrs, _els}.

decode_error_policy_violation(__TopXMLNS, __IgnoreEls,
			      {xmlel, <<"policy-violation">>, _attrs, _els}) ->
    'policy-violation'.

encode_error_policy_violation('policy-violation',
			      _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"policy-violation">>, _attrs, _els}.

decode_error_not_authorized(__TopXMLNS, __IgnoreEls,
			    {xmlel, <<"not-authorized">>, _attrs, _els}) ->
    'not-authorized'.

encode_error_not_authorized('not-authorized',
			    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"not-authorized">>, _attrs, _els}.

decode_error_not_allowed(__TopXMLNS, __IgnoreEls,
			 {xmlel, <<"not-allowed">>, _attrs, _els}) ->
    'not-allowed'.

encode_error_not_allowed('not-allowed', _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"not-allowed">>, _attrs, _els}.

decode_error_not_acceptable(__TopXMLNS, __IgnoreEls,
			    {xmlel, <<"not-acceptable">>, _attrs, _els}) ->
    'not-acceptable'.

encode_error_not_acceptable('not-acceptable',
			    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"not-acceptable">>, _attrs, _els}.

decode_error_jid_malformed(__TopXMLNS, __IgnoreEls,
			   {xmlel, <<"jid-malformed">>, _attrs, _els}) ->
    'jid-malformed'.

encode_error_jid_malformed('jid-malformed',
			   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"jid-malformed">>, _attrs, _els}.

decode_error_item_not_found(__TopXMLNS, __IgnoreEls,
			    {xmlel, <<"item-not-found">>, _attrs, _els}) ->
    'item-not-found'.

encode_error_item_not_found('item-not-found',
			    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"item-not-found">>, _attrs, _els}.

decode_error_internal_server_error(__TopXMLNS,
				   __IgnoreEls,
				   {xmlel, <<"internal-server-error">>, _attrs,
				    _els}) ->
    'internal-server-error'.

encode_error_internal_server_error('internal-server-error',
				   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"internal-server-error">>, _attrs, _els}.

decode_error_gone(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"gone">>, _attrs, _els}) ->
    Uri = decode_error_gone_els(__TopXMLNS, __IgnoreEls,
				_els, <<>>),
    {gone, Uri}.

decode_error_gone_els(__TopXMLNS, __IgnoreEls, [],
		      Uri) ->
    decode_error_gone_cdata(__TopXMLNS, Uri);
decode_error_gone_els(__TopXMLNS, __IgnoreEls,
		      [{xmlcdata, _data} | _els], Uri) ->
    decode_error_gone_els(__TopXMLNS, __IgnoreEls, _els,
			  <<Uri/binary, _data/binary>>);
decode_error_gone_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Uri) ->
    decode_error_gone_els(__TopXMLNS, __IgnoreEls, _els,
			  Uri).

encode_error_gone({gone, Uri}, _xmlns_attrs) ->
    _els = encode_error_gone_cdata(Uri, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"gone">>, _attrs, _els}.

decode_error_gone_cdata(__TopXMLNS, <<>>) -> undefined;
decode_error_gone_cdata(__TopXMLNS, _val) -> _val.

encode_error_gone_cdata(undefined, _acc) -> _acc;
encode_error_gone_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_error_forbidden(__TopXMLNS, __IgnoreEls,
		       {xmlel, <<"forbidden">>, _attrs, _els}) ->
    forbidden.

encode_error_forbidden(forbidden, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"forbidden">>, _attrs, _els}.

decode_error_feature_not_implemented(__TopXMLNS,
				     __IgnoreEls,
				     {xmlel, <<"feature-not-implemented">>,
				      _attrs, _els}) ->
    'feature-not-implemented'.

encode_error_feature_not_implemented('feature-not-implemented',
				     _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"feature-not-implemented">>, _attrs, _els}.

decode_error_conflict(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"conflict">>, _attrs, _els}) ->
    conflict.

encode_error_conflict(conflict, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"conflict">>, _attrs, _els}.

decode_error_bad_request(__TopXMLNS, __IgnoreEls,
			 {xmlel, <<"bad-request">>, _attrs, _els}) ->
    'bad-request'.

encode_error_bad_request('bad-request', _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"bad-request">>, _attrs, _els}.

decode_presence(__TopXMLNS, __IgnoreEls,
		{xmlel, <<"presence">>, _attrs, _els}) ->
    {Error, Status, Show, Priority, __Els} =
	decode_presence_els(__TopXMLNS, __IgnoreEls, _els,
			    undefined, [], undefined, undefined, []),
    {Id, Type, From, To, Lang} =
	decode_presence_attrs(__TopXMLNS, _attrs, undefined,
			      undefined, undefined, undefined, undefined),
    {presence, Id, Type, Lang, From, To, Show, Status,
     Priority, Error, __Els}.

decode_presence_els(__TopXMLNS, __IgnoreEls, [], Error,
		    Status, Show, Priority, __Els) ->
    {Error, lists:reverse(Status), Show, Priority,
     lists:reverse(__Els)};
decode_presence_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"error">>, _attrs, _} = _el | _els], Error,
		    Status, Show, Priority, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_presence_els(__TopXMLNS, __IgnoreEls, _els,
			       decode_error(__TopXMLNS, __IgnoreEls, _el),
			       Status, Show, Priority, __Els);
       true ->
	   decode_presence_els(__TopXMLNS, __IgnoreEls, _els,
			       Error, Status, Show, Priority, __Els)
    end;
decode_presence_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"show">>, _attrs, _} = _el | _els], Error,
		    Status, Show, Priority, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_presence_els(__TopXMLNS, __IgnoreEls, _els,
			       Error, Status,
			       decode_presence_show(__TopXMLNS, __IgnoreEls,
						    _el),
			       Priority, __Els);
       true ->
	   decode_presence_els(__TopXMLNS, __IgnoreEls, _els,
			       Error, Status, Show, Priority, __Els)
    end;
decode_presence_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"status">>, _attrs, _} = _el | _els], Error,
		    Status, Show, Priority, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_presence_els(__TopXMLNS, __IgnoreEls, _els,
			       Error,
			       [decode_presence_status(__TopXMLNS, __IgnoreEls,
						       _el)
				| Status],
			       Show, Priority, __Els);
       true ->
	   decode_presence_els(__TopXMLNS, __IgnoreEls, _els,
			       Error, Status, Show, Priority, __Els)
    end;
decode_presence_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, <<"priority">>, _attrs, _} = _el | _els],
		    Error, Status, Show, Priority, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_presence_els(__TopXMLNS, __IgnoreEls, _els,
			       Error, Status, Show,
			       decode_presence_priority(__TopXMLNS, __IgnoreEls,
							_el),
			       __Els);
       true ->
	   decode_presence_els(__TopXMLNS, __IgnoreEls, _els,
			       Error, Status, Show, Priority, __Els)
    end;
decode_presence_els(__TopXMLNS, __IgnoreEls,
		    [{xmlel, _, _, _} = _el | _els], Error, Status, Show,
		    Priority, __Els) ->
    if __IgnoreEls ->
	   decode_presence_els(__TopXMLNS, __IgnoreEls, _els,
			       Error, Status, Show, Priority, [_el | __Els]);
       true ->
	   case is_known_tag(_el) of
	     true ->
		 decode_presence_els(__TopXMLNS, __IgnoreEls, _els,
				     Error, Status, Show, Priority,
				     [decode(_el) | __Els]);
	     false ->
		 decode_presence_els(__TopXMLNS, __IgnoreEls, _els,
				     Error, Status, Show, Priority, __Els)
	   end
    end;
decode_presence_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		    Error, Status, Show, Priority, __Els) ->
    decode_presence_els(__TopXMLNS, __IgnoreEls, _els,
			Error, Status, Show, Priority, __Els).

decode_presence_attrs(__TopXMLNS,
		      [{<<"id">>, _val} | _attrs], _Id, Type, From, To,
		      Lang) ->
    decode_presence_attrs(__TopXMLNS, _attrs, _val, Type,
			  From, To, Lang);
decode_presence_attrs(__TopXMLNS,
		      [{<<"type">>, _val} | _attrs], Id, _Type, From, To,
		      Lang) ->
    decode_presence_attrs(__TopXMLNS, _attrs, Id, _val,
			  From, To, Lang);
decode_presence_attrs(__TopXMLNS,
		      [{<<"from">>, _val} | _attrs], Id, Type, _From, To,
		      Lang) ->
    decode_presence_attrs(__TopXMLNS, _attrs, Id, Type,
			  _val, To, Lang);
decode_presence_attrs(__TopXMLNS,
		      [{<<"to">>, _val} | _attrs], Id, Type, From, _To,
		      Lang) ->
    decode_presence_attrs(__TopXMLNS, _attrs, Id, Type,
			  From, _val, Lang);
decode_presence_attrs(__TopXMLNS,
		      [{<<"xml:lang">>, _val} | _attrs], Id, Type, From, To,
		      _Lang) ->
    decode_presence_attrs(__TopXMLNS, _attrs, Id, Type,
			  From, To, _val);
decode_presence_attrs(__TopXMLNS, [_ | _attrs], Id,
		      Type, From, To, Lang) ->
    decode_presence_attrs(__TopXMLNS, _attrs, Id, Type,
			  From, To, Lang);
decode_presence_attrs(__TopXMLNS, [], Id, Type, From,
		      To, Lang) ->
    {decode_presence_attr_id(__TopXMLNS, Id),
     decode_presence_attr_type(__TopXMLNS, Type),
     decode_presence_attr_from(__TopXMLNS, From),
     decode_presence_attr_to(__TopXMLNS, To),
     'decode_presence_attr_xml:lang'(__TopXMLNS, Lang)}.

encode_presence({presence, Id, Type, Lang, From, To,
		 Show, Status, Priority, Error, __Els},
		_xmlns_attrs) ->
    _els = [encode(_el) || _el <- __Els] ++
	     lists:reverse('encode_presence_$error'(Error,
						    'encode_presence_$status'(Status,
									      'encode_presence_$show'(Show,
												      'encode_presence_$priority'(Priority,
																  []))))),
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

decode_presence_attr_id(__TopXMLNS, undefined) ->
    undefined;
decode_presence_attr_id(__TopXMLNS, _val) -> _val.

encode_presence_attr_id(undefined, _acc) -> _acc;
encode_presence_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_presence_attr_type(__TopXMLNS, undefined) ->
    undefined;
decode_presence_attr_type(__TopXMLNS, _val) ->
    case catch dec_enum(_val,
			[unavailable, subscribe, subscribed, unsubscribe,
			 unsubscribed, probe, error])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"type">>, <<"presence">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_presence_attr_type(undefined, _acc) -> _acc;
encode_presence_attr_type(_val, _acc) ->
    [{<<"type">>, enc_enum(_val)} | _acc].

decode_presence_attr_from(__TopXMLNS, undefined) ->
    undefined;
decode_presence_attr_from(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"from">>, <<"presence">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_presence_attr_from(undefined, _acc) -> _acc;
encode_presence_attr_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_presence_attr_to(__TopXMLNS, undefined) ->
    undefined;
decode_presence_attr_to(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"to">>, <<"presence">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_presence_attr_to(undefined, _acc) -> _acc;
encode_presence_attr_to(_val, _acc) ->
    [{<<"to">>, enc_jid(_val)} | _acc].

'decode_presence_attr_xml:lang'(__TopXMLNS,
				undefined) ->
    undefined;
'decode_presence_attr_xml:lang'(__TopXMLNS, _val) ->
    _val.

'encode_presence_attr_xml:lang'(undefined, _acc) ->
    _acc;
'encode_presence_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_presence_priority(__TopXMLNS, __IgnoreEls,
			 {xmlel, <<"priority">>, _attrs, _els}) ->
    Cdata = decode_presence_priority_els(__TopXMLNS,
					 __IgnoreEls, _els, <<>>),
    Cdata.

decode_presence_priority_els(__TopXMLNS, __IgnoreEls,
			     [], Cdata) ->
    decode_presence_priority_cdata(__TopXMLNS, Cdata);
decode_presence_priority_els(__TopXMLNS, __IgnoreEls,
			     [{xmlcdata, _data} | _els], Cdata) ->
    decode_presence_priority_els(__TopXMLNS, __IgnoreEls,
				 _els, <<Cdata/binary, _data/binary>>);
decode_presence_priority_els(__TopXMLNS, __IgnoreEls,
			     [_ | _els], Cdata) ->
    decode_presence_priority_els(__TopXMLNS, __IgnoreEls,
				 _els, Cdata).

encode_presence_priority(Cdata, _xmlns_attrs) ->
    _els = encode_presence_priority_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"priority">>, _attrs, _els}.

decode_presence_priority_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_presence_priority_cdata(__TopXMLNS, _val) ->
    case catch dec_int(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"priority">>, __TopXMLNS}});
      _res -> _res
    end.

encode_presence_priority_cdata(undefined, _acc) -> _acc;
encode_presence_priority_cdata(_val, _acc) ->
    [{xmlcdata, enc_int(_val)} | _acc].

decode_presence_status(__TopXMLNS, __IgnoreEls,
		       {xmlel, <<"status">>, _attrs, _els}) ->
    Data = decode_presence_status_els(__TopXMLNS,
				      __IgnoreEls, _els, <<>>),
    Lang = decode_presence_status_attrs(__TopXMLNS, _attrs,
					undefined),
    {text, Lang, Data}.

decode_presence_status_els(__TopXMLNS, __IgnoreEls, [],
			   Data) ->
    decode_presence_status_cdata(__TopXMLNS, Data);
decode_presence_status_els(__TopXMLNS, __IgnoreEls,
			   [{xmlcdata, _data} | _els], Data) ->
    decode_presence_status_els(__TopXMLNS, __IgnoreEls,
			       _els, <<Data/binary, _data/binary>>);
decode_presence_status_els(__TopXMLNS, __IgnoreEls,
			   [_ | _els], Data) ->
    decode_presence_status_els(__TopXMLNS, __IgnoreEls,
			       _els, Data).

decode_presence_status_attrs(__TopXMLNS,
			     [{<<"xml:lang">>, _val} | _attrs], _Lang) ->
    decode_presence_status_attrs(__TopXMLNS, _attrs, _val);
decode_presence_status_attrs(__TopXMLNS, [_ | _attrs],
			     Lang) ->
    decode_presence_status_attrs(__TopXMLNS, _attrs, Lang);
decode_presence_status_attrs(__TopXMLNS, [], Lang) ->
    'decode_presence_status_attr_xml:lang'(__TopXMLNS,
					   Lang).

encode_presence_status({text, Lang, Data},
		       _xmlns_attrs) ->
    _els = encode_presence_status_cdata(Data, []),
    _attrs = 'encode_presence_status_attr_xml:lang'(Lang,
						    _xmlns_attrs),
    {xmlel, <<"status">>, _attrs, _els}.

'decode_presence_status_attr_xml:lang'(__TopXMLNS,
				       undefined) ->
    undefined;
'decode_presence_status_attr_xml:lang'(__TopXMLNS,
				       _val) ->
    _val.

'encode_presence_status_attr_xml:lang'(undefined,
				       _acc) ->
    _acc;
'encode_presence_status_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_presence_status_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_presence_status_cdata(__TopXMLNS, _val) -> _val.

encode_presence_status_cdata(undefined, _acc) -> _acc;
encode_presence_status_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_presence_show(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"show">>, _attrs, _els}) ->
    Cdata = decode_presence_show_els(__TopXMLNS,
				     __IgnoreEls, _els, <<>>),
    Cdata.

decode_presence_show_els(__TopXMLNS, __IgnoreEls, [],
			 Cdata) ->
    decode_presence_show_cdata(__TopXMLNS, Cdata);
decode_presence_show_els(__TopXMLNS, __IgnoreEls,
			 [{xmlcdata, _data} | _els], Cdata) ->
    decode_presence_show_els(__TopXMLNS, __IgnoreEls, _els,
			     <<Cdata/binary, _data/binary>>);
decode_presence_show_els(__TopXMLNS, __IgnoreEls,
			 [_ | _els], Cdata) ->
    decode_presence_show_els(__TopXMLNS, __IgnoreEls, _els,
			     Cdata).

encode_presence_show(Cdata, _xmlns_attrs) ->
    _els = encode_presence_show_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"show">>, _attrs, _els}.

decode_presence_show_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_presence_show_cdata(__TopXMLNS, _val) ->
    case catch dec_enum(_val, [away, chat, dnd, xa]) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_cdata_value, <<>>, <<"show">>, __TopXMLNS}});
      _res -> _res
    end.

encode_presence_show_cdata(undefined, _acc) -> _acc;
encode_presence_show_cdata(_val, _acc) ->
    [{xmlcdata, enc_enum(_val)} | _acc].

decode_message(__TopXMLNS, __IgnoreEls,
	       {xmlel, <<"message">>, _attrs, _els}) ->
    {Error, Thread, Subject, Body, __Els} =
	decode_message_els(__TopXMLNS, __IgnoreEls, _els,
			   undefined, undefined, [], [], []),
    {Id, Type, From, To, Lang} =
	decode_message_attrs(__TopXMLNS, _attrs, undefined,
			     undefined, undefined, undefined, undefined),
    {message, Id, Type, Lang, From, To, Subject, Body,
     Thread, Error, __Els}.

decode_message_els(__TopXMLNS, __IgnoreEls, [], Error,
		   Thread, Subject, Body, __Els) ->
    {Error, Thread, lists:reverse(Subject),
     lists:reverse(Body), lists:reverse(__Els)};
decode_message_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"error">>, _attrs, _} = _el | _els], Error,
		   Thread, Subject, Body, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_message_els(__TopXMLNS, __IgnoreEls, _els,
			      decode_error(__TopXMLNS, __IgnoreEls, _el),
			      Thread, Subject, Body, __Els);
       true ->
	   decode_message_els(__TopXMLNS, __IgnoreEls, _els, Error,
			      Thread, Subject, Body, __Els)
    end;
decode_message_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"subject">>, _attrs, _} = _el | _els], Error,
		   Thread, Subject, Body, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_message_els(__TopXMLNS, __IgnoreEls, _els, Error,
			      Thread,
			      [decode_message_subject(__TopXMLNS, __IgnoreEls,
						      _el)
			       | Subject],
			      Body, __Els);
       true ->
	   decode_message_els(__TopXMLNS, __IgnoreEls, _els, Error,
			      Thread, Subject, Body, __Els)
    end;
decode_message_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"thread">>, _attrs, _} = _el | _els], Error,
		   Thread, Subject, Body, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_message_els(__TopXMLNS, __IgnoreEls, _els, Error,
			      decode_message_thread(__TopXMLNS, __IgnoreEls,
						    _el),
			      Subject, Body, __Els);
       true ->
	   decode_message_els(__TopXMLNS, __IgnoreEls, _els, Error,
			      Thread, Subject, Body, __Els)
    end;
decode_message_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"body">>, _attrs, _} = _el | _els], Error,
		   Thread, Subject, Body, __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_message_els(__TopXMLNS, __IgnoreEls, _els, Error,
			      Thread, Subject,
			      [decode_message_body(__TopXMLNS, __IgnoreEls, _el)
			       | Body],
			      __Els);
       true ->
	   decode_message_els(__TopXMLNS, __IgnoreEls, _els, Error,
			      Thread, Subject, Body, __Els)
    end;
decode_message_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, _, _, _} = _el | _els], Error, Thread, Subject,
		   Body, __Els) ->
    if __IgnoreEls ->
	   decode_message_els(__TopXMLNS, __IgnoreEls, _els, Error,
			      Thread, Subject, Body, [_el | __Els]);
       true ->
	   case is_known_tag(_el) of
	     true ->
		 decode_message_els(__TopXMLNS, __IgnoreEls, _els, Error,
				    Thread, Subject, Body,
				    [decode(_el) | __Els]);
	     false ->
		 decode_message_els(__TopXMLNS, __IgnoreEls, _els, Error,
				    Thread, Subject, Body, __Els)
	   end
    end;
decode_message_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		   Error, Thread, Subject, Body, __Els) ->
    decode_message_els(__TopXMLNS, __IgnoreEls, _els, Error,
		       Thread, Subject, Body, __Els).

decode_message_attrs(__TopXMLNS,
		     [{<<"id">>, _val} | _attrs], _Id, Type, From, To,
		     Lang) ->
    decode_message_attrs(__TopXMLNS, _attrs, _val, Type,
			 From, To, Lang);
decode_message_attrs(__TopXMLNS,
		     [{<<"type">>, _val} | _attrs], Id, _Type, From, To,
		     Lang) ->
    decode_message_attrs(__TopXMLNS, _attrs, Id, _val, From,
			 To, Lang);
decode_message_attrs(__TopXMLNS,
		     [{<<"from">>, _val} | _attrs], Id, Type, _From, To,
		     Lang) ->
    decode_message_attrs(__TopXMLNS, _attrs, Id, Type, _val,
			 To, Lang);
decode_message_attrs(__TopXMLNS,
		     [{<<"to">>, _val} | _attrs], Id, Type, From, _To,
		     Lang) ->
    decode_message_attrs(__TopXMLNS, _attrs, Id, Type, From,
			 _val, Lang);
decode_message_attrs(__TopXMLNS,
		     [{<<"xml:lang">>, _val} | _attrs], Id, Type, From, To,
		     _Lang) ->
    decode_message_attrs(__TopXMLNS, _attrs, Id, Type, From,
			 To, _val);
decode_message_attrs(__TopXMLNS, [_ | _attrs], Id, Type,
		     From, To, Lang) ->
    decode_message_attrs(__TopXMLNS, _attrs, Id, Type, From,
			 To, Lang);
decode_message_attrs(__TopXMLNS, [], Id, Type, From, To,
		     Lang) ->
    {decode_message_attr_id(__TopXMLNS, Id),
     decode_message_attr_type(__TopXMLNS, Type),
     decode_message_attr_from(__TopXMLNS, From),
     decode_message_attr_to(__TopXMLNS, To),
     'decode_message_attr_xml:lang'(__TopXMLNS, Lang)}.

encode_message({message, Id, Type, Lang, From, To,
		Subject, Body, Thread, Error, __Els},
	       _xmlns_attrs) ->
    _els = [encode(_el) || _el <- __Els] ++
	     lists:reverse('encode_message_$error'(Error,
						   'encode_message_$thread'(Thread,
									    'encode_message_$subject'(Subject,
												      'encode_message_$body'(Body,
															     []))))),
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

decode_message_attr_id(__TopXMLNS, undefined) ->
    undefined;
decode_message_attr_id(__TopXMLNS, _val) -> _val.

encode_message_attr_id(undefined, _acc) -> _acc;
encode_message_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_message_attr_type(__TopXMLNS, undefined) ->
    normal;
decode_message_attr_type(__TopXMLNS, _val) ->
    case catch dec_enum(_val,
			[chat, normal, groupchat, headline, error])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"type">>, <<"message">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_message_attr_type(normal, _acc) -> _acc;
encode_message_attr_type(_val, _acc) ->
    [{<<"type">>, enc_enum(_val)} | _acc].

decode_message_attr_from(__TopXMLNS, undefined) ->
    undefined;
decode_message_attr_from(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"from">>, <<"message">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_message_attr_from(undefined, _acc) -> _acc;
encode_message_attr_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_message_attr_to(__TopXMLNS, undefined) ->
    undefined;
decode_message_attr_to(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"to">>, <<"message">>, __TopXMLNS}});
      _res -> _res
    end.

encode_message_attr_to(undefined, _acc) -> _acc;
encode_message_attr_to(_val, _acc) ->
    [{<<"to">>, enc_jid(_val)} | _acc].

'decode_message_attr_xml:lang'(__TopXMLNS, undefined) ->
    undefined;
'decode_message_attr_xml:lang'(__TopXMLNS, _val) ->
    _val.

'encode_message_attr_xml:lang'(undefined, _acc) -> _acc;
'encode_message_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_message_thread(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"thread">>, _attrs, _els}) ->
    Cdata = decode_message_thread_els(__TopXMLNS,
				      __IgnoreEls, _els, <<>>),
    Cdata.

decode_message_thread_els(__TopXMLNS, __IgnoreEls, [],
			  Cdata) ->
    decode_message_thread_cdata(__TopXMLNS, Cdata);
decode_message_thread_els(__TopXMLNS, __IgnoreEls,
			  [{xmlcdata, _data} | _els], Cdata) ->
    decode_message_thread_els(__TopXMLNS, __IgnoreEls, _els,
			      <<Cdata/binary, _data/binary>>);
decode_message_thread_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Cdata) ->
    decode_message_thread_els(__TopXMLNS, __IgnoreEls, _els,
			      Cdata).

encode_message_thread(Cdata, _xmlns_attrs) ->
    _els = encode_message_thread_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"thread">>, _attrs, _els}.

decode_message_thread_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_message_thread_cdata(__TopXMLNS, _val) -> _val.

encode_message_thread_cdata(undefined, _acc) -> _acc;
encode_message_thread_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_message_body(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"body">>, _attrs, _els}) ->
    Data = decode_message_body_els(__TopXMLNS, __IgnoreEls,
				   _els, <<>>),
    Lang = decode_message_body_attrs(__TopXMLNS, _attrs,
				     undefined),
    {text, Lang, Data}.

decode_message_body_els(__TopXMLNS, __IgnoreEls, [],
			Data) ->
    decode_message_body_cdata(__TopXMLNS, Data);
decode_message_body_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Data) ->
    decode_message_body_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Data/binary, _data/binary>>);
decode_message_body_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Data) ->
    decode_message_body_els(__TopXMLNS, __IgnoreEls, _els,
			    Data).

decode_message_body_attrs(__TopXMLNS,
			  [{<<"xml:lang">>, _val} | _attrs], _Lang) ->
    decode_message_body_attrs(__TopXMLNS, _attrs, _val);
decode_message_body_attrs(__TopXMLNS, [_ | _attrs],
			  Lang) ->
    decode_message_body_attrs(__TopXMLNS, _attrs, Lang);
decode_message_body_attrs(__TopXMLNS, [], Lang) ->
    'decode_message_body_attr_xml:lang'(__TopXMLNS, Lang).

encode_message_body({text, Lang, Data}, _xmlns_attrs) ->
    _els = encode_message_body_cdata(Data, []),
    _attrs = 'encode_message_body_attr_xml:lang'(Lang,
						 _xmlns_attrs),
    {xmlel, <<"body">>, _attrs, _els}.

'decode_message_body_attr_xml:lang'(__TopXMLNS,
				    undefined) ->
    undefined;
'decode_message_body_attr_xml:lang'(__TopXMLNS, _val) ->
    _val.

'encode_message_body_attr_xml:lang'(undefined, _acc) ->
    _acc;
'encode_message_body_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_message_body_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_message_body_cdata(__TopXMLNS, _val) -> _val.

encode_message_body_cdata(undefined, _acc) -> _acc;
encode_message_body_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_message_subject(__TopXMLNS, __IgnoreEls,
		       {xmlel, <<"subject">>, _attrs, _els}) ->
    Data = decode_message_subject_els(__TopXMLNS,
				      __IgnoreEls, _els, <<>>),
    Lang = decode_message_subject_attrs(__TopXMLNS, _attrs,
					undefined),
    {text, Lang, Data}.

decode_message_subject_els(__TopXMLNS, __IgnoreEls, [],
			   Data) ->
    decode_message_subject_cdata(__TopXMLNS, Data);
decode_message_subject_els(__TopXMLNS, __IgnoreEls,
			   [{xmlcdata, _data} | _els], Data) ->
    decode_message_subject_els(__TopXMLNS, __IgnoreEls,
			       _els, <<Data/binary, _data/binary>>);
decode_message_subject_els(__TopXMLNS, __IgnoreEls,
			   [_ | _els], Data) ->
    decode_message_subject_els(__TopXMLNS, __IgnoreEls,
			       _els, Data).

decode_message_subject_attrs(__TopXMLNS,
			     [{<<"xml:lang">>, _val} | _attrs], _Lang) ->
    decode_message_subject_attrs(__TopXMLNS, _attrs, _val);
decode_message_subject_attrs(__TopXMLNS, [_ | _attrs],
			     Lang) ->
    decode_message_subject_attrs(__TopXMLNS, _attrs, Lang);
decode_message_subject_attrs(__TopXMLNS, [], Lang) ->
    'decode_message_subject_attr_xml:lang'(__TopXMLNS,
					   Lang).

encode_message_subject({text, Lang, Data},
		       _xmlns_attrs) ->
    _els = encode_message_subject_cdata(Data, []),
    _attrs = 'encode_message_subject_attr_xml:lang'(Lang,
						    _xmlns_attrs),
    {xmlel, <<"subject">>, _attrs, _els}.

'decode_message_subject_attr_xml:lang'(__TopXMLNS,
				       undefined) ->
    undefined;
'decode_message_subject_attr_xml:lang'(__TopXMLNS,
				       _val) ->
    _val.

'encode_message_subject_attr_xml:lang'(undefined,
				       _acc) ->
    _acc;
'encode_message_subject_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_message_subject_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_message_subject_cdata(__TopXMLNS, _val) -> _val.

encode_message_subject_cdata(undefined, _acc) -> _acc;
encode_message_subject_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_iq(__TopXMLNS, __IgnoreEls,
	  {xmlel, <<"iq">>, _attrs, _els}) ->
    {Error, __Els} = decode_iq_els(__TopXMLNS, __IgnoreEls,
				   _els, undefined, []),
    {Id, Type, From, To, Lang} = decode_iq_attrs(__TopXMLNS,
						 _attrs, undefined, undefined,
						 undefined, undefined,
						 undefined),
    {iq, Id, Type, Lang, From, To, Error, __Els}.

decode_iq_els(__TopXMLNS, __IgnoreEls, [], Error,
	      __Els) ->
    {Error, lists:reverse(__Els)};
decode_iq_els(__TopXMLNS, __IgnoreEls,
	      [{xmlel, <<"error">>, _attrs, _} = _el | _els], Error,
	      __Els) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_iq_els(__TopXMLNS, __IgnoreEls, _els,
			 decode_error(__TopXMLNS, __IgnoreEls, _el), __Els);
       true ->
	   decode_iq_els(__TopXMLNS, __IgnoreEls, _els, Error,
			 __Els)
    end;
decode_iq_els(__TopXMLNS, __IgnoreEls,
	      [{xmlel, _, _, _} = _el | _els], Error, __Els) ->
    if __IgnoreEls ->
	   decode_iq_els(__TopXMLNS, __IgnoreEls, _els, Error,
			 [_el | __Els]);
       true ->
	   case is_known_tag(_el) of
	     true ->
		 decode_iq_els(__TopXMLNS, __IgnoreEls, _els, Error,
			       [decode(_el) | __Els]);
	     false ->
		 decode_iq_els(__TopXMLNS, __IgnoreEls, _els, Error,
			       __Els)
	   end
    end;
decode_iq_els(__TopXMLNS, __IgnoreEls, [_ | _els],
	      Error, __Els) ->
    decode_iq_els(__TopXMLNS, __IgnoreEls, _els, Error,
		  __Els).

decode_iq_attrs(__TopXMLNS, [{<<"id">>, _val} | _attrs],
		_Id, Type, From, To, Lang) ->
    decode_iq_attrs(__TopXMLNS, _attrs, _val, Type, From,
		    To, Lang);
decode_iq_attrs(__TopXMLNS,
		[{<<"type">>, _val} | _attrs], Id, _Type, From, To,
		Lang) ->
    decode_iq_attrs(__TopXMLNS, _attrs, Id, _val, From, To,
		    Lang);
decode_iq_attrs(__TopXMLNS,
		[{<<"from">>, _val} | _attrs], Id, Type, _From, To,
		Lang) ->
    decode_iq_attrs(__TopXMLNS, _attrs, Id, Type, _val, To,
		    Lang);
decode_iq_attrs(__TopXMLNS, [{<<"to">>, _val} | _attrs],
		Id, Type, From, _To, Lang) ->
    decode_iq_attrs(__TopXMLNS, _attrs, Id, Type, From,
		    _val, Lang);
decode_iq_attrs(__TopXMLNS,
		[{<<"xml:lang">>, _val} | _attrs], Id, Type, From, To,
		_Lang) ->
    decode_iq_attrs(__TopXMLNS, _attrs, Id, Type, From, To,
		    _val);
decode_iq_attrs(__TopXMLNS, [_ | _attrs], Id, Type,
		From, To, Lang) ->
    decode_iq_attrs(__TopXMLNS, _attrs, Id, Type, From, To,
		    Lang);
decode_iq_attrs(__TopXMLNS, [], Id, Type, From, To,
		Lang) ->
    {decode_iq_attr_id(__TopXMLNS, Id),
     decode_iq_attr_type(__TopXMLNS, Type),
     decode_iq_attr_from(__TopXMLNS, From),
     decode_iq_attr_to(__TopXMLNS, To),
     'decode_iq_attr_xml:lang'(__TopXMLNS, Lang)}.

encode_iq({iq, Id, Type, Lang, From, To, Error, __Els},
	  _xmlns_attrs) ->
    _els = [encode(_el) || _el <- __Els] ++
	     lists:reverse('encode_iq_$error'(Error, [])),
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

decode_iq_attr_id(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"id">>, <<"iq">>, __TopXMLNS}});
decode_iq_attr_id(__TopXMLNS, _val) -> _val.

encode_iq_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_iq_attr_type(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"type">>, <<"iq">>, __TopXMLNS}});
decode_iq_attr_type(__TopXMLNS, _val) ->
    case catch dec_enum(_val, [get, set, result, error]) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"type">>, <<"iq">>, __TopXMLNS}});
      _res -> _res
    end.

encode_iq_attr_type(_val, _acc) ->
    [{<<"type">>, enc_enum(_val)} | _acc].

decode_iq_attr_from(__TopXMLNS, undefined) -> undefined;
decode_iq_attr_from(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"from">>, <<"iq">>, __TopXMLNS}});
      _res -> _res
    end.

encode_iq_attr_from(undefined, _acc) -> _acc;
encode_iq_attr_from(_val, _acc) ->
    [{<<"from">>, enc_jid(_val)} | _acc].

decode_iq_attr_to(__TopXMLNS, undefined) -> undefined;
decode_iq_attr_to(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"to">>, <<"iq">>, __TopXMLNS}});
      _res -> _res
    end.

encode_iq_attr_to(undefined, _acc) -> _acc;
encode_iq_attr_to(_val, _acc) ->
    [{<<"to">>, enc_jid(_val)} | _acc].

'decode_iq_attr_xml:lang'(__TopXMLNS, undefined) ->
    undefined;
'decode_iq_attr_xml:lang'(__TopXMLNS, _val) -> _val.

'encode_iq_attr_xml:lang'(undefined, _acc) -> _acc;
'encode_iq_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_stats(__TopXMLNS, __IgnoreEls,
	     {xmlel, <<"query">>, _attrs, _els}) ->
    Stat = decode_stats_els(__TopXMLNS, __IgnoreEls, _els,
			    []),
    {stats, Stat}.

decode_stats_els(__TopXMLNS, __IgnoreEls, [], Stat) ->
    lists:reverse(Stat);
decode_stats_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"stat">>, _attrs, _} = _el | _els], Stat) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_stats_els(__TopXMLNS, __IgnoreEls, _els,
			    [decode_stat(__TopXMLNS, __IgnoreEls, _el) | Stat]);
       true ->
	   decode_stats_els(__TopXMLNS, __IgnoreEls, _els, Stat)
    end;
decode_stats_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		 Stat) ->
    decode_stats_els(__TopXMLNS, __IgnoreEls, _els, Stat).

encode_stats({stats, Stat}, _xmlns_attrs) ->
    _els = lists:reverse('encode_stats_$stat'(Stat, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"query">>, _attrs, _els}.

'encode_stats_$stat'([], _acc) -> _acc;
'encode_stats_$stat'([Stat | _els], _acc) ->
    'encode_stats_$stat'(_els,
			 [encode_stat(Stat, []) | _acc]).

decode_stat(__TopXMLNS, __IgnoreEls,
	    {xmlel, <<"stat">>, _attrs, _els}) ->
    Error = decode_stat_els(__TopXMLNS, __IgnoreEls, _els,
			    []),
    {Name, Units, Value} = decode_stat_attrs(__TopXMLNS,
					     _attrs, undefined, undefined,
					     undefined),
    {stat, Name, Units, Value, Error}.

decode_stat_els(__TopXMLNS, __IgnoreEls, [], Error) ->
    lists:reverse(Error);
decode_stat_els(__TopXMLNS, __IgnoreEls,
		[{xmlel, <<"error">>, _attrs, _} = _el | _els],
		Error) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_stat_els(__TopXMLNS, __IgnoreEls, _els,
			   [decode_stat_error(__TopXMLNS, __IgnoreEls, _el)
			    | Error]);
       true ->
	   decode_stat_els(__TopXMLNS, __IgnoreEls, _els, Error)
    end;
decode_stat_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		Error) ->
    decode_stat_els(__TopXMLNS, __IgnoreEls, _els, Error).

decode_stat_attrs(__TopXMLNS,
		  [{<<"name">>, _val} | _attrs], _Name, Units, Value) ->
    decode_stat_attrs(__TopXMLNS, _attrs, _val, Units,
		      Value);
decode_stat_attrs(__TopXMLNS,
		  [{<<"units">>, _val} | _attrs], Name, _Units, Value) ->
    decode_stat_attrs(__TopXMLNS, _attrs, Name, _val,
		      Value);
decode_stat_attrs(__TopXMLNS,
		  [{<<"value">>, _val} | _attrs], Name, Units, _Value) ->
    decode_stat_attrs(__TopXMLNS, _attrs, Name, Units,
		      _val);
decode_stat_attrs(__TopXMLNS, [_ | _attrs], Name, Units,
		  Value) ->
    decode_stat_attrs(__TopXMLNS, _attrs, Name, Units,
		      Value);
decode_stat_attrs(__TopXMLNS, [], Name, Units, Value) ->
    {decode_stat_attr_name(__TopXMLNS, Name),
     decode_stat_attr_units(__TopXMLNS, Units),
     decode_stat_attr_value(__TopXMLNS, Value)}.

encode_stat({stat, Name, Units, Value, Error},
	    _xmlns_attrs) ->
    _els = lists:reverse('encode_stat_$error'(Error, [])),
    _attrs = encode_stat_attr_value(Value,
				    encode_stat_attr_units(Units,
							   encode_stat_attr_name(Name,
										 _xmlns_attrs))),
    {xmlel, <<"stat">>, _attrs, _els}.

'encode_stat_$error'([], _acc) -> _acc;
'encode_stat_$error'([Error | _els], _acc) ->
    'encode_stat_$error'(_els,
			 [encode_stat_error(Error, []) | _acc]).

decode_stat_attr_name(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"name">>, <<"stat">>, __TopXMLNS}});
decode_stat_attr_name(__TopXMLNS, _val) -> _val.

encode_stat_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_stat_attr_units(__TopXMLNS, undefined) ->
    undefined;
decode_stat_attr_units(__TopXMLNS, _val) -> _val.

encode_stat_attr_units(undefined, _acc) -> _acc;
encode_stat_attr_units(_val, _acc) ->
    [{<<"units">>, _val} | _acc].

decode_stat_attr_value(__TopXMLNS, undefined) ->
    undefined;
decode_stat_attr_value(__TopXMLNS, _val) -> _val.

encode_stat_attr_value(undefined, _acc) -> _acc;
encode_stat_attr_value(_val, _acc) ->
    [{<<"value">>, _val} | _acc].

decode_stat_error(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"error">>, _attrs, _els}) ->
    Cdata = decode_stat_error_els(__TopXMLNS, __IgnoreEls,
				  _els, <<>>),
    Code = decode_stat_error_attrs(__TopXMLNS, _attrs,
				   undefined),
    {Code, Cdata}.

decode_stat_error_els(__TopXMLNS, __IgnoreEls, [],
		      Cdata) ->
    decode_stat_error_cdata(__TopXMLNS, Cdata);
decode_stat_error_els(__TopXMLNS, __IgnoreEls,
		      [{xmlcdata, _data} | _els], Cdata) ->
    decode_stat_error_els(__TopXMLNS, __IgnoreEls, _els,
			  <<Cdata/binary, _data/binary>>);
decode_stat_error_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Cdata) ->
    decode_stat_error_els(__TopXMLNS, __IgnoreEls, _els,
			  Cdata).

decode_stat_error_attrs(__TopXMLNS,
			[{<<"code">>, _val} | _attrs], _Code) ->
    decode_stat_error_attrs(__TopXMLNS, _attrs, _val);
decode_stat_error_attrs(__TopXMLNS, [_ | _attrs],
			Code) ->
    decode_stat_error_attrs(__TopXMLNS, _attrs, Code);
decode_stat_error_attrs(__TopXMLNS, [], Code) ->
    decode_stat_error_attr_code(__TopXMLNS, Code).

encode_stat_error({Code, Cdata}, _xmlns_attrs) ->
    _els = encode_stat_error_cdata(Cdata, []),
    _attrs = encode_stat_error_attr_code(Code,
					 _xmlns_attrs),
    {xmlel, <<"error">>, _attrs, _els}.

decode_stat_error_attr_code(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"code">>, <<"error">>, __TopXMLNS}});
decode_stat_error_attr_code(__TopXMLNS, _val) ->
    case catch dec_int(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"code">>, <<"error">>, __TopXMLNS}});
      _res -> _res
    end.

encode_stat_error_attr_code(_val, _acc) ->
    [{<<"code">>, enc_int(_val)} | _acc].

decode_stat_error_cdata(__TopXMLNS, <<>>) -> undefined;
decode_stat_error_cdata(__TopXMLNS, _val) -> _val.

encode_stat_error_cdata(undefined, _acc) -> _acc;
encode_stat_error_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_bookmarks_storage(__TopXMLNS, __IgnoreEls,
			 {xmlel, <<"storage">>, _attrs, _els}) ->
    {Conference, Url} =
	decode_bookmarks_storage_els(__TopXMLNS, __IgnoreEls,
				     _els, [], []),
    {bookmark_storage, Conference, Url}.

decode_bookmarks_storage_els(__TopXMLNS, __IgnoreEls,
			     [], Conference, Url) ->
    {lists:reverse(Conference), lists:reverse(Url)};
decode_bookmarks_storage_els(__TopXMLNS, __IgnoreEls,
			     [{xmlel, <<"conference">>, _attrs, _} = _el
			      | _els],
			     Conference, Url) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_bookmarks_storage_els(__TopXMLNS, __IgnoreEls,
					_els,
					[decode_bookmark_conference(__TopXMLNS,
								    __IgnoreEls,
								    _el)
					 | Conference],
					Url);
       true ->
	   decode_bookmarks_storage_els(__TopXMLNS, __IgnoreEls,
					_els, Conference, Url)
    end;
decode_bookmarks_storage_els(__TopXMLNS, __IgnoreEls,
			     [{xmlel, <<"url">>, _attrs, _} = _el | _els],
			     Conference, Url) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_bookmarks_storage_els(__TopXMLNS, __IgnoreEls,
					_els, Conference,
					[decode_bookmark_url(__TopXMLNS,
							     __IgnoreEls, _el)
					 | Url]);
       true ->
	   decode_bookmarks_storage_els(__TopXMLNS, __IgnoreEls,
					_els, Conference, Url)
    end;
decode_bookmarks_storage_els(__TopXMLNS, __IgnoreEls,
			     [_ | _els], Conference, Url) ->
    decode_bookmarks_storage_els(__TopXMLNS, __IgnoreEls,
				 _els, Conference, Url).

encode_bookmarks_storage({bookmark_storage, Conference,
			  Url},
			 _xmlns_attrs) ->
    _els =
	lists:reverse('encode_bookmarks_storage_$conference'(Conference,
							     'encode_bookmarks_storage_$url'(Url,
											     []))),
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

decode_bookmark_url(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"url">>, _attrs, _els}) ->
    {Name, Url} = decode_bookmark_url_attrs(__TopXMLNS,
					    _attrs, undefined, undefined),
    {bookmark_url, Name, Url}.

decode_bookmark_url_attrs(__TopXMLNS,
			  [{<<"name">>, _val} | _attrs], _Name, Url) ->
    decode_bookmark_url_attrs(__TopXMLNS, _attrs, _val,
			      Url);
decode_bookmark_url_attrs(__TopXMLNS,
			  [{<<"url">>, _val} | _attrs], Name, _Url) ->
    decode_bookmark_url_attrs(__TopXMLNS, _attrs, Name,
			      _val);
decode_bookmark_url_attrs(__TopXMLNS, [_ | _attrs],
			  Name, Url) ->
    decode_bookmark_url_attrs(__TopXMLNS, _attrs, Name,
			      Url);
decode_bookmark_url_attrs(__TopXMLNS, [], Name, Url) ->
    {decode_bookmark_url_attr_name(__TopXMLNS, Name),
     decode_bookmark_url_attr_url(__TopXMLNS, Url)}.

encode_bookmark_url({bookmark_url, Name, Url},
		    _xmlns_attrs) ->
    _els = [],
    _attrs = encode_bookmark_url_attr_url(Url,
					  encode_bookmark_url_attr_name(Name,
									_xmlns_attrs)),
    {xmlel, <<"url">>, _attrs, _els}.

decode_bookmark_url_attr_name(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"name">>, <<"url">>, __TopXMLNS}});
decode_bookmark_url_attr_name(__TopXMLNS, _val) -> _val.

encode_bookmark_url_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_bookmark_url_attr_url(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"url">>, <<"url">>, __TopXMLNS}});
decode_bookmark_url_attr_url(__TopXMLNS, _val) -> _val.

encode_bookmark_url_attr_url(_val, _acc) ->
    [{<<"url">>, _val} | _acc].

decode_bookmark_conference(__TopXMLNS, __IgnoreEls,
			   {xmlel, <<"conference">>, _attrs, _els}) ->
    {Password, Nick} =
	decode_bookmark_conference_els(__TopXMLNS, __IgnoreEls,
				       _els, undefined, undefined),
    {Name, Jid, Autojoin} =
	decode_bookmark_conference_attrs(__TopXMLNS, _attrs,
					 undefined, undefined, undefined),
    {bookmark_conference, Name, Jid, Autojoin, Nick,
     Password}.

decode_bookmark_conference_els(__TopXMLNS, __IgnoreEls,
			       [], Password, Nick) ->
    {Password, Nick};
decode_bookmark_conference_els(__TopXMLNS, __IgnoreEls,
			       [{xmlel, <<"nick">>, _attrs, _} = _el | _els],
			       Password, Nick) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_bookmark_conference_els(__TopXMLNS, __IgnoreEls,
					  _els, Password,
					  decode_conference_nick(__TopXMLNS,
								 __IgnoreEls,
								 _el));
       true ->
	   decode_bookmark_conference_els(__TopXMLNS, __IgnoreEls,
					  _els, Password, Nick)
    end;
decode_bookmark_conference_els(__TopXMLNS, __IgnoreEls,
			       [{xmlel, <<"password">>, _attrs, _} = _el
				| _els],
			       Password, Nick) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_bookmark_conference_els(__TopXMLNS, __IgnoreEls,
					  _els,
					  decode_conference_password(__TopXMLNS,
								     __IgnoreEls,
								     _el),
					  Nick);
       true ->
	   decode_bookmark_conference_els(__TopXMLNS, __IgnoreEls,
					  _els, Password, Nick)
    end;
decode_bookmark_conference_els(__TopXMLNS, __IgnoreEls,
			       [_ | _els], Password, Nick) ->
    decode_bookmark_conference_els(__TopXMLNS, __IgnoreEls,
				   _els, Password, Nick).

decode_bookmark_conference_attrs(__TopXMLNS,
				 [{<<"name">>, _val} | _attrs], _Name, Jid,
				 Autojoin) ->
    decode_bookmark_conference_attrs(__TopXMLNS, _attrs,
				     _val, Jid, Autojoin);
decode_bookmark_conference_attrs(__TopXMLNS,
				 [{<<"jid">>, _val} | _attrs], Name, _Jid,
				 Autojoin) ->
    decode_bookmark_conference_attrs(__TopXMLNS, _attrs,
				     Name, _val, Autojoin);
decode_bookmark_conference_attrs(__TopXMLNS,
				 [{<<"autojoin">>, _val} | _attrs], Name, Jid,
				 _Autojoin) ->
    decode_bookmark_conference_attrs(__TopXMLNS, _attrs,
				     Name, Jid, _val);
decode_bookmark_conference_attrs(__TopXMLNS,
				 [_ | _attrs], Name, Jid, Autojoin) ->
    decode_bookmark_conference_attrs(__TopXMLNS, _attrs,
				     Name, Jid, Autojoin);
decode_bookmark_conference_attrs(__TopXMLNS, [], Name,
				 Jid, Autojoin) ->
    {decode_bookmark_conference_attr_name(__TopXMLNS, Name),
     decode_bookmark_conference_attr_jid(__TopXMLNS, Jid),
     decode_bookmark_conference_attr_autojoin(__TopXMLNS,
					      Autojoin)}.

encode_bookmark_conference({bookmark_conference, Name,
			    Jid, Autojoin, Nick, Password},
			   _xmlns_attrs) ->
    _els =
	lists:reverse('encode_bookmark_conference_$password'(Password,
							     'encode_bookmark_conference_$nick'(Nick,
												[]))),
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

decode_bookmark_conference_attr_name(__TopXMLNS,
				     undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"name">>, <<"conference">>,
		   __TopXMLNS}});
decode_bookmark_conference_attr_name(__TopXMLNS,
				     _val) ->
    _val.

encode_bookmark_conference_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_bookmark_conference_attr_jid(__TopXMLNS,
				    undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"conference">>,
		   __TopXMLNS}});
decode_bookmark_conference_attr_jid(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"conference">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_bookmark_conference_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_bookmark_conference_attr_autojoin(__TopXMLNS,
					 undefined) ->
    false;
decode_bookmark_conference_attr_autojoin(__TopXMLNS,
					 _val) ->
    case catch dec_bool(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"autojoin">>, <<"conference">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_bookmark_conference_attr_autojoin(false, _acc) ->
    _acc;
encode_bookmark_conference_attr_autojoin(_val, _acc) ->
    [{<<"autojoin">>, enc_bool(_val)} | _acc].

decode_conference_password(__TopXMLNS, __IgnoreEls,
			   {xmlel, <<"password">>, _attrs, _els}) ->
    Cdata = decode_conference_password_els(__TopXMLNS,
					   __IgnoreEls, _els, <<>>),
    Cdata.

decode_conference_password_els(__TopXMLNS, __IgnoreEls,
			       [], Cdata) ->
    decode_conference_password_cdata(__TopXMLNS, Cdata);
decode_conference_password_els(__TopXMLNS, __IgnoreEls,
			       [{xmlcdata, _data} | _els], Cdata) ->
    decode_conference_password_els(__TopXMLNS, __IgnoreEls,
				   _els, <<Cdata/binary, _data/binary>>);
decode_conference_password_els(__TopXMLNS, __IgnoreEls,
			       [_ | _els], Cdata) ->
    decode_conference_password_els(__TopXMLNS, __IgnoreEls,
				   _els, Cdata).

encode_conference_password(Cdata, _xmlns_attrs) ->
    _els = encode_conference_password_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"password">>, _attrs, _els}.

decode_conference_password_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_conference_password_cdata(__TopXMLNS, _val) ->
    _val.

encode_conference_password_cdata(undefined, _acc) ->
    _acc;
encode_conference_password_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_conference_nick(__TopXMLNS, __IgnoreEls,
		       {xmlel, <<"nick">>, _attrs, _els}) ->
    Cdata = decode_conference_nick_els(__TopXMLNS,
				       __IgnoreEls, _els, <<>>),
    Cdata.

decode_conference_nick_els(__TopXMLNS, __IgnoreEls, [],
			   Cdata) ->
    decode_conference_nick_cdata(__TopXMLNS, Cdata);
decode_conference_nick_els(__TopXMLNS, __IgnoreEls,
			   [{xmlcdata, _data} | _els], Cdata) ->
    decode_conference_nick_els(__TopXMLNS, __IgnoreEls,
			       _els, <<Cdata/binary, _data/binary>>);
decode_conference_nick_els(__TopXMLNS, __IgnoreEls,
			   [_ | _els], Cdata) ->
    decode_conference_nick_els(__TopXMLNS, __IgnoreEls,
			       _els, Cdata).

encode_conference_nick(Cdata, _xmlns_attrs) ->
    _els = encode_conference_nick_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"nick">>, _attrs, _els}.

decode_conference_nick_cdata(__TopXMLNS, <<>>) ->
    undefined;
decode_conference_nick_cdata(__TopXMLNS, _val) -> _val.

encode_conference_nick_cdata(undefined, _acc) -> _acc;
encode_conference_nick_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_private(__TopXMLNS, __IgnoreEls,
	       {xmlel, <<"query">>, _attrs, _els}) ->
    __Xmls = decode_private_els(__TopXMLNS, __IgnoreEls,
				_els, []),
    {private, __Xmls}.

decode_private_els(__TopXMLNS, __IgnoreEls, [],
		   __Xmls) ->
    lists:reverse(__Xmls);
decode_private_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, _, _, _} = _el | _els], __Xmls) ->
    decode_private_els(__TopXMLNS, __IgnoreEls, _els,
		       [_el | __Xmls]);
decode_private_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		   __Xmls) ->
    decode_private_els(__TopXMLNS, __IgnoreEls, _els,
		       __Xmls).

encode_private({private, __Xmls}, _xmlns_attrs) ->
    _els = __Xmls,
    _attrs = _xmlns_attrs,
    {xmlel, <<"query">>, _attrs, _els}.

decode_disco_items(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"query">>, _attrs, _els}) ->
    Items = decode_disco_items_els(__TopXMLNS, __IgnoreEls,
				   _els, []),
    Node = decode_disco_items_attrs(__TopXMLNS, _attrs,
				    undefined),
    {disco_items, Node, Items}.

decode_disco_items_els(__TopXMLNS, __IgnoreEls, [],
		       Items) ->
    lists:reverse(Items);
decode_disco_items_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"item">>, _attrs, _} = _el | _els], Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_disco_items_els(__TopXMLNS, __IgnoreEls, _els,
				  [decode_disco_item(__TopXMLNS, __IgnoreEls,
						     _el)
				   | Items]);
       true ->
	   decode_disco_items_els(__TopXMLNS, __IgnoreEls, _els,
				  Items)
    end;
decode_disco_items_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Items) ->
    decode_disco_items_els(__TopXMLNS, __IgnoreEls, _els,
			   Items).

decode_disco_items_attrs(__TopXMLNS,
			 [{<<"node">>, _val} | _attrs], _Node) ->
    decode_disco_items_attrs(__TopXMLNS, _attrs, _val);
decode_disco_items_attrs(__TopXMLNS, [_ | _attrs],
			 Node) ->
    decode_disco_items_attrs(__TopXMLNS, _attrs, Node);
decode_disco_items_attrs(__TopXMLNS, [], Node) ->
    decode_disco_items_attr_node(__TopXMLNS, Node).

encode_disco_items({disco_items, Node, Items},
		   _xmlns_attrs) ->
    _els = lists:reverse('encode_disco_items_$items'(Items,
						     [])),
    _attrs = encode_disco_items_attr_node(Node,
					  _xmlns_attrs),
    {xmlel, <<"query">>, _attrs, _els}.

'encode_disco_items_$items'([], _acc) -> _acc;
'encode_disco_items_$items'([Items | _els], _acc) ->
    'encode_disco_items_$items'(_els,
				[encode_disco_item(Items, []) | _acc]).

decode_disco_items_attr_node(__TopXMLNS, undefined) ->
    undefined;
decode_disco_items_attr_node(__TopXMLNS, _val) -> _val.

encode_disco_items_attr_node(undefined, _acc) -> _acc;
encode_disco_items_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_disco_item(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"item">>, _attrs, _els}) ->
    {Jid, Name, Node} = decode_disco_item_attrs(__TopXMLNS,
						_attrs, undefined, undefined,
						undefined),
    {disco_item, Jid, Name, Node}.

decode_disco_item_attrs(__TopXMLNS,
			[{<<"jid">>, _val} | _attrs], _Jid, Name, Node) ->
    decode_disco_item_attrs(__TopXMLNS, _attrs, _val, Name,
			    Node);
decode_disco_item_attrs(__TopXMLNS,
			[{<<"name">>, _val} | _attrs], Jid, _Name, Node) ->
    decode_disco_item_attrs(__TopXMLNS, _attrs, Jid, _val,
			    Node);
decode_disco_item_attrs(__TopXMLNS,
			[{<<"node">>, _val} | _attrs], Jid, Name, _Node) ->
    decode_disco_item_attrs(__TopXMLNS, _attrs, Jid, Name,
			    _val);
decode_disco_item_attrs(__TopXMLNS, [_ | _attrs], Jid,
			Name, Node) ->
    decode_disco_item_attrs(__TopXMLNS, _attrs, Jid, Name,
			    Node);
decode_disco_item_attrs(__TopXMLNS, [], Jid, Name,
			Node) ->
    {decode_disco_item_attr_jid(__TopXMLNS, Jid),
     decode_disco_item_attr_name(__TopXMLNS, Name),
     decode_disco_item_attr_node(__TopXMLNS, Node)}.

encode_disco_item({disco_item, Jid, Name, Node},
		  _xmlns_attrs) ->
    _els = [],
    _attrs = encode_disco_item_attr_node(Node,
					 encode_disco_item_attr_name(Name,
								     encode_disco_item_attr_jid(Jid,
												_xmlns_attrs))),
    {xmlel, <<"item">>, _attrs, _els}.

decode_disco_item_attr_jid(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"item">>, __TopXMLNS}});
decode_disco_item_attr_jid(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"item">>, __TopXMLNS}});
      _res -> _res
    end.

encode_disco_item_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_disco_item_attr_name(__TopXMLNS, undefined) ->
    undefined;
decode_disco_item_attr_name(__TopXMLNS, _val) -> _val.

encode_disco_item_attr_name(undefined, _acc) -> _acc;
encode_disco_item_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_disco_item_attr_node(__TopXMLNS, undefined) ->
    undefined;
decode_disco_item_attr_node(__TopXMLNS, _val) -> _val.

encode_disco_item_attr_node(undefined, _acc) -> _acc;
encode_disco_item_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_disco_info(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"query">>, _attrs, _els}) ->
    {Xdata, Features, Identities} =
	decode_disco_info_els(__TopXMLNS, __IgnoreEls, _els, [],
			      [], []),
    Node = decode_disco_info_attrs(__TopXMLNS, _attrs,
				   undefined),
    {disco_info, Node, Identities, Features, Xdata}.

decode_disco_info_els(__TopXMLNS, __IgnoreEls, [],
		      Xdata, Features, Identities) ->
    {lists:reverse(Xdata), lists:reverse(Features),
     lists:reverse(Identities)};
decode_disco_info_els(__TopXMLNS, __IgnoreEls,
		      [{xmlel, <<"identity">>, _attrs, _} = _el | _els],
		      Xdata, Features, Identities) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_disco_info_els(__TopXMLNS, __IgnoreEls, _els,
				 Xdata, Features,
				 [decode_disco_identity(__TopXMLNS, __IgnoreEls,
							_el)
				  | Identities]);
       true ->
	   decode_disco_info_els(__TopXMLNS, __IgnoreEls, _els,
				 Xdata, Features, Identities)
    end;
decode_disco_info_els(__TopXMLNS, __IgnoreEls,
		      [{xmlel, <<"feature">>, _attrs, _} = _el | _els], Xdata,
		      Features, Identities) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_disco_info_els(__TopXMLNS, __IgnoreEls, _els,
				 Xdata,
				 [decode_disco_feature(__TopXMLNS, __IgnoreEls,
						       _el)
				  | Features],
				 Identities);
       true ->
	   decode_disco_info_els(__TopXMLNS, __IgnoreEls, _els,
				 Xdata, Features, Identities)
    end;
decode_disco_info_els(__TopXMLNS, __IgnoreEls,
		      [{xmlel, <<"x">>, _attrs, _} = _el | _els], Xdata,
		      Features, Identities) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<"jabber:x:data">> ->
	   decode_disco_info_els(__TopXMLNS, __IgnoreEls, _els,
				 [decode_xdata(_xmlns, __IgnoreEls, _el)
				  | Xdata],
				 Features, Identities);
       true ->
	   decode_disco_info_els(__TopXMLNS, __IgnoreEls, _els,
				 Xdata, Features, Identities)
    end;
decode_disco_info_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Xdata, Features, Identities) ->
    decode_disco_info_els(__TopXMLNS, __IgnoreEls, _els,
			  Xdata, Features, Identities).

decode_disco_info_attrs(__TopXMLNS,
			[{<<"node">>, _val} | _attrs], _Node) ->
    decode_disco_info_attrs(__TopXMLNS, _attrs, _val);
decode_disco_info_attrs(__TopXMLNS, [_ | _attrs],
			Node) ->
    decode_disco_info_attrs(__TopXMLNS, _attrs, Node);
decode_disco_info_attrs(__TopXMLNS, [], Node) ->
    decode_disco_info_attr_node(__TopXMLNS, Node).

encode_disco_info({disco_info, Node, Identities,
		   Features, Xdata},
		  _xmlns_attrs) ->
    _els = lists:reverse('encode_disco_info_$xdata'(Xdata,
						    'encode_disco_info_$features'(Features,
										  'encode_disco_info_$identities'(Identities,
														  [])))),
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

decode_disco_info_attr_node(__TopXMLNS, undefined) ->
    undefined;
decode_disco_info_attr_node(__TopXMLNS, _val) -> _val.

encode_disco_info_attr_node(undefined, _acc) -> _acc;
encode_disco_info_attr_node(_val, _acc) ->
    [{<<"node">>, _val} | _acc].

decode_disco_feature(__TopXMLNS, __IgnoreEls,
		     {xmlel, <<"feature">>, _attrs, _els}) ->
    Var = decode_disco_feature_attrs(__TopXMLNS, _attrs,
				     undefined),
    Var.

decode_disco_feature_attrs(__TopXMLNS,
			   [{<<"var">>, _val} | _attrs], _Var) ->
    decode_disco_feature_attrs(__TopXMLNS, _attrs, _val);
decode_disco_feature_attrs(__TopXMLNS, [_ | _attrs],
			   Var) ->
    decode_disco_feature_attrs(__TopXMLNS, _attrs, Var);
decode_disco_feature_attrs(__TopXMLNS, [], Var) ->
    decode_disco_feature_attr_var(__TopXMLNS, Var).

encode_disco_feature(Var, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_disco_feature_attr_var(Var,
					   _xmlns_attrs),
    {xmlel, <<"feature">>, _attrs, _els}.

decode_disco_feature_attr_var(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"var">>, <<"feature">>, __TopXMLNS}});
decode_disco_feature_attr_var(__TopXMLNS, _val) -> _val.

encode_disco_feature_attr_var(_val, _acc) ->
    [{<<"var">>, _val} | _acc].

decode_disco_identity(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"identity">>, _attrs, _els}) ->
    {Category, Type, Lang, Name} =
	decode_disco_identity_attrs(__TopXMLNS, _attrs,
				    undefined, undefined, undefined, undefined),
    {identity, Category, Type, Lang, Name}.

decode_disco_identity_attrs(__TopXMLNS,
			    [{<<"category">>, _val} | _attrs], _Category, Type,
			    Lang, Name) ->
    decode_disco_identity_attrs(__TopXMLNS, _attrs, _val,
				Type, Lang, Name);
decode_disco_identity_attrs(__TopXMLNS,
			    [{<<"type">>, _val} | _attrs], Category, _Type,
			    Lang, Name) ->
    decode_disco_identity_attrs(__TopXMLNS, _attrs,
				Category, _val, Lang, Name);
decode_disco_identity_attrs(__TopXMLNS,
			    [{<<"xml:lang">>, _val} | _attrs], Category, Type,
			    _Lang, Name) ->
    decode_disco_identity_attrs(__TopXMLNS, _attrs,
				Category, Type, _val, Name);
decode_disco_identity_attrs(__TopXMLNS,
			    [{<<"name">>, _val} | _attrs], Category, Type, Lang,
			    _Name) ->
    decode_disco_identity_attrs(__TopXMLNS, _attrs,
				Category, Type, Lang, _val);
decode_disco_identity_attrs(__TopXMLNS, [_ | _attrs],
			    Category, Type, Lang, Name) ->
    decode_disco_identity_attrs(__TopXMLNS, _attrs,
				Category, Type, Lang, Name);
decode_disco_identity_attrs(__TopXMLNS, [], Category,
			    Type, Lang, Name) ->
    {decode_disco_identity_attr_category(__TopXMLNS,
					 Category),
     decode_disco_identity_attr_type(__TopXMLNS, Type),
     'decode_disco_identity_attr_xml:lang'(__TopXMLNS, Lang),
     decode_disco_identity_attr_name(__TopXMLNS, Name)}.

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

decode_disco_identity_attr_category(__TopXMLNS,
				    undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"category">>, <<"identity">>,
		   __TopXMLNS}});
decode_disco_identity_attr_category(__TopXMLNS, _val) ->
    _val.

encode_disco_identity_attr_category(_val, _acc) ->
    [{<<"category">>, _val} | _acc].

decode_disco_identity_attr_type(__TopXMLNS,
				undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"type">>, <<"identity">>,
		   __TopXMLNS}});
decode_disco_identity_attr_type(__TopXMLNS, _val) ->
    _val.

encode_disco_identity_attr_type(_val, _acc) ->
    [{<<"type">>, _val} | _acc].

'decode_disco_identity_attr_xml:lang'(__TopXMLNS,
				      undefined) ->
    undefined;
'decode_disco_identity_attr_xml:lang'(__TopXMLNS,
				      _val) ->
    _val.

'encode_disco_identity_attr_xml:lang'(undefined,
				      _acc) ->
    _acc;
'encode_disco_identity_attr_xml:lang'(_val, _acc) ->
    [{<<"xml:lang">>, _val} | _acc].

decode_disco_identity_attr_name(__TopXMLNS,
				undefined) ->
    undefined;
decode_disco_identity_attr_name(__TopXMLNS, _val) ->
    _val.

encode_disco_identity_attr_name(undefined, _acc) ->
    _acc;
encode_disco_identity_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_block_list(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"blocklist">>, _attrs, _els}) ->
    {block_list}.

encode_block_list({block_list}, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"blocklist">>, _attrs, _els}.

decode_unblock(__TopXMLNS, __IgnoreEls,
	       {xmlel, <<"unblock">>, _attrs, _els}) ->
    Items = decode_unblock_els(__TopXMLNS, __IgnoreEls,
			       _els, []),
    {unblock, Items}.

decode_unblock_els(__TopXMLNS, __IgnoreEls, [],
		   Items) ->
    lists:reverse(Items);
decode_unblock_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"item">>, _attrs, _} = _el | _els], Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_unblock_els(__TopXMLNS, __IgnoreEls, _els,
			      case decode_block_item(__TopXMLNS, __IgnoreEls,
						     _el)
				  of
				undefined -> Items;
				_new_el -> [_new_el | Items]
			      end);
       true ->
	   decode_unblock_els(__TopXMLNS, __IgnoreEls, _els, Items)
    end;
decode_unblock_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		   Items) ->
    decode_unblock_els(__TopXMLNS, __IgnoreEls, _els,
		       Items).

encode_unblock({unblock, Items}, _xmlns_attrs) ->
    _els = lists:reverse('encode_unblock_$items'(Items,
						 [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"unblock">>, _attrs, _els}.

'encode_unblock_$items'([], _acc) -> _acc;
'encode_unblock_$items'([Items | _els], _acc) ->
    'encode_unblock_$items'(_els,
			    [encode_block_item(Items, []) | _acc]).

decode_block(__TopXMLNS, __IgnoreEls,
	     {xmlel, <<"block">>, _attrs, _els}) ->
    Items = decode_block_els(__TopXMLNS, __IgnoreEls, _els,
			     []),
    {block, Items}.

decode_block_els(__TopXMLNS, __IgnoreEls, [], Items) ->
    lists:reverse(Items);
decode_block_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"item">>, _attrs, _} = _el | _els], Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_block_els(__TopXMLNS, __IgnoreEls, _els,
			    case decode_block_item(__TopXMLNS, __IgnoreEls, _el)
				of
			      undefined -> Items;
			      _new_el -> [_new_el | Items]
			    end);
       true ->
	   decode_block_els(__TopXMLNS, __IgnoreEls, _els, Items)
    end;
decode_block_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		 Items) ->
    decode_block_els(__TopXMLNS, __IgnoreEls, _els, Items).

encode_block({block, Items}, _xmlns_attrs) ->
    _els = lists:reverse('encode_block_$items'(Items, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"block">>, _attrs, _els}.

'encode_block_$items'([], _acc) -> _acc;
'encode_block_$items'([Items | _els], _acc) ->
    'encode_block_$items'(_els,
			  [encode_block_item(Items, []) | _acc]).

decode_block_item(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"item">>, _attrs, _els}) ->
    Jid = decode_block_item_attrs(__TopXMLNS, _attrs,
				  undefined),
    Jid.

decode_block_item_attrs(__TopXMLNS,
			[{<<"jid">>, _val} | _attrs], _Jid) ->
    decode_block_item_attrs(__TopXMLNS, _attrs, _val);
decode_block_item_attrs(__TopXMLNS, [_ | _attrs],
			Jid) ->
    decode_block_item_attrs(__TopXMLNS, _attrs, Jid);
decode_block_item_attrs(__TopXMLNS, [], Jid) ->
    decode_block_item_attr_jid(__TopXMLNS, Jid).

encode_block_item(Jid, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_block_item_attr_jid(Jid, _xmlns_attrs),
    {xmlel, <<"item">>, _attrs, _els}.

decode_block_item_attr_jid(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"item">>, __TopXMLNS}});
decode_block_item_attr_jid(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"item">>, __TopXMLNS}});
      _res -> _res
    end.

encode_block_item_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_privacy(__TopXMLNS, __IgnoreEls,
	       {xmlel, <<"query">>, _attrs, _els}) ->
    {Lists, Default, Active} =
	decode_privacy_els(__TopXMLNS, __IgnoreEls, _els, [],
			   undefined, undefined),
    {privacy, Lists, Default, Active}.

decode_privacy_els(__TopXMLNS, __IgnoreEls, [], Lists,
		   Default, Active) ->
    {lists:reverse(Lists), Default, Active};
decode_privacy_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"list">>, _attrs, _} = _el | _els], Lists,
		   Default, Active) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_privacy_els(__TopXMLNS, __IgnoreEls, _els,
			      [decode_privacy_list(__TopXMLNS, __IgnoreEls, _el)
			       | Lists],
			      Default, Active);
       true ->
	   decode_privacy_els(__TopXMLNS, __IgnoreEls, _els, Lists,
			      Default, Active)
    end;
decode_privacy_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"default">>, _attrs, _} = _el | _els], Lists,
		   Default, Active) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_privacy_els(__TopXMLNS, __IgnoreEls, _els, Lists,
			      decode_privacy_default_list(__TopXMLNS,
							  __IgnoreEls, _el),
			      Active);
       true ->
	   decode_privacy_els(__TopXMLNS, __IgnoreEls, _els, Lists,
			      Default, Active)
    end;
decode_privacy_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"active">>, _attrs, _} = _el | _els], Lists,
		   Default, Active) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_privacy_els(__TopXMLNS, __IgnoreEls, _els, Lists,
			      Default,
			      decode_privacy_active_list(__TopXMLNS,
							 __IgnoreEls, _el));
       true ->
	   decode_privacy_els(__TopXMLNS, __IgnoreEls, _els, Lists,
			      Default, Active)
    end;
decode_privacy_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		   Lists, Default, Active) ->
    decode_privacy_els(__TopXMLNS, __IgnoreEls, _els, Lists,
		       Default, Active).

encode_privacy({privacy, Lists, Default, Active},
	       _xmlns_attrs) ->
    _els = lists:reverse('encode_privacy_$lists'(Lists,
						 'encode_privacy_$default'(Default,
									   'encode_privacy_$active'(Active,
												    [])))),
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

decode_privacy_active_list(__TopXMLNS, __IgnoreEls,
			   {xmlel, <<"active">>, _attrs, _els}) ->
    Name = decode_privacy_active_list_attrs(__TopXMLNS,
					    _attrs, undefined),
    Name.

decode_privacy_active_list_attrs(__TopXMLNS,
				 [{<<"name">>, _val} | _attrs], _Name) ->
    decode_privacy_active_list_attrs(__TopXMLNS, _attrs,
				     _val);
decode_privacy_active_list_attrs(__TopXMLNS,
				 [_ | _attrs], Name) ->
    decode_privacy_active_list_attrs(__TopXMLNS, _attrs,
				     Name);
decode_privacy_active_list_attrs(__TopXMLNS, [],
				 Name) ->
    decode_privacy_active_list_attr_name(__TopXMLNS, Name).

encode_privacy_active_list(Name, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_privacy_active_list_attr_name(Name,
						  _xmlns_attrs),
    {xmlel, <<"active">>, _attrs, _els}.

decode_privacy_active_list_attr_name(__TopXMLNS,
				     undefined) ->
    none;
decode_privacy_active_list_attr_name(__TopXMLNS,
				     _val) ->
    _val.

encode_privacy_active_list_attr_name(none, _acc) ->
    _acc;
encode_privacy_active_list_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_privacy_default_list(__TopXMLNS, __IgnoreEls,
			    {xmlel, <<"default">>, _attrs, _els}) ->
    Name = decode_privacy_default_list_attrs(__TopXMLNS,
					     _attrs, undefined),
    Name.

decode_privacy_default_list_attrs(__TopXMLNS,
				  [{<<"name">>, _val} | _attrs], _Name) ->
    decode_privacy_default_list_attrs(__TopXMLNS, _attrs,
				      _val);
decode_privacy_default_list_attrs(__TopXMLNS,
				  [_ | _attrs], Name) ->
    decode_privacy_default_list_attrs(__TopXMLNS, _attrs,
				      Name);
decode_privacy_default_list_attrs(__TopXMLNS, [],
				  Name) ->
    decode_privacy_default_list_attr_name(__TopXMLNS, Name).

encode_privacy_default_list(Name, _xmlns_attrs) ->
    _els = [],
    _attrs = encode_privacy_default_list_attr_name(Name,
						   _xmlns_attrs),
    {xmlel, <<"default">>, _attrs, _els}.

decode_privacy_default_list_attr_name(__TopXMLNS,
				      undefined) ->
    none;
decode_privacy_default_list_attr_name(__TopXMLNS,
				      _val) ->
    _val.

encode_privacy_default_list_attr_name(none, _acc) ->
    _acc;
encode_privacy_default_list_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_privacy_list(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"list">>, _attrs, _els}) ->
    Items = decode_privacy_list_els(__TopXMLNS, __IgnoreEls,
				    _els, []),
    Name = decode_privacy_list_attrs(__TopXMLNS, _attrs,
				     undefined),
    {privacy_list, Name, Items}.

decode_privacy_list_els(__TopXMLNS, __IgnoreEls, [],
			Items) ->
    lists:reverse(Items);
decode_privacy_list_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"item">>, _attrs, _} = _el | _els], Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_privacy_list_els(__TopXMLNS, __IgnoreEls, _els,
				   [decode_privacy_item(__TopXMLNS, __IgnoreEls,
							_el)
				    | Items]);
       true ->
	   decode_privacy_list_els(__TopXMLNS, __IgnoreEls, _els,
				   Items)
    end;
decode_privacy_list_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Items) ->
    decode_privacy_list_els(__TopXMLNS, __IgnoreEls, _els,
			    Items).

decode_privacy_list_attrs(__TopXMLNS,
			  [{<<"name">>, _val} | _attrs], _Name) ->
    decode_privacy_list_attrs(__TopXMLNS, _attrs, _val);
decode_privacy_list_attrs(__TopXMLNS, [_ | _attrs],
			  Name) ->
    decode_privacy_list_attrs(__TopXMLNS, _attrs, Name);
decode_privacy_list_attrs(__TopXMLNS, [], Name) ->
    decode_privacy_list_attr_name(__TopXMLNS, Name).

encode_privacy_list({privacy_list, Name, Items},
		    _xmlns_attrs) ->
    _els = lists:reverse('encode_privacy_list_$items'(Items,
						      [])),
    _attrs = encode_privacy_list_attr_name(Name,
					   _xmlns_attrs),
    {xmlel, <<"list">>, _attrs, _els}.

'encode_privacy_list_$items'([], _acc) -> _acc;
'encode_privacy_list_$items'([Items | _els], _acc) ->
    'encode_privacy_list_$items'(_els,
				 [encode_privacy_item(Items, []) | _acc]).

decode_privacy_list_attr_name(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"name">>, <<"list">>, __TopXMLNS}});
decode_privacy_list_attr_name(__TopXMLNS, _val) -> _val.

encode_privacy_list_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_privacy_item(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"item">>, _attrs, _els}) ->
    Kinds = decode_privacy_item_els(__TopXMLNS, __IgnoreEls,
				    _els, []),
    {Action, Order, Type, Value} =
	decode_privacy_item_attrs(__TopXMLNS, _attrs, undefined,
				  undefined, undefined, undefined),
    {privacy_item, Order, Action, Type, Value, Kinds}.

decode_privacy_item_els(__TopXMLNS, __IgnoreEls, [],
			Kinds) ->
    lists:reverse(Kinds);
decode_privacy_item_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"message">>, _attrs, _} = _el | _els],
			Kinds) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_privacy_item_els(__TopXMLNS, __IgnoreEls, _els,
				   Kinds);
       true ->
	   decode_privacy_item_els(__TopXMLNS, __IgnoreEls, _els,
				   Kinds)
    end;
decode_privacy_item_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"iq">>, _attrs, _} = _el | _els], Kinds) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_privacy_item_els(__TopXMLNS, __IgnoreEls, _els,
				   Kinds);
       true ->
	   decode_privacy_item_els(__TopXMLNS, __IgnoreEls, _els,
				   Kinds)
    end;
decode_privacy_item_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"presence-in">>, _attrs, _} = _el | _els],
			Kinds) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_privacy_item_els(__TopXMLNS, __IgnoreEls, _els,
				   Kinds);
       true ->
	   decode_privacy_item_els(__TopXMLNS, __IgnoreEls, _els,
				   Kinds)
    end;
decode_privacy_item_els(__TopXMLNS, __IgnoreEls,
			[{xmlel, <<"presence-out">>, _attrs, _} = _el | _els],
			Kinds) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_privacy_item_els(__TopXMLNS, __IgnoreEls, _els,
				   Kinds);
       true ->
	   decode_privacy_item_els(__TopXMLNS, __IgnoreEls, _els,
				   Kinds)
    end;
decode_privacy_item_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Kinds) ->
    decode_privacy_item_els(__TopXMLNS, __IgnoreEls, _els,
			    Kinds).

decode_privacy_item_attrs(__TopXMLNS,
			  [{<<"action">>, _val} | _attrs], _Action, Order, Type,
			  Value) ->
    decode_privacy_item_attrs(__TopXMLNS, _attrs, _val,
			      Order, Type, Value);
decode_privacy_item_attrs(__TopXMLNS,
			  [{<<"order">>, _val} | _attrs], Action, _Order, Type,
			  Value) ->
    decode_privacy_item_attrs(__TopXMLNS, _attrs, Action,
			      _val, Type, Value);
decode_privacy_item_attrs(__TopXMLNS,
			  [{<<"type">>, _val} | _attrs], Action, Order, _Type,
			  Value) ->
    decode_privacy_item_attrs(__TopXMLNS, _attrs, Action,
			      Order, _val, Value);
decode_privacy_item_attrs(__TopXMLNS,
			  [{<<"value">>, _val} | _attrs], Action, Order, Type,
			  _Value) ->
    decode_privacy_item_attrs(__TopXMLNS, _attrs, Action,
			      Order, Type, _val);
decode_privacy_item_attrs(__TopXMLNS, [_ | _attrs],
			  Action, Order, Type, Value) ->
    decode_privacy_item_attrs(__TopXMLNS, _attrs, Action,
			      Order, Type, Value);
decode_privacy_item_attrs(__TopXMLNS, [], Action, Order,
			  Type, Value) ->
    {decode_privacy_item_attr_action(__TopXMLNS, Action),
     decode_privacy_item_attr_order(__TopXMLNS, Order),
     decode_privacy_item_attr_type(__TopXMLNS, Type),
     decode_privacy_item_attr_value(__TopXMLNS, Value)}.

encode_privacy_item({privacy_item, Order, Action, Type,
		     Value, Kinds},
		    _xmlns_attrs) ->
    _els = lists:reverse('encode_privacy_item_$kinds'(Kinds,
						      [])),
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

decode_privacy_item_attr_action(__TopXMLNS,
				undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"action">>, <<"item">>, __TopXMLNS}});
decode_privacy_item_attr_action(__TopXMLNS, _val) ->
    case catch dec_enum(_val, [allow, deny]) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"action">>, <<"item">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_privacy_item_attr_action(_val, _acc) ->
    [{<<"action">>, enc_enum(_val)} | _acc].

decode_privacy_item_attr_order(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"order">>, <<"item">>, __TopXMLNS}});
decode_privacy_item_attr_order(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"order">>, <<"item">>, __TopXMLNS}});
      _res -> _res
    end.

encode_privacy_item_attr_order(_val, _acc) ->
    [{<<"order">>, enc_int(_val)} | _acc].

decode_privacy_item_attr_type(__TopXMLNS, undefined) ->
    undefined;
decode_privacy_item_attr_type(__TopXMLNS, _val) ->
    case catch dec_enum(_val, [group, jid, subscription]) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"type">>, <<"item">>, __TopXMLNS}});
      _res -> _res
    end.

encode_privacy_item_attr_type(undefined, _acc) -> _acc;
encode_privacy_item_attr_type(_val, _acc) ->
    [{<<"type">>, enc_enum(_val)} | _acc].

decode_privacy_item_attr_value(__TopXMLNS, undefined) ->
    undefined;
decode_privacy_item_attr_value(__TopXMLNS, _val) ->
    _val.

encode_privacy_item_attr_value(undefined, _acc) -> _acc;
encode_privacy_item_attr_value(_val, _acc) ->
    [{<<"value">>, _val} | _acc].

decode_privacy_presence_out(__TopXMLNS, __IgnoreEls,
			    {xmlel, <<"presence-out">>, _attrs, _els}) ->
    'presence-out'.

encode_privacy_presence_out('presence-out',
			    _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"presence-out">>, _attrs, _els}.

decode_privacy_presence_in(__TopXMLNS, __IgnoreEls,
			   {xmlel, <<"presence-in">>, _attrs, _els}) ->
    'presence-in'.

encode_privacy_presence_in('presence-in',
			   _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"presence-in">>, _attrs, _els}.

decode_privacy_iq(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"iq">>, _attrs, _els}) ->
    iq.

encode_privacy_iq(iq, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"iq">>, _attrs, _els}.

decode_privacy_message(__TopXMLNS, __IgnoreEls,
		       {xmlel, <<"message">>, _attrs, _els}) ->
    message.

encode_privacy_message(message, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"message">>, _attrs, _els}.

decode_roster(__TopXMLNS, __IgnoreEls,
	      {xmlel, <<"query">>, _attrs, _els}) ->
    Items = decode_roster_els(__TopXMLNS, __IgnoreEls, _els,
			      []),
    Ver = decode_roster_attrs(__TopXMLNS, _attrs,
			      undefined),
    {roster, Items, Ver}.

decode_roster_els(__TopXMLNS, __IgnoreEls, [], Items) ->
    lists:reverse(Items);
decode_roster_els(__TopXMLNS, __IgnoreEls,
		  [{xmlel, <<"item">>, _attrs, _} = _el | _els], Items) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_roster_els(__TopXMLNS, __IgnoreEls, _els,
			     [decode_roster_item(__TopXMLNS, __IgnoreEls, _el)
			      | Items]);
       true ->
	   decode_roster_els(__TopXMLNS, __IgnoreEls, _els, Items)
    end;
decode_roster_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		  Items) ->
    decode_roster_els(__TopXMLNS, __IgnoreEls, _els, Items).

decode_roster_attrs(__TopXMLNS,
		    [{<<"ver">>, _val} | _attrs], _Ver) ->
    decode_roster_attrs(__TopXMLNS, _attrs, _val);
decode_roster_attrs(__TopXMLNS, [_ | _attrs], Ver) ->
    decode_roster_attrs(__TopXMLNS, _attrs, Ver);
decode_roster_attrs(__TopXMLNS, [], Ver) ->
    decode_roster_attr_ver(__TopXMLNS, Ver).

encode_roster({roster, Items, Ver}, _xmlns_attrs) ->
    _els = lists:reverse('encode_roster_$items'(Items, [])),
    _attrs = encode_roster_attr_ver(Ver, _xmlns_attrs),
    {xmlel, <<"query">>, _attrs, _els}.

'encode_roster_$items'([], _acc) -> _acc;
'encode_roster_$items'([Items | _els], _acc) ->
    'encode_roster_$items'(_els,
			   [encode_roster_item(Items, []) | _acc]).

decode_roster_attr_ver(__TopXMLNS, undefined) ->
    undefined;
decode_roster_attr_ver(__TopXMLNS, _val) -> _val.

encode_roster_attr_ver(undefined, _acc) -> _acc;
encode_roster_attr_ver(_val, _acc) ->
    [{<<"ver">>, _val} | _acc].

decode_roster_item(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"item">>, _attrs, _els}) ->
    Groups = decode_roster_item_els(__TopXMLNS, __IgnoreEls,
				    _els, []),
    {Jid, Name, Subscription, Ask} =
	decode_roster_item_attrs(__TopXMLNS, _attrs, undefined,
				 undefined, undefined, undefined),
    {roster_item, Jid, Name, Groups, Subscription, Ask}.

decode_roster_item_els(__TopXMLNS, __IgnoreEls, [],
		       Groups) ->
    lists:reverse(Groups);
decode_roster_item_els(__TopXMLNS, __IgnoreEls,
		       [{xmlel, <<"group">>, _attrs, _} = _el | _els],
		       Groups) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_roster_item_els(__TopXMLNS, __IgnoreEls, _els,
				  [decode_roster_group(__TopXMLNS, __IgnoreEls,
						       _el)
				   | Groups]);
       true ->
	   decode_roster_item_els(__TopXMLNS, __IgnoreEls, _els,
				  Groups)
    end;
decode_roster_item_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Groups) ->
    decode_roster_item_els(__TopXMLNS, __IgnoreEls, _els,
			   Groups).

decode_roster_item_attrs(__TopXMLNS,
			 [{<<"jid">>, _val} | _attrs], _Jid, Name, Subscription,
			 Ask) ->
    decode_roster_item_attrs(__TopXMLNS, _attrs, _val, Name,
			     Subscription, Ask);
decode_roster_item_attrs(__TopXMLNS,
			 [{<<"name">>, _val} | _attrs], Jid, _Name,
			 Subscription, Ask) ->
    decode_roster_item_attrs(__TopXMLNS, _attrs, Jid, _val,
			     Subscription, Ask);
decode_roster_item_attrs(__TopXMLNS,
			 [{<<"subscription">>, _val} | _attrs], Jid, Name,
			 _Subscription, Ask) ->
    decode_roster_item_attrs(__TopXMLNS, _attrs, Jid, Name,
			     _val, Ask);
decode_roster_item_attrs(__TopXMLNS,
			 [{<<"ask">>, _val} | _attrs], Jid, Name, Subscription,
			 _Ask) ->
    decode_roster_item_attrs(__TopXMLNS, _attrs, Jid, Name,
			     Subscription, _val);
decode_roster_item_attrs(__TopXMLNS, [_ | _attrs], Jid,
			 Name, Subscription, Ask) ->
    decode_roster_item_attrs(__TopXMLNS, _attrs, Jid, Name,
			     Subscription, Ask);
decode_roster_item_attrs(__TopXMLNS, [], Jid, Name,
			 Subscription, Ask) ->
    {decode_roster_item_attr_jid(__TopXMLNS, Jid),
     decode_roster_item_attr_name(__TopXMLNS, Name),
     decode_roster_item_attr_subscription(__TopXMLNS,
					  Subscription),
     decode_roster_item_attr_ask(__TopXMLNS, Ask)}.

encode_roster_item({roster_item, Jid, Name, Groups,
		    Subscription, Ask},
		   _xmlns_attrs) ->
    _els =
	lists:reverse('encode_roster_item_$groups'(Groups, [])),
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

decode_roster_item_attr_jid(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"item">>, __TopXMLNS}});
decode_roster_item_attr_jid(__TopXMLNS, _val) ->
    case catch dec_jid(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"item">>, __TopXMLNS}});
      _res -> _res
    end.

encode_roster_item_attr_jid(_val, _acc) ->
    [{<<"jid">>, enc_jid(_val)} | _acc].

decode_roster_item_attr_name(__TopXMLNS, undefined) ->
    undefined;
decode_roster_item_attr_name(__TopXMLNS, _val) -> _val.

encode_roster_item_attr_name(undefined, _acc) -> _acc;
encode_roster_item_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_roster_item_attr_subscription(__TopXMLNS,
				     undefined) ->
    none;
decode_roster_item_attr_subscription(__TopXMLNS,
				     _val) ->
    case catch dec_enum(_val,
			[none, to, from, both, remove])
	of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"subscription">>, <<"item">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_roster_item_attr_subscription(none, _acc) ->
    _acc;
encode_roster_item_attr_subscription(_val, _acc) ->
    [{<<"subscription">>, enc_enum(_val)} | _acc].

decode_roster_item_attr_ask(__TopXMLNS, undefined) ->
    undefined;
decode_roster_item_attr_ask(__TopXMLNS, _val) ->
    case catch dec_enum(_val, [subscribe]) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"ask">>, <<"item">>, __TopXMLNS}});
      _res -> _res
    end.

encode_roster_item_attr_ask(undefined, _acc) -> _acc;
encode_roster_item_attr_ask(_val, _acc) ->
    [{<<"ask">>, enc_enum(_val)} | _acc].

decode_roster_group(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"group">>, _attrs, _els}) ->
    Cdata = decode_roster_group_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_roster_group_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_roster_group_cdata(__TopXMLNS, Cdata);
decode_roster_group_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_roster_group_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_roster_group_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_roster_group_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_roster_group(Cdata, _xmlns_attrs) ->
    _els = encode_roster_group_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"group">>, _attrs, _els}.

decode_roster_group_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmpp_codec,
		  {missing_cdata, <<>>, <<"group">>, __TopXMLNS}});
decode_roster_group_cdata(__TopXMLNS, _val) -> _val.

encode_roster_group_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_version(__TopXMLNS, __IgnoreEls,
	       {xmlel, <<"query">>, _attrs, _els}) ->
    {Ver, Os, Name} = decode_version_els(__TopXMLNS,
					 __IgnoreEls, _els, undefined,
					 undefined, undefined),
    {version, Name, Ver, Os}.

decode_version_els(__TopXMLNS, __IgnoreEls, [], Ver, Os,
		   Name) ->
    {Ver, Os, Name};
decode_version_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"name">>, _attrs, _} = _el | _els], Ver, Os,
		   Name) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_version_els(__TopXMLNS, __IgnoreEls, _els, Ver,
			      Os,
			      decode_version_name(__TopXMLNS, __IgnoreEls,
						  _el));
       true ->
	   decode_version_els(__TopXMLNS, __IgnoreEls, _els, Ver,
			      Os, Name)
    end;
decode_version_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"version">>, _attrs, _} = _el | _els], Ver,
		   Os, Name) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_version_els(__TopXMLNS, __IgnoreEls, _els,
			      decode_version_ver(__TopXMLNS, __IgnoreEls, _el),
			      Os, Name);
       true ->
	   decode_version_els(__TopXMLNS, __IgnoreEls, _els, Ver,
			      Os, Name)
    end;
decode_version_els(__TopXMLNS, __IgnoreEls,
		   [{xmlel, <<"os">>, _attrs, _} = _el | _els], Ver, Os,
		   Name) ->
    _xmlns = get_attr(<<"xmlns">>, _attrs),
    if _xmlns == <<>>; _xmlns == __TopXMLNS ->
	   decode_version_els(__TopXMLNS, __IgnoreEls, _els, Ver,
			      decode_version_os(__TopXMLNS, __IgnoreEls, _el),
			      Name);
       true ->
	   decode_version_els(__TopXMLNS, __IgnoreEls, _els, Ver,
			      Os, Name)
    end;
decode_version_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		   Ver, Os, Name) ->
    decode_version_els(__TopXMLNS, __IgnoreEls, _els, Ver,
		       Os, Name).

encode_version({version, Name, Ver, Os},
	       _xmlns_attrs) ->
    _els = lists:reverse('encode_version_$ver'(Ver,
					       'encode_version_$os'(Os,
								    'encode_version_$name'(Name,
											   [])))),
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

decode_version_os(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"os">>, _attrs, _els}) ->
    Cdata = decode_version_os_els(__TopXMLNS, __IgnoreEls,
				  _els, <<>>),
    Cdata.

decode_version_os_els(__TopXMLNS, __IgnoreEls, [],
		      Cdata) ->
    decode_version_os_cdata(__TopXMLNS, Cdata);
decode_version_os_els(__TopXMLNS, __IgnoreEls,
		      [{xmlcdata, _data} | _els], Cdata) ->
    decode_version_os_els(__TopXMLNS, __IgnoreEls, _els,
			  <<Cdata/binary, _data/binary>>);
decode_version_os_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Cdata) ->
    decode_version_os_els(__TopXMLNS, __IgnoreEls, _els,
			  Cdata).

encode_version_os(Cdata, _xmlns_attrs) ->
    _els = encode_version_os_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"os">>, _attrs, _els}.

decode_version_os_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmpp_codec,
		  {missing_cdata, <<>>, <<"os">>, __TopXMLNS}});
decode_version_os_cdata(__TopXMLNS, _val) -> _val.

encode_version_os_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_version_ver(__TopXMLNS, __IgnoreEls,
		   {xmlel, <<"version">>, _attrs, _els}) ->
    Cdata = decode_version_ver_els(__TopXMLNS, __IgnoreEls,
				   _els, <<>>),
    Cdata.

decode_version_ver_els(__TopXMLNS, __IgnoreEls, [],
		       Cdata) ->
    decode_version_ver_cdata(__TopXMLNS, Cdata);
decode_version_ver_els(__TopXMLNS, __IgnoreEls,
		       [{xmlcdata, _data} | _els], Cdata) ->
    decode_version_ver_els(__TopXMLNS, __IgnoreEls, _els,
			   <<Cdata/binary, _data/binary>>);
decode_version_ver_els(__TopXMLNS, __IgnoreEls,
		       [_ | _els], Cdata) ->
    decode_version_ver_els(__TopXMLNS, __IgnoreEls, _els,
			   Cdata).

encode_version_ver(Cdata, _xmlns_attrs) ->
    _els = encode_version_ver_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"version">>, _attrs, _els}.

decode_version_ver_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmpp_codec,
		  {missing_cdata, <<>>, <<"version">>, __TopXMLNS}});
decode_version_ver_cdata(__TopXMLNS, _val) -> _val.

encode_version_ver_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_version_name(__TopXMLNS, __IgnoreEls,
		    {xmlel, <<"name">>, _attrs, _els}) ->
    Cdata = decode_version_name_els(__TopXMLNS, __IgnoreEls,
				    _els, <<>>),
    Cdata.

decode_version_name_els(__TopXMLNS, __IgnoreEls, [],
			Cdata) ->
    decode_version_name_cdata(__TopXMLNS, Cdata);
decode_version_name_els(__TopXMLNS, __IgnoreEls,
			[{xmlcdata, _data} | _els], Cdata) ->
    decode_version_name_els(__TopXMLNS, __IgnoreEls, _els,
			    <<Cdata/binary, _data/binary>>);
decode_version_name_els(__TopXMLNS, __IgnoreEls,
			[_ | _els], Cdata) ->
    decode_version_name_els(__TopXMLNS, __IgnoreEls, _els,
			    Cdata).

encode_version_name(Cdata, _xmlns_attrs) ->
    _els = encode_version_name_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"name">>, _attrs, _els}.

decode_version_name_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmpp_codec,
		  {missing_cdata, <<>>, <<"name">>, __TopXMLNS}});
decode_version_name_cdata(__TopXMLNS, _val) -> _val.

encode_version_name_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_last(__TopXMLNS, __IgnoreEls,
	    {xmlel, <<"query">>, _attrs, _els}) ->
    Text = decode_last_els(__TopXMLNS, __IgnoreEls, _els,
			   <<>>),
    Seconds = decode_last_attrs(__TopXMLNS, _attrs,
				undefined),
    {last, Seconds, Text}.

decode_last_els(__TopXMLNS, __IgnoreEls, [], Text) ->
    decode_last_cdata(__TopXMLNS, Text);
decode_last_els(__TopXMLNS, __IgnoreEls,
		[{xmlcdata, _data} | _els], Text) ->
    decode_last_els(__TopXMLNS, __IgnoreEls, _els,
		    <<Text/binary, _data/binary>>);
decode_last_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		Text) ->
    decode_last_els(__TopXMLNS, __IgnoreEls, _els, Text).

decode_last_attrs(__TopXMLNS,
		  [{<<"seconds">>, _val} | _attrs], _Seconds) ->
    decode_last_attrs(__TopXMLNS, _attrs, _val);
decode_last_attrs(__TopXMLNS, [_ | _attrs], Seconds) ->
    decode_last_attrs(__TopXMLNS, _attrs, Seconds);
decode_last_attrs(__TopXMLNS, [], Seconds) ->
    decode_last_attr_seconds(__TopXMLNS, Seconds).

encode_last({last, Seconds, Text}, _xmlns_attrs) ->
    _els = encode_last_cdata(Text, []),
    _attrs = encode_last_attr_seconds(Seconds,
				      _xmlns_attrs),
    {xmlel, <<"query">>, _attrs, _els}.

decode_last_attr_seconds(__TopXMLNS, undefined) ->
    undefined;
decode_last_attr_seconds(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"seconds">>, <<"query">>,
			 __TopXMLNS}});
      _res -> _res
    end.

encode_last_attr_seconds(undefined, _acc) -> _acc;
encode_last_attr_seconds(_val, _acc) ->
    [{<<"seconds">>, enc_int(_val)} | _acc].

decode_last_cdata(__TopXMLNS, <<>>) -> undefined;
decode_last_cdata(__TopXMLNS, _val) -> _val.

encode_last_cdata(undefined, _acc) -> _acc;
encode_last_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].
