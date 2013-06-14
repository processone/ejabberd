%% Created automatically by XML generator (xml_gen.erl)
%% Source: xmpp_codec.spec
%% Date: Fri, 14 Jun 2013 08:43:04 GMT

-record(last, {seconds, text}).

-record(version,
	{version_name, version_ver, version_os}).

-record(roster_item,
	{jid, name, groups = [], subscription = none, ask}).

-record(roster, {item = [], ver}).

-record(privacy_item,
	{order, action, type, value, stanza}).

-record(privacy_list, {name, items = []}).

-record(privacy, {lists = [], default, active}).

-record(block, {items = []}).

-record(unblock, {items = []}).

-record(block_list, {}).

-record(identity, {category, type, name}).

-record(disco_info,
	{node, identity = [], feature = [], xdata = []}).

-record(disco_item, {jid, name, node}).

-record(disco_items, {node, items = []}).

-record(private, {sub_els = []}).

-record(bookmark_conference,
	{name, jid, autojoin = false, nick, password}).

-record(bookmark_url, {name, url}).

-record(bookmark_storage, {conference = [], url = []}).

-record(stat, {name, units, value, error = []}).

-record(stats, {stat = []}).

-record(iq,
	{id, type, lang, from, to, error, sub_els = []}).

-record(message,
	{id, type = normal, lang, from, to, subject = [],
	 body = [], thread, error, sub_els = []}).

-record(presence,
	{id, type, lang, from, to, show, status = [], priority,
	 error, sub_els = []}).

-record(gone, {uri}).

-record(redirect, {uri}).

-record(error, {type, by, reason, text}).

-record(bind, {jid, resource}).

-record(sasl_auth, {mechanism, text}).

-record(sasl_abort, {}).

-record(sasl_challenge, {text}).

-record(sasl_response, {text}).

-record(sasl_success, {text}).

-record(sasl_failure, {reason, text = []}).

-record(sasl_mechanisms, {list = []}).

-record(starttls, {required = false}).

-record(starttls_proceed, {}).

-record(starttls_failure, {}).

-record(stream_features, {sub_els = []}).

-record(p1_push, {}).

-record(p1_rebind, {}).

-record(p1_ack, {}).

-record(caps, {hash, node, ver}).

-record(feature_register, {}).

-record(register,
	{registered = false, instructions, username, nick,
	 password, name, first, last, email, address, city,
	 state, zip, phone, url, date, misc, text, key}).

-record(session, {}).

-record(ping, {}).

-record(time, {tzo, utc}).

-record('see-other-host', {host}).

-record(stream_error, {reason, text}).

-record(vcard_name,
	{family, given, middle, prefix, suffix}).

-record(vcard_adr,
	{home = false, work = false, postal = false,
	 parcel = false, dom = false, intl = false, pref = false,
	 pobox, extadd, street, locality, region, pcode, ctry}).

-record(vcard_label,
	{home = false, work = false, postal = false,
	 parcel = false, dom = false, intl = false, pref = false,
	 line = []}).

-record(vcard_tel,
	{home = false, work = false, voice = false, fax = false,
	 pager = false, msg = false, cell = false, video = false,
	 bbs = false, modem = false, isdn = false, pcs = false,
	 pref = false, number}).

-record(vcard_email,
	{home = false, work = false, internet = false,
	 pref = false, x400 = false, userid}).

-record(vcard_geo, {lat, lon}).

-record(vcard_logo, {type, binval, extval}).

-record(vcard_photo, {type, binval, extval}).

-record(vcard_org, {name, units = []}).

-record(vcard_sound, {phonetic, binval, extval}).

-record(vcard_key, {type, cred}).

-record(vcard_agent, {vcard, extval}).

-record(vcard,
	{version, fn, n, nickname, photo, bday, adr = [],
	 label = [], tel = [], email = [], jabberid, mailer, tz,
	 geo, title, role, logo, org, categories = [], note,
	 prodid, agent, rev, sort_string, sound, uid, url, class,
	 key, desc}).

-record(xdata_field,
	{label, type, var, required = false, desc, values = [],
	 options = []}).

-record(xdata,
	{type, instructions = [], title, reported, items = [],
	 fields = []}).

-record(pubsub_subscription, {jid, node, subid, type}).

-record(pubsub_affiliation, {node, type}).

-record(pubsub_item, {id, sub_els = []}).

-record(pubsub_items,
	{node, max_items, subid, items = []}).

-record(pubsub_event_item, {id, node, publisher}).

-record(pubsub_event_items,
	{node, retract = [], items = []}).

-record(pubsub_event, {items = []}).

-record(pubsub,
	{subscriptions, affiliations, publish, subscribe}).

-record(delay, {stamp, from}).

-record(legacy_delay, {stamp, from}).
