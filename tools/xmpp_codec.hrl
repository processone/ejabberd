-record(last, {seconds, text}).

-record(version, {name, version, os}).

-record(roster, {item = [], ver}).

-record(roster_item,
	{jid, name, groups = [], subscription = none, ask}).

-record(privacy_item,
	{order, action, type, value, stanza}).

-record(privacy, {list = [], default, active}).

-record(privacy_list, {name, privacy_item = []}).

-record(block, {block_item = []}).

-record(unblock, {block_item = []}).

-record(block_list, {}).

-record(disco_info,
	{node, identity = [], feature = [], xdata = []}).

-record(disco_items, {node, items = []}).

-record(disco_item, {jid, name, node}).

-record(private, {sub_els = []}).

-record(bookmark_conference,
	{name, jid, autojoin = false, nick, password}).

-record(bookmark_storage, {conference = [], url = []}).

-record(bookmark_url, {name, url}).

-record(stats, {stat = []}).

-record(stat, {name, units, value, error = []}).

-record(iq,
	{id, type, lang, from, to, error, sub_els = []}).

-record(message,
	{id, type = normal, lang, from, to, subject = [],
	 body = [], thread, error, sub_els = []}).

-record(presence,
	{id, type, lang, from, to, show, status = [], priority,
	 error, sub_els = []}).

-record(error, {error_type, by, reason, text}).

-record(redirect, {cdata}).

-record(gone, {cdata}).

-record(bind, {jid, resource}).

-record(sasl_auth, {mechanism, cdata}).

-record(sasl_abort, {}).

-record(sasl_challenge, {cdata}).

-record(sasl_response, {cdata}).

-record(sasl_success, {cdata}).

-record(sasl_failure, {reason, text}).

-record(sasl_mechanisms, {mechanism = []}).

-record(starttls, {required = false}).

-record(starttls_proceed, {}).

-record(starttls_failure, {}).

-record(stream_features, {sub_els = []}).

-record(p1_push, {}).

-record(p1_rebind, {}).

-record(p1_ack, {}).

-record(caps, {hash, node, ver}).

-record(register, {}).

-record(session, {}).

-record(ping, {}).

-record(time, {tzo, utc}).

-record(stream_error, {reason, text}).

-record('see-other-host', {cdata}).

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

-record(vcard,
	{version, fn, n, nickname, photo, bday, adr = [],
	 label = [], tel = [], email = [], jabberid, mailer, tz,
	 geo, title, role, logo, org, categories = [], note,
	 prodid, rev, 'sort-string', sound, uid, url, class, key,
	 desc}).

-record(xfield,
	{label, type, var, required = false, desc, values = [],
	 options = []}).

-record(xdata,
	{type, instructions = [], title, reported, items = [],
	 fields = []}).

-record(pubsub_subscription, {jid, node, subid, type}).

-record(pubsub_affiliation, {node, type}).

-record(pubsub_item, {id, sub_els = []}).

-record(pubsub_items,
	{node, max_items, subid, item = []}).

-record(pubsub_event, {items = []}).

-record(pubsub,
	{subscriptions, affiliations, publish, subscribe}).

-record(delay, {stamp, from}).
