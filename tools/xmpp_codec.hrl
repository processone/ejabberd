%% Created automatically by XML generator (xml_gen.erl)
%% Source: xmpp_codec.spec

-record(bind, {jid, resource}).

-record(block, {items = []}).

-record(block_list, {}).

-record(bookmark_conference,
	{name, jid, autojoin = false, nick, password}).

-record(bookmark_storage, {conference = [], url = []}).

-record(bookmark_url, {name, url}).

-record(bytestreams,
	{hosts = [], used, activate, dstaddr, mode = tcp, sid}).

-record(caps, {hash, node, ver}).

-record(compress, {methods = []}).

-record(compress_failure, {reason}).

-record(compressed, {}).

-record(compression, {methods = []}).

-record(delay, {stamp, from}).

-record(disco_info,
	{node, identity = [], feature = [], xdata = []}).

-record(disco_item, {jid, name, node}).

-record(disco_items, {node, items = []}).

-record(error, {type, by, reason, text}).

-record(feature_register, {}).

-record(gone, {uri}).

-record(identity, {category, type, name}).

-record(iq,
	{id, type, lang, from, to, error, sub_els = []}).

-record(last, {seconds, text}).

-record(legacy_delay, {stamp, from}).

-record(message,
	{id, type = normal, lang, from, to, subject = [],
	 body = [], thread, error, sub_els = []}).

-record(muc, {history, password}).

-record(muc_actor, {jid, nick}).

-record(muc_decline, {reason, from, to}).

-record(muc_history,
	{maxchars, maxstanzas, seconds, since}).

-record(muc_invite, {reason, from, to}).

-record(muc_item,
	{actor, continue, reason, affiliation, role, jid,
	 nick}).

-record(muc_owner, {destroy, config}).

-record(muc_owner_destroy, {jid, reason, password}).

-record(muc_user,
	{decline, destroy, invites = [], items = [],
	 status_codes = [], password}).

-record(muc_user_destroy, {reason, jid}).

-record(p1_ack, {}).

-record(p1_push, {}).

-record(p1_rebind, {}).

-record(ping, {}).

-record(presence,
	{id, type, lang, from, to, show, status = [], priority,
	 error, sub_els = []}).

-record(privacy, {lists = [], default, active}).

-record(privacy_item,
	{order, action, type, value, kinds = []}).

-record(privacy_list, {name, items = []}).

-record(private, {sub_els = []}).

-record(pubsub,
	{subscriptions, affiliations, publish, subscribe}).

-record(pubsub_affiliation, {node, type}).

-record(pubsub_event, {items = []}).

-record(pubsub_event_item, {id, node, publisher}).

-record(pubsub_event_items,
	{node, retract = [], items = []}).

-record(pubsub_item, {id, sub_els = []}).

-record(pubsub_items,
	{node, max_items, subid, items = []}).

-record(pubsub_subscription, {jid, node, subid, type}).

-record(redirect, {uri}).

-record(register,
	{registered = false, remove = false, instructions,
	 username, nick, password, name, first, last, email,
	 address, city, state, zip, phone, url, date, misc, text,
	 key}).

-record(roster, {items = [], ver}).

-record(roster_item,
	{jid, name, groups = [], subscription = none, ask}).

-record(sasl_abort, {}).

-record(sasl_auth, {mechanism, text}).

-record(sasl_challenge, {text}).

-record(sasl_failure, {reason, text = []}).

-record(sasl_mechanisms, {list = []}).

-record(sasl_response, {text}).

-record(sasl_success, {text}).

-record('see-other-host', {host}).

-record(session, {}).

-record(starttls, {required = false}).

-record(starttls_failure, {}).

-record(starttls_proceed, {}).

-record(stat, {name, units, value, error = []}).

-record(stats, {stat = []}).

-record(stream_error, {reason, text}).

-record(stream_features, {sub_els = []}).

-record(streamhost, {jid, host, port = 1080}).

-record(text, {lang, data}).

-record(time, {tzo, utc}).

-record(unblock, {items = []}).

-record(vcard,
	{version, fn, n, nickname, photo, bday, adr = [],
	 label = [], tel = [], email = [], jabberid, mailer, tz,
	 geo, title, role, logo, org, categories = [], note,
	 prodid, agent, rev, sort_string, sound, uid, url, class,
	 key, desc}).

-record(vcard_adr,
	{home = false, work = false, postal = false,
	 parcel = false, dom = false, intl = false, pref = false,
	 pobox, extadd, street, locality, region, pcode, ctry}).

-record(vcard_agent, {vcard, extval}).

-record(vcard_email,
	{home = false, work = false, internet = false,
	 pref = false, x400 = false, userid}).

-record(vcard_geo, {lat, lon}).

-record(vcard_key, {type, cred}).

-record(vcard_label,
	{home = false, work = false, postal = false,
	 parcel = false, dom = false, intl = false, pref = false,
	 line = []}).

-record(vcard_logo, {type, binval, extval}).

-record(vcard_name,
	{family, given, middle, prefix, suffix}).

-record(vcard_org, {name, units = []}).

-record(vcard_photo, {type, binval, extval}).

-record(vcard_sound, {phonetic, binval, extval}).

-record(vcard_tel,
	{home = false, work = false, voice = false, fax = false,
	 pager = false, msg = false, cell = false, video = false,
	 bbs = false, modem = false, isdn = false, pcs = false,
	 pref = false, number}).

-record(version,
	{version_name, version_ver, version_os}).

-record(xdata,
	{type, instructions = [], title, reported, items = [],
	 fields = []}).

-record(xdata_field,
	{label, type, var, required = false, desc, values = [],
	 options = []}).
