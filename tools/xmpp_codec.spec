{last,
 #elem{name = <<"query">>,
       xmlns = <<"jabber:iq:last">>,
       result = {last, '$seconds', '$text'},
       attrs = [#attr{name = <<"seconds">>,
                      default = undefined,
                      enc = {enc_int, []},
                      dec = {dec_int, [0, infinity]}}],
       cdata = #cdata{label = '$text'}}}.

{version_name,
 #elem{name = <<"name">>,
       xmlns = <<"jabber:iq:version">>,
       result = '$cdata',
       cdata = #cdata{label = '$cdata', required = true}}}.

{version_ver,
 #elem{name = <<"version">>,
       xmlns = <<"jabber:iq:version">>,
       result = '$cdata',
       cdata = #cdata{label = '$cdata', required = true}}}.

{version_os,
 #elem{name = <<"os">>,
       xmlns = <<"jabber:iq:version">>,
       result = '$cdata',
       cdata = #cdata{label = '$cdata', required = true}}}.

{version,
 #elem{name = <<"query">>,
       xmlns = <<"jabber:iq:version">>,
       result = {version, '$version_name', '$version_ver', '$version_os'},
       refs = [#ref{name = version_name,
                    min = 0, max = 1},
               #ref{name = version_ver,
                    min = 0, max = 1},
               #ref{name = version_os,
                    min = 0, max = 1}]}}.

{roster_group,
 #elem{name = <<"group">>,
       xmlns = <<"jabber:iq:roster">>,
       result = '$cdata',
       cdata = #cdata{required = true, label = '$cdata'}}}.

{roster_item,
 #elem{name = <<"item">>,
       xmlns = <<"jabber:iq:roster">>,
       result = {roster_item, '$jid', '$name',
                 '$groups', '$subscription', '$ask'},
       attrs = [#attr{name = <<"jid">>,
                      required = true,
                      dec = {dec_jid, []},
                      enc = {enc_jid, []}},
                #attr{name = <<"name">>},
                #attr{name = <<"subscription">>,
                      default = none,
                      enc = {enc_enum, []},
                      dec = {dec_enum, [[none,to,from,both,remove]]}},
                #attr{name = <<"ask">>,
                      default = undefined,
                      enc = {enc_enum, []},
                      dec = {dec_enum, [[subscribe]]}}],
       refs = [#ref{name = roster_group, label = '$groups'}]}}.

{roster,
 #elem{name = <<"query">>,
       xmlns = <<"jabber:iq:roster">>,
       result = {roster, '$item', '$ver'},
       attrs = [#attr{name = <<"ver">>}],
       refs = [#ref{name = roster_item, label = '$item'}]}}.

{privacy_message, #elem{name = <<"message">>, xmlns = <<"jabber:iq:privacy">>,
                        result = message}}.
{privacy_iq, #elem{name = <<"iq">>, xmlns = <<"jabber:iq:privacy">>,
                   result = iq}}.
{privacy_presence_in, #elem{name = <<"presence-in">>,
                            xmlns = <<"jabber:iq:privacy">>,
                            result = 'presence-in'}}.
{privacy_presence_out, #elem{name = <<"presence-out">>,
                             xmlns = <<"jabber:iq:privacy">>,
                             result = 'presence-out'}}.

{privacy_item,
 #elem{name = <<"item">>,
       xmlns = <<"jabber:iq:privacy">>,
       result = {privacy_item, '$order', '$action', '$type',
                 '$value', '$stanza'},
       attrs = [#attr{name = <<"action">>,
                      required = true,
                      dec = {dec_enum, [[allow, deny]]},
                      enc = {enc_enum, []}},
                #attr{name = <<"order">>,
                      required = true,
                      dec = {dec_int, [0, infinity]},
                      enc = {enc_int, []}},
                #attr{name = <<"type">>,
                      dec = {dec_enum, [[group, jid, subscription]]},
                      enc = {enc_enum, []}},
                #attr{name = <<"value">>}],
       refs = [#ref{name = privacy_message,
                    min = 0, max = 1,
                    label = '$stanza'},
               #ref{name = privacy_iq,
                    min = 0, max = 1,
                    label = '$stanza'},
               #ref{name = privacy_presence_in,
                    min = 0, max = 1,
                    label = '$stanza'},
               #ref{name = privacy_presence_out,
                    min = 0, max = 1,
                    label = '$stanza'}]}}.

{privacy_list,
 #elem{name = <<"list">>,
       xmlns = <<"jabber:iq:privacy">>,
       result = {privacy_list, '$name', '$items'},
       attrs = [#attr{name = <<"name">>,
                      required = true}],
       refs = [#ref{name = privacy_item,
                    label = '$items'}]}}.

{privacy_default_list,
 #elem{name = <<"default">>,
       xmlns = <<"jabber:iq:privacy">>,
       result = '$name',
       attrs = [#attr{name = <<"name">>,
                      default = none}]}}.

{privacy_active_list,
 #elem{name = <<"active">>,
       xmlns = <<"jabber:iq:privacy">>,
       result = '$name',
       attrs = [#attr{name = <<"name">>,
                      default = none}]}}.

{privacy,
 #elem{name = <<"query">>,
       xmlns = <<"jabber:iq:privacy">>,
       result = {privacy, '$lists', '$default', '$active'},
       refs = [#ref{name = privacy_list,
                    label = '$lists'},
               #ref{name = privacy_default_list,
                    min = 0, max = 1,
                    label = '$default'},
               #ref{name = privacy_active_list,
                    min = 0, max = 1,
                    label = '$active'}]}}.

{block_item,
 #elem{name = <<"item">>,
       xmlns = <<"urn:xmpp:blocking">>,
       result = '$jid',
       attrs = [#attr{name = <<"jid">>,
                      required = true,
                      dec = {dec_jid, []},
                      enc = {enc_jid, []}}]}}.

{block,
 #elem{name = <<"block">>,
       xmlns = <<"urn:xmpp:blocking">>,
       result = {block, '$items'},
       refs = [#ref{name = block_item,
                    label = '$items'}]}}.

{unblock,
 #elem{name = <<"unblock">>,
       xmlns = <<"urn:xmpp:blocking">>,
       result = {unblock, '$items'},
       refs = [#ref{name = block_item,
                    label = '$items'}]}}.

{block_list,
 #elem{name = <<"blocklist">>,
       xmlns = <<"urn:xmpp:blocking">>,
       result = {block_list}}}.

{disco_identity,
 #elem{name = <<"identity">>,
       xmlns = <<"http://jabber.org/protocol/disco#info">>,
       result = {identity, '$category', '$type', '$name'},
       attrs = [#attr{name = <<"category">>,
                      required = true},
                #attr{name = <<"type">>,
                      required = true},
                #attr{name = <<"name">>}]}}.

{disco_feature,
 #elem{name = <<"feature">>,
       xmlns = <<"http://jabber.org/protocol/disco#info">>,
       result = '$var',
       attrs = [#attr{name = <<"var">>,
                      required = true}]}}.

{disco_info,
 #elem{name = <<"query">>,
       xmlns = <<"http://jabber.org/protocol/disco#info">>,
       result = {disco_info, '$node', '$identity', '$feature', '$xdata'},
       attrs = [#attr{name = <<"node">>}],
       refs = [#ref{name = disco_identity,
                    label = '$identity'},
               #ref{name = disco_feature,
                    label = '$feature'},
               #ref{name = xdata,
                    label = '$xdata'}]}}.

{disco_item,
 #elem{name = <<"item">>,
       xmlns = <<"http://jabber.org/protocol/disco#items">>,
       result = {disco_item, '$jid', '$name', '$node'},
       attrs = [#attr{name = <<"jid">>,
                      dec = {dec_jid, []},
                      enc = {enc_jid, []},
                      required = true},
                #attr{name = <<"name">>},
                #attr{name = <<"node">>}]}}.
{disco_items,
 #elem{name = <<"query">>,
       xmlns = <<"http://jabber.org/protocol/disco#items">>,
       result = {disco_items, '$node', '$items'},
       attrs = [#attr{name = <<"node">>}],
       refs = [#ref{name = disco_item,
                    label = '$items'}]}}.

{private,
 #elem{name = <<"query">>,
       xmlns = <<"jabber:iq:private">>,
       result = {private, '$_els'}}}.

{conference_nick,
 #elem{name = <<"nick">>,
       xmlns = <<"storage:bookmarks">>,
       result = '$cdata'}}.

{conference_password,
 #elem{name = <<"password">>,
       xmlns = <<"storage:bookmarks">>,
       result = '$cdata'}}.

{bookmark_conference,
 #elem{name = <<"conference">>,
       xmlns = <<"storage:bookmarks">>,
       result = {bookmark_conference, '$name', '$jid',
                 '$autojoin', '$nick', '$password'},
       attrs = [#attr{name = <<"name">>,
                      required = true},
                #attr{name = <<"jid">>,
                      required = true,
                      dec = {dec_jid, []},
                      enc = {enc_jid, []}},
                #attr{name = <<"autojoin">>,
                      default = false,
                      dec = {dec_bool, []},
                      enc = {enc_bool, []}}],
       refs = [#ref{name = conference_nick,
                    label = '$nick',
                    min = 0, max = 1},
               #ref{name = conference_password,
                    label = '$password',
                    min = 0, max = 1}]}}.

{bookmark_url,
 #elem{name = <<"url">>,
       xmlns = <<"storage:bookmarks">>,
       result = {bookmark_url, '$name', '$url'},
       attrs = [#attr{name = <<"name">>,
                      required = true},
                #attr{name = <<"url">>,
                      required = true}]}}.

{bookmarks_storage,
 #elem{name = <<"storage">>,
       xmlns = <<"storage:bookmarks">>,
       result = {bookmark_storage, '$conference', '$url'},
       refs = [#ref{name = bookmark_conference,
                    label = '$conference'},
               #ref{name = bookmark_url,
                    label = '$url'}]}}.

{stat_error,
 #elem{name = <<"error">>,
       xmlns = <<"http://jabber.org/protocol/stats">>,
       result = {'$code', '$cdata'},
       attrs = [#attr{name = <<"code">>,
                      required = true,
                      enc = {enc_int, []},
                      dec = {dec_int, []}}]}}.

{stat,
 #elem{name = <<"stat">>,
       xmlns = <<"http://jabber.org/protocol/stats">>,
       result = {stat, '$name', '$units', '$value', '$error'},
       attrs = [#attr{name = <<"name">>,
                      required = true},
                #attr{name = <<"units">>},
                #attr{name = <<"value">>}],
       refs = [#ref{name = stat_error,
                    label = '$error'}]}}.

{stats,
 #elem{name = <<"query">>,
       xmlns = <<"http://jabber.org/protocol/stats">>,
       result = {stats, '$stat'},
       refs = [#ref{name = stat,
                    label = '$stat'}]}}.

{iq,
 #elem{name = <<"iq">>,
       xmlns = <<"jabber:client">>,
       result = {iq, '$id', '$type', '$lang', '$from', '$to',
                 '$error', '$_els'},
       attrs = [#attr{name = <<"id">>,
                      required = true},
                #attr{name = <<"type">>,
                      required = true,
                      enc = {enc_enum, []},
                      dec = {dec_enum, [[get, set, result, error]]}},
                #attr{name = <<"from">>,
                      dec = {dec_jid, []},
                      enc = {enc_jid, []}},
                #attr{name = <<"to">>,
                      dec = {dec_jid, []},
                      enc = {enc_jid, []}},
                #attr{name = <<"xml:lang">>,
                      label = '$lang'}],
       refs = [#ref{name = error, min = 0, max = 1, label = '$error'}]}}.

{message_subject,
 #elem{name = <<"subject">>,
       xmlns = <<"jabber:client">>,
       result = {'$lang', '$cdata'},
       attrs = [#attr{name = <<"xml:lang">>, label = '$lang'}]}}.

{message_body,
 #elem{name = <<"body">>,
       xmlns = <<"jabber:client">>,
       result = {'$lang', '$cdata'},
       attrs = [#attr{name = <<"xml:lang">>, label = '$lang'}]}}.

{message_thread,
 #elem{name = <<"thread">>,
       xmlns = <<"jabber:client">>,
       result = '$cdata'}}.

{message,
 #elem{name = <<"message">>,
       xmlns = <<"jabber:client">>,
       result = {message, '$id', '$type', '$lang', '$from', '$to',
                 '$subject', '$body', '$thread', '$error', '$_els'},
       attrs = [#attr{name = <<"id">>},
                #attr{name = <<"type">>,
                      default = normal,
                      enc = {enc_enum, []},
                      dec = {dec_enum, [[chat, normal, groupchat,
                                         headline, error]]}},
                #attr{name = <<"from">>,
                      dec = {dec_jid, []},
                      enc = {enc_jid, []}},
                #attr{name = <<"to">>,
                      dec = {dec_jid, []},
                      enc = {enc_jid, []}},
                #attr{name = <<"xml:lang">>,
                      label = '$lang'}],
       refs = [#ref{name = error, min = 0, max = 1, label = '$error'},
               #ref{name = message_subject, label = '$subject'},
               #ref{name = message_thread, min = 0, max = 1, label = '$thread'},
               #ref{name = message_body, label = '$body'}]}}.

{presence_show,
 #elem{name = <<"show">>,
       xmlns = <<"jabber:client">>,
       result = '$cdata',
       cdata = #cdata{enc = {enc_enum, []},
                      dec = {dec_enum, [[away, chat, dnd, xa]]}}}}.

{presence_status,
 #elem{name = <<"status">>,
       xmlns = <<"jabber:client">>,
       result = {'$lang', '$cdata'},
       attrs = [#attr{name = <<"xml:lang">>,
                      label = '$lang'}]}}.

{presence_priority,
 #elem{name = <<"priority">>,
       xmlns = <<"jabber:client">>,
       result = '$cdata',
       cdata = #cdata{enc = {enc_int, []},
                      dec = {dec_int, []}}}}.

{presence,
 #elem{name = <<"presence">>,
       xmlns = <<"jabber:client">>,
       result = {presence, '$id', '$type', '$lang', '$from', '$to',
                 '$show', '$status', '$priority', '$error', '$_els'},
       attrs = [#attr{name = <<"id">>},
                #attr{name = <<"type">>,
                      enc = {enc_enum, []},
                      dec = {dec_enum, [[unavailable, subscribe, subscribed,
                                         unsubscribe, unsubscribed,
                                         probe, error]]}},
                #attr{name = <<"from">>,
                      dec = {dec_jid, []},
                      enc = {enc_jid, []}},
                #attr{name = <<"to">>,
                      dec = {dec_jid, []},
                      enc = {enc_jid, []}},
                #attr{name = <<"xml:lang">>,
                      label = '$lang'}],
       refs = [#ref{name = error, min = 0, max = 1, label = '$error'},
               #ref{name = presence_show, min = 0, max = 1, label = '$show'},
               #ref{name = presence_status, label = '$status'},
               #ref{name = presence_priority, min = 0, max = 1,
                    label = '$priority'}]}}.

{error_bad_request,
 #elem{name = <<"bad-request">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'bad-request'}}.
{error_conflict,
 #elem{name = <<"conflict">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'conflict'}}.
{error_feature_not_implemented,
 #elem{name = <<"feature-not-implemented">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'feature-not-implemented'}}.
{error_forbidden,
 #elem{name = <<"forbidden">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'forbidden'}}.
{error_gone,
 #elem{name = <<"gone">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       cdata = #cdata{label = '$uri'},
       result = {'gone', '$uri'}}}.
{error_internal_server_error,
 #elem{name = <<"internal-server-error">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'internal-server-error'}}.
{error_item_not_found,
 #elem{name = <<"item-not-found">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'item-not-found'}}.
{error_jid_malformed,
 #elem{name = <<"jid-malformed">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'jid-malformed'}}.
{error_not_acceptable,
 #elem{name = <<"not-acceptable">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'not-acceptable'}}.
{error_not_allowed,
 #elem{name = <<"not-allowed">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'not-allowed'}}.
{error_not_authorized,
 #elem{name = <<"not-authorized">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'not-authorized'}}.
{error_policy_violation,
 #elem{name = <<"policy-violation">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'policy-violation'}}.
{error_recipient_unavailable,
 #elem{name = <<"recipient-unavailable">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'recipient-unavailable'}}.
{error_redirect,
 #elem{name = <<"redirect">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       cdata = #cdata{label = '$uri'},
       result = {'redirect', '$uri'}}}.
{error_registration_required,
 #elem{name = <<"registration-required">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'registration-required'}}.
{error_remote_server_not_found,
 #elem{name = <<"remote-server-not-found">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'remote-server-not-found'}}.
{error_remote_server_timeout,
 #elem{name = <<"remote-server-timeout">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'remote-server-timeout'}}.
{error_resource_constraint,
 #elem{name = <<"resource-constraint">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'resource-constraint'}}.
{error_service_unavailable,
 #elem{name = <<"service-unavailable">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'service-unavailable'}}.
{error_subscription_required,
 #elem{name = <<"subscription-required">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'subscription-required'}}.
{error_undefined_condition,
 #elem{name = <<"undefined-condition">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'undefined-condition'}}.
{error_unexpected_request,
 #elem{name = <<"unexpected-request">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       result = 'unexpected-request'}}.

{error_text,
 #elem{name = <<"text">>,
       result = {'$lang', '$cdata'},
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
       attrs = [#attr{name = <<"xml:lang">>,
                      label = '$lang'}]}}.

{error,
 #elem{name = <<"error">>,
       xmlns = <<"jabber:client">>,
       result = {error, '$type', '$by', '$reason', '$text'},
       attrs = [#attr{name = <<"type">>,
                      label = '$type',
                      required = true,
                      dec = {dec_enum, [[auth, cancel, continue,
                                         modify, wait]]},
                      enc = {enc_enum, []}},
                #attr{name = <<"by">>}],
       refs = [#ref{name = error_text,
                    min = 0, max = 1, label = '$text'},
               #ref{name = error_bad_request,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_conflict,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_feature_not_implemented,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_forbidden,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_gone,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_internal_server_error,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_item_not_found,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_jid_malformed,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_not_acceptable,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_not_allowed,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_not_authorized,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_policy_violation,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_recipient_unavailable,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_redirect,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_registration_required,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_remote_server_not_found,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_remote_server_timeout,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_resource_constraint,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_service_unavailable,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_subscription_required,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_undefined_condition,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = error_unexpected_request,
                    min = 0, max = 1, label = '$reason'}]}}.

{bind_jid,
 #elem{name = <<"jid">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-bind">>,
       result = '$cdata',
       cdata = #cdata{dec = {dec_jid, []},
                      enc = {enc_jid, []}}}}.

{bind_resource,
 #elem{name = <<"resource">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-bind">>,
       result = '$cdata',
       cdata = #cdata{dec = {resourceprep, []},
                      enc = {resourceprep, []}}}}.

{bind, #elem{name = <<"bind">>,
             xmlns = <<"urn:ietf:params:xml:ns:xmpp-bind">>,
             result = {bind, '$jid', '$resource'},
             refs = [#ref{name = bind_jid,
                          label = '$jid',
                          min = 0, max = 1},
                     #ref{name = bind_resource,
                          min = 0, max = 1,
                          label = '$resource'}]}}.

{sasl_auth,
 #elem{name = <<"auth">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
       cdata = #cdata{label = '$text',
                      dec = {base64, decode, []},
                      enc = {base64, encode, []}},
       result = {sasl_auth, '$mechanism', '$text'},
       attrs = [#attr{name = <<"mechanism">>,
                      required = true}]}}.

{sasl_abort,
 #elem{name = <<"abort">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
       result = {sasl_abort}}}.

{sasl_challenge,
 #elem{name = <<"challenge">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
       cdata = #cdata{label = '$text',
                      dec = {base64, decode, []},
                      enc = {base64, encode, []}},
       result = {sasl_challenge, '$text'}}}.

{sasl_response,
 #elem{name = <<"response">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
       cdata = #cdata{label = '$text',
                      dec = {base64, decode, []},
                      enc = {base64, encode, []}},
       result = {sasl_response, '$text'}}}.

{sasl_success,
 #elem{name = <<"success">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
       cdata = #cdata{label = '$text',
                      dec = {base64, decode, []},
                      enc = {base64, encode, []}},
       result = {sasl_success, '$text'}}}.

{sasl_failure_text,
 #elem{name = <<"text">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
       result = {'$lang', '$cdata'},
       attrs = [#attr{name = <<"xml:lang">>,
                      label = '$lang'}]}}.

{sasl_failure_aborted,
 #elem{name = <<"aborted">>,
       result = 'aborted',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}}.
{sasl_failure_account_disabled,
 #elem{name = <<"account-disabled">>,
       result = 'account-disabled',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}}.
{sasl_failure_credentials_expired,
 #elem{name = <<"credentials-expired">>,
       result = 'credentials-expired',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}}.
{sasl_failure_encryption_required,
 #elem{name = <<"encryption-required">>,
       result = 'encryption-required',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}}.
{sasl_failure_incorrect_encoding,
 #elem{name = <<"incorrect-encoding">>,
       result = 'incorrect-encoding',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}}.
{sasl_failure_invalid_authzid,
 #elem{name = <<"invalid-authzid">>,
       result = 'invalid-authzid',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}}.
{sasl_failure_invalid_mechanism,
 #elem{name = <<"invalid-mechanism">>,
       result = 'invalid-mechanism',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}}.
{sasl_failure_malformed_request,
 #elem{name = <<"malformed-request">>,
       result = 'malformed-request',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}}.
{sasl_failure_mechanism_too_weak,
 #elem{name = <<"mechanism-too-weak">>,
       result = 'mechanism-too-weak',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}}.
{sasl_failure_not_authorized,
 #elem{name = <<"not-authorized">>,
       result = 'not-authorized',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}}.
{sasl_failure_temporary_auth_failure,
 #elem{name = <<"temporary-auth-failure">>,
       result = 'temporary-auth-failure',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}}.

{sasl_failure,
 #elem{name = <<"failure">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
       result = {sasl_failure, '$reason', '$text'},
       refs = [#ref{name = sasl_failure_text,
                    label = '$text'},
               #ref{name = sasl_failure_aborted,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = sasl_failure_account_disabled,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = sasl_failure_credentials_expired,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = sasl_failure_encryption_required,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = sasl_failure_incorrect_encoding,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = sasl_failure_invalid_authzid,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = sasl_failure_invalid_mechanism,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = sasl_failure_malformed_request,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = sasl_failure_mechanism_too_weak,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = sasl_failure_not_authorized,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = sasl_failure_temporary_auth_failure,
                    min = 0, max = 1, label = '$reason'}]}}.

{sasl_mechanism,
 #elem{name = <<"mechanism">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
       result = '$cdata'}}.

{sasl_mechanisms,
 #elem{name = <<"mechanisms">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
       result = {sasl_mechanisms, '$list'},
       refs = [#ref{name = sasl_mechanism,
                    label = '$list'}]}}.

{starttls_required,
 #elem{name = <<"required">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-tls">>,
       result = true}}.

{starttls,
 #elem{name = <<"starttls">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-tls">>,
       result = {starttls, '$required'},
       refs = [#ref{name = starttls_required,
                    label = '$required',
                    min = 0, max = 1,
                    default = false}]}}.

{starttls_proceed,
 #elem{name = <<"proceed">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-tls">>,
       result = {starttls_proceed}}}.

{starttls_failure,
 #elem{name = <<"failure">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-tls">>,
       result = {starttls_failure}}}.

{stream_features,
 #elem{name = <<"stream:features">>,
       xmlns = <<"http://etherx.jabber.org/streams">>,
       result = {stream_features, '$_els'}}}.

{p1_push,
 #elem{name = <<"push">>,
       result = {p1_push},
       xmlns = <<"p1:push">>}}.

{p1_rebind,
 #elem{name = <<"rebind">>,
       result = {p1_rebind},
       xmlns = <<"p1:rebind">>}}.

{p1_ack,
 #elem{name = <<"ack">>,
       result = {p1_ack},
       xmlns = <<"p1:ack">>}}.

{caps,
 #elem{name = <<"c">>,
       xmlns = <<"http://jabber.org/protocol/caps">>,
       result = {caps, '$hash', '$node', '$ver'},
       attrs = [#attr{name = <<"hash">>},
                #attr{name = <<"node">>},
                #attr{name = <<"ver">>,
                      enc = {base64, encode, []},
                      dec = {base64, decode, []}}]}}.

{register_feature,
 #elem{name = <<"register">>,
       xmlns = <<"http://jabber.org/features/iq-register">>,
       result = {register_feature}}}.

{session,
 #elem{name = <<"session">>,
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-session">>,
       result = {session}}}.

{ping,
 #elem{name = <<"ping">>,
       xmlns = <<"urn:xmpp:ping">>,
       result = {ping}}}.

{time_utc,
 #elem{name = <<"utc">>,
       xmlns = <<"urn:xmpp:time">>,
       result = '$cdata',
       cdata = #cdata{dec = {dec_utc, []},
                      enc = {enc_utc, []}}}}.

{time_tzo,
 #elem{name = <<"tzo">>,
       xmlns = <<"urn:xmpp:time">>,
       result = '$cdata',
       cdata = #cdata{dec = {dec_tzo, []},
                      enc = {enc_tzo, []}}}}.

{time,
 #elem{name = <<"time">>,
       xmlns = <<"urn:xmpp:time">>,
       result = {time, '$tzo', '$utc'},
       refs = [#ref{name = time_tzo,
                    label = '$tzo',
                    min = 0, max = 1},
               #ref{name = time_utc,
                    label = '$utc',
                    min = 0, max = 1}]}}.

{stream_error_text,
 #elem{name = <<"text">>,
       result = {'$lang', '$text'},
       cdata = #cdata{label = '$text'},
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>,
       attrs = [#attr{name = <<"xml:lang">>,
                      label = '$lang'}]}}.

{stream_error_bad_format,
 #elem{name = <<"bad-format">>,
       result = 'bad-format',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.       
{stream_error_bad_namespace_prefix,
 #elem{name = <<"bad-namespace-prefix">>,
       result = 'bad-namespace-prefix',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_conflict,
 #elem{name = <<"conflict">>,
       result = 'conflict',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_connection_timeout,
 #elem{name = <<"connection-timeout">>,
       result = 'connection-timeout',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_host_gone,
 #elem{name = <<"host-gone">>,
       result = 'host-gone',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_host_unknown,
 #elem{name = <<"host-unknown">>,
       result = 'host-unknown',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_improper_addressing,
 #elem{name = <<"improper-addressing">>,
       result = 'improper-addressing',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_internal_server_error,
 #elem{name = <<"internal-server-error">>,
       result = 'internal-server-error',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_invalid_from,
 #elem{name = <<"invalid-from">>,
       result = 'invalid-from',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_invalid_id,
 #elem{name = <<"invalid-id">>,
       result = 'invalid-id',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_invalid_namespace,
 #elem{name = <<"invalid-namespace">>,
       result = 'invalid-namespace',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_invalid_xml,
 #elem{name = <<"invalid-xml">>,
       result = 'invalid-xml',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_not_authorized,
 #elem{name = <<"not-authorized">>,
       result = 'not-authorized',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_not_well_formed,
 #elem{name = <<"not-well-formed">>,
       result = 'not-well-formed',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_policy_violation,
 #elem{name = <<"policy-violation">>,
       result = 'policy-violation',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_remote_connection_failed,
 #elem{name = <<"remote-connection-failed">>,
       result = 'remote-connection-failed',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_reset,
 #elem{name = <<"reset">>,
       result = 'reset',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_resource_constraint,
 #elem{name = <<"resource-constraint">>,
       result = 'resource-constraint',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_restricted_xml,
 #elem{name = <<"restricted-xml">>,
       result = 'restricted-xml',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_see_other_host,
 #elem{name = <<"see-other-host">>,
       cdata = #cdata{required = true, label = '$host'},
       result = {'see-other-host', '$host'},
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_system_shutdown,
 #elem{name = <<"system-shutdown">>,
       result = 'system-shutdown',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_undefined_condition,
 #elem{name = <<"undefined-condition">>,
       result = 'undefined-condition',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_unsupported_encoding,
 #elem{name = <<"unsupported-encoding">>,
       result = 'unsupported-encoding',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_unsupported_stanza_type,
 #elem{name = <<"unsupported-stanza-type">>,
       result = 'unsupported-stanza-type',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.
{stream_error_unsupported_version,
 #elem{name = <<"unsupported-version">>,
       result = 'unsupported-version',
       xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}}.

{stream_error,
 #elem{name = <<"stream:error">>,
       xmlns = <<"http://etherx.jabber.org/streams">>,
       result = {stream_error, '$reason', '$text'},
       refs = [#ref{name = stream_error_text,
                    label = '$text',
                    min = 0, max = 1},
               #ref{name = stream_error_bad_format,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_bad_namespace_prefix,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_conflict,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_connection_timeout,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_host_gone,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_host_unknown,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_improper_addressing,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_internal_server_error,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_invalid_from,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_invalid_id,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_invalid_namespace,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_invalid_xml,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_not_authorized,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_not_well_formed,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_policy_violation,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_remote_connection_failed,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_reset,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_resource_constraint,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_restricted_xml,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_see_other_host,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_system_shutdown,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_undefined_condition,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_unsupported_encoding,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_unsupported_stanza_type,
                    min = 0, max = 1, label = '$reason'},
               #ref{name = stream_error_unsupported_version,
                    min = 0, max = 1, label = '$reason'}
              ]}}.

{vcard_HOME, #elem{name = <<"HOME">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_WORK, #elem{name = <<"WORK">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_VOICE, #elem{name = <<"VOICE">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_FAX, #elem{name = <<"FAX">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_PAGER, #elem{name = <<"PAGER">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_MSG, #elem{name = <<"MSG">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_CELL, #elem{name = <<"CELL">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_VIDEO, #elem{name = <<"VIDEO">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_BBS, #elem{name = <<"BBS">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_MODEM, #elem{name = <<"MODEM">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_ISDN, #elem{name = <<"ISDN">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_PCS, #elem{name = <<"PCS">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_POSTAL, #elem{name = <<"POSTAL">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_PARCEL, #elem{name = <<"PARCEL">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_DOM, #elem{name = <<"DOM">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_INTL, #elem{name = <<"INTL">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_PREF, #elem{name = <<"PREF">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_INTERNET, #elem{name = <<"INTERNET">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_X400, #elem{name = <<"X400">>, xmlns = <<"vcard-temp">>, result = true}}.
{vcard_FAMILY, #elem{name = <<"FAMILY">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_GIVEN, #elem{name = <<"GIVEN">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_MIDDLE, #elem{name = <<"MIDDLE">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_PREFIX, #elem{name = <<"PREFIX">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_SUFFIX, #elem{name = <<"SUFFIX">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_POBOX, #elem{name = <<"POBOX">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_EXTADD, #elem{name = <<"EXTADD">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_STREET, #elem{name = <<"STREET">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_LOCALITY, #elem{name = <<"LOCALITY">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_REGION, #elem{name = <<"REGION">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_PCODE, #elem{name = <<"PCODE">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_CTRY, #elem{name = <<"CTRY">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_LINE, #elem{name = <<"LINE">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_NUMBER, #elem{name = <<"NUMBER">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_USERID, #elem{name = <<"USERID">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_LAT, #elem{name = <<"LAT">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_LON, #elem{name = <<"LON">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_ORGNAME, #elem{name = <<"ORGNAME">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_ORGUNIT, #elem{name = <<"ORGUNIT">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_PHONETIC, #elem{name = <<"PHONETIC">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_CRED, #elem{name = <<"CRED">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_VERSION, #elem{name = <<"VERSION">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_FN, #elem{name = <<"FN">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_NICKNAME, #elem{name = <<"NICKNAME">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_BDAY, #elem{name = <<"BDAY">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_JABBERID, #elem{name = <<"JABBERID">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_MAILER, #elem{name = <<"MAILER">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_TZ, #elem{name = <<"TZ">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_TITLE, #elem{name = <<"TITLE">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_ROLE, #elem{name = <<"ROLE">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_KEYWORD, #elem{name = <<"KEYWORD">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_NOTE, #elem{name = <<"NOTE">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_PRODID, #elem{name = <<"PRODID">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_REV, #elem{name = <<"REV">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_SORT_STRING, #elem{name = <<"SORT-STRING">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_UID, #elem{name = <<"UID">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_URL, #elem{name = <<"URL">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_DESC, #elem{name = <<"DESC">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_TYPE, #elem{name = <<"TYPE">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_EXTVAL, #elem{name = <<"EXTVAL">>, xmlns = <<"vcard-temp">>, result = '$cdata'}}.
{vcard_PUBLIC, #elem{name = <<"PUBLIC">>, xmlns = <<"vcard-temp">>, result = public}}.
{vcard_PRIVATE, #elem{name = <<"PRIVATE">>, xmlns = <<"vcard-temp">>, result = private}}.
{vcard_CONFIDENTIAL, #elem{name = <<"CONFIDENTIAL">>, xmlns = <<"vcard-temp">>, result = confidential}}.

{vcard_N,
 #elem{name = <<"N">>,
       xmlns = <<"vcard-temp">>,
       result = {vcard_name, '$family', '$given', '$middle',
                 '$prefix', '$suffix'},
       refs = [#ref{name = vcard_FAMILY, min = 0, max = 1, label = '$family'},
               #ref{name = vcard_GIVEN, min = 0, max = 1, label = '$given'},
               #ref{name = vcard_MIDDLE, min = 0, max = 1, label = '$middle'},
               #ref{name = vcard_PREFIX, min = 0, max = 1, label = '$prefix'},
               #ref{name = vcard_SUFFIX, min = 0, max = 1, label = '$suffix'}]}}.

{vcard_ADR,
 #elem{name = <<"ADR">>,
       xmlns = <<"vcard-temp">>,
       result = {vcard_adr, '$home', '$work', '$postal', '$parcel',
                '$dom', '$intl', '$pref', '$pobox', '$extadd', '$street',
                '$locality', '$region', '$pcode', '$ctry'},
       refs = [#ref{name = vcard_HOME, default = false,
                    min = 0, max = 1, label = '$home'},
               #ref{name = vcard_WORK, default = false,
                    min = 0, max = 1, label = '$work'},
               #ref{name = vcard_POSTAL, default = false,
                    min = 0, max = 1, label = '$postal'},
               #ref{name = vcard_PARCEL, default = false,
                    min = 0, max = 1, label = '$parcel'},
               #ref{name = vcard_DOM, default = false,
                    min = 0, max = 1, label = '$dom'},
               #ref{name = vcard_INTL, default = false,
                    min = 0, max = 1, label = '$intl'},
               #ref{name = vcard_PREF, default = false,
                    min = 0, max = 1, label = '$pref'},
               #ref{name = vcard_POBOX, min = 0, max = 1, label = '$pobox'},
               #ref{name = vcard_EXTADD, min = 0, max = 1, label = '$extadd'},
               #ref{name = vcard_STREET, min = 0, max = 1, label = '$street'},
               #ref{name = vcard_LOCALITY, min = 0, max = 1, label = '$locality'},
               #ref{name = vcard_REGION, min = 0, max = 1, label = '$region'},
               #ref{name = vcard_PCODE, min = 0, max = 1, label = '$pcode'},
               #ref{name = vcard_CTRY, min = 0, max = 1, label = '$ctry'}]}}.

{vcard_LABEL,
 #elem{name = <<"LABEL">>,
       xmlns = <<"vcard-temp">>,
       result = {vcard_label, '$home', '$work', '$postal', '$parcel',
                '$dom', '$intl', '$pref', '$line'},
       refs = [#ref{name = vcard_HOME, default = false,
                    min = 0, max = 1, label = '$home'},
               #ref{name = vcard_WORK, default = false,
                    min = 0, max = 1, label = '$work'},
               #ref{name = vcard_POSTAL, default = false,
                    min = 0, max = 1, label = '$postal'},
               #ref{name = vcard_PARCEL, default = false,
                    min = 0, max = 1, label = '$parcel'},
               #ref{name = vcard_DOM, default = false,
                    min = 0, max = 1, label = '$dom'},
               #ref{name = vcard_INTL, default = false,
                    min = 0, max = 1, label = '$intl'},
               #ref{name = vcard_PREF, default = false,
                    min = 0, max = 1, label = '$pref'},
               #ref{name = vcard_LINE, label = '$line'}]}}.

{vcard_TEL,
 #elem{name = <<"TEL">>,
       xmlns = <<"vcard-temp">>,
       result = {vcard_tel, '$home', '$work', '$voice', '$fax',
                 '$pager', '$msg', '$cell', '$video', '$bbs',
                 '$modem', '$isdn', '$pcs', '$pref', '$number'},
       refs = [#ref{name = vcard_HOME, default = false,
                    min = 0, max = 1, label = '$home'},
               #ref{name = vcard_WORK, default = false,
                    min = 0, max = 1, label = '$work'},
               #ref{name = vcard_VOICE, default = false,
                    min = 0, max = 1, label = '$voice'},
               #ref{name = vcard_FAX, default = false,
                    min = 0, max = 1, label = '$fax'},
               #ref{name = vcard_PAGER, default = false,
                    min = 0, max = 1, label = '$pager'},
               #ref{name = vcard_MSG, default = false,
                    min = 0, max = 1, label = '$msg'},
               #ref{name = vcard_CELL, default = false,
                    min = 0, max = 1, label = '$cell'},
               #ref{name = vcard_VIDEO, default = false,
                    min = 0, max = 1, label = '$video'},
               #ref{name = vcard_BBS, default = false,
                    min = 0, max = 1, label = '$bbs'},
               #ref{name = vcard_MODEM, default = false,
                    min = 0, max = 1, label = '$modem'},
               #ref{name = vcard_ISDN, default = false,
                    min = 0, max = 1, label = '$isdn'},
               #ref{name = vcard_PCS, default = false,
                    min = 0, max = 1, label = '$pcs'},
               #ref{name = vcard_PREF, default = false,
                    min = 0, max = 1, label = '$pref'},
               #ref{name = vcard_NUMBER,
                    min = 1, max = 1, label = '$number'}]}}.

{vcard_EMAIL,
 #elem{name = <<"EMAIL">>,
       xmlns = <<"vcard-temp">>,
       result = {vcard_email, '$home', '$work',
                '$internet', '$pref', '$x400', '$userid'},
       refs = [#ref{name = vcard_HOME, default = false,
                    min = 0, max = 1, label = '$home'},
               #ref{name = vcard_WORK, default = false,
                    min = 0, max = 1, label = '$work'},
               #ref{name = vcard_INTERNET, default = false,
                    min = 0, max = 1, label = '$internet'},
               #ref{name = vcard_PREF, default = false,
                    min = 0, max = 1, label = '$pref'},
               #ref{name = vcard_X400, default = false,
                    min = 0, max = 1, label = '$x400'},
               #ref{name = vcard_USERID,
                    min = 1, max = 1, label = '$userid'}]}}.

{vcard_GEO,
 #elem{name = <<"GEO">>,
       xmlns = <<"vcard-temp">>,
       result = {vcard_geo, '$lat', '$lon'},
       refs = [#ref{name = vcard_LAT, min = 1, max = 1, label = '$lat'},
               #ref{name = vcard_LON, min = 1, max = 1, label = '$lon'}]}}.

{vcard_BINVAL,
 #elem{name = <<"BINVAL">>,
       xmlns = <<"vcard-temp">>,
       cdata = #cdata{dec = {base64, decode, []},
                      enc = {base64, encode, []}},
       result = '$cdata'}}.

{vcard_LOGO,
 #elem{name = <<"LOGO">>,
       xmlns = <<"vcard-temp">>,
       result = {vcard_logo, '$type', '$binval', '$extval'},
       refs = [#ref{name = vcard_TYPE, min = 0, max = 1, label = '$type'},
               #ref{name = vcard_BINVAL, min = 0, max = 1, label = '$binval'},
               #ref{name = vcard_EXTVAL, min = 0, max = 1, label = '$extval'}]}}.

{vcard_PHOTO,
 #elem{name = <<"PHOTO">>,
       xmlns = <<"vcard-temp">>,
       result = {vcard_photo, '$type', '$binval', '$extval'},
       refs = [#ref{name = vcard_TYPE, min = 0, max = 1, label = '$type'},
               #ref{name = vcard_BINVAL, min = 0, max = 1, label = '$binval'},
               #ref{name = vcard_EXTVAL, min = 0, max = 1, label = '$extval'}]}}.

{vcard_ORG,
 #elem{name = <<"ORG">>,
       xmlns = <<"vcard-temp">>,
       result = {vcard_org, '$name', '$units'},
       refs = [#ref{name = vcard_ORGNAME,
                    label = '$name',
                    min = 1, max = 1},
               #ref{name = vcard_ORGUNIT,
                    label = '$units'}]}}.

{vcard_SOUND,
 #elem{name = <<"SOUND">>,
       xmlns = <<"vcard-temp">>,
       result = {vcard_sound, '$phonetic', '$binval', '$extval'},
       refs = [#ref{name = vcard_BINVAL, min = 0, max = 1, label = '$binval'},
               #ref{name = vcard_EXTVAL, min = 0, max = 1, label = '$extval'},
               #ref{name = vcard_PHONETIC, min = 0, max = 1, label = '$phonetic'}]}}.

{vcard_KEY,
 #elem{name = <<"KEY">>,
       xmlns = <<"vcard-temp">>,
       result = {vcard_key, '$type', '$cred'},
       refs = [#ref{name = vcard_TYPE, min = 0, max = 1, label = '$type'},
               #ref{name = vcard_CRED, min = 1, max = 1, label = '$cred'}]}}.

{vcard_CATEGORIES,
 #elem{name = <<"CATEGORIES">>,
       xmlns = <<"vcard-temp">>,
       result = '$keywords',
       refs = [#ref{name = vcard_KEYWORD, label = '$keywords'}]}}.

{vcard_CLASS,
 #elem{name = <<"CLASS">>,
       xmlns = <<"vcard-temp">>,
       result = '$class',
       refs = [#ref{name = vcard_PUBLIC, min = 0, max = 1, label = '$class'},
               #ref{name = vcard_PRIVATE, min = 0, max = 1, label = '$class'},
               #ref{name = vcard_CONFIDENTIAL, min = 0, max = 1, label = '$class'}]}}.

{vcard_AGENT,
 #elem{name = <<"AGENT">>,
       xmlns = <<"vcard-temp">>,
       result = {vcard_agent, '$vcard', '$extval'},
       refs = [#ref{name = vcard, min = 0, max = 1, label = '$vcard'},
               #ref{name = vcard_EXTVAL, min = 0, max = 1, label = '$extval'}]}}.

{vcard,
 #elem{name = <<"vCard">>,
       xmlns = <<"vcard-temp">>,
       result = {vcard, '$version', '$fn', '$n', '$nickname', '$photo',
                 '$bday', '$adr', '$label', '$tel', '$email', '$jabberid',
                 '$mailer', '$tz', '$geo', '$title', '$role', '$logo',
                 '$org', '$categories', '$note', '$prodid', '$agent',
                 '$rev', '$sort_string', '$sound', '$uid', '$url', '$class',
                 '$key', '$desc'},
       refs = [#ref{name = vcard_N, min = 0, max = 1, label = '$n'},
               #ref{name = vcard_ADR, label = '$adr'},
               #ref{name = vcard_LABEL, label = '$label'},
               #ref{name = vcard_TEL, label = '$tel'},
               #ref{name = vcard_EMAIL, label = '$email'},
               #ref{name = vcard_GEO, min = 0, max = 1, label = '$geo'},
               #ref{name = vcard_LOGO, min = 0, max = 1, label = '$logo'},
               #ref{name = vcard_PHOTO, min = 0, max = 1, label = '$photo'},
               #ref{name = vcard_ORG, min = 0, max = 1, label = '$org'},
               #ref{name = vcard_SOUND, min = 0, max = 1, label = '$sound'},
               #ref{name = vcard_KEY, min = 0, max = 1, label = '$key'},
               #ref{name = vcard_VERSION, min = 0, max = 1, label = '$version'},
               #ref{name = vcard_FN, min = 0, max = 1, label = '$fn'},
               #ref{name = vcard_NICKNAME, min = 0, max = 1, label = '$nickname'},
               #ref{name = vcard_BDAY, min = 0, max = 1, label = '$bday'},
               #ref{name = vcard_JABBERID, min = 0, max = 1, label = '$jabberid'},
               #ref{name = vcard_MAILER, min = 0, max = 1, label = '$mailer'},
               #ref{name = vcard_TZ, min = 0, max = 1, label = '$tz'},
               #ref{name = vcard_TITLE, min = 0, max = 1, label = '$title'},
               #ref{name = vcard_ROLE, min = 0, max = 1, label = '$role'},
               #ref{name = vcard_NOTE, min = 0, max = 1, label = '$note'},
               #ref{name = vcard_PRODID, min = 0, max = 1, label = '$prodid'},
               #ref{name = vcard_REV, min = 0, max = 1, label = '$rev'},
               #ref{name = vcard_AGENT, min = 0, max = 1, label = '$agent'},
               #ref{name = vcard_SORT_STRING, min = 0, max = 1,
                    label = '$sort_string'},
               #ref{name = vcard_UID, min = 0, max = 1, label = '$uid'},
               #ref{name = vcard_URL, min = 0, max = 1, label = '$url'},
               #ref{name = vcard_DESC, min = 0, max = 1, label = '$desc'},
               #ref{name = vcard_CATEGORIES, default = [], min = 0, max = 1,
                    label = '$categories'},
               #ref{name = vcard_CLASS, min = 0, max = 1, label = '$class'}]}}.

{xdata_field_required,
 #elem{name = <<"required">>,
       xmlns = <<"jabber:x:data">>,
       result = true}}.

{xdata_field_desc,
 #elem{name = <<"desc">>, xmlns = <<"jabber:x:data">>, result = '$cdata'}}.

{xdata_field_value,
 #elem{name = <<"value">>, xmlns = <<"jabber:x:data">>, result = '$cdata'}}.

{xdata_field_option,
 #elem{name = <<"option">>,
       xmlns = <<"jabber:x:data">>,
       result = '$value',
       refs = [#ref{name = xdata_field_value,
                    label = '$value',
                    min = 1, max = 1}]}}.

{xdata_field,
 #elem{name = <<"field">>,
       xmlns = <<"jabber:x:data">>,
       result = {xdata_field, '$label', '$type', '$var',
                 '$required', '$desc', '$values', '$options'},
       attrs = [#attr{name = <<"label">>},
                #attr{name = <<"type">>,
                      enc = {enc_enum, []},
                      dec = {dec_enum, [['boolean',
                                         'fixed',
                                         'hidden',
                                         'jid-multi',
                                         'jid-single',
                                         'list-multi',
                                         'list-single',
                                         'text-multi',
                                         'text-private',
                                         'text-single']]}},
                #attr{name = <<"var">>}],
       refs = [#ref{name = xdata_field_required,
                    label = '$required',
                    default = false,
                    min = 0, max = 1},
               #ref{name = xdata_field_desc,
                    label = '$desc',
                    min = 0, max = 1},
               #ref{name = xdata_field_value,
                    label = '$values'},
               #ref{name = xdata_field_option,
                    label = '$options'}]}}.

{xdata_instructions,  #elem{name = <<"instructions">>,
                            xmlns = <<"jabber:x:data">>,
                            result = '$cdata'}}.
{xdata_title, #elem{name = <<"title">>,
                    xmlns = <<"jabber:x:data">>,
                    result = '$cdata'}}.
{xdata_reported, #elem{name = <<"reported">>,
                       xmlns = <<"jabber:x:data">>,
                       result = '$fields',
                       refs = [#ref{name = xdata_field,
                                    label = '$fields'}]}}.
{xdata_item,  #elem{name = <<"item">>,
                    xmlns = <<"jabber:x:data">>,
                    result = '$fields',
                    refs = [#ref{name = xdata_field,
                                 label = '$fields'}]}}.

{xdata,
 #elem{name = <<"x">>,
       xmlns = <<"jabber:x:data">>,
       result = {xdata, '$type', '$instructions', '$title',
                 '$reported', '$items', '$fields'},
       attrs = [#attr{name = <<"type">>,
                      required = true,
                      dec = {dec_enum, [[cancel, form, result, submit]]},
                      enc = {enc_enum, []}}],
       refs = [#ref{name = xdata_instructions,
                    label = '$instructions'},
               #ref{name = xdata_title,
                    label = '$title',
                    min = 0, max = 1},
               #ref{name = xdata_reported,
                    label = '$reported',
                    min = 0, max = 1},
               #ref{name = xdata_item,
                    label = '$items'},
               #ref{name = xdata_field,
                    label = '$fields'}]}}.

{pubsub_subscription,
 #elem{name = <<"subscription">>,
       xmlns = <<"http://jabber.org/protocol/pubsub">>,
       result = {pubsub_subscription, '$jid', '$node', '$subid',
                 '$type'},
       attrs = [#attr{name = <<"jid">>,
                      required = true,
                      dec = {dec_jid, []},
                      enc = {enc_jid, []}},
                #attr{name = <<"node">>},
                #attr{name = <<"subid">>},
                #attr{name = <<"subscription">>,
                      label = '$type',
                      dec = {dec_enum, [[none, pending, subscribed,
                                         unconfigured]]},
                      enc = {enc_enum, []}}]}}.

{pubsub_affiliation,
 #elem{name = <<"affiliation">>,
       xmlns = <<"http://jabber.org/protocol/pubsub">>,
       result = {pubsub_affiliation, '$node', '$type'},
       attrs = [#attr{name = <<"node">>,
                      required = true},
                #attr{name = <<"affiliation">>,
                      label = '$type',
                      required = true,
                      dec = {dec_enum, [[member, none, outcast, owner,
                                         publisher, 'publish-only']]},
                      enc = {enc_enum, []}}]}}.

{pubsub_item,
 #elem{name = <<"item">>,
       xmlns = <<"http://jabber.org/protocol/pubsub">>,
       result = {pubsub_item, '$id', '$_els'},
       attrs = [#attr{name = <<"id">>}]}}.

{pubsub_items,
 #elem{name = <<"items">>,
       xmlns = <<"http://jabber.org/protocol/pubsub">>,
       result = {pubsub_items, '$node', '$max_items',
                 '$subid', '$items'},
       attrs = [#attr{name = <<"max_items">>,
                      dec = {dec_int, [0, infinity]},
                      enc = {enc_int, []}},
                #attr{name = <<"node">>,
                      required = true},
                #attr{name = <<"subid">>}],
       refs = [#ref{name = pubsub_item, label = '$items'}]}}.

{pubsub_event_retract,
 #elem{name = <<"retract">>,
       xmlns = <<"http://jabber.org/protocol/pubsub#event">>,
       result = '$id',
       attrs = [#attr{name = <<"id">>, required = true}]}}.

{pubsub_event_item,
 #elem{name = <<"item">>,
       xmlns = <<"http://jabber.org/protocol/pubsub#event">>,
       result = {pubsub_event_item, '$id', '$node', '$publisher'},
       attrs = [#attr{name = <<"id">>},
                #attr{name = <<"node">>},
                #attr{name = <<"publisher">>}]}}.

{pubsub_event_items,
 #elem{name = <<"items">>,
       xmlns = <<"http://jabber.org/protocol/pubsub#event">>,
       result = {pubsub_event_items, '$node', '$retract', '$items'},
       attrs = [#attr{name = <<"node">>,
                      required = true}],
       refs = [#ref{name = pubsub_event_retract, label = '$retract'},
               #ref{name = pubsub_event_item, label = '$items'}]}}.

{pubsub_event,
 #elem{name = <<"event">>,
       xmlns = <<"http://jabber.org/protocol/pubsub#event">>,
       result = {pubsub_event, '$items'},
       refs = [#ref{name = pubsub_event_items, label = '$items'}]}}.

{pubsub_subscriptions,
 #elem{name = <<"subscriptions">>,
       xmlns = <<"http://jabber.org/protocol/pubsub">>,
       result = {'$node', '$subscriptions'},
       attrs = [#attr{name = <<"node">>,
                      default = none}],
       refs = [#ref{name = pubsub_subscription, label = '$subscriptions'}]}}.

{pubsub_affiliations,
 #elem{name = <<"affiliations">>,
       xmlns = <<"http://jabber.org/protocol/pubsub">>,
       result = '$affiliations',
       refs = [#ref{name = pubsub_affiliation, label = '$affiliations'}]}}.

{pubsub_subscribe,
 #elem{name = <<"subscribe">>,
       xmlns = <<"http://jabber.org/protocol/pubsub">>,
       result = {'$node', '$jid'},
       attrs = [#attr{name = <<"node">>},
                #attr{name = <<"jid">>,
                      required = true,
                      dec = {dec_jid, []},
                      enc = {enc_jid, []}}]}}.

{pubsub_publish,
 #elem{name = <<"publish">>,
       xmlns = <<"http://jabber.org/protocol/pubsub">>,
       result = {'$node', '$items'},
       attrs = [#attr{name = <<"node">>,
                      required = true}],
       refs = [#ref{name = pubsub_item, label = '$items'}]}}.

{pubsub,
 #elem{name = <<"pubsub">>,
       xmlns = <<"http://jabber.org/protocol/pubsub">>,
       result = {pubsub, '$subscriptions', '$affiliations', '$publish',
                 '$subscribe'},
       refs = [#ref{name = pubsub_subscriptions, label = '$subscriptions',
                    min = 0, max = 1},
               #ref{name = pubsub_affiliations, label = '$affiliations',
                    min = 0, max = 1},
               #ref{name = pubsub_subscribe, label = '$subscribe',
                    min = 0, max = 1},
               #ref{name = pubsub_publish, label = '$publish',
                    min = 0, max = 1}]}}.

{delay,
 #elem{name = <<"delay">>,
       xmlns = <<"urn:xmpp:delay">>,
       result = {delay, '$stamp', '$from'},
       attrs = [#attr{name = <<"stamp">>,
                      required = true,
                      dec = {dec_utc, []},
                      enc = {enc_utc, []}},
                #attr{name = <<"from">>,
                      dec = {dec_jid, []},
                      enc = {enc_jid, []}}]}}.

dec_tzo(Val) ->
    [H1, M1] = str:tokens(Val, <<":">>),
    H = erlang:binary_to_integer(H1),
    M = erlang:binary_to_integer(M1),
    if H >= -12, H =< 12, M >= 0, M < 60  ->
            {H, M}
    end.

enc_tzo({H, M}) ->
    Sign = if H >= 0 ->
                   <<>>;
              true ->
                   <<"-">>
           end,
    list_to_binary([Sign, io_lib:format("~2..0w:~2..0w", [H, M])]).

dec_utc(Val) ->
    {_, _, _} = jlib:datetime_string_to_timestamp(Val).

enc_utc(Val) ->
    jlib:now_to_utc_string(Val).

dec_jid(Val) ->
    case jlib:string_to_jid(Val) of
        error ->
            erlang:error(badarg);
        J ->
            J
    end.

enc_jid(J) ->            
    jlib:jid_to_string(J).

resourceprep(R) ->
    case jlib:resourceprep(R) of
        error ->
            erlang:error(badarg);
        R1 ->
            R1
    end.

dec_bool(<<"false">>) -> false;
dec_bool(<<"true">>) -> true.

enc_bool(false) -> <<"false">>;
enc_bool(true) -> <<"true">>.

%% Local Variables:
%% mode: erlang
%% End:
%% vim: set filetype=erlang tabstop=8:
