-xml(last,
     #elem{name = <<"query">>,
           xmlns = <<"jabber:iq:last">>,
           result = {last, '$seconds', '$text'},
           attrs = [#attr{name = <<"seconds">>,
                          default = undefined,
                          enc = {enc_int, []},
                          dec = {dec_int, [0, infinity]}}],
           cdata = #cdata{label = '$text'}}).

-xml(version_name,
     #elem{name = <<"name">>,
           xmlns = <<"jabber:iq:version">>,
           result = '$cdata',
           cdata = #cdata{label = '$cdata', required = true}}).

-xml(version_ver,
     #elem{name = <<"version">>,
           xmlns = <<"jabber:iq:version">>,
           result = '$cdata',
           cdata = #cdata{label = '$cdata', required = true}}).

-xml(version_os,
     #elem{name = <<"os">>,
           xmlns = <<"jabber:iq:version">>,
           result = '$cdata',
           cdata = #cdata{label = '$cdata', required = true}}).

-xml(version,
     #elem{name = <<"query">>,
           xmlns = <<"jabber:iq:version">>,
           result = {version, '$name', '$ver', '$os'},
           refs = [#ref{name = version_name,
                        label = '$name',
                        min = 0, max = 1},
                   #ref{name = version_ver,
                        label = '$ver',
                        min = 0, max = 1},
                   #ref{name = version_os,
                        label = '$os',
                        min = 0, max = 1}]}).

-xml(roster_group,
     #elem{name = <<"group">>,
           xmlns = <<"jabber:iq:roster">>,
           result = '$cdata',
           cdata = #cdata{required = true, label = '$cdata'}}).

-xml(roster_item,
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
           refs = [#ref{name = roster_group, label = '$groups'}]}).

-xml(roster,
     #elem{name = <<"query">>,
           xmlns = <<"jabber:iq:roster">>,
           result = {roster, '$items', '$ver'},
           attrs = [#attr{name = <<"ver">>}],
           refs = [#ref{name = roster_item, label = '$items'}]}).

-xml(privacy_message, #elem{name = <<"message">>, xmlns = <<"jabber:iq:privacy">>,
                            result = message}).
-xml(privacy_iq, #elem{name = <<"iq">>, xmlns = <<"jabber:iq:privacy">>,
                       result = iq}).
-xml(privacy_presence_in, #elem{name = <<"presence-in">>,
                                xmlns = <<"jabber:iq:privacy">>,
                                result = 'presence-in'}).
-xml(privacy_presence_out, #elem{name = <<"presence-out">>,
                                 xmlns = <<"jabber:iq:privacy">>,
                                 result = 'presence-out'}).

-xml(privacy_item,
     #elem{name = <<"item">>,
           xmlns = <<"jabber:iq:privacy">>,
           result = {privacy_item, '$order', '$action', '$type',
                     '$value', '$kinds'},
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
                        label = '$kinds'},
                   #ref{name = privacy_iq,
                        label = '$kinds'},
                   #ref{name = privacy_presence_in,
                        label = '$kinds'},
                   #ref{name = privacy_presence_out,
                        label = '$kinds'}]}).

-xml(privacy_list,
     #elem{name = <<"list">>,
           xmlns = <<"jabber:iq:privacy">>,
           result = {privacy_list, '$name', '$items'},
           attrs = [#attr{name = <<"name">>,
                          required = true}],
           refs = [#ref{name = privacy_item,
                        label = '$items'}]}).

-xml(privacy_default_list,
     #elem{name = <<"default">>,
           xmlns = <<"jabber:iq:privacy">>,
           result = '$name',
           attrs = [#attr{name = <<"name">>,
                          default = none}]}).

-xml(privacy_active_list,
     #elem{name = <<"active">>,
           xmlns = <<"jabber:iq:privacy">>,
           result = '$name',
           attrs = [#attr{name = <<"name">>,
                          default = none}]}).

-xml(privacy,
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
                        label = '$active'}]}).

-xml(block_item,
     #elem{name = <<"item">>,
           xmlns = <<"urn:xmpp:blocking">>,
           result = '$jid',
           attrs = [#attr{name = <<"jid">>,
                          required = true,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}}]}).

-xml(block,
     #elem{name = <<"block">>,
           xmlns = <<"urn:xmpp:blocking">>,
           result = {block, '$items'},
           refs = [#ref{name = block_item,
                        label = '$items'}]}).

-xml(unblock,
     #elem{name = <<"unblock">>,
           xmlns = <<"urn:xmpp:blocking">>,
           result = {unblock, '$items'},
           refs = [#ref{name = block_item,
                        label = '$items'}]}).

-xml(block_list,
     #elem{name = <<"blocklist">>,
           xmlns = <<"urn:xmpp:blocking">>,
           result = {block_list}}).

-xml(disco_identity,
     #elem{name = <<"identity">>,
           xmlns = <<"http://jabber.org/protocol/disco#info">>,
           result = {identity, '$category', '$type', '$lang', '$name'},
           attrs = [#attr{name = <<"category">>,
                          required = true},
                    #attr{name = <<"type">>,
                          required = true},
                    #attr{name = <<"xml:lang">>,
                          label = '$lang'},
                    #attr{name = <<"name">>}]}).

-xml(disco_feature,
     #elem{name = <<"feature">>,
           xmlns = <<"http://jabber.org/protocol/disco#info">>,
           result = '$var',
           attrs = [#attr{name = <<"var">>,
                          required = true}]}).

-xml(disco_info,
     #elem{name = <<"query">>,
           xmlns = <<"http://jabber.org/protocol/disco#info">>,
           result = {disco_info, '$node', '$identities', '$features', '$xdata'},
           attrs = [#attr{name = <<"node">>}],
           refs = [#ref{name = disco_identity,
                        label = '$identities'},
                   #ref{name = disco_feature,
                        label = '$features'},
                   #ref{name = xdata,
                        label = '$xdata'}]}).

-xml(disco_item,
     #elem{name = <<"item">>,
           xmlns = <<"http://jabber.org/protocol/disco#items">>,
           result = {disco_item, '$jid', '$name', '$node'},
           attrs = [#attr{name = <<"jid">>,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []},
                          required = true},
                    #attr{name = <<"name">>},
                    #attr{name = <<"node">>}]}).
-xml(disco_items,
     #elem{name = <<"query">>,
           xmlns = <<"http://jabber.org/protocol/disco#items">>,
           result = {disco_items, '$node', '$items'},
           attrs = [#attr{name = <<"node">>}],
           refs = [#ref{name = disco_item,
                        label = '$items'}]}).

-xml(private,
     #elem{name = <<"query">>,
           xmlns = <<"jabber:iq:private">>,
           result = {private, '$_xmls'}}).

-xml(conference_nick,
     #elem{name = <<"nick">>,
           xmlns = <<"storage:bookmarks">>,
           result = '$cdata'}).

-xml(conference_password,
     #elem{name = <<"password">>,
           xmlns = <<"storage:bookmarks">>,
           result = '$cdata'}).

-xml(bookmark_conference,
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
                        min = 0, max = 1}]}).

-xml(bookmark_url,
     #elem{name = <<"url">>,
           xmlns = <<"storage:bookmarks">>,
           result = {bookmark_url, '$name', '$url'},
           attrs = [#attr{name = <<"name">>,
                          required = true},
                    #attr{name = <<"url">>,
                          required = true}]}).

-xml(bookmarks_storage,
     #elem{name = <<"storage">>,
           xmlns = <<"storage:bookmarks">>,
           result = {bookmark_storage, '$conference', '$url'},
           refs = [#ref{name = bookmark_conference,
                        label = '$conference'},
                   #ref{name = bookmark_url,
                        label = '$url'}]}).

-xml(stat_error,
     #elem{name = <<"error">>,
           xmlns = <<"http://jabber.org/protocol/stats">>,
           result = {'$code', '$cdata'},
           attrs = [#attr{name = <<"code">>,
                          required = true,
                          enc = {enc_int, []},
                          dec = {dec_int, []}}]}).

-xml(stat,
     #elem{name = <<"stat">>,
           xmlns = <<"http://jabber.org/protocol/stats">>,
           result = {stat, '$name', '$units', '$value', '$error'},
           attrs = [#attr{name = <<"name">>,
                          required = true},
                    #attr{name = <<"units">>},
                    #attr{name = <<"value">>}],
           refs = [#ref{name = stat_error,
                        label = '$error'}]}).

-xml(stats,
     #elem{name = <<"query">>,
           xmlns = <<"http://jabber.org/protocol/stats">>,
           result = {stats, '$stat'},
           refs = [#ref{name = stat,
                        label = '$stat'}]}).

-xml(iq,
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
           refs = [#ref{name = error, min = 0, max = 1, label = '$error'}]}).

-xml(message_subject,
     #elem{name = <<"subject">>,
           xmlns = <<"jabber:client">>,
           result = {text, '$lang', '$data'},
           cdata = #cdata{label = '$data'},
           attrs = [#attr{name = <<"xml:lang">>, label = '$lang'}]}).

-xml(message_body,
     #elem{name = <<"body">>,
           xmlns = <<"jabber:client">>,
           result = {text, '$lang', '$data'},
           cdata = #cdata{label = '$data'},
           attrs = [#attr{name = <<"xml:lang">>, label = '$lang'}]}).

-xml(message_thread,
     #elem{name = <<"thread">>,
           xmlns = <<"jabber:client">>,
           result = '$cdata'}).

-xml(message,
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
                   #ref{name = message_body, label = '$body'}]}).

-xml(presence_show,
     #elem{name = <<"show">>,
           xmlns = <<"jabber:client">>,
           result = '$cdata',
           cdata = #cdata{enc = {enc_enum, []},
                          dec = {dec_enum, [[away, chat, dnd, xa]]}}}).

-xml(presence_status,
     #elem{name = <<"status">>,
           xmlns = <<"jabber:client">>,
           result = {text, '$lang', '$data'},
           cdata = #cdata{label = '$data'},
           attrs = [#attr{name = <<"xml:lang">>,
                          label = '$lang'}]}).

-xml(presence_priority,
     #elem{name = <<"priority">>,
           xmlns = <<"jabber:client">>,
           result = '$cdata',
           cdata = #cdata{enc = {enc_int, []},
                          dec = {dec_int, []}}}).

-xml(presence,
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
                        label = '$priority'}]}).

-xml(error_bad_request,
     #elem{name = <<"bad-request">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'bad-request'}).
-xml(error_conflict,
     #elem{name = <<"conflict">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'conflict'}).
-xml(error_feature_not_implemented,
     #elem{name = <<"feature-not-implemented">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'feature-not-implemented'}).
-xml(error_forbidden,
     #elem{name = <<"forbidden">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'forbidden'}).
-xml(error_gone,
     #elem{name = <<"gone">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           cdata = #cdata{label = '$uri'},
           result = {'gone', '$uri'}}).
-xml(error_internal_server_error,
     #elem{name = <<"internal-server-error">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'internal-server-error'}).
-xml(error_item_not_found,
     #elem{name = <<"item-not-found">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'item-not-found'}).
-xml(error_jid_malformed,
     #elem{name = <<"jid-malformed">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'jid-malformed'}).
-xml(error_not_acceptable,
     #elem{name = <<"not-acceptable">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'not-acceptable'}).
-xml(error_not_allowed,
     #elem{name = <<"not-allowed">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'not-allowed'}).
-xml(error_not_authorized,
     #elem{name = <<"not-authorized">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'not-authorized'}).
-xml(error_policy_violation,
     #elem{name = <<"policy-violation">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'policy-violation'}).
-xml(error_recipient_unavailable,
     #elem{name = <<"recipient-unavailable">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'recipient-unavailable'}).
-xml(error_redirect,
     #elem{name = <<"redirect">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           cdata = #cdata{label = '$uri'},
           result = {'redirect', '$uri'}}).
-xml(error_registration_required,
     #elem{name = <<"registration-required">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'registration-required'}).
-xml(error_remote_server_not_found,
     #elem{name = <<"remote-server-not-found">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'remote-server-not-found'}).
-xml(error_remote_server_timeout,
     #elem{name = <<"remote-server-timeout">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'remote-server-timeout'}).
-xml(error_resource_constraint,
     #elem{name = <<"resource-constraint">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'resource-constraint'}).
-xml(error_service_unavailable,
     #elem{name = <<"service-unavailable">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'service-unavailable'}).
-xml(error_subscription_required,
     #elem{name = <<"subscription-required">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'subscription-required'}).
-xml(error_undefined_condition,
     #elem{name = <<"undefined-condition">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'undefined-condition'}).
-xml(error_unexpected_request,
     #elem{name = <<"unexpected-request">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           result = 'unexpected-request'}).

-xml(error_text,
     #elem{name = <<"text">>,
           result = {text, '$lang', '$data'},
           cdata = #cdata{label = '$data'},
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-stanzas">>,
           attrs = [#attr{name = <<"xml:lang">>,
                          label = '$lang'}]}).

-xml(error,
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
                        min = 0, max = 1, label = '$reason'}]}).

-xml(bind_jid,
     #elem{name = <<"jid">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-bind">>,
           result = '$cdata',
           cdata = #cdata{dec = {dec_jid, []},
                          enc = {enc_jid, []}}}).

-xml(bind_resource,
     #elem{name = <<"resource">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-bind">>,
           result = '$cdata',
           cdata = #cdata{dec = {resourceprep, []},
                          enc = {resourceprep, []}}}).

-xml(bind, #elem{name = <<"bind">>,
                 xmlns = <<"urn:ietf:params:xml:ns:xmpp-bind">>,
                 result = {bind, '$jid', '$resource'},
                 refs = [#ref{name = bind_jid,
                              label = '$jid',
                              min = 0, max = 1},
                         #ref{name = bind_resource,
                              min = 0, max = 1,
                              label = '$resource'}]}).

-xml(sasl_auth,
     #elem{name = <<"auth">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
           cdata = #cdata{label = '$text',
                          dec = {base64, decode, []},
                          enc = {base64, encode, []}},
           result = {sasl_auth, '$mechanism', '$text'},
           attrs = [#attr{name = <<"mechanism">>,
                          required = true}]}).

-xml(sasl_abort,
     #elem{name = <<"abort">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
           result = {sasl_abort}}).

-xml(sasl_challenge,
     #elem{name = <<"challenge">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
           cdata = #cdata{label = '$text',
                          dec = {base64, decode, []},
                          enc = {base64, encode, []}},
           result = {sasl_challenge, '$text'}}).

-xml(sasl_response,
     #elem{name = <<"response">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
           cdata = #cdata{label = '$text',
                          dec = {base64, decode, []},
                          enc = {base64, encode, []}},
           result = {sasl_response, '$text'}}).

-xml(sasl_success,
     #elem{name = <<"success">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
           cdata = #cdata{label = '$text',
                          dec = {base64, decode, []},
                          enc = {base64, encode, []}},
           result = {sasl_success, '$text'}}).

-xml(sasl_failure_text,
     #elem{name = <<"text">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
           result = {text, '$lang', '$data'},
           cdata = #cdata{label = '$data'},
           attrs = [#attr{name = <<"xml:lang">>,
                          label = '$lang'}]}).

-xml(sasl_failure_aborted,
     #elem{name = <<"aborted">>,
           result = 'aborted',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}).
-xml(sasl_failure_account_disabled,
     #elem{name = <<"account-disabled">>,
           result = 'account-disabled',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}).
-xml(sasl_failure_credentials_expired,
     #elem{name = <<"credentials-expired">>,
           result = 'credentials-expired',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}).
-xml(sasl_failure_encryption_required,
     #elem{name = <<"encryption-required">>,
           result = 'encryption-required',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}).
-xml(sasl_failure_incorrect_encoding,
     #elem{name = <<"incorrect-encoding">>,
           result = 'incorrect-encoding',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}).
-xml(sasl_failure_invalid_authzid,
     #elem{name = <<"invalid-authzid">>,
           result = 'invalid-authzid',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}).
-xml(sasl_failure_invalid_mechanism,
     #elem{name = <<"invalid-mechanism">>,
           result = 'invalid-mechanism',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}).
-xml(sasl_failure_malformed_request,
     #elem{name = <<"malformed-request">>,
           result = 'malformed-request',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}).
-xml(sasl_failure_mechanism_too_weak,
     #elem{name = <<"mechanism-too-weak">>,
           result = 'mechanism-too-weak',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}).
-xml(sasl_failure_not_authorized,
     #elem{name = <<"not-authorized">>,
           result = 'not-authorized',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}).
-xml(sasl_failure_temporary_auth_failure,
     #elem{name = <<"temporary-auth-failure">>,
           result = 'temporary-auth-failure',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>}).

-xml(sasl_failure,
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
                        min = 0, max = 1, label = '$reason'}]}).

-xml(sasl_mechanism,
     #elem{name = <<"mechanism">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
           result = '$cdata'}).

-xml(sasl_mechanisms,
     #elem{name = <<"mechanisms">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-sasl">>,
           result = {sasl_mechanisms, '$list'},
           refs = [#ref{name = sasl_mechanism,
                        label = '$list'}]}).

-xml(starttls_required,
     #elem{name = <<"required">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-tls">>,
           result = true}).

-xml(starttls,
     #elem{name = <<"starttls">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-tls">>,
           result = {starttls, '$required'},
           refs = [#ref{name = starttls_required,
                        label = '$required',
                        min = 0, max = 1,
                        default = false}]}).

-xml(starttls_proceed,
     #elem{name = <<"proceed">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-tls">>,
           result = {starttls_proceed}}).

-xml(starttls_failure,
     #elem{name = <<"failure">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-tls">>,
           result = {starttls_failure}}).

-xml(compress_failure_setup_failed,
     #elem{name = <<"setup-failed">>,
           xmlns = <<"http://jabber.org/protocol/compress">>,
           result = 'setup-failed'}).
-xml(compress_failure_processing_failed,
     #elem{name = <<"processing-failed">>,
           xmlns = <<"http://jabber.org/protocol/compress">>,
           result = 'processing-failed'}).
-xml(compress_failure_unsupported_method,
     #elem{name = <<"unsupported-method">>,
           xmlns = <<"http://jabber.org/protocol/compress">>,
           result = 'unsupported-method'}).

-xml(compress_failure,
     #elem{name = <<"failure">>,
           xmlns = <<"http://jabber.org/protocol/compress">>,
           result = {compress_failure, '$reason'},
           refs = [#ref{name = compress_failure_setup_failed,
                        min = 0, max = 1, label = '$reason'},
                   #ref{name = compress_failure_processing_failed,
                        min = 0, max = 1, label = '$reason'},
                   #ref{name = compress_failure_unsupported_method,
                        min = 0, max = 1, label = '$reason'}]}).

-xml(compress_method,
     #elem{name = <<"method">>,
           xmlns = <<"http://jabber.org/protocol/compress">>,
           result = '$cdata'}).

-xml(compress,
     #elem{name = <<"compress">>,
           xmlns = <<"http://jabber.org/protocol/compress">>,
           result = {compress, '$methods'},
           refs = [#ref{name = compress_method,
                        label = '$methods'}]}).

-xml(compressed,
     #elem{name = <<"compressed">>,
           xmlns = <<"http://jabber.org/protocol/compress">>,
           result = {compressed}}).

-xml(compression_method,
     #elem{name = <<"method">>,
           xmlns = <<"http://jabber.org/features/compress">>,
           result = '$cdata'}).

-xml(compression,
     #elem{name = <<"compression">>,
           xmlns = <<"http://jabber.org/features/compress">>,
           result = {compression, '$methods'},
           refs = [#ref{name = compression_method, label = '$methods'}]}).

-xml(stream_features,
     #elem{name = <<"stream:features">>,
           xmlns = <<"http://etherx.jabber.org/streams">>,
           result = {stream_features, '$_els'}}).

-xml(p1_push,
     #elem{name = <<"push">>,
           result = {p1_push},
           xmlns = <<"p1:push">>}).

-xml(p1_rebind,
     #elem{name = <<"rebind">>,
           result = {p1_rebind},
           xmlns = <<"p1:rebind">>}).

-xml(p1_ack,
     #elem{name = <<"ack">>,
           result = {p1_ack},
           xmlns = <<"p1:ack">>}).

-xml(caps,
     #elem{name = <<"c">>,
           xmlns = <<"http://jabber.org/protocol/caps">>,
           result = {caps, '$hash', '$node', '$ver'},
           attrs = [#attr{name = <<"hash">>},
                    #attr{name = <<"node">>},
                    #attr{name = <<"ver">>,
                          enc = {base64, encode, []},
                          dec = {base64, decode, []}}]}).

-xml(feature_register,
     #elem{name = <<"register">>,
           xmlns = <<"http://jabber.org/features/iq-register">>,
           result = {feature_register}}).

-xml(register_registered,
     #elem{name = <<"registered">>,
           xmlns = <<"jabber:iq:register">>,
           result = true}).
-xml(register_remove,
     #elem{name = <<"remove">>,
           xmlns = <<"jabber:iq:register">>,
           result = true}).
-xml(register_instructions,
     #elem{name = <<"instructions">>,
           xmlns = <<"jabber:iq:register">>,
           result = '$cdata'}).
-xml(register_username,
     #elem{name = <<"username">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_nick,
     #elem{name = <<"nick">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_password,
     #elem{name = <<"password">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_name,
     #elem{name = <<"name">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_first,
     #elem{name = <<"first">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_last,
     #elem{name = <<"last">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_email,
     #elem{name = <<"email">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_address,
     #elem{name = <<"address">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_city,
     #elem{name = <<"city">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_state,
     #elem{name = <<"state">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_zip,
     #elem{name = <<"zip">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_phone,
     #elem{name = <<"phone">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_url,
     #elem{name = <<"url">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_date,
     #elem{name = <<"date">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_misc,
     #elem{name = <<"misc">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_text,
     #elem{name = <<"text">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).
-xml(register_key,
     #elem{name = <<"key">>,
           xmlns = <<"jabber:iq:register">>,
           cdata = #cdata{default = none},
           result = '$cdata'}).

-xml(register,
     #elem{name = <<"query">>,
           xmlns = <<"jabber:iq:register">>,
           result = {register, '$registered', '$remove', '$instructions',
                     '$username', '$nick', '$password', '$name',
                     '$first', '$last', '$email', '$address',
                     '$city', '$state', '$zip', '$phone', '$url',
                     '$date', '$misc', '$text', '$key'},
           refs = [#ref{name = register_registered, min = 0, max = 1,
                        default = false, label = '$registered'},
                   #ref{name = register_remove, min = 0, max = 1,
                        default = false, label = '$remove'},
                   #ref{name = register_instructions, min = 0, max = 1,
                        label = '$instructions'},
                   #ref{name = register_username, min = 0, max = 1,
                        label = '$username'},
                   #ref{name = register_nick, min = 0, max = 1,
                        label = '$nick'},
                   #ref{name = register_password, min = 0, max = 1,
                        label = '$password'},
                   #ref{name = register_name, min = 0, max = 1,
                        label = '$name'},
                   #ref{name = register_first, min = 0, max = 1,
                        label = '$first'},
                   #ref{name = register_last, min = 0, max = 1,
                        label = '$last'},
                   #ref{name = register_email, min = 0, max = 1,
                        label = '$email'},
                   #ref{name = register_address, min = 0, max = 1,
                        label = '$address'},
                   #ref{name = register_city, min = 0, max = 1,
                        label = '$city'},
                   #ref{name = register_state, min = 0, max = 1,
                        label = '$state'},
                   #ref{name = register_zip, min = 0, max = 1,
                        label = '$zip'},
                   #ref{name = register_phone, min = 0, max = 1,
                        label = '$phone'},
                   #ref{name = register_url, min = 0, max = 1,
                        label = '$url'},
                   #ref{name = register_date, min = 0, max = 1,
                        label = '$date'},
                   #ref{name = register_misc, min = 0, max = 1,
                        label = '$misc'},
                   #ref{name = register_text, min = 0, max = 1,
                        label = '$text'},
                   #ref{name = register_key, min = 0, max = 1,
                        label = '$key'}]}).

-xml(session,
     #elem{name = <<"session">>,
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-session">>,
           result = {session}}).

-xml(ping,
     #elem{name = <<"ping">>,
           xmlns = <<"urn:xmpp:ping">>,
           result = {ping}}).

-xml(time_utc,
     #elem{name = <<"utc">>,
           xmlns = <<"urn:xmpp:time">>,
           result = '$cdata',
           cdata = #cdata{dec = {dec_utc, []},
                          enc = {enc_utc, []}}}).

-xml(time_tzo,
     #elem{name = <<"tzo">>,
           xmlns = <<"urn:xmpp:time">>,
           result = '$cdata',
           cdata = #cdata{dec = {dec_tzo, []},
                          enc = {enc_tzo, []}}}).

-xml(time,
     #elem{name = <<"time">>,
           xmlns = <<"urn:xmpp:time">>,
           result = {time, '$tzo', '$utc'},
           refs = [#ref{name = time_tzo,
                        label = '$tzo',
                        min = 0, max = 1},
                   #ref{name = time_utc,
                        label = '$utc',
                        min = 0, max = 1}]}).

-xml(stream_error_text,
     #elem{name = <<"text">>,
           result = {text, '$lang', '$data'},
           cdata = #cdata{label = '$data'},
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>,
           attrs = [#attr{name = <<"xml:lang">>,
                          label = '$lang'}]}).

-xml(stream_error_bad_format,
     #elem{name = <<"bad-format">>,
           result = 'bad-format',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).       
-xml(stream_error_bad_namespace_prefix,
     #elem{name = <<"bad-namespace-prefix">>,
           result = 'bad-namespace-prefix',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_conflict,
     #elem{name = <<"conflict">>,
           result = 'conflict',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_connection_timeout,
     #elem{name = <<"connection-timeout">>,
           result = 'connection-timeout',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_host_gone,
     #elem{name = <<"host-gone">>,
           result = 'host-gone',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_host_unknown,
     #elem{name = <<"host-unknown">>,
           result = 'host-unknown',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_improper_addressing,
     #elem{name = <<"improper-addressing">>,
           result = 'improper-addressing',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_internal_server_error,
     #elem{name = <<"internal-server-error">>,
           result = 'internal-server-error',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_invalid_from,
     #elem{name = <<"invalid-from">>,
           result = 'invalid-from',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_invalid_id,
     #elem{name = <<"invalid-id">>,
           result = 'invalid-id',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_invalid_namespace,
     #elem{name = <<"invalid-namespace">>,
           result = 'invalid-namespace',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_invalid_xml,
     #elem{name = <<"invalid-xml">>,
           result = 'invalid-xml',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_not_authorized,
     #elem{name = <<"not-authorized">>,
           result = 'not-authorized',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_not_well_formed,
     #elem{name = <<"not-well-formed">>,
           result = 'not-well-formed',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_policy_violation,
     #elem{name = <<"policy-violation">>,
           result = 'policy-violation',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_remote_connection_failed,
     #elem{name = <<"remote-connection-failed">>,
           result = 'remote-connection-failed',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_reset,
     #elem{name = <<"reset">>,
           result = 'reset',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_resource_constraint,
     #elem{name = <<"resource-constraint">>,
           result = 'resource-constraint',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_restricted_xml,
     #elem{name = <<"restricted-xml">>,
           result = 'restricted-xml',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_see_other_host,
     #elem{name = <<"see-other-host">>,
           cdata = #cdata{required = true, label = '$host'},
           result = {'see-other-host', '$host'},
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_system_shutdown,
     #elem{name = <<"system-shutdown">>,
           result = 'system-shutdown',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_undefined_condition,
     #elem{name = <<"undefined-condition">>,
           result = 'undefined-condition',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_unsupported_encoding,
     #elem{name = <<"unsupported-encoding">>,
           result = 'unsupported-encoding',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_unsupported_stanza_type,
     #elem{name = <<"unsupported-stanza-type">>,
           result = 'unsupported-stanza-type',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).
-xml(stream_error_unsupported_version,
     #elem{name = <<"unsupported-version">>,
           result = 'unsupported-version',
           xmlns = <<"urn:ietf:params:xml:ns:xmpp-streams">>}).

-xml(stream_error,
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
                  ]}).

-xml(vcard_HOME, #elem{name = <<"HOME">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_WORK, #elem{name = <<"WORK">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_VOICE, #elem{name = <<"VOICE">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_FAX, #elem{name = <<"FAX">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_PAGER, #elem{name = <<"PAGER">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_MSG, #elem{name = <<"MSG">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_CELL, #elem{name = <<"CELL">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_VIDEO, #elem{name = <<"VIDEO">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_BBS, #elem{name = <<"BBS">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_MODEM, #elem{name = <<"MODEM">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_ISDN, #elem{name = <<"ISDN">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_PCS, #elem{name = <<"PCS">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_POSTAL, #elem{name = <<"POSTAL">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_PARCEL, #elem{name = <<"PARCEL">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_DOM, #elem{name = <<"DOM">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_INTL, #elem{name = <<"INTL">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_PREF, #elem{name = <<"PREF">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_INTERNET, #elem{name = <<"INTERNET">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_X400, #elem{name = <<"X400">>, xmlns = <<"vcard-temp">>, result = true}).
-xml(vcard_FAMILY, #elem{name = <<"FAMILY">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_GIVEN, #elem{name = <<"GIVEN">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_MIDDLE, #elem{name = <<"MIDDLE">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_PREFIX, #elem{name = <<"PREFIX">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_SUFFIX, #elem{name = <<"SUFFIX">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_POBOX, #elem{name = <<"POBOX">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_EXTADD, #elem{name = <<"EXTADD">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_STREET, #elem{name = <<"STREET">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_LOCALITY, #elem{name = <<"LOCALITY">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_REGION, #elem{name = <<"REGION">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_PCODE, #elem{name = <<"PCODE">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_CTRY, #elem{name = <<"CTRY">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_LINE, #elem{name = <<"LINE">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_NUMBER, #elem{name = <<"NUMBER">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_USERID, #elem{name = <<"USERID">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_LAT, #elem{name = <<"LAT">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_LON, #elem{name = <<"LON">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_ORGNAME, #elem{name = <<"ORGNAME">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_ORGUNIT, #elem{name = <<"ORGUNIT">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_PHONETIC, #elem{name = <<"PHONETIC">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_CRED, #elem{name = <<"CRED">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_VERSION, #elem{name = <<"VERSION">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_FN, #elem{name = <<"FN">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_NICKNAME, #elem{name = <<"NICKNAME">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_BDAY, #elem{name = <<"BDAY">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_JABBERID, #elem{name = <<"JABBERID">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_MAILER, #elem{name = <<"MAILER">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_TZ, #elem{name = <<"TZ">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_TITLE, #elem{name = <<"TITLE">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_ROLE, #elem{name = <<"ROLE">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_KEYWORD, #elem{name = <<"KEYWORD">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_NOTE, #elem{name = <<"NOTE">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_PRODID, #elem{name = <<"PRODID">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_REV, #elem{name = <<"REV">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_SORT_STRING, #elem{name = <<"SORT-STRING">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_UID, #elem{name = <<"UID">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_URL, #elem{name = <<"URL">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_DESC, #elem{name = <<"DESC">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_TYPE, #elem{name = <<"TYPE">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_EXTVAL, #elem{name = <<"EXTVAL">>, xmlns = <<"vcard-temp">>, result = '$cdata'}).
-xml(vcard_PUBLIC, #elem{name = <<"PUBLIC">>, xmlns = <<"vcard-temp">>, result = public}).
-xml(vcard_PRIVATE, #elem{name = <<"PRIVATE">>, xmlns = <<"vcard-temp">>, result = private}).
-xml(vcard_CONFIDENTIAL, #elem{name = <<"CONFIDENTIAL">>, xmlns = <<"vcard-temp">>, result = confidential}).

-xml(vcard_N,
     #elem{name = <<"N">>,
           xmlns = <<"vcard-temp">>,
           result = {vcard_name, '$family', '$given', '$middle',
                     '$prefix', '$suffix'},
           refs = [#ref{name = vcard_FAMILY, min = 0, max = 1, label = '$family'},
                   #ref{name = vcard_GIVEN, min = 0, max = 1, label = '$given'},
                   #ref{name = vcard_MIDDLE, min = 0, max = 1, label = '$middle'},
                   #ref{name = vcard_PREFIX, min = 0, max = 1, label = '$prefix'},
                   #ref{name = vcard_SUFFIX, min = 0, max = 1, label = '$suffix'}]}).

-xml(vcard_ADR,
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
                   #ref{name = vcard_CTRY, min = 0, max = 1, label = '$ctry'}]}).

-xml(vcard_LABEL,
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
                   #ref{name = vcard_LINE, label = '$line'}]}).

-xml(vcard_TEL,
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
                        min = 0, max = 1, label = '$number'}]}).

-xml(vcard_EMAIL,
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
                        min = 0, max = 1, label = '$userid'}]}).

-xml(vcard_GEO,
     #elem{name = <<"GEO">>,
           xmlns = <<"vcard-temp">>,
           result = {vcard_geo, '$lat', '$lon'},
           refs = [#ref{name = vcard_LAT, min = 0, max = 1, label = '$lat'},
                   #ref{name = vcard_LON, min = 0, max = 1, label = '$lon'}]}).

-xml(vcard_BINVAL,
     #elem{name = <<"BINVAL">>,
           xmlns = <<"vcard-temp">>,
           cdata = #cdata{dec = {base64, decode, []},
                          enc = {base64, encode, []}},
           result = '$cdata'}).

-xml(vcard_LOGO,
     #elem{name = <<"LOGO">>,
           xmlns = <<"vcard-temp">>,
           result = {vcard_logo, '$type', '$binval', '$extval'},
           refs = [#ref{name = vcard_TYPE, min = 0, max = 1, label = '$type'},
                   #ref{name = vcard_BINVAL, min = 0, max = 1, label = '$binval'},
                   #ref{name = vcard_EXTVAL, min = 0, max = 1, label = '$extval'}]}).

-xml(vcard_PHOTO,
     #elem{name = <<"PHOTO">>,
           xmlns = <<"vcard-temp">>,
           result = {vcard_photo, '$type', '$binval', '$extval'},
           refs = [#ref{name = vcard_TYPE, min = 0, max = 1, label = '$type'},
                   #ref{name = vcard_BINVAL, min = 0, max = 1, label = '$binval'},
                   #ref{name = vcard_EXTVAL, min = 0, max = 1, label = '$extval'}]}).

-xml(vcard_ORG,
     #elem{name = <<"ORG">>,
           xmlns = <<"vcard-temp">>,
           result = {vcard_org, '$name', '$units'},
           refs = [#ref{name = vcard_ORGNAME,
                        label = '$name',
                        min = 0, max = 1},
                   #ref{name = vcard_ORGUNIT,
                        label = '$units'}]}).

-xml(vcard_SOUND,
     #elem{name = <<"SOUND">>,
           xmlns = <<"vcard-temp">>,
           result = {vcard_sound, '$phonetic', '$binval', '$extval'},
           refs = [#ref{name = vcard_BINVAL, min = 0, max = 1, label = '$binval'},
                   #ref{name = vcard_EXTVAL, min = 0, max = 1, label = '$extval'},
                   #ref{name = vcard_PHONETIC, min = 0, max = 1, label = '$phonetic'}]}).

-xml(vcard_KEY,
     #elem{name = <<"KEY">>,
           xmlns = <<"vcard-temp">>,
           result = {vcard_key, '$type', '$cred'},
           refs = [#ref{name = vcard_TYPE, min = 0, max = 1, label = '$type'},
                   #ref{name = vcard_CRED, min = 0, max = 1, label = '$cred'}]}).

-xml(vcard_CATEGORIES,
     #elem{name = <<"CATEGORIES">>,
           xmlns = <<"vcard-temp">>,
           result = '$keywords',
           refs = [#ref{name = vcard_KEYWORD, label = '$keywords'}]}).

-xml(vcard_CLASS,
     #elem{name = <<"CLASS">>,
           xmlns = <<"vcard-temp">>,
           result = '$class',
           refs = [#ref{name = vcard_PUBLIC, min = 0, max = 1, label = '$class'},
                   #ref{name = vcard_PRIVATE, min = 0, max = 1, label = '$class'},
                   #ref{name = vcard_CONFIDENTIAL, min = 0, max = 1, label = '$class'}]}).

%% {vcard_AGENT,
%%  #elem{name = <<"AGENT">>,
%%        xmlns = <<"vcard-temp">>,
%%        result = {vcard_agent, '$vcard', '$extval'},
%%        refs = [#ref{name = vcard, min = 0, max = 1, label = '$vcard'},
%%                #ref{name = vcard_EXTVAL, min = 0, max = 1, label = '$extval'}]}).

-xml(vcard,
     #elem{name = <<"vCard">>,
           xmlns = <<"vcard-temp">>,
           result = {vcard, '$version', '$fn', '$n', '$nickname', '$photo',
                     '$bday', '$adr', '$label', '$tel', '$email', '$jabberid',
                     '$mailer', '$tz', '$geo', '$title', '$role', '$logo',
                     '$org', '$categories', '$note', '$prodid', %% '$agent',
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
                   %%#ref{name = vcard_AGENT, min = 0, max = 1, label = '$agent'},
                   #ref{name = vcard_SORT_STRING, min = 0, max = 1,
                        label = '$sort_string'},
                   #ref{name = vcard_UID, min = 0, max = 1, label = '$uid'},
                   #ref{name = vcard_URL, min = 0, max = 1, label = '$url'},
                   #ref{name = vcard_DESC, min = 0, max = 1, label = '$desc'},
                   #ref{name = vcard_CATEGORIES, default = [], min = 0, max = 1,
                        label = '$categories'},
                   #ref{name = vcard_CLASS, min = 0, max = 1, label = '$class'}]}).

-xml(xdata_field_required,
     #elem{name = <<"required">>,
           xmlns = <<"jabber:x:data">>,
           result = true}).

-xml(xdata_field_desc,
     #elem{name = <<"desc">>, xmlns = <<"jabber:x:data">>, result = '$cdata'}).

-xml(xdata_field_value,
     #elem{name = <<"value">>, xmlns = <<"jabber:x:data">>, result = '$cdata'}).

-xml(xdata_field_option,
     #elem{name = <<"option">>,
           xmlns = <<"jabber:x:data">>,
           result = '$value',
           refs = [#ref{name = xdata_field_value,
                        label = '$value',
                        min = 1, max = 1}]}).

-xml(xdata_field,
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
                        label = '$options'}]}).

-xml(xdata_instructions,  #elem{name = <<"instructions">>,
                                xmlns = <<"jabber:x:data">>,
                                result = '$cdata'}).
-xml(xdata_title, #elem{name = <<"title">>,
                        xmlns = <<"jabber:x:data">>,
                        result = '$cdata'}).
-xml(xdata_reported, #elem{name = <<"reported">>,
                           xmlns = <<"jabber:x:data">>,
                           result = '$fields',
                           refs = [#ref{name = xdata_field,
                                        label = '$fields'}]}).
-xml(xdata_item,  #elem{name = <<"item">>,
                        xmlns = <<"jabber:x:data">>,
                        result = '$fields',
                        refs = [#ref{name = xdata_field,
                                     label = '$fields'}]}).

-xml(xdata,
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
                        label = '$fields'}]}).

-xml(pubsub_subscription,
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
                          enc = {enc_enum, []}}]}).

-xml(pubsub_affiliation,
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
                          enc = {enc_enum, []}}]}).

-xml(pubsub_item,
     #elem{name = <<"item">>,
           xmlns = <<"http://jabber.org/protocol/pubsub">>,
           result = {pubsub_item, '$id', '$_xmls'},
           attrs = [#attr{name = <<"id">>}]}).

-xml(pubsub_items,
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
           refs = [#ref{name = pubsub_item, label = '$items'}]}).

-xml(pubsub_event_retract,
     #elem{name = <<"retract">>,
           xmlns = <<"http://jabber.org/protocol/pubsub#event">>,
           result = '$id',
           attrs = [#attr{name = <<"id">>, required = true}]}).

-xml(pubsub_event_item,
     #elem{name = <<"item">>,
           xmlns = <<"http://jabber.org/protocol/pubsub#event">>,
           result = {pubsub_event_item, '$id', '$node', '$publisher'},
           attrs = [#attr{name = <<"id">>},
                    #attr{name = <<"node">>},
                    #attr{name = <<"publisher">>}]}).

-xml(pubsub_event_items,
     #elem{name = <<"items">>,
           xmlns = <<"http://jabber.org/protocol/pubsub#event">>,
           result = {pubsub_event_items, '$node', '$retract', '$items'},
           attrs = [#attr{name = <<"node">>,
                          required = true}],
           refs = [#ref{name = pubsub_event_retract, label = '$retract'},
                   #ref{name = pubsub_event_item, label = '$items'}]}).

-xml(pubsub_event,
     #elem{name = <<"event">>,
           xmlns = <<"http://jabber.org/protocol/pubsub#event">>,
           result = {pubsub_event, '$items'},
           refs = [#ref{name = pubsub_event_items, label = '$items'}]}).

-xml(pubsub_subscriptions,
     #elem{name = <<"subscriptions">>,
           xmlns = <<"http://jabber.org/protocol/pubsub">>,
           result = {'$node', '$subscriptions'},
           attrs = [#attr{name = <<"node">>,
                          default = none}],
           refs = [#ref{name = pubsub_subscription, label = '$subscriptions'}]}).

-xml(pubsub_affiliations,
     #elem{name = <<"affiliations">>,
           xmlns = <<"http://jabber.org/protocol/pubsub">>,
           result = '$affiliations',
           refs = [#ref{name = pubsub_affiliation, label = '$affiliations'}]}).

-xml(pubsub_subscribe,
     #elem{name = <<"subscribe">>,
           xmlns = <<"http://jabber.org/protocol/pubsub">>,
           result = {pubsub_subscribe, '$node', '$jid'},
           attrs = [#attr{name = <<"node">>},
                    #attr{name = <<"jid">>,
                          required = true,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}}]}).

-xml(pubsub_unsubscribe,
     #elem{name = <<"unsubscribe">>,
           xmlns = <<"http://jabber.org/protocol/pubsub">>,
           result = {pubsub_unsubscribe, '$node', '$jid', '$subid'},
           attrs = [#attr{name = <<"node">>},
                    #attr{name = <<"subid">>},
                    #attr{name = <<"jid">>,
                          required = true,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}}]}).

-xml(pubsub_publish,
     #elem{name = <<"publish">>,
           xmlns = <<"http://jabber.org/protocol/pubsub">>,
           result = {pubsub_publish, '$node', '$items'},
           attrs = [#attr{name = <<"node">>,
                          required = true}],
           refs = [#ref{name = pubsub_item, label = '$items'}]}).

-xml(pubsub_options,
     #elem{name = <<"options">>,
           xmlns = <<"http://jabber.org/protocol/pubsub">>,
           result = {pubsub_options, '$node', '$jid', '$subid', '$xdata'},
           attrs = [#attr{name = <<"node">>},
                    #attr{name = <<"subid">>},
                    #attr{name = <<"jid">>,
                          required = true,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}}],
           refs = [#ref{name = xdata, min = 0, max = 1,
                        label = '$xdata'}]}).

-xml(pubsub_retract,
     #elem{name = <<"retract">>,
           xmlns = <<"http://jabber.org/protocol/pubsub">>,
           result = {pubsub_retract, '$node', '$notify', '$items'},
           attrs = [#attr{name = <<"node">>,
                          required = true},
                    #attr{name = <<"notify">>,
                          default = false,
                          dec = {dec_bool, []},
                          enc = {enc_bool, []}}],
           refs = [#ref{name = pubsub_item, label = '$items'}]}).

-xml(pubsub,
     #elem{name = <<"pubsub">>,
           xmlns = <<"http://jabber.org/protocol/pubsub">>,
           result = {pubsub, '$subscriptions', '$affiliations', '$publish',
                     '$subscribe', '$unsubscribe', '$options', '$items',
                     '$retract'},
           refs = [#ref{name = pubsub_subscriptions, label = '$subscriptions',
                        min = 0, max = 1},
                   #ref{name = pubsub_affiliations, label = '$affiliations',
                        min = 0, max = 1},
                   #ref{name = pubsub_subscribe, label = '$subscribe',
                        min = 0, max = 1},
                   #ref{name = pubsub_unsubscribe, label = '$unsubscribe',
                        min = 0, max = 1},
                   #ref{name = pubsub_options, label = '$options',
                        min = 0, max = 1},
                   #ref{name = pubsub_items, label = '$items',
                        min = 0, max = 1},
                   #ref{name = pubsub_retract, label = '$retract',
                        min = 0, max = 1},
                   #ref{name = pubsub_publish, label = '$publish',
                        min = 0, max = 1}]}).

-xml(shim_header,
     #elem{name = <<"header">>,
           xmlns = <<"http://jabber.org/protocol/shim">>,
           result = {'$name', '$cdata'},
           attrs = [#attr{name = <<"name">>,
                          required = true}]}).

-xml(shim_headers,
     #elem{name = <<"headers">>,
           xmlns = <<"http://jabber.org/protocol/shim">>,
           result = {shim, '$headers'},
           refs = [#ref{name = shim_header, label = '$headers'}]}).

-xml(delay,
     #elem{name = <<"delay">>,
           xmlns = <<"urn:xmpp:delay">>,
           result = {delay, '$stamp', '$from'},
           attrs = [#attr{name = <<"stamp">>,
                          required = true,
                          dec = {dec_utc, []},
                          enc = {enc_utc, []}},
                    #attr{name = <<"from">>,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}}]}).

-xml(legacy_delay,
     #elem{name = <<"x">>,
           xmlns = <<"jabber:x:delay">>,
           result = {legacy_delay, '$stamp', '$from'},
           attrs = [#attr{name = <<"stamp">>,
                          required = true},
                    #attr{name = <<"from">>,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}}]}).

-xml(bytestreams_streamhost,
     #elem{name = <<"streamhost">>,
           xmlns = <<"http://jabber.org/protocol/bytestreams">>,
           result = {streamhost, '$jid', '$host', '$port'},
           attrs = [#attr{name = <<"jid">>,
                          required = true,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}},
                    #attr{name = <<"host">>,
                          required = true},
                    #attr{name = <<"port">>,
                          default = 1080,
                          dec = {dec_int, [0, 65535]},
                          enc = {enc_int, []}}]}).

-xml(bytestreams_streamhost_used,
     #elem{name = <<"streamhost-used">>,
           xmlns = <<"http://jabber.org/protocol/bytestreams">>,
           result = '$jid',
           attrs = [#attr{name = <<"jid">>,
                          required = true,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}}]}).

-xml(bytestreams_activate,
     #elem{name = <<"activate">>,
           xmlns = <<"http://jabber.org/protocol/bytestreams">>,
           cdata = #cdata{enc = {enc_jid, []}, dec = {dec_jid, []}},
           result = '$cdata'}).

-xml(bytestreams,
     #elem{name = <<"query">>,
           xmlns = <<"http://jabber.org/protocol/bytestreams">>,
           result = {bytestreams, '$hosts', '$used', '$activate',
                     '$dstaddr', '$mode', '$sid'},
           attrs = [#attr{name = <<"dstaddr">>},
                    #attr{name = <<"sid">>},
                    #attr{name = <<"mode">>,
                          default = tcp,
                          dec = {dec_enum, [[tcp, udp]]},
                          enc = {enc_enum, []}}],
           refs = [#ref{name = bytestreams_streamhost, label = '$hosts'},
                   #ref{name = bytestreams_streamhost_used,
                        min = 0, max = 1, label = '$used'},
                   #ref{name = bytestreams_activate,
                        min = 0, max = 1, label = '$activate'}]}).

-xml(muc_history,
     #elem{name = <<"history">>,
           xmlns = <<"http://jabber.org/protocol/muc">>,
           result = {muc_history, '$maxchars', '$maxstanzas',
                     '$seconds', '$since'},
           attrs = [#attr{name = <<"maxchars">>,
                          dec = {dec_int, [0, infinity]},
                          enc = {enc_int, []}},
                    #attr{name = <<"maxstanzas">>,
                          dec = {dec_int, [0, infinity]},
                          enc = {enc_int, []}},
                    #attr{name = <<"seconds">>,
                          dec = {dec_int, [0, infinity]},
                          enc = {enc_int, []}},
                    #attr{name = <<"since">>,
                          dec = {dec_utc, []},
                          enc = {enc_utc, []}}]}).

-xml(muc_user_reason,
     #elem{name = <<"reason">>,
           xmlns = <<"http://jabber.org/protocol/muc#user">>,
           result = '$cdata'}).

-xml(muc_user_decline,
     #elem{name = <<"decline">>,
           xmlns = <<"http://jabber.org/protocol/muc#user">>,
           result = {muc_decline, '$reason', '$from', '$to'},
           attrs = [#attr{name = <<"to">>,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}},
                    #attr{name = <<"from">>,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}}],
           refs = [#ref{name = muc_user_reason, min = 0,
                        max = 1, label = '$reason'}]}).

-xml(muc_user_destroy,
     #elem{name = <<"destroy">>,
           xmlns = <<"http://jabber.org/protocol/muc#user">>,
           result = {muc_user_destroy, '$reason', '$jid'},
           attrs = [#attr{name = <<"jid">>,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}}],
           refs = [#ref{name = muc_user_reason, min = 0,
                        max = 1, label = '$reason'}]}).

-xml(muc_user_invite,
     #elem{name = <<"invite">>,
           xmlns = <<"http://jabber.org/protocol/muc#user">>,
           result = {muc_invite, '$reason', '$from', '$to'},
           attrs = [#attr{name = <<"to">>,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}},
                    #attr{name = <<"from">>,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}}],
           refs = [#ref{name = muc_user_reason, min = 0,
                        max = 1, label = '$reason'}]}).

-xml(muc_user_actor,
     #elem{name = <<"actor">>,
           xmlns = <<"http://jabber.org/protocol/muc#user">>,
           result = {muc_actor, '$jid', '$nick'},
           attrs = [#attr{name = <<"jid">>,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}},
                    #attr{name = <<"nick">>}]}).

-xml(muc_user_continue,
     #elem{name = <<"continue">>,
           xmlns = <<"http://jabber.org/protocol/muc#user">>,
           result = '$thread',
           attrs = [#attr{name = <<"thread">>}]}).

-xml(muc_user_status,
     #elem{name = <<"status">>,
           xmlns = <<"http://jabber.org/protocol/muc#user">>,
           result = '$code',
           attrs = [#attr{name = <<"code">>,
                          dec = {dec_int, [100, 999]},
                          enc = {enc_int, []}}]}).

-xml(muc_user_item,
     #elem{name = <<"item">>,
           xmlns = <<"http://jabber.org/protocol/muc#user">>,
           result = {muc_item, '$actor', '$continue', '$reason',
                     '$affiliation', '$role', '$jid', '$nick'},
           refs = [#ref{name = muc_user_actor,
                        min = 0, max = 1, label = '$actor'},
                   #ref{name = muc_user_continue,
                        min = 0, max = 1, label = '$continue'},
                   #ref{name = muc_user_reason,
                        min = 0, max = 1, label = '$reason'}],
           attrs = [#attr{name = <<"affiliation">>,
                          dec = {dec_enum, [[admin, member, none,
                                             outcast, owner]]},
                          enc = {enc_enum, []}},
                    #attr{name = <<"role">>,
                          dec = {dec_enum, [[moderator, none,
                                             participant, visitor]]},
                          enc = {enc_enum, []}},
                    #attr{name = <<"jid">>,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}},
                    #attr{name = <<"nick">>}]}).

-xml(muc_user,
     #elem{name = <<"x">>,
           xmlns = <<"http://jabber.org/protocol/muc#user">>,
           result = {muc_user, '$decline', '$destroy', '$invites',
                     '$items', '$status_codes', '$password'},
           attrs = [#attr{name = <<"password">>}],
           refs = [#ref{name = muc_user_decline, min = 0,
                        max = 1, label = '$decline'},
                   #ref{name = muc_user_destroy, min = 0, max = 1,
                        label = '$destroy'},
                   #ref{name = muc_user_invite, label = '$invites'},
                   #ref{name = muc_user_item, label = '$items'},
                   #ref{name = muc_user_status, label = '$status_codes'}]}).

-xml(muc_owner_password,
     #elem{name = <<"password">>,
           xmlns = <<"http://jabber.org/protocol/muc#owner">>,
           result = '$cdata'}).

-xml(muc_owner_reason,
     #elem{name = <<"reason">>,
           xmlns = <<"http://jabber.org/protocol/muc#owner">>,
           result = '$cdata'}).

-xml(muc_owner_destroy,
     #elem{name = <<"destroy">>,
           xmlns = <<"http://jabber.org/protocol/muc#owner">>,
           result = {muc_owner_destroy, '$jid', '$reason', '$password'},
           attrs = [#attr{name = <<"jid">>,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}}],
           refs = [#ref{name = muc_owner_password, min = 0, max = 1,
                        label = '$password'},
                   #ref{name = muc_owner_reason, min = 0, max = 1,
                        label = '$reason'}]}).

-xml(muc_owner,
     #elem{name = <<"query">>,
           xmlns = <<"http://jabber.org/protocol/muc#owner">>,
           result = {muc_owner, '$destroy', '$config'},
           refs = [#ref{name = muc_owner_destroy, min = 0, max = 1,
                        label = '$destroy'},
                   #ref{name = xdata, min = 0, max = 1, label = '$config'}]}).

-xml(muc,
     #elem{name = <<"x">>,
           xmlns = <<"http://jabber.org/protocol/muc">>,
           result = {muc, '$history', '$password'},
           attrs = [#attr{name = <<"password">>}],
           refs = [#ref{name = muc_history, min = 0, max = 1,
                        label = '$history'}]}).

dec_tzo(Val) ->
    [H1, M1] = str:tokens(Val, <<":">>),
    H = jlib:binary_to_integer(H1),
    M = jlib:binary_to_integer(M1),
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
