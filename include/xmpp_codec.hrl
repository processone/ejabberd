%% Created automatically by XML generator (fxml_gen.erl)
%% Source: xmpp_codec.spec

-record(vcard_xupdate, {us = {<<>>, <<>>} :: {binary(), binary()},
			hash :: binary()}).
-type vcard_xupdate() :: #vcard_xupdate{}.

-record(ps_affiliation, {xmlns = <<>> :: binary(),
			 node = <<>> :: binary(),
			 type :: member | none | outcast |
				 owner | publisher | 'publish-only',
			 jid :: jid:jid()}).
-type ps_affiliation() :: #ps_affiliation{}.

-type ps_error_type() :: 'closed-node' | 'configuration-required' |
			 'invalid-jid' | 'invalid-options' |
			 'invalid-payload' | 'invalid-subid' |
			 'item-forbidden' | 'item-required' | 'jid-required' |
			 'max-items-exceeded' | 'max-nodes-exceeded' |
			 'nodeid-required' | 'not-in-roster-group' |
			 'not-subscribed' | 'payload-too-big' |
			 'payload-required' | 'pending-subscription' |
			 'presence-subscription-required' | 'subid-required' |
			 'too-many-subscriptions' | 'unsupported' |
			 'unsupported-access-model'.
-type ps_feature() :: 'access-authorize' | 'access-open' |
		      'access-presence' | 'access-roster' |
		      'access-whitelist' | 'auto-create' |
		      'auto-subscribe' | 'collections' | 'config-node' |
		      'create-and-configure' | 'create-nodes' |
		      'delete-items' | 'delete-nodes' |
		      'filtered-notifications' | 'get-pending' |
		      'instant-nodes' | 'item-ids' | 'last-published' |
		      'leased-subscription' | 'manage-subscriptions' |
		      'member-affiliation' | 'meta-data' |
		      'modify-affiliations' | 'multi-collection' |
		      'multi-subscribe' | 'outcast-affiliation' |
		      'persistent-items' | 'presence-notifications' |
		      'presence-subscribe' | 'publish' |
		      'publish-options' | 'publish-only-affiliation' |
		      'publisher-affiliation' | 'purge-nodes' |
		      'retract-items' | 'retrieve-affiliations' |
		      'retrieve-default' | 'retrieve-items' |
		      'retrieve-subscriptions' | 'subscribe' |
		      'subscription-options' | 'subscription-notifications'.
-record(ps_error, {type :: ps_error_type(), feature :: ps_feature()}).
-type ps_error() :: #ps_error{}.

-record(chatstate, {type :: active | composing | gone | inactive | paused}).
-type chatstate() :: #chatstate{}.

-record(csi, {type :: active | inactive}).
-type csi() :: #csi{}.

-record(hint, {type :: 'no-copy' | 'no-store' | 'no-storage' | 'store' |
		       'no-permanent-store' | 'no-permanent-storage'}).
-type hint() :: #hint{}.

-record(iq, {id = <<>> :: binary(),
             type :: 'error' | 'get' | 'result' | 'set',
             lang = <<>> :: binary(),
             from :: jid:jid(),
             to :: jid:jid(),
             sub_els = [] :: [xmpp_element() | fxml:xmlel()]}).
-type iq() :: #iq{}.

-record(feature_register, {}).
-type feature_register() :: #feature_register{}.

-record(adhoc_note, {type = info :: 'error' | 'info' | 'warn',
                     data = <<>> :: binary()}).
-type adhoc_note() :: #adhoc_note{}.

-record(address, {type :: 'bcc' | 'cc' | 'noreply' | 'ofrom' | 'replyroom' | 'replyto' | 'to',
                  jid :: jid:jid(),
                  desc = <<>> :: binary(),
                  node = <<>> :: binary(),
                  delivered :: boolean()}).
-type address() :: #address{}.

-record(sasl_success, {text = <<>> :: binary()}).
-type sasl_success() :: #sasl_success{}.

-record(mam_result, {xmlns = <<>> :: binary(),
                     queryid = <<>> :: binary(),
                     id = <<>> :: binary(),
                     sub_els = [] :: [xmpp_element() | fxml:xmlel()]}).
-type mam_result() :: #mam_result{}.

-record(rsm_first, {index :: non_neg_integer(),
                    data = <<>> :: binary()}).
-type rsm_first() :: #rsm_first{}.

-record(text, {lang = <<>> :: binary(),
               data = <<>> :: binary()}).
-type text() :: #text{}.

-record(streamhost, {jid :: jid:jid(),
                     host = <<>> :: binary(),
                     port = 1080 :: non_neg_integer()}).
-type streamhost() :: #streamhost{}.

-record(sm_resume, {h :: non_neg_integer(),
                    previd = <<>> :: binary(),
                    xmlns = <<>> :: binary()}).
-type sm_resume() :: #sm_resume{}.

-record(carbons_enable, {}).
-type carbons_enable() :: #carbons_enable{}.

-record(carbons_private, {}).
-type carbons_private() :: #carbons_private{}.

-record(expire, {seconds :: non_neg_integer(),
                 stored :: non_neg_integer()}).
-type expire() :: #expire{}.

-record(muc_unsubscribe, {}).
-type muc_unsubscribe() :: #muc_unsubscribe{}.

-record(ps_unsubscribe, {node = <<>> :: binary(),
                         jid :: jid:jid(),
                         subid = <<>> :: binary()}).
-type ps_unsubscribe() :: #ps_unsubscribe{}.

-record(mix_leave, {}).
-type mix_leave() :: #mix_leave{}.

-record(ping, {}).
-type ping() :: #ping{}.

-record(delay, {stamp :: erlang:timestamp(),
                from :: jid:jid(),
                desc = <<>> :: binary()}).
-type delay() :: #delay{}.

-record(muc_history, {maxchars :: non_neg_integer(),
                      maxstanzas :: non_neg_integer(),
                      seconds :: non_neg_integer(),
                      since :: erlang:timestamp()}).
-type muc_history() :: #muc_history{}.

-record(thumbnail, {uri = <<>> :: binary(),
                    'media-type' = <<>> :: binary(),
                    width :: non_neg_integer(),
                    height :: non_neg_integer()}).
-type thumbnail() :: #thumbnail{}.

-record(muc_decline, {reason = <<>> :: binary(),
                      from :: jid:jid(),
                      to :: jid:jid()}).
-type muc_decline() :: #muc_decline{}.

-record(sm_a, {h :: non_neg_integer(),
               xmlns = <<>> :: binary()}).
-type sm_a() :: #sm_a{}.

-record(stream_start, {from :: jid:jid(),
                       to :: jid:jid(),
                       id = <<>> :: binary(),
                       version = <<>> :: binary(),
                       xmlns = <<>> :: binary(),
                       stream_xmlns = <<>> :: binary(),
                       db_xmlns = <<>> :: binary(),
                       lang = <<>> :: binary()}).
-type stream_start() :: #stream_start{}.

-record(muc_subscribe, {nick = <<>> :: binary(),
                        events = [] :: [binary()]}).
-type muc_subscribe() :: #muc_subscribe{}.

-record(stanza_id, {by :: jid:jid(),
                    id = <<>> :: binary()}).
-type stanza_id() :: #stanza_id{}.

-record(starttls_proceed, {}).
-type starttls_proceed() :: #starttls_proceed{}.

-record(client_id, {id = <<>> :: binary()}).
-type client_id() :: #client_id{}.

-record(sm_resumed, {h :: non_neg_integer(),
                     previd = <<>> :: binary(),
                     xmlns = <<>> :: binary()}).
-type sm_resumed() :: #sm_resumed{}.

-record(forwarded, {delay :: #delay{},
                    sub_els = [] :: [xmpp_element() | fxml:xmlel()]}).
-type forwarded() :: #forwarded{}.

-record(sm_enable, {max :: non_neg_integer(),
                    resume = false :: boolean(),
                    xmlns = <<>> :: binary()}).
-type sm_enable() :: #sm_enable{}.

-record(starttls_failure, {}).
-type starttls_failure() :: #starttls_failure{}.

-record(sasl_challenge, {text = <<>> :: binary()}).
-type sasl_challenge() :: #sasl_challenge{}.

-record(handshake, {data = <<>> :: binary()}).
-type handshake() :: #handshake{}.

-record(gone, {uri = <<>> :: binary()}).
-type gone() :: #gone{}.

-record(x_conference, {jid :: jid:jid(),
                       password = <<>> :: binary(),
                       reason = <<>> :: binary(),
                       continue :: boolean(),
                       thread = <<>> :: binary()}).
-type x_conference() :: #x_conference{}.

-record(private, {xml_els = [] :: [fxml:xmlel()]}).
-type private() :: #private{}.

-record(db_verify, {from = <<>> :: binary(),
                    to = <<>> :: binary(),
                    id = <<>> :: binary(),
                    type :: 'error' | 'invalid' | 'valid',
                    key = <<>> :: binary(),
                    sub_els = [] :: [xmpp_element() | fxml:xmlel()]}).
-type db_verify() :: #db_verify{}.

-record(nick, {name = <<>> :: binary()}).
-type nick() :: #nick{}.

-record(p1_ack, {}).
-type p1_ack() :: #p1_ack{}.

-record(feature_sm, {xmlns = <<>> :: binary()}).
-type feature_sm() :: #feature_sm{}.

-record(ps_item, {xmlns = <<>> :: binary(),
                  id = <<>> :: binary(),
                  xml_els = [] :: [fxml:xmlel()],
                  node = <<>> :: binary(),
                  publisher = <<>> :: binary()}).
-type ps_item() :: #ps_item{}.

-record(ps_publish, {node = <<>> :: binary(),
                     items = [] :: [#ps_item{}]}).
-type ps_publish() :: #ps_publish{}.

-record(roster_item, {jid :: jid:jid(),
                      name = <<>> :: binary(),
                      groups = [] :: [binary()],
                      subscription = none :: 'both' | 'from' | 'none' | 'remove' | 'to',
                      ask :: 'subscribe'}).
-type roster_item() :: #roster_item{}.

-record(roster_query, {items = [] :: [#roster_item{}],
                       ver :: binary()}).
-type roster_query() :: #roster_query{}.

-record(sm_r, {xmlns = <<>> :: binary()}).
-type sm_r() :: #sm_r{}.

-record(muc_actor, {jid :: jid:jid(),
                    nick = <<>> :: binary()}).
-type muc_actor() :: #muc_actor{}.

-record(stat_error, {code :: integer(),
                     reason = <<>> :: binary()}).
-type stat_error() :: #stat_error{}.

-record(stat, {name = <<>> :: binary(),
               units = <<>> :: binary(),
               value = <<>> :: binary(),
               error :: #stat_error{}}).
-type stat() :: #stat{}.

-record(addresses, {list = [] :: [#address{}]}).
-type addresses() :: #addresses{}.

-record('see-other-host', {host :: binary() | inet:ip_address() | {binary() | inet:ip_address(),non_neg_integer()}}).
-type 'see-other-host'() :: #'see-other-host'{}.

-record(compress, {methods = [] :: [binary()]}).
-type compress() :: #compress{}.

-record(starttls, {required = false :: boolean()}).
-type starttls() :: #starttls{}.

-record(last, {seconds :: non_neg_integer(),
               status = <<>> :: binary()}).
-type last() :: #last{}.

-record(redirect, {uri = <<>> :: binary()}).
-type redirect() :: #redirect{}.

-record(sm_enabled, {id = <<>> :: binary(),
                     location = <<>> :: binary(),
                     max :: non_neg_integer(),
                     resume = false :: boolean(),
                     xmlns = <<>> :: binary()}).
-type sm_enabled() :: #sm_enabled{}.

-record(muc_unique, {name = <<>> :: binary()}).
-type muc_unique() :: #muc_unique{}.

-record(sasl_response, {text = <<>> :: binary()}).
-type sasl_response() :: #sasl_response{}.

-record(legacy_auth, {username :: binary(),
                      password :: binary(),
                      digest :: binary(),
                      resource :: binary()}).
-type legacy_auth() :: #legacy_auth{}.

-record(ps_subscribe, {node = <<>> :: binary(),
                       jid :: jid:jid()}).
-type ps_subscribe() :: #ps_subscribe{}.

-record(message, {id = <<>> :: binary(),
                  type = normal :: 'chat' | 'error' | 'groupchat' | 'headline' | 'normal',
                  lang = <<>> :: binary(),
                  from :: jid:jid(),
                  to :: jid:jid(),
                  subject = [] :: [#text{}],
                  body = [] :: [#text{}],
                  thread :: binary(),
                  sub_els = [] :: [xmpp_element() | fxml:xmlel()]}).
-type message() :: #message{}.

-record(sasl_auth, {mechanism = <<>> :: binary(),
                    text = <<>> :: binary()}).
-type sasl_auth() :: #sasl_auth{}.

-record(p1_push, {}).
-type p1_push() :: #p1_push{}.

-record(feature_csi, {xmlns = <<>> :: binary()}).
-type feature_csi() :: #feature_csi{}.

-record(disco_item, {jid :: jid:jid(),
                     name = <<>> :: binary(),
                     node = <<>> :: binary()}).
-type disco_item() :: #disco_item{}.

-record(unblock, {items = [] :: [jid:jid()]}).
-type unblock() :: #unblock{}.

-record(block, {items = [] :: [jid:jid()]}).
-type block() :: #block{}.

-record(compression, {methods = [] :: [binary()]}).
-type compression() :: #compression{}.

-record(muc_subscriptions, {list = [] :: [jid:jid()]}).
-type muc_subscriptions() :: #muc_subscriptions{}.

-record(ps_subscription, {xmlns = <<>> :: binary(),
                          jid :: jid:jid(),
                          type :: 'none' | 'pending' | 'subscribed' | 'unconfigured',
                          node = <<>> :: binary(),
                          subid = <<>> :: binary(),
                          expiry :: erlang:timestamp()}).
-type ps_subscription() :: #ps_subscription{}.

-record(bob_data, {cid = <<>> :: binary(),
                   'max-age' :: non_neg_integer(),
                   type = <<>> :: binary(),
                   data = <<>> :: binary()}).
-type bob_data() :: #bob_data{}.

-record(muc_item, {actor :: #muc_actor{},
                   continue :: binary(),
                   reason = <<>> :: binary(),
                   affiliation :: 'admin' | 'member' | 'none' | 'outcast' | 'owner',
                   role :: 'moderator' | 'none' | 'participant' | 'visitor',
                   jid :: jid:jid(),
                   nick = <<>> :: binary()}).
-type muc_item() :: #muc_item{}.

-record(muc_admin, {items = [] :: [#muc_item{}]}).
-type muc_admin() :: #muc_admin{}.

-record(shim, {headers = [] :: [{binary(),binary()}]}).
-type shim() :: #shim{}.

-record(mam_prefs, {xmlns = <<>> :: binary(),
                    default :: 'always' | 'never' | 'roster',
                    always :: [jid:jid()],
                    never :: [jid:jid()]}).
-type mam_prefs() :: #mam_prefs{}.

-record(caps, {node = <<>> :: binary(),
               version = <<>> :: binary(),
               hash = <<>> :: binary(),
               exts = [] :: [binary()]}).
-type caps() :: #caps{}.

-record(muc, {history :: #muc_history{},
              password :: binary()}).
-type muc() :: #muc{}.

-record(stream_features, {sub_els = [] :: [xmpp_element() | fxml:xmlel()]}).
-type stream_features() :: #stream_features{}.

-record(stats, {list = [] :: [#stat{}],
                node = <<>> :: binary()}).
-type stats() :: #stats{}.

-record(ps_items, {xmlns = <<>> :: binary(),
                   node = <<>> :: binary(),
                   items = [] :: [#ps_item{}],
                   max_items :: non_neg_integer(),
                   subid = <<>> :: binary(),
                   retract :: binary()}).
-type ps_items() :: #ps_items{}.

-record(presence, {id = <<>> :: binary(),
                   type = available :: 'available' | 'error' | 'probe' | 'subscribe' | 'subscribed' | 'unavailable' | 'unsubscribe' | 'unsubscribed',
                   lang = <<>> :: binary(),
                   from :: jid:jid(),
                   to :: jid:jid(),
                   show :: 'away' | 'chat' | 'dnd' | 'xa',
                   status = [] :: [#text{}],
                   priority :: integer(),
                   sub_els = [] :: [xmpp_element() | fxml:xmlel()]}).
-type presence() :: #presence{}.

-record(sic, {ip :: inet:ip_address(),
              port :: non_neg_integer(),
              xmlns = <<>> :: binary()}).
-type sic() :: #sic{}.

-record(carbons_sent, {forwarded :: #forwarded{}}).
-type carbons_sent() :: #carbons_sent{}.

-record(mam_archived, {by :: jid:jid(),
                       id = <<>> :: binary()}).
-type mam_archived() :: #mam_archived{}.

-record(p1_rebind, {}).
-type p1_rebind() :: #p1_rebind{}.

-record(compress_failure, {reason :: 'processing-failed' | 'setup-failed' | 'unsupported-method'}).
-type compress_failure() :: #compress_failure{}.

-record(sasl_abort, {}).
-type sasl_abort() :: #sasl_abort{}.

-record(xevent, {offline = false :: boolean(),
                 delivered = false :: boolean(),
                 displayed = false :: boolean(),
                 composing = false :: boolean(),
                 id :: binary()}).
-type xevent() :: #xevent{}.

-record(vcard_email, {home = false :: boolean(),
                      work = false :: boolean(),
                      internet = false :: boolean(),
                      pref = false :: boolean(),
                      x400 = false :: boolean(),
                      userid :: binary()}).
-type vcard_email() :: #vcard_email{}.

-record(db_result, {from = <<>> :: binary(),
                    to = <<>> :: binary(),
                    type :: 'error' | 'invalid' | 'valid',
                    key = <<>> :: binary(),
                    sub_els = [] :: [xmpp_element() | fxml:xmlel()]}).
-type db_result() :: #db_result{}.

-record(carbons_received, {forwarded :: #forwarded{}}).
-type carbons_received() :: #carbons_received{}.

-record(ps_retract, {node = <<>> :: binary(),
                     notify = false :: boolean(),
                     items = [] :: [#ps_item{}]}).
-type ps_retract() :: #ps_retract{}.

-record(upload_slot, {get :: binary(),
                      put :: binary(),
                      xmlns = <<>> :: binary()}).
-type upload_slot() :: #upload_slot{}.

-record(mix_participant, {jid :: jid:jid(),
                          nick = <<>> :: binary()}).
-type mix_participant() :: #mix_participant{}.

-record(vcard_geo, {lat :: binary(),
                    lon :: binary()}).
-type vcard_geo() :: #vcard_geo{}.

-record(compressed, {}).
-type compressed() :: #compressed{}.

-record(sasl_failure, {reason :: 'aborted' | 'account-disabled' | 'bad-protocol' | 'credentials-expired' | 'encryption-required' | 'incorrect-encoding' | 'invalid-authzid' | 'invalid-mechanism' | 'malformed-request' | 'mechanism-too-weak' | 'not-authorized' | 'temporary-auth-failure',
                       text = [] :: [#text{}]}).
-type sasl_failure() :: #sasl_failure{}.

-record(block_list, {items = [] :: [jid:jid()]}).
-type block_list() :: #block_list{}.

-record(upload_request, {filename :: binary(),
                         size :: non_neg_integer(),
                         'content-type' = <<>> :: binary(),
                         xmlns = <<>> :: binary()}).
-type upload_request() :: #upload_request{}.

-record(xdata_option, {label = <<>> :: binary(),
                       value :: binary()}).
-type xdata_option() :: #xdata_option{}.

-record(xdata_field, {label = <<>> :: binary(),
                      type :: 'boolean' | 'fixed' | 'hidden' | 'jid-multi' | 'jid-single' | 'list-multi' | 'list-single' | 'text-multi' | 'text-private' | 'text-single',
                      var = <<>> :: binary(),
                      required = false :: boolean(),
                      desc :: binary(),
                      values = [] :: [binary()],
                      options = [] :: [#xdata_option{}],
                      sub_els = [] :: [xmpp_element() | fxml:xmlel()]}).
-type xdata_field() :: #xdata_field{}.

-record(version, {name :: binary(),
                  ver :: binary(),
                  os :: binary()}).
-type version() :: #version{}.

-record(bind, {jid :: jid:jid(),
               resource :: binary()}).
-type bind() :: #bind{}.

-record(rosterver_feature, {}).
-type rosterver_feature() :: #rosterver_feature{}.

-record(muc_invite, {reason = <<>> :: binary(),
                     from :: jid:jid(),
                     to :: jid:jid(),
                     continue :: binary()}).
-type muc_invite() :: #muc_invite{}.

-record(carbons_disable, {}).
-type carbons_disable() :: #carbons_disable{}.

-record(bytestreams, {hosts = [] :: [#streamhost{}],
                      used :: jid:jid(),
                      activate :: jid:jid(),
                      dstaddr = <<>> :: binary(),
                      mode = tcp :: 'tcp' | 'udp',
                      sid = <<>> :: binary()}).
-type bytestreams() :: #bytestreams{}.

-record(adhoc_actions, {execute :: 'complete' | 'next' | 'prev',
                        prev = false :: boolean(),
                        next = false :: boolean(),
                        complete = false :: boolean()}).
-type adhoc_actions() :: #adhoc_actions{}.

-record(vcard_org, {name :: binary(),
                    units = [] :: [binary()]}).
-type vcard_org() :: #vcard_org{}.

-record(rsm_set, {'after' :: binary(),
                  before :: binary(),
                  count :: non_neg_integer(),
                  first :: #rsm_first{},
                  index :: non_neg_integer(),
                  last :: binary(),
                  max :: non_neg_integer()}).
-type rsm_set() :: #rsm_set{}.

-record(mam_fin, {xmlns = <<>> :: binary(),
                  id = <<>> :: binary(),
                  rsm :: #rsm_set{},
                  stable :: boolean(),
                  complete :: boolean()}).
-type mam_fin() :: #mam_fin{}.

-record(disco_items, {node = <<>> :: binary(),
                      items = [] :: [#disco_item{}],
                      rsm :: #rsm_set{}}).
-type disco_items() :: #disco_items{}.

-record(vcard_tel, {home = false :: boolean(),
                    work = false :: boolean(),
                    voice = false :: boolean(),
                    fax = false :: boolean(),
                    pager = false :: boolean(),
                    msg = false :: boolean(),
                    cell = false :: boolean(),
                    video = false :: boolean(),
                    bbs = false :: boolean(),
                    modem = false :: boolean(),
                    isdn = false :: boolean(),
                    pcs = false :: boolean(),
                    pref = false :: boolean(),
                    number :: binary()}).
-type vcard_tel() :: #vcard_tel{}.

-record(media_uri, {type = <<>> :: binary(),
                    uri = <<>> :: binary()}).
-type media_uri() :: #media_uri{}.

-record(media, {height :: non_neg_integer(),
                width :: non_neg_integer(),
                uri = [] :: [#media_uri{}]}).
-type media() :: #media{}.

-record(muc_destroy, {xmlns = <<>> :: binary(),
                      jid :: jid:jid(),
                      reason = <<>> :: binary(),
                      password :: binary()}).
-type muc_destroy() :: #muc_destroy{}.

-record(muc_user, {decline :: #muc_decline{},
                   destroy :: #muc_destroy{},
                   invites = [] :: [#muc_invite{}],
                   items = [] :: [#muc_item{}],
                   status_codes = [] :: [pos_integer()],
                   password :: binary()}).
-type muc_user() :: #muc_user{}.

-record(vcard_key, {type :: binary(),
                    cred :: binary()}).
-type vcard_key() :: #vcard_key{}.

-record(vcard_name, {family :: binary(),
                     given :: binary(),
                     middle :: binary(),
                     prefix :: binary(),
                     suffix :: binary()}).
-type vcard_name() :: #vcard_name{}.

-record(identity, {category = <<>> :: binary(),
                   type = <<>> :: binary(),
                   lang = <<>> :: binary(),
                   name = <<>> :: binary()}).
-type identity() :: #identity{}.

-record(bookmark_conference, {name = <<>> :: binary(),
                              jid :: jid:jid(),
                              autojoin = false :: boolean(),
                              nick :: binary(),
                              password :: binary()}).
-type bookmark_conference() :: #bookmark_conference{}.

-record(xmpp_session, {optional = false :: boolean()}).
-type xmpp_session() :: #xmpp_session{}.

-record(bookmark_url, {name = <<>> :: binary(),
                       url = <<>> :: binary()}).
-type bookmark_url() :: #bookmark_url{}.

-record(bookmark_storage, {conference = [] :: [#bookmark_conference{}],
                           url = [] :: [#bookmark_url{}]}).
-type bookmark_storage() :: #bookmark_storage{}.

-record(oob_x, {url :: binary(),
                desc = <<>> :: binary(),
                sid = <<>> :: binary()}).
-type oob_x() :: #oob_x{}.

-record(vcard_sound, {phonetic :: binary(),
                      binval :: binary(),
                      extval :: binary()}).
-type vcard_sound() :: #vcard_sound{}.

-record(vcard_photo, {type :: binary(),
                      binval :: binary(),
                      extval :: binary()}).
-type vcard_photo() :: #vcard_photo{}.

-record(vcard_label, {home = false :: boolean(),
                      work = false :: boolean(),
                      postal = false :: boolean(),
                      parcel = false :: boolean(),
                      dom = false :: boolean(),
                      intl = false :: boolean(),
                      pref = false :: boolean(),
                      line = [] :: [binary()]}).
-type vcard_label() :: #vcard_label{}.

-record(vcard_adr, {home = false :: boolean(),
                    work = false :: boolean(),
                    postal = false :: boolean(),
                    parcel = false :: boolean(),
                    dom = false :: boolean(),
                    intl = false :: boolean(),
                    pref = false :: boolean(),
                    pobox :: binary(),
                    extadd :: binary(),
                    street :: binary(),
                    locality :: binary(),
                    region :: binary(),
                    pcode :: binary(),
                    ctry :: binary()}).
-type vcard_adr() :: #vcard_adr{}.

-record(search_item, {jid :: jid:jid(),
                      first :: binary(),
                      last :: binary(),
                      nick :: binary(),
                      email :: binary()}).
-type search_item() :: #search_item{}.

-record(xdata, {type :: 'cancel' | 'form' | 'result' | 'submit',
                instructions = [] :: [binary()],
                title :: binary(),
                reported :: [#xdata_field{}],
                items = [] :: [[#xdata_field{}]],
                fields = [] :: [#xdata_field{}]}).
-type xdata() :: #xdata{}.

-record(xcaptcha, {xdata :: #xdata{}}).
-type xcaptcha() :: #xcaptcha{}.

-record(adhoc_command, {node = <<>> :: binary(),
                        action = execute :: 'cancel' | 'complete' | 'execute' | 'next' | 'prev',
                        sid = <<>> :: binary(),
                        status :: 'canceled' | 'completed' | 'executing',
                        lang = <<>> :: binary(),
                        actions :: #adhoc_actions{},
                        notes = [] :: [#adhoc_note{}],
                        xdata :: #xdata{}}).
-type adhoc_command() :: #adhoc_command{}.

-record(search, {instructions :: binary(),
                 first :: binary(),
                 last :: binary(),
                 nick :: binary(),
                 email :: binary(),
                 items = [] :: [#search_item{}],
                 xdata :: #xdata{}}).
-type search() :: #search{}.

-record(mam_query, {xmlns = <<>> :: binary(),
                    id = <<>> :: binary(),
                    start :: erlang:timestamp(),
                    'end' :: erlang:timestamp(),
                    with :: jid:jid(),
                    withtext :: binary(),
                    rsm :: #rsm_set{},
                    xdata :: #xdata{}}).
-type mam_query() :: #mam_query{}.

-record(pubsub_owner, {affiliations :: {binary(),[#ps_affiliation{}]},
                       configure :: {binary(),'undefined' | #xdata{}},
                       default :: {binary(),'undefined' | #xdata{}},
                       delete :: {binary(),binary()},
                       purge :: binary(),
                       subscriptions :: {binary(),[#ps_subscription{}]}}).
-type pubsub_owner() :: #pubsub_owner{}.

-record(ps_options, {node = <<>> :: binary(),
                     jid :: jid:jid(),
                     subid = <<>> :: binary(),
                     xdata :: #xdata{}}).
-type ps_options() :: #ps_options{}.

-record(pubsub, {subscriptions :: {binary(),[#ps_subscription{}]},
                 subscription :: #ps_subscription{},
                 affiliations :: {binary(),[#ps_affiliation{}]},
                 publish :: #ps_publish{},
                 publish_options :: #xdata{},
                 subscribe :: #ps_subscribe{},
                 unsubscribe :: #ps_unsubscribe{},
                 options :: #ps_options{},
                 items :: #ps_items{},
                 retract :: #ps_retract{},
                 create :: binary(),
                 configure :: {binary(),'undefined' | #xdata{}},
                 default :: {binary(),'undefined' | #xdata{}},
                 delete :: {binary(),binary()},
                 purge :: binary(),
                 rsm :: #rsm_set{}}).
-type pubsub() :: #pubsub{}.

-record(ps_event, {items :: #ps_items{},
                   purge :: binary(),
                   subscription :: #ps_subscription{},
                   delete :: {binary(),binary()},
                   create :: binary(),
                   configuration :: {binary(),'undefined' | #xdata{}}}).
-type ps_event() :: #ps_event{}.

-record(register, {registered = false :: boolean(),
                   remove = false :: boolean(),
                   instructions :: binary(),
                   username :: binary(),
                   nick :: binary(),
                   password :: binary(),
                   name :: binary(),
                   first :: binary(),
                   last :: binary(),
                   email :: binary(),
                   address :: binary(),
                   city :: binary(),
                   state :: binary(),
                   zip :: binary(),
                   phone :: binary(),
                   url :: binary(),
                   date :: binary(),
                   misc :: binary(),
                   text :: binary(),
                   key :: binary(),
                   xdata :: #xdata{},
                   sub_els = [] :: [xmpp_element() | fxml:xmlel()]}).
-type register() :: #register{}.

-record(disco_info, {node = <<>> :: binary(),
                     identities = [] :: [#identity{}],
                     features = [] :: [binary()],
                     xdata = [] :: [#xdata{}]}).
-type disco_info() :: #disco_info{}.

-record(offline_item, {node = <<>> :: binary(),
                       action :: 'remove' | 'view'}).
-type offline_item() :: #offline_item{}.

-record(offline, {items = [] :: [#offline_item{}],
                  purge = false :: boolean(),
                  fetch = false :: boolean()}).
-type offline() :: #offline{}.

-record(muc_owner, {destroy :: #muc_destroy{},
                    config :: #xdata{},
                    items = [] :: [#muc_item{}]}).
-type muc_owner() :: #muc_owner{}.

-record(sasl_mechanisms, {list = [] :: [binary()]}).
-type sasl_mechanisms() :: #sasl_mechanisms{}.

-record(sm_failed, {reason :: atom() | #gone{} | #redirect{},
                    h :: non_neg_integer(),
                    xmlns = <<>> :: binary()}).
-type sm_failed() :: #sm_failed{}.

-record(stanza_error, {type :: 'auth' | 'cancel' | 'continue' | 'modify' | 'wait',
                       code :: non_neg_integer(),
                       by = <<>> :: binary(),
                       reason :: atom() | #gone{} | #redirect{},
                       text :: #text{},
                       sub_els = [] :: [xmpp_element() | fxml:xmlel()]}).
-type stanza_error() :: #stanza_error{}.

-record(mix_join, {jid :: jid:jid(),
                   subscribe = [] :: [binary()]}).
-type mix_join() :: #mix_join{}.

-record(privacy_item, {order :: non_neg_integer(),
                       action :: 'allow' | 'deny',
                       type :: 'group' | 'jid' | 'subscription',
                       value = <<>> :: binary(),
                       message = false :: boolean(),
                       iq = false :: boolean(),
                       presence_in = false :: boolean(),
                       presence_out = false :: boolean()}).
-type privacy_item() :: #privacy_item{}.

-record(privacy_list, {name = <<>> :: binary(),
                       items = [] :: [#privacy_item{}]}).
-type privacy_list() :: #privacy_list{}.

-record(privacy_query, {lists = [] :: [#privacy_list{}],
                        default :: 'none' | binary(),
                        active :: 'none' | binary()}).
-type privacy_query() :: #privacy_query{}.

-record(stream_error, {reason :: atom() | #'see-other-host'{},
                       text :: #text{}}).
-type stream_error() :: #stream_error{}.

-record(vcard_logo, {type :: binary(),
                     binval :: binary(),
                     extval :: binary()}).
-type vcard_logo() :: #vcard_logo{}.

-record(vcard_temp, {version :: binary(),
                     fn :: binary(),
                     n :: #vcard_name{},
                     nickname :: binary(),
                     photo :: #vcard_photo{},
                     bday :: binary(),
                     adr = [] :: [#vcard_adr{}],
                     label = [] :: [#vcard_label{}],
                     tel = [] :: [#vcard_tel{}],
                     email = [] :: [#vcard_email{}],
                     jabberid :: binary(),
                     mailer :: binary(),
                     tz :: binary(),
                     geo :: #vcard_geo{},
                     title :: binary(),
                     role :: binary(),
                     logo :: #vcard_logo{},
                     org :: #vcard_org{},
                     categories = [] :: [binary()],
                     note :: binary(),
                     prodid :: binary(),
                     rev :: binary(),
                     sort_string :: binary(),
                     sound :: #vcard_sound{},
                     uid :: binary(),
                     url :: binary(),
                     class :: 'confidential' | 'private' | 'public',
                     key :: #vcard_key{},
                     desc :: binary()}).
-type vcard_temp() :: #vcard_temp{}.

-record(time, {tzo :: {integer(),integer()},
               utc :: erlang:timestamp()}).
-type time() :: #time{}.

-type xmpp_element() :: muc_admin() |
                        compression() |
                        ps_subscription() |
                        xdata_option() |
                        version() |
                        ps_affiliation() |
                        mam_fin() |
                        sm_a() |
                        bob_data() |
                        media() |
                        stanza_id() |
                        starttls_proceed() |
                        client_id() |
                        sm_resumed() |
                        forwarded() |
                        xevent() |
                        privacy_list() |
                        carbons_sent() |
                        mam_archived() |
                        p1_rebind() |
                        sasl_abort() |
                        db_result() |
                        carbons_received() |
                        upload_slot() |
                        mix_participant() |
                        compressed() |
                        block_list() |
                        rsm_set() |
                        'see-other-host'() |
                        hint() |
                        stream_start() |
                        text() |
                        vcard_org() |
                        shim() |
                        search_item() |
                        offline_item() |
                        feature_sm() |
                        roster_item() |
                        muc_item() |
                        vcard_temp() |
                        address() |
                        sasl_success() |
                        addresses() |
                        muc_subscriptions() |
                        disco_items() |
                        compress() |
                        bytestreams() |
                        adhoc_actions() |
                        muc_history() |
                        identity() |
                        feature_csi() |
                        privacy_query() |
                        delay() |
                        thumbnail() |
                        vcard_tel() |
                        vcard_geo() |
                        vcard_photo() |
                        pubsub_owner() |
                        pubsub() |
                        muc_owner() |
                        muc_actor() |
                        ps_error() |
                        starttls_failure() |
                        sasl_challenge() |
                        x_conference() |
                        private() |
                        sasl_failure() |
                        vcard_name() |
                        adhoc_note() |
                        rosterver_feature() |
                        muc_invite() |
                        vcard_xupdate() |
                        carbons_disable() |
                        bookmark_conference() |
                        offline() |
                        time() |
                        ps_subscribe() |
                        sm_enable() |
                        handshake() |
                        compress_failure() |
                        bookmark_storage() |
                        muc_decline() |
                        legacy_auth() |
                        search() |
                        ps_publish() |
                        nick() |
                        p1_ack() |
                        block() |
                        mix_join() |
                        xmpp_session() |
                        xdata() |
                        iq() |
                        xcaptcha() |
                        streamhost() |
                        bind() |
                        ps_retract() |
                        last() |
                        redirect() |
                        sm_enabled() |
                        vcard_sound() |
                        ps_event() |
                        mam_result() |
                        rsm_first() |
                        stat() |
                        upload_request() |
                        xdata_field() |
                        adhoc_command() |
                        sm_failed() |
                        ping() |
                        privacy_item() |
                        disco_item() |
                        ps_item() |
                        mam_prefs() |
                        sasl_mechanisms() |
                        caps() |
                        muc() |
                        stream_features() |
                        stats() |
                        ps_items() |
                        sic() |
                        ps_options() |
                        starttls() |
                        media_uri() |
                        muc_destroy() |
                        vcard_key() |
                        csi() |
                        db_verify() |
                        roster_query() |
                        mam_query() |
                        bookmark_url() |
                        vcard_email() |
                        vcard_label() |
                        vcard_logo() |
                        disco_info() |
                        feature_register() |
                        register() |
                        sm_r() |
                        stat_error() |
                        stanza_error() |
                        stream_error() |
                        muc_user() |
                        vcard_adr() |
                        gone() |
                        carbons_private() |
                        mix_leave() |
                        muc_subscribe() |
                        muc_unique() |
                        sasl_response() |
                        message() |
                        presence() |
                        sm_resume() |
                        carbons_enable() |
                        expire() |
                        muc_unsubscribe() |
                        ps_unsubscribe() |
                        chatstate() |
                        sasl_auth() |
                        p1_push() |
                        oob_x() |
                        unblock().
