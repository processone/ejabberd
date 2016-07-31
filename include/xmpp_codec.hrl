%% Created automatically by XML generator (fxml_gen.erl)
%% Source: xmpp_codec.spec

-record(vcard_xupdate, {us :: {binary(), binary()},
			hash :: binary()}).
-type vcard_xupdate() :: #vcard_xupdate{}.

-record(chatstate, {type :: active | composing | gone | inactive | paused}).
-type chatstate() :: #chatstate{}.

-record(csi, {type :: active | inactive}).
-type csi() :: #csi{}.

-record(hint, {type :: 'no-copy' | 'no-store' | 'no-storage' | 'store' |
		       'no-permanent-store' | 'no-permanent-storage'}).
-type hint() :: #hint{}.

-record(iq, {id :: binary(),
             type :: 'error' | 'get' | 'result' | 'set',
             lang :: binary(),
             from :: any(),
             to :: any(),
             sub_els = [] :: [any()]}).
-type iq() :: #iq{}.

-record(feature_register, {}).
-type feature_register() :: #feature_register{}.

-record(adhoc_note, {type = info :: 'error' | 'info' | 'warn',
                     data = <<>> :: binary()}).
-type adhoc_note() :: #adhoc_note{}.

-record(address, {type :: 'bcc' | 'cc' | 'noreply' | 'ofrom' | 'replyroom' | 'replyto' | 'to',
                  jid :: any(),
                  desc :: binary(),
                  node :: binary(),
                  delivered :: any()}).
-type address() :: #address{}.

-record(sasl_success, {text :: any()}).
-type sasl_success() :: #sasl_success{}.

-record(mam_result, {xmlns :: binary(),
                     queryid :: binary(),
                     id :: binary(),
                     sub_els = [] :: [any()]}).
-type mam_result() :: #mam_result{}.

-record(rsm_first, {index :: non_neg_integer(),
                    data :: binary()}).
-type rsm_first() :: #rsm_first{}.

-record(text, {lang :: binary(),
               data :: binary()}).
-type text() :: #text{}.

-record(streamhost, {jid :: any(),
                     host :: binary(),
                     port = 1080 :: non_neg_integer()}).
-type streamhost() :: #streamhost{}.

-record(sm_resume, {h :: non_neg_integer(),
                    previd :: binary(),
                    xmlns :: binary()}).
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

-record(pubsub_unsubscribe, {node :: binary(),
                             jid :: any(),
                             subid :: binary()}).
-type pubsub_unsubscribe() :: #pubsub_unsubscribe{}.

-record(mix_leave, {}).
-type mix_leave() :: #mix_leave{}.

-record(ping, {}).
-type ping() :: #ping{}.

-record(delay, {stamp :: any(),
                from :: any(),
                desc = <<>> :: binary()}).
-type delay() :: #delay{}.

-record(muc_history, {maxchars :: non_neg_integer(),
                      maxstanzas :: non_neg_integer(),
                      seconds :: non_neg_integer(),
                      since :: any()}).
-type muc_history() :: #muc_history{}.

-record(thumbnail, {uri :: binary(),
                    'media-type' = <<>> :: binary(),
                    width :: non_neg_integer(),
                    height :: non_neg_integer()}).
-type thumbnail() :: #thumbnail{}.

-record(pubsub_affiliation, {node :: binary(),
                             type :: 'member' | 'none' | 'outcast' | 'owner' | 'publish-only' | 'publisher'}).
-type pubsub_affiliation() :: #pubsub_affiliation{}.

-record(muc_decline, {reason = <<>> :: 'undefined' | binary(),
                      from :: any(),
                      to :: any()}).
-type muc_decline() :: #muc_decline{}.

-record(sm_a, {h :: non_neg_integer(),
               xmlns :: binary()}).
-type sm_a() :: #sm_a{}.

-record(stream_start, {from :: any(),
                       to :: any(),
                       id = <<>> :: binary(),
                       version = <<>> :: binary(),
                       xmlns :: binary(),
                       stream_xmlns = <<>> :: binary(),
                       db_xmlns = <<>> :: binary(),
                       lang = <<>> :: binary()}).
-type stream_start() :: #stream_start{}.

-record(muc_subscribe, {nick :: binary(),
                        events = [] :: [binary()]}).
-type muc_subscribe() :: #muc_subscribe{}.

-record(stanza_id, {by :: any(),
                    id :: binary()}).
-type stanza_id() :: #stanza_id{}.

-record(starttls_proceed, {}).
-type starttls_proceed() :: #starttls_proceed{}.

-record(client_id, {id :: binary()}).
-type client_id() :: #client_id{}.

-record(sm_resumed, {h :: non_neg_integer(),
                     previd :: binary(),
                     xmlns :: binary()}).
-type sm_resumed() :: #sm_resumed{}.

-record(forwarded, {delay :: #delay{},
                    sub_els = [] :: [any()]}).
-type forwarded() :: #forwarded{}.

-record(sm_enable, {max :: non_neg_integer(),
                    resume = false :: any(),
                    xmlns :: binary()}).
-type sm_enable() :: #sm_enable{}.

-record(starttls_failure, {}).
-type starttls_failure() :: #starttls_failure{}.

-record(sasl_challenge, {text :: any()}).
-type sasl_challenge() :: #sasl_challenge{}.

-record(handshake, {data = <<>> :: binary()}).
-type handshake() :: #handshake{}.

-record(gone, {uri :: binary()}).
-type gone() :: #gone{}.

-record(x_conference, {jid :: any(),
                       password = <<>> :: binary(),
                       reason = <<>> :: binary(),
                       continue :: any(),
                       thread = <<>> :: binary()}).
-type x_conference() :: #x_conference{}.

-record(private, {xml_els = [] :: [any()]}).
-type private() :: #private{}.

-record(db_verify, {from :: any(),
                    to :: any(),
                    id :: binary(),
                    type :: 'error' | 'invalid' | 'valid',
                    key = <<>> :: binary(),
                    sub_els = [] :: [any()]}).
-type db_verify() :: #db_verify{}.

-record(nick, {name :: binary()}).
-type nick() :: #nick{}.

-record(p1_ack, {}).
-type p1_ack() :: #p1_ack{}.

-record(feature_sm, {xmlns :: binary()}).
-type feature_sm() :: #feature_sm{}.

-record(pubsub_item, {id :: binary(),
                      xml_els = [] :: [any()]}).
-type pubsub_item() :: #pubsub_item{}.

-record(pubsub_publish, {node :: binary(),
                         items = [] :: [#pubsub_item{}]}).
-type pubsub_publish() :: #pubsub_publish{}.

-record(roster_item, {jid :: any(),
                      name = <<>> :: binary(),
                      groups = [] :: [binary()],
                      subscription = none :: 'both' | 'from' | 'none' | 'remove' | 'to',
                      ask :: 'subscribe'}).
-type roster_item() :: #roster_item{}.

-record(roster_query, {items = [] :: [#roster_item{}],
                       ver :: binary()}).
-type roster_query() :: #roster_query{}.

-record(pubsub_event_item, {id :: binary(),
                            node :: binary(),
                            publisher :: binary(),
                            xml_els = [] :: [any()]}).
-type pubsub_event_item() :: #pubsub_event_item{}.

-record(sm_r, {xmlns :: binary()}).
-type sm_r() :: #sm_r{}.

-record(muc_actor, {jid :: any(),
                    nick :: binary()}).
-type muc_actor() :: #muc_actor{}.

-record(stat_error, {code :: integer(),
                     reason = <<>> :: binary()}).
-type stat_error() :: #stat_error{}.

-record(stat, {name :: binary(),
               units = <<>> :: binary(),
               value = <<>> :: binary(),
               error :: #stat_error{}}).
-type stat() :: #stat{}.

-record(addresses, {list = [] :: [#address{}]}).
-type addresses() :: #addresses{}.

-record('see-other-host', {host :: binary()}).
-type 'see-other-host'() :: #'see-other-host'{}.

-record(compress, {methods = [] :: [binary()]}).
-type compress() :: #compress{}.

-record(starttls, {required = false :: boolean()}).
-type starttls() :: #starttls{}.

-record(last, {seconds :: non_neg_integer(),
               status = <<>> :: binary()}).
-type last() :: #last{}.

-record(redirect, {uri :: binary()}).
-type redirect() :: #redirect{}.

-record(sm_enabled, {id :: binary(),
                     location :: binary(),
                     max :: non_neg_integer(),
                     resume = false :: any(),
                     xmlns :: binary()}).
-type sm_enabled() :: #sm_enabled{}.

-record(pubsub_event_items, {node :: binary(),
                             retract = [] :: [binary()],
                             items = [] :: [#pubsub_event_item{}]}).
-type pubsub_event_items() :: #pubsub_event_items{}.

-record(pubsub_event, {items = [] :: [#pubsub_event_items{}]}).
-type pubsub_event() :: #pubsub_event{}.

-record(muc_unique, {name = <<>> :: binary()}).
-type muc_unique() :: #muc_unique{}.

-record(sasl_response, {text :: any()}).
-type sasl_response() :: #sasl_response{}.

-record(legacy_auth, {username :: 'none' | binary(),
                      password :: 'none' | binary(),
                      digest :: 'none' | binary(),
                      resource :: 'none' | binary()}).
-type legacy_auth() :: #legacy_auth{}.

-record(pubsub_subscribe, {node :: binary(),
                           jid :: any()}).
-type pubsub_subscribe() :: #pubsub_subscribe{}.

-record(message, {id :: binary(),
                  type = normal :: 'chat' | 'error' | 'groupchat' | 'headline' | 'normal',
                  lang :: binary(),
                  from :: any(),
                  to :: any(),
                  subject = [] :: [#text{}],
                  body = [] :: [#text{}],
                  thread :: binary(),
                  sub_els = [] :: [any()]}).
-type message() :: #message{}.

-record(sasl_auth, {mechanism :: binary(),
                    text :: any()}).
-type sasl_auth() :: #sasl_auth{}.

-record(p1_push, {}).
-type p1_push() :: #p1_push{}.

-record(feature_csi, {xmlns :: binary()}).
-type feature_csi() :: #feature_csi{}.

-record(disco_item, {jid :: any(),
                     name :: binary(),
                     node :: binary()}).
-type disco_item() :: #disco_item{}.

-record(unblock, {items = [] :: [any()]}).
-type unblock() :: #unblock{}.

-record(block, {items = [] :: [any()]}).
-type block() :: #block{}.

-record(compression, {methods = [] :: [binary()]}).
-type compression() :: #compression{}.

-record(muc_subscriptions, {list = [] :: [any()]}).
-type muc_subscriptions() :: #muc_subscriptions{}.

-record(pubsub_subscription, {jid :: any(),
                              node :: binary(),
                              subid :: binary(),
                              type :: 'none' | 'pending' | 'subscribed' | 'unconfigured'}).
-type pubsub_subscription() :: #pubsub_subscription{}.

-record(bob_data, {cid :: binary(),
                   'max-age' :: non_neg_integer(),
                   type :: binary(),
                   data = <<>> :: any()}).
-type bob_data() :: #bob_data{}.

-record(muc_item, {actor :: #muc_actor{},
                   continue :: binary(),
                   reason = <<>> :: 'undefined' | binary(),
                   affiliation :: 'admin' | 'member' | 'none' | 'outcast' | 'owner',
                   role :: 'moderator' | 'none' | 'participant' | 'visitor',
                   jid :: any(),
                   nick :: binary()}).
-type muc_item() :: #muc_item{}.

-record(muc_admin, {items = [] :: [#muc_item{}]}).
-type muc_admin() :: #muc_admin{}.

-record(shim, {headers = [] :: [{binary(),'undefined' | binary()}]}).
-type shim() :: #shim{}.

-record(mam_prefs, {xmlns :: binary(),
                    default :: 'always' | 'never' | 'roster',
                    always :: [any()],
                    never :: [any()]}).
-type mam_prefs() :: #mam_prefs{}.

-record(caps, {node :: binary(),
               version :: binary(),
               hash :: binary(),
               exts = [] :: any()}).
-type caps() :: #caps{}.

-record(muc, {history :: #muc_history{},
              password :: binary()}).
-type muc() :: #muc{}.

-record(stream_features, {sub_els = [] :: [any()]}).
-type stream_features() :: #stream_features{}.

-record(stats, {list = [] :: [#stat{}],
                node = <<>> :: binary()}).
-type stats() :: #stats{}.

-record(pubsub_items, {node :: binary(),
                       max_items :: non_neg_integer(),
                       subid :: binary(),
                       items = [] :: [#pubsub_item{}]}).
-type pubsub_items() :: #pubsub_items{}.

-record(presence, {id :: binary(),
                   type = available :: 'available' | 'error' | 'probe' | 'subscribe' | 'subscribed' | 'unavailable' | 'unsubscribe' | 'unsubscribed',
                   lang :: binary(),
                   from :: any(),
                   to :: any(),
                   show :: 'away' | 'chat' | 'dnd' | 'xa',
                   status = [] :: [#text{}],
                   priority :: integer(),
                   sub_els = [] :: [any()]}).
-type presence() :: #presence{}.

-record(sic, {ip :: any(),
              port :: non_neg_integer(),
              xmlns :: binary()}).
-type sic() :: #sic{}.

-record(carbons_sent, {forwarded :: #forwarded{}}).
-type carbons_sent() :: #carbons_sent{}.

-record(mam_archived, {by :: any(),
                       id :: binary()}).
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

-record(db_result, {from :: any(),
                    to :: any(),
                    type :: 'error' | 'invalid' | 'valid',
                    key = <<>> :: binary(),
                    sub_els = [] :: [any()]}).
-type db_result() :: #db_result{}.

-record(carbons_received, {forwarded :: #forwarded{}}).
-type carbons_received() :: #carbons_received{}.

-record(pubsub_retract, {node :: binary(),
                         notify = false :: any(),
                         items = [] :: [#pubsub_item{}]}).
-type pubsub_retract() :: #pubsub_retract{}.

-record(upload_slot, {get :: binary(),
                      put :: binary(),
                      xmlns :: binary()}).
-type upload_slot() :: #upload_slot{}.

-record(mix_participant, {jid :: any(),
                          nick :: binary()}).
-type mix_participant() :: #mix_participant{}.

-record(vcard_geo, {lat :: binary(),
                    lon :: binary()}).
-type vcard_geo() :: #vcard_geo{}.

-record(compressed, {}).
-type compressed() :: #compressed{}.

-record(sasl_failure, {reason :: 'aborted' | 'account-disabled' | 'bad-protocol' | 'credentials-expired' | 'encryption-required' | 'incorrect-encoding' | 'invalid-authzid' | 'invalid-mechanism' | 'malformed-request' | 'mechanism-too-weak' | 'not-authorized' | 'temporary-auth-failure',
                       text = [] :: [#text{}]}).
-type sasl_failure() :: #sasl_failure{}.

-record(block_list, {items = [] :: [any()]}).
-type block_list() :: #block_list{}.

-record(upload_request, {filename :: binary(),
                         size :: non_neg_integer(),
                         'content-type' = <<>> :: binary(),
                         xmlns :: binary()}).
-type upload_request() :: #upload_request{}.

-record(xdata_option, {label :: binary(),
                       value :: binary()}).
-type xdata_option() :: #xdata_option{}.

-record(xdata_field, {label :: binary(),
                      type :: 'boolean' | 'fixed' | 'hidden' | 'jid-multi' | 'jid-single' | 'list-multi' | 'list-single' | 'text-multi' | 'text-private' | 'text-single',
                      var :: binary(),
                      required = false :: boolean(),
                      desc :: binary(),
                      values = [] :: [binary()],
                      options = [] :: [#xdata_option{}],
                      sub_els = [] :: [any()]}).
-type xdata_field() :: #xdata_field{}.

-record(version, {name :: binary(),
                  ver :: binary(),
                  os :: binary()}).
-type version() :: #version{}.

-record(bind, {jid :: any(),
               resource :: any()}).
-type bind() :: #bind{}.

-record(rosterver_feature, {}).
-type rosterver_feature() :: #rosterver_feature{}.

-record(muc_invite, {reason = <<>> :: 'undefined' | binary(),
                     from :: any(),
                     to :: any(),
                     continue :: binary()}).
-type muc_invite() :: #muc_invite{}.

-record(carbons_disable, {}).
-type carbons_disable() :: #carbons_disable{}.

-record(bytestreams, {hosts = [] :: [#streamhost{}],
                      used :: any(),
                      activate :: any(),
                      dstaddr :: binary(),
                      mode = tcp :: 'tcp' | 'udp',
                      sid :: binary()}).
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

-record(mam_fin, {id :: binary(),
                  rsm :: #rsm_set{},
                  stable :: any(),
                  complete :: any()}).
-type mam_fin() :: #mam_fin{}.

-record(disco_items, {node :: binary(),
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

-record(media_uri, {type :: binary(),
                    uri = <<>> :: binary()}).
-type media_uri() :: #media_uri{}.

-record(media, {height :: non_neg_integer(),
                width :: non_neg_integer(),
                uri = [] :: [#media_uri{}]}).
-type media() :: #media{}.

-record(muc_destroy, {xmlns :: binary(),
                      jid :: any(),
                      reason = <<>> :: 'undefined' | binary(),
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

-record(identity, {category :: binary(),
                   type :: binary(),
                   lang :: binary(),
                   name :: binary()}).
-type identity() :: #identity{}.

-record(bookmark_conference, {name :: binary(),
                              jid :: any(),
                              autojoin = false :: any(),
                              nick :: binary(),
                              password :: binary()}).
-type bookmark_conference() :: #bookmark_conference{}.

-record(xmpp_session, {optional = false :: boolean()}).
-type xmpp_session() :: #xmpp_session{}.

-record(bookmark_url, {name :: binary(),
                       url :: binary()}).
-type bookmark_url() :: #bookmark_url{}.

-record(bookmark_storage, {conference = [] :: [#bookmark_conference{}],
                           url = [] :: [#bookmark_url{}]}).
-type bookmark_storage() :: #bookmark_storage{}.

-record(oob_x, {url :: binary(),
                desc = <<>> :: binary(),
                sid = <<>> :: binary()}).
-type oob_x() :: #oob_x{}.

-record(vcard_sound, {phonetic :: binary(),
                      binval :: any(),
                      extval :: binary()}).
-type vcard_sound() :: #vcard_sound{}.

-record(vcard_photo, {type :: binary(),
                      binval :: any(),
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

-record(search_item, {jid :: any(),
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

-record(adhoc_command, {node :: binary(),
                        action = execute :: 'cancel' | 'complete' | 'execute' | 'next' | 'prev',
                        sid :: binary(),
                        status :: 'canceled' | 'completed' | 'executing',
                        lang :: binary(),
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

-record(mam_query, {xmlns :: binary(),
                    id :: binary(),
                    start :: any(),
                    'end' :: any(),
                    with :: any(),
                    withtext :: binary(),
                    rsm :: #rsm_set{},
                    xdata :: #xdata{}}).
-type mam_query() :: #mam_query{}.

-record(pubsub_options, {node :: binary(),
                         jid :: any(),
                         subid :: binary(),
                         xdata :: #xdata{}}).
-type pubsub_options() :: #pubsub_options{}.

-record(pubsub, {subscriptions :: {'none' | binary(),[#pubsub_subscription{}]},
                 affiliations :: [#pubsub_affiliation{}],
                 publish :: #pubsub_publish{},
                 subscribe :: #pubsub_subscribe{},
                 unsubscribe :: #pubsub_unsubscribe{},
                 options :: #pubsub_options{},
                 items :: #pubsub_items{},
                 retract :: #pubsub_retract{}}).
-type pubsub() :: #pubsub{}.

-record(register, {registered = false :: boolean(),
                   remove = false :: boolean(),
                   instructions :: binary(),
                   username :: 'none' | binary(),
                   nick :: 'none' | binary(),
                   password :: 'none' | binary(),
                   name :: 'none' | binary(),
                   first :: 'none' | binary(),
                   last :: 'none' | binary(),
                   email :: 'none' | binary(),
                   address :: 'none' | binary(),
                   city :: 'none' | binary(),
                   state :: 'none' | binary(),
                   zip :: 'none' | binary(),
                   phone :: 'none' | binary(),
                   url :: 'none' | binary(),
                   date :: 'none' | binary(),
                   misc :: 'none' | binary(),
                   text :: 'none' | binary(),
                   key :: 'none' | binary(),
                   xdata :: #xdata{},
                   sub_els = [] :: [any()]}).
-type register() :: #register{}.

-record(disco_info, {node :: binary(),
                     identities = [] :: [#identity{}],
                     features = [] :: [binary()],
                     xdata = [] :: [#xdata{}]}).
-type disco_info() :: #disco_info{}.

-record(offline_item, {node :: binary(),
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
                    xmlns :: binary()}).
-type sm_failed() :: #sm_failed{}.

-record(error, {type :: 'auth' | 'cancel' | 'continue' | 'modify' | 'wait',
                code :: non_neg_integer(),
                by :: binary(),
                reason :: atom() | #gone{} | #redirect{},
                text :: #text{},
                sub_els = [] :: [any()]}).
-type error() :: #error{}.

-record(mix_join, {jid :: any(),
                   subscribe = [] :: [binary()]}).
-type mix_join() :: #mix_join{}.

-record(privacy_item, {order :: non_neg_integer(),
                       action :: 'allow' | 'deny',
                       type :: 'group' | 'jid' | 'subscription',
                       value :: binary(),
                       message = false :: boolean(),
                       iq = false :: boolean(),
                       presence_in = false :: boolean(),
                       presence_out = false :: boolean()}).
-type privacy_item() :: #privacy_item{}.

-record(privacy_list, {name :: binary(),
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
                     binval :: any(),
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

-record(time, {tzo :: any(),
               utc :: any()}).
-type time() :: #time{}.

-type xmpp_element() :: muc_admin() |
                        compression() |
                        pubsub_subscription() |
                        xdata_option() |
                        version() |
                        pubsub_affiliation() |
                        mam_fin() |
                        sm_a() |
                        bob_data() |
                        media() |
                        carbons_sent() |
                        mam_archived() |
                        p1_rebind() |
                        sasl_abort() |
                        db_result() |
                        carbons_received() |
                        pubsub_retract() |
                        upload_slot() |
                        mix_participant() |
                        compressed() |
                        block_list() |
                        rsm_set() |
                        'see-other-host'() |
                        hint() |
                        stream_start() |
                        stanza_id() |
                        starttls_proceed() |
                        client_id() |
                        sm_resumed() |
                        forwarded() |
                        xevent() |
                        privacy_list() |
                        text() |
                        vcard_org() |
                        shim() |
                        search_item() |
                        offline_item() |
                        feature_sm() |
                        pubsub_item() |
                        roster_item() |
                        pubsub_event_item() |
                        muc_item() |
                        vcard_temp() |
                        address() |
                        sasl_success() |
                        addresses() |
                        pubsub_event_items() |
                        muc_subscriptions() |
                        disco_items() |
                        pubsub_options() |
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
                        pubsub() |
                        muc_owner() |
                        muc_actor() |
                        vcard_name() |
                        adhoc_note() |
                        rosterver_feature() |
                        muc_invite() |
                        vcard_xupdate() |
                        carbons_disable() |
                        bookmark_conference() |
                        offline() |
                        time() |
                        sm_enable() |
                        starttls_failure() |
                        sasl_challenge() |
                        handshake() |
                        x_conference() |
                        private() |
                        compress_failure() |
                        sasl_failure() |
                        bookmark_storage() |
                        muc_decline() |
                        legacy_auth() |
                        search() |
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
                        last() |
                        redirect() |
                        sm_enabled() |
                        pubsub_event() |
                        vcard_sound() |
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
                        caps() |
                        muc() |
                        stream_features() |
                        stats() |
                        pubsub_items() |
                        sic() |
                        starttls() |
                        mam_prefs() |
                        sasl_mechanisms() |
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
                        error() |
                        stream_error() |
                        muc_user() |
                        vcard_adr() |
                        carbons_private() |
                        mix_leave() |
                        muc_subscribe() |
                        muc_unique() |
                        sasl_response() |
                        pubsub_subscribe() |
                        message() |
                        presence() |
                        gone() |
                        sm_resume() |
                        carbons_enable() |
                        expire() |
                        muc_unsubscribe() |
                        pubsub_unsubscribe() |
                        chatstate() |
                        sasl_auth() |
                        p1_push() |
                        oob_x() |
                        pubsub_publish() |
                        unblock().
