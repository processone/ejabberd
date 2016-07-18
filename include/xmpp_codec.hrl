%% Created automatically by XML generator (fxml_gen.erl)
%% Source: xmpp_codec.spec

-record(vcard_xupdate, {us :: {binary(), binary()},
			hash :: binary()}).
-type vcard_xupdate() :: #vcard_xupdate{}.

-record(chatstate, {type :: active | composing | gone | inactive | paused}).
-type chatstate() :: #chatstate{}.

-record(csi, {type :: active | inactive}).
-type csi() :: #csi{}.

-record(hint, {type :: 'no-copy' | 'no-store' | 'store' | 'no-permanent-store'}).
-type hint() :: #hint{}.

-record(feature_register, {}).
-type feature_register() :: #feature_register{}.

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

-record(pubsub_affiliation, {node :: binary(),
                             type :: 'member' | 'none' | 'outcast' | 'owner' | 'publish-only' | 'publisher'}).
-type pubsub_affiliation() :: #pubsub_affiliation{}.

-record(muc_decline, {reason :: binary(),
                      from :: any(),
                      to :: any()}).
-type muc_decline() :: #muc_decline{}.

-record(sm_a, {h :: non_neg_integer(),
               xmlns :: binary()}).
-type sm_a() :: #sm_a{}.

-record(starttls_proceed, {}).
-type starttls_proceed() :: #starttls_proceed{}.

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

-record(gone, {uri :: binary()}).
-type gone() :: #gone{}.

-record(private, {xml_els = [] :: [any()]}).
-type private() :: #private{}.

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

-record(stat, {name :: binary(),
               units :: binary(),
               value :: binary(),
               error = [] :: [{integer(),'undefined' | binary()}]}).
-type stat() :: #stat{}.

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

-record(sasl_auth, {mechanism :: binary(),
                    text :: any()}).
-type sasl_auth() :: #sasl_auth{}.

-record(p1_push, {}).
-type p1_push() :: #p1_push{}.

-record(feature_csi, {xmlns :: binary()}).
-type feature_csi() :: #feature_csi{}.

-record(muc_user_destroy, {reason :: binary(),
                           jid :: any()}).
-type muc_user_destroy() :: #muc_user_destroy{}.

-record(disco_item, {jid :: any(),
                     name :: binary(),
                     node :: binary()}).
-type disco_item() :: #disco_item{}.

-record(disco_items, {node :: binary(),
                      items = [] :: [#disco_item{}]}).
-type disco_items() :: #disco_items{}.

-record(unblock, {items = [] :: [any()]}).
-type unblock() :: #unblock{}.

-record(block, {items = [] :: [any()]}).
-type block() :: #block{}.

-record(compression, {methods = [] :: [binary()]}).
-type compression() :: #compression{}.

-record(muc_owner_destroy, {jid :: any(),
                            reason :: binary(),
                            password :: binary()}).
-type muc_owner_destroy() :: #muc_owner_destroy{}.

-record(pubsub_subscription, {jid :: any(),
                              node :: binary(),
                              subid :: binary(),
                              type :: 'none' | 'pending' | 'subscribed' | 'unconfigured'}).
-type pubsub_subscription() :: #pubsub_subscription{}.

-record(muc_item, {actor :: #muc_actor{},
                   continue :: binary(),
                   reason :: binary(),
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
                    always = [] :: [any()],
                    never = [] :: [any()]}).
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

-record(stats, {stat = [] :: [#stat{}]}).
-type stats() :: #stats{}.

-record(pubsub_items, {node :: binary(),
                       max_items :: non_neg_integer(),
                       subid :: binary(),
                       items = [] :: [#pubsub_item{}]}).
-type pubsub_items() :: #pubsub_items{}.

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

-record(vcard_email, {home = false :: boolean(),
                      work = false :: boolean(),
                      internet = false :: boolean(),
                      pref = false :: boolean(),
                      x400 = false :: boolean(),
                      userid :: binary()}).
-type vcard_email() :: #vcard_email{}.

-record(carbons_received, {forwarded :: #forwarded{}}).
-type carbons_received() :: #carbons_received{}.

-record(pubsub_retract, {node :: binary(),
                         notify = false :: any(),
                         items = [] :: [#pubsub_item{}]}).
-type pubsub_retract() :: #pubsub_retract{}.

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

-record(block_list, {}).
-type block_list() :: #block_list{}.

-record(xdata_field, {label :: binary(),
                      type :: 'boolean' | 'fixed' | 'hidden' | 'jid-multi' | 'jid-single' | 'list-multi' | 'list-single' | 'text-multi' | 'text-private' | 'text-single',
                      var :: binary(),
                      required = false :: boolean(),
                      desc :: binary(),
                      values = [] :: [binary()],
                      options = [] :: [binary()]}).
-type xdata_field() :: #xdata_field{}.

-record(version, {name :: binary(),
                  ver :: binary(),
                  os :: binary()}).
-type version() :: #version{}.

-record(muc_invite, {reason :: binary(),
                     from :: any(),
                     to :: any()}).
-type muc_invite() :: #muc_invite{}.

-record(bind, {jid :: any(),
               resource :: any()}).
-type bind() :: #bind{}.

-record(rosterver_feature, {}).
-type rosterver_feature() :: #rosterver_feature{}.

-record(muc_user, {decline :: #muc_decline{},
                   destroy :: #muc_user_destroy{},
                   invites = [] :: [#muc_invite{}],
                   items = [] :: [#muc_item{}],
                   status_codes = [] :: [pos_integer()],
                   password :: binary()}).
-type muc_user() :: #muc_user{}.

-record(carbons_disable, {}).
-type carbons_disable() :: #carbons_disable{}.

-record(bytestreams, {hosts = [] :: [#streamhost{}],
                      used :: any(),
                      activate :: any(),
                      dstaddr :: binary(),
                      mode = tcp :: 'tcp' | 'udp',
                      sid :: binary()}).
-type bytestreams() :: #bytestreams{}.

-record(vcard_org, {name :: binary(),
                    units = [] :: [binary()]}).
-type vcard_org() :: #vcard_org{}.

-record(rsm_set, {'after' :: binary(),
                  before :: 'none' | binary(),
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
                    rsm :: #rsm_set{},
                    xdata :: #xdata{}}).
-type mam_query() :: #mam_query{}.

-record(muc_owner, {destroy :: #muc_owner_destroy{},
                    config :: #xdata{}}).
-type muc_owner() :: #muc_owner{}.

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
                   xdata :: #xdata{}}).
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
                text :: #text{}}).
-type error() :: #error{}.

-record(presence, {id :: binary(),
                   type = available :: 'available' | 'error' | 'probe' | 'subscribe' | 'subscribed' | 'unavailable' | 'unsubscribe' | 'unsubscribed',
                   lang :: binary(),
                   from :: any(),
                   to :: any(),
                   show :: 'away' | 'chat' | 'dnd' | 'xa',
                   status = [] :: [#text{}],
                   priority :: integer(),
                   error :: #error{},
                   sub_els = [] :: [any()]}).
-type presence() :: #presence{}.

-record(message, {id :: binary(),
                  type = normal :: 'chat' | 'error' | 'groupchat' | 'headline' | 'normal',
                  lang :: binary(),
                  from :: any(),
                  to :: any(),
                  subject = [] :: [#text{}],
                  body = [] :: [#text{}],
                  thread :: binary(),
                  error :: #error{},
                  sub_els = [] :: [any()]}).
-type message() :: #message{}.

-record(iq, {id :: binary(),
             type :: 'error' | 'get' | 'result' | 'set',
             lang :: binary(),
             from :: any(),
             to :: any(),
             error :: #error{},
             sub_els = [] :: [any()]}).
-type iq() :: #iq{}.

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

-type xmpp_element() :: compression() |
                        pubsub_subscription() |
                        version() |
                        pubsub_affiliation() |
                        muc_admin() |
                        mam_fin() |
                        sm_a() |
                        carbons_sent() |
                        mam_archived() |
                        p1_rebind() |
                        sasl_abort() |
                        carbons_received() |
                        pubsub_retract() |
                        mix_participant() |
                        compressed() |
                        block_list() |
                        rsm_set() |
                        'see-other-host'() |
                        hint() |
                        starttls_proceed() |
                        sm_resumed() |
                        forwarded() |
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
                        sasl_success() |
                        pubsub_event_items() |
                        disco_items() |
                        pubsub_options() |
                        compress() |
                        bytestreams() |
                        identity() |
                        feature_csi() |
                        muc_user_destroy() |
                        privacy_query() |
                        delay() |
                        muc_history() |
                        vcard_tel() |
                        vcard_logo() |
                        disco_info() |
                        vcard_geo() |
                        vcard_photo() |
                        feature_register() |
                        register() |
                        muc_owner() |
                        pubsub() |
                        sm_r() |
                        muc_actor() |
                        error() |
                        stream_error() |
                        muc_user() |
                        vcard_adr() |
                        carbons_private() |
                        mix_leave() |
                        muc_invite() |
                        rosterver_feature() |
                        vcard_xupdate() |
                        carbons_disable() |
                        bookmark_conference() |
                        offline() |
                        time() |
                        sasl_response() |
                        pubsub_subscribe() |
                        presence() |
                        message() |
                        sm_enable() |
                        starttls_failure() |
                        sasl_challenge() |
                        gone() |
                        private() |
                        compress_failure() |
                        sasl_failure() |
                        bookmark_storage() |
                        vcard_name() |
                        sm_resume() |
                        carbons_enable() |
                        pubsub_unsubscribe() |
                        muc_decline() |
                        chatstate() |
                        sasl_auth() |
                        p1_push() |
                        legacy_auth() |
                        search() |
                        pubsub_publish() |
                        unblock() |
                        p1_ack() |
                        block() |
                        mix_join() |
                        xmpp_session() |
                        xdata() |
                        iq() |
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
                        xdata_field() |
                        sm_failed() |
                        ping() |
                        disco_item() |
                        privacy_item() |
                        caps() |
                        muc() |
                        stream_features() |
                        stats() |
                        pubsub_items() |
                        starttls() |
                        mam_prefs() |
                        sasl_mechanisms() |
                        vcard_key() |
                        csi() |
                        roster_query() |
                        muc_owner_destroy() |
                        mam_query() |
                        bookmark_url() |
                        vcard_email() |
                        vcard_label().
