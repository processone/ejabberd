%% Created automatically by XML generator (xml_gen.erl)
%% Source: xmpp_codec.spec

-record(feature_register, {}).

-record(sasl_success, {text :: any()}).

-record(streamhost, {jid :: any(),
                     host :: binary(),
                     port = 1080 :: non_neg_integer()}).

-record(pubsub_unsubscribe, {node :: binary(),
                             jid :: any(),
                             subid :: binary()}).

-record(ping, {}).

-record(delay, {stamp :: any(),
                from :: any()}).

-record(muc_history, {maxchars :: non_neg_integer(),
                      maxstanzas :: non_neg_integer(),
                      seconds :: non_neg_integer(),
                      since :: any()}).

-record(pubsub_affiliation, {node :: binary(),
                             type :: 'member' | 'none' | 'outcast' | 'owner' | 'publish-only' | 'publisher'}).

-record(muc_decline, {reason :: binary(),
                      from :: any(),
                      to :: any()}).

-record(starttls_proceed, {}).

-record(starttls_failure, {}).

-record(sasl_challenge, {text :: any()}).

-record(gone, {uri :: binary()}).

-record(private, {xml_els = [] :: [any()]}).

-record(p1_ack, {}).

-record(pubsub_item, {id :: binary(),
                      xml_els = [] :: [any()]}).

-record(pubsub_publish, {node :: binary(),
                         items = [] :: [#pubsub_item{}]}).

-record(roster_item, {jid :: any(),
                      name :: binary(),
                      groups = [] :: [binary()],
                      subscription = none :: 'both' | 'from' | 'none' | 'remove' | 'to',
                      ask :: 'subscribe'}).

-record(roster, {items = [] :: [#roster_item{}],
                 ver :: binary()}).

-record(pubsub_event_item, {id :: binary(),
                            node :: binary(),
                            publisher :: binary()}).

-record(muc_actor, {jid :: any(),
                    nick :: binary()}).

-record(stat, {name :: binary(),
               units :: binary(),
               value :: binary(),
               error = [] :: [{integer(),'undefined' | binary()}]}).

-record('see-other-host', {host :: binary()}).

-record(compress, {methods = [] :: [binary()]}).

-record(starttls, {required = false :: boolean()}).

-record(last, {seconds :: non_neg_integer(),
               text :: binary()}).

-record(pubsub_event_items, {node :: binary(),
                             retract = [] :: [binary()],
                             items = [] :: [#pubsub_event_item{}]}).

-record(pubsub_event, {items = [] :: [#pubsub_event_items{}]}).

-record(redirect, {uri :: binary()}).

-record(sasl_response, {text :: any()}).

-record(pubsub_subscribe, {node :: binary(),
                           jid :: any()}).

-record(sasl_auth, {mechanism :: binary(),
                    text :: any()}).

-record(p1_push, {}).

-record(legacy_delay, {stamp :: binary(),
                       from :: any()}).

-record(muc_user_destroy, {reason :: binary(),
                           jid :: any()}).

-record(disco_item, {jid :: any(),
                     name :: binary(),
                     node :: binary()}).

-record(disco_items, {node :: binary(),
                      items = [] :: [#disco_item{}]}).

-record(unblock, {items = [] :: [any()]}).

-record(block, {items = [] :: [any()]}).

-record(session, {}).

-record(compression, {methods = [] :: [binary()]}).

-record(muc_owner_destroy, {jid :: any(),
                            reason :: binary(),
                            password :: binary()}).

-record(pubsub_subscription, {jid :: any(),
                              node :: binary(),
                              subid :: binary(),
                              type :: 'none' | 'pending' | 'subscribed' | 'unconfigured'}).

-record(shim, {headers = [] :: [{binary(),'undefined' | binary()}]}).

-record(caps, {hash :: binary(),
               node :: binary(),
               ver :: any()}).

-record(muc, {history :: #muc_history{},
              password :: binary()}).

-record(stream_features, {sub_els = [] :: [any()]}).

-record(stats, {stat = [] :: [#stat{}]}).

-record(pubsub_items, {node :: binary(),
                       max_items :: non_neg_integer(),
                       subid :: binary(),
                       items = [] :: [#pubsub_item{}]}).

-record(p1_rebind, {}).

-record(compress_failure, {reason :: 'processing-failed' | 'setup-failed' | 'unsupported-method'}).

-record(sasl_abort, {}).

-record(vcard_email, {home = false :: boolean(),
                      work = false :: boolean(),
                      internet = false :: boolean(),
                      pref = false :: boolean(),
                      x400 = false :: boolean(),
                      userid :: binary()}).

-record(pubsub_retract, {node :: binary(),
                         notify = false :: any(),
                         items = [] :: [#pubsub_item{}]}).

-record(text, {lang :: binary(),
               data :: binary()}).

-record(vcard_geo, {lat :: binary(),
                    lon :: binary()}).

-record(compressed, {}).

-record(sasl_failure, {reason :: 'aborted' | 'account-disabled' | 'credentials-expired' | 'encryption-required' | 'incorrect-encoding' | 'invalid-authzid' | 'invalid-mechanism' | 'malformed-request' | 'mechanism-too-weak' | 'not-authorized' | 'temporary-auth-failure',
                       text = [] :: [#text{}]}).

-record(block_list, {}).

-record(xdata_field, {label :: binary(),
                      type :: 'boolean' | 'fixed' | 'hidden' | 'jid-multi' | 'jid-single' | 'list-multi' | 'list-single' | 'text-multi' | 'text-private' | 'text-single',
                      var :: binary(),
                      required = false :: boolean(),
                      desc :: binary(),
                      values = [] :: [binary()],
                      options = [] :: [binary()]}).

-record(version, {name :: binary(),
                  ver :: binary(),
                  os :: binary()}).

-record(muc_invite, {reason :: binary(),
                     from :: any(),
                     to :: any()}).

-record(bind, {jid :: any(),
               resource :: any()}).

-record(muc_item, {actor :: #muc_actor{},
                   continue :: binary(),
                   reason :: binary(),
                   affiliation :: 'admin' | 'member' | 'none' | 'outcast' | 'owner',
                   role :: 'moderator' | 'none' | 'participant' | 'visitor',
                   jid :: any(),
                   nick :: binary()}).

-record(muc_user, {decline :: #muc_decline{},
                   destroy :: #muc_user_destroy{},
                   invites = [] :: [#muc_invite{}],
                   items = [] :: [#muc_item{}],
                   status_codes = [] :: [pos_integer()],
                   password :: binary()}).

-record(bytestreams, {hosts = [] :: [#streamhost{}],
                      used :: any(),
                      activate :: any(),
                      dstaddr :: binary(),
                      mode = tcp :: 'tcp' | 'udp',
                      sid :: binary()}).

-record(vcard_org, {name :: binary(),
                    units = [] :: [binary()]}).

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

-record(vcard_key, {type :: binary(),
                    cred :: binary()}).

-record(vcard_name, {family :: binary(),
                     given :: binary(),
                     middle :: binary(),
                     prefix :: binary(),
                     suffix :: binary()}).

-record(identity, {category :: binary(),
                   type :: binary(),
                   lang :: binary(),
                   name :: binary()}).

-record(bookmark_conference, {name :: binary(),
                              jid :: any(),
                              autojoin = false :: any(),
                              nick :: binary(),
                              password :: binary()}).

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
                   key :: 'none' | binary()}).

-record(bookmark_url, {name :: binary(),
                       url :: binary()}).

-record(bookmark_storage, {conference = [] :: [#bookmark_conference{}],
                           url = [] :: [#bookmark_url{}]}).

-record(vcard_sound, {phonetic :: binary(),
                      binval :: any(),
                      extval :: binary()}).

-record(vcard_photo, {type :: binary(),
                      binval :: any(),
                      extval :: binary()}).

-record(vcard_label, {home = false :: boolean(),
                      work = false :: boolean(),
                      postal = false :: boolean(),
                      parcel = false :: boolean(),
                      dom = false :: boolean(),
                      intl = false :: boolean(),
                      pref = false :: boolean(),
                      line = [] :: [binary()]}).

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

-record(xdata, {type :: 'cancel' | 'form' | 'result' | 'submit',
                instructions = [] :: [binary()],
                title :: binary(),
                reported :: [#xdata_field{}],
                items = [] :: [[#xdata_field{}]],
                fields = [] :: [#xdata_field{}]}).

-record(muc_owner, {destroy :: #muc_owner_destroy{},
                    config :: #xdata{}}).

-record(pubsub_options, {node :: binary(),
                         jid :: any(),
                         subid :: binary(),
                         xdata :: #xdata{}}).

-record(pubsub, {subscriptions :: {'none' | binary(),[#pubsub_subscription{}]},
                 affiliations :: [#pubsub_affiliation{}],
                 publish :: #pubsub_publish{},
                 subscribe :: #pubsub_subscribe{},
                 unsubscribe :: #pubsub_unsubscribe{},
                 options :: #pubsub_options{},
                 items :: #pubsub_items{},
                 retract :: #pubsub_retract{}}).

-record(disco_info, {node :: binary(),
                     identities = [] :: [#identity{}],
                     features = [] :: [binary()],
                     xdata = [] :: [#xdata{}]}).

-record(sasl_mechanisms, {list = [] :: [binary()]}).

-record(error, {type :: 'auth' | 'cancel' | 'continue' | 'modify' | 'wait',
                by :: binary(),
                reason :: atom() | #gone{} | #redirect{},
                text :: #text{}}).

-record(presence, {id :: binary(),
                   type :: 'error' | 'probe' | 'subscribe' | 'subscribed' | 'unavailable' | 'unsubscribe' | 'unsubscribed',
                   lang :: binary(),
                   from :: any(),
                   to :: any(),
                   show :: 'away' | 'chat' | 'dnd' | 'xa',
                   status = [] :: [#text{}],
                   priority :: integer(),
                   error :: #error{},
                   sub_els = [] :: [any()]}).

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

-record(iq, {id :: binary(),
             type :: 'error' | 'get' | 'result' | 'set',
             lang :: binary(),
             from :: any(),
             to :: any(),
             error :: #error{},
             sub_els = [] :: [any()]}).

-record(privacy_item, {order :: non_neg_integer(),
                       action :: 'allow' | 'deny',
                       type :: 'group' | 'jid' | 'subscription',
                       value :: binary(),
                       kinds = [] :: ['iq' | 'message' | 'presence-in' | 'presence-out']}).

-record(privacy_list, {name :: binary(),
                       items = [] :: [#privacy_item{}]}).

-record(privacy, {lists = [] :: [#privacy_list{}],
                  default :: 'none' | binary(),
                  active :: 'none' | binary()}).

-record(stream_error, {reason :: atom() | #'see-other-host'{},
                       text :: #text{}}).

-record(vcard_logo, {type :: binary(),
                     binval :: any(),
                     extval :: binary()}).

-record(vcard, {version :: binary(),
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

-record(time, {tzo :: any(),
               utc :: any()}).
