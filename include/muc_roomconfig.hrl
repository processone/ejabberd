%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: muc_roomconfig.xdata
%% Form type: http://jabber.org/protocol/muc#roomconfig
%% Document: XEP-0045

-type 'allow_private_messages_from_visitors'() :: nobody | moderators | anyone.
-type 'maxusers'() :: none | non_neg_integer().
-type 'presencebroadcast'() :: moderator | participant | visitor.
-type 'whois'() :: moderators | anyone.

-type property() :: {'maxhistoryfetch', binary()} |
                    {'allowpm', binary()} |
                    {'allow_private_messages', boolean()} |
                    {'allow_private_messages_from_visitors', 'allow_private_messages_from_visitors'()} |
                    {'allow_visitor_status', boolean()} |
                    {'allow_visitor_nickchange', boolean()} |
                    {'allow_voice_requests', boolean()} |
                    {'allow_subscription', boolean()} |
                    {'voice_request_min_interval', non_neg_integer()} |
                    {'captcha_protected', boolean()} |
                    {'captcha_whitelist', [jid:jid()]} |
                    {'allow_query_users', boolean()} |
                    {'allowinvites', boolean()} |
                    {'changesubject', boolean()} |
                    {'enablelogging', boolean()} |
                    {'getmemberlist', [binary()]} |
                    {'lang', binary()} |
                    {'pubsub', binary()} |
                    {'maxusers', 'maxusers'()} |
                    {'membersonly', boolean()} |
                    {'moderatedroom', boolean()} |
                    {'members_by_default', boolean()} |
                    {'passwordprotectedroom', boolean()} |
                    {'persistentroom', boolean()} |
                    {'presencebroadcast', ['presencebroadcast'()]} |
                    {'publicroom', boolean()} |
                    {'public_list', boolean()} |
                    {'roomadmins', [jid:jid()]} |
                    {'roomdesc', binary()} |
                    {'roomname', binary()} |
                    {'roomowners', [jid:jid()]} |
                    {'roomsecret', binary()} |
                    {'whois', 'whois'()} |
                    {'mam', boolean()}.
-type result() :: [property()].

-type options(T) :: [{binary(), T}].
-type property_with_options() ::
      {'allowpm', binary(), options(binary())} |
      {'allow_private_messages_from_visitors', 'allow_private_messages_from_visitors'(), options('allow_private_messages_from_visitors'())} |
      {'getmemberlist', [binary()], options(binary())} |
      {'maxusers', 'maxusers'(), options('maxusers'())} |
      {'presencebroadcast', ['presencebroadcast'()], options('presencebroadcast'())} |
      {'whois', 'whois'(), options('whois'())}.
-type form() :: [property() | property_with_options() | xdata_field()].
