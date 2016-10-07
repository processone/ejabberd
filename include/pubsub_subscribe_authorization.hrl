%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: pubsub_subscribe_authorization.xdata
%% Form type: http://jabber.org/protocol/pubsub#subscribe_authorization
%% Document: XEP-0060


-type property() :: {'allow', boolean()} |
                    {'node', binary()} |
                    {'subscriber_jid', jid:jid()} |
                    {'subid', binary()}.
-type result() :: [property()].

-type form() :: [property() | xdata_field()].
