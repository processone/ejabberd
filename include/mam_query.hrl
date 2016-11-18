%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: mam_query.xdata
%% Form type: urn:xmpp:mam:1
%% Document: XEP-0313


-type property() :: {'with', jid:jid()} |
                    {'start', erlang:timestamp()} |
                    {'end', erlang:timestamp()} |
                    {'withtext', binary()}.
-type result() :: [property()].

-type form() :: [property() | xdata_field()].
