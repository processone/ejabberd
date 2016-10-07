%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: muc_register.xdata
%% Form type: http://jabber.org/protocol/muc#register
%% Document: XEP-0045


-type property() :: {'allow', boolean()} |
                    {'email', binary()} |
                    {'faqentry', [binary()]} |
                    {'first', binary()} |
                    {'last', binary()} |
                    {'roomnick', binary()} |
                    {'url', binary()}.
-type result() :: [property()].

-type form() :: [property() | xdata_field()].
