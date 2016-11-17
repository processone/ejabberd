%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: muc_roominfo.xdata
%% Form type: http://jabber.org/protocol/muc#roominfo
%% Document: XEP-0045


-type property() :: {'maxhistoryfetch', non_neg_integer()} |
                    {'contactjid', [jid:jid()]} |
                    {'description', binary()} |
                    {'lang', binary()} |
                    {'ldapgroup', binary()} |
                    {'logs', binary()} |
                    {'occupants', non_neg_integer()} |
                    {'subject', binary()} |
                    {'subjectmod', boolean()}.
-type result() :: [property()].

-type form() :: [property() | xdata_field()].
