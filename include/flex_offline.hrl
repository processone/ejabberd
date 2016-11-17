%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: flex_offline.xdata
%% Form type: http://jabber.org/protocol/offline
%% Document: XEP-0013


-type property() :: {'number_of_messages', non_neg_integer()}.
-type result() :: [property()].

-type form() :: [property() | xdata_field()].
