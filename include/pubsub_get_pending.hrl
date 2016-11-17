%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: pubsub_get_pending.xdata
%% Form type: http://jabber.org/protocol/pubsub#subscribe_authorization
%% Document: XEP-0060


-type property() :: {'node', binary()}.
-type result() :: [property()].

-type options(T) :: [{binary(), T}].
-type property_with_options() ::
      {'node', binary(), options(binary())}.
-type form() :: [property() | property_with_options() | xdata_field()].
