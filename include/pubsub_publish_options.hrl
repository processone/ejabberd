%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: pubsub_publish_options.xdata
%% Form type: http://jabber.org/protocol/pubsub#publish-options
%% Document: XEP-0060

-type 'access_model'() :: authorize | open | presence | roster | whitelist.

-type property() :: {'access_model', 'access_model'()}.
-type result() :: [property()].

-type options(T) :: [{binary(), T}].
-type property_with_options() ::
      {'access_model', 'access_model'(), options('access_model'())}.
-type form() :: [property() | property_with_options() | xdata_field()].
