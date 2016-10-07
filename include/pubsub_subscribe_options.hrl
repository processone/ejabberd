%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: pubsub_subscribe_options.xdata
%% Form type: http://jabber.org/protocol/pubsub#subscribe_options
%% Document: XEP-0060

-type 'show-values'() :: away | chat | dnd | online | xa.
-type 'subscription_type'() :: items | nodes.
-type 'subscription_depth'() :: 1 | all.

-type property() :: {'deliver', boolean()} |
                    {'digest', boolean()} |
                    {'digest_frequency', binary()} |
                    {'expire', binary()} |
                    {'include_body', boolean()} |
                    {'show-values', ['show-values'()]} |
                    {'subscription_type', 'subscription_type'()} |
                    {'subscription_depth', 'subscription_depth'()}.
-type result() :: [property()].

-type options(T) :: [{binary(), T}].
-type property_with_options() ::
      {'show-values', ['show-values'()], options('show-values'())} |
      {'subscription_type', 'subscription_type'(), options('subscription_type'())} |
      {'subscription_depth', 'subscription_depth'(), options('subscription_depth'())}.
-type form() :: [property() | property_with_options() | xdata_field()].
