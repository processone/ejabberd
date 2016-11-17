%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: pubsub_node_config.xdata
%% Form type: http://jabber.org/protocol/pubsub#node_config
%% Document: XEP-0060

-type 'access_model'() :: authorize | open | presence | roster | whitelist.
-type 'children_association_policy'() :: all | owners | whitelist.
-type 'itemreply'() :: owner | publisher | none.
-type 'node_type'() :: leaf | collection.
-type 'notification_type'() :: normal | headline.
-type 'publish_model'() :: publishers | subscribers | open.
-type 'send_last_published_item'() :: never | on_sub | on_sub_and_presence.

-type property() :: {'access_model', 'access_model'()} |
                    {'body_xslt', binary()} |
                    {'children_association_policy', 'children_association_policy'()} |
                    {'children_association_whitelist', [jid:jid()]} |
                    {'children', [binary()]} |
                    {'children_max', binary()} |
                    {'collection', [binary()]} |
                    {'contact', [jid:jid()]} |
                    {'dataform_xslt', binary()} |
                    {'deliver_notifications', boolean()} |
                    {'deliver_payloads', boolean()} |
                    {'description', binary()} |
                    {'item_expire', binary()} |
                    {'itemreply', 'itemreply'()} |
                    {'language', binary()} |
                    {'max_items', non_neg_integer()} |
                    {'max_payload_size', non_neg_integer()} |
                    {'node_type', 'node_type'()} |
                    {'notification_type', 'notification_type'()} |
                    {'notify_config', boolean()} |
                    {'notify_delete', boolean()} |
                    {'notify_retract', boolean()} |
                    {'notify_sub', boolean()} |
                    {'persist_items', boolean()} |
                    {'presence_based_delivery', boolean()} |
                    {'publish_model', 'publish_model'()} |
                    {'purge_offline', boolean()} |
                    {'roster_groups_allowed', [binary()]} |
                    {'send_last_published_item', 'send_last_published_item'()} |
                    {'tempsub', boolean()} |
                    {'subscribe', boolean()} |
                    {'title', binary()} |
                    {'type', binary()}.
-type result() :: [property()].

-type options(T) :: [{binary(), T}].
-type property_with_options() ::
      {'access_model', 'access_model'(), options('access_model'())} |
      {'children_association_policy', 'children_association_policy'(), options('children_association_policy'())} |
      {'itemreply', 'itemreply'(), options('itemreply'())} |
      {'language', binary(), options(binary())} |
      {'node_type', 'node_type'(), options('node_type'())} |
      {'notification_type', 'notification_type'(), options('notification_type'())} |
      {'publish_model', 'publish_model'(), options('publish_model'())} |
      {'roster_groups_allowed', [binary()], options(binary())} |
      {'send_last_published_item', 'send_last_published_item'(), options('send_last_published_item'())}.
-type form() :: [property() | property_with_options() | xdata_field()].
