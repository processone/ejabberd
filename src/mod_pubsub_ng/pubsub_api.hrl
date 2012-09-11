%% API function definition

-record(api_core,
{
  create_node                        = 'pubsub_core' :: module(),
  delete_node                        = 'pubsub_core' :: module(),
  purge_node                         = 'pubsub_core' :: module(),
  get_configure_node                 = 'pubsub_core' :: module(),
  get_configure_node_default         = 'pubsub_core' :: module(),
  %%
  publish_item                       = 'pubsub_core' :: module(),
  retract_item                       = 'pubsub_core' :: module(),
  %%
  subscribe_node                     = 'pubsub_core' :: module(),
  unsubscribe_node                   = 'pubsub_core' :: module(),
  set_configure_subscription         = 'pubsub_core' :: module(),
  get_configure_subscription         = 'pubsub_core' :: module(),
  get_configure_subscription_default = 'pubsub_core' :: module(),
  get_items                          = 'pubsub_core' :: module(),
  %%
  get_entity_affiliations            = 'pubsub_core' :: module(),
  get_entity_subscriptions           = 'pubsub_core' :: module(),
  %%
  get_node_affiliations              = 'pubsub_core' :: module(),
  get_node_subscriptions             = 'pubsub_core' :: module()
}).

-record(api_db,
{
  create_node                        = 'pubsub_db' :: module(),
  delete_node                        = 'pubsub_db' :: module(),
  purge_node                         = 'pubsub_db' :: module(),
  get_configure_node                 = 'pubsub_db' :: module(),
 %get_configure_node_default         = 'pubsub_db' :: module(),
  %%
  publish_item                       = 'pubsub_db' :: module(),
  retract_item                       = 'pubsub_db' :: module(),
  %%
  subscribe_node                     = 'pubsub_db' :: module(),
  unsubscribe_node                   = 'pubsub_db' :: module(),
  set_configure_subscription         = 'pubsub_db' :: module(),
  get_configure_subscription         = 'pubsub_db' :: module(),
  get_configure_subscription_default = 'pubsub_db' :: module(),
  get_items                          = 'pubsub_db' :: module(),
  %%
  get_entity_affiliations            = 'pubsub_db' :: module(),
  get_entity_subscriptions           = 'pubsub_db' :: module(),
  %%
  get_node_affiliations              = 'pubsub_db' :: module(),
  get_node_subscriptions             = 'pubsub_db' :: module()
}).

-record(api_broadcast,
{
  broadcast_publish      = 'pubsub_broadcast' :: module(),
  broadcast_publish_last = 'pubsub_broadcast' :: module(),
  notify_create          = 'pubsub_broadcast' :: module(),
  notify_delete          = 'pubsub_broadcast' :: module(),
  notify_publish         = 'pubsub_broadcast' :: module(),
  notify_purge           = 'pubsub_broadcast' :: module(),
  notify_retract         = 'pubsub_broadcast' :: module(),
  notify_subscription    = 'pubsub_broadcast' :: module(),
  notify_subscriptions   = 'pubsub_broadcast' :: module()
}).

-record(api,
{
  core      = #api_core{}      :: #api_core{},
  db        = #api_db{}        :: #api_db{},
  broadcast = #api_broadcast{} :: #api_broadcast{},
  parser    = 'pubsub_parser'  :: module(),
  options   = 'pubsub_options' :: module()
}).

-record(capabilities,
{
  plugin           :: exmpp_pubsub:plugin(),
  privacy = false  :: boolean(),
  api     = #api{} :: #api{}
}).

-record(api2,
{
  func,
  core :: 'core',
  db   :: 'db',
  bkd  :: 'mnesia',
  rtr  :: 'router'
}).

-record(mod_pubsub,
{
  server,
  component,
  plugin,
  entity,
  parameters,
  features,
  parser,
  options
}).
