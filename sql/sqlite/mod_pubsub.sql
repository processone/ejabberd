CREATE TABLE pubsub_node (
  host text NOT NULL,
  node text NOT NULL,
  parent text NOT NULL DEFAULT '',
  plugin text NOT NULL,
  nodeid INTEGER PRIMARY KEY AUTOINCREMENT
);
CREATE INDEX i_pubsub_node_parent ON pubsub_node (parent);
CREATE UNIQUE INDEX i_pubsub_node_tuple ON pubsub_node (host, node);

CREATE TABLE pubsub_node_option (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  name text NOT NULL,
  val text NOT NULL
);
CREATE INDEX i_pubsub_node_option_nodeid ON pubsub_node_option (nodeid);

CREATE TABLE pubsub_node_owner (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  owner text NOT NULL
);
CREATE INDEX i_pubsub_node_owner_nodeid ON pubsub_node_owner (nodeid);

CREATE TABLE pubsub_state (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  jid text NOT NULL,
  affiliation character(1),
  subscriptions text NOT NULL DEFAULT '',
  stateid INTEGER PRIMARY KEY AUTOINCREMENT
);
CREATE INDEX i_pubsub_state_jid ON pubsub_state (jid);
CREATE UNIQUE INDEX i_pubsub_state_tuple ON pubsub_state (nodeid, jid);

CREATE TABLE pubsub_item (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  itemid text NOT NULL,
  publisher text NOT NULL,
  creation varchar(32) NOT NULL,
  modification varchar(32) NOT NULL,
  payload text NOT NULL DEFAULT ''
);
CREATE INDEX i_pubsub_item_itemid ON pubsub_item (itemid);
CREATE UNIQUE INDEX i_pubsub_item_tuple ON pubsub_item (nodeid, itemid);

 CREATE TABLE pubsub_subscription_opt (
  subid text NOT NULL,
  opt_name varchar(32),
  opt_value text NOT NULL
);
CREATE UNIQUE INDEX i_pubsub_subscription_opt ON pubsub_subscription_opt (subid, opt_name);
