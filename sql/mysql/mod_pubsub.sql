CREATE TABLE pubsub_node (
  host text NOT NULL,
  node text NOT NULL,
  parent VARCHAR(191) NOT NULL DEFAULT '',
  plugin text NOT NULL,
  nodeid bigint auto_increment primary key
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE INDEX i_pubsub_node_parent ON pubsub_node(parent(120));
CREATE UNIQUE INDEX i_pubsub_node_tuple ON pubsub_node(host(20), node(120));

CREATE TABLE pubsub_node_option (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  name text NOT NULL,
  val text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE INDEX i_pubsub_node_option_nodeid ON pubsub_node_option(nodeid);

CREATE TABLE pubsub_node_owner (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  owner text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE INDEX i_pubsub_node_owner_nodeid ON pubsub_node_owner(nodeid);

CREATE TABLE pubsub_state (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  jid text NOT NULL,
  affiliation character(1),
  subscriptions VARCHAR(191) NOT NULL DEFAULT '',
  stateid bigint auto_increment primary key
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE INDEX i_pubsub_state_jid ON pubsub_state(jid(60));
CREATE UNIQUE INDEX i_pubsub_state_tuple ON pubsub_state(nodeid, jid(60));

CREATE TABLE pubsub_item (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  itemid text NOT NULL,
  publisher text NOT NULL,
  creation varchar(32) NOT NULL,
  modification varchar(32) NOT NULL,
  payload text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE INDEX i_pubsub_item_itemid ON pubsub_item(itemid(36));
CREATE UNIQUE INDEX i_pubsub_item_tuple ON pubsub_item(nodeid, itemid(36));

CREATE TABLE pubsub_subscription_opt (
  subid text NOT NULL,
  opt_name varchar(32),
  opt_value text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE UNIQUE INDEX i_pubsub_subscription_opt ON pubsub_subscription_opt(subid(32), opt_name(32));
