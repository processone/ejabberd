--
-- ejabberd, Copyright (C) 2002-2017   ProcessOne
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
--

CREATE TABLE users (
    username varchar(191) PRIMARY KEY,
    password text NOT NULL,
    serverkey varchar(64) NOT NULL DEFAULT '',
    salt varchar(64) NOT NULL DEFAULT '',
    iterationcount integer NOT NULL DEFAULT 0,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- Add support for SCRAM auth to a database created before ejabberd 16.03:
-- ALTER TABLE users ADD COLUMN serverkey varchar(64) NOT NULL DEFAULT '';
-- ALTER TABLE users ADD COLUMN salt varchar(64) NOT NULL DEFAULT '';
-- ALTER TABLE users ADD COLUMN iterationcount integer NOT NULL DEFAULT 0;

CREATE TABLE last (
    username varchar(191) PRIMARY KEY,
    seconds text NOT NULL,
    state text NOT NULl
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;


CREATE TABLE rosterusers (
    username varchar(191) NOT NULL,
    jid varchar(191) NOT NULL,
    nick text NOT NULL,
    subscription character(1) NOT NULL,
    ask character(1) NOT NULL,
    askmessage text NOT NULL,
    server character(1) NOT NULL,
    subscribe text NOT NULL,
    type text,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_rosteru_user_jid ON rosterusers(username(75), jid(75));
CREATE INDEX i_rosteru_username ON rosterusers(username);
CREATE INDEX i_rosteru_jid ON rosterusers(jid);

CREATE TABLE rostergroups (
    username varchar(191) NOT NULL,
    jid varchar(191) NOT NULL,
    grp text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX pk_rosterg_user_jid ON rostergroups(username(75), jid(75));

CREATE TABLE sr_group (
    name varchar(191) NOT NULL,
    opts text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE TABLE sr_user (
    jid varchar(191) NOT NULL,
    grp varchar(191) NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_sr_user_jid_group ON sr_user(jid(75), grp(75));
CREATE INDEX i_sr_user_jid ON sr_user(jid);
CREATE INDEX i_sr_user_grp ON sr_user(grp);

CREATE TABLE spool (
    username varchar(191) NOT NULL,
    xml BLOB NOT NULL,
    seq BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_despool USING BTREE ON spool(username);
CREATE INDEX i_spool_created_at USING BTREE ON spool(created_at);

CREATE TABLE archive (
    username varchar(191) NOT NULL,
    timestamp BIGINT UNSIGNED NOT NULL,
    peer varchar(191) NOT NULL,
    bare_peer varchar(191) NOT NULL,
    xml text NOT NULL,
    txt text,
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE,
    kind varchar(10),
    nick varchar(191),
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE FULLTEXT INDEX i_text ON archive(txt);
CREATE INDEX i_username USING BTREE ON archive(username);
CREATE INDEX i_timestamp USING BTREE ON archive(timestamp);
CREATE INDEX i_peer USING BTREE ON archive(peer);
CREATE INDEX i_bare_peer USING BTREE ON archive(bare_peer);

CREATE TABLE archive_prefs (
    username varchar(191) NOT NULL PRIMARY KEY,
    def text NOT NULL,
    always text NOT NULL,
    never text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE TABLE vcard (
    username varchar(191) PRIMARY KEY,
    vcard mediumtext NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE TABLE vcard_xupdate (
    username varchar(191) PRIMARY KEY,
    hash text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE TABLE vcard_search (
    username varchar(191) NOT NULL,
    lusername varchar(191) PRIMARY KEY,
    fn text NOT NULL,
    lfn varchar(191) NOT NULL,
    family text NOT NULL,
    lfamily varchar(191) NOT NULL,
    given text NOT NULL,
    lgiven varchar(191) NOT NULL,
    middle text NOT NULL,
    lmiddle varchar(191) NOT NULL,
    nickname text NOT NULL,
    lnickname varchar(191) NOT NULL,
    bday text NOT NULL,
    lbday varchar(191) NOT NULL,
    ctry text NOT NULL,
    lctry varchar(191) NOT NULL,
    locality text NOT NULL,
    llocality varchar(191) NOT NULL,
    email text NOT NULL,
    lemail varchar(191) NOT NULL,
    orgname text NOT NULL,
    lorgname varchar(191) NOT NULL,
    orgunit text NOT NULL,
    lorgunit varchar(191) NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_vcard_search_lfn       ON vcard_search(lfn);
CREATE INDEX i_vcard_search_lfamily   ON vcard_search(lfamily);
CREATE INDEX i_vcard_search_lgiven    ON vcard_search(lgiven);
CREATE INDEX i_vcard_search_lmiddle   ON vcard_search(lmiddle);
CREATE INDEX i_vcard_search_lnickname ON vcard_search(lnickname);
CREATE INDEX i_vcard_search_lbday     ON vcard_search(lbday);
CREATE INDEX i_vcard_search_lctry     ON vcard_search(lctry);
CREATE INDEX i_vcard_search_llocality ON vcard_search(llocality);
CREATE INDEX i_vcard_search_lemail    ON vcard_search(lemail);
CREATE INDEX i_vcard_search_lorgname  ON vcard_search(lorgname);
CREATE INDEX i_vcard_search_lorgunit  ON vcard_search(lorgunit);

CREATE TABLE privacy_default_list (
    username varchar(191) PRIMARY KEY,
    name varchar(191) NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE TABLE privacy_list (
    username varchar(191) NOT NULL,
    name varchar(191) NOT NULL,
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_privacy_list_username  USING BTREE ON privacy_list(username);
CREATE UNIQUE INDEX i_privacy_list_username_name USING BTREE ON privacy_list (username(75), name(75));

CREATE TABLE privacy_list_data (
    id bigint,
    t character(1) NOT NULL,
    value text NOT NULL,
    action character(1) NOT NULL,
    ord NUMERIC NOT NULL,
    match_all boolean NOT NULL,
    match_iq boolean NOT NULL,
    match_message boolean NOT NULL,
    match_presence_in boolean NOT NULL,
    match_presence_out boolean NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_privacy_list_data_id ON privacy_list_data(id);

CREATE TABLE private_storage (
    username varchar(191) NOT NULL,
    namespace varchar(191) NOT NULL,
    data text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_private_storage_username USING BTREE ON private_storage(username);
CREATE UNIQUE INDEX i_private_storage_username_namespace USING BTREE ON private_storage(username(75), namespace(75));

-- Not tested in mysql
CREATE TABLE roster_version (
    username varchar(191) PRIMARY KEY,
    version text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- To update from 1.x:
-- ALTER TABLE rosterusers ADD COLUMN askmessage text AFTER ask;
-- UPDATE rosterusers SET askmessage = '';
-- ALTER TABLE rosterusers ALTER COLUMN askmessage SET NOT NULL;

CREATE TABLE pubsub_node (
  host text NOT NULL,
  node text NOT NULL,
  parent VARCHAR(191) NOT NULL DEFAULT '',
  type text NOT NULL,
  nodeid bigint auto_increment primary key
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE INDEX i_pubsub_node_parent ON pubsub_node(parent(120));
CREATE UNIQUE INDEX i_pubsub_node_tuple ON pubsub_node(host(20), node(120));

CREATE TABLE pubsub_node_option (
  nodeid bigint,
  name text NOT NULL,
  val text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE INDEX i_pubsub_node_option_nodeid ON pubsub_node_option(nodeid);
ALTER TABLE `pubsub_node_option` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_node_owner (
  nodeid bigint,
  owner text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE INDEX i_pubsub_node_owner_nodeid ON pubsub_node_owner(nodeid);
ALTER TABLE `pubsub_node_owner` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_state (
  nodeid bigint,
  jid text NOT NULL,
  affiliation character(1),
  subscriptions VARCHAR(191) NOT NULL DEFAULT '',
  stateid bigint auto_increment primary key
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE INDEX i_pubsub_state_jid ON pubsub_state(jid(60));
CREATE UNIQUE INDEX i_pubsub_state_tuple ON pubsub_state(nodeid, jid(60));
ALTER TABLE `pubsub_state` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_item (
  nodeid bigint,
  itemid text NOT NULL,
  publisher text NOT NULL,
  creation text NOT NULL,
  modification text NOT NULL,
  payload text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE INDEX i_pubsub_item_itemid ON pubsub_item(itemid(36));
CREATE UNIQUE INDEX i_pubsub_item_tuple ON pubsub_item(nodeid, itemid(36));
ALTER TABLE `pubsub_item` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_subscription_opt (
  subid text NOT NULL,
  opt_name varchar(32),
  opt_value text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE UNIQUE INDEX i_pubsub_subscription_opt ON pubsub_subscription_opt(subid(32), opt_name(32));

CREATE TABLE muc_room (
    name text NOT NULL,
    host text NOT NULL,
    opts mediumtext NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_muc_room_name_host USING BTREE ON muc_room(name(75), host(75));

CREATE TABLE muc_registered (
    jid text NOT NULL,
    host text NOT NULL,
    nick text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_muc_registered_nick USING BTREE ON muc_registered(nick(75));
CREATE UNIQUE INDEX i_muc_registered_jid_host USING BTREE ON muc_registered(jid(75), host(75));

CREATE TABLE muc_online_room (
    name text NOT NULL,
    host text NOT NULL,
    node text NOT NULL,
    pid text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_muc_online_room_name_host USING BTREE ON muc_online_room(name(75), host(75));

CREATE TABLE muc_online_users (
    username text NOT NULL,
    server text NOT NULL,
    resource text NOT NULL,
    name text NOT NULL,
    host text NOT NULL,
    node text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_muc_online_users USING BTREE ON muc_online_users(username(75), server(75), resource(75), name(75), host(75));
CREATE INDEX i_muc_online_users_us USING BTREE ON muc_online_users(username(75), server(75));

CREATE TABLE irc_custom (
    jid text NOT NULL,
    host text NOT NULL,
    data text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_irc_custom_jid_host USING BTREE ON irc_custom(jid(75), host(75));

CREATE TABLE motd (
    username varchar(191) PRIMARY KEY,
    xml text,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE TABLE caps_features (
    node varchar(191) NOT NULL,
    subnode varchar(191) NOT NULL,
    feature text,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_caps_features_node_subnode ON caps_features(node(75), subnode(75));

CREATE TABLE sm (
    usec bigint NOT NULL,
    pid text NOT NULL,
    node text NOT NULL,
    username varchar(191) NOT NULL,
    resource varchar(191) NOT NULL,
    priority text NOT NULL,
    info text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_sid ON sm(usec, pid(75));
CREATE INDEX i_node ON sm(node(75));
CREATE INDEX i_username ON sm(username);

CREATE TABLE oauth_token (
    token varchar(191) NOT NULL PRIMARY KEY,
    jid text NOT NULL,
    scope text NOT NULL,
    expire bigint NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE TABLE route (
    domain text NOT NULL,
    server_host text NOT NULL,
    node text NOT NULL,
    pid text NOT NULL,
    local_hint text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_route ON route(domain(75), server_host(75), node(75), pid(75));
CREATE INDEX i_route_domain ON route(domain(75));

CREATE TABLE bosh (
    sid text NOT NULL,
    node text NOT NULL,
    pid text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_bosh_sid ON bosh(sid(75));

CREATE TABLE carboncopy (
    username text NOT NULL,
    resource text NOT NULL,
    namespace text NOT NULL,
    node text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_carboncopy_ur ON carboncopy (username(75), resource(75));
CREATE INDEX i_carboncopy_user ON carboncopy (username(75));

CREATE TABLE proxy65 (
    sid text NOT NULL,
    pid_t text NOT NULL,
    pid_i text NOT NULL,
    node_t text NOT NULL,
    node_i text NOT NULL,
    jid_i text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_proxy65_sid ON proxy65 (sid(191));
CREATE INDEX i_proxy65_jid ON proxy65 (jid_i(191));
