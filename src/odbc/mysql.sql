--
-- ejabberd, Copyright (C) 2002-2011   ProcessOne
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
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
-- 02111-1307 USA
--

-- Needs MySQL (at least 4.0.x) with innodb back-end
SET table_type=InnoDB;

CREATE TABLE hosts (
    clusterid integer NOT NULL,
    host varchar(250) NOT NULL PRIMARY KEY,
    config text NOT NULL
) CHARACTER SET utf8;

INSERT INTO hosts (clusterid, host, config)
VALUES (1, 'localhost', '');

CREATE TABLE users (
    host varchar(250) NOT NULL,
    username varchar(250) NOT NULL,
    password text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (host, username)
) CHARACTER SET utf8;


CREATE TABLE last (
    host varchar(250) NOT NULL,
    username varchar(250) NOT NULL,
    seconds text NOT NULL,
    state text NOT NULL,
    PRIMARY KEY (host, username)
) CHARACTER SET utf8;


CREATE TABLE rosteritem (
    host varchar(250) NOT NULL,
    user varchar(250) NOT NULL,
    jid varchar(250) NOT NULL,
    name text,
    subscription text NOT NULL,
    ask text NOT NULL,
    askmessage text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (host(75), username(75), jid(75))
) CHARACTER SET utf8;

CREATE INDEX i_rosteru_username ON rosteritem(username);
CREATE INDEX i_rosteru_jid ON rosteritem(jid);

CREATE TABLE rostergroup (
    host varchar(250) NOT NULL,
    username varchar(250) NOT NULL,
    jid varchar(250) NOT NULL,
    grp text NOT NULL,
    PRIMARY KEY (host(75), username(75), jid(75))
) CHARACTER SET utf8;

CREATE TABLE spool (
    host varchar(250) NOT NULL,
    username varchar(250) NOT NULL,
    xml text NOT NULL,
    seq BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (host, username, seq)
) CHARACTER SET utf8;


CREATE TABLE vcard (
    host varchar(250) NOT NULL,
    username varchar(250) NOT NULL,
    vcard mediumtext NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (host, username)
) CHARACTER SET utf8;


CREATE TABLE vcard_search (
    host varchar(250) NOT NULL,
    username varchar(250) NOT NULL,
    lusername varchar(250) NOT NULL,
    fn text NOT NULL,
    lfn varchar(250) NOT NULL,
    family text NOT NULL,
    lfamily varchar(250) NOT NULL,
    given text NOT NULL,
    lgiven varchar(250) NOT NULL,
    middle text NOT NULL,
    lmiddle varchar(250) NOT NULL,
    nickname text NOT NULL,
    lnickname varchar(250) NOT NULL,
    bday text NOT NULL,
    lbday varchar(250) NOT NULL,
    ctry text NOT NULL,
    lctry varchar(250) NOT NULL,
    locality text NOT NULL,
    llocality varchar(250) NOT NULL,
    email text NOT NULL,
    lemail varchar(250) NOT NULL,
    orgname text NOT NULL,
    lorgname varchar(250) NOT NULL,
    orgunit text NOT NULL,
    lorgunit varchar(250) NOT NULL,
    PRIMARY KEY (host, lusername)
) CHARACTER SET utf8;

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
    host varchar(250) NOT NULL,
    username varchar(250),
    name varchar(250) NOT NULL,
    PRIMARY KEY (host, username)
) CHARACTER SET utf8;

CREATE TABLE privacy_list (
    host varchar(250) NOT NULL,
    username varchar(250) NOT NULL,
    name varchar(250) NOT NULL,
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (host, username, name)
) CHARACTER SET utf8;

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
) CHARACTER SET utf8;

CREATE TABLE private_storage (
    host varchar(250) NOT NULL,
    username varchar(250) NOT NULL,
    namespace varchar(250) NOT NULL,
    data text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (host(75), username(75), namespace(75))
) CHARACTER SET utf8;

CREATE INDEX i_private_storage_username USING BTREE ON private_storage(username);

-- Not tested in mysql
CREATE TABLE roster_version (
    username varchar(250) PRIMARY KEY,
    version text NOT NULL
) CHARACTER SET utf8;

-- To update from 1.x:
-- ALTER TABLE rosterusers ADD COLUMN askmessage text AFTER ask;
-- UPDATE rosterusers SET askmessage = '';
-- ALTER TABLE rosterusers ALTER COLUMN askmessage SET NOT NULL;

CREATE TABLE pubsub_node (
  host text,
  node text,
  parent text,
  type text,
  nodeid bigint auto_increment primary key
) CHARACTER SET utf8;
CREATE INDEX i_pubsub_node_parent ON pubsub_node(parent(120));
CREATE UNIQUE INDEX i_pubsub_node_tuple ON pubsub_node(host(20), node(120));

CREATE TABLE pubsub_node_option (
  nodeid bigint,
  name text,
  val text
) CHARACTER SET utf8;
CREATE INDEX i_pubsub_node_option_nodeid ON pubsub_node_option(nodeid);
ALTER TABLE `pubsub_node_option` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_node_owner (
  nodeid bigint,
  owner text
) CHARACTER SET utf8;
CREATE INDEX i_pubsub_node_owner_nodeid ON pubsub_node_owner(nodeid);
ALTER TABLE `pubsub_node_owner` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_state (
  nodeid bigint,
  jid text,
  affiliation character(1),
  subscriptions text,
  stateid bigint auto_increment primary key
) CHARACTER SET utf8;
CREATE INDEX i_pubsub_state_jid ON pubsub_state(jid(60));
CREATE UNIQUE INDEX i_pubsub_state_tuple ON pubsub_state(nodeid, jid(60));
ALTER TABLE `pubsub_state` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_item (
  nodeid bigint,
  itemid text,
  publisher text,
  creation text,
  modification text,
  payload text
) CHARACTER SET utf8;
CREATE INDEX i_pubsub_item_itemid ON pubsub_item(itemid(36));
CREATE UNIQUE INDEX i_pubsub_item_tuple ON pubsub_item(nodeid, itemid(36));
ALTER TABLE `pubsub_item` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_subscription_opt (
  subid text,
  opt_name varchar(32),
  opt_value text
);
CREATE UNIQUE INDEX i_pubsub_subscription_opt ON pubsub_subscription_opt(subid(32), opt_name(32));
