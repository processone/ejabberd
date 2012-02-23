--
-- ejabberd, Copyright (C) 2002-2012   ProcessOne
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

--   WARNING !!!
-- ejabberd creates the tables automatically.
-- This file is obsolete.
-- Read the ejabberd modules source code for up-to-date table schema.

-- Needs MySQL (at least 4.0.x) with innodb back-end
SET table_type=InnoDB;

--
-- Tables schemas keep from previous ejabberd versions
--

CREATE TABLE hosts (
    clusterid integer NOT NULL,
    host varchar(250) NOT NULL PRIMARY KEY,
    config text NOT NULL
) CHARACTER SET utf8;

INSERT INTO hosts (clusterid, host, config)
VALUES (1, 'localhost', '');

-- Not tested in mysql
CREATE TABLE roster_version (
    username varchar(250) PRIMARY KEY,
    version text NOT NULL
) CHARACTER SET utf8;

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

--
-- Tables schemas dumped from gen_storage
--

CREATE TABLE `last_activity` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `timestamp` bigint(20) DEFAULT NULL,
  `status` text,
  PRIMARY KEY (`user`(105),`host`(105))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `muc_online_room` (
  `name` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `pid` text,
  PRIMARY KEY (`name`(105),`host`(105))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `muc_registered` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `nick` text,
  PRIMARY KEY (`user`(105),`host`(105)),
  KEY `muc_registered_nick` (`nick`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `muc_room_affiliation` (
  `name` varchar(255) DEFAULT NULL,
  `host` varchar(255) DEFAULT NULL,
  `jid` varchar(255) DEFAULT NULL,
  `affiliation` varchar(255) DEFAULT NULL,
  `reason` varchar(255) DEFAULT NULL,
  KEY `muc_room_affiliation_bag` (`name`(75),`host`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `muc_room_opt` (
  `name` varchar(255) DEFAULT NULL,
  `host` varchar(255) DEFAULT NULL,
  `opt` varchar(255) DEFAULT NULL,
  `val` varchar(255) DEFAULT NULL,
  KEY `muc_room_opt_bag` (`name`(75),`host`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `offline_msg` (
  `user` varchar(255) DEFAULT NULL,
  `host` varchar(255) DEFAULT NULL,
  `timestamp` bigint(20) DEFAULT NULL,
  `expire` bigint(20) DEFAULT NULL,
  `from` varchar(255) DEFAULT NULL,
  `to` varchar(255) DEFAULT NULL,
  `packet` text,
  KEY `offline_msg_bag` (`user`(75),`host`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `passwd` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `password` text,
  `storedkey` text,
  `serverkey` text,
  `salt` text,
  `iterationcount` int(11) DEFAULT NULL,
  PRIMARY KEY (`user`(105),`host`(105))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `privacy_default_list` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `name` text,
  PRIMARY KEY (`user`(105),`host`(105))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `privacy_list` (
  `user` varchar(255) DEFAULT NULL,
  `host` varchar(255) DEFAULT NULL,
  `name` varchar(255) DEFAULT NULL,
  KEY `privacy_list_bag` (`user`(75),`host`(75)),
  KEY `privacy_list_name` (`name`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `privacy_list_data` (
  `user` varchar(255) DEFAULT NULL,
  `host` varchar(255) DEFAULT NULL,
  `name` varchar(255) DEFAULT NULL,
  `type` varchar(255) DEFAULT NULL,
  `value` varchar(255) DEFAULT NULL,
  `action` varchar(255) DEFAULT NULL,
  `order` int(11) DEFAULT NULL,
  `match_all` varchar(255) DEFAULT NULL,
  `match_iq` varchar(255) DEFAULT NULL,
  `match_message` varchar(255) DEFAULT NULL,
  `match_presence_in` varchar(255) DEFAULT NULL,
  `match_presence_out` varchar(255) DEFAULT NULL,
  KEY `privacy_list_data_bag` (`user`(75),`host`(75)),
  KEY `privacy_list_data_name` (`name`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `private_storage` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `ns` varchar(255) NOT NULL DEFAULT '',
  `xml` text,
  PRIMARY KEY (`user`(105),`host`(105),`ns`(105))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `rostergroup` (
  `user` varchar(255) DEFAULT NULL,
  `host` varchar(255) DEFAULT NULL,
  `jid` varchar(255) DEFAULT NULL,
  `grp` varchar(255) DEFAULT NULL,
  KEY `rostergroup_bag` (`user`(75),`host`(75),`jid`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `rosteritem` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `jid` varchar(255) NOT NULL DEFAULT '',
  `name` text,
  `subscription` text,
  `ask` text,
  `askmessage` text,
  PRIMARY KEY (`user`(105),`host`(105),`jid`(105))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `vcard` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `vcard` text,
  PRIMARY KEY (`user`(105),`host`(105))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE `vcard_search` (
  `user` varchar(255) NOT NULL DEFAULT '',
  `host` varchar(255) NOT NULL DEFAULT '',
  `username` text,
  `lusername` text,
  `fn` text,
  `lfn` text,
  `family` text,
  `lfamily` text,
  `given` text,
  `lgiven` text,
  `middle` text,
  `lmiddle` text,
  `nickname` text,
  `lnickname` text,
  `bday` text,
  `lbday` text,
  `ctry` text,
  `lctry` text,
  `locality` text,
  `llocality` text,
  `email` text,
  `lemail` text,
  `orgname` text,
  `lorgname` text,
  `orgunit` text,
  `lorgunit` text,
  PRIMARY KEY (`user`(105),`host`(105)),
  KEY `vcard_search_lusername` (`lusername`(75)),
  KEY `vcard_search_lfn` (`lfn`(75)),
  KEY `vcard_search_lfamily` (`lfamily`(75)),
  KEY `vcard_search_lgiven` (`lgiven`(75)),
  KEY `vcard_search_lmiddle` (`lmiddle`(75)),
  KEY `vcard_search_lnickname` (`lnickname`(75)),
  KEY `vcard_search_lbday` (`lbday`(75)),
  KEY `vcard_search_lctry` (`lctry`(75)),
  KEY `vcard_search_llocality` (`llocality`(75)),
  KEY `vcard_search_lemail` (`lemail`(75)),
  KEY `vcard_search_lorgname` (`lorgname`(75)),
  KEY `vcard_search_lorgunit` (`lorgunit`(75))
) ENGINE=MyISAM DEFAULT CHARSET=utf8;
