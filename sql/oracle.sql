--
-- ejabberd, Copyright (C) 2002-2015   ProcessOne
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
-- Script for Oracle Database
-- Author: carlostimoshenkorodrigueslopes@gmail.com

CREATE TABLE users (
    username varchar2(255) PRIMARY KEY,
    "password" varchar2(255) NOT NULL,
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);

-- To support SCRAM auth:
-- ALTER TABLE USERS ADD (serverkey VARCHAR2(255) DEFAULT '' NOT NULL);
-- ALTER TABLE USERS ADD (salt VARCHAR2(255) DEFAULT '' NOT NULL);
-- ALTER TABLE USERS ADD (iterationcount INTEGER DEFAULT 0 NOT NULL);

CREATE TABLE last (
    username varchar2(255) PRIMARY KEY,
    seconds varchar2(255) NOT NULL,
    state varchar2(255) NOT NULL
);


CREATE TABLE rosterusers (
    username varchar2(255) NOT NULL,
    jid varchar2(255) NOT NULL,
    nick varchar2(255) NOT NULL,
    subscription char(1) NOT NULL,
    ask char(1) NOT NULL,
    askmessage varchar2(255) NOT NULL,
    server char(1) NOT NULL,
    subscribe varchar2(255),
    "type" varchar2(255),
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);
CREATE UNIQUE INDEX i_rosteru_user_jid ON rosterusers (username ASC, jid ASC) PARALLEL COMPRESS;
CREATE INDEX i_rosteru_username ON rosterusers (username ASC) PARALLEL COMPRESS;
CREATE INDEX i_rosteru_jid ON rosterusers (jid ASC) PARALLEL COMPRESS;

CREATE TABLE rostergroups (
    username varchar2(255) NOT NULL,
    jid varchar2(255) NOT NULL,
    grp varchar2(255) NOT NULL
);
CREATE INDEX pk_rosterg_user_jid ON rostergroups (username ASC, jid ASC) PARALLEL COMPRESS;

CREATE TABLE sr_group (
    name varchar2(255) NOT NULL,
    opts varchar2(255) NOT NULL,
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);

CREATE TABLE sr_user (
    jid varchar2(255) NOT NULL,
    grp varchar2(255) NOT NULL,
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);
CREATE UNIQUE INDEX i_sr_user_jid_grp ON sr_user (jid ASC, grp ASC) PARALLEL COMPRESS;
CREATE INDEX i_sr_user_jid ON sr_user (jid ASC) PARALLEL COMPRESS;
CREATE INDEX i_sr_user_grp ON sr_user (grp ASC) PARALLEL COMPRESS;

CREATE TABLE spool (
    username varchar2(255) NOT NULL,
    xml varchar2(255) NOT NULL,
    seq NUMERIC(10),
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);
CREATE SEQUENCE tb_spool_sequence;
CREATE TRIGGER tb_spool_seq_trigger 
BEFORE INSERT ON spool 
FOR EACH ROW WHEN (new.seq IS NULL)
BEGIN
  <<COLUMN_SEQUENCES>>
  BEGIN
    SELECT tb_spool_sequence.NEXTVAL INTO :new.seq FROM SYS.DUAL;
  END COLUMN_SEQUENCES;
END;
/
CREATE INDEX i_despool ON spool (username ASC) PARALLEL COMPRESS;

CREATE TABLE archive (
    username varchar2(255) NOT NULL,
    "timestamp" INTEGER NOT NULL,
    peer varchar2(255) NOT NULL,
    bare_peer varchar2(255) NOT NULL,
    xml varchar2(4000) NOT NULL,
    txt varchar2(4000),
    id NUMERIC(10),
    kind varchar2(255),
    nick varchar2(255),
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);
CREATE SEQUENCE tb_archive_sequence;
CREATE TRIGGER tb_archive_seq_trigger 
BEFORE INSERT ON archive 
FOR EACH ROW WHEN (new.id IS NULL)
BEGIN
  <<COLUMN_SEQUENCES>>
  BEGIN
    SELECT tb_archive_sequence.NEXTVAL INTO :new.id FROM SYS.DUAL;
  END COLUMN_SEQUENCES;
END;
/
CREATE INDEX i_username ON archive (username ASC) PARALLEL COMPRESS;
CREATE INDEX i_timestamp ON archive ("timestamp" ASC) PARALLEL COMPRESS;
CREATE INDEX i_peer ON archive (peer ASC) PARALLEL COMPRESS;
CREATE INDEX i_bare_peer ON archive (bare_peer ASC) PARALLEL COMPRESS;

CREATE TABLE archive_prefs (
    username varchar2(255) NOT NULL PRIMARY KEY,
    def varchar2(255) NOT NULL,
    always varchar2(255) NOT NULL,
    never varchar2(255) NOT NULL,
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);

CREATE TABLE vcard (
    username varchar2(255) PRIMARY KEY,
    vcard varchar2(255) NOT NULL,
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);

CREATE TABLE vcard_xupdate (
    username varchar2(255) PRIMARY KEY,
    hash varchar2(255) NOT NULL,
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);

CREATE TABLE vcard_search (
    username varchar2(255) NOT NULL,
    lusername varchar2(255) PRIMARY KEY,
    fn varchar2(255) NOT NULL,
    lfn varchar2(255) NOT NULL,
    family varchar2(255) NOT NULL,
    lfamily varchar2(255) NOT NULL,
    given varchar2(255) NOT NULL,
    lgiven varchar2(255) NOT NULL,
    middle varchar2(255) NOT NULL,
    lmiddle varchar2(255) NOT NULL,
    nickname varchar2(255) NOT NULL,
    lnickname varchar2(255) NOT NULL,
    bday varchar2(255) NOT NULL,
    lbday varchar2(255) NOT NULL,
    ctry varchar2(255) NOT NULL,
    lctry varchar2(255) NOT NULL,
    locality varchar2(255) NOT NULL,
    llocality varchar2(255) NOT NULL,
    email varchar2(255) NOT NULL,
    lemail varchar2(255) NOT NULL,
    orgname varchar2(255) NOT NULL,
    lorgname varchar2(255) NOT NULL,
    orgunit varchar2(255) NOT NULL,
    lorgunit varchar2(255) NOT NULL
);
CREATE INDEX i_vcard_search_lfn       ON vcard_search(lfn ASC) PARALLEL COMPRESS;
CREATE INDEX i_vcard_search_lfamily   ON vcard_search(lfamily ASC) PARALLEL COMPRESS;
CREATE INDEX i_vcard_search_lgiven    ON vcard_search(lgiven ASC) PARALLEL COMPRESS;
CREATE INDEX i_vcard_search_lmiddle   ON vcard_search(lmiddle ASC) PARALLEL COMPRESS;
CREATE INDEX i_vcard_search_lnickname ON vcard_search(lnickname ASC) PARALLEL COMPRESS;
CREATE INDEX i_vcard_search_lbday     ON vcard_search(lbday ASC) PARALLEL COMPRESS;
CREATE INDEX i_vcard_search_lctry     ON vcard_search(lctry ASC) PARALLEL COMPRESS;
CREATE INDEX i_vcard_search_llocality ON vcard_search(llocality ASC) PARALLEL COMPRESS;
CREATE INDEX i_vcard_search_lemail    ON vcard_search(lemail ASC) PARALLEL COMPRESS;
CREATE INDEX i_vcard_search_lorgname  ON vcard_search(lorgname ASC) PARALLEL COMPRESS;
CREATE INDEX i_vcard_search_lorgunit  ON vcard_search(lorgunit ASC) PARALLEL COMPRESS;

CREATE TABLE privacy_default_list (
    username varchar2(255) PRIMARY KEY,
    name varchar2(255) NOT NULL
);

CREATE TABLE privacy_list (
    username varchar2(255) NOT NULL,
    name varchar2(255) NOT NULL,
    id NUMERIC(10) UNIQUE,
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);
CREATE SEQUENCE tb_privacy_list_sequence;
CREATE TRIGGER tb_privacy_list_seq_trigger 
BEFORE INSERT ON privacy_list 
FOR EACH ROW WHEN (new.id IS NULL)
BEGIN
  <<COLUMN_SEQUENCES>>
  BEGIN
    SELECT tb_privacy_list_sequence.NEXTVAL INTO :new.id FROM SYS.DUAL;
  END COLUMN_SEQUENCES;
END;
/
CREATE INDEX i_privacy_list_username ON privacy_list (username ASC) PARALLEL COMPRESS;
CREATE UNIQUE INDEX i_privacy_list_username_name ON privacy_list (username ASC, name ASC) PARALLEL COMPRESS;

CREATE TABLE privacy_list_data (
    id INTEGER REFERENCES privacy_list(id) ON DELETE CASCADE,
    t char(1) NOT NULL,
    value varchar2(255) NOT NULL,
    action char(1) NOT NULL,
    ord NUMERIC(10) NOT NULL,
    match_all char(1) NOT NULL,
    match_iq char(1) NOT NULL,
    match_message char(1) NOT NULL,
    match_presence_in char(1) NOT NULL,
    match_presence_out char(1) NOT NULL
);
CREATE INDEX i_privacy_list_data_id ON privacy_list_data (id ASC) PARALLEL COMPRESS;

CREATE TABLE private_storage (
    username varchar2(255) NOT NULL,
    namespace varchar2(255) NOT NULL,
    data varchar2(255) NOT NULL,
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);
CREATE INDEX i_private_storage_username ON private_storage (username ASC) PARALLEL COMPRESS;
CREATE UNIQUE INDEX i_private_storage_uname_nspace ON private_storage (username ASC, namespace ASC) PARALLEL COMPRESS;

CREATE TABLE roster_version (
    username varchar2(255) PRIMARY KEY,
    version varchar2(255) NOT NULL
);

CREATE TABLE pubsub_node (
  host varchar2(255),
  node varchar2(255),
  parent varchar2(255),
  "type" varchar2(255),
  nodeid NUMERIC(10) UNIQUE
);
CREATE SEQUENCE tb_pubsub_node_sequence;
CREATE TRIGGER tb_pubsub_node_seq_trigger 
BEFORE INSERT ON pubsub_node 
FOR EACH ROW WHEN (new.nodeid IS NULL) 
BEGIN
  <<COLUMN_SEQUENCES>>
  BEGIN
    SELECT tb_pubsub_node_sequence.NEXTVAL INTO :new.nodeid FROM SYS.DUAL;
  END COLUMN_SEQUENCES;
END;
/
CREATE INDEX i_pubsub_node_parent ON pubsub_node (parent ASC) PARALLEL COMPRESS;
CREATE UNIQUE INDEX i_pubsub_node_tuple ON pubsub_node (host ASC, node ASC) PARALLEL COMPRESS;

CREATE TABLE pubsub_node_option (
  nodeid INTEGER REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  name varchar2(255),
  val varchar2(255)
);
CREATE INDEX i_pubsub_node_option_nodeid ON pubsub_node_option (nodeid ASC) PARALLEL COMPRESS;

CREATE TABLE pubsub_node_owner (
  nodeid INTEGER REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  owner varchar2(255)
);
CREATE INDEX i_pubsub_node_owner_nodeid ON pubsub_node_owner (nodeid ASC) PARALLEL COMPRESS;

CREATE TABLE pubsub_state (
  nodeid INTEGER REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  jid varchar2(255),
  affiliation char(1),
  subscriptions varchar2(255),
  stateid NUMERIC(10) UNIQUE
);
CREATE SEQUENCE tb_pubsub_state_sequence;
CREATE TRIGGER tb_pubsub_state_seq_trigger 
BEFORE INSERT ON pubsub_state 
FOR EACH ROW WHEN (new.stateid IS NULL)
BEGIN
  <<COLUMN_SEQUENCES>>
  BEGIN
    SELECT tb_spool_sequence.NEXTVAL INTO :new.stateid FROM SYS.DUAL;
  END COLUMN_SEQUENCES;
END;
/
CREATE INDEX i_pubsub_state_jid ON pubsub_state (jid ASC) PARALLEL COMPRESS;
CREATE UNIQUE INDEX i_pubsub_state_tuple ON pubsub_state (nodeid ASC, jid ASC) PARALLEL COMPRESS;

CREATE TABLE pubsub_item (
  nodeid INTEGER REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  itemid varchar2(255), 
  publisher varchar2(255),
  creation varchar2(255),
  modification varchar2(255),
  payload varchar2(4000)
);
CREATE INDEX i_pubsub_item_itemid ON pubsub_item (itemid ASC) PARALLEL COMPRESS;
CREATE UNIQUE INDEX i_pubsub_item_tuple ON pubsub_item (nodeid ASC, itemid ASC) PARALLEL COMPRESS;

CREATE TABLE pubsub_subscription_opt (
  subid varchar2(255),
  opt_name varchar2(32),
  opt_value varchar2(1024)
);
CREATE UNIQUE INDEX i_pubsub_subscription_opt ON pubsub_subscription_opt (subid ASC, opt_name ASC) PARALLEL COMPRESS;

CREATE TABLE muc_room (
    name varchar2(255) NOT NULL,
    host varchar2(255) NOT NULL,
    opts varchar2(255) NOT NULL,
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);
CREATE UNIQUE INDEX i_muc_room_name_host ON muc_room (name ASC, host ASC) PARALLEL COMPRESS;

CREATE TABLE muc_registered (
    jid varchar2(255) NOT NULL,
    host varchar2(255) NOT NULL,
    nick varchar2(255) NOT NULL,
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);
CREATE INDEX i_muc_registered_nick ON muc_registered (nick ASC) PARALLEL COMPRESS;
CREATE UNIQUE INDEX i_muc_registered_jid_host ON muc_registered (jid ASC, host ASC) PARALLEL COMPRESS;

CREATE TABLE irc_custom (
    jid varchar2(255) NOT NULL,
    host varchar2(255) NOT NULL,
    data varchar2(255) NOT NULL,
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);
CREATE UNIQUE INDEX i_irc_custom_jid_host ON irc_custom (jid ASC, host ASC) PARALLEL COMPRESS;

CREATE TABLE motd (
    username varchar2(255) PRIMARY KEY,
    xml varchar2(4000),
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);

CREATE TABLE caps_features (
    node varchar2(255) NOT NULL,
    subnode varchar2(255) NOT NULL,
    feature varchar2(255),
    created_at TIMESTAMP DEFAULT sysdate NOT NULL
);
CREATE INDEX i_caps_features_node_subnode ON caps_features (node ASC, subnode ASC) PARALLEL COMPRESS;

CREATE TABLE sm (
    usec INTEGER NOT NULL,
    pid varchar2(255) NOT NULL,
    node varchar2(255) NOT NULL,
    username varchar2(255) NOT NULL,
    "resource" varchar2(255) NOT NULL,
    "priority" varchar2(255) NOT NULL,
    info varchar2(4000) NOT NULL
);
CREATE UNIQUE INDEX i_sm_sid ON sm (usec ASC, pid ASC) PARALLEL COMPRESS;
CREATE INDEX i_sm_node ON sm (node ASC) PARALLEL COMPRESS;
CREATE INDEX i_sm_username ON sm (username ASC) PARALLEL COMPRESS;
