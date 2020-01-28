--
-- ejabberd, Copyright (C) 2002-2020   ProcessOne
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

-- To update from the old schema, replace <HOST> with the host's domain:

-- ALTER TABLE users ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- ALTER TABLE users DROP CONSTRAINT users_pkey;
-- ALTER TABLE users ADD PRIMARY KEY (server_host, username);
-- ALTER TABLE users ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE last ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- ALTER TABLE last DROP CONSTRAINT last_pkey;
-- ALTER TABLE last ADD PRIMARY KEY (server_host, username);
-- ALTER TABLE last ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE rosterusers ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- DROP INDEX i_rosteru_user_jid;
-- DROP INDEX i_rosteru_username;
-- DROP INDEX i_rosteru_jid;
-- CREATE UNIQUE INDEX i_rosteru_sh_user_jid ON rosterusers USING btree (server_host, username, jid);
-- CREATE INDEX i_rosteru_sh_username ON rosterusers USING btree (server_host, username);
-- CREATE INDEX i_rosteru_sh_jid ON rosterusers USING btree (server_host, jid);
-- ALTER TABLE rosterusers ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE rostergroups ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- DROP INDEX pk_rosterg_user_jid;
-- CREATE INDEX i_rosterg_sh_user_jid ON rostergroups USING btree (server_host, username, jid);
-- ALTER TABLE rostergroups ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE sr_group ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- ALTER TABLE sr_group ADD PRIMARY KEY (server_host, name);
-- ALTER TABLE sr_group ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE sr_user ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- DROP INDEX i_sr_user_jid_grp;
-- DROP INDEX i_sr_user_jid;
-- DROP INDEX i_sr_user_grp;
-- ALTER TABLE sr_user ADD PRIMARY KEY (server_host, jid, grp);
-- CREATE INDEX i_sr_user_sh_jid ON sr_user USING btree (server_host, jid);
-- CREATE INDEX i_sr_user_sh_grp ON sr_user USING btree (server_host, grp);
-- ALTER TABLE sr_user ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE spool ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- DROP INDEX i_despool;
-- CREATE INDEX i_spool_sh_username ON spool USING btree (server_host, username);
-- ALTER TABLE spool ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE archive ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- DROP INDEX i_username_timestamp;
-- DROP INDEX i_username_peer;
-- DROP INDEX i_username_bare_peer;
-- DROP INDEX i_timestamp;
-- CREATE INDEX i_archive_sh_username_timestamp ON archive USING btree (server_host, username, timestamp);
-- CREATE INDEX i_archive_sh_username_peer ON archive USING btree (server_host, username, peer);
-- CREATE INDEX i_archive_sh_username_bare_peer ON archive USING btree (server_host, username, bare_peer);
-- CREATE INDEX i_archive_sh_timestamp ON archive USING btree (server_host, timestamp);
-- ALTER TABLE archive ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE archive_prefs ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- ALTER TABLE archive_prefs DROP CONSTRAINT archive_prefs_pkey;
-- ALTER TABLE archive_prefs ADD PRIMARY KEY (server_host, username);
-- ALTER TABLE archive_prefs ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE vcard ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- ALTER TABLE vcard DROP CONSTRAINT vcard_pkey;
-- ALTER TABLE vcard ADD PRIMARY KEY (server_host, username);
-- ALTER TABLE vcard ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE vcard_search ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- ALTER TABLE vcard_search DROP CONSTRAINT vcard_search_pkey;
-- DROP INDEX i_vcard_search_lfn;
-- DROP INDEX i_vcard_search_lfamily;
-- DROP INDEX i_vcard_search_lgiven;
-- DROP INDEX i_vcard_search_lmiddle;
-- DROP INDEX i_vcard_search_lnickname;
-- DROP INDEX i_vcard_search_lbday;
-- DROP INDEX i_vcard_search_lctry;
-- DROP INDEX i_vcard_search_llocality;
-- DROP INDEX i_vcard_search_lemail;
-- DROP INDEX i_vcard_search_lorgname;
-- DROP INDEX i_vcard_search_lorgunit;
-- ALTER TABLE vcard_search ADD PRIMARY KEY (server_host, username);
-- CREATE INDEX i_vcard_search_sh_lfn       ON vcard_search(server_host, lfn);
-- CREATE INDEX i_vcard_search_sh_lfamily   ON vcard_search(server_host, lfamily);
-- CREATE INDEX i_vcard_search_sh_lgiven    ON vcard_search(server_host, lgiven);
-- CREATE INDEX i_vcard_search_sh_lmiddle   ON vcard_search(server_host, lmiddle);
-- CREATE INDEX i_vcard_search_sh_lnickname ON vcard_search(server_host, lnickname);
-- CREATE INDEX i_vcard_search_sh_lbday     ON vcard_search(server_host, lbday);
-- CREATE INDEX i_vcard_search_sh_lctry     ON vcard_search(server_host, lctry);
-- CREATE INDEX i_vcard_search_sh_llocality ON vcard_search(server_host, llocality);
-- CREATE INDEX i_vcard_search_sh_lemail    ON vcard_search(server_host, lemail);
-- CREATE INDEX i_vcard_search_sh_lorgname  ON vcard_search(server_host, lorgname);
-- CREATE INDEX i_vcard_search_sh_lorgunit  ON vcard_search(server_host, lorgunit);
-- ALTER TABLE vcard_search ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE privacy_default_list ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- ALTER TABLE privacy_default_list DROP CONSTRAINT privacy_default_list_pkey;
-- ALTER TABLE privacy_default_list ADD PRIMARY KEY (server_host, username);
-- ALTER TABLE privacy_default_list ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE privacy_list ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- DROP INDEX i_privacy_list_username;
-- DROP INDEX i_privacy_list_username_name;
-- CREATE INDEX i_privacy_list_sh_username ON privacy_list USING btree (server_host, username);
-- CREATE UNIQUE INDEX i_privacy_list_sh_username_name ON privacy_list USING btree (server_host, username, name);
-- ALTER TABLE privacy_list ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE private_storage ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- DROP INDEX i_private_storage_username;
-- DROP INDEX i_private_storage_username_namespace;
-- ALTER TABLE private_storage ADD PRIMARY KEY (server_host, username, namespace);
-- CREATE INDEX i_private_storage_sh_username ON private_storage USING btree (server_host, username);
-- ALTER TABLE private_storage ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE roster_version ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- ALTER TABLE roster_version DROP CONSTRAINT roster_version_pkey;
-- ALTER TABLE roster_version ADD PRIMARY KEY (server_host, username);
-- ALTER TABLE roster_version ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE muc_room ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- ALTER TABLE muc_room ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE muc_registered ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- ALTER TABLE muc_registered ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE muc_online_room ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- ALTER TABLE muc_online_room ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE muc_online_users ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- ALTER TABLE muc_online_users ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE motd ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- ALTER TABLE motd DROP CONSTRAINT motd_pkey;
-- ALTER TABLE motd ADD PRIMARY KEY (server_host, username);
-- ALTER TABLE motd ALTER COLUMN server_host DROP DEFAULT;

-- ALTER TABLE sm ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
-- DROP INDEX i_sm_sid;
-- DROP INDEX i_sm_username;
-- ALTER TABLE sm ADD PRIMARY KEY (usec, pid);
-- CREATE INDEX i_sm_sh_username ON sm USING btree (server_host, username);
-- ALTER TABLE sm ALTER COLUMN server_host DROP DEFAULT;


CREATE TABLE users (
    username text NOT NULL,
    server_host text NOT NULL,
    "password" text NOT NULL,
    serverkey text NOT NULL DEFAULT '',
    salt text NOT NULL DEFAULT '',
    iterationcount integer NOT NULL DEFAULT 0,
    created_at TIMESTAMP NOT NULL DEFAULT now(),
    PRIMARY KEY (server_host, username)
);

-- Add support for SCRAM auth to a database created before ejabberd 16.03:
-- ALTER TABLE users ADD COLUMN serverkey text NOT NULL DEFAULT '';
-- ALTER TABLE users ADD COLUMN salt text NOT NULL DEFAULT '';
-- ALTER TABLE users ADD COLUMN iterationcount integer NOT NULL DEFAULT 0;

CREATE TABLE last (
    username text NOT NULL,
    server_host text NOT NULL,
    seconds text NOT NULL,
    state text NOT NULL,
    PRIMARY KEY (server_host, username)
);


CREATE TABLE rosterusers (
    username text NOT NULL,
    server_host text NOT NULL,
    jid text NOT NULL,
    nick text NOT NULL,
    subscription character(1) NOT NULL,
    ask character(1) NOT NULL,
    askmessage text NOT NULL,
    server character(1) NOT NULL,
    subscribe text NOT NULL,
    "type" text,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX i_rosteru_sh_user_jid ON rosterusers USING btree (server_host, username, jid);
CREATE INDEX i_rosteru_sh_username ON rosterusers USING btree (server_host, username);
CREATE INDEX i_rosteru_sh_jid ON rosterusers USING btree (server_host, jid);


CREATE TABLE rostergroups (
    username text NOT NULL,
    server_host text NOT NULL,
    jid text NOT NULL,
    grp text NOT NULL
);

CREATE INDEX i_rosterg_sh_user_jid ON rostergroups USING btree (server_host, username, jid);

CREATE TABLE sr_group (
    name text NOT NULL,
    server_host text NOT NULL,
    opts text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now(),
    PRIMARY KEY (server_host, name)
);

CREATE TABLE sr_user (
    jid text NOT NULL,
    server_host text NOT NULL,
    grp text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now(),
    PRIMARY KEY (server_host, jid, grp)
);

CREATE INDEX i_sr_user_sh_jid ON sr_user USING btree (server_host, jid);
CREATE INDEX i_sr_user_sh_grp ON sr_user USING btree (server_host, grp);

CREATE TABLE spool (
    username text NOT NULL,
    server_host text NOT NULL,
    xml text NOT NULL,
    seq SERIAL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_spool_sh_username ON spool USING btree (server_host, username);

CREATE TABLE archive (
    username text NOT NULL,
    server_host text NOT NULL,
    timestamp BIGINT NOT NULL,
    peer text NOT NULL,
    bare_peer text NOT NULL,
    xml text NOT NULL,
    txt text,
    id SERIAL,
    kind text,
    nick text,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_archive_sh_username_timestamp ON archive USING btree (server_host, username, timestamp);
CREATE INDEX i_archive_sh_username_peer ON archive USING btree (server_host, username, peer);
CREATE INDEX i_archive_sh_username_bare_peer ON archive USING btree (server_host, username, bare_peer);
CREATE INDEX i_archive_sh_timestamp ON archive USING btree (server_host, timestamp);

CREATE TABLE archive_prefs (
    username text NOT NULL,
    server_host text NOT NULL,
    def text NOT NULL,
    always text NOT NULL,
    never text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now(),
    PRIMARY KEY (server_host, username)
);

CREATE TABLE vcard (
    username text NOT NULL,
    server_host text NOT NULL,
    vcard text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now(),
    PRIMARY KEY (server_host, username)
);

CREATE TABLE vcard_search (
    username text NOT NULL,
    lusername text NOT NULL,
    server_host text NOT NULL,
    fn text NOT NULL,
    lfn text NOT NULL,
    "family" text NOT NULL,
    lfamily text NOT NULL,
    given text NOT NULL,
    lgiven text NOT NULL,
    middle text NOT NULL,
    lmiddle text NOT NULL,
    nickname text NOT NULL,
    lnickname text NOT NULL,
    bday text NOT NULL,
    lbday text NOT NULL,
    ctry text NOT NULL,
    lctry text NOT NULL,
    locality text NOT NULL,
    llocality text NOT NULL,
    email text NOT NULL,
    lemail text NOT NULL,
    orgname text NOT NULL,
    lorgname text NOT NULL,
    orgunit text NOT NULL,
    lorgunit text NOT NULL,
    PRIMARY KEY (server_host, username)
);

CREATE INDEX i_vcard_search_sh_lfn       ON vcard_search(server_host, lfn);
CREATE INDEX i_vcard_search_sh_lfamily   ON vcard_search(server_host, lfamily);
CREATE INDEX i_vcard_search_sh_lgiven    ON vcard_search(server_host, lgiven);
CREATE INDEX i_vcard_search_sh_lmiddle   ON vcard_search(server_host, lmiddle);
CREATE INDEX i_vcard_search_sh_lnickname ON vcard_search(server_host, lnickname);
CREATE INDEX i_vcard_search_sh_lbday     ON vcard_search(server_host, lbday);
CREATE INDEX i_vcard_search_sh_lctry     ON vcard_search(server_host, lctry);
CREATE INDEX i_vcard_search_sh_llocality ON vcard_search(server_host, llocality);
CREATE INDEX i_vcard_search_sh_lemail    ON vcard_search(server_host, lemail);
CREATE INDEX i_vcard_search_sh_lorgname  ON vcard_search(server_host, lorgname);
CREATE INDEX i_vcard_search_sh_lorgunit  ON vcard_search(server_host, lorgunit);

CREATE TABLE privacy_default_list (
    username text NOT NULL,
    server_host text NOT NULL,
    name text NOT NULL,
    PRIMARY KEY (server_host, username)
);

CREATE TABLE privacy_list (
    username text NOT NULL,
    server_host text NOT NULL,
    name text NOT NULL,
    id SERIAL UNIQUE,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_privacy_list_sh_username ON privacy_list USING btree (server_host, username);
CREATE UNIQUE INDEX i_privacy_list_sh_username_name ON privacy_list USING btree (server_host, username, name);

CREATE TABLE privacy_list_data (
    id bigint REFERENCES privacy_list(id) ON DELETE CASCADE,
    t character(1) NOT NULL,
    value text NOT NULL,
    action character(1) NOT NULL,
    ord NUMERIC NOT NULL,
    match_all boolean NOT NULL,
    match_iq boolean NOT NULL,
    match_message boolean NOT NULL,
    match_presence_in boolean NOT NULL,
    match_presence_out boolean NOT NULL
);

CREATE INDEX i_privacy_list_data_id ON privacy_list_data USING btree (id);

CREATE TABLE private_storage (
    username text NOT NULL,
    server_host text NOT NULL,
    namespace text NOT NULL,
    data text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now(),
    PRIMARY KEY (server_host, username, namespace)
);

CREATE INDEX i_private_storage_sh_username ON private_storage USING btree (server_host, username);


CREATE TABLE roster_version (
    username text NOT NULL,
    server_host text NOT NULL,
    version text NOT NULL,
    PRIMARY KEY (server_host, username)
);

-- To update from 0.9.8:
-- CREATE SEQUENCE spool_seq_seq;
-- ALTER TABLE spool ADD COLUMN seq integer;
-- ALTER TABLE spool ALTER COLUMN seq SET DEFAULT nextval('spool_seq_seq');
-- UPDATE spool SET seq = DEFAULT;
-- ALTER TABLE spool ALTER COLUMN seq SET NOT NULL;

-- To update from 1.x:
-- ALTER TABLE rosterusers ADD COLUMN askmessage text;
-- UPDATE rosterusers SET askmessage = '';
-- ALTER TABLE rosterusers ALTER COLUMN askmessage SET NOT NULL;

CREATE TABLE pubsub_node (
  host text NOT NULL,
  node text NOT NULL,
  parent text NOT NULL DEFAULT '',
  plugin text NOT NULL,
  nodeid SERIAL UNIQUE
);
CREATE INDEX i_pubsub_node_parent ON pubsub_node USING btree (parent);
CREATE UNIQUE INDEX i_pubsub_node_tuple ON pubsub_node USING btree (host, node);

CREATE TABLE pubsub_node_option (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  name text NOT NULL,
  val text NOT NULL
);
CREATE INDEX i_pubsub_node_option_nodeid ON pubsub_node_option USING btree (nodeid);

CREATE TABLE pubsub_node_owner (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  owner text NOT NULL
);
CREATE INDEX i_pubsub_node_owner_nodeid ON pubsub_node_owner USING btree (nodeid);

CREATE TABLE pubsub_state (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  jid text NOT NULL,
  affiliation character(1),
  subscriptions text NOT NULL DEFAULT '',
  stateid SERIAL UNIQUE
);
CREATE INDEX i_pubsub_state_jid ON pubsub_state USING btree (jid);
CREATE UNIQUE INDEX i_pubsub_state_tuple ON pubsub_state USING btree (nodeid, jid);

CREATE TABLE pubsub_item (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  itemid text NOT NULL,
  publisher text NOT NULL,
  creation varchar(32) NOT NULL,
  modification varchar(32) NOT NULL,
  payload text NOT NULL DEFAULT ''
);
CREATE INDEX i_pubsub_item_itemid ON pubsub_item USING btree (itemid);
CREATE UNIQUE INDEX i_pubsub_item_tuple ON pubsub_item USING btree (nodeid, itemid);

CREATE TABLE pubsub_subscription_opt (
  subid text NOT NULL,
  opt_name varchar(32),
  opt_value text NOT NULL
);
CREATE UNIQUE INDEX i_pubsub_subscription_opt ON pubsub_subscription_opt USING btree (subid, opt_name);

CREATE TABLE muc_room (
    name text NOT NULL,
    host text NOT NULL,
    server_host text NOT NULL,
    opts text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX i_muc_room_name_host ON muc_room USING btree (name, host);

CREATE TABLE muc_registered (
    jid text NOT NULL,
    host text NOT NULL,
    server_host text NOT NULL,
    nick text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_muc_registered_nick ON muc_registered USING btree (nick);
CREATE UNIQUE INDEX i_muc_registered_jid_host ON muc_registered USING btree (jid, host);

CREATE TABLE muc_online_room (
    name text NOT NULL,
    host text NOT NULL,
    server_host text NOT NULL,
    node text NOT NULL,
    pid text NOT NULL
);

CREATE UNIQUE INDEX i_muc_online_room_name_host ON muc_online_room USING btree (name, host);

CREATE TABLE muc_online_users (
    username text NOT NULL,
    server text NOT NULL,
    resource text NOT NULL,
    name text NOT NULL,
    host text NOT NULL,
    server_host text NOT NULL,
    node text NOT NULL
);

CREATE UNIQUE INDEX i_muc_online_users ON muc_online_users USING btree (username, server, resource, name, host);
CREATE INDEX i_muc_online_users_us ON muc_online_users USING btree (username, server);

CREATE TABLE muc_room_subscribers (
   room text NOT NULL,
   host text NOT NULL,
   jid text NOT NULL,
   nick text NOT NULL,
   nodes text NOT NULL,
   created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_muc_room_subscribers_host_jid ON muc_room_subscribers USING btree (host, jid);
CREATE UNIQUE INDEX i_muc_room_subscribers_host_room_jid ON muc_room_subscribers USING btree (host, room, jid);

CREATE TABLE motd (
    username text NOT NULL,
    server_host text NOT NULL,
    xml text,
    created_at TIMESTAMP NOT NULL DEFAULT now(),
    PRIMARY KEY (server_host, username)
);

CREATE TABLE caps_features (
    node text NOT NULL,
    subnode text NOT NULL,
    feature text,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_caps_features_node_subnode ON caps_features USING btree (node, subnode);

CREATE TABLE sm (
    usec bigint NOT NULL,
    pid text NOT NULL,
    node text NOT NULL,
    username text NOT NULL,
    server_host text NOT NULL,
    resource text NOT NULL,
    priority text NOT NULL,
    info text NOT NULL,
    PRIMARY KEY (usec, pid)
);

CREATE INDEX i_sm_node ON sm USING btree (node);
CREATE INDEX i_sm_sh_username ON sm USING btree (server_host, username);

CREATE TABLE oauth_token (
    token text NOT NULL,
    jid text NOT NULL,
    scope text NOT NULL,
    expire bigint NOT NULL
);

CREATE UNIQUE INDEX i_oauth_token_token ON oauth_token USING btree (token);

CREATE TABLE oauth_client (
    client_id text PRIMARY KEY,
    client_name text NOT NULL,
    grant_type text NOT NULL,
    options text NOT NULL
);

CREATE TABLE route (
    domain text NOT NULL,
    server_host text NOT NULL,
    node text NOT NULL,
    pid text NOT NULL,
    local_hint text NOT NULL
);

CREATE UNIQUE INDEX i_route ON route USING btree (domain, server_host, node, pid);
CREATE INDEX i_route_domain ON route USING btree (domain);

CREATE TABLE bosh (
    sid text NOT NULL,
    node text NOT NULL,
    pid text NOT NULL
);

CREATE UNIQUE INDEX i_bosh_sid ON bosh USING btree (sid);

CREATE TABLE proxy65 (
    sid text NOT NULL,
    pid_t text NOT NULL,
    pid_i text NOT NULL,
    node_t text NOT NULL,
    node_i text NOT NULL,
    jid_i text NOT NULL
);

CREATE UNIQUE INDEX i_proxy65_sid ON proxy65 USING btree (sid);
CREATE INDEX i_proxy65_jid ON proxy65 USING btree (jid_i);

CREATE TABLE push_session (
    username text NOT NULL,
    server_host text NOT NULL,
    timestamp bigint NOT NULL,
    service text NOT NULL,
    node text NOT NULL,
    xml text NOT NULL,
    PRIMARY KEY (server_host, username, timestamp)
);

CREATE UNIQUE INDEX i_push_session_susn ON push_session USING btree (server_host, username, service, node);

CREATE TABLE mix_channel (
    channel text NOT NULL,
    service text NOT NULL,
    username text NOT NULL,
    domain text NOT NULL,
    jid text NOT NULL,
    hidden boolean NOT NULL,
    hmac_key text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_mix_channel ON mix_channel (channel, service);
CREATE INDEX i_mix_channel_serv ON mix_channel (service);

CREATE TABLE mix_participant (
    channel text NOT NULL,
    service text NOT NULL,
    username text NOT NULL,
    domain text NOT NULL,
    jid text NOT NULL,
    id text NOT NULL,
    nick text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_mix_participant ON mix_participant (channel, service, username, domain);
CREATE INDEX i_mix_participant_chan_serv ON mix_participant (channel, service);

CREATE TABLE mix_subscription (
    channel text NOT NULL,
    service text NOT NULL,
    username text NOT NULL,
    domain text NOT NULL,
    node text NOT NULL,
    jid text NOT NULL
);

CREATE UNIQUE INDEX i_mix_subscription ON mix_subscription (channel, service, username, domain, node);
CREATE INDEX i_mix_subscription_chan_serv_ud ON mix_subscription (channel, service, username, domain);
CREATE INDEX i_mix_subscription_chan_serv_node ON mix_subscription (channel, service, node);
CREATE INDEX i_mix_subscription_chan_serv ON mix_subscription (channel, service);

CREATE TABLE mix_pam (
    username text NOT NULL,
    server_host text NOT NULL,
    channel text NOT NULL,
    service text NOT NULL,
    id text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_mix_pam ON mix_pam (username, server_host, channel, service);
CREATE INDEX i_mix_pam_us ON mix_pam (username, server_host);

CREATE TABLE mqtt_pub (
    username text NOT NULL,
    server_host text NOT NULL,
    resource text NOT NULL,
    topic text NOT NULL,
    qos smallint NOT NULL,
    payload bytea NOT NULL,
    payload_format smallint NOT NULL,
    content_type text NOT NULL,
    response_topic text NOT NULL,
    correlation_data bytea NOT NULL,
    user_properties bytea NOT NULL,
    expiry bigint NOT NULL
);

CREATE UNIQUE INDEX i_mqtt_topic_server ON mqtt_pub (topic, server_host);
