CREATE TABLE muc_room (
    name text NOT NULL,
    host text NOT NULL,
    server_host text NOT NULL,
    opts text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_muc_room_name_host ON muc_room (name, host);

CREATE TABLE muc_registered (
    jid text NOT NULL,
    host text NOT NULL,
    server_host text NOT NULL,
    nick text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX i_muc_registered_nick ON muc_registered (nick);
CREATE UNIQUE INDEX i_muc_registered_jid_host ON muc_registered (jid, host);

CREATE TABLE muc_online_room (
    name text NOT NULL,
    host text NOT NULL,
    server_host text NOT NULL,
    node text NOT NULL,
    pid text NOT NULL
);

CREATE UNIQUE INDEX i_muc_online_room_name_host ON muc_online_room (name, host);

CREATE TABLE muc_online_users (
    username text NOT NULL,
    server text NOT NULL,
    resource text NOT NULL,
    name text NOT NULL,
    host text NOT NULL,
    server_host text NOT NULL,
    node text NOT NULL
);

CREATE UNIQUE INDEX i_muc_online_users ON muc_online_users (username, server, resource, name, host);
CREATE INDEX i_muc_online_users_us ON muc_online_users (username, server);

CREATE TABLE muc_room_subscribers (
   room text NOT NULL,
   host text NOT NULL,
   jid text NOT NULL,
   nick text NOT NULL,
   nodes text NOT NULL,
   created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX i_muc_room_subscribers_host_jid ON muc_room_subscribers (host, jid);
CREATE UNIQUE INDEX i_muc_room_subscribers_host_room_jid ON muc_room_subscribers (host, room, jid);
