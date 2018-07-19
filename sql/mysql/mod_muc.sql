CREATE TABLE muc_room (
    name text NOT NULL,
    host text NOT NULL,
    server_host text NOT NULL,
    opts mediumtext NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_muc_room_name_host ON muc_room(name(75), host(75));

CREATE TABLE muc_registered (
    jid text NOT NULL,
    host text NOT NULL,
    server_host text NOT NULL,
    nick text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_muc_registered_nick ON muc_registered(nick(75));
CREATE UNIQUE INDEX i_muc_registered_jid_host ON muc_registered(jid(75), host(75));

CREATE TABLE muc_online_room (
    name text NOT NULL,
    host text NOT NULL,
    server_host text NOT NULL,
    node text NOT NULL,
    pid text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_muc_online_room_name_host ON muc_online_room(name(75), host(75));

CREATE TABLE muc_online_users (
    username text NOT NULL,
    server text NOT NULL,
    resource text NOT NULL,
    name text NOT NULL,
    host text NOT NULL,
    server_host text NOT NULL,
    node text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_muc_online_users ON muc_online_users(username(75), server(75), resource(75), name(75), host(75));
CREATE INDEX i_muc_online_users_us ON muc_online_users(username(75), server(75));

CREATE TABLE muc_room_subscribers (
   room varchar(191) NOT NULL,
   host varchar(191) NOT NULL,
   jid varchar(191) NOT NULL,
   nick text NOT NULL,
   nodes text NOT NULL,
   created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_muc_room_subscribers_host_room_jid ON muc_room_subscribers(host, room, jid);
CREATE INDEX i_muc_room_subscribers_host_jid ON muc_room_subscribers(host, jid);
