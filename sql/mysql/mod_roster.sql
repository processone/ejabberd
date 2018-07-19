CREATE TABLE rosterusers (
    username varchar(191) NOT NULL,
    server_host text NOT NULL,
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

CREATE UNIQUE INDEX i_rosteru_sh_user_jid ON rosterusers(server_host(191), username(75), jid(75));
CREATE INDEX i_rosteru_sh_username ON rosterusers(server_host(191), username);
CREATE INDEX i_rosteru_sh_jid ON rosterusers(server_host(191), jid);

CREATE TABLE rostergroups (
    username varchar(191) NOT NULL,
    server_host text NOT NULL,
    jid varchar(191) NOT NULL,
    grp text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_rosterg_sh_user_jid ON rostergroups(server_host(191), username(75), jid(75));

CREATE TABLE roster_version (
    username varchar(191) NOT NULL,
    server_host text NOT NULL,
    version text NOT NULL,
    PRIMARY KEY (server_host(191), username)
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
