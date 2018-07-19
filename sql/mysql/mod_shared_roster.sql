CREATE TABLE sr_group (
    name varchar(191) NOT NULL,
    server_host text NOT NULL,
    opts text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (server_host(191), name)
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE TABLE sr_user (
    jid varchar(191) NOT NULL,
    server_host text NOT NULL,
    grp varchar(191) NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (server_host(191), jid, grp)
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_sr_user_sh_jid ON sr_user(server_host(191), jid);
CREATE INDEX i_sr_user_sh_grp ON sr_user(server_host(191), grp);
