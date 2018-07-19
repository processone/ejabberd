CREATE TABLE spool (
    username varchar(191) NOT NULL,
    server_host text NOT NULL,
    xml BLOB NOT NULL,
    seq BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_spool_sh_username ON spool(server_host(191), username);
CREATE INDEX i_spool_created_at ON spool(created_at);
