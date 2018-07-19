CREATE TABLE archive (
    username varchar(191) NOT NULL,
    server_host text NOT NULL,
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

CREATE INDEX i_archive_sh_username_timestamp ON archive(server_host(191), username(191), timestamp);
CREATE INDEX i_archive_sh_username_peer ON archive(server_host(191), username(191), peer(191));
CREATE INDEX i_archive_sh_username_bare_peer ON archive(server_host(191), username(191), bare_peer(191));
CREATE INDEX i_archive_sh_timestamp ON archive(server_host(191), timestamp);

CREATE TABLE archive_prefs (
    username varchar(191) NOT NULL,
    server_host text NOT NULL,
    def text NOT NULL,
    always text NOT NULL,
    never text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (server_host(191), username)
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
