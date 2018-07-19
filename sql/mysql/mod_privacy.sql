CREATE TABLE privacy_default_list (
    username varchar(191) NOT NULL,
    server_host text NOT NULL,
    name varchar(191) NOT NULL,
    PRIMARY KEY (server_host(191), username)
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE TABLE privacy_list (
    username varchar(191) NOT NULL,
    server_host text NOT NULL,
    name varchar(191) NOT NULL,
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_privacy_list_sh_username  ON privacy_list(server_host(191), username);
CREATE UNIQUE INDEX i_privacy_list_sh_username_name ON privacy_list (server_host(191), username(75), name(75));

CREATE TABLE privacy_list_data (
    id bigint,
    t character(1) NOT NULL,
    value text NOT NULL,
    `action` character(1) NOT NULL,
    ord NUMERIC NOT NULL,
    match_all boolean NOT NULL,
    match_iq boolean NOT NULL,
    match_message boolean NOT NULL,
    match_presence_in boolean NOT NULL,
    match_presence_out boolean NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_privacy_list_data_id ON privacy_list_data(id);
