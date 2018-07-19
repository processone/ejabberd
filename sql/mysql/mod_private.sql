CREATE TABLE private_storage (
    username varchar(191) NOT NULL,
    server_host text NOT NULL,
    namespace varchar(191) NOT NULL,
    data text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (server_host(191), username, namespace)
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_private_storage_sh_username ON private_storage(server_host(191), username);
