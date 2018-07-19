CREATE TABLE `users` (
    username varchar(191) NOT NULL,
    server_host text NOT NULL,
    password text NOT NULL,
    serverkey varchar(64) NOT NULL DEFAULT '',
    salt varchar(64) NOT NULL DEFAULT '',
    iterationcount integer NOT NULL DEFAULT 0,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (server_host(191), username)
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
