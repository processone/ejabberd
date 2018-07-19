CREATE TABLE last (
    username varchar(191) NOT NULL,
    server_host text NOT NULL,
    seconds text NOT NULL,
    state text NOT NULL,
    PRIMARY KEY (server_host(191), username)
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
