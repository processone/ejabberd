CREATE TABLE oauth_token (
    token varchar(191) NOT NULL PRIMARY KEY,
    jid text NOT NULL,
    scope text NOT NULL,
    `expire` bigint NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
