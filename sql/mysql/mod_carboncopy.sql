CREATE TABLE carboncopy (
    username text NOT NULL,
    server_host text NOT NULL,
    resource text NOT NULL,
    namespace text NOT NULL,
    node text NOT NULL,
    PRIMARY KEY (server_host(191), username(191), resource(191))
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_carboncopy_sh_user ON carboncopy (server_host(191), username(75));
