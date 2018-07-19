CREATE TABLE sm (
    usec bigint NOT NULL,
    pid text NOT NULL,
    node text NOT NULL,
    username varchar(191) NOT NULL,
    server_host text NOT NULL,
    resource varchar(191) NOT NULL,
    priority text NOT NULL,
    info text NOT NULL,
    PRIMARY KEY (usec, pid(75))
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_sm_node ON sm(node(75));
CREATE INDEX i_sm_sh_username ON sm(server_host(191), username);
