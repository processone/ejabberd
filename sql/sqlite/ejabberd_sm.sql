CREATE TABLE sm (
    usec bigint NOT NULL,
    pid text NOT NULL,
    node text NOT NULL,
    username text NOT NULL,
    server_host text NOT NULL,
    resource text NOT NULL,
    priority text NOT NULL,
    info text NOT NULL
);

CREATE UNIQUE INDEX i_usec_pid ON sm(usec, pid);
CREATE INDEX i_sm_node ON sm(node);
CREATE INDEX i_sm_sh_username ON sm (server_host, username);
