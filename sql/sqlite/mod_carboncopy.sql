CREATE TABLE carboncopy (
    username text NOT NULL,
    server_host text NOT NULL,
    resource text NOT NULL,
    namespace text NOT NULL,
    node text NOT NULL
);

CREATE UNIQUE INDEX i_server_host_username_resource ON carboncopy (server_host, username, resource);
CREATE INDEX i_carboncopy_sh_user ON carboncopy (server_host, username);
