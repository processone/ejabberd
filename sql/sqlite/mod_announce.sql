CREATE TABLE motd (
    username text NOT NULL,
    server_host text NOT NULL,
    xml text,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_motd_sh_username ON motd (server_host, username);
