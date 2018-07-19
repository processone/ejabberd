CREATE TABLE spool (
    username text NOT NULL,
    server_host text NOT NULL,
    xml text NOT NULL,
    seq INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX i_spool_sh_username ON spool (server_host, username);
