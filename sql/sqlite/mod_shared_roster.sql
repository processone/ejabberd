CREATE TABLE sr_group (
    name text NOT NULL,
    server_host text NOT NULL,
    opts text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_server_host_name ON sr_group (server_host, name);

CREATE TABLE sr_user (
    jid text NOT NULL,
    server_host text NOT NULL,
    grp text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_server_host_jid_grp ON sr_user (server_host, jid, grp);
CREATE INDEX i_sr_user_sh_jid ON sr_user (server_host, jid);
CREATE INDEX i_sr_user_sh_grp ON sr_user (server_host, grp);
