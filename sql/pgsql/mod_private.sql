CREATE TABLE private_storage (
    username text NOT NULL,
    server_host text NOT NULL,
    namespace text NOT NULL,
    data text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (server_host, username, namespace)
);

CREATE INDEX i_private_storage_sh_username ON private_storage (server_host, username);
