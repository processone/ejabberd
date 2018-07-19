CREATE TABLE motd (
    username text NOT NULL,
    server_host text NOT NULL,
    xml text,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (server_host, username)
);
