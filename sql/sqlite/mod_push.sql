CREATE TABLE push_session (
    username text NOT NULL,
    server_host text NOT NULL,
    timestamp bigint NOT NULL,
    service text NOT NULL,
    node text NOT NULL,
    xml text NOT NULL
);

CREATE UNIQUE INDEX i_server_host_username_timestamp ON push_session (server_host, username, timestamp);
CREATE UNIQUE INDEX i_push_session_susn ON push_session (server_host, username, service, node);
