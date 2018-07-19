CREATE TABLE last (
    username text NOT NULL,
    server_host text NOT NULL,
    seconds text NOT NULL,
    state text NOT NULL,
    PRIMARY KEY (server_host, username)
);
