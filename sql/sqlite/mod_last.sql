CREATE TABLE last (
    username text NOT NULL,
    server_host text NOT NULL,
    seconds text NOT NULL,
    state text NOT NULL
);

CREATE UNIQUE INDEX i_last_sh_username ON last (server_host, username);
