CREATE TABLE archive (
    username text NOT NULL,
    server_host text NOT NULL,
    timestamp BIGINT UNSIGNED NOT NULL,
    peer text NOT NULL,
    bare_peer text NOT NULL,
    xml text NOT NULL,
    txt text,
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    kind text,
    nick text,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX i_archive_sh_username_timestamp ON archive (server_host, username, timestamp);
CREATE INDEX i_archive_sh_username_peer ON archive (server_host, username, peer);
CREATE INDEX i_archive_sh_username_bare_peer ON archive (server_host, username, bare_peer);
CREATE INDEX i_archive_sh_timestamp ON archive (server_host, timestamp);

CREATE TABLE archive_prefs (
    username text NOT NULL,
    server_host text NOT NULL,
    def text NOT NULL,
    always text NOT NULL,
    never text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_archive_prefs_sh_username ON archive_prefs (server_host, username);
