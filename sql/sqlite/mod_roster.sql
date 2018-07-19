CREATE TABLE rosterusers (
    username text NOT NULL,
    server_host text NOT NULL,
    jid text NOT NULL,
    nick text NOT NULL,
    subscription character(1) NOT NULL,
    ask character(1) NOT NULL,
    askmessage text NOT NULL,
    server character(1) NOT NULL,
    subscribe text NOT NULL,
    type text,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_rosteru_sh_user_jid ON rosterusers (server_host, username, jid);
CREATE INDEX i_rosteru_sh_username ON rosterusers (server_host, username);
CREATE INDEX i_rosteru_sh_jid ON rosterusers (server_host, jid);


CREATE TABLE rostergroups (
    username text NOT NULL,
    server_host text NOT NULL,
    jid text NOT NULL,
    grp text NOT NULL
);

CREATE INDEX i_rosterg_sh_user_jid ON rostergroups (server_host, username, jid);

CREATE TABLE roster_version (
    username text NOT NULL,
    server_host text NOT NULL,
    version text NOT NULL
);

CREATE UNIQUE INDEX i_roster_version_sh_user ON roster_version (server_host, username);
