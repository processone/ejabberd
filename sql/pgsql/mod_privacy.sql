CREATE TABLE privacy_default_list (
    username text NOT NULL,
    server_host text NOT NULL,
    name text NOT NULL,
    PRIMARY KEY (server_host, username)
);

CREATE TABLE privacy_list (
    username text NOT NULL,
    server_host text NOT NULL,
    name text NOT NULL,
    id SERIAL UNIQUE,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX i_privacy_list_sh_username ON privacy_list (server_host, username);
CREATE UNIQUE INDEX i_privacy_list_sh_username_name ON privacy_list (server_host, username, name);

CREATE TABLE privacy_list_data (
    id bigint REFERENCES privacy_list(id) ON DELETE CASCADE,
    t character(1) NOT NULL,
    value text NOT NULL,
    "action" character(1) NOT NULL,
    ord NUMERIC NOT NULL,
    match_all boolean NOT NULL,
    match_iq boolean NOT NULL,
    match_message boolean NOT NULL,
    match_presence_in boolean NOT NULL,
    match_presence_out boolean NOT NULL
);

CREATE INDEX i_privacy_list_data_id ON privacy_list_data (id);
