CREATE TABLE vcard (
    username text NOT NULL,
    server_host text NOT NULL,
    vcard text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_vcard_sh_user ON vcard (server_host, username);

CREATE TABLE vcard_search (
    username text NOT NULL,
    lusername text NOT NULL,
    server_host text NOT NULL,
    fn text NOT NULL,
    lfn text NOT NULL,
    family text NOT NULL,
    lfamily text NOT NULL,
    given text NOT NULL,
    lgiven text NOT NULL,
    middle text NOT NULL,
    lmiddle text NOT NULL,
    nickname text NOT NULL,
    lnickname text NOT NULL,
    bday text NOT NULL,
    lbday text NOT NULL,
    ctry text NOT NULL,
    lctry text NOT NULL,
    locality text NOT NULL,
    llocality text NOT NULL,
    email text NOT NULL,
    lemail text NOT NULL,
    orgname text NOT NULL,
    lorgname text NOT NULL,
    orgunit text NOT NULL,
    lorgunit text NOT NULL
);

CREATE UNIQUE INDEX i_server_host_lusername ON vcard_search (server_host, lusername);
CREATE INDEX i_vcard_search_sh_lfn       ON vcard_search(server_host, lfn);
CREATE INDEX i_vcard_search_sh_lfamily   ON vcard_search(server_host, lfamily);
CREATE INDEX i_vcard_search_sh_lgiven    ON vcard_search(server_host, lgiven);
CREATE INDEX i_vcard_search_sh_lmiddle   ON vcard_search(server_host, lmiddle);
CREATE INDEX i_vcard_search_sh_lnickname ON vcard_search(server_host, lnickname);
CREATE INDEX i_vcard_search_sh_lbday     ON vcard_search(server_host, lbday);
CREATE INDEX i_vcard_search_sh_lctry     ON vcard_search(server_host, lctry);
CREATE INDEX i_vcard_search_sh_llocality ON vcard_search(server_host, llocality);
CREATE INDEX i_vcard_search_sh_lemail    ON vcard_search(server_host, lemail);
CREATE INDEX i_vcard_search_sh_lorgname  ON vcard_search(server_host, lorgname);
CREATE INDEX i_vcard_search_sh_lorgunit  ON vcard_search(server_host, lorgunit);
