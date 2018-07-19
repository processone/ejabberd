CREATE TABLE vcard (
    username varchar(191) NOT NULL,
    server_host text NOT NULL,
    vcard mediumtext NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (server_host(191), username)
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE TABLE vcard_search (
    username varchar(191) NOT NULL,
    lusername varchar(191) NOT NULL,
    server_host text NOT NULL,
    fn text NOT NULL,
    lfn varchar(191) NOT NULL,
    family text NOT NULL,
    lfamily varchar(191) NOT NULL,
    given text NOT NULL,
    lgiven varchar(191) NOT NULL,
    middle text NOT NULL,
    lmiddle varchar(191) NOT NULL,
    nickname text NOT NULL,
    lnickname varchar(191) NOT NULL,
    bday text NOT NULL,
    lbday varchar(191) NOT NULL,
    ctry text NOT NULL,
    lctry varchar(191) NOT NULL,
    locality text NOT NULL,
    llocality varchar(191) NOT NULL,
    email text NOT NULL,
    lemail varchar(191) NOT NULL,
    orgname text NOT NULL,
    lorgname varchar(191) NOT NULL,
    orgunit text NOT NULL,
    lorgunit varchar(191) NOT NULL,
    PRIMARY KEY (server_host(191), lusername)
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_vcard_search_sh_lfn       ON vcard_search(server_host(191), lfn);
CREATE INDEX i_vcard_search_sh_lfamily   ON vcard_search(server_host(191), lfamily);
CREATE INDEX i_vcard_search_sh_lgiven    ON vcard_search(server_host(191), lgiven);
CREATE INDEX i_vcard_search_sh_lmiddle   ON vcard_search(server_host(191), lmiddle);
CREATE INDEX i_vcard_search_sh_lnickname ON vcard_search(server_host(191), lnickname);
CREATE INDEX i_vcard_search_sh_lbday     ON vcard_search(server_host(191), lbday);
CREATE INDEX i_vcard_search_sh_lctry     ON vcard_search(server_host(191), lctry);
CREATE INDEX i_vcard_search_sh_llocality ON vcard_search(server_host(191), llocality);
CREATE INDEX i_vcard_search_sh_lemail    ON vcard_search(server_host(191), lemail);
CREATE INDEX i_vcard_search_sh_lorgname  ON vcard_search(server_host(191), lorgname);
CREATE INDEX i_vcard_search_sh_lorgunit  ON vcard_search(server_host(191), lorgunit);
