-- Needs MySQL max with innodb back-end

CREATE TABLE users (
    username varchar(250) PRIMARY KEY,
    password text NOT NULL
) TYPE=InnoDB CHARACTER SET utf8;


CREATE TABLE last (
    username varchar(250) PRIMARY KEY,
    seconds text NOT NULL,
    state text
) TYPE=InnoDB CHARACTER SET utf8;


CREATE TABLE rosterusers (
    username varchar(250) NOT NULL,
    jid varchar(250) NOT NULL,
    nick text,
    subscription character(1) NOT NULL,
    ask character(1) NOT NULL,
    server character(1) NOT NULL,
    subscribe text,
    type text
) TYPE=InnoDB CHARACTER SET utf8;

CREATE UNIQUE INDEX i_rosteru_user_jid USING HASH ON rosterusers(username(75), jid(75));
CREATE INDEX i_rosteru_username USING HASH ON rosterusers(username);
CREATE INDEX i_rosteru_jid USING HASH ON rosterusers(jid);

CREATE TABLE rostergroups (
    username varchar(250) NOT NULL,
    jid varchar(250) NOT NULL,
    grp text NOT NULL
) TYPE=InnoDB CHARACTER SET utf8;

CREATE INDEX pk_rosterg_user_jid USING HASH ON rostergroups(username(75), jid(75));


CREATE TABLE spool (
    username varchar(250) NOT NULL,
    xml text,
    seq SERIAL
) TYPE=InnoDB CHARACTER SET utf8;

CREATE INDEX i_despool USING BTREE ON spool(username);


CREATE TABLE vcard (
    username varchar(250) PRIMARY KEY,
    vcard text NOT NULL
) TYPE=InnoDB CHARACTER SET utf8;


CREATE TABLE vcard_search (
    username varchar(250) NOT NULL,
    lusername varchar(250) PRIMARY KEY,
    fn text NOT NULL,
    lfn varchar(250) NOT NULL,
    family text NOT NULL,
    lfamily varchar(250) NOT NULL,
    given text NOT NULL,
    lgiven varchar(250) NOT NULL,
    middle text NOT NULL,
    lmiddle varchar(250) NOT NULL,
    nickname text NOT NULL,
    lnickname varchar(250) NOT NULL,
    bday text NOT NULL,
    lbday varchar(250) NOT NULL,
    ctry text NOT NULL,
    lctry varchar(250) NOT NULL,
    locality text NOT NULL,
    llocality varchar(250) NOT NULL,
    email text NOT NULL,
    lemail varchar(250) NOT NULL,
    orgname text NOT NULL,
    lorgname varchar(250) NOT NULL,
    orgunit text NOT NULL,
    lorgunit varchar(250) NOT NULL
) TYPE=InnoDB CHARACTER SET utf8;

CREATE INDEX i_vcard_search_lfn       ON vcard_search(lfn);
CREATE INDEX i_vcard_search_lfamily   ON vcard_search(lfamily);
CREATE INDEX i_vcard_search_lgiven    ON vcard_search(lgiven);
CREATE INDEX i_vcard_search_lmiddle   ON vcard_search(lmiddle);
CREATE INDEX i_vcard_search_lnickname ON vcard_search(lnickname);
CREATE INDEX i_vcard_search_lbday     ON vcard_search(lbday);
CREATE INDEX i_vcard_search_lctry     ON vcard_search(lctry);
CREATE INDEX i_vcard_search_llocality ON vcard_search(llocality);
CREATE INDEX i_vcard_search_lemail    ON vcard_search(lemail);
CREATE INDEX i_vcard_search_lorgname  ON vcard_search(lorgname);
CREATE INDEX i_vcard_search_lorgunit  ON vcard_search(lorgunit);

