

CREATE TABLE users (
    username text NOT NULL,
    "password" text NOT NULL
);


CREATE TABLE last (
    username text NOT NULL,
    seconds text NOT NULL,
    state text
);


CREATE TABLE rosterusers (
    username text NOT NULL,
    jid text NOT NULL,
    nick text,
    subscription character(1) NOT NULL,
    ask character(1) NOT NULL,
    server character(1) NOT NULL,
    subscribe text,
    "type" text
);



CREATE TABLE rostergroups (
    username text NOT NULL,
    jid text NOT NULL,
    grp text NOT NULL
);


CREATE TABLE spool (
    username text NOT NULL,
    xml text
);



CREATE TABLE vcard (
    username text NOT NULL,
    full_name text,
    first_name text,
    last_name text,
    nick_name text,
    url text,
    address1 text,
    address2 text,
    locality text,
    region text,
    pcode text,
    country text,
    telephone text,
    email text,
    orgname text,
    orgunit text,
    title text,
    role text,
    b_day date,
    descr text
);




CREATE INDEX i_users_login ON users USING btree (username, "password");

CREATE INDEX i_rosteru_user_jid ON rosterusers USING btree (username, jid);

CREATE INDEX i_rosteru_username ON rosterusers USING btree (username);

CREATE INDEX pk_rosterg_user_jid ON rostergroups USING btree (username, jid);

CREATE INDEX i_despool ON spool USING btree (username);

CREATE INDEX i_rosteru_jid ON rosterusers USING btree (jid);

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (username);

ALTER TABLE ONLY last
    ADD CONSTRAINT last_pkey PRIMARY KEY (username);

ALTER TABLE ONLY vcard
    ADD CONSTRAINT vcard_pkey PRIMARY KEY (username);

