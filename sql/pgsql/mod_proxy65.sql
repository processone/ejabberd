CREATE TABLE proxy65 (
    sid text NOT NULL,
    pid_t text NOT NULL,
    pid_i text NOT NULL,
    node_t text NOT NULL,
    node_i text NOT NULL,
    jid_i text NOT NULL
);

CREATE UNIQUE INDEX i_proxy65_sid ON proxy65 (sid);
CREATE INDEX i_proxy65_jid ON proxy65 (jid_i);
