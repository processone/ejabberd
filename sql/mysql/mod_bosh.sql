CREATE TABLE bosh (
    sid text NOT NULL,
    node text NOT NULL,
    pid text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_bosh_sid ON bosh(sid(75));
