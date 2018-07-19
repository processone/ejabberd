CREATE TABLE oauth_token (
    token text NOT NULL PRIMARY KEY,
    jid text NOT NULL,
    scope text NOT NULL,
    "expire" bigint NOT NULL
);
