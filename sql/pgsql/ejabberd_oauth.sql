CREATE TABLE oauth_token (
    token text NOT NULL,
    jid text NOT NULL,
    scope text NOT NULL,
    "expire" bigint NOT NULL
);

CREATE UNIQUE INDEX i_oauth_token_token ON oauth_token (token);
