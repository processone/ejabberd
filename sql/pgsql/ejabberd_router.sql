CREATE TABLE route (
    domain text NOT NULL,
    server_host text NOT NULL,
    node text NOT NULL,
    pid text NOT NULL,
    local_hint text NOT NULL
);

CREATE UNIQUE INDEX i_route ON route (domain, server_host, node, pid);
CREATE INDEX i_route_domain ON route (domain);
