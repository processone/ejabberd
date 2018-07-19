CREATE TABLE caps_features (
    node varchar(191) NOT NULL,
    subnode varchar(191) NOT NULL,
    feature text,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_caps_features_node_subnode ON caps_features(node(75), subnode(75));
