-- Migration script to add updated_at column to muc_room table
-- This script should be run after upgrading ejabberd

-- For MySQL/MariaDB
-- ALTER TABLE muc_room ADD COLUMN updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP;
-- UPDATE muc_room SET updated_at = created_at;

-- For PostgreSQL
-- ALTER TABLE muc_room ADD COLUMN updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP;
-- UPDATE muc_room SET updated_at = created_at;

-- For SQLite
-- ALTER TABLE muc_room ADD COLUMN updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP;
-- UPDATE muc_room SET updated_at = created_at;

-- For MSSQL
-- ALTER TABLE muc_room ADD updated_at datetime NOT NULL DEFAULT CURRENT_TIMESTAMP;
-- UPDATE muc_room SET updated_at = created_at;

-- Note: Uncomment the appropriate section for your database

