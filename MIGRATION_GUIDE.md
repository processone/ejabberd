# Migration Guide: Add `updated_at` to MUC Rooms

## Overview

This migration adds an `updated_at` column to the `muc_room` table to properly track when rooms are modified, separate from when they were created.

## Why This Change?

Previously, the `created_at` field was incorrectly:
1. Set to `1970-01-02 00:00:00` for new rooms
2. Updated every time the room was modified
3. Confused with `hibernation_time` (when room goes to sleep)

This migration fixes these issues by:
- Ensuring `created_at` is set correctly on room creation and never changes
- Adding `updated_at` to track when room configuration is modified
- Separating concerns between creation time, update time, and hibernation time

## Prerequisites

1. Backup your database before running migration
2. Stop ejabberd or ensure no rooms are being created/modified during migration
3. Compile the new ejabberd code

## Migration Steps

### Step 1: Backup Database

```bash
# MySQL
mysqldump -u root -p ejabberd > ejabberd_backup_$(date +%Y%m%d).sql

# PostgreSQL
pg_dump ejabberd > ejabberd_backup_$(date +%Y%m%d).sql

# SQLite
cp /path/to/ejabberd.db /path/to/ejabberd_backup_$(date +%Y%m%d).db
```

### Step 2: Run Migration SQL

#### For MySQL/MariaDB:

```sql
USE ejabberd;

-- Add updated_at column
ALTER TABLE muc_room ADD COLUMN updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Initialize updated_at with created_at value
UPDATE muc_room SET updated_at = created_at;

-- Verify
SELECT name, host, created_at, updated_at FROM muc_room LIMIT 10;
```

#### For PostgreSQL:

```sql
\c ejabberd;

-- Add updated_at column
ALTER TABLE muc_room ADD COLUMN updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Initialize updated_at with created_at value
UPDATE muc_room SET updated_at = created_at;

-- Verify
SELECT name, host, created_at, updated_at FROM muc_room LIMIT 10;
```

#### For SQLite:

```sql
-- Add updated_at column
ALTER TABLE muc_room ADD COLUMN updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Initialize updated_at with created_at value
UPDATE muc_room SET updated_at = created_at;

-- Verify
SELECT name, host, created_at, updated_at FROM muc_room LIMIT 10;
```

#### For MSSQL:

```sql
USE ejabberd;

-- Add updated_at column
ALTER TABLE muc_room ADD updated_at datetime NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Initialize updated_at with created_at value
UPDATE muc_room SET updated_at = created_at;

-- Verify
SELECT TOP 10 name, host, created_at, updated_at FROM muc_room;
```

### Step 3: Compile and Restart ejabberd

```bash
cd /path/to/ejabberd
./rebar3 compile
ejabberdctl restart
```

### Step 4: Verify

```bash
# Create a test room
ejabberdctl create_room testroom conference.localhost localhost

# Check database
# MySQL/PostgreSQL/SQLite:
SELECT name, created_at, updated_at FROM muc_room WHERE name='testroom';

# Both timestamps should be current time and equal

# Update room option
ejabberdctl change_room_option testroom conference.localhost title "Test Room"

# Check database again
SELECT name, created_at, updated_at FROM muc_room WHERE name='testroom';

# created_at should be unchanged, updated_at should be newer
```

## Rollback

If you need to rollback:

```sql
-- MySQL/PostgreSQL/SQLite
ALTER TABLE muc_room DROP COLUMN updated_at;

-- MSSQL
ALTER TABLE muc_room DROP COLUMN updated_at;
```

Then restore your backup and use the old ejabberd code.

## Troubleshooting

### Issue: Migration fails with "column already exists"

**Solution:** The column may have been added in a previous attempt. Check if it exists:

```sql
-- MySQL
SHOW COLUMNS FROM muc_room LIKE 'updated_at';

-- PostgreSQL
SELECT column_name FROM information_schema.columns 
WHERE table_name='muc_room' AND column_name='updated_at';

-- SQLite
PRAGMA table_info(muc_room);
```

If it exists, skip to Step 3.

### Issue: Old rooms still have `1970-01-02` in `created_at`

**Solution:** This is expected for rooms created before the migration. You can optionally update them:

```sql
-- Set created_at to updated_at for old rooms
UPDATE muc_room 
SET created_at = updated_at 
WHERE created_at < '1971-01-01 00:00:00';
```

### Issue: Performance concerns on large databases

**Solution:** For very large `muc_room` tables (>1M rows), consider:

1. Add the column without default first:
```sql
ALTER TABLE muc_room ADD COLUMN updated_at timestamp NULL;
```

2. Update in batches:
```sql
-- MySQL
UPDATE muc_room SET updated_at = created_at WHERE updated_at IS NULL LIMIT 10000;
-- Repeat until all rows updated

-- PostgreSQL
UPDATE muc_room SET updated_at = created_at WHERE updated_at IS NULL;
```

3. Make it NOT NULL after all rows are updated:
```sql
ALTER TABLE muc_room MODIFY updated_at timestamp NOT NULL;
```

## Benefits After Migration

1. ✅ Accurate room creation timestamps
2. ✅ Track when room configuration changes
3. ✅ Better analytics and reporting
4. ✅ Clearer separation of concerns
5. ✅ No more `1970-01-02 00:00:00` timestamps!

## Support

If you encounter issues, please report them on the ejabberd GitHub repository with:
- Database type and version
- ejabberd version
- Error messages
- Output of `SELECT * FROM muc_room LIMIT 1;`
