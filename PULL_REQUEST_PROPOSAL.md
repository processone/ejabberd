# Fix MUC Room `created_at` Timestamp Issue

## 🐛 Problem

Currently, the `created_at` field in the `muc_room` table is incorrectly set to `1970-01-02 00:00:00` for newly created rooms. This happens because:

1. The `store_room/5` function in `mod_muc_sql.erl` uses `hibernation_time` from room options to set `created_at`
2. When a room is first created, `hibernation_time` is not set (or is `undefined`)
3. The fallback value is hardcoded to `1970-01-02 00:00:00`

### Current Code (mod_muc_sql.erl:146-150)
```erlang
Timestamp = case lists:keyfind(hibernation_time, 1, Opts) of
    false -> <<"1970-01-02 00:00:00">>;
    {_, undefined} -> <<"1970-01-02 00:00:00">>;
    {_, Time} -> usec_to_sql_timestamp(Time)
end,
```

### Issues with Current Implementation:
- **Semantic confusion**: `hibernation_time` represents when a room goes to sleep (no users), not when it was created
- **Incorrect timestamps**: All new rooms get `1970-01-02 00:00:00` as creation time
- **UPSERT behavior**: On updates, `created_at` is overwritten with hibernation time, which is semantically wrong

## 💡 Proposed Solutions

### Solution 1: Separate `created_at` and `updated_at` (Recommended)

Add a new `updated_at` column and fix the semantics:

**Database Schema Changes:**
```erlang
#sql_table{
    name = <<"muc_room">>,
    columns =
        [#sql_column{name = <<"name">>, type = text},
         #sql_column{name = <<"host">>, type = text},
         #sql_column{name = <<"server_host">>, type = text},
         #sql_column{name = <<"opts">>, type = {text, big}},
         #sql_column{name = <<"created_at">>, type = timestamp,
                     default = true},
         #sql_column{name = <<"updated_at">>, type = timestamp,
                     default = true}],
    ...
}
```

**Code Changes (mod_muc_sql.erl):**
```erlang
store_room(LServer, Host, Name, Opts, ChangesHints) ->
    {Subs, Opts2} = case lists:keytake(subscribers, 1, Opts) of
        {value, {subscribers, S}, OptN} -> {S, OptN};
        _ -> {[], Opts}
    end,
    SOpts = misc:term_to_expr(Opts2),
    CurrentTime = usec_to_sql_timestamp(erlang:system_time(microsecond)),
    
    F = fun () ->
        case ejabberd_sql:sql_query_t(
            ?SQL("select @(created_at)t from muc_room where "
                 "name=%(Name)s and host=%(Host)s")) of
            {selected, [{CreatedAt}]} ->
                % Room exists, update only updated_at
                ?SQL_UPSERT_T(
                    "muc_room",
                    ["!name=%(Name)s",
                     "!host=%(Host)s",
                     "server_host=%(LServer)s",
                     "opts=%(SOpts)s",
                     "created_at=%(CreatedAt)t",
                     "updated_at=%(CurrentTime)t"]);
            _ ->
                % New room, set both created_at and updated_at
                ?SQL_UPSERT_T(
                    "muc_room",
                    ["!name=%(Name)s",
                     "!host=%(Host)s",
                     "server_host=%(LServer)s",
                     "opts=%(SOpts)s",
                     "created_at=%(CurrentTime)t",
                     "updated_at=%(CurrentTime)t"])
        end,
        % Handle subscribers...
    end,
    ejabberd_sql:sql_transaction(LServer, F).
```

**Migration SQL:**
```sql
-- MySQL
ALTER TABLE muc_room ADD COLUMN updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP;
UPDATE muc_room SET updated_at = created_at WHERE updated_at IS NULL;

-- PostgreSQL
ALTER TABLE muc_room ADD COLUMN updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP;
UPDATE muc_room SET updated_at = created_at WHERE updated_at IS NULL;

-- SQLite
ALTER TABLE muc_room ADD COLUMN updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP;
UPDATE muc_room SET updated_at = created_at WHERE updated_at IS NULL;
```

### Solution 2: Fix `created_at` to Use Current Time (Simple Fix)

If we don't want to add `updated_at`, at least fix `created_at` to use the current time:

```erlang
Timestamp = case lists:keyfind(hibernation_time, 1, Opts) of
    false -> usec_to_sql_timestamp(erlang:system_time(microsecond));
    {_, undefined} -> usec_to_sql_timestamp(erlang:system_time(microsecond));
    {_, Time} -> usec_to_sql_timestamp(Time)
end,
```

**Pros:**
- Minimal code change
- No database migration needed
- Fixes the immediate issue

**Cons:**
- Still uses `hibernation_time` for `created_at` (semantic confusion)
- `created_at` gets updated on every room update

### Solution 3: Add `hibernation_time` Support in API (Workaround)

Allow users to pass `hibernation_time` via API:

**Code Changes (mod_muc_admin.erl:1734):**
```erlang
format_room_option(OptionString, ValueString) ->
    Option = misc:binary_to_atom(OptionString),
    Value = case Option of
        ...
        hibernation_time -> try_convert_integer(Option, ValueString);
        ...
    end,
    {Option, Value}.
```

**API Usage:**
```bash
curl -X POST /api/create_room_with_opts \
  -H "Content-Type: application/json" \
  -d '{
    "room": "myroom",
    "service": "conference.example.com",
    "host": "example.com",
    "options": [
      {
        "name": "hibernation_time",
        "value": "1704672000000000"
      }
    ]
  }'
```

**Pros:**
- Gives API users control over `created_at`
- Minimal code change

**Cons:**
- Still semantic confusion between `hibernation_time` and `created_at`
- Requires users to manually calculate microsecond timestamps

## 🎯 Recommendation

**Solution 1** is the best approach because:
1. ✅ Separates concerns: `created_at` vs `updated_at` vs `hibernation_time`
2. ✅ Follows database best practices
3. ✅ Provides more information (when created AND when last updated)
4. ✅ Fixes the semantic confusion
5. ✅ Backward compatible with proper migration

## 📊 Impact Analysis

### Files to Modify:
- `src/mod_muc_sql.erl` - Update schema and store_room function
- `sql/mysql.new.sql` - Add updated_at column
- `sql/pg.new.sql` - Add updated_at column
- `sql/lite.new.sql` - Add updated_at column
- `sql/mssql.new.sql` - Add updated_at column

### Backward Compatibility:
- Migration script provided
- Existing rooms will have `updated_at = created_at` initially
- No breaking changes to API

## 🧪 Testing

```bash
# Test 1: Create new room
ejabberdctl create_room testroom1 conference.localhost localhost

# Verify created_at and updated_at are set to current time
SELECT name, created_at, updated_at FROM muc_room WHERE name='testroom1';

# Test 2: Update room options
ejabberdctl change_room_option testroom1 conference.localhost title "New Title"

# Verify created_at unchanged, updated_at updated
SELECT name, created_at, updated_at FROM muc_room WHERE name='testroom1';

# Test 3: Room hibernation
# Let room hibernate (all users leave)
# Verify created_at and updated_at remain unchanged
```

## 📝 Additional Notes

The current implementation conflates three different concepts:
1. **`created_at`**: When the room was first created (should never change)
2. **`updated_at`**: When the room configuration was last modified
3. **`hibernation_time`**: When the room went to sleep (runtime state, not persistent metadata)

This PR proposes to properly separate these concerns.

## 🔗 Related Issues

- Rooms created via API have incorrect `created_at` timestamp
- `get_hibernated_rooms_older_than` query filters out rooms with `1970-01-02` timestamp
- Confusion between room creation time and hibernation time

---

**Author**: Community Contribution  
**Type**: Bug Fix + Enhancement  
**Priority**: Medium  
**Backward Compatible**: Yes (with migration)
