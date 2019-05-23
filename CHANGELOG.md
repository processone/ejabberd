# Version NEXT

# Version 19.05

* Admin
- The minimum required Erlang/OTP version is now 19.1
- Provide a suggestion when unknown command, module, option or request handler is detected
- Deprecate some listening options: captcha, register, web_admin, http_bind and xmlrpc
- Add commands to get Mnesia info: mnesia_info and mnesia_table_info
- Fix Register command to respect mod_register's Access option
- Fixes in Prosody import: privacy and rooms
- Remove TLS options from the example config
- Improve request_handlers validator
- Fix syntax in example Elixir config file

* Auth
- Correctly support cache tags in ejabberd_auth
- Don't process failed EXTERNAL authentication by mod_fail2ban
- Don't call to mod_register when it's not loaded
- Make anonymous auth don't {de}register user when there are other resources

* Developer
- Rename listening callback from start/2 to start/3
- New hook called when room gets destroyed: room_destroyed
- New hooks for tracking mucsub subscriptions changes: muc_subscribed, muc_unsubscribed
- Make static hooks analyzer working again

* MUC
- Service admins are allowed to recreate room even if archiv is nonempty
- New option user_mucsub_from_muc_archive
- Avoid late arrival of get_disco_item response
- Handle get_subscribed_rooms call from mod_muc_room pid
- Fix room state cleanup from db on change of persistent option change
- Make get_subscribed_rooms work even for non-persistant rooms
- Allow non-moderator subscribers to get list of room subscribers

* Offline
- New option bounce_groupchat: make it not bounce mucsub/groupchat messages
- New option use_mam_for_storage: fetch data from mam instead of spool table
- When applying limit of max msgs in spool check only spool size
- Do not store mucsub wrapped messages with no-store hint in offline storage
- Always store ActivityMarker messages
- Don't issue count/message fetch queries for offline from mam when not needed
- Properly handle infinity as max number of message in mam offline storage
- Sort messages by stanza_id when using mam storage in mod_offline
- Return correct value from count_offline_messages with mam storage option
- Make mod_offline put msg ignored by mam in spool when mam storage is on

* SQL:
- Add SQL schemas for MQTT tables
- Report better errors on SQL terms decode failure
- Fix PostgreSQL compatibility in mod_offline_sql:remove_old_messages
- Fix handling of list arguments on pgsql
- Preliminary support for SQL in process_rosteritems command

* Tests
- Add tests for user mucsub mam from muc mam
- Add tests for offline with mam storage
- Add tests for offline use_mam_for_storage
- Initial Docker environment to run ejabberd test suite
- Test offline:use_mam_for_storage, mam:user_mucsub_from_muc_archive used together

* Websocket
- Add WebSockets support to mod_mqtt
- Return "Bad request" error when origin in websocket connection doesn't match
- Fix RFC6454 violation on websocket connection when validating Origin header
- Origin header validation on websocket connection

* Other modules
- mod_adhoc: Use xml:lang from stanza when it's missing in <command/> element
- mod_announce: Add 'sessionid' attribute when required
- mod_bosh: Don't put duplicate polling attribute in bosh payload
- mod_http_api: Improve argument error messages and log messages
- mod_http_upload: Feed whole image to eimp:identify/1
- mod_http_upload: Log nicer warning on unknown host
- mod_http_upload: Case-insensitive host comparison
- mod_mqtt: Support other socket modules
- mod_push: Check for payload in encrypted messages

# Version 19.02

* Admin
- Fix in configure.ac the Erlang/OTP version: from 17.5 to 19.0
- reload_config command: Fix crash when sql_pool_size option is used
- reload_config command: Fix crash when SQL is not configured
- rooms_empty_destroy command: Several fixes to behave more conservative
- Fix serverhost->host parameter name for muc_(un)register_nick API

* Configuration
- Allow specifying tag for listener for api_permission purposes
- Change default ciphers to intermediate
- Define default ciphers/protocol_option in example config
- Don't crash on malformed 'modules' section
- mod_mam: New option clear_archive_on_room_destroy to prevent archive removal on room destroy
- mod_mam: New option access_preferences to restrict who can modify the MAM preferences
- mod_muc: New option access_mam to restrict who can modify that room option
- mod_offline: New option store_groupchat to allow storing group chat messages

* Core
- Add MQTT protocol support
- Fix (un)setting of priority
- Use OTP application startup infrastructure for starting dependencies
- Improve starting order of several dependencies

* MAM
- mod_mam_mnesia/sql: Improve check for empty archive
- disallow room creation if archive not empty and clear_archive_on_room_destroy is false
- allow check if archive is empty for or user or room
- Additional checks for database failures

* MUC
- Make sure that room_destroyed is called even when some code throws in terminate
- Update muc room state after adding extra access field to it
- MUC/Sub: Send mucsub subscriber notification events with from set to room jid

* Shared Roster
- Don't perform roster push for non-local contacts
- Handle versioning result when shared roster group has remote account
- Fix SQL queries

* Miscelanea
- CAPTCHA: Add no-store hint to CAPTCHA challenge stanzas
- HTTP: Reject http_api request with malformed Authentication header
- mod_carboncopy: Don't lose carbons on presence change or session resumption
- mod_mix: Fix submission-id and channel resource
- mod_ping: Fix ping IQ reply/timeout processing (17.x regression)
- mod_private: Hardcode item ID for PEP bookmarks
- mod_push: Improve notification error handling
- PIEFXIS: Fix user export when password is scrammed
- Prosody: Improve import of roster items, rooms and attributes
- Translations: fixed "make translations"
- WebAdmin: Fix support to restart module with new options

# Version 18.12

* MAM data store compression
* Proxy protocol support (http://www.haproxy.org/download/1.8/doc/proxy-protocol.txt)
* MUC Self-Ping optimization (XEP-0410)
* Bookmarks conversion (XEP-0411)
