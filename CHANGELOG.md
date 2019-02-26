# Version NEXT

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
