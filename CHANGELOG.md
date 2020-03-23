# Version 20.03

* Changes in this version
- Add support of ssl connection when connection to mysql
  database (configured with `sql_ssl: true` option)
- Experimental support for cockroachdb when configured
  with postgres connector 
- Add cache and optimize queries issued by `mod_shared_roster`,
  this should greatly improve performance of this module when
  used with `sql` backend
- Fix problem with accessing webadmin
- Make webadmin work even when url is missing trailing slash
- When compiling external modules with ext_mod, use flags
  that were detected during compilation of ejabberd
- Make config changed to ldap options be updated when issued
  `reload_config` command
- Fix `room_empty_destory` command
- Fix reporting errors in `send_stanza` command when xml
  passed to it couldn't be passed correctly

# Version 20.02

* Changes in this version
- Fix problems when trying to use string format with unicode
  values directly in xmpp nodes
- Add missing oauth_client table declaration in lite.new.sql
- Improve compatibility with CocroachDB
- Fix importing of piefxis files that did use scram passwords
- Fix importing of piefxis files that had multiple includes
  in them
- Update jiffy dependency
- Allow storage of emojis when using mssql database (Thanks
  to Christoph Scholz)
- Make ejabberd_auth_http be able to use auth_opts
- Make custom_headers options in http modules correctly
  override built-in values
- Fix return value of reload_config and dump_config commands

# Version 20.01

* New features
- Implement OAUTH authentication in mqtt
- Make logging infrastructure use new logger introduced
  in Erlang (requires OTP22)
- New configuration parser/validator
- Initial work on being able to use CockroachDB as database backend
- Add gc command
- Add option to disable using prepared statements on Postgresql
- Implement routine for converting password to SCRAM format
  for all backends not only SQL
- Add infrastructure for having module documentation directly
  in individual module source code
- Generate man page automaticaly
- Implement copy feature in mod_carboncopy

* Fixes
- Make webadmin work with configurable paths
- Fix handling of result in xmlrpc module
- Make webadmin work even when accessed through not declared domain
- Better error reporting in xmlrpc
- Limit ammount of results returned by disco queries to pubsub nodes
- Improve validation of configured JWT keys
- Fix race condition in Redis/SQL startup
- Fix loading order of third party modules
- Fix reloading of ACL rules
- Make account removal requests properly route response
- Improve handling of malformed inputs in send_message command
- Omit push notification if storing message in offline storage
  failed
- Fix crash in stream management when timeout was not set

# Version 19.09

* Admin
- The minimum required Erlang/OTP version is now 19.3
- Fix API call using OAuth (#2982)
- Rename MUC command arguments from Host to Service (#2976)

* Webadmin
- Don't treat 'Host' header as a virtual XMPP host (#2989)
- Fix some links to Guide in WebAdmin and add new ones (#3003)
- Use select fields to input host in WebAdmin Backup (#3000)
- Check account auth provided in WebAdmin is a local host (#3000)

* ACME
- Improve ACME implementation
- Fix IDA support in ACME requests
- Fix unicode formatting in ACME module
- Log an error message on IDNA failure
- Support IDN hostnames in ACME requests
- Don't attempt to create ACME directory on ejabberd startup
- Don't allow requesting certificates for localhost or IP-like domains
- Don't auto request certificate for localhost and IP-like domains
- Add listener for ACME challenge in example config

* Authentication
- JWT-only authentication for some users (#3012)

* MUC
- Apply default role after revoking admin affiliation (#3023)
- Custom exit message is not broadcast (#3004)
- Revert "Affiliations other than admin and owner cannot invite to members_only rooms" (#2987)
- When join new room with password, set pass and password_protected (#2668)
- Improve rooms_* commands to accept 'global' as MUC service argument (#2976)
- Rename MUC command arguments from Host to Service (#2976)

* SQL
- Fix transactions for Microsoft SQL Server (#2978)
- Spawn SQL connections on demand only

* Misc
- Add support for XEP-0328: JID Prep
- Added gsfonts for captcha
- Log Mnesia table type on creation
- Replicate Mnesia 'bosh' table when nodes are joined
- Fix certificate selection for s2s (#3015)
- Provide meaningful error when adding non-local users to shared roster (#3000)
- Websocket: don't treat 'Host' header as a virtual XMPP host (#2989)
- Fix sm ack related c2s error (#2984)
- Don't hide the reason why c2s connection has failed
- Unicode support
- Correctly handle unicode in log messages
- Fix unicode processing in ejabberd.yml

# Version 19.08

* Administration
- Improve ejabberd halting procedure
- Process unexpected erlang messages uniformly: logging a warning
- mod_configure: Remove modules management

* Configuration
- Use new configuration validator
- ejabberd_http: Use correct virtual host when consulting trusted_proxies
- Fix Elixir modules detection in the configuration file
- Make option 'validate_stream' global
- Allow multiple definitions of host_config and append_host_config
- Introduce option 'captcha_url'
- mod_stream_mgmt: Allow flexible timeout format
- mod_mqtt: Allow flexible timeout format in session_expiry option

* Misc
- Fix SQL connections leakage
- New authentication method using JWT tokens
- extauth: Add 'certauth' command
- Improve SQL pool logic
- Add and improve type specs
- Improve extraction of translated strings
- Improve error handling/reporting when loading language translations
- Improve hooks validator and fix bugs related to hooks registration
- Gracefully close inbound s2s connections
- mod_mqtt: Fix usage of TLS
- mod_offline: Make count_offline_messages cache work when using mam for storage
- mod_privacy: Don't attempt to query 'undefined' active list
- mod_privacy: Fix race condition

* MUC
- Add code for hibernating inactive muc_room processes
- Improve handling of unexpected iq in mod_muc_room
- Attach mod_muc_room processes to a supervisor
- Restore room when receiving message or generic iq for not started room
- Distribute routing of MUC messages accross all CPU cores

* PubSub
- Fix pending nodes retrieval for SQL backend
- Check access_model when publishing PEP
- Remove deprecated pubsub plugins
- Expose access_model and publish_model in pubsub#metadata

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
