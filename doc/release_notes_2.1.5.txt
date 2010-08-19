
                              Release Notes
                             ejabberd 2.1.5

  ejabberd 2.1.5 is the fifth release in ejabberd 2.1.x branch,
  and includes several minor bugfixes and a few improvements.

  Read more details about the changes in:
    http://redir.process-one.net/ejabberd-2.1.4

  Download the source code and installers from:
    http://www.process-one.net/en/ejabberd/


  This is the full list of changes:

* Authentication
- Extauth: Support parallel script running (EJAB-1280)
- mod_register: Return Registered element when account exists

* ejabberdctl
- Fix print of command result that contains ~
- Fix problem when FIREWALL_WINDOW options for erl kernel were used
- Fix typo in update_list command (EJAB-1237)
- Some systems delete the lock dir; in such case don't use flock at all
- The command Update now returns meaningful message and exit-status (EJAB-1237)

* HTTP-Bind (BOSH)
- Don't say v1.2 in the Bind HTTP page
- New optional BOSH connection attribute process-delay (EJAB-1257)

* MUC
- Document the mod_muc option captcha_protected
- Now admins are able to see private rooms in disco (EJAB-1269)
- Show some more room options in the log file

* ODBC
- Correct handling of SQL boolean types (EJAB-1275)
- Discard too old queued requests (the caller has already got a timeout)
- Fixes wrong SQL escaping when --enable-full-xml is set
- Use ets insead of asking supervisor in ejabberd_odbc_sup:get_pids/1

* Pubsub, PEP and Caps
- Enforce disco features results (EJAB-1033, EJAB-1228, EJAB-1238)
- Support all the hash functions required by Caps XEP-0115

* Requirements
- Fixed support for Erlang R12; which doesn't support: true andalso ok
- Support OTP R14A by using public_key library instead of old ssl (EJAB-953)
- Requirement of OpenSSL increased from 0.9.6 to 0.9.8
- OpenSSL is now required, not optional

* Other
- Don't ask for client certificate when using tls (EJAB-1267)
- Fix typo in --enable-transient_supervisors
- Fix privacy check when serving local Last (EJAB-1271)
- Inform client that SSL session caching is disabled
- New configure option: --enable-nif
- Use driver allocator in C files for reflecting memory in erlang:memory(system)
- Debug: New p1_prof compiled with: make debugtools=true
- Debug: Added functions to collect stats about queues, memory, reductions etc
- HTTP: Log error if request has ambiguous Host header (EJAB-1261)
- Logs: When logging s2s out connection attempt or success, log if TLS is used
- Shared Rosters: When account is deleted, delete also member of stored rosters


  Bug reports

  You can officially report bugs on ProcessOne support site:
  http://support.process-one.net/
