# Version NEXT

*

# Version 18.12.1

* Fix compilation with rebar3 by updating xmpp dependency
* Fix issue with ordering of pkix and fast_tls startup
* Fix loosing info about enabling carbons after resuming old session
* Re-enable port 5280 in default config
* Don't add ver attribute to roster result when roster versioning is not enabled

# Version 18.12

* MAM data store compression
* Proxy protocol support (http://www.haproxy.org/download/1.8/doc/proxy-protocol.txt)
* MUC Self-Ping optimization (XEP-0410)
* Bookmarks conversion (XEP-0411)
