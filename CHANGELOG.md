Changelog
================================================================================
MongooseIM 1.2. is a new version of MongooseIM 1.1. - a robust, reliable XMPP
server capable of running on big clusters. 




MongooseIM 1.2
--------------------------------------------------------------------------------
- BOSH support (XMPP over HTTP) 
- WSS (WebSocekt Secure) 
- various XMPP related metrics exposed via REST API
  (https://github.com/esl/ejabberd/wiki/REST-interface-to-folsom-metrics) 
- alarm handler for better monitoring and bottleneck finding 

We've also assured compatibility with the latest ejabberd Community Edition by
ProcessOne, so that backporting ejabberd modules to MongooseIM requires
less effort.
