# Managing pubsub nodes through HTTP Atompub #


## Configuration ##
	
These options will be used by the service to know how to build URLs. Using the previous configuration items the service should be accessed through `http://notify.push.bbc.co.uk:5280/pshb/<host>/<node>/`.

Also, in the ejabberd_http handler configuration, add the identified line.

	{5280, ejabberd_http, [
		 http_poll, 
		 web_admin,
		 {request_handlers, [{["pshb"], pshb_http}]} % this should be added
	]}

It will automatically detect the version of mod_pubsub (odbc or mnesia) and call the appropriate module.

## Important notice ##

In the current version of the code, some security checks are not done :

 * node creation uses the default `all` access_createnode acl, not checking for the actual configuration.

 * most read operations are successfully executed without authentication. HOWEVER listing items can only be done when the node access_model is "open". In all other cases, the service returns 403. A finer grained authentication will be implemented.


## Usage example with cURL ##

### Errors ###

HTTP status codes are used as intended. Additionally, the XMPP error stanza can also be set in the body :

	$ curl -i -X POST -u cstar@localhost:encore -d @createnode.xml http://localhost:5280/pshb/localhost
	HTTP/1.1 409 Conflict
	Content-Type: text/html; charset=utf-8
	Content-Length: 95
	Content-type: application/xml
	
	<error code='409' type='cancel'><conflict xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/></error>

or

	$ curl -i -X DELETE -u cstar@localhost:encore http://localhost:5280/pshb/localhost/princely_musings
	HTTP/1.1 404 Not Found
	Content-Type: text/html; charset=utf-8
	Content-Length: 101
	Content-type: application/xml

	<error code='404' type='cancel'><item-not-found xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/></error>

### Getting the service document ###

No authentication necessary. All nodes are listed.

 $ curl -i http://host:port/pshb/domain/
	
### Getting items from a node ###

No authentication done, and all nodes are accessible.

 $ curl -i http://host:port/pshb/domain/node/


### Posting a new item ###

	$ curl -u jid:password -i -X POST -d @entry.atom http://post:port/pshb/domain/node 
	
User ability to post is based on node configuration.

### Editing a new item ###

	$ curl -u jid:password -i -X POST -d @entry.atom http://post:port/pshb/domain/node/itemid 
	
User ability to post is based on node configuration.

### Deleting an item ###

	$ curl -u jid:password -i -X DELETE http://post:port/pshb/domain/node/itemid 
	
User ability to post is based on node configuration.


### Creating a new node ###

An instant node can be created if server configuration allows:

	$ curl -X POST -u cstar@localhost:encore -d "" http://localhost:5280/pshb/localhost
	
or

	$ curl -X POST -u cstar@localhost:encore -d "<pubsub><create node='princely_musings'/></pubsub>" http://localhost:5280/pshb/localhost
	
configure element (as per XEP-60) can be passed in the pubsub body.
	
	$ cat createnode.xml
	<pubsub><create node='princely_musings' type='flat'/>
	<x xmlns='jabber:x:data' type='submit'>
	  <field var='FORM_TYPE' type='hidden'>
	    <value>http://jabber.org/protocol/pubsub#node_config</value>
	  </field>
	  <field var='pubsub#title'><value>Princely Musings (Atom)</value></field>
	  <field var='pubsub#max_payload_size'><value>1028</value></field>
	  <field var='pubsub#type'><value>Atom</value></field>
	</x>
	</pubsub>
	
	$ curl -i -X POST -u cstar@localhost:encore -d @createnode.xml http://localhost:5280/pshb/localhost
	HTTP/1.1 200 OK
	Content-Length: 130
	Content-Type: application/xml

	<?xml version="1.0" encoding="utf-8"?><pubsub xmlns='http://jabber.org/protocol/pubsub'><create node='princely_musings'/></pubsub>
	
### Editing a node configuration ###

	$ cat editnode.xml
	<pubsub xmlns='http://jabber.org/protocol/pubsub#owner'>
	    <configure node='princely_musings'>
	      <x xmlns='jabber:x:data' type='submit'>
	        <field var='FORM_TYPE' type='hidden'>
	          <value>http://jabber.org/protocol/pubsub#node_config</value>
	        </field>
	        <field var='pubsub#title'><value>Princely Musings (Atom)</value></field>
	        <field var='pubsub#deliver_notifications'><value>1</value></field>
	        <field var='pubsub#deliver_payloads'><value>1</value></field>
	        <field var='pubsub#persist_items'><value>1</value></field>
	        <field var='pubsub#max_items'><value>10</value></field>
	        <field var='pubsub#item_expire'><value>604800</value></field>
	        <field var='pubsub#access_model'><value>roster</value></field>
				</x>
			</configure>
		</pubsub>
		

	$ curl -i -X PUT -u cstar@localhost:encore -d @createnode.xml http://localhost:5280/pshb/localhost/princely_musings
	
	
	
### Deleting a node ###

A node is deleted by: 

	$ curl -X DELETE -u cstar@localhost:encore http://localhost:5280/pshb/localhost/princely_musings



