# Managing pubsub nodes through HTTP Atompub #


## Configuration ##
	
These options will be used by the service to know how to build URLs. Using the previous configuration items the service should be accessed through `http://notify.push.bbc.co.uk:5280/pshb/<host>/<node>/`.

Also, in the ejabberd_http handler configuration, add the identified line.

	{5280, ejabberd_http, [
		 http_poll, 
		 web_admin,
		 {request_handlers, [{["pshb"], pshb_http}]} % this should be added
	]}

## Important notice ##

In the current version of the code, some security checks are not done :

 * read operations can all be done without authentication

 * node creation uses the default `all` access_createnode acl, not checking for the actual configuration.

## Usage example with cURL ##

### Getting the service document ###

No authentication necessary. All nodes are listed.

 curl -i http://host:port/pshb/domain/
	
### Getting items from a node ###

No authentication done, and all nodes are accessible.

 curl -i http://host:port/pshb/domain/node/


### Posting a new item ###

	curl -u jid:password -i -X POST -d @entry.atom http://post:port/pshb/domain/node 
	
User ability to post is based on node configuration.


### Creating a new node ###

An instant node can be created if server configuration allows:

	curl -X POST -u cstar@localhost:encore -d "" http://localhost:5280/pshb/localhost
	
or

	curl -X POST -u cstar@localhost:encore -d "<pubsub><create node='princely_musings'/></pubsub>" http://localhost:5280/pshb/localhost
	
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
	
	$ curl -X POST -u cstar@localhost:encore -d @createnode.xml http://localhost:5280/pshb/localhost
	
### Deleting a node ###

A node is deleted by: 

	curl -X DELETE -u cstar@localhost:encore http://localhost:5280/pshb/localhost/princely_musings



