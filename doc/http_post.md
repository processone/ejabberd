# Managing pubsub nodes through HTTP Atompub #


## Configuration ##
	
These options will be used by the service to know how to build URLs. Using the previous configuration items the service should be accessed through `http://notify.push.bbc.co.uk:5280/pshb/<host>/<node>/`.

Also, in the ejabberd_http handler configuration, add the identified line.

	{5280, ejabberd_http, [
		 http_poll, 
		 web_admin,
		 {request_handlers, [{["pshb"], pshb_http}]} % this should be added
	]}

## Usage example with cURL ##

### Getting the service document ###

No authentication necessary. All nodes are listed.

 curl -i http://<host>:<port>/pshb/<domain>/
	
### Getting items from a node ###

No authentication done, and all nodes are accessible.

 curl -i http://<host>:<port>/pshb/<domain>/<node>/


### Posting a new item ###

	curl -u <jid>:<password> -i -X POST -d @entry.atom http://<host>:<port>/pshb/<domain>/<node> 
	
User ability to post is based on node configuration.


### Creating a new node ###

An instant node can be created :

	curl -X POST -u cstar@localhost:encore -d "" http://localhost:5280/pshb/localhost



