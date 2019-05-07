#!/bin/bash

# Add standard config items
cat <<END >>$RIAK_CONF
nodename = riak@$HOST
distributed_cookie = $CLUSTER_NAME
listener.protobuf.internal = $HOST:$PB_PORT
listener.http.internal = $HOST:$HTTP_PORT
END
