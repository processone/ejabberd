#!/bin/bash
set -e

source "${EJABBERD_HOME}/docker/lib/base_config.sh"
source "${EJABBERD_HOME}/docker/lib/config.sh"
source "${EJABBERD_HOME}/docker/lib/base_functions.sh"
source "${EJABBERD_HOME}/docker/lib/functions.sh"


leave_cluster() {
    log "Leave cluster..."
    rm ${CLUSTER_NODE_FILE}
    NO_WARNINGS=true ${EJABBERDCTL} leave_cluster
}


file_exist ${CLUSTER_NODE_FILE} \
    && leave_cluster


exit 0
