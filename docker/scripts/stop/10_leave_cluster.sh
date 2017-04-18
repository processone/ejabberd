#!/bin/bash
set -e

source "${EJABBERD_HOME}/scripts/lib/base_config.sh"
source "${EJABBERD_HOME}/scripts/lib/config.sh"
source "${EJABBERD_HOME}/scripts/lib/base_functions.sh"
source "${EJABBERD_HOME}/scripts/lib/functions.sh"


leave_cluster() {
    echo "Leave cluster... "
    rm ${CLUSTER_NODE_FILE}
    NO_WARNINGS=true ${EJABBERDCTL} leave_cluster
}


file_exist ${CLUSTER_NODE_FILE} \
    && leave_cluster


exit 0
