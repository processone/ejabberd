#!/bin/bash
set -e

source "${EJABBERD_HOME}/scripts/lib/base_config.sh"
source "${EJABBERD_HOME}/scripts/lib/config.sh"
source "${EJABBERD_HOME}/scripts/lib/base_functions.sh"
source "${EJABBERD_HOME}/scripts/lib/functions.sh"


readonly whoami=$(whoami)


change_ejabberd_run_user() {
    echo "Change ejabberd install user to root..."
    sed -i "s/INSTALLUSER=${EJABBERD_USER}/INSTALLUSER=${whoami}/" ${EJABBERDCTL}
}


[[ "${whoami}" == "root" ]] \
    && change_ejabberd_run_user


exit 0
