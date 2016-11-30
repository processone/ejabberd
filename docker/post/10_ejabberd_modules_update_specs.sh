#!/bin/bash
set -e

# Updates the known modules as to be found in https://github.com/processone/ejabberd-contrib

source "${EJABBERD_HOME}/docker/lib/base_config.sh"
source "${EJABBERD_HOME}/docker/lib/config.sh"
source "${EJABBERD_HOME}/docker/lib/base_functions.sh"
source "${EJABBERD_HOME}/docker/lib/functions.sh"


run_modules_update_specs() {
    log "Updating module specs... "
    ${EJABBERDCTL} modules_update_specs
}


is_true ${EJABBERD_SKIP_MODULES_UPDATE} \
    && exit 0

run_modules_update_specs


exit 0
