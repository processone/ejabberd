#!/bin/bash
set -e

# Write a first-start-done file

source "${EJABBERD_HOME}/docker/lib/base_config.sh"
source "${EJABBERD_HOME}/docker/lib/config.sh"
source "${EJABBERD_HOME}/docker/lib/base_functions.sh"
source "${EJABBERD_HOME}/docker/lib/functions.sh"


if [ ! -e "${FIRST_START_DONE_FILE}" ]; then
    touch ${FIRST_START_DONE_FILE}
fi


exit 0
