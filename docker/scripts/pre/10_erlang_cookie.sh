#!/bin/bash
set -e

source "${EJABBERD_HOME}/scripts/lib/base_config.sh"
source "${EJABBERD_HOME}/scripts/lib/config.sh"
source "${EJABBERD_HOME}/scripts/lib/base_functions.sh"
source "${EJABBERD_HOME}/scripts/lib/functions.sh"


set_erlang_cookie() {
    echo "Set erlang cookie to ${ERLANG_COOKIE}..."
    echo ${ERLANG_COOKIE} > ${ERLANGCOOKIEFILE}
    chmod 400 ${ERLANGCOOKIEFILE}
}


file_exist ${FIRST_START_DONE_FILE} \
    && exit 0


# set erlang cookie if ERLANG_COOKIE is set in environemt
is_set ${ERLANG_COOKIE} \
    && set_erlang_cookie

exit 0
