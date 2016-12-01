#!/bin/bash
set -e

source "${EJABBERD_HOME}/docker/lib/base_config.sh"
source "${EJABBERD_HOME}/docker/lib/config.sh"
source "${EJABBERD_HOME}/docker/lib/base_functions.sh"
source "${EJABBERD_HOME}/docker/lib/functions.sh"

make_dhparam() {
	local dhfile=$1
	local bits=$2

	log "Writing dh file to '${dhfile}'..."
	openssl dhparam -out ${dhfile} ${bits}
}

if is_true ${EJABBERD_DHPARAM} ; then
	file_exist ${SSLDHPARAM} \
		|| make_dhparam ${SSLDHPARAM} 4096
fi

exit 0
