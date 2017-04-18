#!/bin/bash
set -e

source "${EJABBERD_HOME}/scripts/lib/base_config.sh"
source "${EJABBERD_HOME}/scripts/lib/config.sh"
source "${EJABBERD_HOME}/scripts/lib/base_functions.sh"
source "${EJABBERD_HOME}/scripts/lib/functions.sh"

# Instead of having to mount a direction, specify the ssl certs
# via environment variables:
# `EJABBERD_SSLCERT_HOST` and `EJABBERD_SSLCERT_{domain_name}`.
# For example: `EJABBERD_SSLCERT_EXAMPLE_COM`.

write_file_from_env() {
    echo "Writing $1 to $2"
    mkdir -p "$(dirname $2)"
    echo "${!1}" > $2
}

# Write the host certificate
is_set ${EJABBERD_SSLCERT_HOST} \
  && write_file_from_env "EJABBERD_SSLCERT_HOST" ${SSLCERTHOST}

# Write the domain certificates for each XMPP_DOMAIN
for xmpp_domain in ${XMPP_DOMAIN} ; do
    var="EJABBERD_SSLCERT_$(echo $xmpp_domain | awk '{print toupper($0)}' | sed 's/\./_/g;s/-/_/g')"
    if is_set ${!var} ; then
        file_exist "${SSLCERTDIR}/${xmpp_domain}.pem" \
          || write_file_from_env "$var" "${SSLCERTDIR}/${xmpp_domain}.pem"
    fi
done

exit 0
