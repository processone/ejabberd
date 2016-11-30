#!/bin/bash
set -e

source "${EJABBERD_HOME}/docker/lib/base_config.sh"
source "${EJABBERD_HOME}/docker/lib/config.sh"
source "${EJABBERD_HOME}/docker/lib/base_functions.sh"
source "${EJABBERD_HOME}/docker/lib/functions.sh"


make_snakeoil_certificate() {
    local domain=$1
    local certfile=$2

    openssl req -subj "/CN=${domain}" \
                -new \
                -newkey rsa:4096 \
                -days 365 \
                -nodes \
                -x509 \
                -keyout /tmp/selfsigned.key \
                -out /tmp/selfsigned.crt

    log "Writing ssl cert and private key to '${certfile}'..."
    cat /tmp/selfsigned.crt /tmp/selfsigned.key > ${certfile}
    rm /tmp/selfsigned.crt /tmp/selfsigned.key
}


make_host_snakeoil_certificate() {
    local IFS=@
    local domain='localhost'
    local erlang_node=${ERLANG_NODE}

    if is_true ${erlang_node} ; then
        domain=${HOSTNAME}
    elif is_set ${erlang_node} ; then
        set ${erlang_node}
        local nodehost=$2
        if is_zero ${nodehost} ; then
            domain=${HOSTNAME}
        else
            domain=${nodehost}
        fi
    fi

    log "Generating snakeoil ssl cert for ${domain}..."

    make_snakeoil_certificate ${domain} ${SSLCERTHOST}
}


make_domain_snakeoil_certificate() {
    local domain=$1
    local certfile=$2

    log "Generating snakeoil ssl cert for ${domain}..."

    make_snakeoil_certificate ${domain} ${certfile}
}


# generate host ssl cert if missing
file_exist ${SSLCERTHOST} \
  || make_host_snakeoil_certificate


# generate xmmp domain ssl certificates if missing
for xmpp_domain in ${XMPP_DOMAIN} ; do
  domain_certfile="${SSLCERTDIR}/${xmpp_domain}.pem"
  file_exist ${domain_certfile} \
    || make_domain_snakeoil_certificate ${xmpp_domain} ${domain_certfile}
done


exit 0
