# overwrite get_nodename to discover hostname from DNS
get_nodename() {
    local hostname=${HOSTNAME}

    # get hostname from dns
    if ( is_true ${USE_DNS} ); then
        # wait for dns registration
        sleep 1

        nodename=$(discover_dns_hostname ${HOSTIP})

        is_set ${nodename} \
            && hostname=${nodename}
    fi

    echo $hostname
    return 0
}


# discover hostname from dns with a reverse lookup
discover_dns_hostname() {
    local hostip=$1

    # try to get the hostname from dns
    local dnsname=$(drill -x ${hostip} \
        | grep PTR \
        | awk '{print $5}' \
        | grep -E "^[a-zA-Z0-9]+([-._]?[a-zA-Z0-9]+)*.[a-zA-Z]+\.$" \
        | cut -d '.' -f1 \
        | tail -1)

    is_set ${dnsname} \
        && echo ${dnsname}

    return 0
}
