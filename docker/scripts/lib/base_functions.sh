is_set() {
    local var=$1

    [[ -n $var ]]
}


is_zero() {
    local var=$1

    [[ -z $var ]]
}


file_exist() {
    local file=$1

    [[ -e $file ]]
}


is_true() {
    local var=${1,,}
    local choices=("yes" "1" "y" "true")
    for ((i=0;i < ${#choices[@]};i++)) {
        [[ "${choices[i]}" == $var ]] && return 0
    }
    return 1
}


# overwrite this function to get hostname from other sources
# like dns or etcd
get_nodename() {
    echo ${HOSTNAME}
}


join_cluster() {
    local cluster_node=$1

    is_zero ${cluster_node} \
        && exit 0

    echo "Join cluster..."

    local erlang_node_name=${ERLANG_NODE%@*}
    local erlang_cluster_node="${erlang_node_name}@${cluster_node}"

    response=$(${EJABBERDCTL} ping ${erlang_cluster_node})
    while [ "$response" != "pong" ]; do
        echo "Waiting for ${erlang_cluster_node}..."
        sleep 2
        response=$(${EJABBERDCTL} ping ${erlang_cluster_node})
    done

    echo "Join cluster at ${erlang_cluster_node}... "
    NO_WARNINGS=true ${EJABBERDCTL} join_cluster $erlang_cluster_node

    if [ $? -eq 0 ]; then
        touch ${CLUSTER_NODE_FILE}
    else
        echo "cloud not join cluster"
        exit 1
    fi
}
