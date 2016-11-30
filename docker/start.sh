#!/bin/bash
set -e

# Environment
export EJABBERD_HTTPS=${EJABBERD_HTTPS:-'true'}
export EJABBERD_STARTTLS=${EJABBERD_STARTTLS:-'true'}
export EJABBERD_S2S_SSL=${EJABBERD_S2S_SSL:-'true'}

source "${EJABBERD_HOME}/docker/lib/base_config.sh"
source "${EJABBERD_HOME}/docker/lib/config.sh"
source "${EJABBERD_HOME}/docker/lib/base_functions.sh"
source "${EJABBERD_HOME}/docker/lib/functions.sh"


# discover hostname
readonly nodename=$(get_nodename)

# set erlang node to node name from get_nodename
if [[ "$ERLANG_NODE" == "nodename" ]]; then
    export ERLANG_NODE="ejabberd@${nodename}"
fi


run_scripts() {
    local run_script=$1
    local run_script_dir="${EJABBERD_HOME}/docker/${run_script}"

    log "Run ${run_script} scripts..."
    for script in ${run_script_dir}/*.sh ; do
        if [ -f ${script} -a -x ${script} ] ; then
            ${script}
        fi
    done
}


_trap() {
    run_scripts "stop"
    log "Stopping ejabberd..."
    $EJABBERDCTL stop
    $EJABBERDCTL stopped
    exit 0
}


# Catch signals and shutdown ejabberd
trap _trap SIGTERM SIGINT

# print logfiles to stdout
tail -F ${LOGDIR}/crash.log \
        ${LOGDIR}/error.log \
        ${LOGDIR}/erlang.log \
        ${LOGDIR}/ejabberd.log &

# start ejabberd
run_scripts "pre"
log "Starting ejabberd..."
$EJABBERDCTL start
$EJABBERDCTL started
log "Ejabberd started."
run_scripts "post"

# run forever
while true; do sleep 1; done

log "Ejabberd stopped."


exit 0
