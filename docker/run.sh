#!/bin/bash
set -e

source "${EJABBERD_HOME}/scripts/lib/base_config.sh"
source "${EJABBERD_HOME}/scripts/lib/config.sh"
source "${EJABBERD_HOME}/scripts/lib/base_functions.sh"
source "${EJABBERD_HOME}/scripts/lib/functions.sh"

# discover hostname
readonly nodename=$(get_nodename)

is_zero ${ERLANG_NODE} \
    && export ERLANG_NODE="ejabberd@localhost"

## backward compatibility
# if ERLANG_NODE is true reset it to "ejabberd" and add
# hostname to the nodename.
# else: export ${ERLANG_NODE} with nodename
if (is_true ${ERLANG_NODE}); then
    export ERLANG_NODE="ejabberd@${nodename}"
fi


run_scripts() {
    local run_script_dir="${EJABBERD_HOME}/scripts/${1}"
    for script in ${run_script_dir}/*.sh ; do
        if [ -f ${script} -a -x ${script} ] ; then
            ${script}
        fi
    done
}


pre_scripts() {
    run_scripts "pre"
}


post_scripts() {
    run_scripts "post"
}

stop_scripts() {
    run_scripts "stop"
}


ctl() {
    local action="$1"
    ${EJABBERDCTL} ${action} >/dev/null
}


_trap() {
    echo "Stopping ejabberd..."
    stop_scripts
    if ctl stop ; then
        local cnt=0
        sleep 1
        while ctl status || test $? = 1 ; do
            cnt=`expr $cnt + 1`
            if [ $cnt -ge 60 ] ; then
                break
            fi
            sleep 1
        done
    fi
}


# Catch signals and shutdown ejabberd
trap _trap SIGTERM SIGINT

## run ejabberd
case "$@" in
    start)
        pre_scripts
        tail -n 0 -F ${LOGDIR}/crash.log \
                ${LOGDIR}/error.log \
                ${LOGDIR}/erlang.log &
        echo "Starting ejabberd..."
        exec ${EJABBERDCTL} "foreground" &
        child=$!
        ${EJABBERDCTL} "started"
        post_scripts
        wait $child
    ;;
    live)
        pre_scripts
        echo "Starting ejabberd in 'live' mode..."
        exec ${EJABBERDCTL} "live"
    ;;
    shell)
        exec "/bin/bash"
    ;;
    *)
        exec $@
    ;;
esac
