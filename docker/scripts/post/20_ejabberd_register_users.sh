#!/bin/bash

source "${EJABBERD_HOME}/scripts/lib/base_config.sh"
source "${EJABBERD_HOME}/scripts/lib/config.sh"
source "${EJABBERD_HOME}/scripts/lib/base_functions.sh"
source "${EJABBERD_HOME}/scripts/lib/functions.sh"

# Do not exit if users already registered
set +e

randpw() {
    < /dev/urandom tr -dc A-Z-a-z-0-9 | head -c ${1:-16};
    echo;
}


register_user() {
    local user=$1
    local domain=$2
    local password=$3

    ${EJABBERDCTL} register ${user} ${domain} ${password}
    return $?
}


register_all_users() {
    # register users from environment $EJABBERD_USERS with given
    # password or random password written to stout. Use whitespace
    # to seperate users.
    #
    # sample:
    # - add a user with an given password:
    #   -e "EJABBERD_USERS=admin@example.com:adminSecret"
    # - add a user with a random password:
    #   -e "EJABBERD_USERS=user@example.com"
    # - set password for admin and use random for user1:
    #   -e "EJABBERD_USERS=admin@example.com:adminSecret user@example.com"

    for user in ${EJABBERD_USERS} ; do
        local jid=${user%%:*}
        local password=${user#*:}

        local username=${jid%%@*}
        local domain=${jid#*@}

        [[ "${password}" == "${jid}" ]] \
            && password=$(randpw)

        register_user ${username} ${domain} ${password}
        local retval=$?

        [[ ${retval} -eq 0 ]] \
            && echo "Password for user ${username}@${domain} is ${password}"
    done
}


file_exist ${FIRST_START_DONE_FILE} \
    && exit 0


file_exist ${CLUSTER_NODE_FILE} \
    && exit 0


is_set ${EJABBERD_USERS} \
    && register_all_users


##################################
## Keep for backward compatibility

register_all_ejabberd_admins() {
    # add all admins from environment $EJABBERD_ADMINS with the passwords from
    # environment $EJABBERD_ADMIN_PASS.

    local passwords
    local IFS=' '
    read -a passwords <<< "${EJABBERD_ADMIN_PWD}"

    for admin in ${EJABBERD_ADMINS} ; do
        local user=${admin%%@*}
        local domain=${admin#*@}
        local password=${passwords[0]}
        passwords=("${passwords[@]:1}")
        register_user ${user} ${domain} ${password}
    done
}


register_all_ejabberd_admins_randpw() {
    # add all admins from environment $EJABBERD_ADMINS with a random
    # password and write the password to stdout.

    for admin in ${EJABBERD_ADMINS} ; do
        local user=${admin%%@*}
        local domain=${admin#*@}
        local password=$(randpw)

        register_user ${user} ${domain} ${password}
        local retval=$?

        [[ ${retval} -eq 0 ]] \
            && echo "Password for user ${user}@${domain} is ${password}"
    done
}


is_set ${EJABBERD_ADMIN_PWD} \
    && register_all_ejabberd_admins


is_true ${EJABBERD_ADMIN_RANDPWD} \
    && register_all_ejabberd_admins_randpw


exit 0
