#!/bin/bash
set -e

source "${EJABBERD_HOME}/scripts/lib/base_config.sh"
source "${EJABBERD_HOME}/scripts/lib/config.sh"
source "${EJABBERD_HOME}/scripts/lib/base_functions.sh"
source "${EJABBERD_HOME}/scripts/lib/functions.sh"


make_config() {
    if [ ! -e ${CONFIGFILE} ]; then
        echo "Generating ejabberd config file..."
        cat ${CONFIGTEMPLATE} \
          | python -c "${PYTHON_JINJA2}" \
          > ${CONFIGFILE}
    else
        echo "ejabberd config file exists."
    fi

    if [ ! -e ${CTLCONFIGFILE} ]; then
        echo "Generating ejabberdctl config file..."
        cat ${CTLCONFIGTEMPLATE} \
          | python -c "${PYTHON_JINJA2}" \
          > ${CTLCONFIGFILE}
    else
        echo "ejabberdctl config file exists."
    fi
}


file_exist ${FIRST_START_DONE_FILE} \
    && exit 0


# generate config file
make_config

exit 0
