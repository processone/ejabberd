readonly HOSTIP=$(hostname -i)
readonly HOSTNAME=$(hostname -f)
readonly DOMAINNAME=$(hostname -d)

readonly ERLANGCOOKIEFILE="${EJABBERD_HOME}/.erlang.cookie"
readonly EJABBERDCTL="/sbin/ejabberdctl"
readonly CONFIGFILE="${EJABBERD_HOME}/conf/ejabberd.yml"
readonly CONFIGTEMPLATE="${EJABBERD_HOME}/conf/ejabberd.yml.tpl"
readonly CTLCONFIGFILE="${EJABBERD_HOME}/conf/ejabberdctl.cfg"
readonly CTLCONFIGTEMPLATE="${EJABBERD_HOME}/conf/ejabberdctl.cfg.tpl"
readonly SSLCERTDIR="${EJABBERD_HOME}/ssl"
readonly SSLCERTHOST="${SSLCERTDIR}/host.pem"
readonly SSLDHPARAM="${SSLCERTDIR}/dh.pem"
readonly LOGDIR="/var/log/ejabberd"
readonly FIRST_START_DONE_FILE="/${EJABBERD_HOME}/first-start-done"
readonly CLUSTER_NODE_FILE="/${EJABBERD_HOME}/cluster-done"

readonly PYTHON_JINJA2="import os;
import sys;
import jinja2;
sys.stdout.write(
    jinja2.Template
        (sys.stdin.read()
    ).render(env=os.environ))"

# backward compatibility environment variables
set +e

[[ -n $EJABBERD_ADMIN ]] \
    && export EJABBERD_ADMINS=${EJABBERD_ADMIN}

[[ -n $AUTH_METHOD ]] \
    && export EJABBERD_AUTH_METHOD=${AUTH_METHOD}

[[ -n $SKIP_MODULES_UPDATE ]] \
    && export EJABBERD_SKIP_MODULES_UPDATE=${SKIP_MODULES_UPDATE}

[[ -n $ERL_OPTIONS ]] \
    && export ERLANG_OPTIONS=${ERL_OPTIONS}

[[ -n $SSLCERT_HOST ]] \
    && export EJABBERD_SSLCERT_HOST=${SSLCERT_HOST}

[[ -n $SSLCERT_EXAMPLE_COM ]] \
    && export EJABBERD_SSLCERT_EXAMPLE_COM=${SSLCERT_EXAMPLE_COM}

[[ -n $LOGLEVEL ]] \
    && export EJABBERD_LOGLEVEL=${LOGLEVEL}

[[ -n $EJABBERD_WEB_ADMIN_SSL ]] \
    && export EJABBERD_HTTPS=${EJABBERD_WEB_ADMIN_SSL}

set -e
