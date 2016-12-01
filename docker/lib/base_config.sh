readonly HOSTIP=$(hostname -i)
readonly HOSTNAME=$(hostname -f)
readonly DOMAINNAME=$(hostname -d)

readonly DOCKER_LIB="${EJABBERD_HOME}/docker/lib"
readonly ERLANGCOOKIEFILE="${EJABBERD_HOME}/.erlang.cookie"
readonly EJABBERDCTL="/sbin/ejabberdctl"
readonly CONFIGDIR="${EJABBERD_HOME}/conf"
readonly CONFIGTMPDIR="${EJABBERD_HOME}/docker/conf"
readonly SSLCERTDIR="${EJABBERD_HOME}/ssl"
readonly SSLCERTHOST="${SSLCERTDIR}/host.pem"
readonly LOGDIR="/var/log/ejabberd"
readonly FIRST_START_DONE_FILE="${EJABBERD_HOME}/first-start-done"
readonly CLUSTER_NODE_FILE="${EJABBERD_HOME}/cluster-done"

readonly PYTHON_JINJA2="import os;
import sys;
import jinja2;
sys.stdout.write(
    jinja2.Template
        (sys.stdin.read()
    ).render(env=os.environ))"
