#
# In this file you can configure options that are passed by ejabberdctl
# to the erlang runtime system when starting ejabberd
#

#' POLL: Kernel polling ([true|false])
#
# The kernel polling option requires support in the kernel.
# Additionally, you need to enable this feature while compiling Erlang.
#
# Default: true
#
#POLL=true

#.
#' SMP: SMP support ([enable|auto|disable])
#
# Explanation in Erlang/OTP documentation:
# enable: starts the Erlang runtime system with SMP support enabled.
#   This may fail if no runtime system with SMP support is available.
# auto: starts the Erlang runtime system with SMP support enabled if it
#   is available and more than one logical processor are detected.
# disable: starts a runtime system without SMP support.
#
# Default: auto
#
#SMP=auto

#.
#' ERL_MAX_PORTS: Maximum number of simultaneously open Erlang ports
#
# ejabberd consumes two or three ports for every connection, either
# from a client or from another Jabber server. So take this into
# account when setting this limit.
#
# Default: 32000
# Maximum: 268435456
#
#ERL_MAX_PORTS=32000

#.
#' FIREWALL_WINDOW: Range of allowed ports to pass through a firewall
#
# If Ejabberd is configured to run in cluster, and a firewall is blocking ports,
# it's possible to make Erlang use a defined range of port (instead of dynamic
# ports) for node communication.
#
# Default: not defined
# Example: 4200-4210
#
#FIREWALL_WINDOW=

#.
#' INET_DIST_INTERFACE: IP address where this Erlang node listens other nodes
#
# This communication is used by ejabberdctl command line tool,
# and in a cluster of several ejabberd nodes.
#
# Default: 127.0.0.1
#
#INET_DIST_INTERFACE=127.0.0.1

#.
#' ERL_EPMD_ADDRESS: IP addresses where epmd listens for connections
#
# IMPORTANT: This option works only in Erlang/OTP R14B03 and newer.
#
# This environment variable may be set to a comma-separated
# list of IP addresses, in which case the epmd daemon
# will listen only on the specified address(es) and on the
# loopback address (which is implicitly added to the list if it
# has not been specified). The default behaviour is to listen on
# all available IP addresses.
#
# Default: 0.0.0.0
#
#ERL_EPMD_ADDRESS=127.0.0.1

#.
#' ERL_PROCESSES: Maximum number of Erlang processes
#
# Erlang consumes a lot of lightweight processes. If there is a lot of activity
# on ejabberd so that the maximum number of processes is reached, people will
# experience greater latency times. As these processes are implemented in
# Erlang, and therefore not related to the operating system processes, you do
# not have to worry about allowing a huge number of them.
#
# Default: 250000
# Maximum: 268435456
#
#ERL_PROCESSES=250000

#.
#' ERL_MAX_ETS_TABLES: Maximum number of ETS and Mnesia tables
#
# The number of concurrent ETS and Mnesia tables is limited. When the limit is
# reached, errors will appear in the logs:
#   ** Too many db tables **
# You can safely increase this limit when starting ejabberd. It impacts memory
# consumption but the difference will be quite small.
#
# Default: 1400
#
#ERL_MAX_ETS_TABLES=1400

#.
#' ERL_OPTIONS: Additional Erlang options
#
# The next variable allows to specify additional options passed to erlang while
# starting ejabberd. Some useful options are -noshell, -detached, -heart. When
# ejabberd is started from an init.d script options -noshell and -detached are
# added implicitly. See erl(1) for more info.
#
# It might be useful to add "-pa /usr/local/lib/ejabberd/ebin" if you
# want to add local modules in this path.
#
# Default: ""
#
ERL_OPTIONS="{{ env['ERL_OPTIONS'] or "-noshell" }}"

#.
#' ERLANG_NODE: Erlang node name
#
# The next variable allows to explicitly specify erlang node for ejabberd
# It can be given in different formats:
# ERLANG_NODE=ejabberd
#   Lets erlang add hostname to the node (ejabberd uses short name in this case)
# ERLANG_NODE=ejabberd@hostname
#   Erlang uses node name as is (so make sure that hostname is a real
#   machine hostname or you'll not be able to control ejabberd)
# ERLANG_NODE=ejabberd@hostname.domainname
#   The same as previous, but erlang will use long hostname
#   (see erl (1) manual for details)
#
# Default: ejabberd@localhost
#
ERLANG_NODE={{ env['ERLANG_NODE'] or "ejabberd@localhost" }}

#.
#' EJABBERD_PID_PATH: ejabberd PID file
#
# Indicate the full path to the ejabberd Process identifier (PID) file.
# If this variable is defined, ejabberd writes the PID file when starts,
# and deletes it when stops.
# Remember to create the directory and grant write permission to ejabberd.
#
# Default: don't write PID file
#
#EJABBERD_PID_PATH=/var/run/ejabberd/ejabberd.pid

#.
#' EJABBERD_CONFIG_PATH: ejabberd configuration file
#
# Specify the full path to the ejabberd configuration file. If the file name has
# yml or yaml extension, it is parsed as a YAML file; otherwise, Erlang syntax is
# expected.
#
# Default: $ETC_DIR/ejabberd.yml
#
#EJABBERD_CONFIG_PATH=/etc/ejabberd/ejabberd.yml

#.
#' CONTRIB_MODULES_PATH: contributed ejabberd modules path
#
# Specify the full path to the contributed ejabberd modules. If the path is not
# defined, ejabberd will use ~/.ejabberd-modules in home of user running ejabberd.
#
# Default: $HOME/.ejabberd-modules
#
#CONTRIB_MODULES_PATH=/opt/ejabberd-modules

#.
#' EJABBERD_BYPASS_WARNINGS: Bypass LIVE warning
#
# Default: don't bypass the warning
#
EJABBERD_BYPASS_WARNINGS=true

#.
#' SPOOL_DIR: Database spool dir
#
# Specify the full path to the database spool dir used in binary installer for
# backwards compatibility.
#
# Docker: rroemhild/ejabberd
#
SPOOL_DIR=$EJABBERD_HOME/database/$ERLANG_NODE
