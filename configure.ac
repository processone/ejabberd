#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.53)
AC_PACKAGE_VERSION(3.0.0)
AC_INIT(ejabberd, 3.0.0, [ejabberd@process-one.net], [ejabberd])

# Checks for programs.
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_SED

if test "x$GCC" = "xyes"; then
    CFLAGS="$CFLAGS -Wall"
fi

# Checks Erlang runtime and compiler
AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC

# Checks and sets ERLANG_ROOT_DIR and ERLANG_LIB_DIR variable
# AC_ERLANG_SUBST_ROOT_DIR
# AC_ERLANG_SUBST_LIB_DIR

#locating escript
AC_PATH_PROG([ESCRIPT], [escript], [])

#locating rebar
AC_PATH_PROG([REBAR], [rebar], [./rebar])

#locating make
AC_CHECK_PROG([MAKE], [make], [make], [])

if test "x$ESCRIPT" = "x"; then
   AC_MSG_ERROR(['escript' was not found])
fi

if test "x$MAKE" = "x"; then
   AC_MSG_ERROR(['make' was not found])
fi

# Change default prefix
AC_PREFIX_DEFAULT(/)

AC_ARG_ENABLE(hipe,
[AC_HELP_STRING([--enable-hipe], [compile natively with HiPE, not recommended (default: no)])],
[case "${enableval}" in
  yes) hipe=true ;;
  no)  hipe=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-hipe) ;;
esac],[hipe=false])

AC_ARG_ENABLE(roster_gateway_workaround,
[AC_HELP_STRING([--enable-roster-gateway-workaround], [turn on workaround for processing gateway subscriptions (default: no)])],
[case "${enableval}" in
  yes) roster_gateway_workaround=true ;;
  no)  roster_gateway_workaround=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-roster-gateway-workaround) ;;
esac],[roster_gateway_workaround=false])

AC_ARG_ENABLE(flash_hack,
[AC_HELP_STRING([--enable-flash-hack], [support Adobe Flash client XML (default: no)])],
[case "${enableval}" in
  yes) flash_hack=true ;;
  no)  flash_hack=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-flash-hack) ;;
esac],[flash_hack=false])

AC_ARG_ENABLE(transient_supervisors,
[AC_HELP_STRING([--enable-transient_supervisors], [use Erlang supervision for transient process (default: no)])],
[case "${enableval}" in
  yes) transient_supervisors=true ;;
  no)  transient_supervisors=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-transient_supervisors) ;;
esac],[transient_supervisors=false])

AC_ARG_ENABLE(full_xml,
[AC_HELP_STRING([--enable-full-xml], [use XML features in XMPP stream (ex: CDATA) (default: no, requires XML compliant clients)])],
[case "${enableval}" in
  yes) full_xml=true ;;
  no)  full_xml=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-full-xml) ;;
esac],[full_xml=false])

AC_ARG_ENABLE(nif,
[AC_HELP_STRING([--enable-nif], [replace some functions with C equivalents. Requires Erlang R13B04 or higher (default: no)])],
[case "${enableval}" in
  yes) nif=true ;;
  no)  nif=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-nif) ;;
esac],[nif=false])

AC_ARG_ENABLE(mssql,
[AC_HELP_STRING([--enable-mssql], [use Microsoft SQL Server database (default: no, requires --enable-odbc)])],
[case "${enableval}" in
  yes) db_type=mssql ;;
  no)  db_type=generic ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-mssql) ;;
esac],[db_type=generic])

AC_ARG_ENABLE(mysql,
[AC_HELP_STRING([--enable-mysql], [enable MySQL support (default: no)])],
[case "${enableval}" in
  yes) mysql=true ;;
  no)  mysql=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-mysql) ;;
esac],[mysql=false])

AC_ARG_ENABLE(pgsql,
[AC_HELP_STRING([--enable-pgsql], [enable PostgreSQL support (default: no)])],
[case "${enableval}" in
  yes) pgsql=true ;;
  no)  pgsql=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-pgsql) ;;
esac],[pgsql=false])

AC_ARG_ENABLE(pam,
[AC_HELP_STRING([--enable-pam], [enable PAM support (default: no)])],
[case "${enableval}" in
  yes) pam=true ;;
  no)  pam=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-pam) ;;
esac],[pam=false])

AC_ARG_ENABLE(zlib,
[AC_HELP_STRING([--enable-zlib], [enable Stream Compression (XEP-0138) using zlib (default: yes)])],
[case "${enableval}" in
  yes) zlib=true ;;
  no)  zlib=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-zlib) ;;
esac],[zlib=true])

AC_ARG_ENABLE(stun,
[AC_HELP_STRING([--enable-stun], [enable STUN support (default: no)])],
[case "${enableval}" in
  yes) stun=true ;;
  no)  stun=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-stun) ;;
esac],[stun=false])

AC_ARG_ENABLE(riak,
[AC_HELP_STRING([--enable-riak], [enable Riak support (default: no)])],
[case "${enableval}" in
  yes) riak=true ;;
  no)  riak=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-riak) ;;
esac],[riak=false])

AC_ARG_ENABLE(json,
[AC_HELP_STRING([--enable-json], [enable JSON support for mod_bosh (default: no)])],
[case "${enableval}" in
  yes) json=true ;;
  no)  json=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-json) ;;
esac],[json=false])

AC_ARG_ENABLE(iconv,
[AC_HELP_STRING([--enable-iconv], [enable iconv support (default: no)])],
[case "${enableval}" in
  yes) iconv=true ;;
  no)  iconv=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-iconv) ;;
esac],[iconv=false])

AC_ARG_ENABLE(debug,
[AC_HELP_STRING([--enable-debug], [enable debug information (default: yes)])],
[case "${enableval}" in
  yes) debug=true ;;
  no)  debug=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-debug) ;;
esac],[debug=true])

AC_ARG_ENABLE(pubsub_ng,
[AC_HELP_STRING([--enable-pubsub_ng], [enable PubSub NG (default: no)])],
[case "${enableval}" in
  yes) pubsub_ng=true ;;
  no)  pubsub_ng=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-pubsub_ng) ;;
esac],[pubsub_ng=false])

AC_ARG_ENABLE(http,
[AC_HELP_STRING([--enable-http], [build external HTTP libraries ('ibrowse' and 'lhttpc', default: no)])],
[case "${enableval}" in
  yes) http=true ;;
  no)  http=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-http) ;;
esac],[http=false])

AC_ARG_ENABLE(all,
[AC_HELP_STRING([--enable-all], [same as --enable-nif --enable-mysql --enable-pgsql --enable-pam --enable-zlib --enable-stun --enable-riak --enable-json --enable-iconv --enable-debug --enable-pubsub_ng --enable-http (useful for Dialyzer checks, default: no)])],
[case "${enableval}" in
  yes) nif=true mysql=true pgsql=true pam=true zlib=true stun=true riak=true json=true iconv=true debug=true pubsub_ng=true http=true ;;
  no) nif=false mysql=false pgsql=false pam=false zlib=false stun=false riak=false json=false iconv=false debug=false pubsub_ng=false http=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-all) ;;
esac],[])

AC_CONFIG_FILES([Makefile
		 config.vars
		 src/ejabberd.app.src])

ENABLEUSER=""
AC_ARG_ENABLE(user,
  [AS_HELP_STRING([--enable-user[[[[=USER]]]]], [allow this system user to start ejabberd (default: no)])],
  [case "${enableval}" in
     yes) ENABLEUSER=`whoami` ;;
     no) ENABLEUSER="" ;;
     *) ENABLEUSER=$enableval
   esac],
  [])
if test "$ENABLEUSER" != ""; then
  echo "allow this system user to start ejabberd: $ENABLEUSER"
  AC_SUBST([INSTALLUSER], [$ENABLEUSER])
fi

AC_SUBST(hipe)
AC_SUBST(roster_gateway_workaround)
AC_SUBST(flash_hack)
AC_SUBST(transient_supervisors)
AC_SUBST(full_xml)
AC_SUBST(nif)
AC_SUBST(db_type)
AC_SUBST(mysql)
AC_SUBST(pgsql)
AC_SUBST(pam)
AC_SUBST(zlib)
AC_SUBST(stun)
AC_SUBST(riak)
AC_SUBST(json)
AC_SUBST(iconv)
AC_SUBST(debug)
AC_SUBST(pubsub_ng)
AC_SUBST(http)

AC_OUTPUT
