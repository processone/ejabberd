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

AC_ARG_ENABLE(mssql,
[AC_HELP_STRING([--enable-mssql], [use Microsoft SQL Server database (default: no, requires --enable-odbc)])],
[case "${enableval}" in
  yes) db_type=mssql ;;
  no)  db_type=generic ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-mssql) ;;
esac],[db_type=generic])

AC_ARG_ENABLE(tools,
[AC_HELP_STRING([--enable-tools], [build development tools (currently the ejabberd profiler only, default: no)])],
[case "${enableval}" in
  yes) tools=true ;;
  no)  tools=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-tools) ;;
esac],[tools=false])

AC_ARG_ENABLE(all,
[AC_HELP_STRING([--enable-all], [same as --enable-nif --enable-mysql --enable-pgsql --enable-pam --enable-zlib --enable-stun --enable-riak --enable-json --enable-iconv --enable-debug --enable-pubsub_ng --enable-http (useful for Dialyzer checks, default: no)])],
[case "${enableval}" in
  yes) nif=true mysql=true pgsql=true pam=true zlib=true stun=true riak=true json=true iconv=true debug=true pubsub_ng=true http=true ;;
  no) nif=false mysql=false pgsql=false pam=false zlib=false stun=false riak=false json=false iconv=false debug=false pubsub_ng=false http=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-all) ;;
esac],[])

AC_ARG_ENABLE(nif,
[AC_HELP_STRING([--enable-nif], [replace some functions with C equivalents. Requires Erlang R13B04 or higher (default: no)])],
[case "${enableval}" in
  yes) nif=true ;;
  no)  nif=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-nif) ;;
esac],[if test "x$nif" = "x"; then nif=false; fi])

AC_ARG_ENABLE(mysql,
[AC_HELP_STRING([--enable-mysql], [enable MySQL support (default: no)])],
[case "${enableval}" in
  yes) mysql=true ;;
  no)  mysql=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-mysql) ;;
esac],[if test "x$mysql" = "x"; then mysql=false; fi])

AC_ARG_ENABLE(pgsql,
[AC_HELP_STRING([--enable-pgsql], [enable PostgreSQL support (default: no)])],
[case "${enableval}" in
  yes) pgsql=true ;;
  no)  pgsql=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-pgsql) ;;
esac],[if test "x$pgsql" = "x"; then pgsql=false; fi])

AC_ARG_ENABLE(pam,
[AC_HELP_STRING([--enable-pam], [enable PAM support (default: no)])],
[case "${enableval}" in
  yes) pam=true ;;
  no)  pam=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-pam) ;;
esac],[if test "x$pam" = "x"; then pam=false; fi])

AC_ARG_ENABLE(zlib,
[AC_HELP_STRING([--enable-zlib], [enable Stream Compression (XEP-0138) using zlib (default: yes)])],
[case "${enableval}" in
  yes) zlib=true ;;
  no)  zlib=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-zlib) ;;
esac],[if test "x$zlib" = "x"; then zlib=true; fi])

AC_ARG_ENABLE(stun,
[AC_HELP_STRING([--enable-stun], [enable STUN support (default: no)])],
[case "${enableval}" in
  yes) stun=true ;;
  no)  stun=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-stun) ;;
esac],[if test "x$stun" = "x"; then stun=false; fi])

AC_ARG_ENABLE(riak,
[AC_HELP_STRING([--enable-riak], [enable Riak support (default: no)])],
[case "${enableval}" in
  yes) riak=true ;;
  no)  riak=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-riak) ;;
esac],[if test "x$riak" = "x"; then riak=false; fi])

AC_ARG_ENABLE(json,
[AC_HELP_STRING([--enable-json], [enable JSON support for mod_bosh (default: no)])],
[case "${enableval}" in
  yes) json=true ;;
  no)  json=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-json) ;;
esac],[if test "x$json" = "x"; then json=false; fi])

AC_ARG_ENABLE(iconv,
[AC_HELP_STRING([--enable-iconv], [enable iconv support (default: yes)])],
[case "${enableval}" in
  yes) iconv=true ;;
  no)  iconv=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-iconv) ;;
esac],[if test "x$iconv" = "x"; then iconv=true; fi])

AC_ARG_ENABLE(debug,
[AC_HELP_STRING([--enable-debug], [enable debug information (default: yes)])],
[case "${enableval}" in
  yes) debug=true ;;
  no)  debug=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-debug) ;;
esac],[if test "x$debug" = "x"; then debug=true; fi])

AC_ARG_ENABLE(pubsub_ng,
[AC_HELP_STRING([--enable-pubsub_ng], [enable PubSub NG (default: no)])],
[case "${enableval}" in
  yes) pubsub_ng=true ;;
  no)  pubsub_ng=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-pubsub_ng) ;;
esac],[if test "x$pubsub_ng" = "x"; then pubsub_ng=false; fi])

AC_ARG_ENABLE(http,
[AC_HELP_STRING([--enable-http], [build external HTTP libraries ('ibrowse' and 'lhttpc', default: no)])],
[case "${enableval}" in
  yes) http=true ;;
  no)  http=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-http) ;;
esac],[if test "x$http" = "x"; then http=false; fi])

AC_CONFIG_FILES([Makefile
		 vars.config
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

AC_ERLANG_CHECK_LIB([sasl], [],
    [AC_MSG_ERROR([Erlang application 'sasl' was not found])])
AC_ERLANG_CHECK_LIB([crypto], [],
    [AC_MSG_ERROR([Erlang application 'crypto' was not found])])
AC_ERLANG_CHECK_LIB([public_key], [],
    [AC_MSG_ERROR([Erlang application 'public_key' was not found])])
AC_ERLANG_CHECK_LIB([ssl], [],
    [AC_MSG_ERROR([Erlang application 'ssl' was not found])])
AC_ERLANG_CHECK_LIB([mnesia], [],
    [AC_MSG_ERROR([Erlang application 'mnesia' was not found])])
AC_ERLANG_CHECK_LIB([inets], [],
    [AC_MSG_ERROR([Erlang application 'inets' was not found])])
AC_ERLANG_CHECK_LIB([odbc], [],
    [AC_MSG_ERROR([Erlang application 'odbc' was not found])])
AC_ERLANG_CHECK_LIB([compiler], [],
    [AC_MSG_ERROR([Erlang application 'compiler' was not found])])
if test "x$tools" = "xtrue"; then
   AC_ERLANG_CHECK_LIB([tools], [],
      [AC_MSG_ERROR([Erlang application 'tools' was not found])])
   AC_ERLANG_CHECK_LIB([runtime_tools], [],
      [AC_MSG_ERROR([Erlang application 'runtime_tools' was not found])])
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
AC_SUBST(tools)

AC_OUTPUT
