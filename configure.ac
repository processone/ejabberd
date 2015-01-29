#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.53)
AC_INIT(ejabberd, m4_esyscmd([echo `git describe --tags 2>/dev/null || echo 0.0` | sed 's/-g.*//;s/-/./' | tr -d '\012']), [ejabberd@process-one.net], [ejabberd])
REQUIRE_ERLANG_MIN="5.9.1 (Erlang/OTP R15B01)"
REQUIRE_ERLANG_MAX="9.0.0 (No Max)"

# Checks for programs.
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_SED

if test "x$GCC" = "xyes"; then
    CFLAGS="$CFLAGS -Wall"
fi

# Checks Erlang runtime and compiler
AC_ARG_WITH(erlang,
	AC_HELP_STRING([--with-erlang=dir],
		[search for erlang in dir]),
[if test "$withval" = "yes" -o "$withval" = "no" -o "X$with_erlang" = "X"; then
    extra_erl_path=""
else
    extra_erl_path="$with_erlang:$with_erlang/bin:"
fi
])

AC_PATH_TOOL(ERL, erl, , [${extra_erl_path}$PATH])
AC_PATH_TOOL(ERLC, erlc, , [${extra_erl_path}$PATH])

AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC

AC_ARG_ENABLE(erlang-version-check,
[AC_HELP_STRING([--enable-erlang-version-check],
	[Check Erlang/OTP version @<:@default=yes@:>@])])
case "$enable_erlang_version_check" in
	yes|'')
		ERLANG_VERSION_CHECK([$REQUIRE_ERLANG_MIN],[$REQUIRE_ERLANG_MAX])
		;;
	no)
		ERLANG_VERSION_CHECK([$REQUIRE_ERLANG_MIN],[$REQUIRE_ERLANG_MAX],[warn])
		;;
esac

# Checks and sets ERLANG_ROOT_DIR and ERLANG_LIB_DIR variable
AC_ERLANG_SUBST_ROOT_DIR
# AC_ERLANG_SUBST_LIB_DIR

#locating escript
AC_PATH_PROG([ESCRIPT], [escript], [], [$ERLANG_ROOT_DIR/bin])

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

AC_ARG_ENABLE(transient_supervisors,
[AC_HELP_STRING([--disable-transient-supervisors], [disable Erlang supervision for transient processes (default: no)])],
[case "${enableval}" in
  yes) transient_supervisors=true ;;
  no)  transient_supervisors=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-transient_supervisors) ;;
esac],[transient_supervisors=true])

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

AC_ARG_ENABLE(all,
[AC_HELP_STRING([--enable-all], [same as --enable-nif --enable-odbc --enable-mysql --enable-pgsql --enable-pam --enable-zlib --enable-riak --enable-json --enable-elixir --enable-iconv --enable-debug --enable-lager --enable-tools (useful for Dialyzer checks, default: no)])],
[case "${enableval}" in
  yes) nif=true odbc=true mysql=true pgsql=true pam=true zlib=true riak=true json=true elixir=true iconv=true debug=true lager=true tools=true ;;
  no) nif=false odbc=false mysql=false pgsql=false pam=false zlib=false riak=false json=false elixir=false iconv=false debug=false lager=false tools=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-all) ;;
esac],[])

AC_ARG_ENABLE(tools,
[AC_HELP_STRING([--enable-tools], [build development tools (default: no)])],
[case "${enableval}" in
  yes) tools=true ;;
  no)  tools=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-tools) ;;
esac],[if test "x$tools" = "x"; then tools=false; fi])

AC_ARG_ENABLE(nif,
[AC_HELP_STRING([--enable-nif], [replace some functions with C equivalents. Requires Erlang R13B04 or higher (default: no)])],
[case "${enableval}" in
  yes) nif=true ;;
  no)  nif=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-nif) ;;
esac],[if test "x$nif" = "x"; then nif=false; fi])

AC_ARG_ENABLE(odbc,
[AC_HELP_STRING([--enable-odbc], [enable pure ODBC support (default: no)])],
[case "${enableval}" in
  yes) odbc=true ;;
  no)  odbc=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-odbc) ;;
esac],[if test "x$odbc" = "x"; then odbc=false; fi])

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

AC_ARG_ENABLE(elixir,
[AC_HELP_STRING([--enable-elixir], [enable Elixir support (default: no)])],
[case "${enableval}" in
  yes) elixir=true ;;
  no)  elixir=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-elixir) ;;
esac],[if test "x$elixir" = "x"; then elixir=false; fi])

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

AC_ARG_ENABLE(lager,
[AC_HELP_STRING([--enable-lager], [enable lager support (default: yes)])],
[case "${enableval}" in
  yes) lager=true ;;
  no)  lager=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-lager) ;;
esac],[if test "x$lager" = "x"; then lager=true; fi])

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

AC_SUBST(hipe)
AC_SUBST(roster_gateway_workaround)
AC_SUBST(transient_supervisors)
AC_SUBST(full_xml)
AC_SUBST(nif)
AC_SUBST(db_type)
AC_SUBST(odbc)
AC_SUBST(mysql)
AC_SUBST(pgsql)
AC_SUBST(pam)
AC_SUBST(zlib)
AC_SUBST(riak)
AC_SUBST(json)
AC_SUBST(elixir)
AC_SUBST(iconv)
AC_SUBST(debug)
AC_SUBST(lager)
AC_SUBST(tools)

AC_OUTPUT
