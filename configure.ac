#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(ejabberd, m4_esyscmd([echo `git describe --tags 2>/dev/null || echo 25.07` | sed 's/-g.*//;s/-/./' | tr -d '\012']), [ejabberd@process-one.net], [ejabberd])

AC_ARG_WITH(min-erlang,
	AS_HELP_STRING([--with-min-erlang=version],[set minimal required erlang version, default to OTP25]),
[if test "X$withval" = "X"; then
    REQUIRE_ERLANG_MIN="13.0 (Erlang/OTP 25.0)"
else
    REQUIRE_ERLANG_MIN="$withval"
fi
], [REQUIRE_ERLANG_MIN="13.0 (Erlang/OTP 25.0)"])

REQUIRE_ERLANG_MAX="100.0.0 (No Max)"

AC_CONFIG_MACRO_DIR([m4])

# Checks for programs.
AC_PROG_MAKE_SET
AC_PROG_AWK
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_PROG_SED

if test "x$GCC" = "xyes"; then
    CFLAGS="$CFLAGS -Wall"
fi

# Checks Erlang runtime and compiler
AC_ARG_WITH(erlang,
	AS_HELP_STRING([--with-erlang=dir],[search for erlang in dir]),
[if test "$withval" = "yes" -o "$withval" = "no" -o "X$with_erlang" = "X"; then
    extra_erl_path=""
else
    extra_erl_path="$with_erlang:$with_erlang/bin:"
fi
])

AC_ARG_WITH(rebar,
	AS_HELP_STRING([--with-rebar=bin],[use as build tool the rebar/rebar3/mix binary specified]),
[if test "$withval" = "yes" -o "$withval" = "no" -o "X$with_rebar" = "X"; then
    rebar="rebar3"
else
    rebar="$with_rebar"
fi
], [rebar="unconfigured"])

AC_PATH_TOOL(ERL, erl, , [${extra_erl_path}$PATH])
AC_PATH_TOOL(ERLC, erlc, , [${extra_erl_path}$PATH])
AC_PATH_TOOL(EPMD, epmd, , [${extra_erl_path}$PATH])

AC_PATH_TOOL(REBAR, rebar, , [${extra_erl_path}$PATH])
AC_PATH_TOOL(REBAR3, rebar3, , [${extra_erl_path}$PATH])
AC_PATH_TOOL(ELIXIR, elixir, , [${extra_erl_path}$PATH])
AC_PATH_TOOL(IEX, iex, , [${extra_erl_path}$PATH])
AC_PATH_TOOL(MIX, mix, , [${extra_erl_path}$PATH])

if test "$rebar" = unconfigured; then
    if test "x$ELIXIR" = "x" -o "x$IEX" = "x" -o "x$MIX" = "x"; then
        if test "x$REBAR3" = "x"; then
            rebar="rebar3"
        else
            rebar=$REBAR3
        fi
    else
        rebar=$MIX
    fi
fi
if test "x$rebar" = "xrebar" -a "x$REBAR" = "x" ; then
    rebar="./rebar"
fi
if test "x$rebar" = "xrebar3" -a "x$REBAR3" = "x" ; then
    rebar="./rebar3"
fi

AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC

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
AC_PREFIX_DEFAULT(/usr/local)

AC_CONFIG_FILES([Makefile
		 vars.config])

AC_ARG_ENABLE(all,
[AS_HELP_STRING([--enable-all],[same as --enable-odbc --enable-mssql --enable-mysql --enable-pgsql --enable-sqlite --enable-pam --enable-zlib --enable-redis --enable-elixir --enable-stun --enable-sip --enable-debug --enable-lua --enable-tools (useful for Dialyzer checks, default: no)])],
[case "${enableval}" in
  yes) odbc=true mssql=true mysql=true pgsql=true sqlite=true pam=true zlib=true redis=true elixir=true stun=true sip=true debug=true lua=true tools=true ;;
  no) odbc=false mssql=false mysql=false pgsql=false sqlite=false pam=false zlib=false redis=false elixir=false stun=false sip=false debug=false lua=false tools=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-all) ;;
esac],[])

AC_ARG_ENABLE(debug,
[AS_HELP_STRING([--enable-debug],[enable debug information (default: yes)])],
[case "${enableval}" in
  yes) debug=true ;;
  no)  debug=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-debug) ;;
esac],[if test "x$debug" = "x"; then debug=true; fi])

AC_ARG_ENABLE(elixir,
[AS_HELP_STRING([--enable-elixir],[enable Elixir support in Rebar3 (default: no)])],
[case "${enableval}" in
  yes) elixir=true ;;
  no)  elixir=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-elixir) ;;
esac],[if test "x$elixir" = "x"; then elixir=false; fi])

AC_ARG_ENABLE(erlang-version-check,
[AS_HELP_STRING([--enable-erlang-version-check],[Check Erlang/OTP version (default: yes)])])
case "$enable_erlang_version_check" in
	yes|'')
		ERLANG_VERSION_CHECK([$REQUIRE_ERLANG_MIN],[$REQUIRE_ERLANG_MAX])
		;;
	no)
		ERLANG_VERSION_CHECK([$REQUIRE_ERLANG_MIN],[$REQUIRE_ERLANG_MAX],[warn])
		;;
esac

AC_ARG_ENABLE(full_xml,
[AS_HELP_STRING([--enable-full-xml],[use XML features in XMPP stream (ex: CDATA) (default: no, requires XML compliant clients)])],
[case "${enableval}" in
  yes) full_xml=true ;;
  no)  full_xml=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-full-xml) ;;
esac],[full_xml=false])

ENABLEGROUP=""
AC_ARG_ENABLE(group,
  [AS_HELP_STRING([--enable-group[[=GROUP]]], [specify the group of the account defined in --enable-user (default: no)])],
  [case "${enableval}" in
     yes) ENABLEGROUP=`groups |head -n 1` ;;
     no) ENABLEGROUP="" ;;
     *) ENABLEGROUP=$enableval
   esac],
  [])
if test "$ENABLEGROUP" != ""; then
  echo "allow this system group to start ejabberd: $ENABLEGROUP"
  AC_SUBST([INSTALLGROUP], [$ENABLEGROUP])
fi

AC_ARG_ENABLE(latest_deps,
[AS_HELP_STRING([--enable-latest-deps],[makes rebar use latest commits for dependencies instead of tagged versions (default: no)])],
[case "${enableval}" in
  yes) latest_deps=true ;;
  no)  latest_deps=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-latest-deps) ;;
esac],[if test "x$latest_deps" = "x"; then latest_deps=false; fi])

AC_ARG_ENABLE(lua,
[AS_HELP_STRING([--enable-lua],[enable Lua support, to import from Prosody (default: no)])],
[case "${enableval}" in
  yes) lua=true ;;
  no)  lua=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-lua) ;;
esac],[if test "x$lua" = "x"; then lua=false; fi])

AC_ARG_ENABLE(mssql,
[AS_HELP_STRING([--enable-mssql],[use Microsoft SQL Server database (default: no, requires --enable-odbc)])],
[case "${enableval}" in
  yes) mssql=true ;;
  no)  mssql=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-mssql) ;;
esac],[if test "x$mssql" = "x"; then mssql=false; fi])

AC_ARG_ENABLE(mysql,
[AS_HELP_STRING([--enable-mysql],[enable MySQL support (default: no)])],
[case "${enableval}" in
  yes) mysql=true ;;
  no)  mysql=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-mysql) ;;
esac],[if test "x$mysql" = "x"; then mysql=false; fi])

AC_ARG_ENABLE(new_sql_schema,
[AS_HELP_STRING([--enable-new-sql-schema],[use new SQL schema by default (default: no)])],
[case "${enableval}" in
  yes) new_sql_schema=true ;;
  no)  new_sql_schema=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-new-sql-schema) ;;
esac],[new_sql_schema=false])

AC_ARG_ENABLE(odbc,
[AS_HELP_STRING([--enable-odbc],[enable pure ODBC support (default: no)])],
[case "${enableval}" in
  yes) odbc=true ;;
  no)  odbc=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-odbc) ;;
esac],[if test "x$odbc" = "x"; then odbc=false; fi])

AC_ARG_ENABLE(pam,
[AS_HELP_STRING([--enable-pam],[enable PAM support (default: no)])],
[case "${enableval}" in
  yes) pam=true ;;
  no)  pam=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-pam) ;;
esac],[if test "x$pam" = "x"; then pam=false; fi])

AC_ARG_ENABLE(pgsql,
[AS_HELP_STRING([--enable-pgsql],[enable PostgreSQL support (default: no)])],
[case "${enableval}" in
  yes) pgsql=true ;;
  no)  pgsql=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-pgsql) ;;
esac],[if test "x$pgsql" = "x"; then pgsql=false; fi])

AC_ARG_ENABLE(redis,
[AS_HELP_STRING([--enable-redis],[enable Redis support (default: no)])],
[case "${enableval}" in
  yes) redis=true ;;
  no)  redis=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-redis) ;;
esac],[if test "x$redis" = "x"; then redis=false; fi])

AC_ARG_ENABLE(roster_gateway_workaround,
[AS_HELP_STRING([--enable-roster-gateway-workaround],[turn on workaround for processing gateway subscriptions (default: no)])],
[case "${enableval}" in
  yes) roster_gateway_workaround=true ;;
  no)  roster_gateway_workaround=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-roster-gateway-workaround) ;;
esac],[roster_gateway_workaround=false])

AC_ARG_ENABLE(sip,
[AS_HELP_STRING([--enable-sip],[enable SIP support (default: no)])],
[case "${enableval}" in
  yes) sip=true ;;
  no)  sip=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-sip) ;;
esac],[if test "x$sip" = "x"; then sip=false; fi])

AC_ARG_ENABLE(sqlite,
[AS_HELP_STRING([--enable-sqlite],[enable SQLite support (default: no)])],
[case "${enableval}" in
  yes) sqlite=true ;;
  no)  sqlite=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-sqlite) ;;
esac],[if test "x$sqlite" = "x"; then sqlite=false; fi])

AC_ARG_ENABLE(stun,
[AS_HELP_STRING([--enable-stun],[enable STUN/TURN support (default: yes)])],
[case "${enableval}" in
  yes) stun=true ;;
  no)  stun=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-stun) ;;
esac],[if test "x$stun" = "x"; then stun=true; fi])

AC_ARG_ENABLE(system_deps,
[AS_HELP_STRING([--enable-system-deps],[makes rebar use locally installed dependencies instead of downloading them (default: no)])],
[case "${enableval}" in
  yes) system_deps=true ;;
  no)  system_deps=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-system-deps) ;;
esac],[if test "x$system_deps" = "x"; then system_deps=false; fi])

AC_ARG_ENABLE(tools,
[AS_HELP_STRING([--enable-tools],[include debugging/development tools (default: no)])],
[case "${enableval}" in
  yes) tools=true ;;
  no)  tools=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-tools) ;;
esac],[if test "x$tools" = "x"; then tools=false; fi])

ENABLEUSER=""
AC_ARG_ENABLE(user,
  [AS_HELP_STRING([--enable-user[[=USER]]], [allow this system user to start ejabberd (default: no)])],
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

AC_ARG_ENABLE(zlib,
[AS_HELP_STRING([--enable-zlib],[enable Stream Compression (XEP-0138) using zlib (default: yes)])],
[case "${enableval}" in
  yes) zlib=true ;;
  no)  zlib=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-zlib) ;;
esac],[if test "x$zlib" = "x"; then zlib=true; fi])

case "`uname`" in
  "Darwin")
    # Darwin (macos) erlang-sqlite is built using amalgamated lib, so no external dependency
    ;;
  *)
    if test "$sqlite" = "true"; then
      AX_LIB_SQLITE3([3.6.19])
      if test "x$SQLITE3_VERSION" = "x"; then
        AC_MSG_ERROR(SQLite3 library >= 3.6.19 was not found)
      fi
    fi
  ;;
esac

AC_MSG_RESULT([build tool to use (change using --with-rebar): $rebar])

AC_SUBST(roster_gateway_workaround)
AC_SUBST(new_sql_schema)
AC_SUBST(full_xml)
AC_SUBST(odbc)
AC_SUBST(mssql)
AC_SUBST(mysql)
AC_SUBST(pgsql)
AC_SUBST(sqlite)
AC_SUBST(pam)
AC_SUBST(zlib)
AC_SUBST(rebar)
AC_SUBST(redis)
AC_SUBST(elixir)
AC_SUBST(stun)
AC_SUBST(sip)
AC_SUBST(debug)
AC_SUBST(lua)
AC_SUBST(tools)
AC_SUBST(latest_deps)
AC_SUBST(system_deps)
AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)
AC_SUBST(LDFLAGS)

AC_OUTPUT

AS_CASE([$rebar],
 [*rebar3], [
  deps=""
  AS_IF([test "x$stun" = "xfalse"], [deps="stun,$deps"])
  AS_IF([test "x$sqlite" = "xfalse"], [deps="sqlite3,$deps"])
  AS_IF([test "x$pgsql" = "xfalse"], [deps="p1_pgsql,$deps"])
  AS_IF([test "x$mysql" = "xfalse"], [deps="p1_mysql,$deps"])
  AS_IF([test "x$zlib" = "xfalse"], [deps="ezlib,$deps"])
  AS_IF([test "x$sip" = "xfalse"], [deps="esip,$deps"])
  AS_IF([test "x$redis" = "xfalse"], [deps="eredis,$deps"])
  AS_IF([test "x$pam" = "xfalse"], [deps="epam,$deps"])
  AS_IF([test "x$deps" = "x"], [],
   [AC_MSG_NOTICE([unlocking disabled rebar3 dependencies: $deps])
    $rebar unlock "$deps"])
  deps=""
  ERLANG_VERSION=m4_esyscmd([erl -noinput -noshell -eval 'erlang:display(list_to_integer(erlang:system_info(otp_release))), halt().'])
  AS_IF([test "$ERLANG_VERSION" -lt "21"], [deps="luerl,$deps"])
  AS_IF([test "$ERLANG_VERSION" -lt "22"], [deps="lager,$deps"])
  AS_IF([test "$ERLANG_VERSION" -le "23"], [deps="jose,$deps"])
  AS_IF([test "$ERLANG_VERSION" -ge "27"], [deps="jiffy,$deps"])
  AS_IF([test "x$deps" = "x"], [],
   [AC_MSG_NOTICE([unlocking rebar3 dependencies for old Erlang/OTP: $deps])
    $rebar unlock "$deps"])
 ])
