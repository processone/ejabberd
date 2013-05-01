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
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR

#locating escript
AC_PATH_PROG([ESCRIPT], [escript], [])

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
AC_SUBST(hipe)

AC_ARG_ENABLE(roster_gateway_workaround,
[AC_HELP_STRING([--enable-roster-gateway-workaround], [turn on workaround for processing gateway subscriptions (default: no)])],
[case "${enableval}" in
  yes) roster_gateway_workaround=true ;;
  no)  roster_gateway_workaround=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-roster-gateway-workaround) ;;
esac],[roster_gateway_workaround=false])
AC_SUBST(roster_gateway_workaround)

AC_ARG_ENABLE(flash_hack,
[AC_HELP_STRING([--enable-flash-hack], [support Adobe Flash client XML (default: no)])],
[case "${enableval}" in
  yes) flash_hack=true ;;
  no)  flash_hack=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-flash-hack) ;;
esac],[flash_hack=false])
AC_SUBST(flash_hack)

AC_ARG_ENABLE(mssql,
[AC_HELP_STRING([--enable-mssql], [use Microsoft SQL Server database (default: no, requires --enable-odbc)])],
[case "${enableval}" in
  yes) db_type=mssql ;;
  no)  db_type=generic ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-mssql) ;;
esac],[db_type=generic])
AC_SUBST(db_type)

AC_ARG_ENABLE(transient_supervisors,
[AC_HELP_STRING([--enable-transient_supervisors], [use Erlang supervision for transient process (default: yes)])],
[case "${enableval}" in
  yes) transient_supervisors=true ;;
  no)  transient_supervisors=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-transient_supervisors) ;;
esac],[transient_supervisors=true])
AC_SUBST(transient_supervisors)

AC_ARG_ENABLE(full_xml,
[AC_HELP_STRING([--enable-full-xml], [use XML features in XMPP stream (ex: CDATA) (default: no, requires XML compliant clients)])],
[case "${enableval}" in
  yes) full_xml=true ;;
  no)  full_xml=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-full-xml) ;;
esac],[full_xml=false])
AC_SUBST(full_xml)

AC_ARG_ENABLE(nif,
[AC_HELP_STRING([--enable-nif], [replace some functions with C equivalents. Requires Erlang R13B04 or higher (default: no)])],
[case "${enableval}" in
  yes) nif=true ;;
  no)  nif=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-nif) ;;
esac],[nif=false])
AC_SUBST(nif)

AC_CONFIG_FILES([Makefile
		 rebar.config
		 src/ejabberd.app.src
		 rel/reltool.config])

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

AC_CHECK_HEADER(openssl/md2.h, md2=true, md2=false)
AC_SUBST(md2)

AC_CANONICAL_SYSTEM
#AC_DEFINE_UNQUOTED(CPU_VENDOR_OS, "$target")
#AC_SUBST(target_os)


case "$target_os" in
    *darwin10*)
	echo "Target OS is 'Darwin10'"
	AC_LANG(Erlang)
	AC_RUN_IFELSE(
	    [AC_LANG_PROGRAM([],[dnl
	     halt(case erlang:system_info(wordsize) of
	     8 -> 0; 4 -> 1 end)])],
	    [AC_MSG_NOTICE(found 64-bit Erlang)
	     CBIT=-m64],
	    [AC_MSG_NOTICE(found 32-bit Erlang)
	     CBIT=-m32])
	;;
    *)        
	echo "Target OS is '$target_os'"
	CBIT=""
	;;
esac
CFLAGS="$CFLAGS $CBIT"
LD_SHARED="$LD_SHARED $CBIT"
echo "CBIT is set to '$CBIT'"

AC_OUTPUT
