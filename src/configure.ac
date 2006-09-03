#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.53)
AC_INIT(FULL-PACKAGE-NAME, VERSION, BUG-REPORT-ADDRESS)

# Checks for programs.
AC_PROG_CC
AC_PROG_MAKE_SET

#locating erlang
AM_WITH_ERLANG
#locating iconv
AM_ICONV
#locating libexpat
AM_WITH_EXPAT
#locating zlib
AM_WITH_ZLIB

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Change default prefix
AC_PREFIX_DEFAULT /

# Checks for library functions.
AC_FUNC_MALLOC
AC_HEADER_STDC

AC_MOD_ENABLE(mod_pubsub, yes)
AC_MOD_ENABLE(mod_irc, yes)
AC_MOD_ENABLE(mod_muc, yes)
AC_MOD_ENABLE(eldap, yes)
AC_MOD_ENABLE(web, yes)
AC_MOD_ENABLE(tls, yes)
AC_MOD_ENABLE(odbc, no)
AC_MOD_ENABLE(ejabberd_zlib, yes)

AC_ARG_ENABLE(roster_gateway_workaround,
[  --enable-roster-gateway-workaround    Turn on workaround for processing gateway subscriptions (default: no)],
[case "${enableval}" in
  yes) roster_gateway_workaround=true ;;
  no)  roster_gateway_workaround=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-roster-gateway-workaround) ;;
esac],[roster_gateway_workaround=false])
AC_SUBST(roster_gateway_workaround)

AC_ARG_ENABLE(mssql,
[  --enable-mssql    Use Microsoft SQL Server database (default: no, requires --enable-odbc)],
[case "${enableval}" in
  yes) db_type=mssql ;;
  no)  db_type=generic ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-mssql) ;;
esac],[db_type=generic])
AC_SUBST(db_type)

AC_CONFIG_FILES([Makefile
                 $make_mod_irc
                 $make_mod_muc
                 $make_mod_pubsub
                 $make_eldap
                 $make_web
                 stringprep/Makefile
                 $make_tls
                 $make_odbc
                 $make_ejabberd_zlib])
#openssl
AM_WITH_OPENSSL
# If ssl is kerberized it need krb5.h
# On RedHat and OpenBSD, krb5.h is in an unsual place:
KRB5_INCLUDE="`krb5-config --cflags 2>/dev/null`"
if test -n "$KRB5_INCLUDE" ; then
        CPPFLAGS="$CPPFLAGS $KRB5_INCLUDE"
else
        #         For RedHat                                  For BSD
        for D in /usr/kerberos/include /usr/include/kerberos /usr/include/kerberosV
        do
                if test -d $D ; then
                        CPPFLAGS="$CPPFLAGS -I$D"
                fi
        done
fi
AC_CHECK_HEADER(krb5.h,,)

AC_OUTPUT
