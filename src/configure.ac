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

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Checks for library functions.
AC_FUNC_MALLOC
AC_HEADER_STDC

AC_MOD_ENABLE(mod_pubsub, yes)
AC_MOD_ENABLE(mod_irc, yes)
AC_MOD_ENABLE(mod_muc, yes)
AC_MOD_ENABLE(eldap, yes)
AC_MOD_ENABLE(web, yes)
AC_MOD_ENABLE(tls, yes)

AC_CONFIG_FILES([Makefile
                 $make_mod_irc
                 $make_mod_muc
                 $make_mod_pubsub
                 $make_eldap
                 $make_web
                 stringprep/Makefile
                 $make_tls])
#openssl
AM_WITH_OPENSSL
AC_OUTPUT
