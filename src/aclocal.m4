AC_DEFUN(AM_WITH_EXPAT,
[ AC_ARG_WITH(expat,
	      [  --with-expat=PREFIX	prefix where EXPAT is installed])

  EXPAT_CFLAGS=
  EXPAT_LIBS=
	if test x"$with_expat" != x; then
		EXPAT_CFLAGS="-I$with_expat/include"
		EXPAT_LIBS="-L$with_expat/lib"
	fi

	AC_CHECK_LIB(expat, XML_ParserCreate,
		     [ EXPAT_LIBS="$EXPAT_LIBS -lexpat"
		       expat_found=yes ],
		     [ expat_found=no ],
		     "$EXPAT_LIBS")
	if test $expat_found = no; then
		AC_MSG_ERROR([Could not find the Expat library])
	fi
	expat_save_CFLAGS="$CFLAGS"
	CFLAGS="$CFLAGS $EXPAT_CFLAGS"
       expat_save_CPPFLAGS="$CPPFLAGS"
       CPPFLAGS="$CPPFLAGS $EXPAT_CFLAGS"
	AC_CHECK_HEADERS(expat.h, , expat_found=no)
	if test $expat_found = no; then
		AC_MSG_ERROR([Could not find expat.h])
	fi
	CFLAGS="$expat_save_CFLAGS"
       CPPFLAGS="$expat_save_CPPFLAGS"

  AC_SUBST(EXPAT_CFLAGS)
  AC_SUBST(EXPAT_LIBS)
])

AC_DEFUN(AM_WITH_ZLIB,
[ AC_ARG_WITH(zlib,
	      [  --with-zlib=PREFIX	prefix where zlib is installed])

  ZLIB_CFLAGS=
  ZLIB_LIBS=
	if test x"$with_zlib" != x; then
		ZLIB_CFLAGS="-I$with_zlib/include"
		ZLIB_LIBS="-L$with_zlib/lib"
	fi

	AC_CHECK_LIB(z, gzgets,
		     [ ZLIB_LIBS="$ZLIB_LIBS -lz"
		       zlib_found=yes ],
		     [ zlib_found=no ],
		     "$ZLIB_LIBS")
	if test $zlib_found = no; then
		AC_MSG_ERROR([Could not find the zlib library])
	fi
	zlib_save_CFLAGS="$CFLAGS"
	CFLAGS="$CFLAGS $ZLIB_CFLAGS"
       zlib_save_CPPFLAGS="$CFLAGS"
       CPPFLAGS="$CPPFLAGS $ZLIB_CFLAGS"
	AC_CHECK_HEADERS(zlib.h, , zlib_found=no)
	if test $zlib_found = no; then
		AC_MSG_ERROR([Could not find zlib.h])
	fi
	CFLAGS="$zlib_save_CFLAGS"
       CPPFLAGS="$zlib_save_CPPFLAGS"

  AC_SUBST(ZLIB_CFLAGS)
  AC_SUBST(ZLIB_LIBS)
])

AC_DEFUN(AM_WITH_PAM,
[ AC_ARG_WITH(pam,
	      [  --with-pam=PREFIX	prefix where PAM is installed])

  PAM_CFLAGS=
  PAM_LIBS=
	if test x"$with_pam" != x; then
		PAM_CFLAGS="-I$with_pam/include"
		PAM_LIBS="-L$with_pam/lib"
	fi

	AC_CHECK_LIB(pam, pam_start,
		     [ PAM_LIBS="$PAM_LIBS -lpam"
		       pam_found=yes ],
		     [ pam_found=no ],
		     "$PAM_LIBS")
	if test $pam_found = no; then
		AC_MSG_WARN([Could not find the PAM library])
	fi
	pam_save_CFLAGS="$CFLAGS"
	CFLAGS="$CFLAGS $PAM_CFLAGS"
       pam_save_CPPFLAGS="$CPPFLAGS"
       CPPFLAGS="$CPPFLAGS $PAM_CFLAGS"
	AC_CHECK_HEADERS(security/pam_appl.h, , pam_found=no)
	if test $pam_found = no; then
		AC_MSG_WARN([Could not find security/pam_appl.h])
	fi
	CFLAGS="$pam_save_CFLAGS"
       CPPFLAGS="$pam_save_CPPFLAGS"

  AC_SUBST(PAM_CFLAGS)
  AC_SUBST(PAM_LIBS)
])

AC_DEFUN(AM_WITH_ERLANG,
[ AC_ARG_WITH(erlang,
	      [  --with-erlang=PREFIX    path to erlc and erl ])

   AC_PATH_TOOL(ERLC, erlc, , $with_erlang:$with_erlang/bin:$PATH)
   AC_PATH_TOOL(ERL, erl, , $with_erlang:$with_erlang/bin:$PATH)

   if test "z$ERLC" = "z" || test "z$ERL" = "z"; then
   		AC_MSG_ERROR([erlang not found])
   fi


   cat >>conftest.erl <<_EOF

-module(conftest).
-author('alexey@sevcom.net').

-export([[start/0]]).
-include_lib("ssl/include/ssl_pkix.hrl").

start() ->
    EIDirS = code:lib_dir("erl_interface") ++ "\n",
    EILibS =  libpath("erl_interface") ++ "\n",
    EXMPPDir = code:lib_dir("exmpp"),
    case EXMPPDir of
        {error, bad_name} -> exit("exmpp not found");
        _                 -> ok
    end,
    EXMPPDirS = EXMPPDir ++ "\n",
    RootDirS = code:root_dir() ++ "\n",
    file:write_file("conftest.out", list_to_binary(EIDirS ++ EILibS ++ ssldef() ++ EXMPPDirS ++ RootDirS)),
    halt().

-[ifdef]('id-pkix').
ssldef() -> "-DSSL39\n".
-else.
ssldef() -> "\n".
-endif.

%% return physical architecture based on OS/Processor
archname() ->
    ArchStr = erlang:system_info(system_architecture),
    case os:type() of
	{win32, _} -> "windows";
	{unix,UnixName} ->
	    Specs = string:tokens(ArchStr,"-"),
	    Cpu = case lists:nth(2,Specs) of
		      "pc" -> "x86";
		      _ -> hd(Specs)
		  end,
	    atom_to_list(UnixName) ++ "-" ++ Cpu;
	_ -> "generic"
    end.

%% Return arch-based library path or a default value if this directory
%% does not exist
libpath(App) ->
    PrivDir    = code:priv_dir(App),
    ArchDir    = archname(),
    LibArchDir = filename:join([[PrivDir,"lib",ArchDir]]),
    case file:list_dir(LibArchDir) of
	%% Arch lib dir exists: We use it
	{ok, _List}  -> LibArchDir;
	%% Arch lib dir does not exist: Return the default value
	%% ({error, enoent}):
	_Error -> code:lib_dir("erl_interface") ++ "/lib"
    end.

_EOF

   if ! $ERLC conftest.erl; then
   	   AC_MSG_ERROR([could not compile sample program])
   fi

   if ! $ERL -s conftest -noshell; then
       AC_MSG_ERROR([could not run sample program])
   fi

   if ! test -f conftest.out; then
       AC_MSG_ERROR([erlang program was not properly executed, (conftest.out was not produced)])
   fi

   # First line
   ERLANG_EI_DIR=`cat conftest.out | head -n 1`
   # Second line
   ERLANG_EI_LIB=`cat conftest.out | head -n 2 | tail -n 1`
   # Third line
   ERLANG_SSL39=`cat conftest.out | head -n 3 | tail -n 1`
   # Fourth line
   ERLANG_EXMPP=`cat conftest.out | head -n 4 | tail -n 1`
   # End line
   ERLANG_DIR=`cat conftest.out | tail -n 1`

   ERLANG_CFLAGS="-I$ERLANG_EI_DIR/include -I$ERLANG_DIR/usr/include"
   ERLANG_LIBS="-L$ERLANG_EI_LIB -lerl_interface -lei"

   AC_SUBST(ERLANG_CFLAGS)
   AC_SUBST(ERLANG_LIBS)
   AC_SUBST(ERLANG_SSL39)
   AC_SUBST(ERLANG_EXMPP)
   AC_SUBST(ERLC)
   AC_SUBST(ERL)
])


AC_DEFUN(AC_MOD_ENABLE,
[
$1=
make_$1=
AC_MSG_CHECKING([whether build $1])
AC_ARG_ENABLE($1,
  [  --enable-$1        enable $1 (default: $2)],
    [mr_enable_$1="$enableval"],
     [mr_enable_$1=$2])
if test "$mr_enable_$1" = "yes"; then
$1=$1
make_$1=$1/Makefile
fi
AC_MSG_RESULT($mr_enable_$1)
AC_SUBST($1)
AC_SUBST(make_$1)

])


dnl From Bruno Haible.

AC_DEFUN([AM_ICONV],
[
  dnl Some systems have iconv in libc, some have it in libiconv (OSF/1 and
  dnl those with the standalone portable GNU libiconv installed).
  AC_ARG_WITH([libiconv-prefix],
[  --with-libiconv-prefix=PREFIX	prefix where libiconv is installed], [
    for dir in `echo "$withval" | tr : ' '`; do
      if test -d $dir/include; then CPPFLAGS="$CPPFLAGS -I$dir/include"; fi
      if test -d $dir/include; then CFLAGS="$CFLAGS -I$dir/include"; fi
      if test -d $dir/lib; then LDFLAGS="$LDFLAGS -L$dir/lib"; fi
    done
   ])

  AC_CACHE_CHECK(for iconv, am_cv_func_iconv, [
    am_cv_func_iconv="no, consider installing GNU libiconv"
    am_cv_lib_iconv=no
    AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
     [iconv_t cd = iconv_open("","");
       iconv(cd,NULL,NULL,NULL,NULL);
       iconv_close(cd);],
       am_cv_func_iconv=yes)
    if test "$am_cv_func_iconv" != yes; then
      am_save_LIBS="$LIBS"
      LIBS="$LIBS -liconv"
      AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
        [iconv_t cd = iconv_open("","");
         iconv(cd,NULL,NULL,NULL,NULL);
         iconv_close(cd);],
        am_cv_lib_iconv=yes
        am_cv_func_iconv=yes)
      LIBS="$am_save_LIBS"
    fi
	dnl trying /usr/local
    if test "$am_cv_func_iconv" != yes; then
      am_save_LIBS="$LIBS"
	  am_save_CFLAGS="$CFLAGS"
	  am_save_LDFLAGS="$LDFLAGS"
      LIBS="$LIBS -liconv"
	  LDFLAGS="$LDFLAGS -L/usr/local/lib"
	  CFLAGS="$CFLAGS -I/usr/local/include"
      AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
        [iconv_t cd = iconv_open("","");
         iconv(cd,NULL,NULL,NULL,NULL);
         iconv_close(cd);],
        am_cv_lib_iconv=yes
        am_cv_func_iconv=yes
		CPPFLAGS="$CPPFLAGS -I/usr/local/include",
		LDFLAGS="$am_save_LDFLAGS"
		CFLAGS="$am_save_CFLAGS")
      LIBS="$am_save_LIBS"
    fi

  ])
  if test "$am_cv_func_iconv" = yes; then
    AC_DEFINE(HAVE_ICONV, 1, [Define if you have the iconv() function.])
    AC_MSG_CHECKING([for iconv declaration])
    AC_CACHE_VAL(am_cv_proto_iconv, [
      AC_TRY_COMPILE([
#include <stdlib.h>
#include <iconv.h>
extern
#ifdef __cplusplus
"C"
#endif
#if defined(__STDC__) || defined(__cplusplus)
size_t iconv (iconv_t cd, char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t *outbytesleft);
#else
size_t iconv();
#endif
], [], am_cv_proto_iconv_arg1="", am_cv_proto_iconv_arg1="const")
      am_cv_proto_iconv="extern size_t iconv (iconv_t cd, $am_cv_proto_iconv_arg1 char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t *outbytesleft);"])
    am_cv_proto_iconv=`echo "[$]am_cv_proto_iconv" | tr -s ' ' | sed -e 's/( /(/'`
    AC_MSG_RESULT([$]{ac_t:-
         }[$]am_cv_proto_iconv)
    AC_DEFINE_UNQUOTED(ICONV_CONST, $am_cv_proto_iconv_arg1,
      [Define as const if the declaration of iconv() needs const.])
  fi
  LIBICONV=
  if test "$am_cv_lib_iconv" = yes; then
    LIBICONV="-liconv"
  fi
  AC_SUBST(LIBICONV)
])

dnl <openssl>
AC_DEFUN(AM_WITH_OPENSSL,
[ AC_ARG_WITH(openssl,
          [  --with-openssl=PREFIX    prefix where OPENSSL is installed ])
unset SSL_LIBS;
unset SSL_CFLAGS;
have_openssl=no
if test x"$tls" != x; then
    for ssl_prefix in $withval /usr/local/ssl /usr/lib/ssl /usr/ssl /usr/pkg /usr/local /usr; do
        printf "looking for openssl in $ssl_prefix...\n"
        SSL_CFLAGS="-I$ssl_prefix/include"
        SSL_LIBS="-L$ssl_prefix/lib -lcrypto"
        AC_CHECK_LIB(ssl, SSL_new, [ have_openssl=yes ], [ have_openssl=no ], [ $SSL_LIBS $SSL_CFLAGS ])
        if test x"$have_openssl" = xyes; then
            save_CPPFLAGS=$CPPFLAGS
            CPPFLAGS="-I$ssl_prefix/include $CPPFLAGS"
            AC_CHECK_HEADERS(openssl/ssl.h, have_openssl_h=yes)
            CPPFLAGS=$save_CPPFLAGS
            if test x"$have_openssl_h" = xyes; then
                have_openssl=yes
                printf "openssl found in $ssl_prefix\n";
                SSL_LIBS="-L$ssl_prefix/lib -lssl -lcrypto"
                CPPFLAGS="-I$ssl_prefix/include $CPPFLAGS"
                SSL_CFLAGS="-DHAVE_SSL"
                break
            fi
	else
	    # Clear this from the autoconf cache, so in the next pass of
	    # this loop with different -L arguments, it will test again.
	    unset ac_cv_lib_ssl_SSL_new
        fi
    done
if test x${have_openssl} != xyes; then
    AC_MSG_ERROR([openssl library cannot be found. Install openssl or disable `tls' module (--disable-tls).])
fi
AC_SUBST(SSL_LIBS)
AC_SUBST(SSL_CFLAGS)
fi
])
dnl <openssl/>
