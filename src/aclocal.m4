AC_DEFUN(AM_WITH_EXPAT,
[ AC_ARG_WITH(expat,
	      [AC_HELP_STRING([--with-expat=PREFIX], [prefix where EXPAT is installed])])

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
		AC_MSG_ERROR([Could not find development files of Expat library])
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
	      [AC_HELP_STRING([--with-zlib=PREFIX], [prefix where zlib is installed])])

if test x"$ejabberd_zlib" != x; then
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
		AC_MSG_ERROR([Could not find development files of zlib library. Install them or disable `ejabberd_zlib' with: --disable-ejabberd_zlib])
	fi
	zlib_save_CFLAGS="$CFLAGS"
	CFLAGS="$CFLAGS $ZLIB_CFLAGS"
       zlib_save_CPPFLAGS="$CFLAGS"
       CPPFLAGS="$CPPFLAGS $ZLIB_CFLAGS"
	AC_CHECK_HEADERS(zlib.h, , zlib_found=no)
	if test $zlib_found = no; then
		AC_MSG_ERROR([Could not find zlib.h. Install it or disable `ejabberd_zlib' with: --disable-ejabberd_zlib])
	fi
	CFLAGS="$zlib_save_CFLAGS"
       CPPFLAGS="$zlib_save_CPPFLAGS"

  AC_SUBST(ZLIB_CFLAGS)
  AC_SUBST(ZLIB_LIBS)
fi
])

AC_DEFUN(AM_WITH_PAM,
[ AC_ARG_WITH(pam,
	      [AC_HELP_STRING([--with-pam=PREFIX], [prefix where PAM is installed])])
if test x"$pam" != x; then
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
		AC_MSG_ERROR([Could not find development files of PAM library. Install them or disable `pam' with: --disable-pam])
	fi
	pam_save_CFLAGS="$CFLAGS"
	CFLAGS="$CFLAGS $PAM_CFLAGS"
       pam_save_CPPFLAGS="$CPPFLAGS"
       CPPFLAGS="$CPPFLAGS $PAM_CFLAGS"
	AC_CHECK_HEADERS(security/pam_appl.h, , pam_found=no)
	if test $pam_found = no; then
		AC_MSG_ERROR([Could not find security/pam_appl.h. Install it or disable `pam' with: --disable-pam])
	fi
	CFLAGS="$pam_save_CFLAGS"
       CPPFLAGS="$pam_save_CPPFLAGS"

  AC_SUBST(PAM_CFLAGS)
  AC_SUBST(PAM_LIBS)
fi
])

AC_DEFUN(AM_WITH_ERLANG,
[ AC_ARG_WITH(erlang,
	      [AC_HELP_STRING([--with-erlang=PREFIX], [path to erlc and erl])])

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
  [AC_HELP_STRING([--enable-$1], [enable $1 (default: $2)])],
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

dnl <openssl>
AC_DEFUN(AM_WITH_OPENSSL,
[ AC_ARG_WITH(openssl,
      [AC_HELP_STRING([--with-openssl=PREFIX], [prefix where OPENSSL is installed])])
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
    AC_MSG_ERROR([Could not find development files of OpenSSL library. Install them or disable `tls' with: --disable-tls])
fi
AC_SUBST(SSL_LIBS)
AC_SUBST(SSL_CFLAGS)
fi
])
dnl <openssl/>
