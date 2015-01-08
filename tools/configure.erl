%%%----------------------------------------------------------------------
%%% File    : configure.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose :
%%% Created : 27 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(configure).
-author('alexey@process-one.net').

-export([start/0]).

-include("ejabberd.hrl").
-include("logger.hrl").

start() ->
    Static = case os:getenv("arg") of
		 false ->
		     false;
		 "static" ->
		     true;
		 _ ->
		     false
	     end,
    case Static of
	true ->
	    ExpatLib  = "EXPAT_LIB = $(EXPAT_DIR)\\StaticLibs\\libexpatMT.lib\n",
	    ExpatFlag = "EXPAT_FLAG = -DXML_STATIC\n",
	    IconvDir  = "ICONV_DIR = c:\\sdk\\GnuWin32\n",
	    IconvLib  = "ICONV_LIB = $(ICONV_DIR)\\lib\\libiconv.lib\n",
	    ZlibDir   = "ZLIB_DIR = c:\\sdk\\GnuWin32\n",
	    ZlibLib   = "ZLIB_LIB = $(ZLIB_DIR)\\lib\\zlib.lib\n";
	false ->
	    ExpatLib  = "EXPAT_LIB = $(EXPAT_DIR)\\Libs\\libexpat.lib\n",
	    ExpatFlag = "",
	    IconvDir  = "ICONV_DIR = c:\\sdk\\GnuWin32\n",
	    IconvLib  = "ICONV_LIB = $(ICONV_DIR)\\lib\\libiconv.lib\n",
	    ZlibDir   = "ZLIB_DIR = c:\\sdk\\GnuWin32\n",
	    ZlibLib   = "ZLIB_LIB = $(ZLIB_DIR)\\lib\\zlib.lib\n"
    end,

    EVersion = "ERLANG_VERSION = " ++ erlang:system_info(version) ++ "\n",
    EIDirS   = "EI_DIR = " ++ code:lib_dir(erl_interface) ++ "\n",
    RootDirS = "ERLANG_DIR = " ++ code:root_dir() ++ "\n",
    %% Load the ejabberd application description so that ?VERSION can read the vsn key
    application:load(ejabberd),
    Version  = "EJABBERD_VERSION = " ++ binary_to_list(?VERSION) ++ "\n",
    ExpatDir = "EXPAT_DIR = c:\\sdk\\Expat-2.0.0\n",
    OpenSSLDir = "OPENSSL_DIR = c:\\sdk\\OpenSSL\n",
    DBType = "DBTYPE = generic\n",    %% 'generic' or 'mssql'

    SSLDir    = "SSLDIR = " ++ code:lib_dir(ssl) ++ "\n",
    StdLibDir = "STDLIBDIR = " ++ code:lib_dir(stdlib) ++ "\n",

    file:write_file("Makefile.inc",
		    list_to_binary(EVersion ++
				   EIDirS ++
				   RootDirS ++
				   Version ++
				   SSLDir ++
				   StdLibDir ++
				   OpenSSLDir ++
				   DBType ++
				   ExpatDir ++
				   ExpatLib ++
				   ExpatFlag ++
				   IconvDir ++
				   IconvLib ++
				   ZlibDir ++
				   ZlibLib)),
    halt().
