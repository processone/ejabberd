%%%----------------------------------------------------------------------
%%% File    : configure.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 27 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(configure).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0]).

-include("ejabberd.hrl").

start() ->
    EIDirS = "EI_DIR = " ++ code:lib_dir("erl_interface") ++ "\n",
    RootDirS = "ERLANG_DIR = " ++ code:root_dir() ++ "\n",
    Version = "EJABBERD_VERSION = " ++ ?VERSION ++ "\n",
    file:write_file("Makefile.inc",
		    list_to_binary(EIDirS ++ RootDirS ++ Version)),
    halt().


