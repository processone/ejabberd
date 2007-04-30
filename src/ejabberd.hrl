%%%----------------------------------------------------------------------
%%% File    : ejabberd.hrl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 17 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-define(VERSION, "1.1.3").

%-define(ejabberd_debug, true).
%-define(DBGFSM, true).

-ifdef(ejabberd_debug).
-define(DEBUG(Format, Args), io:format("D(~p:~p:~p) : "++Format++"~n",
                                       [self(),?MODULE,?LINE]++Args)).
-else.
-define(DEBUG(F,A),[]).
-endif.

-define(ERROR_MSG(Format, Args),
	error_logger:error_msg("E(~p:~p:~p): "++Format++"~n",
			       [self(),?MODULE,?LINE]++Args)).

-define(INFO_MSG(Format, Args),
	error_logger:info_msg("I(~p:~p:~p): "++Format++"~n",
			      [self(),?MODULE,?LINE]++Args)).


-define(MYHOSTS, ejabberd_config:get_global_option(hosts)).
-define(MYNAME, hd(ejabberd_config:get_global_option(hosts))).
-define(S2STIMEOUT, 600000).
-define(MYLANG, ejabberd_config:get_global_option(language)).

-define(MSGS_DIR, "msgs").
-define(CONFIG_PATH, "ejabberd.cfg").
-define(LOG_PATH, "ejabberd.log").


-define(PRIVACY_SUPPORT, true).

