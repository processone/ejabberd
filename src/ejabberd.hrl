%%%----------------------------------------------------------------------
%%% File    : ejabberd.hrl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 17 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-define(VERSION, "0.1-alpha").

%-define(ejabberd_debug, true).
%-define(DBGFSM, true).

-ifdef(ejabberd_debug).
-define(DEBUG(Format, Args), io:format("D(~p:~p:~p) : "++Format++"~n",
                                       [self(),?MODULE,?LINE]++Args)).
-else.
-define(DEBUG(F,A),[]).
-endif.

-define(ERROR_MSG(Format, Args),
	error_logger:format("E(~p:~p:~p): "++Format++"~n",
			    [self(),?MODULE,?LINE]++Args)).


%-define(MYNAME,"e.localhost").
-define(MYNAME, ejabberd_config:get_global_option(host)).
-define(S2STIMEOUT, 600000).
%-define(S2STIMEOUT, 6000).

-define(MSGS_DIR, "msgs").
-define(CONFIG_PATH, "ejabberd.cfg").
-define(ERROR_LOG_PATH, "error.log").

