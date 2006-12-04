%%%----------------------------------------------------------------------
%%% File    : ejabberd.hrl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 17 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------
%-define(ejabberd_debug, true).
%-define(DBGFSM, true).

-define(VERSION, "1.1.2").

%% ---------------------------------
%% Logging mechanism

-define(DEBUG(Format, Args),
    ejabberd_logger:debug_msg(?MODULE,?LINE,Format, Args)).

-define(INFO_MSG(Format, Args),
    ejabberd_logger:info_msg(?MODULE,?LINE,Format, Args)).
			      
-define(WARNING_MSG(Format, Args),
    ejabberd_logger:warning_msg(?MODULE,?LINE,Format, Args)).
			      
-define(ERROR_MSG(Format, Args),
    ejabberd_logger:error_msg(?MODULE,?LINE,Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    ejabberd_logger:critical_msg(?MODULE,?LINE,Format, Args)).

-define(MYHOSTS, ejabberd_config:get_global_option(hosts)).
-define(MYNAME, hd(ejabberd_config:get_global_option(hosts))).
-define(S2STIMEOUT, 600000).
-define(MYLANG, ejabberd_config:get_global_option(language)).

-define(MSGS_DIR, "msgs").
-define(CONFIG_PATH, "ejabberd.cfg").
-define(LOG_PATH, "ejabberd.log").

-define(PRIVACY_SUPPORT, true).

-define(EJABBERD_URI, "http://ejabberd.jabber.ru").
