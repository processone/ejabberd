%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

%% This macro returns a string of the ejabberd version running, e.g. "2.3.4"
%% If the ejabberd application description isn't loaded, returns atom: undefined
-define(VERSION, element(2, application:get_key(ejabberd,vsn))).

-define(MYHOSTS, ejabberd_config:get_global_option(hosts)).
-define(MYNAME,  hd(ejabberd_config:get_global_option(hosts))).
-define(MYLANG,  ejabberd_config:get_global_option(language)).

-define(MSGS_DIR,    "msgs").
-define(CONFIG_PATH, "etc/ejabberd.cfg").
-define(LOG_PATH,    "log/ejabberd.log").

-define(EJABBERD_URI, "http://www.process-one.net/en/ejabberd/").

-define(S2STIMEOUT, 600000).

%%-define(DBGFSM, true).

%% ---------------------------------
%% Logging mechanism

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

-define(DEBUG(Format, Args),
    lager:debug(Format, Args)).

-define(INFO_MSG(Format, Args),
    lager:info(Format, Args)).

-define(WARNING_MSG(Format, Args),
    lager:warning(Format, Args)).

-define(ERROR_MSG(Format, Args),
    lager:error(Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    lager:critical(Format, Args)).

-record(session, {sid :: tuple(),
                  usr :: {binary(), binary(), binary()},
                  us :: {binary(), binary()},
                  priority :: integer(),
                  info :: list()}).
