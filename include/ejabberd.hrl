%%%----------------------------------------------------------------------
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

%% This macro returns a string of the ejabberd version running, e.g. "2.3.4"
%% If the ejabberd application description isn't loaded, returns atom: undefined
-define(VERSION, ejabberd_config:get_version()).

-define(MYHOSTS, ejabberd_config:get_myhosts()).

-define(MYNAME, hd(ejabberd_config:get_myhosts())).

-define(MYLANG, ejabberd_config:get_mylang()).

-define(MSGS_DIR, filename:join(["priv", "msgs"])).

-define(CONFIG_PATH, <<"ejabberd.cfg">>).

-define(LOG_PATH, <<"ejabberd.log">>).

-define(EJABBERD_URI, <<"http://www.process-one.net/en/ejabberd/">>).

-define(S2STIMEOUT, 600000).

%%-define(DBGFSM, true).

-record(scram,
	{storedkey = <<"">>,
         serverkey = <<"">>,
         salt = <<"">>,
         iterationcount = 0 :: integer()}).

-type scram() :: #scram{}.

-define(SCRAM_DEFAULT_ITERATION_COUNT, 4096).
