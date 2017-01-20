%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-ifndef(EJABBERD_HRL).
-define(EJABBERD_HRL, true).

-define(VERSION, ejabberd_config:get_version()).

-define(MYHOSTS, ejabberd_config:get_myhosts()).

-define(MYNAME, hd(ejabberd_config:get_myhosts())).

-define(MYLANG, ejabberd_config:get_mylang()).

-define(MSGS_DIR, filename:join(["priv", "msgs"])).

-define(SQL_DIR, filename:join(["priv", "sql"])).

-define(CONFIG_PATH, <<"ejabberd.cfg">>).

-define(LOG_PATH, <<"ejabberd.log">>).

-define(EJABBERD_URI, <<"http://www.process-one.net/en/ejabberd/">>).

-define(COPYRIGHT, "Copyright (c) 2002-2017 ProcessOne").

%%-define(DBGFSM, true).

-record(scram,
	{storedkey = <<"">>,
         serverkey = <<"">>,
         salt = <<"">>,
         iterationcount = 0 :: integer()}).

-type scram() :: #scram{}.

-define(SCRAM_DEFAULT_ITERATION_COUNT, 4096).

-ifdef(ERL_DEPRECATED_TYPES).

-define(TDICT, dict()).
-define(TGB_TREE, gb_tree()).
-define(TGB_SET, gb_set()).
-define(TQUEUE, queue()).

-else.

-define(TDICT, dict:dict()).
-define(TGB_TREE, gb_trees:tree()).
-define(TGB_SET, gb_sets:set()).
-define(TQUEUE, queue:queue()).

-endif.

-endif.
