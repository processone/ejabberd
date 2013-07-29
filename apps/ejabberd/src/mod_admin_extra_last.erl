%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra_last.erl
%%% Author  : Badlop <badlop@process-one.net>, Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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
%%%-------------------------------------------------------------------

-module(mod_admin_extra_last).
-author('badlop@process-one.net').

-export([
    commands/0,
	 
    set_last/4,
    get_lastactivity_module/1
	]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("mod_roster.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

%%%
%%% Register commands
%%%

commands() ->
    [
     #ejabberd_commands{name = set_last, tags = [last],
			desc = "Set last activity information",
			longdesc = "Timestamp is the seconds since"
			"1970-01-01 00:00:00 UTC, for example: date +%s",
			module = ?MODULE, function = set_last,
			args = [{user, binary}, {host, binary}, {timestamp, integer}, {status, binary}],
			result = {res, rescode}}
    ].

%%%
%%% Last Activity
%%%

set_last(User, Server, Timestamp, Status) ->
    Mod = get_lastactivity_module(Server),
    Mod:store_last_info(User, Server, Timestamp, Status),
    ok.

get_lastactivity_module(Server) ->
    case lists:member(mod_last, gen_mod:loaded_modules(Server)) of
        true -> mod_last;
        _ -> mod_last_odbc
    end.
