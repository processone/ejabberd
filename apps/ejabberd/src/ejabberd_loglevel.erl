%%%----------------------------------------------------------------------
%%% File    : ejabberd_loglevel.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Loglevel switcher.
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne, Erlang Solutions Ltd.
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

-module(ejabberd_loglevel).
-author('piotr.nosek@erlang-solutions.com').

-export([set/1,
	 get/0,
	 set_custom/2,
	 clear_custom/0,
	 clear_custom/1
	 ]).

-include("ejabberd.hrl").

-define(LOG_LEVELS,
	[{0, log_none},
	 {1, critical},
	 {2, error},
	 {3, warning},
	 {4, info},
	 {5, debug}]).

-spec get() -> {integer(), atom()}.
get() ->
    Name = lager:get_loglevel(lager_console_backend),
    {value, Res} = lists:keysearch(Name, 2, ?LOG_LEVELS),
    Res.

set(Level) when is_integer(Level) ->
    {value, {_, Name}} = lists:keysearch(Level, 1, ?LOG_LEVELS),
    set(Name);
set(Level) ->
    ok = lager:set_loglevel(lager_console_backend, Level),
    ok = lager:set_loglevel(lager_file_backend, "log/ejabberd.log", Level).

set_custom(Module, Level) ->
    ok = lager:set_mod_loglevel(lager_console_backend, Level, Module),
    ok = lager:set_mod_loglevel(lager_file_backend, "log/ejabberd.log", Level, Module).
    
clear_custom() ->
    clear_custom('_').

clear_custom(Module) when is_atom(Module) ->
    ok = lager:clear_mod_loglevel(lager_console_backend, Module),
    ok = lager:clear_mod_loglevel(lager_file_backend, "log/ejabberd.log", Module).
