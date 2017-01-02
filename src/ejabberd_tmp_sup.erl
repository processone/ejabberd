%%%----------------------------------------------------------------------
%%% File    : ejabberd_tmp_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Supervisor for temporary processess
%%% Created : 18 Jul 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
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

-module(ejabberd_tmp_sup).

-author('alexey@process-one.net').

-export([start_link/2, init/1]).

start_link(Name, Module) ->
    supervisor:start_link({local, Name}, ?MODULE, Module).

init(Module) ->
    {ok,
     {{simple_one_for_one, 10, 1},
      [{undefined, {Module, start_link, []}, temporary,
	1000, worker, [Module]}]}}.
