%%%----------------------------------------------------------------------
%%% File    : ejabberd.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd wrapper: start / stop
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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

-module(ejabberd).
-author('alexey@process-one.net').

-export([start/0, stop/0,
	 get_pid_file/0,
	 get_so_path/0, get_bin_path/0]).

start() ->
    %%ejabberd_cover:start(),
    application:start(ejabberd).

stop() ->
    application:stop(ejabberd).
    %%ejabberd_cover:stop().

get_so_path() ->
    case os:getenv("EJABBERD_SO_PATH") of
	false ->
	    case code:priv_dir(ejabberd) of
		{error, _} ->
		    ".";
		Path ->
		    filename:join([Path, "lib"])
	    end;
	Path ->
	    Path
    end.

get_bin_path() ->
    case os:getenv("EJABBERD_BIN_PATH") of
	false ->
	    case code:priv_dir(ejabberd) of
		{error, _} ->
		    ".";
		Path ->
		    filename:join([Path, "bin"])
	    end;
	Path ->
	    Path
    end.

%% @spec () -> false | string()
get_pid_file() ->
    case os:getenv("EJABBERD_PID_PATH") of
	false ->
	    false;
	"" ->
	    false;
	Path ->
	    Path
    end.
