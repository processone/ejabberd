%%%----------------------------------------------------------------------
%%% File    : ejabberd_c2s_config.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Functions for c2s interactions from other client
%%%           connector modules
%%% Created :  2 Nov 2007 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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

-module(ejabberd_c2s_config).

-author('mremond@process-one.net').

-export([get_c2s_limits/0]).

%% Get first c2s configuration limitations to apply it to other c2s
%% connectors.
get_c2s_limits() ->
    C2SFirstListen = ejabberd_option:listen(),
    case lists:keysearch(ejabberd_c2s, 2, C2SFirstListen) of
	false -> [];
	{value, {_Port, ejabberd_c2s, Opts}} ->
	    select_opts_values(Opts)
    end.

%% Only get access, shaper and max_stanza_size values
select_opts_values(Opts) ->
    maps:fold(
      fun(Opt, Val, Acc) when Opt == access;
			      Opt == shaper;
			      Opt == max_stanza_size ->
	      [{Opt, Val}|Acc];
	 (_, _, Acc) ->
	      Acc
      end, [], Opts).
