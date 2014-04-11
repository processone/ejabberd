%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
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

-record(privacy, {us = {<<"">>, <<"">>} :: {binary(), binary()},
                  default = none        :: none | binary(),
                  lists = []            :: [{binary(), [listitem()]}]}).

-record(listitem, {type = none :: none | jid | group | subscription,
                   value = none :: none | both | from | to | ljid() | binary(),
                   action = allow :: allow | deny,
                   order = 0 :: integer(),
                   match_all = false :: boolean(),
                   match_iq = false :: boolean(),
                   match_message = false :: boolean(),
                   match_presence_in = false :: boolean(),
                   match_presence_out = false :: boolean()}).

-type listitem() :: #listitem{}.

-record(userlist, {name = none :: none | binary(),
                   list = [] :: [listitem()],
                   needdb = false :: boolean()}).

-type userlist() :: #userlist{}.

-export_type([userlist/0]).
