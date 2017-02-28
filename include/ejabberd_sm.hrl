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

-ifndef(EJABBERD_SM_HRL).
-define(EJABBERD_SM_HRL, true).

-record(session, {sid, usr, us, priority, info = []}).
-record(session_counter, {vhost, count}).
-type sid() :: {erlang:timestamp(), pid()}.
-type ip() :: {inet:ip_address(), inet:port_number()} | undefined.
-type info() :: [{conn, atom()} | {ip, ip()} | {node, atom()}
                 | {oor, boolean()} | {auth_module, atom()}
                 | {num_stanzas_in, non_neg_integer()}
		 | offline].
-type prio() :: undefined | integer().

-endif.
