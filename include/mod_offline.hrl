%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

-record(offline_msg,
	{us = {<<"">>, <<"">>} :: {binary(), binary()},
	 timestamp             :: erlang:timestamp() | '_' | undefined,
	 expire                :: erlang:timestamp() | never | undefined | '_',
	 from = #jid{}         :: jid() | '_',
	 to = #jid{}           :: jid() | '_',
	 packet = #xmlel{}     :: xmlel() | message() | '_'}).

-record(state,
	{host = <<"">> :: binary(),
	 access_max_offline_messages}).
