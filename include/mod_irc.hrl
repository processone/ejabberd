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

-type conn_param() :: {binary(), binary(), inet:port_number(), binary()} |
                      {binary(), binary(), inet:port_number()} |
                      {binary(), binary()} |
                      {binary()}.

-type irc_data() :: [{username, binary()} | {connections_params, [conn_param()]}].

-record(irc_connection,
        {jid_server_host = {#jid{}, <<"">>, <<"">>} :: {jid(), binary(), binary()},
         pid = self()                               :: pid()}).

-record(irc_custom,
        {us_host = {{<<"">>, <<"">>}, <<"">>} :: {{binary(), binary()},
                                                  binary()},
         data = [] :: irc_data()}).
