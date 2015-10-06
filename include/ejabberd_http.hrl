%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-record(request,
	{method            :: method(),
	 path = []         :: [binary()],
	 q = []            :: [{binary() | nokey, binary()}],
	 us = {<<>>, <<>>} :: {binary(), binary()},
	 auth              :: {binary(), binary()} |
	 {auth_jid, {binary(), binary()}, jlib:jid()},
	 lang = <<"">>     :: binary(),
	 data = <<"">>     :: binary(),
	 ip                :: {inet:ip_address(), inet:port_number()},
	 host = <<"">>     :: binary(),
	 port = 5280       :: inet:port_number(),
	 opts = []         :: list(),
	 tp = http         :: protocol(),
	 headers = []      :: [{atom() | binary(), binary()}]}).

-record(ws,
	{socket                  :: inet:socket() | p1_tls:tls_socket(),
	 sockmod = gen_tcp       :: gen_tcp | p1_tls,
	 ip                      :: {inet:ip_address(), inet:port_number()},
	 host = <<"">>           :: binary(),
	 port = 5280             :: inet:port_number(),
	 path = []               :: [binary()],
	 headers = []            :: [{atom() | binary(), binary()}],
	 local_path = []         :: [binary()],
	 q = []                  :: [{binary() | nokey, binary()}],
	 buf                     :: binary(),
         http_opts = []          :: list()}).

-type method() :: 'GET' | 'HEAD' | 'DELETE' | 'OPTIONS' | 'PUT' | 'POST' | 'TRACE'.
-type protocol() :: http | https.
-type http_request() :: #request{}.
