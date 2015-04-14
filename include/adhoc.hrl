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

-record(adhoc_request,
{
    lang = <<"">>      :: binary(),
    node = <<"">>      :: binary(),
    sessionid = <<"">> :: binary(),
    action = <<"">>    :: binary(),
    xdata = false      :: false | xmlel(),
    others = []        :: [xmlel()]
}).

-record(adhoc_response,
{
    lang = <<"">>          :: binary(),
    node = <<"">>          :: binary(),
    sessionid = <<"">>     :: binary(),
    status                 :: atom(),
    defaultaction = <<"">> :: binary(),
    actions       = []     :: [binary()],
    notes         = []     :: [{binary(), binary()}],
    elements      = []     :: [xmlel()]
}).

-type adhoc_request() :: #adhoc_request{}.
-type adhoc_response() :: #adhoc_response{}.
