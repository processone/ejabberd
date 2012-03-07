%%%----------------------------------------------------------------------
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

-record(privacy_list, {user_host, name}).
-record(privacy_default_list, {user_host, name}).
-record(privacy_list_data, {user_host, name,
			    type, value, action, order,
			    match_all, match_iq, match_message,
			    match_presence_in, match_presence_out}).

%% ejabberd 2 format:
-record(privacy, {user_host,
		  default = none,
		  lists = []}).

%% ejabberd 2 format:
-record(listitem, {type = none,
		   value = none,
		   action,
		   order,
		   match_all = false,
		   match_iq = false,
		   match_message = false,
		   match_presence_in = false,
		   match_presence_out = false
		  }).

-record(userlist, {name = none, list = [], needdb = false }).

