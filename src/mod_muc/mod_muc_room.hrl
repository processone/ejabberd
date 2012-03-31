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

-define(MAX_USERS_DEFAULT, 200).

-define(SETS, gb_sets).
-define(DICT, dict).

-record(lqueue, {queue, len, max}).

-record(config, {title = "",
		 description = "",
		 allow_change_subj = true,
		 allow_query_users = true,
		 allow_private_messages = true,
		 allow_private_messages_from_visitors = anyone,
		 allow_visitor_status = true,
		 allow_visitor_nickchange = true,
		 public = true,
		 public_list = true,
		 persistent = false,
		 moderated = true,
		 captcha_protected = false,
		 members_by_default = true,
		 members_only = false,
		 allow_user_invites = false,
		 password_protected = false,
		 password = "",
		 anonymous = true,
		 allow_voice_requests = true,
		 voice_request_min_interval = 1800,
		 max_users = ?MAX_USERS_DEFAULT,
		 logging = false,
                 captcha_whitelist = ?SETS:empty()
		}).

-record(user, {jid,
	       nick,
	       role,
	       last_presence}).

-record(activity, {message_time = 0,
		   presence_time = 0,
		   message_shaper,
		   presence_shaper,
		   message,
		   presence}).

-record(state, {room,
		host,
		server_host,
                mod,
		access,
		jid,
		config = #config{},
		users = ?DICT:new(),
		last_voice_request_time = treap:empty(),
		robots = ?DICT:new(),
		nicks = ?DICT:new(),
		affiliations = ?DICT:new(),
		history,
		subject = "",
		subject_author = "",
		just_created = false,
		activity = treap:empty(),
		room_shaper,
		room_queue = queue:new()}).

-record(muc_online_users, {us,
			   resource,
			   room,
			   host}).
