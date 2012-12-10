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

-ifndef(mod_privacy_hrl).

-include("mod_privacy.hrl").

-endif.

%-define(SETS, gb_sets).
-define(SETS, ejabberd_sets).

-define(DICT, dict).

-record(state,
	{socket,
         sockmod = ejabberd_socket :: ejabberd_socket | ejabberd_frontend_socket,
         socket_monitor = make_ref() :: reference(),
         xml_socket = false :: boolean(),
         streamid = <<"">> :: binary(),
	 sasl_state :: any(),
         access :: atom(),
         shaper = none :: shaper:shaper(),
         zlib = false :: boolean(),
         tls = false :: boolean(),
	 tls_required = false :: boolean(),
         tls_enabled = false :: boolean(),
	 tls_options = [] :: list(),
         authenticated = false :: boolean() | replaced | rebinded,
         jid = #jid{} :: jid(),
	 user = <<"">> :: binary(),
         server = ?MYNAME :: binary(),
         resource = <<"">> :: binary(),
         sid = {now(), self()} :: ejabberd_sm:sid(),
	 pres_t = (?SETS):new() :: ?SETS:ej_set() | {pres_t, non_neg_integer()},
         pres_f = (?SETS):new() :: ?SETS:ej_set() | {pres_f, non_neg_integer()},
	 pres_a = (?SETS):new() :: ?SETS:ej_set() | {pres_a, non_neg_integer()},
	 pres_last :: xmlel(),
         pres_timestamp :: calendar:datetime(),
	 privacy_list = #userlist{} :: userlist(),
         conn = unknown :: atom(),
	 auth_module = unknown :: atom(),
         ip :: {inet:ip_address(), inet:port_number()},
         redirect = false :: boolean(),
	 aux_fields = [] :: [{atom(), any()}],
         fsm_limit_opts = [] :: [{atom(), any()}],
         lang = ?MYLANG :: binary(),
         debug = false :: boolean(),
         flash_hack = false :: boolean(),
	 flash_connection = false :: boolean(),
         reception = true :: boolean(),
	 standby = false :: boolean(),
         queue = queue:new() :: queue(),
         queue_len = 0 :: integer(),
	 pres_queue = gb_trees:empty() :: gb_tree(),
         keepalive_timer :: reference(),
	 keepalive_timeout :: timeout(),
         oor_timeout :: timeout(),
         oor_status = <<"">> :: binary(),
	 oor_show = <<"">> :: binary(),
         oor_notification :: xmlel(),
	 oor_send_body = all :: first_per_user | first | all | none,
         oor_send_groupchat = false :: boolean(),
	 oor_send_from = jid :: jid | username | name | none,
         oor_appid = <<"">> :: binary(),
         oor_unread = 0 :: integer(),
	 oor_unread_users = (?SETS):new() :: ?SETS:ej_set(),
         oor_unread_client = 0 :: integer(),
	 oor_offline = false :: boolean(),
         ack_enabled = false :: boolean(),
	 ack_counter = 0 :: integer(),
         ack_queue = queue:new() :: queue(),
         ack_timer :: reference()}).

-type c2s_state() :: #state{}.
