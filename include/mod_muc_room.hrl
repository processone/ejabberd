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

-define(MAX_USERS_DEFAULT, 200).

-define(SETS, gb_sets).

-define(DICT, dict).

-record(lqueue,
{
    queue :: queue(),
    len :: integer(),
    max :: integer()
}).

-type lqueue() :: #lqueue{}.

-record(config,
{
    title                                = <<"">> :: binary(),
    description                          = <<"">> :: binary(),
    allow_change_subj                    = true :: boolean(),
    allow_query_users                    = true :: boolean(),
    allow_private_messages               = true :: boolean(),
    allow_private_messages_from_visitors = anyone :: anyone | moderators | nobody ,
    allow_visitor_status                 = true :: boolean(),
    allow_visitor_nickchange             = true :: boolean(),
    public                               = true :: boolean(),
    public_list                          = true :: boolean(),
    persistent                           = false :: boolean(),
    moderated                            = true :: boolean(),
    captcha_protected                    = false :: boolean(),
    members_by_default                   = true :: boolean(),
    members_only                         = false :: boolean(),
    allow_user_invites                   = false :: boolean(),
    password_protected                   = false :: boolean(),
    password                             = <<"">> :: binary(),
    anonymous                            = true :: boolean(),
    allow_voice_requests                 = true :: boolean(),
    voice_request_min_interval           = 1800 :: non_neg_integer(),
    max_users                            = ?MAX_USERS_DEFAULT :: non_neg_integer() | none,
    logging                              = false :: boolean(),
    vcard                                = <<"">> :: boolean(),
    captcha_whitelist                    = (?SETS):empty() :: gb_set()
}).

-type config() :: #config{}.

-type role() :: moderator | participant | visitor | none.

-record(user,
{
    jid :: jid(),
    nick :: binary(),
    role :: role(),
    last_presence :: xmlel()
}).

-record(activity,
{
    message_time    = 0 :: integer(),
    presence_time   = 0 :: integer(),
    message_shaper :: shaper:shaper(),
    presence_shaper :: shaper:shaper(),
    message :: xmlel(),
    presence :: {binary(), xmlel()}
}).

-record(state,
{
    room                    = <<"">> :: binary(),
    host                    = <<"">> :: binary(),
    server_host             = <<"">> :: binary(),
    access                  = {none,none,none,none} :: {atom(), atom(), atom(), atom()},
    jid                     = #jid{} :: jid(),
    config                  = #config{} :: config(),
    users                   = (?DICT):new() :: dict(),
    last_voice_request_time = treap:empty() :: treap:treap(),
    robots                  = (?DICT):new() :: dict(),
    nicks                   = (?DICT):new() :: dict(),
    affiliations            = (?DICT):new() :: dict(),
    history                 :: lqueue(),
    subject                 = <<"">> :: binary(),
    subject_author          = <<"">> :: binary(),
    just_created            = false :: boolean(),
    activity                = treap:empty() :: treap:treap(),
    room_shaper             = none :: shaper:shaper(),
    room_queue              = queue:new() :: queue()
}).

-record(muc_online_users, {us = {<<>>, <<>>} :: {binary(), binary()},
                           resource = <<>> :: binary() | '_',
                           room = <<>> :: binary() | '_',
                           host = <<>> :: binary() | '_'}).

-type muc_online_users() :: #muc_online_users{}.

-type muc_room_state() :: #state{}.
