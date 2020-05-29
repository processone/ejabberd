%%%-------------------------------------------------------------------
%%% File    : mod_block_strangers.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Block packets from non-subscribers
%%% Created : 25 Dec 2016 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2021   ProcessOne
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
%%%-------------------------------------------------------------------
-module(mod_block_strangers).

-author('alexey@process-one.net').

-behaviour(gen_mod).

%% API
-export([start/2, stop/1, reload/3, mod_doc/0,
         depends/2, mod_opt_type/1, mod_options/1]).

-export([filter_packet/1, filter_offline_msg/1, filter_subscription/2]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("translate.hrl").

-define(SETS, gb_sets).

-type c2s_state() :: ejabberd_c2s:state().

%%%===================================================================
%%% Callbacks and hooks
%%%===================================================================
start(Host, _Opts) ->
    ejabberd_hooks:add(user_receive_packet, Host,
                       ?MODULE, filter_packet, 25),
    ejabberd_hooks:add(roster_in_subscription, Host,
		       ?MODULE, filter_subscription, 25),
    ejabberd_hooks:add(offline_message_hook, Host,
		       ?MODULE, filter_offline_msg, 25).

stop(Host) ->
    ejabberd_hooks:delete(user_receive_packet, Host,
                          ?MODULE, filter_packet, 25),
    ejabberd_hooks:delete(roster_in_subscription, Host,
			  ?MODULE, filter_subscription, 25),
    ejabberd_hooks:delete(offline_message_hook, Host,
			  ?MODULE, filter_offline_msg, 25).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

-spec filter_packet({stanza(), c2s_state()}) -> {stanza(), c2s_state()} |
						{stop, {drop, c2s_state()}}.
filter_packet({#message{from = From} = Msg, State} = Acc) ->
    LFrom = jid:tolower(From),
    LBFrom = jid:remove_resource(LFrom),
    #{pres_a := PresA} = State,
    case (?SETS):is_element(LFrom, PresA)
	orelse (?SETS):is_element(LBFrom, PresA)
        orelse sets_bare_member(LBFrom, PresA) of
	false ->
	    case check_message(Msg) of
		allow -> Acc;
		deny -> {stop, {drop, State}}
	    end;
	true ->
	    Acc
    end;
filter_packet(Acc) ->
    Acc.

-spec filter_offline_msg({_, message()}) -> {_, message()} | {stop, {drop, message()}}.
filter_offline_msg({_Action, #message{} = Msg} = Acc) ->
    case check_message(Msg) of
	allow -> Acc;
	deny -> {stop, {drop, Msg}}
    end.

-spec filter_subscription(boolean(), presence()) -> boolean() | {stop, false}.
filter_subscription(Acc, #presence{meta = #{captcha := passed}}) ->
    Acc;
filter_subscription(Acc, #presence{from = From, to = To, lang = Lang,
				   id = SID, type = subscribe} = Pres) ->
    LServer = To#jid.lserver,
    case mod_block_strangers_opt:drop(LServer) andalso
	 mod_block_strangers_opt:captcha(LServer) andalso
	 need_check(Pres) of
	true ->
	    case check_subscription(From, To) of
		false ->
		    BFrom = jid:remove_resource(From),
		    BTo = jid:remove_resource(To),
		    Limiter = jid:tolower(BFrom),
		    case ejabberd_captcha:create_captcha(
			   SID, BTo, BFrom, Lang, Limiter,
			   fun(Res) -> handle_captcha_result(Res, Pres) end) of
			{ok, ID, Body, CaptchaEls} ->
			    Msg = #message{from = BTo, to = From,
					   id = ID, body = Body,
					   sub_els = CaptchaEls},
			    case mod_block_strangers_opt:log(LServer) of
				true ->
				    ?INFO_MSG("Challenge subscription request "
					      "from stranger ~ts to ~ts with "
					      "CAPTCHA",
					      [jid:encode(From), jid:encode(To)]);
				false ->
				    ok
			    end,
			    ejabberd_router:route(Msg);
			{error, limit} ->
			    ErrText = ?T("Too many CAPTCHA requests"),
			    Err = xmpp:err_resource_constraint(ErrText, Lang),
			    ejabberd_router:route_error(Pres, Err);
			_ ->
			    ErrText = ?T("Unable to generate a CAPTCHA"),
			    Err = xmpp:err_internal_server_error(ErrText, Lang),
			    ejabberd_router:route_error(Pres, Err)
		    end,
		    {stop, false};
		true ->
		    Acc
	    end;
	false ->
	    Acc
    end;
filter_subscription(Acc, _) ->
    Acc.

-spec handle_captcha_result(captcha_succeed | captcha_failed, presence()) -> ok.
handle_captcha_result(captcha_succeed, Pres) ->
    Pres1 = xmpp:put_meta(Pres, captcha, passed),
    ejabberd_router:route(Pres1);
handle_captcha_result(captcha_failed, #presence{lang = Lang} = Pres) ->
    Txt = ?T("The CAPTCHA verification has failed"),
    ejabberd_router:route_error(Pres, xmpp:err_not_allowed(Txt, Lang)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec check_message(message()) -> allow | deny.
check_message(#message{from = From, to = To, lang = Lang} = Msg) ->
    LServer = To#jid.lserver,
    case need_check(Msg) of
	true ->
	    case check_subscription(From, To) of
		false ->
		    Drop = mod_block_strangers_opt:drop(LServer),
		    Log = mod_block_strangers_opt:log(LServer),
		    if
			Log ->
			    ?INFO_MSG("~ts message from stranger ~ts to ~ts",
				      [if Drop -> "Rejecting";
					  true -> "Allow"
				       end,
				       jid:encode(From), jid:encode(To)]);
			true ->
			    ok
		    end,
		    if
			Drop ->
			    Txt = ?T("Messages from strangers are rejected"),
			    Err = xmpp:err_policy_violation(Txt, Lang),
			    Msg1 = maybe_adjust_from(Msg),
			    ejabberd_router:route_error(Msg1, Err),
			    deny;
			true ->
			    allow
		    end;
		true ->
		    allow
	    end;
	false ->
	    allow
    end.

-spec maybe_adjust_from(message()) -> message().
maybe_adjust_from(#message{type = groupchat, from = From} = Msg) ->
    Msg#message{from = jid:remove_resource(From)};
maybe_adjust_from(#message{} = Msg) ->
    Msg.

-spec need_check(presence() | message()) -> boolean().
need_check(Pkt) ->
    To = xmpp:get_to(Pkt),
    From = xmpp:get_from(Pkt),
    IsSelf = To#jid.luser == From#jid.luser andalso
	     To#jid.lserver == From#jid.lserver,
    LServer = To#jid.lserver,
    IsEmpty = case Pkt of
		  #message{body = [], subject = []} ->
		      true;
		  _ ->
		      false
	      end,
    IsError = (error == xmpp:get_type(Pkt)),
    AllowLocalUsers = mod_block_strangers_opt:allow_local_users(LServer),
    Access = mod_block_strangers_opt:access(LServer),
    not (IsSelf orelse IsEmpty orelse IsError
	 orelse acl:match_rule(LServer, Access, From) == allow
	 orelse ((AllowLocalUsers orelse From#jid.luser == <<"">>)
		 andalso ejabberd_router:is_my_host(From#jid.lserver))).

-spec check_subscription(jid(), jid()) -> boolean().
check_subscription(From, To) ->
    LocalServer = To#jid.lserver,
    {RemoteUser, RemoteServer, _} = jid:tolower(From),
    case mod_roster:is_subscribed(From, To) of
	false when RemoteUser == <<"">> ->
	    false;
	false ->
	    %% Check if the contact's server is in the roster
	    mod_block_strangers_opt:allow_transports(LocalServer)
		andalso mod_roster:is_subscribed(jid:make(RemoteServer), To);
	true ->
	    true
    end.

-spec sets_bare_member(ljid(), ?SETS:set()) -> boolean().
sets_bare_member({U, S, <<"">>} = LBJID, Set) ->
    case ?SETS:next(?SETS:iterator_from(LBJID, Set)) of
        {{U, S, _}, _} -> true;
        _ -> false
    end.

depends(_Host, _Opts) ->
    [].

mod_opt_type(access) ->
    econf:acl();
mod_opt_type(drop) ->
    econf:bool();
mod_opt_type(log) ->
    econf:bool();
mod_opt_type(captcha) ->
    econf:bool();
mod_opt_type(allow_local_users) ->
    econf:bool();
mod_opt_type(allow_transports) ->
    econf:bool().

mod_options(_) ->
    [{access, none},
     {drop, true},
     {log, false},
     {captcha, false},
     {allow_local_users, true},
     {allow_transports, true}].

mod_doc() ->
    #{desc =>
          ?T("This module allows to block/log messages coming from an "
             "unknown entity. If a writing entity is not in your roster, "
             "you can let this module drop and/or log the message. "
             "By default you'll just not receive message from that entity. "
             "Enable this module if you want to drop SPAM messages."),
      opts =>
          [{access,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("The option is supposed to be used when 'allow_local_users' "
                     "and 'allow_transports' are not enough. It's an ACL where "
                     "'deny' means the message will be rejected (or a CAPTCHA "
                     "would be generated for a presence, if configured), and "
                     "'allow' means the sender is whitelisted and the stanza "
                     "will pass through. The default value is 'none', which "
                     "means nothing is whitelisted.")}},
           {drop,
            #{value => "true | false",
              desc =>
                  ?T("This option specifies if strangers messages should "
                     "be dropped or not. The default value is 'true'.")}},
           {log,
            #{value => "true | false",
              desc =>
                  ?T("This option specifies if strangers' messages should "
                     "be logged (as info message) in ejabberd.log. "
                     "The default value is 'false'.")}},
           {allow_local_users,
            #{value => "true | false",
              desc =>
                  ?T("This option specifies if strangers from the same "
                     "local host should be accepted or not. "
                     "The default value is 'true'.")}},
           {allow_transports,
            #{value => "true | false",
              desc =>
                  ?T("If set to 'true' and some server's JID is in user's "
                     "roster, then messages from any user of this server "
                     "are accepted even if no subscription present. "
                     "The default value is 'true'.")}},
           {captcha,
            #{value => "true | false",
              desc =>
                  ?T("Whether to generate CAPTCHA or not in response to "
                     "messages from strangers. See also section "
                     "https://docs.ejabberd.im/admin/configuration/#captcha"
                     "[CAPTCHA] of the Configuration Guide. "
                     "The default value is 'false'.")}}]}.
