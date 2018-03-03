%%%-------------------------------------------------------------------
%%% File    : mod_block_strangers.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Block packets from non-subscribers
%%% Created : 25 Dec 2016 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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
-export([start/2, stop/1, reload/3,
         depends/2, mod_opt_type/1, mod_options/1]).

-export([filter_packet/1, filter_offline_msg/1, filter_subscription/2]).

-include("xmpp.hrl").
-include("ejabberd.hrl").
-include("logger.hrl").

-define(SETS, gb_sets).

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

filter_offline_msg({_Action, #message{} = Msg} = Acc) ->
    case check_message(Msg) of
	allow -> Acc;
	deny -> {stop, {drop, Msg}}
    end.

filter_subscription(Acc, #presence{meta = #{captcha := passed}}) ->
    Acc;
filter_subscription(Acc, #presence{from = From, to = To, lang = Lang,
				   id = SID, type = subscribe} = Pres) ->
    LServer = To#jid.lserver,
    case gen_mod:get_module_opt(LServer, ?MODULE, drop) andalso
	 gen_mod:get_module_opt(LServer, ?MODULE, captcha) andalso
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
			    case gen_mod:get_module_opt(LServer, ?MODULE, log) of
				true ->
				    ?INFO_MSG("Challenge subscription request "
					      "from stranger ~s to ~s with "
					      "CAPTCHA",
					      [jid:encode(From), jid:encode(To)]);
				false ->
				    ok
			    end,
			    ejabberd_router:route(Msg);
			{error, limit} ->
			    ErrText = <<"Too many CAPTCHA requests">>,
			    Err = xmpp:err_resource_constraint(ErrText, Lang),
			    ejabberd_router:route_error(Pres, Err);
			_ ->
			    ErrText = <<"Unable to generate a CAPTCHA">>,
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

handle_captcha_result(captcha_succeed, Pres) ->
    Pres1 = xmpp:put_meta(Pres, captcha, passed),
    ejabberd_router:route(Pres1);
handle_captcha_result(captcha_failed, #presence{lang = Lang} = Pres) ->
    Txt = <<"The CAPTCHA verification has failed">>,
    ejabberd_router:route_error(Pres, xmpp:err_not_allowed(Txt, Lang)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_message(#message{from = From, to = To, lang = Lang} = Msg) ->
    LServer = To#jid.lserver,
    case need_check(Msg) of
	true ->
	    case check_subscription(From, To) of
		false ->
		    Drop = gen_mod:get_module_opt(LServer, ?MODULE, drop),
		    Log = gen_mod:get_module_opt(LServer, ?MODULE, log),
		    if
			Log ->
			    ?INFO_MSG("~s message from stranger ~s to ~s",
				      [if Drop -> "Rejecting";
					  true -> "Allow"
				       end,
				       jid:encode(From), jid:encode(To)]);
			true ->
			    ok
		    end,
		    if
			Drop ->
			    Txt = <<"Messages from strangers are rejected">>,
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
    LServer = To#jid.lserver,
    IsEmpty = case Pkt of
		  #message{body = [], subject = []} ->
		      true;
		  _ ->
		      false
	      end,
    AllowLocalUsers = gen_mod:get_module_opt(LServer, ?MODULE, allow_local_users),
    Access = gen_mod:get_module_opt(LServer, ?MODULE, access),
    not (IsEmpty orelse acl:match_rule(LServer, Access, From) == allow
	 orelse ((AllowLocalUsers orelse From#jid.luser == <<"">>)
		 andalso ejabberd_router:is_my_host(From#jid.lserver))).

-spec check_subscription(jid(), jid()) -> boolean().
check_subscription(From, To) ->
    {LocalUser, LocalServer, _} = jid:tolower(To),
    {RemoteUser, RemoteServer, _} = jid:tolower(From),
    case has_subscription(LocalUser, LocalServer, From) of
	false when RemoteUser == <<"">> ->
	    false;
	false ->
	    %% Check if the contact's server is in the roster
	    gen_mod:get_module_opt(LocalServer, ?MODULE, allow_transports)
		andalso has_subscription(LocalUser, LocalServer,
					 jid:make(RemoteServer));
	true ->
	    true
    end.

-spec has_subscription(binary(), binary(), jid()) -> boolean().
has_subscription(User, Server, JID) ->
    {Sub, Ask, _} = ejabberd_hooks:run_fold(
		      roster_get_jid_info, Server,
		      {none, none, []},
		      [User, Server, JID]),
    (Sub /= none) orelse (Ask == subscribe)
	orelse (Ask == out) orelse (Ask == both).

sets_bare_member({U, S, <<"">>} = LBJID, Set) ->
    case ?SETS:next(sets_iterator_from(LBJID, Set)) of
        {{U, S, _}, _} -> true;
        _ -> false
    end.

-ifdef(GB_SETS_ITERATOR_FROM).
sets_iterator_from(Element, Set) ->
    ?SETS:iterator_from(Element, Set).
-else.
%% Copied from gb_sets.erl
%% TODO: Remove after dropping R17 support
sets_iterator_from(S, {_, T}) ->
    iterator_from(S, T, []).

iterator_from(S, {K, _, T}, As) when K < S ->
    iterator_from(S, T, As);
iterator_from(_, {_, nil, _} = T, As) ->
    [T | As];
iterator_from(S, {_, L, _} = T, As) ->
    iterator_from(S, L, [T | As]);
iterator_from(_, nil, As) ->
    As.
-endif.


depends(_Host, _Opts) ->
    [].

mod_opt_type(drop) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(log) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(allow_local_users) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(allow_transports) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(captcha) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(access) ->
    fun acl:access_rules_validator/1.


mod_options(_) ->
    [{access, none},
     {drop, true},
     {log, false},
     {captcha, false},
     {allow_local_users, true},
     {allow_transports, true}].
