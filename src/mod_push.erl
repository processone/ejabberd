%%%----------------------------------------------------------------------
%%% File    : mod_push.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Push Notifications (XEP-0357)
%%% Created : 15 Jul 2017 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2017-2024 ProcessOne
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

-module(mod_push).
-author('holger@zedat.fu-berlin.de').
-protocol({xep, 357, '0.2', '17.08', "", ""}).

-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, mod_opt_type/1, mod_options/1, depends/2]).
-export([mod_doc/0]).
%% ejabberd_hooks callbacks.
-export([disco_sm_features/5, c2s_session_pending/1, c2s_copy_session/2,
	 c2s_session_resumed/1, c2s_handle_cast/2, c2s_stanza/3, mam_message/7,
	 offline_message/1, remove_user/2]).

%% gen_iq_handler callback.
-export([process_iq/1]).

%% ejabberd command.
-export([get_commands_spec/0, delete_old_sessions/1]).

%% API (used by mod_push_keepalive).
-export([notify/3, notify/5, notify/7, is_incoming_chat_msg/1]).

%% For IQ callbacks
-export([delete_session/3]).

-include("ejabberd_commands.hrl").
-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

-define(PUSH_CACHE, push_cache).

-type c2s_state() :: ejabberd_c2s:state().
-type push_session_id() :: erlang:timestamp().
-type push_session() :: {push_session_id(), ljid(), binary(), xdata()}.
-type err_reason() :: notfound | db_failure.
-type direction() :: send | recv | undefined.

-callback init(binary(), gen_mod:opts())
	  -> any().
-callback store_session(binary(), binary(), push_session_id(), jid(), binary(),
			xdata())
	  -> {ok, push_session()} | {error, err_reason()}.
-callback lookup_session(binary(), binary(), jid(), binary())
	  -> {ok, push_session()} | {error, err_reason()}.
-callback lookup_session(binary(), binary(), push_session_id())
	  -> {ok, push_session()} | {error, err_reason()}.
-callback lookup_sessions(binary(), binary(), jid())
	  -> {ok, [push_session()]} | {error, err_reason()}.
-callback lookup_sessions(binary(), binary())
	  -> {ok, [push_session()]} | {error, err_reason()}.
-callback lookup_sessions(binary())
	  -> {ok, [push_session()]} | {error, err_reason()}.
-callback delete_session(binary(), binary(), push_session_id())
	  -> ok | {error, err_reason()}.
-callback delete_old_sessions(binary() | global, erlang:timestamp())
	  -> ok | {error, err_reason()}.
-callback use_cache(binary())
	  -> boolean().
-callback cache_nodes(binary())
	  -> [node()].

-optional_callbacks([use_cache/1, cache_nodes/1]).

%%--------------------------------------------------------------------
%% gen_mod callbacks.
%%--------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> {ok, [gen_mod:registration()]}.
start(Host, Opts) ->
    Mod = gen_mod:db_mod(Opts, ?MODULE),
    Mod:init(Host, Opts),
    init_cache(Mod, Host, Opts),
    ejabberd_commands:register_commands(?MODULE, get_commands_spec()),
    {ok, [{iq_handler, ejabberd_sm,  ?NS_PUSH_0, process_iq},
          {hook, disco_sm_features, disco_sm_features, 50},
          {hook, c2s_session_pending, c2s_session_pending, 50},
          {hook, c2s_copy_session, c2s_copy_session, 50},
          {hook, c2s_session_resumed, c2s_session_resumed, 50},
          {hook, c2s_handle_cast, c2s_handle_cast, 50},
          {hook, c2s_handle_send, c2s_stanza, 50},
          {hook, store_mam_message, mam_message, 50},
          {hook, offline_message_hook, offline_message, 55},
          {hook, remove_user, remove_user, 50}]}.


-spec stop(binary()) -> ok.
stop(Host) ->
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
        false ->
            ejabberd_commands:unregister_commands(get_commands_spec());
        true ->
            ok
    end.

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(OldOpts, ?MODULE),
    if NewMod /= OldMod ->
	    NewMod:init(Host, NewOpts);
       true ->
	    ok
    end,
    init_cache(NewMod, Host, NewOpts),
    ok.

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [].

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(notify_on) ->
    econf:enum([messages, all]);
mod_opt_type(include_sender) ->
    econf:bool();
mod_opt_type(include_body) ->
    econf:either(
      econf:bool(),
      econf:binary());
mod_opt_type(db_type) ->
    econf:db_type(?MODULE);
mod_opt_type(use_cache) ->
    econf:bool();
mod_opt_type(cache_size) ->
    econf:pos_int(infinity);
mod_opt_type(cache_missed) ->
    econf:bool();
mod_opt_type(cache_life_time) ->
    econf:timeout(second, infinity).

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(Host) ->
    [{notify_on, all},
     {include_sender, false},
     {include_body, <<"New message">>},
     {db_type, ejabberd_config:default_db(Host, ?MODULE)},
     {use_cache, ejabberd_option:use_cache(Host)},
     {cache_size, ejabberd_option:cache_size(Host)},
     {cache_missed, ejabberd_option:cache_missed(Host)},
     {cache_life_time, ejabberd_option:cache_life_time(Host)}].

mod_doc() ->
    #{desc =>
          ?T("This module implements the XMPP server's part of "
             "the push notification solution specified in "
             "https://xmpp.org/extensions/xep-0357.html"
             "[XEP-0357: Push Notifications]. It does not generate, "
             "for example, APNS or FCM notifications directly. "
             "Instead, it's designed to work with so-called "
             "\"app servers\" operated by third-party vendors of "
             "mobile apps. Those app servers will usually trigger "
             "notification delivery to the user's mobile device using "
             "platform-dependant backend services such as FCM or APNS."),
      opts =>
          [{notify_on,
            #{value => "messages | all",
              note => "added in 23.10",
              desc =>
                  ?T("If this option is set to 'messages', notifications are "
                     "generated only for actual chat messages with a body text "
                     "(or some encrypted payload). If it's set to 'all', any "
                     "kind of XMPP stanza will trigger a notification. If "
                     "unsure, it's strongly recommended to stick to 'all', "
                     "which is the default value.")}},
           {include_sender,
            #{value => "true | false",
              desc =>
                  ?T("If this option is set to 'true', the sender's JID "
                     "is included with push notifications generated for "
                     "incoming messages with a body. "
                     "The default value is 'false'.")}},
           {include_body,
            #{value => "true | false | Text",
              desc =>
                  ?T("If this option is set to 'true', the message text "
                     "is included with push notifications generated for "
                     "incoming messages with a body. The option can instead "
                     "be set to a static 'Text', in which case the specified "
                     "text will be included in place of the actual message "
                     "body. This can be useful to signal the app server "
                     "whether the notification was triggered by a message "
                     "with body (as opposed to other types of traffic) "
                     "without leaking actual message contents. "
                     "The default value is \"New message\".")}},
           {db_type,
            #{value => "mnesia | sql",
              desc =>
                  ?T("Same as top-level _`default_db`_ option, but applied to this module only.")}},
           {use_cache,
            #{value => "true | false",
              desc =>
                  ?T("Same as top-level _`use_cache`_ option, but applied to this module only.")}},
           {cache_size,
            #{value => "pos_integer() | infinity",
              desc =>
                  ?T("Same as top-level _`cache_size`_ option, but applied to this module only.")}},
           {cache_missed,
            #{value => "true | false",
              desc =>
                  ?T("Same as top-level _`cache_missed`_ option, but applied to this module only.")}},
           {cache_life_time,
            #{value => "timeout()",
              desc =>
                  ?T("Same as top-level _`cache_life_time`_ option, but applied to this module only.")}}]}.

%%--------------------------------------------------------------------
%% ejabberd command callback.
%%--------------------------------------------------------------------
-spec get_commands_spec() -> [ejabberd_commands()].
get_commands_spec() ->
    [#ejabberd_commands{name = delete_old_push_sessions, tags = [purge],
			desc = "Remove push sessions older than DAYS",
			module = ?MODULE, function = delete_old_sessions,
			args = [{days, integer}],
			result = {res, rescode}}].

-spec delete_old_sessions(non_neg_integer()) -> ok | any().
delete_old_sessions(Days) ->
    CurrentTime = erlang:system_time(microsecond),
    Diff = Days * 24 * 60 * 60 * 1000000,
    TimeStamp = misc:usec_to_now(CurrentTime - Diff),
    DBTypes = lists:usort(
		lists:map(
		  fun(Host) ->
			  case mod_push_opt:db_type(Host) of
			      sql -> {sql, Host};
			      Other -> {Other, global}
			  end
		  end, ejabberd_option:hosts())),
    Results = lists:map(
		fun({DBType, Host}) ->
			Mod = gen_mod:db_mod(DBType, ?MODULE),
			Mod:delete_old_sessions(Host, TimeStamp)
		end, DBTypes),
    ets_cache:clear(?PUSH_CACHE, ejabberd_cluster:get_nodes()),
    case lists:filter(fun(Res) -> Res /= ok end, Results) of
	[] ->
	    ?INFO_MSG("Deleted push sessions older than ~B days", [Days]),
	    ok;
	[{error, Reason} | _] ->
	    ?ERROR_MSG("Error while deleting old push sessions: ~p", [Reason]),
	    Reason
    end.

%%--------------------------------------------------------------------
%% Service discovery.
%%--------------------------------------------------------------------
-spec disco_sm_features(empty | {result, [binary()]} | {error, stanza_error()},
			jid(), jid(), binary(), binary())
      -> {result, [binary()]} | {error, stanza_error()}.
disco_sm_features(empty, From, To, Node, Lang) ->
    disco_sm_features({result, []}, From, To, Node, Lang);
disco_sm_features({result, OtherFeatures},
		  #jid{luser = U, lserver = S},
		  #jid{luser = U, lserver = S}, <<"">>, _Lang) ->
    {result, [?NS_PUSH_0 | OtherFeatures]};
disco_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%%--------------------------------------------------------------------
%% IQ handlers.
%%--------------------------------------------------------------------
-spec process_iq(iq()) -> iq().
process_iq(#iq{type = get, lang = Lang} = IQ) ->
    Txt = ?T("Value 'get' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_iq(#iq{lang = Lang, sub_els = [#push_enable{node = <<>>}]} = IQ) ->
    Txt = ?T("Enabling push without 'node' attribute is not supported"),
    xmpp:make_error(IQ, xmpp:err_feature_not_implemented(Txt, Lang));
process_iq(#iq{from = #jid{lserver = LServer} = JID,
	       to = #jid{lserver = LServer},
	       lang = Lang,
	       sub_els = [#push_enable{jid = PushJID,
				       node = Node,
				       xdata = XData}]} = IQ) ->
    case enable(JID, PushJID, Node, XData) of
	ok ->
	    xmpp:make_iq_result(IQ);
	{error, db_failure} ->
	    Txt = ?T("Database failure"),
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang));
	{error, notfound} ->
	    Txt = ?T("User session not found"),
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang))
    end;
process_iq(#iq{from = #jid{lserver = LServer} = JID,
	       to = #jid{lserver = LServer},
	       lang = Lang,
	       sub_els = [#push_disable{jid = PushJID,
					node = Node}]} = IQ) ->
    case disable(JID, PushJID, Node) of
	ok ->
	    xmpp:make_iq_result(IQ);
	{error, db_failure} ->
	    Txt = ?T("Database failure"),
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang));
	{error, notfound} ->
	    Txt = ?T("Push record not found"),
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang))
    end;
process_iq(IQ) ->
    xmpp:make_error(IQ, xmpp:err_not_allowed()).

-spec enable(jid(), jid(), binary(), xdata()) -> ok | {error, err_reason()}.
enable(#jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
       PushJID, Node, XData) ->
    case ejabberd_sm:get_session_sid(LUser, LServer, LResource) of
	{ID, PID} ->
	    case store_session(LUser, LServer, ID, PushJID, Node, XData) of
		{ok, _} ->
		    ?INFO_MSG("Enabling push notifications for ~ts",
			      [jid:encode(JID)]),
		    ejabberd_c2s:cast(PID, {push_enable, ID}),
		    ejabberd_sm:set_user_info(LUser, LServer, LResource,
					      push_id, ID);
		{error, _} = Err ->
		    ?ERROR_MSG("Cannot enable push for ~ts: database error",
			       [jid:encode(JID)]),
		    Err
	    end;
	none ->
	    ?WARNING_MSG("Cannot enable push for ~ts: session not found",
			 [jid:encode(JID)]),
	    {error, notfound}
    end.

-spec disable(jid(), jid(), binary() | undefined) -> ok | {error, err_reason()}.
disable(#jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
       PushJID, Node) ->
    case ejabberd_sm:get_session_pid(LUser, LServer, LResource) of
	PID when is_pid(PID) ->
	    ?INFO_MSG("Disabling push notifications for ~ts",
		      [jid:encode(JID)]),
	    ejabberd_sm:del_user_info(LUser, LServer, LResource, push_id),
	    ejabberd_c2s:cast(PID, push_disable);
	none ->
	    ?WARNING_MSG("Session not found while disabling push for ~ts",
			 [jid:encode(JID)])
    end,
    if Node /= <<>> ->
	   delete_session(LUser, LServer, PushJID, Node);
       true ->
	   delete_sessions(LUser, LServer, PushJID)
    end.

%%--------------------------------------------------------------------
%% Hook callbacks.
%%--------------------------------------------------------------------
-spec c2s_stanza(c2s_state(), xmpp_element() | xmlel(), term()) -> c2s_state().
c2s_stanza(State, #stream_error{}, _SendResult) ->
    State;
c2s_stanza(#{push_enabled := true, mgmt_state := pending} = State,
	   Pkt, _SendResult) ->
    ?DEBUG("Notifying client of stanza", []),
    notify(State, Pkt, get_direction(Pkt)),
    State;
c2s_stanza(State, _Pkt, _SendResult) ->
    State.

-spec mam_message(message() | drop, binary(), binary(), jid(),
		  binary(), chat | groupchat, recv | send) -> message().
mam_message(#message{} = Pkt, LUser, LServer, _Peer, _Nick, chat, Dir) ->
    case lookup_sessions(LUser, LServer) of
	{ok, [_|_] = Clients} ->
	    case drop_online_sessions(LUser, LServer, Clients) of
		[_|_] = Clients1 ->
		    ?DEBUG("Notifying ~ts@~ts of MAM message", [LUser, LServer]),
		    notify(LUser, LServer, Clients1, Pkt, Dir);
		[] ->
		    ok
	    end;
	_ ->
	    ok
    end,
    Pkt;
mam_message(Pkt, _LUser, _LServer, _Peer, _Nick, _Type, _Dir) ->
    Pkt.

-spec offline_message({any(), message()}) -> {any(), message()}.
offline_message({offlined, #message{meta = #{mam_archived := true}}} = Acc) ->
    Acc; % Push notification was triggered via MAM.
offline_message({offlined,
		 #message{to = #jid{luser = LUser,
				    lserver = LServer}} = Pkt} = Acc) ->
    case lookup_sessions(LUser, LServer) of
	{ok, [_|_] = Clients} ->
	    ?DEBUG("Notifying ~ts@~ts of offline message", [LUser, LServer]),
	    notify(LUser, LServer, Clients, Pkt, recv);
	_ ->
	    ok
    end,
    Acc;
offline_message(Acc) ->
    Acc.

-spec c2s_session_pending(c2s_state()) -> c2s_state().
c2s_session_pending(#{push_enabled := true, mgmt_queue := Queue} = State) ->
    case p1_queue:len(Queue) of
	Len when Len > 0 ->
	    ?DEBUG("Notifying client of unacknowledged stanza(s)", []),
	    {Pkt, Dir} = case mod_stream_mgmt:queue_find(
				fun is_incoming_chat_msg/1, Queue) of
			     none -> {none, undefined};
			     Pkt0 -> {Pkt0, get_direction(Pkt0)}
			 end,
	    notify(State, Pkt, Dir),
	    State;
	0 ->
	    State
    end;
c2s_session_pending(State) ->
    State.

-spec c2s_copy_session(c2s_state(), c2s_state()) -> c2s_state().
c2s_copy_session(State, #{push_enabled := true,
			  push_session_id := ID}) ->
    State#{push_enabled => true,
	   push_session_id => ID};
c2s_copy_session(State, _) ->
    State.

-spec c2s_session_resumed(c2s_state()) -> c2s_state().
c2s_session_resumed(#{push_session_id := ID,
		      user := U, server := S, resource := R} = State) ->
    ejabberd_sm:set_user_info(U, S, R, push_id, ID),
    State;
c2s_session_resumed(State) ->
    State.

-spec c2s_handle_cast(c2s_state(), any()) -> c2s_state() | {stop, c2s_state()}.
c2s_handle_cast(State, {push_enable, ID}) ->
    {stop, State#{push_enabled => true,
		  push_session_id => ID}};
c2s_handle_cast(State, push_disable) ->
    State1 = maps:remove(push_enabled, State),
    State2 = maps:remove(push_session_id, State1),
    {stop, State2};
c2s_handle_cast(State, _Msg) ->
    State.

-spec remove_user(binary(), binary()) -> ok | {error, err_reason()}.
remove_user(LUser, LServer) ->
    ?INFO_MSG("Removing any push sessions of ~ts@~ts", [LUser, LServer]),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    LookupFun = fun() -> Mod:lookup_sessions(LUser, LServer) end,
    delete_sessions(LUser, LServer, LookupFun, Mod).

%%--------------------------------------------------------------------
%% Generate push notifications.
%%--------------------------------------------------------------------
-spec notify(c2s_state(), xmpp_element() | xmlel() | none, direction()) -> ok.
notify(#{jid := #jid{luser = LUser, lserver = LServer}} = State, Pkt, Dir) ->
    case lookup_session(LUser, LServer, State) of
	{ok, Client} ->
	    notify(LUser, LServer, [Client], Pkt, Dir);
	_Err ->
	    ok
    end.

-spec notify(binary(), binary(), [push_session()],
	     xmpp_element() | xmlel() | none, direction()) -> ok.
notify(LUser, LServer, Clients, Pkt, Dir) ->
    lists:foreach(
      fun({ID, PushLJID, Node, XData}) ->
	      HandleResponse =
	          fun(#iq{type = result}) ->
			  ?DEBUG("~ts accepted notification for ~ts@~ts (~ts)",
				 [jid:encode(PushLJID), LUser, LServer, Node]);
		     (#iq{type = error} = IQ) ->
			  case inspect_error(IQ) of
			      {wait, Reason} ->
				  ?INFO_MSG("~ts rejected notification for "
					    "~ts@~ts (~ts) temporarily: ~ts",
					    [jid:encode(PushLJID), LUser,
					     LServer, Node, Reason]);
			      {Type, Reason} ->
				  spawn(?MODULE, delete_session,
					[LUser, LServer, ID]),
				  ?WARNING_MSG("~ts rejected notification for "
					       "~ts@~ts (~ts), disabling push: ~ts "
					       "(~ts)",
					       [jid:encode(PushLJID), LUser,
						LServer, Node, Reason, Type])
			  end;
		     (timeout) ->
			  ?DEBUG("Timeout sending notification for ~ts@~ts (~ts) "
				 "to ~ts",
				 [LUser, LServer, Node, jid:encode(PushLJID)]),
			  ok % Hmm.
		  end,
	      notify(LServer, PushLJID, Node, XData, Pkt, Dir, HandleResponse)
      end, Clients).

-spec notify(binary(), ljid(), binary(), xdata(),
	     xmpp_element() | xmlel() | none, direction(),
	     fun((iq() | timeout) -> any())) -> ok.
notify(LServer, PushLJID, Node, XData, Pkt0, Dir, HandleResponse) ->
    Pkt = unwrap_message(Pkt0),
    From = jid:make(LServer),
    case {make_summary(LServer, Pkt, Dir), mod_push_opt:notify_on(LServer)} of
	{undefined, messages} ->
	    ?DEBUG("Suppressing notification for stanza without payload", []),
	    ok;
	{Summary, _NotifyOn} ->
	    Item = #ps_item{sub_els = [#push_notification{xdata = Summary}]},
	    PubSub = #pubsub{publish = #ps_publish{node = Node, items = [Item]},
			     publish_options = XData},
	    IQ = #iq{type = set,
		     from = From,
		     to = jid:make(PushLJID),
		     id = p1_rand:get_string(),
		     sub_els = [PubSub]},
	    ejabberd_router:route_iq(IQ, HandleResponse)
    end.

%%--------------------------------------------------------------------
%% Miscellaneous.
%%--------------------------------------------------------------------
-spec is_incoming_chat_msg(stanza()) -> boolean().
is_incoming_chat_msg(#message{} = Msg) ->
    case get_direction(Msg) of
	recv -> get_body_text(unwrap_message(Msg)) /= none;
	send -> false
    end;
is_incoming_chat_msg(_Stanza) ->
    false.

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec store_session(binary(), binary(), push_session_id(), jid(), binary(),
		    xdata()) -> {ok, push_session()} | {error, err_reason()}.
store_session(LUser, LServer, ID, PushJID, Node, XData) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    delete_session(LUser, LServer, PushJID, Node),
    case use_cache(Mod, LServer) of
	true ->
	    ets_cache:delete(?PUSH_CACHE, {LUser, LServer},
			     cache_nodes(Mod, LServer)),
	    ets_cache:update(
		?PUSH_CACHE,
		{LUser, LServer, ID}, {ok, {ID, PushJID, Node, XData}},
		fun() ->
			Mod:store_session(LUser, LServer, ID, PushJID, Node,
					  XData)
		end, cache_nodes(Mod, LServer));
	false ->
	    Mod:store_session(LUser, LServer, ID, PushJID, Node, XData)
    end.

-spec lookup_session(binary(), binary(), c2s_state())
      -> {ok, push_session()} | error | {error, err_reason()}.
lookup_session(LUser, LServer, #{push_session_id := ID}) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case use_cache(Mod, LServer) of
	true ->
	    ets_cache:lookup(
	      ?PUSH_CACHE, {LUser, LServer, ID},
	      fun() -> Mod:lookup_session(LUser, LServer, ID) end);
	false ->
	    Mod:lookup_session(LUser, LServer, ID)
    end.

-spec lookup_sessions(binary(), binary()) -> {ok, [push_session()]} | {error, err_reason()}.
lookup_sessions(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case use_cache(Mod, LServer) of
	true ->
	    ets_cache:lookup(
	      ?PUSH_CACHE, {LUser, LServer},
	      fun() -> Mod:lookup_sessions(LUser, LServer) end);
	false ->
	    Mod:lookup_sessions(LUser, LServer)
    end.

-spec delete_session(binary(), binary(), push_session_id())
      -> ok | {error, db_failure}.
delete_session(LUser, LServer, ID) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:delete_session(LUser, LServer, ID) of
	ok ->
	    case use_cache(Mod, LServer) of
		true ->
		    ets_cache:delete(?PUSH_CACHE, {LUser, LServer},
				     cache_nodes(Mod, LServer)),
		    ets_cache:delete(?PUSH_CACHE, {LUser, LServer, ID},
				     cache_nodes(Mod, LServer));
		false ->
		    ok
	    end;
	{error, _} = Err ->
	    Err
    end.

-spec delete_session(binary(), binary(), jid(), binary()) -> ok | {error, err_reason()}.
delete_session(LUser, LServer, PushJID, Node) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:lookup_session(LUser, LServer, PushJID, Node) of
	{ok, {ID, _, _, _}} ->
	    delete_session(LUser, LServer, ID);
	error ->
	    {error, notfound};
	{error, _} = Err ->
	    Err
    end.

-spec delete_sessions(binary(), binary(), jid()) -> ok | {error, err_reason()}.
delete_sessions(LUser, LServer, PushJID) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    LookupFun = fun() -> Mod:lookup_sessions(LUser, LServer, PushJID) end,
    delete_sessions(LUser, LServer, LookupFun, Mod).

-spec delete_sessions(binary(), binary(), fun(() -> any()), module())
      -> ok | {error, err_reason()}.
delete_sessions(LUser, LServer, LookupFun, Mod) ->
    case LookupFun() of
	{ok, []} ->
	    {error, notfound};
	{ok, Clients} ->
	    case use_cache(Mod, LServer) of
		true ->
		    ets_cache:delete(?PUSH_CACHE, {LUser, LServer},
				     cache_nodes(Mod, LServer));
		false ->
		    ok
	    end,
	    lists:foreach(
	      fun({ID, _, _, _}) ->
		      ok = Mod:delete_session(LUser, LServer, ID),
		      case use_cache(Mod, LServer) of
			  true ->
			      ets_cache:delete(?PUSH_CACHE,
					       {LUser, LServer, ID},
					       cache_nodes(Mod, LServer));
			  false ->
			      ok
		      end
	      end, Clients);
	{error, _} = Err ->
	    Err
    end.

-spec drop_online_sessions(binary(), binary(), [push_session()])
      -> [push_session()].
drop_online_sessions(LUser, LServer, Clients) ->
    OnlineIDs = lists:filtermap(
		  fun({_, Info}) ->
			  case proplists:get_value(push_id, Info) of
			      OnlineID = {_, _, _} ->
				  {true, OnlineID};
			      undefined ->
				  false
			  end
		  end, ejabberd_sm:get_user_info(LUser, LServer)),
    [Client || {ID, _, _, _} = Client <- Clients,
	       not lists:member(ID, OnlineIDs)].

-spec make_summary(binary(), xmpp_element() | xmlel() | none, direction())
      -> xdata() | undefined.
make_summary(Host, #message{from = From0} = Pkt, recv) ->
    case {mod_push_opt:include_sender(Host),
	  mod_push_opt:include_body(Host)} of
	{false, false} ->
	    undefined;
	{IncludeSender, IncludeBody} ->
	    case get_body_text(Pkt) of
		none ->
		    undefined;
		Text ->
		    Fields1 = case IncludeBody of
				  StaticText when is_binary(StaticText) ->
				      [{'last-message-body', StaticText}];
				  true ->
				      [{'last-message-body', Text}];
				  false ->
				      []
			      end,
		    Fields2 = case IncludeSender of
				  true ->
				      From = jid:remove_resource(From0),
				      [{'last-message-sender', From} | Fields1];
				  false ->
				      Fields1
			      end,
		    #xdata{type = submit, fields = push_summary:encode(Fields2)}
	    end
    end;
make_summary(_Host, _Pkt, _Dir) ->
    undefined.

-spec unwrap_message(Stanza) -> Stanza when Stanza :: stanza() | none.
unwrap_message(#message{meta = #{carbon_copy := true}} = Msg) ->
    misc:unwrap_carbon(Msg);
unwrap_message(#message{type = normal} = Msg) ->
    case misc:unwrap_mucsub_message(Msg) of
	#message{} = InnerMsg ->
	    InnerMsg;
	false ->
	    Msg
    end;
unwrap_message(Stanza) ->
    Stanza.

-spec get_direction(stanza()) -> direction().
get_direction(#message{meta = #{carbon_copy := true},
		       from = #jid{luser = U, lserver = S},
		       to = #jid{luser = U, lserver = S}}) ->
    send;
get_direction(#message{}) ->
    recv;
get_direction(_Stanza) ->
    undefined.

-spec get_body_text(message()) -> binary() | none.
get_body_text(#message{body = Body} = Msg) ->
    case xmpp:get_text(Body) of
	Text when byte_size(Text) > 0 ->
	    Text;
	<<>> ->
	    case body_is_encrypted(Msg) of
		true ->
		    <<"(encrypted)">>;
		false ->
		    none
	    end
    end.

-spec body_is_encrypted(message()) -> boolean().
body_is_encrypted(#message{sub_els = MsgEls}) ->
    case lists:keyfind(<<"encrypted">>, #xmlel.name, MsgEls) of
	#xmlel{children = EncEls} ->
	    lists:keyfind(<<"payload">>, #xmlel.name, EncEls) /= false;
	false ->
	    false
    end.

-spec inspect_error(iq()) -> {atom(), binary()}.
inspect_error(IQ) ->
    case xmpp:get_error(IQ) of
	#stanza_error{type = Type} = Err ->
	    {Type, xmpp:format_stanza_error(Err)};
	undefined ->
	    {undefined, <<"unrecognized error">>}
    end.

%%--------------------------------------------------------------------
%% Caching.
%%--------------------------------------------------------------------
-spec init_cache(module(), binary(), gen_mod:opts()) -> ok.
init_cache(Mod, Host, Opts) ->
    case use_cache(Mod, Host) of
	true ->
	    CacheOpts = cache_opts(Opts),
	    ets_cache:new(?PUSH_CACHE, CacheOpts);
	false ->
	    ets_cache:delete(?PUSH_CACHE)
    end.

-spec cache_opts(gen_mod:opts()) -> [proplists:property()].
cache_opts(Opts) ->
    MaxSize = mod_push_opt:cache_size(Opts),
    CacheMissed = mod_push_opt:cache_missed(Opts),
    LifeTime = mod_push_opt:cache_life_time(Opts),
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec use_cache(module(), binary()) -> boolean().
use_cache(Mod, Host) ->
    case erlang:function_exported(Mod, use_cache, 1) of
	true -> Mod:use_cache(Host);
	false -> mod_push_opt:use_cache(Host)
    end.

-spec cache_nodes(module(), binary()) -> [node()].
cache_nodes(Mod, Host) ->
    case erlang:function_exported(Mod, cache_nodes, 1) of
	true -> Mod:cache_nodes(Host);
	false -> ejabberd_cluster:get_nodes()
    end.
