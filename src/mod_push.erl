%%%----------------------------------------------------------------------
%%% File    : mod_push.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Push Notifications (XEP-0357)
%%% Created : 15 Jul 2017 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2017-2018 ProcessOne
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
-protocol({xep, 357, '0.2'}).

-behavior(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, mod_opt_type/1, depends/2]).

%% ejabberd_hooks callbacks.
-export([disco_sm_features/5, c2s_session_pending/1, c2s_copy_session/2,
	 c2s_handle_cast/2, c2s_stanza/3, mam_message/6, offline_message/1,
	 remove_user/2]).

%% gen_iq_handler callback.
-export([process_iq/1]).

%% ejabberd command.
-export([get_commands_spec/0, delete_old_sessions/1]).

%% API (used by mod_push_keepalive).
-export([notify/1, notify/3, notify/5]).

%% For IQ callbacks
-export([delete_session/3]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-define(PUSH_CACHE, push_cache).

-type c2s_state() :: ejabberd_c2s:state().
-type timestamp() :: erlang:timestamp().
-type push_session() :: {timestamp(), ljid(), binary(), xdata()}.
-type err_reason() :: notfound | db_failure.

-callback init(binary(), gen_mod:opts())
	  -> any().
-callback store_session(binary(), binary(), timestamp(), jid(), binary(),
			xdata())
	  -> {ok, push_session()} | {error, err_reason()}.
-callback lookup_session(binary(), binary(), jid(), binary())
	  -> {ok, push_session()} | {error, err_reason()}.
-callback lookup_session(binary(), binary(), timestamp())
	  -> {ok, push_session()} | {error, err_reason()}.
-callback lookup_sessions(binary(), binary(), jid())
	  -> {ok, [push_session()]} | {error, err_reason()}.
-callback lookup_sessions(binary(), binary())
	  -> {ok, [push_session()]} | {error, err_reason()}.
-callback lookup_sessions(binary())
	  -> {ok, [push_session()]} | {error, err_reason()}.
-callback delete_session(binary(), binary(), timestamp())
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
-spec start(binary(), gen_mod:opts()) -> ok.
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, gen_iq_handler:iqdisc(Host)),
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, Opts),
    init_cache(Mod, Host, Opts),
    register_iq_handlers(Host, IQDisc),
    register_hooks(Host),
    ejabberd_commands:register_commands(get_commands_spec()).

-spec stop(binary()) -> ok.
stop(Host) ->
    unregister_hooks(Host),
    unregister_iq_handlers(Host),
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
        false ->
            ejabberd_commands:unregister_commands(get_commands_spec());
        true ->
            ok
    end.

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(Host, NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(Host, OldOpts, ?MODULE),
    if NewMod /= OldMod ->
	    NewMod:init(Host, NewOpts);
       true ->
	    ok
    end,
    case gen_mod:is_equal_opt(iqdisc, NewOpts, OldOpts,
			      gen_iq_handler:iqdisc(Host)) of
	{false, IQDisc, _} ->
	    register_iq_handlers(Host, IQDisc);
	true ->
	    ok
    end.

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [].

-spec mod_opt_type(atom()) -> fun((term()) -> term()) | [atom()].
mod_opt_type(db_type) ->
    fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(O) when O == cache_life_time; O == cache_size ->
    fun(I) when is_integer(I), I > 0 -> I;
       (infinity) -> infinity
    end;
mod_opt_type(O) when O == use_cache; O == cache_missed ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(iqdisc) ->
    fun gen_iq_handler:check_type/1;
mod_opt_type(_) ->
    [db_type, cache_life_time, cache_size, use_cache, cache_missed, iqdisc].

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
    CurrentTime = p1_time_compat:system_time(micro_seconds),
    Diff = Days * 24 * 60 * 60 * 1000000,
    TimeStamp = misc:usec_to_now(CurrentTime - Diff),
    DBTypes = lists:usort(
		lists:map(
		  fun(Host) ->
			  case gen_mod:db_type(Host, ?MODULE) of
			      sql -> {sql, Host};
			      Other -> {Other, global}
			  end
		  end, ?MYHOSTS)),
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
	[NotOk | _] ->
	    ?ERROR_MSG("Error while deleting old push sessions: ~p", [NotOk]),
	    NotOk
    end.

%%--------------------------------------------------------------------
%% Register/unregister hooks.
%%--------------------------------------------------------------------
-spec register_hooks(binary()) -> ok.
register_hooks(Host) ->
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE,
		       disco_sm_features, 50),
    ejabberd_hooks:add(c2s_session_pending, Host, ?MODULE,
		       c2s_session_pending, 50),
    ejabberd_hooks:add(c2s_copy_session, Host, ?MODULE,
		       c2s_copy_session, 50),
    ejabberd_hooks:add(c2s_handle_cast, Host, ?MODULE,
		       c2s_handle_cast, 50),
    ejabberd_hooks:add(c2s_handle_send, Host, ?MODULE,
		       c2s_stanza, 50),
    ejabberd_hooks:add(store_mam_message, Host, ?MODULE,
		       mam_message, 50),
    ejabberd_hooks:add(store_offline_message, Host, ?MODULE,
		       offline_message, 50),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50).

-spec unregister_hooks(binary()) -> ok.
unregister_hooks(Host) ->
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE,
			  disco_sm_features, 50),
    ejabberd_hooks:delete(c2s_session_pending, Host, ?MODULE,
			  c2s_session_pending, 50),
    ejabberd_hooks:delete(c2s_copy_session, Host, ?MODULE,
			  c2s_copy_session, 50),
    ejabberd_hooks:delete(c2s_handle_cast, Host, ?MODULE,
			  c2s_handle_cast, 50),
    ejabberd_hooks:delete(c2s_handle_send, Host, ?MODULE,
			  c2s_stanza, 50),
    ejabberd_hooks:delete(store_mam_message, Host, ?MODULE,
			  mam_message, 50),
    ejabberd_hooks:delete(store_offline_message, Host, ?MODULE,
			  offline_message, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50).

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
-spec register_iq_handlers(binary(), gen_iq_handler:type()) -> ok.
register_iq_handlers(Host, IQDisc) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PUSH_0,
				  ?MODULE, process_iq, IQDisc).

-spec unregister_iq_handlers(binary()) -> ok.
unregister_iq_handlers(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PUSH_0).

-spec process_iq(iq()) -> iq().
process_iq(#iq{type = get, lang = Lang} = IQ) ->
    Txt = <<"Value 'get' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_iq(#iq{lang = Lang, sub_els = [#push_enable{node = <<>>}]} = IQ) ->
    Txt = <<"Enabling push without 'node' attribute is not supported">>,
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
	    Txt = <<"Database failure">>,
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang));
	{error, notfound} ->
	    Txt = <<"User session not found">>,
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
	    Txt = <<"Database failure">>,
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang));
	{error, notfound} ->
	    Txt = <<"Push record not found">>,
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang))
    end;
process_iq(IQ) ->
    xmpp:make_error(IQ, xmpp:err_not_allowed()).

-spec enable(jid(), jid(), binary(), xdata()) -> ok | {error, err_reason()}.
enable(#jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
       PushJID, Node, XData) ->
    case ejabberd_sm:get_session_sid(LUser, LServer, LResource) of
	{TS, PID} ->
	    case store_session(LUser, LServer, TS, PushJID, Node, XData) of
		{ok, _} ->
		    ?INFO_MSG("Enabling push notifications for ~s",
			      [jid:encode(JID)]),
		    ejabberd_c2s:cast(PID, push_enable);
		{error, _} = Err ->
		    ?ERROR_MSG("Cannot enable push for ~s: database error",
			       [jid:encode(JID)]),
		    Err
	    end;
	none ->
	    ?WARNING_MSG("Cannot enable push for ~s: session not found",
			 [jid:encode(JID)]),
	    {error, notfound}
    end.

-spec disable(jid(), jid(), binary() | undefined) -> ok | {error, err_reason()}.
disable(#jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
       PushJID, Node) ->
    case ejabberd_sm:get_session_sid(LUser, LServer, LResource) of
	{_TS, PID} ->
	    ?INFO_MSG("Disabling push notifications for ~s",
		      [jid:encode(JID)]),
	    ejabberd_c2s:cast(PID, push_disable);
	none ->
	    ?WARNING_MSG("Session not found while disabling push for ~s",
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
c2s_stanza(#{push_enabled := true, mgmt_state := pending} = State,
	   _Pkt, _SendResult) ->
    notify(State),
    State;
c2s_stanza(State, _Pkt, _SendResult) ->
    State.

-spec mam_message(message() | drop, binary(), binary(), jid(),
		  chat | groupchat, recv | send) -> message().
mam_message(#message{} = Pkt, LUser, LServer, _Peer, chat, _Dir) ->
    case lookup_sessions(LUser, LServer) of
	{ok, [_|_] = Clients} ->
	    case drop_online_sessions(LUser, LServer, Clients) of
		[_|_] = Clients1 ->
		    ?DEBUG("Notifying ~s@~s of MAM message", [LUser, LServer]),
		    notify(LUser, LServer, Clients1);
		[] ->
		    ok
	    end;
	_ ->
	    ok
    end,
    Pkt;
mam_message(Pkt, _LUser, _LServer, _Peer, _Type, _Dir) ->
    Pkt.

-spec offline_message(message()) -> message().
offline_message(#message{meta = #{mam_archived := true}} = Pkt) ->
    Pkt; % Push notification was triggered via MAM.
offline_message(#message{to = #jid{luser = LUser, lserver = LServer}} = Pkt) ->
    case lookup_sessions(LUser, LServer) of
	{ok, [_|_] = Clients} ->
	    ?DEBUG("Notifying ~s@~s of offline message", [LUser, LServer]),
	    notify(LUser, LServer, Clients);
	_ ->
	    ok
    end,
    Pkt.

-spec c2s_session_pending(c2s_state()) -> c2s_state().
c2s_session_pending(#{push_enabled := true, mgmt_queue := Queue} = State) ->
    case p1_queue:len(Queue) of
	Len when Len > 0 ->
	    ?DEBUG("Notifying client of unacknowledged messages", []),
	    notify(State),
	    State;
	0 ->
	    State
    end;
c2s_session_pending(State) ->
    State.

-spec c2s_copy_session(c2s_state(), c2s_state()) -> c2s_state().
c2s_copy_session(State, #{push_enabled := true}) ->
    State#{push_enabled => true};
c2s_copy_session(State, _) ->
    State.

-spec c2s_handle_cast(c2s_state(), any()) -> c2s_state() | {stop, c2s_state()}.
c2s_handle_cast(State, push_enable) ->
    {stop, State#{push_enabled => true}};
c2s_handle_cast(State, push_disable) ->
    {stop, maps:remove(push_enabled, State)};
c2s_handle_cast(State, _Msg) ->
    State.

-spec remove_user(binary(), binary()) -> ok | {error, err_reason()}.
remove_user(LUser, LServer) ->
    ?INFO_MSG("Removing any push sessions of ~s@~s", [LUser, LServer]),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    LookupFun = fun() -> Mod:lookup_sessions(LUser, LServer) end,
    delete_sessions(LUser, LServer, LookupFun, Mod).

%%--------------------------------------------------------------------
%% Generate push notifications.
%%--------------------------------------------------------------------
-spec notify(c2s_state()) -> ok.
notify(#{jid := #jid{luser = LUser, lserver = LServer}, sid := {TS, _}}) ->
    case lookup_session(LUser, LServer, TS) of
	{ok, Client} ->
	    notify(LUser, LServer, [Client]);
	_Err ->
	    ok
    end.

-spec notify(binary(), binary(), [push_session()]) -> ok.
notify(LUser, LServer, Clients) ->
    lists:foreach(
      fun({TS, PushLJID, Node, XData}) ->
	      HandleResponse = fun(#iq{type = result}) ->
				       ok;
				  (#iq{type = error}) ->
				       spawn(?MODULE, delete_session,
					     [LUser, LServer, TS]);
				  (timeout) ->
				       ok % Hmm.
			       end,
	      notify(LServer, PushLJID, Node, XData, HandleResponse)
      end, Clients).

-spec notify(binary(), ljid(), binary(), xdata(),
	     fun((iq() | timeout) -> any())) -> ok.
notify(LServer, PushLJID, Node, XData, HandleResponse) ->
    From = jid:make(LServer),
    Item = #ps_item{xml_els = [xmpp:encode(#push_notification{})]},
    PubSub = #pubsub{publish = #ps_publish{node = Node, items = [Item]},
		     publish_options = XData},
    IQ = #iq{type = set,
	     from = From,
	     to = jid:make(PushLJID),
	     id = randoms:get_string(),
	     sub_els = [PubSub]},
    ejabberd_router:route_iq(IQ, HandleResponse).

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec store_session(binary(), binary(), timestamp(), jid(), binary(), xdata())
      -> {ok, push_session()} | {error, err_reason()}.
store_session(LUser, LServer, TS, PushJID, Node, XData) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    delete_session(LUser, LServer, PushJID, Node),
    case use_cache(Mod, LServer) of
	true ->
	    ets_cache:delete(?PUSH_CACHE, {LUser, LServer},
			     cache_nodes(Mod, LServer)),
	    ets_cache:update(
		?PUSH_CACHE,
		{LUser, LServer, TS}, {ok, {TS, PushJID, Node, XData}},
		fun() ->
			Mod:store_session(LUser, LServer, TS, PushJID, Node,
					  XData)
		end, cache_nodes(Mod, LServer));
	false ->
	    Mod:store_session(LUser, LServer, TS, PushJID, Node, XData)
    end.

-spec lookup_session(binary(), binary(), timestamp())
      -> {ok, push_session()} | error | {error, err_reason()}.
lookup_session(LUser, LServer, TS) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case use_cache(Mod, LServer) of
	true ->
	    ets_cache:lookup(
	      ?PUSH_CACHE, {LUser, LServer, TS},
	      fun() -> Mod:lookup_session(LUser, LServer, TS) end);
	false ->
	    Mod:lookup_session(LUser, LServer, TS)
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

-spec delete_session(binary(), binary(), timestamp()) -> ok | {error, db_failure}.
delete_session(LUser, LServer, TS) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:delete_session(LUser, LServer, TS) of
	ok ->
	    case use_cache(Mod, LServer) of
		true ->
		    ets_cache:delete(?PUSH_CACHE, {LUser, LServer},
				     cache_nodes(Mod, LServer)),
		    ets_cache:delete(?PUSH_CACHE, {LUser, LServer, TS},
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
	{ok, {TS, _, _, _}} ->
	    delete_session(LUser, LServer, TS);
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
	      fun({TS, _, _, _}) ->
		      ok = Mod:delete_session(LUser, LServer, TS),
		      case use_cache(Mod, LServer) of
			  true ->
			      ets_cache:delete(?PUSH_CACHE,
					       {LUser, LServer, TS},
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
    SessIDs = ejabberd_sm:get_session_sids(LUser, LServer),
    [Client || {TS, _, _, _} = Client <- Clients,
	       lists:keyfind(TS, 1, SessIDs) == false].

%%--------------------------------------------------------------------
%% Caching.
%%--------------------------------------------------------------------
-spec init_cache(module(), binary(), gen_mod:opts()) -> ok.
init_cache(Mod, Host, Opts) ->
    case use_cache(Mod, Host) of
	true ->
	    CacheOpts = cache_opts(Host, Opts),
	    ets_cache:new(?PUSH_CACHE, CacheOpts);
	false ->
	    ets_cache:delete(?PUSH_CACHE)
    end.

-spec cache_opts(binary(), gen_mod:opts()) -> [proplists:property()].
cache_opts(Host, Opts) ->
    MaxSize = gen_mod:get_opt(
		cache_size, Opts,
		ejabberd_config:cache_size(Host)),
    CacheMissed = gen_mod:get_opt(
		    cache_missed, Opts,
		    ejabberd_config:cache_missed(Host)),
    LifeTime = case gen_mod:get_opt(
		      cache_life_time, Opts,
		      ejabberd_config:cache_life_time(Host)) of
		   infinity -> infinity;
		   I -> timer:seconds(I)
	       end,
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec use_cache(module(), binary()) -> boolean().
use_cache(Mod, Host) ->
    case erlang:function_exported(Mod, use_cache, 1) of
	true -> Mod:use_cache(Host);
	false ->
	    gen_mod:get_module_opt(
	      Host, ?MODULE, use_cache,
	      ejabberd_config:use_cache(Host))
    end.

-spec cache_nodes(module(), binary()) -> [node()].
cache_nodes(Mod, Host) ->
    case erlang:function_exported(Mod, cache_nodes, 1) of
	true -> Mod:cache_nodes(Host);
	false -> ejabberd_cluster:get_nodes()
    end.
