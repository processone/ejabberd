%%%----------------------------------------------------------------------
%%% File    : ejabberd_sm.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Session manager
%%% Created : 24 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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
%%%----------------------------------------------------------------------

-module(ejabberd_sm).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-ifndef(GEN_SERVER).
-define(GEN_SERVER, gen_server).
-endif.
-behaviour(?GEN_SERVER).

%% API
-export([start_link/0,
	 stop/0,
	 route/1,
	 route/2,
	 process_iq/1,
	 open_session/5,
	 open_session/6,
	 close_session/4,
	 check_in_subscription/6,
	 bounce_offline_message/1,
	 disconnect_removed_user/2,
	 get_user_resources/2,
	 get_user_present_resources/2,
	 set_presence/7,
	 unset_presence/6,
	 close_session_unset_presence/5,
	 set_offline_info/5,
	 get_offline_info/4,
	 dirty_get_sessions_list/0,
	 dirty_get_my_sessions_list/0,
	 get_vh_session_list/1,
	 get_vh_session_number/1,
	 get_vh_by_backend/1,
	 register_iq_handler/5,
	 unregister_iq_handler/2,
	 force_update_presence/1,
	 connected_users/0,
	 connected_users_number/0,
	 user_resources/2,
	 kick_user/2,
	 get_session_pid/3,
	 get_session_sid/3,
	 get_session_sids/2,
	 get_user_info/2,
	 get_user_info/3,
	 get_user_ip/3,
	 get_max_user_sessions/2,
	 get_all_pids/0,
	 is_existing_resource/3,
	 get_commands_spec/0,
	 c2s_handle_info/2,
	 host_up/1,
	 host_down/1,
	 make_sid/0,
	 clean_cache/1,
	 config_reloaded/0
	]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

-include("ejabberd_commands.hrl").
-include("ejabberd_sm.hrl").

-callback init() -> ok | {error, any()}.
-callback set_session(#session{}) -> ok | {error, any()}.
-callback delete_session(#session{}) -> ok | {error, any()}.
-callback get_sessions() -> [#session{}].
-callback get_sessions(binary()) -> [#session{}].
-callback get_sessions(binary(), binary()) -> {ok, [#session{}]} | {error, any()}.
-callback use_cache(binary()) -> boolean().
-callback cache_nodes(binary()) -> [node()].

-optional_callbacks([use_cache/1, cache_nodes/1]).

-record(state, {}).

%% default value for the maximum number of user connections
-define(MAX_USER_SESSIONS, infinity).

%%====================================================================
%% API
%%====================================================================
-export_type([sid/0, info/0]).

start_link() ->
    ?GEN_SERVER:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    ok.

-spec route(jid(), term()) -> ok.
%% @doc route arbitrary term to c2s process(es)
route(To, Term) ->
    case catch do_route(To, Term) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("route ~p to ~p failed: ~p",
		       [Term, To, Reason]);
	_ ->
	    ok
    end.

-spec route(stanza()) -> ok.
route(Packet) ->
    #jid{lserver = LServer} = xmpp:get_to(Packet),
    case ejabberd_hooks:run_fold(sm_receive_packet, LServer, Packet, []) of
	drop ->
	    ?DEBUG("hook dropped stanza:~n~s", [xmpp:pp(Packet)]);
	Packet1 ->
	    try do_route(Packet1), ok
	    catch E:R ->
		    ?ERROR_MSG("failed to route packet:~n~s~nReason = ~p",
			       [xmpp:pp(Packet1),
				{E, {R, erlang:get_stacktrace()}}])
	    end
    end.

-spec open_session(sid(), binary(), binary(), binary(), prio(), info()) -> ok.

open_session(SID, User, Server, Resource, Priority, Info) ->
    set_session(SID, User, Server, Resource, Priority, Info),
    check_for_sessions_to_replace(User, Server, Resource),
    JID = jid:make(User, Server, Resource),
    ejabberd_hooks:run(sm_register_connection_hook,
		       JID#jid.lserver, [SID, JID, Info]).

-spec open_session(sid(), binary(), binary(), binary(), info()) -> ok.

open_session(SID, User, Server, Resource, Info) ->
    open_session(SID, User, Server, Resource, undefined, Info).

-spec close_session(sid(), binary(), binary(), binary()) -> ok.

close_session(SID, User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod = get_sm_backend(LServer),
    Sessions = get_sessions(Mod, LUser, LServer, LResource),
    Info = case lists:keyfind(SID, #session.sid, Sessions) of
	       #session{info = I} = Session ->
		   delete_session(Mod, Session),
		   I;
	       _ ->
		   []
	   end,
    JID = jid:make(User, Server, Resource),
    ejabberd_hooks:run(sm_remove_connection_hook,
		       JID#jid.lserver, [SID, JID, Info]).

-spec check_in_subscription(boolean(), binary(), binary(), jid(),
			    subscribe | subscribed | unsubscribe | unsubscribed,
			    binary()) -> boolean() | {stop, false}.
check_in_subscription(Acc, User, Server, _JID, _Type, _Reason) ->
    case ejabberd_auth:user_exists(User, Server) of
      true -> Acc;
      false -> {stop, false}
    end.

-spec bounce_offline_message({bounce, message()} | any()) -> any().

bounce_offline_message({bounce, #message{type = T} = Packet} = Acc)
    when T == chat; T == groupchat; T == normal ->
    Lang = xmpp:get_lang(Packet),
    Txt = <<"User session not found">>,
    Err = xmpp:err_service_unavailable(Txt, Lang),
    ejabberd_router:route_error(Packet, Err),
    {stop, Acc};
bounce_offline_message(Acc) ->
    Acc.

-spec disconnect_removed_user(binary(), binary()) -> ok.

disconnect_removed_user(User, Server) ->
    route(jid:make(User, Server), {exit, <<"User removed">>}).

get_user_resources(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = get_sm_backend(LServer),
    Ss = online(get_sessions(Mod, LUser, LServer)),
    [element(3, S#session.usr) || S <- clean_session_list(Ss)].

-spec get_user_present_resources(binary(), binary()) -> [tuple()].

get_user_present_resources(LUser, LServer) ->
    Mod = get_sm_backend(LServer),
    Ss = online(get_sessions(Mod, LUser, LServer)),
    [{S#session.priority, element(3, S#session.usr)}
     || S <- clean_session_list(Ss), is_integer(S#session.priority)].

-spec get_user_ip(binary(), binary(), binary()) -> ip().

get_user_ip(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod = get_sm_backend(LServer),
    case online(get_sessions(Mod, LUser, LServer, LResource)) of
	[] ->
	    undefined;
	Ss ->
	    Session = lists:max(Ss),
	    proplists:get_value(ip, Session#session.info)
    end.

-spec get_user_info(binary(), binary()) -> [{binary(), info()}].
get_user_info(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = get_sm_backend(LServer),
    Ss = online(get_sessions(Mod, LUser, LServer)),
    [{LResource, [{node, node(Pid)}|Info]}
     || #session{usr = {_, _, LResource},
		 info = Info,
		 sid = {_, Pid}} <- clean_session_list(Ss)].

-spec get_user_info(binary(), binary(), binary()) -> info() | offline.

get_user_info(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod = get_sm_backend(LServer),
    case online(get_sessions(Mod, LUser, LServer, LResource)) of
	[] ->
	    offline;
	Ss ->
	    Session = lists:max(Ss),
	    Node = node(element(2, Session#session.sid)),
	    [{node, Node}|Session#session.info]
    end.

-spec set_presence(sid(), binary(), binary(), binary(),
                   prio(), presence(), info()) -> ok.

set_presence(SID, User, Server, Resource, Priority,
	     Presence, Info) ->
    set_session(SID, User, Server, Resource, Priority,
		Info),
    ejabberd_hooks:run(set_presence_hook,
		       jid:nameprep(Server),
		       [User, Server, Resource, Presence]).

-spec unset_presence(sid(), binary(), binary(),
                     binary(), binary(), info()) -> ok.

unset_presence(SID, User, Server, Resource, Status,
	       Info) ->
    set_session(SID, User, Server, Resource, undefined,
		Info),
    ejabberd_hooks:run(unset_presence_hook,
		       jid:nameprep(Server),
		       [User, Server, Resource, Status]).

-spec close_session_unset_presence(sid(), binary(), binary(),
                                   binary(), binary()) -> ok.

close_session_unset_presence(SID, User, Server,
			     Resource, Status) ->
    close_session(SID, User, Server, Resource),
    ejabberd_hooks:run(unset_presence_hook,
		       jid:nameprep(Server),
		       [User, Server, Resource, Status]).

-spec get_session_pid(binary(), binary(), binary()) -> none | pid().

get_session_pid(User, Server, Resource) ->
    case get_session_sid(User, Server, Resource) of
	{_, PID} -> PID;
	none -> none
    end.

-spec get_session_sid(binary(), binary(), binary()) -> none | sid().

get_session_sid(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod = get_sm_backend(LServer),
    case online(get_sessions(Mod, LUser, LServer, LResource)) of
	[] ->
	    none;
	Ss ->
	    #session{sid = SID} = lists:max(Ss),
	    SID
    end.

-spec get_session_sids(binary(), binary()) -> [sid()].

get_session_sids(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = get_sm_backend(LServer),
    Sessions = online(get_sessions(Mod, LUser, LServer)),
    [SID || #session{sid = SID} <- Sessions].

-spec set_offline_info(sid(), binary(), binary(), binary(), info()) -> ok.

set_offline_info(SID, User, Server, Resource, Info) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    set_session(SID, LUser, LServer, LResource, undefined, [offline | Info]).

-spec get_offline_info(erlang:timestamp(), binary(), binary(),
                       binary()) -> none | info().

get_offline_info(Time, User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod = get_sm_backend(LServer),
    case get_sessions(Mod, LUser, LServer, LResource) of
	[#session{sid = {Time, _}, info = Info}] ->
	    case proplists:get_bool(offline, Info) of
		true ->
		    Info;
		false ->
		    none
	    end;
	_ ->
	    none
    end.

-spec dirty_get_sessions_list() -> [ljid()].

dirty_get_sessions_list() ->
    lists:flatmap(
      fun(Mod) ->
	      [S#session.usr || S <- online(get_sessions(Mod))]
      end, get_sm_backends()).

-spec dirty_get_my_sessions_list() -> [#session{}].

dirty_get_my_sessions_list() ->
    lists:flatmap(
      fun(Mod) ->
	      [S || S <- online(get_sessions(Mod)),
		    node(element(2, S#session.sid)) == node()]
      end, get_sm_backends()).

-spec get_vh_session_list(binary()) -> [ljid()].

get_vh_session_list(Server) ->
    LServer = jid:nameprep(Server),
    Mod = get_sm_backend(LServer),
    [S#session.usr || S <- online(get_sessions(Mod, LServer))].

-spec get_all_pids() -> [pid()].

get_all_pids() ->
    lists:flatmap(
      fun(Mod) ->
	      [element(2, S#session.sid) || S <- online(get_sessions(Mod))]
      end, get_sm_backends()).

-spec get_vh_session_number(binary()) -> non_neg_integer().

get_vh_session_number(Server) ->
    LServer = jid:nameprep(Server),
    Mod = get_sm_backend(LServer),
    length(online(get_sessions(Mod, LServer))).

-spec register_iq_handler(binary(), binary(), atom(), atom(), list()) -> ok.

register_iq_handler(Host, XMLNS, Module, Fun, Opts) ->
    ?GEN_SERVER:cast(?MODULE,
		    {register_iq_handler, Host, XMLNS, Module, Fun, Opts}).

-spec unregister_iq_handler(binary(), binary()) -> ok.

unregister_iq_handler(Host, XMLNS) ->
    ?GEN_SERVER:cast(?MODULE, {unregister_iq_handler, Host, XMLNS}).

%% Why the hell do we have so many similar kicks?
c2s_handle_info(#{lang := Lang} = State, replaced) ->
    State1 = State#{replaced => true},
    Err = xmpp:serr_conflict(<<"Replaced by new connection">>, Lang),
    {stop, ejabberd_c2s:send(State1, Err)};
c2s_handle_info(#{lang := Lang} = State, kick) ->
    Err = xmpp:serr_policy_violation(<<"has been kicked">>, Lang),
    c2s_handle_info(State, {kick, kicked_by_admin, Err});
c2s_handle_info(State, {kick, _Reason, Err}) ->
    {stop, ejabberd_c2s:send(State, Err)};
c2s_handle_info(#{lang := Lang} = State, {exit, Reason}) ->
    Err = xmpp:serr_conflict(Reason, Lang),
    {stop, ejabberd_c2s:send(State, Err)};
c2s_handle_info(State, _) ->
    State.

-spec config_reloaded() -> ok.
config_reloaded() ->
    init_cache().

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    init_cache(),
    lists:foreach(fun(Mod) -> Mod:init() end, get_sm_backends()),
    clean_cache(),
    ets:new(sm_iqtable, [named_table, public, {read_concurrency, true}]),
    ejabberd_hooks:add(host_up, ?MODULE, host_up, 50),
    ejabberd_hooks:add(host_down, ?MODULE, host_down, 60),
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 50),
    lists:foreach(fun host_up/1, ?MYHOSTS),
    ejabberd_commands:register_commands(get_commands_spec()),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

handle_cast({register_iq_handler, Host, XMLNS, Module,
	     Function, Opts},
	    State) ->
    ets:insert(sm_iqtable,
	       {{Host, XMLNS}, Module, Function, Opts}),
    {noreply, State};
handle_cast({unregister_iq_handler, Host, XMLNS},
	    State) ->
    case ets:lookup(sm_iqtable, {Host, XMLNS}) of
      [{_, Module, Function, Opts}] ->
	  gen_iq_handler:stop_iq_handler(Module, Function, Opts);
      _ -> ok
    end,
    ets:delete(sm_iqtable, {Host, XMLNS}),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({route, Packet}, State) ->
    route(Packet),
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    lists:foreach(fun host_down/1, ?MYHOSTS),
    ejabberd_hooks:delete(host_up, ?MODULE, host_up, 50),
    ejabberd_hooks:delete(host_down, ?MODULE, host_down, 60),
    ejabberd_hooks:delete(config_reloaded, ?MODULE, config_reloaded, 50),
    ejabberd_commands:unregister_commands(get_commands_spec()),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec host_up(binary()) -> ok.
host_up(Host) ->
    ejabberd_hooks:add(c2s_handle_info, Host,
		       ejabberd_sm, c2s_handle_info, 50),
    ejabberd_hooks:add(roster_in_subscription, Host,
		       ejabberd_sm, check_in_subscription, 20),
    ejabberd_hooks:add(offline_message_hook, Host,
		       ejabberd_sm, bounce_offline_message, 100),
    ejabberd_hooks:add(remove_user, Host,
		       ejabberd_sm, disconnect_removed_user, 100),
    ejabberd_c2s:host_up(Host).

-spec host_down(binary()) -> ok.
host_down(Host) ->
    Mod = get_sm_backend(Host),
    Err = case ejabberd_cluster:get_nodes() of
	    [Node] when Node == node() -> xmpp:serr_system_shutdown();
	    _ -> xmpp:serr_reset()
	  end,
    lists:foreach(
      fun(#session{sid = {_, Pid}}) when node(Pid) == node() ->
	      ejabberd_c2s:send(Pid, Err),
	      ejabberd_c2s:stop(Pid);
	 (_) ->
	      ok
      end, get_sessions(Mod, Host)),
    ejabberd_hooks:delete(c2s_handle_info, Host,
			  ejabberd_sm, c2s_handle_info, 50),
    ejabberd_hooks:delete(roster_in_subscription, Host,
			  ejabberd_sm, check_in_subscription, 20),
    ejabberd_hooks:delete(offline_message_hook, Host,
			  ejabberd_sm, bounce_offline_message, 100),
    ejabberd_hooks:delete(remove_user, Host,
			  ejabberd_sm, disconnect_removed_user, 100),
    ejabberd_c2s:host_down(Host).

-spec set_session(sid(), binary(), binary(), binary(),
                  prio(), info()) -> ok | {error, any()}.

set_session(SID, User, Server, Resource, Priority, Info) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},
    Mod = get_sm_backend(LServer),
    case Mod:set_session(#session{sid = SID, usr = USR, us = US,
				  priority = Priority, info = Info}) of
	ok ->
	    case use_cache(Mod, LServer) of
		true ->
		    ets_cache:delete(?SM_CACHE, {LUser, LServer},
				     cache_nodes(Mod, LServer));
		false ->
		    ok
	    end;
	{error, _} = Err ->
	    Err
    end.

-spec get_sessions(module()) -> [#session{}].
get_sessions(Mod) ->
    Mod:get_sessions().

-spec get_sessions(module(), binary()) -> [#session{}].
get_sessions(Mod, LServer) ->
    Mod:get_sessions(LServer).

-spec get_sessions(module(), binary(), binary()) -> [#session{}].
get_sessions(Mod, LUser, LServer) ->
    case use_cache(Mod, LServer) of
	true ->
	    case ets_cache:lookup(
		   ?SM_CACHE, {LUser, LServer},
		   fun() ->
			   case Mod:get_sessions(LUser, LServer) of
			       {ok, Ss} when Ss /= [] ->
				   {ok, Ss};
			       _ ->
				   error
			   end
		   end) of
		{ok, Sessions} ->
		    Sessions;
		error ->
		    []
	    end;
	false ->
	    case Mod:get_sessions(LUser, LServer) of
		{ok, Ss} -> Ss;
		_ -> []
	    end
    end.

-spec get_sessions(module(), binary(), binary(), binary()) -> [#session{}].
get_sessions(Mod, LUser, LServer, LResource) ->
    Sessions = get_sessions(Mod, LUser, LServer),
    [S || S <- Sessions, element(3, S#session.usr) == LResource].

-spec delete_session(module(), #session{}) -> ok.
delete_session(Mod, #session{usr = {LUser, LServer, _}} = Session) ->
    Mod:delete_session(Session),
    case use_cache(Mod, LServer) of
	true ->
	    ets_cache:delete(?SM_CACHE, {LUser, LServer},
			     cache_nodes(Mod, LServer));
	false ->
	    ok
    end.

-spec online([#session{}]) -> [#session{}].

online(Sessions) ->
    lists:filter(fun is_online/1, Sessions).

-spec is_online(#session{}) -> boolean().

is_online(#session{info = Info}) ->
    not proplists:get_bool(offline, Info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec do_route(jid(), term()) -> any().
do_route(#jid{lresource = <<"">>} = To, Term) ->
    lists:foreach(
      fun(R) ->
	      do_route(jid:replace_resource(To, R), Term)
      end, get_user_resources(To#jid.user, To#jid.server));
do_route(To, Term) ->
    ?DEBUG("broadcasting ~p to ~s", [Term, jid:encode(To)]),
    {U, S, R} = jid:tolower(To),
    Mod = get_sm_backend(S),
    case online(get_sessions(Mod, U, S, R)) of
	[] ->
	    ?DEBUG("dropping broadcast to unavailable resourse: ~p", [Term]);
	Ss ->
	    Session = lists:max(Ss),
	    Pid = element(2, Session#session.sid),
	    ?DEBUG("sending to process ~p: ~p", [Pid, Term]),
	    ejabberd_c2s:route(Pid, Term)
    end.

-spec do_route(stanza()) -> any().
do_route(#presence{from = From, to = To, type = T, status = Status} = Packet)
  when T == subscribe; T == subscribed; T == unsubscribe; T == unsubscribed ->
    ?DEBUG("processing subscription:~n~s", [xmpp:pp(Packet)]),
    #jid{user = User, server = Server,
	 luser = LUser, lserver = LServer} = To,
    Reason = if T == subscribe -> xmpp:get_text(Status);
		true -> <<"">>
	     end,
    case is_privacy_allow(Packet) andalso
	ejabberd_hooks:run_fold(
	  roster_in_subscription,
	  LServer, false,
	  [User, Server, From, T, Reason]) of
	true ->
	    Mod = get_sm_backend(LServer),
	    lists:foreach(
	      fun(#session{sid = SID, usr = {_, _, R},
			   priority = Prio}) when is_integer(Prio) ->
		      Pid = element(2, SID),
		      Packet1 = Packet#presence{to = jid:replace_resource(To, R)},
		      ?DEBUG("sending to process ~p:~n~s",
			     [Pid, xmpp:pp(Packet1)]),
		      ejabberd_c2s:route(Pid, {route, Packet1});
		 (_) ->
		      ok
	      end, online(get_sessions(Mod, LUser, LServer)));
	false ->
	    ok
    end;
do_route(#presence{to = #jid{lresource = <<"">>} = To} = Packet) ->
    ?DEBUG("processing presence to bare JID:~n~s", [xmpp:pp(Packet)]),
    {LUser, LServer, _} = jid:tolower(To),
    lists:foreach(
      fun({_, R}) ->
	      do_route(Packet#presence{to = jid:replace_resource(To, R)})
      end, get_user_present_resources(LUser, LServer));
do_route(#message{to = #jid{lresource = <<"">>}, type = T} = Packet) ->
    ?DEBUG("processing message to bare JID:~n~s", [xmpp:pp(Packet)]),
    if T == chat; T == headline; T == normal ->
	    route_message(Packet);
       true ->
	    Lang = xmpp:get_lang(Packet),
	    ErrTxt = <<"User session not found">>,
	    Err = xmpp:err_service_unavailable(ErrTxt, Lang),
	    ejabberd_router:route_error(Packet, Err)
    end;
do_route(#iq{to = #jid{lresource = <<"">>}} = Packet) ->
    ?DEBUG("processing IQ to bare JID:~n~s", [xmpp:pp(Packet)]),
    process_iq(Packet);
do_route(Packet) ->
    ?DEBUG("processing packet to full JID:~n~s", [xmpp:pp(Packet)]),
    To = xmpp:get_to(Packet),
    {LUser, LServer, LResource} = jid:tolower(To),
    Mod = get_sm_backend(LServer),
    case online(get_sessions(Mod, LUser, LServer, LResource)) of
	[] ->
	    case Packet of
		#message{type = T} when T == chat; T == normal ->
		    route_message(Packet);
		#message{type = T} when T == headline ->
		    ?DEBUG("dropping headline to unavailable resource:~n~s",
			   [xmpp:pp(Packet)]);
		#presence{} ->
		    ?DEBUG("dropping presence to unavailable resource:~n~s",
			   [xmpp:pp(Packet)]);
		_ ->
		    Lang = xmpp:get_lang(Packet),
		    ErrTxt = <<"User session not found">>,
		    Err = xmpp:err_service_unavailable(ErrTxt, Lang),
		    ejabberd_router:route_error(Packet, Err)
	    end;
	Ss ->
	    Session = lists:max(Ss),
	    Pid = element(2, Session#session.sid),
	    ?DEBUG("sending to process ~p:~n~s", [Pid, xmpp:pp(Packet)]),
	    ejabberd_c2s:route(Pid, {route, Packet})
    end.

%% The default list applies to the user as a whole,
%% and is processed if there is no active list set
%% for the target session/resource to which a stanza is addressed,
%% or if there are no current sessions for the user.
-spec is_privacy_allow(stanza()) -> boolean().
is_privacy_allow(Packet) ->
    To = xmpp:get_to(Packet),
    LServer = To#jid.server,
    allow == ejabberd_hooks:run_fold(
	       privacy_check_packet, LServer, allow,
	       [To, Packet, in]).

-spec route_message(message()) -> any().
route_message(#message{to = To, type = Type} = Packet) ->
    LUser = To#jid.luser,
    LServer = To#jid.lserver,
    PrioRes = get_user_present_resources(LUser, LServer),
    case catch lists:max(PrioRes) of
      {MaxPrio, MaxRes}
	  when is_integer(MaxPrio), MaxPrio >= 0 ->
	  lists:foreach(fun ({P, R}) when P == MaxPrio;
					  (P >= 0) and (Type == headline) ->
				LResource = jid:resourceprep(R),
				Mod = get_sm_backend(LServer),
				case online(get_sessions(Mod, LUser, LServer,
							     LResource)) of
				  [] ->
				      ok; % Race condition
				  Ss ->
				      Session = lists:max(Ss),
				      Pid = element(2, Session#session.sid),
				      ?DEBUG("sending to process ~p~n", [Pid]),
				      LMaxRes = jid:resourceprep(MaxRes),
				      Packet1 = maybe_mark_as_copy(Packet,
								   LResource,
								   LMaxRes,
								   P, MaxPrio),
				      ejabberd_c2s:route(Pid, {route, Packet1})
				end;
			    %% Ignore other priority:
			    ({_Prio, _Res}) -> ok
			end,
			PrioRes);
      _ ->
	    case ejabberd_auth:user_exists(LUser, LServer) andalso
		is_privacy_allow(Packet) of
		true ->
		    ejabberd_hooks:run_fold(offline_message_hook,
					    LServer, {bounce, Packet}, []);
		false ->
		    Err = xmpp:err_service_unavailable(),
		    ejabberd_router:route_error(Packet, Err)
	    end
    end.

-spec maybe_mark_as_copy(message(), binary(), binary(), integer(), integer())
      -> message().
maybe_mark_as_copy(Packet, R, R, P, P) ->
    Packet;
maybe_mark_as_copy(Packet, _, _, P, P) ->
    xmpp:put_meta(Packet, sm_copy, true);
maybe_mark_as_copy(Packet, _, _, _, _) ->
    Packet.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec clean_session_list([#session{}]) -> [#session{}].
clean_session_list(Ss) ->
    clean_session_list(lists:keysort(#session.usr, Ss), []).

-spec clean_session_list([#session{}], [#session{}]) -> [#session{}].
clean_session_list([], Res) -> Res;
clean_session_list([S], Res) -> [S | Res];
clean_session_list([S1, S2 | Rest], Res) ->
    if S1#session.usr == S2#session.usr ->
	   if S1#session.sid > S2#session.sid ->
		  clean_session_list([S1 | Rest], Res);
	      true -> clean_session_list([S2 | Rest], Res)
	   end;
       true -> clean_session_list([S2 | Rest], [S1 | Res])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% On new session, check if some existing connections need to be replace
-spec check_for_sessions_to_replace(binary(), binary(), binary()) -> ok | replaced.
check_for_sessions_to_replace(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    check_existing_resources(LUser, LServer, LResource),
    check_max_sessions(LUser, LServer).

-spec check_existing_resources(binary(), binary(), binary()) -> ok.
check_existing_resources(LUser, LServer, LResource) ->
    Mod = get_sm_backend(LServer),
    Ss = get_sessions(Mod, LUser, LServer, LResource),
    {OnlineSs, OfflineSs} = lists:partition(fun is_online/1, Ss),
    lists:foreach(fun(S) ->
			  delete_session(Mod, S)
		  end, OfflineSs),
    if OnlineSs == [] -> ok;
       true ->
	   SIDs = [SID || #session{sid = SID} <- OnlineSs],
	   MaxSID = lists:max(SIDs),
	   lists:foreach(fun ({_, Pid} = S) when S /= MaxSID ->
				 ejabberd_c2s:route(Pid, replaced);
			     (_) -> ok
			 end,
			 SIDs)
    end.

-spec is_existing_resource(binary(), binary(), binary()) -> boolean().

is_existing_resource(LUser, LServer, LResource) ->
    [] /= get_resource_sessions(LUser, LServer, LResource).

-spec get_resource_sessions(binary(), binary(), binary()) -> [sid()].
get_resource_sessions(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod = get_sm_backend(LServer),
    [S#session.sid || S <- online(get_sessions(Mod, LUser, LServer, LResource))].

-spec check_max_sessions(binary(), binary()) -> ok | replaced.
check_max_sessions(LUser, LServer) ->
    Mod = get_sm_backend(LServer),
    Ss = get_sessions(Mod, LUser, LServer),
    {OnlineSs, OfflineSs} = lists:partition(fun is_online/1, Ss),
    MaxSessions = get_max_user_sessions(LUser, LServer),
    if length(OnlineSs) =< MaxSessions -> ok;
       true ->
	    #session{sid = {_, Pid}} = lists:min(OnlineSs),
	    ejabberd_c2s:route(Pid, replaced)
    end,
    if length(OfflineSs) =< MaxSessions -> ok;
       true ->
	    delete_session(Mod, lists:min(OfflineSs))
    end.

%% Get the user_max_session setting
%% This option defines the max number of time a given users are allowed to
%% log in
%% Defaults to infinity
-spec get_max_user_sessions(binary(), binary()) -> infinity | non_neg_integer().
get_max_user_sessions(LUser, Host) ->
    case acl:match_rule(Host, max_user_sessions,
			jid:make(LUser, Host))
	of
      Max when is_integer(Max) -> Max;
      infinity -> infinity;
      _ -> ?MAX_USER_SESSIONS
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_iq(iq()) -> any().
process_iq(#iq{to = To, type = T, lang = Lang, sub_els = [El]} = Packet)
  when T == get; T == set ->
    XMLNS = xmpp:get_ns(El),
    Host = To#jid.lserver,
    case ets:lookup(sm_iqtable, {Host, XMLNS}) of
	[{_, Module, Function, Opts}] ->
	    gen_iq_handler:handle(Host, Module, Function, Opts, Packet);
	[] ->
	    Txt = <<"No module is handling this query">>,
	    Err = xmpp:err_service_unavailable(Txt, Lang),
	    ejabberd_router:route_error(Packet, Err)
    end;
process_iq(#iq{type = T, lang = Lang, sub_els = SubEls} = Packet)
  when T == get; T == set ->
    Txt = case SubEls of
	      [] -> <<"No child elements found">>;
	      _ -> <<"Too many child elements">>
	  end,
    Err = xmpp:err_bad_request(Txt, Lang),
    ejabberd_router:route_error(Packet, Err);
process_iq(#iq{}) ->
    ok.

-spec force_update_presence({binary(), binary()}) -> ok.

force_update_presence({LUser, LServer}) ->
    Mod = get_sm_backend(LServer),
    Ss = online(get_sessions(Mod, LUser, LServer)),
    lists:foreach(fun (#session{sid = {_, Pid}}) ->
			  ejabberd_c2s:resend_presence(Pid)
		  end,
		  Ss).

-spec get_sm_backend(binary()) -> module().

get_sm_backend(Host) ->
    DBType = ejabberd_config:get_option(
	       {sm_db_type, Host},
	       ejabberd_config:default_ram_db(Host, ?MODULE)),
    list_to_atom("ejabberd_sm_" ++ atom_to_list(DBType)).

-spec get_sm_backends() -> [module()].

get_sm_backends() ->
    lists:usort([get_sm_backend(Host) || Host <- ?MYHOSTS]).

-spec get_vh_by_backend(module()) -> [binary()].

get_vh_by_backend(Mod) ->
    lists:filter(
      fun(Host) ->
	      get_sm_backend(Host) == Mod
      end, ?MYHOSTS).

%%--------------------------------------------------------------------
%%% Cache stuff
%%--------------------------------------------------------------------
-spec init_cache() -> ok.
init_cache() ->
    case use_cache() of
	true ->
	    ets_cache:new(?SM_CACHE, cache_opts());
	false ->
	    ets_cache:delete(?SM_CACHE)
    end.

-spec cache_opts() -> [proplists:property()].
cache_opts() ->
    MaxSize = ejabberd_config:get_option(
		sm_cache_size,
		ejabberd_config:cache_size(global)),
    CacheMissed = ejabberd_config:get_option(
		    sm_cache_missed,
		    ejabberd_config:cache_missed(global)),
    LifeTime = case ejabberd_config:get_option(
		      sm_cache_life_time,
		      ejabberd_config:cache_life_time(global)) of
		   infinity -> infinity;
		   I -> timer:seconds(I)
	       end,
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec clean_cache(node()) -> ok.
clean_cache(Node) ->
    ets_cache:filter(
      ?SM_CACHE,
      fun(_, error) ->
	      false;
	 (_, {ok, Ss}) ->
	      not lists:any(
		    fun(#session{sid = {_, Pid}}) ->
			    node(Pid) == Node
		    end, Ss)
      end).

-spec clean_cache() -> ok.
clean_cache() ->
    ejabberd_cluster:eval_everywhere(?MODULE, clean_cache, [node()]).

-spec use_cache(module(), binary()) -> boolean().
use_cache(Mod, LServer) ->
    case erlang:function_exported(Mod, use_cache, 1) of
	true -> Mod:use_cache(LServer);
	false ->
	    ejabberd_config:get_option(
	      {sm_use_cache, LServer},
	      ejabberd_config:use_cache(LServer))
    end.

-spec use_cache() -> boolean().
use_cache() ->
    lists:any(
      fun(Host) ->
	      Mod = get_sm_backend(Host),
	      use_cache(Mod, Host)
      end, ?MYHOSTS).

-spec cache_nodes(module(), binary()) -> [node()].
cache_nodes(Mod, LServer) ->
    case erlang:function_exported(Mod, cache_nodes, 1) of
	true -> Mod:cache_nodes(LServer);
	false -> ejabberd_cluster:get_nodes()
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ejabberd commands

get_commands_spec() ->
    [#ejabberd_commands{name = connected_users, tags = [session],
			desc = "List all established sessions",
                        policy = admin,
			module = ?MODULE, function = connected_users, args = [],
			result_desc = "List of users sessions",
			result_example = [<<"user1@example.com">>, <<"user2@example.com">>],
			result = {connected_users, {list, {sessions, string}}}},
     #ejabberd_commands{name = connected_users_number, tags = [session, stats],
			desc = "Get the number of established sessions",
                        policy = admin,
			module = ?MODULE, function = connected_users_number,
			result_example = 2,
			args = [], result = {num_sessions, integer}},
     #ejabberd_commands{name = user_resources, tags = [session],
			desc = "List user's connected resources",
                        policy = admin,
			module = ?MODULE, function = user_resources,
			args = [{user, binary}, {host, binary}],
			args_desc = ["User name", "Server name"],
			args_example = [<<"user1">>, <<"example.com">>],
			result_example = [<<"tka1">>, <<"Gajim">>, <<"mobile-app">>],
			result = {resources, {list, {resource, string}}}},
     #ejabberd_commands{name = kick_user, tags = [session],
			desc = "Disconnect user's active sessions",
			module = ?MODULE, function = kick_user,
			args = [{user, binary}, {host, binary}],
			args_desc = ["User name", "Server name"],
			args_example = [<<"user1">>, <<"example.com">>],
			result_desc = "Number of resources that were kicked",
			result_example = 3,
			result = {num_resources, integer}}].

-spec connected_users() -> [binary()].

connected_users() ->
    USRs = dirty_get_sessions_list(),
    SUSRs = lists:sort(USRs),
    lists:map(fun ({U, S, R}) -> <<U/binary, $@, S/binary, $/, R/binary>> end,
	      SUSRs).

connected_users_number() ->
    length(dirty_get_sessions_list()).

user_resources(User, Server) ->
    Resources = get_user_resources(User, Server),
    lists:sort(Resources).

kick_user(User, Server) ->
    Resources = get_user_resources(User, Server),
    lists:foreach(
	fun(Resource) ->
		PID = get_session_pid(User, Server, Resource),
		ejabberd_c2s:route(PID, kick)
	end, Resources),
    length(Resources).

make_sid() ->
    {p1_time_compat:unique_timestamp(), self()}.

-spec opt_type(sm_db_type) -> fun((atom()) -> atom());
	      (sm_use_cache) -> fun((boolean()) -> boolean());
	      (sm_cache_missed) -> fun((boolean()) -> boolean());
	      (sm_cache_size) -> fun((timeout()) -> timeout());
	      (sm_cache_life_time) -> fun((timeout()) -> timeout());
	      (atom()) -> [atom()].
opt_type(sm_db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
opt_type(O) when O == sm_use_cache; O == sm_cache_missed ->
    fun(B) when is_boolean(B) -> B end;
opt_type(O) when O == sm_cache_size; O == sm_cache_life_time ->
    fun(I) when is_integer(I), I>0 -> I;
       (unlimited) -> infinity;
       (infinity) -> infinity
    end;
opt_type(_) ->
    [sm_db_type, sm_use_cache, sm_cache_size, sm_cache_missed,
     sm_cache_life_time].
