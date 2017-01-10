%%%----------------------------------------------------------------------
%%% File    : ejabberd_sm.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Session manager
%%% Created : 24 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-behaviour(gen_server).

%% API
-export([start/0,
	 start_link/0,
	 route/3,
	 process_iq/3,
	 open_session/5,
	 open_session/6,
	 close_session/4,
	 check_in_subscription/6,
	 bounce_offline_message/3,
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
	 register_iq_handler/4,
	 register_iq_handler/5,
	 unregister_iq_handler/2,
	 force_update_presence/1,
	 connected_users/0,
	 connected_users_number/0,
	 user_resources/2,
	 kick_user/2,
	 get_session_pid/3,
	 get_user_info/3,
	 get_user_ip/3,
	 get_max_user_sessions/2,
	 get_all_pids/0,
	 is_existing_resource/3,
	 get_commands_spec/0,
	 make_sid/0
	]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

-include("ejabberd_commands.hrl").
-include("mod_privacy.hrl").
-include("ejabberd_sm.hrl").

-callback init() -> ok | {error, any()}.
-callback set_session(#session{}) -> ok.
-callback delete_session(binary(), binary(), binary(), sid()) ->
    {ok, #session{}} | {error, notfound}.
-callback get_sessions() -> [#session{}].
-callback get_sessions(binary()) -> [#session{}].
-callback get_sessions(binary(), binary()) -> [#session{}].
-callback get_sessions(binary(), binary(), binary()) -> [#session{}].

-record(state, {}).

%% default value for the maximum number of user connections
-define(MAX_USER_SESSIONS, infinity).

-type broadcast() :: {broadcast, broadcast_data()}.

-type broadcast_data() ::
        {rebind, pid(), binary()} | %% ejabberd_c2s
        {item, ljid(), mod_roster:subscription()} | %% mod_roster/mod_shared_roster
        {exit, binary()} | %% mod_roster/mod_shared_roster
        {privacy_list, mod_privacy:userlist(), binary()} | %% mod_privacy
        {blocking, unblock_all | {block | unblock, [ljid()]}}. %% mod_blocking

%%====================================================================
%% API
%%====================================================================
-export_type([sid/0, info/0]).

start() ->
    ChildSpec = {?MODULE, {?MODULE, start_link, []},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec route(jid(), jid(), stanza() | broadcast()) -> ok.

route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
      {'EXIT', Reason} ->
	  ?ERROR_MSG("~p~nwhen processing: ~p",
		     [Reason, {From, To, Packet}]);
      _ -> ok
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
    Info = case Mod:delete_session(LUser, LServer, LResource, SID) of
	       {ok, #session{info = I}} -> I;
	       {error, notfound} -> []
	   end,
    JID = jid:make(User, Server, Resource),
    ejabberd_hooks:run(sm_remove_connection_hook,
		       JID#jid.lserver, [SID, JID, Info]).

-spec check_in_subscription(boolean(), binary(), binary(), jid(),
			    subscribe | subscribed | unsubscribe | unsubscribed,
			    binary()) -> boolean() | {stop, false}.
check_in_subscription(Acc, User, Server, _JID, _Type, _Reason) ->
    case ejabberd_auth:is_user_exists(User, Server) of
      true -> Acc;
      false -> {stop, false}
    end.

-spec bounce_offline_message(jid(), jid(), message()) -> stop.

bounce_offline_message(From, To, Packet) ->
    Lang = xmpp:get_lang(Packet),
    Txt = <<"User session not found">>,
    Err = xmpp:err_service_unavailable(Txt, Lang),
    ejabberd_router:route_error(To, From, Packet, Err),
    stop.

-spec disconnect_removed_user(binary(), binary()) -> ok.

disconnect_removed_user(User, Server) ->
    ejabberd_sm:route(jid:make(<<"">>, <<"">>, <<"">>),
		      jid:make(User, Server, <<"">>),
                      {broadcast, {exit, <<"User removed">>}}).

get_user_resources(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = get_sm_backend(LServer),
    Ss = online(Mod:get_sessions(LUser, LServer)),
    [element(3, S#session.usr) || S <- clean_session_list(Ss)].

-spec get_user_present_resources(binary(), binary()) -> [tuple()].

get_user_present_resources(LUser, LServer) ->
    Mod = get_sm_backend(LServer),
    Ss = online(Mod:get_sessions(LUser, LServer)),
    [{S#session.priority, element(3, S#session.usr)}
     || S <- clean_session_list(Ss), is_integer(S#session.priority)].

-spec get_user_ip(binary(), binary(), binary()) -> ip().

get_user_ip(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod = get_sm_backend(LServer),
    case online(Mod:get_sessions(LUser, LServer, LResource)) of
	[] ->
	    undefined;
	Ss ->
	    Session = lists:max(Ss),
	    proplists:get_value(ip, Session#session.info)
    end.

-spec get_user_info(binary(), binary(), binary()) -> info() | offline.

get_user_info(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod = get_sm_backend(LServer),
    case online(Mod:get_sessions(LUser, LServer, LResource)) of
	[] ->
	    offline;
	Ss ->
	    Session = lists:max(Ss),
	    Node = node(element(2, Session#session.sid)),
	    Conn = proplists:get_value(conn, Session#session.info),
	    IP = proplists:get_value(ip, Session#session.info),
	    [{node, Node}, {conn, Conn}, {ip, IP}]
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
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod = get_sm_backend(LServer),
    case online(Mod:get_sessions(LUser, LServer, LResource)) of
	[#session{sid = {_, Pid}}] -> Pid;
	_ -> none
    end.

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
    case Mod:get_sessions(LUser, LServer, LResource) of
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
	      [S#session.usr || S <- online(Mod:get_sessions())]
      end, get_sm_backends()).

-spec dirty_get_my_sessions_list() -> [#session{}].

dirty_get_my_sessions_list() ->
    lists:flatmap(
      fun(Mod) ->
	      [S || S <- online(Mod:get_sessions()),
		    node(element(2, S#session.sid)) == node()]
      end, get_sm_backends()).

-spec get_vh_session_list(binary()) -> [ljid()].

get_vh_session_list(Server) ->
    LServer = jid:nameprep(Server),
    Mod = get_sm_backend(LServer),
    [S#session.usr || S <- online(Mod:get_sessions(LServer))].

-spec get_all_pids() -> [pid()].

get_all_pids() ->
    lists:flatmap(
      fun(Mod) ->
	      [element(2, S#session.sid) || S <- online(Mod:get_sessions())]
      end, get_sm_backends()).

-spec get_vh_session_number(binary()) -> non_neg_integer().

get_vh_session_number(Server) ->
    LServer = jid:nameprep(Server),
    Mod = get_sm_backend(LServer),
    length(online(Mod:get_sessions(LServer))).

register_iq_handler(Host, XMLNS, Module, Fun) ->
    ejabberd_sm ! {register_iq_handler, Host, XMLNS, Module, Fun}.

-spec register_iq_handler(binary(), binary(), atom(), atom(), list()) -> any().

register_iq_handler(Host, XMLNS, Module, Fun, Opts) ->
    ejabberd_sm ! {register_iq_handler, Host, XMLNS, Module, Fun, Opts}.

-spec unregister_iq_handler(binary(), binary()) -> any().

unregister_iq_handler(Host, XMLNS) ->
    ejabberd_sm ! {unregister_iq_handler, Host, XMLNS}.


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    lists:foreach(fun(Mod) -> Mod:init() end, get_sm_backends()),
    ets:new(sm_iqtable, [named_table]),
    lists:foreach(
      fun(Host) ->
	      ejabberd_hooks:add(roster_in_subscription, Host,
				 ejabberd_sm, check_in_subscription, 20),
	      ejabberd_hooks:add(offline_message_hook, Host,
				 ejabberd_sm, bounce_offline_message, 100),
	      ejabberd_hooks:add(remove_user, Host,
				 ejabberd_sm, disconnect_removed_user, 100)
      end, ?MYHOSTS),
    ejabberd_commands:register_commands(get_commands_spec()),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);
	_ ->
	    ok
    end,
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module, Function}, State) ->
    ets:insert(sm_iqtable, {{XMLNS, Host}, Module, Function}),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module,
	     Function, Opts},
	    State) ->
    ets:insert(sm_iqtable,
	       {{XMLNS, Host}, Module, Function, Opts}),
    {noreply, State};
handle_info({unregister_iq_handler, Host, XMLNS},
	    State) ->
    case ets:lookup(sm_iqtable, {XMLNS, Host}) of
      [{_, Module, Function, Opts}] ->
	  gen_iq_handler:stop_iq_handler(Module, Function, Opts);
      _ -> ok
    end,
    ets:delete(sm_iqtable, {XMLNS, Host}),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_commands:unregister_commands(get_commands_spec()),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec set_session(sid(), binary(), binary(), binary(),
                  prio(), info()) -> ok.

set_session(SID, User, Server, Resource, Priority, Info) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},
    Mod = get_sm_backend(LServer),
    Mod:set_session(#session{sid = SID, usr = USR, us = US,
			     priority = Priority, info = Info}).

-spec online([#session{}]) -> [#session{}].

online(Sessions) ->
    lists:filter(fun is_online/1, Sessions).

-spec is_online(#session{}) -> boolean().

is_online(#session{info = Info}) ->
    not proplists:get_bool(offline, Info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec do_route(jid(), jid(), stanza() | broadcast()) -> any().
do_route(From, #jid{lresource = <<"">>} = To, {broadcast, _} = Packet) ->
    ?DEBUG("processing broadcast to bare JID: ~p", [Packet]),
    lists:foreach(
      fun(R) ->
	      do_route(From, jid:replace_resource(To, R), Packet)
      end, get_user_resources(To#jid.user, To#jid.server));
do_route(From, To, {broadcast, _} = Packet) ->
    ?DEBUG("processing broadcast to full JID: ~p", [Packet]),
    {U, S, R} = jid:tolower(To),
    Mod = get_sm_backend(S),
    case online(Mod:get_sessions(U, S, R)) of
	[] ->
	    ?DEBUG("dropping broadcast to unavailable resourse: ~p", [Packet]);
	Ss ->
	    Session = lists:max(Ss),
	    Pid = element(2, Session#session.sid),
	    ?DEBUG("sending to process ~p: ~p", [Pid, Packet]),
	    Pid ! {route, From, To, Packet}
    end;
do_route(From, To, #presence{type = T, status = Status} = Packet)
  when T == subscribe; T == subscribed; T == unsubscribe; T == unsubscribed ->
    ?DEBUG("processing subscription:~n~s", [xmpp:pp(Packet)]),
    #jid{user = User, server = Server,
	 luser = LUser, lserver = LServer} = To,
    Reason = if T == subscribe -> xmpp:get_text(Status);
		true -> <<"">>
	     end,
    case is_privacy_allow(From, To, Packet) andalso
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
		      ?DEBUG("sending to process ~p:~n~s",
			     [Pid, xmpp:pp(Packet)]),
		      Pid ! {route, From, jid:replace_resource(To, R), Packet};
		 (_) ->
		      ok
	      end, online(Mod:get_sessions(LUser, LServer)));
	false ->
	    ok
    end;
do_route(From, #jid{lresource = <<"">>} = To, #presence{} = Packet) ->
    ?DEBUG("processing presence to bare JID:~n~s", [xmpp:pp(Packet)]),
    {LUser, LServer, _} = jid:tolower(To),
    lists:foreach(
      fun({_, R}) ->
	      do_route(From, jid:replace_resource(To, R), Packet)
      end, get_user_present_resources(LUser, LServer));
do_route(From, #jid{lresource = <<"">>} = To, #message{type = T} = Packet) ->
    ?DEBUG("processing message to bare JID:~n~s", [xmpp:pp(Packet)]),
    if T == chat; T == headline; T == normal; T == groupchat ->
	    route_message(From, To, Packet, T);
       true ->
	    Lang = xmpp:get_lang(Packet),
	    ErrTxt = <<"User session not found">>,
	    Err = xmpp:err_service_unavailable(ErrTxt, Lang),
	    ejabberd_router:route_error(To, From, Packet, Err)
    end;
do_route(From, #jid{lresource = <<"">>} = To, #iq{} = Packet) ->
    ?DEBUG("processing IQ to bare JID:~n~s", [xmpp:pp(Packet)]),
    process_iq(From, To, Packet);
do_route(From, To, Packet) ->
    ?DEBUG("processing packet to full JID:~n~s", [xmpp:pp(Packet)]),
    {LUser, LServer, LResource} = jid:tolower(To),
    Mod = get_sm_backend(LServer),
    case online(Mod:get_sessions(LUser, LServer, LResource)) of
	[] ->
	    case Packet of
		#message{type = T} when T == chat; T == normal;
					T == headline; T == groupchat ->
		    route_message(From, To, Packet, T);
		#presence{} ->
		    ?DEBUG("dropping presence to unavalable resource:~n~s",
			   [xmpp:pp(Packet)]);
		_ ->
		    Lang = xmpp:get_lang(Packet),
		    ErrTxt = <<"User session not found">>,
		    Err = xmpp:err_service_unavailable(ErrTxt, Lang),
		    ejabberd_router:route_error(To, From, Packet, Err)
	    end;
	Ss ->
	    Session = lists:max(Ss),
	    Pid = element(2, Session#session.sid),
	    ?DEBUG("sending to process ~p:~n~s", [Pid, xmpp:pp(Packet)]),
	    Pid ! {route, From, To, Packet}
    end.

%% The default list applies to the user as a whole,
%% and is processed if there is no active list set
%% for the target session/resource to which a stanza is addressed,
%% or if there are no current sessions for the user.
-spec is_privacy_allow(jid(), jid(), stanza()) -> boolean().
is_privacy_allow(From, To, Packet) ->
    User = To#jid.user,
    Server = To#jid.server,
    PrivacyList =
	ejabberd_hooks:run_fold(privacy_get_user_list, Server,
				#userlist{}, [User, Server]),
    is_privacy_allow(From, To, Packet, PrivacyList).

%% Check if privacy rules allow this delivery
%% Function copied from ejabberd_c2s.erl
-spec is_privacy_allow(jid(), jid(), stanza(), #userlist{}) -> boolean().
is_privacy_allow(From, To, Packet, PrivacyList) ->
    User = To#jid.user,
    Server = To#jid.server,
    allow ==
      ejabberd_hooks:run_fold(privacy_check_packet, Server,
			      allow,
			      [User, Server, PrivacyList, {From, To, Packet},
			       in]).

-spec route_message(jid(), jid(), message(), message_type()) -> any().
route_message(From, To, Packet, Type) ->
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
				case online(Mod:get_sessions(LUser, LServer,
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
				      Pid ! {route, From, To, Packet1}
				end;
			    %% Ignore other priority:
			    ({_Prio, _Res}) -> ok
			end,
			PrioRes);
      _ ->
	    case ejabberd_auth:is_user_exists(LUser, LServer) andalso
		is_privacy_allow(From, To, Packet) of
		true ->
		    ejabberd_hooks:run(offline_message_hook, LServer,
				       [From, To, Packet]);
		false ->
		    Err = xmpp:err_service_unavailable(),
		    ejabberd_router:route_error(To, From, Packet, Err)
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
    Ss = Mod:get_sessions(LUser, LServer, LResource),
    {OnlineSs, OfflineSs} = lists:partition(fun is_online/1, Ss),
    lists:foreach(fun(#session{sid = S}) ->
			  Mod:delete_session(LUser, LServer, LResource, S)
		  end, OfflineSs),
    if OnlineSs == [] -> ok;
       true ->
	   SIDs = [SID || #session{sid = SID} <- OnlineSs],
	   MaxSID = lists:max(SIDs),
	   lists:foreach(fun ({_, Pid} = S) when S /= MaxSID ->
				 Pid ! replaced;
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
    [S#session.sid || S <- online(Mod:get_sessions(LUser, LServer, LResource))].

-spec check_max_sessions(binary(), binary()) -> ok | replaced.
check_max_sessions(LUser, LServer) ->
    Mod = get_sm_backend(LServer),
    Ss = Mod:get_sessions(LUser, LServer),
    {OnlineSs, OfflineSs} = lists:partition(fun is_online/1, Ss),
    MaxSessions = get_max_user_sessions(LUser, LServer),
    if length(OnlineSs) =< MaxSessions -> ok;
       true ->
	    #session{sid = {_, Pid}} = lists:min(OnlineSs),
	    Pid ! replaced
    end,
    if length(OfflineSs) =< MaxSessions -> ok;
       true ->
	    #session{sid = SID, usr = {_, _, R}} = lists:min(OfflineSs),
	    Mod:delete_session(LUser, LServer, R, SID)
    end.

%% Get the user_max_session setting
%% This option defines the max number of time a given users are allowed to
%% log in
%% Defaults to infinity
-spec get_max_user_sessions(binary(), binary()) -> infinity | non_neg_integer().
get_max_user_sessions(LUser, Host) ->
    case acl:match_rule(Host, max_user_sessions,
			jid:make(LUser, Host, <<"">>))
	of
      Max when is_integer(Max) -> Max;
      infinity -> infinity;
      _ -> ?MAX_USER_SESSIONS
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_iq(jid(), jid(), iq()) -> any().
process_iq(From, To, #iq{type = T, lang = Lang, sub_els = [El]} = Packet)
  when T == get; T == set ->
    XMLNS = xmpp:get_ns(El),
    Host = To#jid.lserver,
    case ets:lookup(sm_iqtable, {XMLNS, Host}) of
	[{_, Module, Function}] ->
	    gen_iq_handler:handle(Host, Module, Function, no_queue,
				  From, To, Packet);
	[{_, Module, Function, Opts}] ->
	    gen_iq_handler:handle(Host, Module, Function, Opts,
				  From, To, Packet);
	[] ->
	    Txt = <<"No module is handling this query">>,
	    Err = xmpp:err_service_unavailable(Txt, Lang),
	    ejabberd_router:route_error(To, From, Packet, Err)
    end;
process_iq(From, To, #iq{type = T} = Packet) when T == get; T == set ->
    Err = xmpp:err_bad_request(),
    ejabberd_router:route_error(To, From, Packet, Err),
    ok;
process_iq(_From, _To, #iq{}) ->
    ok.

-spec force_update_presence({binary(), binary()}) -> ok.

force_update_presence({LUser, LServer}) ->
    Mod = get_sm_backend(LServer),
    Ss = online(Mod:get_sessions(LUser, LServer)),
    lists:foreach(fun (#session{sid = {_, Pid}}) ->
			  Pid ! {force_update_presence, LUser, LServer}
		  end,
		  Ss).

-spec get_sm_backend(binary()) -> module().

get_sm_backend(Host) ->
    DBType = ejabberd_config:get_option(
	       {sm_db_type, Host},
	       fun(T) -> ejabberd_config:v_db(?MODULE, T) end,
	       mnesia),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ejabberd commands

get_commands_spec() ->
    [#ejabberd_commands{name = connected_users,
			tags = [session],
			desc = "List all established sessions",
                        policy = admin,
			module = ?MODULE, function = connected_users, args = [],
			result = {connected_users, {list, {sessions, string}}}},
     #ejabberd_commands{name = connected_users_number,
			tags = [session, stats],
			desc = "Get the number of established sessions",
                        policy = admin,
			module = ?MODULE, function = connected_users_number,
			args = [], result = {num_sessions, integer}},
     #ejabberd_commands{name = user_resources,
			tags = [session],
			desc = "List user's connected resources",
                        policy = user,
			module = ?MODULE, function = user_resources,
			args = [],
			result = {resources, {list, {resource, string}}}},
     #ejabberd_commands{name = kick_user,
			tags = [session],
			desc = "Disconnect user's active sessions",
			module = ?MODULE, function = kick_user,
			args = [{user, binary}, {host, binary}],
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
		PID ! kick
	end, Resources),
    length(Resources).

make_sid() ->
    {p1_time_compat:unique_timestamp(), self()}.

opt_type(sm_db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
opt_type(_) -> [sm_db_type].
