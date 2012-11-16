%%%----------------------------------------------------------------------
%%% File    : ejabberd_sm.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Session manager
%%% Created : 24 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
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

-module(ejabberd_sm).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/0, route/3, do_route1/3, set_session/6,
	 open_session/5, open_session/6, close_session/4,
	 close_migrated_session/4, drop_session/1,
	 check_in_subscription/6, bounce_offline_message/3,
	 disconnect_removed_user/2, get_user_sessions/2,
	 get_user_resources/2, set_presence/7, unset_presence/6,
	 close_session_unset_presence/5,
	 dirty_get_sessions_list/0, dirty_get_my_sessions_list/0,
	 get_vh_session_list/1, get_vh_my_session_list/1,
	 get_vh_session_number/1, register_iq_handler/4,
	 register_iq_handler/5, unregister_iq_handler/2,
	 force_update_presence/1, connected_users/0,
	 connected_users_number/0, user_resources/2,
	 get_session_pid/3, get_user_info/3, get_user_ip/3,
	 is_existing_resource/3, node_up/1, node_down/1,
	 migrate/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").

-include("jlib.hrl").

-include("ejabberd_commands.hrl").

-include("mod_privacy.hrl").

-record(session, {sid, usr, us, priority, info}).

-record(state, {}).

-define(MAX_USER_SESSIONS, infinity).

-type sid() :: {erlang:timestamp(), pid()}.
-type ip() :: {inet:ip_address(), inet:port_number()} | undefined.
-type info() :: [{conn, atom()} | {ip, ip()} | {node, atom()}
                 | {oor, boolean()} | {auth_module, atom()}].
-type prio() :: undefined | integer().
-type session() :: #session{}.

-export_type([sid/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

-spec route(jid(), jid(), xmlel() | broadcast()) -> ok.

route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
      {'EXIT', Reason} ->
	  ?ERROR_MSG("~p~nwhen processing: ~p",
		     [Reason, {From, To, Packet}]);
      _ -> ok
    end.

-spec open_session(sid(), binary(), binary(), binary(), info()) -> ok.

open_session(SID, User, Server, Resource, Info) ->
    open_session(SID, User, Server, Resource, undefined,
		 Info).

-spec open_session(sid(), binary(), binary(), binary(), prio(), info()) -> ok.

open_session(SID, User, Server, Resource, Priority,
	     Info) ->
    set_session(SID, User, Server, Resource, Priority,
		Info),
    check_for_sessions_to_replace(User, Server, Resource),
    JID = jlib:make_jid(User, Server, Resource),
    ejabberd_hooks:run(sm_register_connection_hook,
		       JID#jid.lserver, [SID, JID, Info]).

-spec close_session(sid(), binary(), binary(), binary()) -> ok.

close_session(SID, User, Server, Resource) ->
    Info = do_close_session(SID),
    US = {jlib:nodeprep(User), jlib:nameprep(Server)},
    case ejabberd_cluster:get_node_new(US) of
      Node when Node /= node() ->
	  rpc:cast(Node, ?MODULE, drop_session, [SID]);
      _ -> ok
    end,
    JID = jlib:make_jid(User, Server, Resource),
    ejabberd_hooks:run(sm_remove_connection_hook,
		       JID#jid.lserver, [SID, JID, Info]).

-spec close_migrated_session(sid(), binary(), binary(), binary()) -> ok.

close_migrated_session(SID, User, Server, Resource) ->
    Info = do_close_session(SID),
    JID = jlib:make_jid(User, Server, Resource),
    ejabberd_hooks:run(sm_remove_migrated_connection_hook,
		       JID#jid.lserver, [SID, JID, Info]).

-spec do_close_session(sid()) -> info().

do_close_session(SID) ->
    Info = case mnesia:dirty_read({session, SID}) of
	     [] -> [];
	     [#session{info = I}] -> I
	   end,
    drop_session(SID),
    Info.

-spec drop_session(sid()) -> any().

drop_session(SID) ->
    F = fun () -> mnesia:delete({session, SID}) end,
    mnesia:sync_dirty(F).

-spec check_in_subscription(any(), binary(), binary(),
                            any(), any(), any()) -> any().

check_in_subscription(Acc, User, Server, _JID, _Type,
		      _Reason) ->
    case ejabberd_auth:is_user_exists(User, Server) of
      true -> Acc;
      false -> {stop, false}
    end.

-spec bounce_offline_message(jid(), jid(), xmlel()) -> stop.

bounce_offline_message(From, To, Packet) ->
    Err = jlib:make_error_reply(Packet,
				?ERR_SERVICE_UNAVAILABLE),
    ejabberd_router:route(To, From, Err),
    stop.

-spec disconnect_removed_user(binary(), binary()) -> ok.

disconnect_removed_user(User, Server) ->
    ejabberd_sm:route(jlib:make_jid(<<"">>, <<"">>, <<"">>),
		      jlib:make_jid(User, Server, <<"">>),
                      {broadcast, {exit, <<"User removed">>}}).

-spec get_user_sessions(binary(), binary()) -> [session()].

get_user_sessions(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case ejabberd_cluster:get_node({LUser, LServer}) of
      Node when Node == node() ->
	  catch mnesia:dirty_index_read(session, US, #session.us);
      Node ->
	  catch rpc:call(Node, mnesia, dirty_index_read,
			 [session, US, #session.us], 5000)
    end.

-spec get_user_resources(binary(), binary()) -> [binary()].

get_user_resources(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    Ss = case ejabberd_cluster:get_node({LUser, LServer}) of
	   Node when Node == node() ->
	       catch mnesia:dirty_index_read(session, US, #session.us);
	   Node ->
	       catch rpc:call(Node, mnesia, dirty_index_read,
			      [session, US, #session.us], 5000)
	 end,
    if is_list(Ss) ->
	   [element(3, S#session.usr)
	    || S <- clean_session_list(Ss)];
       true -> []
    end.

-spec get_user_ip(binary(), binary(), binary()) -> ip().

get_user_ip(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    USR = {LUser, LServer, LResource},
    Ss = case ejabberd_cluster:get_node({LUser, LServer}) of
	   Node when Node == node() ->
	       mnesia:dirty_index_read(session, USR, #session.usr);
	   Node ->
	       catch rpc:call(Node, mnesia, dirty_index_read,
			      [session, USR, #session.usr], 5000)
	 end,
    if is_list(Ss), Ss /= [] ->
	   Session = lists:max(Ss),
	   proplists:get_value(ip, Session#session.info);
       true -> undefined
    end.

-spec get_user_info(binary(), binary(), binary()) -> info() | offline.

get_user_info(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    USR = {LUser, LServer, LResource},
    Ss = case ejabberd_cluster:get_node({LUser, LServer}) of
	   Node when Node == node() ->
	       mnesia:dirty_index_read(session, USR, #session.usr);
	   Node ->
	       catch rpc:call(Node, mnesia, dirty_index_read,
			      [session, USR, #session.usr], 5000)
	 end,
    if is_list(Ss), Ss /= [] ->
	   Session = lists:max(Ss),
	   N = node(element(2, Session#session.sid)),
	   Conn = proplists:get_value(conn, Session#session.info),
	   IP = proplists:get_value(ip, Session#session.info),
	   [{node, N}, {conn, Conn}, {ip, IP}];
       true -> offline
    end.

-spec set_presence(sid(), binary(), binary(), binary(),
                   prio(), xmlel(), info()) -> ok.

set_presence(SID, User, Server, Resource, Priority,
	     Presence, Info) ->
    set_session(SID, User, Server, Resource, Priority,
		Info),
    ejabberd_hooks:run(set_presence_hook,
		       jlib:nameprep(Server),
		       [User, Server, Resource, Presence]).

-spec unset_presence(sid(), binary(), binary(),
                     binary(), binary(), info()) -> ok.

unset_presence(SID, User, Server, Resource, Status,
	       Info) ->
    set_session(SID, User, Server, Resource, undefined,
		Info),
    ejabberd_hooks:run(unset_presence_hook,
		       jlib:nameprep(Server),
		       [User, Server, Resource, Status]).

-spec close_session_unset_presence(sid(), binary(), binary(),
                                   binary(), binary()) -> ok.

close_session_unset_presence(SID, User, Server,
			     Resource, Status) ->
    close_session(SID, User, Server, Resource),
    ejabberd_hooks:run(unset_presence_hook,
		       jlib:nameprep(Server),
		       [User, Server, Resource, Status]).

-spec get_session_pid(binary(), binary(), binary()) -> none | pid().

get_session_pid(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    USR = {LUser, LServer, LResource},
    Res = case ejabberd_cluster:get_node({LUser, LServer})
	      of
	    Node when Node == node() ->
		mnesia:dirty_index_read(session, USR, #session.usr);
	    Node ->
		catch rpc:call(Node, mnesia, dirty_index_read,
			       [session, USR, #session.usr], 5000)
	  end,
    case Res of
      [#session{sid = {_, Pid}}] -> Pid;
      _ -> none
    end.

-spec dirty_get_sessions_list() -> [ljid()].

dirty_get_sessions_list() ->
    Match = [{#session{usr = '$1', _ = '_'}, [], ['$1']}],
    lists:flatmap(fun (Node) when Node == node() ->
			  mnesia:dirty_select(session, Match);
		      (Node) ->
			  case catch rpc:call(Node, mnesia, dirty_select,
					      [session, Match], 5000)
			      of
			    Ss when is_list(Ss) -> Ss;
			    _ -> []
			  end
		  end,
		  ejabberd_cluster:get_nodes()).

-spec dirty_get_my_sessions_list() -> [ljid()].

dirty_get_my_sessions_list() ->
    mnesia:dirty_select(session, [{#session{usr = '$1', _ = '_'}, [], ['$1']}]).

-spec get_vh_my_session_list(binary()) -> [ljid()].

get_vh_my_session_list(Server) ->
    LServer = jlib:nameprep(Server),
    mnesia:dirty_select(session,
			[{#session{usr = '$1', _ = '_'},
			  [{'==', {element, 2, '$1'}, LServer}], ['$1']}]).

-spec get_vh_session_list(binary()) -> [ljid()].

get_vh_session_list(Server) ->
    lists:flatmap(fun (Node) when Node == node() ->
			  get_vh_my_session_list(Server);
		      (Node) ->
			  case catch rpc:call(Node, ?MODULE,
					      get_vh_my_session_list, [Server],
					      5000)
			      of
			    Ss when is_list(Ss) -> Ss;
			    _ -> []
			  end
		  end,
		  ejabberd_cluster:get_nodes()).

-spec get_vh_session_number(binary()) -> non_neg_integer().

get_vh_session_number(Server) ->
    length(get_vh_session_list(Server)).

-spec register_iq_handler(binary(), binary(), atom(), atom()) -> any().

register_iq_handler(Host, XMLNS, Module, Fun) ->
    ejabberd_sm !
      {register_iq_handler, Host, XMLNS, Module, Fun}.

-spec register_iq_handler(binary(), binary(), atom(), atom(), list()) -> any().

register_iq_handler(Host, XMLNS, Module, Fun, Opts) ->
    ejabberd_sm !
      {register_iq_handler, Host, XMLNS, Module, Fun, Opts}.

-spec unregister_iq_handler(binary(), binary()) -> any().

unregister_iq_handler(Host, XMLNS) ->
    ejabberd_sm ! {unregister_iq_handler, Host, XMLNS}.

-spec migrate(atom(), up | down, non_neg_integer()) -> ok.

migrate(InitiatorNode, UpOrDown, After) ->
    Ss = mnesia:dirty_select(session,
			     [{#session{us = '$1', sid = {'_', '$2'}, _ = '_'},
			       [], ['$$']}]),
    lists:foreach(fun ([US, Pid]) ->
			  case ejabberd_cluster:get_node(US) of
			    Node when Node /= node() ->
				if InitiatorNode == node() andalso
				     UpOrDown == down ->
				       ejabberd_c2s:migrate_shutdown(Pid, Node,
								     random:uniform(After));
				   true ->
				       ejabberd_c2s:migrate(Pid, Node,
							    random:uniform(After))
				end;
			    _ -> ok
			  end
		  end,
		  Ss).

-spec node_up(atom()) -> ok.

node_up(_Node) ->
    copy_sessions(mnesia:dirty_first(session), fun(_) -> true end).

-spec node_down(atom()) -> ok.

node_down(Node) when Node == node() ->
    copy_sessions(mnesia:dirty_first(session),
                  fun ejabberd_c2s:is_remote_socket/1);
node_down(Node) ->
    ets:select_delete(
      session,
      [{#session{sid = {'_', '$1'}, _ = '_'},
        [{'==', {'node', '$1'}, Node}],
        [true]}]).

copy_sessions('$end_of_table', _CheckFun) -> ok;
copy_sessions(Key, CheckFun) ->
    case mnesia:dirty_read(session, Key) of
      [#session{us = US, sid = {_, Pid}} = Session] ->
	  case ejabberd_cluster:get_node_new(US) of
	    Node when node() /= Node ->
                  case CheckFun(Pid) of
                      true ->
                          rpc:cast(Node, mnesia, dirty_write, [Session]);
                      false ->
                          ok
                  end;
	    _ -> ok
	  end;
      _ -> ok
    end,
    copy_sessions(mnesia:dirty_next(session, Key), CheckFun).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    update_tables(),
    mnesia:create_table(session,
			[{ram_copies, [node()]}, {local_content, true},
			 {attributes, record_info(fields, session)}]),
    mnesia:add_table_index(session, usr),
    mnesia:add_table_index(session, us),
    mnesia:add_table_copy(session, node(), ram_copies),
    ets:new(sm_iqtable, [named_table, bag]),
    ejabberd_hooks:add(node_up, ?MODULE, node_up, 100),
    ejabberd_hooks:add(node_down, ?MODULE, node_down, 100),
    ejabberd_hooks:add(node_hash_update, ?MODULE, migrate,
		       100),
    lists:foreach(fun (Host) ->
			  ejabberd_hooks:add(roster_in_subscription, Host,
					     ejabberd_sm, check_in_subscription,
					     20),
			  ejabberd_hooks:add(offline_message_hook, Host,
					     ejabberd_sm,
					     bounce_offline_message, 100),
			  ejabberd_hooks:add(remove_user, Host, ejabberd_sm,
					     disconnect_removed_user, 100)
		  end,
		  ?MYHOSTS),
    ejabberd_commands:register_commands(commands()),
    start_handlers(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~nwhen processing: ~p",
                       [Reason, {From, To, Packet}]);
        _ ->
            ok
    end,
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module,
	     Function},
	    State) ->
    ets:insert(sm_iqtable,
	       {{XMLNS, Host}, Module, Function}),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module, Function, Opts}, State) ->
    ets:insert(sm_iqtable, {{XMLNS, Host}, Module, Function, Opts}),
    if is_pid(Opts) ->
            erlang:monitor(process, Opts);
       true ->
            ok
    end,
    {noreply, State};
handle_info({unregister_iq_handler, Host, XMLNS},
	    State) ->
    case ets:lookup(sm_iqtable, {XMLNS, Host}) of
	[{_, Module, Function, Opts1}|Tail] when is_pid(Opts1) ->
            Opts = [Opts1 | [Pid || {_, _, _, Pid} <- Tail]],
	    gen_iq_handler:stop_iq_handler(Module, Function, Opts);
	_ ->
	    ok
    end,
    ets:delete(sm_iqtable, {XMLNS, Host}),
    {noreply, State};
handle_info({'DOWN', _MRef, _Type, Pid, _Info}, State) ->
    Rs = ets:select(sm_iqtable,
                    [{{'_','_','_','$1'},
                      [{'==', '$1', Pid}],
                      ['$_']}]),
    lists:foreach(fun(R) -> ets:delete_object(sm_iqtable, R) end, Rs),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_hooks:delete(node_up, ?MODULE, node_up, 100),
    ejabberd_hooks:delete(node_down, ?MODULE, node_down,
			  100),
    ejabberd_hooks:delete(node_hash_update, ?MODULE,
			  migrate, 100),
    ejabberd_commands:unregister_commands(commands()),
    stop_handlers(),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec set_session(sid(), binary(), binary(), binary(),
                  prio(), info()) -> ok.

set_session({_, Pid} = SID, User, Server, Resource,
	    Priority, Info) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},
    F = fun () ->
		mnesia:write(#session{sid = SID, usr = USR, us = US,
				      priority = Priority, info = Info})
	end,
    mnesia:sync_dirty(F),
    case ejabberd_cluster:get_node_new(US) of
      Node when node() /= Node ->
	  rpc:cast(Node, mnesia, dirty_write,
		   [#session{sid = SID, usr = USR, us = US,
			     priority = Priority, info = Info}]),
	  case ejabberd_cluster:get_node(US) of
	    Node when node() /= Node ->
		ejabberd_c2s:migrate(Pid, Node, 0);
	    _ -> ok
	  end;
      _ -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_route(From, To, Packet) ->
    ?DEBUG("session manager~n\tfrom ~p~n\tto ~p~n\tpacket "
	   "~P~n",
	   [From, To, Packet, 8]),
    {U, S, _} = jlib:jid_tolower(To),
    case ejabberd_cluster:get_node({U, S}) of
	Node when Node /= node() ->
	    {?MODULE, Node} ! {route, From, To, Packet};
	_ ->
	    dispatch(From, To, Packet)
    end.

do_route1(From, To, {broadcast, _} = Packet) ->
    case To#jid.lresource of
        <<"">> ->
            lists:foreach(fun (R) ->
                                  do_route(From,
                                           jlib:jid_replace_resource(To, R),
                                           Packet)
                          end,
                          get_user_resources(To#jid.user, To#jid.server));
        _ ->
            USR = jlib:jid_tolower(To),
            case mnesia:dirty_index_read(session, USR, #session.usr) of
                [] ->
                    ?DEBUG("packet droped~n", []);
                Ss ->
                    Session = lists:max(Ss),
                    Pid = element(2, Session#session.sid),
                    ?DEBUG("sending to process ~p~n", [Pid]),
                    Pid ! {route, From, To, Packet}
            end
    end;
do_route1(From, To, Packet) ->
    #jid{user = User, server = Server, luser = LUser,
	 lserver = LServer, lresource = LResource} =
	To,
    #xmlel{name = Name, attrs = Attrs} = Packet,
    case LResource of
      <<"">> ->
	  case Name of
	    <<"presence">> ->
		{Pass, _Subsc} = case xml:get_attr_s(<<"type">>, Attrs)
				     of
				   <<"subscribe">> ->
				       Reason = xml:get_path_s(Packet,
							       [{elem,
								 <<"status">>},
								cdata]),
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   subscribe,
								   Reason]),
					true};
				   <<"subscribed">> ->
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   subscribed,
								   <<"">>]),
					true};
				   <<"unsubscribe">> ->
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   unsubscribe,
								   <<"">>]),
					true};
				   <<"unsubscribed">> ->
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   unsubscribed,
								   <<"">>]),
					true};
				   _ -> {true, false}
				 end,
		if Pass ->
		       PResources = get_user_present_resources(LUser, LServer),
		       lists:foreach(fun ({_, R}) ->
					     do_route(From,
						      jlib:jid_replace_resource(To,
										R),
						      Packet)
				     end,
				     PResources);
		   true -> ok
		end;
	    <<"message">> -> route_message(From, To, Packet);
	    <<"iq">> -> process_iq(From, To, Packet);
	    _ -> ok
	  end;
      _ ->
	  USR = {LUser, LServer, LResource},
	  case mnesia:dirty_index_read(session, USR, #session.usr)
	      of
	    [] ->
		case Name of
		  <<"message">> -> route_message(From, To, Packet);
		  <<"iq">> ->
		      case xml:get_attr_s(<<"type">>, Attrs) of
			<<"error">> -> ok;
			<<"result">> -> ok;
			_ ->
			    Err = jlib:make_error_reply(Packet,
							?ERR_SERVICE_UNAVAILABLE),
			    ejabberd_router:route(To, From, Err)
		      end;
		  _ -> ?DEBUG("packet droped~n", [])
		end;
	    Ss ->
		Session = lists:max(Ss),
		Pid = element(2, Session#session.sid),
		?DEBUG("sending to process ~p~n", [Pid]),
		Pid ! {route, From, To, Packet}
	  end
    end.

is_privacy_allow(From, To, Packet) ->
    User = To#jid.user,
    Server = To#jid.server,
    PrivacyList =
	ejabberd_hooks:run_fold(privacy_get_user_list, Server,
				#userlist{}, [User, Server]),
    is_privacy_allow(From, To, Packet, PrivacyList).

is_privacy_allow(From, To, Packet, PrivacyList) ->
    User = To#jid.user,
    Server = To#jid.server,
    allow ==
      ejabberd_hooks:run_fold(privacy_check_packet, Server,
			      allow,
			      [User, Server, PrivacyList, {From, To, Packet},
			       in]).

route_message(From, To, Packet) ->
    LUser = To#jid.luser,
    LServer = To#jid.lserver,
    PrioRes = get_user_present_resources(LUser, LServer),
    case catch lists:max(PrioRes) of
      {Priority, _R}
	  when is_integer(Priority), Priority >= 0 ->
	  lists:foreach(fun ({P, R}) when P == Priority ->
				LResource = jlib:resourceprep(R),
				USR = {LUser, LServer, LResource},
				case mnesia:dirty_index_read(session, USR,
							     #session.usr)
				    of
				  [] ->
				      ok; % Race condition
				  Ss ->
				      Session = lists:max(Ss),
				      Pid = element(2, Session#session.sid),
				      ?DEBUG("sending to process ~p~n", [Pid]),
				      Pid ! {route, From, To, Packet}
				end;
			    %% Ignore other priority:
			    ({_Prio, _Res}) -> ok
			end,
			PrioRes);
      _ ->
	  case xml:get_tag_attr_s(<<"type">>, Packet) of
	    <<"error">> -> ok;
	    <<"groupchat">> ->
		bounce_offline_message(From, To, Packet);
	    <<"headline">> ->
		bounce_offline_message(From, To, Packet);
	    _ ->
		case ejabberd_auth:is_user_exists(LUser, LServer) of
		  true ->
		      case is_privacy_allow(From, To, Packet) of
			true ->
			    ejabberd_hooks:run(offline_message_hook, LServer,
					       [From, To, Packet]);
			false -> ok
		      end;
		  _ ->
		      Err = jlib:make_error_reply(Packet,
						  ?ERR_SERVICE_UNAVAILABLE),
		      ejabberd_router:route(To, From, Err)
		end
	  end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clean_session_list(Ss) ->
    clean_session_list(lists:keysort(#session.usr, Ss), []).

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

get_user_present_resources(LUser, LServer) ->
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(session, US,
				       #session.us)
	of
      {'EXIT', _Reason} -> [];
      Ss ->
	  [{S#session.priority, element(3, S#session.usr)}
	   || S <- clean_session_list(Ss),
	      is_integer(S#session.priority)]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_for_sessions_to_replace(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    check_existing_resources(LUser, LServer, LResource),
    check_max_sessions(LUser, LServer).

check_existing_resources(LUser, LServer, LResource) ->
    SIDs = get_resource_sessions(LUser, LServer, LResource),
    if SIDs == [] -> ok;
       true ->
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

get_resource_sessions(User, Server, Resource) ->
    USR = {jlib:nodeprep(User), jlib:nameprep(Server),
	   jlib:resourceprep(Resource)},
    mnesia:dirty_select(session,
			[{#session{sid = '$1', usr = USR, _ = '_'}, [],
			  ['$1']}]).

check_max_sessions(LUser, LServer) ->
    SIDs = mnesia:dirty_select(session,
			       [{#session{sid = '$1', us = {LUser, LServer},
					  _ = '_'},
				 [], ['$1']}]),
    MaxSessions = get_max_user_sessions(LUser, LServer),
    if length(SIDs) =< MaxSessions -> ok;
       true -> {_, Pid} = lists:min(SIDs), Pid ! replaced
    end.

get_max_user_sessions(LUser, Host) ->
    case acl:match_rule(Host, max_user_sessions,
			jlib:make_jid(LUser, Host, <<"">>))
	of
      Max when is_integer(Max) -> Max;
      infinity -> infinity;
      _ -> ?MAX_USER_SESSIONS
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_iq(From, To, Packet) ->
    IQ = jlib:iq_query_info(Packet),
    case IQ of
	#iq{xmlns = XMLNS} ->
	    Host = To#jid.lserver,
	    case ets:lookup(sm_iqtable, {XMLNS, Host}) of
		[{_, Module, Function}] ->
		    ResIQ = Module:Function(From, To, IQ),
		    if
			ResIQ /= ignore ->
			    ejabberd_router:route(To, From,
						  jlib:iq_to_xml(ResIQ));
			true ->
			    ok
		    end;
		[{_, Module, Function, Opts1}|Tail] ->
                    Opts = if is_pid(Opts1) ->
                                   [Opts1 |
                                    [Pid || {_, _, _, Pid} <- Tail]];
                              true ->
                                   Opts1
                           end,
		    gen_iq_handler:handle(Host, Module, Function, Opts,
					  From, To, IQ);
		[] ->
		    Err = jlib:make_error_reply(
			    Packet, ?ERR_SERVICE_UNAVAILABLE),
		    ejabberd_router:route(To, From, Err)
	    end;
	reply ->
	    ok;
	_ ->
	    Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
	    ejabberd_router:route(To, From, Err),
	    ok
    end.

-spec force_update_presence({binary(), binary()}) -> any().

force_update_presence({LUser, _LServer} = US) ->
    case catch mnesia:dirty_index_read(session, US,
				       #session.us)
	of
      {'EXIT', _Reason} -> ok;
      Ss ->
	  lists:foreach(fun (#session{sid = {_, Pid}}) ->
				Pid ! {force_update_presence, LUser}
			end,
			Ss)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ejabberd commands

commands() ->
    [#ejabberd_commands{name = connected_users,
			tags = [session],
			desc = "List all established sessions",
			module = ?MODULE, function = connected_users, args = [],
			result = {connected_users, {list, {sessions, string}}}},
     #ejabberd_commands{name = connected_users_number,
			tags = [session, stats],
			desc = "Get the number of established sessions",
			module = ?MODULE, function = connected_users_number,
			args = [], result = {num_sessions, integer}},
     #ejabberd_commands{name = user_resources,
			tags = [session],
			desc = "List user's connected resources",
			module = ?MODULE, function = user_resources,
			args = [{user, string}, {host, string}],
			result = {resources, {list, {resource, string}}}}].

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

get_proc_num() ->
    erlang:system_info(logical_processors).

get_proc_by_hash(Term) ->
    N = erlang:phash2(Term, get_proc_num()) + 1,
    get_proc(N).

get_proc(N) ->
    jlib:binary_to_atom(<<(iolist_to_binary(atom_to_list(?MODULE)))/binary,
			    "_",
			    (iolist_to_binary(integer_to_list(N)))/binary>>).

start_handlers() ->
    N = get_proc_num(),
    lists:foreach(
      fun(I) ->
              ejabberd_sm_handler:start(get_proc(I))
      end, lists:seq(1, N)).

stop_handlers() ->
    N = get_proc_num(),
    lists:foreach(
      fun(I) ->
              ejabberd_sm_handler:stop(get_proc(I))
      end, lists:seq(1, N)).

dispatch(From, To, Packet) ->
    #jid{luser = U, lserver = S} = To,
    ejabberd_sm_handler:route(get_proc_by_hash({U, S}), From, To, Packet).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Update Mnesia tables

update_tables() ->
    case catch mnesia:table_info(session, attributes) of
      [ur, user, node] -> mnesia:delete_table(session);
      [ur, user, pid] -> mnesia:delete_table(session);
      [usr, us, pid] -> mnesia:delete_table(session);
      [sid, usr, us, priority] ->
	  mnesia:delete_table(session);
      [sid, usr, us, priority, info] -> ok;
      {'EXIT', _} -> ok
    end,
    case lists:member(presence, mnesia:system_info(tables))
	of
      true -> mnesia:delete_table(presence);
      false -> ok
    end,
    case lists:member(local_session,
		      mnesia:system_info(tables))
	of
      true -> mnesia:delete_table(local_session);
      false -> ok
    end,
    mnesia:delete_table(session_counter),
    case catch mnesia:table_info(session, local_content) of
      false -> mnesia:delete_table(session);
      _ -> ok
    end.
