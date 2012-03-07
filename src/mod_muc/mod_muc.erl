%%%----------------------------------------------------------------------
%%% File    : mod_muc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC support (XEP-0045)
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
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

%%% Database schema (version / storage / table)
%%%
%%% 2.1.x / mnesia / muc_online_room
%%%  name_host = {Name::string(), Host::string()}
%%%  pid = pid()
%%% 2.1.x / mnesia / muc_registered
%%%  us_host = {{Username::string, Hostname::string()}, Host::string()}}
%%%  nick = string()
%%% 2.1.x / mnesia / muc_room
%%%  name_host = {Name::string(), Host::string()}
%%%  opts = [{Option::atom(), Value::any()}
%%%
%%% 3.0.0-alpha / mnesia / muc_online_room
%%%  name_host = {Name::binary(), Host::binary()}
%%%  pid = pid()
%%% 3.0.0-alpha / mnesia / muc_room_opt
%%%  name_host = {Name::binary(), Host::binary()}
%%%  opt = atom()
%%%  val = atom() | string()
%%% 3.0.0-alpha / mnesia / muc_room_affiliation
%%%  name_host = {Name::binary(), Host::binary()}
%%%  jid = jid()
%%%  affiliation = owner | 
%%%  reason = string()
%%% 3.0.0-alpha / mnesia / muc_registered
%%%  user_host = {User::jid(), MucHost::binary()}}
%%%  nick = binary()
%%%
%%% 3.0.0-alpha / odbc / muc_online_room
%%%  name = text
%%%  host = text
%%%  pid = text
%%% 3.0.0-alpha / odbc / muc_room_opt
%%%  name = text
%%%  host = text
%%%  opt = text
%%% 3.0.0-alpha / mnesia / muc_room_affiliation
%%%  name = text
%%%  host = text
%%%  jid = text
%%%  affiliation = text
%%%  reason = text
%%% 3.0.0-alpha / mnesia / muc_registered
%%%  user = text
%%%  host = text
%%%  nick = text
%%%

-module(mod_muc).
-author('alexey@process-one.net').

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start_link/2,
	 start/2,
	 stop/1,
	 room_destroyed/4,
	 store_room/3,
	 forget_room/2,
	 create_room/5,
	 process_iq_disco_items/4,
	 broadcast_service_message/2,
	 register_room/3,
	 migrate/1,
	 get_vh_rooms/1,
	 remove_host/1,
	 can_use_nick/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("jlib.hrl").


-record(muc_room_opt, {name_host, opt, val}).
-record(muc_room_affiliation, {name_host, jid, affiliation, reason}).
-record(muc_online_room, {name_host, pid}).
-record(muc_registered, {user_host, nick}).

-record(state, {host,
		server_host,
		access,
		history_size,
		default_room_opts,
		room_shaper}).

-define(PROCNAME, ejabberd_mod_muc).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    start_supervisor(HostB),
    Proc = gen_mod:get_module_proc(HostB, ?PROCNAME),
    ChildSpec =
	{Proc,
	 {?MODULE, start_link, [HostB, Opts]},
	 temporary,
	 1000,
	 worker,
	 [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    stop_supervisor(Host),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:delete_child(ejabberd_sup, Proc).

%% This function is called by a room in three situations:
%% A) The owner of the room destroyed it
%% B) The only participant of a temporary room leaves it
%% C) mod_muc:stop was called, and each room is being terminated
%%    In this case, the mod_muc process died before the room processes
%%    So the message sending must be catched
room_destroyed(Host, Room, Pid, ServerHost) when is_binary(Host), 
                                                 is_binary(Room) ->
    catch gen_mod:get_module_proc_existing(ServerHost, ?PROCNAME) !
	{room_destroyed, {Room, Host}, Pid},
    ok.

%% @doc Create a room.
%% If Opts = default, the default room options are used.
%% Else use the passed options as defined in mod_muc_room.
create_room(Host, Name, From, Nick, Opts) ->
    Proc = gen_mod:get_module_proc_existing(Host, ?PROCNAME),
    RoomHost = gen_mod:get_module_opt_host(Host, ?MODULE, "conference.@HOST@"),
    Node = ejabberd_cluster:get_node({Name, RoomHost}),
    gen_server:call({Proc, Node}, {create, Name, From, Nick, Opts}).

store_room(Host, Name, Opts) when is_binary(Host), is_binary(Name) ->
    F = fun() ->
		gen_storage:delete(Host, {muc_room_opt, {Name, Host}}),
		gen_storage:delete(Host, {muc_room_affiliation, {Name, Host}}),

		lists:foreach(
		  fun({affiliations, Affiliations}) ->
			  lists:foreach(
			    fun({JID, Affiliation1}) ->
				    {Affiliation, Reason} =
					case Affiliation1 of
					    {_, _} = A -> A;
					    A when is_atom(A) -> {A, ""}
					end,
				    {Username, Server, Resource} = JID,
				    gen_storage:write(
				      Host,
				      #muc_room_affiliation{name_host = {Name, Host},
							    jid = exmpp_jid:make(Username, Server, Resource),
							    affiliation = Affiliation,
							    reason = Reason})
			    end, Affiliations);
		     ({Opt, Val}) ->
			  ValS = if
				     is_integer(Val) -> integer_to_list(Val);
				     is_binary(Val) -> binary_to_list(Val);
				     is_list(Val) -> Val;
				     is_atom(Val) -> Val
				 end,
			  gen_storage:write(Host,
					    #muc_room_opt{name_host = {Name, Host},
							  opt = Opt, val = ValS})
		  end, Opts)
	end,
    {atomic, ok} = gen_storage:transaction(Host, muc_room_opt, F).

restore_room_internal(Host, Name) ->
    RoomOpts = gen_storage:read(Host, {muc_room_opt, {Name, Host}}),
    Opts = 
	lists:map(
	  fun(#muc_room_opt{opt = Opt, val = Val}) ->
		  Val2 = if
			     is_list(Val) andalso
			     (Opt =:= allow_change_subj orelse
			      Opt =:= allow_query_users orelse
			      Opt =:= allow_private_messages orelse
			      Opt =:= allow_visitor_status orelse
			      Opt =:= allow_visitor_nickchange orelse
			      Opt =:= public orelse
			      Opt =:= public_list orelse
			      Opt =:= persistent orelse
			      Opt =:= moderated orelse
			      Opt =:= members_by_default orelse
			      Opt =:= members_only orelse
			      Opt =:= allow_user_invites orelse
			      Opt =:= password_protected orelse
			      Opt =:= anonymous orelse
			      Opt =:= logging) ->
				 list_to_atom(Val);
			     is_list(Val) andalso
			     Opt =:= max_users ->
				 list_to_integer(Val);
			     true ->
				 Val
			 end,
		  {Opt, Val2}
	  end, RoomOpts),
    RoomAffiliations = gen_storage:read(Host, {muc_room_affiliation,
					       {Name, Host}}),
    Affiliations =
	lists:map(fun(#muc_room_affiliation{jid = JID,
					    affiliation = Affiliation,
					    reason = Reason}) ->
			  A = case Reason of
				  "" -> Affiliation;
				  _ -> {Affiliation, Reason}
			      end,
			  {jlib:short_prepd_jid(JID), A}
		  end, RoomAffiliations),
    [{affiliations, Affiliations} | Opts].

forget_room(Host, Name) when is_binary(Host), is_binary(Name) ->
    F = fun() ->
		gen_storage:delete(Host, {muc_room_opt, {Name, Host}}),
		gen_storage:delete(Host, {muc_room_affiliation, {Name, Host}})
	end,
    gen_storage:transaction(Host, muc_room_opt, F).

process_iq_disco_items(Host, From, To, #iq{lang = Lang} = IQ) ->
    Rsm = jlib:rsm_decode(IQ),
    Res = exmpp_iq:result(IQ, #xmlel{ns = ?NS_DISCO_ITEMS, 
                               name = 'query',
                               children = iq_disco_items(Host, From, Lang, Rsm)}),
    ejabberd_router:route(To,
			  From,
			  exmpp_iq:iq_to_xmlel(Res)).

can_use_nick(_Host, _JID, <<>>)  ->
    false;
can_use_nick(Host, JID, Nick) when is_binary(Host), is_binary(Nick) ->
    case catch gen_storage:dirty_select(
		 Host,
		 muc_registered,
		 [{'=', user_host, {'_', Host}},
		  {'=', nick, Nick}]) of
	{'EXIT', _Reason} ->
	    true;
	[] ->
	    true;
	[#muc_registered{user_host = {U, _Host}}] ->
	    U == exmpp_jid:bare(JID)
    end.

migrate(After) ->
    Rs = mnesia:dirty_select(
	   muc_online_room,
	   [{#muc_online_room{name_host = '$1', pid = '$2', _ = '_'},
	     [],
	     ['$$']}]),
    lists:foreach(
      fun([NameHost, Pid]) ->
	      case ejabberd_cluster:get_node_new(NameHost) of
		  Node when Node /= node() ->
		      mod_muc_room:migrate(Pid, Node, After);
		  _ ->
		      ok
	      end
      end, Rs).

remove_host(MyHostB) when is_binary(MyHostB) ->
    Host = gen_mod:get_module_opt_host(binary_to_list(MyHostB), ?MODULE, "conference.@HOST@"),
    ?INFO_MSG("Removing rooms of MUC service ~p", [Host]),
    lists:foreach(
	fun(#muc_online_room{name_host = {NameB, HostB}, pid = Pid}) ->
	    gen_fsm:send_all_state_event(Pid, destroy),
	    forget_room(HostB, NameB)
	end,
	get_vh_rooms(list_to_binary(Host))).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Opts]) ->
    MyHostStr = gen_mod:get_opt_host(Host, Opts, "conference.@HOST@"),
    MyHost = l2b(MyHostStr),
    Backend = gen_mod:get_opt(backend, Opts, mnesia),
    gen_storage:create_table(Backend, MyHost, muc_room_opt,
			     [{disc_copies, [node()]},
			      {odbc_host, Host},
			      {attributes, record_info(fields, muc_room_opt)},
			      {type, bag},
			      {types, [{name_host, {binary, binary}},
				       {opt, atom}]}]),
    gen_storage:create_table(Backend, MyHost, muc_room_affiliation,
			     [{disc_copies, [node()]},
			      {odbc_host, Host},
			      {attributes, record_info(fields, muc_room_affiliation)},
			      {type, bag},
			      {types, [{name_host, {binary, binary}},
				       {affiliation, atom},
				       {jid, jid}]}]),
    gen_storage:create_table(Backend, MyHost, muc_registered,
			     [{disc_copies, [node()]},
			      {odbc_host, Host},
			      {attributes, record_info(fields, muc_registered)},
			      {types, [{user_host, {jid, text}}, {nick, binary}]}]),
    gen_storage:create_table(Backend, MyHost, muc_online_room,
			     [{ram_copies, [node()]},
			      {odbc_host, Host},
			      {attributes, record_info(fields, muc_online_room)},
			      {types, [{name_host, {binary, binary}},
				       {pid, pid}]}]),
    %% If ejabberd stops abruptly, ODBC table keeps obsolete data. Let's clean:
    gen_storage:dirty_delete_where(MyHost, muc_online_room,
                                  [{'=', name_host, {'_', MyHost}}]),
    gen_storage:add_table_copy(MyHost, muc_online_room, node(), ram_copies),
    catch ets:new(muc_online_users, [bag, named_table, public, {keypos, 2}]),
    gen_storage:add_table_index(MyHost, muc_registered, nick),
    update_tables(MyHost, Backend),
    Access = gen_mod:get_opt(access, Opts, all),
    AccessCreate = gen_mod:get_opt(access_create, Opts, all),
    AccessAdmin = gen_mod:get_opt(access_admin, Opts, none),
    AccessPersistent = gen_mod:get_opt(access_persistent, Opts, all),
    HistorySize = gen_mod:get_opt(history_size, Opts, 20),
    DefRoomOpts = gen_mod:get_opt(default_room_options, Opts, []),
    RoomShaper = gen_mod:get_opt(room_shaper, Opts, none),
    ejabberd_router:register_route(MyHostStr),
    ejabberd_hooks:add(node_hash_update, ?MODULE, migrate, 100),
    ejabberd_hooks:add(remove_host, Host, ?MODULE, remove_host, 50),
    load_permanent_rooms(MyHost, Host,
			 {Access, AccessCreate, AccessAdmin, AccessPersistent},
			 HistorySize,
			 RoomShaper),
    {ok, #state{host = MyHost,
		server_host = Host,
		access = {Access, AccessCreate, AccessAdmin, AccessPersistent},
		default_room_opts = DefRoomOpts,
		history_size = HistorySize,
		room_shaper = RoomShaper}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({create, Room, From, Nick, Opts},
	    _From,
	    #state{host = Host,
		   server_host = ServerHost,
		   access = Access,
		   default_room_opts = DefOpts,
		   history_size = HistorySize,
		   room_shaper = RoomShaper} = State) ->
    ?DEBUG("MUC: create new room '~s'~n", [Room]),
    NewOpts = case Opts of
		  default -> DefOpts;
		  _ -> Opts
	      end,
    {ok, Pid} = mod_muc_room:start(
		  Host, ServerHost, Access,
		  Room, HistorySize,
		  RoomShaper, From,
		  Nick, NewOpts),
    register_room(Host, Room, Pid),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({route, From, To, Packet},
	    #state{server_host = ServerHost,
		   access = Access,
 		   default_room_opts = DefRoomOpts,
		   history_size = HistorySize,
		   room_shaper = RoomShaper} = State) ->
    Host = exmpp_jid:domain(To),
    US = {exmpp_jid:prep_node(To), exmpp_jid:prep_domain(To)},
    case ejabberd_cluster:get_node(US) of
	Node when Node == node() ->
	    case catch do_route(Host, ServerHost, Access, HistorySize,
				RoomShaper, From, To, Packet, DefRoomOpts) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]);
		_ ->
		    ok
	    end;
	Node ->
	    Proc = gen_mod:get_module_proc_existing(ServerHost, ?PROCNAME),
	    {Proc, Node} ! {route, From, To, Packet}
    end,
    {noreply, State};
handle_info({room_destroyed, {_, Host} = RoomHost, Pid}, State) ->
    F = fun() ->
		gen_storage:delete_object(Host,
					  #muc_online_room{name_host = RoomHost,
							   pid = Pid})
	end,
    gen_storage:sync_dirty(Host, muc_online_room, F),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ejabberd_hooks:delete(node_hash_update, ?MODULE, migrate, 100),
    ejabberd_hooks:delete(remove_host, State#state.server_host, ?MODULE, remove_host, 50),
    ejabberd_router:unregister_route(binary_to_list(State#state.host)),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_supervisor(Host) ->
    Proc = gen_mod:get_module_proc(Host, ejabberd_mod_muc_sup),
    ChildSpec =
	{Proc,
	 {ejabberd_tmp_sup, start_link,
	  [Proc, mod_muc_room]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop_supervisor(Host) ->
    Proc = gen_mod:get_module_proc(Host, ejabberd_mod_muc_sup),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

do_route(Host, ServerHost, Access, HistorySize, RoomShaper,
	 From, To, Packet, DefRoomOpts) ->
    {AccessRoute, _AccessCreate, _AccessAdmin, _AccessPersistent} = Access,
    case acl:match_rule(ServerHost, AccessRoute, From) of
	allow ->
	    do_route1(Host, ServerHost, Access, HistorySize, RoomShaper,
		      From, To, Packet, DefRoomOpts);
	_ ->
        Lang = exmpp_stanza:get_lang(Packet),
        ErrText = "Access denied by service policy",
        Err = exmpp_iq:error(Packet,exmpp_stanza:error(Packet#xmlel.ns,
                                                       'forbidden',
                                                       {Lang,ErrText})),
	    ejabberd_router:route_error(To, From, Err, Packet)
    end.

l2b(global) -> global;
l2b(String) when is_list(String) -> list_to_binary(String);
l2b(Other) -> Other.

do_route1(Host, ServerHost, Access, HistorySize, RoomShaper,
	  From, To, Packet, DefRoomOpts) ->
    {_AccessRoute, AccessCreate, AccessAdmin, _AccessPersistent} = Access,
    Room = exmpp_jid:prep_node(To),
    Nick = exmpp_jid:prep_resource(To),
    #xmlel{name = Name} = Packet,
    case Room of
	'undefined' ->
	    case Nick of
		'undefined' ->
		    case Name of
			'iq' ->
			    case exmpp_iq:xmlel_to_iq(Packet) of
				#iq{type = get, ns = ?NS_DISCO_INFO = XMLNS,
 				    payload = _SubEl, lang = Lang} = IQ ->
		    ServerHostB = l2b(ServerHost),
		    Info = ejabberd_hooks:run_fold(
			     disco_info, ServerHostB, [],
			     [ServerHost, ?MODULE, <<>>, ""]),
                    ResPayload = #xmlel{ns = XMLNS, name = 'query',
                                        children = iq_disco_info(Lang)++Info},
                    Res = exmpp_iq:result(IQ, ResPayload),
				    ejabberd_router:route(To,
							  From,
							  exmpp_iq:iq_to_xmlel(Res));
				#iq{type = get,
				    ns = ?NS_DISCO_ITEMS} = IQ ->
				    spawn(?MODULE,
					  process_iq_disco_items,
					  [Host, From, To, IQ]);
				#iq{type = get,
				    ns = ?NS_INBAND_REGISTER = XMLNS,
				    lang = Lang} = IQ ->
                    ResPayload = #xmlel{ns = XMLNS, name = 'query',
                                        children = iq_get_register_info(Host, 
                                                                    From,
                                                                    Lang)},
                    Res = exmpp_iq:result(IQ,ResPayload),
				    ejabberd_router:route(To,
							  From,
							  exmpp_iq:iq_to_xmlel(Res));
				#iq{type = set,
				    ns = ?NS_INBAND_REGISTER ,
				    lang = Lang,
				    payload = SubEl} = IQ ->
				    case process_iq_register_set(Host, From, SubEl, Lang) of
					ok ->
                        Res = exmpp_iq:result(IQ),
					    ejabberd_router:route(
					      To, From, exmpp_iq:iq_to_xmlel(Res));
					{error, Error} ->
					    Err = exmpp_iq:error(IQ,Error),
					    ejabberd_router:route(
					      To, From, exmpp_iq:iq_to_xmlel(Err))
				    end;
				#iq{type = get,
				    ns = ?NS_VCARD,
				    lang = Lang} = IQ ->
                    Res = exmpp_iq:result(IQ,iq_get_vcard(Lang)),
				    ejabberd_router:route(To,
							  From,
							  exmpp_iq:iq_to_xmlel(Res));
				#iq{type = get,
				   ns = ?NS_MUC_UNIQUE} = IQ ->
				   Res = exmpp_iq:result(IQ, iq_get_unique_el(From)),
				   ejabberd_router:route(To,
				   			 From,
							  exmpp_iq:iq_to_xmlel(Res));
				#iq{} = IQ ->
				    Err = exmpp_iq:error(IQ,'feature-not-implemented'),
				    ejabberd_router:route(To, From, Err)
			    end;
			'message' ->
			    case exmpp_xml:get_attribute_as_list(Packet,<<"type">>, "chat") of
				"error" ->
				    ok;
				_ ->
				    case acl:match_rule(ServerHost, AccessAdmin, From) of
					allow ->
					    Msg = exmpp_xml:get_path(Packet, 
                                                 [{element,'body'},cdata]),
					    broadcast_service_message(Host, Msg);
					_ ->
					    Lang = exmpp_stanza:get_lang(Packet),
					    ErrText = "Only service administrators "
						      "are allowed to send service messages",
                        Err = exmpp_iq:error(Packet,exmpp_stanza:error(Packet#xmlel.ns,
                                                       'forbidden',
                                                       {Lang,ErrText})),
					    ejabberd_router:route(
					      To, From, Err)
				    end
			    end;
			'presence' ->
			    ok
		    end;
		_ ->
		    case exmpp_stanza:get_type(Packet) of
			<<"error">> ->
			    ok;
			<<"result">> ->
			    ok;
			_ ->
			    Err = exmpp_iq:error(Packet,'item-not-found'),
			    ejabberd_router:route(To, From, Err)
		    end
	    end;
	_ ->
	    case gen_storage:dirty_read(Host, muc_online_room, {Room, Host}) of
		[] ->
		    Type = exmpp_stanza:get_type(Packet),
		    case {Name, Type} of
			{'presence', 'undefined'} ->
			    case check_user_can_create_room(ServerHost,
							    AccessCreate, From,
							    Room) of
				true ->
				    {ok, Pid} = start_new_room(
						  Host, ServerHost, Access,
						  Room, HistorySize,
						  RoomShaper, From,
						  Nick, DefRoomOpts),
				    register_room(Host, Room, Pid),
				    mod_muc_room:route(Pid, From, Nick, Packet),
				    ok;
				false ->
				    Lang = exmpp_stanza:get_lang(Packet),
				    ErrText = "Room creation is denied by service policy",
                                    Err = exmpp_stanza:reply_with_error(Packet,exmpp_stanza:error(Packet#xmlel.ns,
                                                       'forbidden',
                                                       {Lang,ErrText})),
				    ejabberd_router:route(To, From, Err)
			    end;
			_ ->
                Lang = exmpp_stanza:get_lang(Packet),
		ErrText = "Room does not exist",
                Err = exmpp_stanza:reply_with_error(Packet,
                                        exmpp_stanza:error(Packet#xmlel.ns,
                                                          'item-not-found',
                                                           {Lang,ErrText})),
			    ejabberd_router:route(To, From, Err)
		    end;
		[R] ->
		    Pid = R#muc_online_room.pid,
		    ?DEBUG("MUC: send to process ~p~n", [Pid]),
		    mod_muc_room:route(Pid, From, Nick, Packet),
		    ok
	    end
    end.

check_user_can_create_room(ServerHost, AccessCreate, From, RoomID) ->
    case acl:match_rule(ServerHost, AccessCreate, From) of
	allow ->
	    (size(RoomID) =< gen_mod:get_module_opt(ServerHost, mod_muc,
						      max_room_id, infinite));
	_ ->
	    false
    end.


load_permanent_rooms({global, Prefix}, global, Access, HistorySize, RoomShaper) ->
    timer:sleep(2000), %% Wait for ejabberd_hosts to start
    lists:foreach(
	fun(ServerHost) ->
	    Host = list_to_binary(Prefix++"."++ServerHost),
	    load_permanent_rooms(Host, global, Access, HistorySize, RoomShaper)
	end,
	?MYHOSTS);

load_permanent_rooms(Host, ServerHost1, Access, HistorySize, RoomShaper) ->
     F = fun() ->
 		Rs = gen_storage:select(Host, muc_room_opt,
					[{'=', opt, persistent},
					 {'=', name_host, {'_', Host}}]),
 		Names = lists:foldl(
 			  fun(#muc_room_opt{name_host = {Room, _}}, Names) ->
 				  sets:add_element(Room, Names)
 			  end, sets:new(), Rs),
 		lists:foreach(
 		  fun(Room) ->
		      case ejabberd_cluster:get_node({Room, Host}) of
			  Node when Node == node() ->
 			      case gen_storage:read(Host, {muc_online_room, {Room, Host}}) of
				  [] ->
				      Opts = restore_room_internal(Host, Room),
				      {ok, Pid} = mod_muc_room:start(
						    Host,
						    ServerHost1,
						    Access,
						    Room,
						    HistorySize,
						    RoomShaper,
						    Opts),
 				      register_room_internal(Host, Room, Pid);
				  _ ->
				      ok
			      end;
			  _ ->
			      ok
		      end
 		  end, sets:to_list(Names))
 	end,
    {atomic, ok} = gen_storage:transaction(Host, muc_room_opt, F).

start_new_room(Host, ServerHost, Access, Room,
	       HistorySize, RoomShaper, From,
	       Nick, DefRoomOpts) ->
    case gen_storage:dirty_read(Host, {muc_room_opt, {Room, Host}}) of
	[] ->
	    ?DEBUG("MUC: open new room '~s'~n", [Room]),
	    mod_muc_room:start(Host, ServerHost, Access,
			       Room, HistorySize,
			       RoomShaper, From,
			       Nick, DefRoomOpts);
	Opts when is_list(Opts) ->
	    ?DEBUG("MUC: restore room '~s'~n", [Room]),
	    mod_muc_room:start(Host, ServerHost, Access,
			       Room, HistorySize,
			       RoomShaper, Opts)
    end.

register_room_internal(Host, Room, Pid) ->
    gen_storage:write(Host,
		      #muc_online_room{name_host = {Room, Host},
				       pid = Pid}).

register_room(Host, Room, Pid) ->
    F = fun() ->
		register_room_internal(Host, Room, Pid)
	end,
    gen_storage:sync_dirty(Host, muc_online_room, F).


iq_disco_info(Lang) ->
    [#xmlel{ns = ?NS_DISCO_INFO, name = 'identity',
           attrs = [?XMLATTR(<<"category">>, 
                             <<"conference">>),
                    ?XMLATTR(<<"type">>, 
                             <<"text">>),
                    ?XMLATTR(<<"name">>, 
                             translate:translate(Lang, "Rooms"))]},
     #xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = 
                              [?XMLATTR(<<"var">>, 
                                       ?NS_DISCO_INFO_s)]},
     #xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = 
                              [?XMLATTR(<<"var">>, 
                                       ?NS_DISCO_ITEMS_s)]},
     #xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = 
                              [?XMLATTR(<<"var">>, 
                                       ?NS_MUC_s)]},
     #xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = 
                              [?XMLATTR(<<"var">>, 
                                       ?NS_MUC_UNIQUE_s)]},
     #xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = 
                              [?XMLATTR(<<"var">>, 
                                        ?NS_INBAND_REGISTER_s)]},
     #xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = 
                              [?XMLATTR(<<"var">>, 
                                        ?NS_RSM_s)]},
     #xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = 
                              [?XMLATTR(<<"var">>, 
                                        ?NS_VCARD_s)]}].


iq_disco_items(Host, From, Lang, none) when is_binary(Host) ->
    lists:zf(fun(#muc_online_room{name_host = {Name, _Host}, pid = Pid}) ->
		     case catch gen_fsm:sync_send_all_state_event(
				  Pid, {get_disco_item, From, Lang}, 100) of
			 {item, Desc} ->
			     flush(),
			     {true,
			      {xmlelement, "item",
			       [{"jid", exmpp_jid:to_list(Name, Host, "")},
				{"name", Desc}], []}};
			 _ ->
			     false
		     end
	     end, get_vh_rooms(Host));

iq_disco_items(Host, From, Lang, Rsm) ->
    {Rooms, RsmO} = get_vh_rooms(Host, Rsm),
    RsmOut = jlib:rsm_encode(RsmO),
    lists:zf(fun(#muc_online_room{name_host = {Name, _Host}, pid = Pid}) ->
		     case catch gen_fsm:sync_send_all_state_event(
				  Pid, {get_disco_item, From, Lang}, 100) of
			 {item, Desc} ->
			     flush(),
			     {true,
                  #xmlel{name = 'item',
                         attrs = [?XMLATTR(<<"jid">>, 
				 exmpp_jid:to_binary(exmpp_jid:make(Name, Host))),
                                 ?XMLATTR(<<"name">>,
                                          Desc)]}};
			 _ ->
			     false
		     end
	     end, Rooms) ++ RsmOut.

get_vh_rooms(Host, #rsm_in{max=M, direction=Direction, id=I, index=Index})->
    AllRooms = get_vh_rooms_all_nodes(Host),
    Count = erlang:length(AllRooms),
    L = get_vh_rooms_direction(Direction, I, Index, AllRooms),
    L2 = if
	     Index == undefined andalso Direction == before ->
		 lists:reverse(lists:sublist(lists:reverse(L), 1, M));
	     Index == undefined ->
		 lists:sublist(L, 1, M);
	     Index > Count  orelse Index < 0 ->
		 [];
	     true ->
		 lists:sublist(L, Index+1, M)
	 end,
    if
	L2 == [] ->
	    {L2, #rsm_out{count=Count}};
	true ->
	    H = hd(L2),
	    NewIndex = get_room_pos(H, AllRooms),
	    T=lists:last(L2),
	    {F, _}=H#muc_online_room.name_host,
	    {Last, _}=T#muc_online_room.name_host,
	    {L2, #rsm_out{first=F, last=Last, count=Count, index=NewIndex}}
    end.

get_vh_rooms_direction(_Direction, _I, Index, AllRooms) when Index =/= undefined ->
		AllRooms;
get_vh_rooms_direction(aft, I, _Index, AllRooms) ->
    {_Before, After} =
	lists:splitwith(
	  fun(#muc_online_room{name_host = {Na, _}}) ->
		  Na < I end, AllRooms),
    case After of
	[] -> [];
	[#muc_online_room{name_host = {I, _Host}} | AfterTail] -> AfterTail;
	_ -> After
    end;
get_vh_rooms_direction(before, I, _Index, AllRooms) when I =/= []->
    {Before, _} =
	lists:splitwith(
	  fun(#muc_online_room{name_host = {Na, _}}) ->
		  Na < I end, AllRooms),
    Before;
get_vh_rooms_direction(_Direction, _I, _Index, AllRooms) ->
    AllRooms.

%% @doc Return the position of desired room in the list of rooms.
%% The room must exist in the list. The count starts in 0.
%% @spec (Desired::muc_online_room(), Rooms::[muc_online_room()]) -> integer()
get_room_pos(Desired, Rooms) ->
    get_room_pos(Desired, Rooms, 0).
get_room_pos(Desired, [HeadRoom | _], HeadPosition)
  when (Desired#muc_online_room.name_host ==
	HeadRoom#muc_online_room.name_host) ->
    HeadPosition;
get_room_pos(Desired, [_ | Rooms], HeadPosition) ->
    get_room_pos(Desired, Rooms, HeadPosition + 1).

flush() ->
    receive
	_ ->
	    flush()
    after 0 ->
	    ok
    end.

-define(XFIELD(Type, Label, Var, Val),
	#xmlel{name = "field",
          attrs = [?XMLATTR(<<"type">>, Type),
			       ?XMLATTR(<<"label">>, 
                            translate:translate(Lang, Label)),
			       ?XMLATTR(<<"var">>, Var)],
          children = [#xmlel{name = 'value',
                             children = [#xmlcdata{cdata = Val}]}]}).

iq_get_register_info(Host, From, Lang)  ->
    FromBare = exmpp_jid:bare(From),
    {Nick, Registered} =
	case catch gen_storage:dirty_read(Host, muc_registered, {FromBare, Host}) of
	    {'EXIT', _Reason} ->
		{"", []};
	    [] ->
		{"", []};
	    [#muc_registered{nick = N}] ->
		{N, [#xmlel{name = 'registered'}]}
	end,
    Registered ++
    [#xmlel{name = 'instructions' ,
        children = [#xmlcdata{cdata = 
              	    list_to_binary(translate:translate(Lang,
	            "You need an x:data capable client to register nickname"))}]},
    #xmlel{ns = ?NS_DATA_FORMS, name = 'x',
        children = [
            #xmlel{ns = ?NS_DATA_FORMS, name = 'title',
                        children = [#xmlcdata{cdata = 
	       list_to_binary(translate:translate(Lang, "Nickname Registration at ") ++ Host)}]},
           #xmlel{ns = ?NS_DATA_FORMS, name = 'instructions',
                 children = [#xmlcdata{cdata = 
	       translate:translate(Lang, "Enter nickname you want to register")}]},
	   ?XFIELD(<<"text-single">>, "Nickname", <<"nick">>, Nick)]}].



iq_set_register_info(Host, From, Nick, Lang) when is_binary(Host), is_binary(Nick) ->
    FromBare = exmpp_jid:bare(From),
    F = fun() ->
		case Nick of
		    <<>> ->
			gen_storage:delete(Host, {muc_registered, {FromBare, Host}}),
			ok;
		    _ ->
			Allow =
			    case gen_storage:select(
				   Host,
				   muc_registered,
				   [{'=', user_host, {'_', Host}},
				    {'=', nick, Nick}]) of
				[] ->
				    true;
				[#muc_registered{user_host = {U, _Host}}] ->
				    U == FromBare
			    end,
			if
			    Allow ->
				gen_storage:write(
				  Host,
				  #muc_registered{user_host = {FromBare, Host},
						  nick = Nick}),
				ok;
			    true ->
				false
			end
		end
	end,
    case gen_storage:transaction(Host, muc_registered, F) of
	{atomic, ok} ->
	    ok;
	{atomic, false} ->
	    ErrText = "That nickname is registered by another person",
        %%TODO: Always in the jabber:client namespace?
	    {error,exmpp_stanza:error(?NS_JABBER_CLIENT, 
                                  'conflict', 
                                  {Lang, ErrText})};
	_ ->
	    {error, 'internal-server-error'}
    end.

process_iq_register_set(Host, From, SubEl, Lang) ->
%    {xmlelement, _Name, _Attrs, Els} = SubEl,
    case exmpp_xml:get_element(SubEl,'remove') of
	undefined ->
	    case exmpp_xml:get_child_elements(SubEl) of
		[#xmlel{ns= NS, name = 'x'} = XEl] ->
		    case {NS, exmpp_stanza:get_type(XEl)} of
            {?NS_DATA_FORMS, <<"cancel">>} ->
			    ok;
			{?NS_DATA_FORMS, <<"submit">>} ->
			    XData = jlib:parse_xdata_submit(XEl),
			    case XData of
				invalid ->
				    {error, 'bad-request'};
				_ ->
				    case lists:keysearch("nick", 1, XData) of
					{value, {_, [Nick]}} ->
					    iq_set_register_info(Host, From, list_to_binary(Nick), Lang);
					_ ->
					    ErrText = "You must fill in field \"Nickname\" in the form",
                        Err = exmpp_stanza:error(SubEl#xmlel.ns, 
                                   'not-acceptable', 
                                   {Lang, translate:translate(Lang,ErrText)}),
					    {error, Err}
				    end
			    end;
			_ ->
			    {error, 'bad-request'}
		    end;
		_ ->
		    {error, 'bad-request'}
	    end;
	_ ->
	    iq_set_register_info(Host, From, <<>>, Lang)
    end.

iq_get_vcard(Lang) ->
    #xmlel{ns = ?NS_VCARD, name = 'vCard',
           children =
        [#xmlel{ns = ?NS_VCARD, name = 'FN',
            children = [#xmlcdata{cdata = <<"ejabberd/mod_muc">>}]},
         #xmlel{ns = ?NS_VCARD, name = 'URL',
            children = [#xmlcdata{cdata = list_to_binary(?EJABBERD_URI)}]},
         #xmlel{ns = ?NS_VCARD, name = 'DESC',
            children = [#xmlcdata{cdata = 
                    list_to_binary(translate:translate(Lang, "ejabberd MUC module") ++
                	  "\nCopyright (c) 2002-2012 ProcessOne")}]}]}.

iq_get_unique_el(From) ->
    #xmlel{ns = ?NS_MUC_UNIQUE, name = 'unique',
            children = [#xmlcdata{cdata = list_to_binary(iq_get_unique_name(From))}]}.

%% @doc Get a pseudo unique Room Name. The Room Name is generated as a hash of 
%%      the requester JID, the local time and a random salt.
%%
%%      "pseudo" because we don't verify that there is not a room
%%       with the returned Name already created, nor mark the generated Name 
%%       as "already used".  But in practice, it is unique enough. See
%%       http://xmpp.org/extensions/xep-0045.html#createroom-unique
iq_get_unique_name(From) ->
	sha:sha(term_to_binary([From, now(), randoms:get_string()])).

broadcast_service_message(Host, Msg) ->
    lists:foreach(
      fun(#muc_online_room{pid = Pid}) ->
	      gen_fsm:send_all_state_event(
		Pid, {service_message, Msg})
      end, get_vh_rooms_all_nodes(Host)).

get_vh_rooms_all_nodes(Host) ->
    Rooms = lists:foldl(
	      fun(Node, Acc) when Node == node() ->
		      get_vh_rooms(Host) ++ Acc;
		 (Node, Acc) ->
		      case catch rpc:call(Node, ?MODULE, get_vh_rooms,
					  [Host], 5000) of
			  Res when is_list(Res) ->
			      Res ++ Acc;
			  _ ->
			      Acc
		      end
	      end, [], ejabberd_cluster:get_nodes()),
    lists:ukeysort(#muc_online_room.name_host, Rooms).

get_vh_rooms(Host) when is_binary(Host) ->
    gen_storage:dirty_select(Host, muc_online_room,
			     [{'=', name_host, {'_', Host}}]).

%%%%%
%%%%% Database migration from ejabberd 2.1.x
%%%%%

update_tables(global, Storage) ->
    [update_tables(HostB, Storage) || HostB <- ejabberd_hosts:get_hosts(ejabberd)];

update_tables(HostB, mnesia) ->
    gen_storage_migration:migrate_mnesia(
      HostB, muc_registered,
      [{muc_registered, [us_host, nick],
	fun({muc_registered, {{UserS, ServerS}, HostS}, NickS}) ->
		#muc_registered{user_host = {exmpp_jid:make(UserS, ServerS),
					     list_to_binary(HostS)},
				nick = list_to_binary(NickS)}
	end}]),
    gen_storage_migration:migrate_mnesia(
      HostB, muc_room_opt,
      [{muc_room, [name_host, opts],
	fun({muc_room, {NameString, HostString}, Options}) ->
		NameHost = {list_to_binary(NameString),
			    list_to_binary(HostString)},
		lists:foreach(
		  fun({affiliations, Affiliations}) ->
			  lists:foreach(
			    fun({{Username, Hostname, Resource}, AffiliationIni}) ->
				    UserJid = exmpp_jid:make(Username, Hostname, Resource),
				    {Affiliation, Reason} = case AffiliationIni of
								Aff when is_atom(Aff) -> {Aff, ""};
								{Aff, Reas} -> {Aff, Reas}
							    end,
				    gen_storage:write(HostB,
						      #muc_room_affiliation{name_host = NameHost,
									    jid = UserJid,
									    affiliation = Affiliation,
									    reason = Reason})
			    end, Affiliations);
		     ({Opt, Val}) ->
			  gen_storage:write(HostB,
					    #muc_room_opt{name_host = NameHost,
							  opt = Opt,
							  val = Val})
		  end, Options)
	end}]);

update_tables(_HostB, odbc) ->
    ok.
