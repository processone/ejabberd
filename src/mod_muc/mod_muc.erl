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

-module(mod_muc).
-author('alexey@process-one.net').

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start_link/2,
	 start/2,
	 stop/1,
	 room_destroyed/4,
	 store_room/4,
	 restore_room/3,
	 forget_room/3,
	 create_room/5,
	 process_iq_disco_items/4,
	 broadcast_service_message/2,
	 can_use_nick/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").


-record(muc_room, {name_host, opts}).
-record(muc_online_room, {name_host, pid}).
-record(muc_registered, {us_host, nick}).

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

start(Host, Opts) ->
    start_supervisor(Host),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
	{Proc,
	 {?MODULE, start_link, [Host, Opts]},
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
room_destroyed(Host, Room, Pid, ServerHost) ->
    catch gen_mod:get_module_proc(ServerHost, ?PROCNAME) !
	{room_destroyed, {Room, Host}, Pid},
    ok.

%% @doc Create a room.
%% If Opts = default, the default room options are used.
%% Else use the passed options as defined in mod_muc_room.
create_room(Host, Name, From, Nick, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, {create, Name, From, Nick, Opts}).

store_room(ServerHost, Host, Name, Opts) ->
    LServer = jlib:nameprep(ServerHost),
    store_room(LServer, Host, Name, Opts, gen_mod:db_type(LServer, ?MODULE)).

store_room(_LServer, Host, Name, Opts, mnesia) ->
    F = fun() ->
		mnesia:write(#muc_room{name_host = {Name, Host},
				       opts = Opts})
	end,
    mnesia:transaction(F);
store_room(LServer, Host, Name, Opts, odbc) ->
    SName = ejabberd_odbc:escape(Name),
    SHost = ejabberd_odbc:escape(Host),
    SOpts = ejabberd_odbc:encode_term(Opts),
    F = fun() ->
                odbc_queries:update_t(
                  "muc_room",
                  ["name", "host", "opts"],
                  [SName, SHost, SOpts],
                  ["name='", SName, "' and host='", SHost, "'"])
	end,
    ejabberd_odbc:sql_transaction(LServer, F).

restore_room(ServerHost, Host, Name) ->
    LServer = jlib:nameprep(ServerHost),
    restore_room(LServer, Host, Name, gen_mod:db_type(LServer, ?MODULE)).

restore_room(_LServer, Host, Name, mnesia) ->
    case catch mnesia:dirty_read(muc_room, {Name, Host}) of
	[#muc_room{opts = Opts}] ->
	    Opts;
	_ ->
	    error
    end;
restore_room(LServer, Host, Name, odbc) ->
    SName = ejabberd_odbc:escape(Name),
    SHost = ejabberd_odbc:escape(Host),
    case catch ejabberd_odbc:sql_query(
                 LServer, ["select opts from muc_room where name='",
                           SName, "' and host='", SHost, "';"]) of
        {selected, ["opts"], [{Opts}]} ->
            ejabberd_odbc:decode_term(Opts);
        _ ->
            error
    end.

forget_room(ServerHost, Host, Name) ->
    LServer = jlib:nameprep(ServerHost),
    forget_room(LServer, Host, Name, gen_mod:db_type(LServer, ?MODULE)).

forget_room(_LServer, Host, Name, mnesia) ->
    F = fun() ->
		mnesia:delete({muc_room, {Name, Host}})
	end,
    mnesia:transaction(F);
forget_room(LServer, Host, Name, odbc) ->
    SName = ejabberd_odbc:escape(Name),
    SHost = ejabberd_odbc:escape(Host),
    F = fun() ->
		ejabberd_odbc:sql_query_t(
                  ["delete from muc_room where name='",
                   SName, "' and host='", SHost, "';"])
	end,
    ejabberd_odbc:sql_transaction(LServer, F).

process_iq_disco_items(Host, From, To, #iq{lang = Lang} = IQ) ->
    Rsm = jlib:rsm_decode(IQ),
    Res = IQ#iq{type = result,
		sub_el = [{xmlelement, "query",
			   [{"xmlns", ?NS_DISCO_ITEMS}],
			   iq_disco_items(Host, From, Lang, Rsm)}]},
    ejabberd_router:route(To,
			  From,
			  jlib:iq_to_xml(Res)).

can_use_nick(_ServerHost, _Host, _JID, "") ->
    false;
can_use_nick(ServerHost, Host, JID, Nick) ->
    LServer = jlib:nameprep(ServerHost),
    can_use_nick(LServer, Host, JID, Nick, gen_mod:db_type(LServer, ?MODULE)).

can_use_nick(_LServer, Host, JID, Nick, mnesia) ->
    {LUser, LServer, _} = jlib:jid_tolower(JID),
    LUS = {LUser, LServer},
    case catch mnesia:dirty_select(
		 muc_registered,
		 [{#muc_registered{us_host = '$1',
				   nick = Nick,
				   _ = '_'},
		   [{'==', {element, 2, '$1'}, Host}],
		   ['$_']}]) of
	{'EXIT', _Reason} ->
	    true;
	[] ->
	    true;
	[#muc_registered{us_host = {U, _Host}}] ->
	    U == LUS
    end;
can_use_nick(LServer, Host, JID, Nick, odbc) ->
    SJID = jlib:jid_to_string(
             jlib:jid_tolower(
               jlib:jid_remove_resource(JID))),
    SNick = ejabberd_odbc:escape(Nick),
    SHost = ejabberd_odbc:escape(Host),
    case catch ejabberd_odbc:sql_query(
                 LServer, ["select jid from muc_registered ",
                           "where nick='", SNick, "' and host='",
                           SHost, "';"]) of
        {selected, ["jid"], [{SJID1}]} ->
            SJID == SJID1;
        _ ->
            true
    end.

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
    MyHost = gen_mod:get_opt_host(Host, Opts, "conference.@HOST@"),
    case gen_mod:db_type(Opts) of
        mnesia ->
            mnesia:create_table(muc_room,
                                [{disc_copies, [node()]},
                                 {attributes,
                                  record_info(fields, muc_room)}]),
            mnesia:create_table(muc_registered,
                                [{disc_copies, [node()]},
                                 {attributes,
                                  record_info(fields, muc_registered)}]),
            update_tables(MyHost),
            mnesia:add_table_index(muc_registered, nick);
        _ ->
            ok
    end,
    mnesia:create_table(muc_online_room,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, muc_online_room)}]),
    mnesia:add_table_copy(muc_online_room, node(), ram_copies),
    catch ets:new(muc_online_users, [bag, named_table, public, {keypos, 2}]),
    clean_table_from_bad_node(node(), MyHost),
    mnesia:subscribe(system),
    Access = gen_mod:get_opt(access, Opts, all),
    AccessCreate = gen_mod:get_opt(access_create, Opts, all),
    AccessAdmin = gen_mod:get_opt(access_admin, Opts, none),
    AccessPersistent = gen_mod:get_opt(access_persistent, Opts, all),
    HistorySize = gen_mod:get_opt(history_size, Opts, 20),
    DefRoomOpts = gen_mod:get_opt(default_room_options, Opts, []),
    RoomShaper = gen_mod:get_opt(room_shaper, Opts, none),
    ejabberd_router:register_route(MyHost),
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
	    #state{host = Host,
		   server_host = ServerHost,
		   access = Access,
 		   default_room_opts = DefRoomOpts,
		   history_size = HistorySize,
		   room_shaper = RoomShaper} = State) ->
    case catch do_route(Host, ServerHost, Access, HistorySize, RoomShaper,
			From, To, Packet, DefRoomOpts) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	_ ->
	    ok
    end,
    {noreply, State};
handle_info({room_destroyed, RoomHost, Pid}, State) ->
    F = fun() ->
		mnesia:delete_object(#muc_online_room{name_host = RoomHost,
						      pid = Pid})
	end,
    mnesia:transaction(F),
    {noreply, State};
handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    clean_table_from_bad_node(Node),
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
    ejabberd_router:unregister_route(State#state.host),
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
	    {xmlelement, _Name, Attrs, _Els} = Packet,
	    Lang = xml:get_attr_s("xml:lang", Attrs),
	    ErrText = "Access denied by service policy",
	    Err = jlib:make_error_reply(Packet,
					?ERRT_FORBIDDEN(Lang, ErrText)),
	    ejabberd_router:route_error(To, From, Err, Packet)
    end.


do_route1(Host, ServerHost, Access, HistorySize, RoomShaper,
	  From, To, Packet, DefRoomOpts) ->
    {_AccessRoute, AccessCreate, AccessAdmin, _AccessPersistent} = Access,
    {Room, _, Nick} = jlib:jid_tolower(To),
    {xmlelement, Name, Attrs, _Els} = Packet,
    case Room of
	"" ->
	    case Nick of
		"" ->
		    case Name of
			"iq" ->
			    case jlib:iq_query_info(Packet) of
				#iq{type = get, xmlns = ?NS_DISCO_INFO = XMLNS,
 				    sub_el = _SubEl, lang = Lang} = IQ ->
				    Info = ejabberd_hooks:run_fold(
					     disco_info, ServerHost, [],
					     [ServerHost, ?MODULE, "", ""]),
				    Res = IQ#iq{type = result,
						sub_el = [{xmlelement, "query",
							   [{"xmlns", XMLNS}],
							   iq_disco_info(Lang)
							   ++Info}]},
				    ejabberd_router:route(To,
							  From,
							  jlib:iq_to_xml(Res));
				#iq{type = get,
				    xmlns = ?NS_DISCO_ITEMS} = IQ ->
				    spawn(?MODULE,
					  process_iq_disco_items,
					  [Host, From, To, IQ]);
				#iq{type = get,
				    xmlns = ?NS_REGISTER = XMLNS,
				    lang = Lang,
				    sub_el = _SubEl} = IQ ->
				    Res = IQ#iq{type = result,
						sub_el =
						[{xmlelement, "query",
						  [{"xmlns", XMLNS}],
						  iq_get_register_info(
						    ServerHost, Host, From, Lang)}]},
				    ejabberd_router:route(To,
							  From,
							  jlib:iq_to_xml(Res));
				#iq{type = set,
				    xmlns = ?NS_REGISTER = XMLNS,
				    lang = Lang,
				    sub_el = SubEl} = IQ ->
				    case process_iq_register_set(
                                           ServerHost, Host, From, SubEl, Lang) of
					{result, IQRes} ->
					    Res = IQ#iq{type = result,
							sub_el =
							[{xmlelement, "query",
							  [{"xmlns", XMLNS}],
							  IQRes}]},
					    ejabberd_router:route(
					      To, From, jlib:iq_to_xml(Res));
					{error, Error} ->
					    Err = jlib:make_error_reply(
						    Packet, Error),
					    ejabberd_router:route(
					      To, From, Err)
				    end;
				#iq{type = get,
				    xmlns = ?NS_VCARD = XMLNS,
				    lang = Lang,
				    sub_el = _SubEl} = IQ ->
				    Res = IQ#iq{type = result,
						sub_el =
						[{xmlelement, "vCard",
						  [{"xmlns", XMLNS}],
						  iq_get_vcard(Lang)}]},
				    ejabberd_router:route(To,
							  From,
							  jlib:iq_to_xml(Res));
				#iq{type = get,
				   xmlns = ?NS_MUC_UNIQUE
				   } = IQ ->
				   Res = IQ#iq{type = result,
						sub_el =
						[{xmlelement, "unique",
						   [{"xmlns", ?NS_MUC_UNIQUE}],
						   [iq_get_unique(From)]}]},
				   ejabberd_router:route(To,
				   			 From,
							 jlib:iq_to_xml(Res));
				#iq{} ->
				    Err = jlib:make_error_reply(
					    Packet,
					    ?ERR_FEATURE_NOT_IMPLEMENTED),
				    ejabberd_router:route(To, From, Err);
				_ ->
				    ok
			    end;
			"message" ->
			    case xml:get_attr_s("type", Attrs) of
				"error" ->
				    ok;
				_ ->
				    case acl:match_rule(ServerHost, AccessAdmin, From) of
					allow ->
					    Msg = xml:get_path_s(
						    Packet,
						    [{elem, "body"}, cdata]),
					    broadcast_service_message(Host, Msg);
					_ ->
					    Lang = xml:get_attr_s("xml:lang", Attrs),
					    ErrText = "Only service administrators "
						      "are allowed to send service messages",
					    Err = jlib:make_error_reply(
						    Packet,
						    ?ERRT_FORBIDDEN(Lang, ErrText)),
					    ejabberd_router:route(
					      To, From, Err)
				    end
			    end;
			"presence" ->
			    ok
		    end;
		_ ->
		    case xml:get_attr_s("type", Attrs) of
			"error" ->
			    ok;
			"result" ->
			    ok;
			_ ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_ITEM_NOT_FOUND),
			    ejabberd_router:route(To, From, Err)
		    end
	    end;
	_ ->
	    case mnesia:dirty_read(muc_online_room, {Room, Host}) of
		[] ->
		    Type = xml:get_attr_s("type", Attrs),
		    case {Name, Type} of
			{"presence", ""} ->
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
				    Lang = xml:get_attr_s("xml:lang", Attrs),
				    ErrText = "Room creation is denied by service policy",
				    Err = jlib:make_error_reply(
					    Packet, ?ERRT_FORBIDDEN(Lang, ErrText)),
				    ejabberd_router:route(To, From, Err)
			    end;
			_ ->
			    Lang = xml:get_attr_s("xml:lang", Attrs),
			    ErrText = "Conference room does not exist",
			    Err = jlib:make_error_reply(
				    Packet, ?ERRT_ITEM_NOT_FOUND(Lang, ErrText)),
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
	    (length(RoomID) =< gen_mod:get_module_opt(ServerHost, ?MODULE,
						      max_room_id, infinite));
	_ ->
	    false
    end.

get_rooms(ServerHost, Host) ->
    LServer = jlib:nameprep(ServerHost),
    get_rooms(LServer, Host, gen_mod:db_type(LServer, ?MODULE)).

get_rooms(_LServer, Host, mnesia) ->
    case catch mnesia:dirty_select(
		 muc_room, [{#muc_room{name_host = {'_', Host}, _ = '_'},
			     [],
			     ['$_']}]) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]),
	    [];
	Rs ->
            Rs
    end;
get_rooms(LServer, Host, odbc) ->
    SHost = ejabberd_odbc:escape(Host),
    case catch ejabberd_odbc:sql_query(
                 LServer, ["select name, opts from muc_room ",
                           "where host='", SHost, "';"]) of
        {'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]),
            [];
        {selected, ["name", "opts"], RoomOpts} ->
            lists:map(
              fun({Room, Opts}) ->
                      #muc_room{name_host = {Room, Host},
                                opts = ejabberd_odbc:decode_term(Opts)}
              end, RoomOpts)
    end.

load_permanent_rooms(Host, ServerHost, Access, HistorySize, RoomShaper) ->
    lists:foreach(
      fun(R) ->
              {Room, Host} = R#muc_room.name_host,
              case mnesia:dirty_read(muc_online_room, {Room, Host}) of
                  [] ->
                      {ok, Pid} = mod_muc_room:start(
                                    Host,
                                    ServerHost,
                                    Access,
                                    Room,
                                    HistorySize,
                                    RoomShaper,
                                    R#muc_room.opts),
                      register_room(Host, Room, Pid);
                  _ ->
                      ok
              end
      end, get_rooms(ServerHost, Host)).

start_new_room(Host, ServerHost, Access, Room,
	       HistorySize, RoomShaper, From,
	       Nick, DefRoomOpts) ->
    case restore_room(ServerHost, Room, Host) of
        error ->
	    ?DEBUG("MUC: open new room '~s'~n", [Room]),
	    mod_muc_room:start(Host, ServerHost, Access,
			       Room, HistorySize,
			       RoomShaper, From,
			       Nick, DefRoomOpts);
        Opts ->
	    ?DEBUG("MUC: restore room '~s'~n", [Room]),
	    mod_muc_room:start(Host, ServerHost, Access,
			       Room, HistorySize,
			       RoomShaper, Opts)
    end.

register_room(Host, Room, Pid) ->
    F = fun() ->
		mnesia:write(#muc_online_room{name_host = {Room, Host},
					      pid = Pid})
	end,
    mnesia:transaction(F).


iq_disco_info(Lang) ->
    [{xmlelement, "identity",
      [{"category", "conference"},
       {"type", "text"},
       {"name", translate:translate(Lang, "Chatrooms")}], []},
     {xmlelement, "feature", [{"var", ?NS_DISCO_INFO}], []},
     {xmlelement, "feature", [{"var", ?NS_DISCO_ITEMS}], []},
     {xmlelement, "feature", [{"var", ?NS_MUC}], []},
     {xmlelement, "feature", [{"var", ?NS_MUC_UNIQUE}], []},
     {xmlelement, "feature", [{"var", ?NS_REGISTER}], []},
     {xmlelement, "feature", [{"var", ?NS_RSM}], []},
     {xmlelement, "feature", [{"var", ?NS_VCARD}], []}].


iq_disco_items(Host, From, Lang, none) ->
    lists:zf(fun(#muc_online_room{name_host = {Name, _Host}, pid = Pid}) ->
		     case catch gen_fsm:sync_send_all_state_event(
				  Pid, {get_disco_item, From, Lang}, 100) of
			 {item, Desc} ->
			     flush(),
			     {true,
			      {xmlelement, "item",
			       [{"jid", jlib:jid_to_string({Name, Host, ""})},
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
			      {xmlelement, "item",
			       [{"jid", jlib:jid_to_string({Name, Host, ""})},
				{"name", Desc}], []}};
			 _ ->
			     false
		     end
	     end, Rooms) ++ RsmOut.

get_vh_rooms(Host, #rsm_in{max=M, direction=Direction, id=I, index=Index})->
    AllRooms = lists:sort(get_vh_rooms(Host)),
    Count = erlang:length(AllRooms),
    Guard = case Direction of
		_ when Index =/= undefined -> [{'==', {element, 2, '$1'}, Host}];
		aft -> [{'==', {element, 2, '$1'}, Host}, {'>=',{element, 1, '$1'} ,I}];
		before when I =/= []-> [{'==', {element, 2, '$1'}, Host}, {'=<',{element, 1, '$1'} ,I}];
		_ -> [{'==', {element, 2, '$1'}, Host}]
	    end,
    L = lists:sort(
	  mnesia:dirty_select(muc_online_room,
			      [{#muc_online_room{name_host = '$1', _ = '_'},
				Guard,
				['$_']}])),
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
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).

%% @doc Get a pseudo unique Room Name. The Room Name is generated as a hash of 
%%      the requester JID, the local time and a random salt.
%%
%%      "pseudo" because we don't verify that there is not a room
%%       with the returned Name already created, nor mark the generated Name 
%%       as "already used".  But in practice, it is unique enough. See
%%       http://xmpp.org/extensions/xep-0045.html#createroom-unique
iq_get_unique(From) ->
	{xmlcdata, sha:sha(term_to_binary([From, now(), randoms:get_string()]))}.

get_nick(ServerHost, Host, From) ->
    LServer = jlib:nameprep(ServerHost),
    get_nick(LServer, Host, From, gen_mod:db_type(LServer, ?MODULE)).

get_nick(_LServer, Host, From, mnesia) ->
    {LUser, LServer, _} = jlib:jid_tolower(From),
    LUS = {LUser, LServer},
    case catch mnesia:dirty_read(muc_registered, {LUS, Host}) of
        {'EXIT', _Reason} ->
            error;
        [] ->
            error;
        [#muc_registered{nick = Nick}] ->
            Nick
    end;
get_nick(LServer, Host, From, odbc) ->
    SJID = ejabberd_odbc:escape(
             jlib:jid_to_string(
               jlib:jid_tolower(
                 jlib:jid_remove_resource(From)))),
    SHost = ejabberd_odbc:escape(Host),
    case catch ejabberd_odbc:sql_query(
                 LServer, ["select nick from muc_registered where "
                           "jid='", SJID, "' and host='", SHost, "';"]) of
        {selected, ["nick"], [{Nick}]} ->
            Nick;
        _ ->
            error
    end.

iq_get_register_info(ServerHost, Host, From, Lang) ->
    {Nick, Registered} =
        case get_nick(ServerHost, Host, From) of
            error ->
                {"", []};
            N ->
                {N, [{xmlelement, "registered", [], []}]}
        end,
    Registered ++
	[{xmlelement, "instructions", [],
	  [{xmlcdata,
	    translate:translate(
	      Lang, "You need a client that supports x:data to register the nickname")}]},
	 {xmlelement, "x",
	  [{"xmlns", ?NS_XDATA}],
	  [{xmlelement, "title", [],
	    [{xmlcdata,
	      translate:translate(
		Lang, "Nickname Registration at ") ++ Host}]},
	   {xmlelement, "instructions", [],
	    [{xmlcdata,
	      translate:translate(
		Lang, "Enter nickname you want to register")}]},
	   ?XFIELD("text-single", "Nickname", "nick", Nick)]}].

set_nick(ServerHost, Host, From, Nick) ->
    LServer = jlib:nameprep(ServerHost),
    set_nick(LServer, Host, From, Nick, gen_mod:db_type(LServer, ?MODULE)).

set_nick(_LServer, Host, From, Nick, mnesia) ->
    {LUser, LServer, _} = jlib:jid_tolower(From),
    LUS = {LUser, LServer},
    F = fun() ->
		case Nick of
		    "" ->
			mnesia:delete({muc_registered, {LUS, Host}}),
			ok;
		    _ ->
			Allow =
			    case mnesia:select(
				   muc_registered,
				   [{#muc_registered{us_host = '$1',
						     nick = Nick,
						     _ = '_'},
				     [{'==', {element, 2, '$1'}, Host}],
				     ['$_']}]) of
				[] ->
				    true;
				[#muc_registered{us_host = {U, _Host}}] ->
				    U == LUS
			    end,
			if
			    Allow ->
				mnesia:write(
				  #muc_registered{us_host = {LUS, Host},
						  nick = Nick}),
				ok;
			    true ->
				false
			end
		end
	end,
    mnesia:transaction(F);
set_nick(LServer, Host, From, Nick, odbc) ->
    JID = jlib:jid_to_string(
            jlib:jid_tolower(
              jlib:jid_remove_resource(From))),
    SJID = ejabberd_odbc:escape(JID),
    SNick = ejabberd_odbc:escape(Nick),
    SHost = ejabberd_odbc:escape(Host),
    F = fun() ->
                case Nick of
                    "" ->
                        ejabberd_odbc:sql_query_t(
                          ["delete from muc_registered where ",
                           "jid='", SJID, "' and host='", Host, "';"]),
                        ok;
                    _ ->
                        Allow =
                            case ejabberd_odbc:sql_query_t(
                                   ["select jid from muc_registered ",
                                    "where nick='", SNick, "' and host='",
                                    SHost, "';"]) of
                                {selected, ["jid"], [{J}]} ->
                                    J == JID;
                                _ ->
                                    true
                            end,
                        if Allow ->
                                odbc_queries:update_t(
                                  "muc_registered",
                                  ["jid", "host", "nick"],
                                  [SJID, SHost, SNick],
                                  ["jid='", SJID, "' and host='", SHost, "'"]),
                                ok;
                           true ->
                                false
                        end
                end
        end,
    ejabberd_odbc:sql_transaction(LServer, F).

iq_set_register_info(ServerHost, Host, From, Nick, Lang) ->
    case set_nick(ServerHost, Host, From, Nick) of
	{atomic, ok} ->
	    {result, []};
	{atomic, false} ->
	    ErrText = "That nickname is registered by another person",
	    {error, ?ERRT_CONFLICT(Lang, ErrText)};
	_ ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

process_iq_register_set(ServerHost, Host, From, SubEl, Lang) ->
    {xmlelement, _Name, _Attrs, Els} = SubEl,
    case xml:get_subtag(SubEl, "remove") of
	false ->
	    case xml:remove_cdata(Els) of
		[{xmlelement, "x", _Attrs1, _Els1} = XEl] ->
		    case {xml:get_tag_attr_s("xmlns", XEl),
			  xml:get_tag_attr_s("type", XEl)} of
			{?NS_XDATA, "cancel"} ->
			    {result, []};
			{?NS_XDATA, "submit"} ->
			    XData = jlib:parse_xdata_submit(XEl),
			    case XData of
				invalid ->
				    {error, ?ERR_BAD_REQUEST};
				_ ->
				    case lists:keysearch("nick", 1, XData) of
					{value, {_, [Nick]}} when Nick /= "" ->
					    iq_set_register_info(ServerHost, Host,
                                                                 From, Nick, Lang);
					_ ->
					    ErrText = "You must fill in field \"Nickname\" in the form",
					    {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)}
				    end
			    end;
			_ ->
			    {error, ?ERR_BAD_REQUEST}
		    end;
		_ ->
		    {error, ?ERR_BAD_REQUEST}
	    end;
	_ ->
	    iq_set_register_info(ServerHost, Host, From, "", Lang)
    end.

iq_get_vcard(Lang) ->
    [{xmlelement, "FN", [],
      [{xmlcdata, "ejabberd/mod_muc"}]},
     {xmlelement, "URL", [],
      [{xmlcdata, ?EJABBERD_URI}]},
     {xmlelement, "DESC", [],
      [{xmlcdata, translate:translate(Lang, "ejabberd MUC module") ++
	  "\nCopyright (c) 2003-2012 ProcessOne"}]}].


broadcast_service_message(Host, Msg) ->
    lists:foreach(
      fun(#muc_online_room{pid = Pid}) ->
	      gen_fsm:send_all_state_event(
		Pid, {service_message, Msg})
      end, get_vh_rooms(Host)).

get_vh_rooms(Host) ->
    mnesia:dirty_select(muc_online_room,
			[{#muc_online_room{name_host = '$1', _ = '_'},
			  [{'==', {element, 2, '$1'}, Host}],
			  ['$_']}]).


clean_table_from_bad_node(Node) ->
    F = fun() ->
		Es = mnesia:select(
		       muc_online_room,
		       [{#muc_online_room{pid = '$1', _ = '_'},
			 [{'==', {node, '$1'}, Node}],
			 ['$_']}]),
		lists:foreach(fun(E) ->
				      mnesia:delete_object(E)
			      end, Es)
        end,
    mnesia:async_dirty(F).

clean_table_from_bad_node(Node, Host) ->
    F = fun() ->
		Es = mnesia:select(
		       muc_online_room,
		       [{#muc_online_room{pid = '$1',
					  name_host = {'_', Host},
					  _ = '_'},
			 [{'==', {node, '$1'}, Node}],
			 ['$_']}]),
		lists:foreach(fun(E) ->
				      mnesia:delete_object(E)
			      end, Es)
        end,
    mnesia:async_dirty(F).

update_tables(Host) ->
    update_muc_room_table(Host),
    update_muc_registered_table(Host).

update_muc_room_table(Host) ->
    Fields = record_info(fields, muc_room),
    case mnesia:table_info(muc_room, attributes) of
	Fields ->
	    ok;
	[name, opts] ->
	    ?INFO_MSG("Converting muc_room table from "
		      "{name, opts} format", []),
	    {atomic, ok} = mnesia:create_table(
			     mod_muc_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, muc_room},
			      {attributes, record_info(fields, muc_room)}]),
	    mnesia:transform_table(muc_room, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_muc_tmp_table),
			 mnesia:foldl(
			   fun(#muc_room{name_host = Name} = R, _) ->
				   mnesia:dirty_write(
				     mod_muc_tmp_table,
				     R#muc_room{name_host = {Name, Host}})
			   end, ok, muc_room)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(muc_room),
	    F2 = fun() ->
			 mnesia:write_lock_table(muc_room),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_muc_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_muc_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating muc_room table", []),
	    mnesia:transform_table(muc_room, ignore, Fields)
    end.


update_muc_registered_table(Host) ->
    Fields = record_info(fields, muc_registered),
    case mnesia:table_info(muc_registered, attributes) of
	Fields ->
	    ok;
	[user, nick] ->
	    ?INFO_MSG("Converting muc_registered table from "
		      "{user, nick} format", []),
	    {atomic, ok} = mnesia:create_table(
			     mod_muc_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, muc_registered},
			      {attributes, record_info(fields, muc_registered)}]),
	    mnesia:del_table_index(muc_registered, nick),
	    mnesia:transform_table(muc_registered, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_muc_tmp_table),
			 mnesia:foldl(
			   fun(#muc_registered{us_host = US} = R, _) ->
				   mnesia:dirty_write(
				     mod_muc_tmp_table,
				     R#muc_registered{us_host = {US, Host}})
			   end, ok, muc_registered)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(muc_registered),
	    F2 = fun() ->
			 mnesia:write_lock_table(muc_registered),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_muc_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_muc_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating muc_registered table", []),
	    mnesia:transform_table(muc_registered, ignore, Fields)
    end.
