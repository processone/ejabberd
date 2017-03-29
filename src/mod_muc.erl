%%%----------------------------------------------------------------------
%%% File    : mod_muc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC support (XEP-0045)
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_muc).

-author('alexey@process-one.net').

-protocol({xep, 45, '1.25'}).

-behaviour(gen_server).

-behaviour(gen_mod).

%% API
-export([start/2,
	 stop/1,
	 reload/3,
	 room_destroyed/4,
	 store_room/4,
	 restore_room/3,
	 forget_room/3,
	 create_room/5,
	 shutdown_rooms/1,
	 process_disco_info/1,
	 process_disco_items/1,
	 process_vcard/1,
	 process_register/1,
	 process_muc_unique/1,
	 process_mucsub/1,
	 broadcast_service_message/3,
	 export/1,
	 import_info/0,
	 import/5,
	 import_start/2,
	 opts_to_binary/1,
	 find_online_room/2,
	 register_online_room/3,
	 get_online_rooms/1,
	 count_online_rooms/1,
	 register_online_user/4,
	 unregister_online_user/4,
	 count_online_rooms_by_user/3,
	 get_online_rooms_by_user/3,
	 can_use_nick/4]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3,
	 mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").
-include("mod_muc.hrl").

-record(state,
	{host = <<"">> :: binary(),
         server_host = <<"">> :: binary(),
         access = {none, none, none, none} :: {atom(), atom(), atom(), atom()},
         history_size = 20 :: non_neg_integer(),
         max_rooms_discoitems = 100 :: non_neg_integer(),
	 queue_type = ram :: ram | file,
         default_room_opts = [] :: list(),
         room_shaper = none :: shaper:shaper()}).

-type muc_room_opts() :: [{atom(), any()}].
-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), binary(), [binary()]) -> ok.
-callback store_room(binary(), binary(), binary(), list()) -> {atomic, any()}.
-callback restore_room(binary(), binary(), binary()) -> muc_room_opts() | error.
-callback forget_room(binary(), binary(), binary()) -> {atomic, any()}.
-callback can_use_nick(binary(), binary(), jid(), binary()) -> boolean().
-callback get_rooms(binary(), binary()) -> [#muc_room{}].
-callback get_nick(binary(), binary(), jid()) -> binary() | error.
-callback set_nick(binary(), binary(), jid(), binary()) -> {atomic, ok | false}.
-callback register_online_room(binary(), binary(), binary(), pid()) -> any().
-callback unregister_online_room(binary(), binary(), binary(), pid()) -> any().
-callback find_online_room(binary(), binary(), binary()) -> {ok, pid()} | error.
-callback get_online_rooms(binary(), binary(), undefined | rsm_set()) -> [{binary(), binary(), pid()}].
-callback count_online_rooms(binary(), binary()) -> non_neg_integer().
-callback rsm_supported() -> boolean().
-callback register_online_user(binary(), ljid(), binary(), binary()) -> any().
-callback unregister_online_user(binary(), ljid(), binary(), binary()) -> any().
-callback count_online_rooms_by_user(binary(), binary(), binary()) -> non_neg_integer().
-callback get_online_rooms_by_user(binary(), binary(), binary()) -> [{binary(), binary()}].

%%====================================================================
%% API
%%====================================================================
start(Host, Opts) ->
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    Rooms = shutdown_rooms(Host),
    gen_mod:stop_child(?MODULE, Host),
    {wait, Rooms}.

reload(Host, NewOpts, OldOpts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {reload, Host, NewOpts, OldOpts}).

depends(_Host, _Opts) ->
    [{mod_mam, soft}].

shutdown_rooms(Host) ->
    RMod = gen_mod:ram_db_mod(Host, ?MODULE),
    MyHost = gen_mod:get_module_opt_host(Host, mod_muc,
					 <<"conference.@HOST@">>),
    Rooms = RMod:get_online_rooms(Host, MyHost, undefined),
    lists:flatmap(
      fun({_, _, Pid}) when node(Pid) == node() ->
	      Pid ! shutdown,
	      [Pid];
	 (_) ->
	      []
      end, Rooms).

%% This function is called by a room in three situations:
%% A) The owner of the room destroyed it
%% B) The only participant of a temporary room leaves it
%% C) mod_muc:stop was called, and each room is being terminated
%%    In this case, the mod_muc process died before the room processes
%%    So the message sending must be catched
room_destroyed(Host, Room, Pid, ServerHost) ->
    catch gen_mod:get_module_proc(ServerHost, ?MODULE) !
	    {room_destroyed, {Room, Host}, Pid},
    ok.

%% @doc Create a room.
%% If Opts = default, the default room options are used.
%% Else use the passed options as defined in mod_muc_room.
create_room(Host, Name, From, Nick, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, {create, Name, From, Nick, Opts}).

store_room(ServerHost, Host, Name, Opts) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:store_room(LServer, Host, Name, Opts).

restore_room(ServerHost, Host, Name) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:restore_room(LServer, Host, Name).

forget_room(ServerHost, Host, Name) ->
    LServer = jid:nameprep(ServerHost),
    ejabberd_hooks:run(remove_room, LServer, [LServer, Name, Host]),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:forget_room(LServer, Host, Name).

can_use_nick(_ServerHost, _Host, _JID, <<"">>) -> false;
can_use_nick(ServerHost, Host, JID, Nick) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:can_use_nick(LServer, Host, JID, Nick).

-spec find_online_room(binary(), binary()) -> {ok, pid()} | error.
find_online_room(Room, Host) ->
    ServerHost = ejabberd_router:host_of_route(Host),
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    RMod:find_online_room(ServerHost, Room, Host).

-spec register_online_room(binary(), binary(), pid()) -> any().
register_online_room(Room, Host, Pid) ->
    ServerHost = ejabberd_router:host_of_route(Host),
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    RMod:register_online_room(ServerHost, Room, Host, Pid).

-spec get_online_rooms(binary()) -> [{binary(), binary(), pid()}].
get_online_rooms(Host) ->
    ServerHost = ejabberd_router:host_of_route(Host),
    get_online_rooms(ServerHost, Host).

-spec count_online_rooms(binary()) -> non_neg_integer().
count_online_rooms(Host) ->
    ServerHost = ejabberd_router:host_of_route(Host),
    count_online_rooms(ServerHost, Host).

-spec register_online_user(binary(), ljid(), binary(), binary()) -> any().
register_online_user(ServerHost, LJID, Name, Host) ->
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    RMod:register_online_user(ServerHost, LJID, Name, Host).

-spec unregister_online_user(binary(), ljid(), binary(), binary()) -> any().
unregister_online_user(ServerHost, LJID, Name, Host) ->
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    RMod:unregister_online_user(ServerHost, LJID, Name, Host).

-spec count_online_rooms_by_user(binary(), binary(), binary()) -> non_neg_integer().
count_online_rooms_by_user(ServerHost, LUser, LServer) ->
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    RMod:count_online_rooms_by_user(ServerHost, LUser, LServer).

-spec get_online_rooms_by_user(binary(), binary(), binary()) -> [{binary(), binary()}].
get_online_rooms_by_user(ServerHost, LUser, LServer) ->
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    RMod:get_online_rooms_by_user(ServerHost, LUser, LServer).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, Opts]) ->
    process_flag(trap_exit, true),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    #state{access = Access, host = MyHost,
	   history_size = HistorySize, queue_type = QueueType,
	   room_shaper = RoomShaper} = State = init_state(Host, Opts),
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    RMod = gen_mod:ram_db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, [{host, MyHost}|Opts]),
    RMod:init(Host, [{host, MyHost}|Opts]),
    register_iq_handlers(MyHost, IQDisc),
    ejabberd_router:register_route(MyHost, Host),
    load_permanent_rooms(MyHost, Host, Access, HistorySize, RoomShaper, QueueType),
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({create, Room, From, Nick, Opts}, _From,
	    #state{host = Host, server_host = ServerHost,
		   access = Access, default_room_opts = DefOpts,
		   history_size = HistorySize, queue_type = QueueType,
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
		  Nick, NewOpts, QueueType),
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    RMod:register_online_room(ServerHost, Room, Host, Pid),
    {reply, ok, State}.

handle_cast({reload, ServerHost, NewOpts, OldOpts}, #state{host = OldHost}) ->
    NewIQDisc = gen_mod:get_opt(iqdisc, NewOpts,
				fun gen_iq_handler:check_type/1,
				one_queue),
    OldIQDisc = gen_mod:get_opt(iqdisc, OldOpts,
				fun gen_iq_handler:check_type/1,
				one_queue),
    NewMod = gen_mod:db_mod(ServerHost, NewOpts, ?MODULE),
    NewRMod = gen_mod:ram_db_mod(ServerHost, NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(ServerHost, OldOpts, ?MODULE),
    OldRMod = gen_mod:ram_db_mod(ServerHost, OldOpts, ?MODULE),
    #state{host = NewHost} = NewState = init_state(ServerHost, NewOpts),
    if NewMod /= OldMod ->
	    NewMod:init(ServerHost, [{host, NewHost}|NewOpts]);
       true ->
	    ok
    end,
    if NewRMod /= OldRMod ->
	    NewRMod:init(ServerHost, [{host, NewHost}|NewOpts]);
       true ->
	    ok
    end,
    if (NewIQDisc /= OldIQDisc) or (NewHost /= OldHost) ->
	    register_iq_handlers(NewHost, NewIQDisc);
       true ->
	    ok
    end,
    if NewHost /= OldHost ->
	    ejabberd_router:register_route(NewHost, ServerHost),
	    ejabberd_router:unregister_route(OldHost),
	    unregister_iq_handlers(OldHost);
       true ->
	    ok
    end,
    {noreply, NewState};
handle_cast(Msg, State) ->
    ?WARNING_MSG("unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({route, Packet},
	    #state{host = Host, server_host = ServerHost,
		   access = Access, default_room_opts = DefRoomOpts,
		   history_size = HistorySize, queue_type = QueueType,
		   max_rooms_discoitems = MaxRoomsDiscoItems,
		   room_shaper = RoomShaper} = State) ->
    From = xmpp:get_from(Packet),
    To = xmpp:get_to(Packet),
    case catch do_route(Host, ServerHost, Access, HistorySize, RoomShaper,
			From, To, Packet, DefRoomOpts, MaxRoomsDiscoItems,
			QueueType) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	_ ->
	    ok
    end,
    {noreply, State};
handle_info({room_destroyed, {Room, Host}, Pid}, State) ->
    ServerHost = State#state.server_host,
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    RMod:unregister_online_room(ServerHost, Room, Host, Pid),
    {noreply, State};
handle_info(Info, State) ->
    ?ERROR_MSG("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{host = MyHost}) ->
    ejabberd_router:unregister_route(MyHost),
    unregister_iq_handlers(MyHost).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
init_state(Host, Opts) ->
    MyHost = gen_mod:get_opt_host(Host, Opts,
				  <<"conference.@HOST@">>),
    Access = gen_mod:get_opt(access, Opts,
                             fun acl:access_rules_validator/1, all),
    AccessCreate = gen_mod:get_opt(access_create, Opts,
                                   fun acl:access_rules_validator/1, all),
    AccessAdmin = gen_mod:get_opt(access_admin, Opts,
                                  fun acl:access_rules_validator/1,
                                  none),
    AccessPersistent = gen_mod:get_opt(access_persistent, Opts,
				       fun acl:access_rules_validator/1,
                                       all),
    HistorySize = gen_mod:get_opt(history_size, Opts,
                                  fun(I) when is_integer(I), I>=0 -> I end,
                                  20),
    MaxRoomsDiscoItems = gen_mod:get_opt(max_rooms_discoitems, Opts,
                                  fun(I) when is_integer(I), I>=0 -> I end,
                                  100),
    DefRoomOpts1 = gen_mod:get_opt(default_room_options, Opts,
				   fun(L) when is_list(L) -> L end,
				   []),
    QueueType = case gen_mod:get_opt(queue_type, Opts,
				     mod_opt_type(queue_type)) of
		    undefined ->
			ejabberd_config:default_queue_type(Host);
		    Type ->
			Type
		end,
    DefRoomOpts =
	lists:flatmap(
	  fun({Opt, Val}) ->
		  Bool = fun(B) when is_boolean(B) -> B end,
		  VFun = case Opt of
			     allow_change_subj -> Bool;
			     allow_private_messages -> Bool;
			     allow_query_users -> Bool;
			     allow_user_invites -> Bool;
			     allow_visitor_nickchange -> Bool;
			     allow_visitor_status -> Bool;
			     anonymous -> Bool;
			     captcha_protected -> Bool;
			     logging -> Bool;
			     members_by_default -> Bool;
			     members_only -> Bool;
			     moderated -> Bool;
			     password_protected -> Bool;
			     persistent -> Bool;
			     public -> Bool;
			     public_list -> Bool;
			     mam -> Bool;
			     allow_subscription -> Bool;
			     password -> fun iolist_to_binary/1;
			     title -> fun iolist_to_binary/1;
			     allow_private_messages_from_visitors ->
				 fun(anyone) -> anyone;
				    (moderators) -> moderators;
				    (nobody) -> nobody
				 end;
			     max_users ->
				 fun(I) when is_integer(I), I > 0 -> I end;
                             presence_broadcast ->
                                 fun(L) ->
                                         lists:map(
                                           fun(moderator) -> moderator;
                                              (participant) -> participant;
                                              (visitor) -> visitor
                                           end, L)
                                 end;
			     _ ->
				 ?ERROR_MSG("unknown option ~p with value ~p",
					    [Opt, Val]),
				 fun(_) -> undefined end
			 end,
		  case gen_mod:get_opt(Opt, [{Opt, Val}], VFun) of
		      undefined -> [];
		      NewVal -> [{Opt, NewVal}]
		  end
	  end, DefRoomOpts1),
    RoomShaper = gen_mod:get_opt(room_shaper, Opts,
                                 fun(A) when is_atom(A) -> A end,
                                 none),
    #state{host = MyHost,
	   server_host = Host,
	   access = {Access, AccessCreate, AccessAdmin, AccessPersistent},
	   default_room_opts = DefRoomOpts,
	   queue_type = QueueType,
	   history_size = HistorySize,
	   max_rooms_discoitems = MaxRoomsDiscoItems,
	   room_shaper = RoomShaper}.

register_iq_handlers(Host, IQDisc) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_REGISTER,
				  ?MODULE, process_register, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_VCARD,
				  ?MODULE, process_vcard, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MUCSUB,
				  ?MODULE, process_mucsub, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MUC_UNIQUE,
				  ?MODULE, process_muc_unique, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO,
				  ?MODULE, process_disco_info, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS,
				  ?MODULE, process_disco_items, IQDisc).

unregister_iq_handlers(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_REGISTER),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_MUCSUB),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_MUC_UNIQUE),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS).

do_route(Host, ServerHost, Access, HistorySize, RoomShaper,
	 From, To, Packet, DefRoomOpts, _MaxRoomsDiscoItems, QueueType) ->
    {AccessRoute, _AccessCreate, _AccessAdmin, _AccessPersistent} = Access,
    case acl:match_rule(ServerHost, AccessRoute, From) of
	allow ->
	    do_route1(Host, ServerHost, Access, HistorySize, RoomShaper,
		      From, To, Packet, DefRoomOpts, QueueType);
	deny ->
	    Lang = xmpp:get_lang(Packet),
	    ErrText = <<"Access denied by service policy">>,
	    Err = xmpp:err_forbidden(ErrText, Lang),
	    ejabberd_router:route_error(Packet, Err)
    end.

do_route1(_Host, _ServerHost, _Access, _HistorySize, _RoomShaper,
	  _From, #jid{luser = <<"">>, lresource = <<"">>} = _To,
	  #iq{} = IQ, _DefRoomOpts, _QueueType) ->
    ejabberd_local:process_iq(IQ);
do_route1(Host, ServerHost, Access, _HistorySize, _RoomShaper,
	  From, #jid{luser = <<"">>, lresource = <<"">>} = _To,
	  #message{lang = Lang, body = Body, type = Type} = Packet, _, _) ->
    {_AccessRoute, _AccessCreate, AccessAdmin, _AccessPersistent} = Access,
    if Type == error ->
	    ok;
       true ->
	    case acl:match_rule(ServerHost, AccessAdmin, From) of
		allow ->
		    Msg = xmpp:get_text(Body),
		    broadcast_service_message(ServerHost, Host, Msg);
		deny ->
		    ErrText = <<"Only service administrators are allowed "
				"to send service messages">>,
		    Err = xmpp:err_forbidden(ErrText, Lang),
		    ejabberd_router:route_error(Packet, Err)
	    end
    end;
do_route1(_Host, _ServerHost, _Access, _HistorySize, _RoomShaper,
	  _From, #jid{luser = <<"">>} = _To, Packet, _DefRoomOpts, _) ->
    Err = xmpp:err_service_unavailable(),
    ejabberd_router:route_error(Packet, Err);
do_route1(Host, ServerHost, Access, HistorySize, RoomShaper,
	  From, To, Packet, DefRoomOpts, QueueType) ->
    {_AccessRoute, AccessCreate, _AccessAdmin, _AccessPersistent} = Access,
    {Room, _, Nick} = jid:tolower(To),
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    case RMod:find_online_room(ServerHost, Room, Host) of
	error ->
	    case is_create_request(Packet) of
		true ->
		    case check_user_can_create_room(
			   ServerHost, AccessCreate, From, Room) and
			check_create_roomid(ServerHost, Room) of
			true ->
			    {ok, Pid} = start_new_room(
					  Host, ServerHost, Access,
					  Room, HistorySize,
					  RoomShaper, From, Nick, DefRoomOpts,
					  QueueType),
			    RMod:register_online_room(ServerHost, Room, Host, Pid),
			    mod_muc_room:route(Pid, Packet),
			    ok;
			false ->
			    Lang = xmpp:get_lang(Packet),
			    ErrText = <<"Room creation is denied by service policy">>,
			    Err = xmpp:err_forbidden(ErrText, Lang),
			    ejabberd_router:route_error(Packet, Err)
		    end;
		false ->
		    Lang = xmpp:get_lang(Packet),
		    ErrText = <<"Conference room does not exist">>,
		    Err = xmpp:err_item_not_found(ErrText, Lang),
		    ejabberd_router:route_error(Packet, Err)
	    end;
	{ok, Pid} ->
	    ?DEBUG("MUC: send to process ~p~n", [Pid]),
	    mod_muc_room:route(Pid, Packet),
	    ok
    end.

-spec process_vcard(iq()) -> iq().
process_vcard(#iq{type = get, lang = Lang, sub_els = [#vcard_temp{}]} = IQ) ->
    Desc = translate:translate(Lang, <<"ejabberd MUC module">>),
    xmpp:make_iq_result(
      IQ, #vcard_temp{fn = <<"ejabberd/mod_muc">>,
		      url = ?EJABBERD_URI,
		      desc = <<Desc/binary, $\n, ?COPYRIGHT>>});
process_vcard(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_vcard(#iq{lang = Lang} = IQ) ->
    Txt = <<"No module is handling this query">>,
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec process_register(iq()) -> iq().
process_register(#iq{type = get, from = From, to = To, lang = Lang,
		     sub_els = [#register{}]} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    xmpp:make_iq_result(IQ, iq_get_register_info(ServerHost, Host, From, Lang));
process_register(#iq{type = set, from = From, to = To,
		     lang = Lang, sub_els = [El = #register{}]} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    case process_iq_register_set(ServerHost, Host, From, El, Lang) of
	{result, Result} ->
	    xmpp:make_iq_result(IQ, Result);
	{error, Err} ->
	    xmpp:make_error(IQ, Err)
    end.

-spec process_disco_info(iq()) -> iq().
process_disco_info(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_info(#iq{type = get, to = To, lang = Lang,
		       sub_els = [#disco_info{node = <<"">>}]} = IQ) ->
    ServerHost = ejabberd_router:host_of_route(To#jid.lserver),
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    X = ejabberd_hooks:run_fold(disco_info, ServerHost, [],
				[ServerHost, ?MODULE, <<"">>, Lang]),
    MAMFeatures = case gen_mod:is_loaded(ServerHost, mod_mam) of
		      true -> [?NS_MAM_TMP, ?NS_MAM_0, ?NS_MAM_1];
		      false -> []
		  end,
    RSMFeatures = case RMod:rsm_supported() of
		      true -> [?NS_RSM];
		      false -> []
		  end,
    Features = [?NS_DISCO_INFO, ?NS_DISCO_ITEMS,
		?NS_REGISTER, ?NS_MUC, ?NS_VCARD, ?NS_MUCSUB, ?NS_MUC_UNIQUE
		| RSMFeatures ++ MAMFeatures],
    Identity = #identity{category = <<"conference">>,
			 type = <<"text">>,
			 name = translate:translate(Lang, <<"Chatrooms">>)},
    xmpp:make_iq_result(
      IQ, #disco_info{features = Features,
		      identities = [Identity],
		      xdata = X});
process_disco_info(#iq{type = get, lang = Lang,
		       sub_els = [#disco_info{}]} = IQ) ->
    xmpp:make_error(IQ, xmpp:err_item_not_found(<<"Node not found">>, Lang));
process_disco_info(#iq{lang = Lang} = IQ) ->
    Txt = <<"No module is handling this query">>,
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec process_disco_items(iq()) -> iq().
process_disco_items(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_items(#iq{type = get, from = From, to = To, lang = Lang,
			sub_els = [#disco_items{node = Node, rsm = RSM}]} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    MaxRoomsDiscoItems = gen_mod:get_module_opt(
			   ServerHost, ?MODULE, max_rooms_discoitems,
			   fun(I) when is_integer(I), I>=0 -> I end,
			   100),
    case iq_disco_items(ServerHost, Host, From, Lang,
			MaxRoomsDiscoItems, Node, RSM) of
	{error, Err} ->
	    xmpp:make_error(IQ, Err);
	{result, Result} ->
	    xmpp:make_iq_result(IQ, Result)
    end;
process_disco_items(#iq{lang = Lang} = IQ) ->
    Txt = <<"No module is handling this query">>,
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec process_muc_unique(iq()) -> iq().
process_muc_unique(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_muc_unique(#iq{from = From, type = get,
		       sub_els = [#muc_unique{}]} = IQ) ->
    Name = str:sha(term_to_binary([From, p1_time_compat:timestamp(),
				      randoms:get_string()])),
    xmpp:make_iq_result(IQ, #muc_unique{name = Name}).

-spec process_mucsub(iq()) -> iq().
process_mucsub(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_mucsub(#iq{type = get, from = From, to = To,
		   sub_els = [#muc_subscriptions{}]} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    RoomJIDs = get_subscribed_rooms(ServerHost, Host, From),
    xmpp:make_iq_result(IQ, #muc_subscriptions{list = RoomJIDs});
process_mucsub(#iq{lang = Lang} = IQ) ->
    Txt = <<"No module is handling this query">>,
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec is_create_request(stanza()) -> boolean().
is_create_request(#presence{type = available}) ->
    true;
is_create_request(#iq{type = T} = IQ) when T == get; T == set ->
    xmpp:has_subtag(IQ, #muc_subscribe{}) orelse
    xmpp:has_subtag(IQ, #muc_owner{});
is_create_request(_) ->
    false.

check_user_can_create_room(ServerHost, AccessCreate,
			   From, _RoomID) ->
    case acl:match_rule(ServerHost, AccessCreate, From) of
      allow -> true;
      _ -> false
    end.

check_create_roomid(ServerHost, RoomID) ->
    Max = gen_mod:get_module_opt(ServerHost, ?MODULE, max_room_id,
				 fun(infinity) -> infinity;
				    (I) when is_integer(I), I>0 -> I
				 end, infinity),
    Regexp = gen_mod:get_module_opt(ServerHost, ?MODULE, regexp_room_id,
				    fun iolist_to_binary/1, ""),
    (byte_size(RoomID) =< Max) and
    (re:run(RoomID, Regexp, [unicode, {capture, none}]) == match).

get_rooms(ServerHost, Host) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_rooms(LServer, Host).

load_permanent_rooms(Host, ServerHost, Access,
		     HistorySize, RoomShaper, QueueType) ->
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    lists:foreach(
      fun(R) ->
		{Room, Host} = R#muc_room.name_host,
	      case RMod:find_online_room(ServerHost, Room, Host) of
		  error ->
			{ok, Pid} = mod_muc_room:start(Host,
				ServerHost, Access, Room,
				HistorySize, RoomShaper,
				R#muc_room.opts, QueueType),
		      RMod:register_online_room(ServerHost, Room, Host, Pid);
		  {ok, _} ->
		      ok
		end
	end,
	get_rooms(ServerHost, Host)).

start_new_room(Host, ServerHost, Access, Room,
	    HistorySize, RoomShaper, From,
	    Nick, DefRoomOpts, QueueType) ->
    case restore_room(ServerHost, Host, Room) of
	error ->
	    ?DEBUG("MUC: open new room '~s'~n", [Room]),
	    mod_muc_room:start(Host, ServerHost, Access, Room,
		HistorySize, RoomShaper,
		From, Nick, DefRoomOpts, QueueType);
	Opts ->
	    ?DEBUG("MUC: restore room '~s'~n", [Room]),
	    mod_muc_room:start(Host, ServerHost, Access, Room,
		HistorySize, RoomShaper, Opts, QueueType)
    end.

-spec iq_disco_items(binary(), binary(), jid(), binary(), integer(), binary(),
		     rsm_set() | undefined) ->
			    {result, disco_items()} | {error, stanza_error()}.
iq_disco_items(ServerHost, Host, From, Lang, MaxRoomsDiscoItems, Node, RSM)
  when Node == <<"">>; Node == <<"nonemptyrooms">>; Node == <<"emptyrooms">> ->
    Count = count_online_rooms(ServerHost, Host),
    Query = if Node == <<"">>, RSM == undefined, Count > MaxRoomsDiscoItems ->
		    {get_disco_item, only_non_empty, From, Lang};
	       Node == <<"nonemptyrooms">> ->
		    {get_disco_item, only_non_empty, From, Lang};
	       Node == <<"emptyrooms">> ->
		    {get_disco_item, 0, From, Lang};
	       true ->
		    {get_disco_item, all, From, Lang}
	    end,
    Items = lists:flatmap(
	      fun(R) ->
		      case get_room_disco_item(R, Query) of
			  {ok, Item} -> [Item];
			  {error, _} -> []
		      end
	      end, get_online_rooms(ServerHost, Host, RSM)),
    ResRSM = case Items of
		 [_|_] when RSM /= undefined ->
		     #disco_item{jid = #jid{luser = First}} = hd(Items),
		     #disco_item{jid = #jid{luser = Last}} = lists:last(Items),
		     #rsm_set{first = #rsm_first{data = First},
			      last = Last,
			      count = Count};
		 [] when RSM /= undefined ->
		     #rsm_set{count = Count};
		 _ ->
		     undefined
	     end,
    {result, #disco_items{node = Node, items = Items, rsm = ResRSM}};
iq_disco_items(_ServerHost, _Host, _From, Lang, _MaxRoomsDiscoItems, _Node, _RSM) ->
    {error, xmpp:err_item_not_found(<<"Node not found">>, Lang)}.

-spec get_room_disco_item({binary(), binary(), pid()},
			  term()) -> {ok, disco_item()} |
							   {error, timeout | notfound}.
get_room_disco_item({Name, Host, Pid}, Query) ->
	    RoomJID = jid:make(Name, Host),
	    try gen_fsm:sync_send_all_state_event(Pid, Query, 100) of
		{item, Desc} ->
		    {ok, #disco_item{jid = RoomJID, name = Desc}};
		false ->
		    {error, notfound}
	    catch _:{timeout, _} ->
		    {error, timeout};
		  _:{noproc, _} ->
		    {error, notfound}
    end.

get_subscribed_rooms(ServerHost, Host, From) ->
    Rooms = get_online_rooms(ServerHost, Host),
    BareFrom = jid:remove_resource(From),
    lists:flatmap(
      fun({Name, _, Pid}) ->
	      case gen_fsm:sync_send_all_state_event(Pid, {is_subscribed, BareFrom}) of
		  true -> [jid:make(Name, Host)];
		  false -> []
	      end;
	 (_) ->
	      []
      end, Rooms).

get_nick(ServerHost, Host, From) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_nick(LServer, Host, From).

iq_get_register_info(ServerHost, Host, From, Lang) ->
    {Nick, Registered} = case get_nick(ServerHost, Host, From) of
			     error -> {<<"">>, false};
			     N -> {N, true}
			 end,
    Title = <<(translate:translate(
		 Lang, <<"Nickname Registration at ">>))/binary, Host/binary>>,
    Inst = translate:translate(Lang, <<"Enter nickname you want to register">>),
    Fields = muc_register:encode([{roomnick, Nick}], Lang),
    X = #xdata{type = form, title = Title,
	       instructions = [Inst], fields = Fields},
    #register{nick = Nick,
	      registered = Registered,
	      instructions = 
		  translate:translate(
		    Lang, <<"You need a client that supports x:data "
			    "to register the nickname">>),
	      xdata = X}.

set_nick(ServerHost, Host, From, Nick) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:set_nick(LServer, Host, From, Nick).

iq_set_register_info(ServerHost, Host, From, Nick,
		     Lang) ->
    case set_nick(ServerHost, Host, From, Nick) of
      {atomic, ok} -> {result, undefined};
      {atomic, false} ->
	  ErrText = <<"That nickname is registered by another "
		      "person">>,
	  {error, xmpp:err_conflict(ErrText, Lang)};
      _ ->
	  Txt = <<"Database failure">>,
	  {error, xmpp:err_internal_server_error(Txt, Lang)}
    end.

process_iq_register_set(ServerHost, Host, From,
			#register{remove = true}, Lang) ->
    iq_set_register_info(ServerHost, Host, From, <<"">>, Lang);
process_iq_register_set(_ServerHost, _Host, _From,
			#register{xdata = #xdata{type = cancel}}, _Lang) ->
    {result, undefined};
process_iq_register_set(ServerHost, Host, From,
			#register{nick = Nick, xdata = XData}, Lang) ->
    case XData of
	#xdata{type = submit, fields = Fs} ->
	    try
		Options = muc_register:decode(Fs),
		N = proplists:get_value(roomnick, Options),
		iq_set_register_info(ServerHost, Host, From, N, Lang)
	    catch _:{muc_register, Why} ->
		    ErrText = muc_register:format_error(Why),
		    {error, xmpp:err_bad_request(ErrText, Lang)}
	    end;
	#xdata{} ->
	    Txt = <<"Incorrect data form">>,
	    {error, xmpp:err_bad_request(Txt, Lang)};
	_ when is_binary(Nick), Nick /= <<"">> ->
	    iq_set_register_info(ServerHost, Host, From, Nick, Lang);
	_ ->
	    ErrText = <<"You must fill in field \"Nickname\" in the form">>,
	    {error, xmpp:err_not_acceptable(ErrText, Lang)}
    end.

-spec broadcast_service_message(binary(), binary(), binary()) -> ok.
broadcast_service_message(ServerHost, Host, Msg) ->
    lists:foreach(
      fun({_, _, Pid}) ->
		gen_fsm:send_all_state_event(
		    Pid, {service_message, Msg})
      end, get_online_rooms(ServerHost, Host)).

-spec get_online_rooms(binary(), binary()) -> [{binary(), binary(), pid()}].
get_online_rooms(ServerHost, Host) ->
    get_online_rooms(ServerHost, Host, undefined).

-spec get_online_rooms(binary(), binary(), undefined | rsm_set()) ->
			  [{binary(), binary(), pid()}].
get_online_rooms(ServerHost, Host, RSM) ->
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    RMod:get_online_rooms(ServerHost, Host, RSM).

-spec count_online_rooms(binary(), binary()) -> non_neg_integer().
count_online_rooms(ServerHost, Host) ->
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    RMod:count_online_rooms(ServerHost, Host).

opts_to_binary(Opts) ->
    lists:map(
      fun({title, Title}) ->
              {title, iolist_to_binary(Title)};
         ({description, Desc}) ->
              {description, iolist_to_binary(Desc)};
         ({password, Pass}) ->
              {password, iolist_to_binary(Pass)};
         ({subject, Subj}) ->
              {subject, iolist_to_binary(Subj)};
         ({subject_author, Author}) ->
              {subject_author, iolist_to_binary(Author)};
         ({affiliations, Affs}) ->
              {affiliations, lists:map(
                               fun({{U, S, R}, Aff}) ->
                                       NewAff =
                                           case Aff of
                                               {A, Reason} ->
                                                   {A, iolist_to_binary(Reason)};
                                               _ ->
                                                   Aff
                                           end,
                                       {{iolist_to_binary(U),
                                         iolist_to_binary(S),
                                         iolist_to_binary(R)},
                                        NewAff}
                               end, Affs)};
         ({captcha_whitelist, CWList}) ->
              {captcha_whitelist, lists:map(
                                    fun({U, S, R}) ->
                                            {iolist_to_binary(U),
                                             iolist_to_binary(S),
                                             iolist_to_binary(R)}
                                    end, CWList)};
         (Opt) ->
              Opt
      end, Opts).

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import_info() ->
    [{<<"muc_room">>, 4}, {<<"muc_registered">>, 4}].

import_start(LServer, DBType) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:init(LServer, []).

import(LServer, {sql, _}, DBType, Tab, L) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, Tab, L).

mod_opt_type(access) ->
    fun acl:access_rules_validator/1;
mod_opt_type(access_admin) ->
    fun acl:access_rules_validator/1;
mod_opt_type(access_create) ->
    fun acl:access_rules_validator/1;
mod_opt_type(access_persistent) ->
    fun acl:access_rules_validator/1;
mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(ram_db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(default_room_options) ->
    fun (L) when is_list(L) -> L end;
mod_opt_type(history_size) ->
    fun (I) when is_integer(I), I >= 0 -> I end;
mod_opt_type(host) -> fun iolist_to_binary/1;
mod_opt_type(max_room_desc) ->
    fun (infinity) -> infinity;
	(I) when is_integer(I), I > 0 -> I
    end;
mod_opt_type(max_room_id) ->
    fun (infinity) -> infinity;
	(I) when is_integer(I), I > 0 -> I
    end;
mod_opt_type(max_rooms_discoitems) ->
    fun (I) when is_integer(I), I >= 0 -> I end;
mod_opt_type(regexp_room_id) ->
    fun iolist_to_binary/1;
mod_opt_type(max_room_name) ->
    fun (infinity) -> infinity;
	(I) when is_integer(I), I > 0 -> I
    end;
mod_opt_type(max_user_conferences) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(max_users) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(max_users_admin_threshold) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(max_users_presence) ->
    fun (MUP) when is_integer(MUP) -> MUP end;
mod_opt_type(min_message_interval) ->
    fun (MMI) when is_number(MMI) -> MMI end;
mod_opt_type(min_presence_interval) ->
    fun (I) when is_number(I), I >= 0 -> I end;
mod_opt_type(room_shaper) ->
    fun (A) when is_atom(A) -> A end;
mod_opt_type(user_message_shaper) ->
    fun (A) when is_atom(A) -> A end;
mod_opt_type(user_presence_shaper) ->
    fun (A) when is_atom(A) -> A end;
mod_opt_type(queue_type) ->
    fun(ram) -> ram; (file) -> file end;
mod_opt_type(_) ->
    [access, access_admin, access_create, access_persistent,
     db_type, ram_db_type, default_room_options, history_size, host,
     max_room_desc, max_room_id, max_room_name,
     max_rooms_discoitems, max_user_conferences, max_users,
     max_users_admin_threshold, max_users_presence,
     min_message_interval, min_presence_interval, queue_type,
     regexp_room_id, room_shaper, user_message_shaper, user_presence_shaper].
