%%%----------------------------------------------------------------------
%%% File    : mod_muc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC support (XEP-0045)
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
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
-export([start_link/2,
	 start/2,
	 stop/1,
	 room_destroyed/4,
	 store_room/4,
	 restore_room/3,
	 forget_room/3,
	 create_room/5,
	 shutdown_rooms/1,
	 process_iq_disco_items/4,
	 broadcast_service_message/2,
	 export/1,
	 import/1,
	 import/3,
	 opts_to_binary/1,
	 can_use_nick/4]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3,
	 mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").
-include("mod_muc.hrl").

-record(state,
	{host = <<"">> :: binary(),
         server_host = <<"">> :: binary(),
         access = {none, none, none, none} :: {atom(), atom(), atom(), atom()},
         history_size = 20 :: non_neg_integer(),
         default_room_opts = [] :: list(),
         room_shaper = none :: shaper:shaper()}).

-define(PROCNAME, ejabberd_mod_muc).

-define(MAX_ROOMS_DISCOITEMS, 100).

-type muc_room_opts() :: [{atom(), any()}].
-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), #muc_room{} | #muc_registered{}) -> ok | pass.
-callback store_room(binary(), binary(), binary(), list()) -> {atomic, any()}.
-callback restore_room(binary(), binary(), binary()) -> muc_room_opts() | error.
-callback forget_room(binary(), binary(), binary()) -> {atomic, any()}.
-callback can_use_nick(binary(), binary(), jid(), binary()) -> boolean().
-callback get_rooms(binary(), binary()) -> [#muc_room{}].
-callback get_nick(binary(), binary(), jid()) -> binary() | error.
-callback set_nick(binary(), binary(), jid(), binary()) -> {atomic, ok | false}.

%%====================================================================
%% API
%%====================================================================
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		 temporary, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Rooms = shutdown_rooms(Host),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:delete_child(ejabberd_sup, Proc),
    {wait, Rooms}.

depends(_Host, _Opts) ->
    [{mod_mam, soft}].

shutdown_rooms(Host) ->
    MyHost = gen_mod:get_module_opt_host(Host, mod_muc,
					 <<"conference.@HOST@">>),
    Rooms = mnesia:dirty_select(muc_online_room,
				[{#muc_online_room{name_host = '$1',
						   pid = '$2'},
				  [{'==', {element, 2, '$1'}, MyHost},
				   {'==', {node, '$2'}, node()}],
				  ['$2']}]),
    [Pid ! shutdown || Pid <- Rooms],
    Rooms.

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
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:store_room(LServer, Host, Name, Opts).

restore_room(ServerHost, Host, Name) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:restore_room(LServer, Host, Name).

forget_room(ServerHost, Host, Name) ->
    LServer = jid:nameprep(ServerHost),
    remove_room_mam(LServer, Host, Name),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:forget_room(LServer, Host, Name).

remove_room_mam(LServer, Host, Name) ->
    case gen_mod:is_loaded(LServer, mod_mam) of
	true ->
	    mod_mam:remove_room(LServer, Name, Host);
	false ->
	    ok
    end.

process_iq_disco_items(Host, From, To,
		       #iq{lang = Lang} = IQ) ->
    Rsm = jlib:rsm_decode(IQ),
    DiscoNode = fxml:get_tag_attr_s(<<"node">>, IQ#iq.sub_el),
    Res = IQ#iq{type = result,
		sub_el =
		    [#xmlel{name = <<"query">>,
			    attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS}],
			    children = iq_disco_items(Host, From, Lang, DiscoNode, Rsm)}]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(Res)).

can_use_nick(_ServerHost, _Host, _JID, <<"">>) -> false;
can_use_nick(ServerHost, Host, JID, Nick) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:can_use_nick(LServer, Host, JID, Nick).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, Opts]) ->
    MyHost = gen_mod:get_opt_host(Host, Opts,
				  <<"conference.@HOST@">>),
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, [{host, MyHost}|Opts]),
    mnesia:create_table(muc_online_room,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, muc_online_room)}]),
    mnesia:add_table_copy(muc_online_room, node(), ram_copies),
    catch ets:new(muc_online_users, [bag, named_table, public, {keypos, 2}]),
    clean_table_from_bad_node(node(), MyHost),
    mnesia:subscribe(system),
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
    DefRoomOpts1 = gen_mod:get_opt(default_room_options, Opts,
				   fun(L) when is_list(L) -> L end,
				   []),
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
    ejabberd_router:register_route(MyHost, Host),
    load_permanent_rooms(MyHost, Host,
			 {Access, AccessCreate, AccessAdmin, AccessPersistent},
			 HistorySize, RoomShaper),
    {ok, #state{host = MyHost,
		server_host = Host,
		access = {Access, AccessCreate, AccessAdmin, AccessPersistent},
		default_room_opts = DefRoomOpts,
		history_size = HistorySize,
		room_shaper = RoomShaper}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({create, Room, From, Nick, Opts}, _From,
	    #state{host = Host, server_host = ServerHost,
		   access = Access, default_room_opts = DefOpts,
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

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({route, From, To, Packet},
	    #state{host = Host, server_host = ServerHost,
		   access = Access, default_room_opts = DefRoomOpts,
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
    F = fun () ->
		mnesia:delete_object(#muc_online_room{name_host =
							  RoomHost,
						      pid = Pid})
	end,
    mnesia:transaction(F),
    {noreply, State};
handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    clean_table_from_bad_node(Node),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
    ejabberd_router:unregister_route(State#state.host),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

do_route(Host, ServerHost, Access, HistorySize, RoomShaper,
	 From, To, Packet, DefRoomOpts) ->
    {AccessRoute, _AccessCreate, _AccessAdmin, _AccessPersistent} = Access,
    case acl:match_rule(ServerHost, AccessRoute, From) of
	allow ->
	    do_route1(Host, ServerHost, Access, HistorySize, RoomShaper,
		From, To, Packet, DefRoomOpts);
	_ ->
	    #xmlel{attrs = Attrs} = Packet,
	    Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
	    ErrText = <<"Access denied by service policy">>,
	    Err = jlib:make_error_reply(Packet,
		    ?ERRT_FORBIDDEN(Lang, ErrText)),
	    ejabberd_router:route_error(To, From, Err, Packet)
    end.


do_route1(Host, ServerHost, Access, HistorySize, RoomShaper,
	  From, To, Packet, DefRoomOpts) ->
    {_AccessRoute, AccessCreate, AccessAdmin, _AccessPersistent} = Access,
    {Room, _, Nick} = jid:tolower(To),
    #xmlel{name = Name, attrs = Attrs} = Packet,
    case Room of
      <<"">> ->
	  case Nick of
	    <<"">> ->
		case Name of
		  <<"iq">> ->
		      case jlib:iq_query_info(Packet) of
			#iq{type = get, xmlns = (?NS_DISCO_INFO) = XMLNS,
			    sub_el = _SubEl, lang = Lang} =
			    IQ ->
			    Info = ejabberd_hooks:run_fold(disco_info,
							   ServerHost, [],
							   [ServerHost, ?MODULE,
							    <<"">>, <<"">>]),
			    Res = IQ#iq{type = result,
					sub_el =
					    [#xmlel{name = <<"query">>,
						    attrs =
							[{<<"xmlns">>, XMLNS}],
						    children =
							iq_disco_info(
							  ServerHost, Lang) ++
							  Info}]},
			    ejabberd_router:route(To, From,
						  jlib:iq_to_xml(Res));
			#iq{type = get, xmlns = ?NS_DISCO_ITEMS} = IQ ->
			    spawn(?MODULE, process_iq_disco_items,
				  [Host, From, To, IQ]);
			#iq{type = get, xmlns = (?NS_REGISTER) = XMLNS,
			    lang = Lang, sub_el = _SubEl} =
			    IQ ->
			    Res = IQ#iq{type = result,
					sub_el =
					    [#xmlel{name = <<"query">>,
						    attrs =
							[{<<"xmlns">>, XMLNS}],
						    children =
							iq_get_register_info(ServerHost,
									     Host,
									     From,
									     Lang)}]},
			    ejabberd_router:route(To, From,
						  jlib:iq_to_xml(Res));
			#iq{type = set, xmlns = (?NS_REGISTER) = XMLNS,
			    lang = Lang, sub_el = SubEl} =
			    IQ ->
			    case process_iq_register_set(ServerHost, Host, From,
							 SubEl, Lang)
				of
			      {result, IQRes} ->
				  Res = IQ#iq{type = result,
					      sub_el =
						  [#xmlel{name = <<"query">>,
							  attrs =
							      [{<<"xmlns">>,
								XMLNS}],
							  children = IQRes}]},
				  ejabberd_router:route(To, From,
							jlib:iq_to_xml(Res));
			      {error, Error} ->
				  Err = jlib:make_error_reply(Packet, Error),
				  ejabberd_router:route(To, From, Err)
			    end;
			#iq{type = get, xmlns = (?NS_VCARD) = XMLNS,
			    lang = Lang, sub_el = _SubEl} =
			    IQ ->
			    Res = IQ#iq{type = result,
					sub_el =
					    [#xmlel{name = <<"vCard">>,
						    attrs =
							[{<<"xmlns">>, XMLNS}],
						    children =
							iq_get_vcard(Lang)}]},
			    ejabberd_router:route(To, From,
						  jlib:iq_to_xml(Res));
			#iq{type = get, xmlns = ?NS_MUC_UNIQUE} = IQ ->
			    Res = IQ#iq{type = result,
					sub_el =
					    [#xmlel{name = <<"unique">>,
						    attrs =
							[{<<"xmlns">>,
							  ?NS_MUC_UNIQUE}],
						    children =
							[iq_get_unique(From)]}]},
			    ejabberd_router:route(To, From,
						  jlib:iq_to_xml(Res));
			#iq{} ->
			    Err = jlib:make_error_reply(Packet,
							?ERR_FEATURE_NOT_IMPLEMENTED),
			    ejabberd_router:route(To, From, Err);
			_ -> ok
		      end;
		  <<"message">> ->
		      case fxml:get_attr_s(<<"type">>, Attrs) of
			<<"error">> -> ok;
			_ ->
			    case acl:match_rule(ServerHost, AccessAdmin, From)
				of
			      allow ->
				  Msg = fxml:get_path_s(Packet,
						       [{elem, <<"body">>},
							cdata]),
				  broadcast_service_message(Host, Msg);
			      _ ->
				  Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
				  ErrText =
				      <<"Only service administrators are allowed "
					"to send service messages">>,
				  Err = jlib:make_error_reply(Packet,
							      ?ERRT_FORBIDDEN(Lang,
									      ErrText)),
				  ejabberd_router:route(To, From, Err)
			    end
		      end;
		  <<"presence">> -> ok
		end;
	    _ ->
		case fxml:get_attr_s(<<"type">>, Attrs) of
		  <<"error">> -> ok;
		  <<"result">> -> ok;
		  _ ->
		      Err = jlib:make_error_reply(Packet,
						  ?ERR_ITEM_NOT_FOUND),
		      ejabberd_router:route(To, From, Err)
		end
	  end;
      _ ->
	    case mnesia:dirty_read(muc_online_room, {Room, Host}) of
		[] ->
		    Type = fxml:get_attr_s(<<"type">>, Attrs),
		    case {Name, Type} of
			{<<"presence">>, <<"">>} ->
			    case check_user_can_create_room(ServerHost,
				    AccessCreate, From, Room) and
				check_create_roomid(ServerHost, Room) of
				true ->
				    {ok, Pid} = start_new_room(Host, ServerHost, Access,
					    Room, HistorySize,
					    RoomShaper, From, Nick, DefRoomOpts),
				    register_room(Host, Room, Pid),
				    mod_muc_room:route(Pid, From, Nick, Packet),
				    ok;
				false ->
				    Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
				    ErrText = <<"Room creation is denied by service policy">>,
				    Err = jlib:make_error_reply(
					    Packet, ?ERRT_FORBIDDEN(Lang, ErrText)),
				    ejabberd_router:route(To, From, Err)
			    end;
			_ ->
			    Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
			    ErrText = <<"Conference room does not exist">>,
			    Err = jlib:make_error_reply(Packet,
				    ?ERRT_ITEM_NOT_FOUND(Lang, ErrText)),
			    ejabberd_router:route(To, From, Err)
		    end;
		[R] ->
		    Pid = R#muc_online_room.pid,
		    ?DEBUG("MUC: send to process ~p~n", [Pid]),
		    mod_muc_room:route(Pid, From, Nick, Packet),
		    ok
	    end
    end.

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
		     HistorySize, RoomShaper) ->
    lists:foreach(
      fun(R) ->
		{Room, Host} = R#muc_room.name_host,
		case mnesia:dirty_read(muc_online_room, {Room, Host}) of
		    [] ->
			{ok, Pid} = mod_muc_room:start(Host,
				ServerHost, Access, Room,
				HistorySize, RoomShaper,
				R#muc_room.opts),
			register_room(Host, Room, Pid);
		    _ -> ok
		end
	end,
	get_rooms(ServerHost, Host)).

start_new_room(Host, ServerHost, Access, Room,
	    HistorySize, RoomShaper, From,
	    Nick, DefRoomOpts) ->
    case restore_room(ServerHost, Host, Room) of
	error ->
	    ?DEBUG("MUC: open new room '~s'~n", [Room]),
	    mod_muc_room:start(Host, ServerHost, Access, Room,
		HistorySize, RoomShaper,
		From, Nick, DefRoomOpts);
	Opts ->
	    ?DEBUG("MUC: restore room '~s'~n", [Room]),
	    mod_muc_room:start(Host, ServerHost, Access, Room,
		HistorySize, RoomShaper, Opts)
    end.

register_room(Host, Room, Pid) ->
    F = fun() ->
	    mnesia:write(#muc_online_room{name_host = {Room, Host},
		    pid = Pid})
    end,
    mnesia:transaction(F).


iq_disco_info(ServerHost, Lang) ->
    [#xmlel{name = <<"identity">>,
	    attrs =
		[{<<"category">>, <<"conference">>},
		 {<<"type">>, <<"text">>},
		 {<<"name">>,
		  translate:translate(Lang, <<"Chatrooms">>)}],
	    children = []},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_DISCO_INFO}], children = []},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_DISCO_ITEMS}], children = []},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_MUC}], children = []},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_MUC_UNIQUE}], children = []},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_REGISTER}], children = []},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_RSM}], children = []},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_VCARD}], children = []}] ++
	case gen_mod:is_loaded(ServerHost, mod_mam) of
	    true ->
		[#xmlel{name = <<"feature">>,
			attrs = [{<<"var">>, ?NS_MAM_TMP}]},
		 #xmlel{name = <<"feature">>,
			attrs = [{<<"var">>, ?NS_MAM_0}]},
		 #xmlel{name = <<"feature">>,
			attrs = [{<<"var">>, ?NS_MAM_1}]}];
	    false ->
		[]
	end.

iq_disco_items(Host, From, Lang, <<>>, none) ->
    Rooms = get_vh_rooms(Host),
    case erlang:length(Rooms) < ?MAX_ROOMS_DISCOITEMS of
	true ->
	    iq_disco_items_list(Host, Rooms, {get_disco_item, all, From, Lang});
	false ->
	    iq_disco_items(Host, From, Lang, <<"nonemptyrooms">>, none)
    end;
iq_disco_items(Host, From, Lang, <<"nonemptyrooms">>, none) ->
    XmlEmpty = #xmlel{name = <<"item">>,
				   attrs =
				       [{<<"jid">>, <<"conference.localhost">>},
					{<<"node">>, <<"emptyrooms">>},
					{<<"name">>, translate:translate(Lang, <<"Empty Rooms">>)}],
				   children = []},
    Query = {get_disco_item, only_non_empty, From, Lang},
    [XmlEmpty | iq_disco_items_list(Host, get_vh_rooms(Host), Query)];
iq_disco_items(Host, From, Lang, <<"emptyrooms">>, none) ->
    iq_disco_items_list(Host, get_vh_rooms(Host), {get_disco_item, 0, From, Lang});
iq_disco_items(Host, From, Lang, _DiscoNode, Rsm) ->
    {Rooms, RsmO} = get_vh_rooms(Host, Rsm),
    RsmOut = jlib:rsm_encode(RsmO),
    iq_disco_items_list(Host, Rooms, {get_disco_item, all, From, Lang}) ++ RsmOut.

iq_disco_items_list(Host, Rooms, Query) ->
    lists:zf(fun (#muc_online_room{name_host =
				       {Name, _Host},
				   pid = Pid}) ->
		     case catch gen_fsm:sync_send_all_state_event(Pid,
								  Query,
								  100)
			 of
		       {item, Desc} ->
			   flush(),
			   {true,
			    #xmlel{name = <<"item">>,
				   attrs =
				       [{<<"jid">>,
					 jid:to_string({Name, Host,
							     <<"">>})},
					{<<"name">>, Desc}],
				   children = []}};
		       _ -> false
		     end
	     end, Rooms).

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
    if L2 == [] -> {L2, #rsm_out{count = Count}};
       true ->
	   H = hd(L2),
	   NewIndex = get_room_pos(H, AllRooms),
	   T = lists:last(L2),
	   {F, _} = H#muc_online_room.name_host,
	   {Last, _} = T#muc_online_room.name_host,
	   {L2,
	    #rsm_out{first = F, last = Last, count = Count,
		     index = NewIndex}}
    end.

%% @doc Return the position of desired room in the list of rooms.
%% The room must exist in the list. The count starts in 0.
%% @spec (Desired::muc_online_room(), Rooms::[muc_online_room()]) -> integer()
get_room_pos(Desired, Rooms) ->
    get_room_pos(Desired, Rooms, 0).

get_room_pos(Desired, [HeadRoom | _], HeadPosition)
    when Desired#muc_online_room.name_host ==
	   HeadRoom#muc_online_room.name_host ->
    HeadPosition;
get_room_pos(Desired, [_ | Rooms], HeadPosition) ->
    get_room_pos(Desired, Rooms, HeadPosition + 1).

flush() -> receive _ -> flush() after 0 -> ok end.

-define(XFIELD(Type, Label, Var, Val),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"type">>, Type},
		    {<<"label">>, translate:translate(Lang, Label)},
		    {<<"var">>, Var}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children = [{xmlcdata, Val}]}]}).

iq_get_unique(From) ->
    {xmlcdata,
     p1_sha:sha(term_to_binary([From, p1_time_compat:timestamp(),
			     randoms:get_string()]))}.

get_nick(ServerHost, Host, From) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_nick(LServer, Host, From).

iq_get_register_info(ServerHost, Host, From, Lang) ->
    {Nick, Registered} = case get_nick(ServerHost, Host,
				       From)
			     of
			   error -> {<<"">>, []};
			   N ->
			       {N,
				[#xmlel{name = <<"registered">>, attrs = [],
					children = []}]}
			 end,
    Registered ++
      [#xmlel{name = <<"instructions">>, attrs = [],
	      children =
		  [{xmlcdata,
		    translate:translate(Lang,
					<<"You need a client that supports x:data "
					  "to register the nickname">>)}]},
       #xmlel{name = <<"x">>,
	      attrs = [{<<"xmlns">>, ?NS_XDATA},
		       {<<"type">>, <<"form">>}],
	      children =
		  [#xmlel{name = <<"title">>, attrs = [],
			  children =
			      [{xmlcdata,
				<<(translate:translate(Lang,
						       <<"Nickname Registration at ">>))/binary,
				  Host/binary>>}]},
		   #xmlel{name = <<"instructions">>, attrs = [],
			  children =
			      [{xmlcdata,
				translate:translate(Lang,
						    <<"Enter nickname you want to register">>)}]},
		   ?XFIELD(<<"text-single">>, <<"Nickname">>, <<"nick">>,
			   Nick)]}].

set_nick(ServerHost, Host, From, Nick) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:set_nick(LServer, Host, From, Nick).

iq_set_register_info(ServerHost, Host, From, Nick,
		     Lang) ->
    case set_nick(ServerHost, Host, From, Nick) of
      {atomic, ok} -> {result, []};
      {atomic, false} ->
	  ErrText = <<"That nickname is registered by another "
		      "person">>,
	  {error, ?ERRT_CONFLICT(Lang, ErrText)};
      _ ->
	  Txt = <<"Database failure">>,
	  {error, ?ERRT_INTERNAL_SERVER_ERROR(Lang, Txt)}
    end.

process_iq_register_set(ServerHost, Host, From, SubEl,
			Lang) ->
    #xmlel{children = Els} = SubEl,
    case fxml:get_subtag(SubEl, <<"remove">>) of
      false ->
	  case fxml:remove_cdata(Els) of
	    [#xmlel{name = <<"x">>} = XEl] ->
		case {fxml:get_tag_attr_s(<<"xmlns">>, XEl),
		      fxml:get_tag_attr_s(<<"type">>, XEl)}
		    of
		  {?NS_XDATA, <<"cancel">>} -> {result, []};
		  {?NS_XDATA, <<"submit">>} ->
		      XData = jlib:parse_xdata_submit(XEl),
		      case XData of
			invalid ->
			      Txt = <<"Incorrect data form">>,
			      {error, ?ERRT_BAD_REQUEST(Lang, Txt)};
			_ ->
			    case lists:keysearch(<<"nick">>, 1, XData) of
			      {value, {_, [Nick]}} when Nick /= <<"">> ->
				  iq_set_register_info(ServerHost, Host, From,
						       Nick, Lang);
			      _ ->
				  ErrText =
				      <<"You must fill in field \"Nickname\" "
					"in the form">>,
				  {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)}
			    end
		      end;
		  _ -> {error, ?ERR_BAD_REQUEST}
		end;
	    _ -> {error, ?ERR_BAD_REQUEST}
	  end;
      _ ->
	  iq_set_register_info(ServerHost, Host, From, <<"">>,
			       Lang)
    end.

iq_get_vcard(Lang) ->
    [#xmlel{name = <<"FN">>, attrs = [],
	    children = [{xmlcdata, <<"ejabberd/mod_muc">>}]},
     #xmlel{name = <<"URL">>, attrs = [],
	    children = [{xmlcdata, ?EJABBERD_URI}]},
     #xmlel{name = <<"DESC">>, attrs = [],
	    children =
		[{xmlcdata,
		  <<(translate:translate(Lang,
					 <<"ejabberd MUC module">>))/binary,
		    "\nCopyright (c) 2003-2016 ProcessOne">>}]}].

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

import(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:import(LServer).

import(LServer, DBType, Data) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, Data).

mod_opt_type(access) ->
    fun acl:access_rules_validator/1;
mod_opt_type(access_admin) ->
    fun acl:access_rules_validator/1;
mod_opt_type(access_create) ->
    fun acl:access_rules_validator/1;
mod_opt_type(access_persistent) ->
    fun acl:access_rules_validator/1;
mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
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
mod_opt_type(_) ->
    [access, access_admin, access_create, access_persistent,
     db_type, default_room_options, history_size, host,
     max_room_desc, max_room_id, max_room_name, regexp_room_id,
     max_user_conferences, max_users,
     max_users_admin_threshold, max_users_presence,
     min_message_interval, min_presence_interval,
     room_shaper, user_message_shaper, user_presence_shaper].
