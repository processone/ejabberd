%%%----------------------------------------------------------------------
%%% File    : mod_muc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC support (XEP-0045)
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2020   ProcessOne
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
-ifndef(GEN_SERVER).
-define(GEN_SERVER, gen_server).
-endif.
-behaviour(?GEN_SERVER).
-behaviour(gen_mod).

%% API
-export([start/2,
	 stop/1,
	 start_link/2,
	 reload/3,
         mod_doc/0,
	 room_destroyed/4,
	 store_room/4,
	 store_room/5,
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
	 iq_set_register_info/5,
	 count_online_rooms_by_user/3,
	 get_online_rooms_by_user/3,
	 can_use_nick/4,
	 get_subscribed_rooms/2,
	 procname/2,
	 route/1]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3,
	 mod_opt_type/1, mod_options/1, depends/2]).

-include("logger.hrl").
-include("xmpp.hrl").
-include("mod_muc.hrl").
-include("mod_muc_room.hrl").
-include("translate.hrl").
-include("ejabberd_stacktrace.hrl").

-type state() :: #{hosts := [binary()],
		   server_host := binary(),
		   worker := pos_integer()}.
-type access() :: {acl:acl(), acl:acl(), acl:acl(), acl:acl(), acl:acl()}.
-type muc_room_opts() :: [{atom(), any()}].
-export_type([access/0]).
-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), binary(), [binary()]) -> ok.
-callback store_room(binary(), binary(), binary(), list(), list()|undefined) -> {atomic, any()}.
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
-callback get_subscribed_rooms(binary(), binary(), jid()) ->
          {ok, [{jid(), binary(), [binary()]}]} | {error, db_failure}.

-optional_callbacks([get_subscribed_rooms/3]).

%%====================================================================
%% API
%%====================================================================
start(Host, Opts) ->
    case mod_muc_sup:start(Host) of
	{ok, _} ->
	    MyHosts = gen_mod:get_opt_hosts(Opts),
	    Mod = gen_mod:db_mod(Opts, ?MODULE),
	    RMod = gen_mod:ram_db_mod(Opts, ?MODULE),
	    Mod:init(Host, gen_mod:set_opt(hosts, MyHosts, Opts)),
	    RMod:init(Host, gen_mod:set_opt(hosts, MyHosts, Opts)),
	    load_permanent_rooms(MyHosts, Host, Opts);
	Err ->
	    Err
    end.

stop(Host) ->
    Proc = mod_muc_sup:procname(Host),
    supervisor:terminate_child(ejabberd_gen_mod_sup, Proc),
    supervisor:delete_child(ejabberd_gen_mod_sup, Proc).

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(ServerHost, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(NewOpts, ?MODULE),
    NewRMod = gen_mod:ram_db_mod(NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(OldOpts, ?MODULE),
    OldRMod = gen_mod:ram_db_mod(OldOpts, ?MODULE),
    NewHosts = gen_mod:get_opt_hosts(NewOpts),
    OldHosts = gen_mod:get_opt_hosts(OldOpts),
    AddHosts = NewHosts -- OldHosts,
    DelHosts = OldHosts -- NewHosts,
    if NewMod /= OldMod ->
	    NewMod:init(ServerHost, gen_mod:set_opt(hosts, NewHosts, NewOpts));
       true ->
	    ok
    end,
    if NewRMod /= OldRMod ->
	    NewRMod:init(ServerHost, gen_mod:set_opt(hosts, NewHosts, NewOpts));
       true ->
	    ok
    end,
    lists:foreach(
      fun(I) ->
	      ?GEN_SERVER:cast(procname(ServerHost, I),
			       {reload, AddHosts, DelHosts, NewHosts})
      end, lists:seq(1, erlang:system_info(logical_processors))),
    load_permanent_rooms(AddHosts, ServerHost, NewOpts),
    shutdown_rooms(ServerHost, DelHosts, OldRMod),
    lists:foreach(
      fun(Host) ->
	      lists:foreach(
		fun({_, _, Pid}) when node(Pid) == node() ->
			mod_muc_room:config_reloaded(Pid);
		   (_) ->
			ok
		end, get_online_rooms(ServerHost, Host))
      end, misc:intersection(NewHosts, OldHosts)).

depends(_Host, _Opts) ->
    [{mod_mam, soft}].

start_link(Host, I) ->
    Proc = procname(Host, I),
    ?GEN_SERVER:start_link({local, Proc}, ?MODULE, [Host, I],
			   ejabberd_config:fsm_limit_opts([])).

-spec procname(binary(), pos_integer() | {binary(), binary()}) -> atom().
procname(Host, I) when is_integer(I) ->
    binary_to_atom(
      <<(atom_to_binary(?MODULE, latin1))/binary, "_", Host/binary,
	"_", (integer_to_binary(I))/binary>>, utf8);
procname(Host, RoomHost) ->
    Cores = erlang:system_info(logical_processors),
    I = erlang:phash2(RoomHost, Cores) + 1,
    procname(Host, I).

-spec route(stanza()) -> ok.
route(Pkt) ->
    To = xmpp:get_to(Pkt),
    ServerHost = ejabberd_router:host_of_route(To#jid.lserver),
    route(Pkt, ServerHost).

-spec route(stanza(), binary()) -> ok.
route(Pkt, ServerHost) ->
    From = xmpp:get_from(Pkt),
    To = xmpp:get_to(Pkt),
    Host = To#jid.lserver,
    Access = mod_muc_opt:access(ServerHost),
    case acl:match_rule(ServerHost, Access, From) of
	allow ->
	    route(Pkt, Host, ServerHost);
	deny ->
	    Lang = xmpp:get_lang(Pkt),
            ErrText = ?T("Access denied by service policy"),
            Err = xmpp:err_forbidden(ErrText, Lang),
            ejabberd_router:route_error(Pkt, Err)
    end.

-spec route(stanza(), binary(), binary()) -> ok.
route(#iq{to = #jid{luser = <<"">>, lresource = <<"">>}} = IQ, _, _) ->
    ejabberd_router:process_iq(IQ);
route(#message{lang = Lang, body = Body, type = Type, from = From,
	       to = #jid{luser = <<"">>, lresource = <<"">>}} = Pkt,
      Host, ServerHost) ->
    if Type == error ->
            ok;
       true ->
	    AccessAdmin = mod_muc_opt:access_admin(ServerHost),
            case acl:match_rule(ServerHost, AccessAdmin, From) of
                allow ->
                    Msg = xmpp:get_text(Body),
                    broadcast_service_message(ServerHost, Host, Msg);
                deny ->
                    ErrText = ?T("Only service administrators are allowed "
                                 "to send service messages"),
                    Err = xmpp:err_forbidden(ErrText, Lang),
                    ejabberd_router:route_error(Pkt, Err)
            end
    end;
route(Pkt, Host, ServerHost) ->
    {Room, _, _} = jid:tolower(xmpp:get_to(Pkt)),
    case Room of
	<<"">> ->
	    Txt = ?T("No module is handling this query"),
	    Err = xmpp:err_service_unavailable(Txt, xmpp:get_lang(Pkt)),
	    ejabberd_router:route_error(Pkt, Err);
       _ ->
	    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
	    case RMod:find_online_room(ServerHost, Room, Host) of
		error ->
		    Proc = procname(ServerHost, {Room, Host}),
		    case whereis(Proc) of
			Pid when Pid == self() ->
			    route_to_room(Pkt, ServerHost);
			Pid when is_pid(Pid) ->
			    ?DEBUG("Routing to MUC worker ~p:~n~ts", [Proc, xmpp:pp(Pkt)]),
			    ?GEN_SERVER:cast(Pid, {route_to_room, Pkt});
			undefined ->
			    ?DEBUG("MUC worker ~p is dead", [Proc]),
			    Err = xmpp:err_internal_server_error(),
			    ejabberd_router:route_error(Pkt, Err)
		    end;
		{ok, Pid} ->
		    mod_muc_room:route(Pid, Pkt)
	    end
    end.

-spec shutdown_rooms(binary()) -> [pid()].
shutdown_rooms(ServerHost) ->
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    Hosts = gen_mod:get_module_opt_hosts(ServerHost, mod_muc),
    shutdown_rooms(ServerHost, Hosts, RMod).

-spec shutdown_rooms(binary(), [binary()], module()) -> [pid()].
shutdown_rooms(ServerHost, Hosts, RMod) ->
    Rooms = [RMod:get_online_rooms(ServerHost, Host, undefined)
	     || Host <- Hosts],
    lists:flatmap(
      fun({_, _, Pid}) when node(Pid) == node() ->
	      mod_muc_room:shutdown(Pid),
	      [Pid];
	 (_) ->
	      []
      end, lists:flatten(Rooms)).

%% This function is called by a room in three situations:
%% A) The owner of the room destroyed it
%% B) The only participant of a temporary room leaves it
%% C) mod_muc:stop was called, and each room is being terminated
%%    In this case, the mod_muc process died before the room processes
%%    So the message sending must be caught
-spec room_destroyed(binary(), binary(), pid(), binary()) -> ok.
room_destroyed(Host, Room, Pid, ServerHost) ->
    Proc = procname(ServerHost, {Room, Host}),
    ?GEN_SERVER:cast(Proc, {room_destroyed, {Room, Host}, Pid}).

%% @doc Create a room.
%% If Opts = default, the default room options are used.
%% Else use the passed options as defined in mod_muc_room.
create_room(Host, Name, From, Nick, Opts) ->
    ServerHost = ejabberd_router:host_of_route(Host),
    Proc = procname(ServerHost, {Name, Host}),
    ?GEN_SERVER:call(Proc, {create, Name, Host, From, Nick, Opts}).

store_room(ServerHost, Host, Name, Opts) ->
    store_room(ServerHost, Host, Name, Opts, undefined).

store_room(ServerHost, Host, Name, Opts, ChangesHints) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:store_room(LServer, Host, Name, Opts, ChangesHints).

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
-spec init(list()) -> {ok, state()}.
init([Host, Worker]) ->
    process_flag(trap_exit, true),
    Opts = gen_mod:get_module_opts(Host, ?MODULE),
    MyHosts = gen_mod:get_opt_hosts(Opts),
    register_routes(Host, MyHosts, Worker),
    register_iq_handlers(MyHosts, Worker),
    {ok, #{server_host => Host, hosts => MyHosts, worker => Worker}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
			 {reply, ok | {ok, pid()} | {error, any()}, state()} |
			 {stop, normal, ok, state()}.
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({create, Room, Host, From, Nick, Opts}, _From,
	    #{server_host := ServerHost} = State) ->
    ?DEBUG("MUC: create new room '~ts'~n", [Room]),
    NewOpts = case Opts of
		  default -> mod_muc_opt:default_room_options(ServerHost);
		  _ -> Opts
	      end,
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    case start_room(RMod, Host, ServerHost, Room, NewOpts, From, Nick) of
	{ok, _} ->
	    ejabberd_hooks:run(create_room, ServerHost, [ServerHost, Room, Host]),
	    {reply, ok, State};
	Err ->
	    {reply, Err, State}
    end.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({route_to_room, Packet}, #{server_host := ServerHost} = State) ->
    try route_to_room(Packet, ServerHost)
    catch ?EX_RULE(Class, Reason, St) ->
            StackTrace = ?EX_STACK(St),
            ?ERROR_MSG("Failed to route packet:~n~ts~n** ~ts",
		       [xmpp:pp(Packet),
			misc:format_exception(2, Class, Reason, StackTrace)])
    end,
    {noreply, State};
handle_cast({room_destroyed, {Room, Host}, Pid},
	    #{server_host := ServerHost} = State) ->
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    RMod:unregister_online_room(ServerHost, Room, Host, Pid),
    {noreply, State};
handle_cast({reload, AddHosts, DelHosts, NewHosts},
	    #{server_host := ServerHost, worker := Worker} = State) ->
    register_routes(ServerHost, AddHosts, Worker),
    register_iq_handlers(AddHosts, Worker),
    unregister_routes(DelHosts, Worker),
    unregister_iq_handlers(DelHosts, Worker),
    {noreply, State#{hosts => NewHosts}};
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({route, Packet}, #{server_host := ServerHost} = State) ->
    %% We can only receive the packet here from other nodes
    %% where mod_muc is not loaded. Such configuration
    %% is *highly* discouraged
    try route(Packet, ServerHost)
    catch ?EX_RULE(Class, Reason, St) ->
            StackTrace = ?EX_STACK(St),
            ?ERROR_MSG("Failed to route packet:~n~ts~n** ~ts",
		       [xmpp:pp(Packet),
			misc:format_exception(2, Class, Reason, StackTrace)])
    end,
    {noreply, State};
handle_info({room_destroyed, {Room, Host}, Pid}, State) ->
    %% For backward compat
    handle_cast({room_destroyed, {Room, Host}, Pid}, State);
handle_info(Info, State) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(term(), state()) -> any().
terminate(_Reason, #{hosts := Hosts, worker := Worker}) ->
    unregister_routes(Hosts, Worker),
    unregister_iq_handlers(Hosts, Worker).

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec register_iq_handlers([binary()], pos_integer()) -> ok.
register_iq_handlers(Hosts, 1) ->
    %% Only register handlers on first worker
    lists:foreach(
      fun(Host) ->
	      gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_REGISTER,
					    ?MODULE, process_register),
	      gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_VCARD,
					    ?MODULE, process_vcard),
	      gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MUCSUB,
					    ?MODULE, process_mucsub),
	      gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MUC_UNIQUE,
					    ?MODULE, process_muc_unique),
	      gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO,
					    ?MODULE, process_disco_info),
	      gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS,
					    ?MODULE, process_disco_items)
      end, Hosts);
register_iq_handlers(_, _) ->
    ok.

-spec unregister_iq_handlers([binary()], pos_integer()) -> ok.
unregister_iq_handlers(Hosts, 1) ->
    %% Only unregister handlers on first worker
    lists:foreach(
      fun(Host) ->
	      gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_REGISTER),
	      gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VCARD),
	      gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_MUCSUB),
	      gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_MUC_UNIQUE),
	      gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO),
	      gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS)
      end, Hosts);
unregister_iq_handlers(_, _) ->
    ok.

-spec register_routes(binary(), [binary()], pos_integer()) -> ok.
register_routes(ServerHost, Hosts, 1) ->
    %% Only register routes on first worker
    lists:foreach(
      fun(Host) ->
	      ejabberd_router:register_route(
		Host, ServerHost, {apply, ?MODULE, route})
      end, Hosts);
register_routes(_, _, _) ->
    ok.

-spec unregister_routes([binary()], pos_integer()) -> ok.
unregister_routes(Hosts, 1) ->
    %% Only unregister routes on first worker
    lists:foreach(
      fun(Host) ->
	      ejabberd_router:unregister_route(Host)
      end, Hosts);
unregister_routes(_, _) ->
    ok.

%% Function copied from mod_muc_room.erl
-spec extract_password(presence() | iq()) -> binary() | false.
extract_password(#presence{} = Pres) ->
    case xmpp:get_subtag(Pres, #muc{}) of
        #muc{password = Password} when is_binary(Password) ->
            Password;
        _ ->
            false
    end;
extract_password(#iq{} = IQ) ->
    case xmpp:get_subtag(IQ, #muc_subscribe{}) of
        #muc_subscribe{password = Password} when Password /= <<"">> ->
            Password;
        _ ->
            false
    end.

-spec route_to_room(stanza(), binary()) -> ok.
route_to_room(Packet, ServerHost) ->
    From = xmpp:get_from(Packet),
    To = xmpp:get_to(Packet),
    {Room, Host, Nick} = jid:tolower(To),
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    case RMod:find_online_room(ServerHost, Room, Host) of
	error ->
	    case should_start_room(Packet) of
		false ->
		    Lang = xmpp:get_lang(Packet),
		    ErrText = ?T("Conference room does not exist"),
		    Err = xmpp:err_item_not_found(ErrText, Lang),
		    ejabberd_router:route_error(Packet, Err);
		StartType ->
		    case load_room(RMod, Host, ServerHost, Room) of
			{error, notfound} when StartType == start ->
			    case check_create_room(ServerHost, Host, Room, From) of
				true ->
				    Pass = extract_password(Packet),
				    case start_new_room(RMod, Host, ServerHost, Room, Pass, From, Nick) of
					{ok, Pid} ->
					    mod_muc_room:route(Pid, Packet);
					_Err ->
					    Err = xmpp:err_internal_server_error(),
					    ejabberd_router:route_error(Packet, Err)
				    end;
				false ->
				    Lang = xmpp:get_lang(Packet),
				    ErrText = ?T("Room creation is denied by service policy"),
				    Err = xmpp:err_forbidden(ErrText, Lang),
				    ejabberd_router:route_error(Packet, Err)
			    end;
			{error, notfound} ->
			    Lang = xmpp:get_lang(Packet),
			    ErrText = ?T("Conference room does not exist"),
			    Err = xmpp:err_item_not_found(ErrText, Lang),
			    ejabberd_router:route_error(Packet, Err);
			{error, _} ->
			    Err = xmpp:err_internal_server_error(),
			    ejabberd_router:route_error(Packet, Err);
			{ok, Pid2} ->
			    mod_muc_room:route(Pid2, Packet)
		    end
	    end;
	{ok, Pid} ->
	    mod_muc_room:route(Pid, Packet)
    end.

-spec process_vcard(iq()) -> iq().
process_vcard(#iq{type = get, to = To, lang = Lang, sub_els = [#vcard_temp{}]} = IQ) ->
    ServerHost = ejabberd_router:host_of_route(To#jid.lserver),
    VCard = case mod_muc_opt:vcard(ServerHost) of
		undefined ->
		    #vcard_temp{fn = <<"ejabberd/mod_muc">>,
				url = ejabberd_config:get_uri(),
				desc = misc:get_descr(Lang, ?T("ejabberd MUC module"))};
		V ->
		    V
	    end,
    xmpp:make_iq_result(IQ, VCard);
process_vcard(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_vcard(#iq{lang = Lang} = IQ) ->
    Txt = ?T("No module is handling this query"),
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec process_register(iq()) -> iq().
process_register(#iq{type = Type, from = From, to = To, lang = Lang,
		     sub_els = [El = #register{}]} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    AccessRegister = mod_muc_opt:access_register(ServerHost),
    case acl:match_rule(ServerHost, AccessRegister, From) of
	allow ->
	    case Type of
		get ->
		    xmpp:make_iq_result(
		      IQ, iq_get_register_info(ServerHost, Host, From, Lang));
		set ->
		    case process_iq_register_set(ServerHost, Host, From, El, Lang) of
			{result, Result} ->
			    xmpp:make_iq_result(IQ, Result);
			{error, Err} ->
			    xmpp:make_error(IQ, Err)
		    end
	    end;
	deny ->
	    ErrText = ?T("Access denied by service policy"),
	    Err = xmpp:err_forbidden(ErrText, Lang),
	    xmpp:make_error(IQ, Err)
    end.

-spec process_disco_info(iq()) -> iq().
process_disco_info(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_info(#iq{type = get, from = From, to = To, lang = Lang,
		       sub_els = [#disco_info{node = <<"">>}]} = IQ) ->
    ServerHost = ejabberd_router:host_of_route(To#jid.lserver),
    RMod = gen_mod:ram_db_mod(ServerHost, ?MODULE),
    AccessRegister = mod_muc_opt:access_register(ServerHost),
    X = ejabberd_hooks:run_fold(disco_info, ServerHost, [],
				[ServerHost, ?MODULE, <<"">>, Lang]),
    MAMFeatures = case gen_mod:is_loaded(ServerHost, mod_mam) of
		      true -> [?NS_MAM_TMP, ?NS_MAM_0, ?NS_MAM_1, ?NS_MAM_2];
		      false -> []
		  end,
    RSMFeatures = case RMod:rsm_supported() of
		      true -> [?NS_RSM];
		      false -> []
		  end,
    RegisterFeatures = case acl:match_rule(ServerHost, AccessRegister, From) of
			   allow -> [?NS_REGISTER];
			   deny -> []
		       end,
    Features = [?NS_DISCO_INFO, ?NS_DISCO_ITEMS,
		?NS_MUC, ?NS_VCARD, ?NS_MUCSUB, ?NS_MUC_UNIQUE
		| RegisterFeatures ++ RSMFeatures ++ MAMFeatures],
    Name = mod_muc_opt:name(ServerHost),
    Identity = #identity{category = <<"conference">>,
			 type = <<"text">>,
			 name = translate:translate(Lang, Name)},
    xmpp:make_iq_result(
      IQ, #disco_info{features = Features,
		      identities = [Identity],
		      xdata = X});
process_disco_info(#iq{type = get, lang = Lang,
		       sub_els = [#disco_info{}]} = IQ) ->
    xmpp:make_error(IQ, xmpp:err_item_not_found(?T("Node not found"), Lang));
process_disco_info(#iq{lang = Lang} = IQ) ->
    Txt = ?T("No module is handling this query"),
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec process_disco_items(iq()) -> iq().
process_disco_items(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_items(#iq{type = get, from = From, to = To, lang = Lang,
			sub_els = [#disco_items{node = Node, rsm = RSM}]} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    MaxRoomsDiscoItems = mod_muc_opt:max_rooms_discoitems(ServerHost),
    case iq_disco_items(ServerHost, Host, From, Lang,
			MaxRoomsDiscoItems, Node, RSM) of
	{error, Err} ->
	    xmpp:make_error(IQ, Err);
	{result, Result} ->
	    xmpp:make_iq_result(IQ, Result)
    end;
process_disco_items(#iq{lang = Lang} = IQ) ->
    Txt = ?T("No module is handling this query"),
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec process_muc_unique(iq()) -> iq().
process_muc_unique(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_muc_unique(#iq{from = From, type = get,
		       sub_els = [#muc_unique{}]} = IQ) ->
    Name = str:sha(term_to_binary([From, erlang:timestamp(),
				      p1_rand:get_string()])),
    xmpp:make_iq_result(IQ, #muc_unique{name = Name}).

-spec process_mucsub(iq()) -> iq().
process_mucsub(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_mucsub(#iq{type = get, from = From, to = To, lang = Lang,
		   sub_els = [#muc_subscriptions{}]} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    case get_subscribed_rooms(ServerHost, Host, From) of
	{ok, Subs} ->
	    List = [#muc_subscription{jid = JID, nick = Nick, events = Nodes}
		    || {JID, Nick, Nodes} <- Subs],
	    xmpp:make_iq_result(IQ, #muc_subscriptions{list = List});
	{error, _} ->
	    Txt = ?T("Database failure"),
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
    end;
process_mucsub(#iq{lang = Lang} = IQ) ->
    Txt = ?T("No module is handling this query"),
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec should_start_room(stanza()) -> start | load | false.
should_start_room(#presence{type = available}) ->
    start;
should_start_room(#iq{type = T} = IQ) when T == get; T == set ->
    case xmpp:has_subtag(IQ, #muc_subscribe{}) orelse
	 xmpp:has_subtag(IQ, #muc_owner{}) of
	true ->
	    start;
	_ ->
	    load
    end;
should_start_room(#message{type = T, to = #jid{lresource = <<>>}})
    when T == groupchat; T == normal->
    load;
should_start_room(#message{type = T, to = #jid{lresource = Res}})
    when Res /= <<>> andalso T /= groupchat andalso T /= error ->
    load;
should_start_room(_) ->
    false.

-spec check_create_room(binary(), binary(), binary(), jid()) -> boolean().
check_create_room(ServerHost, Host, Room, From) ->
    AccessCreate = mod_muc_opt:access_create(ServerHost),
    case acl:match_rule(ServerHost, AccessCreate, From) of
	allow ->
	    case mod_muc_opt:max_room_id(ServerHost) of
		Max when byte_size(Room) =< Max ->
		    Regexp = mod_muc_opt:regexp_room_id(ServerHost),
		    case re:run(Room, Regexp, [{capture, none}]) of
			match ->
			    AccessAdmin = mod_muc_opt:access_admin(ServerHost),
			    case acl:match_rule(ServerHost, AccessAdmin, From) of
				allow ->
				    true;
				_ ->
				    ejabberd_hooks:run_fold(
				      check_create_room, ServerHost, true,
				      [ServerHost, Room, Host])
			    end;
			_ ->
			    false
		    end;
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

-spec get_access(binary() | gen_mod:opts()) -> access().
get_access(ServerHost) ->
    Access = mod_muc_opt:access(ServerHost),
    AccessCreate = mod_muc_opt:access_create(ServerHost),
    AccessAdmin = mod_muc_opt:access_admin(ServerHost),
    AccessPersistent = mod_muc_opt:access_persistent(ServerHost),
    AccessMam = mod_muc_opt:access_mam(ServerHost),
    {Access, AccessCreate, AccessAdmin, AccessPersistent, AccessMam}.

-spec get_rooms(binary(), binary()) -> [#muc_room{}].
get_rooms(ServerHost, Host) ->
    Mod = gen_mod:db_mod(ServerHost, ?MODULE),
    Mod:get_rooms(ServerHost, Host).

-spec load_permanent_rooms([binary()], binary(), gen_mod:opts()) -> ok.
load_permanent_rooms(Hosts, ServerHost, Opts) ->
    case mod_muc_opt:preload_rooms(Opts) of
	true ->
	    Access = get_access(Opts),
	    HistorySize = mod_muc_opt:history_size(Opts),
	    QueueType = mod_muc_opt:queue_type(Opts),
	    RoomShaper = mod_muc_opt:room_shaper(Opts),
	    RMod = gen_mod:ram_db_mod(Opts, ?MODULE),
	    lists:foreach(
	      fun(Host) ->
		      ?DEBUG("Loading rooms at ~ts", [Host]),
		      lists:foreach(
			fun(R) ->
				{Room, _} = R#muc_room.name_host,
				case RMod:find_online_room(ServerHost, Room, Host) of
				    error ->
					start_room(RMod, Host, ServerHost, Access,
						   Room, HistorySize, RoomShaper,
						   R#muc_room.opts, QueueType);
				    {ok, _} ->
					ok
				end
			end, get_rooms(ServerHost, Host))
	      end, Hosts);
	false ->
	    ok
    end.

-spec load_room(module(), binary(), binary(), binary()) -> {ok, pid()} |
							   {error, notfound | term()}.
load_room(RMod, Host, ServerHost, Room) ->
    case restore_room(ServerHost, Host, Room) of
	error ->
	    {error, notfound};
	Opts0 ->
	    case proplists:get_bool(persistent, Opts0) of
		true ->
		    ?DEBUG("Restore room: ~ts", [Room]),
		    start_room(RMod, Host, ServerHost, Room, Opts0);
		_ ->
		    ?DEBUG("Restore hibernated non-persistent room: ~ts", [Room]),
		    Res = start_room(RMod, Host, ServerHost, Room, Opts0),
		    Mod = gen_mod:db_mod(ServerHost, mod_muc),
		    case erlang:function_exported(Mod, get_subscribed_rooms, 3) of
			true ->
			    ok;
			_ ->
			    forget_room(ServerHost, Host, Room)
		    end,
		    Res
	    end
    end.

start_new_room(RMod, Host, ServerHost, Room, Pass, From, Nick) ->
    ?DEBUG("Open new room: ~ts", [Room]),
    DefRoomOpts = mod_muc_opt:default_room_options(ServerHost),
    DefRoomOpts2 = add_password_options(Pass, DefRoomOpts),
    start_room(RMod, Host, ServerHost, Room, DefRoomOpts2, From, Nick).

add_password_options(false, DefRoomOpts) ->
    DefRoomOpts;
add_password_options(<<>>, DefRoomOpts) ->
    DefRoomOpts;
add_password_options(Pass, DefRoomOpts) when is_binary(Pass) ->
    O2 = lists:keystore(password, 1, DefRoomOpts, {password, Pass}),
    lists:keystore(password_protected, 1, O2, {password_protected, true}).

start_room(Mod, Host, ServerHost, Room, DefOpts) ->
    Access = get_access(ServerHost),
    HistorySize = mod_muc_opt:history_size(ServerHost),
    QueueType = mod_muc_opt:queue_type(ServerHost),
    RoomShaper = mod_muc_opt:room_shaper(ServerHost),
    start_room(Mod, Host, ServerHost, Access, Room, HistorySize,
	       RoomShaper, DefOpts, QueueType).

start_room(Mod, Host, ServerHost, Room, DefOpts, Creator, Nick) ->
    Access = get_access(ServerHost),
    HistorySize = mod_muc_opt:history_size(ServerHost),
    QueueType = mod_muc_opt:queue_type(ServerHost),
    RoomShaper = mod_muc_opt:room_shaper(ServerHost),
    start_room(Mod, Host, ServerHost, Access, Room,
	       HistorySize, RoomShaper,
	       Creator, Nick, DefOpts, QueueType).

start_room(Mod, Host, ServerHost, Access, Room,
	   HistorySize, RoomShaper, DefOpts, QueueType) ->
    case mod_muc_room:start(Host, ServerHost, Access, Room,
			    HistorySize, RoomShaper, DefOpts, QueueType) of
	{ok, Pid} ->
	    Mod:register_online_room(ServerHost, Room, Host, Pid),
	    {ok, Pid};
	Err ->
	    Err
    end.

start_room(Mod, Host, ServerHost, Access, Room, HistorySize,
	   RoomShaper, Creator, Nick, DefOpts, QueueType) ->
    case mod_muc_room:start(Host, ServerHost, Access, Room,
			    HistorySize, RoomShaper,
			    Creator, Nick, DefOpts, QueueType) of
	{ok, Pid} ->
	    Mod:register_online_room(ServerHost, Room, Host, Pid),
	    {ok, Pid};
	Err ->
	    Err
    end.

-spec iq_disco_items(binary(), binary(), jid(), binary(), integer(), binary(),
		     rsm_set() | undefined) ->
			    {result, disco_items()} | {error, stanza_error()}.
iq_disco_items(ServerHost, Host, From, Lang, MaxRoomsDiscoItems, Node, RSM)
  when Node == <<"">>; Node == <<"nonemptyrooms">>; Node == <<"emptyrooms">> ->
    Count = count_online_rooms(ServerHost, Host),
    Query = if Node == <<"">>, RSM == undefined, Count > MaxRoomsDiscoItems ->
		    {only_non_empty, From, Lang};
	       Node == <<"nonemptyrooms">> ->
		    {only_non_empty, From, Lang};
	       Node == <<"emptyrooms">> ->
		    {0, From, Lang};
	       true ->
		    {all, From, Lang}
	    end,
    MaxItems = case RSM of
		   undefined ->
		       MaxRoomsDiscoItems;
		   #rsm_set{max = undefined} ->
		       MaxRoomsDiscoItems;
		   #rsm_set{max = Max} when Max > MaxRoomsDiscoItems ->
		       MaxRoomsDiscoItems;
		   #rsm_set{max = Max} ->
		       Max
	       end,
    {Items, HitMax} = lists:foldr(
	fun(_, {Acc, _}) when length(Acc) >= MaxItems ->
	    {Acc, true};
	   (R, {Acc, _}) ->
	    case get_room_disco_item(R, Query) of
		{ok, Item} -> {[Item | Acc], false};
		{error, _} -> {Acc, false}
	    end
	end, {[], false}, get_online_rooms(ServerHost, Host, RSM)),
    ResRSM = case Items of
		 [_|_] when RSM /= undefined; HitMax ->
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
    {error, xmpp:err_item_not_found(?T("Node not found"), Lang)}.

-spec get_room_disco_item({binary(), binary(), pid()},
			  {mod_muc_room:disco_item_filter(),
			   jid(), binary()}) -> {ok, disco_item()} |
						{error, timeout | notfound}.
get_room_disco_item({Name, Host, Pid}, {Filter, JID, Lang}) ->
    case mod_muc_room:get_disco_item(Pid, Filter, JID, Lang) of
	{ok, Desc} ->
	    RoomJID = jid:make(Name, Host),
	    {ok, #disco_item{jid = RoomJID, name = Desc}};
	{error, _} = Err ->
	    Err
    end.

-spec get_subscribed_rooms(binary(), jid()) -> {ok, [{jid(), binary(), [binary()]}]} | {error, any()}.
get_subscribed_rooms(Host, User) ->
    ServerHost = ejabberd_router:host_of_route(Host),
    get_subscribed_rooms(ServerHost, Host, User).

-spec get_subscribed_rooms(binary(), binary(), jid()) ->
			   {ok, [{jid(), binary(), [binary()]}]} | {error, any()}.
get_subscribed_rooms(ServerHost, Host, From) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    BareFrom = jid:remove_resource(From),
    case erlang:function_exported(Mod, get_subscribed_rooms, 3) of
	false ->
	    Rooms = get_online_rooms(ServerHost, Host),
	    {ok, lists:flatmap(
		   fun({Name, _, Pid}) when Pid == self() ->
		       USR = jid:split(BareFrom),
		       case erlang:get(muc_subscribers) of
			   #{USR := #subscriber{nodes = Nodes, nick = Nick}} ->
			       [{jid:make(Name, Host), Nick, Nodes}];
			   _ ->
			       []
		       end;
		       ({Name, _, Pid}) ->
			   case mod_muc_room:is_subscribed(Pid, BareFrom) of
			       {true, Nick, Nodes} ->
				   [{jid:make(Name, Host), Nick, Nodes}];
			       false -> []
			   end;
		      (_) ->
			   []
		   end, Rooms)};
	true ->
	    Mod:get_subscribed_rooms(LServer, Host, BareFrom)
    end.

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
		 Lang, ?T("Nickname Registration at ")))/binary, Host/binary>>,
    Inst = translate:translate(Lang, ?T("Enter nickname you want to register")),
    Fields = muc_register:encode([{roomnick, Nick}], Lang),
    X = #xdata{type = form, title = Title,
	       instructions = [Inst], fields = Fields},
    #register{nick = Nick,
	      registered = Registered,
	      instructions =
		  translate:translate(
		    Lang, ?T("You need a client that supports x:data "
			     "to register the nickname")),
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
	  ErrText = ?T("That nickname is registered by another person"),
	  {error, xmpp:err_conflict(ErrText, Lang)};
      _ ->
	  Txt = ?T("Database failure"),
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
	    Txt = ?T("Incorrect data form"),
	    {error, xmpp:err_bad_request(Txt, Lang)};
	_ when is_binary(Nick), Nick /= <<"">> ->
	    iq_set_register_info(ServerHost, Host, From, Nick, Lang);
	_ ->
	    ErrText = ?T("You must fill in field \"Nickname\" in the form"),
	    {error, xmpp:err_not_acceptable(ErrText, Lang)}
    end.

-spec broadcast_service_message(binary(), binary(), binary()) -> ok.
broadcast_service_message(ServerHost, Host, Msg) ->
    lists:foreach(
      fun({_, _, Pid}) ->
	      mod_muc_room:service_message(Pid, Msg)
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
         ({subject, [C|_] = Subj}) when is_integer(C), C >= 0, C =< 255 ->
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
    econf:acl();
mod_opt_type(access_admin) ->
    econf:acl();
mod_opt_type(access_create) ->
    econf:acl();
mod_opt_type(access_persistent) ->
    econf:acl();
mod_opt_type(access_mam) ->
    econf:acl();
mod_opt_type(access_register) ->
    econf:acl();
mod_opt_type(history_size) ->
    econf:non_neg_int();
mod_opt_type(name) ->
    econf:binary();
mod_opt_type(max_room_desc) ->
    econf:pos_int(infinity);
mod_opt_type(max_room_id) ->
    econf:pos_int(infinity);
mod_opt_type(max_rooms_discoitems) ->
    econf:non_neg_int();
mod_opt_type(regexp_room_id) ->
    econf:re([unicode]);
mod_opt_type(max_room_name) ->
    econf:pos_int(infinity);
mod_opt_type(max_user_conferences) ->
    econf:pos_int();
mod_opt_type(max_users) ->
    econf:pos_int();
mod_opt_type(max_users_admin_threshold) ->
    econf:pos_int();
mod_opt_type(max_users_presence) ->
    econf:int();
mod_opt_type(min_message_interval) ->
    econf:number(0);
mod_opt_type(min_presence_interval) ->
    econf:number(0);
mod_opt_type(preload_rooms) ->
    econf:bool();
mod_opt_type(room_shaper) ->
    econf:atom();
mod_opt_type(user_message_shaper) ->
    econf:atom();
mod_opt_type(user_presence_shaper) ->
    econf:atom();
mod_opt_type(default_room_options) ->
    econf:options(
      #{allow_change_subj => econf:bool(),
	allow_private_messages => econf:bool(),
	allow_private_messages_from_visitors =>
	    econf:enum([anyone, moderators, nobody]),
	allow_query_users => econf:bool(),
	allow_subscription => econf:bool(),
	allow_user_invites => econf:bool(),
	allow_visitor_nickchange => econf:bool(),
	allow_visitor_status => econf:bool(),
	anonymous => econf:bool(),
	captcha_protected => econf:bool(),
	lang => econf:lang(),
	logging => econf:bool(),
	mam => econf:bool(),
	max_users => econf:pos_int(),
	members_by_default => econf:bool(),
	members_only => econf:bool(),
	moderated => econf:bool(),
	password => econf:binary(),
	password_protected => econf:bool(),
	persistent => econf:bool(),
	presence_broadcast =>
	    econf:list(
	      econf:enum([moderator, participant, visitor])),
	public => econf:bool(),
	public_list => econf:bool(),
	title => econf:binary()});
mod_opt_type(db_type) ->
    econf:db_type(?MODULE);
mod_opt_type(ram_db_type) ->
    econf:db_type(?MODULE);
mod_opt_type(host) ->
    econf:host();
mod_opt_type(hosts) ->
    econf:hosts();
mod_opt_type(queue_type) ->
    econf:queue_type();
mod_opt_type(hibernation_timeout) ->
    econf:timeout(second, infinity);
mod_opt_type(vcard) ->
    econf:vcard_temp().

mod_options(Host) ->
    [{access, all},
     {access_admin, none},
     {access_create, all},
     {access_persistent, all},
     {access_mam, all},
     {access_register, all},
     {db_type, ejabberd_config:default_db(Host, ?MODULE)},
     {ram_db_type, ejabberd_config:default_ram_db(Host, ?MODULE)},
     {history_size, 20},
     {host, <<"conference.", Host/binary>>},
     {hosts, []},
     {name, ?T("Chatrooms")},
     {max_room_desc, infinity},
     {max_room_id, infinity},
     {max_room_name, infinity},
     {max_rooms_discoitems, 100},
     {max_user_conferences, 100},
     {max_users, 200},
     {max_users_admin_threshold, 5},
     {max_users_presence, 1000},
     {min_message_interval, 0},
     {min_presence_interval, 0},
     {queue_type, ejabberd_option:queue_type(Host)},
     {regexp_room_id, <<"">>},
     {room_shaper, none},
     {user_message_shaper, none},
     {user_presence_shaper, none},
     {preload_rooms, true},
     {hibernation_timeout, infinity},
     {vcard, undefined},
     {default_room_options,
      [{allow_change_subj,true},
       {allow_private_messages,true},
       {allow_query_users,true},
       {allow_user_invites,false},
       {allow_visitor_nickchange,true},
       {allow_visitor_status,true},
       {anonymous,true},
       {captcha_protected,false},
       {lang,<<>>},
       {logging,false},
       {members_by_default,true},
       {members_only,false},
       {moderated,true},
       {password_protected,false},
       {persistent,false},
       {public,true},
       {public_list,true},
       {mam,false},
       {allow_subscription,false},
       {password,<<>>},
       {title,<<>>},
       {allow_private_messages_from_visitors,anyone},
       {max_users,200},
       {presence_broadcast,[moderator,participant,visitor]}]}].

mod_doc() ->
    #{desc =>
          [?T("This module provides support for https://xmpp.org/extensions/xep-0045.html"
             "[XEP-0045: Multi-User Chat]. Users can discover existing rooms, "
             "join or create them. Occupants of a room can chat in public or have private chats."), "",
	   ?T("The MUC service allows any Jabber ID to register a nickname, so "
	      "nobody else can use that nickname in any room in the MUC "
	      "service. To register a nickname, open the Service Discovery in "
	      "your XMPP client and register in the MUC service."), "",
	   ?T("This module supports clustering and load balancing. One module "
	      "can be started per cluster node. Rooms are distributed at "
	      "creation time on all available MUC module instances. The "
	      "multi-user chat module is clustered but the rooms themselves "
	      "are not clustered nor fault-tolerant: if the node managing a "
	      "set of rooms goes down, the rooms disappear and they will be "
	      "recreated on an available node on first connection attempt.")],
      opts =>
          [{access,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("You can specify who is allowed to use the Multi-User Chat service. "
                     "By default everyone is allowed to use it.")}},
           {access_admin,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("This option specifies who is allowed to administrate "
                     "the Multi-User Chat service. The default value is 'none', "
                     "which means that only the room creator can administer "
                     "their room. The administrators can send a normal message "
                     "to the service JID, and it will be shown in all active "
                     "rooms as a service message. The administrators can send a "
                     "groupchat message to the JID of an active room, and the "
                     "message will be shown in the room as a service message.")}},
           {access_create,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("To configure who is allowed to create new rooms at the "
                     "Multi-User Chat service, this option can be used. "
                     "By default any account in the local ejabberd server is "
                     "allowed to create rooms.")}},
           {access_persistent,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("To configure who is allowed to modify the 'persistent' room option. "
                     "By default any account in the local ejabberd server is allowed to "
                     "modify that option.")}},
           {access_mam,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("To configure who is allowed to modify the 'mam' room option. "
                     "By default any account in the local ejabberd server is allowed to "
                     "modify that option.")}},
           {access_register,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("This option specifies who is allowed to register nickname "
                     "within the Multi-User Chat service. The default is 'all' for "
                     "backward compatibility, which means that any user is allowed "
                     "to register any free nick.")}},
           {db_type,
            #{value => "mnesia | sql",
              desc =>
                  ?T("Define the type of persistent storage where the module will "
                     "store room information. The default is the storage defined "
                     "by the global option 'default_db', or 'mnesia' if omitted.")}},
           {ram_db_type,
            #{value => "mnesia",
              desc =>
                  ?T("Define the type of volatile (in-memory) storage where the module "
                     "will store room information. The only available value for this "
                     "module is 'mnesia'.")}},
           {hibernation_timeout,
            #{value => "infinity | Seconds",
              desc =>
                  ?T("Timeout before hibernating the room process, expressed "
		     "in seconds. The default value is 'infinity'.")}},
           {history_size,
            #{value => ?T("Size"),
              desc =>
                  ?T("A small history of the current discussion is sent to users "
                     "when they enter the room. With this option you can define the "
                     "number of history messages to keep and send to users joining the room. "
                     "The value is a non-negative integer. Setting the value to 0 disables "
                     "the history feature and, as a result, nothing is kept in memory. "
                     "The default value is 20. This value affects all rooms on the service. "
                     "NOTE: modern XMPP clients rely on Message Archives (XEP-0313), so feel "
                     "free to disable the history feature if you're only using modern clients "
                     "and have 'mod_mam' module loaded.")}},
           {host, #{desc => ?T("Deprecated. Use 'hosts' instead.")}},
           {hosts,
            #{value => ?T("[Host, ...]"),
              desc =>
                  ?T("This option defines the Jabber IDs of the service. "
                     "If the 'hosts' option is not specified, the only Jabber ID will "
                     "be the hostname of the virtual host with the prefix \"conference.\". "
                     "The keyword '@HOST@' is replaced with the real virtual host name.")}},
           {name,
            #{value => "string()",
              desc =>
                  ?T("The value of the service name. This name is only visible in some "
                     "clients that support https://xmpp.org/extensions/xep-0030.html"
                     "[XEP-0030: Service Discovery]. The default is 'Chatrooms'.")}},
           {max_room_desc,
            #{value => ?T("Number"),
              desc =>
                  ?T("This option defines the maximum number of characters that "
                     "Room Description can have when configuring the room. "
                     "The default value is 'infinity'.")}},
           {max_room_id,
            #{value => ?T("Number"),
              desc =>
                  ?T("This option defines the maximum number of characters that "
                     "Room ID can have when creating a new room. "
                     "The default value is 'infinity'.")}},
           {max_room_name,
            #{value => ?T("Number"),
              desc =>
                  ?T("This option defines the maximum number of characters "
                     "that Room Name can have when configuring the room. "
                     "The default value is 'infinity'.")}},
           {max_rooms_discoitems,
            #{value => ?T("Number"),
              desc =>
                  ?T("When there are more rooms than this 'Number', "
                     "only the non-empty ones are returned in a Service Discovery query. "
                     "The default value is '100'.")}},
           {max_user_conferences,
            #{value => ?T("Number"),
              desc =>
                  ?T("This option defines the maximum number of rooms that any "
                     "given user can join. The default value is '100'. This option "
                     "is used to prevent possible abuses. Note that this is a soft "
                     "limit: some users can sometimes join more conferences in "
                     "cluster configurations.")}},
           {max_users,
            #{value => ?T("Number"),
              desc =>
                  ?T("This option defines at the service level, the maximum "
                     "number of users allowed per room. It can be lowered in "
                     "each room configuration but cannot be increased in "
                     "individual room configuration. The default value is '200'.")}},
           {max_users_admin_threshold,
            #{value => ?T("Number"),
              desc =>
                  ?T("This option defines the number of service admins or room "
                     "owners allowed to enter the room when the maximum number "
                     "of allowed occupants was reached. The default limit is '5'.")}},
           {max_users_presence,
            #{value => ?T("Number"),
              desc =>
                  ?T("This option defines after how many users in the room, "
                     "it is considered overcrowded. When a MUC room is considered "
                     "overcrowed, presence broadcasts are limited to reduce load, "
                     "traffic and excessive presence \"storm\" received by participants.")}},
           {min_message_interval,
            #{value => ?T("Number"),
              desc =>
                  ?T("This option defines the minimum interval between two "
                     "messages send by an occupant in seconds. This option "
                     "is global and valid for all rooms. A decimal value can be used. "
                     "When this option is not defined, message rate is not limited. "
                     "This feature can be used to protect a MUC service from occupant "
                     "abuses and limit number of messages that will be broadcasted by "
                     "the service. A good value for this minimum message interval is 0.4 second. "
                     "If an occupant tries to send messages faster, an error is send back "
                     "explaining that the message has been discarded and describing the "
                     "reason why the message is not acceptable.")}},
           {min_presence_interval,
            #{value => ?T("Number"),
              desc =>
                  ?T("This option defines the minimum of time between presence "
                     "changes coming from a given occupant in seconds. "
                     "This option is global and valid for all rooms. A decimal "
                     "value can be used. When this option is not defined, no "
                     "restriction is applied. This option can be used to protect "
                     "a MUC service for occupants abuses. If an occupant tries "
                     "to change its presence more often than the specified interval, "
                     "the presence is cached by ejabberd and only the last presence "
                     "is broadcasted to all occupants in the room after expiration "
                     "of the interval delay. Intermediate presence packets are "
                     "silently discarded. A good value for this option is 4 seconds.")}},
           {queue_type,
            #{value => "ram | file",
              desc =>
                  ?T("Same as top-level 'queue_type' option, but applied to this module only.")}},
           {regexp_room_id,
            #{value => "string()",
              desc =>
                  ?T("This option defines the regular expression that a Room ID "
                     "must satisfy to allow the room creation. The default value "
                     "is the empty string.")}},
           {preload_rooms,
            #{value => "true | false",
              desc =>
                  ?T("Whether to load all persistent rooms in memory on startup. "
                     "If disabled, the room is only loaded on first participant join. "
                     "The default is 'true'. It makes sense to disable room preloading "
                     "when the number of rooms is high: this will improve server startup "
                     "time and memory consumption.")}},
           {room_shaper,
            #{value => "none | ShaperName",
              desc =>
                  ?T("This option defines shaper for the MUC rooms. "
		     "The default value is 'none'.")}},
           {user_message_shaper,
            #{value => "none | ShaperName",
              desc =>
                  ?T("This option defines shaper for the users messages. "
		     "The default value is 'none'.")}},
           {user_presence_shaper,
            #{value => "none | ShaperName",
              desc =>
                  ?T("This option defines shaper for the users presences. "
		     "The default value is 'none'.")}},
           {vcard,
            #{value => ?T("vCard"),
              desc =>
                  ?T("A custom vCard of the service that will be displayed "
                     "by some XMPP clients in Service Discovery. The value of "
                     "'vCard' is a YAML map constructed from an XML representation "
                     "of vCard. Since the representation has no attributes, "
                     "the mapping is straightforward."),
              example =>
                  [{?T("For example, the following XML representation of vCard:"),
                    ["<vCard xmlns='vcard-temp'>",
                     "  <FN>Conferences</FN>",
                     "  <ADR>",
                     "    <WORK/>",
                     "    <STREET>Elm Street</STREET>",
                     "  </ADR>",
                     "</vCard>"]},
                   {?T("will be translated to:"),
                    ["vcard:",
                     "  fn: Conferences",
                     "  adr:",
                     "    -",
                     "      work: true",
                     "      street: Elm Street"]}]}},
           {default_room_options,
            #{value => ?T("Options"),
              desc =>
                  ?T("This option allows to define the desired "
                     "default room options. Note that the creator of a room "
                     "can modify the options of his room at any time using an "
                     "XMPP client with MUC capability. The 'Options' are:")},
            [{allow_change_subj,
              #{value => "true | false",
                desc =>
                    ?T("Allow occupants to change the subject. "
                       "The default value is 'true'.")}},
             {allow_private_messages,
              #{value => "true | false",
                desc =>
                    ?T("Occupants can send private messages to other occupants. "
                       "The default value is 'true'.")}},
             {allow_query_users,
              #{value => "true | false",
                desc =>
                    ?T("Occupants can send IQ queries to other occupants. "
                       "The default value is 'true'.")}},
             {allow_user_invites,
              #{value => "true | false",
                desc =>
                    ?T("Allow occupants to send invitations. "
                       "The default value is 'false'.")}},
             {allow_visitor_nickchange,
              #{value => "true | false",
                desc => ?T("Allow visitors to change nickname. "
                           "The default value is 'true'.")}},
             {allow_visitor_status,
              #{value => "true | false",
                desc =>
                    ?T("Allow visitors to send status text in presence updates. "
                       "If disallowed, the status text is stripped before broadcasting "
                       "the presence update to all the room occupants. "
                       "The default value is 'true'.")}},
             {anonymous,
              #{value => "true | false",
                desc =>
                    ?T("The room is anonymous: occupants don't see the real "
                       "JIDs of other occupants. Note that the room moderators "
                       "can always see the real JIDs of the occupants. "
                       "The default value is 'true'.")}},
             {captcha_protected,
              #{value => "true | false",
                desc =>
                    ?T("When a user tries to join a room where they have no "
                       "affiliation (not owner, admin or member), the room "
                       "requires them to fill a CAPTCHA challenge (see section "
                       "https://docs.ejabberd.im/admin/configuration/#captcha[CAPTCHA] "
                       "in order to accept their join in the room. "
                       "The default value is 'false'.")}},
             {lang,
              #{value => ?T("Language"),
                desc =>
                    ?T("Preferred language for the discussions in the room. "
                       "The language format should conform to RFC 5646. "
                       "There is no value by default.")}},
             {logging,
              #{value => "true | false",
                desc =>
                    ?T("The public messages are logged using 'mod_muc_log'. "
                       "The default value is 'false'.")}},
             {members_by_default,
              #{value => "true | false",
                desc =>
                    ?T("The occupants that enter the room are participants "
                       "by default, so they have \"voice\". "
                       "The default value is 'true'.")}},
             {members_only,
              #{value => "true | false",
                desc =>
                    ?T("Only members of the room can enter. "
                       "The default value is 'false'.")}},
             {moderated,
              #{value => "true | false",
                desc =>
                    ?T("Only occupants with \"voice\" can send public messages. "
                       "The default value is 'true'.")}},
             {password_protected,
              #{value => "true | false",
                desc =>
                    ?T("The password is required to enter the room. "
                       "The default value is 'false'.")}},
             {password,
              #{value => ?T("Password"),
                desc =>
                    ?T("Password of the room. Implies option 'password_protected' "
                       "set to 'true'. There is no default value.")}},
             {persistent,
              #{value => "true | false",
                desc =>
                    ?T("The room persists even if the last participant leaves. "
                       "The default value is 'false'.")}},
             {public,
              #{value => "true | false",
                desc =>
                    ?T("The room is public in the list of the MUC service, "
                       "so it can be discovered. MUC admins and room participants "
                       "will see private rooms in Service Discovery if their XMPP "
                       "client supports this feature. "
                       "The default value is 'true'.")}},
             {public_list,
              #{value => "true | false",
                desc =>
                    ?T("The list of participants is public, without requiring "
                       "to enter the room. The default value is 'true'.")}},
             {mam,
              #{value => "true | false",
                desc =>
                    ?T("Enable message archiving. Implies mod_mam is enabled. "
                       "The default value is 'false'.")}},
             {allow_subscription,
              #{value => "true | false",
                desc =>
                    ?T("Allow users to subscribe to room events as described in "
                       "https://docs.ejabberd.im/developer/xmpp-clients-bots/extensions/muc-sub/"
                       "[Multi-User Chat Subscriptions]. "
                       "The default value is 'false'.")}},
             {title,
              #{value => ?T("Room Title"),
                desc =>
                    ?T("A human-readable title of the room. "
                       "There is no default value")}},
             {allow_private_messages_from_visitors,
              #{value => "anyone | moderators | nobody",
                desc =>
                    ?T("Visitors can send private messages to other occupants. "
                       "The default value is 'anyone' which means visitors "
                       "can send private messages to any occupant.")}},
             {max_users,
              #{value => ?T("Number"),
                desc =>
                    ?T("Maximum number of occupants in the room. "
                       "The default value is '200'.")}},
             {presence_broadcast,
              #{value => "[moderator | participant | visitor, ...]",
                desc =>
                    ?T("List of roles for which presence is broadcasted. "
                       "The list can contain one or several of: 'moderator', "
                       "'participant', 'visitor'. The default value is shown "
                       "in the example below:"),
                example =>
                    ["presence_broadcast:",
                     "  - moderator",
                     "  - participant",
                     "  - visitor"]}}]}]}.
