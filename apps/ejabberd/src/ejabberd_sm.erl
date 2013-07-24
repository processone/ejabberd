%%%----------------------------------------------------------------------
%%% File    : ejabberd_sm.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Session manager
%%% Created : 24 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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
-export([start_link/0,
         route/3,
         open_session/5, close_session/4,
         check_in_subscription/6,
         bounce_offline_message/3,
         disconnect_removed_user/2,
         get_user_resources/2,
         set_presence/7,
         unset_presence/6,
         close_session_unset_presence/5,
         get_unique_sessions_number/0,
         get_total_sessions_number/0,
         get_node_sessions_number/0,
         get_vh_session_number/1,
         get_vh_session_list/1,
         get_full_session_list/0,
         register_iq_handler/4,
         register_iq_handler/5,
         unregister_iq_handler/2,
         force_update_presence/1,
         user_resources/2,
         get_session_pid/3,
         get_session/3,
         get_session_ip/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_commands.hrl").
-include("mod_privacy.hrl").

-record(state, {}).

%% default value for the maximum number of user connections
-define(MAX_USER_SESSIONS, infinity).
-define(SM_BACKEND, (ejabberd_sm_backend:backend())).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~nwhen processing: ~p",
                       [Reason, {From, To, Packet}]);
        _ ->
            ok
    end.

open_session(SID, User, Server, Resource, Info) ->
    set_session(SID, User, Server, Resource, undefined, Info),
    check_for_sessions_to_replace(User, Server, Resource),
    JID = jlib:make_jid(User, Server, Resource),
    ejabberd_hooks:run(sm_register_connection_hook, JID#jid.lserver,
                       [SID, JID, Info]).

close_session(SID, User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    Info = case ?SM_BACKEND:get_sessions(LUser, LServer, LResource) of
               [Session] ->
                   Session#session.info;
               _ ->
                   []
           end,
    ?SM_BACKEND:delete_session(SID, LUser, LServer, LResource),
    JID = jlib:make_jid(User, Server, Resource),
    ejabberd_hooks:run(sm_remove_connection_hook, JID#jid.lserver,
                       [SID, JID, Info]).

check_in_subscription(Acc, User, Server, _JID, _Type, _Reason) ->
    case ejabberd_auth:is_user_exists(User, Server) of
        true ->
            Acc;
        false ->
            {stop, false}
    end.

bounce_offline_message(#jid{server = Server} = From, To, Packet) ->
    ejabberd_hooks:run(xmpp_bounce_message,
                       Server,
                       [Server, Packet]),
    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
    ejabberd_router:route(To, From, Err),
    stop.

disconnect_removed_user(User, Server) ->
    ejabberd_sm:route(jlib:make_jid(<<>>, <<>>, <<>>),
                      jlib:make_jid(User, Server, <<>>),
                      #xmlel{name = <<"broadcast">>,
                             children = [{exit, <<"User removed">>}]}).

get_user_resources(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Ss = ?SM_BACKEND:get_sessions(LUser, LServer),
    [element(3, S#session.usr) || S <- clean_session_list(Ss)].

get_session_ip(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    case ?SM_BACKEND:get_sessions(LUser, LServer, LResource) of
        [] ->
            undefined;
        Ss ->
            Session = lists:max(Ss),
            proplists:get_value(ip, Session#session.info)
    end.

get_session(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    case ?SM_BACKEND:get_sessions(LUser, LServer, LResource) of
        [] ->
            offline;
        Ss ->
            Session = lists:max(Ss),
            {Session#session.usr,
             Session#session.sid,
             Session#session.priority,
             Session#session.info}
    end.

set_presence(SID, User, Server, Resource, Priority, Presence, Info) ->
    set_session(SID, User, Server, Resource, Priority, Info),
    ejabberd_hooks:run(set_presence_hook, jlib:nameprep(Server),
                       [User, Server, Resource, Presence]).

unset_presence(SID, User, Server, Resource, Status, Info) ->
    set_session(SID, User, Server, Resource, undefined, Info),
    ejabberd_hooks:run(unset_presence_hook, jlib:nameprep(Server),
                       [User, Server, Resource, Status]).

close_session_unset_presence(SID, User, Server, Resource, Status) ->
    close_session(SID, User, Server, Resource),
    ejabberd_hooks:run(unset_presence_hook, jlib:nameprep(Server),
                       [User, Server, Resource, Status]).

get_session_pid(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    case ?SM_BACKEND:get_sessions(LUser, LServer, LResource) of
        [#session{sid = {_, Pid}}] ->
            Pid;
        _ ->
            none
    end.

-spec get_unique_sessions_number() -> integer().
get_unique_sessions_number() ->
    ?SM_BACKEND:unique_count().

-spec get_total_sessions_number() -> integer().
get_total_sessions_number() ->
    ?SM_BACKEND:total_count().

get_vh_session_number(Server) ->
    length(?SM_BACKEND:get_sessions(Server)).

get_vh_session_list(Server) ->
    ?SM_BACKEND:get_sessions(Server).

get_node_sessions_number() ->
    {value, {active, Active}} = lists:keysearch(active, 1, supervisor:count_children(ejabberd_c2s_sup)),
    Active.

get_full_session_list() ->
    ?SM_BACKEND:get_sessions().

register_iq_handler(Host, XMLNS, Module, Fun) ->
    ejabberd_sm ! {register_iq_handler, Host, XMLNS, Module, Fun}.

register_iq_handler(Host, XMLNS, Module, Fun, Opts) ->
    ejabberd_sm ! {register_iq_handler, Host, XMLNS, Module, Fun, Opts}.

unregister_iq_handler(Host, XMLNS) ->
    ejabberd_sm ! {unregister_iq_handler, Host, XMLNS}.

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
init([]) ->
    {Backend, Opts} = ejabberd_config:get_global_option(sm_backend),
    {Mod, Code} = dynamic_compile:from_string(sm_backend(Backend)),
    code:load_binary(Mod, "ejabberd_sm_backend.erl", Code),

    net_kernel:monitor_nodes(true),
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
    ejabberd_commands:register_commands(commands()),

    ?SM_BACKEND:start(Opts),

    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~nwhen processing: ~p",
                       [Reason, {From, To, Packet}]);
        _ ->
            ok
    end,
    {noreply, State};
handle_info({nodedown, Node}, State) ->
    ?SM_BACKEND:cleanup(Node),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module, Function}, State) ->
    ets:insert(sm_iqtable, {{XMLNS, Host}, Module, Function}),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module, Function, Opts}, State) ->
    ets:insert(sm_iqtable, {{XMLNS, Host}, Module, Function, Opts}),
    {noreply, State};
handle_info({unregister_iq_handler, Host, XMLNS}, State) ->
    case ets:lookup(sm_iqtable, {XMLNS, Host}) of
        [{_, Module, Function, Opts}] ->
            gen_iq_handler:stop_iq_handler(Module, Function, Opts);
        _ ->
            ok
    end,
    ets:delete(sm_iqtable, {XMLNS, Host}),
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
terminate(_Reason, _State) ->
    ejabberd_commands:unregister_commands(commands()),
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

set_session(SID, User, Server, Resource, Priority, Info) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},
    Session = #session{sid = SID,
                       usr = USR,
                       us = US,
                       priority = Priority,
                       info = Info},
    ?SM_BACKEND:create_session(LUser, LServer, LResource, Session).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_route(From, To, Packet) ->
    ?DEBUG("session manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
           [From, To, Packet, 8]),
    #jid{ luser = LUser, lserver = LServer, lresource = LResource} = To,
    #xmlel{name = Name, attrs = Attrs} = Packet,
    case LResource of
        <<>> ->
            do_route_no_resource(Name, xml:get_attr_s(<<"type">>, Attrs),
                                 From, To, Packet);
        _ ->
            case ?SM_BACKEND:get_sessions(LUser, LServer, LResource) of
                [] ->
                    do_route_offline(Name, xml:get_attr_s(<<"type">>, Attrs),
                                     From, To, Packet);
                Ss ->
                    Session = lists:max(Ss),
                    Pid = element(2, Session#session.sid),
                    ?DEBUG("sending to process ~p~n", [Pid]),
                    Pid ! {route, From, To, Packet}
	    end
    end.

do_route_no_resource_presence_prv(From,To,Packet,Type,Reason) ->
	is_privacy_allow(From, To, Packet) andalso ejabberd_hooks:run_fold(
	   roster_in_subscription,
	   To#jid.lserver,
	   false,
	   [To#jid.user, To#jid.server, From, Type, Reason]).

-spec do_route_no_resource_presence(binary(), #jid{}, #jid{}, tuple()) -> any().
do_route_no_resource_presence(<<"subscribe">>, From, To, Packet) ->
	Reason = xml:get_path_s(Packet, [{elem, <<"status">>}, cdata]),
	do_route_no_resource_presence_prv(From, To, Packet, subscribe, Reason);
do_route_no_resource_presence(<<"subscribed">>, From, To, Packet) ->
	do_route_no_resource_presence_prv(From, To, Packet, subscribed, <<>>);
do_route_no_resource_presence(<<"unsubscribe">>, From, To, Packet) ->
	do_route_no_resource_presence_prv(From, To, Packet, unsubscribe, <<>>);
do_route_no_resource_presence(<<"unsubscribed">>, From, To, Packet) ->
	do_route_no_resource_presence_prv(From, To, Packet, unsubscribed, <<>>);
do_route_no_resource_presence(_, _, _, _) ->
	true.

do_route_no_resource(<<"presence">>, Type, From, To, Packet) ->
	case do_route_no_resource_presence(Type, From, To, Packet) of
	    true ->
            PResources = get_user_present_resources(To#jid.luser, To#jid.lserver),
		    lists:foreach(
		      fun({_, R}) ->
                      do_route(From, jlib:jid_replace_resource(To, R), Packet)
		      end, PResources);
        false ->
            ok
	end;
do_route_no_resource(<<"message">>, _, From, To, Packet) ->
	route_message(From, To, Packet);
do_route_no_resource(<<"iq">>, _, From, To, Packet) ->
	process_iq(From, To, Packet);
do_route_no_resource(<<"broadcast">>, _, From, To, Packet) ->
	ejabberd_hooks:run(sm_broadcast, To#jid.lserver, [From, To, Packet]),
	broadcast_packet(From, To, Packet);
do_route_no_resource(_, _, _, _, _) ->
	ok.

do_route_offline(<<"message">>, _, From, To, Packet)  ->
	route_message(From, To, Packet);
do_route_offline(<<"iq">>, <<"error">>, _From, _To, _Packet) ->
	ok;
do_route_offline(<<"iq">>, <<"result">>, _From, _To, _Packet) ->
	ok;
do_route_offline(<<"iq">>, _, From, To, Packet) ->
	Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
	ejabberd_router:route(To, From, Err);
do_route_offline(_, _, _, _, _) ->
	?DEBUG("packet droped~n", []).

broadcast_packet(From, To, Packet) ->
    #jid{user = User, server = Server} = To,
    lists:foreach(
      fun(R) ->
              do_route(From,
                       jlib:jid_replace_resource(To, R),
                       Packet)
      end, get_user_resources(User, Server)).

%% The default list applies to the user as a whole,
%% and is processed if there is no active list set
%% for the target session/resource to which a stanza is addressed,
%% or if there are no current sessions for the user.
is_privacy_allow(From, To, Packet) ->
    User = To#jid.user,
    Server = To#jid.server,
    PrivacyList = ejabberd_hooks:run_fold(privacy_get_user_list, Server,
                                          #userlist{}, [User, Server]),
    is_privacy_allow(From, To, Packet, PrivacyList).

%% Check if privacy rules allow this delivery
%% Function copied from ejabberd_c2s.erl
is_privacy_allow(From, To, Packet, PrivacyList) ->
    User = To#jid.user,
    Server = To#jid.server,
    allow == ejabberd_hooks:run_fold(
               privacy_check_packet, Server,
               allow,
               [User, Server, PrivacyList,
                {From, To, Packet}, in]).

route_message(From, To, Packet) ->
    LUser = To#jid.luser,
    LServer = To#jid.lserver,
    PrioRes = get_user_present_resources(LUser, LServer),
    case catch lists:max(PrioRes) of
        {Priority, _R} when is_integer(Priority), Priority >= 0 ->
            lists:foreach(
              %% Route messages to all priority that equals the max, if
              %% positive
              fun({P, R}) when P == Priority ->
                      LResource = jlib:resourceprep(R),
                      case ?SM_BACKEND:get_sessions(LUser, LServer, LResource) of
                          [] ->
                              ok; % Race condition
                          Ss ->
                              Session = lists:max(Ss),
                              Pid = element(2, Session#session.sid),
                              ?DEBUG("sending to process ~p~n", [Pid]),
                              Pid ! {route, From, To, Packet}
                      end;
                 %% Ignore other priority:
                 ({_Prio, _Res}) ->
                      ok
              end,
              PrioRes);
        _ ->
            case xml:get_tag_attr_s(<<"type">>, Packet) of
                <<"error">> ->
                    ok;
                <<"groupchat">> ->
                    bounce_offline_message(From, To, Packet);
                <<"headline">> ->
                    bounce_offline_message(From, To, Packet);
                _ ->
                    case ejabberd_auth:is_user_exists(LUser, LServer) of
                        true ->
                            case is_privacy_allow(From, To, Packet) of
                                true ->
                                    ejabberd_hooks:run(offline_message_hook,
                                                       LServer,
                                                       [From, To, Packet]);
                                false ->
                                    ok
                            end;
                        _ ->
                            Err = jlib:make_error_reply(
                                    Packet, ?ERR_SERVICE_UNAVAILABLE),
                            ejabberd_router:route(To, From, Err)
                    end
            end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clean_session_list(Ss) ->
    clean_session_list(lists:keysort(#session.usr, Ss), []).

clean_session_list([], Res) ->
    Res;
clean_session_list([S], Res) ->
    [S | Res];
clean_session_list([S1, S2 | Rest], Res) ->
    if
        S1#session.usr == S2#session.usr ->
            if
                S1#session.sid > S2#session.sid ->
                    clean_session_list([S1 | Rest], Res);
                true ->
                    clean_session_list([S2 | Rest], Res)
            end;
        true ->
            clean_session_list([S2 | Rest], [S1 | Res])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_user_present_resources(LUser, LServer) ->
    Ss = ?SM_BACKEND:get_sessions(LUser, LServer),
    [{S#session.priority, element(3, S#session.usr)} ||
        S <- clean_session_list(Ss), is_integer(S#session.priority)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% On new session, check if some existing connections need to be replace
check_for_sessions_to_replace(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),

    %% TODO: Depending on how this is executed, there could be an unneeded
    %% replacement for max_sessions. We need to check this at some point.
    check_existing_resources(LUser, LServer, LResource),
    check_max_sessions(LUser, LServer).

check_existing_resources(LUser, LServer, LResource) ->
    %% A connection exist with the same resource. We replace it:
    Sessions = ?SM_BACKEND:get_sessions(LUser, LServer, LResource),
    SIDs = [S#session.sid || S <- Sessions],
    if
        SIDs == [] ->
            ok;
        true ->
            MaxSID = lists:max(SIDs),
            lists:foreach(
              fun({_, Pid} = S) when S /= MaxSID ->
                      Pid ! replaced;
                 (_) -> ok
              end, SIDs)
    end.

check_max_sessions(LUser, LServer) ->
    %% If the max number of sessions for a given is reached, we replace the
    %% first one
    Sessions = ?SM_BACKEND:get_sessions(LUser, LServer),
    SIDs = [S#session.sid || S <- Sessions],
    MaxSessions = get_max_user_sessions(LUser, LServer),
    if
        length(SIDs) =< MaxSessions ->
            ok;
        true ->
            {_, Pid} = lists:min(SIDs),
            Pid ! replaced
    end.

%% Get the user_max_session setting
%% This option defines the max number of time a given users are allowed to
%% log in
%% Defaults to infinity
get_max_user_sessions(LUser, Host) ->
    case acl:match_rule(
           Host, max_user_sessions, jlib:make_jid(LUser, Host, <<>>)) of
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
                [{_, Module, Function, Opts}] ->
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

force_update_presence({LUser, LServer}) ->
    Ss = ?SM_BACKEND:get_sessions(LUser, LServer),
    lists:foreach(fun(#session{sid = {_, Pid}}) ->
                          Pid ! {force_update_presence, LUser}
                  end, Ss).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ejabberd commands

commands() ->
	[
     %% TODO: Implement following API functions with pluggable backends architcture
     %% #ejabberd_commands{name = connected_users,
     %%                    tags = [session],
     %%                    desc = "List all established sessions",
     %%                    module = ?MODULE, function = connected_users,
     %%                    args = [],
     %%                    result = {connected_users, {list, {sessions, string}}}},
     %% #ejabberd_commands{name = connected_users_number,
     %%                    tags = [session, stats],
     %%                    desc = "Get the number of established sessions",
     %%                    module = ?MODULE, function = connected_users_number,
     %%                    args = [],
     %%                    result = {num_sessions, integer}},
     #ejabberd_commands{name = user_resources,
                        tags = [session],
                        desc = "List user's connected resources",
                        module = ?MODULE, function = user_resources,
                        args = [{user, string}, {host, string}],
                        result = {resources, {list, {resource, binary}}}}
	].

user_resources(User, Server) ->
    Resources =  get_user_resources(list_to_binary(User), list_to_binary(Server)),
    lists:sort(Resources).

-spec sm_backend(atom()) -> string().
sm_backend(Backend) ->
    lists:flatten(
      ["-module(ejabberd_sm_backend).
        -export([backend/0]).

        -spec backend() -> atom().
        backend() ->
            ejabberd_sm_",
       atom_to_list(Backend),
       ".\n"]).
