%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : S2S connections manager
%%% Created :  7 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2021   ProcessOne
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

-module(ejabberd_s2s).

-protocol({xep, 220, '1.1'}).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, route/1, have_connection/1,
	 get_connections_pids/1,
	 start_connection/2, start_connection/3,
	 dirty_get_connections/0, allow_host/2,
	 incoming_s2s_number/0, outgoing_s2s_number/0,
	 stop_s2s_connections/0,
	 clean_temporarily_blocked_table/0,
	 list_temporarily_blocked_hosts/0,
	 external_host_overloaded/1, is_temporarly_blocked/1,
	 get_commands_spec/0, zlib_enabled/1, get_idle_timeout/1,
	 tls_required/1, tls_enabled/1, tls_options/3,
	 host_up/1, host_down/1, queue_type/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-export([get_info_s2s_connections/1]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_commands.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("ejabberd_stacktrace.hrl").
-include("translate.hrl").

-define(DEFAULT_MAX_S2S_CONNECTIONS_NUMBER, 1).
-define(DEFAULT_MAX_S2S_CONNECTIONS_NUMBER_PER_NODE, 1).
-define(S2S_OVERLOAD_BLOCK_PERIOD, 60).

%% once a server is temporary blocked, it stay blocked for 60 seconds

-record(s2s, {fromto :: {binary(), binary()} | '_',
              pid    :: pid()}).

-record(state, {}).

-record(temporarily_blocked, {host      :: binary(),
                              timestamp :: integer()}).

-type temporarily_blocked() :: #temporarily_blocked{}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    _ = supervisor:terminate_child(ejabberd_sup, ?MODULE),
    _ = supervisor:delete_child(ejabberd_sup, ?MODULE),
    ok.

clean_temporarily_blocked_table() ->
    mnesia:clear_table(temporarily_blocked).

-spec list_temporarily_blocked_hosts() -> [temporarily_blocked()].
list_temporarily_blocked_hosts() ->
    ets:tab2list(temporarily_blocked).

-spec external_host_overloaded(binary()) -> {aborted, any()} | {atomic, ok}.
external_host_overloaded(Host) ->
    ?INFO_MSG("Disabling s2s connections to ~ts for ~p seconds",
	      [Host, ?S2S_OVERLOAD_BLOCK_PERIOD]),
    mnesia:transaction(fun () ->
                               Time = erlang:monotonic_time(),
			       mnesia:write(#temporarily_blocked{host = Host,
								 timestamp = Time})
		       end).

-spec is_temporarly_blocked(binary()) -> boolean().
is_temporarly_blocked(Host) ->
    case mnesia:dirty_read(temporarily_blocked, Host) of
	[] -> false;
	[#temporarily_blocked{timestamp = T} = Entry] ->
	    Diff = erlang:monotonic_time() - T,
	    case erlang:convert_time_unit(Diff, native, microsecond) of
		N when N > (?S2S_OVERLOAD_BLOCK_PERIOD) * 1000 * 1000 ->
		    mnesia:dirty_delete_object(Entry), false;
		_ -> true
	    end
    end.

-spec have_connection({binary(), binary()}) -> boolean().
have_connection(FromTo) ->
    case catch mnesia:dirty_read(s2s, FromTo) of
	[_] ->
            true;
        _ ->
            false
    end.

-spec get_connections_pids({binary(), binary()}) -> [pid()].
get_connections_pids(FromTo) ->
    case catch mnesia:dirty_read(s2s, FromTo) of
	L when is_list(L) ->
	    [Connection#s2s.pid || Connection <- L];
	_ ->
	    []
    end.

-spec dirty_get_connections() -> [{binary(), binary()}].
dirty_get_connections() ->
    mnesia:dirty_all_keys(s2s).

-spec tls_options(binary(), binary(), [proplists:property()]) -> [proplists:property()].
tls_options(LServer, ServerHost, DefaultOpts) ->
    TLSOpts1 = case ejabberd_pkix:get_certfile(LServer) of
		   error -> DefaultOpts;
		   {ok, CertFile} ->
		       lists:keystore(certfile, 1, DefaultOpts,
				      {certfile, CertFile})
	       end,
    TLSOpts2 = case ejabberd_option:s2s_ciphers(ServerHost) of
                   undefined -> TLSOpts1;
                   Ciphers -> lists:keystore(ciphers, 1, TLSOpts1,
					     {ciphers, Ciphers})
               end,
    TLSOpts3 = case ejabberd_option:s2s_protocol_options(ServerHost) of
                   undefined -> TLSOpts2;
                   ProtoOpts -> lists:keystore(protocol_options, 1, TLSOpts2,
					       {protocol_options, ProtoOpts})
               end,
    TLSOpts4 = case ejabberd_option:s2s_dhfile(ServerHost) of
                   undefined -> TLSOpts3;
                   DHFile -> lists:keystore(dhfile, 1, TLSOpts3,
					    {dhfile, DHFile})
               end,
    TLSOpts5 = case lists:keymember(cafile, 1, TLSOpts4) of
		   true -> TLSOpts4;
		   false -> [{cafile, get_cafile(ServerHost)}|TLSOpts4]
	       end,
    case ejabberd_option:s2s_tls_compression(ServerHost) of
	undefined -> TLSOpts5;
	false -> [compression_none | TLSOpts5];
	true -> lists:delete(compression_none, TLSOpts5)
    end.

-spec tls_required(binary()) -> boolean().
tls_required(LServer) ->
    TLS = use_starttls(LServer),
    TLS == required.

-spec tls_enabled(binary()) -> boolean().
tls_enabled(LServer) ->
    TLS = use_starttls(LServer),
    TLS /= false.

-spec zlib_enabled(binary()) -> boolean().
zlib_enabled(LServer) ->
    ejabberd_option:s2s_zlib(LServer).

-spec use_starttls(binary()) -> boolean() | optional | required.
use_starttls(LServer) ->
    ejabberd_option:s2s_use_starttls(LServer).

-spec get_idle_timeout(binary()) -> non_neg_integer() | infinity.
get_idle_timeout(LServer) ->
    ejabberd_option:s2s_timeout(LServer).

-spec queue_type(binary()) -> ram | file.
queue_type(LServer) ->
    ejabberd_option:s2s_queue_type(LServer).

-spec get_cafile(binary()) -> file:filename_all() | undefined.
get_cafile(LServer) ->
    case ejabberd_option:s2s_cafile(LServer) of
	undefined ->
	    ejabberd_option:ca_file();
	File ->
	    File
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    update_tables(),
    ejabberd_mnesia:create(?MODULE, s2s,
			   [{ram_copies, [node()]},
			    {type, bag},
			    {attributes, record_info(fields, s2s)}]),
    case mnesia:subscribe(system) of
	{ok, _} ->
	    ejabberd_commands:register_commands(get_commands_spec()),
	    ejabberd_mnesia:create(
	      ?MODULE, temporarily_blocked,
	      [{ram_copies, [node()]},
	       {attributes, record_info(fields, temporarily_blocked)}]),
	    ejabberd_hooks:add(host_up, ?MODULE, host_up, 50),
	    ejabberd_hooks:add(host_down, ?MODULE, host_down, 60),
	    lists:foreach(fun host_up/1, ejabberd_option:hosts()),
	    {ok, #state{}};
	{error, Reason} ->
	    {stop, Reason}
    end.

handle_call({new_connection, Args}, _From, State) ->
    {reply, erlang:apply(fun new_connection_int/7, Args), State};
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    clean_table_from_bad_node(Node),
    {noreply, State};
handle_info({route, Packet}, State) ->
    try route(Packet)
    catch ?EX_RULE(Class, Reason, St) ->
	    StackTrace = ?EX_STACK(St),
	    ?ERROR_MSG("Failed to route packet:~n~ts~n** ~ts",
		       [xmpp:pp(Packet),
			misc:format_exception(2, Class, Reason, StackTrace)])
    end,
    {noreply, State};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case mnesia:dirty_match_object(s2s, #s2s{fromto = '_', pid = Pid}) of
	[#s2s{pid = Pid, fromto = {From, To}} = Obj] ->
	    F = fun() -> mnesia:delete_object(Obj) end,
	    case mnesia:transaction(F) of
		{atomic, _} -> ok;
		{aborted, Reason} ->
		    ?ERROR_MSG("Failed to unregister s2s connection for pid ~p (~ts -> ~ts):"
			       "Mnesia failure: ~p",
			       [Pid, From, To, Reason])
	    end,
	    {noreply, State};
	_ ->
	    {noreply, State}
    end;
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_commands:unregister_commands(get_commands_spec()),
    stop_s2s_connections(stream_error()),
    lists:foreach(fun host_down/1, ejabberd_option:hosts()),
    ejabberd_hooks:delete(host_up, ?MODULE, host_up, 50),
    ejabberd_hooks:delete(host_down, ?MODULE, host_down, 60).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec host_up(binary()) -> ok.
host_up(Host) ->
    ejabberd_s2s_in:host_up(Host),
    ejabberd_s2s_out:host_up(Host).

-spec host_down(binary()) -> ok.
host_down(Host) ->
    Err = stream_error(),
    lists:foreach(
      fun(#s2s{fromto = {From, _}, pid = Pid}) when node(Pid) == node() ->
	      case ejabberd_router:host_of_route(From) of
		  Host ->
		      ejabberd_s2s_out:send(Pid, Err),
		      ejabberd_s2s_out:stop_async(Pid);
		  _ ->
		      ok
	      end;
	 (_) ->
	      ok
      end, ets:tab2list(s2s)),
    ejabberd_s2s_in:host_down(Host),
    ejabberd_s2s_out:host_down(Host).

-spec clean_table_from_bad_node(node()) -> any().
clean_table_from_bad_node(Node) ->
    F = fun() ->
		Es = mnesia:select(
		       s2s,
		       ets:fun2ms(
			 fun(#s2s{pid = Pid} = E) when node(Pid) == Node ->
				 E
			 end)),
		lists:foreach(fun mnesia:delete_object/1, Es)
	end,
    mnesia:async_dirty(F).

-spec route(stanza()) -> ok.
route(Packet) ->
    ?DEBUG("Local route:~n~ts", [xmpp:pp(Packet)]),
    From = xmpp:get_from(Packet),
    To = xmpp:get_to(Packet),
    case start_connection(From, To) of
	{ok, Pid} when is_pid(Pid) ->
	    ?DEBUG("Sending to process ~p~n", [Pid]),
	    #jid{lserver = MyServer} = From,
	    case ejabberd_hooks:run_fold(s2s_send_packet, MyServer, Packet,
					 []) of
		drop -> ok;
		Packet1 -> ejabberd_s2s_out:route(Pid, Packet1)
	    end;
	{error, Reason} ->
	    Lang = xmpp:get_lang(Packet),
	    Err = case Reason of
		      forbidden ->
			  xmpp:err_forbidden(?T("Access denied by service policy"), Lang);
		      internal_server_error ->
			  xmpp:err_internal_server_error()
		  end,
	    ejabberd_router:route_error(Packet, Err)
    end.

-spec start_connection(jid(), jid())
		      -> {ok, pid()} | {error, forbidden | internal_server_error}.
start_connection(From, To) ->
    start_connection(From, To, []).

-spec start_connection(jid(), jid(), [proplists:property()])
		      -> {ok, pid()} | {error, forbidden | internal_server_error}.
start_connection(From, To, Opts) ->
    #jid{lserver = MyServer} = From,
    #jid{lserver = Server} = To,
    FromTo = {MyServer, Server},
    MaxS2SConnectionsNumber =
	max_s2s_connections_number(FromTo),
    MaxS2SConnectionsNumberPerNode =
	max_s2s_connections_number_per_node(FromTo),
    ?DEBUG("Finding connection for ~p~n", [FromTo]),
    case mnesia:dirty_read(s2s, FromTo) of
	[] ->
	    %% We try to establish all the connections if the host is not a
	    %% service and if the s2s host is not blacklisted or
	    %% is in whitelist:
	    LServer = ejabberd_router:host_of_route(MyServer),
	    case allow_host(LServer, Server) of
		true ->
		    NeededConnections = needed_connections_number(
					  [],
					  MaxS2SConnectionsNumber,
					  MaxS2SConnectionsNumberPerNode),
		    open_several_connections(NeededConnections, MyServer,
					     Server, From, FromTo,
					     MaxS2SConnectionsNumber,
					     MaxS2SConnectionsNumberPerNode, Opts);
		false ->
		    {error, forbidden}
	    end;
	L when is_list(L) ->
	    NeededConnections = needed_connections_number(L,
							  MaxS2SConnectionsNumber,
							  MaxS2SConnectionsNumberPerNode),
	    if NeededConnections > 0 ->
		    %% We establish the missing connections for this pair.
		    open_several_connections(NeededConnections, MyServer,
					     Server, From, FromTo,
					     MaxS2SConnectionsNumber,
					     MaxS2SConnectionsNumberPerNode, Opts);
	       true ->
		    %% We choose a connexion from the pool of opened ones.
		    {ok, choose_connection(From, L)}
	    end
    end.

-spec choose_connection(jid(), [#s2s{}]) -> pid().
choose_connection(From, Connections) ->
    choose_pid(From, [C#s2s.pid || C <- Connections]).

-spec choose_pid(jid(), [pid()]) -> pid().
choose_pid(From, Pids) ->
    Pids1 = case [P || P <- Pids, node(P) == node()] of
		[] -> Pids;
		Ps -> Ps
	    end,
    Pid =
	lists:nth(erlang:phash2(jid:remove_resource(From),
			       length(Pids1))+1,
		  Pids1),
    ?DEBUG("Using ejabberd_s2s_out ~p~n", [Pid]),
    Pid.

-spec open_several_connections(pos_integer(), binary(), binary(),
			       jid(), {binary(), binary()},
			       integer(), integer(), [proplists:property()]) ->
				      {ok, pid()} | {error, internal_server_error}.
open_several_connections(N, MyServer, Server, From,
			 FromTo, MaxS2SConnectionsNumber,
			 MaxS2SConnectionsNumberPerNode, Opts) ->
    case lists:flatmap(
	   fun(_) ->
		   new_connection(MyServer, Server,
				  From, FromTo, MaxS2SConnectionsNumber,
				  MaxS2SConnectionsNumberPerNode, Opts)
	   end, lists:seq(1, N)) of
	[] ->
	    {error, internal_server_error};
	PIDs ->
	    {ok, choose_pid(From, PIDs)}
    end.

-spec new_connection(binary(), binary(), jid(), {binary(), binary()},
		     integer(), integer(), [proplists:property()]) -> [pid()].
new_connection(MyServer, Server, From, FromTo,
	       MaxS2SConnectionsNumber, MaxS2SConnectionsNumberPerNode, Opts) ->
    case whereis(ejabberd_s2s) == self() of
	true ->
	    new_connection_int(MyServer, Server, From, FromTo,
			       MaxS2SConnectionsNumber, MaxS2SConnectionsNumberPerNode, Opts);
	false ->
	    gen_server:call(ejabberd_s2s, {new_connection, [MyServer, Server, From, FromTo,
							    MaxS2SConnectionsNumber,
							    MaxS2SConnectionsNumberPerNode, Opts]})
    end.

new_connection_int(MyServer, Server, From, FromTo,
	       MaxS2SConnectionsNumber, MaxS2SConnectionsNumberPerNode, Opts) ->
    {ok, Pid} = ejabberd_s2s_out:start(MyServer, Server, Opts),
    F = fun() ->
		L = mnesia:read({s2s, FromTo}),
		NeededConnections = needed_connections_number(L,
							      MaxS2SConnectionsNumber,
							      MaxS2SConnectionsNumberPerNode),
		if NeededConnections > 0 ->
			mnesia:write(#s2s{fromto = FromTo, pid = Pid}),
			Pid;
		   true -> choose_connection(From, L)
		end
	end,
    TRes = mnesia:transaction(F),
    case TRes of
	{atomic, Pid1} ->
	    if Pid1 == Pid ->
		    erlang:monitor(process, Pid),
		    ejabberd_s2s_out:connect(Pid);
	       true ->
		    ejabberd_s2s_out:stop_async(Pid)
	    end,
	    [Pid1];
	{aborted, Reason} ->
	    ?ERROR_MSG("Failed to register s2s connection ~ts -> ~ts: "
		       "Mnesia failure: ~p",
		       [MyServer, Server, Reason]),
	    ejabberd_s2s_out:stop_async(Pid),
	    []
    end.

-spec max_s2s_connections_number({binary(), binary()}) -> pos_integer().
max_s2s_connections_number({From, To}) ->
    case ejabberd_shaper:match(From, max_s2s_connections, jid:make(To)) of
	Max when is_integer(Max) -> Max;
	_ -> ?DEFAULT_MAX_S2S_CONNECTIONS_NUMBER
    end.

-spec max_s2s_connections_number_per_node({binary(), binary()}) -> pos_integer().
max_s2s_connections_number_per_node({From, To}) ->
    case ejabberd_shaper:match(From, max_s2s_connections_per_node, jid:make(To)) of
	Max when is_integer(Max) -> Max;
	_ -> ?DEFAULT_MAX_S2S_CONNECTIONS_NUMBER_PER_NODE
    end.

-spec needed_connections_number([#s2s{}], integer(), integer()) -> integer().
needed_connections_number(Ls, MaxS2SConnectionsNumber,
			  MaxS2SConnectionsNumberPerNode) ->
    LocalLs = [L || L <- Ls, node(L#s2s.pid) == node()],
    lists:min([MaxS2SConnectionsNumber - length(Ls),
	       MaxS2SConnectionsNumberPerNode - length(LocalLs)]).

%%%----------------------------------------------------------------------
%%% ejabberd commands

get_commands_spec() ->
    [#ejabberd_commands{
        name = incoming_s2s_number, tags = [statistics, s2s],
        desc = "Number of incoming s2s connections on the node",
	policy = admin,
	module = ?MODULE, function = incoming_s2s_number,
	args = [], result = {s2s_incoming, integer}},
     #ejabberd_commands{
        name = outgoing_s2s_number, tags = [statistics, s2s],
        desc = "Number of outgoing s2s connections on the node",
	policy = admin,
	module = ?MODULE, function = outgoing_s2s_number,
	args = [], result = {s2s_outgoing, integer}},
     #ejabberd_commands{
	name = stop_s2s_connections, tags = [s2s],
	desc = "Stop all s2s outgoing and incoming connections",
	policy = admin,
	module = ?MODULE, function = stop_s2s_connections,
	args = [], result = {res, rescode}}].

%% TODO Move those stats commands to ejabberd stats command ?
incoming_s2s_number() ->
    supervisor_count(ejabberd_s2s_in_sup).

outgoing_s2s_number() ->
    supervisor_count(ejabberd_s2s_out_sup).

-spec supervisor_count(atom()) -> non_neg_integer().
supervisor_count(Supervisor) ->
    try supervisor:count_children(Supervisor) of
	Props ->
	    proplists:get_value(workers, Props, 0)
    catch _:_ ->
	    0
    end.

-spec stop_s2s_connections() -> ok.
stop_s2s_connections() ->
    stop_s2s_connections(xmpp:serr_reset()).

-spec stop_s2s_connections(stream_error()) -> ok.
stop_s2s_connections(Err) ->
    lists:foreach(
      fun({_Id, Pid, _Type, _Module}) ->
	      ejabberd_s2s_in:send(Pid, Err),
	      ejabberd_s2s_in:stop_async(Pid),
	      supervisor:terminate_child(ejabberd_s2s_in_sup, Pid)
      end, supervisor:which_children(ejabberd_s2s_in_sup)),
    lists:foreach(
      fun({_Id, Pid, _Type, _Module}) ->
	      ejabberd_s2s_out:send(Pid, Err),
	      ejabberd_s2s_out:stop_async(Pid),
	      supervisor:terminate_child(ejabberd_s2s_out_sup, Pid)
      end, supervisor:which_children(ejabberd_s2s_out_sup)),
    _ = mnesia:clear_table(s2s),
    ok.

-spec stream_error() -> stream_error().
stream_error() ->
    case ejabberd_cluster:get_nodes() of
	[Node] when Node == node() -> xmpp:serr_system_shutdown();
	_ -> xmpp:serr_reset()
    end.

%%%----------------------------------------------------------------------
%%% Update Mnesia tables

update_tables() ->
    _ = mnesia:delete_table(local_s2s),
    ok.

%% Check if host is in blacklist or white list
-spec allow_host(binary(), binary()) -> boolean().
allow_host(MyServer, S2SHost) ->
    allow_host1(MyServer, S2SHost) andalso
	not is_temporarly_blocked(S2SHost).

-spec allow_host1(binary(), binary()) -> boolean().
allow_host1(MyHost, S2SHost) ->
    Rule = ejabberd_option:s2s_access(MyHost),
    JID = jid:make(S2SHost),
    case acl:match_rule(MyHost, Rule, JID) of
        deny -> false;
        allow ->
            case ejabberd_hooks:run_fold(s2s_allow_host, MyHost,
                                         allow, [MyHost, S2SHost]) of
                deny -> false;
                allow -> true
            end
    end.

%% Get information about S2S connections of the specified type.
%% @spec (Type) -> [Info]
%% where Type = in | out
%%       Info = [{InfoName::atom(), InfoValue::any()}]
get_info_s2s_connections(Type) ->
    ChildType = case Type of
		    in -> ejabberd_s2s_in_sup;
		    out -> ejabberd_s2s_out_sup
		end,
    Connections = supervisor:which_children(ChildType),
    get_s2s_info(Connections, Type).

get_s2s_info(Connections, Type) ->
    complete_s2s_info(Connections, Type, []).

complete_s2s_info([], _, Result) -> Result;
complete_s2s_info([Connection | T], Type, Result) ->
    {_, PID, _, _} = Connection,
    State = get_s2s_state(PID),
    complete_s2s_info(T, Type, [State | Result]).

-spec get_s2s_state(pid()) -> [{status, open | closed | error} | {s2s_pid, pid()}].
get_s2s_state(S2sPid) ->
    Infos = case p1_fsm:sync_send_all_state_event(S2sPid,
						  get_state_infos)
	    of
		{state_infos, Is} -> [{status, open} | Is];
		{noproc, _} -> [{status, closed}]; %% Connection closed
		{badrpc, _} -> [{status, error}]
	    end,
    [{s2s_pid, S2sPid} | Infos].
