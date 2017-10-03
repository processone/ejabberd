%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : S2S connections manager
%%% Created :  7 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_s2s).

-protocol({xep, 220, '1.1'}).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/0, route/1, have_connection/1,
	 get_connections_pids/1, try_register/1,
	 remove_connection/2, start_connection/2, start_connection/3,
	 dirty_get_connections/0, allow_host/2,
	 incoming_s2s_number/0, outgoing_s2s_number/0,
	 stop_s2s_connections/0,
	 clean_temporarily_blocked_table/0,
	 list_temporarily_blocked_hosts/0,
	 external_host_overloaded/1, is_temporarly_blocked/1,
	 get_commands_spec/0, zlib_enabled/1, get_idle_timeout/1,
	 tls_required/1, tls_verify/1, tls_enabled/1, tls_options/2,
	 host_up/1, host_down/1, queue_type/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-export([get_info_s2s_connections/1,
	 transform_options/1, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

-include("ejabberd_commands.hrl").

-include_lib("public_key/include/public_key.hrl").

-define(PKIXEXPLICIT, 'OTP-PUB-KEY').

-define(PKIXIMPLICIT, 'OTP-PUB-KEY').

-include("XmppAddr.hrl").

-define(DEFAULT_MAX_S2S_CONNECTIONS_NUMBER, 1).

-define(DEFAULT_MAX_S2S_CONNECTIONS_NUMBER_PER_NODE, 1).

-define(S2S_OVERLOAD_BLOCK_PERIOD, 60).

%% once a server is temporarly blocked, it stay blocked for 60 seconds

-record(s2s, {fromto = {<<"">>, <<"">>} :: {binary(), binary()} | '_',
              pid = self()              :: pid() | '_' | '$1'}).

-record(state, {}).

-record(temporarily_blocked, {host = <<"">>     :: binary(),
                              timestamp         :: integer()}).

-type temporarily_blocked() :: #temporarily_blocked{}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

-spec route(stanza()) -> ok.

route(Packet) ->
    try do_route(Packet)
    catch E:R ->
            ?ERROR_MSG("failed to route packet:~n~s~nReason = ~p",
                       [xmpp:pp(Packet), {E, {R, erlang:get_stacktrace()}}])
    end.

clean_temporarily_blocked_table() ->
    mnesia:clear_table(temporarily_blocked).

-spec list_temporarily_blocked_hosts() -> [temporarily_blocked()].

list_temporarily_blocked_hosts() ->
    ets:tab2list(temporarily_blocked).

-spec external_host_overloaded(binary()) -> {aborted, any()} | {atomic, ok}.

external_host_overloaded(Host) ->
    ?INFO_MSG("Disabling connections from ~s for ~p "
	      "seconds",
	      [Host, ?S2S_OVERLOAD_BLOCK_PERIOD]),
    mnesia:transaction(fun () ->
                               Time = p1_time_compat:monotonic_time(),
			       mnesia:write(#temporarily_blocked{host = Host,
								 timestamp = Time})
		       end).

-spec is_temporarly_blocked(binary()) -> boolean().

is_temporarly_blocked(Host) ->
    case mnesia:dirty_read(temporarily_blocked, Host) of
      [] -> false;
      [#temporarily_blocked{timestamp = T} = Entry] ->
          Diff = p1_time_compat:monotonic_time() - T,
	  case p1_time_compat:convert_time_unit(Diff, native, micro_seconds) of
	    N when N > (?S2S_OVERLOAD_BLOCK_PERIOD) * 1000 * 1000 ->
		mnesia:dirty_delete_object(Entry), false;
	    _ -> true
	  end
    end.

-spec remove_connection({binary(), binary()},
                        pid()) -> {atomic, ok} | ok | {aborted, any()}.

remove_connection(FromTo, Pid) ->
    case catch mnesia:dirty_match_object(s2s,
					 #s2s{fromto = FromTo, pid = Pid})
	of
      [#s2s{pid = Pid}] ->
	  F = fun () ->
		      mnesia:delete_object(#s2s{fromto = FromTo, pid = Pid})
	      end,
	  mnesia:transaction(F);
      _ -> ok
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

-spec try_register({binary(), binary()}) -> boolean().

try_register(FromTo) ->
    MaxS2SConnectionsNumber = max_s2s_connections_number(FromTo),
    MaxS2SConnectionsNumberPerNode =
	max_s2s_connections_number_per_node(FromTo),
    F = fun () ->
		L = mnesia:read({s2s, FromTo}),
		NeededConnections = needed_connections_number(L,
							      MaxS2SConnectionsNumber,
							      MaxS2SConnectionsNumberPerNode),
		if NeededConnections > 0 ->
		       mnesia:write(#s2s{fromto = FromTo, pid = self()}),
		       true;
		   true -> false
		end
	end,
    case mnesia:transaction(F) of
      {atomic, Res} -> Res;
      _ -> false
    end.

-spec dirty_get_connections() -> [{binary(), binary()}].

dirty_get_connections() ->
    mnesia:dirty_all_keys(s2s).

-spec tls_options(binary(), [proplists:property()]) -> [proplists:property()].
tls_options(LServer, DefaultOpts) ->
    TLSOpts1 = case ejabberd_config:get_option(
		      {domain_certfile, LServer},
		      ejabberd_config:get_option(
			{s2s_certfile, LServer})) of
		   undefined -> DefaultOpts;
		   CertFile -> lists:keystore(certfile, 1, DefaultOpts,
					      {certfile, CertFile})
	       end,
    TLSOpts2 = case ejabberd_config:get_option(
		      {s2s_ciphers, LServer}) of
                   undefined -> TLSOpts1;
                   Ciphers -> lists:keystore(ciphers, 1, TLSOpts1,
					     {ciphers, Ciphers})
               end,
    TLSOpts3 = case ejabberd_config:get_option(
                      {s2s_protocol_options, LServer}) of
                   undefined -> TLSOpts2;
                   ProtoOpts -> lists:keystore(protocol_options, 1, TLSOpts2,
					       {protocol_options, ProtoOpts})
               end,
    TLSOpts4 = case ejabberd_config:get_option(
		      {s2s_dhfile, LServer}) of
                   undefined -> TLSOpts3;
                   DHFile -> lists:keystore(dhfile, 1, TLSOpts3,
					    {dhfile, DHFile})
               end,
    TLSOpts5 = case ejabberd_config:get_option(
		      {s2s_cafile, LServer}) of
		   undefined -> TLSOpts4;
		   CAFile -> lists:keystore(cafile, 1, TLSOpts4,
					    {cafile, CAFile})
	       end,
    case ejabberd_config:get_option({s2s_tls_compression, LServer}) of
	undefined -> TLSOpts5;
	false -> [compression_none | TLSOpts5];
	true -> lists:delete(compression_none, TLSOpts5)
    end.

-spec tls_required(binary()) -> boolean().
tls_required(LServer) ->
    TLS = use_starttls(LServer),
    TLS == required orelse TLS == required_trusted.

-spec tls_verify(binary()) -> boolean().
tls_verify(LServer) ->
    TLS = use_starttls(LServer),
    TLS == required_trusted.

-spec tls_enabled(binary()) -> boolean().
tls_enabled(LServer) ->
    TLS = use_starttls(LServer),
    TLS /= false.

-spec zlib_enabled(binary()) -> boolean().
zlib_enabled(LServer) ->
    ejabberd_config:get_option({s2s_zlib, LServer}, false).

-spec use_starttls(binary()) -> boolean() | optional | required | required_trusted.
use_starttls(LServer) ->
    ejabberd_config:get_option({s2s_use_starttls, LServer}, false).

-spec get_idle_timeout(binary()) -> non_neg_integer() | infinity.
get_idle_timeout(LServer) ->
    ejabberd_config:get_option({s2s_timeout, LServer}, timer:minutes(10)).

-spec queue_type(binary()) -> ram | file.
queue_type(LServer) ->
    ejabberd_config:get_option(
      {s2s_queue_type, LServer},
      ejabberd_config:default_queue_type(LServer)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    update_tables(),
    ejabberd_mnesia:create(?MODULE, s2s,
			[{ram_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, s2s)}]),
    mnesia:subscribe(system),
    ejabberd_commands:register_commands(get_commands_spec()),
    ejabberd_mnesia:create(?MODULE, temporarily_blocked,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, temporarily_blocked)}]),
    ejabberd_hooks:add(host_up, ?MODULE, host_up, 50),
    ejabberd_hooks:add(host_down, ?MODULE, host_down, 60),
    lists:foreach(fun host_up/1, ?MYHOSTS),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    clean_table_from_bad_node(Node),
    {noreply, State};
handle_info({route, Packet}, State) ->
    route(Packet),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_commands:unregister_commands(get_commands_spec()),
    lists:foreach(fun host_down/1, ?MYHOSTS),
    ejabberd_hooks:delete(host_up, ?MODULE, host_up, 50),
    ejabberd_hooks:delete(host_down, ?MODULE, host_down, 60),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
host_up(Host) ->
    ejabberd_s2s_in:host_up(Host),
    ejabberd_s2s_out:host_up(Host).

host_down(Host) ->
    lists:foreach(
      fun(#s2s{fromto = {From, _}, pid = Pid}) when node(Pid) == node() ->
	      case ejabberd_router:host_of_route(From) of
		  Host ->
		      ejabberd_s2s_out:send(Pid, xmpp:serr_system_shutdown()),
		      ejabberd_s2s_out:stop(Pid);
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
		       [{#s2s{pid = '$1', _ = '_'},
			 [{'==', {node, '$1'}, Node}],
			 ['$_']}]),
		lists:foreach(fun(E) ->
				      mnesia:delete_object(E)
			      end, Es)
	end,
    mnesia:async_dirty(F).

-spec do_route(stanza()) -> ok.
do_route(Packet) ->
    ?DEBUG("local route:~n~s", [xmpp:pp(Packet)]),
    From = xmpp:get_from(Packet),
    To = xmpp:get_to(Packet),
    case start_connection(From, To) of
	{ok, Pid} when is_pid(Pid) ->
	  ?DEBUG("sending to process ~p~n", [Pid]),
	  #jid{lserver = MyServer} = From,
	    ejabberd_hooks:run(s2s_send_packet, MyServer, [Packet]),
	    ejabberd_s2s_out:route(Pid, Packet);
	{error, Reason} ->
	  Lang = xmpp:get_lang(Packet),
	    Err = case Reason of
		      policy_violation ->
			  xmpp:err_policy_violation(
			    <<"Server connections to local "
			      "subdomains are forbidden">>, Lang);
		      forbidden ->
			  xmpp:err_forbidden(<<"Access denied by service policy">>, Lang);
		      internal_server_error ->
			  xmpp:err_internal_server_error()
		  end,
	    ejabberd_router:route_error(Packet, Err)
    end.

-spec start_connection(jid(), jid())
      -> {ok, pid()} | {error, policy_violation | forbidden | internal_server_error}.
start_connection(From, To) ->
    start_connection(From, To, []).

-spec start_connection(jid(), jid(), [proplists:property()])
      -> {ok, pid()} | {error, policy_violation | forbidden | internal_server_error}.
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
	  case is_service(From, To) of
	    true ->
		  {error, policy_violation};
	      false ->
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
		  end
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
	lists:nth(erlang:phash(jid:remove_resource(From),
			       length(Pids1)),
		  Pids1),
    ?DEBUG("Using ejabberd_s2s_out ~p~n", [Pid]),
    Pid.

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

new_connection(MyServer, Server, From, FromTo,
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
		    ejabberd_s2s_out:connect(Pid);
	       true ->
		    ejabberd_s2s_out:stop(Pid)
	    end,
	    [Pid1];
      {aborted, Reason} ->
	    ?ERROR_MSG("failed to register connection ~s -> ~s: ~p",
		       [MyServer, Server, Reason]),
	    ejabberd_s2s_out:stop(Pid),
	    []
    end.

-spec max_s2s_connections_number({binary(), binary()}) -> integer().
max_s2s_connections_number({From, To}) ->
    case acl:match_rule(From, max_s2s_connections, jid:make(To)) of
      Max when is_integer(Max) -> Max;
      _ -> ?DEFAULT_MAX_S2S_CONNECTIONS_NUMBER
    end.

-spec max_s2s_connections_number_per_node({binary(), binary()}) -> integer().
max_s2s_connections_number_per_node({From, To}) ->
    case acl:match_rule(From, max_s2s_connections_per_node, jid:make(To)) of
      Max when is_integer(Max) -> Max;
      _ -> ?DEFAULT_MAX_S2S_CONNECTIONS_NUMBER_PER_NODE
    end.

-spec needed_connections_number([#s2s{}], integer(), integer()) -> integer().
needed_connections_number(Ls, MaxS2SConnectionsNumber,
			  MaxS2SConnectionsNumberPerNode) ->
    LocalLs = [L || L <- Ls, node(L#s2s.pid) == node()],
    lists:min([MaxS2SConnectionsNumber - length(Ls),
	       MaxS2SConnectionsNumberPerNode - length(LocalLs)]).

%%--------------------------------------------------------------------
%% Function: is_service(From, To) -> true | false
%% Description: Return true if the destination must be considered as a
%% service.
%% --------------------------------------------------------------------
-spec is_service(jid(), jid()) -> boolean().
is_service(From, To) ->
    LFromDomain = From#jid.lserver,
    case ejabberd_config:get_option({route_subdomains, LFromDomain}, local) of
      s2s -> % bypass RFC 3920 10.3
	  false;
      local ->
	  Hosts = (?MYHOSTS),
	  P = fun (ParentDomain) ->
		      lists:member(ParentDomain, Hosts)
	      end,
	  lists:any(P, parent_domains(To#jid.lserver))
    end.

parent_domains(Domain) ->
    lists:foldl(fun (Label, []) -> [Label];
		    (Label, [Head | Tail]) ->
			[<<Label/binary, ".", Head/binary>>, Head | Tail]
		end,
		[], lists:reverse(str:tokens(Domain, <<".">>))).

%%%----------------------------------------------------------------------
%%% ejabberd commands

get_commands_spec() ->
    [#ejabberd_commands{
        name = incoming_s2s_number, tags = [stats, s2s],
        desc = "Number of incoming s2s connections on the node",
	policy = admin,
	module = ?MODULE, function = incoming_s2s_number,
	args = [], result = {s2s_incoming, integer}},
     #ejabberd_commands{
        name = outgoing_s2s_number, tags = [stats, s2s],
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

supervisor_count(Supervisor) ->
    case catch supervisor:which_children(Supervisor) of
        {'EXIT', _} -> 0;
        Result ->
            length(Result)
    end.

-spec stop_s2s_connections() -> ok.
stop_s2s_connections() ->
    lists:foreach(
      fun({_Id, Pid, _Type, _Module}) ->
	      supervisor:terminate_child(ejabberd_s2s_in_sup, Pid)
      end, supervisor:which_children(ejabberd_s2s_in_sup)),
    lists:foreach(
      fun({_Id, Pid, _Type, _Module}) ->
	      supervisor:terminate_child(ejabberd_s2s_out_sup, Pid)
      end, supervisor:which_children(ejabberd_s2s_out_sup)),
    mnesia:clear_table(s2s),
    ok.

%%%----------------------------------------------------------------------
%%% Update Mnesia tables

update_tables() ->
    case catch mnesia:table_info(s2s, type) of
      bag -> ok;
      {'EXIT', _} -> ok;
      _ -> mnesia:delete_table(s2s)
    end,
    case catch mnesia:table_info(s2s, attributes) of
      [fromto, node, key] ->
	  mnesia:transform_table(s2s, ignore, [fromto, pid]),
	  mnesia:clear_table(s2s);
      [fromto, pid, key] ->
	  mnesia:transform_table(s2s, ignore, [fromto, pid]),
	  mnesia:clear_table(s2s);
      [fromto, pid] -> ok;
      {'EXIT', _} -> ok
    end,
    case lists:member(local_s2s, mnesia:system_info(tables)) of
	true -> mnesia:delete_table(local_s2s);
	false -> ok
    end.

%% Check if host is in blacklist or white list
allow_host(MyServer, S2SHost) ->
    allow_host1(MyServer, S2SHost) andalso
      not is_temporarly_blocked(S2SHost).

allow_host1(MyHost, S2SHost) ->
    Rule = ejabberd_config:get_option({s2s_access, MyHost}, all),
    JID = jid:make(S2SHost),
    case acl:match_rule(MyHost, Rule, JID) of
        deny -> false;
        allow ->
            case ejabberd_hooks:run_fold(s2s_allow_host, MyHost,
                                         allow, [MyHost, S2SHost]) of
                deny -> false;
                allow -> true;
                _ -> true
            end
    end.

transform_options(Opts) ->
    lists:foldl(fun transform_options/2, [], Opts).

transform_options({{s2s_host, Host}, Action}, Opts) ->
    ?WARNING_MSG("Option 's2s_host' is deprecated. "
                 "The option is still supported but it is better to "
                 "fix your config: use access rules instead.", []),
    ACLName = misc:binary_to_atom(
                iolist_to_binary(["s2s_access_", Host])),
    [{acl, ACLName, {server, Host}},
     {access, s2s, [{Action, ACLName}]},
     {s2s_access, s2s} |
     Opts];
transform_options({s2s_default_policy, Action}, Opts) ->
    ?WARNING_MSG("Option 's2s_default_policy' is deprecated. "
                 "The option is still supported but it is better to "
                 "fix your config: "
                 "use 's2s_access' with an access rule.", []),
    [{access, s2s, [{Action, all}]},
     {s2s_access, s2s} |
     Opts];
transform_options(Opt, Opts) ->
    [Opt|Opts].

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

-type use_starttls() :: boolean() | optional | required | required_trusted.
-spec opt_type(route_subdomains) -> fun((s2s | local) -> s2s | local);
	      (s2s_access) -> fun((any()) -> any());
	      (s2s_certfile) -> fun((binary()) -> binary());
	      (s2s_ciphers) -> fun((binary()) -> binary());
	      (s2s_dhfile) -> fun((binary()) -> binary());
	      (s2s_cafile) -> fun((binary()) -> binary());
	      (s2s_protocol_options) -> fun(([binary()]) -> binary());
	      (s2s_tls_compression) -> fun((boolean()) -> boolean());
	      (s2s_use_starttls) -> fun((use_starttls()) -> use_starttls());
	      (s2s_zlib) -> fun((boolean()) -> boolean());
	      (s2s_timeout) -> fun((timeout()) -> timeout());
	      (s2s_queue_type) -> fun((ram | file) -> ram | file);
	      (atom()) -> [atom()].
opt_type(route_subdomains) ->
    fun (s2s) -> s2s;
	(local) -> local
    end;
opt_type(s2s_access) ->
    fun acl:access_rules_validator/1;
opt_type(s2s_certfile) -> fun misc:try_read_file/1;
opt_type(s2s_ciphers) -> fun iolist_to_binary/1;
opt_type(s2s_dhfile) -> fun misc:try_read_file/1;
opt_type(s2s_cafile) -> fun misc:try_read_file/1;
opt_type(s2s_protocol_options) ->
    fun (Options) -> str:join(Options, <<"|">>) end;
opt_type(s2s_tls_compression) ->
    fun (true) -> true;
	(false) -> false
    end;
opt_type(s2s_use_starttls) ->
    fun (true) -> true;
	(false) -> false;
	(optional) -> optional;
	(required) -> required;
	(required_trusted) ->
	    ?WARNING_MSG("The value 'required_trusted' of option "
			 "'s2s_use_starttls' is deprected and will be "
			 "unsupported in future releases. Instead, "
			 "set it to 'required' and make sure "
			 "mod_s2s_dialback is *NOT* loaded", []),
	    required_trusted
    end;
opt_type(s2s_zlib) ->
    fun(B) when is_boolean(B) -> B end;
opt_type(s2s_timeout) ->
    fun(I) when is_integer(I), I >= 0 -> timer:seconds(I);
       (infinity) -> infinity;
       (unlimited) -> infinity
    end;
opt_type(s2s_queue_type) ->
    fun(ram) -> ram; (file) -> file end;
opt_type(_) ->
    [route_subdomains, s2s_access,  s2s_certfile, s2s_zlib,
     s2s_ciphers, s2s_dhfile, s2s_cafile, s2s_protocol_options,
     s2s_tls_compression, s2s_use_starttls, s2s_timeout, s2s_queue_type].
