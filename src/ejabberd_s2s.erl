%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : S2S connections manager
%%% Created :  7 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
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

-module(ejabberd_s2s).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/0, route/3, have_connection/1,
	 has_key/2, get_connections_pids/1, try_register/1,
	 remove_connection/3, find_connection/2,
	 dirty_get_connections/0, allow_host/2,
	 incoming_s2s_number/0, outgoing_s2s_number/0,
	 clean_temporarily_blocked_table/0,
	 list_temporarily_blocked_hosts/0,
         get_connections_number/1, needed_connections_number/3,
         get_connections_number_per_node/1,
	 external_host_overloaded/1, is_temporarly_blocked/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

%% ejabberd API
-export([get_info_s2s_connections/1]).

-include("ejabberd.hrl").

-include("jlib.hrl").

-include("ejabberd_commands.hrl").

-define(DEFAULT_MAX_S2S_CONNECTIONS_NUMBER, 1).

-define(DEFAULT_MAX_S2S_CONNECTIONS_NUMBER_PER_NODE, 1).

-define(S2S_OVERLOAD_BLOCK_PERIOD, 60).

%% once a server is temporarly blocked, it stay blocked for 60 seconds

-record(s2s, {fromto = {<<"">>, <<"">>} :: {binary(), binary()},
              pid = self()              :: pid() | '_',
              key = <<"">>              :: binary() | '_'}).

-record(state, {}).

-record(temporarily_blocked, {host = <<"">>     :: binary(),
                              timestamp = now() :: erlang:timestamp()}).

-type temporarily_blocked() :: #temporarily_blocked{}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

-spec route(jid(), jid(), xmlel()) -> ok.

route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
      {'EXIT', Reason} ->
	  ?ERROR_MSG("~p~nwhen processing: ~p",
		     [Reason, {From, To, Packet}]);
      _ -> ok
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
			       mnesia:write(#temporarily_blocked{host = Host,
								 timestamp =
								     now()})
		       end).

-spec is_temporarly_blocked(binary()) -> boolean().

is_temporarly_blocked(Host) ->
    case mnesia:dirty_read(temporarily_blocked, Host) of
      [] -> false;
      [#temporarily_blocked{timestamp = T} = Entry] ->
	  case timer:now_diff(now(), T) of
	    N when N > (?S2S_OVERLOAD_BLOCK_PERIOD) * 1000 * 1000 ->
		mnesia:dirty_delete_object(Entry), false;
	    _ -> true
	  end
    end.

-spec remove_connection({binary(), binary()},
                        pid(), binary()) -> {atomic, ok} |
                                            ok |
                                            {aborted, any()}.

remove_connection(FromTo, Pid, Key) ->
    case catch mnesia:dirty_match_object(s2s,
					 #s2s{fromto = FromTo, pid = Pid,
					      _ = '_'})
	of
      [#s2s{pid = Pid, key = Key}] ->
	  F = fun () ->
		      mnesia:delete_object(#s2s{fromto = FromTo, pid = Pid,
						key = Key})
	      end,
	  mnesia:transaction(F);
      _ -> ok
    end.

-spec have_connection({binary(), binary()}) -> boolean().

have_connection(FromTo) ->
    case mnesia:dirty_read(s2s, FromTo) of
      [_] -> true;
      _ -> false
    end.

-spec has_key({binary(), binary()}, binary()) -> boolean().

has_key(FromTo, Key) ->
    Query = [{#s2s{fromto = FromTo, key = Key, _ = '_'}, [],
	      ['$_']}],
    case get_node_by_key(Key) of
      Node when Node == node() ->
	  case mnesia:dirty_select(s2s, Query) of
	    [] -> false;
	    _ -> true
	  end;
      Node ->
	  case catch rpc:call(Node, mnesia, dirty_select,
			      [s2s, Query], 5000)
	      of
	    [_ | _] -> true;
	    _ -> false
	  end
    end.

-spec get_connections_pids({binary(), binary()}) -> [pid()].

get_connections_pids(FromTo) ->
    case catch mnesia:dirty_read(s2s, FromTo) of
      L when is_list(L) ->
	  [Connection#s2s.pid || Connection <- L];
      _ -> []
    end.

-spec get_connections_number({binary(), binary()}) -> non_neg_integer().

get_connections_number(FromTo) ->
    {ResL, _BadNodes} = rpc:multicall(
                          ejabberd_cluster:get_nodes(),
                          ?MODULE, get_connections_number_per_node, [FromTo]),
    lists:sum(ResL).

-spec get_connections_number_per_node({binary(), binary()}) -> non_neg_integer().

get_connections_number_per_node(FromTo) ->
    ets:select_count(s2s, [{#s2s{fromto = FromTo, _ = '_'}, [], [true]}]).

-spec try_register({binary(), binary()}) -> {key, binary()} | false.

try_register(FromTo) ->
    Key = new_key(),
    MaxS2SConnectionsNumber =
	max_s2s_connections_number(FromTo),
    MaxS2SConnectionsNumberPerNode =
	max_s2s_connections_number_per_node(FromTo),
    NeededConnections = needed_connections_number(
                          FromTo,
                          MaxS2SConnectionsNumber,
                          MaxS2SConnectionsNumberPerNode),
    F = fun () ->
		if NeededConnections > 0 ->
		       mnesia:write(#s2s{fromto = FromTo, pid = self(),
					 key = Key}),
		       {key, Key};
		   true -> false
		end
	end,
    case mnesia:transaction(F) of
      {atomic, Res} -> Res;
      _ -> false
    end.

-spec dirty_get_connections() -> [{binary(), binary()}].

dirty_get_connections() ->
    lists:flatmap(fun (Node) when Node == node() ->
			  mnesia:dirty_all_keys(s2s);
		      (Node) ->
			  case catch rpc:call(Node, mnesia, dirty_all_keys,
					      [s2s], 5000)
			      of
			    L when is_list(L) -> L;
			    _ -> []
			  end
		  end,
		  ejabberd_cluster:get_nodes()).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    update_tables(),
    mnesia:create_table(s2s,
			[{ram_copies, [node()]}, {type, bag},
			 {local_content, true},
			 {attributes, record_info(fields, s2s)}]),
    mnesia:add_table_copy(s2s, node(), ram_copies),
    ejabberd_commands:register_commands(commands()),
    mnesia:create_table(temporarily_blocked,
			[{ram_copies, [node()]},
			 {attributes,
			  record_info(fields, temporarily_blocked)}]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
      {'EXIT', Reason} ->
	  ?ERROR_MSG("~p~nwhen processing: ~p",
		     [Reason, {From, To, Packet}]);
      _ -> ok
    end,
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_commands:unregister_commands(commands()), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

do_route(From, To, Packet) ->
    ?DEBUG("s2s manager~n\tfrom ~p~n\tto ~p~n\tpacket "
	   "~P~n",
	   [From, To, Packet, 8]),
    case find_connection(From, To) of
      {atomic, Pid} when is_pid(Pid) ->
	  ?DEBUG("sending to process ~p~n", [Pid]),
	  #xmlel{name = Name, attrs = Attrs, children = Els} =
	      Packet,
	  NewAttrs =
	      jlib:replace_from_to_attrs(jlib:jid_to_string(From),
					 jlib:jid_to_string(To), Attrs),
	  #jid{lserver = MyServer} = From,
	  ejabberd_hooks:run(s2s_send_packet, MyServer,
			     [From, To, Packet]),
	  send_element(Pid,
		       #xmlel{name = Name, attrs = NewAttrs, children = Els}),
	  ok;
      {aborted, _Reason} ->
	  case xml:get_tag_attr_s(<<"type">>, Packet) of
	    <<"error">> -> ok;
	    <<"result">> -> ok;
	    _ ->
		Err = jlib:make_error_reply(Packet,
					    ?ERR_SERVICE_UNAVAILABLE),
		ejabberd_router:route(To, From, Err)
	  end,
	  false
    end.

-spec find_connection(jid(), jid()) -> {aborted, any()} | {atomic, pid()}.

find_connection(From, To) ->
    #jid{lserver = MyServer} = From,
    #jid{lserver = Server} = To,
    FromTo = {MyServer, Server},
    MaxS2SConnectionsNumber =
	max_s2s_connections_number(FromTo),
    MaxS2SConnectionsNumberPerNode =
	max_s2s_connections_number_per_node(FromTo),
    NeededConnections = needed_connections_number(
                          FromTo,
                          MaxS2SConnectionsNumber,
                          MaxS2SConnectionsNumberPerNode),
    ?DEBUG("Finding connection for ~p~n", [FromTo]),
    case catch mnesia:dirty_read(s2s, FromTo) of
      {'EXIT', Reason} -> {aborted, Reason};
      [] ->
	  %% We try to establish all the connections if the host is not a
	  %% service and if the s2s host is not blacklisted or
	  %% is in whitelist:
	  case not is_service(From, To) andalso
		 allow_host(MyServer, Server)
	      of
	    true ->
		open_several_connections(NeededConnections, MyServer,
					 Server, From, FromTo,
					 MaxS2SConnectionsNumber,
					 MaxS2SConnectionsNumberPerNode);
	    false -> {aborted, error}
	  end;
      L when is_list(L) ->
	  if NeededConnections > 0 ->
		 %% We establish the missing connections for this pair.
		 open_several_connections(NeededConnections, MyServer,
					  Server, From, FromTo,
					  MaxS2SConnectionsNumber,
					  MaxS2SConnectionsNumberPerNode);
	     true ->
		 %% We choose a connexion from the pool of opened ones.
		 {atomic, choose_connection(From, L)}
	  end
    end.

choose_connection(From, Connections) ->
    choose_pid(From, [C#s2s.pid || C <- Connections]).

choose_pid(From, Pids) ->
    Pids1 = case [P || P <- Pids, node(P) == node()] of
	      [] -> Pids;
	      Ps -> Ps
	    end,
    Pid =
	lists:nth(erlang:phash(jlib:jid_remove_resource(From),
			       length(Pids1)),
		  Pids1),
    ?DEBUG("Using ejabberd_s2s_out ~p~n", [Pid]),
    Pid.

open_several_connections(N, MyServer, Server, From,
			 FromTo, MaxS2SConnectionsNumber,
			 MaxS2SConnectionsNumberPerNode) ->
    ConnectionsResult = [new_connection(MyServer, Server,
					From, FromTo, MaxS2SConnectionsNumber,
					MaxS2SConnectionsNumberPerNode)
			 || _N <- lists:seq(1, N)],
    case [PID || {atomic, PID} <- ConnectionsResult] of
      [] -> hd(ConnectionsResult);
      PIDs -> {atomic, choose_pid(From, PIDs)}
    end.

new_connection(MyServer, Server, From, FromTo,
	       MaxS2SConnectionsNumber,
	       MaxS2SConnectionsNumberPerNode) ->
    Key = new_key(),
    {ok, Pid} = ejabberd_s2s_out:start(MyServer, Server,
				       {new, Key}),
    NeededConnections = needed_connections_number(
                          FromTo,
                          MaxS2SConnectionsNumber,
                          MaxS2SConnectionsNumberPerNode),
    F = fun () ->
		L = mnesia:read({s2s, FromTo}),
		if NeededConnections > 0 ->
		       mnesia:write(#s2s{fromto = FromTo, pid = Pid,
					 key = Key}),
		       ?INFO_MSG("New s2s connection started ~p", [Pid]),
		       Pid;
		   true -> choose_connection(From, L)
		end
	end,
    TRes = mnesia:transaction(F),
    case TRes of
      {atomic, Pid} -> ejabberd_s2s_out:start_connection(Pid);
      _ -> ejabberd_s2s_out:stop_connection(Pid)
    end,
    TRes.

max_s2s_connections_number({From, To}) ->
    case acl:match_rule(From, max_s2s_connections,
			jlib:make_jid(<<"">>, To, <<"">>))
	of
      Max when is_integer(Max) -> Max;
      _ -> ?DEFAULT_MAX_S2S_CONNECTIONS_NUMBER
    end.

max_s2s_connections_number_per_node({From, To}) ->
    case acl:match_rule(From, max_s2s_connections_per_node,
			jlib:make_jid(<<"">>, To, <<"">>))
	of
      Max when is_integer(Max) -> Max;
      _ -> ?DEFAULT_MAX_S2S_CONNECTIONS_NUMBER_PER_NODE
    end.

needed_connections_number(FromTo,
                          MaxS2SConnectionsNumber,
			  MaxS2SConnectionsNumberPerNode) ->
    lists:min(
      [MaxS2SConnectionsNumber - get_connections_number(FromTo),
       MaxS2SConnectionsNumberPerNode - get_connections_number_per_node(FromTo)]).

new_key() ->
    <<(randoms:get_string())/binary, "-",
      (ejabberd_cluster:node_id())/binary>>.

get_node_by_key(Key) ->
    case str:tokens(Key, <<"-">>) of
      [_, NodeID] -> ejabberd_cluster:get_node_by_id(NodeID);
      _ -> node()
    end.

is_service(From, To) ->
    LFromDomain = From#jid.lserver,
    case ejabberd_config:get_local_option(
           {route_subdomains, LFromDomain},
           fun(s2s) -> s2s end) of
      s2s -> % bypass RFC 3920 10.3
	  false;
      undefined ->
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

send_element(Pid, El) -> Pid ! {send_element, El}.

%%%----------------------------------------------------------------------
%%% ejabberd commands

commands() ->
    [#ejabberd_commands{name = incoming_s2s_number,
			tags = [stats, s2s],
			desc =
			    "Number of incoming s2s connections on "
			    "the node",
			module = ?MODULE, function = incoming_s2s_number,
			args = [], result = {s2s_incoming, integer}},
     #ejabberd_commands{name = outgoing_s2s_number,
			tags = [stats, s2s],
			desc =
			    "Number of outgoing s2s connections on "
			    "the node",
			module = ?MODULE, function = outgoing_s2s_number,
			args = [], result = {s2s_outgoing, integer}}].

incoming_s2s_number() ->
    length(supervisor:which_children(ejabberd_s2s_in_sup)).

outgoing_s2s_number() ->
    length(supervisor:which_children(ejabberd_s2s_out_sup)).

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
	  mnesia:transform_table(s2s, ignore, [fromto, pid, key]),
	  mnesia:clear_table(s2s);
      [fromto, pid, key] -> ok;
      {'EXIT', _} -> ok
    end,
    case lists:member(local_s2s, mnesia:system_info(tables))
	of
      true -> mnesia:delete_table(local_s2s);
      false -> ok
    end,
    case catch mnesia:table_info(s2s, local_content) of
      false -> mnesia:delete_table(s2s);
      _ -> ok
    end.

allow_host(MyServer, S2SHost) ->
    allow_host2(MyServer, S2SHost) andalso
      not is_temporarly_blocked(S2SHost).

allow_host2(MyServer, S2SHost) ->
    Hosts = (?MYHOSTS),
    case lists:dropwhile(fun (ParentDomain) ->
				 not lists:member(ParentDomain, Hosts)
			 end,
			 parent_domains(MyServer))
	of
      [MyHost | _] -> allow_host1(MyHost, S2SHost);
      [] -> allow_host1(MyServer, S2SHost)
    end.

allow_host1(MyHost, S2SHost) ->
    case ejabberd_config:get_local_option(
           {{s2s_host, S2SHost}, MyHost},
           fun(deny) -> deny; (allow) -> allow end)
	of
      deny -> false;
      allow -> true;
      undefined ->
	  case ejabberd_config:get_local_option(
                 {s2s_default_policy, MyHost},
                 fun(deny) -> deny; (allow) -> allow end)
	      of
	    deny -> false;
	    _ ->
		case ejabberd_hooks:run_fold(s2s_allow_host, MyHost,
					     allow, [MyHost, S2SHost])
		    of
		  deny -> false;
		  allow -> true;
		  _ -> true
		end
	  end
    end.


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
    Infos = case gen_fsm:sync_send_all_state_event(S2sPid,
						   get_state_infos)
		of
	      {state_infos, Is} -> [{status, open} | Is];
	      {noproc, _} -> [{status, closed}]; %% Connection closed
	      {badrpc, _} -> [{status, error}]
	    end,
    [{s2s_pid, S2sPid} | Infos].
