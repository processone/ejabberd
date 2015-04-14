%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : S2S connections manager
%%% Created :  7 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
	 external_host_overloaded/1, is_temporarly_blocked/1,
	 check_peer_certificate/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

%% ejabberd API
-export([get_info_s2s_connections/1, transform_options/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

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
              pid = self()              :: pid() | '_' | '$1',
              key = <<"">>              :: binary() | '_'}).

-record(state, {}).

-record(temporarily_blocked, {host = <<"">>     :: binary(),
                              timestamp = now() :: erlang:timestamp()}).

-type temporarily_blocked() :: #temporarily_blocked{}.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
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
    case catch mnesia:dirty_read(s2s, FromTo) of
        [_] ->
            true;
        _ ->
            false
    end.

-spec has_key({binary(), binary()}, binary()) -> boolean().

has_key(FromTo, Key) ->
    case mnesia:dirty_select(s2s,
			     [{#s2s{fromto = FromTo, key = Key, _ = '_'},
			       [],
			       ['$_']}]) of
	[] ->
	    false;
	_ ->
	    true
    end.

-spec get_connections_pids({binary(), binary()}) -> [pid()].

get_connections_pids(FromTo) ->
    case catch mnesia:dirty_read(s2s, FromTo) of
	L when is_list(L) ->
	    [Connection#s2s.pid || Connection <- L];
	_ ->
	    []
    end.

-spec try_register({binary(), binary()}) -> {key, binary()} | false.

try_register(FromTo) ->
    Key = randoms:get_string(),
    MaxS2SConnectionsNumber = max_s2s_connections_number(FromTo),
    MaxS2SConnectionsNumberPerNode =
	max_s2s_connections_number_per_node(FromTo),
    F = fun () ->
		L = mnesia:read({s2s, FromTo}),
		NeededConnections = needed_connections_number(L,
							      MaxS2SConnectionsNumber,
							      MaxS2SConnectionsNumberPerNode),
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
    mnesia:dirty_all_keys(s2s).

check_peer_certificate(SockMod, Sock, Peer) ->
    case SockMod:get_peer_certificate(Sock) of
      {ok, Cert} ->
	  case SockMod:get_verify_result(Sock) of
	    0 ->
		case idna:domain_utf8_to_ascii(Peer) of
		  false ->
		      {error, <<"Cannot decode remote server name">>};
		  AsciiPeer ->
		      case
			lists:any(fun(D) -> match_domain(AsciiPeer, D) end,
				  get_cert_domains(Cert)) of
			true ->
			    {ok, <<"Verification successful">>};
			false ->
			    {error, <<"Certificate host name mismatch">>}
		      end
		end;
	    VerifyRes ->
		{error, p1_tls:get_cert_verify_string(VerifyRes, Cert)}
	  end;
      error ->
	  {error, <<"Cannot get peer certificate">>}
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
init([]) ->
    update_tables(),
    mnesia:create_table(s2s, [{ram_copies, [node()]}, {type, bag},
			      {attributes, record_info(fields, s2s)}]),
    mnesia:add_table_copy(s2s, node(), ram_copies),
    mnesia:subscribe(system),
    ejabberd_commands:register_commands(commands()),
    mnesia:create_table(temporarily_blocked, [{ram_copies, [node()]}, {attributes, record_info(fields, temporarily_blocked)}]),
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
handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    clean_table_from_bad_node(Node),
    {noreply, State};
handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
      {'EXIT', Reason} ->
	  ?ERROR_MSG("~p~nwhen processing: ~p",
		     [Reason, {From, To, Packet}]);
      _ -> ok
    end,
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

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
		NeededConnections = needed_connections_number([],
							      MaxS2SConnectionsNumber,
							      MaxS2SConnectionsNumberPerNode),
		open_several_connections(NeededConnections, MyServer,
					 Server, From, FromTo,
					 MaxS2SConnectionsNumber,
					 MaxS2SConnectionsNumberPerNode);
	    false -> {aborted, error}
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
	       MaxS2SConnectionsNumber, MaxS2SConnectionsNumberPerNode) ->
    Key = randoms:get_string(),
    {ok, Pid} = ejabberd_s2s_out:start(
		  MyServer, Server, {new, Key}),
    F = fun() ->
		L = mnesia:read({s2s, FromTo}),
		NeededConnections = needed_connections_number(L,
							      MaxS2SConnectionsNumber,
							      MaxS2SConnectionsNumberPerNode),
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
is_service(From, To) ->
    LFromDomain = From#jid.lserver,
    case ejabberd_config:get_option(
           {route_subdomains, LFromDomain},
           fun(s2s) -> s2s; (local) -> local end, local) of
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
    case lists:member(local_s2s, mnesia:system_info(tables)) of
        true ->
            mnesia:delete_table(local_s2s);
        false ->
            ok
    end.

%% Check if host is in blacklist or white list
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
    Rule = ejabberd_config:get_option(
             s2s_access,
             fun(A) when is_atom(A) -> A end,
             all),
    JID = jlib:make_jid(<<"">>, S2SHost, <<"">>),
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
    ACLName = jlib:binary_to_atom(
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
    Infos = case gen_fsm:sync_send_all_state_event(S2sPid,
						   get_state_infos)
		of
	      {state_infos, Is} -> [{status, open} | Is];
	      {noproc, _} -> [{status, closed}]; %% Connection closed
	      {badrpc, _} -> [{status, error}]
	    end,
    [{s2s_pid, S2sPid} | Infos].

get_cert_domains(Cert) ->
    {rdnSequence, Subject} =
	(Cert#'Certificate'.tbsCertificate)#'TBSCertificate'.subject,
    Extensions =
	(Cert#'Certificate'.tbsCertificate)#'TBSCertificate'.extensions,
    lists:flatmap(fun (#'AttributeTypeAndValue'{type =
						    ?'id-at-commonName',
						value = Val}) ->
			  case 'OTP-PUB-KEY':decode('X520CommonName', Val) of
			    {ok, {_, D1}} ->
				D = if is_binary(D1) -> D1;
				       is_list(D1) -> list_to_binary(D1);
				       true -> error
				    end,
				if D /= error ->
				       case jlib:string_to_jid(D) of
					 #jid{luser = <<"">>, lserver = LD,
					      lresource = <<"">>} ->
					     [LD];
					 _ -> []
				       end;
				   true -> []
				end;
			    _ -> []
			  end;
		      (_) -> []
		  end,
		  lists:flatten(Subject))
      ++
      lists:flatmap(fun (#'Extension'{extnID =
					  ?'id-ce-subjectAltName',
				      extnValue = Val}) ->
			    BVal = if is_list(Val) -> list_to_binary(Val);
				      true -> Val
				   end,
			    case 'OTP-PUB-KEY':decode('SubjectAltName', BVal)
				of
			      {ok, SANs} ->
				  lists:flatmap(fun ({otherName,
						      #'AnotherName'{'type-id' =
									 ?'id-on-xmppAddr',
								     value =
									 XmppAddr}}) ->
							case
							  'XmppAddr':decode('XmppAddr',
									    XmppAddr)
							    of
							  {ok, D}
							      when
								is_binary(D) ->
							      case
								jlib:string_to_jid((D))
								  of
								#jid{luser =
									 <<"">>,
								     lserver =
									 LD,
								     lresource =
									 <<"">>} ->
								    case
								      idna:domain_utf8_to_ascii(LD)
									of
								      false ->
									  [];
								      PCLD ->
									  [PCLD]
								    end;
								_ -> []
							      end;
							  _ -> []
							end;
						    ({dNSName, D})
							when is_list(D) ->
							case
							  jlib:string_to_jid(list_to_binary(D))
							    of
							  #jid{luser = <<"">>,
							       lserver = LD,
							       lresource =
								   <<"">>} ->
							      [LD];
							  _ -> []
							end;
						    (_) -> []
						end,
						SANs);
			      _ -> []
			    end;
			(_) -> []
		    end,
		    Extensions).

match_domain(Domain, Domain) -> true;
match_domain(Domain, Pattern) ->
    DLabels = str:tokens(Domain, <<".">>),
    PLabels = str:tokens(Pattern, <<".">>),
    match_labels(DLabels, PLabels).

match_labels([], []) -> true;
match_labels([], [_ | _]) -> false;
match_labels([_ | _], []) -> false;
match_labels([DL | DLabels], [PL | PLabels]) ->
    case lists:all(fun (C) ->
			   $a =< C andalso C =< $z orelse
			     $0 =< C andalso C =< $9 orelse
			       C == $- orelse C == $*
		   end,
		   binary_to_list(PL))
	of
      true ->
	  Regexp = ejabberd_regexp:sh_to_awk(PL),
	  case ejabberd_regexp:run(DL, Regexp) of
	    match -> match_labels(DLabels, PLabels);
	    nomatch -> false
	  end;
      false -> false
    end.
