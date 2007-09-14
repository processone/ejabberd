%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : S2S connections manager
%%% Created :  7 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id: ejabberd_s2s.erl 820 2007-07-19 21:17:13Z mremond $
%%%----------------------------------------------------------------------

-module(ejabberd_s2s).
-author('alexey@sevcom.net').
-vsn('$Revision: 820 $ ').

-behaviour(gen_server).

%% API
-export([start_link/0,
	 route/3,
	 have_connection/1,
	 has_key/2,
	 try_register/1,
	 remove_connection/3,
	 dirty_get_connections/0,
	 allow_host/2,
	 ctl_process/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_ctl.hrl").

-define(DEFAULT_MAX_S2S_CONNEXIONS_NUMBER, 3).

-record(s2s, {fromto, pid, key}).
-record(state, {}).

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

remove_connection(FromTo, Pid, Key) ->
    ?ERROR_MSG("XXXXXXXXXXX ~p~n", [{FromTo, Pid, Key}]),
    case catch mnesia:dirty_match_object(s2s, {s2s, FromTo, Pid, '_'}) of
	[#s2s{pid = Pid, key = Key}] ->
	    F = fun() ->
			mnesia:delete_object(#s2s{fromto = FromTo,
						  pid = Pid,
						  key = Key})
		end,
	    mnesia:transaction(F);
	_ ->
	    ok
    end.

have_connection(FromTo) ->
    case catch mnesia:dirty_read(s2s, FromTo) of
	[_] ->
	    true;
	_ ->
	    false
    end.

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

try_register(FromTo) ->
    Key = randoms:get_string(),
    Max_S2S_Connexions_Number = max_s2s_connexions_number(element(1, FromTo)),
    F = fun() ->
		case mnesia:read({s2s, FromTo}) of
		    L when length(L) < Max_S2S_Connexions_Number ->
			   mnesia:write(#s2s{fromto = FromTo,
					     pid = self(),
					     key = Key}),
			   {key, Key};
		    _ ->
			false
		end
        end,
    case mnesia:transaction(F) of
	{atomic, Res} ->
	    Res;
	_ ->
	    false
    end.

dirty_get_connections() ->
    mnesia:dirty_all_keys(s2s).

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
    mnesia:add_table_index(s2s, key),
    mnesia:subscribe(system),
    ejabberd_ctl:register_commands(
      [{"incoming-s2s-number", "print number of incoming s2s connections on the node"},
       {"outgoing-s2s-number", "print number of outgoing s2s connections on the node"}],
      ?MODULE, ctl_process),
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
	_ ->
	    ok
    end,
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
    mnesia:transaction(F).

do_route(From, To, Packet) ->
    ?DEBUG("s2s manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
	   [From, To, Packet, 8]),
    case find_connection(From, To) of
	{atomic, Pid} when pid(Pid) ->
	    ?DEBUG("sending to process ~p~n", [Pid]),
	    {xmlelement, Name, Attrs, Els} = Packet,
	    NewAttrs = jlib:replace_from_to_attrs(jlib:jid_to_string(From),
						  jlib:jid_to_string(To),
						  Attrs),
	    send_element(Pid, {xmlelement, Name, NewAttrs, Els}),
	    ok;
	{aborted, _Reason} ->
	    case xml:get_tag_attr_s("type", Packet) of
		"error" -> ok;
		"result" -> ok;
		_ ->
		    Err = jlib:make_error_reply(
			    Packet, ?ERR_SERVICE_UNAVAILABLE),
		    ejabberd_router:route(To, From, Err)
	    end,
	    false
    end.

find_connection(From, To) ->
    #jid{lserver = MyServer} = From,
    #jid{lserver = Server} = To,
    FromTo = {MyServer, Server},
    Max_S2S_Connexions_Number = max_s2s_connexions_number(MyServer),
    ?ERROR_MSG("XXX Finding connection for ~p~n", [FromTo]),
    case catch mnesia:dirty_read(s2s, FromTo) of
	{'EXIT', Reason} ->
	    {aborted, Reason};
	[] ->
	    %% We try to establish connection if the host is not a
	    %% service and if the s2s host is not blacklisted or
	    %% is in whitelist:
	    case {is_service(From, To),
		  allow_host(MyServer, Server)} of
		{false, true} ->
		    new_connection(MyServer, Server, From, FromTo, Max_S2S_Connexions_Number);
		_ ->
		    {aborted, error}
	    end;
	L when is_list(L) , length(L) < Max_S2S_Connexions_Number ->
	    %% We establish another connection for this pair.
	    new_connection(MyServer, Server, From, FromTo, Max_S2S_Connexions_Number);
	L when is_list(L) ->
	    %% We choose a connexion from the pool of opened ones.
	    {atomic, choose_connection(From, L)}
    end.

choose_connection(From, Connections) ->
    % use sticky connections based on the full JID of the sender
    El = lists:nth(erlang:phash(From, length(Connections)), Connections),
    %El = lists:nth(random:uniform(length(Connections)), Connections),
    ?ERROR_MSG("XXX using ejabberd_s2s_out ~p~n", [El#s2s.pid]),
    El#s2s.pid.

new_connection(MyServer, Server, From, FromTo, Max_S2S_Connexions_Number) ->
    Key = randoms:get_string(),
    {ok, Pid} = ejabberd_s2s_out:start(
		  MyServer, Server, {new, Key}),
    F = fun() ->
		case mnesia:read({s2s, FromTo}) of
		    L when length(L) < Max_S2S_Connexions_Number ->
			mnesia:write(#s2s{fromto = FromTo,
					  pid = Pid,
					  key = Key}),
			?ERROR_MSG("XXX new s2s connection started ~p~n", [Pid]),
			Pid;
		    L ->
			choose_connection(From, L)
		end
	end,
    TRes = mnesia:transaction(F),
    case TRes of
	{atomic, Pid} ->
	    ejabberd_s2s_out:start_connection(Pid);
	_ ->
	    ejabberd_s2s_out:stop_connection(Pid)
    end,
    TRes.

max_s2s_connexions_number(Host) ->
    case ejabberd_config:get_local_option({max_s2s_connexions_number, Host}) of
	N when is_integer(N) ->
	    N;
	_ ->
	    ?DEFAULT_MAX_S2S_CONNEXIONS_NUMBER
    end.


%%--------------------------------------------------------------------
%% Function: is_service(From, To) -> true | false
%% Description: Return true if the destination must be considered as a
%% service.
%% --------------------------------------------------------------------
is_service(From, To) ->
    LFromDomain = From#jid.lserver,
    case ejabberd_config:get_local_option({route_subdomains, LFromDomain}) of
	s2s -> % bypass RFC 3920 10.3
	    false;
	_ ->
	    LDstDomain = To#jid.lserver,
	    P = fun(Domain) -> is_subdomain(LDstDomain, Domain) end,
	    lists:any(P, ?MYHOSTS)
    end.

%%--------------------------------------------------------------------
%% Function: is_subdomain(Domain1, Domain2) -> true | false
%% Description: Return true if Domain1 (a string representing an
%% internet domain name) is a subdomain (or the same domain) of
%% Domain2
%% --------------------------------------------------------------------
is_subdomain(Domain1, Domain2) ->
    lists:suffix(string:tokens(Domain2, "."), string:tokens(Domain1, ".")).

send_element(Pid, El) ->
    Pid ! {send_element, El}.

ctl_process(_Val, ["incoming-s2s-number"]) ->
    N = length(supervisor:which_children(ejabberd_s2s_in_sup)),
    io:format("~p~n", [N]),
    {stop, ?STATUS_SUCCESS};
ctl_process(_Val, ["outgoing-s2s-number"]) ->
    N = length(supervisor:which_children(ejabberd_s2s_out_sup)),
    io:format("~p~n", [N]),
    {stop, ?STATUS_SUCCESS};
ctl_process(Val, _Args) ->
    Val.

update_tables() ->
    case catch mnesia:table_info(s2s, type) of
	bag ->
	    ok;
	{'EXIT', _} ->
	    ok;
	_ ->
	    % XXX TODO convert it ?
	    mnesia:delete_table(s2s)
    end,
    case catch mnesia:table_info(s2s, attributes) of
	[fromto, node, key] ->
	    mnesia:transform_table(s2s, ignore, [fromto, pid, key]),
	    mnesia:clear_table(s2s);
	[fromto, pid, key] ->
	    ok;
	{'EXIT', _} ->
	    ok
    end,
    case lists:member(local_s2s, mnesia:system_info(tables)) of
	true ->
	    mnesia:delete_table(local_s2s);
	false ->
	    ok
    end.

%% Check if host is in blacklist or white list
allow_host(MyServer, S2SHost) ->
    case ejabberd_config:get_local_option({{s2s_host, S2SHost},MyServer}) of
	deny -> false;
	allow -> true;
	_ ->
	    case ejabberd_config:get_local_option({s2s_default_policy, MyServer}) of
		deny -> false;
		allow -> true;
		_ -> true %% The default s2s policy is allow
	    end
    end.
