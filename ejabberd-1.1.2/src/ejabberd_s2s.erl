%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : S2S connections manager
%%% Created :  7 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_s2s).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_server).

%% API
-export([start_link/0,
	 route/3,
	 have_connection/1,
	 get_key/1,
	 try_register/1,
	 remove_connection/1,
	 dirty_get_connections/0,
	 ctl_process/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_ctl.hrl").

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

remove_connection(FromTo) ->
    F = fun() ->
		mnesia:delete({s2s, FromTo})
	end,
    mnesia:transaction(F).

have_connection(FromTo) ->
    case catch mnesia:dirty_read(s2s, FromTo) of
	[_] ->
	    true;
	_ ->
	    false
    end.

get_key(FromTo) ->
    case catch mnesia:dirty_read(s2s, FromTo) of
	[E] ->
	    E#s2s.key;
	_ ->
	    error
    end.

try_register(FromTo) ->
    Key = randoms:get_string(),
    F = fun() ->
		case mnesia:read({s2s, FromTo}) of
		    [] ->
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
    mnesia:create_table(s2s, [{ram_copies, [node()]},
			      {attributes, record_info(fields, s2s)}]),
    mnesia:add_table_copy(s2s, node(), ram_copies),
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
	    % TODO
	    {xmlelement, Name, Attrs, Els} = Packet,
	    NewAttrs = jlib:replace_from_to_attrs(jlib:jid_to_string(From),
						  jlib:jid_to_string(To),
						  Attrs),
	    send_element(Pid, {xmlelement, Name, NewAttrs, Els}),
	    ok;
	{aborted, Reason} ->
	    ?DEBUG("delivery failed: ~p~n", [Reason]),
	    false
    end.

find_connection(From, To) ->
    #jid{lserver = MyServer} = From,
    #jid{lserver = Server} = To,
    FromTo = {MyServer, Server},
    case catch mnesia:dirty_read(s2s, FromTo) of
	{'EXIT', Reason} ->
	    {aborted, Reason};
	[] ->
	    ?DEBUG("starting new s2s connection~n", []),
	    Key = randoms:get_string(),
	    {ok, Pid} = ejabberd_s2s_out:start(MyServer, Server, {new, Key}),
	    F = fun() ->
			case mnesia:read({s2s, FromTo}) of
			    [El] ->
				El#s2s.pid;
			    [] ->
				mnesia:write(#s2s{fromto = FromTo,
						  pid = Pid,
						  key = Key}),
				Pid
			end
		end,
	    TRes = mnesia:transaction(F),
	    ejabberd_s2s_out:start_connection(Pid),
	    TRes;
	[El] ->
	    {atomic, El#s2s.pid}
    end.

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

