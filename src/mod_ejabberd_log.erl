%%%----------------------------------------------------------------------
%%% File    : mod_ejabberd_log.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Generate generic log file for analysis
%%% Created : 10 Nov 2012 by Mickael Remond <mremond@process-one.net>
%%%
%%% ejabberd, Copyright (C) 2002-2012   Process-one
%%%
%%% Starting module manually:
%%%  mod_admin_p1:restart_module("mod_ejabberd_log", "localhost").
%%% or
%%%  mod_admin_p1:restart_module("mod_ejabberd_log", "push.bbc.co.uk").
%%%
%%% Options: {dir, "/tmp/"}
%%%
%%% example module config:
%%%  {mod_ejabberd_log, [{dir, "/home/webadmin/xmpp-logs"}]},
%%%----------------------------------------------------------------------

-module(mod_ejabberd_log).
-author('mremond@process-one.net').

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start_link/2,
	 start/2,
	 stop/1,
	 on_connect/3,
	 on_disconnect/3,
	 reopen_log/1,
	 packet/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, {host,
		filename,
		dir,
		fd,
		day,
		disabled}).

-define(PROCNAME, ejabberd_mod_ejabberd_log).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
        {Proc,
	 {?MODULE, start_link, [Host, Opts]},
	 temporary,
	 1000,
	 worker,
	 [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:delete_child(ejabberd_sup, Proc).

%% SID is of form {Now, Pid}
%% Now being {A,B,C}
%% connect event is of the form:
%%  2012-11-10 15:51:18|DOMAIN|ERL_NODE|JID|CONN|User connect|IP
on_connect(_SID, JID, Info) ->
    IP = proplists:get_value(ip, Info),
    log(JID#jid.lserver, JID, conn, [formated_ip(IP)]).

%% SID is of form {Now, Pid}
%% Now being {A,B,C}
%% disconnect event is of the form:
%%  2012-11-10 15:52:18|DOMAIN|ERL_NODE|JID|DISC|User disconnect|IP|SESSION_DURATION
on_disconnect(_SID = {Now, _Pid}, JID, Info) ->
    IP = proplists:get_value(ip, Info),

    Timestamp1 = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(Now)),
    Timestamp2 = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(os:timestamp())),
    DurationInSecond = Timestamp2 - Timestamp1,

    log(JID#jid.lserver, JID, disc, [formated_ip(IP), integer_to_list(DurationInSecond)]).

reopen_log(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:cast(Proc, reopen).

packet(_DebugFlag, JID, To, #xmlel{name = <<"message">>, attrs = Attrs}) ->
    case xml:get_attr_s(<<"type">>, Attrs) of
	<<"normal">> -> log(JID#jid.lserver, JID, chat, [jlib:jid_to_string(To)]);
	<<"chat">> -> log(JID#jid.lserver, JID, chat, [jlib:jid_to_string(To)]);
	<<"groupchat">> -> log(JID#jid.lserver, JID, groupchat, [jlib:jid_to_string(To)]);
	_ -> ok
    end;
packet(_, _, _, _) -> ok.

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
init([Host, Opts]) ->
    Dir = gen_mod:get_opt(dir, Opts, fun(A) when is_list(A) -> A end, "/tmp/"),
    Day = day(),
    Filename = filename(Dir, Day),
    Disabled = gen_mod:get_opt(disabled, Opts, fun(A) when is_list(A) -> A end, []),
    {ok, FD} = file:open(Filename, [append, raw]),
    ejabberd_hooks:add(sm_register_connection_hook, Host,
		       ?MODULE, on_connect, 50),
    ejabberd_hooks:add(sm_remove_connection_hook, Host,
		       ?MODULE, on_disconnect, 50),
    ejabberd_hooks:add(reopen_log_hook, Host,
		       ?MODULE, reopen_log, 55),
    ejabberd_hooks:add(user_send_packet, Host,
		       ?MODULE, packet, 75),
    {ok, #state{host = Host,
		dir = Dir,
		filename = Filename,
		fd = FD,
		day = Day,
		disabled = Disabled}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({log, JID, Event, Specific}, State) ->
    case day() of
        %% Same day
        Day when Day == State#state.day ->
            do_log(State, JID, Event, Specific),
            {noreply, State};
        %% Another day, change log file
        NewDay ->
            Filename = filename(State#state.dir, NewDay),
            file:close(State#state.fd),
            {ok, FD} = file:open(Filename, [append, raw]),
            NewState = State#state{filename = Filename, fd = FD, day = NewDay},
            do_log(NewState, JID, Event, Specific),
            {noreply, NewState}
    end;
handle_cast(reopen, State) ->
    file:close(State#state.fd),
    {ok, FD} = file:open(State#state.filename, [append, raw]),
    {noreply, State#state{fd = FD}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    Host = State#state.host,
    ejabberd_hooks:delete(sm_register_connection_hook, Host,
			  ?MODULE, on_connect, 50),
    ejabberd_hooks:delete(sm_remove_connection_hook, Host,
			  ?MODULE, on_disconnect, 50),
    ejabberd_hooks:delete(reopen_log_hook, Host,
			  ?MODULE, reopen_log, 55),
    ejabberd_hooks:delete(user_send_packet, Host,
			  ?MODULE, packet, 75),
    file:close(State#state.fd),
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
log(Host, JID, Event, Specific) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:cast(Proc, {log, JID, Event, Specific}).

do_log(State, JID, Event, Specific) ->
    case lists:member(Event, State#state.disabled) of
        true ->
            ok;
        false ->
            TimeStamp = timestamp(),
            Host = State#state.host,
            User = jlib:jid_to_string(jlib:jid_remove_resource(
					jlib:jid_tolower(JID))),
            {EventID, EventName} = event(Event),
            Node = atom_to_list(erlang:node()),
            Mandatory = join(lists:map(
			       fun escape/1,
			       [TimeStamp, Host, Node, User, EventID, EventName]),
			     "|"),
            Optional = join(lists:map(fun escape/1, Specific), "|"),
            Log = [Mandatory, $|, Optional, $\n],
            file:write(State#state.fd, Log)
    end.

event(chat) -> {"MSG", "User send message"};
event(groupchat) -> {"MUC", "User send groupchat message"};
event(conn) -> {"CONN", "User connect"};
event(disc) -> {"DISC", "User disconnect"}.

day() ->
    {{Year,Month,Day},_} = calendar:now_to_datetime(os:timestamp()),
    lists:flatten(io_lib:format("~4w~.2.0w~.2.0w", [Year, Month, Day])).

filename(Dir, Day) ->
    filename:join(Dir, Day ++ "-xmpp.log").

escape(S) when is_binary(S) ->
    escape(binary_to_list(S));
escape(S) when is_list(S) ->
    [case C of
	 $\\ -> "\\\\";
	 $| -> "\\|";            %%% "
	 $\n -> "\\n";
	 _ -> C
     end || C <- S].

join(List, Sep) ->
    lists:foldr(fun(A, "") -> A;
		   (A, Acc) -> A ++ Sep ++ Acc
		end, "", List).

timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:now_to_local_time(now()),
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
		    [Year, Month, Day, Hour, Minute, Second])).

formated_ip(IP) ->
    case IP of
        {{I1,I2,I3,I4}, _} -> io_lib:format("~p.~p.~p.~p", [I1,I2,I3,I4]);
        {{0,0,0,0,0,0,I7,I8}, _} -> io_lib:format("::~p.~p.~p.~p", [I7 bsr 8, I7 band 255, I8 bsr 8, I8 band 255]);
        {{I1,I2,I3,I4,I5,I6,I7,I8}, _} -> io_lib:format("~.16b:~.16b:~.16b:~.16b:~.16b:~.16b:~.16b:~.16b", [I1,I2,I3,I4,I5,I6,I7,I8]);
        _ -> "unknown"
    end.
