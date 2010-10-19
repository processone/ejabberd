%% Usage:
%% In config file:
%% {mod_c2s_debug, [{logdir, "/tmp/xmpplogs"}]},
%% From Erlang shell:
%% mod_c2s_debug:start("localhost", []).
%% mod_c2s_debug:stop("localhost").
%%
%% Warning: Only one module for the debug handler can be defined.
-module(mod_c2s_debug).
-author('mremond@process-one.net').

-behaviour(gen_mod).
-behavior(gen_server).

-export([start/2, start_link/2, stop/1,
	 debug_start/3, debug_stop/2, log_packet/4, log_packet/5]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_c2s.hrl").

-record(modstate, {host, logdir, pid, iodevice}).
-record(clientinfo, {pid, jid, auth_module, ip}).

-define(SUPERVISOR, ejabberd_sup).
-define(PROCNAME, c2s_debug).

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    Spec = {Proc, {?MODULE, start_link, [Host, Opts]},
	    transient, 2000, worker, [?MODULE]},
    supervisor:start_child(?SUPERVISOR, Spec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:delete_child(?SUPERVISOR, Proc).

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

%%====================================================================
%% Hooks
%%====================================================================

%% Debug handled by another module... Do nothing:
debug_start(_Status, Pid, C2SState) ->
    Host = C2SState#state.server,
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),

    JID = jlib:jid_to_string(C2SState#state.jid),
    AuthModule = C2SState#state.auth_module,
    IP = C2SState#state.ip,
    ClientInfo = #clientinfo{pid = Pid, jid = JID, auth_module = AuthModule, ip = IP},

    gen_server:call(Proc, {debug_start, ClientInfo}).

debug_stop(Pid, C2SState) ->
    Host = C2SState#state.server,
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:cast(Proc, {debug_stop, Pid}).

log_packet(false, _FromJID, _ToJID, _Packet) ->
    ok;
log_packet(true, FromJID, ToJID, Packet) ->
    Host = FromJID#jid.lserver,
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:cast(Proc, {addlog, {"Send", FromJID, ToJID, Packet}}).
log_packet(false, _JID, _FromJID, _ToJID, _Packet) ->
    ok;
log_packet(true, JID, FromJID, ToJID, Packet) ->
    Host = JID#jid.lserver,
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:cast(Proc, {addlog, {"Receive", FromJID, ToJID, Packet}}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    ?INFO_MSG("Starting c2s debug module for: ~p", [Host]),
    MyHost = gen_mod:get_opt_host(Host, Opts, "c2s_debug.@HOST@"),
    ejabberd_hooks:add(c2s_debug_start_hook, Host,
		       ?MODULE, debug_start, 50),
    ejabberd_hooks:add(c2s_debug_stop_hook, Host,
		       ?MODULE, debug_stop, 50),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, log_packet, 50),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, log_packet, 50),

    Logdir = gen_mod:get_opt(logdir, Opts, "/tmp/xmpplogs/"),
    make_dir_rec(Logdir),
    {ok, #modstate{host = MyHost, logdir = Logdir}}.

terminate(_Reason, #modstate{host = Host}) ->
    ?INFO_MSG("Stopping c2s debug module for: ~s", [Host]),
    ejabberd_hooks:delete(c2s_debug_start_hook, Host,
			  ?MODULE, debug_start, 50),
    ejabberd_hooks:delete(c2s_debug_stop_hook, Host,
			  ?MODULE, debug_stop, 50),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, log_packet, 50).

handle_call({debug_start, ClientInfo}, _From, #modstate{pid=undefined} = State) ->
    Pid = ClientInfo#clientinfo.pid,
    ?INFO_MSG("Debug started for PID:~p", [Pid]),

    JID = ClientInfo#clientinfo.jid,
    AuthModule = ClientInfo#clientinfo.auth_module,
    IP = ClientInfo#clientinfo.ip,

    {ok, IOD} = file:open(filename(State#modstate.logdir), [append]),
    Line = io_lib:format("~s - Session open~nJID: ~s~nAuthModule: ~p~nIP: ~p~n",
			 [timestamp(), JID, AuthModule, IP]),
    file:write(IOD, Line),

    {reply, true, State#modstate{pid = Pid, iodevice = IOD}};
handle_call({debug_start, _ClientInfo}, _From, State) ->
    {reply, false, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast({addlog, _}, #modstate{iodevice=undefined} = State) ->
    {noreply, State};
handle_cast({addlog, {Direction, FromJID, ToJID, Packet}}, #modstate{iodevice=IOD} = State) ->
    LogEntry = io_lib:format("=====~n~s - ~s~nFrom: ~s~nTo: ~s~n~s~n", [timestamp(), Direction,
									jlib:jid_to_string(FromJID),
									jlib:jid_to_string(ToJID),
									xml:element_to_string(Packet)]),
    file:write(IOD, LogEntry),
    {noreply, State};
handle_cast({debug_stop, Pid}, #modstate{pid=Pid, iodevice=IOD} = State) ->
    Line = io_lib:format("=====~n~s - Session closed~n",
			 [timestamp()]),
    file:write(IOD, Line),

    file:close(IOD),
    {noreply, State#modstate{pid = undefined, iodevice=undefined}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Generate filename
filename(LogDir) ->
    Filename = lists:flatten(timestamp()) ++ "-c2s.log",
    filename:join([LogDir, Filename]).

%% Generate timestamp
timestamp() ->
    {Y,Mo,D} = erlang:date(),
    {H,Mi,S} = erlang:time(),
    io_lib:format("~4.4.0w~2.2.0w~2.2.0w-~2.2.0w~2.2.0w~2.2.0w", [Y,Mo,D,H,Mi,S]).

%% Create dir recusively
make_dir_rec(Dir) ->
    case file:read_file_info(Dir) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            DirS = filename:split(Dir),
            DirR = lists:sublist(DirS, length(DirS)-1),
            make_dir_rec(filename:join(DirR)),
            file:make_dir(Dir)
    end.
