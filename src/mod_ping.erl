%%%-------------------------------------------------------------------
%%% @doc Implements support for XEP-0199 (XMPP Ping) and periodic
%%% keepalives.
%%%
%%% <p>When enabled (see below), ejabberd will respond correctly to
%%% ping packets, as defined in XEP-0199.</p>
%%%
%%% <p>In addition you can have the server generate pings to clients
%%% as a method of keeping them alive or checking
%%% availibility. However, this feature is disabled by default since
%%% it is mostly not needed and consumes resources. For "interesting"
%%% uses it can be enabled in the config (see below).</p>
%%%
%%% <p>To use this module simply include it in the modules section of
%%% your ejabberd config.</p>
%%%
%%% <p>Configuration options:</p>
%%% <dl>
%%%   <dt>{send_pings, true | false}</dt>
%%%   <dd>Whether to send pings to connected clients.</dd>
%%%   <dt>{ping_interval, Seconds}</dt>
%%%   <dd>How often to send pings to connected clients.</dd>
%%% </dl>
%%%
%%% @reference <a
%%% href="http://xmpp.org/extensions/xep-0199.html">XEP-0199</a>
%%% @end
%%% -------------------------------------------------------------------
-module(mod_ping).
-author('bjc@kublai.com').

-behavior(gen_mod).
-behavior(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(SUPERVISOR, ejabberd_sup).
-define(NS_PING, "urn:xmpp:ping").
-define(DEFAULT_SEND_PINGS, false). % bool()
-define(DEFAULT_PING_INTERVAL, 60). % seconds

-define(DICT, dict).

%% API
-export([start_link/2, start_ping/2, stop_ping/2]).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3]).

%% Hook callbacks
-export([iq_ping/3, user_online/3, user_offline/3, user_send/3]).

-record(state, {host = "",
                send_pings = ?DEFAULT_SEND_PINGS,
                ping_interval = ?DEFAULT_PING_INTERVAL,
                timers = ?DICT:new()}).

%%====================================================================
%% API
%%====================================================================
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start_ping(Host, JID) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {start_ping, JID}).

stop_ping(Host, JID) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {stop_ping, JID}).

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    PingSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
                transient, 2000, worker, [?MODULE]},
    supervisor:start_child(?SUPERVISOR, PingSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    supervisor:delete_child(?SUPERVISOR, Proc).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    SendPings = gen_mod:get_opt(send_pings, Opts, ?DEFAULT_SEND_PINGS),
    PingInterval = gen_mod:get_opt(ping_interval, Opts, ?DEFAULT_PING_INTERVAL),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mod_disco:register_feature(Host, ?NS_PING),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PING,
                                  ?MODULE, iq_ping, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_PING,
                                  ?MODULE, iq_ping, IQDisc),
    case SendPings of
        true ->
            ejabberd_hooks:add(sm_register_connection_hook, Host,
                               ?MODULE, user_online, 100),
            ejabberd_hooks:add(sm_remove_connection_hook, Host,
                               ?MODULE, user_offline, 100),
	    ejabberd_hooks:add(user_send_packet, Host,
			       ?MODULE, user_send, 100);
        _ ->
            ok
    end,
    {ok, #state{host = Host,
                send_pings = SendPings,
                ping_interval = PingInterval,
                timers = ?DICT:new()}}.

terminate(_Reason, #state{host = Host}) ->
    ejabberd_hooks:delete(sm_remove_connection_hook, Host,
			  ?MODULE, user_offline, 100),
    ejabberd_hooks:delete(sm_register_connection_hook, Host,
			  ?MODULE, user_online, 100),
    ejabberd_hooks:delete(user_send_packet, Host,
			  ?MODULE, user_send, 100),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_PING),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PING),
    mod_disco:unregister_feature(Host, ?NS_PING).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast({start_ping, JID}, State) ->
    Timers = add_timer(JID, State#state.ping_interval, State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_cast({stop_ping, JID}, State) ->
    Timers = del_timer(JID, State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_cast({iq_pong, JID, timeout}, State) ->
    Timers = del_timer(JID, State#state.timers),
    ejabberd_hooks:run(user_ping_timeout, State#state.host, [JID]),
    {noreply, State#state{timers = Timers}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _TRef, {ping, JID}}, State) ->
    IQ = #iq{type = get,
             sub_el = [{xmlelement, "ping", [{"xmlns", ?NS_PING}], []}]},
    Pid = self(),
    F = fun(Response) ->
		gen_server:cast(Pid, {iq_pong, JID, Response})
	end,
    From = jlib:make_jid("", State#state.host, ""),
    ejabberd_local:route_iq(From, JID, IQ, F),
    Timers = add_timer(JID, State#state.ping_interval, State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Hook callbacks
%%====================================================================
iq_ping(_From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case {Type, SubEl} of
        {get, {xmlelement, "ping", _, _}} ->
            IQ#iq{type = result, sub_el = []};
        _ ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.

user_online(_SID, JID, _Info) ->
    start_ping(JID#jid.lserver, JID).

user_offline(_SID, JID, _Info) ->
    stop_ping(JID#jid.lserver, JID).

user_send(JID, _From, _Packet) ->
    start_ping(JID#jid.lserver, JID).

%%====================================================================
%% Internal functions
%%====================================================================
add_timer(JID, Interval, Timers) ->
    LJID = jlib:jid_tolower(JID),
    NewTimers = case ?DICT:find(LJID, Timers) of
		    {ok, OldTRef} ->
			cancel_timer(OldTRef),
			?DICT:erase(LJID, Timers);
		    _ ->
			Timers
		end,
    TRef = erlang:start_timer(Interval * 1000, self(), {ping, JID}),
    ?DICT:store(LJID, TRef, NewTimers).

del_timer(JID, Timers) ->
    LJID = jlib:jid_tolower(JID),
    case ?DICT:find(LJID, Timers) of
        {ok, TRef} ->
	    cancel_timer(TRef),
	    ?DICT:erase(LJID, Timers);
        _ ->
	    Timers
    end.

cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
	false ->
	    receive
                {timeout, TRef, _} ->
                    ok
            after 0 ->
                    ok
            end;
        _ ->
            ok
    end.
