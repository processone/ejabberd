%%%----------------------------------------------------------------------
%%% File    : mod_ping.erl
%%% Author  : Brian Cully <bjc@kublai.com>
%%% Purpose : Support XEP-0199 XMPP Ping and periodic keepalives
%%% Created : 11 Jul 2009 by Brian Cully <bjc@kublai.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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

-module(mod_ping).
-author('bjc@kublai.com').

-behavior(gen_mod).
-behavior(gen_server).

-include("ejabberd.hrl").
-include_lib("exmpp/include/exmpp.hrl").

-define(SUPERVISOR, ejabberd_sup).
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

-record(state, {host, % binary() | global
                send_pings = ?DEFAULT_SEND_PINGS,
                ping_interval = ?DEFAULT_PING_INTERVAL,
		timeout_action = none,
                timers = ?DICT:new()}).

%%====================================================================
%% API
%%====================================================================
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start_ping(Host, JID) ->
    Proc = gen_mod:get_module_proc_existing(Host, ?MODULE),
    gen_server:cast(Proc, {start_ping, JID}).

stop_ping(Host, JID) ->
    Proc = gen_mod:get_module_proc_existing(Host, ?MODULE),
    gen_server:cast(Proc, {stop_ping, JID}).

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
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
init([HostB, Opts]) ->
    SendPings = gen_mod:get_opt(send_pings, Opts, ?DEFAULT_SEND_PINGS),
    PingInterval = gen_mod:get_opt(ping_interval, Opts, ?DEFAULT_PING_INTERVAL),
    TimeoutAction = gen_mod:get_opt(timeout_action, Opts, none),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, no_queue),
    mod_disco:register_feature(HostB, ?NS_PING),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_PING,
                                  ?MODULE, iq_ping, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_PING,
                                  ?MODULE, iq_ping, IQDisc),
    case SendPings of
        true ->
	    %% Ping requests are sent to all entities, whether they
	    %% announce 'urn:xmpp:ping' in their caps or not
            ejabberd_hooks:add(sm_register_connection_hook, HostB,
                               ?MODULE, user_online, 100),
            ejabberd_hooks:add(sm_remove_connection_hook, HostB,
                               ?MODULE, user_offline, 100),
	    ejabberd_hooks:add(user_send_packet, HostB,
			       ?MODULE, user_send, 100);
        _ ->
            ok
    end,
    {ok, #state{host = HostB,
                send_pings = SendPings,
                ping_interval = PingInterval,
		timeout_action = TimeoutAction,
                timers = ?DICT:new()}}.

terminate(_Reason, #state{host = HostB}) ->
    ejabberd_hooks:delete(sm_remove_connection_hook, HostB,
			  ?MODULE, user_offline, 100),
    ejabberd_hooks:delete(sm_register_connection_hook, HostB,
			  ?MODULE, user_online, 100),
    ejabberd_hooks:delete(user_send_packet, HostB,
			  ?MODULE, user_send, 100),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB, ?NS_PING),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_PING),
    mod_disco:unregister_feature(HostB, ?NS_PING).

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
    Host = exmpp_jid:domain(JID),
    ejabberd_hooks:run(user_ping_timeout, Host, [JID]),
    case State#state.timeout_action of
	kill ->
	    case ejabberd_sm:get_session_pid(JID) of
		Pid when is_pid(Pid) ->
		    ejabberd_c2s:stop(Pid);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end,
    {noreply, State#state{timers = Timers}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _TRef, {ping, JID}}, State) ->
    %%IQ = #iq{type = get,
    %%         sub_el = [{xmlelement, "ping", [{"xmlns", ?NS_PING}], []}]},

    %% Build an iq:query request
    %%IQ = exmpp_iq:get(?NS_PING, #xmlel{ns = ?NS_PING, name = 'ping'}),
    IQ = #iq{type = get, payload = #xmlel{name = 'ping', ns = ?NS_PING}},

    Pid = self(),
    F = fun(Response) ->
		gen_server:cast(Pid, {iq_pong, JID, Response})
	end,
    From = exmpp_jid:make(exmpp_jid:domain(JID)),
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
iq_ping(_From, _To, #iq{type = Type, payload = SubEl} = IQ) ->
    case {Type, SubEl} of
        {get, #xmlel{name = ping, ns = ?NS_PING}} ->
            exmpp_iq:result(IQ);
        _ ->
	    exmpp_iq:error(IQ, 'feature-not-implemented')
    end.

user_online(_SID, JID, _Info) ->
    Host = exmpp_jid:prep_domain_as_list(JID),
    start_ping(Host, JID).

user_offline(_SID, JID, _Info) ->
    Host = exmpp_jid:prep_domain_as_list(JID),
    start_ping(Host, JID).

user_send(JID, _From, _Packet) ->
    Host = exmpp_jid:prep_domain_as_list(JID),
    start_ping(Host, JID).

%%====================================================================
%% Internal functions
%%====================================================================
add_timer(JID, Interval, Timers) ->
    LJID = exmpp_jid:prep_to_binary(JID),
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
    LJID = exmpp_jid:prep_to_binary(JID),
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
