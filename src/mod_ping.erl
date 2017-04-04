%%%----------------------------------------------------------------------
%%% File    : mod_ping.erl
%%% Author  : Brian Cully <bjc@kublai.com>
%%% Purpose : Support XEP-0199 XMPP Ping and periodic keepalives
%%% Created : 11 Jul 2009 by Brian Cully <bjc@kublai.com>
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

-module(mod_ping).

-author('bjc@kublai.com').

-protocol({xep, 199, '2.0'}).

-behavior(gen_mod).

-behavior(gen_server).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

-define(DEFAULT_SEND_PINGS, false).

-define(DEFAULT_PING_INTERVAL, 60).

%% API
-export([start_ping/2, stop_ping/2]).

%% gen_mod callbacks
-export([start/2, stop/1, reload/3]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).

-export([iq_ping/1, user_online/3, user_offline/3,
	 user_send/1, mod_opt_type/1, depends/2]).

-record(state,
	{host = <<"">>,
         send_pings = ?DEFAULT_SEND_PINGS :: boolean(),
	 ping_interval = ?DEFAULT_PING_INTERVAL :: non_neg_integer(),
	 ping_ack_timeout = undefined :: non_neg_integer(),
	 timeout_action = none :: none | kill,
         timers = maps:new() :: map()}).

%%====================================================================
%% API
%%====================================================================
-spec start_ping(binary(), jid()) -> ok.
start_ping(Host, JID) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {start_ping, JID}).

-spec stop_ping(binary(), jid()) -> ok.
stop_ping(Host, JID) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {stop_ping, JID}).

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) ->
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    gen_mod:stop_child(?MODULE, Host).

reload(Host, NewOpts, OldOpts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {reload, Host, NewOpts, OldOpts}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    process_flag(trap_exit, true),
    State = init_state(Host, Opts),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             no_queue),
    register_iq_handlers(Host, IQDisc),
    case State#state.send_pings of
	true -> register_hooks(Host);
	false -> ok
    end,
    {ok, State}.

terminate(_Reason, #state{host = Host}) ->
    unregister_hooks(Host),
    unregister_iq_handlers(Host).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast({reload, Host, NewOpts, OldOpts},
	    #state{timers = Timers} = OldState) ->
    case gen_mod:is_equal_opt(iqdisc, NewOpts, OldOpts,
			      fun gen_iq_handler:check_type/1,
			      one_queue) of
	{false, IQDisc, _} -> register_iq_handlers(Host, IQDisc);
	true -> ok
    end,
    NewState = init_state(Host, NewOpts),
    case {NewState#state.send_pings, OldState#state.send_pings} of
	{true, false} -> register_hooks(Host);
	{false, true} -> unregister_hooks(Host);
	_ -> ok
    end,
    {noreply, NewState#state{timers = Timers}};
handle_cast({start_ping, JID}, State) ->
    Timers = add_timer(JID, State#state.ping_interval,
		       State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_cast({stop_ping, JID}, State) ->
    Timers = del_timer(JID, State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_cast({iq_pong, JID, timeout}, State) ->
    Timers = del_timer(JID, State#state.timers),
    ejabberd_hooks:run(user_ping_timeout, State#state.host,
		       [JID]),
    case State#state.timeout_action of
      kill ->
	  #jid{user = User, server = Server,
	       resource = Resource} =
	      JID,
	  case ejabberd_sm:get_session_pid(User, Server, Resource)
	      of
	    Pid when is_pid(Pid) -> ejabberd_c2s:close(Pid, _SendTrailer = false);
	    _ -> ok
	  end;
      _ -> ok
    end,
    {noreply, State#state{timers = Timers}};
handle_cast({iq_pong, _JID, _}, State) ->
    {noreply, State};
handle_cast(Msg, State) ->
    ?WARNING_MSG("unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({timeout, _TRef, {ping, JID}}, State) ->
    From = jid:make(State#state.host),
    IQ = #iq{from = From, to = JID, type = get, sub_els = [#ping{}]},
    Pid = self(),
    F = fun (Response) ->
		gen_server:cast(Pid, {iq_pong, JID, Response})
	end,
    ejabberd_local:route_iq(IQ, F, State#state.ping_ack_timeout),
    Timers = add_timer(JID, State#state.ping_interval,
		       State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% Hook callbacks
%%====================================================================
-spec iq_ping(iq()) -> iq().
iq_ping(#iq{type = get, sub_els = [#ping{}]} = IQ) ->
    xmpp:make_iq_result(IQ);
iq_ping(#iq{lang = Lang} = IQ) ->
    Txt = <<"Ping query is incorrect">>,
    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang)).

-spec user_online(ejabberd_sm:sid(), jid(), ejabberd_sm:info()) -> ok.
user_online(_SID, JID, _Info) ->
    start_ping(JID#jid.lserver, JID).

-spec user_offline(ejabberd_sm:sid(), jid(), ejabberd_sm:info()) -> ok.
user_offline(_SID, JID, _Info) ->
    stop_ping(JID#jid.lserver, JID).

-spec user_send({stanza(), ejabberd_c2s:state()}) -> {stanza(), ejabberd_c2s:state()}.
user_send({Packet, #{jid := JID} = C2SState}) ->
    start_ping(JID#jid.lserver, JID),
    {Packet, C2SState}.

%%====================================================================
%% Internal functions
%%====================================================================
init_state(Host, Opts) ->
    SendPings = gen_mod:get_opt(send_pings, Opts,
                                fun(B) when is_boolean(B) -> B end,
				?DEFAULT_SEND_PINGS),
    PingInterval = gen_mod:get_opt(ping_interval, Opts,
                                   fun(I) when is_integer(I), I>0 -> I end,
				   ?DEFAULT_PING_INTERVAL),
    PingAckTimeout = gen_mod:get_opt(ping_ack_timeout, Opts,
                                     fun(I) when is_integer(I), I>0 -> I * 1000 end,
                                     undefined),
    TimeoutAction = gen_mod:get_opt(timeout_action, Opts,
                                    fun(none) -> none;
                                       (kill) -> kill
                                    end, none),
    #state{host = Host,
	   send_pings = SendPings,
	   ping_interval = PingInterval,
	   timeout_action = TimeoutAction,
	   ping_ack_timeout = PingAckTimeout,
	   timers = maps:new()}.

register_hooks(Host) ->
    ejabberd_hooks:add(sm_register_connection_hook, Host,
		       ?MODULE, user_online, 100),
    ejabberd_hooks:add(sm_remove_connection_hook, Host,
		       ?MODULE, user_offline, 100),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       user_send, 100).

unregister_hooks(Host) ->
    ejabberd_hooks:delete(sm_remove_connection_hook, Host,
			  ?MODULE, user_offline, 100),
    ejabberd_hooks:delete(sm_register_connection_hook, Host,
			  ?MODULE, user_online, 100),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  user_send, 100).

register_iq_handlers(Host, IQDisc) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PING,
				  ?MODULE, iq_ping, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_PING,
				  ?MODULE, iq_ping, IQDisc).

unregister_iq_handlers(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_PING),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PING).

-spec add_timer(jid(), non_neg_integer(), map()) -> map().
add_timer(JID, Interval, Timers) ->
    LJID = jid:tolower(JID),
    NewTimers = case maps:find(LJID, Timers) of
      {ok, OldTRef} ->
		      cancel_timer(OldTRef),
          maps:remove(LJID, Timers);
      _ -> Timers
		end,
    TRef = erlang:start_timer(Interval * 1000, self(),
			      {ping, JID}),
    maps:put(LJID, TRef, NewTimers).

-spec del_timer(jid(), map()) -> map().
del_timer(JID, Timers) ->
    LJID = jid:tolower(JID),
    case maps:find(LJID, Timers) of
      {ok, TRef} ->
	  cancel_timer(TRef),
    maps:remove(LJID, Timers);
      _ -> Timers
    end.

-spec cancel_timer(reference()) -> ok.
cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
      false ->
	  receive {timeout, TRef, _} -> ok after 0 -> ok end;
      _ -> ok
    end.

depends(_Host, _Opts) ->
    [].

mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(ping_interval) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(ping_ack_timeout) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(send_pings) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(timeout_action) ->
    fun (none) -> none;
	(kill) -> kill
    end;
mod_opt_type(_) ->
    [iqdisc, ping_interval, ping_ack_timeout, send_pings, timeout_action].
