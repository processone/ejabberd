%%%----------------------------------------------------------------------
%%% File    : mod_ping.erl
%%% Author  : Brian Cully <bjc@kublai.com>
%%% Purpose : Support XEP-0199 XMPP Ping and periodic keepalives
%%% Created : 11 Jul 2009 by Brian Cully <bjc@kublai.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
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

-behaviour(gen_mod).

-behaviour(gen_server).

-include("logger.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-include("translate.hrl").

%% API
-export([start_ping/2, stop_ping/2]).

%% gen_mod callbacks
-export([start/2, stop/1, reload/3]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).

-export([iq_ping/1, user_online/3, user_offline/3, mod_doc/0,
	 user_send/1, mod_opt_type/1, mod_options/1, depends/2]).

-record(state,
	{host                :: binary(),
         send_pings          :: boolean(),
	 ping_interval       :: pos_integer(),
	 ping_ack_timeout    :: undefined | non_neg_integer(),
	 timeout_action      :: none | kill,
         timers              :: timers()}).

-type timers() :: #{ljid() => reference()}.

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
init([Host|_]) ->
    process_flag(trap_exit, true),
    Opts = gen_mod:get_module_opts(Host, ?MODULE),
    State = init_state(Host, Opts),
    register_iq_handlers(Host),
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
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast({reload, Host, NewOpts, _OldOpts},
	    #state{timers = Timers} = OldState) ->
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
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({iq_reply, #iq{type = error} = IQ, JID}, State) ->
    Timers = case xmpp:get_error(IQ) of
		 #stanza_error{type=cancel, reason='service-unavailable'} ->
		     del_timer(JID, State#state.timers);
		 _ ->
		     State#state.timers
	     end,
    {noreply, State#state{timers = Timers}};
handle_info({iq_reply, #iq{}, _JID}, State) ->
    {noreply, State};
handle_info({iq_reply, timeout, JID}, State) ->
    ejabberd_hooks:run(user_ping_timeout, State#state.host,
		       [JID]),
    Timers = case State#state.timeout_action of
		 kill ->
		     #jid{user = User, server = Server,
			  resource = Resource} =
			 JID,
		     case ejabberd_sm:get_session_pid(User, Server, Resource) of
			 Pid when is_pid(Pid) ->
			     ejabberd_c2s:close(Pid, ping_timeout);
			 _ ->
			     ok
		     end,
		     del_timer(JID, State#state.timers);
		 _ ->
		     State#state.timers
	     end,
    {noreply, State#state{timers = Timers}};
handle_info({timeout, _TRef, {ping, JID}}, State) ->
    Timers = case ejabberd_sm:get_session_pid(JID#jid.luser,
					      JID#jid.lserver,
					      JID#jid.lresource) of
		 none ->
		     del_timer(JID, State#state.timers);
		 _ ->
		     Host = State#state.host,
		     From = jid:make(Host),
		     IQ = #iq{from = From, to = JID, type = get, sub_els = [#ping{}]},
		     ejabberd_router:route_iq(IQ, JID,
					      gen_mod:get_module_proc(Host, ?MODULE),
					      State#state.ping_ack_timeout),
		     add_timer(JID, State#state.ping_interval,
			       State#state.timers)
	     end,
    {noreply, State#state{timers = Timers}};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% Hook callbacks
%%====================================================================
-spec iq_ping(iq()) -> iq().
iq_ping(#iq{type = get, sub_els = [#ping{}]} = IQ) ->
    xmpp:make_iq_result(IQ);
iq_ping(#iq{lang = Lang} = IQ) ->
    Txt = ?T("Ping query is incorrect"),
    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang)).

-spec user_online(ejabberd_sm:sid(), jid(), ejabberd_sm:info()) -> ok.
user_online(_SID, JID, _Info) ->
    start_ping(JID#jid.lserver, JID).

-spec user_offline(ejabberd_sm:sid(), jid(), ejabberd_sm:info()) -> ok.
user_offline(_SID, JID, _Info) ->
    case ejabberd_sm:get_session_pid(JID#jid.luser,
                                     JID#jid.lserver,
                                     JID#jid.lresource) of
        PID when PID =:= none; node(PID) /= node() ->
            stop_ping(JID#jid.lserver, JID);
        _ ->
            ok
    end.

-spec user_send({stanza(), ejabberd_c2s:state()}) -> {stanza(), ejabberd_c2s:state()}.
user_send({Packet, #{jid := JID} = C2SState}) ->
    start_ping(JID#jid.lserver, JID),
    {Packet, C2SState}.

%%====================================================================
%% Internal functions
%%====================================================================
init_state(Host, Opts) ->
    SendPings = mod_ping_opt:send_pings(Opts),
    PingInterval = mod_ping_opt:ping_interval(Opts),
    PingAckTimeout = mod_ping_opt:ping_ack_timeout(Opts),
    TimeoutAction = mod_ping_opt:timeout_action(Opts),
    #state{host = Host,
	   send_pings = SendPings,
	   ping_interval = PingInterval,
	   timeout_action = TimeoutAction,
	   ping_ack_timeout = PingAckTimeout,
	   timers = #{}}.

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

register_iq_handlers(Host) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PING,
				  ?MODULE, iq_ping),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_PING,
				  ?MODULE, iq_ping).

unregister_iq_handlers(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_PING),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PING).

-spec add_timer(jid(), pos_integer(), timers()) -> timers().
add_timer(JID, Interval, Timers) ->
    LJID = jid:tolower(JID),
    NewTimers = case maps:find(LJID, Timers) of
		    {ok, OldTRef} ->
			misc:cancel_timer(OldTRef),
			maps:remove(LJID, Timers);
		    _ -> Timers
		end,
    TRef = erlang:start_timer(Interval, self(), {ping, JID}),
    maps:put(LJID, TRef, NewTimers).

-spec del_timer(jid(), timers()) -> timers().
del_timer(JID, Timers) ->
    LJID = jid:tolower(JID),
    case maps:find(LJID, Timers) of
	{ok, TRef} ->
	    misc:cancel_timer(TRef),
	    maps:remove(LJID, Timers);
	_ -> Timers
    end.

depends(_Host, _Opts) ->
    [].

mod_opt_type(ping_interval) ->
    econf:timeout(second);
mod_opt_type(ping_ack_timeout) ->
    econf:timeout(second);
mod_opt_type(send_pings) ->
    econf:bool();
mod_opt_type(timeout_action) ->
    econf:enum([none, kill]).

mod_options(_Host) ->
    [{ping_interval, timer:minutes(1)},
     {ping_ack_timeout, undefined},
     {send_pings, false},
     {timeout_action, none}].

mod_doc() ->
    #{desc =>
          ?T("This module implements support for "
             "https://xmpp.org/extensions/xep-0199.html"
             "[XEP-0199: XMPP Ping] and periodic keepalives. "
             "When this module is enabled ejabberd responds "
             "correctly to ping requests, as defined by the protocol."),
      opts =>
          [{ping_interval,
            #{value => "timeout()",
              desc =>
                  ?T("How often to send pings to connected clients, "
                     "if option 'send_pings' is set to 'true'. If a client "
                     "connection does not send or receive any stanza "
                     "within this interval, a ping request is sent to "
                     "the client. The default value is '1' minute.")}},
           {ping_ack_timeout,
            #{value => "timeout()",
              desc =>
                  ?T("How long to wait before deeming that a client "
                     "has not answered a given server ping request. "
                     "The default value is 'undefined'.")}},
           {send_pings,
            #{value => "true | false",
              desc =>
                  ?T("If this option is set to 'true', the server "
                     "sends pings to connected clients that are not "
                     "active in a given interval defined in 'ping_interval' "
                     "option. This is useful to keep client connections "
                     "alive or checking availability. "
                     "The default value is 'false'.")}},
           {timeout_action,
            #{value => "none | kill",
              desc =>
                  ?T("What to do when a client does not answer to a "
                     "server ping request in less than period defined "
                     "in 'ping_ack_timeout' option: "
                     "'kill' means destroying the underlying connection, "
                     "'none' means to do nothing. NOTE: when _`mod_stream_mgmt`_ "
                     "is loaded and stream management is enabled by "
                     "a client, killing the client connection doesn't mean "
                     "killing the client session - the session will be kept "
                     "alive in order to give the client a chance to resume it. "
                     "The default value is 'none'.")}}],
      example =>
          ["modules:",
           "  ...",
           "  mod_ping:",
           "    send_pings: true",
           "    ping_interval: 4 min",
           "    timeout_action: kill",
           "  ..."]}.
