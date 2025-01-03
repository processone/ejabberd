%%%-------------------------------------------------------------------
%%% Created : 16 Dec 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
%%%-------------------------------------------------------------------
-module(ejabberd_s2s_out).
-behaviour(xmpp_stream_out).

%% xmpp_stream_out callbacks
-export([tls_options/1, tls_required/1, tls_verify/1, tls_enabled/1,
	 connect_options/3, connect_timeout/1, address_families/1, default_port/1,
	 dns_retries/1, dns_timeout/1,
	 handle_auth_success/2, handle_auth_failure/3, handle_packet/2,
	 handle_stream_end/2, handle_stream_downgraded/2,
	 handle_recv/3, handle_send/3, handle_cdata/2,
	 handle_stream_established/1, handle_timeout/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
%% Hooks
-export([process_auth_result/2, process_closed/2, handle_unexpected_info/2,
	 handle_unexpected_cast/2, process_downgraded/2, handle_unauthenticated_features/2]).
%% API
-export([start/3, start_link/3, connect/1, close/1, close/2, stop_async/1, send/2,
	 route/2, establish/1, update_state/2, host_up/1, host_down/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("translate.hrl").

-type state() :: xmpp_stream_out:state().
-export_type([state/0]).

%%%===================================================================
%%% API
%%%===================================================================
start(From, To, Opts) ->
    case proplists:get_value(supervisor, Opts, true) of
	true ->
	    case supervisor:start_child(ejabberd_s2s_out_sup,
					[From, To, Opts]) of
		{ok, undefined} -> ignore;
		Res -> Res
	    end;
	_ ->
	    xmpp_stream_out:start(?MODULE, [From, To, Opts],
				  ejabberd_config:fsm_limit_opts([]))
    end.

start_link(From, To, Opts) ->
    xmpp_stream_out:start_link(?MODULE, [From, To, Opts],
			       ejabberd_config:fsm_limit_opts([])).

-spec connect(pid()) -> ok.
connect(Ref) ->
    xmpp_stream_out:connect(Ref).

-spec close(pid()) -> ok;
	   (state()) -> state().
close(Ref) ->
    xmpp_stream_out:close(Ref).

-spec close(pid(), atom()) -> ok.
close(Ref, Reason) ->
    xmpp_stream_out:close(Ref, Reason).

-spec stop_async(pid()) -> ok.
stop_async(Pid) ->
    xmpp_stream_out:stop_async(Pid).

-spec send(pid(), xmpp_element()) -> ok;
	  (state(), xmpp_element()) -> state().
send(Stream, Pkt) ->
    xmpp_stream_out:send(Stream, Pkt).

-spec route(pid(), xmpp_element()) -> ok.
route(Ref, Pkt) ->
    Ref ! {route, Pkt},
    ok.

-spec establish(state()) -> state().
establish(State) ->
    xmpp_stream_out:establish(State).

-spec update_state(pid(), fun((state()) -> state()) |
		   {module(), atom(), list()}) -> ok.
update_state(Ref, Callback) ->
    xmpp_stream_out:cast(Ref, {update_state, Callback}).

-spec host_up(binary()) -> ok.
host_up(Host) ->
    ejabberd_hooks:add(s2s_out_auth_result, Host, ?MODULE,
		       process_auth_result, 100),
    ejabberd_hooks:add(s2s_out_closed, Host, ?MODULE,
		       process_closed, 100),
    ejabberd_hooks:add(s2s_out_handle_info, Host, ?MODULE,
		       handle_unexpected_info, 100),
    ejabberd_hooks:add(s2s_out_handle_cast, Host, ?MODULE,
		       handle_unexpected_cast, 100),
    ejabberd_hooks:add(s2s_out_downgraded, Host, ?MODULE,
		       process_downgraded, 100).

-spec host_down(binary()) -> ok.
host_down(Host) ->
    ejabberd_hooks:delete(s2s_out_auth_result, Host, ?MODULE,
			  process_auth_result, 100),
    ejabberd_hooks:delete(s2s_out_closed, Host, ?MODULE,
			  process_closed, 100),
    ejabberd_hooks:delete(s2s_out_handle_info, Host, ?MODULE,
			  handle_unexpected_info, 100),
    ejabberd_hooks:delete(s2s_out_handle_cast, Host, ?MODULE,
			  handle_unexpected_cast, 100),
    ejabberd_hooks:delete(s2s_out_downgraded, Host, ?MODULE,
			  process_downgraded, 100).

%%%===================================================================
%%% Hooks
%%%===================================================================
process_auth_result(#{server := LServer, remote_server := RServer} = State,
		    {false, Reason}) ->
    Delay = get_delay(),
    ?WARNING_MSG("Failed to establish outbound s2s connection ~ts -> ~ts: "
		 "authentication failed; bouncing for ~p seconds",
		 [LServer, RServer, Delay div 1000]),
    State1 = State#{on_route => bounce, stop_reason => Reason},
    State2 = close(State1),
    State3 = bounce_queue(State2),
    xmpp_stream_out:set_timeout(State3, Delay);
process_auth_result(State, true) ->
    State.

process_closed(#{server := LServer, remote_server := RServer,
		 on_route := send} = State,
	       Reason) ->
    ?INFO_MSG("Closing outbound s2s connection ~ts -> ~ts: ~ts",
	      [LServer, RServer, format_error(Reason)]),
    stop_async(self()),
    State;
process_closed(#{server := LServer, remote_server := RServer} = State,
	       Reason) ->
    Delay = get_delay(),
    ?WARNING_MSG("Failed to establish outbound s2s connection ~ts -> ~ts: ~ts; "
		 "bouncing for ~p seconds",
		 [LServer, RServer, format_error(Reason), Delay div 1000]),
    State1 = State#{on_route => bounce},
    State2 = bounce_queue(State1),
    xmpp_stream_out:set_timeout(State2, Delay).

handle_unexpected_info(State, Info) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    State.

handle_unexpected_cast(State, Msg) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    State.

process_downgraded(State, _StreamStart) ->
    send(State, xmpp:serr_unsupported_version()).

%%%===================================================================
%%% xmpp_stream_out callbacks
%%%===================================================================
tls_options(#{server := LServer, server_host := ServerHost}) ->
    ejabberd_s2s:tls_options(LServer, ServerHost, []).

tls_required(#{server_host := ServerHost}) ->
    ejabberd_s2s:tls_required(ServerHost).

tls_verify(#{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_out_tls_verify, ServerHost, true, [State]).

tls_enabled(#{server_host := ServerHost}) ->
    ejabberd_s2s:tls_enabled(ServerHost).

connect_options(Addr, Opts, #{server_host := ServerHost}) ->
    BindAddr = case get_addr_type(Addr) of
		   inet ->
		       ejabberd_option:outgoing_s2s_ipv4_address(ServerHost);
		   inet6 ->
		       ejabberd_option:outgoing_s2s_ipv6_address(ServerHost)
	       end,
    case BindAddr of
	undefined ->
	    Opts;
	_ ->
	    [{ip, BindAddr} | Opts]
    end.

connect_timeout(#{server_host := ServerHost}) ->
    ejabberd_option:outgoing_s2s_timeout(ServerHost).

default_port(#{server_host := ServerHost}) ->
    ejabberd_option:outgoing_s2s_port(ServerHost).

address_families(#{server_host := ServerHost}) ->
    ejabberd_option:outgoing_s2s_families(ServerHost).

dns_retries(#{server_host := ServerHost}) ->
    ejabberd_option:s2s_dns_retries(ServerHost).

dns_timeout(#{server_host := ServerHost}) ->
    ejabberd_option:s2s_dns_timeout(ServerHost).

handle_unauthenticated_features(Features, #{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_out_unauthenticated_features, ServerHost, State, [Features]).

handle_auth_success(Mech, #{socket := Socket, ip := IP,
			    remote_server := RServer,
			    server_host := ServerHost,
			    server := LServer} = State) ->
    ?INFO_MSG("(~ts) Accepted outbound s2s ~ts authentication ~ts -> ~ts (~ts)",
	      [xmpp_socket:pp(Socket), Mech, LServer, RServer,
	       ejabberd_config:may_hide_data(misc:ip_to_list(IP))]),
    ejabberd_hooks:run_fold(s2s_out_auth_result, ServerHost, State, [true]).

handle_auth_failure(Mech, Reason,
		    #{socket := Socket, ip := IP,
		      remote_server := RServer,
		      server_host := ServerHost,
		      server := LServer} = State) ->
    ?WARNING_MSG("(~ts) Failed outbound s2s ~ts authentication ~ts -> ~ts (~ts): ~ts",
		 [xmpp_socket:pp(Socket), Mech, LServer, RServer,
		  ejabberd_config:may_hide_data(misc:ip_to_list(IP)),
		  xmpp_stream_out:format_error(Reason)]),
    ejabberd_hooks:run_fold(s2s_out_auth_result, ServerHost, State, [{false, Reason}]).

handle_packet(Pkt, #{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_out_packet, ServerHost, State, [Pkt]).

handle_stream_end(Reason, #{server_host := ServerHost} = State) ->
    State1 = State#{stop_reason => Reason},
    ejabberd_hooks:run_fold(s2s_out_closed, ServerHost, State1, [Reason]).

handle_stream_downgraded(StreamStart, #{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_out_downgraded, ServerHost, State, [StreamStart]).

handle_stream_established(State) ->
    State1 = State#{on_route => send},
    State2 = resend_queue(State1),
    set_idle_timeout(State2).

handle_cdata(Data, #{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_out_handle_cdata, ServerHost, State, [Data]).

handle_recv(El, Pkt, #{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_out_handle_recv, ServerHost, State, [El, Pkt]).

handle_send(El, Pkt, #{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_out_handle_send, ServerHost, State, [El, Pkt]).

handle_timeout(#{on_route := Action, lang := Lang} = State) ->
    case Action of
	bounce ->
	    stop_async(self()),
	    State;
	_ ->
	    Txt = ?T("Idle connection"),
	    send(State, xmpp:serr_connection_timeout(Txt, Lang))
    end.

init([#{server := LServer, remote_server := RServer} = State, Opts]) ->
    ServerHost = ejabberd_router:host_of_route(LServer),
    QueueType = ejabberd_s2s:queue_type(ServerHost),
    QueueLimit = case lists:keyfind(
			max_queue, 1, ejabberd_config:fsm_limit_opts([])) of
		     {_, N} -> N;
		     false -> unlimited
		 end,
    Timeout = ejabberd_option:negotiation_timeout(),
    State1 = State#{on_route => queue,
		    queue => p1_queue:new(QueueType, QueueLimit),
		    xmlns => ?NS_SERVER,
		    lang => ejabberd_option:language(),
		    server_host => ServerHost,
		    shaper => none},
    State2 = xmpp_stream_out:set_timeout(State1, Timeout),
    ?INFO_MSG("Outbound s2s connection started: ~ts -> ~ts",
	      [LServer, RServer]),
    ejabberd_hooks:run_fold(s2s_out_init, ServerHost, {ok, State2}, [Opts]).

handle_call(Request, From, #{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_out_handle_call, ServerHost, State, [Request, From]).

handle_cast({update_state, Fun}, State) ->
    case Fun of
	{M, F, A} -> erlang:apply(M, F, [State|A]);
	_ when is_function(Fun) -> Fun(State)
	  end;
handle_cast(Msg, #{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_out_handle_cast, ServerHost, State, [Msg]).

handle_info({route, Pkt}, #{queue := Q, on_route := Action} = State) ->
    case Action of
	queue ->
	    try State#{queue => p1_queue:in(Pkt, Q)}
	    catch error:full ->
		    Q1 = p1_queue:set_limit(Q, unlimited),
		    Q2 = p1_queue:in(Pkt, Q1),
		    handle_stream_end(queue_full, State#{queue => Q2})
	    end;
	bounce -> bounce_packet(Pkt, State);
	send -> set_idle_timeout(send(State, Pkt))
    end;
handle_info(Info, #{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_out_handle_info, ServerHost, State, [Info]).

terminate(Reason, #{server := LServer,
		    remote_server := RServer} = State) ->
    State1 = case Reason of
		 normal -> State;
		 _ -> State#{stop_reason => internal_failure}
    end,
    State2 = bounce_queue(State1),
    bounce_message_queue({LServer, RServer}, State2).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_addr_type(inet:ip_address()) -> inet:address_family().
get_addr_type({_, _, _, _}) -> inet;
get_addr_type({_, _, _, _, _, _, _, _}) -> inet6.

-spec resend_queue(state()) -> state().
resend_queue(State) ->
    queue_fold(
      fun(Pkt, AccState) ->
	      send(AccState, Pkt)
      end, State).

-spec bounce_queue(state()) -> state().
bounce_queue(State) ->
    queue_fold(
      fun(Pkt, AccState) ->
	      bounce_packet(Pkt, AccState)
      end, State).

-spec bounce_message_queue({binary(), binary()}, state()) -> state().
bounce_message_queue(FromTo, State) ->
    receive {route, Pkt} ->
	    State1 = bounce_packet(Pkt, State),
	    bounce_message_queue(FromTo, State1)
    after 0 ->
	    State
    end.

-spec bounce_packet(xmpp_element(), state()) -> state().
bounce_packet(Pkt, State) when ?is_stanza(Pkt) ->
    #{server_host := Host} = State,
    case ejabberd_hooks:run_fold(
           s2s_out_bounce_packet, Host, State, [Pkt]) of
        ignore ->
            State;
        State2 ->
            Lang = xmpp:get_lang(Pkt),
            Err = mk_bounce_error(Lang, State2),
            ejabberd_router:route_error(Pkt, Err),
            State2
    end;
bounce_packet(_, State) ->
    State.

-spec mk_bounce_error(binary(), state()) -> stanza_error().
mk_bounce_error(Lang, #{stop_reason := Why}) ->
    Reason = format_error(Why),
    case Why of
	internal_failure ->
	    xmpp:err_internal_server_error(Reason, Lang);
	queue_full ->
	    xmpp:err_resource_constraint(Reason, Lang);
	{dns, _} ->
	    xmpp:err_remote_server_not_found(Reason, Lang);
	{idna, _} ->
	    xmpp:err_remote_server_not_found(Reason, Lang);
	_ ->
	    xmpp:err_remote_server_timeout(Reason, Lang)
    end;
mk_bounce_error(_Lang, _State) ->
    %% We should not be here. Probably :)
    xmpp:err_remote_server_not_found().

-spec get_delay() -> non_neg_integer().
get_delay() ->
    MaxDelay = ejabberd_option:s2s_max_retry_delay(),
    p1_rand:uniform(MaxDelay).

-spec set_idle_timeout(state()) -> state().
set_idle_timeout(#{on_route := send, server_host := ServerHost} = State) ->
    Timeout = ejabberd_s2s:get_idle_timeout(ServerHost),
    xmpp_stream_out:set_timeout(State, Timeout);
set_idle_timeout(State) ->
    State.

-spec queue_fold(fun((xmpp_element(), state()) -> state()), state()) -> state().
queue_fold(F, #{queue := Q} = State) ->
    case p1_queue:out(Q) of
	{{value, Pkt}, Q1} ->
	    State1 = F(Pkt, State#{queue => Q1}),
	    queue_fold(F, State1);
	{empty, Q1} ->
	    State#{queue => Q1}
    end.

format_error(internal_failure) ->
    <<"Internal server error">>;
format_error(queue_full) ->
    <<"Stream queue is overloaded">>;
format_error(Reason) ->
    xmpp_stream_out:format_error(Reason).
