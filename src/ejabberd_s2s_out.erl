%%%-------------------------------------------------------------------
%%% Created : 16 Dec 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
%%%-------------------------------------------------------------------
-module(ejabberd_s2s_out).
-behaviour(xmpp_stream_out).
-behaviour(ejabberd_config).

%% ejabberd_config callbacks
-export([opt_type/1, transform_options/1]).
%% xmpp_stream_out callbacks
-export([tls_options/1, tls_required/1, tls_verify/1, tls_enabled/1,
	 connect_timeout/1, address_families/1, default_port/1,
	 dns_retries/1, dns_timeout/1,
	 handle_auth_success/2, handle_auth_failure/3, handle_packet/2,
	 handle_stream_end/2, handle_stream_downgraded/2,
	 handle_recv/3, handle_send/3, handle_cdata/2,
	 handle_stream_established/1, handle_timeout/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
%% Hooks
-export([process_auth_result/2, process_closed/2, handle_unexpected_info/2,
	 handle_unexpected_cast/2, process_downgraded/2]).
%% API
-export([start/3, start_link/3, connect/1, close/1, stop/1, send/2,
	 route/2, establish/1, update_state/2, host_up/1, host_down/1]).

-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").

-type state() :: map().
-export_type([state/0]).

%%%===================================================================
%%% API
%%%===================================================================
start(From, To, Opts) ->
    case proplists:get_value(supervisor, Opts, true) of
	   true ->
	    supervisor:start_child(ejabberd_s2s_out_sup,
				   [From, To, Opts]);
		_ ->
	    xmpp_stream_out:start(?MODULE, [ejabberd_socket, From, To, Opts],
				  ejabberd_config:fsm_limit_opts([]))
    end.

start_link(From, To, Opts) ->
    xmpp_stream_out:start_link(?MODULE, [ejabberd_socket, From, To, Opts],
			       ejabberd_config:fsm_limit_opts([])).

-spec connect(pid()) -> ok.
connect(Ref) ->
    xmpp_stream_out:connect(Ref).

-spec close(pid()) -> ok;
	   (state()) -> state().
close(Ref) ->
    xmpp_stream_out:close(Ref).

-spec stop(pid()) -> ok;
	  (state()) -> no_return().
stop(Ref) ->
    xmpp_stream_out:stop(Ref).

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
    ?INFO_MSG("Failed to establish outbound s2s connection ~s -> ~s: "
	      "authentication failed; bouncing for ~p seconds",
	      [LServer, RServer, Delay]),
    State1 = State#{on_route => bounce, stop_reason => Reason},
    State2 = close(State1),
    State3 = bounce_queue(State2),
    xmpp_stream_out:set_timeout(State3, timer:seconds(Delay));
process_auth_result(State, true) ->
    State.

process_closed(#{server := LServer, remote_server := RServer,
		 on_route := send} = State,
	       Reason) ->
    ?INFO_MSG("Closing outbound s2s connection ~s -> ~s: ~s",
	      [LServer, RServer, format_error(Reason)]),
    stop(State);
process_closed(#{server := LServer, remote_server := RServer} = State,
	       Reason) ->
    Delay = get_delay(),
    ?INFO_MSG("Failed to establish outbound s2s connection ~s -> ~s: ~s; "
	      "bouncing for ~p seconds",
	      [LServer, RServer, format_error(Reason), Delay]),
    State1 = State#{on_route => bounce},
    State2 = bounce_queue(State1),
    xmpp_stream_out:set_timeout(State2, timer:seconds(Delay)).

handle_unexpected_info(State, Info) ->
    ?WARNING_MSG("got unexpected info: ~p", [Info]),
    State.

handle_unexpected_cast(State, Msg) ->
    ?WARNING_MSG("got unexpected cast: ~p", [Msg]),
    State.

process_downgraded(State, _StreamStart) ->
    send(State, xmpp:serr_unsupported_version()).

%%%===================================================================
%%% xmpp_stream_out callbacks
%%%===================================================================
tls_options(#{server := LServer}) ->
    ejabberd_s2s:tls_options(LServer, []).

tls_required(#{server := LServer}) ->
    ejabberd_s2s:tls_required(LServer).

tls_verify(#{server := LServer}) ->
    ejabberd_s2s:tls_verify(LServer).

tls_enabled(#{server := LServer}) ->
    ejabberd_s2s:tls_enabled(LServer).

connect_timeout(#{server := LServer}) ->
    ejabberd_config:get_option(
      {outgoing_s2s_timeout, LServer},
      fun(TimeOut) when is_integer(TimeOut), TimeOut > 0 ->
              timer:seconds(TimeOut);
         (infinity) ->
              infinity
      end, timer:seconds(10)).

default_port(#{server := LServer}) ->
    ejabberd_config:get_option(
      {outgoing_s2s_port, LServer},
      fun(I) when is_integer(I), I > 0, I =< 65536 -> I end,
      5269).

address_families(#{server := LServer}) ->
    ejabberd_config:get_option(
      {outgoing_s2s_families, LServer},
      fun(Families) ->
	      lists:map(
		fun(ipv4) -> inet;
		   (ipv6) -> inet6
		end, Families)
      end, [inet, inet6]).

dns_retries(#{server := LServer}) ->
    ejabberd_config:get_option(
      {s2s_dns_retries, LServer},
      fun(I) when is_integer(I), I>=0 -> I end,
      2).

dns_timeout(#{server := LServer}) ->
    ejabberd_config:get_option(
      {s2s_dns_timeout, LServer},
      fun(I) when is_integer(I), I>=0 ->
	      timer:seconds(I);
	 (infinity) ->
	      infinity
      end, timer:seconds(10)).

handle_auth_success(Mech, #{sockmod := SockMod,
			    socket := Socket, ip := IP,
			    remote_server := RServer,
			    server_host := ServerHost,
			    server := LServer} = State) ->
    ?INFO_MSG("(~s) Accepted outbound s2s ~s authentication ~s -> ~s (~s)",
	      [SockMod:pp(Socket), Mech, LServer, RServer,
	       ejabberd_config:may_hide_data(misc:ip_to_list(IP))]),
    ejabberd_hooks:run_fold(s2s_out_auth_result, ServerHost, State, [true]).

handle_auth_failure(Mech, Reason,
		    #{sockmod := SockMod,
		      socket := Socket, ip := IP,
		      remote_server := RServer,
		      server_host := ServerHost,
		      server := LServer} = State) ->
    ?INFO_MSG("(~s) Failed outbound s2s ~s authentication ~s -> ~s (~s): ~s",
	      [SockMod:pp(Socket), Mech, LServer, RServer,
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

handle_timeout(#{on_route := Action} = State) ->
    case Action of
	bounce -> stop(State);
	_ -> send(State, xmpp:serr_connection_timeout())
    end.

init([#{server := LServer, remote_server := RServer} = State, Opts]) ->
    ServerHost = ejabberd_router:host_of_route(LServer),
    QueueType = ejabberd_s2s:queue_type(LServer),
    QueueLimit = case lists:keyfind(
			max_queue, 1, ejabberd_config:fsm_limit_opts([])) of
		     {_, N} -> N;
		     false -> unlimited
		 end,
    State1 = State#{on_route => queue,
		    queue => p1_queue:new(QueueType, QueueLimit),
		    xmlns => ?NS_SERVER,
		    lang => ?MYLANG,
		    server_host => ServerHost,
		    shaper => none},
    ?INFO_MSG("Outbound s2s connection started: ~s -> ~s",
	      [LServer, RServer]),
    ejabberd_hooks:run_fold(s2s_out_init, ServerHost, {ok, State1}, [Opts]).

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
    ejabberd_s2s:remove_connection({LServer, RServer}, self()),
    State1 = case Reason of
		 normal -> State;
		 _ -> State#{stop_reason => internal_failure}
    end,
    bounce_queue(State1),
    bounce_message_queue(State1).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
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

-spec bounce_message_queue(state()) -> state().
bounce_message_queue(State) ->
    receive {route, Pkt} ->
	    State1 = bounce_packet(Pkt, State),
	    bounce_message_queue(State1)
    after 0 ->
	    State
    end.

-spec bounce_packet(xmpp_element(), state()) -> state().
bounce_packet(Pkt, State) when ?is_stanza(Pkt) ->
    Lang = xmpp:get_lang(Pkt),
    Err = mk_bounce_error(Lang, State),
    ejabberd_router:route_error(Pkt, Err),
    State;
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
					     _ ->
	    xmpp:err_remote_server_timeout(Reason, Lang)
	  end;
mk_bounce_error(_Lang, _State) ->
    %% We should not be here. Probably :)
    xmpp:err_remote_server_not_found().

-spec get_delay() -> non_neg_integer().
get_delay() ->
    MaxDelay = ejabberd_config:get_option(
		 s2s_max_retry_delay,
		 fun(I) when is_integer(I), I > 0 -> I end,
		 300),
    crypto:rand_uniform(1, MaxDelay).

-spec set_idle_timeout(state()) -> state().
set_idle_timeout(#{on_route := send, server := LServer} = State) ->
    Timeout = ejabberd_s2s:get_idle_timeout(LServer),
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

transform_options(Opts) ->
    lists:foldl(fun transform_options/2, [], Opts).

transform_options({outgoing_s2s_options, Families, Timeout}, Opts) ->
    ?WARNING_MSG("Option 'outgoing_s2s_options' is deprecated. "
                 "The option is still supported "
                 "but it is better to fix your config: "
                 "use 'outgoing_s2s_timeout' and "
                 "'outgoing_s2s_families' instead.", []),
    maybe_report_huge_timeout(outgoing_s2s_timeout, Timeout),
    [{outgoing_s2s_families, Families},
     {outgoing_s2s_timeout, Timeout}
     | Opts];
transform_options({s2s_dns_options, S2SDNSOpts}, AllOpts) ->
    ?WARNING_MSG("Option 's2s_dns_options' is deprecated. "
                 "The option is still supported "
                 "but it is better to fix your config: "
                 "use 's2s_dns_timeout' and "
                 "'s2s_dns_retries' instead", []),
    lists:foldr(
      fun({timeout, T}, AccOpts) ->
	      maybe_report_huge_timeout(s2s_dns_timeout, T),
              [{s2s_dns_timeout, T}|AccOpts];
         ({retries, R}, AccOpts) ->
              [{s2s_dns_retries, R}|AccOpts];
         (_, AccOpts) ->
              AccOpts
      end, AllOpts, S2SDNSOpts);
transform_options({Opt, T}, Opts)
  when Opt == outgoing_s2s_timeout; Opt == s2s_dns_timeout ->
    maybe_report_huge_timeout(Opt, T),
    [{Opt, T}|Opts];
transform_options(Opt, Opts) ->
    [Opt|Opts].

maybe_report_huge_timeout(Opt, T) when is_integer(T), T >= 1000 ->
    ?WARNING_MSG("value '~p' of option '~p' is too big, "
		 "are you sure you have set seconds?",
		 [T, Opt]);
maybe_report_huge_timeout(_, _) ->
    ok.

opt_type(outgoing_s2s_families) ->
    fun (Families) ->
	    true = lists:all(fun (ipv4) -> true;
				 (ipv6) -> true
			     end,
			     Families),
	    Families
    end;
opt_type(outgoing_s2s_port) ->
    fun (I) when is_integer(I), I > 0, I =< 65536 -> I end;
opt_type(outgoing_s2s_timeout) ->
    fun (TimeOut) when is_integer(TimeOut), TimeOut > 0 ->
	    TimeOut;
	(infinity) -> infinity
    end;
opt_type(s2s_dns_retries) ->
    fun (I) when is_integer(I), I >= 0 -> I end;
opt_type(s2s_dns_timeout) ->
    fun (TimeOut) when is_integer(TimeOut), TimeOut > 0 ->
	    TimeOut;
	(infinity) -> infinity
    end;
opt_type(s2s_max_retry_delay) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(_) ->
    [outgoing_s2s_families, outgoing_s2s_port, outgoing_s2s_timeout,
     s2s_dns_retries, s2s_dns_timeout, s2s_max_retry_delay].
