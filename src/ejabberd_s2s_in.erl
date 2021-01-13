%%%-------------------------------------------------------------------
%%% Created : 12 Dec 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2021   ProcessOne
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
-module(ejabberd_s2s_in).
-behaviour(xmpp_stream_in).
-behaviour(ejabberd_listener).

%% ejabberd_listener callbacks
-export([start/3, start_link/3, accept/1, listen_options/0]).
%% xmpp_stream_in callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).
-export([tls_options/1, tls_required/1, tls_enabled/1, compress_methods/1,
	 unauthenticated_stream_features/1, authenticated_stream_features/1,
	 handle_stream_start/2, handle_stream_end/2,
	 handle_stream_established/1, handle_auth_success/4,
	 handle_auth_failure/4, handle_send/3, handle_recv/3, handle_cdata/2,
	 handle_unauthenticated_packet/2, handle_authenticated_packet/2]).
%% Hooks
-export([handle_unexpected_info/2, handle_unexpected_cast/2,
	 reject_unauthenticated_packet/2, process_closed/2]).
%% API
-export([stop_async/1, close/1, close/2, send/2, update_state/2, establish/1,
	 host_up/1, host_down/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").

-type state() :: xmpp_stream_in:state().
-export_type([state/0]).

%%%===================================================================
%%% API
%%%===================================================================
start(SockMod, Socket, Opts) ->
    xmpp_stream_in:start(?MODULE, [{SockMod, Socket}, Opts],
			 ejabberd_config:fsm_limit_opts(Opts)).

start_link(SockMod, Socket, Opts) ->
    xmpp_stream_in:start_link(?MODULE, [{SockMod, Socket}, Opts],
			      ejabberd_config:fsm_limit_opts(Opts)).

close(Ref) ->
    xmpp_stream_in:close(Ref).

close(Ref, Reason) ->
    xmpp_stream_in:close(Ref, Reason).

-spec stop_async(pid()) -> ok.
stop_async(Pid) ->
    xmpp_stream_in:stop_async(Pid).

accept(Ref) ->
    xmpp_stream_in:accept(Ref).

-spec send(pid(), xmpp_element()) -> ok;
	  (state(), xmpp_element()) -> state().
send(Stream, Pkt) ->
    xmpp_stream_in:send(Stream, Pkt).

-spec establish(state()) -> state().
establish(State) ->
    xmpp_stream_in:establish(State).

-spec update_state(pid(), fun((state()) -> state()) |
		   {module(), atom(), list()}) -> ok.
update_state(Ref, Callback) ->
    xmpp_stream_in:cast(Ref, {update_state, Callback}).

-spec host_up(binary()) -> ok.
host_up(Host) ->
    ejabberd_hooks:add(s2s_in_closed, Host, ?MODULE,
		       process_closed, 100),
    ejabberd_hooks:add(s2s_in_unauthenticated_packet, Host, ?MODULE,
		       reject_unauthenticated_packet, 100),
    ejabberd_hooks:add(s2s_in_handle_info, Host, ?MODULE,
		       handle_unexpected_info, 100),
    ejabberd_hooks:add(s2s_in_handle_cast, Host, ?MODULE,
		       handle_unexpected_cast, 100).

-spec host_down(binary()) -> ok.
host_down(Host) ->
    ejabberd_hooks:delete(s2s_in_closed, Host, ?MODULE,
			  process_closed, 100),
    ejabberd_hooks:delete(s2s_in_unauthenticated_packet, Host, ?MODULE,
			  reject_unauthenticated_packet, 100),
    ejabberd_hooks:delete(s2s_in_handle_info, Host, ?MODULE,
			  handle_unexpected_info, 100),
    ejabberd_hooks:delete(s2s_in_handle_cast, Host, ?MODULE,
			  handle_unexpected_cast, 100).

%%%===================================================================
%%% Hooks
%%%===================================================================
handle_unexpected_info(State, Info) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    State.

handle_unexpected_cast(State, Msg) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    State.

reject_unauthenticated_packet(State, _Pkt) ->
    Err = xmpp:serr_not_authorized(),
    send(State, Err).

process_closed(#{server := LServer} = State, Reason) ->
    RServer = case State of
		  #{remote_server := Name} ->
		      Name;
		  #{ip := IP} ->
		      ejabberd_config:may_hide_data(misc:ip_to_list(IP))
	      end,
    ?INFO_MSG("Closing inbound s2s connection ~ts -> ~ts: ~ts",
	      [RServer, LServer, xmpp_stream_out:format_error(Reason)]),
    stop_async(self()),
    State.

%%%===================================================================
%%% xmpp_stream_in callbacks
%%%===================================================================
tls_options(#{tls_options := TLSOpts, lserver := LServer, server_host := ServerHost}) ->
    ejabberd_s2s:tls_options(LServer, ServerHost, TLSOpts).

tls_required(#{server_host := ServerHost}) ->
    ejabberd_s2s:tls_required(ServerHost).

tls_enabled(#{server_host := ServerHost}) ->
    ejabberd_s2s:tls_enabled(ServerHost).

compress_methods(#{server_host := ServerHost}) ->
    case ejabberd_s2s:zlib_enabled(ServerHost) of
	true -> [<<"zlib">>];
	false -> []
    end.

unauthenticated_stream_features(#{server_host := LServer}) ->
    ejabberd_hooks:run_fold(s2s_in_pre_auth_features, LServer, [], [LServer]).

authenticated_stream_features(#{server_host := LServer}) ->
    ejabberd_hooks:run_fold(s2s_in_post_auth_features, LServer, [], [LServer]).

handle_stream_start(_StreamStart, #{lserver := LServer} = State) ->
    case check_to(jid:make(LServer), State) of
	false ->
	    send(State, xmpp:serr_host_unknown());
	true ->
	    ServerHost = ejabberd_router:host_of_route(LServer),
	    Opts = ejabberd_config:codec_options(),
	    State#{server_host => ServerHost, codec_options => Opts}
    end.

handle_stream_end(Reason, #{server_host := ServerHost} = State) ->
    State1 = State#{stop_reason => Reason},
    ejabberd_hooks:run_fold(s2s_in_closed, ServerHost, State1, [Reason]).

handle_stream_established(State) ->
    set_idle_timeout(State#{established => true}).

handle_auth_success(RServer, Mech, _AuthModule,
		    #{socket := Socket, ip := IP,
		      auth_domains := AuthDomains,
		      server_host := ServerHost,
		      lserver := LServer} = State) ->
    ?INFO_MSG("(~ts) Accepted inbound s2s ~ts authentication ~ts -> ~ts (~ts)",
	      [xmpp_socket:pp(Socket), Mech, RServer, LServer,
	       ejabberd_config:may_hide_data(misc:ip_to_list(IP))]),
    State1 = case ejabberd_s2s:allow_host(ServerHost, RServer) of
		 true ->
		     AuthDomains1 = sets:add_element(RServer, AuthDomains),
		     State0 = change_shaper(State, RServer),
		     State0#{auth_domains => AuthDomains1};
		 false ->
		     State
	     end,
    ejabberd_hooks:run_fold(s2s_in_auth_result, ServerHost, State1, [true, RServer]).

handle_auth_failure(RServer, Mech, Reason,
		    #{socket := Socket, ip := IP,
		      server_host := ServerHost,
		      lserver := LServer} = State) ->
    ?WARNING_MSG("(~ts) Failed inbound s2s ~ts authentication ~ts -> ~ts (~ts): ~ts",
		 [xmpp_socket:pp(Socket), Mech, RServer, LServer,
		  ejabberd_config:may_hide_data(misc:ip_to_list(IP)), Reason]),
    ejabberd_hooks:run_fold(s2s_in_auth_result,
			    ServerHost, State, [false, RServer]).

handle_unauthenticated_packet(Pkt, #{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_in_unauthenticated_packet,
			    ServerHost, State, [Pkt]).

handle_authenticated_packet(Pkt, #{server_host := ServerHost} = State) when not ?is_stanza(Pkt) ->
    ejabberd_hooks:run_fold(s2s_in_authenticated_packet, ServerHost, State, [Pkt]);
handle_authenticated_packet(Pkt0, #{ip := {IP, _}} = State) ->
    Pkt = xmpp:put_meta(Pkt0, ip, IP),
    From = xmpp:get_from(Pkt),
    To = xmpp:get_to(Pkt),
    case check_from_to(From, To, State) of
	ok ->
	    LServer = ejabberd_router:host_of_route(To#jid.lserver),
	    State1 = ejabberd_hooks:run_fold(s2s_in_authenticated_packet,
					     LServer, State, [Pkt]),
	    {Pkt1, State2} = ejabberd_hooks:run_fold(s2s_receive_packet, LServer,
						     {Pkt, State1}, []),
	    case Pkt1 of
		drop -> ok;
		_ -> ejabberd_router:route(Pkt1)
	    end,
	    State2;
	{error, Err} ->
	    send(State, Err)
    end.

handle_cdata(Data, #{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_in_handle_cdata, ServerHost, State, [Data]).

handle_recv(El, Pkt, #{server_host := ServerHost} = State) ->
    State1 = set_idle_timeout(State),
    ejabberd_hooks:run_fold(s2s_in_handle_recv, ServerHost, State1, [El, Pkt]).

handle_send(Pkt, Result, #{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_in_handle_send, ServerHost,
			    State, [Pkt, Result]).

init([State, Opts]) ->
    Shaper = proplists:get_value(shaper, Opts, none),
    TLSOpts1 = lists:filter(
		 fun({certfile, _}) -> true;
		    ({ciphers, _}) -> true;
		    ({dhfile, _}) -> true;
		    ({cafile, _}) -> true;
		    ({protocol_options, _}) -> true;
		    (_) -> false
		 end, Opts),
    TLSOpts2 = case proplists:get_bool(tls_compression, Opts) of
		   false -> [compression_none | TLSOpts1];
		   true -> TLSOpts1
	       end,
    Timeout = ejabberd_option:negotiation_timeout(),
    State1 = State#{tls_options => TLSOpts2,
		    auth_domains => sets:new(),
		    xmlns => ?NS_SERVER,
		    lang => ejabberd_option:language(),
		    server => ejabberd_config:get_myname(),
		    lserver => ejabberd_config:get_myname(),
		    server_host => ejabberd_config:get_myname(),
		    established => false,
		    shaper => Shaper},
    State2 = xmpp_stream_in:set_timeout(State1, Timeout),
    ejabberd_hooks:run_fold(s2s_in_init, {ok, State2}, [Opts]).

handle_call(Request, From, #{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_in_handle_call, ServerHost, State, [Request, From]).

handle_cast({update_state, Fun}, State) ->
    case Fun of
	{M, F, A} -> erlang:apply(M, F, [State|A]);
	_ when is_function(Fun) -> Fun(State)
    end;
handle_cast(Msg, #{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_in_handle_cast, ServerHost, State, [Msg]).

handle_info(Info, #{server_host := ServerHost} = State) ->
    ejabberd_hooks:run_fold(s2s_in_handle_info, ServerHost, State, [Info]).

terminate(Reason, #{auth_domains := AuthDomains,
		    socket := Socket} = State) ->
    case maps:get(stop_reason, State, undefined) of
	{tls, _} = Err ->
	    ?WARNING_MSG("(~ts) Failed to secure inbound s2s connection: ~ts",
			 [xmpp_socket:pp(Socket), xmpp_stream_in:format_error(Err)]);
	_ ->
	    ok
    end,
    case Reason of
      {process_limit, _} ->
	    sets:fold(
	      fun(Host, _) ->
		      ejabberd_s2s:external_host_overloaded(Host)
	      end, ok, AuthDomains);
	_ ->
	    ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec check_from_to(jid(), jid(), state()) -> ok | {error, stream_error()}.
check_from_to(From, To, State) ->
    case check_from(From, State) of
	true ->
	    case check_to(To, State) of
		true ->
		    ok;
		false ->
		    {error, xmpp:serr_host_unknown()}
	    end;
	false ->
	    {error, xmpp:serr_invalid_from()}
    end.

-spec check_from(jid(), state()) -> boolean().
check_from(#jid{lserver = S1}, #{auth_domains := AuthDomains}) ->
    sets:is_element(S1, AuthDomains).

-spec check_to(jid(), state()) -> boolean().
check_to(#jid{lserver = LServer}, _State) ->
    ejabberd_router:is_my_route(LServer).

-spec set_idle_timeout(state()) -> state().
set_idle_timeout(#{server_host := ServerHost,
		   established := true} = State) ->
    Timeout = ejabberd_s2s:get_idle_timeout(ServerHost),
    xmpp_stream_in:set_timeout(State, Timeout);
set_idle_timeout(State) ->
    State.

-spec change_shaper(state(), binary()) -> state().
change_shaper(#{shaper := ShaperName, server_host := ServerHost} = State,
	      RServer) ->
    Shaper = ejabberd_shaper:match(ServerHost, ShaperName, jid:make(RServer)),
    xmpp_stream_in:change_shaper(State, ejabberd_shaper:new(Shaper)).

listen_options() ->
    [{shaper, none},
     {ciphers, undefined},
     {dhfile, undefined},
     {cafile, undefined},
     {protocol_options, undefined},
     {tls, false},
     {tls_compression, false},
     {max_stanza_size, infinity},
     {max_fsm_queue, 10000}].
