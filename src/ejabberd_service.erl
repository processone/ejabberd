%%%-------------------------------------------------------------------
%%% Created : 11 Dec 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
-module(ejabberd_service).
-behaviour(xmpp_stream_in).
-behaviour(ejabberd_listener).

-protocol({xep, 114, '1.6', '0.1.0', "complete", ""}).

%% ejabberd_listener callbacks
-export([start/3, start_link/3, stop/0, accept/1]).
-export([listen_opt_type/1, listen_options/0]).
%% xmpp_stream_in callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3]).
-export([handle_stream_start/2, handle_auth_success/4, handle_auth_failure/4,
	 handle_authenticated_packet/2, get_password_fun/1, tls_options/1]).
%% API
-export([send/2, close/1, close/2, stop_async/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("translate.hrl").

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

-spec stop() -> ok.
stop() ->
    Err = xmpp:serr_system_shutdown(),
    lists:foreach(
      fun({_Id, Pid, _Type, _Module}) ->
              send(Pid, Err),
              stop_async(Pid),
              supervisor:terminate_child(ejabberd_service_sup, Pid)
      end, supervisor:which_children(ejabberd_service_sup)),
    _ = supervisor:terminate_child(ejabberd_sup, ejabberd_service_sup),
    _ = supervisor:delete_child(ejabberd_sup, ejabberd_service_sup),
    ok.

accept(Ref) ->
    xmpp_stream_in:accept(Ref).

-spec send(pid(), xmpp_element()) -> ok;
	  (state(), xmpp_element()) -> state().
send(Stream, Pkt) ->
    xmpp_stream_in:send(Stream, Pkt).

-spec close(pid()) -> ok;
	   (state()) -> state().
close(Ref) ->
    xmpp_stream_in:close(Ref).

-spec close(pid(), atom()) -> ok.
close(Ref, Reason) ->
    xmpp_stream_in:close(Ref, Reason).

-spec stop_async(pid()) -> ok.
stop_async(Pid) ->
    xmpp_stream_in:stop_async(Pid).

%%%===================================================================
%%% xmpp_stream_in callbacks
%%%===================================================================
tls_options(#{tls_options := TLSOptions}) ->
    TLSOptions.

init([State, Opts]) ->
    Access = proplists:get_value(access, Opts, all),
    Shaper = proplists:get_value(shaper, Opts,
				 proplists:get_value(shaper_rule, Opts, none)),
    GlobalPassword = proplists:get_value(password, Opts, random_password()),
    HostOpts = proplists:get_value(hosts, Opts, [{global, GlobalPassword}]),
    HostOpts1 = lists:map(
		  fun({Host, undefined}) -> {Host, GlobalPassword};
		     ({Host, Password}) -> {Host, Password}
		  end, HostOpts),
    CheckFrom = proplists:get_value(check_from, Opts, true),
    TLSOpts1 = lists:filter(
		 fun({certfile, _}) -> true;
		    ({ciphers, _}) -> true;
		    ({dhfile, _}) -> true;
		    ({cafile, _}) -> true;
		    ({protocol_options, _}) -> true;
		    (_) -> false
		 end, Opts),
    TLSOpts = case proplists:get_bool(tls_compression, Opts) of
		  false -> [compression_none | TLSOpts1];
		  true -> TLSOpts1
	      end,
    GlobalRoutes = proplists:get_value(global_routes, Opts, true),
    Timeout = ejabberd_option:negotiation_timeout(),
    State1 = xmpp_stream_in:change_shaper(State, ejabberd_shaper:new(Shaper)),
    State2 = xmpp_stream_in:set_timeout(State1, Timeout),
    State3 = State2#{access => Access,
		     xmlns => ?NS_COMPONENT,
		     lang => ejabberd_option:language(),
		     server => ejabberd_config:get_myname(),
		     host_opts => maps:from_list(HostOpts1),
		     stream_version => undefined,
		     tls_options => TLSOpts,
		     global_routes => GlobalRoutes,
		     check_from => CheckFrom},
    ejabberd_hooks:run_fold(component_init, {ok, State3}, [Opts]).

handle_stream_start(_StreamStart,
		    #{remote_server := RemoteServer,
		      lang := Lang,
		      host_opts := HostOpts} = State) ->
    case ejabberd_router:is_my_host(RemoteServer) of
	true ->
	    Txt = ?T("Unable to register route on existing local domain"),
	    xmpp_stream_in:send(State, xmpp:serr_conflict(Txt, Lang));
	false ->
	    NewHostOpts = case maps:is_key(RemoteServer, HostOpts) of
			      true ->
				  HostOpts;
			      false ->
				  case maps:find(global, HostOpts) of
				      {ok, GlobalPass} ->
					  maps:from_list([{RemoteServer, GlobalPass}]);
				      error ->
					  HostOpts
				  end
			  end,
	    CodecOpts = ejabberd_config:codec_options(),
	    State#{host_opts => NewHostOpts, codec_options => CodecOpts}
    end.

get_password_fun(#{remote_server := RemoteServer,
		   socket := Socket, ip := IP,
		   host_opts := HostOpts}) ->
    fun(_) ->
	    case maps:find(RemoteServer, HostOpts) of
		{ok, Password} ->
		    {Password, undefined};
		error ->
		    ?WARNING_MSG("(~ts) Domain ~ts is unconfigured for "
				 "external component from ~ts",
				 [xmpp_socket:pp(Socket), RemoteServer,
				  ejabberd_config:may_hide_data(misc:ip_to_list(IP))]),
		    {false, undefined}
	    end
    end.

handle_auth_success(_, Mech, _,
		    #{remote_server := RemoteServer, host_opts := HostOpts,
		      socket := Socket, ip := IP,
		      global_routes := GlobalRoutes} = State) ->
    ?INFO_MSG("(~ts) Accepted external component ~ts authentication "
	      "for ~ts from ~ts",
	      [xmpp_socket:pp(Socket), Mech, RemoteServer,
	       ejabberd_config:may_hide_data(misc:ip_to_list(IP))]),
    Routes = if GlobalRoutes ->
		     maps:keys(HostOpts);
		true ->
		     [RemoteServer]
	     end,
    lists:foreach(
      fun(H) ->
	      ejabberd_router:register_route(H, ejabberd_config:get_myname()),
	      ejabberd_hooks:run(component_connected, [H])
      end, Routes),
    State#{routes => Routes}.

handle_auth_failure(_, Mech, Reason,
		    #{remote_server := RemoteServer,
		      socket := Socket, ip := IP} = State) ->
    ?WARNING_MSG("(~ts) Failed external component ~ts authentication "
		 "for ~ts from ~ts: ~ts",
		 [xmpp_socket:pp(Socket), Mech, RemoteServer,
		  ejabberd_config:may_hide_data(misc:ip_to_list(IP)),
		  Reason]),
    State.

handle_authenticated_packet(Pkt0, #{ip := {IP, _}, lang := Lang} = State)
  when ?is_stanza(Pkt0) ->
    Pkt = xmpp:put_meta(Pkt0, ip, IP),
    From = xmpp:get_from(Pkt),
    case check_from(From, State) of
	true ->
        {Pkt2, State2} = ejabberd_hooks:run_fold(component_send_packet, {Pkt, State}, []),
        case Pkt2 of
            drop ->
                ok;
            _ ->
                ejabberd_router:route(Pkt2)
		end,
        State2;
	false ->
	    Txt = ?T("Improper domain part of 'from' attribute"),
	    Err = xmpp:serr_invalid_from(Txt, Lang),
	    xmpp_stream_in:send(State, Err)
    end;
handle_authenticated_packet(_Pkt, State) ->
    State.

handle_info({route, Packet}, #{access := Access} = State) ->
    case acl:match_rule(global, Access, xmpp:get_from(Packet)) of
	allow ->
	    xmpp_stream_in:send(State, Packet);
	deny ->
	    Lang = xmpp:get_lang(Packet),
	    Err = xmpp:err_not_allowed(?T("Access denied by service policy"), Lang),
	    ejabberd_router:route_error(Packet, Err),
	    State
    end;
handle_info(Info, State) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    State.

terminate(Reason, #{routes := Routes}) ->
    lists:foreach(
      fun(H) ->
	      ejabberd_router:unregister_route(H),
	      ejabberd_hooks:run(component_disconnected, [H, Reason])
      end, Routes);
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec check_from(jid(), state()) -> boolean().
check_from(_From, #{check_from := false}) ->
    %% If the admin does not want to check the from field
    %% when accept packets from any address.
    %% In this case, the component can send packet of
    %% behalf of the server users.
    true;
check_from(From, #{host_opts := HostOpts}) ->
    %% The default is the standard behaviour in XEP-0114
    Server = From#jid.lserver,
    maps:is_key(Server, HostOpts).

random_password() ->
    str:sha(p1_rand:bytes(20)).

listen_opt_type(shaper_rule) ->
    econf:and_then(
      econf:shaper(),
      fun(S) ->
	      ?WARNING_MSG("Listening option 'shaper_rule' of module ~ts "
			   "is renamed to 'shaper'. Please adjust your "
			   "configuration", [?MODULE]),
	      S
      end);
listen_opt_type(check_from) ->
    econf:bool();
listen_opt_type(password) ->
    econf:binary();
listen_opt_type(hosts) ->
    econf:map(
      econf:domain(),
      econf:and_then(
	econf:options(
	  #{password => econf:binary()}),
	fun(Opts) -> proplists:get_value(password, Opts) end));
listen_opt_type(global_routes) ->
    econf:bool().

listen_options() ->
    [{access, all},
     {shaper, none},
     {shaper_rule, none},
     {certfile, undefined},
     {ciphers, undefined},
     {dhfile, undefined},
     {cafile, undefined},
     {protocol_options, undefined},
     {tls, false},
     {tls_compression, false},
     {max_stanza_size, infinity},
     {max_fsm_queue, 10000},
     {password, undefined},
     {hosts, []},
     {check_from, true},
     {global_routes, true}].
