%%%-------------------------------------------------------------------
%%% Created : 11 Dec 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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
-behaviour(xmpp_socket).

-protocol({xep, 114, '1.6'}).

%% xmpp_socket callbacks
-export([start/2, start_link/2, socket_type/0, close/1, close/2]).
%% ejabberd_listener callbacks
-export([listen_opt_type/1, transform_listen_option/2]).
%% xmpp_stream_in callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3]).
-export([handle_stream_start/2, handle_auth_success/4, handle_auth_failure/4,
	 handle_authenticated_packet/2, get_password_fun/1, tls_options/1]).
%% API
-export([send/2]).

-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").

-type state() :: map().
-export_type([state/0]).

%%%===================================================================
%%% API
%%%===================================================================
start(SockData, Opts) ->
    xmpp_stream_in:start(?MODULE, [SockData, Opts],
			 ejabberd_config:fsm_limit_opts(Opts)).

start_link(SockData, Opts) ->
    xmpp_stream_in:start_link(?MODULE, [SockData, Opts],
			      ejabberd_config:fsm_limit_opts(Opts)).

socket_type() ->
    xml_stream.

-spec send(pid(), xmpp_element()) -> ok;
	  (state(), xmpp_element()) -> state().
send(Stream, Pkt) ->
    xmpp_stream_in:send(Stream, Pkt).

-spec close(pid()) -> ok;
	   (state()) -> state().
close(Ref) ->
    xmpp_stream_in:close(Ref).

-spec close(pid(), atom()) -> ok;
	   (state(), atom()) -> state().
close(Ref, Reason) ->
    xmpp_stream_in:close(Ref, Reason).

%%%===================================================================
%%% xmpp_stream_in callbacks
%%%===================================================================
tls_options(#{tls_options := TLSOptions}) ->
    TLSOptions.

init([State, Opts]) ->
    Access = proplists:get_value(access, Opts, all),
    Shaper = proplists:get_value(shaper_rule, Opts, none),
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
    State1 = xmpp_stream_in:change_shaper(State, Shaper),
    State2 = State1#{access => Access,
		    xmlns => ?NS_COMPONENT,
		    lang => ?MYLANG,
		    server => ?MYNAME,
		    host_opts => dict:from_list(HostOpts1),
		    stream_version => undefined,
		    tls_options => TLSOpts,
		    check_from => CheckFrom},
    ejabberd_hooks:run_fold(component_init, {ok, State2}, [Opts]).

handle_stream_start(_StreamStart,
		    #{remote_server := RemoteServer,
		      lang := Lang,
		      host_opts := HostOpts} = State) ->
    case ejabberd_router:is_my_host(RemoteServer) of
			   true ->
	    Txt = <<"Unable to register route on existing local domain">>,
	    xmpp_stream_in:send(State, xmpp:serr_conflict(Txt, Lang));
			   false ->
	    NewHostOpts = case dict:is_key(RemoteServer, HostOpts) of
			      true ->
				  HostOpts;
			      false ->
				  case dict:find(global, HostOpts) of
				   {ok, GlobalPass} ->
					  dict:from_list([{RemoteServer, GlobalPass}]);
				   error ->
					  HostOpts
			       end
		       end,
	    State#{host_opts => NewHostOpts}
    end.

get_password_fun(#{remote_server := RemoteServer,
		   socket := Socket, ip := IP,
		   host_opts := HostOpts}) ->
    fun(_) ->
	    case dict:find(RemoteServer, HostOpts) of
    	{ok, Password} ->
		    {Password, undefined};
		error ->
		    ?INFO_MSG("(~s) Domain ~s is unconfigured for "
			      "external component from ~s",
			      [xmpp_socket:pp(Socket), RemoteServer,
			       ejabberd_config:may_hide_data(misc:ip_to_list(IP))]),
		    {false, undefined}
	    end
    end.

handle_auth_success(_, Mech, _,
		    #{remote_server := RemoteServer, host_opts := HostOpts,
		      socket := Socket, ip := IP} = State) ->
    ?INFO_MSG("(~s) Accepted external component ~s authentication "
	      "for ~s from ~s",
	      [xmpp_socket:pp(Socket), Mech, RemoteServer,
	       ejabberd_config:may_hide_data(misc:ip_to_list(IP))]),
    		    lists:foreach(
    		      fun (H) ->
    			      ejabberd_router:register_route(H, ?MYNAME),
    			      ejabberd_hooks:run(component_connected, [H])
      end, dict:fetch_keys(HostOpts)),
    State.

handle_auth_failure(_, Mech, Reason,
		    #{remote_server := RemoteServer,
		      socket := Socket, ip := IP} = State) ->
    ?INFO_MSG("(~s) Failed external component ~s authentication "
	      "for ~s from ~s: ~s",
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
	    ejabberd_router:route(Pkt),
	    State;
		false ->
		    Txt = <<"Improper domain part of 'from' attribute">>,
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
	    Err = xmpp:err_not_allowed(<<"Access denied by service policy">>, Lang),
	    ejabberd_router:route_error(Packet, Err),
	    State
    end;
handle_info(Info, State) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    State.

terminate(Reason, #{stream_state := StreamState, host_opts := HostOpts}) ->
    case StreamState of
	established ->
	    lists:foreach(
	      fun(H) ->
				ejabberd_router:unregister_route(H),
		      ejabberd_hooks:run(component_disconnected, [H, Reason])
	      end, dict:fetch_keys(HostOpts));
	_ ->
                    ok
    end.

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
    dict:is_key(Server, HostOpts).

random_password() ->
    str:sha(randoms:bytes(20)).

transform_listen_option({hosts, Hosts, O}, Opts) ->
    case lists:keyfind(hosts, 1, Opts) of
        {_, PrevHostOpts} ->
            NewHostOpts =
                lists:foldl(
                  fun(H, Acc) ->
                          dict:append_list(H, O, Acc)
                  end, dict:from_list(PrevHostOpts), Hosts),
            [{hosts, dict:to_list(NewHostOpts)}|
             lists:keydelete(hosts, 1, Opts)];
        _ ->
            [{hosts, [{H, O} || H <- Hosts]}|Opts]
    end;
transform_listen_option({host, Host, Os}, Opts) ->
    transform_listen_option({hosts, [Host], Os}, Opts);
transform_listen_option(Opt, Opts) ->
    [Opt|Opts].

-spec listen_opt_type(access) -> fun((any()) -> any());
		     (shaper_rule) -> fun((any()) -> any());
		     (certfile) -> fun((binary()) -> binary());
		     (ciphers) -> fun((binary()) -> binary());
		     (dhfile) -> fun((binary()) -> binary());
		     (cafile) -> fun((binary()) -> binary());
		     (protocol_options) -> fun(([binary()]) -> binary());
		     (tls_compression) -> fun((boolean()) -> boolean());
		     (tls) -> fun((boolean()) -> boolean());
		     (check_from) -> fun((boolean()) -> boolean());
		     (password) -> fun((boolean()) -> boolean());
		     (hosts) -> fun(([{binary(), [{password, binary()}]}]) ->
				     [{binary(), binary() | undefined}]);
		     (max_stanza_type) -> fun((timeout()) -> timeout());
		     (max_fsm_queue) -> fun((pos_integer()) -> pos_integer());
		     (atom()) -> [atom()].
listen_opt_type(access) -> fun acl:access_rules_validator/1;
listen_opt_type(shaper_rule) -> fun acl:shaper_rules_validator/1;
listen_opt_type(certfile) ->
    fun(S) ->
	    ejabberd_pkix:add_certfile(S),
	    iolist_to_binary(S)
    end;
listen_opt_type(ciphers) -> fun iolist_to_binary/1;
listen_opt_type(dhfile) -> fun misc:try_read_file/1;
listen_opt_type(cafile) -> fun misc:try_read_file/1;
listen_opt_type(protocol_options) ->
    fun(Options) -> str:join(Options, <<"|">>) end;
listen_opt_type(tls_compression) -> fun(B) when is_boolean(B) -> B end;
listen_opt_type(tls) -> fun(B) when is_boolean(B) -> B end;
listen_opt_type(check_from) -> fun(B) when is_boolean(B) -> B end;
listen_opt_type(password) -> fun iolist_to_binary/1;
listen_opt_type(hosts) ->
    fun(HostOpts) ->
	    lists:map(
	      fun({Host, Opts}) ->
		      Password = case proplists:get_value(password, Opts) of
				     undefined -> undefined;
				     P -> iolist_to_binary(P)
				 end,
		      {iolist_to_binary(Host), Password}
	      end, HostOpts)
    end;
listen_opt_type(max_stanza_size) ->
    fun(I) when is_integer(I) -> I;
       (unlimited) -> infinity;
       (infinity) -> infinity
    end;
listen_opt_type(max_fsm_queue) ->
    fun(I) when is_integer(I), I>0 -> I end;
listen_opt_type(_) ->
    [access, shaper_rule, certfile, ciphers, dhfile, cafile, tls,
     protocol_options, tls_compression, password, hosts, check_from,
     max_fsm_queue].
