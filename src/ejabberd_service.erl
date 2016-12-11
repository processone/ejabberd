%%%-------------------------------------------------------------------
%%% Created : 11 Dec 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
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
-behaviour(ejabberd_config).

-protocol({xep, 114, '1.6'}).

%% ejabberd_socket callbacks
-export([start/2, socket_type/0]).
%% ejabberd_config callbacks
-export([opt_type/1, transform_listen_option/2]).
%% xmpp_stream_in callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([handshake/2, handle_stream_start/1, handle_authenticated_packet/2]).
%% API
-export([send/2]).

-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").

%%-define(DBGFSM, true).
-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-type state() :: map().
-type next_state() :: {noreply, state()} | {stop, term(), state()}.
-export_type([state/0, next_state/0]).

%%%===================================================================
%%% API
%%%===================================================================
start(SockData, Opts) ->
    xmpp_stream_in:start(?MODULE, [SockData, Opts],
			 fsm_limit_opts(Opts) ++ ?FSMOPTS).

socket_type() ->
    xml_stream.

-spec send(state(), xmpp_element()) -> next_state().
send(State, Pkt) ->
    xmpp_stream_in:send(State, Pkt).

%%%===================================================================
%%% xmpp_stream_in callbacks
%%%===================================================================
init([#{socket := Socket} = State, Opts]) ->
    ?INFO_MSG("(~w) External service connected", [Socket]),
    Access = gen_mod:get_opt(access, Opts, fun acl:access_rules_validator/1, all),
    Shaper = gen_mod:get_opt(shaper_rule, Opts, fun acl:shaper_rules_validator/1, none),
    HostOpts = case lists:keyfind(hosts, 1, Opts) of
		   {hosts, HOpts} ->
		       lists:foldl(
			 fun({H, Os}, D) ->
				 P = proplists:get_value(
				       password, Os,
				       p1_sha:sha(randoms:bytes(20))),
				 dict:store(H, P, D)
			 end, dict:new(), HOpts);
		   false ->
		       Pass = proplists:get_value(
				password, Opts,
				p1_sha:sha(randoms:bytes(20))),
		       dict:from_list([{global, Pass}])
	       end,
    CheckFrom = gen_mod:get_opt(check_from, Opts,
				fun(Flag) when is_boolean(Flag) -> Flag end),
    xmpp_stream_in:change_shaper(State, Shaper),
    State1 = State#{access => Access,
		    xmlns => ?NS_COMPONENT,
		    lang => ?MYLANG,
		    server => ?MYNAME,
		    host_opts => HostOpts,
		    check_from => CheckFrom},
    ejabberd_hooks:run_fold(component_init, {ok, State1}, []).

handle_stream_start(#{remote_server := RemoteServer,
		      host_opts := HostOpts} = State) ->
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
    {noreply, State#{host_opts => NewHostOpts}}.

handshake(Digest, #{remote_server := RemoteServer,
		    stream_id := StreamID,
		    host_opts := HostOpts} = State) ->
    case dict:find(RemoteServer, HostOpts) of
    	{ok, Password} ->
	    case p1_sha:sha(<<StreamID/binary, Password/binary>>) of
    		Digest ->
    		    lists:foreach(
    		      fun (H) ->
    			      ejabberd_router:register_route(H, ?MYNAME),
			      ?INFO_MSG("Route registered for service ~p~n", [H]),
    			      ejabberd_hooks:run(component_connected, [H])
		      end, dict:fetch_keys(HostOpts)),
		    {ok, State};
		_ ->
		    ?ERROR_MSG("Failed authentication for service ~s", [RemoteServer]),
		    {error, xmpp:serr_not_authorized(), State}
	    end;
	_ ->
	    ?ERROR_MSG("Failed authentication for service ~s", [RemoteServer]),
	    {error, xmpp:serr_not_authorized(), State}
    end.

handle_authenticated_packet(Pkt, #{lang := Lang} = State) ->
    From = xmpp:get_from(Pkt),
    case check_from(From, State) of
	true ->
	    To = xmpp:get_to(Pkt),
	    ejabberd_router:route(From, To, Pkt),
	    {noreply, State};
	false ->
	    Txt = <<"Improper domain part of 'from' attribute">>,
	    Err = xmpp:serr_invalid_from(Txt, Lang),
	    xmpp_stream_in:send(State, Err)
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({route, From, To, Packet}, #{access := Access} = State) ->
    case acl:match_rule(global, Access, From) of
	allow ->
	    Pkt = xmpp:set_from_to(Packet, From, To),
	    xmpp_stream_in:send(State, Pkt);
	deny ->
	    Lang = xmpp:get_lang(Packet),
	    Err = xmpp:err_not_allowed(<<"Denied by ACL">>, Lang),
	    ejabberd_router:route_error(To, From, Packet, Err),
	    {noreply, State}
    end;
handle_info(Info, State) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, #{stream_state := StreamState, host_opts := HostOpts}) ->
    ?INFO_MSG("External service disconnected: ~p", [Reason]),
    case StreamState of
	session_established ->
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

fsm_limit_opts(Opts) ->
    case lists:keysearch(max_fsm_queue, 1, Opts) of
        {value, {_, N}} when is_integer(N) ->
            [{max_queue, N}];
        _ ->
            case ejabberd_config:get_option(
                   max_fsm_queue,
                   fun(I) when is_integer(I), I > 0 -> I end) of
                undefined -> [];
                N -> [{max_queue, N}]
            end
    end.

opt_type(max_fsm_queue) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(_) -> [max_fsm_queue].
