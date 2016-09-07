%%%----------------------------------------------------------------------
%%% File    : ejabberd_service.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : External component management (XEP-0114)
%%% Created :  6 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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
%%%----------------------------------------------------------------------

-module(ejabberd_service).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-protocol({xep, 114, '1.6'}).

-define(GEN_FSM, p1_fsm).

-behaviour(?GEN_FSM).

%% External exports
-export([start/0, start/2, start_link/2, send_text/2,
	 send_element/2, socket_type/0, transform_listen_option/2]).

-export([init/1, wait_for_stream/2,
	 wait_for_handshake/2, stream_established/2,
	 handle_event/3, handle_sync_event/4, code_change/4,
	 handle_info/3, terminate/3, print_state/1, opt_type/1]).

-include("ejabberd_service.hrl").
-include("mod_privacy.hrl").

-export([get_delegated_ns/1]).

%-define(DBGFSM, true).

-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

-endif.

-define(STREAM_HEADER,
	<<"<?xml version='1.0'?><stream:stream "
	  "xmlns:stream='http://etherx.jabber.org/stream"
	  "s' xmlns='jabber:component:accept' id='~s' "
	  "from='~s'>">>).

-define(STREAM_TRAILER, <<"</stream:stream>">>).

-define(INVALID_HEADER_ERR,
	<<"<stream:stream xmlns:stream='http://etherx.ja"
	  "bber.org/streams'><stream:error>Invalid "
	  "Stream Header</stream:error></stream:stream>">>).

-define(INVALID_HANDSHAKE_ERR,
	<<"<stream:error><not-authorized xmlns='urn:ietf"
	  ":params:xml:ns:xmpp-streams'/><text "
	  "xmlns='urn:ietf:params:xml:ns:xmpp-streams' "
	  "xml:lang='en'>Invalid Handshake</text></strea"
	  "m:error></stream:stream>">>).

-define(INVALID_XML_ERR,
	fxml:element_to_binary(?SERR_XML_NOT_WELL_FORMED)).

-define(INVALID_NS_ERR,
	fxml:element_to_binary(?SERR_INVALID_NAMESPACE)).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% for xep-0355
%% table contans records like {namespace, fitering attributes, pid(),
%% host, disco info for general case, bare jid disco info }

start() ->
  ets:new(delegated_namespaces, [named_table, public]),
  ets:new(hooks_tmp, [named_table, public]).

start(SockData, Opts) ->
    supervisor:start_child(ejabberd_service_sup,
			   [SockData, Opts]).

start_link(SockData, Opts) ->
    (?GEN_FSM):start_link(ejabberd_service,
			  [SockData, Opts], fsm_limit_opts(Opts) ++ (?FSMOPTS)).

socket_type() -> xml_stream.

get_delegated_ns(FsmRef) ->
    (?GEN_FSM):sync_send_all_state_event(FsmRef, {get_delegated_ns}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%%----------------------------------------------------------------------
init([{SockMod, Socket}, Opts]) ->
    ?INFO_MSG("(~w) External service connected", [Socket]),
    Access = case lists:keysearch(access, 1, Opts) of
	       {value, {_, A}} -> A;
	       _ -> all
	     end,
    HostOpts = case lists:keyfind(hosts, 1, Opts) of
		   {hosts, HOpts} ->
		       lists:foldl(
			 fun({H, Os}, D) ->
				 P = proplists:get_value(
				       password, Os,
				       p1_sha:sha(crypto:rand_bytes(20))),
				 dict:store(H, P, D)
			 end, dict:new(), HOpts);
		   false ->
		       Pass = proplists:get_value(
				password, Opts,
				p1_sha:sha(crypto:rand_bytes(20))),
		       dict:from_list([{global, Pass}])
	       end,
    %% privilege access to entities data
    PrivAccess = case lists:keysearch(privilege_access, 1, Opts) of
		     {value, {_, PrivAcc}} -> PrivAcc;
		     _ -> []
		 end,
    Delegations = case lists:keyfind(delegations, 1, Opts) of
		      {delegations, Del} ->
			  lists:foldl(
			    fun({Ns, FiltAttr}, D) when Ns /= ?NS_DELEGATION ->
				    Attr = proplists:get_value(filtering, FiltAttr, []),
				    D ++ [{Ns, Attr}];
			       (_Deleg, D) -> D
			    end, [], Del);
		      false -> []
		  end,
    Shaper = case lists:keysearch(shaper_rule, 1, Opts) of
	       {value, {_, S}} -> S;
	       _ -> none
	     end,
    CheckFrom = case lists:keysearch(service_check_from, 1,
				     Opts)
		    of
		  {value, {_, CF}} -> CF;
		  _ -> true
		end,
    SockMod:change_shaper(Socket, Shaper),
    {ok, wait_for_stream,
     #state{socket = Socket, sockmod = SockMod,
	    streamid = new_id(), host_opts = HostOpts, access = Access,
	    check_from = CheckFrom, privilege_access = PrivAccess,
	    delegations = Delegations}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

wait_for_stream({xmlstreamstart, _Name, Attrs},
		StateData) ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
      <<"jabber:component:accept">> ->
	  To = fxml:get_attr_s(<<"to">>, Attrs),
	  Host = jid:nameprep(To),
	  if Host == error ->
		  Header = io_lib:format(?STREAM_HEADER,
					 [<<"none">>, ?MYNAME]),
		  send_text(StateData,
			    <<(list_to_binary(Header))/binary,
			      (?INVALID_XML_ERR)/binary,
			      (?STREAM_TRAILER)/binary>>),
		  {stop, normal, StateData};
	     true ->
		  Header = io_lib:format(?STREAM_HEADER,
					 [StateData#state.streamid, fxml:crypt(To)]),
		  send_text(StateData, Header),
		  HostOpts = case dict:is_key(Host, StateData#state.host_opts) of
				 true ->
				     StateData#state.host_opts;
				 false ->
				     case dict:find(global, StateData#state.host_opts) of
					 {ok, GlobalPass} ->
					     dict:from_list([{Host, GlobalPass}]);
					 error ->
					     StateData#state.host_opts
				     end
			     end,
		  {next_state, wait_for_handshake,
		   StateData#state{host = Host, host_opts = HostOpts}}
	  end;
      _ ->
	  send_text(StateData, ?INVALID_HEADER_ERR),
	  {stop, normal, StateData}
    end;
wait_for_stream({xmlstreamerror, _}, StateData) ->
    Header = io_lib:format(?STREAM_HEADER,
			   [<<"none">>, ?MYNAME]),
    send_text(StateData,
	      <<(list_to_binary(Header))/binary, (?INVALID_XML_ERR)/binary,
		(?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.

wait_for_handshake({xmlstreamelement, El}, StateData) ->
    #xmlel{name = Name, children = Els} = El,
    case {Name, fxml:get_cdata(Els)} of
      {<<"handshake">>, Digest} ->
	  case dict:find(StateData#state.host, StateData#state.host_opts) of
	      {ok, Password} ->
		  case p1_sha:sha(<<(StateData#state.streamid)/binary,
				    Password/binary>>) of
		      Digest ->
			  send_text(StateData, <<"<handshake/>">>),
			  lists:foreach(
			    fun (H) ->
				    ejabberd_router:register_route(H, ?MYNAME),
				    ?INFO_MSG("Route registered for service ~p~n",
					      [H]),
				    ejabberd_hooks:run(component_connected,
			     		[H])
			    end, dict:fetch_keys(StateData#state.host_opts)),

			  mod_privilege:advertise_permissions(StateData),
			  DelegatedNs = mod_delegation:advertise_delegations(StateData),

			  RosterAccess = proplists:get_value(roster,
							     StateData#state.privilege_access),

			  case proplists:get_value(presence,
						   StateData#state.privilege_access) of
				<<"managed_entity">> ->
				    mod_privilege:initial_presences(StateData),
				    Fun = mod_privilege:process_presence(self()),
				    add_hooks(user_send_packet, Fun);
				<<"roster">> when (RosterAccess == <<"both">>) or
						  (RosterAccess == <<"get">>) ->
				    mod_privilege:initial_presences(StateData),
				    Fun = mod_privilege:process_presence(self()),
				    add_hooks(user_send_packet, Fun),
				    Fun2 = mod_privilege:process_roster_presence(self()),
				    add_hooks(s2s_receive_packet, Fun2);
				_ -> ok
			    end,
			    {next_state, stream_established,
			     StateData#state{delegations = DelegatedNs}};
		      _ ->
			  send_text(StateData, ?INVALID_HANDSHAKE_ERR),
			  {stop, normal, StateData}
		  end;
	      _ ->
		  send_text(StateData, ?INVALID_HANDSHAKE_ERR),
		  {stop, normal, StateData}
	  end;
      _ -> {next_state, wait_for_handshake, StateData}
    end;
wait_for_handshake({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};
wait_for_handshake({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      <<(?INVALID_XML_ERR)/binary,
		(?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
wait_for_handshake(closed, StateData) ->
    {stop, normal, StateData}.

stream_established({xmlstreamelement, El}, StateData) ->
    NewEl = jlib:remove_attr(<<"xmlns">>, El),
    #xmlel{name = Name, attrs = Attrs} = NewEl,
    From = fxml:get_attr_s(<<"from">>, Attrs),
    FromJID = case StateData#state.check_from of
		%% If the admin does not want to check the from field
		%% when accept packets from any address.
		%% In this case, the component can send packet of
		%% behalf of the server users.
		false -> jid:from_string(From);
		%% The default is the standard behaviour in XEP-0114
		_ ->
		    FromJID1 = jid:from_string(From),
		    case FromJID1 of
		      #jid{lserver = Server} ->
			  case dict:is_key(Server, StateData#state.host_opts) of
			    true -> FromJID1;
			    false -> error
			  end;
		      _ -> error
		    end
	      end,
    To = fxml:get_attr_s(<<"to">>, Attrs),
    ToJID = case To of
	      <<"">> -> error;
	      _ -> jid:from_string(To)
	    end,
    if  (Name == <<"iq">>) and (ToJID /= error) and (FromJID /= error) ->
	    mod_privilege:process_iq(StateData, FromJID, ToJID, NewEl);
	(Name == <<"presence">>) and (ToJID /= error) and (FromJID /= error) ->
	    ejabberd_router:route(FromJID, ToJID, NewEl);
	(Name == <<"message">>) and (ToJID /= error) and (FromJID /= error) ->
	    mod_privilege:process_message(StateData, FromJID, ToJID, NewEl);
       true ->
	   Lang = fxml:get_tag_attr_s(<<"xml:lang">>, El),
	   Txt = <<"Incorrect stanza name or from/to JID">>,
	   Err = jlib:make_error_reply(NewEl, ?ERRT_BAD_REQUEST(Lang, Txt)),
	   send_element(StateData, Err),
	   error
    end,
    {next_state, stream_established, StateData};
stream_established({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};
stream_established({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      <<(?INVALID_XML_ERR)/binary,
		(?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
stream_established(closed, StateData) ->
    {stop, normal, StateData}.

%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
%state_name(Event, From, StateData) ->
%    Reply = ok,
%    {reply, Reply, state_name, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
handle_sync_event({get_delegated_ns}, _From, StateName, StateData) ->
    Reply =  {StateData#state.host, StateData#state.delegations},
    {reply, Reply, StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok, {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData, Text),
    {next_state, StateName, StateData};
handle_info({send_element, El}, StateName, StateData) ->
    send_element(StateData, El),
    {next_state, StateName, StateData};
handle_info({route, From, To, Packet}, StateName,
	    StateData) ->
    case acl:match_rule(global, StateData#state.access,
			From)
	of
      allow ->
	  #xmlel{name = Name, attrs = Attrs, children = Els} =
	      Packet,
	  Attrs2 =
	      jlib:replace_from_to_attrs(jid:to_string(From),
					 jid:to_string(To), Attrs),
	  Text = fxml:element_to_binary(#xmlel{name = Name,
					      attrs = Attrs2, children = Els}),
	  send_text(StateData, Text);
      deny ->
	  Lang = fxml:get_tag_attr_s(<<"xml:lang">>, Packet),
	  Txt = <<"Denied by ACL">>,
	  Err = jlib:make_error_reply(Packet, ?ERRT_NOT_ALLOWED(Lang, Txt)),
	  ejabberd_router:route_error(To, From, Err, Packet)
    end,
    {next_state, StateName, StateData};

handle_info({user_presence, Packet, From},
	    stream_established, StateData) ->
    To = jid:from_string(StateData#state.host),
    PacketNew = jlib:replace_from_to(From, To, Packet),
    send_element(StateData, PacketNew),
    {next_state, stream_established, StateData};

handle_info({roster_presence, Packet, From},
	    stream_established, StateData) ->
    %% check that current presence stanza is equivalent to last
    PresenceNew = jlib:remove_attr(<<"to">>, Packet),
    Dict = StateData#state.last_pres,
    LastPresence =
    try dict:fetch(From, Dict)
    catch _:_ ->
	      undefined
    end,
    case mod_privilege:compare_presences(LastPresence, PresenceNew) of
	false ->
	    #xmlel{attrs = Attrs} = PresenceNew,
	    Presence = PresenceNew#xmlel{attrs = [{<<"to">>, StateData#state.host} | Attrs]},
	    send_element(StateData, Presence),
	    DictNew = dict:store(From, PresenceNew, Dict),
	    StateDataNew = StateData#state{last_pres = DictNew},
	    {next_state, stream_established, StateDataNew};
	_ ->
	    {next_state, stream_established, StateData}
    end;

handle_info(Info, StateName, StateData) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
    ?INFO_MSG("terminated: ~p", [Reason]),
    case StateName of
      stream_established ->
	  lists:foreach(fun (H) ->
				ejabberd_router:unregister_route(H),
				ejabberd_hooks:run(component_disconnected,
					[StateData#state.host, Reason])
			end,
			dict:fetch_keys(StateData#state.host_opts)),

	  lists:foreach(fun({Ns, _FilterAttr}) ->
				ets:delete(delegated_namespaces, Ns),
				remove_iq_handlers(Ns)
			end, StateData#state.delegations),

	  RosterAccess = proplists:get_value(roster, StateData#state.privilege_access),
	  case proplists:get_value(presence, StateData#state.privilege_access) of
	      <<"managed_entity">> ->
		  Fun = mod_privilege:process_presence(self()),
		  remove_hooks(user_send_packet, Fun);
	      <<"roster">> when (RosterAccess == <<"both">>) or
				(RosterAccess == <<"get">>) ->
		  Fun = mod_privilege:process_presence(self()),
		  remove_hooks(user_send_packet, Fun),
		  Fun2 = mod_privilege:process_roster_presence(self()),
		  remove_hooks(s2s_receive_packet, Fun2);
	      _ -> ok
	  end;
      _ -> ok
    end,
    (StateData#state.sockmod):close(StateData#state.socket),
    ok.

%%----------------------------------------------------------------------
%% Func: print_state/1
%% Purpose: Prepare the state to be printed on error log
%% Returns: State to print
%%----------------------------------------------------------------------
print_state(State) -> State.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

send_text(StateData, Text) ->
    (StateData#state.sockmod):send(StateData#state.socket,
				   Text).

send_element(StateData, El) ->
    send_text(StateData, fxml:element_to_binary(El)).

new_id() -> randoms:get_string().

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

remove_iq_handlers(Ns) ->
    lists:foreach(fun(Host) ->
                      gen_iq_handler:remove_iq_handler(ejabberd_local, Host, Ns),
                      gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, Ns)
                  end, ?MYHOSTS).

add_hooks(Hook, Fun) ->
  lists:foreach(fun(Host) ->
                    ejabberd_hooks:add(Hook, Host,Fun, 100)
                end, ?MYHOSTS).

remove_hooks(Hook, Fun) ->
  lists:foreach(fun(Host) ->
                    ejabberd_hooks:delete(Hook, Host, Fun, 100)
                end, ?MYHOSTS).
