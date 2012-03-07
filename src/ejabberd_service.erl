%%%----------------------------------------------------------------------
%%% File    : ejabberd_service.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : External component management (XEP-0114)
%%% Created :  6 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_service).
-author('alexey@process-one.net').

-define(GEN_FSM, p1_fsm).

-behaviour(?GEN_FSM).

%% External exports
-export([start/2,
	 start_link/2,
	 send_text/2,
	 send_element/2,
	 socket_type/0]).

%% gen_fsm callbacks
-export([init/1,
	 wait_for_stream/2,
	 wait_for_handshake/2,
	 stream_established/2,
	 handle_event/3,
	 handle_sync_event/4,
	 code_change/4,
	 handle_info/3,
	 terminate/3,
     print_state/1]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").

-record(state, {socket, sockmod, streamid,
		hosts, password, access,
		check_from}).

%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

% These are the namespace already declared by the stream opening. This is
% used at serialization time.
-define(DEFAULT_NS, ?NS_COMPONENT_ACCEPT).
-define(PREFIXED_NS, [{?NS_XMPP, ?NS_XMPP_pfx}]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(SockData, Opts) ->
    supervisor:start_child(ejabberd_service_sup, [SockData, Opts]).

start_link(SockData, Opts) ->
    ?GEN_FSM:start_link(ejabberd_service, [SockData, Opts],
			fsm_limit_opts(Opts) ++ ?FSMOPTS).

socket_type() ->
    xml_stream.

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
    {Hosts, Password} =
	case lists:keysearch(hosts, 1, Opts) of
	    {value, {_, Hs, HOpts}} ->
		case lists:keysearch(password, 1, HOpts) of
		    {value, {_, P}} ->
			{Hs, P};
		    _ ->
			% TODO: generate error
			false
		end;
	    _ ->
		case lists:keysearch(host, 1, Opts) of
		    {value, {_, H, HOpts}} ->
			case lists:keysearch(password, 1, HOpts) of
			    {value, {_, P}} ->
				{[H], P};
			    _ ->
				% TODO: generate error
				false
			end;
		    _ ->
			% TODO: generate error
			false
		end
	end,
    Shaper = case lists:keysearch(shaper_rule, 1, Opts) of
		 {value, {_, S}} -> S;
		 _ -> none
	     end,
    CheckFrom = case lists:keysearch(service_check_from, 1, Opts) of
		 {value, {_, CF}} -> CF;
		 _ -> true
	     end,
    SockMod:change_shaper(Socket, Shaper),
    {ok, wait_for_stream, #state{socket = Socket,
				 sockmod = SockMod,
				 streamid = new_id(),
				 hosts = [list_to_binary(H) || H <- Hosts],
				 password = Password,
				 access = Access,
				 check_from = CheckFrom
				 }}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

wait_for_stream({xmlstreamstart, El = #xmlel{ns = _NS, attrs = Attrs}}, StateData) ->
    case exmpp_xml:is_ns_declared_here(El, ?NS_COMPONENT_ACCEPT) of
	true ->
	    %% Note: XEP-0114 requires to check that destination is a Jabber
	    %% component served by this Jabber server.
	    %% However several transports don't respect that,
	    %% so ejabberd doesn't check 'to' attribute (EJAB-717)
	    To = exmpp_stanza:get_recipient_from_attrs(Attrs),
	    Opening_Reply = exmpp_stream:opening_reply(To,
	      ?NS_COMPONENT_ACCEPT,
	      {0, 0}, StateData#state.streamid),
	    send_element(StateData, Opening_Reply),
	    {next_state, wait_for_handshake, StateData};
	false ->
	    Error = #xmlel{ns = ?NS_XMPP, name = 'stream', children = [
		#xmlel{ns = ?NS_XMPP, name = 'error', children = [
		    #xmlcdata{cdata = <<"Invalid Stream Header">>}
		  ]}
	      ]},
	    send_element(StateData, Error),
	    {stop, normal, StateData}
    end;

wait_for_stream({xmlstreamerror, _}, StateData) ->
    Opening_Reply = exmpp_stream:opening_reply(?MYNAME,
      ?NS_COMPONENT_ACCEPT,
      {0, 0}, "none"),
    send_element(StateData, Opening_Reply),
    send_element(StateData, exmpp_stream:error('xml-not-well-formed')),
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_handshake({xmlstreamelement, El}, StateData) ->
    case {El#xmlel.name, exmpp_xml:get_cdata_as_list(El)} of
	{'handshake', Digest} ->
	    case sha:sha(StateData#state.streamid ++
			 StateData#state.password) of
		Digest ->
		    send_element(StateData,
		      #xmlel{ns = ?NS_COMPONENT_ACCEPT, name = 'handshake'}),
		    lists:foreach(
		      fun(H) ->
			      ejabberd_router:register_route(binary_to_list(H)),
			      ?INFO_MSG("Route registered for service ~p~n", [H])
		      end, StateData#state.hosts),
		    {next_state, stream_established, StateData};
		 _ ->
		    TextEl =
			#xmlel{ns = ?NS_STANZA_ERRORS,
			       name = 'text',
			       children =
			       [#xmlcdata{cdata = <<"Invalid Handshake">>}]
			      },
		    NotAuthorizedEl =
			#xmlel{ns = ?NS_STANZA_ERRORS,
			       name = 'not-authorized',
			       children = [TextEl]
			      },
		    InvalidHandshakeEl =
			#xmlel{ns = ?NS_XMPP,
			       name = 'error',
			       children = [NotAuthorizedEl]
			      },
		    send_element(StateData, InvalidHandshakeEl),
		    send_element(StateData, exmpp_stream:closing()),
		    {stop, normal, StateData}
	    end;
	_ ->
	    {next_state, wait_for_handshake, StateData}
    end;

wait_for_handshake({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};

wait_for_handshake({xmlstreamerror, _}, StateData) ->
    send_element(StateData, exmpp_stream:error('xml-not-well-formed')),
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

wait_for_handshake(closed, StateData) ->
    {stop, normal, StateData}.


stream_established({xmlstreamelement, El}, StateData) ->
    From = exmpp_stanza:get_sender(El),
    FromJID = case StateData#state.check_from of
		  %% If the admin does not want to check the from field
		  %% when accept packets from any address.
		  %% In this case, the component can send packet of
		  %% behalf of the server users.
		  false -> exmpp_jid:parse(From);
		  %% The default is the standard behaviour in XEP-0114
		  _ ->
		      FromJID1 = exmpp_jid:parse(From),
		      Server =  exmpp_jid:prep_domain(FromJID1),
			  case lists:member(Server, StateData#state.hosts) of
				  true -> FromJID1;
				  false -> error
			  end
	      end,
    To = exmpp_stanza:get_recipient(El),
    ToJID = case To of
		undefined -> error;
		_ -> exmpp_jid:parse(To)
	    end,
    if ((El#xmlel.name == 'iq') or
	(El#xmlel.name == 'message') or
	(El#xmlel.name == 'presence')) and
       (ToJID /= error) and (FromJID /= error) ->
	    ejabberd_router:route(FromJID, ToJID, El);
       true ->
	    Err = exmpp_stanza:reply_with_error(El, 'bad-request'),
	    send_element(StateData, Err),
	    error
    end,
    {next_state, stream_established, StateData};

stream_established({xmlstreamend, _Name}, StateData) ->
    % TODO
    {stop, normal, StateData};

stream_established({xmlstreamerror, _}, StateData) ->
    send_element(StateData, exmpp_stream:error('xml-not-well-formed')),
    send_element(StateData, exmpp_stream:closing()),
    {stop, normal, StateData};

stream_established(closed, StateData) ->
    % TODO
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
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info({send_text, Text}, StateName, StateData) ->
    % XXX OLD FORMAT: This clause should be removed.
    send_text(StateData, Text),
    {next_state, StateName, StateData};
handle_info({send_element, El}, StateName, StateData) ->
    io:format("ejabberd_service send_element ~p~n",[ El]),
    send_element(StateData, El),
    {next_state, StateName, StateData};
handle_info({route, From, To, Packet}, StateName, StateData) ->
    case acl:match_rule(global, StateData#state.access, From) of
	allow ->
	    El1 = exmpp_stanza:set_sender(Packet, From),
	    El2 = exmpp_stanza:set_recipient(El1, To),
	    send_element(StateData, El2);
	deny ->
	    Err = exmpp_stanza:reply_with_error(Packet, 'not-allowed'),
	    ejabberd_router:route_error(To, From, Err, Packet)
    end,
    {next_state, StateName, StateData};
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
	    lists:foreach(
	      fun(H) ->
		      ejabberd_router:unregister_route(binary_to_list(H))
	      end, StateData#state.hosts);
	_ ->
	    ok
    end,
    (StateData#state.sockmod):close(StateData#state.socket),
    ok.

%%----------------------------------------------------------------------
%% Func: print_state/1
%% Purpose: Prepare the state to be printed on error log
%% Returns: State to print
%%----------------------------------------------------------------------
print_state(State) ->
   State.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

send_text(StateData, Text) ->
    io:format(">>~n ~s ~n", [Text]),
    (StateData#state.sockmod):send(StateData#state.socket, Text).

send_element(StateData, #xmlel{ns = ?NS_XMPP, name = 'stream'} = El) ->
    send_text(StateData, exmpp_stream:to_iolist(El));
send_element(StateData, El) ->
    send_text(StateData, exmpp_stanza:to_iolist(El)).

new_id() ->
    randoms:get_string().

fsm_limit_opts(Opts) ->
    case lists:keysearch(max_fsm_queue, 1, Opts) of
	{value, {_, N}} when is_integer(N) ->
	    [{max_queue, N}];
	_ ->
	    case ejabberd_config:get_local_option(max_fsm_queue) of
		N when is_integer(N) ->
		    [{max_queue, N}];
		_ ->
		    []
	    end
    end.
