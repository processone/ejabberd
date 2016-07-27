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
-export([start/2, start_link/2, send_text/2,
	 send_element/2, socket_type/0, transform_listen_option/2]).

-export([init/1, wait_for_stream/2,
	 wait_for_handshake/2, stream_established/2,
	 handle_event/3, handle_sync_event/4, code_change/4,
	 handle_info/3, terminate/3, print_state/1, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-record(state,
	{socket                    :: ejabberd_socket:socket_state(),
         sockmod = ejabberd_socket :: ejabberd_socket | ejabberd_frontend_socket,
         streamid = <<"">>         :: binary(),
         host_opts = dict:new()    :: ?TDICT,
         host = <<"">>             :: binary(),
         access                    :: atom(),
	 check_from = true         :: boolean()}).

-type state_name() :: wait_for_stream | wait_for_handshake | stream_established.
-type state() :: #state{}.
-type fsm_next() :: {next_state, state_name(), state()}.
-type fsm_stop() :: {stop, normal, state()}.
-type fsm_transition() :: fsm_stop() | fsm_next().

%-define(DBGFSM, true).
-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(SockData, Opts) ->
    supervisor:start_child(ejabberd_service_sup,
			   [SockData, Opts]).

start_link(SockData, Opts) ->
    (?GEN_FSM):start_link(ejabberd_service,
			  [SockData, Opts], fsm_limit_opts(Opts) ++ (?FSMOPTS)).

socket_type() -> xml_stream.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------
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
	    streamid = new_id(), host_opts = HostOpts,
	    access = Access, check_from = CheckFrom}}.

wait_for_stream({xmlstreamstart, Name, Attrs}, StateData) ->
    try xmpp:decode(#xmlel{name = Name, attrs = Attrs}) of
	#stream_start{xmlns = ?NS_COMPONENT, to = To} when is_record(To, jid) ->
	    Host = To#jid.lserver,
	    send_header(StateData, To),
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
	     StateData#state{host = Host, host_opts = HostOpts}};
	#stream_start{xmlns = ?NS_COMPONENT} ->
	    send_header(StateData, ?MYNAME),
	    send_element(StateData, xmpp:serr_improper_addressing()),
	    {stop, normal, StateData};
	#stream_start{} ->
	    send_header(StateData, ?MYNAME),
	    send_element(StateData, xmpp:serr_invalid_namespace()),
	    {stop, normal, StateData}
    catch _:{xmpp_codec, Why} ->
	    Txt = xmpp:format_error(Why),
	    send_header(StateData, ?MYNAME),
	    send_element(StateData, xmpp:serr_invalid_xml(Txt, ?MYLANG)),
	    {stop, normal, StateData}
    end;
wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_header(StateData, ?MYNAME),
    send_element(StateData, xmpp:serr_not_well_formed()),
    {stop, normal, StateData};
wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.

wait_for_handshake({xmlstreamelement, El}, StateData) ->
    decode_element(El, wait_for_handshake, StateData);
wait_for_handshake(#handshake{data = Digest}, StateData) ->
    case dict:find(StateData#state.host, StateData#state.host_opts) of
	{ok, Password} ->
	    case p1_sha:sha(<<(StateData#state.streamid)/binary,
			      Password/binary>>) of
		Digest ->
		    send_element(StateData, #handshake{}),
		    lists:foreach(
		      fun (H) ->
			      ejabberd_router:register_route(H, ?MYNAME),
			      ?INFO_MSG("Route registered for service ~p~n",
					[H])
		      end, dict:fetch_keys(StateData#state.host_opts)),
		    {next_state, stream_established, StateData};
		_ ->
		    send_element(StateData, xmpp:serr_not_authorized()),
		    {stop, normal, StateData}
	    end;
	_ ->
	    send_element(StateData, xmpp:serr_not_authorized()),
	    {stop, normal, StateData}
    end;
wait_for_handshake({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};
wait_for_handshake({xmlstreamerror, _}, StateData) ->
    send_element(StateData, xmpp:serr_not_well_formed()),
    {stop, normal, StateData};
wait_for_handshake(closed, StateData) ->
    {stop, normal, StateData};
wait_for_handshake(_Pkt, StateData) ->
    {next_state, wait_for_handshake, StateData}.

stream_established({xmlstreamelement, El}, StateData) ->
    decode_element(El, stream_established, StateData);
stream_established(El, StateData) when ?is_stanza(El) ->
    From = xmpp:get_from(El),
    To = xmpp:get_to(El),
    Lang = xmpp:get_lang(El),
    if From == undefined orelse To == undefined ->
	    send_error(StateData, El, xmpp:err_jid_malformed());
       true ->
	    FromJID = case StateData#state.check_from of
			  false ->
			      %% If the admin does not want to check the from field
			      %% when accept packets from any address.
			      %% In this case, the component can send packet of
			      %% behalf of the server users.
			      From;
			  _ ->
			      %% The default is the standard behaviour in XEP-0114
			      case From of
				  #jid{lserver = Server} ->
				      case dict:is_key(Server, StateData#state.host_opts) of
					  true -> From;
					  false -> error
				      end;
				  _ -> error
			      end
		      end,
	    if FromJID /= error ->
		    ejabberd_router:route(FromJID, To, El);
	       true ->
		    Txt = <<"Incorrect value of 'from' or 'to' attribute">>,
		    send_error(StateData, El, xmpp:err_not_allowed(Txt, Lang))
	    end
    end,
    {next_state, stream_established, StateData};
stream_established({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};
stream_established({xmlstreamerror, _}, StateData) ->
    send_element(StateData, xmpp:serr_not_well_formed()),
    {stop, normal, StateData};
stream_established(closed, StateData) ->
    {stop, normal, StateData};
stream_established(_Event, StateData) ->
    {next_state, stream_established, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName,
		  StateData) ->
    Reply = ok, {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData, Text),
    {next_state, StateName, StateData};
handle_info({send_element, El}, StateName, StateData) ->
    send_element(StateData, El),
    {next_state, StateName, StateData};
handle_info({route, From, To, Packet}, StateName,
	    StateData) ->
    case acl:match_rule(global, StateData#state.access, From) of
      allow ->
	    Pkt = xmpp:set_from_to(Packet, From, To),
	    send_element(StateData, Pkt);
	deny ->
	    Lang = xmpp:get_lang(Packet),
	    Err = xmpp:err_not_allowed(<<"Denied by ACL">>, Lang),
	    ejabberd_router:route_error(To, From, Packet, Err)
    end,
    {next_state, StateName, StateData};
handle_info(Info, StateName, StateData) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    {next_state, StateName, StateData}.

terminate(Reason, StateName, StateData) ->
    ?INFO_MSG("terminated: ~p", [Reason]),
    case StateName of
      stream_established ->
	  lists:foreach(fun (H) ->
				ejabberd_router:unregister_route(H)
			end,
			dict:fetch_keys(StateData#state.host_opts));
      _ -> ok
    end,
    catch send_trailer(StateData),
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

-spec send_text(state(), iodata()) -> ok.
send_text(StateData, Text) ->
    (StateData#state.sockmod):send(StateData#state.socket,
				   Text).

-spec send_element(state(), xmpp_element()) -> ok.
send_element(StateData, El) ->
    El1 = fix_ns(xmpp:encode(El)),
    send_text(StateData, fxml:element_to_binary(El1)).

-spec send_error(state(), xmlel() | stanza(), error()) -> ok.
send_error(StateData, Stanza, Error) ->
    Type = xmpp:get_type(Stanza),
    if Type == error; Type == result;
       Type == <<"error">>; Type == <<"result">> ->
	    ok;
       true ->
	    send_element(StateData, xmpp:make_error(Stanza, Error))
    end.

-spec send_header(state(), binary()) -> ok.
send_header(StateData, Host) ->
    send_text(StateData,
	      io_lib:format(
		<<"<?xml version='1.0'?><stream:stream "
		  "xmlns:stream='http://etherx.jabber.org/stream"
		  "s' xmlns='jabber:component:accept' id='~s' "
		  "from='~s'>">>,
		[StateData#state.streamid, fxml:crypt(Host)])).

-spec send_trailer(state()) -> ok.
send_trailer(StateData) ->
    send_text(StateData, <<"</stream:stream>">>).

-spec fix_ns(xmlel()) -> xmlel().
fix_ns(#xmlel{name = Name} = El) when Name == <<"message">>;
                                      Name == <<"iq">>;
                                      Name == <<"presence">> ->
    Attrs = lists:filter(
              fun({<<"xmlns">>, _}) -> false;
                 (_) -> true
              end, El#xmlel.attrs),
    El#xmlel{attrs = Attrs};
fix_ns(El) ->
    El.

-spec decode_element(xmlel(), state_name(), state()) -> fsm_transition().
decode_element(#xmlel{} = El, StateName, StateData) ->
    try xmpp:decode(El, [ignore_els]) of
	Pkt -> ?MODULE:StateName(Pkt, StateData)
    catch error:{xmpp_codec, Why} ->
            case xmpp:is_stanza(El) of
                true ->
                    Lang = xmpp:get_lang(El),
                    Txt = xmpp:format_error(Why),
                    send_error(StateData, El, xmpp:err_bad_request(Txt, Lang));
                false ->
                    ok
            end,
            {next_state, StateName, StateData}
    end.

-spec new_id() -> binary().
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
