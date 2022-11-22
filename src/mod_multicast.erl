%%%----------------------------------------------------------------------
%%% File    : mod_multicast.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Extended Stanza Addressing (XEP-0033) support
%%% Created : 29 May 2007 by Badlop <badlop@process-one.net>
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

-module(mod_multicast).

-author('badlop@process-one.net').

-protocol({xep, 33, '1.1', '15.04', "", ""}).

-behaviour(gen_server).

-behaviour(gen_mod).

%% API
-export([start/2, stop/1, reload/3,
	 user_send_packet/1]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3,
	 handle_cast/2, terminate/2, code_change/3]).

-export([purge_loop/1, mod_opt_type/1, mod_options/1, depends/2, mod_doc/0]).

-include("logger.hrl").
-include("translate.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-record(multicastc, {rserver :: binary(),
		     response,
		     ts :: integer()}).

-type limit_value() :: {default | custom, integer()}.
-record(limits, {message :: limit_value(),
		 presence :: limit_value()}).

-record(service_limits, {local :: #limits{},
			 remote :: #limits{}}).

-record(state, {lserver :: binary(),
		lservice :: binary(),
		access :: atom(),
		service_limits :: #service_limits{}}).
-type state() :: #state{}.

%% All the elements are of type value()

-define(PURGE_PROCNAME,
	ejabberd_mod_multicast_purgeloop).

-define(MAXTIME_CACHE_POSITIVE, 86400).

-define(MAXTIME_CACHE_NEGATIVE, 86400).

-define(MAXTIME_CACHE_NEGOTIATING, 600).

-define(CACHE_PURGE_TIMER, 86400000).

-define(DEFAULT_LIMIT_LOCAL_MESSAGE, 100).

-define(DEFAULT_LIMIT_LOCAL_PRESENCE, 100).

-define(DEFAULT_LIMIT_REMOTE_MESSAGE, 20).

-define(DEFAULT_LIMIT_REMOTE_PRESENCE, 20).

start(LServerS, Opts) ->
    gen_mod:start_child(?MODULE, LServerS, Opts).

stop(LServerS) ->
    gen_mod:stop_child(?MODULE, LServerS).

reload(LServerS, NewOpts, OldOpts) ->
    Proc = gen_mod:get_module_proc(LServerS, ?MODULE),
    gen_server:cast(Proc, {reload, NewOpts, OldOpts}).

-define(SETS, gb_sets).

user_send_packet({#presence{} = Packet, C2SState} = Acc) ->
    case xmpp:get_subtag(Packet, #addresses{}) of
        #addresses{list = Addresses} ->
            {CC, BCC, _Invalid, _Delivered} = partition_addresses(Addresses),
            NewState =
                lists:foldl(
                  fun(Address, St) ->
                          case Address#address.jid of
                              #jid{} = JID ->
                                  LJID = jid:tolower(JID),
                                  #{pres_a := PresA} = St,
                                  A =
                                      case Packet#presence.type of
                                          available ->
                                              ?SETS:add_element(LJID, PresA);
                                          unavailable ->
                                              ?SETS:del_element(LJID, PresA);
                                          _ ->
                                              PresA
                                      end,
                                  St#{pres_a => A};
                              undefined ->
                                  St
                          end
                  end, C2SState, CC ++ BCC),
            {Packet, NewState};
	false ->
	    Acc
    end;
user_send_packet(Acc) ->
    Acc.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(list()) -> {ok, state()}.
init([LServerS|_]) ->
    process_flag(trap_exit, true),
    Opts = gen_mod:get_module_opts(LServerS, ?MODULE),
    [LServiceS|_] = gen_mod:get_opt_hosts(Opts),
    Access = mod_multicast_opt:access(Opts),
    SLimits = build_service_limit_record(mod_multicast_opt:limits(Opts)),
    create_cache(),
    try_start_loop(),
    ejabberd_router_multicast:register_route(LServerS),
    ejabberd_router:register_route(LServiceS, LServerS),
    ejabberd_hooks:add(user_send_packet, LServerS, ?MODULE,
		       user_send_packet, 50),
    {ok,
     #state{lservice = LServiceS, lserver = LServerS,
	    access = Access, service_limits = SLimits}}.

handle_call(stop, _From, State) ->
    try_stop_loop(), {stop, normal, ok, State}.

handle_cast({reload, NewOpts, NewOpts},
	    #state{lserver = LServerS, lservice = OldLServiceS} = State) ->
    Access = mod_multicast_opt:access(NewOpts),
    SLimits = build_service_limit_record(mod_multicast_opt:limits(NewOpts)),
    [NewLServiceS|_] = gen_mod:get_opt_hosts(NewOpts),
    if NewLServiceS /= OldLServiceS ->
	    ejabberd_router:register_route(NewLServiceS, LServerS),
	    ejabberd_router:unregister_route(OldLServiceS);
       true ->
	    ok
    end,
    {noreply, State#state{lservice = NewLServiceS,
			  access = Access, service_limits = SLimits}};
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info({route, #iq{} = Packet}, State) ->
    case catch handle_iq(Packet, State) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("Error when processing IQ stanza: ~p",
                       [Reason]);
        _ -> ok
    end,
    {noreply, State};
%% XEP33 allows only 'message' and 'presence' stanza type
handle_info({route, Packet},
	    #state{lservice = LServiceS, lserver = LServerS,
		   access = Access, service_limits = SLimits} =
		State) when ?is_stanza(Packet) ->
    route_untrusted(LServiceS, LServerS, Access, SLimits, Packet),
    {noreply, State};
%% Handle multicast packets sent by trusted local services
handle_info({route_trusted, Destinations, Packet},
	    #state{lservice = LServiceS, lserver = LServerS} =
		State) ->
    From = xmpp:get_from(Packet),
    case catch route_trusted(LServiceS, LServerS, From, Destinations,
                             Packet) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("Error in route_trusted: ~p", [Reason]);
        _ -> ok
    end,
    {noreply, State};
handle_info({get_host, Pid}, State) ->
    Pid ! {my_host, State#state.lservice},
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
    ejabberd_hooks:delete(user_send_packet, State#state.lserver, ?MODULE,
			  user_send_packet, 50),
    ejabberd_router_multicast:unregister_route(State#state.lserver),
    ejabberd_router:unregister_route(State#state.lservice),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================

%%%------------------------
%%% IQ Request Processing
%%%------------------------

handle_iq(Packet, State) ->
    try
	IQ = xmpp:decode_els(Packet),
	case process_iq(IQ, State) of
	    {result, SubEl} ->
		ejabberd_router:route(xmpp:make_iq_result(Packet, SubEl));
	    {error, Error} ->
		ejabberd_router:route_error(Packet, Error);
	    reply ->
		To = xmpp:get_to(IQ),
		LServiceS = jid:encode(To),
		case Packet#iq.type of
		    result ->
			process_iqreply_result(LServiceS, IQ);
		    error ->
			process_iqreply_error(LServiceS, IQ)
		end
	end
    catch _:{xmpp_codec, Why} ->
	    Lang = xmpp:get_lang(Packet),
	    Err = xmpp:err_bad_request(xmpp:io_format_error(Why), Lang),
	    ejabberd_router:route_error(Packet, Err)
    end.

-spec process_iq(iq(), state()) -> {result, xmpp_element()} |
				   {error, stanza_error()} | reply.
process_iq(#iq{type = get, lang = Lang, from = From,
	       sub_els = [#disco_info{}]}, State) ->
    {result, iq_disco_info(From, Lang, State)};
process_iq(#iq{type = get, sub_els = [#disco_items{}]}, _) ->
    {result, #disco_items{}};
process_iq(#iq{type = get, lang = Lang, sub_els = [#vcard_temp{}]}, State) ->
    {result, iq_vcard(Lang, State)};
process_iq(#iq{type = T}, _) when T == set; T == get ->
    {error, xmpp:err_service_unavailable()};
process_iq(_, _) ->
    reply.

iq_disco_info(From, Lang, State) ->
    Name = mod_multicast_opt:name(State#state.lserver),
    #disco_info{
       identities = [#identity{category = <<"service">>,
			       type = <<"multicast">>,
			       name = translate:translate(Lang, Name)}],
       features = [?NS_DISCO_INFO, ?NS_DISCO_ITEMS, ?NS_VCARD, ?NS_ADDRESS],
       xdata = iq_disco_info_extras(From, State)}.

-spec iq_vcard(binary(), state()) -> #vcard_temp{}.
iq_vcard(Lang, State) ->
    case mod_multicast_opt:vcard(State#state.lserver) of
	undefined ->
	    #vcard_temp{fn = <<"ejabberd/mod_multicast">>,
			url = ejabberd_config:get_uri(),
			desc = misc:get_descr(Lang, ?T("ejabberd Multicast service"))};
	VCard ->
	    VCard
    end.

%%%-------------------------
%%% Route
%%%-------------------------

-spec route_trusted(binary(), binary(), jid(), [jid()], stanza()) -> 'ok'.
route_trusted(LServiceS, LServerS, FromJID, Destinations, Packet) ->
    Addresses = [#address{type = bcc, jid = D} || D <- Destinations],
    Groups = group_by_destinations(Addresses, #{}),
    route_grouped(LServerS, LServiceS, FromJID, Groups, [], Packet).

-spec route_untrusted(binary(), binary(), atom(), #service_limits{}, stanza()) -> 'ok'.
route_untrusted(LServiceS, LServerS, Access, SLimits, Packet) ->
    try route_untrusted2(LServiceS, LServerS, Access,
			 SLimits, Packet)
    catch
      adenied ->
	  route_error(Packet, forbidden,
		      ?T("Access denied by service policy"));
      eadsele ->
	  route_error(Packet, bad_request,
		      ?T("No addresses element found"));
      eadeles ->
	  route_error(Packet, bad_request,
		      ?T("No address elements found"));
      ewxmlns ->
	  route_error(Packet, bad_request,
		      ?T("Wrong xmlns"));
      etoorec ->
	  route_error(Packet, not_acceptable,
		      ?T("Too many receiver fields were specified"));
      edrelay ->
	  route_error(Packet, forbidden,
		      ?T("Packet relay is denied by service policy"));
      EType:EReason ->
	  ?ERROR_MSG("Multicast unknown error: Type: ~p~nReason: ~p",
		     [EType, EReason]),
	  route_error(Packet, internal_server_error,
		      ?T("Internal server error"))
    end.

-spec route_untrusted2(binary(), binary(), atom(), #service_limits{}, stanza()) -> 'ok'.
route_untrusted2(LServiceS, LServerS, Access, SLimits, Packet) ->
    FromJID = xmpp:get_from(Packet),
    ok = check_access(LServerS, Access, FromJID),
    {ok, PacketStripped, Addresses} = strip_addresses_element(Packet),
    {CC, BCC, NotJids, Rest} = partition_addresses(Addresses),
    report_not_jid(FromJID, Packet, NotJids),
    ok = check_limit_dests(SLimits, FromJID, Packet, length(CC) + length(BCC)),
    Groups0 = group_by_destinations(CC, #{}),
    Groups = group_by_destinations(BCC, Groups0),
    ok = check_relay(FromJID#jid.server, LServerS, Groups),
    route_grouped(LServerS, LServiceS, FromJID, Groups, Rest, PacketStripped).

-spec mark_as_delivered([address()]) -> [address()].
mark_as_delivered(Addresses) ->
    [A#address{delivered = true} || A <- Addresses].

-spec route_individual(jid(), [address()], [address()], [address()], stanza()) -> ok.
route_individual(From, CC, BCC, Other, Packet) ->
    CCDelivered = mark_as_delivered(CC),
    Addresses = CCDelivered ++ Other,
    PacketWithAddresses = xmpp:append_subtags(Packet, [#addresses{list = Addresses}]),
    lists:foreach(
	fun(#address{jid = To}) ->
	    ejabberd_router:route(xmpp:set_from_to(PacketWithAddresses, From, To))
	end, CC),
    lists:foreach(
	fun(#address{jid = To} = Address) ->
	    Packet2 = case Addresses of
			  [] ->
			      Packet;
			  _ ->
			      xmpp:append_subtags(Packet, [#addresses{list = [Address | Addresses]}])
		      end,
	    ejabberd_router:route(xmpp:set_from_to(Packet2, From, To))
	end, BCC).

-spec route_chunk(jid(), jid(), stanza(), [address()]) -> ok.
route_chunk(From, To, Packet, Addresses) ->
    PacketWithAddresses = xmpp:append_subtags(Packet, [#addresses{list = Addresses}]),
    ejabberd_router:route(xmpp:set_from_to(PacketWithAddresses, From, To)).

-spec route_in_chunks(jid(), jid(), stanza(), integer(), [address()], [address()], [address()]) -> ok.
route_in_chunks(_From, _To, _Packet, _Limit, [], [], _) ->
    ok;
route_in_chunks(From, To, Packet, Limit, CC, BCC, RestOfAddresses) when length(CC) > Limit ->
    {Chunk, Rest} = lists:split(Limit, CC),
    route_chunk(From, To, Packet, Chunk ++ RestOfAddresses),
    route_in_chunks(From, To, Packet, Limit, Rest, BCC, RestOfAddresses);
route_in_chunks(From, To, Packet, Limit, [], BCC, RestOfAddresses) when length(BCC) > Limit ->
    {Chunk, Rest} = lists:split(Limit, BCC),
    route_chunk(From, To, Packet, Chunk ++ RestOfAddresses),
    route_in_chunks(From, To, Packet, Limit, [], Rest, RestOfAddresses);
route_in_chunks(From, To, Packet, Limit, CC, BCC, RestOfAddresses) when length(BCC) + length(CC) > Limit ->
    {Chunk, Rest} = lists:split(Limit - length(CC), BCC),
    route_chunk(From, To, Packet, CC ++ Chunk ++ RestOfAddresses),
    route_in_chunks(From, To, Packet, Limit, [], Rest, RestOfAddresses);
route_in_chunks(From, To, Packet, _Limit, CC, BCC, RestOfAddresses) ->
    route_chunk(From, To, Packet, CC ++ BCC ++ RestOfAddresses).

-spec route_multicast(jid(), jid(), [address()], [address()], [address()], stanza(), #limits{}) -> ok.
route_multicast(From, To, CC, BCC, RestOfAddresses, Packet, Limits) ->
    {_Type, Limit} = get_limit_number(element(1, Packet),
				      Limits),
    route_in_chunks(From, To, Packet, Limit, CC, BCC, RestOfAddresses).

-spec route_grouped(binary(), binary(), jid(), #{}, [address()], stanza()) -> ok.
route_grouped(LServer, LService, From, Groups, RestOfAddresses, Packet) ->
    maps:fold(
	fun(Server, {CC, BCC}, _) ->
	    OtherCC = maps:fold(
		fun(Server2, _, Res) when Server2 == Server ->
			Res;
		   (_, {CC2, _}, Res) ->
			mark_as_delivered(CC2) ++ Res
		end, [], Groups),
	    case search_server_on_cache(Server,
					LServer, LService,
					{?MAXTIME_CACHE_POSITIVE,
					 ?MAXTIME_CACHE_NEGATIVE}) of
		route_single ->
		    route_individual(From, CC, BCC, OtherCC ++ RestOfAddresses, Packet);
		{route_multicast, Service, Limits} ->
		    route_multicast(From, jid:make(Service), CC, BCC, OtherCC ++ RestOfAddresses, Packet, Limits)
	    end
	end, ok, Groups).

%%%-------------------------
%%% Check access permission
%%%-------------------------

check_access(LServerS, Access, From) ->
    case acl:match_rule(LServerS, Access, From) of
      allow -> ok;
      _ -> throw(adenied)
    end.

%%%-------------------------
%%% Strip 'addresses' XML element
%%%-------------------------

-spec strip_addresses_element(stanza()) -> {ok, stanza(), [address()]}.
strip_addresses_element(Packet) ->
    case xmpp:get_subtag(Packet, #addresses{}) of
	#addresses{list = Addrs} ->
	    PacketStripped = xmpp:remove_subtag(Packet, #addresses{}),
	    {ok, PacketStripped, Addrs};
	false ->
	    throw(eadsele)
    end.

%%%-------------------------
%%% Split Addresses
%%%-------------------------

partition_addresses(Addresses) ->
    lists:foldl(
	fun(#address{delivered = true} = A, {C, B, I, D}) ->
	    {C, B, I, [A | D]};
	   (#address{type = T, jid = undefined} = A, {C, B, I, D})
	       when T == to; T == cc; T == bcc ->
	       {C, B, [A | I], D};
	   (#address{type = T} = A, {C, B, I, D})
	       when T == to; T == cc ->
	       {[A | C], B, I, D};
	   (#address{type = bcc} = A, {C, B, I, D}) ->
	       {C, [A | B], I, D};
	   (A, {C, B, I, D}) ->
	       {C, B, I, [A | D]}
	end, {[], [], [], []}, Addresses).

%%%-------------------------
%%% Check does not exceed limit of destinations
%%%-------------------------

-spec check_limit_dests(#service_limits{}, jid(), stanza(), integer()) -> ok.
check_limit_dests(SLimits, FromJID, Packet, NumOfAddresses) ->
    SenderT = sender_type(FromJID),
    Limits = get_slimit_group(SenderT, SLimits),
    StanzaType = type_of_stanza(Packet),
    {_Type, Limit} = get_limit_number(StanzaType,
				      Limits),
    case NumOfAddresses > Limit of
      false -> ok;
      true -> throw(etoorec)
    end.


-spec report_not_jid(jid(), stanza(), [address()]) -> any().
report_not_jid(From, Packet, Addresses) ->
    lists:foreach(
	fun(Address) ->
	    route_error(
		xmpp:set_from_to(Packet, From, From), jid_malformed,
		str:format(?T("This service can not process the address: ~s"),
			   [fxml:element_to_binary(xmpp:encode(Address))]))
	end, Addresses).

%%%-------------------------
%%% Group destinations by their servers
%%%-------------------------

group_by_destinations(Addrs, Map) ->
    lists:foldl(
	fun
	    (#address{type = Type, jid = #jid{lserver = Server}} = Addr, Map2) when Type == to; Type == cc ->
		maps:update_with(Server,
		    fun({CC, BCC}) ->
			{[Addr | CC], BCC}
		    end, {[Addr], []}, Map2);
	    (#address{type = bcc, jid = #jid{lserver = Server}} = Addr, Map2) ->
		maps:update_with(Server,
		    fun({CC, BCC}) ->
			{CC, [Addr | BCC]}
		    end, {[], [Addr]}, Map2)
	end, Map, Addrs).

%%%-------------------------
%%% Route packet
%%%-------------------------

%%%-------------------------
%%% Check relay
%%%-------------------------

-spec check_relay(binary(), binary(), #{}) -> ok.
check_relay(RS, LS, Gs) ->
    case lists:suffix(str:tokens(LS, <<".">>),
		      str:tokens(RS, <<".">>)) orelse
	(maps:is_key(LS, Gs) andalso maps:size(Gs) == 1) of
	true -> ok;
	_ -> throw(edrelay)
    end.

%%%-------------------------
%%% Check protocol support: Send request
%%%-------------------------

-spec send_query_info(binary(), binary(), binary()) -> ok.
send_query_info(RServerS, LServiceS, ID) ->
    case str:str(RServerS, <<"echo.">>) of
      1 -> ok;
      _ -> send_query(RServerS, LServiceS, ID, #disco_info{})
    end.

-spec send_query_items(binary(), binary(), binary()) -> ok.
send_query_items(RServerS, LServiceS, ID) ->
    send_query(RServerS, LServiceS, ID, #disco_items{}).

-spec send_query(binary(), binary(), binary(), disco_info()|disco_items()) -> ok.
send_query(RServerS, LServiceS, ID, SubEl) ->
    Packet = #iq{from = stj(LServiceS),
		 to = stj(RServerS),
		 id = ID,
		 type = get, sub_els = [SubEl]},
    ejabberd_router:route(Packet).

%%%-------------------------
%%% Check protocol support: Receive response: Error
%%%-------------------------

process_iqreply_error(LServiceS, Packet) ->
    FromS = jts(xmpp:get_from(Packet)),
    ID = Packet#iq.id,
    case str:tokens(ID, <<"/">>) of
        [RServer, _] ->
            case look_server(RServer) of
                {cached, {_Response, {wait_for_info, ID}}, _TS}
                when RServer == FromS ->
                    add_response(RServer, not_supported, cached);
                {cached, {_Response, {wait_for_items, ID}}, _TS}
                when RServer == FromS ->
                    add_response(RServer, not_supported, cached);
                {cached, {Response, {wait_for_items_info, ID, Items}},
                 _TS} ->
                    case lists:member(FromS, Items) of
                        true ->
                            received_awaiter(
                              FromS, RServer, Response, ID, Items,
                              LServiceS);
                        false ->
                            ok
                    end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

%%%-------------------------
%%% Check protocol support: Receive response: Disco
%%%-------------------------

-spec process_iqreply_result(binary(), iq()) -> any().
process_iqreply_result(LServiceS, #iq{from = From, id = ID, sub_els = [SubEl]}) ->
    case SubEl of
	#disco_info{} ->
	    process_discoinfo_result(From, LServiceS, ID, SubEl);
	#disco_items{} ->
	    process_discoitems_result(From, LServiceS, ID, SubEl);
	_ ->
	    ok
    end.

%%%-------------------------
%%% Check protocol support: Receive response: Disco Info
%%%-------------------------

process_discoinfo_result(From, LServiceS, ID, DiscoInfo) ->
    FromS = jts(From),
    case str:tokens(ID, <<"/">>) of
        [RServer, _] ->
            case look_server(RServer) of
                {cached, {Response, {wait_for_info, ID} = ST}, _TS}
                when RServer == FromS ->
                    process_discoinfo_result2(
                      From, FromS, LServiceS, DiscoInfo,
                      RServer, Response, ST);
                {cached, {Response, {wait_for_items_info, ID, Items} = ST},
                 _TS} ->
                    case lists:member(FromS, Items) of
                        true ->
                            process_discoinfo_result2(
                              From, FromS, LServiceS, DiscoInfo,
                              RServer, Response, ST);
                        false ->
                            ok
                    end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

process_discoinfo_result2(From, FromS, LServiceS,
			  #disco_info{features = Feats} = DiscoInfo,
			  RServer, Response, ST) ->
    Multicast_support = lists:member(?NS_ADDRESS, Feats),
    case Multicast_support of
	true ->
	    SenderT = sender_type(From),
	    RLimits = get_limits_xml(DiscoInfo, SenderT),
	    add_response(RServer, {multicast_supported, FromS, RLimits}, cached);
	false ->
	    case ST of
		{wait_for_info, _ID} ->
		    Random = p1_rand:get_string(),
		    ID = <<RServer/binary, $/, Random/binary>>,
		    send_query_items(FromS, LServiceS, ID),
		    add_response(RServer, Response, {wait_for_items, ID});
		%% We asked a component, and it does not support XEP33
		{wait_for_items_info, ID, Items} ->
		    received_awaiter(FromS, RServer, Response, ID, Items, LServiceS)
	    end
    end.

get_limits_xml(DiscoInfo, SenderT) ->
    LimitOpts = get_limits_els(DiscoInfo),
    build_remote_limit_record(LimitOpts, SenderT).

-spec get_limits_els(disco_info()) -> [{atom(), integer()}].
get_limits_els(DiscoInfo) ->
    lists:flatmap(
      fun(#xdata{type = result} = X) ->
	      get_limits_fields(X);
	 (_) ->
	      []
      end, DiscoInfo#disco_info.xdata).

-spec get_limits_fields(xdata()) -> [{atom(), integer()}].
get_limits_fields(X) ->
    {Head, Tail} = lists:partition(
		     fun(#xdata_field{var = Var, type = Type}) ->
			     Var == <<"FORM_TYPE">> andalso Type == hidden
		     end, X#xdata.fields),
    case Head of
      [] -> [];
      _ -> get_limits_values(Tail)
    end.

-spec get_limits_values([xdata_field()]) -> [{atom(), integer()}].
get_limits_values(Fields) ->
    lists:flatmap(
      fun(#xdata_field{var = Name, values = [Number]}) ->
	      try
		  [{binary_to_atom(Name, utf8), binary_to_integer(Number)}]
	      catch _:badarg ->
		      []
	      end;
	 (_) ->
	      []
      end, Fields).

%%%-------------------------
%%% Check protocol support: Receive response: Disco Items
%%%-------------------------

process_discoitems_result(From, LServiceS, ID, #disco_items{items = Items}) ->
    FromS = jts(From),
    case str:tokens(ID, <<"/">>) of
        [FromS = RServer, _] ->
            case look_server(RServer) of
                {cached, {Response, {wait_for_items, ID}}, _TS} ->
                    List = lists:flatmap(
                             fun(#disco_item{jid = #jid{luser = <<"">>,
                                                        lserver = LServer,
                                                        lresource = <<"">>}}) ->
                                     [LServer];
                                (_) ->
                                     []
                             end, Items),
                    case List of
                        [] ->
                            add_response(RServer, not_supported, cached);
                        _ ->
                            Random = p1_rand:get_string(),
                            ID2 = <<RServer/binary, $/, Random/binary>>,
                            [send_query_info(Item, LServiceS, ID2) || Item <- List],
                            add_response(RServer, Response,
                                         {wait_for_items_info, ID2, List})
                    end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

%%%-------------------------
%%% Check protocol support: Receive response: Received awaiter
%%%-------------------------

received_awaiter(JID, RServer, Response, ID, JIDs, _LServiceS) ->
    case lists:delete(JID, JIDs) of
        [] ->
            add_response(RServer, not_supported, cached);
        JIDs2 ->
            add_response(RServer, Response, {wait_for_items_info, ID, JIDs2})
    end.

%%%-------------------------
%%% Cache
%%%-------------------------

create_cache() ->
    ejabberd_mnesia:create(?MODULE, multicastc,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, multicastc)}]).

add_response(RServer, Response, State) ->
    Secs = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    mnesia:dirty_write(#multicastc{rserver = RServer,
				   response = {Response, State}, ts = Secs}).

search_server_on_cache(RServer, LServerS, _LServiceS, _Maxmins)
    when RServer == LServerS ->
    route_single;
search_server_on_cache(RServer, _LServerS, LServiceS, _Maxmins)
    when RServer == LServiceS ->
    route_single;
search_server_on_cache(RServer, _LServerS, LServiceS, Maxmins) ->
    case look_server(RServer) of
        not_cached ->
            query_info(RServer, LServiceS, not_supported),
            route_single;
        {cached, {Response, State}, TS} ->
            Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
            Response2 =
                case State of
                    cached ->
                        case is_obsolete(Response, TS, Now, Maxmins) of
                            false -> ok;
                            true ->
                                query_info(RServer, LServiceS, Response)
                        end,
                        Response;
                    _ ->
                        if
                            Now - TS > ?MAXTIME_CACHE_NEGOTIATING ->
                                query_info(RServer, LServiceS, not_supported),
                                not_supported;
                            true ->
                                Response
                        end
                end,
            case Response2 of
                not_supported -> route_single;
                {multicast_supported, Service, Limits} ->
                    {route_multicast, Service, Limits}
            end
    end.

query_info(RServer, LServiceS, Response) ->
    Random = p1_rand:get_string(),
    ID = <<RServer/binary, $/, Random/binary>>,
    send_query_info(RServer, LServiceS, ID),
    add_response(RServer, Response, {wait_for_info, ID}).

look_server(RServer) ->
    case mnesia:dirty_read(multicastc, RServer) of
      [] -> not_cached;
      [M] -> {cached, M#multicastc.response, M#multicastc.ts}
    end.

is_obsolete(Response, Ts, Now, {Max_pos, Max_neg}) ->
    Max = case Response of
	    multicast_not_supported -> Max_neg;
	    _ -> Max_pos
	  end,
    Now - Ts > Max.

%%%-------------------------
%%% Purge cache
%%%-------------------------

purge() ->
    Maxmins_positive = (?MAXTIME_CACHE_POSITIVE),
    Maxmins_negative = (?MAXTIME_CACHE_NEGATIVE),
    Now =
        calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    purge(Now, {Maxmins_positive, Maxmins_negative}).

purge(Now, Maxmins) ->
    F = fun () ->
		mnesia:foldl(fun (R, _) ->
				     #multicastc{response = Response, ts = Ts} =
					 R,
				     case is_obsolete(Response, Ts, Now,
						      Maxmins)
					 of
				       true -> mnesia:delete_object(R);
				       false -> ok
				     end
			     end,
			     none, multicastc)
	end,
    mnesia:transaction(F).

%%%-------------------------
%%% Purge cache loop
%%%-------------------------

try_start_loop() ->
    case lists:member(?PURGE_PROCNAME, registered()) of
      true -> ok;
      false -> start_loop()
    end,
    (?PURGE_PROCNAME) ! new_module.

start_loop() ->
    register(?PURGE_PROCNAME,
	     spawn(?MODULE, purge_loop, [0])),
    (?PURGE_PROCNAME) ! purge_now.

try_stop_loop() -> (?PURGE_PROCNAME) ! try_stop.

purge_loop(NM) ->
    receive
      purge_now ->
	  purge(),
	  timer:send_after(?CACHE_PURGE_TIMER, ?PURGE_PROCNAME,
			   purge_now),
	  purge_loop(NM);
      new_module -> purge_loop(NM + 1);
      try_stop when NM > 1 -> purge_loop(NM - 1);
      try_stop -> purge_loop_finished
    end.

%%%-------------------------
%%% Limits: utils
%%%-------------------------

%% Type definitions for data structures related with XEP33 limits
%% limit() = {Name, Value}
%% Name = atom()
%% Value = {Type, Number}
%% Type = default | custom
%% Number = integer() | infinite

list_of_limits(local) ->
    [{message, ?DEFAULT_LIMIT_LOCAL_MESSAGE},
     {presence, ?DEFAULT_LIMIT_LOCAL_PRESENCE}];
list_of_limits(remote) ->
    [{message, ?DEFAULT_LIMIT_REMOTE_MESSAGE},
     {presence, ?DEFAULT_LIMIT_REMOTE_PRESENCE}].

build_service_limit_record(LimitOpts) ->
    LimitOptsL = get_from_limitopts(LimitOpts, local),
    LimitOptsR = get_from_limitopts(LimitOpts, remote),
    {service_limits, build_limit_record(LimitOptsL, local),
     build_limit_record(LimitOptsR, remote)}.

get_from_limitopts(LimitOpts, SenderT) ->
    case lists:keyfind(SenderT, 1, LimitOpts) of
	false -> [];
	{SenderT, Result} -> Result
    end.

build_remote_limit_record(LimitOpts, SenderT) ->
    build_limit_record(LimitOpts, SenderT).

-spec build_limit_record(any(), local | remote) -> #limits{}.
build_limit_record(LimitOpts, SenderT) ->
    Limits = [get_limit_value(Name, Default, LimitOpts)
	      || {Name, Default} <- list_of_limits(SenderT)],
    list_to_tuple([limits | Limits]).

-spec get_limit_value(atom(), integer(), any()) -> limit_value().
get_limit_value(Name, Default, LimitOpts) ->
    case lists:keysearch(Name, 1, LimitOpts) of
      {value, {Name, Number}} -> {custom, Number};
      false -> {default, Default}
    end.

type_of_stanza(Stanza) -> element(1, Stanza).

-spec get_limit_number(message | presence, #limits{}) -> limit_value().
get_limit_number(message, Limits) ->
    Limits#limits.message;
get_limit_number(presence, Limits) ->
    Limits#limits.presence.

-spec get_slimit_group(local | remote, #service_limits{}) -> #limits{}.
get_slimit_group(local, SLimits) ->
    SLimits#service_limits.local;
get_slimit_group(remote, SLimits) ->
    SLimits#service_limits.remote.

%%%-------------------------
%%% Limits: XEP-0128 Service Discovery Extensions
%%%-------------------------

%% Some parts of code are borrowed from mod_muc_room.erl

-define(RFIELDT(Type, Var, Val),
	#xdata_field{type = Type, var = Var, values = [Val]}).

-define(RFIELDV(Var, Val),
	#xdata_field{var = Var, values = [Val]}).

iq_disco_info_extras(From, State) ->
    SenderT = sender_type(From),
    Service_limits = State#state.service_limits,
    case iq_disco_info_extras2(SenderT, Service_limits) of
      [] -> [];
      List_limits_xmpp ->
	    [#xdata{type = result,
		   fields = [?RFIELDT(hidden, <<"FORM_TYPE">>, ?NS_ADDRESS)
			     | List_limits_xmpp]}]
    end.

sender_type(From) ->
    Local_hosts = ejabberd_option:hosts(),
    case lists:member(From#jid.lserver, Local_hosts) of
      true -> local;
      false -> remote
    end.

iq_disco_info_extras2(SenderT, SLimits) ->
    Limits = get_slimit_group(SenderT, SLimits),
    Stanza_types = [message, presence],
    lists:foldl(fun (Type_of_stanza, R) ->
			case get_limit_number(Type_of_stanza, Limits) of
			  {custom, Number} ->
			      [?RFIELDV((to_binary(Type_of_stanza)),
					(to_binary(Number)))
			       | R];
			  {default, _} -> R
			end
		end,
		[], Stanza_types).

to_binary(A) -> list_to_binary(hd(io_lib:format("~p", [A]))).

%%%-------------------------
%%% Error report
%%%-------------------------

route_error(Packet, ErrType, ErrText) ->
    Lang = xmpp:get_lang(Packet),
    Err = make_reply(ErrType, Lang, ErrText),
    ejabberd_router:route_error(Packet, Err).

make_reply(bad_request, Lang, ErrText) ->
    xmpp:err_bad_request(ErrText, Lang);
make_reply(jid_malformed, Lang, ErrText) ->
    xmpp:err_jid_malformed(ErrText, Lang);
make_reply(not_acceptable, Lang, ErrText) ->
    xmpp:err_not_acceptable(ErrText, Lang);
make_reply(internal_server_error, Lang, ErrText) ->
    xmpp:err_internal_server_error(ErrText, Lang);
make_reply(forbidden, Lang, ErrText) ->
    xmpp:err_forbidden(ErrText, Lang).

stj(String) -> jid:decode(String).

jts(String) -> jid:encode(String).

depends(_Host, _Opts) ->
    [].

mod_opt_type(access) ->
    econf:acl();
mod_opt_type(name) ->
    econf:binary();
mod_opt_type(limits) ->
    econf:options(
      #{local =>
	    econf:options(
	      #{message => econf:non_neg_int(infinite),
		presence => econf:non_neg_int(infinite)}),
	remote =>
	    econf:options(
	      #{message => econf:non_neg_int(infinite),
		presence => econf:non_neg_int(infinite)})});
mod_opt_type(host) ->
    econf:host();
mod_opt_type(hosts) ->
    econf:hosts();
mod_opt_type(vcard) ->
    econf:vcard_temp().

mod_options(Host) ->
    [{access, all},
     {host, <<"multicast.", Host/binary>>},
     {hosts, []},
     {limits, [{local, []}, {remote, []}]},
     {vcard, undefined},
     {name, ?T("Multicast")}].

mod_doc() ->
    #{desc =>
	  [?T("This module implements a service for "
	      "https://xmpp.org/extensions/xep-0033.html"
	      "[XEP-0033: Extended Stanza Addressing].")],
      opts =>
          [{access,
            #{value => "Access",
              desc =>
                  ?T("The access rule to restrict who can send packets to "
		     "the multicast service. Default value: 'all'.")}},
           {host,
            #{desc => ?T("Deprecated. Use 'hosts' instead.")}},
           {hosts,
            #{value => ?T("[Host, ...]"),
              desc =>
                  [?T("This option defines the Jabber IDs of the service. "
		      "If the 'hosts' option is not specified, the only "
		      "Jabber ID will be the hostname of the virtual host "
		      "with the prefix \"multicast.\". The keyword '@HOST@' "
		      "is replaced with the real virtual host name."),
		   ?T("The default value is 'multicast.@HOST@'.")]}},
	   {limits,
	    #{value => "Sender: Stanza: Number",
	      desc =>
		  [?T("Specify a list of custom limits which override the "
		      "default ones defined in XEP-0033. Limits are defined "
		      "per sender type and stanza type, where:"), "",
		   ?T("- 'sender' can be: 'local' or 'remote'."),
		   ?T("- 'stanza' can be: 'message' or 'presence'."),
		   ?T("- 'number' can be a positive integer or 'infinite'.")],
              example =>
                    ["# Default values:",
                     "local:",
		     "  message: 100",
		     "  presence: 100",
		     "remote:",
		     "  message: 20",
		     "  presence: 20"]
		  }},
           {name,
            #{desc => ?T("Service name to provide in the Info query to the "
			 "Service Discovery. Default is '\"Multicast\"'.")}},
           {vcard,
            #{desc => ?T("vCard element to return when queried. "
			 "Default value is 'undefined'.")}}],
      example =>
          ["# Only admins can send packets to multicast service",
	   "access_rules:",
	   "  multicast:",
	   "    - allow: admin",
	   "",
	   "# If you want to allow all your users:",
	   "access_rules:",
	   "  multicast:",
	   "    - allow",
	   "",
	   "# This allows both admins and remote users to send packets,",
	   "# but does not allow local users",
	   "acl:",
	   "  allservers:",
	   "    server_glob: \"*\"",
	   "access_rules:",
	   "  multicast:",
	   "    - allow: admin",
	   "    - deny: local",
	   "    - allow: allservers",
	   "",
	   "modules:",
	   "  mod_multicast:",
	   "     host: multicast.example.org",
	   "     access: multicast",
	   "     limits:",
	   "       local:",
	   "         message: 40",
	   "         presence: infinite",
	   "       remote:",
	   "         message: 150"]}.
