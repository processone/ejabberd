%%%----------------------------------------------------------------------
%%% File    : mod_multicast.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Extended Stanza Addressing (XEP-0033) support
%%% Created : 29 May 2007 by Badlop <badlop@process-one.net>
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
%%%----------------------------------------------------------------------

-module(mod_multicast).

-author('badlop@process-one.net').

-protocol({xep, 33, '1.1'}).

-behaviour(gen_server).

-behaviour(gen_mod).

%% API
-export([start/2, stop/1, reload/3]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3,
	 handle_cast/2, terminate/2, code_change/3]).

-export([purge_loop/1, mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

-record(state,
	{lserver, lservice, access, service_limits}).
-type state() :: #state{}.

-record(multicastc, {rserver, response, ts}).

%% ts: timestamp (in seconds) when the cache item was last updated

-record(dest, {jid_string = none :: binary(),
	       jid_jid :: jid(),
	       type :: atom(),
	       full_xml :: address()}).

%% jid_string = string()
%% jid_jid = jid()
%% full_xml = xml()

-record(group,
	{server, dests, multicast, others, addresses}).

%% server = string()
%% dests = [string()]
%% multicast = {cached, local_server} | {cached, string()} | {cached, not_supported} | {obsolete, not_supported} | {obsolete, string()} | not_cached
%%  after being updated, possible values are: local | multicast_not_supported | {multicast_supported, string(), limits()}
%% others = [xml()]
%% packet = xml()

-record(waiter,
	{awaiting, group, renewal = false, sender, packet,
	 aattrs, addresses}).

%% awaiting = {[Remote_service], Local_service, Type_awaiting}
%%  Remote_service = Local_service = string()
%%  Type_awaiting = info | items
%% group = #group
%% renewal = true | false
%% sender = From
%% packet = xml()
%% aattrs = [xml()]

-record(limits, {message, presence}).

%% message = presence = integer() | infinite

-record(service_limits, {local, remote}).

%% All the elements are of type value()

-define(VERSION_MULTICAST, <<"$Revision: 440 $ ">>).

-define(PURGE_PROCNAME,
	ejabberd_mod_multicast_purgeloop).

-define(MAXTIME_CACHE_POSITIVE, 86400).

-define(MAXTIME_CACHE_NEGATIVE, 86400).

-define(CACHE_PURGE_TIMER, 86400000).

-define(DISCO_QUERY_TIMEOUT, 10000).

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

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([LServerS, Opts]) ->
    process_flag(trap_exit, true),
    LServiceS = gen_mod:get_opt_host(LServerS, Opts,
				     <<"multicast.@HOST@">>),
    Access = gen_mod:get_opt(access, Opts,
			     fun acl:access_rules_validator/1, all),
    SLimits =
	build_service_limit_record(gen_mod:get_opt(limits, Opts,
						   fun (A) when is_list(A) ->
							   A
						   end,
						   [])),
    create_cache(),
    try_start_loop(),
    create_pool(),
    ejabberd_router_multicast:register_route(LServerS),
    ejabberd_router:register_route(LServiceS, LServerS),
    {ok,
     #state{lservice = LServiceS, lserver = LServerS,
	    access = Access, service_limits = SLimits}}.

handle_call(stop, _From, State) ->
    try_stop_loop(), {stop, normal, ok, State}.

handle_cast({reload, NewOpts, NewOpts},
	    #state{lserver = LServerS, lservice = OldLServiceS} = State) ->
    Access = gen_mod:get_opt(access, NewOpts,
			     fun acl:access_rules_validator/1, all),
    SLimits =
	build_service_limit_record(gen_mod:get_opt(limits, NewOpts,
						   fun (A) when is_list(A) ->
							   A
						   end,
						   [])),
    NewLServiceS = gen_mod:get_opt_host(LServerS, NewOpts,
					<<"multicast.@HOST@">>),
    if NewLServiceS /= OldLServiceS ->
	    ejabberd_router:register_route(NewLServiceS, LServerS),
	    ejabberd_router:unregister_route(OldLServiceS);
       true ->
	    ok
    end,
    {noreply, State#state{lservice = NewLServiceS,
			  access = Access, service_limits = SLimits}};
handle_cast(Msg, State) ->
    ?WARNING_MSG("unexpected cast: ~p", [Msg]),
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
	    Err = xmpp:err_bad_request(xmpp:format_error(Why), Lang),
	    ejabberd_router:route_error(Packet, Err)
    end.

-spec process_iq(iq(), state()) -> {result, xmpp_element()} |
				   {error, stanza_error()} | reply.
process_iq(#iq{type = get, lang = Lang, from = From,
	       sub_els = [#disco_info{}]}, State) ->
    {result, iq_disco_info(From, Lang, State)};
process_iq(#iq{type = get, sub_els = [#disco_items{}]}, _) ->
    {result, #disco_items{}};
process_iq(#iq{type = get, lang = Lang, sub_els = [#vcard_temp{}]}, _) ->
    {result, iq_vcard(Lang)};
process_iq(#iq{type = T}, _) when T == set; T == get ->
    {error, xmpp:err_service_unavailable()};
process_iq(_, _) ->
    reply.

-define(FEATURE(Feat), Feat).

iq_disco_info(From, Lang, State) ->
    #disco_info{
       identities = [#identity{category = <<"service">>,
			       type = <<"multicast">>,
			       name = translate:translate(Lang, <<"Multicast">>)}],
       features = [?NS_DISCO_INFO, ?NS_DISCO_ITEMS, ?NS_VCARD, ?NS_ADDRESS],
       xdata = iq_disco_info_extras(From, State)}.

iq_vcard(Lang) ->
    Desc = translate:translate(Lang, <<"ejabberd Multicast service">>),
    #vcard_temp{fn = <<"ejabberd/mod_multicast">>,
		url = ?EJABBERD_URI,
		desc = <<Desc/binary, $\n, ?COPYRIGHT>>}.

%%%-------------------------
%%% Route
%%%-------------------------

route_trusted(LServiceS, LServerS, FromJID,
	      Destinations, Packet) ->
    Packet_stripped = Packet,
    AAttrs = [],
    Delivereds = [],
    Dests2 = lists:map(
	       fun(D) ->
		       #dest{jid_string = jid:encode(D),
			     jid_jid = D, type = bcc,
			     full_xml = #address{type = bcc, jid = D}}
	       end, Destinations),
    Groups = group_dests(Dests2),
    route_common(LServerS, LServiceS, FromJID, Groups,
		 Delivereds, Packet_stripped, AAttrs).

route_untrusted(LServiceS, LServerS, Access, SLimits, Packet) ->
    try route_untrusted2(LServiceS, LServerS, Access,
			 SLimits, Packet)
    catch
      adenied ->
	  route_error(Packet, forbidden,
		      <<"Access denied by service policy">>);
      eadsele ->
	  route_error(Packet, bad_request,
		      <<"No addresses element found">>);
      eadeles ->
	  route_error(Packet, bad_request,
		      <<"No address elements found">>);
      ewxmlns ->
	  route_error(Packet, bad_request,
		      <<"Wrong xmlns">>);
      etoorec ->
	  route_error(Packet, not_acceptable,
		      <<"Too many receiver fields were specified">>);
      edrelay ->
	  route_error(Packet, forbidden,
		      <<"Packet relay is denied by service policy">>);
      EType:EReason ->
	  ?ERROR_MSG("Multicast unknown error: Type: ~p~nReason: ~p",
		     [EType, EReason]),
	  route_error(Packet, internal_server_error,
		      <<"Unknown problem">>)
    end.

route_untrusted2(LServiceS, LServerS, Access, SLimits, Packet) ->
    FromJID = xmpp:get_from(Packet),
    ok = check_access(LServerS, Access, FromJID),
    {ok, Packet_stripped, Addresses} = strip_addresses_element(Packet),
    {To_deliver, Delivereds} = split_addresses_todeliver(Addresses),
    Dests = convert_dest_record(To_deliver),
    {Dests2, Not_jids} = split_dests_jid(Dests),
    report_not_jid(FromJID, Packet, Not_jids),
    ok = check_limit_dests(SLimits, FromJID, Packet, Dests2),
    Groups = group_dests(Dests2),
    ok = check_relay(FromJID#jid.server, LServerS, Groups),
    route_common(LServerS, LServiceS, FromJID, Groups,
		 Delivereds, Packet_stripped, []).

-spec route_common(binary(), binary(), jid(), [#group{}],
		   [address()], stanza(), list()) -> any().
route_common(LServerS, LServiceS, FromJID, Groups,
	     Delivereds, Packet_stripped, AAttrs) ->
    Groups2 = look_cached_servers(LServerS, Groups),
    Groups3 = build_others_xml(Groups2),
    Groups4 = add_addresses(Delivereds, Groups3),
    AGroups = decide_action_groups(Groups4),
    act_groups(FromJID, Packet_stripped, AAttrs, LServiceS,
	       AGroups).

act_groups(FromJID, Packet_stripped, AAttrs, LServiceS,
	   AGroups) ->
    [perform(FromJID, Packet_stripped, AAttrs, LServiceS,
	     AGroup)
     || AGroup <- AGroups].

perform(From, Packet, AAttrs, _,
	{route_single, Group}) ->
    [route_packet(From, ToUser, Packet, AAttrs,
		  Group#group.others, Group#group.addresses)
     || ToUser <- Group#group.dests];
perform(From, Packet, AAttrs, _,
	{{route_multicast, JID, RLimits}, Group}) ->
    route_packet_multicast(From, JID, Packet, AAttrs,
			   Group#group.dests, Group#group.addresses, RLimits);
perform(From, Packet, AAttrs, LServiceS,
	{{ask, Old_service, renewal}, Group}) ->
    send_query_info(Old_service, LServiceS),
    add_waiter(#waiter{awaiting =
			   {[Old_service], LServiceS, info},
		       group = Group, renewal = true, sender = From,
		       packet = Packet, aattrs = AAttrs,
		       addresses = Group#group.addresses});
perform(_From, _Packet, _AAttrs, LServiceS,
	{{ask, LServiceS, _}, _Group}) ->
    ok;
perform(From, Packet, AAttrs, LServiceS,
	{{ask, Server, not_renewal}, Group}) ->
    send_query_info(Server, LServiceS),
    add_waiter(#waiter{awaiting =
			   {[Server], LServiceS, info},
		       group = Group, renewal = false, sender = From,
		       packet = Packet, aattrs = AAttrs,
		       addresses = Group#group.addresses}).

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
	undefined ->
	    throw(eadsele)
    end.

%%%-------------------------
%%% Split Addresses
%%%-------------------------

-spec split_addresses_todeliver([address()]) -> {[address()], [address()]}.
split_addresses_todeliver(Addresses) ->
    lists:partition(
      fun(#address{delivered = true}) ->
	      false;
	 (#address{type = Type}) ->
	      case Type of
		  to -> true;
		  cc -> true;
		  bcc -> true;
		  _ -> false
	      end
      end, Addresses).

%%%-------------------------
%%% Check does not exceed limit of destinations
%%%-------------------------

-spec check_limit_dests(_, jid(), stanza(), [address()]) -> ok.
check_limit_dests(SLimits, FromJID, Packet,
		  Addresses) ->
    SenderT = sender_type(FromJID),
    Limits = get_slimit_group(SenderT, SLimits),
    Type_of_stanza = type_of_stanza(Packet),
    {_Type, Limit_number} = get_limit_number(Type_of_stanza,
					     Limits),
    case length(Addresses) > Limit_number of
      false -> ok;
      true -> throw(etoorec)
    end.

%%%-------------------------
%%% Convert Destination XML to record
%%%-------------------------

-spec convert_dest_record([address()]) -> [#dest{}].
convert_dest_record(Addrs) ->
    lists:map(
      fun(#address{jid = undefined} = Addr) ->
	      #dest{jid_string = none, full_xml = Addr};
	 (#address{jid = JID, type = Type} = Addr) ->
	      #dest{jid_string = jid:encode(JID), jid_jid = JID,
		    type = Type, full_xml = Addr}
      end, Addrs).

%%%-------------------------
%%% Split destinations by existence of JID
%%% and send error messages for other dests
%%%-------------------------

-spec split_dests_jid([#dest{}]) -> {[#dest{}], [#dest{}]}.
split_dests_jid(Dests) ->
    lists:partition(fun (Dest) ->
			    case Dest#dest.jid_string of
			      none -> false;
			      _ -> true
			    end
		    end,
		    Dests).

-spec report_not_jid(jid(), stanza(), #dest{}) -> any().
report_not_jid(From, Packet, Dests) ->
    Dests2 = [fxml:element_to_binary(xmpp:encode(Dest#dest.full_xml))
	      || Dest <- Dests],
    [route_error(xmpp:set_from_to(Packet, From, From), jid_malformed,
		 <<"This service can not process the address: ",
		   D/binary>>)
     || D <- Dests2].

%%%-------------------------
%%% Group destinations by their servers
%%%-------------------------

-spec group_dests([#dest{}]) -> [#group{}].
group_dests(Dests) ->
    D = lists:foldl(fun (Dest, Dict) ->
			    ServerS = (Dest#dest.jid_jid)#jid.server,
			    dict:append(ServerS, Dest, Dict)
		    end,
		    dict:new(), Dests),
    Keys = dict:fetch_keys(D),
    [#group{server = Key, dests = dict:fetch(Key, D)}
     || Key <- Keys].

%%%-------------------------
%%% Look for cached responses
%%%-------------------------

look_cached_servers(LServerS, Groups) ->
    [look_cached(LServerS, Group) || Group <- Groups].

look_cached(LServerS, G) ->
    Maxtime_positive = (?MAXTIME_CACHE_POSITIVE),
    Maxtime_negative = (?MAXTIME_CACHE_NEGATIVE),
    Cached_response = search_server_on_cache(G#group.server,
					     LServerS,
					     {Maxtime_positive,
					      Maxtime_negative}),
    G#group{multicast = Cached_response}.

%%%-------------------------
%%% Build delivered XML element
%%%-------------------------

build_others_xml(Groups) ->
    [Group#group{others =
		     build_other_xml(Group#group.dests)}
     || Group <- Groups].

build_other_xml(Dests) ->
    lists:foldl(fun (Dest, R) ->
			XML = Dest#dest.full_xml,
			case Dest#dest.type of
			  to -> [add_delivered(XML) | R];
			  cc -> [add_delivered(XML) | R];
			  bcc -> R;
			  _ -> [XML | R]
			end
		end,
		[], Dests).

-spec add_delivered(address()) -> address().
add_delivered(Addr) ->
    Addr#address{delivered = true}.

%%%-------------------------
%%% Add preliminary packets
%%%-------------------------

add_addresses(Delivereds, Groups) ->
    Ps = [Group#group.others || Group <- Groups],
    add_addresses2(Delivereds, Groups, [], [], Ps).

add_addresses2(_, [], Res, _, []) -> Res;
add_addresses2(Delivereds, [Group | Groups], Res, Pa,
	       [Pi | Pz]) ->
    Addresses = lists:append([Delivereds] ++ Pa ++ Pz),
    Group2 = Group#group{addresses = Addresses},
    add_addresses2(Delivereds, Groups, [Group2 | Res],
		   [Pi | Pa], Pz).

%%%-------------------------
%%% Decide action groups
%%%-------------------------

decide_action_groups(Groups) ->
    [{decide_action_group(Group), Group}
     || Group <- Groups].

decide_action_group(Group) ->
    Server = Group#group.server,
    case Group#group.multicast of
      {cached, local_server} ->
	  %% Send a copy of the packet to each local user on Dests
	  route_single;
      {cached, not_supported} ->
	  %% Send a copy of the packet to each remote user on Dests
	  route_single;
      {cached, {multicast_supported, JID, RLimits}} ->
	  {route_multicast, JID, RLimits};
      {obsolete,
       {multicast_supported, Old_service, _RLimits}} ->
	  {ask, Old_service, renewal};
      {obsolete, not_supported} -> {ask, Server, not_renewal};
      not_cached -> {ask, Server, not_renewal}
    end.

%%%-------------------------
%%% Route packet
%%%-------------------------

route_packet(From, ToDest, Packet, AAttrs, Others, Addresses) ->
    Dests = case ToDest#dest.type of
	      bcc -> [];
	      _ -> [ToDest]
	    end,
    route_packet2(From, ToDest#dest.jid_string, Dests,
		  Packet, AAttrs, {Others, Addresses}).

route_packet_multicast(From, ToS, Packet, AAttrs, Dests,
		       Addresses, Limits) ->
    Type_of_stanza = type_of_stanza(Packet),
    {_Type, Limit_number} = get_limit_number(Type_of_stanza,
					     Limits),
    Fragmented_dests = fragment_dests(Dests, Limit_number),
    [route_packet2(From, ToS, DFragment, Packet, AAttrs,
		   Addresses)
     || DFragment <- Fragmented_dests].

-spec route_packet2(jid(), binary(), [#dest{}], stanza(), list(), [address()]) -> ok.
route_packet2(From, ToS, Dests, Packet, _AAttrs,
	      Addresses) ->
    Els = case append_dests(Dests, Addresses) of
	      [] ->
		  xmpp:get_els(Packet);
	      ACs ->
		  [#addresses{list = ACs}|xmpp:get_els(Packet)]
	  end,
    Packet2 = xmpp:set_els(Packet, Els),
    ToJID = stj(ToS),
    ejabberd_router:route(xmpp:set_from_to(Packet2, From, ToJID)).

-spec append_dests([#dest{}], {[address()], [address()]} | [address()]) -> [address()].
append_dests(_Dests, {Others, Addresses}) ->
    Addresses++Others;
append_dests([], Addresses) -> Addresses;
append_dests([Dest | Dests], Addresses) ->
    append_dests(Dests, [Dest#dest.full_xml | Addresses]).

%%%-------------------------
%%% Check relay
%%%-------------------------

-spec check_relay(binary(), binary(), [#group{}]) -> ok.
check_relay(RS, LS, Gs) ->
    case check_relay_required(RS, LS, Gs) of
      false -> ok;
      true -> throw(edrelay)
    end.

-spec check_relay_required(binary(), binary(), [#group{}]) -> boolean().
check_relay_required(RServer, LServerS, Groups) ->
    case lists:suffix(str:tokens(LServerS, <<".">>),
                      str:tokens(RServer, <<".">>)) of
      true -> false;
      false -> check_relay_required(LServerS, Groups)
    end.

-spec check_relay_required(binary(), [#group{}]) -> boolean().
check_relay_required(LServerS, Groups) ->
    lists:any(fun (Group) -> Group#group.server /= LServerS
	      end,
	      Groups).

%%%-------------------------
%%% Check protocol support: Send request
%%%-------------------------

send_query_info(RServerS, LServiceS) ->
    case str:str(RServerS, <<"echo.">>) of
      1 -> false;
      _ -> send_query(RServerS, LServiceS, #disco_info{})
    end.

send_query_items(RServerS, LServiceS) ->
    send_query(RServerS, LServiceS, #disco_items{}).

-spec send_query(binary(), binary(), [disco_info()|disco_items()]) -> ok.
send_query(RServerS, LServiceS, SubEl) ->
    Packet = #iq{from = stj(LServiceS),
		 to = stj(RServerS),
		 id = randoms:get_string(),
		 type = get, sub_els = [SubEl]},
    ejabberd_router:route(Packet).

%%%-------------------------
%%% Check protocol support: Receive response: Error
%%%-------------------------

process_iqreply_error(LServiceS, Packet) ->
    FromS = jts(xmpp:get_from(Packet)),
    case search_waiter(FromS, LServiceS, info) of
      {found_waiter, Waiter} ->
	  received_awaiter(FromS, Waiter, LServiceS);
      _ -> ok
    end.

%%%-------------------------
%%% Check protocol support: Receive response: Disco
%%%-------------------------

-spec process_iqreply_result(binary(), iq()) -> any().
process_iqreply_result(LServiceS, #iq{from = From, sub_els = [SubEl]}) ->
    case SubEl of
	#disco_info{} ->
	    process_discoinfo_result(From, LServiceS, SubEl);
	#disco_items{} ->
	    process_discoitems_result(From, LServiceS, SubEl);
	_ ->
	    ok
    end.

%%%-------------------------
%%% Check protocol support: Receive response: Disco Info
%%%-------------------------

process_discoinfo_result(From, LServiceS, DiscoInfo) ->
    FromS = jts(From),
    case search_waiter(FromS, LServiceS, info) of
      {found_waiter, Waiter} ->
	  process_discoinfo_result2(From, FromS, LServiceS, DiscoInfo,
				    Waiter);
      _ -> ok
    end.

process_discoinfo_result2(From, FromS, LServiceS,
			  #disco_info{features = Feats} = DiscoInfo,
			  Waiter) ->
    Multicast_support = lists:member(?NS_ADDRESS, Feats),
    Group = Waiter#waiter.group,
    RServer = Group#group.server,
    case Multicast_support of
	true ->
	    SenderT = sender_type(From),
	    RLimits = get_limits_xml(DiscoInfo, SenderT),
	    add_response(RServer, {multicast_supported, FromS, RLimits}),
	    FromM = Waiter#waiter.sender,
	    DestsM = Group#group.dests,
	    PacketM = Waiter#waiter.packet,
	    AAttrsM = Waiter#waiter.aattrs,
	    AddressesM = Waiter#waiter.addresses,
	    RServiceM = FromS,
	    route_packet_multicast(FromM, RServiceM, PacketM,
		AAttrsM, DestsM, AddressesM, RLimits),
	    delo_waiter(Waiter);
	false ->
	    case FromS of
		RServer ->
		send_query_items(FromS, LServiceS),
		delo_waiter(Waiter),
		add_waiter(Waiter#waiter{awaiting =
					     {[FromS], LServiceS, items},
					 renewal = false});
	    %% We asked a component, and it does not support XEP33
	    _ -> received_awaiter(FromS, Waiter, LServiceS)
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

process_discoitems_result(From, LServiceS, #disco_items{items = Items}) ->
    FromS = jts(From),
    case search_waiter(FromS, LServiceS, items) of
        {found_waiter, Waiter} ->
            List = lists:flatmap(
		     fun(#disco_item{jid = #jid{luser = <<"">>,
						lresource = <<"">>} = J}) ->
			     [J];
			(_) ->
			     []
		     end, Items),
            case List of
                [] ->
                    received_awaiter(FromS, Waiter, LServiceS);
                _ ->
                    [send_query_info(Item, LServiceS) || Item <- List],
                    delo_waiter(Waiter),
                    add_waiter(Waiter#waiter{awaiting =
                                             {List, LServiceS, info},
                                             renewal = false})
            end;
        _ ->
            ok
    end.

%%%-------------------------
%%% Check protocol support: Receive response: Received awaiter
%%%-------------------------

received_awaiter(JID, Waiter, LServiceS) ->
    {JIDs, LServiceS, _} = Waiter#waiter.awaiting,
    delo_waiter(Waiter),
    Group = Waiter#waiter.group,
    RServer = Group#group.server,
    case lists:delete(JID, JIDs) of
      [] ->
	  case Waiter#waiter.renewal of
	    false ->
		add_response(RServer, not_supported),
		From = Waiter#waiter.sender,
		Packet = Waiter#waiter.packet,
		AAttrs = Waiter#waiter.aattrs,
		Others = Group#group.others,
		Addresses = Waiter#waiter.addresses,
		[route_packet(From, ToUser, Packet, AAttrs, Others, Addresses)
		 || ToUser <- Group#group.dests];
	    true ->
		send_query_info(RServer, LServiceS),
		add_waiter(Waiter#waiter{awaiting =
					     {[RServer], LServiceS, info},
					 renewal = false})
	  end;
      JIDs2 ->
	  add_waiter(Waiter#waiter{awaiting =
				       {JIDs2, LServiceS, info},
				   renewal = false})
    end.

%%%-------------------------
%%% Cache
%%%-------------------------

create_cache() ->
    ejabberd_mnesia:create(?MODULE, multicastc,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, multicastc)}]).

add_response(RServer, Response) ->
    Secs = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    mnesia:dirty_write(#multicastc{rserver = RServer,
				   response = Response, ts = Secs}).

search_server_on_cache(RServer, LServerS, _Maxmins)
    when RServer == LServerS ->
    {cached, local_server};
search_server_on_cache(RServer, _LServerS, Maxmins) ->
    case look_server(RServer) of
      not_cached -> not_cached;
      {cached, Response, Ts} ->
	  Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	  case is_obsolete(Response, Ts, Now, Maxmins) of
	    false -> {cached, Response};
	    true -> {obsolete, Response}
	  end
    end.

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
%%% Pool
%%%-------------------------

create_pool() ->
    catch
      begin
          ets:new(multicastp,
                  [duplicate_bag, public, named_table, {keypos, 2}]),
          ets:give_away(multicastp, whereis(ejabberd), ok)
      end.

add_waiter(Waiter) ->
    true = ets:insert(multicastp, Waiter).

delo_waiter(Waiter) ->
    true = ets:delete_object(multicastp, Waiter).

-spec search_waiter(binary(), binary(), info | items) ->
    {found_waiter, #waiter{}} | waiter_not_found.

search_waiter(JID, LServiceS, Type) ->
    Rs = ets:foldl(fun (W, Res) ->
			   {JIDs, LServiceS1, Type1} = W#waiter.awaiting,
			   case lists:member(JID, JIDs) and
				  (LServiceS == LServiceS1)
				  and (Type1 == Type)
			       of
			     true -> Res ++ [W];
			     false -> Res
			   end
		   end,
		   [], multicastp),
    case Rs of
      [R | _] -> {found_waiter, R};
      [] -> waiter_not_found
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
    [{StanzaT, Number}
     || {SenderT2, StanzaT, Number} <- LimitOpts,
	SenderT =:= SenderT2].

build_remote_limit_record(LimitOpts, SenderT) ->
    build_limit_record(LimitOpts, SenderT).

build_limit_record(LimitOpts, SenderT) ->
    Limits = [get_limit_value(Name, Default, LimitOpts)
	      || {Name, Default} <- list_of_limits(SenderT)],
    list_to_tuple([limits | Limits]).

get_limit_value(Name, Default, LimitOpts) ->
    case lists:keysearch(Name, 1, LimitOpts) of
      {value, {Name, Number}} -> {custom, Number};
      false -> {default, Default}
    end.

type_of_stanza(Stanza) -> element(1, Stanza).

get_limit_number(message, Limits) ->
    Limits#limits.message;
get_limit_number(presence, Limits) ->
    Limits#limits.presence.

get_slimit_group(local, SLimits) ->
    SLimits#service_limits.local;
get_slimit_group(remote, SLimits) ->
    SLimits#service_limits.remote.

fragment_dests(Dests, Limit_number) ->
    {R, _} = lists:foldl(fun (Dest, {Res, Count}) ->
				 case Count of
				   Limit_number ->
				       Head2 = [Dest], {[Head2 | Res], 0};
				   _ ->
				       [Head | Tail] = Res,
				       Head2 = [Dest | Head],
				       {[Head2 | Tail], Count + 1}
				 end
			 end,
			 {[[]], 0}, Dests),
    R.

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
	    #xdata{type = result,
		   fields = [?RFIELDT(hidden, <<"FORM_TYPE">>, ?NS_ADDRESS)
			     | List_limits_xmpp]}
    end.

sender_type(From) ->
    Local_hosts = (?MYHOSTS),
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
    fun acl:access_rules_validator/1;
mod_opt_type(host) -> fun iolist_to_binary/1;
mod_opt_type(limits) ->
    fun (A) when is_list(A) -> A end;
mod_opt_type(_) -> [access, host, limits].
