%%%----------------------------------------------------------------------
%%% File    : mod_multicast.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Extended Stanza Addressing (XEP-0033) support
%%% Created : 29 May 2007 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
-export([start_link/2, start/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3,
	 handle_cast/2, terminate/2, code_change/3]).

-export([purge_loop/1, mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-record(state,
	{lserver, lservice, access, service_limits}).

-record(multicastc, {rserver, response, ts}).

%% ts: timestamp (in seconds) when the cache item was last updated

-record(dest, {jid_string, jid_jid, type, full_xml}).

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

-define(PROCNAME, ejabberd_mod_multicast).

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

start_link(LServerS, Opts) ->
    Proc = gen_mod:get_module_proc(LServerS, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [LServerS, Opts], []).

start(LServerS, Opts) ->
    Proc = gen_mod:get_module_proc(LServerS, ?PROCNAME),
    ChildSpec = {Proc,
		 {?MODULE, start_link, [LServerS, Opts]}, temporary,
		 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(LServerS) ->
    Proc = gen_mod:get_module_proc(LServerS, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([LServerS, Opts]) ->
    LServiceS = gen_mod:get_opt_host(LServerS, Opts,
				     <<"multicast.@HOST@">>),
    Access = gen_mod:get_opt(access, Opts,
			     fun (A) when is_atom(A) -> A end, all),
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
    ejabberd_router:register_route(LServiceS),
    {ok,
     #state{lservice = LServiceS, lserver = LServerS,
	    access = Access, service_limits = SLimits}}.

handle_call(stop, _From, State) ->
    try_stop_loop(), {stop, normal, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info({route, From, To,
	     #xmlel{name = <<"iq">>, attrs = Attrs} = Packet},
	    State) ->
    case catch handle_iq(From, To, #xmlel{attrs = Attrs} = Packet, State) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("Error when processing IQ stanza: ~p",
                       [Reason]);
        _ -> ok
    end,
    {noreply, State};
%% XEP33 allows only 'message' and 'presence' stanza type
handle_info({route, From, To,
	     #xmlel{name = Stanza_type} = Packet},
	    #state{lservice = LServiceS, lserver = LServerS,
		   access = Access, service_limits = SLimits} =
		State)
    when (Stanza_type == <<"message">>) or
	   (Stanza_type == <<"presence">>) ->
    route_untrusted(LServiceS, LServerS, Access, SLimits,
		    From, To, Packet),
    {noreply, State};
%% Handle multicast packets sent by trusted local services
handle_info({route_trusted, From, Destinations, Packet},
	    #state{lservice = LServiceS, lserver = LServerS} =
		State) ->
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

handle_iq(From, To, #xmlel{attrs = Attrs} = Packet, State) ->
    IQ = jlib:iq_query_info(Packet),
    case catch process_iq(From, IQ, State) of
        Result when is_record(Result, iq) ->
            ejabberd_router:route(To, From, jlib:iq_to_xml(Result));
        {'EXIT', Reason} ->
            ?ERROR_MSG("Error when processing IQ stanza: ~p",
                       [Reason]),
            Err = jlib:make_error_reply(Packet,
                                        ?ERR_INTERNAL_SERVER_ERROR),
            ejabberd_router:route(To, From, Err);
        reply ->
            LServiceS = jts(To),
            case xml:get_attr_s(<<"type">>, Attrs) of
                <<"result">> ->
                    process_iqreply_result(From, LServiceS, Packet, State);
                <<"error">> ->
                    process_iqreply_error(From, LServiceS, Packet)
            end;
        ok -> ok
    end.

process_iq(From,
	   #iq{type = get, xmlns = ?NS_DISCO_INFO, lang = Lang} =
	       IQ,
	   State) ->
    IQ#iq{type = result,
	  sub_el =
	      [#xmlel{name = <<"query">>,
		      attrs = [{<<"xmlns">>, ?NS_DISCO_INFO}],
		      children = iq_disco_info(From, Lang, State)}]};
%% disco#items request
process_iq(_,
	   #iq{type = get, xmlns = ?NS_DISCO_ITEMS} = IQ, _) ->
    IQ#iq{type = result,
	  sub_el =
	      [#xmlel{name = <<"query">>,
		      attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS}],
		      children = []}]};
%% vCard request
process_iq(_,
	   #iq{type = get, xmlns = ?NS_VCARD, lang = Lang} = IQ,
	   _) ->
    IQ#iq{type = result,
	  sub_el =
	      [#xmlel{name = <<"vCard">>,
		      attrs = [{<<"xmlns">>, ?NS_VCARD}],
		      children = iq_vcard(Lang)}]};
%% Unknown "set" or "get" request
process_iq(_, #iq{type = Type, sub_el = SubEl} = IQ, _)
    when Type == get; Type == set ->
    IQ#iq{type = error,
	  sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]};
%% IQ "result" or "error".
process_iq(_, reply, _) -> reply;
%% IQ "result" or "error".
process_iq(_, _, _) -> ok.

-define(FEATURE(Feat),
	#xmlel{name = <<"feature">>,
	       attrs = [{<<"var">>, Feat}], children = []}).

iq_disco_info(From, Lang, State) ->
    [#xmlel{name = <<"identity">>,
	    attrs =
		[{<<"category">>, <<"service">>},
		 {<<"type">>, <<"multicast">>},
		 {<<"name">>,
		  translate:translate(Lang, <<"Multicast">>)}],
	    children = []},
     ?FEATURE((?NS_DISCO_INFO)), ?FEATURE((?NS_DISCO_ITEMS)),
     ?FEATURE((?NS_VCARD)), ?FEATURE((?NS_ADDRESS))]
      ++ iq_disco_info_extras(From, State).

iq_vcard(Lang) ->
    [#xmlel{name = <<"FN">>, attrs = [],
	    children = [{xmlcdata, <<"ejabberd/mod_multicast">>}]},
     #xmlel{name = <<"URL">>, attrs = [],
	    children = [{xmlcdata, ?EJABBERD_URI}]},
     #xmlel{name = <<"DESC">>, attrs = [],
	    children =
		[{xmlcdata,
                  <<(translate:translate(Lang,
                                      <<"ejabberd Multicast service">>))/binary,
                                        "\nCopyright (c) 2002-2015 ProcessOne">>}]}].

%%%-------------------------
%%% Route
%%%-------------------------

route_trusted(LServiceS, LServerS, FromJID,
	      Destinations, Packet) ->
    Packet_stripped = Packet,
    AAttrs = [{<<"xmlns">>, ?NS_ADDRESS}],
    Delivereds = [],
    Dests2 = lists:map(fun (D) ->
			       DS = jts(D),
			       XML = #xmlel{name = <<"address">>,
					    attrs =
						[{<<"type">>, <<"bcc">>},
						 {<<"jid">>, DS}],
					    children = []},
			       #dest{jid_string = DS, jid_jid = D,
				     type = <<"bcc">>, full_xml = XML}
		       end,
		       Destinations),
    Groups = group_dests(Dests2),
    route_common(LServerS, LServiceS, FromJID, Groups,
		 Delivereds, Packet_stripped, AAttrs).

route_untrusted(LServiceS, LServerS, Access, SLimits,
		From, To, Packet) ->
    try route_untrusted2(LServiceS, LServerS, Access,
			 SLimits, From, Packet)
    catch
      adenied ->
	  route_error(To, From, Packet, forbidden,
		      <<"Access denied by service policy">>);
      eadsele ->
	  route_error(To, From, Packet, bad_request,
		      <<"No addresses element found">>);
      eadeles ->
	  route_error(To, From, Packet, bad_request,
		      <<"No address elements found">>);
      ewxmlns ->
	  route_error(To, From, Packet, bad_request,
		      <<"Wrong xmlns">>);
      etoorec ->
	  route_error(To, From, Packet, not_acceptable,
		      <<"Too many receiver fields were specified">>);
      edrelay ->
	  route_error(To, From, Packet, forbidden,
		      <<"Packet relay is denied by service policy">>);
      EType:EReason ->
	  ?ERROR_MSG("Multicast unknown error: Type: ~p~nReason: ~p",
		     [EType, EReason]),
	  route_error(To, From, Packet, internal_server_error,
		      <<"Unknown problem">>)
    end.

route_untrusted2(LServiceS, LServerS, Access, SLimits,
		 FromJID, Packet) ->
    ok = check_access(LServerS, Access, FromJID),
    {ok, Packet_stripped, AAttrs, Addresses} =
	strip_addresses_element(Packet),
    {To_deliver, Delivereds} =
	split_addresses_todeliver(Addresses),
    Dests = convert_dest_record(To_deliver),
    {Dests2, Not_jids} = split_dests_jid(Dests),
    report_not_jid(FromJID, Packet, Not_jids),
    ok = check_limit_dests(SLimits, FromJID, Packet,
			   Dests2),
    Groups = group_dests(Dests2),
    ok = check_relay(FromJID#jid.server, LServerS, Groups),
    route_common(LServerS, LServiceS, FromJID, Groups,
		 Delivereds, Packet_stripped, AAttrs).

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
		  Group#group.addresses)
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

strip_addresses_element(Packet) ->
    case xml:get_subtag(Packet, <<"addresses">>) of
      #xmlel{name = <<"addresses">>, attrs = AAttrs,
	     children = Addresses} ->
	  case xml:get_attr_s(<<"xmlns">>, AAttrs) of
	    ?NS_ADDRESS ->
		#xmlel{name = Name, attrs = Attrs, children = Els} =
		    Packet,
		Els_stripped = lists:keydelete(<<"addresses">>, 2, Els),
		Packet_stripped = #xmlel{name = Name, attrs = Attrs,
					 children = Els_stripped},
		{ok, Packet_stripped, AAttrs, xml:remove_cdata(Addresses)};
	    _ -> throw(ewxmlns)
	  end;
      _ -> throw(eadsele)
    end.

%%%-------------------------
%%% Split Addresses
%%%-------------------------

split_addresses_todeliver(Addresses) ->
    lists:partition(fun (XML) ->
			    case XML of
			      #xmlel{name = <<"address">>, attrs = Attrs} ->
				  case xml:get_attr_s(<<"delivered">>, Attrs) of
				    <<"true">> -> false;
				    _ ->
					Type = xml:get_attr_s(<<"type">>,
							      Attrs),
					case Type of
					  <<"to">> -> true;
					  <<"cc">> -> true;
					  <<"bcc">> -> true;
					  _ -> false
					end
				  end;
			      _ -> false
			    end
		    end,
		    Addresses).

%%%-------------------------
%%% Check does not exceed limit of destinations
%%%-------------------------

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

convert_dest_record(XMLs) ->
    lists:map(fun (XML) ->
		      case xml:get_tag_attr_s(<<"jid">>, XML) of
			<<"">> -> #dest{jid_string = none, full_xml = XML};
			JIDS ->
			    Type = xml:get_tag_attr_s(<<"type">>, XML),
			    JIDJ = stj(JIDS),
			    #dest{jid_string = JIDS, jid_jid = JIDJ,
				  type = Type, full_xml = XML}
		      end
	      end,
	      XMLs).

%%%-------------------------
%%% Split destinations by existence of JID
%%% and send error messages for other dests
%%%-------------------------

split_dests_jid(Dests) ->
    lists:partition(fun (Dest) ->
			    case Dest#dest.jid_string of
			      none -> false;
			      _ -> true
			    end
		    end,
		    Dests).

report_not_jid(From, Packet, Dests) ->
    Dests2 = [xml:element_to_binary(Dest#dest.full_xml)
	      || Dest <- Dests],
    [route_error(From, From, Packet, jid_malformed,
		 <<"This service can not process the address: ",
		   D/binary>>)
     || D <- Dests2].

%%%-------------------------
%%% Group destinations by their servers
%%%-------------------------

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
			  <<"to">> -> [add_delivered(XML) | R];
			  <<"cc">> -> [add_delivered(XML) | R];
			  <<"bcc">> -> R;
			  _ -> [XML | R]
			end
		end,
		[], Dests).

add_delivered(#xmlel{name = Name, attrs = Attrs,
		     children = Els}) ->
    Attrs2 = [{<<"delivered">>, <<"true">>} | Attrs],
    #xmlel{name = Name, attrs = Attrs2, children = Els}.

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

route_packet(From, ToDest, Packet, AAttrs, Addresses) ->
    Dests = case ToDest#dest.type of
	      <<"bcc">> -> [];
	      _ -> [ToDest]
	    end,
    route_packet2(From, ToDest#dest.jid_string, Dests,
		  Packet, AAttrs, Addresses).

route_packet_multicast(From, ToS, Packet, AAttrs, Dests,
		       Addresses, Limits) ->
    Type_of_stanza = type_of_stanza(Packet),
    {_Type, Limit_number} = get_limit_number(Type_of_stanza,
					     Limits),
    Fragmented_dests = fragment_dests(Dests, Limit_number),
    [route_packet2(From, ToS, DFragment, Packet, AAttrs,
		   Addresses)
     || DFragment <- Fragmented_dests].

route_packet2(From, ToS, Dests, Packet, AAttrs,
	      Addresses) ->
    #xmlel{name = T, attrs = A, children = C} = Packet,
    C2 = case append_dests(Dests, Addresses) of
	   [] -> C;
	   ACs ->
	       [#xmlel{name = <<"addresses">>, attrs = AAttrs,
		       children = ACs}
		| C]
	 end,
    Packet2 = #xmlel{name = T, attrs = A, children = C2},
    ToJID = stj(ToS),
    ejabberd_router:route(From, ToJID, Packet2).

append_dests([], Addresses) -> Addresses;
append_dests([Dest | Dests], Addresses) ->
    append_dests(Dests, [Dest#dest.full_xml | Addresses]).

%%%-------------------------
%%% Check relay
%%%-------------------------

check_relay(RS, LS, Gs) ->
    case check_relay_required(RS, LS, Gs) of
      false -> ok;
      true -> throw(edrelay)
    end.

check_relay_required(RServer, LServerS, Groups) ->
    case lists:suffix(str:tokens(LServerS, <<".">>),
                      str:tokens(RServer, <<".">>)) of
      true -> false;
      false -> check_relay_required(LServerS, Groups)
    end.

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
      _ -> send_query(RServerS, LServiceS, ?NS_DISCO_INFO)
    end.

send_query_items(RServerS, LServiceS) ->
    send_query(RServerS, LServiceS, ?NS_DISCO_ITEMS).

send_query(RServerS, LServiceS, XMLNS) ->
    Packet = #xmlel{name = <<"iq">>,
		    attrs = [{<<"to">>, RServerS}, {<<"type">>, <<"get">>}],
		    children =
			[#xmlel{name = <<"query">>,
				attrs = [{<<"xmlns">>, XMLNS}],
				children = []}]},
    ejabberd_router:route(stj(LServiceS), stj(RServerS),
			  Packet).

%%%-------------------------
%%% Check protocol support: Receive response: Error
%%%-------------------------

process_iqreply_error(From, LServiceS, _Packet) ->
    FromS = jts(From),
    case search_waiter(FromS, LServiceS, info) of
      {found_waiter, Waiter} ->
	  received_awaiter(FromS, Waiter, LServiceS);
      _ -> ok
    end.

%%%-------------------------
%%% Check protocol support: Receive response: Disco
%%%-------------------------

process_iqreply_result(From, LServiceS, Packet, State) ->
    #xmlel{name = <<"query">>, attrs = Attrs2,
	   children = Els2} =
	xml:get_subtag(Packet, <<"query">>),
    case xml:get_attr_s(<<"xmlns">>, Attrs2) of
      ?NS_DISCO_INFO ->
	  process_discoinfo_result(From, LServiceS, Els2, State);
      ?NS_DISCO_ITEMS ->
	  process_discoitems_result(From, LServiceS, Els2)
    end.

%%%-------------------------
%%% Check protocol support: Receive response: Disco Info
%%%-------------------------

process_discoinfo_result(From, LServiceS, Els,
			 _State) ->
    FromS = jts(From),
    case search_waiter(FromS, LServiceS, info) of
      {found_waiter, Waiter} ->
	  process_discoinfo_result2(From, FromS, LServiceS, Els,
				    Waiter);
      _ -> ok
    end.

process_discoinfo_result2(From, FromS, LServiceS, Els,
			  Waiter) ->
    Multicast_support =
	lists:any(
	    fun(XML) ->
		    case XML of
			#xmlel{name = <<"feature">>, attrs = Attrs} ->
			    (?NS_ADDRESS) == xml:get_attr_s(<<"var">>, Attrs);
			_ -> false
		    end
	    end,
	    Els),
    Group = Waiter#waiter.group,
    RServer = Group#group.server,
    case Multicast_support of
	true ->
	    SenderT = sender_type(From),
	    RLimits = get_limits_xml(Els, SenderT),
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

get_limits_xml(Els, SenderT) ->
    LimitOpts = get_limits_els(Els),
    build_remote_limit_record(LimitOpts, SenderT).

get_limits_els(Els) ->
    lists:foldl(fun (XML, R) ->
			case XML of
			  #xmlel{name = <<"x">>, attrs = Attrs,
				 children = SubEls} ->
			      case ((?NS_XDATA) ==
				      xml:get_attr_s(<<"xmlns">>, Attrs))
				     and
				     (<<"result">> ==
					xml:get_attr_s(<<"type">>, Attrs))
				  of
				true -> get_limits_fields(SubEls) ++ R;
				false -> R
			      end;
			  _ -> R
			end
		end,
		[], Els).

get_limits_fields(Fields) ->
    {Head, Tail} = lists:partition(fun (Field) ->
					   case Field of
					     #xmlel{name = <<"field">>,
						    attrs = Attrs} ->
						 (<<"FORM_TYPE">> ==
						    xml:get_attr_s(<<"var">>,
								   Attrs))
						   and
						   (<<"hidden">> ==
						      xml:get_attr_s(<<"type">>,
								     Attrs));
					     _ -> false
					   end
				   end,
				   Fields),
    case Head of
      [] -> [];
      _ -> get_limits_values(Tail)
    end.

get_limits_values(Values) ->
    lists:foldl(fun (Value, R) ->
			case Value of
			  #xmlel{name = <<"field">>, attrs = Attrs,
				 children = SubEls} ->
			      [#xmlel{name = <<"value">>, children = SubElsV}] =
				  SubEls,
			      Number = xml:get_cdata(SubElsV),
			      Name = xml:get_attr_s(<<"var">>, Attrs),
			      [{jlib:binary_to_atom(Name),
				jlib:binary_to_integer(Number)}
			       | R];
			  _ -> R
			end
		end,
		[], Values).

%%%-------------------------
%%% Check protocol support: Receive response: Disco Items
%%%-------------------------

process_discoitems_result(From, LServiceS, Els) ->
    FromS = jts(From),
    case search_waiter(FromS, LServiceS, items) of
        {found_waiter, Waiter} ->
            List = lists:foldl(
                     fun(XML, Res) ->
                             case XML of
                                 #xmlel{name = <<"item">>, attrs = Attrs} ->
                                     SJID = xml:get_attr_s(<<"jid">>, Attrs),
                                     case jid:from_string(SJID) of
                                         #jid{luser = <<"">>,
                                              lresource = <<"">>} ->
                                             [SJID | Res];
                                         _ -> Res
                                     end;
                                 _ -> Res
                             end
                     end,
                     [], Els),
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
		Addresses = Waiter#waiter.addresses,
		[route_packet(From, ToUser, Packet, AAttrs, Addresses)
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
    mnesia:create_table(multicastc,
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

type_of_stanza(#xmlel{name = <<"message">>}) -> message;
type_of_stanza(#xmlel{name = <<"presence">>}) ->
    presence.

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
	#xmlel{name = <<"field">>,
	       attrs = [{<<"var">>, Var}, {<<"type">>, Type}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children = [{xmlcdata, Val}]}]}).

-define(RFIELDV(Var, Val),
	#xmlel{name = <<"field">>, attrs = [{<<"var">>, Var}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children = [{xmlcdata, Val}]}]}).

iq_disco_info_extras(From, State) ->
    SenderT = sender_type(From),
    Service_limits = State#state.service_limits,
    case iq_disco_info_extras2(SenderT, Service_limits) of
      [] -> [];
      List_limits_xmpp ->
	  [#xmlel{name = <<"x">>,
		  attrs =
		      [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"result">>}],
		  children =
		      [?RFIELDT(<<"hidden">>, <<"FORM_TYPE">>, (?NS_ADDRESS))]
			++ List_limits_xmpp}]
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

route_error(From, To, Packet, ErrType, ErrText) ->
    #xmlel{attrs = Attrs} = Packet,
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    Reply = make_reply(ErrType, Lang, ErrText),
    Err = jlib:make_error_reply(Packet, Reply),
    ejabberd_router:route(From, To, Err).

make_reply(bad_request, Lang, ErrText) ->
    ?ERRT_BAD_REQUEST(Lang, ErrText);
make_reply(jid_malformed, Lang, ErrText) ->
    ?ERRT_JID_MALFORMED(Lang, ErrText);
make_reply(not_acceptable, Lang, ErrText) ->
    ?ERRT_NOT_ACCEPTABLE(Lang, ErrText);
make_reply(internal_server_error, Lang, ErrText) ->
    ?ERRT_INTERNAL_SERVER_ERROR(Lang, ErrText);
make_reply(forbidden, Lang, ErrText) ->
    ?ERRT_FORBIDDEN(Lang, ErrText).

stj(String) -> jid:from_string(String).

jts(String) -> jid:to_string(String).

mod_opt_type(access) ->
    fun (A) when is_atom(A) -> A end;
mod_opt_type(host) -> fun iolist_to_binary/1;
mod_opt_type(limits) ->
    fun (A) when is_list(A) -> A end;
mod_opt_type(_) -> [access, host, limits].
