%%%----------------------------------------------------------------------
%%% File    : mod_antispam_rtbl.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Collection of RTBL specific functionality
%%% Created : 20 Mar 2025 by Stefan Strigler <stefan@strigler.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2026 ProcessOne
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
-module(mod_antispam_rtbl).
-author('stefan@strigler.de').

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("mod_antispam.hrl").

-define(SERVICE_JID_PREFIX, "rtbl-").

-export([parse_iq_result/3,
	 pubsub_event_handler/1,
         cancel_timers/1,
	 get_items/2,
	 subscribe/2,
	 unsubscribe/2]).

%% @format-begin

subscribe(Host, Services) ->
    for_all_services_and_nodes(fun(RTBLHost, {RTBLNode, _}) ->
                                  FromJID = service_jid(Host),
                                  SubIQ =
                                      #iq{type = set,
                                          to = jid:make(RTBLHost),
                                          from = FromJID,
                                          sub_els =
                                              [#pubsub{subscribe =
                                                           #ps_subscribe{jid = FromJID,
                                                                         node = RTBLNode}}]},
                                  {SubIQ, {subscribe_result, RTBLHost, RTBLNode}}
                               end,
                               Services).

-spec unsubscribe(binary(), rtbl_services()) -> ok.
unsubscribe(Host, Services) ->
    for_all_services_and_nodes(fun (RTBLHost,
                                    {RTBLNode, #rtbl_service_node{subscribed = true}}) ->
                                       FromJID = service_jid(Host),
                                       SubIQ =
                                           #iq{type = set,
                                               to = jid:make(RTBLHost),
                                               from = FromJID,
                                               sub_els =
                                                   [#pubsub{unsubscribe =
                                                                #ps_unsubscribe{jid = FromJID,
                                                                                node = RTBLNode}}]},
                                       {SubIQ, {unsubscribe_result, RTBLHost, RTBLNode}};
                                   (_, _) ->
                                       ok
                               end,
                               Services).

-spec get_items(binary(), rtbl_services()) -> ok.
get_items(Host, Services) ->
    for_all_services_and_nodes(fun(RTBLHost, {RTBLNode, _}) ->
                                  IQ = #iq{type = get,
                                           from = jid:make(Host),
                                           to = jid:make(RTBLHost),
                                           sub_els = [#pubsub{items = #ps_items{node = RTBLNode}}]},
                                  {IQ, {get_items, RTBLHost, RTBLNode}}
                               end,
                               Services).

cancel_timers(Services) ->
    for_all_services_and_nodes(fun(_RTBLHost, {_, #rtbl_service_node{retry_timer = TRef}}) ->
                                  misc:cancel_timer(TRef)
                               end,
                               Services).

-spec parse_iq_result(stanza(), binary(), rtbl_service_node_type()) ->
                         #{binary() => any()} | undefined.
parse_iq_result(#iq{to = #jid{lserver = LServer}, type = result} = IQ,
                RTBLNode,
                RTBLServiceNodeType) ->
    ?DEBUG("parsing iq-result items: ~p", [IQ]),
    case xmpp:get_subtag(IQ, #pubsub{}) of
        #pubsub{items = #ps_items{node = RTBLNode, items = Items}} ->
            ParsedItems = parse_items(Items),
            ejabberd_hooks:run(antispam_rtbl_items,
                               LServer,
                               [LServer, {RTBLServiceNodeType, {[], ParsedItems}}]),
            ParsedItems;
        _ ->
            undefined
    end.

%%--------------------------------------------------------------------
%% Hook callbacks.
%%--------------------------------------------------------------------

-spec pubsub_event_handler(stanza()) -> stop | ok.
pubsub_event_handler(#message{from = FromJid,
                              to =
                                  #jid{lserver = LServer,
                                       lresource = <<?SERVICE_JID_PREFIX, _/binary>>}} =
                         Msg) ->
    ?DEBUG("Got RTBL message:~n~p", [Msg]),
    From = jid:encode(FromJid),
    RTBLServices = mod_antispam:get_rtbl_services_option(LServer),
    case parse_pubsub_event(Msg) of
        {RTBLNode, ParsedItems} ->
            case RTBLServices of
                #{From := #{RTBLNode := RTBLServiceNode}} ->
                    mod_antispam:update_blocked_items(LServer,
                                                      From,
                                                      RTBLNode,
                                                      RTBLServiceNode#rtbl_service_node.type,
                                                      ParsedItems),
                    ejabberd_hooks:run(antispam_rtbl_items,
                                       LServer,
                                       [LServer,
                                        {RTBLServiceNode#rtbl_service_node.type, ParsedItems}]),
                    stop;
                _NoMatch ->
                    ?INFO_MSG("Got unexpected message from ~s to rtbl resource:~n~p", [From, Msg]),
                    ok
            end;
        undefined ->
            ok
    end;
pubsub_event_handler(Other) ->
    ?DEBUG("unexpected something on pubsub_event_handler: ~p", [Other]),
    ok.

%%--------------------------------------------------------------------
%% Helpers.
%%--------------------------------------------------------------------

parse_pubsub_event(#message{to = #jid{lserver = _LServer}} = Msg) ->
    case xmpp:get_subtag(Msg, #ps_event{}) of
        #ps_event{items =
                      #ps_items{node = Node,
                                items = Items,
                                retract = RetractIds}} ->
            {Node, {RetractIds, parse_items(Items)}};
        Other ->
            ?WARNING_MSG("Couldn't extract items: ~p", [Other]),
            undefined
    end.

-spec parse_items([ps_item()]) -> #{binary() => any()}.
parse_items(Items) ->
    lists:foldl(fun(#ps_item{id = ID}, Acc) ->
                   %% TODO extract meta/extra instructions
                   Acc#{ID => true}
                end,
                #{},
                Items).

-spec service_jid(binary()) -> jid().
service_jid(Host) ->
    jid:make(<<>>, Host, <<?SERVICE_JID_PREFIX, (ejabberd_cluster:node_id())/binary>>).
%% @format-end

for_all_services_and_nodes(Fun, Services) ->
    [maybe_send_iq(Fun, RTBLHost, NodeName, Node)
     || {RTBLHost, Nodes} <- maps:to_list(Services), {NodeName, Node} <- maps:to_list(Nodes)],
    ok.

maybe_send_iq(Fun, RTBLHost, NodeName, Node) ->
    case Fun(RTBLHost, {NodeName, Node}) of
        {IQ, State} ->
            ?DEBUG("Sending service request: host=~s: node=~s state=~p~n~p", [RTBLHost, NodeName, State, xmpp:encode(IQ)]),
            ejabberd_router:route_iq(IQ, State, self());
        _ ->
            ok
    end.
