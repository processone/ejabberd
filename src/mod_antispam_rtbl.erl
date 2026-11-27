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

-export([cancel_timers/1,
         get_items/2,
         get_rtbl_exports/1,
         get_rtbl_service_node/3,
         get_rtbl_services_option/1,
         item_id_and_payload/1,
         parse_iq_result/3,
         publish_blocked/3,
         publish_unblocked/3,
         pubsub_event_handler/1,
         set_retry_timer/4,
         set_rtbl_service_node/4,
         start/2,
         stop/1,
         subscribe/3,
         unsubscribe/2
        ]).

-type rtbl_block_item() :: binary() | {binary(), spam | abuse, binary()}.
-export_type([rtbl_block_item/0]).

%% @format-begin

start(Server, Opts) ->
    try get_rtbl_exports(Server, mod_antispam_opt:rtbl_exports(Opts)) of
        none ->
            ok;
        RTBLExports ->
            Owner = jid:make(Server),
            maps:foreach(fun(_Type, Nodes) ->
                            lists:foreach(fun({Host, Node}) ->
                                             mod_pubsub:create_node(Host,
                                                                    Server,
                                                                    Node,
                                                                    Owner,
                                                                    <<"flat">>)
                                          end,
                                          Nodes)
                         end,
                         RTBLExports)
    catch
        error:({module_not_loaded, mod_pubsub, Server} = E) ->
            ?CRITICAL_MSG("'mod_antispam->rtbl_exports' configured with option 'auto' but 'mod_pubsub' not loaded for host ~s",
                          [Server]),
            {error, E}
    end.

-spec get_rtbl_exports(Server :: binary()) ->
                          none |
                          #{muc_bans | spam_source_domains =>
                                [{PHost :: binary(), Node :: binary()}]}.
get_rtbl_exports(Server) ->
    get_rtbl_exports(Server, mod_antispam_opt:rtbl_exports(Server)).

-spec get_rtbl_exports(Server :: binary(), none | auto | [[{PHost, Nodes :: [Node]}]]) ->
                          none | #{muc_bans | spam_source_domains => [{PHost, Node}]}
    when Node :: binary(),
         PHost :: binary().
get_rtbl_exports(_Server, none) ->
    none;
get_rtbl_exports(Server, auto) ->
    Host = hd(gen_mod:get_module_opt_hosts(Server, mod_pubsub)),
    #{muc_bans => [{Host, ?DEFAULT_MUC_BANS_NODE}],
      spam_source_domains => [{Host, ?DEFAULT_SPAM_SOURCE_DOMAINS_NODE}]};
get_rtbl_exports(_Server, Opts) ->
    lists:foldl(fun to_rtbl_export/2, #{}, Opts).

to_rtbl_export([{Host, Nodes}], M) ->
    lists:foldl(fun({Type, NodeName}, M2) -> add_node(M2, node_type(Type), {Host, NodeName})
                end,
                M,
                Nodes);
to_rtbl_export(Host, M) ->
    add_node(add_node(M, muc_bans, {Host, ?DEFAULT_MUC_BANS_NODE}),
             spam_source_domains,
             {Host, ?DEFAULT_SPAM_SOURCE_DOMAINS_NODE}).

add_node(M, Type, Node) ->
    case M of
        #{Type := Nodes} ->
            M#{Type => [Node | Nodes]};
        _ ->
            M#{Type => [Node]}
    end.

stop(_Host) ->
    ok.

get_rtbl_services_option(HostOrOpts) ->
    Services = mod_antispam_opt:rtbl_services(HostOrOpts),
    lists:foldl(fun to_rtbl_service/2, #{}, Services).

to_rtbl_service([{Host, Nodes}], M) ->
    M#{Host => lists:foldl(fun to_rtbl_service_nodes/2, #{}, Nodes)};
to_rtbl_service(<<"xmppbl.org">> = Host, M) ->
    M#{Host =>
           #{?DEFAULT_SPAM_SOURCE_DOMAINS_NODE => #rtbl_service_node{},
             ?XMPPBL_MUC_BANS_NODE => #rtbl_service_node{type = muc_bans}}};
to_rtbl_service(Host, M) ->
    M#{Host =>
           #{?DEFAULT_SPAM_SOURCE_DOMAINS_NODE => #rtbl_service_node{},
             ?DEFAULT_MUC_BANS_NODE => #rtbl_service_node{type = muc_bans}}}.

to_rtbl_service_nodes({Type, none}, M)
    when Type == spam_source_domains_node; Type == muc_bans_node ->
    M;
to_rtbl_service_nodes({Type, Node}, M)
    when Type == spam_source_domains_node; Type == muc_bans_node ->
    M#{Node => #rtbl_service_node{type = node_type(Type)}};
to_rtbl_service_nodes({UnknownType, Node}, M) ->
    ?WARNING_MSG("Unknown RTBL node type configured: type=~p node=~s", [UnknownType, Node]),
    M.

node_type(spam_source_domains_node) ->
    spam_source_domains;
node_type(muc_bans_node) ->
    muc_bans.

get_rtbl_service_node(RTBLServices, RTBLHost, RTBLNode) ->
    #{RTBLHost := #{RTBLNode := RTBLServiceNode}} = RTBLServices,
    RTBLServiceNode.

set_retry_timer(RTBLServices, RTBLHost, RTBLNode, TRef) ->
    #{RTBLHost := #{RTBLNode := RTBLServiceNode} = RTBLNodes} = RTBLServices,
    RTBLServices#{RTBLHost =>
                      RTBLNodes#{RTBLNode =>
                                     RTBLServiceNode#rtbl_service_node{retry_timer = TRef}}}.

set_rtbl_service_node(RTBLServices, RTBLHost, RTBLNode, RTBLServiceNode) ->
    #{RTBLHost := RTBLNodes} = RTBLServices,
    RTBLServices#{RTBLHost => RTBLNodes#{RTBLNode => RTBLServiceNode}}.

subscribe(Host, RTBLHost, RTBLNode) ->
    FromJID = service_jid(Host),
    IQ = #iq{type = set,
             to = jid:make(RTBLHost),
             from = FromJID,
             sub_els = [#pubsub{subscribe = #ps_subscribe{jid = FromJID, node = RTBLNode}}]},
    ejabberd_router:route_iq(IQ, {subscribe_result, RTBLHost, RTBLNode}, self()).

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

-spec publish_blocked(binary(),
                      rtbl_service_node_type(),
                      rtbl_block_item() | [rtbl_block_item()]) ->
                         ok.
publish_blocked(Host, Type, ItemOrItems) ->
    publish_to_exports(Host, Type, ItemOrItems, fun publish_item/5).

-spec publish_unblocked(binary(),
                        rtbl_service_node_type(),
                        rtbl_block_item() | [rtbl_block_item()]) ->
                           ok.
publish_unblocked(Host, Type, ItemOrItems) ->
    publish_to_exports(Host, Type, ItemOrItems, fun delete_item/5).

publish_to_exports(Host, Type, Items, PublishFun) when is_list(Items) ->
    lists:foreach(fun(Item) -> publish_to_exports(Host, Type, Item, PublishFun) end, Items);
publish_to_exports(Host, Type, Item, PublishFun) ->
    case get_rtbl_exports(Host) of
        #{Type := HostsAndNodes} ->
            Publisher = jid:make(Host),
            lists:foreach(fun({PHost, Node}) -> PublishFun(PHost, Host, Node, Publisher, Item) end,
                          HostsAndNodes);
        _ ->
            ok
    end.

publish_item(PHost, Host, Node, Publisher, Item) ->
    {ItemId, Payload} = item_id_and_payload(Item),
    mod_pubsub:publish_item(PHost, Host, Node, Publisher, ItemId, Payload).

delete_item(PHost, _Host, Node, Publisher, Item) ->
    {ItemId, _} = item_id_and_payload(Item),
    mod_pubsub:delete_item(PHost, Node, Publisher, ItemId, true).

-spec item_id_and_payload(Item :: rtbl_block_item()) -> {binary(), [xmlel()]}.
item_id_and_payload(Item) when is_binary(Item) ->
    Payload = [xmpp:encode(#report{reason = spam})],
    {Item, Payload};
item_id_and_payload({ItemId, Reason, Text}) ->
    Payload = [xmpp:encode(#report{reason = Reason, text = [#text{data = Text}]})],
    {ItemId, Payload}.

%%--------------------------------------------------------------------
%% Hook callbacks.
%%--------------------------------------------------------------------

-spec pubsub_event_handler(stanza()) -> stop | ok.
pubsub_event_handler(#message{from = FromJid,
                              to =
                                  #jid{lserver = LServer,
                                       lresource = <<?SERVICE_JID_PREFIX, _/binary>>}} =
                         Msg) ->
    ?DEBUG("Got RTBL message (~p):~n~p", [self(), Msg]),
    From = jid:encode(FromJid),
    RTBLServices = get_rtbl_services_option(LServer),
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
    lists:foldl(fun(#ps_item{id = ID, sub_els = SubEls}, Acc) -> Acc#{ID => SubEls} end,
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
