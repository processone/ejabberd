%%%----------------------------------------------------------------------
%%% File    : mod_antispam_rtbl.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Collection of RTBL specific functionality
%%% Created : 20 Mar 2025 by Stefan Strigler <stefan@strigler.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2025 ProcessOne
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

-define(SERVICE_MODULE, mod_antispam).
-define(SERVICE_JID_PREFIX, "rtbl-").

-export([parse_blocked_domains/1,
	 parse_pubsub_event/1,
	 pubsub_event_handler/1,
	 request_blocked_domains/3,
	 subscribe/3,
	 unsubscribe/3]).

%% @format-begin

subscribe(RTBLHost, RTBLDomainsNode, From) ->
    FromJID = service_jid(From),
    SubIQ =
        #iq{type = set,
            to = jid:make(RTBLHost),
            from = FromJID,
            sub_els = [#pubsub{subscribe = #ps_subscribe{jid = FromJID, node = RTBLDomainsNode}}]},
    ?DEBUG("Sending subscription request:~n~p", [xmpp:encode(SubIQ)]),
    ejabberd_router:route_iq(SubIQ, subscribe_result, self()).

-spec unsubscribe(binary() | none, binary(), binary()) -> ok.
unsubscribe(none, _PSNode, _From) ->
    ok;
unsubscribe(RTBLHost, RTBLDomainsNode, From) ->
    FromJID = jid:make(From),
    SubIQ =
        #iq{type = set,
            to = jid:make(RTBLHost),
            from = FromJID,
            sub_els =
                [#pubsub{unsubscribe = #ps_unsubscribe{jid = FromJID, node = RTBLDomainsNode}}]},
    ejabberd_router:route_iq(SubIQ, unsubscribe_result, self()).

-spec request_blocked_domains(binary() | none, binary(), binary()) -> ok.
request_blocked_domains(none, _PSNode, _From) ->
    ok;
request_blocked_domains(RTBLHost, RTBLDomainsNode, From) ->
    IQ = #iq{type = get,
             from = jid:make(From),
             to = jid:make(RTBLHost),
             sub_els = [#pubsub{items = #ps_items{node = RTBLDomainsNode}}]},
    ?DEBUG("Requesting RTBL blocked domains from ~s:~n~p", [RTBLHost, xmpp:encode(IQ)]),
    ejabberd_router:route_iq(IQ, blocked_domains, self()).

-spec parse_blocked_domains(stanza()) -> #{binary() => any()} | undefined.
parse_blocked_domains(#iq{to = #jid{lserver = LServer}, type = result} = IQ) ->
    ?DEBUG("parsing iq-result items: ~p", [IQ]),
    [#rtbl_service{node = RTBLDomainsNode}] = mod_antispam:get_rtbl_services_option(LServer),
    case xmpp:get_subtag(IQ, #pubsub{}) of
        #pubsub{items = #ps_items{node = RTBLDomainsNode, items = Items}} ->
            ?DEBUG("Got items:~n~p", [Items]),
            parse_items(Items);
        _ ->
            undefined
    end.

-spec parse_pubsub_event(stanza()) -> #{binary() => any()}.
parse_pubsub_event(#message{to = #jid{lserver = LServer}} = Msg) ->
    [#rtbl_service{node = RTBLDomainsNode}] = mod_antispam:get_rtbl_services_option(LServer),
    case xmpp:get_subtag(Msg, #ps_event{}) of
        #ps_event{items =
                      #ps_items{node = RTBLDomainsNode,
                                items = Items,
                                retract = RetractIds}} ->
            maps:merge(retract_items(RetractIds), parse_items(Items));
        Other ->
            ?WARNING_MSG("Couldn't extract items: ~p", [Other]),
            #{}
    end.

-spec parse_items([ps_item()]) -> #{binary() => any()}.
parse_items(Items) ->
    lists:foldl(fun(#ps_item{id = ID}, Acc) ->
                   %% TODO extract meta/extra instructions
                   maps:put(ID, true, Acc)
                end,
                #{},
                Items).

-spec retract_items([binary()]) -> #{binary() => false}.
retract_items(Ids) ->
    lists:foldl(fun(ID, Acc) -> Acc#{ID => false} end, #{}, Ids).

-spec service_jid(binary()) -> jid().
service_jid(Host) ->
    jid:make(<<>>, Host, <<?SERVICE_JID_PREFIX, (ejabberd_cluster:node_id())/binary>>).

%%--------------------------------------------------------------------
%% Hook callbacks.
%%--------------------------------------------------------------------

-spec pubsub_event_handler(stanza()) -> drop | stanza().
pubsub_event_handler(#message{from = FromJid,
                              to =
                                  #jid{lserver = LServer,
                                       lresource = <<?SERVICE_JID_PREFIX, _/binary>>}} =
                         Msg) ->
    ?DEBUG("Got RTBL message:~n~p", [Msg]),
    From = jid:encode(FromJid),
    [#rtbl_service{host = RTBLHost}] = mod_antispam:get_rtbl_services_option(LServer),
    case RTBLHost of
        From ->
            ParsedItems = parse_pubsub_event(Msg),
            Proc = gen_mod:get_module_proc(LServer, ?SERVICE_MODULE),
            gen_server:cast(Proc, {update_blocked_domains, ParsedItems}),
            %% FIXME what's the difference between `{drop, ...}` and `{stop, {drop, ...}}`?
            drop;
        _Other ->
            ?INFO_MSG("Got unexpected message from ~s to rtbl resource:~n~p", [From, Msg]),
            Msg
    end;
pubsub_event_handler(Acc) ->
    ?DEBUG("unexpected something on pubsub_event_handler: ~p", [Acc]),
    Acc.
