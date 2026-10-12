%%%----------------------------------------------------------------------
%%% File    : mod_antispam_filter.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Filter C2S and S2S stanzas
%%% Created : 31 Mar 2019 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2019-2026 ProcessOne
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

%%| Definitions
%% @format-begin

-module(mod_antispam_filter).

-author('holger@zedat.fu-berlin.de').
-author('stefan@strigler.de').

-export([init_filtering/1, terminate_filtering/1]).
%% ejabberd_hooks callbacks
-export([s2s_in_handle_info/2, s2s_receive_packet/1, sm_receive_packet/1]).

-include("logger.hrl").
-include("mod_antispam.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-type s2s_in_state() :: ejabberd_s2s_in:state().

-define(HTTPC_TIMEOUT, timer:seconds(3)).

%%--------------------------------------------------------------------
%%| Exported

init_filtering(Host) ->
    ejabberd_hooks:add(s2s_in_handle_info, Host, ?MODULE, s2s_in_handle_info, 90),
    ejabberd_hooks:add(s2s_receive_packet, Host, ?MODULE, s2s_receive_packet, 50),
    ejabberd_hooks:add(sm_receive_packet, Host, ?MODULE, sm_receive_packet, 50).

terminate_filtering(Host) ->
    ejabberd_hooks:delete(s2s_receive_packet, Host, ?MODULE, s2s_receive_packet, 50),
    ejabberd_hooks:delete(sm_receive_packet, Host, ?MODULE, sm_receive_packet, 50),
    ejabberd_hooks:delete(s2s_in_handle_info, Host, ?MODULE, s2s_in_handle_info, 90).

%%--------------------------------------------------------------------
%%| Hook callbacks

-spec s2s_receive_packet({stanza() | drop, s2s_in_state()}) ->
                            {stanza() | drop, s2s_in_state()} | {stop, {drop, s2s_in_state()}}.
s2s_receive_packet({Acc, #{server_host := ServerHost} = State}) ->
    case handle_receive_packet(Acc, ServerHost) of
        {stop, drop} ->
            {stop, {drop, State}};
        Result ->
            {Result, State}
    end.

-spec sm_receive_packet(stanza() | drop) -> stanza() | drop | {stop, drop}.
sm_receive_packet(drop = Acc) ->
    Acc;
sm_receive_packet(Stanza) ->
    #jid{lserver = LServer} = xmpp:get_to(Stanza),
    handle_receive_packet(Stanza, LServer).

-spec handle_receive_packet(stanza() | drop, binary()) -> stanza() | drop | {stop, drop}.
handle_receive_packet(#message{from = From,
                           to = To,
                           type = Type} = Msg,
                      Host)
    when Type /= error ->
    do_check(From, To, Host, Msg);
handle_receive_packet(#presence{from = From,
                            to = To,
                            type = subscribe} = Presence,
                      Host) ->
    do_check(From, To, Host, Presence);
handle_receive_packet(Acc, _Host) ->
    Acc.

%%--------------------------------------------------------------------
%%| Filtering deciding

do_check(From, To, Host, Stanza) ->
    case needs_checking(Host, From, To) of
        true ->
            case check_from(Host, From) of
                ham ->
                    case check_stanza(Host, From, Stanza) of
                        ham ->
                            Stanza;
                        spam ->
                            reject(Stanza),
                            {stop, drop}
                    end;
                spam ->
                    reject(Stanza),
                    {stop, drop}
            end;
        false ->
            Stanza
    end.

check_stanza(Host, From, #message{body = Body}) ->
    check_body(Host, From, xmpp:get_text(Body));
check_stanza(_, _, _) ->
    ham.

-spec s2s_in_handle_info(s2s_in_state(), any()) ->
                            s2s_in_state() | {stop, s2s_in_state()}.
s2s_in_handle_info(State, {_Ref, {spam_filter, _}}) ->
    ?DEBUG("Dropping expired spam filter result", []),
    {stop, State};
s2s_in_handle_info(State, _) ->
    State.

-spec needs_checking(binary(), jid(), jid()) -> boolean().
needs_checking(Host, #jid{lserver = FromHost} = From, To) ->
    case gen_mod:is_loaded(Host, ?MODULE_ANTISPAM) of
        true ->
            Access = gen_mod:get_module_opt(Host, ?MODULE_ANTISPAM, access_spam),
            case acl:match_rule(Host, Access, To) of
                allow ->
                    ?DEBUG("Spam not filtered for ~s", [jid:encode(To)]),
                    false;
                deny ->
                    ?DEBUG("Spam is filtered for ~s", [jid:encode(To)]),
                    not mod_roster:is_subscribed(From, To)
                    andalso not
                        mod_roster:is_subscribed(
                                  jid:make(<<>>, FromHost),
                                    To) % likely a gateway or user allowed whole domain
                    andalso not is_room_affiliated(From, To)
            end;
        false ->
            ?DEBUG("~s not loaded for ~s", [?MODULE_ANTISPAM, Host]),
            false
    end.

-spec check_from(binary(), jid()) -> ham | spam.
check_from(Host, From) ->
    try
        Proc = gen_mod:get_module_proc_check(Host, ?MODULE_ANTISPAM),
        LFrom =
            {_, FromDomain, _} =
                jid:remove_resource(
                    jid:tolower(From)),
        case gen_server:call(Proc, {is_blocked_domain, FromDomain}) of
            true ->
                ?DEBUG("Spam JID found in blocked domains: ~p", [From]),
                ejabberd_hooks:run(spam_found, Host, [{jid, From}]),
                spam;
            false ->
                case gen_server:call(Proc, {check_jid, LFrom}) of
                    {spam_filter, Result} ->
                        Result
                end
        end
    catch
        error:badarg ->
            ham;
        exit:{timeout, _} ->
            ?WARNING_MSG("Timeout while checking ~s against list of blocked domains or spammers",
                         [jid:encode(From)]),
            ham
    end.

-spec check_body(binary(), jid(), binary()) -> ham | spam.
check_body(Host, From, Body) ->
    case {extract_urls(Body), extract_jids(Body)} of
        {none, none} ->
            ?DEBUG("No JIDs/URLs found in message", []),
            ham;
        {URLs, JIDs} ->
            try Proc = gen_mod:get_module_proc_check(Host, ?MODULE_ANTISPAM),
                LFrom =
                    jid:remove_resource(
                        jid:tolower(From)),
                gen_server:call(Proc, {check_body, URLs, JIDs, LFrom})
            of
                {spam_filter, Result} ->
                    Result
            catch
                error:badarg ->
                    ham;
                exit:{timeout, _} ->
                    ?WARNING_MSG("Timeout while checking body", []),
                    ham
            end
    end.

%%--------------------------------------------------------------------
%%| Auxiliary

-spec extract_urls(binary()) -> {urls, [url()]} | none.
extract_urls(Body) ->
    RE = <<"https?://\\S+">>,
    Options = [global, {capture, all, binary}],
    case re:run(Body, RE, Options) of
        {match, Captured} when is_list(Captured) ->
            Urls = resolve_redirects(lists:flatten(Captured)),
            {urls, Urls};
        nomatch ->
            none
    end.

-spec resolve_redirects([url()]) -> [url()].
resolve_redirects(URLs) ->
    try do_resolve_redirects(URLs, []) of
        ResolvedURLs ->
            ResolvedURLs
    catch
        exit:{timeout, _} ->
            ?WARNING_MSG("Timeout while resolving redirects: ~p", [URLs]),
            URLs
    end.

-spec do_resolve_redirects([url()], [url()]) -> [url()].
do_resolve_redirects([], Result) ->
    Result;
do_resolve_redirects([URL | Rest], Acc) ->
    case httpc:request(get,
                       {URL, [{"user-agent", "curl/8.7.1"}]},
                       [{autoredirect, false}, {timeout, ?HTTPC_TIMEOUT}],
                       [])
    of
        {ok, {{_, StatusCode, _}, Headers, _Body}} when StatusCode >= 300, StatusCode < 400 ->
            Location = proplists:get_value("location", Headers),
            case Location == undefined orelse lists:member(Location, Acc) of
                true ->
                    do_resolve_redirects(Rest, [URL | Acc]);
                false ->
                    do_resolve_redirects([Location | Rest], [URL | Acc])
            end;
        _Res ->
            do_resolve_redirects(Rest, [URL | Acc])
    end.

-spec extract_jids(binary()) -> {jids, [ljid()]} | none.
extract_jids(Body) ->
    RE = <<"\\S+@\\S+">>,
    Options = [global, {capture, all, binary}],
    case re:run(Body, RE, Options) of
        {match, Captured} when is_list(Captured) ->
            {jids, lists:filtermap(fun try_decode_jid/1, lists:flatten(Captured))};
        nomatch ->
            none
    end.

-spec try_decode_jid(binary()) -> {true, ljid()} | false.
try_decode_jid(S) ->
    try jid:decode(S) of
        #jid{} = JID ->
            {true,
             jid:remove_resource(
                 jid:tolower(JID))}
    catch
        _:{bad_jid, _} ->
            false
    end.

-spec reject(stanza()) -> ok.
reject(#message{from = From,
                to = To,
                type = Type,
                lang = Lang} =
           Msg)
    when Type /= groupchat, Type /= error ->
    ?INFO_MSG("Rejecting unsolicited message from ~s to ~s",
              [jid:encode(From), jid:encode(To)]),
    Txt = <<"Your message is unsolicited">>,
    Err = xmpp:err_policy_violation(Txt, Lang),
    ejabberd_hooks:run(spam_stanza_rejected, To#jid.lserver, [Msg]),
    ejabberd_router:route_error(Msg, Err);
reject(#presence{from = From,
                 to = To,
                 lang = Lang} =
           Presence) ->
    ?INFO_MSG("Rejecting unsolicited presence from ~s to ~s",
              [jid:encode(From), jid:encode(To)]),
    Txt = <<"Your traffic is unsolicited">>,
    Err = xmpp:err_policy_violation(Txt, Lang),
    ejabberd_router:route_error(Presence, Err);
reject(_) ->
    ok.

is_room_affiliated(From, #jid{luser = RoomName, lserver = MucHost}) ->
    try mod_muc:find_online_room(RoomName, MucHost) of
        {ok, Pid} ->
            {ok, Room} = mod_muc_room:get_state(Pid),
            mod_muc_room:get_affiliation(jid:tolower(From), Room) =/= none;
        error ->
            false
    catch
        _:{unregistered_route, _Route} ->
            false
    end.
%%--------------------------------------------------------------------

%%| vim: set foldmethod=marker foldmarker=%%|,%%-:
