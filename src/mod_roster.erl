%%%----------------------------------------------------------------------
%%% File    : mod_roster.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Roster management
%%% Created : 11 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

%%% @doc Roster management (Mnesia storage).
%%%
%%% Includes support for XEP-0237: Roster Versioning.
%%% The roster versioning follows an all-or-nothing strategy:
%%%  - If the version supplied by the client is the latest, return an empty response.
%%%  - If not, return the entire new roster (with updated version string).
%%% Roster version is a hash digest of the entire roster.
%%% No additional data is stored in DB.

-module(mod_roster).

-protocol({xep, 237, '1.3', '2.1.0', "complete", ""}).

-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
         stop/1,
         reload/3,
         process_iq/1,
         export/1,
         import_info/0,
         process_local_iq/1,
         get_user_roster_items/2,
         import/5,
         get_roster/2,
         push_item/3,
         import_start/2,
         import_stop/2,
         is_subscribed/2,
         c2s_self_presence/1,
         in_subscription/2,
         out_subscription/1,
         set_items/3,
         remove_user/2,
         get_jid_info/4,
         encode_item/1,
         get_versioning_feature/2,
         roster_version/2,
         mod_doc/0,
         mod_opt_type/1,
         mod_options/1,
         set_roster/1,
         del_roster/3,
         process_rosteritems/5,
         depends/2,
         set_item_and_notify_clients/3]).

-export([webadmin_page_hostuser/4, webadmin_menu_hostuser/4, webadmin_user/4]).

-import(ejabberd_web_admin, [make_command/4, make_command_raw_value/3, make_table/4]).

-include("logger.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-include("mod_roster.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include("ejabberd_stacktrace.hrl").
-include("translate.hrl").

-define(ROSTER_CACHE,         roster_cache).
-define(ROSTER_ITEM_CACHE,    roster_item_cache).
-define(ROSTER_VERSION_CACHE, roster_version_cache).
-define(SM_MIX_ANNOTATE,      roster_mix_annotate).

-type c2s_state() :: ejabberd_c2s:state().
-export_type([subscription/0]).


-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), binary(), #roster{} | [binary()]) -> ok.
-callback read_roster_version(binary(), binary()) -> {ok, binary()} | error.
-callback write_roster_version(binary(), binary(), boolean(), binary()) -> any().
-callback get_roster(binary(), binary()) -> {ok, [#roster{}]} | error.
-callback get_roster_item(binary(), binary(), ljid()) -> {ok, #roster{}} | error.
-callback read_subscription_and_groups(binary(), binary(), ljid()) ->
              {ok, {subscription(), ask(), [binary()]}} | error.
-callback roster_subscribe(binary(), binary(), ljid(), #roster{}) -> any().
-callback transaction(binary(), fun(() -> T)) -> {atomic, T} | {aborted, any()}.
-callback remove_user(binary(), binary()) -> any().
-callback update_roster(binary(), binary(), ljid(), #roster{}) -> any().
-callback del_roster(binary(), binary(), ljid()) -> any().
-callback use_cache(binary(), roster | roster_version) -> boolean().
-callback cache_nodes(binary()) -> [node()].

-optional_callbacks([use_cache/2, cache_nodes/1]).


start(Host, Opts) ->
    Mod = gen_mod:db_mod(Opts, ?MODULE),
    Mod:init(Host, Opts),
    init_cache(Mod, Host, Opts),
    {ok, [{hook, roster_get, get_user_roster_items, 50},
          {hook, roster_in_subscription, in_subscription, 50},
          {hook, roster_out_subscription, out_subscription, 50},
          {hook, roster_get_jid_info, get_jid_info, 50},
          {hook, remove_user, remove_user, 50},
          {hook, c2s_self_presence, c2s_self_presence, 50},
          {hook, c2s_post_auth_features, get_versioning_feature, 50},
          {hook, webadmin_menu_hostuser, webadmin_menu_hostuser, 50},
          {hook, webadmin_page_hostuser, webadmin_page_hostuser, 50},
          {hook, webadmin_user, webadmin_user, 50},
          {iq_handler, ejabberd_sm, ?NS_ROSTER, process_iq}]}.


stop(_Host) ->
    ok.


reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(OldOpts, ?MODULE),
    if
        NewMod /= OldMod ->
            NewMod:init(Host, NewOpts);
        true ->
            ok
    end,
    init_cache(NewMod, Host, NewOpts).


depends(_Host, _Opts) ->
    [].


-spec process_iq(iq()) -> iq().
process_iq(#iq{
             from = #jid{luser = U, lserver = S},
             to = #jid{luser = U, lserver = S}
            } = IQ) ->
    process_local_iq(IQ);
process_iq(#iq{lang = Lang, to = To} = IQ) ->
    case ejabberd_hooks:run_fold(roster_remote_access,
                                 To#jid.lserver,
                                 false,
                                 [IQ]) of
        false ->
            Txt = ?T("Query to another users is forbidden"),
            xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang));
        {true, IQ1} ->
            process_local_iq(IQ1)
    end.


-spec process_local_iq(iq()) -> iq().
process_local_iq(#iq{
                   type = set,
                   lang = Lang,
                   sub_els = [#roster_query{
                                items = [#roster_item{ask = Ask}]
                               }]
                  } = IQ)
  when Ask /= undefined ->
    Txt = ?T("Possessing 'ask' attribute is not allowed by RFC6121"),
    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
process_local_iq(#iq{
                   type = set,
                   from = From,
                   lang = Lang,
                   sub_els = [#roster_query{
                                items = [#roster_item{} = Item]
                               }]
                  } = IQ) ->
    case has_duplicated_groups(Item#roster_item.groups) of
        true ->
            Txt = ?T("Duplicated groups are not allowed by RFC6121"),
            xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
        false ->
            From1 = case xmpp:get_meta(IQ, privilege_from, none) of
                        #jid{} = PrivFrom ->
                            PrivFrom;
                        none ->
                            From
                    end,
            #jid{lserver = LServer} = From1,
            Access = mod_roster_opt:access(LServer),
            case acl:match_rule(LServer, Access, From) of
                deny ->
                    Txt = ?T("Access denied by service policy"),
                    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
                allow ->
                    process_iq_set(IQ)
            end
    end;
process_local_iq(#iq{
                   type = set,
                   lang = Lang,
                   sub_els = [#roster_query{items = [_ | _]}]
                  } = IQ) ->
    Txt = ?T("Multiple <item/> elements are not allowed by RFC6121"),
    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
process_local_iq(#iq{
                   type = get,
                   lang = Lang,
                   sub_els = [#roster_query{items = Items}]
                  } = IQ) ->
    case Items of
        [] ->
            process_iq_get(IQ);
        [_ | _] ->
            Txt = ?T("The query must not contain <item/> elements"),
            xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
    end;
process_local_iq(#iq{lang = Lang} = IQ) ->
    Txt = ?T("No module is handling this query"),
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).


-spec roster_hash([#roster{}]) -> binary().
roster_hash(Items) ->
    str:sha(term_to_binary(lists:sort([ R#roster_item{groups = lists:sort(Grs)}
                                        || R = #roster_item{groups = Grs} <- Items ]))).


%% Returns a list that may contain an xmlelement with the XEP-237 feature if it's enabled.
-spec get_versioning_feature([xmpp_element()], binary()) -> [xmpp_element()].
get_versioning_feature(Acc, Host) ->
    case gen_mod:is_loaded(Host, ?MODULE) of
        true ->
            case mod_roster_opt:versioning(Host) of
                true ->
                    [#rosterver_feature{} | Acc];
                false ->
                    Acc
            end;
        false ->
            Acc
    end.


-spec roster_version(binary(), binary()) -> undefined | binary().
roster_version(LServer, LUser) ->
    case mod_roster_opt:store_current_id(LServer) of
        true ->
            case read_roster_version(LUser, LServer) of
                error -> undefined;
                {ok, V} -> V
            end;
        false ->
            roster_hash(run_roster_get_hook(LUser, LServer))
    end.


-spec read_roster_version(binary(), binary()) -> {ok, binary()} | error.
read_roster_version(LUser, LServer) ->
    ets_cache:lookup(
      ?ROSTER_VERSION_CACHE,
      {LUser, LServer},
      fun() ->
              Mod = gen_mod:db_mod(LServer, ?MODULE),
              Mod:read_roster_version(LUser, LServer)
      end).


-spec write_roster_version(binary(), binary()) -> binary().
write_roster_version(LUser, LServer) ->
    write_roster_version(LUser, LServer, false).


-spec write_roster_version_t(binary(), binary()) -> binary().
write_roster_version_t(LUser, LServer) ->
    write_roster_version(LUser, LServer, true).


-spec write_roster_version(binary(), binary(), boolean()) -> binary().
write_roster_version(LUser, LServer, InTransaction) ->
    Ver = str:sha(term_to_binary(erlang:unique_integer())),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:write_roster_version(LUser, LServer, InTransaction, Ver),
    if
        InTransaction -> ok;
        true ->
            ets_cache:delete(?ROSTER_VERSION_CACHE,
                             {LUser, LServer},
                             cache_nodes(Mod, LServer))
    end,
    Ver.


%% Load roster from DB only if necessary.
%% It is necessary if
%%     - roster versioning is disabled in server OR
%%     - roster versioning is not used by the client OR
%%     - roster versioning is used by server and client, BUT the server isn't storing versions on db OR
%%     - the roster version from client don't match current version.
-spec process_iq_get(iq()) -> iq().
process_iq_get(#iq{
                 to = To,
                 from = From,
                 sub_els = [#roster_query{ver = RequestedVersion, mix_annotate = MixEnabled}]
                } = IQ) ->
    LUser = To#jid.luser,
    LServer = To#jid.lserver,
    {ItemsToSend, VersionToSend} =
        case {mod_roster_opt:versioning(LServer),
              mod_roster_opt:store_current_id(LServer)} of
            {true, true} when RequestedVersion /= undefined ->
                case read_roster_version(LUser, LServer) of
                    error ->
                        RosterVersion = write_roster_version(LUser, LServer),
                        {run_roster_get_hook(LUser, LServer), RosterVersion};
                    {ok, RequestedVersion} ->
                        {false, false};
                    {ok, NewVersion} ->
                        {run_roster_get_hook(LUser, LServer), NewVersion}
                end;
            {true, false} when RequestedVersion /= undefined ->
                RosterItems = run_roster_get_hook(LUser, LServer),
                case roster_hash(RosterItems) of
                    RequestedVersion ->
                        {false, false};
                    New ->
                        {RosterItems, New}
                end;
            _ ->
                {run_roster_get_hook(LUser, LServer), false}
        end,
    % Store that MIX annotation is enabled (for roster pushes)
    set_mix_annotation_enabled(From, MixEnabled),
    % Only include <channel/> element when MIX annotation is enabled
    Items = case ItemsToSend of
                false -> false;
                FullItems -> process_items_mix(FullItems, MixEnabled)
            end,
    xmpp:make_iq_result(
      IQ,
      case {Items, VersionToSend} of
          {false, false} ->
              undefined;
          {Items, false} ->
              #roster_query{items = Items};
          {Items, Version} ->
              #roster_query{
                items = Items,
                ver = Version
               }
      end).


-spec run_roster_get_hook(binary(), binary()) -> [#roster_item{}].
run_roster_get_hook(LUser, LServer) ->
    ejabberd_hooks:run_fold(roster_get, LServer, [], [{LUser, LServer}]).


-spec get_filtered_roster(binary(), binary()) -> [#roster{}].
get_filtered_roster(LUser, LServer) ->
    lists:filter(
      fun(#roster{subscription = none, ask = in}) -> false;
         (_) -> true
      end,
      get_roster(LUser, LServer)).


-spec get_user_roster_items([#roster_item{}], {binary(), binary()}) -> [#roster_item{}].
get_user_roster_items(Acc, {LUser, LServer}) ->
    lists:map(fun encode_item/1, get_filtered_roster(LUser, LServer)) ++ Acc.


-spec get_roster(binary(), binary()) -> [#roster{}].
get_roster(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    R = case use_cache(Mod, LServer, roster) of
            true ->
                ets_cache:lookup(
                  ?ROSTER_CACHE,
                  {LUser, LServer},
                  fun() -> Mod:get_roster(LUser, LServer) end);
            false ->
                Mod:get_roster(LUser, LServer)
        end,
    case R of
        {ok, Items} -> Items;
        error -> []
    end.


-spec get_roster_item(binary(), binary(), ljid()) -> #roster{}.
get_roster_item(LUser, LServer, LJID) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:get_roster_item(LUser, LServer, LJID) of
        {ok, Item} ->
            Item;
        error ->
            LBJID = jid:remove_resource(LJID),
            #roster{
              usj = {LUser, LServer, LBJID},
              us = {LUser, LServer},
              jid = LBJID
             }
    end.


-spec get_subscription_and_groups(binary(), binary(), ljid()) ->
          {subscription(), ask(), [binary()]}.
get_subscription_and_groups(LUser, LServer, LJID) ->
    LBJID = jid:remove_resource(LJID),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Res = case use_cache(Mod, LServer, roster) of
              true ->
                  ets_cache:lookup(
                    ?ROSTER_ITEM_CACHE,
                    {LUser, LServer, LBJID},
                    fun() ->
                            Items = get_roster(LUser, LServer),
                            case lists:keyfind(LBJID, #roster.jid, Items) of
                                #roster{
                                  subscription = Sub,
                                  ask = Ask,
                                  groups = Groups
                                 } ->
                                    {ok, {Sub, Ask, Groups}};
                                false ->
                                    error
                            end
                    end);
              false ->
                  case Mod:read_subscription_and_groups(LUser, LServer, LBJID) of
                      {ok, {Sub, Groups}} ->
                          %% Backward compatibility for third-party backends
                          {ok, {Sub, none, Groups}};
                      Other ->
                          Other
                  end
          end,
    case Res of
        {ok, SubAndGroups} ->
            SubAndGroups;
        error ->
            {none, none, []}
    end.


-spec set_roster(#roster{}) -> {atomic | aborted, any()}.
set_roster(#roster{us = {LUser, LServer}, jid = LJID} = Item) ->
    transaction(
      LUser,
      LServer,
      [LJID],
      fun() ->
              update_roster_t(LUser, LServer, LJID, Item)
      end).


-spec del_roster(binary(), binary(), ljid()) -> {atomic | aborted, any()}.
del_roster(LUser, LServer, LJID) ->
    transaction(
      LUser,
      LServer,
      [LJID],
      fun() ->
              del_roster_t(LUser, LServer, LJID)
      end).


-spec encode_item(#roster{}) -> roster_item().
encode_item(Item) ->
    #roster_item{
      jid = jid:make(Item#roster.jid),
      name = Item#roster.name,
      subscription = Item#roster.subscription,
      ask = case ask_to_pending(Item#roster.ask) of
                out -> subscribe;
                both -> subscribe;
                _ -> undefined
            end,
      groups = Item#roster.groups
     }.


-spec decode_item(roster_item(), #roster{}, boolean()) -> #roster{}.
decode_item(#roster_item{subscription = remove} = Item, R, _) ->
    R#roster{
      jid = jid:tolower(Item#roster_item.jid),
      name = <<"">>,
      subscription = remove,
      ask = none,
      groups = [],
      askmessage = <<"">>,
      xs = []
     };
decode_item(Item, R, Managed) ->
    R#roster{
      jid = jid:tolower(Item#roster_item.jid),
      name = Item#roster_item.name,
      subscription = case Item#roster_item.subscription of
                         Sub when Managed -> Sub;
                         _ -> R#roster.subscription
                     end,
      groups = Item#roster_item.groups
     }.


-spec process_iq_set(iq()) -> iq().
process_iq_set(#iq{
                 from = _From,
                 to = To,
                 lang = Lang,
                 sub_els = [#roster_query{items = [QueryItem]}]
                } = IQ) ->
    case set_item_and_notify_clients(To, QueryItem, false) of
        ok ->
            xmpp:make_iq_result(IQ);
        {error, _} ->
            Txt = ?T("Database failure"),
            Err = xmpp:err_internal_server_error(Txt, Lang),
            xmpp:make_error(IQ, Err)
    end.


-spec set_item_and_notify_clients(jid(), #roster_item{}, boolean()) -> ok | {error, any()}.
set_item_and_notify_clients(To,
                            #roster_item{jid = PeerJID} = RosterItem,
                            OverrideSubscription) ->
    #jid{luser = LUser, lserver = LServer} = To,
    PeerLJID = jid:tolower(PeerJID),
    F = fun() ->
                Item1 = get_roster_item(LUser, LServer, PeerLJID),
                Item2 = decode_item(RosterItem, Item1, OverrideSubscription),
                Item3 = ejabberd_hooks:run_fold(roster_process_item,
                                                LServer,
                                                Item2,
                                                [LServer]),
                case Item3#roster.subscription of
                    remove -> del_roster_t(LUser, LServer, PeerLJID);
                    _ -> update_roster_t(LUser, LServer, PeerLJID, Item3)
                end,
                case mod_roster_opt:store_current_id(LServer) of
                    true -> write_roster_version_t(LUser, LServer);
                    false -> ok
                end,
                {Item1, Item3}
        end,
    case transaction(LUser, LServer, [PeerLJID], F) of
        {atomic, {OldItem, NewItem}} ->
            push_item(To, encode_item(OldItem), encode_item(NewItem)),
            case NewItem#roster.subscription of
                remove ->
                    send_unsubscribing_presence(To, OldItem);
                _ ->
                    ok
            end;
        {aborted, Reason} ->
            {error, Reason}
    end.


-spec push_item(jid(), #roster_item{}, #roster_item{}) -> ok.
push_item(To, OldItem, NewItem) ->
    #jid{luser = LUser, lserver = LServer} = To,
    Ver = case mod_roster_opt:versioning(LServer) of
              true -> roster_version(LServer, LUser);
              false -> undefined
          end,
    lists:foreach(
      fun(Resource) ->
              To1 = jid:replace_resource(To, Resource),
              push_item(To1, OldItem, NewItem, Ver)
      end,
      ejabberd_sm:get_user_resources(LUser, LServer)).


-spec push_item(jid(), #roster_item{}, #roster_item{}, undefined | binary()) -> ok.
push_item(To, OldItem, NewItem, Ver) ->
    route_presence_change(To, OldItem, NewItem),
    [Item] = process_items_mix([NewItem], To),
    IQ = #iq{
           type = set,
           to = To,
           from = jid:remove_resource(To),
           id = <<"push", (p1_rand:get_string())/binary>>,
           sub_els = [#roster_query{
                        ver = Ver,
                        items = [Item]
                       }]
          },
    ejabberd_router:route(IQ).


-spec route_presence_change(jid(), #roster_item{}, #roster_item{}) -> ok.
route_presence_change(From, OldItem, NewItem) ->
    OldSub = OldItem#roster_item.subscription,
    NewSub = NewItem#roster_item.subscription,
    To = NewItem#roster_item.jid,
    NewIsFrom = NewSub == both orelse NewSub == from,
    OldIsFrom = OldSub == both orelse OldSub == from,
    if
        NewIsFrom andalso not OldIsFrom ->
            case ejabberd_sm:get_session_pid(
                   From#jid.luser, From#jid.lserver, From#jid.lresource) of
                none ->
                    ok;
                Pid ->
                    ejabberd_c2s:resend_presence(Pid, To)
            end;
        OldIsFrom andalso not NewIsFrom ->
            PU = #presence{from = From, to = To, type = unavailable},
            case ejabberd_hooks:run_fold(
                   privacy_check_packet,
                   allow,
                   [From, PU, out]) of
                deny ->
                    ok;
                allow ->
                    ejabberd_router:route(PU)
            end;
        true ->
            ok
    end.


-spec ask_to_pending(ask()) -> none | in | out | both.
ask_to_pending(subscribe) -> out;
ask_to_pending(unsubscribe) -> none;
ask_to_pending(Ask) -> Ask.


-spec roster_subscribe_t(binary(), binary(), ljid(), #roster{}) -> any().
roster_subscribe_t(LUser, LServer, LJID, Item) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:roster_subscribe(LUser, LServer, LJID, Item).


-spec transaction(binary(), binary(), [ljid()], fun(() -> T)) -> {atomic, T} | {aborted, any()}.
transaction(LUser, LServer, LJIDs, F) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:transaction(LServer, F) of
        {atomic, _} = Result ->
            delete_cache(Mod, LUser, LServer, LJIDs),
            Result;
        Err ->
            Err
    end.


-spec in_subscription(boolean(), presence()) -> boolean().
in_subscription(_,
                #presence{
                  from = JID,
                  to = To,
                  sub_els = SubEls,
                  type = Type,
                  status = Status
                 }) ->
    #jid{user = User, server = Server} = To,
    Reason = if
                 Type == subscribe -> xmpp:get_text(Status);
                 true -> <<"">>
             end,
    process_subscription(in,
                         User,
                         Server,
                         JID,
                         Type,
                         Reason,
                         SubEls).


-spec out_subscription(presence()) -> boolean().
out_subscription(#presence{from = From, to = JID, type = Type}) ->
    #jid{user = User, server = Server} = From,
    process_subscription(out, User, Server, JID, Type, <<"">>, []).


-spec process_subscription(in | out,
                           binary(),
                           binary(),
                           jid(),
                           subscribe | subscribed | unsubscribe | unsubscribed,
                           binary(),
                           [fxml:xmlel()]) -> boolean().
process_subscription(Direction,
                     User,
                     Server,
                     JID1,
                     Type,
                     Reason,
                     SubEls) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LJID = jid:tolower(jid:remove_resource(JID1)),
    F = fun() ->
                Item = get_roster_item(LUser, LServer, LJID),
                NewState = case Direction of
                               out ->
                                   out_state_change(Item#roster.subscription,
                                                    Item#roster.ask,
                                                    Type);
                               in ->
                                   in_state_change(Item#roster.subscription,
                                                   Item#roster.ask,
                                                   Type)
                           end,
                AutoReply = case Direction of
                                out -> none;
                                in ->
                                    in_auto_reply(Item#roster.subscription,
                                                  Item#roster.ask,
                                                  Type)
                            end,
                AskMessage = case NewState of
                                 {_, both} -> Reason;
                                 {_, in} -> Reason;
                                 _ -> <<"">>
                             end,
                case NewState of
                    none ->
                        {none, AutoReply};
                    {none, none} when Item#roster.subscription == none,
                                      Item#roster.ask == in ->
                        del_roster_t(LUser, LServer, LJID), {none, AutoReply};
                    {Subscription, Pending} ->
                        NewItem = Item#roster{
                                    subscription = Subscription,
                                    ask = Pending,
                                    name = get_nick_subels(SubEls, Item#roster.name),
                                    xs = SubEls,
                                    askmessage = AskMessage
                                   },
                        roster_subscribe_t(LUser, LServer, LJID, NewItem),
                        case mod_roster_opt:store_current_id(LServer) of
                            true -> write_roster_version_t(LUser, LServer);
                            false -> ok
                        end,
                        {{push, Item, NewItem}, AutoReply}
                end
        end,
    case transaction(LUser, LServer, [LJID], F) of
        {atomic, {Push, AutoReply}} ->
            case AutoReply of
                none -> ok;
                _ ->
                    ejabberd_router:route(
                      #presence{
                        type = AutoReply,
                        from = jid:make(User, Server),
                        to = JID1
                       })
            end,
            case Push of
                {push, OldItem, NewItem} ->
                    if
                        NewItem#roster.subscription == none,
                        NewItem#roster.ask == in ->
                            ok;
                        true ->
                            push_item(jid:make(User, Server),
                                      encode_item(OldItem),
                                      encode_item(NewItem))
                    end,
                    true;
                none ->
                    false
            end;
        _ ->
            false
    end.


get_nick_subels(SubEls, Default) ->
    case xmpp:get_subtag(#presence{sub_els = SubEls}, #nick{}) of
        {nick, N} -> N;
        _ -> Default
    end.


%% in_state_change(Subscription, Pending, Type) -> NewState
%% NewState = none | {NewSubscription, NewPending}
-ifdef(ROSTER_GATEWAY_WORKAROUND).

-define(NNSD, {to, none}).

-define(NISD, {to, in}).

-else.

-define(NNSD, none).

-define(NISD, none).

-endif.


in_state_change(none, none, subscribe) -> {none, in};
in_state_change(none, none, subscribed) -> ?NNSD;
in_state_change(none, none, unsubscribe) -> none;
in_state_change(none, none, unsubscribed) -> none;
in_state_change(none, out, subscribe) -> {none, both};
in_state_change(none, out, subscribed) -> {to, none};
in_state_change(none, out, unsubscribe) -> none;
in_state_change(none, out, unsubscribed) ->
    {none, none};
in_state_change(none, in, subscribe) -> none;
in_state_change(none, in, subscribed) -> ?NISD;
in_state_change(none, in, unsubscribe) -> {none, none};
in_state_change(none, in, unsubscribed) -> none;
in_state_change(none, both, subscribe) -> none;
in_state_change(none, both, subscribed) -> {to, in};
in_state_change(none, both, unsubscribe) -> {none, out};
in_state_change(none, both, unsubscribed) -> {none, in};
in_state_change(to, none, subscribe) -> {to, in};
in_state_change(to, none, subscribed) -> none;
in_state_change(to, none, unsubscribe) -> none;
in_state_change(to, none, unsubscribed) -> {none, none};
in_state_change(to, in, subscribe) -> none;
in_state_change(to, in, subscribed) -> none;
in_state_change(to, in, unsubscribe) -> {to, none};
in_state_change(to, in, unsubscribed) -> {none, in};
in_state_change(from, none, subscribe) -> none;
in_state_change(from, none, subscribed) -> {both, none};
in_state_change(from, none, unsubscribe) ->
    {none, none};
in_state_change(from, none, unsubscribed) -> none;
in_state_change(from, out, subscribe) -> none;
in_state_change(from, out, subscribed) -> {both, none};
in_state_change(from, out, unsubscribe) -> {none, out};
in_state_change(from, out, unsubscribed) ->
    {from, none};
in_state_change(both, none, subscribe) -> none;
in_state_change(both, none, subscribed) -> none;
in_state_change(both, none, unsubscribe) -> {to, none};
in_state_change(both, none, unsubscribed) ->
    {from, none};
% Invalid states that can occurs from roster modification from API
in_state_change(to, out, subscribe) -> {to, in};
in_state_change(to, out, subscribed) -> none;
in_state_change(to, out, unsubscribe) -> none;
in_state_change(to, out, unsubscribed) -> {none, none};
in_state_change(to, both, subscribe) -> none;
in_state_change(to, both, subscribed) -> none;
in_state_change(to, both, unsubscribe) -> {to, none};
in_state_change(to, both, unsubscribed) -> {none, in};
in_state_change(from, in, subscribe) -> none;
in_state_change(from, in, subscribed) -> {both, none};
in_state_change(from, in, unsubscribe) ->
    {none, none};
in_state_change(from, in, unsubscribed) -> none;
in_state_change(from, both, subscribe) -> none;
in_state_change(from, both, subscribed) -> {both, none};
in_state_change(from, both, unsubscribe) -> {none, out};
in_state_change(from, both, unsubscribed) ->
    {from, none};
in_state_change(both, _, subscribe) -> none;
in_state_change(both, _, subscribed) -> none;
in_state_change(both, _, unsubscribe) -> {to, none};
in_state_change(both, _, unsubscribed) ->
    {from, none}.


out_state_change(none, none, subscribe) -> {none, out};
out_state_change(none, none, subscribed) -> none;
out_state_change(none, none, unsubscribe) -> none;
out_state_change(none, none, unsubscribed) -> none;
out_state_change(none, out, subscribe) ->
    {none, out};  %% We need to resend query (RFC3921, section 9.2)
out_state_change(none, out, subscribed) -> none;
out_state_change(none, out, unsubscribe) ->
    {none, none};
out_state_change(none, out, unsubscribed) -> none;
out_state_change(none, in, subscribe) -> {none, both};
out_state_change(none, in, subscribed) -> {from, none};
out_state_change(none, in, unsubscribe) -> none;
out_state_change(none, in, unsubscribed) ->
    {none, none};
out_state_change(none, both, subscribe) -> none;
out_state_change(none, both, subscribed) -> {from, out};
out_state_change(none, both, unsubscribe) -> {none, in};
out_state_change(none, both, unsubscribed) ->
    {none, out};
out_state_change(to, none, subscribe) -> none;
out_state_change(to, none, subscribed) -> {both, none};
out_state_change(to, none, unsubscribe) -> {none, none};
out_state_change(to, none, unsubscribed) -> none;
out_state_change(to, in, subscribe) -> none;
out_state_change(to, in, subscribed) -> {both, none};
out_state_change(to, in, unsubscribe) -> {none, in};
out_state_change(to, in, unsubscribed) -> {to, none};
out_state_change(from, none, subscribe) -> {from, out};
out_state_change(from, none, subscribed) -> none;
out_state_change(from, none, unsubscribe) -> none;
out_state_change(from, none, unsubscribed) ->
    {none, none};
out_state_change(from, out, subscribe) -> none;
out_state_change(from, out, subscribed) -> none;
out_state_change(from, out, unsubscribe) ->
    {from, none};
out_state_change(from, out, unsubscribed) ->
    {none, out};
out_state_change(both, none, subscribe) -> none;
out_state_change(both, none, subscribed) -> none;
out_state_change(both, none, unsubscribe) ->
    {from, none};
out_state_change(both, none, unsubscribed) ->
    {to, none};
% Invalid states that can occurs from roster modification from API
out_state_change(to, out, subscribe) -> none;
out_state_change(to, out, subscribed) -> {both, none};
out_state_change(to, out, unsubscribe) -> {none, none};
out_state_change(to, out, unsubscribed) -> none;
out_state_change(to, both, subscribe) -> none;
out_state_change(to, both, subscribed) -> {both, none};
out_state_change(to, both, unsubscribe) -> {none, in};
out_state_change(to, both, unsubscribed) -> {to, none};
out_state_change(from, in, subscribe) -> {from, out};
out_state_change(from, in, subscribed) -> none;
out_state_change(from, in, unsubscribe) -> none;
out_state_change(from, in, unsubscribed) ->
    {none, none};
out_state_change(from, both, subscribe) -> none;
out_state_change(from, both, subscribed) -> none;
out_state_change(from, both, unsubscribe) ->
    {from, none};
out_state_change(from, both, unsubscribed) ->
    {none, out};
out_state_change(both, _, subscribe) -> none;
out_state_change(both, _, subscribed) -> none;
out_state_change(both, _, unsubscribe) ->
    {from, none};
out_state_change(both, _, unsubscribed) ->
    {to, none}.


in_auto_reply(from, none, subscribe) -> subscribed;
in_auto_reply(from, out, subscribe) -> subscribed;
in_auto_reply(both, none, subscribe) -> subscribed;
in_auto_reply(none, in, unsubscribe) -> unsubscribed;
in_auto_reply(none, both, unsubscribe) -> unsubscribed;
in_auto_reply(to, in, unsubscribe) -> unsubscribed;
in_auto_reply(from, none, unsubscribe) -> unsubscribed;
in_auto_reply(from, out, unsubscribe) -> unsubscribed;
in_auto_reply(both, none, unsubscribe) -> unsubscribed;
in_auto_reply(_, _, _) -> none.


-spec remove_user(binary(), binary()) -> ok.
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Items = get_filtered_roster(LUser, LServer),
    send_unsubscription_to_rosteritems(LUser, LServer, Items),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_user(LUser, LServer),
    delete_cache(Mod, LUser, LServer, [ Item#roster.jid || Item <- Items ]).


%% For each contact with Subscription:
%% Both or From, send a "unsubscribed" presence stanza;
%% Both or To, send a "unsubscribe" presence stanza.
-spec send_unsubscription_to_rosteritems(binary(), binary(), [#roster{}]) -> ok.
send_unsubscription_to_rosteritems(LUser, LServer, RosterItems) ->
    From = jid:make({LUser, LServer, <<"">>}),
    lists:foreach(fun(RosterItem) ->
                          send_unsubscribing_presence(From, RosterItem)
                  end,
                  RosterItems).


-spec send_unsubscribing_presence(jid(), #roster{}) -> ok.
send_unsubscribing_presence(From, Item) ->
    IsTo = case Item#roster.subscription of
               both -> true;
               to -> true;
               _ -> false
           end,
    IsFrom = case Item#roster.subscription of
                 both -> true;
                 from -> true;
                 _ -> false
             end,
    if
        IsTo ->
            ejabberd_router:route(
              #presence{
                type = unsubscribe,
                from = jid:remove_resource(From),
                to = jid:make(Item#roster.jid)
               });
        true -> ok
    end,
    if
        IsFrom ->
            ejabberd_router:route(
              #presence{
                type = unsubscribed,
                from = jid:remove_resource(From),
                to = jid:make(Item#roster.jid)
               });
        true -> ok
    end.


%%%===================================================================
%%% MIX
%%%===================================================================


-spec remove_mix_channel([#roster_item{}]) -> [#roster_item{}].
remove_mix_channel(Items) ->
    lists:map(
      fun(Item) ->
              Item#roster_item{mix_channel = undefined}
      end,
      Items).


-spec process_items_mix([#roster_item{}], boolean() | jid()) -> [#roster_item{}].
process_items_mix(Items, true) -> Items;
process_items_mix(Items, false) -> remove_mix_channel(Items);
process_items_mix(Items, JID) -> process_items_mix(Items, is_mix_annotation_enabled(JID)).


-spec is_mix_annotation_enabled(jid()) -> boolean().
is_mix_annotation_enabled(#jid{luser = User, lserver = Host, lresource = Res}) ->
    case ejabberd_sm:get_user_info(User, Host, Res) of
        offline -> false;
        Info ->
            case lists:keyfind(?SM_MIX_ANNOTATE, 1, Info) of
                {_, true} -> true;
                _ -> false
            end
    end.


-spec set_mix_annotation_enabled(jid(), boolean()) -> ok | {error, any()}.
set_mix_annotation_enabled(#jid{luser = U, lserver = Host, lresource = R} = JID, false) ->
    case is_mix_annotation_enabled(JID) of
        true ->
            ?DEBUG("Disabling roster MIX annotation for ~ts@~ts/~ts", [U, Host, R]),
            case ejabberd_sm:del_user_info(U, Host, R, ?SM_MIX_ANNOTATE) of
                ok -> ok;
                {error, Reason} = Err ->
                    ?ERROR_MSG("Failed to disable roster MIX annotation for ~ts@~ts/~ts: ~p",
                               [U, Host, R, Reason]),
                    Err
            end;
        false -> ok
    end;
set_mix_annotation_enabled(#jid{luser = U, lserver = Host, lresource = R}, true) ->
    ?DEBUG("Enabling roster MIX annotation for ~ts@~ts/~ts", [U, Host, R]),
    case ejabberd_sm:set_user_info(U, Host, R, ?SM_MIX_ANNOTATE, true) of
        ok -> ok;
        {error, Reason} = Err ->
            ?ERROR_MSG("Failed to enable roster MIX annotation for ~ts@~ts/~ts: ~p",
                       [U, Host, R, Reason]),
            Err
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec set_items(binary(), binary(), roster_query()) -> {atomic, ok} | {aborted, any()}.
set_items(User, Server, #roster_query{items = Items}) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LJIDs = [ jid:tolower(Item#roster_item.jid) || Item <- Items ],
    F = fun() ->
                lists:foreach(
                  fun(Item) ->
                          process_item_set_t(LUser, LServer, Item)
                  end,
                  Items)
        end,
    transaction(LUser, LServer, LJIDs, F).


-spec update_roster_t(binary(), binary(), ljid(), #roster{}) -> any().
update_roster_t(LUser, LServer, LJID, Item) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:update_roster(LUser, LServer, LJID, Item).


-spec del_roster_t(binary(), binary(), ljid()) -> any().
del_roster_t(LUser, LServer, LJID) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:del_roster(LUser, LServer, LJID).


-spec process_item_set_t(binary(), binary(), roster_item()) -> any().
process_item_set_t(LUser, LServer, #roster_item{jid = JID1} = QueryItem) ->
    JID = {JID1#jid.user, JID1#jid.server, <<>>},
    LJID = {JID1#jid.luser, JID1#jid.lserver, <<>>},
    Item = #roster{
             usj = {LUser, LServer, LJID},
             us = {LUser, LServer},
             jid = JID
            },
    Item2 = decode_item(QueryItem, Item, _Managed = true),
    case Item2#roster.subscription of
        remove -> del_roster_t(LUser, LServer, LJID);
        _ -> update_roster_t(LUser, LServer, LJID, Item2)
    end;
process_item_set_t(_LUser, _LServer, _) -> ok.


-spec c2s_self_presence({presence(), c2s_state()}) -> {presence(), c2s_state()}.
c2s_self_presence({_, #{pres_last := _}} = Acc) ->
    Acc;
c2s_self_presence({#presence{type = available} = Pkt, State}) ->
    Prio = get_priority_from_presence(Pkt),
    if
        Prio >= 0 ->
            State1 = resend_pending_subscriptions(State),
            {Pkt, State1};
        true ->
            {Pkt, State}
    end;
c2s_self_presence(Acc) ->
    Acc.


-spec resend_pending_subscriptions(c2s_state()) -> c2s_state().
resend_pending_subscriptions(#{jid := JID} = State) ->
    BareJID = jid:remove_resource(JID),
    Result = get_roster(JID#jid.luser, JID#jid.lserver),
    lists:foldl(
      fun(#roster{ask = Ask} = R, AccState) when Ask == in; Ask == both ->
              Message = R#roster.askmessage,
              Status = if
                           is_binary(Message) -> (Message);
                           true -> <<"">>
                       end,
              Sub = #presence{
                      from = jid:make(R#roster.jid),
                      to = BareJID,
                      type = subscribe,
                      sub_els = R#roster.xs,
                      status = xmpp:mk_text(Status)
                     },
              ejabberd_c2s:send(AccState, Sub);
         (_, AccState) ->
              AccState
      end,
      State,
      Result).


-spec get_priority_from_presence(presence()) -> integer().
get_priority_from_presence(#presence{priority = Prio}) ->
    case Prio of
        undefined -> 0;
        _ -> Prio
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_jid_info({subscription(), ask(), [binary()]}, binary(), binary(), jid()) ->
          {subscription(), ask(), [binary()]}.
get_jid_info(_, User, Server, JID) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LJID = jid:tolower(JID),
    get_subscription_and_groups(LUser, LServer, LJID).


%% Check if `From` is subscriberd to `To`s presence
%% note 1: partial subscriptions are also considered, i.e.
%%         `To` has already sent a subscription request to `From`
%% note 2: it's assumed a user is subscribed to self
%% note 3: `To` MUST be a local user, `From` can be any user
-spec is_subscribed(jid(), jid()) -> boolean().
is_subscribed(#jid{luser = LUser, lserver = LServer},
              #jid{luser = LUser, lserver = LServer}) ->
    true;
is_subscribed(From, #jid{luser = LUser, lserver = LServer}) ->
    {Sub, Ask, _} = ejabberd_hooks:run_fold(
                      roster_get_jid_info,
                      LServer,
                      {none, none, []},
                      [LUser, LServer, From]),
    (Sub /= none) orelse (Ask == subscribe) orelse
    (Ask == out) orelse (Ask == both).


process_rosteritems(ActionS, SubsS, AsksS, UsersS, ContactsS) ->
    LServer = ejabberd_config:get_myname(),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:process_rosteritems(ActionS, SubsS, AsksS, UsersS, ContactsS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% @format-begin


webadmin_menu_hostuser(Acc, _Host, _Username, _Lang) ->
    Acc ++ [{<<"roster">>, <<"Roster">>}].


webadmin_page_hostuser(_, Host, Username, #request{path = [<<"roster">> | RPath]} = R) ->
    Head = ?H1GL(<<"Roster">>, <<"modules/#mod_roster">>, <<"mod_roster">>),
    %% Execute twice: first to perform the action, the second to get new roster
    _ = make_webadmin_roster_table(Host, Username, R, RPath),
    RV2 = make_webadmin_roster_table(Host, Username, R, RPath),
    Set = [make_command(add_rosteritem,
                        R,
                        [{<<"localuser">>, Username}, {<<"localhost">>, Host}],
                        []),
           make_command(push_roster, R, [{<<"user">>, Username}, {<<"host">>, Host}], [])],
    Get = [make_command(get_roster, R, [], [{only, presentation}]),
           make_command(delete_rosteritem, R, [], [{only, presentation}]),
           RV2],
    {stop, Head ++ Get ++ Set};
webadmin_page_hostuser(Acc, _, _, _) ->
    Acc.


make_webadmin_roster_table(Host, Username, R, RPath) ->
    Contacts =
        case make_command_raw_value(get_roster, R, [{<<"user">>, Username}, {<<"host">>, Host}]) of
            Cs when is_list(Cs) ->
                Cs;
            _ ->
                []
        end,
    Level = 5 + length(RPath),
    Columns =
        [<<"jid">>, <<"nick">>, <<"subscription">>, <<"pending">>, <<"groups">>, <<"">>],
    Rows =
        lists:map(fun({Jid, Nick, Subscriptions, Pending, Groups}) ->
                          {JidSplit, ProblematicBin} =
                              try jid:decode(Jid) of
                                  #jid{} = J ->
                                      {jid:split(J), <<"">>}
                              catch
                                  _:{bad_jid, _} ->
                                      ?INFO_MSG("Error parsing contact of ~s@~s that is invalid JID: ~s",
                                                [Username, Host, Jid]),
                                      {{<<"000--error-parsing-jid">>, <<"localhost">>, <<"">>},
                                       <<", Error parsing JID: ", Jid/binary>>}
                              end,
                          {make_command(echo,
                                        R,
                                        [{<<"sentence">>, jid:encode(JidSplit)}],
                                        [{only, raw_and_value},
                                         {result_links, [{sentence, user, Level, <<"">>}]}]),
                           ?C(<<Nick/binary, ProblematicBin/binary>>),
                           ?C(Subscriptions),
                           ?C(Pending),
                           ?C(Groups),
                           make_command(delete_rosteritem,
                                        R,
                                        [{<<"localuser">>, Username},
                                         {<<"localhost">>, Host},
                                         {<<"user">>, element(1, JidSplit)},
                                         {<<"host">>, element(2, JidSplit)}],
                                        [{only, button},
                                         {style, danger},
                                         {input_name_append,
                                          [Username,
                                           Host,
                                           element(1, JidSplit),
                                           element(2, JidSplit)]}])}
                  end,
                  lists:keysort(1, Contacts)),
    Table = make_table(20, RPath, Columns, Rows),
    ?XE(<<"blockquote">>, [Table]).


webadmin_user(Acc, User, Server, R) ->
    Acc ++
    [make_command(get_roster_count,
                  R,
                  [{<<"user">>, User}, {<<"host">>, Server}],
                  [{result_links,
                    [{value, arg_host, 4, <<"user/", User/binary, "/roster/">>}]}])].
%%% @format-end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec has_duplicated_groups([binary()]) -> boolean().
has_duplicated_groups(Groups) ->
    GroupsPrep = lists:usort([ jid:resourceprep(G) || G <- Groups ]),
    not (length(GroupsPrep) == length(Groups)).


-spec init_cache(module(), binary(), gen_mod:opts()) -> ok.
init_cache(Mod, Host, Opts) ->
    CacheOpts = cache_opts(Opts),
    case use_cache(Mod, Host, roster_version) of
        true ->
            ets_cache:new(?ROSTER_VERSION_CACHE, CacheOpts);
        false ->
            ets_cache:delete(?ROSTER_VERSION_CACHE)
    end,
    case use_cache(Mod, Host, roster) of
        true ->
            ets_cache:new(?ROSTER_CACHE, CacheOpts),
            ets_cache:new(?ROSTER_ITEM_CACHE, CacheOpts);
        false ->
            ets_cache:delete(?ROSTER_CACHE),
            ets_cache:delete(?ROSTER_ITEM_CACHE)
    end.


-spec cache_opts(gen_mod:opts()) -> [proplists:property()].
cache_opts(Opts) ->
    MaxSize = mod_roster_opt:cache_size(Opts),
    CacheMissed = mod_roster_opt:cache_missed(Opts),
    LifeTime = mod_roster_opt:cache_life_time(Opts),
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].


-spec use_cache(module(), binary(), roster | roster_version) -> boolean().
use_cache(Mod, Host, Table) ->
    case erlang:function_exported(Mod, use_cache, 2) of
        true -> Mod:use_cache(Host, Table);
        false -> mod_roster_opt:use_cache(Host)
    end.


-spec cache_nodes(module(), binary()) -> [node()].
cache_nodes(Mod, Host) ->
    case erlang:function_exported(Mod, cache_nodes, 1) of
        true -> Mod:cache_nodes(Host);
        false -> ejabberd_cluster:get_nodes()
    end.


-spec delete_cache(module(), binary(), binary(), [ljid()]) -> ok.
delete_cache(Mod, LUser, LServer, LJIDs) ->
    case use_cache(Mod, LServer, roster_version) of
        true ->
            ets_cache:delete(?ROSTER_VERSION_CACHE,
                             {LUser, LServer},
                             cache_nodes(Mod, LServer));
        false ->
            ok
    end,
    case use_cache(Mod, LServer, roster) of
        true ->
            Nodes = cache_nodes(Mod, LServer),
            ets_cache:delete(?ROSTER_CACHE, {LUser, LServer}, Nodes),
            lists:foreach(
              fun(LJID) ->
                      ets_cache:delete(
                        ?ROSTER_ITEM_CACHE,
                        {LUser, LServer, jid:remove_resource(LJID)},
                        Nodes)
              end,
              LJIDs);
        false ->
            ok
    end.


export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).


import_info() ->
    [{<<"roster_version">>, 2},
     {<<"rostergroups">>, 3},
     {<<"rosterusers">>, 10}].


import_start(LServer, DBType) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    ets:new(rostergroups_tmp, [private, named_table, bag]),
    Mod:init(LServer, []),
    ok.


import_stop(_LServer, _DBType) ->
    ets:delete(rostergroups_tmp),
    ok.


row_length() ->
    case ejabberd_sql:use_new_schema() of
        true -> 10;
        false -> 9
    end.


import(LServer, {sql, _}, _DBType, <<"rostergroups">>, [LUser, SJID, Group]) ->
    LJID = jid:tolower(jid:decode(SJID)),
    ets:insert(rostergroups_tmp, {{LUser, LServer, LJID}, Group}),
    ok;
import(LServer, {sql, _}, DBType, <<"rosterusers">>, Row) ->
    I = mod_roster_sql:raw_to_record(LServer, lists:sublist(Row, row_length())),
    Groups = [ G || {_, G} <- ets:lookup(rostergroups_tmp, I#roster.usj) ],
    RosterItem = I#roster{groups = Groups},
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, <<"rosterusers">>, RosterItem);
import(LServer, {sql, _}, DBType, <<"roster_version">>, [LUser, Ver]) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, <<"roster_version">>, [LUser, Ver]).


mod_opt_type(access) ->
    econf:acl();
mod_opt_type(store_current_id) ->
    econf:bool();
mod_opt_type(versioning) ->
    econf:bool();
mod_opt_type(db_type) ->
    econf:db_type(?MODULE);
mod_opt_type(use_cache) ->
    econf:bool();
mod_opt_type(cache_size) ->
    econf:pos_int(infinity);
mod_opt_type(cache_missed) ->
    econf:bool();
mod_opt_type(cache_life_time) ->
    econf:timeout(second, infinity).


mod_options(Host) ->
    [{access, all},
     {store_current_id, false},
     {versioning, false},
     {db_type, ejabberd_config:default_db(Host, ?MODULE)},
     {use_cache, ejabberd_option:use_cache(Host)},
     {cache_size, ejabberd_option:cache_size(Host)},
     {cache_missed, ejabberd_option:cache_missed(Host)},
     {cache_life_time, ejabberd_option:cache_life_time(Host)}].


mod_doc() ->
    #{
      desc =>
          ?T("This module implements roster management as "
             "defined in https://tools.ietf.org/html/rfc6121#section-2"
             "[RFC6121 Section 2]. The module also adds support for "
             "https://xmpp.org/extensions/xep-0237.html"
             "[XEP-0237: Roster Versioning]."),
      opts =>
          [{access,
            #{
              value => ?T("AccessName"),
              desc =>
                  ?T("This option can be configured to specify "
                     "rules to restrict roster management. "
                     "If the rule returns 'deny' on the requested "
                     "user name, that user cannot modify their personal "
                     "roster, i.e. they cannot add/remove/modify contacts "
                     "or send presence subscriptions. "
                     "The default value is 'all', i.e. no restrictions.")
             }},
           {versioning,
            #{
              value => "true | false",
              desc =>
                  ?T("Enables/disables Roster Versioning. "
                     "The default value is 'false'.")
             }},
           {store_current_id,
            #{
              value => "true | false",
              desc =>
                  ?T("If this option is set to 'true', the current "
                     "roster version number is stored on the database. "
                     "If set to 'false', the roster version number is "
                     "calculated on the fly each time. Enabling this "
                     "option reduces the load for both ejabberd and the database. "
                     "This option does not affect the client in any way. "
                     "This option is only useful if option 'versioning' is "
                     "set to 'true'. The default value is 'false'. "
                     "IMPORTANT: if you use _`mod_shared_roster`_ or "
                     " _`mod_shared_roster_ldap`_, you must set the value "
                     "of the option to 'false'.")
             }},
           {db_type,
            #{
              value => "mnesia | sql",
              desc =>
                  ?T("Same as top-level _`default_db`_ option, but applied to this module only.")
             }},
           {use_cache,
            #{
              value => "true | false",
              desc =>
                  ?T("Same as top-level _`use_cache`_ option, but applied to this module only.")
             }},
           {cache_size,
            #{
              value => "pos_integer() | infinity",
              desc =>
                  ?T("Same as top-level _`cache_size`_ option, but applied to this module only.")
             }},
           {cache_missed,
            #{
              value => "true | false",
              desc =>
                  ?T("Same as top-level _`cache_missed`_ option, but applied to this module only.")
             }},
           {cache_life_time,
            #{
              value => "timeout()",
              desc =>
                  ?T("Same as top-level _`cache_life_time`_ option, but applied to this module only.")
             }}],
      example =>
          ["modules:",
           "  mod_roster:",
           "    versioning: true",
           "    store_current_id: false"]
     }.
