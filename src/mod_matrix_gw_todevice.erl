%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_todevice.erl
%%% Author  : Paavel A. <pd@narayana.im>
%%% Purpose : The gateway's per-device queue and Matrix to-device messages
%%% Created : 2 Nov 2026 by Paavel A. <pd@narayana.im>
%%%
%%% What is addressed to one device of a local user rides on an iq to that
%%% device's full JID, never a message (a message to an absent resource may
%%% be handed to another one; an iq fails at once):
%%%
%%%   the queue     matrix_todevice {{US, DeviceId, Seq}, kind, payload, ts}
%%%                 (disc_copies, ordered by seq; the counter per device is
%%%                 matrix_todevice_seq and never goes back, so the device's
%%%                 dedup on seq holds across restarts). queue/4 is the seam
%%%                 every part writes through; unlimited unless the option
%%%                 todevice_max says otherwise (the oldest dropped past it,
%%%                 a warning once per device per hour)
%%%   the courier   one gen_server per host (a child of mod_matrix_gw_sup)
%%%                 keeping each device's state: stopped, idle (seen, nothing
%%%                 in flight) or busy (one iq in flight). <resume/> and the
%%%                 device's resource coming online (set_presence_hook)
%%%                 start it; a result deletes the row and sends the next;
%%%                 an error or 30 s without an answer stops it until the
%%%                 next <resume/> or presence; unavailable stops it
%%%   to Matrix     <to-device/> from a local device: one m.direct_to_device
%%%                 EDU per remote server through the s2s EDU queue, rows of
%%%                 the queue for local users (the sender's other devices);
%%%                 a txn seen in the last five minutes is not sent again
%%%   from Matrix   m.direct_to_device in a /send (inbound_edu/3): a row per
%%%                 local device it names (`*`: every device of the user)
%%%
%%% The devices and their resources are mod_matrix_gw_e2e's (devices_of/2,
%%% the program's seam), the one-time-key count too (otk_count/3).
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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
%%%-------------------------------------------------------------------
-module(mod_matrix_gw_todevice).

-behaviour(gen_server).

-export([create_db/0, start_link/1, procname/1, queue/4, route_iq/1,
         inbound_edu/2, inbound_edu/3, set_presence_hook/4,
         unset_presence_hook/4, item_el/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-ifdef(TEST).
-export([items/2, next_item/2, device_of/3, devices_of/2, mem/1, state/1,
         to_device_parse/1, seen/3]).
-endif.

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").

-define(NS_E2E, <<"https://ns.another.im/matrix-e2e/0">>).
-define(IQ_TIMEOUT, 30000).
-define(MAX_JSON, 65536).
-define(TXN_TTL, 300000).
-define(WARN_EVERY, 3600000).
-define(PURGE_EVERY, 60000).

-type us() :: {binary(), binary()}.
-type item() :: {to_device, binary(), binary(), binary(), binary() | map()} |
                {devices_changed, [{binary(), integer() | binary()}]} |
                {keys_count, binary(), non_neg_integer(), boolean()} |
                {device_deleted, binary()}.

-record(matrix_todevice,
        {key :: {us(), binary() | '$1', pos_integer() | '_' | '$1'},
         kind :: atom(),
         payload :: item() | '_',
         ts :: integer() | '_'}).

-record(matrix_todevice_seq,
        {key :: {us(), binary()},
         seq :: non_neg_integer()}).

create_db() ->
    ejabberd_mnesia:create(
      ?MODULE, matrix_todevice,
      [{disc_copies, [node()]},
       {type, ordered_set},
       {attributes, record_info(fields, matrix_todevice)}]),
    ejabberd_mnesia:create(
      ?MODULE, matrix_todevice_seq,
      [{disc_copies, [node()]},
       {type, set},
       {attributes, record_info(fields, matrix_todevice_seq)}]),
    ok.

%%--------------------------------------------------------------------
%% The seam: an item for a device of a local user
%%--------------------------------------------------------------------

%% Item for User's device DeviceId (all: each device the user has), kept
%% until the device answers it; delivered at once when the device is being
%% served. User is the local part on the server Host.
-spec queue(binary(), binary(), binary() | all, item()) -> ok.
queue(Host, User, all, Item) ->
    lists:foreach(fun({Dev, _}) -> queue(Host, User, Dev, Item) end,
                  devices_of(Host, User));
queue(Host, User, Dev, Item) ->
    US = {User, Host},
    Seq = mnesia:dirty_update_counter(matrix_todevice_seq, {US, Dev}, 1),
    Payload = normalize(Item),
    mnesia:dirty_write(#matrix_todevice{key = {US, Dev, Seq}, kind = element(1, Payload),
                                        payload = Payload, ts = now_ms()}),
    bound(Host, US, Dev),
    gen_server:cast(procname(Host), {kick, US, Dev}).

normalize({to_device, Type, Sender, MsgID, Content}) when is_map(Content) ->
    {to_device, Type, Sender, MsgID, misc:json_encode(Content)};
normalize(Item) ->
    Item.

%% The device's seqs, oldest first.
-spec items(us(), binary()) -> [pos_integer()].
items(US, Dev) ->
    lists:sort(mnesia:dirty_select(
                 matrix_todevice,
                 [{#matrix_todevice{key = {US, Dev, '$1'}, _ = '_'}, [], ['$1']}])).

%% The device's oldest item, or none.
-spec next_item(us(), binary()) -> {pos_integer(), item()} | none.
next_item(US, Dev) ->
    case mnesia:dirty_next(matrix_todevice, {US, Dev, 0}) of
        {US, Dev, Seq} = Key ->
            case mnesia:dirty_read(matrix_todevice, Key) of
                [#matrix_todevice{payload = Item}] -> {Seq, Item};
                [] -> next_item(US, Dev)
            end;
        _ ->
            none
    end.

%% todevice_max: past it the oldest go, and the log says so once an hour.
bound(Host, US, Dev) ->
    case mod_matrix_gw_opt:todevice_max(Host) of
        unlimited ->
            ok;
        Max ->
            case items(US, Dev) of
                Seqs when length(Seqs) > Max ->
                    {Drop, _} = lists:split(length(Seqs) - Max, Seqs),
                    lists:foreach(fun(S) -> mnesia:dirty_delete(matrix_todevice, {US, Dev, S}) end,
                                  Drop),
                    warn(Host, US, Dev, length(Drop), Max);
                _ ->
                    ok
            end
    end.

warn(Host, {User, _}, Dev, N, Max) ->
    Key = {warned, User, Host, Dev},
    Now = now_ms(),
    case catch ets:lookup(mem(Host), Key) of
        [{_, Last, warn}] when Now - Last < ?WARN_EVERY ->
            ok;
        _ ->
            ?WARNING_MSG("Matrix gateway: the to-device queue of ~ts@~ts's device ~ts is "
                         "past todevice_max (~B): its oldest items are dropped (~B now)",
                         [User, Host, Dev, Max, N]),
            catch ets:insert(mem(Host), {Key, Now, warn}),
            ok
    end.

%%--------------------------------------------------------------------
%% The devices (mod_matrix_gw_e2e's)
%%--------------------------------------------------------------------

%% The suite stands in for the other modules and waits less.
-ifdef(TEST).
e2e() -> persistent_term:get({?MODULE, e2e}, mod_matrix_gw_e2e).
s2s() -> persistent_term:get({?MODULE, s2s}, mod_matrix_gw_s2s).
iq_timeout() -> persistent_term:get({?MODULE, iq_timeout}, ?IQ_TIMEOUT).
-else.
e2e() -> mod_matrix_gw_e2e.
s2s() -> mod_matrix_gw_s2s.
iq_timeout() -> ?IQ_TIMEOUT.
-endif.

-spec devices_of(binary(), binary()) -> [{binary(), binary()}].
devices_of(Host, User) ->
    (e2e()):devices_of(Host, User).

otk_count(Host, User, Dev) ->
    (e2e()):otk_count(Host, User, Dev).

%% The device bound to User's resource LRes, or none.
-spec device_of(binary(), binary(), binary()) -> {ok, binary()} | none.
device_of(Host, User, LRes) ->
    case [D || {D, R} <- devices_of(Host, User), jid:resourceprep(R) == LRes] of
        [Dev | _] -> {ok, Dev};
        [] -> none
    end.

%% Where Dev of User is (a replaced device too, until it is forgotten:
%% mod_matrix_gw_e2e:resource_of/3), or none.
resource_of(Host, User, Dev) ->
    case (e2e()):resource_of(Host, User, Dev) of
        {ok, Res} when is_binary(Res), Res /= <<>> -> {ok, Res};
        _ -> none
    end.

%% The devices of US with items queued.
queued_devices(US) ->
    lists:usort(mnesia:dirty_select(
                  matrix_todevice,
                  [{#matrix_todevice{key = {US, '$1', '_'}, _ = '_'}, [], ['$1']}])).

%%--------------------------------------------------------------------
%% Presence: a device online is served, offline is not
%%--------------------------------------------------------------------

-spec set_presence_hook(binary(), binary(), binary(), presence()) -> any().
set_presence_hook(User, Server, Resource, _Presence) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LRes = jid:resourceprep(Resource),
    US = {LUser, LServer},
    Own = case device_of(LServer, LUser, LRes) of
              {ok, Dev} -> [Dev];
              none -> []
          end,
    %% and a device replaced while it was away, whose resource this still
    %% is: its <device-deleted/> is waiting
    Replaced = [D || D <- queued_devices(US) -- Own,
                     case resource_of(LServer, LUser, D) of
                         {ok, R} -> jid:resourceprep(R) == LRes;
                         none -> false
                     end],
    [gen_server:cast(procname(LServer), {start, US, D, Resource}) || D <- Own ++ Replaced],
    ok.

-spec unset_presence_hook(binary(), binary(), binary(), binary()) -> any().
unset_presence_hook(User, Server, Resource, _Status) ->
    LServer = jid:nameprep(Server),
    gen_server:cast(procname(LServer), {stop, {jid:nodeprep(User), LServer},
                                        jid:resourceprep(Resource)}).

%%--------------------------------------------------------------------
%% The iqs to the gateway: <resume/> and <to-device/>
%%--------------------------------------------------------------------

-spec route_iq(iq()) -> any().
route_iq(#iq{type = set, from = From, lang = Lang,
             sub_els = [#xmlel{name = Name, attrs = Attrs} = El]} = IQ)
  when Name == <<"resume">>; Name == <<"to-device">> ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
        ?NS_E2E ->
            Host = ejabberd_config:get_myname(),
            Reply =
                case From of
                    #jid{luser = U, lserver = Host, lresource = R} when U /= <<>>, R /= <<>> ->
                        case device_of(Host, U, R) of
                            {ok, Dev} when Name == <<"resume">> ->
                                resume(Host, IQ, U, Dev, From#jid.resource);
                            {ok, Dev} ->
                                to_device(Host, IQ, U, Dev, El);
                            none ->
                                xmpp:make_error(IQ, xmpp:err_not_authorized(
                                                      <<"No Matrix device is registered by this resource">>, Lang))
                        end;
                    _ ->
                        xmpp:make_error(IQ, xmpp:err_not_authorized(
                                              <<"Only for the devices of this server's users">>, Lang))
                end,
            case Reply of
                sent -> ok;
                _ -> ejabberd_router:route(Reply)
            end;
        _ ->
            ejabberd_router:process_iq(IQ)
    end;
route_iq(IQ) ->
    ejabberd_router:process_iq(IQ).

resume(Host, IQ, User, Dev, Resource) ->
    US = {User, Host},
    Queued = length(items(US, Dev)),
    Count = otk_count(Host, User, Dev),
    %% the answer first, then the items
    ejabberd_router:route(
      xmpp:make_iq_result(
        IQ, #xmlel{name = <<"resume">>,
                   attrs = [{<<"xmlns">>, ?NS_E2E}, {<<"device">>, Dev},
                            {<<"queued">>, integer_to_binary(Queued)},
                            {<<"count">>, integer_to_binary(Count)}]})),
    gen_server:cast(procname(Host), {start, US, Dev, Resource}),
    sent.

to_device(Host, #iq{lang = Lang} = IQ, User, Dev, El) ->
    Type = fxml:get_tag_attr_s(<<"type">>, El),
    Txn = fxml:get_tag_attr_s(<<"txn">>, El),
    case to_device_parse(El) of
        _ when Type == <<>>; Txn == <<>> ->
            xmpp:make_error(IQ, xmpp:err_bad_request(<<"type and txn are required">>, Lang));
        too_large ->
            xmpp:make_error(IQ, (xmpp:err_not_acceptable(<<"Over 64 KiB of JSON">>, Lang))
                            #stanza_error{sub_els = [#xmlel{name = <<"too-large">>,
                                                            attrs = [{<<"xmlns">>, ?NS_E2E}]}]});
        error ->
            xmpp:make_error(IQ, xmpp:err_bad_request(<<"A user id or a content is not valid">>, Lang));
        {ok, Messages} ->
            case seen(Host, {txn, User, Txn}, true) of
                true ->
                    ok;
                false ->
                    send_to_device(Host, User, Dev, Type, Txn, Messages)
            end,
            xmpp:make_iq_result(IQ)
    end.

%% <user id><device id>JSON</device>...</user>...: [{{U, Server, MXID}, [{DevID, Content}]}],
%% or too_large (over 64 KiB of JSON in all), or error.
-spec to_device_parse(xmlel()) -> {ok, list()} | too_large | error.
to_device_parse(El) ->
    Users = [{fxml:get_tag_attr_s(<<"id">>, U),
              [{fxml:get_tag_attr_s(<<"id">>, D), fxml:get_tag_cdata(D)}
               || #xmlel{name = <<"device">>} = D <- U#xmlel.children]}
             || #xmlel{name = <<"user">>} = U <- El#xmlel.children],
    Size = lists:sum([byte_size(J) || {_, Ds} <- Users, {_, J} <- Ds]),
    if Size > ?MAX_JSON ->
            too_large;
       true ->
            try {ok, [{user(MXID), [{D, content(J)} || {D, J} <- Ds, D /= <<>>]}
                      || {MXID, Ds} <- Users]}
            catch throw:invalid -> error
            end
    end.

user(MXID) ->
    case mod_matrix_gw_room:parse_user_id(MXID) of
        {ok, U, S} when U /= <<>>, S /= <<>> -> {U, S, MXID};
        _ -> throw(invalid)
    end.

content(JSON) ->
    try misc:json_decode(JSON) of
        M when is_map(M) -> M;
        _ -> throw(invalid)
    catch _:_ -> throw(invalid)
    end.

send_to_device(Host, User, Dev, Type, Txn, Messages) ->
    Domain = mod_matrix_gw_opt:matrix_domain(Host),
    Sender = <<"@", User/binary, ":", Domain/binary>>,
    {Local, Remote} = lists:partition(fun({{_, S, _}, _}) -> S == Domain end, Messages),
    %% ours: rows of the queue (the sender's other devices, or another local user's)
    lists:foreach(
      fun({{U, _, _}, Ds}) ->
              case jid:nodeprep(U) of
                  LU when is_binary(LU), LU /= <<>> ->
                      Known = devices_of(Host, LU),
                      lists:foreach(
                        fun({D, Content}) ->
                                Item = {to_device, Type, Sender, Txn, Content},
                                Targets = case D of
                                              <<"*">> -> [K || {K, _} <- Known, {LU, K} /= {User, Dev}];
                                              _ -> [D || lists:keymember(D, 1, Known)]
                                          end,
                                case Targets of
                                    [] -> ?DEBUG("Matrix to-device for ~ts's unknown device ~ts", [LU, D]);
                                    _ -> [queue(Host, LU, T, Item) || T <- Targets]
                                end
                        end, Ds);
                  _ ->
                      ok
              end
      end, Local),
    %% theirs: one EDU per server
    ByServer = lists:foldl(
                 fun({{_, S, MXID}, Ds}, Acc) ->
                         Devs = maps:from_list(Ds),
                         maps:update_with(S, fun(M) -> maps:update_with(MXID, fun(Old) -> maps:merge(Old, Devs) end,
                                                                         Devs, M) end,
                                          #{MXID => Devs}, Acc)
                 end, #{}, Remote),
    maps:foreach(
      fun(Server, Msgs) ->
              (s2s()):queue_edu(
                Host, Server,
                #{<<"edu_type">> => <<"m.direct_to_device">>,
                  <<"content">> => #{<<"sender">> => Sender,
                                     <<"type">> => Type,
                                     <<"message_id">> => Txn,
                                     <<"messages">> => Msgs}})
      end, ByServer).

%% Whether Key was seen in the last five minutes; Mark: it is now.
-spec seen(binary(), term(), boolean()) -> boolean().
seen(Host, Key, Mark) ->
    Now = now_ms(),
    Seen = case catch ets:lookup(mem(Host), Key) of
               [{_, T, txn}] -> Now - T < ?TXN_TTL;
               _ -> false
           end,
    [catch ets:insert(mem(Host), {Key, Now, txn}) || Mark, not Seen],
    Seen.

%%--------------------------------------------------------------------
%% From Matrix: m.direct_to_device in a /send
%%--------------------------------------------------------------------

-spec inbound_edu(binary(), map()) -> ok.
inbound_edu(Host, EDU) ->
    inbound_edu(Host, undefined, EDU).

%% Origin: the server the transaction came from, which must be the
%% sender's (undefined: not checked).
-spec inbound_edu(binary(), binary() | undefined, map()) -> ok.
inbound_edu(Host, Origin, #{<<"content">> := #{<<"sender">> := Sender, <<"type">> := Type,
                                              <<"messages">> := Msgs} = C})
  when is_binary(Sender), is_binary(Type), is_map(Msgs) ->
    MsgID = case maps:get(<<"message_id">>, C, <<>>) of
                B when is_binary(B) -> B;
                _ -> <<>>
            end,
    case mod_matrix_gw_room:parse_user_id(Sender) of
        {ok, _, S} when Origin == undefined; S == Origin ->
            case MsgID /= <<>> andalso seen(Host, {in, S, Sender, MsgID}, true) of
                true -> ok;
                false -> deliver_inbound(Host, Sender, Type, MsgID, Msgs)
            end;
        _ ->
            ?INFO_MSG("Matrix gateway: an m.direct_to_device from ~ts names the sender ~ts: dropped",
                      [Origin, Sender])
    end;
inbound_edu(_Host, Origin, _EDU) ->
    ?DEBUG("Matrix gateway: a malformed m.direct_to_device from ~ts", [Origin]),
    ok.

deliver_inbound(Host, Sender, Type, MsgID, Msgs) ->
    Domain = mod_matrix_gw_opt:matrix_domain(Host),
    maps:foreach(
      fun(MXID, Devs) when is_map(Devs) ->
              case mod_matrix_gw_room:parse_user_id(MXID) of
                  {ok, U, Domain} ->
                      LU = jid:nodeprep(U),
                      Known = case LU of
                                  error -> [];
                                  _ -> devices_of(Host, LU)
                              end,
                      maps:foreach(
                        fun(D, Content) when is_map(Content) ->
                                Item = {to_device, Type, Sender, MsgID, Content},
                                case D of
                                    <<"*">> -> [queue(Host, LU, K, Item) || {K, _} <- Known];
                                    _ ->
                                        case lists:keymember(D, 1, Known) of
                                            true -> queue(Host, LU, D, Item);
                                            false -> ?DEBUG("Matrix to-device for ~ts's unknown device ~ts",
                                                            [MXID, D])
                                        end
                                end;
                           (_, _) ->
                                ok
                        end, Devs);
                  _ ->
                      ?DEBUG("Matrix to-device for ~ts, not a user of ours", [MXID])
              end;
         (_, _) ->
              ok
      end, Msgs),
    ok.

%%--------------------------------------------------------------------
%% The items on the wire
%%--------------------------------------------------------------------

-spec item_el(binary(), pos_integer(), item()) -> xmlel().
item_el(Dev, Seq, {to_device, Type, Sender, MsgID, JSON}) ->
    el(<<"to-device">>, Seq, [{<<"type">>, Type}, {<<"sender">>, Sender},
                              {<<"message_id">>, MsgID}, {<<"device">>, Dev}],
       [{xmlcdata, JSON}]);
item_el(_Dev, Seq, {devices_changed, Users}) ->
    el(<<"devices-changed">>, Seq, [],
       [#xmlel{name = <<"user">>, attrs = [{<<"id">>, MXID}, {<<"stream_id">>, bin(Stream)}]}
        || {MXID, Stream} <- Users]);
item_el(_Dev, Seq, {keys_count, Alg, Count, FallbackUsed}) ->
    el(<<"keys-count">>, Seq,
       [{<<"algorithm">>, Alg}, {<<"count">>, bin(Count)}] ++
           [{<<"fallback_used">>, <<"1">>} || FallbackUsed == true],
       []);
item_el(_Dev, Seq, {device_deleted, ID}) ->
    el(<<"device-deleted">>, Seq, [{<<"id">>, ID}], []).

el(Name, Seq, Attrs, Children) ->
    #xmlel{name = Name, attrs = [{<<"xmlns">>, ?NS_E2E}, {<<"seq">>, integer_to_binary(Seq)} | Attrs],
           children = Children}.

bin(I) when is_integer(I) -> integer_to_binary(I);
bin(B) when is_binary(B) -> B.

%%--------------------------------------------------------------------
%% The courier
%%--------------------------------------------------------------------

procname(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

%% The courier's memory: txns seen, warnings logged (an ets table it owns).
mem(Host) ->
    gen_mod:get_module_proc(Host, matrix_todevice_mem).

start_link(Host) ->
    gen_server:start_link({local, procname(Host)}, ?MODULE, [Host], []).

-ifdef(TEST).
%% The devices' states (for the tests).
state(Host) ->
    gen_server:call(procname(Host), state).
-endif.

init([Host]) ->
    ets:new(mem(Host), [named_table, public, set]),
    erlang:send_after(?PURGE_EVERY, self(), purge),
    self() ! online,
    {ok, #{host => Host, devs => #{}}}.

handle_call(state, _From, #{devs := Devs} = State) ->
    {reply, Devs, State};
handle_call(_Request, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast({start, US, Dev, Res}, #{devs := Devs} = State) ->
    K = {US, Dev},
    case maps:get(K, Devs, stopped) of
        {busy, _, Seq} -> {noreply, State#{devs := Devs#{K => {busy, Res, Seq}}}};
        _ -> {noreply, next(K, Res, State)}
    end;
handle_cast({kick, US, Dev}, #{devs := Devs} = State) ->
    K = {US, Dev},
    case maps:get(K, Devs, stopped) of
        {idle, Res} -> {noreply, next(K, Res, State)};
        _ -> {noreply, State}
    end;
handle_cast({stop, US, LRes}, #{devs := Devs} = State) ->
    {noreply, State#{devs := maps:filter(
                                fun({U, _}, S) when U == US -> jid:resourceprep(element(2, S)) /= LRes;
                                   (_, _) -> true
                                end, Devs)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({iq_reply, Reply, {{US, Dev} = K, Seq}}, #{devs := Devs} = State) ->
    Current = maps:get(K, Devs, stopped),
    case Reply of
        #iq{type = result} ->
            mnesia:dirty_delete(matrix_todevice, {US, Dev, Seq}),
            case Current of
                {busy, Res, Seq} -> {noreply, next(K, Res, State)};
                _ -> {noreply, State}
            end;
        _ ->
            log_failure(US, Dev, Seq, Reply),
            case Current of
                {busy, _, Seq} -> {noreply, State#{devs := maps:remove(K, Devs)}};
                _ -> {noreply, State}
            end
    end;
handle_info(online, #{host := Host} = State) ->
    %% a restarted courier: the devices online with items are served again
    Keys = lists:usort([{US, Dev} || {{_, S} = US, Dev, _} <- mnesia:dirty_all_keys(matrix_todevice),
                                     S == Host]),
    {noreply, lists:foldl(
                fun({{U, S}, Dev} = K, Acc) ->
                        case resource_of(S, U, Dev) of
                            {ok, Res} ->
                                case ejabberd_sm:get_session_pid(U, S, Res) of
                                    Pid when is_pid(Pid) -> next(K, Res, Acc);
                                    _ -> Acc
                                end;
                            none ->
                                Acc
                        end
                end, State, Keys)};
handle_info(purge, #{host := Host} = State) ->
    Now = now_ms(),
    ets:select_delete(mem(Host), [{{'_', '$1', txn}, [{'<', '$1', Now - ?TXN_TTL}], [true]},
                                  {{'_', '$1', warn}, [{'<', '$1', Now - ?WARN_EVERY}], [true]}]),
    erlang:send_after(?PURGE_EVERY, self(), purge),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% K's oldest item to its resource Res, or K idle when there is none.
next({{User, Server} = US, Dev} = K, Res, #{host := Host, devs := Devs} = State) ->
    case next_item(US, Dev) of
        none ->
            State#{devs := Devs#{K => {idle, Res}}};
        {Seq, Item} ->
            IQ = #iq{type = set, id = <<>>,
                     from = jid:make(mod_matrix_gw_opt:host(Host)),
                     to = jid:make(User, Server, Res),
                     sub_els = [item_el(Dev, Seq, Item)]},
            ejabberd_router:route_iq(IQ, {K, Seq}, self(), iq_timeout()),
            State#{devs := Devs#{K => {busy, Res, Seq}}}
    end.

log_failure({User, Server}, Dev, Seq, timeout) ->
    ?INFO_MSG("Matrix gateway: ~ts@~ts's device ~ts did not answer item ~B in 30 s: "
              "its queue waits for the device", [User, Server, Dev, Seq]);
log_failure({User, Server}, Dev, Seq, #iq{} = Err) ->
    NotMine = case xmpp:get_error(Err) of
                  #stanza_error{sub_els = Els} ->
                      lists:any(fun(#xmlel{name = <<"not-mine">>}) -> true; (_) -> false end, Els);
                  _ -> false
              end,
    case NotMine of
        true ->
            ?INFO_MSG("Matrix gateway: ~ts@~ts's resource does not hold device ~ts (item ~B): "
                      "kept for the device's next binding", [User, Server, Dev, Seq]);
        false ->
            ?INFO_MSG("Matrix gateway: ~ts@~ts's device ~ts refused item ~B: ~ts",
                      [User, Server, Dev, Seq, format_error(xmpp:get_error(Err))])
    end.

format_error(#stanza_error{reason = R}) when is_atom(R) -> atom_to_binary(R);
format_error(_) -> <<"an error">>.

now_ms() ->
    erlang:system_time(millisecond).
