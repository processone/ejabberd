%%%----------------------------------------------------------------------
%%% File    : mod_antispam_report.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : handle/forward incoming spam/absuse reports
%%% Created : 15 Oct 2026 by Stefan Strigler <stefan@strigler.de>
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
-module(mod_antispam_report).
-author('stefan@strigler.de').
-behaviour(gen_server).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-export([start/2, stop/1]).

%% gen_server callbacks.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% hooks
-export([spam_reported/5,
         s2s_handle_cast/2]).

-record(state,
        {host = <<>>		 :: binary(),
         pending_reports = []    :: [{{jid(), #report{}}, integer()}],
         report_addresses = #{}  :: #{binary() => {binary(), integer()}},
         retry_timers = #{}      :: #{binary() => #{ref => reference(),
                                                    count => non_neg_integer()}}
        }).

-type state() :: #state{}.

-define(PENDING_TIMEOUT_SECS, 3600).
-define(PENDING_MAX_ITEMS, 1000).
-define(ADDRESSES_TIMEOUT_SECS, 86400).
-define(ADDRESSES_MAX_SIZE, 1000).
-define(RETRY_TIMEOUT, 60000).
-define(MAX_RETRIES, 120).

%% @format-begin

-spec start(binary(), gen_mod:opts()) ->
               {ok, Pid :: pid()} |
               {error, Error :: {already_started, pid()}} |
               {error, Error :: term()}.
start(Host, Opts) ->
    ejabberd_hooks:add(s2s_in_handle_cast, Host, ?MODULE, s2s_handle_cast, 1000),
    ejabberd_hooks:add(s2s_out_handle_cast, Host, ?MODULE, s2s_handle_cast, 1000),
    {ok, _Pid} = gen_mod:start_child(?MODULE, Host, Opts).

-spec stop(binary()) -> ok | {error, any()}.
stop(Host) ->
    ejabberd_hooks:delete(s2s_in_handle_cast, Host, ?MODULE, s2s_handle_cast, 1000),
    ejabberd_hooks:delete(s2s_out_handle_cast, Host, ?MODULE, s2s_handle_cast, 1000),
    gen_mod:stop_child(?MODULE, Host).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: state()}.
init([Host, _Opts]) ->
    process_flag(trap_exit, true),
    {ok, #state{host = Host}}.

-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                     {reply, Reply :: term(), NewState :: state()}.
handle_call({forward_report, JID, ReceivedReport}, _From, State) ->
    case maps:find(JID#jid.lserver, State#state.report_addresses) of
        {ok, {ReportAddress, _}} ->
            send_report(origin, State#state.host, ReportAddress, JID, ReceivedReport),
            {reply, ok, cleanup_state(State)};
        error ->
            send_disco(jid:make(State#state.host), jid:make(JID#jid.lserver)),
            {reply, pending, cleanup_state(add_pending_report({JID, ReceivedReport}, State))}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, cleanup_state(State)}.

-spec handle_cast(Request :: term(), State :: term()) -> {noreply, NewState :: state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                     {noreply, NewState :: state()}.
handle_info({iq_reply, timeout, {disco_info_reply, OriginServer}}, State) ->
    ?INFO_MSG("Timeout in disco query for ~s", [OriginServer]),
    {noreply, cleanup_state(retry_send_disco(State, OriginServer))};
handle_info({iq_reply, #iq{type = error} = IQ, {disco_info_reply, OriginServer}},
            State) ->
    ?INFO_MSG("Error doing disco query for ~s: ~p", [OriginServer, IQ]),
    {noreply, cleanup_state(retry_send_disco(State, OriginServer))};
handle_info({iq_reply, #iq{type = result} = IQ, {disco_info_reply, OriginServer}},
            State) ->
    ?DEBUG("Result doing disco query for ~s: ~p", [OriginServer, IQ]),
    #iq{sub_els = [#disco_info{xdata = Xdatas}]} = IQ,
    case extract_address(Xdatas) of
        <<>> ->
            ?INFO_MSG("Failed extracting address for ~s from ~p", [OriginServer, Xdatas]),
            {noreply, cleanup_state(retry_send_disco(State, OriginServer))};
        ReportAddress ->
            ?DEBUG("Got address: ~s", [ReportAddress]),
            NewState = add_report_address(State, OriginServer, ReportAddress),
            Pending = send_pending(NewState),
            {noreply, cleanup_state(NewState#state{pending_reports = Pending})}
    end;
handle_info({send_disco, OriginServer}, State) ->
    send_disco(jid:make(State#state.host), jid:make(OriginServer)),
    {noreply, cleanup_state(State)};
handle_info(_Info, State) ->
    {noreply, cleanup_state(State)}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) ->
                   any().
terminate(_Reason, #state{retry_timers = Timers} = _State) ->
    lists:foreach(fun(#{ref := Ref}) -> erlang:cancel_timer(Ref) end, maps:values(Timers)),
    ok.

-spec code_change(OldVsn :: term() | {down, term()}, State :: term(), Extra :: term()) ->
                     {ok, NewState :: term()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% hooks
%%--------------------------------------------------------------------

s2s_handle_cast(State, {get_ip, Pid}) ->
    #{ip := {Ip, _Port}, remote_server := RemoteServer} = State,
    Pid ! {s2s_ip, {RemoteServer, Ip}},
    State;
s2s_handle_cast(State, _) ->
    State.

spam_reported(Host, FromJID, ReportedJID, Reason, Report) ->
    ?INFO_MSG("Got ~p report from ~s for jid ~s",
              [Reason, jid:encode(FromJID), jid:encode(ReportedJID)]),
    #jid{lserver = OriginServer} = ReportedJID,
    ReceivedReport = make_received_report(Host, FromJID, ReportedJID, Report),
    maybe_report_origin(Host,
                        ReportedJID,
                        maybe_anonymize(Host, OriginServer, ReceivedReport)),
    maybe_report_third_party(Host, ReportedJID, ReceivedReport),
    ok.

make_received_report(Host, FromJID, JID, Report) ->
    UUID =
        uuid:uuid_to_string(
            uuid:get_v4(), binary_standard),
    Now = erlang:timestamp(),
    ReportedEntity = maybe_add_ip(Host, #reported_entity{jid = JID}),
    #received_report{id = UUID,
                     report = Report,
                     reported_at = Now,
                     reported_entity = ReportedEntity,
                     reporter = make_reporter(FromJID),
                     stanzas = get_stanzas(Host, FromJID, JID, Report)}.

%% strip_report(Report) ->
%%     Report#report{report_origin = false, third_party = false}.

maybe_add_ip(Host, #reported_entity{jid = #jid{lserver = Host} = JID} = ReportedEntity) ->
    %% A user on our host, we can lookup a client IP
    case get_client_ip(Host, JID) of
        undefined ->
            ReportedEntity;
        ClientIP ->
            ReportedEntity#reported_entity{ip = #reported_ip{type = client, cdata = ClientIP}}
    end;
maybe_add_ip(Host, #reported_entity{jid = #jid{lserver = LServer}} = ReportedEntity) ->
    case get_server_ip(Host, LServer) of
        undefined ->
            ReportedEntity;
        ServerIP ->
            ReportedEntity#reported_entity{ip = #reported_ip{type = server, cdata = ServerIP}}
    end.

get_client_ip(_Host, JID) ->
    case jid:tolower(JID) of
        {User, Server, <<>>} ->
            case ejabberd_sm:get_user_info(User, Server) of
                [{_Res, Info} | _] ->
                    {ip, {IP, _Port}} = lists:keyfind(ip, 1, Info),
                    IP;
                _ ->
                    undefined
            end;
        {User, Server, Resource} ->
            case ejabberd_sm:get_user_info(User, Server, Resource) of
                offline ->
                    undefined;
                Info ->
                    {ip, {IP, _Port}} = lists:keyfind(ip, 1, Info),
                    IP
            end
    end.

get_server_ip(Host, Server) ->
    case ejabberd_s2s:get_connections_pids({Host, Server}) of
        [Pid | _] ->
            try p1_server:cast(Pid, {get_ip, self()}) of
                _ ->
                    ok
            catch
                _:_ ->
                    ok
            end,

            receive
                {s2s_ip, {Server, Ip}} ->
                    Ip
            after 100 ->
                undefined
            end;
        _ ->
            undefined
    end.

make_reporter(JID) ->
    #reporter{jid = JID}.

get_stanzas(_, _, _, #report{stanza_ids = []}) ->
    undefined;
get_stanzas(Host, FromJID, With, #report{stanza_ids = StanzaIds}) ->
    {Results, _Complete, _Count} =
        mod_mam:select(Host, FromJID, FromJID,
                       [{'with', With},
                        {'ids', [Id || #stanza_id{by = By, id = Id} <- StanzaIds, By == With]}],
                       undefined, chat, only_messages),
    [Forwarded || {_BinID, _ID, #forwarded{} = Forwarded} <- Results].

maybe_anonymize(Host, To, ReceivedReport) ->
    case lists:member(To, trusted_origin_servers(Host)) of
        true ->
            ReceivedReport;
        false ->
            lists:foldl(fun anonymize/2, ReceivedReport, [reporter, reported_entity, stanzas])
    end.

anonymize(reporter, #received_report{reporter = Reporter} = ReceivedReport) ->
    SemiAnonReporter = #reporter{id = reporter_id(jid:encode(Reporter#reporter.jid))},
    ReceivedReport#received_report{reporter = SemiAnonReporter};
anonymize(reported_entity,
          #received_report{reported_entity =
                               #reported_entity{ip = #reported_ip{type = client}} =
                                   ReportedEntity} =
              ReceivedReport) ->
    ReceivedReport#received_report{reported_entity =
                                       ReportedEntity#reported_entity{ip = undefined}};
anonymize(reported_entity, ReceivedReport) ->
    ReceivedReport;
anonymize(stanzas, #received_report{stanzas = undefined} = ReceivedReport) ->
    ReceivedReport;
anonymize(stanzas, #received_report{stanzas = Stanzas} = ReceivedReport) ->
    ReceivedReport#received_report{stanzas = [remove_to(Stanza) || Stanza <- Stanzas]}.

trusted_origin_servers(Host) ->
    %% We always trust ourselves
    %% TODO make it a config opt to add servers here
    [Host].

remove_to(#forwarded{sub_els = Els} = Forwarded) ->
    AnonEls = [remove_to(El) || El <- Els],
    Forwarded#forwarded{sub_els = AnonEls};
remove_to(#message{} = Msg) ->
    Msg#message{to = undefined};
remove_to(Other) ->
    Other.

maybe_report_origin(Host,
                    JID,
                    #received_report{report = #report{report_origin = ReportOrigin}} =
                        ReceivedReport) ->
    case {mod_antispam_opt:forward_reports(Host), ReportOrigin} of
        {true, true} ->
            report_origin(Host, JID, ReceivedReport);
        _ ->
            ok
    end.

maybe_report_third_party(Host,
                         JID,
                         #received_report{report = #report{third_party = ThirdParty}} =
                             ReceivedReport) ->
    case {mod_antispam_opt:forward_reports_to_third_party(Host), ThirdParty} of
        {ReportJIDs, true} when ReportJIDs =/= [] ->
            %% We don't allow forwarding IP information to third parties at this time.
            report_third_party(Host, ReportJIDs, JID, anonymize(reported_entity, ReceivedReport));
        _ ->
            ok
    end.

report_origin(Host, JID, Report) ->
    mod_antispam_util:try_call_by_host(Host, ?MODULE, {forward_report, JID, Report}).

report_third_party(Host, ReportJIDs, JID, ReceivedReport) ->
    lists:foreach(fun(ReportJID) ->
                     send_report(third_party, Host, ReportJID, JID, ReceivedReport)
                  end,
                  ReportJIDs).

send_report(Type, Host, To, JID, ReceivedReport) when is_binary(To) ->
    send_report(Type, Host, jid:decode(To), JID, ReceivedReport);
send_report(Type, Host, ToJID, JID, ReceivedReport) ->
    ?DEBUG("Sending report to ~s", [jid:encode(ToJID)]),
    Msg = make_report_message(Type, ToJID, JID, ReceivedReport),
    send(Host, ToJID, Msg).

make_report_message(third_party, ToJID, _JID, ReceivedReport) ->
    #message{to = ToJID, sub_els = [ReceivedReport]};
make_report_message(_Type, ToJID, JID, ReceivedReport) ->
    #received_report{reported_at = Timestamp} = ReceivedReport,
    ReportVars =
        #{<<"reported_jid">> => jid:encode(JID),
          <<"reported_message_time">> => xmpp_util:encode_timestamp(Timestamp)},
    #message{to = ToJID,
             sub_els = [ReceivedReport],
             body = [#text{data = report_to_text(ReportVars)}]}.

send(Host, To, Stanza) ->
    ejabberd_router:route(
        xmpp:set_from_to(Stanza, jid:make(Host), To)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_disco(From, To) ->
    Disco =
        #iq{type = get,
            to = To,
            from = From,
            sub_els = [#disco_info{}]},
    ejabberd_router:route_iq(Disco, {disco_info_reply, jid:encode(To)}, self()).

add_pending_report(PendingReport, #state{pending_reports = Pending0} = State) ->
    Pending =
        if length(Pending0) == ?PENDING_MAX_ITEMS ->
               lists:droplast(Pending0);
           true ->
               Pending0
        end,
    State#state{pending_reports = [{PendingReport, erlang:system_time(seconds)} | Pending]}.

-spec extract_address([#xdata{}]) -> binary().
extract_address([]) ->
    <<>>;
extract_address([#xdata{type = result} = Xdata | Others]) ->
    case xmpp_util:get_xdata_values(<<"FORM_TYPE">>, Xdata) of
        [<<"http://jabber.org/network/serverinfo">>] ->
            grep_for_xmpp_address(lists:append([xmpp_util:get_xdata_values(Field, Xdata)
                                                || Field
                                                       <- [<<"report-addresses">>,
                                                           <<"abuse-addresses">>,
                                                           <<"admin-addresses">>]]));
        _ ->
            extract_address(Others)
    end;
extract_address([#xdata{} | Others]) ->
    extract_address(Others).

-spec grep_for_xmpp_address([binary()]) -> binary().
grep_for_xmpp_address([]) ->
    <<>>;
grep_for_xmpp_address([<<"xmpp:", Address/binary>> | _]) ->
    Address;
grep_for_xmpp_address([_Address | Others]) ->
    grep_for_xmpp_address(Others).

send_pending(#state{host = Host,
                    pending_reports = Pending,
                    report_addresses = Addresses}) ->
    send_pending(Pending, [], Addresses, Host).

send_pending([], Acc, _, _) ->
    Acc;
send_pending([{{JID, Report}, _Ts1} | Pending], Acc, Addresses, Host) ->
    NewAcc =
        case maps:find(JID#jid.lserver, Addresses) of
            {ok, {To, _Ts2}} ->
                send_report(origin, Host, To, JID, Report),
                Acc;
            error ->
                [{JID, Report} | Acc]
        end,
    send_pending(Pending, NewAcc, Addresses, Host).

add_report_address(#state{report_addresses = Addresses0} = State,
                   OriginServer,
                   ReportAddress) ->
    Addresses =
        case maps:size(Addresses0) of
            ?ADDRESSES_MAX_SIZE ->
                remove_oldest(Addresses0);
            _ ->
                Addresses0
        end,
    Now = erlang:system_time(seconds),
    State#state{report_addresses = Addresses#{OriginServer => {ReportAddress, Now}}}.

remove_oldest(M) ->
    I = maps:iterator(M),
    {Oldest, _, _} =
        maps:fold(fun(K1, {V1, T1}, {_K2, {_V2, T2}, I2} = Acc) ->
                     if T1 < T2 ->
                            {K1, {V1, T1}, I2};
                        true ->
                            Acc
                     end
                  end,
                  maps:next(I),
                  I),
    maps:remove(Oldest, M).

cleanup_state(#state{pending_reports = Pending, report_addresses = Addresses} = State) ->
    Now = erlang:system_time(seconds),
    FilteredPending =
        lists:filter(fun({_Report, Then}) -> Now - Then < ?PENDING_TIMEOUT_SECS end, Pending),
    FilteredAddresses =
        maps:filter(fun(_K, {_V, Then}) -> Now - Then < ?ADDRESSES_TIMEOUT_SECS end, Addresses),
    State#state{pending_reports = FilteredPending, report_addresses = FilteredAddresses}.

retry_send_disco(#state{retry_timers = Timers0} = State, OriginServer) ->
    %% TODO exponential backoff
    case maps:get(OriginServer, Timers0, #{count => 0}) of
        #{count := Count} when Count >= ?MAX_RETRIES ->
            ?INFO_MSG("Stop querying ~s, max tries reached", [OriginServer]),
            State;
        #{count := Count} ->
            Timers =
                Timers0#{OriginServer =>
                             #{ref =>
                                   erlang:send_after(?RETRY_TIMEOUT,
                                                     self(),
                                                     {send_disco, OriginServer}),
                               count => Count + 1}},
            State#state{retry_timers = Timers}
    end.

reporter_id(Msg) when Msg /= <<>> ->
    SecretKey = ejabberd_config:get_shared_key(),
    base64:encode(
        crypto:mac(hmac, sha, SecretKey, Msg)).
%% @format-end

report_to_text(Vars) ->
    T =
"""
SPAM/ABUSE REPORT
-----------------

Reported JID: {reported_jid}

A user on our service has reported a message originating from the above JID on
your server.

The reported message was sent at: {reported_message_time}

--
This message contains also machine-readable payloads, including XEP-0377, in case
you want to automate handling of these reports. You can receive these reports
to a different address by setting 'report-addresses' in your server
contact info configuration. For more information, see https://xmppbl.org/reports/
""",
    maps:fold(fun(K, V, Acc) -> re:replace(Acc, <<"{", K/binary, "}">>, V, [{return, binary}])
              end,
              T,
              Vars).
