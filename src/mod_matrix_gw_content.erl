%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_content.erl
%%% Author  : Paavel A. <pd@narayana.im>
%%% Purpose : What a message carries besides its text, both ways
%%% Created : 2 Nov 2026 by Paavel A. <pd@narayana.im>
%%%
%%% The message content a daily driver needs, each a standard XEP mapped to
%%% the Matrix spec, as functions of their input, so the room module calls
%%% them and the tests check them on their own:
%%%
%%%   XMPP                                       Matrix
%%%   XEP-0461 <reply/> + XEP-0428 fallback      m.relates_to m.in_reply_to (the spec's
%%%                                              reply fallback stripped when it comes;
%%%                                              none sent, spec v1.13); m.thread is
%%%                                              read as a reply to its parent or root
%%%   XEP-0372 <reference type='mention'/>,      m.mentions user_ids (+ the body's name),
%%%   XEP-0513 <mention/>                        m.mentions room
%%%   XEP-0080 <geoloc/>, a geo: URI body        m.location geo_uri
%%%   urn:fluux:poll:0 <poll/>                   org.matrix.msc3381.poll.start (m.poll.start
%%%                                              read too), answer ids = the emojis
%%%   a XEP-0444 reaction with a poll's emoji    org.matrix.msc3381.poll.response
%%%   urn:fluux:poll:0 <poll-closed/>            org.matrix.msc3381.poll.end
%%%   XEP-0449 <sticker/> beside a XEP-0447      m.sticker
%%%   share
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
-module(mod_matrix_gw_content).

-export([rel/2, mention_mxid/2, mention_mxid/4, resolve_rel/2, outbound/3,
         inbound_els/3, in_reply_to/1, strip_matrix_reply/1, quote/1,
         poll_start/1, poll_response/1, poll_end_target/1, poll_options/2,
         poll_els/3, poll_closed_els/4, vote_split/2, response_content/2,
         sticker_content/2, sticker_els/0, nick_el/1, cp_len/1,
         is_content_type/1, geo_els/2, receipt_els/2, failed_pdus/1]).

-include_lib("xmpp/include/xmpp.hrl").

-define(NS_REPLY, <<"urn:xmpp:reply:0">>).
-define(NS_FALLBACK, <<"urn:xmpp:fallback:0">>).
-define(NS_REFERENCE, <<"urn:xmpp:reference:0">>).
-define(NS_MENTIONS, <<"urn:xmpp:mentions:0">>).
-define(MENTION_CHANNEL, <<"urn:xmpp:mentions:0#channel">>).
-define(NS_GEOLOC, <<"http://jabber.org/protocol/geoloc">>).
-define(NS_POLL, <<"urn:fluux:poll:0">>).
-define(NS_STICKERS, <<"urn:xmpp:stickers:0">>).
-define(NS_SFS, <<"urn:xmpp:sfs:0">>).
-define(NS_NICK_EL, <<"http://jabber.org/protocol/nick">>).
-define(XMPP_ID, <<"net.process-one.xmpp-id">>).
-define(POLL_START, <<"org.matrix.msc3381.poll.start">>).
-define(POLL_RESPONSE, <<"org.matrix.msc3381.poll.response">>).
-define(POLL_END, <<"org.matrix.msc3381.poll.end">>).
-define(TEXT1767, <<"org.matrix.msc1767.text">>).
-define(MAX_MENTIONS, 64).

%% The Matrix event types besides m.room.message and m.reaction that carry
%% content to the XMPP side.
-spec is_content_type(binary()) -> boolean().
is_content_type(<<"m.sticker">>) -> true;
is_content_type(?POLL_START) -> true;
is_content_type(?POLL_RESPONSE) -> true;
is_content_type(?POLL_END) -> true;
is_content_type(<<"m.poll.start">>) -> true;
is_content_type(<<"m.poll.response">>) -> true;
is_content_type(<<"m.poll.end">>) -> true;
is_content_type(_) -> false.

%%--------------------------------------------------------------------
%% XMPP to Matrix
%%--------------------------------------------------------------------

%% What a message's elements add to its text: {Text2, Rel}, Text2 the body
%% with the XEP-0428 fallback of a reply cut out, Rel a map of
%%   reply => {To, ID}         XEP-0461
%%   mentions => [URI]         XEP-0372 references of type mention, XEP-0513
%%                             mentions of a jid (xmpp: URIs)
%%   room_mention => true      XEP-0513 #channel
%%   geo => map()              XEP-0080 <geoloc/> (lat, lon, accuracy,
%%                             description, text) or #{uri} of a body that
%%                             is a geo: URI alone
%%   poll => map()             urn:fluux:poll:0 <poll/> (title, description,
%%                             options [{Emoji, Label}], multiple, hide)
%%   poll_closed => map()      <poll-closed/> (message_id, title)
-spec rel([xmpp_element() | xmlel()], binary()) -> {binary(), map()}.
rel(Els0, Text) ->
    Els = [xmlel(E) || E <- Els0],
    {Text2, R1} =
        case find(Els, <<"reply">>, ?NS_REPLY) of
            #xmlel{} = R ->
                case attr(<<"id">>, R) of
                    <<>> -> {Text, #{}};
                    ID -> {strip_fallback(Els, ?NS_REPLY, Text),
                           #{reply => {attr(<<"to">>, R), ID}}}
                end;
            none ->
                {Text, #{}}
        end,
    R2 = case mentions(Els) of
             [] -> R1;
             Ms -> R1#{mentions => Ms}
         end,
    R3 = case [M || #xmlel{name = <<"mention">>} = M <- Els, xmlns(M) == ?NS_MENTIONS,
                    attr(<<"mentions">>, M) == ?MENTION_CHANNEL] of
             [] -> R2;
             _ -> R2#{room_mention => true}
         end,
    R4 = case geo(Els, Text2) of
             none -> R3;
             G -> R3#{geo => G}
         end,
    R5 = case poll(Els) of
             none -> R4;
             P -> R4#{poll => P}
         end,
    R6 = case poll_closed(Els) of
             none -> R5;
             C -> R5#{poll_closed => C}
         end,
    {Text2, R6}.

xmlel(#xmlel{} = El) -> El;
xmlel(El) ->
    try xmpp:encode(El) catch _:_ -> El end.

find([], _Name, _NS) -> none;
find([#xmlel{name = Name} = El | Els], Name, NS) ->
    case xmlns(El) of
        NS -> El;
        _ -> find(Els, Name, NS)
    end;
find([_ | Els], Name, NS) -> find(Els, Name, NS).

xmlns(#xmlel{attrs = Attrs}) -> fxml:get_attr_s(<<"xmlns">>, Attrs).
attr(Name, #xmlel{attrs = Attrs}) -> fxml:get_attr_s(Name, Attrs).
cdata(El, Name) ->
    string:trim(fxml:get_path_s(El, [{elem, Name}, cdata])).

%% Text without the spans XEP-0428 <fallback for=For/> marks (code point
%% offsets, end exclusive); a <fallback/> naming no <body/> is the whole
%% text.
strip_fallback(Els, For, Text) ->
    Fallbacks = [F || #xmlel{name = <<"fallback">>} = F <- Els,
                      xmlns(F) == ?NS_FALLBACK, attr(<<"for">>, F) == For],
    case unicode:characters_to_list(Text) of
        Cps when is_list(Cps), Fallbacks /= [] ->
            Len = length(Cps),
            Ranges =
                lists:append(
                  [case [B || #xmlel{name = <<"body">>} = B <- F#xmlel.children] of
                       [] -> [{0, Len}];
                       Bodies -> [{offset(attr(<<"start">>, B), 0), offset(attr(<<"end">>, B), Len)}
                                  || B <- Bodies]
                   end || F <- Fallbacks]),
            Keep = [C || {I, C} <- lists:zip(lists:seq(0, Len - 1), Cps),
                         not lists:any(fun({S, E}) -> I >= S andalso I < E end, Ranges)],
            unicode:characters_to_binary(Keep);
        _ ->
            Text
    end.

offset(<<>>, Default) -> Default;
offset(B, Default) ->
    case catch binary_to_integer(B) of
        N when is_integer(N), N >= 0 -> N;
        _ -> Default
    end.

mentions(Els) ->
    Refs = [U || #xmlel{name = <<"reference">>} = R <- Els, xmlns(R) == ?NS_REFERENCE,
                 attr(<<"type">>, R) == <<"mention">>, U <- [attr(<<"uri">>, R)], U /= <<>>],
    Jids = [<<"xmpp:", J/binary>> || #xmlel{name = <<"mention">>} = M <- Els,
                                     xmlns(M) == ?NS_MENTIONS, J <- [attr(<<"jid">>, M)], J /= <<>>],
    lists:sublist(lists:usort(Refs ++ Jids), ?MAX_MENTIONS).

geo(Els, Text) ->
    case find(Els, <<"geoloc">>, ?NS_GEOLOC) of
        #xmlel{} = G ->
            Lat = cdata(G, <<"lat">>),
            Lon = cdata(G, <<"lon">>),
            case is_degrees(Lat, 90) andalso is_degrees(Lon, 180) of
                true ->
                    maps:from_list(
                      [{K, V} || {K, V} <- [{lat, Lat}, {lon, Lon},
                                            {accuracy, cdata(G, <<"accuracy">>)},
                                            {description, cdata(G, <<"description">>)},
                                            {text, cdata(G, <<"text">>)}],
                                 V /= <<>>]);
                false ->
                    none
            end;
        none ->
            case geo_uri(string:trim(Text)) of
                {ok, _} -> #{uri => string:trim(Text)};
                error -> none
            end
    end.

is_degrees(B, Max) ->
    case re:run(B, <<"^[+-]?[0-9]+(\\.[0-9]+)?$">>, [{capture, none}]) of
        match ->
            N = to_float(B),
            N >= -Max andalso N =< Max;
        nomatch ->
            false
    end.

to_float(B) ->
    case catch binary_to_float(B) of
        F when is_float(F) -> F;
        _ ->
            case catch binary_to_integer(string:trim(B, leading, "+")) of
                I when is_integer(I) -> float(I);
                _ -> 0.0
            end
    end.

%% A geo: URI (RFC 5870) alone: {ok, #{lat, lon, accuracy}} or error.
-spec geo_uri(binary()) -> {ok, map()} | error.
geo_uri(B) ->
    case re:run(B, <<"^geo:([+-]?[0-9]+(?:\\.[0-9]+)?),([+-]?[0-9]+(?:\\.[0-9]+)?)(?:,[+-]?[0-9]+(?:\\.[0-9]+)?)?((?:;[^;\\s]*)*)$">>,
                [caseless, {capture, all_but_first, binary}]) of
        {match, [Lat, Lon, Params]} ->
            case is_degrees(Lat, 90) andalso is_degrees(Lon, 180) of
                true ->
                    Acc = lists:foldl(
                            fun(P, A) ->
                                    case binary:split(P, <<"=">>) of
                                        [K, V] ->
                                            case string:lowercase(K) of
                                                <<"u">> -> V;
                                                _ -> A
                                            end;
                                        _ -> A
                                    end
                            end, <<>>, binary:split(Params, <<";">>, [global, trim_all])),
                    {ok, maps:from_list([{K, V} || {K, V} <- [{lat, Lat}, {lon, Lon}, {accuracy, Acc}],
                                                   V /= <<>>])};
                false ->
                    error
            end;
        _ ->
            error
    end.

poll(Els) ->
    case find(Els, <<"poll">>, ?NS_POLL) of
        #xmlel{children = Children} = P ->
            Options = [{attr(<<"emoji">>, O), string:trim(fxml:get_tag_cdata(O))}
                       || #xmlel{name = <<"option">>} = O <- Children],
            Options2 = [{E, L} || {E, L} <- Options, E /= <<>>, L /= <<>>],
            Title = cdata(P, <<"title">>),
            case length(Options2) >= 2 andalso length(Options2) =< 20 andalso Title /= <<>> of
                true ->
                    #{title => Title, description => cdata(P, <<"description">>),
                      options => Options2,
                      multiple => is_true(attr(<<"allow-multiple">>, P)),
                      hide => is_true(attr(<<"hide-results">>, P))};
                false ->
                    none
            end;
        none ->
            none
    end.

is_true(<<"true">>) -> true;
is_true(<<"1">>) -> true;
is_true(_) -> false.

poll_closed(Els) ->
    case find(Els, <<"poll-closed">>, ?NS_POLL) of
        #xmlel{} = C ->
            case attr(<<"message-id">>, C) of
                <<>> -> none;
                ID -> #{message_id => ID, title => cdata(C, <<"title">>)}
            end;
        none ->
            none
    end.

%% The Matrix user an xmpp: mention URI names, on the gateway Host:
%% xmpp:u%srv@<gateway> (a Matrix user), xmpp:<room>@<gateway>/<nick> (an
%% occupant of a Matrix room, whose nick is the MXID without its @),
%% xmpp:user@<Host> (a local user).
-spec mention_mxid(binary(), binary()) -> {ok, binary()} | error.
mention_mxid(URI, Host) ->
    mention_mxid(URI, Host, mod_matrix_gw_opt:host(Host), mod_matrix_gw_opt:matrix_domain(Host)).

-spec mention_mxid(binary(), binary(), binary(), binary()) -> {ok, binary()} | error.
mention_mxid(<<"xmpp:", Rest/binary>>, Host, Gateway, MatrixDomain) ->
    %% the JID as a client may write it, a Matrix JID's % unencoded
    [J | _] = binary:split(Rest, <<"?">>),
    case catch jid:decode(J) of
        #jid{luser = <<C, _/binary>>, lserver = Gateway, resource = Nick}
          when C == $!; C == $# ->
            nick_mxid(Nick);
        #jid{luser = U, lserver = Gateway} when U /= <<>> ->
            case binary:split(U, <<"%">>) of
                [EscU, EscS] when EscU /= <<>>, EscS /= <<>> ->
                    {ok, <<$@, (mod_matrix_gw_room:unescape(EscU))/binary, $:,
                           (mod_matrix_gw_room:unescape(EscS))/binary>>};
                _ ->
                    error
            end;
        #jid{luser = U, lserver = Host} when U /= <<>> ->
            {ok, <<$@, U/binary, $:, MatrixDomain/binary>>};
        _ ->
            error
    end;
mention_mxid(_, _, _, _) ->
    error.

nick_mxid(Nick) ->
    case binary:split(Nick, <<":">>) of
        [U, S] when U /= <<>>, S /= <<>> -> {ok, <<$@, Nick/binary>>};
        _ -> error
    end.

%% Rel with what names XMPP things resolved to Matrix ones; Resolve is a
%% map of funs: event (an XMPP message id -> event id | undefined), sender
%% (an event id -> its sender | undefined), mxid (a mention URI -> {ok,
%% MXID} | error). A reply or poll close whose target is unknown is left out
%% (the text still goes); the sender of the message replied to is mentioned
%% (the spec's m.mentions rule for replies).
-spec resolve_rel(map(), map()) -> map().
resolve_rel(Rel, Resolve) ->
    Event = maps:get(event, Resolve, fun(_) -> undefined end),
    Sender = maps:get(sender, Resolve, fun(_) -> undefined end),
    MXID = maps:get(mxid, Resolve, fun(_) -> error end),
    {Rel1, ReplyTo} =
        case maps:take(reply, Rel) of
            {{_To, ID}, R} ->
                case Event(ID) of
                    undefined -> {R, []};
                    EventID ->
                        {R#{reply => EventID},
                         [S || S <- [Sender(EventID)], is_binary(S)]}
                end;
            error ->
                {Rel, []}
        end,
    Rel2 =
        case lists:usort(ReplyTo ++ [M || U <- maps:get(mentions, Rel1, []),
                                          {ok, M} <- [MXID(U)]]) of
            [] -> maps:remove(mentions, Rel1);
            Ms -> Rel1#{mentions => Ms}
        end,
    case maps:take(poll_closed, Rel2) of
        {#{message_id := PID}, R3} ->
            case Event(PID) of
                undefined -> R3;
                PollEvent -> R3#{poll_end => PollEvent}
            end;
        error ->
            Rel2
    end.

%% The event a text with its resolved Rel is: {Type, Content}.
-spec outbound(binary(), map(), binary()) -> {binary(), map()}.
outbound(Text, #{poll := Poll}, XmppID) ->
    {?POLL_START, with_id(poll_start_content(Poll, Text), XmppID)};
outbound(Text, #{poll_end := PollEvent}, XmppID) ->
    {?POLL_END, with_id(#{<<"m.relates_to">> => #{<<"rel_type">> => <<"m.reference">>,
                                                  <<"event_id">> => PollEvent},
                          ?POLL_END => #{},
                          ?TEXT1767 => Text,
                          <<"body">> => Text}, XmppID)};
outbound(Text, Rel, XmppID) ->
    C0 = case Rel of
             #{geo := Geo} -> location_content(Geo, Text);
             _ -> #{<<"msgtype">> => <<"m.text">>, <<"body">> => Text}
         end,
    C1 = case Rel of
             #{reply := EventID} when is_binary(EventID) ->
                 C0#{<<"m.relates_to">> => #{<<"m.in_reply_to">> => #{<<"event_id">> => EventID}}};
             _ -> C0
         end,
    Mentions = maps:from_list(
                 [{<<"user_ids">>, Ms} || Ms <- [maps:get(mentions, Rel, [])], Ms /= []] ++
                 [{<<"room">>, true} || maps:get(room_mention, Rel, false)]),
    C2 = case map_size(Mentions) of
             0 -> C1;
             _ -> C1#{<<"m.mentions">> => Mentions}
         end,
    {<<"m.room.message">>, with_id(C2, XmppID)}.

with_id(Content, <<>>) -> Content;
with_id(Content, XmppID) -> Content#{?XMPP_ID => XmppID}.

location_content(#{uri := URI}, Text) ->
    location(URI, <<>>, Text);
location_content(#{lat := Lat, lon := Lon} = G, Text) ->
    URI = case maps:get(accuracy, G, <<>>) of
              <<>> -> <<"geo:", Lat/binary, $,, Lon/binary>>;
              Acc -> <<"geo:", Lat/binary, $,, Lon/binary, ";u=", Acc/binary>>
          end,
    Desc = case {maps:get(description, G, <<>>), maps:get(text, G, <<>>)} of
               {<<>>, T} -> T;
               {D, <<>>} -> D;
               {D, T} -> <<D/binary, ", ", T/binary>>
           end,
    location(URI, Desc, Text).

location(URI, Desc, Text) ->
    Body = case {string:trim(Text), Desc} of
               {<<>>, <<>>} -> <<"Location ", URI/binary>>;
               {<<>>, _} -> <<Desc/binary, $\n, URI/binary>>;
               {T, _} -> T
           end,
    Loc = case Desc of
              <<>> -> #{<<"uri">> => URI};
              _ -> #{<<"uri">> => URI, <<"description">> => Desc}
          end,
    #{<<"msgtype">> => <<"m.location">>, <<"body">> => Body, <<"geo_uri">> => URI,
      <<"org.matrix.msc3488.location">> => Loc}.

%% MSC3381 as Element sends it today (the unstable names); an answer's id
%% is its emoji, so a vote comes back as the reaction it was.
poll_start_content(#{title := Title, options := Options} = P, Text) ->
    Multiple = maps:get(multiple, P, false),
    #{?POLL_START =>
          #{<<"question">> => #{?TEXT1767 => Title, <<"body">> => Title},
            <<"kind">> => case maps:get(hide, P, false) of
                              true -> <<"org.matrix.msc3381.poll.undisclosed">>;
                              false -> <<"org.matrix.msc3381.poll.disclosed">>
                          end,
            <<"max_selections">> => case Multiple of
                                        true -> length(Options);
                                        false -> 1
                                    end,
            <<"answers">> => [#{<<"id">> => E, ?TEXT1767 => L} || {E, L} <- Options]},
      ?TEXT1767 => Text}.

%% A vote: the poll's answers among a reactor's keys. {Votes, Others}
%% where Votes are the answer ids (the emojis' answers), Others the keys
%% that are no answer. Options is poll_options/2's list.
-spec vote_split([binary()], [{binary(), binary(), binary()}]) -> {[binary()], [binary()]}.
vote_split(Keys, Options) ->
    Emojis = [E || {_, E, _} <- Options],
    {[Id || {Id, E, _} <- Options, lists:member(E, Keys)],
     [K || K <- Keys, not lists:member(K, Emojis)]}.

-spec response_content(binary(), [binary()]) -> map().
response_content(PollEvent, AnswerIDs) ->
    #{<<"m.relates_to">> => #{<<"rel_type">> => <<"m.reference">>, <<"event_id">> => PollEvent},
      ?POLL_RESPONSE => #{<<"answers">> => AnswerIDs}}.

%% m.sticker's content from the m.image media_content/3 made of a share,
%% its body the sticker's name (the share's <desc/>) when it has one.
-spec sticker_content(map(), binary()) -> map().
sticker_content(Media, Desc) ->
    C = maps:without([<<"msgtype">>, <<"filename">>], Media),
    case Desc of
        <<>> -> C;
        _ -> C#{<<"body">> => Desc}
    end.

%% The ids of the PDUs a /send answer refused (an error in its pdus).
-spec failed_pdus(binary()) -> [binary()].
failed_pdus(Body) ->
    try misc:json_decode(Body) of
        #{<<"pdus">> := PDUs} when is_map(PDUs) ->
            [ID || {ID, #{<<"error">> := _}} <- maps:to_list(PDUs)];
        _ -> []
    catch _:_ -> []
    end.

%% XEP-0184 <received/> and XEP-0333 <received/> of the message XmppID,
%% as Kinds (receipt, markable) asked.
-spec receipt_els(binary(), [receipt | markable]) -> [xmlel()].
receipt_els(XmppID, Kinds) ->
    [#xmlel{name = <<"received">>, attrs = [{<<"xmlns">>, NS}, {<<"id">>, XmppID}]}
     || {K, NS} <- [{receipt, <<"urn:xmpp:receipts">>}, {markable, <<"urn:xmpp:chat-markers:0">>}],
        lists:member(K, Kinds)].

%%--------------------------------------------------------------------
%% Matrix to XMPP
%%--------------------------------------------------------------------

%% The event an m.room.message answers: its m.in_reply_to, else a
%% thread's root (a thread message is a reply to it on the XMPP side).
-spec in_reply_to(map()) -> binary() | undefined.
in_reply_to(#{<<"m.relates_to">> := #{<<"m.in_reply_to">> := #{<<"event_id">> := E}}})
  when is_binary(E) -> E;
in_reply_to(#{<<"m.relates_to">> := #{<<"rel_type">> := <<"m.thread">>, <<"event_id">> := E}})
  when is_binary(E) -> E;
in_reply_to(_) -> undefined.

%% A body without the spec's reply fallback (the "> " lines that begin it
%% and the empty line after them), which spec v1.13 drops but older
%% clients still send.
-spec strip_matrix_reply(binary()) -> binary().
strip_matrix_reply(<<"> ", _/binary>> = Body) ->
    Lines = binary:split(Body, <<"\n">>, [global]),
    case lists:splitwith(fun(L) -> binary:first(<<L/binary, " ">>) == $> end, Lines) of
        {_Quote, [<<>> | Rest]} -> iolist_to_binary(lists:join(<<"\n">>, Rest));
        _ -> Body
    end;
strip_matrix_reply(Body) ->
    Body.

%% XEP-0461's quote of Body, as clients write it: "> " before each line.
-spec quote(binary()) -> binary().
quote(Body) ->
    iolist_to_binary([[<<"> ">>, L, <<"\n">>] || L <- binary:split(Body, <<"\n">>, [global])]).

-spec cp_len(binary()) -> non_neg_integer().
cp_len(B) ->
    case unicode:characters_to_list(B) of
        L when is_list(L) -> length(L);
        _ -> byte_size(B)
    end.

%% The body and elements an m.room.message with a body is on the XMPP
%% side, chat and groupchat alike: {Body2, Els}. Ctx is a map of funs (each
%% optional):
%%   reply_id    event id -> the id the XMPP side knows it by
%%   reply_to    event id -> the JID of its sender (<reply to/>), or <<>>
%%   target_body event id -> its body, for the quote, or undefined
%%   jid_of      MXID -> the bare JID that names it (mentions), or error
-spec inbound_els(map(), binary(), map()) -> {binary(), [xmlel()]}.
inbound_els(Content, Body, Ctx) ->
    {Body1, Els1, Shift, Own} =
        case in_reply_to(Content) of
            undefined ->
                {Body, [], 0, Body};
            Target ->
                Stripped = strip_matrix_reply(Body),
                ReplyID = (maps:get(reply_id, Ctx, fun(E) -> E end))(Target),
                To = (maps:get(reply_to, Ctx, fun(_) -> <<>> end))(Target),
                ReplyEl = #xmlel{name = <<"reply">>,
                                 attrs = [{<<"xmlns">>, ?NS_REPLY}, {<<"id">>, ReplyID} |
                                          [{<<"to">>, To} || To /= <<>>]]},
                case (maps:get(target_body, Ctx, fun(_) -> undefined end))(Target) of
                    TB when is_binary(TB), TB /= <<>> ->
                        Q = quote(TB),
                        N = cp_len(Q),
                        {<<Q/binary, Stripped/binary>>,
                         [ReplyEl,
                          #xmlel{name = <<"fallback">>,
                                 attrs = [{<<"xmlns">>, ?NS_FALLBACK}, {<<"for">>, ?NS_REPLY}],
                                 children = [#xmlel{name = <<"body">>,
                                                    attrs = [{<<"start">>, <<"0">>},
                                                             {<<"end">>, integer_to_binary(N)}]}]}],
                         N, Stripped};
                    _ ->
                        {Stripped, [ReplyEl], 0, Stripped}
                end
        end,
    %% a mention's name is looked for in the sender's own words, past the quote
    Els2 = mention_els(Content, Own, Shift, maps:get(jid_of, Ctx, fun(_) -> error end)),
    case Content of
        #{<<"msgtype">> := <<"m.location">>, <<"geo_uri">> := URI} when is_binary(URI) ->
            case geo_els(URI, Body1) of
                {ok, Body3, GeoEls} -> {Body3, Els1 ++ Els2 ++ GeoEls};
                error -> {Body1, Els1 ++ Els2}
            end;
        _ ->
            {Body1, Els1 ++ Els2}
    end.

%% XEP-0372 references for m.mentions' users, each at the first place the
%% body names it (the pill's text in formatted_body, the MXID, its local
%% part), in code points after Shift (the quote before the text); XEP-0513
%% #channel for m.mentions' room.
mention_els(#{<<"m.mentions">> := M} = Content, Body, Shift, JidOf) when is_map(M) ->
    Users = case maps:get(<<"user_ids">>, M, []) of
                L when is_list(L) -> lists:sublist([U || U <- L, is_binary(U)], ?MAX_MENTIONS);
                _ -> []
            end,
    Refs = lists:filtermap(
             fun(MXID) ->
                     case JidOf(MXID) of
                         J when is_binary(J), J /= <<>> ->
                             Span = case span(Body, names(Content, MXID)) of
                                        {B, E} -> [{<<"begin">>, integer_to_binary(B + Shift)},
                                                   {<<"end">>, integer_to_binary(E + Shift)}];
                                        none -> []
                                    end,
                             {true, #xmlel{name = <<"reference">>,
                                           attrs = [{<<"xmlns">>, ?NS_REFERENCE},
                                                    {<<"type">>, <<"mention">>},
                                                    {<<"uri">>, <<"xmpp:", J/binary>>} | Span]}};
                         _ ->
                             false
                     end
             end, Users),
    Room = case maps:get(<<"room">>, M, false) of
               true -> [#xmlel{name = <<"mention">>,
                               attrs = [{<<"xmlns">>, ?NS_MENTIONS},
                                        {<<"mentions">>, ?MENTION_CHANNEL}]}];
               _ -> []
           end,
    Refs ++ Room;
mention_els(_, _, _, _) ->
    [].

names(Content, <<$@, Rest/binary>> = MXID) ->
    Pill = case Content of
               #{<<"formatted_body">> := F} when is_binary(F) ->
                   case re:run(F, <<"<a href=\"https://matrix.to/#/",
                                    (re_quote(MXID))/binary, "\"[^>]*>([^<]+)</a>">>,
                               [{capture, all_but_first, binary}, unicode]) of
                       {match, [P]} -> [P];
                       _ -> []
                   end;
               _ -> []
           end,
    [Local | _] = binary:split(Rest, <<":">>),
    Pill ++ [MXID, Local];
names(_, MXID) ->
    [MXID].

re_quote(B) ->
    re:replace(B, <<"[.^$*+?()\\[\\]{}|\\\\]">>, <<"\\\\&">>, [global, {return, binary}]).

span(_Body, []) -> none;
span(Body, [<<>> | Names]) -> span(Body, Names);
span(Body, [Name | Names]) ->
    case binary:match(Body, Name) of
        {Pos, Len} ->
            B = cp_len(binary:part(Body, 0, Pos)),
            {B, B + cp_len(binary:part(Body, Pos, Len))};
        nomatch ->
            span(Body, Names)
    end.

%% m.location as XEP-0080: the body the geo: URI alone (what Conversations
%% reads as a place), the Matrix body its description.
-spec geo_els(binary(), binary()) -> {ok, binary(), [xmlel()]} | error.
geo_els(URI, Body) ->
    case geo_uri(URI) of
        {ok, #{lat := Lat, lon := Lon} = G} ->
            Desc = case string:trim(Body) of
                       URI -> <<>>;
                       <<"Location">> -> <<>>;
                       <<"Location ", URI/binary>> -> <<>>;
                       D -> D
                   end,
            Kids = [#xmlel{name = N, children = [{xmlcdata, V}]}
                    || {N, V} <- [{<<"accuracy">>, maps:get(accuracy, G, <<>>)},
                                  {<<"description">>, Desc},
                                  {<<"lat">>, Lat}, {<<"lon">>, Lon}],
                       V /= <<>>],
            {ok, URI,
             [#xmlel{name = <<"geoloc">>, attrs = [{<<"xmlns">>, ?NS_GEOLOC}], children = Kids},
              #xmlel{name = <<"fallback">>, attrs = [{<<"xmlns">>, ?NS_FALLBACK},
                                                     {<<"for">>, ?NS_GEOLOC}],
                     children = [#xmlel{name = <<"body">>}]}]};
        error ->
            error
    end.

%% An MSC3381 poll start (unstable or stable names): {ok, #{title,
%% description, answers [{Id, Label}], multiple, hide}} or error.
-spec poll_start(map()) -> {ok, map()} | error.
poll_start(#{?POLL_START := #{<<"answers">> := Answers} = P}) when is_list(Answers) ->
    poll_map(text1767(maps:get(<<"question">>, P, #{})),
             [{Id, text1767(A)} || #{<<"id">> := Id} = A <- Answers, is_binary(Id)],
             maps:get(<<"max_selections">>, P, 1),
             maps:get(<<"kind">>, P, <<>>) == <<"org.matrix.msc3381.poll.undisclosed">>);
poll_start(#{<<"m.poll">> := #{<<"answers">> := Answers} = P}) when is_list(Answers) ->
    poll_map(mtext(maps:get(<<"question">>, P, #{})),
             [{Id, mtext(A)} || #{<<"m.id">> := Id} = A <- Answers, is_binary(Id)],
             maps:get(<<"max_selections">>, P, 1),
             maps:get(<<"kind">>, P, <<>>) == <<"m.undisclosed">>);
poll_start(_) ->
    error.

poll_map(Title, Answers, Max, Hide) ->
    Answers2 = lists:sublist([{Id, L} || {Id, L} <- Answers, L /= <<>>], 20),
    case Title /= <<>> andalso length(Answers2) >= 2 of
        true -> {ok, #{title => Title, answers => Answers2,
                       multiple => is_integer(Max) andalso Max > 1, hide => Hide}};
        false -> error
    end.

text1767(#{?TEXT1767 := T}) when is_binary(T) -> T;
text1767(#{<<"body">> := T}) when is_binary(T) -> T;
text1767(M) -> mtext(M).

mtext(#{<<"m.text">> := [#{<<"body">> := T} | _]}) when is_binary(T) -> T;
mtext(#{<<"m.text">> := T}) when is_binary(T) -> T;
mtext(_) -> <<>>.

%% A poll response: {PollEventID, AnswerIDs} or error.
-spec poll_response(map()) -> {binary(), [binary()]} | error.
poll_response(#{<<"m.relates_to">> := #{<<"event_id">> := Target}} = C) when is_binary(Target) ->
    Answers = case C of
                  #{?POLL_RESPONSE := #{<<"answers">> := A}} when is_list(A) -> A;
                  #{<<"m.selections">> := A} when is_list(A) -> A;
                  _ -> undefined
              end,
    case Answers of
        undefined -> error;
        _ -> {Target, [X || X <- Answers, is_binary(X)]}
    end;
poll_response(_) ->
    error.

-spec poll_end_target(map()) -> binary() | error.
poll_end_target(#{<<"m.relates_to">> := #{<<"event_id">> := Target}}) when is_binary(Target) ->
    Target;
poll_end_target(_) ->
    error.

%% A poll's answers with their emojis: [{Id, Emoji, Label}]. A poll sent
%% from XMPP (Ours) has the emojis as its ids; another gets 1️⃣ .. 🔟 then
%% 🇦 .. 🇯 in order.
-spec poll_options(map(), boolean()) -> [{binary(), binary(), binary()}].
poll_options(#{answers := Answers}, true) ->
    [{Id, Id, L} || {Id, L} <- Answers];
poll_options(#{answers := Answers}, false) ->
    [{Id, E, L} || {{Id, L}, E} <- lists:zip(Answers, lists:sublist(emojis(), length(Answers)))].

emojis() ->
    [<<"1️⃣"/utf8>>, <<"2️⃣"/utf8>>, <<"3️⃣"/utf8>>, <<"4️⃣"/utf8>>, <<"5️⃣"/utf8>>,
     <<"6️⃣"/utf8>>, <<"7️⃣"/utf8>>, <<"8️⃣"/utf8>>, <<"9️⃣"/utf8>>, <<"🔟"/utf8>>]
        ++ [unicode:characters_to_binary([16#1F1E6 + I]) || I <- lists:seq(0, 9)].

%% A poll start as the XMPP side reads it: {Body, Els}, the body the
%% XEP-0428 fallback as Fluux writes it.
-spec poll_els(map(), [{binary(), binary(), binary()}], binary()) -> {binary(), [xmlel()]}.
poll_els(#{title := Title} = P, Options, Description) ->
    Body = iolist_to_binary(
             [<<"📊 Poll: "/utf8>>, Title,
              [[$\n, Description] || Description /= <<>>],
              [[$\n, E, $\s, L] || {_, E, L} <- Options]]),
    Attrs = [{<<"xmlns">>, ?NS_POLL}] ++
        [{<<"allow-multiple">>, <<"true">>} || maps:get(multiple, P, false)] ++
        [{<<"hide-results">>, <<"true">>} || maps:get(hide, P, false)],
    PollEl = #xmlel{name = <<"poll">>, attrs = Attrs,
                    children = [#xmlel{name = <<"title">>, children = [{xmlcdata, Title}]}] ++
                        [#xmlel{name = <<"description">>, children = [{xmlcdata, Description}]}
                         || Description /= <<>>] ++
                        [#xmlel{name = <<"option">>, attrs = [{<<"emoji">>, E}],
                                children = [{xmlcdata, L}]} || {_, E, L} <- Options]},
    {Body, [PollEl, poll_fallback(), mod_matrix_gw_xmpp:store_el()]}.

poll_fallback() ->
    #xmlel{name = <<"fallback">>, attrs = [{<<"xmlns">>, ?NS_FALLBACK}, {<<"for">>, ?NS_POLL}],
           children = [#xmlel{name = <<"body">>}]}.

%% A poll end as <poll-closed/>: the tally of the voters' reactions,
%% Votes [{Voter, Keys}] (the reactor's current keys on the poll); on a
%% single-answer poll a voter counts once, for the last of its answers in
%% option order (Fluux's rule). PollID is the id the XMPP side has for the
%% poll.
-spec poll_closed_els(map(), [{binary(), binary(), binary()}], [{binary(), [binary()]}], binary()) ->
          {binary(), [xmlel()]}.
poll_closed_els(#{title := Title} = P, Options, Votes, PollID) ->
    Multiple = maps:get(multiple, P, false),
    Chosen = [{Voter, case Multiple of
                          true -> Es;
                          false -> lists:sublist(lists:reverse(Es), 1)
                      end}
              || {Voter, Keys} <- Votes,
                 Es <- [[E || {_, E, _} <- Options, lists:member(E, Keys)]]],
    Tallies = [{E, L, [V || {V, Es} <- Chosen, lists:member(E, Es)]} || {_, E, L} <- Options],
    Body = iolist_to_binary(
             [<<"📊 Poll closed: "/utf8>>, Title,
              [[$\n, E, $\s, L, <<": ">>, integer_to_binary(length(Vs))] || {E, L, Vs} <- Tallies]]),
    El = #xmlel{name = <<"poll-closed">>,
                attrs = [{<<"xmlns">>, ?NS_POLL}, {<<"message-id">>, PollID}],
                children = [#xmlel{name = <<"title">>, children = [{xmlcdata, Title}]}] ++
                    [#xmlel{name = <<"tally">>,
                            attrs = [{<<"emoji">>, E}, {<<"label">>, L},
                                     {<<"count">>, integer_to_binary(length(Vs))}] ++
                                [{<<"voters">>, iolist_to_binary(lists:join(<<",">>, Vs))} || Vs /= []]}
                     || {E, L, Vs} <- Tallies]},
    {Body, [El, poll_fallback(), mod_matrix_gw_xmpp:store_el()]}.

%% What makes a share from Matrix a XEP-0449 sticker: <sticker/> (no pack:
%% Matrix has none to name) and the XEP-0428 fallbacks that say the body
%% (the file's URL) stands in for the share and the sticker.
-spec sticker_els() -> [xmlel()].
sticker_els() ->
    [#xmlel{name = <<"sticker">>, attrs = [{<<"xmlns">>, ?NS_STICKERS}]} |
     [#xmlel{name = <<"fallback">>, attrs = [{<<"xmlns">>, ?NS_FALLBACK}, {<<"for">>, F}],
             children = [#xmlel{name = <<"body">>}]} || F <- [?NS_SFS, ?NS_STICKERS]]].

%% XEP-0172's <nick/>.
-spec nick_el(binary()) -> xmlel().
nick_el(Name) ->
    #xmlel{name = <<"nick">>, attrs = [{<<"xmlns">>, ?NS_NICK_EL}], children = [{xmlcdata, Name}]}.
