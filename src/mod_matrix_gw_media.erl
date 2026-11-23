%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_media.erl
%%% Author  : Paavel A. <pd@narayana.im>
%%% Purpose : Files, images, audio and video between XMPP and Matrix
%%% Created : 25 Oct 2026 by Paavel A. <pd@narayana.im>
%%%
%%% The gateway is a courier for bytes: it keeps metadata only, moves files
%%% between the server's XEP-0363 store (upload.<domain>, mod_s3_upload's
%%% bucket) and the federation, and never holds a key.
%%%
%%%   our media      a file a local user put in the store is named
%%%                  mxc://<matrix_domain>/<id> (mint/3, the table
%%%                  matrix_media); the federation's GET
%%%                  /federation/v1/media/download/<id> answers a multipart
%%%                  whose second part is a Location to the store, the
%%%                  legacy /media/v3/download/<server>/<id> a 307 there
%%%                  (allow_redirect=true) or the bytes (process/2)
%%%   remote media   mxc://<other>/<id> is fetched once over the federation
%%%                  (authenticated media, the legacy endpoint as the
%%%                  fallback), bounded by media_max_bytes, PUT into the
%%%                  store through a slot the gateway asks upload.<domain>
%%%                  for, and the store's URL kept (matrix_media_remote), so
%%%                  every later asker gets that link (resolve/3); a
%%%                  Location or redirect the remote server names is
%%%                  followed only to a public https address
%%%                  (mod_matrix_gw_http:safe_url/2) or into the store,
%%%                  at most two hops; at most ?MAX_FETCHES fetches at once
%%%                  (fewer when media_max_bytes is large: ?MEM_BUDGET over
%%%                  it), each within ?FETCH_DEADLINE in all; the table
%%%                  bounded by media_max_entries as matrix_media is
%%%   the wire       <media-id/> and <media-fetch/> in https://ns.another.im/matrix-e2e/0
%%%                  to the gateway's JID, for the local users (route_iq/1)
%%%   plain chats    an XMPP share (XEP-0066 OOB, XEP-0447 with XEP-0446
%%%                  metadata) of a file in the store is an m.image, m.video,
%%%                  m.audio or m.file (share/1, outbound_contents/5); such
%%%                  an event from Matrix is a share of the re-hosted copy
%%%                  (route_share/3), delivered by a process of its own,
%%%                  never by the room's
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
-module(mod_matrix_gw_media).

-export([create_db/0, upload_url/1, under/2, mint/3, parse_mxc/1, resolve/3,
         fetch/4, fetch/5, process/2, route_iq/1, share/1, outbound_contents/5,
         media_content/3, route_share/3, share_message/3, share_els/1,
         failure_body/2, file_name/1, event_el/6, multipart/2,
         parse_multipart/2, upload/4, plain_get/3, plain_get/4, with_slot/3]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").

-define(NS_E2E, <<"https://ns.another.im/matrix-e2e/0">>).
-define(NS_OOB_X, <<"jabber:x:oob">>).
-define(NS_SFS, <<"urn:xmpp:sfs:0">>).
-define(NS_FILE_META, <<"urn:xmpp:file:metadata:0">>).
-define(NS_URL_DATA, <<"http://jabber.org/protocol/url-data">>).
-define(FETCH_TIMEOUT, 120000).
%% A fetch of a remote file in all (federation, Location, redirect), from
%% the moment it has a slot; the time a fetch waits for one.
-define(FETCH_DEADLINE, 300000).
-define(SLOT_WAIT, 120000).
%% Fetches at once, and the bytes they may hold in memory together (each
%% may hold media_max_bytes):.
-define(MAX_FETCHES, 4).
-define(MEM_BUDGET, 268435456).
-define(SLOTS, mod_matrix_gw_media_slots).
%% Redirects followed after the first Location or 3xx.
-define(MAX_HOPS, 1).
%% the legacy endpoint's proxy: 8 MiB at most, a
%% slot waited for 10 s at most
-define(PROXY_MAX, 8388608).
-define(PROXY_WAIT, 10000).
-define(SLOT_TIMEOUT, 35000).

-record(matrix_media,
        {id :: binary(),
         url :: binary(),
         name :: binary(),
         mime :: binary(),
         size :: non_neg_integer() | undefined,
         created :: integer()}).

-record(matrix_media_remote,
        {key :: {binary(), binary()},
         url :: binary(),
         name :: binary(),
         mime :: binary(),
         size :: non_neg_integer(),
         fetched :: integer()}).

%% What is known of a file: url (the store's), name, mime, size, and from
%% an event's info or a share's metadata width, height, duration (ms).
-type info() :: #{atom() => binary() | integer()}.

create_db() ->
    ejabberd_mnesia:create(
      ?MODULE, matrix_media,
      [{disc_copies, [node()]},
       {type, set},
       {index, [url]},
       {attributes, record_info(fields, matrix_media)}]),
    ejabberd_mnesia:create(
      ?MODULE, matrix_media_remote,
      [{disc_copies, [node()]},
       {type, set},
       {attributes, record_info(fields, matrix_media_remote)}]),
    %% the fetch slots: owned by the caller, mod_matrix_gw's process
    case ets:info(?SLOTS) of
        undefined -> ets:new(?SLOTS, [named_table, public, set]);
        _ -> ok
    end,
    ok.

%%--------------------------------------------------------------------
%% The store
%%--------------------------------------------------------------------

%% The store's base URL: the upload_url option, else where mod_s3_upload's
%% objects are read (its download_url, else its bucket_url); undefined when
%% neither is set, and then no file crosses.
-spec upload_url(binary()) -> binary() | undefined.
upload_url(Host) ->
    case mod_matrix_gw_opt:upload_url(Host) of
        undefined ->
            try gen_mod:get_module_opt(Host, mod_s3_upload, download_url) of
                undefined -> gen_mod:get_module_opt(Host, mod_s3_upload, bucket_url);
                Download -> Download
            catch _:_ -> undefined
            end;
        URL ->
            URL
    end.

%% Whether URL is an object under the store's base Base.
-spec under(binary(), binary() | undefined) -> boolean().
under(_URL, undefined) ->
    false;
under(URL, Base0) ->
    Base = case binary:last(Base0) of
               $/ -> Base0;
               _ -> <<Base0/binary, $/>>
           end,
    N = byte_size(Base),
    case URL of
        <<Base:N/binary, Path/binary>> when Path /= <<>> ->
            binary:match(Path, [<<"/../">>, <<"/./">>, <<"#">>]) == nomatch
                andalso binary:part(Path, 0, min(3, byte_size(Path))) /= <<"../">>
                andalso Path /= <<"..">>;
        _ ->
            false
    end.

%% The upload service the gateway asks for a slot: mod_s3_upload's first
%% host, else upload.<Host>.
upload_service(Host) ->
    try gen_mod:get_module_opt(Host, mod_s3_upload, hosts) of
        [H | _] -> misc:expand_keyword(<<"@HOST@">>, H, Host);
        _ -> <<"upload.", Host/binary>>
    catch _:_ -> <<"upload.", Host/binary>>
    end.

%%--------------------------------------------------------------------
%% Our media: mxc://<matrix_domain>/<id>
%%--------------------------------------------------------------------

%% The mxc of a file in the store, the same one for the same URL; a new id
%% is 22 characters of URL-safe base64 from 16 random bytes. The table
%% holds media_max_entries, the oldest dropped.
-spec mint(binary(), binary(), info()) -> {ok, binary()} | {error, not_allowed}.
mint(Host, URL, Meta) ->
    case under(URL, upload_url(Host)) of
        false ->
            {error, not_allowed};
        true ->
            New = #matrix_media{id = base64:encode(crypto:strong_rand_bytes(16),
                                                   #{mode => urlsafe, padding => false}),
                                url = URL,
                                name = maps:get(name, Meta, url_name(URL)),
                                mime = maps:get(mime, Meta, mime_of(maps:get(name, Meta, url_name(URL)))),
                                size = maps:get(size, Meta, undefined),
                                created = erlang:system_time(millisecond)},
            {atomic, ID} =
                mnesia:transaction(
                  fun() ->
                          case mnesia:index_read(matrix_media, URL, #matrix_media.url) of
                              [#matrix_media{id = Old} | _] ->
                                  Old;
                              [] ->
                                  mnesia:write(New),
                                  New#matrix_media.id
                          end
                  end),
            bound(matrix_media, #matrix_media.created, mod_matrix_gw_opt:media_max_entries(Host)),
            {ok, mxc(mod_matrix_gw_opt:matrix_domain(Host), ID)}
    end.

mxc(Server, ID) ->
    <<"mxc://", Server/binary, $/, ID/binary>>.

%% Keeps Table at Max entries: above it, the oldest tenth goes (at least
%% what is over), so the scan is not made at every insert.
bound(Table, Pos, Max) ->
    Size = mnesia:table_info(Table, size),
    case Size > Max of
        false ->
            ok;
        true ->
            Ages = mnesia:dirty_select(Table, [{'$1', [], [{{{element, Pos, '$1'},
                                                             {element, 2, '$1'}}}]}]),
            Drop = lists:sublist(lists:sort(Ages), max(Size - Max, Max div 10)),
            lists:foreach(fun({_, Key}) -> mnesia:dirty_delete(Table, Key) end, Drop)
    end.

%% {Server, MediaID} of an mxc URI, or error.
-spec parse_mxc(binary()) -> {binary(), binary()} | error.
parse_mxc(<<"mxc://", Rest/binary>>) ->
    case binary:split(Rest, <<"/">>) of
        [Server, ID] when Server /= <<>>, ID /= <<>> ->
            case re:run(ID, <<"^[A-Za-z0-9_-]+$">>, [{capture, none}]) of
                match -> {Server, ID};
                nomatch -> error
            end;
        _ ->
            error
    end;
parse_mxc(_) ->
    error.

info(#matrix_media{url = URL, name = Name, mime = Mime, size = Size}) ->
    with_size(#{url => URL, name => Name, mime => Mime}, Size);
info(#matrix_media_remote{url = URL, name = Name, mime = Mime, size = Size}) ->
    #{url => URL, name => Name, mime => Mime, size => Size}.

with_size(Info, Size) when is_integer(Size) -> Info#{size => Size};
with_size(Info, _) -> Info.

%%--------------------------------------------------------------------
%% Remote media
%%--------------------------------------------------------------------

%% The store's copy of the file an mxc names: ours from the table, a remote
%% one fetched once. Hints (name, mime, size) are what the asker knows of
%% it. Errors: not_found, too_large, timeout, unreachable, upload.
-spec resolve(binary(), binary(), info()) -> {ok, info()} | {error, atom()}.
resolve(Host, MXC, Hints) ->
    Local = mod_matrix_gw_opt:matrix_domain(Host),
    case parse_mxc(MXC) of
        {Local, ID} ->
            case mnesia:dirty_read(matrix_media, ID) of
                [M] -> {ok, info(M)};
                [] -> {error, not_found}
            end;
        {Origin, ID} ->
            Key = {Origin, ID},
            case cached(Key) of
                {ok, Info} ->
                    {ok, Info};
                error ->
                    %% one fetch per file: whoever comes second waits and
                    %% finds the first one's copy
                    global:trans({{?MODULE, Key}, self()},
                                 fun() ->
                                         case cached(Key) of
                                             {ok, Info} -> {ok, Info};
                                             error -> fetch(Host, Origin, ID, Hints)
                                         end
                                 end, [node()])
            end;
        error ->
            {error, not_found}
    end.

cached(Key) ->
    case mnesia:dirty_read(matrix_media_remote, Key) of
        [R] -> {ok, info(R)};
        [] -> error
    end.

%% Fetches mxc://Origin/ID over the federation and puts it into the store,
%% in a slot (with_slot/3) and within ?FETCH_DEADLINE.
-spec fetch(binary(), binary(), binary(), info()) -> {ok, info()} | {error, atom()}.
fetch(Host, Origin, ID, Hints) ->
    Max = mod_matrix_gw_opt:media_max_bytes(Host),
    Slots = max(1, min(?MAX_FETCHES, ?MEM_BUDGET div max(1, Max))),
    with_slot(Slots, ?SLOT_WAIT,
              fun() ->
                      Deadline = erlang:monotonic_time(millisecond) + ?FETCH_DEADLINE,
                      Http = #{federation => fun(Path, Query) ->
                                                     federation_get(Host, Origin, Path, Query, Max, Deadline)
                                             end,
                               get => fun(URL) -> plain_get(Host, URL, Max, Deadline) end,
                               store => upload_url(Host),
                               upload => fun(Name, Mime, Bytes) -> upload(Host, Name, Mime, Bytes) end},
                      Res = fetch(Origin, ID, Hints, Max, Http),
                      bound(matrix_media_remote, #matrix_media_remote.fetched,
                            mod_matrix_gw_opt:media_max_entries(Host)),
                      Res
              end).

%% Fun run in one of Slots slots shared by every fetch of the node, waited
%% for up to Wait ms; {error, busy} when none came free.
-spec with_slot(pos_integer(), non_neg_integer(), fun(() -> T)) -> T | {error, busy}.
with_slot(Slots, Wait, Fun) ->
    case acquire(Slots, erlang:monotonic_time(millisecond) + Wait) of
        ok ->
            try Fun()
            after ets:update_counter(?SLOTS, fetching, {2, -1}, {fetching, 1})
            end;
        busy ->
            ?INFO_MSG("Matrix media: all ~B fetch slots busy for ~B s, a file not fetched",
                      [Slots, Wait div 1000]),
            {error, busy}
    end.

acquire(Slots, Until) ->
    case ets:update_counter(?SLOTS, fetching, {2, 1}, {fetching, 0}) of
        N when N =< Slots ->
            ok;
        _ ->
            ets:update_counter(?SLOTS, fetching, {2, -1}),
            case erlang:monotonic_time(millisecond) >= Until of
                true ->
                    busy;
                false ->
                    timer:sleep(100 + rand:uniform(100)),
                    acquire(Slots, Until)
            end
    end.

fetch(Origin, ID, Hints, Max, Http) ->
    Download = maps:get(federation, Http),
    Got =
        case Download([<<"_matrix">>, <<"federation">>, <<"v1">>, <<"media">>,
                       <<"download">>, ID], []) of
            {ok, 200, Headers, Body} ->
                from_multipart(Headers, Body, Http);
            {ok, Status, _, Body} = Res ->
                case errcode(Body) of
                    <<"M_UNRECOGNIZED">> ->
                        %% a server older than v1.11: the unauthenticated
                        %% endpoint
                        legacy(Download([<<"_matrix">>, <<"media">>, <<"v3">>, <<"download">>,
                                         Origin, ID],
                                        [{<<"allow_remote">>, <<"false">>}]), Http);
                    _ ->
                        ?DEBUG("media ~s/~s: ~p", [Origin, ID, Res]),
                        {error, status_error(Status, Body)}
                end;
            {error, Reason} ->
                {error, Reason}
        end,
    case Got of
        {ok, Bytes, PartHeaders} when byte_size(Bytes) =< Max ->
            Name = case maps:get(name, Hints, <<>>) of
                       <<>> -> disposition_name(PartHeaders, ID);
                       N -> N
                   end,
            Mime = case maps:get(mime, Hints, <<>>) of
                       <<>> -> content_type(PartHeaders, Name);
                       T -> T
                   end,
            case (maps:get(upload, Http))(Name, Mime, Bytes) of
                {ok, URL} ->
                    R = #matrix_media_remote{key = {Origin, ID}, url = URL, name = Name,
                                             mime = Mime, size = byte_size(Bytes),
                                             fetched = erlang:system_time(millisecond)},
                    mnesia:dirty_write(R),
                    {ok, info(R)};
                {error, _} = Err ->
                    Err
            end;
        {ok, _, _} ->
            {error, too_large};
        {error, Reason2} ->
            {error, error_reason(Reason2)}
    end.

legacy({ok, 200, Headers, Body}, _Http) ->
    {ok, Body, Headers};
legacy({ok, Status, Headers, _}, Http) when Status >= 300, Status < 400 ->
    follow(header(<<"location">>, Headers), Http, undefined, ?MAX_HOPS);
legacy({ok, Status, _, Body}, _Http) ->
    {error, status_error(Status, Body)};
legacy({error, _} = Err, _Http) ->
    Err.

%% The spec's answer: a JSON part, then the bytes or a Location (followed
%% once).
from_multipart(Headers, Body, Http) ->
    case parse_multipart(header(<<"content-type">>, Headers), Body) of
        {ok, [_JSON, {PartHeaders, Bytes} | _]} ->
            case header(<<"location">>, PartHeaders) of
                undefined -> {ok, Bytes, PartHeaders};
                Location -> follow(Location, Http, undefined, ?MAX_HOPS)
            end;
        _ ->
            {error, bad_response}
    end.

%% A Location (or redirect) a remote server named, relative to Base: GET
%% only when it is in the store or at a public https address
%% (allowed/2), and a redirect from there followed the same
%% way Hops more times.
follow(undefined, _Http, _Base, _Hops) ->
    {error, bad_response};
follow(Location0, Http, Base, Hops) ->
    Location = case Base of
                   undefined -> Location0;
                   _ -> try uri_string:resolve(Location0, Base) of
                            L when is_binary(L) -> L;
                            _ -> Location0
                        catch _:_ -> Location0
                        end
               end,
    case allowed(Location, Http) of
        ok ->
            case (maps:get(get, Http))(Location) of
                {ok, 200, Headers, Bytes} ->
                    {ok, Bytes, Headers};
                {ok, Status, Headers, _} when Status >= 300, Status < 400, Hops > 0 ->
                    follow(header(<<"location">>, Headers), Http, Location, Hops - 1);
                {ok, Status, _, Body} ->
                    {error, status_error(Status, Body)};
                {error, _} = Err ->
                    Err
            end;
        {error, Why} ->
            refused(Location, Why)
    end.

%% Whether a URL a remote server named may be fetched: in the store
%% (the Http map's `store`), or https to public addresses only (resolved
%% by the map's `resolve`, the system's resolver by default).
allowed(URL, Http) ->
    case under(URL, maps:get(store, Http, undefined)) of
        true ->
            ok;
        false ->
            Checked = case maps:find(resolve, Http) of
                          {ok, Resolve} -> mod_matrix_gw_http:safe_url(URL, Resolve);
                          error -> mod_matrix_gw_http:safe_url(URL)
                      end,
            case Checked of
                {ok, _} -> ok;
                {error, _} = Err -> Err
            end
    end.

refused(URL, Why) ->
    Where = case catch uri_string:parse(URL) of
                #{scheme := S, host := H} -> <<S/binary, "://", H/binary>>;
                _ -> <<"a malformed URL">>
            end,
    ?WARNING_MSG("Matrix media: refused to fetch from ~ts, which a remote server named: ~p",
                 [Where, Why]),
    {error, refused}.

errcode(Body) ->
    try misc:json_decode(Body) of
        #{<<"errcode">> := E} when is_binary(E) -> E;
        _ -> <<>>
    catch _:_ -> <<>>
    end.

status_error(Status, Body) ->
    case {Status, errcode(Body)} of
        {_, <<"M_TOO_LARGE">>} -> too_large;
        {413, _} -> too_large;
        {_, <<"M_NOT_FOUND">>} -> not_found;
        {404, _} -> not_found;
        {504, _} -> timeout;
        _ -> unreachable
    end.

error_reason(too_large) -> too_large;
error_reason(not_found) -> not_found;
error_reason(timeout) -> timeout;
error_reason(upload) -> upload;
error_reason(refused) -> refused;
error_reason(busy) -> busy;
error_reason({failed_connect, _}) -> unreachable;
error_reason(_) -> unreachable.

%% A response's header (names in lower case), or undefined.
header(Name, Headers) ->
    case [V || {K, V} <- Headers, string:lowercase(iolist_to_binary(K)) == Name] of
        [V | _] -> iolist_to_binary(V);
        [] -> undefined
    end.

content_type(Headers, Name) ->
    case header(<<"content-type">>, Headers) of
        undefined -> mime_of(Name);
        T -> hd(binary:split(T, <<";">>))
    end.

disposition_name(Headers, Default) ->
    case header(<<"content-disposition">>, Headers) of
        undefined ->
            Default;
        D ->
            case re:run(D, <<"filename\\*=[Uu][Tt][Ff]-8''([^;\\s]+)">>, [{capture, all_but_first, binary}]) of
                {match, [Enc]} ->
                    percent_decode(Enc, Default);
                nomatch ->
                    case re:run(D, <<"filename=\"?([^\";]+)\"?">>, [{capture, all_but_first, binary}]) of
                        {match, [N]} -> N;
                        nomatch -> Default
                    end
            end
    end.

%% A signed GET to the remote server (mod_matrix_gw's discovery and
%% verified TLS), its body bounded.
federation_get(Host, Origin, Path, Query, Max, Deadline) ->
    case mod_matrix_gw:send_request(Host, get, Origin, Path, Query, none,
                                    [{connect_timeout, 10000}, {timeout, ?FETCH_TIMEOUT},
                                     {autoredirect, false}],
                                    [{sync, false}, {stream, self}, {body_format, binary}]) of
        {ok, ReqID} -> collect(ReqID, Max, Deadline);
        {error, _} = Err -> Err
    end.

%% A GET of a URL, the body bounded, within FETCH_TIMEOUT. A URL in the
%% store (the upload_url the operator set) is fetched as it is, its
%% certificate verified when https; any other only when
%% mod_matrix_gw_http:safe_url/1 allows it (https, every address public),
%% then from the address it checked (no second resolution), the name in
%% the Host header and the certificate.
plain_get(Host, URL, Max) ->
    plain_get(Host, URL, Max, erlang:monotonic_time(millisecond) + ?FETCH_TIMEOUT).

plain_get(Host, URL, Max, Deadline) ->
    Target =
        case under(URL, upload_url(Host)) of
            true ->
                case uri_string:parse(URL) of
                    #{scheme := <<"https">>, host := H} ->
                        {ok, URL, [], [{ssl, mod_matrix_gw_http:tls_options(H, mod_matrix_gw_s2s:cacerts(Host))}]};
                    _ ->
                        {ok, URL, [], []}
                end;
            false ->
                case mod_matrix_gw_http:safe_url(URL) of
                    {ok, #{url := IPURL, name := Name, host_header := HH}} ->
                        {ok, IPURL, [{"host", HH}],
                         [{ssl, mod_matrix_gw_http:tls_options(Name, mod_matrix_gw_s2s:cacerts(Host))}]};
                    {error, Why} ->
                        refused(URL, Why)
                end
        end,
    case Target of
        {ok, Get, Headers, TLS} ->
            case catch httpc:request(get, {Get, Headers},
                                     [{connect_timeout, 10000}, {timeout, ?FETCH_TIMEOUT},
                                      {autoredirect, false} | TLS],
                                     [{sync, false}, {stream, self}, {body_format, binary}]) of
                {ok, ReqID} -> collect(ReqID, Max, Deadline);
                {error, _} = Err -> Err;
                Other -> {error, Other}
            end;
        {error, _} = Err ->
            Err
    end.

%% httpc's streamed answer, read up to Max bytes before Deadline (a
%% monotonic time in ms, the whole answer's, not a chunk's):
%% {ok, Status, Headers, Body} with the headers as binaries, or
%% {error, too_large | timeout | Reason}.
collect(ReqID, Max, Deadline) ->
    collect(ReqID, Max, Deadline, [], [], 0).

collect(ReqID, Max, Deadline, Headers, Acc, Size) ->
    receive
        {http, {ReqID, stream_start, Hs}} ->
            Hs2 = [{iolist_to_binary(K), iolist_to_binary(V)} || {K, V} <- Hs],
            case catch binary_to_integer(header(<<"content-length">>, Hs2)) of
                N when is_integer(N), N > Max ->
                    httpc:cancel_request(ReqID),
                    {error, too_large};
                _ ->
                    collect(ReqID, Max, Deadline, Hs2, Acc, Size)
            end;
        {http, {ReqID, stream, Bin}} ->
            case Size + byte_size(Bin) of
                S when S > Max ->
                    httpc:cancel_request(ReqID),
                    {error, too_large};
                S ->
                    collect(ReqID, Max, Deadline, Headers, [Bin | Acc], S)
            end;
        {http, {ReqID, stream_end, _}} ->
            {ok, 200, Headers, iolist_to_binary(lists:reverse(Acc))};
        {http, {ReqID, {{_, Status, _}, Hs, Body}}} ->
            {ok, Status, [{iolist_to_binary(K), iolist_to_binary(V)} || {K, V} <- Hs],
             iolist_to_binary(Body)};
        {http, {ReqID, {error, Reason}}} ->
            {error, Reason}
    after max(0, Deadline - erlang:monotonic_time(millisecond)) ->
            httpc:cancel_request(ReqID),
            {error, timeout}
    end.

%% Puts Bytes into the store: a slot from the upload service, asked from
%% the gateway's JID, and the PUT. The GET URL, or {error, upload}.
-spec upload(binary(), binary(), binary(), binary()) -> {ok, binary()} | {error, upload}.
upload(Host, Name, Mime, Bytes) ->
    Self = self(),
    Ref = make_ref(),
    IQ = #iq{type = get, id = p1_rand:get_string(),
             from = jid:make(mod_matrix_gw_opt:host(Host)),
             to = jid:make(upload_service(Host)),
             sub_els = [#upload_request_0{filename = Name, size = byte_size(Bytes),
                                          'content-type' = Mime, xmlns = ?NS_HTTP_UPLOAD_0}]},
    ejabberd_router:route_iq(IQ, fun(Reply) -> Self ! {Ref, Reply} end),
    receive
        {Ref, #iq{type = result} = Reply} ->
            case catch xmpp:decode_els(Reply) of
                #iq{sub_els = [#upload_slot_0{put = Put, get = Get}]} when is_binary(Put) ->
                    put_object(Host, Put, Mime, Bytes, Get);
                _ ->
                    ?INFO_MSG("no upload slot for a Matrix file: ~p", [Reply]),
                    {error, upload}
            end;
        {Ref, Reply} ->
            ?INFO_MSG("no upload slot for a Matrix file: ~p", [Reply]),
            {error, upload}
    after ?SLOT_TIMEOUT ->
            {error, upload}
    end.

put_object(Host, Put, Mime, Bytes, Get) ->
    TLS = case uri_string:parse(Put) of
              #{scheme := <<"https">>, host := H} ->
                  [{ssl, mod_matrix_gw_http:tls_options(H, mod_matrix_gw_s2s:cacerts(Host))}];
              _ ->
                  []
          end,
    case catch httpc:request(put, {Put, [], binary_to_list(Mime), Bytes},
                             [{connect_timeout, 10000}, {timeout, ?FETCH_TIMEOUT} | TLS],
                             [{body_format, binary}]) of
        {ok, {{_, Status, _}, _, _}} when Status >= 200, Status < 300 ->
            {ok, Get};
        Res ->
            ?INFO_MSG("the store refused a Matrix file: ~p", [Res]),
            {error, upload}
    end.

%%--------------------------------------------------------------------
%% Serving our media
%%--------------------------------------------------------------------

%% The media endpoints under /_matrix: the federation's (the caller has
%% checked the request's signature) and the legacy unauthenticated ones,
%% for our own server name only.
process([<<"federation">>, <<"v1">>, <<"media">>, Kind, ID | _],
        #request{method = 'GET'}) when Kind == <<"download">>; Kind == <<"thumbnail">> ->
    case own(ID, Kind) of
        {ok, #matrix_media{url = URL}} ->
            Boundary = hex(crypto:strong_rand_bytes(16)),
            {200, [{<<"Content-Type">>, <<"multipart/mixed; boundary=", Boundary/binary>>}],
             multipart(Boundary, [{[{<<"Content-Type">>, <<"application/json">>}], <<"{}">>},
                                  {[{<<"Location">>, URL}], <<>>}])};
        error ->
            not_found()
    end;
process([<<"media">>, _Version, Kind, Server, ID | _],
        #request{method = 'GET', host = _} = Request)
  when Kind == <<"download">>; Kind == <<"thumbnail">> ->
    Host = ejabberd_config:get_myname(),
    case mod_matrix_gw_opt:matrix_domain(Host) of
        Server ->
            case own(ID, Kind) of
                {ok, #matrix_media{url = URL} = M} ->
                    case proplists:get_value(<<"allow_redirect">>, Request#request.q) of
                        <<"true">> ->
                            {307, [{<<"Location">>, URL}], <<>>};
                        _ ->
                            proxy(Host, M)
                    end;
                error ->
                    not_found()
            end;
        _ ->
            %% the gateway proxies nobody else's media
            not_found()
    end;
process(_Path, _Request) ->
    mod_matrix_gw_http:error(404, <<"M_UNRECOGNIZED">>, <<"Unrecognized request">>).

not_found() ->
    mod_matrix_gw_http:error(404, <<"M_NOT_FOUND">>, <<"no such media">>).

%% Our file of that id; for a thumbnail, an image (the original stands in).
own(ID, Kind) ->
    case mnesia:dirty_read(matrix_media, ID) of
        [#matrix_media{mime = <<"image/", _/binary>>} = M] -> {ok, M};
        [M] when Kind == <<"download">> -> {ok, M};
        _ -> error
    end.

%% The object's bytes, read from the store. The gateway: the
%% unauthenticated endpoint holds a file in memory only in one of the
%% fetches' slots (with_slot/3, the same pool), waited for ?PROXY_WAIT at
%% most (503 M_LIMIT_EXCEEDED then), and only up to ?PROXY_MAX bytes (and
%% media_max_bytes): a larger file is 502 M_TOO_LARGE, never read; the
%% client that may follow a redirect (allow_redirect=true) gets the store's
%% URL for any size.
proxy(Host, #matrix_media{url = URL, name = Name, mime = Mime, size = Size}) ->
    MaxBytes = mod_matrix_gw_opt:media_max_bytes(Host),
    Max = min(MaxBytes, mod_matrix_gw_s2s:limit(proxy_max, ?PROXY_MAX)),
    Slots = max(1, min(?MAX_FETCHES, ?MEM_BUDGET div max(1, MaxBytes))),
    Res = case Size of
              N when is_integer(N), N > Max ->
                  {error, too_large};
              _ ->
                  with_slot(Slots, mod_matrix_gw_s2s:limit(proxy_wait, ?PROXY_WAIT),
                            fun() -> plain_get(Host, URL, Max) end)
          end,
    case Res of
        {ok, 200, _, Bytes} ->
            {200, [{<<"Content-Type">>, Mime},
                   {<<"Content-Disposition">>, disposition(Name)}], Bytes};
        {error, too_large} ->
            mod_matrix_gw_http:error(502, <<"M_TOO_LARGE">>, <<"the file is too large to proxy">>);
        {error, busy} ->
            mod_matrix_gw_http:error(503, <<"M_LIMIT_EXCEEDED">>, <<"too many files being read, try again">>);
        _ ->
            ?DEBUG("store answered ~p for ~s", [Res, URL]),
            mod_matrix_gw_http:error(502, <<"M_UNKNOWN">>, <<"the store did not answer">>)
    end.

disposition(Name) ->
    <<"inline; filename*=utf-8''", (uri_string:quote(Name))/binary>>.

hex(Bin) ->
    << <<(integer_to_binary(N, 16))/binary>> || <<N:4>> <= Bin >>.

%% A multipart/mixed body of Parts, [{Headers, Body}].
-spec multipart(binary(), [{[{binary(), binary()}], binary()}]) -> binary().
multipart(Boundary, Parts) ->
    iolist_to_binary(
      [[[<<"--">>, Boundary, <<"\r\n">>],
        [[K, <<": ">>, V, <<"\r\n">>] || {K, V} <- Headers],
        <<"\r\n">>, Body, <<"\r\n">>] || {Headers, Body} <- Parts]
      ++ [<<"--">>, Boundary, <<"--\r\n">>]).

%% The parts of a multipart body, its headers' names in lower case.
-spec parse_multipart(binary() | undefined, binary()) ->
          {ok, [{[{binary(), binary()}], binary()}]} | error.
parse_multipart(undefined, _Body) ->
    error;
parse_multipart(ContentType, Body) ->
    case re:run(ContentType, <<"^\\s*multipart/[^;]+;.*boundary=(\"([^\"]+)\"|([^;\\s]+))">>,
                [caseless, {capture, all_but_first, binary}]) of
        {match, Groups} ->
            Boundary = hd([G || G <- tl(Groups), G /= <<>>]),
            case binary:split(Body, <<"--", Boundary/binary>>, [global]) of
                [_Preamble | Rest] when length(Rest) >= 2 ->
                    {Parts, [_Epilogue]} = lists:split(length(Rest) - 1, Rest),
                    try {ok, [part(P) || P <- Parts]}
                    catch _:_ -> error
                    end;
                _ ->
                    error
            end;
        nomatch ->
            error
    end.

part(<<"\r\n", P/binary>>) ->
    Content = case binary:longest_common_suffix([P, <<"\r\n">>]) of
                  2 -> binary:part(P, 0, byte_size(P) - 2);
                  _ -> P
              end,
    {HeaderBlock, Body} =
        case Content of
            <<"\r\n", B/binary>> ->
                {<<>>, B};
            _ ->
                [H, B] = binary:split(Content, <<"\r\n\r\n">>),
                {H, B}
        end,
    Headers = [header(Line) || Line <- binary:split(HeaderBlock, <<"\r\n">>, [global]), Line /= <<>>],
    {Headers, Body}.

header(Line) ->
    [K, V] = binary:split(Line, <<":">>),
    {string:lowercase(string:trim(K)), string:trim(V)}.

%%--------------------------------------------------------------------
%% The wire: <media-id/> and <media-fetch/> to the gateway's JID
%%--------------------------------------------------------------------

%% An iq to the gateway's own JID: one of the two media requests from a
%% local user is answered here (<media-fetch/> by a process of its own),
%% anything else goes on as before.
-spec route_iq(iq()) -> any().
route_iq(#iq{type = get, from = From, lang = Lang,
             sub_els = [#xmlel{name = Name, attrs = Attrs}]} = IQ)
  when Name == <<"media-id">>; Name == <<"media-fetch">> ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
        ?NS_E2E ->
            Host = ejabberd_config:get_myname(),
            case is_local_user(From, Host) of
                false ->
                    ejabberd_router:route_error(
                      IQ, xmpp:err_not_allowed(<<"Only for the users of this server">>, Lang));
                true when Name == <<"media-id">> ->
                    ejabberd_router:route(media_id(Host, IQ, Attrs));
                true ->
                    spawn(fun() -> ejabberd_router:route(media_fetch(Host, IQ, Attrs)) end),
                    ok
            end;
        _ ->
            ejabberd_router:process_iq(IQ)
    end;
route_iq(IQ) ->
    ejabberd_router:process_iq(IQ).

is_local_user(#jid{luser = U, lserver = Host}, Host) when U /= <<>> ->
    ejabberd_auth:user_exists(U, Host);
is_local_user(_, _) ->
    false.

media_id(Host, #iq{lang = Lang} = IQ, Attrs) ->
    URL = fxml:get_attr_s(<<"url">>, Attrs),
    case mint(Host, URL, hints(Attrs)) of
        {ok, MXC} ->
            xmpp:make_iq_result(IQ, #xmlel{name = <<"media-id">>,
                                           attrs = [{<<"xmlns">>, ?NS_E2E}, {<<"mxc">>, MXC}]});
        {error, not_allowed} ->
            xmpp:make_error(IQ, xmpp:err_not_allowed(<<"Not a file of this server's store">>, Lang))
    end.

media_fetch(Host, #iq{lang = Lang} = IQ, Attrs) ->
    case resolve(Host, fxml:get_attr_s(<<"mxc">>, Attrs), hints(Attrs)) of
        {ok, #{url := URL, name := Name, mime := Mime} = Info} ->
            Size = case Info of
                       #{size := S} -> [{<<"size">>, integer_to_binary(S)}];
                       _ -> []
                   end,
            xmpp:make_iq_result(
              IQ, #xmlel{name = <<"media-fetch">>,
                         attrs = [{<<"xmlns">>, ?NS_E2E}, {<<"url">>, URL}, {<<"name">>, Name},
                                  {<<"mime">>, Mime} | Size]});
        {error, Reason} ->
            xmpp:make_error(IQ, iq_error(Reason, Lang))
    end.

iq_error(not_found, Lang) -> xmpp:err_item_not_found(<<"No such media">>, Lang);
iq_error(too_large, Lang) -> xmpp:err_resource_constraint(<<"The file is too large">>, Lang);
iq_error(timeout, Lang) -> xmpp:err_remote_server_timeout(<<"The Matrix server did not answer">>, Lang);
iq_error(upload, Lang) -> xmpp:err_internal_server_error(<<"The file could not be stored">>, Lang);
iq_error(refused, Lang) -> xmpp:err_not_allowed(<<"The Matrix server named a place the gateway does not fetch from">>, Lang);
iq_error(busy, Lang) -> xmpp:err_resource_constraint(<<"The gateway is fetching too many files">>, Lang);
iq_error(_, Lang) -> xmpp:err_remote_server_not_found(<<"The Matrix server cannot be reached">>, Lang).

hints(Attrs) ->
    maps:from_list(
      [{K, V} || {K, V} <- [{name, fxml:get_attr_s(<<"name">>, Attrs)},
                            {mime, fxml:get_attr_s(<<"mime">>, Attrs)},
                            {size, to_int(fxml:get_attr_s(<<"size">>, Attrs))}],
                 V /= <<>>, V /= undefined]).

to_int(B) ->
    case catch binary_to_integer(B) of
        N when is_integer(N), N >= 0 -> N;
        _ -> undefined
    end.

%%--------------------------------------------------------------------
%% Plain rooms and direct chats: XMPP to Matrix
%%--------------------------------------------------------------------

%% The file a message shares, {URL, Meta}, or none: the XEP-0447
%% <file-sharing/> with its XEP-0446 metadata and a <url-data/> source,
%% else an XEP-0066 <x xmlns='jabber:x:oob'/>.
-spec share([xmpp_element() | xmlel()]) -> {binary(), info()} | none.
share(Els) ->
    case [S || #xmlel{name = <<"file-sharing">>} = El <- Els,
               xmlns(El) == ?NS_SFS, {_, _} = S <- [sfs(El)]] of
        [S | _] ->
            S;
        [] ->
            case [U || El <- Els, U <- [oob_url(El)], U /= <<>>] of
                [URL | _] -> {URL, #{}};
                [] -> none
            end
    end.

xmlns(#xmlel{attrs = Attrs}) -> fxml:get_attr_s(<<"xmlns">>, Attrs).

oob_url(#oob_x{url = URL}) ->
    URL;
oob_url(#xmlel{name = <<"x">>} = El) ->
    case xmlns(El) of
        ?NS_OOB_X -> fxml:get_path_s(El, [{elem, <<"url">>}, cdata]);
        _ -> <<>>
    end;
oob_url(_) ->
    <<>>.

sfs(El) ->
    URLs = [fxml:get_attr_s(<<"target">>, Attrs)
            || #xmlel{name = <<"sources">>, children = Sources} <- El#xmlel.children,
               #xmlel{name = <<"url-data">>, attrs = Attrs} <- Sources],
    case [U || U <- URLs, U /= <<>>] of
        [URL | _] ->
            Meta = case [F || #xmlel{name = <<"file">>} = F <- El#xmlel.children,
                              xmlns(F) == ?NS_FILE_META] of
                       [File | _] -> file_meta(File);
                       [] -> #{}
                   end,
            {URL, Meta};
        [] ->
            none
    end.

file_meta(File) ->
    Text = fun(Name) -> fxml:get_path_s(File, [{elem, Name}, cdata]) end,
    %% <dimensions>WxH</dimensions> as clients
    %% writes them, <desc/> (a sticker's name)
    {W, H} = case binary:split(Text(<<"dimensions">>), <<"x">>) of
                 [W0, H0] -> {to_int(W0), to_int(H0)};
                 _ -> {undefined, undefined}
             end,
    Fields = [{mime, Text(<<"media-type">>)}, {name, Text(<<"name">>)},
              {size, to_int(Text(<<"size">>))},
              {width, case to_int(Text(<<"width">>)) of undefined -> W; W1 -> W1 end},
              {height, case to_int(Text(<<"height">>)) of undefined -> H; H1 -> H1 end},
              {duration, to_int(Text(<<"length">>))}, {desc, Text(<<"desc">>)}],
    maps:from_list([{K, V} || {K, V} <- Fields, V /= <<>>, V /= undefined]).

%% The m.room.message contents a share of URL is, the file then the
%% caption (Text when it is more than the URL), or not_ours when URL is
%% not in the store (then the message is the text it was).
-spec outbound_contents(binary(), binary(), info(), binary(), binary()) ->
          {ok, [map()]} | not_ours.
outbound_contents(Host, URL, Meta, Text, XmppID) ->
    case mint(Host, URL, Meta) of
        {ok, MXC} ->
            Caption = string:trim(binary:replace(Text, URL, <<>>, [global])),
            {ok, [media_content(MXC, Meta#{name => maps:get(name, Meta, url_name(URL))}, XmppID)
                  | [mod_matrix_gw_xmpp:text_content(Caption, <<>>) || Caption /= <<>>]]};
        {error, not_allowed} ->
            not_ours
    end.

%% m.image, m.video, m.audio or m.file by the media type.
-spec media_content(binary(), info(), binary()) -> map().
media_content(MXC, Meta, XmppID) ->
    Name = maps:get(name, Meta, <<"file">>),
    Mime = maps:get(mime, Meta, mime_of(Name)),
    Info = maps:from_list(
             [{<<"mimetype">>, Mime} |
              [{K, V} || {F, K} <- [{size, <<"size">>}, {width, <<"w">>}, {height, <<"h">>},
                                    {duration, <<"duration">>}],
                         V <- [maps:get(F, Meta, undefined)], is_integer(V)]]),
    Content = #{<<"msgtype">> => msgtype(Mime), <<"body">> => Name, <<"filename">> => Name,
                <<"url">> => MXC, <<"info">> => Info},
    case XmppID of
        <<>> -> Content;
        _ -> Content#{<<"net.process-one.xmpp-id">> => XmppID}
    end.

msgtype(<<"image/", _/binary>>) -> <<"m.image">>;
msgtype(<<"video/", _/binary>>) -> <<"m.video">>;
msgtype(<<"audio/", _/binary>>) -> <<"m.audio">>;
msgtype(_) -> <<"m.file">>.

url_name(URL) ->
    Path = case uri_string:parse(URL) of
               #{path := P} -> P;
               _ -> <<>>
           end,
    case lists:last([<<>> | binary:split(Path, <<"/">>, [global])]) of
        <<>> -> <<"file">>;
        Seg ->
            percent_decode(Seg, Seg)
    end.

%% uri_string:percent_decode/1 throws on a bad escape.
percent_decode(Bin, Default) ->
    try uri_string:percent_decode(Bin) of
        D when is_binary(D) -> D;
        _ -> Default
    catch _:_ -> Default
    end.

mime_of(Name) ->
    Ext = string:lowercase(filename:extension(Name)),
    proplists:get_value(
      Ext,
      [{<<".jpg">>, <<"image/jpeg">>}, {<<".jpeg">>, <<"image/jpeg">>}, {<<".png">>, <<"image/png">>},
       {<<".gif">>, <<"image/gif">>}, {<<".webp">>, <<"image/webp">>}, {<<".heic">>, <<"image/heic">>},
       {<<".svg">>, <<"image/svg+xml">>}, {<<".mp4">>, <<"video/mp4">>},
       {<<".webm">>, <<"video/webm">>}, {<<".mov">>, <<"video/quicktime">>},
       {<<".mkv">>, <<"video/x-matroska">>}, {<<".mp3">>, <<"audio/mpeg">>},
       {<<".m4a">>, <<"audio/mp4">>}, {<<".ogg">>, <<"audio/ogg">>}, {<<".oga">>, <<"audio/ogg">>},
       {<<".opus">>, <<"audio/ogg">>}, {<<".wav">>, <<"audio/wav">>}, {<<".flac">>, <<"audio/flac">>},
       {<<".pdf">>, <<"application/pdf">>}, {<<".txt">>, <<"text/plain">>},
       {<<".zip">>, <<"application/zip">>}],
      <<"application/octet-stream">>).

%%--------------------------------------------------------------------
%% Plain rooms and direct chats: Matrix to XMPP
%%--------------------------------------------------------------------

%% Delivers Msg (a room's message for one XMPP resource, its body empty) as
%% the share of the file the event Content names, once the file is in the
%% store: by a process of its own, so the room goes on meanwhile.
-spec route_share(binary(), message(), map()) -> pid().
route_share(Host, Msg, Content) ->
    spawn(fun() -> ejabberd_router:route(share_message(Host, Msg, Content)) end).

%% Msg as the share of Content's file, or with a body that names the file
%% and why it is not there.
-spec share_message(binary(), message(), map()) -> message().
share_message(Host, #message{sub_els = Els} = Msg, Content) ->
    Info0 = maps:get(<<"info">>, Content, #{}),
    Info = case is_map(Info0) of true -> Info0; false -> #{} end,
    Hints = maps:from_list(
              [{K, V} || {K, V} <- [{name, file_name(Content)},
                                    {mime, maps:get(<<"mimetype">>, Info, <<>>)}],
                         is_binary(V), V /= <<>>]),
    case resolve(Host, maps:get(<<"url">>, Content, <<>>), Hints) of
        {ok, #{url := URL} = File} ->
            Extra = maps:from_list(
                      [{K, V} || {F, K} <- [{<<"w">>, width}, {<<"h">>, height},
                                            {<<"duration">>, duration}],
                                 V <- [maps:get(F, Info, undefined)], is_integer(V)]),
            Msg#message{body = [#text{data = URL}],
                        sub_els = Els ++ share_els(maps:merge(Extra, File))};
        {error, Reason} ->
            Msg#message{body = [#text{data = failure_body(file_name(Content), Reason)}]}
    end.

%% The name an event's file goes by.
-spec file_name(map()) -> binary().
file_name(#{<<"filename">> := N}) when is_binary(N), N /= <<>> -> N;
file_name(#{<<"body">> := N}) when is_binary(N), N /= <<>> -> N;
file_name(_) -> <<"file">>.

-spec failure_body(binary(), atom()) -> binary().
failure_body(Name, Reason) ->
    Why = case Reason of
              not_found -> <<"not found on its server">>;
              too_large -> <<"too large for the gateway">>;
              timeout -> <<"its server did not answer">>;
              upload -> <<"it could not be stored">>;
              refused -> <<"its server named a place the gateway does not fetch from">>;
              busy -> <<"the gateway is fetching too many files">>;
              _ -> <<"its server cannot be reached">>
          end,
    <<Name/binary, " (a file from Matrix that could not be fetched: ", Why/binary, ")">>.

%% XEP-0066 OOB, XEP-0447 file sharing with XEP-0446 metadata and one
%% url-data source, and the hint to store the message.
-spec share_els(info()) -> [xmlel()].
share_els(#{url := URL} = File) ->
    Meta = [#xmlel{name = Name, children = [{xmlcdata, V}]}
            || {Key, Name} <- [{mime, <<"media-type">>}, {name, <<"name">>}, {desc, <<"desc">>},
                               {size, <<"size">>},
                               {width, <<"width">>}, {height, <<"height">>},
                               {duration, <<"length">>}],
               V0 <- [maps:get(Key, File, undefined)], V0 /= undefined,
               V <- [case V0 of I when is_integer(I) -> integer_to_binary(I); B -> B end]],
    [#xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_OOB_X}],
            children = [#xmlel{name = <<"url">>, children = [{xmlcdata, URL}]}]},
     #xmlel{name = <<"file-sharing">>, attrs = [{<<"xmlns">>, ?NS_SFS}],
            children = [#xmlel{name = <<"file">>, attrs = [{<<"xmlns">>, ?NS_FILE_META}],
                               children = Meta},
                        #xmlel{name = <<"sources">>,
                               children = [#xmlel{name = <<"url-data">>,
                                                  attrs = [{<<"xmlns">>, ?NS_URL_DATA},
                                                           {<<"target">>, URL}]}]}]},
     mod_matrix_gw_xmpp:store_el()].

%% A room event as the end-to-end wire carries it: an m.room.message
%% whose file is an EncryptedFile goes to the device as it came, for the
%% device to open.
-spec event_el(binary(), binary(), binary(), binary(), integer(), map()) -> xmlel().
event_el(Type, EventID, RoomID, Sender, TS, Content) ->
    mod_matrix_gw_event:event_el(#{type => Type, id => EventID, room => RoomID,
                                   sender => Sender, ts => TS}, Content).
