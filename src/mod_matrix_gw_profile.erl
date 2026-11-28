%%%-------------------------------------------------------------------
%%% File    : mod_matrix_gw_profile.erl
%%% Author  : Paavel A. <pd@narayana.im>
%%% Purpose : Names and avatars between XMPP and Matrix
%%% Created : 2 Nov 2026 by Paavel A. <pd@narayana.im>
%%%
%%%
%%%   our users      query/profile answers displayname (the vCard's
%%%                  NICKNAME, else FN) and avatar_url: the vCard's PHOTO
%%%                  put into the server's upload store once per picture
%%%                  and named by an mxc of ours (mod_matrix_gw_media:mint/3),
%%%                  which the federation downloads; an m.room.member join the
%%%                  gateway makes for a user carries both (member_content/3)
%%%   Matrix users   their /query/profile, asked of their server and kept an
%%%                  hour (an ets table), answers a vcard-temp get on
%%%                  u%srv@<gateway> (FN, NICKNAME, PHOTO fetched through the
%%%                  media module) and names them in XEP-0172 <nick/>; a
%%%                  subscription to u%srv@<gateway> is accepted and a probe
%%%                  answered with an unavailable presence carrying the
%%%                  XEP-0153 photo hash and the <nick/> (m.presence is not
%%%                  carried, by ruling: the contact shows offline, with its
%%%                  name and picture)
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
-module(mod_matrix_gw_profile).

-export([create_db/0, local_profile/3, local_avatar/3, member_content/3,
         remote_profile/2, fetch_profile_with/2, cached_name/2, route_iq/1, route_presence/1,
         vcard_el/3, xupdate_el/1, parse_profile/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").

-define(CACHE, matrix_gw_profile_cache).
-define(TTL, 3600000).
-define(AVATAR_MAX, 1048576).

-record(matrix_local_avatar,
        {us :: {binary(), binary()},
         hash :: binary(),
         mxc :: binary()}).

create_db() ->
    ejabberd_mnesia:create(
      ?MODULE, matrix_local_avatar,
      [{disc_copies, [node()]},
       {type, set},
       {attributes, record_info(fields, matrix_local_avatar)}]),
    case ets:info(?CACHE) of
        undefined ->
            %% owned by mod_matrix_gw's process, which outlives the
            %% requests; lost with it, which only costs a fetch
            ets:new(?CACHE, [named_table, public, set, {read_concurrency, true}]);
        _ ->
            ok
    end,
    ok.

%%--------------------------------------------------------------------
%% Our users
%%--------------------------------------------------------------------

%% The profile of a local user for the federation: displayname and
%% avatar_url when there are any. Mode block makes the avatar's mxc when
%% the picture is new (an upload to the store); cached answers only what
%% is already made and makes it in the background.
-spec local_profile(binary(), binary(), block | cached) -> map().
local_profile(LUser, Host, Mode) ->
    VCard = vcard(LUser, Host),
    Name = case fxml:get_path_s(VCard, [{elem, <<"NICKNAME">>}, cdata]) of
               <<>> -> fxml:get_path_s(VCard, [{elem, <<"FN">>}, cdata]);
               Nick -> Nick
           end,
    maps:from_list([{<<"displayname">>, Name} || Name /= <<>>] ++
                   [{<<"avatar_url">>, MXC} || MXC <- [avatar_mxc(LUser, Host, VCard, Mode)],
                                               MXC /= <<>>]).

vcard(LUser, Host) ->
    case catch mod_vcard:get_vcard(LUser, Host) of
        [#xmlel{} = VCard | _] -> VCard;
        _ -> #xmlel{name = <<"vCard">>}
    end.

-spec local_avatar(binary(), binary(), block | cached) -> binary().
local_avatar(LUser, Host, Mode) ->
    avatar_mxc(LUser, Host, vcard(LUser, Host), Mode).

avatar_mxc(LUser, Host, VCard, Mode) ->
    case photo(VCard) of
        none ->
            <<>>;
        {Mime, Bytes} ->
            Hash = sha1(Bytes),
            case mnesia:dirty_read(matrix_local_avatar, {LUser, Host}) of
                [#matrix_local_avatar{hash = Hash, mxc = MXC}] ->
                    MXC;
                _ when Mode == cached ->
                    spawn(fun() -> make_avatar(LUser, Host, Hash, Mime, Bytes) end),
                    <<>>;
                _ ->
                    make_avatar(LUser, Host, Hash, Mime, Bytes)
            end
    end.

photo(VCard) ->
    case fxml:get_path_s(VCard, [{elem, <<"PHOTO">>}, {elem, <<"BINVAL">>}, cdata]) of
        <<>> ->
            none;
        B64 ->
            try base64:decode(binary:replace(B64, [<<"\n">>, <<"\r">>, <<" ">>, <<"\t">>], <<>>, [global])) of
                Bytes ->
                    Mime = case fxml:get_path_s(VCard, [{elem, <<"PHOTO">>}, {elem, <<"TYPE">>}, cdata]) of
                               <<>> -> <<"image/png">>;
                               T -> T
                           end,
                    {Mime, Bytes}
            catch _:_ ->
                    none
            end
    end.

make_avatar(LUser, Host, Hash, Mime, Bytes) ->
    Name = <<"avatar", (ext(Mime))/binary>>,
    case mod_matrix_gw_media:upload(Host, Name, Mime, Bytes) of
        {ok, URL} ->
            case mod_matrix_gw_media:mint(Host, URL, #{name => Name, mime => Mime,
                                                       size => byte_size(Bytes)}) of
                {ok, MXC} ->
                    mnesia:dirty_write(#matrix_local_avatar{us = {LUser, Host}, hash = Hash,
                                                            mxc = MXC}),
                    MXC;
                {error, _} ->
                    <<>>
            end;
        {error, _} ->
            ?DEBUG("no avatar mxc for ~s@~s: the store refused it", [LUser, Host]),
            <<>>
    end.

ext(<<"image/jpeg">>) -> <<".jpg">>;
ext(<<"image/gif">>) -> <<".gif">>;
ext(<<"image/webp">>) -> <<".webp">>;
ext(_) -> <<".png">>.

sha1(Bytes) ->
    str:to_hexlist(crypto:hash(sha, Bytes)).

%% Content (an m.room.member join's) with the profile of MXID when it is a
%% local user: displayname and avatar_url, the avatar only when already made.
-spec member_content(binary(), binary(), map()) -> map().
member_content(Host, MXID, Content) ->
    MatrixDomain = mod_matrix_gw_opt:matrix_domain(Host),
    case catch mod_matrix_gw_room:parse_user_id(MXID) of
        {ok, LUser, MatrixDomain} ->
            try maps:merge(Content, local_profile(LUser, Host, cached))
            catch _:_ -> Content
            end;
        _ ->
            Content
    end.

%%--------------------------------------------------------------------
%% Matrix users
%%--------------------------------------------------------------------

%% A remote user's profile: {ok, #{displayname, avatar_url}} (either may be
%% missing) from the cache or asked of the user's server.
-spec remote_profile(binary(), binary()) -> {ok, map()} | error.
remote_profile(Host, MXID) ->
    Now = erlang:system_time(millisecond),
    case cache_get(MXID) of
        {ok, TS, Profile, _} when Now - TS < ?TTL ->
            {ok, Profile};
        _ ->
            fetch_profile(Host, MXID)
    end.

cache_get(MXID) ->
    try ets:lookup(?CACHE, MXID) of
        [{_, TS, Profile, Photo}] -> {ok, TS, Profile, Photo};
        [] -> error
    catch _:_ -> error
    end.

cache_put(MXID, Profile, Photo) ->
    catch ets:insert(?CACHE, {MXID, erlang:system_time(millisecond), Profile, Photo}),
    ok.

fetch_profile(Host, MXID) ->
    fetch_profile_with(MXID, fun(Server) ->
                                mod_matrix_gw:send_request(
                                  Host, get, Server,
                                  [<<"_matrix">>, <<"federation">>, <<"v1">>, <<"query">>, <<"profile">>],
                                  [{<<"user_id">>, MXID}], none,
                                  [{timeout, 5000}, {connect_timeout, 5000}],
                                  [{sync, true}, {body_format, binary}])
                        end).

%% MXID's profile asked of its server through Get(Server) (the federation's
%% request, a function so the tests can stand in for it), and cached; the
%% avatar's hash is kept while the avatar is the same.
-spec fetch_profile_with(binary(), fun((binary()) -> term())) -> {ok, map()} | error.
fetch_profile_with(MXID, Get) ->
    case catch mod_matrix_gw:get_id_domain_exn(MXID) of
        Server when is_binary(Server) ->
            case Get(Server) of
                {ok, {{_, 200, _}, _, Body}} ->
                    Profile = parse_profile(Body),
                    Photo = case cache_get(MXID) of
                                {ok, _, #{<<"avatar_url">> := A}, P} ->
                                    case maps:get(<<"avatar_url">>, Profile, <<>>) of
                                        A -> P;
                                        _ -> undefined
                                    end;
                                _ -> undefined
                            end,
                    cache_put(MXID, Profile, Photo),
                    {ok, Profile};
                Res ->
                    ?DEBUG("no profile of ~s: ~p", [MXID, Res]),
                    %% a server that does not answer is asked again in
                    %% five minutes, not at every message
                    case cache_get(MXID) of
                        error ->
                            catch ets:insert(?CACHE, {MXID, erlang:system_time(millisecond) - ?TTL + 300000,
                                                      #{}, undefined});
                        _ ->
                            ok
                    end,
                    error
            end;
        _ ->
            error
    end.

%% A /query/profile answer's fields that are strings.
-spec parse_profile(binary()) -> map().
parse_profile(Body) ->
    try misc:json_decode(Body) of
        #{} = J ->
            maps:filter(fun(K, V) -> lists:member(K, [<<"displayname">>, <<"avatar_url">>])
                                         andalso is_binary(V) andalso V /= <<>> end, J);
        _ -> #{}
    catch _:_ -> #{}
    end.

%% The display name of MXID if the cache has it; a miss fetches it in the
%% background for the next message.
-spec cached_name(binary(), binary()) -> binary().
cached_name(Host, MXID) ->
    case cache_get(MXID) of
        {ok, TS, Profile, _} ->
            case erlang:system_time(millisecond) - TS < ?TTL of
                true -> ok;
                false -> spawn(fun() -> fetch_profile(Host, MXID) end)
            end,
            maps:get(<<"displayname">>, Profile, <<>>);
        error ->
            spawn(fun() -> fetch_profile(Host, MXID) end),
            <<>>
    end.

%% The avatar of a remote profile: {Mime, Bytes, Hash}, none, or error.
avatar(Host, MXID, #{<<"avatar_url">> := MXC}) ->
    case mod_matrix_gw_media:resolve(Host, MXC, #{}) of
        {ok, #{url := URL} = Info} ->
            case mod_matrix_gw_media:plain_get(Host, URL, ?AVATAR_MAX) of
                {ok, 200, _, Bytes} ->
                    Hash = sha1(Bytes),
                    case cache_get(MXID) of
                        {ok, _, Profile, _} -> cache_put(MXID, Profile, Hash);
                        _ -> ok
                    end,
                    {maps:get(mime, Info, <<"image/png">>), Bytes, Hash};
                Other ->
                    ?DEBUG("no avatar of ~s from ~s: ~p", [MXID, URL, Other]),
                    error
            end;
        {error, Reason} ->
            ?DEBUG("no avatar of ~s: ~p", [MXID, Reason]),
            error
    end;
avatar(_Host, _MXID, _) ->
    none.

%% A vcard-temp get on a Matrix user's JID, answered from its profile in a
%% process of its own (the profile and the picture may take a while).
-spec route_iq(iq()) -> ok.
route_iq(#iq{type = get, from = From, to = To} = IQ) ->
    Host = ejabberd_config:get_myname(),
    case mxid(To, Host) of
        {ok, MXID} when From#jid.lserver == Host ->
            spawn(fun() ->
                          Reply =
                              case remote_profile(Host, MXID) of
                                  {ok, Profile} ->
                                      Photo = case avatar(Host, MXID, Profile) of
                                                  {Mime, Bytes, _} -> {Mime, Bytes};
                                                  _ -> none
                                              end,
                                      xmpp:make_iq_result(
                                        IQ, vcard_el(MXID, maps:get(<<"displayname">>, Profile, <<>>),
                                                     Photo));
                                  error ->
                                      xmpp:make_iq_result(IQ, #xmlel{name = <<"vCard">>,
                                                                     attrs = [{<<"xmlns">>, ?NS_VCARD}]})
                              end,
                          ejabberd_router:route(Reply)
                  end),
            ok;
        _ ->
            ejabberd_router:route_error(IQ, xmpp:err_service_unavailable())
    end;
route_iq(IQ) ->
    ejabberd_router:route_error(IQ, xmpp:err_not_allowed()).

%% vcard-temp of a Matrix user: FN and NICKNAME its display name (else its
%% id), PHOTO its avatar.
-spec vcard_el(binary(), binary(), {binary(), binary()} | none) -> xmlel().
vcard_el(MXID, Name0, Photo) ->
    Name = case Name0 of <<>> -> MXID; _ -> Name0 end,
    Text = fun(N, V) -> #xmlel{name = N, children = [{xmlcdata, V}]} end,
    #xmlel{name = <<"vCard">>, attrs = [{<<"xmlns">>, ?NS_VCARD}],
           children = [Text(<<"FN">>, Name), Text(<<"NICKNAME">>, Name)] ++
               case Photo of
                   {Mime, Bytes} ->
                       [#xmlel{name = <<"PHOTO">>,
                               children = [Text(<<"TYPE">>, Mime),
                                           Text(<<"BINVAL">>, base64:encode(Bytes))]}];
                   none ->
                       []
               end}.

%% XEP-0153's <x/>: the photo's hash, empty for none, no <photo/> while
%% not known.
-spec xupdate_el(binary() | undefined) -> xmlel().
xupdate_el(Hash) ->
    #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_VCARD_UPDATE}],
           children = case Hash of
                          undefined -> [];
                          <<>> -> [#xmlel{name = <<"photo">>}];
                          _ -> [#xmlel{name = <<"photo">>, children = [{xmlcdata, Hash}]}]
                      end}.

%% Presence to a Matrix user's JID from a local user: a subscription is
%% accepted, a probe answered (unavailable, with the picture's hash and the
%% name), an unsubscription confirmed; m.presence is not carried.
-spec route_presence(presence()) -> ok.
route_presence(#presence{type = Type, from = From, to = To}) ->
    Host = ejabberd_config:get_myname(),
    case mxid(To, Host) of
        {ok, MXID} when From#jid.lserver == Host ->
            Contact = jid:remove_resource(To),
            case Type of
                subscribe ->
                    ejabberd_router:route(#presence{type = subscribed, from = Contact,
                                                    to = jid:remove_resource(From)}),
                    ejabberd_router:route(#presence{type = unavailable, from = Contact,
                                                    to = jid:remove_resource(From)});
                unsubscribe ->
                    ejabberd_router:route(#presence{type = unsubscribed, from = Contact,
                                                    to = jid:remove_resource(From)});
                probe ->
                    spawn(fun() -> answer_probe(Host, MXID, Contact, From) end);
                _ ->
                    ok
            end,
            ok;
        _ ->
            ok
    end.

answer_probe(Host, MXID, Contact, To) ->
    case remote_profile(Host, MXID) of
        {ok, Profile} ->
            Hash = case cache_get(MXID) of
                       {ok, _, _, H} when is_binary(H) -> H;
                       _ ->
                           case avatar(Host, MXID, Profile) of
                               {_, _, H} -> H;
                               none -> <<>>;
                               error -> undefined
                           end
                   end,
            Nick = [mod_matrix_gw_content:nick_el(N)
                    || N <- [maps:get(<<"displayname">>, Profile, <<>>)], N /= <<>>],
            ejabberd_router:route(#presence{type = unavailable, from = Contact, to = To,
                                            sub_els = [xupdate_el(Hash) | Nick]});
        error ->
            ejabberd_router:route(#presence{type = unavailable, from = Contact, to = To})
    end.

mxid(#jid{luser = U, lserver = Server}, Host) when U /= <<>> ->
    case mod_matrix_gw_opt:host(Host) of
        Server ->
            case binary:split(U, <<"%">>) of
                [EscU, EscS] when EscU /= <<>>, EscS /= <<>> ->
                    {ok, <<$@, (mod_matrix_gw_room:unescape(EscU))/binary, $:,
                           (mod_matrix_gw_room:unescape(EscS))/binary>>};
                _ ->
                    error
            end;
        _ ->
            error
    end;
mxid(_, _) ->
    error.
