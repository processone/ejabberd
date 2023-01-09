%%%----------------------------------------------------------------------
%%% File    : mod_vcard_xupdate.erl
%%% Author  : Igor Goryachev <igor@goryachev.org>
%%% Purpose : Add avatar hash in presence on behalf of client (XEP-0153)
%%% Created : 9 Mar 2007 by Igor Goryachev <igor@goryachev.org>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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

-module(mod_vcard_xupdate).
-behaviour(gen_mod).

-protocol({xep, 398, '0.2.0'}).

%% gen_mod callbacks
-export([start/2, stop/1, reload/3]).

-export([update_presence/1, vcard_set/1, remove_user/2, mod_doc/0,
	 user_send_packet/1, mod_opt_type/1, mod_options/1, depends/2]).
%% API
-export([compute_hash/1]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

-define(VCARD_XUPDATE_CACHE, vcard_xupdate_cache).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

start(Host, Opts) ->
    init_cache(Host, Opts),
    ejabberd_hooks:add(c2s_self_presence, Host, ?MODULE,
		       update_presence, 100),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       user_send_packet, 50),
    ejabberd_hooks:add(vcard_iq_set, Host, ?MODULE, vcard_set,
		       90),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50).

stop(Host) ->
    ejabberd_hooks:delete(c2s_self_presence, Host,
			  ?MODULE, update_presence, 100),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  user_send_packet, 50),
    ejabberd_hooks:delete(vcard_iq_set, Host, ?MODULE,
			  vcard_set, 90),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 50).

reload(Host, NewOpts, _OldOpts) ->
    init_cache(Host, NewOpts).

depends(_Host, _Opts) ->
    [{mod_vcard, hard}].

%%====================================================================
%% Hooks
%%====================================================================
-spec update_presence({presence(), ejabberd_c2s:state()})
      -> {presence(), ejabberd_c2s:state()}.
update_presence({#presence{type = available} = Pres,
		 #{jid := #jid{luser = LUser, lserver = LServer}} = State}) ->
    case xmpp:get_subtag(Pres, #vcard_xupdate{}) of
	#vcard_xupdate{hash = <<>>} ->
	    %% XEP-0398 forbids overwriting vcard:x:update
	    %% tags with empty <photo/> element
	    {Pres, State};
	_ ->
	    Pres1 = case get_xupdate(LUser, LServer) of
			undefined -> xmpp:remove_subtag(Pres, #vcard_xupdate{});
			XUpdate -> xmpp:set_subtag(Pres, XUpdate)
		    end,
	    {Pres1, State}
    end;
update_presence(Acc) ->
    Acc.

-spec user_send_packet({presence(), ejabberd_c2s:state()})
      -> {presence(), ejabberd_c2s:state()}.
user_send_packet({#presence{type = available,
			    to = #jid{luser = U, lserver = S,
				      lresource = <<"">>}},
		  #{jid := #jid{luser = U, lserver = S}}} = Acc) ->
    %% This is processed by update_presence/2 explicitly, we don't
    %% want to call this multiple times for performance reasons
    Acc;
user_send_packet(Acc) ->
    update_presence(Acc).

-spec vcard_set(iq()) -> iq().
vcard_set(#iq{from = #jid{luser = LUser, lserver = LServer}} = IQ) ->
    ets_cache:delete(?VCARD_XUPDATE_CACHE, {LUser, LServer}),
    ejabberd_sm:force_update_presence({LUser, LServer}),
    IQ;
vcard_set(Acc) ->
    Acc.

-spec remove_user(binary(), binary()) -> ok.
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    ets_cache:delete(?VCARD_XUPDATE_CACHE, {LUser, LServer}).

%%====================================================================
%% Storage
%%====================================================================
-spec get_xupdate(binary(), binary()) -> vcard_xupdate() | undefined.
get_xupdate(LUser, LServer) ->
    Result = case use_cache(LServer) of
		 true ->
		     ets_cache:lookup(
		       ?VCARD_XUPDATE_CACHE, {LUser, LServer},
		       fun() -> db_get_xupdate(LUser, LServer) end);
		 false ->
		     db_get_xupdate(LUser, LServer)
	     end,
    case Result of
	{ok, external} -> undefined;
	{ok, Hash} -> #vcard_xupdate{hash = Hash};
	error -> #vcard_xupdate{}
    end.

-spec db_get_xupdate(binary(), binary()) -> {ok, binary() | external} | error.
db_get_xupdate(LUser, LServer) ->
    case mod_vcard:get_vcard(LUser, LServer) of
	[VCard] ->
	    {ok, compute_hash(VCard)};
	_ ->
	    error
    end.

-spec init_cache(binary(), gen_mod:opts()) -> ok.
init_cache(Host, Opts) ->
    case use_cache(Host) of
	true ->
	    CacheOpts = cache_opts(Opts),
	    ets_cache:new(?VCARD_XUPDATE_CACHE, CacheOpts);
	false ->
	    ets_cache:delete(?VCARD_XUPDATE_CACHE)
    end.

-spec cache_opts(gen_mod:opts()) -> [proplists:property()].
cache_opts(Opts) ->
    MaxSize = mod_vcard_xupdate_opt:cache_size(Opts),
    CacheMissed = mod_vcard_xupdate_opt:cache_missed(Opts),
    LifeTime = mod_vcard_xupdate_opt:cache_life_time(Opts),
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec use_cache(binary()) -> boolean().
use_cache(Host) ->
    mod_vcard_xupdate_opt:use_cache(Host).

-spec compute_hash(xmlel()) -> binary() | external.
compute_hash(VCard) ->
    case fxml:get_subtag(VCard, <<"PHOTO">>) of
	false ->
	    <<>>;
	Photo ->
	    try xmpp:decode(Photo, ?NS_VCARD, []) of
		#vcard_photo{binval = <<_, _/binary>> = BinVal} ->
		    str:sha(BinVal);
		#vcard_photo{extval = <<_, _/binary>>} ->
		    external;
		_ ->
		    <<>>
	    catch _:{xmpp_codec, _} ->
		    <<>>
	    end
    end.

%%====================================================================
%% Options
%%====================================================================
mod_opt_type(use_cache) ->
    econf:bool();
mod_opt_type(cache_size) ->
    econf:pos_int(infinity);
mod_opt_type(cache_missed) ->
    econf:bool();
mod_opt_type(cache_life_time) ->
    econf:timeout(second, infinity).

mod_options(Host) ->
    [{use_cache, ejabberd_option:use_cache(Host)},
     {cache_size, ejabberd_option:cache_size(Host)},
     {cache_missed, ejabberd_option:cache_missed(Host)},
     {cache_life_time, ejabberd_option:cache_life_time(Host)}].

mod_doc() ->
    #{desc =>
          [?T("The user's client can store an avatar in the "
              "user vCard. The vCard-Based Avatars protocol "
              "(https://xmpp.org/extensions/xep-0153.html[XEP-0153]) "
              "provides a method for clients to inform the contacts "
              "what is the avatar hash value. However, simple or small "
              "clients may not implement that protocol."), "",
           ?T("If this module is enabled, all the outgoing client presence "
              "stanzas get automatically the avatar hash on behalf of the "
              "client. So, the contacts receive the presence stanzas with "
              "the 'Update Data' described in "
              "https://xmpp.org/extensions/xep-0153.html[XEP-0153] as if the "
              "client would had inserted it itself. If the client had already "
              "included such element in the presence stanza, it is replaced "
              "with the element generated by ejabberd."), "",
           ?T("By enabling this module, each vCard modification produces "
              "a hash recalculation, and each presence sent by a client "
              "produces hash retrieval and a presence stanza rewrite. "
              "For this reason, enabling this module will introduce a "
              "computational overhead in servers with clients that change "
              "frequently their presence. However, the overhead is significantly "
              "reduced by the use of caching, so you probably don't want "
              "to set 'use_cache' to 'false'."), "",
           ?T("The module depends on _`mod_vcard`_."), "",
           ?T("NOTE: Nowadays https://xmpp.org/extensions/xep-0153.html"
              "[XEP-0153] is used mostly as \"read-only\", i.e. modern "
              "clients don't publish their avatars inside vCards. Thus "
              "in the majority of cases the module is only used along "
              "with _`mod_avatar`_ for providing backward compatibility.")],
      opts =>
          [{use_cache,
            #{value => "true | false",
              desc =>
                  ?T("Same as top-level _`use_cache`_ option, but applied to this module only.")}},
           {cache_size,
            #{value => "pos_integer() | infinity",
              desc =>
                  ?T("Same as top-level _`cache_size`_ option, but applied to this module only.")}},
           {cache_missed,
            #{value => "true | false",
              desc =>
                  ?T("Same as top-level _`cache_missed`_ option, but applied to this module only.")}},
           {cache_life_time,
            #{value => "timeout()",
              desc =>
                  ?T("Same as top-level _`cache_life_time`_ option, but applied to this module only.")}}]}.
