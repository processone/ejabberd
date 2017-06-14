%%%----------------------------------------------------------------------
%%% File    : mod_vcard_xupdate.erl
%%% Author  : Igor Goryachev <igor@goryachev.org>
%%% Purpose : Add avatar hash in presence on behalf of client (XEP-0153)
%%% Created : 9 Mar 2007 by Igor Goryachev <igor@goryachev.org>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

%% gen_mod callbacks
-export([start/2, stop/1, reload/3]).

-export([update_presence/1, vcard_set/3, remove_user/2,
	 mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-define(VCARD_XUPDATE_CACHE, vcard_xupdate_cache).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

start(Host, Opts) ->
    init_cache(Host, Opts),
    ejabberd_hooks:add(c2s_self_presence, Host, ?MODULE,
		       update_presence, 100),
    ejabberd_hooks:add(vcard_set, Host, ?MODULE, vcard_set,
		       100),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50).

stop(Host) ->
    ejabberd_hooks:delete(c2s_self_presence, Host,
			  ?MODULE, update_presence, 100),
    ejabberd_hooks:delete(vcard_set, Host, ?MODULE,
			  vcard_set, 100),
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
    Hash = get_xupdate(LUser, LServer),
    Pres1 = xmpp:set_subtag(Pres, #vcard_xupdate{hash = Hash}),
    {Pres1, State};
update_presence(Acc) ->
    Acc.

-spec vcard_set(binary(), binary(), xmlel()) -> ok.
vcard_set(LUser, LServer, _VCARD) ->
    ets_cache:delete(?VCARD_XUPDATE_CACHE, {LUser, LServer}),
    ejabberd_sm:force_update_presence({LUser, LServer}).

-spec remove_user(binary(), binary()) -> ok.
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    ets_cache:delete(?VCARD_XUPDATE_CACHE, {LUser, LServer}).

%%====================================================================
%% Storage
%%====================================================================
-spec get_xupdate(binary(), binary()) -> binary() | undefined.
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
	{ok, Hash} -> Hash;
	error -> undefined
    end.

-spec db_get_xupdate(binary(), binary()) -> {ok, binary()} | error.
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
	    CacheOpts = cache_opts(Host, Opts),
	    ets_cache:new(?VCARD_XUPDATE_CACHE, CacheOpts);
	false ->
	    ets_cache:delete(?VCARD_XUPDATE_CACHE)
    end.

-spec cache_opts(binary(), gen_mod:opts()) -> [proplists:property()].
cache_opts(Host, Opts) ->
    MaxSize = gen_mod:get_opt(
		cache_size, Opts,
		ejabberd_config:cache_size(Host)),
    CacheMissed = gen_mod:get_opt(
		    cache_missed, Opts,
		    ejabberd_config:cache_missed(Host)),
    LifeTime = case gen_mod:get_opt(
		      cache_life_time, Opts,
		      ejabberd_config:cache_life_time(Host)) of
		   infinity -> infinity;
		   I -> timer:seconds(I)
	       end,
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec use_cache(binary()) -> boolean().
use_cache(Host) ->
    gen_mod:get_module_opt(
      Host, ?MODULE, use_cache,
      ejabberd_config:use_cache(Host)).

-spec compute_hash(xmlel()) -> binary().
compute_hash(VCard) ->
    case fxml:get_path_s(VCard,
			 [{elem, <<"PHOTO">>},
			  {elem, <<"BINVAL">>},
			  cdata]) of
	<<>> ->
	    <<>>;
	BinVal ->
	    try str:sha(base64:decode(BinVal))
	    catch _:badarg -> <<>>
	    end
    end.

%%====================================================================
%% Options
%%====================================================================
mod_opt_type(O) when O == cache_life_time; O == cache_size ->
    fun (I) when is_integer(I), I > 0 -> I;
        (infinity) -> infinity
    end;
mod_opt_type(O) when O == use_cache; O == cache_missed ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(_) ->
    [cache_life_time, cache_size, use_cache, cache_missed].
