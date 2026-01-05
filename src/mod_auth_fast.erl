%%%-------------------------------------------------------------------
%%% File    : mod_auth_fast.erl
%%% Author  : Pawel Chmielowski <pawel@process-one.net>
%%% Created : 1 Dec 2024 by Pawel Chmielowski <pawel@process-one.net>
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
-module(mod_auth_fast).
-behaviour(gen_mod).
-protocol({xep, 484, '0.2.0', '24.12', "complete", ""}).

%% gen_mod API
-export([start/2, stop/1, reload/3, depends/2, mod_options/1, mod_opt_type/1]).
-export([mod_doc/0]).
%% Hooks
-export([c2s_inline_features/3, c2s_handle_sasl2_inline/1,
	 get_tokens/3, get_mechanisms/1, remove_user_tokens/2]).

-include_lib("xmpp/include/xmpp.hrl").
-include_lib("xmpp/include/scram.hrl").
-include("logger.hrl").
-include("translate.hrl").

-callback get_tokens(binary(), binary(), binary()) ->
    [{current | next, binary(), non_neg_integer()}].
-callback rotate_token(binary(), binary(), binary()) ->
    ok | {error, atom()}.
-callback del_token(binary(), binary(), binary(), current | next) ->
    ok | {error, atom()}.
-callback set_token(binary(), binary(), binary(), current | next, binary(), non_neg_integer()) ->
    ok | {error, atom()}.

%%%===================================================================
%%% API
%%%===================================================================
-spec start(binary(), gen_mod:opts()) -> {ok, [gen_mod:registration()]}.
start(Host, Opts) ->
    Mod = gen_mod:db_mod(Opts, ?MODULE),
    Mod:init(Host, Opts),
    {ok, [{hook, c2s_inline_features, c2s_inline_features, 50},
	  {hook, c2s_handle_sasl2_inline, c2s_handle_sasl2_inline, 10},
	  {hook, set_password, remove_user_tokens, 50},
	  {hook, sm_kick_user, remove_user_tokens, 50},
	  {hook, remove_user, remove_user_tokens, 50}]}.

-spec stop(binary()) -> ok.
stop(_Host) ->
    ok.

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(OldOpts, ?MODULE),
    if NewMod /= OldMod ->
	NewMod:init(Host, NewOpts);
	true ->
	    ok
    end,
    ok.

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [].

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(db_type) ->
    econf:db_type(?MODULE);
mod_opt_type(token_lifetime) ->
    econf:timeout(second);
mod_opt_type(token_refresh_age) ->
    econf:timeout(second).

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(Host) ->
    [{db_type, ejabberd_config:default_db(Host, ?MODULE)},
     {token_lifetime, 30*24*60*60},
     {token_refresh_age, 24*60*60}].

mod_doc() ->
    #{desc =>
      [?T("The module adds support for "
	  "https://xmpp.org/extensions/xep-0484.html"
	  "[XEP-0484: Fast Authentication Streamlining Tokens] that allows users to authenticate "
	  "using self-managed tokens.")],
      note => "added in 24.12",
      opts =>
      [{db_type,
	#{value => "mnesia",
	  desc =>
	  ?T("Same as top-level _`default_db`_ option, but applied to this module only.")}},
       {token_lifetime,
	#{value => "timeout()",
	  desc => ?T("Time that tokens will be kept, measured from it's creation time. "
		     "Default value set to 30 days")}},
       {token_refresh_age,
	#{value => "timeout()",
	  desc => ?T("This time determines age of token, that qualifies for automatic refresh. "
		     "Default value set to 1 day")}}],
      example =>
      ["modules:",
       "  mod_auth_fast:",
       "    token_lifetime: 14days"]}.

get_mechanisms(_LServer) ->
    [<<"HT-SHA-256-NONE">>, <<"HT-SHA-256-UNIQ">>, <<"HT-SHA-256-EXPR">>, <<"HT-SHA-256-ENDP">>].

ua_hash(UA) ->
    crypto:hash(sha256, UA).

get_tokens(LServer, LUser, UA) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    ToRefresh = erlang:system_time(second) - mod_auth_fast_opt:token_refresh_age(LServer),
    lists:map(
	fun({Type, Token, CreatedAt}) ->
	    {{Type, CreatedAt < ToRefresh}, Token}
	end, Mod:get_tokens(LServer, LUser, ua_hash(UA))).

c2s_inline_features({Sasl, Bind, Extra}, Host, _State) ->
    {Sasl ++ [#fast{mechs = get_mechanisms(Host)}], Bind, Extra}.

gen_token(#{sasl2_ua_id := UA, server := Server, user := User}) ->
    Mod = gen_mod:db_mod(Server, ?MODULE),
    Token = base64:encode(ua_hash(<<UA/binary, (p1_rand:get_string())/binary>>)),
    ExpiresAt = erlang:system_time(second) + mod_auth_fast_opt:token_lifetime(Server),
    Mod:set_token(Server, User, ua_hash(UA), next, Token, ExpiresAt),
    #fast_token{token = Token, expiry = misc:usec_to_now(ExpiresAt*1000000)}.

c2s_handle_sasl2_inline({#{server := Server, user := User, sasl2_ua_id := UA,
			   sasl2_axtra_auth_info := Extra} = State, Els, Results} = Acc) ->
    Mod = gen_mod:db_mod(Server, ?MODULE),
    NeedRegen =
	case Extra of
	    {token, {next, Rotate}} ->
		Mod:rotate_token(Server, User, ua_hash(UA)),
		Rotate;
	    {token, {_, true}} ->
		true;
	    _ ->
		false
	end,
    case {lists:keyfind(fast_request_token, 1, Els), lists:keyfind(fast, 1, Els)} of
	{#fast_request_token{mech = _Mech}, #fast{invalidate = true}} ->
	    Mod:del_token(Server, User, ua_hash(UA), current),
	    {State, Els, [gen_token(State) | Results]};
	{_, #fast{invalidate = true}} ->
	    Mod:del_token(Server, User, ua_hash(UA), current),
	    Acc;
	{#fast_request_token{mech = _Mech}, _} ->
	    {State, Els, [gen_token(State) | Results]};
	_ when NeedRegen ->
	    {State, Els, [gen_token(State) | Results]};
	_ ->
	    Acc
    end.

-spec remove_user_tokens(binary(), binary()) -> ok.
remove_user_tokens(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:del_tokens(LServer, LUser).
