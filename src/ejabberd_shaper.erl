%%%----------------------------------------------------------------------
%%% ejabberd, Copyright (C) 2002-2021   ProcessOne
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
-module(ejabberd_shaper).
-behaviour(gen_server).

-export([start_link/0, new/1, update/2, match/3, get_max_rate/1]).
-export([reload_from_config/0]).
-export([validator/1, shaper_rules_validator/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("logger.hrl").

-type state() :: #{hosts := [binary()]}.
-type shaper() :: none | p1_shaper:state().
-type shaper_rate() :: {pos_integer(), pos_integer()} | pos_integer() | infinity.
-type shaper_rule() :: {atom() | pos_integer(), [acl:access_rule()]}.
-type shaper_rate_rule() :: {shaper_rate(), [acl:access_rule()]}.

-export_type([shaper/0, shaper_rule/0, shaper_rate/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec match(global | binary(), atom() | [shaper_rule()],
	    jid:jid() | jid:ljid() | inet:ip_address() | acl:match()) -> none | shaper_rate().
match(_, none, _) -> none;
match(_, infinity, _) -> infinity;
match(Host, Shaper, Match) when is_map(Match) ->
    Rules = if is_atom(Shaper) -> read_shaper_rules(Shaper, Host);
	       true -> Shaper
	    end,
    Rate = acl:match_rules(Host, Rules, Match, none),
    read_shaper(Rate);
match(Host, Shaper, IP) when tuple_size(IP) == 4; tuple_size(IP) == 8 ->
    match(Host, Shaper, #{ip => IP});
match(Host, Shaper, JID) ->
    match(Host, Shaper, #{usr => jid:tolower(JID)}).

-spec get_max_rate(none | shaper_rate()) -> none | pos_integer().
get_max_rate({Rate, _}) -> Rate;
get_max_rate(Rate) when is_integer(Rate), Rate > 0 -> Rate;
get_max_rate(_) -> none.

-spec new(none | shaper_rate()) -> shaper().
new({Rate, Burst}) -> p1_shaper:new(Rate, Burst);
new(Rate) when is_integer(Rate), Rate > 0 -> p1_shaper:new(Rate);
new(_) -> none.

-spec update(shaper(), non_neg_integer()) -> {shaper(), non_neg_integer()}.
update(none, _Size) -> {none, 0};
update(Shaper1, Size) ->
    Shaper2 = p1_shaper:update(Shaper1, Size),
    ?DEBUG("Shaper update:~n~ts =>~n~ts",
	   [p1_shaper:pp(Shaper1), p1_shaper:pp(Shaper2)]),
    Shaper2.

-spec validator(shaper | shaper_rules) -> econf:validator().
validator(shaper) ->
    econf:options(
      #{'_' => shaper_validator()},
      [{disallowed, reserved()}, {return, map}, unique]);
validator(shaper_rules) ->
    econf:options(
      #{'_' => shaper_rules_validator()},
      [{disallowed, reserved()}, unique]).

-spec shaper_rules_validator() -> econf:validator().
shaper_rules_validator() ->
    fun(L) when is_list(L) ->
	    lists:map(
	      fun({K, V}) ->
		      {(shaper_name())(K), (acl:access_validator())(V)};
		 (N) ->
		      {(shaper_name())(N), [{acl, all}]}
	      end, lists:flatten(L));
       (N) ->
	    [{(shaper_name())(N), [{acl, all}]}]
    end.

-spec reload_from_config() -> ok.
reload_from_config() ->
    gen_server:call(?MODULE, reload_from_config, timer:minutes(1)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    create_tabs(),
    Hosts = ejabberd_option:hosts(),
    load_from_config([], Hosts),
    ejabberd_hooks:add(config_reloaded, ?MODULE, reload_from_config, 20),
    {ok, #{hosts => Hosts}}.

-spec handle_call(term(), term(), state()) -> {reply, ok, state()} | {noreply, state()}.
handle_call(reload_from_config, _, #{hosts := OldHosts} = State) ->
    NewHosts = ejabberd_option:hosts(),
    load_from_config(OldHosts, NewHosts),
    {reply, ok, State#{hosts => NewHosts}};
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
    ejabberd_hooks:delete(config_reloaded, ?MODULE, reload_from_config, 20).

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%===================================================================
%%% Table management
%%%===================================================================
-spec load_from_config([binary()], [binary()]) -> ok.
load_from_config(OldHosts, NewHosts) ->
    ?DEBUG("Loading shaper rules from config", []),
    Shapers = ejabberd_option:shaper(),
    ets:insert(shaper, maps:to_list(Shapers)),
    ets:insert(
      shaper_rules,
      lists:flatmap(
	fun(Host) ->
		lists:flatmap(
		  fun({Name, List}) ->
			  case resolve_shapers(Name, List, Shapers) of
			      [] -> [];
			      List1 ->
				  [{{Name, Host}, List1}]
			  end
		  end, ejabberd_option:shaper_rules(Host))
	end, [global|NewHosts])),
    lists:foreach(
      fun(Host) ->
	      ets:match_delete(shaper_rules, {{'_', Host}, '_'})
      end, OldHosts -- NewHosts),
    ?DEBUG("Shaper rules loaded successfully", []).

-spec create_tabs() -> ok.
create_tabs() ->
    _ = mnesia:delete_table(shaper),
    _ = ets:new(shaper, [named_table, {read_concurrency, true}]),
    _ = ets:new(shaper_rules, [named_table, {read_concurrency, true}]),
    ok.

-spec read_shaper_rules(atom(), global | binary()) -> [shaper_rate_rule()].
read_shaper_rules(Name, Host) ->
    case ets:lookup(shaper_rules, {Name, Host}) of
	[{_, Rule}] -> Rule;
	[] -> []
    end.

-spec read_shaper(atom() | shaper_rate()) -> none | shaper_rate().
read_shaper(Name) when is_atom(Name), Name /= none, Name /= infinity ->
    case ets:lookup(shaper, Name) of
	[{_, Rate}] -> Rate;
	[] -> none
    end;
read_shaper(Rate) ->
    Rate.

%%%===================================================================
%%% Validators
%%%===================================================================
shaper_name() ->
    econf:either(
      econf:and_then(
	econf:atom(),
	fun(infinite) -> infinity;
	   (unlimited) -> infinity;
	   (A) -> A
	end),
      econf:pos_int()).

shaper_validator() ->
    econf:either(
      econf:and_then(
	econf:options(
	  #{rate => econf:pos_int(),
	    burst_size => econf:pos_int()},
	  [unique, {required, [rate]}, {return, map}]),
	fun(#{rate := Rate} = Map) ->
		{Rate, maps:get(burst_size, Map, Rate)}
	end),
      econf:pos_int(infinity)).

%%%===================================================================
%%% Aux
%%%===================================================================
reserved() ->
    [none, infinite, unlimited, infinity].

-spec resolve_shapers(atom(), [shaper_rule()], #{atom() => shaper_rate()}) -> [shaper_rate_rule()].
resolve_shapers(ShaperRule, Rules, Shapers) ->
    lists:filtermap(
      fun({Name, Rule}) when is_atom(Name), Name /= none, Name /= infinity ->
	      try {true, {maps:get(Name, Shapers), Rule}}
	      catch _:{badkey, _} ->
		      ?WARNING_MSG(
			 "Shaper rule '~ts' refers to unknown shaper: ~ts",
			 [ShaperRule, Name]),
		      false
	      end;
	 (_) ->
	      true
      end, Rules).
