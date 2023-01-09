%%%-------------------------------------------------------------------
%%% File    : ejabberd_access_permissions.erl
%%% Author  : Paweł Chmielowski <pawel@process-one.net>
%%% Purpose : Administrative functions and commands
%%% Created :  7 Sep 2016 by Paweł Chmielowski <pawel@process-one.net>
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
%%%-------------------------------------------------------------------
-module(ejabberd_access_permissions).
-author("pawel@process-one.net").

-include("ejabberd_commands.hrl").
-include("logger.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
	 can_access/2,
	 invalidate/0,
	 validator/0,
	 show_current_definitions/0]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-define(SERVER, ?MODULE).
-define(CACHE_TAB, access_permissions_cache).

-record(state,
	{definitions = none :: none | [definition()]}).

-type state() :: #state{}.
-type rule() :: {access, acl:access()} |
		{acl, all | none | acl:acl_rule()}.
-type what() :: all | none | [atom() | {tag, atom()}].
-type who() :: rule() | {oauth, {[binary()], [rule()]}}.
-type from() :: atom().
-type permission() :: {binary(), {[from()], [who()], {what(), what()}}}.
-type definition() :: {binary(), {[from()], [who()], [atom()] | all}}.
-type caller_info() :: #{caller_module => module(),
			 caller_host => global | binary(),
			 tag => binary() | none,
			 extra_permissions => [definition()],
			 atom() => term()}.

-export_type([permission/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec can_access(atom(), caller_info()) -> allow | deny.
can_access(Cmd, CallerInfo) ->
    Defs0 = show_current_definitions(),
    CallerModule = maps:get(caller_module, CallerInfo, none),
    Host = maps:get(caller_host, CallerInfo, global),
    Tag = maps:get(tag, CallerInfo, none),
    Defs = maps:get(extra_permissions, CallerInfo, []) ++ Defs0,
    Res = lists:foldl(
	fun({Name, _} = Def, none) ->
	    case matches_definition(Def, Cmd, CallerModule, Tag, Host, CallerInfo) of
		true ->
		    ?DEBUG("Command '~p' execution allowed by rule "
			   "'~ts' (CallerInfo=~p)", [Cmd, Name, CallerInfo]),
		    allow;
		_ ->
		    none
	    end;
	   (_, Val) ->
	       Val
	end, none, Defs),
    case Res of
	allow -> allow;
	_ ->
	    ?DEBUG("Command '~p' execution denied "
		   "(CallerInfo=~p)", [Cmd, CallerInfo]),
	    deny
    end.

-spec invalidate() -> ok.
invalidate() ->
    gen_server:cast(?MODULE, invalidate),
    ets_cache:delete(?CACHE_TAB, definitions).

-spec show_current_definitions() -> [definition()].
show_current_definitions() ->
    ets_cache:lookup(?CACHE_TAB, definitions,
		     fun() ->
			 {cache, gen_server:call(?MODULE, show_current_definitions)}
		     end).
start_link() ->
    ets_cache:new(?CACHE_TAB, [{max_size, 2}]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init([]) -> {ok, state()}.
init([]) ->
    ejabberd_hooks:add(config_reloaded, ?MODULE, invalidate, 90),
    ets_cache:new(access_permissions),
    {ok, #state{}}.

-spec handle_call(show_current_definitions | term(),
		  term(), state()) -> {reply, term(), state()}.
handle_call(show_current_definitions, _From, State) ->
    {State2, Defs} = get_definitions(State),
    {reply, Defs, State2};
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_cast(invalidate | term(), state()) -> {noreply, state()}.
handle_cast(invalidate, State) ->
    {noreply, State#state{definitions = none}};
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_hooks:delete(config_reloaded, ?MODULE, invalidate, 90).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_definitions(state()) -> {state(), [definition()]}.
get_definitions(#state{definitions = Defs} = State) when Defs /= none ->
    {State, Defs};
get_definitions(#state{definitions = none} = State) ->
    ApiPerms = ejabberd_option:api_permissions(),
    AllCommands = ejabberd_commands:get_commands_definition(),
    NDefs0 = lists:map(
	       fun({Name, {From, Who, {Add, Del}}}) ->
		       Cmds = filter_commands_with_permissions(AllCommands, Add, Del),
		       {Name, {From, Who, Cmds}}
	       end, ApiPerms),
    NDefs = case lists:keyfind(<<"console commands">>, 1, NDefs0) of
		false ->
		    [{<<"console commands">>,
		      {[ejabberd_ctl],
		       [{acl, all}],
		       filter_commands_with_permissions(AllCommands, all, none)}} | NDefs0];
		_ ->
		    NDefs0
	    end,
    {State#state{definitions = NDefs}, NDefs}.

-spec matches_definition(definition(), atom(), module(),
			 atom(), global | binary(), caller_info()) -> boolean().
matches_definition({_Name, {From, Who, What}}, Cmd, Module, Tag, Host, CallerInfo) ->
    case What == all orelse lists:member(Cmd, What) of
	true ->
	    {Tags, Modules} = lists:partition(fun({tag, _}) -> true; (_) -> false end, From),
	    case (Modules == [] orelse lists:member(Module, Modules)) andalso
		 (Tags == [] orelse lists:member({tag, Tag}, Tags)) of
		true ->
		    Scope = maps:get(oauth_scope, CallerInfo, none),
		    lists:any(
		      fun({access, Access}) when Scope == none ->
			      acl:match_rule(Host, Access, CallerInfo) == allow;
			 ({acl, Name} = Acl) when Scope == none, is_atom(Name) ->
			      acl:match_acl(Host, Acl, CallerInfo);
			 ({acl, Acl}) when Scope == none ->
			      acl:match_acl(Host, Acl, CallerInfo);
			 ({oauth, {Scopes, List}}) when Scope /= none ->
			      case ejabberd_oauth:scope_in_scope_list(Scope, Scopes) of
				  true ->
				      lists:any(
					fun({access, Access}) ->
						acl:match_rule(Host, Access, CallerInfo) == allow;
					   ({acl, Name} = Acl) when is_atom(Name) ->
						acl:match_acl(Host, Acl, CallerInfo);
					   ({acl, Acl}) ->
						acl:match_acl(Host, Acl, CallerInfo)
					end, List);
				  _ ->
				      false
			      end;
			 (_) ->
			      false
		      end, Who);
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

-spec filter_commands_with_permissions([#ejabberd_commands{}], what(), what()) -> [atom()].
filter_commands_with_permissions(AllCommands, Add, Del) ->
    CommandsAdd = filter_commands_with_patterns(AllCommands, Add, []),
    CommandsDel = filter_commands_with_patterns(CommandsAdd, Del, []),
    lists:map(fun(#ejabberd_commands{name = N}) -> N end,
	      CommandsAdd -- CommandsDel).

-spec filter_commands_with_patterns([#ejabberd_commands{}], what(),
				    [#ejabberd_commands{}]) -> [#ejabberd_commands{}].
filter_commands_with_patterns([], _Patterns, Acc) ->
    Acc;
filter_commands_with_patterns([C | CRest], Patterns, Acc) ->
    case command_matches_patterns(C, Patterns) of
	true ->
	    filter_commands_with_patterns(CRest, Patterns, [C | Acc]);
	_ ->
	    filter_commands_with_patterns(CRest, Patterns, Acc)
    end.

-spec command_matches_patterns(#ejabberd_commands{}, what()) -> boolean().
command_matches_patterns(_, all) ->
    true;
command_matches_patterns(_, none) ->
    false;
command_matches_patterns(_, []) ->
    false;
command_matches_patterns(#ejabberd_commands{tags = Tags} = C, [{tag, Tag} | Tail]) ->
    case lists:member(Tag, Tags) of
	true ->
	    true;
	_ ->
	    command_matches_patterns(C, Tail)
    end;
command_matches_patterns(#ejabberd_commands{name = Name}, [Name | _Tail]) ->
    true;
command_matches_patterns(C, [_ | Tail]) ->
    command_matches_patterns(C, Tail).

%%%===================================================================
%%% Validators
%%%===================================================================
-spec parse_what([binary()]) -> {what(), what()}.
parse_what(Defs) ->
    {A, D} =
	lists:foldl(
	  fun(Def, {Add, Del}) ->
		  case parse_single_what(Def) of
		      {error, Err} ->
			  econf:fail({invalid_syntax, [Err, ": ", Def]});
		      all ->
			  {case Add of none -> none; _ -> all end, Del};
		      {neg, all} ->
			  {none, all};
		      {neg, Value} ->
			  {Add, case Del of L when is_list(L) -> [Value | L]; L2 -> L2 end};
		      Value ->
			  {case Add of L when is_list(L) -> [Value | L]; L2 -> L2 end, Del}
		  end
	  end, {[], []}, Defs),
    case {A, D} of
	{[], _} ->
	    {none, all};
	{A2, []} ->
	    {A2, none};
	V ->
	    V
    end.

-spec parse_single_what(binary()) -> atom() | {neg, atom()} | {tag, atom()} | {error, string()}.
parse_single_what(<<"*">>) ->
    all;
parse_single_what(<<"!*">>) ->
    {neg, all};
parse_single_what(<<"!", Rest/binary>>) ->
    case parse_single_what(Rest) of
	{neg, _} ->
	    {error, "double negation"};
	{error, _} = Err ->
	    Err;
	V ->
	    {neg, V}
    end;
parse_single_what(<<"[tag:", Rest/binary>>) ->
    case binary:split(Rest, <<"]">>) of
	[TagName, <<"">>] ->
	    case parse_single_what(TagName) of
		{error, _} = Err ->
		    Err;
		V when is_atom(V) ->
		    {tag, V};
		_ ->
		    {error, "invalid tag"}
	    end;
	_ ->
	    {error, "invalid tag"}
    end;
parse_single_what(B) ->
    case re:run(B, "^[a-z0-9_\\-]*$") of
	nomatch -> {error, "invalid command"};
	_ -> binary_to_atom(B, latin1)
    end.

validator(Map, Opts) ->
    econf:and_then(
      fun(L) when is_list(L) ->
              lists:map(
                fun({K, V}) -> {(econf:atom())(K), V};
                   (A) -> {acl, (econf:atom())(A)}
                end, lists:flatten(L));
         (A) ->
              [{acl, (econf:atom())(A)}]
      end,
      econf:and_then(
	econf:options(maps:merge(acl:validators(), Map), Opts),
	fun(Rules) ->
		lists:flatmap(
		  fun({Type, Rs}) when is_list(Rs) ->
			  case maps:is_key(Type, acl:validators()) of
			      true -> [{acl, {Type, R}} || R <- Rs];
			      false -> [{Type, Rs}]
			  end;
		     (Other) ->
			  [Other]
		  end, Rules)
	end)).

validator(from) ->
    fun(L) when is_list(L) ->
	    lists:map(
	      fun({K, V}) -> {(econf:enum([tag]))(K), (econf:binary())(V)};
		 (A) -> (econf:enum([ejabberd_xmlrpc, mod_cron, mod_http_api, ejabberd_ctl]))(A)
	      end, lists:flatten(L));
       (A) ->
	    [(econf:enum([ejabberd_xmlrpc, mod_cron, mod_http_api, ejabberd_ctl]))(A)]
    end;
validator(what) ->
    econf:and_then(
      econf:list_or_single(econf:non_empty(econf:binary())),
      fun parse_what/1);
validator(who) ->
    validator(#{access => econf:acl(), oauth => validator(oauth)}, []);
validator(oauth) ->
    econf:and_then(
      validator(#{access => econf:acl(),
		  scope => econf:non_empty(
			     econf:list_or_single(econf:binary()))},
		[{required, [scope]}]),
      fun(Os) ->
	      {[Scopes], Rest} = proplists:split(Os, [scope]),
	      {lists:flatten([S || {_, S} <- Scopes]), Rest}
      end).

validator() ->
    econf:map(
      econf:binary(),
      econf:and_then(
	econf:options(
	  #{from => validator(from),
	    what => validator(what),
	    who => validator(who)}),
	fun(Os) ->
		{proplists:get_value(from, Os, []),
		 proplists:get_value(who, Os, none),
		 proplists:get_value(what, Os, {none, none})}
	end),
      [unique]).
