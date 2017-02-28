%%%-------------------------------------------------------------------
%%% File    : ejabberd_access_permissions.erl
%%% Author  : Paweł Chmielowski <pawel@process-one.net>
%%% Purpose : Administrative functions and commands
%%% Created :  7 Sep 2016 by Paweł Chmielowski <pawel@process-one.net>
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
%%%-------------------------------------------------------------------
-module(ejabberd_access_permissions).
-author("pawel@process-one.net").

-include("ejabberd_commands.hrl").
-include("logger.hrl").

-behaviour(gen_server).
-behavior(ejabberd_config).

%% API
-export([start_link/0,
	 parse_api_permissions/1,
	 can_access/2,
	 invalidate/0,
	 opt_type/1,
	 show_current_definitions/0,
	 register_permission_addon/2,
	 unregister_permission_addon/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    definitions = none,
    fragments_generators = []
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec can_access(atom(), map()) -> allow | deny.
can_access(Cmd, CallerInfo) ->
    gen_server:call(?MODULE, {can_access, Cmd, CallerInfo}).

-spec invalidate() -> ok.
invalidate() ->
    gen_server:cast(?MODULE, invalidate).

-spec register_permission_addon(atom(), fun()) -> ok.
register_permission_addon(Name, Fun) ->
    gen_server:call(?MODULE, {register_config_fragment_generator, Name, Fun}).

-spec unregister_permission_addon(atom()) -> ok.
unregister_permission_addon(Name) ->
    gen_server:call(?MODULE, {unregister_config_fragment_generator, Name}).

-spec show_current_definitions() -> any().
show_current_definitions() ->
    gen_server:call(?MODULE, show_current_definitions).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([]) ->
    ejabberd_hooks:add(config_reloaded, ?MODULE, invalidate, 90),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		  State :: #state{}) ->
		     {reply, Reply :: term(), NewState :: #state{}} |
		     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
		     {noreply, NewState :: #state{}} |
		     {noreply, NewState :: #state{}, timeout() | hibernate} |
		     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
		     {stop, Reason :: term(), NewState :: #state{}}.
handle_call({can_access, Cmd, CallerInfo}, _From, State) ->
    CallerModule = maps:get(caller_module, CallerInfo, none),
    Host = maps:get(caller_host, CallerInfo, global),
    {State2, Defs0} = get_definitions(State),
    Defs = maps:get(extra_permissions, CallerInfo, []) ++ Defs0,
    Res = lists:foldl(
	fun({Name, _} = Def, none) ->
	    case matches_definition(Def, Cmd, CallerModule, Host, CallerInfo) of
		true ->
		    ?DEBUG("Command '~p' execution allowed by rule '~s' (CallerInfo=~p)", [Cmd, Name, CallerInfo]),
		    allow;
		_ ->
		    none
	    end;
	   (_, Val) ->
	       Val
	end, none, Defs),
    Res2 = case Res of
	       allow -> allow;
	       _ ->
		   ?DEBUG("Command '~p' execution denied (CallerInfo=~p)", [Cmd, CallerInfo]),
		   deny
	   end,
    {reply, Res2, State2};
handle_call(show_current_definitions, _From, State) ->
    {State2, Defs} = get_definitions(State),
    {reply, Defs, State2};
handle_call({register_config_fragment_generator, Name, Fun}, _From, #state{fragments_generators = Gens} = State) ->
    NGens = lists:keystore(Name, 1, Gens, {Name, Fun}),
    {reply, ok, State#state{fragments_generators = NGens}};
handle_call({unregister_config_fragment_generator, Name}, _From, #state{fragments_generators = Gens} = State) ->
    NGens = lists:keydelete(Name, 1, Gens),
    {reply, ok, State#state{fragments_generators = NGens}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_cast(invalidate, State) ->
    {noreply, State#state{definitions = none}};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #state{}) -> term().
terminate(_Reason, _State) ->
    ejabberd_hooks:delete(config_reloaded, ?MODULE, invalidate, 90).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{},
		  Extra :: term()) ->
		     {ok, NewState :: #state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_definitions(#state{}) -> {#state{}, any()}.
get_definitions(#state{definitions = Defs} = State) when Defs /= none ->
    {State, Defs};
get_definitions(#state{definitions = none, fragments_generators = Gens} = State) ->
    DefaultOptions = [{<<"admin access">>,
		       {[],
			[{acl,{acl,admin}},
			 {oauth,[<<"ejabberd:admin">>],[{acl,{acl,admin}}]}],
			{all, [start, stop]}}}],
    ApiPerms = ejabberd_config:get_option(api_permissions, fun(A) -> A end,
					  DefaultOptions),
    AllCommands = ejabberd_commands:get_commands_definition(),
    Frags = lists:foldl(
	      fun({_Name, Generator}, Acc) ->
		      Acc ++ Generator()
	      end, [], Gens),
    NDefs0 = lists:map(
	       fun({Name, {From, Who, {Add, Del}}}) ->
		       Cmds = filter_commands_with_permissions(AllCommands, Add, Del),
		       {Name, {From, Who, Cmds}}
	       end, ApiPerms ++ Frags),
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

matches_definition({_Name, {From, Who, What}}, Cmd, Module, Host, CallerInfo) ->
    case What == all orelse lists:member(Cmd, What) of
	true ->
	    case From == [] orelse lists:member(Module, From) of
		true ->
		    Scope = maps:get(oauth_scope, CallerInfo, none),
		    lists:any(
			fun({access, Access}) when Scope == none ->
			    acl:access_matches(Access, CallerInfo, Host) == allow;
			   ({acl, Acl}) when Scope == none ->
			       acl:acl_rule_matches(Acl, CallerInfo, Host);
			   ({oauth, Scopes, List}) when Scope /= none ->
			       case ejabberd_oauth:scope_in_scope_list(Scope, Scopes) of
				   true ->
				       lists:any(
					   fun({access, Access}) ->
					       acl:access_matches(Access, CallerInfo, Host) == allow;
					      ({acl, Acl}) ->
						  acl:acl_rule_matches(Acl, CallerInfo, Host)
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

filter_commands_with_permissions(AllCommands, Add, Del) ->
    CommandsAdd = filter_commands_with_patterns(AllCommands, Add, []),
    CommandsDel = filter_commands_with_patterns(CommandsAdd, Del, []),
    lists:map(fun(#ejabberd_commands{name = N}) -> N end,
	      CommandsAdd -- CommandsDel).

filter_commands_with_patterns([], _Patterns, Acc) ->
    Acc;
filter_commands_with_patterns([C | CRest], Patterns, Acc) ->
    case command_matches_patterns(C, Patterns) of
	true ->
	    filter_commands_with_patterns(CRest, Patterns, [C | Acc]);
	_ ->
	    filter_commands_with_patterns(CRest, Patterns, Acc)
    end.

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
%%% Options parsing code
%%%===================================================================

parse_api_permissions(Data) when is_list(Data) ->
    throw({replace_with, [parse_api_permission(Name, Args) || {Name, Args} <- Data]}).

parse_api_permission(Name, Args0) ->
    Args = lists:flatten(Args0),
    {From, Who, What} = case key_split(Args, [{from, []}, {who, none}, {what, []}]) of
			    {error, Msg} ->
				report_error(<<"~s inside api_permission '~s' section">>, [Msg, Name]);
			    Val -> Val
			end,
    {Name, {parse_from(Name, From), parse_who(Name, Who, oauth), parse_what(Name, What)}}.

parse_from(_Name, Module) when is_atom(Module) ->
    [Module];
parse_from(Name, Modules) when is_list(Modules) ->
    lists:foreach(fun(Module) when is_atom(Module) ->
	ok;
		     (Val) ->
			 report_error(<<"Invalid value '~p' used inside 'from' section for api_permission '~s'">>,
				      [Val, Name])
		  end, Modules),
    Modules;
parse_from(Name, Val) ->
    report_error(<<"Invalid value '~p' used inside 'from' section for api_permission '~s'">>,
		 [Val, Name]).

parse_who(Name, Atom, ParseOauth) when is_atom(Atom) ->
    parse_who(Name, [Atom], ParseOauth);
parse_who(Name, Defs, ParseOauth) when is_list(Defs) ->
    lists:map(
      fun([Val]) ->
	      [NVal] = parse_who(Name, [Val], ParseOauth),
	      NVal;
	 ({access, Val}) ->
	    try acl:access_rules_validator(Val) of
		Rule ->
		    {access, Rule}
	    catch
		throw:{invalid_syntax, Msg} ->
		    report_error(<<"Invalid access rule: '~s' used inside 'who' section for api_permission '~s'">>,
				 [Msg, Name]);
		throw:{replace_with, NVal} ->
		    {access, NVal};
		error:_ ->
		    report_error(<<"Invalid access rule '~p' used inside 'who' section for api_permission '~s'">>,
				 [Val, Name])
	    end;
	   ({oauth, OauthList}) when is_list(OauthList) ->
	       case ParseOauth of
		   oauth ->
		       Nested = parse_who(Name, lists:flatten(OauthList), scope),
		       {Scopes, Rest} = lists:partition(
				   fun({scope, _}) -> true;
				      (_) -> false
				   end, Nested),
		       case Scopes of
			   [] ->
			       report_error(<<"Oauth rule must contain at least one scope rule in 'who' section for api_permission '~s'">>,
					    [Name]);
			   _ ->
			       {oauth, lists:foldl(fun({scope, S}, A) -> S ++ A end, [], Scopes), Rest}
		       end;
		   scope ->
		       report_error(<<"Oauth rule can't be embeded inside other oauth rule in 'who' section for api_permission '~s'">>,
				    [Name])
	       end;
	   ({scope, ScopeList}) ->
	       case ParseOauth of
		   oauth ->
		       report_error(<<"Scope can be included only inside oauth rule in 'who' section for api_permission '~s'">>,
				    [Name]);
		   scope ->
		       ScopeList2 = case ScopeList of
					V when is_binary(V) -> [V];
					V2 when is_list(V2) -> V2;
					V3 ->
					    report_error(<<"Invalid value for scope '~p' in 'who' section for api_permission '~s'">>,
							 [V3, Name])
				    end,
		       {scope, ScopeList2}
	       end;
	   (Atom) when is_atom(Atom) ->
	       {acl, {acl, Atom}};
	   (Other) ->
	       try acl:normalize_spec(Other) of
		   Rule2 ->
		       {acl, Rule2}
	       catch
		   _:_ ->
		       report_error(<<"Invalid value '~p' used inside 'who' section for api_permission '~s'">>,
				    [Other, Name])
	       end
	end, Defs);
parse_who(Name, Val, _ParseOauth) ->
    report_error(<<"Invalid value '~p' used inside 'who' section for api_permission '~s'">>,
		 [Val, Name]).

parse_what(Name, Binary) when is_binary(Binary) ->
    parse_what(Name, [Binary]);
parse_what(Name, Defs) when is_list(Defs) ->
    {A, D} = lists:foldl(
	fun(Def, {Add, Del}) ->
	    case parse_single_what(Def) of
		{error, Err} ->
		    report_error(<<"~s used in value '~p' in 'what' section for api_permission '~s'">>,
				 [Err, Def, Name]);
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
    end;
parse_what(Name, Val) ->
    report_error(<<"Invalid value '~p' used inside 'what' section for api_permission '~s'">>,
		 [Val, Name]).

parse_single_what(<<"*">>) ->
    all;
parse_single_what(<<"!*">>) ->
    {neg, all};
parse_single_what(<<"!", Rest/binary>>) ->
    case parse_single_what(Rest) of
	{neg, _} ->
	    {error, <<"Double negation">>};
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
		    {error, <<"Invalid tag">>}
	    end;
	_ ->
	    {error, <<"Invalid tag">>}
    end;
parse_single_what(Binary) when is_binary(Binary) ->
    case is_valid_command_name(Binary) of
	true ->
	    binary_to_atom(Binary, latin1);
	_ ->
	    {error, <<"Invalid value">>}
    end;
parse_single_what(_) ->
    {error, <<"Invalid value">>}.

is_valid_command_name(<<>>) ->
    false;
is_valid_command_name(Val) ->
    is_valid_command_name2(Val).

is_valid_command_name2(<<>>) ->
    true;
is_valid_command_name2(<<K:8, Rest/binary>>) when K >= $a andalso K =< $z orelse K == $_ ->
    is_valid_command_name2(Rest);
is_valid_command_name2(_) ->
    false.

key_split(Args, Fields) ->
    {_, Order1, Results1, Required1} = lists:foldl(
	fun({Field, Default}, {Idx, Order, Results, Required}) ->
	    {Idx + 1, maps:put(Field, Idx, Order), [Default | Results], Required};
	   (Field, {Idx, Order, Results, Required}) ->
	       {Idx + 1, maps:put(Field, Idx, Order), [none | Results], maps:put(Field, 1, Required)}
	end, {1, #{}, [], #{}}, Fields),
    key_split(Args, list_to_tuple(Results1), Order1, Required1, #{}).

key_split([], _Results, _Order, Required, _Duplicates) when map_size(Required) > 0 ->
    parse_error(<<"Missing fields '~s">>, [str:join(maps:keys(Required), <<", ">>)]);
key_split([], Results, _Order, _Required, _Duplicates) ->
    Results;
key_split([{Arg, Value} | Rest], Results, Order, Required, Duplicates) ->
    case maps:find(Arg, Order) of
	{ok, Idx} ->
	    case maps:is_key(Arg, Duplicates) of
		false ->
		    Results2 = setelement(Idx, Results, Value),
		    key_split(Rest, Results2, Order, maps:remove(Arg, Required), maps:put(Arg, 1, Duplicates));
		true ->
		    parse_error(<<"Duplicate field '~s'">>, [Arg])
	    end;
	_ ->
	    parse_error(<<"Unknown field '~s'">>, [Arg])
    end.

report_error(Format, Args) ->
    throw({invalid_syntax, (str:format(Format, Args))}).

parse_error(Format, Args) ->
    {error, (str:format(Format, Args))}.

opt_type(api_permissions) ->
    fun parse_api_permissions/1;
opt_type(_) ->
    [api_permissions].
