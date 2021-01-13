%%%----------------------------------------------------------------------
%%% File    : ejabberd_commands.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Management of ejabberd commands
%%% Created : 20 May 2008 by Badlop <badlop@process-one.net>
%%%
%%%
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

-module(ejabberd_commands).
-author('badlop@process-one.net').

-behaviour(gen_server).

-define(DEFAULT_VERSION, 1000000).

-export([start_link/0,
	 list_commands/0,
	 list_commands/1,
	 get_command_format/1,
	 get_command_format/2,
	 get_command_format/3,
	 get_command_definition/1,
	 get_command_definition/2,
	 get_tags_commands/0,
	 get_tags_commands/1,
	 register_commands/1,
	 unregister_commands/1,
	 get_commands_spec/0,
	 get_commands_definition/0,
	 get_commands_definition/1,
	 execute_command2/3,
	 execute_command2/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd_commands.hrl").
-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(POLICY_ACCESS, '$policy').

-type auth() :: {binary(), binary(), binary() | {oauth, binary()}, boolean()} | map().

-record(state, {}).

get_commands_spec() ->
    [
        #ejabberd_commands{name = gen_html_doc_for_commands, tags = [documentation],
                           desc = "Generates html documentation for ejabberd_commands",
                           module = ejabberd_commands_doc, function = generate_html_output,
                           args = [{file, binary}, {regexp, binary}, {examples, binary}],
                           result = {res, rescode},
                           args_desc = ["Path to file where generated "
                                        "documentation should be stored",
                                        "Regexp matching names of commands or modules "
                                        "that will be included inside generated document",
                                        "Comma separated list of languages (chosen from java, perl, xmlrpc, json)"
                                        "that will have example invocation include in markdown document"],
                           result_desc = "0 if command failed, 1 when succeeded",
                           args_example = ["/home/me/docs/api.html", "mod_admin", "java,json"],
                           result_example = ok},
        #ejabberd_commands{name = gen_markdown_doc_for_commands, tags = [documentation],
                           desc = "Generates markdown documentation for ejabberd_commands",
                           module = ejabberd_commands_doc, function = generate_md_output,
                           args = [{file, binary}, {regexp, binary}, {examples, binary}],
                           result = {res, rescode},
                           args_desc = ["Path to file where generated "
                                        "documentation should be stored",
                                        "Regexp matching names of commands or modules "
                                        "that will be included inside generated document",
                                        "Comma separated list of languages (chosen from java, perl, xmlrpc, json)"
                                        "that will have example invocation include in markdown document"],
                           result_desc = "0 if command failed, 1 when succeeded",
                           args_example = ["/home/me/docs/api.html", "mod_admin", "java,json"],
                           result_example = ok}].

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    try mnesia:transform_table(ejabberd_commands, ignore,
			       record_info(fields, ejabberd_commands))
    catch exit:{aborted, {no_exists, _}} -> ok
    end,
    ejabberd_mnesia:create(?MODULE, ejabberd_commands,
                        [{ram_copies, [node()]},
                         {local_content, true},
                         {attributes, record_info(fields, ejabberd_commands)},
                         {type, bag}]),
    register_commands(get_commands_spec()),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec register_commands([ejabberd_commands()]) -> ok.

register_commands(Commands) ->
    lists:foreach(
      fun(Command) ->
              %% XXX check if command exists
              mnesia:dirty_write(Command)
              %% ?DEBUG("This command is already defined:~n~p", [Command])
      end,
      Commands),
    ejabberd_access_permissions:invalidate(),
    ok.

-spec unregister_commands([ejabberd_commands()]) -> ok.

unregister_commands(Commands) ->
    lists:foreach(
      fun(Command) ->
	      mnesia:dirty_delete_object(Command)
      end,
      Commands),
    ejabberd_access_permissions:invalidate().

-spec list_commands() -> [{atom(), [aterm()], string()}].

list_commands() ->
    list_commands(?DEFAULT_VERSION).

-spec list_commands(integer()) -> [{atom(), [aterm()], string()}].

list_commands(Version) ->
    Commands = get_commands_definition(Version),
    [{Name, Args, Desc} || #ejabberd_commands{name = Name,
                                              args = Args,
                                              desc = Desc} <- Commands].

-spec get_command_format(atom()) -> {[aterm()], [{atom(),atom()}], rterm()}.

get_command_format(Name) ->
    get_command_format(Name, noauth, ?DEFAULT_VERSION).
get_command_format(Name, Version) when is_integer(Version) ->
    get_command_format(Name, noauth, Version);
get_command_format(Name, Auth)  ->
    get_command_format(Name, Auth, ?DEFAULT_VERSION).

-spec get_command_format(atom(), noauth | admin | auth(), integer()) -> {[aterm()], [{atom(),atom()}], rterm()}.
get_command_format(Name, Auth, Version) ->
    Admin = is_admin(Name, Auth, #{}),
    #ejabberd_commands{args = Args,
		       result = Result,
		       args_rename = Rename,
                       policy = Policy} =
        get_command_definition(Name, Version),
    case Policy of
        user when Admin;
                  Auth == noauth ->
            {[{user, binary}, {host, binary} | Args], Rename, Result};
        _ ->
            {Args, Rename, Result}
    end.

-spec get_command_definition(atom()) -> ejabberd_commands().

get_command_definition(Name) ->
    get_command_definition(Name, ?DEFAULT_VERSION).

-spec get_command_definition(atom(), integer()) -> ejabberd_commands().

get_command_definition(Name, Version) ->
    case lists:reverse(
           lists:sort(
             mnesia:dirty_select(
               ejabberd_commands,
               ets:fun2ms(
                 fun(#ejabberd_commands{name = N, version = V} = C)
                       when N == Name, V =< Version ->
                         {V, C}
                 end)))) of
        [{_, Command} | _ ] -> Command;
        _E -> throw({error, unknown_command})
    end.

get_commands_definition() ->
    get_commands_definition(?DEFAULT_VERSION).

-spec get_commands_definition(integer()) -> [ejabberd_commands()].

get_commands_definition(Version) ->
    L = lists:reverse(
          lists:sort(
            mnesia:dirty_select(
              ejabberd_commands,
              ets:fun2ms(
                fun(#ejabberd_commands{name = Name, version = V} = C)
                      when V =< Version ->
                        {Name, V, C}
                end)))),
    F = fun({_Name, _V, Command}, []) ->
                [Command];
           ({Name, _V, _Command}, [#ejabberd_commands{name=Name}|_T] = Acc) ->
                Acc;
           ({_Name, _V, Command}, Acc) -> [Command | Acc]
        end,
    lists:foldl(F, [], L).

execute_command2(Name, Arguments, CallerInfo) ->
    execute_command2(Name, Arguments, CallerInfo, ?DEFAULT_VERSION).

execute_command2(Name, Arguments, CallerInfo, Version) ->
    Command = get_command_definition(Name, Version),
    case ejabberd_access_permissions:can_access(Name, CallerInfo) of
	allow ->
	    do_execute_command(Command, Arguments);
	_ ->
	    throw({error, access_rules_unauthorized})
    end.


do_execute_command(Command, Arguments) ->
    Module = Command#ejabberd_commands.module,
    Function = Command#ejabberd_commands.function,
    ?DEBUG("Executing command ~p:~p with Args=~p", [Module, Function, Arguments]),
    ejabberd_hooks:run(api_call, [Module, Function, Arguments]),
    apply(Module, Function, Arguments).

-spec get_tags_commands() -> [{string(), [string()]}].

get_tags_commands() ->
    get_tags_commands(?DEFAULT_VERSION).

-spec get_tags_commands(integer()) -> [{string(), [string()]}].

get_tags_commands(Version) ->
    CommandTags = [{Name, Tags} ||
		      #ejabberd_commands{name = Name, tags = Tags}
			  <- get_commands_definition(Version)],
    Dict = lists:foldl(
	     fun({CommandNameAtom, CTags}, D) ->
		     CommandName = atom_to_list(CommandNameAtom),
		     case CTags of
			 [] ->
			     orddict:append("untagged", CommandName, D);
			 _ ->
			     lists:foldl(
			       fun(TagAtom, DD) ->
				       Tag = atom_to_list(TagAtom),
				       orddict:append(Tag, CommandName, DD)
			       end,
			       D,
			       CTags)
		     end
	     end,
	     orddict:new(),
	     CommandTags),
    orddict:to_list(Dict).

%% -----------------------------
%% Access verification
%% -----------------------------
-spec is_admin(atom(), admin | noauth | auth(), map()) -> boolean().
is_admin(_Name, admin, _Extra) ->
    true;
is_admin(_Name, {_User, _Server, _, false}, _Extra) ->
    false;
is_admin(_Name, Map, _extra) when is_map(Map) ->
    true;
is_admin(_Name, _Auth, _Extra) ->
    false.
