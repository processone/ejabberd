%%%----------------------------------------------------------------------
%%% File    : ejabberd_commands.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Management of ejabberd commands
%%% Created : 20 May 2008 by Badlop <badlop@process-one.net>
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

%%% @headerfile "ejabberd_commands.hrl"

%%% @doc Management of ejabberd commands.
%%%
%%% An ejabberd command is an abstract function identified by a name,
%%% with a defined number and type of calling arguments and type of
%%% result, that can be defined in any Erlang module and executed
%%% using any valid frontend.
%%%
%%%
%%% == Define a new ejabberd command ==
%%%
%%% ejabberd commands can be defined and registered in
%%% any Erlang module.
%%%
%%% Some commands are procedures; and their purpose is to perform an
%%% action in the server, so the command result is only some result
%%% code or result tuple.  Other commands are inspectors, and their
%%% purpose is to gather some information about the server and return
%%% a detailed response: it can be integer, string, atom, tuple, list
%%% or a mix of those ones.
%%%
%%% The arguments and result of an ejabberd command are strictly
%%% defined.  The number and format of the arguments provided when
%%% calling an ejabberd command must match the definition of that
%%% command.  The format of the result provided by an ejabberd command
%%% must be exactly its definition. For example, if a command is said
%%% to return an integer, it must always return an integer (except in
%%% case of a crash).
%%%
%%% If you are developing an Erlang module that will run inside
%%% ejabberd and you want to provide a new ejabberd command to
%%% administer some task related to your module, you only need to:
%%% implement a function, define the command, and register it.
%%%
%%%
%%% === Define a new ejabberd command ===
%%%
%%% An ejabberd command is defined using the Erlang record
%%% 'ejabberd_commands'.  This record has several elements that you
%%% must define. Note that 'tags', 'desc' and 'longdesc' are optional.
%%%
%%% For example let's define an ejabberd command 'pow' that gets the
%%% integers 'base' and 'exponent'. Its result will be an integer
%%% 'power':
%%%
%%% <pre>#ejabberd_commands{name = pow, tags = [test],
%%%                 desc = "Return the power of base for exponent",
%%%                 longdesc = "This is an example command. The formula is:\n"
%%%                 "  power = base ^ exponent",
%%%                 module = ?MODULE, function = pow,
%%%                 args = [{base, integer}, {exponent, integer}],
%%%                 result = {power, integer}}</pre>
%%%
%%%
%%% === Implement the function associated to the command ===
%%%
%%% Now implement a function in your module that matches the arguments
%%% and result of the ejabberd command.
%%%
%%% For example the function calc_power gets two integers Base and
%%% Exponent. It calculates the power and rounds to an integer:
%%%
%%% <pre>calc_power(Base, Exponent) ->
%%%    PowFloat = math:pow(Base, Exponent),
%%%    round(PowFloat).</pre>
%%%
%%% Since this function will be called by ejabberd_commands, it must
%%% be exported.
%%% Add to your module:
%%% <pre>-export([calc_power/2]).</pre>
%%%
%%% Only some types of result formats are allowed.
%%% If the format is defined as 'rescode', then your function must return:
%%%   ok | true | atom()
%%% where the atoms ok and true as considered positive answers,
%%% and any other response atom is considered negative.
%%%
%%% If the format is defined as 'restuple', then the command must return:
%%%   {rescode(), string()}
%%%
%%% If the format is defined as '{list, something()}', then the command
%%% must return a list of something().
%%%
%%%
%%% === Register the command ===
%%%
%%% Define this function and put inside the #ejabberd_command you
%%% defined in the beginning:
%%%
%%% <pre>commands() ->
%%%    [
%%%
%%%    ].</pre>
%%%
%%% You need to include this header file in order to use the record:
%%%
%%% <pre>-include("ejabberd_commands.hrl").</pre>
%%%
%%% When your module is initialized or started, register your commands:
%%%
%%% <pre>ejabberd_commands:register_commands(commands()),</pre>
%%%
%%% And when your module is stopped, unregister your commands:
%%%
%%% <pre>ejabberd_commands:unregister_commands(commands()),</pre>
%%%
%%% That's all! Now when your module is started, the command will be
%%% registered and any frontend can access it. For example:
%%%
%%% <pre>$ ejabberdctl help pow
%%%
%%%   Command Name: pow
%%%
%%%   Arguments: base::integer
%%%              exponent::integer
%%%
%%%   Returns: power::integer
%%%
%%%   Tags: test
%%%
%%%   Description: Return the power of base for exponent
%%%
%%% This is an example command. The formula is:
%%%  power = base ^ exponent
%%%
%%% $ ejabberdctl pow 3 4
%%% 81
%%% </pre>
%%%
%%%
%%% == Execute an ejabberd command ==
%%%
%%% ejabberd commands are mean to be executed using any valid
%%% frontend.  An ejabberd commands is implemented in a regular Erlang
%%% function, so it is also possible to execute this function in any
%%% Erlang module, without dealing with the associated ejabberd
%%% commands.
%%%
%%%
%%% == Frontend to ejabberd commands ==
%%%
%%% Currently there are two frontends to ejabberd commands: the shell
%%% script {@link ejabberd_ctl. ejabberdctl}, and the XML-RPC server
%%% ejabberd_xmlrpc.
%%%
%%%
%%% === ejabberdctl as a frontend to ejabberd commands ===
%%%
%%% It is possible to use ejabberdctl to get documentation of any
%%% command. But ejabberdctl does not support all the argument types
%%% allowed in ejabberd commands, so there are some ejabberd commands
%%% that cannot be executed using ejabberdctl.
%%%
%%% Also note that the ejabberdctl shell administration script also
%%% manages ejabberdctl commands, which are unrelated to ejabberd
%%% commands and can only be executed using ejabberdctl.
%%%
%%%
%%% === ejabberd_xmlrpc as a frontend to ejabberd commands ===
%%%
%%% ejabberd_xmlrpc provides an XML-RPC server to execute ejabberd commands.
%%% ejabberd_xmlrpc is a contributed module published in ejabberd-modules SVN.
%%%
%%% Since ejabberd_xmlrpc does not provide any method to get documentation
%%% of the ejabberd commands, please use ejabberdctl to know which
%%% commands are available, and their usage.
%%%
%%% The number and format of the arguments provided when calling an
%%% ejabberd command must match the definition of that command. Please
%%% make sure the XML-RPC call provides the required arguments, with
%%% the specified format. The order of the arguments in an XML-RPC
%%% call is not important, because all the data is tagged and will be
%%% correctly prepared by ejabberd_xmlrpc before executing the ejabberd
%%% command.

%%% TODO: consider this feature:
%%% All commands are catched. If an error happens, return the restuple:
%%%   {error, flattened error string}
%%% This means that ecomm call APIs (ejabberd_ctl, ejabberd_xmlrpc)
%%% need to allows this. And ejabberd_xmlrpc must be prepared to
%%% handle such an unexpected response.


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
	 get_command_policy_and_scope/1,
	 get_command_definition/1,
	 get_command_definition/2,
	 get_tags_commands/0,
	 get_tags_commands/1,
	 get_exposed_commands/0,
	 register_commands/1,
	 unregister_commands/1,
	 expose_commands/1,
	 execute_command/2,
	 execute_command/3,
	 execute_command/4,
	 execute_command/5,
	 execute_command/6,
	 opt_type/1,
	 get_commands_spec/0,
	 get_commands_definition/0,
	 get_commands_definition/1,
	 execute_command2/3,
	 execute_command2/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd_commands.hrl").
-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(POLICY_ACCESS, '$policy').

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
                                        "Comma separated list of languages (choosen from java, perl, xmlrpc, json)"
                                        "that will have example invocation include in markdown document"],
                           result_desc = "0 if command failed, 1 when succedded",
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
                                        "Comma separated list of languages (choosen from java, perl, xmlrpc, json)"
                                        "that will have example invocation include in markdown document"],
                           result_desc = "0 if command failed, 1 when succedded",
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
    mnesia:add_table_copy(ejabberd_commands, node(), ram_copies),
    register_commands(get_commands_spec()),
    ejabberd_access_permissions:register_permission_addon(?MODULE, fun permission_addon/0),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec register_commands([ejabberd_commands()]) -> ok.

%% @doc Register ejabberd commands.
%% If a command is already registered, a warning is printed and the
%% old command is preserved.
%% A registered command is not directly available to be called through
%% ejabberd ReST API. It need to be exposed to be available through API.
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

%% @doc Unregister ejabberd commands.
unregister_commands(Commands) ->
    lists:foreach(
      fun(Command) ->
	      mnesia:dirty_delete_object(Command)
      end,
      Commands),
    ejabberd_access_permissions:invalidate(),
    ok.

%% @doc Expose command through ejabberd ReST API.
%% Pass a list of command names or policy to expose.
-spec expose_commands([ejabberd_commands()|atom()|open|user|admin|restricted]) -> ok | {error, atom()}.

expose_commands(Commands) ->
    Names = lists:map(fun(#ejabberd_commands{name = Name}) ->
                              Name;
                         (Name) when is_atom(Name) ->
                              Name
                      end,
                      Commands),

    case ejabberd_config:add_option(commands, [{add_commands, Names}]) of
        {aborted, Reason} ->
            {error, Reason};
        {atomic, Result} ->
            Result
    end.

-spec list_commands() -> [{atom(), [aterm()], string()}].

%% @doc Get a list of all the available commands, arguments and description.
list_commands() ->
    list_commands(?DEFAULT_VERSION).

-spec list_commands(integer()) -> [{atom(), [aterm()], string()}].

%% @doc Get a list of all the available commands, arguments and
%% description in a given API verion.
list_commands(Version) ->
    Commands = get_commands_definition(Version),
    [{Name, Args, Desc} || #ejabberd_commands{name = Name,
                                              args = Args,
                                              desc = Desc} <- Commands].


-spec list_commands_policy(integer()) ->
				  [{atom(), [aterm()], string(), atom()}].

%% @doc Get a list of all the available commands, arguments,
%% description, and policy in a given API version.
list_commands_policy(Version) ->
    Commands = get_commands_definition(Version),
    [{Name, Args, Desc, Policy} ||
        #ejabberd_commands{name = Name,
                           args = Args,
                           desc = Desc,
                           policy = Policy} <- Commands].

-spec get_command_format(atom()) -> {[aterm()], rterm()}.

%% @doc Get the format of arguments and result of a command.
get_command_format(Name) ->
    get_command_format(Name, noauth, ?DEFAULT_VERSION).
get_command_format(Name, Version) when is_integer(Version) ->
    get_command_format(Name, noauth, Version);
get_command_format(Name, Auth)  ->
    get_command_format(Name, Auth, ?DEFAULT_VERSION).

-spec get_command_format(atom(),
			 {binary(), binary(), binary(), boolean()} |
			 noauth | admin,
			 integer()) ->
				{[aterm()], rterm()}.

get_command_format(Name, Auth, Version) ->
    Admin = is_admin(Name, Auth, #{}),
    #ejabberd_commands{args = Args,
		       result = Result,
                       policy = Policy} =
        get_command_definition(Name, Version),
    case Policy of
        user when Admin;
                  Auth == noauth ->
            {[{user, binary}, {server, binary} | Args], Result};
        _ ->
            {Args, Result}
    end.

-spec get_command_policy_and_scope(atom()) -> {ok, open|user|admin|restricted, [oauth_scope()]} | {error, command_not_found}.

%% @doc return command policy.
get_command_policy_and_scope(Name) ->
    case get_command_definition(Name) of
        #ejabberd_commands{policy = Policy} = Cmd ->
            {ok, Policy, cmd_scope(Cmd)};
        command_not_found ->
            {error, command_not_found}
    end.

%% The oauth scopes for a command are the command name itself,
%% also might include either 'ejabberd:user' or 'ejabberd:admin'
cmd_scope(#ejabberd_commands{policy = Policy, name = Name}) ->
    [erlang:atom_to_binary(Name,utf8)] ++ [<<"ejabberd:user">> || Policy == user] ++ [<<"ejabberd:admin">> || Policy == admin].


-spec get_command_definition(atom()) -> ejabberd_commands().

%% @doc Get the definition record of a command.
get_command_definition(Name) ->
    get_command_definition(Name, ?DEFAULT_VERSION).

-spec get_command_definition(atom(), integer()) -> ejabberd_commands().

%% @doc Get the definition record of a command in a given API version.
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

% @doc Returns all commands for a given API version
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

%% @spec (Name::atom(), Arguments) -> ResultTerm
%% where
%%       Arguments = [any()]
%% @doc Execute a command.
%% Can return the following exceptions:
%% command_unknown | account_unprivileged | invalid_account_data |
%% no_auth_provided | access_rules_unauthorized
execute_command(Name, Arguments) ->
    execute_command(Name, Arguments, ?DEFAULT_VERSION).

-spec execute_command(atom(),
                      [any()],
		      integer() |
		      {binary(), binary(), binary(), boolean()} |
                      noauth | admin
                     ) -> any().

%% @spec (Name::atom(), Arguments, integer() | Auth) -> ResultTerm
%% where
%%       Auth = {User::string(), Server::string(), Password::string(),
%%               Admin::boolean()}
%%            | noauth
%%            | admin
%%       Arguments = [any()]
%%
%% @doc Execute a command in a given API version
%% Can return the following exceptions:
%% command_unknown | account_unprivileged | invalid_account_data |
%% no_auth_provided
execute_command(Name, Arguments, Version) when is_integer(Version) ->
    execute_command([], noauth, Name, Arguments, Version);
execute_command(Name, Arguments, Auth) ->
    execute_command([], Auth, Name, Arguments, ?DEFAULT_VERSION).

%% @spec (AccessCommands, Auth, Name::atom(), Arguments) ->
%%                                     ResultTerm | {error, Error}
%% where
%%       AccessCommands = [{Access, CommandNames, Arguments}] | undefined
%%       Auth = {User::string(), Server::string(), Password::string(), Admin::boolean()}
%%            | noauth
%%            | admin
%%       Arguments = [any()]
%%
%% @doc Execute a command
%% Can return the following exceptions:
%% command_unknown | account_unprivileged | invalid_account_data | no_auth_provided
execute_command(AccessCommands, Auth, Name, Arguments) ->
    execute_command(AccessCommands, Auth, Name, Arguments, ?DEFAULT_VERSION).

-spec execute_command([{atom(), [atom()], [any()]}] | undefined,
                      {binary(), binary(), binary(), boolean()} |
                      noauth | admin,
                      atom(),
                      [any()],
		      integer()
                     ) -> any().

%% @spec (AccessCommands, Auth, Name::atom(), Arguments, integer()) -> ResultTerm
%% where
%%       AccessCommands = [{Access, CommandNames, Arguments}] | undefined
%%       Auth = {User::string(), Server::string(), Password::string(), Admin::boolean()}
%%            | noauth
%%            | admin
%%       Arguments = [any()]
%%
%% @doc Execute a command in a given API version
%% Can return the following exceptions:
%% command_unknown | account_unprivileged | invalid_account_data | no_auth_provided | access_rules_unauthorized
execute_command(AccessCommands1, Auth1, Name, Arguments, Version) ->
    execute_command(AccessCommands1, Auth1, Name, Arguments, Version, #{}).

execute_command(AccessCommands1, Auth1, Name, Arguments, Version, CallerInfo) ->
    Auth = case is_admin(Name, Auth1, CallerInfo) of
               true -> admin;
               false -> Auth1
           end,
    TokenJID = oauth_token_user(Auth1),
    Command = get_command_definition(Name, Version),
    AccessCommands = get_all_access_commands(AccessCommands1),

    case check_access_commands(AccessCommands, Auth, Name, Command, Arguments, CallerInfo) of
        ok -> execute_check_policy(Auth, TokenJID, Command, Arguments)
    end.


execute_check_policy(
  _Auth, _JID, #ejabberd_commands{policy = open} = Command, Arguments) ->
    do_execute_command(Command, Arguments);
execute_check_policy(
  noauth, _JID, Command, Arguments) ->
    do_execute_command(Command, Arguments);
execute_check_policy(
  _Auth, _JID, #ejabberd_commands{policy = restricted} = Command, Arguments) ->
    do_execute_command(Command, Arguments);
execute_check_policy(
  _Auth, JID, #ejabberd_commands{policy = admin} = Command, Arguments) ->
    execute_check_access(JID, Command, Arguments);
execute_check_policy(
  admin, JID, #ejabberd_commands{policy = user} = Command, Arguments) ->
    execute_check_access(JID, Command, Arguments);
execute_check_policy(
  {User, Server, _, _}, JID, #ejabberd_commands{policy = user} = Command, Arguments) ->
    execute_check_access(JID, Command, [User, Server | Arguments]).

execute_check_access(_FromJID, #ejabberd_commands{access = []} = Command, Arguments) ->
    do_execute_command(Command, Arguments);
execute_check_access(undefined, _Command, _Arguments) ->
    throw({error, access_rules_unauthorized});
execute_check_access(FromJID, #ejabberd_commands{access = AccessRefs} = Command, Arguments) ->
    %% TODO Review: Do we have smarter / better way to check rule on other Host than global ?
    Host = global,
    Rules = lists:map(fun({Mod, AccessName, Default}) ->
                              gen_mod:get_module_opt(Host, Mod,
                                                     AccessName, fun(A) -> A end, Default);
                         (Default) ->
                              Default
                      end, AccessRefs),
    case acl:any_rules_allowed(Host, Rules, FromJID) of
        true ->
            do_execute_command(Command, Arguments);
        false ->
            throw({error, access_rules_unauthorized})
    end.

do_execute_command(Command, Arguments) ->
    Module = Command#ejabberd_commands.module,
    Function = Command#ejabberd_commands.function,
    ?DEBUG("Executing command ~p:~p with Args=~p", [Module, Function, Arguments]),
    apply(Module, Function, Arguments).

-spec get_tags_commands() -> [{string(), [string()]}].

%% @spec () -> [{Tag::string(), [CommandName::string()]}]
%% @doc Get all the tags and associated commands.
get_tags_commands() ->
    get_tags_commands(?DEFAULT_VERSION).

-spec get_tags_commands(integer()) -> [{string(), [string()]}].

%% @spec (integer) -> [{Tag::string(), [CommandName::string()]}]
%% @doc Get all the tags and associated commands in a given API version
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

%% @spec (AccessCommands, Auth, Method, Command, Arguments) -> ok
%% where
%%       AccessCommands =  [ {Access, CommandNames, Arguments} ]
%%       Auth = {User::string(), Server::string(), Password::string()} | noauth
%%       Method = atom()
%%       Arguments = [any()]
%% @doc Check access is allowed to that command.
%% At least one AccessCommand must be satisfied.
%% It may throw {error, Error} where:
%% Error = account_unprivileged | invalid_account_data
check_access_commands([], _Auth, _Method, _Command, _Arguments, _CallerInfo) ->
    ok;
check_access_commands(AccessCommands, Auth, Method, Command1, Arguments, CallerInfo) ->
    Command =
        case {Command1#ejabberd_commands.policy, Auth} of
            {user, {_, _, _, _}} ->
                Command1;
            {user, _} ->
                Command1#ejabberd_commands{
                  args = [{user, binary}, {server, binary} |
                          Command1#ejabberd_commands.args]};
            _ ->
                Command1
        end,
    AccessCommandsAllowed =
        lists:filter(
          fun({Access, Commands, ArgumentRestrictions}) ->
                  case check_access(Command, Access, Auth, CallerInfo) of
                      true ->
                          check_access_command(Commands, Command,
                                               ArgumentRestrictions,
                                               Method, Arguments);
                      false ->
                          false
                  end;
             ({Access, Commands}) ->
                  ArgumentRestrictions = [],
                  case check_access(Command, Access, Auth, CallerInfo) of
                      true ->
                          check_access_command(Commands, Command,
                                               ArgumentRestrictions,
                                               Method, Arguments);
                      false ->
                          false
                  end
          end,
          AccessCommands),
    case AccessCommandsAllowed of
        [] -> throw({error, account_unprivileged});
        L when is_list(L) -> ok
    end.

-spec check_auth(ejabberd_commands(), noauth) -> noauth_provided;
                (ejabberd_commands(),
                 {binary(), binary(), binary(), boolean()}) ->
    {ok, binary(), binary()}.

check_auth(_Command, noauth) ->
    no_auth_provided;
check_auth(Command, {User, Server, {oauth, Token}, _}) ->
    ScopeList = cmd_scope(Command),
    case ejabberd_oauth:check_token(User, Server, ScopeList, Token) of
        true ->
            {ok, User, Server};
        _ ->
            throw({error, invalid_account_data})
    end;
check_auth(_Command, {User, Server, Password, _}) when is_binary(Password) ->
    %% Check the account exists and password is valid
    case ejabberd_auth:check_password(User, <<"">>, Server, Password) of
        true -> {ok, User, Server};
        _ -> throw({error, invalid_account_data})
    end.

check_access(Command, ?POLICY_ACCESS, _, _)
  when Command#ejabberd_commands.policy == open ->
    true;
check_access(_Command, _Access, admin, _) ->
    true;
check_access(_Command, _Access, {_User, _Server, _, true}, _) ->
    false;
check_access(Command, Access, Auth, CallerInfo)
  when Access =/= ?POLICY_ACCESS;
       Command#ejabberd_commands.policy == open;
       Command#ejabberd_commands.policy == user ->
    case check_auth(Command, Auth) of
	{ok, User, Server} ->
	    check_access2(Access, CallerInfo#{usr => jid:split(jid:make(User, Server))}, Server);
	no_auth_provided ->
	    case Command#ejabberd_commands.policy of
		user ->
		    false;
		_ ->
		    check_access2(Access, CallerInfo, global)
	    end;
	_ ->
	    false
    end;
check_access(_Command, _Access, _Auth, _CallerInfo) ->
    false.

check_access2(?POLICY_ACCESS, _CallerInfo, _Server) ->
    true;
check_access2(Access, AccessInfo, Server) ->
    %% Check this user has access permission
    case acl:access_matches(Access, AccessInfo, Server) of
	allow -> true;
	deny -> false
    end.

check_access_command(Commands, Command, ArgumentRestrictions,
		     Method, Arguments) ->
    case Commands==all orelse lists:member(Method, Commands) of
        true -> check_access_arguments(Command, ArgumentRestrictions,
                                       Arguments);
        false -> false
    end.

check_access_arguments(Command, ArgumentRestrictions, Arguments) ->
    ArgumentsTagged = tag_arguments(Command#ejabberd_commands.args, Arguments),
    lists:all(
      fun({ArgName, ArgAllowedValue}) ->
	      %% If the call uses the argument, check the value is acceptable
	      case lists:keysearch(ArgName, 1, ArgumentsTagged) of
		  {value, {ArgName, ArgValue}} -> ArgValue == ArgAllowedValue;
		  false -> true
	      end
      end, ArgumentRestrictions).

tag_arguments(ArgsDefs, Args) ->
    lists:zipwith(
      fun({ArgName, _ArgType}, ArgValue) ->
	      {ArgName, ArgValue}
      end,
      ArgsDefs,
      Args).


%% Get commands for all version
get_all_access_commands(AccessCommands) ->
    get_access_commands(AccessCommands, ?DEFAULT_VERSION).

get_access_commands(undefined, Version) ->
    Cmds = get_exposed_commands(Version),
    [{?POLICY_ACCESS, Cmds, []}];
get_access_commands(AccessCommands, _Version) ->
    AccessCommands.

get_exposed_commands() ->
    get_exposed_commands(?DEFAULT_VERSION).
get_exposed_commands(Version) ->
    Opts0 = ejabberd_config:get_option(
             commands,
             fun(V) when is_list(V) -> V end,
              []),
    Opts = lists:map(fun(V) when is_tuple(V) -> [V]; (V) -> V end, Opts0),
    CommandsList = list_commands_policy(Version),
    OpenCmds = [N || {N, _, _, open} <- CommandsList],
    RestrictedCmds = [N || {N, _, _, restricted} <- CommandsList],
    AdminCmds = [N || {N, _, _, admin} <- CommandsList],
    UserCmds = [N || {N, _, _, user} <- CommandsList],
    Cmds =
        lists:foldl(
          fun([{add_commands, L}], Acc) ->
                  Cmds = expand_commands(L, OpenCmds, UserCmds, AdminCmds, RestrictedCmds),
                  lists:usort(Cmds ++ Acc);
             ([{remove_commands, L}], Acc) ->
                  Cmds = expand_commands(L, OpenCmds, UserCmds, AdminCmds, RestrictedCmds),
                  Acc -- Cmds;
             (_, Acc) -> Acc
          end, [], Opts),
    Cmds.

%% This is used to allow mixing command policy (like open, user, admin, restricted), with command entry
expand_commands(L, OpenCmds, UserCmds, AdminCmds, RestrictedCmds) when is_atom(L) ->
    expand_commands([L], OpenCmds, UserCmds, AdminCmds, RestrictedCmds);
expand_commands(L, OpenCmds, UserCmds, AdminCmds, RestrictedCmds) when is_list(L) ->
    lists:foldl(fun(open, Acc) -> OpenCmds ++ Acc;
                   (user, Acc) -> UserCmds ++ Acc;
                   (admin, Acc) -> AdminCmds ++ Acc;
                   (restricted, Acc) -> RestrictedCmds ++ Acc;
                   (Command, Acc) when is_atom(Command) ->
                        [Command|Acc]
                end, [], L).

oauth_token_user(noauth) ->
    undefined;
oauth_token_user(admin) ->
    undefined;
oauth_token_user({User, Server, _, _}) ->
    jid:make(User, Server).

is_admin(_Name, admin, _Extra) ->
    true;
is_admin(_Name, {_User, _Server, _, false}, _Extra) ->
    false;
is_admin(_Name, Map, _extra) when is_map(Map) ->
    true;
is_admin(Name, Auth, Extra) ->
    {ACLInfo, Server} = case Auth of
			    {U, S, _, _} ->
				{Extra#{usr=>jid:split(jid:make(U, S))}, S};
			    _ ->
				{Extra, global}
	      end,
    AdminAccess = ejabberd_config:get_option(
                    commands_admin_access,
		    fun(V) -> V end,
                    none),
    case acl:access_matches(AdminAccess, ACLInfo, Server) of
        allow ->
            case catch check_auth(get_command_definition(Name), Auth) of
                {ok, _, _} -> true;
		no_auth_provided -> true;
                _ -> false
            end;
        deny -> false
    end.

permission_addon() ->
    [{<<"'commands' option compatibility shim">>,
     {[],
      [{access, ejabberd_config:get_option(commands_admin_access,
					   fun(V) -> V end,
					   none)}],
      {get_exposed_commands(), []}}}].

opt_type(commands_admin_access) -> fun acl:access_rules_validator/1;
opt_type(commands) ->
    fun(V) when is_list(V) -> V end;
opt_type(_) -> [commands, commands_admin_access].
