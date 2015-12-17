%%%----------------------------------------------------------------------
%%% File    : ejabberd_commands.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Management of ejabberd commands
%%% Created : 20 May 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
%%% Since this function will be called by ejabberd_commands, it must be exported.
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
%%% This means that ecomm call APIs (ejabberd_ctl, ejabberd_xmlrpc) need to allows this.
%%% And ejabberd_xmlrpc must be prepared to handle such an unexpected response.


-module(ejabberd_commands).
-author('badlop@process-one.net').

-export([init/0,
	 list_commands/0,
	 get_command_format/1,
         get_command_format/2,
	 get_command_definition/1,
	 get_tags_commands/0,
         get_commands/0,
	 register_commands/1,
	 unregister_commands/1,
	 execute_command/2,
         execute_command/4,
         opt_type/1
	]).

-include("ejabberd_commands.hrl").
-include("ejabberd.hrl").
-include("logger.hrl").

-define(POLICY_ACCESS, '$policy').

init() ->
    ets:new(ejabberd_commands, [named_table, set, public,
				{keypos, #ejabberd_commands.name}]).

-spec register_commands([ejabberd_commands()]) -> ok.

%% @doc Register ejabberd commands.
%% If a command is already registered, a warning is printed and the old command is preserved.
register_commands(Commands) ->
    lists:foreach(
      fun(Command) ->
	      case ets:insert_new(ejabberd_commands, Command) of
		  true ->
		      ok;
		  false ->
		      ?DEBUG("This command is already defined:~n~p", [Command])
	      end
      end,
      Commands).

-spec unregister_commands([ejabberd_commands()]) -> ok.

%% @doc Unregister ejabberd commands.
unregister_commands(Commands) ->
    lists:foreach(
      fun(Command) ->
	      ets:delete_object(ejabberd_commands, Command)
      end,
      Commands).

-spec list_commands() -> [{atom(), [aterm()], string()}].

%% @doc Get a list of all the available commands, arguments and description.
list_commands() ->
    Commands = ets:match(ejabberd_commands,
			 #ejabberd_commands{name = '$1',
					    args = '$2',
					    desc = '$3',
					    _ = '_'}),
    [{A, B, C} || [A, B, C] <- Commands].

-spec list_commands_policy() -> [{atom(), [aterm()], string(), atom()}].

%% @doc Get a list of all the available commands, arguments, description, and
%% policy.
list_commands_policy() ->
    Commands = ets:match(ejabberd_commands,
			 #ejabberd_commands{name = '$1',
					    args = '$2',
					    desc = '$3',
					    policy = '$4',
					    _ = '_'}),
    [{A, B, C, D} || [A, B, C, D] <- Commands].

-spec get_command_format(atom()) -> {[aterm()], rterm()} | {error, command_unknown}.

%% @doc Get the format of arguments and result of a command.
get_command_format(Name) ->
    get_command_format(Name, noauth).

get_command_format(Name, Auth) ->
    Admin = is_admin(Name, Auth),
    Matched = ets:match(ejabberd_commands,
			#ejabberd_commands{name = Name,
					   args = '$1',
					   result = '$2',
                                           policy = '$3',
					   _ = '_'}),
    case Matched of
	[] ->
	    {error, command_unknown};
	[[Args, Result, user]] when Admin;
                                    Auth == noauth ->
	    {[{user, binary}, {server, binary} | Args], Result};
	[[Args, Result, _]] ->
	    {Args, Result}
    end.

-spec get_command_definition(atom()) -> ejabberd_commands() | command_not_found.

%% @doc Get the definition record of a command.
get_command_definition(Name) ->
    case ets:lookup(ejabberd_commands, Name) of
	[E] -> E;
	[] -> command_not_found
    end.

%% @spec (Name::atom(), Arguments) -> ResultTerm | {error, command_unknown}
%% @doc Execute a command.
execute_command(Name, Arguments) ->
    execute_command([], noauth, Name, Arguments).

-spec execute_command([{atom(), [atom()], [any()]}],
                      {binary(), binary(), binary(), boolean()} |
                      noauth | admin,
                      atom(),
                      [any()]
                     ) -> any().

%% @spec (AccessCommands, Auth, Name::atom(), Arguments) -> ResultTerm | {error, Error}
%% where
%%       AccessCommands = [{Access, CommandNames, Arguments}]
%%       Auth = {User::string(), Server::string(), Password::string(), Admin::boolean()}
%%            | noauth
%%            | admin
%%       Method = atom()
%%       Arguments = [any()]
%%       Error = command_unknown | account_unprivileged | invalid_account_data | no_auth_provided
execute_command(AccessCommands1, Auth1, Name, Arguments) ->
    Auth = case is_admin(Name, Auth1) of
               true -> admin;
               false -> Auth1
           end,
    case ets:lookup(ejabberd_commands, Name) of
	[Command] ->
            AccessCommands = get_access_commands(AccessCommands1),
	    try check_access_commands(AccessCommands, Auth, Name, Command, Arguments) of
		ok -> execute_command2(Auth, Command, Arguments)
	    catch
		{error, Error} -> {error, Error}
	    end;
	[] -> {error, command_unknown}
    end.

execute_command2(
  _Auth, #ejabberd_commands{policy = open} = Command, Arguments) ->
    execute_command2(Command, Arguments);
execute_command2(
  _Auth, #ejabberd_commands{policy = restricted} = Command, Arguments) ->
    execute_command2(Command, Arguments);
execute_command2(
  _Auth, #ejabberd_commands{policy = admin} = Command, Arguments) ->
    execute_command2(Command, Arguments);
execute_command2(
  admin, #ejabberd_commands{policy = user} = Command, Arguments) ->
    execute_command2(Command, Arguments);
execute_command2(
  noauth, #ejabberd_commands{policy = user} = Command, Arguments) ->
    execute_command2(Command, Arguments);
execute_command2(
  {User, Server, _, _}, #ejabberd_commands{policy = user} = Command, Arguments) ->
    execute_command2(Command, [User, Server | Arguments]).

execute_command2(Command, Arguments) ->
    Module = Command#ejabberd_commands.module,
    Function = Command#ejabberd_commands.function,
    ?DEBUG("Executing command ~p:~p with Args=~p", [Module, Function, Arguments]),
    try apply(Module, Function, Arguments) of
	Response ->
	    Response
    catch
	Problem ->
	    {error, Problem}
    end.

-spec get_tags_commands() -> [{string(), [string()]}].

%% @spec () -> [{Tag::string(), [CommandName::string()]}]
%% @doc Get all the tags and associated commands.
get_tags_commands() ->
    CommandTags = ets:match(ejabberd_commands,
			    #ejabberd_commands{
			      name = '$1',
			      tags = '$2',
			      _ = '_'}),
    Dict = lists:foldl(
	     fun([CommandNameAtom, CTags], D) ->
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
check_access_commands([], _Auth, _Method, _Command, _Arguments) ->
    ok;
check_access_commands(AccessCommands, Auth, Method, Command1, Arguments) ->
    Command =
        case {Command1#ejabberd_commands.policy, Auth} of
            {user, {_, _, _}} ->
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
		  case check_access(Command, Access, Auth) of
		      true ->
			  check_access_command(Commands, Command, ArgumentRestrictions,
					       Method, Arguments);
		      false ->
			  false
		  end;
	      ({Access, Commands}) ->
		  ArgumentRestrictions = [],
		  case check_access(Command, Access, Auth) of
		      true ->
			  check_access_command(Commands, Command, ArgumentRestrictions,
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
    Scope = erlang:atom_to_binary(Command#ejabberd_commands.name, utf8),
    case ejabberd_oauth:check_token(User, Server, Scope, Token) of
        true ->
            {ok, User, Server};
        false ->
            throw({error, invalid_account_data})
    end;
check_auth(_Command, {User, Server, Password, _}) when is_binary(Password) ->
    %% Check the account exists and password is valid
    case ejabberd_auth:check_password(User, Server, Password) of
        true -> {ok, User, Server};
        _ -> throw({error, invalid_account_data})
    end.

check_access(Command, ?POLICY_ACCESS, _)
  when Command#ejabberd_commands.policy == open ->
    true;
check_access(_Command, _Access, admin) ->
    true;
check_access(_Command, _Access, {_User, _Server, _, true}) ->
    false;
check_access(Command, Access, Auth)
  when Access =/= ?POLICY_ACCESS;
       Command#ejabberd_commands.policy == open;
       Command#ejabberd_commands.policy == user ->
    case check_auth(Command, Auth) of
	{ok, User, Server} ->
	    check_access2(Access, User, Server);
	_ ->
	    false
    end;
check_access(_Command, _Access, _Auth) ->
    false.

check_access2(?POLICY_ACCESS, _User, _Server) ->
    true;
check_access2(Access, User, Server) ->
    %% Check this user has access permission
    case acl:match_rule(Server, Access, jid:make(User, Server, <<"">>)) of
	allow -> true;
	deny -> false
    end.

check_access_command(Commands, Command, ArgumentRestrictions, Method, Arguments) ->
    case Commands==all orelse lists:member(Method, Commands) of
	true -> check_access_arguments(Command, ArgumentRestrictions, Arguments);
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


get_access_commands(undefined) ->
    Cmds = get_commands(),
    [{?POLICY_ACCESS, Cmds, []}];
get_access_commands(AccessCommands) ->
    AccessCommands.

get_commands() ->
    Opts = ejabberd_config:get_option(
             commands,
             fun(V) when is_list(V) -> V end,
             []),
    CommandsList = list_commands_policy(),
    OpenCmds = [N || {N, _, _, open} <- CommandsList],
    RestrictedCmds = [N || {N, _, _, restricted} <- CommandsList],
    AdminCmds = [N || {N, _, _, admin} <- CommandsList],
    UserCmds = [N || {N, _, _, user} <- CommandsList],
    Cmds =
        lists:foldl(
          fun({add_commands, L}, Acc) ->
                  Cmds = case L of
                             open -> OpenCmds;
                             restricted -> RestrictedCmds;
                             admin -> AdminCmds;
                             user -> UserCmds;
                             _ when is_list(L) -> L
                         end,
                  lists:usort(Cmds ++ Acc);
             ({remove_commands, L}, Acc) ->
                  Cmds = case L of
                             open -> OpenCmds;
                             restricted -> RestrictedCmds;
                             admin -> AdminCmds;
                             user -> UserCmds;
                             _ when is_list(L) -> L
                         end,
                  Acc -- Cmds;
             (_, Acc) -> Acc
          end, AdminCmds ++ UserCmds, Opts),
    Cmds.

is_admin(_Name, noauth) ->
    false;
is_admin(_Name, admin) ->
    true;
is_admin(_Name, {_User, _Server, _, false}) ->
    false;
is_admin(Name, {User, Server, _, true} = Auth) ->
    AdminAccess = ejabberd_config:get_option(
                    commands_admin_access,
                    fun(A) when is_atom(A) -> A end,
                    none),
    case acl:match_rule(Server, AdminAccess,
                        jid:make(User, Server, <<"">>)) of
        allow ->
            case catch check_auth(get_command_definition(Name), Auth) of
                {ok, _, _} -> true;
                _ -> false
            end;
        deny -> false
    end.

opt_type(commands_admin_access) ->
    fun(A) when is_atom(A) -> A end;
opt_type(commands) ->
    fun(V) when is_list(V) -> V end;
opt_type(_) -> [commands, commands_admin_access].
