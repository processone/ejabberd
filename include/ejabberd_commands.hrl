%%%----------------------------------------------------------------------
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

-type aterm() :: {atom(), atype()}.
-type atype() :: integer | string | binary |
                 {tuple, [aterm()]} | {list, aterm()}.
-type rterm() :: {atom(), rtype()}.
-type rtype() :: integer | string | atom |
                 {tuple, [rterm()]} | {list, rterm()} |
                 rescode | restuple.

-type oauth_scope() :: atom().

%% ejabberd_commands OAuth ReST ACL definition:
%% Two fields exist that are used to control access on a command from ReST API:
%% 1. Policy
%% If policy is:
%%  - restricted: command is not exposed as OAuth Rest API.
%%  - admin: Command is allowed for user that have Admin Rest command enabled by access rule: commands_admin_access
%%  - user: Command might be called by any server user.
%%  - open: Command can be called by anyone.
%%
%% Policy is just used to control who can call the command. A specific additional access rules can be performed, as
%% defined by access option.
%% Access option can be a list of:
%% - {Module, accessName, DefaultValue}: Reference and existing module access to limit who can use the command.
%% - AccessRule name: direct name of the access rule to check in config file.
%% TODO: Access option could be atom command (not a list). In the case, User performing the command, will be added as first parameter
%% to command, so that the command can perform additional check.

-record(ejabberd_commands,
        {name                    :: atom(),
         tags = []               :: [atom()] | '_' | '$2',
         desc = ""               :: string() | '_' | '$3',
         longdesc = ""           :: string() | '_',
         version = 0             :: integer(),
         weight = 1              :: integer(),
         module                  :: atom() | '_',
         function                :: atom() | '_',
         args = []               :: [aterm()] | '_' | '$1' | '$2',
         policy = restricted     :: open | restricted | admin | user,
        %% access is: [accessRuleName] or [{Module, AccessOption, DefaultAccessRuleName}]
         access = []             :: [{atom(),atom(),atom()}|atom()],
         result = {res, rescode} :: rterm() | '_' | '$2',
         args_desc = none        :: none | [string()] | '_',
         result_desc = none      :: none | string() | '_',
         args_example = none     :: none | [any()] | '_',
         result_example = none   :: any()}).

%% TODO Fix me: Type is not up to date
-type ejabberd_commands() :: #ejabberd_commands{name :: atom(),
                                                tags :: [atom()],
                                                desc :: string(),
                                                longdesc :: string(),
                                                version :: integer(),
                                                module :: atom(),
                                                function :: atom(),
                                                args :: [aterm()],
                                                policy :: open | restricted | admin | user,
                                                access :: [{atom(),atom(),atom()}|atom()],
                                                result :: rterm()}.

%% @type ejabberd_commands() = #ejabberd_commands{
%%    name = atom(),
%%    tags = [atom()],
%%    desc = string(),
%%    longdesc = string(),
%%    module = atom(),
%%    function = atom(),
%%    args = [aterm()],
%%    result = rterm()
%%    }.
%% desc: Description of the command
%% args: Describe the accepted arguments.
%% This way the function that calls the command can format the
%% arguments before calling.

%% @type atype() = integer | string | {tuple, [aterm()]} | {list, aterm()}.
%% Allowed types for arguments are integer, string, tuple and list.

%% @type rtype() = integer | string | atom | {tuple, [rterm()]} | {list, rterm()} | rescode | restuple.
%% A rtype is either an atom or a tuple with two elements.

%% @type aterm() = {Name::atom(), Type::atype()}.
%% An argument term is a tuple with the term name and the term type.

%% @type rterm() = {Name::atom(), Type::rtype()}.
%% A result term is a tuple with the term name and the term type.
