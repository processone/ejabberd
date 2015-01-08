%%%----------------------------------------------------------------------
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

-type aterm() :: {atom(), atype()}.
-type atype() :: integer | string | binary |
                 {tuple, [aterm()]} | {list, aterm()}.
-type rterm() :: {atom(), rtype()}.
-type rtype() :: integer | string | atom |
                 {tuple, [rterm()]} | {list, rterm()} |
                 rescode | restuple.

-record(ejabberd_commands,
	{name                    :: atom(),
         tags = []               :: [atom()] | '_' | '$2',
         desc = ""               :: string() | '_' | '$3',
         longdesc = ""           :: string() | '_',
	 module                  :: atom(),
         function                :: atom(),
         args = []               :: [aterm()] | '_' | '$1' | '$2',
         result = {res, rescode} :: rterm() | '_' | '$2'}).

-type ejabberd_commands() :: #ejabberd_commands{name :: atom(),
                                                tags :: [atom()],
                                                desc :: string(),
                                                longdesc :: string(),
                                                module :: atom(),
                                                function :: atom(),
                                                args :: [aterm()],
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

